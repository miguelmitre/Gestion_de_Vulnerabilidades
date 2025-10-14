################################################################################
#Proyecto          => SISTEMA DE AFORES ( SAFRE )                              #
#Propietario       => E.F.P.                                                   #
#Programa RETC8051 => REVERSO GENERA SALDOS DE DEVOLUCION POR LEY 73 (RET B)   #            
#Fecha creacion    => 10 DE MARZO DE 2008                                      #
#Desarrado por     => JAVIER GONZALEZ JERONIMO                                 #
#Sistema           => RET                                                      #
################################################################################
DATABASE safre_af     
GLOBALS
    DEFINE reg_2 RECORD #glo #reg_2
        recibido              LIKE ret_estado.estado_solicitud ,
        enviado               LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE reg_3 RECORD #glo #reg_3
        nss                   LIKE ret_transf_rx.nss       ,
        consecutivo           LIKE ret_transf_rx.consecutivo          
    END RECORD
    
    DEFINE #glo #integer
        vfolio_oper02         INTEGER

    DEFINE #glo #smallint
        vestado               ,
        v_tipo_mov            SMALLINT                

    DEFINE #glo #date
        HOY                   DATE 

    DEFINE #glo #char
        enter                 CHAR,
        v_tipo_retiro         CHAR(1)
END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL init() #i
    OPEN WINDOW retc81411 AT 4,4 WITH FORM "RETC81411" ATTRIBUTE(BORDER)
    DISPLAY " <Ctrl-C> Salir                                              TIPO RETIRO B     " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC8141 REVERSO GENERA SALDOS DE DEVOLUCION POR LEY 73                       " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    INPUT BY NAME vfolio_oper02 WITHOUT DEFAULTS

        AFTER FIELD vfolio_oper02
            SELECT estado_solicitud
            INTO   vestado
            FROM   ret_transf_rx
            WHERE  folio       = vfolio_oper02
            AND    tipo_retiro = v_tipo_retiro
            GROUP  BY 1 
           
            IF STATUS = NOTFOUND THEN
                ERROR "    FOLIO INEXISTENTE"
                ATTRIBUTE(NORMAL)
                NEXT FIELD vfolio_oper02
            ELSE
                IF vestado = reg_2.enviado THEN
                    ERROR "    ERROR...PRIMERO SE DEBE REVERSAR EL LOTE"
                    ATTRIBUTE(NORMAL)
                    NEXT FIELD vfolio_oper02
                END IF
            END IF

            SELECT "OK"
            FROM   dis_cuenta
            WHERE  folio = vfolio_oper02
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
                ERROR "    ERROR...FOLIO YA LIQUIDADO"
                ATTRIBUTE(NORMAL)
                NEXT FIELD vfolio_oper02
            END IF

        ON KEY (ESC)
            SELECT estado_solicitud
            INTO   vestado
            FROM   ret_transf_rx
            WHERE  folio       = vfolio_oper02
            AND    tipo_retiro = v_tipo_retiro
            GROUP  BY 1 
            
            IF STATUS = NOTFOUND THEN
                ERROR "    FOLIO INEXISTENTE"
                ATTRIBUTE(NORMAL)
                NEXT FIELD vfolio_oper02
            ELSE
                IF vestado = reg_2.enviado THEN
                    ERROR "    ERROR...PRIMERO SE DEBE REVERSAR EL LOTE"
                    ATTRIBUTE(NORMAL)
                    NEXT FIELD vfolio_oper02
                END IF
            END IF

            SELECT "OK"
            FROM   dis_cuenta
            WHERE  folio = vfolio_oper02
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
                ERROR "    ERROR...FOLIO YA LIQUIDADO"
                ATTRIBUTE(NORMAL)
                NEXT FIELD vfolio_oper02
            END IF

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

    CLOSE WINDOW retc81411
END MAIN

FUNCTION init()
#i-------------
    LET HOY = TODAY
    
    LET v_tipo_retiro = "B"
    
    SELECT movimiento
    INTO   v_tipo_mov
    FROM   tab_retiro
    WHERE  tipo_retiro = v_tipo_retiro
    
--    LET v_tipo_mov = 810

    SELECT A.estado_solicitud
    INTO   reg_2.recibido
    FROM   ret_estado A
    WHERE  A.descripcion = "RECEPCIONADO"
 
    SELECT A.estado_solicitud
    INTO   reg_2.enviado
    FROM   ret_estado A
    WHERE  A.descripcion = "ENVIADO"

    SELECT MAX(folio)
    INTO   vfolio_oper02
    FROM   ret_transf_rx
    WHERE  tipo_retiro = v_tipo_retiro
END FUNCTION

FUNCTION primer_paso()
#pp-------------------

    DECLARE cur_1 CURSOR FOR
    SELECT nss         ,
           consecutivo
    FROM   ret_transf_rx
    WHERE  folio       = vfolio_oper02
    AND    tipo_retiro = v_tipo_retiro;

    FOREACH cur_1 INTO reg_3.*
        DELETE
        FROM   ret_transf_tx
        WHERE  folio       = vfolio_oper02
        AND    nss         = reg_3.nss
        AND    consecutivo = reg_3.consecutivo
        
        
        SELECT "OK"
        FROM   cta_act_marca
        WHERE  nss         = reg_3.nss
        AND    correlativo = reg_3.consecutivo
        
        IF STATUS = NOTFOUND THEN
            INSERT INTO cta_act_marca
            SELECT A.nss          ,
                   A.marca_cod    ,
                   A.fecha_ini    ,
                   A.hora_ini     ,
                   A.estado_marca ,
                   A.marca_causa  ,
                   A.fecha_causa  ,
                   A.correlativo  ,
                   A.usr_marca
            FROM   cta_his_marca A
            WHERE  A.nss         = reg_3.nss
            AND    A.correlativo = reg_3.consecutivo
        END IF
    END FOREACH

    DELETE
    FROM  dis_provision
    WHERE folio = vfolio_oper02
    AND   tipo_movimiento = v_tipo_mov;

    DELETE
    FROM  ret_ctr_envio_lote
    WHERE folio       = vfolio_oper02
    AND   tipo_retiro = v_tipo_retiro;

    DELETE
    FROM   ret_monto_siefore
    WHERE  folio       = vfolio_oper02
    AND    tipo_retiro = v_tipo_retiro;

    DELETE
    FROM   ret_monto_viv
    WHERE  folio       = vfolio_oper02
    AND    tipo_retiro = v_tipo_retiro;

    UPDATE ret_transf_rx
    SET    estado_solicitud   = reg_2.recibido
    WHERE  folio              = vfolio_oper02
    AND    tipo_retiro        = v_tipo_retiro;
    
    
END FUNCTION
