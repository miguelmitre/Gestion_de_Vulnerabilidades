################################################################################
#Proyecto          => SISTEMA DE AFORES ( SAFRE )                              #
#Propietario       => E.F.P.                                                   #
#Programa RETC8161 => REVERSO GENERA NOTIFICACION DE DISPOSICION DE RECURSOS   #
#                     SOLICITUD OP.05 POR RETIRO "E"                           #
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
        vfolio_oper27         INTEGER

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
    OPEN WINDOW retc90111 AT 4,4 WITH FORM "RETC90111" ATTRIBUTE(BORDER)
    DISPLAY " <Ctrl-C> Salir                                                                " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC9011 REVERSO GENERA ARCHIVO DE RETIROS TOTALES ISSSTE (Ope. 27)           " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    INPUT BY NAME vfolio_oper27 WITHOUT DEFAULTS

        AFTER FIELD vfolio_oper27
            SELECT estado_solicitud
            INTO   vestado
            FROM   ret_sol_issste_tot
            WHERE  folio       = vfolio_oper27
            GROUP  BY 1

            IF STATUS = NOTFOUND THEN
                ERROR "    FOLIO INEXISTENTE"
                ATTRIBUTE(NORMAL)
                NEXT FIELD vfolio_oper27
            ELSE
                IF vestado <> reg_2.enviado THEN
                    ERROR "    ERROR...EL ESTATUS DEBE SER ENVIADO"
                    ATTRIBUTE(NORMAL)
                    NEXT FIELD vfolio_oper27
                END IF
            END IF

        ON KEY (ESC)
            SELECT estado_solicitud
            INTO   vestado
            FROM   ret_sol_issste_tot
            WHERE  folio       = vfolio_oper27
            GROUP  BY 1

            IF STATUS = NOTFOUND THEN
                ERROR "    FOLIO INEXISTENTE"
                ATTRIBUTE(NORMAL)
                NEXT FIELD vfolio_oper27
            ELSE
                IF vestado <> reg_2.enviado THEN
                    ERROR "    ERROR...EL ESTATUS DEBE SER ENVIADO"
                    ATTRIBUTE(NORMAL)
                    NEXT FIELD vfolio_oper27
                END IF
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

    CLOSE WINDOW retc90111
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
    INTO   vfolio_oper27
    FROM   ret_sol_issste_tot

END FUNCTION

FUNCTION primer_paso()
#pp-------------------

    DELETE
    FROM  dis_provision
    WHERE folio = vfolio_oper27;

    DELETE
    FROM  ret_ctr_envio
    WHERE folio       = vfolio_oper27

    DELETE
    FROM  ret_monto_sie_issste
    WHERE folio       = vfolio_oper27
 
    UPDATE ret_sol_issste_tot
    SET    acciones_siefore1  = 0,
           acciones_siefore2  = 0,
           impt_ahorro_ret    = 0,
           impt_fon_viv_liq   = 0,
           folio              = 0,
           fecha_envio        = "01010001",
           estado_solicitud   = reg_2.confirmado
    WHERE  folio              = vfolio_oper27

END FUNCTION
