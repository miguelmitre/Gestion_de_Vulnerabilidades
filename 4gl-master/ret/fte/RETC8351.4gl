################################################################################
#Proyecto          => SISTEMA DE AFORES ( SAFRE )                              #
#Propietario       => E.F.P.                                                   #
#Programa RETC8351 => REVERSO.LIQUIDACION TIPO RETIRO M                        #
#Fecha creacion    => 20 DE FEBRERO DEL 2008                                   #
#Desarrado por     => FRANCO ULLOA VIDELA                                      #
#Sistema           => RET                                                      #
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE reg_1 RECORD #glo #reg_1
        nss                   LIKE cta_his_marca.nss         ,
        marca_cod             LIKE cta_his_marca.marca_cod   ,
        correlativo           LIKE cta_his_marca.correlativo ,
        fecha_ini             LIKE cta_his_marca.fecha_ini
    END RECORD

    DEFINE reg_2 RECORD #glo #reg_2
        enviado               LIKE ret_estado.estado_solicitud ,
        liquidado             LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE #glo #integer
        vfolio_oper05         INTEGER

    DEFINE #glo #smallint
        vtipo_movimiento      SMALLINT

    DEFINE #glo #date
        HOY                   DATE

    DEFINE #glo #char
        enter                 CHAR     ,
        rev_desmarca          CHAR(100)
END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL init() #i
    OPEN WINDOW retc83511 AT 4,4 WITH FORM "RETC83511" ATTRIBUTE(BORDER)
    DISPLAY " <Ctrl-C> Salir                                              TIPO RETIRO M     " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC8081              REVERSO LIQUIDACION TIPO RETIRO M                       " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    INPUT BY NAME vfolio_oper05 WITHOUT DEFAULTS

        AFTER FIELD vfolio_oper05
            SELECT "OK"
            FROM   ret_solicitud_tx
            WHERE  folio       = vfolio_oper05
            AND    tipo_retiro = "M"
            GROUP BY 1

            IF STATUS = NOTFOUND THEN
                ERROR "    FOLIO INEXISTENTE"  ATTRIBUTE(NORMAL)
                NEXT FIELD vfolio_oper05
            END IF

            DECLARE cur_1 CURSOR FOR
            SELECT tipo_movimiento
            FROM   dis_cuenta
            WHERE  folio = vfolio_oper05
            GROUP BY 1

            FOREACH cur_1 INTO vtipo_movimiento

                IF vtipo_movimiento = 825 OR vtipo_movimiento = 10 THEN
                    # EL FOLIO ES UNICO PARA RETIRO M
                ELSE
                    ERROR "    ERROR...FOLIO NO UNICO PARA TIPO RETIRO M "
                    ATTRIBUTE(NORMAL)
                    NEXT FIELD vfolio_oper05
                END IF
            END FOREACH


        ON KEY (ESC)
            SELECT "OK"
            FROM   ret_solicitud_tx
            WHERE  folio       = vfolio_oper05
            AND    tipo_retiro = "M"
            GROUP BY 1

            IF STATUS = NOTFOUND THEN
                ERROR "    FOLIO INEXISTENTE" ATTRIBUTE(NORMAL)
                NEXT FIELD vfolio_oper05
            END IF

            DECLARE cur_2 CURSOR FOR
            SELECT tipo_movimiento
            FROM   dis_cuenta
            WHERE  folio = vfolio_oper05
            GROUP BY 1

            FOREACH cur_2 INTO vtipo_movimiento
                IF vtipo_movimiento = 825 OR vtipo_movimiento = 10 THEN
                    # EL FOLIO ES UNICO PARA RETIRO H
                ELSE
                    ERROR "    ERROR...FOLIO NO UNICO PARA TIPO RETIRO M "
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

    CLOSE WINDOW retc83511
END MAIN

FUNCTION init()
#i-------------
    LET HOY = TODAY

    SELECT A.estado_solicitud
    INTO   reg_2.enviado
    FROM   ret_estado A
    WHERE  A.descripcion = "ENVIADO"

    SELECT A.estado_solicitud
    INTO   reg_2.liquidado
    FROM   ret_estado A
    WHERE  A.descripcion = "LIQUIDADO"

    SELECT MAX(folio)
    INTO   vfolio_oper05
    FROM   ret_solicitud_tx
    WHERE  tipo_retiro      = "M"
    AND    estado_solicitud = 8
END FUNCTION

FUNCTION primer_paso()
#pp-------------------
    DECLARE cur_3 CURSOR FOR
    SELECT  A.nss         ,
            A.marca_cod   ,
            A.correlativo ,
            A.fecha_ini
    FROM    cta_his_marca A,ret_solicitud_tx B
    WHERE   A.nss         = B.nss
    AND     A.correlativo = B.consecutivo
    AND     A.marca_cod   = 825
    AND     B.folio       = vfolio_oper05
    AND     B.tipo_retiro = "M"

    FOREACH cur_3 INTO reg_1.*

        LET rev_desmarca = " EXECUTE PROCEDURE reversa_desmarca('",reg_1.nss,"',",
                                                                 reg_1.marca_cod,",",
                                                                 reg_1.correlativo,",",
                                                                 "'",reg_1.fecha_ini,"')"
        PREPARE eje_rev_desmarca FROM rev_desmarca
        EXECUTE eje_rev_desmarca
    END FOREACH

    DELETE
    FROM  dis_cuenta
    WHERE folio = vfolio_oper05;

    UPDATE ret_solicitud_tx
    SET    estado_solicitud   = reg_2.enviado
    WHERE  folio              = vfolio_oper05
    AND    tipo_retiro        = "M"
    AND    estado_solicitud   = reg_2.liquidado
END FUNCTION
