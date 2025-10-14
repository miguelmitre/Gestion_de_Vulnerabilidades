################################################################################
#Proyecto          => SISTEMA DE safre_af ( MEXICO )                           #
#Sistema           => PRO. 	                                               #
#Programa PROB0101 => REVERSA TRASPASA PREPROMOTORES AL MAESTRO DE PROMOTORES  #
#Fecha creacion    => 08 DE FEBRERO DEL 2000                                   #
#ELABORADO POR     => FRANCO ESTEBAN ULLOA VIDELA                              #
#Actualizacion     => 29 DE MARZO DEL 2004                                     #
#Fecha actualiz.   => LAURA EUGENIA CORTES GUZMAN                              #
################################################################################
DATABASE safre_af
GLOBALS                                    
    DEFINE reg_1 RECORD 
                 folio    INTEGER
           END RECORD,

           HOY            DATE,
           enter          CHAR(1)

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("PROB0101.log")

    CALL init() #i
    OPEN WINDOW prob01011  AT 4,4 WITH FORM "PROB01011" ATTRIBUTE(BORDER)
    DISPLAY "                             <Ctrl-C> Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " PROB0101     REVERSO TRASPASA AL MAESTRO DE PROMOTORES                        " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)
        
    DISPLAY " PROCESANDO INFORMACION " AT 19,2 ATTRIBUTE(REVERSE)

    INPUT BY NAME reg_1.folio WITHOUT DEFAULTS
        AFTER FIELD folio
            IF reg_1.folio IS NULL THEN
                ERROR " CAMPO NO PUEDE SER NULO "
                NEXT FIELD folio
            END IF

            SELECT "OK"
            FROM   pro_solicitud A
            WHERE  A.folio = reg_1.folio
            GROUP BY 1
 
            IF STATUS = NOTFOUND THEN
                ERROR " FOLIO INEXISTENTE "
                NEXT FIELD folio
            ELSE
                SELECT "OK"
                FROM   pro_solicitud A
                WHERE  A.folio          = reg_1.folio
                AND    A.status_interno = 5
                AND    A.diag_proceso   IN ("1A","1R","1K","4A")
                GROUP BY 1

                IF STATUS = NOTFOUND THEN
                    PROMPT " NO HAY REGISTROS PARA REVERSAR " FOR CHAR enter
                    EXIT PROGRAM
                END IF
            END IF

        ON KEY (ESC)
            IF reg_1.folio IS NULL THEN
                ERROR " CAMPO NO PUEDE SER NULO "
                NEXT FIELD folio
            END IF

            SELECT "OK"
            FROM   pro_solicitud A
            WHERE  A.folio = reg_1.folio
            GROUP BY 1
 
            IF STATUS = NOTFOUND THEN
                ERROR " FOLIO INEXISTENTE "
                NEXT FIELD folio
            ELSE
                SELECT "OK"
                FROM   pro_solicitud A
                WHERE  A.folio          = reg_1.folio
                AND    A.status_interno = 5
                AND    A.diag_proceso   IN ("1A","1R","1K","4A")
                GROUP BY 1

                IF STATUS = NOTFOUND THEN
                    PROMPT " NO HAY REGISTROS PARA REVERSAR " FOR CHAR enter
                    EXIT PROGRAM
                END IF
            END IF

            WHILE TRUE
                PROMPT " ESTA SEGURO S/N " FOR CHAR enter 
                IF enter MATCHES "[SsNn]" THEN
                    IF enter MATCHES "[Nn]" THEN
                        PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR "
                        FOR CHAR enter
                        EXIT PROGRAM
                    ELSE
                        EXIT WHILE
                    END IF
                END IF
            END WHILE

            EXIT INPUT
  
        ON KEY (INTERRUPT)
            PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
            EXIT PROGRAM

    END INPUT

    DISPLAY " PROCESANDO INFORMACION " AT 19,2 ATTRIBUTE(REVERSE)

    CALL primer_paso() #pp

    PROMPT " PROCESO FINALIZADO ...<ENTER> PARA SALIR " FOR CHAR enter
    CLOSE WINDOW prob01011
END MAIN

FUNCTION init()
#i-------------
    LET HOY = TODAY

    SELECT MAX(folio)
    INTO   reg_1.folio
    FROM   pro_solicitud A
    WHERE  A.status_interno = 5
    AND    A.diag_proceso IN ("1A","1R","1K","4A")
END FUNCTION 

FUNCTION primer_paso()
#pp-------------------
    DEFINE #loc #char
        vcod_promotor            CHAR(10)

    DECLARE cur_1 CURSOR FOR
    SELECT  A.cod_promotor 
    FROM    pro_solicitud A
    WHERE   A.folio          = reg_1.folio
    AND     A.status_interno IN(4,5)
    AND     A.diag_proceso   IN ("1A","1R","1K","4A")

    FOREACH cur_1 INTO vcod_promotor
        DELETE
        FROM   pro_mae_promotor
        WHERE  cod_promotor = vcod_promotor

        UPDATE pro_solicitud 
        SET    pro_solicitud.status_interno = 4
        WHERE  pro_solicitud.folio          = reg_1.folio
        AND    pro_solicitud.cod_promotor   = vcod_promotor

    INSERT INTO pro_mae_promotor
    SELECT A.codven          ,
           A.seguro          ,
           A.nip             ,
           A.agenc_cod       ,
           A.unico           ,
           A.rfc             ,
           A.paterno         ,
           A.materno         ,
           A.nombres         ,
           A.fingre          ,
           A.fenvio          ,
           A.fecha_registro  ,
           A.fecha_baja      ,
           A.calle           ,
           A.numero          ,
           A.dpto            ,
           A.colonia         ,
           A.deleg           ,
           A.ciudad          ,
           A.estado          ,
           A.codpos          ,
           A.fono            ,
           A.sup             ,
           A.nivel           ,
           A.resuelva        ,
           A.fnaci           ,
           A.diag_proceso    ,
           A.fautoriz        ,
           A.status          ,
           A.nro_solicitud   ,
           A.status_interno  ,
           A.fecha_certifi   ,
           A.motivo_suspende ,
           A.fecha_suspende  ,
           A.fech_credencial ,
           A.cod_promotor    ,
           A.tipo_recibo     ,
           A.escolar
    FROM   pro_his_revalida A
    WHERE  A.cod_promotor = vcod_promotor

    DELETE
    FROM   pro_his_revalida
    WHERE  cod_promotor = vcod_promotor

    END FOREACH

    INSERT INTO pro_mae_promotor
    SELECT A.codven          ,
           A.seguro          ,
           A.nip             ,
           A.agenc_cod       ,
           A.unico           ,
           A.rfc             ,
           A.paterno         ,
           A.materno         ,
           A.nombres         ,
           A.fingre          ,
           A.fenvio          ,
           A.fecha_registro  ,
           A.fecha_baja      ,
           A.calle           ,
           A.numero          ,
           A.dpto            ,
           A.colonia         ,
           A.deleg           ,
           A.ciudad          ,
           A.estado          ,
           A.codpos          ,
           A.fono            ,
           A.sup             ,
           A.nivel           ,
           A.resuelva        ,
           A.fnaci           ,
           A.diag_proceso    ,
           A.fautoriz        ,
           A.status          ,
           A.nro_solicitud   ,
           A.status_interno  ,
           A.fecha_certifi   ,
           A.motivo_suspende ,
           A.fecha_suspende  ,
           A.fech_credencial ,
           A.cod_promotor    ,
           A.tipo_recibo     ,
           A.escolar
    FROM   pro_his_baja A
    WHERE  A.folio = reg_1.folio

    DELETE
    FROM   pro_his_baja
    WHERE  folio = reg_1.folio
END FUNCTION
