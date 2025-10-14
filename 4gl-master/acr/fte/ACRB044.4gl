###############################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                             #
#Propietario       => E.F.P.                                                  #
#Programa REVCRGA  => REVERSA PROVISION Y/O LIQUIDACION CREDITOS EN GARANTIA  #
#                     (Recibe solicitud de marcaje)                           #
#Por               => MAURO MUNIZ CABALLERO                                   #
#Fecha creacion    => 28 DE JULIO DE 2000                                     #
#Actualizacion     => MODIFICACION DERL PROGRAMA ACRC003                      #
#Fecha actualiz.   => 06 DE FEBRERO DE 2003                                   #
#Sistema           => ACR                                                     #
###############################################################################

DATABASE safre_af

GLOBALS

    DEFINE g_acr_parametro RECORD LIKE seg_modulo.*
    DEFINE s_codigo_afore LIKE safre_af:tab_afore_local.codigo_afore

    DEFINE
        opcion       SMALLINT,
        vfecha       DATE,
        hoy          DATE,
        fecha_presentacion DATE,
        enter        CHAR(1),
        tot_borrados INTEGER

END GLOBALS

MAIN

    OPTIONS INPUT WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT

    CALL STARTLOG("ACRB044.log")
    CALL inicio()            #i
    CALL proceso_principal() #pp

END MAIN

FUNCTION inicio()
#i-------------

    LET hoy = today

    SELECT codigo_afore
    INTO   s_codigo_afore
    FROM   safre_af:tab_afore_local

    SELECT *
    INTO   g_acr_parametro.*
    FROM   seg_modulo
    WHERE  modulo_cod = 'acr' 

    SELECT MAX(acr_det_tra_cred.fecha_presentacion)
    INTO   fecha_presentacion
    FROM   acr_det_tra_cred

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ACRB044 AT 4,4 WITH FORM "ACRB044" ATTRIBUTE(BORDER)
    DISPLAY "ACRB044         REVERSO OPERACIONES CREDITOS EN GARANTIA                       " AT 3,1 ATTRIBUTE(REVERSE)   
    DISPLAY " < CTRL-C > Salir " AT 1,26              
    DISPLAY hoy USING "DD-MM-YYYY" AT 3,61 ATTRIBUTE(REVERSE)

    INPUT BY NAME opcion, fecha_presentacion WITHOUT DEFAULTS

      AFTER FIELD opcion
         IF opcion IS NULL THEN
            ERROR "Campo opcion reverso NO puede ser NULO"
            NEXT FIELD opcion
         ELSE

            IF opcion < 1 OR opcion > 2 THEN
               ERROR "1=> DEVOL, 2=> PROCESO"
               NEXT FIELD opcion
            ELSE
                --EXIT INPUT
               NEXT FIELD fecha_presentacion
            END IF
         END IF

      AFTER FIELD fecha_presentacion
         IF fecha_presentacion IS NULL THEN
            ERROR "Campo fecha_presentacion NO puede ser NULA"
            NEXT FIELD fecha_presentacion
         ELSE
            SELECT 'X'
            FROM   acr_det_tra_cred
            WHERE  acr_det_tra_cred.fecha_presentacion = fecha_presentacion
            GROUP BY 1

            IF SQLCA.SQLCODE = 0 THEN
                EXIT INPUT
            ELSE
                PROMPT "No existen registros para esa fecha, [Enter] p/salir"
                FOR enter
            END IF
         END IF
        ON KEY ( INTERRUPT )
            EXIT PROGRAM

    END INPUT

    WHILE TRUE
        PROMPT "ESTA SEGURO DE GENERAR EL PROCESO [S/N] ? " FOR enter
        IF enter MATCHES "[sSnN]" THEN 
            IF enter MATCHES "[sS]" THEN
                EXIT WHILE
            ELSE
                ERROR "PROCESO CANCELADO" ATTRIBUTE(REVERSE) 
                SLEEP 2
                EXIT PROGRAM
            END IF
        END IF
    END WHILE

    DISPLAY "PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)

    CASE opcion 
        WHEN 1 CALL reversa_devolucion()  #rd
        WHEN 2 CALL reversa_proceso()     #rp
    END CASE

    PROMPT  "PROCESO FINALIZADO, ",
            tot_borrados," reg. reversados, [Enter] para salir " 
            ATTRIBUTE(REVERSE) 
    FOR enter

    CLOSE WINDOW ACRB044

END FUNCTION

FUNCTION reversa_devolucion()
#rd--------------------------

    DEFINE reg_dev RECORD
        nss CHAR(11)
    END RECORD

    DECLARE cur_dev CURSOR FOR
    SELECT safre_tmp:det_devol_cred.nss_afore
    FROM   safre_tmp:det_devol_cred

    FOREACH cur_dev INTO reg_dev.nss
    END FOREACH

END FUNCTION

FUNCTION reversa_proceso()
#rp-----------------------

END FUNCTION
