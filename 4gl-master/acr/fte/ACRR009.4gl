######################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                   #
#Propietario       => EFP                                            #
#Programa          => ACRR009                                        #
#Descripcion       => REVERSA PROVISION Y/O LIQUIDACION DE DEVOLUCION#
#                     DE SALDOS POR ANUALIDADES GARANTIZADAS         #
#Sistema           => ACR                                            #
#Fecha creacion    => 15 ENERO 2010                                  #
#Por               => STEFANIE DANIELA VERA PIÑA                     #
######################################################################

DATABASE safre_af

GLOBALS

    DEFINE g_seg_modulo    RECORD LIKE seg_modulo.*
    DEFINE s_codigo_afore LIKE tab_afore_local.codigo_afore

    DEFINE
        enter         CHAR(1),
	     vproceso_cod  CHAR(5)

    DEFINE
        vfecha        ,
        HOY           DATE

    DEFINE
        vfolio        INTEGER
    
    DEFINE
        opcion        SMALLINT

END GLOBALS


MAIN

    OPTIONS INPUT WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT

    CALL inicio()            #i
    CALL proceso_principal() #pp

END MAIN


FUNCTION inicio()
#i-------------

    LET HOY = TODAY

    SELECT codigo_afore
    INTO   s_codigo_afore
    FROM   tab_afore_local

    SELECT *
    INTO   g_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = 'acr'

    LET vproceso_cod = '00058'

END FUNCTION


FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ACRR0091 AT 4,4 WITH FORM "ACRR0091" ATTRIBUTE(BORDER)
    DISPLAY " ACRR009        REVERSO DE DEVOLUCION DE SALDOS POR AG                      " AT 3,1 ATTRIBUTE(REVERSE)   
    DISPLAY " < CTRL-C > Salir " AT 1,26              
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,64 ATTRIBUTE(REVERSE)

    INPUT BY NAME vfolio, opcion

        AFTER FIELD vfolio
            IF vfolio IS NULL THEN
                ERROR " FOLIO NO PUEDES SER NULO "
                NEXT FIELD vfolio
            ELSE 
                SELECT "X"
	             FROM   con_transaccion
	             WHERE  @folio       = vfolio
	             AND    @proceso_cod = vproceso_cod
	             AND    @estado      = 40

	             IF STATUS <> NOTFOUND THEN
	                 PROMPT " FOLIO PROCESADO EN EL AREA DE CONTABILIDAD",
	                        "...<ENTER> PARA CONTINUAR" 
	                 ATTRIBUTES (REVERSE) FOR enter
	                 NEXT FIELD vfolio
                END IF

                SELECT "X"
                FROM   acr_cza_dev_ag h
                WHERE  h.folio = vfolio

                IF SQLCA.SQLCODE <> 0 THEN
                    PROMPT " EL FOLIO NO ES DEL PROCESO DE DEVOLUCION",
	                        "...<ENTER> PARA CONTINUAR"
                    ATTRIBUTES (REVERSE) FOR enter
                    NEXT FIELD vfolio
                END IF

                NEXT FIELD opcion
            END IF                                   

        AFTER FIELD opcion
            IF opcion IS NULL THEN
                ERROR " OPCION NO PUEDE SER NULA "
                NEXT FIELD opcion
            ELSE
                IF opcion < 1 OR opcion > 4 THEN
                    ERROR "REVERSAR : 1=> PROVISION, 2=> LIQUIDACION,",
		                    " 3=> PROV/LIQ, 4=>PROCESO"
                    NEXT FIELD opcion
                ELSE
                    EXIT INPUT
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

    CASE opcion 
        WHEN 1 CALL reversa_provision()  #rp
               CALL actualiza_hist_ps()     
        WHEN 2 CALL reversa_liquidacion() #rl
               CALL actualiza_histdep()     
        WHEN 3 CALL reversa_provision() #rp
               CALL reversa_liquidacion() #rl
               CALL actualiza_hist_ps()     
               CALL actualiza_histdep()     
        WHEN 4 CALL reversa_provision() #rp
               CALL reversa_liquidacion() #rl
               CALL reversa_historicos()     
    END CASE

    PROMPT  " PROCESO FINALIZADO...<ENTER> PARA SALIR " ATTRIBUTE(REVERSE) 
    FOR enter

    CLOSE WINDOW ACRR0091

END FUNCTION


FUNCTION reversa_provision()
#rp-------------------------

    DELETE 
    FROM   dis_provision
    WHERE  folio = vfolio

END FUNCTION


FUNCTION reversa_liquidacion()
#rp---------------------------

    DELETE 
    FROM   dis_cuenta
    WHERE  folio = vfolio

    UPDATE dis_provision
    SET    estado = 5
    WHERE  folio = vfolio

END FUNCTION


FUNCTION actualiza_hist_ps()
#aha-----------------------

    UPDATE acr_cza_dev_ag
    SET    estado = 1
    WHERE  folio = vfolio

    UPDATE acr_det_dev_ag
    SET    estado = 1
    WHERE  folio = vfolio

    UPDATE acr_sum_dev_ag
    SET    estado = 1
    WHERE  folio = vfolio

END FUNCTION


FUNCTION actualiza_histdep()
#aha-----------------------

    UPDATE acr_devol_ag
    SET    estado = 2
    WHERE  folio = vfolio

END FUNCTION


FUNCTION reversa_historicos()
#rh--------------------------

    DELETE 
    FROM   acr_cza_dev_ag
    WHERE  folio = vfolio

    DELETE 
    FROM   acr_det_dev_ag
    WHERE  folio = vfolio

    DELETE 
    FROM   acr_sum_dev_ag
    WHERE  folio = vfolio

    DELETE 
    FROM   acr_devol_ag
    WHERE  folio = vfolio

END FUNCTION
