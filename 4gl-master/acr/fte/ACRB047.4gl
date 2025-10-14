###############################################################################
#Proyecto          => AFORES ( MEXICO )                                       #
#Propietario       => E.F.P.                                                  #
#Programa ACRB047  => REVERSA PROV. Y/O LIQ. DEVOLUCION DE EXCEDENTES         #
#Fecha             => 31 DE ENERO DE 2001                                     #
#Autor             => MAURO MUNIZ CABALLERO                                   #
#Modificacion      => Se modifico el programa original ACRB011                #
#Modificado por    => FERNANDO HERRERA HERNANDEZ                              #
#Sistema           => TRA                                                     #
###############################################################################

DATABASE safre_af

GLOBALS

    DEFINE g_seg_modulo    RECORD LIKE seg_modulo.*
    DEFINE s_codigo_afore LIKE tab_afore_local.codigo_afore

    DEFINE
        opcion       SMALLINT,
        vfolio       INTEGER,
        vfecha       DATE,
        hoy          DATE,
        enter        CHAR(1)

    DEFINE
        vproceso_cod CHAR(5)

END GLOBALS

MAIN

    DEFER INTERRUPT
    OPTIONS INPUT WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I

    CALL STARTLOG('ACRB047.log')
    CALL inicio()            #i
    CALL proceso_principal() #pp

END MAIN

FUNCTION inicio()
#i-------------

    LET hoy = today

    SELECT codigo_afore
    INTO   s_codigo_afore
    FROM   tab_afore_local

    SELECT *
    INTO   g_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = 'acr'

    LET vproceso_cod = '00026'

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ACRB047 AT 4,4 WITH FORM "ACRB047" ATTRIBUTE(BORDER)
    DISPLAY "ACRB047      REVERSO DE OPERACIONES DEVOLUCIONES EXCEDENTES CRED.              " AT 3,1 ATTRIBUTE(REVERSE)   
    DISPLAY " < CTRL-C > Salir " AT 1,26              
    DISPLAY hoy USING "DD-MM-YYYY" AT 3,61 ATTRIBUTE(REVERSE)

    INPUT BY NAME vfolio, opcion

      AFTER FIELD vfolio
         IF vfolio IS NULL THEN
            ERROR "Campo folio NO puede ser NULO"
            NEXT FIELD vfolio
         ELSE 

            SELECT "X"
	      FROM con_transaccion
	     WHERE @folio       = vfolio
	       AND @proceso_cod = vproceso_cod
	       AND @estado      = 40
	    IF STATUS <> NOTFOUND THEN
	       PROMPT " FOLIO PROCESADO EN EL AREA DE CONTABILIDAD,",
	              " [Enter] continuar "
	       ATTRIBUTES (REVERSE) FOR enter
	       NEXT FIELD vfolio
	    END IF

            SELECT "X"
            FROM   acr_cza_dev_cred h
            WHERE  h.folio = vfolio
            GROUP BY 1

            IF SQLCA.SQLCODE <> 0 THEN
                PROMPT " EL FOLIO NO ES DEL PROCESO DE DEVOLUCION, ",
		       " [Enter] p/continuar "
                ATTRIBUTES (REVERSE) FOR enter
                NEXT FIELD vfolio
            END IF

            NEXT FIELD opcion
         END IF                                   

      AFTER FIELD opcion
         IF opcion IS NULL THEN
            ERROR "Campo opcion reverso NO puede ser NULO"
            NEXT FIELD opcion
         ELSE
            IF opcion < 1 OR opcion > 4 THEN
                ERROR "REVERSAR : 1=> PROVISION, 2=> LIQUIDACION, ",
		      "3=> PROV/LIQ, 4=>PROCESO"
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

    DISPLAY "PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)

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

    PROMPT  "PROCESO FINALIZADO, [Enter] para salir " ATTRIBUTE(REVERSE) 
    FOR enter

    CLOSE WINDOW ACRB047

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

    UPDATE acr_cza_dev_cred
    SET    estado = 1
    WHERE  folio = vfolio

    UPDATE acr_det_dev_cred
    SET    estado = 1
    WHERE  folio = vfolio

    UPDATE acr_sum_dev_cred 
    SET    estado = 1
    WHERE  folio = vfolio

END FUNCTION

FUNCTION actualiza_histdep()
#aha-----------------------

    UPDATE acr_devol_cred
    SET    estado = 2
    WHERE  folio = vfolio

END FUNCTION

FUNCTION reversa_historicos()
#rh--------------------------

    DELETE 
    FROM   acr_cza_dev_cred
    WHERE  folio = vfolio

    DELETE 
    FROM   acr_det_dev_cred
    WHERE  folio = vfolio

    DELETE 
    FROM   acr_sum_dev_cred 
    WHERE  folio = vfolio

    DELETE 
    FROM   acr_devol_cred
    WHERE  folio = vfolio

END FUNCTION
