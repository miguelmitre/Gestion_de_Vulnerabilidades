###############################################################################
#Proyecto          => AFORE ( MEXICO )                                        #
#Propietario       => E.F.P.                                                  #
#Programa TAAR002  => REVERSA RECEPCION ARCHIVO SALDOS PREVIOS OP 29          #
#                  => AFORE RECEPTORA                                         #
#Autor             => FERNANDO HERRERA HERNANDEZ                              #
#Fecha             => 26 DE MARZO DE 2009                                     #
#Sistema           => CTA                                                     #
###############################################################################

DATABASE safre_af

GLOBALS

    DEFINE g_param_taa    RECORD LIKE seg_modulo.*
    DEFINE s_codigo_afore LIKE tab_afore_local.codigo_afore

    DEFINE
      generar             CHAR(20), 
      nombre_arch         CHAR(20),
      varchivo            CHAR(20),
      opcion              SMALLINT,
      vfolio              INTEGER,
      hoy                 DATE,
      enter               CHAR(1),
      vfactualiza         DATE,
      vfecha_presentacion DATE

END GLOBALS

MAIN

    OPTIONS INPUT WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT

    CALL STARTLOG("TAAR002.log")
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
    INTO   g_param_taa.*
    FROM   seg_modulo
    WHERE  modulo_cod = 'taa'

    INITIALIZE generar     TO NULL
    INITIALIZE vfactualiza TO NULL
    INITIALIZE vfolio      TO NULL

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW TAAR0021 AT 4,4 WITH FORM "TAAR0021" ATTRIBUTE(BORDER)
    DISPLAY "TAAR002     REVERSO DE OPERACIONES SALDOS PREVIOS OP 29                        " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY " < CTRL-C > Salir " AT 1,26
    DISPLAY hoy USING "DD-MM-YYYY" AT 3,61 ATTRIBUTE(REVERSE)
    
    INPUT BY NAME generar
      AFTER FIELD generar
    
        IF generar IS NULL THEN
           ERROR "Campo NO puede ser NULO"
           NEXT FIELD generar
        END IF
    
        SELECT nombre_archivo, folio
        INTO   varchivo, vfolio
        FROM   taa_ctr_traspaso_previo       
        WHERE  nombre_archivo = generar
        IF STATUS = NOTFOUND THEN
           ERROR "ARCHIVO NO HA SIDO PROCESADO"
           SLEEP 3
           ERROR ""
           INITIALIZE generar TO NULL
           CLEAR FORM
           NEXT FIELD generar
        ELSE
           EXIT INPUT
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
    
    CALL reverso_sdo_previo() #rsp
    
    PROMPT  "PROCESO FINALIZADO, [Enter] para salir " ATTRIBUTE(REVERSE)
    FOR enter

    CLOSE WINDOW TAAR0021

END FUNCTION

FUNCTION reverso_sdo_previo()
#rdv------------------------

  DELETE
    FROM cza_tra_sdo_previo
   WHERE folio = vfolio

  DELETE
    FROM sum_tra_sdo_previo
   WHERE folio = vfolio 

  DELETE 
    FROM det_tra_sdo_previo
   WHERE folio = vfolio 

  DELETE
    FROM taa_ctr_traspaso_previo
   WHERE nombre_archivo = generar

END FUNCTION
