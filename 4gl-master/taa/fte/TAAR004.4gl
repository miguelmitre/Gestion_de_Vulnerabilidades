###############################################################################
#Proyecto          => AFORE ( MEXICO )                                        #
#Propietario       => E.F.P.                                                  #
#Programa TAAR004  => REVERSA RECEPCION ARCHIVO CUENTAS LOCALIZADAS OP 53     #
#                  => AFORE RECEPTORA                                         #
#Autor             => FERNANDO HERRERA HERNANDEZ                              #
#Fecha             => 06 DE MAYO DE 2009                                      #
#Sistema           => CTA                                                     #
###############################################################################

DATABASE safre_af

GLOBALS

    DEFINE g_param_taa    RECORD LIKE seg_modulo.*
    DEFINE s_codigo_afore LIKE tab_afore_local.codigo_afore

    DEFINE
      hoy                 DATE,
      enter               CHAR(1),
      vfecha_presentacion DATE

END GLOBALS

MAIN

    OPTIONS INPUT WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT

    CALL STARTLOG("TAAR004.log")
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
    
    INITIALIZE vfecha_presentacion TO NULL

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW TAAR0041 AT 4,4 WITH FORM "TAAR0041" ATTRIBUTE(BORDER)
    DISPLAY "TAAR004   REVERSO DE OPERACIONES CUENTAS LOCALIZADAS OP 53                     " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY " < CTRL-C > Salir " AT 1,26
    DISPLAY hoy USING "DD-MM-YYYY" AT 3,61 ATTRIBUTE(REVERSE)
    
    INPUT BY NAME vfecha_presentacion
      AFTER FIELD vfecha_presentacion
    
        IF vfecha_presentacion IS NULL THEN
           ERROR "Campo NO puede ser NULO"
           NEXT FIELD vfecha_presentacion
        END IF
    
        SELECT a.fecha_presentacion
        FROM   taa_det_notifica a
        WHERE  a.fecha_presentacion = vfecha_presentacion
        GROUP BY 1
        IF STATUS = NOTFOUND THEN
           ERROR "FECHA NO HA SIDO PROCESADO"
           SLEEP 3
           ERROR ""
           INITIALIZE vfecha_presentacion TO NULL
           CLEAR FORM
           NEXT FIELD vfecha_presentacion
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
    
    CALL reverso_ctas_loc() #rcl
    
    PROMPT  "PROCESO FINALIZADO, [Enter] para salir " ATTRIBUTE(REVERSE)
    FOR enter

    CLOSE WINDOW TAAR0041

END FUNCTION

FUNCTION reverso_ctas_loc()
#rcl------------------------


  DELETE
    FROM taa_cza_notifica
   WHERE f_transf_lote = vfecha_presentacion

  DELETE
    FROM taa_sum_notifica
   WHERE f_transf_lote = vfecha_presentacion
   
  DELETE 
    FROM taa_det_notifica
   WHERE fecha_presentacion = vfecha_presentacion

END FUNCTION

