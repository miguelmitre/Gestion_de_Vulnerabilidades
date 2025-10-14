###############################################################################
#Proyecto          => AFORE ( MEXICO )                                        #
#Propietario       => E.F.P.                                                  #
#Programa TAAR001  => REVERSA RECEPCION ARCHIVO DIAG VERIF IMAGENES OP 06     #
#                  => AFORE RECEPTORA                                         #
#Autor             => FERNANDO HERRERA HERNANDEZ                              #
#Fecha             => 26 DE MARZO DE 2009                                     #
#Sistema           => CTA                                                     #
#Modificado        => FERNANDO HERRERA HERNANDEZ                              #
#Fecha Modificacion=> 09 DE AGOSTO DE 2010                                    #
#                     Actualización del proceso ahora llamado Liberación de   #
#                     Pendientes y Regeneradas (Cicular Única).               #
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

    CALL STARTLOG("TAAR001.log")
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

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW TAAR0011 AT 4,4 WITH FORM "TAAR0011" ATTRIBUTE(BORDER)
    DISPLAY "TAAR001   REVERSO LIBERACION DE PENDIENTES Y REGENERADAS                       " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                            < CTRL-C > Salir                                   " AT 1,1 ATTRIBUTE(REVERSE) 
    DISPLAY " SAFRE v2.0 (OP 06-1)                                                          " AT 2,1
    DISPLAY hoy USING "DD-MM-YYYY" AT 3,61 ATTRIBUTE(REVERSE)
    
    INPUT BY NAME generar
      AFTER FIELD generar
    
        IF generar IS NULL THEN
           ERROR "Campo NO puede ser NULO"
           NEXT FIELD generar
        END IF
    
        SELECT nombre_archivo, fecha_proceso
        INTO   varchivo, vfactualiza
        FROM   taa_ctr_arh       
        WHERE  nombre_archivo = generar
        IF STATUS = NOTFOUND THEN
           ERROR "ARCHIVO NO HA SIDO PROCESADO"
           SLEEP 3
           ERROR ""
           INITIALIZE generar TO NULL
           CLEAR FORM
           NEXT FIELD generar
        ELSE
           SELECT unique fecha_presentacion
             INTO vfecha_presentacion
             FROM taa_det_devol
            WHERE f_actualiza = vfactualiza
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
    
    CALL reverso_diag_verif() #rdv
    
    PROMPT  "PROCESO FINALIZADO, [Enter] para salir " ATTRIBUTE(REVERSE)
    FOR enter

    CLOSE WINDOW TAAR0011

END FUNCTION

FUNCTION reverso_diag_verif()
#rdv------------------------

  SELECT n_folio
    FROM taa_det_devol
   WHERE fecha_presentacion = vfecha_presentacion
    INTO TEMP rev_op06;

  DELETE
    FROM taa_cza_devol
   WHERE fecha_presentacion = vfecha_presentacion

  DELETE
    FROM taa_sum_devol
   WHERE fecha_presentacion = vfecha_presentacion

  DELETE 
    FROM afi_ctr_solicitud
   WHERE n_folio IN (SELECT n_folio
                       FROM rev_op06)
    AND fecha_recepcion = vfactualiza
    AND factualiza      = vfecha_presentacion

  UPDATE afi_solicitud
     SET status_interno = 65
   WHERE n_folio IN (SELECT n_folio
                       FROM rev_op06)
     AND tipo_solicitud  = 2
     AND status_interno <> 100

  UPDATE afi_solicitud
     SET status_interno = 70
   WHERE n_folio IN (SELECT n_folio
                       FROM rev_op06)
     AND tipo_solicitud  = 9
     AND status_interno <> 100

  DELETE
    FROM int_ctr_carta  
   WHERE n_folio IN (SELECT n_folio
                       FROM rev_op06)
     AND fecha_registro = vfecha_presentacion
     AND tipo_solicitud = 2

  DELETE 
    FROM taa_det_devol
   WHERE fecha_presentacion = vfecha_presentacion

  DELETE
    FROM taa_ctr_arh
   WHERE nombre_archivo = generar

END FUNCTION

