##############################################################################
#Proyecto          => AFORE ( MEXICO )                                       #
#Propietario       => E.F.P.                                                 #
#Programa TAAB001  => LANZA LA CARGA ARCHIVO DISPERSION CUENTA AFORE-AFORE   #
#                     PROGRAMA TAAB0011.4gl                                  #
#Fecha             => 26 DE FEBRERO 2008                                     #
#Por               => JOSUE LISANDRO HUERTA SIERRA                           #
#Sistema           => TAA                                                    #
##############################################################################
##############################################################################
#Ult. Actualizacion => MLM-3842  08Marzo2016  CPL-2609 DMR 21/06/2017        #
#                     Se Cambiaran las Sig: Tablas temporales por Fisicas    #
#                     cza_tra_afo                                            #
#                     det_tra_viv                                            #
#                     det_tra_rcv                                            #
#                     sum_tra_afo                                            #
# Ademas se Agregará el Campo nom_archivo en el cual se realizaran ahora las #
# busquedas.                                                                 #
##############################################################################
DATABASE safre_af

GLOBALS

    DEFINE bnd_proceso SMALLINT
    DEFINE HOY         DATE
    DEFINE fecha_ini   DATETIME YEAR TO SECOND
    DEFINE reg_modulo  RECORD LIKE seg_modulo.*
    DEFINE vusuario    CHAR(8)
    DEFINE reg_bat RECORD
           pid            INTEGER,
           proceso_cod    INTEGER,
           opera_cod      INTEGER,
           nombre_archivo CHAR(25)
    END RECORD
    DEFINE fliquida         DATE
    DEFINE nomarchivo       VARCHAR(20) #CPL-3956 SE PASA A VARCHAR, CPL-2609
    DEFINE vcve_ced_cuenta  CHAR(03)
    DEFINE enter            CHAR(1)     #CPL-2609

END GLOBALS


MAIN

    DISPLAY " "
    DISPLAY ".1"

    CALL STARTLOG(FGL_GETENV('USER')||'.TAAB001.log')   #MLM-3842 CPL-2609

    CALL inicio()

    IF NOT bnd_proceso THEN
        DEFER INTERRUPT
            OPTIONS INPUT WRAP,
            PROMPT LINE LAST - 1,
            ACCEPT KEY CONTROL-I

        CALL abre_ventana()
    ELSE
        CALL valida_traspaso()
    END IF

END MAIN

FUNCTION inicio()
#i-------------

    LET reg_bat.pid            = ARG_VAL(1)
    LET reg_bat.proceso_cod    = ARG_VAL(2)
    LET reg_bat.opera_cod      = ARG_VAL(3)

    LET bnd_proceso = 0

    IF reg_bat.pid THEN
        DISPLAY "INICIANDO PROCESO ..."
        LET bnd_proceso = 1
    END IF

    SELECT *
      INTO reg_modulo.*
      FROM seg_modulo
     WHERE modulo_cod = 'taa'

    LET HOY       = TODAY
    LET fecha_ini = CURRENT

END FUNCTION

FUNCTION abre_ventana()

    DEFINE opc       CHAR (1)
    DEFINE enter     CHAR (1)

    #MLM-3842 CPL-2609
    OPEN WINDOW ventana_1  AT 2,2 WITH FORM "TAAB0011" ATTRIBUTE(BORDER)

    DISPLAY " TAAB001    CARGA REGISTROS DE SALDOS TRASPASOS AFORE - AFORE                  " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY "                              < Ctrl-C > Salir                                 " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)
    
    LET fliquida = TODAY
    INITIALIZE nomarchivo TO NULL

    INPUT BY NAME fliquida,nomarchivo WITHOUT DEFAULTS
      AFTER FIELD fliquida
         IF fliquida IS NULL THEN
            ERROR "La Fecha de Liquidacion no puede ser nula"
            NEXT FIELD fliquida
         END IF

      AFTER FIELD nomarchivo
         IF nomarchivo IS NULL THEN
            ERROR "El nombre del archivo no puede ser nulo"
            NEXT FIELD nomarchivo
         END IF

         -- Validando que el nombre del archivo sea valido
         SELECT "X"
         FROM   safre_tmp:cza_tra_afo
         WHERE  nom_archivo = nomarchivo
         GROUP BY 1

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR "El nombre del archivo no es valido"
            NEXT FIELD nomarchivo
         END IF

      ON KEY (ESC)
         IF fliquida IS NULL THEN
            ERROR "La Fecha de Liquidacion no puede ser nula"
            NEXT FIELD fliquida
         END IF

         IF nomarchivo IS NULL THEN
            ERROR "El nombre del archivo no puede ser nulo"
            NEXT FIELD nomarchivo
         END IF

         -- Validando que el nombre del archivo sea valido
         SELECT "X"
         FROM   safre_tmp:cza_tra_afo
         WHERE  nom_archivo = nomarchivo
         GROUP BY 1

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR "El nombre del archivo no es valido"
            NEXT FIELD nomarchivo
         END IF

         EXIT INPUT

      ON KEY (CONTROL-C)
         ERROR "PROCESO CANCELADO "
         SLEEP 3
         EXIT PROGRAM

      ON KEY (INTERRUPT)
         ERROR "PROCESO CANCELADO "
         SLEEP 3
         EXIT PROGRAM

    END INPUT

    #--

    WHILE TRUE
        ERROR""
        PROMPT "¿ DESEA GENERAR EL PROCESO [S/N] ? " FOR opc
        IF opc <> "S" AND
           opc <> "s" AND
           opc <> "n" AND
           opc <> "N" THEN
            ERROR "SOLO INDIQUE S o N "
            SLEEP 3
            ERROR ""
            CONTINUE WHILE
        ELSE
            IF opc = "S" OR
               opc = "s" THEN
                EXIT WHILE
            ELSE
                IF opc = "N" OR
                   opc = "n" THEN
                    ERROR "PROCESO CANCELADO."
                    SLEEP 3
                    ERROR ""
                    EXIT PROGRAM
                END IF
            END IF
        END IF
    END WHILE

    SELECT 'X'
    FROM   safre_tmp:det_tra_viv
    WHERE  estado_reg    =  2
    AND    nom_archivo   =  nomarchivo
    GROUP BY 1

    IF SQLCA.SQLCODE = 0 THEN
        PROMPT "Existen registros con rechazos, [Enter] p/salir "
        ATTRIBUTES (REVERSE) FOR enter
        EXIT PROGRAM
    END IF

    CALL lanza_proceso()

END FUNCTION

FUNCTION lanza_proceso()

    DEFINE id_op              SMALLINT
    DEFINE vfolio             INTEGER
    DEFINE eje_cadena         CHAR(300)
    DEFINE reg_ctr_traspaso   RECORD LIKE safre_af:taa_ctr_traspaso.*
    DEFINE reg_cza_tras       RECORD LIKE safre_tmp:cza_tra_afo.*
    DEFINE enter              CHAR (1)
    DEFINE v_cad              CHAR(500)   -- CPL-3956


    -- CPL-3956 INI SE PASAN A PREPARE 
    LET v_cad = '',
                '\n SELECT *, USER                                ',
                '\n   FROM safre_tmp:cza_tra_afo                  ',
                '\n  WHERE nom_archivo = "',nomarchivo CLIPPED,'" '   #MLM-3842 CPL-2609
    PREPARE con_cza FROM v_cad   
    EXECUTE con_cza INTO reg_cza_tras.*, vusuario

    LET v_cad = '',
                '\n SELECT FIRST 1 *                                                  ',  --- CPL-3956 - FALLA AQUI MAS DE 1 REGISTRO
                '\n   FROM taa_ctr_traspaso                                           ',                      
                '\n  WHERE fecha_presentacion = "',reg_cza_tras.fecha_presentacion,'" ',
                '\n    AND id_operacion       = "',reg_cza_tras.ident_operacion,'"    ',
                '\n    AND folio             IS NULL                                  ',
                '\n    AND nombre_archivo     = "',nomarchivo CLIPPED,'"              '
                
    PREPARE con_reg FROM v_cad
    EXECUTE con_reg INTO reg_ctr_traspaso.*
    -- CPL-3956 FIN SE PASAN A PREPARE     

    IF reg_ctr_traspaso.ini_incorpora IS NOT NULL OR
       reg_ctr_traspaso.ini_incorpora <> DATETIME (1899-12-31) YEAR TO DAY THEN
        IF reg_ctr_traspaso.fin_incorpora IS NOT NULL OR
           reg_ctr_traspaso.fin_incorpora <> DATETIME (1899-12-31) YEAR TO DAY THEN
             IF reg_bat.pid THEN
                 DISPLAY "LOS REGISTROS YA FUERON INCORPORADOS."
                 EXIT PROGRAM
             ELSE
                 PROMPT "LOS REGISTROS YA FUERON INCORPORADOS. [Enter] p/salir "
                    FOR enter
                 EXIT PROGRAM
             END IF
        ELSE
             IF reg_bat.pid THEN
                 DISPLAY "PROCESO AUN NO FINALIZA, CONTINUA POR NOHUP."
                 EXIT PROGRAM
             ELSE
                 PROMPT "PROCESO AUN NO FINALIZA, CONTINUA POR NOHUP. [Enter] p/salir "
                    FOR enter
                 EXIT PROGRAM
             END IF
        END IF
    END IF

    CASE reg_cza_tras.ident_operacion
        WHEN '09' LET id_op = 3
        WHEN '12' LET id_op = 4
    END CASE
    
    SELECT unique cve_ced_cuenta
    INTO   vcve_ced_cuenta
    FROM   safre_tmp:det_tra_viv
    WHERE  fecha_presentacion = reg_cza_tras.fecha_presentacion
    AND    ident_operacion    = reg_cza_tras.ident_operacion
    AND    cve_ced_cuenta     = '531'
    AND    nom_archivo        = nomarchivo   #MLM-3842 CPL-2609
    GROUP BY 1
    IF STATUS <> NOTFOUND THEN
       CASE reg_cza_tras.ident_operacion
         WHEN '09' LET id_op = 31
         WHEN '12' LET id_op = 32
       END CASE   
    END IF

    {SELECT UNIQUE folio
    INTO   vfolio
    FROM   taa_folio
    WHERE  fecha = reg_cza_tras.fecha_presentacion
    AND    tipo = id_op

    IF SQLCA.SQLCODE <> 0 THEN }
        INSERT INTO glo_folio VALUES (0)

        SELECT MAX(folio), USER
        INTO   vfolio
        FROM   glo_folio

        INSERT INTO taa_folio VALUES(vfolio, id_op, reg_cza_tras.fecha_presentacion, vusuario)
    --END IF

    IF bnd_proceso THEN
        DISPLAY "F O L I O : ",vfolio 
    ELSE
        DISPLAY "F O L I O              : ",vfolio USING "#######" AT 11,05
        SLEEP 3
    END IF

    UPDATE taa_ctr_traspaso
       SET folio         = vfolio,
           ini_incorpora = fecha_ini,
           usr_incorpora = vusuario
     WHERE fecha_presentacion = reg_cza_tras.fecha_presentacion
       AND id_operacion       = reg_cza_tras.ident_operacion
       AND folio             IS NULL

    IF bnd_proceso THEN
        DISPLAY "PROCESO CONTINUA EJECUTANDOSE POR NO-HUP, [Enter] p/salir"
    ELSE
        PROMPT "PROCESO CONTINUA EJECUTANDOSE POR NO-HUP, [Enter] p/salir" FOR enter
    END IF

    LET eje_cadena = " nohup fglgo ",reg_modulo.ruta_exp CLIPPED,
                    "/TAAB0011 ", " ", vfolio, " '", fliquida, "' ",
                     " ", nomarchivo CLIPPED, "  ", #MLM-3842 CPL-2609
                    " ",reg_bat.pid ,
                    " ", reg_bat.proceso_cod,
                    " ", reg_bat.opera_cod, "  &"

    RUN eje_cadena

END FUNCTION

FUNCTION valida_traspaso()

    SELECT 'X'
      FROM safre_tmp:det_tra_viv
     WHERE estado_reg     = 2
     AND   nom_archivo    = nomarchivo  #MLM-3842 CPL-2609
     GROUP BY 1
 
    IF SQLCA.SQLCODE = 0 THEN
        DISPLAY "Existen registros con rechazos, [Enter] p/salir "
        EXIT PROGRAM
    END IF
 
    CALL lanza_proceso()

END FUNCTION
