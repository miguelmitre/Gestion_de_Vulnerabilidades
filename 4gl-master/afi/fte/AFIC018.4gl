############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                         #
#Propietario       => E.F.P.                                               #
#Programa AFIC018  => CARGA DE ARCHIVO RESPUESTA DE DOMICILIOS MODIFICADOS #
#Sistema           => AFI.                                                 #
#Autor             => MAURO MUNIZ CABALLERO                                #
#Fecha             => 28 DE JUNIO DE 2004                                  #
############################################################################

DATABASE safre_af

GLOBALS

    DEFINE generar           CHAR(20)
    DEFINE hoy               DATE
    DEFINE xx_fecha          DATE
    DEFINE diaSig            DATE
    DEFINE enter             CHAR(1)
    DEFINE aux_pausa         CHAR(1)
    DEFINE sello             CHAR(24)
    DEFINE varchivo          CHAR(40)
    DEFINE carga             CHAR(50)
    DEFINE ejecuta           CHAR(100)
    DEFINE corr              CHAR(100)
    DEFINE total_reg         SMALLINT
    DEFINE g_plano1          SMALLINT
    DEFINE aceptar           SMALLINT
    DEFINE rechazar          SMALLINT
    DEFINE g_paramgrales     RECORD LIKE seg_modulo.*

    DEFINE reg_det RECORD
        tipo_registro        CHAR(02),
        contador_servicio    CHAR(10),
        clave_operacion      CHAR(02),
        nss_solicitud        CHAR(11),
        calle                CHAR(65),
        no_ext               CHAR(15),
        no_int               CHAR(15),
        colonia              CHAR(65),
        delegacion           CHAR(65),
        cp                   CHAR(05),
        entidad              CHAR(65),
        cod_operacion        CHAR(02),
        diag_proceso         CHAR(15)  
    END RECORD 

    DEFINE
        x_fecha,f1,f2,f3     CHAR(10),
        falta,fpafil,fnbd    DATE

    DEFINE reg_bat RECORD
        pid                  INTEGER,
        proceso_cod          INTEGER,
        opera_cod            INTEGER,
        nombre_archivo       CHAR(25)
    END RECORD

    DEFINE 
        bnd_proceso          SMALLINT,
        aux_status_interno   SMALLINT,
        cuantos              INTEGER

    DEFINE
        c_periodo            CHAR(10),
        f_periodo            DATE

    DEFINE
        G_LISTA              CHAR(200),
        g_usuario            CHAR(008),
        v_nombre_archivo     CHAR(025),
        v_imprime            CHAR(200)

END GLOBALS

MAIN

    DISPLAY " "
    DISPLAY ".1"

    CALL STARTLOG('AFIC018.log')
    CALL inicio() #i
    CALL crea_tablas()

    IF NOT bnd_proceso THEN
        DEFER INTERRUPT
        OPTIONS INPUT WRAP,
            PROMPT LINE LAST,
            ACCEPT KEY CONTROL-I
        CALL proceso_principal()   #pp
    ELSE
        CALL sube_archivo()
        CALL rescata_valores()     #rv
        CALL actualiza_bat_f(0)    #ab
    END IF

END MAIN 

FUNCTION inicio()
#----------------

    LET reg_bat.pid            = ARG_VAL(1)
    LET reg_bat.proceso_cod    = ARG_VAL(2)
    LET reg_bat.opera_cod      = ARG_VAL(3)
    LET reg_bat.nombre_archivo = ARG_VAL(4)

    LET bnd_proceso = 0

    IF reg_bat.pid THEN
        DISPLAY "INICIANDO PROCESO ..."
        LET bnd_proceso = 1
    END IF

    LET hoy        = TODAY
    LET aceptar    = 0
    LET rechazar   = 0

    SELECT *, USER
    INTO   g_paramgrales.*, g_usuario
    FROM   seg_modulo
    WHERE  modulo_cod = 'afi'

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 4,4 WITH FORM "AFIC0181" ATTRIBUTE(BORDER)
    DISPLAY " AFIC018      CARGA ARCHIVO RESPUESTA MODIFICACION DOMICILIOS                  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                           < Ctrl-C > Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)

    DISPLAY g_paramgrales.ruta_rescate AT 7,10

    INPUT BY NAME generar
        AFTER FIELD generar
        IF generar IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD generar
        END IF

        LET carga = NULL
        LET v_nombre_archivo = generar
        LET carga = g_paramgrales.ruta_rescate CLIPPED,"/",generar CLIPPED

        WHENEVER ERROR CONTINUE
            LOAD FROM carga INSERT INTO plano1
        WHENEVER ERROR STOP

        SELECT COUNT(*)
        INTO   g_plano1
        FROM   plano1

        IF g_plano1 IS NULL OR g_plano1 = 0 THEN
            ERROR "NOMBRE DE ARCHIVO INCORRECTO O VACIO, REVISE."
            SLEEP 5
            EXIT PROGRAM
        END IF

        ERROR "Procesando Informacion"
        CALL rescata_valores()

        PROMPT "Proceso finalizado, [Enter] p/salir" FOR enter

        EXIT PROGRAM

    END INPUT

END FUNCTION

FUNCTION rescata_valores()
#rv-----------------------

    CALL actualiza_datos()
    CALL revisa_datos()
    CALL despliega_resultados()

END FUNCTION

FUNCTION sube_archivo()
#sa-------------------

    LET carga = g_paramgrales.ruta_rescate CLIPPED,"/",
                reg_bat.nombre_archivo CLIPPED

    LOAD FROM carga INSERT INTO plano1

    SELECT count(*)
    INTO   cuantos
    FROM   plano1

    IF cuantos = 0 OR
       cuantos IS NULL THEN
        DISPLAY  "Program stopped, NOMBRE DE ARCHIVO INCORRECTO O VACIO"
        EXIT PROGRAM
    END IF

END FUNCTION

FUNCTION crea_tablas()
#ct-------------------

    CREATE TEMP TABLE plano1
        (n_registros          CHAR(370))

    CREATE TEMP TABLE cza_dom
        (tipo_registro        CHAR(2),
         id_servicio          CHAR(2),
         id_operacion         CHAR(2),
         tipo_origen          CHAR(2),
         cve_origen           CHAR(3),
         tipo_destino         CHAR(2),
         cve_destino          CHAR(3),
         ent_fed_envio        CHAR(3),
         fecha_presentacion   CHAR(8),
         consec_lote          CHAR(3),
         cve_mod_recep        CHAR(2),
         cod_result_op        CHAR(2),
         motivo_rechazo       CHAR(3))

    CREATE TEMP TABLE det_dom
        (tipo_registro        CHAR(02),
         contador_servicio    CHAR(10),
         clave_operacion      CHAR(02),
         nss_solicitud        CHAR(11),
         calle                CHAR(65),
         no_ext               CHAR(15),
         no_int               CHAR(15),
         colonia              CHAR(65),
         delegacion           CHAR(65),
         cp                   CHAR(05),
         entidad              CHAR(65),
         cod_operacion        CHAR(02),
         diag_proceso         CHAR(15))

    CREATE TEMP TABLE sum_dom
        (tipo_registro        CHAR(2),
         cantidad_reg         CHAR(9))

END FUNCTION

FUNCTION actualiza_datos()
#-------------------------

    DEFINE
        cont_reg   SMALLINT,
        carga_reg  CHAR(570)

    DEFINE
        campo_011  CHAR(02),
        campo_012  CHAR(02),
        campo_013  CHAR(02),
        campo_014  CHAR(02),
        campo_015  CHAR(03),
        campo_016  CHAR(02),
        campo_017  CHAR(03),
        campo_018  CHAR(03),
        campo_019  CHAR(08),
        campo_110  CHAR(03),
        campo_111  CHAR(02),
        campo_112  CHAR(02),
        campo_113  CHAR(03),

        campo_01   CHAR(02),
        campo_02   CHAR(10),
        campo_03   CHAR(02),
        campo_04   CHAR(11),
        campo_05   CHAR(65),
        campo_06   CHAR(15),
        campo_07   CHAR(15),
        campo_08   CHAR(65),
        campo_09   CHAR(65),
        campo_10   CHAR(05),
        campo_11   CHAR(65),
        campo_12   CHAR(02),
        campo_13   CHAR(15),

        campo_201  CHAR(02),
        campo_202  CHAR(09)

    LET cont_reg = 0

    SELECT COUNT(*)
    INTO   total_reg
    FROM   plano1

    DECLARE cursor_1 CURSOR FOR 
    SELECT  * 
    FROM    plano1

    FOREACH cursor_1 INTO carga_reg
        LET cont_reg = cont_reg + 1
        IF cont_reg = 1 THEN
            LET campo_011 = carga_reg[001,002]
            LET campo_012 = carga_reg[003,004]
            LET campo_013 = carga_reg[005,006]
            LET campo_014 = carga_reg[007,008]
            LET campo_015 = carga_reg[009,011]
            LET campo_016 = carga_reg[012,013]
            LET campo_017 = carga_reg[014,016]
            LET campo_018 = carga_reg[017,019]
            LET campo_019 = carga_reg[020,027]
            LET campo_110 = carga_reg[028,030]
            LET campo_111 = carga_reg[031,032]
            LET campo_112 = carga_reg[033,034]
            LET campo_113 = carga_reg[035,043]

            INSERT INTO cza_dom
            VALUES (campo_011,
                    campo_012,
                    campo_013,
                    campo_014,
                    campo_015,
                    campo_016,
                    campo_017,
                    campo_018,
                    campo_019,
                    campo_110,
                    campo_111,
                    campo_112,
                    campo_113)
        END IF

        IF cont_reg <> total_reg AND cont_reg <> 1 THEN
            LET campo_01 = carga_reg[001,002]
            LET campo_02 = carga_reg[003,012]
            LET campo_03 = carga_reg[013,014]
            LET campo_04 = carga_reg[018,028]
            LET campo_05 = carga_reg[029,093]
            LET campo_06 = carga_reg[094,108]
            LET campo_07 = carga_reg[109,123]
            LET campo_08 = carga_reg[124,188]
            LET campo_09 = carga_reg[189,253]
            LET campo_10 = carga_reg[254,258]
            LET campo_11 = carga_reg[259,323]
            LET campo_12 = carga_reg[324,325]
            LET campo_13 = carga_reg[326,328]

            INSERT INTO det_dom
            VALUES (campo_01,
                    campo_02,
                    campo_03,
                    campo_04,
                    campo_05,
                    campo_06,
                    campo_07,
                    campo_08,
                    campo_09,
                    campo_10,
                    campo_11,
                    campo_12,
                    campo_13)
        END IF

        IF cont_reg = total_reg THEN
            LET campo_201 = carga_reg[001,002]
            LET campo_202 = carga_reg[003,011]

            INSERT INTO sum_dom
            VALUES (campo_201,
                    campo_202)
        END IF

    END FOREACH

END FUNCTION

FUNCTION revisa_datos()
#----------------------

    DEFINE 
        aux_pausa    CHAR(1),
        rechazo_lote CHAR(3),
        rechazo_deta CHAR(3),
        l_reg RECORD LIKE tab_rch_lote.* ,
        x_reg RECORD LIKE tab_rdeta.* 

    DEFINE 
        rechazo_09   CHAR(02),
        rechazo_001  CHAR(02),
        rechazo_002  CHAR(02),
        rechazo_003  CHAR(02)

    DEFINE 
        uno          CHAR(3),
        dos          CHAR(3),
        tre          CHAR(3),
        cua          CHAR(3),
        cin          CHAR(3),
        diag         CHAR(3)

    DEFINE
        l_status_int SMALLINT


    # ENCABEZADO #

    SELECT tipo_registro,
           id_servicio  , 
           id_operacion 
    INTO   rechazo_001,
           rechazo_002,
           rechazo_003
    FROM   cza_dom

    SELECT *
    INTO   l_reg.*
    FROM   tab_rch_lote
    WHERE  rlote_cod = rechazo_lote

    IF STATUS  <> NOTFOUND THEN
        CLEAR SCREEN
        DISPLAY l_reg.rlote_cod AT 10,1
        DISPLAY l_reg.rlote_desc_c AT 11,1
        PROMPT "ERROR DE PROCESO, NO PUEDE CONTINUAR PROCESAR" FOR aux_pausa
        EXIT PROGRAM
    END IF

    IF rechazo_001 <> "01" THEN
        IF bnd_proceso THEN
           DISPLAY "Program stopped, Tipo de registro debe ser 01 en ENCABEZADO"
           EXIT PROGRAM
        ELSE
           CLEAR SCREEN
           DISPLAY "Tipo de registro debe ser 01 en ENCABEZADO" AT 10,1
           PROMPT "ERROR DE PROCESO, NO PUEDE CONTINUAR PROCESAR" FOR aux_pausa
           EXIT PROGRAM
        END IF
    END IF

    IF rechazo_002 <> "01" THEN
        IF bnd_proceso THEN
           DISPLAY "Program stopped, Identificador de servicio debe ser 01 en ENCABEZADO"
           EXIT PROGRAM
        ELSE
           CLEAR SCREEN
           DISPLAY "Identificador de servicio ser 01 en ENCABEZADO" AT 10,1
           PROMPT "ERROR DE PROCESO, NO PUEDE CONTINUAR PROCESAR" FOR aux_pausa
           EXIT PROGRAM
        END IF
    END IF

    IF rechazo_003 <> "15" THEN
        IF bnd_proceso THEN
           DISPLAY "Program stopped, Identificador de operacion debe ser 15 en ENCABEZADO" 
           EXIT PROGRAM
        ELSE
           CLEAR SCREEN
           DISPLAY "Identificador de operacion debe ser 15 en ENCABEZADO" AT 10,1
           PROMPT "ERROR DE PROCESO, NO PUEDE CONTINUAR PROCESAR" FOR aux_pausa
            EXIT PROGRAM
        END IF
    END IF

    # SUMARIO #

    SELECT tipo_registro
    INTO   rechazo_09 
    FROM   sum_dom

    IF rechazo_09 <> "09" THEN
        IF bnd_proceso THEN
           DISPLAY "Program stopped, Tipo de registro debe ser 09 en RESUMEN" 
           EXIT PROGRAM
        ELSE
           CLEAR SCREEN
           DISPLAY "Tipo de registro debe ser 09 en RESUMEN" AT 10,1
           PROMPT "ERROR DE PROCESO, NO PUEDE CONTINUAR PROCESAR" FOR aux_pausa
           EXIT PROGRAM
        END IF
    END IF

    # DETALLE #

    LET G_LISTA = g_paramgrales.ruta_listados CLIPPED, "/",
                  g_usuario CLIPPED, ".RES_MOD_DOM" CLIPPED,
                  hoy USING "dd-mm-yy"

    START REPORT listado TO G_LISTA
                  
    DECLARE cursor_2 CURSOR FOR 
    SELECT *
    FROM   det_dom

    FOREACH cursor_2 INTO reg_det.*
        UPDATE afi_mae_modifica
        SET    cod_operacion  = reg_det.cod_operacion,
               diag_proceso   = reg_det.diag_proceso
        WHERE  n_seguro       = reg_det.nss_solicitud
        AND    status_interno = 212 
        AND    cod_operacion  = 0
        AND    diag_proceso   = 0

        IF reg_det.cod_operacion = '01' THEN
            LET aceptar  = aceptar + 1
        ELSE
            LET rechazar = rechazar + 1
        END IF
        OUTPUT TO REPORT listado (reg_det.nss_solicitud,
                                  reg_det.cod_operacion, 
                                  reg_det.diag_proceso,
                                  aceptar, rechazar, 
                                  v_nombre_archivo)
      
    END FOREACH

    FINISH REPORT listado

    LET v_imprime = "lp ", G_LISTA CLIPPED
    RUN v_imprime

END FUNCTION

FUNCTION despliega_resultados()
#dr----------------------------

    DEFINE total_resp SMALLINT

    LET total_resp = aceptar + rechazar 

    IF bnd_proceso THEN
        DISPLAY "                TOTAL REGISTROS RECIBIDOS                "

        DISPLAY "Total de Registros del lote   : ", total_resp USING "#######&"

        DISPLAY "Registros aceptados        : ", aceptar USING "#######&"

        DISPLAY "Registros rechazados       : ", rechazar USING "#######&"
    ELSE
        DISPLAY "                TOTAL REGISTROS RECIBIDOS                 " AT 7,10 ATTRIBUTE ( REVERSE )

        DISPLAY "Total de Registros del lote     : ",
            total_resp USING "#######&" AT 9,15 ATTRIBUTE ( CYAN )

        DISPLAY "Registros aceptados        : ",
            aceptar USING "#######&" AT 10,15 ATTRIBUTE ( CYAN )

        DISPLAY "Registros rechazados       : ",
            rechazar USING "#######&" AT 11,15 ATTRIBUTE ( CYAN ) 

        DISPLAY "Nombre del Reporte: ", G_LISTA CLIPPED 
        AT 13,1 ATTRIBUTE ( CYAN ) 
    END IF

END FUNCTION

FUNCTION actualiza_bat_f(v_folio)
#ab-----------------------------

define v_cat          CHAR(600),
       vv_fecha_log   CHAR(030),
       vv_prog        CHAR(010),
       paso           CHAR(100)

define v_fecha_log DATETIME YEAR TO SECOND

define v_folio  integer
define reg_ruta RECORD LIKE seg_modulo.*

SELECT A.*
INTO reg_ruta.*
FROM  seg_modulo A
WHERE modulo_cod = "bat"
 
UPDATE bat_ctr_operacion
set    folio      = NULL ,      
       estado_cod = 4    ,
       fecha_fin  = CURRENT,
       nom_archivo = reg_bat.nombre_archivo
WHERE pid         = reg_bat.pid
and   proceso_cod = reg_bat.proceso_cod
and   opera_cod   = reg_bat.opera_cod

UPDATE bat_ctr_proceso
set    folio       = NULL ,      
       estado_cod  = 4    ,
       fecha_fin   = CURRENT
WHERE  pid         = reg_bat.pid
and    proceso_cod = reg_bat.proceso_cod

UPDATE bat_tmp_predecesor
SET    bandera_ejecuta  = 1
WHERE  pid_prod         = reg_bat.pid
AND    proceso_cod_prod = reg_bat.proceso_cod
AND    opera_cod_prod   = reg_bat.opera_cod

LET v_fecha_log = CURRENT
LET vv_fecha_log = v_fecha_log

SELECT A.programa_cod 
INTO   vv_prog 
FROM   bat_ctr_operacion A
WHERE  A.pid         = reg_bat.pid
AND    A.proceso_cod = reg_bat.proceso_cod
AND    A.opera_cod   = reg_bat.opera_cod

LET paso = "nohup:"            ,
    reg_bat.pid         USING"&&&&&",":",
    reg_bat.proceso_cod USING"&&&&&",":",
    reg_bat.opera_cod   USING"&&&&&"

                 LET v_cat = "echo '"                ,
                             vv_fecha_log[1,4]       ,   
                             vv_fecha_log[6,7]       ,  
                             vv_fecha_log[9,10]      ,  
                             vv_fecha_log[12,13]     ,   
                             vv_fecha_log[15,16]     ,    
                             vv_fecha_log[18,19]     ,
                             "|"                    ,
                             vv_prog  CLIPPED        ,
                             "|"                     ,
                             "FINOK"                ,
                             "|"                     ,
                             reg_ruta.ruta_listados CLIPPED,  
                             "/"                     ,
                             paso CLIPPED            ,
                             "'"                     ,
                             " >> "                  ,
                             reg_ruta.ruta_envio CLIPPED ,
                             "/"                     ,
                             "aad_safre.log"

                  LET v_cat = v_cat CLIPPED
                  RUN v_cat
END FUNCTION

REPORT listado (lnss, lcod_operacion, ldiag_proceso, laceptar, 
                lrechazar, lnombre_archivo)

    DEFINE 
        lnss                  CHAR(11),
        lcod_operacion        CHAR(02),
        ldiag_proceso         CHAR(15),
        laceptar              INTEGER,
        lrechazar             INTEGER,
        lnombre_archivo       CHAR(25),
        ltotal                INTEGER

    OUTPUT
        TOP MARGIN 1
        BOTTOM MARGIN 0
        LEFT   MARGIN 0
        RIGHT  MARGIN 0
        PAGE   LENGTH 60
        ORDER BY lcod_operacion

    FORMAT
        PAGE HEADER
          PRINT COLUMN 69,"Pagina: ", PAGENO USING "<<<<"
          PRINT COLUMN 02,"AFIC018",
                COLUMN 14,"RESPUESTA ARCHIVO MODIFICACION DOMICILIOS",
                COLUMN 67, hoy USING "mm-dd-yyyy"
          PRINT COLUMN 02,"_________________________________________________________________________________________________________"

          PRINT COLUMN 02, "NOMBRE DEL ARCHIVO: ", lnombre_archivo CLIPPED

          PRINT COLUMN 02,"_________________________________________________________________________________________________________"

          PRINT COLUMN 02,"NSS",
                COLUMN 25,"COD OPER",
                COLUMN 35,"DIAG PROC"

          PRINT COLUMN 02,"_________________________________________________________________________________________________________"

          SKIP 2 LINES

             
        ON EVERY ROW
          PRINT COLUMN 02,lnss,
                COLUMN 25,lcod_operacion,
                COLUMN 35,ldiag_proceso


        AFTER GROUP OF lcod_operacion
          SKIP 1 LINES
          PRINT COLUMN 02,"SUBTOTAL COD OP", lcod_operacion, ": ", 
                          GROUP COUNT(*)
          SKIP 1 LINES

        ON LAST ROW
          SELECT COUNT(*)
            INTO ltotal
            FROM det_dom

          SKIP 3 LINES
          PRINT COLUMN 02,"TOTAL REGISTROS: ", ltotal

END REPORT
