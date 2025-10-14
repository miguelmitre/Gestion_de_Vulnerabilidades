#############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                          #
#Propietario       => E.F.P.                                                #
#Programa AFIB028  => GENERACION ARCHIVO PLANO PARA PROCESAR (ACTIVACION)   #
#Sistema           => AFI.                                                  #
#Autor             => FERNANDO HERRERA HERNANDEZ                            #
#Fecha             => 15 DE ABRIL DE 2006                                   #
#############################################################################

DATABASE safre_af

GLOBALS

    DEFINE
        generar     CHAR(1),
        enter       CHAR(1),
        g_opcion    CHAR(2),
        g_usuario   CHAR(8),
        HORA        CHAR(8),
        G_LISTA     CHAR(200),
        comm        CHAR(500)

    DEFINE
        HOYDIA        ,
        fecha_hasta   ,
        HOY           DATE

    DEFINE
        bnd_proceso ,
        h_corr      ,
        h_corrd     ,
        num         SMALLINT

    DEFINE
        HAY_REGISTROS ,
        vcont         INTEGER

    DEFINE w_aux RECORD
        nss              LIKE afi_sol_activacion.nss,
        curp             LIKE afi_sol_activacion.curp,
        paterno          LIKE afi_sol_activacion.paterno,
        materno          LIKE afi_sol_activacion.materno,
        nombres          LIKE afi_sol_activacion.nombres
    END RECORD

    DEFINE k_aux RECORD
        tipo_reg         CHAR(02),
        con_dservicio    CHAR(10),
        clave_doperacion CHAR(02),
        fech_recep_afor  CHAR(08),
        folio_solicitud  CHAR(10)
    END RECORD

    DEFINE g_paramgrales RECORD LIKE seg_modulo.*
    DEFINE g_afore       RECORD LIKE tab_afore_local.*
    DEFINE x_lotes       RECORD LIKE tab_lote.*

    DEFINE reg_bat RECORD
        pid            INTEGER,
        proceso_cod    INTEGER,
        opera_cod      INTEGER,
        nombre_archivo CHAR(25)
    END RECORD

    DEFINE
        consec    CHAR(10),
        consecd   CHAR(10)

END GLOBALS

MAIN

    DISPLAY " "
    DISPLAY ".1"

    CALL STARTLOG('AFIB028.log')
    CALL inicio()     #i
    CALL inicializa() #iz

    IF NOT bnd_proceso THEN
        DEFER INTERRUPT
        OPTIONS INPUT WRAP,
            PROMPT LINE LAST,
            ACCEPT KEY CONTROL-I

        CALL proceso_principal()   #pp
    ELSE
        #CALL fecha_max()
        CALL rescata_valores()     #rv
        CALL actualiza_operacion() #ao
    END IF

END MAIN

FUNCTION inicio()
#i---------------

    LET reg_bat.pid          = ARG_VAL(1)
    LET reg_bat.proceso_cod  = ARG_VAL(2)
    LET reg_bat.opera_cod    = ARG_VAL(3)

    LET bnd_proceso = 0

    IF reg_bat.pid THEN
        DISPLAY "INICIANDO PROCESO ..."
        LET bnd_proceso = 1
    END IF

    LET HOYDIA                 = TODAY
    LET HOY                    = TODAY
    LET HORA                   = TIME
    LET num                    = 1
    LET g_opcion               = '02'
    LET k_aux.tipo_reg         = "02"  
    LET k_aux.con_dservicio    = "          "
    LET k_aux.clave_doperacion = "63" 
    LET k_aux.fech_recep_afor  = "       "
    LET k_aux.folio_solicitud  = "          "

    SELECT *
    INTO   g_paramgrales.*
    FROM   seg_modulo
    WHERE  modulo_cod = 'afi'

END FUNCTION

FUNCTION inicializa()
#iz--------------

    INITIALIZE w_aux.* TO NULL

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 4,4 WITH FORM "AFIB0081" ATTRIBUTE(BORDER)
    DISPLAY " AFIB028    GENERACION DE ARCHIVO PROCESAR (ACTIVACION)                        " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY "                                 < Ctrl-C > Salir                           " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)

    DISPLAY g_paramgrales.ruta_envio AT 10,10

    INPUT BY NAME generar

    AFTER FIELD generar
        IF generar IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD generar
        END IF

        IF generar  MATCHES "[Ss]" THEN
            #CALL fecha_max()
            CALL rescata_valores()

            IF NOT HAY_REGISTROS THEN
                ERROR "NO HAY REGISTROS PARA PROCESAR"
                SLEEP 3
                EXIT PROGRAM
            END IF

            EXIT INPUT
        ELSE
            ERROR "PROCESO CANCELADO" SLEEP 2
            EXIT PROGRAM
        END IF

        ON KEY ( INTERRUPT )
            ERROR "PROCESO CANCELADO" SLEEP 2
            EXIT PROGRAM

    END INPUT

    PROMPT "NOMBRE ARCHIVO PLANO : ", comm CLIPPED,
           " /[Enter] para salir" FOR enter

END FUNCTION

FUNCTION rescata_valores()
#rv-----------------------

    DEFINE
        contador  INTEGER

    LET contador  = 0 

    LET HAY_REGISTROS = FALSE

    SELECT COUNT(*)
    INTO   HAY_REGISTROS
    FROM   afi_mae_afiliado a, afi_sol_activacion b
    WHERE  b.status_interno in(10)
    AND    a.n_unico        = b.curp
    AND    b.cod_operacion  = 0

    LET contador = HAY_REGISTROS

    IF NOT bnd_proceso THEN
        DISPLAY "REGISTROS A SER ENVIADOS ", contador AT 16,20
        SLEEP 3
    ELSE
        DISPLAY "REGISTROS A SER ENVIADOS ", contador
    END IF 

    IF HAY_REGISTROS THEN
        SELECT *
        INTO   x_lotes.*
        FROM   tab_lote
        WHERE  lotes_cod   = 63
        AND    lotes_fecha = HOYDIA

        IF STATUS <> NOTFOUND THEN
            UPDATE tab_lote
            SET    lotes_correlativo = lotes_correlativo + 1
            WHERE  lotes_cod         = 63
            AND    lotes_fecha       = HOYDIA
        ELSE
            INSERT INTO tab_lote
            VALUES(HOYDIA     ,
                   63          ,
                   'ACTIVACION NO AFIL',
                   1          ,
                   1)
        END IF

        SELECT @lotes_correlativo
        INTO   h_corr
        FROM   tab_lote
        WHERE  lotes_cod   = 2
        AND    lotes_fecha = HOYDIA

        LET consec = h_corr

        SELECT l.lotes_correlativo
        INTO   h_corrd
        FROM   tab_lote l
        WHERE  l.lotes_cod   = 2
        AND    l.lotes_fecha = HOYDIA

        LET consecd = h_corrd

        SELECT *,USER
        INTO   g_afore.*,g_usuario
        FROM tab_afore_local

        DECLARE curs_1 CURSOR FOR
        SELECT b.nss, 
               b.curp,
               b.paterno,
               b.materno,
               b.nombres
        FROM   afi_mae_afiliado a, afi_sol_activacion b
        WHERE  b.status_interno IN(10) # STATUS CAPTURA ACTIVACION 
        AND    a.n_unico        = b.curp
        AND    b.cod_operacion  = 0

        LET comm = g_paramgrales.ruta_envio CLIPPED,"/E",
                   HOYDIA USING "yyyymmdd" CLIPPED,
                   consec CLIPPED, ".ACT" CLIPPED

        DISPLAY "Archivo: ", comm CLIPPED

        LET G_LISTA = g_paramgrales.ruta_listados CLIPPED,"/",
                      g_usuario CLIPPED, ".ACT." CLIPPED,
                      HOY USING "dd-mm-yyyy","_", consec CLIPPED,
                      HORA CLIPPED
 
        START REPORT listado_2 TO G_LISTA
        START REPORT listado TO comm

        FOREACH curs_1 INTO w_aux.*

          LET HAY_REGISTROS = TRUE

          IF w_aux.materno IS NULL OR
             w_aux.materno MATCHES ' *' THEN
             LET w_aux.materno = "N/A"
          END IF

          OUTPUT TO REPORT listado(k_aux.tipo_reg,
                                   k_aux.con_dservicio,
                                   k_aux.clave_doperacion,
                                   w_aux.nss,
                                   w_aux.curp,
                                   w_aux.paterno,
                                   w_aux.materno,
                                   w_aux.nombres)


          OUTPUT TO REPORT listado_2(k_aux.tipo_reg,
                                     k_aux.con_dservicio,
                                     k_aux.clave_doperacion,
                                     w_aux.nss,
                                     w_aux.curp,
                                     w_aux.paterno,
                                     w_aux.materno,
                                     w_aux.nombres)

          LET vcont = vcont + 1

          {UPDATE afi_mae_afiliado
          SET    status_interno = 130,
                 status_captura = 130,
                 lote           = h_corr,
                 fecha_envio    = HOYDIA
          WHERE  n_unico        = w_aux.n_unico
          AND    status_interno = 120}

          UPDATE afi_sol_activacion
          SET    status_interno = 30,
                 fecha_envio    = TODAY
          WHERE  curp           = w_aux.curp
          AND    status_interno = 10
          AND    cod_operacion  = 0
          
          IF vcont > 9999 THEN
             EXIT FOREACH
          END IF

          CALL inicializa()

        END FOREACH

        FINISH REPORT listado
        FINISH REPORT listado_2

        CALL limpia_nulos()

        DISPLAY "REGISTROS A SER ENVIADOS ", contador AT 16,20

        LET G_LISTA = "chmod 777 ",g_paramgrales.ruta_listados CLIPPED,"/",
                      g_usuario CLIPPED,".ACT." 
                      CLIPPED,HOY USING "dd-mm-yyyy","_", consec CLIPPED,
                      HORA CLIPPED
        RUN G_LISTA
    END IF

    IF NOT HAY_REGISTROS THEN
        DISPLAY "Program stopped, NO HAY REGISTROS PARA PROCESAR..."
        EXIT PROGRAM
    END IF

END FUNCTION

REPORT listado(w_aux)
#--------------------
    DEFINE w_aux  RECORD
        tipo_reg         CHAR(02),
        con_dservicio    CHAR(10),
        clave_doperacion CHAR(02),
        nss              LIKE afi_sol_activacion.nss,
        curp             LIKE afi_sol_activacion.curp,
        paterno          CHAR(40),
        materno          CHAR(40),
        nombres          CHAR(40)
    END RECORD

    DEFINE
        d CHAR(02),
        m CHAR(02),
        a CHAR(02)

    DEFINE
        dia    CHAR(02),
        mes    CHAR(02),
        ano    CHAR(04),
        dia1   CHAR(02),
        mes1   CHAR(02),
        ano1   CHAR(04)

    DEFINE 
        hoy_env    CHAR(8),
        num10      CHAR(10),
        tot_char   CHAR(9)

    DEFINE x_afore RECORD LIKE tab_afore_local.*

    DEFINE 
        tot ,
        var SMALLINT

    OUTPUT
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0
        PAGE LENGTH   60

    FORMAT
    FIRST PAGE HEADER
        LET hoy_env = YEAR(HOYDIA) USING "&&&&",
                      MONTH(HOYDIA)USING "&&"  ,
                      DAY(HOYDIA)  USING "&&"

        SELECT * 
        INTO   x_afore.*
        FROM   tab_afore_local

        IF g_opcion IS NULL THEN
            LET g_opcion= '02'
        END IF

        PRINT COLUMN 001,"01",
                         "01",
                         "63",
                         "01",
                         x_afore.codigo_afore USING "&&&",
                         "03",
                         "001",
                         hoy_env,
                         "001",
                         173 SPACES

    ON EVERY ROW
        LET dia1  = DAY(TODAY)             USING"&&"
        LET mes1  = MONTH(TODAY)           USING"&&"
        LET ano1  = YEAR(TODAY)            USING "&&&&"
        LET num10 = "          "
        LET num10 = num                    USING "&&&&&&&&&&"

        PRINT
            COLUMN 1,"02",
                     "01",
                     "63",
                     w_aux.nss,
                     w_aux.curp,
                     w_aux.paterno,
                     w_aux.materno,
                     w_aux.nombres,
                     45 SPACES


        LET num = num + 1

    ON LAST ROW
        LET tot = 0
        LET tot = count(*)
        LET tot_char = tot USING "&&&&&&&&&"

        PRINT COLUMN 001,"09",
                          tot_char,
                          189 SPACES

END REPORT

REPORT listado_2(w_aux)
#--------------------
    DEFINE w_aux  RECORD
            tipo_reg         CHAR(02),
            con_dservicio    CHAR(10),
            clave_doperacion CHAR(02),
            nss              LIKE afi_sol_activacion.nss,
            curp             LIKE afi_sol_activacion.curp,
            paterno          CHAR(40),
            materno          CHAR(40),
            nombres          CHAR(40)
    END RECORD
    
    DEFINE
        d CHAR(02),
        m CHAR(02),
        a CHAR(04)

    DEFINE
        dia      CHAR(02),
        mes      CHAR(02),
        ano      CHAR(04),
        dia1     CHAR(02),
        mes1     CHAR(02),
        ano1     CHAR(04),
        hoy      CHAR(8),
        num10    CHAR(10),
        tot_char CHAR(9)

    DEFINE x_afore RECORD LIKE tab_afore_local.*

    DEFINE
        tot SMALLINT

    OUTPUT
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0
        PAGE LENGTH   66

    FORMAT
    PAGE HEADER
        LET hoy = YEAR(TODAY) USING "&&&&",
                  MONTH(TODAY)USING "&&"  ,
                  DAY(TODAY)  USING "&&"

        SELECT * 
        INTO   x_afore.*
        FROM   tab_afore_local

       PRINT
           COLUMN 01,"=================================================",
           COLUMN 50,"=============================="
       PRINT
           COLUMN 01,x_afore.razon_social  CLIPPED,
           COLUMN 35,"LISTA DE SOLICITUDES DE ACTIVACION ",
           COLUMN 60,"FECHA     :",TODAY     USING "DD-MM-YYYY"
       PRINT
           COLUMN 01,"AFIB028",
           COLUMN 35,"ENVIADOS  A PROCESAR",
           COLUMN 60,"Nro.PAGINA:",PAGENO    USING "##########"
       PRINT
           COLUMN 01,"=================================================",
           COLUMN 50,"=============================="

       PRINT 
           COLUMN 01,"CURP",
           COLUMN 19,"NSS",
           COLUMN 38,"A.PATERNO",
           COLUMN 54,"A.MATERNO"

       PRINT 
           COLUMN 01,"NOMBRES"

       PRINT
           COLUMN 01,"-------------------------------------------------",
           COLUMN 50,"------------------------------"
        
                 
    ON EVERY ROW
        LET dia1  = DAY(TODAY)             USING"&&"
        LET mes1  = MONTH(TODAY)           USING"&&"
        LET ano1  = YEAR(TODAY)            USING "&&&&"
        LET num10 = "          "
        LET num10 = num                    USING "&&&&&&&&&&"

        PRINT
            COLUMN 01,w_aux.curp,
            COLUMN 19,w_aux.nss,
            COLUMN 38,w_aux.paterno [1,15],
            COLUMN 54,w_aux.materno [1,15]                      
        PRINT
            COLUMN 01,w_aux.nombres [1,20]

END REPORT

FUNCTION Tipo_de_envio()
#te---------------------

    DEFINE z_reg ARRAY[6] OF RECORD
        cod  CHAR(2),
        desc CHAR(20)
    END RECORD

    DEFINE
        i SMALLINT

    LET z_reg[1].cod = "01" LET z_reg[1].desc = "CINTA"
    LET z_reg[2].cod = "02" LET z_reg[2].desc = "TRA"
    LET z_reg[3].cod = "03" LET z_reg[3].desc = "EN LINEA"
    LET z_reg[4].cod = "04" LET z_reg[4].desc = "LIB.PENDIENTE"
    LET z_reg[5].cod = "05" LET z_reg[5].desc = "CARTUCHO"
    LET z_reg[6].cod = "06" LET z_reg[6].desc = "DAT"

    OPEN WINDOW cent AT 11,25 WITH FORM "AFIB0272" ATTRIBUTE(BORDER)
    DISPLAY "       MEDIOS  DE  ENVIO      " AT 2,1 ATTRIBUTE(REVERSE)

    FOR i = 1 TO 6
        DISPLAY z_reg[i].* TO scr_1[i].*
    END FOR

    INPUT BY NAME g_opcion
    AFTER FIELD g_opcion
        IF g_opcion IS NULL THEN
            ERROR "OPCION NO PUEDE SER NULA"
            NEXT FIELD g_opcion
        END IF

        IF g_opcion <> "01" AND
           g_opcion <> "02" AND
           g_opcion <> "03" AND
           g_opcion <> "04" AND
           g_opcion <> "05" AND
           g_opcion <> "06" THEN
            ERROR "OPcion ERRONEA, reingrese"
            NEXT FIELD g_opcion
        ELSE
            EXIT INPUT
            END IF
        END INPUT

    CLOSE WINDOW cent

END FUNCTION

FUNCTION limpia_nulos()

    LET comm = "cd " ,g_paramgrales.ruta_envio CLIPPED,
               "/ ; sed -e '/^$/d' ","E", HOYDIA USING "yyyymmdd" CLIPPED,
               consec CLIPPED,".ACT > informix" 

    RUN comm

    LET comm = "cd " ,g_paramgrales.ruta_envio CLIPPED ,
               "/ ; mv informix  ","E", HOYDIA USING "yyyymmdd" CLIPPED,
               consec CLIPPED,".ACT " 

    RUN comm

END FUNCTION

FUNCTION actualiza_operacion()
#ao---------------------------

    UPDATE bat_ctr_operacion
    SET    estado_operacion = 4,
           fecha_fin        = CURRENT,
           nombre_archivo   = nom_afi
    WHERE  pid              = reg_bat.pid
    AND    proceso_cod      = reg_bat.proceso_cod
    AND    opera_cod        = reg_bat.opera_cod

    UPDATE bat_ctr_proceso
    SET    estado_proceso = 4,
           fecha_fin      = CURRENT
    WHERE  pid            = reg_bat.pid
    AND    proceso_cod    = reg_bat.proceso_cod

END FUNCTION

FUNCTION fecha_max()
#fh-------------------

    DEFINE diaSemana SMALLINT

    LET diaSemana = WEEKDAY(HOY)

    LET fecha_hasta = HOY - diaSemana UNITS DAY

    CALL fechas(fecha_hasta) RETURNING HOYDIA

END FUNCTION

FUNCTION fechas(diaActual)
#sdh----------------------

    DEFINE
        diaTmp    DATE,
        contador  SMALLINT,
        diaActual DATE,
        numDias   SMALLINT

    LET diaTmp = diaActual

    FOR contador = 1 TO 3
        IF contador = 1 THEN
            CALL habil_siguiente(diaTmp) RETURNING diaTmp
        ELSE
            LET diaTmp = diaTmp + 1 UNITS DAY
            CALL habil_siguiente(diaTmp) RETURNING diaTmp
        END IF
    END FOR

    RETURN diaTmp

END FUNCTION

FUNCTION habil_siguiente(diaActual)
#hs--------------------------------

    DEFINE
        diaTmp    DATE,
        contador  SMALLINT,
        diaActual DATE

    DEFINE
        diaHabilSig DATE,
        diaSemana   SMALLINT,
        feriado     SMALLINT,
        finSemana   SMALLINT

    LET diaHabilSig = diaActual

    WHILE TRUE
        LET feriado   = 0
        LET finSemana = 0
        LET diaSemana = WEEKDAY(diaHabilSig)

        IF diaSemana = 0 OR diaSemana = 6 THEN
            LET finSemana = 1
        END IF

        SELECT *
        FROM   tab_feriado
        WHERE  feria_fecha = diaHabilSig

        IF STATUS <> NOTFOUND THEN
            LET feriado = 1
        END IF

        IF feriado = 1 OR finSemana = 1 THEN
            LET diaHabilSig = diaHabilSig + 1 UNITS DAY
        ELSE
            EXIT WHILE
        END IF
    END WHILE

    RETURN diaHabilSig

END FUNCTION

