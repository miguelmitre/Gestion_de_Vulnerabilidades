#############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                          #
#Propietario       => E.F.P.                                                #
#Programa AFIB008  => GENERACION ARCHIVO PLANO PARA PROCESAR (MODIFICADOS)  #
#Sistema           => AFI.                                                  #
#Autor             => Mauro Muniz Caballero                                 #
#Fecha             => 18 DE ENERO DE 2001                                   #
#Actualizo         => Mauro Muniz Caballero (Proceso Batch)                 #
#Actualizo         => Ferando Herrera Hernandez                             #
#Fecha actualiza   => OCTUBRE DE 2003                                       #
#                     Adecuacion cod error origen 9999 y cambios segun MPT  #
#Actualizo         => Mauro Muniz Caballero                                 #
#Fecha             => 5 DE FEBRERO DE 2004                                  #
#                     Adecuaciones segun MPT                                #
#Actualizo         => Fernando Herrera Hernandez                            #
#Fecha             => 17 DE AGOSTO DE 2005                                  #
#                     Circular 7-10                                         #
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
        comm        CHAR(500),
        list_salida CHAR(500)

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
        verror        ,
        vfol_prob     ,
        vcont         ,
        verror_or     INTEGER

    DEFINE w_aux RECORD
        n_seguro         LIKE afi_mae_afiliado.n_seguro,
        n_unico          LIKE afi_mae_afiliado.n_unico,
        n_rfc            LIKE afi_mae_afiliado.n_rfc,
        paterno          LIKE afi_mae_afiliado.paterno,
        materno          LIKE afi_mae_afiliado.materno,
        nombres          LIKE afi_mae_afiliado.nombres,
        fena             DATE,
        cod_promotor     LIKE afi_mae_afiliado.cod_promotor,
        sexo             LIKE afi_mae_afiliado.sexo,
        estadon          LIKE afi_mae_afiliado.estadon,
        n_folio          LIKE afi_mae_afiliado.n_folio,
        frecafor         LIKE afi_mae_afiliado.frecafor,
        nacionalidad     LIKE afi_mae_afiliado.nacionalidad,
        tip_prob         LIKE afi_mae_afiliado.tip_prob,
        fol_prob         LIKE afi_mae_afiliado.fol_prob,
        doc_prob         LIKE afi_mae_afiliado.doc_prob,
        ind_infonavit    LIKE afi_mae_afiliado.ind_infonavit,
        cod_error_origen SMALLINT,
        const_curp       SMALLINT
    END RECORD

    DEFINE w_ant RECORD
        n_seguro         LIKE afi_mae_afiliado.n_seguro,
        n_unico          LIKE afi_mae_afiliado.n_unico,
        n_rfc            LIKE afi_mae_afiliado.n_rfc,
        paterno          LIKE afi_mae_afiliado.paterno,
        materno          LIKE afi_mae_afiliado.materno,
        nombres          LIKE afi_mae_afiliado.nombres,
        fena             DATE,
        cod_promotor     LIKE afi_mae_afiliado.cod_promotor,
        sexo             LIKE afi_mae_afiliado.sexo,
        estadon          LIKE afi_mae_afiliado.estadon,
        n_folio          LIKE afi_mae_afiliado.n_folio,
        frecafor         LIKE afi_mae_afiliado.frecafor,
        nacionalidad     LIKE afi_mae_afiliado.nacionalidad,
        tip_prob         LIKE afi_mae_afiliado.tip_prob,
        fol_prob         LIKE afi_mae_afiliado.fol_prob,
        doc_prob         LIKE afi_mae_afiliado.doc_prob,
        ind_infonavit    LIKE afi_mae_afiliado.ind_infonavit,
        cod_error_origen SMALLINT,
        const_curp       SMALLINT
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

    DEFINE reg_afi_mae RECORD LIKE afi_mae_afiliado.*

    DEFINE a_arr ARRAY[10] OF RECORD
           marca CHAR(1),
           conta SMALLINT
           END RECORD

    DEFINE b_arr ARRAY[10] OF RECORD
           marca CHAR(1),
           conta SMALLINT
           END RECORD

    DEFINE
        bnd_mod   ,
        i         ,
        j         ,
        sumatoria ,
        paso      ,
        paso1     ,
        pasa_dat  ,
        tod_id    SMALLINT

    DEFINE
        cont_reg  INTEGER

    DEFINE
        a_marca   CHAR(1),
        b_marca   CHAR(1),
        consec    CHAR(10),
        consecd   CHAR(10)

END GLOBALS

MAIN

    DISPLAY " "
    DISPLAY ".1"

    CALL STARTLOG('AFIB008.log')
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

    INITIALIZE reg_afi_mae.* TO NULL

    LET bnd_proceso = 0

    IF reg_bat.pid THEN
        DISPLAY "INICIANDO PROCESO ..."
        LET bnd_proceso = 1
    END IF

    LET HOYDIA   = TODAY
    LET HOY      = TODAY
    LET HORA     = TIME
    LET num      = 1
    LET g_opcion = '02'
    LET k_aux.tipo_reg         = "02"  
    LET k_aux.con_dservicio    = "          "
    LET k_aux.clave_doperacion = "01" 
    LET k_aux.fech_recep_afor  = "       "
    LET k_aux.folio_solicitud  = "          "

    SELECT *
    INTO   g_paramgrales.*
    FROM   seg_modulo
    WHERE  modulo_cod = 'afi'

    LET a_arr[1].marca = 'A' LET a_arr[2].marca = 'B' LET a_arr[3].marca = 'C'
    LET a_arr[4].marca = 'D' LET a_arr[5].marca = 'E' LET a_arr[6].marca = 'F'
    LET a_arr[7].marca = 'G' LET a_arr[8].marca = 'H' LET a_arr[9].marca = 'I'
    LET a_arr[10].marca = 'J'

    LET a_arr[1].conta  = 512
    LET a_arr[2].conta  = 256
    LET a_arr[3].conta  = 128
    LET a_arr[4].conta  = 64
    LET a_arr[5].conta  = 32
    LET a_arr[6].conta  = 16
    LET a_arr[7].conta  = 8
    LET a_arr[8].conta  = 4
    LET a_arr[9].conta  = 2
    LET a_arr[10].conta = 1

    FOR i = 1 TO 10
        LET b_arr[i].* = a_arr[i].*
    END FOR

END FUNCTION

FUNCTION inicializa()
#iz--------------

    LET i         = 0
    LET j         = 0
    LET paso      = 0
    LET paso1     = 0
    LET tod_id    = 0
    LET bnd_mod   = 0
    LET pasa_dat  = 0
    LET cont_reg  = 0
    LET sumatoria = 0
    LET a_marca   = NULL
    LET b_marca   = NULL

    INITIALIZE w_aux.*       TO NULL
    INITIALIZE w_ant.*       TO NULL
    INITIALIZE reg_afi_mae.* TO NULL

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 4,4 WITH FORM "AFIB0081" ATTRIBUTE(BORDER)
    DISPLAY " AFIB008    GENERACION DE ARCHIVO  PROCESAR (MODIFICADOS)                   " AT 3,1 ATTRIBUTE(REVERSE)

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
        contador  INTEGER,
        ban_rfc   SMALLINT,
        ban_unico SMALLINT,
        vfol_sol  DECIMAL(8,0)

    LET contador  = 0 
    LET ban_rfc   = 0
    LET ban_unico = 0

    LET HAY_REGISTROS = FALSE

    SELECT COUNT(*)
    INTO   HAY_REGISTROS
    FROM   afi_mae_afiliado a, afi_mae_modifica b
--  WHERE  a.status_interno in(120,160)
    WHERE  a.n_seguro       = b.n_seguro
    AND    a.n_folio        = b.n_folio
    AND    a.tipo_solicitud NOT IN (8,12)
    AND    a.tipo_solicitud = b.tipo_solicitud
--  AND    b.cod_operacion  = 0
    AND    b.status_interno = 230

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
        WHERE  lotes_cod   = 2
        AND    lotes_fecha = HOYDIA

        #IF SQLCA.SQLCODE = 0 THEN
        IF STATUS <> NOTFOUND THEN
            UPDATE tab_lote
            SET    lotes_correlativo = lotes_correlativo + 1
            WHERE  lotes_cod         = 2
            AND    lotes_fecha       = HOYDIA
        ELSE
            INSERT INTO tab_lote
            VALUES(HOYDIA     ,
                   2          ,
                   'AFILIADOS',
                   1          ,
                   1)
        END IF

        SELECT @lotes_correlativo
        INTO   h_corr
        FROM   tab_lote
        WHERE  lotes_cod   = 2
        AND    lotes_fecha = HOYDIA

        LET consec = h_corr

        {UPDATE tab_lote
        SET    lotes_correlativo = lotes_correlativo + 1
        WHERE  lotes_cod         = 2
        AND    lotes_fecha       = HOYDIA}


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
        SELECT a.n_seguro,
               b.n_unico,
               b.n_rfc,
               b.paterno,
               b.materno,
               b.nombres,
               b.fena,
               a.cod_promotor,
               b.sexo, 
               b.estadon,
               b.folio_nvo,
               a.frecafor,
               b.nacionalidad,
               b.tip_prob,
               b.fol_prob,
               b.doc_prob,
               a.ind_infonavit,
               b.cod_error_origen,
               b.const_curp,
               a.n_folio
        FROM   afi_mae_afiliado a, afi_mae_modifica b
        --ERE  a.status_interno IN(120,160) # STATUS MODIFICADOS
        WHERE  a.n_seguro       = b.n_seguro
        AND    a.n_folio        = b.n_folio
        AND    a.tipo_solicitud NOT IN (8,12)
        AND    a.tipo_solicitud = b.tipo_solicitud
--      AND    b.cod_operacion  = 0
        AND    b.status_interno = 230

        LET comm = g_paramgrales.ruta_envio CLIPPED,"/E",
                   HOYDIA USING "yyyymmdd" CLIPPED,
                   consec CLIPPED, ".MOD" CLIPPED

        DISPLAY "Archivo: ", comm CLIPPED

        LET G_LISTA = g_paramgrales.ruta_listados CLIPPED,"/",
                      g_usuario CLIPPED, ".MODIF." CLIPPED,
                      HOY USING "dd-mm-yyyy","_", consec CLIPPED,
                      HORA CLIPPED
 
        -- Se inicia listado
        START REPORT listado_2 TO G_LISTA
        START REPORT listado TO comm

        FOREACH curs_1 INTO w_aux.*, vfol_sol

            LET w_ant.*               = w_aux.*
            LET HAY_REGISTROS         = TRUE
            LET k_aux.folio_solicitud = w_aux.n_folio

            IF w_aux.materno IS NULL OR
               w_aux.materno MATCHES ' *' THEN
                LET w_aux.materno = "N/A"
            END IF

            #### Modificacion #### 
            DECLARE c_1 CURSOR FOR
             SELECT *
               FROM afi_mae_afiliado 
              WHERE @n_seguro = w_aux.n_seguro

            FOREACH c_1 INTO reg_afi_mae.* 
            
                --- Elimina espacios
                LET reg_afi_mae.paterno      = reg_afi_mae.paterno CLIPPED
                LET w_aux.paterno            = w_aux.paterno       CLIPPED
                LET w_ant.paterno            = w_ant.paterno       CLIPPED

                LET reg_afi_mae.materno      = reg_afi_mae.materno CLIPPED
                LET w_aux.materno            = w_aux.materno       CLIPPED
                LET w_ant.materno            = w_ant.materno       CLIPPED

                LET reg_afi_mae.nombres      = reg_afi_mae.nombres CLIPPED
                LET w_aux.nombres            = w_aux.nombres       CLIPPED
                LET w_ant.nombres            = w_ant.nombres       CLIPPED

                LET reg_afi_mae.n_rfc        = reg_afi_mae.n_rfc   CLIPPED
                LET w_aux.n_rfc              = w_aux.n_rfc         CLIPPED
                LET w_ant.n_rfc              = w_ant.n_rfc         CLIPPED

                LET reg_afi_mae.fena         = reg_afi_mae.fena    CLIPPED
                LET w_aux.fena               = w_aux.fena          CLIPPED
                LET w_ant.fena               = w_ant.fena          CLIPPED

                LET reg_afi_mae.sexo         = reg_afi_mae.sexo    CLIPPED
                LET w_aux.sexo               = w_aux.sexo          CLIPPED
                LET w_ant.sexo               = w_ant.sexo          CLIPPED

                LET reg_afi_mae.nacionalidad = reg_afi_mae.nacionalidad CLIPPED
                LET w_aux.nacionalidad       = w_aux.nacionalidad       CLIPPED
                LET w_ant.nacionalidad       = w_ant.nacionalidad       CLIPPED

                LET reg_afi_mae.estadon      = reg_afi_mae.estadon      CLIPPED
                LET w_aux.estadon            = w_aux.estadon            CLIPPED
                LET w_ant.estadon            = w_ant.estadon            CLIPPED

                LET reg_afi_mae.tip_prob     = reg_afi_mae.tip_prob     CLIPPED
                LET w_aux.tip_prob           = w_aux.tip_prob           CLIPPED
                LET w_ant.tip_prob           = w_ant.tip_prob           CLIPPED

                LET reg_afi_mae.fol_prob     = reg_afi_mae.fol_prob     CLIPPED
                LET w_aux.fol_prob           = w_aux.fol_prob           CLIPPED
                LET w_ant.fol_prob           = w_ant.fol_prob           CLIPPED

                LET reg_afi_mae.doc_prob     = reg_afi_mae.doc_prob     CLIPPED
                LET w_aux.doc_prob           = w_aux.doc_prob           CLIPPED
                LET w_ant.doc_prob           = w_ant.doc_prob           CLIPPED

                {--- Compara valores entre el maestro y modificaciones
                IF reg_afi_mae.n_rfc MATCHES w_aux.n_rfc OR
                   reg_afi_mae.n_rfc LIKE    w_aux.n_rfc OR
                   reg_afi_mae.n_rfc =       w_aux.n_rfc THEN
                    LET w_aux.n_rfc  = NULL
                ELSE
                    LET ban_rfc = 1
                END IF 

                IF reg_afi_mae.n_unico MATCHES w_aux.n_unico OR
                   reg_afi_mae.n_unico LIKE    w_aux.n_unico OR
                   reg_afi_mae.n_unico =       w_aux.n_unico THEN
                ELSE
                    LET ban_unico = 1
                END IF 

                IF reg_afi_mae.fena MATCHES w_aux.fena OR
                   reg_afi_mae.fena LIKE    w_aux.fena OR
                   reg_afi_mae.fena =       w_aux.fena THEN
                    LET w_aux.fena  = NULL
                ELSE
                   LET bnd_mod = 1
                END IF 

                IF reg_afi_mae.sexo MATCHES w_aux.sexo OR
                   reg_afi_mae.sexo LIKE    w_aux.sexo OR
                   reg_afi_mae.sexo =       w_aux.sexo THEN
                    LET w_aux.sexo  = NULL
                ELSE
                    LET bnd_mod = 1
                END IF

                IF reg_afi_mae.nacionalidad MATCHES w_aux.nacionalidad OR
                   reg_afi_mae.nacionalidad LIKE    w_aux.nacionalidad OR
                   reg_afi_mae.nacionalidad =       w_aux.nacionalidad THEN
                    LET w_aux.nacionalidad  = NULL
                ELSE
                    LET bnd_mod = 1
                END IF

                IF reg_afi_mae.estadon MATCHES w_aux.estadon OR
                   reg_afi_mae.estadon LIKE    w_aux.estadon OR
                   reg_afi_mae.estadon =       w_aux.estadon THEN
                    LET w_aux.estadon  = NULL
                ELSE
                    LET bnd_mod = 1
                END IF


                IF (reg_afi_mae.tip_prob MATCHES w_aux.tip_prob  OR
                    reg_afi_mae.tip_prob LIKE    w_aux.tip_prob  OR
                    reg_afi_mae.tip_prob =       w_aux.tip_prob) THEN

                    IF (reg_afi_mae.fol_prob MATCHES w_aux.fol_prob  OR
                        reg_afi_mae.fol_prob LIKE    w_aux.fol_prob  OR
                        reg_afi_mae.fol_prob =       w_aux.fol_prob) OR
                        reg_afi_mae.fol_prob IS NULL                 AND
                        w_aux.fol_prob       IS NULL                 THEN
              
                        IF (reg_afi_mae.doc_prob MATCHES w_aux.doc_prob  OR
                            reg_afi_mae.doc_prob LIKE    w_aux.doc_prob  OR
                            reg_afi_mae.doc_prob =       w_aux.doc_prob) OR
                            reg_afi_mae.doc_prob IS NULL                 AND
                            w_aux.doc_prob       IS NULL                 THEN
                            LET w_aux.tip_prob   = NULL
                            LET w_aux.fol_prob   = NULL
                            LET w_aux.doc_prob   = NULL
                        ELSE
                            LET w_aux.tip_prob = w_ant.tip_prob
                            LET w_aux.fol_prob = w_ant.fol_prob 
                            LET w_aux.doc_prob = w_ant.doc_prob
                            LET bnd_mod        = 1
                        END IF
                    END IF
                END IF
                
                --- Valida el codigo error de origen
                IF w_aux.cod_error_origen IS NOT NULL AND
                   w_aux.cod_error_origen <> 0        THEN
                    --- Valida que el codigo de error de origen sea igual a 9999
                    IF w_aux.cod_error_origen = 9999 THEN
                        LET w_aux.cod_error_origen = 256
                        LET w_aux.paterno          = w_ant.paterno
                        LET w_aux.materno          = w_ant.materno
                        LET w_aux.nombres          = w_ant.nombres
                        LET w_aux.fol_prob         = w_ant.fol_prob 
                        LET w_aux.doc_prob         = w_ant.doc_prob
                        LET w_aux.sexo             = w_ant.sexo
                        LET w_aux.fena             = w_ant.fena
                        LET w_aux.estadon          = w_ant.estadon
                        LET w_aux.nacionalidad     = w_ant.nacionalidad
                        LET w_aux.tip_prob         = w_ant.tip_prob
                    ELSE
                        LET tod_id   = 1
                    END IF

                    LET pasa_dat = 1
                    LET bnd_mod  = 1
                END IF

                --- Se tiene codigo error origen diferente a 9999
                IF tod_id = 1 THEN
                    FOR i = 1 TO 10
                        IF w_aux.cod_error_origen = a_arr[i].conta OR
                           w_aux.cod_error_origen > a_arr[i].conta THEN
                            LET sumatoria = w_aux.cod_error_origen - 
                                            a_arr[i].conta
                            LET paso = a_arr[i].conta

                            CASE paso
                                WHEN 512 LET pasa_dat = 1
                                WHEN 256 LET pasa_dat = 1
                                WHEN 128 LET pasa_dat = 1
                                         LET w_aux.tip_prob     = w_ant.tip_prob
                                         LET w_aux.fol_prob     = w_ant.fol_prob
                                         LET w_aux.doc_prob     = w_ant.doc_prob
                                WHEN 64  LET pasa_dat = 1
                                         LET w_aux.nacionalidad = 
                                             w_ant.nacionalidad
                                WHEN 32  LET pasa_dat = 1
                                         LET w_aux.estadon      = w_ant.estadon
                                WHEN 16  LET pasa_dat = 1
                                         LET w_aux.fena         = w_ant.fena
                                WHEN 8   LET pasa_dat = 1
                                         LET w_aux.sexo         = w_ant.sexo
                                WHEN 4   LET pasa_dat = 1
                                WHEN 2   LET pasa_dat = 1
                                WHEN 1   LET pasa_dat = 1
                            END CASE

                            IF sumatoria = 0 THEN
                                EXIT FOR
                            ELSE
                                LET w_aux.cod_error_origen = sumatoria
                            END IF
                        END IF
                    END FOR

                    LET w_aux.cod_error_origen = w_ant.cod_error_origen 
                END IF

                IF bnd_mod OR
                   pasa_dat THEN
                    LET w_aux.paterno  = w_ant.paterno
                    LET w_aux.materno  = w_ant.materno
                    LET w_aux.nombres  = w_ant.nombres

                    --- Valida tipo de doc 5 y envio CURP
                    IF w_aux.tip_prob = 5 THEN
                        LET w_aux.n_unico  = w_ant.n_unico
                        LET w_aux.tip_prob = w_ant.tip_prob
                        LET w_aux.fol_prob = w_ant.fol_prob
                        LET w_aux.doc_prob = w_ant.doc_prob
                    ELSE
                        LET w_aux.n_unico = NULL
                    END IF
                END IF

                --- Valida materno en caso de que sea nulo
                IF w_aux.materno IS NULL OR
                   w_aux.materno[1] = " " THEN
                    LET w_aux.materno = "N/A"
                END IF

                --- Si el tipo de doc probatorio es dif a 5 lo envio nulo
                IF w_aux.tip_prob <> 5 THEN
                    LET w_aux.n_unico = NULL
                END IF

                --- Valida tipo de documento probatorio igual a 6
                IF w_aux.tip_prob = 6 THEN
                    LET w_aux.n_unico  = NULL
                    LET w_aux.tip_prob = NULL
                    LET w_aux.fol_prob = NULL
                    LET w_aux.doc_prob = NULL
                END IF

                --- Si se modifica RFC
                IF ban_rfc THEN
                    IF NOT bnd_mod THEN
                        LET w_aux.n_unico  = NULL
                        LET w_aux.tip_prob = NULL
                        LET w_aux.fol_prob = NULL
                        LET w_aux.doc_prob = NULL
                    END IF
                END IF

                --- Cuenta registros e inicializa variables
                LET cont_reg = cont_reg + 1
                LET w_aux.cod_promotor    = NULL
                LET k_aux.fech_recep_afor = NULL
                LET w_aux.frecafor        = NULL
                LET w_aux.ind_infonavit   = NULL

                --- Valida numero de folio de la solicitud
                IF vfol_sol <> w_aux.n_folio THEN
                    LET k_aux.folio_solicitud = NULL
                END IF

                --- Valida codigo error origen igual a 0000
                IF w_aux.cod_error_origen = 0 THEN
                   LET w_aux.cod_error_origen = NULL
                   #LET w_aux.n_unico          = NULL
                END IF

                --- Si se modifica CURP
                IF ban_unico THEN
                   LET w_aux.n_unico  = w_ant.n_unico
                   LET w_aux.tip_prob = w_ant.tip_prob
                   LET w_aux.fol_prob = w_ant.fol_prob
                   LET w_aux.doc_prob = w_ant.doc_prob
                END IF}

                LET w_aux.n_rfc        = NULL
                LET w_aux.sexo         = NULL
                LET w_aux.fena         = NULL
                LET w_aux.estadon      = NULL 
                LET w_aux.nacionalidad = NULL 
                #LET w_aux.tip_prob     = NULL

                OUTPUT TO REPORT listado(k_aux.tipo_reg,
                                         k_aux.con_dservicio,
                                         k_aux.clave_doperacion,
                                         w_aux.n_seguro,
                                         w_aux.n_unico,
                                         w_aux.n_rfc,
                                         w_aux.paterno,
                                         w_aux.materno,
                                         w_aux.nombres,
                                         w_aux.fena,
                                         w_aux.cod_promotor,
                                         k_aux.fech_recep_afor,
                                         k_aux.folio_solicitud,
                                         w_aux.sexo,
                                         w_aux.estadon,
                                         w_aux.frecafor,
                                         w_aux.nacionalidad,
                                         w_aux.tip_prob,
                                         w_aux.fol_prob,
                                         w_aux.doc_prob,
                                         w_aux.ind_infonavit,
                                         w_aux.cod_error_origen,
                                         w_aux.const_curp)

                OUTPUT TO REPORT listado_2(k_aux.tipo_reg,
                                           k_aux.con_dservicio,
                                           k_aux.clave_doperacion,
                                           w_aux.n_seguro,
                                           w_aux.n_unico,
                                           w_aux.n_rfc,
                                           w_aux.paterno,
                                           w_aux.materno,
                                           w_aux.nombres,
                                           w_aux.fena,
                                           w_aux.cod_promotor,
                                           k_aux.fech_recep_afor,
                                           k_aux.folio_solicitud,
                                           w_aux.sexo,
                                           w_aux.estadon,
                                           w_aux.frecafor,
                                           w_aux.nacionalidad,
                                           w_aux.tip_prob,
                                           w_aux.fol_prob,
                                           w_aux.doc_prob,
                                           w_aux.ind_infonavit)

                LET vcont = vcont + 1

                UPDATE afi_mae_afiliado
                SET    status_interno = 130,
                       status_captura = 130,
                       lote           = h_corr,
                       fecha_envio    = HOYDIA
                WHERE  n_seguro       = w_aux.n_seguro
                #AND    status_interno = 120

                UPDATE afi_mae_modifica
                SET    status_interno = 130
                WHERE  n_seguro       = w_aux.n_seguro
                AND    status_interno = 230
                AND    cod_operacion  = 0
                AND    diag_proceso   = '0'

                IF vcont > 9999 THEN
                    EXIT FOREACH
                END IF

                CALL inicializa()

                LET ban_unico = 0

            END FOREACH
            #### Modificacion #### 
        END FOREACH 
        #### Modificacion #### 

        FINISH REPORT listado
        FINISH REPORT listado_2

        CALL limpia_nulos()

        DISPLAY "REGISTROS A SER ENVIADOS ", contador AT 16,20

        LET G_LISTA = "chmod 777 ",g_paramgrales.ruta_listados CLIPPED,"/",
                      g_usuario CLIPPED,".MODIF." 
                      CLIPPED,HOY USING "dd-mm-yyyy" CLIPPED, consec CLIPPED,
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
        n_seguro         LIKE afi_mae_afiliado.n_seguro,
        n_unico          LIKE afi_mae_afiliado.n_unico,
        n_rfc            LIKE afi_mae_afiliado.n_rfc,
        paterno          CHAR(40),
        materno          CHAR(40),
        nombres          CHAR(40),
        fena             DATE,
        cod_promotor     LIKE afi_mae_afiliado.cod_promotor,
        fech_recep_afor  CHAR(08),
        folio_solicitud  CHAR(08),
        sexo             LIKE afi_mae_afiliado.sexo,
        estadon          LIKE afi_mae_afiliado.estadon,
        frecafor         LIKE afi_mae_afiliado.frecafor,
        nacionalidad     LIKE afi_mae_afiliado.nacionalidad,
        tip_prob         LIKE afi_mae_afiliado.tip_prob,
        fol_prob         LIKE afi_mae_afiliado.fol_prob,
        doc_prob         LIKE afi_mae_afiliado.doc_prob,
        ind_infonavit    LIKE afi_mae_afiliado.ind_infonavit,
        cod_error_origen SMALLINT,
        const_curp       SMALLINT
    END RECORD

    DEFINE 
        nombre_comp  ,
        nombre_comp2 CHAR(120)

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
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0
        PAGE LENGTH 60

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
                         "13",
                         "01",
                         x_afore.codigo_afore USING "&&&",
                         "03",
                         "001",
                         hoy_env,
                         "09 ",
                         h_corr USING "&&&",   #CORRELATIVO DE ENVIO
                         g_opcion USING "&&",  #MEDIO POR DONDE SE ENVIA ARCHIVO
                         538 SPACES

        LET verror = 00000512

    ON EVERY ROW
        LET a     = YEAR (w_aux.frecafor)  using "&&&&"
        LET m     = MONTH (w_aux.frecafor) using "&&"
        LET d     = DAY (w_aux.frecafor)   using "&&"
        LET dia   = DAY(w_aux.fena)        USING"&&"
        LET mes   = MONTH(w_aux.fena)      USING"&&"
        LET ano   = YEAR(w_aux.fena)       USING "&&&&"
        LET dia1  = DAY(TODAY)             USING"&&"
        LET mes1  = MONTH(TODAY)           USING"&&"
        LET ano1  = YEAR(TODAY)            USING "&&&&"
        LET num10 = "          "
        LET num10 = num                    USING "&&&&&&&&&&"
        LET verror_or = 0

        LET vfol_prob = w_aux.fol_prob

        PRINT
            COLUMN 1,"03",
                     num10,
                     "03",
                     w_aux.n_seguro,
                     w_aux.n_unico,
                     w_aux.n_rfc,
                     w_aux.paterno,
                     w_aux.materno,
                     w_aux.nombres,
                     ano,mes,dia,
		     28 SPACES,
                     #w_aux.cod_promotor     USING "&&&&&&&&&&" ,
                     #w_aux.frecafor         USING "YYYYMMDD",
                     #k_aux.folio_solicitud  USING "&&&&&&&&&&" ,
                     w_aux.sexo             USING "&",
                     w_aux.estadon          USING "&&",
                     #var                    USING "&",
		     1 SPACES,
                     w_aux.nacionalidad,
                     w_aux.tip_prob,
                     w_aux.fol_prob,
                     w_aux.doc_prob,
                     w_aux.cod_error_origen USING "&&&&",
                     312 SPACES,
                     w_aux.const_curp       USING "&",
                     7 SPACES

        LET num = num + 1

    ON LAST ROW
        LET tot = 0
        LET tot = count(*)
        LET tot_char = tot USING "&&&&&&&&&"

        PRINT COLUMN 001,"09",
                         "01",
                          x_afore.codigo_afore USING "&&&",
                          hoy_env,
                          h_corr USING "&&&",
                          "01",
                          "13",
                          tot_char,
                          539 SPACES

END REPORT

REPORT listado_2(w_aux)
#--------------------

    DEFINE w_aux  RECORD
        tipo_reg         CHAR(02),
        con_dservicio    CHAR(10),
        clave_doperacion CHAR(02),
        n_seguro         LIKE afi_mae_afiliado.n_seguro,
        n_unico          LIKE afi_mae_afiliado.n_unico,
        n_rfc            LIKE afi_mae_afiliado.n_rfc,
        paterno          CHAR(40),
        materno          CHAR(40),
        nombres          CHAR(40),
        fena             DATE,
        cod_promotor     LIKE afi_mae_afiliado.cod_promotor,
        fech_recep_afor  CHAR(08),
        folio_solicitud  CHAR(08),
        sexo             LIKE afi_mae_afiliado.sexo,
        estadon          LIKE afi_mae_afiliado.estadon,
        frecafor         LIKE afi_mae_afiliado.frecafor,
        nacionalidad     LIKE afi_mae_afiliado.nacionalidad,
        tip_prob         LIKE afi_mae_afiliado.tip_prob,
        fol_prob         LIKE afi_mae_afiliado.fol_prob,
        doc_prob         LIKE afi_mae_afiliado.doc_prob,
        ind_infonavit    LIKE afi_mae_afiliado.ind_infonavit
    END RECORD

    DEFINE
        nombre_comp,
        nombre_comp2 CHAR(120)

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
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0
        PAGE LENGTH 66

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
           COLUMN 35,"LISTA DE MODIFICADOS",
           COLUMN 60,"FECHA     :",TODAY     USING "DD-MM-YYYY"
       PRINT
           COLUMN 01,"AFIB008",
           COLUMN 35,"ENVIADOS  A PROCESAR",
           COLUMN 60,"Nro.PAGINA:",PAGENO    USING "##########"
       PRINT
           COLUMN 01,"=================================================",
           COLUMN 50,"=============================="

       PRINT 
           COLUMN 01,"NSS",
           COLUMN 17,"CURP",
           COLUMN 38,"RFC",
           COLUMN 54,"A.PATERNO",
           COLUMN 72,"A.MATERNO"

       PRINT 
           COLUMN 01,"NOMBRES",
           COLUMN 24,"F.NAC",
           COLUMN 37,"CVE.PROM. ",
           COLUMN 50,"F.RECEP",
           COLUMN 63,"F.SOLIC",
           COLUMN 76,"SEXO",
           COLUMN 80,"ENT.NAC"

       PRINT 
           COLUMN 01,"CRED.INFONAVIT",
           COLUMN 17,"NAC.",
           COLUMN 22,"TIP.PROB",
           COLUMN 32,"FOL.PROB",
           COLUMN 42,"DOC.PROB"            

       PRINT
           COLUMN 01,"-------------------------------------------------",
           COLUMN 50,"------------------------------"
        
                 
    ON EVERY ROW
        LET a     = YEAR (w_aux.frecafor)  using "&&&&"
        LET m     = MONTH (w_aux.frecafor) using "&&"
        LET d     = DAY (w_aux.frecafor)   using "&&"
        LET dia   = DAY(w_aux.fena)        USING"&&"
        LET mes   = MONTH(w_aux.fena)      USING"&&"
        LET ano   = YEAR(w_aux.fena)       USING "&&&&"
        LET dia1  = DAY(TODAY)             USING"&&"
        LET mes1  = MONTH(TODAY)           USING"&&"
        LET ano1  = YEAR(TODAY)            USING "&&&&"
        LET num10 = "          "
        LET num10 = num                    USING "&&&&&&&&&&"

        PRINT
            COLUMN 01,w_aux.n_seguro,
            COLUMN 17,w_aux.n_unico,
            COLUMN 38,w_aux.n_rfc,
            COLUMN 54,w_aux.paterno [1,15],
            COLUMN 72,w_aux.materno [1,15]                      
        PRINT
            COLUMN 01,w_aux.nombres [1,20],
            COLUMN 24,ano,mes,dia,
            COLUMN 37,w_aux.cod_promotor           USING "&&&&&&&&&&",
            COLUMN 50,a,m,d                                     ,
            COLUMN 63,k_aux.folio_solicitud  USING "&&&&&&&&&&",
            COLUMN 76,w_aux.sexo             USING "&",
            COLUMN 80,w_aux.estadon          USING "&&"         

        PRINT
            COLUMN 01,w_aux.ind_infonavit,
            COLUMN 17,w_aux.nacionalidad,
            COLUMN 22,w_aux.tip_prob,
            COLUMN 32,w_aux.fol_prob,
            COLUMN 42,w_aux.doc_prob

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

    OPEN WINDOW cent AT 11,25 WITH FORM "AFIB0082" ATTRIBUTE(BORDER)
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
               consec CLIPPED,".MOD > informix" 

    RUN comm

    LET comm = "cd " ,g_paramgrales.ruta_envio CLIPPED ,
               "/ ; mv informix  ","E", HOYDIA USING "yyyymmdd" CLIPPED,
               consec CLIPPED,".MOD " 

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

