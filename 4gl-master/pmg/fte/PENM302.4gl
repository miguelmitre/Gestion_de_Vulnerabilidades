#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETC810  => CARGA DE ARCHIVO URV                                      #
#Fecha creacion    => 15 DE SEPTIEMBRE DE 2022                                  #
#By                =>                                                           #
#Fecha actualiz.   =>                                                           #
#Sistema           => PMG                                                       #
#################################################################################
DATABASE safre_af
GLOBALS

    DEFINE gs_modulo RECORD LIKE seg_modulo.*

    DEFINE gr_dat RECORD
        nom_archivo           CHAR(35)
    END RECORD

    DEFINE
        gc_usuario              CHAR(012),
        gc_ruta_archivo         CHAR(200),
        gc_tipo_operacion       CHAR(002),
        enter                   CHAR(001)

    DEFINE
        gs_flag_err             ,
        gs_flag_proceso         ,
        gs_codigo_afore         ,
        gs_cod_inv              SMALLINT

    DEFINE
        gi_proceso              INTEGER

    DEFINE
        HOY                     DATE

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL f_lib_abre_log("PENM302")
    CALL init()

    OPEN WINDOW main_win AT 4,4 WITH 19 ROWS, 75 COLUMNS ATTRIBUTE(BORDER)
    DISPLAY " PENM302            CARGA DE ARCHIVO DE URV                                    " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    MENU "ARCHIVO URV"
        COMMAND "Carga archivo" "Carga el Archivo de URV"
            CALL f_genera_carga()

        COMMAND "Consulta" "Consulta URV Vigente"
            CALL f_consulta_urv()

        COMMAND "Elimina" "Elimina URV Vigente"
            CALL f_elimina_urv()

        COMMAND "Salir" "Salir del Programa "
            EXIT MENU
    END MENU

    CLOSE WINDOW main_win

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    LET HOY  = TODAY

    SELECT *
    INTO   gs_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "pmg"

    ----- CODIGOS AFORES -----
    SELECT codigo_afore    ,
           USER
    INTO   gs_codigo_afore ,
           gc_usuario
    FROM   tab_afore_local

    SELECT afore_cod
    INTO   gs_cod_inv
    FROM   tab_afore
    WHERE  afore_desc MATCHES '*INVERCAP*'

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_carga : Ejecuta los pasos para cargar el archivo de URV          #
#                  que el usuario seleccione                                #
#---------------------------------------------------------------------------#
FUNCTION f_genera_carga()

    DEFINE
        lc_tramite          CHAR(02)

    -- -----------------------------------------------------------------------------

    CALL f_carga_archivo() RETURNING gs_flag_proceso

    IF gs_flag_proceso = 0 THEN #-- La carga se realizo sin errores

        CALL primer_paso()        #-- Vacia la informacion del archivo a las tablas de validacion

        CALL segundo_paso()       #-- Realiza las validaciones de la informacion
            RETURNING gs_flag_err

        IF gs_flag_err = 0 THEN
            CALL tercer_paso()    #-- Vacia la informacion hacia las tablas fisicas
        ELSE
            PROMPT " SE ENCONTRARON INCONSISTENCIAS EN EL PROCESO ... <ENTER> " FOR CHAR enter
        END IF


    ELSE
        IF gi_proceso <> 0 THEN
            DISPLAY " ARCHIVO CON INCONSISTENCIAS DE ESTRUCTURA ..." AT 19,1
        END IF
    END IF

    CLEAR SCREEN
    CLOSE WINDOW penm3021

END FUNCTION

#---------------------------------------------------------------------------#
# f_carga_archivo : Captura el nombre del archivo y busca en la ruta de     #
#                   rescate si existe. En caso de existir, lo carga sin     #
#                   formato en la tabla temporal                            #
#---------------------------------------------------------------------------#
FUNCTION f_carga_archivo()

    DEFINE ls_flag                 SMALLINT
    DEFINE ls_procesa              SMALLINT

    DEFINE li_num_reg              INTEGER

    DEFINE lc_tipo_tramite         CHAR(02)
    DEFINE mc_msg                  CHAR(50)

    -- -----------------------------------------------------------------------------

    OPEN WINDOW penm3021 AT 4,4 WITH FORM "PENM3021" ATTRIBUTE(BORDER)
    DISPLAY " < Ctrl-C > Salir                                      < ENTER > Continuar     " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " PENM302            CARGA DE ARCHIVO DE URV                                    " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    CALL f_tablas_tmp()

    LET li_num_reg  = 0
    LET ls_procesa  = 0

    INPUT BY NAME gr_dat.nom_archivo WITHOUT DEFAULTS
        BEFORE FIELD nom_archivo
            LET gr_dat.nom_archivo = NULL
            CLEAR FORM

        AFTER FIELD nom_archivo
            IF gr_dat.nom_archivo IS NULL THEN
                ERROR "   CAMPO NO PUEDE SER NULO  "
                NEXT FIELD nom_archivo
            END IF

            LET gc_ruta_archivo = gs_modulo.ruta_rescate CLIPPED,"/",
                                  gr_dat.nom_archivo CLIPPED

            WHENEVER ERROR CONTINUE

            LOAD FROM gc_ruta_archivo DELIMITER ","
            INSERT INTO tmp_arch_carga

            WHENEVER ERROR STOP

            SELECT COUNT(*)
            INTO   li_num_reg
            FROM   tmp_arch_carga

            IF li_num_reg = 0 THEN
                ERROR "  NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO  "
                NEXT FIELD nom_archivo
            ELSE
                LET mc_msg = "¿ESTA SEGURO S/N ?  "
                LET mc_msg = mc_msg CLIPPED

                IF (f_lib_pregunta(mc_msg) = TRUE) THEN
                    LET ls_procesa  = 0
                    EXIT INPUT
                ELSE
                    PROMPT " CARGA CANCELADA...<ENTER> PARA SALIR " FOR CHAR enter
                    LET ls_procesa  = 1
                    EXIT INPUT
                END IF
            END IF

        ON KEY (INTERRUPT)
            ERROR " PROCESO CANCELADO  "
            LET ls_procesa  = 1
            SLEEP 1
            ERROR ""
            EXIT INPUT
    END INPUT

    RETURN ls_procesa

END FUNCTION

#---------------------------------------------------------------------------#
# primer_paso : Carga y vacia la informacion del archivo de URV             #
#               a las tablas temporales donde se realizara su validacion    #
#---------------------------------------------------------------------------#
FUNCTION primer_paso()

    DEFINE lr_paso RECORD
        edad           CHAR(03),
        ini_vigencia   CHAR(10),
        fin_vigencia   CHAR(10),
        tasa           CHAR(10),
        urv_hombre     CHAR(25),
        urv_mujer      CHAR(25),
        filler         CHAR(3)
    END RECORD

    DEFINE lr_tab_urv_rp    RECORD LIKE tab_urv_rp.*
    DEFINE lc_fechas        CHAR(10)

    -- -----------------------------------------------------------------------------

    DISPLAY " CARGANDO ARCHIVO ... " AT 10,1 ATTRIBUTE(REVERSE)

    DECLARE cur_ar CURSOR FOR
    SELECT  *
    FROM    tmp_arch_carga

    FOREACH cur_ar INTO lr_paso.*

        -- -----------------------------------------------------------------------------
        
        LET lr_tab_urv_rp.edad        = lr_paso.edad
        
        IF (lr_paso.ini_vigencia IS NOT NULL) OR (LENGTH(lr_paso.ini_vigencia CLIPPED) > 0) THEN
            LET lc_fechas                   = mdy(lr_paso.ini_vigencia[04,05],
                                              lr_paso.ini_vigencia[01,02],
                                              lr_paso.ini_vigencia[07,10])
            LET lr_tab_urv_rp.ini_vigencia  = lc_fechas
        END IF
        
        IF (lr_paso.fin_vigencia IS NOT NULL) OR (LENGTH(lr_paso.fin_vigencia CLIPPED) > 0) THEN
            LET lc_fechas                   = mdy(lr_paso.fin_vigencia[04,05],
                                              lr_paso.fin_vigencia[01,02],
                                              lr_paso.fin_vigencia[07,10])
            LET lr_tab_urv_rp.fin_vigencia  = lc_fechas
        ELSE
            LET lr_tab_urv_rp.fin_vigencia  = NULL
        END IF
        LET lr_tab_urv_rp.tasa        = lr_paso.tasa
        LET lr_tab_urv_rp.urv_hombre  = lr_paso.urv_hombre
        LET lr_tab_urv_rp.urv_mujer   = lr_paso.urv_mujer
        
        
        -- -----------------------------------------------------------------------------
        
        INSERT INTO tmp_tab_urv_rp VALUES(lr_tab_urv_rp.*)
          
    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# segundo_paso : Valida la informacion del archivo de URV                   #
#---------------------------------------------------------------------------#
FUNCTION segundo_paso()

    DEFINE ls_flag         SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_flag       = 0

    -- Se valida que las referencias no vengan duplicadas
    DISPLAY "Validando duplicidad..." AT 11,1 ATTRIBUTE(REVERSE)
    CALL fn_valida_duplicidad() RETURNING ls_flag

    IF (ls_flag == 0) THEN

        -- Se valida los montos recibidos
        DISPLAY "Validando cifras..." AT 12,1 ATTRIBUTE(REVERSE)
        CALL fn_valida_cifras() RETURNING ls_flag

        IF (ls_flag == 0) THEN

            -- Se valida que la información por cargar no haya sido cargada antes
            DISPLAY "Validando URV historico..." AT 13,1 ATTRIBUTE(REVERSE)
            CALL fn_valida_urv_historica() RETURNING ls_flag

        END IF -- fn_valida_cifras

    END IF -- fn_valida_duplicidad

    RETURN ls_flag

END FUNCTION


{===============================================================================
Funcion:         fn_valida_duplicidad
Descripcion:     Funcion que valida si hay datos duplicados
Parametros:      Ninguno.
Valores Retorno: r_resultado - Record con el resultado de la ejecucion.
===============================================================================}
FUNCTION fn_valida_duplicidad()

    DEFINE v_total_repetidos    SMALLINT
    DEFINE v_edad               CHAR(3)
    DEFINE v_ini_vigencia       CHAR(10)
    DEFINE v_total              SMALLINT
    DEFINE v_query              CHAR(1000)
    DEFINE lc_mensaje           CHAR(1000)

    DEFINE ls_flag              SMALLINT

    LET ls_flag  = 0
    LET lc_mensaje = ""

    -- Se valida que no haya la combinación edad
    SELECT COUNT(*)
    INTO   v_total_repetidos
    FROM   tmp_tab_urv_rp
    GROUP  BY edad
    HAVING COUNT(edad) > 1

    IF (v_total_repetidos > 0) THEN

        LET ls_flag  = 1
        LET lc_mensaje = "Hay edades repetidas en el archivo."
        CALL ERRORLOG(lc_mensaje CLIPPED)
        DISPLAY lc_mensaje CLIPPED AT 14,1 ATTRIBUTE(REVERSE)

    END IF -- (v_total_duplicados > 0)

    RETURN ls_flag

END FUNCTION -- fn_valida_duplicidad


{===============================================================================
Funcion:         fn_valida_cifras
Descripcion:     Funcion que valida que las cifras sean validas.
Parametros:      Ninguno.
Valores Retorno: r_resultado - Record con el resultado de la ejecucion.
===============================================================================}
FUNCTION fn_valida_cifras()

    DEFINE v_total          SMALLINT
    DEFINE v_edad           LIKE tab_urv_rp.edad
    DEFINE v_tasa           LIKE tab_urv_rp.tasa
    DEFINE v_urv_hombre     LIKE tab_urv_rp.urv_hombre
    DEFINE v_urv_mujer      LIKE tab_urv_rp.urv_mujer

    
    DEFINE v_edad_char          CHAR(3)
    DEFINE v_tasa_char          CHAR(10)
    DEFINE v_urv_hombre_char    CHAR(25)
    DEFINE v_urv_mujer_char     CHAR(25)

    DEFINE v_ini_vigencia   CHAR(10)

    DEFINE ls_flag          SMALLINT
    DEFINE lc_mensaje       CHAR(1000)

    LET ls_flag  = 0
    LET lc_mensaje = ""

    DECLARE cur_valida_numeros CURSOR FOR
    SELECT edad,
           tasa,
           urv_hombre,
           urv_mujer,
           ini_vigencia
    FROM   tmp_tab_urv_rp

    FOREACH cur_valida_numeros INTO v_edad_char,
                                    v_tasa_char,
                                    v_urv_hombre_char,
                                    v_urv_mujer_char,
                                    v_ini_vigencia

        WHENEVER ERROR CONTINUE
            -- Edad ----------------------------------------------------------------
            LET v_edad = v_edad_char
            IF v_edad IS NULL THEN
                LET ls_flag  = 1
            END IF

            -- Tasa ----------------------------------------------------------------
            LET v_tasa = v_tasa_char
            IF v_tasa IS NULL THEN
                LET ls_flag  = 1
            END IF

            -- URV Hombre ----------------------------------------------------------
            LET v_urv_hombre = v_urv_hombre_char
            IF v_urv_hombre IS NULL THEN
                LET ls_flag  = 1
            END IF

            -- URV Mujer -----------------------------------------------------------
            LET v_urv_mujer = v_urv_mujer_char
            IF v_urv_mujer IS NULL THEN
                LET ls_flag  = 1
            END IF
        WHENEVER ERROR STOP

    END FOREACH
    CLOSE cur_valida_numeros
    FREE cur_valida_numeros

    IF (ls_flag == 1) THEN
        LET lc_mensaje = "Se encontro un dato no númerico."
        CALL ERRORLOG(lc_mensaje CLIPPED)
        DISPLAY lc_mensaje CLIPPED AT 15,1 ATTRIBUTE(REVERSE)
    END IF -- (r_resultado.estado == 1)


    IF (ls_flag == 0) THEN

        -- Se valida no haya edades invalidas
        SELECT COUNT(*)
        INTO   v_total
        FROM   tmp_tab_urv_rp
        WHERE  edad < 60
           OR  edad > 110

        IF (v_total > 0) THEN

            LET ls_flag  = 1
            LET lc_mensaje = "Hay edades inválidas."
            CALL ERRORLOG(lc_mensaje CLIPPED)
            DISPLAY lc_mensaje CLIPPED AT 16,1 ATTRIBUTE(REVERSE)

        END IF -- (v_total > 0)


        -- Se valida que no haya tasas invalidas
        SELECT COUNT(*)
        INTO   v_total
        FROM   tmp_tab_urv_rp
        WHERE  tasa <= 0

        IF (v_total > 0) THEN

            LET ls_flag  = 1
            LET lc_mensaje = "Hay tasas inválidas."
            CALL ERRORLOG(lc_mensaje CLIPPED)
            DISPLAY lc_mensaje CLIPPED AT 16,1 ATTRIBUTE(REVERSE)

        END IF -- (v_total > 0)


        -- Se valida que no haya urv de hombre invalidas
        SELECT COUNT(*)
        INTO   v_total
        FROM   tmp_tab_urv_rp
        WHERE  urv_hombre <= 0

        IF (v_total > 0) THEN

            LET ls_flag  = 1
            LET lc_mensaje = "Hay URV Hombre inválidas."
            CALL ERRORLOG(lc_mensaje CLIPPED)
            DISPLAY lc_mensaje CLIPPED AT 16,1 ATTRIBUTE(REVERSE)

        END IF -- (v_total > 0)


        -- Se valida que no haya urv de mujer invalidas
        SELECT COUNT(*)
        INTO   v_total
        FROM   tmp_tab_urv_rp
        WHERE  urv_mujer <= 0

        IF (v_total > 0) THEN

            LET ls_flag  = 1
            LET lc_mensaje = "Hay URV Mujer inválidas."
            CALL ERRORLOG(lc_mensaje CLIPPED)
            DISPLAY lc_mensaje CLIPPED AT 16,1 ATTRIBUTE(REVERSE)

        END IF -- (v_total > 0)

    END IF -- (r_resultado.estado == 0)

    RETURN ls_flag

END FUNCTION -- fn_valida_cifras


{===============================================================================
Funcion:         fn_valida_fechas
Descripcion:     Funcion que valida que las fechas que estan en el archivo sean
                 validas.
Parametros:      Ninguno.
Valores Retorno: r_resultado - Record con el resultado de la ejecucion.
===============================================================================}
FUNCTION fn_valida_fechas()

    DEFINE v_edad           LIKE tab_urv_rp.edad
    DEFINE v_fecha_char     CHAR(10)
    DEFINE v_ini_vigencia   LIKE tab_urv_rp.ini_vigencia
    DEFINE v_query          CHAR(1000)
    DEFINE lc_mensaje       CHAR(1000)

    DEFINE ls_flag      SMALLINT

    LET ls_flag  = 0
    LET lc_mensaje = ""

    DECLARE cur_val_fecha CURSOR FOR
    SELECT edad,
           ini_vigencia
    FROM   tmp_tab_urv_rp

    FOREACH cur_val_fecha INTO v_edad,
                               v_fecha_char

        -- Se valida la conversion de la fecha
        WHENEVER ERROR CONTINUE

            LET v_query = "\n SELECT TO_DATE('",v_fecha_char,"','%m/%d/%Y') AS f_movimiento ",
                          "\n FROM   systables                                              ",
                          "\n WHERE  tabid = 1                                              "
            PREPARE prp_valida_fecha FROM v_query
            EXECUTE prp_valida_fecha INTO v_ini_vigencia

            IF (SQLCA.SQLCODE != 0) THEN
                LET ls_flag  = 1
            END IF

        WHENEVER ERROR STOP

    END FOREACH
    CLOSE cur_val_fecha
    FREE cur_val_fecha

    IF (ls_flag == 1) THEN
        LET lc_mensaje = "Hay errores en el formato de las Fechas Inicio Vigencia."
        CALL ERRORLOG(lc_mensaje CLIPPED)
        DISPLAY lc_mensaje CLIPPED AT 17,1 ATTRIBUTE(REVERSE)
    END IF -- (r_resultado.estado == 1)

    RETURN ls_flag

END FUNCTION -- fn_valida_fechas


{===============================================================================
Funcion:         fn_valida_urv_historica
Descripcion:     Funcion que valida que los datos cargados en el archivo no contenga
                 fechas menores o iguales a las que contiene la tabla historica.
Parametros:      Ninguno.
Valores Retorno: r_resultado - Record con el resultado de la ejecucion.
===============================================================================}
FUNCTION fn_valida_urv_historica()

    DEFINE v_total              SMALLINT
    DEFINE v_tmp_edad           LIKE tab_urv_rp.edad
    DEFINE v_tmp_ini_vigencia   LIKE tab_urv_rp.ini_vigencia
    DEFINE v_his_edad           LIKE tab_urv_rp.edad
    DEFINE v_his_ini_vigencia   LIKE tab_urv_rp.ini_vigencia
    DEFINE v_query              CHAR(1000)
    DEFINE lc_mensaje           CHAR(1000)

    DEFINE ls_flag      SMALLINT

    LET ls_flag  = 0
    LET lc_mensaje = ""

    -- Se valida que no se intente cargar fechas anteriores a las vigentes
    SELECT COUNT(tmp.edad)
    INTO   v_total
    FROM   tmp_tab_urv_rp tmp,
           safre_af:tab_urv_rp         his
    WHERE  his.edad          = tmp.edad
      AND  his.fin_vigencia IS NULL
      AND  his.ini_vigencia >= tmp.ini_vigencia

    IF (v_total > 0) THEN

        LET ls_flag  = 1
        LET lc_mensaje = "Se esta intentando cargar URV con fecha anterior a las vigentes."
        CALL ERRORLOG(lc_mensaje CLIPPED)
        DISPLAY lc_mensaje CLIPPED AT 18,1 ATTRIBUTE(REVERSE)

    END IF -- (v_total > 0)

    RETURN ls_flag

END FUNCTION -- fn_valida_urv_historica

#---------------------------------------------------------------------------#
# tercer_paso : Guarda la informacion en las tablas finales una vez que ya  #
#               fue validada                                                #
#---------------------------------------------------------------------------#
FUNCTION tercer_paso()

    DEFINE pc_tramite          CHAR(02)
    DEFINE li_folio            INTEGER
    DEFINE lr_tab_urv_rp       RECORD LIKE tab_urv_rp.*

    -- -----------------------------------------------------------------------------

    DISPLAY "                                             " AT 18,1
    DISPLAY " ACTUALIZANDO TABLAS ... " AT 14,1 ATTRIBUTE(REVERSE)
    SLEEP 1

    DECLARE cur_ar_2 CURSOR FOR
    SELECT  *
    FROM    tmp_tab_urv_rp

    FOREACH cur_ar_2 INTO lr_tab_urv_rp.*

        -- Se cierra la vigencia para esta edad
        UPDATE tab_urv_rp
        SET    fin_vigencia = (lr_tab_urv_rp.ini_vigencia - 1)
        WHERE  edad         = lr_tab_urv_rp.edad
          AND  fin_vigencia IS NULL
          
    END FOREACH

    INSERT INTO tab_urv_rp
    SELECT *
    FROM   tmp_tab_urv_rp

    DISPLAY "                                             " AT 18,1
    PROMPT " PROCESO FINALIZADO. PRESIONE <ENTER> PARA REGRESAR AL MENU "
    FOR CHAR enter

END FUNCTION

#---------------------------------------------------------------------------#
# f_tablas_tmp : Crea las tablas temporales que se usaran en el proceso     #
#---------------------------------------------------------------------------#
FUNCTION f_tablas_tmp()

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_arch_carga
        DROP TABLE tmp_tab_urv_rp
    WHENEVER ERROR STOP

    #---
    CREATE TEMP TABLE tmp_arch_carga
    (
    edad           CHAR(03),
    ini_vigencia   CHAR(10),
    fin_vigencia   CHAR(10),
    tasa           CHAR(10),
    urv_hombre     CHAR(25),
    urv_mujer      CHAR(25),
    filler         CHAR(3)
    )

    #---
    SELECT *
    FROM   tab_urv_rp
    WHERE  1 = 0
    INTO TEMP tmp_tab_urv_rp

END FUNCTION

FUNCTION f_consulta_urv()
#dts----------------------------
    DEFINE l_reg ARRAY[61] OF RECORD
        edad           SMALLINT,
        ini_vigencia   DATE,
        tasa           DECIMAL(5,4),
        urv_hombre     DECIMAL(24,18),
        urv_mujer      DECIMAL(24,18)
    END RECORD

    DEFINE x_x            CHAR(200)
    DEFINE x_buscar       CHAR(030)
    DEFINE pos		        SMALLINT

    OPEN WINDOW penm3022 AT 4,4 WITH FORM "PENM3022" ATTRIBUTE(BORDER)
    DISPLAY " < Ctrl-C > Salir                                                              " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " PENM302                     CONSULTA DE URV                                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

        WHILE TRUE
            LET x_x = " SELECT edad, ini_vigencia, tasa, urv_hombre , urv_mujer FROM tab_urv_rp ",
                      " WHERE fin_vigencia IS NULL ",
                      " ORDER BY 1 " CLIPPED

            PREPARE pre_5 FROM x_x
            DECLARE cur_5 CURSOR FOR pre_5
            LET pos = 1
            FOREACH cur_5 INTO l_reg[pos].*
                LET pos = pos + 1
            END FOREACH

            IF (pos-1) < 1 THEN
                ERROR "NO SE ENCONTRO URV VIGENTE "
            END IF

            CALL SET_COUNT(pos-1)
            DISPLAY ARRAY l_reg TO scr_1.*
                ON KEY ( CONTROL-C )
                    LET pos = 0
                    EXIT DISPLAY

                ON KEY ( INTERRUPT )
                    LET pos = 0
                    EXIT DISPLAY

                ON KEY ( CONTROL-M )
                    LET pos = ARR_CURR()
                    EXIT DISPLAY
            END DISPLAY

            IF pos = 0 THEN
                EXIT WHILE
            END IF
        END WHILE
    CLOSE WINDOW penm3022
END FUNCTION

FUNCTION f_elimina_urv()
#dts----------------------------
    DEFINE l_reg ARRAY[61] OF RECORD
        edad           SMALLINT,
        ini_vigencia   DATE,
        tasa           DECIMAL(5,4),
        urv_hombre     DECIMAL(24,18),
        urv_mujer      DECIMAL(24,18)
    END RECORD

    DEFINE x_x            CHAR(200)
    DEFINE x_buscar       CHAR(030)
    DEFINE pos		        SMALLINT

    OPEN WINDOW penme3022 AT 4,4 WITH FORM "PENM3022" ATTRIBUTE(BORDER)
    DISPLAY " < Ctrl-C > Salir                                        < ESC > Eliminar      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " PENM302                       ELIMINA URV                                     " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

        WHILE TRUE
            LET x_x = " SELECT edad, ini_vigencia, tasa, urv_hombre , urv_mujer FROM tab_urv_rp ",
                      " WHERE fin_vigencia IS NULL ",
                      " ORDER BY 1 " CLIPPED

            PREPARE pre_6 FROM x_x
            DECLARE cur_6 CURSOR FOR pre_6
            LET pos = 1
            FOREACH cur_6 INTO l_reg[pos].*
                LET pos = pos + 1
            END FOREACH

            IF (pos-1) < 1 THEN
                ERROR "NO SE ENCONTRO URV VIGENTE "
            END IF

            CALL SET_COUNT(pos-1)
            DISPLAY ARRAY l_reg TO scr_1.*
                ON KEY ( CONTROL-C )
                    LET pos = 0
                    EXIT DISPLAY

                ON KEY ( INTERRUPT )
                    LET pos = 0
                    EXIT DISPLAY

                ON KEY ( ESC )
                    PROMPT " ESTA SEGURO S/N ? " FOR CHAR enter
                    IF enter MATCHES "[sSnN]" THEN
                        IF enter MATCHES "[sS]" THEN
                            LET pos = ARR_CURR()
                            CALL fn_elimina_registros(l_reg[pos].ini_vigencia)
                            LET pos = 0
                            PROMPT " PROCESO FINALIZADO. PRESIONE <ENTER> PARA REGRESAR AL MENU "
                            FOR CHAR enter
                            EXIT DISPLAY
                        ELSE
                            PROMPT " ELIMINACIÓN CANCELADA...<ENTER> PARA SALIR " FOR CHAR enter
                            LET pos = 0
                            EXIT DISPLAY
                        END IF
                    END IF
                    EXIT DISPLAY
            END DISPLAY

            IF pos = 0 THEN
                EXIT WHILE
            END IF
        END WHILE
    CLOSE WINDOW penme3022
END FUNCTION

FUNCTION fn_elimina_registros(p_ini_vigencia)
    DEFINE p_ini_vigencia     DATE
    DEFINE ls_existe          SMALLINT
    
    LET ls_existe = 0
    
    DELETE FROM tab_urv_rp
    WHERE ini_vigencia = p_ini_vigencia
    
    SELECT NVL(COUNT(*),0)
    INTO ls_existe
    FROM tab_urv_rp
    WHERE fin_vigencia = p_ini_vigencia - 1
    
    IF ls_existe > 0 THEN
        UPDATE tab_urv_rp
        SET fin_vigencia = NULL
        WHERE fin_vigencia = p_ini_vigencia - 1
    END IF
    
END FUNCTION
