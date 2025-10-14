###############################################################################
#Proyecto          => AFORE ( MEXICO )                                        #
#Propietario       => E.F.P.                                                  #
#Programa TAAM003  => CONTROL DEL ARCHIVO DE SALDOS DE TAA RECEPTORA          #
#                                                                             #
#Autor             => JOSUE LISANDRO HUERTA SIERRA                            #
#Fecha             => 03 DE MARZO DE 2008                                     #
#Sistema           => TAA                                                     #
###############################################################################
DATABASE safre_af

GLOBALS

    DEFINE arr_ctr_taa ARRAY[1000] OF RECORD
         id_operacion           CHAR(02),
         nombre_archivo         CHAR(20),
         folio                  INTEGER,
         fecha_presentacion     DATE,
         fecha_liquidacion      DATE
    END RECORD
    DEFINE cla_where CHAR(400)
    DEFINE HOY       DATE

END GLOBALS


MAIN

    CALL STARTLOG("TAAC015.log")
    OPTIONS INPUT WRAP,
            ACCEPT KEY CONTROL-V,
            PROMPT LINE LAST
    DEFER INTERRUPT 

    CALL menu_principal()

END MAIN

FUNCTION menu_principal()

    LET HOY = today

    OPEN WINDOW ventana_3 AT 2,2 WITH 19 ROWS, 74 COLUMNS ATTRIBUTE(BORDER, PROMPT LINE LAST-1)
    DISPLAY " TAAC0015           CONTROL ARCHIVO SALDOS TAA RECEPTORA                      " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "------------------------------------------------------------------------------" AT 4,1
    DISPLAY HOY USING " DD-MM-YYYY " AT 3,61 ATTRIBUTE(REVERSE)

    MENU "TAA RECEPTORA"
        COMMAND KEY(B) "Consulta " "Despliega informacion de control de archivo"
            CALL busca_arh()
         COMMAND "Salir" "Salir del Programa"
            EXIT MENU
    END MENU

END FUNCTION

FUNCTION busca_arh()

    DEFINE qry_busqueda CHAR(800)
    DEFINE i            INTEGER
    DEFINE pos          INTEGER
    DEFINE bnd_liq      SMALLINT

    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "TAAC0151"ATTRIBUTE (BORDER)
    DISPLAY " TAAC0015           CONTROL ARCHIVO SALDOS TAA RECEPTORA                      " AT 2,1 ATTRIBUTE(REVERSE)
    DISPLAY "---------------- CTRL-C-   Salir ---------- CTRL-V   Ver Detalle -------------" AT 4,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING " DD-MM-YYYY " AT 2,62 ATTRIBUTE(REVERSE)

    LET int_flag = FALSE

    CONSTRUCT  cla_where ON taa_ctr_traspaso.id_operacion      ,
                            taa_ctr_traspaso.nombre_archivo    ,
                            taa_ctr_traspaso.folio             ,
                            taa_ctr_traspaso.fecha_presentacion,
                            taa_recepcion_af.fecha_liquidacion
                        FROM
                            id_operacion      ,
                            nombre_archivo    ,
                            folio             ,
                            fecha_presentacion,
                            fecha_liquidacion

       AFTER CONSTRUCT
           IF FIELD_TOUCHED(fecha_liquidacion) THEN
               LET bnd_liq = TRUE
               LET int_flag = FALSE
               EXIT CONSTRUCT
           ELSE
               LET bnd_liq = FALSE
               LET int_flag = FALSE
               EXIT CONSTRUCT
           END IF

        ON KEY(INTERRUPT)
            EXIT CONSTRUCT

    END CONSTRUCT

    IF int_flag THEN
        CLOSE WINDOW ventana_1
        LET int_flag = FALSE
        RETURN
    ELSE
        LET int_flag = FALSE
    END IF

    IF NOT bnd_liq THEN
        LET qry_busqueda = " SELECT taa_ctr_traspaso.id_operacion, ",
                           " taa_ctr_traspaso.nombre_archivo, ",
                           " taa_ctr_traspaso.folio, ",
                           " taa_ctr_traspaso.fecha_presentacion " ,
                           " FROM taa_ctr_traspaso WHERE ",
                             cla_where CLIPPED,
                           " ORDER BY fecha_presentacion DESC " 
    ELSE
        LET qry_busqueda = " SELECT taa_ctr_traspaso.id_operacion, ",
                           " taa_ctr_traspaso.nombre_archivo, ",
                           " taa_ctr_traspaso.folio, ",
                           " taa_ctr_traspaso.fecha_presentacion, " ,
                           " taa_recepcion_af.fecha_liquidacion ",
                           " FROM taa_ctr_traspaso, taa_recepcion_af WHERE ",
                             cla_where CLIPPED,
                           " AND taa_recepcion_af.folio = taa_ctr_traspaso.folio ",
                           " ORDER BY fecha_presentacion DESC " 
    END IF

    PREPARE pre_busqueda FROM qry_busqueda
    DECLARE cur_busqueda CURSOR FOR pre_busqueda

    LET i = 1 
    FOREACH cur_busqueda INTO arr_ctr_taa[i].*
        IF arr_ctr_taa[i].fecha_liquidacion IS NULL OR
           arr_ctr_taa[i].fecha_liquidacion = "12/31/1899" THEN
            SELECT a.fecha_liquidacion
              INTO arr_ctr_taa[i].fecha_liquidacion
              FROM taa_recepcion_af a
             WHERE a.folio = arr_ctr_taa[i].folio

             IF arr_ctr_taa[i].fecha_liquidacion IS NULL OR
                arr_ctr_taa[i].fecha_liquidacion = "12/31/1899" THEN
                LET arr_ctr_taa[i].fecha_liquidacion = NULL
             END IF

        END IF
        LET i = i + 1
    END FOREACH

    INITIALIZE arr_ctr_taa[i].* TO NULL

    IF (i-1) >= 1 THEN
        CALL SET_COUNT(i-1)
        DISPLAY ARRAY arr_ctr_taa TO arr_taa.*
            ON KEY (CONTROL-V)
               LET pos = ARR_CURR()
               CALL despliega_detalle(pos)

            ON KEY (INTERRUPT)
               EXIT DISPLAY
         END DISPLAY
    ELSE
        ERROR "NO EXISTEN REGISTROS CON EL RANGO DE BUSQUEDA "
        SLEEP 2
        ERROR ""
    END IF

    CLOSE WINDOW ventana_1
    INITIALIZE arr_ctr_taa TO NULL
END FUNCTION

FUNCTION despliega_detalle (j)

    DEFINE reg_ctr_taa            RECORD LIKE taa_ctr_traspaso.*
    DEFINE desc_recepcion         CHAR(10)
    DEFINE desc_incorpora         CHAR(10)
    DEFINE desc_provision         CHAR(10)
    DEFINE desc_liquida           CHAR(10)
    DEFINE nombre_archivo         CHAR(20)
    DEFINE id_operacion           CHAR(02)
    DEFINE enter                  CHAR(1)
    DEFINE fecha_paso             CHAR(20)
    DEFINE j                      INTEGER
    DEFINE folio                  INTEGER
    DEFINE reg_incorpora          INTEGER
    DEFINE fecha_presentacion     DATE
    DEFINE fecha_liquidacion      DATE

    SELECT tt.*
      INTO reg_ctr_taa.*
      FROM taa_ctr_traspaso tt
     WHERE tt.fecha_presentacion = arr_ctr_taa[j].fecha_presentacion
       AND tt.id_operacion       = arr_ctr_taa[j].id_operacion
       AND tt.nombre_archivo     = arr_ctr_taa[j].nombre_archivo
       AND tt.folio              = arr_ctr_taa[j].folio

    IF reg_ctr_taa.fecha_recepcion IS NOT NULL OR
       reg_ctr_taa.fecha_recepcion <> '31/12/1899' THEN
        LET fecha_paso = reg_ctr_taa.fecha_recepcion
        LET desc_recepcion = fecha_paso[9,10],"/",
                             fecha_paso[6,7],"/",
                             fecha_paso[1,4]
    ELSE
        LET desc_recepcion = '    NO    '
    END IF

    IF reg_ctr_taa.ini_incorpora IS NOT NULL OR
       reg_ctr_taa.ini_incorpora <> '31/12/1899' THEN
        IF reg_ctr_taa.fin_incorpora IS NOT NULL OR
           reg_ctr_taa.fin_incorpora <> '31/12/1899' THEN
            LET fecha_paso = reg_ctr_taa.fin_incorpora
            LET desc_incorpora = fecha_paso[9,10],"/",
                                 fecha_paso[6,7],"/",
                                 fecha_paso[1,4]
        ELSE
            LET desc_incorpora = 'EN PROCESO'
        END IF
    ELSE
        LET desc_incorpora = '    NO    '
    END IF

    IF reg_ctr_taa.ini_provision IS NOT NULL OR
       reg_ctr_taa.ini_provision <> '31/12/1899' THEN
        IF reg_ctr_taa.fin_provision IS NOT NULL OR
           reg_ctr_taa.fin_provision <> '31/12/1899' THEN
            LET fecha_paso = reg_ctr_taa.fin_provision
            LET desc_provision = fecha_paso[9,10],"/",
                                 fecha_paso[6,7],"/",
                                 fecha_paso[1,4]
        ELSE
            LET desc_provision = 'EN PROCESO'
        END IF
    ELSE
        LET desc_provision = '    NO    '
    END IF

    IF reg_ctr_taa.ini_liquida IS NOT NULL OR
       reg_ctr_taa.ini_liquida <> '31/12/1899' THEN
        IF reg_ctr_taa.fin_liquida IS NOT NULL OR
           reg_ctr_taa.fin_liquida <> '31/12/1899' THEN
            LET fecha_paso = reg_ctr_taa.fin_liquida
            LET desc_liquida = fecha_paso[9,10],"/",
                               fecha_paso[6,7],"/",
                               fecha_paso[1,4]
        ELSE
            LET desc_liquida = 'EN PROCESO'
        END IF
    ELSE
        LET desc_liquida = '    NO    '
    END IF

    LET nombre_archivo     = arr_ctr_taa[j].nombre_archivo
    LET id_operacion       = arr_ctr_taa[j].id_operacion
    LET folio              = arr_ctr_taa[j].folio
    LET fecha_presentacion = arr_ctr_taa[j].fecha_presentacion
    LET fecha_liquidacion  = arr_ctr_taa[j].fecha_liquidacion
    LET reg_incorpora      = reg_ctr_taa.reg_incorpora


    IF fecha_liquidacion  IS NULL OR
       fecha_liquidacion = '31/12/1899' THEN
         LET fecha_liquidacion = NULL
    END IF

    OPEN WINDOW ventana_2 AT 2,2 WITH FORM "TAAC0152" ATTRIBUTE (BORDER)
    DISPLAY " TAAC0015           CONTROL ARCHIVO SALDOS TAA RECEPTORA                      " AT 2,1 ATTRIBUTE(REVERSE)
    DISPLAY "----------------------------- CTRL-C-   Regresar ---------- ------------------" AT 4,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING " DD-MM-YYYY " AT 2,62 ATTRIBUTE(REVERSE)

    DISPLAY BY NAME nombre_archivo,
                    id_operacion,
                    folio,
                    fecha_presentacion,
                    fecha_liquidacion,
                    desc_recepcion,
                    desc_incorpora,
                    desc_provision,
                    desc_liquida,
                    reg_incorpora

    PROMPT "PRESIONE ENTER PARA REGRESAR A LA CONSULTA" FOR enter
    CLOSE WINDOW ventana_2
    CURRENT WINDOW IS ventana_1

END FUNCTION
