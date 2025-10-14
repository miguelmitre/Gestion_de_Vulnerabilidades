#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETC940  => CONFIRMA SOLICITUDES DE RETIROS TOTALES Y PARCIALES       #
#                                                                               #
#Fecha creacion    => 24 DE OCTUBRE DE 2009                                     #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   => 25 DE NOVIEMBRE DE 2009                                   #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                  => Se agregan validaciones para dejar pasar las solicitudes  #
#                     de acuerdo a su regimen (Solo Pension ISSSTE)             #                      
#Fecha actualiz.   => 13 DE JUNIO DE 2012                                       #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                  => Se agrega el tipo de retiro I a los tipos validos de      #
#                     retiros totales ISSSTE                                    #
#Sistema           => RET                                                       #
#################################################################################
DATABASE safre_af
GLOBALS

    DEFINE gr_edo RECORD
        capturado             LIKE ret_estado.estado_solicitud ,
        confirmado            LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE gr_modo RECORD
        tot     CHAR(3),
        par     CHAR(3)
    END RECORD

    DEFINE gr_accion RECORD
        con     CHAR(3),
        des     CHAR(3)
    END RECORD

    DEFINE #glo #date
        HOY                   DATE

    DEFINE #glo #char
        gc_where              CHAR(400) ,
        enter                 CHAR(001) ,
        gs_usuario            CHAR(020)

    DEFINE #glo #smallint
        gs_codigo_afore       ,
        gs_peiss              ,
        gs_flag_err           SMALLINT

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("RETC940.log")

    CALL init()

    OPEN WINDOW main_win AT 4,4 WITH 19 ROWS, 75 COLUMNS ATTRIBUTE(BORDER)
    DISPLAY " RETC940      CONFIRMACION DE SOLICITUDES DE RETIROS ISSSTE                " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    MENU "CONFIRMA"
        #--
        COMMAND "Totales" "Confirma/Desconfirma solicitudes de Retiros Totales ISSSTE "
            MENU "Totales"
                COMMAND "Confirma" "Confirma solicitudes de Retiros Totales ISSSTE "
                    CALL f_genera_proceso(gr_modo.tot,gr_accion.con)
                
                COMMAND "Desconfirma" "Reversa la confirmacion de solicitudes de Retiros Totales ISSSTE "
                    CALL f_genera_proceso(gr_modo.tot,gr_accion.des)

                COMMAND "Regresa" "Regresa al menu anterior"
                    EXIT MENU

            END MENU
        #-- 
        COMMAND "Parciales" "Confirma/Desconfirma solicitudes de Retiros Parciales ISSSTE "
            MENU "Parciales"
                COMMAND "Confirma" "Confirma solicitudes de Retiros Parciales ISSSTE "
                    CALL f_genera_proceso(gr_modo.par,gr_accion.con)

                COMMAND "Desconfirma" "Reversa la confirmacion de solicitudes de Retiros Parciales ISSSTE "
                    CALL f_genera_proceso(gr_modo.par,gr_accion.des)

                COMMAND "Regresa" "Regresa al menu anterior"
                    EXIT MENU
            END MENU

        COMMAND "Salir" "Salir del Programa "
            EXIT MENU

    END MENU

    CLOSE WINDOW main_win

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    LET HOY = TODAY
    
    LET gr_modo.tot     = "tot"
    LET gr_modo.par     = "par"
    LET gr_accion.con   = "con"
    LET gr_accion.des   = "des"

    ----- CODIGOS AFORES -----
    SELECT codigo_afore,
           USER
    INTO   gs_codigo_afore,
           gs_usuario
    FROM   tab_afore_local

    LET gs_peiss = 578 -- Clave PENSION ISSSTE

    ----- ESTADOS DE S0LICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.capturado
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "CAPTURADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.confirmado
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "CONFIRMADO"

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_proceso : Ejecuta las acciones necesarias para realizar la       #
#                    confirmacion/desconfirmacion de las solicitudes del    #
#                    tipo indicado por pr_modalidad                         #
#---------------------------------------------------------------------------#
FUNCTION f_genera_proceso(pr_modalidad, pr_accion)

    DEFINE 
        pr_modalidad        ,
        pr_accion           CHAR(03)

    CALL f_captura_datos(pr_modalidad, pr_accion) 
        RETURNING gs_flag_err, gc_where
    
    IF gs_flag_err = 0 THEN
        CALL f_confirmacion(gc_where, pr_modalidad, pr_accion)
        CLOSE WINDOW retc9402
    END IF
    
    CLOSE WINDOW retc9401

END FUNCTION


#---------------------------------------------------------------------------#
# f_captura_datos : Captura los datos necesarios para armar el query que    #
#                   obtiene los registros a procesarse                      #
#---------------------------------------------------------------------------#
FUNCTION f_captura_datos(pc_tipo_ret, pc_oper)

    DEFINE
        pc_tipo_ret     ,
        pc_oper         CHAR(3)

    DEFINE lr_datos RECORD
        fec_inicio      LIKE ret_sol_issste_tx.fecha_captura,
        fec_fin         LIKE ret_sol_issste_tx.fecha_captura,
        regimen         LIKE ret_sol_issste_tx.regimen      ,
        curp            LIKE ret_sol_issste_tx.curp         ,
        nss             LIKE ret_sol_issste_tx.nss
    END RECORD

    DEFINE
        ls_exit         SMALLINT

    DEFINE
        lc_regimen      CHAR(100),
        lc_nss          CHAR(100),
        lc_curp         CHAR(100),
        lc_fecha        CHAR(100),
        lc_where        CHAR(400)


    OPEN WINDOW retc9401 AT 4,4 WITH FORM "RETC9401" ATTRIBUTE (BORDER)
    DISPLAY " <CTRL-C> SALIR                                                <ESC> BUSCAR   " AT 1,1 ATTRIBUTE(REVERSE)

    IF pc_oper = gr_accion.con THEN
        IF pc_tipo_ret = gr_modo.tot THEN
            DISPLAY " RETC940   CONFIRMACION DE SOLICITUDES DE RETIROS ISSSTE TOTALES              " AT 3,1 ATTRIBUTE(REVERSE)
        ELSE
            DISPLAY " RETC940  CONFIRMACION DE SOLICITUDES DE RETIROS ISSSTE PARCIALES             " AT 3,1 ATTRIBUTE(REVERSE)
        END IF
    ELSE
        IF pc_tipo_ret = gr_modo.tot THEN
            DISPLAY " RETC940   REVERSO - CONFIRMACION DE SOL RETIROS ISSSTE TOTALES             " AT 3,1 ATTRIBUTE(REVERSE)
        ELSE
            DISPLAY " RETC940   REVERSO - CONFIRMACION DE SOL RETIROS ISSSTE PARCIALES           " AT 3,1 ATTRIBUTE(REVERSE)
        END IF
    END IF

    DISPLAY HOY USING"DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

    LET ls_exit             = 0
    INITIALIZE lr_datos.* TO NULL

    INPUT BY NAME lr_datos.* WITHOUT DEFAULTS

        AFTER FIELD fec_inicio
            IF lr_datos.fec_inicio IS NOT NULL THEN
                NEXT FIELD fec_fin
            END IF

        AFTER FIELD fec_fin
            IF lr_datos.fec_fin IS NOT NULL THEN
                IF lr_datos.fec_inicio IS NULL THEN
                    ERROR "SI CAPTURA UN INTERVALO, LA FECHA INICIAL NO PUEDE SER NULA "
                    SLEEP 2
                    ERROR ""
                    NEXT FIELD fec_inicio
                ELSE
                    IF lr_datos.fec_inicio > lr_datos.fec_fin THEN
                        ERROR "LA FECHA INICIAL NO PUEDE SER MAYOR A LA FECHA FINAL"
                        SLEEP 2
                        ERROR ""
                        NEXT FIELD fec_inicio
                    END IF
                END IF
            END IF

        AFTER FIELD regimen
            IF gs_codigo_afore = gs_peiss THEN
                IF lr_datos.regimen IS NULL THEN
                    CALL f_muestra_regimen(pc_tipo_ret) RETURNING lr_datos.regimen
                    DISPLAY BY NAME lr_datos.regimen
                ELSE
                    -- Valida si el regimen actual esta disponible para confirmacion
                    IF (NOT f_valida_confirma(pc_tipo_ret,lr_datos.regimen) ) 
                       AND (pc_oper = gr_accion.con)  THEN

                        ERROR "POR EL MOMENTO NO SE PERMITE CONFIRMAR REGISTROS CON ESTE REGIMEN "
                        SLEEP 2
                        ERROR ""
                        NEXT FIELD regimen

                    END IF  -- Valida regimen
                END IF -- Regimen nulo
            END IF -- Codigo Peisss

        ON KEY(ESC)
            IF gs_codigo_afore = gs_peiss THEN
                IF (lr_datos.regimen IS NULL) AND (pc_oper = gr_accion.con) THEN
                    ERROR "DEBE CAPTURAR EL REGIMEN A BUSCAR "
                    SLEEP 2
                    ERROR ""
                    NEXT FIELD regimen
                END IF
            END IF

            IF lr_datos.fec_inicio IS NOT NULL THEN
                IF lr_datos.fec_fin IS NULL THEN
                    ERROR "SI CAPTURA UN INTERVALO, LA FECHA FINAL NO PUEDE SER NULA "
                    SLEEP 2
                    ERROR ""
                    NEXT FIELD fec_fin
                END IF
            END IF

            IF lr_datos.fec_fin IS NOT NULL THEN
                IF lr_datos.fec_inicio IS NULL THEN
                    ERROR "SI CAPTURA UN INTERVALO, LA FECHA INICIAL NO PUEDE SER NULA "
                    SLEEP 2
                    ERROR ""
                    NEXT FIELD fec_inicio
                ELSE
                    IF lr_datos.fec_inicio > lr_datos.fec_fin THEN
                        ERROR "LA FECHA INICIAL NO PUEDE SER MAYOR A LA FECHA FINAL"
                        SLEEP 2
                        ERROR ""
                        NEXT FIELD fec_inicio
                    END IF
                END IF
            END IF

            EXIT INPUT

        ON KEY (CONTROL-C)
            LET ls_exit = 1
            EXIT INPUT

        ON KEY (INTERRUPT)
            LET ls_exit = 1
            EXIT INPUT

    END INPUT

    -- Armamos la seccion de datos del query donde se realizara la busqueda
    IF ls_exit = 0 THEN

        IF lr_datos.regimen IS NOT NULL THEN
            LET lc_regimen = " AND regimen = '", lr_datos.regimen, "' "
        ELSE
            LET lc_regimen = " "
        END IF

        IF lr_datos.curp IS NOT NULL THEN
            LET lc_curp = " AND curp = '", lr_datos.curp, "' "
        ELSE
            LET lc_curp = " "
        END IF

        IF lr_datos.nss IS NOT NULL THEN
            LET lc_nss = " AND nss = '", lr_datos.nss, "' "
        ELSE
            LET lc_nss = " "
        END IF

        IF (lr_datos.fec_inicio IS NOT NULL) AND (lr_datos.fec_fin IS NOT NULL) THEN
            LET lc_fecha = " AND fecha_captura BETWEEN '", lr_datos.fec_inicio,
                           "' AND '", lr_datos.fec_fin, "' "
        ELSE
            LET lc_fecha = " "
        END IF

        LET lc_where = lc_regimen, lc_curp, lc_nss, lc_fecha CLIPPED

        IF (lr_datos.curp IS NULL) AND (lr_datos.nss IS NULL) AND (lr_datos.regimen IS NULL) AND
           (lr_datos.fec_inicio IS NULL) AND (lr_datos.fec_fin IS NULL) THEN
            LET lc_where = " AND 1 = 1 "
        END IF

    END IF

    RETURN ls_exit, lc_where

END FUNCTION

#---------------------------------------------------------------------------#
# f_confirmacion : Obtiene los datos y despliega la pantalla con los        #
#                  comandos que se usaran para realizar la confirmacion o   #
#                  el reverso de la confirmacion de las solicitudes totales #
#                  o parciales                                              #
#---------------------------------------------------------------------------#
FUNCTION f_confirmacion(pc_cond, pc_tipo_ret, pc_oper)

    DEFINE
        pc_oper         CHAR(003),
        pc_tipo_ret     CHAR(003),
        pc_cond         CHAR(400)

    DEFINE lar_datos_sol ARRAY[5000] OF RECORD
        id_confirma        CHAR(1)                               ,
        curp               LIKE ret_sol_issste_tx.curp           ,
        nombre_comp        CHAR(40)                              ,
        fecha_captura      LIKE ret_sol_issste_tx.fecha_captura  ,
        tipo_retiro        LIKE ret_sol_issste_tx.tipo_retiro    ,
        sec_pension        LIKE ret_sol_issste_tx.sec_pension
    END RECORD

    DEFINE lr_soli RECORD
        curp               LIKE ret_sol_issste_tx.curp           ,
        paterno            LIKE ret_sol_issste_tx.paterno_afore  ,
        materno            LIKE ret_sol_issste_tx.materno_afore  ,
        nombre             LIKE ret_sol_issste_tx.nombre_afore   ,
        fecha_captura      LIKE ret_sol_issste_tx.fecha_captura  ,
        tipo_retiro        LIKE ret_sol_issste_tx.tipo_retiro    ,
        sec_pension        LIKE ret_sol_issste_tx.sec_pension    ,
        estado_sol         LIKE ret_sol_issste_tx.estado_solicitud
    END RECORD

    DEFINE ls_edo_sol LIKE ret_sol_issste_tx.estado_solicitud

    DEFINE
        li_tot_elem         ,
        li_arr_elem         ,
        li_scr_elem         ,
        li_cont             INTEGER

    DEFINE
        ls_flag             SMALLINT

    DEFINE
        lc_marca            CHAR(1)     ,
        lc_query            CHAR(1000)


    OPEN WINDOW retc9402 AT 4,4 WITH FORM "RETC9402" ATTRIBUTE (BORDER)

    DISPLAY " RETC940                                                     <CTRL-C> SALIR    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " <CTRL-U> MARCA/DESMARCA UNA SOL              <CTRL-T> MARCA/DESMARCA TODAS    " AT 2,1 ATTRIBUTE(REVERSE)
    DISPLAY " <CTRL-V> VER/MODIF SOLICITUD               <CTRL-E> EJECUTA MARCA/DESMARCA    " AT 3,1 ATTRIBUTE(REVERSE)

    LET li_cont = 1
    LET ls_flag = 0

    IF pc_oper = gr_accion.con THEN
        LET ls_edo_sol = gr_edo.capturado
    ELSE
        LET ls_edo_sol = gr_edo.confirmado
    END IF


    IF pc_tipo_ret = gr_modo.tot THEN
        -- query para totales
        LET lc_query = " SELECT curp, ",
                            " paterno_afore,    ",
                            " materno_afore,    ",
                            " nombre_afore,     ",
                            " fecha_captura,    ",
                            " tipo_retiro,      ",
                            " sec_pension,      ",
                            " estado_solicitud  ",
                       " FROM ret_sol_issste_tx ",
                       " WHERE  estado_solicitud = ? ",
                       pc_cond CLIPPED ,
                       " ORDER BY 5,6,2 "
    ELSE
        -- query para parciales
        LET lc_query = " SELECT curp, ",
                            " apellido_paterno, ",
                            " apellido_materno, ",
                            " nombre,           ",
                            " fecha_captura,    ",
                            " 'F',              ",
                            " sec_pension,      ",
                            " estado_solicitud  ",
                       " FROM ret_parcial_issste ",
                       " WHERE  estado_solicitud = ? ",
                       pc_cond CLIPPED ,
                       " ORDER BY 5,6,2 "
    END IF

    PREPARE prp_datos_tot FROM lc_query
    DECLARE cur_datos_tot CURSOR FOR prp_datos_tot

    FOREACH cur_datos_tot USING ls_edo_sol
                          INTO lr_soli.*

        LET lar_datos_sol[li_cont].id_confirma    = " "
        LET lar_datos_sol[li_cont].curp           = lr_soli.curp
        LET lar_datos_sol[li_cont].fecha_captura  = lr_soli.fecha_captura
        LET lar_datos_sol[li_cont].tipo_retiro    = lr_soli.tipo_retiro
        LET lar_datos_sol[li_cont].sec_pension    = lr_soli.sec_pension

        LET lar_datos_sol[li_cont].nombre_comp = lr_soli.paterno CLIPPED, " ",
                                                 lr_soli.materno CLIPPED, " ",
                                                 lr_soli.nombre CLIPPED
        LET li_cont = li_cont + 1

        IF li_cont > 5000 THEN
            LET li_cont = li_cont - 1
            PROMPT "CAPACIDAD DE ARREGLO REBASADA, SE MUESTRAN ", li_cont, " REGISTROS." FOR CHAR enter
            LET li_cont = li_cont + 1
            EXIT FOREACH
        END IF

    END FOREACH

    IF li_cont = 1 THEN 
        PROMPT "NO EXISTEN REGISTROS CON EL CRITERIO CAPTURADO ... ENTER PARA SALIR " FOR CHAR enter
        RETURN 
    END IF 

    CALL SET_COUNT(li_cont - 1)
    DISPLAY ARRAY lar_datos_sol TO scr_soli.*

        -- Marca/Desmarca la solicitud activa
        ON KEY (CONTROL-U)
            LET li_arr_elem = ARR_CURR()
            LET li_scr_elem = SCR_LINE()

            IF lar_datos_sol[li_arr_elem].id_confirma = " " THEN
                LET lar_datos_sol[li_arr_elem].id_confirma    = "x"
            ELSE
                LET lar_datos_sol[li_arr_elem].id_confirma    = " "
            END IF

            DISPLAY lar_datos_sol[li_arr_elem].id_confirma TO scr_soli[li_scr_elem].id_confirma

        -- Marca/Desmarca todas las solicitudes
        ON KEY (CONTROL-T)
            LET li_tot_elem = ARR_COUNT()
            LET li_scr_elem = 0

            IF NOT ls_flag THEN
                LET ls_flag     = 1
                LET lc_marca    = "x"
            ELSE
                LET ls_flag     = 0
                LET lc_marca    = " "
            END IF

            FOR li_arr_elem = 1 TO li_tot_elem

                -- Como el screen array tiene 12 elementos definidos, al llegar a este se regresa
                -- el contador de pantalla a 1, en otro caso lo incrementamos
                IF li_arr_elem > 12 THEN
                    LET li_scr_elem = 1
                ELSE
                    LET li_scr_elem = li_scr_elem + 1
                END IF

                LET lar_datos_sol[li_arr_elem].id_confirma    = lc_marca
                DISPLAY lar_datos_sol[li_arr_elem].id_confirma TO scr_soli[li_scr_elem].id_confirma
            END FOR

        -- Abre el programa de Captura para Consultar y/o modificar la solicitud
        ON KEY (CONTROL-V)
            LET li_arr_elem = ARR_CURR()

            CALL f_consulta_solicitud(pc_tipo_ret                             ,
                                      lar_datos_sol[li_arr_elem].curp         ,
                                      lar_datos_sol[li_arr_elem].fecha_captura,
                                      lar_datos_sol[li_arr_elem].tipo_retiro  )

        -- Ejecuta la marca/desmarca de los registros seleccionados
        ON KEY (CONTROL-E)
            
            IF pc_oper = gr_accion.con THEN
                DISPLAY " EJECUTANDO CONFIRMACION ... " AT 19,2 ATTRIBUTE(REVERSE)
            ELSE
                DISPLAY " EJECUTANDO REVERSO DE CONFIRMACION ... " AT 19,2 ATTRIBUTE(REVERSE)
            END IF
            
            LET li_tot_elem = ARR_COUNT()
            LET li_scr_elem = 0

            FOR li_arr_elem = 1 TO li_tot_elem

                -- Como el screen array tiene 12 elementos definidos, al llegar a este se regresa
                -- el contador de pantalla a 1, en otro caso lo incrementamos
                IF li_arr_elem > 12 THEN
                    LET li_scr_elem = 1
                ELSE
                    LET li_scr_elem = li_scr_elem + 1
                END IF

                IF lar_datos_sol[li_arr_elem].id_confirma = "x" THEN

                    IF pc_oper = gr_accion.con THEN
                        CALL f_act_confirma_sol(pc_tipo_ret                             ,
                                                lar_datos_sol[li_arr_elem].curp         ,
                                                lar_datos_sol[li_arr_elem].fecha_captura,
                                                lar_datos_sol[li_arr_elem].tipo_retiro  )

                        LET lar_datos_sol[li_arr_elem].id_confirma = "C"
                        DISPLAY lar_datos_sol[li_arr_elem].id_confirma TO scr_soli[li_scr_elem].id_confirma
                    ELSE
                        CALL f_reverso_confirma(pc_tipo_ret                             ,
                                                lar_datos_sol[li_arr_elem].curp         ,
                                                lar_datos_sol[li_arr_elem].fecha_captura,
                                                lar_datos_sol[li_arr_elem].tipo_retiro  )

                        LET lar_datos_sol[li_arr_elem].id_confirma = "x"
                        DISPLAY lar_datos_sol[li_arr_elem].id_confirma TO scr_soli[li_scr_elem].id_confirma
                    END IF
                END IF
            END FOR

            IF pc_oper = gr_accion.con THEN
                PROMPT "CONFIRMACION TERMINADA... PRESIONE ENTER PARA SALIR AL MENU" FOR CHAR enter
            ELSE
                PROMPT "REVERSO DE CONFIRMACION TERMINADO... PRESIONE ENTER PARA SALIR AL MENU" FOR CHAR enter
            END IF

            EXIT DISPLAY

    END DISPLAY

END FUNCTION

#---------------------------------------------------------------------------#
# f_act_confirma_sol : Actualiza la solicitud a estado confirmado           #
#---------------------------------------------------------------------------#
FUNCTION f_act_confirma_sol(pr_confirma)

    DEFINE pr_confirma RECORD
        tipo_oper   CHAR(3)                             ,
        curp        LIKE ret_sol_issste_tx.curp         ,
        fec_cap     LIKE ret_sol_issste_tx.fecha_captura,
        tipo_ret    LIKE ret_sol_issste_tx.tipo_retiro
    END RECORD

    IF pr_confirma.tipo_oper = gr_modo.tot THEN
        UPDATE ret_sol_issste_tx
        SET    estado_solicitud = gr_edo.confirmado ,
               usuario_confirma = gs_usuario        ,
               fecha_confirma   = HOY
        WHERE  curp             = pr_confirma.curp
        AND    estado_solicitud = gr_edo.capturado
        AND    fecha_captura    = pr_confirma.fec_cap
        AND    tipo_retiro      = pr_confirma.tipo_ret
    ELSE
        UPDATE ret_parcial_issste
        SET    estado_solicitud = gr_edo.confirmado ,
               usuario_confirma = gs_usuario        ,
               fecha_confirma   = HOY
        WHERE  curp             = pr_confirma.curp
        AND    estado_solicitud = gr_edo.capturado
        AND    fecha_captura    = pr_confirma.fec_cap
    END IF

END FUNCTION

#---------------------------------------------------------------------------#
# f_reverso_confirma : Actualiza la solicitud de estado confirmado a        #
#                      capturado                                            #
#---------------------------------------------------------------------------#
FUNCTION f_reverso_confirma(pr_confirma)

    DEFINE pr_confirma RECORD
        tipo_oper   CHAR(3)                             ,
        curp        LIKE ret_sol_issste_tx.curp         ,
        fec_cap     LIKE ret_sol_issste_tx.fecha_captura,
        tipo_ret    LIKE ret_sol_issste_tx.tipo_retiro
    END RECORD

    IF pr_confirma.tipo_oper = gr_modo.tot THEN
        UPDATE ret_sol_issste_tx
        SET    estado_solicitud = gr_edo.capturado  ,
               usuario_confirma = NULL              ,
               fecha_confirma   = NULL
        WHERE  curp             = pr_confirma.curp
        AND    estado_solicitud = gr_edo.confirmado
        AND    fecha_captura    = pr_confirma.fec_cap
        AND    tipo_retiro      = pr_confirma.tipo_ret
    ELSE
        UPDATE ret_parcial_issste
        SET    estado_solicitud = gr_edo.capturado  ,
               usuario_confirma = NULL              ,
               fecha_confirma   = NULL
        WHERE  curp             = pr_confirma.curp
        AND    estado_solicitud = gr_edo.confirmado
        AND    fecha_captura    = pr_confirma.fec_cap
    END IF
END FUNCTION

#---------------------------------------------------------------------------#
# f_consulta_solicitud : Ejecuta el comando que abre el programa de captura #
#                        de solicitudes para modificarla o consultarla      #
#---------------------------------------------------------------------------#
FUNCTION f_consulta_solicitud(pr_confirma)

    DEFINE pr_confirma RECORD
        modalidad   CHAR(003)                           ,
        curp        LIKE ret_sol_issste_tx.curp         ,
        fec_cap     LIKE ret_sol_issste_tx.fecha_captura,
        tipo_ret    LIKE ret_sol_issste_tx.tipo_retiro
    END RECORD

    DEFINE lc_nss LIKE ret_sol_issste_tx.nss

    DEFINE li_consec LIKE ret_sol_issste_tx.consecutivo

    DEFINE
        lc_comando      CHAR(100)

    IF pr_confirma.modalidad = gr_modo.tot THEN
        SELECT nss,
               consecutivo
        INTO   lc_nss,
               li_consec
        FROM   ret_sol_issste_tx
        WHERE  curp             = pr_confirma.curp
        AND    fecha_captura    = pr_confirma.fec_cap
        AND    tipo_retiro      = pr_confirma.tipo_ret
    ELSE
        SELECT nss,
               consecutivo
        INTO   lc_nss,
               li_consec
        FROM   ret_parcial_issste
        WHERE  curp             = pr_confirma.curp
        AND    fecha_captura    = pr_confirma.fec_cap
    END IF

    LET lc_comando = "fglgo RETM920.4gi ",
                     "M ",
                     pr_confirma.curp, " ",
                     lc_nss, " ",
                     pr_confirma.tipo_ret, " ",
                     li_consec
    RUN lc_comando

END FUNCTION

#---------------------------------------------------------------------------#
# f_muestra_regimen : Despliega la pantalla con el catalogo del regimen.    #
#                     Muestra los valores que pueden ser capturados de      #
#                     acuerdo a lo que se tiene confirmado actualmente      #
#---------------------------------------------------------------------------#
FUNCTION f_muestra_regimen(pc_retiro)

    DEFINE
        pc_retiro       CHAR(03)

    DEFINE lr_reg ARRAY[5] OF RECORD
        codigo            CHAR(02),
        descripcion       CHAR(50)
    END RECORD

    DEFINE
        lc_tabla        CHAR(100),
        lc_prepare      CHAR(500)

    DEFINE
        ls_pos          SMALLINT

    DEFINE
        lc_codigo       CHAR(02)

    OPEN WINDOW retc9403 AT 12,36 WITH FORM "RETC9403" ATTRIBUTE(BORDER)
    DISPLAY "    CLAVE DE REGIMEN A CONFIRMAR     " AT 2,1  ATTRIBUTE(REVERSE)

    IF pc_retiro = gr_modo.tot THEN
        LET lc_tabla = " FROM ret_sol_issste_tx "
    ELSE
        LET lc_tabla = " FROM ret_parcial_issste "
    END IF

    WHILE TRUE
        LET lc_prepare = " SELECT UNIQUE(regimen), ",
                         "        CASE regimen ",
                         "            WHEN 'RO' THEN 'REGIMEN ORDINARIO' ",
                         "            WHEN 'DT' THEN 'DECIMO TRANSITORIO' ",
                         "        END descripcion ",
                         lc_tabla ,
                         " WHERE  estado_solicitud = ? "

        PREPARE prp_dat FROM lc_prepare
        DECLARE cur_dat CURSOR FOR prp_dat

        LET ls_pos = 1

        FOREACH cur_dat USING gr_edo.confirmado
                        INTO lr_reg[ls_pos].*
            
            LET ls_pos = ls_pos + 1
            IF ls_pos >= 5 THEN
                ERROR "    Fue Sobrepasada la capacidad maxima del arreglo"
                ATTRIBUTE(NORMAL)
                EXIT FOREACH
            END IF
        END FOREACH

        IF (ls_pos-1) < 1 THEN
            LET lr_reg[1].codigo        = "RO"
            LET lr_reg[1].descripcion   = "REGIMEN ORDINARIO"

            LET lr_reg[2].codigo        = "DT"
            LET lr_reg[2].descripcion   = "DECIMO TRANSITORIO"

            LET ls_pos = 3
        END IF

        CALL SET_COUNT(ls_pos-1)
        DISPLAY ARRAY lr_reg TO scr_2.*

            ON KEY (CONTROL-C, INTERRUPT)
                LET ls_pos = 0
                EXIT DISPLAY

            ON KEY ( CONTROL-M )
                LET ls_pos      = ARR_CURR()
                LET lc_codigo   = lr_reg[ls_pos].codigo
                EXIT DISPLAY
        
        END DISPLAY

        IF ls_pos <> 0 THEN
            EXIT WHILE
        END IF
    
    END WHILE
    
    CLOSE WINDOW retc9403
    RETURN lc_codigo

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_confirma : Valida que el regimen capturado puede ser usado para  #
#                     la confirmacion de acuerdo a la modalidad de retiro   #
#---------------------------------------------------------------------------#
FUNCTION f_valida_confirma(pc_retiro, pr_regimen)

    DEFINE pr_regimen LIKE ret_sol_issste_tx.regimen

    DEFINE
        pc_retiro       CHAR(03)

    DEFINE 
        ls_valido       SMALLINT 

    LET ls_valido = 0

    IF pc_retiro = gr_modo.tot THEN
        SELECT "OK"
        FROM   ret_sol_issste_tx
        WHERE  estado_solicitud = gr_edo.confirmado
        GROUP BY 1
        
        IF STATUS = NOTFOUND THEN
            -- No existen confirmados, por lo que cualquier regimen se acepta
            LET ls_valido = 1
        ELSE
            -- Existen confirmados, se verifica si se permite confirmar del regimen actual 
            SELECT "OK"
            FROM   ret_sol_issste_tx
            WHERE  estado_solicitud = gr_edo.confirmado
            AND    regimen          = pr_regimen
            GROUP BY 1        
        
            IF STATUS <> NOTFOUND THEN
                LET ls_valido = 1
            END IF 
        END IF
    ELSE
        -- Parciales
        SELECT "OK"
        FROM   ret_parcial_issste
        WHERE  estado_solicitud = gr_edo.confirmado
        GROUP BY 1
        
        IF STATUS = NOTFOUND THEN
            -- No existen confirmados, por lo que cualquier regimen se acepta
            LET ls_valido = 1
        ELSE
            -- Existen confirmados, se verifica si se permite confirmar del regimen actual 
            SELECT "OK"
            FROM   ret_parcial_issste
            WHERE  estado_solicitud = gr_edo.confirmado
            AND    regimen          = pr_regimen
            GROUP BY 1        
        
            IF STATUS <> NOTFOUND THEN
                LET ls_valido = 1
            END IF 
        END IF
        
    END IF
    
    RETURN ls_valido

END FUNCTION
