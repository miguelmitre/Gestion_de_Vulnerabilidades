#################################################################################
#Owner             => E.F.P.                                                    #
#Programa PENM101  => CONFIRMA SOLICITUDES DE RETIROS DE PENSION MINIMA         #
#                     GARANTIZADA                                               #
#Fecha creacion    => 26 DE MARZO DE 2010                                       #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   =>                                                           #
#Actualizacion     =>                                                           #
#                  =>                                                           #
#Sistema           => RET                                                       #
#Actualizacion     => EMMANUEL REYES 26/02/2015 ADECUACION PARA OP13 PLUS       #
#                  => EVITA QUE EL NSS SE CONFIRME CUANDO NO TIENE EXPEDIENTE   #
#                  => DE IDENTIFICACION                                         #
#################################################################################
DATABASE safre_af
GLOBALS

    DEFINE gr_edo RECORD
        capturado             LIKE pen_estado_pmg.estado_solicitud ,
        confirmado            LIKE pen_estado_pmg.estado_solicitud
    END RECORD

    DEFINE gr_accion RECORD
        con     CHAR(3),
        des     CHAR(3)
    END RECORD

    DEFINE #glo #date
        HOY                   DATE

    DEFINE #glo #char
        gc_where              CHAR(200) ,
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

    CALL init()
    CALL STARTLOG(gs_usuario CLIPPED||"PENM101.log")

    OPEN WINDOW main_win AT 4,4 WITH 19 ROWS, 75 COLUMNS ATTRIBUTE(BORDER)
    DISPLAY " PENM101            CONFIRMACION DE SOLICITUDES DE PMG                     " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    MENU "CONFIRMA"

        COMMAND "Confirma" "Confirma solicitudes de Pension Minima Garantizada "
            CALL f_genera_proceso(gr_accion.con)

        COMMAND "Desconfirma" "Reversa la confirmacion de solicitudes de Pension Minima Garantizada "
            CALL f_genera_proceso(gr_accion.des)

        COMMAND "Salir" "Salir del Programa"
            EXIT MENU

    END MENU

    CLOSE WINDOW main_win

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    LET HOY = TODAY
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
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "CAPTURADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.confirmado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "CONFIRMADO"
    
    --OP13 Plus 26/02/2015
    LET gc_where = "EXECUTE FUNCTION fn_verifica_expediente(?,?,?)"
    PREPARE exe_verifica_exp FROM gc_where
    
    LET gc_where = ""

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_proceso : Ejecuta las acciones necesarias para realizar la       #
#                    confirmacion/desconfirmacion de las solicitudes        #
#---------------------------------------------------------------------------#
FUNCTION f_genera_proceso(pr_accion)

    DEFINE
        pr_accion           CHAR(03)

    CALL f_captura_datos(pr_accion)
        RETURNING gs_flag_err, gc_where

    IF gs_flag_err = 0 THEN
        CALL f_confirmacion(gc_where,pr_accion)
        CLOSE WINDOW PENM1012
    END IF

    CLOSE WINDOW PENM1011

END FUNCTION


#---------------------------------------------------------------------------#
# f_captura_datos : Captura los datos necesarios para armar el query que    #
#                   obtiene los registros a procesarse                      #
#---------------------------------------------------------------------------#
FUNCTION f_captura_datos(pc_oper)

    DEFINE
        pc_oper         CHAR(3)

    DEFINE lr_datos RECORD
        fec_inicio      LIKE pen_solicitud_pmg.fecha_captura,
        fec_fin         LIKE pen_solicitud_pmg.fecha_captura,
        nss             LIKE pen_solicitud_pmg.nss          ,
        curp            LIKE pen_solicitud_pmg.curp
    END RECORD

    DEFINE
        ls_exit         SMALLINT

    DEFINE
        lc_nss          CHAR(100),
        lc_curp         CHAR(100),
        lc_fecha        CHAR(100),
        lc_where        CHAR(200)


    OPEN WINDOW PENM1011 AT 4,4 WITH FORM "PENM1011" ATTRIBUTE (BORDER)
    DISPLAY " <CTRL-C> SALIR                                                <ESC> BUSCAR   " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

    IF pc_oper = gr_accion.con THEN
        DISPLAY " PENM101        CONFIRMACION DE SOLICITUDES DE RETIROS PMG                    " AT 3,1 ATTRIBUTE(REVERSE)
    ELSE
        DISPLAY " PENM101  REVERSO DE CONFIRMACION DE SOLICITUDES DE RETIROS PMG               " AT 3,1 ATTRIBUTE(REVERSE)
    END IF

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

        ON KEY(ESC)
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

        ON KEY (CONTROL-C, INTERRUPT)
            LET ls_exit = 1
            EXIT INPUT

    END INPUT

    -- Armamos la seccion de datos del query donde se realizara la busqueda
    IF ls_exit = 0 THEN

        IF lr_datos.nss IS NOT NULL THEN
            LET lc_nss = " AND nss = '", lr_datos.nss, "' "
        ELSE
            LET lc_nss = " "
        END IF

        IF lr_datos.curp IS NOT NULL THEN
            LET lc_curp = " AND curp = '", lr_datos.curp, "' "
        ELSE
            LET lc_curp = " "
        END IF

        IF (lr_datos.fec_inicio IS NOT NULL) AND (lr_datos.fec_fin IS NOT NULL) THEN
            LET lc_fecha = " AND fecha_captura BETWEEN '", lr_datos.fec_inicio,
                           "' AND '", lr_datos.fec_fin, "' "
        ELSE
            LET lc_fecha = " "
        END IF

        LET lc_where = lc_nss, lc_curp, lc_fecha CLIPPED

        IF (lr_datos.nss IS NULL) AND (lr_datos.curp IS NULL) AND
           (lr_datos.fec_inicio IS NULL) AND (lr_datos.fec_fin IS NULL) THEN
            LET lc_where = " AND 1 = 1 "
        END IF

    END IF

    RETURN ls_exit, lc_where

END FUNCTION

#---------------------------------------------------------------------------#
# f_confirmacion : Obtiene los datos y despliega la pantalla con los        #
#                  comandos que se usaran para realizar la confirmacion o   #
#                  el reverso de la confirmacion de las solicitudes PMG     #
#---------------------------------------------------------------------------#
FUNCTION f_confirmacion(pc_cond, pc_oper)

    DEFINE
        pc_oper         CHAR(003),
        pc_cond         CHAR(200)

    DEFINE lar_datos_sol ARRAY[5000] OF RECORD
        id_confirma        CHAR(1)                               ,
        nss                LIKE pen_solicitud_pmg.nss            ,
        nombre_comp        CHAR(40)                              ,
        fecha_captura      LIKE pen_solicitud_pmg.fecha_captura  ,
        tipo_retiro        LIKE pen_solicitud_pmg.tipo_retiro    ,
        sec_contrato       LIKE pen_solicitud_pmg.sec_contrato
    END RECORD

    DEFINE lr_soli RECORD
        nss                LIKE pen_solicitud_pmg.nss            ,
        paterno            LIKE afi_mae_afiliado.paterno         ,
        materno            LIKE afi_mae_afiliado.materno         ,
        nombre             LIKE afi_mae_afiliado.nombres         ,
        fecha_captura      LIKE pen_solicitud_pmg.fecha_captura  ,
        tipo_retiro        LIKE pen_solicitud_pmg.tipo_retiro    ,
        sec_contrato       LIKE pen_solicitud_pmg.sec_contrato   ,
        estado_sol         LIKE pen_solicitud_pmg.estado_solicitud
    END RECORD

    DEFINE ls_edo_sol LIKE pen_solicitud_pmg.estado_solicitud

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
        
    DEFINE ls_indicador   SMALLINT --1 = tiene expediente, 0 = no tiene expediente


    OPEN WINDOW PENM1012 AT 4,4 WITH FORM "PENM1012" ATTRIBUTE (BORDER)

    DISPLAY " PENM101                                                     <CTRL-C> SALIR    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " <CTRL-U> MARCA/DESMARCA UNA SOL              <CTRL-T> MARCA/DESMARCA TODAS    " AT 2,1 ATTRIBUTE(REVERSE)

    LET li_cont = 1
    LET ls_flag = 0

    IF pc_oper = gr_accion.con THEN
        LET ls_edo_sol = gr_edo.capturado
        DISPLAY " <CTRL-V> VER/MODIF SOLICITUD                 <CTRL-E> EJECUTA CONFIRMACION    " AT 3,1 ATTRIBUTE(REVERSE)
    ELSE
        LET ls_edo_sol = gr_edo.confirmado
        DISPLAY " <CTRL-V> CONSULTA SOLICITUD              <CTRL-E> EJECUTA REVERSO CONFIRMA    " AT 3,1 ATTRIBUTE(REVERSE)
    END IF

    LET lc_query = " SELECT nss          ,   ",
                          " ' '          ,   ",
                          " ' '          ,   ",
                          " ' '          ,   ",
                          " fecha_captura,   ",
                          " tipo_retiro  ,   ",
                          " sec_contrato ,   ",
                          " estado_solicitud ",
                   " FROM pen_solicitud_pmg ",
                   " WHERE  estado_solicitud = ? ",
                   pc_cond CLIPPED ,
                   " ORDER BY 5,6,1 "

    PREPARE prp_datos_tot FROM lc_query
    DECLARE cur_datos_tot CURSOR FOR prp_datos_tot

    FOREACH cur_datos_tot USING ls_edo_sol
                          INTO lr_soli.*

        --OP13 Plus ERP 26/02/2015 inicio
        CALL f_verifica_expediente(lr_soli.nss) RETURNING ls_indicador
        
        IF ls_indicador = 0 THEN --No tiene expediente
        	 OPEN WINDOW aux_win AT 12,6 WITH 2 ROWS, 70 COLUMNS ATTRIBUTE(BORDER)
        	 PROMPT "El NSS: ",lr_soli.nss," no cuenta con expediente." FOR CHAR enter
        	 CLOSE WINDOW aux_win
        ELSE --Si tiene expediente
            LET lar_datos_sol[li_cont].id_confirma    = " "
            LET lar_datos_sol[li_cont].nss            = lr_soli.nss
            LET lar_datos_sol[li_cont].fecha_captura  = lr_soli.fecha_captura
            LET lar_datos_sol[li_cont].tipo_retiro    = lr_soli.tipo_retiro
            LET lar_datos_sol[li_cont].sec_contrato   = lr_soli.sec_contrato
            
            CALL f_obten_datos_afiliado(lr_soli.nss)
                RETURNING lr_soli.paterno ,
                          lr_soli.materno ,
                          lr_soli.nombre
            
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
        END IF --if de expediente
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

            CALL f_consulta_solicitud(lar_datos_sol[li_arr_elem].nss          ,
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
                        CALL f_act_confirma_sol(lar_datos_sol[li_arr_elem].nss          ,
                                                lar_datos_sol[li_arr_elem].fecha_captura,
                                                lar_datos_sol[li_arr_elem].tipo_retiro  )

                        LET lar_datos_sol[li_arr_elem].id_confirma = "C"
                        DISPLAY lar_datos_sol[li_arr_elem].id_confirma TO scr_soli[li_scr_elem].id_confirma
                    ELSE
                        CALL f_reverso_confirma(lar_datos_sol[li_arr_elem].nss          ,
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
        nss         LIKE pen_solicitud_pmg.curp         ,
        fec_cap     LIKE pen_solicitud_pmg.fecha_captura,
        tipo_ret    LIKE pen_solicitud_pmg.tipo_retiro
    END RECORD

    UPDATE pen_solicitud_pmg
    SET    estado_solicitud = gr_edo.confirmado ,
           usuario_confirma = gs_usuario        ,
           fecha_confirma   = HOY               ,
           hora_confirma    = CURRENT HOUR TO SECOND
    WHERE  nss              = pr_confirma.nss
    AND    estado_solicitud = gr_edo.capturado
    AND    fecha_captura    = pr_confirma.fec_cap
    AND    tipo_retiro      = pr_confirma.tipo_ret

END FUNCTION

#---------------------------------------------------------------------------#
# f_reverso_confirma : Actualiza la solicitud de estado confirmado a        #
#                      capturado                                            #
#---------------------------------------------------------------------------#
FUNCTION f_reverso_confirma(pr_confirma)

    DEFINE pr_confirma RECORD
        nss         LIKE pen_solicitud_pmg.nss          ,
        fec_cap     LIKE pen_solicitud_pmg.fecha_captura,
        tipo_ret    LIKE pen_solicitud_pmg.tipo_retiro
    END RECORD

    UPDATE pen_solicitud_pmg
    SET    estado_solicitud = gr_edo.capturado  ,
           usuario_confirma = NULL              ,
           fecha_confirma   = NULL              ,
           hora_confirma    = NULL
    WHERE  nss              = pr_confirma.nss
    AND    estado_solicitud = gr_edo.confirmado
    AND    fecha_captura    = pr_confirma.fec_cap
    AND    tipo_retiro      = pr_confirma.tipo_ret

END FUNCTION

#---------------------------------------------------------------------------#
# f_consulta_solicitud : Ejecuta el comando que abre el programa de captura #
#                        de solicitudes para modificarla o consultarla      #
#---------------------------------------------------------------------------#
FUNCTION f_consulta_solicitud(pr_confirma)

    DEFINE pr_confirma RECORD
        nss         LIKE pen_solicitud_pmg.nss              ,
        fec_cap     LIKE pen_solicitud_pmg.fecha_captura    ,
        tipo_ret    LIKE pen_solicitud_pmg.tipo_retiro
    END RECORD

    DEFINE li_consec    LIKE pen_solicitud_pmg.consecutivo
    DEFINE ls_edo_sol   LIKE pen_solicitud_pmg.estado_solicitud

    DEFINE
        lc_accion           CHAR(001) ,
        lc_comando          CHAR(100)

    SELECT consecutivo      ,
           estado_solicitud
    INTO   li_consec        ,
           ls_edo_sol
    FROM   pen_solicitud_pmg
    WHERE  nss              = pr_confirma.nss
    AND    fecha_captura    = pr_confirma.fec_cap
    AND    tipo_retiro      = pr_confirma.tipo_ret

    IF ls_edo_sol <> gr_edo.capturado THEN
        LET lc_accion = "C"
    ELSE
        LET lc_accion = "M"
    END IF

    LET lc_comando = "fglgo PENM100.4gi ",
                     lc_accion      , " ",
                     pr_confirma.nss, " ",
                     li_consec      , " ",
                     pr_confirma.tipo_ret

    RUN lc_comando

END FUNCTION

#---------------------------------------------------------------------------#
# f_obten_datos_afiliado : Obtiene el nombre del afiliado mediante su nss   #
#---------------------------------------------------------------------------#
FUNCTION f_obten_datos_afiliado(p_nss)

    DEFINE
        p_nss           LIKE pen_solicitud_pmg.nss

    DEFINE lr_datos_af  RECORD #glo #rg_input
        paterno             LIKE afi_mae_afiliado.paterno         ,
        materno             LIKE afi_mae_afiliado.materno         ,
        nombre              LIKE afi_mae_afiliado.nombres
    END RECORD

    INITIALIZE lr_datos_af.* TO NULL

    SELECT paterno ,
           materno ,
           nombres
    INTO   lr_datos_af.paterno  ,
           lr_datos_af.materno  ,
           lr_datos_af.nombre
    FROM   afi_mae_afiliado
    WHERE  n_seguro = p_nss

    RETURN lr_datos_af.*

END FUNCTION

#------------------------------------------------------------------------------#
# f_verifica_expediente Verifica si el NSS ya tiene expediente de id de        #
# acuerdo con la OP13 Plus. Emmanuel Reyes 26/02/2015                          #
#------------------------------------------------------------------------------#
FUNCTION f_verifica_expediente(pc_nss)
   
   DEFINE pc_nss         LIKE afi_mae_afiliado.n_seguro
   
   DEFINE lr_datos RECORD
          n_folio        LIKE afi_mae_afiliado.n_folio       ,
          tipo_solicitud LIKE afi_mae_afiliado.tipo_solicitud
   END RECORD
   
   DEFINE ls_afi_ctr     SMALLINT --Regresa diagnostico segun afi_ctr_expediente       
   DEFINE ls_afi_ref     SMALLINT --Regresa diagnostico segun afi_solicitud_referencias
   DEFINE ls_indicador   SMALLINT --1 = tiene expediente, 0 = no tiene expediente
   
   --Obtiene los datos de afi_mae_afiliado
   WHENEVER ERROR CONTINUE
      SELECT n_folio,
             tipo_solicitud
        INTO lr_datos.*
        FROM afi_mae_afiliado
       WHERE n_seguro = pc_nss
      
      IF SQLCA.SQLCODE < 0 THEN
        ERROR "ERROR OBTENIENDO DATOS EXPEDIENTE ID PARA NSS: ", pc_nss
      END IF
   WHENEVER ERROR STOP
   
   EXECUTE exe_verifica_exp USING pc_nss,lr_datos.*
                             INTO ls_afi_ctr ,
                                  ls_afi_ref
   
   IF ls_afi_ctr = 0 AND ls_afi_ref = 0 THEN
   	  LET ls_indicador = 0
   ELSE
   	  LET ls_indicador = 1
   END IF
   
   RETURN ls_indicador
   
END FUNCTION