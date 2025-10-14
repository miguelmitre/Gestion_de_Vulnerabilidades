#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETC557  => REALIZA EL BARRIDO HISTORICO E INSERTA LOS REGISTROS DE   #
#                     MARCA DE TRABAJADOR PENSIONADO Y MARCA DE RETIRO PARCIAL  #
#                                  (VERSION COPPEL)                             #
#Fecha creacion    => 1 DE AGOSTO DE 2011                                       #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   =>                                                           #
#Actualizacion     =>                                                           #
#                  =>                                                           #
#Sistema           => RET                                                       #
#################################################################################
DATABASE safre_af
GLOBALS

    DEFINE #glo #date
        HOY                         DATE

    DEFINE gr_edo RECORD
        liquidado             LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE gr_edo_iss RECORD
        liquidado             LIKE ret_estado_issste.estado_solicitud
    END RECORD

    DEFINE gr_tipo_tramite RECORD
        trasferencia_imss           SMALLINT    ,
        disposicion_imss            SMALLINT    ,
        parcial_imss                SMALLINT    ,
        trasferencia_issste         SMALLINT    ,
        disposicion_issste          SMALLINT    ,
        parcial_issste              SMALLINT    
    END RECORD

    DEFINE 
        gc_where                    CHAR(400) ,
        enter                       CHAR(001) ,
        gc_usuario                  CHAR(020)

    DEFINE
        gs_flag_err                 ,
        gs_codigo_afore             SMALLINT

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("RETC557.log")

    CALL init()
    CALL f_abre_ventana()

    WHILE TRUE
        PROMPT "¿DESEA EJECUTAR EL BARRIDO HISTORICO? (S/N) " FOR enter
        IF enter MATCHES "[sSnN]" THEN
            IF enter MATCHES "[sS]" THEN
                CALL f_marca_historico()
            ELSE
                PROMPT " EJECUCION CANCELADA...<ENTER> PARA SALIR " FOR enter
            END IF
        
            EXIT WHILE
        END IF
    END WHILE

    CLOSE WINDOW win_despliega

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    DEFINE
        lc_prepare      CHAR(300)

    -- -----------------------------------------------------------------------------

    LET HOY                 = TODAY

    SELECT A.estado_solicitud
    INTO   gr_edo.liquidado
    FROM   ret_estado A
    WHERE  A.descripcion = "LIQUIDADO"

    SELECT A.estado_solicitud
    INTO   gr_edo_iss.liquidado
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "LIQUIDADO"

   ----- CODIGOS AFORES -----
    SELECT codigo_afore,
           USER
    INTO   gs_codigo_afore,
           gc_usuario
    FROM   tab_afore_local

    ----- CODIGO DE TRAMITE -----
    SELECT cod_tramite
    INTO   gr_tipo_tramite.trasferencia_imss
    FROM   tab_tipo_tramite
    WHERE  descripcion = "TRANSFERENCIA"

    SELECT cod_tramite
    INTO   gr_tipo_tramite.disposicion_imss
    FROM   tab_tipo_tramite
    WHERE  descripcion = "DISPOSICION"

    SELECT cod_tramite
    INTO   gr_tipo_tramite.parcial_imss
    FROM   tab_tipo_tramite
    WHERE  descripcion = "RETIRO PARCIAL"

    SELECT cod_tramite
    INTO   gr_tipo_tramite.trasferencia_issste
    FROM   tab_tipo_tramite
    WHERE  descripcion = "TRANSFERENCIA ISSSTE"

    SELECT cod_tramite
    INTO   gr_tipo_tramite.disposicion_issste
    FROM   tab_tipo_tramite
    WHERE  descripcion = "DISPOSICION ISSSTE"

    SELECT cod_tramite
    INTO   gr_tipo_tramite.parcial_issste
    FROM   tab_tipo_tramite
    WHERE  descripcion = "RETIRO PARCIAL ISSSTE"

    ----- INSERTA MARCA PENSIONADO -----
    LET lc_prepare = " "
    LET lc_prepare = " EXECUTE FUNCTION fn_inserta_marca_pen(?,?,?,?,?,?,?) "
    PREPARE eje_marca_pen FROM lc_prepare

END FUNCTION

#---------------------------------------------------------------------------#
# f_marca_historico : Ejecuta las funciones que realizan el barrido de las  #
#                     solicitudes de retiros imss e issste                  #
#---------------------------------------------------------------------------#
FUNCTION f_marca_historico()

    -- -----------------------------------------------------------------------------

    
    CALL f_historico_transferencias()
    CALL f_historico_disposiciones()
    CALL f_historico_parciales()

    CALL f_historico_transfer_issste()
    CALL f_historico_disp_issste()
    CALL f_historico_parcial_issste()

    PROMPT " PROCESO TERMINADO CORRECTAMENTE ...<ENTER> PARA SALIR " FOR CHAR enter

END FUNCTION 

#---------------------------------------------------------------------------#
# f_historico_transferencias : Realiza el barrido de las solicitudes de     #
#                              transferencias para insertar las marcas de   #
#                              pensionados historicas                       #
#---------------------------------------------------------------------------#
FUNCTION f_historico_transferencias()

    DEFINE lr_marca_transf RECORD
        nss             LIKE ret_marca_pensionado.nss           ,
        curp            LIKE ret_marca_pensionado.curp          ,
        folio           LIKE ret_marca_pensionado.folio         ,
        consecutivo     LIKE ret_marca_pensionado.consecutivo   ,
        tipo_retiro     LIKE ret_marca_pensionado.tipo_retiro   ,
        cod_tramite     LIKE ret_marca_pensionado.cod_tramite   ,
        fecha_marca     LIKE ret_marca_pensionado.fecha_marca
    END RECORD

    DEFINE
        ldt_lim_fecha           DATE

    DEFINE
        li_cont                 INTEGER

    DEFINE
        ls_resp                 SMALLINT

    -- -----------------------------------------------------------------------------
    
    INITIALIZE lr_marca_transf.* TO NULL
    
    LET li_cont     = 0

    DISPLAY "MARCA TRANSFERENCIAS IMSS          : ", li_cont AT 6,6

    -- Obtenemos la ultima fecha registrada en la tabla
    LET ldt_lim_fecha = f_ultima_ejecucion(gr_tipo_tramite.trasferencia_imss)

    DECLARE cur_transf CURSOR FOR
        SELECT A.nss            ,
               A.curp           ,
               A.folio          ,
               A.consecutivo    ,
               A.tipo_retiro    ,
               1                , -- trasferencia_imss
               B.fecha_ini
        FROM   ret_trans_imss A ,
               cta_his_marca B  ,
               tab_retiro C
        WHERE  A.nss            = B.nss
        AND    A.consecutivo    = B.correlativo
        AND    A.tipo_retiro    = C.tipo_retiro
        AND    C.movimiento     = B.marca_cod
        AND    B.fecha_ini      > ldt_lim_fecha
        ORDER BY B.fecha_ini    ,
                 A.folio        ,
                 A.nss

    FOREACH cur_transf INTO lr_marca_transf.*

        IF (lr_marca_transf.folio IS NULL) THEN
            LET lr_marca_transf.folio = 0
        END IF

        SELECT "OK"
        FROM   ret_marca_pensionado
        WHERE  nss          = lr_marca_transf.nss
        AND    folio        = lr_marca_transf.folio
        AND    consecutivo  = lr_marca_transf.consecutivo
        AND    cod_tramite  = lr_marca_transf.cod_tramite
        GROUP BY 1

        IF (STATUS = NOTFOUND) THEN

            EXECUTE eje_marca_pen USING lr_marca_transf.*
                                  INTO  ls_resp

            LET li_cont = li_cont + 1
            DISPLAY "MARCA TRANSFERENCIAS IMSS          : ", li_cont AT 6,6

        END IF

        INITIALIZE lr_marca_transf.* TO NULL

    END FOREACH


END FUNCTION

#---------------------------------------------------------------------------#
# f_historico_disposiciones : Realiza el barrido de las solicitudes de      #
#                             disposiciones para insertar las marcas de     #
#                             pensionados historicas                        #
#---------------------------------------------------------------------------#
FUNCTION f_historico_disposiciones()

    DEFINE lr_marca_disp RECORD
        nss             LIKE ret_marca_pensionado.nss           ,
        curp            LIKE ret_marca_pensionado.curp          ,
        folio           LIKE ret_marca_pensionado.folio         ,
        consecutivo     LIKE ret_marca_pensionado.consecutivo   ,
        tipo_retiro     LIKE ret_marca_pensionado.tipo_retiro   ,
        cod_tramite     LIKE ret_marca_pensionado.cod_tramite   ,
        fecha_marca     LIKE ret_marca_pensionado.fecha_marca
    END RECORD

    DEFINE
        ldt_lim_fecha           DATE

    DEFINE
        li_cont                 INTEGER

    DEFINE
        ls_resp                 SMALLINT

    -- -----------------------------------------------------------------------------
    
    LET li_cont     = 0
    INITIALIZE lr_marca_disp.* TO NULL

    DISPLAY "MARCA DISPOSICIONES IMSS           : ", li_cont AT 7,6

    -- Obtenemos la ultima fecha registrada en la tabla
    LET ldt_lim_fecha = f_ultima_ejecucion(gr_tipo_tramite.disposicion_imss)

    -- Disposiciones que se marcan en la carga de datamart
    DECLARE cur_disp_dtm CURSOR FOR
        SELECT A.nss            ,
               A.curp           ,
               A.folio          ,
               A.consecutivo    ,
               A.tipo_retiro    ,
               2                , -- disposicion_imss ,
               C.fecha_carga
        FROM   ret_solicitud_tx A   ,
               ret_det_datamart B   ,
               ret_cza_datamart C  
        WHERE  A.nss            = B.nss
        AND    A.tipo_retiro IN ("D","E","J")
        AND    B.folio          = C.folio
        AND    C.fecha_carga    > ldt_lim_fecha
        ORDER BY C.fecha_carga  ,
                 A.tipo_retiro  ,
                 A.nss

    FOREACH cur_disp_dtm INTO lr_marca_disp.*

        IF (lr_marca_disp.folio IS NULL) THEN
            LET lr_marca_disp.folio = 0
        END IF

        SELECT "OK"
        FROM   ret_marca_pensionado
        WHERE  nss          = lr_marca_disp.nss
        AND    folio        = lr_marca_disp.folio
        AND    consecutivo  = lr_marca_disp.consecutivo
        AND    cod_tramite  = lr_marca_disp.cod_tramite
        GROUP BY 1

        IF (STATUS = NOTFOUND) THEN

            EXECUTE eje_marca_pen USING lr_marca_disp.*
                                  INTO  ls_resp

            LET li_cont = li_cont + 1
            DISPLAY "MARCA DISPOSICIONES IMSS           : ", li_cont AT 7,6

        END IF

        INITIALIZE lr_marca_disp.* TO NULL

    END FOREACH

    -- Disposiciones que se marcan al momento de la liquidacion
    DECLARE cur_disp_liquida CURSOR FOR
        SELECT A.nss            ,
               A.curp           ,
               A.folio          ,
               A.consecutivo    ,
               A.tipo_retiro    ,
               2                , -- disposicion_imss
               B.fecha_fin
        FROM   ret_solicitud_tx A   ,
               cta_his_marca B      ,
               tab_retiro C
        WHERE  A.nss                = B.nss
        AND    A.consecutivo        = B.correlativo
        AND    A.tipo_retiro       IN ("F","G","H","M","D","E","J")
        AND    A.estado_solicitud   = gr_edo.liquidado
        AND    A.tipo_retiro        = C.tipo_retiro
        AND    C.movimiento         = B.marca_cod
        AND    B.fecha_fin          > ldt_lim_fecha
        ORDER BY B.fecha_fin    ,
                 A.tipo_retiro  ,
                 A.nss

    FOREACH cur_disp_liquida INTO lr_marca_disp.*

        SELECT "OK"
        FROM   ret_marca_pensionado
        WHERE  nss          = lr_marca_disp.nss
        AND    folio        = lr_marca_disp.folio
        AND    consecutivo  = lr_marca_disp.consecutivo
        AND    cod_tramite  = lr_marca_disp.cod_tramite
        GROUP BY 1

        IF STATUS = NOTFOUND THEN

            EXECUTE eje_marca_pen USING lr_marca_disp.*
                                  INTO  ls_resp

            LET li_cont = li_cont + 1
            DISPLAY "MARCA DISPOSICIONES IMSS           : ", li_cont AT 7,6

        END IF

        INITIALIZE lr_marca_disp.* TO NULL

    END FOREACH


END FUNCTION

#---------------------------------------------------------------------------#
# f_historico_parciales : Realiza el barrido de las solicitudes de retiros  #
#                         parciales imsss para insertar las marcas de       #
#                         pensionados historicas                            #
#---------------------------------------------------------------------------#
FUNCTION f_historico_parciales()

    DEFINE lr_marca_par RECORD
        nss             LIKE ret_marca_pensionado.nss           ,
        curp            LIKE ret_marca_pensionado.curp          ,
        folio           LIKE ret_marca_pensionado.folio         ,
        consecutivo     LIKE ret_marca_pensionado.consecutivo   ,
        tipo_retiro     LIKE ret_marca_pensionado.tipo_retiro   ,
        cod_tramite     LIKE ret_marca_pensionado.cod_tramite   ,
        fecha_marca     LIKE ret_marca_pensionado.fecha_marca
    END RECORD

    DEFINE
        ldt_lim_fecha           DATE

    DEFINE
        li_cont                 INTEGER

    DEFINE
        ls_resp                 SMALLINT

    -- -----------------------------------------------------------------------------
    
    LET li_cont     = 0

    DISPLAY "MARCA RETIROS PARCIALES            : ", li_cont AT 8,6

    -- Obtenemos la ultima fecha registrada en la tabla
    LET ldt_lim_fecha = f_ultima_ejecucion(gr_tipo_tramite.parcial_imss)

    -- Disposiciones que se marcan al momento de la liquidacion
    DECLARE cur_par_liquida CURSOR FOR
        SELECT A.nss            ,
               A.curp           ,
               A.folio          ,
               A.consecutivo    ,
               A.tipo_retiro    ,
               4                , -- parcial_imss
               B.fecha_fin
        FROM   ret_parcial A        ,
               cta_his_marca B
        WHERE  A.nss                = B.nss
        AND    A.consecutivo        = B.correlativo
        AND    A.estado_solicitud   = gr_edo.liquidado
        AND    B.marca_cod          IN (870, 875)
        AND    B.fecha_fin          > ldt_lim_fecha
        ORDER BY B.fecha_fin    ,
                 A.tipo_retiro  ,
                 A.nss

    FOREACH cur_par_liquida INTO lr_marca_par.*

        SELECT "OK"
        FROM   ret_marca_pensionado
        WHERE  nss          = lr_marca_par.nss
        AND    folio        = lr_marca_par.folio
        AND    consecutivo  = lr_marca_par.consecutivo
        AND    cod_tramite  = lr_marca_par.cod_tramite
        GROUP BY 1

        IF STATUS = NOTFOUND THEN

            EXECUTE eje_marca_pen USING lr_marca_par.*
                                  INTO  ls_resp

            LET li_cont = li_cont + 1
            DISPLAY "MARCA RETIROS PARCIALES            : ", li_cont AT 8,6

        END IF

    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# f_historico_transfer_issste : Realiza el barrido de las solicitudes de    #
#                               transferencias ISSSTE para insertar las     #
#                               marcas de pensionados historicas            #
#---------------------------------------------------------------------------#
FUNCTION f_historico_transfer_issste()

    DEFINE lr_marca_transf_issste RECORD
        nss             LIKE ret_marca_pensionado.nss           ,
        curp            LIKE ret_marca_pensionado.curp          ,
        folio           LIKE ret_marca_pensionado.folio         ,
        consecutivo     LIKE ret_marca_pensionado.consecutivo   ,
        tipo_retiro     LIKE ret_marca_pensionado.tipo_retiro   ,
        cod_tramite     LIKE ret_marca_pensionado.cod_tramite   ,
        fecha_marca     LIKE ret_marca_pensionado.fecha_marca
    END RECORD

    DEFINE
        ldt_lim_fecha           DATE

    DEFINE
        li_cont                 INTEGER

    DEFINE
        ls_resp                 SMALLINT

    -- -----------------------------------------------------------------------------
    
    LET li_cont     = 0
    INITIALIZE lr_marca_transf_issste.* TO NULL

    DISPLAY "MARCA TRANSFERENCIAS ISSSTE        : ", li_cont AT 10,6

    -- Obtenemos la ultima fecha registrada en la tabla
    LET ldt_lim_fecha = f_ultima_ejecucion(gr_tipo_tramite.trasferencia_issste)

    DECLARE cur_transf_issste CURSOR FOR
        SELECT A.nss            ,
               A.curp           ,
               A.folio          ,
               A.consecutivo    ,
               A.tipo_retiro    ,
               10               , -- trasferencia_issste
               B.fecha_ini
        FROM   ret_trans_issste A  ,
               cta_his_marca B  ,
               tab_ret_issste C
        WHERE  A.nss            = B.nss
        AND    A.consecutivo    = B.correlativo
        AND    A.tipo_retiro    = C.tipo_retiro
        AND    C.movimiento     = B.marca_cod
        AND    B.fecha_ini      > ldt_lim_fecha
        ORDER BY B.fecha_ini    ,
                 A.tipo_retiro  ,
                 A.nss

    FOREACH cur_transf_issste INTO lr_marca_transf_issste.*

        IF (lr_marca_transf_issste.folio IS NULL) THEN
            LET lr_marca_transf_issste.folio = 0
        END IF

        SELECT "OK"
        FROM   ret_marca_pensionado
        WHERE  nss          = lr_marca_transf_issste.nss
        AND    folio        = lr_marca_transf_issste.folio
        AND    consecutivo  = lr_marca_transf_issste.consecutivo
        AND    cod_tramite  = lr_marca_transf_issste.cod_tramite
        GROUP BY 1

        IF STATUS = NOTFOUND THEN

            EXECUTE eje_marca_pen USING lr_marca_transf_issste.*
                                  INTO  ls_resp

            LET li_cont = li_cont + 1
            DISPLAY "MARCA TRANSFERENCIAS ISSSTE        : ", li_cont AT 10,6

        END IF

        INITIALIZE lr_marca_transf_issste.* TO NULL

    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# f_historico_disp_issste : Realiza el barrido de las solicitudes de        #
#                           disposiciones issste para insertar las marcas   #
#                           de pensionados historicas                       #
#---------------------------------------------------------------------------#
FUNCTION f_historico_disp_issste()

    DEFINE lr_marca_disp_issste RECORD
        nss             LIKE ret_marca_pensionado.nss           ,
        curp            LIKE ret_marca_pensionado.curp          ,
        folio           LIKE ret_marca_pensionado.folio         ,
        consecutivo     LIKE ret_marca_pensionado.consecutivo   ,
        tipo_retiro     LIKE ret_marca_pensionado.tipo_retiro   ,
        cod_tramite     LIKE ret_marca_pensionado.cod_tramite   ,
        fecha_marca     LIKE ret_marca_pensionado.fecha_marca
    END RECORD

    DEFINE
        ldt_lim_fecha           DATE

    DEFINE
        li_cont                 INTEGER

    DEFINE
        ls_resp                 SMALLINT

    -- -----------------------------------------------------------------------------
    LET li_cont     = 0
    INITIALIZE lr_marca_disp_issste.* TO NULL

    DISPLAY "MARCA DISPOSICIONES ISSSTE         : ", li_cont AT 11,6

    -- Obtenemos la ultima fecha registrada en la tabla
    LET ldt_lim_fecha = f_ultima_ejecucion(gr_tipo_tramite.disposicion_issste)

    -- Disposiciones que se marcan en la carga de datamart
    DECLARE cur_disp_iss_dtm CURSOR FOR
        SELECT A.nss            ,
               A.curp           ,
               A.folio          ,
               A.consecutivo    ,
               A.tipo_retiro    ,
               9                , -- disposicion_issste
               C.fecha_carga
        FROM   ret_sol_issste_tx A      ,
               ret_datamart_issste B    ,
               ret_cza_datamart C  
        WHERE  A.curp           = B.curp
        AND    A.sec_pension    = B.sec_pension
        AND    A.tipo_retiro    IN ("A","B","G","K")
        AND    B.folio          = C.folio
        AND    C.fecha_carga    > ldt_lim_fecha
        ORDER BY C.fecha_carga  ,
                 A.tipo_retiro  ,
                 A.nss

    FOREACH cur_disp_iss_dtm INTO lr_marca_disp_issste.*

        IF lr_marca_disp_issste.folio IS NULL THEN
            LET lr_marca_disp_issste.folio = 0
        END IF

        SELECT "OK"
        FROM   ret_marca_pensionado
        WHERE  nss          = lr_marca_disp_issste.nss
        AND    folio        = lr_marca_disp_issste.folio
        AND    consecutivo  = lr_marca_disp_issste.consecutivo
        AND    cod_tramite  = lr_marca_disp_issste.cod_tramite
        GROUP BY 1

        IF STATUS = NOTFOUND THEN

            EXECUTE eje_marca_pen USING lr_marca_disp_issste.*
                                  INTO  ls_resp

            LET li_cont = li_cont + 1
            DISPLAY "MARCA DISPOSICIONES ISSSTE         : ", li_cont AT 11,6

        END IF

        INITIALIZE lr_marca_disp_issste.* TO NULL

    END FOREACH

    -- Disposiciones que se marcan al momento de la liquidacion
    DECLARE cur_disp_isss_liquida CURSOR FOR
        SELECT A.nss            ,
               A.curp           ,
               A.folio          ,
               A.consecutivo    ,
               A.tipo_retiro    ,
               9                , -- disposicion_issste
               B.fecha_fin
        FROM   ret_sol_issste_tx A  ,
               cta_his_marca B      ,
               tab_ret_issste C
        WHERE  A.nss                = B.nss
        AND    A.consecutivo        = B.correlativo
        AND    A.tipo_retiro       IN ("C","D","E","A","B","G","K")
        AND    A.estado_solicitud   = gr_edo_iss
        AND    A.tipo_retiro        = C.tipo_retiro
        AND    C.movimiento         = B.marca_cod
        AND    B.fecha_fin          > ldt_lim_fecha
        ORDER BY B.fecha_fin    ,
                 A.tipo_retiro  ,
                 A.nss

    FOREACH cur_disp_isss_liquida INTO lr_marca_disp_issste.*

        SELECT "OK"
        FROM   ret_marca_pensionado
        WHERE  nss          = lr_marca_disp_issste.nss
        AND    folio        = lr_marca_disp_issste.folio
        AND    consecutivo  = lr_marca_disp_issste.consecutivo
        AND    cod_tramite  = lr_marca_disp_issste.cod_tramite
        GROUP BY 1

        IF STATUS = NOTFOUND THEN

            EXECUTE eje_marca_pen USING lr_marca_disp_issste.*
                                  INTO  ls_resp

            LET li_cont = li_cont + 1
            DISPLAY "MARCA DISPOSICIONES ISSSTE         : ", li_cont AT 11,6

        END IF

        INITIALIZE lr_marca_disp_issste.* TO NULL

    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# f_historico_parcial_issste : Realiza el barrido de las solicitudes de     #
#                              retiros parciales issste para insertar las   #
#                              marcas de pensionados historicas             #
#---------------------------------------------------------------------------#
FUNCTION f_historico_parcial_issste()

    DEFINE lr_marca_par_issste RECORD
        nss             LIKE ret_marca_pensionado.nss           ,
        curp            LIKE ret_marca_pensionado.curp          ,
        folio           LIKE ret_marca_pensionado.folio         ,
        consecutivo     LIKE ret_marca_pensionado.consecutivo   ,
        tipo_retiro     LIKE ret_marca_pensionado.tipo_retiro   ,
        cod_tramite     LIKE ret_marca_pensionado.cod_tramite   ,
        fecha_marca     LIKE ret_marca_pensionado.fecha_marca
    END RECORD

    DEFINE
        ldt_lim_fecha           DATE

    DEFINE
        li_cont                 INTEGER

    DEFINE
        ls_resp                 SMALLINT

    -- -----------------------------------------------------------------------------

    LET li_cont     = 0
    INITIALIZE lr_marca_par_issste.* TO NULL

    DISPLAY "MARCA RETIROS PARCIALES ISSSTE     : ", li_cont AT 12,6

    -- Obtenemos la ultima fecha registrada en la tabla
    LET ldt_lim_fecha = f_ultima_ejecucion(gr_tipo_tramite.parcial_issste)

    -- Disposiciones que se marcan al momento de la liquidacion
    DECLARE cur_par_iss_liquida CURSOR FOR
        SELECT A.nss            ,
               A.curp           ,
               A.folio          ,
               A.consecutivo    ,
               "F"              ,
               11               , -- parcial_issste
               B.fecha_fin
        FROM   ret_parcial_issste A ,
               cta_his_marca B      ,
               tab_ret_issste C
        WHERE  A.nss                = B.nss
        AND    A.consecutivo        = B.correlativo
        AND    A.estado_solicitud   = gr_edo_iss.liquidado
        AND    C.tipo_retiro        = "F"
        AND    C.movimiento         = B.marca_cod
        AND    B.fecha_fin          > ldt_lim_fecha
        ORDER BY B.fecha_fin    ,
                 A.nss

    FOREACH cur_par_iss_liquida INTO lr_marca_par_issste.*

        SELECT "OK"
        FROM   ret_marca_pensionado
        WHERE  nss          = lr_marca_par_issste.nss
        AND    folio        = lr_marca_par_issste.folio
        AND    consecutivo  = lr_marca_par_issste.consecutivo
        AND    cod_tramite  = lr_marca_par_issste.cod_tramite
        GROUP BY 1

        IF STATUS = NOTFOUND THEN

            EXECUTE eje_marca_pen USING lr_marca_par_issste.*
                                  INTO  ls_resp

            LET li_cont = li_cont + 1
            DISPLAY "MARCA RETIROS PARCIALES ISSSTE     : ", li_cont AT 12,6

        END IF

        INITIALIZE lr_marca_par_issste.* TO NULL

    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# f_ultima_ejecucion : Recupera la ultima fecha registrada en la tabla      #
#                      ret_marca_pensionado para el proceso indicado        #
#---------------------------------------------------------------------------#
FUNCTION f_ultima_ejecucion(ps_cod_tramite)

    DEFINE 
        ps_cod_tramite              SMALLINT

    DEFINE
        ldt_ultima_fecha            DATE

    -- -----------------------------------------------------------------------------

    -- Obtenemos la ultima fecha registrada en la tabla
    SELECT MAX(fecha_marca)
    INTO   ldt_ultima_fecha
    FROM   ret_marca_pensionado
    WHERE  cod_tramite = ps_cod_tramite
    
    IF (ldt_ultima_fecha IS NULL) THEN
        LET ldt_ultima_fecha = HOY
    END IF 
    
    RETURN ldt_ultima_fecha
    
END FUNCTION 

#---------------------------------------------------------------------------#
# f_abre_ventana : Abre la ventana donde se captura los datos para armar    #
#                  el query de busqueda                                     #
#---------------------------------------------------------------------------#
FUNCTION f_abre_ventana()

    OPEN WINDOW win_despliega AT 4,4 WITH 20 ROWS, 75 COLUMNS
        ATTRIBUTE(BORDER, PROMPT LINE LAST -1)

    DISPLAY "                                                          <CTRL-C> - Salir   " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC557 BARRIDO HISTORICO - MARCA DE PENSIONADO Y RET PARCIAL               " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

END FUNCTION

