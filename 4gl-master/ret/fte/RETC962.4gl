#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETC962  => GENERACION DE LA OPERACION 43 DE TRANFERENCIAS ISSSTE     #
#Fecha creacion    => 22 DE OCTUBRE DE 2009                                     #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   => 21 DE JUNIO DE 2010                                       #
#                  => Se modifica para permitir la carga del campo de clave de  #
#                     aseguradora                                               #
#Sistema           => RET                                                       #
#################################################################################
DATABASE safre_af

GLOBALS

    DEFINE g_seg_modulo RECORD LIKE seg_modulo.*


    DEFINE gr_edo RECORD
        provisionado          LIKE ret_estado_issste.estado_solicitud ,
        enviado               LIKE ret_estado_issste.estado_solicitud ,
        rechazado             LIKE ret_estado_issste.estado_solicitud
    END RECORD

    DEFINE #glo #date
        gdt_fecha_proc        ,
        HOY                   DATE

    DEFINE
        gdt_fec_val_trans     ,
        gdt_fec_oper          DATE


    DEFINE
        c12_nom_plano         CHAR(012) ,
        G_LISTA_DET           CHAR(100) ,
        G_LISTA_CZA           CHAR(100) ,
        G_LISTA_SUM           CHAR(100) ,
        enter                 CHAR(001) ,
        gc_usuario            CHAR(015)

    DEFINE #glo #smallint
        gs_tipo_op            ,
        gs_procesa            ,
        gs_codigo_afore       SMALLINT

    DEFINE
        gi_num_regs           ,
        gs_ult_folio          INTEGER

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("RETC962.log")

    CALL init() #i
    
    IF gdt_fecha_proc IS NULL THEN 
        CALL f_captura_folio() RETURNING gs_procesa    ,
                                         gs_ult_folio  ,
                                         gdt_fecha_proc
    ELSE
        LET gs_procesa = 1
    END IF
    
    IF gs_procesa THEN
        CALL f_obten_fechas(gs_ult_folio)
            RETURNING gdt_fec_val_trans, gdt_fec_oper

        CALL f_abre_ventana()
        LET  c12_nom_plano = gdt_fec_oper USING "YYYYMMDD",".43T"

        CALL primer_paso(gs_ult_folio)      #-- Genera encabezado transaccional

        CALL segundo_paso(gs_ult_folio)     #-- Genera el detalle del reporte
            RETURNING gi_num_regs

        CALL tercer_paso(gi_num_regs)       #-- Genera el sumario del reporte

        CALL cuarto_paso()                  #-- Concatena los archivos

        CALL quinto_paso(gs_ult_folio)      #-- Actualiza tablas
    END IF

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    DEFINE
        lc_prepare      CHAR(300)

    LET HOY             = TODAY
    LET gs_tipo_op      = 43

    SELECT *
    INTO   g_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    ----- PARAMETROS DE ENTRADA -----
    LET gdt_fecha_proc  = ARG_VAL(1)

    ----- CODIGOS AFORES -----
    SELECT codigo_afore,
           USER
    INTO   gs_codigo_afore,
           gc_usuario
    FROM   tab_afore_local

    ----- ESTADOS DE S0LICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.enviado
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "ENVIADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.rechazado
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "RECHAZADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.provisionado
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "PROVISIONADO"

    ----- FOLIO MAXIMO -----
    SELECT MAX(folio)
    INTO   gs_ult_folio
    FROM   ret_trans_issste
    WHERE  estado_solicitud = gr_edo.provisionado

    IF (gdt_fecha_proc IS NOT NULL AND gs_ult_folio IS NULL) THEN 
        PROMPT " NO EXISTEN REGISTROS PARA GENERAR EL ARCHIVO ...<ENTER> PARA SALIR " FOR CHAR enter
        EXIT PROGRAM
    END IF

END FUNCTION


#---------------------------------------------------------------------------#
# f_captura_folio : Captura el folio que se usara en la generacion de la    #
#                   operacion 43 de transferencias                          #
#---------------------------------------------------------------------------#
FUNCTION f_captura_folio()

    DEFINE lr_cap RECORD
        folio_oper_42   LIKE ret_trans_issste.folio ,
        fecha_envio     DATE
    END RECORD

    DEFINE li_folio_trans  LIKE ret_trans_issste.folio

    DEFINE
        li_tot_prov        INTEGER

    DEFINE
        ls_ret              ,
        ls_cont             ,
        ls_flag             SMALLINT

    DEFINE
        lc_query            CHAR(1000)


    LET ls_flag = 1
    LET lr_cap.folio_oper_42 = gs_ult_folio
    LET lr_cap.fecha_envio   = HOY


    OPEN WINDOW retc9621 AT 4,4 WITH FORM "RETC9621" ATTRIBUTE (BORDER)
    DISPLAY " <CTRL-C> Salir                                             <ESC> Ejecutar  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC962   GENERACION DE LA OPERACION 43 TRANSFERENCIAS ISSSTE              " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

    INPUT BY NAME lr_cap.* WITHOUT DEFAULTS

        BEFORE FIELD folio_oper_42

            DISPLAY BY NAME lr_cap.*

            IF lr_cap.folio_oper_42 IS NULL OR lr_cap.folio_oper_42 <= 0 THEN
                PROMPT " NO EXISTEN REGISTROS PARA GENERAR EL ARCHIVO ...<ENTER> PARA SALIR " FOR CHAR enter
                LET ls_flag = 0
                EXIT INPUT
            END IF

        AFTER FIELD folio_oper_42
            IF lr_cap.folio_oper_42 IS NULL OR lr_cap.folio_oper_42 <= 0 THEN
                ERROR "  CAMPO NO PUEDE SER NULO " ATTRIBUTE(NORMAL)
                NEXT FIELD folio_oper_42
            END IF

            SELECT "OK"
            FROM   ret_trans_issste
            WHERE  folio = lr_cap.folio_oper_42
            GROUP BY 1

            IF SQLCA.SQLCODE = NOTFOUND  THEN
                ERROR " EL FOLIO INGRESADO NO EXISTE O NO CORRESPONDE A UNA TRANSFERENCIA "
                SLEEP 2
                ERROR ""
                NEXT FIELD folio_oper_42
            END IF

            SELECT "OK"
            FROM   ret_ctr_envio
            WHERE  folio = lr_cap.folio_oper_42
            GROUP BY 1

            IF SQLCA.SQLCODE <> NOTFOUND  THEN
                ERROR " YA FUE GENERADO UN ARCHIVO PARA ESTE FOLIO "
                SLEEP 2
                ERROR ""
                NEXT FIELD folio_oper_42
            END IF

        AFTER FIELD fecha_envio

            IF lr_cap.fecha_envio IS NULL OR lr_cap.fecha_envio < TODAY THEN
                ERROR " FECHA INCORRECTA ... "
                NEXT FIELD fecha_envio
            END IF


        ON KEY (ESC)
            IF lr_cap.folio_oper_42 IS NULL OR lr_cap.folio_oper_42 <= 0 THEN
                ERROR " CAMPO NO PUEDE SER NULO " ATTRIBUTE(NORMAL)
                NEXT FIELD folio_oper_42
            END IF

            SELECT "OK"
            FROM   ret_trans_issste
            WHERE  folio = lr_cap.folio_oper_42
            GROUP BY 1

            IF SQLCA.SQLCODE = NOTFOUND  THEN
                ERROR " EL FOLIO INGRESADO NO EXISTE O NO CORRESPONDE A UNA TRANSFERENCIA "
                SLEEP 2
                ERROR ""
                NEXT FIELD folio_oper_42
            END IF

            SELECT "OK"
            FROM   ret_ctr_envio
            WHERE  folio = lr_cap.folio_oper_42
            GROUP BY 1

            IF SQLCA.SQLCODE <> NOTFOUND  THEN
                ERROR " YA FUE GENERADO UN ARCHIVO PARA ESTE FOLIO "
                SLEEP 2
                ERROR ""
                NEXT FIELD folio_oper_42
            END IF

            IF lr_cap.fecha_envio IS NULL OR lr_cap.fecha_envio < TODAY THEN
                ERROR " FECHA INCORRECTA ... "
                NEXT FIELD fecha_envio
            END IF

            WHILE TRUE
                PROMPT " ESTA SEGURO S/N ? " FOR CHAR enter
                IF enter MATCHES "[sSnN]" THEN
                    IF enter MATCHES "[sS]" THEN
                        LET ls_flag = 1
                    ELSE
                        PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
                        LET ls_flag = 0
                    END IF
                    EXIT WHILE
                END IF
            END WHILE

            EXIT INPUT

        ON KEY (CONTROL-C,INTERRUPT)
            PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
            LET ls_flag = 0
            EXIT INPUT

    END INPUT

    CLOSE WINDOW retc9621

    RETURN ls_flag, lr_cap.*

END FUNCTION

#---------------------------------------------------------------------------#
# primer_paso : Genera el encabezado transaccional del archivo de la op 43  #
#---------------------------------------------------------------------------#
FUNCTION primer_paso(pi_folio)

    DEFINE pi_folio LIKE ret_trans_issste.folio

    LET G_LISTA_CZA = g_seg_modulo.ruta_envio CLIPPED, "/CZA_ISS"

    START REPORT cza_tran TO G_LISTA_CZA
        OUTPUT TO REPORT cza_tran(pi_folio)
    FINISH REPORT cza_tran

END FUNCTION

#---------------------------------------------------------------------------#
# segundo_paso : Genera el detalle del archivo de la op 43                  #
#---------------------------------------------------------------------------#
FUNCTION segundo_paso(pi_folio)

    DEFINE pi_folio LIKE ret_trans_issste.folio

    DEFINE #loc #r_transf_rx
        lr_transf_issste        RECORD LIKE ret_trans_issste.*      ,
        lr_monto_issste         RECORD LIKE ret_monto_issste.*      ,
        lr_monto_viv            RECORD LIKE ret_monto_viv_issste.*

    DEFINE
        ruta_det_nss      ,
        ruta_det_sie      ,
        ruta_det_viv      CHAR(100)

    DEFINE #loc #char
        hora              CHAR(5)

    DEFINE
        ls_diag           SMALLINT
    
    DEFINE
        li_num_regs       INTEGER

    DISPLAY "GENERANDO ARCHIVO PLANO ..." AT 18,5 ATTRIBUTE(REVERSE)
    
    LET li_num_regs = 0
    LET ls_diag     = 0

    #-- Determinamos la ubicacion de los reportes
    LET ruta_det_nss = g_seg_modulo.ruta_envio CLIPPED, "/", "DET-NSS-TR-tmp"
    LET ruta_det_nss = ruta_det_nss CLIPPED

    LET ruta_det_sie = g_seg_modulo.ruta_envio CLIPPED, "/", "DET-SIE-TR-tmp"
    LET ruta_det_sie = ruta_det_sie CLIPPED

    LET ruta_det_viv = g_seg_modulo.ruta_envio CLIPPED, "/", "DET-VIV-TR-tmp"
    LET ruta_det_viv = ruta_det_viv CLIPPED

    LET G_LISTA_DET = g_seg_modulo.ruta_envio CLIPPED, "/", "DET_ISS_TR"
    LET G_LISTA_DET = G_LISTA_DET CLIPPED

    --SELECCION DE LOS REGISTROS A REPORTAR
    DECLARE cur_det CURSOR FOR
    SELECT A.*
    FROM   ret_trans_issste  A
    WHERE  A.folio        = pi_folio
    ORDER  BY A.tipo_retiro

    FOREACH cur_det INTO lr_transf_issste.*

        INITIALIZE lr_monto_issste.* TO NULL
        INITIALIZE lr_monto_viv.* TO NULL

        #-- Iniciamos los reportes
        START REPORT det_transferencia TO ruta_det_nss
        START REPORT det_siefores TO ruta_det_sie
        START REPORT det_vivienda TO ruta_det_viv

        LET li_num_regs = li_num_regs + 1

        SELECT B.*
        INTO   lr_monto_issste.*
        FROM   ret_monto_issste B
        WHERE  B.folio        = pi_folio
        AND    B.curp         = lr_transf_issste.curp
        AND    B.consecutivo  = lr_transf_issste.consecutivo

        IF STATUS = NOTFOUND THEN
            LET ls_diag = 0
        ELSE
            LET ls_diag = lr_transf_issste.diag_procesar
        END IF 

        SELECT C.*
        INTO   lr_monto_viv.*
        FROM   ret_monto_viv_issste  C
        WHERE  C.folio        = pi_folio
        AND    C.curp         = lr_transf_issste.curp
        AND    C.consecutivo  = lr_transf_issste.consecutivo

        OUTPUT TO REPORT det_transferencia(lr_transf_issste.*)
        OUTPUT TO REPORT det_siefores(lr_transf_issste.*, lr_monto_issste.*)
        OUTPUT TO REPORT det_vivienda(lr_transf_issste.*, lr_transf_issste.diag_procesar, lr_monto_viv.*)

        FINISH REPORT det_transferencia
        FINISH REPORT det_siefores
        FINISH REPORT det_vivienda

        #-- Una vez generados los archivos para el nss actual, los concatenamos en uno solo
        CALL concat_reportes(ruta_det_nss   ,
                             ruta_det_sie   ,
                             ruta_det_viv   ,
                             li_num_regs    ,
                             ls_diag        )

    END FOREACH

    RETURN li_num_regs

END FUNCTION

#---------------------------------------------------------------------------#
# tercer_paso : Genera el sumario del archivo de la op 43                   #
#---------------------------------------------------------------------------#
FUNCTION tercer_paso(pi_num_regs)

    DEFINE
        pi_num_regs     INTEGER

    LET G_LISTA_SUM = g_seg_modulo.ruta_envio CLIPPED,"/SUMTRISS"

    START REPORT sum_tran TO G_LISTA_SUM
        OUTPUT TO REPORT sum_tran(pi_num_regs) #st
    FINISH REPORT sum_tran

END FUNCTION

#---------------------------------------------------------------------------#
# cuarto_paso : Concatena los archivos generados para formar el lote        #
#---------------------------------------------------------------------------#
FUNCTION cuarto_paso()

    DEFINE
        comando     ,
        cat         CHAR(500)

    LET cat = "cat ", G_LISTA_CZA ,
                      G_LISTA_DET ,
                      G_LISTA_SUM ,
                     "> ",g_seg_modulo.ruta_envio CLIPPED,"/",c12_nom_plano

    RUN cat

    WHENEVER ERROR CONTINUE

    LET comando = "chmod 777 ", G_LISTA_CZA
    RUN comando

    LET comando = "chmod 777 ", G_LISTA_DET
    RUN comando

    LET comando = "chmod 777 ", G_LISTA_SUM
    RUN comando

    LET comando = "chmod 777 ", g_seg_modulo.ruta_envio CLIPPED,"/",c12_nom_plano
    RUN comando

    WHENEVER ERROR STOP

END FUNCTION

#---------------------------------------------------------------------------#
# quinto_paso : Actualiza las tablas de envio y muestra resultados en       #
#               la pantalla                                                 #
#---------------------------------------------------------------------------#
FUNCTION quinto_paso(pi_folio)

    DEFINE
        pi_folio        INTEGER

    UPDATE ret_trans_issste
    SET    estado_solicitud = gr_edo.enviado
    WHERE  folio            = pi_folio
    AND    estado_solicitud = gr_edo.provisionado

    INSERT INTO ret_ctr_envio
    VALUES ( pi_folio       ,
             "TRAN_ISSSTE"  ,
             "CZA_TRAN_ISSS DET_TRAN_ISSS SUM_TRAN_ISSS",
             gr_edo.enviado ,
             1              ,
             gdt_fecha_proc
            )

    DISPLAY "                           " AT 18,5
    DISPLAY " EL LOTE HA SIDO GENERADO EN LA RUTA : " AT 8,19
    DISPLAY g_seg_modulo.ruta_envio CLIPPED AT 10,20
    DISPLAY "                                                " AT 12,11
    DISPLAY " CON EL NOMBRE : ",c12_nom_plano AT 12,19

    PROMPT " PRESIONE <ENTER> PARA FINALIZAR " FOR CHAR enter

    CLOSE WINDOW retc9622

END FUNCTION

#---------------------------------------------------------------------------#
# f_abre_ventana : Abre la ventana donde se muestran los resultados de la   #
#                  generacion de la op. 43 de transferencias                #
#---------------------------------------------------------------------------#
FUNCTION f_abre_ventana()

    OPEN WINDOW retc9622 AT 4,4 WITH FORM "RETC9622" ATTRIBUTE(BORDER)
    DISPLAY " <CTRL-C> Salir                                             <ESC> Ejecutar  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC962   GENERACION DE LA OPERACION 43 TRANSFERENCIAS ISSSTE              " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

END FUNCTION

#---------------------------------------------------------------------------#
# f_obten_fechas : Obtiene las fechas de transferencia y de operacion para  #
#                  la generacion de la op. 43 de transferencias             #
#---------------------------------------------------------------------------#
FUNCTION f_obten_fechas(pi_folio)

    DEFINE
        pi_folio        INTEGER

    DEFINE lr_fechas RECORD
        val_trans       DATE,
        operacion       DATE
    END RECORD


    LET lr_fechas.val_trans   = NULL
    LET lr_fechas.operacion   = NULL

    SELECT fecha_valor_trans ,
           fecha_operacion
    INTO   lr_fechas.*
    FROM   ret_cza_lote
    WHERE  folio = pi_folio

    RETURN lr_fechas.*

END FUNCTION

#---------------------------------------------------------------------------#
# concat_reportes : Dadas las rutas de los tres archivos de detalle         #
#                   temporales de los reportes, los va concatenando en uno  #
#                   solo que sera el archivo de detalle final               #
#---------------------------------------------------------------------------#
FUNCTION concat_reportes(lc_det_3, lc_det_4, lc_det_5, p_regs, ps_diag)

    DEFINE
        lc_det_3      ,
        lc_det_4      ,
        lc_det_5      CHAR(100)

    DEFINE
        ps_diag         ,
        p_regs          SMALLINT

    #--
    DEFINE
        ruta_tmp        ,
        ruta_det        CHAR(100)

    DEFINE
        com_cat         ,
        com_rm          CHAR(500)

    #---

    LET ruta_tmp    = g_seg_modulo.ruta_envio CLIPPED, "/", "rep_temporal.tmp"
    LET ruta_tmp    = ruta_tmp CLIPPED

    LET ruta_det    = g_seg_modulo.ruta_envio CLIPPED, "/", "detalle.tmp"
    LET ruta_det    = ruta_det CLIPPED

    #-- Preparamos el comando para borrar los reportes
    LET com_rm = "rm ", lc_det_3 CLIPPED, " ",
                        lc_det_4 CLIPPED, " ",
                        lc_det_5 CLIPPED
    LET com_rm = com_rm CLIPPED

    IF (ps_diag = 501 OR ps_diag = 507) THEN
        LET com_cat = "cat ", lc_det_3 CLIPPED, " ",
                              lc_det_4 CLIPPED, " ",
                              lc_det_5 CLIPPED, " > ", ruta_tmp
        LET com_cat = com_cat CLIPPED
    ELSE
        LET com_cat = "cat ", lc_det_3 CLIPPED, " ",
                              lc_det_5 CLIPPED, " > ", ruta_tmp
        LET com_cat = com_cat CLIPPED
    END IF

    #-- Concatenamos los archivos en uno solo y borramos los temporales
    RUN com_cat
    RUN com_rm

    #-- Acumulamos el archivo generado al reporte final
    IF p_regs > 1 THEN
        LET com_cat = "cat ", G_LISTA_DET, " ", ruta_tmp, " > ", ruta_det
        RUN com_cat

        LET com_cat = " mv ", ruta_det, " ", G_LISTA_DET
        RUN com_cat
    ELSE
        #-- Si es el primer registro procesado entonces creamos el archivo de detalle
        LET com_cat = " cp ", ruta_tmp, " ", G_LISTA_DET
        RUN com_cat
    END IF

    #-- Borramos el temporal generado para quedarnos solo con el archivo de detalle
    LET com_rm = "rm ", ruta_tmp
    RUN com_rm

END FUNCTION

#---------------------------------------------------------------------------#
# cza_tran : Reporte que genera el encabezado del archivo de la op. 43      #
#---------------------------------------------------------------------------#
REPORT cza_tran(pi_folio)

    DEFINE
        pi_folio              INTEGER

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
            PRINT
                COLUMN 001, "01"                              ,-- Tipo registro
                COLUMN 003, "04"                              ,-- Id de servicio
                COLUMN 005, "01"                              ,-- Entidad origen
                COLUMN 007, gs_codigo_afore USING"&&&"        ,-- Cve entidad origen
                COLUMN 010, "03"                              ,-- Entidad destino
                COLUMN 012, "001"                             ,-- Cve entidad destino
                COLUMN 015, gdt_fec_oper   USING"YYYYMMDD"    ,-- Fecha operacion
                COLUMN 023, gdt_fec_val_trans USING"YYYYMMDD" ,-- Fecha valor trans
                COLUMN 031, 2 SPACES                          ,-- Resultado de la operacion
                COLUMN 033, 3 SPACES                          ,-- Motivo Rechazo 1
                COLUMN 036, 3 SPACES                          ,-- Motivo Rechazo 2
                COLUMN 039, 3 SPACES                          ,-- Motivo Rechazo 3
                COLUMN 042, 379 SPACES                         -- Filler
END REPORT


#---------------------------------------------------------------------------#
# det_transferencia : Reporte que genera el detalle del archivo de la op. 43#
#---------------------------------------------------------------------------#
REPORT det_transferencia(pr_transf)

    DEFINE
        pr_transf           RECORD LIKE ret_trans_issste.*

    DEFINE
        c6_porcentaje_ret08   CHAR(006) ,
        c6_porcentaje_cv      CHAR(006) ,
        c6_porcentaje_ahsol   CHAR(006) ,
        c6_porcentaje_viv     CHAR(006) ,
        c16_mto_const         CHAR(016) ,

        c5_porcentaje_ret08   CHAR(005) ,
        c5_porcentaje_cv      CHAR(005) ,
        c5_porcentaje_ahsol   CHAR(005) ,
        c5_porcentaje_viv     CHAR(005) ,
        c15_mto_const         CHAR(015)


    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
            
            -- Formateamos Clave de pension
            IF pr_transf.cve_pension = "NA" THEN
                LET pr_transf.cve_pension = "   "
            END IF
            
            -- Formateamos Porcentajes 
            IF pr_transf.porcentaje_ret08 IS NULL THEN
                LET pr_transf.porcentaje_ret08 = 0
            END IF

            LET c6_porcentaje_ret08 = pr_transf.porcentaje_ret08 USING "&&&.&&"
            LET c5_porcentaje_ret08 = c6_porcentaje_ret08[01,03],
                                      c6_porcentaje_ret08[05,06]

            IF pr_transf.porcentaje_cv IS NULL THEN
                LET pr_transf.porcentaje_cv = 0
            END IF

            LET c6_porcentaje_cv    = pr_transf.porcentaje_cv USING "&&&.&&"
            LET c5_porcentaje_cv    = c6_porcentaje_cv[01,03],
                                      c6_porcentaje_cv[05,06]

            IF pr_transf.porcentaje_ahorro_sol IS NULL THEN
                LET pr_transf.porcentaje_ahorro_sol = 0
            END IF

            LET c6_porcentaje_ahsol    = pr_transf.porcentaje_ahorro_sol USING "&&&.&&"
            LET c5_porcentaje_ahsol    = c6_porcentaje_ahsol[01,03],
                                         c6_porcentaje_ahsol[05,06]

            IF pr_transf.porcentaje_viv08 IS NULL THEN
                LET pr_transf.porcentaje_viv08 = 0
            END IF

            LET c6_porcentaje_viv    = pr_transf.porcentaje_viv08 USING "&&&.&&"
            LET c5_porcentaje_viv    = c6_porcentaje_viv[01,03],
                                       c6_porcentaje_viv[05,06]

            -- Formateamos el monto constitutivo solicitado
            LET c16_mto_const = pr_transf.mto_solic_issste USING "&&&&&&&&&&&&&.&&"
            LET c15_mto_const = c16_mto_const[01,13],
                                c16_mto_const[15,16]

        PRINT
            COLUMN 001, "03"                                            , -- Tipo de registro
            COLUMN 003, "04"                                            , -- ID de servicio
            COLUMN 005, gs_tipo_op USING "&&"                           , -- ID de operacion
            COLUMN 007, pr_transf.num_issste                            , -- Numero issste
            COLUMN 015, pr_transf.curp                                  , -- CURP
            COLUMN 033, pr_transf.nombre_datamart                       , -- Nombre Datamart
            COLUMN 073, pr_transf.paterno_datamart                      , -- Paterno Datamart
            COLUMN 113, pr_transf.materno_datamart                      , -- Materno Datamart
            COLUMN 153, pr_transf.nombre_afore                          , -- Nombre
            COLUMN 193, pr_transf.paterno_afore                         , -- Ap Paterno
            COLUMN 233, pr_transf.materno_afore                         , -- Ap Materno
            COLUMN 273, pr_transf.nss                                   , -- NSS
            COLUMN 284, pr_transf.num_concesion                         , -- Num Concesion
            COLUMN 293, pr_transf.delegacion USING "&&&"                , -- Delegacion
            COLUMN 296, pr_transf.sec_pension USING "&&"                , -- Sec de Pension
            COLUMN 298, pr_transf.tipo_movimiento USING "&&&"           , -- Tipo Movimiento
            COLUMN 301, pr_transf.regimen                               , -- Regimen
            COLUMN 303, pr_transf.tipo_retiro                           , -- Tipo de Retiro
            COLUMN 304, pr_transf.tipo_seguro                           , -- Tipo de Seguro
            COLUMN 306, pr_transf.tipo_pension                          , -- Tipo de Pension
            COLUMN 308, pr_transf.cve_pension                           , -- Clave de Pension
            COLUMN 311, pr_transf.tipo_prestacion USING "&&"            , -- Tipo de Prestacion
            COLUMN 313, pr_transf.fecha_ini_pen USING "YYYYMMDD"        , -- Fecha Inicio pension
            COLUMN 321, pr_transf.fecha_resolucion   USING "YYYYMMDD"   , -- Fecha resolucion
            COLUMN 329, pr_transf.semanas_cotizadas  USING "&&&&"       , -- Sems cotizadas
            COLUMN 333, pr_transf.fecha_carga_datamart USING "YYYYMMDD" , -- Fecha de carga DATAMART
            COLUMN 341, pr_transf.diag_procesar USING "&&&"             , -- Diagnostico del registro
            COLUMN 344, "000000"                                        , -- Fecha periodo de pago reing
            COLUMN 350, c5_porcentaje_ret08                             , -- Porc. Retiro 08 a trans
            COLUMN 355, c5_porcentaje_cv                                , -- Porc. CV a trans
            COLUMN 360, c5_porcentaje_ahsol                             , -- Porc. Ahorro Sol a trans
            COLUMN 365, c5_porcentaje_viv                               , -- Porc. Viv 97 a trans
            COLUMN 370, c15_mto_const                                   , -- Monto constitutivo
            COLUMN 385, pr_transf.cve_aseguradora USING "&&&"           , -- Clave de aseguradora
            COLUMN 388, 23 SPACES                                       , -- Filler
            COLUMN 410, 2  SPACES                                       , -- Resultado de la oper
            COLUMN 412, 3  SPACES                                       , -- Mot Rechazo 1
            COLUMN 415, 3  SPACES                                       , -- Mot Rechazo 2
            COLUMN 418, 3  SPACES                                         -- Mot Rechazo 3
END REPORT

#---------------------------------------------------------------------------#
# det_siefores : Reporte que genera el det de siefore del archivo de la op43#
#---------------------------------------------------------------------------#
REPORT det_siefores(pr_transfer, pr_montos)

    DEFINE pr_transfer RECORD LIKE ret_trans_issste.*
    DEFINE pr_montos RECORD LIKE ret_monto_issste.*

    DEFINE
        c14_impt_ret_08     CHAR(14) ,
        c14_impt_ces_vej    CHAR(14) ,
        c14_impt_ah_sol     CHAR(14) ,
        c14_impt_comp_ret   CHAR(14)

    DEFINE
        c15_impt_ret_08     CHAR(15) ,
        c15_impt_ces_vej    CHAR(15) ,
        c15_impt_ah_sol     CHAR(15) ,
        c15_impt_comp_ret   CHAR(15)

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW

            #-- Obtenemos el valor de las Acciones de Retiro 08
            IF pr_montos.acc_ret08 IS NULL THEN
                LET pr_montos.acc_ret08 = 0
            END IF

            LET c15_impt_ret_08 = pr_montos.acc_ret08 USING "&&&&&&&&.&&&&&&"
            LET c14_impt_ret_08 = c15_impt_ret_08[01,08],
                                  c15_impt_ret_08[10,15]

            #-- Obtenemos el valor de las Acciones de CV
            IF pr_montos.acc_cv IS NULL THEN
                LET pr_montos.acc_cv = 0
            END IF

            LET c15_impt_ces_vej = pr_montos.acc_cv USING "&&&&&&&&.&&&&&&"
            LET c14_impt_ces_vej = c15_impt_ces_vej[01,08],
                                   c15_impt_ces_vej[10,15]

            #-- Obtenemos el valor de las Acciones de Ahorro Sol
            IF pr_montos.acc_ahorro_sol IS NULL THEN
                LET pr_montos.acc_ahorro_sol = 0
            END IF

            LET c15_impt_ah_sol = pr_montos.acc_ahorro_sol USING "&&&&&&&&.&&&&&&"
            LET c14_impt_ah_sol = c15_impt_ah_sol[01,08],
                                  c15_impt_ah_sol[10,15]

            #-- Obtenemos el valor de las Acciones de Complementarias de Retiro
            IF pr_montos.acc_comp_ret IS NULL THEN
                LET pr_montos.acc_comp_ret = 0
            END IF

            LET c15_impt_comp_ret = pr_montos.acc_comp_ret USING "&&&&&&&&.&&&&&&"
            LET c14_impt_comp_ret = c15_impt_comp_ret[01,08],
                                    c15_impt_comp_ret[10,15]

        PRINT
            COLUMN 001, "04"                                   , -- Tipo de registro
            COLUMN 003, pr_transfer.nss                        , -- NSS
            COLUMN 014, pr_transfer.curp                       , -- CURP
            COLUMN 032, pr_montos.siefore USING "&&"           , -- Clave de siefore
            COLUMN 034, c14_impt_ret_08                        , -- Acciones de retiro 97
            COLUMN 048, c14_impt_ces_vej                       , -- Acciones de CV
            COLUMN 062, c14_impt_ah_sol                        , -- Acciones de Ahorro Solidario
            COLUMN 076, c14_impt_comp_ret                      , -- Acciones de Ahorro Solidario
            COLUMN 090, 331 SPACES                               -- Filler

END REPORT

#----------------------------------------------------------------------------#
# det_vivienda : Reporte que genera el det de vivienda del archivo de la op43#
#----------------------------------------------------------------------------#
REPORT det_vivienda(pr_transf, ps_diag_procesar, pr_viv)

    DEFINE pr_transf RECORD LIKE ret_trans_issste.*
    DEFINE ps_diag_procesar LIKE ret_trans_issste.diag_procesar

    DEFINE pr_viv RECORD LIKE ret_monto_viv_issste.*

    DEFINE #loc #char
        c15_impt_viv_08       CHAR(15) ,
        c14_impt_viv_08       CHAR(14)

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW

            #-- Obtenemos Intereses de Vivienda 08
            IF pr_viv.acciones_viv08 IS NULL THEN
                LET pr_viv.acciones_viv08 = 0
            END IF

            LET c15_impt_viv_08 = pr_viv.acciones_viv08 USING "&&&&&&&&.&&&&&&"
            LET c14_impt_viv_08 = c15_impt_viv_08[01,08],
                                  c15_impt_viv_08[10,15]

            IF ps_diag_procesar <> 501 THEN
               LET pr_viv.fecha_valor_viv = "01/01/0001"
            END IF

        PRINT
            COLUMN 001, "05"                                         , -- Tipo de registro
            COLUMN 003, pr_transf.nss                                , -- NSS
            COLUMN 014, pr_transf.curp                               , -- CURP
            COLUMN 032, pr_viv.estado_sub_viv                        , -- Estatus subcuenta vivienda
            COLUMN 033, pr_viv.fecha_valor_viv USING "YYYYMMDD"      , -- Fecha Valor de la vivienda
            COLUMN 041, c14_impt_viv_08                              , -- Intereses Vivienda 97
            COLUMN 055, 366 SPACES                                     -- Filler

END REPORT

#---------------------------------------------------------------------------#
# sum_tran : Reporte que genera el sumario del archivo de la op43           #
#---------------------------------------------------------------------------#
REPORT sum_tran(pi_num_regs)

    DEFINE
        pi_num_regs     INTEGER

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
            PRINT
                COLUMN 001, "09"                            ,-- Tipo Registro
                COLUMN 003, "04"                            ,-- Id de servicio
                COLUMN 005, "01"                            ,-- Entidad Origen
                COLUMN 007, gs_codigo_afore USING "&&&"     ,-- Cve Entidad origen
                COLUMN 010, "03"                            ,-- Entidad destino
                COLUMN 012, "001"                           ,-- Cve Entidad destino
                COLUMN 015, gdt_fec_oper USING "YYYYMMDD"   ,-- Fecha operacion
                COLUMN 023, pi_num_regs USING"&&&&&&"       ,-- Total registros
                COLUMN 029, 392 SPACES                       -- Filler
END REPORT