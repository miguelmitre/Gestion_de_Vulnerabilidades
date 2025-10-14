#################################################################################
#Proyecto          => SISTEMA DE AFORE( SAFRE )                                 #
#Owner             => E.F.P.                                                    #
#Programa RETM821  => GENERA Y RECIBE ARCHIVO DE PAGO BANCOPPEL                 #
#Fecha creacion    => 14 DE SEPTIEMBRE DE 2009                                  #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Modificado por    => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiza   => 1 DE JUNIO DE 2010                                        #
#                  => Se cambia la forma de determinar la fecha de pago de caja #
#                     Coppel, en caso de recibirse el archivo el mismo dia u    #
#                     otro diferente al que se genera.                          #
#Modificado por    => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiza   => 15 DE JUNIO DE 2011                                       #
#                  => Se agregan los movimientos correspondientes a Pension     #
#                     Minima Garantizada                                        #
# Actualizacion    => JAVIER GONZALEZ JERONIMO                                  #
# Fecha            => 15 DE FEBRERO DE 2012                                     #
#                     Modificaciones relacionadas a los cambios de la ley del   #
#                     INFONAVIT (Req. EFPS-187)                                 #
#Sistema           => RETIROS                                                   #
#-------------------------------------------------------------------------------#
#Modificado por    => Phelippe R Santos                                         #
#                  => Se modifica el query de parciales                         #
#Fecha actualiza   => 30 ABRIL DE 2015        CPL-1963                          #
#################################################################################
#Req CPL-1902  => CMR 30/07/2015 se agregan retiros complementarios             #
#Req CPL-3030  => IJR 30/11/2019 se agregan movimientos de pmg ISSSTE           #
#################################################################################
DATABASE safre_af

    DEFINE gs_modulo RECORD LIKE seg_modulo.*

    DEFINE gr_edo RECORD
        liquidado               LIKE ret_estado.estado_solicitud,
        enviaop16               LIKE ret_estado.estado_solicitud,    #CPL-2021
        recibeop16              LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE gr_edo_pmg RECORD
        en_proceso_pago         LIKE pen_estado_pmg.estado_solicitud    ,
        liquidado               LIKE pen_estado_pmg.estado_solicitud
    END RECORD

    DEFINE gr_cza_coppel RECORD
        tipo_registro         CHAR(01)        ,
        num_cont_emp          CHAR(08)        ,
        fecha_gen             DATE            ,
        fecha_ini             DATE            ,
        fecha_fin             DATE            ,
        num_movs              INTEGER
    END RECORD

    DEFINE gr_det_coppel RECORD
        tipo_registro         CHAR(001)                     ,
        n_seguro              CHAR(011)                     ,
        nombre                CHAR(040)                     ,
        paterno               CHAR(040)                     ,
        materno               CHAR(040)                     ,
        forma_pago            CHAR(001)                     ,
        clabe                 CHAR(018)                     ,
        fecha_captura         DATE                          ,
        imp_doc_neto          DECIMAL(15,2)                 ,
        imp_doc_antes_impues  DECIMAL(15,2)                 ,
        impuesto_retenido     DECIMAL(11,2)                 ,
        num_fol_serv          CHAR(010)                     ,
        num_tienda            CHAR(004)                     ,
        tp_retiro             CHAR(003)                     ,
        consec_retiro         INTEGER                       ,
        curp                  CHAR(018)                     ,
        rfc                   LIKE afi_mae_afiliado.n_rfc   ,
        estatus               SMALLINT
    END RECORD

    DEFINE gr_sum_coppel RECORD
        tipo_registro         CHAR(01)        ,
        num_tot_movs          INTEGER         ,
        imp_tot_neto          DECIMAL(17, 2)  ,
        imp_tot_sin_impues    DECIMAL(17, 2)  ,
        imp_retenido          DECIMAL(17, 2)  ,
        imp_tot_retiros_efec  DECIMAL(17, 2)  ,
        imp_tot_retiros_depos DECIMAL(17, 2)
    END RECORD

    DEFINE
        gd_tot_neto                 ,
        gd_tot_sin_impues           ,
        gd_tot_retenido             ,
        gd_tot_retiros_efec         ,
        gd_tot_retiros_depos        DECIMAL(17,2)

    DEFINE
        gd_tot_neto_otros           ,
        gd_tot_sin_impues_otros     ,
        gd_tot_retenido_otros       ,
        gd_tot_retiros_efec_otros   ,
        gd_tot_retiros_depos_otros  DECIMAL(17,2)

    DEFINE
        gc_comando                  CHAR(100) ,
        gc_usuario                  CHAR(012) ,
        gc_nom_bncpl                CHAR(100) ,
        gc_nom_otros1               CHAR(100) ,
        G_LISTA_1                   CHAR(100) ,
        G_LISTA_2                   CHAR(100) ,
        G_LISTA_3                   CHAR(100) ,
        G_LISTA_4                   CHAR(100) ,
        G_LISTA_5                   CHAR(100) ,
        G_LISTA_6                   CHAR(100) ,
        enter                       CHAR(001) ,
        cat                         CHAR(300) ,
        ch                          CHAR(100)

    DEFINE
        gs_tipo_disposicion         ,
        gs_tipo_disp_issste         ,
        gs_mov_viv97                ,
        gs_bancoppel                SMALLINT,
        gs_otrosbancos              SMALLINT,
        gs_bancomer                 SMALLINT

    DEFINE
        gdt_cambio_infonavit        ,
        gd_fec_ini                  ,
        gd_fec_fin                  ,
        HOY                         DATE

    DEFINE
        gi_num_regs                 ,
        gi_ult_folio                ,
        gi_total_movs               INTEGER,
        gi_total_movs_otros         INTEGER

#####################################################################

MAIN
    DEFER INTERRUPT

    OPTIONS
        INPUT WRAP          ,
        PROMPT LINE LAST    ,
        ACCEPT KEY CONTROL-I

    CALL f_lib_crea_log()
    CALL init()

    OPEN WINDOW main_win AT 2,2 WITH 20 ROWS, 78 COLUMNS ATTRIBUTE(BORDER)
    DISPLAY "                          < Ctrl-C > Salir                                     " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY "  RETM821       GENERA Y RECIBE ARCHIVO DE PAGO BANCOPPEL                      " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    MENU "BANCOPPEL"
        COMMAND "Genera archivo" "Genera Archivo para BANCOPEL"
            CALL f_genera_bancopel()

        COMMAND "Carga archivo" "Carga Archivo de Respuesta de BANCOPEL"
            CALL f_carga_bancoppel()

        COMMAND "Salir" "Salir del Programa"
            EXIT MENU
    END MENU

    CLOSE WINDOW main_win

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    -- -----------------------------------------------------------------------------

    INITIALIZE gd_fec_ini,
               gd_fec_fin TO NULL

    LET HOY                     = TODAY
    LET gdt_cambio_infonavit    = MDY(01,12,2012)
    LET gc_usuario              = f_lib_obten_user()

    SELECT codigo
    INTO   gs_mov_viv97
    FROM   tab_movimiento
    WHERE  descripcion  = "RETIRO DISPOSICION VIVIENDA 97 REG 73"

    ----- TIPOS DE TRAMITE -----
    SELECT cod_tramite
    INTO   gs_tipo_disposicion
    FROM   tab_tipo_tramite
    WHERE  descripcion = "DISPOSICION"

    SELECT cod_tramite
    INTO   gs_tipo_disp_issste
    FROM   tab_tipo_tramite
    WHERE  descripcion = "DISPOSICION ISSSTE"

    SELECT tipo_pago                     #3
    INTO   gs_bancoppel
    FROM   tab_pago
    WHERE  descripcion = "PAGO EN BANCOPPEL"

    SELECT tipo_pago                    
    INTO   gs_otrosbancos
    FROM   tab_pago
    WHERE  descripcion = "PAGO EN OTROS BANCOS"

    SELECT tipo_pago                    
    INTO   gs_bancomer
    FROM   tab_pago
    WHERE  descripcion = "PAGO EN BANCOMER"

    SELECT A.estado_solicitud            #8
    INTO   gr_edo.liquidado
    FROM   ret_estado A
    WHERE  A.descripcion = "LIQUIDADO"

    SELECT A.estado_solicitud            #13
    INTO   gr_edo.enviaop16
    FROM   ret_estado A
    WHERE  A.descripcion = "ENVIADO OP16"

    SELECT A.estado_solicitud            #14
    INTO   gr_edo.recibeop16
    FROM   ret_estado A
    WHERE  A.descripcion = "RECIBIDO OP16"

    SELECT A.estado_solicitud
    INTO   gr_edo_pmg.en_proceso_pago
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "EN PROCESO DE PAGO"

    SELECT A.estado_solicitud
    INTO   gr_edo_pmg.liquidado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "LIQUIDADO"

    SELECT *
    INTO   gs_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "int"

--    LET gs_modulo.ruta_envio = "/home/efp2/ret"

    LET gr_cza_coppel.num_movs      = 0
    LET gi_total_movs               = 0
    LET gi_total_movs_otros         = 0
    LET gd_tot_neto                 = 0
    LET gd_tot_sin_impues           = 0
    LET gd_tot_retenido             = 0
    LET gd_tot_retiros_efec         = 0
    LET gd_tot_retiros_depos        = 0
    LET gd_tot_neto_otros           = 0
    LET gd_tot_sin_impues_otros     = 0
    LET gd_tot_retenido_otros       = 0
    LET gd_tot_retiros_efec_otros   = 0

    ----- HABIL SIGUIENTE -----
    LET gc_comando = " EXECUTE FUNCTION fn_habil_siguiente ( ?,? )"
    PREPARE eje_dia_sig FROM gc_comando

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_bancopel : Ejecuta los pasos para generar el archivo bancoppel   #
#---------------------------------------------------------------------------#
FUNCTION f_genera_bancopel()

    DEFINE
        ls_flag         SMALLINT

    -- -----------------------------------------------------------------------------

    OPEN WINDOW RETM8211 AT 2,2 WITH FORM "RETM8211" ATTRIBUTE(BORDER)
    DISPLAY " RETM821              GENERA ARCHIVO DE PAGO BANCOPPEL                         " AT 2,1 ATTRIBUTE(REVERSE)
    DISPLAY " <ESC> - Ejecutar                                            <CTRL-C> - Salir  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 2,68 ATTRIBUTE(REVERSE)
    DISPLAY " VER. 3367" AT 2,1 ATTRIBUTE(REVERSE)

    CALL f_captura_fechas() RETURNING gd_fec_ini, gd_fec_fin, ls_flag

    IF (ls_flag = TRUE) THEN
        IF f_lib_pregunta("¿DESEA GENERAR EL REPORTE? (S/N) ") = TRUE THEN
            -- Genera la tabla temporal de dis_cuenta
            CALL f_genera_tmp_cuenta(gd_fec_ini, gd_fec_fin)
            CALL f_genera_archivo()
            CALL f_lib_error_msg("PROCESO FINALIZADO CORRECTAMENTE")
        ELSE
            CALL f_lib_error_msg("PROCESO CANCELADO")
        END IF
    ELSE
        CALL f_lib_error_msg("PROCESO CANCELADO")
    END IF

    CLEAR SCREEN
    CLOSE WINDOW RETM8211

END FUNCTION

#---------------------------------------------------------------------------#
# f_captura_fechas : Captura el intervalo de fechas de donde se tomaran los #
#                    datos para generar el archivo bancoppel                #
#---------------------------------------------------------------------------#
FUNCTION f_captura_fechas()

    DEFINE lr_captura RECORD
        fecha_conversion_ini  DATE,
        fecha_conversion_fin  DATE
    END RECORD

    DEFINE
        ls_flag             SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_flag = TRUE
    LET lr_captura.fecha_conversion_fin  =  HOY
    LET lr_captura.fecha_conversion_ini  =  HOY


    DISPLAY BY NAME lr_captura.fecha_conversion_fin
    DISPLAY BY NAME lr_captura.fecha_conversion_ini

    INPUT BY NAME lr_captura.* WITHOUT DEFAULTS

        AFTER FIELD fecha_conversion_fin

            IF (lr_captura.fecha_conversion_ini) IS NULL THEN
                CALL f_lib_error_msg("LA FECHA INICIAL NO PUEDE SER NULA")
                NEXT FIELD fecha_conversion_ini
            END IF

            IF (lr_captura.fecha_conversion_fin) IS NULL THEN
                CALL f_lib_error_msg("LA FECHA FINAL NO PUEDE SER NULA")
                NEXT FIELD fecha_conversion_fin
            END IF

            IF ( (lr_captura.fecha_conversion_ini > lr_captura.fecha_conversion_fin) OR
                 (lr_captura.fecha_conversion_fin > HOY)
               )
            THEN
                CALL f_lib_error_msg("LA FECHA INICIAL NO PUEDE SER MAYOR A LA FECHA FINAL")
                NEXT FIELD fecha_conversion_ini
            END IF

        ON KEY(ESC)
            IF (lr_captura.fecha_conversion_ini) IS NULL THEN
                CALL f_lib_error_msg("LA FECHA INICIAL NO PUEDE SER NULA")
                NEXT FIELD fecha_conversion_ini
            END IF

            IF (lr_captura.fecha_conversion_fin) IS NULL THEN
                CALL f_lib_error_msg("LA FECHA FINAL NO PUEDE SER NULA")
                NEXT FIELD fecha_conversion_fin
            END IF

            IF ( (lr_captura.fecha_conversion_ini > lr_captura.fecha_conversion_fin) OR
                 (lr_captura.fecha_conversion_fin > HOY)
               )
            THEN
                CALL f_lib_error_msg("LA FECHA INICIAL NO PUEDE SER MAYOR A LA FECHA FINAL")
                NEXT FIELD fecha_conversion_ini
            END IF

            EXIT INPUT

        ON KEY(CONTROL-C,INTERRUPT)
            LET ls_flag = FALSE
            EXIT INPUT

    END INPUT

    RETURN lr_captura.*, ls_flag

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_archivo : Llama a los reportes para generar el archivo bancoppel #
#---------------------------------------------------------------------------#
FUNCTION f_genera_archivo()

    DEFINE
        lc_nom_arch_gen         CHAR(100),        
        lc_nom_arch_gen_otros   CHAR(100)

    -- -----------------------------------------------------------------------------

    LET gi_ult_folio    = f_lib_obtiene_ult_folio()

    CALL genera_det_coppel()   #gdc
    CALL genera_cza_coppel()   #gcc
    CALL genera_sum_coppel()   #gsc

    LET gc_nom_bncpl = "PAGOS", HOY USING"DDMMYYYY", ".ACOPPEL.01"
    LET gc_nom_bncpl = gc_nom_bncpl CLIPPED

    LET gc_nom_otros1 = "PAGOS", HOY USING"DDMMYYYY", ".OBACOPPEL.01"
    LET gc_nom_otros1 = gc_nom_otros1 CLIPPED

    LET lc_nom_arch_gen = gs_modulo.ruta_envio CLIPPED, "/", gc_nom_bncpl
    LET lc_nom_arch_gen = lc_nom_arch_gen CLIPPED

    LET lc_nom_arch_gen = gs_modulo.ruta_envio CLIPPED, "/", gc_nom_otros1
    LET lc_nom_arch_gen = lc_nom_arch_gen_otros CLIPPED

    LET cat = "cat ",G_LISTA_1 CLIPPED," ",G_LISTA_2 CLIPPED," ",
              G_LISTA_3 CLIPPED," > ", gs_modulo.ruta_envio CLIPPED, "/", gc_nom_bncpl
    RUN cat

    LET cat = "cat ",G_LISTA_4 CLIPPED," ",G_LISTA_5 CLIPPED," ",
              G_LISTA_6 CLIPPED," > ", gs_modulo.ruta_envio CLIPPED, "/", gc_nom_otros1
    RUN cat

    DISPLAY "lc_nom_arch_gen",lc_nom_arch_gen
    DISPLAY "lc_nom_arch_gen_otros",lc_nom_arch_gen_otros
    LET ch = "chmod 777 ",lc_nom_arch_gen
    RUN ch

    LET ch = "chmod 777 ",lc_nom_arch_gen_otros
    RUN ch

    DISPLAY "                           " AT 19,2
    DISPLAY "EL ARCHIVO HA SIDO GENERADO EN LA RUTA :                 " AT 11,18
    DISPLAY gs_modulo.ruta_envio CLIPPED ,"                           " AT 12,18
    DISPLAY "CON EL NOMBRE COPPEL: ", gc_nom_bncpl AT 14,18
    DISPLAY "CON EL NOMBRE OTROS BANCOS: ", gc_nom_otros1 AT 15,18
    DISPLAY "               FOLIO : ", gi_ult_folio AT 17,18
    DISPLAY "REGISTROS PROCESADOS COPPEL: ", gi_total_movs  AT 18,18
    DISPLAY "REGISTROS PROCESADOS OTROS: ", gi_total_movs_otros  AT 19,18

END FUNCTION

#-----------------------------------------------------------------------------#
# genera_cza_coppel : Obtiene los datos para generar el encabezado del archivo#
#-----------------------------------------------------------------------------#
FUNCTION genera_cza_coppel()

    LET gr_cza_coppel.tipo_registro      = "E"
    LET gr_cza_coppel.num_cont_emp       = "12345678"
    LET gr_cza_coppel.fecha_gen          = HOY
    LET gr_cza_coppel.fecha_ini          = gd_fec_ini
    LET gr_cza_coppel.fecha_fin          = gd_fec_fin
    LET gr_cza_coppel.num_movs           = gi_total_movs

    LET G_LISTA_1 = gs_modulo.ruta_envio CLIPPED,"/CST"
    LET G_LISTA_4 = gs_modulo.ruta_envio CLIPPED,"/CST2"

    START REPORT rpt_encabezado TO G_LISTA_1
        OUTPUT TO REPORT rpt_encabezado(gr_cza_coppel.*) #1
    FINISH REPORT rpt_encabezado

    START REPORT rpt_encabezado_otros TO G_LISTA_4
        LET gr_cza_coppel.num_movs           = gi_total_movs_otros
        OUTPUT TO REPORT rpt_encabezado_otros(gr_cza_coppel.*) #4
    FINISH REPORT rpt_encabezado_otros

    LET ch = "chmod 777 ",G_LISTA_1
    RUN ch

    LET ch = "chmod 777 ",G_LISTA_4
    RUN ch

END FUNCTION

#---------------------------------------------------------------------------#
# genera_det_coppel : Obtiene los datos para generar el detalle del archivo #
#---------------------------------------------------------------------------#
FUNCTION genera_det_coppel()

    DEFINE lc_bandera    CHAR(10)

    DEFINE v_valida_saldo SMALLINT
    DEFINE v_fecha        CHAR(10)
    DEFINE v_reporte_diferencia CHAR(100)
    DEFINE v_borra_arh          SMALLINT
    DEFINE v_monto_dis          DECIMAL(22,6)
    DEFINE v_monto_benef        DECIMAL(22,6)

    ----------------------------------------------------------------------------

    DISPLAY "GENERANDO DETALLE ..." AT 19,2

    CALL f_genera_tabla_datos()

    LET gr_det_coppel.tipo_registro = "D"
    LET gr_det_coppel.estatus       = 1

    LET G_LISTA_2 = gs_modulo.ruta_envio CLIPPED,"/DST"
    START REPORT rpt_detalle TO G_LISTA_2

    LET G_LISTA_5 = gs_modulo.ruta_envio CLIPPED,"/DST2"
    START REPORT rpt_detalle_otros TO G_LISTA_5

    --[CPL-3342]
    LET v_fecha     = TODAY
    LET v_borra_arh = 1
    LET v_reporte_diferencia = gs_modulo.ruta_envio CLIPPED,"/RETM821_DIFERENCIAS_IMPORTES_",v_fecha[4,5],v_fecha[1,2],v_fecha[7,10],".txt"
    START REPORT rpt_diferencia TO v_reporte_diferencia
    --//

    --SELECCION DE MOVIMIENTOS A INFORMAR EN REPORTE   --CPL-3030
    DECLARE cur_1 CURSOR FOR
    SELECT *
    FROM   tmp_datos_bancoppel
    ORDER  BY nss

    FOREACH cur_1 INTO gr_det_coppel.n_seguro      ,
                       gr_det_coppel.nombre        ,
                       gr_det_coppel.paterno       ,
                       gr_det_coppel.materno       ,
                       gr_det_coppel.num_tienda    ,
                       gr_det_coppel.consec_retiro ,
                       gr_det_coppel.clabe         ,
                       gr_det_coppel.forma_pago    ,
                       gr_det_coppel.curp          ,
                       gr_det_coppel.tp_retiro     ,
                       lc_bandera

        --Sustituye cadenas de nombre y apellido
        LET gr_det_coppel.nombre  = f_lib_evalua_cadenas_enie(gr_det_coppel.nombre)
        LET gr_det_coppel.paterno = f_lib_evalua_cadenas_enie(gr_det_coppel.paterno)
        LET gr_det_coppel.materno = f_lib_evalua_cadenas_enie(gr_det_coppel.materno)

        LET gr_det_coppel.impuesto_retenido = f_calc_impuesto_retenido(gr_det_coppel.n_seguro     ,
                                                                       gr_det_coppel.consec_retiro)

        LET gr_det_coppel.rfc = f_obten_rfc(gr_det_coppel.n_seguro,gr_det_coppel.consec_retiro)

        LET lc_bandera = lc_bandera CLIPPED
        
        CASE lc_bandera
            WHEN "dis"
                CALL f_datos_disposicion(gr_det_coppel.n_seguro, gr_det_coppel.consec_retiro)
                    RETURNING gr_det_coppel.fecha_captura,
                              gr_det_coppel.num_fol_serv ,
                              gr_det_coppel.imp_doc_neto

            WHEN "benef"
                LET gr_det_coppel.impuesto_retenido = f_calc_impuesto_retenido_benef(gr_det_coppel.n_seguro,
                                                                               gr_det_coppel.consec_retiro)
                CALL f_datos_disposicion_benef(gr_det_coppel.n_seguro, gr_det_coppel.consec_retiro)
                    RETURNING gr_det_coppel.fecha_captura,
                              gr_det_coppel.num_fol_serv ,
                              gr_det_coppel.imp_doc_neto,
                              v_valida_saldo,
                              v_monto_dis,
                              v_monto_benef
                              
                IF NOT v_valida_saldo THEN
                   OUTPUT TO REPORT rpt_diferencia(gr_det_coppel.num_fol_serv,gr_det_coppel.n_seguro,gr_det_coppel.consec_retiro,v_monto_dis,v_monto_benef,lc_bandera) --(gr_det_coppel.n_seguro,gr_det_coppel.consec_retiro,lc_bandera)
                   LET v_borra_arh = 0
                END IF

            WHEN "iss"
                CALL f_datos_issste(gr_det_coppel.n_seguro      ,
                                    gr_det_coppel.consec_retiro ,
                                    gr_det_coppel.tp_retiro     )
                    RETURNING gr_det_coppel.fecha_captura,
                              gr_det_coppel.num_fol_serv ,
                              gr_det_coppel.imp_doc_neto

            WHEN "iss_ant"
                CALL f_datos_issste_ant(gr_det_coppel.n_seguro      ,
                                        gr_det_coppel.consec_retiro ,
                                        gr_det_coppel.tp_retiro     )
                    RETURNING gr_det_coppel.fecha_captura,
                              gr_det_coppel.num_fol_serv ,
                              gr_det_coppel.imp_doc_neto

            WHEN "par"

                CALL f_datos_parcial(gr_det_coppel.n_seguro, gr_det_coppel.consec_retiro)
                    RETURNING gr_det_coppel.fecha_captura,
                              gr_det_coppel.num_fol_serv ,
                              gr_det_coppel.imp_doc_neto

            WHEN "pmg"
                CALL f_datos_pmg(gr_det_coppel.n_seguro, gr_det_coppel.consec_retiro)
                    RETURNING gr_det_coppel.fecha_captura,
                              gr_det_coppel.num_fol_serv ,
                              gr_det_coppel.imp_doc_neto

            WHEN "pmg_iss"   --CPL-3030
                CALL f_datos_pmg_iss(gr_det_coppel.n_seguro, gr_det_coppel.consec_retiro)
                    RETURNING gr_det_coppel.fecha_captura,
                              gr_det_coppel.num_fol_serv ,
                              gr_det_coppel.imp_doc_neto

            WHEN "vol"
                LET gr_det_coppel.fecha_captura = gd_fec_ini

                CALL f_datos_voluntarias(gr_det_coppel.n_seguro, gr_det_coppel.consec_retiro)
                    RETURNING gr_det_coppel.num_fol_serv ,
                              gr_det_coppel.imp_doc_neto

            WHEN "com"      --cpl-1902
               LET gr_det_coppel.fecha_captura = gd_fec_ini

                CALL f_datos_complemen(gr_det_coppel.n_seguro, gr_det_coppel.consec_retiro)
                    RETURNING gr_det_coppel.num_fol_serv ,
                              gr_det_coppel.imp_doc_neto

        END CASE
        #CPL-2065
        CALL f_valida_datos(gr_det_coppel.n_seguro, lc_bandera,gr_det_coppel.num_fol_serv, gr_det_coppel.fecha_captura)

        LET gr_det_coppel.imp_doc_antes_impues = gr_det_coppel.imp_doc_neto + gr_det_coppel.impuesto_retenido

        --Acumula totales
        IF(gr_det_coppel.forma_pago = gs_bancoppel)THEN
            LET gi_total_movs           = gi_total_movs + 1
            LET gd_tot_neto             = gd_tot_neto + gr_det_coppel.imp_doc_neto
            LET gd_tot_sin_impues       = gd_tot_sin_impues + gr_det_coppel.imp_doc_antes_impues
            LET gd_tot_retenido         = gd_tot_retenido + gr_det_coppel.impuesto_retenido
            LET gd_tot_retiros_efec         = 0
            LET gd_tot_retiros_depos        = 0
        ELSE
            LET gi_total_movs_otros     = gi_total_movs_otros + 1
            LET gd_tot_neto_otros       = gd_tot_neto_otros + gr_det_coppel.imp_doc_neto
            LET gd_tot_sin_impues_otros = gd_tot_sin_impues_otros + gr_det_coppel.imp_doc_antes_impues
            LET gd_tot_retenido_otros   = gd_tot_retenido_otros + gr_det_coppel.impuesto_retenido
            LET gd_tot_retiros_efec_otros   = 0
            LET gd_tot_retiros_depos_otros  = 0
        END IF

        DISPLAY "PROCESANDO REGISTRO BANCOPPEL: ", gi_total_movs  AT 11,18        
        DISPLAY "PROCESANDO REGISTRO OTROS    : ", gi_total_movs_otros  AT 12,18

        CALL f_insert_tabla(gi_ult_folio, gr_det_coppel.*, lc_bandera)

        IF(gr_det_coppel.forma_pago = gs_bancoppel)THEN
            OUTPUT TO REPORT rpt_detalle(gr_det_coppel.*) #2
        ELSE
            OUTPUT TO REPORT rpt_detalle_otros(gr_det_coppel.*) #5
        END IF

    END FOREACH

    FINISH REPORT rpt_detalle
    FINISH REPORT rpt_detalle_otros
    LET ch = "chmod 777 ",G_LISTA_2
    RUN ch
    
    LET ch = "chmod 777 ",G_LISTA_5
    RUN ch

    FINISH REPORT rpt_diferencia
    IF v_borra_arh THEN 
       LET ch = "rm ",v_reporte_diferencia
    ELSE
       LET ch = "chmod 777 ",v_reporte_diferencia
    END IF
    RUN ch

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_datos : valida la fecha de captura el folio de servicios CPL-2065#
#---------------------------------------------------------------------------#
FUNCTION f_valida_datos(pc_nss, pc_mod, pc_folio_serv,pf_cap)
DEFINE pc_nss            CHAR(11),
       pc_mod,
       pc_folio_serv     CHAR(10),
       pf_cap            DATE,      #fecha captura
       lc_msj            CHAR(100)

LET pc_nss          = pc_nss        CLIPPED
LET pc_mod          = pc_mod        CLIPPED
LET pc_folio_serv   = pc_folio_serv CLIPPED

LET lc_msj = ""

IF pc_folio_serv IS NULL OR pc_folio_serv = "" THEN
   LET lc_msj = "SIN FOLIO DE SERVICIO NSS: ",pc_nss, " MOD-",pc_mod
   CALL f_lib_error_msg (lc_msj)
   EXIT PROGRAM
END IF

IF pf_cap IS NULL OR pf_cap = MDY(12,31,1899) THEN
   LET lc_msj = "SIN FECHA CAPTURA NSS: ",pc_nss, " MOD-",pc_mod
   CALL f_lib_error_msg (lc_msj)
   EXIT PROGRAM
END IF

END FUNCTION

#---------------------------------------------------------------------------#
# genera_sum_coppel : Obtiene los datos para generar el sumario del archivo #
#---------------------------------------------------------------------------#
FUNCTION genera_sum_coppel()

    LET gr_sum_coppel.tipo_registro         = "S"
    LET gr_sum_coppel.num_tot_movs          = gi_total_movs
    LET gr_sum_coppel.imp_tot_neto          = gd_tot_neto
    LET gr_sum_coppel.imp_tot_sin_impues    = gd_tot_sin_impues
    LET gr_sum_coppel.imp_retenido          = gd_tot_retenido
    LET gr_sum_coppel.imp_tot_retiros_efec  = gd_tot_retiros_efec
    LET gr_sum_coppel.imp_tot_retiros_depos = gd_tot_neto

    LET G_LISTA_3 = gs_modulo.ruta_envio CLIPPED,"/SST"

    START REPORT rpt_sumario TO G_LISTA_3
        OUTPUT TO REPORT rpt_sumario(gr_sum_coppel.*) #3
    FINISH REPORT rpt_sumario

    LET G_LISTA_6 = gs_modulo.ruta_envio CLIPPED,"/SST2"

    START REPORT rpt_sumario_otros TO G_LISTA_6
        LET gr_sum_coppel.num_tot_movs          = gi_total_movs_otros
        LET gr_sum_coppel.imp_tot_neto          = gd_tot_neto_otros
        LET gr_sum_coppel.imp_tot_sin_impues    = gd_tot_sin_impues_otros
        LET gr_sum_coppel.imp_retenido          = gd_tot_retenido_otros
        LET gr_sum_coppel.imp_tot_retiros_efec  = gd_tot_retiros_efec_otros
        LET gr_sum_coppel.imp_tot_retiros_depos = gd_tot_neto_otros
        OUTPUT TO REPORT rpt_sumario_otros(gr_sum_coppel.*) #6
    FINISH REPORT rpt_sumario_otros

    LET ch = "chmod 777 ",G_LISTA_3
    RUN ch

    LET ch = "chmod 777 ",G_LISTA_6
    RUN ch


END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_tmp_cuenta : Genera el extracto de discuenta con los movimientos #
#                       a utilizarse usando las fechas capturadas           #
#---------------------------------------------------------------------------#
FUNCTION f_genera_tmp_cuenta(pr_fechas)

    DEFINE pr_fechas RECORD
        fecha_ini  DATE,
        fecha_fin  DATE
    END RECORD

    -- -----------------------------------------------------------------------------

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_dat_dis_cta
    WHENEVER ERROR STOP

    SELECT nss              ,
           curp             ,
           folio            ,
           fecha_conversion ,
           subcuenta        ,
           consecutivo_lote ,
           tipo_movimiento  ,
           monto_en_pesos   ,
           monto_en_acciones
    FROM   dis_cuenta
    WHERE  fecha_conversion BETWEEN pr_fechas.fecha_ini AND pr_fechas.fecha_fin
    AND   (   tipo_movimiento = 490               -- Voluntarias
           OR tipo_movimiento = 493               -- Voluntarias APP    
           OR tipo_movimiento = 10                -- Retencion de ISR
           OR tipo_movimiento BETWEEN 800 AND 899 -- Retiros IMSS e ISSTE
           )
    INTO TEMP tmp_dat_dis_cta

    CREATE INDEX cta_data_01
    ON tmp_dat_dis_cta (tipo_movimiento, consecutivo_lote, nss)

    UPDATE STATISTICS FOR TABLE tmp_dat_dis_cta

    CALL f_elimina_reg73_viv()

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_tabla_datos : Genera la tabla que contiene los datos de los      #
#                        registros liquidados en el periodo dado            #
#---------------------------------------------------------------------------#
FUNCTION f_genera_tabla_datos()

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_datos_bancoppel
    WHENEVER ERROR STOP

    SELECT  UNIQUE(a.nss) nss   , -- Disposiciones
            b.nombres           ,
            b.paterno           ,
            b.materno           ,
            b.tienda_cod        ,
            b.consecutivo       ,
            b.num_cuenta        ,
            b.tipo_pago         ,
            c.curp              ,
            a.tipo_movimiento   ,
            "dis" mod_retiro
    FROM    tmp_dat_dis_cta a   ,
            ret_beneficiario b  ,
            ret_solicitud_tx c
    WHERE   (a.tipo_movimiento  BETWEEN 800 AND 899
             AND a.tipo_movimiento NOT BETWEEN 881 AND 889)
    AND     a.consecutivo_lote  = b.consecutivo
    AND     a.consecutivo_lote  = c.consecutivo
    AND     a.nss               = b.nss
    AND     a.nss               = c.nss
    AND     b.tipo_pago         IN( gs_bancoppel,gs_otrosbancos,gs_bancomer)
    AND     c.consecutivo NOT IN (SELECT d.consecutivo_solic
                                  FROM ret_ctr_benef d
                                  WHERE nss = a.nss )
    UNION 
    SELECT  UNIQUE(a.nss) nss   , -- Beneficiarios designados
            b.nombres           ,
            b.paterno           ,
            b.materno           ,
            b.tienda_cod        ,
            b.consecutivo       ,
            b.num_cuenta        ,
            b.tipo_pago         ,
            a.curp              ,
            a.tipo_movimiento   ,
            "benef" mod_retiro
    FROM    tmp_dat_dis_cta  a  ,
            ret_beneficiario b  ,
            ret_ctr_benef    c
    WHERE   (a.tipo_movimiento  BETWEEN 800 AND 899
             AND a.tipo_movimiento NOT BETWEEN 881 AND 889)
    AND     b.consecutivo       = c.consecutivo_solic
    AND     a.consecutivo_lote  = c.consecutivo_padre
    AND     a.nss               = b.nss
    AND     a.nss               = c.nss
    AND     b.tipo_pago         IN( gs_bancoppel,gs_otrosbancos,gs_bancomer)
    UNION
    SELECT  unique(a.nss) nss   , -- Retiros Totales ISSSTE
            b.nombres           ,
            b.paterno           ,
            b.materno           ,
            b.tienda_cod        ,
            b.consecutivo       ,
            b.num_cuenta        ,
            b.tipo_pago         ,
            c.curp              ,
            a.tipo_movimiento   ,
            "iss" mod_retiro
    FROM    tmp_dat_dis_cta a   ,
            ret_beneficiario b  ,
            ret_sol_issste_tx c
    WHERE   ( (a.tipo_movimiento BETWEEN 851 AND 855)
               OR (a.tipo_movimiento = 864)
               OR (a.tipo_movimiento = 857) )     ---- El tipo de retiro G y/o R tambien pueden ser totales
    AND     a.consecutivo_lote  = b.consecutivo
    AND     a.consecutivo_lote  = c.consecutivo
    AND     a.nss               = b.nss
    AND     a.nss               = c.nss
    AND     b.tipo_pago         IN( gs_bancoppel,gs_otrosbancos,gs_bancomer)
    UNION
    SELECT  unique(a.nss) nss   , -- Retiros Parciales ISSSTE
            b.nombres           ,
            b.paterno           ,
            b.materno           ,
            b.tienda_cod        ,
            b.consecutivo       ,
            b.num_cuenta        ,
            b.tipo_pago         ,
            c.curp              ,
            a.tipo_movimiento   ,
            "iss" mod_retiro
    FROM    tmp_dat_dis_cta a   ,
            ret_beneficiario b  ,
            ret_parcial_issste c
    WHERE   a.tipo_movimiento BETWEEN 856 AND 856
    AND     a.consecutivo_lote  = b.consecutivo
    AND     a.consecutivo_lote  = c.consecutivo
    AND     a.nss               = b.nss
    AND     a.nss               = c.nss
    AND     b.tipo_pago         IN( gs_bancoppel,gs_otrosbancos,gs_bancomer)
    UNION
    SELECT  unique(a.nss) nss   , -- Parciales    #CPL-2021
            b.nombres           ,
            b.paterno           ,
            b.materno           ,
            b.tienda_cod        ,
            b.consecutivo       ,
            b.num_cuenta        ,
            b.tipo_pago         ,
            d.curp              ,
            a.tipo_movimiento   ,
            "par" mod_retiro
    FROM    tmp_dat_dis_cta a   ,
            ret_beneficiario b  ,
            ret_parcial d
    WHERE   a.tipo_movimiento  IN (870, 875, 876, 877, 878)
    AND     a.consecutivo_lote  = b.consecutivo
    AND     a.consecutivo_lote  = d.consecutivo                    --CPL-1963
    AND     a.nss               = b.nss
    AND     a.nss               = d.nss
    AND     b.tipo_pago         IN( gs_bancoppel,gs_otrosbancos,gs_bancomer)
    UNION
    SELECT  unique(a.nss) nss   , -- Voluntarias
            b.nombres           ,
            b.paterno           ,
            b.materno           ,
            b.tienda_cod        ,
            b.consecutivo       ,
            b.num_cuenta        ,
            b.tipo_pago         ,
            a.curp              ,
            a.tipo_movimiento   ,
            "vol" mod_retiro
    FROM    tmp_dat_dis_cta a   ,
            ret_beneficiario b
    WHERE   a.tipo_movimiento   IN (490,493)
    AND     a.consecutivo_lote  = b.consecutivo
    AND     a.nss               = b.nss
    AND     b.tipo_pago         IN( gs_bancoppel,gs_otrosbancos,gs_bancomer)
    UNION
    SELECT  unique(a.nss) nss   , -- complementarias   --cpl-1902 inicia
            b.nombres           ,
            b.paterno           ,
            b.materno           ,
            b.tienda_cod        ,
            b.consecutivo       ,
            b.num_cuenta        ,
            b.tipo_pago         ,
            a.curp              ,
            a.tipo_movimiento   ,
            "com" mod_retiro
    FROM    tmp_dat_dis_cta a   ,
            ret_beneficiario b
    WHERE   a.tipo_movimiento   = 897
    AND     a.consecutivo_lote  = b.consecutivo
    AND     a.nss               = b.nss
    AND     b.tipo_pago         IN( gs_bancoppel,gs_otrosbancos,gs_bancomer)
    UNION
    SELECT  unique(a.nss) nss   , -- Retiros Totales ISSSTE (Modulo Antiguo)
            b.nombres           ,
            b.paterno           ,
            b.materno           ,
            b.tienda_cod        ,
            b.consecutivo       ,
            b.num_cuenta        ,
            b.tipo_pago         ,
            c.curp              ,
            a.tipo_movimiento   ,
            "iss_ant" mod_retiro
    FROM    tmp_dat_dis_cta a   ,
            ret_beneficiario b  ,
            ret_sol_issste_tot c
    WHERE   (a.tipo_movimiento BETWEEN 881 AND 889
             AND a.tipo_movimiento NOT IN (884, 885) )
    AND     a.consecutivo_lote  = b.consecutivo
    AND     a.consecutivo_lote  = c.consecutivo
    AND     a.nss               = b.nss
    AND     a.nss               = c.nss_imss
    AND     b.tipo_pago         IN( gs_bancoppel,gs_otrosbancos,gs_bancomer)
    UNION
    SELECT  unique(a.nss) nss   , -- Retiros Parciales ISSSTE (Modulo Antiguo)
            b.nombres           ,
            b.paterno           ,
            b.materno           ,
            b.tienda_cod        ,
            b.consecutivo       ,
            b.num_cuenta        ,
            b.tipo_pago         ,
            c.curp              ,
            a.tipo_movimiento   ,
            "iss_ant" mod_retiro
    FROM    tmp_dat_dis_cta a   ,
            ret_beneficiario b  ,
            ret_sol_issste_par c
    WHERE   a.tipo_movimiento IN (884, 885)
    AND     a.consecutivo_lote  = b.consecutivo
    AND     a.consecutivo_lote  = c.consecutivo
    AND     a.nss               = b.nss
    AND     a.nss               = c.nss_imss
    AND     b.tipo_pago         IN( gs_bancoppel,gs_otrosbancos,gs_bancomer)
    UNION
    SELECT  unique(a.nss) nss   , -- Pension Minima Garantizada
            b.nombres           ,
            b.paterno           ,
            b.materno           ,
            b.tienda_cod        ,
            b.consecutivo       ,
            b.num_cuenta        ,
            b.tipo_pago         ,
            d.curp              ,
            a.tipo_movimiento   ,
            "pmg" mod_retiro
    FROM    tmp_dat_dis_cta a   ,
            ret_beneficiario b  ,
            pen_solicitud_pmg d
    WHERE   a.tipo_movimiento   = 841
    AND     a.consecutivo_lote  = b.consecutivo
    AND     a.nss               = b.nss
    AND     a.nss               = d.nss
    AND     b.tipo_pago         IN( gs_bancoppel,gs_otrosbancos,gs_bancomer)
    UNION
    SELECT  unique(a.nss) nss   , -- Pension Minima Garantizada ISSSTE--CPl-3030
            b.nombres           ,
            b.paterno           ,
            b.materno           ,
            b.tienda_cod        ,
            b.consecutivo       ,
            b.num_cuenta        ,
            b.tipo_pago         ,
            d.curp              ,
            a.tipo_movimiento   ,
            "pmg_iss" mod_retiro
    FROM    tmp_dat_dis_cta   a ,
            ret_beneficiario  b ,
            pen_solicitud_iss d
    WHERE   a.tipo_movimiento   IN (857, 859)
    AND     a.consecutivo_lote  = b.consecutivo
    AND     a.nss               = b.nss
    AND     a.nss               = d.nss
    AND     b.tipo_pago         IN( gs_bancoppel,gs_otrosbancos,gs_bancomer)
    INTO TEMP tmp_datos_bancoppel

END FUNCTION

#---------------------------------------------------------------------------#
# f_elimina_reg73_viv : Elimina los registros de dis_cuenta correspondientes#
#                      a la liquidacion de las disposiciones regimen 73     #
#                      cuya fecha de resolucion sea mayor al 12 de enero    #
#                      de 2012                                              #
#---------------------------------------------------------------------------#
FUNCTION f_elimina_reg73_viv()

    DEFINE lr_elimina RECORD
        nss         LIKE dis_cuenta.nss             ,
        folio       LIKE dis_cuenta.folio           ,
        consecutivo LIKE dis_cuenta.consecutivo_lote
    END RECORD

    DECLARE cur_elimina CURSOR FOR
       SELECT nss               ,
              folio             ,
              consecutivo_lote
       FROM   tmp_dat_dis_cta
       WHERE  tipo_movimiento  = gs_mov_viv97
       AND    subcuenta        IN (4,8)
       AND    nss IN (SELECT B.nss
                      FROM   tmp_dat_dis_cta  A   ,
                             ret_solicitud_tx B
                      WHERE  A.nss              = B.nss
                      AND    A.folio            = B.folio
                      AND    A.consecutivo_lote = B.consecutivo
                      AND    A.tipo_movimiento  = gs_mov_viv97
                      AND    B.tipo_retiro      IN ("E","M")
                      AND    B.regimen          = 73
                      AND    A.subcuenta        IN (4,8)
                      AND    B.fecha_resolucion > gdt_cambio_infonavit
                     )

    FOREACH cur_elimina INTO lr_elimina.*
        DELETE
        FROM   tmp_dat_dis_cta
        WHERE  nss              = lr_elimina.nss
        AND    folio            = lr_elimina.folio
        AND    consecutivo_lote = lr_elimina.consecutivo
        AND    subcuenta        IN (4,8)
    END FOREACH

END FUNCTION


#---------------------------------------------------------------------------#
# f_calc_impuesto_retenido : Obtiene el monto de retencion de ISR del       #
#                            registros liquidado                            #
#---------------------------------------------------------------------------#
FUNCTION f_calc_impuesto_retenido(pc_nss, pi_consec)

    DEFINE
        pc_nss      LIKE dis_cuenta.nss             ,
        pi_consec   LIKE dis_cuenta.consecutivo_lote

    DEFINE
        ld_impuesto_ret     DECIMAL(11,2)

    -- -----------------------------------------------------------------------------

    --query para el importe retenido:
    SELECT SUM(ROUND(a.monto_en_pesos, 2))
    INTO   ld_impuesto_ret
    FROM   tmp_dat_dis_cta a
    WHERE  a.nss              = pc_nss
    AND    a.consecutivo_lote = pi_consec
    AND    a.tipo_movimiento  = 10

    IF ld_impuesto_ret IS NULL OR ld_impuesto_ret = " " THEN
        LET ld_impuesto_ret = 0
    END IF

    LET ld_impuesto_ret = ld_impuesto_ret * -100

    RETURN ld_impuesto_ret

END FUNCTION


#---------------------------------------------------------------------------#
# f_calc_impuesto_retenido : Obtiene el monto de retencion de ISR del       #
#                            registros liquidado Beneficiarios designados   #
#---------------------------------------------------------------------------#
FUNCTION f_calc_impuesto_retenido_benef(pc_nss, pi_consec)

    DEFINE
        pc_nss      LIKE dis_cuenta.nss             ,
        pi_consec   LIKE dis_cuenta.consecutivo_lote

    DEFINE
        ld_impuesto_ret     DECIMAL(11,2)

    --query para el importe retenido:
    SELECT SUM(ROUND(a.pesos_isr, 2))
    INTO   ld_impuesto_ret
    FROM   ret_ctr_benef_det a
    WHERE  a.nss               = pc_nss
    AND    a.consecutivo_solic = pi_consec

    IF ld_impuesto_ret IS NULL OR ld_impuesto_ret = " " THEN
       LET ld_impuesto_ret = 0
    END IF

    LET ld_impuesto_ret = ld_impuesto_ret * 100

    RETURN ld_impuesto_ret

END FUNCTION


#---------------------------------------------------------------------------#
# f_obten_rfc : Obtiene el rfc del registros liquidado                      #
#---------------------------------------------------------------------------#
FUNCTION f_obten_rfc(pc_nss,pc_consec)

    DEFINE pc_nss LIKE afi_mae_afiliado.n_seguro
    DEFINE pc_consec LIKE ret_beneficiario.consecutivo

    DEFINE lc_rfc  LIKE afi_mae_afiliado.n_rfc
    DEFINE len_rfc SMALLINT 

    LET len_rfc = 0

    -- -----------------------------------------------------------------------------

    SELECT UNIQUE rfc_benef    #CPL-2326
    INTO   lc_rfc
    FROM   ret_beneficiario
    WHERE  nss = pc_nss
    AND    consecutivo = pc_consec
    AND    fecha_captura = (SELECT MAX(b.fecha_captura) FROM ret_beneficiario b
                            WHERE b.nss = pc_nss AND b.consecutivo = pc_consec)

    SELECT UNIQUE LENGTH(rfc_benef)    #CPL-2326
    INTO   len_rfc
    FROM   ret_beneficiario
    WHERE  nss = pc_nss
    AND    consecutivo = pc_consec
    AND    fecha_captura = (SELECT MAX(b.fecha_captura) FROM ret_beneficiario b
                            WHERE b.nss = pc_nss AND b.consecutivo = pc_consec)
   
    IF (lc_rfc IS NULL OR lc_rfc = " " OR len_rfc = 0 ) THEN
        SELECT n_rfc
        INTO   lc_rfc
        FROM   afi_mae_afiliado
        WHERE  n_seguro = pc_nss

        IF lc_rfc IS NULL THEN
            LET lc_rfc = "   "
        END IF
    END IF

    RETURN lc_rfc

END FUNCTION

#---------------------------------------------------------------------------#
# f_insert_tabla : Inserta los valores en la tabla ret_datos_bancoppel      #
#---------------------------------------------------------------------------#
FUNCTION f_insert_tabla(pr_dat_coppel)

    DEFINE pr_dat_coppel RECORD
        folio                 INTEGER                       ,
        tipo_registro         CHAR(001)                     ,
        n_seguro              CHAR(011)                     ,
        nombre                CHAR(040)                     ,
        paterno               CHAR(040)                     ,
        materno               CHAR(040)                     ,
        forma_pago            CHAR(001)                     ,
        clabe                 CHAR(018)                     ,
        fecha_captura         DATE                          ,
        imp_doc_neto          DECIMAL(15,2)                 ,
        imp_doc_antes_impues  DECIMAL(15,2)                 ,
        impuesto_retenido     DECIMAL(11,2)                 ,
        num_fol_serv          CHAR(010)                     ,
        num_tienda            CHAR(004)                     ,
        tp_retiro             CHAR(003)                     ,
        consec_retiro         INTEGER                       ,
        curp                  CHAR(018)                     ,
        rfc                   LIKE afi_mae_afiliado.n_rfc   ,
        estatus               SMALLINT                      ,
        tipo_retiro           CHAR(03)
    END RECORD

    DEFINE lr_datos_bancoppel RECORD LIKE ret_datos_bancoppel.*

    -- -----------------------------------------------------------------------------

    INITIALIZE lr_datos_bancoppel.* TO NULL

    LET lr_datos_bancoppel.folio                = pr_dat_coppel.folio
    LET lr_datos_bancoppel.nss                  = pr_dat_coppel.n_seguro
    LET lr_datos_bancoppel.nombre               = pr_dat_coppel.nombre
    LET lr_datos_bancoppel.paterno              = pr_dat_coppel.paterno
    LET lr_datos_bancoppel.materno              = pr_dat_coppel.materno
    LET lr_datos_bancoppel.forma_pago           = pr_dat_coppel.forma_pago
    LET lr_datos_bancoppel.clabe_o_cta          = pr_dat_coppel.clabe
    LET lr_datos_bancoppel.fecha_captura        = pr_dat_coppel.fecha_captura
    LET lr_datos_bancoppel.monto_neto           = pr_dat_coppel.imp_doc_neto
    LET lr_datos_bancoppel.monto_bruto          = pr_dat_coppel.imp_doc_antes_impues
    LET lr_datos_bancoppel.impuesto_ret         = pr_dat_coppel.impuesto_retenido
    LET lr_datos_bancoppel.folio_serv           = pr_dat_coppel.num_fol_serv
    LET lr_datos_bancoppel.num_tienda           = pr_dat_coppel.num_tienda
    LET lr_datos_bancoppel.tipo_movimiento      = pr_dat_coppel.tp_retiro
    LET lr_datos_bancoppel.consec_retiro        = pr_dat_coppel.consec_retiro
    LET lr_datos_bancoppel.curp                 = pr_dat_coppel.curp
    LET lr_datos_bancoppel.rfc                  = pr_dat_coppel.rfc
    LET lr_datos_bancoppel.estatus              = pr_dat_coppel.estatus
    LET lr_datos_bancoppel.tipo_retiro          = pr_dat_coppel.tipo_retiro
    LET lr_datos_bancoppel.fecha_genera         = HOY
    LET lr_datos_bancoppel.usuario_genera       = gc_usuario

    IF lr_datos_bancoppel.nombre IS NULL THEN
       LET lr_datos_bancoppel.nombre = " "
    END IF

    IF lr_datos_bancoppel.paterno IS NULL THEN
      LET lr_datos_bancoppel.paterno = " "
    END IF

    INSERT INTO ret_datos_bancoppel
    VALUES (lr_datos_bancoppel.*)

END FUNCTION

#---------------------------------------------------------------------------#
# f_datos_disposicion : Obtiene la fecha de captura, el folio de solicitud  #
#                       y el monto total liquidado del registro de          #
#                       disposiciones IMSS                                  #
#---------------------------------------------------------------------------#
FUNCTION f_datos_disposicion(pc_nss, pi_consec)

    DEFINE
        pc_nss      LIKE dis_cuenta.nss             ,
        pi_consec   LIKE dis_cuenta.consecutivo_lote

    DEFINE lr_datos RECORD
         fecha_captura          DATE,
         num_fol_serv           CHAR(010),
         imp_doc_neto           DECIMAL(15,2)
    END RECORD

    -- -----------------------------------------------------------------------------

    --obtencion de fecha_captura (se elige la maxima)
    SELECT  MAX(fecha_captura)
    INTO    lr_datos.fecha_captura
    FROM    ret_solicitud_tx c
    WHERE   c.nss = pc_nss
    AND     c.estado_solicitud = 8

    --obtencion de folio_solicitud
    SELECT folio_solicitud
    INTO   lr_datos.num_fol_serv
    FROM   ret_solicitud_tx c
    WHERE  c.nss           = pc_nss
    AND    c.consecutivo   = pi_consec
    AND    c.fecha_captura = lr_datos.fecha_captura

    --para el importe neto
    SELECT SUM(ROUND(a.monto_en_pesos, 2))
    INTO   lr_datos.imp_doc_neto
    FROM   tmp_dat_dis_cta a    ,
           ret_solicitud_tx c
    WHERE  a.nss              = pc_nss
    AND    a.consecutivo_lote = pi_consec
    AND    a.tipo_movimiento BETWEEN 800 AND 899
    AND    a.consecutivo_lote = c.consecutivo
    AND    a.nss              = c.nss
    AND    c.tipo_retiro IN (SELECT tipo_retiro
                             FROM   tab_tramite_retiro
                             WHERE  cod_tramite = gs_tipo_disposicion
                            )

    -- Validacion del importe neto
    IF lr_datos.imp_doc_neto IS NULL OR lr_datos.imp_doc_neto = " " THEN
        LET lr_datos.imp_doc_neto = 0
    END IF

    LET lr_datos.imp_doc_neto = lr_datos.imp_doc_neto * -100

    RETURN lr_datos.*

END FUNCTION


#---------------------------------------------------------------------------#
# f_datos_disposicion : Obtiene la fecha de captura, el folio de solicitud  #
#                       y el monto total liquidado del registro de          #
#                       disposiciones IMSS                                  #
#---------------------------------------------------------------------------#
FUNCTION f_datos_disposicion_benef(pc_nss, pi_consec)

    DEFINE
        pc_nss      LIKE dis_cuenta.nss             ,
        pi_consec   LIKE dis_cuenta.consecutivo_lote

    DEFINE lr_datos RECORD
         fecha_captura          DATE,
         num_fol_serv           CHAR(010),
         imp_doc_neto           DECIMAL(15,2)
    END RECORD

    DEFINE r_valida SMALLINT

    DEFINE v_compara_benef DECIMAL(22,6)
    DEFINE v_compara_cta   DECIMAL(22,6)
    DEFINE v_consec_padre  LIKE dis_cuenta.consecutivo_lote

    LET r_valida = 1

    --obtencion de fecha_captura (se elige la maxima)
    SELECT  MAX(fecha_captura)
    INTO    lr_datos.fecha_captura
    FROM    ret_solicitud_tx c
    WHERE   c.nss = pc_nss
    AND     c.consecutivo   = pi_consec
    AND     c.estado_solicitud = 8

    --obtencion de folio_solicitud
    SELECT folio_solicitud
    INTO   lr_datos.num_fol_serv
    FROM   ret_solicitud_tx c
    WHERE  c.nss           = pc_nss
    AND    c.consecutivo   = pi_consec
    AND    c.fecha_captura = lr_datos.fecha_captura

    --para el importe neto
    SELECT SUM(ROUND(a.pesos_neto,2))
    INTO   lr_datos.imp_doc_neto
    FROM   ret_ctr_benef_det a
    WHERE  nss               = pc_nss
    AND    consecutivo_solic = pi_consec

    -- Validacion del importe neto
    IF lr_datos.imp_doc_neto IS NULL OR lr_datos.imp_doc_neto = " " THEN
        LET lr_datos.imp_doc_neto = 0
    END IF

    --Obtenemos al padre para validar:
    SELECT DISTINCT consecutivo_padre
       INTO v_consec_padre
       FROM ret_ctr_benef
       WHERE nss = pc_nss
       AND consecutivo_solic = pi_consec

    --Obtiene el total liquidado del padre en dis_cuenta
    SELECT ABS(NVL(SUM(monto_en_acciones),0))
       INTO v_compara_cta
       FROM dis_cuenta
       WHERE nss = pc_nss
       AND consecutivo_lote = v_consec_padre

    SELECT ABS(NVL(SUM(a.acciones_bruto),0))
       INTO v_compara_benef
       FROM ret_ctr_benef_det a,
            ret_ctr_benef     b
       WHERE a.nss = b.nss
       AND   a.consecutivo_solic = b.consecutivo_solic
       AND   a.nss               = pc_nss
       AND   b.consecutivo_padre = v_consec_padre

    IF v_compara_cta <> v_compara_benef THEN
       LET r_valida = 0
    END IF

    LET lr_datos.imp_doc_neto = lr_datos.imp_doc_neto * 100

    RETURN lr_datos.*,r_valida,v_compara_cta,v_compara_benef

END FUNCTION


#---------------------------------------------------------------------------#
# f_datos_issste : Obtiene la fecha de captura, el folio de solicitud y el  #
#                  monto total liquidado del registro de disposiciones y    #
#                  retiros parciales ISSSTE                                 #
#---------------------------------------------------------------------------#
FUNCTION f_datos_issste(pr_sol)

    DEFINE pr_sol RECORD
        nss         LIKE dis_cuenta.nss             ,
        consec      LIKE dis_cuenta.consecutivo_lote,
        tipo_mov    LIKE dis_cuenta.tipo_movimiento
    END RECORD


    DEFINE lr_datos RECORD
         fecha_captura          DATE            ,
         num_fol_serv           CHAR(010)       ,
         imp_doc_neto           DECIMAL(15,2)
    END RECORD

    -- -----------------------------------------------------------------------------

    -- Obtencion de fecha_captura (se elige la maxima), del folio de solicitud
    -- y del monto total

    IF pr_sol.tipo_mov = 856 THEN

        -- Retiros Parciales ISSSTE
        SELECT  MAX(fecha_captura)
        INTO    lr_datos.fecha_captura
        FROM    ret_parcial_issste c
        WHERE   c.nss = pr_sol.nss
        AND     c.consecutivo = pr_sol.consec

        SELECT folio_solicitud
        INTO   lr_datos.num_fol_serv
        FROM   ret_parcial_issste c
        WHERE  c.nss           = pr_sol.nss
        AND    c.consecutivo   = pr_sol.consec
        AND    c.fecha_captura = lr_datos.fecha_captura

        SELECT SUM(ROUND(a.monto_en_pesos, 2))
        INTO   lr_datos.imp_doc_neto
        FROM   tmp_dat_dis_cta a    ,
               ret_parcial_issste c
        WHERE  a.nss              = pr_sol.nss
        AND    a.consecutivo_lote = pr_sol.consec
        AND    a.tipo_movimiento  = pr_sol.tipo_mov
        AND    a.consecutivo_lote = c.consecutivo
        AND    a.nss              = c.nss
    ELSE
        -- Retiros Totales ISSSTE
        --[CPL-3510] Se corrige consulta por nss y consecutivo:
        SELECT MAX(fecha_captura),MAX(folio_solicitud)
        INTO lr_datos.fecha_captura,lr_datos.num_fol_serv
        FROM ret_sol_issste_tx
        WHERE nss = pr_sol.nss
        AND consecutivo = pr_sol.consec

        SELECT SUM(ROUND(a.monto_en_pesos, 2))
        INTO   lr_datos.imp_doc_neto
        FROM   tmp_dat_dis_cta a    ,
               ret_sol_issste_tx c
        WHERE  a.nss              = pr_sol.nss
        AND    a.consecutivo_lote = pr_sol.consec
        AND    a.tipo_movimiento  = pr_sol.tipo_mov
        AND    a.consecutivo_lote = c.consecutivo
        AND    a.nss              = c.nss
        AND    c.tipo_retiro IN (SELECT tipo_retiro
                                 FROM   tab_ret_issste
                                 WHERE  cod_tramite = gs_tipo_disp_issste
                                )
    END IF

    -- Validacion del importe neto
    IF lr_datos.imp_doc_neto IS NULL OR lr_datos.imp_doc_neto = " " THEN
        LET lr_datos.imp_doc_neto = 0
    END IF

    LET lr_datos.imp_doc_neto = lr_datos.imp_doc_neto * -100

    RETURN lr_datos.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_datos_issste_ant : Obtiene la fecha de captura, el folio de solicitud   #
#                      y el monto total liquidado del registro de retiros   #
#                      ISSSTE Modulo anterior                               #
#---------------------------------------------------------------------------#
FUNCTION f_datos_issste_ant(pr_sol)

    DEFINE pr_sol RECORD
        nss         LIKE dis_cuenta.nss             ,
        consec      LIKE dis_cuenta.consecutivo_lote,
        tipo_mov    LIKE dis_cuenta.tipo_movimiento
    END RECORD

    DEFINE lr_datos RECORD
         fecha_captura          DATE            ,
         num_fol_serv           CHAR(010)       ,
         imp_doc_neto           DECIMAL(15,2)
    END RECORD

    -- Obtencion de fecha_captura (se elige la maxima), del folio de solicitud
    -- y del monto total

    -- -----------------------------------------------------------------------------

    IF pr_sol.tipo_mov = 884 OR pr_sol.tipo_mov = 885 THEN

        -- Retiros Parciales ISSSTE
        SELECT  MAX(fecha_captura)
        INTO    lr_datos.fecha_captura
        FROM    ret_sol_issste_par c
        WHERE   c.nss_imss = pr_sol.nss

        SELECT folio_solicitud
        INTO   lr_datos.num_fol_serv
        FROM   ret_sol_issste_par c
        WHERE  c.nss_imss      = pr_sol.nss
        AND    c.consecutivo   = pr_sol.consec
        AND    c.fecha_captura = lr_datos.fecha_captura

        SELECT SUM(ROUND(a.monto_en_pesos, 2))
        INTO   lr_datos.imp_doc_neto
        FROM   tmp_dat_dis_cta a    ,
               ret_sol_issste_par c
        WHERE  a.nss              = pr_sol.nss
        AND    a.consecutivo_lote = pr_sol.consec
        AND    a.tipo_movimiento  = pr_sol.tipo_mov
        AND    a.consecutivo_lote = c.consecutivo
        AND    a.nss              = c.nss_imss
        AND    c.tipo_retiro IN (4,5)
    ELSE
        -- Retiros Totales ISSSTE
        SELECT  MAX(fecha_captura)
        INTO    lr_datos.fecha_captura
        FROM    ret_sol_issste_tot c
        WHERE   c.nss_imss = pr_sol.nss

        SELECT folio_sol
        INTO   lr_datos.num_fol_serv
        FROM   ret_sol_issste_tot c
        WHERE  c.nss_imss      = pr_sol.nss
        AND    c.consecutivo   = pr_sol.consec
        AND    c.fecha_captura = lr_datos.fecha_captura

        SELECT SUM(ROUND(a.monto_en_pesos, 2))
        INTO   lr_datos.imp_doc_neto
        FROM   tmp_dat_dis_cta a    ,
               ret_sol_issste_tot c
        WHERE  a.nss              = pr_sol.nss
        AND    a.consecutivo_lote = pr_sol.consec
        AND    a.tipo_movimiento  = pr_sol.tipo_mov
        AND    a.consecutivo_lote = c.consecutivo
        AND    a.nss              = c.nss_imss
        AND    c.tipo_retiro IN (1,2,3,6,7,8,9)
    END IF

    -- Validacion del importe neto
    IF lr_datos.imp_doc_neto IS NULL OR lr_datos.imp_doc_neto = " " THEN
        LET lr_datos.imp_doc_neto = 0
    END IF

    LET lr_datos.imp_doc_neto = lr_datos.imp_doc_neto * -100

    RETURN lr_datos.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_datos_parcial : Obtiene la fecha de captura, el folio de solicitud y el #
#                   monto total liquidado del registro de retiros parciales #
#                   IMSS                                                    #
#---------------------------------------------------------------------------#
FUNCTION f_datos_parcial(pc_nss, pi_consec)

    DEFINE
        pc_nss      LIKE dis_cuenta.nss             ,
        pi_consec   LIKE dis_cuenta.consecutivo_lote

    DEFINE lr_datos RECORD
         fecha_captura          DATE,
         num_fol_serv           CHAR(010),
         imp_doc_neto           DECIMAL(15,2)
    END RECORD
    DEFINE lr_datos_par RECORD
         tipo_retiro            CHAR(1),
         tipo_prestacion        SMALLINT,
         tipo_pago              SMALLINT
    END RECORD 
    
    --obtencion de fecha_captura (se elige la maxima)
    SELECT  MAX(fecha_captura)
    INTO    lr_datos.fecha_captura
    FROM    ret_parcial d
    WHERE   d.nss = pc_nss
    AND     d.estado_solicitud IN(gr_edo.liquidado, gr_edo.enviaop16, gr_edo.recibeop16)

    --obtencion de folio_solicitud
    LET lr_datos.num_fol_serv    = NULL
    INITIALIZE lr_datos_par TO NULL
    
    SELECT folio_solicitud, tipo_retiro, tipo_prestacion, tipo_pago
    INTO   lr_datos.num_fol_serv, lr_datos_par.tipo_retiro, lr_datos_par.tipo_prestacion, lr_datos_par.tipo_pago
    FROM   ret_parcial d
    WHERE  d.nss              = pc_nss
    AND    d.consecutivo      = pi_consec
    AND    d.fecha_captura    = lr_datos.fecha_captura
    AND     d.estado_solicitud IN(gr_edo.liquidado, gr_edo.enviaop16, gr_edo.recibeop16)

    IF lr_datos_par.tipo_retiro = 'I' AND lr_datos_par.tipo_prestacion = 6 AND lr_datos_par.tipo_pago >= 4 THEN
       SELECT folio_rec INTO lr_datos.num_fol_serv
       FROM   ret_parcialidad_des d
       WHERE  d.nss              = pc_nss
       AND    d.consecutivo      = pi_consec
       AND    d.consec_pago      = (SELECT MAX(consec_pago)
                                    FROM   ret_parcialidad_des
                                    WHERE  nss         = pc_nss
                                    AND    consecutivo = pi_consec
                                    AND    estado      = gr_edo.liquidado) 
    END IF 
    
    --para el importe neto
    SELECT SUM(ROUND(a.monto_en_pesos, 2))
    INTO   lr_datos.imp_doc_neto
    FROM   tmp_dat_dis_cta a,
           ret_parcial d
    WHERE  a.nss              = pc_nss
    AND    a.consecutivo_lote = pi_consec
    AND    a.tipo_movimiento IN (870, 875, 876, 877, 878)
    AND    a.consecutivo_lote = d.consecutivo
    AND    a.nss              = d.nss

    -- Validacion del importe neto
    IF lr_datos.imp_doc_neto IS NULL OR lr_datos.imp_doc_neto = " " THEN
        LET lr_datos.imp_doc_neto = 0
    END IF

    LET lr_datos.imp_doc_neto = lr_datos.imp_doc_neto * -100

    RETURN lr_datos.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_datos_pmg : Obtiene la fecha de captura, el folio de solicitud y el     #
#               monto total liquidado del registro de Pension Minima        #
#               Garantizada                                                 #
#---------------------------------------------------------------------------#
FUNCTION f_datos_pmg(pc_nss, pi_consec)

    DEFINE pc_nss             LIKE dis_cuenta.nss             
    DEFINE pi_consec          LIKE dis_cuenta.consecutivo_lote

    DEFINE lr_datos           RECORD
           fecha_captura      DATE,
           num_fol_serv       CHAR(010),
           imp_doc_neto       DECIMAL(15,2)
           END RECORD

    -- -----------------------------------------------------------------------------

    --obtencion de fecha_captura (se elige la maxima)
    SELECT  MAX(fecha_captura)
    INTO    lr_datos.fecha_captura
    FROM    pen_solicitud_pmg d
    WHERE   d.nss = pc_nss
    AND     d.consecutivo = pi_consec

    --obtencion de folio_solicitud
    SELECT folio_solicitud
    INTO   lr_datos.num_fol_serv
    FROM   pen_solicitud_pmg d
    WHERE  d.nss              = pc_nss
    AND    d.consecutivo      = pi_consec
    AND    d.fecha_captura    = lr_datos.fecha_captura
    AND    d.estado_solicitud IN (gr_edo_pmg.en_proceso_pago ,
                                  gr_edo_pmg.liquidado
                                 )

    --para el importe neto
    SELECT SUM(ROUND(a.monto_en_pesos, 2))
    INTO   lr_datos.imp_doc_neto
    FROM   tmp_dat_dis_cta a,
           pen_solicitud_pmg d
    WHERE  a.nss              = pc_nss
    AND    a.consecutivo_lote = pi_consec
    AND    a.tipo_movimiento  = 841
    AND    a.consecutivo_lote = d.consecutivo
    AND    a.nss              = d.nss

    -- Validacion del importe neto
    IF lr_datos.imp_doc_neto IS NULL OR lr_datos.imp_doc_neto = " " THEN
        LET lr_datos.imp_doc_neto = 0
    END IF

    LET lr_datos.imp_doc_neto = lr_datos.imp_doc_neto * -100

    RETURN lr_datos.*

END FUNCTION

#==============================================================================#
# f_datos_pmg_iss : Obtiene la fecha de captura, el folio de solicitud y el    #
#               monto total liquidado del registro de Pension Minima           #
#               Garantizada                                                    #
#==============================================================================#
FUNCTION f_datos_pmg_iss(pc_nss, pi_consec)

    DEFINE pc_nss             LIKE dis_cuenta.nss             
    DEFINE pi_consec          LIKE dis_cuenta.consecutivo_lote

    DEFINE lr_datos           RECORD
           fecha_captura      DATE,
           num_fol_serv       CHAR(010),
           imp_doc_neto       DECIMAL(15,2)
           END RECORD

    -- -----------------------------------------------------------------------------

    --obtencion de fecha_captura (se elige la maxima)
    SELECT  MAX(fecha_captura)
    INTO    lr_datos.fecha_captura
    FROM    pen_solicitud_iss d
    WHERE   d.nss = pc_nss

    --obtencion de folio_solicitud
    SELECT folio_solicitud
    INTO   lr_datos.num_fol_serv
    FROM   pen_solicitud_iss d
    WHERE  d.nss              = pc_nss
    AND    d.consecutivo      = pi_consec
    AND    d.fecha_captura    = lr_datos.fecha_captura
    AND    d.estado_solicitud IN (gr_edo_pmg.en_proceso_pago ,
                                  gr_edo_pmg.liquidado
                                 )

    --para el importe neto
    SELECT SUM(ROUND(a.monto_en_pesos, 2))
    INTO   lr_datos.imp_doc_neto
    FROM   tmp_dat_dis_cta a,
           pen_solicitud_iss d
    WHERE  a.nss              = pc_nss
    AND    a.consecutivo_lote = pi_consec
    AND    a.tipo_movimiento  IN (857, 859)
    AND    a.consecutivo_lote = d.consecutivo
    AND    a.nss              = d.nss

    -- Validacion del importe neto
    IF lr_datos.imp_doc_neto IS NULL OR lr_datos.imp_doc_neto = " " THEN
        LET lr_datos.imp_doc_neto = 0
    END IF

    LET lr_datos.imp_doc_neto = lr_datos.imp_doc_neto * -100

    RETURN lr_datos.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_datos_voluntarias : Obtiene la fecha de captura, el folio de solicitud  #
#                       y el monto total liquidado del registro de          #
#                       Voluntarias                                         #
#---------------------------------------------------------------------------#
FUNCTION f_datos_voluntarias(pc_nss, pi_consec)

    DEFINE
        pc_nss      LIKE dis_cuenta.nss             ,
        pi_consec   LIKE dis_cuenta.consecutivo_lote

    DEFINE lr_datos RECORD
         num_fol_serv           CHAR(010),
         imp_doc_neto           DECIMAL(15,2)
    END RECORD

    -- -----------------------------------------------------------------------------

    --obtencion de folio_solicitud
    SELECT  n_folio_sol
    INTO    lr_datos.num_fol_serv
    FROM    ret_cta_vol d
    WHERE   d.n_seguro      = pc_nss
    AND     d.fecha_captura = (SELECT MAX(fecha_captura)
                               FROM   ret_cta_vol d
                               WHERE  d.n_seguro = pc_nss
                                )
    AND    d.consecutivo = pi_consec

    --para el importe neto
    SELECT SUM(ROUND(a.monto_en_pesos, 2))
    INTO   lr_datos.imp_doc_neto
    FROM   tmp_dat_dis_cta  a,
           ret_cta_vol d
    WHERE  a.nss              = pc_nss
    AND    a.consecutivo_lote = pi_consec
    AND    a.tipo_movimiento  IN (490,493)
    AND    a.consecutivo_lote = d.consecutivo
    AND    a.nss              = d.n_seguro

    -- Validacion del importe neto
    IF lr_datos.imp_doc_neto IS NULL OR lr_datos.imp_doc_neto = " " THEN
        LET lr_datos.imp_doc_neto = 0
    END IF

    LET lr_datos.imp_doc_neto = lr_datos.imp_doc_neto * -100

    RETURN lr_datos.*

END FUNCTION
#---------------------------------------------------------------------------#
# f_datos_complemen  :Obtiene la fecha de captura, el folio de solicitud    #
#                       y el monto total liquidado del registro de          #
#                       complementarias                cpl-1902             #
#---------------------------------------------------------------------------#
FUNCTION f_datos_complemen(pc_nss, pi_consec)

    DEFINE
        pc_nss      LIKE dis_cuenta.nss             ,
        pi_consec   LIKE dis_cuenta.consecutivo_lote

    DEFINE lr_datos RECORD
         num_fol_serv           CHAR(010),
         imp_doc_neto           DECIMAL(15,2)
    END RECORD

    -- -----------------------------------------------------------------------------

    --obtencion de folio_solicitud
    SELECT  n_folio_sol
    INTO    lr_datos.num_fol_serv
    FROM    ret_cta_vol d
    WHERE   d.n_seguro      = pc_nss
    AND     d.fecha_captura = (SELECT MAX(fecha_captura)
                               FROM   ret_cta_vol d
                               WHERE  d.n_seguro = pc_nss
                                )
    AND    d.consecutivo = pi_consec

    --para el importe neto
    SELECT SUM(ROUND(a.monto_en_pesos, 2))
    INTO   lr_datos.imp_doc_neto
    FROM   tmp_dat_dis_cta  a,
           ret_cta_vol d
    WHERE  a.nss              = pc_nss
    AND    a.consecutivo_lote = pi_consec
    AND    a.tipo_movimiento  = 897
    AND    a.consecutivo_lote = d.consecutivo
    AND    a.nss              = d.n_seguro

    -- Validacion del importe neto
    IF lr_datos.imp_doc_neto IS NULL OR lr_datos.imp_doc_neto = " " THEN
        LET lr_datos.imp_doc_neto = 0
    END IF

    LET lr_datos.imp_doc_neto = lr_datos.imp_doc_neto * -100

    RETURN lr_datos.*

END FUNCTION
#---------------------------------------------------------------------------#
# f_carga_bancoppel : Ejecuta los pasos para realizar la carga del archivo  #
#---------------------------------------------------------------------------#
FUNCTION f_carga_bancoppel()

    -- El folio se inicializa ya que se obtiene en f_carga_archivo
    LET gi_ult_folio = 0

    CALL f_tablas_tmp()

    IF (f_carga_archivo() = TRUE) THEN
        DISPLAY " CARGANDO ARCHIVO ... " AT 18,1 ATTRIBUTE(REVERSE)
        CALL f_act_datos(gi_ult_folio)
    END IF

    CLEAR SCREEN
    CLOSE WINDOW RETM8212

END FUNCTION

#---------------------------------------------------------------------------#
# f_tablas_tmp : Genera las tablas temporales para la carga del archivo     #
#---------------------------------------------------------------------------#
FUNCTION f_tablas_tmp()

    WHENEVER ERROR CONTINUE

    DROP TABLE tmp_bancopel
    CREATE TEMP TABLE tmp_bancopel
    (
    n_registros          CHAR(310)
    )

    WHENEVER ERROR STOP

END FUNCTION

#---------------------------------------------------------------------------#
# f_carga_archivo : Captura el folio y el nombre del archivo y carga este   #
#                   a la tabla tempral sin formato para ser procesado       #
#---------------------------------------------------------------------------#
FUNCTION f_carga_archivo()

    DEFINE lr_carga RECORD
        nom_archivo     CHAR(30),
        folio           INTEGER
    END RECORD

    DEFINE
        lc_ruta_archivo         CHAR(200)

    DEFINE
        ls_procesa              SMALLINT

    -- -----------------------------------------------------------------------------

    OPEN WINDOW RETM8212 AT 2,2 WITH FORM "RETM8212"  -- ATTRIBUTE(BORDER)
    DISPLAY "                             < Ctrl-C > Salir                                          " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETM821            CARGA ARCHIVO DE RESPUESTA BANCOPEL                                " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,69 ATTRIBUTE(REVERSE)

    LET gi_num_regs     = 0
    LET ls_procesa      = FALSE

    SELECT MAX(folio)
    INTO   lr_carga.folio
    FROM   ret_datos_bancoppel

    INPUT BY NAME lr_carga.* WITHOUT DEFAULTS

        AFTER FIELD nom_archivo
            IF lr_carga.nom_archivo IS NULL THEN
                CALL f_lib_error_msg("NOMBRE DE ARCHIVO NO PUEDE SER NULO")
                NEXT FIELD nom_archivo
            END IF

            SELECT "OK"
            FROM   ret_cza_resol
            WHERE  nom_archivo = lr_carga.nom_archivo
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
                CALL f_lib_error_msg("ARCHIVO YA PROCESADO CON ANTERIORIDAD")
                NEXT FIELD nom_archivo
            END IF

        AFTER FIELD folio
            IF lr_carga.folio IS NULL THEN
                CALL f_lib_error_msg("FOLIO NO PUEDE SER NULO")
                NEXT FIELD folio
            END IF

            SELECT "OK"
            FROM   ret_datos_bancoppel
            WHERE  folio = lr_carga.folio
            GROUP BY 1

            IF STATUS = NOTFOUND THEN
                CALL f_lib_error_msg("FOLIO INEXISTENTE")
                NEXT FIELD folio
            END IF

        ON KEY (ESC)
            IF lr_carga.folio IS NULL THEN
                CALL f_lib_error_msg("FOLIO NO PUEDE SER NULO")
                NEXT FIELD folio
            END IF

            SELECT "OK"
            FROM   ret_datos_bancoppel
            WHERE  folio = lr_carga.folio
            GROUP BY 1

            IF STATUS = NOTFOUND THEN
                CALL f_lib_error_msg("FOLIO INEXISTENTE")
                NEXT FIELD folio
            END IF

            IF lr_carga.nom_archivo IS NULL THEN
                CALL f_lib_error_msg("NOMBRE DE ARCHIVO NO PUEDE SER NULO")
                NEXT FIELD nom_archivo
            END IF

            SELECT "OK"
            FROM   ret_cza_resol
            WHERE  nom_archivo = lr_carga.nom_archivo
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
                CALL f_lib_error_msg("ARCHIVO YA PROCESADO CON ANTERIORIDAD")
                NEXT FIELD nom_archivo
            END IF

            LET lc_ruta_archivo = gs_modulo.ruta_rescate CLIPPED,"/",
                                  lr_carga.nom_archivo CLIPPED

            WHENEVER ERROR CONTINUE

            LOAD FROM lc_ruta_archivo DELIMITER "+"
            INSERT INTO tmp_bancopel

            WHENEVER ERROR STOP

            SELECT COUNT(*)
            INTO   gi_num_regs
            FROM   tmp_bancopel

            IF gi_num_regs = 0 THEN
                CALL f_lib_error_msg("NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO")
                NEXT FIELD nom_archivo
            ELSE
                IF f_lib_pregunta("¿DESEA CARGAR EL ARCHIVO? (S/N) ") = TRUE THEN
                    LET ls_procesa      = TRUE
                    LET gi_ult_folio    = lr_carga.folio

                    -- Registramos la carga del archivo
                    INSERT INTO ret_cza_resol
                    VALUES (gi_ult_folio            ,
                            HOY                     ,
                            0                       ,
                            0                       ,
                            lr_carga.nom_archivo    ,
                            HOY                     ,
                            gc_usuario              ,
                            1)
                ELSE
                    CALL f_lib_error_msg("CARGA CANCELADA")
                END IF

                EXIT INPUT
            END IF

        ON KEY(CONTROL-C, INTERRUPT)
            LET ls_procesa  = FALSE
            CALL f_lib_error_msg("CARGA CANCELADA")
            EXIT INPUT

     END INPUT

    RETURN ls_procesa

END FUNCTION

#---------------------------------------------------------------------------#
# f_act_datos : Actualiza la tabla ret_datos_bancoppel con los datos        #
#               obtenidos desde el archivo cargado                          #
#---------------------------------------------------------------------------#
FUNCTION f_act_datos(pi_folio)

    DEFINE pi_folio LIKE ret_datos_bancoppel.folio

    DEFINE lr_bancoppel RECORD LIKE ret_datos_bancoppel.*

    DEFINE
        ls_num                SMALLINT

    DEFINE
        li_actualiza          INTEGER

    DEFINE
        carga_reg             CHAR(310)

    DEFINE
        ldt_habil_sig         DATE

    -- -----------------------------------------------------------------------------

    DISPLAY "                               " AT 18,2

    LET ls_num          = 1
    LET li_actualiza    = 0

    DECLARE cur_tmp CURSOR FOR
    SELECT *
    FROM   tmp_bancopel

    FOREACH cur_tmp INTO carga_reg

        IF carga_reg[001,001] = "" THEN
            ERROR "  ERROR, ARCHIVO VACIO "
            EXIT FOREACH
        ELSE
            IF carga_reg[001,001] = "D" THEN
                LET lr_bancoppel.nss        = carga_reg[002,013]
                LET lr_bancoppel.estatus    = carga_reg[254,255]
                LET lr_bancoppel.folio_pago = carga_reg[256,271]

                SELECT "OK"
                FROM   ret_datos_bancoppel
                WHERE  nss            = lr_bancoppel.nss
                AND    folio          = pi_folio
                GROUP BY 1

                IF STATUS <> NOTFOUND THEN
                    UPDATE ret_datos_bancoppel
                    SET    estatus          = lr_bancoppel.estatus      ,
                           folio_pago       = lr_bancoppel.folio_pago   ,
                           fecha_recibe     = HOY                       ,
                           usuario_recibe   = gc_usuario
                    WHERE  nss              = lr_bancoppel.nss
                    AND    folio            = pi_folio

                    LET li_actualiza = li_actualiza + 1
                END IF
            END IF
        END IF
    END FOREACH

    -- Si la fecha de recepcion del archivo es la misma que la de generacion,
    -- el dia del primer pago es el dia habil siguiente a la fecha en curso.
    -- En otro caso es la fecha actual

    SELECT UNIQUE(fecha_genera)
    INTO   lr_bancoppel.fecha_genera
    FROM   ret_datos_bancoppel
    WHERE  folio = pi_folio


    IF lr_bancoppel.fecha_genera = HOY THEN

        EXECUTE eje_dia_sig USING HOY   ,
                                  ls_num
                            INTO  ldt_habil_sig
    ELSE
        LET ldt_habil_sig = HOY
    END IF

    UPDATE ret_datos_bancoppel
    SET    fecha_caja_coppel = ldt_habil_sig
    WHERE  folio             = pi_folio
    AND    estatus           NOT IN (2,20)
    AND    fecha_recibe IS NOT NULL

    UPDATE ret_cza_resol
    SET    total_registros  = gi_num_regs - 2
    WHERE  folio            = pi_folio

    DISPLAY "TOTAL DE REGISTROS PROCESADOS       : ",gi_num_regs AT 12,9
    LET gi_num_regs = gi_num_regs - 2
    DISPLAY "TOTAL DE REGISTROS DE DETALLE       : ",gi_num_regs AT 13,9
    DISPLAY "TOTAL DE REGISTROS ACTUALIZADOS     : ",li_actualiza AT 14,9

    PROMPT " PROCESO FINALIZADO. PRESIONE <ENTER> PARA REGRESAR AL MENU "
    FOR CHAR enter

END FUNCTION

--[CPL-3381]
FUNCTION f_lib_evalua_cadenas_enie(p_cadena)

   DEFINE p_cadena     CHAR(100),
          p_cadena_sal CHAR(100),
          l_x          INTEGER,
          l_y          INTEGER,
          l_car        CHAR(1)

   DEFINE enie_add     SMALLINT
          
   LET l_car = ""
   LET enie_add = 0
   LET l_y = 1
   
   LET p_cadena = p_cadena CLIPPED
   
   FOR l_x = 1 TO LENGTH(p_cadena)
   
      LET l_car = p_cadena[l_x]
      
      IF l_car NOT MATCHES "[a-z|A-Z|0-9]" AND l_car <> " " THEN
         IF enie_add = 0 THEN
            LET p_cadena_sal[l_y] = "Ñ"
            LET enie_add = 1
            LET l_y = l_y + 1
         END IF
      ELSE
         LET p_cadena_sal[l_y] = p_cadena[l_x]
         LET l_y = l_y + 1
      END IF
   END FOR
   
   IF p_cadena_sal = "" OR p_cadena_sal = " " OR p_cadena_sal IS NULL THEN
      LET p_cadena_sal = ""
   ELSE
      LET p_cadena_sal = p_cadena_sal CLIPPED
   END IF
   
   RETURN p_cadena_sal
   
END FUNCTION

-- ----------------------------------------------------------------------------

#---------------------------------------------------------------------------#
# rpt_encabezado : Genera el encabezado del archivo de bancoppel            #
#---------------------------------------------------------------------------#
REPORT rpt_encabezado(lr_cza_coppel)

    DEFINE lr_cza_coppel RECORD
        tipo_registro CHAR(01) ,
        num_cont_emp  CHAR(08) ,
        fecha_gen     DATE     ,
        fecha_ini     DATE     ,
        fecha_fin     DATE     ,
        num_movs      INTEGER
    END RECORD

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
            PRINT
                COLUMN 001,lr_cza_coppel.tipo_registro                     ,
                COLUMN 002,lr_cza_coppel.num_cont_emp                      ,
                COLUMN 010,lr_cza_coppel.fecha_gen USING "MMDDYYYY"        ,
                COLUMN 018,lr_cza_coppel.fecha_ini USING "MMDDYYYY"        ,
                COLUMN 026,lr_cza_coppel.fecha_fin USING "MMDDYYYY"        ,
                COLUMN 034,lr_cza_coppel.num_movs  USING "&&&&&&&&&"       ,
                COLUMN 043,258 SPACES
END REPORT

#---------------------------------------------------------------------------#
# rpt_detalle : Genera el detalle del archivo de bancoppel                  #
#---------------------------------------------------------------------------#
REPORT rpt_detalle(lr_det_coppel)

    DEFINE lr_det_coppel RECORD #loc #lr_det_coppel
        tipo_registro         CHAR(001)                     ,
        n_seguro              CHAR(011)                     ,
        nombre                CHAR(040)                     ,
        paterno               CHAR(040)                     ,
        materno               CHAR(040)                     ,
        forma_pago            CHAR(001)                     ,
        clabe                 CHAR(018)                     ,
        fecha_captura         DATE                          ,
        imp_doc_neto          DECIMAL(15,2)                 ,
        imp_doc_antes_impues  DECIMAL(15,2)                 ,
        impuesto_retenido     DECIMAL(11,2)                 ,
        num_fol_serv          CHAR(010)                     ,
        num_tienda            CHAR(004)                     ,
        tp_retiro             CHAR(003)                     ,
        consec_retiro         INTEGER                       ,
        curp                  CHAR(018)                     ,
        rfc                   LIKE afi_mae_afiliado.n_rfc   ,
        estatus               SMALLINT
    END RECORD

    DEFINE
        lc_rfc10        CHAR(10)


    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW

            LET lc_rfc10 = lr_det_coppel.rfc[1,10]

            IF (lr_det_coppel.num_tienda = "" OR lr_det_coppel.num_tienda IS NULL) THEN    #CPL-2307
                LET lr_det_coppel.num_tienda = "0000"
            END IF

            PRINT
                COLUMN 001,lr_det_coppel.tipo_registro                                    ,
                COLUMN 002,lr_det_coppel.n_seguro                                         ,
                COLUMN 013,lr_det_coppel.nombre                                           ,
                COLUMN 053,lr_det_coppel.paterno                                          ,
                COLUMN 093,lr_det_coppel.materno                                          ,
                COLUMN 133,lr_det_coppel.forma_pago                                       ,
                COLUMN 134,lr_det_coppel.clabe                                            ,
                COLUMN 152,lr_det_coppel.fecha_captura        USING "MMDDYYYY"            ,
                COLUMN 160,lr_det_coppel.imp_doc_neto         USING "&&&&&&&&&&&&&&&"     ,
                COLUMN 175,lr_det_coppel.imp_doc_antes_impues USING "&&&&&&&&&&&&&&&"     ,
                COLUMN 190,lr_det_coppel.impuesto_retenido    USING "&&&&&&&&&&&"         ,
                COLUMN 201,lr_det_coppel.num_fol_serv         USING "&&&&&&&&"            ,
                COLUMN 209,lr_det_coppel.num_tienda           USING "&&&&"                ,
                COLUMN 213,lr_det_coppel.tp_retiro            USING "&&&"                 ,
                COLUMN 216,lr_det_coppel.consec_retiro        USING "&&&&&&&&&&"          ,
                COLUMN 226,lr_det_coppel.curp                                             ,
                COLUMN 244,lc_rfc10                                                       ,
                COLUMN 254,lr_det_coppel.estatus             USING "&&"                   ,
                COLUMN 256,45 SPACES
END REPORT

#---------------------------------------------------------------------------#
# rpt_sumario : Genera el sumario del archivo de bancoppel                  #
#---------------------------------------------------------------------------#
REPORT rpt_sumario(lr_sum_coppel)

    DEFINE lr_sum_coppel RECORD #loc #lr_sum_coppel
        tipo_registro         CHAR(01)        ,
        num_tot_movs          INTEGER         ,
        imp_tot_neto          DECIMAL(17, 2)  ,
        imp_tot_sin_impues    DECIMAL(17, 2)  ,
        imp_retenido          DECIMAL(17, 2)  ,
        imp_tot_retiros_efec  DECIMAL(17, 2)  ,
        imp_tot_retiros_depos DECIMAL(17, 2)
    END RECORD

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
            PRINT
                COLUMN 001,lr_sum_coppel.tipo_registro                                  ,
                COLUMN 002,lr_sum_coppel.num_tot_movs          USING"&&&&&&&&&"         ,
                COLUMN 011,lr_sum_coppel.imp_tot_neto          USING"&&&&&&&&&&&&&&&&&" ,
                COLUMN 028,lr_sum_coppel.imp_tot_sin_impues    USING"&&&&&&&&&&&&&&&&&" ,
                COLUMN 045,lr_sum_coppel.imp_retenido          USING"&&&&&&&&&&&&&&&&&" ,
                COLUMN 062,lr_sum_coppel.imp_tot_retiros_efec  USING"&&&&&&&&&&&&&&&&&" ,
                COLUMN 079,lr_sum_coppel.imp_tot_retiros_depos USING"&&&&&&&&&&&&&&&&&" ,
                COLUMN 096,205 SPACES
END REPORT


#---------------------------------------------------------------------------#
# rpt_diferencia : Genera reporte con los consecutivos que tienen diferencias en Beneficiarios designados #
#---------------------------------------------------------------------------#
REPORT rpt_diferencia(p_folio,p_nss,p_consecutivo,p_monto,p_monto_benef,p_proceso)

   DEFINE p_folio       DECIMAL(11,0)
   DEFINE p_nss         CHAR(11)
   DEFINE p_consecutivo DECIMAL(11,0)
   DEFINE p_monto       DECIMAL(22,6)
   DEFINE p_monto_benef DECIMAL(22,6)
   DEFINE p_proceso     CHAR(10)

    OUTPUT
        PAGE LENGTH   80
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
       PAGE HEADER
          PRINT " RETM821 - ARCHIVO DE DIFERENCIAS DE MONTOS ",TODAY USING "dd/mm/yyyy"
          SKIP 1 LINE 
    
       ON EVERY ROW
            PRINT
                COLUMN 001,
                "FOLIO: ",p_folio,
                "|NSS: ",p_nss,
                "|CONSECUTIVO: ",p_consecutivo,
                "|MONTO: ",p_monto,
                "|MONTO BENEFICIARIO: ",p_monto_benef,
                "|ARCHIVO: ",p_proceso

END REPORT

-- ----------------------------------------------------------------------------

#---------------------------------------------------------------------------#
# rpt_encabezado : Genera el encabezado del archivo de bancoppel            #
#---------------------------------------------------------------------------#
REPORT rpt_encabezado_otros(lr_cza_coppel)

    DEFINE lr_cza_coppel RECORD
        tipo_registro CHAR(01) ,
        num_cont_emp  CHAR(08) ,
        fecha_gen     DATE     ,
        fecha_ini     DATE     ,
        fecha_fin     DATE     ,
        num_movs      INTEGER
    END RECORD

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
            PRINT
                COLUMN 001,lr_cza_coppel.tipo_registro                     ,
                COLUMN 002,lr_cza_coppel.num_cont_emp                      ,
                COLUMN 010,lr_cza_coppel.fecha_gen USING "MMDDYYYY"        ,
                COLUMN 018,lr_cza_coppel.fecha_ini USING "MMDDYYYY"        ,
                COLUMN 026,lr_cza_coppel.fecha_fin USING "MMDDYYYY"        ,
                COLUMN 034,lr_cza_coppel.num_movs  USING "&&&&&&&&&"       ,
                COLUMN 043,258 SPACES
END REPORT

#---------------------------------------------------------------------------#
# rpt_detalle : Genera el detalle del archivo de bancoppel                  #
#---------------------------------------------------------------------------#
REPORT rpt_detalle_otros(lr_det_coppel)

    DEFINE lr_det_coppel RECORD #loc #lr_det_coppel
        tipo_registro         CHAR(001)                     ,
        n_seguro              CHAR(011)                     ,
        nombre                CHAR(040)                     ,
        paterno               CHAR(040)                     ,
        materno               CHAR(040)                     ,
        forma_pago            CHAR(001)                     ,
        clabe                 CHAR(018)                     ,
        fecha_captura         DATE                          ,
        imp_doc_neto          DECIMAL(15,2)                 ,
        imp_doc_antes_impues  DECIMAL(15,2)                 ,
        impuesto_retenido     DECIMAL(11,2)                 ,
        num_fol_serv          CHAR(010)                     ,
        num_tienda            CHAR(004)                     ,
        tp_retiro             CHAR(003)                     ,
        consec_retiro         INTEGER                       ,
        curp                  CHAR(018)                     ,
        rfc                   LIKE afi_mae_afiliado.n_rfc   ,
        estatus               SMALLINT
    END RECORD

    DEFINE
        lc_rfc10        CHAR(10)


    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW

            LET lc_rfc10 = lr_det_coppel.rfc[1,10]

            IF (lr_det_coppel.num_tienda = "" OR lr_det_coppel.num_tienda IS NULL) THEN    #CPL-2307
                LET lr_det_coppel.num_tienda = "0000"
            END IF

            PRINT
                COLUMN 001,lr_det_coppel.tipo_registro                                    ,
                COLUMN 002,lr_det_coppel.n_seguro                                         ,
                COLUMN 013,lr_det_coppel.nombre                                           ,
                COLUMN 053,lr_det_coppel.paterno                                          ,
                COLUMN 093,lr_det_coppel.materno                                          ,
                COLUMN 133,lr_det_coppel.forma_pago                                       ,
                COLUMN 134,lr_det_coppel.clabe                                            ,
                COLUMN 152,lr_det_coppel.fecha_captura        USING "MMDDYYYY"            ,
                COLUMN 160,lr_det_coppel.imp_doc_neto         USING "&&&&&&&&&&&&&&&"     ,
                COLUMN 175,lr_det_coppel.imp_doc_antes_impues USING "&&&&&&&&&&&&&&&"     ,
                COLUMN 190,lr_det_coppel.impuesto_retenido    USING "&&&&&&&&&&&"         ,
                COLUMN 201,lr_det_coppel.num_fol_serv         USING "&&&&&&&&"            ,
                COLUMN 209,lr_det_coppel.num_tienda           USING "&&&&"                ,
                COLUMN 213,lr_det_coppel.tp_retiro            USING "&&&"                 ,
                COLUMN 216,lr_det_coppel.consec_retiro        USING "&&&&&&&&&&"          ,
                COLUMN 226,lr_det_coppel.curp                                             ,
                COLUMN 244,lc_rfc10                                                       ,
                COLUMN 254,lr_det_coppel.estatus             USING "&&"                   ,
                COLUMN 256,45 SPACES
END REPORT

#---------------------------------------------------------------------------#
# rpt_sumario : Genera el sumario del archivo de bancoppel                  #
#---------------------------------------------------------------------------#
REPORT rpt_sumario_otros(lr_sum_coppel)

    DEFINE lr_sum_coppel RECORD #loc #lr_sum_coppel
        tipo_registro         CHAR(01)        ,
        num_tot_movs          INTEGER         ,
        imp_tot_neto          DECIMAL(17, 2)  ,
        imp_tot_sin_impues    DECIMAL(17, 2)  ,
        imp_retenido          DECIMAL(17, 2)  ,
        imp_tot_retiros_efec  DECIMAL(17, 2)  ,
        imp_tot_retiros_depos DECIMAL(17, 2)
    END RECORD

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
            PRINT
                COLUMN 001,lr_sum_coppel.tipo_registro                                  ,
                COLUMN 002,lr_sum_coppel.num_tot_movs          USING"&&&&&&&&&"         ,
                COLUMN 011,lr_sum_coppel.imp_tot_neto          USING"&&&&&&&&&&&&&&&&&" ,
                COLUMN 028,lr_sum_coppel.imp_tot_sin_impues    USING"&&&&&&&&&&&&&&&&&" ,
                COLUMN 045,lr_sum_coppel.imp_retenido          USING"&&&&&&&&&&&&&&&&&" ,
                COLUMN 062,lr_sum_coppel.imp_tot_retiros_efec  USING"&&&&&&&&&&&&&&&&&" ,
                COLUMN 079,lr_sum_coppel.imp_tot_retiros_depos USING"&&&&&&&&&&&&&&&&&" ,
                COLUMN 096,205 SPACES
END REPORT
