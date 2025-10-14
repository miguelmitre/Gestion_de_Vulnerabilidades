################################################################################
#Proyecto          => SISTEMA DE AFORE( SAFRE )                                #
#Owner             => E.F.P.                                                   #
#Programa RETC848  => GENERA ARCHIVO INFORMACION DE RETIROS AFORE COPPEL       #
#                                                                              #
#Fecha creacion    => 19 OCTUBRE 2007                                          #
#By                => MIREYA TOLEDO PANIAGUA                                   #
#Modificado por    => JAVIER GONZALEZ JERONIMO                                 #
#Fecha actualiza   => 20 DE JUNIO DE 2008                                      #
#                  => Modificaciones para direccionar correctamente los movs   #
#                     en dis_cuenta de acuerdo al tipo de retiro realizado     #
#Modificado por    => JAVIER GONZALEZ JERONIMO                                 #
#Fecha actualiza   => 07 DE JULIO DE 2009                                      #
#                  => Modificaciones a los querys y se eliminan partes que ya  #
#                  => no eran necesarias                                       #
#Modificado por    => JAVIER GONZALEZ JERONIMO                                 #
#Fecha actualiza   => 08 DE SEPTIEMBRE DE 2009                                 #
#                  => Se agregan los retiros issste en la generacion del       #
#                  => archivo de cajas                                         #
#Sistema           => RETIROS                                                  #
#------------------------------------------------------------------------------#
#Modificado por    => Phelippe R Santos                                        #
#                  => Se modifica el query de parciales                        #
#Fecha actualiza   => 29 ABRIL DE 2015        CPL-1963                         #
################################################################################
DATABASE safre_af

    DEFINE gr_edo RECORD
        liquidado             LIKE ret_estado.estado_solicitud,
        enviaop16             LIKE ret_estado.estado_solicitud,    #CPL-2021
        recibeop16            LIKE ret_estado.estado_solicitud
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
        tipo_registro         CHAR(001)       ,
        n_seguro              CHAR(011)       ,
        nombre                CHAR(040)       ,
        paterno               CHAR(040)       ,
        materno               CHAR(040)       ,
        forma_pago            CHAR(001)       ,
        clabe                 CHAR(018)       ,
        fecha_captura         DATE            ,
        imp_doc_neto          DECIMAL(15,2)   ,
        imp_doc_antes_impues  DECIMAL(15,2)   ,
        impuesto_retenido     DECIMAL(11,2)   ,
        num_fol_serv          CHAR(008)       ,
        num_tienda            CHAR(004)       ,
        tp_retiro             CHAR(003)       ,
        consec_retiro         INTEGER         ,
        curp                  CHAR(018)
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
        gc_ruta                     CHAR(100) ,
        G_LISTA_1                   CHAR(100) ,
        G_LISTA_2                   CHAR(100) ,
        G_LISTA_3                   CHAR(100) ,
        enter                       CHAR(001) ,
        cat                         CHAR(300) ,
        ch                          CHAR(100)

    DEFINE
        gdt_cambio_infonavit    ,
        gdt_fec_ini              ,
        gdt_fec_fin              ,
        HOY                     DATE

    DEFINE
        gs_flag                 ,
        gs_tipo_disposicion     ,
        gs_mov_viv97            SMALLINT

    DEFINE
        gi_total_movs           INTEGER

#####################################################################

MAIN
    DEFER INTERRUPT

    OPTIONS
        INPUT WRAP          ,
        PROMPT LINE LAST    ,
        ACCEPT KEY CONTROL-I

    CALL f_lib_crea_log()
    CALL init()

    OPEN WINDOW RETC8481 AT 2,2 WITH FORM "RETC8481" ATTRIBUTE(BORDER,FORM LINE 1)
    DISPLAY " RETC8481          GENERA INFORMACION RETIROS AFORE-COPPEL                   " AT 3,1 ATTRIBUTE(REVERSE)
    --DISPLAY " <ESC> - Ejecutar                                          <CTRL-C> - Salir  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

    CALL f_captura_fechas() RETURNING gdt_fec_ini   ,
                                      gdt_fec_fin   ,
                                      gs_flag

    IF (gs_flag = TRUE) THEN

        IF f_lib_pregunta("¿DESEA GENERAR EL REPORTE? (S/N) ") = TRUE THEN

            -- Genera la tabla temporal de dis_cuenta
            CALL f_genera_tabla_tmp(gdt_fec_ini, gdt_fec_fin)
            CALL f_genera_caja_coppel()
            CALL f_lib_error_msg("PROCESO FINALIZADO CORRECTAMENTE")
        ELSE
            CALL f_lib_error_msg("PROCESO CANCELADO")
        END IF
    ELSE
        CALL f_lib_error_msg("PROCESO CANCELADO")
    END IF

    CLOSE WINDOW RETC8481

END MAIN

#########################################################################
FUNCTION init()

    -- -----------------------------------------------------------------------------

    INITIALIZE
        gdt_fec_ini         ,
        gdt_fec_fin         TO NULL

    LET HOY                     = TODAY
    LET gdt_cambio_infonavit    = MDY(01,12,2012)

    ----- TIPOS DE TRAMITE -----
    SELECT cod_tramite
    INTO   gs_tipo_disposicion
    FROM   tab_tipo_tramite
    WHERE  descripcion = "DISPOSICION"

    SELECT codigo
    INTO   gs_mov_viv97
    FROM   tab_movimiento
    WHERE  descripcion  = "RETIRO DISPOSICION VIVIENDA 97 REG 73"

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

    SELECT ruta_envio
    INTO   gc_ruta
    FROM   seg_modulo
    WHERE  modulo_cod = "int"

--    LET gc_ruta = "/home/efp2/ret"

    LET gr_cza_coppel.num_movs      = 0
    LET gi_total_movs               = 0
    LET gs_flag                     = 0
    LET gd_tot_neto                 = 0
    LET gd_tot_sin_impues           = 0
    LET gd_tot_retenido             = 0
    LET gd_tot_retiros_efec         = 0
    LET gd_tot_retiros_depos        = 0

END FUNCTION

#---------------------------------------------------------------------------#
# f_captura_fechas : Captura el intervalo de fechas de donde se tomaran los #
#                    datos para generar el archivo de caja coppel           #
#---------------------------------------------------------------------------#
FUNCTION f_captura_fechas()

    DEFINE lr_dat RECORD
        fecha_conversion_ini    DATE    ,
        fecha_conversion_fin    DATE
    END RECORD

    DEFINE
        ls_flag     SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_flag = TRUE
    LET lr_dat.fecha_conversion_fin  =  HOY
    LET lr_dat.fecha_conversion_ini  =  HOY

    DISPLAY BY NAME lr_dat.fecha_conversion_fin
    DISPLAY BY NAME lr_dat.fecha_conversion_ini

    INPUT BY NAME lr_dat.* WITHOUT DEFAULTS

        AFTER FIELD fecha_conversion_fin

            IF (lr_dat.fecha_conversion_ini IS NULL) THEN
                CALL f_lib_error_msg("LA FECHA INICIAL NO PUEDE SER NULA")
                NEXT FIELD fecha_conversion_ini
            END IF

            IF (lr_dat.fecha_conversion_fin IS NULL) THEN
                CALL f_lib_error_msg("LA FECHA FINAL NO PUEDE SER NULA")
                NEXT FIELD fecha_conversion_fin
            END IF

            IF ( (lr_dat.fecha_conversion_ini > lr_dat.fecha_conversion_fin) OR
                 (lr_dat.fecha_conversion_fin > HOY)
               )
            THEN
                CALL f_lib_error_msg("LA FECHA INICIAL NO PUEDE SER MAYOR A LA FECHA FINAL")
                NEXT FIELD fecha_conversion_ini
            END IF

        ON KEY(ESC)
            IF (lr_dat.fecha_conversion_ini IS NULL) THEN
                CALL f_lib_error_msg("LA FECHA INICIAL NO PUEDE SER NULA")
                NEXT FIELD fecha_conversion_ini
            END IF

            IF (lr_dat.fecha_conversion_fin IS NULL) THEN
                CALL f_lib_error_msg("LA FECHA FINAL NO PUEDE SER NULA")
                NEXT FIELD fecha_conversion_fin
            END IF

            IF ( (lr_dat.fecha_conversion_ini > lr_dat.fecha_conversion_fin) OR
                 (lr_dat.fecha_conversion_fin > HOY)
               )
            THEN
                CALL f_lib_error_msg("LA FECHA INICIAL NO PUEDE SER MAYOR A LA FECHA FINAL")
                NEXT FIELD fecha_conversion_ini
            END IF

            EXIT INPUT

        ON KEY(CONTROL-C, INTERRUPT)
            LET ls_flag = FALSE
            EXIT INPUT

    END INPUT

    RETURN lr_dat.*, ls_flag

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_caja_coppel : Ejecuta los pasos para generar el archivo de caja  #
#                        coppel                                             #
#---------------------------------------------------------------------------#
FUNCTION f_genera_caja_coppel()

    DEFINE
        lc_archivo_gen          CHAR(100)   ,
        lc_nombre_plano         CHAR(100)

    -- -----------------------------------------------------------------------------

    CALL genera_det_coppel()
    CALL genera_cza_coppel()
    CALL genera_sum_coppel()

    LET lc_nombre_plano = "Cajas", HOY USING"YYMMDD", ".txt"
    LET lc_nombre_plano = lc_nombre_plano CLIPPED

    LET lc_archivo_gen  = gc_ruta CLIPPED, "/", lc_nombre_plano
    LET lc_archivo_gen  = lc_archivo_gen CLIPPED

    LET cat = "cat ", G_LISTA_1 CLIPPED, " ",
                      G_LISTA_2 CLIPPED, " ",
                      G_LISTA_3 CLIPPED, " > ",
                      gc_ruta CLIPPED, "/", lc_nombre_plano
    RUN cat

    LET ch = "chmod 777 ",lc_archivo_gen
    RUN ch

    DISPLAY "                           " AT 19,2
    DISPLAY "EL ARCHIVO HA SIDO GENERADO EN LA RUTA : " AT 11,18
    DISPLAY gc_ruta CLIPPED AT 12,18
    DISPLAY "CON EL NOMBRE : ", lc_nombre_plano AT 14,18
    DISPLAY "REGISTROS PROCESADOS : ", gi_total_movs  AT 16,18

END FUNCTION

FUNCTION genera_cza_coppel()
#gcc--------------------------

    LET gr_cza_coppel.tipo_registro      = "E"
    LET gr_cza_coppel.num_cont_emp       = "12345678"
    LET gr_cza_coppel.fecha_gen          = HOY
    LET gr_cza_coppel.fecha_ini          = gdt_fec_ini
    LET gr_cza_coppel.fecha_fin          = gdt_fec_fin
    LET gr_cza_coppel.num_movs           = gi_total_movs

    LET G_LISTA_1 = gc_ruta CLIPPED,"/CST"

    START REPORT rpt_encabezado TO G_LISTA_1
        OUTPUT TO REPORT rpt_encabezado(gr_cza_coppel.*) #1
    FINISH REPORT rpt_encabezado

    LET ch = "chmod 777 ",G_LISTA_1
    RUN ch

END FUNCTION

FUNCTION genera_det_coppel()
#gdc--------------------------

    DEFINE
        lc_bandera          CHAR(10),
        ls_tot_neto_benef   DECIMAL(15,2)

    DISPLAY "GENERANDO DETALLE ..." AT 19,2

    LET gr_det_coppel.tipo_registro = "D"

    LET G_LISTA_2 = gc_ruta CLIPPED,"/DST"
    START REPORT rpt_detalle TO G_LISTA_2

    DECLARE cur_1 CURSOR FOR
        SELECT *
        FROM   tmp_datos_caja_int
        ORDER BY nss

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

        LET gr_det_coppel.impuesto_retenido = f_calc_impuesto_retenido(gr_det_coppel.n_seguro     ,
                                                                       gr_det_coppel.consec_retiro)

        CASE lc_bandera
            WHEN "dis"
                CALL f_datos_disposicion(gr_det_coppel.n_seguro, gr_det_coppel.consec_retiro)
                    RETURNING gr_det_coppel.fecha_captura,
                              gr_det_coppel.num_fol_serv ,
                              gr_det_coppel.imp_doc_neto
                              
                --Valida beneficiario designado y si tiene mas de una solicitud CPL-3342 
                IF f_beneficiarios_designados(gr_det_coppel.*,lc_bandera) THEN 
                    LET gr_det_coppel.imp_doc_neto      = f_importes_beneficiairos(gr_det_coppel.n_seguro, gr_det_coppel.consec_retiro) 
                    LET gr_det_coppel.impuesto_retenido = f_importe_retenido_benef(gr_det_coppel.n_seguro, gr_det_coppel.consec_retiro) 
                END IF

            WHEN "iss"
                CALL f_datos_issste(gr_det_coppel.n_seguro      ,
                                    gr_det_coppel.consec_retiro ,
                                    gr_det_coppel.tp_retiro     )
                    RETURNING gr_det_coppel.fecha_captura,
                              gr_det_coppel.num_fol_serv ,
                              gr_det_coppel.imp_doc_neto
                              
                --Valida beneficiario designado y si tiene mas de una solicitud CPL-3342 
                IF f_beneficiarios_designados(gr_det_coppel.*,lc_bandera) THEN 
                    LET gr_det_coppel.imp_doc_neto      = f_importes_beneficiairos(gr_det_coppel.n_seguro, gr_det_coppel.consec_retiro) 
                    LET gr_det_coppel.impuesto_retenido = f_importe_retenido_benef(gr_det_coppel.n_seguro, gr_det_coppel.consec_retiro) 
                END IF

            WHEN "par"
                CALL f_datos_parcial(gr_det_coppel.n_seguro, gr_det_coppel.consec_retiro)
                    RETURNING gr_det_coppel.fecha_captura,
                              gr_det_coppel.num_fol_serv ,
                              gr_det_coppel.imp_doc_neto

                --Valida beneficiario designado y si tiene mas de una solicitud CPL-3342 
                IF f_beneficiarios_designados(gr_det_coppel.*,lc_bandera) THEN 
                    LET gr_det_coppel.imp_doc_neto      = f_importes_beneficiairos(gr_det_coppel.n_seguro, gr_det_coppel.consec_retiro) 
                    LET gr_det_coppel.impuesto_retenido = f_importe_retenido_benef(gr_det_coppel.n_seguro, gr_det_coppel.consec_retiro) 
                END IF

            WHEN "vol"
                LET gr_det_coppel.fecha_captura = gdt_fec_ini

                CALL f_datos_voluntarias(gr_det_coppel.n_seguro, gr_det_coppel.consec_retiro, gr_det_coppel.tp_retiro)
                    RETURNING gr_det_coppel.num_fol_serv ,
                              gr_det_coppel.imp_doc_neto
                              
                --Valida beneficiario designado y si tiene mas de una solicitud CPL-3342 
                IF f_beneficiarios_designados(gr_det_coppel.*,lc_bandera) THEN 
                    LET gr_det_coppel.imp_doc_neto      = f_importes_beneficiairos(gr_det_coppel.n_seguro, gr_det_coppel.consec_retiro) 
                    LET gr_det_coppel.impuesto_retenido = f_importe_retenido_benef(gr_det_coppel.n_seguro, gr_det_coppel.consec_retiro) 
                END IF
        END CASE

        LET gr_det_coppel.imp_doc_antes_impues = gr_det_coppel.imp_doc_neto + gr_det_coppel.impuesto_retenido


        --Acumula totales
        LET gi_total_movs           = gi_total_movs + 1
        LET gd_tot_neto             = gd_tot_neto + gr_det_coppel.imp_doc_neto
        LET gd_tot_sin_impues       = gd_tot_sin_impues + gr_det_coppel.imp_doc_antes_impues
        LET gd_tot_retenido         = gd_tot_retenido + gr_det_coppel.impuesto_retenido
        LET gd_tot_retiros_efec     = 0
        LET gd_tot_retiros_depos    = 0

        DISPLAY "PROCESANDO REGISTRO : ", gi_total_movs  AT 11,18

        OUTPUT TO REPORT rpt_detalle(gr_det_coppel.*) #2

    END FOREACH

    FINISH REPORT rpt_detalle
    LET ch = "chmod 777 ",G_LISTA_2
    RUN ch

    --Archivo de diferencias de importes CPL-3342
    CALL f_genera_archivo_diferencias()

END FUNCTION

FUNCTION genera_sum_coppel()
#gsc--------------------------

    LET gr_sum_coppel.tipo_registro         = "S"
    LET gr_sum_coppel.num_tot_movs          = gi_total_movs
    LET gr_sum_coppel.imp_tot_neto          = gd_tot_neto
    LET gr_sum_coppel.imp_tot_sin_impues    = gd_tot_sin_impues
    LET gr_sum_coppel.imp_retenido          = gd_tot_retenido
    LET gr_sum_coppel.imp_tot_retiros_efec  = gd_tot_neto
    LET gr_sum_coppel.imp_tot_retiros_depos = gd_tot_retiros_depos

    LET G_LISTA_3 = gc_ruta CLIPPED,"/SST"

    START REPORT rpt_sumario TO G_LISTA_3
        OUTPUT TO REPORT rpt_sumario(gr_sum_coppel.*) #3
    FINISH REPORT rpt_sumario

    LET ch = "chmod 777 ",G_LISTA_3
    RUN ch

END FUNCTION


FUNCTION f_genera_tabla_tmp(pr_fechas)

    DEFINE pr_fechas RECORD
        fecha_ini  DATE,
        fecha_fin  DATE
    END RECORD

    -- -----------------------------------------------------------------------------

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_dat_dis_cta_int
        DROP TABLE tmp_datos_caja_int
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
    #AND    subcuenta NOT IN (30,31,32,33,34,35,36)               #CPL-2345
    AND   (   tipo_movimiento = 490               -- Voluntarias
           OR tipo_movimiento = 493               -- Voluntarias APP
           OR tipo_movimiento = 10                -- Retencion de ISR
           OR tipo_movimiento BETWEEN 800 AND 899 -- Retiros IMSS e ISSTE
           )
    INTO TEMP tmp_dat_dis_cta_int

    CREATE INDEX tmp_cta_data_01
    ON tmp_dat_dis_cta_int (tipo_movimiento, consecutivo_lote, nss)

    UPDATE STATISTICS FOR TABLE tmp_dat_dis_cta_int

    CALL f_elimina_disp_E73()

    -- -----------------------------------------------------------------------------

    SELECT  unique(a.nss) nss   , -- Disposiciones
            b.nombres           ,
            b.paterno           ,
            b.materno           ,
            b.tienda_cod        ,
            b.consecutivo       ,
            b.num_cuenta        ,
            b.tipo_pago         ,
            c.curp              ,
            a.tipo_movimiento   ,
            "dis"  bandera
    FROM    tmp_dat_dis_cta_int a   ,
            ret_beneficiario b  ,
            ret_solicitud_tx c
    WHERE   (a.tipo_movimiento  BETWEEN 800 AND 899
             AND a.tipo_movimiento NOT BETWEEN 881 AND 889)
    AND     a.consecutivo_lote = b.consecutivo
    AND     a.consecutivo_lote = c.consecutivo
    AND     a.nss = b.nss
    AND     a.nss = c.nss
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
            "iss" bandera
    FROM    tmp_dat_dis_cta_int a   ,
            ret_beneficiario b  ,
            ret_sol_issste_tx c
    WHERE   (a.tipo_movimiento IN (851,852,853,854,855,858,864,867)
             AND a.tipo_movimiento NOT IN (856) )
    AND     a.consecutivo_lote = b.consecutivo
    AND     a.consecutivo_lote = c.consecutivo
    AND     a.nss = b.nss
    AND     a.nss = c.nss
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
            "iss" bandera
    FROM    tmp_dat_dis_cta_int a   ,
            ret_beneficiario b  ,
            ret_parcial_issste c
    WHERE   a.tipo_movimiento IN (856)
    AND     a.consecutivo_lote = b.consecutivo
    AND     a.consecutivo_lote = c.consecutivo
    AND     a.nss = b.nss
    AND     a.nss = c.nss
    UNION
    SELECT  unique(a.nss) nss   , -- Parciales
            b.nombres           ,
            b.paterno           ,
            b.materno           ,
            b.tienda_cod        ,
            b.consecutivo       ,
            b.num_cuenta        ,
            b.tipo_pago         ,
            d.curp              ,
            a.tipo_movimiento   ,
            "par" bandera
    FROM    tmp_dat_dis_cta_int a   ,
            ret_beneficiario b  ,
            ret_parcial d
    WHERE   a.tipo_movimiento  IN (870, 875, 876, 877, 878)
    AND     a.consecutivo_lote = b.consecutivo
    AND     a.consecutivo_lote = d.consecutivo                    --CPL-1963
    AND     a.nss = b.nss
    AND     a.nss = d.nss
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
            "vol" bandera
    FROM    tmp_dat_dis_cta_int a   ,
            ret_beneficiario b  ,
            ret_cta_vol c
    WHERE   a.tipo_movimiento  IN (490,493,897)
    AND     a.consecutivo_lote = b.consecutivo
    AND     a.consecutivo_lote = c.consecutivo
    AND     a.nss = b.nss
    AND     a.nss = c.n_seguro
    ORDER BY a.nss
    INTO TEMP tmp_datos_caja_int

    CREATE INDEX tmp_datos_caja_01
    ON tmp_datos_caja_int (nss)

    UPDATE STATISTICS FOR TABLE tmp_datos_caja_int

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_diferencia_imp_neto_benef
        CREATE TEMP TABLE tmp_diferencia_imp_neto_benef
        (
             folio             INTEGER,
             nss               CHAR(11),
             consecutivo       DECIMAL(11,0),
             monto_pesos       DECIMAL(20,6),
             monto_pesos_benef DECIMAL(20,6),
             mod_retiro        CHAR(20)
        )
    WHENEVER ERROR STOP

END FUNCTION

#---------------------------------------------------------------------------#
# f_elimina_disp_E73 : Elimina los registros de dis_cuenta correspondientes #
#                      a la liquidacion de las disposiciones tipo E regimen #
#                      73 cuya fecha de resolucion sea mayor al 12 de enero #
#                      de 2012                                              #
#---------------------------------------------------------------------------#
FUNCTION f_elimina_disp_E73()

    DEFINE lr_elimina RECORD
        nss         LIKE dis_cuenta.nss             ,
        folio       LIKE dis_cuenta.folio           ,
        consecutivo LIKE dis_cuenta.consecutivo_lote
    END RECORD

    DECLARE cur_elimina CURSOR FOR
       SELECT nss               ,
              folio             ,
              consecutivo_lote
       FROM   tmp_dat_dis_cta_int
       WHERE  tipo_movimiento  = gs_mov_viv97
       AND    subcuenta       IN (4,8)
       AND    nss IN (SELECT B.nss
                      FROM   tmp_dat_dis_cta_int  A   ,
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
        FROM   tmp_dat_dis_cta_int
        WHERE  nss              = lr_elimina.nss
        AND    folio            = lr_elimina.folio
        AND    consecutivo_lote = lr_elimina.consecutivo
        AND    subcuenta        IN (4,8)
    END FOREACH

END FUNCTION



FUNCTION f_calc_impuesto_retenido(pc_nss, pi_consec)

    DEFINE
        pc_nss      LIKE dis_cuenta.nss             ,
        pi_consec   LIKE dis_cuenta.consecutivo_lote


    DEFINE
        ld_impuesto_ret     DECIMAL(11,2)

    --query para el importe retenido:
    SELECT SUM(ROUND(a.monto_en_pesos, 2))
    INTO   ld_impuesto_ret
    FROM   tmp_dat_dis_cta_int a
    WHERE  a.nss              = pc_nss
    AND    a.consecutivo_lote = pi_consec
    AND    a.tipo_movimiento  = 10

    IF ld_impuesto_ret IS NULL OR ld_impuesto_ret = " " THEN
        LET ld_impuesto_ret = 0
    END IF

    LET ld_impuesto_ret = ld_impuesto_ret * -100

    RETURN ld_impuesto_ret

END FUNCTION


FUNCTION f_datos_disposicion(pc_nss, pi_consec)

    DEFINE
        pc_nss      LIKE dis_cuenta.nss             ,
        pi_consec   LIKE dis_cuenta.consecutivo_lote

    DEFINE lr_datos RECORD
         fecha_captura          DATE,
         num_fol_serv           CHAR(008),
         imp_doc_neto           DECIMAL(15,2)
    END RECORD

    --obtencion de fecha_captura (se elige la maxima)
    SELECT  MAX(fecha_captura)
    INTO    lr_datos.fecha_captura
    FROM    ret_solicitud_tx c
    WHERE   c.nss = pc_nss

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
    FROM   tmp_dat_dis_cta_int a    ,
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


FUNCTION f_datos_issste(pr_sol)

    DEFINE pr_sol RECORD
        nss         LIKE dis_cuenta.nss             ,
        consec      LIKE dis_cuenta.consecutivo_lote,
        tipo_mov    LIKE dis_cuenta.tipo_movimiento
    END RECORD


    DEFINE lr_datos RECORD
         fecha_captura          DATE            ,
         num_fol_serv           CHAR(008)       ,
         imp_doc_neto           DECIMAL(15,2)
    END RECORD

    -- Obtencion de fecha_captura (se elige la maxima), del folio de solicitud
    -- y del monto total

    IF pr_sol.tipo_mov = 856 THEN

        -- Retiros Parciales ISSSTE
        SELECT  MAX(fecha_captura)
        INTO    lr_datos.fecha_captura
        FROM    ret_parcial_issste c
        WHERE   c.nss = pr_sol.nss

        SELECT folio_solicitud
        INTO   lr_datos.num_fol_serv
        FROM   ret_parcial_issste c
        WHERE  c.nss      = pr_sol.nss
        AND    c.consecutivo   = pr_sol.consec
        AND    c.fecha_captura = lr_datos.fecha_captura

        SELECT SUM(ROUND(a.monto_en_pesos, 2))
        INTO   lr_datos.imp_doc_neto
        FROM   tmp_dat_dis_cta_int a    ,
               ret_parcial_issste c
        WHERE  a.nss              = pr_sol.nss
        AND    a.consecutivo_lote = pr_sol.consec
        AND    a.tipo_movimiento  = pr_sol.tipo_mov
        AND    a.consecutivo_lote = c.consecutivo
        AND    a.nss              = c.nss
        #AND    c.tipo_retiro IN (4,5)
    ELSE
        -- Retiros Totales ISSSTE
        SELECT  MAX(fecha_captura)
        INTO    lr_datos.fecha_captura
        FROM    ret_sol_issste_tx c
        WHERE   c.nss = pr_sol.nss

        SELECT folio_solicitud
        INTO   lr_datos.num_fol_serv
        FROM   ret_sol_issste_tx c
        WHERE  c.nss           = pr_sol.nss
        AND    c.consecutivo   = pr_sol.consec
        AND    c.fecha_captura = lr_datos.fecha_captura

        SELECT SUM(ROUND(a.monto_en_pesos, 2))
        INTO   lr_datos.imp_doc_neto
        FROM   tmp_dat_dis_cta_int a    ,
               ret_sol_issste_tx   c
        WHERE  a.nss              = pr_sol.nss
        AND    a.consecutivo_lote = pr_sol.consec
        AND    a.tipo_movimiento  = pr_sol.tipo_mov
        AND    a.consecutivo_lote = c.consecutivo
        AND    a.nss              = c.nss
        #AND    c.tipo_retiro IN (1,2,3,6,7,8,9)
    END IF

    -- Validacion del importe neto
    IF lr_datos.imp_doc_neto IS NULL OR lr_datos.imp_doc_neto = " " THEN
        LET lr_datos.imp_doc_neto = 0
    END IF

    LET lr_datos.imp_doc_neto = lr_datos.imp_doc_neto * -100

    RETURN lr_datos.*

END FUNCTION


FUNCTION f_datos_parcial(pc_nss, pi_consec)

    DEFINE
        pc_nss      LIKE dis_cuenta.nss             ,
        pi_consec   LIKE dis_cuenta.consecutivo_lote

    DEFINE lr_datos RECORD
         fecha_captura          DATE,
         num_fol_serv           CHAR(008),
         imp_doc_neto           DECIMAL(15,2)
    END RECORD,
         lc_num_fol_serv        CHAR(8)

    --obtencion de fecha_captura (se elige la maxima)
    SELECT  MAX(fecha_captura)
    INTO    lr_datos.fecha_captura
    FROM    ret_parcial d
    WHERE   d.nss = pc_nss
    AND     d.estado_solicitud IN (gr_edo.liquidado, gr_edo.enviaop16, gr_edo.recibeop16)

    --obtencion de folio_solicitud
    LET lr_datos.num_fol_serv = NULL
    SELECT folio_solicitud INTO lr_datos.num_fol_serv
    FROM   ret_parcial d
    WHERE  d.nss              = pc_nss
    AND    d.consecutivo      = pi_consec
    AND    d.fecha_captura    = lr_datos.fecha_captura
    AND     d.estado_solicitud IN (gr_edo.liquidado, gr_edo.enviaop16, gr_edo.recibeop16)

#CPL-2021 INI
      LET lc_num_fol_serv = NULL
        SELECT folio_rec INTO lc_num_fol_serv
        FROM   ret_parcialidad_des d
        WHERE  d.nss         = pc_nss
        AND    d.consecutivo = pi_consec
        AND    d.estado      = gr_edo.liquidado
        AND    d.consec_pago = (SELECT MAX(consec_pago)
                                FROM   ret_parcialidad_des x
                                WHERE  x.nss         = pc_nss
                                AND    x.consecutivo = pi_consec
                                AND    x.estado      = gr_edo.liquidado)

      #Si encuentra folio de parcialidades, se asigna dicho folio
      IF lc_num_fol_serv IS NOT NULL THEN
         LET lr_datos.num_fol_serv  = lc_num_fol_serv
      END IF
#CPL-2021 FIN

    --para el importe neto
    SELECT SUM(ROUND(a.monto_en_pesos, 2))
    INTO   lr_datos.imp_doc_neto
    FROM   tmp_dat_dis_cta_int a,
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


FUNCTION f_datos_voluntarias(pc_nss, pi_consec, ps_tipo_movto)

    DEFINE
        pc_nss        LIKE dis_cuenta.nss             ,
        pi_consec     LIKE dis_cuenta.consecutivo_lote,
        ps_tipo_movto SMALLINT 

    DEFINE lr_datos RECORD
         num_fol_serv           CHAR(008),
         imp_doc_neto           DECIMAL(15,2)
    END RECORD

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
    IF ps_tipo_movto = 897 THEN 
        SELECT SUM(ROUND(a.monto_en_pesos, 2))
        INTO   lr_datos.imp_doc_neto
        FROM   tmp_dat_dis_cta_int  a,
               ret_cta_vol d
        WHERE  a.nss              = pc_nss
        AND    a.consecutivo_lote = pi_consec
        AND    a.tipo_movimiento  IN (897)
        AND    a.consecutivo_lote = d.consecutivo
        AND    a.nss              = d.n_seguro
    ELSE
        SELECT SUM(ROUND(a.monto_en_pesos, 2))
        INTO   lr_datos.imp_doc_neto
        FROM   tmp_dat_dis_cta_int  a,
               ret_cta_vol d
        WHERE  a.nss              = pc_nss
        AND    a.consecutivo_lote = pi_consec
        AND    a.tipo_movimiento  IN (490,493)
        AND    a.consecutivo_lote = d.consecutivo
        AND    a.nss              = d.n_seguro
    END IF 
    -- Validacion del importe neto
    IF lr_datos.imp_doc_neto IS NULL OR lr_datos.imp_doc_neto = " " THEN
        LET lr_datos.imp_doc_neto = 0
    END IF

    LET lr_datos.imp_doc_neto = lr_datos.imp_doc_neto * -100

    RETURN lr_datos.*

END FUNCTION


REPORT rpt_encabezado(lr_cza_coppel)
#1---------------------------------
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
                COLUMN 043,218 SPACES
END REPORT

REPORT rpt_detalle(lr_det_coppel)
#2---------------- -----------------
    DEFINE lr_det_coppel RECORD #loc #lr_det_coppel
        tipo_registro         CHAR(001)     ,
        n_seguro              CHAR(011)     ,
        nombre                CHAR(040)     ,
        paterno               CHAR(040)     ,
        materno               CHAR(040)     ,
        forma_pago            CHAR(001)     ,
        clabe                 CHAR(018)     ,
        fecha_captura         DATE          ,
        imp_doc_neto          DECIMAL(15,2) ,
        imp_doc_antes_impues  DECIMAL(15,2) ,
        impuesto_retenido     DECIMAL(11,2) ,
        num_fol_serv          CHAR(008)     ,
        num_tienda            CHAR(004)     ,
        tp_retiro             CHAR(003)     ,
        consec_retiro         INTEGER       ,
        curp                  CHAR(018)
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
                COLUMN 251,10 SPACES
END REPORT

REPORT rpt_sumario(lr_sum_coppel)
#3---------------------------------
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
                COLUMN 096,165 SPACES
END REPORT


#-----------------------------------------------------------------------------------------#
# f_beneficiarios_designados : Valida beneficiarios designado y mas de una solicitud      #
#-----------------------------------------------------------------------------------------#
FUNCTION f_beneficiarios_designados(gr_det_benef)
    DEFINE gr_det_benef RECORD
        tipo_registro         CHAR(001)       ,
        n_seguro              CHAR(011)       ,
        nombre                CHAR(040)       ,
        paterno               CHAR(040)       ,
        materno               CHAR(040)       ,
        forma_pago            CHAR(001)       ,
        clabe                 CHAR(018)       ,
        fecha_captura         DATE            ,
        imp_doc_neto          DECIMAL(15,2)   ,
        imp_doc_antes_impues  DECIMAL(15,2)   ,
        impuesto_retenido     DECIMAL(11,2)   ,
        num_fol_serv          CHAR(008)       ,
        num_tienda            CHAR(004)       ,
        tp_retiro             CHAR(003)       ,
        consec_retiro         INTEGER         ,
        curp                  CHAR(018)       ,
        lc_archivo            CHAR(10)
    END RECORD
    
    DEFINE
        v_tot_solicitud   INTEGER, 
        v_bandera         SMALLINT,
        ls_tot_neto_benef DECIMAL(15,2)

    LET v_tot_solicitud = 0
    LET v_bandera       = FALSE  

    SELECT 'OK' 
    FROM ret_ctr_benef 
    WHERE nss               = gr_det_benef.n_seguro
    AND   consecutivo_solic = gr_det_benef.consec_retiro
    AND   folio_liquida     = gr_det_benef.num_fol_serv
    --AND   tipo_benef        = 3 --Beneficiario Designado 

    --Beneficiario designado  
    IF STATUS = NOTFOUND THEN 
        
        SELECT COUNT(folio_solicitud) 
        INTO v_tot_solicitud 
        FROM ret_solicitud_tx 
        WHERE nss           = gr_det_benef.n_seguro 
        AND folio_solicitud = gr_det_benef.num_fol_serv

        --Mas de una solicitud para los beneficiarios 
        IF v_tot_solicitud > 1 THEN
            CALL f_importes_beneficiairos(gr_det_benef.n_seguro, gr_det_benef.consec_retiro)  RETURNING ls_tot_neto_benef

            IF ls_tot_neto_benef <> gr_det_benef.imp_doc_neto THEN 
                CALL f_diferencias_importes(gr_det_benef.n_seguro, gr_det_benef.consec_retiro,ls_tot_neto_benef,gr_det_benef.imp_doc_neto,gr_det_benef.lc_archivo,gr_det_benef.num_fol_serv)
            END IF 

            LET v_bandera = TRUE 
            
        END IF 

    END IF 

    RETURN v_bandera 
END FUNCTION

#---------------------------------------------------------------------------#
# f_importes_beneficiairos : Calcula montos de beneficiarios                #
#---------------------------------------------------------------------------#
FUNCTION f_importes_beneficiairos(pc_nss, pi_consec) 
    DEFINE
        pc_nss          LIKE dis_cuenta.nss ,
        pi_consec       LIKE dis_cuenta.consecutivo_lote

    DEFINE 
        imp_neto_benef  DECIMAL(15,2)

    SELECT SUM(ROUND(pesos_neto, 2))
    INTO   imp_neto_benef
    FROM   ret_ctr_benef_det 
    WHERE  nss               = pc_nss
    AND    consecutivo_solic = pi_consec

    IF imp_neto_benef IS NULL OR imp_neto_benef = ' ' THEN 
        LET imp_neto_benef = 0 
    END IF 
    
    RETURN imp_neto_benef
END FUNCTION 

#---------------------------------------------------------------------------#
# f_importe_retenido_benef : Calcula montos de beneficiarios retenido       #
#---------------------------------------------------------------------------#
FUNCTION f_importe_retenido_benef(pc_nss, pi_consec) 
    DEFINE
        pc_nss          LIKE dis_cuenta.nss ,
        pi_consec       LIKE dis_cuenta.consecutivo_lote,
        pc_num_fol_serv LIKE dis_cuenta.folio

    DEFINE 
        imp_ret_benef    DECIMAL(15,2)

    SELECT SUM(ROUND(pesos_isr, 2))
    INTO   imp_ret_benef
    FROM   ret_ctr_benef_det 
    WHERE  nss               = pc_nss
    AND    consecutivo_solic = pi_consec

    IF imp_ret_benef = ' '  OR imp_ret_benef IS NULL THEN 
        LET imp_ret_benef = 0
    END IF 
    
    RETURN imp_ret_benef
END FUNCTION 


#---------------------------------------------------------------------------#
# f_diferencias_importes : Inserta en tmp de detalle montos incosistencias  #
#---------------------------------------------------------------------------#
FUNCTION f_diferencias_importes(pc_nss, pi_consec,p_neto_benef, p_neto, pc_archivo,pc_num_fol_serv) 

    DEFINE
        pc_nss          LIKE dis_cuenta.nss ,
        pc_num_fol_serv LIKE dis_cuenta.folio,
        pi_consec       LIKE dis_cuenta.consecutivo_lote,
        p_neto_benef    DECIMAL(20,6),
        p_neto          DECIMAL(20,6),
        pc_archivo      CHAR(10)

    DEFINE  gr_diferencias_imp RECORD 
             folio             INTEGER,
             nss               CHAR(11),
             consecutivo       DECIMAL(11,0),
             monto_pesos       DECIMAL(20,6),
             monto_pesos_benef DECIMAL(20,6),
             mod_retiro        CHAR(20)
    END RECORD 

    LET pc_archivo = pc_archivo CLIPPED 
    
    CASE pc_archivo
        WHEN "dis"
            LET gr_diferencias_imp.mod_retiro = 'DISPOSICION'
        WHEN "iss"
            LET gr_diferencias_imp.mod_retiro = 'ISSSTE'
        WHEN "iss_ant"
            LET gr_diferencias_imp.mod_retiro = 'ISSSTE ANTERIOR'
        WHEN "par"
            LET gr_diferencias_imp.mod_retiro = 'PARCIALES'
        WHEN "pmg"
            LET gr_diferencias_imp.mod_retiro = 'PMG'
        WHEN "pmg_iss" 
            LET gr_diferencias_imp.mod_retiro = 'PMG ISSSTE'
        WHEN "vol"
            LET gr_diferencias_imp.mod_retiro = 'VOLUNTARIAS'
        WHEN "bcp"
            LET gr_diferencias_imp.mod_retiro = 'BCP'
    END CASE

    LET gr_diferencias_imp.folio       = pc_num_fol_serv
    LET gr_diferencias_imp.nss         = pc_nss
    LET gr_diferencias_imp.consecutivo = pi_consec
    LET gr_diferencias_imp.monto_pesos = p_neto
    LET gr_diferencias_imp.monto_pesos_benef = p_neto_benef 

    INSERT INTO tmp_diferencia_imp_neto_benef VALUES (gr_diferencias_imp.*)

    IF SQLCA.SQLCODE != 0 THEN
         DISPLAY "                                                            " AT 18,1 ATTRIBUTE(REVERSE)
         DISPLAY " ERROR AL INSERTAR LA INFORMACION                           " AT 18,1 ATTRIBUTE(REVERSE)
         CALL ERRORLOG(SQLCA.SQLCODE||"ERROR AL INSERTAR EN LA TABLA TEMPORAL tmp_diferencia_imp_neto_benef")
         EXIT PROGRAM
    END IF

END FUNCTION 

#---------------------------------------------------------------------------#
# f_genera_archivo_diferencias : Genera reporte de montos diferentes        #
#---------------------------------------------------------------------------#
FUNCTION f_genera_archivo_diferencias()
    DEFINE  gr_diferencias_imp RECORD 
             folio             INTEGER,
             nss               CHAR(11),
             consecutivo       DECIMAL(11,0),
             monto_pesos       DECIMAL(20,6),
             monto_pesos_benef DECIMAL(20,6),
             mod_retiro        CHAR(20)
    END RECORD 

    DEFINE  v_ruta_rep_dif      CHAR(100)
    DEFINE  v_tot_dif           INTEGER 
    DEFINE  ch                  CHAR(100)
    
    SELECT COUNT (*)
    INTO v_tot_dif
    FROM   tmp_diferencia_imp_neto_benef

    IF v_tot_dif > 0 THEN 

        DECLARE cur_difer CURSOR FOR
            SELECT *
            FROM   tmp_diferencia_imp_neto_benef
            ORDER BY nss

        LET v_ruta_rep_dif = gc_ruta CLIPPED,"/RETC848_DIRENCIAS_IMPORTES_",TODAY USING "ddmmyyyy"
        LET ch = "chmod 777 ",v_ruta_rep_dif
        RUN ch

        START REPORT rpt_diferencias_imp TO v_ruta_rep_dif 

        FOREACH cur_difer INTO  gr_diferencias_imp.*
            OUTPUT TO REPORT rpt_diferencias_imp(gr_diferencias_imp.*)
        END FOREACH

    END IF 

END FUNCTION 

#---------------------------------------------------------------------------#
# rpt_diferencias_imp : Genera el detalle de los montos diferentes          #
#---------------------------------------------------------------------------#
REPORT rpt_diferencias_imp(gr_diferencias_imp)

    DEFINE  gr_diferencias_imp RECORD 
             folio             INTEGER,
             nss               CHAR(11),
             consecutivo       DECIMAL(11,0),
             monto_pesos       DECIMAL(20,6),
             monto_pesos_benef DECIMAL(20,6),
             mod_retiro        CHAR(20)
    END RECORD 

    OUTPUT
        PAGE LENGTH   70
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        PAGE HEADER
            PRINT "ARCHIVO DE DIFERENCIAS DE MONTOS ",TODAY USING "dd/mm/yyyy"
            SKIP 1 LINE
    
        ON EVERY ROW
            PRINT 
                'FOLIO: ',gr_diferencias_imp.folio ,'|',    
                '   NSS: ',gr_diferencias_imp.nss  ,'|',    
                '   CONSECUTIVO: ',gr_diferencias_imp.consecutivo ,'|',    
                '   MONTO: ',gr_diferencias_imp.monto_pesos ,'|',    
                '   MONTO BENEFICIARIO:',gr_diferencias_imp.monto_pesos_benef ,'|',    
                '   ARCHIVO: ',gr_diferencias_imp.mod_retiro ,'|'

END REPORT
