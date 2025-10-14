################################################################################
#Proyecto          => SISTEMA DE AFORE( SAFRE )                                #
#Owner             => E.F.P.                                                   #
#Programa RETL830  => GENERA REPORTE DE INFORMACION DE RETIROS AFORE COPPEL    #
#Fecha creacion    => 7 DE SEPTIEMBRE DE 2009                                  #
#By                => JAVIER GONZALEZ JERONIMO                                 #
#Modifico          => JAVIER GONZALEZ JERONIMO                                 #
#                  => 23 DE NOVIEMBRE DE 2009                                  #
#                     Se incluyen los tipos de movimiento del modulo nuevo     #
#                     de retiros ISSSTE (Disposiciones y Parciales)            #
#Modifico          => JAVIER GONZALEZ JERONIMO                                 #
#                  => 26 DE MAYO DE 2010                                       #
#                     Se incluyen los rechazos de Bancoppel al reporte         #
#Modifico          => JAVIER GONZALEZ JERONIMO                                 #
#                  => 15 DE JUNIO DE 2011                                      #
#                     Se incluyen los movimientos de Pension Minima Garantizada#
#Sistema           => RETIROS                                                  #
#------------------------------------------------------------------------------#
#Modificado por    => Phelippe R Santos                                        #
#                  => Se modifica el query de parciales                        #
#Fecha actualiza   => 30 ABRIL DE 2015        CPL-1963                         #
#Actualizacion     => 19-nov-2019 CPL-3021 Se incorpora mov pmg issste         #
################################################################################
DATABASE safre_af

GLOBALS
    DEFINE reg_1 RECORD
        fecha_conversion_ini  DATE,
        fecha_conversion_fin  DATE
    END RECORD

    DEFINE gr_edo RECORD
        liquidado             LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE gr_cza_coppel RECORD
        tipo_registro         CHAR(01)        ,
        num_cont_emp          CHAR(08)        ,
        fecha_gen             DATE            ,
        fecha_ini             DATE            ,
        fecha_fin             DATE            ,
        num_movs              INTEGER
    END RECORD

    DEFINE gr_sum_coppel RECORD
        tipo_registro         CHAR(01)        ,
        num_tot_movs          INTEGER         ,
        imp_tot_neto          DECIMAL(17,2)   ,
        imp_tot_sin_impues    DECIMAL(17,2)   ,
        imp_retenido          DECIMAL(17,2)   ,
        imp_tot_retiros_efec  DECIMAL(17,2)   ,
        imp_tot_retiros_depos DECIMAL(17,2)
    END RECORD

    DEFINE
        tot_neto              ,
        tot_sin_impues        ,
        tot_retenido          DECIMAL(17,2)

    DEFINE
        gc_nombre_plano       CHAR(100) ,
        nom_plano_1           CHAR(012) ,
        nom_plano_2           CHAR(012) ,
        RUTA                  CHAR(100) ,
        G_LISTA_1             CHAR(100) ,
        G_LISTA_2             CHAR(100) ,
        G_LISTA_3             CHAR(100) ,
        enter                 CHAR(001) ,
        cat                   CHAR(300) ,
        ch                    CHAR(100) ,
        gnom_arch_gen         CHAR(100)

    DEFINE
        gdt_cambio_infonavit    ,
        gd_fec_ini              ,
        gd_fec_fin              ,
        HOY                     DATE

    DEFINE
        gs_tipo_disposicion     ,
        gs_mov_viv97            SMALLINT

    DEFINE
        gi_total_movs           ,
        gi_flag                 INTEGER

END GLOBALS

MAIN
    DEFER INTERRUPT

    OPTIONS
        INPUT WRAP          ,
        PROMPT LINE LAST    ,
        ACCEPT KEY CONTROL-I

    CALL f_lib_abre_log("RETL830")
    CALL init()
    CALL f_captura_datos() RETURNING gi_flag, reg_1.*

    IF gi_flag = 1 THEN

        -- Genera la tabla temporal de dis_cuenta
        CALL f_genera_tmp_cuenta(reg_1.*)
        CALL f_genera_detalle()

        LET gc_nombre_plano = "RPC_", HOY USING"YYMMDD", ".lst"
        LET gc_nombre_plano = gc_nombre_plano CLIPPED

        LET gnom_arch_gen = RUTA CLIPPED, "/", gc_nombre_plano
        LET gnom_arch_gen = gnom_arch_gen CLIPPED

        LET cat = "cat ",G_LISTA_2 CLIPPED, " > ", RUTA CLIPPED, "/", gc_nombre_plano
        RUN cat

        LET ch = "chmod 777 ",gnom_arch_gen
        RUN ch

        DISPLAY "                           " AT 19,2
        DISPLAY "EL REPORTE HA SIDO GENERADO EN LA RUTA : " AT 11,18
        DISPLAY RUTA CLIPPED AT 12,18
        DISPLAY "CON EL NOMBRE : ", gc_nombre_plano AT 14,18
        DISPLAY "REGISTROS PROCESADOS : ", gi_total_movs  AT 15,18

        DISPLAY "GENERANDO IMPRESION : ", gi_total_movs  AT 16,18

        LET ch = "lp ", gnom_arch_gen CLIPPED
        RUN ch

        PROMPT  "PROCESO FINALIZADO...PRESIONE <ENTER> PARA SALIR" FOR CHAR enter

    END IF

    CLOSE WINDOW RETL8301

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    ----- INICIALIZACION DE VARIABLES -----

    INITIALIZE reg_1.* TO NULL

    LET HOY                     = TODAY
    LET gdt_cambio_infonavit    = "01/12/2012"
    LET gr_cza_coppel.num_movs  = 0
    LET gi_total_movs           = 0
    LET gi_flag                 = 0
    LET tot_neto                = 0
    LET tot_sin_impues          = 0
    LET tot_retenido            = 0

    SELECT codigo
    INTO   gs_mov_viv97
    FROM   tab_movimiento
    WHERE  descripcion  = "RETIRO DISPOSICION VIVIENDA 97 REG 73"

    ----- TIPOS DE TRAMITE -----
    SELECT cod_tramite
    INTO   gs_tipo_disposicion
    FROM   tab_tipo_tramite
    WHERE  descripcion = "DISPOSICION"

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.liquidado
    FROM   ret_estado A
    WHERE  A.descripcion = "LIQUIDADO"

    ----- RUTA DE ARCHIVOS -----
    SELECT ruta_listados
    INTO   RUTA
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

--    LET RUTA = "/home/efp2/ret"

END FUNCTION

#---------------------------------------------------------------------------#
# f_captura_datos : Captura el rango de fechas para la generacion del       #
#                   reporte                                                 #
#---------------------------------------------------------------------------#
FUNCTION f_captura_datos()

    DEFINE lr_cap RECORD
        fecha_conversion_ini  DATE,
        fecha_conversion_fin  DATE
    END RECORD

    DEFINE
        ls_flag         SMALLINT

    -- -----------------------------------------------------------------------------

    OPEN WINDOW RETL8301 AT 2,2 WITH FORM "RETL8301" ATTRIBUTE(BORDER)
    DISPLAY " <ESC> - Ejecutar                                            <CTRL-C> - Salir " AT 2,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETL830             REPORTE DE PAGOS EFECTUADOS COPPEL                       " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

    LET ls_flag = 0
    LET lr_cap.fecha_conversion_fin  =  HOY
    LET lr_cap.fecha_conversion_ini  =  HOY

    DISPLAY BY NAME lr_cap.fecha_conversion_fin
    DISPLAY BY NAME lr_cap.fecha_conversion_ini

    INPUT lr_cap.fecha_conversion_ini,
          lr_cap.fecha_conversion_fin
    WITHOUT DEFAULTS
    FROM  fecha_conversion_ini,
          fecha_conversion_fin

        AFTER FIELD fecha_conversion_fin

            IF lr_cap.fecha_conversion_ini IS NULL THEN
                ERROR "LA FECHA INICIAL NO PUEDE SER NULA..."
                NEXT FIELD fecha_conversion_ini
            END IF

            IF lr_cap.fecha_conversion_fin IS NULL THEN
                ERROR "LA FECHA FINAL NO PUEDE SER NULA..."
                NEXT FIELD fecha_conversion_fin
            END IF

            IF lr_cap.fecha_conversion_ini > lr_cap.fecha_conversion_fin OR
               lr_cap.fecha_conversion_fin > HOY THEN

                DISPLAY " "
                ERROR "La Fecha Inicial no puede ser mayor a la Fecha Final"
                NEXT FIELD fecha_conversion_ini
            END IF

        ON KEY(ESC)
            IF lr_cap.fecha_conversion_ini IS NULL THEN
                ERROR "LA FECHA INICIAL NO PUEDE SER NULA..."
                NEXT FIELD fecha_conversion_ini
            END IF

            IF lr_cap.fecha_conversion_fin IS NULL THEN
                ERROR "LA FECHA FINAL NO PUEDE SER NULA..."
                NEXT FIELD fecha_conversion_fin
            END IF

            WHILE TRUE
                PROMPT " ¿DESEA GENERAR EL REPORTE? (S/N) " FOR CHAR enter
                IF enter MATCHES "[sSnN]" THEN
                    IF enter MATCHES "[sS]" THEN
                        LET ls_flag     = 1
                        LET nom_plano_1 = lr_cap.fecha_conversion_ini USING "YYYYMMDD",".04D"
                        LET nom_plano_2 = lr_cap.fecha_conversion_ini USING "YYYYMMDD",".04D"

                        LET gd_fec_ini  = lr_cap.fecha_conversion_ini
                        LET gd_fec_fin  = lr_cap.fecha_conversion_fin

                        EXIT WHILE
                    ELSE
                        LET ls_flag     = 0
                        PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
                        EXIT WHILE
                    END IF
                END IF
            END WHILE

            EXIT INPUT

        ON KEY(CONTROL-C,INTERRUPT)
            PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
            LET ls_flag = 0
            EXIT INPUT

        ON KEY (INTERRUPT)
            PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
            LET ls_flag = 0
            EXIT INPUT
    END INPUT

    RETURN ls_flag, lr_cap.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_detalle : Obtiene datos y llama al reporte de pagos              #
#---------------------------------------------------------------------------#
FUNCTION f_genera_detalle()

    DEFINE v_det_coppel RECORD
        n_seguro              CHAR(011),
        paterno               CHAR(040),
        materno               CHAR(040),
        nombre                CHAR(040),
        forma_pago            CHAR(001),
        banco                 SMALLINT,
        clabe                 CHAR(018),
        tp_retiro             CHAR(003),
        imp_doc_antes_impues  DECIMAL(15,2),
        imp_doc_neto          DECIMAL(15,2),
        impuesto_retenido     DECIMAL(11,2)
    END RECORD

    DEFINE total_impuesto_retenido     DECIMAL(11,2),
           total_imp_doc_antes_impues  DECIMAL(15,2),
           total_imp_doc_neto          DECIMAL(15,2)

    DEFINE 
       v_bandera_benef      SMALLINT
    
    DEFINE
        lc_bandera          CHAR(10)

    DEFINE
        li_consec           DECIMAL(10,0)

    DEFINE v_valida_saldo       SMALLINT
    DEFINE v_fecha              CHAR(10)
    DEFINE v_reporte_diferencia CHAR(100)
    DEFINE v_borra_arh          SMALLINT
    DEFINE v_folio              DECIMAL(11,0)

    DEFINE v_monto_dis,
           v_monto_benef        DECIMAL(22,6)

    -- -----------------------------------------------------------------------------

    DISPLAY "GENERANDO DETALLE ..." AT 19,2

    CALL f_genera_tabla_datos(gd_fec_ini,gd_fec_fin)

    DECLARE cur_1 CURSOR FOR
        SELECT *
        FROM   tmp_datos_coppel
        ORDER BY 7,9,1

    LET G_LISTA_2 = RUTA CLIPPED,"/DST"
    START REPORT rpt_detalle TO G_LISTA_2

    --[CPL-3342]
    LET v_fecha     = TODAY
    LET v_borra_arh = 1
    LET v_reporte_diferencia = RUTA CLIPPED,"/RETL830_DIFERENCIAS_IMPORTES_",v_fecha[4,5],v_fecha[1,2],v_fecha[7,10],".txt"
    START REPORT rpt_diferencia TO v_reporte_diferencia
    --//

    FOREACH cur_1 INTO v_det_coppel.n_seguro,
                       li_consec,
                       v_det_coppel.nombre,
                       v_det_coppel.paterno,
                       v_det_coppel.materno,
                       v_det_coppel.clabe,
                       v_det_coppel.forma_pago,
                       v_det_coppel.banco,
                       v_det_coppel.tp_retiro,
                       lc_bandera
                       
            LET v_det_coppel.impuesto_retenido = f_calc_impuesto_retenido(v_det_coppel.n_seguro,li_consec)

            CASE lc_bandera
                WHEN "dis"
                    CALL f_datos_disposicion(v_det_coppel.n_seguro,li_consec)
                        RETURNING v_det_coppel.imp_doc_neto

                WHEN "benef"
                    CALL f_datos_disposicion_benef(v_det_coppel.n_seguro,li_consec)
                        RETURNING v_det_coppel.imp_doc_neto,v_folio,v_valida_saldo,v_monto_dis,v_monto_benef

                    LET v_det_coppel.impuesto_retenido = f_calc_impuesto_retenido_benef(v_det_coppel.n_seguro,li_consec)

                    IF NOT v_valida_saldo THEN
                       OUTPUT TO REPORT rpt_diferencia(v_folio,v_det_coppel.n_seguro,li_consec,v_monto_dis,v_monto_benef,lc_bandera)
                       LET v_borra_arh = 0
                    END IF

                WHEN "pmg"
                    CALL f_datos_pmg(v_det_coppel.n_seguro, li_consec)
                        RETURNING v_det_coppel.imp_doc_neto

                WHEN "pmg_iss"
                    CALL f_datos_pmg_iss(v_det_coppel.n_seguro, li_consec)
                        RETURNING v_det_coppel.imp_doc_neto

                WHEN "iss"
                    CALL f_datos_issste(v_det_coppel.n_seguro,li_consec,v_det_coppel.tp_retiro )
                        RETURNING v_det_coppel.imp_doc_neto

                WHEN "iss_ant"
                    CALL f_datos_issste_ant(v_det_coppel.n_seguro,li_consec,v_det_coppel.tp_retiro)
                        RETURNING v_det_coppel.imp_doc_neto

                WHEN "par"
                    CALL f_datos_parcial(v_det_coppel.n_seguro, li_consec)
                        RETURNING v_det_coppel.imp_doc_neto

                WHEN "vol"
                    CALL f_datos_voluntarias(v_det_coppel.n_seguro, li_consec)
                        RETURNING v_det_coppel.imp_doc_neto

                WHEN "com"
                    CALL f_datos_complementarias(v_det_coppel.n_seguro,li_consec) #cpl-1902
                        RETURNING v_det_coppel.imp_doc_neto


                WHEN "bcp"
                    SELECT (monto_neto/100),
                           (impuesto_ret/100)
                    INTO   v_det_coppel.imp_doc_neto,
                           v_det_coppel.impuesto_retenido
                    FROM   ret_datos_bancoppel
                    WHERE  nss                = v_det_coppel.n_seguro
                    AND    consec_retiro      = li_consec
                    AND    estatus            <> 2
                    AND    fecha_caja_coppel BETWEEN gd_fec_ini AND gd_fec_fin
                    AND    fecha_recibe IS NOT NULL

            END CASE

            LET v_det_coppel.imp_doc_antes_impues = v_det_coppel.imp_doc_neto + v_det_coppel.impuesto_retenido

            --Acumula totales
            LET gi_total_movs     = gi_total_movs + 1
            LET tot_neto          = tot_neto + v_det_coppel.imp_doc_neto
            LET tot_sin_impues    = tot_sin_impues + v_det_coppel.imp_doc_antes_impues
            LET tot_retenido      = tot_retenido + v_det_coppel.impuesto_retenido

            DISPLAY "PROCESANDO REGISTRO : ", gi_total_movs  AT 11,18

            OUTPUT TO REPORT rpt_detalle(v_det_coppel.*,reg_1.*)
            
    END FOREACH

    FINISH REPORT rpt_detalle
    LET ch = "chmod 777 ",G_LISTA_2
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
# f_genera_tmp_cuenta : Genera el extracto de discuenta con los movimientos #
#                       a utilizarse usando las fechas capturadas           #
#---------------------------------------------------------------------------#
FUNCTION f_genera_tmp_cuenta(pr_fechas)

    DEFINE pr_fechas RECORD
        fecha_ini  DATE,
        fecha_fin  DATE
    END RECORD

    -- -----------------------------------------------------------------------------

    DISPLAY "RECOPILANDO DATOS ..." AT 19,2

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_dat_dis_cta
    WHENEVER ERROR STOP

    SELECT nss              ,
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
           OR tipo_movimiento BETWEEN 800 AND 899 -- Retiros
           )
    INTO TEMP tmp_dat_dis_cta

    CREATE INDEX cta_data_01
    ON tmp_dat_dis_cta (tipo_movimiento, consecutivo_lote, nss)

    UPDATE STATISTICS FOR TABLE tmp_dat_dis_cta

    CALL f_elimina_reg73_viv()

    DISPLAY "                               " AT 19,2

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
       AND    subcuenta       IN (4,8)
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
# f_genera_tabla_datos : Genera la tabla que contiene los datos de los      #
#                        registros liquidados en el periodo dado            #
#---------------------------------------------------------------------------#
FUNCTION f_genera_tabla_datos(pr_fechas)

    DEFINE pr_fechas RECORD
        fecha_ini  DATE,
        fecha_fin  DATE
    END RECORD

    -- -----------------------------------------------------------------------------

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_datos_coppel
    WHENEVER ERROR STOP

    SELECT  UNIQUE(a.nss) nss   ,  -- Disposiciones
            b.consecutivo       ,
            b.nombres           ,
            b.paterno           ,
            b.materno           ,
            b.num_cuenta        ,
            b.tipo_pago         ,
            b.banco             ,
            a.tipo_movimiento   ,
            "dis" mod_retiro
    FROM    tmp_dat_dis_cta a   ,
            ret_beneficiario b  ,
            ret_solicitud_tx c
    WHERE   a.tipo_movimiento  BETWEEN 800 AND 899
    AND     a.consecutivo_lote = b.consecutivo
    AND     a.consecutivo_lote = c.consecutivo
    AND     a.nss = b.nss
    AND     a.nss = c.nss
    AND     c.consecutivo NOT IN (SELECT d.consecutivo_solic 
                                  FROM ret_ctr_benef d
                                  WHERE nss = a.nss )
    UNION
    SELECT  UNIQUE(a.nss) nss   ,  -- Disposiciones por beneficiario designado
            b.consecutivo       ,
            b.nombres           ,
            b.paterno           ,
            b.materno           ,
            b.num_cuenta        ,
            b.tipo_pago         ,
            b.banco             ,
            a.tipo_movimiento   ,
            "benef" mod_retiro
    FROM    tmp_dat_dis_cta  a  ,
            ret_beneficiario b  ,
            ret_ctr_benef    c
    WHERE   a.tipo_movimiento  BETWEEN 800 AND 899
    AND     b.consecutivo       = c.consecutivo_solic
    AND     a.consecutivo_lote  = c.consecutivo_padre
    AND     a.nss               = b.nss
    AND     a.nss               = c.nss
    UNION
    SELECT  unique(a.nss) nss   , -- Retiros Totales ISSSTE
            b.consecutivo       ,
            b.nombres           ,
            b.paterno           ,
            b.materno           ,
            b.num_cuenta        ,
            b.tipo_pago         ,
            b.banco             ,
            a.tipo_movimiento   ,
            "iss" mod_retiro
    FROM    tmp_dat_dis_cta a   ,
            ret_beneficiario b  ,
            ret_sol_issste_tx c
    WHERE   ( (a.tipo_movimiento BETWEEN 851 AND 855)
               OR (a.tipo_movimiento = 864) )
    AND     a.consecutivo_lote = b.consecutivo
    AND     a.consecutivo_lote = c.consecutivo
    AND     a.nss = b.nss
    AND     a.nss = c.nss
    UNION
    SELECT  unique(a.nss) nss   , -- Retiros Parciales ISSSTE
            b.consecutivo       ,
            b.nombres           ,
            b.paterno           ,
            b.materno           ,
            b.num_cuenta        ,
            b.tipo_pago         ,
            b.banco             ,
            a.tipo_movimiento   ,
            "iss" mod_retiro
    FROM    tmp_dat_dis_cta a   ,
            ret_beneficiario b  ,
            ret_parcial_issste c
    WHERE   a.tipo_movimiento BETWEEN 856 AND 856
    AND     a.consecutivo_lote = b.consecutivo
    AND     a.consecutivo_lote = c.consecutivo
    AND     a.nss = b.nss
    AND     a.nss = c.nss
    UNION
    SELECT  unique(a.nss) nss   , -- Retiros Parciales
            b.consecutivo       ,
            b.nombres           ,
            b.paterno           ,
            b.materno           ,
            b.num_cuenta        ,
            b.tipo_pago         ,
            b.banco             ,
            a.tipo_movimiento   ,
            "par" mod_retiro
    FROM    tmp_dat_dis_cta a   ,
            ret_beneficiario b  ,
            ret_parcial d
    WHERE   a.tipo_movimiento  IN (870, 875, 876, 877, 878)
    AND     a.consecutivo_lote = b.consecutivo
    AND     a.consecutivo_lote = d.consecutivo                    --CPL-1963
    AND     a.nss = b.nss
    AND     a.nss = d.nss
    UNION
    SELECT  unique(a.nss) nss   , -- Voluntarias
            b.consecutivo       ,
            b.nombres           ,
            b.paterno           ,
            b.materno           ,
            b.num_cuenta        ,
            b.tipo_pago         ,
            b.banco             ,
            a.tipo_movimiento   ,
            "vol" mod_retiro
    FROM    tmp_dat_dis_cta a   ,
            ret_beneficiario b
    WHERE   a.tipo_movimiento  IN (490,493)
    AND     a.consecutivo_lote = b.consecutivo
    AND     a.nss = b.nss
    UNION   #cpl-1902
    SELECT  unique(a.nss) nss   , -- Voluntarias
            b.consecutivo       ,
            b.nombres           ,
            b.paterno           ,
            b.materno           ,
            b.num_cuenta        ,
            b.tipo_pago         ,
            b.banco             ,
            a.tipo_movimiento   ,
            "com" mod_retiro
    FROM    tmp_dat_dis_cta a   ,
            ret_beneficiario b
    WHERE   a.tipo_movimiento  = 897
    AND     a.consecutivo_lote = b.consecutivo
    AND     a.nss = b.nss
    UNION
    SELECT  unique(a.nss) nss   , -- Retiros Totales ISSSTE (Modulo Antiguo)
            b.consecutivo       ,
            b.nombres           ,
            b.paterno           ,
            b.materno           ,
            b.num_cuenta        ,
            b.tipo_pago         ,
            b.banco             ,
            a.tipo_movimiento   ,
            "iss_ant" mod_retiro
    FROM    tmp_dat_dis_cta a   ,
            ret_beneficiario b  ,
            ret_sol_issste_tot c
    WHERE   (a.tipo_movimiento BETWEEN 881 AND 889
             AND a.tipo_movimiento NOT IN (884, 885) )
    AND     a.consecutivo_lote = b.consecutivo
    AND     a.consecutivo_lote = c.consecutivo
    AND     a.nss = b.nss
    AND     a.nss = c.nss_imss
    UNION
    SELECT  unique(a.nss) nss   , -- Retiros Parciales ISSSTE (Modulo Antiguo)
            b.consecutivo       ,
            b.nombres           ,
            b.paterno           ,
            b.materno           ,
            b.num_cuenta        ,
            b.tipo_pago         ,
            b.banco             ,
            a.tipo_movimiento   ,
            "iss_ant" mod_retiro
    FROM    tmp_dat_dis_cta a   ,
            ret_beneficiario b  ,
            ret_sol_issste_par c
    WHERE   a.tipo_movimiento IN (884, 885)
    AND     a.consecutivo_lote = b.consecutivo
    AND     a.consecutivo_lote = c.consecutivo
    AND     a.nss = b.nss
    AND     a.nss = c.nss_imss
    UNION
    SELECT  unique(a.nss) nss   , -- Pension Minima Garantizada
            b.consecutivo       ,
            b.nombres           ,
            b.paterno           ,
            b.materno           ,
            b.num_cuenta        ,
            b.tipo_pago         ,
            b.banco             ,
            a.tipo_movimiento   ,
            "pmg" mod_retiro
    FROM    tmp_dat_dis_cta a   ,
            ret_beneficiario b  ,
            pen_solicitud_pmg d
    WHERE   a.tipo_movimiento   = 841
    AND     a.consecutivo_lote  = b.consecutivo
    AND     a.consecutivo_lote  = d.consecutivo
    AND     a.nss               = b.nss
    AND     a.nss               = d.nss
    UNION
    SELECT  unique(a.nss) nss   , -- Pension Minima Garantizada
            b.consecutivo       ,
            b.nombres           ,
            b.paterno           ,
            b.materno           ,
            b.num_cuenta        ,
            b.tipo_pago         ,
            b.banco             ,
            a.tipo_movimiento   ,
            "pmg_iss" mod_retiro
    FROM    tmp_dat_dis_cta a   ,
            ret_beneficiario b  ,
            pen_solicitud_iss d
    WHERE   a.tipo_movimiento   IN(857,859)
    AND     a.consecutivo_lote  = b.consecutivo
    AND     a.consecutivo_lote  = d.consecutivo
    AND     a.nss               = b.nss
    AND     a.nss               = d.nss
    UNION
    SELECT  a.nss               , -- Rechazos Bancoppel
            a.consec_retiro     ,
            a.nombre            ,
            a.paterno           ,
            a.materno           ,
            a.clabe_o_cta       ,
            6                   , -- tipo pago
            b.banco             ,
            a.tipo_movimiento   ,
            "bcp" mod_retiro
    FROM    ret_datos_bancoppel a   ,
            ret_beneficiario b
    WHERE   a.consec_retiro     = b.consecutivo
    AND     a.fecha_caja_coppel BETWEEN pr_fechas.fecha_ini AND pr_fechas.fecha_fin
    AND     a.estatus           <> 2
    AND     a.fecha_recibe IS NOT NULL
    INTO TEMP tmp_datos_coppel

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

    SELECT SUM(ROUND(a.monto_en_pesos, 2))
    INTO   ld_impuesto_ret
    FROM   tmp_dat_dis_cta a
    WHERE  a.nss              = pc_nss
    AND    a.consecutivo_lote = pi_consec
    AND    a.tipo_movimiento  = 10

    IF ld_impuesto_ret IS NULL OR ld_impuesto_ret = " " THEN
        LET ld_impuesto_ret = 0
    END IF

    LET ld_impuesto_ret = ld_impuesto_ret * -1

    RETURN ld_impuesto_ret

END FUNCTION


#---------------------------------------------------------------------------   #
# f_calc_impuesto_retenido : Obtiene el monto de retencion de ISR del          #
#                            registros liquidado para beneficiarios designados #
#---------------------------------------------------------------------------   #
FUNCTION f_calc_impuesto_retenido_benef(pc_nss, pi_consec)

    DEFINE
        pc_nss      LIKE dis_cuenta.nss             ,
        pi_consec   LIKE dis_cuenta.consecutivo_lote

    DEFINE
        ld_impuesto_ret     DECIMAL(11,2)

    -- -----------------------------------------------------------------------------

    SELECT SUM(ROUND(a.pesos_isr,2))
    INTO   ld_impuesto_ret
    FROM   ret_ctr_benef_det a
    WHERE  a.nss               = pc_nss
    AND    a.consecutivo_solic = pi_consec

    IF ld_impuesto_ret IS NULL OR ld_impuesto_ret = " " THEN
        LET ld_impuesto_ret = 0
    END IF

    LET ld_impuesto_ret = ld_impuesto_ret --* -1 

    RETURN ld_impuesto_ret

END FUNCTION

#---------------------------------------------------------------------------#
# f_datos_disposicion : Obtiene el monto total liquidado del registro de    #
#                       disposiciones IMSS                                  #
#---------------------------------------------------------------------------#
FUNCTION f_datos_disposicion(pc_nss, pi_consec)

    DEFINE
        pc_nss      LIKE dis_cuenta.nss             ,
        pi_consec   LIKE dis_cuenta.consecutivo_lote

    DEFINE
        ld_neto_disp            DECIMAL(15,2)

    -- -----------------------------------------------------------------------------

    SELECT NVL(SUM(ROUND(a.monto_en_pesos, 2)),0)
    INTO   ld_neto_disp
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

    LET ld_neto_disp = ld_neto_disp * -1

    RETURN ld_neto_disp

END FUNCTION


#---------------------------------------------------------------------------#
# f_datos_disposicion : Obtiene el monto total liquidado del registro de    #
#                       disposiciones IMSS                                  #
#---------------------------------------------------------------------------#
FUNCTION f_datos_disposicion_benef(pc_nss, pi_consec)

    DEFINE
        pc_nss      LIKE dis_cuenta.nss             ,
        pi_consec   LIKE dis_cuenta.consecutivo_lote

    DEFINE
        ld_neto_disp            DECIMAL(15,2)

    DEFINE v_consec_padre   LIKE dis_cuenta.consecutivo_lote

    DEFINE num_fol_serv           DECIMAL(11,0)

    DEFINE r_valida SMALLINT

    DEFINE v_compara_benef DECIMAL(22,6)
    DEFINE v_compara_cta   DECIMAL(22,6)
    -- -----------------------------------------------------------------------------

    LET r_valida = 1

    SELECT MAX(folio_solicitud)
    INTO num_fol_serv
    FROM ret_solicitud_tx
    WHERE nss = pc_nss
    AND consecutivo = pi_consec

    SELECT NVL(SUM(ROUND(pesos_neto,2)),0)
    INTO   ld_neto_disp
    FROM ret_ctr_benef_det
    WHERE nss = pc_nss
    AND consecutivo_solic = pi_consec

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
       AND   a.nss = pc_nss
       AND   a.consecutivo_solic = b.consecutivo_solic
       AND   b.consecutivo_padre = v_consec_padre

    IF v_compara_cta <> v_compara_benef THEN 
       LET r_valida = 0
    END IF
   
   LET ld_neto_disp = ld_neto_disp --* -1   -- El monto no es negativo por lo que se quita la multiplicación por el -1
    
    RETURN ld_neto_disp,num_fol_serv,r_valida,v_compara_cta,v_compara_benef

END FUNCTION


#---------------------------------------------------------------------------#
# f_datos_issste : Obtiene el monto total liquidado del registro de         #
#                  disposiciones y retiros parciales ISSSTE                 #
#---------------------------------------------------------------------------#
FUNCTION f_datos_issste(pr_sol)

    DEFINE pr_sol RECORD
        nss         LIKE dis_cuenta.nss             ,
        consec      LIKE dis_cuenta.consecutivo_lote,
        tipo_mov    LIKE dis_cuenta.tipo_movimiento
    END RECORD

    DEFINE
        ld_neto_issste          DECIMAL(15,2)

    -- -----------------------------------------------------------------------------

    IF pr_sol.tipo_mov = 856 THEN
        SELECT NVL(SUM(ROUND(a.monto_en_pesos, 2)),0)
        INTO   ld_neto_issste
        FROM   tmp_dat_dis_cta a    ,
               ret_parcial_issste c
        WHERE  a.nss              = pr_sol.nss
        AND    a.consecutivo_lote = pr_sol.consec
        AND    a.tipo_movimiento  = pr_sol.tipo_mov
        AND    a.consecutivo_lote = c.consecutivo
        AND    a.nss              = c.nss
    ELSE
        SELECT NVL(SUM(ROUND(a.monto_en_pesos, 2)),0)
        INTO   ld_neto_issste
        FROM   tmp_dat_dis_cta a    ,
               ret_sol_issste_tx c
        WHERE  a.nss              = pr_sol.nss
        AND    a.consecutivo_lote = pr_sol.consec
        AND    a.tipo_movimiento  = pr_sol.tipo_mov
        AND    a.consecutivo_lote = c.consecutivo
        AND    a.nss              = c.nss
    END IF

    LET ld_neto_issste = ld_neto_issste * -1

    RETURN ld_neto_issste

END FUNCTION

#---------------------------------------------------------------------------#
# f_datos_issste_ant : Obtiene el monto total liquidado del registro de     #
#                      retiros ISSSTE Modulo anterior                       #
#---------------------------------------------------------------------------#
FUNCTION f_datos_issste_ant(pr_sol)

    DEFINE pr_sol RECORD
        nss         LIKE dis_cuenta.nss             ,
        consec      LIKE dis_cuenta.consecutivo_lote,
        tipo_mov    LIKE dis_cuenta.tipo_movimiento
    END RECORD

    DEFINE
        ld_neto_issste         DECIMAL(15,2)

    -- -----------------------------------------------------------------------------

    IF pr_sol.tipo_mov = 884 OR pr_sol.tipo_mov = 885 THEN
        SELECT NVL(SUM(ROUND(a.monto_en_pesos, 2)),0)
        INTO   ld_neto_issste
        FROM   tmp_dat_dis_cta a    ,
               ret_sol_issste_par c
        WHERE  a.nss              = pr_sol.nss
        AND    a.consecutivo_lote = pr_sol.consec
        AND    a.tipo_movimiento  = pr_sol.tipo_mov
        AND    a.consecutivo_lote = c.consecutivo
        AND    a.nss              = c.nss_imss
        AND    c.tipo_retiro IN (4,5)
    ELSE
        SELECT NVL(SUM(ROUND(a.monto_en_pesos, 2)),0)
        INTO   ld_neto_issste
        FROM   tmp_dat_dis_cta a    ,
               ret_sol_issste_tot c
        WHERE  a.nss              = pr_sol.nss
        AND    a.consecutivo_lote = pr_sol.consec
        AND    a.tipo_movimiento  = pr_sol.tipo_mov
        AND    a.consecutivo_lote = c.consecutivo
        AND    a.nss              = c.nss_imss
        AND    c.tipo_retiro IN (1,2,3,6,7,8,9)
    END IF

    LET ld_neto_issste = ld_neto_issste * -1

    RETURN ld_neto_issste

END FUNCTION

#---------------------------------------------------------------------------#
# f_datos_parcial : Obtiene el monto total liquidado del registro de        #
#                   retiros parciales IMSS                                  #
#---------------------------------------------------------------------------#
FUNCTION f_datos_parcial(pc_nss, pi_consec)

    DEFINE
        pc_nss      LIKE dis_cuenta.nss             ,
        pi_consec   LIKE dis_cuenta.consecutivo_lote

    DEFINE
        ld_neto_parcial         DECIMAL(15,2)

    -- -----------------------------------------------------------------------------

    SELECT NVL(SUM(ROUND(a.monto_en_pesos, 2)),0)
    INTO   ld_neto_parcial
    FROM   tmp_dat_dis_cta a,
           ret_parcial d
    WHERE  a.nss              = pc_nss
    AND    a.consecutivo_lote = pi_consec
    AND    a.tipo_movimiento IN (870, 875, 876, 877, 878)
    AND    a.consecutivo_lote = d.consecutivo
    AND    a.nss              = d.nss

    LET ld_neto_parcial = ld_neto_parcial * -1

    RETURN ld_neto_parcial

END FUNCTION

#---------------------------------------------------------------------------#
# f_datos_pmg : Obtiene el monto total liquidado del registro de Pension    #
#               Minima Garantizada                                          #
#---------------------------------------------------------------------------#
FUNCTION f_datos_pmg(pc_nss, pi_consec)

    DEFINE
        pc_nss      LIKE dis_cuenta.nss             ,
        pi_consec   LIKE dis_cuenta.consecutivo_lote

    DEFINE
        ld_neto_parcial         DECIMAL(15,2)

    -- -----------------------------------------------------------------------------

    SELECT NVL(SUM(ROUND(a.monto_en_pesos, 2)),0)
    INTO   ld_neto_parcial
    FROM   tmp_dat_dis_cta a,
           pen_solicitud_pmg d
    WHERE  a.nss              = pc_nss
    AND    a.consecutivo_lote = pi_consec
    AND    a.tipo_movimiento  = 841
    AND    a.consecutivo_lote = d.consecutivo
    AND    a.nss              = d.nss

    LET ld_neto_parcial = ld_neto_parcial * -1

    RETURN ld_neto_parcial

END FUNCTION

#------------------------------------------------------------------------------#
# f_datos_pmg_iss : Obtiene el monto total liquidado del registro de Pension   #
#                   Minima Garantizada                                         #
#------------------------------------------------------------------------------#
FUNCTION f_datos_pmg_iss(pc_nss, pi_consec)

    DEFINE pc_nss             LIKE dis_cuenta.nss
    DEFINE pi_consec          LIKE dis_cuenta.consecutivo_lote

    DEFINE ld_neto_parcial    DECIMAL(15,2)

    ----------------------------------------------------------------------------

    SELECT NVL(SUM(ROUND(a.monto_en_pesos, 2)),0)
    INTO   ld_neto_parcial
    FROM   tmp_dat_dis_cta a,
           pen_solicitud_iss d
    WHERE  a.nss              = pc_nss
    AND    a.consecutivo_lote = pi_consec
    AND    a.tipo_movimiento  IN (857,859)
    AND    a.consecutivo_lote = d.consecutivo
    AND    a.nss              = d.nss

    LET ld_neto_parcial = ld_neto_parcial * -1

    RETURN ld_neto_parcial

END FUNCTION

#---------------------------------------------------------------------------#
# f_datos_voluntarias : Obtiene el monto total liquidado del registro de    #
#                       Voluntarias                                         #
#---------------------------------------------------------------------------#
FUNCTION f_datos_voluntarias(pc_nss, pi_consec)

    DEFINE
        pc_nss      LIKE dis_cuenta.nss             ,
        pi_consec   LIKE dis_cuenta.consecutivo_lote

    DEFINE
        ld_neto_vol             DECIMAL(15,2)

    -- -----------------------------------------------------------------------------

    SELECT NVL(SUM(ROUND(a.monto_en_pesos, 2)),0)
    INTO   ld_neto_vol
    FROM   tmp_dat_dis_cta  a,
           ret_cta_vol d
    WHERE  a.nss              = pc_nss
    AND    a.consecutivo_lote = pi_consec
    AND    a.tipo_movimiento  IN (490,493)
    AND    a.consecutivo_lote = d.consecutivo
    AND    a.nss              = d.n_seguro

    LET ld_neto_vol = ld_neto_vol * -1

    RETURN ld_neto_vol

END FUNCTION

#---------------------------------------------------------------------------#
# f_datos_complementarias : Obtiene el monto total liquidado del registro de#
#                       complementarias                                     #
#---------------------------------------------------------------------------#
FUNCTION f_datos_complementarias(pc_nss, pi_consec)   #cpl-1902

    DEFINE
        pc_nss      LIKE dis_cuenta.nss             ,
        pi_consec   LIKE dis_cuenta.consecutivo_lote

    DEFINE
        ld_neto_vol             DECIMAL(15,2)

    -- -----------------------------------------------------------------------------

    SELECT NVL(SUM(ROUND(a.monto_en_pesos, 2)),0)
    INTO   ld_neto_vol
    FROM   tmp_dat_dis_cta  a,
           ret_cta_vol d
    WHERE  a.nss              = pc_nss
    AND    a.consecutivo_lote = pi_consec
    AND    a.tipo_movimiento  = 897
    AND    a.consecutivo_lote = d.consecutivo
    AND    a.nss              = d.n_seguro

    LET ld_neto_vol = ld_neto_vol * -1

    RETURN ld_neto_vol

END FUNCTION

#---------------------------------------------------------------------------#
# rpt_detalle : Genera el reporte de registros liquiadados                  #
#---------------------------------------------------------------------------#
REPORT rpt_detalle(lr_det_coppel,p_fechas_rpt)

    DEFINE lr_det_coppel RECORD #loc #lr_det_coppel
        n_seguro              CHAR(011),
        paterno               CHAR(015),
        materno               CHAR(015),
        nombre                CHAR(015),
        forma_pago            CHAR(001),
        banco                 SMALLINT,
        clabe                 CHAR(018),
        tp_retiro             CHAR(003),
        imp_doc_antes_impues  DECIMAL(15,2),
        imp_doc_neto          DECIMAL(15,2),
        impuesto_retenido     DECIMAL(11,2)
    END RECORD

    DEFINE p_fechas_rpt RECORD
        fecha_conversion_ini  DATE,
        fecha_conversion_fin  DATE
    END RECORD

    DEFINE lr_total_por_tpago RECORD
        nss_tot             INTEGER         ,
        tipo_pago           CHAR(01)        ,
        bruto               DECIMAL(22,6)   ,
        neto                DECIMAL(22,6)   ,
        impuesto            DECIMAL(22,6)
    END RECORD

    DEFINE lr_total_general RECORD
        nss_tot             INTEGER         ,
        bruto               DECIMAL(22,6)   ,
        neto                DECIMAL(22,6)   ,
        impuesto            DECIMAL(22,6)
    END RECORD

    DEFINE
        li_page_length          ,
        li_page_length_totales  ,
        li_detalle              SMALLINT

    DEFINE
        lc_desc_ret           CHAR(10) ,
        lc_f_pago             CHAR(15) ,
        lc_banco              CHAR(15) ,
        lc_desc_banco         CHAR(15) ,
        encabezado            CHAR(60) ,
        L1                    CHAR(01) ,
        L2                    CHAR(02) ,
        L3                    CHAR(03) ,
        L4                    CHAR(04) ,
        L5                    CHAR(05) ,
        L6                    CHAR(06) ,
        L7                    CHAR(07) ,
        L8                    CHAR(08) ,
        L9                    CHAR(09) ,
        L10                   CHAR(10) ,
        L11                   CHAR(11)

    -- -----------------------------------------------------------------------------

    OUTPUT
        PAGE LENGTH   45
        LEFT MARGIN    0
        RIGHT MARGIN   0
        TOP MARGIN     0
        BOTTOM MARGIN  0

    ORDER BY lr_det_coppel.forma_pago,lr_det_coppel.tp_retiro,lr_det_coppel.n_seguro

    FORMAT
        FIRST PAGE HEADER

            LET lr_total_general.nss_tot   = 0
            LET lr_total_general.bruto     = 0
            LET lr_total_general.neto      = 0
            LET lr_total_general.impuesto  = 0

            LET li_page_length = 45 - 6 --5 titulos, 1 detalle
            LET li_page_length_totales = 45 - 3 --5 titulos, 1 detalle

            LET li_detalle = 0

            LET L1  = "\304"
            LET L2  = "\304\304"
            LET L3  = "\304\304\304"
            LET L4  = "\304\304\304\304"
            LET L5  = "\304\304\304\304\304"
            LET L6  = "\304\304\304\304\304\304"
            LET L7  = "\304\304\304\304\304\304\304"
            LET L8  = "\304\304\304\304\304\304\304\304"
            LET L9  = "\304\304\304\304\304\304\304\304\304"
            LET L10 = "\304\304\304\304\304\304\304\304\304\304"
            LET L11 = "\304\304\304\304\304\304\304\304\304\304\304"

            LET encabezado = "M O D U L O   D E   R E T I R O S"

            PRINT '\033E\033(10U\033&l1O\033&k2S\033&18d',
                   '\033e\033(s18H'

            PRINT
                 COLUMN 073,encabezado,
                 '\033015'

            SKIP 1 LINES

            PRINT COLUMN 73,"REPORTE DE PAGOS EFECTUADOS COPPEL",
                 '\033015'
            SKIP 1 LINES


            PRINT COLUMN 001,"FECHA INICIO : ",p_fechas_rpt.fecha_conversion_ini USING "DD/MM/YYYY"   ,
                  COLUMN 160,"PROG.    : RETL830",
                  '\033015'

            PRINT COLUMN 001,"FECHA FINAL  : ",p_fechas_rpt.fecha_conversion_fin USING "DD/MM/YYYY"   ,
                  COLUMN 160,"PAGINA   :    ",PAGENO USING "####",
                  '\033015'

            PRINT
                 COLUMN 160,"FECHA : ", HOY USING "DD/MM/YYYY",
                 '\033015'

        PAGE HEADER
            PRINT COLUMN 001,"FECHA INICIO : ",p_fechas_rpt.fecha_conversion_ini USING "DD/MM/YYYY",
                             "  FECHA FIN : ",p_fechas_rpt.fecha_conversion_fin USING "DD/MM/YYYY" ,
                           "                       REPORTE DE PAGOS EFECTUADOS COPPEL",
                  COLUMN 160,"PAGINA   :    ",PAGENO USING "####",
                  '\033015'

            PRINT COLUMN 001,L10,L10,L10,L10,L10,
                             L10,L10,L10,L10,L10,
                             L10,L10,L10,L10,L10,
                             L10,L10,L5, L2, L1,
                             '\033015'

        BEFORE GROUP OF lr_det_coppel.forma_pago

            -- Inicializamos totales por tipo de pago
            LET lr_total_por_tpago.tipo_pago = lr_det_coppel.forma_pago

            CASE lr_total_por_tpago.tipo_pago
                WHEN 1
                    LET lc_f_pago       = "1-CAJA COPPEL"
                    LET lc_banco        = " "
                    LET lc_desc_banco   = " "

                WHEN 2
                    LET lc_f_pago       = "2-BANCOMER"
                    LET lc_banco        = " "
                    LET lc_desc_banco   = " "

                WHEN 3
                    LET lc_f_pago       = "3-BANCOPPEL"
                    LET lc_banco        = " "
                    LET lc_desc_banco   = " "

                WHEN 4
                    LET lc_f_pago       = "4-OTRO BANCO"

                #CPL-2810 - CAT INI
                {WHEN 5
                    LET lc_f_pago       = "5-RECHAZOS"
                    LET lc_banco        = " "
                    LET lc_desc_banco   = " "}
                WHEN 5
                    LET lc_f_pago       = "5-CHEQUES"
                    LET lc_banco        = " "
                    LET lc_desc_banco   = " "

                WHEN 6
                    LET lc_f_pago       = "6-RECHAZOS"
                    LET lc_banco        = " "
                    LET lc_desc_banco   = " "
                #CPL-2810 - CAT FIN

            END CASE

            LET lr_total_por_tpago.nss_tot   = 0
            LET lr_total_por_tpago.bruto     = 0
            LET lr_total_por_tpago.neto      = 0
            LET lr_total_por_tpago.impuesto  = 0

            IF LINENO > li_page_length THEN
                SKIP TO TOP OF PAGE
            END IF

            -- LINEA 1
            PRINT COLUMN 001,"\332", L10,L1,          -- nss
                             "\302", L10,L8,          -- paterno
                             "\302", L10,L8,          -- materno
                             "\302", L10,L8,          -- nombre
                             "\302", L10,L5,          -- forma de pago
                             "\302", L10,L5,          -- banco
                             "\302", L10,L9,          -- clabe o cuenta
                             "\302", L10,             -- tipo retiro
                             "\302", L10,L5,          -- bruto
                             "\302", L10,L5,          -- neto
                             "\302", L10,L1,          -- impuesto
                             "\277",
                             '\033015'

            -- LINEA 2
            PRINT COLUMN 001, "|"                  , -- nss
                  COLUMN 013, "|"                  , -- paterno
                  COLUMN 032, "|"                  , -- materno
                  COLUMN 051, "|"                  , -- nombre
                  COLUMN 070, "|     FORMA"        , -- forma de pago
                  COLUMN 086, "|"                  , -- banco
                  COLUMN 102, "|      CLABE"       , -- clabe o cuenta
                  COLUMN 122, "|   TIPO"           , -- tipo retiro
                  COLUMN 133, "|"                  , -- bruto
                  COLUMN 149, "|"                  , -- neto
                  COLUMN 165, "|"                  , -- impuesto
                  COLUMN 177, "|"                  ,
                  '\033015'

            -- LINEA 3
            PRINT COLUMN 001, "|    NSS"        ,
                  COLUMN 013, "|      PATERNO"  ,
                  COLUMN 032, "|      MATERNO"  ,
                  COLUMN 051, "|     NOMBRE"    ,
                  COLUMN 070, "|     DE"        ,
                  COLUMN 086, "|     BANCO"     ,
                  COLUMN 102, "|        O"      ,
                  COLUMN 122, "|    DE"         ,
                  COLUMN 133, "|     MONTO"     ,
                  COLUMN 149, "|     MONTO"     ,
                  COLUMN 165, "|  IMPUESTO"     ,
                  COLUMN 177, "|"               ,
                  '\033015'


            -- LINEA 4
            PRINT COLUMN 001, "|"               , -- nss
                  COLUMN 013, "|"               , -- paterno
                  COLUMN 032, "|"               , -- materno
                  COLUMN 051, "|"               , -- nombre
                  COLUMN 070, "|     PAGO"      , -- forma de pago
                  COLUMN 086, "|"               , -- banco
                  COLUMN 102, "|      CUENTA"   , -- clabe o cuenta
                  COLUMN 122, "|  RETIRO"       , -- tipo retiro
                  COLUMN 133, "|     BRUTO"     , -- bruto
                  COLUMN 149, "|     NETO"      , -- neto
                  COLUMN 165, "|  RETENIDO"     , -- impuesto
                  COLUMN 177, "|"               ,
                  '\033015'


            -- LINEA 5
            PRINT COLUMN 001,"\300", L10,L1,           -- nss
                             "\301", L10,L8,           -- paterno
                             "\301", L10,L8,           -- materno
                             "\301", L10,L8,           -- nombre
                             "\301", L10,L5,           -- forma de pago
                             "\301", L10,L5,           -- banco
                             "\301", L10,L9,           -- clabe o cuenta
                             "\301", L10,              -- tipo retiro
                             "\301", L10,L5,           -- bruto
                             "\301", L10,L5,           -- neto
                             "\301", L10,L1,           -- impuesto
                             "\331",
                             '\033015'

        ON EVERY ROW

            CASE lr_total_por_tpago.tipo_pago

                WHEN 1
                    LET lr_det_coppel.clabe = " "

                WHEN 4
                    SELECT descripcion
                    INTO   lc_desc_banco
                    FROM   tab_banco
                    WHERE  banco = lr_det_coppel.banco

                    LET lc_banco = lr_det_coppel.banco USING "##", "-", lc_desc_banco CLIPPED
            END CASE

            CASE
                WHEN lr_det_coppel.tp_retiro = 490
                    LET lc_desc_ret = "Ret Vol"

                WHEN lr_det_coppel.tp_retiro = 493
                    LET lc_desc_ret = "RetVol App"

                WHEN lr_det_coppel.tp_retiro = 897    #cpl-1902
                    LET lc_desc_ret = "Ret Comp"

                WHEN lr_det_coppel.tp_retiro = 870
                    LET lc_desc_ret = "Par Matr"

                WHEN (lr_det_coppel.tp_retiro >= 875) AND (lr_det_coppel.tp_retiro <= 878)
                    LET lc_desc_ret = "Par Desem"

                WHEN lr_det_coppel.tp_retiro = 820
                    LET lc_desc_ret = "Dis 'D'"

                WHEN lr_det_coppel.tp_retiro = 825
                    LET lc_desc_ret = "Dis 'M'"

                WHEN lr_det_coppel.tp_retiro = 830
                    LET lc_desc_ret = "Dis 'E'"

                WHEN lr_det_coppel.tp_retiro = 835
                    LET lc_desc_ret = "Dis 'P'"

                WHEN lr_det_coppel.tp_retiro = 840
                    LET lc_desc_ret = "Dis 'F'"

                WHEN lr_det_coppel.tp_retiro = 850
                    LET lc_desc_ret = "Dis 'G'"

                WHEN lr_det_coppel.tp_retiro = 860
                    LET lc_desc_ret = "Dis 'H'"

                WHEN lr_det_coppel.tp_retiro = 880
                    LET lc_desc_ret = "Dis 'J'"

                WHEN lr_det_coppel.tp_retiro = 841
                    LET lc_desc_ret = "PMG"

                WHEN lr_det_coppel.tp_retiro = 857
                    LET lc_desc_ret = "PMG ISS"

                WHEN lr_det_coppel.tp_retiro = 859
                    LET lc_desc_ret = "PROG ISS"
                    
                WHEN lr_det_coppel.tp_retiro = 856
                    LET lc_desc_ret = "Issste Par"

                WHEN (lr_det_coppel.tp_retiro >= 851) AND (lr_det_coppel.tp_retiro <= 855)
                    LET lc_desc_ret = "Issste Tot"

                WHEN (lr_det_coppel.tp_retiro = 864)
                    LET lc_desc_ret = "Issste Tot"

                WHEN (lr_det_coppel.tp_retiro >= 881) AND (lr_det_coppel.tp_retiro <= 889)
                    LET lc_desc_ret = "Issste Tot"

            END CASE


            --Se imprime la informacion del reporte
            PRINT COLUMN 002, lr_det_coppel.n_seguro    ,
                  COLUMN 014, lr_det_coppel.paterno     ,
                  COLUMN 033, lr_det_coppel.materno     ,
                  COLUMN 052, lr_det_coppel.nombre      ,
                  COLUMN 072, lc_f_pago                 ,
                  COLUMN 087, lc_banco                  ,
                  COLUMN 104, lr_det_coppel.clabe       USING "##################",
                  COLUMN 123, lc_desc_ret               ,
                  COLUMN 134, lr_det_coppel.imp_doc_antes_impues USING "###########&.&&",
                  COLUMN 150, lr_det_coppel.imp_doc_neto USING "###########&.&&",
                  COLUMN 166, lr_det_coppel.impuesto_retenido USING "#######&.&&",
                  '\033015'

            -- se acumulan los totales
            LET lr_total_por_tpago.nss_tot   = lr_total_por_tpago.nss_tot + 1
            LET lr_total_por_tpago.bruto     = lr_total_por_tpago.bruto + lr_det_coppel.imp_doc_antes_impues
            LET lr_total_por_tpago.neto      = lr_total_por_tpago.neto + lr_det_coppel.imp_doc_neto
            LET lr_total_por_tpago.impuesto  = lr_total_por_tpago.impuesto + lr_det_coppel.impuesto_retenido

        AFTER GROUP OF lr_det_coppel.forma_pago
            LET li_detalle = 0
            SKIP 1 LINE

            IF lr_total_por_tpago.nss_tot > 0 THEN

                IF LINENO > li_page_length_totales THEN
                    SKIP TO TOP OF PAGE
                END IF

                PRINT COLUMN 001, "\332",L10,L10,L7,            -- nss
                                  "\302",L11,L11,L11,L11,L11,
                                         L11,L11,L11,L11,L4,    -- totales
                                  "\302",L10,L5,                -- bruto
                                  "\302",L10,L5,                -- neto
                                  "\302",L10,L1,                -- impuesto
                                  "\277",
                                  '\033015'

                PRINT COLUMN 001, "|TOTAL NSS :",
                                  lr_total_por_tpago.nss_tot USING "#############&",
                      COLUMN 029, "|"                             ,
                      COLUMN 111, "SUBTOTAL TIPO PAGO"            ,
                      COLUMN 130, lr_total_por_tpago.tipo_pago    USING "&",
                      COLUMN 133, "|",lr_total_por_tpago.bruto    USING "###########&.&&",
                      COLUMN 149, "|",lr_total_por_tpago.neto     USING "###########&.&&",
                      COLUMN 165, "|",lr_total_por_tpago.impuesto USING "#######&.&&", "|",
                      '\033015'

                PRINT COLUMN 001, "\300",L10,L10,L7,            -- nss
                                  "\301",L11,L11,L11,L11,L11,
                                         L11,L11,L11,L11,L4,    -- totales
                                  "\301",L10,L5,                -- bruto
                                  "\301",L10,L5,                -- neto
                                  "\301",L10,L1,                -- impuesto
                                  "\331",
                                  '\033015'

                -- Acumulamos totales generales
                LET lr_total_general.nss_tot   = lr_total_general.nss_tot  + lr_total_por_tpago.nss_tot
                LET lr_total_general.bruto     = lr_total_general.bruto    + lr_total_por_tpago.bruto
                LET lr_total_general.neto      = lr_total_general.neto     + lr_total_por_tpago.neto
                LET lr_total_general.impuesto  = lr_total_general.impuesto + lr_total_por_tpago.impuesto
            END IF

            SKIP 1 LINES

        ON LAST ROW
            SKIP 1 LINES

            IF LINENO > li_page_length_totales THEN
                SKIP TO TOP OF PAGE
            END IF

            PRINT COLUMN 001, "\332",L10,L10,L7,            -- nss
                              "\302",L11,L11,L11,L11,L11,
                                     L11,L11,L11,L11,L4,    -- totales
                              "\302",L10,L5,                -- bruto
                              "\302",L10,L5,                -- neto
                              "\302",L10,L1,                -- impuesto
                              "\277",
                              '\033015'

            PRINT COLUMN 001, "|TOTAL NSS :",
                              lr_total_general.nss_tot USING "#############&",
                  COLUMN 029, "|"                             ,
                  COLUMN 111, "TOTAL GENERAL PAGO"            ,
                  COLUMN 133, "|",lr_total_general.bruto    USING "###########&.&&",
                  COLUMN 149, "|",lr_total_general.neto     USING "###########&.&&",
                  COLUMN 165, "|",lr_total_general.impuesto USING "#######&.&&", "|",
                  '\033015'

            PRINT COLUMN 001, "\300",L10,L10,L7,            -- nss
                              "\301",L11,L11,L11,L11,L11,
                                     L11,L11,L11,L11,L4,    -- totales
                              "\301",L10,L5,                -- bruto
                              "\301",L10,L5,                -- neto
                              "\301",L10,L1,                -- impuesto
                              "\331",
                              '\033015'
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
          PRINT " RETL830 - ARCHIVO DE DIFERENCIAS DE MONTOS ",TODAY USING "dd/mm/yyyy"
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
