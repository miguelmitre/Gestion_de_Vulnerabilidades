#################################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                               #
#Owner             => E.F.P.                                                    #
#Programa RETL833b => FUNCIONES PARTICULARES PARA REALIZAR EL RUBRO DE SALIDAS  #
#                     DEL ANEXO 1112                                            #
#                                    VERSION COPPEL                             #
#Fecha creacion    => 04 DE JUNIO DE 2012                                       #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Sistema           => RET                                                       #
#################################################################################

DATABASE safre_af

#---------------------------------------------------------------------------#
# f_genera_salidas : Obtiene el detalle de los registros de entrada del     #
#                     layout del anexo 1112                                 #
#---------------------------------------------------------------------------#
FUNCTION f_genera_salidas(pr_fechas_anexo)

    DEFINE pr_fechas_anexo  RECORD
        inicial             DATE,
        final               DATE
    END RECORD

    DEFINE lr_det_anexo RECORD LIKE ret_det_anexo1112.*

    DEFINE
        lc_where                    CHAR(100)

    DEFINE
        ls_inserta                  ,
        ls_tipo                     ,
        ls_id_movimiento            SMALLINT

    -- -----------------------------------------------------------------------------

    INITIALIZE lr_det_anexo.* TO NULL

    LET ls_id_movimiento            = 0
    LET ls_tipo                     = 0
    LET lr_det_anexo.folio          = 1
    LET lr_det_anexo.fecha_inicio   = pr_fechas_anexo.inicial
    LET lr_det_anexo.fecha_fin      = pr_fechas_anexo.final

    DISPLAY "REGISTROS DE SALIDAS IMSS      : ", ls_id_movimiento AT 10,6

    DECLARE cur_in CURSOR FOR
        SELECT id_movimiento
        FROM   tab_movimientos_anexo1112
        WHERE  tipo_cuenta      = "02"
        AND    id_movimiento    < 1000 -- Los movimientos de IMSS son los id menores a mil
        ORDER BY 1

    -- Barrer los tipos de movimiento de entrada
    FOREACH cur_in INTO ls_id_movimiento

        LET lr_det_anexo.num_solicitudes        = 0
        LET lr_det_anexo.mto_mes_actual         = 0
        LET lr_det_anexo.mto_mes_anterior       = 0
        LET lr_det_anexo.mto_acumulado_anual    = 0

        LET lr_det_anexo.id_movimiento  = ls_id_movimiento
        LET ls_tipo                     = ls_tipo + 1

        CASE ls_id_movimiento

            WHEN 15     --  TOTAL DE TRANSFERENCIAS POR RESOLUCION DE PENSION
                LET lc_where    = " WHERE tipo_movimiento IN (800, 810, 815)"
                LET ls_inserta = 1

            WHEN 16     --  TRANSFERENCIA DE RECURSOS AL IMSS/ASEGURADORA POR UNA RENTA VITALICIA
                LET lc_where    = " WHERE tipo_movimiento = 800 "
                LET ls_inserta = 1

            WHEN 17     --  TRANSFERENCIA DE RECURSOS AL GOBIERNO FEDERAL POR UNA PENSION AUTORIZADA AL IMSS
                LET lc_where    = " WHERE tipo_movimiento = 810 "
                LET ls_inserta = 1

            WHEN 18     --  TRANSFERENCIA DE RECURSOS AL GOBIERNO FEDERAL POR UNA PENSION GARANTIZADA
                LET lc_where    = " WHERE tipo_movimiento = 815 "
                LET ls_inserta = 1

            WHEN 20     -- DISPOSICION POR UN TRAMITE JUDICIAL
                LET ls_inserta = 1
                EXIT CASE

            WHEN 21     --  DISPOSICION POR PLANES PRIVADOS DE PENSION,
                LET lc_where    = " WHERE tipo_movimiento = 840"
                LET ls_inserta = 1

            WHEN 22     --  DISPOSICIONES POR EDAD DEL TRABAJADOR (VEJEZ)
                LET lc_where    = " WHERE tipo_movimiento = 860"
                LET ls_inserta = 1

            WHEN 23     --  DISPOSICION POR UNA NEGATIVA DE PENSION
                LET lc_where    = " WHERE tipo_movimiento = 820 "
                LET ls_inserta = 1

            WHEN 24     --  DISPOSICION AL AMPARO DE UNA RESOLUCION EMITIDA POR EL IMSS
                LET lc_where    = " WHERE tipo_movimiento = 830 "
                LET ls_inserta = 1

            WHEN 25     --  DISPOSICIONES POR RETIROS PARCIALES:
                LET lc_where    = " WHERE tipo_movimiento IN (870, 875, 876, 877, 878)"
                LET ls_inserta = 1

            WHEN 26     --  MATRIMONIO
                LET lc_where    = " WHERE tipo_movimiento = 870 "
                LET ls_inserta = 1

            WHEN 27     --  DESEMPLEO
                LET lc_where    = " WHERE tipo_movimiento IN (875, 876, 877, 878)"
                LET ls_inserta = 1

            WHEN 28     --  DISPOSICIONES POR APORTACIONES VOLUNTARIAS
                LET lc_where    = " WHERE tipo_movimiento = 490 "
                LET ls_inserta = 1

            WHEN 29     --  DISPOSICIONES POR REINGRESOS
                LET lc_where    = " WHERE tipo_movimiento = 880 "
                LET ls_inserta = 1

            WHEN 30     --  DISPOSICIONES DEL PAGO DE RETIRO PROGRAMADO (INCLUYE PMG)
                LET lc_where    = " WHERE tipo_movimiento = 841 "
                LET ls_inserta = 1

            WHEN 31     --  OTRAS NO REGISTRADAS EN EL DATA MART
                LET lc_where    = " WHERE tipo_movimiento = 825 "
                LET ls_inserta = 1

            WHEN 34     --  REINVERSION NEGATIVAS POR CESANTIA E INVALIDEZ
                LET lc_where    = " WHERE tipo_movimiento = 924 AND id_aportante = 'REINV-D' "
                LET ls_inserta = 1

            WHEN 36     --  REINVERSION BAJO LEY 73
                LET lc_where    = " WHERE tipo_movimiento = 924 AND id_aportante = 'REINVER' "
                LET ls_inserta  = 1

{
            WHEN 43     --  OTROS
                LET lc_where    = " WHERE tipo_movimiento = 924 and id_aportante = 'CANCELA' "
                LET ls_inserta  = 1
}
            OTHERWISE
                LET lc_where    = " WHERE tipo_movimiento = 0 "
                LET ls_inserta  = 1

        END CASE

        IF ls_id_movimiento = 20 THEN
            -- Recuperamos los datos capturados en el detalle 3

            --  Se capturan los datos para disposicion por un tramite judicial
            CALL f_recupera_tramite_jud() RETURNING lr_det_anexo.num_solicitudes   ,
                                                   lr_det_anexo.mto_mes_actual

            LET ls_inserta = 1
        ELSE
            IF ls_inserta = 1 THEN
                -- Obtener datos para los tipos de movimiento asociados
                CALL f_obtiene_montos_actual(lc_where) RETURNING lr_det_anexo.num_solicitudes   ,
                                                                 lr_det_anexo.mto_mes_actual
            END IF
        END IF

        -- Se obtienen los datos historicos
        IF ls_inserta = 1 THEN
            CALL f_obtiene_montos_historia(ls_id_movimiento,
                                           pr_fechas_anexo.*) RETURNING lr_det_anexo.mto_mes_anterior   ,
                                                                        lr_det_anexo.mto_acumulado_anual

            -- Actualizamos el acumulado anual con el monto del mes en curso
            LET lr_det_anexo.mto_acumulado_anual    = lr_det_anexo.mto_acumulado_anual + lr_det_anexo.mto_mes_actual
        END IF

        -- Insertar en la tabla temporal de detalle
        INSERT INTO safre_tmp:tmp_det_anexo1112
        VALUES (lr_det_anexo.*)

        DISPLAY "REGISTROS DE SALIDAS IMSS      : ", ls_id_movimiento AT 10,6

    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_salidas_ISSSTE : Obtiene el detalle de los registros de salida   #
#                            del layout del anexo 1112 en la parte de       #
#                            retiros ISSSTE                                 #
#---------------------------------------------------------------------------#
FUNCTION f_genera_salidas_ISSSTE(pr_fechas_anexo_isss)

    DEFINE pr_fechas_anexo_isss  RECORD
        inicial             DATE,
        final               DATE
    END RECORD

    DEFINE lr_det_anexo     ,
           lr_totales       RECORD LIKE ret_det_anexo1112.*

    DEFINE
        lc_where                    CHAR(100)

    DEFINE
        ls_inserta                  ,
        ls_tipo                     ,
        ls_id_movimiento            SMALLINT

    -- -----------------------------------------------------------------------------

    INITIALIZE lr_det_anexo.* TO NULL
    INITIALIZE lr_totales.* TO NULL

    LET ls_id_movimiento            = 0
    LET lr_det_anexo.folio          = 1
    LET lr_det_anexo.fecha_inicio   = pr_fechas_anexo_isss.inicial
    LET lr_det_anexo.fecha_fin      = pr_fechas_anexo_isss.final

    DISPLAY "REGISTROS DE SALIDAS ISSSTE    : ", ls_id_movimiento AT 11,6

    DECLARE cur_out_issste CURSOR FOR
        SELECT id_movimiento
        FROM   tab_movimientos_anexo1112
        WHERE  tipo_cuenta      = "02"
        AND    id_movimiento    > 1000 -- Los movimientos de ISSSTE son los id mayores a mil
        ORDER BY 1

    -- Barrer los tipos de movimiento de entrada
    FOREACH cur_out_issste INTO ls_id_movimiento

        LET lr_det_anexo.id_movimiento  = ls_id_movimiento

        CASE ls_id_movimiento

            WHEN 1022   --  TOTAL DE TRANSFERENCIAS ISSSTE RECIBIDAS
                LET lc_where    = " WHERE tipo_movimiento IN (861,862,863,864,866) "
                LET ls_inserta  = 1

            WHEN 1023   --  TRANSFERENCIA DE RECURSOS ISSSTE PARA EL PAGO DE PENSION MINIMA GARANTIZADA POR ASEGURADORA
                LET lc_where    = " WHERE tipo_movimiento = 861 "
                LET ls_inserta  = 1

            WHEN 1024   --  TRANSFERENCIA DE RECURSOS ISSSTE A LAS ASEGURADORAS PARA EL PAGO DE UNA PENSION
                LET lc_where    = " WHERE tipo_movimiento = 862 "
                LET ls_inserta  = 1

            WHEN 1025   --  TRANSFERENCIA DE RECURSOS ISSSTE POR PRESCRIPCION
                LET lc_where    = " WHERE tipo_movimiento = 863 "
                LET ls_inserta  = 1

            WHEN 1026   --  TRANSFERENCIA DE RECURSOS AL ISSSTE POR MUERTE DEL PENSIONADO CON PMG
                LET lc_where    = " WHERE tipo_movimiento = 864 "
                LET ls_inserta  = 1

            WHEN 1027   --  TRANSFERENCIA DE RECURSOS ISSSTE A UNA ASEGURADORA PARA EL PAGO DEL SEGURO DE SO
                LET lc_where    = " WHERE tipo_movimiento IN (859,866) "
                LET ls_inserta  = 1

            WHEN 1029   --  TOTAL DE PAGOS REALIZADOS AL TRABAJADOR Y/O BENEFICIARIO
                LET lc_where    = " WHERE tipo_movimiento IN (854,853,855,851,852,855,856,858,857,859,866)"
                LET ls_inserta  = 1

            WHEN 1030   --  DISPOSICION ISSSTE POR UN TRAMITE JUDICIAL (INSTRUCCIÓN DE LA AUTORIDAD)
                LET lc_where    = " WHERE tipo_movimiento = 854 "
                LET ls_inserta  = 1

            WHEN 1031   --  DISPOSICION ISSSTE POR PLANES PRIVADOS DE PENSION
                LET lc_where    = " WHERE tipo_movimiento = 853 "
                LET ls_inserta  = 1

            WHEN 1032   --  DISPOSICION ISSSTE POR EDAD DEL TRABAJADOR
                LET lc_where    = " WHERE tipo_movimiento = 855 "
                LET ls_inserta  = 1

            WHEN 1033   --  DISPOSICION ISSSTE POR UNA NEGATIVA DE PENSION
                LET lc_where    = " WHERE tipo_movimiento = 851 "
                LET ls_inserta  = 1

            WHEN 1034   --  DISPOSICION AL AMPARO DE UNA CONCESION DE PENSION EMITIDA POR EL ISSSTE
                LET lc_where    = " WHERE tipo_movimiento = 852 "
                LET ls_inserta  = 1

            WHEN 1035   --  TOTAL DE DISPOSICIONES ISSSTE POR RETIROS PARCIALES
                LET lc_where    = " WHERE (tipo_movimiento = 856) OR (tipo_movimiento = 885) "
                LET ls_inserta  = 1

            WHEN 1036   --  DESEMPLEO REGIMEN 10MO TRANSITORIO ISSSTE
                LET lc_where    = " WHERE (tipo_movimiento = 885) OR (tipo_movimiento = 856 AND subcuenta IN (13,19)) "
                LET ls_inserta  = 1

            WHEN 1037   --  DESEMPLEO REGIMEN ORDINARIO ISSSTE
                LET lc_where    = " WHERE tipo_movimiento = 856 AND subcuenta IN (30,31,32) "
                LET ls_inserta  = 1

            WHEN 1038   --  DISPOSICION POR APORTACIONES VOLUNTARIAS
                LET lc_where    = " WHERE tipo_movimiento = 0 "
                LET ls_inserta  = 1

            WHEN 1039   --  DISPOSICION ISSSTE POR REINGRESOS
                LET lc_where    = " WHERE tipo_movimiento = 858 "
                LET ls_inserta  = 1

            WHEN 1040   --  DISPOSICIONES ISSSTE DEL PAGO DE RETIRO PROGRAMADO Y PMG
                LET lc_where    = " WHERE tipo_movimiento IN (857,859,866) "
                LET ls_inserta  = 1

            WHEN 1041   --  OTRAS NO REGISTRADAS EN EL DATAMART ISSSTE
                LET lc_where    = " WHERE tipo_movimiento = 0 "
                LET ls_inserta  = 1

            OTHERWISE
                LET lc_where    = " WHERE tipo_movimiento = 0 "
                LET ls_inserta  = 1

        END CASE

        IF ls_inserta = 1 THEN
            -- Obtener datos para los tipos de movimiento asociados
            CALL f_obtiene_montos_actual(lc_where) RETURNING lr_det_anexo.num_solicitudes   ,
                                                             lr_det_anexo.mto_mes_actual

            -- Obtener datos historicos
            CALL f_obtiene_montos_historia(ls_id_movimiento,
                                           pr_fechas_anexo_isss.*) RETURNING lr_det_anexo.mto_mes_anterior   ,
                                                                        lr_det_anexo.mto_acumulado_anual

            -- Actualizamos el acumulado anual con el monto del mes en curso
            LET lr_det_anexo.mto_acumulado_anual    = lr_det_anexo.mto_acumulado_anual + lr_det_anexo.mto_mes_actual
        END IF

        -- Insertar en la tabla temporal de detalle
        INSERT INTO safre_tmp:tmp_det_anexo1112
        VALUES (lr_det_anexo.*)

        LET ls_tipo = ls_tipo + 1
        DISPLAY "REGISTROS DE SALIDAS ISSSTE    : ", ls_tipo AT 11,6

    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# f_recupera_tramite_jud : Recupera los montos capturados por el usuario    #
#                          en la entrada de tramite judicial para usarlos   #
#                          en el rubro de salida correspondiente            #
#---------------------------------------------------------------------------#
FUNCTION f_recupera_tramite_jud()

    DEFINE lr_captura RECORD
        num_solicitudes     LIKE ret_det_anexo1112.num_solicitudes  ,
        mto_mes_actual      LIKE ret_det_anexo1112.mto_mes_actual
    END RECORD

    -- -----------------------------------------------------------------------------
    SELECT num_solicitudes  ,
           mto_mes_actual
    INTO   lr_captura.*
    FROM   safre_tmp:tmp_det_anexo1112
    WHERE  id_movimiento    = 3 -- Entrada de tramites judiciales

    RETURN lr_captura.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_actualiza_tot_salidas : Corre las funciones que realizan la             #
#                           actualizacion de los montos de totales          #
#---------------------------------------------------------------------------#
FUNCTION f_actualiza_tot_salidas()

    CALL f_actualiza_parciales_imss_salida()
    CALL f_actualiza_total_pagos_imss()
    CALL f_genera_reinversiones_imss()
    CALL f_actualiza_reinv_por_retiros()
    CALL f_actualiza_res_emitida_IMSS()
    CALL f_actualiza_total_reinv()

END FUNCTION

#---------------------------------------------------------------------------#
# f_actualiza_parciales_imss_salida : Actualiza el rubro 25 (Total Par IMSS) #
#                           con los datos generados de cada subcategoria    #
#---------------------------------------------------------------------------#
FUNCTION f_actualiza_parciales_imss_salida()

    DEFINE lr_reinv_tot RECORD
        num_solicitudes         LIKE ret_det_anexo1112.num_solicitudes      ,
        mto_mes_actual          LIKE ret_det_anexo1112.mto_mes_actual       ,
        mto_mes_anterior        LIKE ret_det_anexo1112.mto_mes_anterior     ,
        mto_acumulado_anual     LIKE ret_det_anexo1112.mto_acumulado_anual
    END RECORD

    -- -----------------------------------------------------------------------------

    SELECT NVL(SUM(num_solicitudes), 0)     ,
           NVL(SUM(mto_mes_actual), 0)      ,
           NVL(SUM(mto_mes_anterior),0)     ,
           NVL(SUM(mto_acumulado_anual),0)
    INTO   lr_reinv_tot.*
    FROM   safre_tmp:tmp_det_anexo1112
    WHERE  id_movimiento IN (26,27)

    UPDATE safre_tmp:tmp_det_anexo1112
    SET    num_solicitudes      = lr_reinv_tot.num_solicitudes      ,
           mto_mes_actual       = lr_reinv_tot.mto_mes_actual       ,
           mto_mes_anterior     = lr_reinv_tot.mto_mes_anterior     ,
           mto_acumulado_anual  = lr_reinv_tot.mto_acumulado_anual
    WHERE  id_movimiento        = 25

END FUNCTION

#---------------------------------------------------------------------------#
# f_actualiza_total_pagos_imss : Actualiza el rubro 19 (Total de pagos -    #
#                            disposiciones) con los datos generados de cada #
#                            subcategoria                                   #
#---------------------------------------------------------------------------#
FUNCTION f_actualiza_total_pagos_imss()

    DEFINE lr_reinv RECORD
        num_solicitudes         LIKE ret_det_anexo1112.num_solicitudes      ,
        mto_mes_actual          LIKE ret_det_anexo1112.mto_mes_actual       ,
        mto_mes_anterior        LIKE ret_det_anexo1112.mto_mes_anterior     ,
        mto_acumulado_anual     LIKE ret_det_anexo1112.mto_acumulado_anual
    END RECORD

    -- -----------------------------------------------------------------------------

    SELECT NVL(SUM(num_solicitudes), 0)     ,
           NVL(SUM(mto_mes_actual), 0)      ,
           NVL(SUM(mto_mes_anterior),0)     ,
           NVL(SUM(mto_acumulado_anual),0)
    INTO   lr_reinv.*
    FROM   safre_tmp:tmp_det_anexo1112
    WHERE  ( (id_movimiento BETWEEN 20 AND 25) OR
             (id_movimiento BETWEEN 28 AND 31) OR
             (id_movimiento BETWEEN 46 AND 47)
           )

    UPDATE safre_tmp:tmp_det_anexo1112
    SET    num_solicitudes      = lr_reinv.num_solicitudes      ,
           mto_mes_actual       = lr_reinv.mto_mes_actual       ,
           mto_mes_anterior     = lr_reinv.mto_mes_anterior     ,
           mto_acumulado_anual  = lr_reinv.mto_acumulado_anual
    WHERE  id_movimiento        = 19

END FUNCTION

#---------------------------------------------------------------------------#
# f_actualiza_res_emitida_IMSS : Actualiza el rubro 33 (Por resolucion      #
#                                emitida por el IMSS) con los datos         #
#                                generados de cada subcategoria             #
#---------------------------------------------------------------------------#
FUNCTION f_actualiza_res_emitida_IMSS()

    DEFINE lr_reinv RECORD
        num_solicitudes         LIKE ret_det_anexo1112.num_solicitudes      ,
        mto_mes_actual          LIKE ret_det_anexo1112.mto_mes_actual       ,
        mto_mes_anterior        LIKE ret_det_anexo1112.mto_mes_anterior     ,
        mto_acumulado_anual     LIKE ret_det_anexo1112.mto_acumulado_anual
    END RECORD

    -- -----------------------------------------------------------------------------

    SELECT NVL(SUM(num_solicitudes), 0)     ,
           NVL(SUM(mto_mes_actual), 0)      ,
           NVL(SUM(mto_mes_anterior),0)     ,
           NVL(SUM(mto_acumulado_anual),0)
    INTO   lr_reinv.*
    FROM   safre_tmp:tmp_det_anexo1112
    WHERE  ( (id_movimiento BETWEEN 34 AND 38) OR
             (id_movimiento BETWEEN 48 AND 49)
           )

    UPDATE safre_tmp:tmp_det_anexo1112
    SET    num_solicitudes      = lr_reinv.num_solicitudes      ,
           mto_mes_actual       = lr_reinv.mto_mes_actual       ,
           mto_mes_anterior     = lr_reinv.mto_mes_anterior     ,
           mto_acumulado_anual  = lr_reinv.mto_acumulado_anual
    WHERE  id_movimiento        = 33

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_reinversiones_imss : Obtiene los registros correspondientes a    #
#                               las reinversiones IMSS barriendo todos los  #
#                               tipos de movimiento 923 del estracto de     #
#                               dis_cuenta                                  #
#---------------------------------------------------------------------------#
FUNCTION f_genera_reinversiones_imss()

    DEFINE lar_reinversion ARRAY[6] OF RECORD
        num_solicitudes         LIKE ret_det_anexo1112.num_solicitudes      ,
        mto_mes_actual          LIKE ret_det_anexo1112.mto_mes_actual       ,
        mto_mes_anterior        LIKE ret_det_anexo1112.mto_mes_anterior     ,
        mto_acumulado_anual     LIKE ret_det_anexo1112.mto_acumulado_anual
    END RECORD

    DEFINE lr_montos RECORD
        nss                 LIKE dis_cuenta.nss             ,
        mto_pesos           LIKE dis_cuenta.monto_en_pesos
    END RECORD

    DEFINE
        ls_tipo             SMALLINT

    -- -----------------------------------------------------------------------------

    -- Se inicializan registros y arreglos
    INITIALIZE lr_montos.* TO NULL
    LET lar_reinversion[1].num_solicitudes  = 0
    LET lar_reinversion[1].mto_mes_actual   = 0

    FOR ls_tipo = 2 TO 6
        LET lar_reinversion[ls_tipo].*  = lar_reinversion[1].*
    END FOR

    LET ls_tipo = 0

    DECLARE cur_reinv CURSOR FOR
        SELECT nss                          ,
               round(SUM(monto_en_pesos),2)
        FROM   tmp_dis_cuenta_1112
        WHERE  tipo_movimiento = 924
        GROUP BY 1
        ORDER BY 1


    -- El orden de los registros en el arreglo es el siguiente :
    -- 1 - NEGATIVAS POR CESANTIA E INVALIDEZ
    -- 2 - RETIROS PROGRAMADOS
    -- 3 - BAJO LEY 73
    -- 4 - INCAPACIDADES PARCIALES MENORES AL 50%
    -- 5 - POR UNA RENTA VITALICIA
    -- 6 - POR AUSENCIA (VIGENCIA VENCIDA) DE CARGA DE RESOLUCION

    FOREACH cur_reinv INTO lr_montos.*

        LET ls_tipo = f_encuentra_tipo_reinv(lr_montos.nss)

        LET lar_reinversion[ls_tipo].num_solicitudes    = lar_reinversion[ls_tipo].num_solicitudes + 1
        LET lar_reinversion[ls_tipo].mto_mes_actual     = lar_reinversion[ls_tipo].mto_mes_actual + lr_montos.mto_pesos

    END FOREACH

    -- inserta en la tabla los resultados
    UPDATE safre_tmp:tmp_det_anexo1112
    SET    num_solicitudes      = lar_reinversion[1].num_solicitudes    ,
           mto_mes_actual       = lar_reinversion[1].mto_mes_actual
    WHERE  id_movimiento        = 34

    UPDATE safre_tmp:tmp_det_anexo1112
    SET    num_solicitudes      = lar_reinversion[2].num_solicitudes    ,
           mto_mes_actual       = lar_reinversion[2].mto_mes_actual
    WHERE  id_movimiento        = 35

    UPDATE safre_tmp:tmp_det_anexo1112
    SET    num_solicitudes      = lar_reinversion[3].num_solicitudes    ,
           mto_mes_actual       = lar_reinversion[3].mto_mes_actual
    WHERE  id_movimiento        = 36

    UPDATE safre_tmp:tmp_det_anexo1112
    SET    num_solicitudes      = lar_reinversion[4].num_solicitudes    ,
           mto_mes_actual       = lar_reinversion[4].mto_mes_actual
    WHERE  id_movimiento        = 37

    UPDATE safre_tmp:tmp_det_anexo1112
    SET    num_solicitudes      = lar_reinversion[5].num_solicitudes    ,
           mto_mes_actual       = lar_reinversion[5].mto_mes_actual
    WHERE  id_movimiento        = 38

    UPDATE safre_tmp:tmp_det_anexo1112
    SET    num_solicitudes      = lar_reinversion[6].num_solicitudes    ,
           mto_mes_actual       = lar_reinversion[6].mto_mes_actual
    WHERE  id_movimiento        = 48

END FUNCTION

#---------------------------------------------------------------------------#
# f_actualiza_reinv_por_retiros : Actualiza el rubro 39 (Reinversiones por  #
#                                 Retiros) con los datos generados de cada  #
#                                 subcategoria                              #
#---------------------------------------------------------------------------#
FUNCTION f_actualiza_reinv_por_retiros()

    DEFINE lr_reinv_ret RECORD
        num_solicitudes         LIKE ret_det_anexo1112.num_solicitudes      ,
        mto_mes_actual          LIKE ret_det_anexo1112.mto_mes_actual       ,
        mto_mes_anterior        LIKE ret_det_anexo1112.mto_mes_anterior     ,
        mto_acumulado_anual     LIKE ret_det_anexo1112.mto_acumulado_anual
    END RECORD

    -- -----------------------------------------------------------------------------

    SELECT NVL(SUM(num_solicitudes), 0)     ,
           NVL(SUM(mto_mes_actual), 0)      ,
           NVL(SUM(mto_mes_anterior),0)     ,
           NVL(SUM(mto_acumulado_anual),0)
    INTO   lr_reinv_ret.*
    FROM   safre_tmp:tmp_det_anexo1112
    WHERE  id_movimiento BETWEEN 40 AND 43

    UPDATE safre_tmp:tmp_det_anexo1112
    SET    num_solicitudes      = lr_reinv_ret.num_solicitudes      ,
           mto_mes_actual       = lr_reinv_ret.mto_mes_actual       ,
           mto_mes_anterior     = lr_reinv_ret.mto_mes_anterior     ,
           mto_acumulado_anual  = lr_reinv_ret.mto_acumulado_anual
    WHERE  id_movimiento        = 39

END FUNCTION

#---------------------------------------------------------------------------#
# f_actualiza_total_reinv : Actualiza el rubro 32 (Total Reinversiones)     #
#                           con los datos generados de cada subcategoria    #
#---------------------------------------------------------------------------#
FUNCTION f_actualiza_total_reinv()

    DEFINE lr_reinv_tot RECORD
        num_solicitudes         LIKE ret_det_anexo1112.num_solicitudes      ,
        mto_mes_actual          LIKE ret_det_anexo1112.mto_mes_actual       ,
        mto_mes_anterior        LIKE ret_det_anexo1112.mto_mes_anterior     ,
        mto_acumulado_anual     LIKE ret_det_anexo1112.mto_acumulado_anual
    END RECORD

    -- -----------------------------------------------------------------------------

    SELECT NVL(SUM(num_solicitudes), 0)     ,
           NVL(SUM(mto_mes_actual), 0)      ,
           NVL(SUM(mto_mes_anterior),0)     ,
           NVL(SUM(mto_acumulado_anual),0)
    INTO   lr_reinv_tot.*
    FROM   safre_tmp:tmp_det_anexo1112
    WHERE  id_movimiento IN (33,39)

    UPDATE safre_tmp:tmp_det_anexo1112
    SET    num_solicitudes      = lr_reinv_tot.num_solicitudes      ,
           mto_mes_actual       = lr_reinv_tot.mto_mes_actual       ,
           mto_mes_anterior     = lr_reinv_tot.mto_mes_anterior     ,
           mto_acumulado_anual  = lr_reinv_tot.mto_acumulado_anual
    WHERE  id_movimiento        = 32

END FUNCTION

#---------------------------------------------------------------------------#
# f_encuentra_tipo_reinv : Dado el nss busca en ret_det_datamart el tipo de #
#                          retiro y determina el tipo de reinversion        #
#---------------------------------------------------------------------------#
FUNCTION f_encuentra_tipo_reinv(pc_nss)

    DEFINE pc_nss LIKE dis_cuenta.nss

    DEFINE lr_dtm RECORD
        tipo_retiro         LIKE ret_det_datamart.tipo_retiro       ,
        regimen             LIKE ret_det_datamart.regimen           ,
        tipo_pension        LIKE ret_det_datamart.tipo_pension      ,
        porcentaje_val      LIKE ret_det_datamart.porcentaje_val
    END RECORD

    DEFINE ldt_fec_carga_datamart   LIKE ret_det_datamart.fec_carga_datamart

    DEFINE
        ls_tipo_reinv           SMALLINT

        DEFINE enter CHAR(1)
    -- -----------------------------------------------------------------------------

    LET ls_tipo_reinv   = 0
    INITIALIZE ldt_fec_carga_datamart TO NULL
    INITIALIZE lr_dtm.* TO NULL

    -- Las claves de tipo de reinversion a retornar son :
    -- 1 - NEGATIVAS POR CESANTIA E INVALIDEZ
    -- 2 - RETIROS PROGRAMADOS
    -- 3 - BAJO LEY 73
    -- 4 - INCAPACIDADES PARCIALES MENORES AL 50%
    -- 5 - POR UNA RENTA VITALICIA
    -- 6 - POR AUSENCIA (VIGENCIA VENCIDA) DE CARGA DE RESOLUCION

--DISPLAY "pc_nss = ", pc_nss

    SELECT MAX(fec_carga_datamart)
    INTO   ldt_fec_carga_datamart
    FROM   ret_det_datamart
    WHERE  nss          = pc_nss
    AND    estado       <> 114
    AND    folio        <> 0
    AND    diag_datamart = 101
    AND    id_registro IS NOT NULL

    IF ldt_fec_carga_datamart IS NULL THEN
        -- Si no tiene tipo de retiro es por ausencia
        LET ls_tipo_reinv   = 6
    ELSE
        SELECT tipo_retiro      ,
               regimen          ,
               tipo_pension     ,
               porcentaje_val
        INTO   lr_dtm.*
        FROM   ret_det_datamart
        WHERE  nss                  = pc_nss
        AND    fec_carga_datamart   = ldt_fec_carga_datamart
--        AND    estado       <> 114
--        AND    folio        <> 0
        AND    id_registro IS NOT NULL

        CASE lr_dtm.tipo_retiro
            WHEN "A"
                IF lr_dtm.tipo_pension = "IP" AND lr_dtm.porcentaje_val < 50 THEN
                    LET ls_tipo_reinv   = 4
                ELSE
                    LET ls_tipo_reinv   = 5
                END IF

            WHEN "B"
                LET ls_tipo_reinv   = 3

            WHEN "C"
                LET ls_tipo_reinv   = 6

            WHEN "D"
                LET ls_tipo_reinv   = 1

            WHEN "S"
                LET ls_tipo_reinv   = 2

        END CASE
    END IF
{
DISPLAY "ls_tipo_reinv = ", ls_tipo_reinv
PROMPT ".." FOR CHAR enter
}

    RETURN ls_tipo_reinv

END FUNCTION