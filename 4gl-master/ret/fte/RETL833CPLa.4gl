#################################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                               #
#Owner             => E.F.P.                                                    #
#Programa RETL833b => FUNCIONES PARTICULARES PARA REALIZAR EL RUBRO DE ENTRADAS #
#                     DEL ANEXO 1112                                            #
#                                    VERSION COPPEL                             #
#Fecha creacion    => 21 DE AGOSTO DE 2012                                      #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Sistema           => RET                                                       #
#################################################################################

DATABASE safre_af

#---------------------------------------------------------------------------#
# f_genera_entradas : Obtiene el detalle de los registros de entrada del    #
#                     layout del anexo 1112 en la parte de retiros IMSS     #
#---------------------------------------------------------------------------#
FUNCTION f_genera_entradas(pr_fechas_anexo)

    DEFINE pr_fechas_anexo  RECORD
        inicial             DATE,
        final               DATE
    END RECORD

    DEFINE lr_det_anexo     ,
           lr_totales       RECORD LIKE ret_det_anexo1112.*

    DEFINE
        lc_where                    CHAR(100)

    DEFINE
        ls_inserta                  ,
        ls_id_movimiento            SMALLINT

    -- -----------------------------------------------------------------------------

    INITIALIZE lr_det_anexo.* TO NULL

    LET ls_id_movimiento            = 0
    LET lr_det_anexo.folio          = 1
    LET lr_det_anexo.fecha_inicio   = pr_fechas_anexo.inicial
    LET lr_det_anexo.fecha_fin      = pr_fechas_anexo.final

    DISPLAY "REGISTROS DE ENTRADAS IMSS     : ", ls_id_movimiento AT 8,6

    DECLARE cur_in_imss CURSOR FOR
        SELECT id_movimiento
        FROM   tab_movimientos_anexo1112
        WHERE  tipo_cuenta      = "01"
        AND    id_movimiento    < 1000 -- Los movimientos de IMSS son los id menores a mil
        ORDER BY 1

    -- Barrer los tipos de movimiento de entrada
    FOREACH cur_in_imss INTO ls_id_movimiento
        
        LET lr_det_anexo.id_movimiento  = ls_id_movimiento

        CASE ls_id_movimiento

            WHEN 2      --  SALDOS PREVIOS
                LET lc_where    = " WHERE tipo_movimiento = 921 "
                LET ls_inserta  = 1

            WHEN 3      --  DISPOSICION POR UN TRAMITE JUDICIAL
                LET ls_inserta = 1
                EXIT CASE

            WHEN 4      --  DISPOSICION POR PLANES PRIVADOS DE PENSION
                LET lc_where    = " WHERE tipo_movimiento = 840 "
                LET ls_inserta  = 1
                
            WHEN 5      --  DISPOSICIONES POR EDAD DEL TRABAJADOR (VEJEZ)
                LET lc_where    = " WHERE tipo_movimiento = 860 "
                LET ls_inserta  = 1
                
            WHEN 6      --  DISPOSICIONES POR UNA NEGATIVAS DE PENSION
                LET lc_where    = " WHERE tipo_movimiento = 820 "
                LET ls_inserta  = 1
                
            WHEN 7      --  DISPOSICION AL AMPARO DE UNA RESOLUCION EMITIDA POR EL IMSS
                LET lc_where    = " WHERE tipo_movimiento = 830 "
                LET ls_inserta  = 1

            WHEN 9      --  MATRIMONIO
                LET lc_where    = " WHERE tipo_movimiento = 870"
                LET ls_inserta  = 1
                
            WHEN 10     --  DESEMPLEO
                LET lc_where    = " WHERE tipo_movimiento IN (875, 876, 877, 878)"
                LET ls_inserta  = 1

            WHEN 11     --  DISPOSICION POR APORTACIONES VOLUNTARIAS
                LET lc_where    = " WHERE tipo_movimiento = 490 "
                LET ls_inserta  = 1

            WHEN 12     --  DISPOSICION POR REINGRESOS
                LET lc_where    = " WHERE tipo_movimiento = 880 "
                LET ls_inserta  = 1

            WHEN 13     --  DISPOSICIONES DEL PAGO DE RETIRO PROGRAMADO (INCLUYE PMG)
                LET lc_where    = " WHERE tipo_movimiento = 841 "
                LET ls_inserta  = 1

            WHEN 14     --  OTRAS NO REGISTRADAS EN EL DATA MART
                LET lc_where    = " WHERE tipo_movimiento = 825 "
                LET ls_inserta  = 1

            OTHERWISE
                LET lc_where    = " WHERE tipo_movimiento = 0 "
                LET ls_inserta  = 1

        END CASE

        IF ls_id_movimiento = 3 THEN 
            --  Se capturan los datos para disposicion por un tramite judicial
            CALL f_captura_tramite_jud() RETURNING lr_det_anexo.num_solicitudes   ,
                                                   lr_det_anexo.mto_mes_actual
            LET ls_inserta  = 1
        ELSE
            IF ls_inserta = 1 THEN
                -- Obtener datos para los tipos de movimiento asociados
                CALL f_obtiene_montos_actual(lc_where) RETURNING lr_det_anexo.num_solicitudes   ,
                                                                 lr_det_anexo.mto_mes_actual
            END IF
        END IF 
        
        IF ls_inserta = 1 THEN 
            -- Obtener datos historicos
            CALL f_obtiene_montos_historia(ls_id_movimiento,
                                           pr_fechas_anexo.*) RETURNING lr_det_anexo.mto_mes_anterior   ,
                                                                        lr_det_anexo.mto_acumulado_anual
            
            -- Actualizamos el acumulado anual con el monto del mes en curso
            LET lr_det_anexo.mto_acumulado_anual    = lr_det_anexo.mto_acumulado_anual + lr_det_anexo.mto_mes_actual

        END IF 

        INSERT INTO safre_tmp:tmp_det_anexo1112
        VALUES (lr_det_anexo.*)

        DISPLAY "REGISTROS DE ENTRADAS IMSS     : ", ls_id_movimiento AT 8,6

    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_entradas_ISSSTE : Obtiene el detalle de los registros de entrada #
#                            del layout del anexo 1112 en la parte de       #
#                            retiros ISSSTE                                 #
#---------------------------------------------------------------------------#
FUNCTION f_genera_entradas_ISSSTE(pr_fechas_anexo_isss)

    DEFINE pr_fechas_anexo_isss  RECORD
        inicial             DATE,
        final               DATE
    END RECORD

    DEFINE lr_det_anexo     RECORD LIKE ret_det_anexo1112.*

    DEFINE
        lc_where                    CHAR(100)

    DEFINE
        ls_inserta                  ,
        ls_id_movimiento            SMALLINT

    -- -----------------------------------------------------------------------------

    INITIALIZE lr_det_anexo.* TO NULL

    LET ls_id_movimiento            = 0
    LET lr_det_anexo.folio          = 1
    LET lr_det_anexo.fecha_inicio   = pr_fechas_anexo_isss.inicial
    LET lr_det_anexo.fecha_fin      = pr_fechas_anexo_isss.final
    
    DISPLAY "REGISTROS DE ENTRADAS ISSSTE   : ", ls_id_movimiento AT 9,6

    DECLARE cur_in_issste CURSOR FOR
        SELECT id_movimiento
        FROM   tab_movimientos_anexo1112
        WHERE  tipo_cuenta      = "01"
        AND    id_movimiento    > 1000 -- Los movimientos de ISSSTE son los id mayores a mil
        ORDER BY 1

    -- Barrer los tipos de movimiento de entrada
    FOREACH cur_in_issste INTO ls_id_movimiento
        
        LET lr_det_anexo.id_movimiento  = ls_id_movimiento

        CASE ls_id_movimiento

            WHEN 1002   --  SALDOS PREVIOS
                LET lc_where    = " WHERE tipo_movimiento = 0   "
                LET ls_inserta  = 1

            WHEN 1003   --  DISPOSICION ISSSTE POR UN TRAMITE JUDICIAL (INSTRUCCIÓN DE LA AUTORIDAD)
                LET lc_where    = " WHERE tipo_movimiento = 854 "
                LET ls_inserta  = 1

            WHEN 1004   --  DISPOSICION ISSSTE POR PLANES PRIVADOS DE PENSION
                LET lc_where    = " WHERE tipo_movimiento = 853 "
                LET ls_inserta  = 1

            WHEN 1005   --  DISPOSICION ISSSTE POR EDAD DEL TRABAJADOR
                LET lc_where    = " WHERE tipo_movimiento = 855 "
                LET ls_inserta  = 1

            WHEN 1006   --  DISPOSICION ISSSTE POR UNA NEGATIVA DE PENSION
                LET lc_where    = " WHERE tipo_movimiento = 851 "
                LET ls_inserta  = 1

            WHEN 1007   --  DISPOSICION AL AMPARO DE UNA CONCESION DE PENSION EMITIDA POR EL ISSSTE
                LET lc_where    = " WHERE tipo_movimiento = 852 "
                LET ls_inserta  = 1

            WHEN 1008   --  TOTAL DE DISPOSICIONES ISSSTE POR RETIROS PARCIALES
                LET lc_where    = " WHERE (tipo_movimiento = 856) OR (tipo_movimiento = 885) "
                LET ls_inserta  = 1

            WHEN 1009   --  DESEMPLEO REGIMEN 10MO TRANSITORIO ISSSTE
                LET lc_where    = " WHERE (tipo_movimiento = 885) OR (tipo_movimiento = 856 AND subcuenta IN (13,19)) "
                LET ls_inserta  = 1

            WHEN 1010   --  DESEMPLEO REGIMEN ORDINARIO ISSSTE
                LET lc_where    = " WHERE tipo_movimiento = 856 AND subcuenta IN (30,31,32) "
                LET ls_inserta  = 1

            WHEN 1011   --  DISPOSICION POR APORTACIONES VOLUNTARIAS
                LET lc_where    = " WHERE tipo_movimiento = 0 "
                LET ls_inserta  = 1

            WHEN 1012   --  DISPOSICION ISSSTE POR REINGRESOS
                LET lc_where    = " WHERE tipo_movimiento = 858 "
                LET ls_inserta  = 1

            WHEN 1013   --  DISPOSICIONES ISSSTE DEL PAGO DE RETIRO PROGRAMADO Y PMG
                LET lc_where    = " WHERE tipo_movimiento IN (857,859,866) "
                LET ls_inserta  = 1

            WHEN 1014   --  OTRAS NO REGISTRADAS EN EL DATAMART ISSSTE
                LET lc_where    = " WHERE tipo_movimiento = 0 "
                LET ls_inserta  = 1

            WHEN 1015   --  TOTAL DE TRANSFERENCIAS ISSSTE RECIBIDAS                                        
                LET lc_where    = " WHERE tipo_movimiento IN (861,862,863,864,866) "
                LET ls_inserta  = 1

            WHEN 1016   --  TRANSFERENCIA DE RECURSOS ISSSTE PARA EL PAGO DE PENSION MINIMA GARANTIZADA POR ASEGURADORA
                LET lc_where    = " WHERE tipo_movimiento = 861 "
                LET ls_inserta  = 1

            WHEN 1017   --  TRANSFERENCIA DE RECURSOS ISSSTE A LAS ASEGURADORAS PARA EL PAGO DE UNA PENSION 
                LET lc_where    = " WHERE tipo_movimiento = 862 "
                LET ls_inserta  = 1

            WHEN 1018   --  TRANSFERENCIA DE RECURSOS ISSSTE POR PRESCRIPCION                               
                LET lc_where    = " WHERE tipo_movimiento = 863 "
                LET ls_inserta  = 1

            WHEN 1019   --  TRANSFERENCIA DE RECURSOS AL ISSSTE POR MUERTE DEL PENSIONADO CON PMG           
                LET lc_where    = " WHERE tipo_movimiento = 864 "
                LET ls_inserta  = 1

            WHEN 1020   --  TRANSFERENCIA DE RECURSOS ISSSTE A UNA ASEGURADORA PARA EL PAGO DEL SEGURO DE SO
                LET lc_where    = " WHERE tipo_movimiento IN (859,866) "
                LET ls_inserta  = 1

            WHEN 1021   --  TRANFERENCIA DE RECURSOS SARISSSTE (ISSSTE-ICEFAS)                              
                LET lc_where    = " WHERE tipo_movimiento BETWEEN 881 AND 889 "
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
    
        LET ls_id_movimiento = ls_id_movimiento - 1000

        DISPLAY "REGISTROS DE ENTRADAS ISSSTE   : ", ls_id_movimiento AT 9,6

    END FOREACH


END FUNCTION

#---------------------------------------------------------------------------#
# f_captura_tramite_jud : Ejecuta la pantalla de captura para COPPEL la     #
#                         cual se ingresan los valores del monto mensual    #
#                         y el numero de solicitudes para Tramite Judicial  #
#---------------------------------------------------------------------------#
FUNCTION f_captura_tramite_jud()

    DEFINE lr_captura RECORD
        num_solicitudes     LIKE ret_det_anexo1112.num_solicitudes  ,
        mto_mes_actual      LIKE ret_det_anexo1112.mto_mes_actual 
    END RECORD 
    

    -- -----------------------------------------------------------------------------

    INITIALIZE lr_captura.* TO NULL

    OPEN WINDOW RETL8334 AT 10,13 WITH FORM "RETL8334" ATTRIBUTE(BORDER)
    DISPLAY " <ESC> - Ejecutar                                        " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETL833       CAPTURA DATOS - TRAMITE JUDICIAL          " AT 3,1 ATTRIBUTE(REVERSE)        

    INPUT BY NAME lr_captura.* WITHOUT DEFAULTS

        AFTER FIELD num_solicitudes
            IF lr_captura.num_solicitudes < 0 THEN
                CALL f_lib_error_msg("DEBE SER MAYOR A CERO")
                NEXT FIELD num_solicitudes
            END IF

        ON KEY(ESC)
            IF lr_captura.num_solicitudes < 0 THEN
                CALL f_lib_error_msg("DEBE SER MAYOR A CERO")
                NEXT FIELD num_solicitudes
            ELSE
                EXIT INPUT
            END IF            

    END INPUT

    CLOSE WINDOW RETL8334 

    RETURN lr_captura.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_actualiza_tot_entradas : Corre las funciones que realizan la            #
#                            actualizacion de los montos de totales         #
#---------------------------------------------------------------------------#
FUNCTION f_actualiza_tot_entradas()

    CALL f_actualiza_tot_parciales_imss()
    CALL f_actualiza_total_imss()
    CALL f_actualiza_tot_parciales_ISSSTE()
    CALL f_actualiza_tot_transfer_ISSSTE()
    CALL f_actualiza_total_ISSSTE()

END FUNCTION

#---------------------------------------------------------------------------#
# f_actualiza_total_imss : Actualiza el rubro 1 (Total de entradas IMSS)    #
#                          con los datos generados de cada subcategoria     #
#---------------------------------------------------------------------------#
FUNCTION f_actualiza_total_imss()

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
    WHERE  ( (id_movimiento BETWEEN  2 AND  8) OR 
             (id_movimiento BETWEEN 11 AND 14) OR
             (id_movimiento BETWEEN 44 AND 45)
           )

    UPDATE safre_tmp:tmp_det_anexo1112
    SET    num_solicitudes      = lr_reinv.num_solicitudes      ,
           mto_mes_actual       = lr_reinv.mto_mes_actual       ,
           mto_mes_anterior     = lr_reinv.mto_mes_anterior     ,
           mto_acumulado_anual  = lr_reinv.mto_acumulado_anual
    WHERE  id_movimiento        = 1

END FUNCTION

#---------------------------------------------------------------------------#
# f_actualiza_tot_parciales_imss : Actualiza el rubro 8 (Total Par IMSS)    #
#                           con los datos generados de cada subcategoria    #
#---------------------------------------------------------------------------#
FUNCTION f_actualiza_tot_parciales_imss()

    DEFINE lr_tot_par_imss RECORD
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
    INTO   lr_tot_par_imss.*
    FROM   safre_tmp:tmp_det_anexo1112
    WHERE  id_movimiento IN (9,10)

    UPDATE safre_tmp:tmp_det_anexo1112
    SET    num_solicitudes      = lr_tot_par_imss.num_solicitudes      ,
           mto_mes_actual       = lr_tot_par_imss.mto_mes_actual       ,
           mto_mes_anterior     = lr_tot_par_imss.mto_mes_anterior     ,
           mto_acumulado_anual  = lr_tot_par_imss.mto_acumulado_anual
    WHERE  id_movimiento        = 8

END FUNCTION

#---------------------------------------------------------------------------#
# f_actualiza_tot_parciales_ISSSTE : Actualiza el rubro 1008 (Total Par     #
#                                    ISSSTE) con los datos generados de cada# 
#                                    subcategoria                           #
#---------------------------------------------------------------------------#
FUNCTION f_actualiza_tot_parciales_ISSSTE()

    DEFINE lr_tot_par_issste RECORD
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
    INTO   lr_tot_par_issste.*
    FROM   safre_tmp:tmp_det_anexo1112
    WHERE  id_movimiento IN (1009, 1010)

    UPDATE safre_tmp:tmp_det_anexo1112
    SET    num_solicitudes      = lr_tot_par_issste.num_solicitudes      ,
           mto_mes_actual       = lr_tot_par_issste.mto_mes_actual       ,
           mto_mes_anterior     = lr_tot_par_issste.mto_mes_anterior     ,
           mto_acumulado_anual  = lr_tot_par_issste.mto_acumulado_anual
    WHERE  id_movimiento        = 1008

END FUNCTION

#---------------------------------------------------------------------------#
# f_actualiza_tot_transfer_ISSSTE : Actualiza el rubro 1015 (Total Transfer #
#                                   ISSSTE) con los datos generados de cada # 
#                                   subcategoria                            #
#---------------------------------------------------------------------------#
FUNCTION f_actualiza_tot_transfer_ISSSTE()

    DEFINE lr_tot_trans_issste RECORD
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
    INTO   lr_tot_trans_issste.*
    FROM   safre_tmp:tmp_det_anexo1112
    WHERE  id_movimiento BETWEEN 1016 AND 1020

    UPDATE safre_tmp:tmp_det_anexo1112
    SET    num_solicitudes      = lr_tot_trans_issste.num_solicitudes      ,
           mto_mes_actual       = lr_tot_trans_issste.mto_mes_actual       ,
           mto_mes_anterior     = lr_tot_trans_issste.mto_mes_anterior     ,
           mto_acumulado_anual  = lr_tot_trans_issste.mto_acumulado_anual
    WHERE  id_movimiento        = 1015

END FUNCTION


#---------------------------------------------------------------------------#
# f_actualiza_total_ISSSTE : Actualiza el rubro 1001 (Total de entradas     #
#                            ISSSTE) con los datos generados de cada        #
#                            subcategoria                                   #
#---------------------------------------------------------------------------#
FUNCTION f_actualiza_total_ISSSTE()

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
    WHERE  ( (id_movimiento BETWEEN 1002 AND 1008) OR 
             (id_movimiento BETWEEN 1011 AND 1015) OR
             (id_movimiento = 1021) OR 
             (id_movimiento = 1052)
           )

    UPDATE safre_tmp:tmp_det_anexo1112
    SET    num_solicitudes      = lr_reinv.num_solicitudes      ,
           mto_mes_actual       = lr_reinv.mto_mes_actual       ,
           mto_mes_anterior     = lr_reinv.mto_mes_anterior     ,
           mto_acumulado_anual  = lr_reinv.mto_acumulado_anual
    WHERE  id_movimiento        = 1001

END FUNCTION