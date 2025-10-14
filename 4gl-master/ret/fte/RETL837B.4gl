#############################################################################
#Proyecto          => AFORES ( MEXICO )                                     #
#Propietario       => E.F.P.                                                #
#Programa RETL837B => ANEXO 115. RETIROS ISSSTE                             #
#Sistema           => RET                                                   #
#Autor             => David Miguel Garibay Rivera                           #
#Fecha             => 29 NOVIEMBRE  DE 2012                                 #
#Modificaciones    => Javier Gonzalez Jeronimo                              #
#Fecha             => 20 DE DICIEMBRE DE 2012                               #
#                  => Se realizaron los siguientes ajustes :                #
#                     - Se inicializan las variables despues de generar cada#
#                       detalle, ya que estaba generando secciones          #
#                       duplicadas.                                         #
#                     - Se ajusta el detalle de disposicion y transferencia #
#                       para que agrupe los saldos de vivienda en cada rubro#
#                       de siefore. En caso de solo tener montos de vivienda#
#                       estos se colocan en una sola linea                  #
#                     - Se eliminan registros que no tengan movimientos     #
#                       liquidados en dicho dia                             #
#Modificaciones    => Javier Gonzalez Jeronimo                              #
#Fecha             => 3 DE ENERO DE 2013                                    #
#                  => Se realizaron los siguientes ajustes :                #
#                     - Unificacion de versiones en todas las afores        #
#                     - Se incluyeron las funciones de las librerias de     #
#                       Retiros                                             #
#                     - Se incluye la generacion de cifras para la subcuenta#
#                       19 en los registros con regimen DT                  #
#############################################################################

DATABASE safre_af

GLOBALS

    DEFINE gr_seg_modulo RECORD LIKE seg_modulo.*

    DEFINE gr_afore RECORD
        peissste        SMALLINT
    END RECORD

    DEFINE
        HOY                             ,
        gdt_fecha_ini                   ,
        gdt_fecha_fin                   DATE

    DEFINE
        gc_usuario                      CHAR(030)   ,
        enter                           CHAR(001)   ,
        gc_ruta_encabezado              CHAR(200)   ,
        gc_ruta_subencabezado           CHAR(200)   ,
        gc_ruta_det_transferencia       CHAR(200)   ,
        gc_ruta_det_disposiciones       CHAR(200)   ,
        gc_ruta_det_parcial             CHAR(200)   ,
        gc_ruta_cons_detalle            CHAR(200)   ,
        gc_nom_archivo                  CHAR(400)   ,
        gc_nombre_anexo_115             CHAR(024)   ,
        gc_nom_rpt_control_704          CHAR(023)

    DEFINE
        gi_num_registros                ,
        gi_num_consolidado              INTEGER

    DEFINE
        gs_bnd_argumentos               ,
        gs_codigo_afore                 SMALLINT

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP              ,
        PROMPT LINE LAST        ,
        ACCEPT KEY CONTROL-I

    CALL f_lib_crea_log()
    CALL inicio()
    CALL f_abre_ventana()

    IF (gs_bnd_argumentos = 4) THEN
        CALL f_carga_informacion()
        CALL f_genera_anexo_115()
    ELSE
        CALL f_lib_error_msg("NUMERO INCORRECTO DE ARGUMENTOS")
    END IF

    CLOSE WINDOW retl8372           #CPL2320

END MAIN

#################################################################################
#Función                        : inicio()                                      #
#Objetivo                       : Función que inicializa las variables globales #
#Parámetros de entrada          : Ninguno                                       #
#Valores de retorno             : Ninguno                                       #
#Variables globales importantes : gr_seg_modulo ,                               #
#                                 gc_usuario     ,                              #
#                                 gs_codigo_afore ,                             #
#                                 gdt_fecha_ini ,                               #
#                                 gdt_fecha_fin                                 #
#Pantallas utilizadas           : Ninguna                                       #
#Autor                          : David Miguel Garibay Rivera                   #
#Fecha                          : 29/11/2012                                    #
#################################################################################
FUNCTION inicio()
#i---------------

    SELECT *    ,
           USER
    INTO   gr_seg_modulo.*  ,
           gc_usuario
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    -- Codigos de afore
    SELECT codigo_afore
    INTO   gs_codigo_afore
    FROM   tab_afore_local
    GROUP BY 1

    SELECT afore_cod
    INTO   gr_afore.peissste
    FROM   tab_afore
    WHERE  afore_desc MATCHES "*PENSIONISSSTE*"
    GROUP BY 1

    -- Inicialización de variables
    LET HOY                 = TODAY
    LET gi_num_registros    = 0
    LET gi_num_consolidado  = 0
    LET gs_bnd_argumentos   = NUM_ARGS()

    IF (gs_bnd_argumentos = 4) THEN
        LET gdt_fecha_ini           = ARG_VAL(1)
        LET gdt_fecha_fin           = ARG_VAL(2)
        LET gc_nombre_anexo_115     = ARG_VAL(3) --Nombre del anexo 115
        LET gc_nom_rpt_control_704  = ARG_VAL(4) --Nombre del reporte de cifras Control para el anexo 115
    END IF

END FUNCTION

#################################################################################
#Función                        : f_carga_informacion()                         #
#Objetivo                       : Función que realiza la busqueda de la         #
#                                 información en dis_cuenta y la almacena en una#
#                                 tabla temporal llamada "tmp_anexo_115".       #
#Parámetros de entrada          : Ninguno                                       #
#Valores de retorno             : Ninguno                                       #
#Variables globales importantes : Ninguna                                       #
#Pantallas utilizadas           : Ninguna                                       #
#Autor                          : David Miguel Garibay Rivera                   #
#Fecha                          : 29/11/2012                                    #
#################################################################################
FUNCTION f_carga_informacion()

    -- -----------------------------------------------------------------------------

    DISPLAY " OBTENIENDO DATOS DE LA CUENTA INDIVIDUAL - ANEXO 115 ...  " AT 19,1 ATTRIBUTE(REVERSE)
    SLEEP 1

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_anexo_115
    WHENEVER ERROR STOP
    

    SELECT *                    ,
           "08" origen_recursos ,
           "001" medio_solic    ,
           "XXX" forma_pago
    FROM   dis_cuenta
    WHERE  fecha_conversion BETWEEN gdt_fecha_ini AND gdt_fecha_fin
    AND    tipo_movimiento IN (851, 852, 853, 854, 855, 857, 858, 859, 864, 867, -- Retiros totales
                               861, 862, 863, 865, 866,                          -- Transferencias
                               856, 884, 885,                                    -- Parciales
                               10                                                -- ISR
                              )
    INTO TEMP tmp_anexo_115
    

    UPDATE STATISTICS FOR TABLE tmp_anexo_115

    IF gs_codigo_afore = gr_afore.peissste THEN
        CALL f_elimina_finde(gdt_fecha_ini, gdt_fecha_fin)
        LET gdt_fecha_ini = f_inserta_DT_finde(gdt_fecha_ini, gdt_fecha_fin)
    END IF

   -- Se actualizan los movimientos de tipo I (disposiciones) a B
   -- Se eliminara la validación que convierte a los retiros tipo I (864) en retiros tipo B (852) CPL-3427
   {UPDATE tmp_anexo_115
   SET    tipo_movimiento     = 852
   WHERE  tipo_movimiento     = 864}

   -- Se actualiza el tipo de prestacion a 07 (Banxico) para los registros con subcta 19
    UPDATE tmp_anexo_115
    SET    origen_recursos  = "07"
    WHERE  subcuenta        = 19

    IF gs_codigo_afore = gr_afore.peissste THEN
        CALL f_identifica_regimen()
    END IF
    -- Se determina la forma de pago
    CALL f_modifica_forma_pago()

    -- Se cambia la siefore 12 por la siefore basica del trabajador en los registros
    CALL fn_modifica_siefore_viv()

END FUNCTION


FUNCTION f_modifica_forma_pago()

    DEFINE lr_solicitud RECORD
        nss             LIKE ret_solicitud_tx.nss           ,
        consecutivo     LIKE ret_solicitud_tx.consecutivo
    END RECORD

    DEFINE lc_forma_pago      CHAR(003)
    DEFINE ls_tipo_pago       SMALLINT

    -- -----------------------------------------------------------------------------

    INITIALIZE lr_solicitud.* TO NULL

    LET lc_forma_pago  = "   "
    LET ls_tipo_pago   = 0

    DECLARE cur_medio_solic CURSOR FOR
        SELECT UNIQUE(nss)      ,
               consecutivo_lote
        FROM   tmp_anexo_115

    FOREACH cur_medio_solic INTO lr_solicitud.*

        SELECT tipo_pago
        INTO   ls_tipo_pago
        FROM   ret_beneficiario
        WHERE  nss          = lr_solicitud.nss
        AND    consecutivo  = lr_solicitud.consecutivo
         
        CASE ls_tipo_pago
        WHEN 5
           LET lc_forma_pago = '001'
           
        WHEN 1
           LET lc_forma_pago = '002'
           
        OTHERWISE
           LET lc_forma_pago = '003'
        END CASE

        UPDATE tmp_anexo_115
        SET    forma_pago           = lc_forma_pago
        WHERE  nss                  = lr_solicitud.nss
        AND    consecutivo_lote     = lr_solicitud.consecutivo

        INITIALIZE lr_solicitud.* TO NULL

        LET lc_forma_pago  = "   "
        LET ls_tipo_pago   = 0

    END FOREACH

END FUNCTION

#################################################################################
#Función                        : f_genera_anexo_115()                          #
#Objetivo                       : Función que                                   #
#                                                                               #
#                                                                               #
#Parámetros de entrada          : Ninguno                                       #
#Valores de retorno             : Ninguno                                       #
#Variables globales importantes : Ninguna                                       #
#Pantallas utilizadas           : Ninguna                                       #
#Autor                          : David Miguel Garibay Rivera                   #
#Fecha                          : 29/11/2012                                    #
#################################################################################
FUNCTION f_genera_anexo_115()

    CALL paso_uno(gdt_fecha_ini, gdt_fecha_fin) -- Genera subencabezados y detalles
    CALL paso_dos()                             -- Genera encabezado
    CALL paso_tres()         -- Concatena el Encabezado, Subencabezados y Detalles
    CALL paso_cuatro()       -- Asigna permisos al archivo final y elimina los archivos temporales

    DISPLAY "                                                           " AT 18,1
    DISPLAY " GENERANDO ARCHIVOS PLANOS DEL ANEXO 115 ...               " AT 19,1 ATTRIBUTE(REVERSE)
    SLEEP 1

END FUNCTION

#################################################################################
#Función                        : paso_uno()                                    #
#Objetivo                       : Función que genera los subencabezados         #
#                                                                               #
#                                                                               #
#Parámetros de entrada          : Ninguno                                       #
#Valores de retorno             : Ninguno                                       #
#Variables globales importantes : Ninguna                                       #
#Pantallas utilizadas           : Ninguna                                       #
#Autor                          : David Miguel Garibay Rivera                   #
#Fecha                          : 29/11/2012                                    #
#################################################################################
FUNCTION paso_uno(pdt_fecha_ini_proceso, pdt_fecha_fin_proceso)

    DEFINE
        pdt_fecha_ini_proceso       ,
        pdt_fecha_fin_proceso       DATE

    DEFINE
        ldt_fecha_ini_semana        ,
        ldt_fecha_fin_semana        DATE

    DEFINE
        li_diferencia               ,
        li_indice                   INTEGER

    DEFINE
        lc_nom_cifras_control       CHAR(200),
        lc_nom_rpt_detalle          CHAR(200),
        lc_comando                  CHAR(300)

    -- -----------------------------------------------------------------------------

    DISPLAY " GENERANDO DATOS DE LOS REPORTES DEL ANEXO 115 ...         " AT 18,1 ATTRIBUTE(REVERSE)
    SLEEP 1

    -- Asigna las rutas de los archivos temporales
    LET gc_ruta_det_transferencia    = gr_seg_modulo.ruta_envio CLIPPED, "/", gc_usuario CLIPPED, ".DET_TRANSFER_ANEXO115"
    LET gc_ruta_det_disposiciones    = gr_seg_modulo.ruta_envio CLIPPED, "/", gc_usuario CLIPPED, ".DET_DISP_ANEXO115"
    LET gc_ruta_det_parcial          = gr_seg_modulo.ruta_envio CLIPPED, "/", gc_usuario CLIPPED, ".DET_PARCIAL_ANEXO115"
    LET gc_ruta_cons_detalle         = gr_seg_modulo.ruta_envio CLIPPED, "/", gc_usuario CLIPPED, ".CONSOLIDA_DET"

    -- Inicia el reporte de cifras de control del anexo 115
    LET lc_nom_cifras_control       = gr_seg_modulo.ruta_listados CLIPPED,"/", gc_nom_rpt_control_704 CLIPPED
    LET lc_nom_cifras_control       = lc_nom_cifras_control CLIPPED

    START REPORT rpt_cifras_control_anexo115 TO lc_nom_cifras_control
    CALL f_imprime_cifras_nulo(0)   -- Se imprime un encabezado

    IF (pdt_fecha_ini_proceso IS NOT NULL AND
       pdt_fecha_fin_proceso IS NOT NULL) THEN

        LET li_diferencia           = pdt_fecha_fin_proceso - pdt_fecha_ini_proceso
        LET ldt_fecha_ini_semana    = pdt_fecha_ini_proceso
        LET ldt_fecha_fin_semana    = ldt_fecha_ini_semana + 6
        LET li_indice               = 0

      -- Genera la información de forma semanal
        WHILE (li_indice <= li_diferencia)
            IF (li_indice = 0) THEN
                LET ldt_fecha_ini_semana = pdt_fecha_ini_proceso
                LET ldt_fecha_fin_semana = ldt_fecha_ini_semana + 6

                IF (ldt_fecha_fin_semana >= pdt_fecha_fin_proceso) THEN
                    LET ldt_fecha_fin_semana = pdt_fecha_fin_proceso
                END IF
            ELSE
                LET ldt_fecha_ini_semana = ldt_fecha_ini_semana + 7
                LET ldt_fecha_fin_semana = ldt_fecha_ini_semana + 6

                IF (ldt_fecha_fin_semana >= pdt_fecha_fin_proceso) THEN
                    LET ldt_fecha_fin_semana = pdt_fecha_fin_proceso
                END IF
            END IF

            SELECT "OK"
            FROM   tmp_anexo_115
            WHERE  fecha_conversion BETWEEN ldt_fecha_ini_semana AND ldt_fecha_fin_semana
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
                CALL f_genera_subencabezado(ldt_fecha_ini_semana, ldt_fecha_fin_semana)
                CALL f_genera_transferencias(ldt_fecha_ini_semana, ldt_fecha_fin_semana)
                CALL f_genera_disposiciones(ldt_fecha_ini_semana, ldt_fecha_fin_semana)
                CALL f_genera_parciales(ldt_fecha_ini_semana, ldt_fecha_fin_semana)
            END IF 

            LET li_indice = li_indice + 7

            IF (ldt_fecha_fin_semana = pdt_fecha_fin_proceso ) THEN
                EXIT WHILE
            END IF
        END WHILE
    END IF

    FINISH REPORT rpt_cifras_control_anexo115

    -- Se cambian los permisos del archivo de cifras
    LET lc_comando = "chmod 777 ", lc_nom_cifras_control
    LET lc_comando = lc_comando CLIPPED
    RUN lc_comando

END FUNCTION

#################################################################################
#Función                        : paso_dos()                                    #
#Objetivo                       : Función que genera el encabezado del archivo  #
#                                                                               #
#                                                                               #
#Parámetros de entrada          : Ninguno                                       #
#Valores de retorno             : Ninguno                                       #
#Variables globales importantes : Ninguna                                       #
#Pantallas utilizadas           : Ninguna                                       #
#Autor                          : David Miguel Garibay Rivera                   #
#Fecha                          : 27/11/2012                                    #
#################################################################################
FUNCTION paso_dos()

    DEFINE
        lc_comando          CHAR(400)

    -- -----------------------------------------------------------------------------

    LET gc_ruta_encabezado = gr_seg_modulo.ruta_envio CLIPPED, "/", gc_usuario CLIPPED, ".CZA_ANEXO115"

    START REPORT rpt_anexo_115 TO gc_ruta_encabezado

        LET gi_num_registros = gi_num_registros + 1
        OUTPUT TO REPORT rpt_anexo_115()

    FINISH REPORT rpt_anexo_115

    LET lc_comando  = "chmod 777 ",  gc_ruta_encabezado CLIPPED
    LET lc_comando  = lc_comando CLIPPED
    RUN lc_comando

END FUNCTION

#################################################################################
#Función                        : paso_tres()                                   #
#Objetivo                       : Función que concatena los Encabezado,         #
#                                 subencabezados y detalles en un solo archivo  #
#Parámetros de entrada          : Ninguno                                       #
#Valores de retorno             : Ninguno                                       #
#Variables globales importantes : gc_ruta_encabezado, gc_ruta_cons_detalle      #
#Pantallas utilizadas           : Ninguna                                       #
#Autor                          : David Miguel Garibay Rivera                   #
#Fecha                          : 29/11/2012                                    #
#################################################################################
FUNCTION paso_tres()

    DEFINE
        lc_ruta_tmp             CHAR(400)   ,
        lc_comando_cat          CHAR(1000)  ,
        lc_comando_rm           CHAR(1000)  ,
        lc_comando_chmod        CHAR(1000)

    -- -----------------------------------------------------------------------------

    -- Inicialización de variables
    LET lc_ruta_tmp = gr_seg_modulo.ruta_envio CLIPPED, "/", gc_usuario CLIPPED, "rep_temporal.tmp"
    LET lc_ruta_tmp = lc_ruta_tmp CLIPPED

    IF (gi_num_consolidado = 0) THEN
        -- En caso de que no exista el archivo consolidado
        LET lc_comando_cat = "cat ", gc_ruta_encabezado CLIPPED, " > ", lc_ruta_tmp
    ELSE
        -- Se concatenan el encabezado y el archivo consolidado (subencabezados y detalles)
        LET lc_comando_cat = "cat ", gc_ruta_encabezado CLIPPED, " ",
                                     gc_ruta_cons_detalle CLIPPED," > ", lc_ruta_tmp
    END IF

    LET lc_comando_cat = lc_comando_cat CLIPPED
    RUN lc_comando_cat

    -- Cambia los permisos de archivo
    LET lc_comando_chmod = "chmod 777 ", lc_ruta_tmp CLIPPED
    LET lc_comando_chmod = lc_comando_chmod CLIPPED
    RUN lc_comando_chmod

    -- Genera el archivo final
    LET lc_comando_cat = "cp ", lc_ruta_tmp CLIPPED, " ", gr_seg_modulo.ruta_envio CLIPPED,"/", gc_nombre_anexo_115 CLIPPED
    RUN lc_comando_cat

    -- Elimina el archivo temporal
    LET lc_comando_rm = "rm ", lc_ruta_tmp CLIPPED
    LET lc_comando_rm = lc_comando_rm CLIPPED
    RUN lc_comando_rm

END FUNCTION

#################################################################################
#Función                        : paso_cuatro()                                 #
#Objetivo                       : Función que asigna permisos al archivo final  #
#                                 y elimina los archivos temporales             #
#Parámetros de entrada          : Ninguno                                       #
#Valores de retorno             : Ninguno                                       #
#Variables globales importantes : gc_nom_archivo, gc_ruta_encabezado,           #
#                                 gc_ruta_cons_detalle, gc_ruta_subencabezado,  #
#                                 gc_ruta_det_transferencia,                    #
#                                 gc_ruta_det_disposiciones,                    #
#                                 gc_ruta_det_parcial                           #
#Pantallas utilizadas           : Ninguna                                       #
#Autor                          : David Miguel Garibay Rivera                   #
#Fecha                          : 29/11/2012                                    #
#################################################################################
FUNCTION paso_cuatro()

    DEFINE
       lc_comando_chmod  CHAR(1000),
       lc_comando_rm     CHAR(4000)

    -- -----------------------------------------------------------------------------

    -- Se cambian los permisos del archivo final
    LET lc_comando_chmod = "chmod 777 ", gr_seg_modulo.ruta_envio CLIPPED,"/", gc_nombre_anexo_115 CLIPPED
    LET lc_comando_chmod = lc_comando_chmod CLIPPED
    RUN lc_comando_chmod

    WHENEVER ERROR CONTINUE
       -- Elimina los archivos temporales que conformaban el reporte
      LET lc_comando_rm = "rm ", gc_ruta_encabezado CLIPPED, " ",
                                 gc_ruta_subencabezado CLIPPED, " ",
                                 gc_ruta_det_transferencia CLIPPED, " ",
                                 gc_ruta_det_disposiciones CLIPPED , " ",
                                 gc_ruta_det_parcial CLIPPED , " ",
                                 gc_ruta_cons_detalle CLIPPED

        LET lc_comando_rm = lc_comando_rm CLIPPED
        RUN lc_comando_rm

    WHENEVER ERROR STOP

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_subencabezado : Genera la seccion del subencabezado en el        #
#                          archivo plano del anexo 115                      #
#---------------------------------------------------------------------------#
FUNCTION f_genera_subencabezado(pr_fechas)

    DEFINE pr_fechas RECORD
        inicio              DATE ,
        fin                 DATE
    END RECORD

    DEFINE
        lc_comando        CHAR(400)

    -- -----------------------------------------------------------------------------
    LET gc_ruta_subencabezado = gr_seg_modulo.ruta_envio CLIPPED, "/", gc_usuario CLIPPED, ".SUB_CZA_ANEXO115"

    -- Crea el subencabezado
    START REPORT rpt_subcza_anexo_115 TO gc_ruta_subencabezado

        OUTPUT TO REPORT rpt_subcza_anexo_115(pr_fechas.*)
        LET gi_num_registros = gi_num_registros + 1
        CALL f_imprime_cifras_nulo(1) -- Se imprime un subencabezado

    FINISH REPORT rpt_subcza_anexo_115

    -- Cambia permisos en el archivo
    LET lc_comando = "chmod 777 ", gc_ruta_subencabezado CLIPPED
    LET lc_comando = lc_comando CLIPPED
    RUN lc_comando

    -- Consolida subencabezado
    CALL f_consolida_subencabezado()

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_transferencias : Genera la seccion de transferencias ISSSTE en   #
#                           el archivo plano del anexo 115                  #
#---------------------------------------------------------------------------#
FUNCTION f_genera_transferencias(pr_fechas)

    DEFINE pr_fechas RECORD
        inicio              DATE ,
        fin                 DATE
    END RECORD

    DEFINE lr_montos RECORD
        tot_retiro_2008         ,
        tot_cesantia_vejez      ,
        tot_sar_issste_92       ,
        tot_ahorro_solidario    ,
        tot_complemen_retiro    ,
        tot_vivienda_92         ,
        tot_vivienda_2008       DECIMAL(22,2)
    END RECORD

    DEFINE
        ldt_fecha_aux               ,
        ldt_fecha_liquida           DATE

    DEFINE
        lc_origen_recursos          CHAR(002)   ,
        lc_comando                  CHAR(400)   ,
        lc_tipo_retiro              CHAR(003)

    DEFINE
        li_contador_dia             ,
        li_num_registros            ,
        li_movimiento               ,
        li_siefore                  ,
        li_num_tranfer              ,
        li_num_disposicion          ,
        li_num_parciales            INTEGER

    DEFINE
        ls_num_siefores             ,
        ls_existe_viv               SMALLINT

    -- -----------------------------------------------------------------------------

    DISPLAY " TRANSFERENCIAS ISSSTE ...                                 " AT 19,1 ATTRIBUTE(REVERSE)
    SLEEP 1

    -- Inicialización de variables
    LET ldt_fecha_aux = pr_fechas.inicio

    -- Hace la búsqueda por cada día del periodo semanal
    FOR li_contador_dia = 1 TO 7

        INITIALIZE ldt_fecha_liquida TO NULL

        SELECT fecha_conversion
        INTO   ldt_fecha_liquida
        FROM   tmp_anexo_115
        WHERE  fecha_conversion =  ldt_fecha_aux
        GROUP BY 1

        LET li_num_tranfer      = 0
        LET li_num_disposicion  = 0
        LET li_num_parciales    = 0
        LET ls_existe_viv       = 0

        IF (ldt_fecha_liquida IS NOT NULL) THEN

            START REPORT det_transfer_anexo_115 TO gc_ruta_det_transferencia

            INITIALIZE li_movimiento, li_siefore, lc_tipo_retiro, lc_origen_recursos TO NULL
            LET li_num_registros = 0
            LET li_num_tranfer   = 0

            DECLARE curs_mov_transferencias CURSOR FOR
                SELECT DISTINCT d.tipo_movimiento   ,
                                d.origen_recursos   ,
                                d.siefore
                FROM tmp_anexo_115 d    ,
                     tab_ret_issste r   ,
                     ret_trans_issste t
                WHERE t.nss             = d.nss
                AND t.tipo_retiro       = r.tipo_retiro
                AND d.tipo_movimiento  IN (861, 862, 863, 865, 866)
                AND r.tipo_retiro      IN ("H", "I", "J", "L", "N")
                AND d.fecha_conversion  = ldt_fecha_liquida
                ORDER BY 1,2,3

            FOREACH curs_mov_transferencias INTO li_movimiento      ,
                                                 lc_origen_recursos ,
                                                 li_siefore

                IF (li_movimiento IS NOT NULL ) THEN

                    LET ls_num_siefores = 0

                    -- Determina cuantas siefores tiene el tipo de movimiento
                    IF li_siefore <> 12 THEN
                        SELECT COUNT(DISTINCT d.siefore)
                        INTO   ls_num_siefores
                        FROM   tmp_anexo_115 d
                        WHERE  d.tipo_movimiento    = li_movimiento
                        AND    d.fecha_conversion   = ldt_fecha_liquida
                    ELSE
                        -- Detecta si existen nss a los que solo se les ha pagado vivienda
                        SELECT NVL(COUNT(*), 0)
                        INTO   ls_existe_viv
                        FROM   tmp_anexo_115
                        WHERE  fecha_conversion = ldt_fecha_liquida
                        AND    siefore = 12
                        AND nss NOT IN (SELECT UNIQUE nss
                                        FROM   tmp_anexo_115
                                        WHERE  fecha_conversion  = ldt_fecha_liquida
                                        AND siefore <> 12
                                       )
                        IF ls_existe_viv > 0 THEN
                            LET ls_num_siefores = 1
                        END IF
                    END IF

                    #-- Comentario por Javier Gonzalez
                    #-- El reporte se ejecutara solo en los siguientes casos:
                    #-- Tenga una sola siefore y esta sea la 11
                    #-- Tenga varias siefores

                    IF (ls_num_siefores = 1 AND li_siefore = 12) OR
                       (ls_num_siefores > 0 AND li_siefore <> 12) THEN

                        LET lc_tipo_retiro = f_regresa_tipo_ret_catalogo(li_movimiento)

                        -- Regresa Importes con formato cadena, sin puntos decimales
                        CALL f_regresa_importes_tranferencias(li_movimiento     ,
                                                              li_siefore        ,
                                                              ldt_fecha_liquida
                                                             )
                            RETURNING lr_montos.*, li_num_registros

                        -- Para determinar si se concatena el archivo
                        LET li_num_tranfer = li_num_tranfer + li_num_registros

                        IF (li_num_registros <>  0) THEN

                            LET gi_num_registros = gi_num_registros + 1

                            -- Envía la información para generar el detalle de Transferencias ISSSTE
                            OUTPUT TO REPORT det_transfer_anexo_115(ldt_fecha_liquida       ,
                                                                    li_siefore              ,
                                                                    lr_montos.*             ,
                                                                    li_num_registros        ,
                                                                    lc_tipo_retiro          ,
                                                                    lc_origen_recursos
                                                                   )

                            -- Envía la información al reporte de cifras de control
                            OUTPUT TO REPORT rpt_cifras_control_anexo115(301                    , --Tipo de detalle
                                                                         ldt_fecha_liquida      , --Fecha de liquidación
                                                                         lc_tipo_retiro         , --Tipo de retiro
                                                                         lc_origen_recursos     , --Origen de recursos
                                                                         li_siefore             , --Siefore
                                                                         lr_montos.*            ,
                                                                         0                      , --RCV
                                                                         li_num_registros       , --Número de registros
                                                                         0                      , --ID encabezado
                                                                         0                      , --ID subencabezado
                                                                         1                        --ID detalle
                                                                        )
                       END IF   --IF (li_num_registros <>  0)
                    END IF    --IF (ls_num_siefores = 1 AND li_siefore = 12)  OR
                              --   (ls_num_siefores > 0 AND li_siefore <> 12)
                END IF    --IF li_movimiento IS NOT NULL
            END FOREACH

            FINISH REPORT det_transfer_anexo_115

            -- Cambia los permisos del archivo generado
            LET lc_comando  = "chmod 777 ", gc_ruta_det_transferencia CLIPPED
            LET lc_comando  = lc_comando CLIPPED
            RUN lc_comando

        END IF -- ldt_fecha_liquida No nula

        -- Consolida los detalles
        IF (li_num_tranfer     <> 0 OR
            li_num_disposicion <> 0 OR
            li_num_parciales   <> 0 ) THEN

            CALL f_consolida_detalles(li_num_tranfer     ,
                                      li_num_disposicion ,
                                      li_num_parciales
                                     )
        END IF

        IF (ldt_fecha_aux = pr_fechas.fin) THEN
            EXIT FOR
        ELSE
            LET ldt_fecha_aux = ldt_fecha_aux + 1
        END IF

        LET li_num_tranfer     = 0
        LET li_num_disposicion = 0
        LET li_num_parciales   = 0

    END FOR

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_disposiciones : Genera la seccion de disposiciones ISSSTE en     #
#                          el archivo plano del anexo 115                   #
#---------------------------------------------------------------------------#
FUNCTION f_genera_disposiciones(pr_fechas)

    DEFINE pr_fechas RECORD
        inicio              DATE ,
        fin                 DATE
    END RECORD

    DEFINE lr_montos RECORD
        tot_retiro_2008         ,
        tot_cesantia_vejez      ,
        tot_sar_issste_92       ,
        tot_ahorro_solidario    ,
        tot_complemen_retiro    ,
        tot_vivienda_92         ,
        tot_vivienda_2008       DECIMAL(22,2)
    END RECORD

    DEFINE
        ldt_fecha_aux               ,
        ldt_fecha_liquida           DATE

    DEFINE
        lc_origen_recursos          CHAR(002)   ,
        lc_comando                  CHAR(400)   ,
        lc_tipo_retiro              CHAR(003)

    DEFINE
        li_contador_dia             ,
        li_num_registros            ,
        li_movimiento               ,
        li_siefore                  ,
        li_num_tranfer              ,
        li_num_disposicion          ,
        li_num_parciales            INTEGER

    DEFINE
        ls_num_siefores             ,
        ls_existe_viv               SMALLINT

    -- -----------------------------------------------------------------------------

    DISPLAY " DISPOSICIONES ISSSTE ...                                  " AT 19,1 ATTRIBUTE(REVERSE)
    SLEEP 1

    -- Inicialización de variables
    LET ldt_fecha_aux = pr_fechas.inicio

    -- Hace la búsqueda por cada día del periodo semanal
    FOR li_contador_dia = 1 TO 7

        INITIALIZE ldt_fecha_liquida TO NULL

        SELECT fecha_conversion
        INTO   ldt_fecha_liquida
        FROM   tmp_anexo_115
        WHERE  fecha_conversion =  ldt_fecha_aux
        GROUP BY 1

        LET li_num_tranfer      = 0
        LET li_num_disposicion  = 0
        LET li_num_parciales    = 0
        LET ls_existe_viv       = 0

        IF (ldt_fecha_liquida IS NOT NULL) THEN

            START REPORT det_disp_anexo_115 TO gc_ruta_det_disposiciones

            INITIALIZE li_movimiento, li_siefore, lc_tipo_retiro, lc_origen_recursos TO NULL
            LET li_num_registros    = 0
            LET li_num_disposicion  = 0

            DECLARE curs_mov_disposicion CURSOR FOR
                SELECT DISTINCT d.tipo_movimiento   , 
                                d.origen_recursos   ,
                                d.siefore  
                FROM   tmp_anexo_115 d      ,  
                       tab_ret_issste r
                WHERE  d.tipo_movimiento    = r.movimiento 
                AND    d.tipo_movimiento   IN (851, 852, 853, 854, 855, 857, 858, 859, 864, 867) 
                AND    r.tipo_retiro       IN ("A", "B", "C", "D", "E", "G", "K", "M", "I", "O")
                AND    d.fecha_conversion   = ldt_fecha_liquida
                ORDER BY 1,2,3

            FOREACH curs_mov_disposicion INTO li_movimiento         ,
                                              lc_origen_recursos    ,
                                              li_siefore

                IF (li_movimiento IS NOT NULL) THEN

                    LET ls_num_siefores = 0

                    -- Determina cuantas siefores tiene el tipo de movimiento
                    IF li_siefore <> 12 THEN
                        SELECT COUNT(DISTINCT d.siefore)
                        INTO   ls_num_siefores
                        FROM   tmp_anexo_115 d
                        WHERE  d.tipo_movimiento  = li_movimiento
                        AND    d.fecha_conversion = ldt_fecha_liquida
                        AND    d.origen_recursos = lc_origen_recursos
                    ELSE
                        SELECT NVL(COUNT(*), 0)
                        INTO   ls_existe_viv
                        FROM   tmp_anexo_115
                        WHERE  fecha_conversion = ldt_fecha_liquida
                        AND    siefore = 12
                        AND    origen_recursos = lc_origen_recursos
                        AND nss NOT IN (SELECT UNIQUE nss
                                        FROM   tmp_anexo_115
                                        WHERE  fecha_conversion  = ldt_fecha_liquida
                                        AND    siefore <> 12
                                       )
                        
                        IF ls_existe_viv > 0 THEN
                            LET ls_num_siefores = 1
                        END IF
                    END IF

                     #-- Comentario por Javier Gonzalez
                     #-- El reporte se ejecutara solo en los siguientes casos:
                     #-- Tenga una sola siefore y esta sea la 11
                     #-- Tenga varias siefores

                     IF (ls_num_siefores = 1 AND li_siefore = 12) OR
                        (ls_num_siefores > 0 AND li_siefore <> 12) THEN

                        LET lc_tipo_retiro = f_regresa_tipo_ret_catalogo(li_movimiento)

                        -- Regresa Importes con formato cadena, sin puntos decimales
                        CALL f_regresa_importes_disposicion(li_movimiento       , 
                                                            li_siefore          , 
                                                            ldt_fecha_liquida   ,
                                                            lc_origen_recursos  )
                            RETURNING lr_montos.*, li_num_registros

                        -- Para determinar si se concatena el archivo
                        LET li_num_disposicion = li_num_disposicion + li_num_registros

                        IF (li_num_registros <>  0) THEN

                            LET gi_num_registros = gi_num_registros + 1

                            --Envía la información para generar el detalle de Disposición
                            OUTPUT TO REPORT det_disp_anexo_115(ldt_fecha_liquida       ,
                                                                li_siefore              ,
                                                                lr_montos.*             ,
                                                                li_num_registros        ,
                                                                lc_tipo_retiro          ,
                                                                lc_origen_recursos
                                                               )

                            -- Envía la información al reporte de cifras de control
                            OUTPUT TO REPORT rpt_cifras_control_anexo115(302                , --Tipo de detalle
                                                                         ldt_fecha_liquida  , --Fecha de liquidación
                                                                         lc_tipo_retiro     , --Tipo de retiro
                                                                         lc_origen_recursos , --Origen de recursos
                                                                         li_siefore         , --Siefore
                                                                         lr_montos.*        ,
                                                                         0                  , --RCV
                                                                         li_num_registros   , --Número de registros
                                                                         0                  , --ID encabezado
                                                                         0                  , --ID subencabezado
                                                                         1                    --ID detalle
                                                                         )
                        END IF   --IF (li_num_registros <>  0)
                    END IF  --(ls_num_siefores = 1 AND li_siefore = 12) OR
                             --(ls_num_siefores > 0 AND li_siefore <> 12)
                END IF    --IF li_movimiento IS NOT NULL
            END FOREACH     --foreach de los tipos de movimiento y de las siefores

            FINISH REPORT det_disp_anexo_115

            -- Cambia los permisos del archivo generado
            LET lc_comando  = "chmod 777 ", gc_ruta_det_transferencia CLIPPED
            LET lc_comando  = lc_comando CLIPPED
            RUN lc_comando

        END IF -- ldt_fecha_liquida No nula

        -- Consolida los detalles
        IF (li_num_tranfer     <> 0 OR
            li_num_disposicion <> 0 OR
            li_num_parciales   <> 0 ) THEN

            CALL f_consolida_detalles(li_num_tranfer     ,
                                      li_num_disposicion ,
                                      li_num_parciales
                                     )
        END IF

        IF (ldt_fecha_aux = pr_fechas.fin) THEN
            EXIT FOR
        ELSE
            LET ldt_fecha_aux = ldt_fecha_aux + 1
        END IF

        LET li_num_tranfer     = 0
        LET li_num_disposicion = 0
        LET li_num_parciales   = 0

    END FOR

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_parciales : Genera la seccion de retiros parciales ISSSTE en el  #
#                      archivo plano del anexo 115                          #
#---------------------------------------------------------------------------#
FUNCTION f_genera_parciales(pr_fechas)

    DEFINE pr_fechas RECORD
        inicio              DATE ,
        fin                 DATE
    END RECORD

    DEFINE lr_montos RECORD
        tot_retiro_2008         ,
        tot_cesantia_vejez      ,
        tot_sar_issste_92       ,
        tot_ahorro_solidario    ,
        tot_complemen_retiro    ,
        tot_vivienda_92         ,
        tot_vivienda_2008       DECIMAL(22,2)
    END RECORD

    DEFINE
        ldt_fecha_aux               ,
        ldt_fecha_liquida           DATE

    DEFINE
        lc_origen_recursos          CHAR(002)   ,
        lc_comando                  CHAR(400)   ,
        lc_tipo_retiro              CHAR(003)

    DEFINE
        ld_importe_RCV              DECIMAL(22,2)

    DEFINE
        li_contador_dia             ,
        li_num_registros            ,
        li_movimiento               ,
        li_siefore                  ,
        li_num_tranfer              ,
        li_num_disposicion          ,
        li_num_parciales            INTEGER

    DEFINE
        ls_num_siefores             ,
        ls_existe_viv               SMALLINT

    DEFINE lc_medio_solic           CHAR(003)
    DEFINE lc_forma_pago            CHAR(003)

    -- -----------------------------------------------------------------------------

    DISPLAY " RETIROS PARCIALES ISSSTE ...                              " AT 19,1 ATTRIBUTE(REVERSE)
    SLEEP 1

    -- Inicialización de variables
    LET ldt_fecha_aux                   = pr_fechas.inicio
    LET lr_montos.tot_retiro_2008       = 0
    LET lr_montos.tot_cesantia_vejez    = 0
    LET lr_montos.tot_sar_issste_92     = 0
    LET lr_montos.tot_ahorro_solidario  = 0
    LET lr_montos.tot_complemen_retiro  = 0
    LET lr_montos.tot_vivienda_92       = 0
    LET lr_montos.tot_vivienda_2008     = 0


    -- Hace la búsqueda por cada día del periodo semanal
    FOR li_contador_dia = 1 TO 7

        INITIALIZE ldt_fecha_liquida TO NULL

        SELECT fecha_conversion
        INTO   ldt_fecha_liquida
        FROM   tmp_anexo_115
        WHERE  fecha_conversion =  ldt_fecha_aux
        GROUP BY 1

        LET li_num_tranfer      = 0
        LET li_num_disposicion  = 0
        LET li_num_parciales    = 0
        LET ls_existe_viv       = 0

         IF (ldt_fecha_liquida IS NOT NULL) THEN

            START REPORT det_parcial_anexo_115 TO gc_ruta_det_parcial

            INITIALIZE li_movimiento, li_siefore, lc_tipo_retiro, lc_origen_recursos TO NULL
            LET li_num_registros    = 0
            LET li_num_parciales    = 0

            DECLARE curs_mov_parcial CURSOR FOR
                SELECT DISTINCT d.tipo_movimiento   ,
                       d.origen_recursos            ,
                       medio_solic                  ,
                       forma_pago
                FROM   tmp_anexo_115 d      ,
                       tab_ret_issste r     ,
                       ret_parcial_issste p
                WHERE  p.nss                = d.nss
                AND    r.movimiento         = d.tipo_movimiento
                AND    d.tipo_movimiento    = 856
                AND    d.fecha_conversion   = ldt_fecha_liquida
                UNION
                SELECT DISTINCT d.tipo_movimiento   ,
                       d.origen_recursos            ,
                       medio_solic                  ,
                       forma_pago
                FROM   tmp_anexo_115 d      ,
                       tab_retiro_issste r     ,
                       ret_sol_issste_par p
                WHERE  p.nss_imss           = d.nss
                AND    r.movimiento         = d.tipo_movimiento
                AND    d.tipo_movimiento    IN (884, 885)
                AND    d.fecha_conversion   = ldt_fecha_liquida
                ORDER BY 1,2

            FOREACH curs_mov_parcial INTO li_movimiento     ,
                                          lc_origen_recursos,
                                          lc_medio_solic    ,
                                          lc_forma_pago

                IF (li_movimiento IS NOT NULL) THEN

                    LET lc_tipo_retiro  = f_regresa_tipo_ret_catalogo(li_movimiento)

                    -- Regresa el importe con formato cadena, sin puntos decimales
                    CALL f_regresa_importes_parcial(li_movimiento, ldt_fecha_liquida, lc_medio_solic, lc_forma_pago)
                        RETURNING ld_importe_RCV, li_num_registros

                    -- Para determinar si se concatena el archivo
                    LET li_num_parciales = li_num_parciales + li_num_registros

                    IF (li_num_registros <>  0) THEN

                        LET gi_num_registros = gi_num_registros + 1

                        -- Envía la información para generar el detalle de Retiros parciales ISSSTE
                        OUTPUT TO REPORT det_parcial_anexo_115(ldt_fecha_liquida    ,
                                                               ld_importe_RCV       ,
                                                               li_num_registros     ,
                                                               lc_tipo_retiro       ,
                                                               lc_origen_recursos   ,
                                                               lc_medio_solic       ,
                                                               lc_forma_pago
                                                              )
                        -- Envía la información al reporte de cifras de control
                        OUTPUT TO REPORT rpt_cifras_control_anexo115(303                , --Tipo de detalle
                                                                     ldt_fecha_liquida  , --Fecha de liquidación
                                                                     lc_tipo_retiro     , --Tipo de retiro
                                                                     lc_origen_recursos , --Origen de recursos
                                                                     li_siefore         , --Siefore
                                                                     lr_montos.*        ,
                                                                     ld_importe_RCV     , --RCV
                                                                     li_num_registros   , --Número de registros
                                                                     0                  , --ID encabezado
                                                                     0                  , --ID subencabezado
                                                                     1                    --ID detalle
                                                                    )
                    END IF   --IF (li_num_registros <>  0)
                END IF    --IF li_movimiento IS NOT NULL
            END FOREACH     --foreach del fecha_liquida

            FINISH REPORT det_parcial_anexo_115

            -- Cambia los permisos del archivo generado
            LET lc_comando  = "chmod 777 ", gc_ruta_det_transferencia CLIPPED
            LET lc_comando  = lc_comando CLIPPED
            RUN lc_comando

        END IF -- ldt_fecha_liquida No nula

        -- Consolida los detalles
        IF (li_num_tranfer     <> 0 OR
            li_num_disposicion <> 0 OR
            li_num_parciales   <> 0 ) THEN

            CALL f_consolida_detalles(li_num_tranfer     ,
                                      li_num_disposicion ,
                                      li_num_parciales
                                     )
        END IF

        IF (ldt_fecha_aux = pr_fechas.fin) THEN
            EXIT FOR
        ELSE
            LET ldt_fecha_aux = ldt_fecha_aux + 1
        END IF

        LET li_num_tranfer     = 0
        LET li_num_disposicion = 0
        LET li_num_parciales   = 0

    END FOR

END FUNCTION

#################################################################################
#Función                        : f_regresa_importes_tranferencias()            #
#Objetivo                       : Función que consulta los importes de          #
#                                 transferencias:                               #
#                                   Retiro 97                                   #
#                                   Cesantia y Vejez                            #
#                                   Cuota Social                                #
#                                   Vivienda 97                                 #
#Parámetros de entrada          : pi_movimiento     ,                           #
#                                 pi_siefore        ,                           #
#                                 pdt_fecha_liquida ,                           #
#                                 pc_tipo_retiro    ,                           #
#                                 pi_tipo_prestacion                            #
#Valores de retorno             : lc_cad_retiro_2008      ,                     #
#                                 lc_cad_cesantia_vejez ,                       #
#                                 lc_cad_sar_issste_92   ,                      #
#                                 lc_cad_ahorro_solidario    ,                  #
#                                 li_num_total_registros                        #
#Variables globales importantes : Ninguna                                       #
#Pantallas utilizadas           : Ninguna                                       #
#Autor                          : David Miguel Garibay Rivera                   #
#Fecha                          : 29/11/2012                                    #
#################################################################################
FUNCTION f_regresa_importes_tranferencias(pi_movimiento, pi_siefore, pdt_fecha_liquida)

    DEFINE
        pi_movimiento               ,
        pi_siefore                  INTEGER

    DEFINE
        pdt_fecha_liquida           DATE

    DEFINE li_subcuenta LIKE dis_cuenta.subcuenta
    DEFINE lc_tipo_retiro_issste LIKE tab_ret_issste.tipo_retiro

    DEFINE
        ld_importe                      ,
        ld_tot_retiro_2008              ,
        ld_tot_cesantia_vejez           ,
        ld_tot_sar_issste_92            ,
        ld_tot_ahorro_solidario         ,
        ld_tot_complemen_retiro         ,
        ld_tot_vivienda_92              ,
        ld_tot_vivienda_2008            DECIMAL(22,2)

    DEFINE
        li_num_total_registros          ,
        li_num_registros                INTEGER

    DEFINE
        lc_qry_importe                  CHAR(1500)  ,
        lc_query_sie12                  CHAR(1500)  ,
        lc_query_viv                    CHAR(2000)

    -- -----------------------------------------------------------------------------

    -- Inicialización de Variables
    LET li_num_total_registros  = 0
    LET li_num_registros        = 0
    LET ld_importe              = 0
    LET ld_tot_retiro_2008      = 0
    LET ld_tot_cesantia_vejez   = 0
    LET ld_tot_sar_issste_92    = 0
    LET ld_tot_ahorro_solidario = 0
    LET ld_tot_complemen_retiro = 0
    LET ld_tot_vivienda_92      = 0
    LET ld_tot_vivienda_2008    = 0

    LET lc_tipo_retiro_issste   = f_obten_tipo_retiro(pi_movimiento)

    LET lc_qry_importe = " SELECT d.subcuenta, NVL(SUM(d.monto_en_pesos) * -1, 0) \n",
                         "   FROM tmp_anexo_115 d, tab_ret_issste r, ret_trans_issste t\n",
                         "  WHERE d.tipo_movimiento  = ? \n",
                         "    AND d.siefore          = ? \n",
                         "    AND d.fecha_conversion = ? \n",
                         "    AND r.movimiento  = d.tipo_movimiento \n",
                         "    AND t.nss         = d.nss \n",
                         "    AND t.consecutivo = d.consecutivo_lote \n"

    IF pi_siefore = 12 THEN
        LET lc_query_sie12 = " AND d.nss NOT IN (SELECT UNIQUE nss \n",
                             "                   FROM   tmp_anexo_115 \n",
                             "                   WHERE  fecha_conversion = '", pdt_fecha_liquida, "' \n",
                             "                   AND    siefore <> 12) \n "
    ELSE
        LET lc_query_sie12 = " "
    END IF

    LET lc_qry_importe = lc_qry_importe CLIPPED,
                         lc_query_sie12 CLIPPED,
                         "    GROUP BY 1 \n"

    PREPARE pre_importe_transferencias  FROM lc_qry_importe
    DECLARE curs_importe_transferencias CURSOR FOR pre_importe_transferencias
    FOREACH curs_importe_transferencias USING pi_movimiento     ,
                                              pi_siefore        ,
                                              pdt_fecha_liquida
                                        INTO  li_subcuenta      ,
                                              ld_importe

        IF (li_subcuenta IS NOT NULL) THEN
            CASE (li_subcuenta)
                -- Importes de la subcuenta 'Retiro 2008'
                WHEN 30
                    LET ld_tot_retiro_2008 = ld_tot_retiro_2008 + ld_importe
                
                -- Importes de la subcuenta 'Cesantía y Vejez'
                WHEN 31
                    LET ld_tot_cesantia_vejez = ld_tot_cesantia_vejez + ld_importe
                WHEN 32
                    LET ld_tot_cesantia_vejez = ld_tot_cesantia_vejez + ld_importe
                
                -- Importes de la subcuenta 'SAR ISSSTE 92'
                WHEN 13
                    LET ld_tot_sar_issste_92 = ld_tot_sar_issste_92 + ld_importe
    
                -- Importes de la subcuenta 'SAR ISSSTE 92' Banxico                
                WHEN 19
                    LET ld_tot_sar_issste_92 = ld_tot_sar_issste_92 + ld_importe

                -- Importes de la subcuenta 'Ahorro solidario'
                WHEN 33
                    LET ld_tot_ahorro_solidario = ld_tot_ahorro_solidario + ld_importe
                WHEN 34
                    LET ld_tot_ahorro_solidario = ld_tot_ahorro_solidario + ld_importe
                
                -- Importes de la subcuenta 'Complementarias de retiro'
                WHEN 11
                    LET ld_tot_complemen_retiro = ld_tot_complemen_retiro + ld_importe
                WHEN 12
                    LET ld_tot_complemen_retiro = ld_tot_complemen_retiro + ld_importe
                WHEN 24
                    LET ld_tot_complemen_retiro = ld_tot_complemen_retiro + ld_importe
                WHEN 25
                    LET ld_tot_complemen_retiro = ld_tot_complemen_retiro + ld_importe

                -- Importes de la subcuenta Vivienda 92
                WHEN 14
                    LET ld_tot_vivienda_92      = ld_tot_vivienda_92 + ld_importe

                -- Importes de la subcuenta Vivienda 2008
                WHEN 35
                    LET ld_tot_vivienda_2008    = ld_tot_vivienda_2008 + ld_importe

            END CASE

            LET li_num_total_registros = li_num_total_registros + li_num_registros
        END IF
   END FOREACH

    -- Si la siefore no es de vivienda obtenemos los montos de vivienda para los nss
    -- liquidados en el grupo
    IF pi_siefore <> 12 THEN
        LET lc_query_viv = " SELECT NVL( SUM(d.monto_en_pesos) * -1, 0)  \n",
                           " FROM   tmp_anexo_115 d, ret_trans_issste t \n",
                           "  WHERE d.tipo_movimiento = ? \n",
                           "   AND d.nss              = t.nss \n ",
                           "   AND d.consecutivo_lote = t.consecutivo \n",
                           "   AND d.siefore          = 12 \n",
                           "   AND d.subcuenta        = ? \n",
                           "   AND d.fecha_conversion = ? \n",
                           "   AND t.tipo_retiro      = ? \n",
                           "   AND t.nss IN (SELECT UNIQUE(d.nss) \n",
                           "                 FROM tmp_anexo_115 d, tab_ret_issste r, ret_trans_issste t \n",
                           "                 WHERE d.tipo_movimiento = ? \n",
                           "                   AND d.siefore            = ? \n",
                           "                   AND d.fecha_conversion   = ? \n",
                           "                   AND r.movimiento  = d.tipo_movimiento \n",
                           "                   AND t.nss         = d.nss \n",
                           "                   AND t.consecutivo = d.consecutivo_lote ) \n"

        LET li_subcuenta = 35

        PREPARE prp_viv_tr FROM lc_query_viv
        EXECUTE prp_viv_tr USING pi_movimiento        ,
                              li_subcuenta         ,
                              pdt_fecha_liquida    ,
                              lc_tipo_retiro_issste,
                              pi_movimiento        ,
                              pi_siefore           ,
                              pdt_fecha_liquida
                        INTO ld_tot_vivienda_2008

        LET li_subcuenta = 14

        EXECUTE prp_viv_tr USING pi_movimiento        ,
                              li_subcuenta         ,
                              pdt_fecha_liquida    ,
                              lc_tipo_retiro_issste,
                              pi_movimiento        ,
                              pi_siefore           ,
                              pdt_fecha_liquida
                        INTO ld_tot_vivienda_92

    END IF

    -- Se elimina el punto decimal
    LET ld_tot_retiro_2008       = ld_tot_retiro_2008       * 100
    LET ld_tot_cesantia_vejez    = ld_tot_cesantia_vejez    * 100
    LET ld_tot_sar_issste_92     = ld_tot_sar_issste_92     * 100
    LET ld_tot_ahorro_solidario  = ld_tot_ahorro_solidario  * 100
    LET ld_tot_complemen_retiro  = ld_tot_complemen_retiro  * 100
    LET ld_tot_vivienda_92       = ld_tot_vivienda_92       * 100
    LET ld_tot_vivienda_2008     = ld_tot_vivienda_2008     * 100

    -- Realiza la búsqueda de los NSS con Transferencias

    LET li_num_total_registros = 0
    LET lc_qry_importe = " SELECT COUNT(DISTINCT d.nss) \n",
                         "   FROM tmp_anexo_115 d, tab_ret_issste r, ret_trans_issste t\n",
                         "  WHERE d.tipo_movimiento = ? \n",
                         "    AND d.siefore         = ? \n",
                         "    AND d.fecha_conversion      = ? \n",
                         "    AND r.movimiento  = d.tipo_movimiento \n",
                         "    AND t.nss         = d.nss\n",
                         "    AND t.consecutivo = d.consecutivo_lote \n"

    LET lc_qry_importe = lc_qry_importe CLIPPED,
                         lc_query_sie12 CLIPPED

    PREPARE pre_num_transferencias FROM lc_qry_importe
    EXECUTE pre_num_transferencias USING pi_movimiento  ,
                                         pi_siefore     ,
                                         pdt_fecha_liquida
                                   INTO  li_num_total_registros

    RETURN ld_tot_retiro_2008       ,
           ld_tot_cesantia_vejez    ,
           ld_tot_sar_issste_92     ,
           ld_tot_ahorro_solidario  ,
           ld_tot_complemen_retiro  ,
           ld_tot_vivienda_92       ,
           ld_tot_vivienda_2008     ,
           li_num_total_registros

END FUNCTION



#################################################################################
#Función                        : f_regresa_importes_disposicion()              #
#Objetivo                       : Función que consulta los importes de los      #
#                                 retiros totales:                              #
#                                   Retiro 97                                   #
#                                   Cesantia y Vejez                            #
#                                   Cuota Social                                #
#                                   Retiro 92                                   #
#                                   Vivienda 92                                 #
#                                   Vivienda 97                                 #
#Parámetros de entrada          : pi_movimiento     ,                           #
#                                 pi_siefore        ,                           #
#                                 pdt_fecha_liquida ,                           #
#                                 pc_tipo_retiro    ,                           #
#                                 pi_tipo_prestacion                            #
#Valores de retorno             : lc_cad_retiro_2008      ,                     #
#                                 lc_cad_cesantia_vejez ,                       #
#                                 lc_cad_sar_issste_92   ,                      #
#                                 lc_cad_retiro_92                              #
#                                 lc_cad_vivienda_92                            #
#                                 lc_cad_ahorro_solidario    ,                  #
#                                 li_num_total_registros                        #
#Variables globales importantes : Ninguna                                       #
#Pantallas utilizadas           : Ninguna                                       #
#Autor                          : David Miguel Garibay Rivera                   #
#Fecha                          : 29/11/2012                                    #
#################################################################################
FUNCTION f_regresa_importes_disposicion(pi_movimiento, pi_siefore, pdt_fecha_liquida, pc_origen)

    DEFINE
        pi_movimiento               ,
        pi_siefore                  INTEGER

    DEFINE
        pdt_fecha_liquida           DATE

    DEFINE
        pc_origen                   CHAR(02)

    DEFINE lc_tipo_retiro_issste LIKE tab_ret_issste.tipo_retiro
    DEFINE li_subcuenta LIKE dis_cuenta.subcuenta

    DEFINE
        ld_importe                      ,
        ld_importe_isr                  ,
        ld_tot_retiro_2008              ,
        ld_tot_cesantia_vejez           ,
        ld_tot_sar_issste_92            ,
        ld_tot_ahorro_solidario         ,
        ld_tot_complemen_retiro         ,
        ld_tot_vivienda_92              ,
        ld_tot_vivienda_2008            DECIMAL(22,2)

    DEFINE
        li_num_total_registros          ,
        li_registros_DT                 INTEGER

    DEFINE
        lc_qry_importe                  CHAR(1500)  ,
        lc_qry_ISR                      CHAR(0500)  ,
        lc_qry_movimiento               CHAR(0100)  ,
        lc_query_viv                    CHAR(2000)  ,
        lc_query_sie12                  CHAR(1500)  ,
        lc_retiro_B                     CHAR(0100)

    -- -----------------------------------------------------------------------------

    -- Inicialización de Variables
    LET li_num_total_registros  = 0
    LET li_registros_DT         = 0
    LET ld_importe              = 0
    LET ld_tot_retiro_2008      = 0
    LET ld_tot_cesantia_vejez   = 0
    LET ld_tot_sar_issste_92    = 0
    LET ld_tot_ahorro_solidario = 0
    LET ld_tot_complemen_retiro = 0
    LET ld_tot_vivienda_92      = 0
    LET ld_tot_vivienda_2008    = 0

    LET lc_tipo_retiro_issste = f_obten_tipo_retiro(pi_movimiento)

    -- En caso de ser movimiento 852 puede ser un retiro B o I
    IF pi_movimiento = 852 THEN
        LET lc_retiro_B = "    AND t.tipo_retiro      IN (?,'I') \n"
    ELSE
        LET lc_retiro_B = "    AND t.tipo_retiro      = ?  \n"
    END IF 

    -- Para calcular el ISR
    LET lc_qry_ISR = " SELECT NVL(SUM(d.monto_en_pesos) * -1, 0) \n",
                     "   FROM tmp_anexo_115 d, ret_sol_issste_tx t \n",
                     "  WHERE d.folio            = t.folio \n",
                     "    AND d.nss              = t.nss \n",
                     "    AND d.consecutivo_lote = t.consecutivo \n",
                     "    AND d.tipo_movimiento  = 10 \n",     --ISR
                     "    AND d.siefore          = ?  \n",
                     "    AND d.subcuenta        = ?  \n",
                     "    AND d.origen_recursos  = ?  \n",
                     "    AND d.fecha_conversion = ?  \n",
                     lc_retiro_B CLIPPED

    PREPARE pre_isr_disposicion FROM  lc_qry_ISR

    -- Obtiene importes  de cada subcuenta
    IF pi_movimiento <> 857 AND pi_movimiento <> 859 THEN
        LET lc_qry_importe = " SELECT d.subcuenta, NVL(SUM(d.monto_en_pesos) * -1, 0) \n",
                             "   FROM tmp_anexo_115 d, tab_ret_issste r, ret_sol_issste_tx t\n",
                             "  WHERE d.tipo_movimiento  = ? \n",
                             "    AND d.siefore          = ? \n",
                             "    AND d.fecha_conversion = ? \n",
                             "    AND d.origen_recursos  = ? \n",
                             "    AND r.movimiento  = d.tipo_movimiento \n",
                             "    AND t.nss         = d.nss \n",
                             "    AND t.consecutivo = d.consecutivo_lote \n"
    ELSE
        LET lc_qry_importe = "SELECT d.subcuenta, NVL(SUM(d.monto_en_pesos) * -1, 0) \n",        
                             "  FROM tmp_anexo_115 d, tab_ret_issste r \n",
                             " WHERE d.tipo_movimiento  = ? \n",         
                             "   AND d.siefore          = ? \n",                           
                             "   AND d.fecha_conversion = ? \n",
                             "   AND d.origen_recursos  = ? \n",
                             "   AND r.movimiento  = d.tipo_movimiento \n",
                             "   AND r.cod_tramite = 9 \n"
    END IF 

--    IF pi_siefore = 12 THEN
--        LET lc_query_sie12 = " AND d.nss NOT IN \n",
--                             " (SELECT UNIQUE nss \n",
--                             " FROM   tmp_anexo_115 \n",
--                             " WHERE  fecha_conversion = '", pdt_fecha_liquida, "' \n",
--                             " AND    siefore <> 12) \n"
--    ELSE
        LET lc_query_sie12 = " "
--    END IF

    LET lc_qry_importe = lc_qry_importe CLIPPED,
                         lc_query_sie12 CLIPPED,
                         "    GROUP BY 1 \n"

    PREPARE pre_importe_disposicion FROM lc_qry_importe
    DECLARE curs_importe_disposicion CURSOR FOR pre_importe_disposicion
    FOREACH curs_importe_disposicion USING pi_movimiento     ,
                                           pi_siefore        ,
                                           pdt_fecha_liquida ,
                                           pc_origen
                                     INTO  li_subcuenta      ,
                                           ld_importe

        LET ld_importe_isr  = 0
        
        IF (li_subcuenta IS NOT NULL) THEN
            EXECUTE pre_isr_disposicion USING pi_siefore            ,
                                              li_subcuenta          ,
                                              pc_origen             ,
                                              pdt_fecha_liquida     ,
                                              lc_tipo_retiro_issste
                                        INTO  ld_importe_isr

            CASE (li_subcuenta)
                -- Importes de la subcuenta Retiro 2008
                WHEN 30
                    LET ld_tot_retiro_2008 = ld_tot_retiro_2008 + ld_importe
                    LET ld_tot_retiro_2008 = ld_tot_retiro_2008 + ld_importe_isr
                
                -- Importes de la subcuenta Cesantía y Vejez
                WHEN 31
                    LET ld_tot_cesantia_vejez = ld_tot_cesantia_vejez + ld_importe
                    LET ld_tot_cesantia_vejez = ld_tot_cesantia_vejez + ld_importe_isr
                WHEN 32
                    LET ld_tot_cesantia_vejez = ld_tot_cesantia_vejez + ld_importe
                    LET ld_tot_cesantia_vejez = ld_tot_cesantia_vejez + ld_importe_isr

                -- Importes de la subcuenta SAR ISSSTE 92
                WHEN 13
                    LET ld_tot_sar_issste_92 = ld_tot_sar_issste_92 + ld_importe
                    LET ld_tot_sar_issste_92 = ld_tot_sar_issste_92 + ld_importe_isr

                -- Importes de la subcuenta SAR ISSSTE 92 Banxico
                WHEN 19
                    LET ld_tot_sar_issste_92 = ld_tot_sar_issste_92 + ld_importe
                    LET ld_tot_sar_issste_92 = ld_tot_sar_issste_92 + ld_importe_isr

                -- Importes de la subcuenta Ahorro solidario
                WHEN 33
                    LET ld_tot_ahorro_solidario = ld_tot_ahorro_solidario + ld_importe
                    LET ld_tot_ahorro_solidario = ld_tot_ahorro_solidario + ld_importe_isr
                WHEN 34
                    LET ld_tot_ahorro_solidario = ld_tot_ahorro_solidario + ld_importe
                    LET ld_tot_ahorro_solidario = ld_tot_ahorro_solidario + ld_importe_isr

                -- Importes de la subcuenta Complementarias de retiro
                WHEN 11
                    LET ld_tot_complemen_retiro = ld_tot_complemen_retiro + ld_importe
                    LET ld_tot_complemen_retiro = ld_tot_complemen_retiro + ld_importe_isr
                WHEN 12
                    LET ld_tot_complemen_retiro = ld_tot_complemen_retiro + ld_importe
                    LET ld_tot_complemen_retiro = ld_tot_complemen_retiro + ld_importe_isr
                WHEN 24
                    LET ld_tot_complemen_retiro = ld_tot_complemen_retiro + ld_importe
                    LET ld_tot_complemen_retiro = ld_tot_complemen_retiro + ld_importe_isr
                WHEN 25
                    LET ld_tot_complemen_retiro = ld_tot_complemen_retiro + ld_importe
                    LET ld_tot_complemen_retiro = ld_tot_complemen_retiro + ld_importe_isr

                -- Importes de la subcuenta Vivienda 92
                WHEN 14
                    LET ld_tot_vivienda_92      = ld_tot_vivienda_92 + ld_importe
                    LET ld_tot_vivienda_92      = ld_tot_vivienda_92 + ld_importe_isr
                
                -- Importes de la subcuenta Vivienda 2008
                WHEN 35
                    LET ld_tot_vivienda_2008    = ld_tot_vivienda_2008 + ld_importe
                    LET ld_tot_vivienda_2008    = ld_tot_vivienda_2008 + ld_importe_isr
            END CASE
        END IF
    END FOREACH

    -- Si la siefore no es de vivienda obtenemos los montos de vivienda para los nss
    -- liquidados en el grupo
{    IF pi_siefore <> 12 THEN
        LET lc_query_viv = " SELECT NVL(SUM(d.monto_en_pesos) * -1, 0)  \n",
                           " FROM   tmp_anexo_115 d, ret_sol_issste_tx t \n",
                           "  WHERE d.tipo_movimiento = ? \n",
                           "   AND d.nss              = t.nss \n ",
                           "   AND d.consecutivo_lote = t.consecutivo \n",
                           "   AND d.siefore          = 12 \n",
                           "   AND d.subcuenta        = ? \n",
                           "   AND d.fecha_conversion = ? \n",
                           "   AND d.origen_recursos  = ? \n",
                           lc_retiro_B ,
                           "   AND t.nss IN (SELECT UNIQUE(d.nss) \n",
                           "                 FROM tmp_anexo_115 d, tab_ret_issste r, ret_sol_issste_tx t \n",
                           "                 WHERE d.tipo_movimiento = ? \n",
                           "                   AND d.siefore            = ? \n",
                           "                   AND d.fecha_conversion   = ? \n",
                           "                   AND d.origen_recursos    = ? \n",
                           "                   AND r.movimiento  = d.tipo_movimiento \n",
                           "                   AND t.nss         = d.nss \n",
                           "                   AND t.consecutivo = d.consecutivo_lote ) \n"

        LET li_subcuenta = 35

        PREPARE prp_viv FROM lc_query_viv
        EXECUTE prp_viv USING pi_movimiento        ,
                              li_subcuenta         ,
                              pdt_fecha_liquida    ,
                              pc_origen            ,
                              lc_tipo_retiro_issste,
                              pi_movimiento        ,
                              pi_siefore           ,
                              pdt_fecha_liquida    ,
                              pc_origen
                        INTO  ld_tot_vivienda_2008

        LET li_subcuenta = 14

        EXECUTE prp_viv USING pi_movimiento        ,
                              li_subcuenta         ,
                              pdt_fecha_liquida    ,
                              pc_origen            ,
                              lc_tipo_retiro_issste,
                              pi_movimiento        ,
                              pi_siefore           ,
                              pdt_fecha_liquida    ,
                              pc_origen
                        INTO  ld_tot_vivienda_92
    END IF
}
    -- Se elimina el punto decimal
    LET ld_tot_retiro_2008      = ld_tot_retiro_2008      * 100
    LET ld_tot_cesantia_vejez   = ld_tot_cesantia_vejez   * 100
    LET ld_tot_sar_issste_92    = ld_tot_sar_issste_92    * 100
    LET ld_tot_ahorro_solidario = ld_tot_ahorro_solidario * 100
    LET ld_tot_complemen_retiro = ld_tot_complemen_retiro * 100
    LET ld_tot_vivienda_92      = ld_tot_vivienda_92      * 100
    LET ld_tot_vivienda_2008    = ld_tot_vivienda_2008    * 100

    -- Realiza la búsqueda de los NSS con Disposiciones
    LET li_num_total_registros = 0

    -- Cuenta el numero de registros que hay en la categoria
    LET lc_qry_importe = " SELECT NVL(COUNT(DISTINCT d.nss),0) \n",
                         "   FROM tmp_anexo_115 d, tab_ret_issste r \n",
                         "  WHERE d.tipo_movimiento  = ? \n",
                         "    AND d.siefore          = ? \n",
                         "    AND d.fecha_conversion = ? \n",
                         "    AND d.origen_recursos  = ? \n",
                         "    AND r.movimiento  = d.tipo_movimiento \n"

    LET lc_qry_importe = lc_qry_importe CLIPPED,
                         lc_query_sie12 CLIPPED

    PREPARE pre_num_disposicion FROM lc_qry_importe
    EXECUTE pre_num_disposicion USING pi_movimiento     ,
                                      pi_siefore        ,
                                      pdt_fecha_liquida ,
                                      pc_origen
                                INTO  li_num_total_registros
  
    RETURN ld_tot_retiro_2008       ,
           ld_tot_cesantia_vejez    ,
           ld_tot_sar_issste_92     ,
           ld_tot_ahorro_solidario  ,
           ld_tot_complemen_retiro  ,
           ld_tot_vivienda_92       ,
           ld_tot_vivienda_2008     ,
           li_num_total_registros

END FUNCTION

#################################################################################
#Función                        : f_regresa_importes_parcial()                  #
#Objetivo                       : Función que consulta los importes de          #
#                                 retiros parciales:                            #
#                                   RCV                                         #
#Parámetros de entrada          : pi_movimiento     ,                           #
#                                 pdt_fecha_liquida ,                           #
#                                 pc_tipo_retiro    ,                           #
#                                 pi_tipo_prestacion                            #
#Valores de retorno             : lc_cad_rcv      ,                             #
#                                 li_num_total_registros                        #
#Variables globales importantes : Ninguna                                       #
#Pantallas utilizadas           : Ninguna                                       #
#Autor                          : David Miguel Garibay Rivera                   #
#Fecha                          : 29/11/2012                                    #
#################################################################################
FUNCTION f_regresa_importes_parcial(pi_movimiento, pdt_fecha_liquida, pc_medio_solic, pc_forma_pago)

    DEFINE
        pi_movimiento               INTEGER

    DEFINE
        pdt_fecha_liquida           DATE

    DEFINE
        li_num_registros            INTEGER

    DEFINE
        lc_qry_isr                  ,
        lc_qry_importe              CHAR(1500)

    DEFINE
        ld_isr                      ,
        ld_importe                  DECIMAL(22,2)

    DEFINE pc_medio_solic     CHAR(003)
    DEFINE pc_forma_pago      CHAR(003)

    -- -----------------------------------------------------------------------------

    -- Inicialización de Variables
    LET li_num_registros    = 0
    LET ld_importe          = 0
    LET ld_isr              = 0

    -- Monto total de Parciales
    LET lc_qry_importe = " SELECT  NVL(SUM(d.monto_en_pesos) * -1, 0) \n",
                         "   FROM tmp_anexo_115 d \n",
                         "  WHERE d.tipo_movimiento = ? \n",
                         "    AND d.fecha_conversion = ? \n",
                         "    AND d.medio_solic      = ? \n",
                         "    AND d.forma_pago       = ? \n"
    LET lc_qry_importe = lc_qry_importe CLIPPED

    PREPARE pre_imp_parcial FROM lc_qry_importe
    EXECUTE pre_imp_parcial USING pi_movimiento     ,
                                  pdt_fecha_liquida ,
                                  pc_medio_solic    ,
                                  pc_forma_pago
                            INTO  ld_importe

    -- ISR
    LET lc_qry_isr      = " SELECT NVL(SUM(d.monto_en_pesos) * -1, 0) \n",
                          " FROM   tmp_anexo_115 d \n",
                          " WHERE  d.tipo_movimiento = 10 \n",
                          " AND    d.nss IN ( SELECT UNIQUE(nss) \n",
                          "                   FROM   tmp_anexo_115 d \n",
                          "                   WHERE  d.tipo_movimiento  = ? \n",
                          "                   AND    d.fecha_conversion = ?  \n",
                          "                   AND    d.medio_solic      = ?  \n",
                          "                   AND    d.forma_pago       = ? ) \n"

    LET lc_qry_isr = lc_qry_isr CLIPPED

    PREPARE prp_isr_parcial FROM lc_qry_isr
    EXECUTE prp_isr_parcial USING pi_movimiento     ,
                                  pdt_fecha_liquida ,
                                  pc_medio_solic    ,
                                  pc_forma_pago
                            INTO  ld_isr

    -- Se elimina el punto decimal
    LET ld_importe          = (ld_importe + ld_isr) * 100

    -- Cuenta el numero de registros
    LET li_num_registros    = 0

    LET lc_qry_importe      = " SELECT COUNT(DISTINCT d.nss) \n",
                              " FROM   tmp_anexo_115 d \n",
                              " WHERE  d.tipo_movimiento  = ? \n",
                              " AND    d.fecha_conversion = ? \n",
                              " AND    d.medio_solic      = ? \n",
                              " AND    d.forma_pago       = ? \n"

    LET lc_qry_importe      = lc_qry_importe CLIPPED

    PREPARE pre_num_parcial FROM lc_qry_importe
    EXECUTE pre_num_parcial USING pi_movimiento     ,
                                  pdt_fecha_liquida ,
                                  pc_medio_solic    ,
                                  pc_forma_pago
                            INTO  li_num_registros

    RETURN ld_importe, li_num_registros

END FUNCTION

#################################################################################
#Función                        : f_consolida_subencabezado()                   #
#Objetivo                       : Función que concatena los registros que se    #
#                                 localizan en el archivo "SUB_CZA_ANEXO115",   #
#                                 y los envía al archivo llamado "CONSOLIDA_DET"#
#Parámetros de entrada          : Ninguno                                       #
#Valores de retorno             : Ninguno                                       #
#Variables globales importantes : gc_ruta_subencabezado, gc_ruta_cons_detalle   #
#Pantallas utilizadas           : Ninguna                                       #
#Autor                          : David Miguel Garibay Rivera                   #
#Fecha                          : 29/11/2012                                    #
#################################################################################
FUNCTION f_consolida_subencabezado()

    DEFINE
        lc_ruta_tmp             CHAR(0400)  ,
        lc_comando_cat          CHAR(1000)  ,
        lc_comando_rm           CHAR(1000)  ,
        lc_comando_chmod        CHAR(1000)

    -- -----------------------------------------------------------------------------

    WHENEVER ERROR CONTINUE

    -- Inicialización de variables
    LET lc_ruta_tmp = gr_seg_modulo.ruta_envio CLIPPED, "/", gc_usuario CLIPPED, "rep_temporal.tmp"
    LET lc_ruta_tmp = lc_ruta_tmp CLIPPED

    -- Se concatena el subencabezado al archivo temporal
    LET lc_comando_cat  = "cat ", gc_ruta_subencabezado CLIPPED, "> ", lc_ruta_tmp
    LET lc_comando_cat  = lc_comando_cat CLIPPED
    RUN lc_comando_cat

    IF (gi_num_registros = 1) THEN
        -- Si es el primer registro, entonce se crea el archivo consolidado
        LET lc_comando_cat = "cat ", lc_ruta_tmp CLIPPED, "> ", gc_ruta_cons_detalle
        LET lc_comando_cat = lc_comando_cat CLIPPED
        RUN lc_comando_cat

        -- Asigna permisos
        LET lc_comando_chmod = "chmod 777 ", gc_ruta_cons_detalle CLIPPED
        LET lc_comando_chmod = lc_comando_chmod
        RUN lc_comando_chmod

        LET gi_num_consolidado = gi_num_consolidado + 1
    ELSE
        -- Se concatena el contenido del archivo temporal al archivo consolidado
        LET lc_comando_cat = "cat ", lc_ruta_tmp CLIPPED, ">> ", gc_ruta_cons_detalle
        LET lc_comando_cat = lc_comando_cat CLIPPED
        RUN lc_comando_cat

        LET gi_num_consolidado = gi_num_consolidado + 1
    END IF

    -- Elimina el archivo temporal
    LET lc_comando_rm    = "rm ", lc_ruta_tmp CLIPPED
    LET lc_comando_rm    = lc_comando_rm CLIPPED
    RUN lc_comando_rm

    WHENEVER ERROR STOP

END FUNCTION

#################################################################################
#Función                        : f_consolida_detalles()                        #
#Objetivo                       : Función que concatena los registros que se    #
#                                 localizan en los detalles de Transferencias,  #
#                                 disposición y retiros parciales               #
#                                 y los envía al archivo llamado "CONSOLIDA_DET"#
#Parámetros de entrada          : Ninguno                                       #
#Valores de retorno             : Ninguno                                       #
#Variables globales importantes : gc_ruta_det_transferencia ,                   #
#                                 gc_ruta_det_disposiciones     ,               #
#                                 gc_ruta_det_parcial  ,                        #
#                                 gc_ruta_cons_detalle                          #
#Pantallas utilizadas           : Ninguna                                       #
#Autor                          : David Miguel Garibay Rivera                   #
#Fecha                          : 29/11/2012                                    #
#################################################################################
FUNCTION f_consolida_detalles(pi_num_tranfer, pi_num_disposicion, pi_num_parciales)

    DEFINE
        pi_num_tranfer              ,
        pi_num_disposicion          ,
        pi_num_parciales            INTEGER


    DEFINE
        li_indice                   INTEGER

    DEFINE
        lc_ruta_tmp                 CHAR(0400)  ,
        lc_comando_cat              CHAR(1000)  ,
        lc_comando_rm               CHAR(1000)

    -- -----------------------------------------------------------------------------

    -- Inicialización de variables
    LET li_indice = 0

    WHENEVER ERROR CONTINUE

    -- Inicialización de variables
    LET lc_ruta_tmp = gr_seg_modulo.ruta_envio CLIPPED, "/", gc_usuario CLIPPED, "rep_temporal.tmp"
    LET lc_ruta_tmp = lc_ruta_tmp CLIPPED

    IF (pi_num_tranfer <>  0) THEN
        IF (li_indice = 0) THEN
            -- Se crea el archivo temporal
            LET lc_comando_cat = "cat ", gc_ruta_det_transferencia CLIPPED, "> ", lc_ruta_tmp
        ELSE
            -- En caso de que ya exista información para el archivo temporal,
            -- entonces el comando ">>" permite concatenar infomación al archivo existente
            LET lc_comando_cat = "cat ", gc_ruta_det_transferencia CLIPPED, ">> ", lc_ruta_tmp
        END IF

        LET lc_comando_cat = lc_comando_cat CLIPPED
        RUN lc_comando_cat
        LET li_indice = li_indice + 1
    END IF

    IF (pi_num_disposicion <>  0) THEN
        IF (li_indice = 0) THEN
            -- Se ccrea el archivo temporal
            LET lc_comando_cat = "cat ", gc_ruta_det_disposiciones CLIPPED, "> ", lc_ruta_tmp
        ELSE
            -- En caso de que ya exista información para el archivo temporal,
            -- entonces el comando ">>" permite concatenar infomación al archivo existente
            LET lc_comando_cat = "cat ", gc_ruta_det_disposiciones CLIPPED, ">> ", lc_ruta_tmp
        END IF

        LET lc_comando_cat = lc_comando_cat CLIPPED
        RUN lc_comando_cat
        LET li_indice = li_indice + 1
    END IF

    IF (pi_num_parciales <>  0) THEN
        IF (li_indice = 0) THEN
            -- Se crea el archivo temporal
            LET lc_comando_cat = "cat ", gc_ruta_det_parcial CLIPPED, "> ", lc_ruta_tmp
        ELSE
            -- En caso de que ya exista información para el archivo temporal,
            -- entonces el comando ">>" permite concatenar infomación al archivo existente
            LET lc_comando_cat = "cat ", gc_ruta_det_parcial CLIPPED, ">> ", lc_ruta_tmp
        END IF

        LET lc_comando_cat = lc_comando_cat CLIPPED
        RUN lc_comando_cat
        LET li_indice = li_indice + 1
    END IF

    -- Se concatena el contenido del archivo temporal al archivo consolidado
    LET lc_comando_cat = "cat ", lc_ruta_tmp CLIPPED, ">> ", gc_ruta_cons_detalle
    LET lc_comando_cat = lc_comando_cat CLIPPED
    RUN lc_comando_cat

    LET gi_num_consolidado = gi_num_consolidado + 1

    -- Elimina el archivo temporal
    LET lc_comando_rm = "rm ", lc_ruta_tmp CLIPPED
    LET lc_comando_rm = lc_comando_rm CLIPPED
    RUN lc_comando_rm

    WHENEVER ERROR STOP

END FUNCTION

#---------------------------------------------------------------------------#
# f_abre_ventana : Abre la forma donde se depliegan los mensajes de al      #
#                  usuario en la generacion del anexo                       #
#---------------------------------------------------------------------------#

FUNCTION f_abre_ventana()

    OPEN WINDOW retl8372 AT 2,2 WITH FORM "RETL8372"  ATTRIBUTE(BORDER)
    DISPLAY " Ctrl-C : Salir                                       Esc : Ejecutar " AT 2,2 ATTRIBUTE(REVERSE)
    DISPLAY " RETL837         GENERACIÓN DE LOS ANEXOS 114 Y 115                  " AT 3,2 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,60 ATTRIBUTE(REVERSE)

END FUNCTION

#---------------------------------------------------------------------------#
# f_identifica_regimen : Busca los folios involucrados en la tabla temporal #
#                        de dis_cuenta y determina el regimen al que        #
#                        pertenece. En caso de ser DT, actualiza el origen  #
#                        de los recursos a "07"                             #
#---------------------------------------------------------------------------#
FUNCTION f_identifica_regimen()

    DEFINE li_folio LIKE dis_cuenta.folio

    DEFINE
        lc_regimen              CHAR(02)

    -- -----------------------------------------------------------------------------

    LET li_folio    = 0
    INITIALIZE lc_regimen TO NULL

    DECLARE cur_folios CURSOR FOR
        SELECT UNIQUE folio
        FROM   tmp_anexo_115
        ORDER BY 1

    FOREACH cur_folios INTO li_folio

        -- Disposiciones ISSSTE
        SELECT UNIQUE regimen
        INTO   lc_regimen
        FROM   ret_sol_issste_tx
        WHERE  folio    = li_folio
        GROUP BY 1

        IF lc_regimen IS NULL THEN
            -- Transferencias ISSSTE
            SELECT UNIQUE regimen
            INTO   lc_regimen
            FROM   ret_trans_issste
            WHERE  folio    = li_folio
            GROUP BY 1

            IF lc_regimen IS NULL THEN
                -- Parciales Nuevo Sistema
                SELECT UNIQUE regimen
                INTO   lc_regimen
                FROM   ret_parcial_issste
                WHERE  folio    = li_folio
                GROUP BY 1

                IF lc_regimen IS NULL THEN
                    -- Parciales Nuevo Sistema
                    SELECT "OK"
                    FROM   ret_sol_issste_par
                    WHERE  folio    = li_folio
                    GROUP BY 1
                    
                    IF STATUS <> NOTFOUND THEN
                        LET lc_regimen = "DT"
                    ELSE 
                        LET lc_regimen = "XX"
                    END IF -- Parciales 
                END IF
            END IF -- Transferencias ISSSTE
        END IF -- Disposiciones ISSSTE

        IF lc_regimen = "DT" THEN
            UPDATE tmp_anexo_115
            SET    origen_recursos  = "07"
            WHERE  folio            = li_folio
        END IF

        INITIALIZE lc_regimen TO NULL 

    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# f_elimina_finde : Elimina los registros que correspondan a un fin de      #
#                   semana. Esto para evitar que se reporten registros de   #
#                   regimen DT cuando no correspondan.                      #
#                                                                           #
#                            SOLO PENSIONISSSTE                             #
#---------------------------------------------------------------------------#
FUNCTION f_elimina_finde(pr_fechas)

    DEFINE pr_fechas RECORD
        inicio          DATE    ,
        fin             DATE 
    END RECORD 

    DEFINE
        ldt_cont            DATE

    -- -----------------------------------------------------------------------------
    
    LET ldt_cont    = pr_fechas.inicio

    WHILE pr_fechas.fin > ldt_cont
        
        -- Si el dia de la semana es sabado o domingo se elimina de la tabla de
        -- datos de dis_Cuenta
        IF (WEEKDAY(ldt_cont) = 6) OR (WEEKDAY(ldt_cont) = 0) THEN
            DELETE
            FROM   tmp_anexo_115
            WHERE  fecha_conversion = ldt_cont
        END IF
        
        LET ldt_cont = ldt_cont + 1
    END WHILE 

END FUNCTION 

#---------------------------------------------------------------------------#
# f_inserta_DT_finde : Busca si existe uno o dias antes de la fecha inicial #
#                      un dia primero natural de mes y que sea sabado o     #
#                      domingo. En caso de encontrarlo, inserta los         #
#                      registros a la tabla de registros                    #
#                            SOLO PENSIONISSSTE                             #
#---------------------------------------------------------------------------#
FUNCTION f_inserta_DT_finde(pr_fechas)

    DEFINE pr_fechas RECORD
        inicio          DATE    ,
        fin             DATE 
    END RECORD 

    DEFINE
        ldt_fecha_inicio        ,
        ldt_ini_mes             DATE

    -- -----------------------------------------------------------------------------

    LET ldt_fecha_inicio    = pr_fechas.inicio
    LET ldt_ini_mes         = MDY(MONTH(pr_fechas.inicio), 1, YEAR(pr_fechas.inicio))

    IF WEEKDAY(pr_fechas.inicio) = 1 THEN 
        IF (pr_fechas.inicio - 1 = ldt_ini_mes) OR (pr_fechas.inicio - 2 = ldt_ini_mes) THEN

            INSERT INTO tmp_anexo_115
            SELECT *                    ,
                   "08" origen_recursos
            FROM   dis_cuenta
            WHERE  fecha_conversion = ldt_ini_mes
            AND    tipo_movimiento IN (851, 852, 853, 854, 855, 857, 858, 859, 864, -- Retiros totales
                                       861, 862, 863, 865, 866,                     -- Transferencias
                                       856, 884, 885,                               -- Parciales
                                       10                                           -- ISR
                                      )

            LET ldt_fecha_inicio    = ldt_ini_mes
        END IF
    END IF 
    
    RETURN ldt_fecha_inicio

END FUNCTION 

#---------------------------------------------------------------------------#
# f_imprime_cifras_nulo : Envia un detalle nulo al reporte de cifras de     #
#                         control, indicandole si es encabezado o           #
#                         subencabezado                                     #
#                                                                           #
#                         Si ps_bandera -> 0 : Es un encabezado             #
#                                          1 : Es un subencabezado          #
#---------------------------------------------------------------------------#
FUNCTION f_imprime_cifras_nulo(ps_bandera)

    DEFINE
        ps_bandera              SMALLINT

    DEFINE
        lc_valor_nulo           CHAR(1)

    DEFINE
        ls_encabezado           ,
        ls_subencabezado        SMALLINT

    -- -----------------------------------------------------------------------------

    INITIALIZE lc_valor_nulo TO NULL
    LET ls_encabezado       = 0
    LET ls_subencabezado    = 0

    IF ps_bandera = 0 THEN
        LET ls_encabezado       = 1
    ELSE
        LET ls_subencabezado    = 1
    END IF

    OUTPUT TO REPORT rpt_cifras_control_anexo115(0                 ,
                                                 lc_valor_nulo     ,
                                                 "000"             ,
                                                 "00"              ,
                                                 0                 ,
                                                 0                 ,
                                                 0                 ,
                                                 0                 ,
                                                 0                 ,
                                                 0                 ,
                                                 0                 ,
                                                 0                 ,
                                                 0                 ,
                                                 0                 ,
                                                 ls_encabezado     , -- Bandera que indica si se trata de un encabezado
                                                 ls_subencabezado  , -- Bandera que indica si se trata de un subencabezado
                                                 0                   -- Bandera que indica si se trata de un detalle
                                                )
END FUNCTION


#---------------------------------------------------------------------------#
# f_regresa_tipo_ret_catalogo : Realiza la conversion del tipo de retiro de #
#                               acuerdo al catalogo del anexo 115           #
#---------------------------------------------------------------------------#
FUNCTION f_regresa_tipo_ret_catalogo(pi_movimiento)

    DEFINE pi_movimiento LIKE dis_cuenta.tipo_movimiento

    DEFINE
        lc_tipo_retiro      CHAR(03)

    -- -----------------------------------------------------------------------------

    CASE (pi_movimiento)

       -- Disposiciones
        WHEN 851
            LET lc_tipo_retiro = "001"
        WHEN 852
            LET lc_tipo_retiro = "002"
        WHEN 853
            LET lc_tipo_retiro = "003"
        WHEN 854
            LET lc_tipo_retiro = "004"
        WHEN 855
            LET lc_tipo_retiro = "005"
        WHEN 857
            LET lc_tipo_retiro = "007"
        WHEN 858
            LET lc_tipo_retiro = "011"
        WHEN 859
            LET lc_tipo_retiro = "013"
        WHEN 864
            LET lc_tipo_retiro = "009"
        WHEN 867
            LET lc_tipo_retiro = "015"

        -- Transferencias
        WHEN 861
            LET lc_tipo_retiro = "008"
        WHEN 862
            LET lc_tipo_retiro = "009"
        WHEN 863
            LET lc_tipo_retiro = "010"
        WHEN 865
            LET lc_tipo_retiro = "012"
        WHEN 866
            LET lc_tipo_retiro = "014"

        -- Parciales
        WHEN 856
            LET lc_tipo_retiro  = "006"

        WHEN 884
            LET lc_tipo_retiro  = "006"

        WHEN 885
            LET lc_tipo_retiro  = "006"

    END CASE

    RETURN lc_tipo_retiro

END FUNCTION

#---------------------------------------------------------------------------#
# f_obten_tipo_retiro : Regresa el tipo de retiro correspondiente al        #
#                       movimiento de dis_cuenta                            #
#---------------------------------------------------------------------------#
FUNCTION f_obten_tipo_retiro(pi_movimiento)

    DEFINE
        pi_movimiento           SMALLINT

    DEFINE
        lc_tipo_retiro          CHAR(1)

    -- -----------------------------------------------------------------------------

    SELECT tipo_retiro
    INTO   lc_tipo_retiro
    FROM   tab_ret_issste
    WHERE  movimiento   = pi_movimiento
    GROUP BY 1

    RETURN lc_tipo_retiro

END FUNCTION

#################################################################################
#Reporte                        : rpt_anexo_115()                               #
#Objetivo                       : Genera el encabezado del anexo 115            #
#Parámetros de entrada          : Ninguno                                       #
#Valores de retorno             : Ninguno                                       #
#Variables globales importantes : Ninguna                                       #
#Pantallas utilizadas           : Ninguna                                       #
#Autor                          : David Miguel Garibay Rivera                   #
#Fecha                          : 29/11/2012                                    #
#################################################################################
REPORT rpt_anexo_115()

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
            PRINT
                COLUMN 001, "000"                             ,-- Tipo registro
                COLUMN 004, "0704"                            ,-- Tipo de Archivo
                COLUMN 008, "001"                             ,-- Tipo de Entidad que reporta
                COLUMN 011, gs_codigo_afore USING "&&&"       ,-- Clave de Entidad que reporta
                COLUMN 014, HOY USING "YYYYMMDD"              ,-- Fecha de Información
                COLUMN 022, "119"                             ,-- Tamaño del Registro
                COLUMN 025, gi_num_registros USING "&&&&&"    ,-- Número de registros
                COLUMN 030, 90 SPACES                          -- Filler

END REPORT



#################################################################################
#Reporte                        : rpt_subcza_anexo_115()                        #
#Objetivo                       : Genera el encabezado del anexo 115            #
#Parámetros de entrada          : Ninguno                                       #
#Valores de retorno             : Ninguno                                       #
#Variables globales importantes : Ninguna                                       #
#Pantallas utilizadas           : Ninguna                                       #
#Autor                          : David Miguel Garibay Rivera                   #
#Fecha                          : 29/11/2012                                    #
#################################################################################
REPORT rpt_subcza_anexo_115(pr_fechas)

    DEFINE pr_fechas RECORD
        inicio      DATE    ,
        fin         DATE
    END RECORD

    -- -----------------------------------------------------------------------------

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
            PRINT
                COLUMN 001, "101"                               ,-- Tipo registro
                COLUMN 004, pr_fechas.inicio USING "YYYYMMDD"   ,-- Fecha de la primera liquidación reportada
                COLUMN 012, pr_fechas.fin    USING "YYYYMMDD"   ,-- Fecha de la última liquidación reportada
                COLUMN 020, 100 SPACES                           -- Filler

END REPORT

#################################################################################
#Reporte                        : det_transfer_anexo_115()                      #
#Objetivo                       : Genera el detalle de las transferencias       #
#Parámetros de entrada          : Ninguno                                       #
#Valores de retorno             : Ninguno                                       #
#Variables globales importantes : Ninguna                                       #
#Pantallas utilizadas           : Ninguna                                       #
#Autor                          : David Miguel Garibay Rivera                   #
#Fecha                          : 29/11/2012                                    #
#################################################################################
REPORT det_transfer_anexo_115(pdt_fecha_liquida       ,
                              pi_siefore              ,
                              pd_tot_retiro_2008      ,
                              pd_tot_cesantia_vejez   ,
                              pd_tot_sar_issste_92    ,
                              pd_tot_ahorro_solidario ,
                              pd_tot_complemen_retiro ,
                              pd_tot_vivienda_92      ,
                              pd_tot_vivienda_2008    ,
                              pi_num_registros        ,
                              pc_tipo_retiro_rpt      ,
                              pc_origen_recursos)

    DEFINE
        pdt_fecha_liquida           DATE            ,
        pi_siefore                  INTEGER         ,
        pd_tot_retiro_2008          DECIMAL(22,0)   ,
        pd_tot_cesantia_vejez       DECIMAL(22,0)   ,
        pd_tot_sar_issste_92        DECIMAL(22,0)   ,
        pd_tot_ahorro_solidario     DECIMAL(22,0)   ,
        pd_tot_complemen_retiro     DECIMAL(22,0)   ,
        pd_tot_vivienda_92          DECIMAL(22,0)   ,
        pd_tot_vivienda_2008        DECIMAL(22,0)   ,
        pi_num_registros            INTEGER         ,
        pc_tipo_retiro_rpt          CHAR(3)         ,
        pc_origen_recursos          CHAR(2)

    -- -----------------------------------------------------------------------------

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
            CALL f_siefore_consar(pi_siefore) RETURNING pi_siefore

            PRINT
                COLUMN 001, "301"                                           ,-- Tipo registro
                COLUMN 004, pdt_fecha_liquida USING "YYYYMMDD"              ,-- Fecha de liquidacion
                COLUMN 012, pc_tipo_retiro_rpt                              ,-- Tipo de retiro
                COLUMN 015, pc_origen_recursos                              ,-- Origen de los recursos
                COLUMN 017, pi_siefore  USING "&&"                          ,-- Siefore
                COLUMN 019, pd_tot_retiro_2008      USING "&&&&&&&&&&&&&"   ,-- Retiro 2008
                COLUMN 032, pd_tot_cesantia_vejez   USING "&&&&&&&&&&&&&"   ,-- Cesantía y Vejez
                COLUMN 045, pd_tot_sar_issste_92    USING "&&&&&&&&&&&&&"   ,-- Sar ISSSTE 92
                COLUMN 058, pd_tot_ahorro_solidario USING "&&&&&&&&&&&&&"   ,-- Ahorro Solidario
                COLUMN 071, pd_tot_complemen_retiro USING "&&&&&&&&&&&&&"   ,-- Complementarias Retiro
                COLUMN 084, pd_tot_vivienda_92      USING "&&&&&&&&&&&&&"   ,-- Vivienda 92
                COLUMN 097, pd_tot_vivienda_2008    USING "&&&&&&&&&&&&&"   ,-- Vivienda 2008
                COLUMN 110, pi_num_registros        USING "&&&&&&&&&&"       -- Número de registros

END REPORT

#################################################################################
#Reporte                        : det_disp_anexo_115()                          #
#Objetivo                       : Genera el detalle de las disposiciones       #
#Parámetros de entrada          : Ninguno                                       #
#Valores de retorno             : Ninguno                                       #
#Variables globales importantes : Ninguna                                       #
#Pantallas utilizadas           : Ninguna                                       #
#Autor                          : David Miguel Garibay Rivera                   #
#Fecha                          : 29/11/2012                                    #
#################################################################################
REPORT det_disp_anexo_115(pdt_fecha_liquida       ,
                          pi_siefore              ,
                          pd_tot_retiro_2008      ,
                          pd_tot_cesantia_vejez   ,
                          pd_tot_sar_issste_92    ,
                          pd_tot_ahorro_solidario ,
                          pd_tot_complemen_retiro ,
                          pd_tot_vivienda_92      ,
                          pd_tot_vivienda_2008    ,
                          pi_num_registros        ,
                          pc_tipo_retiro_rpt      ,
                          pc_origen_recursos)

    DEFINE
        pdt_fecha_liquida               DATE            ,
        pi_siefore                      INTEGER         ,
        pd_tot_retiro_2008              DECIMAL(22,0)   ,
        pd_tot_cesantia_vejez           DECIMAL(22,0)   ,
        pd_tot_sar_issste_92            DECIMAL(22,0)   ,
        pd_tot_ahorro_solidario         DECIMAL(22,0)   ,
        pd_tot_complemen_retiro         DECIMAL(22,0)   ,
        pd_tot_vivienda_92              DECIMAL(22,0)   ,
        pd_tot_vivienda_2008            DECIMAL(22,0)   ,
        pi_num_registros                INTEGER         ,
        pc_tipo_retiro_rpt              CHAR(3)         ,
        pc_origen_recursos              CHAR(2)

    -- -----------------------------------------------------------------------------

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
             CALL f_siefore_consar(pi_siefore) RETURNING pi_siefore

            PRINT
                COLUMN 001, "302"                                           ,-- Tipo registro
                COLUMN 004, pdt_fecha_liquida USING "YYYYMMDD"              ,-- Fecha de liquidacion
                COLUMN 012, pc_tipo_retiro_rpt                              ,-- Tipo de retiro según información anexada
                COLUMN 015, pc_origen_recursos                              ,-- Origen de los recursos
                COLUMN 017, pi_siefore USING "&&"                           ,-- Siefore
                COLUMN 019, pd_tot_retiro_2008      USING "&&&&&&&&&&&&&"   ,-- Retiro 2008
                COLUMN 032, pd_tot_cesantia_vejez   USING "&&&&&&&&&&&&&"   ,-- Cesantía y Vejez
                COLUMN 045, pd_tot_sar_issste_92    USING "&&&&&&&&&&&&&"   ,-- Sar ISSSTE 92
                COLUMN 058, pd_tot_ahorro_solidario USING "&&&&&&&&&&&&&"   ,-- Ahorro Solidario
                COLUMN 071, pd_tot_complemen_retiro USING "&&&&&&&&&&&&&"   ,-- Complementarias Retiro
                COLUMN 084, pd_tot_vivienda_92      USING "&&&&&&&&&&&&&"   ,-- Vivienda 92
                COLUMN 097, pd_tot_vivienda_2008    USING "&&&&&&&&&&&&&"   ,-- Vivienda 2008
                COLUMN 110, pi_num_registros        USING "&&&&&&&&&&"       -- Número de registros con estas características

END REPORT

#################################################################################
#Reporte                        : det_parcial_anexo_115()                       #
#Objetivo                       : Genera el detalle de los retiros parciales    #
#Parámetros de entrada          : Ninguno                                       #
#Valores de retorno             : Ninguno                                       #
#Variables globales importantes : Ninguna                                       #
#Pantallas utilizadas           : Ninguna                                       #
#Autor                          : David Miguel Garibay Rivera                   #
#Fecha                          : 29/11/2012                                    #
#################################################################################
REPORT det_parcial_anexo_115(pdt_fecha_liquida      ,
                             pd_importe             ,
                             pi_num_registros       ,
                             pc_tipo_retiro_rpt     ,
                             pi_origen              ,
                             pc_medio_solic         ,
                             pc_forma_pago)

    DEFINE
        pdt_fecha_liquida           DATE            ,
        pd_importe                  DECIMAL(22,0)   ,
        pi_num_registros            INTEGER         ,
        pc_tipo_retiro_rpt          CHAR(3)         ,
        pi_origen                   CHAR(2)            -- CPL-2902

    DEFINE pc_medio_solic     CHAR(003)
    DEFINE pc_forma_pago      CHAR(003)

    -- -----------------------------------------------------------------------------

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
            PRINT
                COLUMN 001, "303"                                    ,-- Tipo registro
                COLUMN 004, pdt_fecha_liquida USING "YYYYMMDD"       ,-- Fecha de liquidacion
                COLUMN 012, pc_tipo_retiro_rpt                       ,-- Tipo de retiro
                COLUMN 015, pd_importe        USING "&&&&&&&&&&&&&"  ,-- Monto Pagado al trabajador   --CPL-2902
                COLUMN 028, pi_num_registros  USING "&&&&&&&&&&"     ,-- Numero de registros          --CPL-2902
                COLUMN 038, pi_origen                                ,-- Origen de los recursos       --CPL-2902
                COLUMN 040, pc_medio_solic                           ,-- De acuerdo al Catálogo  de Medio de Solicitud del Catálogo General vigente
                COLUMN 043, pc_forma_pago                            ,-- De acuerdo al Catálogo  de Formas de Pago del Catálogo General vigente
                COLUMN 046, 74 SPACES                                 -- Filler                       --CPL-2902

END REPORT

#################################################################################
#Reporte                        : rpt_cifras_control_anexo115()                #
#Objetivo                       : Genera el reporte de cifras de control del    #
#                                 anexo 115                                     #
#Parámetros de entrada          : Ninguno                                       #
#Valores de retorno             : Ninguno                                       #
#Variables globales importantes : Ninguna                                       #
#Pantallas utilizadas           : Ninguna                                       #
#Autor                          : David Miguel Garibay Rivera                   #
#Fecha                          : 17/12/2012                                    #
#################################################################################
REPORT rpt_cifras_control_anexo115(pi_tipo_detalle          ,
                                   pdt_fecha_liquidacion    ,
                                   pc_tipo_retiro           ,
                                   pc_origen_recursos       ,
                                   pi_siefore               ,
                                   pd_imp_retiro_2008       ,
                                   pd_imp_cesantia_vejez    ,
                                   pd_imp_sar_issste_92     ,
                                   pd_imp_ahorro_solidario  ,
                                   pd_imp_complemen_retiro  ,
                                   pd_imp_vivienda_92       ,
                                   pd_imp_vivienda_2008     ,
                                   pd_imp_rcv               ,
                                   pi_num_registros         ,
                                   pi_bnd_encabezado        ,
                                   pi_bnd_subencabezado     ,
                                   pi_bnd_detalle
                                  )
    DEFINE
        pi_tipo_detalle                 INTEGER         ,
        pdt_fecha_liquidacion           DATE            ,
        pc_tipo_retiro                  CHAR(3)         ,
        pc_origen_recursos              CHAR(2)         ,
        pi_siefore                      INTEGER         ,
        pd_imp_retiro_2008              DECIMAL(22,2)   ,
        pd_imp_cesantia_vejez           DECIMAL(22,2)   ,
        pd_imp_rcv                      DECIMAL(22,2)   ,
        pd_imp_sar_issste_92            DECIMAL(22,2)   ,
        pd_imp_ahorro_solidario         DECIMAL(22,2)   ,
        pd_imp_complemen_retiro         DECIMAL(22,2)   ,
        pd_imp_vivienda_92              DECIMAL(22,2)   ,
        pd_imp_vivienda_2008            DECIMAL(22,2)   ,
        pi_num_registros                INTEGER         ,
        pi_bnd_encabezado               INTEGER         ,
        pi_bnd_subencabezado            INTEGER         ,
        pi_bnd_detalle                  INTEGER         ,
        ld_importe_total                DECIMAL(22,2)   ,
        li_num_det_01                   INTEGER         ,
        li_num_det_02                   INTEGER         ,
        li_num_det_03                   INTEGER         ,
        li_num_subencabezado            INTEGER         ,
        li_num_encabezado               INTEGER

    -- -----------------------------------------------------------------------------

    OUTPUT
        PAGE LENGTH   20
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        FIRST PAGE HEADER
            --Inicialización de variables
            LET li_num_det_01           = 0
            LET li_num_det_02           = 0
            LET li_num_det_03           = 0
            LET li_num_subencabezado    = 0
            LET li_num_encabezado       = 0

            PRINT
                "TIPO DE DETALLE"                   , "|",
                "FECHA LIQUIDACIÓN"                 , "|",
                "TIPO DE RETIRO"                    , "|",
                "ORIGEN DE RECURSOS"                , "|",
                "SIEFORE"                           , "|",
                "RETIRO 08"                         , "|",
                "CESANTÍA Y VEJEZ"                  , "|",
                "TOTAL RCV"                         , "|",
                "SARISSSTE 92"                      , "|",
                "AHORRO SOLIDARIO"                  , "|",
                "COMPLEMENTARIAS DE RETIRO"         , "|",
                "FOVISSSTE 92"                      , "|",
                "FOVISSSTE 08"                      , "|",
                "TOTAL"                             , "|",
                "NO. DE REGISTROS"                  , "|"

        ON EVERY ROW
            -- En caso de tratarse de un encabezado, entonces no imprime detalle
            IF (pi_bnd_encabezado = 1) THEN
                LET li_num_encabezado = li_num_encabezado + pi_bnd_encabezado
            END IF

            -- En caso de tratarse de un subencabezado, entonces no imprime detalle
            IF (pi_bnd_subencabezado = 1) THEN
               LET li_num_subencabezado = li_num_subencabezado + pi_bnd_subencabezado
            END IF

            -- Imprime el detalle
            IF (pi_bnd_detalle = 1) THEN
                IF (pc_origen_recursos IS NULL) THEN
                    LET pc_origen_recursos = "08"
                ELSE
                    IF (pi_num_registros = 0) THEN
                        LET pc_origen_recursos = "00"
                    END IF
                END IF

                CASE (pi_tipo_detalle)
                    WHEN 301
                        LET li_num_det_01 = li_num_det_01 + 1

                    WHEN 302
                        LET li_num_det_02 = li_num_det_02 + 1

                    WHEN 303
                        LET li_num_det_03 = li_num_det_03 + 1
                END CASE

                LET ld_importe_total = pd_imp_retiro_2008      +
                                       pd_imp_cesantia_vejez   +
                                       pd_imp_rcv              +
                                       pd_imp_sar_issste_92    +
                                       pd_imp_ahorro_solidario +
                                       pd_imp_complemen_retiro +
                                       pd_imp_vivienda_92      +
                                       pd_imp_vivienda_2008

                -- Se trata de un detalle
                PRINT pi_tipo_detalle                 ,"|",
                      pdt_fecha_liquidacion USING "DD/MM/YYYY","|",
                      pc_tipo_retiro        CLIPPED   ,"|",
                      pc_origen_recursos    CLIPPED   ,"|",
                      pi_siefore            CLIPPED   ,"|",
                      pd_imp_retiro_2008/100          ,"|",
                      pd_imp_cesantia_vejez/100       ,"|",
                      pd_imp_rcv/100                  ,"|",
                      pd_imp_sar_issste_92/100        ,"|",
                      pd_imp_ahorro_solidario/100     ,"|",
                      pd_imp_complemen_retiro/100     ,"|",
                      pd_imp_vivienda_92/100          ,"|",
                      pd_imp_vivienda_2008/100        ,"|",
                      ld_importe_total/100            ,"|",
                      pi_num_registros                ,"|"
            END IF

        ON LAST ROW
            PRINT " "
            PRINT " No. de Registros de Encabezado      ", li_num_encabezado
            PRINT " No. de Registros de Subencabezado   ", li_num_subencabezado
            PRINT " Total de Registros de Detalle 1     ", li_num_det_01
            PRINT " Total de Registros de Detalle 2     ", li_num_det_02
            PRINT " Total de Registros de Detalle 3     ", li_num_det_03

END REPORT


#########################################################################
FUNCTION f_siefore_consar(ls_siefore)

DEFINE ls_siefore       SMALLINT
DEFINE ls_siefore_cns   SMALLINT

SELECT  NVL(cod_siefore_cns,0)
INTO  ls_siefore_cns
FROM   tab_siefore_local   
WHERE  codigo_siefore >= 14
AND    codigo_siefore = ls_siefore


IF ls_siefore_cns IS NULL THEN 
   LET ls_siefore_cns = 0 
END IF 

RETURN ls_siefore_cns
END FUNCTION 
#---------------------------------------------------------------------------#
# fn_modifica_siefore_viv : Modifica la siefore 12 en los registros que     #
#                          pagan la vivienda y le asigna la siefore basica  #
#                          que tenga el trabajador                          #
#---------------------------------------------------------------------------#
FUNCTION fn_modifica_siefore_viv()

    DEFINE lr_vivienda RECORD
        nss                     LIKE dis_cuenta.nss                 ,
        folio                   LIKE dis_cuenta.folio               ,
        consecutivo_lote        LIKE dis_cuenta.consecutivo_lote    ,
        tipo_movimiento         LIKE dis_cuenta.tipo_movimiento     ,
        origen_recursos         CHAR(02)
    END RECORD

    DEFINE
        ls_sie_basica           SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_sie_basica = 0
    INITIALIZE lr_vivienda.* TO NULL

    DECLARE cur_viv CURSOR FOR
        SELECT UNIQUE(nss)          ,
               folio                ,
               consecutivo_lote     ,
               tipo_movimiento      ,
               origen_recursos
        FROM   tmp_anexo_115
        WHERE  siefore  = 12
        ORDER BY folio

    FOREACH cur_viv INTO lr_vivienda.*

        -- Si el origen es 08, los recursos tienen una siefore basica
        IF (lr_vivienda.origen_recursos = "08") THEN
            SELECT UNIQUE(siefore)
            INTO   ls_sie_basica
            FROM   tmp_anexo_115
            WHERE  nss              = lr_vivienda.nss
            AND    folio            = lr_vivienda.folio
            AND    consecutivo_lote = lr_vivienda.consecutivo_lote
            AND    tipo_movimiento  = lr_vivienda.tipo_movimiento
            AND    siefore         <> 12
            AND    subcuenta       IN (30,31,32,33,34)
            
            IF SQLCA.SQLCODE =  100 THEN 
               SELECT UNIQUE(siefore)
               INTO   ls_sie_basica
               FROM   tmp_anexo_115
               WHERE  nss              = lr_vivienda.nss
               AND    folio            = lr_vivienda.folio
               AND    consecutivo_lote = lr_vivienda.consecutivo_lote
               AND    tipo_movimiento  = lr_vivienda.tipo_movimiento
               AND    siefore         <> 12
               AND    subcuenta       IN (11,12,24,25)
                  
                  IF SQLCA.SQLCODE =  100 THEN 
                     SELECT UNIQUE(siefore)
                     INTO   ls_sie_basica
                     FROM   tmp_anexo_115
                     WHERE  nss              = lr_vivienda.nss
                     AND    folio            = lr_vivienda.folio
                     AND    consecutivo_lote = lr_vivienda.consecutivo_lote
                     AND    tipo_movimiento  = lr_vivienda.tipo_movimiento
                     AND    siefore         <> 12
                     AND    subcuenta       IN (13)
                  END IF
            END IF  
            
            IF ( (ls_sie_basica = 0) OR (ls_sie_basica IS NULL) ) THEN
                -- Se modifica la siefore a 100 para que en los querys donde se enlistan
                -- los movimientos, se tenga a la vivienda en el final
                LET ls_sie_basica = 100
            END IF

            IF (ls_sie_basica <> 10) THEN
                UPDATE tmp_anexo_115
                SET    siefore          = ls_sie_basica
                WHERE  nss              = lr_vivienda.nss
                AND    folio            = lr_vivienda.folio
                AND    consecutivo_lote = lr_vivienda.consecutivo_lote
                AND    tipo_movimiento  = lr_vivienda.tipo_movimiento
                AND    siefore          = 12
            END IF
        ELSE
            -- Si el origen es 07 los recursos son BANXICO, por lo que
            -- se les asigna la siefore como cero
            UPDATE tmp_anexo_115
            SET    siefore          = 0
            WHERE  nss              = lr_vivienda.nss
            AND    folio            = lr_vivienda.folio
            AND    consecutivo_lote = lr_vivienda.consecutivo_lote
            AND    tipo_movimiento  = lr_vivienda.tipo_movimiento
            AND    siefore          = 12
        END IF

        LET ls_sie_basica = 0
        INITIALIZE lr_vivienda.* TO NULL

    END FOREACH

END FUNCTION
