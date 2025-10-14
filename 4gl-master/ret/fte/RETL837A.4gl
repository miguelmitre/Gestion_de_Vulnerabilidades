#############################################################################
#Proyecto          => AFORES ( MEXICO )                                     #
#Propietario       => E.F.P.                                                #
#Programa RETL837A => ANEXO 114. RETIROS IMSS                               #
#Sistema           => RET                                                   #
#Autor             => David Miguel Garibay Rivera                           #
#Fecha             => 27 NOVIEMBRE  DE 2012                                 #
#Modificaciones    => Javier Gonzalez Jeronimo                              #
#Fecha             => 19 DE DICIEMBRE DE 2012                               #
#                  => Se realizaron los siguientes ajustes :                #
#                     - Se inicializan las variables despues de generar cada#
#                       detalle, ya que estaba generando secciones          #
#                       duplicadas.                                         #
#                     - Se ajusta el detalle de disposicion para que agrupe #
#                       los saldos de vivienda en cada rubro de siefore. En #
#                       caso de solo tener montos de vivienda estos se      #
#                       colocan en una sola linea                           #
#                     - Se eliminan registros que no tengan movimientos     #
#                       liquidados en dicho dia                             #
#                     - Se agrupan los movimientos de transferencias en una #
#                       sola linea                                          #
#                     - Se incluyen los movimientos de PMG correctamente    #
#Modificaciones    => Javier Gonzalez Jeronimo                              #
#Fecha             => 28 DE DICIEMBRE DE 2012                               #
#                  => Se realizaron los siguientes ajustes :                #
#                     - Reacomodo del codigo. Se declararon funciones       #
#                       para facilitar el manteminimento del programa       #
#                     - Se separo el tipo de retiro F de acuerdo al regimen #
#                       para que se pueda detectar y reportar en el anexo   #
#                     - La funcion que obtiene el numero de registros para  #
#                       la subcuenta 7 no tomaba en cuenta los retiros F    #
#Modificaciones    => Javier Gonzalez Jeronimo                              #
#Fecha             => 3 DE ENERO DE 2013                                    #
#                  => Se realizaron los siguientes ajustes :                #
#                     - Unificacion de versiones en todas las afores        #
#                     - Se incluyeron las funciones de las librerias de     #
#                       Retiros                                             #
#############################################################################

DATABASE safre_af

GLOBALS

    DEFINE gr_seg_modulo RECORD LIKE seg_modulo.*

    DEFINE
        HOY                         ,
        gdt_fecha_ini               ,
        gdt_fecha_fin               DATE

    DEFINE
        enter                       CHAR(001)   ,
        gc_ruta_encabezado          CHAR(200)   ,
        gc_ruta_subencabezado       CHAR(200)   ,
        gc_ruta_det_transferencia   CHAR(200)   ,
        gc_ruta_det_disposiciones   CHAR(200)   ,
        gc_ruta_det_parcial         CHAR(200)   ,
        gc_ruta_det_rechazo         CHAR(200)   ,
        gc_ruta_cons_detalle        CHAR(200)   ,
        gc_nom_archivo              CHAR(400)   ,
        gc_usuario                  CHAR(030)   ,
        gc_nombre_anexo_114         CHAR(024)   ,
        gc_nom_rpt_control_703      CHAR(023)


    DEFINE
        gi_num_registros            ,
        gi_num_consolidado          INTEGER

    DEFINE
        gs_mov_tipo_F97             ,
        gs_codigo_afore             ,
        gs_bnd_argumentos           ,
        gs_coppel                   SMALLINT

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
        CALL f_carga_informacion(gdt_fecha_ini, gdt_fecha_fin)
        CALL f_genera_anexo_114()
    ELSE
        CALL f_lib_error_msg("NUMERO INCORRECTO DE ARGUMENTOS")
    END IF

    CLOSE WINDOW win_anexo_114

END MAIN

#################################################################################
#Función                        : inicio()                                      #
#Objetivo                       : Función que inicializa las variables globales #
#Parámetros de entrada          : Ninguno                                       #
#Valores de retorno             : Ninguno                                       #
#Variables globales importantes : gr_seg_modulo ,                               #
#                                 gc_usuario     ,                               #
#                                 gs_codigo_afore ,                             #
#                                 gdt_fecha_ini ,                               #
#                                 gdt_fecha_fin                                 #
#Pantallas utilizadas           : Ninguna                                       #
#Autor                          : David Miguel Garibay Rivera                   #
#Fecha                          : 27/11/2012                                    #
#################################################################################
FUNCTION inicio()

    -- Inicialización de variables
    LET HOY                 = TODAY
    LET gc_usuario          = f_lib_obten_user()
    LET gi_num_registros    = 0
    LET gi_num_consolidado  = 0
    LET gs_mov_tipo_F97     = 842 -- Tipo de movimiento especial para Registros F
    LET gs_bnd_argumentos   = NUM_ARGS()

    IF (gs_bnd_argumentos = 4) THEN
        LET gdt_fecha_ini           = ARG_VAL(1)
        LET gdt_fecha_fin           = ARG_VAL(2)
        LET gc_nombre_anexo_114     = ARG_VAL(3)
        LET gc_nom_rpt_control_703  = ARG_VAL(4) -- Reporte de cifras Control para el anexo 114
    END IF

    SELECT *
    INTO   gr_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    SELECT codigo_afore
    INTO   gs_codigo_afore
    FROM   tab_afore_local
    GROUP BY 1

    SELECT afore_cod
    INTO   gs_coppel
    FROM   tab_afore
    WHERE  afore_desc = "COPPEL"

END FUNCTION

#################################################################################
#Función                        : f_carga_informacion()                         #
#Objetivo                       : Función que realiza la busqueda de la         #
#                                 información en dis_cuenta y la almacena en una#
#                                 tabla temporal llamada "tmp_anexo_114".       #
#Parámetros de entrada          : Ninguno                                       #
#Valores de retorno             : Ninguno                                       #
#Variables globales importantes : Ninguna                                       #
#Pantallas utilizadas           : Ninguna                                       #
#Autor                          : David Miguel Garibay Rivera                   #
#Fecha                          : 27/11/2012                                    #
#################################################################################
FUNCTION f_carga_informacion(pr_fechas)

    DEFINE pr_fechas RECORD
        fecha_ini  DATE,
        fecha_fin  DATE
    END RECORD

    -- -----------------------------------------------------------------------------

    DISPLAY " OBTENIENDO DATOS DE LA CUENTA INDIVIDUAL - ANEXO 114 ...  " AT 19,1 ATTRIBUTE(REVERSE)
    SLEEP 1

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_anexo_114
    WHENEVER ERROR STOP

    SELECT *                    ,
           "XX"   des_tipo_B    ,
           "XXX"  medio_solic   ,
           "XXX"  forma_pago
    FROM   dis_cuenta
    WHERE  fecha_conversion BETWEEN pr_fechas.fecha_ini AND pr_fechas.fecha_fin
    AND    tipo_movimiento IN (
                               820, 825, 830, 840, 850, 860, 880,       -- Retiros totales
                               870, 875, 876, 877, 878,                 -- Retiros Parciales
                               800, 810, 815, 806, 805,                 -- Transferencias
                               817, 818, 819,                           -- Vivienda Ventanilla
                               841,                                     -- PMG
                               10 ,                                     -- ISR
                               835                                      -- DISPOSICION    -- CPL-2794
                              )
    INTO TEMP tmp_anexo_114

    -- CPL-2902 INI
    -- UPDATE tmp_anexo_114
    -- SET    tipo_movimiento = 810
    -- WHERE  tipo_movimiento = 806;
    -- CPL-2902 FIN
    
    UPDATE STATISTICS FOR TABLE tmp_anexo_114
    -- Todos los movimientos de vivienda por ley INFONAVIT se convierten
    -- a su retiro correspondiente
    CALL f_modifica_viv_infonavit()
    -- Se determina la modalidad de pago para el tipo de desempleo B
    CALL f_modifica_desempleo_B()
    -- Separamos en un tipo de movimiento temporal los registros que corresponden
    -- al retiro F regimen 97
    CALL f_genera_plan_privado_97()
    -- CPL-1840 #CPL-2320 se des comenta la modificacion de la siefore
    -- Se inhabilita para Coppel
--    -- Se cambia la siefore 11 por la siefore basica del trabajador en los registros
      CALL f_modifica_siefore_viv()
    -- Se determina el medio de solicitud y la forma de pago
    CALL f_modifica_medio_solic_forma_pago()


END FUNCTION

#################################################################################
#Función                        : f_genera_anexo_114()                          #
#Objetivo                       : Función que                                   #
#Parámetros de entrada          : Ninguno                                       #
#Valores de retorno             : Ninguno                                       #
#Variables globales importantes : Ninguna                                       #
#Pantallas utilizadas           : Ninguna                                       #
#Autor                          : David Miguel Garibay Rivera                   #
#Fecha                          : 27/11/2012                                    #
#################################################################################
FUNCTION f_genera_anexo_114()
DISPLAY "GENERANDO 114"
    CALL paso_uno(gdt_fecha_ini, gdt_fecha_fin) -- Genera subencabezados y detalles
    CALL paso_dos()                             -- Genera encabezado
    CALL paso_tres()        -- Concatena el Encabezado, Subencabezados y Detalles
    CALL paso_cuatro()      -- Asigna permisos al archivo final y elimina los archivos temporales

    DISPLAY "                                                           " AT 18,1
    DISPLAY " GENERANDO ARCHIVOS PLANOS DEL ANEXO 114 ...               " AT 19,1 ATTRIBUTE(REVERSE)
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
#Fecha                          : 27/11/2012                                    #
#################################################################################
FUNCTION paso_uno(pdt_fecha_ini_proceso, pdt_fecha_fin_proceso)

    DEFINE
        pdt_fecha_ini_proceso           ,
        pdt_fecha_fin_proceso           DATE

    DEFINE
        ldt_fecha_ini_semana            ,
        ldt_fecha_fin_semana            DATE

    DEFINE
        li_diferencia                   ,
        li_indice                       INTEGER

    DEFINE
        lc_nom_cifras_control           CHAR(200),
        lc_nom_rpt_detalle              CHAR(200),
        lc_comando                      CHAR(300)

    -- -----------------------------------------------------------------------------

    DISPLAY " GENERANDO DATOS DE LOS REPORTES DEL ANEXO 114 ...         " AT 18,1 ATTRIBUTE(REVERSE)
    SLEEP 1
    
    LET gc_ruta_det_transferencia   = gr_seg_modulo.ruta_envio CLIPPED, "/", gc_usuario CLIPPED, ".DET_TRANSFER_ANEXO114"
    LET gc_ruta_det_disposiciones   = gr_seg_modulo.ruta_envio CLIPPED, "/", gc_usuario CLIPPED, ".DET_DISP_ANEXO114"
    LET gc_ruta_det_parcial         = gr_seg_modulo.ruta_envio CLIPPED, "/", gc_usuario CLIPPED, ".DET_PARCIAL_ANEXO114"
    LET gc_ruta_det_rechazo         = gr_seg_modulo.ruta_envio CLIPPED, "/", gc_usuario CLIPPED, ".DET_SUBCUENTA_ANEXO114"
    LET gc_ruta_cons_detalle        = gr_seg_modulo.ruta_envio CLIPPED, "/", gc_usuario CLIPPED, ".CONSOLIDA_DET"

    -- Inicia el reporte de cifras de control del anexo 114
    LET lc_nom_cifras_control       = gr_seg_modulo.ruta_listados CLIPPED,"/", gc_nom_rpt_control_703 CLIPPED
    LET lc_nom_cifras_control       = lc_nom_cifras_control CLIPPED

    START REPORT rpt_cifras_control_anexo114 TO lc_nom_cifras_control
    
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

            CALL f_genera_subencabezado(ldt_fecha_ini_semana, ldt_fecha_fin_semana)
            CALL f_genera_transferencias(ldt_fecha_ini_semana, ldt_fecha_fin_semana)
            CALL f_genera_disposiciones(ldt_fecha_ini_semana, ldt_fecha_fin_semana)
            CALL f_genera_parciales(ldt_fecha_ini_semana, ldt_fecha_fin_semana)
            CALL f_genera_rechazo(ldt_fecha_ini_semana, ldt_fecha_fin_semana)

            LET li_indice = li_indice + 7

            IF (ldt_fecha_fin_semana = pdt_fecha_fin_proceso ) THEN
                EXIT WHILE
            END IF
        END WHILE
    END IF

    FINISH REPORT rpt_cifras_control_anexo114

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

    LET gc_ruta_encabezado = gr_seg_modulo.ruta_envio CLIPPED, "/", gc_usuario CLIPPED, ".CZA_ANEXO114"

    START REPORT rpt_cza_anexo_114 TO gc_ruta_encabezado

        LET gi_num_registros = gi_num_registros + 1
        OUTPUT TO REPORT rpt_cza_anexo_114()

    FINISH REPORT rpt_cza_anexo_114

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
    LET lc_comando_cat = "cp ", lc_ruta_tmp CLIPPED, " ", gr_seg_modulo.ruta_envio CLIPPED,"/", gc_nombre_anexo_114
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
#                                 gc_ruta_det_parcial, gc_ruta_det_rechazo      #
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
    LET lc_comando_chmod = "chmod 777 ", gr_seg_modulo.ruta_envio CLIPPED,"/", gc_nombre_anexo_114
    LET lc_comando_chmod = lc_comando_chmod CLIPPED
    RUN lc_comando_chmod

    WHENEVER ERROR CONTINUE
       -- Elimina los archivos temporales que conformaban el reporte
        LET lc_comando_rm = "rm ", gc_ruta_encabezado CLIPPED, " ",
                                   gc_ruta_subencabezado CLIPPED, " ",
                                   gc_ruta_det_transferencia CLIPPED, " ",
                                   gc_ruta_det_disposiciones CLIPPED , " ",
                                   gc_ruta_det_parcial CLIPPED , " ",
                                   gc_ruta_det_rechazo CLIPPED , " ",
                                   gc_ruta_cons_detalle CLIPPED

        LET lc_comando_rm = lc_comando_rm CLIPPED
        RUN lc_comando_rm

    WHENEVER ERROR STOP

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_subencabezado : Genera la seccion del subencabezado en el        #
#                          archivo plano del anexo 114                      #
#---------------------------------------------------------------------------#
FUNCTION f_genera_subencabezado(pr_fechas)

    DEFINE pr_fechas RECORD
        inicio              DATE ,
        fin                 DATE
    END RECORD

    DEFINE
        lc_comando        CHAR(400)

    -- -----------------------------------------------------------------------------

    LET gc_ruta_subencabezado = gr_seg_modulo.ruta_envio CLIPPED, "/", gc_usuario CLIPPED, ".SUB_CZA_ANEXO114"

    -- Crea el subencabezado
    START REPORT rpt_subcza_anexo_114 TO gc_ruta_subencabezado

        OUTPUT TO REPORT rpt_subcza_anexo_114(pr_fechas.*)
        LET gi_num_registros = gi_num_registros + 1
        CALL f_imprime_cifras_nulo(1) -- Se imprime un subencabezado

    FINISH REPORT rpt_subcza_anexo_114

    -- Cambia permisos en el archivo
    LET lc_comando = "chmod 777 ", gc_ruta_subencabezado CLIPPED
    LET lc_comando = lc_comando CLIPPED
    RUN lc_comando

    -- Consolida subencabezado
    CALL f_consolida_subencabezado()

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_transferencias : Genera la seccion de transferencias IMSS en el  #
#                          archivo plano del anexo 114                      #
#---------------------------------------------------------------------------#
FUNCTION f_genera_transferencias(pr_fechas)

    DEFINE pr_fechas RECORD
        inicio              DATE ,
        fin                 DATE
    END RECORD

    DEFINE lr_montos RECORD
        tot_retiro_97        ,
        tot_cesantia_vejez   ,
        tot_cuota_social     ,
        tot_vivienda_97      ,
        tot_sar_92           DECIMAL(22,2)
    END RECORD

    DEFINE lr_tipo_catalogo RECORD
        tipo_retiro             CHAR(3),
        tipo_prestacion         CHAR(2)
    END RECORD


    DEFINE
        ldt_fecha_aux               ,
        ldt_fecha_liquida           DATE

    DEFINE
        lc_comando                  CHAR(400) ,
        lc_tipo_retiro              CHAR(001)

    DEFINE
        li_contador_dia             ,
        li_num_registros            ,
        li_movimiento               ,
        li_siefore                  ,
        li_tipo_prestacion          ,
        li_num_tranfer              ,
        li_num_disposicion          ,
        li_num_parciales            ,
        li_num_subcuentas           INTEGER

    -- -----------------------------------------------------------------------------
    
    DISPLAY " TRANSFERENCIAS IMSS ...                                   " AT 19,1 ATTRIBUTE(REVERSE)
    SLEEP 1

    -- Inicialización de variables
    LET ldt_fecha_aux = pr_fechas.inicio

    -- Hace la búsqueda por cada día del periodo semanal
    FOR li_contador_dia = 1 TO 7

        INITIALIZE ldt_fecha_liquida TO NULL

        SELECT fecha_conversion
        INTO   ldt_fecha_liquida
        FROM   tmp_anexo_114
        WHERE  fecha_conversion =  ldt_fecha_aux
        GROUP BY 1

        LET li_num_tranfer     = 0
        LET li_num_disposicion = 0
        LET li_num_parciales   = 0
        LET li_num_subcuentas  = 0

        IF (ldt_fecha_liquida IS NOT NULL) THEN

            START REPORT rpt_transferencias_anexo114 TO gc_ruta_det_transferencia

            INITIALIZE li_movimiento, li_siefore, lc_tipo_retiro, li_tipo_prestacion TO NULL
            LET li_num_registros = 0
            LET li_num_tranfer   = 0

            DECLARE curs_mov_transferencias CURSOR FOR
                SELECT DISTINCT d.tipo_movimiento
                  FROM tmp_anexo_114    d ,
                       ret_trans_imss   t ,
                       OUTER tab_retiro r                                     -- CPL-2902                                       
                 WHERE t.nss                 = d.nss
                   AND t.tipo_retiro         = r.tipo_retiro
                   AND d.tipo_movimiento     IN (800, 810, 815, 806, 805)
                   AND r.tipo_retiro         IN ("A", "B", "C", "U")
                   AND d.fecha_conversion    =  ldt_fecha_liquida
                 ORDER BY d.tipo_movimiento

            FOREACH curs_mov_transferencias INTO li_movimiento

                IF (li_movimiento IS NOT NULL ) THEN

                    IF (li_siefore IS NULL OR li_siefore = 0) THEN
                        LET li_siefore = 0
                    END IF

                    -- Regresa Importes con formato cadena, sin puntos decimales
                    CALL f_regresa_importes_tranferencias(li_movimiento, ldt_fecha_liquida)
                         RETURNING lr_montos.*, li_num_registros

                    -- Para determinar si se concatena el archivo
                    LET li_num_tranfer = li_num_tranfer + li_num_registros

                    IF (li_num_registros <>  0) THEN
                        CALL f_regresa_dat_cat_114(li_movimiento)
                            RETURNING lr_tipo_catalogo.*

                        -- Se asigna la siefore 10 debido a que es transferencia por Vent. 2.5
                        LET li_siefore          = 10
                        LET gi_num_registros    = gi_num_registros + 1

                        -- Envía la información para generar el detalle de Transferencias IMSS
                        OUTPUT TO REPORT rpt_transferencias_anexo114(ldt_fecha_liquida     ,
                                                                     li_siefore            ,
                                                                     lr_montos.*           ,
                                                                     li_num_registros      ,
                                                                     lr_tipo_catalogo.*
                                                                    )

                        -- Envía la información al reporte de cifras control
                        OUTPUT TO REPORT rpt_cifras_control_anexo114(301                    ,  -- Tipo de detalle
                                                                     ldt_fecha_liquida      ,  -- Fecha de liquidación
                                                                     lr_tipo_catalogo.*     ,
                                                                     li_siefore             ,  -- Siefore
                                                                     lr_montos.tot_retiro_97,
                                                                     lr_montos.tot_cesantia_vejez,
                                                                     lr_montos.tot_cuota_social,
                                                                     lr_montos.tot_vivienda_97,
                                                                     0                      ,  -- Retiro 92
                                                                     0                      ,  -- Vivienda 92
                                                                     0                      ,  -- RCV
                                                                     li_num_registros       ,  -- Número de registros
                                                                     0                      ,  -- ID encabezado
                                                                     0                         -- ID subencabezado
                                                                    )

                    END IF   --IF (li_num_registros <>  0)
                END IF    --IF li_movimiento IS NOT NULL
            END FOREACH

            FINISH REPORT rpt_transferencias_anexo114

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
                                       li_num_parciales    )
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
# f_genera_disposiciones : Genera la seccion de disposiciones IMSS en el    #
#                          archivo plano del anexo 114                      #
#---------------------------------------------------------------------------#
FUNCTION f_genera_disposiciones(pr_fechas)

    DEFINE pr_fechas RECORD
        inicio              DATE ,
        fin                 DATE
    END RECORD

    DEFINE lr_montos RECORD
        tot_retiro_97           ,
        tot_cesantia_vejez      ,
        tot_cuota_social        ,
        tot_vivienda_97         ,
        tot_retiro_92           ,
        tot_vivienda_92         DECIMAL(22,2)
    END RECORD

    DEFINE lr_tipo_catalogo RECORD
        tipo_retiro             CHAR(3),
        tipo_prestacion         CHAR(2)
    END RECORD

    DEFINE
        ldt_fecha_aux               ,
        ldt_fecha_liquida           DATE

    DEFINE
        lc_query_disp               CHAR(3000)  ,
        lc_comando                  CHAR(400)   ,
        lc_tipo_retiro              CHAR(001)

    DEFINE
        li_contador_dia             ,
        li_num_registros            ,
        li_movimiento               ,
        li_siefore                  ,
        li_tipo_prestacion          ,
        li_num_tranfer              ,
        li_num_disposicion          ,
        li_num_parciales            ,
        li_num_subcuentas           INTEGER

    DEFINE
        ls_existe_viv               ,
        ls_num_siefores             SMALLINT

    -- -----------------------------------------------------------------------------

    DISPLAY " DISPOSICIONES Y PMG IMSS ...                              " AT 19,1 ATTRIBUTE(REVERSE)
    SLEEP 1

    -- Inicialización de variables
    INITIALIZE ldt_fecha_liquida TO NULL
    LET ldt_fecha_aux = pr_fechas.inicio

    -- Hace la búsqueda por cada día del periodo semanal
    FOR li_contador_dia = 1 TO 7

        INITIALIZE ldt_fecha_liquida TO NULL

        SELECT fecha_conversion
        INTO   ldt_fecha_liquida
        FROM   tmp_anexo_114
        WHERE  fecha_conversion =  ldt_fecha_aux
        GROUP BY 1

        LET ls_existe_viv       = 0
        LET li_num_tranfer      = 0
        LET li_num_disposicion  = 0
        LET li_num_parciales    = 0
        LET li_num_subcuentas   = 0     
        

        IF (ldt_fecha_liquida IS NOT NULL) THEN

            START REPORT rpt_disposicion_anexo114 TO gc_ruta_det_disposiciones

            INITIALIZE li_movimiento, li_siefore, lc_tipo_retiro, li_tipo_prestacion TO NULL
            LET li_num_registros    = 0
            LET li_num_disposicion  = 0

            LET lc_query_disp =
                " SELECT DISTINCT d.tipo_movimiento, s.tipo_retiro, d.siefore  \n",
                "   FROM tmp_anexo_114 d, ret_solicitud_tx s \n",
                "  WHERE s.nss             = d.nss \n",
                "    AND s.consecutivo     = d.consecutivo_lote \n",
                "    AND d.tipo_movimiento IN (820, 830, 835, 840, 850, 860, 880, 825, 842) \n",
                "    AND s.tipo_retiro IN ('D', 'E', 'F', 'G', 'H', 'J', 'M', 'P') \n",
                "    AND d.fecha_conversion = '", ldt_fecha_liquida,"' \n",
                "    UNION \n",
                " SELECT DISTINCT d.tipo_movimiento, s.tipo_retiro, d.siefore \n",
                "   FROM tmp_anexo_114 d,  pen_solicitud_pmg s \n",
                "  WHERE s.nss             = d.nss \n",
                "    AND s.consecutivo     = d.consecutivo_lote \n",
                "    AND d.tipo_movimiento = 841 \n",
                "    AND d.fecha_conversion = '",ldt_fecha_liquida,"' \n",
                "    UNION \n",
                " SELECT DISTINCT d.tipo_movimiento, s.tipo_retiro, d.siefore \n",
                "   FROM tmp_anexo_114 d,  ret_notifica_vivienda s \n",
                "  WHERE s.nss             = d.nss \n",
                "    AND s.consecutivo     = d.consecutivo_lote \n",
                "    AND d.tipo_movimiento = 830 \n",
                "    AND d.fecha_conversion = '",ldt_fecha_liquida,"' \n",
                "    UNION \n",
                " SELECT DISTINCT d.tipo_movimiento, 'E', d.siefore \n",
                "   FROM tmp_anexo_114 d,  ret_notifica_op76 s \n",
                "  WHERE s.nss             = d.nss \n",
                "    AND s.consecutivo     = d.consecutivo_lote \n",
                "    AND d.tipo_movimiento = 830 \n",
                "    AND d.fecha_conversion = '",ldt_fecha_liquida,"' \n",
                "    ORDER BY s.tipo_retiro, d.siefore \n"

            LET lc_query_disp = lc_query_disp CLIPPED

            PREPARE pre_mov_disposicion FROM lc_query_disp
            DECLARE curs_mov_disposicion CURSOR FOR pre_mov_disposicion

            FOREACH curs_mov_disposicion INTO li_movimiento     ,
                                              lc_tipo_retiro    ,
                                              li_siefore

                LET ls_num_siefores = 0

                IF (li_movimiento IS NOT NULL) THEN

                    IF (li_siefore IS NULL OR li_siefore = 0) THEN
                        LET li_siefore = 0
                    ELSE
                        -- Determina cuantas siefores tiene el tipo de movimiento
                        IF li_siefore <> 11 THEN
                            SELECT COUNT(DISTINCT d.siefore)
                            INTO   ls_num_siefores
                            FROM   tmp_anexo_114 d
                            WHERE  d.tipo_movimiento  = li_movimiento
                            AND    d.fecha_conversion = ldt_fecha_liquida
                        ELSE
                            SELECT NVL(COUNT(*), 0)
                            INTO   ls_existe_viv
                            FROM   tmp_anexo_114
                            WHERE  fecha_conversion = ldt_fecha_liquida
                            AND    siefore          = 11
                            AND nss NOT IN (SELECT UNIQUE nss
                                            FROM   tmp_anexo_114
                                            WHERE  fecha_conversion  = ldt_fecha_liquida
                                            AND    siefore <> 11
                                           )

                            IF ls_existe_viv > 0 THEN
                                LET ls_num_siefores = 1
                            END IF
                        END IF
                    END IF

                    -- El reporte se ejecutara solo en los siguientes casos:
                    -- Tenga una sola siefore y esta sea la 11
                    -- Tenga varias siefores

                    IF (ls_num_siefores = 1 AND li_siefore = 11) OR
                       (ls_num_siefores > 0 AND li_siefore <> 11) THEN

                        CALL f_regresa_importes_disposicion(li_movimiento, li_siefore, ldt_fecha_liquida )
                            RETURNING lr_montos.*, li_num_registros

                        -- Determina si se concatena el archivo
                        LET li_num_disposicion = li_num_disposicion + li_num_registros

                        IF (li_num_registros <>  0) THEN
                            CALL f_regresa_dat_cat_114(li_movimiento)
                                RETURNING lr_tipo_catalogo.*

                            LET gi_num_registros = gi_num_registros + 1

                           -- Envía la información para generar el detalle de Disposicion
                           OUTPUT TO REPORT rpt_disposicion_anexo114(ldt_fecha_liquida     ,
                                                               li_siefore            ,
                                                               lr_montos.*           ,
                                                               li_num_registros      ,
                                                               lr_tipo_catalogo.*
                                                              )

                            -- Envía la información al reporte de cifras control
                            OUTPUT TO REPORT rpt_cifras_control_anexo114(302                    ,  -- Tipo de detalle
                                                                         ldt_fecha_liquida      ,  -- Fecha de liquidación
                                                                         lr_tipo_catalogo.*     ,
                                                                         li_siefore             ,  -- Siefore
                                                                         lr_montos.*            ,
                                                                         0                      ,  -- RCV
                                                                         li_num_registros       ,  -- Número de registros
                                                                         0                      ,  -- ID encabezado
                                                                         0                         -- ID subencabezado
                                                                        )
                        END IF -- Validacion de siefores
                    END IF   --IF (li_num_registros <>  0)
                END IF    --IF li_movimiento IS NOT NULL
            END FOREACH     --foreach del fecha_liquida

            FINISH REPORT rpt_disposicion_anexo114

            -- Cambia los permisos del archivo generado
            LET lc_comando  = "chmod 777 ", gc_ruta_det_transferencia CLIPPED
            LET lc_comando  = lc_comando CLIPPED
            RUN lc_comando

        END IF  -- ldt_fecha_liquida No nula

        -- Consolida los detalles
        IF (li_num_tranfer     <> 0 OR
            li_num_disposicion <> 0 OR
            li_num_parciales   <> 0 ) THEN

            CALL f_consolida_detalles(li_num_tranfer     ,
                                       li_num_disposicion,
                                       li_num_parciales  )
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
# f_genera_parciales : Genera la seccion de retiros parciales IMSS en el    #
#                      archivo plano del anexo 114                          #
#---------------------------------------------------------------------------#
FUNCTION f_genera_parciales(pr_fechas)

    DEFINE pr_fechas RECORD
        inicio              DATE ,
        fin                 DATE
    END RECORD

    DEFINE lr_tipo_catalogo RECORD
        tipo_retiro             CHAR(3),
        tipo_prestacion         CHAR(2)
    END RECORD

    DEFINE lr_montos RECORD
        tot_retiro_97           ,
        tot_cesantia_vejez      ,
        tot_cuota_social        ,
        tot_vivienda_97         ,
        tot_retiro_92           ,
        tot_vivienda_92         DECIMAL(22,2)
    END RECORD

    DEFINE
        ldt_fecha_aux               ,
        ldt_fecha_liquida           DATE

    DEFINE
        lc_query_disp               CHAR(3000)  ,
        lc_comando                  CHAR(400)   ,
        lc_tipo_retiro              CHAR(001)   ,
        lc_retiro_B                 CHAR(002)

    DEFINE
        ld_pago_rcv                  DECIMAL(22,2)

    DEFINE
        li_contador_dia             ,
        li_num_registros            ,
        li_movimiento               ,
        li_siefore                  ,
        li_tipo_prestacion          ,
        li_num_tranfer              ,
        li_num_disposicion          ,
        li_num_parciales            ,
        li_num_subcuentas           INTEGER

    DEFINE
        ls_proceso_retiro           ,
        ls_clasificacion_pago       ,
        ls_num_siefores             SMALLINT

    DEFINE lc_medio_solic           CHAR(003)
    DEFINE lc_forma_pago            CHAR(003)

    -- -----------------------------------------------------------------------------

    DISPLAY " RETIROS PARCIALES IMSS ...                                " AT 19,1 ATTRIBUTE(REVERSE)
    SLEEP 1

    -- Inicialización de variables
    LET ldt_fecha_aux                   = pr_fechas.inicio
    LET lr_montos.tot_retiro_97         = 0
    LET lr_montos.tot_cesantia_vejez    = 0
    LET lr_montos.tot_cuota_social      = 0
    LET lr_montos.tot_vivienda_97       = 0
    LET lr_montos.tot_retiro_92         = 0
    LET lr_montos.tot_vivienda_92       = 0

    -- Hace la búsqueda por cada día del periodo semanal
    FOR li_contador_dia = 1 TO 7

        INITIALIZE ldt_fecha_liquida TO NULL

        SELECT fecha_conversion
        INTO   ldt_fecha_liquida
        FROM   tmp_anexo_114
        WHERE  fecha_conversion =  ldt_fecha_aux
        GROUP BY 1

        LET li_num_tranfer     = 0
        LET li_num_disposicion = 0
        LET li_num_parciales   = 0

        IF (ldt_fecha_liquida IS NOT NULL) THEN

            START REPORT rpt_parciales_anexo114 TO gc_ruta_det_parcial

            INITIALIZE li_movimiento, li_siefore, lc_tipo_retiro, li_tipo_prestacion TO NULL
            LET li_num_registros    = 0
            LET li_num_parciales    = 0
            
            LET lc_medio_solic = "   "
            LET lc_forma_pago  = "   "

            DECLARE curs_mov_parcial CURSOR FOR
                SELECT DISTINCT d.tipo_movimiento   ,
                       des_tipo_B                   ,
                       medio_solic                  ,
                       forma_pago
                FROM   tmp_anexo_114 d  ,
                       tab_retiro r     ,
                       ret_parcial p
                WHERE  p.tipo_retiro        = r.tipo_retiro
                AND    p.nss                = d.nss
                AND    d.tipo_movimiento    IN (870, 875, 876, 877, 878)
                AND    p.estado_solicitud   IN (8,13,14)
                AND    d.consecutivo_lote   = p.consecutivo
                AND    d.fecha_conversion   = ldt_fecha_liquida
                ORDER BY d.tipo_movimiento, medio_solic, forma_pago

            FOREACH curs_mov_parcial INTO li_movimiento, lc_retiro_B,
                                          lc_medio_solic, lc_forma_pago

                IF (li_movimiento IS NOT NULL) THEN

                    CALL f_regresa_importes_parcial(li_movimiento       ,
                                                    ldt_fecha_liquida   ,
                                                    lc_retiro_B         ,
                                                    lc_medio_solic      ,
                                                    lc_forma_pago
                                                   )
                        RETURNING ld_pago_rcv, li_num_registros

                    -- Para determinar si se concatena el archivo
                    LET li_num_parciales = li_num_parciales + li_num_registros

                    IF (li_num_registros <>  0) THEN
                        CALL f_regresa_dat_cat_114(li_movimiento)
                             RETURNING lr_tipo_catalogo.*

                        CASE (li_movimiento)
                           WHEN 876
                              LET ls_proceso_retiro     = 2
                              LET ls_clasificacion_pago = 1

                           WHEN 877
                              LET ls_proceso_retiro     = 2

                              IF (lc_retiro_B = "B1") THEN
                                  LET ls_clasificacion_pago = 2
                              ELSE
                                  LET ls_clasificacion_pago = 3
                              END IF

                           WHEN 878
                              LET ls_proceso_retiro     = 1
                              LET ls_clasificacion_pago = 0

                           WHEN 875
                              LET ls_proceso_retiro     = 3
                              LET ls_clasificacion_pago = 4

                           WHEN 870
                              -- Matrimonio
                              LET ls_proceso_retiro     = 0
                              LET ls_clasificacion_pago = 0

                        END CASE

                        LET gi_num_registros = gi_num_registros + 1

                        -- Envía la información para generar el detalle de Retiros parciales
                        OUTPUT TO REPORT rpt_parciales_anexo114(ldt_fecha_liquida       ,
                                                                ld_pago_rcv             ,
                                                                li_num_registros        ,
                                                                lr_tipo_catalogo.*      ,
                                                                ls_proceso_retiro       ,
                                                                ls_clasificacion_pago   ,
                                                                lc_medio_solic      ,
                                                                lc_forma_pago
                                                               )

                        -- Envía la información al reporte de cifras control
                        OUTPUT TO REPORT rpt_cifras_control_anexo114(303                    ,  -- Tipo de detalle
                                                                     ldt_fecha_liquida      ,  -- Fecha de liquidación
                                                                     lr_tipo_catalogo.*     ,
                                                                     li_siefore             ,  -- Siefore
                                                                     lr_montos.*            ,
                                                                     ld_pago_rcv            ,  -- RCV
                                                                     li_num_registros       ,  -- Número de registros
                                                                     0                      ,  -- ID encabezado
                                                                     0                         -- ID subencabezado
                                                                    )
                    END IF   --IF (li_num_registros <>  0)
                END IF    --IF li_movimiento IS NOT NULL
                
                LET lc_medio_solic = "   "
                LET lc_forma_pago  = "   "
            END FOREACH     --foreach del fecha_liquida

            FINISH REPORT rpt_parciales_anexo114

            -- Cambia los permisos del archivo generado
            LET lc_comando  = "chmod 777 ", gc_ruta_det_parcial CLIPPED
            LET lc_comando  = lc_comando CLIPPED
            RUN lc_comando

        END IF -- ldt_fecha_liquida No nula

        -- Consolida los detalles
        IF (li_num_tranfer     <> 0 OR
            li_num_disposicion <> 0 OR
            li_num_parciales   <> 0 ) THEN

            CALL f_consolida_detalles(li_num_tranfer     ,
                                       li_num_disposicion ,
                                       li_num_parciales    )
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
# f_genera_rechazo : Genera la seccion de retiros con liquidacion en la     #
#                    subcuenta 7 para el archivo plano del anexo 114        #
#---------------------------------------------------------------------------#
FUNCTION f_genera_rechazo(pr_fechas)

    DEFINE pr_fechas RECORD
        inicio              DATE ,
        fin                 DATE
    END RECORD

    DEFINE lr_montos RECORD
        tot_retiro_97           ,
        tot_cesantia_vejez      ,
        tot_cuota_social        ,
        tot_vivienda_97         ,
        tot_retiro_92           ,
        tot_vivienda_92         DECIMAL(22,2)
    END RECORD

    DEFINE lr_tipo_catalogo RECORD
        tipo_retiro             CHAR(3),
        tipo_prestacion         CHAR(2)
    END RECORD

    DEFINE
        ldt_fecha_aux               ,
        ldt_fecha_liquida           DATE

    DEFINE
        lc_query_disp               CHAR(3000)  ,
        lc_tipo_retiro              CHAR(001)
        
    DEFINE 
        lc_comando                  CHAR(2000)     

    DEFINE
        ld_pago_rcv                 DECIMAL(22,2)

    DEFINE
        li_contador_dia             ,
        li_num_registros            ,
        li_movimiento               ,
        li_siefore                  ,
        li_tipo_prestacion          ,
        li_num_tranfer              ,
        li_num_disposicion          ,
        li_num_parciales            ,
        li_num_subcuentas           INTEGER

    DEFINE
        ls_proceso_retiro           ,
        ls_clasificacion_pago       ,
        ls_num_siefores             SMALLINT
        
    DEFINE 
        ln_fecha_solicitud          DATE, 
        ln_tipo_prestacion          SMALLINT,
        ln_tipo_desempleo           CHAR(1),
        ln_contador                 SMALLINT,
        ln_diagnostico              CHAR(03),
        
        lc_tipo_prestacion          CHAR(02),
        lc_fecha_solicitud          CHAR(08),
        lc_tipo_desempleo           CHAR(01),
        lc_contador                 CHAR(12),
        lc_diagnostico              CHAR(04)
    
    DEFINE lc_medio_solic           CHAR(03)

    -- -----------------------------------------------------------------------------

    DISPLAY " CUENTAS CON RETIRO  ...                          " AT 19,1 ATTRIBUTE(REVERSE)
    SLEEP 1

    
    -- Inicialización de Variables
    INITIALIZE li_movimiento TO NULL
    LET li_num_registros                = 0
    LET li_num_subcuentas               = 0
    LET lr_montos.tot_retiro_97         = 0
    LET lr_montos.tot_cesantia_vejez    = 0
    LET lr_montos.tot_cuota_social      = 0
    LET lr_montos.tot_vivienda_97       = 0
    LET lr_montos.tot_retiro_92         = 0
    LET lr_montos.tot_vivienda_92       = 0
    
    CALL f_carga_info_parciales_rech(pr_fechas.*)

    START REPORT rpt_rechazo_anexo114 TO gc_ruta_det_rechazo

    {LET lc_comando = '',
                     '\n SELECT a.fecha_captura, a.tipo_prestacion, a.tipo_desempleo, ',
                     '\n        a.diag_cuenta_ind, COUNT(*)                           ',
                     '\n   FROM ret_parcial  a,                                       ',
                     '\n        tab_diag_procesar_disp b                              ',
                     '\n  WHERE a.diag_cuenta_ind  = b.diag_procesar                  ',
                     '\n    AND a.fecha_captura   >= "',pr_fechas.inicio,'"           ',
                     '\n    AND a.fecha_captura   <= "',pr_fechas.fin,'"              ',
                     '\n    AND a.diag_cuenta_ind IN ( SELECT diag_procesar           ',
                     '\n                                 FROM tab_diag_procesar_disp  ',
                     '\n                                WHERE id_aceptado = 0)        ',
                     '\n  GROUP BY 1, 2, 3, 4                                         ',
                     '\n  ORDER BY 1                                                  '}
 #
 # CPL-2902 FIN 

    LET lc_comando = '',
                     '\n SELECT fecha_captura, tipo_prestacion, tipo_desempleo, ',
                     '\n        diag_cuenta_ind, medio_solic, COUNT(*)          ',
                     '\n   FROM tmp_parciales_rechazados_114                    ',
                     '\n  GROUP BY 1, 2, 3, 4, 5                                ',
                     '\n  ORDER BY 1                                            '

              
    PREPARE con_rech FROM lc_comando                      
    DECLARE nuevo_cur CURSOR WITH HOLD FOR con_rech                 
        
    FOREACH nuevo_cur INTO ln_fecha_solicitud,
                           ln_tipo_prestacion,
                           ln_tipo_desempleo ,
                           ln_diagnostico    ,
                           lc_medio_solic    ,
                           ln_contador
                
     LET lc_tipo_prestacion = ln_tipo_prestacion USING "&&"
     LET lc_fecha_solicitud = ln_fecha_solicitud USING "YYYYMMDD"
     LET lc_contador        = ln_contador        USING "&&&&&&&&&&&&"
     LET lc_diagnostico     = rellenar_diagnostico(ln_diagnostico)

 
    CASE 
       WHEN ln_tipo_desempleo = "A" 
            LET lc_tipo_desempleo = "1"
       WHEN ln_tipo_desempleo = "B" 
            LET lc_tipo_desempleo = "2"
       WHEN ln_tipo_desempleo = "C" 
            LET lc_tipo_desempleo = "3"
       WHEN ln_tipo_desempleo = "D" 
            LET lc_tipo_desempleo = "4"
       OTHERWISE 
            LET lc_tipo_desempleo = "0"                     
     END CASE                      

     LET li_num_registros = li_num_registros + 1
     LET gi_num_registros = gi_num_registros + 1    -- CPL-2902
    
     OUTPUT TO REPORT rpt_rechazo_anexo114(                
                                           lc_fecha_solicitud, 
                                           lc_tipo_prestacion,
                                           lc_tipo_desempleo ,
                                           lc_diagnostico    ,
                                           lc_contador       ,
                                           lc_medio_solic
                                          )
                                                   
                                                   

     -- Envía la información al reporte de cifras control
     OUTPUT TO REPORT rpt_cifras_control_anexo114(304                    ,  -- Tipo de detalle
                                                  pr_fechas.fin          ,  -- Fecha de liquidación
                                                  "009"                  ,  -- pc_tipo_retiro
                                                  ln_tipo_prestacion     ,  -- pc_tipo_prestacion
                                                  0                      ,  -- Siefore
                                                  0                      ,  -- imp_retiro_97
                                                  0                      ,  -- imp_cesantia_vejez
                                                  0                      ,  -- imp_cuota_social
                                                  0                      ,  -- imp_vivienda_97
                                                  0                      ,  -- imp_retiro_92
                                                  0                      ,  -- imp_vivienda_92
                                                  0                      ,  -- RCV
                                                  lc_contador            ,  -- Número de registros
                                                  0                      ,  -- ID encabezado
                                                  0                      )  -- ID subencabezado
                                                            
   END FOREACH
   FREE nuevo_cur
 
   FINISH REPORT rpt_rechazo_anexo114

   -- Si existen registros, entoces los concatena al archivo llamado "CONSOLIDA_DET"
   IF (li_num_registros <>  0) THEN
       LET lc_comando = "chmod 777 ", gc_ruta_det_rechazo CLIPPED
       LET lc_comando = lc_comando CLIPPED
       RUN lc_comando
       CALL f_consolida_rechazo()
    END IF

END FUNCTION

#################################################################################
#Función                        : f_carga_info_parciales_rech()                 #
#Objetivo                       : Función que realiza la busqueda de la         #
#                                 información de retiros parciales rechazados   #
#                                 y las guarda en la tabla temporal             #
#                                 llamada "tmp_parciales_rechazados_114".       #
#Parámetros de entrada          : Ninguno                                       #
#Valores de retorno             : Ninguno                                       #
#Variables globales importantes : Ninguna                                       #
#Pantallas utilizadas           : Ninguna                                       #
#Autor                          : David Miguel Garibay Rivera                   #
#Fecha                          : 27/11/2012                                    #
#################################################################################
FUNCTION f_carga_info_parciales_rech(pr_fechas)

    DEFINE pr_fechas RECORD
        fecha_ini  DATE,
        fecha_fin  DATE
    END RECORD

    -- -----------------------------------------------------------------------------

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_parciales_rechazados_114
    WHENEVER ERROR STOP

    SELECT a.*                   ,
           "XXX"  medio_solic                          
      FROM ret_parcial  a,                                      
           tab_diag_procesar_disp b                             
     WHERE a.diag_cuenta_ind  = b.diag_procesar                 
       AND a.fecha_captura   >= pr_fechas.fecha_ini         
       AND a.fecha_captura   <= pr_fechas.fecha_fin           
       AND a.diag_cuenta_ind IN ( SELECT diag_procesar          
                                    FROM tab_diag_procesar_disp 
                                   WHERE id_aceptado = 0)       
    INTO TEMP tmp_parciales_rechazados_114
    
    UPDATE STATISTICS FOR TABLE tmp_parciales_rechazados_114
    
    -- Se determina el medio de solicitud
    CALL f_modifica_medio_solic()


END FUNCTION


FUNCTION f_modifica_medio_solic()

    DEFINE lr_solicitud RECORD
        nss             LIKE ret_solicitud_tx.nss           ,
        consecutivo     LIKE ret_solicitud_tx.consecutivo
    END RECORD

    DEFINE lc_medio_solic     CHAR(003)

    -- -----------------------------------------------------------------------------

    INITIALIZE lr_solicitud.* TO NULL

    LET lc_medio_solic = "   "

    DECLARE cur_medio_solic_2 CURSOR FOR
        SELECT UNIQUE(nss)      ,
               consecutivo
        FROM   tmp_parciales_rechazados_114

    FOREACH cur_medio_solic_2 INTO lr_solicitud.*

        SELECT "OK"
        FROM   ret_ws_notifica_app
        WHERE  nss          = lr_solicitud.nss
        AND    consecutivo  = lr_solicitud.consecutivo
        AND    aplicacion_origen <> '07'
        GROUP BY 1
        
        IF SQLCA.SQLCODE = 0 THEN  --existe en tabla control de app
             LET lc_medio_solic = '002'
        ELSE
        	   SELECT "OK"
             FROM   ret_ws_notifica_app
             WHERE  nss          = lr_solicitud.nss
             AND    consecutivo  = lr_solicitud.consecutivo
             AND    aplicacion_origen = '07'
             GROUP BY 1

             IF SQLCA.SQLCODE = 0 THEN  --existe en tabla control de afore web
                  LET lc_medio_solic = '003'
             ELSE
             	    LET lc_medio_solic = '001'
             END IF
        END IF

        UPDATE tmp_parciales_rechazados_114
        SET    medio_solic     = lc_medio_solic
        WHERE  nss             = lr_solicitud.nss
        AND    consecutivo     = lr_solicitud.consecutivo

        INITIALIZE lr_solicitud.* TO NULL

        LET lc_medio_solic = "   "

    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# f_regresa_importes_tranferencias : Recupera los montos pagados de trans   #
#       IMSS para las fechas indicadas                                      #
#---------------------------------------------------------------------------#
FUNCTION f_regresa_importes_tranferencias(pi_movimiento, pdt_fecha_liquida )

    DEFINE
       pi_movimiento                INTEGER

    DEFINE                      
        pdt_fecha_liquida           DATE

    DEFINE 
        ls_subcuenta                SMALLINT

    DEFINE
       li_num_total_registros       ,
       li_num_registros             INTEGER

    DEFINE
       lc_qry_importe               CHAR(1500)

    DEFINE
       ld_importe                   ,
       ld_tot_retiro_97             ,
       ld_tot_cesantia_vejez        ,
       ld_tot_cuota_social          ,
       ld_tot_vivienda_97           ,
       ld_tot_sar_92                DECIMAL(22,2)
       
    -- -----------------------------------------------------------------------------

    -- Inicialización de Variables
    LET li_num_total_registros = 0
    LET li_num_registros       = 0
    LET ld_importe             = 0
    LET ld_tot_retiro_97       = 0
    LET ld_tot_cesantia_vejez  = 0
    LET ld_tot_cuota_social    = 0
    LET ld_tot_vivienda_97     = 0
    LET ld_tot_sar_92          = 0
    
    LET lc_qry_importe = " ",
                         "\n SELECT d.subcuenta   ,                                       ",
                         "\n        NVL(SUM(ROUND(d.monto_en_pesos, 2) ) * -1 , 0)        ",
                         "\n   FROM tmp_anexo_114 d, ret_trans_imss t, OUTER tab_retiro r ",    -- CPL-2902
                         "\n  WHERE d.tipo_movimiento   = ?                               ",
                         "\n    AND d.fecha_conversion  = ?                               ",
                         "\n    AND r.movimiento        = d.tipo_movimiento               ",
                         "\n    AND t.consecutivo       = d.consecutivo_lote              ",
                         "\n    AND t.nss               = d.nss                           ",
                         "\n  GROUP BY 1                                                  "

    PREPARE pre_importe_transferencias FROM lc_qry_importe
    
    DECLARE curs_importe_transferencias CURSOR FOR pre_importe_transferencias
    FOREACH curs_importe_transferencias USING pi_movimiento     ,
                                              pdt_fecha_liquida
                                        INTO ls_subcuenta   ,
                                             ld_importe
        CASE (ls_subcuenta)
            WHEN 1
                -- Retiro 97
                LET ld_tot_retiro_97 = ld_tot_retiro_97 + ld_importe
            WHEN 2
                -- Cesantía y Vejez
                LET ld_tot_cesantia_vejez = ld_tot_cesantia_vejez + ld_importe
            WHEN 6
                -- Cesantía y Vejez
                LET ld_tot_cesantia_vejez = ld_tot_cesantia_vejez + ld_importe
            WHEN 9
                 -- Cesantía y Vejez
                LET ld_tot_cesantia_vejez = ld_tot_cesantia_vejez + ld_importe
            WHEN 5
                -- Cuota Social
              LET ld_tot_cuota_social = ld_tot_cuota_social + ld_importe
            WHEN 4
                -- Vivienda 97
              LET ld_tot_vivienda_97 = ld_tot_vivienda_97 + ld_importe
            WHEN 7
                -- SAR 92
              IF pi_movimiento = 805 THEN
                  LET ld_tot_sar_92 = ld_tot_sar_92 + ld_importe
              END IF
  
        END CASE
    
    END FOREACH

    -- Se elimina el punto decimal
    LET ld_tot_retiro_97        = ld_tot_retiro_97      * 100
    LET ld_tot_cesantia_vejez   = ld_tot_cesantia_vejez * 100
    LET ld_tot_cuota_social     = ld_tot_cuota_social   * 100
    LET ld_tot_vivienda_97      = ld_tot_vivienda_97    * 100
    LET ld_tot_sar_92           = ld_tot_sar_92         * 100    
    
   -- Realiza el conteo de los NSS con transferencias
   LET li_num_total_registros = 0
   LET lc_qry_importe = "",
                        "\n SELECT COUNT(DISTINCT d.nss)                                   ",
                        "\n   FROM tmp_anexo_114 d, ret_trans_imss t, OUTER tab_retiro r   ",    -- CPL-2902
                        "\n  WHERE d.tipo_movimiento  = ?                                  ",
                        "\n    AND d.fecha_conversion = ?                                  ",
                        "\n    AND r.movimiento       = d.tipo_movimiento                  ",
                        "\n    AND t.consecutivo      = d.consecutivo_lote                 ",
                        "\n    AND t.nss              = d.nss                              "

    PREPARE pre_num_transferencias FROM lc_qry_importe

    EXECUTE pre_num_transferencias USING pi_movimiento,
                                         pdt_fecha_liquida
                                   INTO li_num_total_registros

    RETURN ld_tot_retiro_97         ,
           ld_tot_cesantia_vejez    ,
           ld_tot_cuota_social      ,
           ld_tot_vivienda_97       ,
           ld_tot_sar_92            ,
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
#Valores de retorno             : lc_cad_retiro_97      ,                       #
#                                 lc_cad_cesantia_vejez ,                       #
#                                 lc_cad_cuota_social   ,                       #
#                                 lc_cad_retiro_92                              #
#                                 lc_cad_vivienda_92                            #
#                                 lc_cad_vivienda_97    ,                       #
#                                 li_num_total_registros                        #
#Variables globales importantes : Ninguna                                       #
#Pantallas utilizadas           : Ninguna                                       #
#Autor                          : David Miguel Garibay Rivera                   #
#Fecha                          : 27/11/2012                                    #
#################################################################################
FUNCTION f_regresa_importes_disposicion(pi_movimiento, ps_siefore, pdt_fecha_liquida)

    DEFINE
        pi_movimiento               INTEGER

    DEFINE
        ps_siefore                  SMALLINT

    DEFINE
        pdt_fecha_liquida           DATE


    DEFINE li_subcuenta LIKE dis_cuenta.subcuenta
    DEFINE lc_tipo_retiro_imss LIKE tab_retiro.tipo_retiro

    DEFINE
        ld_importe                  ,
        ld_tot_retiro_97            ,
        ld_tot_cesantia_vejez       ,
        ld_tot_cuota_social         ,
        ld_tot_vivienda_97          ,
        ld_tot_retiro_92            ,
        ld_tot_vivienda_92          ,
        ld_importe_isr              DECIMAL(22,2)

    DEFINE
        li_num_total_registros      INTEGER

    DEFINE
        lc_qry_importe              CHAR(1500)  ,
        lc_query_sie11              CHAR(1500)  ,
        lc_qry_ISR                  CHAR(0500)  ,
        lc_query_viv                CHAR(2000)  ,
        lc_tabla                    CHAR(0020)

    -- -----------------------------------------------------------------------------

    -- Inicialización de Variables
    LET li_num_total_registros = 0
    LET ld_importe             = 0
    LET ld_tot_retiro_97       = 0
    LET ld_tot_cesantia_vejez  = 0
    LET ld_tot_cuota_social    = 0
    LET ld_tot_vivienda_97     = 0
    LET ld_tot_retiro_92       = 0
    LET ld_tot_vivienda_92     = 0

    LET lc_tipo_retiro_imss = f_obten_tipo_retiro(pi_movimiento)

    IF pi_movimiento = 841 THEN
        LET lc_tabla = "pen_solicitud_pmg"
    ELSE
        LET lc_tabla = "ret_solicitud_tx"
    END IF

    -- Para calcular el ISR
    LET lc_qry_ISR = " SELECT NVL( SUM(d.monto_en_pesos) * -1 , 0) \n",
                     "   FROM tmp_anexo_114 d, ", lc_tabla CLIPPED, " t \n",
                     "  WHERE d.nss              = t.nss \n",
                     "    AND d.consecutivo_lote = t.consecutivo \n",
                     "    AND d.tipo_movimiento  = 10 \n",     --ISR
                     "    AND d.siefore          = ?  \n",
                     "    AND d.subcuenta        = ?  \n",
                     "    AND d.fecha_conversion = ?  \n",
                     "    AND t.tipo_retiro      = ?  \n"

    PREPARE pre_isr FROM lc_qry_ISR

    -- Obtiene importes y número de registros de cada subcuenta
    LET lc_qry_importe = " SELECT d.subcuenta,  NVL(SUM(d.monto_en_pesos) * -1, 0) \n",
                         "   FROM tmp_anexo_114 d \n",
                         "  WHERE d.tipo_movimiento   = ? \n",
                         "    AND d.siefore           = ? \n",
                         "    AND d.fecha_conversion  = ? \n"

    IF ps_siefore = 11 THEN
        LET lc_query_sie11 = " AND d.nss NOT IN (SELECT UNIQUE nss \n",
                             "                   FROM   tmp_anexo_114 \n",
                             "                   WHERE  fecha_conversion = '", pdt_fecha_liquida, "' \n",
                             "                   AND    siefore <> 11) \n "
    ELSE
        LET lc_query_sie11 = " "
    END IF

    LET lc_qry_importe = lc_qry_importe CLIPPED,
                         lc_query_sie11 CLIPPED,
                         "    GROUP BY 1 \n"


    PREPARE pre_importe_disposicion FROM lc_qry_importe

    DECLARE curs_importe_disposicion CURSOR FOR pre_importe_disposicion
    FOREACH curs_importe_disposicion USING pi_movimiento,
                                           ps_siefore,
                                           pdt_fecha_liquida
                                     INTO  li_subcuenta,
                                           ld_importe
        IF (li_subcuenta IS NOT NULL) THEN

            EXECUTE pre_isr USING ps_siefore            ,
                                  li_subcuenta          ,
                                  pdt_fecha_liquida     ,
                                  lc_tipo_retiro_imss
                             INTO ld_importe_isr

            CASE (li_subcuenta)
               WHEN 1
                 ###Importes de la subcuenta 'Retiro 97'
                 LET ld_tot_retiro_97 = ld_tot_retiro_97 + ld_importe
                 LET ld_tot_retiro_97 = ld_tot_retiro_97 + ld_importe_isr
               WHEN 2
                 ###Importes de la subcuenta 'Cesantía y Vejez'
                 LET ld_tot_cesantia_vejez = ld_tot_cesantia_vejez + ld_importe
                 LET ld_tot_cesantia_vejez = ld_tot_cesantia_vejez + ld_importe_isr
               WHEN 6
                 ###Importes de la subcuenta 'Cesantía y Vejez'
                 LET ld_tot_cesantia_vejez = ld_tot_cesantia_vejez + ld_importe
                 LET ld_tot_cesantia_vejez = ld_tot_cesantia_vejez + ld_importe_isr
               WHEN 9
                 ###Importes de la subcuenta 'Cesantía y Vejez'
                 LET ld_tot_cesantia_vejez = ld_tot_cesantia_vejez + ld_importe
                 LET ld_tot_cesantia_vejez = ld_tot_cesantia_vejez + ld_importe_isr
               WHEN 5
                 ###Importes de la subcuenta 'Cuota Social'
                 LET ld_tot_cuota_social = ld_tot_cuota_social + ld_importe
                 LET ld_tot_cuota_social = ld_tot_cuota_social + ld_importe_isr
               WHEN 4
                 ###Importes de la subcuenta 'Vivienda 97'
                 LET ld_tot_vivienda_97 = ld_tot_vivienda_97 + ld_importe
                 LET ld_tot_vivienda_97 = ld_tot_vivienda_97 + ld_importe_isr
               WHEN 7
                 ###Importes de la subcuenta 'Retiro 92'
                 LET ld_tot_retiro_92 = ld_tot_retiro_92 + ld_importe
                 LET ld_tot_retiro_92 = ld_tot_retiro_92 + ld_importe_isr
               WHEN 8
                 ###Importes de la subcuenta 'Vivienda 92'
                 LET ld_tot_vivienda_92 = ld_tot_vivienda_92 + ld_importe
                 LET ld_tot_vivienda_92 = ld_tot_vivienda_92 + ld_importe_isr
            END CASE
        END IF
    END FOREACH
#cpl-2320 se comenta para que no se vayan en ceros las Variables de vivienda
 {   -- Si la siefore no es de vivienda obtenemos los montos de vivienda para los nss
    -- liquidados en el grupo
    IF ps_siefore <> 11 THEN

        LET lc_query_viv = " SELECT NVL( SUM(d.monto_en_pesos) * -1, 0)  \n",
                           " FROM   tmp_anexo_114 d \n",
                           " WHERE d.tipo_movimiento  = ? \n",
                           "   AND d.siefore          = 11 \n",
                           "   AND d.subcuenta        = ? \n",
                           "   AND d.fecha_conversion = ? \n",
                           "   AND d.nss IN (SELECT UNIQUE(d.nss) \n",
                           "                  FROM tmp_anexo_114 d \n",
                           "                 WHERE d.tipo_movimiento    = ? \n",
                           "                   AND d.siefore            = ? \n",
                           "                   AND d.fecha_conversion   = ? ) \n"

        LET li_subcuenta = 4

        PREPARE prp_viv FROM lc_query_viv
        EXECUTE prp_viv USING pi_movimiento         ,
                              li_subcuenta          ,
                              pdt_fecha_liquida     ,
                              pi_movimiento         ,
                              ps_siefore            ,
                              pdt_fecha_liquida
                        INTO ld_tot_vivienda_97

        LET li_subcuenta = 8

        EXECUTE prp_viv USING pi_movimiento         ,
                              li_subcuenta          ,
                              pdt_fecha_liquida     ,
                              pi_movimiento         ,
                              ps_siefore            ,
                              pdt_fecha_liquida
                        INTO ld_tot_vivienda_92

    END IF}

   -- Se elimina el punto decimal
   LET ld_tot_retiro_97      = ld_tot_retiro_97      * 100
   LET ld_tot_cesantia_vejez = ld_tot_cesantia_vejez * 100
   LET ld_tot_cuota_social   = ld_tot_cuota_social   * 100
   LET ld_tot_vivienda_97    = ld_tot_vivienda_97    * 100
   LET ld_tot_retiro_92      = ld_tot_retiro_92      * 100
   LET ld_tot_vivienda_92    = ld_tot_vivienda_92    * 100

   -- Se realiza el conteo de los NSS con Disposiciones (Retiros Totales)
   LET li_num_total_registros = 0

   LET lc_qry_importe = " SELECT NVL(COUNT(DISTINCT d.nss), 0) \n",
                        "   FROM tmp_anexo_114 d \n",
                        "  WHERE d.tipo_movimiento   = ? \n",
                        "    AND d.siefore           = ? \n",
                        "    AND d.fecha_conversion  = ? \n"

    LET lc_qry_importe = lc_qry_importe CLIPPED,
                         lc_query_sie11 CLIPPED


   PREPARE pre_num_disposicion FROM lc_qry_importe
   EXECUTE pre_num_disposicion USING pi_movimiento      ,
                                     ps_siefore         ,
                                     pdt_fecha_liquida
                               INTO li_num_total_registros

   RETURN ld_tot_retiro_97      ,
          ld_tot_cesantia_vejez ,
          ld_tot_cuota_social   ,
          ld_tot_vivienda_97    ,
          ld_tot_retiro_92      ,
          ld_tot_vivienda_92    ,
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
FUNCTION f_regresa_importes_parcial(pi_movimiento, pdt_fecha_liquida, pc_tipo_B, pc_medio_solic, pc_forma_pago)

    DEFINE
        pi_movimiento           INTEGER

    DEFINE
        pdt_fecha_liquida       DATE

    DEFINE
        pc_tipo_B               CHAR(2)

    DEFINE
       li_num_registros         INTEGER

    DEFINE
        ld_importe              DECIMAL(22,6)

    DEFINE
        lc_qry_importe          CHAR(1800)  ,
        lc_desempleo            CHAR(300)

    DEFINE pc_medio_solic     CHAR(003)
    DEFINE pc_forma_pago      CHAR(003)

    -- -----------------------------------------------------------------------------

    -- Inicialización de Variables
    LET li_num_registros       = 0
    LET ld_importe             = 0

    -- Genera la condicion para los retiros parciales por desempleo
    IF (pi_movimiento <> 870) THEN
        -- Si es tipo B, se debe verificar si es tipo B1 o B2
        IF (pi_movimiento = 877) THEN
            LET lc_desempleo = " AND p.tipo_desempleo  = 'B' \n",
                               " AND d.des_tipo_B      = '", pc_tipo_B, "' \n"
        ELSE
            LET lc_desempleo = " AND p.tipo_desempleo IN ('A', 'C', 'D') \n"
        END IF -- Desempleo B
    ELSE
        LET lc_desempleo = " "
    END IF -- Diferente a matrimonio

    LET lc_qry_importe = " SELECT NVL(SUM(ROUND(d.monto_en_pesos, 2) ) * -1 , 0) \n",
                         " FROM   tmp_anexo_114 d, ret_parcial p \n",
                         " WHERE  d.tipo_movimiento  = ? \n",
                         " AND    d.fecha_conversion = ? \n",
                         " AND    d.medio_solic      = ? \n",
                         " AND    d.forma_pago       = ? \n",
                         " AND    p.estado_solicitud IN (8,13,14) \n",
                         " AND    d.consecutivo_lote = p.consecutivo \n",
                         " AND    p.nss              = d.nss\n "

    LET lc_qry_importe = lc_qry_importe CLIPPED, lc_desempleo CLIPPED

    PREPARE pre_importe_parcial FROM lc_qry_importe
    EXECUTE pre_importe_parcial USING pi_movimiento     ,
                                      pdt_fecha_liquida ,
                                      pc_medio_solic    ,
                                      pc_forma_pago
                                INTO  ld_importe

    LET ld_importe = f_lib_redondea_val(ld_importe, 2) * 100

    -- Se realiza el conteo de los NSS que tienen retiros parciales
    LET li_num_registros = 0

    LET lc_qry_importe = " SELECT NVL(COUNT(DISTINCT d.nss), 0) \n",
                         " FROM tmp_anexo_114 d, ret_parcial p \n",
                         " WHERE d.tipo_movimiento  = ? \n",
                         " AND d.fecha_conversion   = ? \n",
                         " AND d.medio_solic        = ? \n",
                         " AND d.forma_pago         = ? \n",
                         " AND p.estado_solicitud   IN (8,13,14) \n",
                         " AND d.consecutivo_lote   = p.consecutivo \n",
                         " AND p.nss                = d.nss\n "

    LET lc_qry_importe = lc_qry_importe CLIPPED ,
                         lc_desempleo   CLIPPED

    PREPARE pre_num_parcial FROM lc_qry_importe

    EXECUTE pre_num_parcial USING pi_movimiento      ,
                                  pdt_fecha_liquida  ,
                                  pc_medio_solic    ,
                                  pc_forma_pago
                            INTO  li_num_registros

    RETURN ld_importe, li_num_registros

END FUNCTION


#################################################################################
#Función                        : f_consolida_subencabezado()                  #
#Objetivo                       : Función que concatena los registros que se    #
#                                 localizan en el archivo "SUB_CZA_ANEXO114",   #
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
       lc_ruta_tmp      CHAR(0400),
       lc_comando_cat   CHAR(1000),
       lc_comando_rm    CHAR(1000),
       lc_comando_chmod CHAR(1000)

    -- -----------------------------------------------------------------------------

    WHENEVER ERROR CONTINUE

    -- Inicialización de variables
    LET lc_ruta_tmp = gr_seg_modulo.ruta_envio CLIPPED, "/", gc_usuario CLIPPED, "rep_temporal.tmp"
    LET lc_ruta_tmp = lc_ruta_tmp CLIPPED

    -- Se concatena el subencabezado al archivo temporal
    LET lc_comando_cat = "cat ", gc_ruta_subencabezado CLIPPED, "> ", lc_ruta_tmp
    LET lc_comando_cat = lc_comando_cat CLIPPED
    RUN lc_comando_cat

    IF ( gi_num_registros = 1 ) THEN
        -- Si es el primer registro, entonce se crea el archivo consolidado
        LET lc_comando_cat = "cat ", lc_ruta_tmp CLIPPED, "> ", gc_ruta_cons_detalle
        LET lc_comando_cat = lc_comando_cat CLIPPED
        RUN lc_comando_cat

        -- Cambia permisos
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
    LET lc_comando_rm = "rm ", lc_ruta_tmp CLIPPED
    LET lc_comando_rm = lc_comando_rm CLIPPED
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
        pi_num_tranfer          ,
        pi_num_disposicion      ,
        pi_num_parciales        INTEGER

    DEFINE
        lc_ruta_tmp             CHAR(0400),
        lc_comando_cat          CHAR(1000),
        lc_comando_rm           CHAR(1000)

    DEFINE
        li_indice               INTEGER

    -- -----------------------------------------------------------------------------

    LET li_indice = 0

    WHENEVER ERROR CONTINUE

    -- Inicialización de variables
    LET lc_ruta_tmp = gr_seg_modulo.ruta_envio CLIPPED, "/", gc_usuario CLIPPED, "rep_temporal.tmp"
    LET lc_ruta_tmp = lc_ruta_tmp CLIPPED

    IF (pi_num_tranfer <>  0) THEN
       IF (li_indice = 0) THEN
          -- Se concatena el detalle de las transferencias al archivo temporal
          -- Por primer vez se crea el archivo con el comando ">"
          LET lc_comando_cat = "cat ", gc_ruta_det_transferencia CLIPPED, "> ", lc_ruta_tmp
       ELSE
          -- Con el comando ">>" Se concatena la información a un archivo existente
          LET lc_comando_cat = "cat ", gc_ruta_det_transferencia CLIPPED, ">> ", lc_ruta_tmp
       END IF

       LET lc_comando_cat = lc_comando_cat CLIPPED
       RUN lc_comando_cat

       LET li_indice = li_indice + 1

    END IF

    IF (pi_num_disposicion <>  0) THEN

       IF (li_indice = 0) THEN
          -- Se concatena el detalle de las disposiciones al archivo temporal
          LET lc_comando_cat = "cat ", gc_ruta_det_disposiciones CLIPPED, "> ", lc_ruta_tmp
       ELSE
          -- Con el comando ">>" Se concatena la información a un archivo existente
          LET lc_comando_cat = "cat ", gc_ruta_det_disposiciones CLIPPED, ">> ", lc_ruta_tmp
       END IF

       LET lc_comando_cat = lc_comando_cat CLIPPED
       RUN lc_comando_cat

       LET li_indice = li_indice + 1

    END IF

    IF (pi_num_parciales <>  0) THEN
       IF (li_indice = 0) THEN
          -- Se concatena el detalle de los retiros parciales al archivo temporal
          LET lc_comando_cat = "cat ", gc_ruta_det_parcial CLIPPED, "> ", lc_ruta_tmp
       ELSE
          -- Con el comando ">>" Se concatena la información a un archivo existente
          LET lc_comando_cat = "cat ", gc_ruta_det_parcial CLIPPED, ">> ", lc_ruta_tmp
       END IF

       LET lc_comando_cat = lc_comando_cat CLIPPED
       RUN lc_comando_cat

       LET li_indice = li_indice + 1

    END IF

    -- Se concatena el contenido del archivo temporal al archivo consolidado
    LET lc_comando_cat = "cat ", lc_ruta_tmp CLIPPED, ">> ", gc_ruta_cons_detalle
    LET lc_comando_cat = lc_comando_cat CLIPPED

    -- Concatenamos los archivos en uno solo y borramos los temporales
    RUN lc_comando_cat

    LET gi_num_consolidado = gi_num_consolidado + 1

    -- Elimina el archivo temporal
    LET lc_comando_rm = "rm ", lc_ruta_tmp CLIPPED
    LET lc_comando_rm = lc_comando_rm CLIPPED
    RUN lc_comando_rm

    WHENEVER ERROR STOP

END FUNCTION

#################################################################################
#Función                        : f_consolida_rechazo()                         #
#Objetivo                       : Función que concatena los registros que se    #
#                                 localizan en el archivo "SUB_CZA_ANEXO114",   #
#                                 y los envía al archivo llamado "CONSOLIDA_DET"#
#Parámetros de entrada          : Ninguno                                       #
#Valores de retorno             : Ninguno                                       #
#Variables globales importantes : gc_ruta_det_rechazo, gc_ruta_cons_detalle     #
#Pantallas utilizadas           : Ninguna                                       #
#Autor                          : David Miguel Garibay Rivera                   #
#Fecha                          : 29/11/2012                                    #
#################################################################################
FUNCTION f_consolida_rechazo()

    DEFINE
        lc_ruta_tmp         CHAR(0400),
        lc_comando_cat      CHAR(1000),
        lc_comando_rm       CHAR(1000)

    -- -----------------------------------------------------------------------------

    WHENEVER ERROR CONTINUE

    -- Inicialización de variables
    LET lc_ruta_tmp = gr_seg_modulo.ruta_envio CLIPPED, "/", gc_usuario CLIPPED, "rep_temporal.tmp"
    LET lc_ruta_tmp = lc_ruta_tmp CLIPPED

    -- Se concatena el detalle de los totales por subcuentas
    LET lc_comando_cat = "cat ", gc_ruta_det_rechazo CLIPPED, "> ", lc_ruta_tmp
    LET lc_comando_cat = lc_comando_cat CLIPPED
    RUN lc_comando_cat

    -- Se concatena el contenido del archivo temporal al archivo consolidado
    LET lc_comando_cat = "cat ", lc_ruta_tmp CLIPPED, ">> ", gc_ruta_cons_detalle
    LET lc_comando_cat = lc_comando_cat CLIPPED
    RUN lc_comando_cat

    -- Concatenamos los archivos en uno solo y borramos los temporales
    LET gi_num_consolidado = gi_num_consolidado + 1

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

    OPEN WINDOW win_anexo_114 AT 4,4 WITH 20 ROWS, 75 COLUMNS
        ATTRIBUTE(BORDER, PROMPT LINE LAST -1)

    DISPLAY " Ctrl-C : Salir                                            Esc : Ejecutar  " AT 1,2 ATTRIBUTE(REVERSE)
    DISPLAY " RETL837            GENERACION DE LOS ANEXOS 114 Y 115                     " AT 3,2 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    -- -----------------------------------------------------------------------------

    CURRENT WINDOW IS win_anexo_114

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

    OUTPUT TO REPORT rpt_cifras_control_anexo114(0                  ,
                                                 lc_valor_nulo      ,
                                                 "0"                ,
                                                 "0"                ,
                                                 0                  ,
                                                 0                  ,
                                                 0                  ,
                                                 0                  ,
                                                 0                  ,
                                                 0                  ,
                                                 0                  ,
                                                 0                  ,
                                                 0                  ,
                                                 ls_encabezado      , -- Bandera que indica si se trata de un encabezado
                                                 ls_subencabezado     -- Bandera que indica si se trata de un subencabezado
                                                )
END FUNCTION

#---------------------------------------------------------------------------#
# f_modifica_viv_infonavit : Busca y modifica los movimientos de los        #
#                            registros que tengan pago de vivienda de       #
#                            acuerdo a los cambios de la ley de infonavit   #
#---------------------------------------------------------------------------#
FUNCTION f_modifica_viv_infonavit()

    DEFINE lr_solicitud RECORD
        folio               LIKE ret_solicitud_tx.folio         ,
        nss                 LIKE ret_solicitud_tx.nss           ,
        consecutivo         LIKE ret_solicitud_tx.consecutivo   ,
        tipo_movimiento     LIKE dis_cuenta.tipo_movimiento
    END RECORD

    DEFINE
        ls_movimiento           SMALLINT

    -- -----------------------------------------------------------------------------

        UPDATE tmp_anexo_114
        SET    tipo_movimiento  = 830
        WHERE  tipo_movimiento  IN (817, 818, 819)

{
    INITIALIZE lr_solicitud.* TO NULL

    DECLARE cur_inf CURSOR FOR
        SELECT folio            ,
               nss              ,
               consecutivo_lote ,
               tipo_movimiento
        FROM   tmp_anexo_114
        WHERE  tipo_movimiento IN (817,818)

    FOREACH cur_inf INTO lr_solicitud.*

        LET ls_movimiento   = 830

        UPDATE tmp_anexo_114
        SET    tipo_movimiento  = ls_movimiento
        WHERE  folio            = lr_solicitud.folio
        AND    nss              = lr_solicitud.nss
        AND    consecutivo_lote = lr_solicitud.consecutivo
        AND    tipo_movimiento  IN (817, 818)

    END FOREACH
}

END FUNCTION

#---------------------------------------------------------------------------#
# f_modifica_desempleo_B : Determina si el tipo de desempleo B fue pagado   #
#                          en el modo 1 o el modo 2                         #
#---------------------------------------------------------------------------#
FUNCTION f_modifica_desempleo_B()

    DEFINE lr_solicitud RECORD
        nss             LIKE ret_solicitud_tx.nss           ,
        consecutivo     LIKE ret_solicitud_tx.consecutivo
    END RECORD

    DEFINE lr_pago_des RECORD
        mto_1           LIKE ret_ctr_pago.mto_1     ,
        mto_2           LIKE ret_ctr_pago.mto_2     ,
        mto_pago        LIKE ret_ctr_pago.mto_pago
    END RECORD

   DEFINE v_dif_mto1                 DECIMAL(16,6),
          v_dif_mto2                 DECIMAL(16,6)

    DEFINE
        lc_tipo_des             CHAR(2)

    DEFINE
        ls_movimiento           SMALLINT

    -- -----------------------------------------------------------------------------

    INITIALIZE
        lr_solicitud.*          ,
        lr_pago_des.*           TO NULL

    LET lc_tipo_des = "  "

    DECLARE cur_des_B CURSOR FOR
        SELECT UNIQUE(nss)      ,
               consecutivo_lote
        FROM   tmp_anexo_114
        WHERE  tipo_movimiento  = 877

    FOREACH cur_des_B INTO lr_solicitud.*

        SELECT mto_1    ,
               mto_2    ,
               mto_pago
        INTO   lr_pago_des.*
        FROM   ret_ctr_pago
        WHERE  nss          = lr_solicitud.nss
        AND    consecutivo  = lr_solicitud.consecutivo

        IF lr_pago_des.mto_pago IS NOT NULL THEN --[CPL-3396]
           IF lr_pago_des.mto_pago = lr_pago_des.mto_1 THEN
              LET lc_tipo_des = "B2"           -- 90 Dias SBC
           ELSE
              IF lr_pago_des.mto_pago = lr_pago_des.mto_2 THEN
                 LET lc_tipo_des = "B1" -- 11.5% RCV
              ELSE
                 LET v_dif_mto1 = lr_pago_des.mto_pago - lr_pago_des.mto_1
                 LET v_dif_mto2 = lr_pago_des.mto_pago - lr_pago_des.mto_2
                 
                 IF v_dif_mto1 < 0 THEN
                    LET v_dif_mto1 = v_dif_mto1 * -1
                 END IF
                 IF v_dif_mto2 < 0 THEN
                    LET v_dif_mto2 = v_dif_mto2 * -1
                 END IF

                 IF v_dif_mto1 > v_dif_mto2 THEN
                    LET lc_tipo_des = "B1" -- 11.5% RCV
                 ELSE
                    LET lc_tipo_des = "B2" -- 90 Dias SBC
                 END IF
              END IF
           END IF
        END IF --//[CPL-3396]

        UPDATE tmp_anexo_114
        SET    des_tipo_B           = lc_tipo_des
        WHERE  nss                  = lr_solicitud.nss
        AND    consecutivo_lote     = lr_solicitud.consecutivo
        AND    tipo_movimiento      = 877

        INITIALIZE
            lr_solicitud.*          ,
            lr_pago_des.*           TO NULL

        LET lc_tipo_des = "  "

    END FOREACH

END FUNCTION


FUNCTION f_modifica_medio_solic_forma_pago()

    DEFINE lr_solicitud RECORD
        nss             LIKE ret_solicitud_tx.nss           ,
        consecutivo     LIKE ret_solicitud_tx.consecutivo
    END RECORD

    DEFINE lc_medio_solic     CHAR(003)
    DEFINE lc_forma_pago      CHAR(003)
    DEFINE ls_tipo_pago       SMALLINT

    -- -----------------------------------------------------------------------------

    INITIALIZE lr_solicitud.* TO NULL

    LET lc_medio_solic = "   "
    LET lc_forma_pago  = "   "
    LET ls_tipo_pago   = 0

    DECLARE cur_medio_solic CURSOR FOR
        SELECT UNIQUE(nss)      ,
               consecutivo_lote
        FROM   tmp_anexo_114

    FOREACH cur_medio_solic INTO lr_solicitud.*

        SELECT "OK"
        FROM   ret_ws_notifica_app
        WHERE  nss          = lr_solicitud.nss
        AND    consecutivo  = lr_solicitud.consecutivo
        AND    aplicacion_origen <> '07'
        GROUP BY 1
        
        IF SQLCA.SQLCODE = 0 THEN  --existe en tabla control de app
             LET lc_medio_solic = '002'
        ELSE
        	   SELECT "OK"
             FROM   ret_ws_notifica_app
             WHERE  nss          = lr_solicitud.nss
             AND    consecutivo  = lr_solicitud.consecutivo
             AND    aplicacion_origen = '07'
             GROUP BY 1
             
             IF SQLCA.SQLCODE = 0 THEN  --existe en tabla control de afore web
             	    LET lc_medio_solic = '003'
             ELSE
             	    LET lc_medio_solic = '001'
             END IF
        END IF
        
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

        UPDATE tmp_anexo_114
        SET    medio_solic          = lc_medio_solic,
               forma_pago           = lc_forma_pago
        WHERE  nss                  = lr_solicitud.nss
        AND    consecutivo_lote     = lr_solicitud.consecutivo

        INITIALIZE lr_solicitud.* TO NULL

        LET lc_medio_solic = "   "
        LET lc_forma_pago  = "   "
        LET ls_tipo_pago   = 0

    END FOREACH

END FUNCTION


#---------------------------------------------------------------------------#
# f_genera_plan_privado_97 : Busca y modifica el tipo de movimiento para    #
#                            los registros que tengan tipo movimiento 840   #
#                            (retiro F) y regimen 97                        #
#---------------------------------------------------------------------------#
FUNCTION f_genera_plan_privado_97()

    DEFINE lr_pp_97 RECORD
        nss             LIKE dis_cuenta.nss                 ,
        folio           LIKE dis_cuenta.folio               ,
        consecutivo     LIKE dis_cuenta.consecutivo_lote
    END RECORD

    -- -----------------------------------------------------------------------------

    INITIALIZE lr_pp_97.* TO NULL

    DECLARE cur_pp97 CURSOR FOR
        SELECT nss          ,
               folio        ,
               consecutivo
        FROM   ret_solicitud_tx
        WHERE  consecutivo IN (SELECT UNIQUE(consecutivo_lote)
                               FROM   tmp_anexo_114
                               WHERE  tipo_movimiento = 840
                               )
        AND    tipo_retiro  = "F"
        AND    regimen      = 97

    FOREACH cur_pp97 INTO lr_pp_97.*

        UPDATE tmp_anexo_114
        SET    tipo_movimiento  = gs_mov_tipo_F97
        WHERE  nss              = lr_pp_97.nss
        AND    folio            = lr_pp_97.folio
        AND    consecutivo_lote = lr_pp_97.consecutivo

    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# f_modifica_siefore_viv : Modifica la siefore 11 en los registros que      #
#                          pagan la vivienda y le asigna la siefore basica  #
#                          que tenga el trabajador                          #
#---------------------------------------------------------------------------#
FUNCTION f_modifica_siefore_viv()

    DEFINE lr_vivienda RECORD
        nss                 LIKE dis_cuenta.nss                 ,
        folio               LIKE dis_cuenta.folio               ,
        consecutivo_lote    LIKE dis_cuenta.consecutivo_lote    ,
        tipo_movimiento     LIKE dis_cuenta.tipo_movimiento 
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
               tipo_movimiento
        FROM   tmp_anexo_114
        WHERE  siefore  = 11
        ORDER BY folio        

    FOREACH cur_viv INTO lr_vivienda.*
        
        SELECT UNIQUE(siefore)
        INTO   ls_sie_basica
        FROM   tmp_anexo_114
        WHERE  nss              = lr_vivienda.nss             
        AND    folio            = lr_vivienda.folio           
        AND    consecutivo_lote = lr_vivienda.consecutivo_lote
        AND    tipo_movimiento  = lr_vivienda.tipo_movimiento 
        AND    siefore         <> 11        
        
        IF ( (ls_sie_basica = 0) OR (ls_sie_basica IS NULL) ) THEN
            LET ls_sie_basica = f_lib_obtiene_siefore_act(lr_vivienda.nss)
        END IF 

        IF (ls_sie_basica <> 10) THEN
            UPDATE tmp_anexo_114
            SET    siefore          = ls_sie_basica
            WHERE  nss              = lr_vivienda.nss             
            AND    folio            = lr_vivienda.folio           
            AND    consecutivo_lote = lr_vivienda.consecutivo_lote
            AND    tipo_movimiento  = lr_vivienda.tipo_movimiento 
            AND    siefore          = 11        
        END IF 
        
        LET ls_sie_basica = 0
        INITIALIZE lr_vivienda.* TO NULL        
        
    END FOREACH 

END FUNCTION

#---------------------------------------------------------------------------#
# f_regresa_dat_cat_114 : Realiza la conversion del tipo de retiro y de     #
#                         tipo prestacion de acuerdo al catalogo del        #
#                         anexo 114                                         #
#---------------------------------------------------------------------------#
FUNCTION f_regresa_dat_cat_114(pi_movimiento)

    DEFINE pi_movimiento LIKE dis_cuenta.tipo_movimiento

    DEFINE lr_movimiento RECORD
        tipo_retiro         CHAR(03)    ,
        tipo_prestacion     CHAR(02)
    END RECORD

    -- -----------------------------------------------------------------------------

    CASE (pi_movimiento)

        -- Transferencias
        WHEN 800
           LET lr_movimiento.tipo_retiro        = "001"
           LET lr_movimiento.tipo_prestacion    = "61"

        WHEN 806
            LET lr_movimiento.tipo_retiro       = "022"   -- CPL-2902
            LET lr_movimiento.tipo_prestacion   = "60"

        WHEN 810
            LET lr_movimiento.tipo_retiro       = "002"
            LET lr_movimiento.tipo_prestacion   = "60"

        WHEN 815
            LET lr_movimiento.tipo_retiro       = "003"
            LET lr_movimiento.tipo_prestacion   = "62"
        
        WHEN 805
            LET lr_movimiento.tipo_retiro       = "021"
            LET lr_movimiento.tipo_prestacion   = "21"

        -- Disposiciones
        WHEN 820
           LET lr_movimiento.tipo_retiro        = "004"
           LET lr_movimiento.tipo_prestacion    = "63"

        WHEN 825
           LET lr_movimiento.tipo_retiro        = "013"
           LET lr_movimiento.tipo_prestacion    = "60"

        WHEN 830
           LET lr_movimiento.tipo_retiro        = "005"
           LET lr_movimiento.tipo_prestacion    = "60"     
           
        WHEN 835                                            --- CPL-2794
        	 LET lr_movimiento.tipo_retiro        = "015"     --- CPL-2794
        	 LET lr_movimiento.tipo_prestacion    = "60"      --- CPL-2794

        WHEN 840
           LET lr_movimiento.tipo_retiro        = "006"
           LET lr_movimiento.tipo_prestacion    = "65"

        -- Retiro F, Regimen 97
        WHEN gs_mov_tipo_F97
           LET lr_movimiento.tipo_retiro        = "006"
           LET lr_movimiento.tipo_prestacion    = "64"

        WHEN 850
           LET lr_movimiento.tipo_retiro        = "007"
           LET lr_movimiento.tipo_prestacion    = "66"

        WHEN 860
           LET lr_movimiento.tipo_retiro        = "008"
           LET lr_movimiento.tipo_prestacion    = "67"

        WHEN 880
            LET lr_movimiento.tipo_retiro       = "010"
            LET lr_movimiento.tipo_prestacion   = "68"

        -- PMG
        WHEN 841
            LET lr_movimiento.tipo_retiro       = "018"
            LET lr_movimiento.tipo_prestacion   = "62"

        -- Parciales
        WHEN 870
            LET lr_movimiento.tipo_retiro       = "009"
            LET lr_movimiento.tipo_prestacion   = "07"

        WHEN 875
            LET lr_movimiento.tipo_retiro       = "009"
            LET lr_movimiento.tipo_prestacion   = "06"

        WHEN 876
            LET lr_movimiento.tipo_retiro       = "009"
            LET lr_movimiento.tipo_prestacion   = "06"

        WHEN 877
            LET lr_movimiento.tipo_retiro       = "009"
            LET lr_movimiento.tipo_prestacion   = "06"

        WHEN 878
            LET lr_movimiento.tipo_retiro       = "009"
            LET lr_movimiento.tipo_prestacion   = "06"

    END CASE

    RETURN lr_movimiento.*


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

    IF pi_movimiento = gs_mov_tipo_F97 THEN
    	
       LET lc_tipo_retiro = "F"
        
    ELSE
    	 
    	 -- CPL-2902
    	 IF pi_movimiento = 806 THEN 
    	 	  LET lc_tipo_retiro = "B" 
    	 ELSE 
          SELECT tipo_retiro
            INTO lc_tipo_retiro
            FROM tab_retiro
           WHERE movimiento = pi_movimiento
           GROUP BY 1
    	 END IF       

       # SELECT tipo_retiro
       # INTO   lc_tipo_retiro
       # FROM   tab_retiro
       # WHERE  movimiento   = pi_movimiento
       # GROUP BY 1        
       -- CPL-2902 
        
    END IF

    RETURN lc_tipo_retiro

END FUNCTION


#---------------------------------------------------------------------------#
# f_num_solicitudes_rechazo : Obtiene el numero de registros liquidados con #
#                             la subcuenta SAR 92 en el periodo indicado    #
#---------------------------------------------------------------------------#
FUNCTION f_num_solicitudes_rechazo(pi_movimiento        ,
                                   pdt_fecha_ini_semana ,
                                   pdt_fecha_fin_semana  )

    DEFINE pi_movimiento LIKE dis_cuenta.tipo_movimiento

    DEFINE
        pdt_fecha_ini_semana            ,
        pdt_fecha_fin_semana            DATE

    DEFINE
        lc_qry_nss                      CHAR(1500)

    DEFINE
        li_num_registros                INTEGER

    -- -----------------------------------------------------------------------------

    -- Inicialización de variables
    LET li_num_registros = 0

    -- Realiza el conteo de los NSS con transferencias
    IF (pi_movimiento = 800 OR
        pi_movimiento = 805 OR
        pi_movimiento = 810 OR
        pi_movimiento = 815 OR
        pi_movimiento = 806 ) THEN

        LET li_num_registros = 0
        LET lc_qry_nss       = " ",
                               "\n SELECT COUNT(DISTINCT d.nss)                                  ",
                               "\n   FROM tmp_anexo_114 d, ret_trans_imss t, OUTER tab_retiro r  ",
                               "\n  WHERE d.tipo_movimiento   = ?                                ",
                               "\n    AND d.fecha_conversion >= ?                                ",
                               "\n    AND d.fecha_conversion <= ?                                ",
                               "\n    AND r.movimiento        = d.tipo_movimiento                ",
                               "\n    AND t.consecutivo       = d.consecutivo_lote               ",
                               "\n    AND t.nss               = d.nss                            ",
                               "\n    AND d.subcuenta        IN ( 7 )                            "

        PREPARE prp_regs_rechazo_trans FROM lc_qry_nss
        EXECUTE prp_regs_rechazo_trans USING pi_movimiento        ,
                                           pdt_fecha_ini_semana ,
                                           pdt_fecha_fin_semana
                                     INTO  li_num_registros
    END IF

    -- Realiza el conteo de los NSS con disposiciones
    IF (pi_movimiento =  820 OR
        pi_movimiento =  825 OR
        pi_movimiento =  830 OR
        pi_movimiento =  840 OR
        pi_movimiento =  850 OR
        pi_movimiento =  860 OR
        pi_movimiento =  880 OR
        pi_movimiento =  841 ) THEN

        LET li_num_registros = 0
        LET lc_qry_nss = " SELECT COUNT(DISTINCT d.nss) \n",
                             "   FROM tmp_anexo_114 d, tab_retiro r, ret_solicitud_tx t\n",
                             "  WHERE d.tipo_movimiento   = ? \n",
                             "    AND d.fecha_conversion  >= ? \n",
                             "    AND d.fecha_conversion  <= ? \n",
                             "    AND r.movimiento = d.tipo_movimiento \n",
                             "    AND t.nss        = d.nss \n",
                             "    AND t.consecutivo = d.consecutivo_lote \n",
                             "    AND d.subcuenta IN ( 7 ) \n"

        PREPARE prp_regs_rechazo_disp FROM lc_qry_nss
        EXECUTE prp_regs_rechazo_disp USING pi_movimiento         ,
                                          pdt_fecha_ini_semana  ,
                                          pdt_fecha_fin_semana
                                    INTO  li_num_registros
    END IF

    RETURN li_num_registros

END FUNCTION

#################################################################################
#Reporte                        : rpt_cza_anexo_114()                           #
#Objetivo                       : Genera el encabezado del anexo 114            #
#Parámetros de entrada          : Ninguno                                       #
#Valores de retorno             : Ninguno                                       #
#Variables globales importantes : Ninguna                                       #
#Pantallas utilizadas           : Ninguna                                       #
#Autor                          : David Miguel Garibay Rivera                   #
#Fecha                          : 27/11/2012                                    #
#################################################################################
REPORT rpt_cza_anexo_114()

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
                COLUMN 004, "0703"                            ,-- Tipo de Archivo
                COLUMN 008, "001"                             ,-- Tipo de Entidad que reporta
                COLUMN 011, gs_codigo_afore USING "&&&"       ,-- Clave de Entidad que reporta
                COLUMN 014, HOY USING "YYYYMMDD"              ,-- Fecha de Información
                COLUMN 022, "106"                             ,-- Tamaño del Registro
                COLUMN 025, gi_num_registros USING "&&&&&"    ,-- Número de registros
                COLUMN 030, 77 SPACES                         -- Filler

END REPORT

#################################################################################
#Reporte                        : rpt_subcza_anexo_114()                        #
#Objetivo                       : Genera el encabezado del anexo 114            #
#Parámetros de entrada          : Ninguno                                       #
#Valores de retorno             : Ninguno                                       #
#Variables globales importantes : Ninguna                                       #
#Pantallas utilizadas           : Ninguna                                       #
#Autor                          : David Miguel Garibay Rivera                   #
#Fecha                          : 27/11/2012                                    #
#################################################################################
REPORT rpt_subcza_anexo_114(pdt_fecha_ini_semana, pdt_fecha_fin_semana)
DEFINE
   pdt_fecha_ini_semana DATE,
   pdt_fecha_fin_semana DATE

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
            PRINT
                COLUMN 001, "101"                                ,-- Tipo registro
                COLUMN 004, pdt_fecha_ini_semana USING "YYYYMMDD",-- Fecha de la primera liquidación reportada
                COLUMN 012, pdt_fecha_fin_semana USING "YYYYMMDD",-- Fecha de la última liquidación reportada
                COLUMN 020, 87 SPACES                             -- Filler

END REPORT

#################################################################################
#Reporte                        : rpt_transferencias_anexo114()                 #
#Objetivo                       : Genera el detalle de las transferencias       #
#Parámetros de entrada          : Ninguno                                       #
#Valores de retorno             : Ninguno                                       #
#Variables globales importantes : Ninguna                                       #
#Pantallas utilizadas           : Ninguna                                       #
#Autor                          : David Miguel Garibay Rivera                   #
#Fecha                          : 27/11/2012                                    #
#################################################################################
REPORT rpt_transferencias_anexo114(pdt_fecha_liquida        ,
                                   pi_siefore               ,
                                   pd_tot_retiro_97         ,
                                   pd_tot_cesantia_vejez    ,
                                   pd_tot_cuota_social      ,
                                   pd_tot_vivienda_97       ,
                                   pd_tot_sar_92            ,
                                   pi_num_registros         ,
                                   pc_tipo_retiro_rpt       ,
                                   pc_tipo_prestacion_rpt
                                  )

    DEFINE
        pdt_fecha_liquida           DATE,
        pi_siefore                  INTEGER,
        pd_tot_retiro_97            DECIMAL(22,0),
        pd_tot_cesantia_vejez       DECIMAL(22,0),
        pd_tot_cuota_social         DECIMAL(22,0),
        pd_tot_vivienda_97          DECIMAL(22,0),
        pd_tot_sar_92               DECIMAL(26,0),
        pi_num_registros            INTEGER,
        pc_tipo_retiro_rpt          CHAR(3),
        pc_tipo_prestacion_rpt      CHAR(2)

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
           CALL f_siefore_consar(pi_siefore) RETURNING pi_siefore --CPL-2902 mtb

            PRINT                
                COLUMN 001, "301"                                       ,-- Tipo registro
                COLUMN 004, pdt_fecha_liquida USING "YYYYMMDD"          ,-- Fecha de liquidacion
                COLUMN 012, pc_tipo_retiro_rpt                          ,-- Tipo de retiro según información anexada
                COLUMN 015, pc_tipo_prestacion_rpt                      ,-- Tipo de prestación según información anexada
                COLUMN 017, pi_siefore USING "&&"                       ,-- Siefore
                COLUMN 019, pd_tot_retiro_97      USING "&&&&&&&&&&&&&" ,-- Monto en pesos liquidado para la subcuenta de Retiro 97
                COLUMN 032, pd_tot_cesantia_vejez USING "&&&&&&&&&&&&&" ,-- Monto en pesos liquidado para la subcuenta de Cesantía y Vejez
                COLUMN 045, pd_tot_cuota_social   USING "&&&&&&&&&&&&&" ,-- Monto en pesos liquidado para la subcuenta de Cuota Social
                COLUMN 058, pd_tot_sar_92         USING "&&&&&&&&&&&&&" ,-- Se reporta SAR 92 para Retiro U CPL-2415                  -- CPL-2902   
                COLUMN 071, pd_tot_vivienda_97    USING "&&&&&&&&&&&&&" ,-- Monto en pesos liquidado para la subcuenta de Vivienda 97 -- CPL-2902
                COLUMN 084, pi_num_registros      USING "&&&&&&&&&&"    ,-- Número de registros con estas carctaísticas               -- CPL-2902
                COLUMN 094, 13 SPACES                                    -- FILLER                                                    -- CPL-2902
                
                -- CPL-2902
                -- COLUMN 094, pd_tot_sar_92 USING "&&&&&&&&&&&&&&&&&&&&&&&&&&"  --Se reporta SAR 92 para Retiro U #CPL-2415
                -- CPL-2902                                

END REPORT

#################################################################################
#Reporte                        : rpt_disposicion_anexo114()                    #
#Objetivo                       : Genera el detalle de las disposiciones        #
#Parámetros de entrada          : Ninguno                                       #
#Valores de retorno             : Ninguno                                       #
#Variables globales importantes : Ninguna                                       #
#Pantallas utilizadas           : Ninguna                                       #
#Autor                          : David Miguel Garibay Rivera                   #
#Fecha                          : 27/11/2012                                    #
#################################################################################
REPORT rpt_disposicion_anexo114(pdt_fecha_liquida      ,
                                pi_siefore             ,
                                pd_cad_retiro_97       ,
                                pd_cad_cesantia_vejez  ,
                                pd_cad_cuota_social    ,
                                pd_cad_vivienda_97     ,
                                pd_cad_retiro_92       ,
                                pd_cad_vivienda_92     ,
                                pi_num_registros       ,
                                pc_tipo_retiro_rpt     ,
                                pc_tipo_prestacion_rpt)

    DEFINE
        pdt_fecha_liquida       DATE,
        pi_siefore              INTEGER,
        pd_cad_retiro_97        DECIMAL(22,0),
        pd_cad_cesantia_vejez   DECIMAL(22,0),
        pd_cad_cuota_social     DECIMAL(22,0),
        pd_cad_vivienda_97      DECIMAL(22,0),
        pd_cad_retiro_92        DECIMAL(22,0),
        pd_cad_vivienda_92      DECIMAL(22,0),
        pi_num_registros        INTEGER,
        pc_tipo_retiro_rpt      CHAR(3),
        pc_tipo_prestacion_rpt  CHAR(2)

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW

            CALL f_siefore_consar(pi_siefore) RETURNING pi_siefore --CPL-2902 mtb

            PRINT
                COLUMN 001, "302"                                       ,-- Tipo registro
                COLUMN 004, pdt_fecha_liquida USING "YYYYMMDD"          ,-- Fecha de liquidacion
                COLUMN 012, pc_tipo_retiro_rpt                          ,-- Tipo de retiro según información anexada
                COLUMN 015, pc_tipo_prestacion_rpt                      ,-- Tipo de prestación según información anexada
                COLUMN 017, pi_siefore USING "&&"                       ,-- Siefore
                COLUMN 019, pd_cad_retiro_97      USING "&&&&&&&&&&&&&" ,-- Monto en pesos liquidado para la subcuenta de Retiro 97
                COLUMN 032, pd_cad_cesantia_vejez USING "&&&&&&&&&&&&&" ,-- Monto en pesos liquidado para la subcuenta de Cesantía y Vejez
                COLUMN 045, pd_cad_cuota_social   USING "&&&&&&&&&&&&&" ,-- Monto en pesos liquidado para la subcuenta de Cuota Social
                COLUMN 058, pd_cad_retiro_92      USING "&&&&&&&&&&&&&" ,-- Monto en pesos liquidado para la subcuenta de Retiro 92
                COLUMN 071, pd_cad_vivienda_92    USING "&&&&&&&&&&&&&" ,-- Monto en pesos liquidado para la subcuenta de Vivienda 92
                COLUMN 084, pd_cad_vivienda_97    USING "&&&&&&&&&&&&&" ,-- Monto en pesos liquidado para la subcuenta de Vivienda 97
                COLUMN 097, pi_num_registros      USING "&&&&&&&&&&"     -- Número de registros con estas carctaísticas

END REPORT


#################################################################################
#Reporte                        : rpt_parciales_anexo114()                      #
#Objetivo                       : Genera el detalle de los retiros parciales    #
#Parámetros de entrada          : Ninguno                                       #
#Valores de retorno             : Ninguno                                       #
#Variables globales importantes : Ninguna                                       #
#Pantallas utilizadas           : Ninguna                                       #
#Autor                          : David Miguel Garibay Rivera                   #
#Fecha                          : 29/11/2012                                    #
#################################################################################
REPORT rpt_parciales_anexo114(pdt_fecha_liquida      ,
                              pd_importe             ,
                              pi_num_registros       ,
                              pc_tipo_retiro_rpt     ,
                              pc_tipo_prestacion_rpt ,
                              pi_proceso_retiro      ,
                              pi_clasificacion_pago  ,
                              pc_medio_solic         ,
                              pc_forma_pago)

    DEFINE
        pdt_fecha_liquida       DATE,
        pd_importe              DECIMAL(22,0),
        pc_cad_rcv              CHAR(13),
        pi_num_registros        INTEGER,
        pc_tipo_retiro_rpt      CHAR(3),
        pc_tipo_prestacion_rpt  CHAR(2),
        pi_proceso_retiro       INTEGER,
        pi_clasificacion_pago   INTEGER

    DEFINE pc_medio_solic     CHAR(003)
    DEFINE pc_forma_pago      CHAR(003)

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
            PRINT
                COLUMN 001, "303"                                   ,-- Tipo registro
                COLUMN 004, pdt_fecha_liquida USING "YYYYMMDD"      ,-- Fecha de liquidacion
                COLUMN 012, pc_tipo_retiro_rpt                      ,-- Tipo de retiro según información anexada
                COLUMN 015, pc_tipo_prestacion_rpt                  ,-- Tipo de prestación según información anexada
                COLUMN 017, pi_proceso_retiro USING "&"             ,-- Tipo de proceso de retiro (Aplica sólamente a Desempleo. Tipo Prestación = 6)
                COLUMN 018, pi_clasificacion_pago USING "&"         ,-- Clasificación de pago (Aplica sólamente a Desempleo. Tipo Prestación = 6)
                COLUMN 019, pd_importe       USING "&&&&&&&&&&&&&"  ,-- Monto Pagado al trabajador
                COLUMN 032, pi_num_registros USING "&&&&&&&&&&"     ,-- Número de registros con estas características
                COLUMN 042, pc_medio_solic                          ,-- De acuerdo al Catálogo  de Medio de Solicitud del Catálogo General vigente
                COLUMN 045, pc_forma_pago                           ,-- De acuerdo al Catálogo  de Formas de Pago del Catálogo General vigente
                COLUMN 048, 59 SPACES                                -- Filler

END REPORT

#################################################################################
#Reporte                        : rpt_rechazo_anexo114()                        #
#Objetivo                       : Genera el detalle por subcuentas              #
#Parámetros de entrada          : Ninguno                                       #
#Valores de retorno             : Ninguno                                       #
#Variables globales importantes : Ninguna                                       #
#Pantallas utilizadas           : Ninguna                                       #
#Autor                          : David Miguel Garibay Rivera                   #
#Fecha                          : 29/11/2012                                    #
#################################################################################
REPORT rpt_rechazo_anexo114(pc_fecha_solicitud, pc_tipo_prestacion, pc_tipo_desempleo, 
                            pc_diagnostico,     pc_contador,        pc_medio_solic  )

    DEFINE pc_fecha_solicitud   CHAR(08)
    DEFINE pc_tipo_prestacion   CHAR(02)
    DEFINE pc_tipo_desempleo    CHAR(01)
    DEFINE pc_diagnostico       CHAR(04)
    DEFINE pc_contador          CHAR(12)
    DEFINE pc_medio_solic       CHAR(03)
                                                           
    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
            
            PRINT                
                COLUMN 001, "304"                    , -- Tipo Registro
                COLUMN 004, pc_fecha_solicitud       , -- Fecha Solicitud
                COLUMN 012, pc_tipo_prestacion       , -- Tipo prestacion 
                COLUMN 014, pc_tipo_desempleo        , -- Tipo Retiro por desempleo
                COLUMN 015, pc_diagnostico           , -- Diagnostico de certificacion
                COLUMN 019, pc_contador              , -- Número de registros con estas caracteríasticas
                COLUMN 031, pc_medio_solic           , -- De acuerdo al Catálogo  de Medio de Solicitud del Catálogo General vigente
                COLUMN 034, 73 SPACES 
 
 
END REPORT

#################################################################################
#Reporte                        : rpt_cifras_control_anexo114()                 #
#Objetivo                       : Genera el reporte de cifras de control del    #
#                                 anexo 114                                     #
#Parámetros de entrada          : Ninguno                                       #
#Valores de retorno             : Ninguno                                       #
#Variables globales importantes : Ninguna                                       #
#Pantallas utilizadas           : Ninguna                                       #
#Autor                          : David Miguel Garibay Rivera                   #
#Fecha                          : 12/12/2012                                    #
#################################################################################
REPORT rpt_cifras_control_anexo114(pi_tipo_detalle       ,
                                   pdt_fecha_liquidacion ,
                                   pc_tipo_retiro        ,
                                   pc_tipo_prestacion    ,
                                   pi_siefore            ,
                                   pd_imp_retiro_97      ,
                                   pd_imp_cesantia_vejez ,
                                   pd_imp_cuota_social   ,
                                   pd_imp_vivienda_97    ,
                                   pd_imp_retiro_92      ,
                                   pd_imp_vivienda_92    ,
                                   pd_imp_rcv            ,
                                   pi_num_registros      ,
                                   pi_bnd_encabezado     ,
                                   pi_bnd_subencabezado
                                  )

    DEFINE
        pi_tipo_detalle             INTEGER         ,
        pdt_fecha_liquidacion       DATE            ,
        pc_tipo_retiro              CHAR(3)         ,
        pc_tipo_prestacion          CHAR(2)         ,
        pi_siefore                  INTEGER         ,
        pd_imp_retiro_97            DECIMAL(22,2)   ,
        pd_imp_cesantia_vejez       DECIMAL(22,2)   ,
        pd_imp_cuota_social         DECIMAL(22,2)   ,
        pd_imp_rcv                  DECIMAL(22,2)   ,
        pd_imp_retiro_92            DECIMAL(22,2)   ,
        pd_imp_vivienda_92          DECIMAL(22,2)   ,
        pd_imp_vivienda_97          DECIMAL(22,2)   ,
        pi_num_registros            INTEGER         ,
        pi_bnd_encabezado           INTEGER         ,
        pi_bnd_subencabezado        INTEGER         ,
        ld_importe_total            DECIMAL(22,2)   ,
        li_num_det_01               INTEGER         ,
        li_num_det_02               INTEGER         ,
        li_num_det_03               INTEGER         ,
        li_num_det_04               INTEGER         ,
        li_num_subencabezado        INTEGER         ,
        li_num_encabezado           INTEGER

    OUTPUT
        PAGE LENGTH   10
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
       FIRST PAGE HEADER
          --Inicialización de variables
          LET li_num_det_01 = 0
          LET li_num_det_02 = 0
          LET li_num_det_03 = 0
          LET li_num_det_04 = 0
          LET li_num_subencabezado = 0
          LET li_num_encabezado    = 0
          PRINT
              "TIPO DE DETALLE"                   , "|",
              "FECHA LIQUIDACIÓN"                 , "|",
              "TIPO DE RETIRO"                    , "|",
              "TIPO DE PRESTACIÓN"                , "|",
              "SIEFORE"                           , "|",
              "RETIRO 97"                         , "|",
              "CESANTÍA Y VEJEZ"                  , "|",
              "CUOTA SOCIAL"                      , "|",
              "TOTAL RCV"                         , "|",
              "RETIRO 92"                         , "|",
              "VIVIENDA 92"                       , "|",
              "VIVIENDA 97"                       , "|",
              "TOTAL"                             , "|",
              "NO. DE REGISTROS"                  , "|"

        ON EVERY ROW
            -- En caso de tratarse de un encabezado, entonces no imprime detalle
            IF (pi_bnd_encabezado = 1) THEN
               LET li_num_encabezado = li_num_encabezado + pi_bnd_encabezado
            ELSE
               -- En caso de tratarse de un subencabezado, entonces no imprime detalle
               IF (pi_bnd_subencabezado = 1) THEN
                  LET li_num_subencabezado = li_num_subencabezado + pi_bnd_subencabezado
               ELSE
                  CASE (pi_tipo_detalle)
                     WHEN 301
                        LET li_num_det_01 = li_num_det_01 + 1
                     WHEN 302
                        LET li_num_det_02 = li_num_det_02 + 1
                     WHEN 303
                        LET li_num_det_03 = li_num_det_03 + 1
                     WHEN 304
                        LET li_num_det_04 = li_num_det_04 + 1
                  END CASE

                  -- Para mostrar el punto decimal al importe
                  LET pd_imp_retiro_97      = pd_imp_retiro_97      / 100
                  LET pd_imp_cesantia_vejez = pd_imp_cesantia_vejez / 100
                  LET pd_imp_cuota_social   = pd_imp_cuota_social   / 100
                  LET pd_imp_rcv            = pd_imp_rcv            / 100
                  LET pd_imp_retiro_92      = pd_imp_retiro_92      / 100
                  LET pd_imp_vivienda_92    = pd_imp_vivienda_92    / 100
                  LET pd_imp_vivienda_97    = pd_imp_vivienda_97    / 100

                  LET ld_importe_total = pd_imp_retiro_97      +
                                         pd_imp_cesantia_vejez +
                                         pd_imp_cuota_social   +
                                         pd_imp_rcv            +
                                         pd_imp_retiro_92      +
                                         pd_imp_vivienda_92    +
                                         pd_imp_vivienda_97

                     -- Se trata de un detalle
                     PRINT pi_tipo_detalle       ,"|",
                           pdt_fecha_liquidacion USING "DD/MM/YYYY","|",
                           pc_tipo_retiro        CLIPPED,"|",
                           pc_tipo_prestacion    CLIPPED,"|",
                           pi_siefore            CLIPPED,"|",
                           pd_imp_retiro_97      ,"|",
                           pd_imp_cesantia_vejez ,"|",
                           pd_imp_cuota_social   ,"|",
                           pd_imp_rcv            ,"|",
                           pd_imp_retiro_92      ,"|",
                           pd_imp_vivienda_92    ,"|",
                           pd_imp_vivienda_97    ,"|",
                           ld_importe_total      ,"|",
                           pi_num_registros      ,"|"
                  --END IF
               END IF
            END IF

        ON LAST ROW
           PRINT " "
           PRINT " No. de Registros de Encabezado      ", li_num_encabezado
           PRINT " No. de Registros de Subencabezado   ", li_num_subencabezado
           PRINT " Total de Registros de Detalle 1     ", li_num_det_01
           PRINT " Total de Registros de Detalle 2     ", li_num_det_02
           PRINT " Total de Registros de Detalle 3     ", li_num_det_03
           PRINT " Total de Registros de Detalle 4     ", li_num_det_04

END REPORT
-------------------------------------------------------------------------------

#########################################################################
FUNCTION f_siefore_consar(ls_siefore)

DEFINE ls_siefore       SMALLINT
DEFINE ls_siefore_cns   SMALLINT

SELECT NVL(cod_siefore_cns,0)
INTO  ls_siefore_cns
FROM   tab_siefore_local
WHERE  codigo_siefore >= 14
AND    codigo_siefore = ls_siefore


IF ls_siefore_cns IS NULL THEN 
   LET ls_siefore_cns = 0 
END IF 

RETURN ls_siefore_cns
END FUNCTION


#Rellena una cadena de caracteres con 0
FUNCTION rellenar_diagnostico(p_diagnostico)
    DEFINE p_diagnostico     CHAR(03)
    DEFINE r_diagnostico    CHAR(04)
    
    LET r_diagnostico = p_diagnostico CLIPPED
    
    WHILE LENGTH(r_diagnostico) < 4
        LET r_diagnostico = "0" || r_diagnostico
    END WHILE
    
    RETURN r_diagnostico
END FUNCTION