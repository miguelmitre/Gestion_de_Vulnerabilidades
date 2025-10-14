#############################################################################
#Proyecto          => AFORES ( MEXICO )                                     #
#Propietario       => E.F.P.                                                #
#Programa RETL837  => LANZADOR DE LOS ANEXOS 114 Y 115                      #
#Sistema           => RET                                                   #
#Autor             => David Miguel Garibay Rivera                           #
#Fecha             => 27 NOVIEMBRE  DE 2012                                 #
#Modificaciones    => Javier Gonzalez Jeronimo                              #
#Fecha             => 3 DE ENERO DE 2013                                    #
#                  => Se realizaron los siguientes ajustes :                #
#                     - Unificacion de versiones en todas las afores        #
#                     - Reacomodo del codigo. Se declararon funciones       #
#                       para facilitar el manteminimento del programa       #
#                     - Se incluyeron las funciones de las librerias de     #
#                       Retiros                                             #
#############################################################################

DATABASE safre_af

GLOBALS

    DEFINE gr_seg_modulo RECORD LIKE seg_modulo.*

    DEFINE gr_afores RECORD
        issste          SMALLINT    ,
        metlife         SMALLINT 
    END RECORD 

    DEFINE
       HOY                      DATE

    DEFINE
        enter                   CHAR(1) ,
        gc_usuario              CHAR(20)  
        
    DEFINE
        gs_codigo_afore         SMALLINT

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP              ,
        PROMPT LINE LAST        ,
        ACCEPT KEY CONTROL-I

    CALL f_lib_crea_log()  
    CALL init()                    
    CALL f_captura_periodo()

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    -- -----------------------------------------------------------------------------
    
    -- Inicialización de variables globales
    LET HOY                 = TODAY
    LET gc_usuario          = f_lib_obten_user()

    SELECT codigo_afore
    INTO   gs_codigo_afore
    FROM   tab_afore_local
    GROUP BY 1

    SELECT *
    INTO   gr_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    SELECT afore_cod
    INTO   gr_afores.issste
    FROM   tab_afore
    WHERE  UPPER(afore_desc) MATCHES "*PENSIONISSSTE*"
    
    SELECT afore_cod
    INTO   gr_afores.metlife
    FROM   tab_afore
    WHERE  UPPER(afore_desc) MATCHES "*METLIFE*"

END FUNCTION

#---------------------------------------------------------------------------#
# f_captura_periodo : Función que le permite al usuario capturar un rango   #
#                     de fechas que serviran como criterios de busqueda     #
#                     para los anexos 114 y 115                             #
#---------------------------------------------------------------------------#
FUNCTION f_captura_periodo()

    DEFINE lr_fechas RECORD
        fecha_ini           DATE    , 
        fecha_fin           DATE
    END RECORD 

    DEFINE
        ls_bnd_captura              ,
        ls_bnd_fecha                SMALLINT
       
    DEFINE
        lc_cad_msg                  CHAR(75)
           
    -- -----------------------------------------------------------------------------
    
    -- Inicialización de varibales
    INITIALIZE lr_fechas.* TO NULL
    LET ls_bnd_captura = 0
    LET ls_bnd_fecha   = 0

    OPEN WINDOW w_RETL837 AT 2,2 WITH FORM "RETL8371"  ATTRIBUTE(BORDER)

    DISPLAY " Ctrl-C : Salir                                             Esc : Ejecutar " AT 1,2 ATTRIBUTE(REVERSE)
    DISPLAY " RETL837            GENERACIÓN DE LOS ANEXOS 114 Y 115                     " AT 3,2 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)
   
    IF (INT_FLAG = TRUE) THEN
        LET INT_FLAG = FALSE
    END IF
   
    INPUT BY NAME lr_fechas.* WITHOUT DEFAULTS
       
        AFTER FIELD fecha_ini
            CALL f_lib_valida_fechas(lr_fechas.fecha_ini) RETURNING ls_bnd_fecha, lc_cad_msg
            
            IF ls_bnd_fecha = 1 THEN
                CALL f_lib_error_msg(lc_cad_msg)
                NEXT FIELD fecha_ini
            END IF
         
        AFTER FIELD fecha_fin
            CALL f_lib_valida_fechas(lr_fechas.fecha_fin) RETURNING ls_bnd_fecha, lc_cad_msg
            
            IF ls_bnd_fecha = 1 THEN
                CALL f_lib_error_msg(lc_cad_msg)
                NEXT FIELD fecha_fin
            ELSE
                IF (lr_fechas.fecha_ini > lr_fechas.fecha_fin) THEN
                    LET lr_fechas.fecha_ini = NULL
                    CALL f_lib_error_msg("ERROR: FECHA INICIAL MAYOR A LA FINAL")
                    NEXT FIELD fecha_ini
                END IF
            END IF
         
        ON KEY (ESC)
            -- Valida la fechas
            CALL f_lib_valida_fechas(lr_fechas.fecha_ini) RETURNING ls_bnd_fecha, lc_cad_msg
            
            IF ls_bnd_fecha = 1 THEN
                CALL f_lib_error_msg(lc_cad_msg)
                NEXT FIELD fecha_ini
            END IF
            
            CALL f_lib_valida_fechas(lr_fechas.fecha_fin) RETURNING ls_bnd_fecha, lc_cad_msg
            
            IF ls_bnd_fecha = 1 THEN
                CALL f_lib_error_msg(lc_cad_msg)
                NEXT FIELD fecha_fin
            ELSE
                IF (lr_fechas.fecha_ini > lr_fechas.fecha_fin) THEN
                    LET lr_fechas.fecha_ini = NULL
                    CALL f_lib_error_msg("ERROR: FECHA INICIAL MAYOR A LA FINAL")
                    NEXT FIELD fecha_ini
                END IF
            END IF
            
            LET ls_bnd_captura = 1  -- Las fechas son válidas
            
            -- Solicitar confirmación al usuario 
            WHILE TRUE
                PROMPT "¿DESEA GENERAR LOS ANEXOS? (S/N) : " ATTRIBUTE(REVERSE) FOR enter
                IF ( enter MATCHES "[sSnN]" ) THEN
                    IF ( enter MATCHES "[sS]" ) THEN
                        CALL f_genera_anexos(lr_fechas.*)
                        EXIT INPUT
                    ELSE
                        CALL f_lib_error_msg("PROCESO CANCELADO")
                        EXIT INPUT
                    END IF
                END IF
            END WHILE

            EXIT INPUT
         
        ON KEY (INTERRUPT, CONTROL-C)
            LET ls_bnd_captura = 0
            LET INT_FLAG = FALSE
            CALL f_lib_error_msg("PROCESO CANCELADO")
            EXIT INPUT
       
    END INPUT 
   
    CLOSE WINDOW w_RETL837
   
END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_anexos : Funcion que lanza los programas RETL837A y RETL837B,    #
#                   los cuales generan los anexos 114 y 115 respectivamente #
#---------------------------------------------------------------------------#
FUNCTION f_genera_anexos(pdt_fecha_ini, pdt_fecha_fin)

    DEFINE
        pdt_fecha_ini                   ,
        pdt_fecha_fin                   DATE

    DEFINE
        lc_comando                      CHAR(500)   ,
        lc_nom_114                      CHAR(024)   ,
        lc_nom_115                      CHAR(024)   ,
        lc_nom_rpt_control_703          CHAR(023)   , 
        lc_nom_rpt_control_704          CHAR(023)   ,
        lc_nom_rpt_detalle_703          CHAR(023)   , 
        lc_nom_rpt_detalle_704          CHAR(023)

    -- -----------------------------------------------------------------------------

    -- Asigna los nombres de los archivos generados por los anexos
    LET lc_nom_114              = HOY USING "YYYYMMDD", "_AF_", gs_codigo_afore USING "&&&", "_000.0703"
    LET lc_nom_115              = HOY USING "YYYYMMDD", "_AF_", gs_codigo_afore USING "&&&", "_000.0704"
    LET lc_nom_rpt_control_703  = HOY USING "YYYYMMDD", "Control0703.txt"
    LET lc_nom_rpt_control_704  = HOY USING "YYYYMMDD", "Control0704.txt"
    LET lc_nom_rpt_detalle_703  = HOY USING "YYYYMMDD", "Detalle0703.txt"
    LET lc_nom_rpt_detalle_704  = HOY USING "YYYYMMDD", "Detalle0704.txt"
   
    -- Anexo 114 y Reporte de cifras de control
    LET lc_comando  = " "
    LET lc_comando  = "fglgo RETL837A ", pdt_fecha_ini          , " ",  -- Fecha inicial
                                         pdt_fecha_fin          , " ",  -- Fecha final
                                         lc_nom_114 CLIPPED     , " ",  -- Nombre del anexo 114
                                         lc_nom_rpt_control_703         -- Nombre del reporte de cifras Control
    LET lc_comando  = lc_comando CLIPPED
    RUN lc_comando

    -- Anexo 115 y Reporte de cifras de control
    LET lc_comando  = " "
    LET lc_comando  = "fglgo RETL837B ", pdt_fecha_ini          , " ",  -- Fecha inicial
                                         pdt_fecha_fin          , " ",  -- Fecha final
                                         lc_nom_115 CLIPPED     , " ",  -- Nombre del anexo 115
                                         lc_nom_rpt_control_704         -- Nombre del reporte de cifras Control
    LET lc_comando  = lc_comando CLIPPED
    RUN lc_comando

    -- Genera reportes de detalle
    IF (gs_codigo_afore = gr_afores.issste) OR (gs_codigo_afore = gr_afores.metlife) THEN
        LET lc_comando  = " "
        LET lc_comando  = "fglgo RETL837C ", pdt_fecha_ini, " ", pdt_fecha_fin
        LET lc_comando  = lc_comando CLIPPED
        RUN lc_comando
    END IF 

    -- Despliega los resultados
    DISPLAY "ARCHIVOS GENERADOS : " AT 10,5
    
    DISPLAY "ANEXOS" AT 11,5
    DISPLAY gr_seg_modulo.ruta_envio    CLIPPED, "/", lc_nom_114 CLIPPED AT 12,12
    DISPLAY gr_seg_modulo.ruta_envio    CLIPPED, "/", lc_nom_115 CLIPPED AT 13,12
    
    IF (gs_codigo_afore = gr_afores.issste) OR (gs_codigo_afore = gr_afores.metlife) THEN
        DISPLAY "CIFRAS DE CONTROL" AT 14,5
        DISPLAY gr_seg_modulo.ruta_listados CLIPPED, "/", lc_nom_rpt_control_703 CLIPPED AT 15,12
        DISPLAY gr_seg_modulo.ruta_listados CLIPPED, "/", lc_nom_rpt_control_704 CLIPPED AT 16,12
        
        DISPLAY "DETALLE DE REGISTROS" AT 17,5
        DISPLAY gr_seg_modulo.ruta_listados CLIPPED, "/", lc_nom_rpt_detalle_703 CLIPPED AT 18,12
        DISPLAY gr_seg_modulo.ruta_listados CLIPPED, "/", lc_nom_rpt_detalle_704 CLIPPED AT 19,12
    END IF
    
    CALL f_lib_error_msg("PROCESO FINALIZADO")

END FUNCTION
