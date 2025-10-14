#################################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                               #
#Owner             => E.F.P.                                                    #
#Programa RETC817  => GENERA LOTE DE DISPOSICION                                #
#Fecha creacion    => 10 DE FEBRERO DEL 2004                                    #
#By                => FRANCO ESTEBAN ULLOA VIDELA                               #
#Fecha actualiz.   => 27 DE AGOSTO DE 2013                                      #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                  => - Arreglo del codigo fuente, aplicacion de librerias      #
#                     - Se modifica la fecha de pago del archivo de dos a tres  #
#                       dias habiles despues del envio                          #
#                     Req. MLM-2115                                             #
#                          CPL-1423                                             #
#Sistema           => RET                                                       #
#################################################################################
DATABASE safre_af

GLOBALS
    DEFINE gr_seg_modulo RECORD LIKE seg_modulo.*

    DEFINE gr_captura RECORD
        fecha_envio           DATE
    END RECORD

    DEFINE gr_estado RECORD
        procesado   LIKE ret_estado.estado_solicitud    ,
        enviado     LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE
        gc_nombre_archivo       CHAR(012) ,
        gc_usuario              CHAR(015) ,
        gc_ruta_cza             CHAR(100) ,
        gc_ruta_det             CHAR(100) ,
        gc_ruta_sum             CHAR(100) ,
        enter                   CHAR(001) 


    DEFINE
        HOY                     DATE

    DEFINE
        gs_tipo_disposicion     ,
        gs_procesa              ,
        gs_dias_habiles         ,
        gs_codigo_afore         SMALLINT

END GLOBALS


MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL f_lib_abre_log("RETC817")
    CALL init()

    CALL f_captura_fecha() RETURNING gr_captura.*, gs_procesa

    IF gs_procesa = 1 THEN
        LET gc_nombre_archivo   = gr_captura.fecha_envio USING "YYYYMMDD",".04D"

        CALL primer_paso()  -- Genera encabezado transaccional
        CALL segundo_paso() -- Genera detalle
        CALL tercer_paso()  -- Genera sumario transaccional
        CALL cuarto_paso()  -- Actualizaciones
    END IF

    CLOSE WINDOW retc8171

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    DEFINE
        lc_prepare      CHAR(300)

    -- -----------------------------------------------------------------------------

    LET HOY             = TODAY
    LET gc_usuario      = f_lib_obten_user()
    LET gs_dias_habiles = 3

    SELECT cod_tramite
    INTO   gs_tipo_disposicion
    FROM   tab_tipo_tramite
    WHERE  descripcion = "DISPOSICION"

    ----- CODIGOS AFORES -----
    SELECT codigo_afore
    INTO   gs_codigo_afore
    FROM   tab_afore_local

    ----- RUTAS DE ARCHIVOS -----
    SELECT *
    INTO   gr_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    ----- ESTADOS DE SOLICITUD -----
    SELECT estado_solicitud
    INTO   gr_estado.procesado
    FROM   ret_estado
    WHERE  descripcion = "PROCESADO"

    SELECT estado_solicitud
    INTO   gr_estado.enviado
    FROM   ret_estado
    WHERE  descripcion = "ENVIADO"

    ----- DIA HABIL POSTERIOR -----
    LET lc_prepare = " EXECUTE FUNCTION fn_habil_siguiente(?,?) "
    PREPARE eje_habil_siguiente FROM lc_prepare

    LET lc_prepare = " "


END FUNCTION

#---------------------------------------------------------------------------#
# f_captura_fecha : Captura la fecha de envio del archivo de la operacion 5 #
#---------------------------------------------------------------------------#
FUNCTION f_captura_fecha()

    DEFINE lr_captura RECORD
        fecha_envio           DATE
    END RECORD

    DEFINE
        ls_procesa              SMALLINT

    -- -----------------------------------------------------------------------------

    OPEN WINDOW retc8171 AT 4,4 WITH FORM "RETC8171" ATTRIBUTE(BORDER)
    DISPLAY " <ESC> - Ejecutar                                         <CTRL-C> - Salir   " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC817       GENERACION DEL LOTE DE DISPOSICIONES IMSS                     " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    LET ls_procesa              = 0
    LET lr_captura.fecha_envio  = HOY

    INPUT BY NAME lr_captura.* WITHOUT DEFAULTS

        #-----
        AFTER FIELD fecha_envio
            IF lr_captura.fecha_envio IS NULL OR lr_captura.fecha_envio < TODAY THEN
                CALL f_lib_error_msg("FECHA INCORRECTA")
                NEXT FIELD fecha_envio
            END IF

            -- Lote generado en la fecha capturada
            SELECT "OK"
            FROM   ret_ctr_envio_lote
            WHERE  tipo_retiro IN (SELECT tipo_retiro
                                   FROM   tab_tramite_retiro
                                   WHERE  cod_tramite = gs_tipo_disposicion
                                  )
            AND    estado       = gr_estado.enviado
            AND    fecha_envio  = lr_captura.fecha_envio
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
                CALL f_lib_error_msg("YA SE GENERO UN LOTE EN ESA FECHA")
                NEXT FIELD fecha_envio
            END IF

            -- Valida si existen registros
            SELECT "OK"
            FROM   ret_ctr_envio_lote
            WHERE  tipo_retiro IN (SELECT tipo_retiro
                                   FROM   tab_tramite_retiro
                                   WHERE  cod_tramite = gs_tipo_disposicion
                                  )
            AND    estado       = gr_estado.procesado
            AND    fecha_envio IS NULL
            GROUP BY 1

            IF STATUS = NOTFOUND THEN
                CALL f_lib_error_msg("NO EXISTEN REGISTROS PARA PROCESAR")
                NEXT FIELD fecha_envio
            END IF

        #-----
        ON KEY (ESC)
            IF lr_captura.fecha_envio IS NULL OR lr_captura.fecha_envio < TODAY THEN
                CALL f_lib_error_msg("FECHA INCORRECTA")
                NEXT FIELD fecha_envio
            END IF

            -- Lote generado en la fecha capturada
            SELECT "OK"
            FROM   ret_ctr_envio_lote
            WHERE  tipo_retiro IN (SELECT tipo_retiro
                                   FROM   tab_tramite_retiro
                                   WHERE  cod_tramite = gs_tipo_disposicion
                                  )
            AND    estado       = gr_estado.enviado
            AND    fecha_envio  = lr_captura.fecha_envio
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
                CALL f_lib_error_msg("YA SE GENERO UN LOTE EN ESA FECHA")
                NEXT FIELD fecha_envio
            END IF

            -- Valida si existen registros
            SELECT "OK"
            FROM   ret_ctr_envio_lote
            WHERE  tipo_retiro IN (SELECT tipo_retiro
                                   FROM   tab_tramite_retiro
                                   WHERE  cod_tramite = gs_tipo_disposicion
                                  )
            AND    estado       = gr_estado.procesado
            AND    fecha_envio IS NULL
            GROUP BY 1

            IF STATUS = NOTFOUND THEN
                CALL f_lib_error_msg("NO EXISTEN REGISTROS PARA PROCESAR")
                NEXT FIELD fecha_envio
            END IF

            WHILE TRUE
                PROMPT "¿DESEA GENERAR EL LOTE DE DISPOSICIONES? (S/N) " FOR enter

                IF enter MATCHES "[sSnN]" THEN
                    IF enter MATCHES "[sS]" THEN
                        DISPLAY " PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)
                        LET ls_procesa = 1
                    ELSE
                        CALL f_lib_error_msg("PROCESO CANCELADO")
                        LET ls_procesa = 0
                    END IF

                    EXIT INPUT
                END IF
            END WHILE

        #-----
        ON KEY (CONTROL-C, INTERRUPT)
            CALL f_lib_error_msg("PROCESO CANCELADO")
            LET ls_procesa = 0
            EXIT INPUT

    END INPUT

    RETURN lr_captura.*, ls_procesa

END FUNCTION

#---------------------------------------------------------------------------#
# primer_paso : Genera el encabezado transaccional del archivo de la        #
#               operacion 05 de disposiciones IMSS                          #
#---------------------------------------------------------------------------#
FUNCTION primer_paso()

    -- -----------------------------------------------------------------------------

    LET gc_ruta_cza = gr_seg_modulo.ruta_envio CLIPPED  ,
                      "/"                               ,
                      gc_usuario CLIPPED                ,
                      "_"                               ,
                      "CZADIS"

    START REPORT rpt_cza_tran TO gc_ruta_cza
        OUTPUT TO REPORT rpt_cza_tran()
    FINISH REPORT rpt_cza_tran

END FUNCTION

#---------------------------------------------------------------------------#
# segundo_paso : Concatena los archivos generados en la provision para      #
#                armar el detalle del archivo de la operacion 05 de         #
#                disposiciones IMSS                                         #
#---------------------------------------------------------------------------#
FUNCTION segundo_paso()

    DEFINE lc_archivo_det LIKE tab_retiro.nom_archivo

    DEFINE
        lc_comando              CHAR(1000)  ,
        lc_ruta_paso            CHAR(300)


    -- -----------------------------------------------------------------------------

    INITIALIZE lc_archivo_det TO NULL
    LET lc_comando  = " "

    DECLARE cur_det CURSOR FOR
        SELECT UNIQUE(C.nom_archivo)
        FROM   ret_ctr_envio_lote A , 
               tab_tramite_retiro B , 
               tab_retiro C
        WHERE  A.tipo_retiro    = B.tipo_retiro
        AND    A.estado         = gr_estado.procesado
        AND    B.cod_tramite    = gs_tipo_disposicion
        AND    A.tipo_retiro    = C.tipo_retiro
        ORDER BY 1

    FOREACH cur_det INTO lc_archivo_det
        LET lc_ruta_paso    = gr_seg_modulo.ruta_envio CLIPPED  ,
                              "/"                               ,
                              lc_archivo_det
                              
        LET lc_comando      = lc_comando CLIPPED, " ", lc_ruta_paso
    END FOREACH

    LET gc_ruta_det = gr_seg_modulo.ruta_envio CLIPPED  ,
                      "/"                               ,
                      gc_usuario CLIPPED                ,
                      "_"                               ,
                      "DETDIS"                            

    LET lc_comando = "cat ", lc_comando CLIPPED ,
                     " > "                      ,
                     gc_ruta_det CLIPPED

    RUN lc_comando

END FUNCTION

#---------------------------------------------------------------------------#
# tercer_paso : Genera el sumario del archivo de la operacion 05 de         #
#               disposiciones IMSS                                          #
#---------------------------------------------------------------------------#
FUNCTION tercer_paso()

    -- -----------------------------------------------------------------------------
    
    LET gc_ruta_sum = gr_seg_modulo.ruta_envio CLIPPED  ,
                      "/"                               ,
                      gc_usuario CLIPPED                ,
                      "_"                               ,
                      "SUMDIS"

    START REPORT rpt_sumario TO gc_ruta_sum
        OUTPUT TO REPORT rpt_sumario()
    FINISH REPORT rpt_sumario
    
END FUNCTION

#---------------------------------------------------------------------------#
# cuarto_paso : Concatena el archivo final de la operacion 05 y actualiza   #
#               las tablas de proceso                                       #
#---------------------------------------------------------------------------#
FUNCTION cuarto_paso()

    DEFINE
        lc_comando              CHAR(1000)
   
    -- -----------------------------------------------------------------------------
    
    WHENEVER ERROR CONTINUE

        LET lc_comando  = "chmod 777 ", gc_ruta_cza CLIPPED
        RUN lc_comando
        LET lc_comando  = " "

        LET lc_comando  = "chmod 777 ", gc_ruta_det CLIPPED
        RUN lc_comando
        LET lc_comando  = " "        

        LET lc_comando  = "chmod 777 ", gc_ruta_sum CLIPPED
        RUN lc_comando
        LET lc_comando  = " " 
        
    WHENEVER ERROR STOP

    LET lc_comando  = " cat ", gc_ruta_cza CLIPPED, " ",
                               gc_ruta_det CLIPPED, " ",
                               gc_ruta_sum CLIPPED, " ",
                      " > ", gr_seg_modulo.ruta_envio CLIPPED, "/", gc_nombre_archivo

    RUN lc_comando
    LET lc_comando  = " " 

    WHENEVER ERROR CONTINUE
    
        LET lc_comando  = "chmod 777 ", gr_seg_modulo.ruta_envio CLIPPED, "/", gc_nombre_archivo
        RUN lc_comando
        LET lc_comando  = " "
        
    WHENEVER ERROR STOP


    UPDATE ret_ctr_envio_lote
    SET    estado           = gr_estado.enviado         ,
           fecha_envio      = gr_captura.fecha_envio    ,
           hora_envio       = CURRENT HOUR TO MINUTE    ,
           usuario_envio    = gc_usuario
    WHERE  estado           = gr_estado.procesado
    AND    tipo_retiro IN (SELECT tipo_retiro
                           FROM   tab_tramite_retiro
                           WHERE  cod_tramite = gs_tipo_disposicion
                          )

    UPDATE ret_solicitud_tx
    SET    estado_solicitud = gr_estado.enviado
    WHERE  folio IN (SELECT folio
                     FROM   ret_ctr_envio_lote
                     WHERE  fecha_envio = gr_captura.fecha_envio
                     AND    tipo_retiro IN (SELECT tipo_retiro
                                            FROM   tab_tramite_retiro
                                            WHERE  cod_tramite = gs_tipo_disposicion
                                           )
                    )

    DISPLAY "                                                       " AT 10, 10

    DISPLAY " EL LOTE HA SIDO GENERADO EN LA RUTA : " AT 6,19
    DISPLAY gr_seg_modulo.ruta_envio CLIPPED AT 8,20
    DISPLAY " CON EL NOMBRE : ", gc_nombre_archivo AT 10,19

    CALL f_lib_error_msg("ARCHIVO FINALIZADO CORRECTAMENTE")
                   
END FUNCTION

#---------------------------------------------------------------------------#
# rpt_cza_tran : Reporte que genera el archivo plano del encabezado         #
#                transaccional para la operacion 05 de disposiciones IMSS   #
#---------------------------------------------------------------------------#
REPORT rpt_cza_tran()

    DEFINE
        ldt_transfer        DATE

    -- -----------------------------------------------------------------------------

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW

        EXECUTE eje_habil_siguiente USING gr_captura.fecha_envio    ,
                                          gs_dias_habiles
                                    INTO  ldt_transfer

    PRINT
        COLUMN 001, "01"                                    ,
        COLUMN 003, "04"                                    ,
        COLUMN 005, "01"                                    ,
        COLUMN 007, gs_codigo_afore USING "&&&"             ,
        COLUMN 010, "03"                                    ,
        COLUMN 012, "001"                                   ,
        COLUMN 015, gr_captura.fecha_envio USING "YYYYMMDD" ,
        COLUMN 023, ldt_transfer USING "YYYYMMDD"           ,
        COLUMN 031,   2 SPACES                              , -- Resultado de la operacion
        COLUMN 033,   3 SPACES                              , -- Motivo de rechazo 1
        COLUMN 036,   3 SPACES                              , -- Motivo de rechazo 2
        COLUMN 039,   3 SPACES                              , -- Motivo de rechazo 3
        COLUMN 042, 739 SPACES                                -- Filler
                                                              -- Se modifica para hacer la longitud de acuerdo al cambio de layout
END REPORT

#---------------------------------------------------------------------------#
# rpt_sumario : Reporte que genera el archivo plano del sumario para la     #
#               operacion 05 de disposiciones IMSS                          #
#---------------------------------------------------------------------------#
REPORT rpt_sumario()

    DEFINE
        li_tot_registros        INTEGER

    -- -----------------------------------------------------------------------------

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
            LET li_tot_registros = 0

            SELECT SUM(A.total_registros)
            INTO   li_tot_registros
            FROM   ret_ctr_envio_lote A , 
                   tab_tramite_retiro B , 
                   tab_retiro C
            WHERE  A.tipo_retiro    = B.tipo_retiro
            AND    A.estado         = gr_estado.procesado
            AND    B.cod_tramite    = gs_tipo_disposicion
            AND    A.tipo_retiro    = C.tipo_retiro

        PRINT
            COLUMN 001, "09"                                    ,
            COLUMN 003, "04"                                    ,
            COLUMN 005, "01"                                    ,
            COLUMN 007, gs_codigo_afore USING "&&&"             ,
            COLUMN 010, "03"                                    ,
            COLUMN 012, "001"                                   ,
            COLUMN 015, gr_captura.fecha_envio USING "YYYYMMDD" ,
            COLUMN 023, li_tot_registros USING "&&&&&&"         ,
            COLUMN 029, 752 SPACES                              -- Se modifica para hacer la longitud de acuerdo al cambio de layout

END REPORT
