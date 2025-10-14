################################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                              #
#Owner             => E.F.P.                                                   #
#Programa RETC818  => GENERA LOTE DE TRANSFERENCIA                             #
#Fecha creacion    => 05 DE ENERO DEL 2004                                     #
#By                => FRANCO ESTEBAN ULLOA VIDELA                              #
#Fecha actualiz.   => 21 MAYO 2004                                             #
#Actualizacion     => FRANCO ESTEBAN ULLOA VIDELA                              #
#Sistema           => RET                                                      #
################################################################################
DATABASE safre_af

GLOBALS
    DEFINE g_seg_modulo RECORD LIKE seg_modulo.*

    DEFINE gr_edo RECORD
        procesado             LIKE ret_estado.estado_solicitud ,
        enviado               LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE gr_input RECORD
        folio       LIKE ret_transf_rx.folio    ,
        fec_envio   DATE
    END RECORD
    
    DEFINE
        gs_cod_afore            SMALLINT

    DEFINE
        gc_usuario            CHAR(012) ,
        c12_nom_plano         CHAR(012) ,
        G_LISTA_CZA           CHAR(100) ,
        G_LISTA_SUM           CHAR(100) ,
        G_LISTA               CHAR(100) ,
        HORA                  CHAR(005) ,
        enter                 CHAR(001) ,
        cat                   CHAR(500) ,
        ch2                   CHAR(300) ,
        ch3                   CHAR(300) ,
        ch4                   CHAR(300) ,
        ch5                   CHAR(300)

    DEFINE
        gdt_valor_trans         ,
        gdt_operacion           ,
        HOY                     DATE

END GLOBALS


MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("RETC818.log")

    CALL init()
    
    OPEN WINDOW retc8181 AT 3,3 WITH FORM "RETC8181" ATTRIBUTE(BORDER)
    DISPLAY " < CTRL-C > Salir                                                               " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC818               GENERA LOTE DE TRANSFERENCIA                             " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    INPUT BY NAME gr_input.* WITHOUT DEFAULTS

        AFTER FIELD folio
            IF gr_input.folio IS NULL THEN
                CALL f_error_msg("EL FOLIO NO PUEDE SER NULO")
                NEXT FIELD folio
            ELSE
                SELECT "OK"
                FROM   ret_cza_lote A
                WHERE  A.folio = gr_input.folio
                AND    A.folio IN ( SELECT B.folio 
                                    FROM   ret_transf_rx B
                                    WHERE  B.estado_solicitud = gr_edo.enviado )
                
                IF STATUS <> NOTFOUND THEN
                    LET gr_input.folio = 0
                    CALL f_error_msg("EL FOLIO Y LOTE YA GENERADO CON ANTERIORIDAD")
                    NEXT FIELD folio
                ELSE
                    SELECT "OK"
                    FROM   ret_cza_lote A
                    WHERE  A.folio  = gr_input.folio
                    AND    A.folio  IN (SELECT B.folio 
                                        FROM   ret_transf_rx B
                                        WHERE  B.estado_solicitud = gr_edo.procesado)
                    
                    IF STATUS = NOTFOUND THEN
                        CALL f_error_msg("NO EXISTEN REGISTROS PARA GENERAR LOTE")
                        NEXT FIELD folio
                    END IF
                END IF
            END IF

        AFTER FIELD fec_envio
            IF gr_input.fec_envio  IS NULL THEN
                CALL f_error_msg("LA FECHA DE ENVIO NO PUEDE SER NULA")
                NEXT FIELD fec_envio
            ELSE
                IF gr_input.fec_envio < HOY THEN
                    CALL f_error_msg("FECHA INCORRECTA")
                    NEXT FIELD fec_envio
                END IF
            END IF

            SELECT "OK"
            FROM   ret_ctr_envio_lote
            WHERE  tipo_retiro IN(SELECT tipo_retiro
                                  FROM   tab_tramite_retiro
                                  WHERE  cod_tramite = 1 --transferencia
                                  )
            AND    fecha_envio  = gr_input.fec_envio
            AND    estado       = gr_edo.enviado
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
                LET gr_input.folio = 0
                CALL f_error_msg("EL LOTE YA FUE GENERADO CON ANTERIORIDAD")
                NEXT FIELD fec_envio
            END IF

            DISPLAY BY NAME gr_input.folio

            WHILE TRUE
                PROMPT "¿ DESEA GENERAR EL LOTE DE TRANSFERENCIAS ? (S/N) " FOR CHAR enter
                IF enter MATCHES "[sSnN]" THEN
                    IF enter MATCHES "[sS]" THEN
                        EXIT WHILE
                    ELSE
                        PROMPT " PROCESO CANCELADO... PRESIONE <ENTER> PARA SALIR " ATTRIBUTE(REVERSE) FOR CHAR enter
                        EXIT PROGRAM
                    END IF
                END IF
            END WHILE

            DISPLAY " PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)
            SLEEP 1

            LET  c12_nom_plano = gr_input.fec_envio USING "YYYYMMDD",".04T"
            
            CALL primer_paso()  -- Genera encabezado transaccional
            CALL segundo_paso() -- Genera detalle
            CALL tercer_paso()  -- Genera sumario transaccional

            LET cat = "cat ",g_seg_modulo.ruta_envio CLIPPED,"/CZATRANSF ",
                             g_seg_modulo.ruta_envio CLIPPED,"/DETTRANSF ",
                             g_seg_modulo.ruta_envio CLIPPED,"/SUMTRANSF ",
                             "> ",g_seg_modulo.ruta_envio CLIPPED,"/",c12_nom_plano
            RUN cat

            WHENEVER ERROR CONTINUE
            LET ch2= "chmod 777 ",g_seg_modulo.ruta_envio CLIPPED,"/CZATRANSF"
            LET ch3= "chmod 777 ",g_seg_modulo.ruta_envio CLIPPED,"/DETTRANSF"
            LET ch4= "chmod 777 ",g_seg_modulo.ruta_envio CLIPPED,"/SUMTRANSF"
            LET ch5= "chmod 777 ",g_seg_modulo.ruta_envio CLIPPED,"/",c12_nom_plano
            RUN ch2
            RUN ch3
            RUN ch4
            RUN ch5
            WHENEVER ERROR STOP

            CALL cuarto_paso() #Actualizaciones

            EXIT INPUT

        #---
        ON KEY (INTERRUPT, CONTROL-C)
            PROMPT " PROCESO CANCELADO... PRESIONE <ENTER> PARA SALIR " ATTRIBUTE(REVERSE) FOR CHAR enter
            EXIT PROGRAM        

    END INPUT

    DISPLAY "                                                                           " AT 11,1
    DISPLAY " EL LOTE HA SIDO GENERADO EN LA RUTA : " AT 9,19
    DISPLAY G_LISTA CLIPPED AT 11,20
    DISPLAY "                                                " AT 19,1
    DISPLAY " CON EL NOMBRE : ",c12_nom_plano AT 13,19

    PROMPT " PROCESO CONCLUIDO ... PRESIONE <ENTER> PARA FINALIZAR " ATTRIBUTE(REVERSE) FOR CHAR enter
    CLOSE WINDOW retc8181

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    LET HOY  = TODAY
    LET HORA = TIME

    SELECT *
    INTO   g_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    ----- CODIGOS AFORES -----
    SELECT codigo_afore ,
           USER
    INTO   gs_cod_afore ,
           gc_usuario
    FROM   tab_afore_local

    LET gr_input.fec_envio  = HOY
    LET G_LISTA             = g_seg_modulo.ruta_envio

    ----- ESTADOS DE S0LICITUD -----
    SELECT estado_solicitud
    INTO   gr_edo.procesado
    FROM   ret_estado
    WHERE  descripcion = "PROCESADO"

    SELECT estado_solicitud
    INTO   gr_edo.enviado
    FROM   ret_estado
    WHERE  descripcion = "ENVIADO"

END FUNCTION

#---------------------------------------------------------------------------#
# primer_paso : Ejecuta el reporte para generar el encabezado del lote      #
#---------------------------------------------------------------------------#
FUNCTION primer_paso()

    LET G_LISTA_CZA = g_seg_modulo.ruta_envio CLIPPED,"/CZATRANSF"

    START REPORT rpt_cza_tran TO G_LISTA_CZA
        OUTPUT TO REPORT rpt_cza_tran() #ct
    FINISH REPORT rpt_cza_tran

END FUNCTION

#---------------------------------------------------------------------------#
# segundo_paso : Concatena los archivos de detalle generados en la provision#
#---------------------------------------------------------------------------#
FUNCTION segundo_paso()

    DEFINE lc_nom_arch LIKE tab_retiro.nom_archivo

    DEFINE
        lc_paso_tmp         CHAR(500)

    -- -----------------------------------------------------------------------------

    DECLARE cur_1 CURSOR FOR
    SELECT C.nom_archivo
    FROM   ret_ctr_envio_lote A , 
           tab_tramite_retiro B , 
           tab_retiro C
    WHERE  A.tipo_retiro    = B.tipo_retiro
    AND    A.estado         = 2
    AND    B.cod_tramite    = 1
    AND    A.tipo_retiro    = C.tipo_retiro
    ORDER BY 1
    
    FOREACH cur_1 INTO lc_nom_arch
        LET lc_paso_tmp = g_seg_modulo.ruta_envio CLIPPED, "/", lc_nom_arch
        LET cat         = cat CLIPPED, " ", lc_paso_tmp
    END FOREACH
    
    LET cat = "cat ",cat CLIPPED," > ",g_seg_modulo.ruta_envio CLIPPED, "/DETTRANSF"
    RUN cat

END FUNCTION

#---------------------------------------------------------------------------#
# tercer_paso : Ejecuta el reporte para generar el sumario del lote         #
#---------------------------------------------------------------------------#
FUNCTION tercer_paso()
    
    LET G_LISTA_SUM = g_seg_modulo.ruta_envio CLIPPED,"/SUMTRANSF"

    START REPORT sum_tran TO G_LISTA_SUM
        OUTPUT TO REPORT sum_tran()
    FINISH REPORT sum_tran

END FUNCTION

#---------------------------------------------------------------------------#
# cuarto_paso : Actualiza las tablas del proceso y elimina los archivos     #
#               temporales utilizados                                       #
#---------------------------------------------------------------------------#
FUNCTION cuarto_paso()
#cp------------------
    DEFINE 
        lc_delete           CHAR(100)
    
    -- -----------------------------------------------------------------------------
    
    UPDATE ret_ctr_envio_lote
    SET    estado           = gr_edo.enviado       ,
           fecha_envio      = gr_input.fec_envio   ,
           hora_envio       = HORA                 ,
           usuario_envio    = gc_usuario
    WHERE  folio            = gr_input.folio

    UPDATE ret_transf_rx
    SET    estado_solicitud = gr_edo.enviado
    WHERE  folio            = gr_input.folio

    LET lc_delete = "rm ",g_seg_modulo.ruta_envio,"/DET-A"
    RUN lc_delete
    LET lc_delete = " "
    
    LET lc_delete = "rm ",g_seg_modulo.ruta_envio,"/DET-B"
    RUN lc_delete
    LET lc_delete = " "
    
    LET lc_delete = "rm ",g_seg_modulo.ruta_envio,"/DET-C"
    RUN lc_delete
    LET lc_delete = " "

END FUNCTION

#---------------------------------------------------------------------------#
# f_error_msg : Formatea y despliega los mensajes de error en la pantalla   #
#---------------------------------------------------------------------------#
FUNCTION f_error_msg(pc_mensaje)

    DEFINE
        pc_mensaje          CHAR(75)

    LET pc_mensaje = " ", pc_mensaje CLIPPED , "... <ENTER> PARA SEGUIR "
    PROMPT pc_mensaje ATTRIBUTE(REVERSE) FOR CHAR enter

END FUNCTION

#---------------------------------------------------------------------------#
# rpt_cza_tran : Reporte que genera el encabezado del lote                  #
#---------------------------------------------------------------------------#
REPORT rpt_cza_tran()

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
            LET gdt_valor_trans     = NULL
            LET gdt_operacion       = NULL

            SELECT fecha_valor_trans  ,
                   fecha_operacion
            INTO   gdt_valor_trans      ,
                   gdt_operacion
            FROM   ret_cza_lote
            WHERE  folio    = gr_input.folio

            PRINT
                COLUMN 001, "01"                            ,-- Tipo registro
                COLUMN 003, "04"                            ,-- Id de servicio
                COLUMN 005, "01"                            ,-- Entidad origen
                COLUMN 007, gs_cod_afore USING "&&&"        ,-- Cve entidad origen
                COLUMN 010, "03"                            ,-- Entidad destino
                COLUMN 012, "001"                           ,-- Cve entidad destino
                COLUMN 015, gdt_operacion USING "YYYYMMDD"  ,-- Fecha operacion
                COLUMN 023, gdt_valor_trans USING "YYYYMMDD",-- Fecha valor trans
                COLUMN 031, 2 SPACES                        ,-- Resultado de la operacion
                COLUMN 033, 3 SPACES                        ,-- Motivo Rechazo 1
                COLUMN 036, 3 SPACES                        ,-- Motivo Rechazo 2
                COLUMN 039, 3 SPACES                        ,-- Motivo Rechazo 3
                COLUMN 042, 309 SPACES                       -- Filler

END REPORT

#---------------------------------------------------------------------------#
# sum_tran : Reporte que genera el sumario del lote                         #
#---------------------------------------------------------------------------#
REPORT sum_tran()
    
    DEFINE
        li_tot_regs         INTEGER
    
    -- -----------------------------------------------------------------------------
    
    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
            LET li_tot_regs = 0

            SELECT SUM(A.total_registros)
            INTO   li_tot_regs
            FROM   ret_ctr_envio_lote A , 
                   tab_tramite_retiro B , 
                   tab_retiro C
            WHERE  A.tipo_retiro    = B.tipo_retiro
            AND    A.estado         = 2
            AND    B.cod_tramite    = 1
            AND    A.tipo_retiro    = C.tipo_retiro

            PRINT
                COLUMN 001, "09"                            ,-- Tipo Registro
                COLUMN 003, "04"                            ,-- Id de servicio
                COLUMN 005, "01"                            ,-- Entidad Origen
                COLUMN 007, gs_cod_afore USING"&&&"         ,-- Cve Entidad origen
                COLUMN 010, "03"                            ,-- Entidad destino
                COLUMN 012, "001"                           ,-- Cve Entidad destino
                COLUMN 015, gdt_operacion USING "YYYYMMDD"  ,-- Fecha operacion
                COLUMN 023, li_tot_regs USING"&&&&&&"       ,-- Total registros
                COLUMN 029, 322 SPACES                       -- Filler
END REPORT
