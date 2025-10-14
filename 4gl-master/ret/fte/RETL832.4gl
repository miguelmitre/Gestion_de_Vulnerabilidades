#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETL832  => GENERA EL REPORTE SEMANAL DE LIQUIDACIONES DE VIVIENDA 97 #
#                     PARA LAS DISPOSICIONES CON REGIMEN 73                     #
#Fecha creacion    => 1 DE MARZO DE 2012                                        #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   =>                                                           #
#Actualizacion     =>                                                           #
#                  =>                                                           #
#Sistema           => RET                                                       #
#################################################################################
DATABASE safre_af
GLOBALS

    DEFINE g_param RECORD LIKE seg_modulo.*

    DEFINE gr_edo RECORD
        liquidado           LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE gr_captura RECORD
        inicio          DATE        ,
        fin             DATE        
    END RECORD


    DEFINE
        HOY                         ,
        gdt_cambio_infonavit        DATE

    DEFINE
        G_LISTA                     CHAR(100) ,
        G_REPORTE                   CHAR(100) ,        
        gc_where                    CHAR(200) ,
        enter                       CHAR(001) ,
        gc_usuario                  CHAR(020)

    DEFINE
        gs_salida                   ,
        gs_hay_datos                ,
        gs_codigo_afore             SMALLINT

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("RETL832.log")
    CALL init()
    
    LET gs_salida    = 0

    WHILE gs_salida = 0

        LET gs_hay_datos = 0
        
        CALL f_captura_fechas() RETURNING gs_salida, gr_captura.*
        
        IF gs_salida = 0 THEN
            CALL f_obten_registros(gr_captura.*) RETURNING gs_hay_datos
            
            IF gs_hay_datos = 1 THEN 
                CALL f_muestra_registros(gr_captura.*)
            
                WHILE TRUE
                    PROMPT "¿ REALIZAR OTRA CONSULTA ? (S/N) : " ATTRIBUTE(REVERSE) FOR CHAR enter
                    IF enter MATCHES "[sSnN]" THEN
                        IF enter MATCHES "[nN]" THEN
                            LET gs_salida = 1
                        END IF
                
                        EXIT WHILE
                    END IF
                END WHILE
            ELSE
                CALL f_error_msg("NO EXISTEN REGISTROS PARA GENERAR EL REPORTE")
                LET gs_salida = 1
            END IF
        ELSE
            CALL f_error_msg("PROCESO CANCELADO")
            LET gs_salida = 1
        END IF
        
        CLOSE WINDOW RETL8321
    
    END WHILE

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    LET HOY                     = TODAY
    LET gdt_cambio_infonavit    = "01/12/2012"    

    ----- CODIGOS AFORES -----
    SELECT codigo_afore,
           USER
    INTO   gs_codigo_afore,
           gc_usuario
    FROM   tab_afore_local

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.liquidado
    FROM   ret_estado A
    WHERE  A.descripcion = "LIQUIDADO"

    ----- PARAMETROS  -----
    SELECT ruta_listados
    INTO   g_param.ruta_listados
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

END FUNCTION

#---------------------------------------------------------------------------#
# f_captura_fechas : Captura el rango de fechas en el que se buscara para   #
#                    generar el reporte de rechazos                         #
#---------------------------------------------------------------------------#
FUNCTION f_captura_fechas()

    DEFINE lr_datos_cap RECORD
        inicio          DATE        ,
        fin             DATE      
    END RECORD

    DEFINE
        ls_estado               ,
        ls_ret                  ,
        ls_cont                 ,
        ls_flag                 SMALLINT

    DEFINE
        lc_mensaje              CHAR(100)

    -- -----------------------------------------------------------------------------

    CALL f_abre_ventana()
    LET ls_flag = 0

    LET lr_datos_cap.inicio    = HOY
    LET lr_datos_cap.fin       = HOY

    DISPLAY BY NAME lr_datos_cap.inicio
    DISPLAY BY NAME lr_datos_cap.fin

    INPUT BY NAME lr_datos_cap.* WITHOUT DEFAULTS

        AFTER FIELD inicio
            CALL f_valida_fechas(lr_datos_cap.inicio)
                RETURNING ls_estado, lc_mensaje

            IF ls_estado <> 0 THEN
                ERROR lc_mensaje CLIPPED ATTRIBUTE(REVERSE)
                SLEEP 2
                ERROR ""
                NEXT FIELD inicio
            END IF

        AFTER FIELD fin
            CALL f_valida_fechas(lr_datos_cap.fin)
                RETURNING ls_estado, lc_mensaje

            IF ls_estado <> 0 THEN
                ERROR lc_mensaje CLIPPED ATTRIBUTE(REVERSE)
                SLEEP 2
                ERROR ""
                NEXT FIELD fin
            ELSE
                IF lr_datos_cap.inicio > lr_datos_cap.fin THEN
                    ERROR "LA FECHA INICIAL NO PUEDE SER MAYOR A LA FECHA FINAL"
                    SLEEP 2
                    ERROR ""
                    NEXT FIELD inicio
                END IF
            END IF

        ON KEY (CONTROL-C,INTERRUPT)
            LET ls_flag = 1
            EXIT INPUT

        #--
        ON KEY (ESC)
            CALL f_valida_fechas(lr_datos_cap.inicio)
                RETURNING ls_estado, lc_mensaje

            IF ls_estado <> 0 THEN
                ERROR lc_mensaje CLIPPED ATTRIBUTE(REVERSE)
                SLEEP 2
                ERROR ""
                NEXT FIELD inicio
            END IF

            CALL f_valida_fechas(lr_datos_cap.fin)
                RETURNING ls_estado, lc_mensaje

            IF ls_estado <> 0 THEN
                ERROR lc_mensaje CLIPPED ATTRIBUTE(REVERSE)
                SLEEP 2
                ERROR ""
                NEXT FIELD fin
            ELSE
                IF lr_datos_cap.inicio > lr_datos_cap.fin THEN
                    ERROR "LA FECHA INICIAL NO PUEDE SER MAYOR A LA FECHA FINAL"
                    SLEEP 2
                    ERROR ""
                    NEXT FIELD inicio
                END IF
            END IF

            LET ls_flag = 0
            EXIT INPUT

    END INPUT

    IF ls_flag = 0 THEN

        WHILE TRUE
            PROMPT "¿ DESEA EJECUTAR LA CONSULTA ? (S/N) : " ATTRIBUTE(REVERSE) FOR CHAR enter
            IF enter MATCHES "[sSnN]" THEN
                IF enter MATCHES "[nN]" THEN
                    LET ls_flag = 1
                END IF

                EXIT WHILE
            END IF
        END WHILE
    END IF

    RETURN ls_flag, lr_datos_cap.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_despliega_tramite_cod : Despliega la pantalla de ayuda que muestra los  #
#                           tipos de tramite validos para el proceso        #
#---------------------------------------------------------------------------#
FUNCTION f_despliega_tramite_cod()

    DEFINE lr_reg RECORD
        codigo            CHAR(02),
        descripcion       CHAR(35)
    END RECORD

    DEFINE lar_display ARRAY[100] OF RECORD
        codigo            CHAR(02),
        descripcion       CHAR(35)
    END RECORD

    DEFINE
        lc_busqueda         CHAR(300),
        lc_desc             CHAR(030)

    DEFINE
        li_pos              INTEGER

    -- -----------------------------------------------------------------------------

    OPEN WINDOW cat_tramite AT 05,12 WITH FORM "RETL8322" ATTRIBUTE(BORDER)
    DISPLAY "               TIPO DE PROCESO DE RETIROS                 " AT 2,1 ATTRIBUTE(REVERSE)

    INPUT BY NAME lc_desc

        BEFORE FIELD lc_desc
            LET lc_desc = "*"

        AFTER FIELD lc_desc
            IF lc_desc IS NULL THEN
                ERROR " DESCRIPCION A BUSCAR NO PUEDE SER NULA " ATTRIBUTE(NORMAL)
                NEXT FIELD lc_desc
            ELSE
                LET lc_desc = "*", lc_desc CLIPPED, "*"
                EXIT INPUT
            END IF
    END INPUT

    WHILE TRUE
        LET lc_busqueda = " SELECT cod_tramite, descripcion FROM tab_tipo_tramite ",
                          " WHERE cod_tramite IN (1,2,4,9,10) ",
                          " AND   descripcion MATCHES ", '"', lc_desc CLIPPED, '"',
                          " ORDER BY 1 " CLIPPED
        LET lar_display[1].codigo       = 0
        LET lar_display[1].descripcion  = "TODOS"

        LET li_pos = 2

        PREPARE prp_medio FROM lc_busqueda
        DECLARE cur_medio CURSOR FOR prp_medio

        FOREACH cur_medio INTO lar_display[li_pos].*

            LET li_pos = li_pos + 1
            IF li_pos >= 1000 THEN
                ERROR " FUE SOBREPASADA LA CAPACIDAD MAXIMA DEL ARREGLO" ATTRIBUTE(NORMAL)
                EXIT FOREACH
            END IF

        END FOREACH

        IF (li_pos - 1) < 1 THEN
            ERROR " EL CATALOGO DE TIPO DE MEDIOS ESTA VACIO" ATTRIBUTE(NORMAL)
        END IF

        CALL SET_COUNT(li_pos-1)

        DISPLAY ARRAY lar_display TO scr_bus.*
            ON KEY (CONTROL-C,INTERRUPT)
                LET li_pos = 0
                EXIT DISPLAY

            ON KEY (CONTROL-M)
                LET li_pos              = ARR_CURR()
                LET lr_reg.codigo       = lar_display[li_pos].codigo
                LET lr_reg.descripcion  = lar_display[li_pos].descripcion
                EXIT DISPLAY

        END DISPLAY

        IF li_pos <> 0 THEN
            EXIT WHILE
        END IF

    END WHILE

    CLOSE WINDOW cat_tramite

    RETURN lr_reg.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_obten_registros : Ejecuta las funciones necesarias para recopilar los   #
#                     datos de los diferentes procesos de acuerdo a los     #
#                     parametros de busqueda capturados                     #
#---------------------------------------------------------------------------#
FUNCTION f_obten_registros(pr_datos)

    DEFINE pr_datos RECORD
        inicio          DATE        ,
        fin             DATE     
    END RECORD
    
    DEFINE 
        ls_genera           SMALLINT

    -- ---------------------------------------------------------------------
    
    LET ls_genera = 1
    
    CALL f_genera_tmp_cuenta(pr_datos.*)
    
    SELECT "OK"
    FROM   tmp_dat_dis_cta
    GROUP BY 1
    
    IF STATUS = NOTFOUND THEN 
        LET ls_genera = 0
    ELSE
        CALL f_genera_datos_tmp()
        CALL f_obtiene_aceptados()
        CALL f_obtiene_rechazos_440()
        CALL f_obtiene_rechazos_viv2()
        CALL f_obtiene_rechazos_viv3()
        CALL f_obtiene_totales()
    END IF

    RETURN ls_genera

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_tmp_cuenta : Genera el extracto de dis_cuenta con los movimientos#
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

    SELECT A.*
    FROM   dis_cuenta A ,
           tab_retiro B
    WHERE  A.fecha_conversion BETWEEN pr_fechas.fecha_ini AND pr_fechas.fecha_fin
    AND    B.tipo_retiro        IN ("E","J","M")
    AND    A.tipo_movimiento    = B.movimiento
    AND    A.subcuenta          IN (1,7,4,8)
    UNION
    SELECT A.*
    FROM   dis_cuenta A
    WHERE  A.fecha_conversion BETWEEN pr_fechas.fecha_ini AND pr_fechas.fecha_fin
    AND    A.tipo_movimiento    = 10
    AND    A.subcuenta          IN (1,7,4,8)
    UNION
    SELECT A.*
    FROM   dis_cuenta A
    WHERE  A.fecha_conversion BETWEEN pr_fechas.fecha_ini AND pr_fechas.fecha_fin
    AND    A.tipo_movimiento    = 817
    AND    A.subcuenta          IN (4,8)
    INTO TEMP tmp_dat_dis_cta

    CREATE INDEX cta_data_01
    ON tmp_dat_dis_cta (tipo_movimiento, consecutivo_lote, nss)

    DISPLAY "                               " AT 19,2

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_datos_tmp : Obtiene las solicitudes de retiro que se utilizaran  #
#                      en la generación del reporte                         #
#---------------------------------------------------------------------------#
FUNCTION f_genera_datos_tmp()

    WHENEVER ERROR CONTINUE    
        DROP TABLE tmp_solicitud_tx
        DROP TABLE tmp_montos
    WHENEVER ERROR STOP

    SELECT *
    FROM   ret_solicitud_tx
    WHERE  nss IN (SELECT UNIQUE(nss)
                   FROM   tmp_dat_dis_cta )
    AND    folio IN (SELECT UNIQUE(folio)
                     FROM   tmp_dat_dis_cta )
    AND    estado_solicitud = gr_edo.liquidado
    AND    regimen          = 73
    INTO TEMP tmp_solicitud_tx
    
    -- ------------------------------------------------------------------------
    
    CREATE TEMP TABLE tmp_montos (
        id_registro                 SMALLINT        ,
        num_trabajadores            INTEGER         ,
        imp_ret_92_97               DECIMAL(16,2)   ,
        imp_sol_infonavit           DECIMAL(16,2)   ,
        imp_pagado_infonavit        DECIMAL(16,2)
       )

END FUNCTION


#---------------------------------------------------------------------------#
# f_obtiene_aceptados : Obtiene el detalle de los registros aceptados       #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_aceptados()

    DEFINE lr_aceptados RECORD
        id_registro             SMALLINT        ,
        num_trabajadores        INTEGER         ,
        imp_ret_92_97           DECIMAL(16,2)   ,
        imp_sol_infonavit       DECIMAL(16,2)   ,
        imp_pagado_infonavit    DECIMAL(16,2)    
    END RECORD

    -- -----------------------------------------------------------------------------

    -------  ANTERIORES AL 12 DE ENERO DE 2012 -------  
    INITIALIZE lr_aceptados.* TO NULL
    
    LET lr_aceptados.id_registro    = 3

    SELECT NVL(COUNT(*), 0)
    INTO   lr_aceptados.num_trabajadores
    FROM   tmp_solicitud_tx
    WHERE  diag_registro    = 400
    AND    fecha_resolucion <= gdt_cambio_infonavit

    SELECT NVL(( SUM(A.monto_en_pesos) * -1 ), 0)
    INTO   lr_aceptados.imp_ret_92_97
    FROM   tmp_dat_dis_cta A    ,
           tmp_solicitud_tx B
    WHERE  A.nss                = B.nss
    AND    A.consecutivo_lote   = B.consecutivo
    AND    A.folio              = B.folio
    AND    B.diag_registro      = 400
    AND    A.subcuenta         IN (1,7)
    AND    B.fecha_resolucion  <= gdt_cambio_infonavit

    SELECT NVL(( SUM(A.monto_en_pesos) * -1 ), 0)
    INTO   lr_aceptados.imp_sol_infonavit
    FROM   tmp_dat_dis_cta A    ,
           tmp_solicitud_tx B
    WHERE  A.nss                = B.nss
    AND    A.consecutivo_lote   = B.consecutivo
    AND    A.folio              = B.folio
    AND    B.diag_registro      = 400
    AND    A.subcuenta         IN (4,8)
    AND    B.fecha_resolucion  <= gdt_cambio_infonavit

    -- Este rubro se informa en ceros por ahora
    LET lr_aceptados.imp_pagado_infonavit = 0 
    
    INSERT INTO tmp_montos
    VALUES(lr_aceptados.*)


    -------  POSTERIORES AL 12 DE ENERO DE 2012 -------  
    INITIALIZE lr_aceptados.* TO NULL
    
    LET lr_aceptados.id_registro    = 9

    SELECT NVL(COUNT(*), 0)
    INTO   lr_aceptados.num_trabajadores
    FROM   tmp_solicitud_tx
    WHERE  diag_registro    = 400
    AND    fecha_resolucion > gdt_cambio_infonavit

    SELECT NVL(( SUM(A.monto_en_pesos) * -1 ), 0)
    INTO   lr_aceptados.imp_ret_92_97
    FROM   tmp_dat_dis_cta A    ,
           tmp_solicitud_tx B
    WHERE  A.nss                = B.nss
    AND    A.consecutivo_lote   = B.consecutivo
    AND    A.folio              = B.folio
    AND    B.diag_registro      = 400
    AND    A.subcuenta         IN (1,7)
    AND    B.fecha_resolucion   > gdt_cambio_infonavit

    SELECT NVL(( SUM(A.monto_en_pesos) * -1 ), 0)
    INTO   lr_aceptados.imp_sol_infonavit
    FROM   tmp_dat_dis_cta A    ,
           tmp_solicitud_tx B
    WHERE  A.nss                = B.nss
    AND    A.consecutivo_lote   = B.consecutivo
    AND    A.folio              = B.folio
    AND    B.diag_registro      = 400
    AND    A.subcuenta         IN (4,8)
    AND    B.fecha_resolucion   > gdt_cambio_infonavit

    -- Este rubro se informa en ceros por ahora
    LET lr_aceptados.imp_pagado_infonavit = 0 

    INSERT INTO tmp_montos
    VALUES(lr_aceptados.*)

END FUNCTION

#---------------------------------------------------------------------------#
# f_obtiene_rechazos_440 : Obtiene el detalle de los registros rechazados   #
#                          con diagnostico PROCESAR = 440                   #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_rechazos_440()

    DEFINE lr_rechazo_440 RECORD
        id_registro             SMALLINT        ,
        num_trabajadores        INTEGER         ,
        imp_ret_92_97           DECIMAL(16,2)   ,
        imp_sol_infonavit       DECIMAL(16,2)   ,
        imp_pagado_infonavit    DECIMAL(16,2)    
    END RECORD

    -- -----------------------------------------------------------------------------

    -------  ANTERIORES AL 12 DE ENERO DE 2012 -------  
    INITIALIZE lr_rechazo_440.* TO NULL
    
    LET lr_rechazo_440.id_registro    = 4

    SELECT NVL(COUNT(*), 0)
    INTO   lr_rechazo_440.num_trabajadores
    FROM   tmp_solicitud_tx
    WHERE  diag_registro    = 440
    AND    fecha_resolucion <= gdt_cambio_infonavit

{
    SELECT NVL(( SUM(A.monto_en_pesos) * -1 ), 0)
    INTO   lr_rechazo_440.imp_ret_92_97
    FROM   tmp_dat_dis_cta A    ,
           tmp_solicitud_tx B
    WHERE  A.nss                = B.nss
    AND    A.consecutivo_lote   = B.consecutivo
    AND    A.folio              = B.folio
    AND    B.diag_registro      = 440
    AND    A.subcuenta         IN (1,7)
    AND    B.fecha_resolucion  <= gdt_cambio_infonavit
}
    LET lr_rechazo_440.imp_ret_92_97 = NULL
{    
    SELECT NVL(( SUM(A.monto_en_pesos) * -1 ), 0)
    INTO   lr_rechazo_440.imp_sol_infonavit
    FROM   dis_provision A    ,
           tmp_solicitud_tx B
    WHERE  A.nss                = B.nss
    AND    A.consecutivo_lote   = B.consecutivo
    AND    A.folio              = B.folio
    AND    B.diag_registro      = 440
    AND    A.subcuenta         IN (4,8)
    AND    B.fecha_resolucion  <= gdt_cambio_infonavit
}
    
    LET lr_rechazo_440.imp_pagado_infonavit = NULL
    
    INSERT INTO tmp_montos
    VALUES(lr_rechazo_440.*)


    -------  POSTERIORES AL 12 DE ENERO DE 2012 -------  
    INITIALIZE lr_rechazo_440.* TO NULL
    
    LET lr_rechazo_440.id_registro    = 10

    SELECT NVL(COUNT(*), 0)
    INTO   lr_rechazo_440.num_trabajadores
    FROM   tmp_solicitud_tx
    WHERE  diag_registro    = 440
    AND    fecha_resolucion > gdt_cambio_infonavit
{
    SELECT NVL(( SUM(A.monto_en_pesos) * -1 ), 0)
    INTO   lr_rechazo_440.imp_ret_92_97
    FROM   tmp_dat_dis_cta A    ,
           tmp_solicitud_tx B
    WHERE  A.nss                = B.nss
    AND    A.consecutivo_lote   = B.consecutivo
    AND    A.folio              = B.folio
    AND    B.diag_registro      = 440
    AND    A.subcuenta         IN (1,7)
    AND    B.fecha_resolucion   > gdt_cambio_infonavit
}
    LET lr_rechazo_440.imp_ret_92_97 = NULL 
{
    SELECT NVL(( SUM(A.monto_en_pesos) * -1 ), 0)
    INTO   lr_rechazo_440.imp_sol_infonavit
    FROM   dis_provision A    ,
           tmp_solicitud_tx B
    WHERE  A.nss                = B.nss
    AND    A.consecutivo_lote   = B.consecutivo
    AND    A.folio              = B.folio
    AND    B.diag_registro      = 440
    AND    A.subcuenta         IN (4,8)
    AND    B.fecha_resolucion   > gdt_cambio_infonavit
}
    LET lr_rechazo_440.imp_pagado_infonavit = NULL

    INSERT INTO tmp_montos
    VALUES(lr_rechazo_440.*)

END FUNCTION

#---------------------------------------------------------------------------#
# f_obtiene_rechazos_viv2 : Obtiene el detalle de los registros rechazados  #
#                          con diagnostico de vivienda = 2                  #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_rechazos_viv2()

    DEFINE lr_rechazo_viv2 RECORD
        id_registro             SMALLINT        ,
        num_trabajadores        INTEGER         ,
        imp_ret_92_97           DECIMAL(16,2)   ,
        imp_sol_infonavit       DECIMAL(16,2)   ,
        imp_pagado_infonavit    DECIMAL(16,2)    
    END RECORD

    -- -----------------------------------------------------------------------------

    -------  ANTERIORES AL 12 DE ENERO DE 2012 -------  
    INITIALIZE lr_rechazo_viv2.* TO NULL
    
    LET lr_rechazo_viv2.id_registro    = 5

    SELECT NVL(COUNT(*), 0)
    INTO   lr_rechazo_viv2.num_trabajadores
    FROM   tmp_solicitud_tx A   ,
           ret_monto_viv    B
    WHERE  A.nss                = B.nss
    AND    A.folio              = B.folio
    AND    A.consecutivo        = B.consecutivo
    AND    B.estado_sub_viv     = 2
    AND    A.fecha_resolucion  <= gdt_cambio_infonavit
{
    SELECT NVL(( SUM(C.monto_en_pesos) * -1 ), 0)
    INTO   lr_rechazo_viv2.imp_ret_92_97
    FROM   tmp_solicitud_tx A   ,
           ret_monto_viv    B   ,
           tmp_dat_dis_cta  C
    WHERE  A.nss                = B.nss
    AND    A.nss                = C.nss
    AND    A.folio              = B.folio
    AND    A.folio              = C.folio
    AND    A.consecutivo        = B.consecutivo
    AND    A.consecutivo        = C.consecutivo_lote
    AND    B.estado_sub_viv     = 2
    AND    C.subcuenta         IN (1,7)
    AND    A.fecha_resolucion  <= gdt_cambio_infonavit
}
    LET lr_rechazo_viv2.imp_ret_92_97 = NULL
{
    SELECT NVL(( SUM(C.monto_en_pesos) * -1 ), 0)
    INTO   lr_rechazo_viv2.imp_sol_infonavit
    FROM   tmp_solicitud_tx A   ,
           ret_monto_viv    B   ,
           dis_provision    C
    WHERE  A.nss                = B.nss
    AND    A.nss                = C.nss
    AND    A.folio              = B.folio
    AND    A.folio              = C.folio
    AND    A.consecutivo        = B.consecutivo
    AND    A.consecutivo        = C.consecutivo_lote
    AND    B.estado_sub_viv     = 2
    AND    C.subcuenta         IN (4,8)
    AND    A.fecha_resolucion  <= gdt_cambio_infonavit
}
   LET lr_rechazo_viv2.imp_pagado_infonavit = NULL
    
    INSERT INTO tmp_montos
    VALUES(lr_rechazo_viv2.*)


    -------  POSTERIORES AL 12 DE ENERO DE 2012 -------  
    INITIALIZE lr_rechazo_viv2.* TO NULL
    
    LET lr_rechazo_viv2.id_registro    = 11

    SELECT NVL(COUNT(*), 0)
    INTO   lr_rechazo_viv2.num_trabajadores
    FROM   tmp_solicitud_tx A   ,
           ret_monto_viv    B
    WHERE  A.nss                = B.nss
    AND    A.folio              = B.folio
    AND    A.consecutivo        = B.consecutivo
    AND    B.estado_sub_viv     = 2
    AND    A.fecha_resolucion   > gdt_cambio_infonavit
{
    SELECT NVL(( SUM(C.monto_en_pesos) * -1 ), 0)
    INTO   lr_rechazo_viv2.imp_ret_92_97
    FROM   tmp_solicitud_tx A   ,
           ret_monto_viv    B   ,
           tmp_dat_dis_cta  C
    WHERE  A.nss                = B.nss
    AND    A.nss                = C.nss
    AND    A.folio              = B.folio
    AND    A.folio              = C.folio
    AND    A.consecutivo        = B.consecutivo
    AND    A.consecutivo        = C.consecutivo_lote
    AND    B.estado_sub_viv     = 2
    AND    C.subcuenta         IN (1,7)
    AND    A.fecha_resolucion   > gdt_cambio_infonavit
}
    LET lr_rechazo_viv2.imp_ret_92_97 = NULL
{    
    SELECT NVL(( SUM(C.monto_en_pesos) * -1 ), 0)
    INTO   lr_rechazo_viv2.imp_sol_infonavit
    FROM   tmp_solicitud_tx A   ,
           ret_monto_viv    B   ,
           dis_provision    C
    WHERE  A.nss                = B.nss
    AND    A.nss                = C.nss
    AND    A.folio              = B.folio
    AND    A.folio              = C.folio
    AND    A.consecutivo        = B.consecutivo
    AND    A.consecutivo        = C.consecutivo_lote
    AND    B.estado_sub_viv     = 2
    AND    C.subcuenta         IN (4,8)
    AND    A.fecha_resolucion   > gdt_cambio_infonavit
}
    LET lr_rechazo_viv2.imp_pagado_infonavit = NULL

    INSERT INTO tmp_montos
    VALUES(lr_rechazo_viv2.*)

END FUNCTION

#---------------------------------------------------------------------------#
# f_obtiene_rechazos_viv3 : Obtiene el detalle de los registros rechazados  #
#                          con diagnostico de vivienda = 3                  #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_rechazos_viv3()

    DEFINE lr_rechazo_viv3 RECORD
        id_registro             SMALLINT        ,
        num_trabajadores        INTEGER         ,
        imp_ret_92_97           DECIMAL(16,2)   ,
        imp_sol_infonavit       DECIMAL(16,2)   ,
        imp_pagado_infonavit    DECIMAL(16,2)    
    END RECORD

    -- -----------------------------------------------------------------------------

    -------  ANTERIORES AL 12 DE ENERO DE 2012 -------  
    INITIALIZE lr_rechazo_viv3.* TO NULL
    
    LET lr_rechazo_viv3.id_registro    = 6

    SELECT NVL(COUNT(*), 0)
    INTO   lr_rechazo_viv3.num_trabajadores
    FROM   tmp_solicitud_tx A   ,
           ret_monto_viv    B
    WHERE  A.nss                = B.nss
    AND    A.folio              = B.folio
    AND    A.consecutivo        = B.consecutivo
    AND    B.estado_sub_viv     = 3
    AND    A.fecha_resolucion  <= gdt_cambio_infonavit
{
    SELECT NVL(( SUM(C.monto_en_pesos) * -1 ), 0)
    INTO   lr_rechazo_viv3.imp_ret_92_97
    FROM   tmp_solicitud_tx A   ,
           ret_monto_viv    B   ,
           tmp_dat_dis_cta  C
    WHERE  A.nss                = B.nss
    AND    A.nss                = C.nss
    AND    A.folio              = B.folio
    AND    A.folio              = C.folio
    AND    A.consecutivo        = B.consecutivo
    AND    A.consecutivo        = C.consecutivo_lote
    AND    B.estado_sub_viv     = 3
    AND    C.subcuenta         IN (1,7)
    AND    A.fecha_resolucion  <= gdt_cambio_infonavit
}
    LET lr_rechazo_viv3.imp_ret_92_97 = NULL
{
    SELECT NVL(( SUM(C.monto_en_pesos) * -1 ), 0)
    INTO   lr_rechazo_viv3.imp_sol_infonavit
    FROM   tmp_solicitud_tx A   ,
           ret_monto_viv    B   ,
           dis_provision    C
    WHERE  A.nss                = B.nss
    AND    A.nss                = C.nss
    AND    A.folio              = B.folio
    AND    A.folio              = C.folio
    AND    A.consecutivo        = B.consecutivo
    AND    A.consecutivo        = C.consecutivo_lote
    AND    B.estado_sub_viv     = 3
    AND    C.subcuenta         IN (4,8)
    AND    A.fecha_resolucion  <= gdt_cambio_infonavit
}
    LET lr_rechazo_viv3.imp_pagado_infonavit = NULL
    
    INSERT INTO tmp_montos
    VALUES(lr_rechazo_viv3.*)

    -------  POSTERIORES AL 12 DE ENERO DE 2012 -------  
    INITIALIZE lr_rechazo_viv3.* TO NULL
    
    LET lr_rechazo_viv3.id_registro    = 12

    SELECT NVL(COUNT(*), 0)
    INTO   lr_rechazo_viv3.num_trabajadores
    FROM   tmp_solicitud_tx A   ,
           ret_monto_viv    B
    WHERE  A.nss                = B.nss
    AND    A.folio              = B.folio
    AND    A.consecutivo        = B.consecutivo
    AND    B.estado_sub_viv     = 3
    AND    A.fecha_resolucion   > gdt_cambio_infonavit
{
    SELECT NVL(( SUM(C.monto_en_pesos) * -1 ), 0)
    INTO   lr_rechazo_viv3.imp_ret_92_97
    FROM   tmp_solicitud_tx A   ,
           ret_monto_viv    B   ,
           tmp_dat_dis_cta  C
    WHERE  A.nss                = B.nss
    AND    A.nss                = C.nss
    AND    A.folio              = B.folio
    AND    A.folio              = C.folio
    AND    A.consecutivo        = B.consecutivo
    AND    A.consecutivo        = C.consecutivo_lote
    AND    B.estado_sub_viv     = 3
    AND    C.subcuenta         IN (1,7)
    AND    A.fecha_resolucion   > gdt_cambio_infonavit
}
    LET lr_rechazo_viv3.imp_ret_92_97 = NULL
{
    SELECT NVL(( SUM(C.monto_en_pesos) * -1 ), 0)
    INTO   lr_rechazo_viv3.imp_sol_infonavit
    FROM   tmp_solicitud_tx A   ,
           ret_monto_viv    B   ,
           dis_provision    C
    WHERE  A.nss                = B.nss
    AND    A.nss                = C.nss
    AND    A.folio              = B.folio
    AND    A.folio              = C.folio
    AND    A.consecutivo        = B.consecutivo
    AND    A.consecutivo        = C.consecutivo_lote
    AND    B.estado_sub_viv     = 3
    AND    C.subcuenta         IN (4,8)
    AND    A.fecha_resolucion   > gdt_cambio_infonavit
}
    LET lr_rechazo_viv3.imp_pagado_infonavit = NULL

    INSERT INTO tmp_montos
    VALUES(lr_rechazo_viv3.*)

END FUNCTION

#---------------------------------------------------------------------------#
# f_obtiene_totales : Obtiene el detalle de los totales por rechazos y el   #
#                     total general                                         #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_totales()

    DEFINE lr_totales RECORD
        id_registro             SMALLINT        ,
        num_trabajadores        INTEGER         ,
        imp_ret_92_97           DECIMAL(16,2)   ,
        imp_sol_infonavit       DECIMAL(16,2)   ,
        imp_pagado_infonavit    DECIMAL(16,2)    
    END RECORD

    -- -----------------------------------------------------------------------------

    ------- TOTAL RECHAZOS ANTERIORES AL 12 DE ENERO DE 2012 -------  
    INITIALIZE lr_totales.* TO NULL
    
    LET lr_totales.id_registro    = 7

    SELECT NVL(COUNT(*), 0)
    INTO   lr_totales.num_trabajadores
    FROM   tmp_solicitud_tx
    WHERE  diag_registro    NOT IN (400,440)
    AND    fecha_resolucion <= gdt_cambio_infonavit
    
    SELECT SUM(imp_ret_92_97)
    INTO   lr_totales.imp_ret_92_97
    FROM   tmp_montos
    WHERE  id_registro IN (4,5,6)

    SELECT SUM(imp_sol_infonavit)
    INTO   lr_totales.imp_sol_infonavit
    FROM   tmp_montos
    WHERE  id_registro IN (4,5,6)

    SELECT SUM(imp_pagado_infonavit)
    INTO   lr_totales.imp_pagado_infonavit
    FROM   tmp_montos
    WHERE  id_registro IN (4,5,6)
    
    INSERT INTO tmp_montos
    VALUES(lr_totales.*)

    ------- TOTAL RECHAZOS POSTERIORES AL 12 DE ENERO DE 2012 -------  
    INITIALIZE lr_totales.* TO NULL
    
    LET lr_totales.id_registro    = 13

    SELECT NVL(COUNT(*), 0)
    INTO   lr_totales.num_trabajadores
    FROM   tmp_solicitud_tx
    WHERE  diag_registro    NOT IN (400,440)
    AND    fecha_resolucion > gdt_cambio_infonavit

    SELECT SUM(imp_ret_92_97)
    INTO   lr_totales.imp_ret_92_97
    FROM   tmp_montos
    WHERE  id_registro IN (10,11,12)

    SELECT SUM(imp_sol_infonavit)
    INTO   lr_totales.imp_sol_infonavit
    FROM   tmp_montos
    WHERE  id_registro IN (10,11,12)

    SELECT SUM(imp_pagado_infonavit)
    INTO   lr_totales.imp_pagado_infonavit
    FROM   tmp_montos
    WHERE  id_registro IN (10,11,12)
    
    INSERT INTO tmp_montos
    VALUES(lr_totales.*)    

    ------- TOTAL TRAMITES ANTERIORES AL 12 DE ENERO DE 2012 -------  
    INITIALIZE lr_totales.* TO NULL
    
    LET lr_totales.id_registro    = 2

    SELECT NVL(SUM(num_trabajadores), 0)
    INTO   lr_totales.num_trabajadores
    FROM   tmp_montos
    WHERE  id_registro IN (3,7)

{
    SELECT NVL(SUM(imp_ret_92_97), 0)
    INTO   lr_totales.imp_ret_92_97
    FROM   tmp_montos
    WHERE  id_registro IN (3,7)
}
    LET lr_totales.imp_ret_92_97 = NULL 
{
    SELECT NVL(SUM(imp_sol_infonavit), 0)
    INTO   lr_totales.imp_sol_infonavit
    FROM   tmp_montos
    WHERE  id_registro IN (3,7)
}
    LET lr_totales.imp_sol_infonavit = NULL
{
    SELECT NVL(SUM(imp_pagado_infonavit), 0)
    INTO   lr_totales.imp_pagado_infonavit
    FROM   tmp_montos
    WHERE  id_registro IN (3,7)
}    
    LET lr_totales.imp_pagado_infonavit = NULL
    
    INSERT INTO tmp_montos
    VALUES(lr_totales.*)    
    
    ------- TOTAL TRAMITES POSTERIORES AL 12 DE ENERO DE 2012 -------  
    INITIALIZE lr_totales.* TO NULL
    
    LET lr_totales.id_registro    = 8

    SELECT NVL(SUM(num_trabajadores), 0)
    INTO   lr_totales.num_trabajadores
    FROM   tmp_montos
    WHERE  id_registro IN (9,13)
{
    SELECT SUM(imp_ret_92_97)
    INTO   lr_totales.imp_ret_92_97
    FROM   tmp_montos
    WHERE  id_registro IN (9,13)
}
    LET lr_totales.imp_ret_92_97 = NULL
{    
    SELECT SUM(imp_sol_infonavit)
    INTO   lr_totales.imp_sol_infonavit
    FROM   tmp_montos
    WHERE  id_registro IN (9,13)
}
    LET lr_totales.imp_sol_infonavit = NULL
{
    SELECT SUM(imp_pagado_infonavit)
    INTO   lr_totales.imp_pagado_infonavit
    FROM   tmp_montos
    WHERE  id_registro IN (9,13)
}   
    LET lr_totales.imp_pagado_infonavit = NULL
    
    INSERT INTO tmp_montos
    VALUES(lr_totales.*)        

    ------- TOTAL TRAMITES  -------  
    INITIALIZE lr_totales.* TO NULL
    
    LET lr_totales.id_registro    = 1

    SELECT NVL(SUM(num_trabajadores), 0)
    INTO   lr_totales.num_trabajadores
    FROM   tmp_montos
    WHERE  id_registro IN (2,8)
{
    SELECT SUM(imp_ret_92_97)
    INTO   lr_totales.imp_ret_92_97
    FROM   tmp_montos
    WHERE  id_registro IN (2,8)
}
    LET lr_totales.imp_ret_92_97 = NULL
{    
    SELECT SUM(imp_sol_infonavit)
    INTO   lr_totales.imp_sol_infonavit
    FROM   tmp_montos
    WHERE  id_registro IN (2,8)
}
    LET lr_totales.imp_sol_infonavit = NULL
{
    SELECT SUM(imp_pagado_infonavit)
    INTO   lr_totales.imp_pagado_infonavit
    FROM   tmp_montos
    WHERE  id_registro IN (2,8)
}   
    LET lr_totales.imp_pagado_infonavit = NULL
    
    INSERT INTO tmp_montos
    VALUES(lr_totales.*)        
    
END FUNCTION

#---------------------------------------------------------------------------#
# f_muestra_registros : Despliega en pantalla los resultados de la busqueda #
#---------------------------------------------------------------------------#
FUNCTION f_muestra_registros(pr_datos)

    DEFINE pr_datos RECORD
        inicio          DATE        ,
        fin             DATE
    END RECORD

    DEFINE lr_datos_tot RECORD
        id_registro             SMALLINT        ,
        num_trabajadores        INTEGER         ,
        imp_ret_92_97           DECIMAL(16,2)   ,
        imp_sol_infonavit       DECIMAL(16,2)   ,
        imp_pagado_infonavit    DECIMAL(16,2)    
    END RECORD

    DEFINE lar_datos_tot ARRAY[7] OF RECORD
        num_trabajadores        INTEGER         ,
        imp_ret_92_97           DECIMAL(16,2)   ,
        imp_sol_infonavit       DECIMAL(16,2)   ,
        imp_pagado_infonavit    DECIMAL(16,2)    
    END RECORD
    
    DEFINE lc_desc_afore LIKE tab_afore.afore_desc

    DEFINE
        li_cnt              SMALLINT

    -- -----------------------------------------------------------------------------

    OPEN WINDOW RETL8322 AT 4,4 WITH FORM "RETL8322" ATTRIBUTE(BORDER)
    DISPLAY " <CTRL-G> - Genera Archivo Plano                           <CTRL-C> - Salir  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " <CTRL-I> - Detalle de Rechazados                                            " AT 2,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETL832      REPORTE SEMANAL DE LIQUIDACIONES VIVIENDA REG 73               " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

    SELECT afore_desc
    INTO   lc_desc_afore
    FROM   tab_afore
    WHERE  afore_cod    = gs_codigo_afore    

    DISPLAY gs_codigo_afore TO cod_afore
    DISPLAY lc_desc_afore TO desc_afore
    DISPLAY BY NAME pr_datos.inicio
    DISPLAY BY NAME pr_datos.fin

    FOR  li_cnt = 1 TO 7
        INITIALIZE lar_datos_tot[li_cnt].* TO NULL
    END FOR

    LET li_cnt = 1

    DECLARE cur_datos CURSOR FOR
    SELECT *
    FROM   tmp_montos
    WHERE  id_registro IN (1,2,3,7,8,9,13)
    ORDER BY 1

    FOREACH cur_datos INTO lr_datos_tot.*

        IF li_cnt > 7 THEN
            CALL f_error_msg("SE ALCANZO EL LIMITE DEL ARREGLO")
            EXIT FOREACH
        END IF

        LET lar_datos_tot[li_cnt].num_trabajadores      = lr_datos_tot.num_trabajadores    
        LET lar_datos_tot[li_cnt].imp_ret_92_97         = lr_datos_tot.imp_ret_92_97       
        LET lar_datos_tot[li_cnt].imp_sol_infonavit     = lr_datos_tot.imp_sol_infonavit   
        LET lar_datos_tot[li_cnt].imp_pagado_infonavit  = lr_datos_tot.imp_pagado_infonavit

        LET li_cnt = li_cnt + 1

    END FOREACH

    IF li_cnt = 1 THEN
        CALL f_error_msg("LA BUSQUEDA NO ARROJO DATOS")
    ELSE
        CALL SET_COUNT(li_cnt-1)
        DISPLAY ARRAY lar_datos_tot TO scr_totales.*

            -- Genera archivo plano
            ON KEY (CONTROL-G)
                CALL f_genera_plano()
                EXIT DISPLAY

            -- Muestra detalle de rechazos
            ON KEY (CONTROL-I)
                CALL f_muestra_det_rechazos()
                
        END DISPLAY
    END IF

    CLOSE WINDOW RETL8322

END FUNCTION

#---------------------------------------------------------------------------#
# f_muestra_registros : Despliega en pantalla el detalle de los registros   #
#                       con rechazo                                         #
#---------------------------------------------------------------------------#
FUNCTION f_muestra_det_rechazos()

    DEFINE lr_rechazos RECORD
        id_registro             SMALLINT        ,
        num_trabajadores        INTEGER         ,
        imp_ret_92_97           DECIMAL(16,2)   ,
        imp_sol_infonavit       DECIMAL(16,2)   ,
        imp_pagado_infonavit    DECIMAL(16,2)    
    END RECORD

    DEFINE lar_rechazos ARRAY[6] OF RECORD
        num_trabajadores        INTEGER         ,
        imp_ret_92_97           DECIMAL(16,2)   ,
        imp_sol_infonavit       DECIMAL(16,2)   ,
        imp_pagado_infonavit    DECIMAL(16,2)    
    END RECORD
    
    DEFINE
        li_cnt              SMALLINT

    -- -----------------------------------------------------------------------------

    OPEN WINDOW RETL8323 AT 4,4 WITH FORM "RETL8323" ATTRIBUTE(BORDER)
    DISPLAY "                                                           <CTRL-C> - Salir  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETL832      REPORTE SEMANAL DE LIQUIDACIONES VIVIENDA REG 73               " AT 2,1 ATTRIBUTE(REVERSE)

    FOR  li_cnt = 1 TO 6
        INITIALIZE lar_rechazos[li_cnt].* TO NULL
    END FOR

    LET li_cnt = 1

    DECLARE cur_rechazo CURSOR FOR
    SELECT *
    FROM   tmp_montos
    WHERE  id_registro IN (4,5,6,10,11,12)
    ORDER BY 1

    FOREACH cur_rechazo INTO lr_rechazos.*

        IF li_cnt > 6 THEN
            CALL f_error_msg("SE ALCANZO EL LIMITE DEL ARREGLO")
            EXIT FOREACH
        END IF

        LET lar_rechazos[li_cnt].num_trabajadores       = lr_rechazos.num_trabajadores    
        LET lar_rechazos[li_cnt].imp_ret_92_97          = lr_rechazos.imp_ret_92_97       
        LET lar_rechazos[li_cnt].imp_sol_infonavit      = lr_rechazos.imp_sol_infonavit   
        LET lar_rechazos[li_cnt].imp_pagado_infonavit   = lr_rechazos.imp_pagado_infonavit

        LET li_cnt = li_cnt + 1

    END FOREACH

    IF li_cnt = 1 THEN
        CALL f_error_msg("LA BUSQUEDA NO ARROJO DATOS")
    ELSE
        CALL SET_COUNT(li_cnt-1)
        DISPLAY ARRAY lar_rechazos TO scr_rechazos.*
    END IF

    CLOSE WINDOW RETL8323

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_plano : Genera el archivo plano separado por pipes de la         #
#                  consulta realizada                                       #
#---------------------------------------------------------------------------#
FUNCTION f_genera_plano()

    DEFINE lr_reporte RECORD
        id_registro             SMALLINT        ,
        num_trabajadores        INTEGER         ,
        imp_ret_92_97           DECIMAL(16,2)   ,
        imp_sol_infonavit       DECIMAL(16,2)   ,
        imp_pagado_infonavit    DECIMAL(16,2)    
    END RECORD

    DEFINE
        lc_mensaje          CHAR(075)   ,
        lc_ejecuta          CHAR(100)

    -- -----------------------------------------------------------------------------

    LET G_LISTA = g_param.ruta_listados CLIPPED,"/",gc_usuario CLIPPED,".RETL832.",
                       HOY USING "DD-MM-YYYY"

    START REPORT listado_plano TO G_LISTA

    DECLARE cur_lpt CURSOR FOR
    SELECT *
    FROM   tmp_montos
    ORDER BY 1

    FOREACH cur_lpt INTO lr_reporte.*
        OUTPUT TO REPORT listado_plano(lr_reporte.*)
    END FOREACH

    FINISH REPORT listado_plano

    LET lc_ejecuta = "chmod 777 ", G_LISTA CLIPPED
    RUN lc_ejecuta

    LET lc_mensaje = "REPORTE GENERADO EN LA RUTA ", G_LISTA CLIPPED
    DISPLAY lc_mensaje AT 19,1
    CALL f_error_msg("ARCHIVO GENERADO CORRECTAMENTE")

END FUNCTION

#---------------------------------------------------------------------------#
# f_abre_ventana : Abre la ventana donde se captura los datos para generar  #
#                  la consulta de rechazos                                  #
#---------------------------------------------------------------------------#
FUNCTION f_abre_ventana()

    OPEN WINDOW RETL8321 AT 4,4 WITH FORM "RETL8321" ATTRIBUTE(BORDER)
    DISPLAY "                                                           <CTRL-C> - Salir  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETL832      REPORTE SEMANAL DE LIQUIDACIONES VIVIENDA REG 73               " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_fechas : Valida que la fecha capturada no sea nula y que cumpla  #
#                   las condiciones necesarias para ser aceptada            #
#---------------------------------------------------------------------------#
FUNCTION f_valida_fechas(pdt_fecha)

    DEFINE
        pdt_fecha           DATE

    DEFINE
        ls_estado           SMALLINT

    DEFINE
        lc_mensaje          CHAR(100)

    -- ---------------------------------------------------------------------

    LET ls_estado = 0

    IF pdt_fecha IS NULL THEN
        LET lc_mensaje = " LA FECHA NO DEBE SER NULA"
        LET ls_estado = 1
    ELSE
        IF pdt_fecha < "01/01/1900" THEN
            LET lc_mensaje = " FECHA INVALIDA"
            LET ls_estado = 1
        ELSE
            IF pdt_fecha > HOY THEN
                LET lc_mensaje = " LA FECHA NO DEBE SER MAYOR A LA FECHA DEL DIA"
                LET ls_estado = 1
            END IF
        END IF
    END IF

    RETURN ls_estado, lc_mensaje

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
# listado_plano : Genera el archivo plano separado por pipes de la consulta #
#               realizada                                                   #
#---------------------------------------------------------------------------#
REPORT listado_plano(pr_plano)

    DEFINE pr_plano RECORD
        id_registro             SMALLINT        ,
        num_trabajadores        INTEGER         ,
        imp_ret_92_97           DECIMAL(16,2)   ,
        imp_sol_infonavit       DECIMAL(16,2)   ,
        imp_pagado_infonavit    DECIMAL(16,2)    
    END RECORD

    -- -----------------------------------------------------------------------------

    OUTPUT
       LEFT MARGIN    0
       RIGHT MARGIN   0
       TOP MARGIN     0
       BOTTOM MARGIN  0
       PAGE LENGTH    1

    FORMAT
       ON EVERY ROW

          PRINT COLUMN 01, pr_plano.num_trabajadores    ,"|",
                           pr_plano.imp_ret_92_97       ,"|",
                           pr_plano.imp_sol_infonavit   ,"|",
                           pr_plano.imp_pagado_infonavit,"|"
END REPORT
