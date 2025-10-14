#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETC870  => REALIZA LA CONSULTA DE LOS REGISTROS DE MARCA DE          #
#                     TRABAJADOR PENSIONADO Y MARCA DE RETIRO PARCIAL           #
#Fecha creacion    => 18 DE MAYO DE 2011                                        #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   => 13 DE MARZO DE 2014                                       #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                  => Se agrega opcion para generar la salida de la consulta    #
#                     en un archivo plano separado por pipes                    #
#Sistema           => RET                                                       #
#################################################################################
DATABASE safre_af
GLOBALS

    DEFINE gr_seg_modulo RECORD LIKE seg_modulo.*

    DEFINE
        HOY                         DATE

    DEFINE
        gc_where                    CHAR(600) ,
        enter                       CHAR(001) ,
        gc_usuario                  CHAR(020)

    DEFINE
        gs_flag_err                 SMALLINT

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL f_lib_crea_log()
    CALL init()
    CALL f_abre_ventanas()

    CALL f_captura_datos() RETURNING gs_flag_err, gc_where

    IF (gs_flag_err = FALSE) THEN
        CALL f_tablas_tmp()
        CALL f_obten_registros(gc_where)
        CALL f_muestra_registros()
    END IF

    CALL f_cierra_ventanas()

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    -- -----------------------------------------------------------------------------

    LET HOY             = TODAY
    LET gc_usuario      = f_lib_obten_user()

    SELECT *
    INTO   gr_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

END FUNCTION

#---------------------------------------------------------------------------#
# f_captura_datos : Captura los parametros de busqueda para el programa     #                                                         
#---------------------------------------------------------------------------#
FUNCTION f_captura_datos()

    DEFINE lr_datos RECORD
        fec_inicio      LIKE ret_marca_pensionado.fecha_marca   ,
        fec_fin         LIKE ret_marca_pensionado.fecha_marca   ,
        cod_tramite     LIKE ret_marca_pensionado.cod_tramite   ,
        descripcion     CHAR(40)                                ,
        tipo_retiro     LIKE ret_marca_pensionado.tipo_retiro   ,
        folio           LIKE ret_marca_pensionado.folio         ,
        nss             LIKE ret_marca_pensionado.nss
    END RECORD
    
    DEFINE
        ls_cancela              SMALLINT

    DEFINE
        lc_fecha                CHAR(100),
        lc_cod_tramite          CHAR(100),
        lc_tipo_ret             CHAR(100),
        lc_folio                CHAR(100),
        lc_nss                  CHAR(100),
        lc_where                CHAR(600)

    -- -----------------------------------------------------------------------------

    LET ls_cancela  = FALSE
    INITIALIZE lr_datos.* TO NULL

    INPUT BY NAME lr_datos.* WITHOUT DEFAULTS

        -------------------------------------
        AFTER FIELD fec_inicio
        -------------------------------------
            IF lr_datos.fec_inicio IS NOT NULL THEN
                NEXT FIELD fec_fin
            END IF

        -------------------------------------
        AFTER FIELD fec_fin
        -------------------------------------
            IF lr_datos.fec_fin IS NOT NULL THEN
                IF lr_datos.fec_inicio IS NULL THEN
                    CALL f_lib_error_msg("EN UN INTERVALO, LA FECHA INICIAL NO PUEDE SER NULA")
                    NEXT FIELD fec_inicio
                ELSE
                    IF lr_datos.fec_inicio > lr_datos.fec_fin THEN
                        CALL f_lib_error_msg("FECHA INICIAL NO PUEDE SER MAYOR A LA FINAL")
                        NEXT FIELD fec_inicio
                    END IF
                END IF
            END IF

        -------------------------------------
        AFTER FIELD cod_tramite
        -------------------------------------
            IF lr_datos.cod_tramite IS NULL THEN
                CALL f_despliega_tramite_cod()
                    RETURNING lr_datos.cod_tramite  ,
                              lr_datos.descripcion
            
                DISPLAY BY NAME lr_datos.descripcion
            ELSE
                IF lr_datos.cod_tramite = 0 THEN
                    LET lr_datos.descripcion = "TODOS"
                    DISPLAY BY NAME lr_datos.descripcion
                ELSE
                    -- Se valida por si no hace la captura por catalogo
                    SELECT descripcion
                    INTO   lr_datos.descripcion
                    FROM   tab_tipo_tramite
                    WHERE  cod_tramite = lr_datos.cod_tramite
                    AND    cod_tramite IN (1,2,4,9,10,11)
                    
                    IF STATUS = NOTFOUND THEN
                        CALL f_lib_error_msg("TIPO TRAMITE NO ACEPTADO")
                        NEXT FIELD cod_tramite
                    ELSE
                        DISPLAY BY NAME lr_datos.descripcion
                    END IF
                END IF
            END IF
        
        -------------------------------------
        ON KEY (ESC)
        -------------------------------------
            IF lr_datos.fec_inicio IS NOT NULL THEN
                IF lr_datos.fec_fin IS NULL THEN
                    CALL f_lib_error_msg("EN UN INTERVALO, LA FECHA FINAL NO PUEDE SER NULA")
                    NEXT FIELD fec_fin
                END IF
            END IF

            IF lr_datos.fec_fin IS NOT NULL THEN
                IF lr_datos.fec_inicio IS NULL THEN
                    CALL f_lib_error_msg("EN UN INTERVALO, LA FECHA INICIAL NO PUEDE SER NULA")
                    NEXT FIELD fec_inicio
                ELSE
                    IF lr_datos.fec_inicio > lr_datos.fec_fin THEN
                        CALL f_lib_error_msg("FECHA INICIAL NO PUEDE SER MAYOR A LA FINAL")
                        NEXT FIELD fec_inicio
                    END IF
                END IF
            END IF

            IF (f_lib_pregunta("¿DESEA GENERAR LA CONSULTA? (S/N) : ") = TRUE) THEN
                LET ls_cancela = FALSE
            ELSE 
                LET ls_cancela = TRUE
                CALL f_lib_error_msg("PROCESO CANCELADO")
            END IF 

            EXIT INPUT

        -------------------------------------
        ON KEY (CONTROL-C, INTERRUPT)
        -------------------------------------
            LET ls_cancela = TRUE
            CALL f_lib_error_msg("PROCESO CANCELADO")
            EXIT INPUT
    
    END INPUT

    -- Armamos la seccion de datos del query donde se realizara la busqueda
    IF (ls_cancela = FALSE) THEN

        IF (lr_datos.cod_tramite IS NOT NULL) THEN
            IF lr_datos.cod_tramite = 0 THEN
                LET lc_cod_tramite = " AND cod_tramite IN (1,2,4,9,10,11) "
            ELSE
                LET lc_cod_tramite = " AND cod_tramite = ", lr_datos.cod_tramite
            END IF
        ELSE
            LET lc_cod_tramite = " "
        END IF

        IF (lr_datos.tipo_retiro IS NOT NULL) THEN
            LET lc_tipo_ret = " AND tipo_retiro = '", lr_datos.tipo_retiro, "' "
        ELSE
            LET lc_tipo_ret = " "
        END IF

        IF (lr_datos.folio IS NOT NULL) THEN
            LET lc_folio = " AND folio = ", lr_datos.folio
        ELSE
            LET lc_folio = " "
        END IF

        IF (lr_datos.nss IS NOT NULL) THEN
            LET lc_nss = " AND nss = '", lr_datos.nss, "' "
        ELSE
            LET lc_nss = " "
        END IF

        IF ( (lr_datos.fec_inicio IS NOT NULL) AND (lr_datos.fec_fin IS NOT NULL) ) THEN
            LET lc_fecha = " AND fecha_marca BETWEEN '", lr_datos.fec_inicio,
                           "' AND '", lr_datos.fec_fin, "' "
        ELSE
            LET lc_fecha = " "
        END IF

        IF ( (lr_datos.cod_tramite IS NULL) AND 
             (lr_datos.nss IS NULL)         AND 
             (lr_datos.tipo_retiro IS NULL) AND
             (lr_datos.folio IS NULL)       AND 
             (lr_datos.fec_inicio IS NULL)  AND 
             (lr_datos.fec_fin IS NULL) 
           )
        THEN
            LET lc_where = " WHERE 1 = 1 "
        ELSE
            LET lc_where = " WHERE 1 = 1 ", lc_cod_tramite, lc_tipo_ret, lc_folio, lc_nss, lc_fecha CLIPPED            
        END IF

    END IF
    
    RETURN ls_cancela, lc_where

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

    OPEN WINDOW cat_tramite AT 05,12 WITH FORM "RETC8702" ATTRIBUTE(BORDER)
    DISPLAY "           TIPO DE MEDIO - RETIRO PARCIAL IMSS            " AT 2,1 ATTRIBUTE(REVERSE)

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
                          " WHERE cod_tramite IN (1,2,4,5,6,9,10,11) ",
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
# f_obten_registros : Llena la tabla temporal con los valores obtenidos de  #
#                     acuerdo a los parametros capturados                   #
#---------------------------------------------------------------------------#
FUNCTION f_obten_registros(pc_where)

    DEFINE 
        pc_where                CHAR(600)
    
    DEFINE lr_marca_pen RECORD LIKE ret_marca_pensionado.*
    
    DEFINE lr_marcas RECORD
        nss                CHAR(11)     ,
        desc_tramite       CHAR(100)    ,          
        tipo_retiro        CHAR(1)      ,          
        folio              INTEGER      ,          
        fecha_marca        DATE         ,          
        usuario_marca      CHAR(30)                
    END RECORD

    DEFINE 
        lc_query                CHAR(2000)

    -- -----------------------------------------------------------------------------

    DISPLAY " RECUPERANDO LA INFORMACION EN BASE DE DATOS ... " AT 20,1
        ATTRIBUTE(REVERSE)
    
    -- INV-2895
    -- Se realiza la consulta de ret_marca_pensionado y ret_marca_pensionado_hist
    -- por los campos exactos que se despliegan
    LET lc_query = " SELECT nss                 , " ,
                   "        curp                , " ,
                   "        folio               , " ,
                   "        consecutivo         , " ,
                   "        tipo_retiro         , " ,
                   "        cod_tramite         , " ,
                   "        id_marca_pen        , " ,
                   "        fecha_marca         , " ,
                   "        hora_marca          , " ,
                   "        usuario_marca         " ,
                   " FROM   ret_marca_pensionado  " ,
                   pc_where CLIPPED                 ,
                   " UNION "                        ,
                   " SELECT nss                 , " ,
                   "        curp                , " ,
                   "        folio               , " ,
                   "        consecutivo         , " ,
                   "        tipo_retiro         , " ,
                   "        cod_tramite         , " ,
                   "        id_marca_pen        , " ,
                   "        fecha_marca         , " ,
                   "        hora_marca          , " ,
                   "        usuario_marca         " ,
                   " FROM   ret_marca_pensionado_hist " ,   
                   pc_where CLIPPED                     ,
                   " ORDER BY 8,5,1 "    

    PREPARE prp_datos_marca FROM lc_query
    DECLARE cur_datos_marca CURSOR FOR prp_datos_marca

    FOREACH cur_datos_marca INTO lr_marca_pen.*   

        INITIALIZE lr_marcas.* TO NULL
        
        LET lr_marcas.nss             = lr_marca_pen.nss           
        LET lr_marcas.tipo_retiro     = lr_marca_pen.tipo_retiro   
        LET lr_marcas.folio           = lr_marca_pen.folio         
        LET lr_marcas.fecha_marca     = lr_marca_pen.fecha_marca   
        LET lr_marcas.usuario_marca   = lr_marca_pen.usuario_marca 

        SELECT descripcion
        INTO   lr_marcas.desc_tramite
        FROM   tab_tipo_tramite
        WHERE  cod_tramite  = lr_marca_pen.cod_tramite
        
        INSERT INTO tmp_marcas
        VALUES (lr_marcas.*)
        
    END FOREACH

    DISPLAY "                                                 " AT 20,1

END FUNCTION

#---------------------------------------------------------------------------#
# f_muestra_registros : Despliega la pantalla de consulta con los registros #
#                       encontrados de acuerdo a la busqueda                #
#---------------------------------------------------------------------------#
FUNCTION f_muestra_registros()

    DEFINE lr_marcas RECORD
        nss                CHAR(11)     ,
        desc_tramite       CHAR(100)    ,          
        tipo_retiro        CHAR(1)      ,          
        folio              INTEGER      ,          
        fecha_marca        DATE         ,          
        usuario_marca      CHAR(30)                
    END RECORD

    DEFINE lar_marca_pen ARRAY[1000] OF RECORD
        nss                CHAR(11)     ,
        desc_tramite       CHAR(100)    ,          
        tipo_retiro        CHAR(1)      ,          
        folio              INTEGER      ,          
        fecha_marca        DATE         ,          
        usuario_marca      CHAR(30)      
    END RECORD

    DEFINE
        li_cnt              SMALLINT
  
    -- -----------------------------------------------------------------------------

    CURRENT WINDOW IS win_despliega

    INITIALIZE lar_marca_pen[1].* TO NULL

    FOR  li_cnt = 2 TO 1000
        LET lar_marca_pen[li_cnt].*  = lar_marca_pen[1].*
    END FOR

    LET li_cnt = 1

    DECLARE cur_datos CURSOR FOR
        SELECT *
        FROM   tmp_marcas
        ORDER BY 2,3,1
    
    FOREACH cur_datos INTO lr_marcas.*

        IF (li_cnt >= 1000) THEN
            CALL f_lib_error_msg("SE ALCANZO EL LIMITE DEL ARREGLO")
            EXIT FOREACH
        END IF 
        
        LET lar_marca_pen[li_cnt].nss           = lr_marcas.nss          
        LET lar_marca_pen[li_cnt].desc_tramite  = lr_marcas.desc_tramite 
        LET lar_marca_pen[li_cnt].tipo_retiro   = lr_marcas.tipo_retiro  
        LET lar_marca_pen[li_cnt].folio         = lr_marcas.folio        
        LET lar_marca_pen[li_cnt].fecha_marca   = lr_marcas.fecha_marca  
        LET lar_marca_pen[li_cnt].usuario_marca = lr_marcas.usuario_marca

        LET li_cnt = li_cnt + 1
    
    END FOREACH
    
    IF li_cnt = 1 THEN 
        CALL f_lib_error_msg("NO HAY REGISTROS CON LOS CRITERIOS CAPTURADOS")
    ELSE
        CALL SET_COUNT(li_cnt-1)
        
        DISPLAY ARRAY lar_marca_pen TO scr_marcas.*
            ATTRIBUTE(CURRENT ROW DISPLAY = "REVERSE")

            ------------------
            ON KEY (CONTROL-P)
            ------------------
                CALL f_genera_plano()

            ------------------
            ON KEY (INTERRUPT)
            ------------------
                CLEAR FORM 
                EXIT DISPLAY

        END DISPLAY

    END IF 

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_plano : Genera el archivo plano de los registros consultados     #
#---------------------------------------------------------------------------#
FUNCTION f_genera_plano()

    DEFINE lr_reporte RECORD
        nss                     CHAR(11)     ,
        desc_tramite            CHAR(100)    ,          
        tipo_retiro             CHAR(1)      ,          
        folio                   INTEGER      ,          
        fecha_marca             DATE         ,          
        usuario_marca           CHAR(30)          
    END RECORD

    DEFINE
        lc_ruta_reporte             CHAR(100)   ,
        lc_nom_reporte              CHAR(100)

    -- -----------------------------------------------------------------------------

    INITIALIZE lr_reporte.* TO NULL

    LET lc_nom_reporte  = gc_usuario CLIPPED    ,
                          "_"                   ,
                          HOY USING "YYYYMMDD"  ,
                          "_MARCA_PEN.txt"
    
    LET lc_ruta_reporte  = gr_seg_modulo.ruta_listados CLIPPED   ,
                           "/"                                   ,
                           lc_nom_reporte CLIPPED

    START REPORT r_marca_pen TO lc_ruta_reporte

    DECLARE cur_reporte CURSOR FOR
        SELECT *
        FROM   tmp_marcas
        ORDER BY 2,3,1
    
    FOREACH cur_reporte INTO lr_reporte.*
        OUTPUT TO REPORT r_marca_pen(lr_reporte.*)
        INITIALIZE lr_reporte.* TO NULL
    END FOREACH

    FINISH REPORT r_marca_pen
    CALL f_lib_borra_lineas(gr_seg_modulo.ruta_listados, lc_nom_reporte)

    DISPLAY "REPORTE : ", lc_ruta_reporte CLIPPED AT 20,2

END FUNCTION

#---------------------------------------------------------------------------#
# f_abre_ventanas : Abre y prepara las ventanas que se usaran en            #
#                   el programa                                             #
#---------------------------------------------------------------------------#
FUNCTION f_abre_ventanas()

    -- -----------------------------------------------------------------------------

    OPEN WINDOW win_captura AT 4,4 WITH FORM "RETC8701" ATTRIBUTE(BORDER)
    DISPLAY " <CTRL-C> SALIR                                             <ESC> CONSULTAR " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC870       CONSULTA - MARCA DE PENSIONADO Y RET PARCIAL                 " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

    -- -----------------------------------------------------------------------------

    OPEN WINDOW win_despliega AT 4,4 WITH FORM "RETC8703" ATTRIBUTE(BORDER)
    DISPLAY " <CTRL-C> SALIR                                <CTRL-P> GENERAR ARCH PLANO  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC870       CONSULTA - MARCA DE PENSIONADO Y RET PARCIAL                 " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)    

    -- -----------------------------------------------------------------------------

    CURRENT WINDOW IS win_captura

END FUNCTION

#---------------------------------------------------------------------------#
# f_cierra_ventanas : Cierra las ventanas usadas por el programa            #
#---------------------------------------------------------------------------#
FUNCTION f_cierra_ventanas()

    CLOSE WINDOW win_despliega
    CLOSE WINDOW win_captura

END FUNCTION

#---------------------------------------------------------------------------#
# f_tablas_tmp : Genera las tablas temporales donde se almacenan los        #
#                calculos y cambios                                         #
#---------------------------------------------------------------------------#
FUNCTION f_tablas_tmp()

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_marcas
    WHENEVER ERROR STOP

    CREATE TEMP TABLE tmp_marcas (
        nss                CHAR(11) NOT NULL       ,
        desc_tramite       CHAR(100)               ,
        tipo_retiro        CHAR(1)                 ,
        folio              INTEGER                 ,
        fecha_marca        DATE                    ,
        usuario_marca      CHAR(30)
    )

END FUNCTION

#---------------------------------------------------------------------------#
# r_marca_pen : Reporte que genera el archivo plano de la consulta generada #
#               por el usuario                                              #
#---------------------------------------------------------------------------#
REPORT r_marca_pen(pr_reporte)

    DEFINE pr_reporte RECORD
        nss                     CHAR(11)     ,
        desc_tramite            CHAR(100)    ,          
        tipo_retiro             CHAR(1)      ,          
        folio                   INTEGER      ,          
        fecha_marca             DATE         ,          
        usuario_marca           CHAR(30)          
    END RECORD

    -- -----------------------------------------------------------------------------
    DEFINE
        lc_encabezado           CHAR(2000)

    -- -----------------------------------------------------------------------------

    OUTPUT
        PAGE LENGTH     1000
        LEFT MARGIN     0
        RIGHT MARGIN    0
        TOP MARGIN      0
        BOTTOM MARGIN   0

    FORMAT
        FIRST PAGE HEADER

            LET lc_encabezado   = "NSS|Tramite|Tipo Retiro|Folio|Fecha Marca|Usuario Marca"

            PRINT COLUMN 001, lc_encabezado CLIPPED

        ON EVERY ROW
            PRINT COLUMN 001, pr_reporte.nss                                , "|",
                              pr_reporte.desc_tramite CLIPPED               , "|",
                              pr_reporte.tipo_retiro                        , "|",
                              pr_reporte.folio        USING "<<<<<<<<<<"    , "|",
                              pr_reporte.fecha_marca  USING "DD/MM/YYYY"    , "|",
                              pr_reporte.usuario_marca CLIPPED              , "|"

END REPORT
