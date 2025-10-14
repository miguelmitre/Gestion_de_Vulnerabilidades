#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETC970  => GENERA LA OPERACION 52 RETIROS PARCIALES ISSSTE           #
#Fecha creacion    => 4 DE NOVIEMBRE DE 2009                                    #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   => 17 DE JUNIO DE 2011                                       #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                     Modificaciones al layout de la operacion 52 de acuerdo    #
#                     a lo indicado en el requerimiento EFPS-161                #
#Sistema           => RET                                                       #
#################################################################################
DATABASE safre_af
GLOBALS

    DEFINE g_seg_modulo RECORD LIKE seg_modulo.*

    DEFINE gr_edo RECORD
        capturado             LIKE ret_estado_issste.estado_solicitud ,
        confirmado            LIKE ret_estado_issste.estado_solicitud ,
        enviado               LIKE ret_estado_issste.estado_solicitud
    END RECORD

    DEFINE #glo #date
        gdt_fecha_proc        ,
        HOY                   DATE

    DEFINE
        G_LISTA_1             CHAR(100) ,
        G_LISTA_2             CHAR(100) ,
        G_LISTA               CHAR(300) ,
        c7_nombre_plano       CHAR(007) ,
        c12_nombre_plano      CHAR(012) ,
        enter                 CHAR(001) ,
        gc_usuario            CHAR(015)

    DEFINE #glo #smallint
        gs_tipo_op            ,
        gs_procesa            ,
        gs_codigo_afore       SMALLINT

    DEFINE
        gi_tot_regs           INTEGER

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("RETC970.log")

    CALL init()
    CALL f_despliega_info() RETURNING gs_procesa

    IF gs_procesa THEN
        CALL f_abre_ventana()
        CALL primer_paso()      #-- Genera lote
        CALL segundo_paso()     #-- Actualiza tablas
    END IF
END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()
    LET HOY             = TODAY
    LET gi_tot_regs     = 0
    LET gs_tipo_op      = 52

    SELECT *
    INTO   g_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    SELECT codigo_afore,
           USER
    INTO   gs_codigo_afore,
           gc_usuario
    FROM   tab_afore_local

    LET G_LISTA          = g_seg_modulo.ruta_envio
    LET c7_nombre_plano  = "DETAV52"
    LET c12_nombre_plano = HOY USING"YYYYMMDD",".52P"

    ----- ESTADOS DE S0LICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.capturado
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "CAPTURADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.confirmado
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "CONFIRMADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.enviado
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "ENVIADO"

END FUNCTION

#---------------------------------------------------------------------------#
# f_despliega_info : Despliega la informacion que se usara para generar el  #
#                    archivo de la operacion 52 de parciales                #
#---------------------------------------------------------------------------#
FUNCTION f_despliega_info()

    DEFINE lar_ret ARRAY[6] OF RECORD
        regimen            LIKE ret_parcial_issste.regimen      ,
        desc_regimen       LIKE tab_regimen_issste.descripcion  ,
        num_cap            INTEGER                              ,
        num_conf           INTEGER                              ,
        num_prov           INTEGER
    END RECORD

    DEFINE lr_soli RECORD
        regimen            SMALLINT                                 ,
        edo_soli           LIKE ret_parcial_issste.estado_solicitud ,
        num_regs           INTEGER
    END RECORD

    DEFINE
        li_tot_prov        INTEGER

    DEFINE
        ls_ret              ,
        ls_cont             ,
        ls_flag             SMALLINT

    DEFINE
        lc_query            CHAR(1000)

    OPEN WINDOW retc9701 AT 4,4 WITH FORM "RETC9701" ATTRIBUTE (BORDER)
    DISPLAY " < CTRL-C > - SALIR                                      < ESC > - EJECUTAR  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC970  GENERACION DE LA OPERACION 52 RETIROS PARCIALES ISSSTE              " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

    FOR ls_cont = 1 TO 2
        INITIALIZE lar_ret[ls_cont].* TO NULL
    END FOR

    LET lar_ret[1].regimen  = "DT"
    LET lar_ret[2].regimen  = "RO"

    FOR ls_cont = 1 TO 2
        SELECT descripcion
        INTO   lar_ret[ls_cont].desc_regimen
        FROM   tab_regimen_issste
        WHERE  regimen = lar_ret[ls_cont].regimen

        LET lar_ret[ls_cont].num_cap  = 0
        LET lar_ret[ls_cont].num_conf = 0
        LET lar_ret[ls_cont].num_prov = 0
    END FOR

    LET li_tot_prov = 0
    LET ls_flag     = 1
    LET ls_cont     = 1

    LET lc_query = " SELECT CASE regimen ",
                      " WHEN 'DT' THEN 1 ",
                      " WHEN 'RO' THEN 2 ",
                      " END , ",
                   "        estado_solicitud ,  ",
                   "        COUNT(*)            ",
                   " FROM   ret_parcial_issste  ",
                   " WHERE  estado_solicitud IN (?,?) ",
                   " GROUP BY 1,2               ",
                   " ORDER BY 1,2               "

    PREPARE prp_datos_tot FROM lc_query
    DECLARE cur_datos_tot CURSOR FOR prp_datos_tot

    FOREACH cur_datos_tot USING gr_edo.capturado,
                                gr_edo.confirmado
                          INTO lr_soli.*

        LET ls_ret = lr_soli.regimen

        IF lr_soli.edo_soli = gr_edo.capturado THEN
            LET lar_ret[ls_ret].num_cap  = lr_soli.num_regs
        ELSE
            LET lar_ret[ls_ret].num_conf = lr_soli.num_regs
            LET lar_ret[ls_ret].num_prov = lr_soli.num_regs
            LET li_tot_prov = li_tot_prov + lr_soli.num_regs
        END IF

        LET ls_cont = ls_cont + 1

    END FOREACH

    IF li_tot_prov = 0 THEN
        PROMPT " NO EXISTEN REGISTROS PARA PROVISIONAR ... <ENTER> PARA SALIR " FOR CHAR enter
        LET ls_flag = 0
    ELSE
        CALL SET_COUNT(2)
        DISPLAY ARRAY lar_ret TO scr_provi.*

            ON KEY (INTERRUPT)
                PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
                LET ls_flag = 0
                EXIT DISPLAY

            -- Provision de las cuentas
            ON KEY (ESC)
                PROMPT "SE INCLUIRAN ", li_tot_prov, " REGISTROS EN EL ARCHIVO. ¿ESTA SEGURO? (S/N)  " FOR CHAR enter
                IF enter MATCHES "[sSnN]" THEN
                    IF enter MATCHES "[sS]" THEN #ff
                        LET ls_flag = 1
                    ELSE
                        PROMPT" PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
                        LET ls_flag = 0
                    END IF
                END IF

                EXIT DISPLAY
        END DISPLAY
    END IF

    CLOSE WINDOW retc9701

    RETURN ls_flag

END FUNCTION

#---------------------------------------------------------------------------#
# primer_paso : Obtiene los registros y genera el archivo de la operacion   #
#               52 de parciales                                             #
#---------------------------------------------------------------------------#
FUNCTION primer_paso()

    DEFINE lr_parcial RECORD LIKE ret_parcial_issste.*

    DEFINE ld_mto_ret92 LIKE ret_monto_par_issste.mto_a_pagar

    DEFINE
        borra_lineas    CHAR(100) ,
        lc_com          CHAR(200)

    -- -----------------------------------------------------------------------------

    LET G_LISTA_1 = G_LISTA CLIPPED,"/",c7_nombre_plano

    LET lc_com = "chmod 777 ", G_LISTA_1
    RUN lc_com

    START REPORT rpt_listado_op52 TO G_LISTA_1

    DECLARE cur_rep CURSOR FOR
    SELECT  A.*
    FROM    ret_parcial_issste A
    WHERE   A.estado_solicitud = gr_edo.confirmado

    FOREACH cur_rep INTO lr_parcial.*

        LET gi_tot_regs = gi_tot_regs + 1
        LET ld_mto_ret92    = 0
        
        IF lr_parcial.regimen = "DT" THEN
            SELECT mto_a_pagar
            INTO   ld_mto_ret92 
            FROM   ret_monto_par_issste
            WHERE  curp         = lr_parcial.curp
            AND    consecutivo  = lr_parcial.consecutivo
        END IF
        
        IF lr_parcial.num_concesion IS NULL THEN
            LET lr_parcial.num_concesion = 0
        END IF 
        
        OUTPUT TO REPORT rpt_listado_op52(lr_parcial.*, ld_mto_ret92)

    END FOREACH

    LET G_LISTA_2 = G_LISTA CLIPPED,"/",c12_nombre_plano

    FINISH REPORT rpt_listado_op52

    LET borra_lineas = "sed -e '/^$/d' ",G_LISTA_1 CLIPPED,">",G_LISTA_2 CLIPPED
    RUN borra_lineas

    LET lc_com = "cp ",G_LISTA_2 CLIPPED," ",G_LISTA_1 CLIPPED
    RUN lc_com

    LET lc_com = "chmod 777 ", G_LISTA_1
    RUN lc_com

    LET lc_com = "chmod 777 ", G_LISTA_2
    RUN lc_com

END FUNCTION

#---------------------------------------------------------------------------#
# segundo_paso : Actualiza el estado de las solicitudes enviadas y registra #
#                el envio del archivo de la operacion 52 de parciales       #
#---------------------------------------------------------------------------#
FUNCTION segundo_paso()

    DEFINE
        li_folio        INTEGER

    LET li_folio = f_ultimo_folio()

    #--
    UPDATE ret_parcial_issste
    SET    estado_solicitud = gr_edo.enviado ,
           folio            = li_folio
    WHERE  estado_solicitud = gr_edo.confirmado

    #--
    UPDATE ret_monto_par_issste
    SET    folio         = li_folio ,
           cve_operacion = gs_tipo_op
    WHERE  consecutivo IN ( SELECT consecutivo
                            FROM   ret_parcial_issste
                            WHERE  folio            = li_folio
                            AND    estado_solicitud = gr_edo.enviado )

    #--
    INSERT INTO ret_ctr_envio
    VALUES (li_folio                   ,
            "AV52"                     ,
            "CZAAV52 DETAV52 SUMAV52"  ,
            gr_edo.enviado             ,
            0                          ,
            HOY
            )

    DISPLAY "EL LOTE HA SIDO GENERADO EN LA RUTA : " AT 8,20
    DISPLAY g_seg_modulo.ruta_envio CLIPPED AT 9,20
    DISPLAY "                                                " AT 12,11
    DISPLAY "CON EL NOMBRE : ", c12_nombre_plano AT 11,20
    DISPLAY "FOLIO OP.52 : ", li_folio AT 13,22
    DISPLAY "REGISTROS PROCESADOS : ", gi_tot_regs AT 14,13

    PROMPT " PRESIONE <ENTER> PARA FINALIZAR " FOR CHAR enter

    CLOSE WINDOW retc9702

END FUNCTION

#---------------------------------------------------------------------------#
# f_abre_ventana : Abre la ventana donde se muestran los resultados de la   #
#                el envio del archivo de la operacion 52 de parciales       #
#---------------------------------------------------------------------------#
FUNCTION f_abre_ventana()

    OPEN WINDOW retc9702 AT 4,4 WITH FORM "RETC9702" ATTRIBUTE(BORDER)
    DISPLAY " < CTRL-C > - SALIR                                      < ESC > - EJECUTAR  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC970  GENERACION DE LA OPERACION 52 RETIROS PARCIALES ISSSTE             " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

END FUNCTION

#---------------------------------------------------------------------------#
# f_ultimo_folio : Obtiene el ultimo folio que se usara para procesar los   #
#                  registros de parciales                                   #
#---------------------------------------------------------------------------#
FUNCTION f_ultimo_folio()

    DEFINE
        li_ult_folio     INTEGER


    SELECT MAX(A.folio) + 1
    INTO   li_ult_folio
    FROM   glo_folio A

    INSERT INTO glo_folio
    VALUES (li_ult_folio)

    RETURN li_ult_folio


END FUNCTION

#---------------------------------------------------------------------------#
# rpt_listado_op52 : Genera el archivo plano de la op. 52 de Parciales      #
#---------------------------------------------------------------------------#
REPORT rpt_listado_op52(pr_parcial, pr_impt_ret_92)

    DEFINE
        pr_parcial RECORD LIKE ret_parcial_issste.*

    DEFINE 
        pr_impt_ret_92          DECIMAL(10,2)

    DEFINE
        c11_impt_ret_92         CHAR(011) ,
        c10_impt_ret_92         CHAR(010)

    DEFINE
        li_cont         SMALLINT

    -- -----------------------------------------------------------------------------

    OUTPUT
        PAGE LENGTH    1000
        LEFT MARGIN    0
        RIGHT MARGIN   0
        TOP MARGIN     0
        BOTTOM MARGIN  0

    FORMAT
        FIRST PAGE HEADER
            PRINT
                COLUMN 001, "01"                        ,--tipo de registro
                COLUMN 003, "04"                        ,--identificador de servicio
                COLUMN 005, "01"                        ,--tipo entidad origen
                COLUMN 007, gs_codigo_afore USING"&&&"  ,--clave entidad origen
                COLUMN 010, "03"                        ,--tipo entidad destino
                COLUMN 012, "001"                       ,--clave entidad destino
                COLUMN 015, HOY USING "YYYYMMDD"        ,--fecha de operacion
                COLUMN 023, 2 SPACES                    ,
                COLUMN 025, 3 SPACES                    ,
                COLUMN 028, 3 SPACES                    ,
                COLUMN 031, 3 SPACES                    ,
                COLUMN 034, 237 SPACES

    ON EVERY ROW
        LET li_cont = li_cont + 1

        PRINT
            COLUMN 001,"03"                                     ,--tipo de registro
            COLUMN 003,"04"                                     ,--ident. de servicio
            COLUMN 005,gs_tipo_op USING "&&"                    ,--ident. de operac
            COLUMN 007,pr_parcial.curp                          ,--curp
            COLUMN 025,pr_parcial.num_issste                    ,--num_issste
            COLUMN 033,pr_parcial.nss                           ,--nss
            COLUMN 044,pr_parcial.nombre                        ,--nombre
            COLUMN 084,pr_parcial.apellido_paterno              ,--apellido paterno
            COLUMN 124,pr_parcial.apellido_materno              ,--apellido materno
            COLUMN 164,"F"                                      ,--tipo de retiro
            COLUMN 165,pr_parcial.regimen                       ,--regimen
            COLUMN 167,"05"                                     ,--tipo de prestacion            
            COLUMN 169,pr_parcial.num_concesion USING "&&&&&&"  ,--numero de consecion            
            COLUMN 175,gs_codigo_afore USING "&&&"              ,--clave de afore            
            COLUMN 181,pr_parcial.sec_pension                   ,--sec_pension
            COLUMN 183,88 SPACES

-- Se comenta a petición de Coppel
-- Requerimiento CPL-1089
-- 6 de diciembre de 2012
{
        IF pr_parcial.regimen = "DT" THEN

            ----- Importe de Retiro 92 ----
            LET c11_impt_ret_92 = pr_impt_ret_92 USING "&&&&&&&&.&&"
            LET c10_impt_ret_92 = c11_impt_ret_92 [01,08],
                                  c11_impt_ret_92 [10,11]



            PRINT
                COLUMN 001,"05"                                     ,-- tipo de registro            
                COLUMN 003,pr_parcial.num_issste                    ,-- num_issste
                COLUMN 011,pr_parcial.nss                           ,-- nss
                COLUMN 022,pr_parcial.curp                          ,-- curp
                COLUMN 040,c10_impt_ret_92                          ,-- Importe retiro 92
                COLUMN 050,221 SPACES                
            
        END IF
}

    ON LAST ROW
        PRINT
            COLUMN 001, "09"                         ,--tipo de registro
            COLUMN 003, "04"                         ,--identificador de servicio
            COLUMN 005, gs_tipo_op USING "&&"        ,--identificado de operacion
            COLUMN 007, "01"                         ,--tipo entidad origen
            COLUMN 009, gs_codigo_afore USING "&&&"  ,--clave entidad origen
            COLUMN 012, "03"                         ,--tipo entidad destino
            COLUMN 014, "001"                        ,--clave entidad destino
            COLUMN 017, HOY USING "YYYYMMDD"         ,--fecha de operacion
            COLUMN 025, "AV "                        ,--area origen
            COLUMN 028, li_cont USING "&&&&&&"       ,--numero de registros
            COLUMN 034, 237 SPACES
END REPORT
