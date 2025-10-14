#################################################################################
#Owner             => E.F.P.                                                    #
#Programa PENC101  => GENERACION DE LA OPERACION 70 DE PENSION MINIMA           #
#                     GARANTIZADA                                               #
#Fecha creacion    => 5 DE ABRIL DE 2010                                        #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   =>                                                           #
#Actualizacion     =>                                                           #
#Sistema           => RET                                                       #
#################################################################################
DATABASE safre_af

GLOBALS

    DEFINE g_seg_modulo RECORD LIKE seg_modulo.*


    DEFINE gr_edo RECORD
        capturado             LIKE pen_estado_pmg.estado_solicitud ,
        provisionado          LIKE pen_estado_pmg.estado_solicitud ,
        enviado               LIKE pen_estado_pmg.estado_solicitud ,
        rechazado             LIKE pen_estado_pmg.estado_solicitud
    END RECORD

    DEFINE gr_genera_file RECORD
        insuficiencia       SMALLINT    ,
        no_renovacion       SMALLINT    ,
        conclusion          SMALLINT    ,
        agotamiento         SMALLINT    ,
        muerte              SMALLINT
    END RECORD


    DEFINE #glo #date
        HOY                   DATE

    DEFINE
        gdt_fec_val_trans     ,
        gdt_fec_oper          DATE


    DEFINE
        c12_nom_plano         CHAR(012) ,
        G_LISTA_DET           CHAR(100) ,
        G_LISTA_CZA           CHAR(100) ,
        G_LISTA_SUM           CHAR(100) ,
        enter                 CHAR(001) ,
        gc_usuario            CHAR(015)

    DEFINE #glo #smallint
        gs_tipo_op            ,
        gs_procesa            ,
        gs_codigo_afore       SMALLINT

    DEFINE
        gi_num_regs           ,
        gs_ult_folio          INTEGER

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("PENC101.log")

    CALL init() #i
    
    CALL f_despliega_datos() RETURNING gs_procesa, gr_genera_file.*


    IF gs_procesa THEN

        CALL f_abre_ventana()
        LET  c12_nom_plano = HOY USING "YYYYMMDD",".70P"

        CALL primer_paso()                  #-- Genera encabezado transaccional

        CALL segundo_paso()     #-- Genera el detalle del reporte
            RETURNING gi_num_regs

        CALL tercer_paso(gi_num_regs)       #-- Genera el sumario del reporte

        CALL cuarto_paso()                  #-- Concatena los archivos

        CALL quinto_paso(gi_num_regs)      #-- Actualiza tablas
    END IF

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    DEFINE
        lc_prepare      CHAR(300)

    LET HOY             = TODAY
    LET gs_tipo_op      = 70

    SELECT *
    INTO   g_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "pmg"

    ----- CODIGOS AFORES -----
    SELECT codigo_afore,
           USER
    INTO   gs_codigo_afore,
           gc_usuario
    FROM   tab_afore_local

    ----- ESTADOS DE S0LICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.capturado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "CAPTURADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.enviado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "ENVIADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.rechazado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "RECHAZADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.provisionado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "PROVISIONADO"

END FUNCTION


#---------------------------------------------------------------------------#
# f_despliega_datos : Despliega la informacion de los registros que se      #
#                     incluyen en el archivo de la op 70                    #
#---------------------------------------------------------------------------#
FUNCTION f_despliega_datos()

    DEFINE lar_notifica ARRAY[5] OF RECORD
        id_confirma        CHAR(1)                               ,
        clave              LIKE tab_tipo_notifica_pmg.clave      ,
        descripcion        LIKE tab_tipo_notifica_pmg.descripcion,
        tot_registros      INTEGER
    END RECORD

    DEFINE lr_genera RECORD
        insuficiencia       SMALLINT    ,
        no_renovacion       SMALLINT    ,
        conclusion          SMALLINT    ,
        agotamiento         SMALLINT    ,
        muerte              SMALLINT
    END RECORD
        
    DEFINE lr_cat_noti RECORD LIKE tab_tipo_notifica_pmg.*

    DEFINE
        ls_ret              ,
        ls_cont             ,
        ls_flag             SMALLINT

    DEFINE
        li_tot_elem         ,
        li_arr_elem         ,
        li_scr_elem         INTEGER

    DEFINE
        lc_marca            CHAR(1)     ,
        lc_query            CHAR(1000)


    OPEN WINDOW penc1011 AT 4,4 WITH FORM "PENC1011" ATTRIBUTE (BORDER)
    DISPLAY " <CTRL-C> Salir                                          <CTRL-E> Ejecutar  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " PENC101   GENERACION OPERACION 70 PENSION MINIMA GARANTIZADA               " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

    LET ls_flag     = 1
    LET ls_cont     = 1
    LET li_tot_elem = 0

    DECLARE cur_cat CURSOR FOR
        SELECT *
        FROM   tab_tipo_notifica_pmg
        ORDER BY 1

    FOREACH cur_cat INTO lr_cat_noti.*
        LET lar_notifica[ls_cont].clave         = lr_cat_noti.clave
        LET lar_notifica[ls_cont].descripcion   = lr_cat_noti.descripcion
        
        LET ls_cont = ls_cont + 1
    END FOREACH

    -- Obtenemos los registros para Insuficiencia
    SELECT NVL(COUNT(*),0)
    INTO   lar_notifica[1].tot_registros
    FROM   pen_detalle_op70 A       ,
           tab_tipo_notifica_pmg B
    WHERE  A.estado             = gr_edo.capturado
    AND    A.id_tipo_notifica   = B.clave
    AND    B.descripcion MATCHES '*INSUFICIENCIA*'

    IF lar_notifica[1].tot_registros = 0 THEN
        LET lar_notifica[1].id_confirma = " "
        LET lr_genera.insuficiencia     = 0
    ELSE 
        LET li_tot_elem = li_tot_elem + lar_notifica[1].tot_registros 
        LET lar_notifica[1].id_confirma = "x"
        LET lr_genera.insuficiencia     = 1
    END IF

    #-- Comentario por javier
    -- Aqui siguen los demas registros
    LET lr_genera.no_renovacion  = 0
    LET lr_genera.conclusion     = 0
    LET lr_genera.agotamiento    = 0
    LET lr_genera.muerte         = 0


    --- -------------
    IF li_tot_elem > 0 THEN
        CALL SET_COUNT(5)
        DISPLAY ARRAY lar_notifica TO scr_notifica.*    

            -- Ejecuta el proceso de generacion del lote
             ON KEY (CONTROL-E)
{
                CASE
                    WHEN lar_notifica[1].id_confirma = "x"
                        LET lr_genera.insuficiencia = 1

                    WHEN lar_notifica[2].id_confirma = "x"
                        LET lr_genera.no_renovacion = 1

                    WHEN lar_notifica[3].id_confirma = "x"
                        LET lr_genera.conclusion    = 1

                    WHEN lar_notifica[4].id_confirma = "x"
                        LET lr_genera.agotamiento   = 1

                    WHEN lar_notifica[5].id_confirma = "x"
                        LET lr_genera.muerte        = 1
                
                END CASE
}
                WHILE TRUE
                    PROMPT "¿ DESEA GENERAR EL ARCHIVO DE LA OP.70 ? (S/N) : " ATTRIBUTE(REVERSE) FOR CHAR enter
                    IF enter MATCHES "[sSnN]" THEN
                        IF enter MATCHES "[sS]" THEN
                            LET ls_flag = 1
                        END IF
                
                        EXIT WHILE
                    END IF
                END WHILE

                EXIT DISPLAY

            ON KEY (CONTROL-C, INTERRUPT)
                CALL f_error_msg("PROCESO CANCELADO")
                LET ls_flag = 0
                EXIT DISPLAY
       
        END DISPLAY 

    ELSE 
        CALL f_error_msg("NO EXISTEN REGISTROS PARA ENVIAR")
        LET ls_flag = 0
    END IF

    CLOSE WINDOW penc1011

    RETURN ls_flag, lr_genera.*

END FUNCTION

#---------------------------------------------------------------------------#
# primer_paso : Genera el encabezado transaccional del archivo de la op 43  #
#---------------------------------------------------------------------------#
FUNCTION primer_paso()

    LET G_LISTA_CZA = g_seg_modulo.ruta_envio CLIPPED, "/", gc_usuario CLIPPED, ".CZA_70P"

    START REPORT cza_tran TO G_LISTA_CZA
        OUTPUT TO REPORT cza_tran()
    FINISH REPORT cza_tran

END FUNCTION


#---------------------------------------------------------------------------#
# segundo_paso : Genera el detalle del archivo de la op 70                  #
#---------------------------------------------------------------------------#
FUNCTION segundo_paso()

    DEFINE lr_detalle_70 RECORD LIKE pen_detalle_op70.*

    DEFINE
        ruta_det_nss      CHAR(100)

    DEFINE
        ls_diag           SMALLINT
    
    DEFINE
        li_num_regs       INTEGER

    DISPLAY "GENERANDO ARCHIVO PLANO ..." AT 18,5 ATTRIBUTE(REVERSE)
    
    LET li_num_regs = 0
    LET ls_diag     = 0

    #-- Detalle general
    LET G_LISTA_DET = g_seg_modulo.ruta_envio CLIPPED, "/",
                                              gc_usuario CLIPPED,
                                              ".DET_70P"
    LET G_LISTA_DET = G_LISTA_DET CLIPPED

    START REPORT det_opera70 TO G_LISTA_DET

    --SELECCION DE LOS REGISTROS A REPORTAR
    DECLARE cur_det CURSOR FOR
    SELECT A.*
    FROM   pen_detalle_op70  A
    WHERE  A.estado         = gr_edo.capturado
    ORDER  BY A.id_tipo_notifica


    FOREACH cur_det INTO lr_detalle_70.*

        LET li_num_regs = li_num_regs + 1

        OUTPUT TO REPORT det_opera70(lr_detalle_70.*)
    END FOREACH

    FINISH REPORT det_opera70

    RETURN li_num_regs

END FUNCTION

#---------------------------------------------------------------------------#
# tercer_paso : Genera el sumario del archivo de la op 43                   #
#---------------------------------------------------------------------------#
FUNCTION tercer_paso(pi_num_regs)

    DEFINE
        pi_num_regs     INTEGER

    LET G_LISTA_SUM = g_seg_modulo.ruta_envio CLIPPED, "/", gc_usuario CLIPPED, ".SUM_70P"

    START REPORT sum_tran TO G_LISTA_SUM
        OUTPUT TO REPORT sum_tran(pi_num_regs) #st
    FINISH REPORT sum_tran

END FUNCTION

#---------------------------------------------------------------------------#
# cuarto_paso : Concatena los archivos generados para formar el lote        #
#---------------------------------------------------------------------------#
FUNCTION cuarto_paso()

    DEFINE
        comando     ,
        cat         CHAR(500)

    LET cat = "cat ", G_LISTA_CZA ,
                      G_LISTA_DET ,
                      G_LISTA_SUM ,
                     "> ",g_seg_modulo.ruta_envio CLIPPED,"/",c12_nom_plano

    RUN cat

    WHENEVER ERROR CONTINUE

    LET comando = "chmod 777 ", G_LISTA_CZA
    RUN comando

    LET comando = "chmod 777 ", G_LISTA_DET
    RUN comando

    LET comando = "chmod 777 ", G_LISTA_SUM
    RUN comando

    LET comando = "chmod 777 ", g_seg_modulo.ruta_envio CLIPPED,"/",c12_nom_plano
    RUN comando

    WHENEVER ERROR STOP

END FUNCTION

#---------------------------------------------------------------------------#
# quinto_paso : Actualiza las tablas de envio y muestra resultados en       #
#               la pantalla                                                 #
#---------------------------------------------------------------------------#
FUNCTION quinto_paso(pi_tot_regs)

    DEFINE
        pi_tot_regs         ,
        li_folio            INTEGER


    LET li_folio = f_obtiene_folio()

    UPDATE pen_detalle_op70
    SET    folio_envio      = li_folio       , 
           estado           = gr_edo.enviado
    WHERE  estado           = gr_edo.capturado
    AND    folio_envio      = 0

    INSERT INTO pen_envio
    VALUES ( li_folio               ,
             70                     ,
             HOY                    ,
             CURRENT HOUR TO SECOND ,
             c12_nom_plano          ,
             pi_tot_regs            ,
             gc_usuario             ,
             gr_edo.enviado             
            )

    DISPLAY "                           " AT 18,5
    DISPLAY " EL LOTE HA SIDO GENERADO EN LA RUTA : " AT 8,19
    DISPLAY g_seg_modulo.ruta_envio CLIPPED AT 10,20
    DISPLAY "                                                " AT 12,11
    DISPLAY " CON EL NOMBRE : ",c12_nom_plano AT 12,19
    DISPLAY " FOLIO         : ",li_folio AT 13,19

    PROMPT " PRESIONE <ENTER> PARA FINALIZAR " FOR CHAR enter

    CLOSE WINDOW penc1012

END FUNCTION

#---------------------------------------------------------------------------#
# f_abre_ventana : Abre la ventana donde se muestran los resultados de la   #
#                  generacion de la op. 43 de transferencias                #
#---------------------------------------------------------------------------#
FUNCTION f_abre_ventana()

    OPEN WINDOW penc1012 AT 4,4 WITH FORM "PENC1012" ATTRIBUTE(BORDER)
    DISPLAY " <CTRL-C> Salir                                             <ESC> Ejecutar  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " PENC101   GENERACION OPERACION 70 PENSION MINIMA GARANTIZADA               " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)


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
# f_obtiene_folio : Obtiene el ultimo folio para asignarse a la operacion   #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_folio()

    DEFINE
        li_ult_folio        INTEGER


    SELECT NVL(MAX(folio),0) + 1
    INTO   li_ult_folio
    FROM   glo_folio
    
    INSERT INTO glo_folio
    VALUES (li_ult_folio)
    
    RETURN li_ult_folio

END FUNCTION

#---------------------------------------------------------------------------#
# f_obtiene_nombres : Regresa los nombres del nss dado                      #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_nombres(pc_nss)

    DEFINE pc_nss LIKE pen_solicitud_pmg.nss

    DEFINE lr_afi RECORD
        ap_paterno      LIKE afi_mae_afiliado.paterno   ,
        ap_materno      LIKE afi_mae_afiliado.materno   ,
        nombres         LIKE afi_mae_afiliado.nombres
    END RECORD

    --  ------------------------------------------------------------------------

    SELECT paterno  ,
           materno  ,
           nombres
    INTO   lr_afi.*
    FROM   afi_mae_afiliado
    WHERE  n_seguro = pc_nss

    RETURN lr_afi.*

END FUNCTION



#---------------------------------------------------------------------------#
# concat_reportes : Dadas las rutas de los tres archivos de detalle         #
#                   temporales de los reportes, los va concatenando en uno  #
#                   solo que sera el archivo de detalle final               #
#---------------------------------------------------------------------------#
FUNCTION concat_reportes(lc_det_3, lc_det_4, lc_det_5, p_regs, ps_diag)

    DEFINE
        lc_det_3      ,
        lc_det_4      ,
        lc_det_5      CHAR(100)

    DEFINE
        ps_diag         ,
        p_regs          SMALLINT

    #--
    DEFINE
        ruta_tmp        ,
        ruta_det        CHAR(100)

    DEFINE
        com_cat         ,
        com_rm          CHAR(500)

    #---

    LET ruta_tmp    = g_seg_modulo.ruta_envio CLIPPED, "/", "rep_temporal.tmp"
    LET ruta_tmp    = ruta_tmp CLIPPED

    LET ruta_det    = g_seg_modulo.ruta_envio CLIPPED, "/", "detalle.tmp"
    LET ruta_det    = ruta_det CLIPPED

    #-- Preparamos el comando para borrar los reportes
    LET com_rm = "rm ", lc_det_3 CLIPPED, " ",
                        lc_det_4 CLIPPED, " ",
                        lc_det_5 CLIPPED
    LET com_rm = com_rm CLIPPED

    IF (ps_diag = 501 OR ps_diag = 507) THEN
        LET com_cat = "cat ", lc_det_3 CLIPPED, " ",
                              lc_det_4 CLIPPED, " ",
                              lc_det_5 CLIPPED, " > ", ruta_tmp
        LET com_cat = com_cat CLIPPED
    ELSE
        LET com_cat = "cat ", lc_det_3 CLIPPED, " ",
                              lc_det_5 CLIPPED, " > ", ruta_tmp
        LET com_cat = com_cat CLIPPED
    END IF

    #-- Concatenamos los archivos en uno solo y borramos los temporales
    RUN com_cat
    RUN com_rm

    #-- Acumulamos el archivo generado al reporte final
    IF p_regs > 1 THEN
        LET com_cat = "cat ", G_LISTA_DET, " ", ruta_tmp, " > ", ruta_det
        RUN com_cat

        LET com_cat = " mv ", ruta_det, " ", G_LISTA_DET
        RUN com_cat
    ELSE
        #-- Si es el primer registro procesado entonces creamos el archivo de detalle
        LET com_cat = " cp ", ruta_tmp, " ", G_LISTA_DET
        RUN com_cat
    END IF

    #-- Borramos el temporal generado para quedarnos solo con el archivo de detalle
    LET com_rm = "rm ", ruta_tmp
    RUN com_rm

END FUNCTION

#---------------------------------------------------------------------------#
# cza_tran : Reporte que genera el encabezado del archivo de la op. 70      #
#---------------------------------------------------------------------------#
REPORT cza_tran()

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
            PRINT
                COLUMN 001, "01"                              ,-- Tipo registro
                COLUMN 003, "04"                              ,-- Id de servicio
                COLUMN 005, "01"                              ,-- Entidad origen
                COLUMN 007, gs_codigo_afore USING"&&&"        ,-- Cve entidad origen
                COLUMN 010, "03"                              ,-- Entidad destino
                COLUMN 012, "001"                             ,-- Cve entidad destino
                COLUMN 015, HOY USING"YYYYMMDD"               ,-- Fecha operacion
                COLUMN 023, 320 SPACES                         -- Filler
END REPORT


#---------------------------------------------------------------------------#
# det_opera70 : Reporte que genera el detalle del archivo de la op. 70      #
#---------------------------------------------------------------------------#
REPORT det_opera70(pr_det_70)

    DEFINE
        pr_det_70           RECORD LIKE pen_detalle_op70.*

    DEFINE lr_nombre RECORD
        ap_paterno      LIKE afi_mae_afiliado.paterno   ,
        ap_materno      LIKE afi_mae_afiliado.materno   ,
        nombres         LIKE afi_mae_afiliado.nombres
    END RECORD

    DEFINE
        lc_11_mto_ret97         CHAR(011)   ,
        lc_11_mto_cv            CHAR(011)   ,
        lc_11_mto_cs            CHAR(011)   ,
        lc_11_mto_viv97         CHAR(011)   ,
        lc_11_saldo_ret97       CHAR(011)   ,
        lc_11_saldo_cv          CHAR(011)   ,
        lc_11_saldo_cs          CHAR(011)   ,
        lc_11_saldo_viv97       CHAR(011)   ,
        lc_11_mto_total_pmg     CHAR(011)   ,

        lc_10_mto_ret97         CHAR(010)   ,
        lc_10_mto_cv            CHAR(010)   ,
        lc_10_mto_cs            CHAR(010)   ,
        lc_10_mto_viv97         CHAR(010)   ,
        lc_10_saldo_ret97       CHAR(010)   ,
        lc_10_saldo_cv          CHAR(010)   ,
        lc_10_saldo_cs          CHAR(010)   ,
        lc_10_saldo_viv97       CHAR(010)   ,
        lc_10_mto_total_pmg     CHAR(010)   ,
        lc_fecha_nula           CHAR(008)   ,
        lc_fecha_primer_pago    CHAR(008)   ,
        lc_fecha_ultimo_pago    CHAR(008)   ,
        lc_fecha_agotamiento    CHAR(008)   ,
        lc_fecha_fallecimiento  CHAR(008)




    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW

            CALL f_obtiene_nombres(pr_det_70.nss) RETURNING lr_nombre.*
            LET lc_fecha_nula   = "00010101"
            
            
            -- Formateamos Clave de pension
            IF pr_det_70.cve_pension = "NA" THEN
                LET pr_det_70.cve_pension = "   "
            END IF
            
            -- Formateamos el monto retiro 97
            LET lc_11_mto_ret97 = pr_det_70.mto_retiro97 USING "&&&&&&&&.&&"
            LET lc_10_mto_ret97 = lc_11_mto_ret97[01,08],
                                  lc_11_mto_ret97[10,11]

            -- Formateamos el monto de cv
            LET lc_11_mto_cv    = pr_det_70.mto_cv USING "&&&&&&&&.&&"
            LET lc_10_mto_cv    = lc_11_mto_cv[01,08],
                                  lc_11_mto_cv[10,11]

            -- Formateamos el monto de cs
            LET lc_11_mto_cs    = pr_det_70.mto_cs USING "&&&&&&&&.&&"
            LET lc_10_mto_cs    = lc_11_mto_cs[01,08],
                                  lc_11_mto_cs[10,11]

            -- Formateamos el monto de vivienda
            LET lc_11_mto_viv97 = pr_det_70.mto_viv97 USING "&&&&&&&&.&&"
            LET lc_10_mto_viv97 = lc_11_mto_viv97[01,08],
                                  lc_11_mto_viv97[10,11]
                                  
            -- Formateamos el saldo de retiro 97
            LET lc_11_saldo_ret97   = pr_det_70.saldo_retiro97 USING "&&&&&&&&.&&"
            LET lc_10_saldo_ret97   = lc_11_saldo_ret97[01,08],
                                      lc_11_saldo_ret97[10,11]

            -- Formateamos el saldo de CV
            LET lc_11_saldo_cv      = pr_det_70.saldo_cv USING "&&&&&&&&.&&"
            LET lc_10_saldo_cv      = lc_11_saldo_cv[01,08],
                                      lc_11_saldo_cv[10,11]

            -- Formateamos el saldo de CS
            LET lc_11_saldo_cs      = pr_det_70.saldo_cs USING "&&&&&&&&.&&"
            LET lc_10_saldo_cs      = lc_11_saldo_cs[01,08],
                                      lc_11_saldo_cs[10,11]

            -- Formateamos el saldo de Vivienda
            LET lc_11_saldo_viv97      = pr_det_70.saldo_viv97 USING "&&&&&&&&.&&"
            LET lc_10_saldo_viv97      = lc_11_saldo_viv97[01,08],
                                         lc_11_saldo_viv97[10,11]

            -- Formateamos el Monto total pagado
            LET lc_11_mto_total_pmg    = pr_det_70.mto_total_pmg USING "&&&&&&&&.&&"
            LET lc_10_mto_total_pmg    = lc_11_mto_total_pmg[01,08],
                                         lc_11_mto_total_pmg[10,11]


            IF pr_det_70.fecha_primer_pago IS NULL THEN
                LET lc_fecha_primer_pago = lc_fecha_nula
            ELSE
                LET lc_fecha_primer_pago = pr_det_70.fecha_primer_pago USING "YYYYMMDD"    
            END IF
            
            IF pr_det_70.fecha_ultimo_pago IS NULL THEN
                LET lc_fecha_ultimo_pago = lc_fecha_nula
            ELSE
                LET lc_fecha_ultimo_pago = pr_det_70.fecha_ultimo_pago USING "YYYYMMDD"    
            END IF

            IF pr_det_70.fecha_agotamiento IS NULL THEN
                LET lc_fecha_agotamiento = lc_fecha_nula
            ELSE
                LET lc_fecha_agotamiento = pr_det_70.fecha_agotamiento USING "YYYYMMDD"    
            END IF

            IF pr_det_70.fecha_fallecimiento IS NULL THEN
                LET lc_fecha_fallecimiento = lc_fecha_nula
            ELSE
                LET lc_fecha_fallecimiento = pr_det_70.fecha_fallecimiento USING "YYYYMMDD"    
            END IF

        PRINT
            COLUMN 001, "03"                                            , -- Tipo de registro
            COLUMN 003, "04"                                            , -- ID de servicio
            COLUMN 005, gs_tipo_op USING "&&"                           , -- ID de operacion
            COLUMN 007, pr_det_70.id_tipo_notifica USING "&&&"          , 
            COLUMN 010, pr_det_70.nss                                   , -- NSS
            COLUMN 021, pr_det_70.curp                                  , -- CURP
            COLUMN 039, lr_nombre.nombres                               , -- Nombre Datamart
            COLUMN 079, lr_nombre.ap_paterno                            , -- Paterno Datamart
            COLUMN 119, lr_nombre.ap_materno                            , -- Materno Datamart
            COLUMN 159, pr_det_70.sec_pension USING "&&"                , -- Sec de Pension
            COLUMN 161, pr_det_70.tipo_retiro                           , -- Tipo de Retiro
            COLUMN 162, pr_det_70.regimen                               , -- Regimen
            COLUMN 164, pr_det_70.tipo_seguro                           , -- Tipo de Seguro
            COLUMN 166, pr_det_70.tipo_pension                          , -- Tipo de Pension
            COLUMN 168, pr_det_70.cve_pension                           , -- Clave de Pension
            COLUMN 171, pr_det_70.tipo_prestacion USING "&&"            , -- Tipo de Prestacion
            COLUMN 173, pr_det_70.fecha_ini_pen USING "YYYYMMDD"        , -- Fecha Inicio pension
            COLUMN 181, lc_fecha_primer_pago                            , -- Fecha primer pago
            COLUMN 189, lc_10_mto_ret97                                 , -- Monto retiro
            COLUMN 199, lc_10_mto_cv                                    , -- Monto CV
            COLUMN 209, lc_10_mto_cs                                    , -- Monto CS
            COLUMN 219, lc_10_mto_viv97                                 , -- Monto Vivienda
            COLUMN 229, pr_det_70.num_men_pagadas USING "&&&"           , -- Tipo Movimiento
            COLUMN 232, lc_10_saldo_ret97                               , -- Saldo retiro
            COLUMN 242, lc_10_saldo_cv                                  , -- Saldo CV
            COLUMN 252, lc_10_saldo_cs                                  , -- Saldo CS
            COLUMN 262, lc_10_saldo_viv97                               , -- Saldo Vivienda
            COLUMN 272, pr_det_70.num_men_calculadas USING "&&"         , -- Mensualidades calc
            COLUMN 274, lc_10_mto_total_pmg                             , -- Monto pagado PMG
            COLUMN 284, lc_fecha_ultimo_pago                            , -- Fecha ultimo pago
            COLUMN 294, lc_fecha_agotamiento                            , -- Fecha agotamiento pago
            COLUMN 302, lc_fecha_fallecimiento                          , -- Fecha fallecimiento
            COLUMN 310, 2  SPACES                                       , -- Resultado de la oper
            COLUMN 312, 3  SPACES                                       , -- Mot Rechazo 1
            COLUMN 315, 3  SPACES                                       , -- Mot Rechazo 2
            COLUMN 318, 3  SPACES                                         -- Mot Rechazo 3
END REPORT



#---------------------------------------------------------------------------#
# sum_tran : Reporte que genera el sumario del archivo de la op 70 de PMG   #
#---------------------------------------------------------------------------#
REPORT sum_tran(pi_num_regs)

    DEFINE
        pi_num_regs     INTEGER

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
            PRINT
                COLUMN 001, "09"                            ,-- Tipo Registro
                COLUMN 003, "04"                            ,-- Id de servicio
                COLUMN 005, "01"                            ,-- Entidad Origen
                COLUMN 007, gs_codigo_afore USING "&&&"     ,-- Cve Entidad origen
                COLUMN 010, "03"                            ,-- Entidad destino
                COLUMN 012, "001"                           ,-- Cve Entidad destino
                COLUMN 015, HOY USING "YYYYMMDD"            ,-- Fecha operacion
                COLUMN 023, pi_num_regs USING"&&&&&&"       ,-- Total registros
                COLUMN 029, 322 SPACES                       -- Filler
END REPORT
