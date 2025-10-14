################################################################################
#Owner             => E.F.P.                                                   #
#Programa RETC972  => REALIZA LA GENERACION Y MANDA A EJECUTAR LA RECEPCION DE #
#                     LA OPERACION 56 DE RETIROS PARCIALES ISSSTE              #
#Fecha creacion    => 10 DE NOVIEMBRE DE 2009                                  #
#By                => JAVIER GONZALEZ JERONIMO                                 #
#Fecha actualiz.   =>                                                          #
#Actualizacion     =>                                                          #
#Sistema           => RET                                                      #
################################################################################
DATABASE safre_af

GLOBALS

    DEFINE g_seg_modulo RECORD LIKE seg_modulo.*

    DEFINE gar_precio_acc ARRAY [20] OF RECORD #Arreglo para los precios_accion
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE gr_edo RECORD
        enviado               LIKE ret_estado_issste.estado_solicitud ,
        liquidado             LIKE ret_estado_issste.estado_solicitud
    END RECORD

    DEFINE #glo #date
        gdt_fecha_proc        ,
        HOY                   DATE

    DEFINE
        c12_nom_plano         CHAR(012) ,
        G_LISTA_DET           CHAR(100) ,
        G_LISTA_CZA           CHAR(100) ,
        G_LISTA_SUM           CHAR(100) ,
        enter                 CHAR(001) ,
        gc_usuario            CHAR(015)

    DEFINE
        gd_total_oper         DECIMAL(15,2)

    DEFINE
        gs_sieviv             ,
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

    CALL STARTLOG("RETC972.log")

    CALL init() #i

    OPEN WINDOW main_win AT 4,4 WITH 19 ROWS, 75 COLUMNS ATTRIBUTE(BORDER)
    DISPLAY " RETC972   GENERA/ENVIA ARCHIVO OP.56 RETIROS PARCIALES ISSSTE                " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    MENU "PARCIALES"
        COMMAND "Genera archivo" "Genera Archivo de la Operacion 56"
            CALL f_genera_archivo()

        COMMAND "Carga archivo" "Carga Archivo de respuesta de la Operacion 56"
            CALL f_ejecuta_carga()
        
        COMMAND "Salir" "Salir del Programa "
            EXIT MENU

    END MENU

    CLOSE WINDOW main_win

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    DEFINE
        lc_prepare      CHAR(300)

    LET HOY             = TODAY
    LET gs_sieviv       = 12
    LET gdt_fecha_proc  = HOY
    LET gs_tipo_op      = 56

    SELECT *
    INTO   g_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    ----- CODIGOS AFORES -----
    SELECT codigo_afore,
           USER
    INTO   gs_codigo_afore,
           gc_usuario
    FROM   tab_afore_local

    ----- ESTADOS DE S0LICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.enviado
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "ENVIADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.liquidado
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "LIQUIDADO"

    ----- FOLIO MAXIMO -----
    SELECT MAX(folio)
    INTO   gs_ult_folio
    FROM   ret_parcial_issste
    WHERE  estado_solicitud = gr_edo.liquidado

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_archivo : Ejecuta los pasos para generar el archivo de la op. 56 #
#                    de retiros parciales                                   #
#---------------------------------------------------------------------------#
FUNCTION f_genera_archivo()

    CALL f_captura_folio() RETURNING gs_procesa    ,
                                     gs_ult_folio
    
    IF gs_procesa THEN
    
        CALL f_abre_ventana()
    
        LET  c12_nom_plano = gdt_fecha_proc USING "YYYYMMDD",".56T"
    
        CALL primer_paso(gs_ult_folio)      #-- Genera encabezado transaccional
    
        CALL segundo_paso(gs_ult_folio)     #-- Genera el detalle del reporte
            RETURNING gi_num_regs, 
                      gd_total_oper
    
        CALL tercer_paso(gi_num_regs  ,
                         gd_total_oper)     #-- Genera el sumario del reporte
    
        CALL cuarto_paso()                  #-- Concatena los archivos
    
        CALL quinto_paso(gs_ult_folio)      #-- Actualiza tablas
    END IF

END FUNCTION

#---------------------------------------------------------------------------#
# f_captura_folio : Captura el folio que se usara en la generacion de la    #
#                   operacion 56 de retiros parciales                       #
#---------------------------------------------------------------------------#
FUNCTION f_captura_folio()

    DEFINE lr_cap RECORD
        folio_oper_56       LIKE ret_parcial_issste.folio
    END RECORD

    DEFINE
        li_tot_prov        INTEGER

    DEFINE
        ls_ret              ,
        ls_cont             ,
        ls_flag             SMALLINT

    DEFINE
        lc_query            CHAR(1000)

    LET ls_flag              = 1
    LET lr_cap.folio_oper_56 = gs_ult_folio

    OPEN WINDOW retc9721 AT 4,4 WITH FORM "RETC9721" ATTRIBUTE (BORDER)
    DISPLAY " <CTRL-C> Salir                                             <ESC> Ejecutar  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC972   GENERACION DE LA OPERACION 56 RETIRO PARCIAL ISSSTE              " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

    INPUT BY NAME lr_cap.* WITHOUT DEFAULTS

        BEFORE FIELD folio_oper_56

            DISPLAY BY NAME lr_cap.*

            IF lr_cap.folio_oper_56 IS NULL OR lr_cap.folio_oper_56 <= 0 THEN
                PROMPT " NO EXISTEN REGISTROS PARA GENERAR EL ARCHIVO ...<ENTER> PARA SALIR " FOR CHAR enter
                LET ls_flag = 0
                EXIT INPUT
            END IF

        AFTER FIELD folio_oper_56
            IF lr_cap.folio_oper_56 IS NULL OR lr_cap.folio_oper_56 <= 0 THEN
                ERROR "  CAMPO NO PUEDE SER NULO " ATTRIBUTE(NORMAL)
                NEXT FIELD folio_oper_56
            END IF

            SELECT "OK"
            FROM   ret_parcial_issste
            WHERE  folio = lr_cap.folio_oper_56
            GROUP BY 1

            IF SQLCA.SQLCODE = NOTFOUND  THEN
                ERROR " EL FOLIO INGRESADO NO EXISTE O NO CORRESPONDE A UNA TRANSFERENCIA "
                SLEEP 2
                ERROR ""
                NEXT FIELD folio_oper_56
            END IF

        ON KEY (ESC)
            IF lr_cap.folio_oper_56 IS NULL OR lr_cap.folio_oper_56 <= 0 THEN
                ERROR " CAMPO NO PUEDE SER NULO " ATTRIBUTE(NORMAL)
                NEXT FIELD folio_oper_56
            END IF

            SELECT "OK"
            FROM   ret_parcial_issste
            WHERE  folio = lr_cap.folio_oper_56
            GROUP BY 1

            IF SQLCA.SQLCODE = NOTFOUND  THEN
                ERROR " EL FOLIO INGRESADO NO EXISTE O NO CORRESPONDE A UNA TRANSFERENCIA "
                SLEEP 2
                ERROR ""
                NEXT FIELD folio_oper_56
            END IF

            WHILE TRUE
                PROMPT " ESTA SEGURO S/N ? " FOR CHAR enter
                IF enter MATCHES "[sSnN]" THEN
                    IF enter MATCHES "[sS]" THEN
                        LET ls_flag = 1
                    ELSE
                        PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
                        LET ls_flag = 0
                    END IF
                    EXIT WHILE
                END IF
            END WHILE

            EXIT INPUT

        ON KEY (CONTROL-C,INTERRUPT)
            PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
            LET ls_flag = 0
            EXIT INPUT

    END INPUT

    CLOSE WINDOW retc9721

    RETURN ls_flag, lr_cap.*

END FUNCTION

#---------------------------------------------------------------------------#
# primer_paso : Genera el encabezado transaccional del archivo de la op 56  #
#---------------------------------------------------------------------------#
FUNCTION primer_paso(pi_folio)

    DEFINE pi_folio LIKE ret_parcial_issste.folio

    LET G_LISTA_CZA = g_seg_modulo.ruta_envio CLIPPED, "/CZA_56"

    START REPORT cza_tran TO G_LISTA_CZA
        OUTPUT TO REPORT cza_tran(pi_folio)
    FINISH REPORT cza_tran

END FUNCTION

#---------------------------------------------------------------------------#
# segundo_paso : Genera el detalle del archivo de la op 56                  #
#---------------------------------------------------------------------------#
FUNCTION segundo_paso(pi_folio)

    DEFINE pi_folio LIKE ret_parcial_issste.folio

    DEFINE
        lr_par_issste           RECORD LIKE ret_parcial_issste.* ,
        lr_monto_issste         RECORD LIKE ret_monto_issste.*

    DEFINE lr_liquida RECORD
        subcta              LIKE dis_cuenta.subcuenta           , 
        mto_acc             LIKE dis_cuenta.monto_en_acciones   , 
        fec_conver          LIKE dis_cuenta.fecha_conversion
    END RECORD

    DEFINE lr_domicilio RECORD
        dom_calle            LIKE afi_domicilio.calle       ,
        dom_numero_ext       LIKE afi_domicilio.numero      ,
        dom_numero_int       LIKE afi_domicilio.depto       ,
        dom_colonia          LIKE afi_domicilio.colonia     ,
        deleg_desc           LIKE tab_delegacion.deleg_desc ,
        dom_codpos           LIKE afi_domicilio.codpos      ,
        estad_desc           LIKE tab_estado.estad_desc
    END RECORD
    
    DEFINE ld_acc_ant LIKE dis_cuenta.monto_en_acciones

    DEFINE
        ruta_det_nss            ,
        ruta_det_sie            CHAR(100)

    DEFINE
        li_num_regs             INTEGER

    DEFINE
        ls_sie                  SMALLINT 

    DEFINE 
        ld_total_operacion      DECIMAL(15,2),
        ld_total_antes_ret      DECIMAL(10,2),
        ld_pesos_liquida        DECIMAL(10,2)
        
        
    -------------------------------------------------------------------------

    LET li_num_regs         = 0
    LET ld_total_operacion  = 0

    #-- Determinamos la ubicacion de los reportes
    LET ruta_det_nss = g_seg_modulo.ruta_envio CLIPPED, "/", "DET-NSS-56-tmp"
    LET ruta_det_nss = ruta_det_nss CLIPPED

    LET ruta_det_sie = g_seg_modulo.ruta_envio CLIPPED, "/", "DET-SIE-56-tmp"
    LET ruta_det_sie = ruta_det_sie CLIPPED

    LET G_LISTA_DET = g_seg_modulo.ruta_envio CLIPPED, "/", "DET_ISS_56"
    LET G_LISTA_DET = G_LISTA_DET CLIPPED

    --SELECCION DE LOS REGISTROS A REPORTAR
    DECLARE cur_det CURSOR FOR
    SELECT A.*
    FROM   ret_parcial_issste  A
    WHERE  A.folio              = pi_folio
    AND    A.estado_solicitud   = gr_edo.liquidado
    ORDER BY A.curp

    FOREACH cur_det INTO lr_par_issste.*

        INITIALIZE lr_monto_issste.* TO NULL

        #-- Iniciamos los reportes
        START REPORT det_parcial TO ruta_det_nss
        START REPORT det_siefores TO ruta_det_sie

        LET li_num_regs = li_num_regs + 1

        SELECT B.*
        INTO   lr_monto_issste.*
        FROM   ret_monto_issste B
        WHERE  B.folio        = pi_folio
        AND    B.curp         = lr_par_issste.curp
        AND    B.consecutivo  = lr_par_issste.consecutivo
        
        LET ld_pesos_liquida    = 0
        LET ld_total_antes_ret  = 0
        LET ld_acc_ant          = 0
        LET ls_sie              = lr_monto_issste.siefore       
        
        -- Ciclo para obtener el monto pagado al trabajador (Se cambia el signo ya que es un retiro)
        DECLARE cur_liquida CURSOR FOR 
            SELECT subcuenta        , 
                   monto_en_acciones,
                   fecha_conversion
            FROM   dis_cuenta
            WHERE  curp             = lr_par_issste.curp
            AND    siefore          = lr_monto_issste.siefore
            AND    folio            = pi_folio            

        FOREACH cur_liquida INTO lr_liquida.*
            
            CALL f_obtiene_precios_accion(lr_liquida.fec_conver)
            
            LET ld_pesos_liquida    = ld_pesos_liquida + (lr_liquida.mto_acc * gar_precio_acc[ls_sie].precio_dia * -1)
            LET ld_total_operacion  = ld_total_operacion + (lr_liquida.mto_acc * gar_precio_acc[ls_sie].precio_dia * -1)
            
            -- Acumulamos el monto antes del retiro
            SELECT SUM(NVL(monto_en_acciones,0)) * gar_precio_acc[ls_sie].precio_dia
            INTO   ld_acc_ant
            FROM   dis_cuenta
            WHERE  nss              =  lr_par_issste.nss
            AND    siefore          =  lr_monto_issste.siefore
            AND    folio            <> pi_folio
            AND    fecha_conversion <= lr_liquida.fec_conver
            AND    subcuenta        =  lr_liquida.subcta
            
            LET ld_total_antes_ret  = ld_total_antes_ret + ld_acc_ant

        END FOREACH -- Monto pagado

        CALL f_datos_domicilio(lr_par_issste.nss) RETURNING lr_domicilio.*
        
        OUTPUT TO REPORT det_parcial(lr_par_issste.*    , 
                                     lr_domicilio.*     ,
                                     ld_pesos_liquida   ,
                                     ld_total_antes_ret ,
                                     lr_liquida.fec_conver
                                     )
        
        OUTPUT TO REPORT det_siefores(lr_par_issste.*, lr_monto_issste.*)

        FINISH REPORT det_parcial
        FINISH REPORT det_siefores

        #-- Una vez generados los archivos para el nss actual, los concatenamos en uno solo
        CALL concat_reportes(ruta_det_nss   ,
                             ruta_det_sie   ,
                             li_num_regs    )

    END FOREACH

    RETURN li_num_regs, ld_total_operacion

END FUNCTION

#---------------------------------------------------------------------------#
# tercer_paso : Genera el sumario del archivo de la op 56                   #
#---------------------------------------------------------------------------#
FUNCTION tercer_paso(pi_num_regs, pd_total_oper)

    DEFINE
        pi_num_regs     INTEGER

    DEFINE
        pd_total_oper   DECIMAL(15,2)

    LET G_LISTA_SUM = g_seg_modulo.ruta_envio CLIPPED,"/SUM56ISS"

    START REPORT sum_tran TO G_LISTA_SUM
        OUTPUT TO REPORT sum_tran(pi_num_regs, pd_total_oper)
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
FUNCTION quinto_paso(pi_folio)

    DEFINE
        pi_folio        INTEGER

    UPDATE ret_ctr_envio
    SET    status       = gr_edo.enviado,
           fecha_envio  = gdt_fecha_proc
    WHERE  folio            = pi_folio
    AND    tipo_operacion   = "AV56"

    DISPLAY " EL LOTE HA SIDO GENERADO EN LA RUTA : " AT 8,19
    DISPLAY g_seg_modulo.ruta_envio CLIPPED AT 10,20
    DISPLAY "                                                " AT 12,11
    DISPLAY " CON EL NOMBRE : ",c12_nom_plano AT 12,19

    PROMPT " PRESIONE <ENTER> PARA FINALIZAR " FOR CHAR enter

    CLOSE WINDOW retc9722

END FUNCTION

#---------------------------------------------------------------------------#
# f_abre_ventana : Abre la ventana donde se muestran los resultados de la   #
#                  generacion de la op. 56 de retiros parciales             #
#---------------------------------------------------------------------------#
FUNCTION f_abre_ventana()

    OPEN WINDOW retc9722 AT 4,4 WITH FORM "RETC9722" ATTRIBUTE(BORDER)
    DISPLAY " <CTRL-C> Salir                                             <ESC> Ejecutar  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC972   GENERACION DE LA OPERACION 56 RETIRO PARCIAL ISSSTE              " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

END FUNCTION


#---------------------------------------------------------------------------#
# concat_reportes : Dadas las rutas de los tres archivos de detalle         #
#                   temporales de los reportes, los va concatenando en uno  #
#                   solo que sera el archivo de detalle final               #
#---------------------------------------------------------------------------#
FUNCTION concat_reportes(lc_det_3, lc_det_4, p_regs)

    DEFINE
        lc_det_3      ,
        lc_det_4      CHAR(100)

    DEFINE
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
                        lc_det_4 CLIPPED
                        
    LET com_rm = com_rm CLIPPED


    #-- Concatenamos los archivos en uno solo y borramos los temporales
    LET com_cat = "cat ", lc_det_3 CLIPPED, " ",
                          lc_det_4 CLIPPED, " > ", ruta_tmp
    LET com_cat = com_cat CLIPPED

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
# f_datos_domicilio : Obtiene los datos de domicilio para el trabajador     #
#---------------------------------------------------------------------------#
FUNCTION f_datos_domicilio(p_nss)

    DEFINE
        p_nss    LIKE afi_domicilio.nss

    DEFINE vn_folio           LIKE afi_mae_afiliado.n_folio
    DEFINE vtipo_solicitud    LIKE afi_mae_afiliado.tipo_solicitud
    DEFINE vfactualiza        DATE
    DEFINE vrowid             INTEGER
    DEFINE vcont_dom          INTEGER

    DEFINE lr_afi RECORD #loc #lr_afi
        dom_calle            LIKE afi_domicilio.calle       ,
        dom_numero_ext       LIKE afi_domicilio.numero      ,
        dom_numero_int       LIKE afi_domicilio.depto       ,
        dom_colonia          LIKE afi_domicilio.colonia     ,
        deleg_desc           LIKE tab_delegacion.deleg_desc ,
        dom_codpos           LIKE afi_domicilio.codpos      ,
        estad_desc           LIKE tab_estado.estad_desc
    END RECORD
    
    SELECT A.n_folio, 
           A.tipo_solicitud
    INTO   vn_folio,
           vtipo_solicitud
    FROM   afi_mae_afiliado A
    WHERE  A.n_seguro = p_nss

    SELECT MAX(factualiza)
    INTO   vfactualiza
    FROM   afi_domicilio
    WHERE  n_folio        = vn_folio
    AND    tipo_solicitud = vtipo_solicitud
    AND    nss            = p_nss
    AND    marca_envio    = "X"
    
    SELECT MAX(ROWID)
    INTO   vrowid
    FROM   afi_domicilio
    WHERE  n_folio        = vn_folio
    AND    tipo_solicitud = vtipo_solicitud
    AND    nss            = p_nss
    AND    marca_envio    = "X"
    AND    factualiza     = vfactualiza

    SELECT A.calle      ,
           A.numero     ,
           A.depto      ,
           A.colonia    ,
           B.deleg_desc ,
           A.codpos     ,
           C.estad_desc
    INTO   lr_afi.* 
    FROM   afi_domicilio A, 
    OUTER  tab_delegacion B, 
    OUTER  tab_estado C
    WHERE  A.nss            = p_nss
    AND    A.n_folio        = vn_folio
    AND    A.tipo_solicitud = vtipo_solicitud
    AND    A.delega         = B.deleg_cod
    AND    A.estado         = C.estad_cod
    AND    A.marca_envio    = "X"
    AND    A.factualiza     = vfactualiza
    AND    A.rowid          = vrowid
    GROUP BY 1,2,3,4,5,6,7

    RETURN lr_afi.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_obtiene_precios_accion : Obtiene los precios de accion a la fecha dada  #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_precios_accion(fecha_precios)

    DEFINE #loc #date
        fecha_precios         DATE

    DEFINE #loc #char
        v_precios_accion      CHAR(100)

    DEFINE lr_precio_acc RECORD #loc #lr_precio_acc Arreglo para los precios_accion
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE #loc #char
        lc_mensaje            CHAR(100) ,
        lc_siefore            CHAR(002)

    DEFINE #loc #smallint
        ls_sie                SMALLINT

    LET ls_sie = 1

    LET v_precios_accion = " EXECUTE FUNCTION fn_verifica_precio_accion_isss(?)"
    PREPARE eje_precios_accion FROM v_precios_accion

    DECLARE c_precios CURSOR FOR eje_precios_accion
    FOREACH c_precios USING fecha_precios
                      INTO lr_precio_acc.*

    IF lr_precio_acc.estado <> 0 THEN
        LET lc_siefore = lr_precio_acc.siefore
    
        LET lc_mensaje = " FALTAN PRECIOS DE ACCION: FECHA ", lr_precio_acc.fecha USING "DD/MM/yyyy",
                         " -- SIEFORE ", lc_siefore CLIPPED
    
        PROMPT lc_mensaje FOR CHAR enter
        EXIT PROGRAM
    ELSE
        LET ls_sie                    = lr_precio_acc.siefore
        LET gar_precio_acc[ls_sie].*  = lr_precio_acc.*
    END IF


    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# f_ejecuta_carga : Ejecuta el programa RETC973 que realiza la carga del    #
#                   archivo de respuesta de la operacion 56                 #
#---------------------------------------------------------------------------#
FUNCTION f_ejecuta_carga()

    DEFINE 
        lc_cmd      CHAR(100)
        
    LET lc_cmd = " fglgo RETC973.4gi "
    RUN lc_cmd

END FUNCTION

#---------------------------------------------------------------------------#
# cza_tran : Reporte que genera el encabezado del archivo de la op. 56      #
#---------------------------------------------------------------------------#
REPORT cza_tran(pi_folio)

    DEFINE
        pi_folio              INTEGER

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
                COLUMN 015, gdt_fecha_proc USING"YYYYMMDD"    ,-- Fecha operacion
                COLUMN 023, 2 SPACES                          ,-- Resultado de la operacion
                COLUMN 025, 3 SPACES                          ,-- Motivo Rechazo 1
                COLUMN 028, 3 SPACES                          ,-- Motivo Rechazo 2
                COLUMN 031, 3 SPACES                          ,-- Motivo Rechazo 3
                COLUMN 034, 567 SPACES                         -- Filler
END REPORT


#---------------------------------------------------------------------------#
# det_parcial : Reporte que genera el detalle del archivo de la op. 56      #
#---------------------------------------------------------------------------#
REPORT det_parcial(pr_parcial,pr_afiliado,pr_mto_pagado,pr_ant_ret,pdt_fec_liquida)

    DEFINE
        pr_parcial RECORD LIKE ret_parcial_issste.*

    DEFINE pr_afiliado RECORD #loc #pr_afiliado
        dom_calle            LIKE afi_domicilio.calle       ,
        dom_numero_ext       LIKE afi_domicilio.numero      ,
        dom_numero_int       LIKE afi_domicilio.depto       ,
        dom_colonia          LIKE afi_domicilio.colonia     ,
        deleg_desc           LIKE tab_delegacion.deleg_desc ,
        dom_codpos           LIKE afi_domicilio.codpos      ,
        estad_desc           LIKE tab_estado.estad_desc
    END RECORD

    DEFINE 
        pr_mto_pagado         DECIMAL(10,2),
        pr_ant_ret            DECIMAL(10,2)

    DEFINE 
        pdt_fec_liquida       DATE

    DEFINE ld_mto_pagado LIKE ret_monto_par_issste.mto_a_pagar

    DEFINE
        c15_mto_const         CHAR(015) ,
        c10_impt_pagado       CHAR(010) ,
        c11_impt_pagado       CHAR(011) ,
        c10_ant_ret           CHAR(010) ,
        c11_ant_ret           CHAR(011)

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW

            ----- Importe pagado al trabajador ----
            LET c11_impt_pagado = pr_mto_pagado USING "&&&&&&&&.&&"
            LET c10_impt_pagado = c11_impt_pagado [01,08],
                                  c11_impt_pagado [10,11]

            ----- Saldo anterior al retiro ----
            LET c11_ant_ret     = pr_ant_ret USING "&&&&&&&&.&&"
            LET c10_ant_ret     = c11_ant_ret [01,08],
                                  c11_ant_ret [10,11]

            -- Si el consecutivo excede los 6 decimales se manda en ceros
            IF pr_parcial.consecutivo > 999999 THEN
                LET pr_parcial.consecutivo = 0
            END IF

        PRINT
            COLUMN 001, "03"                                            , -- Tipo de registro
            COLUMN 003, "04"                                            , -- ID de servicio
            COLUMN 005, gs_tipo_op USING "&&"                           , -- ID de operacion
            COLUMN 007, pr_parcial.num_issste                           , -- Numero issste
            COLUMN 015, pr_parcial.nss                                  , -- NSS
            COLUMN 026, pr_parcial.curp                                 , -- CURP
            COLUMN 044, pr_parcial.nombre                               , -- Nombre
            COLUMN 084, pr_parcial.apellido_paterno                     , -- Apellido Paterno
            COLUMN 124, pr_parcial.apellido_materno                     , -- Apellido Materno
            COLUMN 164, pr_parcial.regimen                              , -- Regimen
            COLUMN 166, "F"                                             , -- Tipo de Retiro
            COLUMN 167, "05"                                            , -- Tipo de Prestacion
            COLUMN 169, pr_parcial.fecha_solicitud USING "YYYYMMDD"     , -- Fecha Solicitud
            COLUMN 177, pr_parcial.consecutivo USING"&&&&&&"            , -- Consecutivo
            COLUMN 183, c10_impt_pagado                                 , -- Importe pagado
            COLUMN 193, c10_ant_ret                                     , -- Saldo anterior al retiro
            COLUMN 203, pdt_fec_liquida USING"YYYYMMDD"                 , -- Fecha valuacion
            COLUMN 214, pr_afiliado.dom_calle                           , -- calle
            COLUMN 279, pr_afiliado.dom_numero_ext                      , -- num_ext
            COLUMN 294, pr_afiliado.dom_numero_int                      , -- num_int
            COLUMN 309, pr_afiliado.dom_colonia                         , -- colonia
            COLUMN 374, pr_afiliado.deleg_desc                          , -- delegacion
            COLUMN 439, pr_afiliado.dom_codpos                          , -- cp
            COLUMN 444, pr_afiliado.estad_desc                          , -- estado
            COLUMN 509, 81 SPACES                                       , -- filler
            COLUMN 590, 02 SPACES                                       , -- Resultado
            COLUMN 592, 03 SPACES                                       , -- Rechazo 1
            COLUMN 595, 03 SPACES                                       , -- Rechazo 2
            COLUMN 598, 03 SPACES                                         -- Rechazo 3

END REPORT

#---------------------------------------------------------------------------#
# det_siefores : Reporte que genera el det de siefore del archivo de la op43#
#---------------------------------------------------------------------------#
REPORT det_siefores(pr_parcial, pr_montos)

    DEFINE pr_parcial RECORD LIKE ret_parcial_issste.*
    DEFINE pr_montos RECORD LIKE ret_monto_issste.*

    DEFINE
        c14_impt_ret_08     CHAR(14) ,
        c14_impt_ces_vej    CHAR(14) ,
        c14_impt_sar92      CHAR(14) 

    DEFINE
        c15_impt_ret_08     CHAR(15) ,
        c15_impt_ces_vej    CHAR(15) ,
        c15_impt_sar92      CHAR(15) 

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW

            #-- Obtenemos el valor de las Acciones de Retiro 08
            IF pr_montos.acc_ret08 IS NULL THEN
                LET pr_montos.acc_ret08 = 0
            END IF

            LET c15_impt_ret_08 = pr_montos.acc_ret08 USING "&&&&&&&&.&&&&&&"
            LET c14_impt_ret_08 = c15_impt_ret_08[01,08],
                                  c15_impt_ret_08[10,15]

            #-- Obtenemos el valor de las Acciones de CV
            IF pr_montos.acc_cv IS NULL THEN
                LET pr_montos.acc_cv = 0
            END IF

            LET c15_impt_ces_vej = pr_montos.acc_cv USING "&&&&&&&&.&&&&&&"
            LET c14_impt_ces_vej = c15_impt_ces_vej[01,08],
                                   c15_impt_ces_vej[10,15]

            #-- Obtenemos el valor de las Acciones de SAR 92
            IF pr_montos.acc_ret92 IS NULL THEN
                LET pr_montos.acc_ret92 = 0
            END IF

            LET c15_impt_sar92 = pr_montos.acc_ret92 USING "&&&&&&&&.&&&&&&"
            LET c14_impt_sar92 = c15_impt_sar92[01,08],
                                 c15_impt_sar92[10,15]


        PRINT
            COLUMN 001, "04"                                    , -- Tipo de registro
            COLUMN 003, pr_parcial.num_issste                   , -- Numero issste
            COLUMN 011, pr_parcial.nss                          , -- NSS
            COLUMN 022, pr_parcial.curp                         , -- CURP
            COLUMN 040, pr_montos.siefore USING "&&"            , -- Clave de siefore
            COLUMN 042, c14_impt_ret_08                         , -- Acciones de retiro 2008
            COLUMN 056, c14_impt_ces_vej                        , -- Acciones de CV
            COLUMN 070, c14_impt_sar92                          , -- Acciones de SAR 92
            COLUMN 084, 517 SPACES                                -- Filler

END REPORT

#---------------------------------------------------------------------------#
# sum_tran : Reporte que genera el sumario del archivo de la op 56          #
#---------------------------------------------------------------------------#
REPORT sum_tran(pi_num_regs, pd_impt_total)

    DEFINE
        pi_num_regs         INTEGER

    DEFINE
        pd_impt_total       DECIMAL(15,2)

    DEFINE
        c16_imp_tot_oper      CHAR(16) ,
        c15_imp_tot_oper      CHAR(15)

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW

            ----- Importe total de los retiros ----
            LET c16_imp_tot_oper = pd_impt_total USING "&&&&&&&&&&&&&.&&"
            LET c15_imp_tot_oper = c16_imp_tot_oper[01,13],
                                   c16_imp_tot_oper[15,16]

            PRINT
                COLUMN 001, "09"                            ,-- Tipo Registro
                COLUMN 003, "04"                            ,-- Id de servicio
                COLUMN 005, gs_tipo_op USING "&&"           ,-- Id de Operacion
                COLUMN 007, "01"                            ,-- Entidad Origen
                COLUMN 009, gs_codigo_afore USING "&&&"     ,-- Cve Entidad origen
                COLUMN 012, "03"                            ,-- Entidad destino
                COLUMN 014, "001"                           ,-- Cve Entidad destino
                COLUMN 017, gdt_fecha_proc USING "YYYYMMDD" ,-- Fecha Proceso
                COLUMN 025, "AV "                           ,-- Area origen
                COLUMN 028, pi_num_regs USING"&&&&&&"       ,-- Total registros
                COLUMN 034, c15_imp_tot_oper                ,-- Suma importes
                COLUMN 049, 552 SPACES                       -- Filler
END REPORT
