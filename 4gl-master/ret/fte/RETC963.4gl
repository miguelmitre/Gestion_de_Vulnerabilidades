################################################################################
#Owner             => E.F.P.                                                   #
#Programa RETC963  => GENERACION DEL ARCHIVO DE MONTOS LIQUIDADOS DE           #
#                     TRANFERENCIAS ISSSTE                                     #
#Fecha creacion    => 29 DE OCTUBRE DE 2009                                    #
#By                => JAVIER GONZALEZ JERONIMO                                 #
#Fecha actualiz.   =>                                                          #
#Actualizacion     =>                                                          #
#Sistema           => RET                                                      #
################################################################################
DATABASE safre_af

GLOBALS

    DEFINE g_seg_modulo RECORD LIKE seg_modulo.*


    DEFINE gr_edo RECORD
        provisionado          LIKE ret_estado_issste.estado_solicitud ,
        enviado               LIKE ret_estado_issste.estado_solicitud ,
        liquidado             LIKE ret_estado_issste.estado_solicitud
    END RECORD

    DEFINE
        HOY                   ,
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
        gs_procesa            ,
        gs_peiss              ,
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

    CALL STARTLOG("RETC963.log")

    CALL init() #i
    CALL f_captura_folio() RETURNING gs_procesa    ,
                                     gs_ult_folio

    IF gs_procesa THEN
        CALL f_obten_fechas(gs_ult_folio)
            RETURNING gdt_fec_val_trans, gdt_fec_oper     

        CALL f_abre_ventana()
        LET  c12_nom_plano = gdt_fec_oper USING "YYYYMMDD",".MONTRAN"
        
        CALL primer_paso(gs_ult_folio)      #--Genera encabezado transaccional

        CALL segundo_paso(gs_ult_folio)     #-- Genera el detalle del reporte
            RETURNING gi_num_regs

        CALL tercer_paso(gi_num_regs)       #-- Genera el sumario del reporte

        CALL cuarto_paso()                  #-- Concatena los archivos

        CALL quinto_paso(gs_ult_folio)      #-- Muestra informacion de generacion
    END IF

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    LET HOY = TODAY

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

    LET gs_peiss = 578 -- Clave PENSION ISSSTE

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.enviado
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "ENVIADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.liquidado
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "LIQUIDADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.provisionado
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "PROVISIONADO"

    ----- FOLIO MAXIMO -----
    SELECT MAX(folio)
    INTO   gs_ult_folio
    FROM   ret_trans_issste
    WHERE  estado_solicitud = gr_edo.liquidado

END FUNCTION

#---------------------------------------------------------------------------#
# f_captura_folio : Captura el folio que se usara en la generacion del      #
#                   archivo de montos liquidados de transferencias          #
#---------------------------------------------------------------------------#
FUNCTION f_captura_folio()

    DEFINE lr_cap RECORD
        folio_liquida   LIKE ret_trans_issste.folio
    END RECORD

    DEFINE
        ls_flag             SMALLINT

    LET ls_flag              = 1
    LET lr_cap.folio_liquida = gs_ult_folio

    OPEN WINDOW retc9631 AT 4,4 WITH FORM "RETC9631" ATTRIBUTE (BORDER)
    DISPLAY " <CTRL-C> Salir                                             <ESC> Ejecutar  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC963   GENERACION DEL ARCHIVO DE MONTOS LIQUIDADOS TRANSF ISSSTE        " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

    INPUT BY NAME lr_cap.* WITHOUT DEFAULTS
        
        BEFORE FIELD folio_liquida
            
            DISPLAY BY NAME lr_cap.*
            
            IF lr_cap.folio_liquida IS NULL OR lr_cap.folio_liquida <= 0 THEN
                PROMPT " NO EXISTEN REGISTROS PARA GENERAR EL ARCHIVO ...<ENTER> PARA SALIR " FOR CHAR enter
                LET ls_flag = 0
                EXIT INPUT
            END IF

        AFTER FIELD folio_liquida
            IF lr_cap.folio_liquida IS NULL OR lr_cap.folio_liquida <= 0 THEN
                ERROR "  CAMPO NO PUEDE SER NULO " ATTRIBUTE(NORMAL)
                NEXT FIELD folio_liquida
            END IF

            SELECT "OK"
            FROM   ret_trans_issste
            WHERE  folio            = lr_cap.folio_liquida
            AND    estado_solicitud = gr_edo.liquidado
            GROUP BY 1

            IF SQLCA.SQLCODE = NOTFOUND  THEN
                ERROR " EL FOLIO INGRESADO NO EXISTE O NO HA SIDO LIQUIDADO "
                SLEEP 2
                ERROR ""
                NEXT FIELD folio_liquida
            END IF
            
        ON KEY (ESC)
            IF lr_cap.folio_liquida IS NULL OR lr_cap.folio_liquida <= 0 THEN
                ERROR " CAMPO NO PUEDE SER NULO " ATTRIBUTE(NORMAL)
                NEXT FIELD folio_liquida
            END IF

            SELECT "OK"
            FROM   ret_trans_issste
            WHERE  folio            = lr_cap.folio_liquida
            AND    estado_solicitud = gr_edo.liquidado
            GROUP BY 1

            IF SQLCA.SQLCODE = NOTFOUND  THEN
                ERROR " EL FOLIO INGRESADO NO EXISTE O NO HA SIDO LIQUIDADO "
                SLEEP 2
                ERROR ""
                NEXT FIELD folio_liquida
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

    CLOSE WINDOW retc9631

    RETURN ls_flag, lr_cap.*

END FUNCTION

#---------------------------------------------------------------------------#
# primer_paso : Genera el encabezado del archivo de montos liquidados       #
#---------------------------------------------------------------------------#
FUNCTION primer_paso(pi_folio)

    DEFINE pi_folio LIKE ret_trans_issste.folio

    LET G_LISTA_CZA = g_seg_modulo.ruta_envio CLIPPED, "/CZA_ISS"

    START REPORT cza_tran TO G_LISTA_CZA
        OUTPUT TO REPORT cza_tran(pi_folio)
    FINISH REPORT cza_tran

END FUNCTION

#---------------------------------------------------------------------------#
# segundo_paso : Genera el detalle del archivo de montos liquidados         #
#---------------------------------------------------------------------------#
FUNCTION segundo_paso(pi_folio)

    DEFINE pi_folio LIKE ret_trans_issste.folio

    DEFINE
        lr_transf_issste        RECORD LIKE ret_trans_issste.*

    DEFINE lr_liquida RECORD
        nss             LIKE ret_trans_issste.nss               ,
        curp            LIKE ret_trans_issste.curp              ,
        consec          LIKE ret_trans_issste.consecutivo       ,
        sec_pension     LIKE ret_trans_issste.sec_pension       ,
        mto_const       LIKE ret_trans_issste.mto_solic_issste  ,
        cve_aseg        LIKE ret_trans_issste.cve_aseguradora   ,
        siefore         LIKE dis_cuenta.siefore                 ,
        mto_ret08       LIKE dis_cuenta.monto_en_pesos          ,
        mto_cv          LIKE dis_cuenta.monto_en_pesos          ,
        mto_ahsol       LIKE dis_cuenta.monto_en_pesos          ,
        acc_ret08       LIKE dis_cuenta.monto_en_acciones       ,
        acc_cv          LIKE dis_cuenta.monto_en_acciones       ,
        acc_ahsol       LIKE dis_cuenta.monto_en_acciones
    END RECORD
    
    DEFINE ld_pesos LIKE dis_cuenta.monto_en_pesos
    DEFINE ld_acc   LIKE dis_cuenta.monto_en_acciones
    
    DEFINE
        ls_subcta           SMALLINT
    
    DEFINE
        ruta_det_nss        CHAR(100)

    DEFINE
        li_num_regs         INTEGER
    

    LET li_num_regs = 0

    #-- Determinamos la ubicacion de los reportes
    LET G_LISTA_DET = g_seg_modulo.ruta_envio CLIPPED, "/", "DET_ISS_MONTRAN"
    LET G_LISTA_DET = G_LISTA_DET CLIPPED

    #-- Iniciamos los reportes
    START REPORT rpt_det_montran TO G_LISTA_DET

    --SELECCION DE LOS REGISTROS A REPORTAR
    DECLARE cur_det CURSOR FOR
    SELECT A.nss                ,
           A.curp               ,
           A.consecutivo        ,
           A.sec_pension        ,
           A.mto_solic_issste   ,
           A.cve_aseguradora
    FROM   ret_trans_issste A
    WHERE  A.folio            = pi_folio
    AND    A.estado_solicitud = gr_edo.liquidado
    ORDER  BY A.tipo_retiro
    
    FOREACH cur_det INTO lr_liquida.nss          ,
                         lr_liquida.curp         ,
                         lr_liquida.consec       ,
                         lr_liquida.sec_pension  ,
                         lr_liquida.mto_const    ,
                         lr_liquida.cve_aseg

        LET lr_liquida.mto_ret08    = 0
        LET lr_liquida.acc_ret08    = 0
        LET lr_liquida.mto_cv       = 0
        LET lr_liquida.acc_cv       = 0
        LET lr_liquida.mto_ahsol    = 0
        LET lr_liquida.acc_ahsol    = 0

        
        -- cursor para los montos
        DECLARE cur_mto CURSOR FOR 
            SELECT siefore,
                   subcuenta,
                   monto_en_pesos * -1,
                   monto_en_acciones * -1
            FROM   dis_cuenta
            WHERE  folio            = pi_folio
            AND    nss              = lr_liquida.nss 
            AND   consecutivo_lote  = lr_liquida.consec 
            AND   subcuenta NOT IN (19,14,35)

        FOREACH cur_mto INTO lr_liquida.siefore     ,
                             ls_subcta              ,
                             ld_pesos               ,
                             ld_acc  
            CASE 
                WHEN ls_subcta = 30
                    LET lr_liquida.mto_ret08 = ld_pesos
                    LET lr_liquida.acc_ret08 = ld_acc

                WHEN (ls_subcta = 31) OR (ls_subcta = 32)
                    LET lr_liquida.mto_cv   = ld_pesos + lr_liquida.mto_cv
                    LET lr_liquida.acc_cv   = ld_acc + lr_liquida.acc_cv
                
                WHEN (ls_subcta = 33) OR (ls_subcta = 34)
                    LET lr_liquida.mto_ahsol    = ld_pesos + lr_liquida.mto_ahsol
                    LET lr_liquida.acc_ahsol    = ld_acc + lr_liquida.acc_ahsol
            END CASE

        END FOREACH

        LET li_num_regs = li_num_regs + 1
        OUTPUT TO REPORT rpt_det_montran(lr_liquida.*)
 
    END FOREACH

    FINISH REPORT rpt_det_montran

    RETURN li_num_regs

END FUNCTION

#---------------------------------------------------------------------------#
# tercer_paso : Genera el sumario del archivo de montos liquidados          #
#---------------------------------------------------------------------------#
FUNCTION tercer_paso(pi_num_regs)

    DEFINE 
        pi_num_regs     INTEGER

    LET G_LISTA_SUM = g_seg_modulo.ruta_envio CLIPPED,"/SUMTRISS"

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
# quinto_paso : Muestra resultados de generacion en la pantalla             #
#---------------------------------------------------------------------------#
FUNCTION quinto_paso(pi_folio)

    DEFINE 
        pi_folio        INTEGER    


    DISPLAY " EL LOTE HA SIDO GENERADO EN LA RUTA : " AT 8,19
    DISPLAY g_seg_modulo.ruta_envio CLIPPED AT 10,20
    DISPLAY "                                                " AT 12,11
    DISPLAY " CON EL NOMBRE : ",c12_nom_plano AT 12,19

    PROMPT " PRESIONE <ENTER> PARA FINALIZAR " FOR CHAR enter
    
    CLOSE WINDOW retc9632

END FUNCTION

#---------------------------------------------------------------------------#
# f_abre_ventana : Abre la ventana donde se muestran los resultados de la   #
#                  generacion del archivo de montos liquidados              #
#---------------------------------------------------------------------------#
FUNCTION f_abre_ventana()

    OPEN WINDOW retc9632 AT 4,4 WITH FORM "RETC9632" ATTRIBUTE(BORDER)
    DISPLAY " <CTRL-C> Salir                                             <ESC> Ejecutar  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC963   GENERACION DEL ARCHIVO DE MONTOS LIQUIDADOS TRANSF ISSSTE        " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

END FUNCTION

#---------------------------------------------------------------------------#
# f_obten_fechas : Obtiene las fechas de transferencia y de operacion para  #
#                  la generacion del archivos de montos liquidados          #
#---------------------------------------------------------------------------#
FUNCTION f_obten_fechas(pi_folio)
    
    DEFINE
        pi_folio        INTEGER

    DEFINE lr_fechas RECORD
        val_trans       DATE,
        operacion       DATE
    END RECORD
    
    SELECT fecha_valor_trans,
           fecha_operacion
    INTO   lr_fechas.*
    FROM   ret_cza_lote
    WHERE  folio = pi_folio

    RETURN lr_fechas.*
            
END FUNCTION

#---------------------------------------------------------------------------#
# cza_tran : Reporte que genera el encabezado del archivo de mtos liquidados#
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
                COLUMN 001, "01"                                ,-- Tipo registro
                COLUMN 003, "04"                                ,-- Id de servicio
                COLUMN 005, "01"                                ,-- Entidad origen
                COLUMN 007, gs_codigo_afore USING "&&&"         ,-- Cve entidad origen
                COLUMN 010, "03"                                ,-- Entidad destino
                COLUMN 012, "001"                               ,-- Cve entidad destino
                COLUMN 015, gdt_fec_oper USING "YYYYMMDD"       ,-- Fecha operacion
                COLUMN 023, gdt_fec_val_trans USING "YYYYMMDD"  ,-- Fecha valor trans
                COLUMN 031, 2 SPACES                            ,-- Resultado de la operacion
                COLUMN 033, 3 SPACES                            ,-- Motivo Rechazo 1
                COLUMN 036, 3 SPACES                            ,-- Motivo Rechazo 2
                COLUMN 039, 3 SPACES                            ,-- Motivo Rechazo 3
                COLUMN 042, 109 SPACES                           -- Filler
END REPORT


#---------------------------------------------------------------------------#
# det_transferencia : Reporte que genera el detalle del archivo de montos   #
#                     liquidados                                            #
#---------------------------------------------------------------------------#
REPORT rpt_det_montran(pr_transf)

   DEFINE pr_transf RECORD
        nss             LIKE ret_trans_issste.nss               ,
        curp            LIKE ret_trans_issste.curp              ,
        consec          LIKE ret_trans_issste.consecutivo       ,
        sec_pension     LIKE ret_trans_issste.sec_pension       ,
        mto_const       LIKE ret_trans_issste.mto_solic_issste  ,
        cve_aseg        LIKE ret_trans_issste.cve_aseguradora   ,
        siefore         LIKE dis_cuenta.siefore                 ,
        mto_ret08       LIKE dis_cuenta.monto_en_pesos          ,
        mto_cv          LIKE dis_cuenta.monto_en_pesos          ,
        mto_ahsol       LIKE dis_cuenta.monto_en_pesos          ,
        acc_ret08       LIKE dis_cuenta.monto_en_acciones       ,
        acc_cv          LIKE dis_cuenta.monto_en_acciones       ,
        acc_ahsol       LIKE dis_cuenta.monto_en_acciones
    END RECORD

    DEFINE
        c11_mto_const       CHAR(011) ,
        c11_mto_ret08       CHAR(011) ,
        c11_mto_cv          CHAR(011) ,
        c11_mto_ahsol       CHAR(011) ,
        c10_mto_const       CHAR(010) ,
        c10_mto_ret08       CHAR(010) ,
        c10_mto_cv          CHAR(010) ,
        c10_mto_ahsol       CHAR(010) ,

        c15_acc_ret08       CHAR(015) ,
        c15_acc_cv          CHAR(015) ,
        c15_acc_ahsol       CHAR(015) ,
        c14_acc_ret08       CHAR(014) ,
        c14_acc_cv          CHAR(014) ,
        c14_acc_ahsol       CHAR(014)


    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
            
            -- Formateamos el monto constitutivo
            IF pr_transf.mto_const IS NULL THEN
                LET pr_transf.mto_const = 0
            END IF

            LET c11_mto_const       = pr_transf.mto_const USING "&&&&&&&&.&&"
            LET c10_mto_const       = c11_mto_const[01,08],
                                      c11_mto_const[10,11]

            -- Formateamos Retiro 08
            IF pr_transf.mto_ret08 IS NULL THEN
                LET pr_transf.mto_ret08 = 0
            END IF

            LET c11_mto_ret08       = pr_transf.mto_ret08 USING "&&&&&&&&.&&"
            LET c10_mto_ret08       = c11_mto_ret08[01,08],
                                      c11_mto_ret08[10,11]

            IF pr_transf.acc_ret08 IS NULL THEN
                LET pr_transf.acc_ret08 = 0
            END IF

            LET c15_acc_ret08       = pr_transf.acc_ret08 USING "&&&&&&&&.&&&&&&"
            LET c14_acc_ret08       = c15_acc_ret08[01,08],
                                      c15_acc_ret08[10,15]

            -- Formateamos CV/CS
            IF pr_transf.mto_cv IS NULL THEN
                LET pr_transf.mto_cv = 0
            END IF

            LET c11_mto_cv          = pr_transf.mto_cv USING "&&&&&&&&.&&"
            LET c10_mto_cv          = c11_mto_cv[01,08],
                                      c11_mto_cv[10,11]

            IF pr_transf.acc_cv IS NULL THEN
                LET pr_transf.acc_cv = 0
            END IF

            LET c15_acc_cv          = pr_transf.acc_cv USING "&&&&&&&&.&&&&&&"
            LET c14_acc_cv          = c15_acc_cv[01,08],
                                      c15_acc_cv[10,15]

            -- Formateamos Ahorro Solidario
            IF pr_transf.mto_ahsol IS NULL THEN
                LET pr_transf.mto_ahsol = 0
            END IF

            LET c11_mto_ahsol       = pr_transf.mto_ahsol USING "&&&&&&&&.&&"
            LET c10_mto_ahsol       = c11_mto_ahsol[01,08],
                                      c11_mto_ahsol[10,11]

            IF pr_transf.acc_ahsol IS NULL THEN
                LET pr_transf.acc_ahsol = 0
            END IF

            LET c15_acc_ahsol       = pr_transf.acc_ahsol USING "&&&&&&&&.&&&&&&"
            LET c14_acc_ahsol       = c15_acc_ahsol[01,08],
                                      c15_acc_ahsol[10,15]


        PRINT
            COLUMN 001, "03"                                            , -- Tipo de registro
            COLUMN 003, "04"                                            , -- ID de servicio
            COLUMN 005, "49"                                            , -- ID de operacion
            COLUMN 007, pr_transf.nss                                   , -- NSS
            COLUMN 018, pr_transf.curp                                  , -- CURP
            COLUMN 036, pr_transf.sec_pension                           , -- Secuencia de Pension
            COLUMN 038, "200"                                           , -- Tipo de Movimiento
            COLUMN 041, c10_mto_const                                   , -- Monto Constitutivo
            COLUMN 051, c10_mto_ret08                                   , -- Monto Retiro 08
            COLUMN 061, c10_mto_cv                                      , -- Monto CV/CS
            COLUMN 071, c10_mto_ahsol                                   , -- Monto Ahorro Solidario
            COLUMN 081, pr_transf.siefore USING "&&"                    , -- Clave de Siefore
            COLUMN 083, c14_acc_ret08                                   , -- Acciones Retiro 08
            COLUMN 097, c14_acc_cv                                      , -- Acciones CV/CS
            COLUMN 111, c14_acc_ahsol                                   , -- Acciones Ahorro Solidario
            COLUMN 125, pr_transf.cve_aseg USING "&&&"                  , -- Clave de Aseguradora
            COLUMN 128, 23 SPACES                                         -- Filler
END REPORT

#---------------------------------------------------------------------------#
# sum_tran : Reporte que genera el sumario del archivo de mtos liquidados   #
#---------------------------------------------------------------------------#
REPORT sum_tran(pi_num_regs)

    DEFINE 
        pi_num_regs         INTEGER
        
    DEFINE
        ldt_fecha_oper       DATE

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
            IF gs_codigo_afore = gs_peiss THEN
                LET ldt_fecha_oper = HOY
            ELSE
                LET ldt_fecha_oper = gdt_fec_oper
            END IF    

            PRINT
                COLUMN 001, "09"                            ,-- Tipo Registro
                COLUMN 003, "04"                            ,-- Id de servicio
                COLUMN 005, "01"                            ,-- Entidad Origen
                COLUMN 007, gs_codigo_afore USING "&&&"     ,-- Cve Entidad origen
                COLUMN 010, "03"                            ,-- Entidad destino
                COLUMN 012, "001"                           ,-- Cve Entidad destino
                COLUMN 015, ldt_fecha_oper USING "YYYYMMDD" ,-- Fecha operacion
                COLUMN 023, pi_num_regs USING "&&&&&&"      ,-- Total registros
                COLUMN 029, 122 SPACES                       -- Filler
END REPORT