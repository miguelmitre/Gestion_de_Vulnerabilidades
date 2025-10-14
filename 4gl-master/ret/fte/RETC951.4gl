################################################################################
#Owner             => E.F.P.                                                   #
#Programa RETC951  => GENERA LA OPERACION 45 DISPOSICIONES ISSSTE              #
#Fecha creacion    => 23 DE OCTUBRE DE 2009                                    #
#By                => CESAR DAVID CHAVEZ MARTINEZ                              #
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
        rechazado             LIKE ret_estado_issste.estado_solicitud

    END RECORD

    DEFINE #glo #date
        gdt_fecha_proc        ,
        HOY                   DATE

    DEFINE
        gdt_fec_val_trans     DATE


    DEFINE
        c12_nom_plano         CHAR(012) ,
        G_LISTA_DET           CHAR(100) ,
        G_LISTA_CZA           CHAR(100) ,
        G_LISTA_SUM           CHAR(100) ,
        gc_tipo_ret           CHAR(001) ,
        enter                 CHAR(001) ,
        gc_usuario            CHAR(015)

    DEFINE #glo #smallint
        gs_procesa            ,
        gs_sieviv             ,
        gs_num_siefores       , #-- Indica el numero de siefores que se usan actualmente
        gs_peiss              ,
        gs_codigo_afore       SMALLINT

    DEFINE
        gi_num_regs           ,
        gs_ult_folio          INTEGER

END GLOBALS
################################################################################
MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("RETC951.log")

    CALL init()
    CALL f_captura_folio() RETURNING gs_procesa    ,
                                     gs_ult_folio  ,
                                     gdt_fecha_proc

    IF gs_procesa THEN
    	  #Obtener fechas para encabezado
        CALL habil_siguiente(gdt_fecha_proc,2) RETURNING gdt_fec_val_trans

        CALL f_abre_ventana()
        LET  c12_nom_plano = gdt_fecha_proc USING "YYYYMMDD",".45D"

        CALL primer_paso(gs_ult_folio)      #--Genera encabezado transaccional

        CALL segundo_paso(gs_ult_folio)     #-- Genera el detalle del reporte
            RETURNING gi_num_regs

        CALL tercer_paso(gi_num_regs)       #-- Genera el sumario del reporte
        CALL cuarto_paso()                  #-- Concatena los archivos
        CALL quinto_paso(gs_ult_folio)      #-- Actualiza tablas

    END IF
END MAIN
################################################################################
FUNCTION init()
    LET HOY             = TODAY
    LET gs_sieviv       = 12

    #-- Obtenemos el numero de siefores actual en el sistema
    SELECT COUNT(*)
    INTO   gs_num_siefores
    FROM   tab_siefore_local
    WHERE  codigo_siefore > 0
    AND    codigo_siefore NOT IN (11,12,13)

    SELECT *
    INTO   g_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    SELECT codigo_afore,
           USER
    INTO   gs_codigo_afore,
           gc_usuario
    FROM   tab_afore_local
    
    LET gs_peiss = 578 -- Clave PENSION ISSSTE

    ----- ESTADOS DE S0LICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.enviado
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "ENVIADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.rechazado
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "RECHAZADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.provisionado
    FROM   ret_estado_issste A
    WHERE  A.descripcion = "PROVISIONADO"


    ----- FOLIO MAXIMO -----
    SELECT MAX(folio)
    INTO   gs_ult_folio
    FROM   ret_sol_issste_tx
    WHERE  estado_solicitud = gr_edo.provisionado

END FUNCTION
################################################################################
FUNCTION f_captura_folio()

    DEFINE lr_cap RECORD
        folio           INTEGER,
        fecha_envio     DATE
    END RECORD

    DEFINE
        ls_flag             SMALLINT

    LET ls_flag = 1
    LET lr_cap.folio         = gs_ult_folio
    LET lr_cap.fecha_envio   = HOY


    OPEN WINDOW retc9511 AT 4,4 WITH FORM "RETC9511" ATTRIBUTE (BORDER)
    DISPLAY " < ESC > - EJECUTAR                                      < CTRL-C > - SALIR " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC951     GENERACION DE LA OPERACION 45 DISPOSICION ISSSTE               " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

    INPUT BY NAME lr_cap.* WITHOUT DEFAULTS

        BEFORE FIELD folio

            DISPLAY BY NAME lr_cap.*

            IF lr_cap.folio IS NULL OR lr_cap.folio <= 0 THEN
                PROMPT " NO EXISTEN REGISTROS PARA GENERAR EL ARCHIVO ...<ENTER> PARA SALIR " FOR CHAR enter
                LET ls_flag = 0
                EXIT INPUT
            END IF

        AFTER FIELD folio
            IF lr_cap.folio IS NULL OR lr_cap.folio <= 0 THEN
                ERROR "  CAMPO NO PUEDE SER NULO " ATTRIBUTE(NORMAL)
                NEXT FIELD folio
            END IF

            SELECT "OK"
            FROM   ret_sol_issste_tx
            WHERE  folio = lr_cap.folio
            GROUP BY 1

            IF SQLCA.SQLCODE = NOTFOUND  THEN
                ERROR " EL FOLIO INGRESADO NO EXISTE O NO CORRESPONDE A UNA DISPOSICION"
                SLEEP 2
                ERROR ""
                NEXT FIELD folio
            END IF

            SELECT "OK"
            FROM   ret_ctr_envio
            WHERE  folio = lr_cap.folio
            GROUP BY 1

            IF SQLCA.SQLCODE <> NOTFOUND  THEN
                ERROR " YA FUE GENERADO UN ARCHIVO PARA ESTE FOLIO "
                SLEEP 2
                ERROR ""
                NEXT FIELD folio
            END IF

        AFTER FIELD fecha_envio

            IF lr_cap.fecha_envio IS NULL THEN
                ERROR "DEBE INIDCAR LA FECHA DE ENVIO"
                NEXT FIELD fecha_envio
            END IF

        ON KEY (ESC)
            IF lr_cap.folio IS NULL OR lr_cap.folio <= 0 THEN
                ERROR " CAMPO NO PUEDE SER NULO " ATTRIBUTE(NORMAL)
                NEXT FIELD folio
            END IF

            SELECT "OK"
            FROM   ret_sol_issste_tx
            WHERE  folio = lr_cap.folio
            GROUP BY 1

            IF SQLCA.SQLCODE = NOTFOUND  THEN
                ERROR " EL FOLIO INGRESADO NO EXISTE O NO CORRESPONDE A UNA DISPOSICION"
                SLEEP 2
                ERROR ""
                NEXT FIELD folio
            END IF

            SELECT "OK"
            FROM   ret_ctr_envio
            WHERE  folio = lr_cap.folio
            GROUP BY 1

            IF SQLCA.SQLCODE <> NOTFOUND  THEN
                ERROR " YA FUE GENERADO UN ARCHIVO PARA ESTE FOLIO "
                SLEEP 2
                ERROR ""
                NEXT FIELD folio
            END IF

            IF lr_cap.fecha_envio IS NULL THEN
                ERROR "DEBE INIDCAR LA FECHA DE ENVIO"
                NEXT FIELD fecha_envio
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

    CLOSE WINDOW retc9511

    RETURN ls_flag, lr_cap.*

END FUNCTION
################################################################################
FUNCTION f_obten_fechas(li_folio)

    DEFINE
        li_folio        INTEGER

    DEFINE lr_fechas RECORD
        val_trans       DATE,
        operacion       DATE
    END RECORD


    LET lr_fechas.val_trans   = NULL
    LET lr_fechas.operacion   = NULL

    SELECT fecha_valor_trans ,
           fecha_operacion
    INTO   lr_fechas.*
    FROM   ret_cza_lote
    WHERE  folio = pi_folio

    RETURN lr_fechas.*

END FUNCTION
################################################################################
FUNCTION habil_siguiente(diaActual,numDiaHabil)
   DEFINE
       diaTmp	              ,
       diaHabilSig	        ,
       diaActual	          DATE

   DEFINE
       cont_1                   ,
       numDiaHabil              ,
       contador	                ,
       diaSemana	              ,
       feriado	                ,
       finSemana	        SMALLINT

   LET cont_1      = 0
   LET diaHabilSig = diaActual + 1 UNITS DAY

   WHILE TRUE
       LET feriado   = 0
       LET finSemana = 0
       LET diaSemana = WEEKDAY(diaHabilSig)

       IF diaSemana = 0 OR diaSemana = 6 THEN
      	   LET finSemana = 1
       ELSE
           SELECT *
           FROM   safre_af:tab_feriado
           WHERE  feria_fecha = diaHabilSig

           IF STATUS <> NOTFOUND THEN
               LET feriado = 1
           END IF
       END IF

       IF feriado = 1 OR finSemana = 1 THEN
           LET diaHabilSig = diaHabilSig + 1 UNITS DAY
       ELSE
           LET cont_1 = cont_1 + 1
           IF cont_1 = numDiaHabil THEN
               EXIT WHILE
           ELSE
               LET diaHabilSig = diaHabilSig + 1 UNITS DAY
           END IF
       END IF
   END WHILE

   RETURN diaHabilSig
END FUNCTION
################################################################################
FUNCTION f_abre_ventana()

    OPEN WINDOW retc9512 AT 4,4 WITH FORM "RETC9512" ATTRIBUTE(BORDER)
    DISPLAY " < ESC > - EJECUTAR                                      < CTRL-C > - SALIR " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC951     GENERACION DE LA OPERACION 45 DISPOSICION ISSSTE               " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

END FUNCTION
################################################################################
FUNCTION primer_paso(li_folio)

    DEFINE li_folio INTEGER

    LET G_LISTA_CZA = g_seg_modulo.ruta_envio CLIPPED, "/", gc_usuario CLIPPED, ".CZA_ISS.45D"

    START REPORT cza_tran TO G_LISTA_CZA
        OUTPUT TO REPORT cza_tran(li_folio)
    FINISH REPORT cza_tran
END FUNCTION
################################################################################
REPORT cza_tran(li_folio)
    DEFINE
        li_folio              INTEGER

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
                COLUMN 015, gdt_fecha_proc    USING"YYYYMMDD" ,-- Fecha operacion
                COLUMN 023, gdt_fec_val_trans USING"YYYYMMDD" ,-- Fecha valor trans
                COLUMN 031, 2 SPACES                          ,-- Resultado de la operacion
                COLUMN 033, 3 SPACES                          ,-- Motivo Rechazo 1
                COLUMN 036, 3 SPACES                          ,-- Motivo Rechazo 2
                COLUMN 039, 3 SPACES                          ,-- Motivo Rechazo 3
                COLUMN 042, 309 SPACES                         -- Filler
END REPORT
################################################################################
FUNCTION segundo_paso(li_folio)

    DEFINE li_folio INTEGER

    DEFINE
        lr_ret_sol_issste_tx    RECORD LIKE ret_sol_issste_tx.*     ,
        lr_monto_issste         RECORD LIKE ret_monto_issste.*      ,
        lr_monto_viv            RECORD LIKE ret_monto_viv_issste.*

    DEFINE
        ruta_det_nss      ,
        ruta_det_sie      ,
        ruta_det_bnx      ,
        ruta_det_viv      CHAR(200)

    DEFINE
        ls_gen_siefore      ,
        ls_gen_banxico      SMALLINT

    DEFINE
        li_num_regs         INTEGER

    DISPLAY "GENERANDO ARCHIVO PLANO ..." AT 18,5 ATTRIBUTE(REVERSE)

    LET li_num_regs     = 0

    #-- Determinamos la ubicacion de los reportes

    #Detalle NSS
    LET ruta_det_nss = g_seg_modulo.ruta_envio CLIPPED, "/",
                                               gc_usuario CLIPPED,
                                               ".DET-NSS-DP-tmp"
    LET ruta_det_nss = ruta_det_nss CLIPPED

    #Detalle SIEFORE
    LET ruta_det_sie = g_seg_modulo.ruta_envio CLIPPED, "/",
                                               gc_usuario CLIPPED,
                                               ".DET-SIE-DP-tmp"
    LET ruta_det_sie = ruta_det_sie CLIPPED

    #Detalle BANXICO
    LET ruta_det_bnx = g_seg_modulo.ruta_envio CLIPPED, "/",
                                               gc_usuario CLIPPED,
                                               ".DET-BNX-DP-tmp"
    LET ruta_det_bnx = ruta_det_bnx CLIPPED

    #Detalle VIVIENDA
    LET ruta_det_viv = g_seg_modulo.ruta_envio CLIPPED, "/",
                                               gc_usuario CLIPPED,
                                               ".DET-VIV-DP-tmp"
    LET ruta_det_viv = ruta_det_viv CLIPPED

    #Detalle GENERAL
    LET G_LISTA_DET = g_seg_modulo.ruta_envio CLIPPED, "/",
                                              gc_usuario CLIPPED,
                                              ".DET_ISS_DP"
    LET G_LISTA_DET = G_LISTA_DET CLIPPED

    --SELECCION DE LOS REGISTROS A REPORTAR
    DECLARE cur_det CURSOR FOR
    SELECT A.*
    FROM   ret_sol_issste_tx A
    WHERE  A.folio = li_folio
    ORDER  BY A.tipo_retiro

    FOREACH cur_det INTO lr_ret_sol_issste_tx.*

        #-- Iniciamos los reportes
        START REPORT det_transferencia TO ruta_det_nss
        START REPORT det_vivienda      TO ruta_det_viv
        START REPORT det_siefores      TO ruta_det_sie
        START REPORT det_banxico       TO ruta_det_bnx

        LET ls_gen_banxico  = 0
        LET ls_gen_siefore  = 0
        LET li_num_regs     = li_num_regs + 1

        SELECT  B.*,
                C.*
        INTO    lr_monto_issste.*,
    	        lr_monto_viv.*
        FROM   ret_monto_issste      B,
               ret_monto_viv_issste  C
        WHERE  B.folio        = li_folio
        AND    B.curp         = lr_ret_sol_issste_tx.curp
        AND    B.consecutivo  = lr_ret_sol_issste_tx.consecutivo
        AND    B.folio        = C.folio
        AND    B.curp         = C.curp
        AND    B.consecutivo  = C.consecutivo

        IF lr_monto_issste.siefore IS NOT NULL AND lr_monto_issste.siefore > 0 THEN
            LET ls_gen_siefore = 1
        END IF

        IF gs_codigo_afore = gs_peiss THEN
            CASE lr_ret_sol_issste_tx.regimen 
               
                WHEN "DT" 
                    LET ls_gen_siefore = 0
                    
                    IF lr_monto_issste.pes_banxico > 0 THEN 
                        LET ls_gen_banxico  = 1
                    END IF
                    
                WHEN "RO"                      
                    LET ls_gen_banxico  = 0   
            
            END CASE
        END IF
        
        OUTPUT TO REPORT det_transferencia(lr_ret_sol_issste_tx.*)
        OUTPUT TO REPORT det_siefores(lr_ret_sol_issste_tx.nss, lr_monto_issste.*)
        OUTPUT TO REPORT det_banxico (lr_ret_sol_issste_tx.nss, lr_ret_sol_issste_tx.consecutivo, lr_monto_issste.*, li_folio)
        OUTPUT TO REPORT det_vivienda(lr_ret_sol_issste_tx.nss, lr_ret_sol_issste_tx.diag_procesar, lr_monto_viv.*)

        FINISH REPORT det_transferencia
        FINISH REPORT det_siefores
        FINISH REPORT det_banxico
        FINISH REPORT det_vivienda

        #-- Una vez generados los archivos para el nss actual, los concatenamos en uno solo
        CALL concat_reportes(ruta_det_nss   ,
                             ruta_det_sie   ,
                             ruta_det_bnx   ,
                             ruta_det_viv   ,
                             li_num_regs    ,
                             ls_gen_siefore ,
                             ls_gen_banxico )
    END FOREACH

    RETURN li_num_regs

END FUNCTION
################################################################################
REPORT det_transferencia(lr_disposicion)
    DEFINE
        lr_disposicion           RECORD LIKE ret_sol_issste_tx.*

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
        
        IF lr_disposicion.semanas_cotizadas IS NULL THEN
        	 LET lr_disposicion.semanas_cotizadas = 0
        END IF

        IF lr_disposicion.tipo_seguro = "NA" THEN 
            LET lr_disposicion.tipo_seguro = "   "
        END IF 

        IF lr_disposicion.tipo_pension = "NA" THEN 
            LET lr_disposicion.tipo_pension = "   "
        END IF 
        
        IF lr_disposicion.cve_pension = "NA" THEN 
            LET lr_disposicion.cve_pension = "   "
        END IF 

        PRINT
            COLUMN 001, "03"                                                 , -- Tipo de registro
            COLUMN 003, "04"                                                 , -- ID de servicio
            COLUMN 005, "45"                                                 , -- ID de operacion
            COLUMN 007, lr_disposicion.nss                                   , -- NSS del trabajador
            COLUMN 018, lr_disposicion.curp                                  , -- CURP
            COLUMN 036, lr_disposicion.nombre_afore                          , -- Nombre
            COLUMN 076, lr_disposicion.paterno_afore                         , -- Ap Paterno
            COLUMN 116, lr_disposicion.materno_afore                         , -- Ap Materno
            COLUMN 156, lr_disposicion.sec_pension       USING "&&"          , -- Sec de Pension
            COLUMN 158, lr_disposicion.tipo_retiro                           , -- Tipo de Retiro
            COLUMN 159, lr_disposicion.regimen                               , -- Regimen
            COLUMN 161, lr_disposicion.tipo_seguro                           , -- Tipo de Seguro
            COLUMN 163, lr_disposicion.tipo_pension                          , -- Tipo de Pension
            COLUMN 165, lr_disposicion.cve_pension                           , -- Clave de Pension
            COLUMN 168, lr_disposicion.tipo_prestacion    USING "&&"         , -- Tipo de Prestacion
            COLUMN 170, lr_disposicion.fecha_ini_pen      USING "YYYYMMDD"   , -- Fecha Inicio pension
            COLUMN 178, lr_disposicion.fecha_resolucion   USING "YYYYMMDD"   , -- Fecha resolucion
            COLUMN 186, 5 SPACES                                             , -- Filler
            COLUMN 191, lr_disposicion.semanas_cotizadas  USING "&&&&"       , -- Sems cotizadas
            COLUMN 195, lr_disposicion.fecha_solicitud    USING "YYYYMMDD"   , -- Fecha de Solicitud
            COLUMN 203, lr_disposicion.cve_doc_probatorio                    , -- Cve Documento Probatorio
            COLUMN 204, lr_disposicion.fecha_nacimiento   USING "YYYYMMDD"   , -- Fecha de Nacimiento
            COLUMN 212, lr_disposicion.aseguradora                           , -- Aseguradora
            COLUMN 215, lr_disposicion.actuario                              , -- Actuario Autorizado
            COLUMN 222, lr_disposicion.num_plan_privado                      , -- #Registro Plan Privado de Pensiones
            COLUMN 230, 6  SPACES                                            , -- Fecha Periodo Pago Reingreso
            COLUMN 236, lr_disposicion.consecutivo        USING "&&&&&&&&&&&", -- #Consecutivo
            COLUMN 247, 90 SPACES                                            , -- Filler
            COLUMN 337, 3  SPACES                                            , -- Diagnóstico del Registro
            COLUMN 340, 2  SPACES                                            , -- Resultado de la Operación
            COLUMN 342, 3  SPACES                                            , -- Motivo de Rechazo 1
            COLUMN 345, 3  SPACES                                            , -- Motivo de Rechazo 2
            COLUMN 348, 3  SPACES                                              -- Motivo de Rechazo 3
END REPORT
################################################################################
REPORT det_siefores(p_nss, pr_montos)
    DEFINE p_nss  LIKE ret_solicitud_tx.nss
    DEFINE pr_montos RECORD LIKE ret_monto_issste.*

    DEFINE
        c14_impt_ret_08     CHAR(14) ,
        c14_impt_ces_vej    CHAR(14) ,
        c14_impt_ah_sol     CHAR(14) ,
        c14_impt_comp_ret   CHAR(14) ,
        c14_acc_ret92       CHAR(14)

    DEFINE
        c15_impt_ret_08     CHAR(15) ,
        c15_impt_ces_vej    CHAR(15) ,
        c15_impt_ah_sol     CHAR(15) ,
        c15_impt_comp_ret   CHAR(15) ,
        c15_acc_ret92       CHAR(15)

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

            #-- Obtenemos el valor de las Acciones de Ahorro Sol
            IF pr_montos.acc_ahorro_sol IS NULL THEN
                LET pr_montos.acc_ahorro_sol = 0
            END IF

            LET c15_impt_ah_sol = pr_montos.acc_ahorro_sol USING "&&&&&&&&.&&&&&&"
            LET c14_impt_ah_sol = c15_impt_ah_sol[01,08],
                                  c15_impt_ah_sol[10,15]

            #-- Obtenemos el valor de las Acciones de Retiro 92
            IF pr_montos.acc_ret92 IS NULL THEN
                LET pr_montos.acc_ret92 = 0
            END IF

            LET c15_acc_ret92 = pr_montos.acc_ret92 USING "&&&&&&&&.&&&&&&"
            LET c14_acc_ret92 = c15_acc_ret92[01,08],
                                c15_acc_ret92[10,15]


            #-- Obtenemos el valor de las Acciones de Complementarias de Retiro
            IF pr_montos.acc_comp_ret IS NULL THEN
                LET pr_montos.acc_comp_ret = 0
            END IF

            LET c15_impt_comp_ret = pr_montos.acc_comp_ret USING "&&&&&&&&.&&&&&&"
            LET c14_impt_comp_ret = c15_impt_comp_ret[01,08],
                                    c15_impt_comp_ret[10,15]

        PRINT
            COLUMN 001, "04"                                   , -- Tipo de registro
            COLUMN 003, p_nss                                  , -- NSS
            COLUMN 014, pr_montos.curp                         , -- CURP
            COLUMN 032, pr_montos.siefore USING "&&"           , -- Clave de siefore
            COLUMN 034, c14_impt_ret_08                        , -- Acciones de retiro 2008
            COLUMN 048, c14_impt_ces_vej                       , -- Acciones de CV
            COLUMN 062, c14_impt_ah_sol                        , -- Acciones de Ahorro Solidario
            COLUMN 076, c14_acc_ret92                          , -- Acciones de Retiro 92
            COLUMN 090, c14_impt_comp_ret                      , -- Acciones de Complementarias de Retiro
            COLUMN 104, 247 SPACES                               -- Filler
END REPORT
################################################################################
REPORT det_banxico(p_nss, li_consecutivo, pr_montos, li_folio)

    DEFINE p_nss          CHAR(11)
    DEFINE li_consecutivo DECIMAL(11,0)
    DEFINE pr_montos RECORD LIKE ret_monto_issste.*
    DEFINE li_folio       INTEGER

    DEFINE ld_impt_ret92     DECIMAL(22,6)

    DEFINE
        c11_impt_ret92      CHAR(11),
        c10_impt_ret92      CHAR(10)

    OUTPUT
        PAGE LENGTH   1
    	LEFT MARGIN   0
	    RIGHT MARGIN  0
	    TOP MARGIN    0
	    BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW

            INITIALIZE ld_impt_ret92 TO NULL

            SELECT SUM(monto_en_pesos)
            INTO   ld_impt_ret92
            FROM   dis_provision
            WHERE  nss              = p_nss
            AND    consecutivo_lote = li_consecutivo
            AND    folio            = li_folio
            AND    subcuenta        = 19

            #-- Obtenemos el valor de los pesos de Retiro 92
            IF ld_impt_ret92 IS NULL THEN
                LET ld_impt_ret92 = 0
            END IF

            LET c11_impt_ret92 = ld_impt_ret92 USING "&&&&&&&&.&&"
            LET c10_impt_ret92 = c11_impt_ret92[01,08],
                                 c11_impt_ret92[10,11]

        PRINT
            COLUMN 001, "06"                                   , -- Tipo de registro
            COLUMN 003, p_nss                                  , -- NSS
            COLUMN 014, pr_montos.curp                         , -- CURP
            COLUMN 032, c10_impt_ret92                         , -- Importe de Retiro 92
            COLUMN 042, 309 SPACES                               -- Filler
END REPORT
################################################################################
REPORT det_vivienda(p_nss, ps_diag_procesar, pr_viv)
    DEFINE p_nss            LIKE ret_trans_issste.nss
    DEFINE ps_diag_procesar LIKE ret_trans_issste.diag_procesar

    DEFINE pr_viv RECORD LIKE ret_monto_viv_issste.*

    DEFINE #loc #char
        c15_impt_viv_08       CHAR(15) ,
        c15_impt_viv_92       CHAR(15) ,
        c14_impt_viv_08       CHAR(14) ,
        c14_impt_viv_92       CHAR(14)

    OUTPUT
      PAGE LENGTH   1
    	LEFT MARGIN   0
	    RIGHT MARGIN  0
	    TOP MARGIN    0
	    BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW

            #-- Obtenemos Intereses de Vivienda 08
            IF pr_viv.acciones_viv08 IS NULL THEN
                LET pr_viv.acciones_viv08 = 0
            END IF

            LET c15_impt_viv_08 = pr_viv.acciones_viv08 USING "&&&&&&&&.&&&&&&"
            LET c14_impt_viv_08 = c15_impt_viv_08[01,08],
                                  c15_impt_viv_08[10,15]


            IF ps_diag_procesar <> 501 THEN
               LET pr_viv.fecha_valor_viv = "01/01/0001"
            END IF

            #-- Obtenemos Intereses de Vivienda 92
            IF pr_viv.acciones_viv92 IS NULL THEN
                LET pr_viv.acciones_viv92 = 0
            END IF

            LET c15_impt_viv_92 = pr_viv.acciones_viv92 USING "&&&&&&&&.&&&&&&"
            LET c14_impt_viv_92 = c15_impt_viv_92[01,08],
                                  c15_impt_viv_92[10,15]

        PRINT
            COLUMN 001, "05"                                         , -- Tipo de registro
            COLUMN 003, p_nss                                        , -- NSS
            COLUMN 014, pr_viv.curp                                  , -- CURP
            COLUMN 032, pr_viv.fecha_valor_viv USING "YYYYMMDD"      , -- Fecha Valor de la vivienda
            COLUMN 040, c14_impt_viv_08                              , -- Intereses Vivienda 08
            COLUMN 054, c14_impt_viv_92                              , -- Intereses Vivienda 92
            COLUMN 068, 14  SPACES                                   , -- Filler
            COLUMN 082, 1   SPACES                                   , -- Estatus de Vivienda
            COLUMN 083, 14  SPACES                                   , -- Intereses de Vivienda 08 en BDSVIV
            COLUMN 097, 14  SPACES                                   , -- Intereses de Vivienda 92 en BDSVIV
            COLUMN 111, 240 SPACES                                     -- Filler
END REPORT
################################################################################
FUNCTION concat_reportes(lc_det_3, lc_det_4, lc_det_6, lc_det_5, p_regs, pr_bandera)
    DEFINE
        lc_det_3      ,
        lc_det_4      ,
        lc_det_6      ,
        lc_det_5      CHAR(100)

    DEFINE pr_bandera RECORD
        sie         SMALLINT,
        bnx         SMALLINT
    END RECORD 

    DEFINE 
        p_regs              SMALLINT

    DEFINE ruta_tmp         ,
           ruta_det         CHAR(200)

    DEFINE com_cat          ,
           com_rm           CHAR(1000)

    LET ruta_tmp    = g_seg_modulo.ruta_envio CLIPPED, "/",
                                              gc_usuario CLIPPED,
                                              ".rep_temporal_DP.tmp"
    LET ruta_tmp    = ruta_tmp CLIPPED

    LET ruta_det    = g_seg_modulo.ruta_envio CLIPPED, "/",
                                              gc_usuario CLIPPED,
                                              ".detalle_DP.tmp"
    LET ruta_det    = ruta_det CLIPPED

    #-- Verificamos si se genera o no el registro dado
    IF NOT pr_bandera.sie THEN
        LET lc_det_4 = " "
    END IF

    IF NOT pr_bandera.bnx THEN
        LET lc_det_6 = " "
    END IF

    #-- Preparamos el comando para borrar los reportes
    LET com_rm = "rm ", lc_det_3 CLIPPED, " ",
                        lc_det_4 CLIPPED, " ",
                        lc_det_6 CLIPPED, " ",
                        lc_det_5 CLIPPED
    LET com_rm = com_rm CLIPPED

    #-- Preparamos el comando para concatenar los archivos
    LET com_cat = "cat ", lc_det_3 CLIPPED, " ",
                          lc_det_4 CLIPPED, " ",
                          lc_det_6 CLIPPED, " ",
                          lc_det_5 CLIPPED, " > ", ruta_tmp
    LET com_cat = com_cat CLIPPED

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
################################################################################
FUNCTION tercer_paso(li_num_regs)
    DEFINE li_num_regs     INTEGER

    LET G_LISTA_SUM = g_seg_modulo.ruta_envio CLIPPED, "/",
                                              gc_usuario CLIPPED,
                                              ".SUMDPISS_DP"

    START REPORT sum_tran TO G_LISTA_SUM
        OUTPUT TO REPORT sum_tran(li_num_regs)
    FINISH REPORT sum_tran
END FUNCTION
################################################################################
REPORT sum_tran(li_num_regs)

    DEFINE
        li_num_regs     INTEGER

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
                COLUMN 015, gdt_fecha_proc USING "YYYYMMDD" ,-- Fecha operacion
                COLUMN 023, li_num_regs    USING "&&&&&&"   ,-- Total registros
                COLUMN 029, 322 SPACES                       -- Filler
END REPORT
################################################################################
FUNCTION cuarto_paso()
    DEFINE comando     ,
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
################################################################################
FUNCTION quinto_paso(li_folio)

    DEFINE li_folio        INTEGER

    UPDATE ret_sol_issste_tx
    SET    estado_solicitud = gr_edo.enviado
    WHERE  folio            = li_folio
    AND    estado_solicitud = gr_edo.provisionado

    INSERT INTO ret_ctr_envio
    VALUES ( li_folio       ,
             "DISP_ISSSTE"  ,
             "CZA_DISP_ISSS DET_DISP_ISSS SUM_DISP_ISSS",
             gr_edo.enviado ,
             1              ,
             gdt_fecha_proc
            )
                                        
    DISPLAY "                           " AT 18,5

    DISPLAY " EL LOTE HA SIDO GENERADO EN LA RUTA : " AT 8,19
    DISPLAY g_seg_modulo.ruta_envio CLIPPED AT 10,20
    DISPLAY "                                                " AT 12,11
    DISPLAY " CON EL NOMBRE : ",c12_nom_plano AT 12,19

    PROMPT " PRESIONE <ENTER> PARA FINALIZAR " FOR CHAR enter

    CLOSE WINDOW retc9512

END FUNCTION
################################################################################