#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETC003  => LIQUIDACION DE SOLICITUDES DE RETIROS IMSS                #
#Fecha creacion    => 8 DE MARZO DE 2014                                        #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Sistema           => RET                                                       #
#-------------------------------------------------------------------------------#
#Requerimiento     => CPL-1709  21-Ago-2014   Alejandro Chagoya Salazar         #
#Descripcion       => Se genera prepare de ret_preliquida                       #
#################################################################################
DATABASE safre_af

GLOBALS
    DEFINE gr_estado RECORD
        provisionado        LIKE ret_estado.estado_solicitud ,
        pre_liq             LIKE ret_estado.estado_solicitud ,
        liquidado           LIKE ret_estado.estado_solicitud ,
        proceso_pago        LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE gr_folios RECORD
        fecha_liq           DATE                        ,
        total               LIKE ret_solicitud_tx.folio ,
        parcial             LIKE ret_parcial.folio
    END RECORD
    
    DEFINE gr_afores RECORD
        metlife                 SMALLINT
    END RECORD

    DEFINE
        HOY                     DATE

    DEFINE
        enter                   CHAR(001) ,
        gc_usuario              CHAR(015)

    DEFINE
        gs_flag                 ,
        gs_primer_pago          ,
        gs_cod_afore         SMALLINT

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I

    CALL f_lib_crea_log()
    CALL init()
    CALL f_abre_ventanas()

    CALL f_despliega_info() RETURNING gs_flag, gr_folios.*

    IF (gs_flag = TRUE) THEN
        CALL primer_paso(gr_folios.*)    #-- Realiza la Liquidacion de los montos
        CALL segundo_paso(gr_folios.*)   #-- Desmarca la cuenta y actualiza el estado de la solicitud

    END IF

    CALL f_cierra_ventanas()

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    DEFINE
        lc_prepare              CHAR(300)

    -- -----------------------------------------------------------------------------

    LET HOY                 = TODAY
    LET gc_usuario          = f_lib_obten_user()
    
    LET gs_primer_pago      = 1

    ----- CODIGOS AFORES -----
    SELECT codigo_afore
    INTO   gs_cod_afore
    FROM   tab_afore_local

    SELECT afore_cod
    INTO   gr_afores.metlife
    FROM   tab_afore
    WHERE  UPPER(afore_desc) MATCHES "*METLIFE*"

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_estado.provisionado
    FROM   ret_estado A
    WHERE  A.descripcion = "PROVISIONADO"

    SELECT A.estado_solicitud
    INTO   gr_estado.pre_liq
    FROM   ret_estado A
    WHERE  A.descripcion = "PRELIQUIDADO"

    SELECT A.estado_solicitud
    INTO   gr_estado.liquidado
    FROM   ret_estado A
    WHERE  A.descripcion = "LIQUIDADO"
    
    SELECT A.estado_solicitud
    INTO   gr_estado.proceso_pago
    FROM   ret_estado A
    WHERE  A.descripcion = "EN PROCESO DE PAGO"

    ----- DESMARCA -----
    LET lc_prepare = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"
    PREPARE eje_desmarca FROM lc_prepare

    LET lc_prepare = " "

    ----- INSERTA EN TABLAS CONSAR  -----
    LET lc_prepare = "EXECUTE FUNCTION fn_inserta_consar(?,?,?,?)"
    PREPARE eje_CONSAR FROM lc_prepare

    LET lc_prepare = " "

    ----- INSERTA MARCA PENSIONADO -----
    LET lc_prepare = " EXECUTE FUNCTION fn_inserta_marca_pen(?,?,?,?,?,?,?) "
    PREPARE eje_marca_pen FROM lc_prepare

    LET lc_prepare = " "

    ----- INSERTA EDO CUENTA PENSIONADO -----
    LET lc_prepare = " EXECUTE FUNCTION fn_inserta_edo_cta_pen(?,?,?,?,?,?) "
    PREPARE eje_edo_cta_pen FROM lc_prepare

END FUNCTION

#---------------------------------------------------------------------------#
# f_despliega_info : Despliega la informacion que se usara para generar la  #
#                    liquidacion de los procesos de retiros IMSS            #
#---------------------------------------------------------------------------#
FUNCTION f_despliega_info()

    DEFINE lr_info RECORD
        fecha_liq           DATE    ,
        sel_tot             CHAR    ,
        sel_par             CHAR    ,
        folio_tot           INTEGER ,
        folio_par           INTEGER
    END RECORD

    DEFINE lr_folios RECORD
        disp                INTEGER ,
        par                 INTEGER
    END RECORD

    DEFINE
        ls_cont             ,
        ls_flag             SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_flag             = 1
    LET lr_info.fecha_liq   = HOY
    INITIALIZE lr_folios.* TO NULL

    -- DISPOSICIONES
    SELECT COUNT(UNIQUE folio)
    INTO   lr_folios.disp
    FROM   ret_solicitud_tx
    WHERE  estado_solicitud = gr_estado.pre_liq

    IF lr_folios.disp IS NULL THEN
        LET lr_folios.disp = 0
    END IF

    -- PARCIALES
    SELECT COUNT(UNIQUE folio)
    INTO   lr_folios.par
    FROM   ret_parcial
    WHERE  estado_solicitud = gr_estado.pre_liq
    AND    (consecutivo NOT IN (SELECT consecutivo FROM ret_parcial_sub
                                WHERE estado <> gr_estado.liquidado)
    OR     consecutivo IN (SELECT consecutivo FROM ret_parcialidad_des
                           WHERE estado = gr_estado.pre_liq
                           AND   consec_pago = gs_primer_pago))
    IF lr_folios.par IS NULL THEN
        LET lr_folios.par = 0
    END IF

    INPUT BY NAME lr_info.* WITHOUT DEFAULTS

        BEFORE INPUT
            IF ( (lr_folios.disp = 0) AND (lr_folios.par = 0) ) THEN
                CALL f_lib_error_msg("NO EXISTEN REGISTROS PARA LIQUIDAR")
                LET ls_flag = 0
                EXIT INPUT
            END IF

        AFTER FIELD fecha_liq
            IF (lr_info.fecha_liq IS NULL) THEN
                CALL f_lib_error_msg("LA FECHA DE LIQUIDACION NO PUEDE SER NULA")
                NEXT FIELD fecha_liq
            END IF

        AFTER FIELD sel_tot
            IF (lr_info.sel_tot = "x") THEN
                CALL f_muestra_folios("dis", lr_folios.disp) RETURNING lr_info.folio_tot
                DISPLAY BY NAME lr_info.folio_tot

                IF lr_info.folio_tot = 0 THEN
                    LET lr_info.sel_tot = " "
                    DISPLAY BY NAME lr_info.sel_tot
                END IF
            END IF

        AFTER FIELD sel_par
            IF (lr_info.sel_par = "x") THEN
                CALL f_muestra_folios("par", lr_folios.par) RETURNING lr_info.folio_par
                DISPLAY BY NAME lr_info.folio_par

                IF lr_info.folio_par = 0 THEN
                    LET lr_info.sel_par = " "
                    DISPLAY BY NAME lr_info.sel_par
                END IF
            END IF

        ON KEY (CONTROL-C, INTERRUPT)
            CALL f_lib_error_msg("PROCESO CANCELADO")
            LET ls_flag = 0
            EXIT INPUT

        ON KEY (ESC)
            IF lr_info.sel_tot IS NULL THEN
                LET lr_info.sel_tot = " "
            END IF

            IF lr_info.sel_par IS NULL THEN
                LET lr_info.sel_par = " "
            END IF

            IF lr_info.sel_tot <> "x" THEN
                LET lr_info.folio_tot = 0
            END IF

            IF lr_info.sel_par <> "x" THEN
                LET lr_info.folio_par = 0
            END IF

            IF ( (lr_info.sel_tot <> "x") AND (lr_info.sel_par <> "x") ) THEN
                CALL f_lib_error_msg("NO EXISTEN REGISTROS PARA LIQUIDAR")
                LET ls_flag = FALSE
                EXIT INPUT
            END IF

            IF (lr_info.fecha_liq IS NULL) THEN
                CALL f_lib_error_msg("LA FECHA DE PRELIQUIDACION NO PUEDE SER NULA")
                NEXT FIELD fecha_liq
            END IF

            IF f_lib_pregunta("¿DESEA GENERAR LA LIQUIDACION? (S/N) : ") = TRUE THEN
                LET ls_flag = TRUE
            ELSE
                CALL f_lib_error_msg("PROCESO CANCELADO")
                LET ls_flag = FALSE
            END IF

            EXIT INPUT

    END INPUT

    RETURN ls_flag              ,
           lr_info.fecha_liq    ,
           lr_info.folio_tot    ,
           lr_info.folio_par

END FUNCTION


#---------------------------------------------------------------------------#
# primer_paso : Llama a las funciones que realizan los procesos de          #
#               liquidacion de cuentas                                      #
#---------------------------------------------------------------------------#
FUNCTION primer_paso(pr_folios)

    DEFINE pr_folios RECORD
        fecha_liq            DATE                        ,
        total               LIKE ret_solicitud_tx.folio ,
        parcial             LIKE ret_parcial.folio
    END RECORD

    -- -----------------------------------------------------------------------------

    CURRENT WINDOW IS win_resultados

    IF (pr_folios.total <> 0) THEN
        CALL f_liquida_folio(pr_folios.fecha_liq    ,
                             pr_folios.total        ,
                             "tot")         -- Liquidacion de retiros totales
    END IF

    IF (pr_folios.parcial <> 0) THEN
        CALL f_liquida_folio(pr_folios.fecha_liq    ,
                             pr_folios.parcial      ,
                             "par")         -- Liquidacion de retiros parciales
    END IF

END FUNCTION

#---------------------------------------------------------------------------#
# segundo_paso : Desmarca las cuentas del folio y actualiza el estado       #
#                de solicitud                                               #
#---------------------------------------------------------------------------#
FUNCTION segundo_paso(pr_folios)

    DEFINE pr_folios RECORD
        fecha_liq           DATE                        ,
        total               LIKE ret_solicitud_tx.folio ,
        parcial             LIKE ret_solicitud_tx.folio
    END RECORD

    -- -----------------------------------------------------------------------------

    DISPLAY "                                             " AT 18,1

    IF (pr_folios.total <> 0) THEN
        CALL f_desmarca_cta(pr_folios.total, "tot")
        CALL f_act_estado_sol(pr_folios.total, "tot")
    END IF

    IF pr_folios.parcial <> 0 THEN
        CALL f_desmarca_cta(pr_folios.parcial, "par")
        CALL f_act_estado_sol(pr_folios.parcial, "par")
    END IF

    CALL f_lib_error_msg("PROCESO TERMINADO CORRECTAMENTE")

END FUNCTION

#---------------------------------------------------------------------------#
# f_muestra_folios : Despliega la pantalla con los folios que existen para  #
#                    ser preliquidados. Busca de acuerdo a la modalidad de  #
#                    retiro que se desee utilizar                           #
#---------------------------------------------------------------------------#
FUNCTION f_muestra_folios(pc_modalidad, pi_numfol)

    DEFINE
        pc_modalidad            CHAR(03)

    DEFINE lr_reg ARRAY[10] OF RECORD
        folio           INTEGER ,
        total           INTEGER ,
        preliquidadas   INTEGER
    END RECORD
    DEFINE
        lc_proceso          CHAR(015),
        lc_tabla            CHAR(100),
        lc_and              CHAR(1000),
        lc_prepare          CHAR(500)

    DEFINE
        ls_pos              SMALLINT

    DEFINE
        pi_numfol           ,
        li_folio            INTEGER

    -- -----------------------------------------------------------------------------

    INITIALIZE lr_reg[1].* TO NULL

    FOR ls_pos = 2 TO 10
        LET lr_reg[ls_pos].*    = lr_reg[1].*
    END FOR

    LET ls_pos      = 1
    LET li_folio    = 0

    CASE pc_modalidad
        WHEN "dis"
            LET lc_tabla    = " FROM ret_solicitud_tx "
            LET lc_proceso  = "DISPOSICIONES"
            LET lc_and      = ""

        WHEN "par"
            LET lc_tabla    = " FROM ret_parcial "
            LET lc_proceso  = "PARCIALES"
            LET lc_and = " AND (consecutivo NOT IN (SELECT consecutivo FROM ret_parcial_sub ",
                         "                          WHERE estado <> ", gr_estado.liquidado,")",
                         " OR consecutivo IN (SELECT consecutivo FROM ret_parcialidad_des ",
                         "                    WHERE estado = ",gr_estado.pre_liq, 
                         "                    AND   consec_pago = ", gs_primer_pago,"))"
    END CASE

    LET lc_prepare = " SELECT UNIQUE(folio)  "      ,
                     lc_tabla                       ,
                     " WHERE  estado_solicitud = ? ",
                     lc_and CLIPPED                 ,
                     " ORDER BY 1 "

    PREPARE prp_dat FROM lc_prepare

    IF (pi_numfol > 0) THEN
        OPEN WINDOW win_folios AT 12,6 WITH FORM "RETC0033" ATTRIBUTE(BORDER)
        DISPLAY "                   FOLIOS DE RETIROS TOTALES A LIQUIDAR                   " AT 2,1
            ATTRIBUTE(REVERSE)

        DISPLAY " PRESIONE <ENTER> PARA SELECCIONAR FOLIO " AT 11,1

        WHILE TRUE
            DECLARE cur_dat CURSOR FOR prp_dat

            LET ls_pos = 1

            FOREACH cur_dat USING gr_estado.pre_liq
                            INTO lr_reg[ls_pos].*

                IF (pc_modalidad = "dis") THEN
                    -- Obtenemos el detalle de solicitudes de totales
                    SELECT NVL(COUNT(*), 0)
                    INTO   lr_reg[ls_pos].preliquidadas
                    FROM   ret_solicitud_tx
                    WHERE  folio            = lr_reg[ls_pos].folio
                    AND    estado_solicitud = gr_estado.pre_liq

                    SELECT NVL(COUNT(*), 0)
                    INTO   lr_reg[ls_pos].total
                    FROM   ret_solicitud_tx
                    WHERE  folio             = lr_reg[ls_pos].folio
                    AND    estado_solicitud <> gr_estado.pre_liq
                    AND    diag_registro IS NOT NULL

                    LET lr_reg[ls_pos].total = lr_reg[ls_pos].total + lr_reg[ls_pos].preliquidadas
                ELSE
                    -- Obtenemos el detalle de solicitudes de totales
                    SELECT NVL(COUNT(*), 0)
                    INTO   lr_reg[ls_pos].preliquidadas
                    FROM   ret_parcial
                    WHERE  folio            = lr_reg[ls_pos].folio
                    AND    estado_solicitud = gr_estado.pre_liq
                    AND    (consecutivo NOT IN (SELECT consecutivo FROM ret_parcial_sub
                                                WHERE estado <> gr_estado.liquidado)
                    OR     consecutivo IN (SELECT consecutivo FROM ret_parcialidad_des
                                           WHERE estado = gr_estado.pre_liq
                                           AND   consec_pago = gs_primer_pago))

                    SELECT NVL(COUNT(*), 0)
                    INTO   lr_reg[ls_pos].total
                    FROM   ret_parcial
                    WHERE  folio             = lr_reg[ls_pos].folio
                    AND    estado_solicitud <> gr_estado.pre_liq
                    AND    diag_cuenta_ind IS NOT NULL
                    AND    (consecutivo NOT IN (SELECT consecutivo FROM ret_parcial_sub
                                                WHERE estado <> gr_estado.liquidado)
                    OR     consecutivo IN (SELECT consecutivo FROM ret_parcialidad_des
                                           WHERE estado = gr_estado.pre_liq
                                           AND   consec_pago = gs_primer_pago))


                    LET lr_reg[ls_pos].total = lr_reg[ls_pos].total + lr_reg[ls_pos].preliquidadas
                END IF

                LET ls_pos = ls_pos + 1
                IF (ls_pos >= 10) THEN
                    CALL f_lib_error_msg("SE REBASO LA CAPACIDAD MAXIMA DEL ARREGLO")
                    EXIT FOREACH
                END IF

            END FOREACH

            CALL SET_COUNT(ls_pos-1)

            DISPLAY ARRAY lr_reg TO scr_2.*
                ATTRIBUTE(CURRENT ROW DISPLAY = "REVERSE")

                ON KEY (CONTROL-C, INTERRUPT)
                    LET ls_pos = 0
                    EXIT DISPLAY

                ON KEY ( CONTROL-M )
                    LET ls_pos      = ARR_CURR()
                    LET li_folio    = lr_reg[ls_pos].folio
                    EXIT DISPLAY

            END DISPLAY

            IF ls_pos <> 0 THEN
                EXIT WHILE
            END IF

        END WHILE

        CLOSE WINDOW win_folios
    ELSE
        LET lc_tabla = "NO HAY FOLIOS PARA ", lc_proceso CLIPPED
        CALL f_lib_error_msg(lc_tabla)
    END IF

    RETURN li_folio

END FUNCTION


#---------------------------------------------------------------------------#
# f_liquida_folio : Realiza la liquidacion de acuerdo a la modalidad        #
#                   de retiro e inserta en las tablas de estadistica consar #
#---------------------------------------------------------------------------#
FUNCTION f_liquida_folio(pr_datos)

    DEFINE pr_datos RECORD
        fecha_liquida           DATE                        ,
        folio                   LIKE ret_solicitud_tx.folio ,
        modalidad               CHAR(3)
    END RECORD

    DEFINE lr_preliquida RECORD LIKE dis_cuenta.*

    DEFINE lr_liquida RECORD
        nss                 LIKE dis_cuenta.nss             ,
        curp                LIKE dis_cuenta.curp            ,
        consecutivo_lote    LIKE dis_cuenta.consecutivo_lote,
        folio               LIKE dis_cuenta.folio           ,
        movimiento          LIKE dis_cuenta.tipo_movimiento ,
        fecha_conversion    LIKE dis_cuenta.fecha_conversion
    END RECORD

    DEFINE lc_regimen LIKE ret_solicitud_tx.regimen

    DEFINE
        lc_tipo_retiro          CHAR(1)

    DEFINE
        ls_resp                 ,
        ls_inserta              ,
        ls_cod_tramite          SMALLINT

    DEFINE  lc_tabla            CHAR(100),    #CPL-1709
            lc_and              CHAR(1000),
            lc_prepare          CHAR(650)

    -- -----------------------------------------------------------------------------

    INITIALIZE lr_preliquida.*, lc_prepare, lc_tabla TO NULL

    CASE pr_datos.modalidad
        WHEN "tot"

            LET lc_tabla    = " ret_solicitud_tx b "
            SELECT cod_tramite
            INTO   ls_cod_tramite
            FROM   tab_tipo_tramite
            WHERE  descripcion = "DISPOSICION"
            
            LET lc_and = ""

            DISPLAY "LIQUIDANDO RETIROS TOTALES ..." AT 4,5

        WHEN "par"

            LET lc_tabla    = " ret_parcial b "
            SELECT cod_tramite
            INTO   ls_cod_tramite
            FROM   tab_tipo_tramite
            WHERE  descripcion = "RETIRO PARCIAL"
            
            LET lc_and = " AND (b.consecutivo NOT IN (SELECT c.consecutivo FROM ret_parcial_sub c",
                         "                          WHERE c.estado <> ", gr_estado.liquidado,")",
                         " OR b.consecutivo IN (SELECT d.consecutivo FROM ret_parcialidad_des d",
                         "                    WHERE d.estado = ",gr_estado.pre_liq, 
                         "                    AND   d.consec_pago = ", gs_primer_pago,"))"

            DISPLAY "LIQUIDANDO RETIROS PARCIALES ..." AT 10,5
    END CASE
    
    LET lc_prepare = 
         " INSERT INTO dis_cuenta ",
         " SELECT a.* FROM ret_preliquida  a, ",  lc_tabla CLIPPED,
         " WHERE  a.folio = b.folio ",
         " AND a.nss = b.nss ",
         " AND a.folio = ", pr_datos.folio, lc_and CLIPPED,
         " AND b.estado_solicitud = ",gr_estado.pre_liq,
         " AND a.monto_en_pesos <> 0 ",
         " AND a.monto_en_acciones <> 0 "

         LET lc_prepare = lc_prepare CLIPPED

         PREPARE pre_liq FROM lc_prepare
         
         ----  Ejecuta  liquidación 
         EXECUTE pre_liq
         
         -----   Genera tabla temporal para  actualizar información masiva
         
       WHENEVER ERROR CONTINUE
          DROP TABLE tmp_ret_liquida
       WHENEVER ERROR STOP
    
        SELECT UNIQUE(nss)      ,
               curp             ,
               consecutivo_lote consecutivo,
               folio            folio_liquida,
               fecha_conversion 
        FROM   dis_cuenta
        WHERE  folio            = pr_datos.folio
        AND    (tipo_movimiento <> 10 AND tipo_movimiento <> 817)
        AND    fecha_conversion = TODAY
        ORDER BY 1
        INTO TEMP tmp_ret_liquida 
 
        CREATE INDEX ixtmp_ret_liquida ON tmp_ret_liquida (nss,consecutivo)
        UPDATE STATISTICS FOR TABLE tmp_ret_liquida
        
        ------ Actualiza estadisticas CONSAR 
        
        UPDATE ret_parcial_tx
        SET    fecha_valuacion  = HOY   ,
               fecha_pago       = HOY
        WHERE  rowid IN  ( SELECT r.rowid
                             FROM tmp_ret_liquida t ,
                                  ret_parcial_tx r
                            WHERE r.nss = t.nss
                              AND r.consecutivo = t.consecutivo
                              AND (r.consecutivo NOT IN (SELECT consecutivo FROM ret_parcial_sub WHERE estado <> gr_estado.liquidado) OR 
                                   r.consecutivo IN (SELECT consecutivo FROM ret_parcialidad_des WHERE estado = gr_estado.pre_liq AND consec_pago = gs_primer_pago)
                                  )
                         )
      ----  Inserta registros con marca de pensionado                   
      ----  Se eliminan las previas                   
        DELETE 
          FROM ret_marca_pensionado
         WHERE nss IN ( SELECT nss FROM tmp_ret_liquida)
        
        INSERT INTO ret_marca_pensionado
        SELECT ret.nss              ,
               ret.curp             ,
               ret.folio_liquida    ,
               ret.consecutivo      ,
               'I'                  , --tipo_retiro      
               cod_tramite       ,
               '1'                  , --id_marca_pen         
               ret.fecha_conversion     ,
               CURRENT HOUR TO SECOND, --hora_marca           
               USER                  -- usuario_marca     
         FROM tmp_ret_liquida ret,
              tab_tipo_tramite
        WHERE  descripcion = "RETIRO PARCIAL"
              

END FUNCTION

#---------------------------------------------------------------------------#
# f_abre_ventanas : Abre y prepara las ventanas que se usaran en            #
#                   el programa                                             #
#---------------------------------------------------------------------------#
FUNCTION f_abre_ventanas()

    -- -----------------------------------------------------------------------------

    OPEN WINDOW win_despliega AT 4,4 WITH FORM "RETC0031" ATTRIBUTE (BORDER)
    DISPLAY " <CTRL-C> SALIR                                             <ESC> EJECUTAR    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC003          LIQUIDACION DE SOLICITUDES DE RETIROS IMSS                  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

    -- -----------------------------------------------------------------------------

    OPEN WINDOW win_resultados AT 4,4 WITH FORM "RETC0031" ATTRIBUTE (BORDER)
    DISPLAY "                                                         DISPOSICIONES IMSS   " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC003          LIQUIDACION DE SOLICITUDES DE RETIROS IMSS                  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

    DISPLAY "                                                                            " AT 5,1
    DISPLAY "                                                                            " AT 6,1
    DISPLAY "                                                                            " AT 7,1
    DISPLAY "                                                                            " AT 8,1
    DISPLAY "                                                                            " AT 9,1
    DISPLAY "                                                                            " AT 10,1
    DISPLAY "                                                                            " AT 11,1
    DISPLAY "                                                                            " AT 12,1
    DISPLAY "                                                                            " AT 13,1
    DISPLAY "                                                                            " AT 14,1
    DISPLAY "                                                                            " AT 15,1
    DISPLAY "                                                                            " AT 16,1

    -- -----------------------------------------------------------------------------

    CURRENT WINDOW IS win_despliega


END FUNCTION


#---------------------------------------------------------------------------#
# f_cierra_ventanas : Cierra las ventanas usadas por el programa            #
#---------------------------------------------------------------------------#
FUNCTION f_cierra_ventanas()

    CLOSE WINDOW win_resultados
    CLOSE WINDOW win_despliega

END FUNCTION

#---------------------------------------------------------------------------#
# f_desmarca_cta : Realiza la desmarca de las cuentas liquidadas            #
#---------------------------------------------------------------------------#
FUNCTION f_desmarca_cta(pi_folio, pc_id_oper)

    DEFINE
        pi_folio        ,
        li_desm         INTEGER

    DEFINE
        pc_id_oper      CHAR(3)

    DEFINE lr_desmarca RECORD
        nss         LIKE cta_his_marca.nss          ,
        movimiento  LIKE cta_his_marca.marca_cod    ,
        consec      LIKE cta_his_marca.correlativo  ,
        edo_causa   LIKE cta_his_marca.estado_marca ,
        marca_causa LIKE cta_his_marca.marca_causa
    END RECORD

    DEFINE  lc_tabla            CHAR(100),    #CPL-1709
            lc_prepare          CHAR(500),
            lc_and              CHAR(150)

    CASE pc_id_oper
        WHEN "tot"
            LET lc_tabla    = " ret_solicitud_tx b "
            LET lc_and      = ""
        WHEN "par"
            LET lc_tabla    = " ret_parcial b "
            LET lc_and      = " AND b.consecutivo NOT IN (SELECT consecutivo FROM ret_parcial_sub ",
                              "                          WHERE estado <> ", gr_estado.liquidado,")"
    END CASE

#CPL-1709 INI
       LET lc_prepare = 
               " SELECT UNIQUE(a.nss)      ,         ",
               "        a.tipo_movimiento  ,         ",
               "        a.consecutivo_lote ,         ",
               "        0                  ,         ",
               "        0                            ",
               " FROM ret_preliquida  a, ",  lc_tabla CLIPPED,
               " WHERE  a.folio = b.folio ",
               " AND a.nss = b.nss ",
               " AND a.folio = ", pi_folio, lc_and CLIPPED, 
               " AND b.estado_solicitud = ",gr_estado.pre_liq,
               " AND    a.tipo_movimiento <> 10        ",
               " ORDER BY 1                          "

         LET lc_prepare = lc_prepare CLIPPED
         PREPARE pre_des FROM lc_prepare
#CPL-1709 FIN

    LET li_desm = 0

    DECLARE cur_desmarca CURSOR FOR pre_des

    FOREACH cur_desmarca INTO lr_desmarca.*

        -- Si el movimiento corresponde a un tipo de retiro parcial por desempleo
        -- la desmarca se debe hacer por el codigo 875
        IF ( (lr_desmarca.movimiento = 875) OR
             (lr_desmarca.movimiento = 876) OR
             (lr_desmarca.movimiento = 877) OR
             (lr_desmarca.movimiento = 878)
           )
        THEN
            LET lr_desmarca.movimiento = 875
        END IF

        EXECUTE eje_desmarca USING lr_desmarca.*, gc_usuario

        LET li_desm = li_desm + 1

        CASE pc_id_oper
            WHEN  "tot"
                DISPLAY "DESMARCAS : ", li_desm AT 5,44

            WHEN  "par"
                DISPLAY "DESMARCAS    : ", li_desm AT 12,48
        END CASE

        INITIALIZE lr_desmarca.* TO NULL
    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# f_act_estado_sol : Actualiza los estados de solicitud de acuerdo al tipo  #
#                   de retiro issste                                        #
#---------------------------------------------------------------------------#
FUNCTION f_act_estado_sol(pr_folio, pc_id_oper)

    DEFINE pr_folio LIKE ret_solicitud_tx.folio

    DEFINE
        pc_id_oper          CHAR(3)

    DEFINE la_rets ARRAY[8] OF INTEGER

    DEFINE lr_cifras RECORD
        tipo_mov        LIKE dis_cuenta.tipo_movimiento     ,
        cuantos         INTEGER
    END RECORD

    DEFINE
        ls_cont     SMALLINT,
        ls_app      SMALLINT
        
    DEFINE li_app        INTEGER,
           li_presencial INTEGER

    DEFINE 
        lc_tabla            CHAR(100),
        lc_prepare          CHAR(2000),
        lc_and              CHAR(1000)
    
    DEFINE
        ld_monto_acciones   DECIMAL(22,6)
    -- -----------------------------------------------------------------------------
    
    LET ld_monto_acciones = 0
    LET li_app = 0
    LET li_presencial = 0
    
    CASE pc_id_oper
        WHEN "tot"
            LET lc_tabla    = " ret_solicitud_tx b "
            LET lc_and      = ""
        WHEN "par"
            LET lc_tabla    = " ret_parcial b "
            LET lc_and = " AND (b.consecutivo NOT IN (SELECT c.consecutivo FROM ret_parcial_sub c",
                         "                          WHERE c.estado <> ", gr_estado.liquidado,")",
                         " OR b.consecutivo IN (SELECT d.consecutivo FROM ret_parcialidad_des d",
                         "                    WHERE d.estado = ",gr_estado.pre_liq, 
                         "                    AND   d.consec_pago = ", gs_primer_pago,"))"
    END CASE

#CPL-1709 INI

    FOR ls_cont = 1 TO 8
        LET la_rets[ls_cont] = 0
    END FOR
    
    WHENEVER ERROR CONTINUE
      DROP TABLE tmp_act_edo
    WHENEVER ERROR STOP
    

        LET lc_prepare = 
        " SELECT a.nss      , ",
        "        a.tipo_movimiento  , ",
        "        a.consecutivo_lote , ",
        "        a.fecha_conversion , ",
        "        SUM(a.monto_en_acciones) monto_en_acciones ",
        " FROM ret_preliquida  a, ",  lc_tabla CLIPPED,
        " WHERE a.folio = b.folio ",
        " AND a.nss = b.nss ",
        " AND a.folio = ", pr_folio, lc_and CLIPPED,
        " AND b.estado_solicitud = ",gr_estado.pre_liq,
        " GROUP BY 1,2,3,4 ",
        " ORDER BY 1 ",
        " INTO TEMP tmp_act_edo"

         LET lc_prepare = lc_prepare CLIPPED
         PREPARE pre_act FROM lc_prepare
         EXECUTE pre_act

        CREATE INDEX ixtmp_act_edo ON tmp_act_edo (consecutivo_lote)
        UPDATE STATISTICS FOR TABLE tmp_act_edo

#CPL-1709 FIN
    --DECLARE cur_liq CURSOR FOR pre_act

    -- Actualiza el estado de la solicitud de los registros liquidados
    CASE pc_id_oper

        WHEN "tot"
            
        WHEN "par"
            DISPLAY "FOLIO           : ", pr_folio AT 11,09

            DISPLAY "DESEMPLEO PRESENCIAL : ", la_rets[1] AT 12,09
            DISPLAY "DESEMPLEO POR APP    : ", la_rets[7] AT 13,09
            DISPLAY "TIPO A               : ", la_rets[2] AT 14,09
            DISPLAY "TIPO B               : ", la_rets[3] AT 15,09
            DISPLAY "COMPLEMENTARIOS      : ", la_rets[4] AT 16,09
            DISPLAY "ANTERIORES           : ", la_rets[5] AT 17,09

            DISPLAY "MATRIMONIO PRESENCIAL: ", la_rets[6] AT 12,48
            DISPLAY "MATRIMONIO POR APP   : ", la_rets[8] AT 13,48
            
            
            -- Actualiza estados 
            
            UPDATE ret_parcial
            SET    estado_solicitud = gr_estado.liquidado
            WHERE  estado_solicitud = gr_estado.pre_liq
            AND    folio            = pr_folio
            
                                   
            UPDATE ret_ctr_pago
            SET    estado           = gr_estado.liquidado
            WHERE  estado           = gr_estado.provisionado
            AND    folio_op12       = pr_folio
            
                                   
            UPDATE ret_ctr_pago_det
            SET    folio_op16       = pr_folio              ,
                   estado           = gr_estado.liquidado   ,
                   fecha_liquida    = gr_folios.fecha_liq 
            WHERE  consecutivo IN (SELECT consecutivo
                                   FROM   ret_parcial
                                   WHERE  folio             = pr_folio
                                   AND    estado_solicitud  = gr_estado.liquidado
                                  )
            AND    estado           = gr_estado.provisionado
            
               
            -----  Actualiza parcialidades
            
            UPDATE ret_parcialidad_des
               SET (estado, fecha_liquidacion, monto_en_acciones) = (( SELECT '8',fecha_conversion,sum(monto_en_acciones)
                                                                         FROM tmp_act_edo
                                                                        WHERE consecutivo_lote = ret_parcialidad_des.consecutivo
                                                                        GROUP BY 1,2
                                                                     ))
             WHERE consecutivo IN  ( SELECT UNIQUE consecutivo_lote FROM tmp_act_edo )
               AND consec_pago = gs_primer_pago
               AND estado = gr_estado.pre_liq

            UPDATE ret_parcial_sub
               SET estado = 57
             WHERE consecutivo IN  ( SELECT UNIQUE r.consecutivo 
                                       FROM  ret_parcialidad_des r,
                                             tmp_act_edo  t
                                      WHERE t.consecutivo_lote = r.consecutivo
                                        AND r.consec_pago = 1 )
               AND estado = gr_estado.pre_liq


               
            ----- Cifras por tipo de retiro                       
                                   
            DECLARE cur_liq  CURSOR FOR  SELECT tipo_movimiento,COUNT(*)
                                        FROM tmp_act_edo
                                       GROUP BY 1
                                    
               FOREACH cur_liq INTO lr_cifras.*
               
 
                CASE lr_cifras.tipo_mov
                
                    WHEN 876    -- Tipo A
                        SELECT COUNT(*)
                        INTO   li_app
                        FROM   ret_ws_notifica_app
                        WHERE  consecutivo IN (SELECT UNIQUE consecutivo_lote
                                               FROM tmp_act_edo
                                               WHERE tipo_movimiento = 876)

                        LET la_rets[1] = la_rets[1] + lr_cifras.cuantos - li_app
                        LET la_rets[7] = la_rets[7] + li_app
                        LET la_rets[2] = lr_cifras.cuantos

                        DISPLAY "DESEMPLEO PRESENCIAL : ", la_rets[1] AT 12,09
                        DISPLAY "DESEMPLEO POR APP    : ", la_rets[7] AT 13,09
                        DISPLAY "TIPO A               : ", la_rets[2] AT 14,09

                    WHEN 877    -- Tipo B
                        SELECT COUNT(*)
                        INTO   li_app
                        FROM   ret_ws_notifica_app
                        WHERE  consecutivo IN (SELECT UNIQUE consecutivo_lote
                                               FROM tmp_act_edo
                                               WHERE tipo_movimiento = 877)

                        LET la_rets[1] = la_rets[1] + lr_cifras.cuantos - li_app
                        LET la_rets[7] = la_rets[7] + li_app
                        LET la_rets[3] = lr_cifras.cuantos
                        
                        DISPLAY "DESEMPLEO PRESENCIAL : ", la_rets[1] AT 12,09
                        DISPLAY "DESEMPLEO POR APP    : ", la_rets[7] AT 13,09
                        DISPLAY "TIPO B               : ", la_rets[3] AT 15,09

                    WHEN 878    -- Complementarios
                        SELECT COUNT(*)
                        INTO   li_app
                        FROM   ret_ws_notifica_app
                        WHERE  consecutivo IN (SELECT UNIQUE consecutivo_lote
                                               FROM tmp_act_edo
                                               WHERE tipo_movimiento = 878)

                        LET la_rets[1] = la_rets[1] + lr_cifras.cuantos - li_app
                        LET la_rets[7] = la_rets[7] + li_app
                        LET la_rets[4] = lr_cifras.cuantos
                        
                        DISPLAY "DESEMPLEO PRESENCIAL : ", la_rets[1] AT 12,09
                        DISPLAY "DESEMPLEO POR APP    : ", la_rets[7] AT 13,09
                        DISPLAY "COMPLEMENTARIOS      : ", la_rets[4] AT 16,09

                    WHEN 875    -- Anteriores
                        SELECT COUNT(*)
                        INTO   li_app
                        FROM   ret_ws_notifica_app
                        WHERE  consecutivo IN (SELECT UNIQUE consecutivo_lote
                                               FROM tmp_act_edo
                                               WHERE tipo_movimiento = 875)

                        LET la_rets[1] = la_rets[1] + lr_cifras.cuantos - li_app
                        LET la_rets[7] = la_rets[7] + li_app
                        LET la_rets[5] = lr_cifras.cuantos
                        
                        DISPLAY "DESEMPLEO PRESENCIAL : ", la_rets[1] AT 12,09
                        DISPLAY "DESEMPLEO POR APP    : ", la_rets[7] AT 13,09
                        DISPLAY "ANTERIORES           : ", la_rets[5] AT 17,09

                    WHEN 870
                        SELECT COUNT(*)
                        INTO   li_app
                        FROM   ret_ws_notifica_app
                        WHERE  consecutivo IN (SELECT UNIQUE consecutivo_lote
                                               FROM tmp_act_edo
                                               WHERE tipo_movimiento = 870)

                        LET la_rets[6] = lr_cifras.cuantos - li_app
                        LET la_rets[8] = la_rets[8] + li_app
                        
                        DISPLAY "MATRIMONIO PRESENCIAL: ", la_rets[6] AT 12,48
                        DISPLAY "MATRIMONIO POR APP   : ", la_rets[8] AT 13,48
                END CASE

            END FOREACH       
                                   
                            
            DISPLAY "(TERMINADO)" AT 11,40

    END CASE

END FUNCTION

