#################################################################################
# Proyecto           => SISTEMA DE AFORES( MEXICO )                             #
# Owner              => E.F.P.                                                  #
# Programa RETC0031P => REVERZAR LA LIQUIDACION DE RETIROS PARCIALES IMSS       #
# Fecha creacion     => 8 DE MARZO DE 2014                                      #
# By                 => JAVIER GONZALEZ JERONIMO                                #
# Actualizacion      =>                                                         #
# Fecha actualiz.    =>                                                         #
# Sistema            => RET                                                     #
#################################################################################
#################################################################################
# Modificacion por requerimiento CPL-1721. Se agrega validacion para fecha      #
# Sandra Longoria Neri.                                                         #
# 04-Septiembre-2014                                                           #
#################################################################################
DATABASE safre_af

GLOBALS

    DEFINE gr_bitacora RECORD LIKE ret_bitacora_rev.*

    -- Se debe modificar de acuerdo al numero de tablas reversadas
    DEFINE gar_tablas_rev ARRAY[7] OF RECORD
        accion      LIKE ret_tablas_rev.accion       ,
        tabla       LIKE ret_tablas_rev.tabla        ,
        num_regs    LIKE ret_tablas_rev.num_registros
    END RECORD

    DEFINE gr_edo RECORD
        provisionado            LIKE ret_estado.estado_solicitud ,
        preliquidado            LIKE ret_estado.estado_solicitud ,
        liquidado               LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE
        enter           CHAR(1)

    DEFINE
        HOY             DATE

    DEFINE
        gs_procesa      SMALLINT

    DEFINE
        gi_folio        INTEGER,
        gi_fecha        DATE  --CPL1721

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I

    CALL f_lib_crea_log()
    CALL init()

    CALL f_captura_folio()
        RETURNING gs_procesa, gi_folio, gi_fecha --CPL1721

    IF (gs_procesa = TRUE) THEN
        CALL f_reverso(gi_folio, gi_fecha) --CPL1721
        CALL f_act_bitacora(gi_folio)
        CALL f_lib_error_msg("REVERSO TERMINADO CORRECTAMENTE")
    END IF

    CLOSE WINDOW main_rev

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    DEFINE
        lc_prepare                  CHAR(300)

    -- -----------------------------------------------------------------------------

    LET HOY                 = TODAY
    LET gr_bitacora.usuario = f_lib_obten_user()

    -- Se almacenan los datos para la bitacora de reversos
    SELECT cod_tramite
    INTO   gr_bitacora.cod_tramite
    FROM   tab_tipo_tramite
    WHERE  descripcion = "RETIRO PARCIAL"

    LET gr_bitacora.programa      = "RETC0031P"
    LET gr_bitacora.desc_tramite  = "LIQ RETIRO PARCIAL IMSS"
    LET gr_bitacora.fecha_ini     = HOY
    LET gr_bitacora.hora_ini      = CURRENT HOUR TO SECOND

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.provisionado
    FROM   ret_estado A
    WHERE  A.descripcion = "PROVISIONADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.liquidado
    FROM   ret_estado A
    WHERE  A.descripcion = "LIQUIDADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.preliquidado
    FROM   ret_estado A
    WHERE  A.descripcion = "PRELIQUIDADO"

    ----- REVERSA DESMARCA -----
    LET lc_prepare = " EXECUTE PROCEDURE reversa_desmarca(?,?,?,?) "
    PREPARE eje_rev_desmarca FROM lc_prepare

    LET lc_prepare = " "

END FUNCTION

#---------------------------------------------------------------------------#
# f_captura_folio : Captura el folio que se usara para el reverso de la     #
#                   liquidacion de retiros parciales IMSS                   #
#---------------------------------------------------------------------------#
FUNCTION f_captura_folio()

    DEFINE lr_captura RECORD
        folio_liq           LIKE ret_parcial.folio,
        fecha_conversion    LIKE dis_cuenta.fecha_conversion --CPL1721 
    END RECORD

    DEFINE
        ls_procesa              INTEGER

    -- -----------------------------------------------------------------------------

    LET ls_procesa = TRUE

    SELECT MAX(folio)
    INTO   lr_captura.folio_liq
    FROM   ret_parcial
    WHERE  estado_solicitud = gr_edo.liquidado

    ---CPL1721
    SELECT MAX(fecha_conversion) 
    INTO   lr_captura.fecha_conversion
    FROM   dis_cuenta
    WHERE  folio = lr_captura.folio_liq
    ----
    
    OPEN WINDOW main_rev AT 4,4 WITH FORM "RETC0031P1" ATTRIBUTE(BORDER)
    DISPLAY " <CTRL-C> Salir                                             <ESC> Ejecutar " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC0031P  REVERSO DE LIQUIDACION DE RETIROS PARCIALES IMSS               " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    INPUT BY NAME lr_captura.* WITHOUT DEFAULTS

        {BEFORE INPUT
            IF ( (lr_captura.folio_liq IS NULL) OR (lr_captura.fecha_conversion IS NULL ) ) THEN
                CALL f_lib_error_msg("NO EXISTEN DATOS CON ESOS PARAMETROS")
                LET ls_procesa = FALSE 
                EXIT INPUT
            END IF}

    
        AFTER FIELD folio_liq
            IF lr_captura.folio_liq IS NULL THEN
                CALL f_lib_error_msg("EL FOLIO NO PUEDE SER NULO")
                NEXT FIELD folio_liq
            END IF

            -- Verifica que el folio exista y este liquidado
            SELECT "OK"
            FROM   ret_parcial
            WHERE  folio            = lr_captura.folio_liq
            AND    estado_solicitud = gr_edo.liquidado
            GROUP BY 1

            IF (SQLCA.SQLCODE = NOTFOUND) THEN
                CALL f_lib_error_msg("EL FOLIO INGRESADO NO ESTA LIQUIDADO")
                NEXT FIELD folio_liq
            END IF

            -- SE AGREGA VALIDACION DE FECHA CPL1721
            
            AFTER FIELD fecha_conversion
            IF lr_captura.fecha_conversion IS NULL THEN
                CALL f_lib_error_msg("LA FECHA NO PUEDE SER NULA")
                NEXT FIELD fecha_conversion
            END IF

            SELECT "OK"
            FROM   dis_cuenta
            WHERE  folio            = lr_captura.folio_liq
            AND    fecha_conversion = lr_captura.fecha_conversion
            GROUP BY 1

            IF (SQLCA.SQLCODE = NOTFOUND) THEN
                CALL f_lib_error_msg("LA FECHA INGRESADA NO ESTA LIQUIDADA")
                NEXT FIELD fecha_conversion
            END IF
            ---------------------------------------------
            
        ON KEY (ESC)
            IF lr_captura.folio_liq IS NULL THEN
                CALL f_lib_error_msg("EL FOLIO NO PUEDE SER NULO")
                NEXT FIELD folio_liq
            END IF

            -- Verifica que el folio exista y este liquidado
            SELECT "OK"
            FROM   ret_parcial
            WHERE  folio            = lr_captura.folio_liq
            AND    estado_solicitud = gr_edo.liquidado
            GROUP BY 1

            IF (SQLCA.SQLCODE = NOTFOUND) THEN
                CALL f_lib_error_msg("EL FOLIO INGRESADO NO ESTA LIQUIDADO")
                NEXT FIELD folio_liq
            END IF
            
            -- SE AGREGA VALIDACION DE FECHA CPL1721
            IF lr_captura.fecha_conversion IS NULL THEN
                CALL f_lib_error_msg("LA FECHA NO PUEDE SER NULA")
                NEXT FIELD fecha_conversion
            END IF

            SELECT "OK"
            FROM   dis_cuenta
            WHERE  folio            = lr_captura.folio_liq
            AND    fecha_conversion = lr_captura.fecha_conversion
            GROUP BY 1

            IF (SQLCA.SQLCODE = NOTFOUND) THEN
                CALL f_lib_error_msg("LA FECHA INGRESADA NO ESTA LIQUIDADA")
                NEXT FIELD fecha_conversion
            END IF
            ---------------------------------------------
            
            IF f_lib_pregunta("¿DESEA REALIZAR EL REVERSO DE LA LIQUIDACION? (S/N) : ") = TRUE THEN
                LET ls_procesa = TRUE
            ELSE
                CALL f_lib_error_msg("PROCESO CANCELADO")
                LET ls_procesa = FALSE
            END IF

            EXIT INPUT

        ON KEY (CONTROL-C, INTERRUPT)
            CALL f_lib_error_msg("PROCESO CANCELADO")
            LET ls_procesa = FALSE
            EXIT INPUT

    END INPUT

    RETURN ls_procesa, lr_captura.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_reverso : Ejecuta el reverso de la operacion solicitada                 #
#---------------------------------------------------------------------------#
# SE AGREGA FECHA DE LIQUIDACION CPL1721  (SANDRA LONGORIA)                 #
#---------------------------------------------------------------------------#
FUNCTION f_reverso(pi_folio, pi_fecha)

    DEFINE
        pi_folio           INTEGER,
        pi_fecha           DATE

    DEFINE lr_desmarca RECORD
        nss             LIKE ret_parcial.nss            ,
        marca_cod       LIKE cta_his_marca.marca_cod    ,
        consec          LIKE ret_parcial.consecutivo    ,
        fecha_ini       LIKE cta_his_marca.fecha_ini
    END RECORD

    DEFINE
        li_num_afect            INTEGER

    -----------------------------------------------------------------------------
    -- SE CREA TABLA TEMPORAL CON NSS PARA REVERSO POR FECHA Y FOLIO CPL1721
    SELECT UNIQUE nss nss_reverso  
    FROM dis_cuenta
    WHERE folio = pi_folio
    AND fecha_conversion  = pi_fecha
    INTO TEMP revliq_cpl1721

    DELETE
    FROM   dis_cuenta
    WHERE  folio            = pi_folio
    AND    fecha_conversion = pi_fecha

    LET li_num_afect = SQLCA.sqlerrd[3]

    DISPLAY "R. ELIMINADOS EN dis_cuenta                : ", li_num_afect
            USING "<<<,<<&" AT 11,14

    LET gar_tablas_rev[1].tabla     = "dis_cuenta"
    LET gar_tablas_rev[1].accion    = "BORRADA ", pi_fecha
    LET gar_tablas_rev[1].num_regs  = li_num_afect

    -----------------------------------------------------------------------------
    -- Reversa desmarca de registros liquidados

    LET li_num_afect  = 0

    DECLARE cur_rev_des CURSOR FOR
        SELECT  A.nss           ,
                A.marca_cod     ,
                A.correlativo   ,
                A.fecha_ini     ,
                A.fecha_fin
        FROM    cta_his_marca A ,
                ret_parcial B,
                dis_cuenta  D   
        WHERE   A.nss               = B.nss
        AND     A.correlativo       = B.consecutivo
        AND     B.folio             = pi_folio
        AND     B.estado_solicitud  = gr_edo.liquidado
        AND     A.marca_cod IN (870, 875)
        AND     A.fecha_fin IS NOT NULL
        AND     A.nss               = D.nss
        AND     D.fecha_conversion  = pi_fecha
        ORDER BY B.tipo_retiro, B.nss

    FOREACH cur_rev_des INTO lr_desmarca.*
        EXECUTE eje_rev_desmarca USING lr_desmarca.*

        IF (SQLCA.SQLCODE = 0) THEN
            LET li_num_afect = li_num_afect + 1
        END IF
    END FOREACH

    DISPLAY "MARCAS REVERSADAS POR FUNCION              : ", li_num_afect
            USING "<<<,<<&" AT 12,14

    LET gar_tablas_rev[2].tabla     = "cta_act_marca"
    LET gar_tablas_rev[2].accion    = "REV x SPL ", pi_fecha
    LET gar_tablas_rev[2].num_regs  = li_num_afect

    DELETE
    FROM   ret_marca_pensionado
    WHERE  folio                = pi_folio
    AND    nss IN (SELECT nss_reverso FROM revliq_cpl1721) --CPL1721
    
    LET li_num_afect = SQLCA.sqlerrd[3]

    DISPLAY "R. ELIMINADOS EN ret_marca_pensionado      : ", li_num_afect
            USING "<<<,<<&" AT 13,14

    LET gar_tablas_rev[3].tabla     = "ret_marca_pensionado"
    LET gar_tablas_rev[3].accion    = "BORRADA ", pi_fecha
    LET gar_tablas_rev[3].num_regs  = li_num_afect

    -----------------------------------------------------------------------------

    UPDATE ret_ctr_pago
    SET    estado       = gr_edo.provisionado
    WHERE  folio_op12   = pi_folio
    AND    estado       = gr_edo.liquidado
    AND    nss IN (SELECT nss_reverso FROM revliq_cpl1721) --CPL1721  

    LET li_num_afect = SQLCA.SQLERRD[3]

    DISPLAY "R. ACTUALIZADOS EN ret_ctr_pago            : ", li_num_afect
             USING "<<<,<<&" AT 14,14
    
    LET gar_tablas_rev[4].tabla     = "ret_ctr_pago"
    LET gar_tablas_rev[4].accion    = "ACTUALIZA ", pi_fecha
    LET gar_tablas_rev[4].num_regs  = li_num_afect    

    -----------------------------------------------------------------------------
    
    UPDATE ret_ctr_pago_det
    SET    estado       = gr_edo.provisionado ,
           folio_op16   = 0
    WHERE  folio_op16   = pi_folio
    AND    estado       = gr_edo.liquidado
    AND    nss IN (SELECT nss_reverso FROM revliq_cpl1721) --CPL1721

    LET li_num_afect = SQLCA.SQLERRD[3]

    DISPLAY "R. ACTUALIZADOS EN ret_ctr_pago_det        : ", li_num_afect
             USING "<<<,<<&" AT 15,14
    
    LET gar_tablas_rev[5].tabla     = "ret_ctr_pago_det"
    LET gar_tablas_rev[5].accion    = "ACTUALIZA ", pi_fecha
    LET gar_tablas_rev[5].num_regs  = li_num_afect    

    -----------------------------------------------------------------------------

    UPDATE ret_parcial_tx
    SET    fecha_valuacion  = HOY   ,
           fecha_pago       = HOY
    WHERE  folio            = pi_folio
    AND    nss IN (SELECT nss_reverso FROM revliq_cpl1721) --INV2911

    LET li_num_afect = SQLCA.sqlerrd[3]

    DISPLAY "R. ACTUALIZADOS EN ret_parcial_tx          : ", li_num_afect
             USING "<<<,<<&" AT 16,14

    LET gar_tablas_rev[6].tabla     = "ret_parcial_tx"
    LET gar_tablas_rev[6].accion    = "ACTUALIZA ", pi_fecha
    LET gar_tablas_rev[6].num_regs  = li_num_afect

    -----------------------------------------------------------------------------

    UPDATE ret_parcial
    SET    estado_solicitud = gr_edo.preliquidado
    WHERE  folio            = pi_folio
    AND    estado_solicitud = gr_edo.liquidado
    AND    nss IN (SELECT nss_reverso FROM revliq_cpl1721) --CPL1721

    LET li_num_afect = SQLCA.sqlerrd[3]

    DISPLAY "R. ACTUALIZADOS EN ret_parcial             : ", li_num_afect
             USING "<<<,<<&" AT 17,14

    LET gar_tablas_rev[7].tabla     = "ret_parcial"
    LET gar_tablas_rev[7].accion    = "ACTUALIZA ", pi_fecha
    LET gar_tablas_rev[7].num_regs  = li_num_afect

    -----------------------------------------------------------------------------
END FUNCTION

#---------------------------------------------------------------------------#
# f_act_bitacora : Inserta en las tablas de bitacora y de tablas del        #
#                  reverso realizado                                        #
#---------------------------------------------------------------------------#
FUNCTION f_act_bitacora(pi_folio)

    DEFINE pi_folio     LIKE ret_bitacora_rev.folio

    DEFINE li_ind_rev   LIKE ret_bitacora_rev.id_rev

    DEFINE
        li_num_afect               SMALLINT

    -- -----------------------------------------------------------------------------

    LET gr_bitacora.folio       = pi_folio
    LET gr_bitacora.fecha_fin   = TODAY
    LET gr_bitacora.hora_fin    = CURRENT HOUR TO SECOND

    INSERT INTO ret_bitacora_rev
    VALUES (gr_bitacora.*)

    -- Rescatamos el serial que se le asigno al movimiento
    SELECT MAX(id_rev)
    INTO   li_ind_rev
    FROM   ret_bitacora_rev
    WHERE  folio = gr_bitacora.folio

    FOR li_num_afect = 1 TO 7
        IF gar_tablas_rev[li_num_afect].num_regs > 0 THEN
            INSERT INTO ret_tablas_rev
            VALUES (li_ind_rev                  ,
                    gr_bitacora.folio           ,
                    gar_tablas_rev[li_num_afect].*
                   )
        END IF
    END FOR

END FUNCTION

