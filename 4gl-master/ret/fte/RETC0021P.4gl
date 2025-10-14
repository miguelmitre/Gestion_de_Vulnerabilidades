#################################################################################
# Proyecto           => SISTEMA DE AFORES( MEXICO )                             #
# Owner              => E.F.P.                                                  #
# Programa RETC0021P => REVERZO DE LA PRELIQUIDACION DE RETIROS PARCIALES IMSS  #
# Fecha creacion     => 2 DE MAYO DE 2014                                       #
# By                 => JAVIER GONZALEZ JERONIMO                                #
# Actualizacion      =>                                                         #
# Fecha              =>                                                         #
# Sistema            => RET                                                     #
#################################################################################
# Modificacion por requerimiento CPL-1721. Se agrega validacion para fecha      #
# Sandra Longoria Neri.                                                         #
# 04- Septiembre-2014                                                           #
#################################################################################

DATABASE safre_af

GLOBALS

    DEFINE gr_bitacora RECORD LIKE ret_bitacora_rev.*

    -- Se debe modificar de acuerdo al numero de tablas reversadas
    DEFINE gar_tablas_rev ARRAY[4] OF RECORD
        accion      LIKE ret_tablas_rev.accion       ,
        tabla       LIKE ret_tablas_rev.tabla        ,
        num_regs    LIKE ret_tablas_rev.num_registros
    END RECORD

    DEFINE gr_edo RECORD
        recibido            LIKE ret_estado.estado_solicitud    ,
        preliquidado        LIKE ret_estado.estado_solicitud    ,
        rechazado           LIKE ret_estado.estado_solicitud 
    END RECORD
 
    DEFINE 
        enter               CHAR(1)

    DEFINE 
        HOY                 DATE

    DEFINE
        gs_procesa          SMALLINT

    DEFINE 
        gi_folio            INTEGER,
        gi_fecha            DATE

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
    --- se agrega campo de fecha de captura CPL1721
    RETURNING gs_procesa, gi_folio, gi_fecha

    IF (gs_procesa = TRUE) THEN 
        CALL f_reverso(gi_folio, gi_fecha)
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
        lc_prepare      CHAR(300)

    -- -----------------------------------------------------------------------------

    LET HOY                 = TODAY
    LET gr_bitacora.usuario = f_lib_obten_user()

    -- Se almacenan los datos para la bitacora de reversos
    SELECT cod_tramite
    INTO   gr_bitacora.cod_tramite
    FROM   tab_tipo_tramite
    WHERE  descripcion = "RETIRO PARCIAL"

    LET gr_bitacora.programa      = "RETC0021P"
    LET gr_bitacora.desc_tramite  = "PRELIQ RETIRO PARCIAL IMSS"
    LET gr_bitacora.fecha_ini     = HOY
    LET gr_bitacora.hora_ini      = CURRENT HOUR TO SECOND

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.recibido
    FROM   ret_estado A
    WHERE  A.descripcion = "RECEPCIONADO"
 
    SELECT A.estado_solicitud
    INTO   gr_edo.preliquidado
    FROM   ret_estado A
    WHERE  A.descripcion = "PRELIQUIDADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.rechazado
    FROM   ret_estado A
    WHERE  A.descripcion = "RECHAZADO"

    ----- REVERSA DESMARCA -----
    LET lc_prepare = " EXECUTE PROCEDURE reversa_desmarca(?,?,?,?) "
    PREPARE eje_rev_desmarca FROM lc_prepare

    LET lc_prepare = " "
    
END FUNCTION

#---------------------------------------------------------------------------#
# f_captura_folio : Captura el folio que se usara para el reverso de la     #
#                   preliquidacion de retiros parciales IMSS                #
#---------------------------------------------------------------------------#
FUNCTION f_captura_folio()

    DEFINE lr_captura RECORD
        folio_preliq        LIKE ret_parcial.folio,
        fecha_conversion    LIKE ret_preliquida.fecha_conversion --CPL1721
    END RECORD 

    DEFINE
        ls_procesa              INTEGER

    LET ls_procesa = TRUE

    SELECT MAX(folio)
    INTO   lr_captura.folio_preliq 
    FROM   ret_parcial
    WHERE  estado_solicitud IN (gr_edo.preliquidado, gr_edo.rechazado)

    -- CPL1721
    SELECT MAX(fecha_conversion)
    INTO   lr_captura.fecha_conversion   
    FROM   ret_preliquida
    WHERE  folio = lr_captura.folio_preliq
    AND    nss IN ( SELECT nss FROM   ret_parcial
      WHERE  folio = lr_captura.folio_preliq
      AND    estado_solicitud IN (gr_edo.preliquidado, gr_edo.rechazado))
    --------
    
    OPEN WINDOW main_rev AT 4,4 WITH FORM "RETC0021P1" ATTRIBUTE(BORDER) 
    DISPLAY " <CTRL-C> Salir                                             <ESC> Ejecutar " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC0021P  REVERSO DE PRELIQUIDACION DE RETIRO PARCIAL IMSS                " AT 3,1 ATTRIBUTE(REVERSE)    
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)
    
    INPUT BY NAME lr_captura.* WITHOUT DEFAULTS

        {BEFORE INPUT
            IF ( (lr_captura.folio_preliq IS NULL) OR (lr_captura.fecha_conversion IS NULL ) ) THEN
                CALL f_lib_error_msg("NO EXISTEN DATOS CON ESOS PARAMETROS")
                LET ls_procesa = FALSE
                EXIT INPUT
            END IF}
            
        AFTER FIELD folio_preliq
            IF lr_captura.folio_preliq IS NULL THEN
                CALL f_lib_error_msg("EL FOLIO NO PUEDE SER NULO")
                NEXT FIELD folio_preliq
            END IF

            -- Verifica que el folio exista y este preliquidado
            SELECT "OK"
            FROM   ret_parcial
            WHERE  folio            = lr_captura.folio_preliq
            AND    estado_solicitud IN (gr_edo.preliquidado, gr_edo.rechazado)
            GROUP BY 1

            IF (SQLCA.SQLCODE = NOTFOUND) THEN
                CALL f_lib_error_msg("EL FOLIO INGRESADO NO ESTA PRELIQUIDADO")
                NEXT FIELD folio_preliq
            END IF
            
        AFTER FIELD fecha_conversion
            IF lr_captura.fecha_conversion IS NULL THEN
                CALL f_lib_error_msg("LA FECHA NO PUEDE SER NULA")
                NEXT FIELD fecha_conversion
            END IF
             
            -- SE AGREGA VALIDACION DE FECHA CPL1721
            SELECT "OK"
            FROM   ret_preliquida
            WHERE  folio            = lr_captura.folio_preliq
            AND    fecha_conversion = lr_captura.fecha_conversion
            AND nss IN ( SELECT nss FROM ret_parcial
            WHERE  folio = lr_captura.folio_preliq
            AND    estado_solicitud IN (gr_edo.preliquidado, gr_edo.rechazado))
            GROUP BY 1

            IF (SQLCA.SQLCODE = NOTFOUND) THEN
                CALL f_lib_error_msg("LA FECHA CAPTURADA NO ESTA PRELIQUIDADA")
                NEXT FIELD fecha_conversion
            END IF
            ---------------------------------------------
            
        ON KEY (ESC)
            IF lr_captura.folio_preliq IS NULL THEN
                CALL f_lib_error_msg("EL FOLIO NO PUEDE SER NULO")
                NEXT FIELD folio_preliq
            END IF

            -- Verifica que el folio exista y este preliquidado
            SELECT "OK"
            FROM   ret_parcial
            WHERE  folio            = lr_captura.folio_preliq
            AND    estado_solicitud IN (gr_edo.preliquidado, gr_edo.rechazado)
            GROUP BY 1

            IF (SQLCA.SQLCODE = NOTFOUND) THEN
                CALL f_lib_error_msg("EL FOLIO INGRESADO NO ESTA PRELIQUIDADO")
                NEXT FIELD folio_preliq
            END IF
            
            -- CPL1721
            IF lr_captura.fecha_conversion IS NULL THEN
                CALL f_lib_error_msg("LA FECHA NO PUEDE SER NULA")
                NEXT FIELD fecha_conversion
            END IF

            -- SE AGREGA VALIDACION DE FECHA CPL1721
            SELECT "OK"
            FROM   ret_preliquida
            WHERE  folio            = lr_captura.folio_preliq
            AND    fecha_conversion = lr_captura.fecha_conversion
            AND nss IN ( SELECT nss FROM   ret_parcial
            WHERE  folio = lr_captura.folio_preliq
            AND    estado_solicitud IN (gr_edo.preliquidado, gr_edo.rechazado))
            GROUP BY 1

            IF (SQLCA.SQLCODE = NOTFOUND) THEN
                CALL f_lib_error_msg("LA FECHA CAPTURADA NO ESTA PRELIQUIDADA")
                NEXT FIELD fecha_conversion
            END IF
            ---------------------------------------------
            
            IF f_lib_pregunta("¿DESEA REALIZAR EL REVERSO DE LA PRELIQUIDACION? (S/N) ") = TRUE THEN
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
FUNCTION f_reverso(pi_folio, pi_fecha)

    DEFINE 
        pi_folio        INTEGER,
        pi_fecha        DATE 

    DEFINE lr_desmarca RECORD
        nss         LIKE ret_parcial.nss            ,
        marca_cod   LIKE cta_his_marca.marca_cod    ,    
        consec      LIKE ret_parcial.consecutivo    ,
        fecha_ini   LIKE cta_his_marca.fecha_ini   
    END RECORD 

    DEFINE 
        li_num_afect        INTEGER

    -- -----------------------------------------------------------------------------
   -- -----------------------------------------------------------------------------
    -- SE AGREGA TABLA TEMPORAL CON NSS'S A REVERSAR POR FOLIO Y FECHA PARA POSTERIO
    -- BORRAR DE RET_PRELIQUIDA Y RET_MONTO_SIEFORE

    SELECT UNIQUE nss nss_reverso  
    FROM ret_preliquida
    WHERE folio = pi_folio
    AND fecha_conversion  = pi_fecha
    AND nss IN ( SELECT nss FROM ret_parcial
    WHERE  folio = pi_folio
    AND    estado_solicitud IN (gr_edo.preliquidado, gr_edo.rechazado))
    INTO TEMP reverso_cpl1721
 
    DELETE 
    FROM   ret_preliquida
    WHERE  folio = pi_folio
    AND    fecha_conversion = pi_fecha
    AND nss IN ( SELECT nss FROM   ret_parcial
        WHERE  folio = pi_folio
        AND    estado_solicitud IN (gr_edo.preliquidado, gr_edo.rechazado))
    
    LET li_num_afect = SQLCA.sqlerrd[3]

    DISPLAY "R. ELIMINADOS EN ret_preliquida        : ", li_num_afect
            USING "<<<,<<&" AT 12,14

    LET gar_tablas_rev[1].tabla     = "ret_preliquida"
    LET gar_tablas_rev[1].accion    = "BORRADA con fecha:", pi_fecha
    LET gar_tablas_rev[1].num_regs  = li_num_afect

    -----------------------------------------------------------------------------
    -- Reversa desmarca de registros rechazados

    LET li_num_afect = 0

    DECLARE cur_rev_des CURSOR FOR
        SELECT  A.nss               ,
                A.marca_cod         ,
                A.correlativo       ,
                A.fecha_ini         ,
                A.fecha_fin
        FROM    cta_his_marca A             ,
                ret_parcial B               ,
                tab_diag_procesar_disp C    ,
                ret_preliquida D
        WHERE   A.nss               = B.nss
        AND     A.correlativo       = B.consecutivo
        AND     B.diag_cuenta_ind   = C.diag_procesar
        AND     C.id_aceptado       = 1
        AND     B.folio             = pi_folio
        AND     B.estado_solicitud  = gr_edo.rechazado
        AND     A.marca_cod in (870, 875)
        AND     A.fecha_fin IS NOT NULL
        AND     A.nss               = D.nss   -- CPL1721
        AND     D.fecha_conversion  = pi_fecha --CPL1721
        ORDER BY B.tipo_retiro, B.nss

    FOREACH cur_rev_des INTO lr_desmarca.*
        EXECUTE eje_rev_desmarca USING lr_desmarca.*

        IF (SQLCA.SQLCODE = 0) THEN
            LET li_num_afect = li_num_afect + 1
        END IF
    END FOREACH

    DISPLAY "MARCAS REVERSADAS POR FUNCION          : ", li_num_afect
            USING "<<<,<<&" AT 13,14

    LET gar_tablas_rev[2].tabla     = "cta_act_marca"
    LET gar_tablas_rev[2].accion    = "REV X SPL ", pi_fecha
    LET gar_tablas_rev[2].num_regs  = li_num_afect
        
    DELETE
    FROM   ret_monto_siefore
    WHERE  folio            = pi_folio
    AND    tipo_operacion   = 16
    AND    nss IN (SELECT nss_reverso FROM reverso_cpl1721) --CPL1721
    
    LET li_num_afect = SQLCA.sqlerrd[3]

    DISPLAY "R. ELIMINADOS EN ret_monto_siefore     : ", li_num_afect
            USING "<<<,<<&" AT 14,14

    LET gar_tablas_rev[3].tabla     = "ret_monto_siefore"
    LET gar_tablas_rev[3].accion    = "BORRADA ", pi_fecha
    LET gar_tablas_rev[3].num_regs  = li_num_afect

    -----------------------------------------------------------------------------
   
    UPDATE ret_parcial
    SET    estado_solicitud = gr_edo.recibido
    WHERE  folio            = pi_folio
    AND    diag_cuenta_ind  = 400 -- Unico diagnostico aceptado en parciales
    AND    estado_solicitud IN (gr_edo.preliquidado, gr_edo.rechazado)
    AND    nss IN (SELECT nss_reverso FROM reverso_cpl1721) --CPL1721

    LET li_num_afect = SQLCA.sqlerrd[3]
    DISPLAY "R. ACTUALIZADOS EN ret_parcial         : ", li_num_afect
             USING "<<<,<<&" AT 15,14
    
    LET gar_tablas_rev[4].tabla     = "ret_parcial"
    LET gar_tablas_rev[4].accion    = "ACTUALIZA ", pi_fecha
    LET gar_tablas_rev[4].num_regs  = li_num_afect

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

    FOR li_num_afect = 1 TO 4
        IF gar_tablas_rev[li_num_afect].num_regs > 0 THEN 
            INSERT INTO ret_tablas_rev
            VALUES (li_ind_rev                  ,
                    gr_bitacora.folio           ,
                    gar_tablas_rev[li_num_afect].*
                   )
        END IF 
    END FOR

END FUNCTION
