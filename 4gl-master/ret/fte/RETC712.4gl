#################################################################################
#Proyecto          => SISTEMA DE AFORES ( SAFRE )                               #
#Propietario       => E.F.P.                                                    #
#Programa RETC712  => REALIZA EL REVERSO DE LA LIQUIDACION DE DISPOSICIONES     #
#Fecha creacion    => 18 DE FEBRERO DE 2010                                     #
#Desarrado por     => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   => 4 DE ABRIL DE 2011                                        #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                  => Se agrega el revierso a las tablas de las estadisticas de #
#                     CONSAR (Req. EFPS-152)                                    #
#Fecha actualiz.   => 11 DE MAYO DE 2011                                        #
#                  => Se realizan las modificaciones al codigo para incluir     #
#                     el llamado de las funciones para realizar el marcaje de   #
#                     trabajador pensionado (REQ. EFPS-157)                     #
#Sistema           => RET                                                       #
#################################################################################
DATABASE safre_af

GLOBALS

    DEFINE gr_bitacora RECORD LIKE ret_bitacora_rev.*

    DEFINE gar_tablas_rev ARRAY[10] OF RECORD
        accion          LIKE ret_tablas_rev.accion       ,
        tabla           LIKE ret_tablas_rev.tabla        ,
        num_regs        LIKE ret_tablas_rev.num_registros
    END RECORD

    DEFINE gr_edo RECORD
        capturado       LIKE ret_estado.estado_solicitud ,
        enviado         LIKE ret_estado.estado_solicitud ,
        liquidado       LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE gr_datos RECORD 
        folio           LIKE ret_solicitud_tx.folio ,
        tipo_retiro     LIKE ret_solicitud_tx.tipo_retiro
    END RECORD

    DEFINE
        gs_procesa      SMALLINT

    DEFINE
        HOY             DATE

    DEFINE
        enter           CHAR     

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL f_lib_crea_log()
    CALL init()
    
    CALL f_captura_datos() RETURNING gs_procesa, gr_datos.*
    
    IF (gs_procesa = TRUE) THEN
        CALL f_reverso(gr_datos.*)
        CALL f_act_bitacora(gr_datos.*)
        CALL f_lib_error_msg("REVERSO TERMINADO CORRECTAMENTE")
    END IF 
    
    CLOSE WINDOW win_main_rev

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

    ----- DATOS PARA LA BITACORA DE REVERSOS ----- 
    SELECT cod_tramite
    INTO   gr_bitacora.cod_tramite
    FROM   tab_tipo_tramite
    WHERE  descripcion = "DISPOSICION"

    LET gr_bitacora.programa    = "RETC712"
    LET gr_bitacora.fecha_ini   = HOY
    LET gr_bitacora.hora_ini    = CURRENT HOUR TO SECOND

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.capturado
    FROM   ret_estado A
    WHERE  A.descripcion = "CAPTURADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.enviado
    FROM   ret_estado A
    WHERE  A.descripcion = "ENVIADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.liquidado
    FROM   ret_estado A
    WHERE  A.descripcion = "LIQUIDADO"

    ----- REVERSA DESMARCA -----
    LET lc_prepare = " EXECUTE PROCEDURE reversa_desmarca(?,?,?,?) "
    PREPARE eje_rev_desmarca FROM lc_prepare
    
    LET lc_prepare = " "

END FUNCTION

#---------------------------------------------------------------------------#
# f_captura_datos : Captura el folio y el tipo de retiro del que se         #
#                   hara el reverso de la liquidacion                       #
#---------------------------------------------------------------------------#
FUNCTION f_captura_datos()

    DEFINE lr_captura RECORD 
        folio           LIKE ret_solicitud_tx.folio         ,
        tipo_retiro     LIKE ret_solicitud_tx.tipo_retiro
    END RECORD
    
    DEFINE ls_estado LIKE ret_solicitud_tx.estado_solicitud

    DEFINE
        lc_cad_msg          CHAR(100)

    DEFINE
        ls_procesa          SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_procesa = TRUE

    OPEN WINDOW win_main_rev AT 4,4 WITH FORM "RETC7121" ATTRIBUTE(BORDER)
    DISPLAY " <Ctrl-C> Salir                                                                " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC712     REVERSO DE LIQUIDACION - RETIRO POR DISPOSICION                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    INPUT BY NAME lr_captura.* WITHOUT DEFAULTS

        AFTER FIELD folio
            IF lr_captura.folio IS NULL THEN
                CALL f_lib_error_msg("EL FOLIO NO PUEDE SER NULO")
                NEXT FIELD folio
            END IF

        AFTER FIELD tipo_retiro
            IF lr_captura.tipo_retiro IS NULL THEN
                CALL f_lib_error_msg("EL TIPO DE RETIRO NO PUEDE SER NULO")
                NEXT FIELD tipo_retiro
            ELSE
                CALL f_verifica_estado(lr_captura.*) RETURNING ls_estado, lc_cad_msg
                
                IF (ls_estado <> 0) THEN
                    CALL f_lib_error_msg(lc_cad_msg)
                    NEXT FIELD folio
                END IF
            END IF

        ON KEY (ESC)
            IF lr_captura.folio IS NULL THEN
                CALL f_lib_error_msg("EL FOLIO NO PUEDE SER NULO")
                NEXT FIELD folio
            END IF

            IF lr_captura.tipo_retiro IS NULL THEN
                CALL f_lib_error_msg("EL TIPO DE RETIRO NO PUEDE SER NULO")
                NEXT FIELD tipo_retiro
            ELSE
                CALL f_verifica_estado(lr_captura.*) RETURNING ls_estado, lc_cad_msg
                IF ls_estado <> 0 THEN
                    CALL f_lib_error_msg(lc_cad_msg)
                    NEXT FIELD folio
                END IF
            END IF

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
FUNCTION f_reverso(pr_datos)

    DEFINE pr_datos RECORD 
        folio           LIKE ret_solicitud_tx.folio         ,
        tipo_retiro     LIKE ret_solicitud_tx.tipo_retiro
    END RECORD

    DEFINE lr_desmarca RECORD
        nss         LIKE ret_solicitud_tx.nss           ,
        marca_cod   LIKE cta_his_marca.marca_cod        ,    
        consec      LIKE ret_solicitud_tx.consecutivo   ,
        fecha_ini   LIKE cta_his_marca.fecha_ini   
    END RECORD 

    DEFINE
        ldt_fecha_fin           ,
        ldt_fecha_liquida       DATE 

    DEFINE 
        ls_movimiento           SMALLINT

    DEFINE 
        li_cont                 INTEGER

    -- -----------------------------------------------------------------------------

    SELECT movimiento
    INTO   ls_movimiento
    FROM   tab_retiro
    WHERE  tipo_retiro   = pr_datos.tipo_retiro

    LET li_cont  = 0
    
    DISPLAY " PROCESANDO INFORMACION " AT 19,2 ATTRIBUTE(REVERSE)

    -----------------------------------------------------------------------------    
    SELECT UNIQUE(fecha_conversion)
    INTO   ldt_fecha_liquida
    FROM   dis_cuenta
    WHERE  folio        = pr_datos.folio

    DISPLAY "Fecha de Liquidacion :        ", ldt_fecha_liquida AT 7,16

    -----------------------------------------------------------------------------    
    DECLARE cur_3 CURSOR FOR
        SELECT  A.nss         ,
                A.marca_cod   ,
                A.correlativo ,
                A.fecha_ini   ,
                A.fecha_fin
        FROM    cta_his_marca A     ,
                ret_solicitud_tx B
        WHERE   A.nss               = B.nss
        AND     A.correlativo       = B.consecutivo
        AND     A.marca_cod         = ls_movimiento
        AND     B.folio             = pr_datos.folio
        AND     B.tipo_retiro       = pr_datos.tipo_retiro
        AND     B.estado_solicitud  = gr_edo.liquidado
        AND     A.fecha_fin IS NOT NULL

    FOREACH cur_3 INTO lr_desmarca.*, ldt_fecha_fin
    
        IF ldt_fecha_fin IS NOT NULL THEN    
            EXECUTE eje_rev_desmarca USING lr_desmarca.*
            IF SQLCA.SQLCODE = 0 THEN
                LET li_cont = li_cont + 1
            END IF        
        END IF 
    END FOREACH

    DISPLAY "MARCAS REVERSADAS POR FUNCION           : ", li_cont
            USING "<<<,<<&" AT 9,10

    LET gar_tablas_rev[1].tabla     = "cta_act_marca"
    LET gar_tablas_rev[1].accion    = "REVERSO POR SPL"
    LET gar_tablas_rev[1].num_regs  = li_cont

    -----------------------------------------------------------------------------

    DELETE
    FROM   dis_cuenta
    WHERE  folio        = pr_datos.folio
    AND    id_aportante = "RETIRO"

    LET li_cont = SQLCA.SQLERRD[3]
    
    DISPLAY "R. ELIMINADOS EN dis_cuenta             : ", li_cont
            USING "<<<,<<&" AT 10,10

    LET gar_tablas_rev[2].tabla     = "dis_cuenta"
    LET gar_tablas_rev[2].accion    = "BORRADA"
    LET gar_tablas_rev[2].num_regs  = li_cont    
    
    -----------------------------------------------------------------------------

    DELETE
    FROM  ret_consar_det_liquida
    WHERE folio_liquida = pr_datos.folio

    LET li_cont = SQLCA.SQLERRD[3]
    
    DISPLAY "R. ELIMINADOS EN ret_consar_det_liquida : ", li_cont
            USING "<<<,<<&" AT 11,10

    LET gar_tablas_rev[3].tabla     = "ret_consar_det_liquida"
    LET gar_tablas_rev[3].accion    = "BORRADA"
    LET gar_tablas_rev[3].num_regs  = li_cont    
    
    -----------------------------------------------------------------------------
    
    DELETE
    FROM  ret_consar_liquida
    WHERE folio_liquida = pr_datos.folio

    LET li_cont = SQLCA.SQLERRD[3]
    
    DISPLAY "R. ELIMINADOS EN ret_consar_liquida     : ", li_cont
            USING "<<<,<<&" AT 12,10

    LET gar_tablas_rev[4].tabla     = "ret_consar_liquida"
    LET gar_tablas_rev[4].accion    = "BORRADA"
    LET gar_tablas_rev[4].num_regs  = li_cont    
    
    -----------------------------------------------------------------------------
    
    DELETE
    FROM  ret_consar
    WHERE folio_liquida = pr_datos.folio

    LET li_cont = SQLCA.SQLERRD[3]
    
    DISPLAY "R. ELIMINADOS EN ret_consar             : ", li_cont
            USING "<<<,<<&" AT 13,10

    LET gar_tablas_rev[5].tabla     = "ret_consar"
    LET gar_tablas_rev[5].accion    = "BORRADA"
    LET gar_tablas_rev[5].num_regs  = li_cont    
    
    -----------------------------------------------------------------------------
    
    DELETE
    FROM  ret_marca_pensionado
    WHERE folio = pr_datos.folio

    LET li_cont = SQLCA.SQLERRD[3]
    
    DISPLAY "R. ELIMINADOS EN ret_marca_pensionado   : ", li_cont
            USING "<<<,<<&" AT 14,10

    LET gar_tablas_rev[6].tabla     = "ret_marca_pensionado"
    LET gar_tablas_rev[6].accion    = "BORRADA"
    LET gar_tablas_rev[6].num_regs  = li_cont    
    
    -----------------------------------------------------------------------------

    DELETE
    FROM   cta_ctr_proceso
    WHERE  nss IN (SELECT nss                   
                   FROM   ret_solicitud_tx      
                   WHERE  folio = pr_datos.folio
                  )
    AND    factualiza   = HOY
    AND    folio IS NULL
 
    LET li_cont = SQLCA.SQLERRD[3]
    
    DISPLAY "R. ELIMINADOS EN cta_ctr_proceso        : ", li_cont
            USING "<<<,<<&" AT 15,10

    LET gar_tablas_rev[7].tabla     = "cta_ctr_proceso"
    LET gar_tablas_rev[7].accion    = "BORRADA"
    LET gar_tablas_rev[7].num_regs  = li_cont    
    
    -----------------------------------------------------------------------------

    UPDATE ret_solicitud_tx
    SET    estado_solicitud   = gr_edo.enviado
    WHERE  folio              = pr_datos.folio
    AND    tipo_retiro        = pr_datos.tipo_retiro
    AND    estado_solicitud   = gr_edo.liquidado
    
    LET li_cont = SQLCA.sqlerrd[3]

    DISPLAY "R. ACTUALIZADOS EN ret_solicitud_tx     : ", li_cont
            USING "<<<,<<&" AT 16,10
                       
    LET gar_tablas_rev[8].tabla     = "ret_solicitud_tx"
    LET gar_tablas_rev[8].accion    = "ACTUALIZA"
    LET gar_tablas_rev[8].num_regs  = li_cont
    
    -----------------------------------------------------------------------------

END FUNCTION

#---------------------------------------------------------------------------#
# f_act_bitacora : Inserta en las tablas de bitacora y de tablas del        #
#                  reverso realizado                                        #
#---------------------------------------------------------------------------#
FUNCTION f_act_bitacora(pr_datos)

    DEFINE pr_datos RECORD 
        folio           LIKE ret_solicitud_tx.folio         ,
        tipo_retiro     LIKE ret_solicitud_tx.tipo_retiro
    END RECORD
    
    DEFINE li_ind_rev   LIKE ret_bitacora_rev.id_rev

    DEFINE
        li_num_afect               SMALLINT

    -- -----------------------------------------------------------------------------

    LET gr_bitacora.folio         = pr_datos.folio
    LET gr_bitacora.fecha_fin     = TODAY
    LET gr_bitacora.hora_fin      = CURRENT HOUR TO SECOND
    LET gr_bitacora.desc_tramite  = "LIQUIDACION RETIRO ", pr_datos.tipo_retiro

    INSERT INTO ret_bitacora_rev
    VALUES (gr_bitacora.*)

    -- Rescatamos el serial que se le asigno al movimiento
    SELECT MAX(id_rev)
    INTO   li_ind_rev
    FROM   ret_bitacora_rev
    WHERE  folio = gr_bitacora.folio

    FOR li_num_afect = 1 TO 8
        IF (gar_tablas_rev[li_num_afect].num_regs > 0) THEN 
            INSERT INTO ret_tablas_rev
            VALUES (li_ind_rev                      ,
                    gr_bitacora.folio               ,
                    gar_tablas_rev[li_num_afect].*
                   )
        END IF
    END FOR

END FUNCTION

#---------------------------------------------------------------------------#
# f_verifica_estado : Valida que los datos capturados sean correctos        #
#---------------------------------------------------------------------------#
FUNCTION f_verifica_estado(lr_datos)

    DEFINE lr_datos RECORD 
        folio           LIKE ret_solicitud_tx.folio ,
        tipo_retiro     LIKE ret_solicitud_tx.tipo_retiro
    END RECORD

    DEFINE
        ls_estado   LIKE ret_solicitud_tx.estado_solicitud

    DEFINE
        lc_error    CHAR(100)

    DEFINE 
        vtipo_movimiento  ,
        ls_id             SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_id = 0
    INITIALIZE ls_estado TO NULL

    SELECT MAX(estado_solicitud)
    INTO   ls_estado
    FROM   ret_solicitud_tx    
    WHERE  folio            = lr_datos.folio
    AND    tipo_retiro      = lr_datos.tipo_retiro
    
    IF (STATUS = NOTFOUND) OR (ls_estado IS NULL) THEN
        LET lc_error = "FOLIO INEXISTENTE"
        LET ls_id = 1
    ELSE
        IF ls_estado <> gr_edo.liquidado THEN
            LET lc_error = "EL ESTADO DEBE SER 8 - LIQUIDADO"
            LET ls_id = 1
        END IF
    END IF

    RETURN ls_id, lc_error

END FUNCTION 
