#################################################################################
#Proyecto          => SISTEMA DE AFORES ( SAFRE )                               #
#Propietario       => E.F.P.                                                    #
#Programa RETC714  => REALIZA EL REVERSO DE LA LIQUIDACION DE TRANSFERENCIAS    #
#Fecha creacion    => 25 DE FEBRERO DE 2010                                     #
#Desarrado por     => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   => 4 DE ABRIL DE 2011                                        #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                  => Se agrega el revierso a las tablas de las estadisticas de #
#                     CONSAR (Req. EFPS-152)                                    #
#Sistema           => RET                                                       #
#################################################################################
DATABASE safre_af

GLOBALS

    DEFINE gr_bitacora RECORD LIKE ret_bitacora_rev.*

    DEFINE gar_tablas_rev ARRAY[7] OF RECORD
        accion          LIKE ret_tablas_rev.accion       ,
        tabla           LIKE ret_tablas_rev.tabla        ,
        num_regs        LIKE ret_tablas_rev.num_registros
    END RECORD

    DEFINE gr_edo RECORD
        enviado         LIKE ret_estado.estado_solicitud ,
        liquidado       LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE gr_datos RECORD
        folio           LIKE ret_transf_rx.folio        ,
        tipo_retiro     LIKE ret_transf_rx.tipo_retiro
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

    CALL STARTLOG("RETC714.log")
    CALL init()

    CALL f_captura_datos() RETURNING gs_procesa, gr_datos.*

    IF gs_procesa THEN
        CALL f_reverso(gr_datos.*)
        CALL f_act_bitacora(gr_datos.*)
        PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR CHAR enter
    END IF

    CLOSE WINDOW RETC7141

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    DEFINE
        lc_prepare      CHAR(300)

    LET HOY = TODAY

    ----- DATOS PARA LA BITACORA DE REVERSOS -----
    SELECT cod_tramite,
           USER
    INTO   gr_bitacora.cod_tramite,
           gr_bitacora.usuario
    FROM   tab_tipo_tramite
    WHERE  descripcion = "TRANSFERENCIA"

    LET gr_bitacora.programa    = "RETC714"
    LET gr_bitacora.fecha_ini   = HOY
    LET gr_bitacora.hora_ini    = CURRENT HOUR TO SECOND

    ----- ESTADOS DE SOLICITUD -----
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

    OPEN WINDOW RETC7141 AT 4,4 WITH FORM "RETC7141" ATTRIBUTE(BORDER)
    DISPLAY " <Ctrl-C> Salir                                                                " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC714   REVERSO DE LIQUIDACION - RETIRO POR TRANSFERENCIAS                  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    INPUT BY NAME lr_captura.* WITHOUT DEFAULTS

        AFTER FIELD folio
            IF lr_captura.folio IS NULL THEN
                ERROR "EL FOLIO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                NEXT FIELD folio
            END IF

        AFTER FIELD tipo_retiro
            IF lr_captura.tipo_retiro IS NULL THEN
                ERROR "EL TIPO DE RETIRO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                NEXT FIELD tipo_retiro
            ELSE
                CALL f_verifica_estado(lr_captura.*) RETURNING ls_estado, lc_cad_msg
                IF ls_estado <> 0 THEN
                    ERROR lc_cad_msg ATTRIBUTE(NORMAL)
                    NEXT FIELD folio
                END IF
            END IF

        ON KEY (ESC)
            IF lr_captura.folio IS NULL THEN
                ERROR "EL FOLIO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                NEXT FIELD folio
            END IF

            IF lr_captura.tipo_retiro IS NULL THEN
                ERROR "EL TIPO DE RETIRO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                NEXT FIELD tipo_retiro
            ELSE
                CALL f_verifica_estado(lr_captura.*) RETURNING ls_estado, lc_cad_msg
                IF ls_estado <> 0 THEN
                    ERROR lc_cad_msg ATTRIBUTE(NORMAL)
                    NEXT FIELD folio
                END IF
            END IF

            WHILE TRUE
                PROMPT "¿ EJECUTAR REVERSO DE LIQUIDACION ? (S/N) : " FOR CHAR enter
                IF enter MATCHES "[sSnN]" THEN
                    IF enter MATCHES "[sS]" THEN
                        LET ls_procesa = 1
                        EXIT INPUT
                    ELSE
                        PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
                        LET ls_procesa = 0
                        EXIT INPUT
                    END IF
                END IF
            END WHILE

        ON KEY (INTERRUPT, CONTROL-C)
            PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
            LET ls_procesa = 0
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
        ldt_fecha_envio         ,
        ldt_fecha_liquida       DATE

    DEFINE
        ls_movimiento           SMALLINT

    DEFINE
        li_cont                 INTEGER

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
    WHERE  folio            = pr_datos.folio
    AND    tipo_movimiento  = ls_movimiento

    DISPLAY "Fecha de Liquidacion :        ", ldt_fecha_liquida AT 8,16

    -----------------------------------------------------------------------------
    DECLARE cur_des CURSOR FOR
        SELECT  A.nss         ,
                A.marca_cod   ,
                A.correlativo ,
                A.fecha_ini
        FROM    cta_his_marca A ,
                ret_transf_rx B
        WHERE   A.nss               = B.nss
        AND     A.correlativo       = B.consecutivo
        AND     A.marca_cod         = ls_movimiento
        AND     B.folio             = pr_datos.folio
        AND     B.tipo_retiro       = pr_datos.tipo_retiro
        AND     B.estado_solicitud  = gr_edo.liquidado
        AND     A.fecha_fin IS NOT NULL

    FOREACH cur_des INTO lr_desmarca.*

        EXECUTE eje_rev_desmarca USING lr_desmarca.*
        
        IF SQLCA.SQLCODE = 0 THEN
            LET li_cont = li_cont + 1
        END IF
    
    END FOREACH

    DISPLAY "MARCAS REVERSADAS POR FUNCION           : ", li_cont
            USING "<<<,<<&" AT 11,10

    LET gar_tablas_rev[1].tabla     = "cta_act_marca"
    LET gar_tablas_rev[1].accion    = "REVERSO POR SPL"
    LET gar_tablas_rev[1].num_regs  = li_cont

    -----------------------------------------------------------------------------
    DELETE
    FROM  dis_cuenta
    WHERE folio             = pr_datos.folio
    AND   tipo_movimiento   = ls_movimiento

    LET li_cont = SQLCA.SQLERRD[3]

    DISPLAY "R. ELIMINADOS EN dis_cuenta             : ", li_cont
            USING "<<<,<<&" AT 12,10

    LET gar_tablas_rev[2].tabla     = "dis_cuenta"
    LET gar_tablas_rev[2].accion    = "BORRADA"
    LET gar_tablas_rev[2].num_regs  = li_cont
    -----------------------------------------------------------------------------
    UPDATE ret_transf_rx
    SET    estado_solicitud   = gr_edo.enviado
    WHERE  folio              = pr_datos.folio
    AND    tipo_retiro        = pr_datos.tipo_retiro
    AND    estado_solicitud   = gr_edo.liquidado

    LET li_cont = SQLCA.sqlerrd[3]

    DISPLAY "R. ACTUALIZADOS EN ret_transf_rx        : ", li_cont
            USING "<<<,<<&" AT 13,10

    LET gar_tablas_rev[3].tabla     = "ret_transf_rx"
    LET gar_tablas_rev[3].accion    = "ACTUALIZA"
    LET gar_tablas_rev[3].num_regs  = li_cont

    -----------------------------------------------------------------------------
    IF pr_datos.tipo_retiro = "A" THEN

        SELECT UNIQUE(fecha_envio)
        INTO   ldt_fecha_envio
        FROM   ret_ctr_envio_lote
        WHERE  folio        = pr_datos.folio
        AND    tipo_retiro <> pr_datos.tipo_retiro

        UPDATE ret_ctr_envio_lote
        SET    estado           = gr_edo.enviado        ,
               usuario_reverso  = gr_bitacora.usuario   ,
               fecha_reverso    = HOY                   ,
               fecha_envio      = ldt_fecha_envio       ,
               usuario_envio    = NULL
        WHERE  tipo_retiro   = pr_datos.tipo_retiro
        AND    folio         = pr_datos.folio
        
        LET li_cont = SQLCA.sqlerrd[3]
        
        DISPLAY "R. ACTUALIZADOS EN ret_ctr_envio_lote   : ", li_cont
                USING "<<<,<<&" AT 14,10
        
        LET gar_tablas_rev[4].tabla     = "ret_ctr_envio_lote"
        LET gar_tablas_rev[4].accion    = "ACTUALIZA"
        LET gar_tablas_rev[4].num_regs  = li_cont
    ELSE
        LET gar_tablas_rev[4].num_regs  = 0
    END IF
    -----------------------------------------------------------------------------
    DELETE
    FROM  ret_consar_det_liquida
    WHERE nss IN ( SELECT nss                 
                   FROM   ret_consar            
                   WHERE  folio_liquida  = pr_datos.folio
                   AND    tipo_retiro    = pr_datos.tipo_retiro
                 )
    
    LET li_cont = SQLCA.SQLERRD[3]

    DISPLAY "R. ELIMINADOS EN ret_consar_det_liquida : ", li_cont
            USING "<<<,<<&" AT 15,10

    LET gar_tablas_rev[5].tabla     = "ret_consar_det_liquida"
    LET gar_tablas_rev[5].accion    = "BORRADA"
    LET gar_tablas_rev[5].num_regs  = li_cont
    
    -----------------------------------------------------------------------------
    DELETE
    FROM  ret_consar_liquida
    WHERE folio_liquida  = pr_datos.folio
    AND   tipo_retiro    = pr_datos.tipo_retiro
    
    LET li_cont = SQLCA.SQLERRD[3]

    DISPLAY "R. ELIMINADOS EN ret_consar_liquida     : ", li_cont
            USING "<<<,<<&" AT 16,10

    LET gar_tablas_rev[6].tabla     = "ret_consar_liquida"
    LET gar_tablas_rev[6].accion    = "BORRADA"
    LET gar_tablas_rev[6].num_regs  = li_cont
    
    -----------------------------------------------------------------------------
    DELETE
    FROM  ret_consar
    WHERE folio_liquida  = pr_datos.folio
    AND   tipo_retiro    = pr_datos.tipo_retiro
    
    LET li_cont = SQLCA.SQLERRD[3]

    DISPLAY "R. ELIMINADOS EN ret_consar             : ", li_cont
            USING "<<<,<<&" AT 17,10

    LET gar_tablas_rev[7].tabla     = "ret_consar"
    LET gar_tablas_rev[7].accion    = "BORRADA"
    LET gar_tablas_rev[7].num_regs  = li_cont
    
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

    DEFINE li_ind_rev LIKE ret_bitacora_rev.id_rev

    DEFINE
        i       SMALLINT

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

    FOR i = 1 TO 7
        IF gar_tablas_rev[i].num_regs > 0 THEN
            INSERT INTO ret_tablas_rev
            VALUES (li_ind_rev          ,
                    gr_bitacora.folio   ,
                    gar_tablas_rev[i].*
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

    LET ls_id = 0
    INITIALIZE ls_estado TO NULL

    SELECT MAX(estado_solicitud)
    INTO   ls_estado
    FROM   ret_transf_rx
    WHERE  folio            = lr_datos.folio
    AND    tipo_retiro      = lr_datos.tipo_retiro

    IF (STATUS = NOTFOUND) OR (ls_estado IS NULL) THEN
        LET lc_error = "    FOLIO INEXISTENTE"
        LET ls_id = 1
    ELSE
        IF ls_estado <> gr_edo.liquidado THEN
            LET lc_error = "    EL ESTADO DEBE SER 8 - LIQUIDADO "
            LET ls_id = 1
        END IF
    END IF

    RETURN ls_id, lc_error

END FUNCTION
