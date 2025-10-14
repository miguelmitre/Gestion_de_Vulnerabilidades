################################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                              #
#Owner             => E.F.P.                                                   #
#Programa RETC8131 => REVERZAR LA LIQUIDACION DE TRANSFERENCIA TIPO RETIRO "A" #
#Fecha creacion    => 14 DE FEBRERO DEL 2005                                   #
#By                => IJR                                                      #
#Fecha actualiza   => 13 DE MARZO DE 2008                                      #
#By                => JAVIER GONZALEZ JERONIMO                                 #
# Actualizacion     => JAVIER GONZALEZ JERONIMO                                #
# Fecha             => 21 DE ENERO DE 2009                                     #
#                      Modificaciones para registrar el reverso en las tablas  #
#                      de bitacora                                             #
#Sistema           => RET                                                      #
################################################################################

DATABASE safre_af

GLOBALS

    DEFINE gr_bitacora RECORD LIKE ret_bitacora_rev.*

    DEFINE gar_tablas_rev ARRAY[5] OF RECORD
        accion      LIKE ret_tablas_rev.accion       ,
        tabla       LIKE ret_tablas_rev.tabla        ,
        num_regs    LIKE ret_tablas_rev.num_registros
    END RECORD


    DEFINE reg_2 RECORD #glo #..reg_2
        enviado               LIKE ret_estado.estado_solicitud ,
        liquidado             LIKE ret_estado.estado_solicitud
    END RECORD
 
    DEFINE 
        v_tipo_mov     ,
        v_folio        INTEGER

    DEFINE 
        v_tipo_retiro  , 
        g_tecla        CHAR(1)

    DEFINE 
        HOY            DATE

END GLOBALS


MAIN
   
    DEFER INTERRUPT
    OPTIONS 
        PROMPT LINE LAST - 1

    CALL init() #i
    
    OPEN WINDOW f81311 AT 2,2 WITH FORM "RETC81311" ATTRIBUTE(BORDER) 
    DISPLAY "   < Ctrl-C > Salir                                        RETIRO 'A'       " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC8131    REVERSO DE LIQUIDACION DE TRANSFERENCIAS IV-RT                 " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)
    
    --CAPTURA DEL FOLIO A REVERSAR
    INPUT v_folio WITHOUT DEFAULTS FROM folio 
        
        AFTER FIELD folio
            IF v_folio IS NULL then
                ERROR "EL FOLIO NO PUEDE SER NULO"
                NEXT FIELD folio
            END IF
             
            --VERIFICA QUE EXISTAN EL FOLIO
            SELECT "OK"
            FROM   ret_cza_lote
            WHERE  folio = v_folio
            GROUP  BY 1
            
            IF SQLCA.SQLCODE = NOTFOUND THEN
                ERROR " FOLIO INEXISTENTE "
                NEXT FIELD folio
            END IF
    END INPUT

    IF INT_FLAG THEN
        ERROR "OPERACION CANCELADA"
        SLEEP 2
        EXIT PROGRAM
    END IF

    WHILE TRUE
        PROMPT "ESTA SEGURO(S/N): " FOR CHAR g_tecla
        IF g_tecla MATCHES "[sSnN]" then
            IF g_tecla MATCHES "[sS]" then
                CALL valida_reversa_liquidacion(v_folio)
            END IF
            EXIT WHILE
        END IF
    END WHILE
     
    PROMPT "PROCESO TERMINADO PRESIONE <ENTER> PARA SALIR:" FOR CHAR g_tecla ATTRIBUTE(NORMAL)
        
END MAIN 

FUNCTION init()
#i-------------
    LET HOY  = TODAY
    
    -- Se almacenan los datos para la bitacora de reversos
    SELECT cod_tramite,
           USER
    INTO   gr_bitacora.cod_tramite,
           gr_bitacora.usuario
    FROM   tab_tipo_tramite
    WHERE  descripcion = "TRANSFERENCIA"

    LET gr_bitacora.programa      = "RETC8131"
    LET gr_bitacora.desc_tramite  = "LIQUIDACION RETIRO A"
    LET gr_bitacora.fecha_ini     = HOY
    LET gr_bitacora.hora_ini      = CURRENT HOUR TO SECOND

    ---------------------------------------------------------

    LET v_tipo_retiro = "A"

    SELECT movimiento
    INTO   v_tipo_mov
    FROM   tab_retiro
    WHERE  tipo_retiro = v_tipo_retiro    

    SELECT MAX(folio)
    INTO   v_folio
    FROM   ret_transf_rx
    WHERE  tipo_retiro = v_tipo_retiro

    SELECT A.estado_solicitud
    INTO   reg_2.liquidado
    FROM   ret_estado A
    WHERE  A.descripcion = "LIQUIDADO"
 
    SELECT A.estado_solicitud
    INTO   reg_2.enviado
    FROM   ret_estado A
    WHERE  A.descripcion = "ENVIADO"

END FUNCTION


#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION valida_reversa_liquidacion(p_folio)
    DEFINE 
        p_folio    INTEGER 

    --Verifica que existan registros Liquidados para el folio
    SELECT "OK"
    FROM   ret_transf_rx
    WHERE  folio            = p_folio
    AND    tipo_retiro      = v_tipo_retiro
    AND    estado_solicitud = reg_2.liquidado
    GROUP BY 1
    
    IF SQLCA.SQLCODE = NOTFOUND THEN 
        ERROR " NO HAY REGISTROS LIQUIDADOS PARA EL FOLIO " ATTRIBUTE(NORMAL)
    ELSE
        CALL reversa_liquidacion(p_folio)
        CALL actualiza_bitacora(p_folio)
    END IF
   
END FUNCTION

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION reversa_liquidacion(p_folio)

    DEFINE 
        p_folio           INTEGER

    DEFINE 
        lr_ret_transf_rx  RECORD LIKE ret_transf_rx.*
    
    DEFINE 
        v_fecha_ini       DATE 
   
    DEFINE 
        v_cont_des        INTEGER

    DEFINE 
        v_marca_cod       SMALLINT
   
    -----------------------------------------------------------------------------
    DELETE 
    FROM   dis_cuenta
    WHERE  folio            = p_folio
    AND    tipo_movimiento  = v_tipo_mov
    
    LET v_cont_des = SQLCA.sqlerrd[3]
    
    DISPLAY "R. ELIMINADOS EN dis_cuenta             : ", v_cont_des
            USING "<<<,<<&" AT 10,10
    
    LET gar_tablas_rev[1].tabla     = "dis_cuenta"
    LET gar_tablas_rev[1].accion    = "BORRADA"
    LET gar_tablas_rev[1].num_regs  = v_cont_des
    -----------------------------------------------------------------------------

    UPDATE ret_ctr_envio_lote
    SET    estado        = reg_2.enviado   ,
           usuario_envio = NULL
    WHERE  tipo_retiro   = v_tipo_retiro
    AND    folio         = p_folio

    LET v_cont_des = SQLCA.sqlerrd[3]

    DISPLAY "R. ACTUALIZADOS EN ret_ctr_envio_lote   : ", v_cont_des
            USING "<<<,<<&" AT 11,10    

    LET gar_tablas_rev[2].tabla     = "ret_ctr_envio_lote"
    LET gar_tablas_rev[2].accion    = "ACTUALIZA"
    LET gar_tablas_rev[2].num_regs  = v_cont_des
    
    -----------------------------------------------------------------------------
    -- REVERSA DESMARCA DE REGISTROS LIQUIDADOS
    -----------------------------------------------------------------------------
    LET v_cont_des  = 0
    LET v_marca_cod = v_tipo_mov
   
    DECLARE cur_rev_des CURSOR FOR
      SELECT * 
      FROM   ret_transf_rx
      WHERE  folio            = p_folio
      AND    estado_solicitud = reg_2.liquidado
      AND    tipo_retiro      = v_tipo_retiro
      
    FOREACH cur_rev_des INTO lr_ret_transf_rx.*
   
        --SELECCIONA LE FECHA DE INICIO DE LA MARCA
        SELECT fecha_ini
        INTO   v_fecha_ini
        FROM   cta_his_marca
        WHERE  nss         = lr_ret_transf_rx.nss
        AND    correlativo = lr_ret_transf_rx.consecutivo
        AND    marca_cod   = v_tipo_mov
        
        IF SQLCA.SQLCODE = NOTFOUND THEN 
            ERROR "NSS: ",lr_ret_transf_rx.nss," NO SE ENCUENTRA MARCADO"
            PROMPT "PRESIONE <ENTER> PARA CONTINUAR" FOR CHAR g_tecla
        ELSE
            PREPARE exe1 FROM "execute procedure reversa_desmarca(?,?,?,?)"
            EXECUTE exe1 USING lr_ret_transf_rx.nss,
                               v_marca_cod,
                               lr_ret_transf_rx.consecutivo,
                               v_fecha_ini
            IF SQLCA.SQLCODE = 0 THEN
                LET v_cont_des = v_cont_des + 1
            END IF
        END IF
    
    END FOREACH
    DISPLAY "MARCAS REVERSADAS POR FUNCION           : ", v_cont_des
            USING "<<<,<<&" AT 12,10

    LET gar_tablas_rev[3].tabla     = "cta_act_marca"
    LET gar_tablas_rev[3].accion    = "REVERSO POR SPL"
    LET gar_tablas_rev[3].num_regs  = v_cont_des

    -----------------------------------------------------------------------------
   
    UPDATE ret_transf_rx
    SET    estado_solicitud = reg_2.enviado
    WHERE  folio            = p_folio
    AND    estado_solicitud = reg_2.liquidado
    AND    tipo_retiro      = v_tipo_retiro

    LET v_cont_des = SQLCA.sqlerrd[3]

    DISPLAY "R. ACTUALIZADOS EN ret_transf_rx        : ", v_cont_des
             USING "<<<,<<&" AT 13,10
    
    LET gar_tablas_rev[4].tabla     = "ret_transf_rx"
    LET gar_tablas_rev[4].accion    = "ACTUALIZA"
    LET gar_tablas_rev[4].num_regs  = v_cont_des

    -----------------------------------------------------------------------------

END FUNCTION

#------------------------------------------------------------------------------#
#Inserta en las tablas de bitacora y de tablas del reverso realizado           #
#------------------------------------------------------------------------------#
FUNCTION actualiza_bitacora(p_folio)

    DEFINE p_folio LIKE ret_bitacora_rev.folio
    DEFINE ind_rev LIKE ret_bitacora_rev.id_rev
    
    DEFINE 
        i       SMALLINT

    LET gr_bitacora.folio       = p_folio
    LET gr_bitacora.fecha_fin   = TODAY
    LET gr_bitacora.hora_fin    = CURRENT HOUR TO SECOND

    INSERT INTO ret_bitacora_rev
    VALUES (gr_bitacora.*)

    -- Rescatamos el serial que se le asigno al movimiento
    SELECT MAX(id_rev)
    INTO   ind_rev
    FROM   ret_bitacora_rev
    WHERE  folio = gr_bitacora.folio

    FOR i = 1 TO 4 
        INSERT INTO ret_tablas_rev
        VALUES (ind_rev,
                gr_bitacora.folio,
                gar_tablas_rev[i].*
               )
    END FOR

END FUNCTION



