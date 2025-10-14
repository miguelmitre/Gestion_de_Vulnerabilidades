#################################################################################
#Proyecto          => SISTEMA DE AFORES ( SAFRE )                               #
#Propietario       => E.F.P.                                                    #
#Programa RETC711  => REALIZA EL REVERSO DE LA PROVISION Y NOTIFICACION DE      #
#                     DISPOSICIONES (OP.05)                                     #
#Fecha creacion    => 24 DE FEBRERO DE 2010                                     #
#Desarrado por     => JAVIER GONZALEZ JERONIMO                                  #
#Actualizacion     =>                                                           #
#Fecha             =>                                                           #
#Sistema           => RET                                                       #
#################################################################################
DATABASE safre_af

GLOBALS

    DEFINE gr_bitacora RECORD LIKE ret_bitacora_rev.*

    DEFINE gar_tablas_rev ARRAY[5] OF RECORD
        accion          LIKE ret_tablas_rev.accion       ,
        tabla           LIKE ret_tablas_rev.tabla        ,
        num_regs        LIKE ret_tablas_rev.num_registros
    END RECORD

    DEFINE gr_edo RECORD
        confirmado            LIKE ret_estado.estado_solicitud ,
        procesado             LIKE ret_estado.estado_solicitud ,
        enviado               LIKE ret_estado.estado_solicitud ,
        liquidado             LIKE ret_estado.estado_solicitud ,
        modificado            LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE gr_datos RECORD 
        folio           LIKE ret_solicitud_tx.folio ,
        tipo_retiro     LIKE ret_solicitud_tx.tipo_retiro
    END RECORD

    DEFINE
        gs_mov_cancela  ,
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

    CALL STARTLOG("RETC711.log")
    CALL init()
    
    CALL f_captura_datos() RETURNING gs_procesa, gr_datos.*
    
    IF gs_procesa THEN 
        CALL f_reverso(gr_datos.*)
        CALL f_act_bitacora(gr_datos.*)
        PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR CHAR enter
    END IF 
    
    CLOSE WINDOW RETC7111

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    LET HOY = TODAY

    ----- DATOS PARA LA BITACORA DE REVERSOS ----- 
    SELECT cod_tramite,
           USER
    INTO   gr_bitacora.cod_tramite,
           gr_bitacora.usuario
    FROM   tab_tipo_tramite
    WHERE  descripcion = "DISPOSICION"

    LET gr_bitacora.programa    = "RETC711"
    LET gr_bitacora.fecha_ini   = HOY
    LET gr_bitacora.hora_ini    = CURRENT HOUR TO SECOND

    SELECT codigo
    INTO   gs_mov_cancela
    FROM   tab_movimiento
    WHERE  descripcion  = "CANCELACION DE INTERESES DE VIVIENDA"

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.confirmado
    FROM   ret_estado A
    WHERE  A.descripcion = "CONFIRMADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.procesado
    FROM   ret_estado A
    WHERE  A.descripcion = "PROCESADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.enviado
    FROM   ret_estado A
    WHERE  A.descripcion = "ENVIADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.liquidado
    FROM   ret_estado A
    WHERE  A.descripcion = "LIQUIDADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.modificado
    FROM   ret_estado A
    WHERE  A.descripcion = "MODIFICADO"

END FUNCTION

#---------------------------------------------------------------------------#
# f_captura_datos : Captura el folio y el tipo de retiro del que se         #
#                   hara el reverso de la provision                         #
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

    DEFINE
        li_tot_reg          INTEGER

    DEFINE
        ldt_fec_genera      DATE

    LET li_tot_reg = 0

    OPEN WINDOW RETC7111 AT 4,4 WITH FORM "RETC7111" ATTRIBUTE(BORDER)
    DISPLAY " <Ctrl-C> Salir                                                                " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC711 REVERSO NOTIFICA Y PROVISIONA DISPOSICION DE RECURSOS                 " AT 3,1 ATTRIBUTE(REVERSE)

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
                CALL f_verifica_estado(lr_captura.*) RETURNING ls_estado, lc_cad_msg, li_tot_reg
                IF ls_estado <> 0 THEN
                    ERROR lc_cad_msg ATTRIBUTE(NORMAL)
                    NEXT FIELD folio
                ELSE
                    SELECT fecha_genera
                    INTO   ldt_fec_genera
                    FROM   ret_ctr_envio_lote
                    WHERE  folio        = lr_captura.folio
                    AND    tipo_retiro  = lr_captura.tipo_retiro
                    
                    DISPLAY "Total de registros del folio   : ", li_tot_reg AT 07,06
                    DISPLAY "Fecha de generacion de folio   : ", ldt_fec_genera USING "DD-MM-YYYY" AT 08,06
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
                CALL f_verifica_estado(lr_captura.*) RETURNING ls_estado, lc_cad_msg, li_tot_reg
                IF ls_estado <> 0 THEN
                    ERROR lc_cad_msg ATTRIBUTE(NORMAL)
                    NEXT FIELD folio
                ELSE
                    SELECT fecha_genera
                    INTO   ldt_fec_genera
                    FROM   ret_ctr_envio_lote
                    WHERE  folio        = lr_captura.folio
                    AND    tipo_retiro  = lr_captura.tipo_retiro
                    
                    DISPLAY "Total de registros del folio   : ", li_tot_reg AT 07,06
                    DISPLAY "Fecha de generacion de folio   : ", ldt_fec_genera USING "DD-MM-YYYY" AT 08,06
                END IF
            END IF
            
            WHILE TRUE
                PROMPT "¿ EJECUTAR REVERSO DE NOTIFICACION ? (S/N) : " FOR CHAR enter
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

    DEFINE lr_rev_E RECORD
        nss                 LIKE ret_solicitud_tx.nss               ,
        consecutivo         LIKE ret_solicitud_tx.consecutivo       ,
        tipo_retiro         LIKE ret_solicitud_tx.tipo_retiro       ,
        regimen             LIKE ret_solicitud_tx.regimen           ,
        tipo_seguro         LIKE ret_solicitud_tx.tipo_seguro       ,
        tipo_pension        LIKE ret_solicitud_tx.tipo_pension      ,
        tipo_prestacion     LIKE ret_solicitud_tx.tipo_prestacion   ,
        grupo               LIKE ret_solicitud_tx.grupo           
    END RECORD 
 
    DEFINE
        ls_grupo            ,
        ls_estado_sol       SMALLINT

    DEFINE 
        li_cont             INTEGER

    -- ----------------------------------------------------------------------------- 
 
    LET li_cont  = 0
    DISPLAY " PROCESANDO INFORMACION " AT 19,2 ATTRIBUTE(REVERSE)
    
    -----------------------------------------------------------------------------
    DELETE
    FROM   dis_provision 
    WHERE  folio            = pr_datos.folio
    AND    id_aportante     = "RETIRO"

    LET li_cont = SQLCA.SQLERRD[3]

    DISPLAY "R. ELIMINADOS EN dis_provision         : ", li_cont
            USING "<<<,<<&" AT 10,10

    LET gar_tablas_rev[1].tabla     = "dis_provision"
    LET gar_tablas_rev[1].accion    = "BORRADA"
    LET gar_tablas_rev[1].num_regs  = li_cont

    -----------------------------------------------------------------------------
    
    DELETE 
    FROM  ret_ctr_envio_lote
    WHERE folio       = pr_datos.folio
    AND   tipo_retiro = pr_datos.tipo_retiro

    LET li_cont = SQLCA.SQLERRD[3]

    DISPLAY "R. ELIMINADOS EN ret_ctr_envio_lote    : ", li_cont
            USING "<<<,<<&" AT 11,10

    LET gar_tablas_rev[2].tabla     = "ret_ctr_envio_lote"
    LET gar_tablas_rev[2].accion    = "BORRADA"
    LET gar_tablas_rev[2].num_regs  = li_cont

    -----------------------------------------------------------------------------
    
    DELETE
    FROM   ret_monto_siefore
    WHERE  folio       = pr_datos.folio
    AND    tipo_retiro = pr_datos.tipo_retiro

    LET li_cont = SQLCA.SQLERRD[3]

    DISPLAY "R. ELIMINADOS EN ret_monto_siefore     : ", li_cont
            USING "<<<,<<&" AT 12,10

    LET gar_tablas_rev[3].tabla     = "ret_monto_siefore"
    LET gar_tablas_rev[3].accion    = "BORRADA"
    LET gar_tablas_rev[3].num_regs  = li_cont

    -----------------------------------------------------------------------------
    
    DELETE
    FROM   ret_monto_viv
    WHERE  folio       = pr_datos.folio
    AND    tipo_retiro = pr_datos.tipo_retiro

    LET li_cont = SQLCA.SQLERRD[3]

    DISPLAY "R. ELIMINADOS EN ret_monto_viv         : ", li_cont
            USING "<<<,<<&" AT 13,10

    LET gar_tablas_rev[4].tabla     = "ret_monto_viv"
    LET gar_tablas_rev[4].accion    = "BORRADA"
    LET gar_tablas_rev[4].num_regs  = li_cont

    -----------------------------------------------------------------------------

    IF pr_datos.tipo_retiro <> "E" THEN
        UPDATE ret_solicitud_tx
        SET    acciones_ret97       = 0,
               acciones_cuota_soc   = 0,
               acciones_ret92       = 0,
               acciones_cv          = 0,
               saldo_viv97          = 0,
               saldo_viv92          = 0,
               saldo_viv72          = 0,
               folio                = 0,
               fecha_envio          = "01010001",
               fecha_valor_viv      = "01010001",
               estado_solicitud     = gr_edo.confirmado
        WHERE  folio                = pr_datos.folio
        AND    tipo_retiro          = pr_datos.tipo_retiro

        LET li_cont = SQLCA.sqlerrd[3]    
    ELSE 
        LET li_cont = 0
        INITIALIZE lr_rev_E.* TO NULL
        
        DECLARE cur_tipoE CURSOR FOR
            SELECT nss                  ,
                   consecutivo          ,
                   tipo_retiro          ,
                   regimen              ,
                   tipo_seguro          ,
                   tipo_pension         ,
                   tipo_prestacion      ,
                   grupo
            FROM   ret_solicitud_tx
            WHERE  folio = pr_datos.folio  
            ORDER BY 1

        FOREACH cur_tipoE INTO lr_rev_E.*

            SELECT grupo
            INTO   ls_grupo
            FROM   ret_matriz_derecho
            WHERE  tipo_retiro     = lr_rev_E.tipo_retiro     
            AND    regimen         = lr_rev_E.regimen         
            AND    tipo_seguro     = lr_rev_E.tipo_seguro     
            AND    tipo_pension    = lr_rev_E.tipo_pension    
            AND    tipo_prestacion = lr_rev_E.tipo_prestacion 
            
            IF lr_rev_E.grupo = ls_grupo THEN
                LET ls_estado_sol   = gr_edo.confirmado
            ELSE
                LET ls_estado_sol   = gr_edo.modificado
            END IF 

            UPDATE ret_solicitud_tx
            SET    acciones_ret97       = 0             ,
                   acciones_cuota_soc   = 0             ,
                   acciones_ret92       = 0             ,
                   acciones_cv          = 0             ,
                   saldo_viv97          = 0             ,
                   saldo_viv92          = 0             ,
                   saldo_viv72          = 0             ,
                   folio                = 0             ,
                   fecha_envio          = "01010001"    ,
                   fecha_valor_viv      = "01010001"    ,
                   estado_solicitud     = ls_estado_sol
            WHERE  folio                = pr_datos.folio
            AND    tipo_retiro          = pr_datos.tipo_retiro
            AND    nss                  = lr_rev_E.nss
            AND    consecutivo          = lr_rev_E.consecutivo

            LET li_cont = li_cont + 1
        
        END FOREACH 
    END IF 
 
    DISPLAY "R. ACTUALIZADOS EN ret_solicitud_tx    : ", li_cont
             USING "<<<,<<&" AT 14,10
    
    LET gar_tablas_rev[5].tabla     = "ret_solicitud_tx"
    LET gar_tablas_rev[5].accion    = "ACTUALIZA"
    LET gar_tablas_rev[5].num_regs  = li_cont
    
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
    LET gr_bitacora.desc_tramite  = "GENERA OP.05 RETIRO ", pr_datos.tipo_retiro

    INSERT INTO ret_bitacora_rev
    VALUES (gr_bitacora.*)

    -- Rescatamos el serial que se le asigno al movimiento
    SELECT MAX(id_rev)
    INTO   li_ind_rev
    FROM   ret_bitacora_rev
    WHERE  folio = gr_bitacora.folio

    FOR i = 1 TO 5
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
        lc_error            CHAR(100)

    DEFINE
        li_num_regs         INTEGER

    DEFINE 
        ls_id               SMALLINT

    LET ls_id = 0

    SELECT MAX(estado_solicitud)
    INTO   ls_estado
    FROM   ret_solicitud_tx    
    WHERE  folio            = lr_datos.folio
    AND    tipo_retiro      = lr_datos.tipo_retiro
    
    IF (STATUS = NOTFOUND) OR (ls_estado IS NULL) THEN
        LET lc_error = "    FOLIO INEXISTENTE"
        LET ls_id = 1
    ELSE
        CASE ls_estado
            WHEN gr_edo.procesado
                LET ls_id = 0
                
            WHEN gr_edo.enviado
                LET lc_error = "    ERROR...PRIMERO SE DEBE REVERSAR EL LOTE"
                LET ls_id    = 1
                
            WHEN gr_edo.liquidado
                LET lc_error =    "    ERROR...FOLIO YA LIQUIDADO"
                LET ls_id    = 1 

            OTHERWISE
                LET lc_error = "    ERROR...EL LOTE DEBE ESTAR EN ESTADO 2 - PROCESADO"
                LET ls_id = 1
        END CASE
    END IF

    IF ls_id = 0 THEN 
        SELECT NVL(COUNT(*),0)
        INTO   li_num_regs
        FROM   ret_solicitud_tx    
        WHERE  folio            = lr_datos.folio
        AND    tipo_retiro      = lr_datos.tipo_retiro
    END IF 
   
    RETURN ls_id, lc_error, li_num_regs

END FUNCTION 
