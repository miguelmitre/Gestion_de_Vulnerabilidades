#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETC8101 => REVERSO DE RECEPCION DE ARCHIVOS ENVIADOS POR PROCESAR    #
#Fecha creacion    => 06 DE FEBRERO DEL 2008                                    #
#By                => FRANCO ULLOA VIDELA                                       #
#Actualizacion     => FRANCO ESTEBAN ULLOA VIDELA                               #
#Fecha             => 09 DE MARZO DEL 2008                                      #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#Fecha             => 22 DE FEBRERO DE 2010                                     #
#                     Modificaciones para registrar el reverso en las tablas    #
#                     de bitacora                                               #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   => 11 DE MAYO DE 2011                                        #
#                  => Se realizan las modificaciones al codigo para incluir     #
#                     el llamado de las funciones para realizar el marcaje de   #
#                     trabajador pensionado (REQ. EFPS-157 )                    #
#Sistema           => RET                                                       #
#################################################################################
DATABASE safre_af
GLOBALS

    DEFINE gr_bitacora RECORD LIKE ret_bitacora_rev.*

    DEFINE gar_tablas_rev ARRAY[10] OF RECORD
        accion      LIKE ret_tablas_rev.accion       ,
        tabla       LIKE ret_tablas_rev.tabla        ,
        num_regs    LIKE ret_tablas_rev.num_registros
    END RECORD

    DEFINE gr_edo RECORD
        recibido              LIKE ret_estado.estado_solicitud 
    END RECORD


    DEFINE #glo DATE
        HOY                   DATE

    DEFINE
        enter                   CHAR(001),
        gc_cve                  CHAR(003)

    DEFINE #glo #integer
        gi_folio                INTEGER

    DEFINE
        gs_codigo_afore         ,
        gs_xxi                  ,
        gs_procesa              SMALLINT

    DEFINE reg_10 RECORD #glo #reg_10
        nss                   CHAR(11) ,
        consecutivo           DECIMAL(11,0)
    END RECORD

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I
        CALL STARTLOG("RETC8101.log")

    CALL init() #i
    CALL f_captura_datos() RETURNING gi_folio, gs_procesa, gc_cve

    IF gs_procesa THEN
        CALL f_genera_reverso(gi_folio, gc_cve)
    END IF
   
END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    DEFINE
        lc_prepare      CHAR(300)

    LET HOY  = TODAY
    
    ----- CODIGOS AFORES -----
    SELECT codigo_afore    ,
           USER
    INTO   gs_codigo_afore ,
           gr_bitacora.usuario
    FROM   tab_afore_local

    SELECT afore_cod
    INTO   gs_xxi
    FROM   tab_afore
    WHERE  afore_desc MATCHES "*XXI*"

    -- Se almacenan los datos para la bitacora de reversos
    LET gr_bitacora.programa      = "RETC8101"
    LET gr_bitacora.fecha_ini     = HOY
    LET gr_bitacora.hora_ini      = CURRENT HOUR TO SECOND

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.recibido
    FROM   ret_estado A
    WHERE  A.descripcion = "RECEPCIONADO"

    ----- REVERSO DE DESMARCA -----
    LET lc_prepare = "EXECUTE PROCEDURE reversa_desmarca (?,?,?,?) "
    PREPARE eje_rev_desmarca FROM lc_prepare

    LET lc_prepare = " "

    ----- DESMARCA CUENTA -----
    LET lc_prepare = "EXECUTE PROCEDURE desmarca_cuenta (?,?,?,?,?,?) "
    PREPARE eje_desmarca FROM lc_prepare

    LET lc_prepare = " "


END FUNCTION

#---------------------------------------------------------------------------#
# f_captura_datos : Captura el folio de carga de archivo con el que se      #
#                   hara el reverso de la carga                             #
#---------------------------------------------------------------------------#
FUNCTION f_captura_datos()

    DEFINE lr_lote RECORD LIKE ret_cza_lote.*

    DEFINE
        li_folio        INTEGER

    DEFINE
        ls_procesa      SMALLINT

    DEFINE
        lc_clave        CHAR(003)

    LET ls_procesa = 1

    OPEN WINDOW retc81011 AT 4,4 WITH FORM "RETC81011" ATTRIBUTE(BORDER)
    DISPLAY " < Ctrl-C > Salir                                                              " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC8101     REVERSO DE RECEPCION DE ARCHIVOS DE PROCESAR                     " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    INPUT BY NAME li_folio WITHOUT DEFAULTS

        AFTER FIELD li_folio
            IF li_folio IS NULL THEN
                ERROR "   CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
	            NEXT FIELD li_folio
	        ELSE
	            SELECT *
	            INTO   lr_lote.*
	            FROM   ret_cza_lote
	            WHERE  folio = li_folio

	            DISPLAY "Nombre del archivo   : ", lr_lote.nom_archivo AT 6,16
	            DISPLAY "Fecha de carga   : ", lr_lote.fecha_carga AT 7,20
	        END IF

        ON KEY (ESC)
            IF li_folio IS NULL THEN
                ERROR "   CAMPO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
	            NEXT FIELD li_folio
            ELSE
                CALL f_determina_tipo(li_folio) RETURNING lc_clave

                IF lc_clave = "xxx" THEN
                    ERROR "   EL FOLIO NO CORRESPONDE A LA CARGA DE ARCHIVO" ATTRIBUTE(NORMAL)
                    LET li_folio = 0
                    DISPLAY BY NAME li_folio
	                NEXT FIELD li_folio
                END IF
            END IF
	        EXIT INPUT

        ON KEY (INTERRUPT)
            PROMPT  "PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
            LET ls_procesa = 0
            EXIT INPUT
    END INPUT

    IF ls_procesa THEN
        WHILE TRUE
            PROMPT "¿ EJECUTAR REVERSO DE RECEPCION DE ARCHIVOS ? (S/N) : " FOR CHAR enter
            IF enter MATCHES "[sSnN]" THEN
                IF enter MATCHES "[sS]" THEN
                    LET ls_procesa = 1
                ELSE
                    PROMPT "PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
                    LET ls_procesa = 0
                END IF
                EXIT WHILE
            END IF
        END WHILE
    END IF

    RETURN li_folio, ls_procesa, lc_clave

END FUNCTION

#---------------------------------------------------------------------------#
# f_determina_tipo : Valida que el folio ingresado corresponda a alguno de  #
#                    los tipos de proceso que se cargan                     #
#---------------------------------------------------------------------------#
FUNCTION f_determina_tipo(li_folio)

    DEFINE
        li_folio        INTEGER

    DEFINE
        lc_clave        CHAR(003)

    -- Verifica si el folio es de transferencias
    SELECT "OK"
    FROM   ret_transf_rx
    WHERE  folio = li_folio
    GROUP BY 1

    IF STATUS <> NOTFOUND THEN
        LET lc_clave = "tra"
    ELSE
        -- Verifica si el folio es de historicos
        SELECT "OK"
        FROM   ret_sal_his_rx
        WHERE  folio = li_folio
        GROUP BY 1

        IF STATUS <> NOTFOUND THEN
            LET lc_clave = "his"
        ELSE
            SELECT "OK"
            FROM   ret_monto_siefore
            WHERE  folio            = li_folio
            AND    tipo_operacion   = 6
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
                LET lc_clave = "dis"
            ELSE
                LET lc_clave = "xxx"
            END IF -- Disposiciones
        END IF -- Historicos
    END IF -- Transferencias

    RETURN lc_clave

END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_reverso : Llena los datos particulares para cada proceso y       #
#                    ejecuta la funcion del reverso                         #
#---------------------------------------------------------------------------#
FUNCTION f_genera_reverso(pi_folio, pc_cve)

    DEFINE
        pi_folio            INTEGER

    DEFINE
        pc_cve              CHAR(03)

    DISPLAY " PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)

    CASE pc_cve
        WHEN "tra"
            LET gr_bitacora.desc_tramite  = "CARGA OP.02 TRANSFERENCIAS"

            SELECT cod_tramite
            INTO   gr_bitacora.cod_tramite
            FROM   tab_tipo_tramite
            WHERE  descripcion = "TRANSFERENCIA"

            CALL f_reversa_trans(pi_folio)

            
        WHEN "his"
            LET gr_bitacora.desc_tramite  = "CARGA OP.19 SALDO HIST"

            SELECT cod_tramite
            INTO   gr_bitacora.cod_tramite
            FROM   tab_tipo_tramite
            WHERE  descripcion = "TRANSFERENCIA"

            CALL f_reversa_his(pi_folio)

            
        WHEN "dis"
            LET gr_bitacora.desc_tramite  = "CARGA OP.06 DISPOSICIONES"
        
            SELECT cod_tramite
            INTO   gr_bitacora.cod_tramite
            FROM   tab_tipo_tramite
            WHERE  descripcion = "DISPOSICION"
            
            CALL f_reversa_disp(pi_folio)
    END CASE
    
    CALL f_act_bitacora(pi_folio)
    
    PROMPT " PROCESO FINALIZADO. PRESIONE <ENTER> PARA SALIR " FOR CHAR enter
    CLOSE WINDOW retc81011
END FUNCTION

#---------------------------------------------------------------------------#
# f_reversa_trans : Realiza el reverso de la carga de archivo para          #
#                   transferencias                                          #
#---------------------------------------------------------------------------#
FUNCTION f_reversa_trans(pi_folio_carga)

    DEFINE lr_desmarca RECORD
        nss             LIKE ret_solicitud_tx.nss           ,
        marca_cod       LIKE cta_his_marca.marca_cod        ,
        consec          LIKE ret_solicitud_tx.consecutivo   ,
        edo_marca       SMALLINT                            ,
        marca_causa     SMALLINT                            
    END RECORD

    DEFINE
        li_cont             ,
        li_cont_marca       ,
        li_cont_reg         ,
        li_cont_viv         ,
        pi_folio_carga      ,
        li_folio            INTEGER

    LET li_cont  = 0

    -----------------------------------------------------------------------------
    DECLARE cur_des CURSOR FOR
        SELECT  A.nss           ,
                A.marca_cod     ,
                A.correlativo   ,
                40              ,
                0               
        FROM    cta_his_marca A ,
                ret_transf_rx B
        WHERE   A.nss               = B.nss
        AND     A.correlativo       = B.consecutivo
        AND     A.marca_cod         IN (800,810,815)
        AND     B.folio             = pi_folio_carga
        AND     B.estado_solicitud  = gr_edo.recibido
        AND     A.fecha_fin IS NULL
    
    FOREACH cur_des INTO lr_desmarca.*

        EXECUTE eje_desmarca USING lr_desmarca.*        , 
                                   gr_bitacora.usuario
        
        IF SQLCA.SQLCODE = 0 THEN
            LET li_cont = li_cont + 1
        END IF
    
    END FOREACH

    DISPLAY "MARCAS REVERSADAS POR FUNCION          : ", li_cont
            USING "<<<,<<&" AT 10,09

    LET gar_tablas_rev[1].tabla     = "cta_act_marca"
    LET gar_tablas_rev[1].accion    = "REVERSO POR SPL"
    LET gar_tablas_rev[1].num_regs  = li_cont

    -----------------------------------------------------------------------------
    DELETE
    FROM   ret_det_datamart
    WHERE  folio          = pi_folio_carga

    LET li_cont = SQLCA.SQLERRD[3]

    DISPLAY "R. ELIMINADOS EN ret_det_datamart      : ", li_cont
            USING "<<<,<<&" AT 11,09

    LET gar_tablas_rev[2].tabla     = "ret_det_datamart"
    LET gar_tablas_rev[2].accion    = "BORRADA"
    LET gar_tablas_rev[2].num_regs  = li_cont
    -----------------------------------------------------------------------------    
    DELETE
    FROM   ret_transf_rx
    WHERE  folio          = pi_folio_carga

    LET li_cont = SQLCA.SQLERRD[3]

    DISPLAY "R. ELIMINADOS EN ret_transf_rx         : ", li_cont
            USING "<<<,<<&" AT 12,09

    LET gar_tablas_rev[3].tabla     = "ret_cza_lote"
    LET gar_tablas_rev[3].accion    = "BORRADA"
    LET gar_tablas_rev[3].num_regs  = li_cont
    -----------------------------------------------------------------------------    
    
    DELETE
    FROM   ret_marca_pensionado
    WHERE  folio          = pi_folio_carga

    LET li_cont = SQLCA.SQLERRD[3]

    DISPLAY "R. ELIMINADOS EN ret_marca_pensionado  : ", li_cont
            USING "<<<,<<&" AT 13,09

    LET gar_tablas_rev[4].tabla     = "ret_marca_pensionado"
    LET gar_tablas_rev[4].accion    = "BORRADA"
    LET gar_tablas_rev[4].num_regs  = li_cont
    
    -----------------------------------------------------------------------------    
    
    DELETE
    FROM   ret_cza_lote
    WHERE  folio          = pi_folio_carga

    LET li_cont = SQLCA.SQLERRD[3]

    DISPLAY "R. ELIMINADOS EN ret_cza_lote          : ", li_cont
            USING "<<<,<<&" AT 14,09

    LET gar_tablas_rev[5].tabla     = "ret_cza_lote"
    LET gar_tablas_rev[5].accion    = "BORRADA"
    LET gar_tablas_rev[5].num_regs  = li_cont
    -----------------------------------------------------------------------------    
END FUNCTION

#---------------------------------------------------------------------------#
# f_reversa_his : Realiza el reverso de la carga de archivo para            #
#                 saldos historicos                                         #
#---------------------------------------------------------------------------#
FUNCTION f_reversa_his(pi_folio_carga)

    DEFINE
        li_cont             ,
        pi_folio_carga      INTEGER
    
    -----------------------------------------------------------------------------    
    DELETE
    FROM   ret_sal_his_rx
    WHERE  folio          = pi_folio_carga
    
    LET li_cont = SQLCA.SQLERRD[3]

    DISPLAY "R. ELIMINADOS EN ret_sal_his_rx       : ", li_cont
            USING "<<<,<<&" AT 11,09

    LET gar_tablas_rev[1].tabla     = "ret_sal_his_rx"
    LET gar_tablas_rev[1].accion    = "BORRADA"
    LET gar_tablas_rev[1].num_regs  = li_cont
    -----------------------------------------------------------------------------
    DELETE
    FROM   ret_cza_lote
    WHERE  folio          = pi_folio_carga

    LET li_cont = SQLCA.SQLERRD[3]

    DISPLAY "R. ELIMINADOS EN ret_cza_lote         : ", li_cont
            USING "<<<,<<&" AT 12,09

    LET gar_tablas_rev[2].tabla     = "ret_cza_lote"
    LET gar_tablas_rev[2].accion    = "BORRADA"
    LET gar_tablas_rev[2].num_regs  = li_cont
    -----------------------------------------------------------------------------    
END FUNCTION

#---------------------------------------------------------------------------#
# f_reversa_disp : Realiza el reverso de la carga de archivo para           #
#                  disposiciones                                            #
#---------------------------------------------------------------------------#
FUNCTION f_reversa_disp(pi_folio_carga)

    DEFINE lr_datos RECORD
        nss             LIKE ret_solicitud_tx.nss           ,
        consecutivo     LIKE ret_solicitud_tx.consecutivo   ,
        tipo_retiro     LIKE ret_solicitud_tx.tipo_retiro   ,
        diag_registro   LIKE ret_solicitud_tx.diag_registro  
    END RECORD

    DEFINE
        li_cont             ,
        li_cont_marca       ,
        li_cont_reg         ,
        li_cont_viv         ,
        pi_folio_carga      ,
        li_folio            INTEGER

    DEFINE
        ls_marca_cod        SMALLINT

    DEFINE
        ldt_fecha_ini       DATE

    LET li_cont         = 0
    LET li_cont_marca   = 0
    LET li_cont_reg     = 0
    LET li_cont_viv     = 0
    
    -- Buscamos los folios involucrados en la carga del archivo
    DECLARE cur_fol CURSOR FOR
    SELECT UNIQUE A.folio
    FROM   ret_monto_siefore A,
           ret_monto_siefore B
    WHERE  A.nss            = B.nss
    AND    A.consecutivo    = B.consecutivo
    AND    B.folio          = pi_folio_carga
    AND    A.tipo_operacion = 5

    FOREACH cur_fol INTO li_folio
        
        DECLARE cur_reg CURSOR FOR
        SELECT nss              ,
               consecutivo      ,
               tipo_retiro      ,
               diag_registro    
        FROM   ret_solicitud_tx
        WHERE  folio    = li_folio
        
        FOREACH cur_reg INTO lr_datos.*
        
            -- Si existe un diagnostico de rechazo, se reversa la desmarca
            IF lr_datos.diag_registro <> 400 AND lr_datos.diag_registro <> 440 THEN
                -- Si la afore es XXI y el retiro es G no se hace nada
                IF gs_codigo_afore <> gs_xxi OR lr_datos.tipo_retiro <> "G" THEN
                    
                    SELECT movimiento
                    INTO   ls_marca_cod
                    FROM   tab_retiro
                    WHERE  tipo_retiro = lr_datos.tipo_retiro
                    
                    SELECT fecha_ini
                    INTO   ldt_fecha_ini
                    FROM   cta_his_marca
                    WHERE  nss          = lr_datos.nss
                    AND    correlativo  = lr_datos.consecutivo
                    AND    marca_cod    = ls_marca_cod
                    AND    fecha_fin IS NOT NULL
                    
                    IF STATUS <> NOTFOUND THEN
                        EXECUTE eje_rev_desmarca USING lr_datos.nss         ,
                                                       ls_marca_cod         ,
                                                       lr_datos.consecutivo ,
                                                       ldt_fecha_ini
            
                        LET li_cont_marca = li_cont_marca + 1

                        DISPLAY "DESMARCAS REVERSADAS POR FUNCION     : ", li_cont_marca
                                USING "<<<,<<&" AT 10,9
                    
                    END IF -- Reverso de desmarca
                END IF -- Afore xxi y ret g
            END IF -- diag de rechazo
            
            UPDATE ret_solicitud_tx
            SET    diag_registro        = ""
            WHERE  folio                = li_folio
            AND    nss                  = lr_datos.nss
            AND    consecutivo          = lr_datos.consecutivo

            LET li_cont_reg   = li_cont_reg + 1
            
            UPDATE ret_monto_viv
            SET    pesos_viv72          = "" ,
                   estado_sub_viv       = "" ,
                   acc_viv97_bdsviv     = "" ,
                   acc_viv92_bdsviv     = ""
            WHERE  folio                = li_folio
            AND    nss                  = lr_datos.nss
            AND    consecutivo          = lr_datos.consecutivo

            LET li_cont_viv   = li_cont_viv + 1
        
        END FOREACH -- Registros por folio

    END FOREACH

    LET gar_tablas_rev[1].tabla     = "cta_act_marca"
    LET gar_tablas_rev[1].accion    = "REVERSO POR SPL"
    LET gar_tablas_rev[1].num_regs  = li_cont_marca

    -----------------------------------------------------------------------------
    DISPLAY "R. ACTUALIZADOS EN ret_solicitud_tx  : ", li_cont_reg
             USING "<<<,<<&" AT 11,09
    
    LET gar_tablas_rev[2].tabla     = "ret_solicitud_tx"
    LET gar_tablas_rev[2].accion    = "ACTUALIZA"
    LET gar_tablas_rev[2].num_regs  = li_cont_reg
    -----------------------------------------------------------------------------
    DISPLAY "R. ACTUALIZADOS EN ret_monto_viv     : ", li_cont_viv
             USING "<<<,<<&" AT 12,09
    
    LET gar_tablas_rev[3].tabla     = "ret_monto_viv"
    LET gar_tablas_rev[3].accion    = "ACTUALIZA"
    LET gar_tablas_rev[3].num_regs  = li_cont_viv
    -----------------------------------------------------------------------------
    DELETE
    FROM   ret_cza_lote
    WHERE  folio          = pi_folio_carga

    LET li_cont = SQLCA.SQLERRD[3]

    DISPLAY "R. ELIMINADOS EN ret_cza_lote        : ", li_cont
            USING "<<<,<<&" AT 13,09

    LET gar_tablas_rev[4].tabla     = "ret_cza_lote"
    LET gar_tablas_rev[4].accion    = "BORRADA"
    LET gar_tablas_rev[4].num_regs  = li_cont
    -----------------------------------------------------------------------------    
    DELETE
    FROM   ret_monto_siefore
    WHERE  folio          = pi_folio_carga
    AND    tipo_operacion = 6
    
    LET li_cont = SQLCA.SQLERRD[3]

    DISPLAY "R. ELIMINADOS EN ret_monto_siefore   : ", li_cont
            USING "<<<,<<&" AT 14,09

    LET gar_tablas_rev[5].tabla     = "ret_monto_siefore"
    LET gar_tablas_rev[5].accion    = "BORRADA"
    LET gar_tablas_rev[5].num_regs  = li_cont
    -----------------------------------------------------------------------------    

END FUNCTION

#---------------------------------------------------------------------------#
# f_act_bitacora : Inserta en las tablas de bitacora y de tablas del        #
#                  reverso realizado                                        #
#---------------------------------------------------------------------------#
FUNCTION f_act_bitacora(p_folio)

    DEFINE p_folio LIKE ret_bitacora_rev.folio
    DEFINE ls_ind_rev LIKE ret_bitacora_rev.id_rev

    DEFINE 
        i           SMALLINT

    LET gr_bitacora.folio       = p_folio
    LET gr_bitacora.fecha_fin   = TODAY
    LET gr_bitacora.hora_fin    = CURRENT HOUR TO SECOND

    INSERT INTO ret_bitacora_rev
    VALUES (gr_bitacora.*)

    -- Rescatamos el serial que se le asigno al movimiento
    SELECT MAX(id_rev)
    INTO   ls_ind_rev
    FROM   ret_bitacora_rev
    WHERE  folio = gr_bitacora.folio

    FOR i = 1 TO 5
        IF gar_tablas_rev[i].num_regs > 0 THEN 
            INSERT INTO ret_tablas_rev
            VALUES (ls_ind_rev          ,
                    gr_bitacora.folio   ,
                    gar_tablas_rev[i].*
                   )
        END IF
    END FOR

END FUNCTION
