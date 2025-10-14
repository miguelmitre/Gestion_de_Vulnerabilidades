################################################################################
#Proyecto          => SISTEMA DE AFORES( SAFRE )                               #
#Owner             => E.F.P.                                                   #
#Programa PENC1051 => REVERSO DE PRELIQUIDACION RETIROS POR PMG                #
#Fecha creacion    => 9 DE ABRIL DE 2010                                       #
#By                => JAVIER GONZALEZ JERONIMO                                 #
#Fecha actualiz.   =>                                                          #
#Actualizacion     =>                                                          #
#Sistema           => RET                                                      #
################################################################################
DATABASE safre_af

GLOBALS

    DEFINE gr_bitacora RECORD LIKE ret_bitacora_rev.*

    DEFINE gr_edo RECORD
        capturado       LIKE pen_estado_pmg.estado_solicitud    ,
        enviado         LIKE pen_estado_pmg.estado_solicitud    ,
        recibido        LIKE pen_estado_pmg.estado_solicitud    ,
        en_pago         LIKE pen_estado_pmg.estado_solicitud    ,
        preliquidado    LIKE pen_estado_pmg.estado_solicitud    ,
        liquidado       LIKE pen_estado_pmg.estado_solicitud    ,
        rechazado       LIKE pen_estado_pmg.estado_solicitud
    END RECORD

    DEFINE gar_tablas_rev ARRAY[9] OF RECORD
        accion      LIKE ret_tablas_rev.accion       ,
        tabla       LIKE ret_tablas_rev.tabla        ,
        num_regs    LIKE ret_tablas_rev.num_registros
    END RECORD

    DEFINE gr_datos RECORD 
        folio           LIKE pen_solicitud_pmg.folio_lote
    END RECORD

    DEFINE
        gs_procesa              SMALLINT

    DEFINE
        HOY                     DATE

    DEFINE
        enter                   CHAR(1)

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        ACCEPT KEY CONTROL-I ,
        INPUT WRAP           ,
        PROMPT LINE LAST

    CALL STARTLOG("PENC1051.log")
    CALL init()#i
    
    CALL f_captura_datos() RETURNING gs_procesa, gr_datos.*
    
    IF gs_procesa THEN 
        CALL f_reverso(gr_datos.*)
        CALL f_act_bitacora(gr_datos.*)
        PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR CHAR enter
    END IF     
 
    CLOSE WINDOW penc1051

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
    WHERE  descripcion = "PENSION MINIMA GARANTIZADA"

    LET gr_bitacora.programa      = "PENC1051"
    LET gr_bitacora.desc_tramite  = "PRELIQUIDACION PMG"
    LET gr_bitacora.fecha_ini     = HOY
    LET gr_bitacora.hora_ini      = CURRENT HOUR TO SECOND
    
    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.capturado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "CAPTURADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.enviado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "ENVIADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.recibido
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "RECIBIDO"

    SELECT A.estado_solicitud
    INTO   gr_edo.en_pago
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "EN PROCESO DE PAGO"

    SELECT A.estado_solicitud
    INTO   gr_edo.preliquidado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "PRELIQUIDADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.liquidado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "LIQUIDADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.rechazado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "RECHAZADO"

    ----- REVERSA DESMARCA -----
    LET lc_prepare = " EXECUTE PROCEDURE reversa_desmarca(?,?,?,?) "
    PREPARE eje_rev_desmarca FROM lc_prepare
    
    LET lc_prepare = " "

END FUNCTION 


#---------------------------------------------------------------------------#
# f_captura_datos : Captura el folio del que se hara el reverso de la       #
#                   liquidacion                                             #
#---------------------------------------------------------------------------#
FUNCTION f_captura_datos()

    DEFINE lr_captura RECORD 
        folio_liquida   LIKE dis_cuenta.folio
    END RECORD
    
    DEFINE ls_estado LIKE pen_solicitud_pmg.estado_solicitud

    DEFINE
        ldt_fecha_liquida       DATE

    DEFINE
        lc_cad_msg          CHAR(100)

    DEFINE
        ls_procesa          SMALLINT

    SELECT MAX(folio_liquida)
    INTO   lr_captura.folio_liquida
    FROM   pen_ctr_pago_det

    OPEN WINDOW penc1051 AT 4,4 WITH FORM "PENC10511" ATTRIBUTE(BORDER)
    DISPLAY " <Ctrl-C> Salir                                                                " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " PENC1051        REVERSO DE PRELIQUIDACION DE RETIROS PMG                      " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    INPUT BY NAME lr_captura.* WITHOUT DEFAULTS
        AFTER FIELD folio_liquida
            IF lr_captura.folio_liquida IS NULL THEN
                ERROR " EL FOLIO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                NEXT FIELD folio_liquida
            ELSE
                CALL f_verifica_estado(lr_captura.*) RETURNING ls_estado, lc_cad_msg
                IF ls_estado <> 0 THEN
                    ERROR lc_cad_msg ATTRIBUTE(NORMAL)
                    NEXT FIELD folio_liquida
                ELSE
                    SELECT UNIQUE(fecha_conversion)
                    INTO   ldt_fecha_liquida
                    FROM   pen_preliquida_pmg
                    WHERE  folio        = lr_captura.folio_liquida
                    AND    tipo_movimiento = 841

                    DISPLAY "Fecha de Liquidacion :        ", ldt_fecha_liquida AT 7,16
                END IF            
            END IF

        ON KEY (ESC)
            IF lr_captura.folio_liquida IS NULL THEN
                ERROR " EL FOLIO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                NEXT FIELD folio_liquida
            ELSE
                CALL f_verifica_estado(lr_captura.*) RETURNING ls_estado, lc_cad_msg
                IF ls_estado <> 0 THEN
                    ERROR lc_cad_msg ATTRIBUTE(NORMAL)
                    NEXT FIELD folio_liquida
                ELSE
                    SELECT UNIQUE(fecha_conversion)
                    INTO   ldt_fecha_liquida
                    FROM   pen_preliquida_pmg
                    WHERE  folio        = lr_captura.folio_liquida
                    AND    tipo_movimiento = 841

                    DISPLAY "Fecha de Liquidacion :        ", ldt_fecha_liquida AT 7,16
                END IF            
            END IF

            WHILE TRUE
                PROMPT "¿ EJECUTAR REVERSO DE LA PRELIQUIDACION DE PMG ? (S/N) : " FOR CHAR enter
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
            PROMPT " PROCESO CANCELADO ...<ENTER> PARA SALIR " FOR CHAR enter
            EXIT PROGRAM

    END INPUT
 
    RETURN ls_procesa, lr_captura.*
   
    
END FUNCTION 

#---------------------------------------------------------------------------#
# f_reverso : Ejecuta el reverso de la operacion solicitada                 #
#---------------------------------------------------------------------------#
FUNCTION f_reverso(pr_datos)

    DEFINE pr_datos RECORD 
        folio           LIKE pen_solicitud_pmg.folio_lote
    END RECORD

    DEFINE lr_desmarca RECORD
        marca_cod   LIKE cta_act_marca.marca_cod    ,
        fecha_ini   LIKE cta_act_marca.fecha_ini
    END RECORD

    DEFINE lr_soli RECORD
        nss         LIKE pen_solicitud_pmg.nss            ,
        consec      LIKE pen_solicitud_pmg.consecutivo    ,
        num_mes     LIKE pen_ctr_pago_det.num_mensualidad
    END RECORD
    
    DEFINE
        ls_edo_pago         SMALLINT

    DEFINE 
        li_desmarca         ,
        li_cont             INTEGER
    #----------------------------------------------------------------------------
        
    LET li_desmarca = 0
    LET li_cont     = 0
    DISPLAY " PROCESANDO INFORMACION " AT 19,2 ATTRIBUTE(REVERSE)

    -----------------------------------------------------------------------------
    DELETE
    FROM   pen_preliquida_pmg
    WHERE  folio = pr_datos.folio

    LET li_cont = SQLCA.SQLERRD[3]
    
    DISPLAY "R. ELIMINADOS EN pen_preliquida_pmg       : ", li_cont
            USING "<<<,<<&" AT 9,10

    LET gar_tablas_rev[1].tabla     = "pen_preliquida_pmg"
    LET gar_tablas_rev[1].accion    = "BORRADA"
    LET gar_tablas_rev[1].num_regs  = li_cont

    -----------------------------------------------------------------------------

    UPDATE pen_ctr_pago
    SET    estado = gr_edo.enviado
    WHERE  estado IN (gr_edo.liquidado, gr_edo.en_pago)
    AND    consecutivo IN (SELECT consecutivo
                           FROM   pen_ctr_pago_det
                           WHERE  folio_liquida = pr_datos.folio
                           AND    estado        = gr_edo.liquidado
                           )

    LET li_cont = SQLCA.SQLERRD[3]

    DISPLAY "R. ACTUALIZADOS EN pen_ctr_pago           : ", li_cont
             USING "<<<,<<&" AT 10,10
    
    LET gar_tablas_rev[2].tabla     = "pen_ctr_pago"
    LET gar_tablas_rev[2].accion    = "ACTUALIZA"
    LET gar_tablas_rev[2].num_regs  = li_cont    

    -----------------------------------------------------------------------------

    LET li_cont     = 0

    DECLARE cur_det CURSOR FOR 
     SELECT nss             ,
            consecutivo     ,
            num_mensualidad
     FROM   pen_ctr_pago_det
     WHERE  folio_liquida = pr_datos.folio
     AND    estado        = gr_edo.liquidado

    FOREACH cur_det INTO lr_soli.*
    
        IF lr_soli.num_mes = 1 THEN
            LET ls_edo_pago = gr_edo.recibido
        ELSE
            LET ls_edo_pago = gr_edo.en_pago
        END IF

        IF lr_soli.num_mes = 12 THEN
            SELECT B.marca_cod        ,
                   B.fecha_ini
            INTO   lr_desmarca.*
            FROM   pen_ctr_pago_det   A   ,
                   cta_his_marca      B
            WHERE  A.nss = B.nss
            AND    A.consecutivo      = B.correlativo 
            AND    A.folio_liquida    = pr_datos.folio
            AND    A.estado           = gr_edo.liquidado
            AND    A.num_mensualidad  = 12
            AND    B.fecha_fin IS NOT NULL

            EXECUTE eje_rev_desmarca USING lr_soli.nss           ,
                                           lr_desmarca.marca_cod ,  
                                           lr_soli.consec        ,
                                           lr_desmarca.fecha_ini  

            IF SQLCA.SQLCODE = 0 THEN
                LET li_desmarca = li_desmarca + 1
            END IF
        
        END IF 

        UPDATE pen_solicitud_pmg
        SET    fecha_pago       = NULL              ,
               estado_solicitud = ls_edo_pago
        WHERE  nss              = lr_soli.nss
        AND    consecutivo      = lr_soli.consec
        AND    estado_solicitud IN (gr_edo.liquidado, gr_edo.en_pago)
        
        UPDATE pen_ctr_pago_det
        SET    fecha_liquida    = NULL  ,
               folio_liquida    = 0     ,
               estado           = gr_edo.recibido
        WHERE  folio_liquida    = pr_datos.folio
        AND    nss              = lr_soli.nss
        AND    consecutivo      = lr_soli.consec
        AND    estado           = gr_edo.liquidado

        LET li_cont = li_cont + 1
    
    END FOREACH

    DISPLAY "R. ACTUALIZADOS EN pen_ctr_pago_det       : ", li_cont
            USING "<<<,<<&" AT 11,10

    LET gar_tablas_rev[3].tabla     = "pen_ctr_pago_det"
    LET gar_tablas_rev[3].accion    = "ACTUALIZA"
    LET gar_tablas_rev[3].num_regs  = li_cont

    -----------------------------------------------------------------------------

    DISPLAY "R. ACTUALIZADOS EN pen_solicitud_pmg      : ", li_cont
             USING "<<<,<<&" AT 12,10
    
    LET gar_tablas_rev[4].tabla     = "pen_solicitud_pmg"
    LET gar_tablas_rev[4].accion    = "ACTUALIZA"
    LET gar_tablas_rev[4].num_regs  = li_cont
    
    -----------------------------------------------------------------------------

    DISPLAY "MARCAS REVERSADAS POR FUNCION             : ", li_desmarca
            USING "<<<,<<&" AT 13,10

    LET gar_tablas_rev[5].tabla     = "cta_act_marca"
    LET gar_tablas_rev[5].accion    = "REVERSO POR SPL"
    LET gar_tablas_rev[5].num_regs  = li_desmarca

    
END FUNCTION

#---------------------------------------------------------------------------#
# f_act_bitacora : Inserta en las tablas de bitacora y de tablas del        #
#                  reverso realizado                                        #
#---------------------------------------------------------------------------#
FUNCTION f_act_bitacora(p_folio)

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

    FOR i = 1 TO 5
        IF gar_tablas_rev[i].num_regs > 0 THEN
            INSERT INTO ret_tablas_rev
            VALUES (ind_rev,
                    gr_bitacora.folio,
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
        folio           LIKE dis_cuenta.folio
    END RECORD

    DEFINE
        ls_estado   LIKE pen_solicitud_pmg.estado_solicitud

    DEFINE
        lc_error        CHAR(100)

    DEFINE
        ls_id           SMALLINT

    LET ls_id = 0
    INITIALIZE ls_estado TO NULL

    SELECT "OK"
    FROM   pen_preliquida_pmg
    WHERE  folio = lr_datos.folio
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        LET lc_error = "EL FOLIO INGRESADO NO ESTA PRELIQUIDADO "
        LET ls_id = 1
    END IF

    RETURN ls_id, lc_error

END FUNCTION


