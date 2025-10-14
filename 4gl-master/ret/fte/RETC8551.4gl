################################################################################
#Proyecto          => SISTEMA DE AFORES( SAFRE )                               #
#Owner             => E.F.P.                                                   #
#Programa RETC8551 => REVERSO LIQUIDACION PARCIALES DESEMPLEO A, B y C         #
#Fecha creacion    => 19 DE JUNIO DE 2008                                      #
#By                => JAVIER GONZALEZ JERONIMO                                 #
#Fecha actualiz.   =>                                                          #
#Actualizacion     =>                                                          #
#Sistema           => RET                                                      #
################################################################################
DATABASE safre_af

GLOBALS

    DEFINE gr_bitacora RECORD LIKE ret_bitacora_rev.*

    DEFINE gr_edo RECORD #gr_edo
        procesado             LIKE ret_estado.estado_solicitud ,
        provisionado          LIKE ret_estado.estado_solicitud ,
        liquidado             LIKE ret_estado.estado_solicitud 
    END RECORD

    DEFINE gar_tablas_rev ARRAY[12] OF RECORD
        accion      LIKE ret_tablas_rev.accion       ,
        tabla       LIKE ret_tablas_rev.tabla        ,
        num_regs    LIKE ret_tablas_rev.num_registros
    END RECORD

    DEFINE gr_datos RECORD 
        folio           LIKE ret_solicitud_tx.folio
    END RECORD

    DEFINE
        gs_procesa      SMALLINT

    DEFINE #glo #date
        HOY                   DATE

    DEFINE #glo #char
        enter                 CHAR(1)

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        ACCEPT KEY CONTROL-I ,
        INPUT WRAP           ,
        PROMPT LINE LAST

    CALL STARTLOG("RETC8551.log")
    CALL init()#i
    
    CALL f_captura_datos() RETURNING gs_procesa, gr_datos.*
    
    IF gs_procesa THEN 
        CALL f_reverso(gr_datos.*)
        CALL f_act_bitacora(gr_datos.*)
        PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR CHAR enter
    END IF     
 
    CLOSE WINDOW retc_8551

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
    WHERE  descripcion = "RETIRO PARCIAL"

    LET gr_bitacora.programa      = "RETC8551"
    LET gr_bitacora.desc_tramite  = "LIQUIDA PAR DES A, B, C"
    LET gr_bitacora.fecha_ini     = HOY
    LET gr_bitacora.hora_ini      = CURRENT HOUR TO SECOND
    
    ----- ESTADOS DE SOLICITUD -----    
    SELECT A.estado_solicitud
    INTO   gr_edo.procesado
    FROM   ret_estado A
    WHERE  A.descripcion = "PROCESADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.provisionado
    FROM   ret_estado A
    WHERE  A.descripcion = "PROVISIONADO"

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
        folio_liquida   LIKE ret_solicitud_tx.folio
    END RECORD
    
    DEFINE ls_estado LIKE ret_solicitud_tx.estado_solicitud

    DEFINE
        ldt_fecha_liquida       DATE

    DEFINE
        lc_cad_msg          CHAR(100)

    DEFINE
        ls_procesa          SMALLINT

    SELECT MAX(folio_op16)
    INTO   lr_captura.folio_liquida
    FROM   ret_ctr_pago_det

    OPEN WINDOW retc_8551 AT 4,4 WITH FORM "RETC85511" ATTRIBUTE(BORDER)
    DISPLAY " <Ctrl-C> Salir                                                                " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC8551 REVERSO LIQUIDACION PARCIALES DESEMPLEO TIPO A, B Y C                " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    INPUT BY NAME lr_captura.* WITHOUT DEFAULTS
        AFTER FIELD folio_liquida
            IF lr_captura.folio_liquida IS NULL THEN
                ERROR "    EL FOLIO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                NEXT FIELD folio_liquida
            ELSE
                CALL f_verifica_estado(lr_captura.*) RETURNING ls_estado, lc_cad_msg
                IF ls_estado <> 0 THEN
                    ERROR lc_cad_msg ATTRIBUTE(NORMAL)
                    NEXT FIELD folio_liquida
                ELSE
                    SELECT UNIQUE(fecha_conversion)
                    INTO   ldt_fecha_liquida
                    FROM   dis_cuenta
                    WHERE  folio        = lr_captura.folio_liquida
                    AND    tipo_movimiento IN (876,877,878)

                    DISPLAY "Fecha de Liquidacion :        ", ldt_fecha_liquida AT 7,16
                END IF            
            END IF

        ON KEY (ESC)
            IF lr_captura.folio_liquida IS NULL THEN
                ERROR "    EL FOLIO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                NEXT FIELD folio_liquida
            ELSE
                CALL f_verifica_estado(lr_captura.*) RETURNING ls_estado, lc_cad_msg
                IF ls_estado <> 0 THEN
                    ERROR lc_cad_msg ATTRIBUTE(NORMAL)
                    NEXT FIELD folio_liquida
                ELSE
                    SELECT UNIQUE(fecha_conversion)
                    INTO   ldt_fecha_liquida
                    FROM   dis_cuenta
                    WHERE  folio        = lr_captura.folio_liquida
                    AND    tipo_movimiento IN (876,877,878)

                    DISPLAY "Fecha de Liquidacion :        ", ldt_fecha_liquida AT 7,16
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
        folio           LIKE ret_solicitud_tx.folio
    END RECORD

    DEFINE 
        li_cont        INTEGER

    DEFINE lr_reg RECORD
        nss         LIKE ret_parcial.nss            ,
        marca_cod   LIKE cta_act_marca.marca_cod    ,
        consec      LIKE ret_parcial.consecutivo    ,
        fecha_ini   LIKE cta_act_marca.fecha_ini
    END RECORD

    LET li_cont = 0
    DISPLAY " PROCESANDO INFORMACION " AT 19,2 ATTRIBUTE(REVERSE)

    -----------------------------------------------------------------------------
    
    DECLARE cur_ptx CURSOR FOR
    SELECT A.nss              , 
           B.marca_cod        ,
           A.consecutivo      ,
           B.fecha_ini
    FROM   ret_parcial      A   ,
           cta_his_marca    B   ,
           ret_ctr_pago_det C   ,
           ret_ctr_pago     D
    WHERE  A.nss = B.nss
    AND    A.nss = C.nss
    AND    A.nss = D.nss
    AND    A.consecutivo      = B.correlativo 
    AND    A.consecutivo      = C.consecutivo
    AND    A.consecutivo      = D.consecutivo
    AND    C.folio_op16       = pr_datos.folio
    AND    A.estado_solicitud = gr_edo.liquidado
    AND    A.estado_solicitud = D.estado
    AND    A.estado_solicitud = C.estado
    AND    D.mensualidades    = C.num_mensualidad
    AND    A.tipo_desempleo   IN ("A", "B", "C")
    AND    B.fecha_fin IS NOT NULL

    FOREACH cur_ptx INTO lr_reg.*

        EXECUTE eje_rev_desmarca USING lr_reg.*

        IF SQLCA.SQLCODE = 0 THEN
            LET li_cont = li_cont + 1
        END IF
        
        UPDATE ret_parcial
        SET    estado_solicitud = gr_edo.procesado
        WHERE  nss              = lr_reg.nss
        AND    consecutivo      = lr_reg.consec
        AND    estado_solicitud = gr_edo.liquidado

    END FOREACH

    DISPLAY "MARCAS REVERSADAS POR FUNCION           : ", li_cont
            USING "<<<,<<&" AT 8,10

    LET gar_tablas_rev[1].tabla     = "cta_act_marca"
    LET gar_tablas_rev[1].accion    = "REVERSO POR SPL"
    LET gar_tablas_rev[1].num_regs  = li_cont

    -----------------------------------------------------------------------------
    
    DISPLAY "R. ACTUALIZADOS EN ret_parcial          : ", li_cont
             USING "<<<,<<&" AT 9,10
    
    LET gar_tablas_rev[2].tabla     = "ret_parcial"
    LET gar_tablas_rev[2].accion    = "ACTUALIZA"
    LET gar_tablas_rev[2].num_regs  = li_cont

    -----------------------------------------------------------------------------

    DELETE
    FROM   ret_ctr_envio
    WHERE  folio          = pr_datos.folio

    LET li_cont = SQLCA.SQLERRD[3]
    
    DISPLAY "R. ELIMINADOS EN ret_ctr_envio          : ", li_cont
            USING "<<<,<<&" AT 10,10

    LET gar_tablas_rev[3].tabla     = "ret_ctr_envio"
    LET gar_tablas_rev[3].accion    = "BORRADA"
    LET gar_tablas_rev[3].num_regs  = li_cont

    -----------------------------------------------------------------------------
    DELETE
    FROM   ret_sum_envio
    WHERE  folio          = pr_datos.folio

    LET li_cont = SQLCA.SQLERRD[3]
    
    DISPLAY "R. ELIMINADOS EN ret_sum_envio          : ", li_cont
            USING "<<<,<<&" AT 11,10

    LET gar_tablas_rev[4].tabla     = "ret_sum_envio"
    LET gar_tablas_rev[4].accion    = "BORRADA"
    LET gar_tablas_rev[4].num_regs  = li_cont

    -----------------------------------------------------------------------------
    
    DELETE
    FROM   dis_cuenta
    WHERE  folio           = pr_datos.folio

    LET li_cont = SQLCA.SQLERRD[3]
    
    DISPLAY "R. ELIMINADOS EN dis_cuenta             : ", li_cont
            USING "<<<,<<&" AT 12,10

    LET gar_tablas_rev[5].tabla     = "dis_cuenta"
    LET gar_tablas_rev[5].accion    = "BORRADA"
    LET gar_tablas_rev[5].num_regs  = li_cont

    -----------------------------------------------------------------------------
    DELETE
    FROM   ret_monto_siefore
    WHERE  folio           = pr_datos.folio
    AND    tipo_retiro     = "I"
    AND    tipo_operacion  = 16
    
    LET li_cont = SQLCA.SQLERRD[3]
    
    DISPLAY "R. ELIMINADOS EN ret_monto_siefore      : ", li_cont
            USING "<<<,<<&" AT 13,10

    LET gar_tablas_rev[6].tabla     = "ret_monto_siefore"
    LET gar_tablas_rev[6].accion    = "BORRADA"
    LET gar_tablas_rev[6].num_regs  = li_cont

    -----------------------------------------------------------------------------
    UPDATE ret_parcial_tx
    SET    fecha_valuacion  = "01/01/0001" ,
           fecha_pago       = "01/01/0001" ,
           impt_ret_97      = 0            ,
           impt_ces_vej     = 0            ,
           impt_cuo_soc     = 0            ,
           impt_tot_sub_rcv = 0            ,
           pago_desempleo   = 0
    WHERE  consecutivo IN ( SELECT consecutivo
                            FROM   ret_ctr_pago_det
                            WHERE  folio_op16 = pr_datos.folio
                            )

    LET li_cont = SQLCA.SQLERRD[3]

    DISPLAY "R. ACTUALIZADOS EN ret_parcial_tx       : ", li_cont
             USING "<<<,<<&" AT 14,10
    
    LET gar_tablas_rev[7].tabla     = "ret_parcial_tx"
    LET gar_tablas_rev[7].accion    = "ACTUALIZA"
    LET gar_tablas_rev[7].num_regs  = li_cont
    
    -----------------------------------------------------------------------------

    UPDATE ret_ctr_pago
    SET    estado = gr_edo.provisionado
    WHERE  estado = gr_edo.liquidado
    AND    consecutivo IN (SELECT consecutivo
                           FROM   ret_ctr_pago_det
                           WHERE  folio_op16 = pr_datos.folio
                           AND    estado     = gr_edo.liquidado
                          )

    LET li_cont = SQLCA.SQLERRD[3]

    DISPLAY "R. ACTUALIZADOS EN ret_ctr_pago         : ", li_cont
             USING "<<<,<<&" AT 15,10
    
    LET gar_tablas_rev[8].tabla     = "ret_ctr_pago"
    LET gar_tablas_rev[8].accion    = "ACTUALIZA"
    LET gar_tablas_rev[8].num_regs  = li_cont    

    -----------------------------------------------------------------------------
    
    UPDATE ret_ctr_pago_det
    SET    estado       = gr_edo.provisionado ,
           folio_op16   = 0
    WHERE  folio_op16   = pr_datos.folio
    AND    estado       = gr_edo.liquidado

    LET li_cont = SQLCA.SQLERRD[3]

    DISPLAY "R. ACTUALIZADOS EN ret_ctr_pago_det     : ", li_cont
             USING "<<<,<<&" AT 16,10
    
    LET gar_tablas_rev[9].tabla     = "ret_ctr_pago_det"
    LET gar_tablas_rev[9].accion    = "ACTUALIZA"
    LET gar_tablas_rev[9].num_regs  = li_cont    
    
    -----------------------------------------------------------------------------

    DELETE
    FROM   ret_consar_parciales
    WHERE  folio_liquida   = pr_datos.folio
    
    LET li_cont = SQLCA.SQLERRD[3]
    
    DISPLAY "R. ELIMINADOS EN ret_consar_parciales   : ", li_cont
            USING "<<<,<<&" AT 17,10

    LET gar_tablas_rev[10].tabla     = "ret_consar_parciales"
    LET gar_tablas_rev[10].accion    = "BORRADA"
    LET gar_tablas_rev[10].num_regs  = li_cont

    -----------------------------------------------------------------------------

    DELETE
    FROM   ret_consar_par_liquida
    WHERE  folio_liquida   = pr_datos.folio
    
    LET li_cont = SQLCA.SQLERRD[3]
    
    DISPLAY "R. ELIMINADOS EN ret_consar_par_liquida : ", li_cont
            USING "<<<,<<&" AT 18,10

    LET gar_tablas_rev[11].tabla     = "ret_consar_par_liquida"
    LET gar_tablas_rev[11].accion    = "BORRADA"
    LET gar_tablas_rev[11].num_regs  = li_cont

    -----------------------------------------------------------------------------

    DELETE
    FROM   ret_marca_pensionado
    WHERE  folio    = pr_datos.folio
    
    LET li_cont = SQLCA.SQLERRD[3]
    
    DISPLAY "R. ELIMINADOS EN ret_marca_pensionado : ", li_cont
            USING "<<<,<<&" AT 19,10

    LET gar_tablas_rev[12].tabla     = "ret_marca_pensionado"
    LET gar_tablas_rev[12].accion    = "BORRADA"
    LET gar_tablas_rev[12].num_regs  = li_cont

    -----------------------------------------------------------------------------


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

    FOR i = 1 TO 12
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
        folio           LIKE ret_solicitud_tx.folio
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

    SELECT "OK"
    FROM   dis_cuenta
    WHERE  folio            = lr_datos.folio
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        LET lc_error = "    EL ESTADO DEBE SER 8 - LIQUIDADO "
        LET ls_id = 1
    END IF

    RETURN ls_id, lc_error

END FUNCTION


