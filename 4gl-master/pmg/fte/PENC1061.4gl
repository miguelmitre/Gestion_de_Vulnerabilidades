################################################################################
#Proyecto          => SISTEMA DE AFORES( SAFRE )                               #
#Owner             => E.F.P.                                                   #
#Programa PENC1061 => REVERSO DE LIQUIDACION RETIROS POR PMG                   #
#Fecha creacion    => 9 DE ABRIL DE 2010                                       #
#By                => JAVIER GONZALEZ JERONIMO                                 #
#Actualizacion     => ISAI JIMENEZ ROJAS 24-OCT-2013    v1.2                   #
#                  => Se incorpora reverso de tablas de folios generados       #
#Sistema           => RET                                                      #
################################################################################
DATABASE safre_af

GLOBALS

    DEFINE gr_bitacora RECORD LIKE ret_bitacora_rev.*

    DEFINE gr_edo RECORD
        preliquidado    LIKE pen_estado_pmg.estado_solicitud    ,
        liquidado       LIKE pen_estado_pmg.estado_solicitud
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

    CALL STARTLOG("PENC1061.log")
    CALL init()#i
    
    CALL f_captura_datos() RETURNING gs_procesa, gr_datos.*
    
    IF gs_procesa THEN 
        CALL f_reverso(gr_datos.*)
        CALL f_act_bitacora(gr_datos.*)
        PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR CHAR enter
    END IF     
 
    CLOSE WINDOW penc1061

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

    LET gr_bitacora.programa      = "PENC1061"
    LET gr_bitacora.desc_tramite  = "LIQUIDACION PMG"
    LET gr_bitacora.fecha_ini     = HOY
    LET gr_bitacora.hora_ini      = CURRENT HOUR TO SECOND
    
    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.preliquidado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "PRELIQUIDADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.liquidado
    FROM   pen_estado_pmg A
    WHERE  A.descripcion = "LIQUIDADO"


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

    SELECT MAX(folio)
    INTO   lr_captura.folio_liquida
    FROM   pen_preliquida_pmg
    WHERE  estado_pmg = gr_edo.liquidado

    OPEN WINDOW penc1061 AT 4,4 WITH FORM "PENC10611" ATTRIBUTE(BORDER)
    DISPLAY " <Ctrl-C> Salir                                                                " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY "                                                                          v1.2 " AT 2,1  
    DISPLAY " PENC1061          REVERSO DE LIQUIDACION DE RETIROS PMG                       " AT 3,1 ATTRIBUTE(REVERSE)
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
                    WHERE  folio            = lr_captura.folio_liquida
                    AND    tipo_movimiento  = 841
                    AND    estado_pmg       = gr_edo.liquidado

                    DISPLAY "Fecha de Liquidacion :        ", ldt_fecha_liquida AT 7,17
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
                    WHERE  folio            = lr_captura.folio_liquida
                    AND    tipo_movimiento  = 841
                    AND    estado_pmg       = gr_edo.liquidado

                    DISPLAY "Fecha de Liquidacion :        ", ldt_fecha_liquida AT 7,16
                END IF            
            END IF

            WHILE TRUE
                PROMPT "¿ EJECUTAR REVERSO DE LA LIQUIDACION DE PMG ? (S/N) : " FOR CHAR enter
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

    DEFINE 
        li_cont             INTEGER
    #----------------------------------------------------------------------------
        
     LET li_cont     = 0
    DISPLAY " PROCESANDO INFORMACION " AT 19,2 ATTRIBUTE(REVERSE)

    --------------------------------
    --ELIMINA MOVIMIENTOS LIQUIDADOS
    --------------------------------
    DELETE
    FROM   dis_cuenta
    WHERE  folio = pr_datos.folio

    LET li_cont = SQLCA.SQLERRD[3]
    
    DISPLAY "R. ELIMINADOS EN dis_cuenta               : ", li_cont
            USING "<<<,<<&" AT 9,10

    LET gar_tablas_rev[1].tabla     = "dis_cuenta"
    LET gar_tablas_rev[1].accion    = "BORRADA"
    LET gar_tablas_rev[1].num_regs  = li_cont

    -------------------------------
    --ACTUALIZA SOLICITUDES DE PMG 
    -------------------------------
    UPDATE pen_preliquida_pmg
    SET    estado_pmg   = gr_edo.preliquidado
    WHERE  folio        = pr_datos.folio
    AND    estado_pmg   = gr_edo.liquidado

    LET li_cont = SQLCA.SQLERRD[3]

    DISPLAY "R. ACTUALIZADOS EN pen_preliquida_pmg     : ", li_cont
             USING "<<<,<<&" AT 10,10
    
    LET gar_tablas_rev[2].tabla     = "pen_ctr_pago"
    LET gar_tablas_rev[2].accion    = "ACTUALIZA"
    LET gar_tablas_rev[2].num_regs  = li_cont    

    
    ----------------------------------
    --REVERSA LOS FOLIOS GENERADOS
    ----------------------------------
    --CALL f_reversa_folios(pr_datos.folio)
    
END FUNCTION

#==============================================================================#
#Objetivo : reversar folios de pago generados en la liquidacion                #
#Autor    : Isai Jimenez Rojas                                                 #
#==============================================================================#
{
FUNCTION f_reversa_folios(p_folio_liquida)
   DEFINE p_folio_liquida          DECIMAL(10,0)
   DEFINE lr_ret_folio_pago_pmg    RECORD LIKE ret_folio_pago_pmg.*
   DEFINE li_contador1             INTEGER
   DEFINE li_contador2             INTEGER
   DEFINE li_contador3             INTEGER
   
   ---------------------------------------------------------------------------
   LET li_contador1 = 0
   LET li_contador2 = 0
   LET li_contador3 = 0
      
   DECLARE cur_folio_pago
   CURSOR FOR
   SELECT * FROM ret_folio_pago_pmg
   WHERE  folio_liquida = p_folio_liquida
   
   FOREACH cur_folio_pago INTO lr_ret_folio_pago_pmg.*
      
      --REGRESA EL FOLIO DE PAGO QUE SE TENIA ANTES DE LA LIQUIDACION
      UPDATE ret_folio_pago   
      SET    folio_pago   = lr_ret_folio_pago_pmg.folio_pago_ant
      WHERE  nss          = lr_ret_folio_pago_pmg.nss
      AND    consecutivo  = lr_ret_folio_pago_pmg.consecutivo
      AND    consec_benef = lr_ret_folio_pago_pmg.consec_benef
      
      IF SQLCA.SQLCODE = 0 THEN 
         LET li_contador1 = li_contador1 + SQLCA.SQLERRD[3]
      END IF
      
      --SE ELIMINA EL FOLIO DE PAGO GENERADO DURANTE LA LIQUIDACION
      DELETE FROM ret_ctr_folio_pago
      WHERE  folio_pago    = lr_ret_folio_pago_pmg.folio_pago_act
      
      LET li_contador2 = li_contador2 + SQLCA.SQLERRD[3]
      
   END FOREACH

   DISPLAY "R. ACTUALIZADOS EN ret_folio_pago         : ", li_contador1
             USING "<<<,<<&" AT 11,10
   
   DISPLAY "R. ELIMINADOS EN ret_ctr_folio_pago       : ", li_contador2
             USING "<<<,<<&" AT 12,10
             
   --ELIMINA LOS REGISTRROS DE FOLIO DE PAGO GENERADOS PARA EL FOLIO DE LIQUIDA
   DELETE FROM ret_folio_pago_pmg
   WHERE  folio_liquida = p_folio_liquida
   
   LET li_contador3 = SQLCA.SQLERRD[3]
   
   DISPLAY "R. ELIMINADOS EN ret_folio_pago_pmg       : ", li_contador3
             USING "<<<,<<&" AT 13,10
   
   
   
END FUNCTION 
}
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

    FOR i = 1 TO 2
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
    WHERE  folio        = lr_datos.folio
    AND    estado_pmg   = gr_edo.liquidado
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        LET lc_error = "EL FOLIO INGRESADO NO ESTA LIQUIDADO "
        LET ls_id = 1
    END IF

    RETURN ls_id, lc_error

END FUNCTION


