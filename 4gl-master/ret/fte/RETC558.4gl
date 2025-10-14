#################################################################################
#Proyecto          => SISTEMA DE AFORE( SAFRE )                                 #
#Owner             => E.F.P.                                                    #
#Programa RETC558  => REINVERSION DE RECURSOS DE DISPOSICIONES AFECTADAS POR EL #
#                     PROCESO DE VENTANILLA 2.5 (PROCESO CONTINGENTE)           #
#Fecha creacion    => 22 DE MAYO DE 2009                                        #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Sistema           => RET                                                       #
#################################################################################
#Requerimiento     => CPL-1060                                                  #
#Fecha y Autor     => 08-Nov-2012  Alejandro Chagoya salazar                    #
#Descripcion       => Se agrega tipo retiro S a los queries de validacion y     #
#                     actualizacion de datamart                                 #
---------------------------------------------------------------------------------
#Requerimiento     => CPL-1041                                                  #
#Fecha y Autor     => 29-Nov-2012  Alejandro Chagoya salazar                    #
#Descripcion       => Se agrega valiadacion para la siefore del regimen         #
---------------------------------------------------------------------------------
#Requerimiento     => CPL-1092                                                  #
#Fecha y Autor     => 10-Dic-2012  Alejandro Chagoya salazar                    #
#Descripcion       => Se elimina query de dis_cuenta para la siefore            #
#################################################################################

DATABASE safre_af

GLOBALS

    DEFINE gar_precio_acc ARRAY [20] OF RECORD #Arreglo para los precios_accion
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE gr_edo RECORD #gr_edo
        liquidado               LIKE ret_estado.estado_solicitud  ,
        derecho_otorgado        LIKE ret_estado.estado_solicitud  ,
        reinvertido             LIKE ret_estado.estado_solicitud  
    END RECORD

    DEFINE 
        gs_folio_cargo          ,
        gs_folio_abono          LIKE ret_det_datamart.folio

    DEFINE #glo #char
        gc_query                CHAR(1000),
        gc_usuario              CHAR(012) ,
        enter                   CHAR(001)

    DEFINE #glo #date
        gr_fecha                ,
        HOY                     DATE

    DEFINE 
        gs_codigo_afore         ,
        gs_salida               SMALLINT 

END GLOBALS


MAIN

    DEFINE mr_datamart RECORD LIKE ret_det_datamart.*

    DEFINE mr_saldo RECORD 
        nss             LIKE ret_det_datamart.nss           ,
        siefore         SMALLINT                            ,
        grupo           SMALLINT                            ,
        fecha           DATE
    END RECORD

    DEFINE mr_montos RECORD
        subcuenta           SMALLINT     ,
        siefore             SMALLINT     ,
        monto_acc           LIKE dis_cuenta.monto_en_acciones,
        monto_pesos         LIKE dis_cuenta.monto_en_pesos
    END RECORD
    
    DEFINE
        ms_cont                     ,
        ms_siefore_orig             SMALLINT
    

    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP          ,
        PROMPT LINE LAST    ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("RETC558.log")

    LET ms_cont = 0
    CALL init()


    CALL f_captura_fechas() RETURNING gr_fecha, gs_salida    

    IF gs_salida = 0 THEN
        
        DISPLAY "REGISTROS REINVERTIDOS  : ", ms_cont AT 11,9
        
        LET gc_query = " SELECT *                                   ",             
                       " FROM   ret_det_datamart                    ",          
                       " WHERE  id_registro IS NOT NULL             ",          
                       " AND    tipo_retiro IN ('D','S')            ",          
                       " AND    cast(fecha_carga_afore AS DATE) = ? ",
                       " AND    nss IN (                            ",          
                       " SELECT nss                                 ",          
                       " FROM   ret_solicitud_saldo                 ",          
                       " WHERE  estado_solicitud IN (?,?) )         ",          
                       " ORDER BY nss                               "         
        
        PREPARE prp_nss FROM gc_query
        DECLARE cur_nss CURSOR FOR prp_nss

        --- SALDO AL DIA ---
        LET gc_query = " EXECUTE FUNCTION fn_saldo_dia (?,?,?,?)"
        PREPARE eje_saldo_dia FROM gc_query
        DECLARE cur_mto CURSOR FOR eje_saldo_dia
            
        FOREACH cur_nss USING gr_fecha                  ,
                              gr_edo.liquidado          ,
                              gr_edo.derecho_otorgado
                        INTO  mr_datamart.*

            LET ms_cont = ms_cont + 1
            


            INITIALIZE mr_montos.* TO NULL
            LET ms_siefore_orig     = 0
            LET mr_saldo.nss        = mr_datamart.nss
            LET mr_saldo.siefore    = 0
            LET mr_saldo.grupo      = 0                     
            LET mr_saldo.fecha      = HOY

{#CPL-1092  se comenta if
            SELECT UNIQUE(siefore)       
            INTO   ms_siefore_orig
            FROM   dis_cuenta            
            WHERE  nss              = mr_datamart.nss
            AND    tipo_movimiento  = 921 
            AND    subcuenta NOT IN (4,8)

            IF ms_siefore_orig <> 0 OR ms_siefore_orig IS NOT NULL THEN
}
                FOREACH cur_mto USING mr_saldo.* INTO mr_montos.*
                    
                    IF mr_montos.siefore = 10 THEN

                    #CPL-1041 INI
                       SELECT codigo_siefore INTO mr_montos.siefore
                        FROM  cta_regimen
                        WHERE nss       = mr_datamart.nss
                        AND   subcuenta = mr_montos.subcuenta

                      IF STATUS = NOTFOUND THEN
                         DISPLAY " NO EXISTE SIEFORE PARA EL NSS:  ", mr_datamart.nss,
                                " SUBCUENTA: ",mr_montos.subcuenta USING "<<<<<"
                         PROMPT  " <ENTER> PARA CONTINUAR " FOR CHAR enter
                         CONTINUE FOREACH
                      END IF
                    #CPL-1041 FIN

                        -- inserta cargo a la siefore 10
                        CALL f_inserta_dis_cuenta(923                       ,
                                                  mr_montos.subcuenta       ,
                                                  10                        ,
                                                  gs_folio_cargo            ,
                                                  mr_datamart.id_registro   ,
                                                  mr_datamart.nss           ,
                                                  mr_datamart.curp          ,
                                                  mr_montos.monto_pesos
                                                 )

                        -- inserta abono a la siefore original
                        CALL f_inserta_dis_cuenta(924                       ,
                                                  mr_montos.subcuenta       ,
                                                  mr_montos.siefore          ,
                                                  gs_folio_abono            ,
                                                  mr_datamart.id_registro   ,
                                                  mr_datamart.nss           ,
                                                  mr_datamart.curp          ,
                                                  mr_montos.monto_pesos
                                                 )
                    END IF
                END FOREACH
#            END IF #CPL-1092  se comenta if

            SQL
            UPDATE ret_det_datamart
            SET    folio                            = $gs_folio_cargo
            WHERE  tipo_retiro                      IN("D","S")   #CPL-1060
            AND    CAST(fecha_carga_afore AS DATE)  = $gr_fecha
            AND    nss                              = $mr_datamart.nss
            AND    id_registro                      = $mr_datamart.id_registro
            END SQL    
            
            UPDATE ret_solicitud_saldo
            SET    estado_solicitud                 = gr_edo.reinvertido
            WHERE  nss                              = mr_datamart.nss
            AND estado_solicitud IN(106,120)    ##liquidado saldo, cancelado

            CALL f_desmarca_cuenta(mr_datamart.nss)
            DISPLAY "REGISTROS REINVERTIDOS  : ", ms_cont AT 11,9
            DISPLAY "FOLIO DE REINVERSION CARGO  : ", gs_folio_cargo AT 13,10
            DISPLAY "FOLIO DE REINVERSION ABONO  : ", gs_folio_abono AT 14,10

        END FOREACH
    END IF

    PROMPT " PROCESO TERMINADO... < ENTER > PARA SALIR " FOR CHAR enter
    CLOSE WINDOW retc5581
END MAIN 

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    DEFINE
        lc_prepare      CHAR(300)

    -- -----------------------------------------------------------------------------

    LET HOY                 = TODAY
    LET gs_salida           = 0

    SELECT codigo_afore   ,
           USER
    INTO   gs_codigo_afore ,
           gc_usuario
    FROM   tab_afore_local

    --- ESTADOS DE SOLICITUD ---
    SELECT A.estado_solicitud
    INTO   gr_edo.liquidado
    FROM   ret_estado A
    WHERE  A.descripcion = "LIQUIDADO SALDO"

    SELECT A.estado_solicitud
    INTO   gr_edo.derecho_otorgado
    FROM   ret_estado A
    WHERE  A.descripcion = "DERECHO OTORGADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.reinvertido
    FROM   ret_estado A
    WHERE  A.descripcion = "REINVERTIDO"

END FUNCTION

#---------------------------------------------------------------------------#
# f_captura_fechas : Captura el intervalo de fechas para realizar la        #
#                    de registros a liquidar                                #
#---------------------------------------------------------------------------#
FUNCTION f_captura_fechas()

    DEFINE lr_fechas RECORD
        carga           DATE
    END RECORD

    DEFINE
        ls_exit        ,
        ls_estado      SMALLINT

    DEFINE
        lc_mensaje     CHAR(100)

    -- -----------------------------------------------------------------------------

    OPEN WINDOW retc5581 AT 4,4 WITH FORM "RETC5581" ATTRIBUTE(BORDER)
    DISPLAY " <ESC> - Ejecutar                                          <CTRL-C> - Salir  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC558   REINVERSION DE DISPOSICIONES - CONTINGENTE VENT 2.5               " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

    LET ls_exit         = 0
    LET lr_fechas.carga = HOY

    DISPLAY BY NAME lr_fechas.carga

    INPUT BY NAME lr_fechas.* WITHOUT DEFAULTS

        AFTER FIELD carga
            CALL f_valida_fechas(lr_fechas.carga)
                RETURNING ls_estado, lc_mensaje

            IF ls_estado <> 0 THEN
                ERROR lc_mensaje CLIPPED ATTRIBUTE(REVERSE)
                SLEEP 2
                ERROR ""
                NEXT FIELD carga
            END IF

        ON KEY(ESC)
            LET ls_exit = f_valida_ejecucion(lr_fechas.*)
            EXIT INPUT

        ON KEY (CONTROL-C)
            PROMPT " PROCESO TERMINADO... < ENTER > PARA SALIR " FOR CHAR enter
            LET ls_exit = 1
            EXIT INPUT

        ON KEY (INTERRUPT)
            PROMPT " PROCESO TERMINADO... < ENTER > PARA SALIR " FOR CHAR enter
            LET ls_exit = 1
            EXIT INPUT
    END INPUT

    RETURN lr_fechas.*, ls_exit

END FUNCTION

#---------------------------------------------------------------------------#
# f_inserta_dis_cuenta :                                                    #
#                                                                           # 
#---------------------------------------------------------------------------#
FUNCTION f_inserta_dis_cuenta(pr_datos)

    DEFINE pr_datos RECORD                      
        tipo_movimiento             LIKE dis_cuenta.tipo_movimiento     ,  
        subcuenta                   LIKE dis_cuenta.subcuenta           ,
        siefore                     LIKE dis_cuenta.siefore             ,
        folio                       LIKE dis_cuenta.folio               ,
        consecutivo_lote            LIKE dis_cuenta.consecutivo_lote    ,
        nss                         LIKE dis_cuenta.nss                 ,
        curp                        LIKE dis_cuenta.curp                ,
        monto_en_pesos              LIKE dis_cuenta.monto_en_pesos
    END RECORD
    
    DEFINE lr_dis_cuenta RECORD LIKE dis_cuenta.*

    DEFINE
        ls_siefore              ,
        ls_factor               SMALLINT

    -- -----------------------------------------------------------------------------
    
    INITIALIZE lr_dis_cuenta.* TO NULL
                
    SELECT tipo  
    INTO   ls_factor
    FROM   tab_movimiento
    WHERE  codigo = pr_datos.tipo_movimiento

    LET ls_siefore                          = pr_datos.siefore
    LET lr_dis_cuenta.precio_accion         = gar_precio_acc[ls_siefore].precio_dia
    LET lr_dis_cuenta.monto_en_pesos        = pr_datos.monto_en_pesos * ls_factor
    LET lr_dis_cuenta.monto_en_acciones     = (pr_datos.monto_en_pesos / lr_dis_cuenta.precio_accion) * ls_factor

    LET lr_dis_cuenta.tipo_movimiento       = pr_datos.tipo_movimiento 
    LET lr_dis_cuenta.subcuenta             = pr_datos.subcuenta       
    LET lr_dis_cuenta.siefore               = pr_datos.siefore         
    LET lr_dis_cuenta.folio                 = pr_datos.folio           
    LET lr_dis_cuenta.consecutivo_lote      = pr_datos.consecutivo_lote
    LET lr_dis_cuenta.nss                   = pr_datos.nss             
    LET lr_dis_cuenta.curp                  = pr_datos.curp            

    LET lr_dis_cuenta.fecha_pago            = HOY
    LET lr_dis_cuenta.fecha_valor           = HOY
    LET lr_dis_cuenta.fecha_conversion      = HOY

    LET lr_dis_cuenta.dias_cotizados        = 0
    LET lr_dis_cuenta.id_aportante          = "REINV-D"
    LET lr_dis_cuenta.estado                = 6
    LET lr_dis_cuenta.fecha_proceso         = HOY
    LET lr_dis_cuenta.usuario               = gc_usuario
    LET lr_dis_cuenta.fecha_archivo         = HOY
    LET lr_dis_cuenta.etiqueta              = 1

    INSERT INTO dis_cuenta
    VALUES (lr_dis_cuenta.*)


END FUNCTION


#---------------------------------------------------------------------------#
# f_obtiene_precios_accion : Obtiene los precios de accion correspondientes #
#                            a la fecha indicada                            #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_precios_accion(fecha_precios)

    DEFINE #loc #date
        fecha_precios         DATE

    DEFINE #loc #char
        v_precios_accion      CHAR(100)

    DEFINE lr_precio_acc RECORD #loc #lr_precio_acc Arreglo para los precios_accion
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE #loc #char
        lc_mensaje            CHAR(100) ,
        lc_siefore            CHAR(002)

    DEFINE #loc #smallint
        li_cont               SMALLINT

    -- -----------------------------------------------------------------------------

    LET li_cont = 0

    LET v_precios_accion = " EXECUTE FUNCTION fn_verifica_precio_accion (?)"
    PREPARE eje_precios_accion FROM v_precios_accion

    DECLARE c_precios CURSOR FOR eje_precios_accion
    FOREACH c_precios USING fecha_precios
                      INTO lr_precio_acc.*

        -- Solo se valida que existan los precios de las siefores basicas (1 a 5)
        IF (lr_precio_acc.siefore <= 10 AND lr_precio_acc.estado <> 0) THEN
            LET lc_siefore = lr_precio_acc.siefore
            LET lc_mensaje = " FALTAN PRECIOS ACCION: DIA ", fecha_precios, ", SIEFORE: ", lc_siefore, " ... <ENTER> PARA SALIR " CLIPPED
            LET lc_mensaje = lc_mensaje CLIPPED
            PROMPT lc_mensaje FOR CHAR enter
            EXIT PROGRAM
        ELSE
            -- Se almacena cada precio de accion el lugar correspondiente
            -- dentro del arreglo de precios.
            LET li_cont                   = lr_precio_acc.siefore
            LET gar_precio_acc[li_cont].* = lr_precio_acc.*
        END IF

    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_fechas : Valida que la fecha capturada no sea nula y que cumpla  #
#                   las condiciones necesarias para ser aceptada            #
#---------------------------------------------------------------------------#
FUNCTION f_valida_fechas(p_fecha)

    DEFINE
        p_fecha     DATE

    DEFINE
        estado      SMALLINT

    DEFINE
        mensaje     CHAR(100)

    LET estado = 0

    IF p_fecha IS NULL THEN
        LET mensaje = "LA FECHA NO DEBE SER NULA ... "
        LET estado = 1
    ELSE
        IF p_fecha < "01/01/1980" THEN
            LET mensaje = "FECHA INVALIDA ... "
            LET estado = 1
        END IF
    END IF

    RETURN estado, mensaje

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_ejecucion : Realiza las validaciones previas a la ejecucion      #
#                      de la liquidacion                                    #
#---------------------------------------------------------------------------#
FUNCTION f_valida_ejecucion(pr_fechas)

    DEFINE pr_fechas RECORD
        carga           DATE
    END RECORD

    DEFINE
        ls_exit        SMALLINT

    DEFINE
        li_regs        INTEGER

    DEFINE
        lc_query        CHAR(1000)

    -- -----------------------------------------------------------------------------

    LET li_regs = 0
    LET ls_exit = 0

    -- Validamos y obtenemos precios de accion
    CALL f_obtiene_precios_accion(HOY)

    -- Validamos que existan registros
    SQL
    SELECT "OK"
    FROM   ret_det_datamart
    WHERE  id_registro IS NOT NULL 
    AND    tipo_retiro IN ("D","S")   #CPL-1060
    AND    cast(fecha_carga_afore AS DATE) = $pr_fechas.carga
    AND    nss IN 
    (
    SELECT nss
    FROM   ret_solicitud_saldo
    WHERE  estado_solicitud IN ($gr_edo.liquidado       , 
                                $gr_edo.derecho_otorgado)
    )
    GROUP BY 1
    END SQL

    IF STATUS = NOTFOUND THEN
        PROMPT " NO EXISTEN REGISTROS PARA LA FECHA CAPTURADA ...<ENTER> PARA SALIR "
        FOR CHAR enter
        LET ls_exit = 1
    ELSE
        SELECT MAX(folio) + 1
        INTO   gs_folio_cargo
        FROM   glo_folio
        
        INSERT INTO glo_folio
        VALUES (gs_folio_cargo)

        SELECT MAX(folio) + 1
        INTO   gs_folio_abono
        FROM   glo_folio
        
        INSERT INTO glo_folio
        VALUES (gs_folio_abono)
        
    END IF

    RETURN ls_exit

END FUNCTION

#---------------------------------------------------------------------------#
# f_desmarca_cuenta : Ejecuta el SPL que realiza la desmarca de la cuenta   #
#---------------------------------------------------------------------------#
FUNCTION f_desmarca_cuenta(pc_nss)

    DEFINE 
        pc_nss         CHAR(11)

    DEFINE lr_marca RECORD LIKE cta_act_marca.*

    DEFINE lr_dat RECORD
        edo_marca       SMALLINT ,
        marca_causa     SMALLINT
    END RECORD

    DEFINE
        lc_prepare      CHAR(100)

    ----- DESMARCA -----
    LET lc_prepare  = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"
    PREPARE eje_desmarca FROM lc_prepare

    LET lr_dat.edo_marca    = 0
    LET lr_dat.marca_causa  = 0

    SELECT *
    INTO   lr_marca.*
    FROM   cta_act_marca
    WHERE  nss          = pc_nss
    AND    marca_cod    = 921
    
    IF STATUS <> NOTFOUND THEN

        EXECUTE eje_desmarca USING lr_marca.nss             ,--nss
                                   lr_marca.marca_cod       ,--marca entrante
                                   lr_marca.correlativo     ,--consecutivo
                                   lr_dat.edo_marca         ,--estado_marco
                                   lr_dat.marca_causa       ,--marca_causa
                                   gc_usuario                --usuario
    END IF
    
END FUNCTION
