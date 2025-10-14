#################################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                               #
#Owner             => E.F.P.                                                    #
#Programa RETM809b => FUNCIONES GENERALES DEL MODULO DE ADMINISTRACION DE       #
#                     SOLICITUDES DE RETIROS PARCIALES (RETM809.4gl)            #
#Fecha creacion    => 04 DE MAYO DE 2010                                        #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Sistema           => RET                                                       #
#################################################################################

GLOBALS "RETM809a.4gl"

#---------------------------------------------------------------------------#
# f_error_msg : Formatea y despliega los mensajes de error en la pantalla   #
#---------------------------------------------------------------------------#
FUNCTION f_error_msg(pc_mensaje)

    DEFINE
        pc_mensaje          CHAR(75)

    LET pc_mensaje = " ", pc_mensaje CLIPPED , "... <ENTER> PARA SEGUIR "
    PROMPT pc_mensaje ATTRIBUTE(REVERSE) FOR CHAR enter

END FUNCTION

#---------------------------------------------------------------------------#
# f_ejecuta_benef : Ejecuta el programa RETM810 para la captura de          #
#                   beneficiarios del nss actual                            #
#---------------------------------------------------------------------------#
FUNCTION f_ejecuta_benef(pr_benef)

    DEFINE pr_benef RECORD
        nss         LIKE ret_parcial.nss          ,
        consec      LIKE ret_parcial.consecutivo  ,
        id_oper     CHAR
    END RECORD

    DEFINE
        lc_ejecuta             CHAR(0200)

    LET lc_ejecuta = "fglgo RETM810 ", pr_benef.nss      CLIPPED ," ",
                                       pr_benef.consec   CLIPPED ," ",
                                       pr_benef.id_oper
    RUN lc_ejecuta


END FUNCTION

#---------------------------------------------------------------------------#
# f_obtiene_ult_consec : Obtiene el ultimo consecutivo a insertarse         #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_ult_consec()

    DEFINE li_consecutivo LIKE ret_parcial.consecutivo

    DEFINE
        lc_prepare      CHAR(300)
        
    -- -----------------------------------------------------------------------------
    
    LET lc_prepare = " EXECUTE FUNCTION fn_obten_ret_consecutivo() "
    PREPARE eje_consecutivo FROM lc_prepare
    EXECUTE eje_consecutivo INTO li_consecutivo
    
    RETURN li_consecutivo

END FUNCTION

#---------------------------------------------------------------------------#
# f_obtiene_precios_accion : Obtiene los precios de accion del dia dado     #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_precios_accion(pdt_fecha)

    DEFINE
        pdt_fecha               DATE

    DEFINE lr_precio_acc RECORD
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE
        lc_eje_precios          CHAR(100) ,
        lc_mensaje              CHAR(100) ,
        lc_siefore              CHAR(002)

    DEFINE
        li_cont                 SMALLINT

    -- -----------------------------------------------------------------------------

    LET li_cont = 0

    LET lc_eje_precios = " EXECUTE FUNCTION fn_verifica_precio_accion (?)"
    PREPARE eje_precios_accion FROM lc_eje_precios

    DECLARE c_precios CURSOR FOR eje_precios_accion
    FOREACH c_precios USING pdt_fecha
                      INTO  lr_precio_acc.*

        -- Solo se valida que existan los precios de las siefores basicas (1 a 5)
        IF (lr_precio_acc.siefore < 10 AND lr_precio_acc.estado <> 0) THEN
            LET lc_siefore = lr_precio_acc.siefore
            LET lc_mensaje = " FALTAN PRECIOS ACCION: DIA ", pdt_fecha, ", SIEFORE: ", lc_siefore, " ... <ENTER> PARA SALIR " CLIPPED
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
# f_obtiene_siefore_act : Obtiene la siefore actual en la que el nss se     #
#                         encuentra invirtiendo sus recursos                #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_siefore_act(pc_nss)

    DEFINE pc_nss LIKE ret_parcial.nss

    DEFINE ls_sie_act LIKE cta_nss_regimen.codigo_siefore

    SELECT codigo_siefore
    INTO   ls_sie_act
    FROM   cta_nss_regimen
    WHERE  grupo_regimen = 1
    AND    nss           = pc_nss

    RETURN ls_sie_act

END FUNCTION

#---------------------------------------------------------------------------#
# f_obtiene_anio : Arma el año en formato YYYY que corresponde a la cadena  #
#                  enviada en formato YY                                    #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_anio(pc_anio)

    DEFINE
        pc_anio             CHAR(2)

    DEFINE
        ls_anio             SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_anio = pc_anio

    IF pc_anio[1] = "0" THEN
        LET ls_anio = ls_anio + 2000
    ELSE
        LET ls_anio = ls_anio + 1900
    END IF
    
    RETURN ls_anio
    
END FUNCTION

#---------------------------------------------------------------------------#
# f_obten_rcv : Obtiene los montos de RCV del nss en la fecha dada          #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_rcv(p_nss, pdt_fecha)

    DEFINE p_nss        LIKE ret_parcial.nss

    DEFINE ls_sie       LIKE dis_provision.siefore

    DEFINE lr_rcv RECORD
        ret_97          DECIMAL(16,2),
        ces_vej         DECIMAL(16,2),
        cuota_soc       DECIMAL(16,2),
        tot_sub_rcv     DECIMAL(16,2)
    END RECORD

    DEFINE
        pdt_fecha           DATE

    DEFINE 
        ld_monto            DECIMAL(16,2)

    -- -----------------------------------------------------------------------------

    LET ld_monto            = 0
    LET lr_rcv.ret_97       = 0
    LET lr_rcv.ces_vej      = 0
    LET lr_rcv.cuota_soc    = 0
    LET lr_rcv.tot_sub_rcv  = 0

    DECLARE cur_rcv_01 CURSOR FOR
    SELECT UNIQUE(siefore)
    FROM   dis_cuenta
    WHERE  nss              = p_nss
    AND    subcuenta        = 1
    AND    fecha_conversion <= pdt_fecha
    AND    tipo_movimiento  > 0

    FOREACH cur_rcv_01 INTO ls_sie

        -- Se obtienen los montos que se insertaran en la tabla
        SELECT SUM(monto_en_acciones) * gar_precio_acc[ls_sie].precio_dia
        INTO   ld_monto
        FROM   dis_cuenta
        WHERE  nss              = p_nss
        AND    siefore          = ls_sie
        AND    subcuenta        = 1
        AND    fecha_conversion <= pdt_fecha
        AND    tipo_movimiento  > 0

        IF ld_monto IS NULL THEN
           LET ld_monto = 0
        END IF

        LET lr_rcv.ret_97 = lr_rcv.ret_97 + ld_monto

    END FOREACH

    LET ld_monto = 0

    DECLARE cur_rcv_02 CURSOR FOR
    SELECT UNIQUE(siefore)
    FROM   dis_cuenta
    WHERE  nss              = p_nss
    AND    subcuenta        IN (2,6,9)
    AND    fecha_conversion <= pdt_fecha
    AND    tipo_movimiento  > 0

    FOREACH cur_rcv_02 INTO ls_sie

        SELECT SUM(monto_en_acciones) * gar_precio_acc[ls_sie].precio_dia
        INTO   ld_monto
        FROM   dis_cuenta
        WHERE  nss              = p_nss
        AND    siefore          = ls_sie
        AND    subcuenta        IN (2,6,9)
        AND    fecha_conversion <= pdt_fecha
        AND    tipo_movimiento  > 0

        IF ld_monto IS NULL THEN
           LET ld_monto = 0
        END IF

        LET lr_rcv.ces_vej  = lr_rcv.ces_vej + ld_monto

    END FOREACH

    LET ld_monto = 0

    DECLARE cur_rcv_03 CURSOR FOR
    SELECT UNIQUE(siefore)
    FROM   dis_cuenta
    WHERE  nss              = p_nss
    AND    subcuenta        = 5
    AND    fecha_conversion <= pdt_fecha
    AND    tipo_movimiento  > 0

    FOREACH cur_rcv_03 INTO ls_sie

        SELECT SUM(monto_en_acciones) * gar_precio_acc[ls_sie].precio_dia
        INTO   ld_monto
        FROM   dis_cuenta
        WHERE  nss              = p_nss
        AND    siefore          = ls_sie
        AND    subcuenta        = 5
        AND    fecha_conversion <= pdt_fecha
        AND    tipo_movimiento  > 0

        IF ld_monto IS NULL THEN
           LET ld_monto = 0
        END IF

        LET lr_rcv.cuota_soc = lr_rcv.cuota_soc + ld_monto

    END FOREACH

    LET lr_rcv.tot_sub_rcv = lr_rcv.ret_97 + lr_rcv.ces_vej + lr_rcv.cuota_soc

    RETURN lr_rcv.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_obtiene_sbc : Recupera los salarios base de cotizacion de la tabla      #
#                 ret_parcial_resol para el nss dado                        #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_sbc(pr_resol)

    DEFINE pr_resol RECORD
        nss         LIKE ret_parcial_resol.nss            ,
        num_resol   LIKE ret_parcial_resol.num_resolucion
    END RECORD


    DEFINE lr_sal RECORD
        retiro_a    LIKE ret_parcial_resol.salario_base_a ,
        retiro_b    LIKE ret_parcial_resol.salario_base_b
    END RECORD

    DEFINE ld_sal_base LIKE ret_parcial_resol.salario_base_a

    DEFINE ld_max_folio LIKE ret_parcial_resol.folio

    DEFINE 
        ld_rowid        DECIMAL(11,0)

    DEFINE
        lc_query        CHAR(300) ,
        lc_opcion       CHAR(30)

    -- -----------------------------------------------------------------------------

    SELECT MAX(folio)
    INTO   ld_max_folio
    FROM   ret_parcial_resol A
    WHERE  A.nss             = pr_resol.nss
    AND    A.num_resolucion  = pr_resol.num_resol

    SELECT MAX(ROWID)
    INTO   ld_rowid
    FROM   ret_parcial_resol A 
    WHERE  A.nss             = pr_resol.nss      
    AND    A.num_resolucion  = pr_resol.num_resol
    AND    A.folio           = ld_max_folio

    SELECT salario_base_a ,
           salario_base_b
    INTO   lr_sal.*
    FROM   ret_parcial_resol A
    WHERE  A.nss             = pr_resol.nss
    AND    A.num_resolucion  = pr_resol.num_resol
    AND    A.folio           = ld_max_folio
    AND    A.rowid           = ld_rowid

    IF lr_sal.retiro_a IS NULL THEN
        LET lr_sal.retiro_a = 0
    END IF

    IF lr_sal.retiro_b IS NULL THEN
        LET lr_sal.retiro_b = 0
    END IF

    RETURN lr_sal.*

END FUNCTION

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

#---------------------------------------------------------------------------#
# f_valida_nss : Valida que el nss ingresado sea correcto y pueda darse de  #   
#                alta una solicitud de retiro parcial                       #
#---------------------------------------------------------------------------#
FUNCTION f_valida_nss(pc_nss)

    DEFINE pc_nss LIKE ret_parcial.nss

    DEFINE lr_afi RECORD
        paterno     LIKE afi_mae_afiliado.paterno   ,
        materno     LIKE afi_mae_afiliado.materno   ,
        nombres     LIKE afi_mae_afiliado.nombres   ,
        rfc         LIKE afi_mae_afiliado.n_rfc     ,
        curp        LIKE afi_mae_afiliado.n_unico
    END RECORD

    DEFINE ldt_max_fecha LIKE ret_parcial.fecha_captura

    DEFINE
        lc_msg_err          CHAR(100)

    DEFINE
        ls_flag             SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_flag = 1

    -- Valida que el nss este afiliado
    SELECT paterno,
           materno,
           nombres,
           n_rfc,
           n_unico
    INTO   lr_afi.*
    FROM   afi_mae_afiliado
    WHERE  n_seguro = pc_nss
    GROUP  BY 1,2,3,4,5
    
    IF STATUS = NOTFOUND THEN
        LET ls_flag     = 0
        LET lc_msg_err  = "EL NSS NO ESTA AFILIADO EN LA AFORE"
    ELSE
        CALL f_valida_edo_sol(pc_nss) RETURNING ls_flag, lc_msg_err

        IF ls_flag <> 0 THEN
            -- Valida que el nss exista en la base de datos de parciales
            SELECT "OK"
            FROM   ret_parcial_resol
            WHERE  nss = pc_nss
            GROUP BY 1
            
            IF STATUS = NOTFOUND THEN
                SELECT "OK"
                FROM   ret_resol_alterno
                WHERE  nss = pc_nss
                GROUP BY 1
            
                IF STATUS = NOTFOUND THEN
                    SELECT "OK"
                    FROM   ret_resol_comp
                    WHERE  nss = pc_nss
                    GROUP BY 1
            
                    IF STATUS = NOTFOUND THEN
                        LET ls_flag = 0
                        LET lc_msg_err = "EL NSS NO EXISTE EN LA BASE DE DATOS DE PARCIALES"
                    END IF
                END IF -- No encontrado en ret_resol_alterno
            END IF -- No encontrado en ret_parcial_resol
        END IF -- NSS en estado de solicitud no permitido
    END IF -- NSS no afiliado

    RETURN lr_afi.*, ls_flag, lc_msg_err

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_edo_sol : Valida que el nss dado no tenga solicitudes previas en #
#                    estados donde no es permitida una captura              #
#---------------------------------------------------------------------------#
FUNCTION f_valida_edo_sol(pc_nss)

    DEFINE pc_nss LIKE ret_parcial.nss

    DEFINE
        lc_descripcion      CHAR(015) ,
        lc_mensage          CHAR(100)

    DEFINE
        ls_edo              ,
        ls_valido           SMALLINT

    -- -----------------------------------------------------------------------------
    
    LET ls_valido   = 1
    
    SELECT NVL(MAX(estado_solicitud),-500)
    INTO   ls_edo
    FROM   ret_parcial
    WHERE  nss = pc_nss

    CASE ls_edo
    
        WHEN -500
            LET ls_valido   = 1
            
        WHEN gr_edo.liquidado
            LET ls_valido   = 1

        WHEN gr_edo.rechazado
            LET ls_valido   = 1
        
        OTHERWISE
            LET ls_valido   = 0
        
    END CASE

    IF ls_valido = 0 THEN
        SELECT  descripcion
        INTO    lc_descripcion
        FROM    ret_estado
        WHERE   estado_solicitud = ls_edo
    
        LET lc_mensage = "EL NSS SE ENCUENTRA EN ESTADO ", lc_descripcion CLIPPED
    END IF
    
    RETURN ls_valido, lc_mensage
    
END FUNCTION


#---------------------------------------------------------------------------#
# f_valida_num_resol : Valida que el numero de resolucion exista en alguna  #
#                      de las tablas de retiros parciales                   #
#---------------------------------------------------------------------------#
FUNCTION f_valida_num_resol(pr_datos)

    DEFINE pr_datos RECORD
        nss         LIKE ret_parcial.nss,
        num_resol   LIKE ret_parcial.num_resolucion
    END RECORD

    DEFINE
        ls_flag             SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_flag = 0

    SELECT "OK"
    FROM   ret_parcial_resol
    WHERE  nss            = pr_datos.nss
    AND    num_resolucion = pr_datos.num_resol
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        SELECT "OK"
        FROM   ret_resol_alterno
        WHERE  nss            = pr_datos.nss
        AND    num_resolucion = pr_datos.num_resol
        GROUP BY 1

        IF STATUS = NOTFOUND THEN
            SELECT "OK"
            FROM   ret_resol_comp
            WHERE  nss = pr_datos.nss
            GROUP BY 1

            IF STATUS = NOTFOUND THEN
                LET ls_flag = 1
            END IF
        END IF -- No encontrado en ret_resol_alterno
    END IF -- No encontrado en ret_parcial_resol

    RETURN ls_flag

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_tipo_pres : Valida que el tipo de prestacion exista en alguna de #
#                      las tablas de retiros parciales                      #
#---------------------------------------------------------------------------#
FUNCTION f_valida_tipo_pres(pr_datos)

    DEFINE pr_datos RECORD
        nss         LIKE ret_parcial.nss,
        num_resol   LIKE ret_parcial.num_resolucion,
        tipo_pres   LIKE ret_parcial.tipo_prestacion
    END RECORD

    DEFINE
        ls_flag             SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_flag = 0

    SELECT "OK"
    FROM   ret_parcial_resol
    WHERE  nss              = pr_datos.nss
    AND    num_resolucion   = pr_datos.num_resol
    AND    tipo_prestacion  = pr_datos.tipo_pres
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        IF pr_datos.tipo_pres = 7 THEN
            LET ls_flag = 1
        ELSE
            SELECT "OK"
            FROM   ret_resol_alterno
            WHERE  nss            = pr_datos.nss
            AND    num_resolucion = pr_datos.num_resol
            GROUP BY 1

            IF STATUS = NOTFOUND THEN
                SELECT "OK"
                FROM   ret_resol_comp
                WHERE  nss = pr_datos.nss
                GROUP BY 1

                IF STATUS = NOTFOUND THEN
                    LET ls_flag = 1
                END IF
            END IF -- No encontrado en ret_resol_alterno
        END IF -- Tipo prestacion = 6
    END IF -- No encontrado en ret_parcial_resol

    RETURN ls_flag

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_5anios_des : Valida que la solicitud no tenga un retiro parcial  #
#                       por desempleo de 5 años a la fecha                  #
#---------------------------------------------------------------------------#
FUNCTION f_valida_5anios_des(pc_nss)

    DEFINE
        pc_nss              CHAR(11)

    DEFINE
        ldt_max_fecha       DATE

    DEFINE
        ls_error            SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_error = 0

    CALL f_genera_tmp_cuenta(pc_nss)

    SELECT MAX(fecha_conversion)
    INTO   ldt_max_fecha
    FROM   tmp_dis_cuenta
    WHERE  nss              = pc_nss
    AND    tipo_movimiento  IN (gr_movs.tipo_d,
                                gr_movs.tipo_a,
                                gr_movs.tipo_b)

    LET ldt_max_fecha = ldt_max_fecha + 5 UNITS YEAR

    IF ldt_max_fecha > HOY THEN
        LET ls_error = 1
    END IF

    RETURN ls_error

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_matrimonio_ant : Valida que la solicitud no tenga un retiro      #
#                           parcial por matrimonio anterior al que se esta  #
#                           solicitando                                     #
#---------------------------------------------------------------------------#
FUNCTION f_valida_matrimonio_ant(pc_nss)

    DEFINE
        pc_nss              CHAR(11)

    DEFINE
        ls_error            SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_error = 0

    CALL f_genera_tmp_cuenta(pc_nss)

    SELECT "OK"
    FROM   tmp_dis_cuenta
    WHERE  nss              = pc_nss
    AND    tipo_movimiento  IN (487, gr_movs.matrimonio)
    GROUP BY 1
    -- 487 = Retiro por Matrimonio anterior

    IF STATUS <> NOTFOUND THEN
        LET ls_error = 1
    END IF

    RETURN ls_error

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_suficiencia : Valida que el nss tenga saldo suficiente para      #
#                        pagar el monto calculado en el retiro parcial      #
#---------------------------------------------------------------------------#
FUNCTION f_valida_suficiencia(pd_pago_des, pd_monto_rcv)

    DEFINE
        pd_pago_des     LIKE ret_parcial.pago_desempleo,
        pd_monto_rcv    DECIMAL(16,2)

    DEFINE
        ld_monto_pago   DECIMAL(16,2)

    -- -----------------------------------------------------------------------------
    IF pd_pago_des > pd_monto_rcv THEN
        LET ld_monto_pago = pd_monto_rcv
        ERROR "EL MONTO A PAGAR REBASA EL MONTO EN LA CUENTA, SE PAGARAN :", pd_monto_rcv
        SLEEP 3
    ELSE
        LET ld_monto_pago = pd_pago_des
    END IF

    RETURN ld_monto_pago

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_resol_alterno : Valida que la resolucion se encuentre cargada en #
#                          la tabla ret_resol_alterno. En caso de           #
#                          encontrarla regresa el valor del sbc y el        #
#                          indicador de tipo de pago                        #
#---------------------------------------------------------------------------#
FUNCTION f_valida_resol_alterno(pr_datos)

    DEFINE pr_datos RECORD
        nss             LIKE ret_parcial_resol.nss              ,
        num_resolucion  LIKE ret_parcial_resol.num_resolucion
    END RECORD

    DEFINE lr_alterno RECORD
        sbc             LIKE ret_resol_alterno.salario_base_cot ,
        tipo_pago       LIKE ret_resol_alterno.ind_tipo_pago
    END RECORD

    DEFINE li_max_folio LIKE ret_resol_alterno.folio

    DEFINE
        ls_anio         ,
        lr_rech         SMALLINT

    LET lr_rech = 0

    SELECT MAX(folio)
    INTO   li_max_folio
    FROM   ret_resol_alterno
    WHERE  nss              = pr_datos.nss
    AND    num_resolucion   = pr_datos.num_resolucion

    SELECT salario_base_cot ,
           ind_tipo_pago
    INTO   lr_alterno.*
    FROM   ret_resol_alterno
    WHERE  nss              = pr_datos.nss
    AND    num_resolucion   = pr_datos.num_resolucion
    AND    folio            = li_max_folio

    IF STATUS = NOTFOUND THEN
        LET lr_rech        = 1
        LET lr_alterno.sbc = 0

        -- Determinamos el tipo de pago de acuerdo a la posicion 3 y 4 del nss
        LET ls_anio = f_obtiene_anio(pr_datos.nss[3,4])

        IF ls_anio <= 2004 THEN
            LET lr_alterno.tipo_pago = 2
        ELSE
            LET lr_alterno.tipo_pago = 3
        END IF
    END IF

    RETURN lr_alterno.*, lr_rech

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_resol_comp : Valida que el nss tenga una resolucion de tipo C    #
#                       complementaria cargada en la tabla ret_resol_comp o #
#                       que tenga datos cargados en ret_parcial_resol. En   #
#                       caso de encontrarlos regresa el monto pagado en el  #
#                       retiro anterior y el saldo de rcv en ese momento    #
#---------------------------------------------------------------------------#
FUNCTION f_valida_resol_comp(pr_datos)

    DEFINE pr_datos RECORD
        nss             LIKE ret_parcial_resol.nss              ,
        num_resolucion  LIKE ret_parcial_resol.num_resolucion
    END RECORD

    DEFINE lr_comp RECORD
        monto_pagado    LIKE ret_resol_comp.monto_pagado ,
        saldo_rcv       LIKE ret_resol_comp.saldo_rcv
    END RECORD

    DEFINE
        ld_max_folio        INTEGER
                            
    DEFINE                  
        ld_rowid            DECIMAL(11,0)
                            
    DEFINE                  
        ls_anio             ,
        lr_rech             SMALLINT

    -- -----------------------------------------------------------------------------
    
    LET lr_rech              = 0
    LET lr_comp.monto_pagado = 0
    LET lr_comp.saldo_rcv    = 0

    SELECT MAX(folio)
    INTO   ld_max_folio
    FROM   ret_parcial_resol A 
    WHERE  A.nss             = pr_datos.nss      
    AND    A.num_resolucion  = pr_datos.num_resolucion

    SELECT MAX(ROWID)
    INTO   ld_rowid
    FROM   ret_parcial_resol A 
    WHERE  A.nss             = pr_datos.nss      
    AND    A.num_resolucion  = pr_datos.num_resolucion
    AND    A.folio           = ld_max_folio

    SELECT A.monto_pagado ,
           A.saldo_rcv_ant
    INTO   lr_comp.*
    FROM   ret_parcial_resol A
    WHERE  A.nss             = pr_datos.nss
    AND    A.num_resolucion  = pr_datos.num_resolucion
    AND    A.folio           = ld_max_folio
    AND    A.ROWID           = ld_rowid    

    IF (STATUS = NOTFOUND) OR
       ( (lr_comp.monto_pagado = 0 OR lr_comp.monto_pagado IS NULL) AND
         (lr_comp.saldo_rcv = 0 OR lr_comp.saldo_rcv IS NULL) ) THEN

        SELECT monto_pagado ,
               saldo_rcv
        INTO   lr_comp.*
        FROM   ret_resol_comp
        WHERE  nss  = pr_datos.nss

        IF STATUS = NOTFOUND THEN
            LET lr_rech = 1
        END IF
    END IF

    RETURN lr_comp.*, lr_rech

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_benef : Verifica que se hayan guardado correctamente los         #
#                  beneficiarios al finalizar su captura                    #
#---------------------------------------------------------------------------#
FUNCTION f_valida_benef(pr_captura)

    DEFINE pr_captura RECORD
        nss             LIKE ret_parcial.nss              ,
        consecutivo     LIKE ret_parcial.consecutivo
    END RECORD

    DEFINE
        ls_error        SMALLINT
        
    -- -----------------------------------------------------------------------------
    
    LET ls_error = 0
    
    SELECT "OK"
    FROM   ret_beneficiario
    WHERE  nss          = pr_captura.nss        
    AND    consecutivo  = pr_captura.consecutivo
    GROUP BY 1    
    
    IF STATUS = NOTFOUND THEN
        LET ls_error = 1
    END IF
    
    RETURN ls_error

END FUNCTION

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

#---------------------------------------------------------------------------#
# f_det_rechazo : Activa la bandera para determinar si la afore actual      #
#                 tiene la opcion de rechazo de solicitudes de parciales    #
#---------------------------------------------------------------------------#
FUNCTION f_det_rechazo(ps_cod_afore)

    DEFINE
        ps_cod_afore        ,
        ls_rechaza          SMALLINT

    LET ls_rechaza = 0

    -- Las afores con opcion de rechazo son Coppel y Scotia
    IF (ps_cod_afore = gs_cod_cpl) OR (ps_cod_afore = gs_cod_sct) THEN
        LET ls_rechaza = 1
    END IF

    RETURN ls_rechaza

END FUNCTION

#---------------------------------------------------------------------------#
# f_det_elimina : Activa la bandera para determinar si el usuario actual    #
#                 tiene permisos para eliminar solicitudes de parciales que #
#                 esten en estado confirmado                                #
#---------------------------------------------------------------------------#
FUNCTION f_det_elimina(pc_usuario)

    DEFINE
        pc_usuario          CHAR(20)

    DEFINE
        ls_elimina          SMALLINT

    -- -----------------------------------------------------------------------------
    
    IF (pc_usuario = "safre")
--    OR (pc_usuario = "fulloa")    -- Desarrollo EFP
    OR (pc_usuario = "mahe5146")  -- Emilio Macias  (Metlife)
    OR (pc_usuario = "flrc4687")  -- Roberto Flores (Metlife)
    OR (pc_usuario = "miml4889")  -- Lorena Miranda (Metlife)
    OR (pc_usuario = "abarboza")  -- Anabel Barboza (Invercap)
    OR (pc_usuario = "mflores")   -- Marlem Flores  (Invercap)
    OR (pc_usuario = "pgarcia")   -- Pedro Garcia   (Invercap)
    
    THEN
        LET ls_elimina = 1
    ELSE
        LET ls_elimina = 0
    END IF

    RETURN ls_elimina

END FUNCTION

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

#---------------------------------------------------------------------------#
# f_genera_tablas_tmp : Genera las tablas temporales donde se almacenan los #
#                       datos de la solicitud y los calculos                #
#---------------------------------------------------------------------------#
FUNCTION f_genera_tablas_tmp()

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_parcial
        DROP TABLE tmp_ctr_pago
        DROP TABLE tmp_ctr_pago_det
    WHENEVER ERROR STOP

    SELECT *
    FROM   ret_parcial
    WHERE  1 = 0
    INTO TEMP tmp_parcial

    SELECT *
    FROM   ret_ctr_pago
    WHERE  1 = 0
    INTO TEMP tmp_ctr_pago

    SELECT *
    FROM   ret_ctr_pago_det
    WHERE  1 = 0
    INTO TEMP tmp_ctr_pago_det

END FUNCTION


#---------------------------------------------------------------------------#
# f_genera_tmp_cuenta : Genera la tabla temporal tmp_dis_cuenta que         #
#                       contiene la informacion historica de la cuenta      #
#                       individual del nss                                  #
#---------------------------------------------------------------------------#
FUNCTION f_genera_tmp_cuenta(p_nss)

    DEFINE
         p_nss                  CHAR(11)  ,
         lc_nom_tabla           CHAR(20)  ,
         lc_sel_hist            CHAR(1500)

     WHENEVER ERROR CONTINUE
         DROP TABLE tmp_dis_cuenta
     WHENEVER ERROR STOP

     DECLARE cur_his CURSOR FOR
     SELECT tabname
     FROM   systables
     WHERE  tabname matches "dis_cuenta??"

     FOREACH cur_his INTO lc_nom_tabla

        LET lc_sel_hist = lc_sel_hist CLIPPED,
                          " SELECT * ",
                          " FROM ",lc_nom_tabla          ,
                          " WHERE nss = ","'",p_nss,"'"  ,
                          " UNION ALL "
    END FOREACH

    CLOSE cur_his

    LET lc_sel_hist = lc_sel_hist CLIPPED,
                      " SELECT * ",
                      " FROM dis_cuenta ",
                      " WHERE nss = ","'",p_nss,"'"  ,
                      " INTO TEMP tmp_dis_cuenta "
    
    PREPARE eje_sel_his FROM lc_sel_hist
    EXECUTE eje_sel_his
    
    CREATE INDEX tmp_dis_cuenta1 ON tmp_dis_cuenta (tipo_movimiento)
    UPDATE STATISTICS FOR TABLE tmp_dis_cuenta

END FUNCTION

