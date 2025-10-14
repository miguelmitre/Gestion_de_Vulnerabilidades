#################################################################################
#Owner             => E.F.P.                                                    #
#                     LIBRERIA DE FUNCIONES DEL MODULO DE RETIROS               #
#                                                                               #
#RETFUN_02          => Funciones de consultas especificas sobre la base de      #
#                      datos safre_af y safre_tmp                               #        
#                                                                               #
#Fecha creacion     => 23 DE AGOSTO DE 2012                                     #
#By                 => JAVIER GONZALEZ JERONIMO                                 #
#Fecha actualiz.    =>                                                          #
#Actualizacion      =>                                                          #
#                   =>                                                          #
#                                                                               #
#Sistema            => RET                                                      #
#################################################################################

DATABASE safre_af

{
Listado de funciones contenidas en RETFUN_02

    -   f_lib_obten_user()
    -   f_lib_obtiene_ult_folio()
    -   f_lib_obtiene_ult_consec()
    -   f_lib_obtiene_num_siefores()
    -   f_lib_obtiene_siefore_act(pc_nss)
    -   f_lib_redondea_val(pr_mto_redondea)
    -   f_lib_suma_mes(pdt_fecha, ls_meses)
    -   f_lib_obten_precio_accion(pdt_fecha, ps_siefore)
    -   f_lib_valida_precios_accion(pdt_fecha)
    -   f_lib_verifica_pago_vivienda(pr_solicitud)
    -   f_lib_genera_cadena_subcuentas(ps_grupo)
}


#---------------------------------------------------------------------------#
# f_lib_obten_user : Obtiene el nombre del usuario activo en el programa    #
#---------------------------------------------------------------------------#
FUNCTION f_lib_obten_user()

    DEFINE
        pc_usuario              CHAR(20)

    -- -----------------------------------------------------------------------------

    SELECT USER
    INTO   pc_usuario
    FROM   SYSTABLES
    WHERE  SYSTABLES.tabid = 1

    RETURN pc_usuario CLIPPED

END FUNCTION

#---------------------------------------------------------------------------#
# f_lib_obtiene_ult_folio : Obtiene el ultimo folio a utilizarse            #
#---------------------------------------------------------------------------#
FUNCTION f_lib_obtiene_ult_folio()

    DEFINE ld_folio LIKE glo_folio.folio

    DEFINE
        lc_prepare      CHAR(50)
        
    -- -----------------------------------------------------------------------------
    
    LET lc_prepare = " EXECUTE FUNCTION fn_obten_glo_folio() "
    PREPARE eje_glo_folio FROM lc_prepare
    EXECUTE eje_glo_folio INTO ld_folio
    
    RETURN ld_folio

END FUNCTION

#---------------------------------------------------------------------------#
# f_lib_obtiene_ult_consec : Obtiene el ultimo consecutivo a insertarse     #
#---------------------------------------------------------------------------#
FUNCTION f_lib_obtiene_ult_consec()

    DEFINE li_consecutivo LIKE ret_consecutivo.consecutivo

    DEFINE
        lc_prepare      CHAR(50)
        
    -- -----------------------------------------------------------------------------
    
    LET lc_prepare = " EXECUTE FUNCTION fn_obten_ret_consecutivo() "
    PREPARE eje_consecutivo FROM lc_prepare
    EXECUTE eje_consecutivo INTO li_consecutivo
    
    RETURN li_consecutivo

END FUNCTION

#---------------------------------------------------------------------------#
# f_lib_obtiene_num_siefores : Obtiene el numero de siefores basicas        #
#                              activas para la afore                        #
#---------------------------------------------------------------------------#
FUNCTION f_lib_obtiene_num_siefores()

    DEFINE
        ls_num_siefores     SMALLINT

    -- -----------------------------------------------------------------------------
    
    SELECT NVL(COUNT(*), 0)
    INTO   ls_num_siefores
    FROM   tab_siefore_local
    WHERE  ( (codigo_siefore NOT IN (0, 6)) AND (codigo_siefore < 10) ) 
    
    RETURN ls_num_siefores
    
END FUNCTION

#-----------------------------------------------------------------------#
# f_lib_obtiene_siefore_act : Obtiene la siefore actual del trabajador  #
#                             para las subcuentas de RCV                #
#-----------------------------------------------------------------------#
FUNCTION f_lib_obtiene_siefore_act(pc_nss)

    DEFINE pc_nss LIKE cta_nss_regimen.nss

    DEFINE
        ls_siefore          SMALLINT

    -- -----------------------------------------------------------------------------

    SELECT codigo_siefore
    INTO   ls_siefore
    FROM   cta_nss_regimen
    WHERE  grupo_regimen = 1
    AND    nss           = pc_nss

    RETURN ls_siefore

END FUNCTION

#---------------------------------------------------------------------------#
# f_lib_redondea_val : Redondea la cantidad p_monto_redondear a tantos      #
#                      digitos como indique la variable exponente           #
#---------------------------------------------------------------------------#
FUNCTION f_lib_redondea_val(pr_mto_redondea)

    DEFINE pr_mto_redondea RECORD 
        base            DECIMAL(20,6)   ,
        exponente       SMALLINT
    END RECORD

    DEFINE
        ls_resultado   DECIMAL(20,6)

    -- -----------------------------------------------------------------------------

    PREPARE round_id FROM "SELECT NVL(ROUND(?, ?),0) FROM SYSTABLES WHERE SYSTABLES.tabid = 1"
    DECLARE round_cur CURSOR FOR round_id

    OPEN round_cur USING pr_mto_redondea.*
        FETCH round_cur INTO ls_resultado
    CLOSE round_cur

    RETURN ls_resultado

END FUNCTION

#---------------------------------------------------------------------------#
# f_lib_suma_mes : Ejecuta el SPL para realizar la suma/resta de tantos     #
#                  meses indicados en la variable ls_meses a una fecha dada #
#                  por la variable pdt_fecha                                #
#---------------------------------------------------------------------------#
FUNCTION f_lib_suma_mes(pdt_fecha, ls_meses)

    DEFINE
        lc_prepare          CHAR(50)

    DEFINE
        pdt_fecha           ,
        ldt_fecha_res       DATE

    DEFINE
        ls_meses            SMALLINT

    -- -----------------------------------------------------------------------------
    
    LET lc_prepare = " EXECUTE FUNCTION fn_agrega_mes(?,?) "
    PREPARE eje_sum_mes FROM lc_prepare

    EXECUTE eje_sum_mes USING pdt_fecha     ,
                              ls_meses
                        INTO  ldt_fecha_res

    RETURN ldt_fecha_res

END FUNCTION

#---------------------------------------------------------------------------#
# f_lib_valida_precios_accion : Devuelve el precio de accion para una fecha #
#                               (pdt_fecha) y el valor de una siefore       #
#                               (ps_siefore)                                #
#                                                                           #
#   La funcion regresa los siguientes datos:                                #
#       ld_precio_acc   : El precio de accion para la fecha y la siefore    #
#---------------------------------------------------------------------------#
FUNCTION f_lib_obten_precio_accion(pdt_fecha, ps_siefore)

    DEFINE
        pdt_fecha               DATE

    DEFINE
        ps_siefore              SMALLINT

    DEFINE ld_precio_acc LIKE glo_valor_accion.precio_del_dia

    -- -----------------------------------------------------------------------------
    
    LET ld_precio_acc = 0
    
    SELECT NVL(precio_del_dia,0)
    INTO   ld_precio_acc
    FROM   glo_valor_accion
    WHERE  fecha_valuacion  = pdt_fecha 
    AND    codigo_siefore   = ps_siefore
    
    RETURN ld_precio_acc

END FUNCTION

#---------------------------------------------------------------------------#
# f_lib_valida_precios_accion : Ejecuta el SPL que recupera los precios del #
#                               dia y en caso de no existir alguno regresa  #
#                               la primera siefore faltante                 #
#                                                                           #
#   La funcion regresa los siguientes datos:                                #
#       codigo_err    : 0 - Precios de accion correctos                     #
#                       1 - Hace falta un precio de accion por lo menos     #
#                                                                           #
#       mensaje       : Un mensaje de error indicado la falla encontrada    #
#---------------------------------------------------------------------------#
FUNCTION f_lib_valida_precios_accion(pdt_fecha)

    DEFINE
        pdt_fecha               DATE

    DEFINE lr_precio_acc RECORD
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE lr_error RECORD
        codigo_err               SMALLINT   ,
        mensaje                  CHAR(100)
    END RECORD 

    DEFINE
        lc_eje_valida_precios       CHAR(100)

    DEFINE
        ls_siefore                  SMALLINT

    -- -----------------------------------------------------------------------------    

    LET ls_siefore              = 0
    LET lr_error.codigo_err     = 0

    LET lc_eje_valida_precios = " EXECUTE FUNCTION fn_verifica_precio_accion (?)"
    PREPARE prp_precios FROM lc_eje_valida_precios

    DECLARE cur_precios CURSOR FOR prp_precios
    FOREACH cur_precios USING pdt_fecha
                        INTO lr_precio_acc.*

        IF lr_precio_acc.estado <> 0 THEN
            LET ls_siefore          = lr_precio_acc.siefore
            LET lr_error.codigo_err = 1

            LET lr_error.mensaje = "FALTAN PRECIOS: FECHA ", 
                                   pdt_fecha USING "DD/MM/YYYY",
                                   " SIEFORE ", ls_siefore USING "&&"
                                   CLIPPED

            EXIT FOREACH
        END IF
    
    END FOREACH

    RETURN lr_error.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_lib_verifica_pago_vivienda : Abre la pantalla donde se pregunta al      #
#                                usuario si desea incluir o no el pago de   #
#                                las subcuentas de vivienda.                #
#                                                                           #
#   En caso de que el usuario responda que No, el programa determina que    #
# grupo paga las mismas subcuentas sin considerar las subcuentas 4 y 8      #
#                                                                           #
#   Este modulo se ejecuta solo despues de la confirmacion de la solicitud  #
#---------------------------------------------------------------------------#
FUNCTION f_lib_verifica_pago_vivienda(pr_solicitud)

    DEFINE pr_solicitud RECORD 
        nss                 LIKE ret_solicitud_tx.nss           ,
        consecutivo         LIKE ret_solicitud_tx.consecutivo
    END RECORD 

    DEFINE lr_edo RECORD
        modificado          LIKE ret_estado.estado_solicitud ,
        confirmado          LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE
        lc_tipo_retiro              CHAR(001)   ,
        lc_enter                    CHAR(001)   ,
        lc_cad_subcuenta            CHAR(150)

    DEFINE
        ls_estado_sol               ,
        ls_id_viv                   ,
        ls_grupo                    SMALLINT 

    -- -----------------------------------------------------------------------------

    SELECT A.estado_solicitud
    INTO   lr_edo.confirmado
    FROM   ret_estado A
    WHERE  A.descripcion = "CONFIRMADO"

    SELECT A.estado_solicitud
    INTO   lr_edo.modificado
    FROM   ret_estado A
    WHERE  A.descripcion = "MODIFICADO"    


    OPEN WINDOW retm8005 AT 20,3 WITH FORM "RETM8005" ATTRIBUTE(BORDER)

    WHILE TRUE
        PROMPT "¿DESEA INCLUIR EL PAGO DE LAS SUBCUENTAS DE VIVIENDA? (S/N) : " FOR lc_enter
        IF lc_enter MATCHES "[sSnN]" THEN
            IF lc_enter MATCHES "[sS]" THEN
                LET ls_id_viv = TRUE
            ELSE
                LET ls_id_viv = FALSE
            END IF
            EXIT WHILE
        END IF
    END WHILE

    -- Se realizan los cambios si no se incluyen las subcuentas de vivienda
    IF ls_id_viv = FALSE THEN
        SELECT tipo_retiro  ,
               grupo
        INTO   lc_tipo_retiro   ,
               ls_grupo
        FROM   ret_solicitud_tx
        WHERE  nss              = pr_solicitud.nss        
        AND    consecutivo      = pr_solicitud.consecutivo
        AND    estado_solicitud = lr_edo.confirmado
        GROUP BY 1,2 

        LET lc_cad_subcuenta = f_lib_genera_cadena_subcuentas(ls_grupo, ls_id_viv)

        SELECT grupo
        INTO   ls_grupo
        FROM   tab_agrupa_subcta_cad
        WHERE  cad_subcta = lc_cad_subcuenta
        GROUP BY 1

        IF lc_tipo_retiro = "E" THEN
            LET ls_estado_sol = lr_edo.modificado  -- Estado = Modificado para el tipo de retiro E
        ELSE 
            LET ls_estado_sol = lr_edo.confirmado  -- Estado = Confirmado para el resto de los tipos de retiro
        END IF 

        UPDATE ret_solicitud_tx
        SET    grupo            = ls_grupo      ,
               estado_solicitud = ls_estado_sol
        WHERE  nss              = pr_solicitud.nss        
        AND    consecutivo      = pr_solicitud.consecutivo
        AND    estado_solicitud = lr_edo.confirmado

        CALL f_lib_error_msg("SE EXCLUYERON LAS SUBCUENTAS DE VIVIENDA")

    END IF 

    CLOSE WINDOW retm8005

END FUNCTION

#---------------------------------------------------------------------------#
# f_lib_genera_cadena_subcuentas : Dado el grupo de subcuentas, genera una  #
#                                  cadena con la lista de subcuentas a pagar#
#                                                                           #
# ps_id_viv indica si se debe incluir o no las subcuentas de vivienda en    #
# el grupo                                                                  #
#---------------------------------------------------------------------------#
FUNCTION f_lib_genera_cadena_subcuentas(ps_grupo, ps_id_viv)

    DEFINE
        ps_grupo                ,
        ps_id_viv               SMALLINT

    DEFINE
        lc_query                CHAR(500)   ,
        lc_cad_subcta           CHAR(150)

    DEFINE
        ls_subcuenta            SMALLINT

    -- -----------------------------------------------------------------------------

    INITIALIZE lc_cad_subcta TO NULL
    LET ls_subcuenta    = 0

    LET lc_query        = " SELECT subcuenta         ",
                          " FROM   tab_agrupa_subcta ",
                          " WHERE  grupo = ?         ",
                          " AND    subcuenta <> 0    "
    
    IF ps_id_viv = 0 THEN
        LET lc_query    = lc_query CLIPPED, 
                          " AND subcuenta NOT IN (4,8) "    ,
                          " ORDER BY 1 "
    ELSE
        LET lc_query    = lc_query CLIPPED, 
                          " ORDER BY 1 "        
    END IF 

    PREPARE prp_cad_sub FROM lc_query
    DECLARE cur_subcuentas CURSOR FOR prp_cad_sub
    
    FOREACH cur_subcuentas USING ps_grupo
                           INTO  ls_subcuenta
        -- Si la cadena no es nula, se acumulan las subcuentas conforme se recuperan
        IF lc_cad_subcta IS NOT NULL THEN
            
            LET lc_cad_subcta   = lc_cad_subcta CLIPPED,
                                  ",",
                                  ls_subcuenta USING "<<<<<<<<<<"
        ELSE
            -- Si la cadena es nula, se esta obteniendo el primer resultado
            LET lc_cad_subcta = ls_subcuenta USING "<<<<<<<<<<"
        END IF
    
    END FOREACH

    RETURN lc_cad_subcta

END FUNCTION
