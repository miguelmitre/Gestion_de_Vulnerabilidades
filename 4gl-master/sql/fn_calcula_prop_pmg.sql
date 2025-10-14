DROP FUNCTION fn_calcula_prop_pmg
;
CREATE FUNCTION fn_calcula_prop_pmg(pc_nss           CHAR(11)      , 
                                    pi_consecutivo   INTEGER       ,
                                    pd_monto_pesos   DECIMAL(16,2) ,
                                    pdt_fecha        DATE          ,
									p_edo_viv        SMALLINT      ,
                                    p_edo_viv_issste SMALLINT
                                    )

    RETURNING SMALLINT      , 
              SMALLINT      , 
              DECIMAL(16,6) , 
              DECIMAL(16,6) , 
              DECIMAL(16,2) ; 

-- -------------------------------------------------------------------------------
-- Proyecto         => SISTEMA DE AFORES (SAFRE)                                --
-- Propietario      => E.F.P.                                                   --
-- Stored Procedure => fn_calcula_prop_pmg                                      --
--                     Genera el monto proporcional por subcuenta del pago de   --
--                     pension minima garantizada indicado de acuerdo al saldo  --
--                     al dia del trabajador                                    --
--                                                                              --
--                                                                              --
--   Parametros de Entrada :                                                    --
--       pc_nss             - NSS al que se calcula el monto a pagar            --
--       pi_consecutivo     - Consecutivo de la solicitud de retiro del NSS     --
--       pd_monto_pesos     - Monto a pagar total del trabajador                --
--       pdt_fecha          - Fecha a la que se hace el calculo proporcional    --
--                                                                              --
--   Variables de Salida :                                                      --
--       ls_subcuenta       - Subcuenta en donde se realiza el pago             --
--       ls_siefore         - Siefore donde el trabajador tiene su saldo        --
--       ld_monto_acciones  - Monto total a pagar en acciones                   --
--       ld_acciones_pagar  - Monto proporcional subcuenta en acciones          --
--       ld_pesos_pagar     - Monto proporcional subcuenta en pesos             --
--                                                                              --
-- Fecha creacion   =>                                                          --
-- By               => JAVIER GONZALEZ JERONIMO                                 --
-- Requerimiento    =>                                                          --
-- Fecha actualiz.  =>                                                          --
-- Actualizacion    =>                                                          --
-- Requerimiento    =>                                                          --
-- Sistema          => RET                                                      --
-- -------------------------------------------------------------------------------

    DEFINE ls_subcuenta       SMALLINT;
    DEFINE f_subcuenta        SMALLINT;
    DEFINE ls_subcta_temp     SMALLINT;

    DEFINE ls_siefore         SMALLINT;
    DEFINE ls_siefore_tmp     SMALLINT;

    DEFINE ld_diferencia      DECIMAL(16,2);
    DEFINE ld_monto_acciones  DECIMAL(16,6);
	DEFINE ld_monto_acc_tmp   DECIMAL(16,6);
    
    DEFINE ld_monto_pesos     DECIMAL(16,2);
    DEFINE ld_monto_pesos_tmp DECIMAL(16,2);
    DEFINE ld_saldo_total     DECIMAL(16,2);

    DEFINE ls_redondeo        SMALLINT;
    DEFINE ld_acciones_pagar  DECIMAL(16,6);
    DEFINE ld_pesos_prov      DECIMAL(16,2);

    DEFINE ld_prop_a_pagar    DECIMAL(16,2);
    DEFINE ld_acc_prop        DECIMAL(16,6);
    DEFINE ld_pesos_pagar     DECIMAL(16,2); 
    DEFINE ld_pesos_pago_tot  DECIMAL(16,2); 
    DEFINE ld_precio_acc      DECIMAL(16,6); 
    DEFINE ls_grupo           SMALLINT     ;

    DEFINE ls_grupo_sol         SMALLINT;
    DEFINE lc_fecha             CHAR(10);
    DEFINE ldt_fecha_viv        DATE    ;

    -- -----------------------------------------------------------------------------

--    SET debug file to "fn_calcula_prop_pmg.log";
--    trace ON;

    LET ldt_fecha_viv       = MONTH(pdt_fecha)||"/01/"||YEAR(pdt_fecha);
    LET ls_siefore          = 0;
    LET ls_subcuenta        = 0;
    LET ld_acc_prop         = 0;
    LET ls_redondeo         = 0;
    LET ld_prop_a_pagar     = 0;
    LET ls_grupo            = 0;
    LET ld_saldo_total      = 0;

    SELECT grupo
    INTO   ls_grupo_sol
    FROM   pen_solicitud_pmg
    WHERE  nss          = pc_nss
    AND    consecutivo  = pi_consecutivo ;


    CREATE TEMP TABLE tmp_mto_parcial
    (
     subcuenta   SMALLINT     ,
     siefore     SMALLINT     ,
     acc_tot     DECIMAL(16,6),
     pesos_tot   DECIMAL(16,2),
     acc_pago    DECIMAL(16,6),
     pesos_pago  DECIMAL(16,2)
    );

    -- Obtenemos monto RCV
    FOREACH
       SELECT subcuenta
       INTO   ls_subcta_temp
       FROM   tab_agrupa_subcta
       WHERE  grupo = ls_grupo_sol
       ORDER BY 1

        FOREACH EXECUTE FUNCTION fn_saldo_dia_isss(pc_nss, ls_subcta_temp, ls_grupo, pdt_fecha)
                        INTO f_subcuenta     , 
                             ls_siefore_tmp  , 
                             ld_monto_acc_tmp, 
                             ld_monto_pesos_tmp
							 
            IF NOT ((f_subcuenta = 4 AND p_edo_viv <> 1) OR (f_subcuenta = 35 AND p_edo_viv_issste <> 1)) THEN
            
                LET ld_saldo_total = ld_saldo_total + ld_monto_pesos_tmp;
                
                -- Obtenemos la subcuenta sobre la que se aplicaria correccion de redondeo
                IF ls_redondeo = 0 THEN 
                    LET ls_redondeo = f_subcuenta;
                END IF;
                
                INSERT INTO tmp_mto_parcial VALUES (f_subcuenta         ,
                                                    ls_siefore_tmp      ,
                                                    ld_monto_acc_tmp    ,
                                                    ld_monto_pesos_tmp  ,
                                                    0                   ,
                                                    0 );
            END IF;
        END FOREACH; -- Saldos
    END FOREACH; -- Subcuentas
    
    IF ld_saldo_total < pd_monto_pesos THEN
        LET ld_pesos_pago_tot = ld_saldo_total;
    ELSE
        LET ld_pesos_pago_tot = pd_monto_pesos;
    END IF;

    -- Obtenemos los montos proporcionales a pagar por subcuenta
    FOREACH
       SELECT subcuenta ,
              siefore   ,
              pesos_tot  
       INTO   ls_subcuenta  ,
              ls_siefore    ,
              ld_monto_pesos
       FROM   tmp_mto_parcial
       ORDER BY 1
    
        -- Obtenemos el precio por accion del dia de la siefore actual
        IF ls_siefore <> 11 THEN  
            SELECT precio_del_dia
            INTO   ld_precio_acc
            FROM   glo_valor_accion
            WHERE  fecha_valuacion = pdt_fecha
            AND    codigo_siefore  = ls_siefore;
        ELSE
            SELECT precio_del_dia
            INTO   ld_precio_acc
            FROM   glo_valor_accion
            WHERE  fecha_valuacion = ldt_fecha_viv
            AND    codigo_siefore  = ls_siefore;
        END IF;
        
        IF ld_precio_acc IS NULL THEN
            EXECUTE PROCEDURE error (-746, "NO EXISTE PRECIO ACCION DE LA SIEFORE " || ls_siefore || " PARA: " || pdt_fecha );
        END IF;

        LET ld_prop_a_pagar = (ld_monto_pesos * ld_pesos_pago_tot) / ld_saldo_total;

        -- Obtenemos el precio en acciones de la siefore
        LET ld_acc_prop = ld_prop_a_pagar / ld_precio_acc;
    
        UPDATE tmp_mto_parcial
        SET    acc_pago     = ld_acc_prop       ,
               pesos_pago   = ld_prop_a_pagar
        WHERE  subcuenta    = ls_subcuenta;
    
    END FOREACH ;

    -- Determinamos si existen diferencias entre lo provisionado y el monto a pagar
    SELECT SUM(pesos_pago)
    INTO   ld_pesos_prov
    FROM   tmp_mto_parcial;
    
    LET ld_diferencia = ld_pesos_pago_tot - ld_pesos_prov;
    
    IF ld_diferencia <> 0 THEN
        -- En caso de existir diferencias se ajusta la subcuenta obtenida anteriormente
        UPDATE tmp_mto_parcial
        SET    pesos_pago = pesos_pago + ld_diferencia
        WHERE  subcuenta  = ls_redondeo;
        
        UPDATE tmp_mto_parcial
        SET    acc_pago   = pesos_pago / ld_precio_acc
        WHERE  subcuenta  = ls_redondeo;        
    END IF;
    
    LET ls_subcuenta        = 0 ;
    LET ls_siefore          = 0 ;
    LET ld_monto_acciones   = 0 ;
    LET ld_acciones_pagar   = 0 ;
    LET ld_pesos_pagar      = 0 ;    

    -- Regresamos los valores obtenidos
    FOREACH
      SELECT subcuenta          ,
             siefore            ,
             acc_tot            ,
             acc_pago           ,
             pesos_pago
      INTO   ls_subcuenta       ,
             ls_siefore         ,
             ld_monto_acciones  ,
             ld_acciones_pagar  ,
             ld_pesos_pagar   
      FROM   tmp_mto_parcial
      ORDER BY 1
        
        RETURN ls_subcuenta         ,
               ls_siefore           ,
               ld_monto_acciones    ,
               ld_acciones_pagar    ,
               ld_pesos_pagar   
               WITH RESUME ;
    END FOREACH;
    
    DROP TABLE tmp_mto_parcial;

END FUNCTION;
