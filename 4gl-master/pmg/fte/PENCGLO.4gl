################################################################################
# FUNCIONES GLOBALES (LIBRERIA) PARA EL MODULO DE PENSION MINIMA GARANTIZADA   #
# IMSS                                                                         #
################################################################################

DATABASE safre_af

DEFINE TECLA CHAR(1)
DEFINE ENTER CHAR(1)

{===============================================================================
Objetivo  : Obtener datos de la ultima mensualidad liquidada
Parametros: nss
Retorno   : fecha estimada de pago
            fecha de liquidacion
            monto pago pesos
            monto liquidado
===============================================================================}
FUNCTION fg_ultima_mensualidad_liquidada(pc_nss)

   DEFINE pc_nss                   CHAR(11)

   DEFINE lr_mensualidad           RECORD LIKE pen_ctr_pago_det.*
   
   --VALORES DE RETORNO
   DEFINE ld_fecha_liquidacion     DATE                       --fecha conversion
   DEFINE ld_monto_liquidado       DECIMAL(10,2)              --monto en pesos

   -----------------------------------------------------------------------------
   
   INITIALIZE ld_fecha_liquidacion   TO NULL
   INITIALIZE ld_monto_liquidado     TO NULL

   --RECUPERA LA ULTIMA MENSUALIDAD LIQUIDADA
   SELECT *
   INTO   lr_mensualidad.*
   FROM   pen_ctr_pago_det
   WHERE  nss             = pc_nss
   AND    num_mensualidad = (SELECT MAX(num_mensualidad)
                             FROM   pen_ctr_pago_det
                             WHERE  nss           = pc_nss
                             AND    fecha_liquida IS NOT NULL
                             AND    folio_liquida IS NOT NULL)
   
   IF sqlca.sqlcode = 0 THEN
      --RECUPERA EL MONTO LIQUIDADO
      SELECT fecha_conversion,
             SUM(monto_en_pesos)
      INTO   ld_fecha_liquidacion,
             ld_monto_liquidado
      FROM   dis_cuenta
      WHERE  nss              = pc_nss
      AND    consecutivo_lote = lr_mensualidad.consecutivo
      AND    folio            = lr_mensualidad.folio_liquida
      GROUP  BY fecha_conversion
   END IF

   RETURN lr_mensualidad.num_mensualidad    ,
          lr_mensualidad.fecha_pago_estimada,
          ld_fecha_liquidacion              ,
          lr_mensualidad.mto_pago_pesos     ,
          ld_monto_liquidado    


END FUNCTION

{===============================================================================
Objetivo  : Obtener solo la fecha de la ultima mensualidad liquidada
Parametros: nss
Retorno   : fecha de ultima mensualidad liquidada
===============================================================================}
FUNCTION fg_fecha_ultima_mens_liquidada(pc_nss)

   DEFINE pc_nss                   CHAR(11)
   DEFINE ld_fecha_ult_mens_liq    DATE                       --fecha conversion

   -----------------------------------------------------------------------------

   --RECUPERA LA ULTIMA MENSUALIDAD LIQUIDADA
   SELECT MAX(fecha_conversion)
   INTO   ld_fecha_ult_mens_liq
   FROM   dis_cuenta
   WHERE  nss             = pc_nss
   AND    tipo_movimiento = 841

   RETURN ld_fecha_ult_mens_liq

END FUNCTION

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION fg_parametro(pc_id_parametro)

   DEFINE pc_id_parametro     CHAR(20)
   DEFINE lc_valor            CHAR(100)
   
   WHENEVER ERROR CONTINUE 
      SELECT valor
      INTO   lc_valor
      FROM   pen_parametro
      WHERE  id_parametro = pc_id_parametro
      
      IF sqlca.sqlcode != 0 THEN 
         LET lc_valor = NULL
      END IF
   WHENEVER ERROR STOP
   
   RETURN lc_valor

END FUNCTION

#==============================================================================#
# f_saldo_dia : obtiene el saldo al dia de las subcuentas relacionadas con PMG #
#             : tomando en cuenta el estatus de vivienda                       #
#==============================================================================#
FUNCTION fg_saldo_dia_pmg(pr_datos_trab)

    DEFINE pr_datos_trab      RECORD
           nss                LIKE pen_preliquida_pmg.nss             , 
           edo_sub_viv        LIKE pen_solicitud_pmg.estado_sub_viv   ,
           fec_liquida        DATE
           END RECORD

    DEFINE lr_saldo           RECORD
           subcta             SMALLINT        ,
           sie                SMALLINT        ,
           monto_acc          DECIMAL(16,6)   ,
           monto_pes          DECIMAL(16,2)
           END RECORD
    
    DEFINE ls_subcta          SMALLINT 
    DEFINE ls_grupo           SMALLINT

    DEFINE ld_saldo_dia       DECIMAL(16,6)

    ----------------------------------------------------------------------------
CALL fg_debug("CALCULANDO SALDO AL DIA PMG NSS: "||pr_datos_trab.nss)
CALL fg_debug("\tedo_sub_viv: "||pr_datos_trab.edo_sub_viv)

    LET ls_subcta       = 0
    LET ls_grupo        = 0
    LET ld_saldo_dia    = 0
    
    PREPARE eje_saldo_dia FROM " EXECUTE FUNCTION fn_saldo_dia(?,?,?,?) "

    DECLARE cur_saldo CURSOR FOR eje_saldo_dia

    FOREACH cur_saldo USING pr_datos_trab.nss           ,
                            ls_subcta                   ,
                            ls_grupo                    ,
                            pr_datos_trab.fec_liquida
                      INTO  lr_saldo.*

        IF lr_saldo.monto_pes <= 0 THEN
            LET lr_saldo.monto_pes = 0
        END IF
        
        IF (lr_saldo.subcta = 1) OR (lr_saldo.subcta = 2) OR (lr_saldo.subcta = 5) OR 
           (lr_saldo.subcta = 6) OR (lr_saldo.subcta = 9) THEN 
            LET ld_saldo_dia = ld_saldo_dia + lr_saldo.monto_pes
        ELSE
            IF (lr_saldo.subcta = 4 AND pr_datos_trab.edo_sub_viv = 1) THEN
                LET ld_saldo_dia = ld_saldo_dia + lr_saldo.monto_pes
            END IF
        END IF
    
    END FOREACH 

    RETURN ld_saldo_dia

END FUNCTION

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION fg_obten_mensualidad_vigente(pc_nss, pc_consecutivo, pc_fecha_ini_pen)

   DEFINE pc_nss                  CHAR(11) 
   DEFINE pc_consecutivo          DECIMAL(11,0)
   DEFINE pc_fecha_ini_pen        DATE
   DEFINE ld_importe_mensual_his  DECIMAL(10,2)
   DEFINE ld_importe_mensual      DECIMAL(10,2)
   DEFINE ls_tipo                 SMALLINT 
   DEFINE ld_consecutivo          DECIMAL(11,0)   -- INV-5188
   DEFINE lc_comando              CHAR(1500)
   DEFINE v_saldo_promedio        LIKE ret_datamart_comp.saldo_promedio
   DEFINE v_edad                  LIKE ret_datamart_comp.edad
   DEFINE v_semanas_cotizadas     LIKE ret_det_datamart.semanas_cotizadas
   DEFINE ls_year_actual          SMALLINT
   DEFINE ls_columna              SMALLINT
   DEFINE ls_edad_consulta        SMALLINT
   DEFINE ld_valor_uma            DECIMAL(10,2)
   DEFINE ld_sal_umas             DECIMAL(10,2)
   
   
   INITIALIZE ld_importe_mensual TO NULL          -- INV-5188
   
   --WHENEVER ERROR CONTINUE                      -- INV-5188 SE COMENTA EL WHENEVER
   
   --RECUPERA EL IMPORTE DE PAGO ORIGINAL PARA SABER EL TABULADOR
   LET lc_comando = '',
                    '\n SELECT FIRST 1 importe_mensual, consecutivo      ',  -- INV-5188
                    '\n   FROM pen_solicitud_pmg                         ',
                    '\n  WHERE nss          = "',pc_nss,'"               ',
                    '\n    AND sec_contrato = 1                          ',
                    '\n    AND estado_solicitud NOT IN (80)              '   -- INV-5188 80=RECHAZADO
                    
   PREPARE consulta_uno FROM lc_comando
   EXECUTE consulta_uno INTO ld_importe_mensual_his, ld_consecutivo 
      
   IF ld_importe_mensual_his = 0 OR ld_importe_mensual_his IS NULL OR SQLCA.SQLCODE = NOTFOUND THEN 
      	
      --SELECCIONA DE TABLA ALTERNA
      -- SELECT UNIQUE pago_mensual_pesos         -- INV-5188
      
      LET lc_comando = '',
                       '\n SELECT FIRST 1 UNIQUE pago_mensual_pesos  ',  -- INV-5188
                       '\n   FROM pen_ctr_pago                       ',
                       '\n  WHERE nss = "',pc_nss,'"                 '
                       
      PREPARE consulta_dos FROM lc_comando
      EXECUTE consulta_dos INTO ld_importe_mensual_his

      -- IF SQLCA.SQLCODE < 0 THEN                -- INV-5188      
      IF SQLCA.SQLCODE = NOTFOUND THEN            -- INV-5188
         ERROR "ERROR AL RECUPERAR IMPORTE MENSUAL HISTORICO"
         INITIALIZE ld_importe_mensual TO NULL    -- INV-5188
         RETURN ld_importe_mensual
      END IF
      
   ELSE 
   	
      SELECT UNIQUE pago_mensual_pesos            -- INV-5188
        INTO ld_importe_mensual_his               -- INV-5188 
        FROM pen_ctr_pago                         -- INV-5188   
       WHERE nss         = pc_nss                 -- INV-5188
         AND consecutivo = ld_consecutivo         -- INV-5188 
   	         
   END IF

   --DETERMINA EL MONTO SEGUN LA FECHA DE INICIO DE LA PENSION CPL-3279
   IF pc_fecha_ini_pen < '01/01/2021' THEN   
       --DETERMINA SI EL MONTO ENCONTRADO ES IMPORTE_MENSUAL O IMPORTE_MENSUAL_11P
       SELECT UNIQUE "1" 
         INTO ls_tipo 
         FROM tab_pmg_historica 
        WHERE importe_mensual     = ld_importe_mensual_his
        UNION 
       SELECT UNIQUE "2"
         FROM tab_pmg_historica 
        WHERE importe_mensual_11p = ld_importe_mensual_his

       IF SQLCA.SQLCODE = NOTFOUND THEN
          LET ld_importe_mensual = 0
       ELSE
          IF ls_tipo = 1 THEN 
             SELECT importe_mensual 
               INTO ld_importe_mensual
               FROM tab_pmg_historica
              WHERE fecha_hasta IS NULL 
          ELSE 
             SELECT importe_mensual_11p 
               INTO ld_importe_mensual
               FROM tab_pmg_historica
              WHERE fecha_hasta IS NULL 
          END IF
       END IF 

    ELSE 

        SELECT DISTINCT a.saldo_promedio, a.edad, b.semanas_cotizadas
        INTO   v_saldo_promedio, v_edad, v_semanas_cotizadas
        FROM   ret_datamart_comp a,
               ret_det_datamart b
        WHERE  a.nss = pc_nss
        AND    a.nss = b.nss
        AND    a.folio_t_procesar = b.folio_t_procesar
        AND    a.saldo_promedio > 0  
        AND    a.edad > 0
        AND    a.saldo_promedio = (SELECT MAX(c.saldo_promedio)
                                   FROM   ret_datamart_comp c,
                                          ret_det_datamart d
                                   WHERE  c.nss = pc_nss
                                   AND    c.nss = d.nss
                                   AND    c.folio_t_procesar = d.folio_t_procesar
                                   AND    c.saldo_promedio > 0  
                                   AND    c.edad > 0)

        LET ls_year_actual      = YEAR(pc_fecha_ini_pen)

        SELECT monto_uma
        INTO   ld_valor_uma
        FROM   tab_valor_uma
        WHERE  (fecha_desde_uma <= pc_fecha_ini_pen AND
                fecha_hasta_uma >= pc_fecha_ini_pen)
        OR     (fecha_desde_uma <= pc_fecha_ini_pen AND
                fecha_hasta_uma IS NULL)
                
        SELECT columna
        INTO   ls_columna
        FROM   pen_matriz_anio_semanas
        WHERE  anio = ls_year_actual
        AND    semanas_min <= v_semanas_cotizadas
        AND    semanas_max >= v_semanas_cotizadas
            
        IF v_edad > 65 THEN 
            LET ls_edad_consulta = 65
        ELSE 
            LET ls_edad_consulta = v_edad
        END IF

        LET ld_sal_umas = v_saldo_promedio / ld_valor_uma
        IF ld_sal_umas > 5 THEN 
            LET ld_sal_umas = 5
        END IF 
        IF ld_sal_umas < 1 AND ld_sal_umas > 0 THEN 
            LET ld_sal_umas = 1
        END IF 

        --- Otenemos el monto actualizado (Por los notificados en enero 2021)
        SELECT pesos_pension
        INTO   ld_importe_mensual
        FROM   pen_matriz_sem_cotiza
        WHERE  columna = ls_columna
        AND    edad = ls_edad_consulta
        AND    (salario_min_inf <= ld_sal_umas AND salario_min_sup >= ld_sal_umas)
        AND    fecha_fin_vigencia IS NULL

   END IF 

   RETURN ld_importe_mensual

END FUNCTION

##==============================================================================#
# Objetivo: devolver el saldo en pesos de las subcuentas involucradas en PMG   #
#         : valuadas las acciones al día de ejecucion                          #
#==============================================================================#
FUNCTION fg_saldo_pesos_pmg(p_nss, p_fecha_saldo)

   DEFINE p_nss               CHAR(11)
   DEFINE p_fecha_saldo       DATE
   
   DEFINE v_mto_pesos_tot     DECIMAL(18,2)
   DEFINE v_precio_del_dia    DECIMAL(18,6)
   DEFINE v_monto_en_pesos    DECIMAL(18,2)
   DEFINE v_monto_en_acciones DECIMAL(18,6)
   DEFINE v_subcuenta         SMALLINT
   DEFINE v_siefore           SMALLINT

   LET v_mto_pesos_tot     = 0
   LET v_monto_en_pesos    = 0 
   LET v_monto_en_pesos    = 0 
   LET v_monto_en_acciones = 0 
   
   --RECUPERA EL SALDO EN ACCIONES DE CADA UNA DE LAS SUBCUENTAS INVOLUCRADAS
   DECLARE cur_sdo_pmg CURSOR FOR
   SELECT subcuenta, siefore, sum(monto_en_acciones)
   FROM   dis_cuenta
   WHERE  nss       = p_nss
   AND    subcuenta IN (1,2,5,6,9,4)
   GROUP  BY 1,2

   FOREACH cur_sdo_pmg INTO v_subcuenta, v_siefore, v_monto_en_acciones
      
      --OBTIENE EL PRECIO DE LA SIEFORE SELECCIONADA
      IF v_subcuenta != 4 THEN 
         --Diferente a vivinda   
         SELECT precio_del_dia
         INTO   v_precio_del_dia
         FROM   glo_valor_accion
         WHERE  fecha_valuacion = p_fecha_saldo
         AND    codigo_siefore  = v_siefore
      ELSE
         --vivinda se valua al primero del mes
         SELECT precio_del_dia
         INTO   v_precio_del_dia
         FROM   glo_valor_accion
         WHERE  fecha_valuacion = MDY(MONTH(p_fecha_saldo),"01",YEAR(p_fecha_saldo))
         AND    codigo_siefore  = v_siefore
      END IF 

      LET v_monto_en_pesos = v_monto_en_acciones * v_precio_del_dia
         
      LET v_mto_pesos_tot = v_mto_pesos_tot + v_monto_en_pesos

   END FOREACH

   RETURN v_mto_pesos_tot

END FUNCTION

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION fg_calcula_monto_pago_pmg_imss(pc_nss, pd_fecha_inicial, pd_fecha_final)
   
   DEFINE pc_nss                  CHAR(11)
   DEFINE pd_fecha_inicial        DATE    --fecha desde la que se realiza el calculo
   DEFINE pd_fecha_final          DATE    --fecha hasta la que se realiza el calculo
   
   DEFINE ld_fecha_inicial        DATE
   DEFINE ld_fecha_final          DATE
   DEFINE ld_fecha_auxiliar       DATE
   
   DEFINE ld_importe_mensual_sol  DECIMAL(10,2)
   DEFINE ld_importe_mensual_his  DECIMAL(10,2)
   DEFINE ld_monto_pago           DECIMAL(10,2)

   --RECUPERA EL IMPORTE MENSUAL DEL PRIMER CONTRATO
   SELECT importe_mensual
   INTO   ld_importe_mensual_sol
   FROM   pen_solicitud_pmg
   WHERE  nss              = pc_nss
   AND    sec_contrato     = (SELECT MAX(sec_contrato)
                              FROM   pen_solicitud_pmg solpmg
                              WHERE  solpmg.nss = pen_solicitud_pmg.nss)
   AND    estado_solicitud != 80
   
   IF sqlca.sqlcode < 0 THEN 
      PROMPT "NO SE ENCONTRO IMPORTE MENSUAL!!! PRESIONE <ENTER>: " FOR CHAR ENTER
      EXIT PROGRAM
   END IF 

   --SE DETERMINA COMO FECHA INICIAL EL MES SIGUIENTE A LA ULTIMA LIQUIDACION 
   LET ld_fecha_inicial  = MDY(MONTH(pd_fecha_inicial),1,YEAR(pd_fecha_inicial))
   LET ld_fecha_final    = MDY(MONTH(pd_fecha_final)  ,1,YEAR(pd_fecha_final))
   
   CALL fg_debug("Calculando monto pago...")
   CALL fg_debug("\tld_fecha_inicial="||ld_fecha_inicial)
   CALL fg_debug("\tld_fecha_final  ="||ld_fecha_final)


   LET ld_fecha_auxiliar = ld_fecha_inicial
   LET ld_monto_pago     = 0
   
   WHILE( ld_fecha_auxiliar <= ld_fecha_final )
      
       IF fg_obtiene_tipo_mensualidad(pc_nss) = 2 THEN
          --RECUPERA LA MENSUALIDAD 11p CORRESPONDIENTE AL MES AUXILIAR
          SELECT importe_mensual_11p
          INTO   ld_importe_mensual_his
          FROM   tab_pmg_historica
          WHERE (ld_fecha_auxiliar BETWEEN fecha_desde AND fecha_hasta)
          OR    (ld_fecha_auxiliar >= fecha_desde AND fecha_hasta IS NULL)

          IF sqlca.sqlcode < 0 THEN 
             PROMPT "NO SE ENCONTRO IMPORTE MENSUAL 11p EN HISTORICO <ENTER>:" FOR CHAR ENTER
             EXIT PROGRAM
          END IF
       ELSE
          --RECUPERA LA MENSUALIDAD CORRESPONDIENTE AL MES AUXILIAR
          SELECT importe_mensual
          INTO   ld_importe_mensual_his
          FROM   tab_pmg_historica
          WHERE (ld_fecha_auxiliar BETWEEN fecha_desde AND fecha_hasta)
          OR    (ld_fecha_auxiliar >= fecha_desde AND fecha_hasta IS NULL)
          
          IF sqlca.sqlcode < 0 THEN 
             PROMPT "NO SE ENCONTRO IMPORTE MENSUAL EN HISTORICO <ENTER>:" FOR CHAR ENTER
             EXIT PROGRAM
          END IF
       END IF
       
       LET ld_monto_pago = ld_monto_pago + ld_importe_mensual_his
       LET ld_fecha_auxiliar = ld_fecha_auxiliar + 1 UNITS MONTH
       
       CALL fg_debug("\tld_fecha_auxiliar     ="||ld_fecha_auxiliar USING "DD-MM-YYYY")
       CALL fg_debug("\tld_importe_mensual_his="||ld_importe_mensual_his              )
       CALL fg_debug("\tld_monto_pago         ="||ld_monto_pago                       )
      
   END WHILE
   
   RETURN  ld_monto_pago

END FUNCTION

#==============================================================================#
# Objetivo: recuperar el tipo de mensualidad (historico) por aplicar, ya sea   #
#           importe autorizado o importe autorizado 11p                        #
# Retorna  0 si no encuentra el monto en catalogo                              #
#          1 si lo encuentra el monto en catalogo                              #
#          2 si lo encuentra el tipo es 11p                                    #
#==============================================================================#
FUNCTION fg_obtiene_tipo_mensualidad(pc_nss)

   DEFINE pc_nss              CHAR(11)
   DEFINE ls_tipo_pago        SMALLINT
   DEFINE ld_importe_mensual  DECIMAL(10,2)

   --RECUPERA EL IMPORTE MENSUAL DEL PRIMER CONTRATO
   SELECT importe_mensual
   INTO   ld_importe_mensual
   FROM   pen_solicitud_pmg
   WHERE  nss               = pc_nss
   AND    sec_contrato      = (SELECT MAX(sec_contrato)
                               FROM   pen_solicitud_pmg solpmg
                               WHERE  solpmg.nss = pen_solicitud_pmg.nss
                              )
   AND    estado_solicitud != 80   --rechazada

   IF sqlca.sqlcode = NOTFOUND THEN
      LET ls_tipo_pago = 0    --erroneo o no identificado
   ELSE
      --BUSCA EL IMPORTE MENSUAL EN HISTORICO 
      SELECT 1
      INTO   ls_tipo_pago
      FROM   tab_pmg_historica
      WHERE  importe_mensual = ld_importe_mensual
      
      IF SQLCA.SQLCODE = NOTFOUND THEN
         --BUSCA EL IMPORTE MENSUAL 11P EN HISTORICO 
         SELECT 2
         INTO   ls_tipo_pago
         FROM   tab_pmg_historica
         WHERE  importe_mensual_11p = ld_importe_mensual
         
         IF SQLCA.SQLCODE = NOTFOUND THEN
            LET ls_tipo_pago = 0
         END IF
      END IF
   END IF
   
   IF ls_tipo_pago IS NULL OR ls_tipo_pago = 0 THEN
      LET ls_tipo_pago = fg_parametro("TIPOHISTORICODEFAULT")
      IF ls_tipo_pago IS NULL OR ls_tipo_pago = 0 THEN
         LET ls_tipo_pago = 2
      END IF 
   END IF 
   
   RETURN ls_tipo_pago

END FUNCTION

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION fg_debug(pc_mensaje)

   DEFINE pc_mensaje     CHAR(100)
   DEFINE lc_nomarchivo  CHAR(100)
   DEFINE lc_cmd         CHAR(1000)
   
   LET lc_nomarchivo = "PMGLOG_", TODAY USING "YYYYMMDD"
   
   IF fgl_getenv("DEBUGEFP") = "1" THEN 
      LET lc_cmd = "echo '",pc_mensaje CLIPPED,"' >> ",lc_nomarchivo CLIPPED
      RUN lc_cmd
      --DISPLAY pc_mensaje CLIPPED
   END IF

END FUNCTION 

#=============================================================================#
#                                                                             #
#=============================================================================#
FUNCTION pregunta_sn(pc_mensaje)
   DEFINE pc_mensaje     CHAR(80)
   DEFINE ls_bandera     SMALLINT
   DEFINE lc_tecla       CHAR(1)

   WHILE TRUE
      PROMPT pc_mensaje CLIPPED FOR CHAR lc_tecla
      
      IF lc_tecla MATCHES "[sS]" THEN
         RETURN TRUE
      ELSE
         IF lc_tecla MATCHES "[nN]" THEN
            RETURN FALSE
         END IF
      END IF
   END WHILE
   
END FUNCTION


