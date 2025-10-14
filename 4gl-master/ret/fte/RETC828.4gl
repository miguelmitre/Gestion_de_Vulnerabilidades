#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETC828  => PROVISION Y GENERACION DE LA OPERACION 05 PARA LA         #
#                     DISPOSICION DE RECURSOS EXCEDENTES AL MC POR EL           #
#                     TRABAJADOR O SUS BENEFICIARIOS - RETIRO P                 #
#Fecha creacion    => 10 DE DICIEMBRE DE 2013                                   #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   =>                                                           #
#Actualizacion     =>                                                           #
#Sistema           => RET                                                       #
#CPL-1530          => Actualizar la fecha_envio con la fecha de la provision
#################################################################################
DATABASE safre_af

GLOBALS
    DEFINE gar_precio_acc ARRAY [90] OF RECORD
        estado          SMALLINT        ,
        fecha           DATE            ,
        siefore         SMALLINT        ,
        precio_dia      DECIMAL(16,6)
    END RECORD

    DEFINE gr_ejecuta RECORD
        num_registros           INTEGER     ,
        procesa                 SMALLINT
    END RECORD

    DEFINE gr_estado RECORD
        confirmado            LIKE ret_estado.estado_solicitud ,
        procesado             LIKE ret_estado.estado_solicitud ,
        cancelado             LIKE ret_estado.estado_solicitud ,
        transicion            LIKE ret_estado.estado_solicitud ,
        liquidado             LIKE ret_estado.estado_solicitud ,
        enviado               LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE gr_seg_modulo RECORD LIKE seg_modulo.*
    DEFINE gr_datos_retiro RECORD LIKE tab_retiro.*

    DEFINE
        gdt_fecha_viv                    ,
        gdt_cambio_infonavit            ,
        HOY                             DATE

    DEFINE
        gc_tipo_ret                     CHAR(001) ,
        enter                           CHAR(001) ,
        gc_usuario                      CHAR(008) ,
        gc_ruta_archivo                 CHAR(100)

    DEFINE
        gs_num_siefores                 ,   -- Indica el numero de siefores que se usan actualmente
        gs_mov_viv97                    SMALLINT

    DEFINE #glo #integer
        gi_ult_folio            INTEGER

    DEFINE cont_reg_rech      INTEGER

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL f_lib_abre_log("RETC828")
    CALL init()
    CALL f_abre_ventana()

    CALL f_valida_ejecucion() RETURNING gr_ejecuta.*

    IF gr_ejecuta.procesa = 1 THEN
    	  CALL fn_identifica_benef_designados()
        CALL f_tablas_tmp()
        CALL primer_paso()
        CALL primer_paso_benef() #-- Calcula saldos benef
        CALL segundo_paso()
        CALL tercer_paso()
    END IF

    CLOSE WINDOW RETC8281

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    DEFINE
        lc_prepare      CHAR(300)
   
   DEFINE ls_count1           SMALLINT
   DEFINE ls_count2           SMALLINT

    -- -----------------------------------------------------------------------------
    LET ls_count1 = 0
    LET ls_count2 = 0

    LET HOY                     = TODAY
    LET gc_tipo_ret             = "P"
    LET gdt_cambio_infonavit    = "01/12/2012"
    LET gc_usuario              = f_lib_obten_user()
    LET gs_num_siefores         = 99 --f_lib_obtiene_num_siefores()

    SELECT codigo
    INTO   gs_mov_viv97
    FROM   tab_movimiento
    WHERE  descripcion  = "RETIRO DISPOSICION VIVIENDA 97 REG 73"

    SELECT *
    INTO   gr_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_estado.confirmado
    FROM   ret_estado A
    WHERE  A.descripcion = "CONFIRMADO"

    SELECT A.estado_solicitud
    INTO   gr_estado.procesado
    FROM   ret_estado A
    WHERE  A.descripcion = "PROCESADO"

    SELECT A.estado_solicitud
    INTO   gr_estado.cancelado
    FROM   ret_estado A
    WHERE  A.descripcion = "RECHAZADO"

    SELECT A.estado_solicitud
    INTO   gr_estado.transicion
    FROM   ret_estado A
    WHERE  A.descripcion = "TRANSICION"

    SELECT A.estado_solicitud
    INTO   gr_estado.liquidado
    FROM   ret_estado A
    WHERE  A.descripcion = "LIQUIDADO"

    SELECT A.estado_solicitud
    INTO   gr_estado.enviado
    FROM   ret_estado A
    WHERE  A.descripcion = "ENVIADO"

    SELECT *
    INTO   gr_datos_retiro.*
    FROM   tab_retiro
    WHERE  tipo_retiro = gc_tipo_ret

    LET lc_prepare = " "
    
    SELECT COUNT(UNIQUE nss)
    INTO   ls_count1
    FROM   ret_solicitud_tx A
    WHERE  A.tipo_retiro        = gc_tipo_ret
    AND    A.estado_solicitud   IN (gr_estado.confirmado)
    AND    A.nss                NOT IN (SELECT UNIQUE nss
                                          FROM ret_ctr_benef)

    SELECT COUNT(UNIQUE nss)
    INTO   ls_count2
    FROM   ret_solicitud_tx A
    WHERE  A.tipo_retiro      = gc_tipo_ret
    AND    A.estado_solicitud IN (gr_estado.confirmado,gr_estado.transicion)
    AND    A.nss              IN (SELECT UNIQUE nss
                                    FROM ret_ctr_benef)

    IF (ls_count1 + ls_count2) = 0 THEN
        CALL f_lib_error_msg("NO EXISTEN REGISTROS PARA PROCESAR")
        EXIT PROGRAM
    ELSE
        LET gi_ult_folio = f_lib_obtiene_ult_folio()
    END IF

    ----- FECHA VALOR VIVIENDA -----
    LET lc_prepare = " EXECUTE FUNCTION fn_obten_fecha_val ( ? ) "
    PREPARE eje_fecha_viv FROM  lc_prepare
    EXECUTE eje_fecha_viv USING HOY
                                INTO gdt_fecha_viv

    LET lc_prepare = " "
    
    ----- DESMARCA DE CUENTA -----
    LET lc_prepare = "EXECUTE PROCEDURE desmarca_cuenta( ?,?,?,?,?,? )"
    PREPARE eje_desmarca FROM lc_prepare

    LET lc_prepare = " "

END FUNCTION


FUNCTION fn_identifica_benef_designados()

   DEFINE arr_subcuentas ARRAY [8] OF SMALLINT

   DEFINE v_fecha_saldo       DATE
   DEFINE v_fecha_saldo_dia   DATE
   DEFINE v_fecha_saldo_viv   DATE
   
   DEFINE i                   SMALLINT
   DEFINE v_nss               CHAR(11)
   DEFINE v_folio_sol         INTEGER
   DEFINE v_consecutivo       DECIMAL(11,0)
   DEFINE v_consecutivo_padre DECIMAL(11,0)
   DEFINE v_consec_padre_nul  DECIMAL(11,0)
   
   DEFINE lr_saldos RECORD
        subcuenta               LIKE dis_cuenta.subcuenta           ,
        siefore                 LIKE dis_cuenta.siefore             ,
        monto_acciones       LIKE dis_cuenta.monto_en_acciones   ,
        monto_pesos          LIKE dis_cuenta.monto_en_pesos
   END RECORD
   DEFINE v_precio_del_dia    DECIMAL(19,14)
   DEFINE ls_tipo_benef       SMALLINT
   DEFINE ls_grupo            SMALLINT

    DISPLAY " IDENTIFICACIÓN DE BENEFICIARIOS " AT 19,1 ATTRIBUTE(REVERSE)
    PREPARE eje_saldo_dia_2 FROM " EXECUTE FUNCTION fn_saldo_dia ( ?,?,?,? )"

   --WHENEVER ERROR CALL fn_error

   LET arr_subcuentas[1] = 1
   LET arr_subcuentas[2] = 2
   LET arr_subcuentas[3] = 4
   LET arr_subcuentas[4] = 5
   LET arr_subcuentas[5] = 6
   LET arr_subcuentas[6] = 7
   LET arr_subcuentas[7] = 8
   LET arr_subcuentas[8] = 9
   
   --Si la fecha de vivienda es dentro del mes actual se toma el primer dia del mes
   IF MONTH(gdt_fecha_viv) = MONTH(TODAY) THEN
       LET v_fecha_saldo_viv = gdt_fecha_viv
   ELSE
      -- Si la fecha de vivienda se corre al mes siguiente, tomamos el saldo al dia
      LET v_fecha_saldo_viv = HOY
   END IF

   LET ls_grupo = 0
   INITIALIZE lr_saldos.* TO NULL

   --BARRE LAS SOLICITUDES QUE COINCIDAN CON BENEFICIARIOS DESIGNADOS
   DECLARE cur_benef CURSOR FOR 
      SELECT a.nss,a.folio_solicitud,a.consecutivo,b.tipo_benef
      FROM ret_solicitud_tx a,
           ret_ctr_benef b
      WHERE a.nss = b.nss
      AND   a.consecutivo = b.consecutivo_solic
      AND   a.tipo_retiro = gc_tipo_ret
      AND   a.estado_solicitud IN (gr_estado.confirmado)
      ORDER BY a.nss,a.folio_solicitud,a.consecutivo

   FOREACH cur_benef INTO v_nss,v_folio_sol,v_consecutivo,ls_tipo_benef
           
      IF ls_tipo_benef = 3 OR ls_tipo_benef = 1 THEN
          IF (fn_valida_benef(v_nss, v_consecutivo, ls_tipo_benef) = 1) THEN
              CONTINUE FOREACH
          END IF
      END IF
      
     CALL genera_tmp_cuenta(v_nss)

      --Busca el consecutivo padre
      SELECT MIN(consecutivo)
      INTO v_consecutivo_padre
      FROM ret_solicitud_tx
      WHERE nss            = v_nss
      AND folio_solicitud  = v_folio_sol
      AND tipo_retiro      = gc_tipo_ret
      AND estado_solicitud IN (gr_estado.confirmado)

      --Evalua si ya tiene consecutivo padre al solicitud
      SELECT UNIQUE consecutivo_padre
      INTO v_consec_padre_nul
      FROM ret_ctr_benef
      WHERE nss             = v_nss
      AND consecutivo_solic = v_consecutivo
      AND tipo_retiro = gc_tipo_ret

      IF v_consec_padre_nul IS NULL THEN  --Si no tiene consecutivo padre, le asigna el menor consecutivo de la solicitud
         UPDATE ret_ctr_benef
         SET consecutivo_padre = v_consecutivo_padre
         WHERE nss             = v_nss
         AND consecutivo_solic = v_consecutivo
         AND consecutivo_padre IS NULL
         AND tipo_retiro = gc_tipo_ret
      END IF

      IF v_consecutivo = v_consecutivo_padre THEN --Evalúa si la solicitud es el consecutivo padre. 
      
         CALL fn_verif_saldo_benef(v_nss)
      
         FOR i = 1 TO 8
             --Consulta saldo benef
             SELECT MAX(fecha_saldo)
             INTO v_fecha_saldo
             FROM ret_saldo_benef
             WHERE nss       = v_nss
               AND subcuenta = arr_subcuentas[i]
             
             -- Si no tiene saldos, llena la tabla con el saldo al día 
             IF v_fecha_saldo IS NULL THEN
                
                 -- Determinamos a que fecha se va a obtener el saldo
                 IF (i <> 3) AND (i <> 7) THEN
                     LET v_fecha_saldo_dia  = HOY
                 ELSE
                     --Si la fecha de vivienda es dentro del mes actual se toma el primer dia del mes
                     IF MONTH(gdt_fecha_viv) = MONTH(TODAY) THEN
                         LET v_fecha_saldo_dia = gdt_fecha_viv
                     ELSE
                        -- Si la fecha de vivienda se corre al mes siguiente, tomamos el saldo al dia
                        LET v_fecha_saldo_dia = HOY
                     END IF
                 END IF
                  
                 DECLARE cur_saldo_dia CURSOR FOR eje_saldo_dia_2
                 OPEN cur_saldo_dia USING v_nss,
                                          arr_subcuentas[i],
                                          ls_grupo,
                                          v_fecha_saldo_dia
                 
                 FETCH cur_saldo_dia INTO lr_saldos.*
                 CLOSE cur_saldo_dia
                 IF lr_saldos.monto_acciones > 0 AND lr_saldos.monto_pesos > 0 THEN
                    INSERT INTO ret_saldo_benef(nss,fecha_saldo,subcuenta,siefore,monto_acc,monto_pes)
                    VALUES(v_nss,HOY,lr_saldos.subcuenta,lr_saldos.siefore,lr_saldos.monto_acciones,lr_saldos.monto_pesos)
                 END IF
                
             ELSE -- Si ya tiene saldos, busca las aportaciones posteriores a la última fecha saldo
                SELECT a.siefore,a.subcuenta,SUM(a.monto_en_acciones),SUM(a.monto_en_pesos)
                  INTO lr_saldos.siefore,lr_saldos.subcuenta,lr_saldos.monto_acciones,lr_saldos.monto_pesos
                FROM tmp_dis_cuenta a,
                     tab_movimiento b
                WHERE b.codigo = a.tipo_movimiento
                AND   b.tipo = 1
                AND   a.subcuenta = arr_subcuentas[i]
                AND   a.nss = v_nss
                AND   a.fecha_conversion > v_fecha_saldo
                AND   a.tipo_movimiento IN (1, 2, 3, 1319, 1320, 1321, 1322)
                GROUP BY a.siefore,a.subcuenta
                HAVING SUM(monto_en_acciones) > 0
             
                IF lr_saldos.siefore = 11 THEN 
                   SELECT precio_del_dia
                   INTO v_precio_del_dia
                   FROM glo_valor_accion
                   WHERE codigo_siefore = lr_saldos.siefore
                   AND fecha_valuacion = v_fecha_saldo_viv
                ELSE
                   SELECT precio_del_dia
                   INTO v_precio_del_dia
                   FROM glo_valor_accion
                   WHERE codigo_siefore = lr_saldos.siefore
                   AND fecha_valuacion = HOY
                END IF
             
                LET lr_saldos.monto_pesos = lr_saldos.monto_acciones * v_precio_del_dia
             
                IF lr_saldos.monto_acciones > 0 AND lr_saldos.monto_pesos > 0 THEN
                    INSERT INTO ret_saldo_benef(nss,fecha_saldo,subcuenta,siefore,monto_acc,monto_pes)
                       VALUES(v_nss,HOY,lr_saldos.subcuenta,lr_saldos.siefore,lr_saldos.monto_acciones,lr_saldos.monto_pesos)
                END IF

             END IF
             INITIALIZE lr_saldos.* TO NULL
         END FOR
     ELSE
         --Actualiza estado de solicitud hijo a transición
         UPDATE ret_solicitud_tx
         SET estado_solicitud = gr_estado.transicion --ESTADO TRANSICIÓN
         WHERE nss           = v_nss
         AND consecutivo     = v_consecutivo
         AND folio_solicitud = v_folio_sol
         AND estado_solicitud IN (gr_estado.confirmado)
     END IF

   END FOREACH

END FUNCTION

FUNCTION fn_verif_saldo_benef(p_nss)

   DEFINE p_nss               CHAR(11)
   DEFINE v_fecha_saldo       DATE
   DEFINE v_subcuenta         SMALLINT

    DECLARE cur_verif_saldo CURSOR FOR
        SELECT UNIQUE fecha_saldo, subcuenta
          FROM ret_saldo_benef
         WHERE nss = p_nss
         ORDER BY 1 DESC, 2 ASC

    INITIALIZE v_fecha_saldo, v_subcuenta TO NULL
    -- Iniciamos ciclo para cada fecha
    FOREACH cur_verif_saldo INTO v_fecha_saldo, v_subcuenta
        
        SELECT "OK"
           FROM dis_provision dp,
                tmp_dis_cuenta dis
          WHERE dp.nss              = p_nss
            AND dp.fecha_conversion = v_fecha_saldo
            AND dp.subcuenta        = v_subcuenta
            AND dp.nss              = dis.nss
            AND dp.consecutivo_lote = dis.consecutivo_lote
            AND dp.subcuenta        = dis.subcuenta
         UNION
         SELECT "OK"
           FROM dis_provision dp,
                ret_solicitud_tx rst
          WHERE dp.nss              = p_nss
            AND dp.fecha_conversion = v_fecha_saldo
            AND dp.subcuenta        = v_subcuenta
            AND dp.nss              = rst.nss
            AND dp.consecutivo_lote = rst.consecutivo
            AND (rst.estado_solicitud  = gr_estado.enviado
                 AND (rst.diag_registro = '0' OR rst.diag_registro IS NULL OR rst.diag_registro = "   "))
            AND rst.tipo_retiro        = gc_tipo_ret
          GROUP BY 1
         
         IF STATUS = NOTFOUND THEN
             DELETE
               FROM ret_saldo_benef 
              WHERE nss = p_nss
                AND fecha_saldo = v_fecha_saldo
                AND subcuenta   = v_subcuenta
         END IF
        
        INITIALIZE v_fecha_saldo, v_subcuenta TO NULL
    END FOREACH

END FUNCTION

#=============================================================================#
# Funcion  : genera_tmp_dis_cuenta                                            #
# Objetivo : Generar la tabla temporal <tmp_dis_cuenta> con la informacion    #
#            historica de las dis_cuenta solo para el NSS recibido            #
#=============================================================================#
FUNCTION genera_tmp_cuenta(p_nss)

    DEFINE p_nss              CHAR(11)
    DEFINE v_nombre_tabla     CHAR(20)
    DEFINE sel_his            CHAR(1500)

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_dis_cuenta;
    WHENEVER ERROR STOP

    DECLARE cur_his CURSOR FOR
    SELECT tabname
    FROM systables
    WHERE tabname matches "dis_cuenta??"

    FOREACH cur_his INTO v_nombre_tabla

        LET sel_his = sel_his CLIPPED,
                      " SELECT * ",
                      "  FROM ",v_nombre_tabla          ,
                      " WHERE nss = ","'",p_nss,"'"  ,
                      "  UNION ALL "
    END FOREACH

    CLOSE cur_his

    LET sel_his = sel_his CLIPPED,
                  " SELECT * ",
                  "  FROM dis_cuenta ",
                  " WHERE nss = ","'",p_nss,"'"  ,
                  " INTO TEMP tmp_dis_cuenta "

    PREPARE eje_sel_his FROM sel_his

    EXECUTE eje_sel_his

    CREATE INDEX tmp_dis_cuenta1 on tmp_dis_cuenta ( folio,
                                                     consecutivo_lote,
                                                     subcuenta,
                                                     siefore
                                                   )
    UPDATE STATISTICS FOR TABLE tmp_dis_cuenta

END FUNCTION

FUNCTION fn_valida_benef(p_nss, p_consecutivo, p_tipo_benef)

    DEFINE p_nss           LIKE ret_solicitud_tx.nss
    DEFINE p_consecutivo   LIKE ret_solicitud_tx.consecutivo
    DEFINE p_tipo_benef    SMALLINT

    DEFINE lc_curp         LIKE ret_beneficiario.curp_benef
    DEFINE ls_indicador    SMALLINT
    DEFINE ls_porcentaje   SMALLINT

    LET ls_indicador  = 0
    LET ls_porcentaje = 0

    IF p_tipo_benef = 3 THEN
        SELECT curp_benef
          INTO lc_curp
          FROM ret_beneficiario
         WHERE nss         = p_nss
           AND consecutivo = p_consecutivo

        SELECT "OK"
          FROM afi_mae_benefici a,
               afi_ctr_beneficiario b
         WHERE a.n_seguro       = p_nss
           AND a.curp           = lc_curp
           AND a.n_folio        = b.n_folio
           AND a.tipo_solicitud = b.tipo_solicitud
           AND b.tipo_beneficiario = 2    -- CPL-3664
           GROUP BY 1

        IF STATUS = NOTFOUND THEN
            UPDATE ret_solicitud_tx
               SET estado_solicitud    = gr_estado.cancelado,
                   cod_rechazo_ent     = 320,
                   folio               = gi_ult_folio
             WHERE nss                 = p_nss
               AND estado_solicitud    IN (gr_estado.confirmado, gr_estado.transicion)

            LET ls_indicador = 1
        END IF
    END IF

--    SELECT SUM(porcentaje)
--      INTO ls_porcentaje
--      FROM ret_ctr_benef
--     WHERE nss         = p_nss
--       AND tipo_benef  = p_tipo_benef
--       AND folio_liquida IS NULL
--       AND tipo_retiro = gc_tipo_ret
    SELECT SUM(porcentaje)
      INTO ls_porcentaje
      FROM ret_ctr_benef a,
           ret_solicitud_tx b
     WHERE a.nss = b.nss
       AND a.consecutivo_solic = b.consecutivo
       AND a.nss           = p_nss
       AND a.tipo_benef    = p_tipo_benef
       AND a.folio_liquida IS NULL
       AND a.tipo_retiro   = gc_tipo_ret
       AND b.estado_solicitud    IN (gr_estado.confirmado, gr_estado.transicion)
    IF (ls_porcentaje > 100) THEN
        UPDATE ret_solicitud_tx
           SET estado_solicitud    = gr_estado.cancelado,
               cod_rechazo_ent     = 321,
               folio               = gi_ult_folio
         WHERE nss                 = p_nss
           AND estado_solicitud    IN (gr_estado.confirmado, gr_estado.transicion)

        LET ls_indicador = 1
    END IF

    RETURN ls_indicador

END FUNCTION

FUNCTION fn_desmarca_cuenta()

    DEFINE lr_desmarca         RECORD
           nss                 LIKE cta_his_marca.nss          ,
           movimiento          LIKE cta_his_marca.marca_cod    ,
           consecutivo         LIKE cta_his_marca.correlativo  ,
           edo_causa           LIKE cta_his_marca.estado_marca ,
           marca_causa         LIKE cta_his_marca.marca_causa
           END RECORD
    
    DECLARE cur_desmarca CURSOR FOR
        SELECT A.nss                ,
               A.marca_cod          ,
               A.correlativo        ,
               0                    ,
               0
        FROM   cta_his_marca A      ,
               ret_solicitud_tx B
        WHERE  A.correlativo        = B.consecutivo
        AND    A.nss                = B.nss
        AND    A.marca_cod          = gr_datos_retiro.movimiento
        AND    B.tipo_retiro        = gc_tipo_ret
        AND    B.folio              = gi_ult_folio
        AND    A.fecha_fin IS NULL
        AND    B.estado_solicitud   = gr_estado.cancelado
        ORDER BY 1
       
    LET cont_reg_rech = 0

    FOREACH cur_desmarca INTO lr_desmarca.*
    
        EXECUTE eje_desmarca USING lr_desmarca.*, gc_usuario
        
        LET cont_reg_rech = cont_reg_rech + 1
        
    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# primer_paso : Obtiene los saldos que se usaran para la provision          #
#---------------------------------------------------------------------------#
FUNCTION primer_paso()

    DEFINE lr_solicitud RECORD LIKE ret_solicitud_tx.*

    DEFINE lar_siefore ARRAY[99] OF RECORD         -- CPL-2959
        activo                  SMALLINT        ,
        acciones_ret97          DECIMAL(16,6)   ,
        acciones_cv             DECIMAL(16,6)   ,
        acciones_cs             DECIMAL(16,6)   ,
        acciones_est            DECIMAL(16,6)   ,
        acciones_ret92          DECIMAL(16,6)   ,
        acciones_esp            DECIMAL(16,6)
    END RECORD

    DEFINE lr_viv97 RECORD
        subcuenta               SMALLINT        ,
        siefore                 SMALLINT        ,
        acciones                DECIMAL(16,6)   ,
        pesos                   DECIMAL(16,6)
    END RECORD

    DEFINE lr_mto_prov RECORD
        subcuenta               LIKE dis_cuenta.subcuenta           ,
        siefore                 LIKE dis_cuenta.siefore             ,
        monto_en_acciones       LIKE dis_cuenta.monto_en_acciones   ,
        monto_en_pesos          LIKE dis_cuenta.monto_en_pesos
    END RECORD

    DEFINE
        ld_fecha_saldo          ,
        ld_fecha_saldo_viv      DATE

    DEFINE
        ls_siefore              , #-- contador para los ciclos for
        f_siefore               ,
        ls_subcta_grupo         SMALLINT

     DEFINE
        ld_acc_CV_total         ,
        li_acc_viv97            ,
        li_acc_viv92            DECIMAL(16,6)

    -- -----------------------------------------------------------------------------

    DISPLAY "CALCULANDO MONTOS ...." AT 7,2

    --Si la fecha de vivienda es dentro del mes actual se toma el primer dia del mes*
    IF MONTH(gdt_fecha_viv) = MONTH(TODAY) THEN
        LET ld_fecha_saldo_viv = gdt_fecha_viv
    ELSE
       -- Si la fecha de vivienda se corre al mes siguiente, tomamos el saldo al dia
       LET ld_fecha_saldo_viv = HOY
    END IF

    DECLARE cur_sol CURSOR FOR
    SELECT *
    FROM   ret_solicitud_tx
    WHERE  tipo_retiro         = gc_tipo_ret
    AND    estado_solicitud    = gr_estado.confirmado
    AND    (rechazo_cod = 0 OR rechazo_cod IS NULL)
    AND     nss NOT IN (SELECT UNIQUE nss
                          FROM ret_ctr_benef)
    ORDER BY nss

    -- Iniciamos ciclo para cada nss
    FOREACH cur_sol INTO lr_solicitud.*

        -- Inicializamos variables del arreglo
        FOR ls_siefore = 1 TO gs_num_siefores
            LET lar_siefore[ls_siefore].activo          = FALSE
            LET lar_siefore[ls_siefore].acciones_ret97  = 0
            LET lar_siefore[ls_siefore].acciones_cv     = 0
            LET lar_siefore[ls_siefore].acciones_cs     = 0
            LET lar_siefore[ls_siefore].acciones_est    = 0
            LET lar_siefore[ls_siefore].acciones_ret92  = 0
            LET lar_siefore[ls_siefore].acciones_esp    = 0
        END FOR

        LET li_acc_viv97    = 0
        LET li_acc_viv92    = 0

        DECLARE cur_grupo CURSOR FOR
        SELECT subcuenta
        FROM   tab_agrupa_subcta
        WHERE  grupo     = lr_solicitud.grupo
        AND    subcuenta > 0

        -- Iniciamos ciclo para cada subcuenta del nss actual
        FOREACH cur_grupo INTO ls_subcta_grupo

            LET lr_mto_prov.subcuenta           = 0
            LET lr_mto_prov.siefore             = 0
            LET lr_mto_prov.monto_en_acciones   = 0
            LET lr_mto_prov.monto_en_pesos      = 0

            -- Determinamos a que fecha se va a obtener el saldo
            IF (ls_subcta_grupo <> 4) AND (ls_subcta_grupo <> 8) THEN
                LET ld_fecha_saldo  = HOY
            ELSE
                --Si la fecha de vivienda es dentro del mes actual se toma el primer dia del mes
                IF MONTH(gdt_fecha_viv) = MONTH(TODAY) THEN
                    LET ld_fecha_saldo = gdt_fecha_viv
                ELSE
                   -- Si la fecha de vivienda se corre al mes siguiente, tomamos el saldo al dia
                   LET ld_fecha_saldo = HOY
                END IF
            END IF

            CALL f_obten_saldo_subcta(lr_solicitud.nss, ls_subcta_grupo, ld_fecha_saldo)
                RETURNING lr_mto_prov.*

            --Si el tipo de pension es IP y el porcentaje es mayor o igual a 50 se paga
            --el total de recursos. En otro caso no se paga.
            IF lr_solicitud.tipo_pension  = "IP" AND lr_solicitud.porcentaje_val < 50 THEN
                LET lr_mto_prov.monto_en_acciones   = 0
                LET lr_mto_prov.monto_en_pesos      = 0
            END IF

            IF lr_mto_prov.monto_en_acciones > 0 THEN

                LET ls_siefore  = lr_mto_prov.siefore

                IF lr_mto_prov.siefore <> 11 THEN

                    -- Marcamos como activo el registro de la siefore actual
                    LET lar_siefore[ls_siefore].activo = TRUE

                    CASE lr_mto_prov.subcuenta
                        WHEN 1
                            LET lar_siefore[ls_siefore].acciones_ret97 = lr_mto_prov.monto_en_acciones

                        WHEN 2
                            LET lar_siefore[ls_siefore].acciones_cv    = lr_mto_prov.monto_en_acciones
                        WHEN 5
                            LET lar_siefore[ls_siefore].acciones_cs    = lr_mto_prov.monto_en_acciones
                        WHEN 6
                            LET lar_siefore[ls_siefore].acciones_est   = lr_mto_prov.monto_en_acciones

                        WHEN 7
                            LET lar_siefore[ls_siefore].acciones_ret92 = lr_mto_prov.monto_en_acciones

                        WHEN 9
                            LET lar_siefore[ls_siefore].acciones_esp   = lr_mto_prov.monto_en_acciones

                        OTHERWISE
                            -- Para evitar que se provisionen subcuentas que no corresponden a este tipo de retiro
                            LET lr_mto_prov.monto_en_acciones   = 0
                            LET lr_mto_prov.monto_en_pesos      = 0
                    END CASE
                ELSE
                    CASE lr_mto_prov.subcuenta
                        WHEN 4
                            LET li_acc_viv97    = lr_mto_prov.monto_en_acciones

                        WHEN 8
                            LET li_acc_viv92    = lr_mto_prov.monto_en_acciones
                    END CASE
                END IF

                CALL f_provisiona_subcta(lr_solicitud.nss               ,
                                         lr_solicitud.curp              ,
                                         lr_mto_prov.subcuenta          ,
                                         lr_solicitud.consecutivo       ,
                                         lr_mto_prov.monto_en_acciones  ,
                                         lr_mto_prov.monto_en_pesos     ,
                                         ld_fecha_saldo                 ,
                                         gr_datos_retiro.movimiento     ,
                                         lr_mto_prov.siefore
                                        )
            END IF

        END FOREACH -- Subcuentas

        FOR ls_siefore = 1 TO gs_num_siefores
            -- Almacenamos solo los registros de las siefores que contengan saldo
            IF lar_siefore[ls_siefore].activo = TRUE THEN

                LET ld_acc_CV_total = lar_siefore[ls_siefore].acciones_cv  +
                                      lar_siefore[ls_siefore].acciones_est +
                                      lar_siefore[ls_siefore].acciones_esp

                INSERT INTO safre_tmp:tmp_monto_siefore
                    VALUES(lr_solicitud.nss                         ,
                           lr_solicitud.consecutivo                 ,
                           1                                        ,
                           gc_tipo_ret                              ,
                           5                                        ,
                           ls_siefore                               ,
                           lar_siefore[ls_siefore].acciones_ret97   ,
                           ld_acc_CV_total                          ,
                           lar_siefore[ls_siefore].acciones_cs      ,
                           lar_siefore[ls_siefore].acciones_ret92
                          )
            END IF
        END FOR

        INSERT INTO safre_tmp:tmp_monto_viv
        VALUES(lr_solicitud.nss             , -- nss
               lr_solicitud.consecutivo     , -- consecutivo
               1                            , -- folio
               gc_tipo_ret                  , -- tipo_retiro
               gdt_fecha_viv                 , -- fecha_valor_viv
               li_acc_viv97                 , -- acciones_viv97
               li_acc_viv92                 , -- acciones_viv92
               0                            , -- pesos_viv72
               NULL                         , -- estado_sub_viv
               0                            , -- acc_viv97_bdsviv
               0                              -- acc_viv92_bdsviv
              )

    END FOREACH -- Siguiente NSS

    DISPLAY "(TERMINADO)" AT 7,36

END FUNCTION

#---------------------------------------------------------------------------#
# primer_paso_benef : Obtiene los saldos que se usaran para la provision    #
#---------------------------------------------------------------------------#
FUNCTION primer_paso_benef()

    DEFINE lr_solicitud RECORD LIKE ret_solicitud_tx.*

    DEFINE lar_siefore ARRAY[99] OF RECORD         -- CPL-2959
        activo                  SMALLINT        ,
        acciones_ret97          DECIMAL(16,6)   ,
        acciones_cv             DECIMAL(16,6)   ,
        acciones_cs             DECIMAL(16,6)   ,
        acciones_est            DECIMAL(16,6)   ,
        acciones_ret92          DECIMAL(16,6)   ,
        acciones_esp            DECIMAL(16,6)
    END RECORD

    DEFINE lr_viv97 RECORD
        subcuenta               SMALLINT        ,
        siefore                 SMALLINT        ,
        acciones                DECIMAL(16,6)   ,
        pesos                   DECIMAL(16,6)
    END RECORD

    DEFINE lr_mto_prov RECORD
        subcuenta               LIKE dis_cuenta.subcuenta           ,
        siefore                 LIKE dis_cuenta.siefore             ,
        monto_en_acciones       LIKE dis_cuenta.monto_en_acciones   ,
        monto_en_pesos          LIKE dis_cuenta.monto_en_pesos
    END RECORD

    DEFINE
        ld_fecha_saldo          ,
        ld_fecha_saldo_viv      DATE

    DEFINE
        ls_siefore              , #-- contador para los ciclos for
        f_siefore               ,
        ls_subcta_grupo         SMALLINT

     DEFINE
        ld_acc_CV_total         ,
        li_acc_viv97            ,
        li_acc_viv92            DECIMAL(16,6)
        
    DEFINE ls_tipo_benef         SMALLINT
    DEFINE aux_tipo_benef        SMALLINT
    DEFINE ld_porcentaje         DECIMAL(5,2)
    DEFINE ld_fecha_saldo_benef  DATE
    DEFINE ld_consecutivo        DECIMAL(11,0)
    DEFINE lc_curp               LIKE ret_beneficiario.curp_benef
    DEFINE ls_ind                SMALLINT
    DEFINE ls_count_reg          SMALLINT
    DEFINE ls_grupo              SMALLINT
    
    DEFINE lr_saldos RECORD
        subcuenta               LIKE dis_cuenta.subcuenta           ,
        siefore                 LIKE dis_cuenta.siefore             ,
        monto_acciones       LIKE dis_cuenta.monto_en_acciones   ,
        monto_pesos          LIKE dis_cuenta.monto_en_pesos
   END RECORD
   
   LET ls_grupo = 0

    -- -----------------------------------------------------------------------------

    DISPLAY "CALCULANDO MONTOS ...." AT 7,2

    DECLARE cur_sol_benef CURSOR FOR
    SELECT A.*
    FROM   ret_solicitud_tx A
    WHERE  A.tipo_retiro         = gc_tipo_ret
    AND    A.estado_solicitud    IN (gr_estado.confirmado, gr_estado.transicion)
    AND    (A.rechazo_cod = 0 OR A.rechazo_cod IS NULL)
    AND     A.nss IN (SELECT UNIQUE B.nss
                        FROM ret_ctr_benef B
                       WHERE B.consecutivo_solic = A.consecutivo)
    ORDER BY nss

    -- Iniciamos ciclo para cada nss
    FOREACH cur_sol_benef INTO lr_solicitud.*
    
        CALL genera_tmp_cuenta(lr_solicitud.nss)

        DECLARE cur_ctr_benef_1 CURSOR FOR
        SELECT tipo_benef, porcentaje
          FROM ret_ctr_benef
         WHERE nss               = lr_solicitud.nss
           AND consecutivo_solic = lr_solicitud.consecutivo
           AND tipo_retiro       = gc_tipo_ret

        FOREACH cur_ctr_benef_1 INTO ls_tipo_benef,
                                     ld_porcentaje
            
            LET aux_tipo_benef = ls_tipo_benef
            
            IF ls_tipo_benef = 1 THEN
                SELECT "OK"
                  FROM afi_mae_benefici a,
                       afi_ctr_beneficiario b
                 WHERE a.n_seguro       = lr_solicitud.nss
                   AND a.n_folio        = b.n_folio
                   AND a.tipo_solicitud = b.tipo_solicitud
                   AND b.tipo_beneficiario = 2    -- CPL-3664
                 GROUP BY 1
                IF STATUS = NOTFOUND THEN
                    LET aux_tipo_benef = 4
                END IF
            END IF

            INITIALIZE lr_solicitud.grupo TO NULL
            SELECT grupo
              INTO lr_solicitud.grupo
              FROM ret_matriz_benef
             WHERE regimen         = lr_solicitud.regimen
               AND tipo_retiro     = gc_tipo_ret
               AND tipo_prestacion = lr_solicitud.tipo_prestacion
               AND tipo_benef      = aux_tipo_benef
            
            IF (lr_solicitud.grupo IS NULL OR lr_solicitud.grupo = 0) THEN
            	  UPDATE ret_solicitud_tx
                   SET estado_solicitud    = gr_estado.cancelado,
                       cod_rechazo_ent     = 322,
                       folio               = gi_ult_folio
                 WHERE nss                 = lr_solicitud.nss
                   AND consecutivo         = lr_solicitud.consecutivo
                   AND tipo_retiro         = gc_tipo_ret
                   AND estado_solicitud    IN (gr_estado.confirmado, gr_estado.transicion)
            END IF
            
            DECLARE cur_grupo_benef CURSOR FOR
            SELECT subcuenta
            FROM   tab_agrupa_subcta
            WHERE  grupo     = lr_solicitud.grupo
            AND    subcuenta > 0
            
            -- Iniciamos ciclo para cada subcuenta del nss actual
            FOREACH cur_grupo_benef INTO ls_subcta_grupo
            
                LET ls_ind                          = 0
                LET lr_mto_prov.subcuenta           = 0
                LET lr_mto_prov.siefore             = 0
                LET lr_mto_prov.monto_en_acciones   = 0
                LET lr_mto_prov.monto_en_pesos      = 0
            
                DECLARE cur_saldo_benef CURSOR FOR
                SELECT siefore, subcuenta, monto_acc, monto_pes, fecha_saldo
                  FROM ret_saldo_benef
                 WHERE nss       = lr_solicitud.nss
                   AND subcuenta = ls_subcta_grupo
                 ORDER BY fecha_saldo ASC
                   
                FOREACH cur_saldo_benef INTO lr_mto_prov.siefore,
                                             lr_mto_prov.subcuenta,
                                             lr_mto_prov.monto_en_acciones,
                                             lr_mto_prov.monto_en_pesos,
                                             ld_fecha_saldo_benef
                    
                    SELECT "OK"
                      FROM dis_provision dp,
                           tmp_dis_cuenta dc,
                           ret_ctr_benef rcb
                     WHERE dp.nss                = lr_solicitud.nss
                       AND dp.subcuenta          = lr_mto_prov.subcuenta
                       AND dp.fecha_conversion   = ld_fecha_saldo_benef
                       AND dp.nss                = dc.nss
                       AND dp.consecutivo_lote   = dc.consecutivo_lote
                       AND dp.subcuenta          = dc.subcuenta
                       AND rcb.nss               = dp.nss
                       AND rcb.consecutivo_solic = dp.consecutivo_lote
                       AND rcb.porcentaje = 100
                       AND rcb.tipo_benef = ls_tipo_benef
                     GROUP BY 1
            
                    IF STATUS <> NOTFOUND THEN
                    	  LET ls_ind = 1
                    	  CONTINUE FOREACH
                    END IF
            
                    SELECT curp_benef
                      INTO lc_curp
                      FROM ret_beneficiario
                     WHERE nss         = lr_solicitud.nss
                       AND consecutivo = lr_solicitud.consecutivo
            
                    DECLARE cur_busca_cons_benef CURSOR FOR
                     SELECT UNIQUE a.consecutivo
                       FROM ret_beneficiario a,
				                    ret_solicitud_tx b,
				                    ret_ctr_benef c
                      WHERE a.nss                = lr_solicitud.nss
                        AND a.curp_benef         = lc_curp
					              AND a.nss              = b.nss
					              AND a.consecutivo      = b.consecutivo
					              AND a.nss              = c.nss
					              AND a.consecutivo      = c.consecutivo_solic
					              AND (b.estado_solicitud IN (gr_estado.confirmado,
					                                         gr_estado.transicion,
					                                         gr_estado.liquidado)
					              OR (b.estado_solicitud = gr_estado.enviado
                            AND (b.diag_registro = '0' OR b.diag_registro IS NULL OR b.diag_registro = "   ")))
					              AND c.tipo_benef       = ls_tipo_benef
					              AND c.tipo_retiro      = gc_tipo_ret
					            ORDER BY a.consecutivo ASC
                    
                    LET ld_consecutivo = 0
            
                    FOREACH cur_busca_cons_benef INTO ld_consecutivo
                        
                        IF ld_consecutivo <> lr_solicitud.consecutivo AND ls_ind = 1 THEN
                        	  CONTINUE FOREACH
                        END IF
                    
                        SELECT "OK"
                          FROM dis_provision dp,
                               tmp_dis_cuenta dc
                         WHERE dp.nss              = lr_solicitud.nss
                           AND dp.consecutivo_lote = ld_consecutivo
                           AND dp.subcuenta        = lr_mto_prov.subcuenta
                           AND dp.fecha_conversion = ld_fecha_saldo_benef
                           AND dp.nss              = dc.nss
                           AND dp.consecutivo_lote = dc.consecutivo_lote
                           AND dp.subcuenta        = dc.subcuenta
                         GROUP BY 1
            
                        IF STATUS = NOTFOUND THEN
                        
                            LET ls_count_reg = 0
                            SELECT COUNT(*)
                              INTO ls_count_reg
                              FROM ret_saldo_benef
                             WHERE nss = lr_solicitud.nss
                               AND subcuenta = lr_mto_prov.subcuenta
                                   
                            IF ld_consecutivo <> lr_solicitud.consecutivo AND ls_count_reg = 1 THEN
                            	  CONTINUE FOREACH
                            END IF
            
                            --Si el tipo de pension es IP y el porcentaje es mayor o igual a 50 se paga
                            --el total de recursos. En otro caso no se paga.
                            IF lr_solicitud.tipo_pension  = "IP" AND lr_solicitud.porcentaje_val < 50 THEN
                                LET lr_mto_prov.monto_en_acciones   = 0
                                LET lr_mto_prov.monto_en_pesos      = 0
                            END IF
                            
                            IF lr_mto_prov.monto_en_acciones > 0 THEN
            
                                LET ls_siefore  = lr_mto_prov.siefore
            
                                LET lr_mto_prov.monto_en_acciones = lr_mto_prov.monto_en_acciones * (ld_porcentaje / 100)
                                
                                PREPARE eje_saldo_dia_3 FROM " EXECUTE FUNCTION fn_saldo_dia ( ?,?,?,? )"
                                INITIALIZE lr_saldos.* TO NULL
                                DECLARE cur_saldo_dia_2 CURSOR FOR eje_saldo_dia_3
                                OPEN cur_saldo_dia_2 USING lr_solicitud.nss,
                                                           lr_mto_prov.subcuenta,
                                                           ls_grupo,
                                                           HOY
                        
                                FETCH cur_saldo_dia_2 INTO lr_saldos.*
                                CLOSE cur_saldo_dia_2
                            
                                IF lr_saldos.monto_acciones IS NULL THEN
                                	   LET lr_saldos.monto_acciones = 0
                                END IF
                        
                                IF lr_mto_prov.monto_en_acciones > lr_saldos.monto_acciones THEN
                                	  LET lr_mto_prov.monto_en_acciones = lr_saldos.monto_acciones
                                END IF
                                
                                LET lr_mto_prov.monto_en_pesos = lr_mto_prov.monto_en_acciones * gar_precio_acc[ls_siefore].precio_dia
                                
                                IF lr_mto_prov.subcuenta = 8 OR lr_mto_prov.subcuenta = 4 THEN
                                    LET lr_mto_prov.monto_en_pesos = f_lib_redondea_val(lr_mto_prov.monto_en_pesos, 2)
                                END IF
                                
                                SELECT "OK"
                                  FROM ret_ctr_benef_det
                                 WHERE nss               = lr_solicitud.nss
                                   AND consecutivo_solic = lr_solicitud.consecutivo
                                   AND folio_liquida     = gi_ult_folio
                                   AND subcuenta         = lr_mto_prov.subcuenta
                                   
                                IF STATUS = NOTFOUND THEN
                                
                                    INSERT INTO ret_ctr_benef_det VALUES(lr_solicitud.nss              ,
                                                                         lr_solicitud.consecutivo      ,
                                                                         gi_ult_folio                  ,
                                                                         lr_mto_prov.subcuenta         ,
                                                                         lr_mto_prov.monto_en_pesos    ,
                                                                         lr_mto_prov.monto_en_acciones ,
                                                                         NULL                          ,
                                                                         NULL                          ,
                                                                         lr_mto_prov.siefore           ,
                                                                         NULL                          ,
                                                                         NULL
                                                                         )
                                ELSE
                                    UPDATE ret_ctr_benef_det 
                                       SET monto_total     = monto_total    + lr_mto_prov.monto_en_pesos,
                                           acciones_bruto  = acciones_bruto + lr_mto_prov.monto_en_acciones
                                     WHERE nss             = lr_solicitud.nss
                                     AND consecutivo_solic = lr_solicitud.consecutivo
                                     AND folio_liquida     = gi_ult_folio
                                     AND subcuenta         = lr_mto_prov.subcuenta
                                END IF
                            END IF
                        ELSE
                            LET ls_ind = 1
                            EXIT FOREACH
                        END IF
                    END FOREACH
                END FOREACH
            END FOREACH -- Subcuentas
        END FOREACH -- benef
    END FOREACH -- Siguiente NSS

    DECLARE cur_sol_benef_2 CURSOR FOR
    SELECT A.*
    FROM   ret_solicitud_tx A
    WHERE  A.tipo_retiro         = gc_tipo_ret
    AND    A.estado_solicitud    = gr_estado.confirmado
    AND    (A.rechazo_cod = 0 OR A.rechazo_cod IS NULL)
    AND     A.nss IN (SELECT UNIQUE B.nss
                        FROM ret_ctr_benef B
                       WHERE B.consecutivo_solic = A.consecutivo)
    ORDER BY nss

    -- Iniciamos ciclo para cada nss
    FOREACH cur_sol_benef_2 INTO lr_solicitud.*

        -- Inicializamos variables del arreglo
        FOR ls_siefore = 1 TO gs_num_siefores
            LET lar_siefore[ls_siefore].activo          = FALSE
            LET lar_siefore[ls_siefore].acciones_ret97  = 0
            LET lar_siefore[ls_siefore].acciones_cv     = 0
            LET lar_siefore[ls_siefore].acciones_cs     = 0
            LET lar_siefore[ls_siefore].acciones_est    = 0
            LET lar_siefore[ls_siefore].acciones_ret92  = 0
            LET lar_siefore[ls_siefore].acciones_esp    = 0
        END FOR

        LET li_acc_viv97    = 0
        LET li_acc_viv92    = 0

        LET lr_mto_prov.subcuenta           = 0
        LET lr_mto_prov.siefore             = 0
        LET lr_mto_prov.monto_en_acciones   = 0
        LET lr_mto_prov.monto_en_pesos      = 0
    
        DECLARE cur_grupo_benef_2 CURSOR FOR
        SELECT siefore, subcuenta, SUM(acciones_bruto), SUM(monto_total)
          FROM ret_ctr_benef_det
         WHERE nss               = lr_solicitud.nss
           AND consecutivo_solic = lr_solicitud.consecutivo
           AND folio_liquida     = gi_ult_folio
         GROUP BY 1,2
             
        FOREACH cur_grupo_benef_2 INTO lr_mto_prov.siefore,
                                       lr_mto_prov.subcuenta,
                                       lr_mto_prov.monto_en_acciones,
                                       lr_mto_prov.monto_en_pesos

            IF lr_mto_prov.monto_en_acciones > 0 THEN

                LET ls_siefore  = lr_mto_prov.siefore

                IF lr_mto_prov.siefore <> 11 THEN

                    -- Marcamos como activo el registro de la siefore actual
                    LET lar_siefore[ls_siefore].activo = TRUE

                    CASE lr_mto_prov.subcuenta
                        WHEN 1
                            LET lar_siefore[ls_siefore].acciones_ret97 = lr_mto_prov.monto_en_acciones

                        WHEN 2
                            LET lar_siefore[ls_siefore].acciones_cv    = lr_mto_prov.monto_en_acciones
                        WHEN 5
                            LET lar_siefore[ls_siefore].acciones_cs    = lr_mto_prov.monto_en_acciones
                        WHEN 6
                            LET lar_siefore[ls_siefore].acciones_est   = lr_mto_prov.monto_en_acciones

                        WHEN 7
                            LET lar_siefore[ls_siefore].acciones_ret92 = lr_mto_prov.monto_en_acciones

                        WHEN 9
                            LET lar_siefore[ls_siefore].acciones_esp   = lr_mto_prov.monto_en_acciones

                        OTHERWISE
                            -- Para evitar que se provisionen subcuentas que no corresponden a este tipo de retiro
                            LET lr_mto_prov.monto_en_acciones   = 0
                            LET lr_mto_prov.monto_en_pesos      = 0
                    END CASE
                ELSE
                    CASE lr_mto_prov.subcuenta
                        WHEN 4
                            LET li_acc_viv97    = lr_mto_prov.monto_en_acciones

                        WHEN 8
                            LET li_acc_viv92    = lr_mto_prov.monto_en_acciones
                    END CASE
                END IF

                CALL f_provisiona_subcta(lr_solicitud.nss               ,
                                         lr_solicitud.curp              ,
                                         lr_mto_prov.subcuenta          ,
                                         lr_solicitud.consecutivo       ,
                                         lr_mto_prov.monto_en_acciones  ,
                                         lr_mto_prov.monto_en_pesos     ,
                                         ld_fecha_saldo                 ,
                                         gr_datos_retiro.movimiento     ,
                                         lr_mto_prov.siefore
                                        )
            END IF

        END FOREACH

        FOR ls_siefore = 1 TO gs_num_siefores
            -- Almacenamos solo los registros de las siefores que contengan saldo
            IF lar_siefore[ls_siefore].activo = TRUE THEN

                LET ld_acc_CV_total = lar_siefore[ls_siefore].acciones_cv  +
                                      lar_siefore[ls_siefore].acciones_est +
                                      lar_siefore[ls_siefore].acciones_esp

                INSERT INTO safre_tmp:tmp_monto_siefore
                    VALUES(lr_solicitud.nss                         ,
                           lr_solicitud.consecutivo                 ,
                           1                                        ,
                           gc_tipo_ret                              ,
                           5                                        ,
                           ls_siefore                               ,
                           lar_siefore[ls_siefore].acciones_ret97   ,
                           ld_acc_CV_total                          ,
                           lar_siefore[ls_siefore].acciones_cs      ,
                           lar_siefore[ls_siefore].acciones_ret92
                          )
            END IF
        END FOR

        INSERT INTO safre_tmp:tmp_monto_viv
        VALUES(lr_solicitud.nss             , -- nss
               lr_solicitud.consecutivo     , -- consecutivo
               1                            , -- folio
               gc_tipo_ret                  , -- tipo_retiro
               gdt_fecha_viv                 , -- fecha_valor_viv
               li_acc_viv97                 , -- acciones_viv97
               li_acc_viv92                 , -- acciones_viv92
               0                            , -- pesos_viv72
               NULL                         , -- estado_sub_viv
               0                            , -- acc_viv97_bdsviv
               0                              -- acc_viv92_bdsviv
              )

    END FOREACH -- Siguiente NSS

    DISPLAY "(TERMINADO)" AT 7,36

END FUNCTION

#---------------------------------------------------------------------------#
# segundo_paso : Genera el archivo plano de la op. 05 de acuerdo a los      #
#                montos provisionados                                       #
#---------------------------------------------------------------------------#
FUNCTION segundo_paso()

    DEFINE lr_solicitud RECORD LIKE ret_solicitud_tx.*
    DEFINE lr_vivienda  RECORD LIKE ret_monto_viv.*


    DEFINE lr_mto_sie RECORD
        siefore                 SMALLINT        ,
        acciones_ret97          DECIMAL(16,6)   ,
        acciones_cv             DECIMAL(16,6)   ,
        acciones_cs             DECIMAL(16,6)   ,
        acciones_ret92          DECIMAL(16,6)
    END RECORD

    DEFINE lr_info_biometrica RECORD
        idsolicitante        CHAR(02),
        curpsolicitante      CHAR(18),
        sellotrabajador      CHAR(14),
        curpagenteservicio   CHAR(18),
        identificador        INTEGER ,
        tiposervicio         INTEGER
    END RECORD

    DEFINE
        lc_comando                  ,
        lc_ruta_det_nss             ,
        lc_ruta_det_sie             ,
        lc_ruta_det_viv             CHAR(100)

    DEFINE
        ls_flag                     ,
        ls_num_montos               SMALLINT

    DEFINE
        li_num_regs                 INTEGER

    -- -----------------------------------------------------------------------------

    DISPLAY "GENERANDO ARCHIVO DE DETALLE ...." AT 8,2

    -- Determinamos la ubicacion de los reportes
    LET lc_ruta_det_nss = gr_seg_modulo.ruta_envio CLIPPED, "/", "NSS-", gr_datos_retiro.nom_archivo CLIPPED
    LET lc_ruta_det_nss = lc_ruta_det_nss CLIPPED

    LET lc_ruta_det_sie = gr_seg_modulo.ruta_envio CLIPPED, "/", "SIE-", gr_datos_retiro.nom_archivo CLIPPED
    LET lc_ruta_det_sie = lc_ruta_det_sie CLIPPED

    LET lc_ruta_det_viv = gr_seg_modulo.ruta_envio CLIPPED, "/", "VIV-", gr_datos_retiro.nom_archivo CLIPPED
    LET lc_ruta_det_viv = lc_ruta_det_viv CLIPPED

    LET gc_ruta_archivo = gr_seg_modulo.ruta_envio CLIPPED, "/", gr_datos_retiro.nom_archivo CLIPPED
    LET gc_ruta_archivo = gc_ruta_archivo CLIPPED

    LET ls_flag         = 0
    LET li_num_regs     = 0

    -- Ciclo por cada NSS confirmado
    DECLARE cur_op05 CURSOR FOR
        SELECT *
        FROM    ret_solicitud_tx
        WHERE   tipo_retiro         = gc_tipo_ret
        AND     estado_solicitud    = gr_estado.confirmado
        AND    (rechazo_cod = 0 OR rechazo_cod IS NULL)
        ORDER BY nss

    FOREACH cur_op05 INTO lr_solicitud.*

        INITIALIZE lr_vivienda.* TO NULL
        INITIALIZE lr_mto_sie.*  TO NULL

        -- Iniciamos los reportes
        START REPORT rpt_detalle_sol_03 TO lc_ruta_det_nss
        START REPORT rpt_detalle_viv_05 TO lc_ruta_det_viv

        LET ls_flag             = 1
        LET li_num_regs         = li_num_regs + 1
        LET ls_num_montos       = 0
        #CPL-2430
        INITIALIZE lr_info_biometrica.* TO NULL
        LET lr_info_biometrica.identificador = 1 --IMSS
        LET lr_info_biometrica.tiposervicio  = 2 --TOTAL

        #Se obtiene informacion biometrica
        SELECT LPAD(idsolicitante,2,0) idsolicitante,
               curpsolicitante                      ,
               LPAD(NVL(sellotrabajador,0),14,0)    ,
               curpagenteservicio
        INTO   lr_info_biometrica.idsolicitante     ,
               lr_info_biometrica.curpsolicitante   ,
               lr_info_biometrica.sellotrabajador   ,
               lr_info_biometrica.curpagenteservicio
        FROM   ret_sellosbiometricos_suv
        WHERE  identificador = lr_info_biometrica.identificador
        AND    tiposervicio  = lr_info_biometrica.tiposervicio
        AND    nss           = lr_solicitud.nss
        AND    consecutivo   = lr_solicitud.consecutivo

        IF lr_info_biometrica.idsolicitante IS NULL THEN
           LET lr_info_biometrica.idsolicitante   = '00'
           LET lr_info_biometrica.sellotrabajador = '00000000000000'
        END IF

        SELECT *
        INTO   lr_vivienda.*
        FROM   safre_tmp:tmp_monto_viv
        WHERE  folio            = 1
        AND    nss              = lr_solicitud.nss
        AND    consecutivo      = lr_solicitud.consecutivo

        OUTPUT TO REPORT rpt_detalle_sol_03(lr_solicitud.*, lr_info_biometrica.*)
        OUTPUT TO REPORT rpt_detalle_viv_05(lr_solicitud.nss, lr_solicitud.curp, lr_vivienda.*)

        SELECT COUNT(*)
        INTO   ls_num_montos
        FROM   safre_tmp:tmp_monto_siefore
        WHERE  folio            = 1
        AND    nss              = lr_solicitud.nss
        AND    consecutivo      = lr_solicitud.consecutivo
        AND    tipo_operacion   = 5

        -- Si existen registros de saldo en siefores para el nss actual,
        -- los barremos para generar el reporte del detalle de siefores

        IF ls_num_montos > 0 THEN
            START REPORT rpt_detalle_sie_04 TO lc_ruta_det_sie

            DECLARE cur_sie CURSOR FOR
                SELECT siefore          ,
                       acciones_ret97   ,
                       acciones_cv      ,
                       acciones_cs      ,
                       acciones_ret92
                FROM   safre_tmp:tmp_monto_siefore
                WHERE  folio            = 1
                AND    nss              = lr_solicitud.nss
                AND    consecutivo      = lr_solicitud.consecutivo
                AND    tipo_operacion   = 5
                ORDER BY 1

            FOREACH cur_sie INTO lr_mto_sie.*
                OUTPUT TO REPORT rpt_detalle_sie_04(lr_solicitud.nss, lr_solicitud.curp, lr_mto_sie.*)
            END FOREACH

            FINISH REPORT rpt_detalle_sie_04
        END IF

        FINISH REPORT rpt_detalle_sol_03
        FINISH REPORT rpt_detalle_viv_05

        #-- Una vez generados los archivos para el nss actual, los concatenamos en uno solo
        CALL f_concatena_reportes(lc_ruta_det_nss    ,
                                  lc_ruta_det_sie    ,
                                  lc_ruta_det_viv    ,
                                  ls_num_montos      ,
                                  li_num_regs
                                 )

    END FOREACH

    LET lc_comando = "chmod 777 ", gc_ruta_archivo CLIPPED
    RUN lc_comando

    IF ls_flag = 1 THEN
        INSERT INTO safre_tmp:tmp_ctr_envio_lote
        VALUES (HOY                     , -- fecha_genera
                gc_tipo_ret             , -- tipo_retiro
                1                       , -- folio
                " "                     , -- fecha_envio
                " "                     , -- fecha_reverso
                CURRENT HOUR TO MINUTE  , -- hora_genera
                " "                     , -- hora_envio
                gc_usuario              , -- gc_usuario_genera
                " "                     , -- gc_usuario_envio
                " "                     , -- gc_usuario_reverso
                gr_estado.procesado     , -- estado
                li_num_regs               -- total_registros
                )
    END IF

    DISPLAY "(TERMINADO)" AT 8,36

END FUNCTION

#---------------------------------------------------------------------------#
# tercer_paso : Realiza el paso de informacion de las tablas temporales a   #
#               las fisicas                                                 #
#---------------------------------------------------------------------------#
FUNCTION tercer_paso()

    DEFINE
        lr_provision        ,
        lr_tmp_prov         RECORD LIKE dis_provision.*

    DEFINE lr_datos RECORD
        nss                 LIKE dis_provision.nss              ,
        consecutivo         LIKE dis_provision.consecutivo_lote
    END RECORD

    DEFINE
        li_num_prov             INTEGER

    DEFINE ld_fecha_conversion DATE

    ------------------------------------------------------------------

    DISPLAY "PROVISIONANDO CUENTAS ...." AT 9,2

    LET li_num_prov     = 0

    DISPLAY "FOLIO    : ", gi_ult_folio AT 12,21
    DISPLAY "NUM. REGISTROS    : ", li_num_prov AT 13,12

    -- Copiamos la provision de la tabla temporal a la definitiva
    UPDATE safre_tmp:tmp_provision_imss
    SET    folio = gi_ult_folio
    WHERE  folio = 1

    INSERT INTO dis_provision
    SELECT *
    FROM   safre_tmp:tmp_provision_imss
    WHERE  folio = gi_ult_folio

    -- Copiamos la tabla de montos temporal a la definitiva
    UPDATE safre_tmp:tmp_monto_siefore
    SET    folio = gi_ult_folio
    WHERE  folio = 1

    INSERT INTO ret_monto_siefore
    SELECT *
    FROM   safre_tmp:tmp_monto_siefore
    WHERE  folio = gi_ult_folio

    -- Copiamos la tabla de montos de vivienda temporal a la definitiva
    UPDATE safre_tmp:tmp_monto_viv
    SET    folio = gi_ult_folio
    WHERE  folio = 1

    INSERT INTO ret_monto_viv
    SELECT *
    FROM   safre_tmp:tmp_monto_viv
    WHERE  folio = gi_ult_folio

    -- Copiamos la tabla de control de envio temporal a la definitiva
    UPDATE safre_tmp:tmp_ctr_envio_lote
    SET    folio = gi_ult_folio
    WHERE  folio = 1

    INSERT INTO ret_ctr_envio_lote
    SELECT *
    FROM   safre_tmp:tmp_ctr_envio_lote
    WHERE  folio = gi_ult_folio

    -- Actualiza el folio y el estado de la solicitud de los registros provisionados
    DECLARE cur_soli CURSOR FOR
    SELECT UNIQUE(nss)      ,
           consecutivo_lote
    FROM   dis_provision
    WHERE  folio = gi_ult_folio
    ORDER BY 1

    #CPL-1530
    #Actualizar la fecha_envio con la fecha de la provision
    SELECT MAX(fecha_conversion)
    INTO   ld_fecha_conversion
    FROM   dis_provision
    WHERE  folio = gi_ult_folio

    FOREACH cur_soli INTO lr_datos.*

        UPDATE ret_solicitud_tx
        SET    folio            = gi_ult_folio          ,
               estado_solicitud = gr_estado.procesado   ,
               fecha_envio      = ld_fecha_conversion
        WHERE  nss              = lr_datos.nss
        AND    consecutivo      = lr_datos.consecutivo
        AND    estado_solicitud = gr_estado.confirmado
        
        UPDATE ret_ctr_benef 
        SET folio_liquida     = gi_ult_folio
        WHERE nss             = lr_datos.nss
        AND consecutivo_solic = lr_datos.consecutivo
        AND tipo_retiro       = gc_tipo_ret

        LET li_num_prov = li_num_prov + 1
        DISPLAY "NUM. REGISTROS    : ", li_num_prov AT 13,12

    END FOREACH

    CALL fn_desmarca_cuenta()
    DISPLAY "TOTAL DE REGISTROS RECHAZADOS   : ",cont_reg_rech    AT 14,12

    DISPLAY "(TERMINADO)" AT 9,36

    CALL f_lib_error_msg("PROVISION FINALIZADA CORRECTAMENTE")

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_ejecucion : Verifica que existan registros para realizar la      #
#                      provision de recursos del retiro P                   #
#---------------------------------------------------------------------------#
FUNCTION f_valida_ejecucion()

    DEFINE lr_ejecuta RECORD
        num_registros           INTEGER     ,
        procesa                 SMALLINT
    END RECORD

    -- -----------------------------------------------------------------------------

    CALL f_obtiene_precios_accion(HOY)

    LET lr_ejecuta.num_registros    = 0
    LET lr_ejecuta.procesa          = 0

    SELECT NVL(COUNT(*), 0)
    INTO   lr_ejecuta.num_registros
    FROM   ret_solicitud_tx
    WHERE  tipo_retiro        = gc_tipo_ret
    AND    estado_solicitud   = gr_estado.confirmado
    AND    (rechazo_cod = 0 OR rechazo_cod IS NULL)

    DISPLAY "REGISTROS A PROVISIONAR" AT 6,9
    DISPLAY "RETIRO P  : ", lr_ejecuta.num_registros  AT 8,23

    IF (lr_ejecuta.num_registros > 0) THEN
        LET lr_ejecuta.procesa = 1
    ELSE
        LET lr_ejecuta.procesa  = 0
        CALL f_lib_error_msg("NO EXISTEN REGISTROS A PROVISIONAR")
    END IF

    IF lr_ejecuta.procesa = 1 THEN
        WHILE TRUE
            PROMPT "¿DESEA EJECUTAR LA PROVISION DEL RETIRO P? (S/N) : " FOR enter
            IF enter MATCHES "[sSnN]" THEN
                IF enter MATCHES "[sS]" THEN
                    LET lr_ejecuta.procesa = 1
                    DISPLAY "                                          " AT 6,1
                    DISPLAY "                                          " AT 8,1
                ELSE
                    LET lr_ejecuta.procesa = 0
                    CALL f_lib_error_msg("PROCESO CANCELADO")
                END IF
                EXIT WHILE
            END IF
        END WHILE
    END IF

    RETURN lr_ejecuta.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_obten_saldo_subcta : Obtiene el saldo del nss de la subcuenta indicada  #
#                        a la fecha dada por pr_monto.fecha_saldo           #
#---------------------------------------------------------------------------#
FUNCTION f_obten_saldo_subcta(pr_monto)

    DEFINE pr_monto RECORD
        nss                 LIKE ret_solicitud_tx.nss       ,
        subcuenta           SMALLINT                        ,
        fecha_saldo         DATE
    END RECORD

    DEFINE lr_saldos RECORD
        subcuenta               LIKE dis_cuenta.subcuenta           ,
        siefore                 LIKE dis_cuenta.siefore             ,
        monto_en_acciones       LIKE dis_cuenta.monto_en_acciones   ,
        monto_en_pesos          LIKE dis_cuenta.monto_en_pesos
    END RECORD

    DEFINE
        lc_prepare          CHAR(300)

    DEFINE
        ls_grupo              ,
        ls_cont               SMALLINT

    DEFINE
        ld_saldo_dia_viv      DECIMAL(16,6)

    -- -----------------------------------------------------------------------------

    LET ls_grupo                = 0
    LET lr_saldos.subcuenta     = 0
    LET lr_saldos.siefore       = 0

    ----- SALDOS -----
    LET lc_prepare =  " EXECUTE FUNCTION fn_saldo_dia ( ?,?,?,? )"
    PREPARE eje_saldo_dia FROM lc_prepare

    DECLARE c_saldo_subcta CURSOR FOR eje_saldo_dia

    OPEN c_saldo_subcta USING pr_monto.nss          ,
                              pr_monto.subcuenta    ,
                              ls_grupo              ,
                              pr_monto.fecha_saldo

    FETCH c_saldo_subcta INTO lr_saldos.*

    CLOSE c_saldo_subcta

    IF lr_saldos.monto_en_acciones IS NULL OR lr_saldos.monto_en_acciones <= 0 THEN
        LET lr_saldos.monto_en_acciones     = 0
        LET lr_saldos.monto_en_pesos        = 0
    END IF

    -- Verificamos si no existe un sobregiro en vivienda
    IF lr_saldos.siefore = 11 THEN

        LET ld_saldo_dia_viv    = 0

        SELECT SUM(monto_en_acciones)
        INTO   ld_saldo_dia_viv
        FROM   dis_cuenta
        WHERE  nss          = pr_monto.nss
        AND    siefore      = 11
        AND    subcuenta    = pr_monto.subcuenta

        IF ld_saldo_dia_viv < 0 OR ld_saldo_dia_viv IS NULL THEN
            LET ld_saldo_dia_viv    = 0
        END IF

        -- Si lo que se obtiene al primer dia natural es mayor a lo que hay
        -- actualmente en la cuenta individual, tomamos el saldo al dia
        IF lr_saldos.monto_en_acciones > ld_saldo_dia_viv THEN
            LET lr_saldos.monto_en_acciones = ld_saldo_dia_viv
        END IF
    END IF

    RETURN lr_saldos.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_provisiona_subcta : Realiza la provision del monto dado en la subcuenta #
#---------------------------------------------------------------------------#
FUNCTION f_provisiona_subcta(pr_provi)

    DEFINE pr_provi RECORD
        nss                 LIKE ret_solicitud_tx.nss           ,
        curp                LIKE ret_solicitud_tx.curp          ,
        subcta              SMALLINT                            ,
        consecutivo         LIKE ret_solicitud_tx.consecutivo   ,
        acciones            DECIMAL(16,6)                       ,
        pesos               DECIMAL(16,6)                       ,
        fecha_proc          DATE                                ,
        tipo_mov            SMALLINT                            ,
        siefore             SMALLINT
    END RECORD

    DEFINE lr_provision RECORD LIKE dis_provision.*

    DEFINE ld_precio_acc LIKE dis_provision.precio_accion

    DEFINE
        ls_sie              SMALLINT

    -- -----------------------------------------------------------------------------

    IF ( pr_provi.siefore = 11 ) THEN
        --LET pr_provi.acciones           = f_lib_redondea_val(pr_provi.acciones, 2)
        LET pr_provi.pesos              = f_lib_redondea_val(pr_provi.pesos, 2)
        LET lr_provision.fecha_valor    = MDY(MONTH(pr_provi.fecha_proc)    ,
                                              01                            ,
                                              YEAR(pr_provi.fecha_proc)
                                             )
    ELSE
        LET lr_provision.fecha_valor    = pr_provi.fecha_proc

    END IF

    IF pr_provi.siefore <> 0 THEN
        LET ls_sie          = pr_provi.siefore
        LET ld_precio_acc   = gar_precio_acc[ls_sie].precio_dia
    ELSE
        LET ld_precio_acc   = 0
    END IF

    LET lr_provision.tipo_movimiento    = pr_provi.tipo_mov
    LET lr_provision.subcuenta          = pr_provi.subcta
    LET lr_provision.siefore            = pr_provi.siefore
    LET lr_provision.folio              = 1
    LET lr_provision.consecutivo_lote   = pr_provi.consecutivo
    LET lr_provision.nss                = pr_provi.nss
    LET lr_provision.curp               = pr_provi.curp
    LET lr_provision.folio_sua          = " "
    LET lr_provision.fecha_pago         = HOY

    LET lr_provision.fecha_conversion   = HOY
    LET lr_provision.monto_en_pesos     = pr_provi.pesos * -1
    LET lr_provision.monto_en_acciones  = pr_provi.acciones * -1
    LET lr_provision.precio_accion      = ld_precio_acc
    LET lr_provision.dias_cotizados     = 0
    LET lr_provision.sucursal           = ""
    LET lr_provision.id_aportante       = "RETIRO"
    LET lr_provision.estado             = 6
    LET lr_provision.fecha_proceso      = HOY
    LET lr_provision.usuario            = gc_usuario
    LET lr_provision.fecha_archivo      = HOY
    LET lr_provision.etiqueta           = 1

    INSERT INTO safre_tmp:tmp_provision_imss
    VALUES (lr_provision.*)

END FUNCTION

#---------------------------------------------------------------------------#
# f_abre_ventana : Abre la ventana donde se captura los datos para generar  #
#                  la consulta de rechazos                                  #
#---------------------------------------------------------------------------#
FUNCTION f_abre_ventana()

    OPEN WINDOW RETC8281 AT 4,4 WITH FORM "RETC8281" ATTRIBUTE(BORDER)
    DISPLAY " RETC828     NOTIFICA Y PROVISIONA DISPOSICION DE RECURSOS                     " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY " < Ctrl-C > Salir                                           TIPO RETIRO P      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,64 ATTRIBUTE(REVERSE)

END FUNCTION

#---------------------------------------------------------------------------#
# f_obtiene_precios_accion : Valida si existen los precios de accion de la  #
#                            fecha indicada y los almacena para su posible  #
#                            uso en el programa                             #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_precios_accion(pdt_fec_precios)

    DEFINE
        pdt_fec_precios         DATE

    DEFINE lr_precio_acc RECORD
        estado              SMALLINT        ,
        fecha               DATE            ,
        siefore             SMALLINT        ,
        precio_dia          DECIMAL(16,6)
    END RECORD

    DEFINE
        lc_precios_acc          CHAR(100)   ,
        lc_mensaje              CHAR(100)   ,
        lc_siefore              CHAR(002)

    DEFINE
        ls_sie                  SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_sie = 1

    LET lc_precios_acc = " EXECUTE FUNCTION fn_verifica_precio_accion (?)"
    PREPARE eje_precios_accion FROM lc_precios_acc
    DECLARE c_precios CURSOR FOR eje_precios_accion

    FOREACH c_precios USING pdt_fec_precios
                      INTO lr_precio_acc.*

        IF lr_precio_acc.estado <> 0 THEN
            LET lc_siefore = lr_precio_acc.siefore

            LET lc_mensaje = " FALTAN PRECIOS DE ACCION: ", lr_precio_acc.fecha USING "DD/MM/YYYY",
                             " SIEFORE ", lc_siefore CLIPPED

            CALL f_lib_error_msg(lc_mensaje)
            EXIT PROGRAM
        ELSE
            LET ls_sie = lr_precio_acc.siefore
            LET gar_precio_acc[ls_sie].* = lr_precio_acc.*
        END IF

    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# f_concatena_reportes : Dadas las rutas de los tres archivos de detalle    #
#                        temporales, los concatena en uno solo archivo que  #
#                        contendra el archivo de detalle final              #
#---------------------------------------------------------------------------#
FUNCTION f_concatena_reportes(pr_rutas, ps_monto, ps_num_regs)

    DEFINE pr_rutas RECORD
        detalle_03              CHAR(100)   ,
        detalle_04              CHAR(100)   ,
        detalle_05              CHAR(100)
    END RECORD

    DEFINE
        ps_monto                ,
        ps_num_regs             SMALLINT

    DEFINE
        lc_ruta_tmp             CHAR(100)   ,
        lc_ruta_det             CHAR(100)   ,
        lc_concatena            CHAR(500)   ,
        lc_borra                CHAR(500)

    -- -----------------------------------------------------------------------------

    LET lc_ruta_tmp     = gr_seg_modulo.ruta_envio CLIPPED, "/", "rep_temporal.tmp"
    LET lc_ruta_tmp     = lc_ruta_tmp CLIPPED

    LET lc_ruta_det     = gr_seg_modulo.ruta_envio CLIPPED, "/", "detalle.tmp"
    LET lc_ruta_det     = lc_ruta_det CLIPPED

    -- Preparamos el comando para borrar los reportes
    LET lc_borra = "rm ", pr_rutas.detalle_03 CLIPPED, " ", pr_rutas.detalle_05 CLIPPED

    -- Si se genero el reporte de saldo de siefores, se incluye en los comandos
    IF ps_monto > 0 THEN
        LET lc_concatena    = "cat ", pr_rutas.detalle_03 CLIPPED, " ",
                                      pr_rutas.detalle_04 CLIPPED, " ",
                                      pr_rutas.detalle_05 CLIPPED, " > ", lc_ruta_tmp

        LET lc_concatena    = lc_concatena CLIPPED
        LET lc_borra        = lc_borra CLIPPED, " ", pr_rutas.detalle_04 CLIPPED
    ELSE
        LET lc_concatena    = "cat ", pr_rutas.detalle_03 CLIPPED, " ",
                                      pr_rutas.detalle_05 CLIPPED, " > ", lc_ruta_tmp CLIPPED
    END IF

    -- Concatenamos los archivos en uno solo y borramos los temporales
    RUN lc_concatena
    RUN lc_borra

    -- Acumulamos el archivo generado al reporte final
    IF ps_num_regs > 1 THEN
        LET lc_concatena = "cat ", gc_ruta_archivo, " ", lc_ruta_tmp, " > ", lc_ruta_det
        RUN lc_concatena

        LET lc_concatena = " mv ", lc_ruta_det, " ", gc_ruta_archivo
        RUN lc_concatena
    ELSE
        -- Si es el primer registro procesado entonces creamos el archivo de detalle
        LET lc_concatena = " cp ", lc_ruta_tmp, " ", gc_ruta_archivo
        RUN lc_concatena
    END IF

    -- Borramos el temporal generado para quedarnos solo con el archivo de detalle
    LET lc_borra = "rm ", lc_ruta_tmp
    RUN lc_borra

END FUNCTION

#---------------------------------------------------------------------------#
# f_tablas_tmp : Genera las tablas temporales donde se almacenan los        #
#                calculos y cambios                                         #
#---------------------------------------------------------------------------#
FUNCTION f_tablas_tmp()

    DATABASE safre_tmp

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_provision_imss
        DROP TABLE tmp_monto_siefore
        DROP TABLE tmp_monto_viv
        DROP TABLE tmp_ctr_envio_lote
    WHENEVER ERROR STOP

    --------------------------------

    CREATE TABLE tmp_provision_imss (
        tipo_movimiento    SMALLINT NOT NULL       ,
        subcuenta          SMALLINT NOT NULL       ,
        siefore            SMALLINT                ,
        folio              DECIMAL(10,0) NOT NULL  ,
        consecutivo_lote   INTEGER                 ,
        nss                CHAR(11) NOT NULL       ,
        curp               CHAR(18)                ,
        folio_sua          CHAR(6)                 ,
        fecha_pago         DATE                    ,
        fecha_valor        DATE                    ,
        fecha_conversion   DATE                    ,
        monto_en_pesos     DECIMAL(16,6)           ,
        monto_en_acciones  DECIMAL(16,6)           ,
        precio_accion      DECIMAL(16,6)           ,
        dias_cotizados     INTEGER                 ,
        sucursal           CHAR(10)                ,
        id_aportante       CHAR(11)                ,
        estado             SMALLINT                ,
        fecha_proceso      DATE                    ,
        usuario            CHAR(8)                 ,
        fecha_archivo      DATE                    ,
        etiqueta           INTEGER
    )

    CREATE INDEX tmp_provision_imss01
    ON tmp_provision_imss(folio             ,
                          subcuenta         ,
                          tipo_movimiento   ,
                          estado
                         )

    CREATE INDEX tmp_provision_imss02
    ON tmp_provision_imss(nss)

    CREATE INDEX tmp_provision_imss03
    ON tmp_provision_imss(folio, subcuenta)

    GRANT ALL ON tmp_provision_imss TO PUBLIC

    --------------------------------

    CREATE TABLE tmp_monto_siefore (
        nss                 CHAR(11)        ,
        consecutivo         DECIMAL(11,0)   ,
        folio               INTEGER         ,
        tipo_retiro         CHAR(1)         ,
        tipo_operacion      SMALLINT        ,
        siefore             SMALLINT        ,
        acciones_ret97      DECIMAL(16,6)   ,
        acciones_cv         DECIMAL(16,6)   ,
        acciones_cs         DECIMAL(16,6)   ,
        acciones_ret92      DECIMAL(16,6)
    )

    CREATE INDEX tmp_mto_imss_01
    ON tmp_monto_siefore(nss            ,
                         consecutivo    ,
                         tipo_operacion
                        )

    GRANT ALL ON tmp_monto_siefore TO PUBLIC

    --------------------------------

    CREATE TABLE tmp_monto_viv (
        nss                 CHAR(11)        ,
        consecutivo         DECIMAL(11,0)   ,
        folio               INTEGER         ,
        tipo_retiro         CHAR(1)         ,
        fecha_valor_viv     DATE            ,
        acciones_viv97      DECIMAL(14,6)   ,
        acciones_viv92      DECIMAL(14,6)   ,
        pesos_viv72         DECIMAL(14,2)   ,
        estado_sub_viv      CHAR(1)         ,
        acc_viv97_bdsviv    DECIMAL(14,6)   ,
        acc_viv92_bdsviv    DECIMAL(14,6)
    )

    CREATE INDEX tmp_viv_imss_01
    ON tmp_monto_viv(nss, consecutivo)

    GRANT ALL ON tmp_monto_viv TO PUBLIC

    --------------------------------

    CREATE TABLE tmp_ctr_envio_lote (
        fecha_genera        DATE                    ,
        tipo_retiro         CHAR(1)                 ,
        folio               INTEGER                 ,
        fecha_envio         DATE                    ,
        fecha_reverso       DATE                    ,
        hora_genera         DATETIME HOUR TO MINUTE ,
        hora_envio          DATETIME HOUR TO MINUTE ,
        usuario_genera      CHAR(12)                ,
        usuario_envio       CHAR(12)                ,
        usuario_reverso     CHAR(12)                ,
        estado              SMALLINT                ,
        total_registros     INTEGER
    )

    CREATE INDEX tmp_ctr_envio_lote_01
    ON tmp_ctr_envio_lote(folio,fecha_genera,tipo_retiro)

    GRANT ALL ON tmp_ctr_envio_lote TO PUBLIC

    --------------------------------

    DATABASE safre_af

END FUNCTION


#---------------------------------------------------------------------------#
# rpt_detalle_sol_03 : Genera el archivo plano del detalle de solicitudes   #
#                      03 para la operacion 05 de disposiciones IMSS        #
#---------------------------------------------------------------------------#
REPORT rpt_detalle_sol_03(pr_solicitud, pr_info_biometrica)

    DEFINE pr_solicitud RECORD LIKE ret_solicitud_tx.*

    DEFINE lr_nombre RECORD
        paterno             LIKE afi_mae_afiliado.paterno,
        materno             LIKE afi_mae_afiliado.materno,
        nombres             LIKE afi_mae_afiliado.nombres
    END RECORD

    DEFINE pr_info_biometrica RECORD
        idsolicitante        CHAR(02),
        curpsolicitante      CHAR(18),
        sellotrabajador      CHAR(14),
        curpagenteservicio   CHAR(18),
        identificador        INTEGER ,
        tiposervicio         INTEGER
    END RECORD

    DEFINE
        lc_fecha_resol_08           CHAR(008)   ,
        lc_fecha_resol_10           CHAR(010)   ,
        lc_porcentaje_06            CHAR(006)   ,
        lc_porcentaje_05            CHAR(005)
    DEFINE #loc #char
        vmax_folio            INTEGER
    DEFINE #loc #char
        c_sec_pension         CHAR(02)

    -- -----------------------------------------------------------------------------

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
        SELECT MAX(A.folio)
            INTO   vmax_folio
            FROM   ret_det_datamart A
            WHERE  A.nss              = pr_solicitud.nss
            AND    A.fecha_resolucion = pr_solicitud.fecha_resolucion
            AND    A.tipo_prestacion  = 1
            AND    A.diag_datamart   IN (101,106,300,301,218)

            SELECT MAX(A.sec_pension)
            INTO   c_sec_pension
            FROM   ret_det_datamart A
            WHERE  A.nss              = pr_solicitud.nss
            AND    A.fecha_resolucion = pr_solicitud.fecha_resolucion
            AND    A.tipo_prestacion  = 1
            AND    A.diag_datamart   IN (101,106,300,301,218)
            AND    A.folio            = vmax_folio

            IF SQLCA.SQLCODE = NOTFOUND THEN
                LET c_sec_pension = 2 SPACES
            END IF

            LET lc_fecha_resol_10   = pr_solicitud.fecha_resolucion
            LET lc_fecha_resol_08   = lc_fecha_resol_10[07,10],
                                      lc_fecha_resol_10[01,02],
                                      lc_fecha_resol_10[04,05]

            IF (lc_fecha_resol_08 IS NULL) OR (lc_fecha_resol_08 = " ") THEN
                LET lc_fecha_resol_08   = "00010101"
            END IF

            SELECT paterno   ,
                   materno   ,
                   nombres
            INTO   lr_nombre.*
            FROM   afi_mae_afiliado
            WHERE  n_seguro = pr_solicitud.nss

            IF pr_solicitud.tipo_pension = "IP" THEN
                IF pr_solicitud.porcentaje_val IS NULL OR pr_solicitud.porcentaje_val = 0 THEN
                    LET pr_solicitud.porcentaje_val = 0
                END IF
            ELSE
                LET pr_solicitud.porcentaje_val = 0
            END IF

            LET lc_porcentaje_06    = pr_solicitud.porcentaje_val USING "&&&.&&"
            LET lc_porcentaje_05    = lc_porcentaje_06[01,03],
                                      lc_porcentaje_06[05,06]

            IF pr_solicitud.fecha_solicitud IS NULL OR pr_solicitud.fecha_solicitud = " " THEN
                LET  pr_solicitud.fecha_solicitud = "01010001"
            END IF

        PRINT
            COLUMN 001, "03"                                            ,-- tipo_registro
            COLUMN 003, "04"                                            ,-- ident_servicio
            COLUMN 005, "05"                                            ,-- ident_operacion
            COLUMN 007, pr_solicitud.nss                                ,
            COLUMN 018, pr_solicitud.curp                               ,
            COLUMN 036, lr_nombre.nombres                               ,
            COLUMN 076, lr_nombre.paterno                               ,
            COLUMN 116, lr_nombre.materno                               ,
            COLUMN 156, c_sec_pension                                   ,--secuencia pension
            COLUMN 158, pr_solicitud.tipo_retiro                        ,
            COLUMN 159, pr_solicitud.regimen                            ,
            COLUMN 161, pr_solicitud.tipo_seguro                        ,
            COLUMN 163, pr_solicitud.tipo_pension                       ,
            COLUMN 165, pr_solicitud.tipo_prestacion USING "&&"         ,
            COLUMN 167, pr_solicitud.fecha_ini_pen USING "YYYYMMDD"     , -- fecha_ini_pen
            COLUMN 175, lc_fecha_resol_08                               , -- fecha_resolucion
            COLUMN 183, lc_porcentaje_05                                , -- porcentaje_val
            COLUMN 188, "0000"                                          , -- semanas_cotizadas
            COLUMN 192, pr_solicitud.fecha_solicitud USING "YYYYMMDD"   ,
            COLUMN 201, "00010101"                                      , -- fecha_nacimiento
            COLUMN 227, "000000"                                        , -- fecha periodo pago
            COLUMN 233, pr_solicitud.consecutivo USING "&&&&&&&&&&&"    ,
            COLUMN 244, pr_info_biometrica.idsolicitante       , --ID Solicitante
            COLUMN 246, pr_info_biometrica.curpsolicitante     , --CURP Solicitante
            COLUMN 264, pr_info_biometrica.sellotrabajador     , --Sello único de verificación
            COLUMN 278, pr_info_biometrica.curpagenteservicio  , --CURP Agente de Servicio
            COLUMN 296, 485 SPACES                               --Campos 31 a 48 Filler LAYOUT(780 posiciones)
END REPORT

#---------------------------------------------------------------------------#
# rpt_detalle_sie_04 : Genera el archivo plano del detalle por siefore 04   #
#                      para la operacion 05 de disposiciones IMSS           #
#---------------------------------------------------------------------------#
REPORT rpt_detalle_sie_04(pc_nss, pc_curp, pr_siefore)

    DEFINE pc_nss       LIKE ret_solicitud_tx.nss
    DEFINE pc_curp      LIKE ret_solicitud_tx.curp

    DEFINE pr_siefore RECORD
        siefore                     SMALLINT        ,
        acciones_ret97              DECIMAL(16,6)   ,
        acciones_cv                 DECIMAL(16,6)   ,
        acciones_cs                 DECIMAL(16,6)   ,
        acciones_ret92              DECIMAL(16,6)
    END RECORD

    DEFINE
        lc_acc_ret97_15             CHAR(15) ,
        lc_acc_ret97_14             CHAR(14) ,
        lc_acc_cv_15                CHAR(15) ,
        lc_acc_cv_14                CHAR(14) ,
        lc_acc_cs_15                CHAR(15) ,
        lc_acc_cs_14                CHAR(14) ,
        lc_acc_ret92_15             CHAR(15) ,
        lc_acc_ret92_14             CHAR(14)

    -- -----------------------------------------------------------------------------

    OUTPUT
        PAGE LENGTH   1
    	LEFT MARGIN   0
	    RIGHT MARGIN  0
	    TOP MARGIN    0
	    BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
            -- Obtenemos el valor de las Acciones de Retiro 97
            IF pr_siefore.acciones_ret97 IS NULL THEN
                LET pr_siefore.acciones_ret97 = 0
            END IF

            LET lc_acc_ret97_15 = pr_siefore.acciones_ret97 USING "&&&&&&&&.&&&&&&"
            LET lc_acc_ret97_14 = lc_acc_ret97_15[01,08],
                                  lc_acc_ret97_15[10,15]

            -- Obtenemos el valor de las Acciones de CV
            IF pr_siefore.acciones_cv IS NULL THEN
                LET pr_siefore.acciones_cv = 0
            END IF

            LET lc_acc_cv_15    = pr_siefore.acciones_cv USING "&&&&&&&&.&&&&&&"
            LET lc_acc_cv_14    = lc_acc_cv_15[01,08],
                                  lc_acc_cv_15[10,15]

            -- Obtenemos el valor de las Acciones de CS
            IF pr_siefore.acciones_cs IS NULL THEN
                LET pr_siefore.acciones_cs = 0
            END IF

            LET lc_acc_cs_15    = pr_siefore.acciones_cs USING "&&&&&&&&.&&&&&&"
            LET lc_acc_cs_14    = lc_acc_cs_15[01,08],
                                  lc_acc_cs_15[10,15]

            -- Obtenemos el valor de las Acciones de Retiro 92
            IF pr_siefore.acciones_ret92 IS NULL THEN
                LET pr_siefore.acciones_ret92 = 0
            END IF

            LET lc_acc_ret92_15 = pr_siefore.acciones_ret92 USING "&&&&&&&&.&&&&&&"
            LET lc_acc_ret92_14 = lc_acc_ret92_15[01,08],
                                  lc_acc_ret92_15[10,15]

        PRINT
            COLUMN 001, "04"                                    ,-- Tipo de registro
            COLUMN 003, pc_nss                                  ,
            COLUMN 014, pc_curp                                 ,
            COLUMN 032, pr_siefore.siefore USING "&&"           ,-- Clave de siefore
            COLUMN 034, lc_acc_ret97_14                         ,-- Acciones de retiro 97
            COLUMN 048, lc_acc_cv_14                            ,-- Acciones de CV
            COLUMN 062, lc_acc_cs_14                            ,-- Acciones de CS
            COLUMN 076, lc_acc_ret92_14                         ,-- Acciones de retiro 92
            COLUMN 090, 691 SPACES                              -- Se modifica para hacer la longitud de acuerdo al cambio de layout
                                                                -- CPL-1054

END REPORT

#---------------------------------------------------------------------------#
# rpt_detalle_viv_05 : Genera el archivo plano del detalle de vivienda 05   #
#                      para la operacion 05 de disposiciones IMSS           #
#---------------------------------------------------------------------------#
REPORT rpt_detalle_viv_05(pr_solicitud, pr_vivienda)

    DEFINE pr_solicitud RECORD
        nss             LIKE ret_solicitud_tx.nss   ,
        curp            LIKE ret_solicitud_tx.curp
    END RECORD

    DEFINE pr_vivienda  RECORD LIKE ret_monto_viv.*

    DEFINE
        lc_fecha_val_08         CHAR(08) ,
        lc_fecha_val_10         CHAR(10) ,
        lc_impt_viv97_15        CHAR(15) ,
        lc_impt_viv97_14        CHAR(14) ,
        lc_impt_viv92_15        CHAR(15) ,
        lc_impt_viv92_14        CHAR(14)

    -- -----------------------------------------------------------------------------

    OUTPUT
        PAGE LENGTH   1
    	LEFT MARGIN   0
	    RIGHT MARGIN  0
	    TOP MARGIN    0
	    BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW
            -- Obtenemos la fecha Valor de Vivienda
            IF pr_vivienda.fecha_valor_viv IS NULL THEN
                LET pr_vivienda.fecha_valor_viv = 0
            END IF

            LET lc_fecha_val_10     = pr_vivienda.fecha_valor_viv
            LET lc_fecha_val_08     = lc_fecha_val_10[07,10] ,
                                      lc_fecha_val_10[01,02] ,
                                      lc_fecha_val_10[04,05]

            #-- Obtenemos aplicacion de intereses viv. 97
            IF pr_vivienda.acciones_viv97 IS NULL THEN
                LET pr_vivienda.acciones_viv97  = 0
            END IF

            LET lc_impt_viv97_15    = pr_vivienda.acciones_viv97 USING "&&&&&&&&.&&&&&&"
            LET lc_impt_viv97_14    = lc_impt_viv97_15[01,08],
                                      lc_impt_viv97_15[10,15]

            #-- Obtenemos aplicacion de intereses viv. 92
            IF pr_vivienda.acciones_viv92 IS NULL THEN
               LET pr_vivienda.acciones_viv92 = 0
            END IF

            LET lc_impt_viv92_15    = pr_vivienda.acciones_viv92 USING "&&&&&&&&.&&&&&&"
            LET lc_impt_viv92_14    = lc_impt_viv92_15[01,08],
                                      lc_impt_viv92_15[10,15]

        PRINT
            COLUMN 001, "05"                                , -- Tipo de registro
            COLUMN 003, pr_solicitud.nss                    ,
            COLUMN 014, pr_solicitud.curp                   ,
            COLUMN 032, lc_fecha_val_08                     , -- Fecha valor vivienda
            COLUMN 040, lc_impt_viv97_14                    , -- Intereses Viv 97
            COLUMN 054, lc_impt_viv92_14                    , -- Intereses Viv 92
            COLUMN 068, "00000000000000"                    , -- Fondo Viv 72
            COLUMN 083, "00000000000000"                    , -- Intereses Viv 97 en BDSVIV
            COLUMN 097, "00000000000000"                    , -- Intereses Viv 92 en BDSVIV
            COLUMN 111, 670 SPACES                              -- Se modifica para hacer la longitud de acuerdo al cambio de layout
                                                                -- CPL-1054

END REPORT

