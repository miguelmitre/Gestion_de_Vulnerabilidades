################################################################################
#Proyecto          => SISTEMA DE AFORE( SAFRE )                                #
#Owner             => E.F.P.                                                   #
#Programa RETC805  => GENERA NOTIFICACION DE DISPOSICION DE RECUSROS SOLICITUD #
#                     OP.05-D POR RETIROS TOTAL POR NEGATIVA DE PENSION        #
#Fecha creacion    => 24 DE ENERO 2004                                         #
#By                => FRANCO ESTEBAN ULLOA VIDELA                              #
#Fecha Modif       => 04 DE JULIO DEL 2004                                     #
#Modific.          => FRANCO ESTEBAN ULLOA VIDELA                              #
#Fecha Modif       => 21 DE NOVIEMBRE DEL 2004                                 #
#Modific.          => JAVIER GONZALEZ JERONIMO                                 #
#Fecha Modif       => 19 DE DICIEMBRE DE 2007                                  #
#Modific.          => XAVIER TORRES RIOS                                       #
#Fecha Modif       => 25 DE MARZO     DE 2008                                  #
#Sistema           => RET                                                      #
################################################################################
#Modificacion      => Jonathan Joddy Zavala Zavala   CPL-2106  1-Oct-2015      #
#                  => Se actualiza programa agregando caracteristicas derivadas#
#                  => de la creacion de la siefore basica 0/90                 #
################################################################################
DATABASE safre_af

GLOBALS
    DEFINE #glo #param_ret
        g_param_ret           RECORD LIKE seg_modulo.*

    DEFINE reg_800 RECORD #glo #reg_800
        confirmado            LIKE ret_estado.estado_solicitud ,
        procesado             LIKE ret_estado.estado_solicitud ,
        cancelado             LIKE ret_estado.estado_solicitud ,
        transicion            LIKE ret_estado.estado_solicitud ,
        liquidado             LIKE ret_estado.estado_solicitud ,
        enviado               LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE reg_12 RECORD #glo #reg_12
        estado                SMALLINT ,
        fecha_rcv             DATE     ,
        fecha_viv             DATE
    END RECORD

    DEFINE #glo #date
        d_primero_mes         ,
        f_fecha_valor_viv     ,
        HOY                   DATE

    DEFINE #glo #char
        v_valida_precio       CHAR(200) ,
        c10_fecha             CHAR(010) ,
        enter                 CHAR(001) ,
        G_LISTA_DET           CHAR(100) ,
        HORA_GEN              CHAR(005) ,
        HORA                  CHAR(008) ,
        id_aporte             CHAR(006) ,
        usuario               CHAR(008) ,
        comando               CHAR(110) ,
        v_marca               CHAR(100) ,
        v_desmarca            CHAR(100) ,
        v_obten_fecha         CHAR(100) ,
        v_saldo_dia           CHAR(300) ,
        v_provisiona          CHAR(300)

    DEFINE #glo #smallint
        gs_num_siefores       , #-- Indica el numero de siefores que se usan actualmente
        f_subcuenta           ,
        f_siefore             ,
        v_marca_res           ,
        v_cod_rechazo         ,
        sw_1                    ,
        s_tipo_movimiento     SMALLINT

    DEFINE #glo #integer
        cont_1                ,
        cont_marcados         ,
        cont_reg              ,
        ultimo_folio          INTEGER

    DEFINE cont_reg_rech      INTEGER

    #DEFINE gar_precio_acc ARRAY [20] OF RECORD   #CPL-2106
    DEFINE gar_precio_acc ARRAY [99] OF RECORD
        estado          SMALLINT        ,
        fecha           DATE            ,
        siefore         SMALLINT        ,
        precio_dia      DECIMAL(16,6)
    END RECORD

END GLOBALS

DEFINE ms_99_siefores  SMALLINT  #CPL-2106

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I

    CALL f_lib_abre_log("RETC805")

    CALL init() #i
    OPEN WINDOW retc805 AT 4,4 WITH FORM "RETC8051" ATTRIBUTE(BORDER)
    DISPLAY " RETC805    GENERA NOTIFICACION POR DISPOSICION DE RECURSOS                    " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                            < Ctrl-C > Salir                  N",
            "EGATIVA         " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

        WHILE TRUE
            PROMPT " ESTA SEGURO S/N ? " FOR CHAR enter
            IF enter MATCHES "[sSnN]" THEN
                IF enter MATCHES "[sS]" THEN #ff

                    CALL f_obtiene_precios_accion(HOY)

                    EXIT WHILE
                ELSE
                    PROMPT" PROCESO CANCELADO...<ENTER> PARA CONTINUAR "
                    FOR CHAR enter
                    EXIT PROGRAM
                END IF
            END IF
        END WHILE

        
        CALL fn_identifica_benef_designados()
        CALL primer_paso()   --pp CALCULA SALDOS
        CALL primer_paso_benef() #-- Calcula saldos benef
        CALL segundo_paso()  --sp GENERA PLANO

        DISPLAY "TOTAL DE REGISTROS A ENVIAR     : ",cont_reg AT 09,19
        DISPLAY "TOTAL DE REGISTROS RECHAZADOS   : ",cont_reg_rech AT 10,19

        DISPLAY " FOLIO NUMERO : ",ultimo_folio  AT 18,1
        PROMPT " PROCESO FINALIZADO... < ENTER > PARA SALIR " FOR CHAR enter

        CLOSE WINDOW retc805
END MAIN


FUNCTION init()
#i-------------
   
   DEFINE ls_count1           SMALLINT
   DEFINE ls_count2           SMALLINT

    -- -----------------------------------------------------------------------------
    LET ls_count1 = 0
    LET ls_count2 = 0
    
    LET HOY           = TODAY

    LET c10_fecha     = HOY
    LET c10_fecha     = c10_fecha[01,02],"/01/",c10_fecha[07,10]
    LET d_primero_mes = c10_fecha

    LET HORA          = TIME
    LET id_aporte     = "RETIRO"

    LET ms_99_siefores = 99   #CPL-2106

    #-- Obtenemos el numero de siefores actual en el sistema
    SELECT COUNT(*)
    INTO   gs_num_siefores
    FROM   tab_siefore_local
    WHERE  codigo_siefore > 0
    AND    codigo_siefore <> 11

    SELECT USER
    INTO   usuario
    FROM   tab_afore_local

    SELECT *
    INTO   g_param_ret.*
    FROM   seg_modulo
    WHERE  modulo_cod = 'ret'

    SELECT A.estado_solicitud
    INTO   reg_800.confirmado
    FROM   ret_estado A
    WHERE  A.descripcion = "CONFIRMADO"

    SELECT A.estado_solicitud
    INTO   reg_800.procesado
    FROM   ret_estado A
    WHERE  A.descripcion = "PROCESADO"

    SELECT A.estado_solicitud
    INTO   reg_800.cancelado
    FROM   ret_estado A
    WHERE  A.descripcion = "RECHAZADO"

    SELECT A.estado_solicitud
    INTO   reg_800.transicion
    FROM   ret_estado A
    WHERE  A.descripcion = "TRANSICION"

    SELECT A.estado_solicitud
    INTO   reg_800.liquidado
    FROM   ret_estado A
    WHERE  A.descripcion = "LIQUIDADO"

    SELECT A.estado_solicitud
    INTO   reg_800.enviado
    FROM   ret_estado A
    WHERE  A.descripcion = "ENVIADO"

    SELECT movimiento
    INTO   s_tipo_movimiento
    FROM   tab_retiro
    WHERE  tipo_retiro = 'D'
    
    

    SELECT COUNT(UNIQUE nss)
    INTO   ls_count1
    FROM   ret_solicitud_tx A
    WHERE  A.tipo_retiro      = 'D'
    AND    A.estado_solicitud IN (reg_800.confirmado)
    AND    A.nss          NOT IN (SELECT UNIQUE nss
                                    FROM ret_ctr_benef)

    SELECT COUNT(UNIQUE nss)
    INTO   ls_count2
    FROM   ret_solicitud_tx A
    WHERE  A.tipo_retiro      = 'D'
    AND    A.estado_solicitud IN (reg_800.confirmado,reg_800.transicion)
    AND    A.nss              IN (SELECT UNIQUE nss
                                    FROM ret_ctr_benef)

    IF (ls_count1 + ls_count2) > 0 THEN
       SELECT MAX(folio) + 1
       INTO   ultimo_folio
       FROM   glo_folio

       INSERT INTO glo_folio VALUES (ultimo_folio)
    ELSE
        PROMPT " NO HAY REGISTROS PARA PROCESAR...<ENTER> PARA SALIR "
        FOR CHAR ENTER
        EXIT PROGRAM
    END IF

    #ff
    --LET v_valida_precio = " EXECUTE FUNCTION fn_valida_existe_pecio() "
    --PREPARE eje_valida_precio FROM v_valida_precio

    -- MARCAJE ---
    LET v_marca = " EXECUTE PROCEDURE marca_cuenta (?,?,?,?,?,?,?,?)"

    LET v_cod_rechazo = 0
    
    ----- DESMARCA DE CUENTA -----
    LET v_desmarca = "EXECUTE PROCEDURE desmarca_cuenta( ?,?,?,?,?,? )"
    PREPARE eje_desmarca FROM v_desmarca

    INITIALIZE  v_saldo_dia, v_provisiona TO NULL

    ----- SALDO  -----
    LET v_saldo_dia = "EXECUTE FUNCTION fn_saldo_dia_asc (?,?,?,?) "
    PREPARE eje_saldo_dia_asc FROM v_saldo_dia

    ----- PROVISIONA  -----

    LET v_provisiona = "EXECUTE FUNCTION fn_prov_cargo (?,?,?,?,?,?,?,?,?,?,?) "
    PREPARE eje_provisiona FROM v_provisiona

    --- FECHA VALOR VIVENDA ---
    LET v_obten_fecha = "EXECUTE FUNCTION fn_obten_fecha_val ( ? )"
    PREPARE eje_obten_fecha FROM v_obten_fecha

    DECLARE cur_obten_fecha CURSOR FOR eje_obten_fecha
    OPEN cur_obten_fecha   USING HOY
        FETCH cur_obten_fecha INTO f_fecha_valor_viv
    CLOSE cur_obten_fecha
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
        estado              SMALLINT     ,
        fecha               DATE         ,
        siefore             SMALLINT     ,
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

            LET lc_mensaje = " FALTAN PRECIOS DE ACCION: FECHA ", lr_precio_acc.fecha USING "DD/MM/yyyy",
                             " -- SIEFORE ", lc_siefore CLIPPED

            PROMPT lc_mensaje FOR CHAR enter
            EXIT PROGRAM
        ELSE
            LET ls_sie = lr_precio_acc.siefore
            LET gar_precio_acc[ls_sie].* = lr_precio_acc.*
        END IF

    END FOREACH

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

    -- -----------------------------------------------------------------------------

    DISPLAY " IDENTIFICACIÓN DE BENEFICIARIOS " AT 19,1 ATTRIBUTE(REVERSE)

   LET arr_subcuentas[1] = 1
   LET arr_subcuentas[2] = 2
   LET arr_subcuentas[3] = 4
   LET arr_subcuentas[4] = 5
   LET arr_subcuentas[5] = 6
   LET arr_subcuentas[6] = 7
   LET arr_subcuentas[7] = 8
   LET arr_subcuentas[8] = 9

   LET ls_grupo = 0
   INITIALIZE lr_saldos.* TO NULL

   --BARRE LAS SOLICITUDES QUE COINCIDAN CON BENEFICIARIOS DESIGNADOS
   DECLARE cur_benef CURSOR FOR 
      SELECT a.nss,a.folio_solicitud,a.consecutivo,b.tipo_benef
      FROM ret_solicitud_tx a,
           ret_ctr_benef b
      WHERE a.nss = b.nss
      AND   a.consecutivo      = b.consecutivo_solic
      AND   a.tipo_retiro      = 'D'
      AND   a.rechazo_cod      = 0
      AND   a.estado_solicitud IN (reg_800.confirmado)
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
      AND tipo_retiro      = 'D'
      AND estado_solicitud IN (reg_800.confirmado)

      --Evalua si ya tiene consecutivo padre al solicitud
      SELECT UNIQUE consecutivo_padre
      INTO v_consec_padre_nul
      FROM ret_ctr_benef
      WHERE nss             = v_nss
      AND consecutivo_solic = v_consecutivo
      AND tipo_retiro       = 'D'

      IF v_consec_padre_nul IS NULL THEN  --Si no tiene consecutivo padre, le asigna el menor consecutivo de la solicitud
         UPDATE ret_ctr_benef
         SET consecutivo_padre = v_consecutivo_padre
         WHERE nss             = v_nss
         AND consecutivo_solic = v_consecutivo
         AND consecutivo_padre IS NULL
         AND tipo_retiro       = 'D'
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

                 DECLARE cur_saldo_dia CURSOR FOR eje_saldo_dia_asc
                 OPEN cur_saldo_dia USING v_nss,
                                          arr_subcuentas[i],
                                          ls_grupo,
                                          HOY
                 
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
         SET estado_solicitud = reg_800.transicion --ESTADO TRANSICIÓN
         WHERE nss           = v_nss
         AND consecutivo     = v_consecutivo
         AND folio_solicitud = v_folio_sol
         AND estado_solicitud IN (reg_800.confirmado)
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
        
        SELECT DISTINCT "OK"
           FROM dis_provision dp,
                tmp_dis_cuenta dis
          WHERE dp.nss              = p_nss
            AND dp.fecha_conversion = v_fecha_saldo
            AND dp.subcuenta        = v_subcuenta
            AND dp.nss              = dis.nss
            AND dp.consecutivo_lote = dis.consecutivo_lote
            AND dp.subcuenta        = dis.subcuenta
         UNION
         SELECT DISTINCT "OK"
           FROM dis_provision dp,
                ret_solicitud_tx rst
          WHERE dp.nss              = p_nss
            AND dp.fecha_conversion = v_fecha_saldo
            AND dp.subcuenta        = v_subcuenta
            AND dp.nss              = rst.nss
            AND dp.consecutivo_lote = rst.consecutivo
            AND (rst.estado_solicitud  = reg_800.enviado
                 AND (rst.diag_registro = '0' OR rst.diag_registro IS NULL OR rst.diag_registro = "   "))
            AND rst.tipo_retiro        = 'D'
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
               SET estado_solicitud = reg_800.cancelado,
                   cod_rechazo_ent  = 320,
                   folio            = ultimo_folio
             WHERE nss              = p_nss
               AND tipo_retiro      = 'D'
               AND estado_solicitud IN (reg_800.confirmado, reg_800.transicion)

            LET ls_indicador = 1
        END IF
    END IF

--    SELECT SUM(porcentaje)
--      INTO ls_porcentaje
--      FROM ret_ctr_benef
--     WHERE nss           = p_nss
--       AND tipo_benef    = p_tipo_benef
--       AND folio_liquida IS NULL
--       AND tipo_retiro   = 'D'
    SELECT SUM(porcentaje)
      INTO ls_porcentaje
      FROM ret_ctr_benef a,
           ret_solicitud_tx b
     WHERE a.nss = b.nss
       AND a.consecutivo_solic = b.consecutivo
       AND a.nss           = p_nss
       AND a.tipo_benef    = p_tipo_benef
       AND a.folio_liquida IS NULL
       AND a.tipo_retiro   = 'D'
       AND b.estado_solicitud    IN (reg_800.confirmado, reg_800.transicion)
   
    IF (ls_porcentaje > 100) THEN
        UPDATE ret_solicitud_tx
           SET estado_solicitud    = reg_800.cancelado,
               cod_rechazo_ent     = 321,
               folio               = ultimo_folio
         WHERE nss                 = p_nss
           AND tipo_retiro      = 'D'
           AND estado_solicitud    IN (reg_800.confirmado, reg_800.transicion)

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
    
    

    DISPLAY " DESMARCANDO SOLICITUDES CANCELADAS " AT 19,1 ATTRIBUTE(REVERSE)
    
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
        AND    A.marca_cod          = s_tipo_movimiento
        AND    B.folio              = ultimo_folio
        AND    B.tipo_retiro        = 'D'
        AND    A.fecha_fin IS NULL
        AND    B.estado_solicitud   = reg_800.cancelado
        ORDER BY 1
       
    LET cont_reg_rech = 0

    FOREACH cur_desmarca INTO lr_desmarca.*
    
        EXECUTE eje_desmarca USING lr_desmarca.*, usuario
        
        LET cont_reg_rech = cont_reg_rech + 1
        
    END FOREACH

END FUNCTION


FUNCTION primer_paso()
#pp-------------------
    DEFINE reg_5 RECORD #loc #reg_5
        nss                   LIKE ret_solicitud_tx.nss             ,
        consecutivo           LIKE ret_solicitud_tx.consecutivo     ,
        regimen               LIKE ret_solicitud_tx.regimen         ,
        tipo_seguro           LIKE ret_solicitud_tx.tipo_seguro     ,
        tipo_pension          LIKE ret_solicitud_tx.tipo_pension    ,
        tipo_prestacion       LIKE ret_solicitud_tx.tipo_prestacion ,
        grupo                 LIKE ret_solicitud_tx.grupo
    END RECORD
    DEFINE v_query            CHAR(1000);

    #DEFINE arr_siefore ARRAY [20] OF RECORD  #CPL-2106
    DEFINE arr_siefore ARRAY [99] OF RECORD
                                        activo            SMALLINT,
                                        acciones_ret97    DECIMAL(16,6) ,
                                        acciones_cv       DECIMAL(16,6) ,
                                        acciones_cs       DECIMAL(16,6) ,
                                        acciones_est      DECIMAL(16,6) ,
                                        acciones_ret92    DECIMAL(16,6) ,
                                        acciones_viv92    DECIMAL(16,6) ,
                                        acciones_esp      DECIMAL(16,6)
                                    END RECORD

    DEFINE #loc #date
        folio_sua             CHAR(06)

    DEFINE #loc #smallint
        ls_siefore            , #-- contador para los ciclos for
        s_grupo               ,
        s_subcta              ,
        si_provisiono         SMALLINT

     DEFINE #loc #decimal
        f_monto_pes          ,
        f_monto_acc          ,
        acciones_cv          ,
        s11_acc_viv97        ,
        s11_acc_viv92        DECIMAL(16,6)
        
    LET v_query = " SELECT  A.nss             , \n",
                  "         A.consecutivo     , \n",
                  "         A.regimen         , \n",
                  "         A.tipo_seguro     , \n",
                  "         A.tipo_pension    , \n",
                  "         A.tipo_prestacion , \n",
                  "         A.grupo             \n",
                  " FROM    ret_solicitud_tx A  \n",
                  "         LEFT OUTER JOIN  ret_ctr_benef B  \n",
                  "                      ON  B.nss = A.nss  \n",
                  "                     AND  B.consecutivo_solic = A.consecutivo  \n",    
                  " WHERE   A.estado_solicitud = ", reg_800.confirmado, " \n",
                  " AND     A.tipo_retiro      = 'D'  \n", --tipo de prestacion  = 3
                  " AND     A.rechazo_cod      = 0  \n",
                  " AND     B.consecutivo_solic IS NULL  \n",
                  " ORDER BY nss "
                  
    PREPARE pre_cur_1 FROM v_query
    DECLARE cur_1 CURSOR FOR pre_cur_1
    LET cont_1 = 0
    FOREACH cur_1 INTO reg_5.*
        LET cont_1                   = cont_1 + 1

        #-- Inicializamos variables del arreglo
        #FOR ls_siefore = 1 TO gs_num_siefores   #CPL-2106
        FOR ls_siefore = 1 TO ms_99_siefores
            LET arr_siefore[ls_siefore].activo             = FALSE
            LET arr_siefore[ls_siefore].acciones_ret97     = 0
            LET arr_siefore[ls_siefore].acciones_cv        = 0
            LET arr_siefore[ls_siefore].acciones_cs        = 0
            LET arr_siefore[ls_siefore].acciones_est       = 0
            LET arr_siefore[ls_siefore].acciones_ret92     = 0
            LET arr_siefore[ls_siefore].acciones_viv92     = 0
            LET arr_siefore[ls_siefore].acciones_esp       = 0
        END FOR

        LET s11_acc_viv97            = 0
        LET s11_acc_viv92            = 0

        DECLARE cur_2 CURSOR FOR
        SELECT A.subcuenta
        FROM   tab_agrupa_subcta A
        WHERE  A.grupo     = reg_5.grupo
        AND    A.subcuenta > 0

        FOREACH cur_2 INTO s_subcta
            LET f_subcuenta = 0
            LET f_siefore   = 0
            LET f_monto_acc = 0
            LET f_monto_pes = 0
            LET s_grupo     = 0

            DECLARE cur_3 CURSOR FOR eje_saldo_dia_asc
            FOREACH cur_3 USING  reg_5.nss   ,--nss
                                 s_subcta    ,--subcuenta
                                 s_grupo     ,--grupo
                                 HOY          --fecha_saldo
                            INTO f_subcuenta ,
                                 f_siefore   ,
                                 f_monto_acc ,
                                 f_monto_pes

                IF f_monto_pes IS NULL OR f_monto_pes = " "
                OR f_monto_pes < 0 THEN
                    LET f_monto_pes = 0
                END IF

                IF f_monto_acc IS NULL OR f_monto_acc = " "
                OR f_monto_acc < 0 THEN
                    LET f_monto_acc = 0
                END IF

                IF f_siefore <> 11 THEN
                    #-- Marcamos como activo el registro de la siefore actual
                    LET arr_siefore[f_siefore].activo = TRUE
                    CASE f_subcuenta
                        WHEN 1
                            LET arr_siefore[f_siefore].acciones_ret97 = f_monto_acc
                        WHEN 2
                            LET arr_siefore[f_siefore].acciones_cv    = f_monto_acc
                        WHEN 5
                            LET arr_siefore[f_siefore].acciones_cs    = f_monto_acc
                        WHEN 6
                            LET arr_siefore[f_siefore].acciones_est   = f_monto_acc
                        WHEN 7
                            LET arr_siefore[f_siefore].acciones_ret92 = f_monto_acc
                        WHEN 8
                            LET arr_siefore[f_siefore].acciones_viv92 = f_monto_acc
                        WHEN 9
                            LET arr_siefore[f_siefore].acciones_esp   = f_monto_acc
                    END CASE
                ELSE
                    CASE f_subcuenta
                        WHEN 4
                            LET s11_acc_viv97 = f_monto_acc
                        WHEN 8
                            LET s11_acc_viv92 = f_monto_acc
                    END CASE
                END IF

                --aqui provision--
                IF f_monto_acc > 0 THEN
                    LET si_provisiono = 0
                    LET folio_sua     = ""
                    LET f_monto_acc   = -f_monto_acc
                    LET f_monto_pes   = -f_monto_pes
                    IF f_monto_pes <> 0 THEN 

                        DECLARE cur_4 CURSOR FOR eje_provisiona
                        OPEN cur_4
                            USING ultimo_folio        ,--folio
                                  folio_sua           ,--folio_sua
                                  reg_5.nss           ,--nss
                                  f_subcuenta         ,--subcuenta
                                  s_tipo_movimiento   ,--tipo_movimiento
                                  reg_5.consecutivo   ,--consecutivo_lote
                                  f_siefore           ,--siefore
                                  f_monto_acc         ,--monto_en_acciones
                                  f_monto_pes         ,--monto_en_pesos
                                  id_aporte           ,--id_aportante
                                  HOY                  --fecha proceso

                            FETCH cur_4 INTO si_provisiono

                            IF si_provisiono < 0 THEN
                                PROMPT "El NSS :",reg_5.nss CLIPPED," NO PROVISIONO LA SUBCTA ",f_subcuenta FOR enter
                            END IF
                        CLOSE cur_4
                    END IF
                END IF
            END FOREACH
        END FOREACH

        #FOR ls_siefore = 1 TO gs_num_siefores    #CPL-2106
        FOR ls_siefore = 1 TO ms_99_siefores
            LET acciones_cv = 0
            #-- Almacenamos solo los registros de las siefores que contengan saldo
            IF arr_siefore[ls_siefore].activo = TRUE THEN

                LET acciones_cv = arr_siefore[ls_siefore].acciones_cv  +
                                  arr_siefore[ls_siefore].acciones_est +
                                  arr_siefore[ls_siefore].acciones_esp

                INSERT INTO ret_monto_siefore
                    VALUES(reg_5.nss                                ,--nss
                           reg_5.consecutivo                        ,--consecutivo
                           ultimo_folio                             ,--folio
                           "D"                                      ,--tipo_retiro
                           5                                        ,--tipo_operacion
                           ls_siefore                               ,--siefore
                           arr_siefore[ls_siefore].acciones_ret97   ,--acciones_ret97
                           acciones_cv                              ,--acciones_cv
                           arr_siefore[ls_siefore].acciones_cs      ,--acciones_cs
                           arr_siefore[ls_siefore].acciones_ret92    --acciones_ret92
                          )
            END IF
        END FOR

        --Modifico FUV--

        INSERT INTO ret_monto_viv
        VALUES(reg_5.nss         ,--nss
               reg_5.consecutivo ,--consecutivo
               ultimo_folio      ,--folio
               "D"               ,--tipo_retiro
               f_fecha_valor_viv ,--fecha_valor_viv
               s11_acc_viv97     ,--acciones_viv97
               s11_acc_viv92     ,--acciones_viv92
               0                 ,--pesos_viv72
               NULL              ,--estado_sub_viv
               0                 ,--acc_viv97_bdsviv
               0                  --acc_viv92_bdsviv
              )

        UPDATE ret_solicitud_tx
        SET    ret_solicitud_tx.folio            = ultimo_folio      ,
               ret_solicitud_tx.saldo_viv97      = s11_acc_viv97     ,
               ret_solicitud_tx.saldo_viv92      = s11_acc_viv92     ,
               ret_solicitud_tx.saldo_viv72      = 0                 ,#PENDIENTE
               ret_solicitud_tx.fecha_envio      = HOY               ,
               ret_solicitud_tx.fecha_valor_viv  = f_fecha_valor_viv ,
               ret_solicitud_tx.estado_solicitud = reg_800.procesado
        WHERE  ret_solicitud_tx.nss              = reg_5.nss
        AND    ret_solicitud_tx.consecutivo      = reg_5.consecutivo
        AND    ret_solicitud_tx.estado_solicitud = reg_800.confirmado

        ---fin modificacion FUV-----------------------------------
    END FOREACH
    
END FUNCTION


FUNCTION primer_paso_benef()
#pp-------------------
    DEFINE reg_5 RECORD #loc #reg_5
        nss                   LIKE ret_solicitud_tx.nss             ,
        consecutivo           LIKE ret_solicitud_tx.consecutivo     ,
        regimen               LIKE ret_solicitud_tx.regimen         ,
        tipo_seguro           LIKE ret_solicitud_tx.tipo_seguro     ,
        tipo_pension          LIKE ret_solicitud_tx.tipo_pension    ,
        tipo_prestacion       LIKE ret_solicitud_tx.tipo_prestacion ,
        grupo                 LIKE ret_solicitud_tx.grupo
    END RECORD

    #DEFINE arr_siefore ARRAY [20] OF RECORD  #CPL-2106
    DEFINE arr_siefore ARRAY [99] OF RECORD
        activo            SMALLINT,
        acciones_ret97    DECIMAL(16,6) ,
        acciones_cv       DECIMAL(16,6) ,
        acciones_cs       DECIMAL(16,6) ,
        acciones_est      DECIMAL(16,6) ,
        acciones_ret92    DECIMAL(16,6) ,
        acciones_viv92    DECIMAL(16,6) ,
        acciones_esp      DECIMAL(16,6)
    END RECORD

    DEFINE #loc #date
        folio_sua             CHAR(06)

    DEFINE #loc #smallint
        ls_siefore            , #-- contador para los ciclos for
        s_grupo               ,
        s_subcta              ,
        si_provisiono         SMALLINT

     DEFINE #loc #decimal
        f_monto_pes          ,
        f_monto_acc          ,
        acciones_cv          ,
        s11_acc_viv97        ,
        s11_acc_viv92        DECIMAL(16,6)
        
    DEFINE ls_tipo_benef         SMALLINT
    DEFINE aux_tipo_benef        SMALLINT
    DEFINE ld_porcentaje         DECIMAL(5,2)
    DEFINE ld_fecha_saldo_benef  DATE
    DEFINE ld_consecutivo        DECIMAL(11,0)
    DEFINE lc_curp               LIKE ret_beneficiario.curp_benef
    DEFINE ls_ind                SMALLINT
    DEFINE ls_count_reg          SMALLINT
    DEFINE lc_mensaje            CHAR(150)
    DEFINE ls_grupo              SMALLINT
    
    DEFINE lr_saldos RECORD
        subcuenta               LIKE dis_cuenta.subcuenta           ,
        siefore                 LIKE dis_cuenta.siefore             ,
        monto_acciones       LIKE dis_cuenta.monto_en_acciones   ,
        monto_pesos          LIKE dis_cuenta.monto_en_pesos
   END RECORD
   
   LET ls_grupo = 0

    DECLARE cur_benef_1 CURSOR FOR
    SELECT  A.nss             ,
            A.consecutivo     ,
            A.regimen         ,
            A.tipo_seguro     ,
            A.tipo_pension    ,
            A.tipo_prestacion ,
            A.grupo
    FROM    ret_solicitud_tx A
    WHERE   A.estado_solicitud IN (reg_800.confirmado,reg_800.transicion)
    AND     A.tipo_retiro      = 'D' --tipo de prestacion = 3
    AND     A.rechazo_cod      = 0
    AND     A.nss IN (SELECT UNIQUE B.nss
                        FROM ret_ctr_benef B
                       WHERE B.consecutivo_solic = A.consecutivo)
    ORDER BY nss

    FOREACH cur_benef_1 INTO reg_5.*
    
        CALL genera_tmp_cuenta(reg_5.nss)
    
        CALL ERRORLOG("")
        LET lc_mensaje = "NSS            >>> ", reg_5.nss
        CALL ERRORLOG(lc_mensaje)
        LET lc_mensaje = "Consecutivo    >>> ", reg_5.consecutivo
        CALL ERRORLOG(lc_mensaje)

        DECLARE cur_ctr_benef_1 CURSOR FOR
        SELECT tipo_benef, porcentaje
          FROM ret_ctr_benef
         WHERE nss               = reg_5.nss
           AND consecutivo_solic = reg_5.consecutivo
           AND tipo_retiro       = 'D'

        FOREACH cur_ctr_benef_1 INTO ls_tipo_benef,
                                     ld_porcentaje
            
            LET aux_tipo_benef = ls_tipo_benef
            
            IF ls_tipo_benef = 1 THEN
                SELECT "OK"
                  FROM afi_mae_benefici a,
                       afi_ctr_beneficiario b
                 WHERE a.n_seguro       = reg_5.nss
                   AND a.n_folio        = b.n_folio
                   AND a.tipo_solicitud = b.tipo_solicitud
                   AND b.tipo_beneficiario = 2    -- CPL-3664
                 GROUP BY 1
                IF STATUS = NOTFOUND THEN
                    LET aux_tipo_benef = 4
                END IF
            END IF
            
            CALL ERRORLOG("")
            LET lc_mensaje = "Tipo Benef     >>> ", ls_tipo_benef
            CALL ERRORLOG(lc_mensaje)
            LET lc_mensaje = "Porcentaje     >>> ", ld_porcentaje
            CALL ERRORLOG(lc_mensaje)
            LET lc_mensaje = "Tipo Benef Aux >>> ", aux_tipo_benef
            CALL ERRORLOG(lc_mensaje)

            INITIALIZE reg_5.grupo TO NULL
            SELECT grupo
              INTO reg_5.grupo
              FROM ret_matriz_benef
             WHERE regimen         = reg_5.regimen
               AND tipo_retiro     = 'D'
               AND tipo_prestacion = reg_5.tipo_prestacion
               AND tipo_benef      = aux_tipo_benef
            
            IF (reg_5.grupo IS NULL OR reg_5.grupo = 0) THEN
            	  UPDATE ret_solicitud_tx
                   SET estado_solicitud = reg_800.cancelado,
                       cod_rechazo_ent  = 322,
                       folio            = ultimo_folio
                 WHERE nss              = reg_5.nss
                   AND consecutivo      = reg_5.consecutivo
                   AND tipo_retiro      = 'D'
                   AND estado_solicitud IN (reg_800.confirmado, reg_800.transicion)
            END IF
            
            LET lc_mensaje = "Grupo          >>> ", reg_5.grupo
            CALL ERRORLOG(lc_mensaje)
            
            DECLARE cur_subct_benef CURSOR FOR
            SELECT A.subcuenta
            FROM   tab_agrupa_subcta A
            WHERE  A.grupo     = reg_5.grupo
            AND    A.subcuenta > 0
            
            FOREACH cur_subct_benef INTO s_subcta
                LET ls_ind      = 0
                LET f_subcuenta = 0
                LET f_siefore   = 0
                LET f_monto_acc = 0
                LET f_monto_pes = 0
            
                DECLARE cur_saldo_benef CURSOR FOR
                SELECT siefore, subcuenta, monto_acc, monto_pes, fecha_saldo
                  FROM ret_saldo_benef
                 WHERE nss       = reg_5.nss
                   AND subcuenta = s_subcta
                 ORDER BY fecha_saldo ASC
                   
                FOREACH cur_saldo_benef INTO f_siefore,
                                             f_subcuenta,
                                             f_monto_acc,
                                             f_monto_pes,
                                             ld_fecha_saldo_benef
                    
                    SELECT "OK"
                      FROM dis_provision dp,
                           tmp_dis_cuenta dc,
                           ret_ctr_benef rcb
                     WHERE dp.nss                = reg_5.nss
                       AND dp.subcuenta          = f_subcuenta
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
                     WHERE nss         = reg_5.nss
                       AND consecutivo = reg_5.consecutivo
                    
                    DECLARE cur_busca_cons_benef CURSOR FOR
                     SELECT UNIQUE a.consecutivo
                       FROM ret_beneficiario a,
				                    ret_solicitud_tx b,
				                    ret_ctr_benef c
                      WHERE a.nss              = reg_5.nss
                        AND a.curp_benef       = lc_curp
					              AND a.nss              = b.nss
					              AND a.consecutivo      = b.consecutivo
					              AND a.nss              = c.nss
					              AND a.consecutivo      = c.consecutivo_solic
					              AND (b.estado_solicitud IN (reg_800.confirmado,
					                                         reg_800.transicion,
					                                         reg_800.liquidado)
					              OR (b.estado_solicitud = reg_800.enviado
                            AND (b.diag_registro = '0' OR b.diag_registro IS NULL OR b.diag_registro = "   ")))
					              AND c.tipo_benef       = ls_tipo_benef
					              AND c.tipo_retiro      = 'D'
					            ORDER BY a.consecutivo ASC
                    
                    LET ld_consecutivo = 0
                    
                    CALL ERRORLOG("")
                    LET lc_mensaje = "Siefore            >>> ", f_siefore
                    CALL ERRORLOG(lc_mensaje)
                    LET lc_mensaje = "Subcuenta          >>> ", f_subcuenta
                    CALL ERRORLOG(lc_mensaje)
                    LET lc_mensaje = "Monto Acciones     >>> ", f_monto_acc
                    CALL ERRORLOG(lc_mensaje)
                    LET lc_mensaje = "Monto Pesos        >>> ", f_monto_pes
                    CALL ERRORLOG(lc_mensaje)
                    LET lc_mensaje = "Fecha Saldo Benef  >>> ", ld_fecha_saldo_benef
                    CALL ERRORLOG(lc_mensaje)
                    LET lc_mensaje = "Curp Beneficiario  >>> ", lc_curp
                    CALL ERRORLOG(lc_mensaje)
            
                    FOREACH cur_busca_cons_benef INTO ld_consecutivo
                        
                        IF ld_consecutivo <> reg_5.consecutivo AND ls_ind = 1 THEN
                        	  CONTINUE FOREACH
                        END IF
                    
                        LET lc_mensaje = "Consecutivo Benef  >>> ", ld_consecutivo
                        CALL ERRORLOG(lc_mensaje)
                    
                        SELECT "OK"
                          FROM dis_provision dp,
                               tmp_dis_cuenta dc
                         WHERE dp.nss              = reg_5.nss
                           AND dp.consecutivo_lote = ld_consecutivo
                           AND dp.subcuenta        = f_subcuenta
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
                             WHERE nss = reg_5.nss
                               AND subcuenta = f_subcuenta
                                   
                            IF ld_consecutivo <> reg_5.consecutivo AND ls_count_reg = 1 THEN
                            	  CONTINUE FOREACH
                            END IF
            
                            IF f_monto_pes IS NULL OR f_monto_pes = " "
                            OR f_monto_pes < 0 THEN
                                LET f_monto_pes = 0
                            END IF
                            
                            IF f_monto_acc IS NULL OR f_monto_acc = " "
                            OR f_monto_acc < 0 THEN
                                LET f_monto_acc = 0
                            END IF
                            
                            --aqui provision--
                            IF f_monto_acc > 0 THEN
            
                                LET f_monto_acc = f_monto_acc * (ld_porcentaje / 100)
                        
                                INITIALIZE lr_saldos.* TO NULL
                                DECLARE cur_saldo_dia_2 CURSOR FOR eje_saldo_dia_asc
                                OPEN cur_saldo_dia_2 USING reg_5.nss,
                                                           f_subcuenta,
                                                           ls_grupo,
                                                           HOY
                        
                                FETCH cur_saldo_dia_2 INTO lr_saldos.*
                                CLOSE cur_saldo_dia_2
                            
                                IF lr_saldos.monto_acciones IS NULL THEN
                                	   LET lr_saldos.monto_acciones = 0
                                END IF
                        
                                IF f_monto_acc > lr_saldos.monto_acciones THEN
                                	  LET f_monto_acc = lr_saldos.monto_acciones
                                END IF
                                    
                                LET f_monto_pes = f_monto_acc * gar_precio_acc[f_siefore].precio_dia
            
                                IF f_subcuenta = 8 OR f_subcuenta = 4 THEN
                                    LET f_monto_pes = f_lib_redondea_val(f_monto_pes, 2)
                                END IF
                                
                                SELECT "OK"
                                  FROM ret_ctr_benef_det
                                 WHERE nss               = reg_5.nss
                                   AND consecutivo_solic = reg_5.consecutivo
                                   AND folio_liquida     = ultimo_folio
                                   AND subcuenta         = f_subcuenta
                                   
                                IF STATUS = NOTFOUND THEN
                                               
                                    INSERT INTO ret_ctr_benef_det VALUES(reg_5.nss           ,
                                                                         reg_5.consecutivo   ,
                                                                         ultimo_folio        ,
                                                                         f_subcuenta         ,
                                                                         f_monto_pes         ,
                                                                         f_monto_acc         ,
                                                                         NULL                ,
                                                                         NULL                ,
                                                                         f_siefore           ,
                                                                         NULL                ,
                                                                         NULL
                                                                         )
                                ELSE
                                    UPDATE ret_ctr_benef_det 
                                       SET monto_total     = monto_total    + f_monto_pes,
                                           acciones_bruto  = acciones_bruto + f_monto_acc
                                     WHERE nss             = reg_5.nss
                                     AND consecutivo_solic = reg_5.consecutivo
                                     AND folio_liquida     = ultimo_folio
                                     AND subcuenta         = f_subcuenta
                                END IF
                            END IF
                        ELSE
                            LET ls_ind = 1
                            EXIT FOREACH
                        END IF
                    END FOREACH
                END FOREACH
            END FOREACH
        END FOREACH
    END FOREACH

    DECLARE cur_benef_2 CURSOR FOR
    SELECT  A.nss             ,
            A.consecutivo     ,
            A.regimen         ,
            A.tipo_seguro     ,
            A.tipo_pension    ,
            A.tipo_prestacion ,
            A.grupo
    FROM    ret_solicitud_tx A
    WHERE   A.estado_solicitud IN (reg_800.confirmado)
    AND     A.tipo_retiro      = 'D' --tipo de prestacion = 3
    AND     A.rechazo_cod      = 0
    AND     A.nss IN (SELECT UNIQUE B.nss
                        FROM ret_ctr_benef B
                       WHERE B.consecutivo_solic = A.consecutivo)
    ORDER BY nss

    FOREACH cur_benef_2 INTO reg_5.*

        #-- Inicializamos variables del arreglo
        #FOR ls_siefore = 1 TO gs_num_siefores   #CPL-2106
        FOR ls_siefore = 1 TO ms_99_siefores
            LET arr_siefore[ls_siefore].activo             = FALSE
            LET arr_siefore[ls_siefore].acciones_ret97     = 0
            LET arr_siefore[ls_siefore].acciones_cv        = 0
            LET arr_siefore[ls_siefore].acciones_cs        = 0
            LET arr_siefore[ls_siefore].acciones_est       = 0
            LET arr_siefore[ls_siefore].acciones_ret92     = 0
            LET arr_siefore[ls_siefore].acciones_viv92     = 0
            LET arr_siefore[ls_siefore].acciones_esp       = 0
        END FOR

        LET s11_acc_viv97            = 0
        LET s11_acc_viv92            = 0

        LET f_subcuenta = 0
        LET f_siefore   = 0
        LET f_monto_acc = 0
        LET f_monto_pes = 0
    
        DECLARE cur_subct_benef_2 CURSOR FOR
        SELECT siefore, subcuenta, SUM(acciones_bruto), SUM(monto_total)
          FROM ret_ctr_benef_det
         WHERE nss               = reg_5.nss
           AND consecutivo_solic = reg_5.consecutivo
           AND folio_liquida     = ultimo_folio
         GROUP BY 1,2

        FOREACH cur_subct_benef_2 INTO f_siefore,
                                       f_subcuenta,
                                       f_monto_acc,
                                       f_monto_pes

            IF f_siefore <> 11 THEN
                #-- Marcamos como activo el registro de la siefore actual
                LET arr_siefore[f_siefore].activo = TRUE
                CASE f_subcuenta
                    WHEN 1
                        LET arr_siefore[f_siefore].acciones_ret97 = f_monto_acc
                    WHEN 2
                        LET arr_siefore[f_siefore].acciones_cv    = f_monto_acc
                    WHEN 5
                        LET arr_siefore[f_siefore].acciones_cs    = f_monto_acc
                    WHEN 6
                        LET arr_siefore[f_siefore].acciones_est   = f_monto_acc
                    WHEN 7
                        LET arr_siefore[f_siefore].acciones_ret92 = f_monto_acc
                    WHEN 8
                        LET arr_siefore[f_siefore].acciones_viv92 = f_monto_acc
                    WHEN 9
                        LET arr_siefore[f_siefore].acciones_esp   = f_monto_acc
                END CASE
            ELSE
                CASE f_subcuenta
                    WHEN 4
                        LET s11_acc_viv97 = f_monto_acc
                    WHEN 8
                        LET s11_acc_viv92 = f_monto_acc
                END CASE
            END IF

            --aqui provision--
            IF f_monto_acc > 0 THEN
                LET si_provisiono = 0
                LET folio_sua     = ""
                LET f_monto_acc   = -f_monto_acc
                LET f_monto_pes   = -f_monto_pes
                IF f_monto_pes <> 0 THEN 

                    DECLARE cur_prov_benef CURSOR FOR eje_provisiona
                    OPEN cur_prov_benef
                        USING ultimo_folio        ,--folio
                              folio_sua           ,--folio_sua
                              reg_5.nss           ,--nss
                              f_subcuenta         ,--subcuenta
                              s_tipo_movimiento   ,--tipo_movimiento
                              reg_5.consecutivo   ,--consecutivo_lote
                              f_siefore           ,--siefore
                              f_monto_acc         ,--monto_en_acciones
                              f_monto_pes         ,--monto_en_pesos
                              id_aporte           ,--id_aportante
                              HOY                  --fecha proceso

                        FETCH cur_prov_benef INTO si_provisiono

                        IF si_provisiono < 0 THEN
                            PROMPT "El NSS :",reg_5.nss CLIPPED," NO PROVISIONO LA SUBCTA ",f_subcuenta FOR enter
                        END IF
                    CLOSE cur_prov_benef
                END IF 
            END IF
        END FOREACH

        #FOR ls_siefore = 1 TO gs_num_siefores    #CPL-2106
        FOR ls_siefore = 1 TO ms_99_siefores
            LET acciones_cv = 0
            #-- Almacenamos solo los registros de las siefores que contengan saldo
            IF arr_siefore[ls_siefore].activo = TRUE THEN

                LET acciones_cv = arr_siefore[ls_siefore].acciones_cv  +
                                  arr_siefore[ls_siefore].acciones_est +
                                  arr_siefore[ls_siefore].acciones_esp

                INSERT INTO ret_monto_siefore
                    VALUES(reg_5.nss                                ,--nss
                           reg_5.consecutivo                        ,--consecutivo
                           ultimo_folio                             ,--folio
                           "D"                                      ,--tipo_retiro
                           5                                        ,--tipo_operacion
                           ls_siefore                               ,--siefore
                           arr_siefore[ls_siefore].acciones_ret97   ,--acciones_ret97
                           acciones_cv                              ,--acciones_cv
                           arr_siefore[ls_siefore].acciones_cs      ,--acciones_cs
                           arr_siefore[ls_siefore].acciones_ret92    --acciones_ret92
                          )
            END IF
        END FOR

        --Modifico FUV--

        INSERT INTO ret_monto_viv
        VALUES(reg_5.nss         ,--nss
               reg_5.consecutivo ,--consecutivo
               ultimo_folio      ,--folio
               "D"               ,--tipo_retiro
               f_fecha_valor_viv ,--fecha_valor_viv
               s11_acc_viv97     ,--acciones_viv97
               s11_acc_viv92     ,--acciones_viv92
               0                 ,--pesos_viv72
               NULL              ,--estado_sub_viv
               0                 ,--acc_viv97_bdsviv
               0                  --acc_viv92_bdsviv
              )

        UPDATE ret_solicitud_tx
        SET    ret_solicitud_tx.folio            = ultimo_folio      ,
               ret_solicitud_tx.saldo_viv97      = s11_acc_viv97     ,
               ret_solicitud_tx.saldo_viv92      = s11_acc_viv92     ,
               ret_solicitud_tx.saldo_viv72      = 0                 ,#PENDIENTE
               ret_solicitud_tx.fecha_envio      = HOY               ,
               ret_solicitud_tx.fecha_valor_viv  = f_fecha_valor_viv ,
               ret_solicitud_tx.estado_solicitud = reg_800.procesado
        WHERE  ret_solicitud_tx.nss              = reg_5.nss
        AND    ret_solicitud_tx.consecutivo      = reg_5.consecutivo
        AND    ret_solicitud_tx.estado_solicitud = reg_800.confirmado
        
        UPDATE ret_ctr_benef 
           SET folio_liquida     = ultimo_folio
         WHERE nss               = reg_5.nss
           AND consecutivo_solic = reg_5.consecutivo
           AND tipo_retiro       = 'D'

        ---fin modificacion FUV-----------------------------------
    END FOREACH
   
    CALL fn_desmarca_cuenta()
    
END FUNCTION

FUNCTION segundo_paso()
#sp-------------------
    DEFINE #loc #reg_6
        reg_6           RECORD LIKE ret_solicitud_tx.*

    DEFINE
        ls_num_montos   SMALLINT

    DEFINE
        ruta_det_nss      ,
        ruta_det_sie      ,
        ruta_det_viv      CHAR(100)

    DEFINE reg_9 RECORD #loc #reg_9
        siefore               SMALLINT      ,
        acciones_ret97        DECIMAL(16,6) ,
        acciones_cv           DECIMAL(16,6) ,
        acciones_cs           DECIMAL(16,6) ,
        acciones_ret92        DECIMAL(16,6)
    END RECORD

    DEFINE lr_info_biometrica RECORD
        idsolicitante        CHAR(02),
        curpsolicitante      CHAR(18),
        sellotrabajador      CHAR(14),
        curpagenteservicio   CHAR(18),
        identificador        INTEGER ,
        tiposervicio         INTEGER
    END RECORD

    #-- Determinamos la ubicacion de los reportes
    LET ruta_det_nss = g_param_ret.ruta_envio CLIPPED, "/", "DET-NSS-D-tmp"
    LET ruta_det_nss = ruta_det_nss CLIPPED

    LET ruta_det_sie = g_param_ret.ruta_envio CLIPPED, "/", "DET-SIE-D-tmp"
    LET ruta_det_sie = ruta_det_sie CLIPPED

    LET ruta_det_viv = g_param_ret.ruta_envio CLIPPED, "/", "DET-VIV-D-tmp"
    LET ruta_det_viv = ruta_det_viv CLIPPED

    LET G_LISTA_DET  = g_param_ret.ruta_envio CLIPPED, "/", "DET-D"
    LET G_LISTA_DET  = G_LISTA_DET CLIPPED

    LET sw_1     = 0
    LET cont_reg = 0

    DECLARE cur_5 CURSOR FOR
    SELECT  *
    FROM    ret_solicitud_tx
    WHERE   ret_solicitud_tx.estado_solicitud = reg_800.procesado
    AND     ret_solicitud_tx.folio            = ultimo_folio
    AND     ret_solicitud_tx.tipo_retiro      = 'D'

    FOREACH cur_5 INTO reg_6.*

        #-- Iniciamos los reportes
        START REPORT det_solicitudes_03 TO ruta_det_nss
        START REPORT det_vivienda_05 TO ruta_det_viv

        LET sw_1          = 1
        LET cont_reg      = cont_reg + 1
        LET ls_num_montos = 0

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
        AND    nss           = reg_6.nss
        AND    consecutivo   = reg_6.consecutivo

        IF lr_info_biometrica.idsolicitante IS NULL THEN
           LET lr_info_biometrica.idsolicitante   = '00'
           LET lr_info_biometrica.sellotrabajador = '00000000000000'
        END IF

        OUTPUT TO REPORT det_solicitudes_03(reg_6.*, lr_info_biometrica.*)
        OUTPUT TO REPORT det_vivienda_05(reg_6.*)

        SELECT COUNT(*)
            INTO   ls_num_montos
            FROM   ret_monto_siefore
            WHERE  folio          = ultimo_folio
            AND    nss            = reg_6.nss
            AND    consecutivo    = reg_6.consecutivo
            AND    tipo_operacion = 5

        #-- Si existen registros de saldo en siefores para el nss actual,
        #-- los barremos para generar el reporte del detalle de siefores
        IF ls_num_montos > 0 THEN
            START REPORT det_siefores_04 TO ruta_det_sie

            DECLARE cur_6 CURSOR FOR
                SELECT siefore        ,
                       acciones_ret97 ,
                       acciones_cv    ,
                       acciones_cs    ,
                       acciones_ret92
                FROM   ret_monto_siefore
                WHERE  folio          = ultimo_folio
                AND    nss            = reg_6.nss
                AND    consecutivo    = reg_6.consecutivo
                AND    tipo_operacion = 5

            FOREACH cur_6 INTO reg_9.*
                OUTPUT TO REPORT det_siefores_04(reg_6.nss, reg_6.curp, reg_9.*)
            END FOREACH

            FINISH REPORT det_siefores_04
        END IF

        FINISH REPORT det_solicitudes_03
        FINISH REPORT det_vivienda_05

        #-- Una vez generados los archivos para el nss actual, los concatenamos en uno solo
        CALL concat_reportes(ruta_det_nss  ,
                             ruta_det_sie  ,
                             ruta_det_viv  ,
                             ls_num_montos ,
                             cont_reg)
    END FOREACH

    LET comando = "chmod 777 ", G_LISTA_DET CLIPPED
    RUN comando

    IF sw_1 = 1 THEN

        LET HORA_GEN = HORA[1,5]

        INSERT INTO ret_ctr_envio_lote
            VALUES (HOY                ,--fecha_genera
                    'D'                ,--tipo_retiro
                    ultimo_folio       ,--folio
                    ' '                ,--fecha_envio
                    ' '                ,--fecha_reverso
                    HORA_GEN           ,--hora_genera
                    ' '                ,--hora_envio
                    usuario            ,--usuario_genera
                    ' '                ,--usuario_envio
                    ' '                ,--usuario_reverso
                    reg_800.procesado  ,--estado
                    cont_reg            --total_registros
                   )

    END IF

END FUNCTION


FUNCTION concat_reportes(lc_det_nss, lc_det_sie, lc_det_viv, p_monto, p_regs)

    DEFINE
        lc_det_nss      ,
        lc_det_sie      ,
        lc_det_viv      CHAR(100)

    DEFINE p_monto SMALLINT
    DEFINE p_regs SMALLINT

    #--
    DEFINE
        ruta_tmp        ,
        ruta_det        CHAR(100)

    DEFINE
        com_cat         ,
        com_rm          CHAR(500)

    #---

    LET ruta_tmp    = g_param_ret.ruta_envio CLIPPED, "/", "rep_temporal.tmp"
    LET ruta_tmp    = ruta_tmp CLIPPED

    LET ruta_det    = g_param_ret.ruta_envio CLIPPED, "/", "detalle.tmp"
    LET ruta_det    = ruta_det CLIPPED

    #-- Preparamos el comando para borrar los reportes
    LET com_rm = "rm ", lc_det_nss CLIPPED, " ", lc_det_viv CLIPPED

    #-- Si se genero el reporte de saldo de siefores, se incluye en los comandos
    IF p_monto > 0 THEN

        LET com_cat = "cat ", lc_det_nss CLIPPED, " ",
                              lc_det_sie CLIPPED, " ",
                              lc_det_viv CLIPPED, " > ", ruta_tmp

        LET com_cat = com_cat CLIPPED

        LET com_rm = com_rm CLIPPED, " ", lc_det_sie CLIPPED
    ELSE
        LET com_cat = "cat ", lc_det_nss CLIPPED, " ",
                              lc_det_viv CLIPPED, " > ", ruta_tmp CLIPPED
    END IF

    #-- Concatenamos los archivos en uno solo y borramos los temporales
    RUN com_cat
    RUN com_rm

    #-- Acumulamos el archivo generado al reporte final
    IF p_regs > 1 THEN
        LET com_cat = "cat ", G_LISTA_DET, " ", ruta_tmp, " > ", ruta_det
        RUN com_cat

        LET com_cat = " mv ", ruta_det, " ", G_LISTA_DET
        RUN com_cat
    ELSE
        #-- Si es el primer registro procesado entonces creamos el archivo de detalle
        LET com_cat = " cp ", ruta_tmp, " ", G_LISTA_DET
        RUN com_cat
    END IF

    #-- Borramos el temporal generado para quedarnos solo con el archivo de detalle
    LET com_rm = "rm ", ruta_tmp
    RUN com_rm

END FUNCTION

REPORT det_solicitudes_03(reg_7, pr_info_biometrica)
#ds05----------------------------
    DEFINE #loc #reg_7
        reg_7                 RECORD LIKE ret_solicitud_tx.*

    DEFINE reg_8 RECORD  #loc #reg_8
        paterno               LIKE afi_mae_afiliado.paterno ,
        materno               LIKE afi_mae_afiliado.materno ,
        nombres               LIKE afi_mae_afiliado.nombres
    END RECORD

    DEFINE pr_info_biometrica RECORD
        idsolicitante        CHAR(02),
        curpsolicitante      CHAR(18),
        sellotrabajador      CHAR(14),
        curpagenteservicio   CHAR(18),
        identificador        INTEGER ,
        tiposervicio         INTEGER
    END RECORD

    DEFINE #loc #char
        vmax_folio            INTEGER

    DEFINE #loc #char
        c_sec_pension         CHAR(02)

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
            WHERE  A.nss              = reg_7.nss
            AND    A.fecha_resolucion = reg_7.fecha_resolucion
            AND    A.tipo_prestacion  = 3
            AND    A.diag_datamart   IN (101,106,300,301,218)

            SELECT MAX(A.sec_pension)
            INTO   c_sec_pension
            FROM   ret_det_datamart A
            WHERE  A.nss              = reg_7.nss
            AND    A.fecha_resolucion = reg_7.fecha_resolucion
            AND    A.tipo_prestacion  = 3
            AND    A.diag_datamart   IN (101,106,300,301,218)
            AND    A.folio            = vmax_folio

            IF SQLCA.SQLCODE = NOTFOUND THEN
                LET c_sec_pension = 2 SPACES
            END IF


            SELECT paterno       ,
                   materno       ,
                   nombres
            INTO   reg_8.paterno ,
                   reg_8.materno ,
                   reg_8.nombres
            FROM   afi_mae_afiliado
            WHERE  n_seguro = reg_7.nss

        PRINT
            COLUMN 001, "03"                                   ,--tipo_registro
            COLUMN 003, "04"                                   ,--ident_servicio
            COLUMN 005, "05"                                   ,--ident_operacion
            COLUMN 007, reg_7.nss                              ,--nss
            COLUMN 018, reg_7.curp                             ,--curp
            COLUMN 036, reg_8.nombres                          ,--nombre
            COLUMN 076, reg_8.paterno                          ,--apellido paterno
            COLUMN 116, reg_8.materno                          ,--apellido materno
            COLUMN 156, c_sec_pension                          ,--secuencia pension
            COLUMN 158, reg_7.tipo_retiro                      ,--tipo retiro
            COLUMN 159, reg_7.regimen                          ,--regimen
            COLUMN 161, reg_7.tipo_seguro                      ,--tipo seguro
            COLUMN 163, reg_7.tipo_pension                     ,--tipo pension
            COLUMN 165, reg_7.tipo_prestacion USING"&&"        ,--tipo prestacion
            COLUMN 167, "00010101"                             ,--fecha inicio pension
            COLUMN 175, reg_7.fecha_resolucion USING"YYYYMMDD" ,--fecha_resolucion
            COLUMN 183, "00000"                                ,--porcentaje valuacion
            COLUMN 188, "0000"                                 ,--semanas cotizadas
            COLUMN 192, reg_7.fecha_solicitud USING "YYYYMMDD" ,--fecha solicitud trabajador
                                                                --cve docto probatorio
            COLUMN 201, "00010101"                             ,--fecha nacimiento
            COLUMN 227, "000000"                               ,--fecha periodo
            COLUMN 233, reg_7.consecutivo USING "&&&&&&&&&&&"  ,--consecutivo trabajador
            #CPL-2430
            COLUMN 244, pr_info_biometrica.idsolicitante       , --ID Solicitante
            COLUMN 246, pr_info_biometrica.curpsolicitante     , --CURP Solicitante
            COLUMN 264, pr_info_biometrica.sellotrabajador     , --Sello único de verificación
            COLUMN 278, pr_info_biometrica.curpagenteservicio  , --CURP Agente de Servicio
            COLUMN 296, 485 SPACES                               --Campos 31 a 48 Filler LAYOUT(780 posiciones)
END REPORT

#---------------------------------------------------------------------------
# Reporte que genera el layout de detalle de siefore
#---------------------------------------------------------------------------
REPORT det_siefores_04(p_nss, p_curp, reg_11)
#ds04----------------------------

    DEFINE p_nss  LIKE ret_solicitud_tx.nss
    DEFINE p_curp LIKE ret_solicitud_tx.curp

    DEFINE reg_11 RECORD #loc #reg_11
        siefore               SMALLINT      ,
        acciones_ret97        DECIMAL(16,6) ,
        acciones_cv           DECIMAL(16,6) ,
        acciones_cs           DECIMAL(16,6) ,
        acciones_ret92        DECIMAL(16,6)
    END RECORD

    DEFINE #loc #char
        v_siefore           CHAR(02) ,
        c15_acc_ret97       CHAR(15) ,
        c14_acc_ret97       CHAR(14) ,
        c15_acc_cv          CHAR(15) ,
        c14_acc_cv          CHAR(14) ,
        c15_acc_cs          CHAR(15) ,
        c14_acc_cs          CHAR(14) ,
        c15_acc_ret92       CHAR(15) ,
        c14_acc_ret92       CHAR(14)

    OUTPUT
        PAGE LENGTH   1
    	LEFT MARGIN   0
	    RIGHT MARGIN  0
	    TOP MARGIN    0
	    BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW

            #-- Formateamos el valor de la siefore
            IF reg_11.siefore >= 10 THEN
                LET v_siefore = reg_11.siefore USING "&&"
            ELSE
                LET v_siefore[1] = "0"
                LET v_siefore[2] = reg_11.siefore USING "&"
            END IF

            #-- Obtenemos el valor de las Acciones de Retiro 97
            IF reg_11.acciones_ret97 IS NULL THEN
                LET reg_11.acciones_ret97 = 0
            END IF

            LET c15_acc_ret97           = reg_11.acciones_ret97 USING"&&&&&&&&.&&&&&&"
            LET c14_acc_ret97           = c15_acc_ret97[01,08],
                                          c15_acc_ret97[10,15]

            #-- Obtenemos el valor de las Acciones de CV
            IF reg_11.acciones_cv IS NULL THEN
                LET reg_11.acciones_cv = 0
            END IF

            LET c15_acc_cv              = reg_11.acciones_cv USING"&&&&&&&&.&&&&&&"
            LET c14_acc_cv              = c15_acc_cv[01,08],
                                          c15_acc_cv[10,15]

            #-- Obtenemos el valor de las Acciones de CS
            IF reg_11.acciones_cs IS NULL THEN
                LET reg_11.acciones_cs = 0
            END IF

            LET c15_acc_cs              = reg_11.acciones_cs USING"&&&&&&&&.&&&&&&"
            LET c14_acc_cs              = c15_acc_cs[01,08],
                                          c15_acc_cs[10,15]

            #-- Obtenemos el valor de las Acciones de Retiro 92
            IF reg_11.acciones_ret92 IS NULL THEN
                LET reg_11.acciones_ret92 = 0
            END IF

            LET c15_acc_ret92           = reg_11.acciones_ret92 USING"&&&&&&&&.&&&&&&"
            LET c14_acc_ret92           = c15_acc_ret92[01,08],
                                          c15_acc_ret92[10,15]

        PRINT
            COLUMN 001, "04"                                   ,# Tipo de registro
            COLUMN 003, p_nss                                  ,
            COLUMN 014, p_curp                                 ,
            COLUMN 032, v_siefore                              ,# Clave de siefore
            COLUMN 034, c14_acc_ret97                          ,# Acciones de retiro 97
            COLUMN 048, c14_acc_cv                             ,# Acciones de CV
            COLUMN 062, c14_acc_cs                             ,# Acciones de CS
            COLUMN 076, c14_acc_ret92                          ,# Acciones de retiro 92
            COLUMN 090, 691 SPACES                              -- Se modifica para hacer la longitud de acuerdo al cambio de layout

END REPORT

#---------------------------------------------------------------------------
# Reporte que genera el layout de detalle de vivienda
#---------------------------------------------------------------------------
REPORT det_vivienda_05(reg_7)
#dv05----------------------------

    DEFINE #loc #reg_7
        reg_7               RECORD LIKE ret_solicitud_tx.*

    DEFINE #loc #dec
       d2_saldo_viv97       ,
       d2_saldo_viv92       DECIMAL(14,6)

    DEFINE #loc #char
        c8_fecha_val        CHAR(08) ,
        c10_fecha_val       CHAR(10) ,
        c15_impt_viv_97     CHAR(15) ,
        c14_impt_viv_97     CHAR(14) ,
        c15_impt_viv_92     CHAR(15) ,
        c14_impt_viv_92     CHAR(14)

    OUTPUT
        PAGE LENGTH   1
    	LEFT MARGIN   0
	    RIGHT MARGIN  0
	    TOP MARGIN    0
	    BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW

            #-- Obtenemos la fecha Valor de Vivienda
            LET c10_fecha_val      = reg_7.fecha_valor_viv
            LET c8_fecha_val       = c10_fecha_val[7,10] ,
                                     c10_fecha_val[1,02] ,
                                     c10_fecha_val[4,05]

            #-- Obtenemos aplicacion de intereses viv. 97
            LET d2_saldo_viv97     = reg_7.saldo_viv97
            LET c15_impt_viv_97    = d2_saldo_viv97 USING "&&&&&&&&.&&&&&&"
            LET c14_impt_viv_97    = c15_impt_viv_97[01,08],
                                     c15_impt_viv_97[10,15]

            #-- Obtenemos aplicacion de intereses viv. 92
            LET d2_saldo_viv92     = reg_7.saldo_viv92
            LET c15_impt_viv_92    = d2_saldo_viv92 USING "&&&&&&&&.&&&&&&"
            LET c14_impt_viv_92    = c15_impt_viv_92[01,08],
                                     c15_impt_viv_92[10,15]

        PRINT
            COLUMN 001, "05"                                   ,# Tipo de registro
            COLUMN 003, reg_7.nss                              ,
            COLUMN 014, reg_7.curp                             ,
            COLUMN 032, c8_fecha_val                           ,# Fecha valor vivienda
            COLUMN 040, c14_impt_viv_97                        ,# Intereses Viv 97
            COLUMN 054, c14_impt_viv_92                        ,# Intereses Viv 92
            COLUMN 068, "00000000000000"                       ,# Fondo Viv 72
            #--                                                   Estatus de la subcta vivienda
            COLUMN 083, "00000000000000"                       ,# Intereses Viv 97 en BDSVIV
            COLUMN 097, "00000000000000"                       ,# Intereses Viv 92 en BDSVIV
            COLUMN 111, 670 SPACES                              -- Se modifica para hacer la longitud de acuerdo al cambio de layout

END REPORT
