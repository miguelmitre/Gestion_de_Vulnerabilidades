#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETM013B => FUNCIONES PARA LA PRELIQUIDACION Y LIQUIDACION DE LOS     #
#                  => REINTEGROS NO COBRADOS APROBADOS                          #
#Fecha creacion    => 20 DE DICIEMBRE DE 2021                                   #
#By                => CRISTINA ABASOLO TAPIA                                    #
#Sistema           => RET (CPL-3437)                                            #
#################################################################################

DATABASE safre_af

GLOBALS "RETM015A.4gl"

DEFINE mr_precio_acc ARRAY [99] OF RECORD
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
END RECORD

DEFINE
        m_siefore             SMALLINT

#---------------------------------------------------------------------------#
# f_preliquida : Realiza la preliquidacion de la reinversion de los retiros #
#                parciales por desempleo.                                   #
#---------------------------------------------------------------------------#
FUNCTION f_preliquida()

    CALL f_obtiene_precios_accion(HOY)

    CALL f_primer_paso()   #-- Realiza la preliquidacion de los montos
    CALL f_segundo_paso()   #-- Vacia la informacion hacia las tablas fisicas

END FUNCTION

#---------------------------------------------------------------------------#
# f_preliquida : Realiza la preliquidacion de la reinversion de los retiros #
#                parciales por desempleo.                                   #
#---------------------------------------------------------------------------#
FUNCTION f_liquida()
DEFINE
        li_num_afect,
        li_num_afect1,
        li_num_afect2       INTEGER,
        lc_nss              CHAR(11),
        ld_consec           DECIMAL(11,0)
        
DEFINE ls_marca_entra       SMALLINT
DEFINE ls_marca_causa       SMALLINT
DEFINE ld_consec_aux        DECIMAL(11,0)

    LET li_num_afect  =  0    LET li_num_afect1 =  0
    LET li_num_afect2 =  0

    ---DESMARCA CUENTA
    LET gc_char = " "
    LET gc_char = "EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"
    PREPARE eje_desmarca FROM gc_char

    DECLARE cur_liq CURSOR FOR
     SELECT UNIQUE a.nss, a.consecutivo_lote
     FROM ret_preliquida a, ret_par_reintegro_des b
     WHERE a.nss = b.nss
     AND a.consecutivo_lote = b.consecutivo
     AND a.folio  = b.folio
     AND a.folio  = gd_folio
     AND b.estado = gr_edo.preliquidado

   FOREACH cur_liq INTO lc_nss, ld_consec
      LET li_num_afect = li_num_afect + 1
      DISPLAY "R. PROCESADOS                          : ", li_num_afect
               USING "<<<,<<&" AT 12,14

      INSERT INTO dis_cuenta
      SELECT * FROM ret_preliquida
      WHERE nss = lc_nss
      AND consecutivo_lote = ld_consec
      AND folio = gd_folio

      SELECT "OK" FROM cta_act_marca
      WHERE nss = lc_nss
      AND correlativo = ld_consec
      AND marca_cod   = gs_marca_proceso
      GROUP BY 1

       IF SQLCA.SQLCODE = 0 THEN
          EXECUTE eje_desmarca USING lc_nss           ,   -- nss
                                     gs_marca_proceso ,   -- marca entrante
                                     ld_consec        ,   -- consecutivo
                                     gs_zero          ,   -- estado_marca
                                     gs_marca_proceso ,   -- marca_causa
                                     gc_usuario           -- usuario

          LET li_num_afect1 = li_num_afect1 + 1

          DISPLAY "DESMARCAS  POR  FUNCION                : ", li_num_afect1
            USING "<<<<<&" AT 13,14

       END IF

      LET ls_marca_entra = 0
      LET ls_marca_causa = 0
      LET ld_consec_aux  = 0

      --Elimina marca saldo 0
      SELECT marca_cod, marca_causa, correlativo
      INTO ls_marca_entra, ls_marca_causa, ld_consec_aux
      FROM cta_act_marca
      WHERE nss = lc_nss
      AND marca_cod IN (151)

      IF ls_marca_entra = 151 THEN
         EXECUTE eje_desmarca USING lc_nss           ,   -- nss
                                    ls_marca_entra   ,   -- marca entrante
                                    ld_consec_aux    ,   -- consecutivo
                                    gs_zero          ,   -- estado_marca
                                    ls_marca_causa   ,   -- marca_causa
                                    gc_usuario           -- usuario
      END IF

      --Elimina marcas de inhabilitación
      SELECT marca_cod, marca_causa, correlativo
      INTO ls_marca_entra, ls_marca_causa, ld_consec_aux
      FROM cta_act_marca
      WHERE nss = lc_nss
      AND marca_cod IN (140,160)

      IF ls_marca_entra = 140 OR ls_marca_entra = 160 THEN
         EXECUTE eje_desmarca USING lc_nss           ,   -- nss
                                    ls_marca_entra   ,   -- marca entrante
                                    ld_consec_aux    ,   -- consecutivo
                                    gs_zero          ,   -- estado_marca
                                    ls_marca_causa   ,   -- marca_causa
                                    gc_usuario           -- usuario

      END IF

       UPDATE ret_par_reintegro_des SET estado = gr_edo.liquidado
       WHERE nss = lc_nss
       AND consecutivo = ld_consec
       AND estado = gr_edo.preliquidado
       AND folio  = gd_folio

       LET li_num_afect2 = li_num_afect2 + SQLCA.SQLERRD[3]
       DISPLAY "REGISTROS DE REINTEGRO ACTUALIZADOS       : ", li_num_afect2
                USING "<<<<<&" AT 14,14

   END FOREACH
   
    CALL f_lib_error_msg("PROCESO TERMINADO CORRECTAMENTE")

    DISPLAY "                                             " AT 12,1
    DISPLAY "                                             " AT 13,1
    DISPLAY "                                             " AT 14,1
    DISPLAY "                                             " AT 15,1
    DISPLAY "                                             " AT 16,1

END FUNCTION

#---------------------------------------------------------------------------#
# f_obtiene_precios_accion : Obtiene los precios de accion para la fecha    #
#                            dada por ldt_precios                           #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_precios_accion(ldt_precios)

    DEFINE
        ldt_precios             DATE

    DEFINE lr_precio_acc RECORD
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE
        lc_precios_acc          CHAR(100) ,
        lc_mensaje              CHAR(100) ,
        lc_siefore              CHAR(002)

    DEFINE
        ls_sie                  SMALLINT

    LET ls_sie = 1

    LET lc_precios_acc = " EXECUTE FUNCTION fn_verifica_precio_accion(?)"
    PREPARE eje_precios_accion FROM lc_precios_acc

    DECLARE c_precios CURSOR FOR eje_precios_accion
    FOREACH c_precios USING ldt_precios
                      INTO lr_precio_acc.*

            IF lr_precio_acc.estado <> 0 THEN
                LET lc_siefore = lr_precio_acc.siefore

                LET lc_mensaje = " FALTAN PRECIOS DE ACCION: FECHA ", lr_precio_acc.fecha USING "DD/MM/yyyy",
                                 " -- SIEFORE ", lc_siefore CLIPPED

                PROMPT lc_mensaje FOR CHAR gc_char
                EXIT PROGRAM
            ELSE
                LET ls_sie                    = lr_precio_acc.siefore
                LET mr_precio_acc[ls_sie].*  = lr_precio_acc.*
            END IF
    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# f_primer_paso : Llama a las funciones que realizan los procesos de        #
#               preliquidacion                                              #
#---------------------------------------------------------------------------#
FUNCTION f_primer_paso()

    DEFINE lr_liquida RECORD
        nss                 LIKE ret_par_reintegro_des.nss            ,
        curp                LIKE ret_par_reintegro_des.curp           ,
        consecutivo         LIKE ret_par_reintegro_des.consecutivo    ,
        folio_his_liq       LIKE ret_par_reintegro_des.folio_his_liq  ,
        monto_en_pesos      LIKE ret_par_reintegro_des.monto_en_pesos
    END RECORD

    DEFINE lr_pago RECORD
        subcta          SMALLINT      ,
        siefore         SMALLINT      ,
        acc_pagar       DECIMAL(16,6) ,
        pesos_pagar     DECIMAL(16,6)
    END RECORD

    DEFINE
        lc_msg              CHAR(60)

    DEFINE
        ls_siefore_reg      SMALLINT
        
    DEFINE v_existe         SMALLINT

    -- -----------------------------------------------------------------------------

    CALL f_tablas_tmp()

    DECLARE cur_reg CURSOR FOR
        SELECT a.nss                 ,
               a.curp                ,
               a.consecutivo         ,
               a.folio_his_liq       ,
               a.monto_en_pesos
        FROM   ret_par_reintegro_des a
        WHERE  a.folio    = gd_folio
        AND    a.estado   = gr_edo.recibido
        

    -- Ciclo sobre cada NSS a liquidar
    FOREACH cur_reg INTO lr_liquida.*
        DISPLAY "NSS >>> ", lr_liquida.nss, " - ", lr_liquida.consecutivo, " - ", lr_liquida.folio_his_liq
    	
        CALL f_genera_tmp_cuenta(lr_liquida.nss)

        LET ls_siefore_reg = f_lib_obtiene_siefore_act(lr_liquida.nss) #MLM-3409
        
        DECLARE  cur_sbc CURSOR FOR
         SELECT  UNIQUE subcuenta,
                 siefore  ,
                 monto_en_acciones ,
                 monto_en_pesos
         FROM    tmp_dis_cuenta_3437
         WHERE   nss = lr_liquida.nss
         AND     consecutivo_lote = lr_liquida.consecutivo
         AND     folio = lr_liquida.folio_his_liq
         AND     tipo_movimiento IN (875,876,877,878)
         
         FOREACH cur_sbc INTO lr_pago.*
                       DISPLAY "Montos >>> ",lr_pago.*
            IF (lr_pago.subcta > 0) THEN

                --#inserta abono a la siefore basica
                LET lr_pago.pesos_pagar = lr_pago.pesos_pagar * -1
                LET lr_pago.acc_pagar = lr_pago.pesos_pagar / mr_precio_acc[ls_siefore_reg].precio_dia
                
                SELECT NVL(COUNT(*), 0)
                  INTO v_existe
                  FROM tmp_preliquida_reint
                 WHERE consecutivo_lote = lr_liquida.consecutivo
                   AND subcuenta        = lr_pago.subcta
                   AND nss              = lr_liquida.nss
                   AND folio            = gd_folio

                IF v_existe > 0 THEN
                    UPDATE tmp_preliquida_reint
                       SET monto_en_pesos    = monto_en_pesos    + lr_pago.pesos_pagar,
                           monto_en_acciones = monto_en_acciones + lr_pago.acc_pagar
                     WHERE consecutivo_lote = lr_liquida.consecutivo
                       AND subcuenta        = lr_pago.subcta
                       AND nss              = lr_liquida.nss
                       AND folio            = gd_folio
                ELSE
                    CALL f_inserta_preliquida(gd_folio                               ,
                                              lr_liquida.nss                         ,
                                              lr_liquida.curp                        ,
                                              lr_liquida.consecutivo                 ,
                                              lr_pago.subcta                         ,
                                              ls_siefore_reg                         ,
                                              gs_tipo_mov                            ,
                                              HOY                                    ,
                                              mr_precio_acc[ls_siefore_reg].precio_dia ,
                                              lr_pago.acc_pagar                     ,
                                              lr_pago.pesos_pagar
                                             )
                END IF

            ELSE
                LET lc_msg  = "ERROR EN LA PRELIQUIDACION ", lr_liquida.nss, " CVE ERR: ", lr_pago.subcta
                CALL f_lib_error_msg(lc_msg)
                EXIT PROGRAM
            END IF
            INITIALIZE lr_pago.* TO NULL
        END FOREACH -- Siguiente subcuenta

         INITIALIZE lr_liquida.* TO NULL
         DISPLAY ""

    END FOREACH -- Siguiente NSS

END FUNCTION

#------------------------------------------------------------------------------#
# f_segundo_paso : Vacia la informacion de las tablas temporales a las fisicas #
#------------------------------------------------------------------------------#
FUNCTION f_segundo_paso()

    DEFINE lr_datos RECORD
        nss         LIKE dis_provision.nss                  ,
        consec      LIKE dis_provision.consecutivo_lote
    END RECORD
    
    DEFINE
        ls_cont                 SMALLINT
        
    LET ls_cont = 0

    INSERT INTO ret_preliquida
    SELECT * FROM   tmp_preliquida_reint
    WHERE  folio = gd_folio
    
    SELECT COUNT(UNIQUE nss)
    INTO ls_cont
    FROM ret_preliquida
    WHERE  folio = gd_folio
    
    DISPLAY " REGISTROS LIQUIDADOS      : ", ls_cont AT 14,05
    LET ls_cont = 0
    
    DECLARE cur_pre CURSOR FOR
        SELECT UNIQUE nss, consecutivo_lote
        FROM   ret_preliquida
        WHERE  folio = gd_folio
        ORDER BY 1

        FOREACH cur_pre INTO lr_datos.*

            UPDATE ret_par_reintegro_des
            SET    estado = gr_edo.preliquidado
            WHERE  nss    = lr_datos.nss
            AND    consecutivo = lr_datos.consec
            AND    folio  = gd_folio
            AND    estado = gr_edo.recibido

            LET ls_cont  = ls_cont + SQLCA.SQLERRD[3]
            INITIALIZE lr_datos.* TO NULL
        END FOREACH

    DISPLAY " REGISTROS ACTUALIZADOS      : ", ls_cont AT 15,05

    CALL f_lib_error_msg("PROCESO TERMINADO CORRECTAMENTE")

END FUNCTION

#---------------------------------------------------------------------------#
# f_inserta_preliquida : Inserta los movimientos de liquidacion en la       #
#                        cuenta individual                                  #
#---------------------------------------------------------------------------#
FUNCTION f_inserta_preliquida(pr_liquida)

    DEFINE pr_liquida RECORD
        folio                   LIKE dis_cuenta.folio               ,
        nss                     LIKE dis_cuenta.nss                 ,
        curp                    LIKE dis_cuenta.curp                ,
        consecutivo             LIKE dis_cuenta.consecutivo_lote    ,
        subcuenta               LIKE dis_cuenta.subcuenta           ,
        siefore                 LIKE dis_cuenta.siefore             ,
        tipo_movimiento         LIKE dis_cuenta.tipo_movimiento     ,
        fecha_liquida           LIKE dis_cuenta.fecha_pago          ,
        precio_accion           LIKE dis_cuenta.precio_accion       ,
        monto_en_acciones       LIKE dis_cuenta.monto_en_acciones   ,
        monto_en_pesos          LIKE dis_cuenta.monto_en_pesos
    END RECORD

    DEFINE lr_dis_cuenta RECORD LIKE dis_cuenta.*

    -- -----------------------------------------------------------------------------

    LET lr_dis_cuenta.tipo_movimiento       = pr_liquida.tipo_movimiento
    LET lr_dis_cuenta.subcuenta             = pr_liquida.subcuenta
    LET lr_dis_cuenta.siefore               = pr_liquida.siefore
    LET lr_dis_cuenta.folio                 = pr_liquida.folio
    LET lr_dis_cuenta.consecutivo_lote      = pr_liquida.consecutivo
    LET lr_dis_cuenta.nss                   = pr_liquida.nss
    LET lr_dis_cuenta.curp                  = pr_liquida.curp
    LET lr_dis_cuenta.folio_sua             = NULL
    LET lr_dis_cuenta.fecha_pago            = pr_liquida.fecha_liquida
    LET lr_dis_cuenta.fecha_valor           = pr_liquida.fecha_liquida
    LET lr_dis_cuenta.fecha_conversion      = pr_liquida.fecha_liquida
    LET lr_dis_cuenta.monto_en_pesos        = pr_liquida.monto_en_pesos
    LET lr_dis_cuenta.monto_en_acciones     = pr_liquida.monto_en_acciones
    LET lr_dis_cuenta.precio_accion         = pr_liquida.precio_accion
    LET lr_dis_cuenta.dias_cotizados        = 0
    LET lr_dis_cuenta.sucursal              = NULL
    LET lr_dis_cuenta.id_aportante          = "REINTEGRO"
    LET lr_dis_cuenta.estado                = gr_edo.liquidado
    LET lr_dis_cuenta.fecha_proceso         = HOY
    LET lr_dis_cuenta.usuario               = gc_usuario
    LET lr_dis_cuenta.fecha_archivo         = HOY
    LET lr_dis_cuenta.etiqueta              = 0

    INSERT INTO tmp_preliquida_reint
    VALUES(lr_dis_cuenta.*)

END FUNCTION