----------------------------------------------------
--Programa: CTALT03 
--Descripcion: Previo Traspaso Siefore id x edad
--Fecha:  01 sep 2010
--Autor:  Jesus Yanez Moreno
----------------------------------------------------

DATABASE safre_af

GLOBALS
   DEFINE g_arr_siefore       ARRAY[5] OF  RECORD
          razon_social        CHAR(15)  ,
          precio_del_dia      DEC(16,6)
   END RECORD
 
   DEFINE l_resul      smallint

    DEFINE    fecha_valuacion        DATE

    DEFINE    bnd_precio             SMALLINT

    DEFINE    g_ruta_log1            CHAR(200)

    DEFINE    g_ruta_envio           CHAR(040)
    
    DEFINE    g_reg_tes_tipo_id      RECORD LIKE tes_tipo_id_aportante.*

    DEFINE    reg_tes_ctr_folio      RECORD  LIKE safre_af:tes_ctr_folio.*

    DEFINE    reg_tes_solicitud      RECORD  LIKE safre_af:tes_solicitud.*

    DEFINE    g_seg_modulo           RECORD  LIKE seg_modulo.*

    DEFINE    g_today                           DATE

    DEFINE    g_siefore                         ,
              g_tipo_mov                        ,
              g_scta                            ,
              g_edo_recibido                    ,
              g_edo_procedente                  ,
              g_provisionada                    ,
              uno                               ,  
              cero                              ,  
              v_subcuenta                       ,
              v_grupo                           , 
              g_regimen_subcuenta               ,
              g_codigo_afore                    SMALLINT

    DEFINE    g_registros                       ,
              g_verifica_prov                   ,
              g_regs_proc_cargo                 ,
              g_regs_proc_abono                 INTEGER

    DEFINE    g_monto                           DEC(18,6),
              g_ins_pesos                       DEC(16,6),
              g_ins_acciones                    DEC(16,6),
              g_pesos                           DEC(16,6),
              verif_sdo                         DEC(16,6)

    DEFINE    g_cta_transfe_56_anios            CHAR(001),  #pendiente
              g_enter                           CHAR(001),
              g_hora                            CHAR(005), 
              g_cat                             CHAR(300),
              g_fecha_nula                      CHAR(008),
              g_comando                         CHAR(100),
              g_lista                           CHAR(100),
              g_afore_cod                       CHAR(003),
              g_raz_social                      CHAR(050),
              g_usuario                         CHAR(008) 

    DEFINE    g_sie_inf       ARRAY[20]    OF    RECORD   --se agranda arreglo
     	      nom_siefore                       CHAR(008),--para modificacion 
              precio_accion                     DEC(11,6) --de multisiefore
    END   RECORD

END GLOBALS

GLOBALS "CTALT03S.4gl"  #  Definicion  de  querys

MAIN
    DEFINE command   CHAR(30)
    OPTIONS INPUT WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I

    LET fecha_valuacion = ARG_VAL(1)

    DISPLAY "CTALT03.",fecha_valuacion using"ddmmyyyy",
            ": INICIA PREVIO LIQ TRASPASO SIEFORE X EDAD"

    UPDATE cta_ctrl_tes_ied
    SET    accion3 = "OK"
    WHERE  tipo_proceso = 1

    CALL inicio()
    CALL prepara_querys_TCAAB005()
    CALL F_010_inicio()

    DISPLAY "CTALT03.",fecha_valuacion using"ddmmyyyy",
            ": TERMINA PREVIO LIQ TRASPASO SIEFORE X EDAD"

    UPDATE cta_ctrl_tes_ied
    SET    estado3 = "OK"
    WHERE  tipo_proceso = 1

    LET command = "fglgo CTALT05.4gi ",fecha_valuacion             
    RUN command

    DISPLAY "=================================="
    DISPLAY "* FIN PROCESO PREVIOTRANSFERENCIA *"
    DISPLAY "=================================="

END MAIN
 
FUNCTION inicio()
  
    SELECT  ruta_envio
      INTO  g_ruta_envio
      FROM  seg_modulo
     WHERE  modulo_cod = 'cta' 

     CALL ini_precio_sie() RETURNING l_resul

END FUNCTION

FUNCTION  prepara_querys_TCAAB005()

   CALL     define_querys_TCAAB005()

   PREPARE  sql_01    FROM  g_sql_01
   PREPARE  sql_02    FROM  g_sql_02
   PREPARE  sql_03    FROM  g_sql_03
   PREPARE  sql_04    FROM  g_sql_04
   PREPARE  sql_05    FROM  g_sql_05
   PREPARE  sql_06    FROM  g_sql_06
   PREPARE  sql_07    FROM  g_sql_07
   PREPARE  sql_08    FROM  g_sql_08
   PREPARE  sql_10    FROM  g_sql_10
   PREPARE  sql_12    FROM  g_sql_12
   PREPARE  sql_13    FROM  g_sql_13
   PREPARE  sql_14    FROM  g_sql_14
   PREPARE  sql_15    FROM  g_sql_15
   PREPARE  sql_31    FROM  g_sql_31
   PREPARE  sql_32    FROM  g_sql_32
   PREPARE  sql_42    FROM  g_sql_42  
   PREPARE  sql_42_3  FROM  g_sql_42_3
   PREPARE  sql_43    FROM  g_sql_43  
   PREPARE  sql_44    FROM  g_sql_44  
   PREPARE  sql_53    FROM  g_sql_53
   PREPARE  sql_54    FROM  g_sql_54
   PREPARE  sql_55    FROM  g_sql_55
   PREPARE  sql_56    FROM  g_sql_56
   PREPARE  sql_57    FROM  g_sql_57

END FUNCTION

FUNCTION  F_010_inicio()

   CALL    STARTLOG("CTALT03.log")

   LET     cero                   =  0
   LET     uno                    =  1
   LET     g_today                =  fecha_valuacion
   LET     g_hora                 =  TIME
   LET     g_regs_proc_cargo      =  cero
   LET     g_regs_proc_abono      =  cero
   
   LET g_edo_procedente = 101
   LET g_provisionada   = 102
   LET g_edo_recibido   = 100

   SELECT a.codigo_afore,a.razon_social,user 
   INTO   g_afore_cod,g_raz_social,g_usuario
   FROM tab_afore_local  a

   CALL     F_020_crea_folio_pendiente()
   CALL     F_030_trae_parametros()

END FUNCTION

FUNCTION  F_020_crea_folio_pendiente()

    DISPLAY  "CTALT03.",g_today using"ddmmyyyy",
             ": TRASPASO ENTRE SIEFORES X EDAD" 
   SELECT "OK"
   FROM   tab_feriado a
   WHERE  a.feria_fecha = g_today

   IF STATUS <> NOTFOUND THEN
      DISPLAY "==========================================="
      DISPLAY "EJECUCION CANCELADA DIA FERIADO            "
      DISPLAY "==========================================="
      EXIT PROGRAM
   END IF

   IF (WEEKDAY(g_today) = 6 OR
       WEEKDAY(g_today) = 0) THEN
      DISPLAY "==========================================="
      DISPLAY "EJECUCION CANCELADA DIA INHABIL            "
      DISPLAY "==========================================="
      EXIT PROGRAM
   END IF

   LET reg_tes_ctr_folio.fecha_presentacion = g_today
   LET reg_tes_ctr_folio.fecha_liquidacion  = g_today
   LET reg_tes_ctr_folio.fecha_provision    = 
       reg_tes_ctr_folio.fecha_liquidacion

   CALL F_022_precio_acc_parti()

    DECLARE cur_56 CURSOR FOR sql_56 
    FOREACH cur_56 INTO   g_reg_tes_tipo_id.*

    LET g_registros = 0
 
        EXECUTE  sql_06
        USING    g_edo_recibido,
                 g_reg_tes_tipo_id.tipo_traspaso
        INTO     g_registros
 
        IF  (g_registros  = 0 OR g_registros IS NULL) THEN 
           DISPLAY "CTALT03: SIN REGISTROS PARA ",
                   g_reg_tes_tipo_id.descripcion CLIPPED
            CONTINUE FOREACH
        END IF

        EXECUTE sql_01 INTO reg_tes_ctr_folio.folio   # obtiene folio
        EXECUTE sql_02 USING reg_tes_ctr_folio.folio  # inserta en glo_folio

        LET reg_tes_ctr_folio.fecha_presentacion = g_today

        --CALL cal_fecha_avant(g_today,1)                     # para anticipar 
        --RETURNING reg_tes_ctr_folio.fecha_liquidacion       # 1 dia antes

        LET reg_tes_ctr_folio.fecha_liquidacion = g_today

        LET reg_tes_ctr_folio.fecha_provision    = 
        reg_tes_ctr_folio.fecha_liquidacion

        LET reg_tes_ctr_folio.estado          = g_edo_procedente
        LET reg_tes_ctr_folio.usuario         = g_usuario
        LET reg_tes_ctr_folio.tipo_traspaso   = g_reg_tes_tipo_id.tipo_traspaso

        EXECUTE sql_31 USING reg_tes_ctr_folio.*

--- no actualiza tes_solicitud en previo

    --    EXECUTE sql_32 USING reg_tes_ctr_folio.folio,
                             --g_edo_procedente       ,
                             --g_edo_recibido         ,
                             --g_reg_tes_tipo_id.tipo_traspaso

        EXECUTE  sql_53 USING reg_tes_ctr_folio.folio 
	                INTO     g_verifica_prov
    
        IF (g_verifica_prov = 0 OR 
            g_verifica_prov IS NULL) THEN
        ELSE 
            DISPLAY "CTALT03: ABORTANDO FOLIO YA SE ENCUENTRA PROVISIONADO ..."
	    EXIT PROGRAM
        END IF

     END FOREACH

END FUNCTION

FUNCTION  F_022_precio_acc_parti()

   DEFINE  l_tot_sie                         SMALLINT ,
           l_nom_siefore                     CHAR(008),
           l_precio_accion                   DEC(11,6)

   SELECT COUNT(*) 
   INTO   l_tot_sie 
   FROM   tab_siefore_local a
   WHERE  a.codigo_siefore not in (0,11) 

   FOR     g_siefore     =  1  TO  l_tot_sie
           LET     g_sie_inf[g_siefore].nom_siefore      =  NULL
           LET     g_sie_inf[g_siefore].precio_accion    =  cero
   END FOR

   DECLARE  cur_sie     CURSOR    FOR   sql_07
   OPEN     cur_sie     USING     reg_tes_ctr_folio.fecha_provision,
                                  g_afore_cod
   FOREACH  cur_sie     INTO      g_siefore,l_nom_siefore,l_precio_accion
       LET     g_sie_inf[g_siefore].nom_siefore      =  l_nom_siefore  CLIPPED
       LET     g_sie_inf[g_siefore].precio_accion    =  l_precio_accion
   END FOREACH

   LET g_siefore  = 1
   LET bnd_precio = 0

   FOR g_siefore = 1 TO l_tot_sie

        IF g_sie_inf[g_siefore].precio_accion = cero THEN
           LET bnd_precio = 1
        END IF

        DISPLAY "TESB001: PRECIO DE ACCION  DE SIEFORE ",
                g_sie_inf[g_siefore].nom_siefore CLIPPED," : ",
                g_sie_inf[g_siefore].precio_accion USING "###,###.&&&&&&"

   END FOR

   IF bnd_precio = 1 THEN
      LET bnd_precio = 0
     -- DISPLAY "ABORTANDO PROCESO...PRECIOS INCONSISTENTES"
     -- EXIT  PROGRAM
   END IF

END FUNCTION

FUNCTION  F_030_trae_parametros()

    LET      g_fecha_nula               = "00010101"

    EXECUTE  sql_10
    INTO     g_seg_modulo.*

    DECLARE cur_57 CURSOR FOR sql_57
    FOREACH cur_57 USING  g_edo_procedente
                   INTO   reg_tes_ctr_folio.*

       EXECUTE sql_08 USING reg_tes_ctr_folio.tipo_traspaso
                      INTO  g_reg_tes_tipo_id.*

       LET g_registros = 0

       EXECUTE sql_06 USING g_edo_recibido,
                            g_reg_tes_tipo_id.tipo_traspaso
                      INTO  g_registros

        DISPLAY "CTALT03: TIPO TRANSFERENCIA     : ", 
          g_reg_tes_tipo_id.descripcion CLIPPED
        DISPLAY ""
        DISPLAY "CTALT03: INICIA PREVIO FOLIO : ", 
          reg_tes_ctr_folio.folio USING "######"
        DISPLAY "CTALT03: FECHA DE RECEPCION     : ", 
          reg_tes_ctr_folio.fecha_presentacion USING "DD-MM-YYYY" 
        DISPLAY "CTALT03: FECHA DE PREVIO        : ", 
          reg_tes_ctr_folio.fecha_provision    USING "DD-MM-YYYY" 
        DISPLAY "CTALT03: FECHA DE LIQUIDACION   : ", 
          reg_tes_ctr_folio.fecha_liquidacion  USING "DD-MM-YYYY" 
        DISPLAY "CTALT03: REGS. A PROCESAR       : ",
          g_registros  USING  "######"
        DISPLAY ""

        CALL    F_100_proceso()

    END FOREACH

END FUNCTION

FUNCTION  F_100_proceso()

    DEFINE l_desc_corta  CHAR(003)

    DISPLAY "CTALT03: PROCESANDO FOLIO " ,
          reg_tes_ctr_folio.folio USING "######"

    DECLARE cur_nss_ced CURSOR FOR sql_12

-- se modifica por el previo

    OPEN    cur_nss_ced USING --reg_tes_ctr_folio.folio,
                              g_edo_recibido

    FOREACH cur_nss_ced INTO reg_tes_solicitud.*

    SELECT a.desc_corta
    INTO   l_desc_corta 
    FROM   tab_grupo_regimen a
    WHERE  a.grupo_regimen = reg_tes_solicitud.grupo_regimen

    LET   g_reg_tes_tipo_id.id_aportante  = g_reg_tes_tipo_id.id_aportante CLIPPED,
                                                              l_desc_corta CLIPPED


        CASE g_reg_tes_tipo_id.ind_folio_origen
        WHEN 1  
          CALL      F_133_arma_saldo_rcv_3()
          EXIT CASE
        OTHERWISE 
          CALL      F_133_arma_saldo_rcv()
          EXIT CASE
        END CASE
    END FOREACH
        CALL      F_250_updates()
        --CALL      F_300_liquida_cargo()
    

END FUNCTION

FUNCTION  F_133_arma_saldo_rcv()

    DEFINE   v_subcuenta           ,
             v_ramo                ,
             v_grupo               SMALLINT

    LET      v_subcuenta              =  cero
    LET      v_grupo                  =  cero
    LET      v_ramo                   =  cero

   --DECLARE  c_saldo_rcv  CURSOR  FOR  sql_42

   DECLARE  c_saldo_rcv  CURSOR  FOR  

   SELECT a.subcuenta,a.siefore ,
          a.monto_en_acciones,
          a.monto_en_pesos
   FROM   safre_tmp:tmp_saldo_edad a 
   WHERE  a.nss = reg_tes_solicitud.nss

   FOREACH  c_saldo_rcv  INTO    g_scta      ,
                                 g_siefore   ,
                                 g_monto     ,
                                 g_pesos

    SELECT "OK" 
    FROM   tab_agrupa_subcta_regimen a
    WHERE  a.grupo_regimen = reg_tes_solicitud.grupo_regimen 
    AND    a.subcuenta     = g_scta 
    GROUP BY 1
    
    IF STATUS = NOTFOUND THEN 
       CONTINUE FOREACH
    END IF 

    IF g_monto <= 0  THEN 
       CONTINUE FOREACH
    END IF

    EXECUTE sql_55 USING reg_tes_solicitud.nss ,
                         reg_tes_solicitud.grupo_regimen
                   INTO  g_regimen_subcuenta

    IF g_regimen_subcuenta = g_siefore THEN 
       CONTINUE FOREACH
    END IF 

    LET g_pesos        = g_monto * g_arr_siefore[g_siefore].precio_del_dia
    LET g_ins_acciones = g_monto
    LET g_ins_pesos    = g_pesos

    CALL F_140_inserta_dis_provision()

   END FOREACH 

END FUNCTION

FUNCTION  F_133_arma_saldo_rcv_3()


   DECLARE  c_saldo_rcv_3  CURSOR  FOR  sql_42_3

   FOREACH  c_saldo_rcv_3  USING  reg_tes_solicitud.folio_origen     ,
                                  reg_tes_solicitud.nss              ,
                                  reg_tes_solicitud.fecha_solicitud  
                          INTO    g_scta      ,
                                  g_siefore   ,
                                  g_monto     ,
                                  g_pesos

    SELECT "OK" 
    FROM   tab_agrupa_subcta_regimen a
    WHERE  a.grupo_regimen = reg_tes_solicitud.grupo_regimen 
    AND    a.subcuenta     = g_scta 
    GROUP BY 1

    IF STATUS = NOTFOUND THEN 
       CONTINUE FOREACH
    END IF 

    IF g_monto <= 0  THEN 
       CONTINUE FOREACH
    END IF

    EXECUTE sql_55 USING reg_tes_solicitud.nss ,
                         reg_tes_solicitud.grupo_regimen
                   INTO  g_regimen_subcuenta

    IF g_regimen_subcuenta = g_siefore THEN 
       CONTINUE FOREACH
    END IF

        SELECT sum(a.monto_en_acciones)
        INTO   verif_sdo
        FROM   dis_cuenta a
        WHERE  a.nss       = reg_tes_solicitud.nss
        AND    a.subcuenta = g_scta
        AND    a.siefore   = g_siefore

        IF verif_sdo < g_monto THEN 
           CONTINUE FOREACH 
        END IF

        LET g_pesos = g_monto * g_sie_inf[g_siefore].precio_accion

        LET g_ins_acciones = g_monto
        LET g_ins_pesos    = g_pesos

        CALL      F_140_inserta_dis_provision()

   END FOREACH 

END FUNCTION

FUNCTION  F_140_inserta_dis_provision()
#idp-----------------------------------
    DEFINE   l_precio_dia_siguiente    LIKE glo_valor_accion.precio_del_dia
    DEFINE   verifica_provision        SMALLINT
    DEFINE   v_folio_sua               SMALLINT

    LET      v_folio_sua               = " " 

    DECLARE  spl_provisiona      CURSOR  FOR  sql_43
    DECLARE  spl_provisiona_abo  CURSOR  FOR  sql_44

    LET      g_ins_acciones            =  -  g_ins_acciones
    LET      g_ins_pesos               =  -  g_ins_pesos
    LET      g_tipo_mov                =  255 #traspaso entre siefores cargo
   
    FOREACH  spl_provisiona        USING
                  reg_tes_ctr_folio.folio           ,      #folio
                  v_folio_sua                       ,      #folio_sua
                  reg_tes_solicitud.nss             ,      #nss
                  g_scta                            ,      #subcuenta
                  g_tipo_mov                        ,      #tipo_movimiento 255
	          reg_tes_solicitud.folio_solicitud ,      #consecutivo
                  g_siefore                         ,      #siefore
                  g_ins_acciones                    ,      #monto_en_acciones  
                  g_ins_pesos                       ,      #monto_en_pesos
                  g_reg_tes_tipo_id.id_aportante    ,      #id_aportante
                  g_today                           ,      #fecha_proceso
                  g_arr_siefore[g_siefore].precio_del_dia
              INTO     verifica_provision

              LET g_regs_proc_cargo = g_regs_proc_cargo + 1
    END FOREACH

    LET l_precio_dia_siguiente = 
         g_arr_siefore[g_siefore].precio_del_dia

    LET      g_ins_pesos               =  - g_ins_acciones * 
                                          l_precio_dia_siguiente
    LET      g_ins_acciones            =  0
    LET      g_tipo_mov                =  55 #traspaso entre siefores abono

    FOREACH  spl_provisiona_abo        USING
                  reg_tes_ctr_folio.folio           ,      #folio
                  v_folio_sua                       ,      #folio_sua
                  reg_tes_solicitud.nss             ,      #nss
                  g_scta                            ,      #subcuenta
                  g_tipo_mov                        ,      #tipo_movimiento 1
	          reg_tes_solicitud.folio_solicitud ,      #consecutivo
                  g_ins_acciones                    ,      #monto_en_acciones  
                  g_ins_pesos                       ,      #monto_en_pesos
                  g_reg_tes_tipo_id.id_aportante    ,      #id_aportante
                  g_today                           ,      #fecha_proceso
         g_arr_siefore[g_regimen_subcuenta].precio_del_dia
              INTO     verifica_provision

              LET g_regs_proc_abono = g_regs_proc_abono + 1
    END FOREACH

END FUNCTION

FUNCTION  F_250_updates()
    --EXECUTE  sql_14
    --   USING  g_provisionada,reg_tes_ctr_folio.folio,g_edo_procedente
    EXECUTE  sql_15
       USING  g_provisionada,reg_tes_ctr_folio.folio,g_edo_procedente
    DISPLAY "CTALT03: PROVISION  FOLIO ",
         reg_tes_ctr_folio.folio USING "######" ," CONCLUIDA "
END FUNCTION 

FUNCTION F_300_liquida_cargo()

DISPLAY "TESB001: TOTAL REGISTROS PROVISION CARGO : ",
        g_regs_proc_cargo USING"#########"

DISPLAY "TESB001: TOTAL REGISTROS PROVISION ABONO : ",
        g_regs_proc_abono USING"#########"

DISPLAY "TESB001: FIN PROVISION FOLIO",
        reg_tes_ctr_folio.folio USING"######"

DISPLAY ""

DISPLAY "TESB001: LANZANDO LIQUIDACION FOLIO",
        reg_tes_ctr_folio.folio USING"######"

DISPLAY ""
      
LET g_comando = "fglgo ",g_seg_modulo.ruta_exp CLIPPED, "/TESB002 ",
                reg_tes_ctr_folio.folio

RUN g_comando

END FUNCTION

FUNCTION cal_fecha_avant(x_fecha,ciclo)
#cf-------------------------------

    DEFINE cc         SMALLINT
    DEFINE x_fecha    DATE
    DEFINE ciclo      SMALLINT

    DEFINE sig_fecha    DATE
    DEFINE dia_semana SMALLINT
    DEFINE ant_habil  SMALLINT

    LET sig_fecha  = x_fecha
    LET cc = 1

    WHILE cc <= ciclo 

        LET sig_fecha  = sig_fecha + 1

        LET dia_semana = WEEKDAY(sig_fecha)
        IF dia_semana = 0 OR dia_semana = 6 THEN
           CONTINUE WHILE
        ELSE  
           SELECT "ok" 
           FROM   tab_feriado
           WHERE  feria_fecha = sig_fecha
       
           IF STATUS <> NOTFOUND THEN
              CONTINUE WHILE
           ELSE 
	      LET cc = cc + 1
           END IF	
        END IF
    END WHILE
    RETURN sig_fecha
END FUNCTION

FUNCTION ini_precio_sie()
#-------------------------

--- Se llena el arreglo con los precios
---------------------------------------

  DEFINE codigo_siefore  smallint  ,
         razon_social    char(15)  ,
         precio_del_dia  dec(16,6)

  DEFINE i, l_resul       smallint

  LET l_resul = 0

  FOR i = 1 TO 5
     INITIALIZE g_arr_siefore[i].* TO NULL
  END FOR

  LET i = 0

  DECLARE cur_1 CURSOR FOR 

  SELECT b.codigo_siefore ,
         a.razon_social, 
         b.precio_del_dia
  FROM   tab_siefore_local a, 
         glo_valor_accion  b
  WHERE  a.codigo_siefore  = b.codigo_siefore
  AND    b.codigo_siefore in (1,2,3,4,5)
  and    b.fecha_valuacion = fecha_valuacion
  ORDER BY 1

  FOREACH cur_1 INTO codigo_siefore ,
                     razon_social   ,
                     precio_del_dia

       LET g_arr_siefore[codigo_siefore].razon_social = 
           razon_social

       LET g_arr_siefore[codigo_siefore].precio_del_dia = 
           precio_del_dia

  END FOREACH

  FOR i = 1 TO 5 

   IF g_arr_siefore[i].precio_del_dia is null THEN
      LET l_resul = 1
      EXIT FOR
   END IF
  END FOR

  RETURN l_resul

END FUNCTION
