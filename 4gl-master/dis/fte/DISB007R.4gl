################################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                              #
#Owner             => E.F.P.                                                   #
#Programa DISB007R => REDENCION DEL BONO ISSSTE                                #
#Fecha creacion    => 26 DE DICIEMBRE DE 2008                                  #
#By                => JAVIER GONZALEZ JERONIMO                                 #
#Fecha actualiz.   => 29 DE DICIEMBRE DE 2009                                  #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                 #
#                  => Se actualiza el movimiento de abono en la version de     #
#                  => PENSION ISSSTE. Se ajustan la provision de la redencion  #
#                  => para tomar exactamente lo que se envia en el archivo     #
#Actualizacion     => DMR 03/Ene/2011                                          #
#                  => Se estandarizo codigo de todas las afores, y al liquidar #
#                  => se coloca la fecha de liquidacion en los campos          #
#                  => fecha_pago,fecha_valor y fecha_conversion para ambas     #
#                  => subcuenta (30 y 36) solo cambia la fecha_valor en la     #
#                  => subcuenta 36 ya que se coloca la fecha redencion tecleada#
################################################################################
DATABASE safre_af

GLOBALS
   DEFINE #glo #char
      enter              CHAR(001),
      gc_cad_spl         CHAR(150)

   DEFINE
      gs_peiss           ,
      gs_coppel          ,
      gs_codigo_afore    SMALLINT

   DEFINE #glo #integer
      gi_ultimo_folio    INTEGER

   DEFINE #glo #date
      HOY                DATE

   DEFINE
      gr_datos_cap RECORD
                      folio_red          LIKE dis_provision.folio,
                      fecha_red_bono     LIKE dis_provision.fecha_conversion
                   END RECORD
 
   DEFINE
      gr_mov       RECORD
                      cargo_36           LIKE tab_movimiento.codigo,
                      abono_30           LIKE tab_movimiento.codigo
                   END RECORD

   DEFINE
      gr_subctas   RECORD
                      subcta_bono        SMALLINT,
                      subcta_ret_issste  SMALLINT
                   END RECORD

   DEFINE
      gar_precio_acc ARRAY [20] OF RECORD        #Arreglo para precios_accion
                                      estado     SMALLINT,
                                      fecha      DATE,
                                      siefore    SMALLINT,
                                      precio_dia DECIMAL(16,6)
                                   END RECORD
END GLOBALS


MAIN
   CALL STARTLOG("DISB007R.log")
    
   DEFER INTERRUPT
   OPTIONS
      INPUT WRAP,
      PROMPT LINE LAST,
      ACCEPT KEY CONTROL-I

   CALL init() #i
   CALL f_captura_datos_user() RETURNING gr_datos_cap.*
   CALL f_obtiene_precios_accion(gr_datos_cap.fecha_red_bono)

   CALL primer_paso (gr_datos_cap.*)             -- Realiza cargo a subcuenta 36

   CALL segundo_paso(gr_datos_cap.*)             -- Realiza abono a subcuenta 30

   -- Si la afore es Coppel entonces ejecuta la desmarca de cta_act_bono
   IF gs_codigo_afore = gs_coppel THEN
      CALL f_desmarca_act_bono(gi_ultimo_folio)
   END IF

   CALL tercer_paso (gr_datos_cap.fecha_red_bono)  -- Genera cifras de control
END MAIN


FUNCTION init()
#i-------------
   LET HOY  = TODAY
    
   ----- CODIGOS AFORES -----
   SELECT codigo_afore
   INTO   gs_codigo_afore
   FROM   tab_afore_local

   SELECT afore_cod
   INTO   gs_coppel
   FROM   tab_afore
   WHERE  afore_desc MATCHES "*COPPEL*"

   LET gs_peiss = 578 -- Clave PENSION ISSSTE

   ----- VARIABLES GLOBALES -----
   LET gr_subctas.subcta_ret_issste  = 30
   LET gr_subctas.subcta_bono        = 36

   IF gs_codigo_afore = gs_peiss THEN
      LET gr_mov.abono_30 = 17
   ELSE
      LET gr_mov.abono_30 = 1
   END IF
    
   LET gr_mov.cargo_36 = 27

   ----- ULTIMO FOLIO -----
   SELECT MAX(folio) + 1
   INTO   gi_ultimo_folio
   FROM   glo_folio
    
   INSERT INTO glo_folio 
   VALUES (gi_ultimo_folio)
    
   --- PROVISIONA CARGO ---
   LET gc_cad_spl = " "
   LET gc_cad_spl = "EXECUTE FUNCTION fn_prov_cargo (?,?,?,?,?,?,?,?,?,?,?) "
   PREPARE eje_prov_cargo FROM gc_cad_spl

   --- PROVISIONA ABONO ---
   LET gc_cad_spl = " "
   LET gc_cad_spl = "EXECUTE FUNCTION fn_prov_abono (?,?,?,?,?,?,?,?,?,?) "
   PREPARE eje_prov_abono FROM gc_cad_spl

   ----- LIQUIDACION -----
   LET gc_cad_spl = " "
   LET gc_cad_spl = " EXECUTE FUNCTION fn_liquida ( ?,?,?,?,? ) "
   PREPARE eje_liquida FROM gc_cad_spl
END FUNCTION


FUNCTION primer_paso(lr_datos)
   DEFINE
      li_cont      ,
      li_consec    INTEGER

   DEFINE
      lr_siefore   LIKE dis_cuenta.siefore

   DEFINE
      lr_datos     RECORD
                      folio_red  LIKE dis_provision.folio,
                      fecha_red  LIKE dis_provision.fecha_conversion
                   END RECORD

   DEFINE
      lr_saldo_dia RECORD
                      nss        LIKE dis_cuenta.nss,
                      curp       LIKE dis_cuenta.curp,
                      siefore    LIKE dis_cuenta.siefore,
                      subcta     LIKE dis_cuenta.subcuenta,
                      monto_acc  LIKE dis_cuenta.monto_en_acciones,
                      monto_pes  LIKE dis_cuenta.monto_en_pesos
                   END RECORD

   LET li_cont = 0
   DISPLAY "PROCESANDO INFORMACION ... " AT 18,2

   -- Se identifican las cuentas a las que se redimira el bono
   DECLARE cur_01 CURSOR FOR
   SELECT nss,
          consecutivo_lote,
          siefore,
          subcuenta,
          monto_en_acciones
   FROM   dis_provision
   WHERE  folio     = lr_datos.folio_red
   AND    subcuenta = gr_subctas.subcta_bono
   ORDER BY 2

   -- Iniciamos ciclo para cada nss
   FOREACH cur_01 INTO lr_saldo_dia.nss,
                       li_consec,
                       lr_saldo_dia.siefore, 
                       lr_saldo_dia.subcta,
                       lr_saldo_dia.monto_acc 

      LET li_cont                = li_cont + 1
      LET lr_siefore             = lr_saldo_dia.siefore
      LET lr_saldo_dia.monto_acc = lr_saldo_dia.monto_acc * -1

      SELECT curp,
             monto_en_pesos
      INTO   lr_saldo_dia.curp,
             lr_saldo_dia.monto_pes
      FROM   dis_provision
      WHERE  folio     = lr_datos.folio_red
      AND    nss       = lr_saldo_dia.nss
      AND    subcuenta = gr_subctas.subcta_ret_issste

      LET lr_saldo_dia.monto_pes = lr_saldo_dia.monto_pes * -1

      DISPLAY "PROCESANDO INFORMACION : REGISTRO ", li_cont  AT 18,2

      -- Provisiona la subcuenta
      CALL f_provisiona_subcta(gi_ultimo_folio, 
                               li_consec,
                               gr_mov.cargo_36,
                               lr_datos.fecha_red,
                               lr_saldo_dia.*)

      -- Afecta la cuenta individual
      CALL f_liquida_subcta(gi_ultimo_folio,
                            lr_saldo_dia.nss, 
                            lr_saldo_dia.curp, 
                            gr_subctas.subcta_bono,
                            lr_datos.fecha_red)
 
      INITIALIZE lr_saldo_dia.* TO NULL
   END FOREACH

   IF li_cont = 0 THEN
      PROMPT "NO SE ENCONTRARON REGISTROS A REDIMIR... PRESIONE <ENTER> PARA FINALIZAR"
      FOR CHAR enter
      EXIT PROGRAM 
   END IF 
END FUNCTION


FUNCTION segundo_paso(lr_datos)
   DEFINE
      lc_curp       LIKE dis_cuenta.curp,
      li_consec     LIKE dis_cuenta.consecutivo_lote,
      lr_siefore    LIKE dis_cuenta.siefore

   DEFINE
      lr_datos      RECORD
                       folio_red  LIKE dis_provision.folio,
                       fecha_red  LIKE dis_provision.fecha_conversion
                    END RECORD

   DEFINE
      lr_saldo_liq  RECORD
                       nss        LIKE dis_cuenta.nss,
                       curp       LIKE dis_cuenta.curp,        
                       siefore    LIKE dis_cuenta.siefore,
                       subcta     LIKE dis_cuenta.subcuenta,
                       monto_acc  LIKE dis_cuenta.monto_en_acciones,
                       monto_pes  LIKE dis_cuenta.monto_en_pesos
                    END RECORD

   DISPLAY "ACTUALIZANDO MONTOS EN DIS_CUENTA ... " AT 18,2
    
   -- Se toman las cuentas liquidadas en el paso anterior
   DECLARE cur_02 CURSOR FOR
   SELECT A.nss,
          A.curp,
          A.consecutivo_lote,
          A.monto_en_pesos
   FROM   dis_cuenta A
   WHERE  A.folio            = gi_ultimo_folio
   AND    A.tipo_movimiento  = gr_mov.cargo_36
   ORDER BY nss

   -- Iniciamos ciclo para cada nss
   FOREACH cur_02 INTO lr_saldo_liq.nss,
                       lr_saldo_liq.curp,
                       li_consec,
                       lr_saldo_liq.monto_pes

      LET lr_saldo_liq.siefore   = f_obtiene_siefore_act(lr_saldo_liq.nss)
      LET lr_saldo_liq.subcta    = gr_subctas.subcta_ret_issste
      LET lr_siefore             = lr_saldo_liq.siefore        
      LET lr_saldo_liq.monto_pes = lr_saldo_liq.monto_pes * -1 
      LET lr_saldo_liq.monto_acc = lr_saldo_liq.monto_pes / gar_precio_acc[lr_siefore].precio_dia

      -- Provisiona la subcuenta
      CALL f_provisiona_subcta(gi_ultimo_folio, 
                               li_consec,
                               gr_mov.abono_30, 
                               lr_datos.fecha_red,
                               lr_saldo_liq.*)

      -- Afecta la cuenta individual
      CALL f_liquida_subcta(gi_ultimo_folio,
                            lr_saldo_liq.nss, 
                            lr_saldo_liq.curp,
                            gr_subctas.subcta_ret_issste, 
                            HOY                  )
      --                    lr_datos.fecha_red   )

      -- Si la afore es pension issste, Actualiza dis_hist_apor_bono
      IF gs_codigo_afore = gs_peiss THEN
         UPDATE dis_hist_apor_bono 
         SET ind_tipo_bono_isss  = "2",
             result_operacion    = "27" -- redencion anual
         WHERE n_unico           = lr_saldo_liq.curp
      END IF

   END FOREACH

   -- Actualizamos la tabla para indicar que el folio de carga fue liquidado
   UPDATE dis_dep_bono
   SET    estado   = 3
   WHERE  folio    = lr_datos.folio_red

   CLOSE WINDOW forma_01
END FUNCTION 


FUNCTION tercer_paso(ld_fecha_reden)
   DEFINE
      ld_fecha_reden     DATE

   DEFINE
      ls_sie             SMALLINT

   DEFINE
      li_num_cta         ,
      li                 INTEGER

   DEFINE
      lr_montos_sie RECORD
                       desc_sie    LIKE tab_siefore_local.razon_social,
                       precio_acc  LIKE dis_cuenta.precio_accion,
                       monto_pes   LIKE dis_cuenta.monto_en_pesos,
                       monto_acc   LIKE dis_cuenta.monto_en_acciones
                    END RECORD
    
   -- El orden varia ya que asi se manda al screen array
   DEFINE
     lar_montos_sb ARRAY[5] OF RECORD
                                 desc_sie   LIKE tab_siefore_local.razon_social,
                                 monto_pes  LIKE dis_cuenta.monto_en_pesos,
                                 precio_acc LIKE dis_cuenta.precio_accion,
                                 monto_acc  LIKE dis_cuenta.monto_en_acciones
                               END RECORD    
        
   DEFINE
      lar_num_ctas ARRAY[5] OF INTEGER

   -- Se abre la forma para mostrar las cifras de control
   OPEN WINDOW forma_02 AT 4,4 WITH FORM "DISB007R2" ATTRIBUTE(BORDER)
   DISPLAY "                            < CTRL-C > Salir                                   " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " DISB007R           REDENCION DE BONO DE PENSION DBMX1                         " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)
    
   -- Obtenemos los montos de los abonos a las siefores basicas
   DECLARE cur_03 CURSOR FOR
   SELECT B.razon_social,
          C.precio_del_dia,
          NVL(SUM(A.monto_en_pesos),0),
          NVL(SUM(A.monto_en_acciones),0),
          C.codigo_siefore,
          COUNT(*)
   FROM   dis_cuenta        A,
          tab_siefore_local B,
          glo_valor_accion  C
   WHERE  A.folio           = gi_ultimo_folio
   AND    A.subcuenta       = gr_subctas.subcta_ret_issste
   AND    A.siefore         = B.codigo_siefore
   AND    A.siefore         = C.codigo_siefore
   AND    C.fecha_valuacion = HOY
##dmr    AND    C.fecha_valuacion = ld_fecha_reden
   GROUP BY 1,2,5
   ORDER BY 5

   LET li         = 1
   INITIALIZE lr_montos_sie.* TO NULL

   -- Iniciamos ciclo para cada nss
   FOREACH cur_03 INTO lr_montos_sie.*,
                       ls_sie,
                       li_num_cta

      LET lar_montos_sb[li].desc_sie   = lr_montos_sie.desc_sie  
      LET lar_montos_sb[li].monto_pes  = lr_montos_sie.monto_pes
      LET lar_montos_sb[li].precio_acc = lr_montos_sie.precio_acc
      LET lar_montos_sb[li].monto_acc  = lr_montos_sie.monto_acc 
      LET lar_num_ctas[ls_sie]         = li_num_cta

      LET li         = li + 1
      LET li_num_cta = 0
      INITIALIZE lr_montos_sie.* TO NULL
   END FOREACH 

   -- Obtenemos los montos del cargo a la siefore 13
   SELECT B.razon_social,
          C.precio_del_dia,
          NVL(SUM(A.monto_en_pesos),0),
          NVL(SUM(A.monto_en_acciones),0)
   INTO   lr_montos_sie.* 
   FROM   dis_cuenta        A,
          tab_siefore_local B,
          glo_valor_accion  C
   WHERE  A.folio           = gi_ultimo_folio
   AND    A.subcuenta       = gr_subctas.subcta_bono
   AND    A.siefore         = B.codigo_siefore
   AND    A.siefore         = C.codigo_siefore
   AND    C.fecha_valuacion = ld_fecha_reden
   GROUP BY 1,2

   -- Desplegamos la informacion en pantalla
   DISPLAY gi_ultimo_folio             TO folio
   DISPLAY lr_montos_sie.desc_sie      TO desc_s13      
   DISPLAY lr_montos_sie.monto_pes     TO monto_pes_s13 
   DISPLAY lr_montos_sie.precio_acc    TO precio_acc_s13
   DISPLAY lr_montos_sie.monto_acc     TO monto_acc_s13 
 
   ---
 
   DISPLAY lar_num_ctas[1]             TO  num_ctas_01
   DISPLAY lar_num_ctas[2]             TO  num_ctas_02
   DISPLAY lar_num_ctas[3]             TO  num_ctas_03
   DISPLAY lar_num_ctas[4]             TO  num_ctas_04
   DISPLAY lar_num_ctas[5]             TO  num_ctas_05
    
   --- 
   CALL SET_COUNT(li-1)
   DISPLAY ARRAY lar_montos_sb TO scr_montos.*

   UPDATE dis_cuenta
   SET   fecha_valor = ld_fecha_reden,
         fecha_pago  = HOY,
         fecha_conversion = HOY
   WHERE folio = gi_ultimo_folio
   AND subcuenta = 36

   PROMPT "PROCESO CONCLUIDO... PRESIONE <ENTER> PARA SALIR " FOR CHAR enter

   CLOSE WINDOW forma_02
END FUNCTION


FUNCTION f_desmarca_act_bono(pi_folio)
   DEFINE
      pi_folio    LIKE dis_cuenta.folio
    
   DEFINE
      lr_his_bono RECORD LIKE cta_his_bono.*
    
   DEFINE
      pc_nss      LIKE dis_cuenta.nss
   
   -- -------------------------------------------------------------------

   -- Recupera los nss que fueron liquidados con el folio dado
   DECLARE cur_liquidados CURSOR FOR 
   SELECT UNIQUE(nss)
   FROM   dis_cuenta
   WHERE  folio            = pi_folio
   AND    tipo_movimiento  = gr_mov.cargo_36
   ORDER BY 1 

   FOREACH cur_liquidados INTO pc_nss

      INITIALIZE lr_his_bono.* TO NULL

      -- Obtiene los registros de cta_act_bono y los inserta en cta_his_bono
      SELECT nss,
             curp,
             fecha_redencion,
             fecha_registro,
             udis,
             pesos,
             proceso,
             today,             -- fecha_baja
             "RED",             -- proceso baja
             "",                -- fecha_reingreso
             usuario          
      INTO   lr_his_bono.*
      FROM   cta_act_bono
      WHERE  nss = pc_nss
    
      INSERT INTO cta_his_bono
      VALUES (lr_his_bono.*)

      -- Elimina el registro de cta_act_bono
      DELETE
      FROM  cta_act_bono
      WHERE nss = pc_nss

   END FOREACH
END FUNCTION


FUNCTION f_obtiene_precios_accion(fecha_precios)
#opa------------------------------------------
   DEFINE #loc #date
      fecha_precios         DATE

   DEFINE #loc #char
      lc_mensaje            CHAR(100),
      lc_siefore            CHAR(002)

   DEFINE #loc #smallint
      li_cont               SMALLINT

   DEFINE #loc #char
      v_precios_accion      CHAR(100)

   DEFINE
      lr_precio_acc         RECORD #loc #Arreglo para los precios_accion
                               estado      SMALLINT,
                               fecha       DATE,
                               siefore     SMALLINT,
                               precio_dia  DECIMAL(16,6)
                            END RECORD

   LET li_cont = 0

   LET v_precios_accion = " EXECUTE FUNCTION fn_verifica_precio_accion (?)"
   PREPARE eje_precios_accion FROM v_precios_accion

   DECLARE c_precios CURSOR FOR eje_precios_accion
   FOREACH c_precios USING fecha_precios
                     INTO lr_precio_acc.*

      -- Se valida que existan precios de siefores basicas (1 a 5) y de sief. 13
##dmr   IF ( (lr_precio_acc.siefore <= 5 OR lr_precio_acc.siefore = 13)
      IF (( lr_precio_acc.siefore = 13) AND lr_precio_acc.estado <> 0) THEN
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


FUNCTION f_captura_datos_user()
   DEFINE
      precio_dia   DECIMAL(16,6),
      i            SMALLINT

   DEFINE
      lr_datos     RECORD
                      folio_reden       LIKE dis_provision.folio,
                      fecha_reden_bono  LIKE dis_provision.fecha_conversion
                   END RECORD

   DEFINE
      lr_folio     RECORD
                      estado            SMALLINT,
                      mensaje           CHAR(100)
                   END RECORD

   OPEN WINDOW forma_01 AT 4,4 WITH FORM "DISB007R1" ATTRIBUTE(BORDER)
   DISPLAY "                            < CTRL-C > Salir                                   " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " DISB007R           REDENCION DE BONO DE PENSION DBMX1                         " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

   LET lr_datos.fecha_reden_bono = HOY

   INPUT BY NAME lr_datos.* WITHOUT DEFAULTS

      AFTER FIELD folio_reden
         CALL f_verifica_folio(lr_datos.folio_reden)
         RETURNING lr_folio.*

         IF lr_folio.estado <> 0 THEN 
             ERROR lr_folio.mensaje
             NEXT FIELD folio_reden
         END IF

      AFTER FIELD fecha_reden_bono
         IF lr_datos.fecha_reden_bono IS NULL THEN
            ERROR " FECHA INCORRECTA ... "
            NEXT FIELD fecha_reden_bono
         END IF

      ON KEY (ESC)
         CALL f_verifica_folio(lr_datos.folio_reden)
         RETURNING lr_folio.*

         IF lr_folio.estado <> 0 THEN 
            ERROR lr_folio.mensaje
            NEXT FIELD folio_reden
         END IF

         IF lr_datos.fecha_reden_bono IS NULL THEN
            ERROR " FECHA INCORRECTA ... "
            NEXT FIELD fecha_reden_bono
         ELSE
            SELECT precio_del_dia
            INTO  precio_dia
            FROM  glo_valor_accion
            WHERE fecha_valuacion = lr_datos.fecha_reden_bono
            AND   codigo_siefore  = 13

            IF STATUS = NOTFOUND THEN
               ERROR " NO Existe Precio de la UDI del dia de Redencion del Bono"
               NEXT FIELD fecha_reden_bono
            END IF
         END IF

         FOR i=1 TO 5                       #siefores basicas
            SELECT precio_del_dia
            INTO  precio_dia
            FROM  glo_valor_accion
            WHERE fecha_valuacion = HOY
            AND   codigo_siefore  = i

            IF STATUS = NOTFOUND THEN
               ERROR " NO Existe Precio del dia de la SIE ",i USING "##"
               NEXT FIELD folio_reden
            END IF
         END FOR

         EXIT INPUT

      ON KEY (CONTROL-C)
         PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
         EXIT PROGRAM

      ON KEY (INTERRUPT)
         PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
         EXIT PROGRAM

   END INPUT
    
   WHILE TRUE
      PROMPT " DESEA GENERAR EL PROCESO ? (S/N) " FOR CHAR enter
        
      IF enter MATCHES "[sSnN]" THEN
         IF enter MATCHES "[sS]" THEN
            EXIT WHILE
         ELSE
            PROMPT " PROCESO CANCELADO...PRESIONE <ENTER> PARA SALIR "
            FOR CHAR enter
            EXIT PROGRAM
         END IF
      END IF
   END WHILE

   RETURN lr_datos.*
END FUNCTION


FUNCTION f_verifica_folio(p_folio)
   DEFINE 
      p_folio     INTEGER

   DEFINE
      lr_datos    RECORD
                     estado   SMALLINT,
                     mensaje  CHAR(100)
                  END RECORD
 
   LET lr_datos.estado = 0
    
   IF p_folio IS NULL THEN
      LET lr_datos.mensaje = "EL FOLIO NO PUEDE SER NULO" 
      LET lr_datos.estado  = 1
   ELSE
      -- Se verifica si se cargo el archivo viendo si ya hay provision
      -- para el folio dado
      SELECT "OK"
      FROM  dis_provision
      WHERE folio     = p_folio
      AND   subcuenta = gr_subctas.subcta_bono
      GROUP BY 1
        
      IF STATUS = NOTFOUND THEN
         LET lr_datos.mensaje = "NO HAY UN ARCHIVO DE BONO CARGADO PARA ESTE FOLIO"
         LET lr_datos.estado  = 1
      ELSE
         -- Verificamos si el folio ya fue redimido
         SELECT "OK"
         FROM  dis_dep_bono
         WHERE folio  = p_folio
         AND   estado = 3
         GROUP BY 1
 
         IF STATUS <> NOTFOUND THEN
            LET lr_datos.mensaje = "EL FOLIO INGRESADO YA FUE REDIMIDO"
            LET lr_datos.estado  = 1
         END IF    
      END IF
   END IF
    
   LET lr_datos.mensaje = lr_datos.mensaje CLIPPED
   RETURN lr_datos.*
END FUNCTION


#-------------------------------------------------------------------------------
# Realiza la provision de un cargo o abono dependiendo de la subcuenta dada
#-------------------------------------------------------------------------------
FUNCTION f_provisiona_subcta(p_folio, p_consecu, p_mov, p_fecha, pr_saldos)
   DEFINE
      p_mov              ,
      si_provisiono      SMALLINT

   DEFINE
      ld_acc             ,
      ld_pesos           DECIMAL(16,6)

   DEFINE
      p_fecha            DATE

   DEFINE #loc #char
      folio_sua          CHAR(06),
      id_aporte          CHAR(15)

   DEFINE
      p_folio     LIKE glo_folio.folio

   DEFINE
      p_consecu   LIKE ret_solicitud_tx.consecutivo

   DEFINE
      pr_saldos   RECORD
                     nss        LIKE dis_cuenta.nss,
                     curp       LIKE dis_cuenta.curp,
                     siefore    LIKE dis_cuenta.siefore,
                     subcta     LIKE dis_cuenta.subcuenta,
                     monto_acc  LIKE dis_cuenta.monto_en_acciones,
                     monto_pes  LIKE dis_cuenta.monto_en_pesos
                  END RECORD

   LET si_provisiono = 0
   LET folio_sua     = ""
   LET id_aporte     = "REDEN BONO"

   -- Si el importe es negativo, se provisiona como cargo
   IF pr_saldos.monto_acc < 0 AND pr_saldos.monto_pes < 0 THEN
      DECLARE cur_prov_cargo CURSOR FOR eje_prov_cargo
      OPEN cur_prov_cargo USING p_folio               ,--folio
                                folio_sua             ,--folio_sua
                                pr_saldos.nss         ,--nss
                                pr_saldos.subcta      ,--subcuenta
                                p_mov                 ,--tipo_movimiento
                                p_consecu             ,--consecutivo_lote
                                pr_saldos.siefore     ,--siefore
                                pr_saldos.monto_acc   ,--monto_en_acciones
                                pr_saldos.monto_pes   ,--monto_en_pesos
                                id_aporte             ,--id_aportante
                                p_fecha                --fecha proceso
        
         FETCH cur_prov_cargo INTO si_provisiono
    
      CLOSE cur_prov_cargo
   ELSE
      DECLARE cur_prov_abono CURSOR FOR eje_prov_abono
      OPEN cur_prov_abono USING p_folio               ,--folio
                                folio_sua             ,--folio_sua
                                pr_saldos.nss         ,--nss
                                pr_saldos.subcta      ,--subcuenta
                                p_mov                 ,--tipo_movimiento
                                p_consecu             ,--consecutivo_lote
                                pr_saldos.monto_acc   ,--monto_en_acciones
                                pr_saldos.monto_pes   ,--monto_en_pesos
                                id_aporte             ,--id_aportante
                                p_fecha                --fecha proceso
        
         FETCH cur_prov_abono INTO si_provisiono    

      CLOSE cur_prov_abono
   END IF 

   IF si_provisiono < 0 THEN
      PROMPT "El NSS :",pr_saldos.nss CLIPPED," NO PROVISIONO LA SUBCTA ", pr_saldos.subcta FOR CHAR enter
   ELSE
      UPDATE dis_provision
      SET    curp             = pr_saldos.curp
      WHERE  folio            = p_folio
      AND    nss              = pr_saldos.nss
      AND    consecutivo_lote = p_consecu
      AND    tipo_movimiento  = p_mov
   END IF
END FUNCTION


#-------------------------------------------------------------------------------
# Realiza la liquidacion de un cargo o abono dependiendo de la subcuenta dada
#-------------------------------------------------------------------------------
FUNCTION f_liquida_subcta(p_folio, p_nss, p_curp, p_subcta, p_fliquida)
   DEFINE
      p_fliquida    DATE
 
   DEFINE
      p_subcta      ,
      ls_edo_liq    SMALLINT

   DEFINE
      p_folio       LIKE glo_folio.folio,
      p_nss         LIKE dis_cuenta.nss,
      p_curp        LIKE dis_cuenta.curp

   LET ls_edo_liq = 0

   DECLARE cur_liquida CURSOR FOR eje_liquida
   OPEN cur_liquida USING p_folio    , -- folio
                          p_nss      , -- nss
                          p_subcta   , -- subcuenta
                          p_fliquida , -- fecha_liquida (fecha redencion bono)
                          HOY          -- fecha_proceso (fecha ejecuta proceso)

      FETCH cur_liquida INTO ls_edo_liq

      IF ls_edo_liq < 0 THEN
         PROMPT "El NSS :",p_nss CLIPPED," NO LIQUIDO LA SUBCTA ", p_subcta FOR CHAR enter
      ELSE
         UPDATE dis_cuenta
         SET   curp             = p_curp
         WHERE folio            = p_folio
         AND   nss              = p_nss
         AND   subcuenta        = p_subcta
         AND   fecha_conversion = p_fliquida
      END IF

   CLOSE cur_liquida
END FUNCTION


#-------------------------------------------------------------------------------
# Determina a que siefore corresponde la subcuenta 30 para un NSS dado
#-------------------------------------------------------------------------------
FUNCTION f_obtiene_siefore_act(p_nss)
   DEFINE
      p_nss        LIKE cta_nss_regimen.nss

   DEFINE
      ls_sie_act   LIKE cta_nss_regimen.codigo_siefore

   SELECT codigo_siefore
   INTO   ls_sie_act
   FROM   cta_nss_regimen
   WHERE  grupo_regimen = 10
   AND    nss           = p_nss

   RETURN ls_sie_act
END FUNCTION

