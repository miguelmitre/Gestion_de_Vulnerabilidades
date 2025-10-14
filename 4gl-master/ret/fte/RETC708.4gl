################################################################################
#Owner             => E.F.P.                                                   #
#Programa RETC708  => PRELIQUIDACION DE RECURSOS A DESINVERTIR                 #
#Fecha creacion    => 16 DE OCTUBRE DE 2009                                    #
#Fecha Nvo Retiro  => Octubre 2011                                             #
#By                => JAVIER GONZALEZ JERONIMO                                 #
#Fecha actualiz.   => Nov 2011                                                 #
#Actualizacion     => JGHM                                                     #
#Sistema           => Retiros referencia punto 2.5                             #
################################################################################
DATABASE safre_af

GLOBALS
    DEFINE gr_edo RECORD
        enviado     LIKE ret_estado_issste.estado_solicitud ,   
        recibido    LIKE ret_estado_issste.estado_solicitud ,
        pre_liq     LIKE ret_estado_issste.estado_solicitud
    END RECORD

    DEFINE gr_folios RECORD
        fec_prel    DATE                         ,
        total       LIKE ret_sol_issste_tx.folio ,
        parcial     LIKE ret_sol_issste_tx.folio ,
        transfer    LIKE ret_sol_issste_tx.folio
    END RECORD


    DEFINE gar_precio_acc ARRAY [20] OF RECORD #Arreglo para los precios_accion
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE #glo #date
        HOY                   DATE

    DEFINE  enter                 CHAR(001)
    DEFINE  gs_usuario            CHAR(015)
    DEFINE  gc_query              CHAR(1800)

    DEFINE #glo #smallint
        gs_peiss              ,
        gs_flag               ,
        gs_flag_err           ,
        gs_sieviv             ,
        gs_num_siefores       , #-- Indica el numero de siefores que se usan actualmente
        gs_cod_afore          SMALLINT

    DEFINE  gi_proceso            INTEGER
    DEFINE  gs_ult_folio          INTEGER

    DEFINE   gar_tot    ARRAY[20]     OF RECORD
        sie                           LIKE dis_cuenta.siefore           ,
        des_sie                       CHAR(17)                          ,
        des_tip                       CHAR(38)                          ,
        des_scta_1                    CHAR(30)                          ,
        des_scta_2                    CHAR(30)                          ,
        des_scta_4                    CHAR(30)                          ,
        des_scta_5                    CHAR(30)                          ,
        des_scta_6                    CHAR(30)                          ,
        des_scta_9                    CHAR(30)                          ,
        des_scta_s                    CHAR(30)                          ,
        accion_1                      LIKE dis_cuenta.monto_en_acciones ,
        pesos_1                       LIKE dis_cuenta.monto_en_pesos    ,
        accion_2                      LIKE dis_cuenta.monto_en_acciones ,
        pesos_2                       LIKE dis_cuenta.monto_en_pesos    ,
        accion_4                      LIKE dis_cuenta.monto_en_acciones ,
        pesos_4                       LIKE dis_cuenta.monto_en_pesos    ,
        accion_5                      LIKE dis_cuenta.monto_en_acciones ,
        pesos_5                       LIKE dis_cuenta.monto_en_pesos    ,
        accion_6                      LIKE dis_cuenta.monto_en_acciones ,
        pesos_6                       LIKE dis_cuenta.monto_en_pesos    ,
        accion_9                      LIKE dis_cuenta.monto_en_acciones ,
        pesos_9                       LIKE dis_cuenta.monto_en_pesos    ,
        accion_s                      LIKE dis_cuenta.monto_en_acciones ,
        pesos_s                       LIKE dis_cuenta.monto_en_pesos    
    END RECORD
    DEFINE   gs_trate                 INTEGER
END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("RETC708.log")

    CALL init() #i

    OPEN WINDOW main_win AT 4,4 WITH 19 ROWS, 75 COLUMNS ATTRIBUTE(BORDER)
    DISPLAY " RETC708    PRELIQUIDACION DE RECURSOS A DESINVERTIR          " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    MENU "PRELIQUIDACION"
        
        COMMAND "Preliquida" "Ejecuta la preliquidacion de recursos a Desinvertir"
            RUN "fglgo RETC717.4go"
            {
            CALL f_despliega_info() RETURNING gs_flag, gr_folios.*
            
            IF gs_flag THEN
                CALL f_obtiene_precios_accion(gr_folios.fec_prel)            
                CALL f_abre_ventana()
            
                CALL primer_paso(gr_folios.*)   #-- Realiza la preliquidacion de los montos
                
                CALL segundo_paso(gr_folios.*)  #-- Valida la informacion de los montos
                    RETURNING gs_flag_err, gi_proceso
            
                IF gs_flag_err = 0 THEN
                    CALL tercer_paso(gr_folios.*)   #-- Vacia la informacion hacia las tablas fisicas
                ELSE
                    DISPLAY "                                             " AT 18,1
                    PROMPT " SE ENCONTRARON INCONSISTENCIAS EN EL PROCESO ... <ENTER> PARA MOSTRAR" FOR CHAR enter
                    CLOSE WINDOW RETC7082
                    CALL f_bitacora_err(gi_proceso) #-- Muestra la pantalla de errores
                END IF
            END IF
            }
        COMMAND "Detalle X Siefore" "Consulta de Pre Liquidación de Recursos a Desinvertir "
            CALL f_genera_detalle(HOY)
            CLEAR SCREEN

        COMMAND "Bitacora" "Consulta la Bitacora de Errores de Preliquidacion"
            CALL f_bitacora_err(0)
            CLEAR SCREEN


        COMMAND "Salir" "Salir del Programa "
            EXIT MENU
    END MENU
    
    CLOSE WINDOW main_win 

END MAIN


FUNCTION init()
#i-------------
    DEFINE  lc_prepare           CHAR(300)
    DEFINE  ls_dias_hab          SMALLINT

    LET HOY                 =  TODAY
    LET gs_sieviv           =  12
    LET ls_dias_hab         =  2

    SELECT COUNT(*)
    INTO   gs_num_siefores
    FROM   tab_siefore_local
    WHERE  codigo_siefore > 0
    AND    codigo_siefore NOT IN (11,12,13)

    SELECT codigo_afore,
           USER
    INTO   gs_cod_afore,
           gs_usuario
    FROM   tab_afore_local

    SELECT A.estado_solicitud
    INTO   gr_edo.enviado
    FROM   ret_estado A
    WHERE  A.descripcion = "ENVIADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.recibido
    FROM   ret_estado A
    WHERE  A.descripcion = "RECIBIDO"

    SELECT A.estado_solicitud
    INTO   gr_edo.pre_liq
    FROM   ret_estado A
    WHERE  A.descripcion = "PRELIQUIDADO SALDO"

    ----- SALDO AL DIA -----
    LET lc_prepare = "EXECUTE FUNCTION fn_saldo_dia_isss (?,?,?,?) "
    PREPARE eje_saldo_dia FROM lc_prepare

    LET lc_prepare = " "

    ----- RETENCION DE ISR -----
    LET lc_prepare = "EXECUTE FUNCTION fn_ret_isr_issste (?,?,?,?) "
    PREPARE eje_ret_isr FROM lc_prepare

    LET  gc_query  =   'SELECT  razon_social       ',
                       '  FROM  tab_siefore_local  ',
                       ' WHERE  codigo_siefore = ? '
    PREPARE  p_sel_sie   FROM gc_query  

    LET  gc_query  =   'SELECT  subct_desc         ',
                       '  FROM  tab_subcuenta      ',
                       ' WHERE  subct_cod      = ? '
    PREPARE  p_sel_sub   FROM gc_query

    LET  gc_query  =  ' SELECT  Pr1.siefore, Pr1.subcuenta, sum(Pr1.monto_en_acciones), SUM(Pr1.monto_en_pesos) ',
                      '   FROM  ret_preliquida      Pr1,                                                        ',
                      '         ret_solicitud_saldo Pr2                                                         ',
                      '  WHERE  Pr1.fecha_conversion            =  ?                                            ',
                      '    AND  Pr2.folio_cargo                 =  ?                                            ',
                      '    AND  Pr2.estado_solicitud            =  '||gr_edo.pre_liq,
                      '    AND  Pr2.folio_cargo                 =  Pr1.folio                                    ',
                      '    AND  Pr2.nss                         =  Pr1.nss                                      ',
                      '    AND  Pr2.id_solicitud_saldo          =  Pr1.consecutivo_lote                         ',
                      '    AND  Pr1.tipo_movimiento             =  921                                          ',
                      '  GROUP  BY  1, 2                                                                        ',
                      '  ORDER  BY  1, 2                                                                        '
    PREPARE  p_sel_car    FROM      gc_query
    DECLARE  d_sel_car  CURSOR FOR  p_sel_car

    LET  gc_query  =  'SELECT  (SELECT  UNIQUE Pr1.siefore                                          ',
                      '           FROM  ret_preliquida Pr1,                                         ',
                      '                 ret_folio      Fo1                                          ',
                      '          WHERE  Pr1.fecha_conversion  =  Pr2.fecha_conversion               ',
                      '            AND  Pr1.nss               =  Pr2.nss                            ',
                      '            AND  Pr1.consecutivo_lote  =  Pr2.consecutivo_lote               ',
                      '            AND  Pr1.subcuenta         =  Pr2.subcuenta                      ',
                      '            AND  Pr1.folio             =  Fo1.folio_cargo                    ',
                      '            AND  Fo1.folio_abono       =  Pr2.folio                          ',
                      '            AND  Pr1.tipo_movimiento   =  921),                              ',
                      '        Pr2.subcuenta,                                                       ',
                      '        SUM(Pr2.monto_en_acciones),                                          ',
                      '        SUM(Pr2.monto_en_pesos)                                              ',
                      '   FROM  ret_preliquida      Pr2,                                                        ',
                      '         ret_solicitud_saldo Pr3                                                         ',
                      '  WHERE  Pr2.folio                       =  ?                                            ',
                      '    AND  Pr3.estado_solicitud            =  '||gr_edo.pre_liq,
                      '    AND  Pr3.folio_cargo                 =  ?                                            ',
                      '    AND  Pr3.nss                         =  Pr2.nss                                      ',
                      '    AND  Pr3.id_solicitud_saldo          =  Pr2.consecutivo_lote                         ',
                      '    AND  Pr2.tipo_movimiento             =  922                                          ',
                      ' GROUP  BY 1, 2                                                              ',
                      ' ORDER  BY 1, 2                                                              '

    PREPARE  p_sel_abo3   FROM      gc_query
    DECLARE  d_sel_abo3 CURSOR FOR  p_sel_abo3

   LET   gc_query  =  ' SELECT  '||'''x'''||', folio_cargo, COUNT(*)  ',
                      '   FROM  ret_solicitud_saldo                   ', 
                      '  WHERE  YEAR(f_recep_procesar)   =  ?         ',
                      '    AND  MONTH(f_recep_procesar)  =  ?         ',
                      '    AND  DAY(f_recep_procesar)    =  ?         ',
                      '    AND  estado_solicitud  =            '||gr_edo.pre_liq,
                      '  GROUP BY 1, 2                         ' 
   PREPARE  p_sel_fecha        FROM  gc_query 
   DECLARE  d_sel_fecha  CURSOR FOR  p_sel_fecha 

   LET   gc_query  =  ' SELECT  folio_abono        ',
                      '   FROM  ret_folio          ',
                      '  WHERE  folio_cargo   =  ? '
   PREPARE  p_sel_fol          FROM  gc_query    

   LET   gc_query  =  ' SELECT  UNIQUE Pr2.fecha_conversion                ',
                      '   FROM  ret_preliquida      Pr2,                   ',
                      '         ret_solicitud_saldo Pr3                    ',
                      '  WHERE  folio                           =   ?      ',
                      '    AND  Pr3.nss                         =  Pr2.nss                          ',
                      '    AND  Pr3.id_solicitud_saldo          =  Pr2.consecutivo_lote             ',
                      '    AND  Pr3.estado_solicitud            =      '||gr_edo.pre_liq
   PREPARE  p_sel_fec          FROM  gc_query

   LET      gs_trate   = 0 
END FUNCTION


--- FUNCTION f_despliega_info()
--- 
---     DEFINE lr_info RECORD
---         fecha_preliq        DATE    ,
---         sel_tot             CHAR    ,
---         sel_par             CHAR    ,
---         sel_tran            CHAR    ,
---         folio_tot           INTEGER ,
---         folio_par           INTEGER ,
---         folio_tran          INTEGER
---     END RECORD
--- 
---     DEFINE
---         li_tot_prov        INTEGER
--- 
---     DEFINE
---         ls_ret              ,
---         ls_cont             ,
---         ls_flag             SMALLINT
--- 
---     OPEN WINDOW RETC7081 AT 4,4 WITH FORM "RETC7081" ATTRIBUTE (BORDER)
---     DISPLAY " <CTRL-C> SALIR                                             <ESC> EJECUTAR    " AT 1,1 ATTRIBUTE(REVERSE)
---     DISPLAY " RETC7081      PRELIQUIDACION DE SOLICITUDES DE RETIROS                       " AT 3,1 ATTRIBUTE(REVERSE)
---     DISPLAY HOY USING"DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)
--- 
---     LET ls_flag              = 1
---     LET lr_info.fecha_preliq = HOY
--- 
---     -- DISPOSICIONES
---     SELECT UNIQUE(folio),
---            "x"
---     INTO   lr_info.folio_tot,
---            lr_info.sel_tot
---     FROM   ret_sol_issste_tx
---     WHERE  estado_solicitud = gr_edo.recibido
--- 
---     IF STATUS = NOTFOUND THEN
---         LET lr_info.folio_tot = 0
---         LET lr_info.sel_tot   = " "
---     END IF
--- 
---     -- PARCIALES  (en desarrollo)
---     SELECT UNIQUE(folio),
---            "x"
---     INTO   lr_info.folio_par,
---            lr_info.sel_par
---     FROM   ret_parcial_issste
---     WHERE  estado_solicitud = gr_edo.recibido
--- 
---     IF STATUS = NOTFOUND THEN
---         LET lr_info.folio_par = 0
---         LET lr_info.sel_par   = " "
---     END IF
--- 
---     -- TRANSFERENCIAS
---     SELECT UNIQUE(folio),
---            "x"
---     INTO   lr_info.folio_tran,
---            lr_info.sel_tran
---     FROM   ret_trans_issste
---     WHERE  estado_solicitud = gr_edo.enviado
--- 
---     IF STATUS = NOTFOUND THEN
---         LET lr_info.folio_tran  = 0
---         LET lr_info.sel_tran    = " "
---     END IF
--- 
---     INPUT BY NAME lr_info.* WITHOUT DEFAULTS
--- 
---         BEFORE INPUT
---             IF (lr_info.sel_tot <> "x") AND (lr_info.sel_par <> "x") AND (lr_info.sel_tran <> "x") THEN
---                 PROMPT " NO EXISTEN REGISTROS PARA LIQUIDAR ... < ENTER > PARA SALIR " FOR CHAR enter
---                 LET ls_flag = 0
---                 EXIT INPUT
---             END IF
--- 
---         AFTER FIELD fecha_preliq
---             IF lr_info.fecha_preliq IS NULL THEN
---                 ERROR " LA FECHA DE PRELIQUIDACION NO PUEDE SER NULA ..."
---                 SLEEP 2
---                 ERROR " "
---                 NEXT FIELD fecha_preliq
---             END IF
--- 
---         ON KEY (CONTROL-C)
---             PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
---             LET ls_flag = 0
---             EXIT INPUT
--- 
---         ON KEY (INTERRUPT)
---             PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
---             LET ls_flag = 0
---             EXIT INPUT
--- 
---         ON KEY (ESC)
--- 
---             IF lr_info.sel_tot IS NULL THEN
---                 LET lr_info.sel_tot = " "
---             END IF
--- 
---             IF lr_info.sel_par IS NULL THEN
---                 LET lr_info.sel_par = " "
---             END IF
--- 
---             IF lr_info.sel_tran IS NULL THEN
---                 LET lr_info.sel_tran = " "
---             END IF
--- 
---             IF lr_info.sel_tot <> "x" THEN
---                 LET lr_info.folio_tot = 0
---             END IF
--- 
---             IF lr_info.sel_par <> "x" THEN
---                 LET lr_info.folio_par = 0
---             END IF            
--- 
---             IF lr_info.sel_tran <> "x" THEN
---                 LET lr_info.folio_tran = 0
---             END IF            
---             
---             IF (lr_info.sel_tot <> "x") AND (lr_info.sel_par <> "x") AND (lr_info.sel_tran <> "x") THEN
---                 PROMPT " NO EXISTEN REGISTROS PARA LIQUIDAR ... < ENTER > PARA SALIR " FOR CHAR enter
---                 LET ls_flag = 0
---                 EXIT INPUT
---             END IF
--- 
---             IF lr_info.fecha_preliq IS NULL THEN
---                 ERROR " LA FECHA DE PRELIQUIDACION NO PUEDE SER NULA ..."
---                 SLEEP 2
---                 ERROR " "
---                 NEXT FIELD fecha_preliq
---             END IF
--- 
---             WHILE TRUE
---                 PROMPT " ESTA SEGURO S/N ? " FOR CHAR enter
---                 IF enter MATCHES "[sSnN]" THEN
---                     IF enter MATCHES "[sS]" THEN
---                         LET ls_flag = 1
---                         EXIT INPUT
---                     ELSE
---                         PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
---                         LET ls_flag = 0
---                         EXIT INPUT
---                     END IF
---                 END IF
---             END WHILE
--- 
---     END INPUT
---     
---     CLOSE WINDOW RETC7081
--- 
---     RETURN ls_flag              ,
---            lr_info.fecha_preliq ,
---            lr_info.folio_tot    ,
---            lr_info.folio_par    ,
---            lr_info.folio_tran
--- 
--- END FUNCTION



---FUNCTION primer_paso(pr_folios)
---
---    DEFINE pr_folios RECORD
---        fec_prel    DATE                         ,
---        total       LIKE ret_sol_issste_tx.folio ,
---        parcial     LIKE ret_sol_issste_tx.folio ,
---        transfer    LIKE ret_sol_issste_tx.folio
---    END RECORD
---
---    CALL f_tablas_tmp()
---
---    IF pr_folios.total <> 0 THEN
---        CALL f_preliquida_tot(pr_folios.total, pr_folios.fec_prel)      #-- Preliquidacion de retiros totales
---    END IF
---    
---    IF pr_folios.transfer <> 0 THEN
---        CALL f_preliquida_trans(pr_folios.transfer, pr_folios.fec_prel)   #-- Preliquidacion de transferencias
---    END IF
---
---{
---    IF pr_folios.parcial <> 0 THEN
---        CALL f_preliquida_par(pr_folios.parcial, pr_folios.fec_prel)   #-- Preliquidacion de retiros parciales
---    END IF
---}    
---
---END FUNCTION


--- FUNCTION segundo_paso(pr_folios)
--- 
---     DEFINE pr_folios RECORD
---         fec_prel    DATE                         ,
---         total       LIKE ret_sol_issste_tx.folio ,
---         parcial     LIKE ret_sol_issste_tx.folio ,
---         transfer    LIKE ret_sol_issste_tx.folio
---     END RECORD
--- 
---     DEFINE li_idproc LIKE ret_bitacora_error.id_proceso
---     
---     DEFINE
---         ls_flag         SMALLINT
--- 
---     LET ls_flag = 0    
--- 
---     DISPLAY "                                             " AT 18,1
---     DISPLAY " VALIDANDO INFORMACION ... " AT 18,1 ATTRIBUTE(REVERSE)
---     SLEEP 1    
--- 
---     LET li_idproc = f_ultimo_id_err()
---     
---     ----------  Valida sobregiro de los retiros Totales     --------------
---     CALL f_verifica_sobregiro(ls_flag, li_idproc, pr_folios.total, pr_folios.fec_prel, "tot")
---         RETURNING ls_flag
--- 
---     ----------  Valida sobregiro de las transferencias      --------------
---     CALL f_verifica_sobregiro(ls_flag, li_idproc, pr_folios.transfer, pr_folios.fec_prel, "tra")
---         RETURNING ls_flag
--- 
--- {
---     ----------  Valida sobregiro de los retiros parciales   --------------
---     CALL f_verifica_sobregiro(ls_flag, li_idproc, pr_folios.parcial, pr_folios.fec_prel, "par")
---         RETURNING ls_flag
--- }
--- 
---     
---     
---     RETURN ls_flag, li_idproc
---     
--- 
--- END FUNCTION

---FUNCTION tercer_paso(pr_folios)
---
---    DEFINE pr_folios RECORD
---        fec_prel    DATE                         ,
---        total       LIKE ret_sol_issste_tx.folio ,
---        parcial     LIKE ret_sol_issste_tx.folio ,
---        transfer    LIKE ret_sol_issste_tx.folio
---    END RECORD
---
---    DISPLAY "                                             " AT 18,1
---    --- PRELIQUIDACION DE TOTALES
---    
---    IF pr_folios.total <> 0 THEN 
---        INSERT INTO safre_tmp:tmp_ret_preliquida
---        SELECT *
---        FROM   tmp_preliquida
---        WHERE  folio = pr_folios.total
---        
---        CALL f_act_estado_sol(pr_folios.total, "tot")
---    END IF
---
---    IF pr_folios.transfer <> 0 THEN
---        INSERT INTO safre_tmp:tmp_ret_preliquida
---        SELECT *
---        FROM   tmp_preliquida
---        WHERE  folio = pr_folios.transfer
---        
---        CALL f_act_estado_sol(pr_folios.transfer, "tra")
---    END IF
---    
---{    
---    IF pr_folios.parcial <> 0 THEN
---        INSERT INTO safre_tmp:tmp_ret_preliquida
---        SELECT *
---        FROM   tmp_preliquida
---        WHERE  folio = pr_folios.parcial
---        
---        CALL f_act_estado_sol(pr_folios.parcial, "par")
---    END IF
---}
---
---
---    PROMPT " PROCESO TERMINADO SATISFACTORIAMENTE ... <ENTER> PARA SALIR " FOR CHAR enter
---    CLOSE WINDOW RETC7082
---END FUNCTION



--- FUNCTION f_preliquida_tot(pi_folio, pdt_fecha_pre)
---     DEFINE pi_folio LIKE ret_sol_issste_tx.folio
---     DEFINE lr_provi,
---            lr_isr   RECORD LIKE dis_cuenta.*
---     DEFINE lr_reten RECORD
---         mto_neto    LIKE dis_provision.monto_en_pesos,
---         mto_isr     LIKE dis_provision.monto_en_pesos
---     END RECORD
---     DEFINE ls_diag_procesar LIKE ret_sol_issste_tx.diag_procesar
---     DEFINE ls_diag_viv      LIKE ret_monto_viv_issste.estado_sub_viv
---     DEFINE #loc #date
---         pdt_fecha_pre           ,
---         ld_fecha_saldo          DATE
---     DEFINE #loc #smallint
---         ls_flag                 ,
---         ls_tipo_mov             ,
---         ls_siefore              , #-- contador para los ciclos for
---         ls_subcta               SMALLINT
---     
---     DISPLAY "PRELIQUIDANDO RETIROS TOTALES ..." AT 5,5
---     DECLARE cur_prov CURSOR FOR
---     SELECT B.*             ,
---            A.diag_procesar ,
---            C.estado_sub_viv
---     FROM   ret_sol_issste_tx A  ,
---            dis_provision B      ,
---            ret_monto_viv_issste C
---     WHERE A.folio            = pi_folio
---     AND   A.folio            = B.folio
---     AND   A.curp             = B.curp
---     AND   A.consecutivo      = B.consecutivo_lote
---     AND   A.estado_solicitud = gr_edo.recibido
---     AND   B.folio            = C.folio
---     AND   B.curp             = C.curp
---     AND   B.consecutivo_lote = C.consecutivo
---     ORDER BY B.curp, B.subcuenta
--- 
---     -- Iniciamos ciclo para cada nss
---     FOREACH cur_prov INTO lr_provi.*, ls_diag_procesar, ls_diag_viv
--- 
---         LET ls_flag                     = 0
---         LET lr_provi.fecha_pago         = pdt_fecha_pre
---         LET lr_provi.fecha_conversion   = pdt_fecha_pre
---         LET lr_provi.fecha_archivo      = pdt_fecha_pre
---         LET lr_provi.fecha_proceso      = pdt_fecha_pre
---         LET lr_provi.usuario            = gs_usuario
--- 
---         -- Preliquidamos subcuentas de vivienda
---         IF (lr_provi.subcuenta = 14) OR (lr_provi.subcuenta = 35) THEN
--- 
---             IF ls_diag_procesar = 400 AND ls_diag_viv = 1 THEN
--- 
---                 LET ls_flag = 1
--- 
---                 IF lr_provi.fecha_valor <> pdt_fecha_pre THEN
---                     LET ls_siefore              = lr_provi.siefore
---                     LET lr_provi.precio_accion  = gar_precio_acc[ls_siefore].precio_dia
---                     LET lr_provi.monto_en_pesos = lr_provi.monto_en_acciones * lr_provi.precio_accion
---                     LET lr_provi.fecha_valor    = pdt_fecha_pre
---                 END IF
---             END IF
---         ELSE
---             -- Preliquidamos el resto de las subcuentas
---             LET ls_flag                 = 1
---             LET ls_siefore              = lr_provi.siefore
---             
---             IF ls_siefore <> 0 THEN
---                 LET lr_provi.precio_accion  = gar_precio_acc[ls_siefore].precio_dia
---                 LET lr_provi.monto_en_pesos = lr_provi.monto_en_acciones * lr_provi.precio_accion
---             ELSE
---                 LET lr_provi.precio_accion  = 0
---             END IF 
---             
---             LET lr_provi.fecha_valor    = pdt_fecha_pre
--- 
---             IF gs_cod_afore <> gs_peiss THEN
---                 -- Se calcula la retencion de ISR
---                 IF lr_provi.subcuenta = 13 THEN
---                     
---                     LET lr_isr.* = lr_provi.*
---                     
---                     LET lr_isr.monto_en_pesos = lr_isr.monto_en_pesos * -1
---                     
---                     EXECUTE eje_ret_isr USING lr_provi.nss              ,
---                                               lr_provi.consecutivo_lote ,
---                                               lr_isr.monto_en_pesos     ,
---                                               lr_isr.monto_en_pesos
---                                         INTO  lr_reten.*
---                 
---                     IF lr_reten.mto_isr > 0 THEN  
---                         
---                         -- si encontro una retencion, introducimos el monto neto en la temporal
---                         -- de preliquidacion y generamos un registro para la retencion
---                         
---                         LET lr_isr.monto_en_pesos    = lr_reten.mto_isr * -1
---                         LET lr_isr.monto_en_acciones = lr_isr.monto_en_pesos / lr_isr.precio_accion
---                         LET lr_isr.tipo_movimiento   = 10
---                 
---                         INSERT INTO tmp_preliquida
---                         VALUES (lr_isr.*)
---                 
---                         LET lr_provi.monto_en_pesos    = lr_reten.mto_neto * -1
---                         LET lr_provi.monto_en_acciones = lr_provi.monto_en_pesos / lr_provi.precio_accion
---                     
---                     END IF -- mto isr > 0
---                 END IF -- sub 13
---             END IF -- Afore diferente a peisss
---             
---         END IF
--- 
---         IF ls_flag THEN
---             INSERT INTO tmp_preliquida
---             VALUES (lr_provi.*)
---         END IF
--- 
---     END FOREACH -- Siguiente registro
--- 
--- END FUNCTION


--- FUNCTION f_preliquida_trans(pi_folio, pdt_fecha_pre)
--- 
---     DEFINE pi_folio LIKE ret_trans_issste.folio
--- 
---     DEFINE lr_provi,
---            lr_isr   RECORD LIKE dis_cuenta.*
--- 
---     DEFINE lr_reten RECORD
---         mto_neto    LIKE dis_provision.monto_en_pesos,
---         mto_isr     LIKE dis_provision.monto_en_pesos
---     END RECORD
---     
---     DEFINE ls_diag_procesar     LIKE ret_trans_issste.diag_procesar
---     DEFINE ls_diag_viv          LIKE ret_monto_viv_issste.estado_sub_viv
---     DEFINE ld_monto_recibido    LIKE ret_trans_issste.mto_solic_issste
---     DEFINE ld_monto_calculado   LIKE ret_trans_issste.mto_const_calculado
--- 
--- 
---     DEFINE #loc #date
---         pdt_fecha_pre           ,
---         ld_fecha_saldo          DATE
--- 
---     DEFINE #loc #smallint
---         ls_flag                 ,
---         ls_tipo_mov             ,
---         ls_siefore              , #-- contador para los ciclos for
---         ls_subcta               SMALLINT
---     
---     DISPLAY "PRELIQUIDANDO TRANSFERENCIAS  ..." AT 11,5
--- 
---     DECLARE cur_trans CURSOR FOR
---     SELECT B.*                   ,
---            A.diag_procesar       ,
---            A.mto_solic_issste    ,
---            A.mto_const_calculado ,
---            C.estado_sub_viv
---     FROM   ret_trans_issste A  ,
---            dis_provision B      ,
---            ret_monto_viv_issste C
---     WHERE A.folio            = pi_folio
---     AND   A.folio            = B.folio
---     AND   A.curp             = B.curp
---     AND   A.consecutivo      = B.consecutivo_lote
---     AND   A.estado_solicitud = gr_edo.enviado
---     AND   B.folio            = C.folio
---     AND   B.curp             = C.curp
---     AND   B.consecutivo_lote = C.consecutivo
---     ORDER BY B.curp, B.subcuenta
--- 
---     -- Iniciamos ciclo para cada nss
---     FOREACH cur_trans INTO lr_provi.*           ,   
---                            ls_diag_procesar     , 
---                            ld_monto_recibido    , 
---                            ld_monto_calculado   ,
---                            ls_diag_viv
--- 
---         LET ls_flag                     = 0
---         LET lr_provi.fecha_pago         = pdt_fecha_pre
---         LET lr_provi.fecha_conversion   = pdt_fecha_pre
---         LET lr_provi.fecha_archivo      = pdt_fecha_pre
---         LET lr_provi.fecha_proceso      = pdt_fecha_pre
---         LET lr_provi.usuario            = gs_usuario
--- 
---         -- Preliquidamos subcuentas de vivienda
---         IF (lr_provi.subcuenta = 14) OR (lr_provi.subcuenta = 35) THEN
---             LET ls_flag = 1
---             
---             IF lr_provi.fecha_valor <> pdt_fecha_pre THEN
---                 LET ls_siefore              = lr_provi.siefore
---                 LET lr_provi.precio_accion  = gar_precio_acc[ls_siefore].precio_dia
---                 LET lr_provi.monto_en_pesos = lr_provi.monto_en_acciones * lr_provi.precio_accion
---                 LET lr_provi.fecha_valor    = pdt_fecha_pre
---             END IF
---         ELSE
---             -- Preliquidamos el resto de las subcuentas
---             LET ls_flag                 = 1
---             LET ls_siefore              = lr_provi.siefore
---             LET lr_provi.precio_accion  = gar_precio_acc[ls_siefore].precio_dia
---             LET lr_provi.fecha_valor    = pdt_fecha_pre
--- 
---             IF ld_monto_recibido <= ld_monto_calculado THEN
---                 -- Si pagamos el monto constitutivo nos aseguramos de no rebasar dicho monto en pesos
---                 LET lr_provi.monto_en_acciones = lr_provi.monto_en_pesos/lr_provi.precio_accion
---             ELSE
---                 -- Si pagamos el saldo de la cuenta nos aseguramos de pagar lo provisionado en acciones                
---                 LET lr_provi.monto_en_pesos = lr_provi.monto_en_acciones * lr_provi.precio_accion
---             END IF
---         END IF
--- 
---         IF ls_flag THEN
---             INSERT INTO tmp_preliquida
---             VALUES (lr_provi.*)
---         END IF
--- 
---     END FOREACH -- Siguiente registro
--- 
--- END FUNCTION


---FUNCTION f_preliquida_par(pi_folio, pdt_fecha_pre)
---
---    DEFINE pi_folio LIKE ret_sol_issste_tx.folio
---
---    DEFINE
---        pdt_fecha_pre           DATE
---
---END FUNCTION


---FUNCTION f_abre_ventana()
---
---    OPEN WINDOW RETC7082 AT 4,4 WITH FORM "RETC7082" ATTRIBUTE(BORDER)
---    DISPLAY "                                                             RETIROS        " AT 1,1 ATTRIBUTE(REVERSE)
---    DISPLAY " RETC7082     PRELIQUIDACION DE SOLICITUDES DE RETIROS                      " AT 3,1 ATTRIBUTE(REVERSE)
---    DISPLAY HOY USING"DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)
---
---END FUNCTION

---FUNCTION f_tablas_tmp()
---
---    WHENEVER ERROR CONTINUE
---        DROP TABLE tmp_preliquida
---    WHENEVER ERROR STOP
---
---    SELECT *
---    FROM   dis_cuenta
---    WHERE  0 = 1
---    INTO TEMP tmp_preliquida 
---
---END FUNCTION


---FUNCTION f_verifica_sobregiro(pr_valida)
---
---    DEFINE pr_valida RECORD
---        bandera         SMALLINT                    ,
---        id_proc         SMALLINT                    ,
---        folio           LIKE ret_sol_issste_tx.folio,
---        fec_preliq      DATE                        ,
---        tipo            CHAR(3)
---    END RECORD
---    
---    DEFINE lr_error RECORD
---        id_proceso   LIKE ret_bitacora_error.id_proceso     ,
---        nss          LIKE ret_bitacora_error.nss            ,
---        curp         LIKE ret_bitacora_error.curp           ,
---        folio        LIKE ret_bitacora_error.folio          ,
---        tipo_campo   LIKE ret_bitacora_error.tipo_campo     ,
---        nom_campo    LIKE ret_bitacora_error.nom_campo      ,
---        valor_campo  LIKE ret_bitacora_error.valor_campo    ,
---        id_error     LIKE ret_bitacora_error.id_error
---    END RECORD
---    
---    DEFINE 
---        lr_saldo_prov, lr_saldo_dia RECORD
---            nss             LIKE ret_sol_issste_tx.nss           ,
---            subcta          SMALLINT                             ,
---            monto_acc       LIKE dis_provision.monto_en_acciones ,
---            monto_pes       LIKE dis_provision.monto_en_acciones
---        END RECORD
---
---    DEFINE ld_curp  LIKE dis_provision.curp
---    
---    DEFINE
---        ls_error            ,
---        ls_grupo            SMALLINT
---
---    CASE pr_valida.tipo
---        WHEN "tot"
---            LET lr_error.tipo_campo   = "RETIRO TOTAL    "
---           
---        WHEN "par"
---            LET lr_error.tipo_campo   = "RETIRO PARCIAL  "
---
---        WHEN "tra"
---            LET lr_error.tipo_campo   = "TRANSFERENCIAS  "
---    END CASE
---    
---    -- Inicializamos las variables de la bitacora de errores
---    LET ls_error                = pr_valida.bandera
---    LET lr_error.id_proceso     = pr_valida.id_proc
---    LET lr_error.folio          = pr_valida.folio
---    
---    LET lr_error.id_error       = 10
---    LET ls_grupo                = 0
---    LET lr_saldo_dia.monto_acc  = 0
---
---    DECLARE cur_tmp CURSOR FOR
---    SELECT curp              ,
---           nss               ,
---           subcuenta         ,
---           monto_en_acciones ,
---           monto_en_pesos
---    FROM   tmp_preliquida
---    WHERE  folio    = pr_valida.folio
---    ORDER BY 1,3
---    
---    FOREACH cur_tmp INTO ld_curp, lr_saldo_prov.*
---    
---
---        LET lr_error.nss          = lr_saldo_prov.nss
---        LET lr_error.curp         = ld_curp
---
---
---    
---        DECLARE cur_val_sobre CURSOR FOR eje_saldo_dia
---        
---        OPEN cur_val_sobre USING lr_saldo_prov.nss      ,
---                                 lr_saldo_prov.subcta   ,
---                                 ls_grupo               ,
---                                 pr_valida.fec_preliq
---        
---        FETCH cur_val_sobre INTO lr_saldo_dia.*
---        
---        CLOSE cur_val_sobre
---
---        -- Si el monto de provisionado es mayor al saldo al dia entonces registra
---        -- un sobregiro
---        
---        LET lr_saldo_prov.monto_acc = lr_saldo_prov.monto_acc * -1
---        
---        IF f_redondea_val(lr_saldo_prov.monto_acc,2) > f_redondea_val(lr_saldo_dia.monto_acc,2) THEN
---
---            LET lr_error.nom_campo    = "Sobregiro Subcta ", lr_saldo_prov.subcta
---            LET lr_error.valor_campo  = "Prov = ", lr_saldo_prov.monto_acc USING "<<<<<<<<<.<<<<<<", 
---                                        " Saldo = ",  lr_saldo_dia.monto_acc USING "<<<<<<<<<.<<<<<<"
---            LET ls_error              = 1
---            CALL f_inserta_bitacora(lr_error.*)
---        END IF
---   
---    END FOREACH
---
---    RETURN ls_error
---
---END FUNCTION



---FUNCTION f_act_estado_sol(pr_folio, pc_id_oper)
---
---
---    DEFINE pr_folio LIKE ret_sol_issste_tx.folio
---
---    DEFINE
---        pc_id_oper      CHAR(3)
---
---    DEFINE la_rets ARRAY[6] OF INTEGER
---
---    DEFINE lr_datos RECORD
---        curp        LIKE dis_provision.curp             ,
---        tipo_mov    LIKE dis_provision.tipo_movimiento  ,
---        consec      LIKE dis_provision.consecutivo_lote
---    END RECORD
---
---    DEFINE
---        ls_cont     SMALLINT
---
---    FOR ls_cont = 1 TO 6
---        LET la_rets[ls_cont] = 0
---    END FOR
---
---    DECLARE cur_pre CURSOR FOR
---    SELECT UNIQUE(curp)    ,
---           tipo_movimiento ,
---           consecutivo_lote
---    FROM   safre_tmp:tmp_ret_preliquida
---    WHERE  folio = pr_folio
---    ORDER BY 1
---
---    -- Actualiza el estado de la solicitud de los registros preliquidados
---    CASE pc_id_oper
---
---        WHEN "tot"
---            DISPLAY "FOLIO    : ", pr_folio AT 6,12
---            
---            FOREACH cur_pre INTO lr_datos.*
---                UPDATE ret_sol_issste_tx
---                SET    estado_solicitud = gr_edo.pre_liq
---                WHERE  curp             = lr_datos.curp
---                AND    consecutivo      = lr_datos.consec
---                AND    estado_solicitud = gr_edo.recibido
---
---                CASE lr_datos.tipo_mov
---                    WHEN 851
---                        LET la_rets[1] = la_rets[1] + 1
---                    WHEN 852
---                        LET la_rets[2] = la_rets[2] + 1
---                    WHEN 853
---                        LET la_rets[3] = la_rets[3] + 1
---                    WHEN 854
---                        LET la_rets[4] = la_rets[4] + 1
---                    WHEN 855
---                        LET la_rets[5] = la_rets[5] + 1
---{
---                    WHEN 858
---                        LET la_rets[6] = la_rets[6] + 1
---}
---                END CASE
---
---                DISPLAY "RETIRO A : ", la_rets[1] AT 7,12
---                DISPLAY "RETIRO B : ", la_rets[2] AT 8,12
---                DISPLAY "RETIRO C : ", la_rets[3] AT 9,12
---                DISPLAY "RETIRO D : ", la_rets[4] AT 7,45
---                DISPLAY "RETIRO E : ", la_rets[5] AT 8,45
-----                DISPLAY "RETIRO K : ", la_rets[6] AT 9,45
---                
---            END FOREACH
---
---            DISPLAY "(TERMINADO)" AT 5,39
---{    
---        WHEN "par"
---            DISPLAY ".."
---}    
---    
---        WHEN "tra"
---            DISPLAY "FOLIO    : ", pr_folio AT 12,12
---            
---            FOREACH cur_pre INTO lr_datos.*
---                UPDATE ret_trans_issste
---                SET    estado_solicitud = gr_edo.pre_liq
---                WHERE  curp             = lr_datos.curp
---                AND    consecutivo      = lr_datos.consec
---                AND    estado_solicitud = gr_edo.enviado
---
---                CASE lr_datos.tipo_mov
---                    WHEN 861
---                        LET la_rets[1] = la_rets[1] + 1
---                    WHEN 862
---                        LET la_rets[2] = la_rets[2] + 1
---                    WHEN 863
---                        LET la_rets[3] = la_rets[3] + 1
---                    WHEN 864 -- Retiro K Pendiente!!!
---                        LET la_rets[4] = la_rets[4] + 1
---                    WHEN 865
---                        LET la_rets[5] = la_rets[5] + 1
---                END CASE
---
---                DISPLAY "RETIRO H : ", la_rets[1] AT 13,12
---                DISPLAY "RETIRO I : ", la_rets[2] AT 14,12
---                DISPLAY "RETIRO J : ", la_rets[3] AT 15,12
---                DISPLAY "RETIRO K : ", la_rets[4] AT 13,45
---                DISPLAY "RETIRO L : ", la_rets[5] AT 14,45
---               
---            END FOREACH
---
---            DISPLAY "(TERMINADO)" AT 11,39
---    END CASE
---
---END FUNCTION


--- FUNCTION f_obtiene_precios_accion(fecha_precios)
--- #opa------------------------------------------
---     DEFINE #loc #date
---         fecha_precios         DATE
--- 
---     DEFINE #loc #char
---         v_precios_accion      CHAR(100)
--- 
---     DEFINE lr_precio_acc RECORD #loc #lr_precio_acc Arreglo para los precios_accion
---         estado                SMALLINT     ,
---         fecha                 DATE         ,
---         siefore               SMALLINT     ,
---         precio_dia            DECIMAL(16,6)
---     END RECORD
--- 
---     DEFINE #loc #char
---         lc_mensaje            CHAR(100) ,
---         lc_siefore            CHAR(002)
--- 
---     DEFINE #loc #smallint
---         ls_sie                SMALLINT
--- 
---     LET ls_sie = 1
--- 
---     LET v_precios_accion = " EXECUTE FUNCTION fn_verifica_precio_accion_isss(?)"
---     PREPARE eje_precios_accion FROM v_precios_accion
--- 
---     DECLARE c_precios CURSOR FOR eje_precios_accion
---     FOREACH c_precios USING fecha_precios
---                       INTO lr_precio_acc.*
--- 
---             IF lr_precio_acc.estado <> 0 THEN
---                 LET lc_siefore = lr_precio_acc.siefore
--- 
---                 LET lc_mensaje = " FALTAN PRECIOS DE ACCION: FECHA ", lr_precio_acc.fecha USING "DD/MM/yyyy",
---                                  " -- SIEFORE ", lc_siefore CLIPPED
--- 
---                 PROMPT lc_mensaje FOR CHAR enter
---                 EXIT PROGRAM
---             ELSE
---                 LET ls_sie                    = lr_precio_acc.siefore
---                 LET gar_precio_acc[ls_sie].*  = lr_precio_acc.*
---             END IF
---     END FOREACH
--- 
--- END FUNCTION

---FUNCTION f_redondea_val(p_monto_redondear, p_redondea)
---
---    DEFINE
---        p_monto_redondear DECIMAL(16,6)
---
---    DEFINE
---        p_redondea       SMALLINT
---
---
---    DEFINE
---        ls_monto_return   DECIMAL(16,2)
---
---    PREPARE round_id FROM "SELECT NVL(ROUND(?, ?),0) FROM systables WHERE tabid=1"
---    DECLARE round_cur CURSOR FOR round_id
---
---    OPEN round_cur USING p_monto_redondear, p_redondea
---    FETCH round_cur INTO ls_monto_return
---
---    CLOSE round_cur
---
---    RETURN ls_monto_return
---
---
---END FUNCTION

---#------------------------------------------------------------------------------#
---# Obtiene el ultimo identificador de proceso                                   #
---#------------------------------------------------------------------------------#
---FUNCTION f_ultimo_id_err()
---
---    DEFINE
---        li_iderr        INTEGER
---
---    SELECT MAX(id_proceso) + 1
---    INTO   li_iderr
---    FROM   ret_bitacora_error
---
---    IF li_iderr IS NULL THEN
---        LET li_iderr = 1
---    END IF
---
---    RETURN li_iderr
---
---END FUNCTION

---#------------------------------------------------------------------------------#
---# Inserta el registro en la bitacora de errores de carga                       #
---#------------------------------------------------------------------------------#
---FUNCTION f_inserta_bitacora(pr_error)
---
---    DEFINE pr_error RECORD
---        id_proceso   LIKE ret_bitacora_error.id_proceso     ,
---        nss          LIKE ret_bitacora_error.nss            ,
---        curp         LIKE ret_bitacora_error.curp           ,
---        folio        LIKE ret_bitacora_error.folio          ,
---        tipo_campo   LIKE ret_bitacora_error.tipo_campo     ,
---        nom_campo    LIKE ret_bitacora_error.nom_campo      ,
---        valor_campo  LIKE ret_bitacora_error.valor_campo    ,
---        id_error     LIKE ret_bitacora_error.id_error
---    END RECORD
---
---    DEFINE lr_bitacora RECORD LIKE ret_bitacora_error.*
---
---    DEFINE
---        lc_hora         CHAR(05)
---
---    LET lc_hora = TIME
---
---    -- Campos generales
---    LET lr_bitacora.programa    = "RETC708"
---    LET lr_bitacora.nom_archivo = " "
---    LET lr_bitacora.fecha_error = HOY
---    LET lr_bitacora.hora_error  = lc_hora
---    LET lr_bitacora.usuario     = gs_usuario
---
---    -- Campos por parametro
---    LET lr_bitacora.id_proceso  = pr_error.id_proceso
---    LET lr_bitacora.nss         = pr_error.nss
---    LET lr_bitacora.curp        = pr_error.curp
---    LET lr_bitacora.folio       = pr_error.folio
---    LET lr_bitacora.tipo_campo  = pr_error.tipo_campo
---    LET lr_bitacora.nom_campo   = pr_error.nom_campo
---    LET lr_bitacora.valor_campo = pr_error.valor_campo
---    LET lr_bitacora.id_error    = pr_error.id_error
---
---    INSERT INTO ret_bitacora_error
---    VALUES (lr_bitacora.*)
---
---
---END FUNCTION

#------------------------------------------------------------------------------#
# Consulta la informacion de la bitacora de errores, ya sea por medio del menu #
# principal o de forma automatica al presentarse un error en la carga          #
#------------------------------------------------------------------------------#
FUNCTION f_bitacora_err(pi_proceso)

    DEFINE pi_proceso LIKE ret_bitacora_error.id_proceso

    DEFINE lar_bitacora ARRAY[5000] OF RECORD
        programa        LIKE ret_bitacora_error.programa     ,
        folio           LIKE ret_bitacora_error.folio        ,
        id_proceso      LIKE ret_bitacora_error.id_proceso   ,
        fecha_error     LIKE ret_bitacora_error.fecha_error  ,
        hora_error      LIKE ret_bitacora_error.hora_error   ,
        usuario         LIKE ret_bitacora_error.usuario      ,
        nss             LIKE ret_bitacora_error.nss          ,
        curp            LIKE ret_bitacora_error.curp         ,
        tipo_campo      LIKE ret_bitacora_error.tipo_campo   ,
        nom_campo       LIKE ret_bitacora_error.nom_campo    ,
        valor_campo     LIKE ret_bitacora_error.valor_campo  ,
        id_error        LIKE ret_bitacora_error.id_error     ,
        desc_error      LIKE tab_ret_cod_error.descripcion
    END RECORD

    DEFINE  li_pos          INTEGER
    DEFINE  lc_where        CHAR(200)

    OPEN WINDOW RETC7083 AT 4,4 WITH FORM "RETC7083" ATTRIBUTE(BORDER)

    DISPLAY "  < Ctrl-C > Salir                                       < ESC > Consulta      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC7083     BITACORA DE ERRORES DE PRELIQUIDACION                            " AT 2,1 ATTRIBUTE(REVERSE)
    DISPLAY "                                                                               " AT 3,1
    DISPLAY HOY USING"DD-MM-YYYY" AT 2,65 ATTRIBUTE(REVERSE)

    -- El proceso viene de la carga de archivo, por lo que se hace directa
    -- la carga del arreglo mediante el id de proceso
    IF pi_proceso <> 0 THEN
        LET gc_query =   "SELECT programa    ,",
                         "       folio       ,",
                         "       id_proceso  ,",
                         "       fecha_error ,",
                         "       hora_error  ,",
                         "       usuario     ,",
                         "       nss         ,",
                         "       curp        ,",
                         "       tipo_campo  ,",
                         "       nom_campo   ,",
                         "       valor_campo ,",
                         "       id_error    ,",
                         "       ' '          ",
                         "FROM   ret_bitacora_error ",
                         "WHERE  id_proceso = " , pi_proceso
    ELSE
        -- Se inicia el construct para obtener los datos de consulta
        -- para la bitacora
        LET int_flag = FALSE

        CONSTRUCT BY NAME lc_where ON  folio             ,
                                       fecha_error       ,
                                       usuario
            ON KEY (CONTROL-C)
                LET INT_FLAG = TRUE
                EXIT CONSTRUCT

            ON KEY ( ESC )
                LET INT_FLAG = FALSE
                EXIT CONSTRUCT


        END CONSTRUCT

        IF INT_FLAG = TRUE THEN
            LET INT_FLAG = FALSE
            ERROR "  BUSQUEDA CANCELADA...  "
            SLEEP 1
            ERROR ""
            CLEAR SCREEN
            CLOSE WINDOW RETC7083
            RETURN
        END IF

        LET gc_query =   "SELECT programa    ,",
                         "       folio       ,",
                         "       id_proceso  ,",
                         "       fecha_error ,",
                         "       hora_error  ,",
                         "       usuario     ,",
                         "       nss         ,",
                         "       curp        ,",
                         "       tipo_campo  ,",
                         "       nom_campo   ,",
                         "       valor_campo ,",
                         "       id_error    ,",
                         "       ' '          ",
                         " FROM   ret_bitacora_error ",
                         " WHERE ", lc_where CLIPPED ,
                         " AND    programa = 'RETC708' ",
                         " ORDER BY id_proceso DESC "
    END IF

    PREPARE prp_err FROM gc_query
    DECLARE cur_err CURSOR FOR prp_err

    LET li_pos = 1

    FOREACH cur_err INTO lar_bitacora[li_pos].*

        SELECT descripcion
        INTO   lar_bitacora[li_pos].desc_error
        FROM   tab_ret_cod_error
        WHERE  id_error = lar_bitacora[li_pos].id_error

        LET li_pos = li_pos + 1

    END FOREACH

    INITIALIZE lar_bitacora[li_pos].* TO NULL

        IF (li_pos - 1) >= 1 THEN
            CALL SET_COUNT(li_pos - 1)

            DISPLAY ARRAY lar_bitacora TO scr_err.*

                ON KEY (INTERRUPT)
                    EXIT DISPLAY

            END DISPLAY

            CLEAR SCREEN
            CLOSE WINDOW RETC7083

        ELSE
            ERROR "  NO EXISTEN REGISTROS  "
            SLEEP 1
            ERROR ""
            CLEAR SCREEN
            CLOSE WINDOW RETC7083
        END IF

END FUNCTION


FUNCTION f_genera_detalle(pdt_fec_detalle)

    DEFINE   lr_montos                RECORD
        tipo_mov                      LIKE dis_cuenta.tipo_movimiento   ,
        acciones                      LIKE dis_cuenta.monto_en_acciones ,
        pesos                         LIKE dis_cuenta.monto_en_pesos
    END RECORD
    
    DEFINE   pdt_fec_detalle          DATE
    DEFINE   ls_sie                   SMALLINT

    CALL   Fnc_Filtro()    
    IF  gs_trate              > 0 THEN
        OPEN WINDOW RETC7084 AT 4,4                                                WITH FORM "RETC7084"  ATTRIBUTE (BORDER)
        DISPLAY " <CTRL-C> SALIR                                                               " AT 1,1  ATTRIBUTE(REVERSE)
        DISPLAY " RETC7084 CONSULTA DE PRE LIQUIDACION DE RECURSOS A DESINVERTIR               " AT 3,1  ATTRIBUTE(REVERSE)
        DISPLAY HOY USING"DD-MM-YYYY"                                                            AT 3,65 ATTRIBUTE(REVERSE)
        CALL SET_COUNT(12)
        DISPLAY ARRAY gar_tot TO scr_det1.*
        CLOSE WINDOW RETC7084 
    ELSE
        CALL SET_COUNT(12)
    END IF

END FUNCTION


FUNCTION Fnc_Filtro()
   DEFINE  ld_fecha_conversion          DATE
   DEFINE  ls_sale                      SMALLINT 
   DEFINE  i                            SMALLINT
   DEFINE  la_fol                       ARRAY[10] OF RECORD
         marca                          CHAR(1),
         folio                          INTEGER 
   END RECORD
   DEFINE  cuantos                      INTEGER
   DEFINE  lc_fecha                     CHAR(10)
   DEFINE  ln_year                      SMALLINT
   DEFINE  ln_month                     SMALLINT
   DEFINE  ln_day                       SMALLINT

   FOR   i = 1 TO 10 
         LET  la_fol[i].marca         =  NULL 
         LET  la_fol[i].folio         =  NULL 
   END FOR
   LET   cuantos                      =  NULL 

   OPEN    WINDOW RETC7085                                             AT 4,4 WITH FORM "RETC7085"   ATTRIBUTE (BORDER)
   DISPLAY " <CTRL-C> SALIR                                             <ESC> EJECUTAR    " AT 1,1   ATTRIBUTE(REVERSE)
   DISPLAY " RETC7085       PRELIQUIDACION DE RECURSOS A DESINVERTIR                      " AT 3,1   ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY"                                                            AT 3,65  ATTRIBUTE(REVERSE)

   LET  ld_fecha_conversion =  HOY
   WHILE TRUE 

         ### ... Input de fecha y validacion 
         INPUT  ld_fecha_conversion  
                WITHOUT DEFAULTS 
          FROM  fecha_conversion 

             AFTER  FIELD fecha_conversion 
                IF  ld_fecha_conversion  IS NULL THEN 
                    LET  ls_sale     =   2        
                    EXIT  INPUT 
                END IF

                LET     i  =  1
                LET     lc_fecha             =   ld_fecha_conversion 
                LET     ln_year              =   lc_fecha[7, 10]
                LET     ln_month             =   lc_fecha[1,  2]
                LET     ln_day               =   lc_fecha[4,  5]
                FOREACH  d_sel_fecha      USING  ln_year,
                                                 ln_month,
                                                 ln_day
                                           INTO  la_fol[i].*, cuantos 
                    LET  i  =  i + 1
                END FOREACH

                IF   SQLCA.SQLCODE  =  NOTFOUND 
                 OR  i              =  1         THEN
                    ERROR ' No hay información para la fecha solicitada '
                    NEXT  FIELD fecha_conversion
                ELSE 
                    CALL  SET_COUNT(i - 1)
                    LET  ls_sale   =  1
                END IF
                EXIT  INPUT 

            ON KEY (CONTROL-C)
                LET  ls_sale   =  0  
                EXIT INPUT 
         END INPUT
     
         IF  ls_sale    =  0  THEN 
             EXIT WHILE 
         END IF 

         LET    ls_sale  =   0
         INPUT  ARRAY  la_fol 
              WITHOUT  DEFAULTS 
                 FROM  scr_det2.*

             BEFORE  FIELD  folio 
                IF   ls_sale    =  1   THEN
                     NEXT FIELD  marca
                END IF

             AFTER   FIELD  folio
                IF   ld_fecha_conversion IS NULL  THEN
                     LET  ld_fecha_conversion =  Fnc_FechaFolio(la_fol[1].folio)
                     DISPLAY  ld_fecha_conversion  TO fecha_conversion
                END IF

             ON KEY (CONTROL-C)
                LET  ls_sale   =  0  
                EXIT INPUT

             ON KEY (ESC) 
                IF   ld_fecha_conversion IS NULL  THEN
                     ERROR  ' ES NECESARIO CAPTURAR UN FOLIO VALIDO  ... '      
                     NEXT FIELD folio
                ELSE
                     LET  ls_sale   =  1
                END IF
                EXIT INPUT

         END INPUT

         IF  ls_sale    =  0  THEN 
         ELSE 
             CALL Fnc_Acumula1a()  
             FOR  i  =  1  TO 10 
                  IF   la_fol[i].marca     =    'x'  THEN 
                       CALL Fnc_Acumula2a( ld_fecha_conversion, la_fol[i].folio) 
                       LET   ls_sale   = 1
                       LET   gs_trate    =   gs_trate + 1
                  END IF 
             END FOR 
         END IF 
         EXIT WHILE 
   END WHILE 
   CLOSE     WINDOW RETC7085    
END FUNCTION 


FUNCTION  Fnc_Acumula1a()
  DEFINE  i                         SMALLINT 
  DEFINE  j                         SMALLINT 

  FOR i  =  1  TO  6
      CASE i WHEN  1
              LET  j                        =  1 
              LET  gar_tot[j].sie           =  1
              LET  gar_tot[j+1].sie         =  10 
             WHEN  2
              LET  j                        =  3 
              LET  gar_tot[j].sie           =  2
              LET  gar_tot[j+1].sie         =  10 
             WHEN  3
              LET  j                        =  5 
              LET  gar_tot[j].sie           =  3
              LET  gar_tot[j+1].sie         =  10 
             WHEN  4
              LET  j                        =  7 
              LET  gar_tot[j].sie           =  4
              LET  gar_tot[j+1].sie         =  10 
             WHEN  5
              LET  j                        =  9 
              LET  gar_tot[j].sie           =  5
              LET  gar_tot[j+1].sie         =  10 
             WHEN  6
              LET  j                        =  11 
              LET  gar_tot[j].sie           =  11
              LET  gar_tot[j+1].sie         =  10 
      END CASE

      IF   gar_tot[j].sie                   <    6    THEN 
           LET  gar_tot[j].des_sie                =  Fnc_Siefore(gar_tot[j].sie)
           LET  gar_tot[j].des_tip                =  '921 - Cargo Desinversión de Recursos ' 
           LET  gar_tot[j].des_scta_1             =  Fnc_Subcta(1)                           
           LET  gar_tot[j].des_scta_2             =  Fnc_Subcta(2)                           
           LET  gar_tot[j].des_scta_4             =  Fnc_Subcta(5)                           
           LET  gar_tot[j].des_scta_5             =  Fnc_Subcta(6)                           
           LET  gar_tot[j].des_scta_6             =  Fnc_Subcta(9)                           
           LET  gar_tot[j].des_scta_9             =  Fnc_Subcta(7)                           
           LET  gar_tot[j].des_scta_s             =  'Subtotal Siefore '                     
           LET  gar_tot[j].accion_1               =  0
           LET  gar_tot[j].pesos_1                =  0
           LET  gar_tot[j].accion_2               =  0
           LET  gar_tot[j].pesos_2                =  0
           LET  gar_tot[j].accion_4               =  0
           LET  gar_tot[j].pesos_4                =  0
           LET  gar_tot[j].accion_5               =  0
           LET  gar_tot[j].pesos_5                =  0
           LET  gar_tot[j].accion_6               =  0
           LET  gar_tot[j].pesos_6                =  0
           LET  gar_tot[j].accion_9               =  0
           LET  gar_tot[j].pesos_9                =  0
           LET  gar_tot[j].accion_s               =  0
           LET  gar_tot[j].pesos_s                =  0

           LET  gar_tot[j+1].des_sie              =  Fnc_Siefore(gar_tot[j+1].sie)
           LET  gar_tot[j+1].des_tip              =  '922 - Abono Desinversión de Recursos ' 
           LET  gar_tot[j+1].des_scta_1           =  Fnc_Subcta(1)                           
           LET  gar_tot[j+1].des_scta_2           =  Fnc_Subcta(2)                           
           LET  gar_tot[j+1].des_scta_4           =  Fnc_Subcta(5)                           
           LET  gar_tot[j+1].des_scta_5           =  Fnc_Subcta(6)                           
           LET  gar_tot[j+1].des_scta_6           =  Fnc_Subcta(9)                           
           LET  gar_tot[j+1].des_scta_9           =  Fnc_Subcta(7)                           
           LET  gar_tot[j+1].des_scta_s           =  'Subtotal Siefore '                     
           LET  gar_tot[j+1].accion_1             =  0
           LET  gar_tot[j+1].pesos_1              =  0
           LET  gar_tot[j+1].accion_2             =  0
           LET  gar_tot[j+1].pesos_2              =  0
           LET  gar_tot[j+1].accion_4             =  0
           LET  gar_tot[j+1].pesos_4              =  0
           LET  gar_tot[j+1].accion_5             =  0
           LET  gar_tot[j+1].pesos_5              =  0
           LET  gar_tot[j+1].accion_6             =  0
           LET  gar_tot[j+1].pesos_6              =  0
           LET  gar_tot[j+1].accion_9             =  0
           LET  gar_tot[j+1].pesos_9              =  0
           LET  gar_tot[j+1].accion_s             =  0
           LET  gar_tot[j+1].pesos_s              =  0
      END IF  

      IF   gar_tot[j].sie                    =   11   THEN 
           LET  gar_tot[j].des_sie                =  Fnc_Siefore(gar_tot[j].sie)
           LET  gar_tot[j].des_tip                =  '921 - Cargo Desinversión de Recursos ' 
           LET  gar_tot[j].des_scta_1             =  Fnc_Subcta(4)                           
           LET  gar_tot[j].des_scta_2             =  Fnc_Subcta(8)                           
           LET  gar_tot[j].des_scta_4             =  ''           
           LET  gar_tot[j].des_scta_5             =  ''           
           LET  gar_tot[j].des_scta_6             =  ''           
           LET  gar_tot[j].des_scta_9             =  ''            
           LET  gar_tot[j].des_scta_s             =  'Subtotal Siefore '                     
           LET  gar_tot[j].accion_1               =  0
           LET  gar_tot[j].pesos_1                =  0
           LET  gar_tot[j].accion_2               =  0
           LET  gar_tot[j].pesos_2                =  0
           LET  gar_tot[j].accion_4               =  ''
           LET  gar_tot[j].pesos_4                =  ''
           LET  gar_tot[j].accion_5               =  ''
           LET  gar_tot[j].pesos_5                =  ''
           LET  gar_tot[j].accion_6               =  ''
           LET  gar_tot[j].pesos_6                =  ''
           LET  gar_tot[j].accion_9               =  ''
           LET  gar_tot[j].pesos_9                =  ''
           LET  gar_tot[j].accion_s               =  0
           LET  gar_tot[j].pesos_s                =  0

           LET  gar_tot[j+1].des_sie              =  Fnc_Siefore(gar_tot[j+1].sie)
           LET  gar_tot[j+1].des_tip              =  '922 - Abono Desinversión de Recursos '
           LET  gar_tot[j+1].des_scta_1           =  Fnc_Subcta(4)
           LET  gar_tot[j+1].des_scta_2           =  Fnc_Subcta(8)
           LET  gar_tot[j+1].des_scta_4           =  ''
           LET  gar_tot[j+1].des_scta_5           =  ''
           LET  gar_tot[j+1].des_scta_6           =  ''
           LET  gar_tot[j+1].des_scta_9           =  ''
           LET  gar_tot[j+1].des_scta_s           =  'Subtotal Siefore '
           LET  gar_tot[j+1].accion_1             =  0
           LET  gar_tot[j+1].pesos_1              =  0
           LET  gar_tot[j+1].accion_2             =  0
           LET  gar_tot[j+1].pesos_2              =  0
           LET  gar_tot[j+1].accion_4             =  ''
           LET  gar_tot[j+1].pesos_4              =  ''
           LET  gar_tot[j+1].accion_5             =  ''
           LET  gar_tot[j+1].pesos_5              =  ''
           LET  gar_tot[j+1].accion_6             =  ''
           LET  gar_tot[j+1].pesos_6              =  ''
           LET  gar_tot[j+1].accion_9             =  ''
           LET  gar_tot[j+1].pesos_9              =  ''
           LET  gar_tot[j+1].accion_s             =  0
           LET  gar_tot[j+1].pesos_s              =  0
      END IF
END FOR

END FUNCTION


FUNCTION Fnc_Siefore(ls_cve)
  DEFINE  ls_cve            INTEGER 
  DEFINE  lc_des            CHAR(40)

  EXECUTE p_sel_sie   USING  ls_cve
                       INTO  lc_des
  IF  STATUS = NOTFOUND   THEN 
      LET  lc_des   = 'No Hay SIEFORE'
  END IF 
  RETURN  lc_des
END FUNCTION 
 

FUNCTION Fnc_Subcta(ls_cve)
  DEFINE  ls_cve            INTEGER
  DEFINE  lc_des            CHAR(40)

  EXECUTE  p_sel_sub  USING  ls_cve 
                       INTO  lc_des 
  IF  STATUS =  NOTFOUND  THEN
      LET  lc_des  = 'No Hay SUBCUENTA '
  END IF 
  RETURN lc_des 
END FUNCTION 


FUNCTION  Fnc_Acumula2a(ld_fecha, ls_folio)
  DEFINE  ld_fecha                  DATE
  DEFINE  ls_folio                  INTEGER 
  DEFINE  k                         SMALLINT 
  DEFINE  j                         SMALLINT 
  DEFINE  ls_sie                    INTEGER
  DEFINE  ls_subcta                 INTEGER
  DEFINE  ls_accion                 DECIMAL(16,6)
  DEFINE  ls_pesos                  DECIMAL(16,6)
  DEFINE  ls_folio_abo              INTEGER 
  DEFINE  lc_fecha                  CHAR(10)
  DEFINE  ln_year                   SMALLINT
  DEFINE  ln_month                  SMALLINT
  DEFINE  ln_day                    SMALLINT

  LET      lc_fecha             =   ld_fecha
  LET      ln_year              =   lc_fecha[7, 10]
  LET      ln_month             =   lc_fecha[1,  2]
  LET      ln_day               =   lc_fecha[4,  5]
  FOREACH  d_sel_car  USING  ld_fecha,
                             ls_folio 
                       INTO  ls_sie,
                             ls_subcta,
                             ls_accion,
                             ls_pesos

      CASE ls_sie     WHEN  1   LET  k  =  1 
                      WHEN  2   LET  k  =  3 
                      WHEN  3   LET  k  =  5 
                      WHEN  4   LET  k  =  7 
                      WHEN  5   LET  k  =  9 
                      WHEN  11  LET  k  =  11 
      END CASE

      IF   ls_sie     <     6   THEN 
           CASE ls_subcta  WHEN  1    
                LET  gar_tot[k].accion_1       =  gar_tot[k].accion_1  +  ls_accion
                LET  gar_tot[k].pesos_1        =  gar_tot[k].pesos_1   +  ls_pesos
                           WHEN  2   
                LET  gar_tot[k].accion_2       =  gar_tot[k].accion_2  +  ls_accion
                LET  gar_tot[k].pesos_2        =  gar_tot[k].pesos_2   +  ls_pesos
                           WHEN  5    
                LET  gar_tot[k].accion_4       =  gar_tot[k].accion_4  +  ls_accion
                LET  gar_tot[k].pesos_4        =  gar_tot[k].pesos_4   +  ls_pesos
                           WHEN  6    
                LET  gar_tot[k].accion_5       =  gar_tot[k].accion_5  +  ls_accion
                LET  gar_tot[k].pesos_5        =  gar_tot[k].pesos_5   +  ls_pesos
                           WHEN  9    
                LET  gar_tot[k].accion_6       =  gar_tot[k].accion_6  +  ls_accion
                LET  gar_tot[k].pesos_6        =  gar_tot[k].pesos_6   +  ls_pesos
                           WHEN  7   
                LET  gar_tot[k].accion_9       =  gar_tot[k].accion_9  +  ls_accion
                LET  gar_tot[k].pesos_9        =  gar_tot[k].pesos_9   +  ls_pesos
           END CASE
           LET  gar_tot[k].accion_s            =  gar_tot[k].accion_s  +  ls_accion
           LET  gar_tot[k].pesos_s             =  gar_tot[k].pesos_s   +  ls_pesos
      END IF

      IF   ls_sie     =     11  THEN
           CASE ls_subcta  WHEN  4
                LET  gar_tot[k].accion_1       =  gar_tot[k].accion_1  +  ls_accion
                LET  gar_tot[k].pesos_1        =  gar_tot[k].pesos_1   +  ls_pesos
                           WHEN  8
                LET  gar_tot[k].accion_2       =  gar_tot[k].accion_2  +  ls_accion
                LET  gar_tot[k].pesos_2        =  gar_tot[k].pesos_2   +  ls_pesos
           END CASE
           LET  gar_tot[k].accion_s            =  gar_tot[k].accion_s  +  ls_accion
           LET  gar_tot[k].pesos_s             =  gar_tot[k].pesos_s   +  ls_pesos
      END IF

  END FOREACH 

  EXECUTE  p_sel_fol       USING  ls_folio 
                            INTO  ls_folio_abo

  FOREACH  d_sel_abo3      USING  ls_folio_abo,
                                  ls_folio 
                            INTO  ls_sie,
                                  ls_subcta,
                                  ls_accion,
                                  ls_pesos

      CASE ls_sie     WHEN  1   LET  j  =  2 
                      WHEN  2   LET  j  =  4
                      WHEN  3   LET  j  =  6 
                      WHEN  4   LET  j  =  8 
                      WHEN  5   LET  j  =  10 
                      WHEN  11  LET  j  =  12 
      END CASE

      IF   ls_sie     <     6   THEN 
           CASE ls_subcta  WHEN  1    
                LET  gar_tot[j].accion_1       =  gar_tot[j].accion_1  +  ls_accion
                LET  gar_tot[j].pesos_1        =  gar_tot[j].pesos_1   +  ls_pesos
                           WHEN  2   
                LET  gar_tot[j].accion_2       =  gar_tot[j].accion_2  +  ls_accion
                LET  gar_tot[j].pesos_2        =  gar_tot[j].pesos_2   +  ls_pesos
                           WHEN  5   
                LET  gar_tot[j].accion_4       =  gar_tot[j].accion_4  +  ls_accion
                LET  gar_tot[j].pesos_4        =  gar_tot[j].pesos_4   +  ls_pesos
                           WHEN  6    
                LET  gar_tot[j].accion_5       =  gar_tot[j].accion_5  +  ls_accion
                LET  gar_tot[j].pesos_5        =  gar_tot[j].pesos_5   +  ls_pesos
                           WHEN  9    
                LET  gar_tot[j].accion_6       =  gar_tot[j].accion_6  +  ls_accion
                LET  gar_tot[j].pesos_6        =  gar_tot[j].pesos_6   +  ls_pesos
                           WHEN  7    
                LET  gar_tot[j].accion_9       =  gar_tot[j].accion_9  +  ls_accion
                LET  gar_tot[j].pesos_9        =  gar_tot[j].pesos_9   +  ls_pesos
           END CASE
           LET  gar_tot[j].accion_s            =  gar_tot[j].accion_s  +  ls_accion
           LET  gar_tot[j].pesos_s             =  gar_tot[j].pesos_s   +  ls_pesos
      END IF
     
      IF   ls_sie     =     11  THEN
           CASE ls_subcta  WHEN  4
                LET  gar_tot[j].accion_1       =  gar_tot[j].accion_1  +  ls_accion
                LET  gar_tot[j].pesos_1        =  gar_tot[j].pesos_1   +  ls_pesos
                           WHEN  8
                LET  gar_tot[j].accion_2       =  gar_tot[j].accion_2  +  ls_accion
                LET  gar_tot[j].pesos_2        =  gar_tot[j].pesos_2   +  ls_pesos
           END CASE
           LET  gar_tot[j].accion_s            =  gar_tot[j].accion_s  +  ls_accion
           LET  gar_tot[j].pesos_s             =  gar_tot[j].pesos_s   +  ls_pesos
      END IF
  END FOREACH 

END FUNCTION


FUNCTION  Fnc_FechaFolio(ls_folio)
  DEFINE  ls_folio                     INTEGER
  DEFINE  ld_fecha                     DATE

  EXECUTE  p_sel_fec    USING  ls_folio
                         INTO  ld_fecha

  IF  STATUS  =  NOTFOUND  THEN
      LET ld_fecha  =  ''
      ERROR  ' No existe el folio solicitado '
  END IF
  RETURN  ld_fecha
END FUNCTION

