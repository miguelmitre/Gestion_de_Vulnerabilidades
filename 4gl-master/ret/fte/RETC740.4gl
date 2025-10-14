################################################################################
#Owner             => E.F.P.                                                   #
#Programa RETC740  => PROVISION  E IDENTIFICACION DE CUENTAS PARA REINVERSION  #
#Fecha creacion    => 30 DE NOVIEMBRE 2011                                     #
#By                => ISAI JIMENEZ ROJAS                                       #
#Fecha actualiz.   => 12/12/2011 11:34:46 a.m.                                 #
#Actualizacion     => IJR 30/01/2012 12:18:39 p.m.                             #
#Sistema           => Retiros Ventanilla 2.5                                   #
################################################################################
#Modificacion      => Se revisan niveles de reinversion 10-Feb-2012 - ACS      # 
################################################################################
#Modificacion      => Se eliminan niveles de reinversion, se toman todas las   #
#                  => solicitudes de reinversion y se reinvierte el saldo actual
#                  => de la siefore 10 --ACS-->  -23-julio-2012                #
################################################################################
DATABASE safre_af

GLOBALS

    DEFINE g_enter               CHAR(1)
    DEFINE g_tecla               CHAR(1)
    DEFINE g_mensaje             CHAR(80)
    DEFINE m_query               CHAR(7000)
    DEFINE m_cuantos             SMALLINT  

    DEFINE gr_edo RECORD
        solicitud     LIKE ret_estado.estado_solicitud ,
        provisionado  LIKE ret_estado.estado_solicitud
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
    DEFINE  g_comando             CHAR(500)
    DEFINE  gc_query              CHAR(1800)
    DEFINE  vGc_Query_EspIni      CHAR(1800)
    DEFINE  vGc_Query_Esp         CHAR(1800)
    DEFINE  vGc_Query_Esp1        CHAR(1800)
    DEFINE  vGc_Query_Esp2        CHAR(1800)
    DEFINE  vGc_Query_Esp3        CHAR(1800)
    DEFINE  vGc_Query_Esp4        CHAR(1800)

    DEFINE #glo #smallint
        gs_peiss              ,
        gs_flag               ,
        gs_flag_err           ,
        gs_sieviv             ,
        gs_num_siefores       , #-- Indica el numero de siefores que se usan actualmente
        gs_cod_afore          SMALLINT

END GLOBALS


#==============================================================================#
#                                                                              #
#==============================================================================#
MAIN
    
    DEFER INTERRUPT
    
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        MESSAGE LINE LAST -1,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG(FGL_GETENV("USER")||".RETC740.log")   #ACS-28012014

    CALL init() #i

    OPEN WINDOW main_win AT 4,2 WITH 19 ROWS, 78 COLUMNS ATTRIBUTE(BORDER)
    DISPLAY " RETC740                    REINVERSION DE RECURSOS                    " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,69 ATTRIBUTE(REVERSE)

    MENU "REINVERSION"
        
        COMMAND "Provision" "Ejecuta la Provision de Reinversion"
            CALL f_provisiona_reinversion()
            
        COMMAND "Identifica Reinversion" "Ejecuta proceso de identificacion de cuentas a reinvertir"
            CALL f_identifica_reinversion()

        COMMAND "Consulta Reinversion" "Consulta las cuentas identificadas para reinversion"
            CALL consulta_reinversion()

        COMMAND "Salir" "Salir del Programa "
            EXIT MENU
    END MENU
    
    CLOSE WINDOW main_win 

END MAIN


#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION init()
    DEFINE  lc_prepare           CHAR(300)
    DEFINE  ls_dias_hab          SMALLINT

    LET HOY                 =  TODAY
    LET gs_sieviv           =  12
    LET ls_dias_hab         =  2
    LET m_query   = ""
    LET m_cuantos = 0

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

    SELECT A.estado_solicitud INTO   gr_edo.solicitud
    FROM   ret_estado A
    WHERE  A.descripcion MATCHES "*SOLICITUD*REINVERSION*"

    SELECT A.estado_solicitud INTO   gr_edo.provisionado
    FROM   ret_estado A
    WHERE  A.descripcion MATCHES "*PROVISIONADO*REINVERSION*"

    ----- SALDO AL DIA -----
    LET lc_prepare = "EXECUTE FUNCTION fn_saldo_dia (?,?,?,?) "
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

   LET   gc_query  =  ' SELECT  folio_abono        ',
                      '   FROM  ret_folio          ',
                      '  WHERE  folio_cargo   =  ? '
   PREPARE  p_sel_fol          FROM  gc_query    

    LET    gc_query   =   '  SELECT  B.id_nivel_reinv  ',
                          '    FROM  ret_tipo_origen_reinv  B    ',
                          '   WHERE  B.id_tipo_origen_reinv = ? '

   PREPARE  p_SelNivel         FROM  gc_query

    LET    vGc_Query_EspIni  =   ' SELECT SUM(monto_en_pesos), subcuenta, siefore, ',
                                 ' tipo_movimiento, curp FROM   dis_cuenta                 ',
                                 '   WHERE (folio            = ?           OR ',
                                        '   folio            = ?           )  ',
                                 '   AND    consecutivo_lote = ?              ',
                                 '   AND    nss              = ?              ',
                                 '   AND    tipo_movimiento IN (921, 922)     ',
                                 ' GROUP BY 2,3,4,5 ',
                                 ' HAVING SUM(monto_en_pesos) != 0 '
                                 

--- Instrucciones 6 Feb 2012 
--- Nivel1 : 1,2,6,9,5,7
--- Nivel2 : 1,2,6,9,5
--- Nivel3 : 7
--- Nivel4 : 1,2,6,9,5,7

    LET    vGc_Query_Esp1  =   '   AND    subcuenta  IN (1,2,6,9,5,7)   '
    LET    vGc_Query_Esp2  =   '   AND    subcuenta  IN (1,2,6,9,5)     '
    LET    vGc_Query_Esp3  =   '   AND    subcuenta  IN (7)             '
    
       ----- DESMARCA -----
   LET lc_prepare = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"
   PREPARE eje_desmarca FROM lc_prepare
   

END FUNCTION

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


#==============================================================================#
# Funcion principal de ejecucion de Provision                                  #
#==============================================================================#
FUNCTION f_provisiona_reinversion()

    DEFINE v_fecha_provision       DATE
    DEFINE v_total_solictudes    INTEGER
    DEFINE v_mensaje             CHAR(80)
    
    ---------------------------------
    --INICIA PROCESO DE PROVISION
    ---------------------------------
    
    OPEN WINDOW RETC7401 AT 4,2 WITH FORM "RETC7401" ATTRIBUTE (BORDER)
    DISPLAY " <CTRL-C> SALIR                                             <ESC> EJECUTAR    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC740                  PROVISION DE REINVERSION                                  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

    # CAPTURA EL CRITERIO DE BUSQUEDA

    #LET fecha_provision = HOY    
    
    INPUT v_fecha_provision FROM fecha_provision
    #DISPLAY BY NAME fecha_provision 
        AFTER FIELD fecha_provision

            IF f_cuenta(v_fecha_provision) = 0 THEN
                ERROR "NO HAY SOLICITUDES DE REINVERSION A PROVISIONAR!"                
                NEXT FIELD fecha_provision
            END IF

        AFTER INPUT            
            IF f_cuenta(v_fecha_provision) = 0 THEN
                ERROR "NO HAY SOLICITUDES DE REINVERSION A PROVISIONAR!"                
                NEXT FIELD fecha_provision
            END IF
      
            ON KEY (ESC)
               IF f_cuenta(v_fecha_provision) = 0 THEN
                   ERROR "NO HAY SOLICITUDES DE REINVERSION A PROVISIONAR!"
                   NEXT FIELD fecha_provision
               ELSE 
                   LET v_mensaje = "SE DETECTARON ",m_cuantos USING "<<<,<<&"," SOLICITUDES DE REINVERSION "
                   DISPLAY v_mensaje AT 18,1 
                   PROMPT "ESTA SEGURO DE PROVISIONAR(S/N):" FOR CHAR g_tecla
                   IF g_tecla MATCHES "[sSnN]" THEN
                      IF g_tecla MATCHES "[sS]" THEN                         
                         IF f_provisiona(v_fecha_provision)=1 THEN 
                             NEXT FIELD fecha_provision
                         END IF
                      ELSE
                         ERROR "PROCESO CANCELADO" 
                         NEXT FIELD fecha_provision
                      END IF
                  ELSE
                     ERROR "SOLO PRESIONE S o N"
                  END IF
               END IF
             EXIT INPUT
            
            ON KEY (CONTROL-C, INTERRUPT)
               ERROR "PROCESO CANCELADO" SLEEP 2
               ERROR ""
               EXIT INPUT
            
    END INPUT     
    CLOSE WINDOW RETC7401

END FUNCTION

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION f_provisiona(p_fecha_pre)

    DEFINE p_folio               INTEGER
    DEFINE p_fecha_pre           DATE 
    DEFINE lr_solicitud        RECORD 
    	  folio_cargo             LIKE safre_af:ret_solicitud_saldo.folio_cargo,
    	  nss                     LIKE safre_af:ret_solicitud_saldo.nss,
    	  id_solicitud_saldo      LIKE safre_af:ret_solicitud_saldo.id_solicitud_saldo,
    	  tipo_reinv              LIKE safre_af:ret_reinversion.id_tipo_origen_reinv,
    	  row_id                  INTEGER
      END RECORD

    DEFINE lr_provi              RECORD LIKE dis_cuenta.*
    DEFINE lr_ret_folio          RECORD LIKE ret_folio.*
    DEFINE v_precio              LIKE glo_valor_accion.precio_del_dia
    DEFINE v_nuevo_folio_cargo   INTEGER
    DEFINE v_nuevo_folio_abono   INTEGER
    DEFINE v_bandera             SMALLINT
    DEFINE v_codigo_siefore      LIKE cta_regimen.codigo_siefore
    DEFINE v_errores             SMALLINT    --0 no hubo errores, 1=si 
    
    DEFINE l_id_nivel_reinv      LIKE  ret_reinversion.id_tipo_origen_reinv
    DEFINE l_cuenta              SMALLINT,
     l_cero    SMALLINT,
     l_cod     SMALLINT,
     lr_saldo RECORD 
        subcuenta SMALLINT,
        siefore   SMALLINT,
        acciones  DECIMAL(13,6),
        pesos     DECIMAL(13,6)
     END RECORD
     DEFINE l_condition    CHAR(100)
    DEFINE errmsg CHAR(50)
    DEFINE  r_marca   RECORD   LIKE safre_af:cta_act_marca.*
    DEFINE  l_msj             CHAR(500)
    DEFINE l_flag  SMALLINT
    
    INITIALIZE l_msj,r_marca.*  TO NULL
    
    LET errmsg = ""
    LET l_cero = 0
    LET l_cod = 30
    LET l_flag = 0
    --CREA TABLA PREVIA DONDE SE ALMACENARA LA INFORMACION DE PROVISION
    CALL f_tablas_tmp()

    LET v_errores = 0
    LET v_bandera = 0   --para controlar si se proceso o no informacion 
    
    --RECUPERA UN NUEVO FOLIO DE CARGO PARA LA PROVISION
    SELECT MAX(folio) + 1 INTO   v_nuevo_folio_cargo
    FROM   glo_folio 

    INSERT INTO glo_folio VALUES (v_nuevo_folio_cargo)

    --RECUPERA UN NUEVO FOLIO DE CARGO PARA LA PROVISION
    SELECT MAX(folio) + 1  INTO   v_nuevo_folio_abono
    FROM   glo_folio 

    INSERT INTO glo_folio VALUES (v_nuevo_folio_abono)
    
    INSERT INTO safre_af:ret_folio VALUES
    (v_nuevo_folio_cargo, v_nuevo_folio_abono, gr_edo.solicitud)

    DISPLAY "PROVISIONANDO REINVERSION...." AT 17,1 ATTRIBUTE(REVERSE)
    
    LET g_mensaje = "FOLIOS GENERADOS: CARGO=", 
                    v_nuevo_folio_cargo USING "<<<<<<", " ABONO=",
                    v_nuevo_folio_abono USING "<<<<<<"

    DISPLAY g_mensaje CLIPPED AT 15,1 ATTRIBUTE(REVERSE) 

    ----------------------------------------------
    --SELECCION DE LAS SOLICITUDES A REINVERTIR
    ----------------------------------------------
    
    IF p_fecha_pre IS NULL THEN
        LET l_condition = ""
    ELSE
        LET l_condition = " AND DATE(B.fecha_registro) = '", p_fecha_pre,"'"        
    END IF
    
    LET m_query = "";
    LET m_query = "SELECT a.folio_cargo, a.nss, a.id_solicitud_saldo,\n",
                  "b.id_tipo_origen_reinv, b.ROWID \n",
                  "FROM   ret_solicitud_saldo A, ret_reinversion     B \n",
                  "WHERE  a.id_solicitud_saldo = b.id_solicitud_saldo \n",
                  "AND    A.estado_solicitud   IN (106,120) \n",  --liquidado saldo, cancelado
                  "AND    b.estado_solicitud   = 140 \n",         --solicitud reinversion
                  l_condition CLIPPED, "\n"
    
    LET m_query = m_query CLIPPED
     PREPARE pre_saldo FROM m_query
      DECLARE cur_sol CURSOR FOR pre_saldo
    
    INITIALIZE lr_solicitud.* TO NULL
     DELETE FROM tmp_provision WHERE 1 = 1
    FOREACH cur_sol INTO lr_solicitud.*
      LET v_errores = 0
        # declara cursor para el saldo
         
         DECLARE cur_10 CURSOR FOR eje_saldo_dia         
          OPEN cur_10 USING lr_solicitud.nss, l_cero, l_cero, hoy               
          INITIALIZE lr_saldo.* TO NULL
          WHENEVER ERROR CONTINUE
          FOREACH cur_10 INTO lr_saldo.*
              IF SQLCA.SQLCODE < 0 THEN              
                 LET errmsg = err_get(SQLCA.SQLCODE)
                 ERROR errmsg 
                 DISPLAY "                                                           " AT 15,1
                 DISPLAY "                                                           " AT 17,1
                 DISPLAY "                                                           " AT 18,1
                 RETURN 1
              END IF
             WHENEVER ERROR STOP
            #Solo reinvierte saldos positivos de la siefore 10            
              IF lr_saldo.siefore != 10 THEN
                 CONTINUE FOREACH
              END IF
              
              IF  lr_saldo.pesos < 0 THEN
                  CONTINUE FOREACH
              END IF

              --ASIGNA VALORES POR OMISION PARA EL REGISTRO A INSERTAR
              LET lr_provi.consecutivo_lote    = lr_solicitud.id_solicitud_saldo
              LET lr_provi.nss                 = lr_solicitud.nss
              LET lr_provi.folio_sua           = ""
              LET lr_provi.dias_cotizados      = 0
              LET lr_provi.sucursal            = NULL
              LET lr_provi.id_aportante        = "REINVER"
              LET lr_provi.estado              = 7
              LET lr_provi.etiqueta            = ""

              LET lr_provi.fecha_valor        = hoy
              LET lr_provi.fecha_pago         = hoy
              LET lr_provi.fecha_conversion   = hoy
              LET lr_provi.fecha_archivo      = hoy
              LET lr_provi.fecha_proceso      = hoy
              LET lr_provi.usuario            = gs_usuario
              LET lr_provi.subcuenta          = lr_saldo.subcuenta

              --------------------------------------------------------------------
              #variables del Abono
              --------------------------------------------------------------------
              --OBTIENE LA SIEFORE A LA CUAL CORRESPONDE LA SUBCUENTA
                 LET lr_provi.siefore = NULL
                 SELECT codigo_siefore INTO lr_provi.siefore
                 FROM   cta_regimen 
                 WHERE  nss       = lr_provi.nss
                 AND    subcuenta = lr_provi.subcuenta

                 --SI NO LA ENCUENTRA ASIGNA LA SIEFORE DE LA CUAL SE PROVISIONO
                 IF STATUS = NOTFOUND THEN
                    DISPLAY " NO EXISTE SIEFORE PARA EL NSS:  ", lr_solicitud.nss,
                    " SUBCUENTA: ",lr_saldo.subcuenta USING "<<<<<"
                    PROMPT  " <ENTER> PARA CONTINUAR " FOR CHAR g_tecla
                    CONTINUE FOREACH
                 END IF

                 --OBTIENE PRECIO DEL DIA PARA ACTUALIZACION DE MONTO EN PESOS
                 LET lr_provi.precio_accion = 0
                 SELECT precio_del_dia
                 INTO   lr_provi.precio_accion
                 FROM   glo_valor_accion
                 WHERE  fecha_valuacion = hoy
                 AND    codigo_siefore  = lr_provi.siefore

                 IF STATUS = NOTFOUND OR  lr_provi.precio_accion IS NULL THEN
                    LET g_mensaje = "ERROR: NO ENCONTRO PRECIO PARA EL DIA ",hoy USING "DD/MM/YYYY",
                                    " SIEFORE: ",lr_provi.siefore USING "<<<<"
                    MESSAGE g_mensaje ATTRIBUTE(REVERSE)
                    PROMPT "PRESIONE <ENTER> PARA SALIR:" FOR CHAR g_enter
                    EXIT PROGRAM
                 END IF

              LET lr_provi.tipo_movimiento   = 924
              LET lr_provi.folio             = v_nuevo_folio_abono
              LET lr_provi.monto_en_pesos    = lr_saldo.pesos
              LET lr_provi.monto_en_acciones = lr_saldo.pesos / lr_provi.precio_accion

              #Inserta abono
                 INSERT INTO safre_af:dis_provision VALUES(lr_provi.*)
              --------------------------------------------------------------------
              #Variables del cargo
              --------------------------------------------------------------------
                 LET lr_provi.folio = v_nuevo_folio_cargo       --Folio de cargo para la reinversion
                 LET lr_provi.tipo_movimiento    = 923
                 LET lr_provi.precio_accion      = 1
                 LET lr_provi.siefore            = lr_saldo.siefore  
                 LET lr_provi.monto_en_acciones  = (lr_saldo.acciones * -1)
                 LET lr_provi.monto_en_pesos     = (lr_saldo.pesos * -1)

              #Inserta Cargo
                 INSERT INTO safre_af:dis_provision VALUES(lr_provi.*)
                 
                 LET l_flag = 1

          END FOREACH
          CLOSE cur_10
          
           IF  l_flag = 1 THEN
                 #Actualiza solicitud
                 UPDATE ret_reinversion SET estado_solicitud   = 142
                 WHERE  id_solicitud_saldo = lr_solicitud.id_solicitud_saldo
                 AND estado_solicitud = 140
           END IF
           IF l_flag = 0 THEN 
                  
                  SELECT * INTO r_marca.* 
                  FROM cta_act_marca
                  WHERE nss = lr_solicitud.nss 
                  AND marca_cod = 921
                  AND correlativo = lr_solicitud.id_solicitud_saldo
                  
                  IF STATUS = NOTFOUND  THEN
                     LET l_msj = "NO SE HA ENCONTRADO LA MARCA NSS: ", lr_solicitud.nss,
                      " CORRELATIVO", lr_solicitud.id_solicitud_saldo
                      LET l_msj = l_msj CLIPPED
                      CALL ERRORLOG(l_msj)
                  END IF
                  
                  EXECUTE eje_desmarca USING
                          lr_solicitud.nss,                    --pnss 
                          r_marca.marca_cod,                                 --pmarca_entra 
                          lr_solicitud.id_solicitud_saldo,     --pcorrelativo 
                          l_cod,                               --pestado_marca
                          l_cero,                              --pmarca_causa 
                          gs_usuario                           --pusuario     
                                    
                  INITIALIZE r_marca.* TO NULL
  
                  SELECT * INTO r_marca.* 
                  FROM cta_act_marca
                  WHERE nss = lr_solicitud.nss  
                  AND marca_cod = 922
                  AND correlativo = lr_solicitud.id_solicitud_saldo
                  
                  # Se desmarca si tiene la marca 922
                  IF STATUS <> NOTFOUND  THEN
                     EXECUTE eje_desmarca USING
                          lr_solicitud.nss,                    --pnss 
                          r_marca.marca_cod,                                 --pmarca_entra 
                          lr_solicitud.id_solicitud_saldo,     --pcorrelativo 
                          l_cod,                               --pestado_marca
                          l_cero,                              --pmarca_causa 
                          gs_usuario                           --pusuario     
                  ELSE
                     LET l_msj = "NO SE HA ENCONTRADO LA MARCA NSS: ", lr_solicitud.nss,
                      " CORRELATIVO", lr_solicitud.id_solicitud_saldo
                      LET l_msj = l_msj CLIPPED
                      CALL ERRORLOG(l_msj)
                  END IF
  
                  UPDATE ret_solicitud_saldo
                  SET estado_solicitud = 122
                  WHERE nss = lr_solicitud.nss
                  AND id_solicitud_saldo = lr_solicitud.id_solicitud_saldo
                  AND estado_solicitud IN (106, 120)
                  
                 #Actualiza solicitud
                 UPDATE ret_reinversion SET estado_solicitud   = 146
                 WHERE  id_solicitud_saldo = lr_solicitud.id_solicitud_saldo
                 AND estado_solicitud = 140
           END IF
           
           LET l_flag = 0

    END FOREACH --siguiente solicitud
   
    --ACTUALIZA EL ESTADO DEL Folio
    UPDATE safre_af:ret_folio SET estado_folio = gr_edo.provisionado
    WHERE folio_cargo = v_nuevo_folio_cargo 
    AND   folio_abono = v_nuevo_folio_abono
    AND   estado_folio = gr_edo.solicitud
    
    PROMPT "PROCESO FINALIZADO  <ENTER> PARA CONTINUAR: " FOR CHAR  g_enter
    RETURN 0   
     -- END IF
      
    --END IF

END FUNCTION

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION f_tablas_tmp()

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_provision
    WHENEVER ERROR STOP

    SELECT *
    FROM   dis_provision
    WHERE  0 = 1
    INTO TEMP tmp_provision WITH NO LOG

END FUNCTION

#==============================================================================#
# Ejecuta proceso batch de identificacion de solicitudes de desinversion       #
# candidatas a ser reinvertidas                                                #
#==============================================================================#
FUNCTION f_identifica_reinversion()

   DEFINE v_status SMALLINT

   MESSAGE "EJECUTANDO PROCESO, POR FAVOR ESPERE" ATTRIBUTE(REVERSE)
   
   RUN "fglgo RETC762" RETURNING v_status

   IF v_status = 0 THEN
      MESSAGE "EL PROCESO HA SIDO EJECUTADO EXITOSAMENTE..."
   ELSE
      MESSAGE "EL PROCESO HA SIDO EJECUTADO CON ERRORES, NOTIFIQUE A SISTEMAS..."
   END IF
   
   PROMPT "PRESIONE UNA <ENTER> PARA SALIR: " FOR CHAR g_enter
              
   MESSAGE " "

END FUNCTION

#==============================================================================#
#                                                                              #
#==============================================================================#
FUNCTION consulta_reinversion()

   RUN "fglgo RETC764"

END FUNCTION

################################################################################

FUNCTION f_cuenta (l_fecha_provision)
    DEFINE   l_fecha_provision       DATE,
             v_total_solictudes    SMALLINT,
             l_condition CHAR(100);
    
    IF l_fecha_provision IS NULL THEN
         LET l_condition = " "
    ELSE         
         LET l_condition = " AND DATE(fecha_registro) = '", l_fecha_provision ,"'"
    END IF
    
    LET m_query = "";
    # DETERMINA LA CANTIDAD DE SOLICITUDES A REINVERTIR
    LET m_query = " SELECT COUNT(*) \n",
                  " FROM   ret_reinversion \n",
                  " WHERE  estado_solicitud = 140 \n", --'SOLICITUD REINVERSION'
                  " AND    id_solicitud_saldo IN (SELECT id_solicitud_saldo \n", 
                  "                               FROM   ret_solicitud_saldo \n",
                  "                               WHERE  estado_solicitud IN(106,120)) \n", --LIQUIDADO SALDO y CANCELADO SALDO
                  l_condition CLIPPED, "\n"
      LET m_query = m_query CLIPPED
      
      PREPARE pre_cuantos FROM m_query    
      EXECUTE pre_cuantos INTO v_total_solictudes      
      
      LET m_cuantos =  v_total_solictudes;
    
    RETURN v_total_solictudes;

END FUNCTION

