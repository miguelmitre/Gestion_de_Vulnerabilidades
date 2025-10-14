#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETC724  => EJECUCION DE PROVISION DE DISPOSICIONES VENTANILLA 2.5    #
#Fecha creacion    => 10 DE DICIEMBRE DE 2011                                   #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   =>                                                           #
#Actualizacion     =>                                                           #
#                                                                               #
#Sistema           => RET                                                       #
#################################################################################
DATABASE safre_af

GLOBALS
    
    DEFINE gar_precio_acc ARRAY [20] OF RECORD
        estado          SMALLINT     ,
        fecha           DATE         ,
        siefore         SMALLINT     ,
        precio_dia      DECIMAL(16,6)
    END RECORD

    DEFINE gr_edo RECORD
        confirmado              LIKE ret_estado.estado_solicitud    ,
        provisionado            LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE
        gdt_fecha_viv           ,
        HOY                     DATE

    DEFINE
        enter                   CHAR(001) ,
        gc_usuario              CHAR(015)

    DEFINE
        gs_procesa              ,
        gs_sieviv               ,
        gs_num_siefores         , #-- Indica el numero de siefores que se usan actualmente
        gs_codigo_afore         SMALLINT

END GLOBALS



-- Afecta tablas fisicas

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("RETC724.log")
    
    CALL f_tablas_tmp()
    CALL init()
    CALL f_obtiene_precios_accion(HOY)

    CALL f_despliega_info() RETURNING gs_procesa

    IF gs_procesa THEN
        CALL primer_paso()      #-- Provisiona registros D y E
        CALL segundo_paso()     #-- Se afectan las tablas fisicas
    END IF

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    DEFINE
        lc_prepare      CHAR(300)

    -- -----------------------------------------------------------------------------

    LET HOY                 = TODAY
    LET gs_sieviv           = 11

    ----- FECHA DE VIVIENDA -----
    LET lc_prepare = " EXECUTE FUNCTION fn_obten_fecha_val(?) "
    PREPARE eje_fecha_viv FROM lc_prepare
    EXECUTE eje_fecha_viv USING HOY INTO gdt_fecha_viv
    LET lc_prepare = " "

    -- Obtenemos el numero de siefores actual en el sistema
    SELECT COUNT(*)
    INTO   gs_num_siefores
    FROM   tab_siefore_local
    WHERE  codigo_siefore > 0
    AND    codigo_siefore NOT IN (11,12,13)

    ----- CODIGOS AFORES -----
    SELECT codigo_afore,
           USER
    INTO   gs_codigo_afore,
           gc_usuario
    FROM   tab_afore_local

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.confirmado
    FROM   ret_estado A
    WHERE  A.descripcion = "CONFIRMADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.provisionado
    FROM   ret_estado A
    WHERE  A.descripcion = "PROVISIONADO DISPOSICION"
    
    ----- SALDO AL DIA -----
    LET lc_prepare = "EXECUTE FUNCTION fn_saldo_dia(?,?,?,?) "
    PREPARE eje_saldo_dia FROM lc_prepare

    LET lc_prepare = " "


END FUNCTION

#---------------------------------------------------------------------------#
# primer_paso : Realiza la provision de las disposiciones de ventanilla     #
#---------------------------------------------------------------------------#
FUNCTION primer_paso()

    DEFINE lr_disp RECORD LIKE ret_solicitud_tx.*

    DEFINE lr_saldo_dia RECORD
        subcuenta       LIKE dis_cuenta.subcuenta           ,
        siefore         LIKE dis_cuenta.siefore             ,
        monto_acc       LIKE dis_cuenta.monto_en_acciones   ,
        monto_pes       LIKE dis_cuenta.monto_en_pesos       
    END RECORD 

    DEFINE 
        ls_grupo            ,
        ls_movimiento       ,
        ls_subcuenta        ,
        ls_siefore          SMALLINT

    -- -----------------------------------------------------------------------------

    DISPLAY "PROVISIONANDO CUENTAS                      ....  " AT 6,2

    LET ls_grupo = 0
    
    DECLARE cur_prov CURSOR FOR
    SELECT *
    FROM   safre_tmp:tmp_disp_imss
    ORDER BY 1

    -- Iniciamos ciclo para cada nss
    FOREACH cur_prov INTO lr_disp.*

        SELECT movimiento
        INTO   ls_movimiento
        FROM   tab_retiro
        WHERE  tipo_retiro = lr_disp.tipo_retiro

        DECLARE cur_subcta CURSOR FOR
        SELECT subcuenta
        FROM   tab_agrupa_subcta 
        WHERE  grupo        = lr_disp.grupo
        AND    subcuenta    > 0
        ORDER BY 1
        
        -- Ciclo para cada subcuenta del NSS provisionado
        FOREACH cur_subcta INTO ls_subcuenta
            
            INITIALIZE lr_saldo_dia.* TO NULL

            #-- Se barre por las subcuentas con saldo
            DECLARE cur_saldo CURSOR FOR eje_saldo_dia
            FOREACH cur_saldo USING lr_disp.nss     ,
                                    ls_subcuenta    ,
                                    ls_grupo        ,
                                    HOY
                              INTO lr_saldo_dia.*

            END FOREACH -- Saldo al dia
            
            IF lr_disp.tipo_retiro = "E" THEN
                IF lr_disp.tipo_pension = "IP" AND lr_disp.porcentaje_val < 50 THEN
                    LET lr_saldo_dia.monto_acc  = 0
                    LET lr_saldo_dia.monto_pes  = 0
                END IF 
            END IF

            IF lr_saldo_dia.monto_acc > 0 AND lr_saldo_dia.monto_pes > 0 THEN
                CALL f_provisiona_subcta(lr_disp.curp           ,
                                         lr_disp.nss            ,
                                         ls_subcuenta           ,
                                         lr_disp.consecutivo    ,
                                         lr_saldo_dia.monto_acc ,
                                         lr_saldo_dia.monto_pes ,
                                         HOY                    ,
                                         ls_movimiento          ,
                                         lr_saldo_dia.siefore     )                
            END IF 
        END FOREACH -- Subcuentas del grupo del NSS
        
        UPDATE safre_tmp:tmp_disp_imss
        SET    estado_solicitud     = gr_edo.provisionado
        WHERE  nss                  = lr_disp.nss
        AND    consecutivo          = lr_disp.consecutivo       
        
    END FOREACH -- Provision por NSS
   
    

END FUNCTION 

#---------------------------------------------------------------------------#
# segundo_paso : Se afectan las tablas fisicas del proceso una vez que se   #
#                termino correctamente                                      #
#---------------------------------------------------------------------------#
FUNCTION segundo_paso()

    DEFINE li_folio LIKE ret_solicitud_tx.folio

    DEFINE
        lr_provision        ,
        lr_tmp_prov         RECORD LIKE dis_provision.*

    DEFINE lr_datos RECORD LIKE ret_solicitud_tx.*

    DEFINE lr_tots RECORD 
        ret_d           INTEGER     ,    
        ret_e           INTEGER     ,    
        total           INTEGER
    END RECORD
    
    DEFINE
        ls_edo_sol          SMALLINT

    -- -----------------------------------------------------------------------------

    LET li_folio        = f_ultimo_folio()
    LET lr_tots.ret_d   = 0
    LET lr_tots.ret_e   = 0
    LET lr_tots.total   = 0

    DISPLAY "CONSOLIDANDO EN TABLAS                     ...." AT 8,2
    DISPLAY "FOLIO :   ", li_folio AT 10,16
    DISPLAY "PROVISIONADOS" AT 12,6
    DISPLAY "RETIRO D   : ", lr_tots.ret_d  AT 13,10
    DISPLAY "RETIRO E   : ", lr_tots.ret_e  AT 14,10
    
    -- Copiamos la provision de la tabla temporal a la definitiva
    UPDATE safre_tmp:tmp_prov_disposicion
    SET    folio = li_folio
    WHERE  folio = 1

    INSERT INTO dis_provision
    SELECT *
    FROM   safre_tmp:tmp_prov_disposicion
    WHERE  folio = li_folio

    -- Actualiza el folio y el estado de la solicitud de los registros provisionados
    DECLARE cur_soli CURSOR FOR
    SELECT *
    FROM   safre_tmp:tmp_disp_imss
    WHERE  estado_solicitud = gr_edo.provisionado
    ORDER BY nss

    FOREACH cur_soli INTO lr_datos.*

        -- Actualizamos la tabla de solicitudes de transferencias
        UPDATE ret_solicitud_tx
        SET    folio            = li_folio              ,
               estado_solicitud = gr_edo.provisionado
        WHERE  nss              = lr_datos.nss
        AND    consecutivo      = lr_datos.consecutivo
        AND    estado_solicitud = gr_edo.confirmado

        CASE lr_datos.tipo_retiro
            WHEN "D"
                LET lr_tots.ret_d = lr_tots.ret_d + 1
                DISPLAY "RETIRO D   : ", lr_tots.ret_d  AT 13,10
            WHEN "E"
                LET lr_tots.ret_e = lr_tots.ret_e + 1
                DISPLAY "RETIRO E   : ", lr_tots.ret_e  AT 14,10
        END CASE

    END FOREACH
    
    DISPLAY "(TERMINADO)" AT 8,51
    PROMPT " PROCESO TERMINADO SATISFACTORIAMENTE ... <ENTER> PARA SALIR " FOR CHAR enter
    CLOSE WINDOW RETC7241

END FUNCTION

#---------------------------------------------------------------------------#
# f_despliega_info : Despliega la informacion de los registros a provisionar#
#---------------------------------------------------------------------------#
FUNCTION f_despliega_info()

    DEFINE lar_ret ARRAY[6] OF RECORD
        tipo_retiro        LIKE ret_trans_issste.tipo_retiro ,
        desc_retiro        LIKE tab_ret_issste.descripcion   ,
        num_cap            INTEGER                           ,
        num_prov           INTEGER
    END RECORD

    DEFINE lr_soli RECORD
        tipo_retiro        SMALLINT                                 ,
        edo_soli           LIKE ret_trans_issste.estado_solicitud   ,
        num_regs           INTEGER
    END RECORD

    DEFINE li_folio_trans  LIKE ret_trans_issste.folio

    DEFINE
        li_tot_prov         INTEGER

    DEFINE
        ls_ret              ,
        ls_cont             ,
        ls_flag             SMALLINT

    DEFINE
        lc_query            CHAR(1000)

    -- -----------------------------------------------------------------------------

    CALL f_abre_ventana()
    CALL f_valida_provisiona() RETURNING ls_flag, li_tot_prov

    IF ls_flag = 1 THEN
        IF li_tot_prov = 0 THEN
            CALL f_error_msg("NO EXISTEN REGISTROS PARA PROVISIONAR")
            LET ls_flag = 0
        ELSE
            WHILE TRUE    
                PROMPT "SE PROVISIONARAN ", li_tot_prov, " CUENTAS. ¿ESTA SEGURO? (S/N)  " FOR CHAR enter
                
                IF enter MATCHES "[sSnN]" THEN
                    IF enter MATCHES "[sS]" THEN
                        LET ls_flag = 1
                    ELSE
                        CALL f_error_msg("PROCESO CANCELADO")
                        LET ls_flag = 0
                    END IF
                
                    EXIT WHILE
                END IF
            END WHILE 
        END IF
    END IF 

    RETURN ls_flag

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_provisiona : Verifica si existen registros a provisionar. En     #
#                       caso de existir la funcion pasa las solicitudes a   #
#                       la tabla temporal y regresa el numero de solicitudes#
#---------------------------------------------------------------------------#
FUNCTION f_valida_provisiona()

    DEFINE
        li_tot_soli         INTEGER

    DEFINE
        ls_procesa          SMALLINT

    -- -----------------------------------------------------------------------------

    LET li_tot_soli = 0

    -- Pasamos todas las solicitudes en estado "Confirmado" a la tabla temporal
    INSERT INTO safre_tmp:tmp_disp_imss
    SELECT A.*
    FROM   ret_solicitud_tx A   ,
           ret_det_datamart B
    WHERE  A.estado_solicitud   = gr_edo.confirmado
    AND    A.tipo_retiro IN ("D", "E")               
    AND    A.nss                = B.nss
    AND    A.consecutivo        = B.id_registro



    SELECT COUNT(*)
    INTO   li_tot_soli
    FROM   safre_tmp:tmp_disp_imss

    IF li_tot_soli <= 0 THEN
        CALL f_error_msg("NO EXISTEN REGISTROS PARA PROVISIONAR")
        LET ls_procesa  = 0
    ELSE
        LET ls_procesa  = 1
    END IF

    RETURN ls_procesa, li_tot_soli

END FUNCTION

#---------------------------------------------------------------------------#
# f_provisiona_subcta : Inserta los montos que se usaran en la provision    #
#                       en la tabla temporal de provisiones                 #
#---------------------------------------------------------------------------#
FUNCTION f_provisiona_subcta(pr_provi, ps_sie)

    DEFINE pr_provi RECORD
        curp        LIKE ret_solicitud_tx.curp          ,
        nss         LIKE ret_solicitud_tx.nss           ,
        subcta      SMALLINT                            ,
        consec      LIKE ret_solicitud_tx.consecutivo   ,
        acciones    DECIMAL(16,6)                       ,
        pesos       DECIMAL(16,6)                       ,
        fecha_proc  DATE                                ,
        tipo_mov    SMALLINT
    END RECORD

    DEFINE lr_provision RECORD LIKE dis_provision.*
    
    DEFINE
        ps_sie              SMALLINT

    DEFINE
        ldt_fec_proc        DATE

    -- -----------------------------------------------------------------------------

    IF (pr_provi.subcta = 4) OR (pr_provi.subcta = 8) THEN
        LET ldt_fec_proc        = gdt_fecha_viv
        LET pr_provi.pesos      = f_redondea_val(pr_provi.pesos,2)   
        LET pr_provi.acciones   = f_redondea_val(pr_provi.acciones,2)
    ELSE
        LET ldt_fec_proc        = pr_provi.fecha_proc
    END IF

    LET lr_provision.tipo_movimiento    = pr_provi.tipo_mov  
    LET lr_provision.subcuenta          = pr_provi.subcta  
    LET lr_provision.siefore            = ps_sie           
    LET lr_provision.folio              = 1                
    LET lr_provision.consecutivo_lote   = pr_provi.consec  
    LET lr_provision.nss                = pr_provi.nss     
    LET lr_provision.curp               = pr_provi.curp    
    LET lr_provision.folio_sua          = NULL             
    LET lr_provision.fecha_pago         = HOY              
    LET lr_provision.fecha_valor        = ldt_fec_proc     
    LET lr_provision.fecha_conversion   = HOY              
    LET lr_provision.monto_en_pesos     = -pr_provi.pesos   
    LET lr_provision.monto_en_acciones  = -pr_provi.acciones
    LET lr_provision.precio_accion      = gar_precio_acc[ps_sie].precio_dia
    LET lr_provision.dias_cotizados     = 0         
    LET lr_provision.sucursal           = ""        
    LET lr_provision.id_aportante       = "RETIRO"  
    LET lr_provision.estado             = gr_edo.provisionado
    LET lr_provision.fecha_proceso      = HOY       
    LET lr_provision.usuario            = gc_usuario
    LET lr_provision.fecha_archivo      = HOY       
    LET lr_provision.etiqueta           = 1         

    INSERT INTO safre_tmp:tmp_prov_disposicion
    VALUES (lr_provision.*)

END FUNCTION

#---------------------------------------------------------------------------#
# f_abre_ventana : Abre la ventana donde se muestran los resultados de la   #
#                  provision de transferencias                              #
#---------------------------------------------------------------------------#
FUNCTION f_abre_ventana()

    OPEN WINDOW RETC7241 AT 4,4 WITH FORM "RETC7241" ATTRIBUTE(BORDER)
    DISPLAY " RETC724                                                                     " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY "           EJECUCION DE PROVISION DE DISPOSICIONES VENT 2.5                  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 1,67 ATTRIBUTE(REVERSE)

END FUNCTION


#---------------------------------------------------------------------------#
# f_error_msg : Formatea y despliega los mensajes de error en la pantalla   #
#---------------------------------------------------------------------------#
FUNCTION f_error_msg(pc_mensaje)

    DEFINE
        pc_mensaje          CHAR(76)

    LET pc_mensaje = " ", pc_mensaje CLIPPED , "... <ENTER> PARA SEGUIR  "
    PROMPT pc_mensaje ATTRIBUTE(REVERSE) FOR CHAR enter

END FUNCTION

#---------------------------------------------------------------------------#
# f_ultimo_folio : Obtiene el ultimo folio que se usara para procesar los   #
#                  registros de disposiciones                               #
#---------------------------------------------------------------------------#
FUNCTION f_ultimo_folio()

    DEFINE
        li_ult_folio     INTEGER


    SELECT MAX(A.folio) + 1
    INTO   li_ult_folio
    FROM   glo_folio A

    INSERT INTO glo_folio
    VALUES (li_ult_folio)

    RETURN li_ult_folio


END FUNCTION

#---------------------------------------------------------------------------#
# f_redondea_val : Redondea el monto dado por p_monto_redondear a tantos    #
#                  como se indique en p_redondea                            #
#---------------------------------------------------------------------------#
FUNCTION f_redondea_val(p_monto_redondear, p_redondea)

    DEFINE
        p_monto_redondear DECIMAL(16,6)

    DEFINE
        p_redondea       SMALLINT


    DEFINE
        ls_monto_return   DECIMAL(16,2)

    PREPARE round_id FROM "SELECT NVL(ROUND(?, ?),0) FROM systables WHERE tabid=1"
    DECLARE round_cur CURSOR FOR round_id

    OPEN round_cur USING p_monto_redondear, p_redondea
    FETCH round_cur INTO ls_monto_return

    CLOSE round_cur

    RETURN ls_monto_return


END FUNCTION


#---------------------------------------------------------------------------#
# f_obtiene_precios_accion : Obtiene los precios de accion a la fecha dada  #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_precios_accion(fecha_precios)

    DEFINE #loc #date
        fecha_precios         DATE

    DEFINE #loc #char
        v_precios_accion      CHAR(100)

    DEFINE lr_precio_acc RECORD #loc #lr_precio_acc Arreglo para los precios_accion
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE #loc #char
        lc_mensaje            CHAR(100) ,
        lc_siefore            CHAR(002)

    DEFINE #loc #smallint
        ls_sie                SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_sie = 1

    LET v_precios_accion = " EXECUTE FUNCTION fn_verifica_precio_accion(?)"
    PREPARE eje_precios_accion FROM v_precios_accion

    DECLARE c_precios CURSOR FOR eje_precios_accion
    FOREACH c_precios USING fecha_precios
                      INTO lr_precio_acc.*

        IF lr_precio_acc.siefore = gs_sieviv THEN

            LET ls_sie = gs_sieviv

            SELECT 0                ,
                   fecha_valuacion  ,
                   codigo_siefore   ,
                   precio_del_dia
            INTO   gar_precio_acc[ls_sie].*
            FROM   glo_valor_accion
            WHERE  fecha_valuacion = gdt_fecha_viv
            AND    codigo_siefore  = gs_sieviv

            IF STATUS = NOTFOUND THEN
                LET lc_mensaje = " FALTAN PRECIOS DE ACCION: FECHA ", gdt_fecha_viv USING "DD/MM/yyyy",
                                 " -- SIEFORE ", gs_sieviv CLIPPED
                PROMPT lc_mensaje FOR CHAR enter
                EXIT PROGRAM
            END IF
        ELSE

            IF lr_precio_acc.estado <> 0 THEN
                LET lc_siefore = lr_precio_acc.siefore

                LET lc_mensaje = " FALTAN PRECIOS DE ACCION: FECHA ", lr_precio_acc.fecha USING "DD/MM/yyyy",
                                 " -- SIEFORE ", lc_siefore CLIPPED

                PROMPT lc_mensaje FOR CHAR enter
                EXIT PROGRAM
            ELSE
                LET ls_sie                    = lr_precio_acc.siefore
                LET gar_precio_acc[ls_sie].*  = lr_precio_acc.*
            END IF

        END IF
    
    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# f_tablas_tmp : Genera las tablas temporales donde se almacenan los        #
#                calculos y cambios                                         #
#---------------------------------------------------------------------------#
FUNCTION f_tablas_tmp()

    DATABASE safre_tmp

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_disp_imss
        DROP TABLE tmp_prov_disposicion
    WHENEVER ERROR STOP

    --------------------------------

    SELECT *
    FROM   safre_af:ret_solicitud_tx
    WHERE  1 = 0
    INTO TEMP tmp_disp_imss
    
    --------------------------------

    CREATE TABLE tmp_prov_disposicion (
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

    CREATE INDEX tmp_prov_disposicion_1 ON tmp_prov_disposicion
        (folio,subcuenta,tipo_movimiento,estado)

    CREATE INDEX tmp_prov_disposicion_2 ON tmp_prov_disposicion
        (nss)

    CREATE INDEX tmp_prov_disposicion_3 ON tmp_prov_disposicion
        (folio,subcuenta)

    GRANT ALL ON tmp_prov_disposicion TO PUBLIC
    
    --------------------------------

    DATABASE safre_af

END FUNCTION


