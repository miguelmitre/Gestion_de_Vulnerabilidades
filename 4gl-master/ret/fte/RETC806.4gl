###############################################################################
#Proyecto          => SISTEMA DE AFORE( SAFRE )                               #
#Owner             => E.F.P.                                                  #
#Programa RETC806  => GENERA NOTIFICACION DE DISPOSICION DE RECUSROS SOLICITU #
#                     OP.05 POR RETIROS DE EDAD DEL TRABAJADOR SAR 92-97      #
#Fecha creacion    => 3 DE FEBRERO 2004                                       #
#By                => JOSE LUIS SALDIVAR CARDOSO                              #
#Fecha actualiza   => 07 DE FEBRERO DEL 2008                                  #
#Por               => FRANCO ULLOA VIDELA                                     #
#Sistema           => RET                                                     #
###############################################################################

DATABASE safre_af

GLOBALS
    DEFINE #glo #reg_6
        reg_6                 RECORD LIKE ret_solicitud_tx.*

    DEFINE #glo #param_ret
        g_param_ret           RECORD LIKE seg_modulo.*

    DEFINE reg_2 RECORD #glo #reg_2
        confirmado            LIKE ret_status.status ,
        procesado             LIKE ret_status.status ,
        provisionado          LIKE ret_status.status , #cambio
        enviado               LIKE ret_status.status
    END RECORD

    DEFINE reg_3 RECORD #reg_3
        fecha_val_acc         DATE,  #cambio nombre variable
        folio_oper_05         INTEGER
    END RECORD
    
    DEFINE reg_12 RECORD #glo #reg_12
        estado                SMALLINT ,
        fecha_rcv             DATE     ,
        fecha_viv             DATE
    END RECORD

    DEFINE #glo #date
        cod_resp_fv           ,
        HOY                   DATE

    DEFINE #glo #char
        v_valida_precio       CHAR(200) ,
        comando               CHAR(110) ,
        v_precios_accion      CHAR(100) ,
        c11_id_aportante      CHAR(011) ,
        v_provisiona          CHAR(150) ,
        v_saldo_dia           CHAR(100) ,
        v_obten_fecha         CHAR(100) ,
        enter                 CHAR(001) ,
        HORA                  CHAR(005) ,
        usuario                         ,
        G_LISTA_DET           CHAR(100) ,
        v_marca               CHAR(100)

    DEFINE #glo #smallint
        gs_num_siefores       ,--Indica el numero de siefores que se usan actualmente
        cod_resp              ,
        cod_resp_r            ,
        v_cod_rechazo         ,
        s_tipo_movimiento     ,
        s_codigo_afore        ,
        sw_1                  SMALLINT

    DEFINE #glo #integer
        ultimo_folio          ,
        cont_1                ,
        cont_tot              ,
        cont_reg              INTEGER

    DEFINE #glo #decimal
        d6_tot_pesos_viv      DECIMAL(16,06)

END GLOBALS


MAIN

    DEFINE lr_precio_acc RECORD
        estado      SMALLINT     ,
        fecha       DATE         ,
        siefore     SMALLINT     ,
        precio_dia  DECIMAL(16,6)
    END RECORD

    DEFINE lc_mensaje CHAR(100)

    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I

    CALL f_lib_abre_log("RETC806")

    CALL init()
    OPEN WINDOW RETC8061 AT 4,4 WITH FORM "RETC8061" ATTRIBUTE(BORDER)
    DISPLAY " RETC806     NOTIFICA Y PROVISIONA DISPOSICION DE RECURSOS                     " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY " < Ctrl-C > Salir                                           TIPO RETIRO H      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,64 ATTRIBUTE(REVERSE)

    WHILE TRUE
        PROMPT " ESTA SEGURO S/N ? " FOR CHAR enter
        IF enter MATCHES "[sSnN]" THEN
            IF enter MATCHES "[sS]" THEN
            
                LET v_valida_precio = " EXECUTE FUNCTION fn_valida_existe_precio() "
                PREPARE eje_valida_precio FROM v_valida_precio
                
                DECLARE cur_sp1 CURSOR FOR eje_valida_precio
                OPEN cur_sp1
                    FETCH cur_sp1 INTO reg_12.estado    ,
                                       reg_12.fecha_rcv ,
                                       reg_12.fecha_viv 
                CLOSE cur_sp1
                          
                IF reg_12.estado = 1 THEN
                    PROMPT " FALTA PRECIO DE ACCION FECHA ",reg_12.fecha_rcv USING "DD-MM-yyyy",", VIV:",reg_12.fecha_viv USING"DD-MM-YYYY"
                    FOR CHAR ENTER
                    EXIT PROGRAM 
                END IF
                
                EXIT WHILE
            ELSE
                PROMPT" PROCESO CANCELADO...<ENTER> PARA CONTINUAR "
                FOR CHAR enter
                EXIT PROGRAM
            END IF
        END IF
    END WHILE

    DISPLAY " PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)

    CALL primer_paso()  #pp #CALCULA SALDOS
    CALL segundo_paso() #sp #GENERA PLANO

    DISPLAY "TOTAL DE REGISTROS A ENVIAR     : ",cont_reg         AT 11,19

    DISPLAY " FOLIO NUMERO : ",ultimo_folio  AT 18,1
    PROMPT " PROCESO FINALIZADO... < ENTER > PARA SALIR " FOR CHAR enter

    CLOSE WINDOW RETC8061
END MAIN


FUNCTION init()
#--------------
    LET HOY              = TODAY
    LET HORA             = TIME
    LET c11_id_aportante = "RETIRO"

    #-- Obtenemos el numero de siefores actual en el sistema
    SELECT COUNT(*)
    INTO   gs_num_siefores
    FROM   tab_siefore_local
    WHERE  codigo_siefore > 0
    AND    codigo_siefore <> 11

    SELECT codigo_afore   ,
           USER
    INTO   s_codigo_afore ,
           usuario
    FROM   tab_afore_local

    SELECT *
    INTO   g_param_ret.*
    FROM   seg_modulo
    WHERE  modulo_cod = 'ret'

    SELECT A.status
    INTO   reg_2.confirmado
    FROM   ret_status A
    WHERE  A.descripcion = "CONFIRMADO"

    SELECT A.status
    INTO   reg_2.procesado
    FROM   ret_status A
    WHERE  A.descripcion = "PROCESADO"

    SELECT A.status
    INTO   reg_2.enviado
    FROM   ret_status A
    WHERE  A.descripcion = "ENVIADO"

    SELECT "OK"
    FROM   ret_solicitud_tx A
    WHERE  A.tipo_retiro      = "H"
    AND    A.estado_solicitud = reg_2.confirmado
    GROUP BY 1

    IF SQLCA.SQLCODE <> NOTFOUND THEN
       SELECT MAX(A.folio) + 1
       INTO   ultimo_folio
       FROM   glo_folio  A
       INSERT INTO glo_folio VALUES (ultimo_folio)
    END IF

    LET reg_3.fecha_val_acc  = HOY
    LET reg_3.folio_oper_05  = ultimo_folio

    LET v_marca = " EXECUTE PROCEDURE marca_cuenta (?,?,?,?,?,?,?,?)"

    LET v_cod_rechazo = 0

    LET v_saldo_dia = " EXECUTE FUNCTION fn_saldo_dia_asc ( ?,?,?,? )"
    PREPARE eje_saldo_dia_asc FROM v_saldo_dia

    LET v_provisiona = "EXECUTE FUNCTION fn_prov_cargo (?,?,?,?,?,?,?,?,?,?,?) "
    PREPARE eje_provisiona FROM v_provisiona

    LET s_tipo_movimiento = 0

    SELECT movimiento
    INTO  s_tipo_movimiento
    FROM  tab_retiro
    WHERE tipo_retiro = 'H'

    LET v_obten_fecha = "EXECUTE FUNCTION fn_obten_fecha_val ( ? )"
    PREPARE eje_obten_fecha FROM v_obten_fecha

    DECLARE cur_obten_fecha CURSOR FOR eje_obten_fecha
    OPEN cur_obten_fecha   USING HOY
        FETCH cur_obten_fecha INTO cod_resp_fv
    CLOSE cur_obten_fecha

END FUNCTION


FUNCTION primer_paso()
#pp--------------------
    DEFINE reg_5 RECORD #loc #reg_5
        nss                   LIKE ret_solicitud_tx.nss             ,
        consecutivo           LIKE ret_solicitud_tx.consecutivo     ,
        regimen               LIKE ret_solicitud_tx.regimen         ,
        tipo_seguro           LIKE ret_solicitud_tx.tipo_seguro     ,
        tipo_pension          LIKE ret_solicitud_tx.tipo_pension    ,
        tipo_prestacion       LIKE ret_solicitud_tx.tipo_prestacion ,
        folio_solicitud       LIKE ret_solicitud_tx.folio_solicitud ,
        grupo                 LIKE ret_solicitud_tx.grupo 
    END RECORD

    DEFINE arr_siefore ARRAY [20] OF RECORD
                                        activo            SMALLINT,
                                        acciones_ret92    DECIMAL(16,6)
                                    END RECORD

    DEFINE #loc #char
        var_nula         CHAR(06)

    DEFINE #loc #smallint
        ls_siefore            , #-- contador para los ciclos for
        f_siefore             ,
        f_subcuenta           ,
        s_grupo               ,
        s_subcta              SMALLINT

    DEFINE #loc  #dec
        f_monto_pes           ,
        f_monto_acc           ,
        s11_acc_viv92         DECIMAL(16,6)

    LET cont_tot      = 0

    SELECT COUNT(*)
    INTO   cont_tot
    FROM   ret_solicitud_tx
    WHERE  estado_solicitud = reg_2.confirmado
    AND    tipo_prestacion  = 9
    AND    rechazo_cod      = 0

    IF cont_tot = 0 THEN
        PROMPT " NO HAY REGISTROS PARA PROCESAR...<ENTER> PARA SALIR "
        FOR enter
        EXIT PROGRAM
    END IF

    DECLARE cur_3 CURSOR FOR
    SELECT  nss                ,
            consecutivo        ,
            regimen            ,
            tipo_seguro        ,
            tipo_pension       ,
            tipo_prestacion    ,
            folio_solicitud    ,
            grupo
    FROM    ret_solicitud_tx A
    WHERE   A.estado_solicitud    = reg_2.confirmado
    AND     A.tipo_prestacion     = 9
    AND     A.rechazo_cod         = 0
    ORDER BY nss

    LET cont_1 = 0
    FOREACH cur_3 INTO reg_5.*

        #-- Inicializamos variables del arreglo
        FOR ls_siefore = 1 TO gs_num_siefores
            LET arr_siefore[ls_siefore].activo             = FALSE
            LET arr_siefore[ls_siefore].acciones_ret92     = 0
        END FOR

        LET s11_acc_viv92            = 0

        DECLARE cur_4 CURSOR FOR
        SELECT A.subcuenta
        FROM   tab_agrupa_subcta A
        WHERE  A.grupo     = reg_5.grupo
        AND    A.subcuenta > 0

        FOREACH cur_4 INTO s_subcta
            LET f_subcuenta = 0
            LET f_siefore   = 0
            LET f_monto_acc = 0
            LET f_monto_pes = 0

            LET s_grupo = 0
            DECLARE c_saldo CURSOR FOR eje_saldo_dia_asc
            FOREACH c_saldo USING reg_5.nss               ,#nss
                                  s_subcta                ,#subcuenta
                                  s_grupo                 ,#grupo
                                  HOY                      #fecha_saldo
                             INTO f_subcuenta             ,
                                  f_siefore               ,
                                  f_monto_acc             ,
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
                        WHEN 7
                            LET arr_siefore[f_siefore].acciones_ret92 = f_monto_acc
                        OTHERWISE
                            #-- Para evitar que se provisionen subcuentas que no corresponden a este tipo de retiro
                            LET f_monto_acc = 0
                    END CASE
                ELSE
                    IF f_subcuenta = 8 THEN
                        LET s11_acc_viv92 = f_monto_acc
                    END IF
                END IF

               ---- Provisión---
                IF f_monto_acc > 0 THEN
                    LET var_nula    = ""
                    LET f_monto_acc = -f_monto_acc
                    LET f_monto_pes = -f_monto_pes

                    DECLARE cur_prov CURSOR FOR eje_provisiona
                    OPEN cur_prov  USING reg_3.folio_oper_05,#folio
                                         var_nula           ,#folio_sua
                                         reg_5.nss          ,#nss
                                         f_subcuenta        ,#subcuenta
                                         s_tipo_movimiento  ,#tipo_movimiento
                                         reg_5.consecutivo  ,#consecutivo_lote
                                         f_siefore          ,#siefore
                                         f_monto_acc        ,#monto_en_acciones
                                         f_monto_pes        ,#monto_en_pesos
                                         c11_id_aportante   ,#id_aporte
                                         HOY                 #fecha_proceso
                    FETCH cur_prov INTO cod_resp
                    CLOSE cur_prov
                ELSE
                    LET f_monto_acc = 0
                END IF
            END FOREACH
        END FOREACH

        LET cont_1 = cont_1 + 1
        DISPLAY "TOTAL REGISTROS PROCESADOS     : ",cont_1  AT 13,20

        FOR ls_siefore = 1 TO gs_num_siefores
            #-- Almacenamos solo los registros de las siefores que contengan saldo
            IF arr_siefore[ls_siefore].activo = TRUE THEN

                IF arr_siefore[ls_siefore].acciones_ret92 < 0 THEN
                    LET arr_siefore[ls_siefore].acciones_ret92 = 0
                END IF

                INSERT INTO ret_monto_siefore
                    VALUES(reg_5.nss                             ,--nss
                           reg_5.consecutivo                     ,--consecutivo
                           reg_3.folio_oper_05                   ,--folio
                           "H"                                   ,--tipo_retiro
                           5                                     ,--tipo_operacion
                           ls_siefore                            ,--siefore
                           0                                     ,--acciones_ret97
                           0                                     ,--acciones_cv
                           0                                     ,--acciones_cs
                           arr_siefore[ls_siefore].acciones_ret92 --acciones_ret92
                          )
            END IF
        END FOR

        INSERT INTO ret_monto_viv
        VALUES(reg_5.nss           ,--nss
               reg_5.consecutivo   ,--consecutivo
               reg_3.folio_oper_05 ,--folio
               "H"                 ,--tipo_retiro
               cod_resp_fv         ,--fecha_valor_viv
               0                   ,--acciones_viv97
               s11_acc_viv92       ,--acciones_viv92
               0                   ,--pesos_viv72
               NULL                ,--estado_sub_viv
               0                   ,--acc_viv97_bdsviv
               0                    --acc_viv92_bdsviv
              )
              
        UPDATE ret_solicitud_tx
        SET    ret_solicitud_tx.folio               = reg_3.folio_oper_05 ,
               ret_solicitud_tx.saldo_viv97         = 0                   ,
               ret_solicitud_tx.saldo_viv92         = s11_acc_viv92       ,
               ret_solicitud_tx.fecha_envio         = HOY                 ,
               ret_solicitud_tx.fecha_valor_viv     = cod_resp_fv         ,
               ret_solicitud_tx.estado_solicitud    = reg_2.procesado
        WHERE  ret_solicitud_tx.nss                 = reg_5.nss
        AND    ret_solicitud_tx.consecutivo         = reg_5.consecutivo
        AND    ret_solicitud_tx.estado_solicitud    IN (0,3)
    END FOREACH

    CALL carga_his_saldos(""                   ,
                          reg_3.fecha_val_acc  ,
                          d6_tot_pesos_viv
                         ) #chs

END FUNCTION


FUNCTION segundo_paso()
#sp-------------------

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

    DEFINE #loc #reg_2
        s_orden_de_envio      SMALLINT


    #-- Determinamos la ubicacion de los reportes
    LET ruta_det_nss = g_param_ret.ruta_envio CLIPPED, "/", "DET-NSS-H-tmp"
    LET ruta_det_nss = ruta_det_nss CLIPPED

    LET ruta_det_sie = g_param_ret.ruta_envio CLIPPED, "/", "DET-SIE-H-tmp"
    LET ruta_det_sie = ruta_det_sie CLIPPED

    LET ruta_det_viv = g_param_ret.ruta_envio CLIPPED, "/", "DET-VIV-H-tmp"
    LET ruta_det_viv = ruta_det_viv CLIPPED

    LET G_LISTA_DET = g_param_ret.ruta_envio CLIPPED, "/", "DET-H"
    LET G_LISTA_DET = G_LISTA_DET CLIPPED

    LET cont_reg           = 0
    LET sw_1               = 0
    
    DECLARE cur_5 CURSOR FOR
    SELECT  *
    FROM    ret_solicitud_tx
    WHERE   ret_solicitud_tx.folio              = reg_3.folio_oper_05
    AND     ret_solicitud_tx.tipo_prestacion    = 9
    AND     ret_solicitud_tx.estado_solicitud   = reg_2.procesado

    FOREACH cur_5 INTO reg_6.*
        
        #-- Iniciamos los reportes
        START REPORT det_solicitudes_03 TO ruta_det_nss
        START REPORT det_vivienda_05 TO ruta_det_viv    
        
        LET sw_1          = 1
        LET cont_reg      = cont_reg + 1
        LET ls_num_montos = 0

        IF reg_6.acciones_ret92 IS NULL THEN
	       LET reg_6.acciones_ret92 = 0
        END IF

        IF reg_6.saldo_viv92 IS NULL OR
           reg_6.saldo_viv92 < 0     THEN
            LET reg_6.saldo_viv92 = 0
        END IF

        OUTPUT TO REPORT det_solicitudes_03(reg_6.*)
        OUTPUT TO REPORT det_vivienda_05(reg_6.*)

        SELECT  COUNT(*)
            INTO   ls_num_montos            
            FROM    ret_monto_siefore
            WHERE   folio          = reg_3.folio_oper_05
            AND     nss            = reg_6.nss
            AND     consecutivo    = reg_6.consecutivo
            AND     tipo_operacion = 5

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
            FROM    ret_monto_siefore
            WHERE   folio          = reg_3.folio_oper_05
            AND     nss            = reg_6.nss
            AND     consecutivo    = reg_6.consecutivo
            AND     tipo_operacion = 5

            FOREACH cur_6 INTO reg_9.*
                OUTPUT TO REPORT det_siefores_04(reg_6.nss, reg_6.curp, reg_9.*)
            END FOREACH

            FINISH REPORT det_siefores_04
        END IF

        FINISH REPORT det_solicitudes_03
        FINISH REPORT det_vivienda_05

        #-- Una vez generados los archivos para el nss actual, los concatenamos en uno solo
        CALL concat_reportes(ruta_det_nss,
                             ruta_det_sie,
                             ruta_det_viv,
                             ls_num_montos,
                             cont_reg)
        
        END FOREACH
   
    LET comando = "chmod 777 ", G_LISTA_DET CLIPPED
    RUN comando

    IF sw_1 = 1 THEN
        INSERT INTO ret_ctr_envio_lote
        VALUES (HOY                   ,#fecha_genera
                'H'                   ,#tipo_retiro
                reg_3.folio_oper_05   ,#folio
                ' '                   ,#fecha_envio
                ' '                   ,#fecha_reverso
                HORA                  ,#hora_genera
                ' '                   ,#hora_envio
                usuario               ,#usuario_genera
                ' '                   ,#usuario_envio
                ' '                   ,#usuario_reverso
                reg_2.procesado       ,#estado
                cont_1                 #total_registros
                )
    END IF

END FUNCTION


FUNCTION carga_his_saldos(inf)
#chs--------------------------
    DEFINE inf RECORD
        fecha_archivo         DATE          ,
        fecha_provision       DATE          ,
        total_importe_viv     DECIMAL(16,6)
    END RECORD

    SELECT "OK"
    FROM   ret_his_dep_retiro A
    WHERE  A.folio     = reg_3.folio_oper_05
    AND    A.subcuenta = 8

    #-- Comentario por Javier
    #-- Ver si se tiene que agregar al reverso el borrar este registro
    IF STATUS = NOTFOUND THEN
        INSERT INTO ret_his_dep_retiro
            VALUES (reg_3.folio_oper_05   ,
                    inf.fecha_archivo     ,
                    inf.fecha_provision   ,
                    ""                    ,
                    8                     ,
                    inf.total_importe_viv ,
                    ""                    ,
                    ""                    ,
                    usuario               ,
                    7
                   )
    END IF
END FUNCTION


#----------------------------------------------------------------------------------
# Esta funcion, dadas las rutas de los tres archivos de detalle temporales de los
# reportes, los va concatenando en uno solo que sera el archivo de detalle final.
#----------------------------------------------------------------------------------
FUNCTION concat_reportes(lc_det_3, lc_det_4, lc_det_5, p_monto, p_regs)

    DEFINE
        lc_det_3      ,
        lc_det_4      ,
        lc_det_5      CHAR(100)

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
    LET com_rm = "rm ", lc_det_3 CLIPPED, " ", lc_det_5 CLIPPED

    #-- Si se genero el reporte de saldo de siefores, se incluye en los comandos
    IF p_monto > 0 THEN

        LET com_cat = "cat ", lc_det_3 CLIPPED, " ",
                              lc_det_4 CLIPPED, " ",
                              lc_det_5 CLIPPED, " > ", ruta_tmp

        LET com_cat = com_cat CLIPPED

        LET com_rm = com_rm CLIPPED, " ", lc_det_4 CLIPPED
    ELSE
        LET com_cat = "cat ", lc_det_3 CLIPPED, " ",
                              lc_det_5 CLIPPED, " > ", ruta_tmp CLIPPED
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



REPORT det_solicitudes_03(reg_7)
#ds05----------------------------

    DEFINE #loc #reg_7
        reg_7                 RECORD LIKE ret_solicitud_tx.*

    DEFINE reg_8 RECORD  #loc #reg_8
        paterno               LIKE afi_mae_afiliado.paterno,
        materno               LIKE afi_mae_afiliado.materno,
        nombres               LIKE afi_mae_afiliado.nombres
    END RECORD

    OUTPUT
        PAGE LENGTH   1
	    LEFT MARGIN   0
	    RIGHT MARGIN  0
	    TOP MARGIN    0
	    BOTTOM MARGIN 0

    FORMAT
        ON EVERY ROW

            SELECT paterno   ,
                   materno   ,
                   nombres
            INTO   reg_8.paterno,
                   reg_8.materno,
                   reg_8.nombres
            FROM   afi_mae_afiliado
            WHERE  n_seguro  = reg_7.nss

        PRINT
            COLUMN 001,"03"                            ,--tipo_registro
            COLUMN 003,"04"                            ,--ident_servicio
            COLUMN 005,"05"                            ,--ident_operacion
            COLUMN 007,reg_7.nss                       ,--nss
            COLUMN 018,reg_7.curp                      ,--curp
            COLUMN 036,reg_8.nombres                   ,--nombre
            COLUMN 076,reg_8.paterno                   ,--apellido paterno
            COLUMN 116,reg_8.materno                   ,--apellido materno
            COLUMN 158,reg_7.tipo_retiro               ,--tipo_retiro
            COLUMN 159,reg_7.regimen                   ,--regimen
            COLUMN 161,reg_7.tipo_seguro               ,--tipo_seguro
            COLUMN 163,reg_7.tipo_pension              ,--tipo_pension
            COLUMN 165,reg_7.tipo_prestacion USING "&&",--tipo_prestacion
            COLUMN 167,"00010101"                      ,--fecha_ini_pension
            COLUMN 175,"00010101"                      ,--fecha_resolucion
            COLUMN 183,"00000"                         ,--porcentaje val
            COLUMN 188,"0000"                          ,--sem cotizadas
            COLUMN 192,reg_7.fecha_solicitud           USING "YYYYMMDD",
            COLUMN 200,reg_7.cve_doc_probatorio        ,
            COLUMN 201,reg_7.fecha_nacimiento          USING "YYYYMMDD",
            COLUMN 227,"000000"                        ,--periodo pago
            COLUMN 233,reg_7.consecutivo               USING "&&&&&&&&&&&",
            COLUMN 244, 537 SPACES                     -- Se modifica para hacer la longitud de acuerdo al cambio de layout

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



