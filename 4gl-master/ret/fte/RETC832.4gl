#################################################################################
#Proyecto         => SISTEMA SAFRE ( MEXICO )                                   #
#Owner            => E.F.P.                                                     #
#Programa RETC832 => PROCESO DE TRANSFERENCIA DE TRASPASOS HISTORICOS (oper.20) #
#By               => FRANCO ESTEBAN ULLOA VIDELA                                #
#Fecha            => 23 SEPTIEMBRE DEL 2004                                     #
#Actualizacion    => FRANCO ESTEBAN ULLOA VIDELA                                #
#Fecha            => 09 DE MARZO DEL 2008                                       #
#Actualizacion    => JAVIER GONZALEZ JERONIMO                                   #
#Fecha            => 25 DE MAYO DE 2009                                         #
#                 => Se agrego el tipo de movimiento 294 al proceso             #
#Actualizacion    => JAVIER GONZALEZ JERONIMO                                   #
#Fecha            => 27 DE ABRIL DE 2010                                        #
#                 => Se realizan mejoras la codigo. Se modifica la generacion   #
#                    del reporte de la op. 20 para que el nombre se genere con  #
#                    el usuario actual para evitar conflictos de permisos       #
#Sistema          => RET                                                        #
#################################################################################
DATABASE safre_af

GLOBALS
    DEFINE gar_precio_acc ARRAY [20] OF RECORD
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE gr_modulo RECORD LIKE seg_modulo.*

    DEFINE gr_edo RECORD
        recibido            LIKE ret_estado.estado_solicitud ,
        enviado             LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE gar_saldos ARRAY[9] OF RECORD
        siefore               SMALLINT      ,
        subcuenta             SMALLINT      ,
        acc_traspaso          DECIMAL(16,6) ,
        aporte                DECIMAL(16,6) ,
        porcentaje_pago       DECIMAL(5,2)
    END RECORD

    DEFINE
        gdt_ult_traspaso_rcv        ,
        gdt_ult_traspaso_viv        ,
        HOY                         DATE

    DEFINE
        ch                          CHAR(150) ,
        gc_usuario                  CHAR(008) ,
        enter                       CHAR(001) ,
        c12_nom_plano               CHAR(012) ,
        borra_lineas                CHAR(200) ,
        G_LISTA_DET2                CHAR(100)

    DEFINE
        gs_procesa                  ,
        gs_cod_afore                SMALLINT

    DEFINE
        gi_ult_folio                INTEGER

END GLOBALS

-- -----------------------------------------------------------------------------

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG ("RETC832.log")

    CALL init()
    CALL f_captura_folio() RETURNING gs_procesa     ,
                                     gi_ult_folio
    
    IF gs_procesa THEN
        CALL primer_paso(gi_ult_folio)  #-- Genera datos
        CALL segundo_paso(gi_ult_folio) #-- Genera el archivo plano
        CALL tercer_paso(gi_ult_folio)  #-- Almacena la informacion en las tablas definitivas
    END IF
    
    CLOSE WINDOW retc8321

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    DEFINE
        lc_prepare      CHAR(300)

    
    LET HOY = TODAY

    ----- CODIGOS AFORES -----
    SELECT codigo_afore   ,
           USER
    INTO   gs_cod_afore ,
           gc_usuario
    FROM   tab_afore_local

    ----- RUTAS DE ARCHIVOS -----
    SELECT *
    INTO   gr_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.recibido
    FROM   ret_estado A
    WHERE  A.descripcion = "RECEPCIONADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.enviado
    FROM   ret_estado A
    WHERE  A.descripcion = "ENVIADO"

    ----- ULTIMO FOLIO DE OPERACION 19 -----
    SELECT MAX(folio)
    INTO   gi_ult_folio
    FROM   ret_sal_his_rx
    WHERE  estado_solicitud = gr_edo.recibido

    ----- OBTIENE TRASPASOS ENTRE SIEFORES -----
    LET lc_prepare = " EXECUTE PROCEDURE sp_traspaso_sie(?,?,?) "
    PREPARE eje_traspaso FROM lc_prepare

    LET lc_prepare = " "

END FUNCTION

#---------------------------------------------------------------------------#
# primer_paso : Genera los montos calculados en base a los porcentajes y    #
#               los almacena en la tabla ret_sal_his_tx2                    #
#---------------------------------------------------------------------------#
FUNCTION primer_paso(pi_folio)

    DEFINE pi_folio LIKE ret_sal_his_rx.folio

    DEFINE lr_sal_hist RECORD
        nss                   LIKE ret_sal_his_rx.nss            ,
        consecutivo           LIKE ret_sal_his_rx.consecutivo    ,
        folio                 LIKE ret_sal_his_rx.folio          ,
        fecha_ini_pen         LIKE ret_sal_his_rx.fecha_ini_pen  ,
        porcentaje_val        LIKE ret_sal_his_rx.porcentaje_val ,
        regimen               LIKE ret_sal_his_rx.regimen        ,
        tipo_seguro           LIKE ret_sal_his_rx.tipo_seguro    ,
        tipo_pension          LIKE ret_sal_his_rx.tipo_pension
    END RECORD

    DEFINE
        ldt_valor_viv           DATE

    DEFINE
        ls_subcta               ,
        ls_cont                 SMALLINT

    -- -----------------------------------------------------------------------------

    DISPLAY " PROCESANDO INFORMACION               " AT 19,1 ATTRIBUTE(REVERSE)
    
    CALL f_tablas_tmp(pi_folio)

    DECLARE cur_sal CURSOR WITH HOLD FOR
    SELECT nss            ,
           consecutivo    ,
           folio          ,
           fecha_ini_pen  ,
           porcentaje_val ,
           regimen        ,
           tipo_seguro    ,
           tipo_pension
    FROM   ret_sal_his_rx
    WHERE  folio            = pi_folio
    AND    estado_solicitud = gr_edo.recibido

    FOREACH cur_sal INTO lr_sal_hist.*
        FOR ls_cont = 1 TO 9
            LET gar_saldos[ls_cont].aporte          = 0
            LET gar_saldos[ls_cont].acc_traspaso    = 0
            LET gar_saldos[ls_cont].porcentaje_pago = 0
        END FOR

        CALL f_genera_tmp_cuenta(lr_sal_hist.nss)
        
        -- Obtenemos la fecha del ultimo traspaso de vivienda como afore cedente                                              
        SELECT MAX (A.fecha_conversion)
        INTO   gdt_ult_traspaso_viv
        FROM   tmp_dis_cuenta A
        WHERE  A.nss              = lr_sal_hist.nss
        AND    A.subcuenta        = 4
        AND    A.tipo_movimiento  IN (220,240,247,290,294,295)


        -- Obtenemos la fecha del ultimo traspaso de RCV como afore cedente
        LET gdt_ult_traspaso_rcv = "01-01-0001"

        SELECT MAX(A.fecha_conversion)
        INTO   gdt_ult_traspaso_rcv
        FROM   tmp_dis_cuenta A
        WHERE  A.nss              = lr_sal_hist.nss
        AND    A.subcuenta       IN (1,2,5,6,9)
        AND    A.tipo_movimiento IN (220,240,247,290,294,295)
        
        -- Minima fecha valor de vivienda del último traspaso de RCV                                               
        LET ldt_valor_viv = "01-01-0001"
        
        SELECT MIN(A.fecha_valor)
        INTO   ldt_valor_viv
        FROM   tmp_dis_cuenta A
        WHERE  A.nss              = lr_sal_hist.nss
        AND    A.subcuenta       IN (1,2,4,5,6,9)
        AND    A.tipo_movimiento IN (220,240,247,290,294,295)
        AND    A.fecha_conversion = gdt_ult_traspaso_rcv

        -- Calcula el porcentaje a pagar de los montos traspasados
        CALL f_aporte_menores(lr_sal_hist.nss            ,
                              lr_sal_hist.fecha_ini_pen  ,
                              gdt_ult_traspaso_rcv   ,
                              gdt_ult_traspaso_viv   ,
                              lr_sal_hist.porcentaje_val ,
                              lr_sal_hist.regimen        ,
                              lr_sal_hist.tipo_seguro    ,
                              lr_sal_hist.tipo_pension
                             )

        IF lr_sal_hist.fecha_ini_pen < "07/01/1997" THEN
            CONTINUE FOREACH
        END IF

        FOR ls_subcta = 1 TO 9
            IF ls_subcta = 3 OR ls_subcta = 7 OR ls_subcta = 8 THEN
                CONTINUE FOR
            END IF

            IF gar_saldos[ls_subcta].acc_traspaso    > 0 OR
               gar_saldos[ls_subcta].porcentaje_pago > 0 THEN

                INSERT INTO tmp_sal_his_tx2
                VALUES(lr_sal_hist.nss                       ,-- nss
                       lr_sal_hist.consecutivo               ,-- consecutivo
                       lr_sal_hist.folio                     ,-- folio
                       gar_saldos[ls_subcta].siefore         ,-- siefore
                       gar_saldos[ls_subcta].subcuenta       ,-- subcuenta
                       gdt_ult_traspaso_rcv                  ,-- fecha_traspaso
                       ldt_valor_viv                         ,-- fecha_valor_viv
                       gar_saldos[ls_subcta].acc_traspaso    ,-- acc_traspaso
                       gar_saldos[ls_subcta].aporte          ,-- acc_posteriores
                       gar_saldos[ls_subcta].porcentaje_pago  -- porcentaje_pago
                      )
            END IF
        END FOR

        UPDATE tmp_sal_his_rx
        SET    estado_solicitud = gr_edo.enviado
        WHERE  nss              = lr_sal_hist.nss
        AND    consecutivo      = lr_sal_hist.consecutivo
    
    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# segundo_paso : Genera el archivo plano de la operacion 20                 #
#---------------------------------------------------------------------------#
FUNCTION segundo_paso(pi_folio)

    DEFINE pi_folio LIKE ret_sal_his_rx.folio
    
    DEFINE lr_his_rx RECORD LIKE ret_sal_his_rx.*
    
    DEFINE lr_val_tx RECORD
        fecha_valor_viv        LIKE ret_sal_his_tx.fecha_valor_viv    ,
        porcentaje_97          LIKE ret_sal_his_tx.porcentaje_97      ,
        porcentaje_cv          LIKE ret_sal_his_tx.porcentaje_cv      ,
        porcentaje_cuo_so      LIKE ret_sal_his_tx.porcentaje_cuo_so  ,
        porcentaje_viv97       LIKE ret_sal_his_tx.porcentaje_viv97
    END RECORD

    DEFINE
        ls_diag_envio               SMALLINT

    DEFINE
        lc_reporte                  CHAR(100)

    -- -----------------------------------------------------------------------------

    DISPLAY " GENERANDO ARCHIVO PLANO              " AT 19,1 ATTRIBUTE(REVERSE)

    DECLARE cur_rx CURSOR FOR
    SELECT A.*,
           " ",
           0  ,
           0  ,
           0  ,
           0
    FROM   ret_sal_his_rx A
    WHERE  A. folio      = pi_folio

    LET lc_reporte = gr_modulo.ruta_envio CLIPPED, "/", gc_usuario CLIPPED, ".DET-20"

    START REPORT r_operacion_20 TO lc_reporte

    FOREACH cur_rx INTO lr_his_rx.*

        SELECT UNIQUE(fecha_valor_viv)
        INTO   lr_val_tx.fecha_valor_viv
        FROM   tmp_sal_his_tx2
        WHERE  folio       = lr_his_rx.folio
        AND    nss         = lr_his_rx.nss
        AND    consecutivo = lr_his_rx.consecutivo

        IF lr_val_tx.fecha_valor_viv IS NULL OR lr_val_tx.fecha_valor_viv = " " THEN
            LET lr_val_tx.fecha_valor_viv = "01/01/0001"
        END IF

        CALL f_obten_porcentajes(lr_his_rx.folio, lr_his_rx.nss, lr_his_rx.consecutivo)
            RETURNING lr_val_tx.porcentaje_97       ,     
                      lr_val_tx.porcentaje_cv       ,
                      lr_val_tx.porcentaje_cuo_so   , 
                      lr_val_tx.porcentaje_viv97

        LET ls_diag_envio = 501

        IF  (lr_val_tx.porcentaje_97 = 0 ) AND (lr_val_tx.porcentaje_cv = 0 ) AND 
            (lr_val_tx.porcentaje_cuo_so = 0 ) AND (lr_val_tx.porcentaje_viv97  = 0) THEN
            
            LET ls_diag_envio = 505

        END IF

        UPDATE  tmp_sal_his_rx
        SET     diag_envio      = ls_diag_envio
        WHERE   nss             = lr_his_rx.nss
        AND     consecutivo     = lr_his_rx.consecutivo

        LET lr_his_rx.diag_envio = ls_diag_envio
        
        OUTPUT TO REPORT r_operacion_20(pi_folio, lr_his_rx.*, lr_val_tx.*)

    END FOREACH
    
    FINISH REPORT r_operacion_20

    LET c12_nom_plano = HOY USING "YYYYMMDD",".020"
    LET G_LISTA_DET2 = gr_modulo.ruta_envio CLIPPED,"/",c12_nom_plano

    LET borra_lineas = "sed -e '/^$/d' ",lc_reporte CLIPPED,">", G_LISTA_DET2 CLIPPED
    RUN borra_lineas

    LET ch = "chmod 777 ",G_LISTA_DET2
    RUN ch

    INSERT INTO tmp_ctr_envio
    VALUES (pi_folio                    ,
            "AV20"                      ,
            "CZAAV20 DETAV20 SUMAV20"   ,
            gr_edo.enviado              ,
            0                           ,
            HOY                         
           )

END FUNCTION

#---------------------------------------------------------------------------#
# tercer_paso : Inserta en las tablas fisicas los registros que se          #
#               encuentran en las tablas temporales                         #
#---------------------------------------------------------------------------#
FUNCTION tercer_paso(pi_folio)

    DEFINE pi_folio LIKE ret_sal_his_rx.folio

    DEFINE lr_sal_his RECORD LIKE ret_sal_his_rx.*

    -- -----------------------------------------------------------------------------

    INSERT INTO ret_sal_his_tx2 
    SELECT *
    FROM   tmp_sal_his_tx2
    WHERE  folio    = pi_folio

    INSERT INTO ret_ctr_envio
    SELECT *
    FROM   tmp_ctr_envio
    WHERE  folio    = pi_folio

    DECLARE cur_his_tx CURSOR FOR
    SELECT *
    FROM   tmp_sal_his_rx
    ORDER BY nss

    FOREACH cur_his_tx INTO lr_sal_his.*
        UPDATE ret_sal_his_rx
        SET    estado_solicitud = lr_sal_his.estado_solicitud,
               diag_envio       = lr_sal_his.diag_envio
        WHERE  folio            = pi_folio
        AND    nss              = lr_sal_his.nss
        AND    consecutivo      = lr_sal_his.consecutivo
    END FOREACH

    CLOSE WINDOW retc8321

    CALL f_despliega_result(pi_folio)

END FUNCTION


#---------------------------------------------------------------------------#
# f_captura_folio : Captura el folio que se usara en la generacion de la    #
#                   operacion 20 de traspasos historicos de transferencias  #
#---------------------------------------------------------------------------#
FUNCTION f_captura_folio()

    DEFINE lr_cap RECORD
        folio_oper_19       LIKE ret_sal_his_rx.folio
    END RECORD

    DEFINE
        ls_flag             SMALLINT

    -- -----------------------------------------------------------------------------
    
    CALL f_abre_ventana()
    CALL f_obtiene_precios_accion(HOY)

    LET ls_flag                 = 1
    LET lr_cap.folio_oper_19    = gi_ult_folio

    INPUT BY NAME lr_cap.folio_oper_19 WITHOUT DEFAULTS

        AFTER FIELD folio_oper_19
            IF lr_cap.folio_oper_19 IS NULL THEN
                ERROR "    FOLIO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                NEXT FIELD folio_oper_19
            ELSE
                SELECT "OK"
                FROM   ret_cza_lote A
                WHERE  A.folio  = lr_cap.folio_oper_19
                AND    A.folio IN (SELECT B.folio
                                   FROM   ret_sal_his_rx B
                                   WHERE  B.folio = A.folio)
                GROUP BY 1

                IF STATUS = NOTFOUND THEN
                    ERROR "    FOLIO DE SOLICITUD DE SALDO INEXISTENTE " ATTRIBUTE(NORMAL)
                    NEXT FIELD folio_oper_19
                ELSE
                    SELECT "OK"
                    FROM  ret_sal_his_rx A
                    WHERE A.folio            = lr_cap.folio_oper_19
                    AND   A.estado_solicitud = gr_edo.recibido
                    GROUP BY 1

                    IF STATUS = NOTFOUND THEN
                        ERROR "    FOLIO YA PROCESADO CON ANTERIORIDAD" ATTRIBUTE(NORMAL)
                        NEXT FIELD folio_oper_19
                    END IF
                END IF
            END IF

        ON KEY (ESC)
            IF lr_cap.folio_oper_19 IS NULL THEN
                ERROR "    FOLIO NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                NEXT FIELD folio_oper_19
            ELSE
                SELECT "OK"
                FROM   ret_cza_lote A
                WHERE  A.folio = lr_cap.folio_oper_19
                AND    A.folio IN (SELECT B.folio
                                   FROM   ret_sal_his_rx B
                                   WHERE  B.folio = A.folio)
                GROUP BY 1

                IF STATUS = NOTFOUND THEN
                    ERROR "    FOLIO DE SOLICITUD DE SALDO INEXISTENTE "
                    ATTRIBUTE(NORMAL)
                    NEXT FIELD folio_oper_19
                ELSE
                    SELECT "OK"
                    FROM  ret_sal_his_rx A
                    WHERE A.folio            = lr_cap.folio_oper_19
                    AND   A.estado_solicitud = gr_edo.recibido
                    GROUP BY 1

                    IF STATUS = NOTFOUND THEN
                        ERROR "    FOLIO YA PROCESADO CON ANTERIORIDAD"
                        ATTRIBUTE(NORMAL)
                        NEXT FIELD folio_oper_19
                    END IF
                END IF
            END IF

            WHILE TRUE
                PROMPT "¿ DESEA GENERAR LA OPERACION 20 (S/N) ? " FOR CHAR enter
                IF enter MATCHES "[sSnN]" THEN
                    IF enter MATCHES "[sS]" THEN
                        LET ls_flag = 1
                    ELSE
                        PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
                        LET ls_flag = 0
                    END IF
                    EXIT WHILE
                END IF
            END WHILE

            EXIT INPUT

        ON KEY (CONTROL-C, INTERRUPT)
            PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
            LET ls_flag = 0
            EXIT INPUT

    END INPUT

    RETURN ls_flag, lr_cap.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_abre_ventana : Abre la ventana activa donde se captura y despliega la   #
#                  informacion de la operacion                              #
#---------------------------------------------------------------------------#
FUNCTION f_abre_ventana()
    
    OPEN WINDOW retc8321 AT 4,4 WITH FORM "RETC8321" ATTRIBUTE(BORDER)
    DISPLAY " < Ctrl-C > Salir                                                              " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC832     TRANSFERENCIA DE TRASPASOS HISTORICOS (Oper.20)                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

END FUNCTION

#---------------------------------------------------------------------------#
# f_despliega_result : Despliega la informacion final del proceso           #
#---------------------------------------------------------------------------#
FUNCTION f_despliega_result(pi_folio)

    DEFINE pi_folio LIKE ret_sal_his_rx.folio

    CALL f_abre_ventana()
    
    DISPLAY gi_ult_folio AT 8,42
    
    DISPLAY " EL LOTE HA SIDO GENERADO EN LA RUTA : " AT 10,19
    DISPLAY gr_modulo.ruta_envio CLIPPED AT 12,20
    DISPLAY "                                                " AT 16,11
    DISPLAY " CON EL NOMBRE : ",c12_nom_plano AT 14,19
    
    PROMPT " PROCESO FINALIZADO...< ENTER PARA SALIR > " FOR CHAR enter

END FUNCTION

#---------------------------------------------------------------------------#
# f_tablas_tmp : Genera las tablas temporales donde se almacenan los        #
#                calculos y cambios                                         #
#---------------------------------------------------------------------------#
FUNCTION f_tablas_tmp(pi_folio)

    DEFINE pi_folio LIKE ret_sal_his_rx.folio

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_sal_his_tx2
        DROP TABLE tmp_sal_his_rx
        DROP TABLE tmp_ctr_envio
    WHENEVER ERROR STOP

    #-- --------------------------------------------------------------------

    SELECT *
    FROM   ret_sal_his_tx2
    WHERE  1 = 0
    INTO TEMP tmp_sal_his_tx2

    #-- --------------------------------------------------------------------

    SELECT *
    FROM   ret_sal_his_rx
    WHERE  folio = pi_folio
    INTO TEMP tmp_sal_his_rx

    #-- --------------------------------------------------------------------
    
    SELECT *
    FROM   ret_ctr_envio
    WHERE  1 = 0
    INTO TEMP tmp_ctr_envio
    
    #-- --------------------------------------------------------------------


END FUNCTION

#---------------------------------------------------------------------------#
# f_genera_tmp_cuenta : Genera la tabla temporal de dis_cuenta para el nss  #
#---------------------------------------------------------------------------#
FUNCTION f_genera_tmp_cuenta(pc_nss)

    DEFINE pc_nss LIKE ret_sal_his_rx.nss

    DEFINE 
        v_nombre_tabla          CHAR(20)    ,
        sel_his                 CHAR(1500)

    -- -----------------------------------------------------------------------------
    
    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_dis_cuenta;
    WHENEVER ERROR STOP

    DECLARE cur_his CURSOR FOR
    SELECT tabname
    FROM   systables
    WHERE  tabname MATCHES "dis_cuenta??"

    FOREACH cur_his INTO v_nombre_tabla

        LET sel_his = sel_his CLIPPED,
                      " SELECT * "                          ,
                      " FROM " ,v_nombre_tabla              ,
                      " WHERE nss = ", "'", pc_nss, "'"     ,
                      " AND   subcuenta IN (1,2,4,5,6,9) "  ,
                      " UNION ALL "
    END FOREACH

    CLOSE cur_his

    LET sel_his = sel_his CLIPPED,
                  " SELECT * "                              ,
                  " FROM dis_cuenta "                       ,
                  " WHERE nss = ", "'", pc_nss, "'"         ,
                  " AND   subcuenta IN (1,2,4,5,6,9) "      ,
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

#---------------------------------------------------------------------------#
# f_obtiene_precios_accion : Obtiene los precios de accion para la fecha    #
#                            dada por el usuario                            #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_precios_accion(pdt_fec_precios)

    DEFINE
        pdt_fec_precios       DATE

    DEFINE lr_precio_acc RECORD
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE
        lc_exe_precios        CHAR(100) ,
        lc_mensaje            CHAR(100) ,
        lc_siefore            CHAR(002)

    DEFINE
        li_cont               SMALLINT

    -- -----------------------------------------------------------------------------

    LET li_cont = 0

    LET lc_exe_precios = " EXECUTE FUNCTION fn_verifica_precio_accion (?)"
    PREPARE eje_precios_accion FROM lc_exe_precios

    DECLARE c_precios CURSOR FOR eje_precios_accion
    FOREACH c_precios USING pdt_fec_precios
                      INTO lr_precio_acc.*

        -- Solo se valida que existan los precios de las siefores basicas (1 a 5)
        IF (lr_precio_acc.siefore < 10 AND lr_precio_acc.estado <> 0) THEN
            LET lc_siefore = lr_precio_acc.siefore
            LET lc_mensaje = " FALTAN PRECIOS ACCION: DIA ", pdt_fec_precios,
                             ", SIEFORE: ", lc_siefore

            LET lc_mensaje = lc_mensaje CLIPPED
            CALL f_error_msg(lc_mensaje)
            EXIT PROGRAM
        ELSE
            -- Se almacena cada precio de accion el lugar correspondiente
            -- dentro del arreglo de precios.
            LET li_cont                   = lr_precio_acc.siefore
            LET gar_precio_acc[li_cont].* = lr_precio_acc.*
        END IF

    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# f_error_msg : Formatea y despliega los mensajes de error en la pantalla   #
#---------------------------------------------------------------------------#
FUNCTION f_error_msg(pc_mensaje)

    DEFINE
        pc_mensaje          CHAR(75)

    LET pc_mensaje = " ", pc_mensaje CLIPPED , "... <ENTER> PARA SEGUIR "
    PROMPT pc_mensaje ATTRIBUTE(REVERSE) FOR CHAR enter

END FUNCTION

#---------------------------------------------------------------------------#
# f_aporte_menores : Calcula el porcentaje a pagar de los montos indicados  #
#---------------------------------------------------------------------------#
FUNCTION f_aporte_menores(pr_hist)

    DEFINE pr_hist RECORD
        nss                 CHAR(11)        ,
        fecha_pension       DATE            ,
        fecha_retiro        DATE            , --Corresponde a la fecha del traspaso RCV
        ult_retiro_viv      DATE            ,
        porcentaje_val      DECIMAL(5,2)    ,
        regimen             CHAR(2)         ,
        tipo_seguro         CHAR(2)         ,
        tipo_pension        CHAR(2)
    END RECORD
    
    DEFINE lr_saldo_his RECORD
        fecha_conversion    DATE            ,
        subcuenta           SMALLINT        ,
        siefore             SMALLINT        ,      
        monto_acc           DECIMAL(16,6)        
    END RECORD

    DEFINE lr_dis_det RECORD
        folio               INTEGER     ,
        consec              INTEGER
    END RECORD

    DEFINE lr_traspaso RECORD
        fecha_traspaso      DATE            ,
        precio_ced          DECIMAL(19,14)  ,
        siefore_rec         SMALLINT        ,               
        precio_rec          DECIMAL(19,14)
    END RECORD

    DEFINE lr_saldo_tmp RECORD
        subcta              SMALLINT        ,
        siefore             SMALLINT        ,    
        acciones            DECIMAL(16,6)   ,
        pesos               DECIMAL(16,6)
    END RECORD
    
    DEFINE la_aporte ARRAY[9] OF DECIMAL(16,6)

    DEFINE 
        lc_periodo              CHAR(06)
    
    DEFINE 
        ldt_fecha_viv           DATE

    DEFINE 
        ld_participa_ant        DECIMAL(16,6)   ,
        ld_participa_pos        DECIMAL(16,6)   ,
        ld_monto_viv            DECIMAL(16,6)   ,
        ld_monto_viv_pes        DECIMAL(16,6)   ,
        ld_porcent_pago         DECIMAL(5,2)    ,
        ld_saldo_acc_subcta     DECIMAL(16,6)   ,
        ld_saldo_pes_subcta     DECIMAL(16,6)   ,
        ld_pesos                DECIMAL(16,6)   ,
        ld_acciones             DECIMAL(16,6)   ,
        ld_precio_aux_ced       DECIMAL(19,14)  ,
        ld_precio_aux_rec       DECIMAL(19,14)  ,
        ld_aporte               DECIMAL(16,6)   ,
        ld_monto_a_pagar        DECIMAL(16,6)

    DEFINE
        ls_ind_traspaso         ,
        ls_ano                  ,
        ls_mes                  ,
        ls_cociente             ,
        ls_residuo              ,
        ls_subcta               ,
        ls_siefore_aux          ,
        ls_grupo                SMALLINT
        
    -- -----------------------------------------------------------------------------

    -- Inicializa variables
    FOR ls_subcta = 1 TO 9
        LET la_aporte[ls_subcta] = 0
    END FOR

--    IF pr_hist.regimen <> "97" OR pr_hist.tipo_seguro <> "RT" OR pr_hist.tipo_pension <> "IP" THEN
    LET pr_hist.porcentaje_val = 100
--    END IF

    LET ls_ind_traspaso             = 0
    LET ld_participa_ant            = 0
    LET ld_participa_pos            = 0
    LET ld_monto_viv                = 0
    LET ld_monto_viv_pes            = 0
    LET ld_saldo_acc_subcta         = 0
    LET ld_saldo_pes_subcta         = 0
    LET lr_saldo_his.monto_acc      = 0

    LET ls_ano           = YEAR(pr_hist.fecha_pension)
    LET ls_mes           = MONTH(pr_hist.fecha_pension)
    LET ls_cociente      = ls_mes / 2
    LET ls_residuo       = ls_mes - (ls_cociente * 2)

    IF ls_residuo = 1 THEN
        LET ls_mes                = ls_mes + 1
        LET pr_hist.fecha_pension = MDY(ls_mes,1,YEAR(pr_hist.fecha_pension))
    END IF

    LET lc_periodo          = ls_ano USING"&&&&",ls_mes USING"&&"
    LET ldt_fecha_viv       = "08012004"

    -- Se utiliza la clave de grupo 1 = RCV. Se obtiene de la tabla tab_grupo_regimen
    LET ls_grupo = 1
    EXECUTE eje_traspaso USING pr_hist.nss          ,
                               ls_grupo             ,
                               pr_hist.fecha_retiro

    SELECT A.codigo_siefore
    INTO   ls_siefore_aux
    FROM   cta_nss_regimen A
    WHERE  A.nss           = pr_hist.nss
    AND    A.grupo_regimen = 1

    DECLARE cur_dis_det CURSOR FOR
    SELECT folio            ,
           consec_reg_lote
    FROM   dis_det_aporte
    WHERE  n_seguro     = pr_hist.nss
    AND    periodo_pago > lc_periodo

    FOREACH cur_dis_det INTO lr_dis_det.*

        DECLARE cur_stmp CURSOR FOR
        SELECT A.fecha_conversion     ,
               A.subcuenta            ,
               A.siefore              ,
               SUM(A.monto_en_acciones)
        FROM   tmp_dis_cuenta A , 
               tab_subcuenta B
        WHERE  A.folio              = lr_dis_det.folio
        AND    A.consecutivo_lote   = lr_dis_det.consec
        AND    A.subcuenta          = B.subct_cod
        GROUP BY 1,2,3

        FOREACH cur_stmp INTO lr_saldo_his.*

            LET ld_acciones = lr_saldo_his.monto_acc

            DECLARE cur_tras CURSOR FOR
            SELECT fecha_traspaso ,
                   precio_ced     ,
                   siefore_rec    ,
                   precio_rec
            FROM   tmp_trasp_siefore
            WHERE  fecha_traspaso >= lr_saldo_his.fecha_conversion
            AND    fecha_traspaso <= pr_hist.fecha_retiro
            ORDER BY fecha_traspaso

            FOREACH cur_tras INTO lr_traspaso.*

                LET ld_pesos        = ld_acciones * lr_traspaso.precio_ced
                LET ld_acciones     = ld_pesos / lr_traspaso.precio_rec
                LET ls_ind_traspaso = 1

            END FOREACH -- Traspasos

            IF  ls_ind_traspaso = 1 THEN
                LET ls_siefore_aux = lr_traspaso.siefore_rec
            ELSE
                IF lr_saldo_his.siefore <> ls_siefore_aux THEN
                    SELECT A.precio_del_dia
                    INTO   lr_traspaso.precio_ced
                    FROM   glo_valor_accion A
                    WHERE  A.codigo_siefore  = lr_saldo_his.siefore
                    AND    A.fecha_valuacion = gdt_ult_traspaso_rcv

                    SELECT precio_del_dia
                    INTO   lr_traspaso.precio_rec
                    FROM   glo_valor_accion
                    WHERE  codigo_siefore  = ls_siefore_aux
                    AND    fecha_valuacion = gdt_ult_traspaso_rcv

                    LET ld_acciones = lr_saldo_his.monto_acc * lr_traspaso.precio_ced / lr_traspaso.precio_rec
                END IF
            END IF

            CASE lr_saldo_his.subcuenta
                WHEN 1
                    LET la_aporte[1] = la_aporte[1] + ld_acciones
                WHEN 2
                    LET la_aporte[2] = la_aporte[2] + ld_acciones
                WHEN 5
                    LET la_aporte[5] = la_aporte[5] + ld_acciones
                WHEN 6
                    LET la_aporte[6] = la_aporte[6] + ld_acciones
                WHEN 9
                    LET la_aporte[9] = la_aporte[9] + ld_acciones
            END CASE
        END FOREACH --cur_stmp

        DECLARE cur_camb CURSOR FOR
        SELECT fn_cambia_aporte(monto_en_pesos   ,
                                fecha_conversion ,
                                ldt_fecha_viv
                                )
        FROM  tmp_dis_cuenta
        WHERE folio             = lr_dis_det.folio
        AND   consecutivo_lote  = lr_dis_det.consec
        AND   subcuenta         = 4
        AND   fecha_conversion  < ldt_fecha_viv
        AND   fecha_conversion  <= pr_hist.ult_retiro_viv

        FOREACH cur_camb INTO ld_participa_ant
            IF ld_participa_ant IS NULL THEN
                LET ld_participa_ant = 0
            END IF

            LET ld_monto_viv = ld_monto_viv + ld_participa_ant
        END FOREACH -- fin cur_camb Vivienda

        SELECT SUM(monto_en_acciones)
        INTO   ld_participa_pos
        FROM   tmp_dis_cuenta
        WHERE  folio             = lr_dis_det.folio
        AND    consecutivo_lote  = lr_dis_det.consec
        AND    subcuenta         = 4
        AND    fecha_conversion >= ldt_fecha_viv
        AND    fecha_conversion <= pr_hist.ult_retiro_viv

        IF ld_participa_pos IS NULL THEN
            LET ld_participa_pos = 0
        END IF

        LET ld_monto_viv = ld_monto_viv + ld_participa_pos;

    END FOREACH --fin cur_dis_det

    CREATE TEMP TABLE tmp_aporte_sub
    (
      subcuenta   SMALLINT,
      aporte      DECIMAL(16,6)
    )

    FOR ls_subcta = 1 TO 9
        IF (ls_subcta <> 3) AND (ls_subcta <> 4) AND 
           (ls_subcta <> 7) AND (ls_subcta <> 8)  THEN
        
            INSERT INTO tmp_aporte_sub VALUES (ls_subcta,la_aporte[ls_subcta])
        
        END IF
    END FOR

    
    FOR ls_subcta = 1 TO 9
        
        IF ls_subcta = 3 OR ls_subcta = 7 OR ls_subcta = 8 THEN
            CONTINUE FOR
        END IF

        LET ld_saldo_acc_subcta     = 0
        LET ld_saldo_pes_subcta     = 0
        LET lr_saldo_tmp.subcta     = 99

        IF ls_subcta <> 4 THEN

            DECLARE cur_tmp CURSOR FOR
            SELECT subcuenta             ,
                   siefore               ,
                   SUM(monto_en_acciones),
                   SUM(monto_en_pesos   )
            FROM   tmp_dis_cuenta A
            WHERE  A.nss              = pr_hist.nss
            AND    A.subcuenta        = ls_subcta
            AND    A.tipo_movimiento IN (220,240,247,290,294,295)
            GROUP BY 1,2

            FOREACH cur_tmp INTO lr_saldo_tmp.*

                IF lr_saldo_tmp.acciones IS NULL THEN
                    LET lr_saldo_tmp.acciones = 0
                ELSE
                    LET lr_saldo_tmp.acciones = lr_saldo_tmp.acciones * (-1)
                END IF

                IF lr_saldo_tmp.pesos IS NULL THEN
                    LET lr_saldo_tmp.pesos = 0
                ELSE
                    LET lr_saldo_tmp.pesos = lr_saldo_tmp.pesos * (-1)
                END IF

                IF lr_saldo_tmp.subcta <> 4 THEN
                    IF lr_saldo_tmp.siefore <>  ls_siefore_aux AND gdt_ult_traspaso_rcv > "01142005" THEN
                        
                        SELECT A.precio_del_dia
                        INTO   ld_precio_aux_ced
                        FROM   glo_valor_accion A
                        WHERE  A.codigo_siefore  = lr_saldo_tmp.siefore
                        AND    A.fecha_valuacion = gdt_ult_traspaso_rcv

                        SELECT precio_del_dia
                        INTO   ld_precio_aux_rec
                        FROM   glo_valor_accion
                        WHERE  codigo_siefore  = ls_siefore_aux
                        AND    fecha_valuacion = gdt_ult_traspaso_rcv

                        LET lr_saldo_tmp.acciones = (lr_saldo_tmp.acciones * ld_precio_aux_ced)/
                                                    ld_precio_aux_rec
                    END IF
                END IF

                LET ld_saldo_acc_subcta = ld_saldo_acc_subcta + lr_saldo_tmp.acciones
                LET ld_saldo_pes_subcta = ld_saldo_pes_subcta + lr_saldo_tmp.pesos

            END FOREACH --fin cur_tmp
        ELSE
            LET lr_saldo_tmp.subcta     = 4
            LET lr_saldo_tmp.siefore    = 11

            DECLARE cur_viv CURSOR FOR
            SELECT fn_cambia_aporte(monto_en_pesos   ,
                                    fecha_conversion ,
                                    ldt_fecha_viv
                                    )
            FROM   tmp_dis_cuenta A
            WHERE  A.nss              = pr_hist.nss
            AND    A.subcuenta        = 4
            AND    A.tipo_movimiento IN (220,240,247,290,294,295)
            AND    fecha_conversion  < ldt_fecha_viv

            FOREACH cur_viv INTO ld_participa_ant

                IF ld_participa_ant IS NULL THEN
                    LET ld_participa_ant = 0
                END IF

                LET ld_saldo_acc_subcta = ld_saldo_acc_subcta + ld_participa_ant

            END FOREACH

            SELECT SUM(monto_en_acciones)
            INTO   ld_participa_pos
            FROM   tmp_dis_cuenta A
            WHERE  A.nss              = pr_hist.nss
            AND    A.subcuenta        = 4
            AND    A.tipo_movimiento  IN (220,240,247,290,294,295)
            AND    fecha_conversion   >= ldt_fecha_viv

            IF ld_participa_pos IS NULL THEN
                LET ld_participa_pos = 0
            END IF

            LET ld_saldo_acc_subcta = ld_saldo_acc_subcta + ld_participa_pos;

            IF ld_saldo_acc_subcta IS NULL THEN
                LET ld_saldo_acc_subcta = 0
            ELSE
                LET ld_saldo_acc_subcta = ld_saldo_acc_subcta * (-1)
            END IF
        END IF -- Subcuenta <> 4

        IF lr_saldo_tmp.subcta <> 4 THEN
            LET ld_aporte = 0

            SELECT aporte
            INTO   ld_aporte
            FROM   tmp_aporte_sub
            WHERE  subcuenta = lr_saldo_tmp.subcta

            IF ld_aporte IS NULL THEN
                LET ld_aporte = 0
            END IF

            LET ld_monto_a_pagar = ld_saldo_acc_subcta - ld_aporte

            IF ld_monto_a_pagar < 0 THEN
                LET ld_monto_a_pagar = 0
            END IF
        ELSE
            LET ld_monto_a_pagar = ld_saldo_acc_subcta - ld_monto_viv
            
            IF ld_monto_a_pagar < 0 THEN
                LET ld_monto_a_pagar = 0
            END IF
        END IF

        IF lr_saldo_tmp.subcta <> 99 THEN
            IF lr_saldo_tmp.subcta <> 4 THEN
                LET ld_porcent_pago = (ld_monto_a_pagar * pr_hist.porcentaje_val) /
                                       ld_saldo_acc_subcta

                IF ld_porcent_pago IS NULL OR ld_porcent_pago < 0 THEN
                   LET ld_porcent_pago = 0
                END IF

                LET gar_saldos[ls_subcta].siefore            = ls_siefore_aux
                LET gar_saldos[ls_subcta].subcuenta          = lr_saldo_tmp.subcta
                LET gar_saldos[ls_subcta].acc_traspaso       = ld_saldo_acc_subcta
                LET gar_saldos[ls_subcta].aporte             = ld_aporte
                LET gar_saldos[ls_subcta].porcentaje_pago    = ld_porcent_pago
            ELSE
                LET ld_porcent_pago = (ld_monto_a_pagar * pr_hist.porcentaje_val) /
                                        ld_saldo_acc_subcta

                IF ld_porcent_pago IS NULL OR ld_porcent_pago < 0 THEN
                    LET ld_porcent_pago = 0

               END IF
               LET gar_saldos[ls_subcta].siefore            = 11
               LET gar_saldos[ls_subcta].subcuenta          = lr_saldo_tmp.subcta
               LET gar_saldos[ls_subcta].acc_traspaso       = ld_saldo_acc_subcta
               LET gar_saldos[ls_subcta].aporte             = ld_monto_viv
               LET gar_saldos[ls_subcta].porcentaje_pago    = ld_porcent_pago
            END IF
        END IF
    END FOR

    DROP TABLE tmp_aporte_sub
    DROP TABLE tmp_trasp_siefore
END FUNCTION

#---------------------------------------------------------------------------#
# f_obten_porcentajes : Recupera los porcentajes por subcuenta para el nss  #
#---------------------------------------------------------------------------#
FUNCTION f_obten_porcentajes(pr_datos)

    DEFINE pr_datos RECORD
        folio         LIKE ret_sal_his_rx.folio         ,
        nss           LIKE ret_sal_his_rx.nss           ,
        consecutivo   LIKE ret_sal_his_rx.consecutivo
    END RECORD

    DEFINE lr_sal_his_rx    RECORD LIKE ret_sal_his_rx.*
    DEFINE lr_sal_his_tx2   RECORD LIKE ret_sal_his_tx2.*

    DEFINE lr_porcent RECORD
        porcen_ret97        DECIMAL(5,2)    ,
        porcen_cesvej       DECIMAL(5,2)    ,
        porcen_cuosoc       DECIMAL(5,2)    ,
        porcen_viv97        DECIMAL(5,2)
    END RECORD

    DEFINE
        ld_monto_pagar              ,
        ld_monto_traspasado         ,
        ld_suma_acc_tras            ,
        ld_suma_acc_post            DECIMAL(16,6)

    -- -----------------------------------------------------------------------------

    SELECT *
    INTO  lr_sal_his_rx.*
    FROM  ret_sal_his_rx
    WHERE folio       = pr_datos.folio
    AND   nss         = pr_datos.nss
    AND   consecutivo = pr_datos.consecutivo

    LET ld_monto_pagar              = 0
    LET ld_monto_traspasado         = 0
    LET ld_suma_acc_tras            = 0
    LET ld_suma_acc_post            = 0

    LET lr_porcent.porcen_ret97     = 0
    LET lr_porcent.porcen_cesvej    = 0
    LET lr_porcent.porcen_cuosoc    = 0
    LET lr_porcent.porcen_viv97     = 0

    DECLARE cur_rcv CURSOR FOR
    SELECT *
    FROM   tmp_sal_his_tx2
    WHERE  folio     = pr_datos.folio
    AND    nss       = lr_sal_his_rx.nss
    AND    subcuenta IN (2,6,9)

    FOREACH cur_rcv INTO lr_sal_his_tx2.*
        LET ld_suma_acc_tras = ld_suma_acc_tras + lr_sal_his_tx2.acc_traspaso
        LET ld_suma_acc_post = ld_suma_acc_post + lr_sal_his_tx2.acc_posteriores
    END FOREACH

    LET ld_monto_pagar               = ld_suma_acc_tras - ld_suma_acc_post
    LET lr_sal_his_rx.porcentaje_val = 100
    LET lr_porcent.porcen_cesvej     = (ld_monto_pagar * lr_sal_his_rx.porcentaje_val) / ld_suma_acc_tras

    IF lr_porcent.porcen_cesvej IS NULL THEN
        LET lr_porcent.porcen_cesvej = 0
    END IF

    DECLARE cur_sub CURSOR FOR
    SELECT *
    FROM   tmp_sal_his_tx2
    WHERE  folio     = pr_datos.folio
    AND    nss       = lr_sal_his_rx.nss
    AND    subcuenta IN (1,4,5)

    FOREACH cur_sub INTO lr_sal_his_tx2.*

        CASE lr_sal_his_tx2.subcuenta 
            WHEN 1
                LET lr_porcent.porcen_ret97 = lr_sal_his_tx2.porcentaje_pago
                
            WHEN 4
                LET lr_porcent.porcen_viv97 = lr_sal_his_tx2.porcentaje_pago

            WHEN 5
                LET lr_porcent.porcen_cuosoc = lr_sal_his_tx2.porcentaje_pago
        END CASE 
    END FOREACH

    IF lr_porcent.porcen_ret97 IS NULL THEN
        LET lr_porcent.porcen_ret97 = 0
    END IF
    
    IF lr_porcent.porcen_cuosoc IS NULL THEN
        LET lr_porcent.porcen_cuosoc = 0
    END IF
    
    IF lr_porcent.porcen_viv97 IS NULL THEN
        LET lr_porcent.porcen_viv97 = 0
    END IF

    RETURN lr_porcent.*

END FUNCTION

#---------------------------------------------------------------------------#
# r_operacion_20 : Reporte que genera el archivo plano de la operacion 20   #
#---------------------------------------------------------------------------#
REPORT r_operacion_20(pi_folio, pr_hist_rx, pr_datos_tx)

    DEFINE pi_folio LIKE ret_sal_his_rx.folio

    DEFINE pr_hist_rx RECORD LIKE ret_sal_his_rx.*

    DEFINE pr_datos_tx RECORD 
        fecha_valor_viv        LIKE ret_sal_his_tx.fecha_valor_viv    ,
        porcentaje_97          LIKE ret_sal_his_tx.porcentaje_97      ,
        porcentaje_cv          LIKE ret_sal_his_tx.porcentaje_cv      ,
        porcentaje_cuo_so      LIKE ret_sal_his_tx.porcentaje_cuo_so  ,
        porcentaje_viv97       LIKE ret_sal_his_tx.porcentaje_viv97
    END RECORD

    DEFINE
        ldt_operacion               ,
        ldt_transferencia           DATE

    DEFINE
        li_tot_registros            INTEGER

    DEFINE #loc #char
        c6_porc_ret97               CHAR(06) ,
        c5_porc_ret97               CHAR(05) ,
        c6_porc_cv                  CHAR(06) ,
        c5_porc_cv                  CHAR(05) ,
        c6_porc_cuo_so              CHAR(06) ,
        c5_porc_cuo_so              CHAR(05) ,
        c6_porc_viv97               CHAR(06) ,
        c5_porc_viv97               CHAR(05) ,
        c6_porc_val                 CHAR(06) ,
        c5_porc_val                 CHAR(05)

    -- -----------------------------------------------------------------------------

    OUTPUT
        PAGE LENGTH  60
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
        FIRST PAGE HEADER

            LET li_tot_registros = 0

            SELECT A.fecha_operacion,
                   A.fecha_valor_trans
            INTO   ldt_operacion,
                   ldt_transferencia
            FROM   ret_cza_lote A
            WHERE  A.folio = pi_folio

        PRINT
            COLUMN 001, "01"                                 ,
            COLUMN 003, "04"                                 ,
            COLUMN 005, "01"                                 ,
            COLUMN 007, gs_cod_afore USING"&&&"              ,
            COLUMN 010, "03"                                 ,
            COLUMN 012, "001"                                ,
            COLUMN 015, ldt_operacion     USING "YYYYMMDD"   ,
            COLUMN 023, ldt_transferencia USING "YYYYMMDD"   ,
            COLUMN 031, 450 SPACES

    ON EVERY ROW

        LET li_tot_registros = li_tot_registros + 1

        IF pr_hist_rx.tipo_retiro = "B" THEN
            LET pr_datos_tx.porcentaje_97 = 0
        END IF

        LET c6_porc_val        = pr_hist_rx.porcentaje_val USING "&&&.&&"
        LET c5_porc_val        = c6_porc_val[01,03],
                                 c6_porc_val[05,06]


        LET c6_porc_ret97      = pr_datos_tx.porcentaje_97 USING "&&&.&&"
        LET c5_porc_ret97      = c6_porc_ret97[01,03],
                                 c6_porc_ret97[05,06]

        LET c6_porc_cv         = pr_datos_tx.porcentaje_cv USING "&&&.&&"
        LET c5_porc_cv         = c6_porc_cv[01,03],
                                 c6_porc_cv[05,06]

        LET c6_porc_cuo_so     = pr_datos_tx.porcentaje_cuo_so USING "&&&.&&"
        LET c5_porc_cuo_so     = c6_porc_cuo_so[01,03],
                                 c6_porc_cuo_so[05,06]

        LET c6_porc_viv97     = pr_datos_tx.porcentaje_viv97 USING "&&&.&&"
        LET c5_porc_viv97     = c6_porc_viv97[01,03],
                                c6_porc_viv97[05,06]

        PRINT
            COLUMN 001, "03"                                            ,-- tipo_registro
            COLUMN 003, "04"                                            ,-- ident_servicio
            COLUMN 005, "20"                                            ,-- ident_operacio
            COLUMN 007, pr_hist_rx.nss                                  ,
            COLUMN 018, pr_hist_rx.curp                                 ,
            COLUMN 036, pr_hist_rx.nombre_datamart                      ,
            COLUMN 086, pr_hist_rx.nombre_afore                         ,
            COLUMN 126, pr_hist_rx.paterno_afore                        ,
            COLUMN 166, pr_hist_rx.materno_afore                        ,
            COLUMN 206, pr_hist_rx.sec_pension                          ,
            COLUMN 208, pr_hist_rx.tipo_mov_procesar USING "&&&"        ,
            COLUMN 211, pr_hist_rx.regimen                              ,
            COLUMN 213, pr_hist_rx.tipo_retiro                          ,
            COLUMN 214, pr_hist_rx.tipo_seguro                          ,
            COLUMN 216, pr_hist_rx.tipo_pension                         ,
            COLUMN 218, pr_hist_rx.tipo_prestacion USING "&&"           ,
            COLUMN 220, pr_hist_rx.fecha_ini_pen USING "YYYYMMDD"       ,
            COLUMN 228, pr_hist_rx.fecha_resolucion USING "YYYYMMDD"    ,
            COLUMN 236, c5_porc_val                                     ,-- porcentaje
            COLUMN 241, pr_hist_rx.semanas_cotizadas  USING "&&&&"      ,
            COLUMN 245, pr_hist_rx.fecha_carga_datama USING "YYYYMMDD"  ,
            COLUMN 253, pr_hist_rx.diag_envio USING "&&&"               ,
            COLUMN 256, pr_hist_rx.estado_sub_viv     USING "&"         ,
            COLUMN 257, "000000"                                        ,
            COLUMN 263, "00000000000000"                                ,
            COLUMN 277, "00000000000000"                                ,
            COLUMN 291, "00000000000000"                                ,
            COLUMN 305, "00010101"                                      ,-- fecha_val_viv
            COLUMN 327, c5_porc_ret97  USING "&&&&&"                    ,-- porcenta_ret97
            COLUMN 332, c5_porc_cv     USING "&&&&&"                    ,-- porcentanje_cv
            COLUMN 337, c5_porc_cuo_so USING "&&&&&"                    ,-- porcent_cuo_so
            COLUMN 342, c5_porc_viv97  USING "&&&&&"                    ,-- porcenta_viv97
            COLUMN 347, 134 SPACES

    ON LAST ROW
        PRINT
            COLUMN 001, "09"                                ,
            COLUMN 003, "04"                                ,
            COLUMN 005, "01"                                ,
            COLUMN 007, gs_cod_afore USING "&&&"            ,
            COLUMN 010, "03"                                ,
            COLUMN 012, "001"                               ,
            COLUMN 015, ldt_operacion USING "YYYYMMDD"      ,
            COLUMN 023, li_tot_registros USING "&&&&&&"     ,
            COLUMN 029, 452 SPACES
END REPORT
