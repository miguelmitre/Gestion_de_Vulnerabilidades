################################################################################
#Proyecto          => SISTEMA DE AFORES (SAFRE)                                #
#Owner             => E.F.P.                                                   #
#Programa RETC901  => GENERA ARCHIVO DE OPERACION 27 RETIROS TOTALES ISSSTE    #
#Fecha creacion    => 02 DE OCTUBRE DEL 2006                                   #
#By                => DMR                                                      #
#Fecha actualiz.   =>                                                          #
#Actualizacion     =>                                                          #
#Sistema           => RET (ISSSTE)                                             #
#Modifico          => XAVIER TORRES RIOS                                       #
#                  => 20 de Noviembre 2007                                     #
#                     Al nss_imss si en la primera posicion trae una "I" se    #
#                     le mueve espacios a todo el campo del reporte y asi se   #
#                     envia a procesar.                                        #
#Modifico          => JAVIER GONZALEZ JERONIMO                                 #
#                  => 17 de Abril de 2008                                      #
#                     Se agregaron cambios para el soporte de multisiefores    #
#                  => CESAR DAVID CHAVEZ MARTINEZ                              #
#                     18 de Agosto 2008                                        #
#                     Modificaciones para la nueva version de layout           #
#                  => JAVIER GONZALEZ JERONIMO                                 #
#                  => 4 de Septiembre de 2008                                  #
#                     Modificaciones debidas a la introduccion de la siefore   #
#                     12 para vivienda                                         #
################################################################################
DATABASE safre_af

GLOBALS
    DEFINE gar_precio_acc ARRAY [20] OF RECORD #Arreglo para los precios_accion
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE
        g_seg_modulo          RECORD LIKE seg_modulo.*

    DEFINE reg_2 RECORD #reg_2
        confirmado            LIKE ret_estado.estado_solicitud ,
        procesado             LIKE ret_estado.estado_solicitud ,
        enviado               LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE #reg_3
        reg_3                 RECORD LIKE ret_sol_issste_tot.*

    DEFINE reg_4 RECORD #reg_4
        acciones_ret_sie1     DECIMAL(16,6) ,
        acciones_ret_sie2     DECIMAL(16,6) ,
        acciones_xico_sie1    DECIMAL(16,6) ,
        acciones_xico_sie2    DECIMAL(16,6) ,
        impt_ahorro_retiro    DECIMAL(14,2)
    END RECORD

    DEFINE #glo #date
        HOY                   DATE

    DEFINE #glo #char
        borra_lineas          CHAR(100) ,
        c7_nombre_plano       CHAR(007) ,
        c12_nombre_plano      CHAR(012) ,
        c40_paterno           CHAR(040) ,
        c40_materno           CHAR(040) ,
        c40_nombres           CHAR(040) ,
        elimina               CHAR(100) ,
        tipo_trab             CHAR(02) ,
        cimpt_tot_sub_rcv     CHAR(010) ,
        c_impt_tot_sub_rcv    CHAR(011) ,
        c11_impt_cuo_soc      CHAR(011) ,
        c10_impt_cuo_soc      CHAR(010) ,
        c11_impt_ces_vej      CHAR(011) ,
        c10_impt_ces_vej      CHAR(010) ,
        c11_impt_ret_97       CHAR(011) ,
        c10_impt_ret_97       CHAR(010) ,
        ch                    CHAR(110) ,
        ch1                   CHAR(110) ,
        G_LISTA_1             CHAR(100) ,
        G_LISTA_2             CHAR(100) ,
        G_LISTA               CHAR(300) ,
        enter                 CHAR(001) ,
        usuario               CHAR(012) ,
        v_saldo_dia           CHAR(150) ,
        v_marca               CHAR(150) ,
        id_aporte             CHAR(006) ,
        v_nss                 CHAR(011),
        v_provisiona          CHAR(300)

    DEFINE #glo #decimal
        d15_2_tot_importe     DECIMAL(15,2) ,
        d6_monto_pesos_so1    DECIMAL(16,6) ,
        d6_monto_pesos_so2    DECIMAL(16,6) ,
        d6tot_monto_pesos     DECIMAL(16,6) ,
        saldo_dia1_mas_aport  DECIMAL(16,6) ,
        total_intereses       DECIMAL(16,6)

    DEFINE #glo #smallint
        gs_num_siefores       ,-- Indica el numero de siefores que se usan actualmente
        gs_viv                ,-- Indica en que posicion se encuentra almacenada
                               -- la info de precio de vivienda
        s_tipo_movimiento     ,
        v_convive_cod         ,
        v_marca_res           ,
        v_rechazo_cod         ,
        s_codigo_afore        SMALLINT

    DEFINE #glo #integer
        cont_1                ,
        ultimo_folio          ,
        cont_marcados         ,
        cont_desmarcados      ,
        cont_pendientes       ,
        cont_fin              ,
        cont_reg_av           INTEGER

END GLOBALS


MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I
    CALL STARTLOG ("RETC901.log")

    CALL init() #i

    OPEN WINDOW retc9011 AT 4,4 WITH FORM "RETC9011" ATTRIBUTE(BORDER)
    DISPLAY "                           < Ctrl-C > Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC901        GENERA ARCHIVO RETIROS TOTALES ISSSTE (Op. 27)                 " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    CALL valida_ejecucion() #ve

    SELECT COUNT(*)
    INTO   cont_fin
    FROM   ret_sol_issste_tot A
    WHERE  A.estado_solicitud   = reg_2.confirmado

    CALL primer_paso()  #pp -----PROVISIONA CUENTAS
    CALL segundo_paso() #sp -----GENERA EL ARCHIVO OP 27

    DISPLAY "TOTAL REGISTROS A PROCESAR          : ",cont_fin         AT 08,19
    DISPLAY "TOTAL REGISTROS A ENVIAR            : ",cont_reg_av      AT 09,19

    DISPLAY "EL LOTE HA SIDO GENERADO EN LA RUTA : "  AT 12,19
    DISPLAY G_LISTA CLIPPED AT 13,19
    DISPLAY "CON EL NOMBRE : ",c12_nombre_plano AT 15,19

    DISPLAY " FOLIO NUMERO : ",ultimo_folio  AT 18,01
    PROMPT  " PROCESO FINALIZADO... <ENTER> PARA SALIR " FOR CHAR enter

    CLOSE WINDOW retc9011

END MAIN


FUNCTION init()
#i-------------
    DEFINE #loc #smallint
        diaSemana             SMALLINT

    --Obtenemos el numero de siefores actual en el sistema--
    SELECT COUNT(*)
    INTO   gs_num_siefores
    FROM   tab_siefore_local
    WHERE  codigo_siefore  > 0
    AND    codigo_siefore <> 11
    AND    codigo_siefore <> 12

    LET gs_viv = 20

    LET HOY               = TODAY
    LET d15_2_tot_importe = 0
    LET cont_1            = 0
    LET id_aporte         = "RETIRO"

    SELECT codigo_afore   ,
           USER
    INTO   s_codigo_afore ,
           usuario
    FROM   tab_afore_local

    SELECT MAX(A.folio) + 1
    INTO   ultimo_folio
    FROM   glo_folio A

    INSERT INTO glo_folio VALUES (ultimo_folio)

    SELECT A.estado_solicitud
    INTO   reg_2.confirmado
    FROM   ret_estado A
    WHERE  A.descripcion = "CONFIRMADO"

    SELECT A.estado_solicitud
    INTO   reg_2.procesado
    FROM   ret_estado A
    WHERE  A.descripcion = "PROCESADO"

    SELECT A.estado_solicitud
    INTO   reg_2.enviado
    FROM   ret_estado A
    WHERE  A.descripcion = "ENVIADO"

    SELECT *
    INTO   g_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    LET G_LISTA          = g_seg_modulo.ruta_envio
    LET c7_nombre_plano  = "DET_I27"
    LET c12_nombre_plano = HOY USING"YYYYMMDD",".27I"

    SELECT  COUNT(*)
    INTO    cont_1
    FROM    ret_sol_issste_tot A
    WHERE   A.estado_solicitud = reg_2.confirmado

    ----- PROVISIONA  -----
    LET v_provisiona = "EXECUTE FUNCTION fn_prov_cargo (?,?,?,?,?,?,?,?,?,?,?) "
    PREPARE eje_provisiona FROM v_provisiona

    ----- SALDO AL DIA -----
    LET v_saldo_dia = " EXECUTE FUNCTION fn_saldo_dia (?,?,?,?) "
    PREPARE eje_saldo_dia FROM v_saldo_dia

    ----- MARCAJE DE LA CUENTA -----
    LET v_marca = " EXECUTE PROCEDURE marca_cuenta ( ?,?,?,?,?,?,?,? )"
    LET v_rechazo_cod = 0
    LET v_convive_cod = 0

    WHENEVER ERROR CONTINUE
        LET elimina = "rm -f ",g_seg_modulo.ruta_envio CLIPPED,HOY USING"YYYYMMDD",".27I"
        RUN elimina
    WHENEVER ERROR STOP

END FUNCTION


FUNCTION valida_ejecucion()
#ve-------------------

    -- Valida y obtiene los precios de accion
    CALL obtiene_precios_accion(HOY)

    -- Valida que existan registros para procesar
    SELECT "OK"
    FROM   ret_sol_issste_tot
    WHERE  estado_solicitud = reg_2.confirmado
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        PROMPT" NO EXISTEN REGISTROS PARA PROCESAR...<ENTER> PARA SALIR "
        FOR CHAR enter
        EXIT PROGRAM
    END IF

    -- Validacion del usuario
    WHILE TRUE
        PROMPT " ESTA SEGURO S/N ? " FOR CHAR enter
        IF enter MATCHES "[sSnN]" THEN
            IF enter MATCHES "[sS]" THEN
                EXIT WHILE
            ELSE
                PROMPT" PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
                EXIT PROGRAM
            END IF
        END IF
    END WHILE


END FUNCTION


FUNCTION primer_paso() 

    DEFINE reg_5 RECORD #loc #reg_5
        nss_imss             LIKE ret_sol_issste_tot.nss_imss         ,
        consecutivo          LIKE ret_sol_issste_tot.consecutivo      ,
        tipo_retiro          LIKE ret_sol_issste_tot.tipo_retiro      ,
        impt_fon_viv_liq     LIKE ret_sol_issste_tot.impt_fon_viv_liq
    END RECORD

    DEFINE arr_siefore ARRAY [20] OF RECORD
                                        activo            SMALLINT,
                                        acciones_ret      DECIMAL(16,6),
                                        acciones_xico     DECIMAL(16,6)
                                    END RECORD
    DEFINE #loc
        v_subcuenta           SMALLINT ,
        v_grupo               SMALLINT ,
        si_provisiono         SMALLINT

    DEFINE #loc
        pre_sie1              LIKE glo_valor_accion.precio_del_dia,
        pre_sie2              LIKE glo_valor_accion.precio_del_dia

    DEFINE #loc  #char
        folio_sua             CHAR(06)

    DEFINE #loc #smallint
        ls_siefore            , #-- contador para los ciclos for
        f_subcuenta           ,
        f_siefore             SMALLINT

    DEFINE #loc #decimal
        f_monto_acc           DECIMAL(16,6) ,
        f_monto_pes           DECIMAL(16,6)

-- --------------------------------------

    SELECT  "OK"
    FROM    ret_sol_issste_tot A
    WHERE   A.estado_solicitud  = reg_2.confirmado
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        PROMPT "NO EXISTEN REGISTROS PROCESO CANCELADO...<ENTER> PARA SALIR "
        FOR CHAR enter
        EXIT PROGRAM
    END IF
    #--

    DECLARE cur_2 CURSOR FOR
    SELECT  A.nss_imss         ,
            A.consecutivo      ,
            A.tipo_retiro      ,
            A.impt_fon_viv_liq
    FROM    ret_sol_issste_tot A
    WHERE   A.estado_solicitud = reg_2.confirmado

    FOREACH cur_2 INTO reg_5.*

        #-- Inicializamos variables del arreglo
        FOR ls_siefore = 1 TO gs_num_siefores
            LET arr_siefore[ls_siefore].activo        = FALSE
            LET arr_siefore[ls_siefore].acciones_ret  = 0
            LET arr_siefore[ls_siefore].acciones_xico = 0
        END FOR

        LET saldo_dia1_mas_aport     = 0
        LET total_intereses          = 0
        LET reg_4.impt_ahorro_retiro = 0 --importe ahorro ret (pesos)
        LET reg_5.impt_fon_viv_liq   = 0 --importe vivienda (en acciones)

        LET v_subcuenta  = 0
        LET v_grupo      = 0

        ----OBTIENE EL SALDO AL DIA POR NSS DE CADA UNA DE LAS SUBCUENTAS----------
        DECLARE c_saldo CURSOR FOR eje_saldo_dia

        FOREACH c_saldo USING reg_5.nss_imss ,
                              v_subcuenta    , --Si subcta. y grupo son 0, se
                              v_grupo        , --obtiene saldo de todas las
                              HOY              --subcuentas.
                        INTO
                              f_subcuenta ,
                              f_siefore   ,
                              f_monto_acc ,
                              f_monto_pes

            IF f_subcuenta = 13 OR f_subcuenta = 14 OR f_subcuenta = 19 THEN

                IF f_monto_acc IS NULL OR f_monto_acc < 0 THEN
                    LET f_monto_acc = 0
                END IF

                IF f_monto_pes IS NULL OR f_monto_pes < 0 THEN
                    LET f_monto_pes = 0
                END IF

                IF f_siefore = 12 AND f_subcuenta = 14 THEN
                    
                    LET f_monto_pes = redondea_val(f_monto_pes, 2)
                    LET f_monto_acc = redondea_val(f_monto_acc, 2)
                        
                    LET reg_5.impt_fon_viv_liq = f_monto_acc
                ELSE
                    IF f_siefore <> 11 THEN
                        #-- Marcamos como activo el registro de la siefore actual
                        
                        DISPLAY "nss = ", reg_5.nss_imss
                        DISPLAY "sie = ", f_siefore
                        LET arr_siefore[f_siefore].activo = TRUE

                        CASE f_subcuenta

                            WHEN 13
                                LET arr_siefore[f_siefore].acciones_ret  = f_monto_acc

                            WHEN 19
                                LET arr_siefore[f_siefore].acciones_xico = f_monto_acc

                            OTHERWISE
                                #-- Para evitar que se provisionen subcuentas que no corresponden a este tipo de retiro
                                LET f_monto_acc = 0
                        END CASE
                    ELSE
                        LET f_monto_acc = 0
                    END IF
                END IF

               ----- SE REALIZA LA PROVISION -----
                IF ( (f_subcuenta = 13 OR f_subcuenta = 14 OR f_subcuenta = 19) AND (f_monto_acc > 0) ) THEN
                    
                    LET si_provisiono = 0
                    LET folio_sua     = ""
                    LET f_monto_acc   = -f_monto_acc
                    LET f_monto_pes   = -f_monto_pes

                    SELECT movimiento
                    INTO   s_tipo_movimiento
                    FROM   tab_retiro_issste
                    WHERE  tipo_retiro = reg_5.tipo_retiro

                    DECLARE cur_4 CURSOR FOR eje_provisiona
                    OPEN cur_4 USING ultimo_folio        ,--folio
                                     folio_sua           ,--folio_sua
                                     reg_5.nss_imss      ,--nss
                                     f_subcuenta         ,--subcuenta
                                     s_tipo_movimiento   ,--tipo_movimiento
                                     reg_5.consecutivo   ,--consecutivo_lote
                                     f_siefore           ,--siefore
                                     f_monto_acc         ,--monto_en_acciones
                                     f_monto_pes         ,--monto_en_pesos
                                     id_aporte           ,--id_aportante
                                     HOY                  --fecha proceso

                    FETCH cur_4 INTO si_provisiono

                    CLOSE cur_4

                    IF si_provisiono < 0 THEN
                        PROMPT "El NSS :",reg_5.nss_imss CLIPPED," NO PROVISIONO LA SUBCTA ",f_subcuenta FOR enter
                    ELSE
                        INSERT INTO ret_monto_sie_issste
                        VALUES ( ultimo_folio           ,
                                 reg_5.nss_imss         ,
                                 reg_5.consecutivo      ,
                                 f_siefore              ,
                                 f_subcuenta            ,
                                 -f_monto_acc           ,
                                 -f_monto_pes           ,
                                 0
                               )
                    END IF
               END IF
            END IF
        END FOREACH

        ----- SE CALCULA EL SALDO AHORRO RETIRO -----
        FOR ls_siefore = 1 TO gs_num_siefores
            #-- Consideramos solo los registros de las siefores que contengan saldo
            IF arr_siefore[ls_siefore].activo = TRUE THEN
                LET reg_4.impt_ahorro_retiro = reg_4.impt_ahorro_retiro +
                                               ( arr_siefore[ls_siefore].acciones_xico *
                                                 gar_precio_acc[ls_siefore].precio_dia  )
            END IF

        END FOR

        UPDATE ret_sol_issste_tot
        SET    estado_solicitud  = reg_2.enviado               ,
               folio             = ultimo_folio                ,
               fecha_envio       = HOY                         ,
               acciones_siefore1 = arr_siefore[1].acciones_ret ,
               acciones_siefore2 = arr_siefore[2].acciones_ret ,
               impt_ahorro_ret   = reg_4.impt_ahorro_retiro    ,
               impt_fon_viv_liq  = reg_5.impt_fon_viv_liq
        WHERE  consecutivo       = reg_5.consecutivo
        AND    nss_imss          = reg_5.nss_imss

    END FOREACH


END FUNCTION


FUNCTION segundo_paso()


    LET G_LISTA_1 = G_LISTA CLIPPED, "/", c7_nombre_plano

    START REPORT listado_det_27_I TO G_LISTA_1

    DECLARE cur_03 CURSOR FOR
    SELECT  A.*
    FROM    ret_sol_issste_tot A
    WHERE   A.estado_solicitud  = reg_2.enviado
    AND     A.folio             = ultimo_folio
    AND     A.fecha_envio       = HOY

    LET cont_reg_av = 0

    LET ch = "chmod 777 ", G_LISTA_1
    RUN ch

    FOREACH cur_03 INTO reg_3.*

        OUTPUT TO REPORT listado_det_27_I(reg_3.*) #ldav12
        LET cont_reg_av = cont_reg_av + 1

    END FOREACH

    FINISH REPORT listado_det_27_I

    LET G_LISTA_2 = G_LISTA CLIPPED,"/",c12_nombre_plano
    #ff

    LET borra_lineas = "sed -e '/^$/d' ",G_LISTA_1 CLIPPED,">",G_LISTA_2 CLIPPED
    RUN borra_lineas

    LET ch = "cp ",G_LISTA_2 CLIPPED," ",G_LISTA_1 CLIPPED
    RUN ch

    INSERT INTO ret_ctr_envio
        VALUES (ultimo_folio              ,#folio
                "27-I"                    ,#tipo_operacion
                "CZA27-I DET27-I SUM27-I" ,#estructura
                reg_2.procesado           ,#status
                0                         ,#orden_de_envio
                HOY                        #fecha_envio
               )

    LET ch = "chmod 777 ", G_LISTA_1
    RUN ch

    LET ch = "chmod 777 ", G_LISTA_2
    RUN ch

END FUNCTION

FUNCTION redondea_val(p_monto_redondear, p_redondea)

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

FUNCTION obtiene_precios_accion(fecha_precios)
#opa------------------------------------------
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
        li_cont               SMALLINT

    LET li_cont = 1

    LET v_precios_accion = " EXECUTE FUNCTION fn_verifica_precio_accion (?)"
    PREPARE eje_precios_accion FROM v_precios_accion

    DECLARE c_precios CURSOR FOR eje_precios_accion
    FOREACH c_precios USING fecha_precios
                      INTO lr_precio_acc.*

        IF lr_precio_acc.estado <> 0 THEN
            LET lc_siefore = lr_precio_acc.siefore
            LET lc_mensaje = " FALTAN PRECIOS ACCION DIA ", fecha_precios, " < ENTER > PARA SALIR "
            PROMPT lc_mensaje CLIPPED FOR CHAR enter
            EXIT PROGRAM
        ELSE
            IF lr_precio_acc.siefore = 11 THEN
                LET gar_precio_acc[gs_viv].* = lr_precio_acc.*
            ELSE
                LET gar_precio_acc[li_cont].* = lr_precio_acc.*
            END IF
        END IF

        LET li_cont = li_cont + 1
    END FOREACH
END FUNCTION


REPORT listado_det_27_I(reg_9)
#ldav12-----------------------
    DEFINE #loc #reg_9
        reg_9                 RECORD LIKE ret_sol_issste_tot.*

    DEFINE #loc #r_afi
        r_afi  RECORD
                  nombres        LIKE afi_mae_afiliado.nombres,
                  paterno        LIKE afi_mae_afiliado.paterno,
                  materno        LIKE afi_mae_afiliado.materno,
                  n_rfc          LIKE afi_mae_afiliado.n_rfc  ,
                  tipo_solicitud LIKE afi_mae_afiliado.tipo_solicitud
               END RECORD

    DEFINE #loc #smallint
        ls_cont               ,
        ls_sie                ,
        cont_2                SMALLINT

    DEFINE #loc #smallint
        monto_viv_2d          DECIMAL(14,2)

    DEFINE #loc #char
        c01_cve_doc_prob      CHAR(1),
        c05_porc_ahorro       ,
        c05_porc_fon_viv      CHAR(5),
        c06_porc_ahorro       ,
        c06_porc_fon_viv      CHAR(6),
        c15_sie               CHAR(15),
        c19_imp_ahorro_ret    CHAR(19),
        c18_imp_ahorro_ret    CHAR(18),
        c15_fon_viv_liq       CHAR(15),
        c14_fon_viv_liq       CHAR(14)


    DEFINE lr_saldos_siefore RECORD #loc #lr_saldos_siefore
        siefore         SMALLINT                                ,
        saldo_acciones  LIKE ret_monto_sie_issste.saldo_acciones
    END RECORD

    DEFINE la_monto_sie ARRAY[20] OF DECIMAL(16,6)

    DEFINE c14_monto_sie ARRAY[20] OF CHAR(14)

    OUTPUT
        PAGE LENGTH    1000
        LEFT MARGIN    0
        RIGHT MARGIN   0
        TOP MARGIN     0
        BOTTOM MARGIN  0

    FORMAT
        FIRST PAGE HEADER
        ---- ENCABEZADO ---
            PRINT
                COLUMN 001, "01"                      ,--tipo de registro
                COLUMN 003, "04"                      ,--identificador de servicio
                COLUMN 005, "27"                      ,--identificador de operacion
                COLUMN 007, "01"                      ,--tipo entidad origen
                COLUMN 009, s_codigo_afore USING"&&&" ,--clave entidad origen
                COLUMN 012, "03"                      ,--tipo entidad destino
                COLUMN 014, "001"                     ,--clave entidad destino
                COLUMN 017, HOY USING "YYYYMMDD"      ,--fecha de operacion
                COLUMN 025, 956 SPACES

        ON EVERY ROW
            LET cont_2 = cont_2 + 1

            IF reg_9.tipo_retiro <> 1 THEN
                LET reg_9.fecha_nac = "01/01/0001"
            END IF

            IF reg_9.tipo_retiro <> 2 AND reg_9.tipo_retiro <> 7 THEN
                LET reg_9.n_pensionista = "       "
                LET reg_9.entidad_tramite = 0
                LET reg_9.fecha_ini_pen   = "01/01/0001"
            END IF

            #-- el nuevo layout ya no indica en 7
--            IF reg_9.tipo_retiro <> 1 AND reg_9.tipo_retiro <> 7 THEN
            IF reg_9.tipo_retiro <> 1 THEN
                LET c01_cve_doc_prob = " "
            ELSE
                LET c01_cve_doc_prob = reg_9.cve_doc_prob
            END IF

            IF reg_9.tipo_retiro <> 7 AND reg_9.tipo_beneficio <> 999 THEN
                LET reg_9.fec_sol_susp = "01/01/0001"
                LET reg_9.fec_sol_reac = "01/01/0001"
                LET reg_9.fecha_baja   = "01/01/0001"
            END IF

            IF reg_9.tipo_retiro <> 8 AND reg_9.tipo_beneficio <> 123 THEN
                LET c05_porc_ahorro = "00000"
            ELSE
                LET c06_porc_ahorro = reg_9.porcentaje_ahorro USING "&&&.&&"
                LET c05_porc_ahorro = c06_porc_ahorro[1,3],
                                      c06_porc_ahorro[5,6]
            END IF

            IF reg_9.tipo_inversion = 1 THEN
                LET reg_9.fecha_val_ahorro = today
            ELSE
                IF MONTH(HOY) = 12 THEN
                    LET reg_9.fecha_val_ahorro = MDY("01","01",YEAR(HOY+1))
                ELSE
                    LET reg_9.fecha_val_ahorro = MDY(MONTH(HOY)+1,"01",YEAR(HOY))
                END IF
            END IF

            ---- SE OBTIENEN LOS VALORES PARA CADA SIEFORE ----
            FOR ls_cont = 1 TO gs_num_siefores
                LET la_monto_sie[ls_cont] = 0
            END FOR

            DECLARE cur_04 CURSOR FOR
                SELECT siefore       ,
                       saldo_acciones
                FROM   ret_monto_sie_issste
                WHERE  nss         = reg_9.nss_imss
                AND    folio       = reg_9.folio
                AND    consecutivo = reg_9.consecutivo
                AND    subcuenta   = 13

            FOREACH cur_04 INTO lr_saldos_siefore.*
                LET ls_sie = lr_saldos_siefore.siefore
                LET la_monto_sie[ls_sie] = lr_saldos_siefore.saldo_acciones
            END FOREACH


            FOR ls_cont = 1 TO gs_num_siefores

                LET c14_monto_sie[ls_cont] = "00000000000000"

                #-- Cambio con el nuevo layout... requerido ahora para tipo de inversion 1 y 8
                IF reg_9.tipo_inversion = 1 OR reg_9.tipo_inversion = 8 THEN
                    LET c15_sie                = la_monto_sie[ls_cont] USING "&&&&&&&&.&&&&&&"
                    LET c14_monto_sie[ls_cont] = c15_sie[01,08],
                                                 c15_sie[10,15]
                END IF

                LET c15_sie = " "

            END FOR

            #-- Cambio con el nuevo layout... requerido ahora para tipo de inversion 7 y 8
            IF reg_9.tipo_inversion = 7 OR reg_9.tipo_inversion = 8 THEN
                IF reg_9.impt_ahorro_ret IS NULL THEN
                    LET reg_9.impt_ahorro_ret = 0
                END IF

                LET c19_imp_ahorro_ret = reg_9.impt_ahorro_ret USING "&&&&&&&&&&&&&&&&.&&"
                LET c18_imp_ahorro_ret = c19_imp_ahorro_ret[01,16],
                                         c19_imp_ahorro_ret[18,19]
            ELSE
                LET c18_imp_ahorro_ret = "000000000000000000"
            END IF


            IF reg_9.tipo_retiro <> 9 AND reg_9.fecha_oficio IS NULL THEN
                LET reg_9.fecha_oficio = "01/01/0001"
            END IF

            IF reg_9.tipo_retiro = 9 THEN
                LET reg_9.n_oficio        = "          "
                LET reg_9.fecha_oficio    = "01/01/0001"
                LET reg_9.entidad_oficio  = "  "
                LET reg_9.rfc_dependencia = "            "
                LET reg_9.nom_dependencia = "                                        "
                LET reg_9.cve_ramo        = "     "
                LET reg_9.cve_pagaduria   = "     "
            END IF

            IF reg_9.tipo_retiro = 8 AND reg_9.tipo_beneficio = 123 THEN
                LET c06_porc_fon_viv = reg_9.porcentaje_fon_viv USING "&&&.&&"
                LET c05_porc_fon_viv = c06_porc_fon_viv[1,3],
                                       c06_porc_fon_viv[5,6]
            ELSE
                LET c05_porc_fon_viv = "00000"
            END IF


            LET monto_viv_2d    = reg_9.impt_fon_viv_liq
            LET c15_fon_viv_liq = monto_viv_2d USING "&&&&&&&&&&&&.&&"
            LET c14_fon_viv_liq = c15_fon_viv_liq[5,12],
                                  c15_fon_viv_liq[14,15],
                                  "0000"

            IF MONTH(HOY) = 12 THEN
                LET reg_9.fecha_val_viv = MDY("01","01",YEAR(HOY+1))
            ELSE
                LET reg_9.fecha_val_viv = MDY(MONTH(HOY)+1,"01",YEAR(HOY))
            END IF

            SELECT nombres        ,
                   paterno        ,
                   materno        ,
                   n_rfc          ,
                   tipo_solicitud
            INTO  r_afi.*
            FROM  afi_mae_afiliado
            WHERE n_seguro = reg_9.nss_imss

            IF reg_9.tipo_retiro = 1 OR reg_9.tipo_retiro = 2 OR
               reg_9.tipo_retiro = 6 THEN
                IF r_afi.tipo_solicitud = 8 THEN
                    LET tipo_trab = "02"
                ELSE
                    LET tipo_trab = "01"
                END IF
            ELSE
                LET tipo_trab = "  "
            END IF

            IF reg_9.nss_imss[1,1] = "I" THEN
                LET reg_9.nss_imss = "           "
            END IF

        --- DETALLE ---
            PRINT
                COLUMN 001, "03"                                    ,--tipo de registro
                COLUMN 003, "04"                                    ,--ident. de servicio
                COLUMN 005, "27"                                    ,--tipo ident. origen
                COLUMN 007, s_codigo_afore USING"&&&"               ,--clave afore
                COLUMN 010, reg_9.nss_imss                          ,--nss_imss
                COLUMN 021, reg_9.nss_issste                        ,--nss_issste
                COLUMN 032, reg_9.curp                              ,--curp
                COLUMN 050, r_afi.nombres                           ,--nombre_afore
                COLUMN 090, r_afi.paterno                           ,--apellido paterno
                COLUMN 130, r_afi.materno                           ,--apellido materno
                COLUMN 170, r_afi.n_rfc                             ,--rfc
                COLUMN 183, reg_9.fecha_nac USING "YYYYMMDD"        ,--fecha nacimiento
                COLUMN 191, reg_9.tipo_retiro    USING "&&"         ,--tipo de retiro
                COLUMN 193, reg_9.tipo_beneficio USING "&&&"        ,--tipo beneficio
                COLUMN 196, reg_9.n_pensionista                     ,--num pensionista
                COLUMN 203, reg_9.entidad_tramite USING "&&"        ,--ent. tramite
                COLUMN 205, reg_9.fecha_ini_pen USING "YYYYMMDD"    ,--fecha_ini_pen
                COLUMN 213, c01_cve_doc_prob                        ,--docprob_cod
                COLUMN 214, reg_9.fec_sol_susp  USING "YYYYMMDD"    ,--fec_sol_susp
                COLUMN 222, reg_9.fec_sol_reac  USING "YYYYMMDD"    ,--fec_sol_reac
                COLUMN 230, reg_9.fecha_baja    USING "YYYYMMDD"    ,--fecha_baja
                COLUMN 238, "          "                            ,--folio dictamen
                COLUMN 248, reg_9.fecha_solicitud USING "YYYYMMDD"  ,--fecha_solicitud
                COLUMN 256, reg_9.tipo_inversion  USING "&&"        ,--tipo_inversion
                COLUMN 258, reg_9.fecha_val_ahorro USING "YYYYMMDD" ,--fec_val_ahorro
                COLUMN 266, c05_porc_ahorro                         ,--porcentaje_aho
                COLUMN 271, c14_monto_sie[1]                        ,--acciones sie1
                COLUMN 285, c14_monto_sie[2]                        ,--acciones sie2
                COLUMN 299, c14_monto_sie[3]                        ,--acciones sie3
                COLUMN 313, c14_monto_sie[4]                        ,--acciones sie4
                COLUMN 327, c14_monto_sie[5]                        ,--acciones sie5
                COLUMN 341, "00000000000000"                        ,--filler
                COLUMN 355, c18_imp_ahorro_ret                      ,--impt_aho_ret
                COLUMN 373, "000000000000000000"                    ,
                COLUMN 391, reg_9.n_oficio                          ,--n_oficio
                COLUMN 401, reg_9.fecha_oficio USING "YYYYMMDD"     ,--fec_oficio
                COLUMN 409, reg_9.entidad_oficio USING "##"         ,--entidad_oficio
                COLUMN 411, reg_9.fecha_val_viv USING "YYYYMMDD"    ,--fec_val_viv
                COLUMN 419, c05_porc_fon_viv                        ,--porc_fon_viv
                COLUMN 424, c14_fon_viv_liq                         ,--impt_fon_viv_l
                COLUMN 438, 3 SPACES                                ,--diag_procesar
                COLUMN 441, tipo_trab USING "&&"                    ,--tipo de trabajador
                COLUMN 443, 2 SPACES                                ,--status_fon_viv
                COLUMN 445, 2 SPACES                                ,--status_sub_aho
                COLUMN 447, "00010101"                              ,--fec_ent_recur
                COLUMN 455, reg_9.rfc_dependencia                   ,--rfc_dependen.
                COLUMN 467, reg_9.nom_dependencia                   ,--nom_dependen.
                COLUMN 507, reg_9.cve_ramo USING "#####"            ,--cve_ramo.
                COLUMN 512, reg_9.cve_pagaduria USING "#####"       ,--cve_pagaduria

                COLUMN 517, 8   SPACES                              ,--id_procesar
                COLUMN 525, 120 SPACES                              ,--nom_completo_icefa
                COLUMN 645, 120 SPACES                              ,--nom_beneficiario
                COLUMN 765, 13  SPACES                              ,--rfc_pension
                COLUMN 778, 40  SPACES                              ,--paterno_pension
                COLUMN 818, 40  SPACES                              ,--materno_pension
                COLUMN 858, 40  SPACES                              ,--nombres_pension

                COLUMN 898, 72  SPACES                              ,--filler
                COLUMN 970, 2   SPACES                              ,--resul_ope
                COLUMN 972, 3   SPACES                              ,--mot_recha_1
                COLUMN 975, 3   SPACES                              ,--mot_recha_2
                COLUMN 978, 3   SPACES                               --mot_recha_3

        --- SUMARIO ---
        ON LAST ROW
            PRINT
                COLUMN 001, "09"                       ,--tipo de registro
                COLUMN 003, "04"                       ,--identificador de servicio
                COLUMN 005, "01"                       ,--tipo entidad origen
                COLUMN 007, s_codigo_afore USING"&&&"  ,--clave entidad origen
                COLUMN 010, "03"                       ,--tipo entidad destino
                COLUMN 012, "001"                      ,--clave entidad destino
                COLUMN 015, HOY USING "YYYYMMDD"       ,--fecha de operacion
                COLUMN 023, cont_2 USING"&&&&&&"       ,--numero de registros
                COLUMN 029, 952 SPACES
END REPORT




