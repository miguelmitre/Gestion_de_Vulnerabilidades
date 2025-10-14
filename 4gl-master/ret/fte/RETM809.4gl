################################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                               #
#Owner             => E.F.P.                                                    #
#Programa RETM809  => ADMINISTRACION DE SOLICITUDES DE RETIROS PARCIALES        #
#Fecha creacion    => 08 DE FEBRERO DE 2004                                     #
#By                => GERARDO ALFONSO VEGA PAREDES                              #
#Sistema           => RET                                                       #
#Fecha Modifica    => 22 DE SEPTIEMBRE 2004                                     #
#                  => ISAI JIMENEZ ROJAS                                        #
#                  => Incluir validaciones para evitar que se ingrese una       #
#                     solicitud sin beneficiarios                               #
#Fecha Modifica    => 2 DE DICIEMBRE DE 2008                                    #
#                  => JAVIER GONZALEZ JERONIMO                                  #
#                  => Correcciones para evitar el bloqueo en la tabla           #
#                     ret_consecutivo                                           #
#Fecha Modifica    => 22 DE MAYO DE 2009                                        #
#                  => JAVIER GONZALEZ JERONIMO                                  #
#                  => Modificaciones para dar soporte a los cambios por la      #
#                     circular 31-11. Se adecuo para soportar los tipos de      #
#                     retiro A y B de desempleo. (Queda pendiente por ahora el  #
#                     tipo de pago C)                                           #
#Fecha Modifica    => 31 DE MAYO DE 2009                                        #
#                  => JAVIER GONZALEZ JERONIMO                                  #
#                  => Modificaciones para agregar la funcionalidad para los     #
#                     retiros tipo D que vengan de resoluciones transitorias    #
#Fecha Modifica    => 10 DE JUNIO DE 2009                                       #
#                  => JAVIER GONZALEZ JERONIMO                                  #
#                  => Se unifican las versiones con diferencias entre afores y  #
#                     se implementa la captura para los tipos de desempleo C    #
#Fecha Modifica    => 10 DE MAYO DE 201O                                        #
#                  => JAVIER GONZALEZ JERONIMO                                  #
#                  => Se separan las varibles globales y las funciones generales#
#                     en los archivos RETM809a y RETM809b respectivamente.      #
#                     Se realizan varios cambios en la funcionalidad del        #
#                     programa                                                  #
#                     Se cambia la captura para que almacene primero sobre      #
#                     tablas temporales.                                        #
#                     Se corrige la seccion de modificacion para evitar que no  #
#                     cambie el wherever error al salir del programa            #
# Modifica         => IJR 20100617                                              #
#                     Modificacion de acuerdo a requerimiento inv-226           #
#                     Tratamiento especial a resoluciones identificadas como    #
#                     complementarias                                           #
#################################################################################

DATABASE safre_af

GLOBALS "RETM809a.4gl"

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP          ,
        PROMPT LINE LAST    ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("RETM809.log")
    CALL init()

    OPEN WINDOW main_win AT 3,3 WITH 20 ROWS, 75 COLUMNS ATTRIBUTE(BORDER)
    DISPLAY " RETM809      CAPTURA DE SOLICITUDES - RETIROS PARCIALES                    " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)    

    MENU "MENU"
        COMMAND KEY(A) "(A)grega"   "Agrega solicitud"
            LET xaccion = " "
            
            CALL f_captura() RETURNING gs_captura, gc_nss
            
            IF gs_captura <> 0 THEN
                CALL f_inserta_registros(gc_nss)
            ELSE
                CLOSE WINDOW retm0011
            END IF

        COMMAND KEY(C) "(C)onsulta" "Consulta solicitud"
            LET xaccion = " "
            CALL f_consulta()

        COMMAND KEY(M) "(M)odifica" "Modifica solicitud"
            LET comando = "M"
            LET xaccion = " "
            CALL f_modifica(1)

        COMMAND KEY(E) "(E)limina"  "Elimina solicitud"
            LET xaccion = " "
            CALL f_elimina()

        COMMAND KEY(N) "Co(N)firma Captura" "Confirma Captura de solicitud"
            LET comando = "C"
            LET xaccion = "F"

            IF gs_rechazo <> 0 THEN
                MESSAGE " <Ctrl-B> ORDEN DE PAGO    ",
                        " <Ctrl-F> RECHAZAR"
            ELSE
                MESSAGE " <Ctrl-B> ORDEN DE PAGO    "
            END IF
            
            CALL f_modifica(2)

        COMMAND KEY(S) "(S)alida" "Termina Programa de Captura"
            EXIT MENU

    END MENU

    CLOSE WINDOW main_win

{
    LET gc_error =  "------------ FIN EJECUCION : ", HOY, " USUARIO : ", gc_usuario CLIPPED , " ------------ " CLIPPED
    CALL ERRORLOG(gc_error CLIPPED)
}

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    DEFINE
        lc_prepare      CHAR(300)

    -- -----------------------------------------------------------------------------

    ----- INICIALIZA VARIABLES -----
    LET HOY                     = TODAY
    LET vimpt_tot_sub_rcv       = 0
    LET vimpt_ret_97            = 0
    LET vimpt_ces_vej           = 0
    LET vimpt_cuo_soc           = 0
    LET gs_edo_invalido         = -1000
    LET gs_num_dias             = 2
    LET gr_prest.desempleo      = 6
    LET gr_prest.matrimonio     = 7
    LET gd_porc_pago            = 10    -- El porcentaje a pagar es el 10%
    LET gs_dias_sbc             = 75    -- El numero de dias del SBC a pagar son 75
    LET gs_dias_nat_des         = 120
    LET gs_dias_nat_mat         = 60
    LET gs_dias_sm_mat          = 30

    LET gr_movs.matrimonio      = 870
    LET gr_movs.tipo_d          = 875
    LET gr_movs.tipo_a          = 876
    LET gr_movs.tipo_b          = 877
    LET gr_movs.tipo_c          = 878

    CALL f_obtiene_precios_accion(HOY)

    ----- CODIGOS AFORES -----
    SELECT codigo_afore   ,
           USER
    INTO   gs_codigo_afore ,
           gc_usuario
    FROM   tab_afore_local

{
    LET gc_error =  "------------ INICIO EJECUCION : ", HOY, " USUARIO : ", gc_usuario CLIPPED , " ------------ " CLIPPED
    CALL ERRORLOG(gc_error CLIPPED)
}

    SELECT afore_cod
    INTO   gs_cod_sct
    FROM   tab_afore
    WHERE  afore_desc MATCHES "*SCOTIA*"

    SELECT afore_cod
    INTO   gs_cod_cpl
    FROM   tab_afore
    WHERE  afore_desc MATCHES "*COPPEL*"

    -- Determina si se activa la bandera para permitir rechazos
    LET gs_rechazo          = f_det_rechazo(gs_codigo_afore)

    -- Determina si se activa la bandera para eliminar registros
    -- en estado confirmado
    LET gs_del_confirm      = f_det_elimina(gc_usuario)

    ----- SALARIO MINIMO PARA EL DF -----
    SELECT monto_sm
    INTO   gd_sal_minimo
    FROM   tabsalario_minimo2
    WHERE  fecha_hasta_sm IS NULL
    AND    zona_cod = "A"

    -- El maximo monto a retirar para los Retiros Tipo A
    -- debe ser 10 veces el Salario Minimo Mensual del DF
    LET gd_limite_pago_a = (gd_sal_minimo * 30) * 10

    -- El limite de pago para los Retiros Tipo B debe ser
    -- 15 dias de salario minimo mensual del df
    LET gd_limite_pago_b = (gd_sal_minimo * 30) * 15

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.capturado
    FROM   ret_estado A
    WHERE  A.descripcion = "CAPTURADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.precapturado
    FROM   ret_estado A
    WHERE  A.descripcion = "PRECAPTURADO"

    IF gr_edo.precapturado IS NULL THEN
        LET gr_edo.precapturado = gr_edo.capturado 
    END IF

    SELECT A.estado_solicitud
    INTO   gr_edo.confirmado
    FROM   ret_estado A
    WHERE  A.descripcion = "CONFIRMADO"

    SELECT  A.estado_solicitud
    INTO    gr_edo.liquidado
    FROM    ret_estado A
    WHERE   A.descripcion = "LIQUIDADO"

    SELECT  A.estado_solicitud
    INTO    gr_edo.rechazado
    FROM    ret_estado A
    WHERE   A.descripcion = "RECHAZADO"

    IF STATUS = NOTFOUND THEN
        LET gr_edo.rechazado = gs_edo_invalido
    END IF

    ----- MARCAJE DE CUENTA -----
    LET lc_prepare = "EXECUTE PROCEDURE marca_cuenta(?,?,?,?,?,?,?,?)"
    PREPARE eje_marca FROM lc_prepare

    LET lc_prepare = " "

    ----- DESMARCA DE CUENTA -----
    LET lc_prepare = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"
    PREPARE eje_desmarca FROM lc_prepare

    LET lc_prepare = " "

    ----- HABIL ANTERIOR -----
    LET lc_prepare = " EXECUTE FUNCTION fn_habil_anterior(?,?) "
    PREPARE eje_vig_solicitud FROM lc_prepare

    LET lc_prepare = " "

END FUNCTION

#---------------------------------------------------------------------------#
# f_abre_pant_inicial : Abre la pantalla inicial dependiendo si en la afore #
#                       se maneja o no el campo de rechazo                  #
#---------------------------------------------------------------------------#
FUNCTION f_abre_pant_inicial()

    IF gs_rechazo <> 0 THEN
        OPEN WINDOW retm0011 AT 2,3 WITH FORM "RETM8091r" ATTRIBUTE(BORDER)
    ELSE
        OPEN WINDOW retm0011 AT 2,3 WITH FORM "RETM8091" ATTRIBUTE(BORDER)
    END IF

    DISPLAY " RETM809             R E T I R O S   P A R C I A L E S                         " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)
    DISPLAY "                  D A T O S  D E  L A  S O L I C I T U D                       " AT 7,1 ATTRIBUTE(REVERSE)
    DISPLAY "               D A T O S  D E  C O N T R O L  I N T E R N O                    " AT 14,1 ATTRIBUTE(REVERSE)

END FUNCTION

#---------------------------------------------------------------------------#
# f_captura : Realiza las operaciones necesarias para capturar una          #
#             solicitud de retiros parciales                                #
#---------------------------------------------------------------------------#
FUNCTION f_captura()

    DEFINE lr_ctr_pago RECORD LIKE ret_ctr_pago.*

    DEFINE lr_sbc RECORD
        retiro_a       LIKE ret_parcial_resol.salario_base_a ,
        retiro_b       LIKE ret_parcial_resol.salario_base_b
    END RECORD

    DEFINE lr_datos_comp RECORD
        monto_pagado    LIKE ret_resol_comp.monto_pagado ,
        saldo_rcv       LIKE ret_resol_comp.saldo_rcv
    END RECORD

    DEFINE
        ls_siefore             LIKE dis_cuenta.siefore

    DEFINE
        ls_flag                 ,
        ls_error                ,
        sw_1                    ,
        numero_resolucion       ,
        ls_id_complemento       ,             --IJR 20100617
        vstatus                 SMALLINT


    DEFINE #loc #date
        fecha_resp_mat_des      ,
        ldt_inicio_vigencia     ,
        ldt_fin_vigencia        DATE

    DEFINE
        lc_msg                 CHAR(100),
        lc_diag_procesar       CHAR(003)

    -- -----------------------------------------------------------------------------
    CALL f_abre_pant_inicial()
    CALL f_genera_tablas_tmp()

    INITIALIZE lr_ctr_pago.* TO NULL
    INITIALIZE g_reg.* TO NULL

    LET ls_flag                 = 1
    LET ls_error                = 0
    LET ls_siefore              = 0
    LET sw_1                    = 1
    LET v1ra_vez                = TRUE
    LET vmonto_sm               = 0
    LET gs_recap_tipo_des       = 1
    LET gs_recap_sbc            = 1
    LET g_reg.fecha_solicitud   = HOY

    DISPLAY BY NAME g_reg.fecha_solicitud
    DISPLAY " <Esc>  : Agrega             " AT 2,1
    DISPLAY " Ctrl-C : Cancelar" AT 2,59

    INPUT BY NAME g_reg.* WITHOUT DEFAULTS

        -------------------------------------
        BEFORE FIELD nss
        -------------------------------------

            LET vimpt_tot_sub_rcv       = 0
            LET vimpt_ret_97            = 0
            LET vimpt_ces_vej           = 0
            LET vimpt_cuo_soc           = 0

            LET g_reg.fecha_captura     = HOY
            LET g_reg.consecutivo       = 0
            LET g_reg.folio             = 0
            LET g_reg.usuario_captura   = gc_usuario
            LET g_reg.tipo_retiro       = "I"

            SELECT descripcion
            INTO   g_reg.desc_tipo_ret
            FROM   tab_retiro
            WHERE  tipo_retiro = g_reg.tipo_retiro

            DISPLAY BY NAME g_reg.consecutivo       ,
                            g_reg.fecha_captura     ,
                            g_reg.fecha_solicitud   ,
                            g_reg.usuario_captura   ,
                            g_reg.tipo_retiro       ,
                            g_reg.desc_tipo_ret

        -------------------------------------
        AFTER FIELD nss
        -------------------------------------
            IF (FGL_LASTKEY() = FGL_KEYVAL("UP")) OR (FGL_LASTKEY() = FGL_KEYVAL("LEFT")) THEN

                IF g_reg.tipo_prestacion IS NOT NULL THEN
                    IF g_reg.tipo_prestacion = gr_prest.desempleo THEN
                        NEXT FIELD fecha_fall_mat_des
                    ELSE
                        NEXT FIELD impt_autorizado
                    END IF
                ELSE
                    NEXT FIELD fecha_fall_mat_des
                END IF
            END IF

            IF g_reg.nss IS NULL THEN
                ERROR "CAMPO NO PUEDE SER NULO"
                NEXT FIELD nss
            END IF

            CALL f_valida_nss(g_reg.nss)
                RETURNING g_reg.paterno ,
                          g_reg.materno ,
                          g_reg.nombres ,
                          g_reg.rfc     ,
                          g_reg.curp    ,
                          ls_error      ,
                          lc_msg

            IF ls_error = 0 THEN
                CALL f_error_msg(lc_msg)
                NEXT FIELD nss
            END IF

            -- Obtenemos la siefore actual del NSS
            LET ls_siefore = f_obtiene_siefore_act(g_reg.nss)

            CALL f_obtiene_rcv(g_reg.nss, HOY)
                RETURNING vimpt_ret_97     ,
                          vimpt_ces_vej    ,
                          vimpt_cuo_soc    ,
                          vimpt_tot_sub_rcv

            IF vimpt_tot_sub_rcv <= 0 THEN
                CALL f_error_msg("CUENTA CON SALDO CERO PULSE ENTER PARA SALIR")
                LET ls_flag = 0
                EXIT INPUT
            END IF

            DISPLAY BY NAME g_reg.*

        -------------------------------------
        AFTER FIELD fecha_solicitud
        -------------------------------------
            IF (FGL_LASTKEY() = FGL_KEYVAL("UP")) OR (FGL_LASTKEY() = FGL_KEYVAL("LEFT")) THEN
                NEXT FIELD PREVIOUS
            ELSE
                IF g_reg.fecha_solicitud IS NULL THEN
                    ERROR "CAMPO NO PUEDE SER NULO"
                    NEXT FIELD fecha_solicitud
                END IF
            END IF

            IF g_reg.fecha_solicitud > HOY THEN
                ERROR "FECHA NO PUEDE SER MAYOR AL DIA DE HOY"
                NEXT FIELD fecha_solicitud
            END IF

            IF g_reg.fecha_solicitud <> HOY THEN

                EXECUTE eje_vig_solicitud USING HOY         ,
                                                gs_num_dias
                                          INTO  gdt_lim_vigencia

                IF g_reg.fecha_solicitud < gdt_lim_vigencia THEN
                    WHILE TRUE
                        PROMPT "FECHA DE SOLICITUD SIN VIGENCIA DE ENVIO, ¿DESEA CONTINUAR? (S/N) " FOR opc

                        IF opc MATCHES "[SsNn]" THEN
                            EXIT WHILE
                        END IF
                    END WHILE

                    IF opc MATCHES "[Nn]" THEN
                        LET ls_flag = 0
                        EXIT INPUT
                    END IF

                END IF
            END IF

        -------------------------------------
        AFTER FIELD num_resolucion
        -------------------------------------
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD PREVIOUS
            END IF

            IF f_valida_num_resol(g_reg.nss, g_reg.num_resolucion) THEN
                ERROR " EL NUMERO RESOLUCION NO EXISTE EN LA BASE DE DATOS DE PARCIALES"
                NEXT FIELD num_resolucion
            END IF

            SELECT MAX(fecha_ini_vigencia)
            INTO   ldt_inicio_vigencia
            FROM   ret_parcial_resol
            WHERE  nss            = g_reg.nss
            AND    num_resolucion = g_reg.num_resolucion

            IF ldt_inicio_vigencia IS NOT NULL THEN
                SELECT diag_procesar        ,
                       fecha_fin_vigencia   ,
                       id_complemento       
                INTO   lc_diag_procesar     ,
                       ldt_fin_vigencia     ,
                       ls_id_complemento         --IJR 20100617
                FROM   ret_parcial_resol
                WHERE  nss                = g_reg.nss
                AND    num_resolucion     = g_reg.num_resolucion
                AND    fecha_ini_vigencia = ldt_inicio_vigencia
                GROUP BY nss                ,
                         num_resolucion     ,
                         diag_procesar      ,
                         fecha_fin_vigencia ,
                         id_complemento

                IF lc_diag_procesar = "400" THEN
                    IF ldt_fin_vigencia >= g_reg.fecha_solicitud THEN
                        NEXT FIELD tipo_prestacion
                    ELSE
                        CALL f_error_msg("LA FECHA DE VIGENCIA DE LA RESOLUCION YA EXPIRO")
                        LET ls_flag = 0
                        EXIT INPUT
                    END IF
                END IF
            END IF
            
            MESSAGE "RESOLUCION IDENTIFICADA COMO COMPLEMENTARIA" ATTRIBUTE(REVERSE)

        -------------------------------------
        BEFORE FIELD tipo_prestacion    --IJR 20100617
        -------------------------------------
             --IJR 20100617
             IF ls_id_complemento = 1 THEN
               --ASIGNA TIPO DE PRESTACION POR OMISION
               LET g_reg.tipo_prestacion = 6

               --RECUPERA LA DESCRIPCION DE LA PRESTACION
               SELECT descripcion
               INTO   g_reg.desc_tipo_prest
               FROM   tab_prestacion
               WHERE  tipo_prestacion = g_reg.tipo_prestacion
   
               --ASIGNA TIPO DESEMPLEO POR OMISION
               LET g_reg.tipo_desempleo = "C"

               --RECUPERA DESCRIPCION DE TIPO DESEMPLEO
               SELECT desc_corta
               INTO   g_reg.desc_tipo_desempleo
               FROM   tab_tipo_desempleo
               WHERE  tipo_desempleo = g_reg.tipo_desempleo
               
               --DESPLIEGA DATOS DE LA PRESTACION Y TIPO DESEMPLEO 
               DISPLAY BY NAME g_reg.tipo_prestacion ,
                               g_reg.desc_tipo_prest ,
                               g_reg.tipo_desempleo  ,
                               g_reg.desc_tipo_desempleo
             END IF --IJR

        -------------------------------------
        AFTER FIELD tipo_prestacion
        -------------------------------------
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD PREVIOUS
            ELSE
                IF g_reg.tipo_prestacion IS NULL THEN
                    ERROR "CAMPO NO PUEDE SER NULO"
                    NEXT FIELD tipo_prestacion
                END IF
            END IF
            
            IF f_valida_tipo_pres(g_reg.nss, g_reg.num_resolucion, g_reg.tipo_prestacion) THEN
                ERROR " EL TIPO DE PRESTACION NO CORRESPONDE AL NUM. DE RESOLUCION "
                NEXT FIELD tipo_prestacion
            END IF

            SELECT descripcion
            INTO   g_reg.desc_tipo_prest
            FROM   tab_prestacion
            WHERE  tipo_prestacion = g_reg.tipo_prestacion

            DISPLAY BY NAME g_reg.tipo_prestacion,
                            g_reg.desc_tipo_prest

            -- Se captura un retiro por desempleo
            IF g_reg.tipo_prestacion = gr_prest.desempleo THEN
                LET g_reg.impt_autorizado = 0
                DISPLAY BY NAME g_reg.impt_autorizado

                NEXT FIELD tipo_desempleo
            ELSE
                -- Se captura un retiro por matrimonio
                LET g_reg.salario_base_cot = 0
                LET g_reg.pago_desempleo   = 0

                INITIALIZE g_reg.tipo_pago TO NULL
                INITIALIZE g_reg.tipo_desempleo TO NULL

                DISPLAY BY NAME g_reg.salario_base_cot  ,
                                g_reg.pago_desempleo    ,
                                g_reg.tipo_desempleo    ,
                                g_reg.tipo_pago

                IF f_valida_matrimonio_ant(g_reg.nss) THEN
                    CALL f_error_msg("SOLO PUEDE TENER UN RETIRO POR MATRIMONIO EN LA VIDA")
                    LET ls_flag = 0
                    EXIT INPUT
                END IF

                LET vsaldo_mat = f_obten_monto_mat(g_reg.nss)

                IF vsaldo_mat <= 0 THEN
                    CALL f_error_msg("CUENTA CON SALDO CERO PULSE ENTER PARA SALIR")
                    LET ls_flag = 0
                    EXIT INPUT
                END IF

                NEXT FIELD fecha_resolucion
            END IF

        -------------------------------------
        BEFORE FIELD tipo_desempleo
        -------------------------------------
            IF g_reg.tipo_prestacion <> gr_prest.desempleo THEN
                NEXT FIELD fecha_resolucion
            END IF
            
        -------------------------------------
        AFTER FIELD tipo_desempleo  -- Modalidad Retiro en la forma
        -------------------------------------
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD PREVIOUS
            ELSE
                IF g_reg.tipo_desempleo IS NULL THEN
                    CALL f_despliega_tipo_desempleo()
                        RETURNING g_reg.tipo_desempleo,
                                  g_reg.desc_tipo_desempleo
                END IF
            END IF

            --IJR 20100617
            IF ls_id_complemento = 1 AND g_reg.tipo_desempleo <> "C" THEN
               ERROR "EL TIPO DESEMPLEO SOLO PUEDE SER 'C'"
               NEXT FIELD tipo_desempleo
            END IF --IJR

            -- Se valida por si no hace la captura por catalogo
            SELECT desc_corta
            INTO   g_reg.desc_tipo_desempleo
            FROM   tab_tipo_desempleo
            WHERE  tipo_desempleo = g_reg.tipo_desempleo

            IF STATUS = NOTFOUND THEN
                ERROR "TIPO DE DESEMPLEO INEXISTENTE"
                NEXT FIELD tipo_desempleo
            ELSE
                DISPLAY BY NAME g_reg.desc_tipo_desempleo
            END IF

            IF g_reg.tipo_desempleo IS NOT NULL THEN

                CALL f_obtiene_sbc(g_reg.nss, g_reg.num_resolucion)
                    RETURNING lr_sbc.*

                -- Validaciones para evitar que se despliegue de nuevo la pantalla
                -- de detalle de pagos si no se ha cambiado de tipo
                IF gs_recap_tipo_des = 0 THEN
                    IF g_reg.tipo_desempleo <> gi_tipo_desempleo THEN
                        LET gs_recap_tipo_des = 1
                    END IF
                END IF

                IF gs_recap_tipo_des <> 0 THEN
                    LET gs_recap_tipo_des   = 0
                    LET gi_tipo_desempleo   = g_reg.tipo_desempleo

                        IF f_valida_5anios_des(g_reg.nss) AND g_reg.tipo_desempleo <> "C" THEN

                            #-- Validacion temporal para metlife
                            #-- Se pregunta si desea seguir o no capturando
                            WHILE TRUE
                                PROMPT "EL NSS YA CUENTA CON UN RETIRO PREVIO POR DESEMPLEO, ¿DESEA CONTINUAR? (S/N) "
                                FOR CHAR opc

                                IF opc MATCHES "[SsNn]" THEN
                                    IF opc MATCHES "[Ss]" THEN
                                        EXIT WHILE
                                    ELSE
                                        LET ls_flag = 0
                                        EXIT INPUT
                                    END IF
                                END IF
                            END WHILE
{
                            CALL f_error_msg("EL NSS YA CUENTA CON UN RETIRO PREVIO POR DESEMPLEO")
                            LET ls_flag = 0
                            EXIT INPUT
}
                        END IF

                    CASE g_reg.tipo_desempleo
                        WHEN "A"
                            LET g_reg.tipo_pago        = 1
                            LET g_reg.salario_base_cot = lr_sbc.retiro_a

                            CALL f_tipo_pago_1(g_reg.nss            ,
                                               g_reg.consecutivo    ,
                                               lr_sbc.retiro_a      )
                            RETURNING lr_ctr_pago.*

                            LET g_reg.pago_desempleo = lr_ctr_pago.mto_pago

                            CALL f_despliega_tipo_1(lr_ctr_pago.mto_1   ,
                                                    lr_ctr_pago.mto_2   ,
                                                    lr_ctr_pago.mto_pago )

                            DISPLAY BY NAME g_reg.tipo_pago         ,
                                            g_reg.desc_tipo_pago    ,
                                            g_reg.pago_desempleo    ,
                                            g_reg.salario_base_cot

                            CALL f_valida_suficiencia(g_reg.pago_desempleo, vimpt_tot_sub_rcv)
                                RETURNING g_reg.pago_desempleo

                            NEXT FIELD fecha_resolucion

                        WHEN "B"
                            CALL f_det_pago_ab(g_reg.nss                 ,
                                               g_reg.consecutivo         ,
                                               g_reg.num_resolucion      ,
                                               g_reg.tipo_desempleo      ,
                                               g_reg.tipo_pago           ,
                                               vimpt_tot_sub_rcv         ,
                                               0                         ,
                                               lr_sbc.retiro_a           ,
                                               lr_sbc.retiro_b
                                               )
                                RETURNING g_reg.pago_desempleo   ,
                                          g_reg.salario_base_cot ,
                                          g_reg.tipo_pago        ,
                                          g_reg.desc_tipo_pago   ,
                                          lr_ctr_pago.*

                            LET g_reg.pago_desempleo = lr_ctr_pago.mto_pago

                            DISPLAY BY NAME g_reg.salario_base_cot  ,
                                            g_reg.tipo_pago         ,
                                            g_reg.pago_desempleo    ,
                                            g_reg.desc_tipo_pago

                            CALL f_valida_suficiencia(g_reg.pago_desempleo, vimpt_tot_sub_rcv)
                                RETURNING g_reg.pago_desempleo

                            NEXT FIELD fecha_resolucion

                        WHEN "C"
                            -- Pago complementario
                            CALL f_valida_resol_comp(g_reg.nss, g_reg.num_resolucion)
                                RETURNING lr_datos_comp.*, ls_error

                            IF ls_error <> 0 THEN
                                CALL f_error_msg("EL NSS NO TIENE UN RETIRO COMPLEMENTARIO")
                                LET ls_flag = 0
                                EXIT INPUT
                            ELSE
                                IF lr_sbc.retiro_a = 0 AND lr_sbc.retiro_b = 0 THEN
                                    CALL f_captura_salarios() RETURNING lr_sbc.*
                                END IF

                                CALL f_retiros_complementarios(g_reg.nss                 ,
                                                               g_reg.consecutivo         ,
                                                               g_reg.num_resolucion      ,
                                                               g_reg.tipo_desempleo      ,
                                                               g_reg.tipo_pago           ,
                                                               lr_datos_comp.saldo_rcv   ,
                                                               lr_datos_comp.monto_pagado,
                                                               lr_sbc.retiro_a           ,
                                                               lr_sbc.retiro_b           ,
                                                               lr_datos_comp.monto_pagado
                                                               )
                                    RETURNING g_reg.pago_desempleo   ,
                                              g_reg.salario_base_cot ,
                                              g_reg.tipo_pago        ,
                                              g_reg.desc_tipo_pago   ,
                                              lr_ctr_pago.*

                                DISPLAY BY NAME g_reg.salario_base_cot  ,
                                                g_reg.tipo_pago         ,
                                                g_reg.pago_desempleo    ,
                                                g_reg.desc_tipo_pago

                                CALL f_valida_suficiencia(g_reg.pago_desempleo, vimpt_tot_sub_rcv)
                                    RETURNING g_reg.pago_desempleo

                                NEXT FIELD fecha_resolucion
                            END IF

                        WHEN "D"
                            -- Forma de pago anterior
                            CALL f_valida_resol_alterno(g_reg.nss, g_reg.num_resolucion)
                                RETURNING lr_sbc.retiro_a   ,
                                          g_reg.tipo_pago   ,
                                          ls_error

                            IF ls_error <> 0 THEN
                                ERROR "NO EXISTE NUMERO DE RESOLUCION EN RESOLUCIONES TRANSITORIAS"
                                SLEEP 3
                            ELSE
                                IF g_reg.tipo_pago = 2 THEN
                                    ERROR "INGRESE EL SALARIO BASE COTIZACION DE LA SOLICITUD "
                                    SLEEP 2
                                END IF
                            END IF

                            INITIALIZE g_reg.desc_tipo_pago TO NULL
                            DISPLAY BY NAME g_reg.tipo_pago,
                                            g_reg.desc_tipo_pago
                            NEXT FIELD salario_base_cot
                    END CASE
                ELSE
                    IF g_reg.tipo_desempleo <> "D" THEN
                        NEXT FIELD fecha_resolucion
                    ELSE
                        NEXT FIELD salario_base_cot
                    END IF
                END IF
            END IF

        -------------------------------------
        AFTER FIELD tipo_pago -- Forma de Pago en la Tabla
        -------------------------------------
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR
                FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD PREVIOUS
            ELSE
                IF g_reg.tipo_pago IS NULL THEN
                    ERROR "CAMPO NO PUEDE SER NULO"
                    NEXT FIELD tipo_pago
                END IF
            END IF

            SELECT desc_corta
            INTO   g_reg.desc_tipo_pago
            FROM   tab_tipo_pago
            WHERE  tipo_pago = g_reg.tipo_pago

            IF STATUS = NOTFOUND THEN
                ERROR "TIPO DE PAGO INEXISTENTE"
                NEXT FIELD tipo_pago
            ELSE
                DISPLAY BY NAME g_reg.tipo_pago     ,
                                g_reg.pago_desempleo,
                                g_reg.desc_tipo_pago

                NEXT FIELD fecha_resolucion
            END IF

        -------------------------------------
        BEFORE FIELD salario_base_cot
        -------------------------------------
            IF g_reg.tipo_desempleo <> "D" THEN
                NEXT FIELD fecha_resolucion
            ELSE
                IF g_reg.tipo_prestacion = gr_prest.desempleo AND g_reg.tipo_pago IS NOT NULL THEN
                    SELECT desc_corta
                    INTO   g_reg.desc_tipo_pago
                    FROM   tab_tipo_pago
                    WHERE  tipo_pago = g_reg.tipo_pago

                    IF STATUS = NOTFOUND THEN
                        ERROR "TIPO DE PAGO INEXISTENTE"
                        NEXT FIELD tipo_pago
                    ELSE
                        DISPLAY BY NAME g_reg.tipo_pago     ,
                                        g_reg.desc_tipo_pago
                    END IF
                END IF
            END IF

        -------------------------------------
        AFTER FIELD salario_base_cot
        -------------------------------------
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR
                FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD tipo_desempleo
            END IF
            IF g_reg.salario_base_cot IS NULL OR g_reg.salario_base_cot = 0 THEN
                ERROR "CAMPO NO PUEDE SER NULO"
                NEXT FIELD salario_base_cot
            ELSE

                IF gs_recap_sbc = 0 THEN
                    IF g_reg.salario_base_cot <> gd_sal_base_cot THEN
                        LET gs_recap_sbc = 1
                    END IF
                END IF

                IF gs_recap_sbc <> 0 THEN
                    LET gs_recap_sbc   = 0
                    LET gd_sal_base_cot   = g_reg.salario_base_cot

                    CALL f_valida_resol_alterno(g_reg.nss, g_reg.num_resolucion)
                        RETURNING lr_sbc.retiro_a   ,
                                  g_reg.tipo_pago   ,
                                  ls_error

                    CALL f_retiros_anteriores(g_reg.nss                 ,
                                              g_reg.consecutivo         ,
                                              g_reg.num_resolucion      ,
                                              g_reg.tipo_desempleo      ,
                                              g_reg.tipo_pago           ,
                                              vimpt_tot_sub_rcv         ,
                                              0                         ,
                                              lr_sbc.retiro_a           ,
                                              g_reg.salario_base_cot
                                             )
                        RETURNING g_reg.pago_desempleo   ,
                                  g_reg.salario_base_cot ,
                                  g_reg.tipo_pago        ,
                                  g_reg.desc_tipo_pago

                ELSE
                    NEXT FIELD fecha_resolucion
                END IF


            END IF

            DISPLAY BY NAME g_reg.pago_desempleo    ,
                            g_reg.salario_base_cot  ,
                            g_reg.desc_tipo_pago

        -------------------------------------
        BEFORE FIELD pago_desempleo
        -------------------------------------
            IF g_reg.tipo_pago > 3 THEN
                NEXT FIELD nss
            END IF

        -------------------------------------
        AFTER FIELD pago_desempleo
        -------------------------------------
            IF g_reg.tipo_prestacion = gr_prest.desempleo AND
               (g_reg.pago_desempleo IS NULL OR g_reg.pago_desempleo = 0) THEN
                ERROR "CAMPO NO PUEDE SER NULO"
                NEXT FIELD salario_base_cot
            END IF

        -------------------------------------
        BEFORE FIELD fecha_resolucion
        -------------------------------------
            IF g_reg.tipo_prestacion = gr_prest.desempleo AND g_reg.tipo_pago IS NOT NULL THEN
                SELECT desc_corta
                INTO   g_reg.desc_tipo_pago
                FROM   tab_tipo_pago
                WHERE  tipo_pago = g_reg.tipo_pago

                IF STATUS = NOTFOUND THEN
                    ERROR "TIPO DE PAGO INEXISTENTE"
                    NEXT FIELD tipo_pago
                ELSE
                    DISPLAY BY NAME g_reg.tipo_pago     ,
                                    g_reg.desc_tipo_pago
                END IF
            END IF

        -------------------------------------
        AFTER FIELD fecha_resolucion
        -------------------------------------
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR
                FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN

                IF g_reg.tipo_prestacion = gr_prest.desempleo THEN
                    IF g_reg.tipo_desempleo = "D" THEN
                        NEXT FIELD salario_base_cot
                    ELSE
                        NEXT FIELD tipo_desempleo
                    END IF
                ELSE
                    NEXT FIELD tipo_prestacion
                END IF

                NEXT FIELD PREVIOUS
            END IF

            IF g_reg.fecha_resolucion IS NULL OR g_reg.fecha_resolucion = 0 THEN
                ERROR "CAMPO NO PUEDE SER NULO "
                NEXT FIELD fecha_resolucion
            END IF

            IF g_reg.fecha_resolucion > HOY THEN
                ERROR "FECHA NO PUEDE SER MAYOR AL DIA DE HOY"
                NEXT FIELD fecha_resolucion
            END IF

            IF g_reg.fecha_resolucion > g_reg.fecha_solicitud THEN
                ERROR "FECHA NO PUEDE SER MAYOR A LA FECHA DE SOLICITUD"
                NEXT FIELD fecha_resolucion
            END IF

            -- Validaciones entre la fecha de solicitud la fecha de resolucion
            LET vdias_naturales = g_reg.fecha_solicitud - g_reg.fecha_resolucion

            IF g_reg.tipo_prestacion = gr_prest.desempleo THEN
                IF vdias_naturales > gs_dias_nat_des THEN
                    CALL f_error_msg("FECHA RESOLUCION VS SOLICITUD PASA DE 120 DIAS NATURALES")
                    LET ls_flag = 0
                    EXIT INPUT
                END IF

                IF f_valida_5anios_des(g_reg.nss) AND g_reg.tipo_desempleo <> "C" THEN

                    #-- Validacion temporal para metlife
                    #-- Se pregunta si desea seguir o no capturando
                    WHILE TRUE
                        PROMPT "EL NSS YA CUENTA CON UN RETIRO PREVIO POR DESEMPLEO, ¿DESEA CONTINUAR? (S/N) "
                        FOR CHAR opc

                        IF opc MATCHES "[SsNn]" THEN
                            IF opc MATCHES "[Ss]" THEN
                                EXIT WHILE
                            ELSE
                                LET ls_flag = 0
                                EXIT INPUT
                            END IF
                        END IF
                    END WHILE
{
                    CALL f_error_msg("EL NSS YA CUENTA CON UN RETIRO PREVIO POR DESEMPLEO")
                    LET ls_flag = 0
                    EXIT INPUT
}
                END IF
            ELSE
                IF vdias_naturales > gs_dias_nat_mat THEN
                    CALL f_error_msg("FECHA RESOLUCION VS SOLICITUD PASA DE 60 DIAS NATURALES")
                    LET ls_flag = 0
                    EXIT INPUT
                END IF
            END IF


        -------------------------------------
        AFTER FIELD fecha_fall_mat_des
        -------------------------------------
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD PREVIOUS
            ELSE
                IF g_reg.fecha_fall_mat_des IS NULL THEN
                    ERROR "CAMPO NO PUEDE SER NULO"
                    NEXT FIELD fecha_fall_mat_des
                END IF
            END IF

            IF g_reg.fecha_fall_mat_des > g_reg.fecha_resolucion THEN
                ERROR "ESTA FECHA NO PUEDE SER MAYOR A LA FECHA RESOLUCION"
                NEXT FIELD fecha_fall_mat_des
            END IF

            IF g_reg.fecha_fall_mat_des > g_reg.fecha_solicitud THEN
                ERROR "ESTA FECHA NO PUEDE SER MAYOR A LA FECHA SOLICITUD"
                NEXT FIELD fecha_fall_mat_des
            END IF

            IF g_reg.tipo_prestacion = gr_prest.desempleo THEN
                NEXT FIELD nss
            ELSE
                IF sw_1 = 1 THEN
                    LET sw_1               = 0
                    LET fecha_resp_mat_des = g_reg.fecha_fall_mat_des
                END IF

                IF g_reg.fecha_fall_mat_des <> fecha_resp_mat_des THEN
                    LET v1ra_vez           = TRUE
                    LET fecha_resp_mat_des = g_reg.fecha_fall_mat_des
                END IF

                NEXT FIELD impt_autorizado
            END IF

        -------------------------------------
        BEFORE FIELD impt_autorizado
        -------------------------------------
            IF g_reg.tipo_prestacion = gr_prest.desempleo THEN
                LET g_reg.impt_autorizado = 0
                DISPLAY BY NAME g_reg.impt_autorizado
            ELSE
                IF v1ra_vez THEN
                    LET ano = HOY USING "0101YYYY"

                    IF g_reg.fecha_fall_mat_des >= ano THEN
                        SELECT monto_sm
                        INTO   vmonto_sm
                        FROM   tabsalario_minimo2
                        WHERE  fecha_desde_sm <= g_reg.fecha_fall_mat_des
                        AND    fecha_hasta_sm IS NULL
                        AND    zona_cod        = "A"

                        IF vmonto_sm = 0 OR vmonto_sm IS NULL THEN
                            CALL f_error_msg("NO EXISTE SALARIO MINIMO PARA ESTE AÑO")
                            EXIT PROGRAM
                        ELSE
                            LET g_reg.impt_autorizado = vmonto_sm * gs_dias_sm_mat
                            DISPLAY BY NAME g_reg.impt_autorizado
                        END IF
                    ELSE
                        SELECT monto_sm
                        INTO   vmonto_sm
                        FROM   tabsalario_minimo2
                        WHERE  fecha_desde_sm <= g_reg.fecha_fall_mat_des
                        AND    fecha_hasta_sm >= g_reg.fecha_fall_mat_des
                        AND    zona_cod        = "A"

                        IF vmonto_sm = 0 OR vmonto_sm IS NULL THEN
                            CALL f_error_msg("NO EXISTE SALARIO MINIMO PARA ESTE AÑO")
                            EXIT PROGRAM
                        ELSE
                            LET g_reg.impt_autorizado = vmonto_sm * gs_dias_sm_mat
                            DISPLAY BY NAME g_reg.impt_autorizado
                        END IF
                    END IF
                END IF
            END IF

            LET v1ra_vez = FALSE

            NEXT FIELD nss

        -------------------------------------
        AFTER FIELD impt_autorizado
        -------------------------------------
            IF FGL_LASTKEY() = FGL_KEYVAL("UP")   OR
                FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD PREVIOUS
            ELSE
                IF g_reg.impt_autorizado IS NULL OR g_reg.impt_autorizado = 0 THEN
                    ERROR "CAMPO NO PUEDE SER NULO"
                    NEXT FIELD impt_autorizado
                ELSE
                    IF g_reg.tipo_prestacion = gr_prest.desempleo THEN
                        LET g_reg.impt_autorizado = 0
                        DISPLAY BY NAME g_reg.impt_autorizado
                    END IF
                END IF
            END IF

            IF g_reg.impt_autorizado > vsaldo_mat THEN
                ERROR "EL IMPORTE DIGITADO NO PUEDE SER MAYOR QUE EL SALDO ",
                       vsaldo_mat
                NEXT FIELD impt_autorizado
            END IF

            NEXT FIELD nss

        -------------------------------------
        ON KEY (ESC)
        -------------------------------------
            IF g_reg.nss IS NULL THEN
                CALL f_error_msg("NSS NO PUEDE SER NULO")
                NEXT FIELD nss
            END IF

            #--
            IF g_reg.fecha_solicitud IS NULL THEN
                CALL f_error_msg("FECHA DE SOLICITUD NO PUEDE SER NULA")
                NEXT FIELD fecha_solicitud
            END IF
            
            IF g_reg.fecha_solicitud > HOY THEN
                ERROR "FECHA NO PUEDE SER MAYOR AL DIA DE HOY"
                NEXT FIELD fecha_solicitud
            END IF

            IF g_reg.fecha_solicitud <> HOY THEN

                EXECUTE eje_vig_solicitud USING HOY         ,
                                                gs_num_dias
                                          INTO  gdt_lim_vigencia

                IF g_reg.fecha_solicitud < gdt_lim_vigencia THEN
                    WHILE TRUE
                        PROMPT "FECHA DE SOLICITUD SIN VIGENCIA DE ENVIO, ¿DESEA CONTINUAR? (S/N) " FOR opc

                        IF opc MATCHES "[SsNn]" THEN
                            EXIT WHILE
                        END IF
                    END WHILE

                    IF opc MATCHES "[Nn]" THEN
                        LET ls_flag = 0
                        EXIT INPUT
                    END IF

                END IF
            END IF

            #--
            IF g_reg.num_resolucion IS NULL THEN
                CALL f_error_msg("EL NUMERO DE RESOLUCION NO PUEDE SER NULO")
                NEXT FIELD num_resolucion
            END IF

            IF g_reg.tipo_prestacion IS NULL THEN
                CALL f_error_msg("TIPO DE PRESTACION NO PUEDE SER NULO")
                NEXT FIELD tipo_prestacion
            END IF
            
            IF g_reg.fecha_resolucion > HOY THEN
                ERROR "FECHA NO PUEDE SER MAYOR AL DIA DE HOY"
                NEXT FIELD fecha_resolucion
            END IF

            LET vdias_naturales = g_reg.fecha_resolucion - g_reg.fecha_solicitud

            IF g_reg.tipo_prestacion = gr_prest.desempleo THEN
                IF vdias_naturales > gs_dias_nat_des THEN
                    ERROR "FECHA RESOLUCION VS FECHA SOLICITUD PASA DE 120 DIAS NATURATURALES PULSE ENTER P/SALIR"
                    NEXT FIELD fecha_resolucion
                END IF
            ELSE
                IF vdias_naturales > gs_dias_nat_mat THEN
                    ERROR "FECHA RESOLUCION VS FECHA SOLICITUD PASA DE 60 DIAS NATURALES "
                    NEXT FIELD fecha_resolucion
                END IF
            END IF

            IF g_reg.fecha_fall_mat_des IS NULL THEN
                ERROR "CAMPO NO PUEDE SER NULO"
                NEXT FIELD fecha_fall_mat_des
            END IF

            IF g_reg.fecha_fall_mat_des > HOY THEN
                ERROR "FECHA NO PUEDE SER MAYOR AL DIA DE HOY"
                NEXT FIELD fecha_fall_mat_des
            END IF

            IF g_reg.fecha_fall_mat_des > g_reg.fecha_resolucion THEN
                ERROR "ESTA FECHA NO PUEDE SER MAYOR A LA FECHA RESOLUCION"
                NEXT FIELD fecha_fall_mat_des
            END IF

            IF g_reg.fecha_fall_mat_des > g_reg.fecha_solicitud THEN
                ERROR "ESTA FECHA NO PUEDE SER MAYOR A LA FECHA SOLICITUD"
                NEXT FIELD fecha_fall_mat_des
            END IF

            IF g_reg.impt_autorizado IS NULL THEN
               LET g_reg.impt_autorizado = 0
            END IF

            IF g_reg.impt_autorizado > vsaldo_mat THEN
               ERROR "EL IMPORTE DIGITADO NO PUEDE SER MAYOR QUE EL SALDO ",
                      vsaldo_mat
               NEXT FIELD impt_autorizado
            END IF

            IF g_reg.salario_base_cot IS NULL THEN
               LET g_reg.salario_base_cot = 0
            END IF

            IF g_reg.pago_desempleo IS NULL THEN
               LET g_reg.pago_desempleo = 0
            END IF

            LET g_reg.estado_solicitud = gr_edo.capturado
            LET g_reg.rechazo_cod      = 0
            LET g_reg.fecha_captura    = HOY

            LET vfecha_causa    = NULL

            -- Obtenemos la siefore actual del NSS
            LET ls_siefore = f_obtiene_siefore_act(g_reg.nss)

            CALL f_obtiene_rcv(g_reg.nss, HOY)
                RETURNING vimpt_ret_97     ,
                          vimpt_ces_vej    ,
                          vimpt_cuo_soc    ,
                          vimpt_tot_sub_rcv

            INSERT INTO tmp_parcial
            VALUES(g_reg.nss                ,
                   g_reg.consecutivo        ,
                   g_reg.folio              ,
                   g_reg.folio_solicitud    ,
                   " "                      ,
                   g_reg.curp               ,
                   g_reg.tipo_retiro        ,
                   g_reg.tipo_prestacion    ,
                   g_reg.tipo_desempleo     ,
                   g_reg.tipo_pago          ,
                   g_reg.num_resolucion     ,
                   g_reg.salario_base_cot   ,
                   g_reg.fecha_resolucion   ,
                   g_reg.fecha_fall_mat_des ,
                   g_reg.impt_autorizado    ,
                   g_reg.pago_desempleo     ,
                   vimpt_ret_97             ,
                   vimpt_ces_vej            ,
                   vimpt_cuo_soc            ,
                   vimpt_tot_sub_rcv        ,
                   NULL                     , -- diag_cuenta_ind
                   g_reg.fecha_solicitud    ,
                   g_reg.estado_solicitud   ,
                   NULL                     ,
                   g_reg.cod_rechazo_ent    ,
                   g_reg.rechazo_cod        ,
                   g_reg.fecha_captura      ,
                   g_reg.fecha_modifica     ,
                   g_reg.fecha_confirma     ,
                   NULL                     , -- fecha_genera
                   NULL                     , -- fecha_envio
                   g_reg.usuario_captura    ,
                   g_reg.usuario_modifica   ,
                   g_reg.usuario_confirma   ,
                   0 )                        -- carta

            IF (g_reg.tipo_prestacion = gr_prest.desempleo AND g_reg.tipo_desempleo <> "D") THEN
                -- Insertamos solo aquellos registros que esten bajo el nuevo formato
                INSERT INTO tmp_ctr_pago
                VALUES (lr_ctr_pago.*)
            END IF

            IF g_reg.tipo_desempleo = "C" THEN
                UPDATE ret_resol_comp
                SET    ult_sbc = lr_sbc.retiro_a
                WHERE  nss = g_reg.nss
            END IF

            EXIT INPUT

        -------------------------------------
        ON KEY (INTERRUPT)
        -------------------------------------
            CALL f_error_msg("OPERACION CANCELADA")
            LET ls_flag = 0
            EXIT INPUT

    END INPUT

--    CLOSE WINDOW retm0011

    RETURN ls_flag, g_reg.nss

END FUNCTION

#---------------------------------------------------------------------------#
# f_consulta : Ejecuta la consulta de solicitudes de retiros parciales      #                                                                          #
#---------------------------------------------------------------------------#
FUNCTION f_consulta()

    CALL f_abre_pant_inicial()

    DISPLAY " <Ctrl-B> ORDEN DE PAGO      " AT 2,1
    DISPLAY " Ctrl-C : Cancelar" AT 2,59

    LET INT_FLAG = FALSE

    CONSTRUCT BY NAME cla_whe ON
       nss              ,
       paterno          ,
       materno          ,
       nombres          ,
       folio_solicitud  ,
       num_resolucion   ,
       tipo_prestacion  ,
       tipo_desempleo   ,
       rechazo_cod      ,
       fecha_captura    ,
       folio            ,
       consecutivo

        ON KEY (INTERRUPT)
            LET INT_FLAG = TRUE
            EXIT CONSTRUCT

        ON KEY (ESC)
            LET INT_FLAG = FALSE
            EXIT CONSTRUCT

    END CONSTRUCT

    IF INT_FLAG = TRUE THEN
        LET INT_FLAG = FALSE
        CLOSE WINDOW retm0011
        RETURN
    END IF

    LET cla_sel = "SELECT A.nss,",
                         "B.n_rfc,",
                         "B.n_unico,",
                         "B.paterno,",
                         "B.materno,",
                         "B.nombres,",
                         "A.folio_solicitud,",
                         "A.fecha_solicitud,",
                         "A.num_resolucion,",
                         "A.tipo_retiro,",
                         "' ',",
                         "A.tipo_prestacion,",
                         "' ',",

                         "A.tipo_desempleo,",
                         "' ',",
                         "A.tipo_pago,",
                         "' ',",
                         "A.salario_base_cot,",
                         "A.pago_desempleo,",

                         "A.fecha_resolucion,",
                         "A.fecha_fall_mat_des,",
                         "A.impt_autorizado,",
                         "A.rechazo_cod,",
                         "' ',",

                         "A.cod_rechazo_ent,",

                         "A.fecha_captura,",
                         "A.fecha_modifica,",
                         "A.fecha_confirma,",
                         "'',",
                         "A.folio,",
                         "A.consecutivo,",
                         "A.usuario_captura,",
                         "A.usuario_modifica,",
                         "A.usuario_confirma,",
                         "A.estado_solicitud,",
                         "'',",
                         "A.fecha_envio,",
                         "A.diag_cuenta_ind ",
                  "FROM   ret_parcial A,",
                  "       OUTER afi_mae_afiliado B ",
                  "WHERE  A.nss = B.n_seguro ",
                  "AND  ",cla_whe CLIPPED

    PREPARE claexe1 FROM cla_sel
    DECLARE cur_1 CURSOR FOR claexe1

    LET i = 1

    ERROR "PROCESANDO INFORMACION"

    FOREACH cur_1 INTO gar_cons[i].*

        LET gar_cons[i].tipo_retiro = "I"
        SELECT descripcion
        INTO   gar_cons[i].desc_tipo_ret
        FROM   tab_retiro
        WHERE  tipo_retiro = gar_cons[i].tipo_retiro

        SELECT descripcion
        INTO   gar_cons[i].desc_tipo_prest
        FROM   tab_prestacion
        WHERE  tipo_prestacion = gar_cons[i].tipo_prestacion

        SELECT rechazo_desc
        INTO   gar_cons[i].desc_cod_rech
        FROM   tab_rch_marca
        WHERE  rechazo_cod = gar_cons[i].rechazo_cod

        SELECT fecha_pago
        INTO   gar_cons[i].fecha_pago
        FROM   ret_parcial_tx
        WHERE  nss         = gar_cons[i].nss
        AND    consecutivo = gar_cons[i].consecutivo

        SELECT descripcion
        INTO   gar_cons[i].estado_sol_desc
        FROM   ret_estado
        WHERE  estado_solicitud = gar_cons[i].estado_solicitud

        SELECT desc_corta
        INTO   gar_cons[i].desc_tipo_desempleo
        FROM   tab_tipo_desempleo
        WHERE  tipo_desempleo = gar_cons[i].tipo_desempleo

        IF gar_cons[i].tipo_prestacion = gr_prest.desempleo THEN
            SELECT desc_corta
            INTO   gar_cons[i].desc_tipo_pago
            FROM   tab_tipo_pago
            WHERE  tipo_pago = gar_cons[i].tipo_pago
        END IF

        LET i = i + 1

        IF i > 1000 THEN
            ERROR "CAPACIDAD DEL ARREGLO REBASADA, SOLO SE MOSTRARAN LOS PRIMEROS 1000 REGISTROS"
            SLEEP 3
            EXIT FOREACH
        END IF

    END FOREACH

    IF i = 1 THEN
        ERROR " "
        CALL f_error_msg("NO EXISTEN REGISTROS")
        CLOSE WINDOW retm0011
        RETURN
    END IF

    CALL SET_COUNT(i-1)

    ERROR ""

    DISPLAY ARRAY gar_cons TO scr_1.*

        ON KEY (INTERRUPT)
            CLEAR FORM
            EXIT DISPLAY

        ON KEY (CONTROL-B)
            LET arr_c     = ARR_CURR()
            LET v_ejecuta = "fglgo RETM810 ",gar_cons[arr_c].nss CLIPPED," ",
                             gar_cons[arr_c].consecutivo CLIPPED," ",'C' CLIPPED
            RUN v_ejecuta
    END DISPLAY

    CLOSE WINDOW retm0011


END FUNCTION

#---------------------------------------------------------------------------#
# f_modifica : Ejecuta el proceso de modificacion de parciales              #
#---------------------------------------------------------------------------#
FUNCTION f_modifica(x_entrada)

    DEFINE
        pos          INTEGER,
        x_entrada    SMALLINT

    -- -----------------------------------------------------------------------------
    
    CALL f_abre_pant_inicial()

    LET v1ra_vez  = TRUE
    LET INT_FLAG  = FALSE


    DISPLAY " <Esc>  : Consulta " AT 2,1
    DISPLAY " Ctrl-C : Cancelar " AT 2,59


    CONSTRUCT BY NAME cla_whe ON
       nss,
       paterno,
       materno,
       nombres,
       folio_solicitud,
       num_resolucion,
       tipo_prestacion,
       rechazo_cod,
       fecha_captura,
       folio,
       consecutivo

        ON KEY (INTERRUPT)
            LET INT_FLAG = TRUE
            EXIT CONSTRUCT

        ON KEY (ESC)
            LET INT_FLAG = FALSE
            DISPLAY " ENTER  : Selecciona     " AT 2,1
            EXIT CONSTRUCT

    END CONSTRUCT

    IF INT_FLAG = TRUE THEN
        LET INT_FLAG = FALSE
        CLOSE WINDOW retm0011
        RETURN
    END IF

    LET cla_sel = "SELECT A.nss,",
                         "B.n_rfc,",
                         "B.n_unico,",
                         "B.paterno,",
                         "B.materno,",
                         "B.nombres,",
                         "A.folio_solicitud,",
                         "A.fecha_solicitud,",
                         "A.num_resolucion,",
                         "A.tipo_retiro,",
                         "' ',",
                         "A.tipo_prestacion,",
                         "' ',",

                         "A.tipo_desempleo,",
                         "' ',",
                         "A.tipo_pago,",
                         "' ',",
                         "A.salario_base_cot,",
                         "A.pago_desempleo,",

                         "A.fecha_resolucion,",
                         "A.fecha_fall_mat_des,",
                         "A.impt_autorizado,",
                         "A.rechazo_cod,",
                         "' ',",

                         "A.cod_rechazo_ent,",

                         "A.fecha_captura,",
                         "A.fecha_modifica,",
                         "A.fecha_confirma,",
                         "' ',",
                         "A.folio,",
                         "A.consecutivo,",
                         "A.usuario_captura,",
                         "A.usuario_modifica,",
                         "A.usuario_confirma,",
                         "A.estado_solicitud,",
                         "' ',",
                         "A.fecha_envio,",
                         "A.diag_cuenta_ind ",
                  "FROM   ret_parcial A,",
                  "       OUTER afi_mae_afiliado B ",
                  "WHERE  A.nss = B.n_seguro ",
                  "AND    estado_solicitud IN (?,?) ",
                  "AND  ",cla_whe CLIPPED

    PREPARE claexe2 FROM cla_sel
    DECLARE cur_2 CURSOR FOR claexe2

    LET i = 1

    ERROR "PROCESANDO INFORMACION"

    FOREACH cur_2 USING gr_edo.precapturado ,
                        gr_edo.capturado
                  INTO gar_cons[i].*

        SELECT descripcion
        INTO   x_estado_solicitud
        FROM   ret_estado
        WHERE  estado_solicitud = gar_cons[i].estado_solicitud

        LET gar_cons[i].estado_sol_desc = x_estado_solicitud

        IF ((gar_cons[i].estado_solicitud <> gr_edo.capturado) AND 
            (gar_cons[i].estado_solicitud <> gr_edo.precapturado)  ) THEN

            PROMPT "LA SOLICITUD DEL NSS <",gar_cons[i].nss,
                   "> SE ENCUENTRA EN EL ESTADO : ",
                   x_estado_solicitud CLIPPED,
                   " " ATTRIBUTE(REVERSE)
            FOR opc ATTRIBUTE(REVERSE)
            CONTINUE FOREACH
        END IF

{svera
        IF x_entrada = 2 THEN
            SELECT usuario_captura
            INTO   x_usuario
            FROM   ret_parcial
            WHERE  nss = gar_cons[i].nss
            AND    consecutivo = gar_cons[i].consecutivo

            IF x_usuario = gc_usuario THEN
                PROMPT "USUARIO ES EL MISMO DE CAPTURA" ATTRIBUTE (REVERSE)
                       FOR opc ATTRIBUTE (REVERSE)
                EXIT FOREACH
            END IF
        END IF
svera}

        LET gar_cons[i].tipo_retiro = "I"
        SELECT descripcion
        INTO   gar_cons[i].desc_tipo_ret
        FROM   tab_retiro
        WHERE  tipo_retiro = gar_cons[i].tipo_retiro

        SELECT descripcion
        INTO   gar_cons[i].desc_tipo_prest
        FROM   tab_prestacion
        WHERE  tipo_prestacion = gar_cons[i].tipo_prestacion

        SELECT rechazo_desc
        INTO   gar_cons[i].desc_cod_rech
        FROM   tab_rch_marca
        WHERE  rechazo_cod = gar_cons[i].rechazo_cod

        SELECT fecha_pago
        INTO   gar_cons[i].fecha_pago
        FROM   ret_parcial_tx
        WHERE  nss         = gar_cons[i].nss
        AND    consecutivo = gar_cons[i].consecutivo

        SELECT desc_corta
        INTO   gar_cons[i].desc_tipo_desempleo
        FROM   tab_tipo_desempleo
        WHERE  tipo_desempleo = gar_cons[i].tipo_desempleo

        IF gar_cons[i].tipo_prestacion = gr_prest.desempleo THEN
            SELECT desc_corta
            INTO   gar_cons[i].desc_tipo_pago
            FROM   tab_tipo_pago
            WHERE  tipo_pago = gar_cons[i].tipo_pago
        END IF

        IF comando = "C" THEN
           LET gar_cons[i].fecha_confirma   = HOY
           LET gar_cons[i].usuario_confirma = gc_usuario
        ELSE
           LET gar_cons[i].fecha_modifica   = HOY
           LET gar_cons[i].usuario_modifica = gc_usuario
        END IF

        LET i = i + 1

        IF i > 1000 THEN
            ERROR "CAPACIDAD DEL ARREGLO REBASADA, SOLO SE MOSTRARAN LOS PRIMEROS 1000 REGISTROS"
            SLEEP 3
            EXIT FOREACH
        END IF

    END FOREACH

    ERROR ""
    IF (i-1) >= 1 THEN
        CALL SET_COUNT(i-1)

        DISPLAY ARRAY gar_cons TO scr_1.*
            ON KEY(INTERRUPT)
                LET INT_FLAG = TRUE
                EXIT DISPLAY

            ON KEY (CONTROL-M)
                LET pos = ARR_CURR()

                IF x_entrada = 1 THEN
                    DISPLAY " <Esc>  : Modifica          " AT 2,1
                ELSE
                    DISPLAY " <Esc>  : Confirma          " AT 2,1
                END IF

                EXIT DISPLAY

            ON KEY (CONTROL-B)
                LET arr_c     =  arr_curr()
                LET v_ejecuta = "fglgo RETM810 ",gar_cons[arr_c].nss CLIPPED," ",
                                gar_cons[arr_c].consecutivo CLIPPED," ",'C' CLIPPED
                RUN v_ejecuta
        END DISPLAY
    ELSE
        LET INT_FLAG = TRUE
        ERROR " "
        CALL f_error_msg("NO EXISTEN REGISTROS")
    END IF

    IF INT_FLAG = TRUE THEN
        LET INT_FLAG = FALSE
        CLOSE WINDOW retm0011
        RETURN
    END IF

    CALL f_construccion(gar_cons[pos].*)

    CLOSE WINDOW retm0011

END FUNCTION


#---------------------------------------------------------------------------#
# f_elimina : Ejecuta la eliminacion de registros de retiros parciales      #
#---------------------------------------------------------------------------#
FUNCTION f_elimina()

    DEFINE
        ls_edo_elimina      SMALLINT

    -- -----------------------------------------------------------------------------
    
    CALL f_abre_pant_inicial()

    DISPLAY " <Esc>  : Consulta             " AT 2,1
    DISPLAY " Ctrl-C : Cancelar" AT 2,59

    IF gs_del_confirm <> 0 THEN
        LET ls_edo_elimina = gr_edo.confirmado
    ELSE
        LET ls_edo_elimina = gr_edo.capturado
    END IF

    LET INT_FLAG = FALSE

    CLEAR FORM
    ERROR "ESTABLEZCA EL CRITERIO DE BUSQUEDA DE LOS REGISTROS A ELIMINAR"

    CONSTRUCT BY NAME cla_whe ON
              nss,
              paterno,
              materno,
              nombres,
              folio_solicitud,
              num_resolucion,
              tipo_prestacion,
              rechazo_cod,
              fecha_captura,
              folio,
              consecutivo

        ON KEY (INTERRUPT)
            LET INT_FLAG = TRUE
            EXIT CONSTRUCT

        ON KEY (ESC)
            LET INT_FLAG = FALSE
            EXIT CONSTRUCT

    END CONSTRUCT

    MESSAGE ""

    IF INT_FLAG = TRUE THEN
        LET INT_FLAG = FALSE
        CLOSE WINDOW retm0011
        RETURN
    END IF

    LET cla_sel = "SELECT A.nss,",
                         "B.n_rfc,",
                         "B.n_unico,",
                         "B.paterno,",
                         "B.materno,",
                         "B.nombres,",
                         "A.folio_solicitud,",
                         "A.fecha_solicitud,",
                         "A.num_resolucion,",
                         "A.tipo_retiro,",
                         "' ',",
                         "A.tipo_prestacion,",
                         "' ',",
                         "A.tipo_desempleo,",
                         "' ',",
                         "A.tipo_pago,",
                         "' ',",
                         "A.salario_base_cot,",
                         "A.pago_desempleo,",
                         "A.fecha_resolucion,",
                         "A.fecha_fall_mat_des,",
                         "A.impt_autorizado,",
                         "A.rechazo_cod,",
                         "' ',",
                         "A.cod_rechazo_ent,",

                         "A.fecha_captura,",
                         "A.fecha_modifica,",
                         "A.fecha_confirma,",
                         "' ',",
                         "A.folio,",
                         "A.consecutivo,",
                         "A.usuario_captura,",
                         "A.usuario_modifica,",
                         "A.usuario_confirma,",
                         "A.estado_solicitud,",
                         "' ',",
                         "A.fecha_envio,",
                         "A.diag_cuenta_ind ",
                  " FROM   ret_parcial A,",
                  " OUTER  afi_mae_afiliado B ",
                  " WHERE  A.nss = B.n_seguro ",
                  " AND    estado_solicitud IN (?,?,?) ",
                  " AND  ", cla_whe CLIPPED

    PREPARE claexe4 FROM cla_sel
    DECLARE cur_4 CURSOR FOR claexe4

    LET i = 1

    FOREACH cur_4 USING gr_edo.precapturado ,
                        gr_edo.capturado    ,
                        gr_edo.confirmado
                  INTO gar_cons[i].*

        SELECT descripcion
        INTO   x_estado_solicitud
        FROM   ret_estado
        WHERE  estado_solicitud = gar_cons[i].estado_solicitud

        LET gar_cons[i].estado_sol_desc = x_estado_solicitud

        IF gar_cons[i].estado_solicitud > ls_edo_elimina THEN

            PROMPT "SOLICITUD DEL NSS <",gar_cons[i].nss,
                   "> SE ENCUENTRA EN EL ESTADO : ",
                     x_estado_solicitud CLIPPED,
                    " " ATTRIBUTE(REVERSE)
            FOR opc ATTRIBUTE(REVERSE)
            CONTINUE FOREACH
        END IF

        LET gar_cons[i].tipo_retiro = "I"

        SELECT descripcion
        INTO   gar_cons[i].desc_tipo_ret
        FROM   tab_retiro
        WHERE  tipo_retiro = gar_cons[i].tipo_retiro

        SELECT descripcion
        INTO   gar_cons[i].desc_tipo_prest
        FROM   tab_prestacion
        WHERE  tipo_prestacion = gar_cons[i].tipo_prestacion

        SELECT rechazo_desc
        INTO   gar_cons[i].desc_cod_rech
        FROM   tab_rch_marca
        WHERE  rechazo_cod = gar_cons[i].rechazo_cod

        SELECT fecha_pago
        INTO   gar_cons[i].fecha_pago
        FROM   ret_parcial_tx
        WHERE  nss         = gar_cons[i].nss
        AND    consecutivo = gar_cons[i].consecutivo

        LET i = i + 1

        IF i > 1000 THEN
            ERROR "CAPACIDAD DEL ARREGLO REBASADA, SOLO SE MOSTRARAN LOS PRIMEROS 1000 REGISTROS"
            SLEEP 3
            EXIT FOREACH
        END IF

    END FOREACH

    ERROR ""

    IF i = 1 THEN
        ERROR " "
        CALL f_error_msg("NO EXISTEN REGISTROS")
        CLOSE WINDOW retm0011
        RETURN
    END IF

    DISPLAY " ENTER  : Elimina      " AT 2,1
    CALL SET_COUNT(i-1)

    DISPLAY ARRAY gar_cons TO scr_1.*

        ON KEY (CONTROL-M)
            LET i = ARR_CURR()

            PROMPT "¿DESEA ELIMINAR EL REGISTRO? (S/N) " FOR opc

            IF opc MATCHES '[Ss]' THEN
                CALL f_elimina_operacion(gar_cons[i].nss            , 
                                         gar_cons[i].consecutivo    ,
                                         gar_cons[i].tipo_prestacion
                                        )
                CALL f_error_msg("REGISTRO ELIMINADO")
            ELSE
                CALL f_error_msg("ELIMINACION CANCELADA")
            END IF

            CLEAR FORM
            EXIT DISPLAY

        ON KEY (INTERRUPT)
            CLEAR FORM
            EXIT DISPLAY

    END DISPLAY

    CLOSE WINDOW retm0011

END FUNCTION

#---------------------------------------------------------------------------#
# f_construccion : Ejecuta la captura especial para modificar cuentas       #
#---------------------------------------------------------------------------#
FUNCTION f_construccion(pr_reg)

    DEFINE pr_reg RECORD
        nss                     LIKE ret_parcial.nss                ,
        rfc                     CHAR(13)                            ,
        curp                    CHAR(18)                            ,
        paterno                 CHAR(40)                            ,
        materno                 CHAR(40)                            ,
        nombres                 CHAR(40)                            ,
        folio_solicitud         LIKE ret_parcial.folio_solicitud    ,
        fecha_solicitud         LIKE ret_parcial.fecha_solicitud    ,
        num_resolucion          LIKE ret_parcial.num_resolucion     ,
        tipo_retiro             LIKE ret_parcial.tipo_retiro        ,
        desc_tipo_ret           CHAR(31)                            ,
        tipo_prestacion         LIKE ret_parcial.tipo_prestacion    ,
        desc_tipo_prest         CHAR(31)                            ,
        tipo_desempleo          LIKE ret_parcial.tipo_desempleo     ,
        desc_tipo_desempleo     CHAR(15)                            ,
        tipo_pago               LIKE ret_parcial.tipo_pago          ,
        desc_tipo_pago          CHAR(15)                            ,
        salario_base_cot        LIKE ret_parcial.salario_base_cot   ,
        pago_desempleo          LIKE ret_parcial.pago_desempleo     ,
        fecha_resolucion        LIKE ret_parcial.fecha_resolucion   ,
        fecha_fall_mat_des      LIKE ret_parcial.fecha_fall_mat_des ,
        impt_autorizado         LIKE ret_parcial.impt_autorizado    ,
        rechazo_cod             LIKE ret_parcial.rechazo_cod        ,
        desc_cod_rech           CHAR(50)                            ,
        cod_rechazo_ent         LIKE ret_parcial.cod_rechazo_ent    ,
        fecha_captura           LIKE ret_parcial.fecha_captura      ,
        fecha_modifica          LIKE ret_parcial.fecha_modifica     ,
        fecha_confirma          LIKE ret_parcial.fecha_confirma     ,
        fecha_pago              LIKE ret_parcial_tx.fecha_pago      ,
        folio                   LIKE ret_parcial.folio              ,
        consecutivo             LIKE ret_parcial.consecutivo        ,
        usuario_captura         LIKE ret_parcial.usuario_captura    ,
        usuario_modifica        LIKE ret_parcial.usuario_modifica   ,
        usuario_confirma        LIKE ret_parcial.usuario_confirma   ,
        estado_solicitud        LIKE ret_parcial.estado_solicitud   ,
        estado_sol_desc         CHAR(40)                            ,
        fecha_envio             LIKE ret_parcial.fecha_envio        ,
        diag_cuenta_ind         CHAR(3)
    END RECORD

    DEFINE pr_importes RECORD
        ret_97              DECIMAL(16,2),
        cv                  DECIMAL(16,2),
        cs                  DECIMAL(16,2),
        total_rcv           DECIMAL(16,2) 
    END RECORD
    
    DEFINE lr_ctr_pago RECORD LIKE ret_ctr_pago.*

    DEFINE ls_siefore LIKE dis_cuenta.siefore

    DEFINE lr_datos_comp RECORD
        monto_pagado    LIKE ret_resol_comp.monto_pagado ,
        saldo_rcv       LIKE ret_resol_comp.saldo_rcv
    END RECORD

    DEFINE lr_sbc RECORD
        retiro_a       LIKE ret_parcial_resol.salario_base_a ,
        retiro_b       LIKE ret_parcial_resol.salario_base_b
    END RECORD

    DEFINE
        ls_edo_tmp                  ,
        ls_entidad                  ,
        ls_salida                   ,
        ls_cod_rechazo_ent          ,
        ls_error                    ,
        sw_1                        SMALLINT

    DEFINE
        li_cont                     ,
        li_dias_nat                 INTEGER

    DEFINE
        ld_saldo_mat                DECIMAL(16,6),
        ld_sal_min                  DECIMAL(10,2)

    DEFINE
        ldt_fec_modifica            ,
        ldt_inicio_vigencia         ,
        ldt_fin_vigencia            ,
        ldt_resp_mat_des            DATE

    DEFINE
        lc_user_modifica            CHAR(12),
        lc_diag_procesar            CHAR(03)

    -- -----------------------------------------------------------------------------
           
    LET ldt_fec_modifica    = pr_reg.fecha_modifica
    LET lc_user_modifica    = pr_reg.usuario_modifica
    LET sw_1                = 1
    LET ld_sal_min          = 0
    LET ls_salida           = 0
    LET li_cont             = 0

    INITIALIZE lr_ctr_pago.* TO NULL

    -- Obtenemos la siefore actual del NSS
    LET ls_siefore = f_obtiene_siefore_act(pr_reg.nss)

    -- Banderas para poder realizar la recaptura del tipo de desempleo y del sbc
    LET gs_recap_tipo_des   = 0
    LET gi_tipo_desempleo   = pr_reg.tipo_desempleo

    LET gs_recap_sbc        = 0
    LET gd_sal_base_cot     = pr_reg.salario_base_cot


    INPUT BY NAME pr_reg.* WITHOUT DEFAULTS

        -------------------------------------
        BEFORE FIELD nss
        -------------------------------------
            LET pr_importes.ret_97      = 0
            LET pr_importes.cv          = 0
            LET pr_importes.cs          = 0
            LET pr_importes.total_rcv   = 0
            
            CALL f_obtiene_rcv(pr_reg.nss, HOY) RETURNING pr_importes.*

            IF pr_importes.total_rcv <= 0 THEN
                CALL f_error_msg("CUENTA CON SALDO CERO")
                LET ls_salida = 1
                EXIT INPUT
            ELSE
                IF (pr_reg.tipo_prestacion = gr_prest.desempleo AND pr_reg.tipo_desempleo <> "D") THEN
                    SELECT *
                    INTO   lr_ctr_pago.*
                    FROM   ret_ctr_pago
                    WHERE  nss          = pr_reg.nss
                    AND    consecutivo  = pr_reg.consecutivo
                    
                    IF lr_ctr_pago.mto_pago IS NULL OR lr_ctr_pago.mto_pago = 0 THEN
                        CALL f_error_msg("CUENTA SIN PAGOS CALCULADOS, VERIFIQUE CON SISTEMAS")
                        LET ls_salida = 1
                        EXIT INPUT
                    END IF
                END IF
            END IF

            NEXT FIELD folio_solicitud

        -------------------------------------
        AFTER FIELD fecha_solicitud
        -------------------------------------
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR
                FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD PREVIOUS
            ELSE
                IF pr_reg.fecha_solicitud IS NULL THEN
                    ERROR "CAMPO NO PUEDE SER NULO"
                    NEXT FIELD fecha_solicitud
                END IF
            END IF

            IF pr_reg.fecha_solicitud > HOY THEN
                ERROR "FECHA NO PUEDE SER MAYOR AL DIA DE HOY"
                NEXT FIELD fecha_solicitud
            END IF

            IF HOY <> pr_reg.fecha_solicitud THEN
                EXECUTE eje_vig_solicitud USING HOY         ,
                                                gs_num_dias
                                          INTO  gdt_lim_vigencia

                IF pr_reg.fecha_solicitud < gdt_lim_vigencia THEN
                    WHILE TRUE
                        PROMPT "FECHA DE SOLICITUD SIN VIGENCIA DE ENVIO, ¿DESEA CONTINUAR? (S/N) " FOR opc
                        IF opc MATCHES "[SsNn]" THEN
                            EXIT WHILE
                        END IF
                    END WHILE

                    IF opc MATCHES "[Nn]" THEN
                        EXIT INPUT
                    END IF
                END IF
            END IF

        -------------------------------------
        BEFORE FIELD num_resolucion
        -------------------------------------
            LET arr_c = ARR_CURR()

        -------------------------------------
        AFTER FIELD num_resolucion
        -------------------------------------
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR
               FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
               NEXT FIELD PREVIOUS
            END IF

            IF f_valida_num_resol(pr_reg.nss, pr_reg.num_resolucion) THEN
                ERROR " EL NUMERO RESOLUCION NO EXISTE EN LA BASE DE DATOS DE PARCIALES"
                NEXT FIELD num_resolucion
            END IF

            SELECT MAX(fecha_ini_vigencia)
            INTO   ldt_inicio_vigencia
            FROM   ret_parcial_resol
            WHERE  nss            = pr_reg.nss
            AND    num_resolucion = pr_reg.num_resolucion

            IF STATUS <> NOTFOUND THEN
                ERROR " NSS, YA EXISTE EN LA BASE DE DATOS DE PARCIALES "
            END IF

            IF ldt_inicio_vigencia IS NOT NULL THEN
                SELECT diag_procesar      ,
                       fecha_fin_vigencia
                INTO   lc_diag_procesar     ,
                       ldt_fin_vigencia
                FROM   ret_parcial_resol
                WHERE  nss                = pr_reg.nss
                AND    num_resolucion     = pr_reg.num_resolucion
                AND    fecha_ini_vigencia = ldt_inicio_vigencia
                GROUP BY nss                ,
                         num_resolucion     ,
                         diag_procesar      ,
                         fecha_fin_vigencia

                IF lc_diag_procesar = "400" THEN
                    IF ldt_fin_vigencia > pr_reg.fecha_solicitud THEN
                        NEXT FIELD tipo_prestacion
                    ELSE
                        ERROR "  LA FECHA DE VIGENCIA YA EXPIRO  "
                        EXIT INPUT
                    END IF
                END IF
            END IF

        -------------------------------------
        BEFORE FIELD tipo_prestacion
        -------------------------------------
            LET arr_c = ARR_CURR()

        -------------------------------------
        AFTER FIELD tipo_prestacion
        -------------------------------------
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR
                FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD PREVIOUS
            ELSE
                IF pr_reg.tipo_prestacion IS NULL THEN
                    ERROR "CAMPO NO PUEDE SER NULO"
                    NEXT FIELD tipo_prestacion
                END IF
            END IF

            IF f_valida_tipo_pres(pr_reg.nss, pr_reg.num_resolucion, pr_reg.tipo_prestacion) THEN
                ERROR " NSS NO EXISTE EN LA BASE DE DATOS DE PARCIALES "
                NEXT FIELD tipo_prestacion
            END IF

            SELECT descripcion
            INTO   pr_reg.desc_tipo_prest
            FROM   tab_prestacion
            WHERE  tipo_prestacion = pr_reg.tipo_prestacion

            DISPLAY BY NAME pr_reg.tipo_prestacion,
                            pr_reg.desc_tipo_prest

            IF pr_reg.tipo_prestacion = gr_prest.desempleo THEN
                LET pr_reg.impt_autorizado = 0
                DISPLAY BY NAME pr_reg.impt_autorizado
            ELSE
                LET pr_reg.salario_base_cot = 0
                LET pr_reg.pago_desempleo   = 0
                DISPLAY BY NAME pr_reg.salario_base_cot,
                                pr_reg.pago_desempleo
            END IF

            IF pr_reg.tipo_prestacion = gr_prest.desempleo THEN -- DESEMPLEO
                NEXT FIELD tipo_desempleo
            ELSE
                IF f_valida_matrimonio_ant(pr_reg.nss) THEN
                    CALL f_error_msg("SOLO PUEDE TENER UN RETIRO POR MATRIMONIO EN LA VIDA")
                    LET ls_salida = 1
                    EXIT INPUT
                END IF

                LET ld_saldo_mat = f_obten_monto_mat(pr_reg.nss)

                IF ld_saldo_mat <= 0 THEN
                   CALL f_error_msg("CUENTA CON SALDO CERO")
                   LET ls_salida = 1
                   EXIT INPUT
                END IF

                NEXT FIELD fecha_resolucion

            END IF

        -------------------------------------
        BEFORE FIELD tipo_desempleo
        -------------------------------------
            LET arr_c = ARR_CURR()

            IF pr_reg.tipo_prestacion <> gr_prest.desempleo THEN
                NEXT FIELD fecha_resolucion
            END IF

        -------------------------------------
        AFTER FIELD tipo_desempleo
        -------------------------------------
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR
                FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD PREVIOUS
            ELSE
                IF pr_reg.tipo_desempleo IS NULL THEN
                    CALL f_despliega_tipo_desempleo()
                        RETURNING pr_reg.tipo_desempleo,
                                  pr_reg.desc_tipo_desempleo
                END IF
            END IF

            -- Se valida por si no hace la captura por catalogo
            SELECT desc_corta
            INTO   pr_reg.desc_tipo_desempleo
            FROM   tab_tipo_desempleo
            WHERE  tipo_desempleo = pr_reg.tipo_desempleo

            IF STATUS = NOTFOUND THEN
                ERROR "TIPO DE DESEMPLEO INEXISTENTE"
                NEXT FIELD tipo_desempleo
            ELSE
                DISPLAY BY NAME pr_reg.desc_tipo_desempleo
            END IF

            IF pr_reg.tipo_desempleo IS NOT NULL THEN

                CALL f_obtiene_sbc(pr_reg.nss, pr_reg.num_resolucion)
                    RETURNING lr_sbc.*

                -- Validaciones para evitar que se despliegue de nuevo la pantalla
                -- de detalle de pagos si no se ha cambiado de tipo
                IF gs_recap_tipo_des = 0 THEN
                    IF pr_reg.tipo_desempleo <> gi_tipo_desempleo THEN
                        LET gs_recap_tipo_des = 1
                    END IF
                END IF

                IF gs_recap_tipo_des <> 0 THEN
                    LET gs_recap_tipo_des   = 0
                    LET gi_tipo_desempleo   = pr_reg.tipo_desempleo

                    IF f_valida_5anios_des(pr_reg.nss) AND pr_reg.tipo_desempleo <> "C" THEN
                        #-- Validacion temporal para metlife
                        #-- Se pregunta si desea seguir o no capturando
                        WHILE TRUE
                            PROMPT "EL NSS YA CUENTA CON UN RETIRO PREVIO POR DESEMPLEO, ¿DESEA CONTINUAR? (S/N) "
                            FOR CHAR opc

                            IF opc MATCHES "[SsNn]" THEN
                                IF opc MATCHES "[Ss]" THEN
                                    EXIT WHILE
                                ELSE
                                    LET ls_salida = 1
                                    EXIT INPUT
                                END IF
                            END IF
                        END WHILE

{
                        ERROR "EL NSS YA CUENTA CON UN RETIRO PREVIO POR DESEMPLEO , PULSE ENTER P/SALIR"

                        PROMPT "" FOR CHAR opc
                        DELETE
                        FROM   ret_consecutivo
                        WHERE  consecutivo = pr_reg.consecutivo

                        LET ls_salida = 1
                        EXIT INPUT
}
                    END IF

                    CASE pr_reg.tipo_desempleo
                        WHEN "A"
                            LET pr_reg.tipo_pago        = 1
                            LET pr_reg.salario_base_cot = lr_sbc.retiro_a

                            CALL f_tipo_pago_1(pr_reg.nss            ,
                                               pr_reg.consecutivo    ,
                                               lr_sbc.retiro_a       )
                            RETURNING lr_ctr_pago.*

                            LET pr_reg.pago_desempleo = lr_ctr_pago.mto_pago

                            CALL f_despliega_tipo_1(lr_ctr_pago.mto_1   ,
                                                    lr_ctr_pago.mto_2   ,
                                                    lr_ctr_pago.mto_pago )

                            DISPLAY BY NAME pr_reg.tipo_pago         ,
                                            pr_reg.desc_tipo_pago    ,
                                            pr_reg.pago_desempleo    ,
                                            pr_reg.salario_base_cot

                            CALL f_valida_suficiencia(pr_reg.pago_desempleo, pr_importes.total_rcv)
                                RETURNING pr_reg.pago_desempleo

                            NEXT FIELD fecha_resolucion

                        WHEN "B"
                            CALL f_det_pago_ab(pr_reg.nss                 ,
                                               pr_reg.consecutivo         ,
                                               pr_reg.num_resolucion      ,
                                               pr_reg.tipo_desempleo      ,
                                               pr_reg.tipo_pago           ,
                                               pr_importes.total_rcv         ,
                                               0                         ,
                                               lr_sbc.retiro_a           ,
                                               lr_sbc.retiro_b
                                               )
                                RETURNING pr_reg.pago_desempleo   ,
                                          pr_reg.salario_base_cot ,
                                          pr_reg.tipo_pago        ,
                                          pr_reg.desc_tipo_pago   ,
                                          lr_ctr_pago.*

                            LET pr_reg.pago_desempleo = lr_ctr_pago.mto_pago

                            DISPLAY BY NAME pr_reg.salario_base_cot  ,
                                            pr_reg.tipo_pago         ,
                                            pr_reg.pago_desempleo    ,
                                            pr_reg.desc_tipo_pago

                            CALL f_valida_suficiencia(pr_reg.pago_desempleo, pr_importes.total_rcv)
                                RETURNING pr_reg.pago_desempleo

                            NEXT FIELD fecha_resolucion

                        WHEN "C"
                            -- Pago complementario
                            CALL f_valida_resol_comp(pr_reg.nss, pr_reg.num_resolucion)
                                RETURNING lr_datos_comp.*, ls_error

                            IF ls_error <> 0 THEN
                                ERROR "EL NSS NO TIENE UN RETIRO COMPLEMENTARIO..."
                                EXIT INPUT
                            ELSE
                                IF lr_sbc.retiro_a = 0 AND lr_sbc.retiro_b = 0 THEN
                                    CALL f_captura_salarios() RETURNING lr_sbc.*
                                END IF

                                CALL f_retiros_complementarios(pr_reg.nss                 ,
                                                               pr_reg.consecutivo         ,
                                                               pr_reg.num_resolucion      ,
                                                               pr_reg.tipo_desempleo      ,
                                                               pr_reg.tipo_pago           ,
                                                               lr_datos_comp.saldo_rcv   ,
                                                               lr_datos_comp.monto_pagado,
                                                               lr_sbc.retiro_a           ,
                                                               lr_sbc.retiro_b           ,
                                                               lr_datos_comp.monto_pagado
                                                               )
                                    RETURNING pr_reg.pago_desempleo   ,
                                              pr_reg.salario_base_cot ,
                                              pr_reg.tipo_pago        ,
                                              pr_reg.desc_tipo_pago   ,
                                              lr_ctr_pago.*

                                DISPLAY BY NAME pr_reg.salario_base_cot  ,
                                                pr_reg.tipo_pago         ,
                                                pr_reg.pago_desempleo    ,
                                                pr_reg.desc_tipo_pago

                                CALL f_valida_suficiencia(pr_reg.pago_desempleo, pr_importes.total_rcv)
                                    RETURNING pr_reg.pago_desempleo

                                NEXT FIELD fecha_resolucion
                            END IF

                        WHEN "D"
                            -- Forma de pago anterior
                            CALL f_valida_resol_alterno(pr_reg.nss, pr_reg.num_resolucion)
                                RETURNING lr_sbc.retiro_a   ,
                                          pr_reg.tipo_pago   ,
                                          ls_error

                            IF ls_error <> 0 THEN
                                ERROR "NO EXISTE NUMERO DE RESOLUCION EN RESOLUCIONES TRANSITORIAS"
                                SLEEP 3
                            ELSE
                                IF pr_reg.tipo_pago = 2 THEN
                                    ERROR "INGRESE EL SALARIO BASE COTIZACION DE LA SOLICITUD "
                                    SLEEP 2
                                END IF
                            END IF

                            INITIALIZE pr_reg.desc_tipo_pago TO NULL
                            DISPLAY BY NAME pr_reg.tipo_pago,
                                            pr_reg.desc_tipo_pago
                            NEXT FIELD salario_base_cot
                    END CASE

                    -- Valida suficiencia de saldos
                    IF pr_reg.pago_desempleo > pr_importes.total_rcv THEN
                        ERROR "EL MONTO A PAGAR REBASA EL MONTO EN LA CUENTA, SE PAGARAN :", pr_importes.total_rcv
                        SLEEP 3
                    END IF
                ELSE
                    IF pr_reg.tipo_desempleo = "D" THEN
                        NEXT FIELD salario_base_cot
                    ELSE
                        NEXT FIELD fecha_resolucion
                    END IF
                END IF

            END IF

        -------------------------------------
        BEFORE FIELD tipo_pago
        -------------------------------------
            LET arr_c = ARR_CURR()

        -------------------------------------
        AFTER FIELD tipo_pago
        -------------------------------------
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR
                FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD PREVIOUS
            ELSE
                IF pr_reg.tipo_pago IS NULL THEN
                    ERROR "CAMPO NO PUEDE SER NULO"
                    NEXT FIELD tipo_pago
                END IF
            END IF

            SELECT desc_corta
            INTO   pr_reg.desc_tipo_pago
            FROM   tab_tipo_pago
            WHERE  tipo_pago = pr_reg.tipo_pago

            IF STATUS = NOTFOUND THEN
                ERROR "TIPO DE PAGO INEXISTENTE"
                NEXT FIELD tipo_pago
            ELSE
                DISPLAY BY NAME pr_reg.tipo_pago     ,
                                pr_reg.pago_desempleo,
                                pr_reg.desc_tipo_pago

                NEXT FIELD fecha_resolucion
            END IF

        -------------------------------------
        BEFORE FIELD salario_base_cot
        -------------------------------------
            LET arr_c = ARR_CURR()

            IF pr_reg.tipo_desempleo <> "D" THEN
                NEXT FIELD fecha_resolucion
            END IF

        -------------------------------------
        AFTER FIELD salario_base_cot
        -------------------------------------
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR
               FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD tipo_desempleo
            END IF

            IF pr_reg.salario_base_cot IS NULL OR pr_reg.salario_base_cot = 0 THEN
                ERROR "CAMPO NO PUEDE SER NULO"
                NEXT FIELD salario_base_cot
            ELSE
                IF gs_recap_sbc = 0 THEN
                    IF pr_reg.salario_base_cot <> gd_sal_base_cot THEN
                        LET gs_recap_sbc = 1
                    END IF
                END IF

                IF gs_recap_sbc <> 0 THEN
                    LET gs_recap_sbc   = 0
                    LET gd_sal_base_cot   = pr_reg.salario_base_cot

                    CALL f_valida_resol_alterno(pr_reg.nss, pr_reg.num_resolucion)
                        RETURNING lr_sbc.retiro_a   ,
                                  pr_reg.tipo_pago  ,
                                  ls_error

                    CALL f_retiros_anteriores(pr_reg.nss                ,
                                              pr_reg.consecutivo        ,
                                              pr_reg.num_resolucion     ,
                                              pr_reg.tipo_desempleo     ,
                                              pr_reg.tipo_pago          ,
                                              pr_importes.total_rcv     ,
                                              0                         ,
                                              lr_sbc.retiro_a           ,
                                              pr_reg.salario_base_cot
                                             )
                        RETURNING pr_reg.pago_desempleo  ,
                                  pr_reg.salario_base_cot,
                                  pr_reg.tipo_pago       ,
                                  pr_reg.desc_tipo_pago

                ELSE
                    NEXT FIELD fecha_resolucion
                END IF

            END IF

            DISPLAY BY NAME pr_reg.pago_desempleo    ,
                            pr_reg.salario_base_cot  ,
                            pr_reg.desc_tipo_pago

        -------------------------------------
        BEFORE FIELD pago_desempleo
        -------------------------------------
            LET arr_c = ARR_CURR()

            IF pr_reg.tipo_pago > 2 THEN
                NEXT FIELD nss
            END IF


        -------------------------------------
        AFTER FIELD pago_desempleo
        -------------------------------------
            IF pr_reg.tipo_prestacion = gr_prest.desempleo AND
               (pr_reg.pago_desempleo IS NULL OR pr_reg.pago_desempleo = 0) THEN
                ERROR "CAMPO NO PUEDE SER NULO"
                NEXT FIELD salario_base_cot
            END IF

        -------------------------------------
        BEFORE FIELD fecha_resolucion
        -------------------------------------
            LET arr_c = ARR_CURR()

        -------------------------------------
        AFTER FIELD fecha_resolucion
        -------------------------------------
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR
                FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                IF pr_reg.tipo_prestacion = gr_prest.matrimonio THEN
                    NEXT FIELD tipo_prestacion
                ELSE
                    IF pr_reg.tipo_desempleo = "D" THEN
                        NEXT FIELD salario_base_cot
                    END IF
                END IF
            END IF

            IF pr_reg.tipo_prestacion = gr_prest.desempleo THEN
                IF (HOY - gs_dias_nat_des) > pr_reg.fecha_resolucion THEN
                    ERROR "FECHA NO PUEDE SER ANTERIOR A 120 DIAS A PARTIR DE HOY"
                    NEXT FIELD fecha_resolucion
                END IF
            END IF

            IF pr_reg.fecha_resolucion > HOY THEN
                ERROR "FECHA NO PUEDE SER MAYOR AL DIA DE HOY"
                NEXT FIELD fecha_resolucion
            END IF

            LET li_dias_nat = pr_reg.fecha_resolucion - pr_reg.fecha_solicitud

            IF pr_reg.tipo_prestacion = gr_prest.desempleo THEN
                IF li_dias_nat > gs_dias_nat_des THEN
                    ERROR "FECHA RESOLUCION VS FECHA SOLICITUD PASA DE 120 DIAS NATURALES PULSE ENTER P/SALIR"
                    NEXT FIELD fecha_resolucion
                END IF
            ELSE
                IF li_dias_nat > gs_dias_nat_mat THEN
                    ERROR "FECHA RESOLUCION VS FECHA SOLICITUD PASA DE 60 DIAS NATURALES "
                    NEXT FIELD fecha_resolucion
                END IF
            END IF

        -------------------------------------
        BEFORE FIELD fecha_fall_mat_des
        -------------------------------------
            LET arr_c = ARR_CURR()

        -------------------------------------
        AFTER FIELD fecha_fall_mat_des
        -------------------------------------
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") OR
                FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD fecha_resolucion
            ELSE
                IF pr_reg.fecha_fall_mat_des IS NULL THEN
                    ERROR "CAMPO NO PUEDE SER NULO"
                    NEXT FIELD fecha_fall_mat_des
                END IF
            END IF

            IF pr_reg.fecha_fall_mat_des > pr_reg.fecha_resolucion THEN
                ERROR "ESTA FECHA NO PUEDE SER MAYOR A LA FECHA RESOLUCION"
                NEXT FIELD fecha_fall_mat_des
            END IF

            IF pr_reg.fecha_fall_mat_des > HOY THEN
                ERROR "FECHA NO PUEDE SER MAYOR AL DIA DE HOY"
                NEXT FIELD fecha_fall_mat_des
            END IF

            IF pr_reg.fecha_fall_mat_des > pr_reg.fecha_solicitud THEN
                ERROR "ESTA FECHA NO PUEDE SER MAYOR A LA FECHA SOLICITUD"
                NEXT FIELD fecha_fall_mat_des
            END IF

            IF pr_reg.tipo_prestacion = gr_prest.desempleo THEN
                NEXT FIELD nss
            ELSE
                IF sw_1 = 1 THEN
                    LET sw_1             = 0
                    LET ldt_resp_mat_des = pr_reg.fecha_fall_mat_des
                END IF

                IF pr_reg.fecha_fall_mat_des <> ldt_resp_mat_des THEN
                    LET v1ra_vez           = TRUE
                    LET ldt_resp_mat_des = pr_reg.fecha_fall_mat_des
                END IF

                NEXT FIELD impt_autorizado
            END IF

        -------------------------------------
        BEFORE FIELD impt_autorizado
        -------------------------------------
            LET arr_c = ARR_CURR()
            IF v1ra_vez THEN
                LET ano = HOY USING "0101YYYY"

                IF pr_reg.fecha_fall_mat_des >= ano THEN
                    SELECT monto_sm
                    INTO   ld_sal_min
                    FROM   tabsalario_minimo2
                    WHERE  fecha_desde_sm <= pr_reg.fecha_fall_mat_des
                    AND    fecha_hasta_sm IS NULL
                    AND    zona_cod        = "A"

                    IF ld_sal_min = 0 OR ld_sal_min IS NULL THEN
                        CALL f_error_msg("NO EXISTE SALARIO MINIMO PARA ESTE AÑO")
                        CLEAR FORM
                        EXIT PROGRAM
                    ELSE
                        LET pr_reg.impt_autorizado = ld_sal_min * gs_dias_sm_mat
                        DISPLAY BY NAME pr_reg.impt_autorizado
                    END IF
                ELSE
                    SELECT monto_sm
                    INTO   ld_sal_min
                    FROM   tabsalario_minimo2
                    WHERE  fecha_desde_sm <= pr_reg.fecha_fall_mat_des
                    AND    fecha_hasta_sm >= pr_reg.fecha_fall_mat_des
                    AND    zona_cod        = "A"

                    IF ld_sal_min = 0 OR ld_sal_min IS NULL THEN
                        CALL f_error_msg("NO EXISTE SALARIO MINIMO PARA ESTE AÑO")
                        CLEAR FORM
                        EXIT PROGRAM
                    ELSE
                        LET pr_reg.impt_autorizado = ld_sal_min * gs_dias_sm_mat
                        DISPLAY BY NAME pr_reg.impt_autorizado
                    END IF
                END IF
            END IF

            LET v1ra_vez = FALSE

        -------------------------------------
        AFTER FIELD impt_autorizado
        -------------------------------------
            LET impt_actual = pr_reg.impt_autorizado
            IF FGL_LASTKEY() = FGL_KEYVAL("UP")   OR
                FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD PREVIOUS
            ELSE
                IF pr_reg.impt_autorizado IS NULL OR
                   pr_reg.impt_autorizado = 0 THEN
                    ERROR "CAMPO NO PUEDE SER NULO"
                    NEXT FIELD impt_autorizado
                END IF
            END IF

            IF pr_reg.impt_autorizado > ld_saldo_mat THEN
                ERROR "EL IMPORTE DIGITADO NO PUEDE SER MAYOR QUE EL SALDO ",
                       ld_saldo_mat
                NEXT FIELD impt_autorizado
            END IF

            NEXT FIELD nss

        -------------------------------------
        ON KEY (CONTROL-F)
        -------------------------------------
            -- Rechazo de solicitud
            IF gs_rechazo <> 0 THEN
                IF xaccion = "F" THEN
                    LET comando = "R"
                    LET arr_c =  arr_curr()
                    CALL f_rechaza_sol(pr_reg.nss             ,
                                       pr_reg.consecutivo     ,
                                       pr_reg.tipo_prestacion ,
                                       pr_reg.tipo_retiro     )
                        RETURNING  codigo,ls_entidad

                    LET pr_reg.cod_rechazo_ent = codigo
                    DISPLAY BY NAME pr_reg.cod_rechazo_ent
                END IF
            END IF

        -------------------------------------
        ON KEY (ESC)
        -------------------------------------
            IF pr_reg.fecha_solicitud IS NULL THEN
               ERROR "CAMPO NO PUEDE SER NULO"
               NEXT FIELD fecha_solicitud
            END IF

            IF pr_reg.fecha_solicitud > HOY THEN
               ERROR "FECHA NO PUEDE SER MAYOR AL DIA DE HOY"
               NEXT FIELD fecha_solicitud
            END IF

            IF HOY <> pr_reg.fecha_solicitud THEN
                EXECUTE eje_vig_solicitud USING HOY         ,
                                                gs_num_dias
                                          INTO  gdt_lim_vigencia

                IF pr_reg.fecha_solicitud < gdt_lim_vigencia THEN
                    WHILE TRUE
                        PROMPT "FECHA DE SOLICITUD SIN VIGENCIA DE ENVIO, ¿DESEA CONTINUAR? (S/N) " FOR opc
                        IF opc MATCHES "[SsNn]" THEN
                            EXIT WHILE
                        END IF
                    END WHILE

                    IF opc MATCHES "[Nn]" THEN
                        LET ls_salida = 1
                        EXIT INPUT
                    END IF
                END IF
            END IF

            IF pr_reg.tipo_prestacion IS NULL THEN
                ERROR "CAMPO NO PUEDE SER NULO"
                NEXT FIELD tipo_prestacion
            END IF

            IF pr_reg.tipo_prestacion = gr_prest.desempleo THEN
                IF (HOY - gs_dias_nat_des) > pr_reg.fecha_resolucion THEN
                    ERROR "FECHA NO PUEDE SER ANTERIOR A 120 DIAS A PARTIR DE HOY"
                    NEXT FIELD fecha_resolucion
                END IF
            END IF

            IF pr_reg.fecha_resolucion > HOY THEN
                ERROR "FECHA NO PUEDE SER MAYOR AL DIA DE HOY"
                NEXT FIELD fecha_resolucion
            END IF

            IF pr_reg.tipo_prestacion = gr_prest.desempleo THEN
                IF li_dias_nat > gs_dias_nat_des THEN
                    ERROR "FECHA RESOLUCION VS FECHA SOLICITUD PASA DE 120 DIAS NATURALES PULSE ENTER P/SALIR"
                    NEXT FIELD fecha_resolucion
                END IF
            ELSE
                IF li_dias_nat > gs_dias_nat_mat THEN
                    ERROR "FECHA RESOLUCION VS FECHA SOLICITUD PASA DE 60 DIAS NATURALES "
                    NEXT FIELD fecha_resolucion
                END IF
            END IF

            IF pr_reg.fecha_fall_mat_des IS NULL THEN
                ERROR "CAMPO NO PUEDE SER NULO"
                NEXT FIELD fecha_fall_mat_des
            END IF
            IF pr_reg.fecha_fall_mat_des > HOY THEN
                ERROR "FECHA NO PUEDE SER MAYOR AL DIA DE HOY"
                NEXT FIELD fecha_fall_mat_des
            END IF
            IF pr_reg.fecha_fall_mat_des > pr_reg.fecha_resolucion THEN
                ERROR "ESTA FECHA NO PUEDE SER MAYOR A LA FECHA RESOLUCION"
                NEXT FIELD fecha_fall_mat_des
            END IF
            IF pr_reg.fecha_fall_mat_des > pr_reg.fecha_solicitud THEN
                ERROR "ESTA FECHA NO PUEDE SER MAYOR A LA FECHA SOLICITUD"
                NEXT FIELD fecha_fall_mat_des
            END IF
            IF pr_reg.impt_autorizado > ld_saldo_mat THEN
                ERROR "EL IMPORTE DIGITADO NO PUEDE SER MAYOR QUE EL SALDO ",
                       ld_saldo_mat
                NEXT FIELD impt_autorizado
            END IF

            IF pr_reg.impt_autorizado IS NULL THEN
               LET pr_reg.impt_autorizado = 0
            END IF

            IF pr_reg.salario_base_cot IS NULL THEN
               LET pr_reg.salario_base_cot = 0
            END IF

            IF pr_reg.pago_desempleo IS NULL THEN
               LET pr_reg.pago_desempleo = 0
            END IF

            IF pr_reg.salario_base_cot IS NULL THEN
               LET pr_reg.salario_base_cot = 0
            END IF

            IF pr_reg.pago_desempleo IS NULL THEN
               LET pr_reg.pago_desempleo = 0
            END IF

            IF gs_rechazo <> 0 THEN
                IF comando ="R" THEN
                    IF pr_reg.cod_rechazo_ent IS NOT NULL THEN
                         -- Valida que el codigo sea valido en catalogo de rechazos
                        SELECT A.cod_rechazo_ent
                        FROM   ret_rechazo_grl A
                        WHERE  A.cod_rechazo_ent = pr_reg.cod_rechazo_ent
                        AND    A.entidad         = 1

                        IF SQLCA.SQLCODE = NOTFOUND THEN
                            ERROR "NO EXISTE ESTE CODIGO DE RECHAZO"

                            CALL f_rechaza_sol(pr_reg.nss             ,
                                          pr_reg.consecutivo     ,
                                          pr_reg.tipo_prestacion ,
                                          pr_reg.tipo_retiro)
                                RETURNING codigo,ls_entidad

                            LET pr_reg.cod_rechazo_ent = codigo
                            DISPLAY pr_reg.cod_rechazo_ent TO cod_rechazo_ent
                            LET pos = arr_c
                        END IF
                    ELSE
                        ERROR "EL CODIGO DE RECHAZO NO PUEDE SER NULO"
                        ------------------------------------------------------
                        CALL f_rechaza_sol(pr_reg.nss             ,
                                           pr_reg.consecutivo     ,
                                           pr_reg.tipo_prestacion ,
                                           pr_reg.tipo_retiro)
                            RETURNING codigo,ls_entidad

                        LET pr_reg.cod_rechazo_ent = codigo
                        DISPLAY pr_reg.cod_rechazo_ent TO cod_rechazo_ent
                        LET pos = arr_c
                        -------------------------------------------------------
                    END IF
                END IF
            END IF

            CASE comando
                WHEN "C"
                    LET pr_reg.fecha_confirma    = HOY
                    LET pr_reg.usuario_confirma  = gc_usuario
                    LET pr_reg.estado_solicitud  = gr_edo.confirmado
                WHEN "R"
                    IF gs_rechazo <> 0 THEN
                        LET pr_reg.fecha_modifica    = HOY       
                        LET pr_reg.usuario_modifica  = gc_usuario
                        LET pr_reg.estado_solicitud  = gr_edo.rechazado
                    END IF
                OTHERWISE
                    LET pr_reg.fecha_modifica    = HOY
                    LET pr_reg.estado_solicitud  = gr_edo.capturado
            END CASE

            LET INT_FLAG = FALSE
            EXIT INPUT

        -------------------------------------
        ON KEY (CONTROL-B)
        -------------------------------------
            CALL f_ejecuta_benef(pr_reg.nss         ,
                                 pr_reg.consecutivo ,
                                 "M"                
                                )
        
        -------------------------------------
        ON KEY (INTERRUPT)
        -------------------------------------
            CALL f_error_msg("OPERACION CANCELADA")
            LET ls_salida = 1
            EXIT INPUT
    
    END INPUT


    IF ls_salida = 0 THEN
        
        CASE comando

            WHEN "C"
                SELECT estado_solicitud
                INTO   ls_edo_tmp
                FROM   ret_parcial
                WHERE  nss          = pr_reg.nss        
                AND    consecutivo  = pr_reg.consecutivo

                IF ls_edo_tmp = gr_edo.capturado THEN
                    PROMPT "¿DESEA REALIZAR LA CONFIRMACION DEL REGISTRO? (S/N) " FOR opc
                ELSE
                    LET opc = "N"
                    CALL f_error_msg("EL REGISTRO DEBE ESTAR EN ESTADO CAPTURADO")
                END IF
            
            WHEN "R"
                PROMPT "¿DESEA REALIZAR EL RECHAZO DEL REGISTRO? (S/N) " FOR opc
            
            OTHERWISE
                PROMPT "¿DESEA REALIZAR LA MODIFICACION DEL REGISTRO? (S/N) " FOR opc
        
        END CASE
        
        IF opc MATCHES '[Ss]' THEN
        
            -- Se actualiza el registro rechazado
            WHENEVER ERROR CONTINUE
            
            IF comando = "R" THEN
                IF gs_rechazo <> 0 THEN
                    UPDATE ret_parcial
                    SET    entidad          = ls_entidad              ,
                           cod_rechazo_ent  = pr_reg.cod_rechazo_ent  ,
                           estado_solicitud = pr_reg.estado_solicitud
                    WHERE  nss              = pr_reg.nss
                    AND    consecutivo      = pr_reg.consecutivo
                END IF
            ELSE
               UPDATE ret_parcial
               SET    folio_solicitud    = pr_reg.folio_solicitud   ,
                      fecha_solicitud    = pr_reg.fecha_solicitud   ,
                      num_resolucion     = pr_reg.num_resolucion    ,
                      tipo_retiro        = pr_reg.tipo_retiro       ,
                      tipo_prestacion    = pr_reg.tipo_prestacion   ,
                      tipo_desempleo     = pr_reg.tipo_desempleo    ,
                      tipo_pago          = pr_reg.tipo_pago         ,
                      fecha_resolucion   = pr_reg.fecha_resolucion  ,
                      fecha_fall_mat_des = pr_reg.fecha_fall_mat_des,
                      impt_autorizado    = pr_reg.impt_autorizado   ,
                      salario_base_cot   = pr_reg.salario_base_cot  ,
                      pago_desempleo     = pr_reg.pago_desempleo    ,
                      fecha_modifica     = pr_reg.fecha_modifica    ,
                      usuario_modifica   = pr_reg.usuario_modifica  ,
                      fecha_confirma     = pr_reg.fecha_confirma    ,
                      usuario_modifica   = pr_reg.usuario_modifica  ,
                      usuario_confirma   = pr_reg.usuario_confirma  ,
                      estado_solicitud   = pr_reg.estado_solicitud  ,
                      impt_ret_97        = pr_importes.ret_97       ,
                      impt_ces_vej       = pr_importes.cv           ,
                      impt_cuo_soc       = pr_importes.cs           ,
                      impt_tot_sub_rcv   = pr_importes.total_rcv
               WHERE  nss                = pr_reg.nss
               AND    consecutivo        = pr_reg.consecutivo
            END IF
        
            WHENEVER ERROR STOP
            
            IF SQLCA.SQLCODE < 0 THEN
                CALL f_error_msg("ERROR AL ACTUALIZAR, AVISE A SISTEMAS")
                
                LET gc_error = " Update ret_parcial ",
                               " nss ", pr_reg.nss,
                               " consecutivo ", pr_reg.consecutivo,
                               " usuario ", gc_usuario,
                               ERR_GET(SQLCA.SQLCODE)
                
                CALL ERRORLOG(gc_error CLIPPED)
                EXIT  PROGRAM
            ELSE
                IF comando = "M" THEN
                    IF (pr_reg.tipo_prestacion = gr_prest.desempleo AND pr_reg.tipo_desempleo <> "D") THEN
                        
                        WHENEVER ERROR CONTINUE
                        
                        UPDATE ret_ctr_pago
                        SET    mto_1          = lr_ctr_pago.mto_1         ,
                               mto_2          = lr_ctr_pago.mto_2         ,
                               mto_pago       = lr_ctr_pago.mto_pago      ,
                               mensualidades  = lr_ctr_pago.mensualidades
                        WHERE  nss            = pr_reg.nss
                        AND    consecutivo    = pr_reg.consecutivo
                        AND    estado         = gr_edo.capturado
                        
                        LET li_cont = SQLCA.SQLERRD[3]
                        
                        WHENEVER ERROR STOP
                        
                        IF (SQLCA.SQLCODE < 0) OR (li_cont <= 0) THEN
                            CALL f_error_msg("ERROR AL ACTUALIZAR, AVISE A SISTEMAS")
                            
                            LET gc_error = " Update ret_ctr_pago ",
                                           " nss ", pr_reg.nss,
                                           " consecutivo ", pr_reg.consecutivo,
                                           " usuario ", gc_usuario,
                                           ERR_GET(SQLCA.SQLCODE)
                            
                            CALL ERRORLOG(gc_error CLIPPED)
                            EXIT PROGRAM
                        END IF -- Error actualiza ret_ctr_pago
                    END IF -- Tipo prest = 6
                END IF -- Comando = modifica
            END IF -- Error actualiza ret_parcial
        
            -- Se desmarca la cuenta en caso de ser rechazo
            IF comando = "R" AND gs_rechazo <> 0 THEN
                CALL f_desmarca_cuenta(pr_reg.nss           ,
                                       pr_reg.consecutivo   ,
                                       pr_reg.tipo_prestacion 
                                      )
            END IF
        
            CASE comando
                WHEN "C"
                    CALL f_error_msg("REGISTRO CONFIRMADO")
                
                WHEN "R"
                    CALL f_error_msg("REGISTRO RECHAZADO")
                
                OTHERWISE
                    CALL f_error_msg("REGISTRO ACTUALIZADO")
             END CASE
        ELSE
            IF comando = "C" THEN
                CALL f_error_msg("EL RESISTRO NO FUE CONFIRMADO")
            ELSE
                CALL f_error_msg("EL RESISTRO NO FUE MODIFICADO")
            END IF
        END IF -- Opcion S

    END IF -- Salida = 0

END FUNCTION

#---------------------------------------------------------------------------#
# f_inserta_registros : Inserta los registros guardados en las tablas       #
#                       temporales, captura beneficiarios y marca la cuenta #
#                       del nss capturado                                   #
#---------------------------------------------------------------------------#
FUNCTION f_inserta_registros(pc_nss)

    DEFINE pc_nss LIKE ret_parcial.nss

    DEFINE ld_consecutivo  LIKE ret_parcial.consecutivo

    DEFINE lr_marca RECORD
        movimiento          SMALLINT    ,
        marca_cod           SMALLINT    ,
        desc_rechazo        CHAR(50)
    END RECORD

    DEFINE
        ls_tipo_pres        ,
        ls_err              SMALLINT

    DEFINE
        li_cont             INTEGER

    DEFINE
        lc_tipo_desempleo   CHAR(1)

    -- -----------------------------------------------------------------------------

    LET li_cont = 0
    LET ls_err  = 0

    -- Obtiene el consecutivo y actualiza las tablas temporales
    LET ld_consecutivo = f_obtiene_ult_consec()

    DISPLAY ld_consecutivo TO consecutivo

    UPDATE tmp_parcial
    SET    consecutivo  = ld_consecutivo
    WHERE  folio        = 0
            
    SELECT tipo_prestacion  ,
           tipo_desempleo
    INTO   ls_tipo_pres     ,
           lc_tipo_desempleo
    FROM   tmp_parcial
    WHERE  nss          = pc_nss
    AND    consecutivo  = ld_consecutivo

    CALL f_marca_cuenta(pc_nss, ld_consecutivo) RETURNING lr_marca.*

    -- Si no ocurrieron errores guardamos la solicitud
    IF lr_marca.marca_cod = 0 THEN

        -- Guardamos la solicitud
        SET LOCK MODE TO WAIT

            INSERT INTO ret_parcial
            SELECT *
            FROM   tmp_parcial
            WHERE  nss          = pc_nss
            AND    consecutivo  = ld_consecutivo
            
            LET li_cont = SQLCA.SQLERRD[3]
            
            LET gc_error = " Insert ret_parcial ",
                           " nss ", pc_nss,
                           " consecutivo ", ld_consecutivo,
                           " usuario ", gc_usuario,
                           ERR_GET(SQLCA.SQLCODE)

        SET LOCK MODE TO NOT WAIT

        -- Si no ocurrieron errores guardamos ret_ctr_pago en caso de ser desempleo
        IF li_cont > 0 THEN
            IF (ls_tipo_pres = gr_prest.desempleo AND lc_tipo_desempleo <> "D") THEN

                UPDATE tmp_ctr_pago
                SET    consecutivo  = ld_consecutivo
            
                SET LOCK MODE TO WAIT
                
                    INSERT INTO ret_ctr_pago
                    SELECT *
                    FROM   tmp_ctr_pago
                    WHERE  nss          = pc_nss
                    AND    consecutivo  = ld_consecutivo
                    
                    LET li_cont = SQLCA.SQLERRD[3]
                    
                    LET gc_error = " Insert ret_ctr_pago ",
                                   " nss ", pc_nss,
                                   " consecutivo ", ld_consecutivo,
                                   " usuario ", gc_usuario,
                                   ERR_GET(SQLCA.SQLCODE)

                SET LOCK MODE TO NOT WAIT
            ELSE
                LET li_cont = 1
            END IF

            -- Si no ocurrieron errores ejecutamos la captura de beneficiarios
            IF li_cont > 0 THEN
                CALL f_ejecuta_benef(pc_nss, ld_consecutivo, "A")
                LET ls_err = f_valida_benef(pc_nss, ld_consecutivo)
                
                -- Si no ocurrieron errores se muestra mensaje de terminacion al usuario
                IF ls_err = 0 THEN
                    DISPLAY "SALVANDO REGISTRO ..." AT 22,2 ATTRIBUTE (REVERSE)
                    SLEEP 1
                    CALL f_error_msg("¡SOLICITUD GUARDADA CORRECTAMENTE!")
                    ERROR "                                   " ATTRIBUTE (NORMAL)
                ELSE
                    LET gc_error = " Insert ret_beneficiario ",
                                   " nss ", pc_nss,
                                   " consecutivo ", ld_consecutivo,
                                   " usuario ", gc_usuario

                    CALL f_error_msg("OCURRIO UN ERROR AL CAPTURAR LOS BENEFICIARIOS")
                END IF
            ELSE
                LET ls_err  = 1
            END IF -- ret_ctr_pago
        ELSE
            LET ls_err  = 1
        END IF -- ret_parcial

        IF ls_err <> 0 THEN
            CALL ERRORLOG(gc_error CLIPPED)
            CALL f_elimina_operacion(pc_nss, ld_consecutivo, ls_tipo_pres)
            CALL f_error_msg("ERROR - NO PUDO CAPTURARSE LA SOLICITUD")
        END IF
    ELSE
        LET lr_marca.desc_rechazo = "ERROR NSS MARCADO, MOV-",
                                    lr_marca.marca_cod USING "<<<", " ",
                                    lr_marca.desc_rechazo CLIPPED
        CALL f_error_msg(lr_marca.desc_rechazo)

    END IF -- Marcaje de la cuenta

    CLOSE WINDOW retm0011

END FUNCTION


#---------------------------------------------------------------------------#
# f_elimina_operacion : Elimina lo que haya podido almacenarse de una       #
#                       solicitud cuando esta se cancele o se elimine       #
#---------------------------------------------------------------------------#
FUNCTION f_elimina_operacion(pc_nss, pi_consecutivo, ps_tipo_prest)

    DEFINE
        pc_nss          LIKE ret_parcial.nss,
        pi_consecutivo  LIKE ret_parcial.consecutivo

    DEFINE
        ps_tipo_prest       SMALLINT

    -- -----------------------------------------------------------------------------

    -- Se elimina la solicitud
    DELETE
    FROM  ret_parcial
    WHERE nss         = pc_nss
    AND   consecutivo = pi_consecutivo

    -- Se elimina el detalle de pagos
    DELETE
    FROM  ret_ctr_pago
    WHERE nss         = pc_nss
    AND   consecutivo = pi_consecutivo

    -- Se elimina el beneficiario
    DELETE
    FROM   ret_beneficiario
    WHERE  nss         = pc_nss
    AND    consecutivo = pi_consecutivo

    -- Se desmarca la cuenta
    CALL f_desmarca_cuenta(pc_nss           ,   
                           pi_consecutivo   ,
                           ps_tipo_prest
                          )
END FUNCTION


#---------------------------------------------------------------------------#
# f_tipo_pago_1 : Obtiene los valores para el tipo de pago 1 (Retiro A)     #
#---------------------------------------------------------------------------#
FUNCTION f_tipo_pago_1(pr_datos)

    DEFINE pr_datos RECORD
        nss         LIKE ret_parcial.nss                   ,
        consecutivo LIKE ret_parcial.consecutivo           ,
        sbc         LIKE ret_parcial.salario_base_cot
    END RECORD

    DEFINE lr_ctr_pago_01 RECORD LIKE ret_ctr_pago.*

    -- -----------------------------------------------------------------------------

    INITIALIZE lr_ctr_pago_01.* TO NULL

    -- Llenamos los datos para el tipo de pago 1
    LET lr_ctr_pago_01.nss           = pr_datos.nss
    LET lr_ctr_pago_01.consecutivo   = pr_datos.consecutivo
    LET lr_ctr_pago_01.mensualidades = 1
    LET lr_ctr_pago_01.estado        = gr_edo.capturado

    LET lr_ctr_pago_01.mto_1         = pr_datos.sbc * 30
    LET lr_ctr_pago_01.mto_2         = gd_limite_pago_a

    IF lr_ctr_pago_01.mto_1 < lr_ctr_pago_01.mto_2 THEN
        LET lr_ctr_pago_01.mto_pago = lr_ctr_pago_01.mto_1
    ELSE
        LET lr_ctr_pago_01.mto_pago = lr_ctr_pago_01.mto_2
    END IF

    RETURN lr_ctr_pago_01.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_despliega_tipo_1 : Despliega los valores para el tipo de pago 1         #
#---------------------------------------------------------------------------#
FUNCTION f_despliega_tipo_1(pr_tipo1)

    DEFINE pr_tipo1 RECORD
        mto_1           LIKE ret_ctr_pago.mto_1   ,
        mto_2           LIKE ret_ctr_pago.mto_2   ,
        mto_pago        LIKE ret_ctr_pago.mto_pago
    END RECORD

    DEFINE lar_tipo1 ARRAY[1] OF RECORD
        tipo_pago       LIKE ret_parcial.tipo_pago,
        desc_tipo_pago  CHAR(20)                  ,
        mto_1           LIKE ret_ctr_pago.mto_1   ,
        mto_2           LIKE ret_ctr_pago.mto_2   ,
        mto_pago        LIKE ret_ctr_pago.mto_pago
    END RECORD

    -- Datos para el screen array
    OPEN WINDOW pago_01 AT 2,3 WITH FORM "RETM8092" ATTRIBUTE(BORDER)
    DISPLAY " RETM809                     RETIROS PARCIALES            <CTRL-C> : Salir     " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY "               DETALLE DE PAGO - RETIRO POR DESEMPLEO TIPO A                   " AT 3,1 ATTRIBUTE(REVERSE)

    LET lar_tipo1[1].tipo_pago      = 1
    LET lar_tipo1[1].mto_1          = pr_tipo1.mto_1
    LET lar_tipo1[1].mto_2          = pr_tipo1.mto_2
    LET lar_tipo1[1].mto_pago       = pr_tipo1.mto_pago

    SELECT desc_corta
    INTO   lar_tipo1[1].desc_tipo_pago
    FROM   tab_tipo_pago
    WHERE  tipo_pago = lar_tipo1[1].tipo_pago

    CALL SET_COUNT(1)

    DISPLAY ARRAY lar_tipo1 TO scr_t1.*
        ON KEY (CONTROL-C)
            EXIT DISPLAY
    END DISPLAY

    CLOSE WINDOW pago_01

END FUNCTION

#---------------------------------------------------------------------------#
# f_tipo_pago_2 : Obtiene los valores para el tipo de pago 2 (Retiro B)     #
#---------------------------------------------------------------------------#
FUNCTION f_tipo_pago_2(pr_datos)

    DEFINE pr_datos RECORD
        nss         LIKE ret_parcial.nss                ,
        consecutivo LIKE ret_parcial.consecutivo        ,
        sbc         LIKE ret_parcial.salario_base_cot   ,
        total_rcv   LIKE ret_ctr_pago.mto_pago
    END RECORD

    DEFINE lr_ctr_pago_02 RECORD LIKE ret_ctr_pago.*
    DEFINE ld_primer_pago   LIKE ret_ctr_pago.mto_pago
    DEFINE ld_mensualidad   LIKE ret_ctr_pago.mto_pago

    INITIALIZE lr_ctr_pago_02.* TO NULL

    -- Llenamos los datos para el tipo de pago 1
    LET lr_ctr_pago_02.nss           = pr_datos.nss
    LET lr_ctr_pago_02.consecutivo   = pr_datos.consecutivo
    LET lr_ctr_pago_02.estado        = gr_edo.capturado
    LET lr_ctr_pago_02.mto_1         = pr_datos.sbc * 90
    LET lr_ctr_pago_02.mto_2         = pr_datos.total_rcv * 0.115

    IF lr_ctr_pago_02.mto_1 < lr_ctr_pago_02.mto_2 THEN
        LET lr_ctr_pago_02.mto_pago = lr_ctr_pago_02.mto_1
    ELSE
        LET lr_ctr_pago_02.mto_pago = lr_ctr_pago_02.mto_2
    END IF

    -- Comentario por Javier Gonzalez
    -- El pago por mensualidades esta deshabilitado por ahora para el retiro B
    -- Se pagara una sola exhibicion

    LET lr_ctr_pago_02.mensualidades = 1
{
    -- Determinamos el numero de mensualidades a pagar
    LET ld_primer_pago  = pr_datos.sbc * 30
    LET ld_mensualidad  = (lr_ctr_pago_02.mto_pago - ld_primer_pago) / 5

    IF ld_mensualidad <= gd_limite_pago_b THEN
        LET lr_ctr_pago_02.mensualidades = 2
    ELSE
        LET lr_ctr_pago_02.mensualidades = 6
    END IF
}

    RETURN lr_ctr_pago_02.*

END FUNCTION


#---------------------------------------------------------------------------#
# f_despliega_tipo_2 : Despliega los valores para el tipo de pago 2         #
#                      (Retiro B o Retiro D tipo 2)                         #
#---------------------------------------------------------------------------#
FUNCTION f_despliega_tipo_2(pr_tipo2)

    DEFINE pr_tipo2 RECORD
        tipo_desempleo      LIKE ret_parcial.tipo_desempleo ,
        retiro_ant          LIKE ret_parcial_resol.saldo_rcv_ant,
        mto_1a              LIKE ret_ctr_pago.mto_1         ,
        mto_2a              LIKE ret_ctr_pago.mto_2         ,
        mto_pago_a          LIKE ret_ctr_pago.mto_pago      ,
        mto_1b              LIKE ret_ctr_pago.mto_1         ,
        mto_2b              LIKE ret_ctr_pago.mto_2         ,
        mto_pago_b          LIKE ret_ctr_pago.mto_pago      ,
        mensualidades       LIKE ret_ctr_pago.mensualidades
    END RECORD

    DEFINE lr_tipo2 RECORD
        tipo_pago           LIKE ret_parcial.tipo_pago  ,
        desc_tipo_pago      CHAR(20)                    ,
        tipo_pago_a         LIKE ret_parcial.tipo_pago,
        desc_tipo_pago_a    CHAR(20)                  ,
        mto_1a              LIKE ret_ctr_pago.mto_1   ,
        mto_2a              LIKE ret_ctr_pago.mto_2   ,
        mto_pago_a          LIKE ret_ctr_pago.mto_pago,
        tipo_pago_b         LIKE ret_parcial.tipo_pago,
        desc_tipo_pago_b    CHAR(20)                  ,
        mto_1b              LIKE ret_ctr_pago.mto_1   ,
        mto_2b              LIKE ret_ctr_pago.mto_2   ,
        mto_pago_b          LIKE ret_ctr_pago.mto_pago
{
        mensualidades       LIKE ret_ctr_pago.mensualidades,
        primer_pago         LIKE ret_ctr_pago.mto_pago,
        pagos_sig           LIKE ret_ctr_pago.mto_pago
}
    END RECORD

    DEFINE ld_pago_desp LIKE ret_ctr_pago.mto_pago

    -- Datos para la pantalla

-- Comentario por Javier Gonzalez
-- La forma RETM809tmp.per es temporal mientras se rehabilita el pago por mensualidades
-- Una vez habilitada esa opcion, se debe usar la forma original (RETM8093.per) y quitarse
-- las partes comentadas en esta funcion

    OPEN WINDOW pago_02 AT 2,3 WITH FORM "RETM809tmp" ATTRIBUTE(BORDER)

--    OPEN WINDOW pago_02 AT 2,3 WITH FORM "RETM8093" ATTRIBUTE(BORDER)

    DISPLAY " RETM809                     RETIROS PARCIALES               <ESC> : Salir   " AT 1,1 ATTRIBUTE(REVERSE)

    CASE pr_tipo2.tipo_desempleo
        WHEN "B"
            DISPLAY "           ELECCION DE FORMA DE PAGO - RETIRO POR DESEMPLEO TIPO B           " AT 3,1 ATTRIBUTE(REVERSE)
        WHEN "C"
            DISPLAY "             DETALLE DE CALCULO DE PAGO - RETIRO COMPLEMENTARIO              " AT 3,1 ATTRIBUTE(REVERSE)
            DISPLAY "SE PAGARA EL MONTO MAYOR DE LOS TIPOS MENOS $", pr_tipo2.retiro_ant,
                    " (RETIRO ANTERIOR)"  AT 20,2 ATTRIBUTE(REVERSE)
        WHEN "D"
            DISPLAY "          DETALLE DE CALCULO DE PAGO - RETIRO POR DESEMPLEO ANTERIOR         " AT 3,1 ATTRIBUTE(REVERSE)
            DISPLAY "EL MONTO A PAGAR ES EL MAYOR ENTRE LOS TIPOS MOSTRADOS" AT 21,2 ATTRIBUTE(REVERSE)
    END CASE



    INPUT BY NAME lr_tipo2.*

        BEFORE INPUT
            LET lr_tipo2.tipo_pago_a      = 1
            LET lr_tipo2.mto_1a           = pr_tipo2.mto_1a
            LET lr_tipo2.mto_2a           = pr_tipo2.mto_2a
            LET lr_tipo2.mto_pago_a       = pr_tipo2.mto_pago_a

            SELECT desc_corta
            INTO   lr_tipo2.desc_tipo_pago_a
            FROM   tab_tipo_pago
            WHERE  tipo_pago = lr_tipo2.tipo_pago_a

            LET lr_tipo2.tipo_pago_b      = 2
            LET lr_tipo2.mto_1b           = pr_tipo2.mto_1b
            LET lr_tipo2.mto_2b           = pr_tipo2.mto_2b
            LET lr_tipo2.mto_pago_b       = pr_tipo2.mto_pago_b

            SELECT desc_corta
            INTO   lr_tipo2.desc_tipo_pago_b
            FROM   tab_tipo_pago
            WHERE  tipo_pago = lr_tipo2.tipo_pago_b


--            LET lr_tipo2.mensualidades    = pr_tipo2.mensualidades

{
            -- La primera mensualidad siempre es 30 veces el sal base de cotizacion
            -- que corresponde al monto 1 del tipo 1
            LET lr_tipo2.primer_pago      = pr_tipo2.mto_1a


            IF pr_tipo2.mensualidades = 2 THEN
                LET lr_tipo2.pagos_sig = pr_tipo2.mto_pago_b - pr_tipo2.mto_1a
            ELSE
                LET lr_tipo2.pagos_sig = (pr_tipo2.mto_pago_b - pr_tipo2.mto_1a)/5
            END IF
}
            DISPLAY BY NAME lr_tipo2.tipo_pago_a      ,
                            lr_tipo2.desc_tipo_pago_a ,
                            lr_tipo2.mto_1a           ,
                            lr_tipo2.mto_2a           ,
                            lr_tipo2.mto_pago_a       ,
                            lr_tipo2.tipo_pago_b      ,
                            lr_tipo2.desc_tipo_pago_b ,
                            lr_tipo2.mto_1b           ,
                            lr_tipo2.mto_2b           ,
                            lr_tipo2.mto_pago_b
{
                            lr_tipo2.mensualidades    ,
                            lr_tipo2.primer_pago      ,
                            lr_tipo2.pagos_sig
}
                IF lr_tipo2.mto_pago_b > lr_tipo2.mto_pago_a THEN
                    LET lr_tipo2.tipo_pago      = 2
                    IF pr_tipo2.tipo_desempleo = "C" THEN

                        LET ld_pago_desp = lr_tipo2.mto_pago_b - pr_tipo2.retiro_ant

                        IF ld_pago_desp < 0 THEN
                            LET ld_pago_desp = 0
                        END IF

                        DISPLAY "SE PAGARAN $",ld_pago_desp,
                                " DE RETIRO COMPLEMENTARIO                  " AT 21,2 ATTRIBUTE(REVERSE)
                    END IF
                ELSE
                    LET lr_tipo2.tipo_pago      = 1
                    IF pr_tipo2.tipo_desempleo = "C" THEN
                        LET ld_pago_desp = lr_tipo2.mto_pago_a - pr_tipo2.retiro_ant

                        IF ld_pago_desp < 0 THEN
                            LET ld_pago_desp = 0
                        END IF

                        DISPLAY "SE PAGARAN $",ld_pago_desp,
                                " DE RETIRO COMPLEMENTARIO                  " AT 21,2 ATTRIBUTE(REVERSE)
                    END IF
                END IF

                SELECT desc_corta
                INTO   lr_tipo2.desc_tipo_pago
                FROM   tab_tipo_pago
                WHERE  tipo_pago = lr_tipo2.tipo_pago

                DISPLAY BY NAME lr_tipo2.desc_tipo_pago

        -------------------------------------
        AFTER FIELD tipo_pago
        -------------------------------------
            IF lr_tipo2.tipo_pago IS NULL OR lr_tipo2.tipo_pago = 0 THEN
                ERROR "DEBE CAPTURAR LA MODALIDAD DE PAGO"
                SLEEP 2
                NEXT FIELD tipo_pago
            END IF

            SELECT desc_corta
            INTO   lr_tipo2.desc_tipo_pago
            FROM   tab_tipo_pago
            WHERE  tipo_pago = lr_tipo2.tipo_pago

            DISPLAY BY NAME lr_tipo2.desc_tipo_pago

            IF pr_tipo2.tipo_desempleo = "C" THEN
                IF lr_tipo2.tipo_pago = 1 THEN
                    LET ld_pago_desp = lr_tipo2.mto_pago_a - pr_tipo2.retiro_ant

                    IF ld_pago_desp < 0 THEN
                        LET ld_pago_desp = 0
                    END IF

                    DISPLAY "SE PAGARAN $",ld_pago_desp,
                            " DE RETIRO COMPLEMENTARIO                  " AT 21,2 ATTRIBUTE(REVERSE)
                ELSE
                    LET ld_pago_desp = lr_tipo2.mto_pago_b - pr_tipo2.retiro_ant

                    IF ld_pago_desp < 0 THEN
                        LET ld_pago_desp = 0
                    END IF

                    DISPLAY "SE PAGARAN $",ld_pago_desp,
                            " DE RETIRO COMPLEMENTARIO                  " AT 21,2 ATTRIBUTE(REVERSE)
                END IF
            END IF

            DISPLAY BY NAME lr_tipo2.desc_tipo_pago

        -------------------------------------
        ON KEY (ESC)
        -------------------------------------
            IF lr_tipo2.tipo_pago IS NULL OR lr_tipo2.tipo_pago = 0 THEN
                ERROR "DEBE CAPTURAR LA MODALIDAD DE PAGO"
                SLEEP 2
                NEXT FIELD tipo_pago
            ELSE
                SELECT desc_corta
                INTO   lr_tipo2.desc_tipo_pago
                FROM   tab_tipo_pago
                WHERE  tipo_pago = lr_tipo2.tipo_pago

                EXIT INPUT
            END IF
    END INPUT

    CLOSE WINDOW pago_02

    RETURN lr_tipo2.tipo_pago       ,
           lr_tipo2.desc_tipo_pago

END FUNCTION

#---------------------------------------------------------------------------#
# f_det_pago_ab : Determina el monto a pagar para las resoluciones          #
#                 transitorias anteriores.                                  #
#---------------------------------------------------------------------------#
FUNCTION f_det_pago_ab(pr_datos)

    DEFINE pr_datos RECORD
        nss         LIKE ret_parcial.nss                    ,
        consecutivo LIKE ret_parcial.consecutivo            ,
        num_resol   LIKE ret_parcial.num_resolucion         ,
        tipo_des    LIKE ret_parcial.tipo_desempleo         ,
        tipo_pago   LIKE ret_parcial.tipo_pago              ,
        monto_rcv   LIKE ret_ctr_pago.mto_pago              ,
        retiro_ant  LIKE ret_parcial_resol.saldo_rcv_ant    ,
        sbc_a       LIKE ret_parcial_resol.salario_base_a   ,
        sbc_b       LIKE ret_parcial_resol.salario_base_b
    END RECORD

    DEFINE lr_datos RECORD
        pago_des    LIKE ret_parcial.pago_desempleo     ,
        salario     LIKE ret_parcial.salario_base_cot   ,
        tipo_des    LIKE ret_parcial.tipo_desempleo     ,
        desc_tipo   CHAR(30)
    END RECORD

    DEFINE
        lr_pago_A       ,
        lr_pago_B       ,
        lr_ctr_pago     RECORD LIKE ret_ctr_pago.*

    DEFINE
        ls_tipo_pago    SMALLINT

    DEFINE
        ld_75dias       DECIMAL(16,2),
        ld_10porc       DECIMAL(16,2)

    CALL f_tipo_pago_1(pr_datos.nss         ,
                       pr_datos.consecutivo ,
                       pr_datos.sbc_a       )
    RETURNING lr_pago_A.*

    CALL f_tipo_pago_2(pr_datos.nss         ,
                       pr_datos.consecutivo ,
                       pr_datos.sbc_b       ,
                       pr_datos.monto_rcv
                       )
    RETURNING lr_pago_B.*

    CALL f_despliega_tipo_2(pr_datos.tipo_des      ,
                            pr_datos.retiro_ant    ,
                            lr_pago_A.mto_1        ,
                            lr_pago_A.mto_2        ,
                            lr_pago_A.mto_pago     ,
                            lr_pago_B.mto_1        ,
                            lr_pago_B.mto_2        ,
                            lr_pago_B.mto_pago     ,
                            lr_pago_B.mensualidades
                            )
        RETURNING ls_tipo_pago      ,
                  lr_datos.desc_tipo

    IF ls_tipo_pago = 2 THEN
        LET lr_ctr_pago.*     = lr_pago_B.*
        LET lr_datos.pago_des = lr_pago_B.mto_pago
        LET lr_datos.salario  = pr_datos.sbc_b
        LET lr_datos.tipo_des = 2
    ELSE
        LET lr_ctr_pago.*     = lr_pago_A.*
        LET lr_datos.pago_des = lr_pago_A.mto_pago
        LET lr_datos.salario  = pr_datos.sbc_a
        LET lr_datos.tipo_des = 1
    END IF

    RETURN lr_datos.*, lr_ctr_pago.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_captura_salarios : Realiza la captura de los salarios base de           #
#                      cotizacion para los retiros complementarios          #
#---------------------------------------------------------------------------#
FUNCTION f_captura_salarios()

    DEFINE lr_sbc RECORD
        salario_base_a LIKE ret_parcial_resol.salario_base_a,
        salario_base_b LIKE ret_parcial_resol.salario_base_b
    END RECORD

    OPEN WINDOW win_sal AT 2,3 WITH FORM "RETM8095" ATTRIBUTE(BORDER)
    DISPLAY "                                          <ESC> : Salir  " AT 1,1 ATTRIBUTE(REVERSE)

    INPUT BY NAME lr_sbc.*

        -------------------------------------
        ON KEY (ESC)
        -------------------------------------
            IF lr_sbc.salario_base_a IS NULL OR lr_sbc.salario_base_a = 0 THEN
                ERROR "DEBE CAPTURAR SALARIO ULTIMA COTIZACION "
                SLEEP 2
                NEXT FIELD salario_base_a
            END IF

            IF lr_sbc.salario_base_b IS NULL OR lr_sbc.salario_base_b = 0 THEN
                ERROR "DEBE CAPTURAR PROMEDIO ULT 250 SEMANAS"
                SLEEP 2
                NEXT FIELD salario_base_b
            END IF

            EXIT INPUT

    END INPUT

    CLOSE WINDOW win_sal

    RETURN lr_sbc.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_marca_cuenta : Ejecuta el script para realizar la marca de la cuenta    #
#---------------------------------------------------------------------------#
FUNCTION f_marca_cuenta(pr_marca)

    DEFINE pr_marca RECORD
        nss         LIKE pen_solicitud_pmg.nss          ,
        consec      LIKE pen_solicitud_pmg.consecutivo
    END RECORD

    DEFINE lr_dat RECORD
        edo_marca       SMALLINT ,
        cod_rechazo     SMALLINT ,
        marca_causa     SMALLINT ,
        fec_causa       DATE
    END RECORD

    DEFINE
        ls_tipo_pres        ,
        ls_marca_res        ,
        ls_cod_rech         ,
        ls_movim            SMALLINT

    DEFINE
        lc_desc_rechazo     CHAR(50)

    -- ---------------------------------------------------------------------------------

    LET lc_desc_rechazo     = " "
    LET lr_dat.edo_marca    = 0
    LET lr_dat.cod_rechazo  = 0
    LET lr_dat.marca_causa  = 0
    INITIALIZE lr_dat.fec_causa TO NULL

    SELECT tipo_prestacion
    INTO   ls_tipo_pres
    FROM   tmp_parcial
    WHERE  nss         = pr_marca.nss

    IF ls_tipo_pres = gr_prest.desempleo THEN
        LET ls_movim = gr_movs.tipo_d
    ELSE
        LET ls_movim = gr_movs.matrimonio
    END IF


    --MARCAJE--
    EXECUTE eje_marca USING pr_marca.nss            ,--nss
                            ls_movim                ,--marca entrante
                            pr_marca.consec         ,--consecutivo
                            lr_dat.edo_marca        ,--estado_marco
                            lr_dat.cod_rechazo      ,--codigo de rechazo
                            lr_dat.marca_causa      ,--marca_causa
                            lr_dat.fec_causa        ,--fecha_causa
                            gc_usuario               --usuario
                      INTO  ls_marca_res   ,
                            ls_cod_rech

    -- Si existe un error en la marca, regresamos el codigo y el mensaje de error
    IF ls_cod_rech <> 0 THEN
        SELECT rechazo_desc
        INTO   lc_desc_rechazo
        FROM   tab_rch_marca
        WHERE  rechazo_cod = ls_cod_rech
    END IF

    RETURN ls_movim, ls_cod_rech, lc_desc_rechazo

END FUNCTION

#---------------------------------------------------------------------------#
# f_desmarca_cuenta : Ejecuta el script para realizar la desmarca de la     #
#                     cuenta de la solicitud de parciales                   #
#---------------------------------------------------------------------------#
FUNCTION f_desmarca_cuenta(pr_desmarca)

    DEFINE pr_desmarca RECORD
        nss         LIKE ret_parcial.nss                ,
        consec      LIKE ret_parcial.consecutivo        ,
        tipo_prest  LIKE ret_parcial.tipo_prestacion
    END RECORD

    DEFINE lr_dat RECORD
        edo_marca       SMALLINT ,
        marca_causa     SMALLINT
    END RECORD

    DEFINE
        ls_movim            SMALLINT

    -- ---------------------------------------------------------------------------------

    LET lr_dat.edo_marca    = 0
    LET lr_dat.marca_causa  = 0

    IF pr_desmarca.tipo_prest = gr_prest.desempleo THEN
        LET ls_movim    = gr_movs.tipo_d
    ELSE
        LET ls_movim    = gr_movs.matrimonio
    END IF

    --DESMARCA DE LA CUENTA --
    EXECUTE eje_desmarca USING pr_desmarca.nss      ,--nss
                               ls_movim             ,--marca entrante
                               pr_desmarca.consec   ,--consecutivo
                               lr_dat.edo_marca     ,--estado_marco
                               lr_dat.marca_causa   ,--marca_causa
                               gc_usuario            --usuario

END FUNCTION

#---------------------------------------------------------------------------#
# f_retiros_complementarios : Determina el monto a pagar para las           #
#                             resoluciones complementarias                  #
#---------------------------------------------------------------------------#
FUNCTION f_retiros_complementarios(pr_datos, p_mto_pagado)

    DEFINE pr_datos RECORD
        nss         LIKE ret_parcial.nss                    ,
        consecutivo LIKE ret_parcial.consecutivo            ,
        num_resol   LIKE ret_parcial.num_resolucion         ,
        tipo_des    LIKE ret_parcial.tipo_desempleo         ,
        tipo_pago   LIKE ret_parcial.tipo_pago              ,
        monto_rcv   LIKE ret_ctr_pago.mto_pago              ,
        retiro_ant  LIKE ret_parcial_resol.saldo_rcv_ant    ,
        sbc_a       LIKE ret_parcial_resol.salario_base_a   ,
        sbc_b       LIKE ret_parcial_resol.salario_base_b
    END RECORD

    DEFINE p_mto_pagado LIKE ret_parcial_resol.monto_pagado


    DEFINE lr_datos RECORD
        pago_des    LIKE ret_parcial.pago_desempleo     ,
        salario     LIKE ret_parcial.salario_base_cot   ,
        tipo_des    LIKE ret_parcial.tipo_desempleo     ,
        desc_tipo   CHAR(30)
    END RECORD

    DEFINE
        lr_ctr_pago     RECORD LIKE ret_ctr_pago.*

    DEFINE
        ls_tipo_pago    SMALLINT

    -- -----------------------------------------------------------------------------

    CALL f_det_pago_ab(pr_datos.*)
        RETURNING lr_datos.*    ,
                  lr_ctr_pago.*

    LET lr_datos.pago_des = lr_datos.pago_des - p_mto_pagado

    #Actualizamos los valores para la ret_ctr_pago
    IF lr_ctr_pago.mto_1 = lr_ctr_pago.mto_pago THEN
        LET lr_ctr_pago.mto_1 = lr_datos.pago_des
    ELSE
        LET lr_ctr_pago.mto_2 = lr_datos.pago_des
    END IF

    LET lr_ctr_pago.mto_pago = lr_datos.pago_des

    RETURN lr_datos.*, lr_ctr_pago.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_retiros_anteriores : Determina el monto a pagar para las resoluciones   #
#                        transitorias anteriores.                           #
#---------------------------------------------------------------------------#
FUNCTION f_retiros_anteriores(pr_datos)

    DEFINE pr_datos RECORD
        nss         LIKE ret_parcial.nss                    ,
        consecutivo LIKE ret_parcial.consecutivo            ,
        num_resol   LIKE ret_parcial.num_resolucion         ,
        tipo_des    LIKE ret_parcial.tipo_desempleo         ,
        tipo_pago   LIKE ret_parcial.tipo_pago              ,
        monto_rcv   LIKE ret_ctr_pago.mto_pago              ,
        retiro_ant  LIKE ret_parcial_resol.saldo_rcv_ant    ,
        sbc_a       LIKE ret_parcial_resol.salario_base_a   ,
        sbc_b       LIKE ret_parcial_resol.salario_base_b
    END RECORD

    DEFINE lr_datos RECORD
        pago_des    LIKE ret_parcial.pago_desempleo     ,
        salario     LIKE ret_parcial.salario_base_cot   ,
        tipo_des    LIKE ret_parcial.tipo_desempleo     ,
        desc_tipo   CHAR(30)
    END RECORD

    DEFINE
        lr_pago_A       ,
        lr_pago_B       RECORD LIKE ret_ctr_pago.*

    DEFINE
        ls_tipo_pago    SMALLINT

    DEFINE
        ld_75dias       DECIMAL(16,2),
        ld_10porc       DECIMAL(16,2)

    DEFINE
        lr_ctr_pago     RECORD LIKE ret_ctr_pago.*

    -- -----------------------------------------------------------------------------

    -- Si el ind tipo pago = 3 se paga el menor entre 75 dias de SBC y 10% rcv
    IF pr_datos.tipo_pago = 3 THEN
        LET ld_75dias  = gs_dias_sbc * pr_datos.sbc_b
        LET ld_10porc  = (pr_datos.monto_rcv * gd_porc_pago)/100

        IF ld_75dias < ld_10porc THEN
            LET lr_datos.pago_des = ld_75dias
        ELSE
            LET lr_datos.pago_des = ld_10porc
        END IF

        LET lr_datos.tipo_des  = 3
        LET lr_datos.desc_tipo = "PAGO ANTERIOR"
        LET lr_datos.salario   = pr_datos.sbc_b
    ELSE
        -- Si el ind tipo pago = 2 se paga el mayor entre ret a y b
        CALL f_det_pago_ab(pr_datos.*)
            RETURNING lr_datos.*    ,
                      lr_ctr_pago.*
    END IF

    RETURN lr_datos.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_obten_monto_mat : Obtiene el monto a pagar para el retiro parcial por   #
#                     matrimonio                                            #
#---------------------------------------------------------------------------#
FUNCTION f_obten_monto_mat(p_nss)

    DEFINE p_nss        LIKE ret_parcial.nss

    DEFINE ls_sie       LIKE dis_provision.siefore

    DEFINE
        ld_monto     ,
        ld_matrim    DECIMAL(16,2)

    DEFINE ld_acciones DECIMAL(22,6)

    DEFINE li_folio_tmp    INTEGER
    DEFINE ls_id_aporte_cs SMALLINT

    LET ld_monto    = 0
    LET ld_matrim   = 0
    LET ld_acciones = 0
    LET ls_sie      = 0

    INITIALIZE ls_id_aporte_cs TO NULL
    INITIALIZE li_folio_tmp    TO NULL

    -- -----------------------------------------------------------------------------

    -- Obtenemos el id de aporte de cs
    SELECT MAX(folio)
    INTO   li_folio_tmp
    FROM   ret_parcial_resol A
    WHERE  A.nss             = p_nss
    AND    A.tipo_prestacion = gr_prest.matrimonio --7

    SELECT A.id_aportacion_cs
    INTO   ls_id_aporte_cs
    FROM   ret_parcial_resol A
    WHERE  A.nss             = p_nss
    AND    A.tipo_prestacion = gr_prest.matrimonio --7
    AND    A.folio           = li_folio_tmp

    -- Si el id aporte cs es 1 se recupera solo cuota social como se hacia anteriormente
    IF ls_id_aporte_cs = 1 OR ls_id_aporte_cs IS NULL THEN
        
        DECLARE cur_mat_cs CURSOR FOR
    	SELECT siefore                 ,
    	       SUM(monto_en_acciones)
    	FROM   dis_cuenta
    	WHERE  nss              = p_nss
        AND    subcuenta        = 5
        AND    fecha_conversion <= HOY
        GROUP BY 1
        ORDER BY 1

        FOREACH cur_mat_cs INTO ls_sie,
       	                        ld_acciones

            IF ld_acciones IS NULL THEN
       	  	    LET ld_acciones = 0
       	    END IF

       	    LET ld_matrim = ld_matrim + ld_acciones * gar_precio_acc[ls_sie].precio_dia
        END FOREACH
    ELSE
        -- Si el id aporte cs es 0 se recupera CV
        DECLARE cur_mat_cv CURSOR FOR
    	SELECT siefore,
    	       SUM(monto_en_acciones)
    	FROM   dis_cuenta
    	WHERE  nss              = p_nss
        AND    subcuenta        IN (1,2,6,9)
        AND    fecha_conversion <= HOY
        GROUP BY 1
        ORDER BY 1

        FOREACH cur_mat_cv INTO ls_sie,
       	                        ld_acciones

            IF ld_acciones IS NULL THEN
                LET ld_acciones = 0
            END IF
            
            LET ld_matrim = ld_matrim + ld_acciones * gar_precio_acc[ls_sie].precio_dia
        END FOREACH
    END IF

    RETURN ld_matrim

END FUNCTION

#---------------------------------------------------------------------------#
# f_rechaza_sol : Ejecuta las instrucciones para realizar el rechazo de la  #
#                 captura de parciales                                      #
#---------------------------------------------------------------------------#
FUNCTION f_rechaza_sol(p_nss, p_consecutivo, p_tipo_prestacion,p_tipo_retiro)

    DEFINE
        p_nss             LIKE  ret_parcial.nss,
        p_consecutivo     LIKE  ret_parcial.consecutivo,
        p_tipo_prestacion LIKE  ret_parcial.tipo_prestacion,
        p_tipo_retiro     CHAR(1)

    DEFINE lr_reg RECORD
       nss              LIKE ret_parcial.nss,
       consecutivo      LIKE ret_parcial.consecutivo,
       cod_rechazo_ent  LIKE ret_parcial.cod_rechazo_ent,
       cod_rec_ent_des  CHAR (60),
       tipo_prestacion  LIKE ret_parcial.tipo_prestacion,
       tipo_retiro  CHAR(1)
    END RECORD

    DEFINE
        ventidad           SMALLINT

    -- -----------------------------------------------------------------------------

    SELECT e.entidad 
    INTO   ventidad
    FROM   tab_entidad e
    WHERE  e.descripcion = "AFORE"
    
    LET ventidad = 1

    LET lr_reg.nss               = p_nss
    LET lr_reg.consecutivo       = p_consecutivo
    LET lr_reg.tipo_retiro       = p_tipo_retiro
    LET lr_reg.tipo_prestacion   = p_tipo_prestacion

    INPUT BY NAME lr_reg.cod_rechazo_ent

        -------------------------------------
        AFTER FIELD cod_rechazo_ent
        -------------------------------------
            IF FGL_LASTKEY()= FGL_KEYVAL("LEFT") THEN
                LET lr_reg.cod_rechazo_ent = null
                NEXT FIELD PREVIOUS
            END IF

            IF lr_reg.cod_rechazo_ent IS NOT NULL THEN
                -- Valida que el codigo sea valido en catalogo de rechazos
                SELECT A.cod_rechazo_ent
                FROM   ret_rechazo_grl A
                WHERE  A.cod_rechazo_ent = lr_reg.cod_rechazo_ent
                AND    A.entidad         = ventidad
                AND    A.tipo_retiro     IN ("P","G")

                IF SQLCA.SQLCODE = NOTFOUND THEN
                    ERROR "NO EXISTE ESTE CODIGO DE RECHAZO"
                    LET lr_reg.cod_rechazo_ent = NULL
                    NEXT FIELD cod_rechazo_ent
                ELSE
                    DISPLAY BY NAME lr_reg.cod_rechazo_ent
--                    DISPLAY lr_reg.cod_rec_ent_des TO cod_rec_ent_des
                    EXIT INPUT
                END IF
            ELSE
                -- Se despliega ventana de ayuda de codigo de rechazo
                CALL f_despliega_cod_rechazo_ent(lr_reg.tipo_retiro)
                RETURNING lr_reg.cod_rechazo_ent, lr_reg.cod_rec_ent_des

                IF lr_reg.cod_rechazo_ent <= 0 THEN
                   ERROR "NO EXISTE ESTE CODIGO DE RECHAZO "
                   LET lr_reg.cod_rechazo_ent = NULL
                   NEXT FIELD cod_rechazo_ent
                END IF

                DISPLAY BY NAME lr_reg.cod_rechazo_ent
--                DISPLAY lr_reg.cod_rec_ent_des TO cod_rec_ent_des
                EXIT INPUT
            END IF
    END INPUT

    RETURN lr_reg.cod_rechazo_ent, ventidad

END FUNCTION

#---------------------------------------------------------------------------#
# f_despliega_tipo_desempleo : Despliega la pantalla de ayuda que muestra   #
#                              los tipos de desempleo existentes            #
#---------------------------------------------------------------------------#
FUNCTION f_despliega_tipo_desempleo()

    DEFINE lr_reg RECORD
        codigo            CHAR(01),
        descripcion       CHAR(15)
    END RECORD

    DEFINE lar_display ARRAY[100] OF RECORD
        codigo            CHAR(01),
        descripcion       CHAR(15)
    END RECORD

    DEFINE
        lc_busqueda         CHAR(300),
        lc_desc             CHAR(030)

    DEFINE
        li_pos              INTEGER

    -- -----------------------------------------------------------------------------

    OPEN WINDOW cat_tpago AT 05,12 WITH FORM "RETM8094" ATTRIBUTE(BORDER)
    DISPLAY "TIPO RETIRO POR DESEMPLEO" AT 2,16  ATTRIBUTE(REVERSE)

    INPUT BY NAME lc_desc

        #--
        BEFORE FIELD lc_desc
            LET lc_desc = "*"

        #--
        AFTER FIELD lc_desc
            IF lc_desc IS NULL THEN
                ERROR " DESCRIPCION A BUSCAR NO PUEDE SER NULA " ATTRIBUTE(NORMAL)
                NEXT FIELD lc_desc
            ELSE
                EXIT INPUT
            END IF

    END INPUT

    WHILE TRUE
        LET lc_busqueda = " SELECT tipo_desempleo, desc_corta FROM tab_tipo_desempleo ",
                          " WHERE desc_corta MATCHES ", '"',
                          lc_desc CLIPPED, '"',
                          " ORDER BY 1 " CLIPPED

        LET li_pos = 1

        PREPARE pre_5 FROM lc_busqueda
        DECLARE cur_5 CURSOR FOR pre_5

        FOREACH cur_5 INTO lar_display[li_pos].*

            LET li_pos = li_pos + 1
            IF li_pos >= 1000 THEN
                ERROR " FUE SOBREPASADA LA CAPACIDAD MAXIMA DEL ARREGLO" ATTRIBUTE(NORMAL)
                EXIT FOREACH
            END IF

        END FOREACH

        IF (li_pos - 1) < 1 THEN
            ERROR " ARCHIVO TIPO DE DESEMPLEO VACIO" ATTRIBUTE(NORMAL)
        END IF

        CALL SET_COUNT(li_pos-1)

        DISPLAY ARRAY lar_display TO scr_2.*
            ON KEY (CONTROL-C,INTERRUPT)
                LET li_pos = 0
                EXIT DISPLAY

            ON KEY (CONTROL-M)
                LET li_pos              = ARR_CURR()
                LET lr_reg.codigo       = lar_display[li_pos].codigo
                LET lr_reg.descripcion  = lar_display[li_pos].descripcion
                EXIT DISPLAY

        END DISPLAY

        IF li_pos <> 0 THEN
            EXIT WHILE
        END IF

    END WHILE

    CLOSE WINDOW cat_tpago

    RETURN lr_reg.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_despliega_cod_rechazo_ent : Despliega la pantalla de ayuda que muestra  #
#                              los tipos de rechazo existentes              #
#---------------------------------------------------------------------------#
FUNCTION f_despliega_cod_rechazo_ent (tipo_retiro)

    DEFINE l_reg ARRAY[100] OF RECORD
       cod_rechazo_ent       SMALLINT,
       des_corta             CHAR(60)
    END RECORD

    DEFINE
        x_x               CHAR(200) ,
        x_buscar          CHAR(030) ,
        descripcion       CHAR(60)  ,
        tipo_retiro       CHAR(1)   ,
        tr                CHAR(1)

    DEFINE
        i                 INTEGER

    DEFINE
        pos               ,
        codigo            SMALLINT


    LET tr = tipo_retiro

    OPEN WINDOW retm8092 AT 05,10 WITH FORM "RETM0064" ATTRIBUTE(BORDER)
    DISPLAY "                      TIPOS DE RECHAZOS                  " AT 2,1 ATTRIBUTE(REVERSE)

    INPUT BY NAME x_buscar

        BEFORE FIELD x_buscar
            LET x_buscar = "*"

        AFTER FIELD x_buscar
            IF x_buscar IS NULL THEN
                ERROR "DESCRIPCION A BUSCAR NO PUEDE SER NULA"
                NEXT FIELD x_buscar
            ELSE
                EXIT INPUT
            END IF

     END INPUT

     WHILE TRUE
        LET x_x = " SELECT cod_rechazo_ent, des_larga FROM ret_rechazo_grl",
                  " WHERE des_corta MATCHES ",'"',x_buscar CLIPPED,'"',
                  --" AND tipo_retiro = '",tr CLIPPED ,"'",
                  " AND (tipo_retiro = 'P' ",
                  " OR  tipo_retiro = 'G') ",
                  " AND entidad = 1 ",
                  " ORDER BY 1 " CLIPPED

        PREPARE pre_rec FROM x_x
        DECLARE cur_rec CURSOR FOR pre_rec
        LET pos = 1

        FOREACH cur_rec INTO l_reg[pos].*
            LET pos = pos + 1
            IF pos >= 1000 THEN
                ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
                EXIT FOREACH
            END IF
        END FOREACH

        IF (pos-1) < 1 THEN
            ERROR "ARCHIVO DE RECHAZOS VACIO   "
        END IF

        CALL SET_COUNT(pos-1)
        DISPLAY ARRAY l_reg TO scr_1.*
            ON KEY ( CONTROL-C )
                LET pos = 0
                EXIT DISPLAY

            ON KEY ( INTERRUPT )
                LET pos = 0
                EXIT DISPLAY

            ON KEY ( CONTROL-M )
                LET pos         = ARR_CURR()
                LET codigo      = l_reg[pos].cod_rechazo_ent
                LET descripcion = l_reg[pos].des_corta
                EXIT DISPLAY
        END DISPLAY

        IF pos <> 0 THEN
            EXIT WHILE
        END IF

    END WHILE

    CLOSE WINDOW retm8092

    RETURN codigo, descripcion

END FUNCTION
