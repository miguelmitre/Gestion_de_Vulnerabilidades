#################################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                               #
#Owner             => E.F.P.                                                    #
#Programa RETC823  => LIQUIDACION DE RCV - RETIRO C                             #
#                     (PENSION GARANTIZADA)                                     #
#Fecha creacion    => 11 DE FEBRERO DEL 2004                                    #
#Fecha actualiz.   => 04-Agosto-2004                                            #
#By                => DMR                                                       #
#Fecha actualiza   => 7 DE ENERO DE 2008                                        #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Sistema           => RET                                                       #
# Actualizacion    => JAVIER GONZALEZ JERONIMO                                  #
# Fecha            => 15 DE DICIEMBRE DE 2008                                   #
#                     Ajustes del programa para el proceso de mto constitutivo  #
#Fecha actualiz.   => 4 DE ABRIL DE 2011                                        #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                  => Se agrega la ejecucion del SPL que inserta en las tablas  #
#                     de las estadisticas de CONSAR (Req. EFPS-152)             #
#################################################################################
DATABASE safre_af

GLOBALS

    DEFINE gar_precio_acc ARRAY [20] OF RECORD #Arreglo para los precios_accion
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE gr_dat RECORD
        folio_oper_02         INTEGER ,
        fecha_val_acc         DATE
    END RECORD

    DEFINE gr_edo RECORD
        recibido              LIKE ret_estado.estado_solicitud ,
        enviado               LIKE ret_estado.estado_solicitud ,
        liquidado             LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE reg_20 RECORD #loc #reg_20
        estado_marca          SMALLINT,
        codigo_rechazo        SMALLINT,
        marca_causa           SMALLINT,
        fecha_causa           DATE
    END RECORD

    DEFINE
        HOY                   DATE

    DEFINE #glo #char
        gc_tipo_retiro        CHAR(001) ,
        gc_usuario            CHAR(008) ,
        enter                 CHAR(001) ,
        v_liquida             CHAR(100) ,
        v_desmarca            CHAR(100)

    DEFINE
        gs_cod_tramite          ,
        gs_tipo_mov             SMALLINT

    DEFINE
        gi_liquidados           INTEGER

END GLOBALS


MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("RETC823.log")

    CALL init() #i
    CALL f_abre_ventana()
    CALL f_obtiene_precios_accion(HOY)
    CALL f_captura_datos()

    CALL primer_paso()      -- Determina importes y afecta cuenta individual
    CALL segundo_paso()     -- Desmarca las cuentas liquidadas

    CLOSE WINDOW retc8231
    CALL f_abre_ventana()

    DISPLAY gr_dat.folio_oper_02 AT 08,43
    DISPLAY gr_dat.fecha_val_acc USING "DD-MM-YYYY" AT 10,37
    DISPLAY "NUMERO DE REGISTROS PROCESADOS  : ",gi_liquidados AT 13,21

    PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR CHAR enter
    CLOSE WINDOW retc8231
END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()

    DEFINE
        lc_prepare      CHAR(300)

    -- -----------------------------------------------------------------------------

    LET HOY                     = TODAY
    LET gr_dat.fecha_val_acc    = HOY USING "MM-DD-YYYY"
    LET gc_tipo_retiro          = "C"

    SELECT USER
    INTO   gc_usuario
    FROM   tab_afore_local

    ----- ESTADOS DE SOLICITUD -----
    SELECT A.estado_solicitud
    INTO   gr_edo.recibido
    FROM   ret_estado A
    WHERE  A.descripcion = "RECEPCIONADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.enviado
    FROM   ret_estado A
    WHERE  A.descripcion = "ENVIADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.liquidado
    FROM   ret_estado A
    WHERE  A.descripcion = "LIQUIDADO"

    ----- CODIGO DE TRAMITE -----
    SELECT cod_tramite
    INTO   gs_cod_tramite
    FROM   tab_tipo_tramite
    WHERE  descripcion = "TRANSFERENCIA"

    ----- FOLIO MAXIMO UTILIZADO -----
    SELECT MAX(folio)
    INTO   gr_dat.folio_oper_02
    FROM   ret_transf_rx
    WHERE  estado_solicitud = gr_edo.enviado
    AND    tipo_retiro      = gc_tipo_retiro

    ----- TIPO MOVIMIENTO -----
    SELECT A.movimiento
    INTO   gs_tipo_mov
    FROM   tab_retiro A
    WHERE  tipo_retiro = gc_tipo_retiro

    ----- DESMARCA CUENTA -----
    LET lc_prepare = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? ) "
    PREPARE eje_desmarca FROM lc_prepare

    LET lc_prepare = " "

    --- LIQUIDACION ---
    LET lc_prepare = " EXECUTE PROCEDURE fn_liquida ( ?,?,?,?,? )"
    PREPARE eje_liquida FROM lc_prepare

    LET lc_prepare = " "

    ----- INSERTA EN TABLAS CONSAR  -----
    LET lc_prepare = "EXECUTE FUNCTION fn_inserta_consar(?,?,?,?)"
    PREPARE eje_CONSAR FROM lc_prepare

    LET lc_prepare = " "

END FUNCTION

#---------------------------------------------------------------------------#
# primer_paso : Afecta montos en la cuenta individual y actualiza el estado #
#               de la solicitud en los nss liquidados                       #
#---------------------------------------------------------------------------#
FUNCTION primer_paso()

    DEFINE lr_provi RECORD LIKE dis_provision.*

    DEFINE lr_liquida RECORD 
        nss                 LIKE dis_cuenta.nss             ,
        consecutivo_lote    LIKE dis_cuenta.consecutivo_lote,
        folio               LIKE dis_cuenta.folio               
    END RECORD

    DEFINE
        ls_inserta              ,
        ls_confirma             SMALLINT

    -- -----------------------------------------------------------------------------

    SELECT COUNT(UNIQUE A.nss)
    INTO   gi_liquidados
    FROM   dis_provision  A
    WHERE  A.folio           = gr_dat.folio_oper_02
    AND    A.tipo_movimiento = gs_tipo_mov

    DECLARE cur_liq CURSOR FOR
    SELECT UNIQUE nss   , 
           folio        , 
           subcuenta
    FROM   dis_provision
    WHERE  folio           = gr_dat.folio_oper_02
    AND    tipo_movimiento = gs_tipo_mov

    FOREACH cur_liq INTO lr_provi.nss   , 
                         lr_provi.folio , 
                         lr_provi.subcuenta
        
        LET ls_confirma = 0

        DECLARE cur_sp2 CURSOR FOR eje_liquida
        OPEN cur_sp2 USING lr_provi.folio      ,
                           lr_provi.nss        ,
                           lr_provi.subcuenta  ,
                           gr_dat.fecha_val_acc ,
                           HOY
        FETCH cur_sp2 INTO ls_confirma

        CLOSE cur_sp2

        DISPLAY "NUMERO DE REGISTROS PROCESADOS ",gi_liquidados AT 13,17

    END FOREACH

    -- Inserta montos a estadisticas CONSAR
    DECLARE cur_liquida CURSOR FOR
    SELECT UNIQUE(nss)      ,
           consecutivo_lote ,
           folio
    FROM   dis_cuenta
    WHERE  folio            = gr_dat.folio_oper_02
    AND    tipo_movimiento  = gs_tipo_mov

    FOREACH cur_liquida INTO lr_liquida.*

        EXECUTE eje_CONSAR USING lr_liquida.nss                 ,
                                 lr_liquida.consecutivo_lote    ,
                                 lr_liquida.folio               ,
                                 gs_cod_tramite
                           INTO  ls_inserta

        INITIALIZE lr_liquida.* TO NULL

    END FOREACH

    UPDATE ret_transf_rx
    SET    estado_solicitud = gr_edo.liquidado
    WHERE  folio            = gr_dat.folio_oper_02
    AND    tipo_retiro      = gc_tipo_retiro

    INSERT INTO ret_seguimiento
    VALUES(gr_dat.folio_oper_02 , -- folio
           "C"                  , -- tipo_oper_recep
           ""                   , -- fecha_recepcion
           "03"                 , -- tipo_oper_envio
           HOY                  , -- fecha_envio
           gr_edo.liquidado     , -- status
           0
          )
END FUNCTION

#---------------------------------------------------------------------------#
# segundo_paso : Realiza la desmarca de las cuentas que fueron liquidadas   #
#---------------------------------------------------------------------------#
FUNCTION segundo_paso()

    DEFINE pr_elimina RECORD
        nss         LIKE dis_provision.nss              ,
        consecu     LIKE dis_provision.consecutivo_lote
    END RECORD
    
    -- -----------------------------------------------------------------------------
                                                                            
    DECLARE cur_des CURSOR FOR                                  
    SELECT nss          ,
           consecutivo
    FROM   ret_transf_rx
    WHERE  folio            = gr_dat.folio_oper_02
    AND    tipo_retiro      = gc_tipo_retiro
    AND    estado_solicitud = gr_edo.liquidado

    FOREACH cur_des INTO pr_elimina.*
        
        CALL f_desmarca_cuenta(pr_elimina.*)

        SELECT "OK"
        FROM   cta_act_marca
        WHERE  nss         = pr_elimina.nss
        AND    correlativo = pr_elimina.consecu
        AND    marca_cod   = 140

        IF STATUS <> NOTFOUND THEN
            CALL f_solicita_edo_cuenta(pr_elimina.nss)
        END IF
    
    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# f_abre_ventana : Abre la ventana general del programa                     #
#---------------------------------------------------------------------------#
FUNCTION f_abre_ventana()

    OPEN WINDOW retc8231 AT 4,4 WITH FORM "RETC8231" ATTRIBUTE(BORDER)
    DISPLAY "   < Ctrl-C > Salir                                        RETIRO 'C'       " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC823       GENERA LIQUIDACION DE PENSION GARANTIZADA                    " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

END FUNCTION

#---------------------------------------------------------------------------#
# f_solicita_edo_cuenta : Actualiza la tabla cta_ctr_cuenta para indicar    #
#                         una solicitud de edo. de cuenta                   #
#---------------------------------------------------------------------------#
FUNCTION f_solicita_edo_cuenta(pc_nss)

    DEFINE pc_nss LIKE cta_ctr_cuenta.nss

    -- -----------------------------------------------------------------------------

    UPDATE cta_ctr_cuenta
    SET    tipo_informe  = 5    ,
           fecha_informe = HOY
    WHERE  nss           = pc_nss

END FUNCTION

#---------------------------------------------------------------------------#
# f_captura_datos : Captura el folio y la fecha de proceso para ejecutar la #
#                   liquidacion                                             #
#---------------------------------------------------------------------------#
FUNCTION f_captura_datos()

    DEFINE
        ls_status       SMALLINT

    DEFINE
        lc_mensaje_err  CHAR(100)


    INPUT BY NAME gr_dat.* WITHOUT DEFAULTS
        
        AFTER FIELD folio_oper_02

            IF gr_dat.folio_oper_02 IS NULL THEN
                ERROR "    EL FOLIO NO PUEDE SER NULO " ATTRIBUTE(REVERSE)
                NEXT FIELD folio_oper_02
            ELSE
                SELECT "OK"
                FROM   ret_transf_rx
                WHERE  folio       = gr_dat.folio_oper_02
                AND    tipo_retiro = gc_tipo_retiro
                GROUP BY 1
                
                IF STATUS = NOTFOUND THEN
                    ERROR "    FOLIO INEXISTENTE " ATTRIBUTE(REVERSE)
                    NEXT FIELD folio_oper_02
                ELSE
                    CALL f_valida_edo_sol() 
                        RETURNING ls_status, lc_mensaje_err

                    IF ls_status <> 0 THEN
                        ERROR lc_mensaje_err ATTRIBUTE(REVERSE)
                        NEXT FIELD folio_oper_02
                    END IF
                END IF
            END IF

        AFTER FIELD fecha_val_acc
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD folio_oper_02
            END IF

            IF gr_dat.fecha_val_acc IS NULL THEN
                ERROR "    LA FECHA NO PUEDE SER NULA " ATTRIBUTE(REVERSE)
                NEXT FIELD fecha_val_acc
            END IF

        ON KEY (ESC)
        
            IF gr_dat.folio_oper_02 IS NULL THEN
                ERROR "    EL FOLIO NO PUEDE SER NULO " ATTRIBUTE(REVERSE)
                NEXT FIELD folio_oper_02
            ELSE
                SELECT "OK"
                FROM   ret_transf_rx
                WHERE  folio  = gr_dat.folio_oper_02
                AND    tipo_retiro = gc_tipo_retiro
                GROUP BY 1

                IF STATUS = NOTFOUND THEN
                    ERROR "    FOLIO INEXISTENTE " ATTRIBUTE(REVERSE)
                    NEXT FIELD folio_oper_02
                ELSE
                     CALL f_valida_edo_sol() 
                        RETURNING ls_status, lc_mensaje_err

                    IF ls_status <> 0 THEN
                        ERROR lc_mensaje_err ATTRIBUTE(REVERSE)
                        NEXT FIELD folio_oper_02
                    END IF
                END IF
            END IF

            IF gr_dat.fecha_val_acc IS NULL THEN
                ERROR "    LA FECHA NO PUEDE SER NULA " ATTRIBUTE(REVERSE)
                NEXT FIELD fecha_val_acc
            END IF

            EXIT INPUT

        ON KEY (CONTROL-C)
            PROMPT" PROCESO CANCELADO...< ENTER > PARA SALIR "FOR CHAR enter
            EXIT PROGRAM

        ON KEY (INTERRUPT)
            PROMPT" PROCESO CANCELADO...< ENTER > PARA SALIR "FOR CHAR enter
            EXIT PROGRAM
    END INPUT

    WHILE TRUE
        PROMPT " ESTA SEGURO S/N ? " FOR CHAR enter
        IF enter MATCHES "[sSnN]" THEN
            IF enter MATCHES "[sS]" THEN
                DISPLAY " PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)
                EXIT WHILE
            ELSE
                PROMPT" PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
                EXIT PROGRAM
            END IF
        END IF
    END WHILE

END FUNCTION

#---------------------------------------------------------------------------#
# f_obtiene_precios_accion : Valida y obtiene los precios de accion para el #
#                            dia en curso                                   #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_precios_accion(pdt_fec_precios)
    
    DEFINE #loc #date
        pdt_fec_precios         DATE

    DEFINE lr_precio_acc RECORD
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE
        lc_precios_acc        CHAR(100) ,
        lc_mensaje            CHAR(100) ,
        lc_siefore            CHAR(002)

    DEFINE
        ls_sie                SMALLINT

    -- -----------------------------------------------------------------------------

    LET ls_sie = 1

    LET lc_precios_acc = " EXECUTE FUNCTION fn_verifica_precio_accion (?)"
    PREPARE eje_precios_accion FROM lc_precios_acc

    DECLARE c_precios CURSOR FOR eje_precios_accion
    FOREACH c_precios USING pdt_fec_precios
                      INTO lr_precio_acc.*

        IF lr_precio_acc.estado <> 0 THEN
            LET lc_siefore = lr_precio_acc.siefore

            LET lc_mensaje = " FALTAN PRECIOS DE ACCION: FECHA ", lr_precio_acc.fecha USING "DD/MM/yyyy",
                             " -- SIEFORE ", lc_siefore CLIPPED

            PROMPT lc_mensaje FOR CHAR enter
            EXIT PROGRAM
        ELSE
            LET ls_sie = lr_precio_acc.siefore
            LET gar_precio_acc[ls_sie].* = lr_precio_acc.*
        END IF
    
    END FOREACH

END FUNCTION

#---------------------------------------------------------------------------#
# f_desmarca_cuenta : Ejecuta el script para realizar la desmarca de la     #
#                     cuenta cuando esta se haya liquidado                  #
#---------------------------------------------------------------------------#
FUNCTION f_desmarca_cuenta(pr_desmarca)

    DEFINE pr_desmarca RECORD
        nss         LIKE ret_transf_rx.nss          ,
        consec      LIKE ret_transf_rx.consecutivo
    END RECORD

    DEFINE lr_dat RECORD
        edo_marca       SMALLINT ,
        marca_causa     SMALLINT
    END RECORD

    -- ---------------------------------------------------------------------------------

    LET lr_dat.edo_marca    = 0
    LET lr_dat.marca_causa  = 0

    --DESMARCA DE LA CUENTA --
    EXECUTE eje_desmarca USING pr_desmarca.nss      ,--nss
                               gs_tipo_mov          ,--marca entrante
                               pr_desmarca.consec   ,--consecutivo
                               lr_dat.edo_marca     ,--estado_marco
                               lr_dat.marca_causa   ,--marca_causa
                               gc_usuario            --usuario

END FUNCTION

#---------------------------------------------------------------------------#
# f_valida_edo_sol : Valida que el estado de solicitud se el correcto       #
#---------------------------------------------------------------------------#
FUNCTION f_valida_edo_sol()

    DEFINE
        ls_err      ,
        ls_estado   SMALLINT

    DEFINE
        lc_mensaje  CHAR(100)

    SELECT MAX(estado_solicitud)
    INTO   ls_estado
    FROM   ret_transf_rx
    WHERE  folio       = gr_dat.folio_oper_02
    AND    tipo_retiro = gc_tipo_retiro
    
    CASE ls_estado
        WHEN gr_edo.recibido
            LET lc_mensaje = "    EL FOLIO INGRESADO AUN NO HA SIDO PROVISIONADO..."
            LET ls_err = 1
        
        WHEN gr_edo.enviado
            LET lc_mensaje = " "
            LET ls_err = 0
        
        WHEN gr_edo.liquidado
            LET lc_mensaje = "    EL FOLIO INGRESADO YA FUE LIQUIDADO..."
            LET ls_err = 1
        
        OTHERWISE
            LET lc_mensaje = "    ERROR: EL ESTATUS DEL FOLIO DEBE SER: 04 (ENVIADO)"
            LET ls_err = 1
    END CASE

    RETURN ls_err, lc_mensaje

END FUNCTION
