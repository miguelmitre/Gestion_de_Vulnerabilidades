#################################################################################
#Owner             => E.F.P.                                                    #
#Programa PENM103  => CALCULA LA SUFICIENCIA DE SALDO PARA UN NSS DADO          #
#                                                                               #
#Fecha creacion    => 14 DE JUNIO DE 2011                                       #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   =>                                                           #
#Actualizacion     =>                                                           #
#                  =>                                                           #
#                                                                               #
#Sistema           => RET                                                       #
#################################################################################

DATABASE safre_af

GLOBALS

    DEFINE gr_datos RECORD
        nss                 LIKE pen_solicitud_pmg.nss  ,
        fecha_ini_pen       LIKE pen_solicitud_pmg.fecha_ini_pen
    END RECORD

    DEFINE gr_flag RECORD
        cierto      SMALLINT    ,
        falso       SMALLINT    ,
        otro        SMALLINT
    END RECORD

    DEFINE 
        enter                   CHAR(001) 

    DEFINE 
        HOY                     DATE

    DEFINE
        gs_salida               ,
        gs_consulta             SMALLINT

END GLOBALS

MAIN 

    DEFER INTERRUPT

    OPTIONS
        INPUT WRAP          ,
        PROMPT LINE LAST    ,
        ACCEPT KEY CONTROL-I

    CALL init()    
    CALL f_abre_pantalla()

    WHILE gs_consulta
        CALL f_captura_datos() RETURNING gr_datos.*, gs_salida
        
        IF gs_salida = gr_flag.falso THEN
            CALL f_despliega_montos(gr_datos.*)

            WHILE TRUE
                PROMPT "¿DESEA REALIZAR UNA NUEVA CONSULTA? (S/N) : " FOR CHAR enter
            
                IF enter MATCHES "[SsNn]" THEN
                    IF enter MATCHES "[Ss]" THEN
                        LET gs_consulta = gr_flag.cierto
                    ELSE
                        LET gs_consulta = gr_flag.falso
                    END IF
                    
                    EXIT WHILE
                END IF
            END WHILE
        ELSE
            CALL f_error_msg("PROCESO CANCELADO")
            EXIT WHILE            
        END IF
        
    END WHILE

    CLOSE WINDOW PENM1031

END MAIN

#---------------------------------------------------------------------------#
# init : Inicializa las variables globales que se usaran en el programa     #
#---------------------------------------------------------------------------#
FUNCTION init()
    
    DEFINE
        lc_prepare      CHAR(300)

    -- -----------------------------------------------------------------------------
    
    ----- VARIABLES GLOBALES -----
    LET HOY             = TODAY
    LET gs_consulta     = 1
    LET gr_flag.cierto  = 1
    LET gr_flag.falso   = 0
    LET gr_flag.otro    = 2

    ----- SALDO AL DIA  -----
    LET lc_prepare = " EXECUTE FUNCTION fn_saldo_dia (?,?,?,?) "
    PREPARE eje_saldo_dia FROM lc_prepare

    LET lc_prepare = " "

END FUNCTION

#---------------------------------------------------------------------------#
# f_captura_datos : Captura los datos necesarios para el proceso            #
#---------------------------------------------------------------------------#
FUNCTION f_captura_datos()

    DEFINE lr_datos RECORD
        nss                 LIKE pen_solicitud_pmg.nss  ,
        fecha_ini_pen       LIKE pen_solicitud_pmg.fecha_ini_pen
    END RECORD
    
    DEFINE 
        ls_exit             SMALLINT
    
    -- -----------------------------------------------------------------------------        

    LET lr_datos.fecha_ini_pen  = HOY
    LET ls_exit                 = gr_flag.falso
    
    INPUT BY NAME lr_datos.* WITHOUT DEFAULTS
        AFTER FIELD nss
            IF lr_datos.nss IS NULL THEN
                ERROR "    NSS NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                NEXT FIELD nss
            END IF

        AFTER FIELD fecha_ini_pen
            IF lr_datos.fecha_ini_pen IS NULL THEN
                ERROR "    NSS NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                NEXT FIELD fecha_ini_pen
            END IF

        ON KEY (ESC)
            IF lr_datos.nss IS NULL THEN
                ERROR "    NSS NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                NEXT FIELD nss
            END IF

            IF lr_datos.fecha_ini_pen IS NULL THEN
                ERROR "    NSS NO PUEDE SER NULO" ATTRIBUTE(NORMAL)
                NEXT FIELD fecha_ini_pen
            END IF

            EXIT INPUT

        ON KEY (CONTROL-C, INTERRUPT)
            LET ls_exit = gr_flag.cierto
            EXIT INPUT
    END INPUT
    
    RETURN lr_datos.*, ls_exit
 
END FUNCTION 


#---------------------------------------------------------------------------#
# f_despliega_montos : Despliega en pantalla los montos del trabajador      #
#                      e indica si tiene o no suficiencia de saldo          #
#---------------------------------------------------------------------------#
FUNCTION f_despliega_montos(pr_captura)

    DEFINE pr_captura RECORD
        nss                 LIKE pen_solicitud_pmg.nss  ,
        fecha_ini_pen       LIKE pen_solicitud_pmg.fecha_ini_pen
    END RECORD

    DEFINE lar_pantalla ARRAY[1] OF RECORD
        pesos_rcv               DECIMAL(16,6)   ,
        pesos_viv               DECIMAL(16,6)   ,
        retroactivo             DECIMAL(16,6)   ,
        retroactivo_11p         DECIMAL(16,6)   ,
        suf_con_viv             CHAR(1)         ,
        suf_sin_viv             CHAR(1)         ,
        suf_con_viv_11p         CHAR(1)         ,
        suf_sin_viv_11p         CHAR(1)
    END RECORD
    
    DEFINE lr_montos RECORD
        retroactivo             DECIMAL(16,6),
        retroactivo_11p         DECIMAL(16,6),
        devengado               DECIMAL(16,6),
        devengado_11p           DECIMAL(16,6),
        imp_mensual             DECIMAL(10,2),
        imp_mensual_11p         DECIMAL(10,2)
    END RECORD

    DEFINE lr_nombre RECORD
        paterno             CHAR(40),
        materno             CHAR(40),
        nombres             CHAR(40)
    END RECORD
    
    -- -----------------------------------------------------------------------------
    
    OPEN WINDOW PENM1032 AT 2,2 WITH FORM "PENM1032" ATTRIBUTE(BORDER)
    DISPLAY " < Ctrl-C > Salir                                                              " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " PENM103         CALCULO DE SUFICIENCIA DE SALDO DE PMG                        " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)    

    DISPLAY BY NAME pr_captura.nss
    DISPLAY BY NAME pr_captura.fecha_ini_pen

    INITIALIZE lar_pantalla[1].* TO NULL

    CALL f_obtiene_nombre(pr_captura.nss) RETURNING lr_nombre.*
    DISPLAY BY NAME lr_nombre.*

    CALL f_obtiene_saldo_pesos(pr_captura.nss, HOY)
        RETURNING lar_pantalla[1].pesos_rcv ,
                  lar_pantalla[1].pesos_viv

    CALL f_calcula_retroactivo(pr_captura.*, HOY)
        RETURNING lr_montos.*

    LET lar_pantalla[1].retroactivo         = lr_montos.retroactivo    
    LET lar_pantalla[1].retroactivo_11p     = lr_montos.retroactivo_11p

    -- determina los diferentes casos de suficiencia de saldo
    IF lr_montos.retroactivo <= (lar_pantalla[1].pesos_rcv + lar_pantalla[1].pesos_viv) THEN
        LET lar_pantalla[1].suf_con_viv = "X"
    END IF

    IF lr_montos.retroactivo <= lar_pantalla[1].pesos_rcv THEN
        LET lar_pantalla[1].suf_sin_viv = "X"
    END IF        

    IF lr_montos.retroactivo_11p <= (lar_pantalla[1].pesos_rcv + lar_pantalla[1].pesos_viv) THEN
        LET lar_pantalla[1].suf_con_viv_11p = "X"
    END IF

    IF lr_montos.retroactivo_11p <= lar_pantalla[1].pesos_rcv THEN
        LET lar_pantalla[1].suf_sin_viv_11p = "X"
    END IF                
    
    IF (lar_pantalla[1].suf_con_viv IS NULL) AND (lar_pantalla[1].suf_sin_viv IS NULL) AND
       (lar_pantalla[1].suf_con_viv_11p IS NULL) AND (lar_pantalla[1].suf_sin_viv_11p IS NULL) THEN
    
        DISPLAY "EL NSS NO TIENE SALDO SUFICIENTE PARA PAGAR LA PMG" AT 20,1 
         ATTRIBUTES (REVERSE)
    
    END IF

    CALL SET_COUNT(1)
    DISPLAY ARRAY lar_pantalla TO scr_det.*    

    CLOSE WINDOW PENM1032

END FUNCTION


#---------------------------------------------------------------------------#
# f_obtiene_saldo_pesos : Obtiene para el nss dado el monto de rcv y viv    #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_saldo_pesos(pc_nss, pdt_fecha_opera)

    DEFINE pc_nss LIKE pen_solicitud_pmg.nss
    
    DEFINE 
        pdt_fecha_opera         DATE
    
    DEFINE lr_pesos RECORD
        rcv           DECIMAL(16,6)   ,
        viv           DECIMAL(16,6)
    END RECORD

    DEFINE lr_saldo RECORD
        subcta      SMALLINT        ,
        sie         SMALLINT        ,
        monto_acc   DECIMAL(16,6)   ,
        monto_pes   DECIMAL(16,6)
    END RECORD

    DEFINE
        ls_subcta               ,
        ls_grupo                SMALLINT

    -- -----------------------------------------------------------------------------
    
    LET ls_subcta       = 0
    LET ls_grupo        = 0
    LET lr_pesos.rcv    = 0
    LET lr_pesos.viv    = 0

    DECLARE cur_saldo CURSOR FOR eje_saldo_dia

    FOREACH cur_saldo USING pc_nss          ,
                            ls_subcta       ,
                            ls_grupo        ,
                            pdt_fecha_opera
                      INTO  lr_saldo.*

        IF (lr_saldo.subcta = 1) OR (lr_saldo.subcta = 2) OR (lr_saldo.subcta = 5) OR 
           (lr_saldo.subcta = 6) OR (lr_saldo.subcta = 9) THEN 
            LET lr_pesos.rcv    = lr_pesos.rcv + lr_saldo.monto_pes
        ELSE
            IF lr_saldo.subcta = 4 THEN
                LET lr_pesos.viv    = lr_pesos.viv + lr_saldo.monto_pes
            END IF
        END IF
    
    END FOREACH 

    RETURN lr_pesos.*

END FUNCTION


#---------------------------------------------------------------------------#
# f_calcula_retroactivo : obtiene el monto retroactivo a pagar por el nss   #
#---------------------------------------------------------------------------#
FUNCTION f_calcula_retroactivo(pc_captura, pdt_fecha_calc)

    DEFINE pc_captura RECORD
        nss                 LIKE pen_solicitud_pmg.nss  ,
        fecha_ini_pen       LIKE pen_solicitud_pmg.fecha_ini_pen
    END RECORD
    
    DEFINE
        pdt_fecha_calc          DATE

    DEFINE lr_montos RECORD
        retroactivo             DECIMAL(16,6),
        retroactivo_11p         DECIMAL(16,6),
        devengado               DECIMAL(16,6),
        devengado_11p           DECIMAL(16,6),
        imp_mensual             DECIMAL(10,2),
        imp_mensual_11p         DECIMAL(10,2)
    END RECORD

    DEFINE 
        ldt_fec_activa           ,
        ldt_fec_ciclo            ,
        ldt_fec_fin_ciclo        ,
        ldt_fec_ini_pen          DATE

    DEFINE 
        ls_cont_year             SMALLINT

    -- -----------------------------------------------------------------------------

    LET lr_montos.retroactivo       = 0
    LET lr_montos.retroactivo_11p   = 0
    LET lr_montos.devengado         = 0
    LET lr_montos.devengado_11p     = 0
    LET lr_montos.imp_mensual       = 0
    LET lr_montos.imp_mensual_11p   = 0 
    
    -- Obtenemos la fecha actual para el pago de pmg
    SELECT fecha_desde
    INTO   ldt_fec_activa
    FROM   tab_pmg_historica
    WHERE  fecha_hasta IS NULL
    
    -- La fecha para detener el ciclo es el primer dia del mes anterior
    LET ldt_fec_fin_ciclo   = MONTH(pdt_fecha_calc)||"/01/"||YEAR(pdt_fecha_calc)
    LET ldt_fec_fin_ciclo   = ldt_fec_fin_ciclo - 1 UNITS MONTH

    -- Obtenemos la fecha del primer dia del mes de la fecha ini pen
    LET ldt_fec_ini_pen = MONTH(pc_captura.fecha_ini_pen)||"/01/"||YEAR(pc_captura.fecha_ini_pen)

    -- Ciclamos para acumular el monto de pmg para cada mes desde 
    -- la fecha ini pen hasta un mes antes a la fecha dada 
    WHILE ldt_fec_fin_ciclo >= ldt_fec_ini_pen
        
        LET ls_cont_year = ls_cont_year + 1
        
        -- Si la fecha del ciclo es mayor a la actual vigente se paga el monto actual de pmg
        IF ldt_fec_ini_pen >= ldt_fec_activa THEN
            SELECT importe_mensual
            INTO   lr_montos.imp_mensual
            FROM   tab_pmg_historica
            WHERE  fecha_hasta IS NULL

            SELECT importe_mensual_11p
            INTO   lr_montos.imp_mensual_11p
            FROM   tab_pmg_historica
            WHERE  fecha_hasta IS NULL      
        ELSE
            SELECT importe_mensual
            INTO   lr_montos.imp_mensual
            FROM   tab_pmg_historica
            WHERE  fecha_desde <= ldt_fec_ini_pen
            AND    fecha_hasta >= ldt_fec_ini_pen

            SELECT importe_mensual_11p
            INTO   lr_montos.imp_mensual_11p
            FROM   tab_pmg_historica
            WHERE  fecha_desde <= ldt_fec_ini_pen
            AND    fecha_hasta >= ldt_fec_ini_pen                
        END IF

        IF ls_cont_year <= 12 THEN
            LET lr_montos.retroactivo      = lr_montos.retroactivo + lr_montos.imp_mensual
            LET lr_montos.retroactivo_11p  = lr_montos.retroactivo_11p + lr_montos.imp_mensual_11p
        ELSE
            LET lr_montos.devengado        = lr_montos.devengado + lr_montos.imp_mensual
            LET lr_montos.devengado_11p    = lr_montos.devengado_11p + lr_montos.imp_mensual_11p
        END IF;

        LET ldt_fec_ini_pen = ldt_fec_ini_pen + 1 UNITS MONTH;
    
    END WHILE
    
    RETURN lr_montos.*

END FUNCTION


#---------------------------------------------------------------------------#
# f_obtiene_nombre : Obtiene el nombre completo del nss indicado            #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_nombre(l_nss)

    DEFINE
        l_nss          CHAR(11)

    DEFINE lr_nombre_afiliado RECORD
        paterno        CHAR(40),
        materno        CHAR(40),
        nombres        CHAR(40)
    END RECORD

    -- -----------------------------------------------------------------------------

    SELECT paterno,
           materno,
           nombres
    INTO   lr_nombre_afiliado.*
    FROM   afi_mae_afiliado
    WHERE  n_seguro = l_nss

    RETURN lr_nombre_afiliado.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_abre_pantalla : Abre la forma que muestra la pantalla principal         #
#---------------------------------------------------------------------------#
FUNCTION f_abre_pantalla()

    OPEN WINDOW PENM1031 AT 2,2 WITH FORM "PENM1031" ATTRIBUTE(BORDER)
    DISPLAY " < Ctrl-C > Salir                                                              " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " PENM103         CALCULO DE SUFICIENCIA DE SALDO DE PMG                        " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

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
