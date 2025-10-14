################################################################################
#Proyecto          => SISTEMA DE AFORE( SAFRE )                                #
#Owner             => E.F.P.                                                   #
#Programa RETC857  => GENERA LA OPERACION 16 DE RETIROS PARCIALES PARA TODOS   #
#                     TIPOS DE DESEMPLEO Y EL RETIRO POR MATRIMONIO            #
#Fecha creacion    => 25 DE JUNIO DE 2009                                      #
#By                => JAVIER GONZALEZ JERONIMO                                 #
#Fecha actualiz.   => 21 DE JULIO DE 2009                                      #
#Actualizacion     => Se cambio para que los retiros tipo B con modalidad de   #
#                  => pago de tipo A se reporten como tipo 'A' en la op 16     #
#Sistema           => RET                                                      #
#------------------------------------------------------------------------------#
#Fecha y Autor     => 28-Mar-2014  Cristina Abasolo Tapia                      #
#Actualizacion     => Se adecua el programa para enviar la operacion 16        #
#                  => en base a la nueva plataforma de Retiros                 #
################################################################################
DATABASE safre_af

GLOBALS
    
    DEFINE reg_1 RECORD #glo #reg_1
        folio          INTEGER,
        fecha          DATE
    END RECORD

    DEFINE gr_seg_modulo RECORD LIKE seg_modulo.*
    
    DEFINE 
        gr_enviaOp16    RECORD LIKE ret_bus_diag16.*

    DEFINE gar_precio_acc ARRAY [20] OF RECORD #Arreglo para los precios_accion
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE gr_edo RECORD #gr_edo
        enviado               LIKE ret_estado.estado_solicitud ,
        liquidado             LIKE ret_estado.estado_solicitud ,
        enviado_op16          LIKE ret_estado.estado_solicitud
    END RECORD

    DEFINE gr_movs RECORD #gr_edo
        matrimonio      SMALLINT,
        tipo_a          SMALLINT,
        tipo_b          SMALLINT,
        tipo_c          SMALLINT,
        tipo_d          SMALLINT
    END RECORD


    DEFINE gr_cont RECORD
        total       INTEGER,
        tipo_a      INTEGER,
        tipo_b      INTEGER,
        tipo_c      INTEGER,
        tipo_d      INTEGER,
        matrimonio  INTEGER
    END RECORD

    DEFINE gr_prest RECORD
        desempleo       SMALLINT,
        matrimonio      SMALLINT
    END RECORD

    DEFINE #glo #smallint
        gs_salida             ,
        gs_codigo_afore       SMALLINT

    DEFINE #glo #char
        gc_usuario            CHAR(012) ,
        c7_nombre_plano       CHAR(007) ,
        c12_nombre_plano      CHAR(016) ,
        c12_nombre_op16       CHAR(012) ,
        gc_comando            CHAR(200) ,
        gc_prepare            CHAR(300) ,
        G_LISTA               CHAR(100) ,
        G_LISTA_1             CHAR(100) ,
        G_LISTA_2             CHAR(100) ,
        enter                 CHAR(001)

    DEFINE #glo #date
        HOY                   DATE

    DEFINE #glo #decimal
        gd_tot_monto_pesos    DECIMAL(15,2)
    
    DEFINE g_error       SMALLINT
    DEFINE g_cuantos     SMALLINT

END GLOBALS

DEFINE m_cadena      CHAR(1000)
DEFINE m_nom_tabla   CHAR(20)
DEFINE m_band_tabla  SMALLINT
DEFINE m_escoje_tb   CHAR(1)


MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP          ,
        PROMPT LINE LAST    ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG(FGL_GETENV("USER")||".RETC857.log") 

    CALL init() #i
    CALL f_captura_folio() RETURNING reg_1.*, gs_salida
    
    IF gs_salida = 0 THEN
        CALL f_genera_op16(reg_1.*)
    END IF

    CLOSE WINDOW retc8571

END MAIN

FUNCTION init()
#i-------------

    INITIALIZE reg_1.folio TO NULL
    INITIALIZE reg_1.fecha TO NULL

    LET gr_cont.tipo_a      = 0
    LET gr_cont.tipo_b      = 0
    LET gr_cont.tipo_c      = 0
    LET gr_cont.tipo_d      = 0
    LET gr_cont.matrimonio  = 0
    LET gr_cont.total       = 0
    
    LET HOY                 = TODAY
    LET gd_tot_monto_pesos  = 0
    LET gs_salida           = 0
    
    LET gr_prest.desempleo  = 6
    LET gr_prest.matrimonio = 7
    
    LET gr_movs.matrimonio  = 870
    LET gr_movs.tipo_d      = 875
    LET gr_movs.tipo_a      = 876
    LET gr_movs.tipo_b      = 877
    LET gr_movs.tipo_c      = 878
    
    LET g_error   = 0
    LET g_cuantos = 0
 
    
    SELECT codigo_afore   ,
           USER
    INTO   gs_codigo_afore ,
           gc_usuario
    FROM   tab_afore_local

    SELECT A.estado_solicitud
    INTO   gr_edo.enviado
    FROM   ret_estado A
    WHERE  A.descripcion = "ENVIADO"

    SELECT A.estado_solicitud
    INTO   gr_edo.liquidado
    FROM   ret_estado A
    WHERE  A.descripcion = "LIQUIDADO"
    
    SELECT A.estado_solicitud
    INTO   gr_edo.enviado_op16
    FROM   ret_estado A
    WHERE  A.descripcion = "ENVIADO OP16"

    SELECT *
    INTO   gr_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"
    
END FUNCTION


#-----------------------------------------------------------------------#
# Captura el folio y fecha de envío para la generacion de la op 16      #
#-----------------------------------------------------------------------#
FUNCTION f_captura_folio()

    DEFINE lr_captura RECORD
        folio       INTEGER,
        fecha       DATE
    END RECORD

    DEFINE
        ls_exit        ,
        ls_estado      SMALLINT

    DEFINE
        lc_mensaje     CHAR(100)

    OPEN WINDOW retc8571 AT 4,4 WITH FORM "RETC8571" ATTRIBUTE(BORDER)
    DISPLAY " <ESC> - Ejecutar                                          <CTRL-C> - Salir  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC857    ENVIO DE NOTIFICACION DE PAGOS DE PARCIALES IMSS                " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING"DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

    LET ls_exit             = 0
    LET lr_captura.folio    = NULL
    LET lr_captura.fecha    = HOY

    DISPLAY BY NAME lr_captura.folio,
                    lr_captura.fecha

    INPUT BY NAME lr_captura.* WITHOUT DEFAULTS

        AFTER FIELD folio
           IF lr_captura.folio IS NULL OR lr_captura.folio = 0 THEN
                 ERROR "ERROR, INGRESE UN FOLIO VALIDO"
                 NEXT FIELD folio
           END IF
        
        AFTER INPUT
           IF lr_captura.folio IS NULL OR lr_captura.folio = 0 THEN
                 ERROR "ERROR, INGRESE UN FOLIO VALIDO"
                 NEXT FIELD folio
           END IF
        
        ON KEY (ESC)
        
           IF lr_captura.folio IS NULL OR lr_captura.folio = 0 THEN
                 ERROR "ERROR, INGRESE UN FOLIO VALIDO"
                 NEXT FIELD folio
           END IF
           LET ls_exit = f_valida_ejecucion(lr_captura.*)  
           
           EXIT INPUT

        ON KEY (CONTROL-C)
            PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
            LET ls_exit = 1
            EXIT INPUT

        ON KEY (INTERRUPT)
            PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
            LET ls_exit = 1
            EXIT INPUT
    END INPUT

    RETURN lr_captura.*, ls_exit	

END FUNCTION

#-----------------------------------------------------------------------#
# Genera la op 16 para la fecha capturada por el usuario                #
#-----------------------------------------------------------------------#
FUNCTION f_genera_op16(pr_fliquida)

    DEFINE pr_fliquida RECORD
        folio          INTEGER,
        fecha          DATE  
    END RECORD

    DEFINE li_tot_des       SMALLINT

    DEFINE
        li_folio            INTEGER

    DEFINE 
        lc_registro         CHAR(600)
        
    DEFINE
        ldt_fecha_liq       DATE
        
    DEFINE ld_fecha_liquidacion DATE
    DEFINE ls_year              SMALLINT
    DEFINE lc_year              CHAR(4)
        
    LET li_tot_des = gr_cont.tipo_a + gr_cont.tipo_b + gr_cont.tipo_c + gr_cont.tipo_d

    DISPLAY "SOLICITUDES A ENVIAR "  AT 8,5
    DISPLAY "DESEMPLEO         : ", li_tot_des AT 10,5
    DISPLAY "MATRIMONIO        : ", gr_cont.matrimonio AT 10,44
    DISPLAY "          TIPO A  : ", gr_cont.tipo_a AT 11,5
    DISPLAY "          TIPO B  : ", gr_cont.tipo_b AT 12,5
    DISPLAY " COMPLEMENTARIOS  : ", gr_cont.tipo_c AT 13,5
    DISPLAY "      ANTERIORES  : ", gr_cont.tipo_d AT 14,5
    DISPLAY "TOTAL SOLICITUDES : ", gr_cont.total AT 16,5
    DISPLAY "         ENVIADAS : ", g_cuantos AT 17,5
    
    
    --DISPLAY "m_nom_tabla 2", m_nom_tabla
       
    LET m_cadena = " "
    LET m_cadena = " SELECT UNIQUE(fecha_conversion) FROM dis_cuenta    \n ",
                   " WHERE  folio =", pr_fliquida.folio,               "\n ",
                   " AND    tipo_movimiento IN (",gr_movs.matrimonio, ",\n ",
                   "                           ",gr_movs.tipo_d,      ",\n ",
                   "                           ",gr_movs.tipo_a,      ",\n ",
                   "                           ",gr_movs.tipo_b,      ",\n ",
                   "                           ",gr_movs.tipo_c,      ")\n "
                   
    IF m_band_tabla = 0 THEN
       	
       LET m_cadena = m_cadena clipped, 
       	              " union \n",
                      " SELECT UNIQUE(fecha_conversion) FROM ",m_nom_tabla CLIPPED," \n ",
                      " WHERE  folio =", pr_fliquida.folio,                         "\n ",
                      " AND    tipo_movimiento IN (",gr_movs.matrimonio,           ",\n ",
                      "                           ",gr_movs.tipo_d,                ",\n ",
                      "                           ",gr_movs.tipo_a,                ",\n ",
                      "                           ",gr_movs.tipo_b,                ",\n ",
                      "                           ",gr_movs.tipo_c,                ")\n ",
                      " ORDER BY 1 "

    END IF               
    LET m_cadena = m_cadena CLIPPED
    --DISPLAY "m_cadena 2", m_cadena  
    PREPARE pre_fol FROM m_cadena
    DECLARE cur_fol CURSOR FOR pre_fol    
    	
    FOREACH cur_fol INTO ldt_fecha_liq
      
        -- Validamos y obtenemos precios de accion del dia a generar la op 16
        CALL f_obtiene_precios_accion(ldt_fecha_liq)
        
        CALL f_op16_nuevos_des(pr_fliquida.folio, ldt_fecha_liq)
        
        CALL f_op16_mat_ant(pr_fliquida.folio, ldt_fecha_liq)
            
    END FOREACH

    PROMPT "SE CONCLUYO EL ENVIO DE SOLICITUDES ...<ENTER> PARA SALIR " FOR CHAR enter

END FUNCTION

#------------------------------------------------------------------------#
# Genera el detalle de la op 16 para los retiros parciales por matrimonio#
# y los retiros por desempleo con formato anterior                       #
#------------------------------------------------------------------------#
FUNCTION f_op16_mat_ant(p_folio_op13, p_fecha_liq)

    DEFINE
        p_folio_op13    INTEGER

    DEFINE
        p_fecha_liq     DATE
        
    DEFINE 
        li_tot_des      SMALLINT
        

    DEFINE lr_parcial_repo RECORD #loc #lr_parcial_repo
        folio_t_procesar      LIKE ret_parcial.folio_t_procesar  ,
        nss                   LIKE ret_parcial.nss                  ,
        curp                  LIKE ret_parcial.curp              ,
        consecutivo           LIKE ret_parcial.consecutivo          ,
        tipo_prestacion       LIKE ret_parcial.tipo_prestacion      ,
        cve_desempleo         LIKE tab_tipo_desempleo.cve_desempleo ,    
        num_mensualidad       LIKE ret_ctr_pago_det.num_mensualidad ,   
        mto_pagado            LIKE ret_parcial.pago_desempleo       ,
        fecha_solicitud       LIKE ret_parcial.fecha_solicitud      ,
        fecha_liquida         LIKE ret_parcial_tx.fecha_valuacion
    END RECORD

    DEFINE lr_par_query RECORD #loc #lr_parcial_repo
        folio_t_procesar      LIKE ret_parcial.folio_t_procesar  ,
        nss                   LIKE ret_parcial.nss                  ,
        curp                  LIKE ret_parcial.curp              ,
        consecutivo           LIKE ret_parcial.consecutivo          ,
        tipo_prestacion       LIKE ret_parcial.tipo_prestacion      ,
        pago_desempleo        LIKE ret_parcial.pago_desempleo       ,
        impt_autorizado       LIKE ret_parcial.impt_autorizado      ,
        fecha_solicitud       LIKE ret_parcial.fecha_solicitud      ,
        fecha_liquida         LIKE ret_parcial_tx.fecha_valuacion
    END RECORD

    DEFINE reg_18 RECORD #loc #reg_18
        dom_calle            LIKE afi_domicilio.calle       ,
        dom_numero_ext       LIKE afi_domicilio.numero      ,
        dom_numero_int       LIKE afi_domicilio.depto       ,
        dom_colonia          LIKE afi_domicilio.colonia     ,
        deleg_desc           LIKE tab_delegacion.deleg_desc ,
        dom_codpos           LIKE afi_domicilio.codpos      ,
        estad_desc           LIKE tab_estado.estad_desc
    END RECORD

    DEFINE
        v_siefore           SMALLINT

    DEFINE
        ld_pesos_liquida    DECIMAL(10,2),
        ld_total_rcv        DECIMAL(10,2)

    DECLARE cur_ant CURSOR FOR
    SELECT  UNIQUE A.folio_t_procesar  ,
            B.nss               ,
            A.curp              ,
            B.consecutivo       ,
            A.tipo_prestacion   ,
            B.pago_desempleo    ,
            A.impt_autorizado   ,
            A.fecha_solicitud   ,
            B.fecha_valuacion
    FROM    ret_parcial         A,
            ret_parcial_tx      B,
            ret_preliquida      C
    WHERE   B.folio            = p_folio_op13
    AND     C.fecha_conversion = p_fecha_liq
    AND     B.nss              = A.nss
    AND     C.consecutivo_lote = A.consecutivo
    AND     B.nss              = C.nss
    AND     B.consecutivo      = A.consecutivo
    AND     A.estado_solicitud = gr_edo.liquidado
    AND     A.diag_cuenta_ind  = 400 --Cuenta Aceptada
    AND    ( A.tipo_prestacion = gr_prest.matrimonio
             OR ( A.tipo_prestacion = gr_prest.desempleo AND A.tipo_desempleo = "D")
           )
    ORDER BY 5,2

    FOREACH cur_ant INTO lr_par_query.*

        LET lr_parcial_repo.folio_t_procesar = lr_par_query.folio_t_procesar
        LET lr_parcial_repo.nss              = lr_par_query.nss
        LET lr_parcial_repo.curp             = lr_par_query.curp
        LET lr_parcial_repo.consecutivo      = lr_par_query.consecutivo    
        LET lr_parcial_repo.tipo_prestacion  = lr_par_query.tipo_prestacion 
        LET lr_parcial_repo.fecha_solicitud  = lr_par_query.fecha_solicitud 
        LET lr_parcial_repo.fecha_liquida    = lr_par_query.fecha_liquida   
        LET lr_parcial_repo.num_mensualidad  = 0

        LET gr_cont.total = gr_cont.total + 1
        DISPLAY "TOTAL SOLICITUDES : ", gr_cont.total AT 16,5

        IF lr_par_query.tipo_prestacion = gr_prest.desempleo THEN
            LET gr_cont.tipo_d                = gr_cont.tipo_d + 1
            LET li_tot_des                    = gr_cont.tipo_a + gr_cont.tipo_b + gr_cont.tipo_c + gr_cont.tipo_d
            LET lr_parcial_repo.cve_desempleo = 4
            LET lr_parcial_repo.mto_pagado    = lr_par_query.pago_desempleo
            DISPLAY "DESEMPLEO         : ", li_tot_des AT 10,5
            DISPLAY "      ANTERIORES  : ", gr_cont.tipo_d AT 14,5
        ELSE
            LET gr_cont.matrimonio            = gr_cont.matrimonio + 1
            LET lr_parcial_repo.cve_desempleo = 0
            LET lr_parcial_repo.mto_pagado    = lr_par_query.impt_autorizado
            DISPLAY "MATRIMONIO        : ", gr_cont.matrimonio AT 10,44
        END IF
        
        LET v_siefore = f_obtiene_siefore(lr_parcial_repo.nss)

        -- Obtenemos el monto antes del retiro
        
        LET m_cadena = " "
        LET m_cadena = " SELECT SUM(NVL(monto_en_acciones,0)) * ",gar_precio_acc[v_siefore].precio_dia,"  \n ",
                       " FROM   dis_cuenta                                                                    \n ",
                       " WHERE  nss              =  '",lr_parcial_repo.nss,"'                                   \n ",
                       " AND    siefore          =  ",v_siefore,"                                             \n ",
                       " AND    folio            <> ",p_folio_op13,"                                          \n ",
                       " AND    fecha_conversion <= '",p_fecha_liq,"'                                           \n ",
                       " AND    subcuenta IN (1,2,5,6,9)" 
            
        LET m_cadena = m_cadena CLIPPED
                       
        PREPARE pre_monto_ant_ret_ant1 FROM m_cadena
        EXECUTE pre_monto_ant_ret_ant1 INTO ld_total_rcv             
        --DISPLAY "ld_total_rcv1", ld_total_rcv
        IF ld_total_rcv IS NULL THEN 
           IF m_band_tabla = 0 THEN
       	      LET m_cadena = " "
              LET m_cadena = " SELECT SUM(NVL(monto_en_acciones,0)) * ",gar_precio_acc[v_siefore].precio_dia,"  \n ",
                             " FROM   ",m_nom_tabla CLIPPED,"                                                       \n ",
                             " WHERE  nss              =  '",lr_parcial_repo.nss,"'                                   \n ",
                             " AND    siefore          =  ",v_siefore,"                                             \n ",
                             " AND    folio            <> ",p_folio_op13,"                                          \n ",
                             " AND    fecha_conversion <= '",p_fecha_liq,"'                                           \n ",
                             " AND    subcuenta IN (1,2,5,6,9)" 
            
                           
               LET m_cadena = m_cadena CLIPPED
               
               PREPARE pre_monto_ant_ret_ant2 FROM m_cadena
               EXECUTE pre_monto_ant_ret_ant2 INTO ld_total_rcv  
            END IF           
        END IF
        --DISPLAY "ld_total_rcv2", ld_total_rcv
        IF ld_total_rcv IS NULL THEN
            LET ld_total_rcv = 0
        END IF

        -- Obtenemos el monto pagado al trabajador (Se cambia el signo ya que es un retiro)
        
        LET m_cadena = " "
        LET m_cadena = " SELECT SUM(NVL(monto_en_acciones * -1,0)) * ",gar_precio_acc[v_siefore].precio_dia,"  \n ",
                       " FROM   dis_cuenta                                                                         \n ",
                       " WHERE  nss          = '",lr_parcial_repo.nss,"'                                           \n ",
                       " AND    siefore      = ",v_siefore,"                                                       \n ",
                       " AND    folio        = ",p_folio_op13,"                                                    \n ",
                       " AND    subcuenta IN (1,2,5,6,9)"
        
        LET m_cadena = m_cadena CLIPPED
        --DISPLAY "m_cadena 3 ", m_cadena
        PREPARE pre_monto_pg_trab_ant1 FROM m_cadena
        EXECUTE pre_monto_pg_trab_ant1 INTO ld_pesos_liquida
        
        IF ld_pesos_liquida IS NULL THEN
           IF m_band_tabla = 0 THEN
       	      LET m_cadena = " "
              LET m_cadena = " SELECT SUM(NVL(monto_en_acciones * -1,0)) * ",gar_precio_acc[v_siefore].precio_dia,"  \n ",
                             " FROM   ",m_nom_tabla CLIPPED,"                                                            \n ",
                             " WHERE  nss          = '",lr_parcial_repo.nss,"'                                           \n ",
                             " AND    siefore      = ",v_siefore,"                                                       \n ",
                             " AND    folio        = ",p_folio_op13,"                                                    \n ",
                             " AND    subcuenta IN (1,2,5,6,9)"
           
                          
              LET m_cadena = m_cadena CLIPPED
              
              PREPARE pre_monto_pg_trab_ant2 FROM m_cadena
              EXECUTE pre_monto_pg_trab_ant2 INTO ld_pesos_liquida
           END IF
        END IF
        
        IF ld_pesos_liquida IS NULL THEN
            LET ld_pesos_liquida = 0
        END IF
        
        -- Si el monto liquidado es igual a lo que se tenia en RCV entonces se
        -- reporta en la op 16 el monto liquidado.
        IF ld_pesos_liquida = ld_total_rcv THEN
            LET lr_parcial_repo.mto_pagado = ld_pesos_liquida
        END IF
        DISPLAY "lr_parcial_repo.mto_pagado", lr_parcial_repo.mto_pagado
        CALL f_datos_domicilio(lr_parcial_repo.nss) RETURNING reg_18.*
        
        CALL f_envia_op16 (p_folio_op13      ,
                           lr_parcial_repo.* ,
                           ld_total_rcv      ,
                           reg_18.*
                           )

        {OUTPUT TO REPORT rpt_detalle_op16(p_folio_op13      ,
                                          lr_parcial_repo.* ,
                                          ld_total_rcv      ,
                                          reg_18.*
                                          )}
    END FOREACH

END FUNCTION

#-----------------------------------------------------------------------#
# Genera el detalle de la op 16 para los retiros parciales por desempleo#
# con formatos nuevos (A y B) y Complementarios (C)                     #
#-----------------------------------------------------------------------#
FUNCTION f_op16_nuevos_des(p_folio_op16, p_fecha_liq)

    DEFINE
        p_folio_op16    INTEGER

    DEFINE
        p_fecha_liq     DATE
        
    DEFINE 
        li_tot_des      SMALLINT

    DEFINE lr_parcial RECORD #loc #lr_parcial
        folio_t_procesar      LIKE ret_parcial.folio_t_procesar  ,
        nss                   LIKE ret_parcial.nss                  ,
        curp                  LIKE ret_parcial.curp              ,
        consecutivo           LIKE ret_parcial.consecutivo          ,
        tipo_prestacion       LIKE ret_parcial.tipo_prestacion      ,
        cve_desempleo         LIKE tab_tipo_desempleo.cve_desempleo ,    
        num_mensualidad       LIKE ret_ctr_pago_det.num_mensualidad ,   
        mto_mensualidad       LIKE ret_ctr_pago_det.mto_mensualidad ,
        fecha_solicitud       LIKE ret_parcial.fecha_solicitud      ,
        fecha_liquida         LIKE ret_ctr_pago_det.fecha_liquida
    END RECORD

    DEFINE lr_afiliado RECORD #loc #lr_afiliado
        dom_calle            LIKE afi_domicilio.calle       ,
        dom_numero_ext       LIKE afi_domicilio.numero      ,
        dom_numero_int       LIKE afi_domicilio.depto       ,
        dom_colonia          LIKE afi_domicilio.colonia     ,
        deleg_desc           LIKE tab_delegacion.deleg_desc ,
        dom_codpos           LIKE afi_domicilio.codpos      ,
        estad_desc           LIKE tab_estado.estad_desc
    END RECORD

    DEFINE 
        v_siefore             SMALLINT

    DEFINE 
        ld_total_rcv          DECIMAL(10,2),
        ld_pesos_liquida      DECIMAL(10,2)

    DECLARE cur_5 CURSOR FOR
    SELECT  A.folio_t_procesar   ,
            A.nss                , 
            A.curp               , 
            A.consecutivo        ,
            A.tipo_prestacion    , 
            C.cve_desempleo      ,
            B.num_mensualidad    ,
            B.mto_mensualidad    , 
            A.fecha_solicitud    ,
            B.fecha_liquida
    FROM    ret_parcial        A , 
            ret_ctr_pago_det   B ,
            tab_tipo_desempleo C 
    WHERE   B.folio_op16       = p_folio_op16
    AND     B.nss              = A.nss
    AND     B.consecutivo      = A.consecutivo
    AND     A.estado_solicitud = gr_edo.liquidado
    AND     B.estado           = gr_edo.liquidado
    AND     B.fecha_liquida    = p_fecha_liq
    AND     A.diag_cuenta_ind  = 400
    AND     A.tipo_desempleo   IN ("A","B","C")
    AND     B.fecha_liquida    = p_fecha_liq
    AND     A.tipo_desempleo   = C.tipo_desempleo
    ORDER BY 2
    
    FOREACH cur_5 INTO lr_parcial.*
        
        LET ld_pesos_liquida = 0
        LET v_siefore        = f_obtiene_siefore(lr_parcial.nss)

        LET gr_cont.total = gr_cont.total + 1
        DISPLAY "TOTAL SOLICITUDES : ", gr_cont.total AT 16,5

        CASE lr_parcial.cve_desempleo
            WHEN 1
                LET gr_cont.tipo_a = gr_cont.tipo_a + 1
                LET li_tot_des     = gr_cont.tipo_a + gr_cont.tipo_b + gr_cont.tipo_c + gr_cont.tipo_d
                DISPLAY "DESEMPLEO         : ", li_tot_des AT 10,5
                DISPLAY "          TIPO A  : ", gr_cont.tipo_a AT 11,5
            WHEN 2
                LET gr_cont.tipo_b = gr_cont.tipo_b + 1
                LET li_tot_des     = gr_cont.tipo_a + gr_cont.tipo_b + gr_cont.tipo_c + gr_cont.tipo_d
                DISPLAY "DESEMPLEO         : ", li_tot_des AT 10,5
                DISPLAY "          TIPO B  : ", gr_cont.tipo_b AT 12,5
            WHEN 3
                LET gr_cont.tipo_c = gr_cont.tipo_c + 1
                LET li_tot_des     = gr_cont.tipo_a + gr_cont.tipo_b + gr_cont.tipo_c + gr_cont.tipo_d
                DISPLAY "DESEMPLEO         : ", li_tot_des AT 10,5
                DISPLAY " COMPLEMENTARIOS  : ", gr_cont.tipo_c AT 13,5
        END CASE

        -- Obtenemos el monto antes del retiro
        
        LET m_cadena = " "
        LET m_cadena = " SELECT SUM(NVL(monto_en_acciones,0)) * ",gar_precio_acc[v_siefore].precio_dia,"    \n ",
                       " FROM   dis_cuenta                                                                      \n ",
                       " WHERE  nss              =  '",lr_parcial.nss,"'                                        \n ",
                       " AND    siefore          =  ",v_siefore,"                                               \n ",
                       " AND    folio            <> ",p_folio_op16,"                                            \n ",
                       " AND    fecha_conversion <= '",p_fecha_liq,"'                                           \n ",
                       " AND    subcuenta IN (1,2,5,6,9)"
        
        LET m_cadena = m_cadena CLIPPED
        
        PREPARE pre_monto_ant_ret_nue1 FROM m_cadena
        EXECUTE pre_monto_ant_ret_nue1 INTO ld_total_rcv
        
        IF ld_total_rcv IS NULL THEN               
           IF m_band_tabla = 0 THEN
       	      LET m_cadena = " "
              LET m_cadena =" SELECT SUM(NVL(monto_en_acciones,0)) * ",gar_precio_acc[v_siefore].precio_dia,"    \n ",
                             " FROM   ",m_nom_tabla CLIPPED,"                                                     \n ",
                             " WHERE  nss              =  '",lr_parcial.nss,"'                                    \n ",
                             " AND    siefore          =  ",v_siefore,"                                           \n ",
                             " AND    folio            <> ",p_folio_op16,"                                        \n ",
                             " AND    fecha_conversion <= '",p_fecha_liq,"'                                       \n ",
                             " AND    subcuenta IN (1,2,5,6,9)"
                             
            
                         
              LET m_cadena = m_cadena CLIPPED
              
              PREPARE pre_monto_ant_ret_nue2 FROM m_cadena
              EXECUTE pre_monto_ant_ret_nue2 INTO ld_total_rcv
           END IF
           
        END IF
        
        IF ld_total_rcv IS NULL THEN
            LET ld_total_rcv = 0
        END IF
        
        -- Obtenemos el monto pagado al trabajador (Se cambia el signo ya que es un retiro)
        
        LET m_cadena = " "
        LET m_cadena = " SELECT SUM(NVL(monto_en_acciones * -1,0)) * ",gar_precio_acc[v_siefore].precio_dia,"   \n ",
                       " FROM   dis_cuenta                                                                          \n ",
                       " WHERE  nss        = '",lr_parcial.nss,"'                                                     \n ",
                       " AND    siefore    = ",v_siefore,"                                                          \n ",
                       " AND    folio      = ",p_folio_op16,"                                                       \n ",
                       " AND    subcuenta IN (1,2,5,6,9)"
        
        LET m_cadena = m_cadena CLIPPED
                       
        PREPARE pre_monto_pg_trab_nue1 FROM m_cadena
        EXECUTE pre_monto_pg_trab_nue1 INTO ld_pesos_liquida               
                       
        IF ld_pesos_liquida IS NULL THEN
           IF m_band_tabla = 0 THEN
       	      LET m_cadena = " "
              LET m_cadena = " SELECT SUM(NVL(monto_en_acciones * -1,0)) * ",gar_precio_acc[v_siefore].precio_dia,"   \n ",
                             " FROM   ",m_nom_tabla CLIPPED,"                                                             \n ",
                             " WHERE  nss        = '",lr_parcial.nss,"'                                                     \n ",
                             " AND    siefore    = ",v_siefore,"                                                          \n ",
                             " AND    folio      = ",p_folio_op16,"                                                       \n ",
                             " AND    subcuenta IN (1,2,5,6,9)"
                             
                          
              LET m_cadena = m_cadena CLIPPED
           
              PREPARE pre_monto_pg_trab_nue2 FROM m_cadena
              EXECUTE pre_monto_pg_trab_nue2 INTO ld_pesos_liquida
           END IF
        END IF
        
        IF ld_pesos_liquida IS NULL THEN
            LET ld_pesos_liquida = 0
        END IF
        
        -- Si el monto liquidado es igual a lo que se tenia en RCV entonces se
        -- reporta en la op 16 el monto liquidado.
        IF ld_pesos_liquida = ld_total_rcv THEN
            LET lr_parcial.mto_mensualidad = ld_pesos_liquida
        END IF
        
        CALL f_datos_domicilio(lr_parcial.nss) RETURNING lr_afiliado.* 
        
        CALL f_envia_op16 (p_folio_op16 ,
                                          lr_parcial.* ,
                                          ld_total_rcv ,
                                          lr_afiliado.*
                                         )
        {OUTPUT TO REPORT rpt_detalle_op16(p_folio_op16 ,
                                          lr_parcial.* ,
                                          ld_total_rcv ,
                                          lr_afiliado.*
                                         )}
    END FOREACH
   
END FUNCTION

#-----------------------------------------------------------------------#
# Determina si el folio dado corresponde a una liquidacion de retiros   #
# por desempleo con formato nuevo o con formatos anteriores y matrimonio#
#-----------------------------------------------------------------------#
FUNCTION f_determina_op(lr_datos)

    DEFINE lr_datos RECORD 
        folio       LIKE dis_cuenta.folio,
        fecha_liq   LIKE dis_cuenta.fecha_conversion
    END RECORD

    DEFINE 
        ls_flag     SMALLINT

    DEFINE lc_tipo_op LIKE ret_ctr_envio.tipo_operacion

    SELECT tipo_operacion
    INTO   lc_tipo_op
    FROM   ret_ctr_envio
    WHERE  folio        = lr_datos.folio
    #AND    fecha_envio  = lr_datos.fecha_liq
    
    IF lc_tipo_op = "AV16" THEN 
        -- El folio ingresado es una liquidacion de retiros anteriores
        LET ls_flag = 0
    ELSE
        -- El folio ingresado es una liquidacion de retiros nuevos
        LET ls_flag = 1
    END IF
    
    RETURN ls_flag

END FUNCTION

#-----------------------------------------------------------------------#
# Realiza las validaciones para la ejecucion correcta del programa      #
#-----------------------------------------------------------------------#
FUNCTION f_valida_ejecucion(pr_captura)

    DEFINE pr_captura RECORD
        folio       INTEGER,
        fecha       DATE
    END RECORD

    
    DEFINE
        ls_exit         SMALLINT

    DEFINE
        li_cont         INTEGER
        
    DEFINE ld_fecha_liquidacion DATE
    DEFINE ls_year              SMALLINT
    DEFINE lc_year              CHAR(4)

    LET li_cont = 0
    LET ls_exit = 0

    -- Validamos que existan registros
    
    LET m_nom_tabla = " "
    LET ls_year = YEAR(today) LET ls_year = ls_year -1  LET lc_year = ls_year
    LET m_nom_tabla = "dis_cuenta", lc_year[3,4]
    
    LET m_cadena = " "
    LET m_cadena = "SELECT tabname FROM SYSTABLES \n ",
                   " WHERE  tabname = '", m_nom_tabla CLIPPED, "'"
    LET m_cadena = m_cadena CLIPPED

    LET m_band_tabla = 1
    PREPARE pre_reg FROM m_cadena
    EXECUTE pre_reg
    
    IF SQLCA.SQLCODE = 0 THEN   #existe
       LET m_band_tabla = 0                     
    END IF
    
    --DISPLAY "m_nom_tabla 1", m_nom_tabla  
    SELECT COUNT(*)                               
    INTO   li_cont                                
    FROM   dis_cuenta                             
    WHERE  folio = pr_captura.folio               
    AND    tipo_movimiento IN (gr_movs.matrimonio,
                               gr_movs.tipo_d    ,
                               gr_movs.tipo_a    ,
                               gr_movs.tipo_b    ,
                               gr_movs.tipo_c    )
         
    IF li_cont = 0 THEN
       IF m_band_tabla = 0 THEN
          LET m_cadena = " "                                                                           
          LET m_cadena = "SELECT COUNT(*)                                    \n ",                     
                         " FROM   ", m_nom_tabla CLIPPED,                    "\n ",                     
                         " WHERE  folio =", pr_captura.folio,                "\n ",                     
                         " AND    tipo_movimiento IN (",gr_movs.matrimonio, ",\n ",                     
                         "                           ",gr_movs.tipo_d,     ",\n ",                     
                         "                           ",gr_movs.tipo_a,     ",\n ",                     
                         "                           ",gr_movs.tipo_b,     ",\n ",                     
                         "                           ",gr_movs.tipo_c,     ")\n "
                                                               
          LET m_cadena = m_cadena CLIPPED                                                              
          --DISPLAY "m_cadena 1", m_cadena                                                                                            
          PREPARE pre_val FROM m_cadena                                                                
          EXECUTE pre_val INTO li_cont                                               
          
          IF li_cont = 0 THEN
             PROMPT " NO EXISTEN REGISTROS PARA EL FOLIO CAPTURADO ...<ENTER> PARA SALIR "
             FOR CHAR enter
             LET ls_exit = 1
          END IF
       ELSE
   	      PROMPT " NO EXISTEN REGISTROS PARA EL FOLIO CAPTURADO ...<ENTER> PARA SALIR "
          FOR CHAR enter
          LET ls_exit = 1
       END IF                           
    END IF

    RETURN ls_exit

END FUNCTION

#-----------------------------------------------------------------------#
# Obtiene la siefore actual del trabajador para las subcuentas de RCV   #
#-----------------------------------------------------------------------#
FUNCTION f_obtiene_siefore(p_nss)

    DEFINE
        p_nss LIKE cta_nss_regimen.nss

    DEFINE
        ls_siefore  SMALLINT

    SELECT codigo_siefore
    INTO   ls_siefore
    FROM   cta_nss_regimen
    WHERE  grupo_regimen = 1
    AND    nss           = p_nss

    RETURN ls_siefore

END FUNCTION 

#-----------------------------------------------------------------------#
# Obtiene los datos de domicilio para el trabajador                     #
#-----------------------------------------------------------------------#
FUNCTION f_datos_domicilio(p_nss)

    DEFINE
        p_nss    LIKE afi_domicilio.nss

    DEFINE vn_folio           LIKE afi_mae_afiliado.n_folio
    DEFINE vtipo_solicitud    LIKE afi_mae_afiliado.tipo_solicitud
    DEFINE vfactualiza        DATE
    DEFINE vrowid             INTEGER
    DEFINE vcont_dom          INTEGER

    DEFINE lr_afi RECORD #loc #lr_afi
        dom_calle            LIKE afi_domicilio.calle       ,
        dom_numero_ext       LIKE afi_domicilio.numero      ,
        dom_numero_int       LIKE afi_domicilio.depto       ,
        dom_colonia          LIKE afi_domicilio.colonia     ,
        deleg_desc           LIKE tab_delegacion.deleg_desc ,
        dom_codpos           LIKE afi_domicilio.codpos      ,
        estad_desc           LIKE tab_estado.estad_desc
    END RECORD
    
    SELECT A.n_folio, 
           A.tipo_solicitud
    INTO   vn_folio,
           vtipo_solicitud
    FROM   afi_mae_afiliado A
    WHERE  A.n_seguro = p_nss

    SELECT MAX(factualiza)
    INTO   vfactualiza
    FROM   afi_domicilio
    WHERE  n_folio        = vn_folio
    AND    tipo_solicitud = vtipo_solicitud
    AND    nss            = p_nss
    AND    marca_envio    = "X"
    
    SELECT MAX(ROWID)
    INTO   vrowid
    FROM   afi_domicilio
    WHERE  n_folio        = vn_folio
    AND    tipo_solicitud = vtipo_solicitud
    AND    nss            = p_nss
    AND    marca_envio    = "X"
    AND    factualiza     = vfactualiza

    SELECT A.calle      ,
           A.numero     ,
           A.depto      ,
           A.colonia    ,
           B.deleg_desc ,
           A.codpos     ,
           C.estad_desc
    INTO   lr_afi.* 
    FROM   afi_domicilio A, 
    OUTER  tab_delegacion B, 
    OUTER  tab_estado C
    WHERE  A.nss            = p_nss
    AND    A.n_folio        = vn_folio
    AND    A.tipo_solicitud = vtipo_solicitud
    AND    A.delega         = B.deleg_cod
    AND    A.estado         = C.estad_cod
    AND    A.marca_envio    = "X"
    AND    A.factualiza     = vfactualiza
    AND    A.rowid          = vrowid
    GROUP BY 1,2,3,4,5,6,7

    RETURN lr_afi.*

END FUNCTION

#-----------------------------------------------------------------------#
# Obtiene los precios de accion para la fecha indicada                  #
#-----------------------------------------------------------------------#
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
        li_cont               SMALLINT

    LET li_cont = 0

    LET v_precios_accion = " EXECUTE FUNCTION fn_verifica_precio_accion (?)"
    PREPARE eje_precios_accion FROM v_precios_accion

    DECLARE c_precios CURSOR FOR eje_precios_accion
    FOREACH c_precios USING fecha_precios
                      INTO lr_precio_acc.*

        -- Solo se valida que existan los precios de las siefores basicas (1 a 5)
        IF (lr_precio_acc.siefore < 10 AND lr_precio_acc.estado <> 0) THEN
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

#------------------------------------------------------------------------#
# Inserta los registros enviados en tabla de control ret_parcial_op16    #
#------------------------------------------------------------------------#
FUNCTION f_inserta_parcialOp16 (lr_capturaOp16)
    
   DEFINE
      lr_capturaOp16       RECORD LIKE   ret_parcial_op16.*
   DEFINE lc_error         CHAR(1000)
   
   # Insertando la solicitud de reinversión
   WHENEVER ERROR CONTINUE
   SET LOCK MODE TO WAIT 2;   # 2 segundos de espera
   LOCK TABLE ret_parcial_op16 IN EXCLUSIVE MODE;
   
   INSERT INTO safre_af:ret_parcial_op16 VALUES(lr_capturaOp16.*)
   IF SQLCA.SQLERRD[3] = 0 THEN
      LET lc_error = "NO SE INSERTÓ EL REGISTRO DE ENVIO, FOLIO PROCESAR: ", lr_capturaOp16.folio_t_procesar CLIPPED," NSS : ", lr_capturaOp16.nss CLIPPED
      CALL ERRORLOG(lc_error CLIPPED)  
      SET LOCK MODE TO NOT WAIT;
      UNLOCK TABLE ret_parcial_op16;
      WHENEVER ERROR STOP
      RETURN
   END IF
   SET LOCK MODE TO NOT WAIT;
   UNLOCK TABLE ret_parcial_op16;
   WHENEVER ERROR STOP

END FUNCTION

#-----------------------------------------------------------------------#
# Se envia la operacion 16                                              #
#-----------------------------------------------------------------------#

FUNCTION f_envia_op16(p_folio_liq, p_parcial, p_suma_rcv, pr_afiliado)
#ld---------------------------------------------
    DEFINE
        p_folio_liq        INTEGER
    
    DEFINE
        lr_capturaOp16     RECORD LIKE   ret_parcial_op16.*
        
    DEFINE 
        lr_sieforeOp16  RECORD LIKE ret_bus_sie_op16.*

    DEFINE p_parcial RECORD #loc #p_parcial
        folio_t_procesar      LIKE ret_parcial.folio_t_procesar  ,
        nss                   LIKE ret_parcial.nss                  ,
        curp                  LIKE ret_parcial.curp              ,
        consecutivo           LIKE ret_parcial.consecutivo          ,
        tipo_prestacion       LIKE ret_parcial.tipo_prestacion      ,
        cve_desempleo         LIKE tab_tipo_desempleo.cve_desempleo ,    
        num_mensualidad       LIKE ret_ctr_pago_det.num_mensualidad ,   
        mto_pagado            LIKE ret_ctr_pago_det.mto_mensualidad ,
        fecha_solicitud       LIKE ret_parcial.fecha_solicitud      ,
        fecha_liquida         LIKE ret_ctr_pago_det.fecha_liquida
    END RECORD

    DEFINE 
        p_suma_rcv         DECIMAL(10,2)

    DEFINE pr_afiliado RECORD #loc #pr_afiliado
        dom_calle            LIKE afi_domicilio.calle       ,
        dom_numero_ext       LIKE afi_domicilio.numero      ,
        dom_numero_int       LIKE afi_domicilio.depto       ,
        dom_colonia          LIKE afi_domicilio.colonia     ,
        deleg_desc           LIKE tab_delegacion.deleg_desc ,
        dom_codpos           LIKE afi_domicilio.codpos      ,
        estad_desc           LIKE tab_estado.estad_desc
    END RECORD

    DEFINE lr_mtos_sie RECORD #loc #lr_mtos_sie
        siefore               SMALLINT      ,
        acciones_ret97        DECIMAL(16,6) ,
        acciones_cv           DECIMAL(16,6) ,
        acciones_cs           DECIMAL(16,6) ,
        acciones_mix          DECIMAL(16,6) 
    END RECORD

    DEFINE #loc #char
        lc_id_fin_pago        CHAR(01) ,
        c40_paterno           CHAR(40) ,
        c40_materno           CHAR(40) ,
        c40_nombres           CHAR(40) ,
        c15_acc_ret97         CHAR(15) ,
        c14_acc_ret97         CHAR(14) ,
        c15_acc_cv            CHAR(15) ,
        c14_acc_cv            CHAR(14) ,
        c15_acc_cs            CHAR(15) ,
        c14_acc_cs            CHAR(14) ,
        c10_impt_pagado       CHAR(10) ,
        c11_impt_pagado       CHAR(11) ,
        c10_impt_cta_ind      CHAR(10) ,
        c11_impt_cta_ind      CHAR(11) ,
        l_sieforeArreglo        CHAR(1000)

    DEFINE #loc #smallint  
        ls_edosol             ,
        ls_sie                ,
        ls_sie_rep            SMALLINT

    DEFINE #loc #decimal
        ld_imp_tot_oper       DECIMAL(13,2)

    DEFINE ls_tipo_pago LIKE ret_parcial.tipo_pago
    
    LET ls_sie                      = 0
    LET ls_sie_rep                  = 0
    LET lr_mtos_sie.siefore         = 0
    LET lr_mtos_sie.acciones_ret97  = 0
    LET lr_mtos_sie.acciones_cv     = 0
    LET lr_mtos_sie.acciones_cs     = 0
    LET lr_mtos_sie.acciones_mix    = 0
    
    LET lc_id_fin_pago = ""
    LET c40_paterno = ""
    LET c40_materno = ""
    LET c40_nombres = ""
    
    INITIALIZE gr_enviaOp16.*, lr_capturaOp16.* TO NULL
    
    SELECT paterno     ,
           materno     ,
           nombres
    INTO   c40_paterno ,
           c40_materno ,
           c40_nombres
    FROM   afi_mae_afiliado
    WHERE  n_seguro = p_parcial.nss
    GROUP BY 1,2,3
    
    LET ls_sie = f_obtiene_siefore(p_parcial.nss)
    
    #----- Importe pagado al trabajador ----
    #LET c11_impt_pagado = p_parcial.mto_pagado USING "&&&&&&&&.&&"

    ----- Acumulador del total de montos para la operacion ----
    LET ld_imp_tot_oper = ld_imp_tot_oper + p_parcial.mto_pagado
    
    #----- Saldo total de la cuenta antes del retiro ----
    #LET c11_impt_cta_ind = p_suma_rcv USING "&&&&&&&&.&&"

    
    IF p_parcial.tipo_prestacion = gr_prest.desempleo THEN

        ----- Numero de mensualidad pagada (Solo retiros B) ----
        {IF p_parcial.cve_desempleo <> 2 THEN
            LET p_parcial.num_mensualidad = 0
        END IF}

                -- Comentario por Javier Gonzalez
                -- Cambio solicitado por procesar para todos los tipos de desempleo
                LET p_parcial.num_mensualidad = 1

        ----- Determina indicador de fin de pago ----
        IF p_parcial.cve_desempleo <> 4 THEN
            SELECT estado
            INTO   ls_edosol
            FROM   ret_ctr_pago
            WHERE  nss         = p_parcial.nss
            AND    consecutivo = p_parcial.consecutivo
            
            IF ls_edosol = gr_edo.liquidado THEN
                LET lc_id_fin_pago = "0"
            ELSE
                LET lc_id_fin_pago = "1"
            END IF
        ELSE
            LET lc_id_fin_pago = "0"
        END IF

        ----- Si un retiro B tiene modalidad de pago Retiro A, se reporta como 'A' ----
        IF p_parcial.cve_desempleo = 2 THEN
            SELECT tipo_pago
            INTO   ls_tipo_pago
            FROM   ret_parcial
            WHERE  nss            = p_parcial.nss        
            AND    consecutivo    = p_parcial.consecutivo
        
            IF ls_tipo_pago = 1 THEN 
                LET p_parcial.cve_desempleo   = 1
                --LET p_parcial.num_mensualidad = 0
            END IF
        END IF
    ELSE
        -- Si la prestacion es matrimonio, se mandan en blanco
        LET p_parcial.num_mensualidad = NULL
        LET lc_id_fin_pago            = NULL
        LET p_parcial.cve_desempleo   = NULL
    END IF
    
    ----- Detalle de pagos por siefore y subcuentas ----

    LET c14_acc_ret97 = "00000000000000"
    LET c14_acc_cv    = "00000000000000"
    LET c14_acc_cs    = "00000000000000"
    
    LET l_sieforeArreglo = ""

    DECLARE cur_8 CURSOR FOR
    SELECT A.siefore        ,
           A.acciones_ret97 ,
           A.acciones_cv    ,
           A.acciones_cs    ,
           "0"
    FROM   ret_monto_siefore A
    WHERE  A.nss            = p_parcial.nss
    AND    A.consecutivo    = p_parcial.consecutivo
    AND    A.tipo_retiro    = "I"
    AND    A.tipo_operacion = 16 
    AND    A.folio          = p_folio_liq
    
   --DISPLAY "p_parcial.nss ", p_parcial.nss
   --DISPLAY "p_parcial.consecutivo ", p_parcial.consecutivo
   --DISPLAY "p_folio_liq ", p_folio_liq
    
    FOREACH cur_8 INTO lr_mtos_sie.*
    --	DISPLAY "lr_mtos_sie.* ", lr_mtos_sie.*
        
        
            
            LET lr_sieforeOp16.folio_procesar      = p_parcial.folio_t_procesar
            LET lr_sieforeOp16.folio               = p_folio_liq
            LET lr_sieforeOp16.nss                 = p_parcial.nss
            LET lr_sieforeOp16.siefore             = lr_mtos_sie.siefore
            LET lr_sieforeOp16.acciones_ret97      = lr_mtos_sie.acciones_ret97
            LET lr_sieforeOp16.acciones_cv         = lr_mtos_sie.acciones_cv
            LET lr_sieforeOp16.acciones_cs         = lr_mtos_sie.acciones_cs
            LET lr_sieforeOp16.acciones_sub_mixta  = 0
            
            WHENEVER ERROR CONTINUE
               INSERT INTO safre_af:ret_bus_sie_op16 VALUES(lr_sieforeOp16.*)
                 IF SQLCA.SQLERRD[3] != 1 THEN
                    CALL ERRORLOG( "ERROR AL INSERTAR EN ret_bus_sie_op16 " || p_parcial.nss || "- Siefore:" ||lr_mtos_sie.siefore)
                    LET g_error = 1
                 END IF
                 
            WHENEVER ERROR STOP
            
        
    END FOREACH           
    --DISPLAY "p_parcial.mto_pagado ", p_parcial.mto_pagado
    LET gr_enviaOp16.folio_procesar                   = p_parcial.folio_t_procesar
    LET gr_enviaOp16.folio                            = p_folio_liq
    LET gr_enviaOp16.nss                              = p_parcial.nss
    LET gr_enviaOp16.curp                             = p_parcial.curp
    LET gr_enviaOp16.nombreTrabajador                 = c40_nombres
    LET gr_enviaOp16.apellidoPaterno                  = c40_paterno
    LET gr_enviaOp16.apellidoMaterno                  = c40_materno
    LET gr_enviaOp16.tipoDeRetiro                     = "I"
    LET gr_enviaOp16.tipoDePrestacion                 = p_parcial.tipo_prestacion USING "&&"
    LET gr_enviaOp16.fechaDeSolicitudTrabajador       = p_parcial.fecha_solicitud
    LET gr_enviaOp16.consecutivoTrabajador            = p_parcial.consecutivo
    LET gr_enviaOp16.importePagadoTrabajador          = p_parcial.mto_pagado USING "&&&&&&&&.&&"
    LET gr_enviaOp16.saldoCuentaIndividualAntesRetiro = p_suma_rcv USING "&&&&&&&&.&&"
    LET gr_enviaOp16.fechaValuacionAcciones           = p_parcial.fecha_liquida
    LET gr_enviaOp16.calleNumeroExtInt                = pr_afiliado.dom_calle
    LET gr_enviaOp16.numeroExterior                   = pr_afiliado.dom_numero_ext
    LET gr_enviaOp16.numeroInterior                   = pr_afiliado.dom_numero_int
    LET gr_enviaOp16.colonia                          = pr_afiliado.dom_colonia
    LET gr_enviaOp16.delegacionMunicipio              = pr_afiliado.deleg_desc
    LET gr_enviaOp16.codigoPostal                     = pr_afiliado.dom_codpos
    LET gr_enviaOp16.entidadFederativa                = pr_afiliado.estad_desc
    LET gr_enviaOp16.tipoRetiroDesempleo              = p_parcial.cve_desempleo
    LET gr_enviaOp16.numeroPago                       = p_parcial.num_mensualidad
    LET gr_enviaOp16.notificacionConclusionPagos      = lc_id_fin_pago
    
         WHENEVER ERROR CONTINUE
            INSERT INTO safre_af:ret_bus_diag16 VALUES(gr_enviaOp16.*)
              IF SQLCA.SQLERRD[3] = 1 THEN
                 LET g_cuantos = g_cuantos + 1
                 DISPLAY "         ENVIADAS : ", g_cuantos AT 17,5
                 
                 LET lr_capturaOp16.id_parcial_op16 = 0
                 LET lr_capturaOp16.folio_t_procesar = p_parcial.folio_t_procesar CLIPPED
                 LET lr_capturaOp16.nss = p_parcial.nss CLIPPED
                 LET lr_capturaOp16.curp = p_parcial.curp CLIPPED
                 LET lr_capturaOp16.consecutivo = p_parcial.consecutivo
                 LET lr_capturaOp16.folio = p_folio_liq
                 LET lr_capturaOp16.fecha_envio = HOY
                 
                 CALL f_inserta_parcialOp16(lr_capturaOp16.*)
                 
              ELSE
                 CALL ERRORLOG( "ERROR AL INSERTAR EN ret_bus_diag16 " || p_parcial.nss)
                 LET g_error = 1
                                 
              END IF
              
         WHENEVER ERROR STOP
   
   INITIALIZE gr_enviaOp16.*, p_parcial.*, p_suma_rcv, lr_mtos_sie.*, pr_afiliado.*, lr_capturaOp16.* TO NULL

END FUNCTION


