#################################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                               #
#Owner             => E.F.P.                                                    #
#Programa RETM809a => VARIABLES GLOBALES DEL MODULO DE ADMINISTRACION DE        #
#                     SOLICITUDES DE RETIROS PARCIALES (RETM809.4gl)            #
#Fecha creacion    => 04 DE MAYO DE 2010                                        #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Sistema           => RET                                                       #
#################################################################################
DATABASE safre_af

GLOBALS

    #---------------------------------------------------------------------------#
    #                                ARREGLOS                                   #
    #---------------------------------------------------------------------------#

    DEFINE gar_precio_acc ARRAY [20] OF RECORD  -- Arreglo para los precios_accion
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE gar_cons ARRAY[1000] OF RECORD
        nss                     LIKE ret_parcial.nss,
        rfc                     CHAR(13),
        curp                    CHAR(18),
        paterno                 CHAR(40),
        materno                 CHAR(40),
        nombres                 CHAR(40),
        folio_solicitud         LIKE ret_parcial.folio_solicitud,
        fecha_solicitud         LIKE ret_parcial.fecha_solicitud,
        num_resolucion          LIKE ret_parcial.num_resolucion,
        tipo_retiro             LIKE ret_parcial.tipo_retiro,
        desc_tipo_ret           CHAR(31),
        tipo_prestacion         LIKE ret_parcial.tipo_prestacion,
        desc_tipo_prest         CHAR(31),
        tipo_desempleo          LIKE ret_parcial.tipo_desempleo,
        desc_tipo_desempleo     CHAR(15) ,
        tipo_pago               LIKE ret_parcial.tipo_pago ,
        desc_tipo_pago          CHAR(15) ,
        salario_base_cot        LIKE ret_parcial.salario_base_cot,
        pago_desempleo          LIKE ret_parcial.pago_desempleo,
        fecha_resolucion        LIKE ret_parcial.fecha_resolucion,
        fecha_fall_mat_des      LIKE ret_parcial.fecha_fall_mat_des,
        impt_autorizado         LIKE ret_parcial.impt_autorizado,
        rechazo_cod             LIKE ret_parcial.rechazo_cod,
        desc_cod_rech           CHAR(50),
        cod_rechazo_ent         LIKE ret_parcial.cod_rechazo_ent,
        fecha_captura           LIKE ret_parcial.fecha_captura,
        fecha_modifica          LIKE ret_parcial.fecha_modifica,
        fecha_confirma          LIKE ret_parcial.fecha_confirma,
        fecha_pago              LIKE ret_parcial_tx.fecha_pago,
        folio                   LIKE ret_parcial.folio,
        consecutivo             LIKE ret_parcial.consecutivo,
        usuario_captura         LIKE ret_parcial.usuario_captura,
        usuario_modifica        LIKE ret_parcial.usuario_modifica,
        usuario_confirma        LIKE ret_parcial.usuario_confirma,
        estado_solicitud        LIKE ret_parcial.estado_solicitud,
        estado_sol_desc         CHAR(40),
        fecha_envio             LIKE ret_parcial.fecha_envio,
        diag_cuenta_ind         CHAR(03)
    END RECORD

    #---------------------------------------------------------------------------#
    #                                REGISTROS                                  #
    #---------------------------------------------------------------------------#

    DEFINE g_reg RECORD
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
        fecha_envio             LIKE ret_parcial.fecha_envio        ,
        diag_cuenta_ind         CHAR(03)
    END RECORD

    DEFINE gr_edo RECORD
        precapturado            LIKE ret_estado.estado_solicitud  ,
        capturado               LIKE ret_estado.estado_solicitud  ,
        rechazado               LIKE ret_estado.estado_solicitud  ,
        confirmado              LIKE ret_estado.estado_solicitud  ,
        liquidado               LIKE ret_estado.estado_solicitud  
    END RECORD

    DEFINE gr_prest RECORD
        desempleo               LIKE ret_parcial.tipo_prestacion ,
        matrimonio              LIKE ret_parcial.tipo_prestacion
    END RECORD

    DEFINE gr_movs RECORD
        matrimonio      SMALLINT,
        tipo_a          SMALLINT,
        tipo_b          SMALLINT,
        tipo_c          SMALLINT,
        tipo_d          SMALLINT
    END RECORD

    #---------------------------------------------------------------------------#
    #                           VARIABLES GLOBALES                              #
    #---------------------------------------------------------------------------#
    DEFINE 
        gc_nss                  LIKE ret_parcial.nss                ,
        gs_cod_cpl              LIKE tab_afore_local.codigo_afore   ,
        gs_cod_sct              LIKE tab_afore_local.codigo_afore   ,
        gd_sal_minimo           LIKE tabsalario_minimo2.monto_sm    ,
        gd_limite_pago_a        LIKE ret_ctr_pago.mto_pago          ,
        gd_limite_pago_b        LIKE ret_ctr_pago.mto_pago          ,
        gd_sal_base_cot         LIKE ret_parcial.salario_base_cot   ,
        gi_tipo_desempleo       LIKE ret_parcial.tipo_desempleo

    -- -----------------------------------------------------------------------------

    DEFINE
        gs_edo_invalido         , -- Estado de solicitud artificial para almacenar si no existe el estado
        gs_captura              ,
        gs_rechazo              , -- Banderas globales de permisos en el programa
        gs_num_dias             , -- Dias habiles permitidos de vigencia
        gs_dias_nat_des         , -- Dias naturales de vigencia de fecha de resolucion para desempleo
        gs_dias_nat_mat         , -- Dias naturales de vigencia de fecha de resolucion para matrimonio
        gs_codigo_afore         ,
        gs_del_confirm          ,
        gs_recap_sbc            ,
        gs_recap_tipo_des       ,
        gs_dias_sbc             , -- Indica el numero de dias a pagar del SBC
        gs_dias_sm_mat          , -- Numero de dias de salario minimo para matrimonio
        codigo                  ,
        pos                     ,
        v1ra_vez                SMALLINT

    -- -----------------------------------------------------------------------------    
    
    DEFINE
        gd_porc_pago            DECIMAL(10,5), -- Indica el porcentaje a pagar de RCV
        vsaldo_mat              DECIMAL(16,6),
        vmonto_sm               DECIMAL(10,2),
        v75dias                 DECIMAL(16,2),
        v10por                  DECIMAL(16,2),
        impt_actual             DECIMAL(16,6),
        vimpt_ret_97            DECIMAL(16,2),
        vimpt_ces_vej           DECIMAL(16,2),
        vimpt_cuo_soc           DECIMAL(16,2),
        vimpt_tot_sub_rcv       DECIMAL(16,2) 

    -- -----------------------------------------------------------------------------
    
    DEFINE
        i                       ,
        arr_c                   ,
        vdias_naturales         INTEGER

    -- -----------------------------------------------------------------------------

    DEFINE 
        gc_usuario              CHAR(0020)  ,
        gc_error                CHAR(0400)  ,
        opc                     CHAR(0001)  ,
        xaccion                 CHAR(0001)  ,
        enter                   CHAR(0001)  ,
        cla_whe                 CHAR(1200)  ,
        cla_sel                 CHAR(1800)  ,
        ano                     CHAR(0008)  ,
        vreversa_marca          CHAR(0100)  ,
        comando                 CHAR(0001)  ,
        v_ejecuta               CHAR(0300)  ,
        x_usuario               CHAR(0012)  ,
        x_estado_solicitud      CHAR(0040)  ,
        desc_rechazo_cod        CHAR(0030) 
    
    -- -----------------------------------------------------------------------------
    
    DEFINE
        HOY                     ,
        gdt_lim_vigencia        ,
        vfecha_causa            DATE

    -- -----------------------------------------------------------------------------

END GLOBALS
