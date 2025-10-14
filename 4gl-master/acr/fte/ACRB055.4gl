######################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                   #
#Propietario       => EFP                                            #
#Programa          => ACRB055                                        #
#Descripcion       => GENERAR HISTORICO DE DEVOLUCIONES DE SALDOS POR#
#                     ANUALIDADES GARANTIZADAS                       #
#Sistema           => ACR                                            #
#Fecha creacion    => 12 ENERO 2010                                  #
#Por               => STEFANIE DANIELA VERA PIÑA                     #
######################################################################
DATABASE safre_af

GLOBALS

    DEFINE
        g_param_taa    RECORD LIKE seg_modulo.* ,
        w_codigo_afore LIKE tab_afore_local.codigo_afore 

    DEFINE reg_cza RECORD LIKE acr_cza_dev_ag.*
    DEFINE reg_det RECORD LIKE acr_det_dev_ag.*
    DEFINE reg_sum RECORD LIKE acr_sum_dev_ag.*

    DEFINE reg_cza_dev_sal_ag RECORD LIKE safre_tmp:cza_dev_sal_ag.*
    DEFINE reg_det_dev_sal_ag RECORD LIKE safre_tmp:det_dev_sal_ag.*
    DEFINE reg_sum_dev_sal_ag RECORD LIKE safre_tmp:sum_dev_sal_ag.*

    DEFINE reg_pagos RECORD
        folio             INTEGER,
        tipo_registro     CHAR(2),
        ident_servicio    CHAR(2),
        ident_pago        CHAR(16),
        importe           CHAR(15),
        fecha_liquidacion DATE,
        impt_aport_acept  CHAR(15),
        impt_inter_acept  CHAR(15),
        estado            CHAR(1),
        fecha_archivo     DATE
    END RECORD

    DEFINE r_prin RECORD 
        folio             INTEGER,
        fecha_recepcion   DATE,
        hora_recepcion    CHAR(8),
        estado            SMALLINT,
        fecha_estado      DATE,
        hora_estado       CHAR(08)
    END RECORD

    DEFINE 
        HOY               ,
        d_fecha           ,
        vfecha_aux        DATE
 
    DEFINE
        enter    		     CHAR(1),
        generar           CHAR(12),
        ejecuta           CHAR(30),
        c_fecha           CHAR(10),
        c_tipo_registro   CHAR(2),
        c_fech_creac_lote CHAR(10),
        nom_arch_generado CHAR(21),
        vfecha_archivo    CHAR(10),
        opc               CHAR(1),
        vusuario          CHAR(8)

    DEFINE 
        vfolio            ,
        vcont_apo         ,
        vcont_int         INTEGER

END GLOBALS


MAIN

    OPTIONS INPUT WRAP,
    PROMPT LINE LAST  ,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT

    CALL inicio()
    CALL primera_pantalla()

END MAIN


FUNCTION primera_pantalla()
#--------------------------

    OPEN WINDOW v1 AT 4,4 WITH 20 rows, 76 COLUMNS ATTRIBUTE(BORDER)
    DISPLAY " ACRB055        CARGA REGISTROS DE DEVOLUCION DE SALDOS AG                     " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                             < Ctrl-C > Salir                                   " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

    PROMPT "DESEA EJECUTAR EL PROCESO ? [S/N]..." FOR opc
        IF opc MATCHES "[Ss]" THEN
   
            CALL proceso_principal() 
        ELSE
            ERROR " PROCESO CANCELADO " SLEEP 3
            EXIT PROGRAM  
        END IF

    PROMPT "PROCESO FINALIZADO          " FOR enter

END FUNCTION


FUNCTION inicio()
#----------------

    INITIALIZE reg_cza.*  TO NULL
    INITIALIZE reg_det.*  TO NULL
    INITIALIZE reg_sum.*  TO NULL

    SELECT codigo_afore, user
    INTO   w_codigo_afore, vusuario
    FROM   tab_afore_local

    SELECT *
    INTO   g_param_taa.*
    FROM   seg_modulo
    WHERE  modulo_cod = 'acr'
   
    LET HOY = TODAY

END FUNCTION


FUNCTION proceso_principal()
#---------------------------

    DEFINE 
        vfecha              DATE,
        vfecha_ban          DATE,
        cfecha              CHAR(10),
        cfecha_ban          CHAR(10),
        vhora               CHAR(8),
        cont_reg            ,
        total_reg           INTEGER
 
    LET vhora = TIME
    LET r_prin.folio           = vfolio
    LET r_prin.fecha_recepcion = HOY
    LET r_prin.hora_recepcion  = TIME
    LET r_prin.estado          = 1       
    LET r_prin.fecha_estado    = HOY
    LET r_prin.hora_estado     = TIME    

    INSERT INTO glo_folio VALUES (0)  #folio

    SELECT MAX(folio)               
    INTO vfolio             
    FROM glo_folio                

    INSERT INTO taa_folio VALUES(vfolio,14,hoy,vusuario)

    DISPLAY "F O L I O              : ",vfolio USING "#######"   AT 09,05
    SLEEP 3

    SELECT *
    INTO   reg_cza_dev_sal_ag.*
    FROM   safre_tmp:cza_dev_sal_ag

    INSERT INTO acr_cza_dev_ag 
	 VALUES (vfolio, reg_cza_dev_sal_ag.*,1)

    LET vcont_apo = 00

    DECLARE cur_3 CURSOR FOR 
    SELECT * 
    FROM   safre_tmp:det_dev_sal_ag

    FOREACH cur_3 INTO reg_det_dev_sal_ag.*

        LET cont_reg = cont_reg + 1

        IF reg_det_dev_sal_ag.tipo_registro= "02" THEN

            LET vcont_apo = vcont_apo + 1

            DISPLAY "REGS. CON DEVOLUCION DE SALDOS :",vcont_apo USING "#######" AT 11,5

            INSERT INTO acr_det_dev_ag VALUES(vfolio, reg_det_dev_sal_ag.*)
        END IF

    END FOREACH

    SELECT SUM(NVL(sum_aivs_v97,0) +
               NVL(sum_aivs_v92,0))
    INTO reg_pagos.impt_aport_acept
    FROM safre_tmp:sum_dev_sal_ag

    LET reg_pagos.folio              = vfolio
    LET reg_pagos.tipo_registro      = "02"
    LET reg_pagos.ident_servicio     = reg_cza_dev_sal_ag.ident_servicio
    LET reg_pagos.ident_pago         = "             58" 
    LET reg_pagos.fecha_liquidacion  = reg_det_dev_sal_ag.fecha_movimiento 
    LET reg_pagos.estado             = 1 
    LET reg_pagos.fecha_archivo      = reg_cza_dev_sal_ag.fecha_presentacion

    LET reg_pagos.importe            = reg_pagos.impt_aport_acept +
                                       reg_pagos.impt_inter_acept

    INSERT INTO acr_devol_ag
	 VALUES(reg_pagos.folio,
           reg_pagos.tipo_registro,
           reg_pagos.ident_servicio,
           reg_pagos.ident_pago,
           reg_pagos.importe,
           reg_pagos.fecha_liquidacion,
           reg_pagos.impt_aport_acept,
           reg_pagos.impt_inter_acept,
           reg_pagos.estado,
           reg_pagos.fecha_archivo,
           HOY)

    SELECT *
    INTO   reg_sum_dev_sal_ag.*
    FROM   safre_tmp:sum_dev_sal_ag

    INSERT INTO acr_sum_dev_ag VALUES(vfolio, reg_sum_dev_sal_ag.*,1)
              
END FUNCTION
