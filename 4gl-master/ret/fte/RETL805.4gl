#################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )		                        #
#Propietario       => E.F.P.        				                            #
#Programa RETL805  => Reporte de Liquidacion de disposicion de Recursos         #
#Fecha             => 23 DE FEBRERO DEL 2004.    		                        #             
#Por               => MAURO MUNIZ CABALLERO / CLAUDIA URUZUA ROSAS              #
#Actualizo         => JUAN CARLOS MENDOZA MORENO                                #
#Fecha Actualiza   => 29 DE JUNIO DE 2004                                       #
# Actualizacion    => JAVIER GONZALEZ JERONIMO                                  #
# Fecha            => 22 DE FEBRERO DE 2012                                     #
#                     Modificaciones relacionadas a los cambios de la ley del   #
#                     INFONAVIT (Req. EFPS-187)                                 #
#Sistema           => RET  					                                    #
#################################################################################
DATABASE safre_af
GLOBALS
    DEFINE w_codigo_afore LIKE tab_afore_local.codigo_afore 
    DEFINE g_param_dis    RECORD LIKE seg_modulo.*
    DEFINE HOY	     DATE
    DEFINE aux_pausa CHAR(1)
    DEFINE enter     CHAR(1)
    DEFINE g_usuario CHAR(8)
    DEFINE hora      CHAR(8)
    DEFINE G_LISTA   CHAR(300)
    DEFINE G_IMPRE   CHAR(300)
    DEFINE impresion CHAR(300)
    DEFINE gdt_cambio_infonavit DATE

    DEFINE v RECORD
        fecha_corte DATE
    END RECORD

    DEFINE l_record RECORD
        tipo_retiro       LIKE ret_solicitud_rx.tipo_retiro,
        nss               LIKE dis_cuenta.nss,
        subcuenta         LIKE dis_cuenta.subcuenta,
        folio             LIKE dis_cuenta.folio,
        consecutivo_lote  LIKE dis_cuenta.consecutivo_lote,
        monto_en_pesos    LIKE dis_cuenta.monto_en_pesos,
        monto_en_acciones LIKE dis_cuenta.monto_en_acciones
    END RECORD

    DEFINE vparametros RECORD
        vfolio INTEGER,
        vfecha DATE
    END RECORD

    DEFINE v_folio_interno   LIKE dis_provision.folio
    DEFINE folio_rx          SMALLINT
    DEFINE ind               SMALLINT 
    DEFINE sw10              SMALLINT 
    DEFINE fec_rx            DATE
    DEFINE maximo_folio      INTEGER 
    DEFINE select_stmt5      CHAR(2000)
    DEFINE ctos_regs         INTEGER 
    DEFINE f_opera           DATE 
    DEFINE f_trans           DATE 

    DEFINE where_clause      CHAR(250)

END GLOBALS


MAIN
    DEFER INTERRUPT
    OPTIONS PROMPT LINE LAST,
    INPUT WRAP,
    ACCEPT KEY control-o

    CALL inicio()     #i
    CALL Consulta()   #c

    PROMPT "  PROCESO FINALIZADO...  <ENTER>  PARA SALIR " FOR CHAR enter
END MAIN


FUNCTION inicio()
#i---------------
    
    LET gdt_cambio_infonavit    = "01/12/2012"    
    
    SELECT codigo_afore,USER
    INTO   w_codigo_afore,g_usuario
    FROM   tab_afore_local

    SELECT ruta_listados
    INTO   g_param_dis.ruta_listados
    FROM   seg_modulo 
    WHERE  modulo_cod='ret'

    LET HOY = TODAY
    LET v_folio_interno = 0 
    CALL startlog("RETL805.log")

    LET ind = 0 
END FUNCTION


#---------------------------------------------------------------------------
# FUNCTION QUE ABRE LA PANTALLA DE CAPTURA
#---------------------------------------------------------------------------
FUNCTION Consulta()
#c-----------------
    DEFINE verifica     CHAR(70)

    DEFINE cuenta_regis INTEGER
    DEFINE l_fecha      DATE   
    
    DEFINE folio LIKE dis_cuenta.folio
    DEFINE fecha_conversion LIKE dis_cuenta.fecha_conversion

    CLEAR SCREEN

    OPEN WINDOW ventana_21 AT 10,4 WITH FORM "RETL8051" ATTRIBUTE( BORDER)
    DISPLAY "RETL805                     (Ctrl-C) Salir             (ESC) Ejecutar         " AT 1,1 ATTRIBUTE(REVERSE,green)

    LET int_flag = FALSE

    CONSTRUCT BY NAME where_clause ON folio,
                                      fecha_conversion 

        ON KEY(ESC)
           LET folio_rx = get_fldbuf(ret_solicitud_rx.folio)
           LET fec_rx   = get_fldbuf(dis_cuenta.fecha_conversion)

           LET int_flag =FALSE  
           EXIT  CONSTRUCT

        ON KEY(CONTROL-C)
            LET int_flag = TRUE
            EXIT CONSTRUCT            
    END CONSTRUCT

    IF int_flag THEN
        LET int_flag = FALSE
	EXIT PROGRAM
    END IF

    CALL arma_consulta(where_clause)

    IF int_flag THEN
        LET int_flag = FALSE
    END IF
END FUNCTION


#---------------------------------------------------------------------------
# FUNCION QUE ARMA EL QUERY
#---------------------------------------------------------------------------
FUNCTION arma_consulta(where_clause)
#aq-------------------------------------------
    DEFINE
        v_permisos            CHAR(110)
	      
    DEFINE 
        v_cont_reg            INTEGER 

    DEFINE l_record RECORD
        tipo_retiro           LIKE ret_solicitud_tx.tipo_retiro ,
        tipo_movimiento       LIKE dis_cuenta.tipo_movimiento   ,
        nss                   LIKE dis_cuenta.nss               ,
        subcuenta             LIKE dis_cuenta.subcuenta         ,
        folio                 LIKE dis_cuenta.folio             ,
        consecutivo_lote      LIKE dis_cuenta.consecutivo_lote  ,
        siefore               LIKE dis_cuenta.siefore           ,
        monto_en_acciones     LIKE dis_cuenta.monto_en_acciones ,
        fecha_conversion      LIKE dis_cuenta.fecha_conversion
   END RECORD
        
    DEFINE lr_sol_tx    RECORD 
        saldo_viv72           LIKE ret_solicitud_tx.saldo_viv72     ,
        tipo_seguro           LIKE ret_solicitud_tx.tipo_seguro     ,
        tipo_pension          LIKE ret_solicitud_tx.tipo_pension    ,
        regimen               LIKE ret_solicitud_tx.regimen         ,
        tipo_prestacion       LIKE ret_solicitud_tx.tipo_prestacion
    END RECORD

    DEFINE lr_sol_rx   RECORD
        diag_registro         LIKE ret_solicitud_rx.diag_registro ,
        estado_sub_viv        LIKE ret_solicitud_rx.estado_sub_viv
    END RECORD

    DEFINE lr_mto_reten RECORD
        reten_sb              LIKE dis_cuenta.monto_en_acciones
    END RECORD     

    DEFINE where_clause      CHAR(250)
    DEFINE mientras          CHAR(250)
    DEFINE select_stmt       CHAR(2000)
    DEFINE pos,cont_siefore  SMALLINT
    DEFINE cual_siefore      SMALLINT
    DEFINE query_var,indica  CHAR(01)
    DEFINE cont_datos        INTEGER
    DEFINE dia               DATE
    DEFINE hora              CHAR(08)
    DEFINE vsiefore          SMALLINT
    DEFINE reten_sb          LIKE dis_cuenta.monto_en_acciones
    
    LET dia  = TODAY  
    LET hora = TIME

    LET G_IMPRE = g_param_dis.ruta_listados CLIPPED,"/",
		  HOY USING "YYYYMMDD",".805" 

    START REPORT rpt_cuenta_imp TO G_IMPRE
    
    CALL f_genera_tmp_cuenta(where_clause)

    LET select_stmt5 = " SELECT CASE WHEN tipo_movimiento = 820 THEN 'D' ",
                       "             WHEN tipo_movimiento = 830 THEN 'E' ",
                       "             WHEN tipo_movimiento = 840 THEN 'F' ",
                       "             WHEN tipo_movimiento = 850 THEN 'G' ",
                       "             WHEN tipo_movimiento = 860 THEN 'H' ",
                       "             WHEN tipo_movimiento = 880 THEN 'J' ",
                       "             WHEN tipo_movimiento = 825 THEN 'M' END, ",
                       "tipo_movimiento,nss,subcuenta,folio,consecutivo_lote, ",
                       "        siefore,monto_en_acciones,fecha_conversion ", 
                       " FROM tmp_dat_dis_cta ",
                       " WHERE ",where_clause CLIPPED,
                       " AND tipo_movimiento IN (820,830,840,850,860,880,825) ",
                       " AND subcuenta         IN (1,2,5,6,7,9,4,8) ",
                       " AND siefore           IN (1,2,3,4,5,11) ",
                       " GROUP BY 1,2,3,4,5,6,7,8,9 ",
                       " ORDER BY 1,2,3,4,5,6,7,8,9 "

    LET select_stmt5 = select_stmt5 CLIPPED

    PREPARE cust_stmt1 FROM select_stmt5
    DECLARE rept_cur CURSOR FOR cust_stmt1

    LET v_cont_reg = 0
            
    LET ctos_regs  =  0
    FOREACH rept_cur INTO l_record.tipo_retiro      ,
                          l_record.tipo_movimiento  ,
                          l_record.nss              ,
                          l_record.subcuenta        ,
                          l_record.folio            ,
                          l_record.consecutivo_lote ,  
                          l_record.siefore          ,
                          l_record.monto_en_acciones,
                          l_record.fecha_conversion
                          
        ERROR "  PROCESANDO INFORMACION... "
        LET ctos_regs = ctos_regs + 1
                                  
        SELECT fecha_genera
	INTO   f_opera
	FROM   ret_ctr_envio_lote
	WHERE  folio = l_record.folio

        LET mientras = where_clause[01,27]

	IF where_clause[01,27] = "dis_cuenta.fecha_conversion" THEN
	    LET v_folio_interno = 1
	END IF

	SELECT sum(monto_en_acciones)
	INTO   lr_mto_reten.reten_sb 
	FROM   tmp_dat_dis_cta
	WHERE  folio           = l_record.folio 
	AND    nss             = l_record.nss
	AND    siefore         = l_record.siefore
	AND    tipo_movimiento = 10
	IF lr_mto_reten.reten_sb IS NULL OR lr_mto_reten.reten_sb = " " THEN
            LET lr_mto_reten.reten_sb = 0
        END IF

        --SELECCION DE DATOS REQUERIDOS DE ret_monto_siefore y ret_solicitud_tx
        
        SELECT B.saldo_viv72             ,
               B.tipo_seguro             ,
               B.tipo_pension            ,
               B.regimen                 ,
               B.diag_registro           ,
               B.tipo_prestacion
        INTO   lr_sol_tx.saldo_viv72     ,
               lr_sol_tx.tipo_seguro     ,
               lr_sol_tx.tipo_pension    ,
               lr_sol_tx.regimen         ,
               lr_sol_rx.diag_registro   ,
               lr_sol_tx.tipo_prestacion
        FROM   ret_solicitud_tx  B 
        WHERE  B.nss             = l_record.nss               
        AND    B.consecutivo     = l_record.consecutivo_lote  
	AND    B.folio           = l_record.folio

	SELECT COUNT(UNIQUE siefore)
        INTO   cont_siefore
        FROM   tmp_dat_dis_cta
        WHERE  nss              = l_record.nss
	AND    consecutivo_lote = l_record.consecutivo_lote
        AND    folio            = l_record.folio
        AND    siefore         IN (1,2,3,4,5,11)

        IF cont_siefore = 1 THEN
           SELECT UNIQUE siefore
	   INTO   cual_siefore
	   FROM   tmp_dat_dis_cta
           WHERE  nss              = l_record.nss
	   AND    consecutivo_lote = l_record.consecutivo_lote
           AND    folio            = l_record.folio

           CASE cual_siefore
	       WHEN 1
	           LET indica = "S"
	       WHEN 2
	           LET indica = "S"
	       WHEN 3
	           LET indica = "S"
	       WHEN 4
	           LET indica = "S"
	       WHEN 5
	           LET indica = "S"
	       WHEN 11
	           LET indica = "N"
           END CASE
        ELSE
           LET indica = "S"
	END IF

	IF indica = "S" THEN
           IF l_record.siefore <> 11 THEN
               --SELECCION DEL DIAGNOSTICO y  ESTADO DE SUBCUENTA DE VIVIENDA

               { SELECT  diag_registro  ,
                       estado_sub_viv
               INTO    lr_sol_rx.diag_registro  ,
                       lr_sol_rx.estado_sub_viv
               FROM    ret_solicitud_rx
               WHERE   nss         = l_record.nss             
               AND     consecutivo = l_record.consecutivo_lote
               
               IF SQLCA.SQLCODE = NOTFOUND THEN
                   INITIALIZE lr_sol_rx.* TO NULL
               END IF
               }
           
               IF lr_sol_rx.diag_registro IS NULL THEN
                   LET lr_sol_rx.diag_registro    = ' '
               END IF
           
               IF lr_sol_tx.tipo_seguro IS NULL THEN
                  LET lr_sol_tx.saldo_viv72      = 0.0
                  LET lr_sol_tx.tipo_seguro      = '  '
                  LET lr_sol_tx.tipo_pension     = '  '
                  LET lr_sol_tx.regimen          = '  '
                  LET lr_sol_tx.tipo_prestacion  = '  '
               END IF

               IF lr_sol_rx.diag_registro  IS NULL OR
             	  (lr_sol_rx.diag_registro <> 400  AND
                  lr_sol_rx.diag_registro <> 440) THEN
                  LET lr_mto_reten.reten_sb    = 0
               END IF

               OUTPUT TO REPORT rpt_cuenta_imp(l_record.*     ,
                                               lr_sol_tx.*    ,
                                               lr_sol_rx.*    ,
                                               lr_mto_reten.* ,
                                               f_opera        ,
                                               v_folio_interno
                                               )
	   END IF
        ELSE
	   LET l_record.monto_en_acciones = 0
	   LET l_record.siefore = 1
           OUTPUT TO REPORT rpt_cuenta_imp(l_record.*     ,
                                           lr_sol_tx.*    ,
                                           lr_sol_rx.*    ,
                                           lr_mto_reten.* ,
                                           f_opera        ,
                                           v_folio_interno
                                           )
	END IF
    END FOREACH

    IF ( ctos_regs <= 0 ) THEN
        ERROR "NO EXISTEN REGISTROS PARA ESTE CRITERIO..."
        SLEEP 2
        ERROR " "
        EXIT PROGRAM 
    END IF 

    FINISH REPORT rpt_cuenta_imp
       
    LET v_permisos = " chmod 777 ",G_IMPRE
    RUN v_permisos

    PROMPT "  DESEA GENERAR IMPRESION (S/N)?  "
    FOR CHAR aux_pausa

    IF aux_pausa MATCHES "[Ss]" THEN
       LET impresion = "lp ",G_IMPRE
        --LET impresion = "vi ",G_IMPRE
        RUN impresion
    END IF
END FUNCTION


###############################################################################
REPORT rpt_cuenta_imp(l_record         ,
                      lr_sol_tx        ,
                      lr_sol_rx        ,
                      lr_mto_reten     ,
                      f_opera2         ,
                      v_folio_interno2 
                      )
#rci--------------------------------------------------
    DEFINE l_record RECORD
        tipo_retiro           LIKE ret_solicitud_tx.tipo_retiro ,
        tipo_movimiento       LIKE dis_cuenta.tipo_movimiento   ,
        nss                   LIKE dis_cuenta.nss               ,
        subcuenta             LIKE dis_cuenta.subcuenta         ,
        folio                 LIKE dis_cuenta.folio             ,
        consecutivo_lote      LIKE dis_cuenta.consecutivo_lote  ,
        siefore               LIKE dis_cuenta.siefore           ,
        monto_en_acciones     LIKE dis_cuenta.monto_en_acciones ,
        fecha_conversion      LIKE dis_cuenta.fecha_conversion 
    END RECORD

    DEFINE lr_montos  RECORD
        tot_reten             DECIMAL(16,6) ,
        bruto                 DECIMAL(16,6) ,
        a_pagar               DECIMAL(16,6) , 
        r97                   DECIMAL(16,6) ,
        cv                    DECIMAL(16,6) ,
        cs                    DECIMAL(16,6) ,
        viv97                 DECIMAL(16,6) ,
        viv92                 DECIMAL(16,6) ,
        r92                   DECIMAL(16,6) ,
        viv72                 DECIMAL(16,6) ,
        rcv                   DECIMAL(16,6) 
    END RECORD

    DEFINE lr_sol_tx    RECORD 
        saldo_viv72           LIKE ret_solicitud_tx.saldo_viv72     ,
        tipo_seguro           LIKE ret_solicitud_tx.tipo_seguro     ,
        tipo_pension          LIKE ret_solicitud_tx.tipo_pension    ,
        regimen               LIKE ret_solicitud_tx.regimen         ,
        tipo_prestacion       LIKE ret_solicitud_tx.tipo_prestacion
    END RECORD

    DEFINE lr_sol_rx   RECORD
        diag_registro         LIKE ret_solicitud_rx.diag_registro ,
        estado_sub_viv        LIKE ret_solicitud_rx.estado_sub_viv
    END RECORD

    DEFINE lr_precio_acc RECORD
        precio_acc_sb1        LIKE dis_cuenta.precio_accion ,
        precio_acc_sb3        LIKE dis_cuenta.precio_accion ,
        precio_acc_sb4        LIKE dis_cuenta.precio_accion ,
        precio_acc_sb5        LIKE dis_cuenta.precio_accion ,
        precio_acc_sb2        LIKE dis_cuenta.precio_accion
    END RECORD

    DEFINE r_datnss  RECORD
        nombre_afore          LIKE ret_transf_rx.nombre_afore    ,
        paterno_afore         LIKE ret_transf_rx.paterno_afore   ,
        materno_afore         LIKE ret_transf_rx.materno_afore   
    END RECORD

    DEFINE v_folio_interno2   LIKE dis_provision.folio

    DEFINE rpt_afore RECORD
        codigo_afore          LIKE safre_af:tab_afore_local.codigo_afore ,
        razon_social          LIKE safre_af:tab_afore_local.razon_social
    END RECORD

    DEFINE lr_mto_reten RECORD
        reten_sb              LIKE dis_cuenta.monto_en_acciones
    END RECORD     

    DEFINE reg_s1, reg_s2, reg_s3, reg_s4, reg_s5     ARRAY[8] OF RECORD
         tot                  INTEGER,
	 tipo_ret             CHAR(1),
	 reten                LIKE dis_cuenta.monto_en_acciones ,
	 bruto                LIKE dis_cuenta.monto_en_acciones ,
         a_pagar              LIKE dis_cuenta.monto_en_acciones ,
	 r97                  LIKE dis_cuenta.monto_en_acciones ,
	 cv                   LIKE dis_cuenta.monto_en_acciones ,
	 cs                   LIKE dis_cuenta.monto_en_acciones ,
	 viv97                LIKE dis_cuenta.monto_en_acciones ,
	 viv92                LIKE dis_cuenta.monto_en_acciones ,
	 r92                  LIKE dis_cuenta.monto_en_acciones ,
	 viv72                LIKE dis_cuenta.monto_en_acciones ,
	 rcv                  LIKE dis_cuenta.monto_en_acciones
    END RECORD

    DEFINE reg ARRAY[7] OF RECORD
        cont_nss_uni          SMALLINT
    END RECORD  

    DEFINE #loc #date
        f_opera2              DATE

    DEFINE #loc #char
        nombre_siefore        CHAR(03) ,
        r_nombre              CHAR(60) ,
        encabezado            CHAR(30)

    DEFINE #loc #smallint
        cont_nss_fin          ,
        cont_nss_unicos       ,
        cont_tipo_retiro1     ,
        cont_tipo_retiro2     ,
        cont_tipo_retiro3     ,
        cont_tipo_retiro4     ,
        cont_tipo_retiro5     ,
        i                     ,
        vsiefore              ,
        cont_siefore          SMALLINT

    DEFINE 
        tot_siefore1          ,
        tot_siefore3          ,
        tot_siefore4          ,
        tot_siefore5          ,
        tot_siefore2          INTEGER

    DEFINE
        r_nom_rep             CHAR(10) , 
        nombre_final          CHAR(50) ,
        L1                    CHAR(01) ,
        L2                    CHAR(02) ,
        L5                    CHAR(05) ,
        L10                   CHAR(10) 
        
    OUTPUT
        TOP MARGIN    0
        BOTTOM MARGIN 0
        LEFT MARGIN   0
        --RIGHT MARGIN  200
        RIGHT MARGIN  0
        PAGE LENGTH   45
    
    ORDER BY l_record.tipo_retiro ,
             l_record.nss         ,
             l_record.siefore
 
    FORMAT
        ------------------
        FIRST PAGE HEADER
        ------------------
        LET ind             = 0
        LET cont_nss_unicos = 0
        LET cont_nss_fin    = 0

        LET  L1  = "\304"
        LET  L2  = "\304\304"
        LET  L5  = "\304\304\304\304\304"
        LET  L10 = "\304\304\304\304\304\304\304\304\304\304"

        --PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'
        --PRINT COLUMN 1,'\033e\033(s218T\033(s15H\033(s7B'

        PRINT '\033E\033(10U\033&l1O\033&k2S\033&18d',
            '\033e\033(s23H'

        SELECT codigo_afore , 
               razon_social
        INTO   rpt_afore.codigo_afore , 
               rpt_afore.razon_social
        FROM   safre_af:tab_afore_local

        LET HOY       = TODAY
        LET r_nom_rep = "RETL805"
        
        LET tot_siefore1 = 0
        LET tot_siefore2 = 0
        LET tot_siefore3 = 0
        LET tot_siefore4 = 0
        LET tot_siefore5 = 0

        IF v_folio_interno2 = 1 THEN
           LET f_opera = f_opera2
           LET v_folio_interno2 = 0
        ELSE
           SELECT fecha_genera
           INTO   f_opera
           FROM   ret_ctr_envio_lote
           WHERE  folio = l_record.folio

           LET v_folio_interno2 = l_record.folio
        END IF

        SELECT precio_del_dia
        INTO   lr_precio_acc.precio_acc_sb1
        FROM   glo_valor_accion
        WHERE  fecha_valuacion = l_record.fecha_conversion
        AND    codigo_siefore  = 1

        SELECT precio_del_dia
        INTO   lr_precio_acc.precio_acc_sb2
        FROM   glo_valor_accion
        WHERE  fecha_valuacion = l_record.fecha_conversion
        AND    codigo_siefore  = 2

        SELECT precio_del_dia
        INTO   lr_precio_acc.precio_acc_sb3
        FROM   glo_valor_accion
        WHERE  fecha_valuacion = l_record.fecha_conversion
        AND    codigo_siefore  = 3

        SELECT precio_del_dia
        INTO   lr_precio_acc.precio_acc_sb4
        FROM   glo_valor_accion
        WHERE  fecha_valuacion = l_record.fecha_conversion
        AND    codigo_siefore  = 4

        SELECT precio_del_dia
        INTO   lr_precio_acc.precio_acc_sb5
        FROM   glo_valor_accion
        WHERE  fecha_valuacion = l_record.fecha_conversion
        AND    codigo_siefore  = 5

        CALL habil_siguiente(f_opera,3)
        RETURNING f_trans

        IF rpt_afore.codigo_afore = 532 THEN
           LET encabezado = "  SUBDIRECCION DE BENEFICIOS  "
        ELSE 
           LET encabezado = "      MODULO DE RETIROS       "     
        END IF 	  

        PRINT COLUMN 060,encabezado,
            '\033015'

        SKIP 1 LINE

        PRINT COLUMN 049,"  REPORTE DE LIQUIDACION DE DISPOSICION DE RECURSOS",
            '\033015'
        SKIP 2 LINE

        PRINT COLUMN 002,"AFORE               : ",rpt_afore.codigo_afore USING "###","  ",rpt_afore.razon_social CLIPPED  ,
	      COLUMN 125,"PAGINA              :",pageno USING "##",
            '\033015'

        PRINT
        PRINT COLUMN 002,"PROGRAMA            : ",r_nom_rep,
            '\033015'

        PRINT
        PRINT COLUMN 002,"FOLIO INTERNO       : ",v_folio_interno2 USING "<<<<<<<",
    	      COLUMN 125,"FECHA GENERACION    : ",HOY USING "DD-MM-YYYY",
            '\033015'

        PRINT
        PRINT COLUMN 002,"FECHA DE OPERACION  : ",l_record.fecha_conversion USING "DD-MM-YYYY" ,
              COLUMN 035,"VALOR ACCION SB1 :",lr_precio_acc.precio_acc_sb1 USING "######&.&&&&&&" ,
              COLUMN 068,"VALOR ACCION SB2 :",lr_precio_acc.precio_acc_sb2 USING "######&.&&&&&&" ,
              COLUMN 100,"VALOR ACCION SB3 :",lr_precio_acc.precio_acc_sb3 USING "######&.&&&&&&",
              COLUMN 132,"VALOR ACCION SB4 :",lr_precio_acc.precio_acc_sb4 USING "######&.&&&&&&",
              COLUMN 164,"VALOR ACCION SB5 :",lr_precio_acc.precio_acc_sb5 USING "######&.&&&&&&",
            '\033015'

        --PRINT COLUMN 1,'\033e\033(s218T\033(s24H\033(s7B'

        PRINT COLUMN 1,"\332",L10,L1,
                       "\302",L10,L10,L10,L5,L2,
                       "\302",L5,L1,
                       "\302",L5,L1,
                       "\302",L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,
                              L10,L10,L10,L10,L10,L5,L2,L2,
                       "\277",
            '\033015'

        PRINT COLUMN 1,"|           |","                                     |"," TIPO |"," TIPO |","                                                              MONTOS LIQUIDADOS DE LA CUENTA INDIVIDUAL (ACCIONES)                                               ","                  |",
            '\033015'

        PRINT COLUMN 1,"|           |","                                     |","      |","      \303",L2,L1,"\302",L10,L2,L2,L1,"\302",L10,L5,"\302",L10,L5,"\302",L10,L5,"\302",L10,L5,"\302",L10,L5,"\302",L10,L5,"\302",L10,L5,"\302",L10,L5,"\302",L10,L5,"\302",L10,L5,"|",
            '\033015'

        PRINT COLUMN 1,"|    NSS    |","        NOMBRE DEL TRABAJADOR        |","  DE  |","  DE  |","   |","   RETENCION   |","    IMPORTE    |","    IMPORTE    |","      R97      |","       CV      |","      CS       |","  V I V  97    |","  V I V  92    |","      R92      |","  V I V  72    |","     RCV       |",
            '\033015'

      	PRINT COLUMN 1,"|           |","                                     |","      |","      |","   |","               |","               |","               |","               |","               |","               |","               |","               |","               |","               |","               |",
            '\033015'

        PRINT COLUMN 1,"|           |","                                     |"," SEG. |"," PENS.|","SB.|","     TOTAL     |","     BRUTO     |","    A PAGAR    |","   ACCIONES    |","   ACCIONES    |","   ACCIONES    |","   ACCIONES    |","   ACCIONES    |","   ACCIONES    |","   ACCIONES    |","   ACCIONES    |",
            '\033015'

        PRINT COLUMN 1,"\300",L10,L1,
                       "\301",L10,L10,L10,L5,L2,
                       "\301",L5,L1,
                       "\301",L5,L1,
                       "\301",L2,L1,     
                       "\301",L10,L5,
                       "\301",L10,L5,
                       "\301",L10,L5,
                       "\301",L10,L5,
                       "\301",L10,L5,
                       "\301",L10,L5,
                       "\301",L10,L5,
                       "\301",L10,L5,
                       "\301",L10,L5,
                       "\301",L10,L5,
                       "\301",L10,L5,
                       "\331",
            '\033015'

      FOR i=1 TO 8
          LET reg_s1[i].tot     = 0 
          LET reg_s1[i].reten   = 0
          LET reg_s1[i].bruto   = 0
          LET reg_s1[i].a_pagar = 0
          LET reg_s1[i].r97     = 0 
          LET reg_s1[i].cv      = 0 
          LET reg_s1[i].cs      = 0 
          LET reg_s1[i].viv97   = 0
          LET reg_s1[i].viv92   = 0
          LET reg_s1[i].r92     = 0
          LET reg_s1[i].viv72   = 0
          LET reg_s1[i].rcv     = 0 
          --------------------------
          LET reg_s2[i].tot     = 0 
          LET reg_s2[i].reten   = 0
          LET reg_s2[i].bruto   = 0
          LET reg_s2[i].a_pagar = 0
          LET reg_s2[i].r97     = 0 
          LET reg_s2[i].cv      = 0 
          LET reg_s2[i].cs      = 0 
          LET reg_s2[i].viv97   = 0
          LET reg_s2[i].viv92   = 0
          LET reg_s2[i].r92     = 0
          LET reg_s2[i].viv72   = 0
          LET reg_s2[i].rcv     = 0 
          --------------------------
          LET reg_s3[i].tot     = 0 
          LET reg_s3[i].reten   = 0
          LET reg_s3[i].bruto   = 0
          LET reg_s3[i].a_pagar = 0
          LET reg_s3[i].r97     = 0 
          LET reg_s3[i].cv      = 0 
          LET reg_s3[i].cs      = 0 
          LET reg_s3[i].viv97   = 0
          LET reg_s3[i].viv92   = 0
          LET reg_s3[i].r92     = 0
          LET reg_s3[i].viv72   = 0
          LET reg_s3[i].rcv     = 0 
          --------------------------
          LET reg_s4[i].tot     = 0 
          LET reg_s4[i].reten   = 0
          LET reg_s4[i].bruto   = 0
          LET reg_s4[i].a_pagar = 0
          LET reg_s4[i].r97     = 0 
          LET reg_s4[i].cv      = 0 
          LET reg_s4[i].cs      = 0 
          LET reg_s4[i].viv97   = 0
          LET reg_s4[i].viv92   = 0
          LET reg_s4[i].r92     = 0
          LET reg_s4[i].viv72   = 0
          LET reg_s4[i].rcv     = 0 
          --------------------------
          LET reg_s5[i].tot     = 0 
          LET reg_s5[i].reten   = 0
          LET reg_s5[i].bruto   = 0
          LET reg_s5[i].a_pagar = 0
          LET reg_s5[i].r97     = 0 
          LET reg_s5[i].cv      = 0 
          LET reg_s5[i].cs      = 0 
          LET reg_s5[i].viv97   = 0
          LET reg_s5[i].viv92   = 0
          LET reg_s5[i].r92     = 0
          LET reg_s5[i].viv72   = 0
          LET reg_s5[i].rcv     = 0 
      END FOR

      SKIP 2 LINES   
        
     --------------------
     PAGE HEADER
     --------------------
              
        LET  L1  = "\304"
        LET  L2  = "\304\304"
        LET  L5  = "\304\304\304\304\304"
        LET  L10 = "\304\304\304\304\304\304\304\304\304\304"
             
        --PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'
        --PRINT COLUMN 1,'\033e\033(s218T\033(s15H\033(s7B'

        PRINT '\033E\033(10U\033&l1O\033&k2S\033&18d',
            '\033e\033(s23H'

        SELECT codigo_afore , 
               razon_social
        INTO   rpt_afore.codigo_afore , 
               rpt_afore.razon_social
        FROM   safre_af:tab_afore_local

        LET HOY       = TODAY
        LET r_nom_rep = "RETL805"

        IF v_folio_interno2 = 1 THEN
           LET f_opera = f_opera2
           LET v_folio_interno2 = 0
        ELSE
           SELECT fecha_genera
           INTO   f_opera
           FROM   ret_ctr_envio_lote
           WHERE  folio = l_record.folio
            
           LET v_folio_interno2 = l_record.folio
        END IF

        CALL habil_siguiente(f_opera,3)
        RETURNING f_trans

        PRINT COLUMN 060,encabezado,
            '\033015'
        SKIP 1 LINE

        PRINT COLUMN 049,"  REPORTE DE LIQUIDACION DE DISPOSICION DE RECURSOS",
            '\033015'
        SKIP 2 LINE

        PRINT COLUMN 002,"AFORE               : ",rpt_afore.codigo_afore USING "###","  ",rpt_afore.razon_social CLIPPED  ,
	      COLUMN 125,"PAGINA              :",pageno USING "##",
            '\033015'

        PRINT
        PRINT COLUMN 002,"PROGRAMA            : ",r_nom_rep,
            '\033015'

        PRINT
        PRINT COLUMN 002,"FOLIO INTERNO       : ",v_folio_interno2 USING "<<<<<<<",
    	      COLUMN 125,"FECHA GENERACION    : ",HOY USING "DD-MM-YYYY",
            '\033015'

        PRINT
        PRINT COLUMN 002,"FECHA DE OPERACION  : ",l_record.fecha_conversion USING "DD-MM-YYYY",
              COLUMN 035,"VALOR ACCION SB1 :",lr_precio_acc.precio_acc_sb1 USING "######&.&&&&&&",
              COLUMN 068,"VALOR ACCION SB2 :",lr_precio_acc.precio_acc_sb2 USING "######&.&&&&&&",
              COLUMN 100,"VALOR ACCION SB3 :",lr_precio_acc.precio_acc_sb3 USING "######&.&&&&&&",
              COLUMN 132,"VALOR ACCION SB4 :",lr_precio_acc.precio_acc_sb4 USING "######&.&&&&&&",
              COLUMN 164,"VALOR ACCION SB5 :",lr_precio_acc.precio_acc_sb5 USING "######&.&&&&&&",
            '\033015'

        --PRINT COLUMN 1,'\033e\033(s218T\033(s24H\033(s7B'

        PRINT COLUMN 1,"\332",L10,L1,
                       "\302",L10,L10,L10,L5,L2,
                       "\302",L5,L1,
                       "\302",L5,L1,
                       "\302",L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,
                              L10,L10,L10,L10,L10,L5,L2,L2,
                       "\277",
            '\033015'

        PRINT COLUMN 1,"|           |","                                     |"," TIPO |"," TIPO |","                                                              MONTOS LIQUIDADOS DE LA CUENTA INDIVIDUAL (ACCIONES)                                               ","                  |",
            '\033015'

        PRINT COLUMN 1,"|           |","                                     |","      |","      \303",L2,L1,"\302",L10,L2,L2,L1,"\302",L10,L5,"\302",L10,L5,"\302",L10,L5,"\302",L10,L5,"\302",L10,L5,"\302",L10,L5,"\302",L10,L5,"\302",L10,L5,"\302",L10,L5,"\302",L10,L5,"|",
            '\033015'

        PRINT COLUMN 1,"|    NSS    |","        NOMBRE DEL TRABAJADOR        |","  DE  |","  DE  |","   |","   RETENCION   |","    IMPORTE    |","    IMPORTE    |","      R97      |","       CV      |","      CS       |","  V I V  97    |","  V I V  92    |","      R92      |","  V I V  72    |","     RCV       |",
            '\033015'

      	PRINT COLUMN 1,"|           |","                                     |","      |","      |","   |","               |","               |","               |","               |","               |","               |","               |","               |","               |","               |","               |",
            '\033015'

        PRINT COLUMN 1,"|           |","                                     |"," SEG. |"," PENS.|","SB.|","     TOTAL     |","     BRUTO     |","    A PAGAR    |","   ACCIONES    |","   ACCIONES    |","   ACCIONES    |","   ACCIONES    |","   ACCIONES    |","   ACCIONES    |","   ACCIONES    |","   ACCIONES    |",
            '\033015'

             PRINT COLUMN 1,"\300",L10,L1,
                            "\301",L10,L10,L10,L5,L2,
                            "\301",L5,L1,
                            "\301",L5,L1,
                            "\301",L2,L1,     
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\331",
               '\033015'
         SKIP 2 LINE

    ---------------------------------
    AFTER GROUP  OF l_record.siefore
    ---------------------------------
        SELECT nombres ,
               paterno ,
               materno
        INTO   r_datnss.nombre_afore  ,
               r_datnss.paterno_afore ,
               r_datnss.materno_afore
        FROM   afi_mae_afiliado
        WHERE  n_seguro = l_record.nss

        IF SQLCA.SQLCODE = NOTFOUND THEN
           LET r_nombre = NULL
        ELSE 
           LET r_nombre = r_datnss.paterno_afore CLIPPED,' ',
                          r_datnss.materno_afore CLIPPED,' ',
                          r_datnss.nombre_afore CLIPPED
        END IF
        LET sw10 = sw10 + 1
        --SELECCION DE DESCRIPCION DE SIEFORE
        IF l_record.siefore = 1 THEN
            LET  nombre_siefore = "SB1"
            LET  cont_tipo_retiro1 = cont_tipo_retiro1 + 1 
        ELSE
         IF l_record.siefore = 2 THEN
            LET  nombre_siefore = "SB2"
            LET  cont_tipo_retiro2 = cont_tipo_retiro2 + 1 
         ELSE
          IF l_record.siefore = 3 THEN
            LET  nombre_siefore = "SB3"
            LET  cont_tipo_retiro3 = cont_tipo_retiro3 + 1 
          ELSE
           IF l_record.siefore = 4 THEN
            LET  nombre_siefore = "SB4"
            LET  cont_tipo_retiro4 = cont_tipo_retiro4 + 1 
           ELSE
            IF l_record.siefore = 5 THEN
             LET  nombre_siefore = "SB5"
             LET  cont_tipo_retiro5 = cont_tipo_retiro5 + 1 
            END IF
           END IF
          END IF
         END IF
        END IF

        LET lr_montos.a_pagar = GROUP SUM(l_record.monto_en_acciones)                  
        IF lr_montos.a_pagar IS NULL OR lr_montos.a_pagar = ' ' THEN
            LET lr_montos.a_pagar = 0
        END IF

        LET lr_montos.r97 = GROUP SUM(l_record.monto_en_acciones)
                            WHERE l_record.subcuenta = 1  
        IF lr_montos.r97 IS NULL OR lr_montos.r97 = ' ' THEN
            LET lr_montos.r97 = 0
        END IF

        LET lr_montos.cv = GROUP SUM(l_record.monto_en_acciones)
                           WHERE l_record.subcuenta = 2 OR
                                 l_record.subcuenta = 6 OR        
                                 l_record.subcuenta = 9		
        IF lr_montos.cv IS NULL OR lr_montos.cv = ' ' THEN
            LET lr_montos.cv = 0
        END IF

        LET lr_montos.cs = GROUP SUM(l_record.monto_en_acciones)
                           WHERE l_record.subcuenta = 5          
        IF lr_montos.cs IS NULL OR lr_montos.cs = ' ' THEN
            LET lr_montos.cs = 0
        END IF

        LET lr_montos.r92 = GROUP SUM(l_record.monto_en_acciones)
                            WHERE l_record.subcuenta = 7         
        IF lr_montos.r92 IS NULL OR lr_montos.r92 = ' ' THEN
            LET lr_montos.r92 = 0
        END IF

        LET lr_montos.rcv = GROUP SUM(l_record.monto_en_acciones)
                            WHERE l_record.subcuenta = 1 OR
                                  l_record.subcuenta = 2 OR 
                                  l_record.subcuenta = 5 OR
                                  l_record.subcuenta = 6 OR       
                                  l_record.subcuenta = 9		        
        IF lr_montos.rcv IS NULL OR lr_montos.rcv = ' ' THEN
            LET lr_montos.rcv = 0
        END IF     

        SELECT SUM(monto_en_acciones)
        INTO   lr_montos.viv97
        FROM   dis_cuenta
        WHERE  nss       = l_record.nss
        AND    folio     = l_record.folio
        AND    subcuenta = 4

        IF lr_montos.viv97 IS NULL OR lr_montos.viv97 = ' ' THEN
            LET lr_montos.viv97 = 0
        END IF

        SELECT SUM(monto_en_acciones)
        INTO   lr_montos.viv92
        FROM   tmp_dat_dis_cta
        WHERE  nss       = l_record.nss
        AND    folio     = l_record.folio
        AND    subcuenta = 8

        SELECT SUM(monto_en_acciones)
        INTO   lr_montos.viv97
        FROM   tmp_dat_dis_cta
        WHERE  nss       = l_record.nss
        AND    folio     = l_record.folio
        AND    subcuenta = 4
      
        IF lr_montos.viv92 IS NULL OR lr_montos.viv92 = ' ' THEN
            LET lr_montos.viv92 = 0
        END IF

        IF lr_montos.viv97 IS NULL OR lr_montos.viv97 = ' ' THEN
            LET lr_montos.viv97 = 0
        END IF

        LET lr_montos.viv72 = GROUP SUM(lr_sol_tx.saldo_viv72)         
        IF lr_montos.viv72 IS NULL OR lr_montos.viv72 = ' ' THEN
            LET lr_montos.viv72 = 0
        END IF
 
        IF lr_sol_rx.diag_registro IS NULL OR 
           lr_sol_rx.diag_registro  = 440   THEN
                 LET lr_montos.viv97  = 0
                 LET lr_montos.viv92  = 0
	         LET lr_montos.viv72  = 0
        END IF                   	
 
        SELECT COUNT(UNIQUE siefore)                      
        INTO   cont_siefore                               
        FROM   tmp_dat_dis_cta                          
        WHERE  nss              = l_record.nss                 
        AND    consecutivo_lote = l_record.consecutivo_lote    
        AND    siefore          IN(1,2,3,4,5)
        AND    folio            = l_record.folio                   
                                                          
        ------ duda -------
        --IF cont_siefore = 2 AND l_record.siefore = 2 then
        IF cont_siefore > 1 AND sw10 > 1 THEN               
            LET lr_montos.viv97  = 0
            LET lr_montos.viv92  = 0
	    LET lr_montos.viv72  = 0
        END IF                                            
       
        PRINT 
            COLUMN 002,l_record.nss                                     ,
            COLUMN 014,r_nombre[1,35]                                   ,
            COLUMN 053,lr_sol_tx.tipo_seguro                            ,
            COLUMN 060,lr_sol_tx.tipo_pension                           ,
            COLUMN 066,nombre_siefore                                   ,
            COLUMN 070,lr_mto_reten.reten_sb   USING "#######&.&&&&&&"    ,
            COLUMN 086,lr_mto_reten.reten_sb + lr_montos.a_pagar USING "#######&.&&&&&&"    ,
            COLUMN 102,lr_montos.a_pagar   USING "#######&.&&&&&&"    ,
            COLUMN 118,lr_montos.r97       USING "#######&.&&&&&&"    , 
            COLUMN 134,lr_montos.cv        USING "#######&.&&&&&&"    , 
            COLUMN 150,lr_montos.cs        USING "#######&.&&&&&&"    ,
            COLUMN 166,lr_montos.viv97     USING "#######&.&&&&&&"    ,
            COLUMN 182,lr_montos.viv92     USING "#######&.&&&&&&"    ,
            COLUMN 198,lr_montos.r92       USING "#######&.&&&&&&"    ,
            COLUMN 214,lr_montos.viv72     USING "#######&.&&&&&&"    ,
            COLUMN 230,lr_montos.rcv       USING "#######&.&&&&&&",
            '\033015'
        
        SKIP 1 LINE

        --A C U M U L A D O S  (POR TIPO DE RETIRO)
        
        CASE l_record.tipo_retiro
             WHEN "D" LET ind = 1
             WHEN "E" LET ind = 2
             WHEN "F" LET ind = 3
             WHEN "G" LET ind = 4
             WHEN "H" LET ind = 5
             WHEN "J" LET ind = 6
             WHEN "M" LET ind = 7
        END CASE

        IF l_record.siefore = 1 THEN
            LET reg_s1[ind].tot      = reg_s1[ind].tot       + 1 
            LET reg_s1[ind].tipo_ret = l_record.tipo_retiro   
            --ACUMULADO POR TIPO DE RETIRO SIEFORE 1          
            LET reg_s1[ind].reten    = reg_s1[ind].reten     + lr_mto_reten.reten_sb
            LET reg_s1[ind].bruto    = reg_s1[ind].bruto     + 
                                       lr_mto_reten.reten_sb + 
                                       lr_montos.a_pagar     
            LET reg_s1[ind].a_pagar  = reg_s1[ind].a_pagar   + lr_montos.a_pagar
            LET reg_s1[ind].r97      = reg_s1[ind].r97       + lr_montos.r97   
            LET reg_s1[ind].cv       = reg_s1[ind].cv        + lr_montos.cv    
            LET reg_s1[ind].cs       = reg_s1[ind].cs        + lr_montos.cs
            LET reg_s1[ind].viv97    = reg_s1[ind].viv97     + lr_montos.viv97
            LET reg_s1[ind].viv92    = reg_s1[ind].viv92     + lr_montos.viv92
            LET reg_s1[ind].r92      = reg_s1[ind].r92       + lr_montos.r92
            LET reg_s1[ind].viv72    = reg_s1[ind].viv72     + lr_montos.viv72
            LET reg_s1[ind].rcv      = reg_s1[ind].rcv       + lr_montos.rcv
            --ACUMULADO PARA EL TOTAL TOTAL SIEFORE 1        
            LET reg_s1[8].tot      = reg_s1[8].tot           + 1 
            LET reg_s1[8].reten    = reg_s1[8].reten         + lr_mto_reten.reten_sb
            LET reg_s1[8].bruto    = reg_s1[8].bruto         + 
                                     lr_mto_reten.reten_sb   + 
                                     lr_montos.a_pagar 
            LET reg_s1[8].a_pagar  = reg_s1[8].a_pagar       + lr_montos.a_pagar
            LET reg_s1[8].r97      = reg_s1[8].r97           + lr_montos.r97   
            LET reg_s1[8].cv       = reg_s1[8].cv            + lr_montos.cv    
            LET reg_s1[8].cs       = reg_s1[8].cs            + lr_montos.cs
            LET reg_s1[8].viv97    = reg_s1[8].viv97         + lr_montos.viv97
            LET reg_s1[8].viv92    = reg_s1[8].viv92         + lr_montos.viv92
            LET reg_s1[8].r92      = reg_s1[8].r92           + lr_montos.r92
            LET reg_s1[8].viv72    = reg_s1[8].viv72         + lr_montos.viv72
            LET reg_s1[8].rcv      = reg_s1[8].rcv           + lr_montos.rcv
        ELSE        
         IF l_record.siefore = 2 THEN
        	  LET reg_s2[ind].tot      = reg_s2[ind].tot       + 1 
        	  LET reg_s2[ind].tipo_ret = l_record.tipo_retiro
            --ACUMULADO POR TIPO DE RETIRO SIEFORE 2
            LET reg_s2[ind].reten    = reg_s2[ind].reten     + lr_mto_reten.reten_sb
            LET reg_s2[ind].bruto    = reg_s2[ind].bruto     + 
                                       lr_mto_reten.reten_sb + 
                                       lr_montos.a_pagar 
            LET reg_s2[ind].a_pagar  = reg_s2[ind].a_pagar   + lr_montos.a_pagar
            LET reg_s2[ind].r97      = reg_s2[ind].r97       + lr_montos.r97   
            LET reg_s2[ind].cv       = reg_s2[ind].cv        + lr_montos.cv    
            LET reg_s2[ind].cs       = reg_s2[ind].cs        + lr_montos.cs
            LET reg_s2[ind].viv97    = reg_s2[ind].viv97     + lr_montos.viv97
            LET reg_s2[ind].viv92    = reg_s2[ind].viv92     + lr_montos.viv92
            LET reg_s2[ind].r92      = reg_s2[ind].r92       + lr_montos.r92
            LET reg_s2[ind].viv72    = reg_s2[ind].viv72     + lr_montos.viv72
            LET reg_s2[ind].rcv      = reg_s2[ind].rcv       + lr_montos.rcv
            --ACUMULADO PARA EL TOTAL TOTAL SIEFORE 2
            LET reg_s2[8].tot        = reg_s2[8].tot         + 1 
            LET reg_s2[8].reten      = reg_s2[8].reten       + lr_mto_reten.reten_sb
            LET reg_s2[8].bruto      = reg_s2[8].bruto       + 
                                       lr_mto_reten.reten_sb + 
                                       lr_montos.a_pagar     
            LET reg_s2[8].a_pagar    = reg_s2[8].a_pagar     + lr_montos.a_pagar
            LET reg_s2[8].r97        = reg_s2[8].r97         + lr_montos.r97   
            LET reg_s2[8].cv         = reg_s2[8].cv          + lr_montos.cv    
            LET reg_s2[8].cs         = reg_s2[8].cs          + lr_montos.cs
            LET reg_s2[8].viv97      = reg_s2[8].viv97       + lr_montos.viv97
            LET reg_s2[8].viv92      = reg_s2[8].viv92       + lr_montos.viv92
            LET reg_s2[8].r92        = reg_s2[8].r92         + lr_montos.r92
            LET reg_s2[8].viv72      = reg_s2[8].viv72       + lr_montos.viv72
            LET reg_s2[8].rcv        = reg_s2[8].rcv         + lr_montos.rcv
         ELSE
          IF l_record.siefore = 3 THEN
             LET reg_s3[ind].tot      = reg_s3[ind].tot       + 1 
             LET reg_s3[ind].tipo_ret = l_record.tipo_retiro
            --ACUMULADO POR TIPO DE RETIRO SIEFORE 3
            LET reg_s3[ind].reten    = reg_s3[ind].reten     + lr_mto_reten.reten_sb
            LET reg_s3[ind].bruto    = reg_s3[ind].bruto     + 
                                       lr_mto_reten.reten_sb + 
                                       lr_montos.a_pagar 
            LET reg_s3[ind].a_pagar  = reg_s3[ind].a_pagar   + lr_montos.a_pagar
            LET reg_s3[ind].r97      = reg_s3[ind].r97       + lr_montos.r97   
            LET reg_s3[ind].cv       = reg_s3[ind].cv        + lr_montos.cv    
            LET reg_s3[ind].cs       = reg_s3[ind].cs        + lr_montos.cs
            LET reg_s3[ind].viv97    = reg_s3[ind].viv97     + lr_montos.viv97
            LET reg_s3[ind].viv92    = reg_s3[ind].viv92     + lr_montos.viv92
            LET reg_s3[ind].r92      = reg_s3[ind].r92       + lr_montos.r92
            LET reg_s3[ind].viv72    = reg_s3[ind].viv72     + lr_montos.viv72
            LET reg_s3[ind].rcv      = reg_s3[ind].rcv       + lr_montos.rcv
            --ACUMULADO PARA EL TOTAL TOTAL SIEFORE 3
            LET reg_s3[8].tot        = reg_s3[8].tot         + 1 
            LET reg_s3[8].reten      = reg_s3[8].reten       + lr_mto_reten.reten_sb
            LET reg_s3[8].bruto      = reg_s3[8].bruto       + 
                                       lr_mto_reten.reten_sb + 
                                       lr_montos.a_pagar     
            LET reg_s3[8].a_pagar    = reg_s3[8].a_pagar     + lr_montos.a_pagar
            LET reg_s3[8].r97        = reg_s3[8].r97         + lr_montos.r97   
            LET reg_s3[8].cv         = reg_s3[8].cv          + lr_montos.cv    
            LET reg_s3[8].cs         = reg_s3[8].cs          + lr_montos.cs
            LET reg_s3[8].viv97      = reg_s3[8].viv97       + lr_montos.viv97
            LET reg_s3[8].viv92      = reg_s3[8].viv92       + lr_montos.viv92
            LET reg_s3[8].r92        = reg_s3[8].r92         + lr_montos.r92
            LET reg_s3[8].viv72      = reg_s3[8].viv72       + lr_montos.viv72
            LET reg_s3[8].rcv        = reg_s3[8].rcv         + lr_montos.rcv
          ELSE
           IF l_record.siefore = 4 THEN
             LET reg_s4[ind].tot      = reg_s4[ind].tot       + 1 
             LET reg_s4[ind].tipo_ret = l_record.tipo_retiro
             --ACUMULADO POR TIPO DE RETIRO SIEFORE 4
             LET reg_s4[ind].reten    = reg_s4[ind].reten     + lr_mto_reten.reten_sb
             LET reg_s4[ind].bruto    = reg_s4[ind].bruto     + 
                                       lr_mto_reten.reten_sb + 
                                       lr_montos.a_pagar 
             LET reg_s4[ind].a_pagar  = reg_s4[ind].a_pagar   + lr_montos.a_pagar
             LET reg_s4[ind].r97      = reg_s4[ind].r97       + lr_montos.r97   
             LET reg_s4[ind].cv       = reg_s4[ind].cv        + lr_montos.cv    
             LET reg_s4[ind].cs       = reg_s4[ind].cs        + lr_montos.cs
             LET reg_s4[ind].viv97    = reg_s4[ind].viv97     + lr_montos.viv97
             LET reg_s4[ind].viv92    = reg_s4[ind].viv92     + lr_montos.viv92
             LET reg_s4[ind].r92      = reg_s4[ind].r92       + lr_montos.r92
             LET reg_s4[ind].viv72    = reg_s4[ind].viv72     + lr_montos.viv72
             LET reg_s4[ind].rcv      = reg_s4[ind].rcv       + lr_montos.rcv
             --ACUMULADO PARA EL TOTAL TOTAL SIEFORE 4
             LET reg_s4[8].tot        = reg_s4[8].tot         + 1 
             LET reg_s4[8].reten      = reg_s4[8].reten       + lr_mto_reten.reten_sb
             LET reg_s4[8].bruto      = reg_s4[8].bruto       + 
                                       lr_mto_reten.reten_sb + 
                                       lr_montos.a_pagar     
             LET reg_s4[8].a_pagar    = reg_s4[8].a_pagar     + lr_montos.a_pagar
             LET reg_s4[8].r97        = reg_s4[8].r97         + lr_montos.r97   
             LET reg_s4[8].cv         = reg_s4[8].cv          + lr_montos.cv    
             LET reg_s4[8].cs         = reg_s4[8].cs          + lr_montos.cs
             LET reg_s4[8].viv97      = reg_s4[8].viv97       + lr_montos.viv97
             LET reg_s4[8].viv92      = reg_s4[8].viv92       + lr_montos.viv92
             LET reg_s4[8].r92        = reg_s4[8].r92         + lr_montos.r92
             LET reg_s4[8].viv72      = reg_s4[8].viv72       + lr_montos.viv72
             LET reg_s4[8].rcv        = reg_s4[8].rcv         + lr_montos.rcv
           ELSE
            IF l_record.siefore = 5 THEN
             LET reg_s5[ind].tot      = reg_s5[ind].tot       + 1 
             LET reg_s5[ind].tipo_ret = l_record.tipo_retiro
             --ACUMULADO POR TIPO DE RETIRO SIEFORE 5
             LET reg_s5[ind].reten    = reg_s5[ind].reten     + lr_mto_reten.reten_sb
             LET reg_s5[ind].bruto    = reg_s5[ind].bruto     + 
                                       lr_mto_reten.reten_sb + 
                                       lr_montos.a_pagar 
             LET reg_s5[ind].a_pagar  = reg_s5[ind].a_pagar   + lr_montos.a_pagar
             LET reg_s5[ind].r97      = reg_s5[ind].r97       + lr_montos.r97   
             LET reg_s5[ind].cv       = reg_s5[ind].cv        + lr_montos.cv    
             LET reg_s5[ind].cs       = reg_s5[ind].cs        + lr_montos.cs
             LET reg_s5[ind].viv97    = reg_s5[ind].viv97     + lr_montos.viv97
             LET reg_s5[ind].viv92    = reg_s5[ind].viv92     + lr_montos.viv92
             LET reg_s5[ind].r92      = reg_s5[ind].r92       + lr_montos.r92
             LET reg_s5[ind].viv72    = reg_s5[ind].viv72     + lr_montos.viv72
             LET reg_s5[ind].rcv      = reg_s5[ind].rcv       + lr_montos.rcv
             --ACUMULADO PARA EL TOTAL TOTAL SIEFORE 5
             LET reg_s5[8].tot        = reg_s5[8].tot         + 1 
             LET reg_s5[8].reten      = reg_s5[8].reten       + lr_mto_reten.reten_sb
             LET reg_s5[8].bruto      = reg_s5[8].bruto       + 
                                       lr_mto_reten.reten_sb + 
                                       lr_montos.a_pagar     
             LET reg_s5[8].a_pagar    = reg_s5[8].a_pagar     + lr_montos.a_pagar
             LET reg_s5[8].r97        = reg_s5[8].r97         + lr_montos.r97   
             LET reg_s5[8].cv         = reg_s5[8].cv          + lr_montos.cv    
             LET reg_s5[8].cs         = reg_s5[8].cs          + lr_montos.cs
             LET reg_s5[8].viv97      = reg_s5[8].viv97       + lr_montos.viv97
             LET reg_s5[8].viv92      = reg_s5[8].viv92       + lr_montos.viv92
             LET reg_s5[8].r92        = reg_s5[8].r92         + lr_montos.r92
             LET reg_s5[8].viv72      = reg_s5[8].viv72       + lr_montos.viv72
             LET reg_s5[8].rcv        = reg_s5[8].rcv         + lr_montos.rcv
            END IF
           END IF
          END IF
         END IF
        END IF
        
     { ON EVERY ROW
        IF lineno > 44 THEN
           SKIP TO TOP OF PAGE
        END IF }

     -----------------------------------
     BEFORE GROUP OF l_record.nss
     -----------------------------------
         LET cont_nss_unicos = cont_nss_unicos + 1
         LET sw10 = 0    
    AFTER GROUP OF l_record.tipo_retiro

        CASE l_record.tipo_retiro
            WHEN "D" LET ind = 1
            WHEN "E" LET ind = 2
            WHEN "F" LET ind = 3
            WHEN "G" LET ind = 4
            WHEN "H" LET ind = 5
            WHEN "J" LET ind = 6
            WHEN "M" LET ind = 7
        END CASE

        LET reg[ind].cont_nss_uni = cont_nss_unicos 

        PRINT 

        -------------------------------------------------------------
        -- IMPRESION DE TOTAL POR TIPO DE RETIRO PARA SIEFORE 1
        -------------------------------------------------------------
        IF reg_s1[ind].tot > 0 THEN
	          PRINT COLUMN 1,"\332",L10,L1,
	                         "\302",L10,L10,L10,L5,L5,L5,L5,L1,
	                         "\302",L2,L1,     
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\277",
                    '\033015'

            PRINT
                COLUMN 001,"|",reg_s1[ind].tot USING "#######"        ,
                COLUMN 013,"|",
                COLUMN 019,"TOTALES POR TIPO RETIRO ",reg_s1[ind].tipo_ret,
                COLUMN 065,"|",
                COLUMN 066,"SB1",
                COLUMN 069,"|",
                COLUMN 070,reg_s1[ind].reten   USING "#######&.&&&&&&",
                COLUMN 085,"|",
                COLUMN 086,reg_s1[ind].bruto   USING "#######&.&&&&&&",
                COLUMN 100,"|",
                COLUMN 101,reg_s1[ind].a_pagar USING "#######&.&&&&&&",
                COLUMN 115,"|",
                COLUMN 116,reg_s1[ind].r97     USING "#######&.&&&&&&",
                COLUMN 130,"|",
                COLUMN 131,reg_s1[ind].cv      USING "#######&.&&&&&&",
                COLUMN 145,"|",
                COLUMN 146,reg_s1[ind].cs      USING "#######&.&&&&&&",
                COLUMN 162,"|",
                COLUMN 163,reg_s1[ind].viv97   USING "#######&.&&&&&&",
                COLUMN 177,"|",
                COLUMN 178,reg_s1[ind].viv92   USING "#######&.&&&&&&",
                COLUMN 193,"|",
                COLUMN 194,reg_s1[ind].r92     USING "#######&.&&&&&&",
                COLUMN 209,"|",
                COLUMN 210,reg_s1[ind].viv72   USING "#######&.&&&&&&",
                COLUMN 225,"|",
                COLUMN 226,reg_s1[ind].rcv     USING "#######&.&&&&&&",
                COLUMN 245,"|",
            '\033015'

            PRINT COLUMN 1,"\300",L10,L1,
                           "\301",L10,L10,L10,L5,L5,L5,L5,L1,
                           "\301",L2,L1,     
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\331",
               '\033015'
        END IF                            
                                                     
        -------------------------------------------------------------
        -- IMPRESION DE TOTAL POR TIPO DE RETIRO PARA SIEFORE 2
        -------------------------------------------------------------
        IF reg_s2[ind].tot > 0 THEN

	          PRINT COLUMN 1,"\332",L10,L1,
	                         "\302",L10,L10,L10,L5,L5,L5,L5,L1,
	                         "\302",L2,L1,     
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\277",
                  '\033015'

            PRINT
                COLUMN 001,"|",reg_s2[ind].tot USING "#######"        ,
                COLUMN 013,"|",
                COLUMN 019,"TOTALES POR TIPO RETIRO  ",reg_s2[ind].tipo_ret,
                COLUMN 065,"|",
                COLUMN 066,"SB2",
                COLUMN 069,"|",
	        COLUMN 070,reg_s2[ind].reten   USING "#######&.&&&&&&",
                COLUMN 085,"|",
                COLUMN 086,reg_s2[ind].bruto   USING "#######&.&&&&&&",
                COLUMN 100,"|",
                COLUMN 101,reg_s2[ind].a_pagar USING "#######&.&&&&&&",
                COLUMN 115,"|",
                COLUMN 116,reg_s2[ind].r97     USING "#######&.&&&&&&",
                COLUMN 130,"|",
                COLUMN 131,reg_s2[ind].cv      USING "#######&.&&&&&&",
                COLUMN 145,"|",
                COLUMN 146,reg_s2[ind].cs      USING "#######&.&&&&&&",
                COLUMN 162,"|",
                COLUMN 163,reg_s2[ind].viv97   USING "#######&.&&&&&&",
                COLUMN 177,"|",
                COLUMN 178,reg_s2[ind].viv92   USING "#######&.&&&&&&",
                COLUMN 193,"|",
                COLUMN 194,reg_s2[ind].r92     USING "#######&.&&&&&&",
                COLUMN 209,"|",
                COLUMN 210,reg_s2[ind].viv72   USING "#######&.&&&&&&",
                COLUMN 225,"|",
	        COLUMN 226,reg_s2[ind].rcv     USING "#######&.&&&&&&",
                COLUMN 245,"|",
            '\033015'

            PRINT COLUMN 1,"\300",L10,L1,
                           "\301",L10,L10,L10,L5,L5,L5,L5,L1,
                           "\301",L2,L1,     
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\331",
            '\033015'

        END IF
        -------------------------------------------------------------
        -- IMPRESION DE TOTAL POR TIPO DE RETIRO PARA SIEFORE 3
        -------------------------------------------------------------
        IF reg_s3[ind].tot > 0 THEN

	          PRINT COLUMN 1,"\332",L10,L1,
	                         "\302",L10,L10,L10,L5,L5,L5,L5,L1,
	                         "\302",L2,L1,     
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\277",
            '\033015'

            PRINT
                COLUMN 001,"|",reg_s3[ind].tot USING "#######"        ,
                COLUMN 013,"|",
                COLUMN 019,"TOTALES POR TIPO RETIRO  ",reg_s3[ind].tipo_ret,
                COLUMN 065,"|",
                COLUMN 066,"SB3",
                COLUMN 069,"|",
	        COLUMN 070,reg_s3[ind].reten   USING "#######&.&&&&&&",
                COLUMN 085,"|",
                COLUMN 086,reg_s3[ind].bruto   USING "#######&.&&&&&&",
                COLUMN 100,"|",
                COLUMN 101,reg_s3[ind].a_pagar USING "#######&.&&&&&&",
                COLUMN 115,"|",
                COLUMN 116,reg_s3[ind].r97     USING "#######&.&&&&&&",
                COLUMN 130,"|",
                COLUMN 131,reg_s3[ind].cv      USING "#######&.&&&&&&",
                COLUMN 145,"|",
                COLUMN 146,reg_s3[ind].cs      USING "#######&.&&&&&&",
                COLUMN 162,"|",
                COLUMN 163,reg_s3[ind].viv97   USING "#######&.&&&&&&",
                COLUMN 177,"|",
                COLUMN 178,reg_s3[ind].viv92   USING "#######&.&&&&&&",
                COLUMN 193,"|",
                COLUMN 194,reg_s3[ind].r92     USING "#######&.&&&&&&",
                COLUMN 209,"|",
                COLUMN 210,reg_s3[ind].viv72   USING "#######&.&&&&&&",
                COLUMN 225,"|",
	        COLUMN 226,reg_s3[ind].rcv     USING "#######&.&&&&&&",
                COLUMN 245,"|",
            '\033015'

            PRINT COLUMN 1,"\300",L10,L1,
                           "\301",L10,L10,L10,L5,L5,L5,L5,L1,
                           "\301",L2,L1,     
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\331",
            '\033015'

        END IF
        -------------------------------------------------------------
        -- IMPRESION DE TOTAL POR TIPO DE RETIRO PARA SIEFORE 4
        -------------------------------------------------------------
        IF reg_s4[ind].tot > 0 THEN

	          PRINT COLUMN 1,"\332",L10,L1,
	                         "\302",L10,L10,L10,L5,L5,L5,L5,L1,
	                         "\302",L2,L1,     
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\277",
            '\033015'

            PRINT
                COLUMN 001,"|",reg_s4[ind].tot USING "#######"        ,
                COLUMN 013,"|",
                COLUMN 019,"TOTALES POR TIPO RETIRO  ",reg_s4[ind].tipo_ret,
                COLUMN 065,"|",
                COLUMN 066,"SB4",
                COLUMN 069,"|",
	        COLUMN 070,reg_s4[ind].reten   USING "#######&.&&&&&&",
                COLUMN 085,"|",
                COLUMN 086,reg_s4[ind].bruto   USING "#######&.&&&&&&",
                COLUMN 100,"|",
                COLUMN 101,reg_s4[ind].a_pagar USING "#######&.&&&&&&",
                COLUMN 115,"|",
                COLUMN 116,reg_s4[ind].r97     USING "#######&.&&&&&&",
                COLUMN 130,"|",
                COLUMN 131,reg_s4[ind].cv      USING "#######&.&&&&&&",
                COLUMN 145,"|",
                COLUMN 146,reg_s4[ind].cs      USING "#######&.&&&&&&",
                COLUMN 162,"|",
                COLUMN 163,reg_s4[ind].viv97   USING "#######&.&&&&&&",
                COLUMN 177,"|",
                COLUMN 178,reg_s4[ind].viv92   USING "#######&.&&&&&&",
                COLUMN 193,"|",
                COLUMN 194,reg_s4[ind].r92     USING "#######&.&&&&&&",
                COLUMN 209,"|",
                COLUMN 210,reg_s4[ind].viv72   USING "#######&.&&&&&&",
                COLUMN 225,"|",
	        COLUMN 226,reg_s4[ind].rcv     USING "#######&.&&&&&&",
                COLUMN 245,"|",
            '\033015'

            PRINT COLUMN 1,"\300",L10,L1,
                           "\301",L10,L10,L10,L5,L5,L5,L5,L1,
                           "\301",L2,L1,     
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\331",
            '\033015'

        END IF
        -------------------------------------------------------------
        -- IMPRESION DE TOTAL POR TIPO DE RETIRO PARA SIEFORE 5
        -------------------------------------------------------------
        IF reg_s5[ind].tot > 0 THEN

	          PRINT COLUMN 1,"\332",L10,L1,
	                         "\302",L10,L10,L10,L5,L5,L5,L5,L1,
	                         "\302",L2,L1,     
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\277",
            '\033015'

            PRINT
                COLUMN 001,"|",reg_s5[ind].tot USING "#######"        ,
                COLUMN 013,"|",
                COLUMN 019,"TOTALES POR TIPO RETIRO  ",reg_s5[ind].tipo_ret,
                COLUMN 065,"|",
                COLUMN 066,"SB5",
                COLUMN 069,"|",
	        COLUMN 070,reg_s5[ind].reten   USING "#######&.&&&&&&",
                COLUMN 085,"|",
                COLUMN 086,reg_s5[ind].bruto   USING "#######&.&&&&&&",
                COLUMN 100,"|",
                COLUMN 101,reg_s5[ind].a_pagar USING "#######&.&&&&&&",
                COLUMN 115,"|",
                COLUMN 116,reg_s5[ind].r97     USING "#######&.&&&&&&",
                COLUMN 130,"|",
                COLUMN 131,reg_s5[ind].cv      USING "#######&.&&&&&&",
                COLUMN 145,"|",
                COLUMN 146,reg_s5[ind].cs      USING "#######&.&&&&&&",
                COLUMN 162,"|",
                COLUMN 163,reg_s5[ind].viv97   USING "#######&.&&&&&&",
                COLUMN 177,"|",
                COLUMN 178,reg_s5[ind].viv92   USING "#######&.&&&&&&",
                COLUMN 193,"|",
                COLUMN 194,reg_s5[ind].r92     USING "#######&.&&&&&&",
                COLUMN 209,"|",
                COLUMN 210,reg_s5[ind].viv72   USING "#######&.&&&&&&",
                COLUMN 225,"|",
	        COLUMN 226,reg_s5[ind].rcv     USING "#######&.&&&&&&",
                COLUMN 245,"|",
            '\033015'

            PRINT COLUMN 1,"\300",L10,L1,
                           "\301",L10,L10,L10,L5,L5,L5,L5,L1,
                           "\301",L2,L1,     
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\331",
            '\033015'

        END IF

        ---------------------------------------------------
        -- DETALLE DE TOTALES DE NSS UNICOS Y SUMA DE VIVIENDA
        ---------------------------------------------------
        PRINT COLUMN 1,"\332",L10,L1,
                      L10,L10,L10,L5,L2,L2,L5,L1,L5,L2,L2,L1,
                      L5,L1,
                      L10,L2,L2,L1,
                      L10,L2,L2,L1,
                      L10,L2,L2,L1,
                      L10,L2,L2,L1,
                      L10,L2,L2,L1,
                      L10,L5,L1,
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\277",
            '\033015'

        PRINT 
            COLUMN 001,"|",
            COLUMN 019,"TOTAL DE NSS UNICOS :  ",cont_nss_unicos USING "#####",
            COLUMN 165,"|",
            COLUMN 166,reg_s1[ind].viv97 +
                       reg_s3[ind].viv97 +
                       reg_s4[ind].viv97 +
                       reg_s5[ind].viv97 +
                       reg_s2[ind].viv97    USING "#######&.&&&&&&",
            COLUMN 181,"|",
            COLUMN 182,reg_s1[ind].viv92 +
                       reg_s3[ind].viv92 +
                       reg_s4[ind].viv92 +
                       reg_s5[ind].viv92 +
                       reg_s2[ind].viv92    USING "#######&.&&&&&&",
            COLUMN 197,"|",
            COLUMN 213,"|",
            COLUMN 214,reg_s1[ind].viv72  +
                       reg_s3[ind].viv72  +
                       reg_s4[ind].viv72  +
                       reg_s5[ind].viv72  +
                       reg_s2[ind].viv72    USING "#######&.&&&&&&",
            COLUMN 229,"|",
            COLUMN 245,"|",
            '\033015'
                            
        PRINT COLUMN 1,"\300",L10,L1,
                      L10,L10,L10,L5,L2,L2,L5,L1,L5,L2,L2,L1,
                      L5,L1,
                      L10,L2,L2,L1,
                      L10,L2,L2,L1,
                      L10,L2,L2,L1,
                      L10,L2,L2,L1,
                      L10,L2,L2,L1,
                      L10,L5,L1,
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\331",
            '\033015'

        SKIP 2 LINES 
        LET cont_nss_unicos   = 0

      -----------------------------------
      AFTER GROUP OF l_record.nss
      -----------------------------------
         LET cont_nss_fin = cont_nss_fin + 1

    --------------
    ON LAST ROW
    --------------
        --NEED 45 LINES
        
        SKIP 3 LINES
        
        PRINT "R E S U M E N ",
            '\033015'
        PRINT
        
        --IMPRESION DE TODOS LOS DETALLES DE CADA TIPO DE RETIRO      
        
        FOR i = 1 TO 7
            -------------------------------------------------------------
            -- IMPRESION DE TOTAL FINAL POR TIPO DE RETIRO PARA SIEFORE 1
            -------------------------------------------------------------
            IF reg_s1[i].tot > 0 THEN
        
	              PRINT COLUMN 1,"\332",L10,L1,
	                             "\302",L10,L10,L10,L5,L5,L5,L5,L1,
	                             "\302",L2,L1,     
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\277",
                        '\033015'
        
                PRINT
                    COLUMN 001,"|",reg_s1[i].tot USING "#######"        ,
                    COLUMN 013,"|",
                    COLUMN 019,"TOTALES POR TIPO RETIRO  ",reg_s1[i].tipo_ret,
                    COLUMN 065,"|",
                    COLUMN 066,"SB1",
                    COLUMN 069,"|",
		    COLUMN 070,reg_s1[i].reten   USING "#######&.&&&&&&",
                    COLUMN 085,"|",
                    COLUMN 086,reg_s1[i].bruto   USING "#######&.&&&&&&",
                    COLUMN 100,"|",
                    COLUMN 101,reg_s1[i].a_pagar USING "#######&.&&&&&&",
                    COLUMN 115,"|",
                    COLUMN 116,reg_s1[i].r97     USING "#######&.&&&&&&",
                    COLUMN 130,"|",
                    COLUMN 131,reg_s1[i].cv      USING "#######&.&&&&&&",
                    COLUMN 145,"|",
                    COLUMN 146,reg_s1[i].cs      USING "#######&.&&&&&&",
                    COLUMN 162,"|",
                    COLUMN 163,reg_s1[i].viv97   USING "#######&.&&&&&&",
                    COLUMN 177,"|",
                    COLUMN 178,reg_s1[i].viv92   USING "#######&.&&&&&&",
                    COLUMN 193,"|",
                    COLUMN 194,reg_s1[i].r92     USING "#######&.&&&&&&",
                    COLUMN 209,"|",
                    COLUMN 210,reg_s1[i].viv72   USING "#######&.&&&&&&",
                    COLUMN 225,"|",
		    COLUMN 226,reg_s1[i].rcv     USING "#######&.&&&&&&",
                    COLUMN 245,"|",
            '\033015'
        
                PRINT COLUMN 1,"\300",L10,L1,
                               "\301",L10,L10,L10,L5,L5,L5,L5,L1,
                               "\301",L2,L1,     
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\331",
                 '\033015'
        
            END IF	 
        
        
            -------------------------------------------------------------
            -- IMPRESION DE TOTAL FINAL POR TIPO DE RETIRO PARA SIEFORE 2
            -------------------------------------------------------------
            IF reg_s2[i].tot > 0 THEN
        
	              PRINT COLUMN 1,"\332",L10,L1,
	                             "\302",L10,L10,L10,L5,L5,L5,L5,L1,
	                             "\302",L2,L1,     
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\277",
                       '\033015'
                PRINT
                    COLUMN 001,"|",reg_s2[i].tot USING "#######"        ,
                    COLUMN 013,"|",
                    COLUMN 019,"TOTALES POR TIPO RETIRO  ",reg_s2[i].tipo_ret,
                    COLUMN 065,"|",
                    COLUMN 066,"SB2",
                    COLUMN 069,"|",
		    COLUMN 070,reg_s2[i].reten   USING "#######&.&&&&&&",
                    COLUMN 085,"|",
                    COLUMN 086,reg_s2[i].bruto   USING "#######&.&&&&&&",
                    COLUMN 100,"|",
                    COLUMN 101,reg_s2[i].a_pagar USING "#######&.&&&&&&",
                    COLUMN 115,"|",
                    COLUMN 116,reg_s2[i].r97     USING "#######&.&&&&&&",
                    COLUMN 130,"|",
                    COLUMN 131,reg_s2[i].cv      USING "#######&.&&&&&&",
                    COLUMN 145,"|",
                    COLUMN 146,reg_s2[i].cs      USING "#######&.&&&&&&",
                    COLUMN 162,"|",
                    COLUMN 163,reg_s2[i].viv97   USING "#######&.&&&&&&",
                    COLUMN 177,"|",
                    COLUMN 178,reg_s2[i].viv92   USING "#######&.&&&&&&",
                    COLUMN 193,"|",
                    COLUMN 194,reg_s2[i].r92     USING "#######&.&&&&&&",
                    COLUMN 209,"|",
                    COLUMN 210,reg_s2[i].viv72   USING "#######&.&&&&&&",
                    COLUMN 225,"|",
		    COLUMN 226,reg_s2[i].rcv     USING "#######&.&&&&&&",
                    COLUMN 245,"|",
                '\033015'
                
                PRINT COLUMN 1,"\300",L10,L1,
                               "\301",L10,L10,L10,L5,L5,L5,L5,L1,
                               "\301",L2,L1,     
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\331",
                   '\033015'
            END IF
            -------------------------------------------------------------
            -- IMPRESION DE TOTAL FINAL POR TIPO DE RETIRO PARA SIEFORE 3
            -------------------------------------------------------------
            IF reg_s3[i].tot > 0 THEN
        
	              PRINT COLUMN 1,"\332",L10,L1,
	                             "\302",L10,L10,L10,L5,L5,L5,L5,L1,
	                             "\302",L2,L1,     
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\277",
                      '\033015'
                PRINT
                    COLUMN 001,"|",reg_s3[i].tot USING "#######"        ,
                    COLUMN 013,"|",
                    COLUMN 019,"TOTALES POR TIPO RETIRO  ",reg_s3[i].tipo_ret,
                    COLUMN 065,"|",
                    COLUMN 066,"SB3",
                    COLUMN 069,"|",
		    COLUMN 070,reg_s3[i].reten   USING "#######&.&&&&&&",
                    COLUMN 085,"|",
                    COLUMN 086,reg_s3[i].bruto   USING "#######&.&&&&&&",
                    COLUMN 100,"|",
                    COLUMN 101,reg_s3[i].a_pagar USING "#######&.&&&&&&",
                    COLUMN 115,"|",
                    COLUMN 116,reg_s3[i].r97     USING "#######&.&&&&&&",
                    COLUMN 130,"|",
                    COLUMN 131,reg_s3[i].cv      USING "#######&.&&&&&&",
                    COLUMN 145,"|",
                    COLUMN 146,reg_s3[i].cs      USING "#######&.&&&&&&",
                    COLUMN 162,"|",
                    COLUMN 163,reg_s3[i].viv97   USING "#######&.&&&&&&",
                    COLUMN 177,"|",
                    COLUMN 178,reg_s3[i].viv92   USING "#######&.&&&&&&",
                    COLUMN 193,"|",
                    COLUMN 194,reg_s3[i].r92     USING "#######&.&&&&&&",
                    COLUMN 209,"|",
                    COLUMN 210,reg_s3[i].viv72   USING "#######&.&&&&&&",
                    COLUMN 225,"|",
		    COLUMN 226,reg_s3[i].rcv     USING "#######&.&&&&&&",
                    COLUMN 245,"|",
                '\033015'
                
                PRINT COLUMN 1,"\300",L10,L1,
                               "\301",L10,L10,L10,L5,L5,L5,L5,L1,
                               "\301",L2,L1,     
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\331",
                      '\033015'
            END IF
            -------------------------------------------------------------
            -- IMPRESION DE TOTAL FINAL POR TIPO DE RETIRO PARA SIEFORE 4
            -------------------------------------------------------------
            IF reg_s4[i].tot > 0 THEN
        
	              PRINT COLUMN 1,"\332",L10,L1,
	                             "\302",L10,L10,L10,L5,L5,L5,L5,L1,
	                             "\302",L2,L1,     
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\277",
                        '\033015'
                PRINT
                    COLUMN 001,"|",reg_s4[i].tot USING "#######"        ,
                    COLUMN 013,"|",
                    COLUMN 019,"TOTALES POR TIPO RETIRO  ",reg_s4[i].tipo_ret,
                    COLUMN 065,"|",
                    COLUMN 066,"SB4",
                    COLUMN 069,"|",
		    COLUMN 070,reg_s4[i].reten   USING "#######&.&&&&&&",
                    COLUMN 085,"|",
                    COLUMN 086,reg_s4[i].bruto   USING "#######&.&&&&&&",
                    COLUMN 100,"|",
                    COLUMN 101,reg_s4[i].a_pagar USING "#######&.&&&&&&",
                    COLUMN 115,"|",
                    COLUMN 116,reg_s4[i].r97     USING "#######&.&&&&&&",
                    COLUMN 130,"|",
                    COLUMN 131,reg_s4[i].cv      USING "#######&.&&&&&&",
                    COLUMN 145,"|",
                    COLUMN 146,reg_s4[i].cs      USING "#######&.&&&&&&",
                    COLUMN 162,"|",
                    COLUMN 163,reg_s4[i].viv97   USING "#######&.&&&&&&",
                    COLUMN 177,"|",
                    COLUMN 178,reg_s4[i].viv92   USING "#######&.&&&&&&",
                    COLUMN 193,"|",
                    COLUMN 194,reg_s4[i].r92     USING "#######&.&&&&&&",
                    COLUMN 209,"|",
                    COLUMN 210,reg_s4[i].viv72   USING "#######&.&&&&&&",
                    COLUMN 225,"|",
		    COLUMN 226,reg_s4[i].rcv     USING "#######&.&&&&&&",
                    COLUMN 245,"|",
                 '\033015'
                
                PRINT COLUMN 1,"\300",L10,L1,
                               "\301",L10,L10,L10,L5,L5,L5,L5,L1,
                               "\301",L2,L1,     
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\331",
                '\033015'
            END IF
            -------------------------------------------------------------
            -- IMPRESION DE TOTAL FINAL POR TIPO DE RETIRO PARA SIEFORE 5
            -------------------------------------------------------------
            IF reg_s5[i].tot > 0 THEN
        
	              PRINT COLUMN 1,"\332",L10,L1,
	                             "\302",L10,L10,L10,L5,L5,L5,L5,L1,
	                             "\302",L2,L1,     
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\302",L10,L5,
	                             "\277",
                      '\033015'
                PRINT
                    COLUMN 001,"|",reg_s5[i].tot USING "#######"        ,
                    COLUMN 013,"|",
                    COLUMN 019,"TOTALES POR TIPO RETIRO  ",reg_s5[i].tipo_ret,
                    COLUMN 065,"|",
                    COLUMN 066,"SB5",
                    COLUMN 069,"|",
		    COLUMN 070,reg_s5[i].reten   USING "#######&.&&&&&&",
                    COLUMN 085,"|",
                    COLUMN 086,reg_s5[i].bruto   USING "#######&.&&&&&&",
                    COLUMN 100,"|",
                    COLUMN 101,reg_s5[i].a_pagar USING "#######&.&&&&&&",
                    COLUMN 115,"|",
                    COLUMN 116,reg_s5[i].r97     USING "#######&.&&&&&&",
                    COLUMN 130,"|",
                    COLUMN 131,reg_s5[i].cv      USING "#######&.&&&&&&",
                    COLUMN 145,"|",
                    COLUMN 146,reg_s5[i].cs      USING "#######&.&&&&&&",
                    COLUMN 162,"|",
                    COLUMN 163,reg_s5[i].viv97   USING "#######&.&&&&&&",
                    COLUMN 177,"|",
                    COLUMN 178,reg_s5[i].viv92   USING "#######&.&&&&&&",
                    COLUMN 193,"|",
                    COLUMN 194,reg_s5[i].r92     USING "#######&.&&&&&&",
                    COLUMN 209,"|",
                    COLUMN 210,reg_s5[i].viv72   USING "#######&.&&&&&&",
                    COLUMN 225,"|",
		    COLUMN 226,reg_s5[i].rcv     USING "#######&.&&&&&&",
                    COLUMN 245,"|",
                '\033015'
                
                PRINT COLUMN 1,"\300",L10,L1,
                               "\301",L10,L10,L10,L5,L5,L5,L5,L1,
                               "\301",L2,L1,     
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\331",
                  '\033015'
            END IF
            IF reg[i].cont_nss_uni > 0 THEN   
             ---------------------------------------------------
             -- DETALLE DE TOTALES DE NSS UNICOS Y SUMA DE VIVIENDA
             ---------------------------------------------------
                PRINT COLUMN 1,"\332",L10,L1,
                              L10,L10,L10,L5,L2,L2,L5,L1,L5,L2,L2,L1,
                              L5,L1,
                              L10,L2,L2,L1,
                              L10,L2,L2,L1,
                              L10,L2,L2,L1,
                              L10,L2,L2,L1,
                              L10,L2,L2,L1,
                              L10,L5,L1,
                              "\302",L10,L5,
                              "\302",L10,L5,
                              "\302",L10,L5,
                              "\302",L10,L5,
                              "\302",L10,L5,
                              "\277",
                '\033015'
                PRINT 
                    COLUMN 001,"|",
                    COLUMN 019,"TOTAL DE NSS UNICOS : ",reg[i].cont_nss_uni USING "#####",
                    COLUMN 165,"|",
                    COLUMN 166,reg_s1[i].viv97 +
                               reg_s3[i].viv97 +
                               reg_s4[i].viv97 +
                               reg_s5[i].viv97 +
                               reg_s2[i].viv97    USING "#######&.&&&&&&",
                    COLUMN 181,"|",
                    COLUMN 182,reg_s1[i].viv92 +
                               reg_s3[i].viv92 +
                               reg_s4[i].viv92 +
                               reg_s5[i].viv92 +
                               reg_s2[i].viv92    USING "#######&.&&&&&&",
                    COLUMN 197,"|",
                    COLUMN 213,"|",
                    COLUMN 214,reg_s1[i].viv72 +
                               reg_s3[i].viv72 +
                               reg_s4[i].viv72 +
                               reg_s5[i].viv72 +
                               reg_s2[i].viv72    USING "#######&.&&&&&&",
                    COLUMN 229,"|",
                    COLUMN 245,"|",
                 '\033015'

                PRINT COLUMN 1,"\300",L10,L1,
                              L10,L10,L10,L5,L2,L2,L5,L1,L5,L2,L2,L1,
                              L5,L1,
                              L10,L2,L2,L1,
                              L10,L2,L2,L1,
                              L10,L2,L2,L1,
                              L10,L2,L2,L1,
                              L10,L2,L2,L1,
                              L10,L5,L1,
                              "\301",L10,L5,
                              "\301",L10,L5,
                              "\301",L10,L5,
                              "\301",L10,L5,
                              "\301",L10,L5,
                              "\331",
                     '\033015'
                SKIP 1 LINE
            END IF 
        END FOR
        SKIP 2 LINES 

      --------------------------------------------------
      -- IMPRIME TOTAL A LIQUIDAR SIEFORE PARA SIEFORE 1
      --------------------------------------------------
           IF reg_s1[8].tot > 0 THEN
      
	             PRINT COLUMN 1,"\332",L10,L1,
	                            "\302",L10,L10,L10,L5,L5,L5,L5,L1,
	                            "\302",L2,L1,     
	                            "\302",L10,L5,
	                            "\302",L10,L5,
	                            "\302",L10,L5,
	                            "\302",L10,L5,
	                            "\302",L10,L5,
	                            "\302",L10,L5,
	                            "\302",L10,L5,
	                            "\302",L10,L5,
	                            "\302",L10,L5,
	                            "\302",L10,L5,
	                            "\302",L10,L5,
	                            "\277",
                         '\033015'
            PRINT
                 COLUMN 001,"|",reg_s1[8].tot USING "#######"        ,
                 COLUMN 013,"|",
                 COLUMN 019,"TOTAL LIQUIDADO:  ",
                 COLUMN 065,"|",
                 COLUMN 066,"SB1",
                 COLUMN 069,"|",
		 COLUMN 070,reg_s1[8].reten   USING "#######&.&&&&&&",
                 COLUMN 085,"|",
                 COLUMN 086,reg_s1[8].bruto   USING "#######&.&&&&&&",
                 COLUMN 100,"|",
                 COLUMN 101,reg_s1[8].a_pagar USING "#######&.&&&&&&",
                 COLUMN 115,"|",
                 COLUMN 116,reg_s1[8].r97     USING "#######&.&&&&&&",
                 COLUMN 130,"|",
                 COLUMN 131,reg_s1[8].cv      USING "#######&.&&&&&&",
                 COLUMN 145,"|",
                 COLUMN 146,reg_s1[8].cs      USING "#######&.&&&&&&",
                 COLUMN 162,"|",
                 COLUMN 163,reg_s1[8].viv97   USING "#######&.&&&&&&",
                 COLUMN 177,"|",
                 COLUMN 178,reg_s1[8].viv92   USING "#######&.&&&&&&",
                 COLUMN 193,"|",
                 COLUMN 194,reg_s1[8].r92     USING "#######&.&&&&&&",
                 COLUMN 209,"|",
                 COLUMN 210,reg_s1[8].viv72   USING "#######&.&&&&&&",
                 COLUMN 225,"|",
		 COLUMN 226,reg_s1[8].rcv     USING "#######&.&&&&&&",
                 COLUMN 245,"|",
                 '\033015'

             PRINT COLUMN 1,"\300",L10,L1,
                            "\301",L10,L10,L10,L5,L5,L5,L5,L1,
                            "\301",L2,L1,     
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\331",
               '\033015'
        END IF

        --------------------------------------------------
        -- IMPRIME TOTAL A LIQUIDAR SIEFORE PARA SIEFORE 2
        --------------------------------------------------
        IF reg_s2[8].tot > 0 THEN

	          PRINT COLUMN 1,"\332",L10,L1,
	                         "\302",L10,L10,L10,L5,L5,L5,L5,L1,
	                         "\302",L2,L1,     
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\277",
                   '\033015'
            PRINT
                 COLUMN 001,"|",reg_s2[8].tot USING "#######"        ,
                 COLUMN 013,"|",
                 COLUMN 019,"TOTAL LIQUIDADO:  ",
                 COLUMN 065,"|",
                 COLUMN 066,"SB2",
                 COLUMN 069,"|",
	         COLUMN 070,reg_s2[8].reten   USING "#######&.&&&&&&",
                 COLUMN 085,"|",
                 COLUMN 086,reg_s2[8].bruto   USING "#######&.&&&&&&",
                 COLUMN 100,"|",
                 COLUMN 101,reg_s2[8].a_pagar USING "#######&.&&&&&&",
                 COLUMN 115,"|",
                 COLUMN 116,reg_s2[8].r97     USING "#######&.&&&&&&",
                 COLUMN 130,"|",
                 COLUMN 131,reg_s2[8].cv      USING "#######&.&&&&&&",
                 COLUMN 145,"|",
                 COLUMN 146,reg_s2[8].cs      USING "#######&.&&&&&&",
                 COLUMN 162,"|",
                 COLUMN 163,reg_s2[8].viv97   USING "#######&.&&&&&&",
                 COLUMN 177,"|",
                 COLUMN 178,reg_s2[8].viv92   USING "#######&.&&&&&&",
                 COLUMN 193,"|",
                 COLUMN 194,reg_s2[8].r92     USING "#######&.&&&&&&",
                 COLUMN 209,"|",
                 COLUMN 210,reg_s2[8].viv72   USING "#######&.&&&&&&",
                 COLUMN 225,"|",
		 COLUMN 226,reg_s2[8].rcv     USING "#######&.&&&&&&",
                 COLUMN 245,"|",
            '\033015'

            PRINT COLUMN 1,"\300",L10,L1,
                           "\301",L10,L10,L10,L5,L5,L5,L5,L1,
                           "\301",L2,L1,     
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\331",
             '\033015'
        END IF
        --------------------------------------------------
        -- IMPRIME TOTAL A LIQUIDAR SIEFORE PARA SIEFORE 3
        --------------------------------------------------
        IF reg_s3[8].tot > 0 THEN

	          PRINT COLUMN 1,"\332",L10,L1,
	                         "\302",L10,L10,L10,L5,L5,L5,L5,L1,
	                         "\302",L2,L1,     
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\277",
                  '\033015'
            PRINT
                 COLUMN 001,"|",reg_s3[8].tot USING "#######"        ,
                 COLUMN 013,"|",
                 COLUMN 019,"TOTAL LIQUIDADO:  ",
                 COLUMN 065,"|",
                 COLUMN 066,"SB3",
                 COLUMN 069,"|",
	         COLUMN 070,reg_s3[8].reten   USING "#######&.&&&&&&",
                 COLUMN 085,"|",
                 COLUMN 086,reg_s3[8].bruto   USING "#######&.&&&&&&",
                 COLUMN 100,"|",
                 COLUMN 101,reg_s3[8].a_pagar USING "#######&.&&&&&&",
                 COLUMN 115,"|",
                 COLUMN 116,reg_s3[8].r97     USING "#######&.&&&&&&",
                 COLUMN 130,"|",
                 COLUMN 131,reg_s3[8].cv      USING "#######&.&&&&&&",
                 COLUMN 145,"|",
                 COLUMN 146,reg_s3[8].cs      USING "#######&.&&&&&&",
                 COLUMN 162,"|",
                 COLUMN 163,reg_s3[8].viv97   USING "#######&.&&&&&&",
                 COLUMN 177,"|",
                 COLUMN 178,reg_s3[8].viv92   USING "#######&.&&&&&&",
                 COLUMN 193,"|",
                 COLUMN 194,reg_s3[8].r92     USING "#######&.&&&&&&",
                 COLUMN 209,"|",
                 COLUMN 210,reg_s3[8].viv72   USING "#######&.&&&&&&",
                 COLUMN 225,"|",
		 COLUMN 226,reg_s3[8].rcv     USING "#######&.&&&&&&",
                 COLUMN 245,"|",
            '\033015'

            PRINT COLUMN 1,"\300",L10,L1,
                           "\301",L10,L10,L10,L5,L5,L5,L5,L1,
                           "\301",L2,L1,     
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\331",
                 '\033015'
        END IF
        --------------------------------------------------
        -- IMPRIME TOTAL A LIQUIDAR SIEFORE PARA SIEFORE 4
        --------------------------------------------------
        IF reg_s4[8].tot > 0 THEN

	          PRINT COLUMN 1,"\332",L10,L1,
	                         "\302",L10,L10,L10,L5,L5,L5,L5,L1,
	                         "\302",L2,L1,     
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\277",
                  '\033015'
            PRINT
                 COLUMN 001,"|",reg_s4[8].tot USING "#######"        ,
                 COLUMN 013,"|",
                 COLUMN 019,"TOTAL LIQUIDADO:  ",
                 COLUMN 065,"|",
                 COLUMN 066,"SB4",
                 COLUMN 069,"|",
	         COLUMN 070,reg_s4[8].reten   USING "#######&.&&&&&&",
                 COLUMN 085,"|",
                 COLUMN 086,reg_s4[8].bruto   USING "#######&.&&&&&&",
                 COLUMN 100,"|",
                 COLUMN 101,reg_s4[8].a_pagar USING "#######&.&&&&&&",
                 COLUMN 115,"|",
                 COLUMN 116,reg_s4[8].r97     USING "#######&.&&&&&&",
                 COLUMN 130,"|",
                 COLUMN 131,reg_s4[8].cv      USING "#######&.&&&&&&",
                 COLUMN 145,"|",
                 COLUMN 146,reg_s4[8].cs      USING "#######&.&&&&&&",
                 COLUMN 162,"|",
                 COLUMN 163,reg_s4[8].viv97   USING "#######&.&&&&&&",
                 COLUMN 177,"|",
                 COLUMN 178,reg_s4[8].viv92   USING "#######&.&&&&&&",
                 COLUMN 193,"|",
                 COLUMN 194,reg_s4[8].r92     USING "#######&.&&&&&&",
                 COLUMN 209,"|",
                 COLUMN 210,reg_s4[8].viv72   USING "#######&.&&&&&&",
                 COLUMN 225,"|",
		 COLUMN 226,reg_s4[8].rcv     USING "#######&.&&&&&&",
                 COLUMN 245,"|",
            '\033015'

            PRINT COLUMN 1,"\300",L10,L1,
                           "\301",L10,L10,L10,L5,L5,L5,L5,L1,
                           "\301",L2,L1,     
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\331",
            '\033015'
        END IF
        --------------------------------------------------
        -- IMPRIME TOTAL A LIQUIDAR SIEFORE PARA SIEFORE 5
        --------------------------------------------------
        IF reg_s5[8].tot > 0 THEN

	          PRINT COLUMN 1,"\332",L10,L1,
	                         "\302",L10,L10,L10,L5,L5,L5,L5,L1,
	                         "\302",L2,L1,     
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\302",L10,L5,
	                         "\277",
                   '\033015'
            PRINT
                 COLUMN 001,"|",reg_s5[8].tot USING "#######"        ,
                 COLUMN 013,"|",
                 COLUMN 019,"TOTAL LIQUIDADO:  ",
                 COLUMN 065,"|",
                 COLUMN 066,"SB5",
                 COLUMN 069,"|",
	         COLUMN 070,reg_s5[8].reten   USING "#######&.&&&&&&",
                 COLUMN 085,"|",
                 COLUMN 086,reg_s5[8].bruto   USING "#######&.&&&&&&",
                 COLUMN 100,"|",
                 COLUMN 101,reg_s5[8].a_pagar USING "#######&.&&&&&&",
                 COLUMN 115,"|",
                 COLUMN 116,reg_s5[8].r97     USING "#######&.&&&&&&",
                 COLUMN 130,"|",
                 COLUMN 131,reg_s5[8].cv      USING "#######&.&&&&&&",
                 COLUMN 145,"|",
                 COLUMN 146,reg_s5[8].cs      USING "#######&.&&&&&&",
                 COLUMN 162,"|",
                 COLUMN 163,reg_s5[8].viv97   USING "#######&.&&&&&&",
                 COLUMN 177,"|",
                 COLUMN 178,reg_s5[8].viv92   USING "#######&.&&&&&&",
                 COLUMN 193,"|",
                 COLUMN 194,reg_s5[8].r92     USING "#######&.&&&&&&",
                 COLUMN 209,"|",
                 COLUMN 210,reg_s5[8].viv72   USING "#######&.&&&&&&",
                 COLUMN 225,"|",
		 COLUMN 226,reg_s5[8].rcv     USING "#######&.&&&&&&",
                 COLUMN 245,"|",
            '\033015'

            PRINT COLUMN 1,"\300",L10,L1,
                           "\301",L10,L10,L10,L5,L5,L5,L5,L1,
                           "\301",L2,L1,     
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\331",
            '\033015'
        END IF
        ---------------------------------------------------
        -- DETALLE DE TOTALES DE NSS UNICOS Y SUMA DE VIVIENDA
        ---------------------------------------------------
        PRINT COLUMN 1,"\332",L10,L1,
                      L10,L10,L10,L5,L2,L2,L5,L1,L5,L2,L2,L1,
                      L5,L1,
                      L10,L2,L2,L1,
                      L10,L2,L2,L1,
                      L10,L2,L2,L1,
                      L10,L2,L2,L1,
                      L10,L2,L2,L1,
                      L10,L5,L1,
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\277",
            '\033015'
        PRINT 
            COLUMN 001,"|",
            COLUMN 019,"TOTAL DE NSS UNICOS : ",cont_nss_fin USING "#####",
            COLUMN 165,"|",
            COLUMN 166,reg_s1[8].viv97 +
                       reg_s3[8].viv97 +
                       reg_s4[8].viv97 +
                       reg_s5[8].viv97 +
                       reg_s2[8].viv97    USING "#######&.&&&&&&",
            COLUMN 181,"|",
            COLUMN 182,reg_s1[8].viv92 +
                       reg_s3[8].viv92 +
                       reg_s4[8].viv92 +
                       reg_s5[8].viv92 +
                       reg_s2[8].viv92    USING "#######&.&&&&&&",
            COLUMN 197,"|",
            COLUMN 213,"|",
            COLUMN 214,reg_s1[8].viv72 +
                       reg_s3[8].viv72 +
                       reg_s4[8].viv72 +
                       reg_s5[8].viv72 +
                       reg_s2[8].viv72    USING "#######&.&&&&&&",
            COLUMN 229,"|",
            COLUMN 245,"|",
            '\033015'

        PRINT COLUMN 1,"\300",L10,L1,
                      L10,L10,L10,L5,L2,L2,L5,L1,L5,L2,L2,L1,
                      L5,L1,
                      L10,L2,L2,L1,
                      L10,L2,L2,L1,
                      L10,L2,L2,L1,
                      L10,L2,L2,L1,
                      L10,L2,L2,L1,
                      L10,L5,L1,
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\331",
            '\033015'
                             
END REPORT

#---------------------------------------------------------------------------#
# f_genera_tmp_cuenta : Genera el extracto de discuenta con los movimientos #
#                       a utilizarse usando las fechas capturadas           #
#---------------------------------------------------------------------------#
FUNCTION f_genera_tmp_cuenta(pc_where)

    DEFINE 
        pc_where            CHAR(250)

    DEFINE lr_cuenta RECORD LIKE dis_cuenta.*
    
    DEFINE
        lc_query            CHAR(1000)
    
    -- -----------------------------------------------------------------------------

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_dat_dis_cta
    WHENEVER ERROR STOP

    SELECT *
    FROM   dis_cuenta
    WHERE  1 = 0
    INTO TEMP tmp_dat_dis_cta

    LET lc_query = " SELECT *           ", 
                   " FROM dis_cuenta    ",
                   " WHERE ",where_clause CLIPPED,
                   " AND tipo_movimiento IN (10,820,830,840,850,860,880,825) "

    PREPARE prp_cuenta FROM lc_query
    DECLARE cur_cuenta CURSOR FOR prp_cuenta

    FOREACH cur_cuenta INTO lr_cuenta.*
    
        INSERT INTO tmp_dat_dis_cta
        VALUES (lr_cuenta.*)
    END FOREACH
 
    CREATE INDEX cta_data_01
    ON tmp_dat_dis_cta (tipo_movimiento, consecutivo_lote, nss)
    
    CALL f_elimina_disp_E73()

END FUNCTION

#---------------------------------------------------------------------------#
# f_elimina_disp_E73 : Elimina los registros de dis_cuenta correspondientes #
#                      a la liquidacion de las disposiciones tipo E regimen #
#                      73 cuya fecha de resolucion sea mayor al 12 de enero #
#                      de 2012                                              #
#---------------------------------------------------------------------------#
FUNCTION f_elimina_disp_E73()

    DEFINE lr_elimina RECORD
        nss         LIKE dis_cuenta.nss             ,
        folio       LIKE dis_cuenta.folio           ,
        consecutivo LIKE dis_cuenta.consecutivo_lote
    END RECORD

    DECLARE cur_elimina CURSOR FOR
       SELECT nss               ,
              folio             ,
              consecutivo_lote
       FROM   tmp_dat_dis_cta
       WHERE  tipo_movimiento  = 830
       AND    subcuenta       IN (4,8)
       AND    nss IN (SELECT B.nss
                      FROM   tmp_dat_dis_cta  A   ,
                             ret_solicitud_tx B
                      WHERE  A.nss              = B.nss
                      AND    A.folio            = B.folio
                      AND    A.consecutivo_lote = B.consecutivo
                      AND    A.tipo_movimiento  = 830
                      AND    B.tipo_retiro      = "E"
                      AND    B.regimen          = 73
                      AND    A.subcuenta        IN (4,8)
                      AND    B.fecha_resolucion > gdt_cambio_infonavit
                     )

    FOREACH cur_elimina INTO lr_elimina.*
        DELETE
        FROM   tmp_dat_dis_cta
        WHERE  nss              = lr_elimina.nss
        AND    folio            = lr_elimina.folio
        AND    consecutivo_lote = lr_elimina.consecutivo
        AND    subcuenta        IN (4,8)
    END FOREACH

END FUNCTION

FUNCTION habil_siguiente(diaActual,numDiaHabil)
#hs--------------------------------------------
   DEFINE 
       diaTmp	                ,
       diaHabilSig	        ,
       diaActual	        DATE
   
   DEFINE
       cont_1                   ,
       numDiaHabil              ,
       contador	                ,
       diaSemana	        ,
       feriado	                ,
       finSemana	        SMALLINT

   LET cont_1      = 0
   LET diaHabilSig = diaActual

   WHILE TRUE
       LET feriado   = 0
       LET finSemana = 0
       LET diaSemana = WEEKDAY(diaHabilSig)  

       IF diaSemana = 0 OR diaSemana = 6 THEN
      	   LET finSemana = 1
       ELSE
           SELECT *
           FROM   tab_feriado 
           WHERE  feria_fecha = diaHabilSig
    	
           IF STATUS <> NOTFOUND THEN
               LET feriado = 1
           END IF 
       END IF
		
       IF feriado = 1 OR finSemana = 1 THEN
           LET diaHabilSig = diaHabilSig + 1 UNITS DAY
       ELSE
           LET cont_1 = cont_1 + 1
           IF cont_1 = numDiaHabil THEN
               EXIT WHILE
           ELSE
               LET diaHabilSig = diaHabilSig + 1 UNITS DAY
           END IF
       END IF
   END WHILE

   RETURN diaHabilSig
END FUNCTION

