###########################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                       #
#Propietario       => E.F.P.                                              #
#Programa RETL821  => Reporte Solicitudes de Disposicion de Recursos(op_5)#
#Fecha             => 20 DE NOVIEMBRE DE 2007                             #
#Por               => MIREYA TOLEDO PANIAGUA                              #
#Sistema           => RET                                                 #
###########################################################################
DATABASE safre_af
GLOBALS
    DEFINE 
        w_codigo_afore     LIKE tab_afore_local.codigo_afore 

    DEFINE 
        g_param_dis        RECORD LIKE seg_modulo.*

    DEFINE #glo #date
        f_opera            ,
        f_trans            ,
        HOY                DATE
   
    DEFINE #glo #char
        aux_pausa          CHAR(1)    ,
        enter              CHAR(0001) ,
        g_usuario          CHAR(0008) ,
        G_LISTA            CHAR(0300) ,
        G_IMPRE            CHAR(0300) ,
        impresion          CHAR(0300) ,
        where_clause       CHAR(0250) ,
        select_stmt5       CHAR(1000) ,
	      select_stmt6       CHAR(1000) ,
	      select_montos      CHAR(1000) ,
	      folio_capturado    CHAR(40)
	
    DEFINE v RECORD
        fecha_corte        DATE
    END RECORD

    DEFINE vparametros RECORD
        vfolio             INTEGER ,
        vfecha             DATE
    END RECORD

    DEFINE 
	      precio_de_liq     DECIMAL(19,14)	

    DEFINE    sw9         SMALLINT
END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS PROMPT LINE LAST ,
            INPUT WRAP       ,
            ACCEPT KEY control-o

    CALL startlog("RETL821.log")
    CALL inicio()     #i
    CALL Consulta()   #c

    PROMPT "  PROCESO FINALIZADO...  <ENTER>  PARA SALIR " FOR CHAR enter
END MAIN


FUNCTION inicio()
#i---------------
    SELECT codigo_afore   ,
	         USER
    INTO   w_codigo_afore ,
	         g_usuario
    FROM   tab_afore_local

    SELECT ruta_listados
    INTO   g_param_dis.ruta_listados
    FROM   seg_modulo 
    WHERE  modulo_cod='ret'

    LET HOY     = TODAY
 
END FUNCTION

#---------------------------------------------------------------------------
# FUNCTION QUE ABRE LA PANTALLA DE CAPTURA
#---------------------------------------------------------------------------
FUNCTION Consulta()
#c-----------------
    DEFINE #loc #char
        verifica              CHAR(0070)
                              
    DEFINE #loc #integer      
        cuenta_regis          INTEGER
                              
    DEFINE #loc #date         
        l_fecha               DATE   

    CLEAR SCREEN

    OPEN WINDOW ventana_21 AT 4,4 WITH FORM "RETL8211" ATTRIBUTE( BORDER)
    DISPLAY "RETL821                       (Ctrl-C) Salir                (ESC) Ejecutar     " AT 3,1 ATTRIBUTE(REVERSE,green)

    LET int_flag = FALSE

    CONSTRUCT BY NAME where_clause ON dis_provision.folio,
                                      dis_provision.fecha_conversion 
        ON KEY(ESC)
            LET int_flag =FALSE  
            EXIT  CONSTRUCT

        ON KEY(CONTROL-C)
            LET int_flag = TRUE
            EXIT CONSTRUCT 
    END CONSTRUCT

    LET folio_capturado = where_clause[15,27]
    IF folio_capturado = "fecha_convers" THEN
       LET folio_capturado = " "
    END IF
    --
    --display "folio_capturado ", folio_capturado
    --sleep 5
    --

    
    IF int_flag THEN
        PROMPT " PROCESO CANCELADO... <ENTER> PARA SALIR " 
        FOR CHAR enter
        CLOSE WINDOW ventana_21
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
#ac---------------------------------
    DEFINE
	      where_clause          CHAR(250) ,
	      v_permisos            CHAR(110) 
	      
    DEFINE 
        v_cont_reg            INTEGER, 
        v_sief                INTEGER

    DEFINE l_record RECORD
        tipo_retiro           LIKE ret_solicitud_tx.tipo_retiro    ,
        nss                   LIKE dis_provision.nss               ,
        folio                 LIKE dis_provision.folio             ,
        consecutivo_lote      LIKE dis_provision.consecutivo_lote  ,
        siefore               LIKE dis_provision.siefore           
    END RECORD

    DEFINE lr_montos   RECORD
        acciones_ret97        LIKE ret_monto_siefore.acciones_ret97 ,
        acciones_cv           LIKE ret_monto_siefore.acciones_cv    ,
        acciones_cs           LIKE ret_monto_siefore.acciones_cs    ,
        acciones_ret92        LIKE ret_monto_siefore.acciones_ret92 ,
        siefore               LIKE ret_monto_siefore.siefore        
    END RECORD

    DEFINE lr_sol_tx    RECORD 
        saldo_viv97           LIKE ret_solicitud_tx.saldo_viv97     ,
        saldo_viv97_p         DECIMAL(16,6)                         ,
        saldo_viv92           LIKE ret_solicitud_tx.saldo_viv92     ,
        saldo_viv92_p         DECIMAL(16,6)                         ,
        saldo_viv72           LIKE ret_solicitud_tx.saldo_viv72     ,
        tipo_seguro           LIKE ret_solicitud_tx.tipo_seguro     ,
        tipo_pension          LIKE ret_solicitud_tx.tipo_pension    ,
        regimen               LIKE ret_solicitud_tx.regimen         ,
        tipo_prestacion       LIKE ret_solicitud_tx.tipo_prestacion
    END RECORD

    DEFINE lr_sol_rx    RECORD 
        diag_registro         LIKE ret_solicitud_rx.diag_registro,
        estado_sub_viv        LIKE ret_solicitud_rx.estado_sub_viv
    END RECORD

    DEFINE 
        cont_siefore          SMALLINT 
        
    
    LET G_IMPRE = g_param_dis.ruta_listados CLIPPED,"/",
                  HOY USING "YYYYMMDD",".821"


    
    START REPORT rpt_cuenta_imp TO G_IMPRE

    ERROR " PROCESANDO... "
    
    --------------------------------------------------
    --SELECCION DE DATOS REQUERIDOS DE dis_provision
    --------------------------------------------------
    --
    --display "where_clause ", where_clause
    --sleep 10
    --
    LET select_stmt5 = " SELECT CASE WHEN tipo_movimiento = 820 THEN 'D'      ",
                       "             WHEN tipo_movimiento = 830 THEN 'E'      ",
                       "             WHEN tipo_movimiento = 840 THEN 'F'      ",
                       "             WHEN tipo_movimiento = 850 THEN 'G'      ",
                       "             WHEN tipo_movimiento = 860 THEN 'H'      ",
                       "             WHEN tipo_movimiento = 880 THEN 'J' END, ",
                       "        nss,folio,consecutivo_lote, siefore ",
                       " FROM  dis_provision                                  ",
                       " WHERE ",where_clause CLIPPED,
                       " AND   subcuenta IN (1,2,4,5,6,7,8,9)                 ",  
                       " AND   tipo_movimiento IN (820,830,840,850,860,880)   ",
                       --" AND   siefore         IN (1,2,11)                    ",       
                       " AND   siefore         IN (1,2,3,4,5,11)                    ",       
                       " GROUP BY 1,2,3,4,5 ",
                       " ORDER BY 1,2,3,4,5 "

    PREPARE cust_stmt1 FROM select_stmt5
    DECLARE rept_cur CURSOR FOR cust_stmt1
        
    LET v_cont_reg  =  0
    FOREACH rept_cur INTO l_record.tipo_retiro      ,
                          l_record.nss              ,
                          l_record.folio            , 
                          l_record.consecutivo_lote ,
                          l_record.siefore          

        ERROR " PROCESANDO REGISTROS... "
        
        LET v_cont_reg = v_cont_reg + 1

        IF where_clause[01,30] = "dis_provision.fecha_conversion" THEN
           LET l_record.folio = 0
        END IF

        IF l_record.folio = 0 THEN
           LET f_opera = where_clause[33,42]
        END IF


        --SELECCION DE DATOS REQUERIDOS DE ret_monto_siefore y ret_solicitud_tx
        
        ERROR " SELECCIONANDO MONTOS... "
        --
        IF folio_capturado = " "  OR folio_capturado IS NULL THEN

	      LET select_montos = "  SELECT                                 ",
	                          "  A.acciones_ret97                 ,     ",
	                          "  A.acciones_cv                    ,     ",
                                  "  A.acciones_cs                    ,     ",
                                  "  A.acciones_ret92                 ,     ",
                                  "  A.siefore                              ",
                            "  FROM   ret_monto_siefore A             ",
                            "  WHERE  A.nss             = ", l_record.nss CLIPPED,                  
                            "  AND    A.consecutivo     = ", l_record.consecutivo_lote CLIPPED,                  
                            "  AND    A.tipo_operacion  = 5 ",
		            "  AND    A.siefore         = ", l_record.siefore  CLIPPED
                       --" GROUP BY 1,2,3,4,5 ",
                       --" ORDER BY 1,2,3,4,5 "
         ELSE
	      LET select_montos = "  SELECT                                 ",
	                          "  A.acciones_ret97                 ,     ",
	                          "  A.acciones_cv                    ,     ",
                                  "  A.acciones_cs                    ,     ",
                                  "  A.acciones_ret92                 ,     ",
                                  "  A.siefore                              ",
                            "  FROM   ret_monto_siefore A             ",
                            "  WHERE  A.nss             = ", l_record.nss CLIPPED,                  
                            "  AND    A.consecutivo     = ", l_record.consecutivo_lote CLIPPED,                  
                            "  AND    A.tipo_operacion  = 5 ",
		            "  AND     A.siefore        = ", l_record.siefore  CLIPPED, 
                            "  AND    A.", folio_capturado CLIPPED
                       --" GROUP BY 1,2,3,4,5 ",
                       --" ORDER BY 1,2,3,4,5 "
         END IF
        --
        LET sw9 = 0
        --
        LET select_montos = select_montos CLIPPED
	PREPARE cur_mont FROM select_montos
        DECLARE cur_montos CURSOR FOR cur_mont      

        FOREACH cur_montos INTO lr_montos.acciones_ret97 ,
                                lr_montos.acciones_cv    ,
                                lr_montos.acciones_cs    ,
                                lr_montos.acciones_ret92 ,
                                lr_montos.siefore          
                  --
                  --display "acciones 97 ",lr_montos.acciones_ret97
                  --display "acciones cv ",lr_montos.acciones_cv     
                  --display "acciones cs ",lr_montos.acciones_cs     
                  --display "acciones 92 ",lr_montos.acciones_ret92  
                  --display "siefore     ",lr_montos.siefore         
                  --sleep 5 
                  --

                  LET sw9 = sw9 + 1
	          --VALIDACION PARA DETERMINAR DE CUAL SIEFORE OBTENER LOS MONTOS
	          IF l_record.siefore = 1 THEN
	              IF  lr_montos.acciones_ret97 = 0
	              AND lr_montos.acciones_cv    = 0
	              AND lr_montos.acciones_cs    = 0
	              AND lr_montos.acciones_ret92 = 0
	              AND lr_montos.siefore        = 0  
	              THEN
	                  LET v_sief = 0
	                  EXIT FOREACH
	              ELSE
	                  LET v_sief = 1
	              END IF
	          END IF

	          IF l_record.siefore = 2 THEN
	              IF  lr_montos.acciones_ret97 = 0
	              AND lr_montos.acciones_cv    = 0
	              AND lr_montos.acciones_cs    = 0
	              AND lr_montos.acciones_ret92 = 0
	              AND lr_montos.siefore        = 0  
	              THEN
	                  LET v_sief = 0
	                  EXIT FOREACH
	              ELSE
	                  LET v_sief = 2
	              END IF
	          END IF
                  --
	          IF l_record.siefore = 3 THEN
	              IF  lr_montos.acciones_ret97 = 0
	              AND lr_montos.acciones_cv    = 0
	              AND lr_montos.acciones_cs    = 0
	              AND lr_montos.acciones_ret92 = 0
	              AND lr_montos.siefore        = 0  
	              THEN
	                  LET v_sief = 0
	                  EXIT FOREACH
	              ELSE
	                  LET v_sief = 3
	              END IF
	          END IF
	          IF l_record.siefore = 4 THEN
	              IF  lr_montos.acciones_ret97 = 0
	              AND lr_montos.acciones_cv    = 0
	              AND lr_montos.acciones_cs    = 0
	              AND lr_montos.acciones_ret92 = 0
	              AND lr_montos.siefore        = 0  
	              THEN
	                  LET v_sief = 0
	                  EXIT FOREACH
	              ELSE
	                  LET v_sief = 4
	              END IF
	          END IF
	          IF l_record.siefore = 5 THEN
	              IF  lr_montos.acciones_ret97 = 0
	              AND lr_montos.acciones_cv    = 0
	              AND lr_montos.acciones_cs    = 0
	              AND lr_montos.acciones_ret92 = 0
	              AND lr_montos.siefore        = 0  
	              THEN
	                  LET v_sief = 0
	                  EXIT FOREACH
	              ELSE
	                  LET v_sief = 5
	              END IF
	          END IF
                  --
	          IF l_record.siefore = 11 THEN
	              IF  lr_montos.acciones_ret97 = 0
	                  AND lr_montos.acciones_cv    = 0
	                  AND lr_montos.acciones_cs    = 0
	                  AND lr_montos.acciones_ret92 = 0
	                  AND lr_montos.siefore        = 0  
	                  THEN
	                     LET v_sief = 0
	                     EXIT FOREACH
	                  ELSE
	                     LET v_sief = 11
	              END IF
	          END IF


            --PARA LOS MONTOS DE VIVIENDA
        IF folio_capturado = " "  OR folio_capturado IS NULL THEN

            LET select_stmt6 = " SELECT                                  ",
	                       " B.saldo_viv97                    ,      ",
                               " B.saldo_viv97 * C.precio_del_dia ,      ",
                               " B.saldo_viv92                    ,      ",
                               " B.saldo_viv92 * C.precio_del_dia ,      ",
                               " B.saldo_viv72                    ,      ",
                               " B.tipo_seguro                    ,      ",
  			       " B.tipo_pension                   ,      ",
                               " B.regimen                        ,      ",
                               " B.tipo_prestacion                       ",
                               " FROM  ret_monto_siefore A ,             ",
                               "       ret_solicitud_tx  B ,             ",
			                         "       glo_valor_accion  C               ",
                               " WHERE B.nss             = ", l_record.nss CLIPPED,                  
                               " AND   B.consecutivo     = ", l_record.consecutivo_lote CLIPPED,                  
		                           " AND   A.siefore         = ", l_record.siefore  CLIPPED, 
                               " AND   A.nss             = B.nss               ",                           
			                         " AND   C.fecha_valuacion = B.fecha_valor_viv   ",       
                               " AND   C.codigo_siefore  = ", v_sief CLIPPED
        ELSE
            LET select_stmt6 = " SELECT                                  ",
	                       " B.saldo_viv97                    ,      ",
                               " B.saldo_viv97 * C.precio_del_dia ,      ",
                               " B.saldo_viv92                    ,      ",
                               " B.saldo_viv92 * C.precio_del_dia ,      ",
                               " B.saldo_viv72                    ,      ",
                               " B.tipo_seguro                    ,      ",
  			       " B.tipo_pension                   ,      ",
                               " B.regimen                        ,      ",
                               " B.tipo_prestacion                       ",
                               " FROM  ret_monto_siefore A ,             ",
                               "       ret_solicitud_tx  B ,             ",
			                         "       glo_valor_accion  C               ",
                               " WHERE B.nss             = ", l_record.nss CLIPPED,                  
                               " AND   B.consecutivo     = ", l_record.consecutivo_lote CLIPPED,                  
		                           " AND   A.siefore         = ", l_record.siefore  CLIPPED, 
                               " AND   A.nss             = B.nss               ",                           
			                         " AND   C.fecha_valuacion = B.fecha_valor_viv   ",       
                               " AND   C.codigo_siefore  = ", v_sief CLIPPED,
                            "  AND    A.", folio_capturado CLIPPED 
        END IF
            
            
            PREPARE cur_mont_viv FROM select_stmt6
            DECLARE rept_cur_mv CURSOR FOR cur_mont_viv
            

            FOREACH rept_cur_mv INTO lr_sol_tx.saldo_viv97     ,            
			             lr_sol_tx.saldo_viv97_p   ,            
		                     lr_sol_tx.saldo_viv92     ,            
                                     lr_sol_tx.saldo_viv92_p   ,            
                                     lr_sol_tx.saldo_viv72     ,            
                                     lr_sol_tx.tipo_seguro     ,            
                                     lr_sol_tx.tipo_pension    ,            
                                     lr_sol_tx.regimen         ,            
                                     lr_sol_tx.tipo_prestacion              

            END FOREACH --foreach de montos de vivienda (rept_cur_mv)
            
            --VALIDACIONES
            IF lr_sol_tx.tipo_seguro IS NULL THEN
               LET lr_sol_tx.saldo_viv72     = 0.0
               LET lr_sol_tx.tipo_seguro     = '  '
               LET lr_sol_tx.tipo_pension    = '  '
               LET lr_sol_tx.regimen         = '  '
               LET lr_sol_tx.tipo_prestacion = '  '
            END IF  
            
            ERROR " GENERANDO DETALLE  "

            SELECT COUNT(UNIQUE siefore)
            INTO   cont_siefore
            FROM   dis_provision
            WHERE  nss              = l_record.nss
            AND    consecutivo_lote = l_record.consecutivo_lote
            --AND    siefore         IN(1,2)
            AND    siefore         IN(1,2,3,4,5)
            
            --IF cont_siefore = 2 AND l_record.siefore = 2 THEN
            IF cont_siefore > 1 AND sw9  > 1 THEN
                LET lr_sol_tx.saldo_viv97   = 0
                LET lr_sol_tx.saldo_viv97_p = 0
                LET lr_sol_tx.saldo_viv92   = 0
                LET lr_sol_tx.saldo_viv92_p = 0
                LET lr_sol_tx.saldo_viv72   = 0
            END IF     

            OUTPUT TO REPORT rpt_cuenta_imp(l_record.*  ,
                                            lr_montos.* ,
                                            lr_sol_tx.* ,
                                            lr_sol_rx.* ,    
                                            f_opera )
        END FOREACH --foreach de montos    
    END FOREACH  -- foreach de dis_provision

    IF ( v_cont_reg <= 0 ) THEN
         ERROR "   NO EXISTEN REGISTROS PARA ESTE CRITERIO...   "
         SLEEP 4
         ERROR " "
         EXIT PROGRAM
    END IF 

    FINISH REPORT rpt_cuenta_imp

    ERROR ""

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

REPORT rpt_cuenta_imp(l_record,lr_montos,lr_sol_tx,lr_sol_rx,f_opera2)
#rci------------------------------------------------------------------
    DEFINE l_record RECORD
        tipo_retiro           LIKE ret_solicitud_tx.tipo_retiro    ,
        nss                   LIKE dis_provision.nss               ,
        folio                 LIKE dis_provision.folio             ,
        consecutivo_lote      LIKE dis_provision.consecutivo_lote  ,
        siefore               LIKE dis_provision.siefore           
    END RECORD

    DEFINE lr_montos   RECORD
        acciones_ret97        LIKE ret_monto_siefore.acciones_ret97 ,
        acciones_cv           LIKE ret_monto_siefore.acciones_cv    ,
        acciones_cs           LIKE ret_monto_siefore.acciones_cs    ,
        acciones_ret92        LIKE ret_monto_siefore.acciones_ret92 ,
        siefore               LIKE ret_monto_siefore.siefore        
    END RECORD

    DEFINE lr_sol_tx    RECORD 
        saldo_viv97           LIKE ret_solicitud_tx.saldo_viv97     ,
        saldo_viv97_p         DECIMAL(16,2)                         ,
        saldo_viv92           LIKE ret_solicitud_tx.saldo_viv92     ,
        saldo_viv92_p         DECIMAL(16,2)                         ,
        saldo_viv72           LIKE ret_solicitud_tx.saldo_viv72     ,
        tipo_seguro           LIKE ret_solicitud_tx.tipo_seguro     ,
        tipo_pension          LIKE ret_solicitud_tx.tipo_pension    ,
        regimen               LIKE ret_solicitud_tx.regimen         ,
        tipo_prestacion       LIKE ret_solicitud_tx.tipo_prestacion
    END RECORD

    DEFINE lr_sol_rx    RECORD 
        diag_registro         LIKE ret_solicitud_rx.diag_registro,
        estado_sub_viv        LIKE ret_solicitud_rx.estado_sub_viv
    END RECORD

    DEFINE r_datnss  RECORD
        monto72             LIKE dis_provision.monto_en_pesos  ,
        nombre_afore        LIKE ret_transf_rx.nombre_afore    ,
        paterno_afore       LIKE ret_transf_rx.paterno_afore   ,
        materno_afore       LIKE ret_transf_rx.materno_afore   ,
        diag_registro       LIKE ret_transf_rx.diag_registro   ,
        tipo_seguro         LIKE ret_transf_rx.tipo_seguro     ,
        tipo_pension        LIKE ret_transf_rx.tipo_seguro     ,
        regimen             LIKE ret_transf_rx.regimen         ,
        tipo_prestacion     LIKE ret_transf_rx.tipo_prestacion
    END RECORD

    DEFINE #loc #date
        f_opera2            ,
        hoy                 DATE

    DEFINE #loc #char
        nombre_siefore      CHAR(03) ,
        r_nombre            CHAR(60) ,
        r_nom_rep           CHAR(07)

    DEFINE #loc #smallint
        cont_tipo_retiro1   ,
        cont_tipo_retiro2   ,
        cont_tipo_retiro3   ,
        cont_tipo_retiro4   ,
        cont_tipo_retiro5   ,
        ind                 , 
        i                   SMALLINT

    DEFINE rpt_afore RECORD
        codigo_afore        LIKE safre_af:tab_afore_local.codigo_afore ,
        razon_social        LIKE safre_af:tab_afore_local.razon_social
    END RECORD

    DEFINE reg_s1, reg_s2, reg_s3, reg_s4 , reg_s5  ARRAY[6] OF RECORD
         tot               INTEGER       ,
         tipo_ret          CHAR(1)       ,
         total             DECIMAL(16,6) ,
         r97               DECIMAL(16,6) ,
         cv                DECIMAL(16,6) ,
         cs                DECIMAL(16,6) ,
         viv97             DECIMAL(16,6) ,
         viv97p            DECIMAL(16,2) ,
         viv92             DECIMAL(16,6) ,
         viv92p            DECIMAL(16,2) ,
         r92               DECIMAL(16,6) ,
         viv72             DECIMAL(16,2)
    END RECORD

    DEFINE reg             ARRAY[6] OF RECORD
         cont_nss_uni      SMALLINT
    END RECORD  

    DEFINE lr_tot_fin_sb1 ,
           lr_tot_fin_sb2 ,      
           lr_tot_fin_sb3 ,      
           lr_tot_fin_sb4 ,     
           lr_tot_fin_sb5 RECORD
        total             DECIMAL(16,6) ,
        ret97             DECIMAL(16,6) ,
        cv                DECIMAL(16,6) ,
        cs                DECIMAL(16,6) ,
        viv97_p           DECIMAL(16,2) ,
        viv92_p           DECIMAL(16,2) ,
        viv97             DECIMAL(16,6) ,
        viv92             DECIMAL(16,6) ,
        ret92             DECIMAL(16,6) ,
        viv72             DECIMAL(16,2)           
    END RECORD 

    DEFINE 
        encabezado           CHAR(30) ,
        nombre_final         CHAR(50) ,
        L1                   CHAR(01) ,
        L2                   CHAR(02) ,
        L5                   CHAR(05) ,
        L10                  CHAR(10)

    DEFINE 
        cont_nss_fin         ,
        cont_nss_unicos      SMALLINT
        
    OUTPUT
        TOP MARGIN    0
        BOTTOM MARGIN 0
        LEFT MARGIN   0
        --RIGHT MARGIN  200
        --PAGE LENGTH   90
        RIGHT MARGIN  0
        PAGE LENGTH   45
    
    ORDER BY l_record.tipo_retiro ,
             l_record.nss         ,
             lr_montos.siefore    
 
    FORMAT
     --------------------
     FIRST PAGE HEADER
     --------------------
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

        SELECT codigo_afore           ,
               razon_social
        INTO   rpt_afore.codigo_afore , 
               rpt_afore.razon_social
        FROM safre_af:tab_afore_local

        LET hoy       = TODAY
        LET r_nom_rep = "RETL821"

        IF l_record.folio = 0 THEN
            LET f_opera = f_opera2
        ELSE
            SELECT fecha_genera
            INTO   f_opera
            FROM   ret_ctr_envio_lote
            WHERE  folio = l_record.folio
        END IF

        CALL habil_siguiente(f_opera,3) RETURNING f_trans

        IF rpt_afore.codigo_afore = 532 THEN
            LET encabezado = "SUBDIRECCION DE BENEFICIOS"
        ELSE 
            LET encabezado = "   MODULO DE RETIROS      "     
        END IF   

        PRINT COLUMN 067,encabezado,
            '\033015'
        SKIP 1 LINE

        PRINT COLUMN 054,"    REPORTE DE SOLICITUD DE DISPOSICION DE RECURSOS",
            '\033015'
        SKIP 2 LINE

        PRINT COLUMN 002,"AFORE               : ",rpt_afore.codigo_afore
                                                USING "###","  ",
                                                rpt_afore.razon_social CLIPPED,
              COLUMN 126,"PAGINA              :",pageno USING "##",
            '\033015'

        PRINT
        PRINT COLUMN 002,"PROGRAMA            : ",r_nom_rep,
              COLUMN 126,"FECHA VAL. TRANSFER : ",HOY USING "DD-MM-YYYY",
            '\033015'

        PRINT
        PRINT COLUMN 002,"FOLIO INTERNO       : ",l_record.folio USING "<<<<<<<<<<",
              COLUMN 126,"FECHA GENERACION    : ",HOY USING "DD-MM-YYYY",
            '\033015'

        PRINT
        PRINT COLUMN 002,"FECHA DE OPERACION  : ",HOY USING "DD-MM-YYYY",
            '\033015'

        --PRINT COLUMN 1,'\033e\033(s218T\033(s24H\033(s7B'

        PRINT COLUMN 1,"\332",L10,L1,
                       "\302",L10,L10,L10,L2,L1,
                       "\302",L5,L1,
                       "\302",L5,L2,
                       "\302",L5,L2,
                       "\302",L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,
                              L10,L10,L5,L1,
                       "\302",L5,
                       "\302",L2,L2,
                       "\277",
            '\033015'

        PRINT COLUMN 1,"|           |",
                       "                                 |",
                       " TIPO |",
                       " TIPO  |",
                       " TIPO  |",
                       "                                                          MONTOS A LIQUIDAR DE LA CUENTA INDIVIDUAL (ACCIONES)                                    |",
                       "     |",
                       "    |",
            '\033015'

        PRINT COLUMN 1,"|           |",
                       "                                 |",
                       "      |",
                       "       |",
                       "       \303",L5,
                       "\302",L10,L5,
                       "\302",L10,L5,
                       "\302",L10,L5,
                       "\302",L10,L5,
                       "\302",L10,L2,L2,
                       "\302",L10,L2,L2,
                       "\302",L10,L5,
                       "\302",L10,L2,L2,
                       "\302",L10,L2,L2,
                       "\302",L5,
                       "\302",L2,L2,L1,"|",
            '\033015'

        PRINT COLUMN 1,"|    NSS    |",
                       "       NOMBRE DEL TRABAJADOR     |",
                       "  DE  |",
                       "  DE   |",
                       "  DE   |",
                       "SIEF.|",
                       "     TOTAL     |",
                       "      R97      |",
                       "       CV      |",
                       "      CS       |",
                       "  V I V  97   |",
                       "  V I V  92   |",
                       "P A R T  VIV97 |",
                       "P A R T  VIV92 |",
                       "      R92     |",                       
                       "DIAG.|",
                       "REG.|",
            '\033015'

        PRINT COLUMN 1,"|           |",
                       "                                 |",
                       "      |",
                       "       |",
                       "       |",
                       "     |",
                       "               |",
                       "               |",
                       "               |",
                       "               |",
                       "              |",
                       "              |",
                       "               |",
                       "               |",
                       "              |",                       
                       "     |",
                       "    |",
            '\033015'

        PRINT COLUMN 1,"|           |",
                       "                                 |",
                       "SEGURO|",
                       "PENSION|",
                       "PRESTA.|",
                       "     |",
                       "    ACCIONES   |",
                       "    ACCIONES   |",
                       "    ACCIONES   |",
                       "    ACCIONES   |",
                       "     PESOS    |",
                       "     PESOS    |",
                       "PARTICIPACIONES|",
                       "PARTICIPACIONES|",
                       "    ACCIONES  |",                     
                       "     |",
                       "    |",
            '\033015'


         PRINT COLUMN 1,"\300",L10,L1,
                        "\301",L10,L10,L10,L2,L1,
                        "\301",L5,L1,
                        "\301",L5,L2,
                        "\301",L5,L2,
                        "\301",L5,
                        "\301",L10,L5,
                        "\301",L10,L5,
                        "\301",L10,L5,
                        "\301",L10,L5,
                        "\301",L10,L2,L2,
                        "\301",L10,L2,L2,
                        "\301",L10,L5,
                        "\301",L10,L2,L2,
                        "\301",L10,L2,L2,
                        "\301",L5,
                        "\301",L2,L2,L1,
                        "\331",
            '\033015'

         SKIP 2 LINE

     --------------------
     PAGE HEADER
     --------------------

        --PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'
        --PRINT COLUMN 1,'\033e\033(s218T\033(s15H\033(s7B'

        PRINT '\033E\033(10U\033&l1O\033&k2S\033&18d',
            '\033e\033(s23H'

        SELECT codigo_afore, razon_social
        INTO rpt_afore.codigo_afore, rpt_afore.razon_social
        FROM safre_af:tab_afore_local

        LET hoy       = TODAY
        LET r_nom_rep = "RETL821"

        IF l_record.folio = 0 THEN
	    LET f_opera = f_opera2
        ELSE
           SELECT fecha_genera
           INTO   f_opera
           FROM   ret_ctr_envio_lote
           WHERE  folio = l_record.folio
        END IF

        CALL habil_siguiente(f_opera,3) RETURNING f_trans

        PRINT COLUMN 067,encabezado,
            '\033015'
        SKIP 1 LINE

        PRINT COLUMN 054,"    REPORTE DE SOLICITUD DE DISPOSICION DE RECURSOS",
            '\033015'
        SKIP 2 LINE

        PRINT COLUMN 002,"AFORE               : ",rpt_afore.codigo_afore
                                                USING "###","  ",
                                                rpt_afore.razon_social CLIPPED,
       	      COLUMN 126,"PAGINA              :",pageno USING "##",
            '\033015'

        PRINT
        PRINT COLUMN 002,"PROGRAMA            : ",r_nom_rep,
              COLUMN 126,"FECHA VAL. TRANSFER : ",HOY USING "DD-MM-YYYY",
            '\033015'

        PRINT
        PRINT COLUMN 002,"FOLIO INTERNO       : ",l_record.folio USING "<<<<<<<<<<",
              COLUMN 126,"FECHA GENERACION    : ",HOY USING "DD-MM-YYYY",
            '\033015'

        PRINT
        PRINT COLUMN 002,"FECHA DE OPERACION  : ",HOY USING "DD-MM-YYYY",
            '\033015'

        --PRINT COLUMN 1,'\033e\033(s218T\033(s24H\033(s7B'

         PRINT COLUMN 1,"\332",L10,L1,
                       "\302",L10,L10,L10,L2,L1,
                       "\302",L5,L1,
                       "\302",L5,L2,
                       "\302",L5,L2,
                       "\302",L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,
                              L10,L10,L5,L1,
                       "\302",L5,
                       "\302",L2,L2,
                       "\277",
            '\033015'

        PRINT COLUMN 1,"|           |",
                       "                                 |",
                       " TIPO |",
                       " TIPO  |",
                       " TIPO  |",
                       "                                                          MONTOS A LIQUIDAR DE LA CUENTA INDIVIDUAL (ACCIONES)                                    |",
                       "     |",
                       "    |",
            '\033015'

        PRINT COLUMN 1,"|           |",
                       "                                 |",
                       "      |",
                       "       |",
                       "       \303",L5,
                       "\302",L10,L5,
                       "\302",L10,L5,
                       "\302",L10,L5,
                       "\302",L10,L2,L2,
                       "\302",L10,L2,L2,
                       "\302",L10,L5,
                       "\302",L10,L5,
                       "\302",L10,L2,L2,
                       "\302",L10,L2,L2,
                       "\302",L5,L1,
                       "\302",L2,L2,"|",
            '\033015'
	
        PRINT COLUMN 1,"|    NSS    |",
	               "       NOMBRE DEL TRABAJADOR     |",
		       "  DE  |",
		       "  DE   |",
		       "  DE   |",
                       "SIEF.|",
		       "     TOTAL     |",
		       "      R97      |",
		       "       CV      |",
		       "      CS       |",
                       "  V I V  97   |",
                       "  V I V  92   |",
		       "P A R T  VIV97 |",
		       "P A R T  VIV92 |",
		       "      R92     |",
                       "DIAG.|",
		       "REG.|",
            '\033015'

	 PRINT COLUMN 1,"|           |",
                        "                                 |",
			"      |",
			"       |",
			"       |",
                        "     |",
		        "               |",
		        "               |",
		        "               |",
		        "               |",
		        "              |",
		        "              |",
		        "               |",
		        "               |",
		        "              |",
                        "     |",
                        "    |",
            '\033015'

         PRINT COLUMN 1,"|           |",
			"                                 |",
			"SEGURO|",
			"PENSION|",
			"PRESTA.|",
                        "     |",
			"    ACCIONES   |",
			"    ACCIONES   |",
			"    ACCIONES   |",
			"    ACCIONES   |",
			"     PESOS    |",
			"     PESOS    |",
			"PARTICIPACIONES|",
			"PARTICIPACIONES|",
			"    ACCIONES  |",
         	        "     |",
                        "    |",
            '\033015'


         PRINT COLUMN 1,"\300",L10,L1,
                        "\301",L10,L10,L10,L2,L1,
                        "\301",L5,L1,
                        "\301",L5,L2,
                        "\301",L5,L2,
                        "\301",L5,
                        "\301",L10,L5,
                        "\301",L10,L5,
                        "\301",L10,L5,
                        "\301",L10,L2,L2,
                        "\301",L10,L2,L2,
                        "\301",L10,L5,
                        "\301",L10,L5,
                        "\301",L10,L2,L2,
                        "\301",L10,L2,L2,
                        "\301",L5,
                        "\301",L2,L2,L1,
                        "\331",
            '\033015'

         SKIP 2 LINE

     ---------------------------------
     AFTER GROUP  OF lr_montos.siefore
     ---------------------------------
          --SE OBTIENE NOMBRE DEL AFILIAD0
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

          --SELECCION DE DESCRIPCION DE SIEFORE
          IF lr_montos.siefore = 1 THEN
              LET  nombre_siefore = "SB1"
              LET  cont_tipo_retiro1 = cont_tipo_retiro1 + 1 
          ELSE
             IF lr_montos.siefore = 2 THEN
              LET  nombre_siefore = "SB2"
              LET  cont_tipo_retiro2 = cont_tipo_retiro2 + 1 
             ELSE
                IF lr_montos.siefore = 3 THEN
                   LET  nombre_siefore = "SB3"
                   LET  cont_tipo_retiro3 = cont_tipo_retiro3 + 1 
                ELSE
                   IF lr_montos.siefore = 4 THEN
                      LET  nombre_siefore = "SB4"
                      LET  cont_tipo_retiro4 = cont_tipo_retiro4 + 1 
                   ELSE
                      IF lr_montos.siefore = 5 THEN
                         LET  nombre_siefore = "SB5"
                         LET  cont_tipo_retiro5 = cont_tipo_retiro5 + 1 
                      END IF
                   END IF
                END IF
             END IF
          END IF

          PRINT COLUMN 002,l_record.nss                                           ,
                COLUMN 014,r_nombre[1,35]                                         ,
                COLUMN 050,lr_sol_tx.tipo_seguro                                  ,
                COLUMN 057,lr_sol_tx.tipo_pension                                 ,
                COLUMN 062,lr_sol_tx.tipo_prestacion                              ,
                COLUMN 072,nombre_siefore                                         ,
                COLUMN 077,lr_montos.acciones_ret97 + 
                           lr_montos.acciones_cv    + 
                           lr_montos.acciones_cs    + 
                           lr_montos.acciones_ret92                
                                                       USING "#######&.&&&&&&"    ,
                COLUMN 093,lr_montos.acciones_ret97    USING "#######&.&&&&&&"    ,
                COLUMN 109,lr_montos.acciones_cv       USING "#######&.&&&&&&"    ,
                COLUMN 125,lr_montos.acciones_cs       USING "#######&.&&&&&&"    , 
                COLUMN 141,lr_sol_tx.saldo_viv97_p     USING "#######&.&&"        , 
                COLUMN 156,lr_sol_tx.saldo_viv92_p     USING "#######&.&&"        ,
                COLUMN 171,lr_sol_tx.saldo_viv97       USING "#######&.&&&&&&"    , 
                COLUMN 187,lr_sol_tx.saldo_viv92       USING "#######&.&&&&&&"    ,
                COLUMN 201,lr_montos.acciones_ret92    USING "#######&.&&&&&&"    ,                
                COLUMN 219,lr_sol_rx.diag_registro                                ,
                COLUMN 225,lr_sol_tx.regimen,                              
            '\033015'
          
          SKIP 1 LINE
          

     -----------------------------------
     BEFORE GROUP OF l_record.nss
     -----------------------------------
         LET cont_nss_unicos = cont_nss_unicos + 1


     -----------------------------------
     AFTER GROUP OF l_record.tipo_retiro
     -----------------------------------
     
          LET ind = ind + 1    -- Ocurrencia del arreglo

          SKIP 1 LINE

          --OBTIENE TOTALES POR TIPO DE RETIRO SIEFORE 1
          LET reg_s1[ind].tot = cont_tipo_retiro1
          IF reg_s1[ind].tot IS NULL OR reg_s1[ind].tot = '' THEN
              LET reg_s1[ind].tot = 0
          END IF 
          	    
          LET reg_s1[ind].tipo_ret = l_record.tipo_retiro
          
          LET reg_s1[ind].total    = GROUP SUM (lr_montos.acciones_ret97 + 
                                                lr_montos.acciones_cv  + 
                                                lr_montos.acciones_cs  + 
                                                lr_montos.acciones_ret92)
                                     WHERE l_record.siefore  = 1
          IF reg_s1[ind].total IS NULL OR reg_s1[ind].total = '' THEN
              LET reg_s1[ind].total = 0
          END IF 

          LET reg_s1[ind].r97      = GROUP SUM(lr_montos.acciones_ret97)  
                                     WHERE lr_montos.siefore = 1
          IF reg_s1[ind].r97 IS NULL OR reg_s1[ind].r97 = '' THEN
              LET reg_s1[ind].r97 = 0
          END IF 

          LET reg_s1[ind].cv       = GROUP SUM(lr_montos.acciones_cv   )  
                                     WHERE lr_montos.siefore = 1
          IF reg_s1[ind].cv IS NULL OR reg_s1[ind].cv = '' THEN
              LET reg_s1[ind].cv = 0
          END IF 

          LET reg_s1[ind].cs       = GROUP SUM(lr_montos.acciones_cs   )  
                                     WHERE lr_montos.siefore = 1
          IF reg_s1[ind].cs IS NULL OR reg_s1[ind].cs = '' THEN
              LET reg_s1[ind].cs = 0
          END IF 

          LET reg_s1[ind].viv97    = GROUP SUM(lr_sol_tx.saldo_viv97    ) 
                                     WHERE lr_montos.siefore = 1
          IF reg_s1[ind].viv97 IS NULL OR reg_s1[ind].viv97 = '' THEN
              LET reg_s1[ind].viv97 = 0
          END IF 

          LET reg_s1[ind].viv97p   = GROUP SUM(lr_sol_tx.saldo_viv97_p  ) 
                                     WHERE lr_montos.siefore = 1
          IF reg_s1[ind].viv97p IS NULL OR reg_s1[ind].viv97p = '' THEN
              LET reg_s1[ind].viv97p = 0
          END IF 

          LET reg_s1[ind].viv92    = GROUP SUM(lr_sol_tx.saldo_viv92    ) 
                                     WHERE lr_montos.siefore = 1
          IF reg_s1[ind].viv92 IS NULL OR reg_s1[ind].viv92 = '' THEN
              LET reg_s1[ind].viv92 = 0
          END IF 

          LET reg_s1[ind].viv92p   = GROUP SUM(lr_sol_tx.saldo_viv92_p  ) 
                                     WHERE lr_montos.siefore = 1
          IF reg_s1[ind].viv92p IS NULL OR reg_s1[ind].viv92p = '' THEN
              LET reg_s1[ind].viv92p = 0
          END IF 

          LET reg_s1[ind].r92      = GROUP SUM(lr_montos.acciones_ret92)  
                                     WHERE lr_montos.siefore = 1
          IF reg_s1[ind].r92 IS NULL OR reg_s1[ind].r92 = '' THEN
              LET reg_s1[ind].r92 = 0
          END IF 

          LET reg_s1[ind].viv72    = GROUP SUM(lr_sol_tx.saldo_viv72    ) 
                                     WHERE lr_montos.siefore = 1
          IF reg_s1[ind].viv72 IS NULL OR reg_s1[ind].viv72 = '' THEN
              LET reg_s1[ind].viv72 = 0
          END IF 

          --OBTIENE TOTALES POR TIPO DE RETIRO SIEFORE 2
          LET reg_s2[ind].tot = cont_tipo_retiro2
          IF reg_s2[ind].tot IS NULL OR reg_s2[ind].tot = '' THEN
              LET reg_s2[ind].tot = 0
          END IF
          	
          LET reg_s2[ind].tipo_ret = l_record.tipo_retiro
          LET reg_s2[ind].total    = GROUP SUM (lr_montos.acciones_ret97 + 
                                                lr_montos.acciones_cv  + 
                                                lr_montos.acciones_cs  + 
                                                lr_montos.acciones_ret92)
                                     WHERE l_record.siefore  = 2
          IF reg_s2[ind].total IS NULL OR reg_s2[ind].total = '' THEN
              LET reg_s2[ind].total = 0
          END IF

          LET reg_s2[ind].r97      = GROUP SUM(lr_montos.acciones_ret97)  
                                     WHERE lr_montos.siefore = 2
          IF reg_s2[ind].r97 IS NULL OR reg_s2[ind].r97 = '' THEN
              LET reg_s2[ind].r97 = 0
          END IF

          LET reg_s2[ind].cv       = GROUP SUM(lr_montos.acciones_cv   )  
                                     WHERE lr_montos.siefore = 2
          IF reg_s2[ind].cv IS NULL OR reg_s2[ind].cv = '' THEN
              LET reg_s2[ind].cv = 0
          END IF

          LET reg_s2[ind].cs       = GROUP SUM(lr_montos.acciones_cs   )  
                                     WHERE lr_montos.siefore = 2
          IF reg_s2[ind].cs IS NULL OR reg_s2[ind].cs = '' THEN
              LET reg_s2[ind].cs = 0
          END IF

          LET reg_s2[ind].viv97    = GROUP SUM(lr_sol_tx.saldo_viv97    ) 
                                     WHERE lr_montos.siefore = 2
          IF reg_s2[ind].viv97 IS NULL OR reg_s2[ind].viv97 = '' THEN
              LET reg_s2[ind].viv97 = 0
          END IF

          LET reg_s2[ind].viv97p   = GROUP SUM(lr_sol_tx.saldo_viv97_p  ) 
                                     WHERE lr_montos.siefore = 2
          IF reg_s2[ind].viv97p IS NULL OR reg_s2[ind].viv97p = '' THEN
              LET reg_s2[ind].viv97p = 0
          END IF

          LET reg_s2[ind].viv92    = GROUP SUM(lr_sol_tx.saldo_viv92    ) 
                                     WHERE lr_montos.siefore = 2
          IF reg_s2[ind].viv92 IS NULL OR reg_s2[ind].viv92 = '' THEN
              LET reg_s2[ind].viv92 = 0
          END IF

          LET reg_s2[ind].viv92p   = GROUP SUM(lr_sol_tx.saldo_viv92_p  ) 
                                     WHERE lr_montos.siefore = 2
          IF reg_s2[ind].viv92p IS NULL OR reg_s2[ind].viv92p = '' THEN
              LET reg_s2[ind].viv92p = 0
          END IF

          LET reg_s2[ind].r92      = GROUP SUM(lr_montos.acciones_ret92)  
                                     WHERE lr_montos.siefore = 2
          IF reg_s2[ind].r92 IS NULL OR reg_s2[ind].r92 = '' THEN
              LET reg_s2[ind].r92 = 0
          END IF
          
          LET reg_s2[ind].viv72    = GROUP SUM(lr_sol_tx.saldo_viv72    )           
                                     WHERE lr_montos.siefore = 2
          IF reg_s2[ind].viv72 IS NULL OR reg_s2[ind].viv72 = '' THEN
              LET reg_s2[ind].viv72 = 0
          END IF

          --OBTIENE TOTALES POR TIPO DE RETIRO SIEFORE 3
          LET reg_s3[ind].tot = cont_tipo_retiro3
          IF reg_s3[ind].tot IS NULL OR reg_s3[ind].tot = '' THEN
              LET reg_s3[ind].tot = 0
          END IF
          	
          LET reg_s3[ind].tipo_ret = l_record.tipo_retiro
          LET reg_s3[ind].total    = GROUP SUM (lr_montos.acciones_ret97 + 
                                                lr_montos.acciones_cv  + 
                                                lr_montos.acciones_cs  + 
                                                lr_montos.acciones_ret92)
                                     WHERE l_record.siefore  = 3
          IF reg_s3[ind].total IS NULL OR reg_s3[ind].total = '' THEN
              LET reg_s3[ind].total = 0
          END IF

          LET reg_s3[ind].r97      = GROUP SUM(lr_montos.acciones_ret97)  
                                     WHERE lr_montos.siefore = 3
          IF reg_s3[ind].r97 IS NULL OR reg_s3[ind].r97 = '' THEN
              LET reg_s3[ind].r97 = 0
          END IF

          LET reg_s3[ind].cv       = GROUP SUM(lr_montos.acciones_cv   )  
                                     WHERE lr_montos.siefore = 3
          IF reg_s3[ind].cv IS NULL OR reg_s3[ind].cv = '' THEN
              LET reg_s3[ind].cv = 0
          END IF

          LET reg_s3[ind].cs       = GROUP SUM(lr_montos.acciones_cs   )  
                                     WHERE lr_montos.siefore = 3
          IF reg_s3[ind].cs IS NULL OR reg_s3[ind].cs = '' THEN
              LET reg_s3[ind].cs = 0
          END IF

          LET reg_s3[ind].viv97    = GROUP SUM(lr_sol_tx.saldo_viv97    ) 
                                     WHERE lr_montos.siefore = 3
          IF reg_s3[ind].viv97 IS NULL OR reg_s3[ind].viv97 = '' THEN
              LET reg_s3[ind].viv97 = 0
          END IF

          LET reg_s3[ind].viv97p   = GROUP SUM(lr_sol_tx.saldo_viv97_p  ) 
                                     WHERE lr_montos.siefore = 3
          IF reg_s3[ind].viv97p IS NULL OR reg_s3[ind].viv97p = '' THEN
              LET reg_s3[ind].viv97p = 0
          END IF

          LET reg_s3[ind].viv92    = GROUP SUM(lr_sol_tx.saldo_viv92    ) 
                                     WHERE lr_montos.siefore = 3
          IF reg_s3[ind].viv92 IS NULL OR reg_s3[ind].viv92 = '' THEN
              LET reg_s3[ind].viv92 = 0
          END IF

          LET reg_s3[ind].viv92p   = GROUP SUM(lr_sol_tx.saldo_viv92_p  ) 
                                     WHERE lr_montos.siefore = 3
          IF reg_s3[ind].viv92p IS NULL OR reg_s3[ind].viv92p = '' THEN
              LET reg_s3[ind].viv92p = 0
          END IF

          LET reg_s3[ind].r92      = GROUP SUM(lr_montos.acciones_ret92)  
                                     WHERE lr_montos.siefore = 3
          IF reg_s3[ind].r92 IS NULL OR reg_s3[ind].r92 = '' THEN
              LET reg_s3[ind].r92 = 0
          END IF
          
          LET reg_s3[ind].viv72    = GROUP SUM(lr_sol_tx.saldo_viv72    )           
                                     WHERE lr_montos.siefore = 3
          IF reg_s3[ind].viv72 IS NULL OR reg_s3[ind].viv72 = '' THEN
              LET reg_s3[ind].viv72 = 0
          END IF

          --OBTIENE TOTALES POR TIPO DE RETIRO SIEFORE 4
          LET reg_s4[ind].tot = cont_tipo_retiro4
          IF reg_s4[ind].tot IS NULL OR reg_s4[ind].tot = '' THEN
              LET reg_s4[ind].tot = 0
          END IF
          	
          LET reg_s4[ind].tipo_ret = l_record.tipo_retiro
          LET reg_s4[ind].total    = GROUP SUM (lr_montos.acciones_ret97 + 
                                                lr_montos.acciones_cv  + 
                                                lr_montos.acciones_cs  + 
                                                lr_montos.acciones_ret92)
                                     WHERE l_record.siefore  = 4
          IF reg_s4[ind].total IS NULL OR reg_s4[ind].total = '' THEN
              LET reg_s4[ind].total = 0
          END IF

          LET reg_s4[ind].r97      = GROUP SUM(lr_montos.acciones_ret97)  
                                     WHERE lr_montos.siefore = 4
          IF reg_s4[ind].r97 IS NULL OR reg_s4[ind].r97 = '' THEN
              LET reg_s4[ind].r97 = 0
          END IF

          LET reg_s4[ind].cv       = GROUP SUM(lr_montos.acciones_cv   )  
                                     WHERE lr_montos.siefore = 4
          IF reg_s4[ind].cv IS NULL OR reg_s4[ind].cv = '' THEN
              LET reg_s4[ind].cv = 0
          END IF

          LET reg_s4[ind].cs       = GROUP SUM(lr_montos.acciones_cs   )  
                                     WHERE lr_montos.siefore = 4
          IF reg_s4[ind].cs IS NULL OR reg_s4[ind].cs = '' THEN
              LET reg_s4[ind].cs = 0
          END IF

          LET reg_s4[ind].viv97    = GROUP SUM(lr_sol_tx.saldo_viv97    ) 
                                     WHERE lr_montos.siefore = 4
          IF reg_s4[ind].viv97 IS NULL OR reg_s4[ind].viv97 = '' THEN
              LET reg_s4[ind].viv97 = 0
          END IF

          LET reg_s4[ind].viv97p   = GROUP SUM(lr_sol_tx.saldo_viv97_p  ) 
                                     WHERE lr_montos.siefore = 4
          IF reg_s4[ind].viv97p IS NULL OR reg_s4[ind].viv97p = '' THEN
              LET reg_s4[ind].viv97p = 0
          END IF

          LET reg_s4[ind].viv92    = GROUP SUM(lr_sol_tx.saldo_viv92    ) 
                                     WHERE lr_montos.siefore = 4
          IF reg_s4[ind].viv92 IS NULL OR reg_s4[ind].viv92 = '' THEN
              LET reg_s4[ind].viv92 = 0
          END IF

          LET reg_s4[ind].viv92p   = GROUP SUM(lr_sol_tx.saldo_viv92_p  ) 
                                     WHERE lr_montos.siefore = 4
          IF reg_s4[ind].viv92p IS NULL OR reg_s4[ind].viv92p = '' THEN
              LET reg_s4[ind].viv92p = 0
          END IF

          LET reg_s4[ind].r92      = GROUP SUM(lr_montos.acciones_ret92)  
                                     WHERE lr_montos.siefore = 4
          IF reg_s4[ind].r92 IS NULL OR reg_s4[ind].r92 = '' THEN
              LET reg_s4[ind].r92 = 0
          END IF
          
          LET reg_s4[ind].viv72    = GROUP SUM(lr_sol_tx.saldo_viv72    )           
                                     WHERE lr_montos.siefore = 4
          IF reg_s4[ind].viv72 IS NULL OR reg_s4[ind].viv72 = '' THEN
              LET reg_s4[ind].viv72 = 0
          END IF

          --OBTIENE TOTALES POR TIPO DE RETIRO SIEFORE 5
          LET reg_s5[ind].tot = cont_tipo_retiro5
          IF reg_s5[ind].tot IS NULL OR reg_s5[ind].tot = '' THEN
              LET reg_s5[ind].tot = 0
          END IF
          	
          LET reg_s5[ind].tipo_ret = l_record.tipo_retiro
          LET reg_s5[ind].total    = GROUP SUM (lr_montos.acciones_ret97 + 
                                                lr_montos.acciones_cv  + 
                                                lr_montos.acciones_cs  + 
                                                lr_montos.acciones_ret92)
                                     WHERE l_record.siefore  = 5
          IF reg_s5[ind].total IS NULL OR reg_s5[ind].total = '' THEN
              LET reg_s5[ind].total = 0
          END IF

          LET reg_s5[ind].r97      = GROUP SUM(lr_montos.acciones_ret97)  
                                     WHERE lr_montos.siefore = 5
          IF reg_s5[ind].r97 IS NULL OR reg_s5[ind].r97 = '' THEN
              LET reg_s5[ind].r97 = 0
          END IF

          LET reg_s5[ind].cv       = GROUP SUM(lr_montos.acciones_cv   )  
                                     WHERE lr_montos.siefore = 5
          IF reg_s5[ind].cv IS NULL OR reg_s5[ind].cv = '' THEN
              LET reg_s5[ind].cv = 0
          END IF

          LET reg_s5[ind].cs       = GROUP SUM(lr_montos.acciones_cs   )  
                                     WHERE lr_montos.siefore = 5
          IF reg_s5[ind].cs IS NULL OR reg_s5[ind].cs = '' THEN
              LET reg_s5[ind].cs = 0
          END IF

          LET reg_s5[ind].viv97    = GROUP SUM(lr_sol_tx.saldo_viv97    ) 
                                     WHERE lr_montos.siefore = 5
          IF reg_s5[ind].viv97 IS NULL OR reg_s5[ind].viv97 = '' THEN
              LET reg_s5[ind].viv97 = 0
          END IF

          LET reg_s5[ind].viv97p   = GROUP SUM(lr_sol_tx.saldo_viv97_p  ) 
                                     WHERE lr_montos.siefore = 5
          IF reg_s5[ind].viv97p IS NULL OR reg_s5[ind].viv97p = '' THEN
              LET reg_s5[ind].viv97p = 0
          END IF

          LET reg_s5[ind].viv92    = GROUP SUM(lr_sol_tx.saldo_viv92    ) 
                                     WHERE lr_montos.siefore = 5
          IF reg_s5[ind].viv92 IS NULL OR reg_s5[ind].viv92 = '' THEN
              LET reg_s5[ind].viv92 = 0
          END IF

          LET reg_s5[ind].viv92p   = GROUP SUM(lr_sol_tx.saldo_viv92_p  ) 
                                     WHERE lr_montos.siefore = 5
          IF reg_s5[ind].viv92p IS NULL OR reg_s5[ind].viv92p = '' THEN
              LET reg_s5[ind].viv92p = 0
          END IF

          LET reg_s5[ind].r92      = GROUP SUM(lr_montos.acciones_ret92)  
                                     WHERE lr_montos.siefore = 5
          IF reg_s5[ind].r92 IS NULL OR reg_s5[ind].r92 = '' THEN
              LET reg_s5[ind].r92 = 0
          END IF
          
          LET reg_s5[ind].viv72    = GROUP SUM(lr_sol_tx.saldo_viv72    )       
                                     WHERE lr_montos.siefore = 5
          IF reg_s5[ind].viv72 IS NULL OR reg_s5[ind].viv72 = '' THEN
              LET reg_s5[ind].viv72 = 0
          END IF

          -- ACUMULA EN ARREGLO CONTADOR DE NSS UNICOS POR T DE RETIRO
          
          LET reg[ind].cont_nss_uni = cont_nss_unicos 

          -------------------------------------------------------------
          -- IMPRESION DE TOTAL POR TIPO DE RETIRO PARA SIEFORE 1
          -------------------------------------------------------------
       --
          IF reg_s1[ind].tot > 0 THEN
             PRINT COLUMN 1,"\332",L10,L1,
                            "\302",L10,L10,L10,L10,L10,L5,L1,
                            "\302",L5,
                            "\302",L10,L5,
                            "\302",L10,L5,
                            "\302",L10,L5,
                            "\302",L10,L5,
                            "\302",L10,L2,L2,
                            "\302",L10,L2,L2,
                            "\302",L10,L5,
                            "\302",L10,L5,
                            "\302",L10,L2,L2,L1,
                            "\277",
            '\033015'
             
             PRINT COLUMN 001,"|",reg_s1[ind].tot  USING "#######"        ,
                   COLUMN 013,"|",
                   COLUMN 015,"TOTALES POR TIPO RETIRO  ",reg_s1[ind].tipo_ret   ,
                   COLUMN 070,"|",
                   COLUMN 072,"SB1"                                              ,
                   COLUMN 076,"|",
                   COLUMN 077,reg_s1[ind].total  USING "#######&.&&&&&&",
                   COLUMN 092,"|",
                   COLUMN 093,reg_s1[ind].r97    USING "#######&.&&&&&&",
                   COLUMN 108,"|",
                   COLUMN 109,reg_s1[ind].cv     USING "#######&.&&&&&&",
                   COLUMN 124,"|",
                   COLUMN 125,reg_s1[ind].cs     USING "#######&.&&&&&&",
                   COLUMN 140,"|",
                   COLUMN 141,reg_s1[ind].viv97p USING "#######&.&&" ,
                   COLUMN 155,"|",
                   COLUMN 156,reg_s1[ind].viv92p USING "#######&.&&",
                   COLUMN 170,"|",
                   COLUMN 171,reg_s1[ind].viv97  USING "#######&.&&&&&&",
                   COLUMN 186,"|",
                   COLUMN 187,reg_s1[ind].viv92  USING "#######&.&&&&&&",
                   COLUMN 202,"|",
                   COLUMN 203,reg_s1[ind].r92    USING "#######&.&&&&&&",
                   COLUMN 218,"|",
            '\033015'
             
             PRINT COLUMN 1,"\300",L10,L1,
                            "\301",L10,L10,L10,L10,L10,L5,L1,
                            "\301",L5,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L2,L2,
                            "\301",L10,L2,L2,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L2,L2,L1,
                            "\331",
            '\033015'
            END IF

          -------------------------------------------------------------
          -- IMPRESION DE TOTAL POR TIPO DE RETIRO PARA SIEFORE 2
          -------------------------------------------------------------
            IF reg_s2[ind].tot > 0 THEN
             PRINT COLUMN 1,"\332",L10,L1,
                            "\302",L10,L10,L10,L10,L10,L5,L1,
                            "\302",L5,
                            "\302",L10,L5,
                            "\302",L10,L5,
                            "\302",L10,L5,
                            "\302",L10,L5,
                            "\302",L10,L2,L2,
                            "\302",L10,L2,L2,
                            "\302",L10,L5,
                            "\302",L10,L5,                            
                            "\302",L10,L2,L2,L1,
                            "\277",
            '\033015'
             
             PRINT COLUMN 001,"|",reg_s2[ind].tot  USING "#######"        ,
                   COLUMN 013,"|",
                   COLUMN 015,"TOTALES POR TIPO RETIRO  ",reg_s2[ind].tipo_ret   ,
                   COLUMN 070,"|",
                   COLUMN 072,"SB2"                                              ,
                   COLUMN 076,"|",
                   COLUMN 077,reg_s2[ind].total  USING "#######&.&&&&&&",
                   COLUMN 092,"|",
                   COLUMN 093,reg_s2[ind].r97    USING "#######&.&&&&&&",
                   COLUMN 108,"|",
                   COLUMN 109,reg_s2[ind].cv     USING "#######&.&&&&&&",
                   COLUMN 124,"|",
                   COLUMN 125,reg_s2[ind].cs     USING "#######&.&&&&&&",
                   COLUMN 140,"|",
                   COLUMN 141,reg_s2[ind].viv97p USING "#######&.&&" ,
                   COLUMN 155,"|",
                   COLUMN 156,reg_s2[ind].viv92p USING "#######&.&&",
                   COLUMN 170,"|",
                   COLUMN 171,reg_s2[ind].viv97  USING "#######&.&&&&&&",
                   COLUMN 186,"|",
                   COLUMN 187,reg_s2[ind].viv92  USING "#######&.&&&&&&",
                   COLUMN 202,"|",
                   COLUMN 203,reg_s2[ind].r92    USING "#######&.&&&&&&",
                   COLUMN 218,"|",
            '\033015'
             
             PRINT COLUMN 1,"\300",L10,L1,
                            "\301",L10,L10,L10,L10,L10,L5,L1,
                            "\301",L5,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L2,L2,
                            "\301",L10,L2,L2,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L2,L2,L1,
                            "\331",
            '\033015'
            END IF

          -------------------------------------------------------------
          -- IMPRESION DE TOTAL POR TIPO DE RETIRO PARA SIEFORE 3
          -------------------------------------------------------------
            IF reg_s3[ind].tot > 0 THEN
             PRINT COLUMN 1,"\332",L10,L1,
                            "\302",L10,L10,L10,L10,L10,L5,L1,
                            "\302",L5,
                            "\302",L10,L5,
                            "\302",L10,L5,
                            "\302",L10,L5,
                            "\302",L10,L5,
                            "\302",L10,L2,L2,
                            "\302",L10,L2,L2,
                            "\302",L10,L5,
                            "\302",L10,L5,                            
                            "\302",L10,L2,L2,L1,
                            "\277",
            '\033015'
             
             PRINT COLUMN 001,"|",reg_s3[ind].tot  USING "#######"        ,
                   COLUMN 013,"|",
                   COLUMN 015,"TOTALES POR TIPO RETIRO  ",reg_s3[ind].tipo_ret   ,
                   COLUMN 070,"|",
                   COLUMN 072,"SB3"                                              ,
                   COLUMN 076,"|",
                   COLUMN 077,reg_s3[ind].total  USING "#######&.&&&&&&",
                   COLUMN 092,"|",
                   COLUMN 093,reg_s3[ind].r97    USING "#######&.&&&&&&",
                   COLUMN 108,"|",
                   COLUMN 109,reg_s3[ind].cv     USING "#######&.&&&&&&",
                   COLUMN 124,"|",
                   COLUMN 125,reg_s3[ind].cs     USING "#######&.&&&&&&",
                   COLUMN 140,"|",
                   COLUMN 141,reg_s3[ind].viv97p USING "#######&.&&" ,
                   COLUMN 155,"|",
                   COLUMN 156,reg_s3[ind].viv92p USING "#######&.&&",
                   COLUMN 170,"|",
                   COLUMN 171,reg_s3[ind].viv97  USING "#######&.&&&&&&",
                   COLUMN 186,"|",
                   COLUMN 187,reg_s3[ind].viv92  USING "#######&.&&&&&&",
                   COLUMN 202,"|",
                   COLUMN 203,reg_s3[ind].r92    USING "#######&.&&&&&&",
                   COLUMN 218,"|",
            '\033015'
             
             PRINT COLUMN 1,"\300",L10,L1,
                            "\301",L10,L10,L10,L10,L10,L5,L1,
                            "\301",L5,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L2,L2,
                            "\301",L10,L2,L2,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L2,L2,L1,
                            "\331",
            '\033015'
            END IF
          -------------------------------------------------------------
          -- IMPRESION DE TOTAL POR TIPO DE RETIRO PARA SIEFORE 4
          -------------------------------------------------------------
            IF reg_s4[ind].tot > 0 THEN
             PRINT COLUMN 1,"\332",L10,L1,
                            "\302",L10,L10,L10,L10,L10,L5,L1,
                            "\302",L5,
                            "\302",L10,L5,
                            "\302",L10,L5,
                            "\302",L10,L5,
                            "\302",L10,L5,
                            "\302",L10,L2,L2,
                            "\302",L10,L2,L2,
                            "\302",L10,L5,
                            "\302",L10,L5,                            
                            "\302",L10,L2,L2,L1,
                            "\277",
            '\033015'
             
             PRINT COLUMN 001,"|",reg_s4[ind].tot  USING "#######"        ,
                   COLUMN 013,"|",
                   COLUMN 015,"TOTALES POR TIPO RETIRO  ",reg_s4[ind].tipo_ret   ,
                   COLUMN 070,"|",
                   COLUMN 072,"SB4"                                              ,
                   COLUMN 076,"|",
                   COLUMN 077,reg_s4[ind].total  USING "#######&.&&&&&&",
                   COLUMN 092,"|",
                   COLUMN 093,reg_s4[ind].r97    USING "#######&.&&&&&&",
                   COLUMN 108,"|",
                   COLUMN 109,reg_s4[ind].cv     USING "#######&.&&&&&&",
                   COLUMN 124,"|",
                   COLUMN 125,reg_s4[ind].cs     USING "#######&.&&&&&&",
                   COLUMN 140,"|",
                   COLUMN 141,reg_s4[ind].viv97p USING "#######&.&&" ,
                   COLUMN 155,"|",
                   COLUMN 156,reg_s4[ind].viv92p USING "#######&.&&",
                   COLUMN 170,"|",
                   COLUMN 171,reg_s4[ind].viv97  USING "#######&.&&&&&&",
                   COLUMN 186,"|",
                   COLUMN 187,reg_s4[ind].viv92  USING "#######&.&&&&&&",
                   COLUMN 202,"|",
                   COLUMN 203,reg_s4[ind].r92    USING "#######&.&&&&&&",
                   COLUMN 218,"|",
            '\033015'
             
             PRINT COLUMN 1,"\300",L10,L1,
                            "\301",L10,L10,L10,L10,L10,L5,L1,
                            "\301",L5,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L2,L2,
                            "\301",L10,L2,L2,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L2,L2,L1,
                            "\331",
            '\033015'
            END IF
          -------------------------------------------------------------
          -- IMPRESION DE TOTAL POR TIPO DE RETIRO PARA SIEFORE 5
          -------------------------------------------------------------
            IF reg_s5[ind].tot > 0 THEN
             PRINT COLUMN 1,"\332",L10,L1,
                            "\302",L10,L10,L10,L10,L10,L5,L1,
                            "\302",L5,
                            "\302",L10,L5,
                            "\302",L10,L5,
                            "\302",L10,L5,
                            "\302",L10,L5,
                            "\302",L10,L2,L2,
                            "\302",L10,L2,L2,
                            "\302",L10,L5,
                            "\302",L10,L5,                            
                            "\302",L10,L2,L2,L1,
                            "\277",
            '\033015'
             
             PRINT COLUMN 001,"|",reg_s5[ind].tot  USING "#######"        ,
                   COLUMN 013,"|",
                   COLUMN 015,"TOTALES POR TIPO RETIRO  ",reg_s5[ind].tipo_ret   ,
                   COLUMN 070,"|",
                   COLUMN 072,"SB5"                                              ,
                   COLUMN 076,"|",
                   COLUMN 077,reg_s5[ind].total  USING "#######&.&&&&&&",
                   COLUMN 092,"|",
                   COLUMN 093,reg_s5[ind].r97    USING "#######&.&&&&&&",
                   COLUMN 108,"|",
                   COLUMN 109,reg_s5[ind].cv     USING "#######&.&&&&&&",
                   COLUMN 124,"|",
                   COLUMN 125,reg_s5[ind].cs     USING "#######&.&&&&&&",
                   COLUMN 140,"|",
                   COLUMN 141,reg_s5[ind].viv97p USING "#######&.&&" ,
                   COLUMN 155,"|",
                   COLUMN 156,reg_s5[ind].viv92p USING "#######&.&&",
                   COLUMN 170,"|",
                   COLUMN 171,reg_s5[ind].viv97  USING "#######&.&&&&&&",
                   COLUMN 186,"|",
                   COLUMN 187,reg_s5[ind].viv92  USING "#######&.&&&&&&",
                   COLUMN 202,"|",
                   COLUMN 203,reg_s5[ind].r92    USING "#######&.&&&&&&",
                   COLUMN 218,"|",
            '\033015'
             
             PRINT COLUMN 1,"\300",L10,L1,
                            "\301",L10,L10,L10,L10,L10,L5,L1,
                            "\301",L5,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L2,L2,
                            "\301",L10,L2,L2,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L2,L2,L1,
                            "\331",
            '\033015'
            END IF
             ---------------------------------------------------
             -- DETALLE DE TOTALES DE NSS UNICOS Y SUMA DE VIVIENDA
             ---------------------------------------------------
             PRINT COLUMN 1,"\332",L10,L1,
                            L10,L10,L10,L10,L10,L5,L1,L1,
                            L5,L1,
                            L10,L5,L1,
                            L10,L5,L1,
                            L10,L5,L1,
                            L10,L5,L1,
                            "\302",L10,L2,L2,
                            "\302",L10,L2,L2,
                            "\302",L10,L5,
                            "\302",L10,L5,
                            "\302",L10,L2,L2,L1,
                            "\277",
            '\033015'

                 PRINT 
                     COLUMN 001,"|",
                     COLUMN 015,"TOTAL DE NSS UNICOS :  ",cont_nss_unicos USING "#####",
                     COLUMN 140,"|",
                     COLUMN 141,reg_s1[ind].viv97p +
                                reg_s2[ind].viv97p +
                                reg_s3[ind].viv97p +
                                reg_s4[ind].viv97p +
                                reg_s5[ind].viv97p    USING "#######&.&&",
                     COLUMN 155,"|",
                     COLUMN 156,reg_s1[ind].viv92p +
                                reg_s2[ind].viv92p +
                                reg_s3[ind].viv92p +
                                reg_s4[ind].viv92p +
                                reg_s5[ind].viv92p    USING "#######&.&&",
                     COLUMN 170,"|",
                     COLUMN 171,reg_s1[ind].viv97  +
                                reg_s2[ind].viv97  +
                                reg_s3[ind].viv97  +
                                reg_s4[ind].viv97  +
                                reg_s5[ind].viv97     USING "#######&.&&&&&&",
                     COLUMN 186,"|",
                     COLUMN 187,reg_s1[ind].viv92  +
                                reg_s2[ind].viv92  +
                                reg_s3[ind].viv92  +
                                reg_s4[ind].viv92  +
                                reg_s5[ind].viv92     USING "#######&.&&&&&&",
                     COLUMN 202,"|",
                     COLUMN 218,"|",
            '\033015'
                                 
             PRINT COLUMN 1,"\300",L10,L1,
                            L10,L10,L10,L10,L10,L5,L1,L1,
                            L5,L1,
                            L10,L5,L1,
                            L10,L5,L1,
                            L10,L5,L1,
                            L10,L5,L1,
                            "\301",L10,L2,L2,
                            "\301",L10,L2,L2,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L2,L2,L1,
                            "\331",
            '\033015'
             SKIP 1 LINE
             
             LET cont_tipo_retiro1 = 0
             LET cont_tipo_retiro2 = 0
             LET cont_tipo_retiro3 = 0
             LET cont_tipo_retiro4 = 0
             LET cont_tipo_retiro5 = 0
             LET cont_nss_unicos   = 0

     -----------------------------------
     AFTER GROUP OF l_record.nss
     -----------------------------------
         LET cont_nss_fin = cont_nss_fin + 1

     ---------------------------------
     ON LAST ROW
     ---------------------------------
          --NEED 49 LINES
          
          SKIP 3 LINES

          PRINT "R E S U M E N ",
            '\033015'
          PRINT

          --IMPRESION DE TODOS LOS DETALLES DE CADA TIPO DE RETIRO
          FOR i = 1 TO 6
            -------------------------------------------------------------
            -- IMPRESION DE TOTAL FINAL POR TIPO DE RETIRO PARA SIEFORE 1
            -------------------------------------------------------------
            IF reg_s1[i].tot > 0 THEN
               PRINT COLUMN 1,"\332",L10,L1,
                              "\302",L10,L10,L10,L10,L10,L5,L1,
                              "\302",L5,
                              "\302",L10,L5,
                              "\302",L10,L5,
                              "\302",L10,L5,
                              "\302",L10,L5,
                              "\302",L10,L2,L2,
                              "\302",L10,L2,L2,
                              "\302",L10,L5,
                              "\302",L10,L5,
                              "\302",L10,L2,L2,L1,
                              "\277",
            '\033015'
               
               PRINT COLUMN 001,"|",reg_s1[i].tot  USING "#######"        ,
                     COLUMN 013,"|",
                     COLUMN 015,"TOTALES POR TIPO RETIRO  ",reg_s1[i].tipo_ret   ,
                     COLUMN 070,"|",
                     COLUMN 072,"SB1"                                              ,
                     COLUMN 076,"|",
                     COLUMN 077,reg_s1[i].total  USING "#######&.&&&&&&",
                     COLUMN 092,"|",
                     COLUMN 093,reg_s1[i].r97    USING "#######&.&&&&&&",
                     COLUMN 108,"|",
                     COLUMN 109,reg_s1[i].cv     USING "#######&.&&&&&&",
                     COLUMN 124,"|",
                     COLUMN 125,reg_s1[i].cs     USING "#######&.&&&&&&",
                     COLUMN 140,"|",
                     COLUMN 141,reg_s1[i].viv97p USING "#######&.&&" ,
                     COLUMN 155,"|",
                     COLUMN 156,reg_s1[i].viv92p USING "#######&.&&",
                     COLUMN 170,"|",
                     COLUMN 171,reg_s1[i].viv97  USING "#######&.&&&&&&",
                     COLUMN 186,"|",
                     COLUMN 187,reg_s1[i].viv92  USING "#######&.&&&&&&",
                     COLUMN 202,"|",
                     COLUMN 203,reg_s1[i].r92    USING "#######&.&&&&&&",
                     COLUMN 218,"|",
            '\033015'
            	   
               PRINT COLUMN 1,"\300",L10,L1,
                              "\301",L10,L10,L10,L10,L10,L5,L1,
                              "\301",L5,
                              "\301",L10,L5,
                              "\301",L10,L5,
                              "\301",L10,L5,
                              "\301",L10,L5,
                              "\301",L10,L2,L2,
                              "\301",L10,L2,L2,
                              "\301",L10,L5,
                              "\301",L10,L5,
                              "\301",L10,L2,L2,L1,
                              "\331",
            '\033015'
            END IF

            -------------------------------------------------------------
            -- IMPRESION DE TOTAL FINAL POR TIPO DE RETIRO PARA SIEFORE 2
            -------------------------------------------------------------
            IF reg_s2[i].tot > 0 THEN
               PRINT COLUMN 1,"\332",L10,L1,
                              "\302",L10,L10,L10,L10,L10,L5,L1,
                              "\302",L5,
                              "\302",L10,L5,
                              "\302",L10,L5,
                              "\302",L10,L5,
                              "\302",L10,L5,
                              "\302",L10,L2,L2,
                              "\302",L10,L2,L2,
                              "\302",L10,L5,
                              "\302",L10,L5,
                              "\302",L10,L2,L2,L1,
                              "\277",
            '\033015'
               
               PRINT COLUMN 001,"|",reg_s2[i].tot  USING "#######"        ,
                     COLUMN 013,"|",
                     COLUMN 015,"TOTALES POR TIPO RETIRO  ",reg_s2[i].tipo_ret   ,
                     COLUMN 070,"|",
                     COLUMN 072,"SB2"                                              ,
                     COLUMN 076,"|",
                     COLUMN 077,reg_s2[i].total  USING "#######&.&&&&&&",
                     COLUMN 092,"|",
                     COLUMN 093,reg_s2[i].r97    USING "#######&.&&&&&&",
                     COLUMN 108,"|",
                     COLUMN 109,reg_s2[i].cv     USING "#######&.&&&&&&",
                     COLUMN 124,"|",
                     COLUMN 125,reg_s2[i].cs     USING "#######&.&&&&&&",
                     COLUMN 140,"|",
                     COLUMN 141,reg_s2[i].viv97p USING "#######&.&&" ,
                     COLUMN 155,"|",
                     COLUMN 156,reg_s2[i].viv92p USING "#######&.&&",
                     COLUMN 170,"|",
                     COLUMN 171,reg_s2[i].viv97  USING "#######&.&&&&&&",
                     COLUMN 186,"|",
                     COLUMN 187,reg_s2[i].viv92  USING "#######&.&&&&&&",
                     COLUMN 202,"|",
                     COLUMN 203,reg_s2[i].r92    USING "#######&.&&&&&&",
                     COLUMN 218,"|",
            '\033015'
               
               PRINT COLUMN 1,"\300",L10,L1,
                              "\301",L10,L10,L10,L10,L10,L5,L1,
                              "\301",L5,
                              "\301",L10,L5,
                              "\301",L10,L5,
                              "\301",L10,L5,
                              "\301",L10,L5,
                              "\301",L10,L2,L2,
                              "\301",L10,L2,L2,
                              "\301",L10,L5,
                              "\301",L10,L5,
                              "\301",L10,L2,L2,L1,
                              "\331",
            '\033015'
            END IF

            -------------------------------------------------------------
            -- IMPRESION DE TOTAL FINAL POR TIPO DE RETIRO PARA SIEFORE 3
            -------------------------------------------------------------
            IF reg_s3[i].tot > 0 THEN
               PRINT COLUMN 1,"\332",L10,L1,
                              "\302",L10,L10,L10,L10,L10,L5,L1,
                              "\302",L5,
                              "\302",L10,L5,
                              "\302",L10,L5,
                              "\302",L10,L5,
                              "\302",L10,L5,
                              "\302",L10,L2,L2,
                              "\302",L10,L2,L2,
                              "\302",L10,L5,
                              "\302",L10,L5,
                              "\302",L10,L2,L2,L1,
                              "\277",
            '\033015'
               
               PRINT COLUMN 001,"|",reg_s3[i].tot  USING "#######"        ,
                     COLUMN 013,"|",
                     COLUMN 015,"TOTALES POR TIPO RETIRO  ",reg_s3[i].tipo_ret   ,
                     COLUMN 070,"|",
                     COLUMN 072,"SB3"                                              ,
                     COLUMN 076,"|",
                     COLUMN 077,reg_s3[i].total  USING "#######&.&&&&&&",
                     COLUMN 092,"|",
                     COLUMN 093,reg_s3[i].r97    USING "#######&.&&&&&&",
                     COLUMN 108,"|",
                     COLUMN 109,reg_s3[i].cv     USING "#######&.&&&&&&",
                     COLUMN 124,"|",
                     COLUMN 125,reg_s3[i].cs     USING "#######&.&&&&&&",
                     COLUMN 140,"|",
                     COLUMN 141,reg_s3[i].viv97p USING "#######&.&&" ,
                     COLUMN 155,"|",
                     COLUMN 156,reg_s3[i].viv92p USING "#######&.&&",
                     COLUMN 170,"|",
                     COLUMN 171,reg_s3[i].viv97  USING "#######&.&&&&&&",
                     COLUMN 186,"|",
                     COLUMN 187,reg_s3[i].viv92  USING "#######&.&&&&&&",
                     COLUMN 202,"|",
                     COLUMN 203,reg_s3[i].r92    USING "#######&.&&&&&&",
                     COLUMN 218,"|",
            '\033015'
               
               PRINT COLUMN 1,"\300",L10,L1,
                              "\301",L10,L10,L10,L10,L10,L5,L1,
                              "\301",L5,
                              "\301",L10,L5,
                              "\301",L10,L5,
                              "\301",L10,L5,
                              "\301",L10,L5,
                              "\301",L10,L2,L2,
                              "\301",L10,L2,L2,
                              "\301",L10,L5,
                              "\301",L10,L5,
                              "\301",L10,L2,L2,L1,
                              "\331",
            '\033015'
            END IF
            -------------------------------------------------------------
            -- IMPRESION DE TOTAL FINAL POR TIPO DE RETIRO PARA SIEFORE 4
            -------------------------------------------------------------
            IF reg_s4[i].tot > 0 THEN
               PRINT COLUMN 1,"\332",L10,L1,
                              "\302",L10,L10,L10,L10,L10,L5,L1,
                              "\302",L5,
                              "\302",L10,L5,
                              "\302",L10,L5,
                              "\302",L10,L5,
                              "\302",L10,L5,
                              "\302",L10,L2,L2,
                              "\302",L10,L2,L2,
                              "\302",L10,L5,
                              "\302",L10,L5,
                              "\302",L10,L2,L2,L1,
                              "\277",
            '\033015'
               
               PRINT COLUMN 001,"|",reg_s4[i].tot  USING "#######"        ,
                     COLUMN 013,"|",
                     COLUMN 015,"TOTALES POR TIPO RETIRO  ",reg_s4[i].tipo_ret   ,
                     COLUMN 070,"|",
                     COLUMN 072,"SB4"                                              ,
                     COLUMN 076,"|",
                     COLUMN 077,reg_s4[i].total  USING "#######&.&&&&&&",
                     COLUMN 092,"|",
                     COLUMN 093,reg_s4[i].r97    USING "#######&.&&&&&&",
                     COLUMN 108,"|",
                     COLUMN 109,reg_s4[i].cv     USING "#######&.&&&&&&",
                     COLUMN 124,"|",
                     COLUMN 125,reg_s4[i].cs     USING "#######&.&&&&&&",
                     COLUMN 140,"|",
                     COLUMN 141,reg_s4[i].viv97p USING "#######&.&&" ,
                     COLUMN 155,"|",
                     COLUMN 156,reg_s4[i].viv92p USING "#######&.&&",
                     COLUMN 170,"|",
                     COLUMN 171,reg_s4[i].viv97  USING "#######&.&&&&&&",
                     COLUMN 186,"|",
                     COLUMN 187,reg_s4[i].viv92  USING "#######&.&&&&&&",
                     COLUMN 202,"|",
                     COLUMN 203,reg_s4[i].r92    USING "#######&.&&&&&&",
                     COLUMN 218,"|",
            '\033015'
               
               PRINT COLUMN 1,"\300",L10,L1,
                              "\301",L10,L10,L10,L10,L10,L5,L1,
                              "\301",L5,
                              "\301",L10,L5,
                              "\301",L10,L5,
                              "\301",L10,L5,
                              "\301",L10,L5,
                              "\301",L10,L2,L2,
                              "\301",L10,L2,L2,
                              "\301",L10,L5,
                              "\301",L10,L5,
                              "\301",L10,L2,L2,L1,
                              "\331",
            '\033015'
            END IF
            -------------------------------------------------------------
            -- IMPRESION DE TOTAL FINAL POR TIPO DE RETIRO PARA SIEFORE 5
            -------------------------------------------------------------
            IF reg_s5[i].tot > 0 THEN
               PRINT COLUMN 1,"\332",L10,L1,
                              "\302",L10,L10,L10,L10,L10,L5,L1,
                              "\302",L5,
                              "\302",L10,L5,
                              "\302",L10,L5,
                              "\302",L10,L5,
                              "\302",L10,L5,
                              "\302",L10,L2,L2,
                              "\302",L10,L2,L2,
                              "\302",L10,L5,
                              "\302",L10,L5,
                              "\302",L10,L2,L2,L1,
                              "\277",
            '\033015'
               
               PRINT COLUMN 001,"|",reg_s5[i].tot  USING "#######"        ,
                     COLUMN 013,"|",
                     COLUMN 015,"TOTALES POR TIPO RETIRO  ",reg_s5[i].tipo_ret   ,
                     COLUMN 070,"|",
                     COLUMN 072,"SB5"                                              ,
                     COLUMN 076,"|",
                     COLUMN 077,reg_s5[i].total  USING "#######&.&&&&&&",
                     COLUMN 092,"|",
                     COLUMN 093,reg_s5[i].r97    USING "#######&.&&&&&&",
                     COLUMN 108,"|",
                     COLUMN 109,reg_s5[i].cv     USING "#######&.&&&&&&",
                     COLUMN 124,"|",
                     COLUMN 125,reg_s5[i].cs     USING "#######&.&&&&&&",
                     COLUMN 140,"|",
                     COLUMN 141,reg_s5[i].viv97p USING "#######&.&&" ,
                     COLUMN 155,"|",
                     COLUMN 156,reg_s5[i].viv92p USING "#######&.&&",
                     COLUMN 170,"|",
                     COLUMN 171,reg_s5[i].viv97  USING "#######&.&&&&&&",
                     COLUMN 186,"|",
                     COLUMN 187,reg_s5[i].viv92  USING "#######&.&&&&&&",
                     COLUMN 202,"|",
                     COLUMN 203,reg_s5[i].r92    USING "#######&.&&&&&&",
                     COLUMN 218,"|",
            '\033015'
               
               PRINT COLUMN 1,"\300",L10,L1,
                              "\301",L10,L10,L10,L10,L10,L5,L1,
                              "\301",L5,
                              "\301",L10,L5,
                              "\301",L10,L5,
                              "\301",L10,L5,
                              "\301",L10,L5,
                              "\301",L10,L2,L2,
                              "\301",L10,L2,L2,
                              "\301",L10,L5,
                              "\301",L10,L5,
                              "\301",L10,L2,L2,L1,
                              "\331",
            '\033015'
            END IF

            IF reg[i].cont_nss_uni > 0 THEN   

             ---------------------------------------------------
             -- DETALLE DE TOTALES DE NSS UNICOS Y SUMA DE VIVIENDA
             ---------------------------------------------------
                PRINT COLUMN 1,"\332",L10,L1,
                               L10,L10,L10,L10,L10,L5,L1,L1,
                               L5,L1,
                               L10,L5,L1,
                               L10,L5,L1,
                               L10,L5,L1,
                               L10,L5,L1,
                               "\302",L10,L2,L2,
                               "\302",L10,L2,L2,
                               "\302",L10,L5,
                               "\302",L10,L5,
                               "\302",L10,L2,L2,L1,
                               "\277",
            '\033015'


                PRINT 
                    COLUMN 001,"|",
                    COLUMN 015,"TOTAL DE NSS UNICOS : ",reg[i].cont_nss_uni USING "#####",
                    COLUMN 140,"|",
                    COLUMN 141,reg_s1[i].viv97p +
                               reg_s2[i].viv97p +
                               reg_s3[i].viv97p +
                               reg_s4[i].viv97p +
                               reg_s5[i].viv97p    USING "#######&.&&",
                    COLUMN 155,"|",
                    COLUMN 156,reg_s1[i].viv92p +
                               reg_s2[i].viv92p +
                               reg_s3[i].viv92p +
                               reg_s4[i].viv92p +
                               reg_s5[i].viv92p    USING "#######&.&&",
                    COLUMN 170,"|",
                    COLUMN 171,reg_s1[i].viv97  +
                               reg_s2[i].viv97  +
                               reg_s3[i].viv97  +
                               reg_s4[i].viv97  +
                               reg_s5[i].viv97     USING "#######&.&&&&&&",
                    COLUMN 186,"|",
                    COLUMN 187,reg_s1[i].viv92  +
                               reg_s2[i].viv92  +
                               reg_s3[i].viv92  +
                               reg_s4[i].viv92  +
                               reg_s5[i].viv92     USING "#######&.&&&&&&",
                    COLUMN 202,"|",
                    COLUMN 218,"|",
            '\033015'
                                 
                PRINT COLUMN 1,"\300",L10,L1,
                               L10,L10,L10,L10,L10,L5,L1,L1,
                               L5,L1,
                               L10,L5,L1,
                               L10,L5,L1,
                               L10,L5,L1,
                               L10,L5,L1,
                               "\301",L10,L2,L2,
                               "\301",L10,L2,L2,
                               "\301",L10,L5,
                               "\301",L10,L5,
                               "\301",L10,L2,L2,L1,
                               "\331",
            '\033015'
                SKIP 1 LINE
            END IF 
          END FOR  -- fin impresion
      
      SKIP 2 LINE
 
      ---------------------------
      --  TOTALES FINALES DE SB1
      ---------------------------
      LET lr_tot_fin_sb1.total   = SUM(lr_montos.acciones_ret97 +
                                       lr_montos.acciones_cv    +  
                                       lr_montos.acciones_cs    +  
                                       lr_montos.acciones_ret92) 
                                   WHERE lr_montos.siefore = 1  
      LET lr_tot_fin_sb1.ret97   = SUM(lr_montos.acciones_ret97)
                                   WHERE lr_montos.siefore = 1
      LET lr_tot_fin_sb1.cv      = SUM(lr_montos.acciones_cv)
                                   WHERE lr_montos.siefore = 1
      LET lr_tot_fin_sb1.cs      = SUM(lr_montos.acciones_cs)
                                   WHERE lr_montos.siefore = 1
      LET lr_tot_fin_sb1.viv97_p = SUM(lr_sol_tx.saldo_viv97_p)
                                   WHERE lr_montos.siefore = 1
      LET lr_tot_fin_sb1.viv92_p = SUM(lr_sol_tx.saldo_viv92_p)
                                   WHERE lr_montos.siefore = 1
      LET lr_tot_fin_sb1.viv97   = SUM(lr_sol_tx.saldo_viv97)
                                   WHERE lr_montos.siefore = 1
      LET lr_tot_fin_sb1.viv92   = SUM(lr_sol_tx.saldo_viv92)
                                   WHERE lr_montos.siefore = 1
      LET lr_tot_fin_sb1.ret92  =  SUM(lr_montos.acciones_ret92)                                                                
                                   WHERE lr_montos.siefore = 1
      LET lr_tot_fin_sb1.viv72  =  SUM(lr_sol_tx.saldo_viv72)                                                                
                                   WHERE lr_montos.siefore = 1

      --------------------------
      --  TOTALES FINALES DE SB2
      --------------------------
      LET lr_tot_fin_sb2.total   = SUM(lr_montos.acciones_ret97 +
                                       lr_montos.acciones_cv    +  
                                       lr_montos.acciones_cs    +  
                                       lr_montos.acciones_ret92) 
                                   WHERE lr_montos.siefore = 2  
      LET lr_tot_fin_sb2.ret97   = SUM(lr_montos.acciones_ret97)
                                   WHERE lr_montos.siefore = 2
      LET lr_tot_fin_sb2.cv      = SUM(lr_montos.acciones_cv)
                                   WHERE lr_montos.siefore = 2
      LET lr_tot_fin_sb2.cs      = SUM(lr_montos.acciones_cs)
                                   WHERE lr_montos.siefore = 2
      LET lr_tot_fin_sb2.viv97_p = SUM(lr_sol_tx.saldo_viv97_p)
                                   WHERE lr_montos.siefore = 2
      LET lr_tot_fin_sb2.viv92_p = SUM(lr_sol_tx.saldo_viv92_p)
                                   WHERE lr_montos.siefore = 2
      LET lr_tot_fin_sb2.viv97   = SUM(lr_sol_tx.saldo_viv97)
                                   WHERE lr_montos.siefore = 2
      LET lr_tot_fin_sb2.viv92   = SUM(lr_sol_tx.saldo_viv92)
                                   WHERE lr_montos.siefore = 2
      LET lr_tot_fin_sb2.ret92  =  SUM(lr_montos.acciones_ret92)                                                                
                                   WHERE lr_montos.siefore = 2
      LET lr_tot_fin_sb2.viv72  =  SUM(lr_sol_tx.saldo_viv72)                                                                
                                   WHERE lr_montos.siefore = 2
      --------------------------
      --  TOTALES FINALES DE SB3
      --------------------------
      LET lr_tot_fin_sb3.total   = SUM(lr_montos.acciones_ret97 +
                                       lr_montos.acciones_cv    +  
                                       lr_montos.acciones_cs    +  
                                       lr_montos.acciones_ret92) 
                                   WHERE lr_montos.siefore = 3  
      LET lr_tot_fin_sb3.ret97   = SUM(lr_montos.acciones_ret97)
                                   WHERE lr_montos.siefore = 3
      LET lr_tot_fin_sb3.cv      = SUM(lr_montos.acciones_cv)
                                   WHERE lr_montos.siefore = 3
      LET lr_tot_fin_sb3.cs      = SUM(lr_montos.acciones_cs)
                                   WHERE lr_montos.siefore = 3
      LET lr_tot_fin_sb3.viv97_p = SUM(lr_sol_tx.saldo_viv97_p)
                                   WHERE lr_montos.siefore = 3
      LET lr_tot_fin_sb3.viv92_p = SUM(lr_sol_tx.saldo_viv92_p)
                                   WHERE lr_montos.siefore = 3
      LET lr_tot_fin_sb3.viv97   = SUM(lr_sol_tx.saldo_viv97)
                                   WHERE lr_montos.siefore = 3
      LET lr_tot_fin_sb3.viv92   = SUM(lr_sol_tx.saldo_viv92)
                                   WHERE lr_montos.siefore = 3
      LET lr_tot_fin_sb3.ret92  =  SUM(lr_montos.acciones_ret92)        
                                   WHERE lr_montos.siefore = 3
      LET lr_tot_fin_sb3.viv72  =  SUM(lr_sol_tx.saldo_viv72)           
                                   WHERE lr_montos.siefore = 3

      --------------------------
      --  TOTALES FINALES DE SB4
      --------------------------
      LET lr_tot_fin_sb4.total   = SUM(lr_montos.acciones_ret97 +
                                       lr_montos.acciones_cv    +  
                                       lr_montos.acciones_cs    +  
                                       lr_montos.acciones_ret92) 
                                   WHERE lr_montos.siefore = 4  
      LET lr_tot_fin_sb4.ret97   = SUM(lr_montos.acciones_ret97)
                                   WHERE lr_montos.siefore = 4
      LET lr_tot_fin_sb4.cv      = SUM(lr_montos.acciones_cv)
                                   WHERE lr_montos.siefore = 4
      LET lr_tot_fin_sb4.cs      = SUM(lr_montos.acciones_cs)
                                   WHERE lr_montos.siefore = 4
      LET lr_tot_fin_sb4.viv97_p = SUM(lr_sol_tx.saldo_viv97_p)
                                   WHERE lr_montos.siefore = 4
      LET lr_tot_fin_sb4.viv92_p = SUM(lr_sol_tx.saldo_viv92_p)
                                   WHERE lr_montos.siefore = 4
      LET lr_tot_fin_sb4.viv97   = SUM(lr_sol_tx.saldo_viv97)
                                   WHERE lr_montos.siefore = 4
      LET lr_tot_fin_sb4.viv92   = SUM(lr_sol_tx.saldo_viv92)
                                   WHERE lr_montos.siefore = 4
      LET lr_tot_fin_sb4.ret92  =  SUM(lr_montos.acciones_ret92)              
                                   WHERE lr_montos.siefore = 4
      LET lr_tot_fin_sb4.viv72  =  SUM(lr_sol_tx.saldo_viv72)
                                   WHERE lr_montos.siefore = 4
      --------------------------
      --  TOTALES FINALES DE SB5
      --------------------------
      LET lr_tot_fin_sb5.total   = SUM(lr_montos.acciones_ret97 +
                                       lr_montos.acciones_cv    +  
                                       lr_montos.acciones_cs    +  
                                       lr_montos.acciones_ret92) 
                                   WHERE lr_montos.siefore = 5  
      LET lr_tot_fin_sb5.ret97   = SUM(lr_montos.acciones_ret97)
                                   WHERE lr_montos.siefore = 5
      LET lr_tot_fin_sb5.cv      = SUM(lr_montos.acciones_cv)
                                   WHERE lr_montos.siefore = 5
      LET lr_tot_fin_sb5.cs      = SUM(lr_montos.acciones_cs)
                                   WHERE lr_montos.siefore = 5
      LET lr_tot_fin_sb5.viv97_p = SUM(lr_sol_tx.saldo_viv97_p)
                                   WHERE lr_montos.siefore = 5
      LET lr_tot_fin_sb5.viv92_p = SUM(lr_sol_tx.saldo_viv92_p)
                                   WHERE lr_montos.siefore = 5
      LET lr_tot_fin_sb5.viv97   = SUM(lr_sol_tx.saldo_viv97)
                                   WHERE lr_montos.siefore = 5
      LET lr_tot_fin_sb5.viv92   = SUM(lr_sol_tx.saldo_viv92)
                                   WHERE lr_montos.siefore = 5
      LET lr_tot_fin_sb5.ret92  =  SUM(lr_montos.acciones_ret92)
                                   WHERE lr_montos.siefore = 5
      LET lr_tot_fin_sb5.viv72  =  SUM(lr_sol_tx.saldo_viv72)
                                   WHERE lr_montos.siefore = 5

      -- IMPRIME TOTAL A LIQUIDAR SIEFORE PARA SIEFORE 1
      --------------------------------------------------
      PRINT COLUMN 1,"\332",L10,L1,
                     "\302",L10,L10,L10,L10,L10,L5,L1,
                     "\302",L5,
                     "\302",L10,L5,
                     "\302",L10,L5,
                     "\302",L10,L5,
                     "\302",L10,L5,
                     "\302",L10,L2,L2,
                     "\302",L10,L2,L2,
                     "\302",L10,L5,
                     "\302",L10,L5,
                     "\302",L10,L2,L2,L1,
                     "\277",
            '\033015'

      PRINT
            COLUMN 001,"|",COUNT(*) 
                           WHERE l_record.siefore=1 USING "#######",
            COLUMN 013,"|",                                               
            COLUMN 015,"TOTAL  A  LIQUIDAR      :  ",                     
            COLUMN 070,"|",                                               
            COLUMN 072,"SB1"                                ,
            COLUMN 076,"|",                                               
            COLUMN 077,lr_tot_fin_sb1.total   USING "#######&.&&&&&&" , 
                                                  
            COLUMN 092,"|",                       
            COLUMN 093,lr_tot_fin_sb1.ret97   USING "#######&.&&&&&&",
            COLUMN 108,"|",                       
            COLUMN 109,lr_tot_fin_sb1.cv      USING "#######&.&&&&&&" , 
            COLUMN 124,"|",                       
            COLUMN 125,lr_tot_fin_sb1.cs      USING "#######&.&&&&&&" , 
            COLUMN 140,"|",                       
            COLUMN 141,lr_tot_fin_sb1.viv97_p USING "#######&.&&"     , 
            COLUMN 155,"|",                       
            COLUMN 156,lr_tot_fin_sb1.viv92_p USING "#######&.&&"     , 
            COLUMN 170,"|",                       
            COLUMN 171,lr_tot_fin_sb1.viv97   USING "#######&.&&&&&&" , 
            COLUMN 186,"|",                       
            COLUMN 187,lr_tot_fin_sb1.viv92   USING "#######&.&&&&&&" , 
            COLUMN 202,"|",                       
            COLUMN 203,lr_tot_fin_sb1.ret92   USING "#######&.&&&&&&" , 
            COLUMN 218,"|",                      
            '\033015'
                                                  
      PRINT COLUMN 1,"\300",L10,L1,                  
                     "\301",L10,L10,L10,L10,L10,L5,L1,
                     "\301",L5,                      
                     "\301",L10,L5,                  
                     "\301",L10,L5,                  
                     "\301",L10,L5,                  
                     "\301",L10,L5,                  
                     "\301",L10,L2,L2,               
                     "\301",L10,L2,L2,               
                     "\301",L10,L5,                  
                     "\301",L10,L5,                  
                     "\301",L10,L2,L2,L1,               
                     "\331",                         
            '\033015'

      -- IMPRIME TOTAL A LIQUIDAR SIEFORE PARA SIEFORE 2
      --------------------------------------------------
      PRINT COLUMN 1,"\332",L10,L1,
                     "\302",L10,L10,L10,L10,L10,L5,L1,
                     "\302",L5,
                     "\302",L10,L5,
                     "\302",L10,L5,
                     "\302",L10,L5,
                     "\302",L10,L5,
                     "\302",L10,L2,L2,
                     "\302",L10,L2,L2,
                     "\302",L10,L5,
                     "\302",L10,L5,
                     "\302",L10,L2,L2,L1,
                     "\277",
            '\033015'
      PRINT
            COLUMN 001,"|",COUNT(*) 
                           WHERE l_record.siefore=2 USING "#######",
            COLUMN 013,"|",                                               
            COLUMN 015,"TOTAL  A  LIQUIDAR      :  ",                     
            COLUMN 070,"|",                                               
            COLUMN 072,"SB2"                                ,
            COLUMN 076,"|",                                               
            COLUMN 077,lr_tot_fin_sb2.total   USING "#######&.&&&&&&" , 
                                                  
            COLUMN 092,"|",                       
            COLUMN 093,lr_tot_fin_sb2.ret97   USING "#######&.&&&&&&",
            COLUMN 108,"|",                       
            COLUMN 109,lr_tot_fin_sb2.cv      USING "#######&.&&&&&&" , 
            COLUMN 124,"|",                       
            COLUMN 125,lr_tot_fin_sb2.cs      USING "#######&.&&&&&&" , 
            COLUMN 140,"|",                       
            COLUMN 141,lr_tot_fin_sb2.viv97_p USING "#######&.&&"     , 
            COLUMN 155,"|",                       
            COLUMN 156,lr_tot_fin_sb2.viv92_p USING "#######&.&&"     , 
            COLUMN 170,"|",                       
            COLUMN 171,lr_tot_fin_sb2.viv97   USING "#######&.&&&&&&" , 
            COLUMN 186,"|",                       
            COLUMN 187,lr_tot_fin_sb2.viv92   USING "#######&.&&&&&&" , 
            COLUMN 202,"|",                       
            COLUMN 203,lr_tot_fin_sb2.ret92   USING "#######&.&&&&&&" , 
            COLUMN 218,"|",                      
            '\033015'
                                                  
      PRINT COLUMN 1,"\300",L10,L1,                  
                     "\301",L10,L10,L10,L10,L10,L5,L1,
                     "\301",L5,                      
                     "\301",L10,L5,                  
                     "\301",L10,L5,                  
                     "\301",L10,L5,                  
                     "\301",L10,L5,                  
                     "\301",L10,L2,L2,               
                     "\301",L10,L2,L2,               
                     "\301",L10,L5,                  
                     "\301",L10,L5,                  
                     "\301",L10,L2,L2,L1,               
                     "\331",                         
            '\033015'

      -- IMPRIME TOTAL A LIQUIDAR SIEFORE PARA SIEFORE 3
      --------------------------------------------------
      PRINT COLUMN 1,"\332",L10,L1,
                     "\302",L10,L10,L10,L10,L10,L5,L1,
                     "\302",L5,
                     "\302",L10,L5,
                     "\302",L10,L5,
                     "\302",L10,L5,
                     "\302",L10,L5,
                     "\302",L10,L2,L2,
                     "\302",L10,L2,L2,
                     "\302",L10,L5,
                     "\302",L10,L5,
                     "\302",L10,L2,L2,L1,
                     "\277",
            '\033015'

      PRINT
            COLUMN 001,"|",COUNT(*) 
                           WHERE l_record.siefore=3 USING "#######",
            COLUMN 013,"|",                                               
            COLUMN 015,"TOTAL  A  LIQUIDAR      :  ",                     
            COLUMN 070,"|",                                               
            COLUMN 072,"SB3"                                ,
            COLUMN 076,"|",                                               
            COLUMN 077,lr_tot_fin_sb3.total   USING "#######&.&&&&&&" ,
            COLUMN 092,"|",                       
            COLUMN 093,lr_tot_fin_sb3.ret97   USING "#######&.&&&&&&",
            COLUMN 108,"|",                       
            COLUMN 109,lr_tot_fin_sb3.cv      USING "#######&.&&&&&&" , 
            COLUMN 124,"|",                       
            COLUMN 125,lr_tot_fin_sb3.cs      USING "#######&.&&&&&&" , 
            COLUMN 140,"|",                       
            COLUMN 141,lr_tot_fin_sb3.viv97_p USING "#######&.&&"     , 
            COLUMN 155,"|",                       
            COLUMN 156,lr_tot_fin_sb3.viv92_p USING "#######&.&&"     , 
            COLUMN 170,"|",                       
            COLUMN 171,lr_tot_fin_sb3.viv97   USING "#######&.&&&&&&" , 
            COLUMN 186,"|",                       
            COLUMN 187,lr_tot_fin_sb3.viv92   USING "#######&.&&&&&&" , 
            COLUMN 202,"|",                       
            COLUMN 203,lr_tot_fin_sb3.ret92   USING "#######&.&&&&&&" , 
            COLUMN 218,"|",                      
            '\033015'
                                                  
      PRINT COLUMN 1,"\300",L10,L1,                  
                     "\301",L10,L10,L10,L10,L10,L5,L1,
                     "\301",L5,                      
                     "\301",L10,L5,                  
                     "\301",L10,L5,                  
                     "\301",L10,L5,                  
                     "\301",L10,L5,                  
                     "\301",L10,L2,L2,               
                     "\301",L10,L2,L2,               
                     "\301",L10,L5,                  
                     "\301",L10,L5,                  
                     "\301",L10,L2,L2,L1,               
                     "\331",                         
            '\033015'

      -- IMPRIME TOTAL A LIQUIDAR SIEFORE PARA SIEFORE 4
      --------------------------------------------------
      PRINT COLUMN 1,"\332",L10,L1,
                     "\302",L10,L10,L10,L10,L10,L5,L1,
                     "\302",L5,
                     "\302",L10,L5,
                     "\302",L10,L5,
                     "\302",L10,L5,
                     "\302",L10,L5,
                     "\302",L10,L2,L2,
                     "\302",L10,L2,L2,
                     "\302",L10,L5,
                     "\302",L10,L5,
                     "\302",L10,L2,L2,L1,
                     "\277",
            '\033015'

      PRINT
            COLUMN 001,"|",COUNT(*) 
                           WHERE l_record.siefore=4 USING "#######",
            COLUMN 013,"|",                                               
            COLUMN 015,"TOTAL  A  LIQUIDAR      :  ",                     
            COLUMN 070,"|",                                               
            COLUMN 072,"SB4"                                ,
            COLUMN 076,"|",                                               
            COLUMN 077,lr_tot_fin_sb4.total   USING "#######&.&&&&&&" , 
            COLUMN 092,"|",                       
            COLUMN 093,lr_tot_fin_sb4.ret97   USING "#######&.&&&&&&",
            COLUMN 108,"|",                       
            COLUMN 109,lr_tot_fin_sb4.cv      USING "#######&.&&&&&&" , 
            COLUMN 124,"|",                       
            COLUMN 125,lr_tot_fin_sb4.cs      USING "#######&.&&&&&&" , 
            COLUMN 140,"|",                       
            COLUMN 141,lr_tot_fin_sb4.viv97_p USING "#######&.&&"     , 
            COLUMN 155,"|",                       
            COLUMN 156,lr_tot_fin_sb4.viv92_p USING "#######&.&&"     , 
            COLUMN 170,"|",                       
            COLUMN 171,lr_tot_fin_sb4.viv97   USING "#######&.&&&&&&" , 
            COLUMN 186,"|",                       
            COLUMN 187,lr_tot_fin_sb4.viv92   USING "#######&.&&&&&&" , 
            COLUMN 202,"|",                       
            COLUMN 203,lr_tot_fin_sb4.ret92   USING "#######&.&&&&&&" , 
            COLUMN 218,"|",                      
            '\033015'
                                                  
      PRINT COLUMN 1,"\300",L10,L1,                  
                     "\301",L10,L10,L10,L10,L10,L5,L1,
                     "\301",L5,                      
                     "\301",L10,L5,                  
                     "\301",L10,L5,                  
                     "\301",L10,L5,                  
                     "\301",L10,L5,                  
                     "\301",L10,L2,L2,               
                     "\301",L10,L2,L2,               
                     "\301",L10,L5,                  
                     "\301",L10,L5,                  
                     "\301",L10,L2,L2,L1,               
                     "\331",                         
            '\033015'

      -- IMPRIME TOTAL A LIQUIDAR SIEFORE PARA SIEFORE 5
      --------------------------------------------------
      PRINT COLUMN 1,"\332",L10,L1,
                     "\302",L10,L10,L10,L10,L10,L5,L1,
                     "\302",L5,
                     "\302",L10,L5,
                     "\302",L10,L5,
                     "\302",L10,L5,
                     "\302",L10,L5,
                     "\302",L10,L2,L2,
                     "\302",L10,L2,L2,
                     "\302",L10,L5,
                     "\302",L10,L5,
                     "\302",L10,L2,L2,L1,
                     "\277", 
            '\033015'

      PRINT
            COLUMN 001,"|",COUNT(*) 
                           WHERE l_record.siefore=5 USING "#######",
            COLUMN 013,"|",                                               
            COLUMN 015,"TOTAL  A  LIQUIDAR      :  ",                     
            COLUMN 070,"|",                                               
            COLUMN 072,"SB5"                                ,
            COLUMN 076,"|",                                               
            COLUMN 077,lr_tot_fin_sb5.total   USING "#######&.&&&&&&" , 
            COLUMN 092,"|",                       
            COLUMN 093,lr_tot_fin_sb5.ret97   USING "#######&.&&&&&&",
            COLUMN 108,"|",                       
            COLUMN 109,lr_tot_fin_sb5.cv      USING "#######&.&&&&&&" , 
            COLUMN 124,"|",                       
            COLUMN 125,lr_tot_fin_sb5.cs      USING "#######&.&&&&&&" , 
            COLUMN 140,"|",                       
            COLUMN 141,lr_tot_fin_sb5.viv97_p USING "#######&.&&"     , 
            COLUMN 155,"|",                       
            COLUMN 156,lr_tot_fin_sb5.viv92_p USING "#######&.&&"     , 
            COLUMN 170,"|",                       
            COLUMN 171,lr_tot_fin_sb5.viv97   USING "#######&.&&&&&&" , 
            COLUMN 186,"|",                       
            COLUMN 187,lr_tot_fin_sb5.viv92   USING "#######&.&&&&&&" , 
            COLUMN 202,"|",                       
            COLUMN 203,lr_tot_fin_sb5.ret92   USING "#######&.&&&&&&" , 
            COLUMN 218,"|",                      
            '\033015'
                                                  
      PRINT COLUMN 1,"\300",L10,L1,                  
                     "\301",L10,L10,L10,L10,L10,L5,L1,
                     "\301",L5,                      
                     "\301",L10,L5,                  
                     "\301",L10,L5,                  
                     "\301",L10,L5,                  
                     "\301",L10,L5,                  
                     "\301",L10,L2,L2,               
                     "\301",L10,L2,L2,               
                     "\301",L10,L5,                  
                     "\301",L10,L5,                  
                     "\301",L10,L2,L2,L1,               
                     "\331",                          
            '\033015'

             ---------------------------------------------------
             -- DETALLE DE TOTALES DE NSS UNICOS Y SUMA DE VIVIENDA
             ---------------------------------------------------
             PRINT COLUMN 1,"\332",L10,L1,
                            L10,L10,L10,L10,L10,L5,L1,L1,
                            L5,L1,
                            L10,L5,L1,
                            L10,L5,L1,
                            L10,L5,L1,
                            L10,L5,L1,
                            "\302",L10,L2,L2,
                            "\302",L10,L2,L2,
                            "\302",L10,L5,
                            "\302",L10,L5,
                            "\302",L10,L2,L2,L1,
                            "\277",
               '\033015'

                 PRINT 
                     COLUMN 001,"|",
                     COLUMN 015,"TOTAL DE NSS UNICOS : ",cont_nss_fin USING "#####",
                     COLUMN 140,"|",
                     COLUMN 141,lr_tot_fin_sb1.viv97_p +
                                lr_tot_fin_sb2.viv97_p +
                                lr_tot_fin_sb3.viv97_p +
                                lr_tot_fin_sb4.viv97_p +
                                lr_tot_fin_sb5.viv97_p  USING "#######&.&&",
                     COLUMN 155,"|",
                     COLUMN 156,lr_tot_fin_sb1.viv92_p +
                                lr_tot_fin_sb2.viv92_p +
                                lr_tot_fin_sb3.viv92_p +
                                lr_tot_fin_sb4.viv92_p +
                                lr_tot_fin_sb5.viv92_p  USING "#######&.&&",
                     COLUMN 170,"|",
                     COLUMN 171,lr_tot_fin_sb1.viv97   +
                                lr_tot_fin_sb2.viv97   +
                                lr_tot_fin_sb3.viv97   +
                                lr_tot_fin_sb4.viv97   +
                                lr_tot_fin_sb5.viv97    USING "#######&.&&&&&&",
                     COLUMN 186,"|",
                     COLUMN 187,lr_tot_fin_sb1.viv92   +
                                lr_tot_fin_sb2.viv92   +
                                lr_tot_fin_sb3.viv92   +
                                lr_tot_fin_sb4.viv92   +
                                lr_tot_fin_sb5.viv92    USING "#######&.&&&&&&",
                     COLUMN 202,"|",
                     COLUMN 218,"|",
            '\033015'
                                 
             PRINT COLUMN 1,"\300",L10,L1,
                            L10,L10,L10,L10,L10,L5,L1,L1,
                            L5,L1,
                            L10,L5,L1,
                            L10,L5,L1,
                            L10,L5,L1,
                            L10,L5,L1,
                            "\301",L10,L2,L2,
                            "\301",L10,L2,L2,
                            "\301",L10,L5,
                            "\301",L10,L5,
                            "\301",L10,L2,L2,L1,
                            "\331",
            '\033015'
END REPORT


#############################################################################
FUNCTION habil_siguiente(diaActual,numDiaHabil)
#hs--------------------------------------------
   DEFINE 
       diaTmp	                ,
       diaHabilSig	        ,
       diaActual	        DATE
   
   DEFINE
       cont_1                   ,
       numDiaHabil              ,

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



