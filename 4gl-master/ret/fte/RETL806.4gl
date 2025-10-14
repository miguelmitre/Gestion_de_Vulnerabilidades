#################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                             #
#Propietario       => E.F.P.                                                    #
#Programa RETL806  => Reporte de Provision de disposicion de Recursos           #
#Fecha             => 23 DE FEBRERO DEL 2004.                                   #
#Por               => MAURO MUNIZ CABALLERO / CLAUDIA URZUA ROSAS               #
#Actualizo         => JUAN CARLOS MENDOZA MORENO                                #
#Fecha Actualiza   => 29 DE JUNIO DE 2004                                       #
#Fecha Actualiza   => 29 DE JUNIO DE 2004                                       #
# Actualizacion    => JAVIER GONZALEZ JERONIMO                                  #
# Fecha            => 22 DE FEBRERO DE 2012                                     #
#                     Modificaciones relacionadas a los cambios de la ley del   #
#                     INFONAVIT (Req. EFPS-187)                                 #
#Sistema           => RET  					                                    #
#################################################################################

DATABASE safre_af
GLOBALS
    DEFINE 
        w_codigo_afore     LIKE tab_afore_local.codigo_afore 

    DEFINE 
        g_param_dis        RECORD LIKE seg_modulo.*

    DEFINE #glo #date
        gdt_cambio_infonavit    ,
        f_opera                 ,
        f_trans                 ,
        HOY                     DATE
   
    DEFINE #glo #char
        aux_pausa          CHAR(1)    ,
        enter              CHAR(0001) ,
        g_usuario          CHAR(0008) ,
        G_LISTA            CHAR(0300) ,
        G_IMPRE            CHAR(0300) ,
        impresion          CHAR(0300) ,
        where_clause       CHAR(0250) ,
        select_stmt5       CHAR(1000)  

    DEFINE v RECORD
        fecha_corte        DATE
    END RECORD

    DEFINE vparametros RECORD
        vfolio             INTEGER ,
        vfecha             DATE
    END RECORD

    DEFINE precio_de_liq   DECIMAL(19,14)
    DEFINE sw10            SMALLINT       
END GLOBALS


MAIN
    DEFER INTERRUPT
    OPTIONS PROMPT LINE LAST,
        INPUT WRAP,
        ACCEPT KEY control-o

    CALL startlog("RETL806.log")
    CALL inicio()     #i
    CALL Consulta()   #c

    PROMPT "  PROCESO FINALIZADO...  <ENTER>  PARA SALIR " FOR CHAR enter
END MAIN


FUNCTION inicio()
#i---------------
    
    LET gdt_cambio_infonavit    = "01/12/2012"    
    
    SELECT codigo_afore ,
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
    DEFINE #loc #integer
        cuenta_regis       INTEGER

    DEFINE #loc #date
        l_fecha            DATE   

    DEFINE folio LIKE dis_cuenta.folio
    DEFINE fecha_conversion LIKE dis_cuenta.fecha_conversion

    CLEAR SCREEN

    OPEN WINDOW ventana_21 AT 4,4 WITH FORM "RETL8061" ATTRIBUTE( BORDER)
    DISPLAY "RETL806                       (Ctrl-C) Salir                (ESC) Ejecutar     " AT 3,1 ATTRIBUTE(REVERSE,green)

    LET int_flag = FALSE

    CONSTRUCT BY NAME where_clause ON folio             ,
                                      fecha_conversion 
        ON KEY(ESC)
            LET int_flag =FALSE  
            EXIT  CONSTRUCT

        ON KEY(CONTROL-C)
            LET int_flag = TRUE
            EXIT CONSTRUCT 
    END CONSTRUCT

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
#aq---------------------------------
    DEFINE
        where_clause          CHAR(250) ,
        v_permisos            CHAR(110) 
	      
    DEFINE 
	v_cont_reg            ,
	v_folio_interno       INTEGER 

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
        tipo_prestacion       LIKE ret_solicitud_tx.tipo_prestacion ,
        fecha_resolucion      LIKE ret_solicitud_tx.fecha_resolucion
    END RECORD

    DEFINE lr_sol_rx    RECORD 
        diag_registro         LIKE ret_solicitud_rx.diag_registro,
        estado_sub_viv        LIKE ret_solicitud_rx.estado_sub_viv
    END RECORD

    DEFINE 
        cont_siefore          SMALLINT, 
        cual_siefore          SMALLINT 
    
    DEFINE 
	indica                CHAR(1)
 
        
    LET G_IMPRE = g_param_dis.ruta_listados CLIPPED,"/",
                  HOY USING "YYYYMMDD",".806"
    
    START REPORT rpt_cuenta_imp TO G_IMPRE

    CALL f_genera_tmp_cuenta(where_clause)

    ERROR " PROCESANDO... "
    
    --------------------------------------------------
    --SELECCION DE DATOS REQUERIDOS DE dis_provision
    --------------------------------------------------
    LET select_stmt5 = " SELECT CASE WHEN tipo_movimiento = 820 THEN 'D' ",
                       "             WHEN tipo_movimiento = 830 THEN 'E' ",
                       "             WHEN tipo_movimiento = 840 THEN 'F' ",
                       "             WHEN tipo_movimiento = 850 THEN 'G' ",
                       "             WHEN tipo_movimiento = 860 THEN 'H' ",
                       "             WHEN tipo_movimiento = 880 THEN 'J' ",
                       "             WHEN tipo_movimiento = 825 THEN 'M' END, ",
                       "             nss,folio,consecutivo_lote, siefore ",
                       " FROM  tmp_dat_dis_cta                                 ",
                       " WHERE ",where_clause CLIPPED,
                       " AND  subcuenta IN (1,2,4,5,6,7,8,9)                  ",
                       " AND  tipo_movimiento IN (820,830,840,850,860,880,825)",
                       " AND  siefore         IN (1,2,3,4,5,11)                     ",
                       " GROUP BY 1,2,3,4,5 ",
                       " ORDER BY 1,2,5,4,3 "

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

        IF where_clause[01,30] = "fecha_conversion" THEN
           LET v_folio_interno = 1
        ELSE
           LET v_folio_interno = l_record.folio
        END IF

        IF v_folio_interno = 1 THEN
            LET f_opera =  where_clause[33,42]
        END IF

        --SELECCION DE DATOS REQUERIDOS DE ret_monto_siefore y ret_solicitud_tx
        
        ERROR " SELECCIONANDO MONTOS... "

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
               SELECT A.acciones_ret97           ,
                      A.acciones_cv              ,
	              A.acciones_cs              ,
                      A.acciones_ret92           ,
                      A.siefore                  
               INTO   lr_montos.acciones_ret97   ,
                      lr_montos.acciones_cv      ,
                      lr_montos.acciones_cs      ,
                      lr_montos.acciones_ret92   ,
                      lr_montos.siefore          
               FROM   ret_monto_siefore A 
               WHERE  A.nss             = l_record.nss             
               AND    A.consecutivo     = l_record.consecutivo_lote 
               AND    A.siefore         = l_record.siefore       
	       AND    A.folio           = l_record.folio

	       IF lr_montos.acciones_ret97 IS NULL  OR lr_montos.acciones_ret97 = " "
	       THEN
	           LET lr_montos.acciones_ret97 = 0
               END IF  

	       IF lr_montos.acciones_cv IS NULL  OR lr_montos.acciones_cv = " "
	       THEN
	           LET lr_montos.acciones_cv = 0
               END IF  

	       IF lr_montos.acciones_cs IS NULL  OR lr_montos.acciones_cs = " "
	       THEN
	           LET lr_montos.acciones_cs = 0
               END IF  

	       IF lr_montos.acciones_ret92 IS NULL  OR lr_montos.acciones_ret92 = " "
	       THEN
	           LET lr_montos.acciones_ret92 = 0
               END IF  

               SELECT  B.saldo_viv97                    ,
                       B.saldo_viv97 * C.precio_del_dia ,
                       B.saldo_viv92                    ,
                       B.saldo_viv92 * C.precio_del_dia ,
                       B.saldo_viv72                    ,
                       B.tipo_seguro                    ,
                       B.tipo_pension                   ,
                       B.regimen                        ,
                       B.diag_registro                  ,
                       B.tipo_prestacion                ,
                       B.fecha_resolucion
               INTO    lr_sol_tx.saldo_viv97       ,
                       lr_sol_tx.saldo_viv97_p     ,
                       lr_sol_tx.saldo_viv92       ,
                       lr_sol_tx.saldo_viv92_p     ,
                       lr_sol_tx.saldo_viv72       ,
                       lr_sol_tx.tipo_seguro       ,
                       lr_sol_tx.tipo_pension      ,
                       lr_sol_tx.regimen           ,
                       lr_sol_rx.diag_registro     ,         
                       lr_sol_tx.tipo_prestacion   ,
                       lr_sol_tx.fecha_resolucion
               FROM    ret_solicitud_tx  B ,
	                   glo_valor_accion  C 
               WHERE   B.nss             = l_record.nss               
               AND     B.consecutivo     = l_record.consecutivo_lote  
               AND     B.folio           = l_record.folio      
               AND     C.fecha_valuacion = B.fecha_valor_viv  
               AND     C.codigo_siefore  = 11

          
               --SELECCION DEL DIAGNOSTICO y  ESTADO DE SUBCUENTA DE VIVIENDA
            
               SELECT 
                      estado_sub_viv
               INTO  
                      lr_sol_rx.estado_sub_viv
               FROM   ret_monto_viv       
               WHERE  nss         = l_record.nss             
               AND    consecutivo = l_record.consecutivo_lote
            
               IF SQLCA.SQLCODE = NOTFOUND THEN
                  INITIALIZE lr_sol_rx.* TO NULL
               END IF
            
               --VALIDACIONES
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
                  LET lr_montos.acciones_ret97  = 0
                  LET lr_montos.acciones_ret92  = 0
                  LET lr_montos.acciones_cv     = 0
                  LET lr_montos.acciones_cs     = 0
                  LET lr_sol_tx.saldo_viv97     = 0
                  LET lr_sol_tx.saldo_viv97_p   = 0
                  LET lr_sol_tx.saldo_viv92     = 0
                  LET lr_sol_tx.saldo_viv92_p   = 0
                  LET lr_sol_tx.saldo_viv72     = 0
               END IF
            
               IF lr_sol_rx.estado_sub_viv IS NULL OR 
                  lr_sol_rx.estado_sub_viv <> 1    OR 
                  lr_sol_rx.diag_registro  = 440   THEN
                  LET lr_sol_tx.saldo_viv97     = 0
                  LET lr_sol_tx.saldo_viv97_p   = 0
                  LET lr_sol_tx.saldo_viv92     = 0
                  LET lr_sol_tx.saldo_viv92_p   = 0
                  LET lr_sol_tx.saldo_viv72     = 0
               END IF

               IF l_record.tipo_retiro = "E" AND lr_sol_tx.regimen = 73 AND 
                  lr_sol_tx.fecha_resolucion > gdt_cambio_infonavit THEN
                    LET lr_sol_tx.saldo_viv97     = 0
                    LET lr_sol_tx.saldo_viv97_p   = 0
                    LET lr_sol_tx.saldo_viv92     = 0
                    LET lr_sol_tx.saldo_viv92_p   = 0
                    LET lr_sol_tx.saldo_viv72     = 0                  
                END IF 



               ERROR " GENERANDO DETALLE  "

               SELECT COUNT(UNIQUE siefore)
               INTO   cont_siefore
               FROM   tmp_dat_dis_cta
               WHERE  nss              = l_record.nss
               AND    consecutivo_lote = l_record.consecutivo_lote
               AND    siefore          IN(1,2,3,4,5)
          

               -- duda -------
               --IF cont_siefore = 2 AND l_record.siefore = 2 THEN
               IF cont_siefore > 1 AND sw10 > 1 THEN               
                  LET lr_sol_tx.saldo_viv97     = 0
                  LET lr_sol_tx.saldo_viv97_p   = 0
                  LET lr_sol_tx.saldo_viv92     = 0
                  LET lr_sol_tx.saldo_viv92_p   = 0
                  LET lr_sol_tx.saldo_viv72     = 0
               END IF

               OUTPUT TO REPORT rpt_cuenta_imp(l_record.*   ,
                                               lr_montos.*  ,
                                               lr_sol_tx.*  ,
                                               lr_sol_rx.*  ,    
                                               f_opera      ,
					       v_folio_interno)
           END IF
        ELSE
           SELECT  B.saldo_viv97                    ,
                   B.saldo_viv97 * C.precio_del_dia ,
                   B.saldo_viv92                    ,
                   B.saldo_viv92 * C.precio_del_dia ,
                   B.saldo_viv72                    ,
                   B.tipo_seguro                    ,
                   B.tipo_pension                   ,
                   B.regimen                        ,
                   B.tipo_prestacion                ,
                   B.fecha_resolucion
           INTO    lr_sol_tx.saldo_viv97       ,
                   lr_sol_tx.saldo_viv97_p     ,
                   lr_sol_tx.saldo_viv92       ,
                   lr_sol_tx.saldo_viv92_p     ,
                   lr_sol_tx.saldo_viv72       ,
                   lr_sol_tx.tipo_seguro       ,
                   lr_sol_tx.tipo_pension      ,
                   lr_sol_tx.regimen           ,
                   lr_sol_tx.tipo_prestacion   ,
                   lr_sol_tx.fecha_resolucion
           FROM    ret_solicitud_tx  B ,
	               glo_valor_accion  C 
           WHERE   B.nss             = l_record.nss               
           AND     B.consecutivo     = l_record.consecutivo_lote  
           AND     B.folio           = l_record.folio      
           AND     C.fecha_valuacion = B.fecha_valor_viv  
           AND     C.codigo_siefore  = 11

               IF l_record.tipo_retiro = "E" AND lr_sol_tx.regimen = 73 AND 
                  lr_sol_tx.fecha_resolucion > gdt_cambio_infonavit THEN
                    LET lr_sol_tx.saldo_viv97     = 0
                    LET lr_sol_tx.saldo_viv97_p   = 0
                    LET lr_sol_tx.saldo_viv92     = 0
                    LET lr_sol_tx.saldo_viv92_p   = 0
                    LET lr_sol_tx.saldo_viv72     = 0                  
                END IF 


	   LET l_record.siefore  = 1
	   LET lr_montos.acciones_ret97 = 0
	   LET lr_montos.acciones_cv = 0
	   LET lr_montos.acciones_cs = 0
	   LET lr_montos.acciones_ret92 = 0
	   LET lr_montos.siefore = 1

           OUTPUT TO REPORT rpt_cuenta_imp(l_record.*   ,
                                           lr_montos.*  ,
                                           lr_sol_tx.*  ,
                                           lr_sol_rx.*  ,    
                                           f_opera      ,
					   v_folio_interno)
        END IF
            
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


#---------------------------------------------------------------------------#
# f_genera_tmp_cuenta : Genera el extracto de discuenta con los movimientos #
#                       a utilizarse usando las fechas capturadas           #
#---------------------------------------------------------------------------#
FUNCTION f_genera_tmp_cuenta(pc_where)

    DEFINE 
        pc_where            CHAR(250)

    DEFINE lr_cuenta RECORD LIKE dis_provision.*
    
    DEFINE
        lc_query            CHAR(1000)
    
    -- -----------------------------------------------------------------------------

    WHENEVER ERROR CONTINUE
        DROP TABLE tmp_dat_dis_cta
    WHENEVER ERROR STOP

    SELECT *
    FROM   dis_provision
    WHERE  1 = 0
    INTO TEMP tmp_dat_dis_cta

    LET lc_query = " SELECT *           ", 
                   " FROM dis_provision ",
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

REPORT rpt_cuenta_imp(l_record,lr_montos,lr_sol_tx,lr_sol_rx,f_opera2,v_folio2)
#rci---------------------------------
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
        tipo_prestacion       LIKE ret_solicitud_tx.tipo_prestacion ,
        fecha_resolucion      LIKE ret_solicitud_tx.fecha_resolucion
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

    DEFINE reg_s1, reg_s2, reg_s3, reg_s4, reg_s5  ARRAY[7] OF RECORD
         tot               INTEGER,
         tipo_ret          CHAR(1),
         total             DECIMAL(16,6),
         r97               DECIMAL(16,6),
         cv                DECIMAL(16,6),
         cs                DECIMAL(16,6),
         viv97             DECIMAL(16,6),
         viv97p            DECIMAL(16,2),
         viv92             DECIMAL(16,6),
         viv92p            DECIMAL(16,2),
         r92               DECIMAL(16,6),
         viv72             DECIMAL(16,2)
    END RECORD

    DEFINE reg             ARRAY[7] OF RECORD
        cont_nss_uni      SMALLINT
    END RECORD  

    DEFINE lr_tot_fin_sb1 ,
           lr_tot_fin_sb3 ,
           lr_tot_fin_sb4 ,
           lr_tot_fin_sb5 ,
           lr_tot_fin_sb2 RECORD
        total             DECIMAL(16,6),
        ret97             DECIMAL(16,6),
        cv                DECIMAL(16,6),
        cs                DECIMAL(16,6),
        viv97_p           DECIMAL(16,2),
        viv92_p           DECIMAL(16,2),
        viv97             DECIMAL(16,6),
        viv92             DECIMAL(16,6),
        ret92             DECIMAL(16,6),
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

    DEFINE
        v_folio2             INTEGER
        
    OUTPUT
        TOP MARGIN    0
        BOTTOM MARGIN 0
        LEFT MARGIN   0
        --RIGHT MARGIN  200
        RIGHT MARGIN    0
        --PAGE LENGTH   90
        PAGE LENGTH   45
    
    ORDER BY l_record.tipo_retiro     ,
             l_record.nss             ,
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
        LET r_nom_rep = "RETL806"

        IF v_folio2 = 1 THEN
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

        PRINT COLUMN 054,"    REPORTE DE PROVISION DE DISPOSICION DE RECURSOS",
            '\033015'
        SKIP 2 LINE

        PRINT COLUMN 002,"AFORE               : ",rpt_afore.codigo_afore
                                                USING "###","  ",
                                                rpt_afore.razon_social CLIPPED,
              COLUMN 126,"PAGINA              :",pageno USING "##",
            '\033015'

        PRINT
        PRINT COLUMN 002,"PROGRAMA            : ",r_nom_rep,
              COLUMN 126,"FECHA VAL. TRANSFER : ",f_trans USING "DD-MM-YYYY",
            '\033015'

        PRINT
        PRINT COLUMN 002,"FOLIO INTERNO       : ",v_folio2 USING "<<<<<<<<<<",
              COLUMN 126,"FECHA GENERACION    : ",HOY USING "DD-MM-YYYY",
            '\033015'

        PRINT
        PRINT COLUMN 002,"FECHA DE OPERACION  : ",f_opera USING "DD-MM-YYYY",
            '\033015'

        --PRINT COLUMN 1,'\033e\033(s218T\033(s24H\033(s7B'

        PRINT COLUMN 1,"\332",L10,L1,
                       "\302",L10,L10,L10,L2,L1,
                       "\302",L5,L1,
                       "\302",L5,L2,
                       "\302",L5,L2,
                       "\302",L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,
                              L10,L10,L10,L5,L5,L1,
                       "\302",L5,
                       "\302",L2,L2,
                       "\277",
            '\033015'

        PRINT COLUMN 1,"|           |",
                       "                                 |",
                       " TIPO |",
                       " TIPO  |",
                       " TIPO  |",
                       "                                                          MONTOS A LIQUIDAR DE LA CUENTA INDIVIDUAL (ACCIONES)                                                   |",
                       "     |",
                       "    |" ,
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
                       "\302",L10,L5,
                       "\302",L10,L2,L2,
                       "\302",L10,L2,L2,
                       "\302",L5,
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
                       "  V I V  72   |",
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
                       "     PESOS    |",
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
                        "\301",L10,L5,
                        "\301",L10,L2,L2,
                        "\301",L10,L2,L2,
                        "\301",L5,
                        "\301",L2,L2,
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
        LET r_nom_rep = "RETL806"

        IF v_folio2 = 1 THEN
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

        PRINT COLUMN 054,"    REPORTE DE PROVISION DE DISPOSICION DE RECURSOS",
            '\033015'

        SKIP 2 LINE

        PRINT COLUMN 002,"AFORE               : ",rpt_afore.codigo_afore
                                                USING "###","  ",
                                                rpt_afore.razon_social CLIPPED,
       	      COLUMN 126,"PAGINA              :",pageno USING "##",
            '\033015'

        PRINT
        PRINT COLUMN 002,"PROGRAMA            : ",r_nom_rep,
              COLUMN 126,"FECHA VAL. TRANSFER : ",f_trans USING "DD-MM-YYYY",
            '\033015'

        PRINT
        PRINT COLUMN 002,"FOLIO INTERNO       : ",v_folio2 USING "<<<<<<<<<<",
              COLUMN 126,"FECHA GENERACION    : ",HOY USING "DD-MM-YYYY",
            '\033015'

        PRINT
        PRINT COLUMN 002,"FECHA DE OPERACION  : ",f_opera USING "DD-MM-YYYY",
            '\033015'

        --PRINT COLUMN 1,'\033e\033(s218T\033(s24H\033(s7B'

        PRINT COLUMN 1,"\332",L10,L1,
                       "\302",L10,L10,L10,L2,L1,
                       "\302",L5,L1,
                       "\302",L5,L2,
                       "\302",L5,L2,
                       "\302",L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,
                              L10,L10,L10,L5,L5,L1,
                       "\302",L5,
                       "\302",L2,L2,
                       "\277",
            '\033015'

        PRINT COLUMN 1,"|           |",
                       "                                 |",
                       " TIPO |",
                       " TIPO  |",
                       " TIPO  |",
                       "                                                          MONTOS A LIQUIDAR DE LA CUENTA INDIVIDUAL (ACCIONES)                                                   |",
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
                       "\302",L10,L5,
                       "\302",L10,L2,L2,
                       "\302",L10,L2,L2,
                       "\302",L5,
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
		       "  V I V  72   |",
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
			"     PESOS    |",
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
                        "\301",L10,L5,
                        "\301",L10,L2,L2,
                        "\301",L10,L2,L2,
                        "\301",L5,
                        "\301",L2,L2,
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
          LET sw10 = sw10 + 1
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

          PRINT COLUMN 002,l_record.nss              ,
                COLUMN 014,r_nombre[1,35]            ,
                COLUMN 050,lr_sol_tx.tipo_seguro     ,
                COLUMN 057,lr_sol_tx.tipo_pension    ,
                COLUMN 062,lr_sol_tx.tipo_prestacion ,
                COLUMN 072,nombre_siefore            ,
                COLUMN 077,lr_montos.acciones_ret97 + 
                           lr_montos.acciones_cv    + 
                           lr_montos.acciones_cs    + 
                           lr_montos.acciones_ret92  USING "#######&.&&&&&&" ,
                COLUMN 093,lr_montos.acciones_ret97  USING "#######&.&&&&&&" ,
                COLUMN 109,lr_montos.acciones_cv     USING "#######&.&&&&&&" ,
                COLUMN 125,lr_montos.acciones_cs     USING "#######&.&&&&&&" , 
                COLUMN 141,lr_sol_tx.saldo_viv97_p   USING "#######&.&&"     , 
                COLUMN 156,lr_sol_tx.saldo_viv92_p   USING "#######&.&&"     ,
                COLUMN 171,lr_sol_tx.saldo_viv97     USING "#######&.&&&&&&" , 
                COLUMN 187,lr_sol_tx.saldo_viv92     USING "#######&.&&&&&&" ,
                COLUMN 201,lr_montos.acciones_ret92  USING "#######&.&&&&&&" ,
                COLUMN 219,lr_sol_tx.saldo_viv72     USING "#######&.&&"     ,
                COLUMN 234,lr_sol_rx.diag_registro                           ,
                COLUMN 240,lr_sol_tx.regimen,                              
            '\033015'
          
          SKIP 1 LINE

     -----------------------------------
     BEFORE GROUP OF l_record.nss
     -----------------------------------
          LET cont_nss_unicos = cont_nss_unicos + 1
          LET sw10 = 0
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
          
          LET reg_s2[ind].viv72    = GROUP SUM(lr_sol_tx.saldo_viv72) 
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
          
          LET reg_s3[ind].viv72    = GROUP SUM(lr_sol_tx.saldo_viv72) 
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
          
          LET reg_s4[ind].viv72    = GROUP SUM(lr_sol_tx.saldo_viv72) 
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
          
          LET reg_s5[ind].viv72    = GROUP SUM(lr_sol_tx.saldo_viv72) 
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
                            "\302",L10,L5,
                            "\302",L10,L2,L2,
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
                   COLUMN 219,reg_s1[ind].viv72  USING "#######&.&&",
                   COLUMN 233,"|",
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
                            "\301",L10,L5,
                            "\301",L10,L2,L2,
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
                            "\302",L10,L5,
                            "\302",L10,L2,L2,
                            "\277",
            '\033015'
             
             PRINT COLUMN 001,"|",reg_s2[ind].tot  USING "#######"        ,
                   COLUMN 013,"|",
                   COLUMN 015,"TOTALES POR TIPO RETIRO  ",reg_s2[ind].tipo_ret,
                   COLUMN 070,"|",
                   COLUMN 072,"SB2"                                           ,
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
                   COLUMN 219,reg_s2[ind].viv72  USING "#######&.&&",
                   COLUMN 233,"|",
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
                            "\301",L10,L5,
                            "\301",L10,L2,L2,
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
                            "\302",L10,L5,
                            "\302",L10,L2,L2,
                            "\277",
            '\033015'
             
             PRINT COLUMN 001,"|",reg_s3[ind].tot  USING "#######"        ,
                   COLUMN 013,"|",
                   COLUMN 015,"TOTALES POR TIPO RETIRO  ",reg_s3[ind].tipo_ret,
                   COLUMN 070,"|",
                   COLUMN 072,"SB3"                                           ,
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
                   COLUMN 219,reg_s3[ind].viv72  USING "#######&.&&",
                   COLUMN 233,"|",
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
                            "\301",L10,L5,
                            "\301",L10,L2,L2,
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
                            "\302",L10,L5,
                            "\302",L10,L2,L2,
                            "\277",
            '\033015'
             
             PRINT COLUMN 001,"|",reg_s4[ind].tot  USING "#######"        ,
                   COLUMN 013,"|",
                   COLUMN 015,"TOTALES POR TIPO RETIRO  ",reg_s4[ind].tipo_ret,
                   COLUMN 070,"|",
                   COLUMN 072,"SB4"                                           ,
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
                   COLUMN 219,reg_s4[ind].viv72  USING "#######&.&&",
                   COLUMN 233,"|",
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
                            "\301",L10,L5,
                            "\301",L10,L2,L2,
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
                            "\302",L10,L5,
                            "\302",L10,L2,L2,
                            "\277",
            '\033015'
             
             PRINT COLUMN 001,"|",reg_s5[ind].tot  USING "#######"        ,
                   COLUMN 013,"|",
                   COLUMN 015,"TOTALES POR TIPO RETIRO  ",reg_s5[ind].tipo_ret,
                   COLUMN 070,"|",
                   COLUMN 072,"SB5"                                           ,
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
                   COLUMN 219,reg_s5[ind].viv72  USING "#######&.&&",
                   COLUMN 233,"|",
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
                            "\301",L10,L5,
                            "\301",L10,L2,L2,
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
                            "\302",L10,L5,
                            "\302",L10,L2,L2,
                            "\277",
            '\033015'

                 PRINT 
                     COLUMN 001,"|",
                     COLUMN 015,"TOTAL DE NSS UNICOS :  ",cont_nss_unicos USING "#####",
                     COLUMN 140,"|",
                     COLUMN 141,reg_s1[ind].viv97p +
                                reg_s3[ind].viv97p +
                                reg_s4[ind].viv97p +
                                reg_s5[ind].viv97p +
                                reg_s2[ind].viv97p    USING "#######&.&&",
                     COLUMN 155,"|",
                     COLUMN 156,reg_s1[ind].viv92p +
                                reg_s3[ind].viv92p +
                                reg_s4[ind].viv92p +
                                reg_s5[ind].viv92p +
                                reg_s2[ind].viv92p    USING "#######&.&&",
                     COLUMN 170,"|",
                     COLUMN 171,reg_s1[ind].viv97  +
                                reg_s3[ind].viv97  +
                                reg_s4[ind].viv97  +
                                reg_s5[ind].viv97  +
                                reg_s2[ind].viv97     USING "#######&.&&&&&&",
                     COLUMN 186,"|",
                     COLUMN 187,reg_s1[ind].viv92  +
                                reg_s3[ind].viv92  +
                                reg_s4[ind].viv92  +
                                reg_s5[ind].viv92  +
                                reg_s2[ind].viv92     USING "#######&.&&&&&&",
                     COLUMN 202,"|",
                     COLUMN 218,"|",
                     COLUMN 219,reg_s1[ind].viv72  +
                                reg_s3[ind].viv72  +
                                reg_s4[ind].viv72  +
                                reg_s5[ind].viv72  +
                                reg_s2[ind].viv72     USING "#######&.&&",
                     COLUMN 233,"|",
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
                            "\301",L10,L5,
                            "\301",L10,L2,L2,
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
          FOR i = 1 TO 7
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
                              "\302",L10,L5,
                              "\302",L10,L2,L2,
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
                     COLUMN 219,reg_s1[i].viv72  USING "#######&.&&",
                     COLUMN 233,"|",
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
                              "\301",L10,L5,
                              "\301",L10,L2,L2,
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
                              "\302",L10,L5,
                              "\302",L10,L2,L2,
                              "\277",
            '\033015'
               
               PRINT COLUMN 001,"|",reg_s2[i].tot  USING "#######"        ,
                     COLUMN 013,"|",
                     COLUMN 015,"TOTALES POR TIPO RETIRO  ",reg_s2[i].tipo_ret,
                     COLUMN 070,"|",
                     COLUMN 072,"SB2",
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
                     COLUMN 219,reg_s2[i].viv72  USING "#######&.&&",
                     COLUMN 233,"|",
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
                              "\301",L10,L5,
                              "\301",L10,L2,L2,
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
                              "\302",L10,L5,
                              "\302",L10,L2,L2,
                              "\277",
            '\033015'
               
               PRINT COLUMN 001,"|",reg_s3[i].tot  USING "#######"        ,
                     COLUMN 013,"|",
                     COLUMN 015,"TOTALES POR TIPO RETIRO  ",reg_s3[i].tipo_ret,
                     COLUMN 070,"|",
                     COLUMN 072,"SB3",
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
                     COLUMN 219,reg_s3[i].viv72  USING "#######&.&&",
                     COLUMN 233,"|",
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
                              "\301",L10,L5,
                              "\301",L10,L2,L2,
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
                              "\302",L10,L5,
                              "\302",L10,L2,L2,
                              "\277",
            '\033015'
               
               PRINT COLUMN 001,"|",reg_s4[i].tot  USING "#######"        ,
                     COLUMN 013,"|",
                     COLUMN 015,"TOTALES POR TIPO RETIRO  ",reg_s4[i].tipo_ret,
                     COLUMN 070,"|",
                     COLUMN 072,"SB4",
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
                     COLUMN 219,reg_s4[i].viv72  USING "#######&.&&",
                     COLUMN 233,"|",
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
                              "\301",L10,L5,
                              "\301",L10,L2,L2,
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
                              "\302",L10,L5,
                              "\302",L10,L2,L2,
                              "\277",
            '\033015'
               
               PRINT COLUMN 001,"|",reg_s5[i].tot  USING "#######"        ,
                     COLUMN 013,"|",
                     COLUMN 015,"TOTALES POR TIPO RETIRO  ",reg_s5[i].tipo_ret,
                     COLUMN 070,"|",
                     COLUMN 072,"SB5",
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
                     COLUMN 219,reg_s5[i].viv72  USING "#######&.&&",
                     COLUMN 233,"|",
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
                              "\301",L10,L5,
                              "\301",L10,L2,L2,
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
                               "\302",L10,L5,
                               "\302",L10,L2,L2,
                               "\277",
            '\033015'

                PRINT 
                    COLUMN 001,"|",
                    COLUMN 015,"TOTAL DE NSS UNICOS : ",reg[i].cont_nss_uni USING "#####",
                    COLUMN 140,"|",
                    COLUMN 141,reg_s1[i].viv97p +
                               reg_s3[i].viv97p +
                               reg_s4[i].viv97p +
                               reg_s5[i].viv97p +
                               reg_s2[i].viv97p    USING "#######&.&&",
                    COLUMN 155,"|",
                    COLUMN 156,reg_s1[i].viv92p +
                               reg_s3[i].viv92p +
                               reg_s4[i].viv92p +
                               reg_s5[i].viv92p +
                               reg_s2[i].viv92p    USING "#######&.&&",
                    COLUMN 170,"|",
                    COLUMN 171,reg_s1[i].viv97  +
                               reg_s3[i].viv97  +
                               reg_s4[i].viv97  +
                               reg_s5[i].viv97  +
                               reg_s2[i].viv97     USING "#######&.&&&&&&",
                    COLUMN 186,"|",
                    COLUMN 187,reg_s1[i].viv92  +
                               reg_s3[i].viv92  +
                               reg_s4[i].viv92  +
                               reg_s5[i].viv92  +
                               reg_s2[i].viv92     USING "#######&.&&&&&&",
                    COLUMN 202,"|",
                    COLUMN 218,"|",
                    COLUMN 219,reg_s1[i].viv72  +
                               reg_s3[i].viv72  +
                               reg_s4[i].viv72  +
                               reg_s5[i].viv72  +
                               reg_s2[i].viv72     USING "#######&.&&",
                    COLUMN 233,"|",
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
                               "\301",L10,L5,
                               "\301",L10,L2,L2,
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
                     "\302",L10,L5,
                     "\302",L10,L2,L2,
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
            COLUMN 219,lr_tot_fin_sb1.viv72   USING "#######&.&&" ,  
            COLUMN 233,"|",                       
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
                     "\301",L10,L5,                  
                     "\301",L10,L2,L2,               
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
                     "\302",L10,L5,
                     "\302",L10,L2,L2,
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
            COLUMN 219,lr_tot_fin_sb2.viv72   USING "#######&.&&" ,  
            COLUMN 233,"|",                       
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
                     "\301",L10,L5,                  
                     "\301",L10,L2,L2,               
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
                     "\302",L10,L5,
                     "\302",L10,L2,L2,
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
            COLUMN 219,lr_tot_fin_sb3.viv72   USING "#######&.&&" ,  
            COLUMN 233,"|",                       
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
                     "\301",L10,L5,                  
                     "\301",L10,L2,L2,               
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
                     "\302",L10,L5,
                     "\302",L10,L2,L2,
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
            COLUMN 219,lr_tot_fin_sb4.viv72   USING "#######&.&&" ,  
            COLUMN 233,"|",                       
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
                     "\301",L10,L5,                  
                     "\301",L10,L2,L2,               
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
                     "\302",L10,L5,
                     "\302",L10,L2,L2,
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
            COLUMN 219,lr_tot_fin_sb5.viv72   USING "#######&.&&" ,  
            COLUMN 233,"|",                       
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
                     "\301",L10,L5,                  
                     "\301",L10,L2,L2,               
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
                            "\302",L10,L5,
                            "\302",L10,L2,L2,
                            "\277",
            '\033015'

                 PRINT 
                     COLUMN 001,"|",
                     COLUMN 015,"TOTAL DE NSS UNICOS : ",cont_nss_fin USING "#####",
                     COLUMN 140,"|",
                     COLUMN 141,lr_tot_fin_sb1.viv97_p +
                                lr_tot_fin_sb3.viv97_p +
                                lr_tot_fin_sb4.viv97_p +
                                lr_tot_fin_sb5.viv97_p +
                                lr_tot_fin_sb2.viv97_p  USING "#######&.&&",
                     COLUMN 155,"|",
                     COLUMN 156,lr_tot_fin_sb1.viv92_p +
                                lr_tot_fin_sb3.viv92_p +
                                lr_tot_fin_sb4.viv92_p +
                                lr_tot_fin_sb5.viv92_p +
                                lr_tot_fin_sb2.viv92_p  USING "#######&.&&",
                     COLUMN 170,"|",
                     COLUMN 171,lr_tot_fin_sb1.viv97   +
                                lr_tot_fin_sb3.viv97   +
                                lr_tot_fin_sb4.viv97   +
                                lr_tot_fin_sb5.viv97   +
                                lr_tot_fin_sb2.viv97    USING "#######&.&&&&&&",
                     COLUMN 186,"|",
                     COLUMN 187,lr_tot_fin_sb1.viv92   +
                                lr_tot_fin_sb3.viv92   +
                                lr_tot_fin_sb4.viv92   +
                                lr_tot_fin_sb5.viv92   +
                                lr_tot_fin_sb2.viv92    USING "#######&.&&&&&&",
                     COLUMN 202,"|",
                     COLUMN 218,"|",
                     COLUMN 219,lr_tot_fin_sb1.viv72   +
                                lr_tot_fin_sb3.viv72   +
                                lr_tot_fin_sb4.viv72   +
                                lr_tot_fin_sb5.viv72   +
                                lr_tot_fin_sb2.viv72    USING "#######&.&&",
                     COLUMN 233,"|",
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
                            "\301",L10,L5,
                            "\301",L10,L2,L2,
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

