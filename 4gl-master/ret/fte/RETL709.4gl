###########################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                       #
#Propietario       => E.F.P.                                              #
#Programa RETL709  => Reporte de Provisión de Derecho Otorgado            #
#Fecha             => 23 DE FEBRERO DEL 2004.                             #
#Por               => MAURO MUNIZ CABALLERO / CLAUDIA URZUA ROSAS         #
#Actualizo         => JUAN CARLOS MENDOZA MORENO                          #
#Fecha Actualiza   => 29 DE JUNIO DE 2004                                 #
#Fecha Nvo Retiro  => Noviembre de 2011                                   #
#Sistema           => Retiros Referencia punto 2.5                        #
#MLM-2182          => Actualización por adecuación al nvo modelo          #
#                     de Servicios en Línea                               #
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
        HOY                DATE,
        ejecuta            CHAR(0500)
   
    DEFINE #glo #char
        aux_pausa          CHAR(1)    ,
        enter              CHAR(0001) ,
        g_usuario          CHAR(0008) ,
        G_LISTA            CHAR(0300) ,
        G_IMPRE            CHAR(0300) ,
        G_IMPRE_MOV        CHAR(0300) ,
        G_IMPRE_DET        CHAR(0300) ,
        impresion          CHAR(0300) ,
        where_clause       CHAR(0250) ,
        select_stmt5       CHAR(1500)  

    DEFINE v RECORD
        fecha_corte        DATE
    END RECORD

    DEFINE vparametros RECORD
        vfolio             INTEGER ,
        vfecha             DATE
    END RECORD

    DEFINE precio_de_liq   DECIMAL(19,14)
    DEFINE sw10            SMALLINT       

   DEFINE  gr_captura              RECORD
           folio                   INTEGER,
           fecha_liquida           DATE
   END RECORD 
   DEFINE  gr_edo                  RECORD
           provisionado            SMALLINT,
           liquidado               SMALLINT
   END RECORD
   DEFINE  ls_cuantos              SMALLINT 
   DEFINE  lc_query                CHAR(1500) 

    DEFINE reg_s1, reg_s2, reg_s3, reg_s4, reg_s5, reg_s10, reg_s11 ARRAY[7] OF RECORD
         tot               INTEGER,
         tipo_ret          SMALLINT,
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

    DEFINE 
        cont_tipo_retiro1   ,
        cont_tipo_retiro2   ,
        cont_tipo_retiro3   ,
        cont_tipo_retiro4   ,
        cont_tipo_retiro5   ,
        cont_tipo_retiro10  ,
        cont_tipo_retiro11  SMALLINT 

#MLM-2182
    DEFINE g_tot_tpomov ARRAY[3] OF RECORD
           tpo_mov       INTEGER,
           total         ,
           ret97         ,
           cv            ,
           cs            ,
           viv97         ,
           viv92         ,
           part_viv97    ,
           part_viv92    ,
           ret92         DECIMAL(18,6)
    END RECORD        

    DEFINE l_tipo_movimiento             ,
           l_subcuenta                   ,
           l_cont                SMALLINT,
           l_monto               DECIMAL(18,6)  

END GLOBALS


MAIN
    DEFER INTERRUPT
    OPTIONS PROMPT LINE LAST,
        INPUT WRAP,
        ACCEPT KEY control-o

     CALL STARTLOG(FGL_GETENV("USER")||".RETL709.log")
    CALL inicio()     #i
    CALL Consulta()   #c

END MAIN


FUNCTION inicio()
#i---------------
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

    ----- ESTADOS DE S0LICITUD -----
    SELECT  A.estado_solicitud
      INTO  gr_edo.provisionado
      FROM  ret_estado A
     WHERE  A.descripcion = "PROVISIONADO DERECHO"


    SELECT  A.estado_solicitud
      INTO  gr_edo.liquidado
      FROM  ret_estado A
     WHERE  A.descripcion = "LIQUIDADO"

    LET lc_query  = " SELECT tipo_movimiento,             ",
                    "        subcuenta,                   ",
                    "        SUM(monto_en_acciones),      ",
                    "        SUM(monto_en_pesos)          ",
                    " FROM   safre_af:dis_provision       ",
                    " WHERE  nss              =  ?        ",
                    " AND    consecutivo_lote =  ?        ",
                    " AND    folio            =  ?        ",
                    " AND    siefore          =  ?        ",
                    " GROUP BY 1 ,2                       ",
                    " ORDER BY 1                          "
    PREPARE  eje_montos     FROM           lc_query
    DECLARE  cur_montos     CURSOR FOR     eje_montos

    LET lc_query  = " SELECT SUM(monto_en_acciones),      ",
                    "        SUM(monto_en_pesos)          ",
                    " FROM   safre_af:dis_provision       ",
                    " WHERE  nss              =  ?        ",
                    " AND    consecutivo_lote =  ?        ",
                    " AND    folio            =  ?        ",
                    " AND    siefore          =  11       ",
                    " AND    subcuenta        =  ?        "
    PREPARE  p_sel_viv      FROM           lc_query

    LET  lc_query  = " SELECT  tipo_pension,                ",
                     "         tipo_seguro,                 ",
                     "         tipo_prestacion,             ",
                     "         regimen,                     ",
                     "         estado_sub_viv,              ",
                     "         diag_datamart                ",
                     "   FROM  ret_det_datamart             ",
                     "  WHERE  nss              =  ?        ",
                     "    AND  diag_datamart    =  101      ",
                     "  ORDER  BY fec_carga_datamart        "
    PREPARE  d_sel_dat      FROM           lc_query
    DECLARE  p_sel_dat      CURSOR FOR     d_sel_dat 

END FUNCTION


#---------------------------------------------------------------------------
# FUNCTION QUE ABRE LA PANTALLA DE CAPTURA
#---------------------------------------------------------------------------
FUNCTION Consulta()
#c-----------------
    DEFINE #loc #integer
        cuenta_regis       INTEGER
    DEFINE v_permisos      CHAR(110)         

    DEFINE #loc #date
        l_fecha            DATE   

    OPEN WINDOW RETL7091 AT 4,4 WITH FORM "RETL7091" ATTRIBUTE (BORDER)
    DISPLAY " <Esc> Ejecutar                                                   < Ctrl-C > " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETL7091         REPORTE DE PROVISION DE DERECHO OTORGADO                   " AT 3,1 ATTRIBUTE (REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE (REVERSE)

    INITIALIZE gr_captura.* TO null

    INPUT BY NAME gr_captura.* WITHOUT DEFAULTS
          AFTER FIELD folio
                IF gr_captura.folio IS NOT NULL THEN

                         SELECT  a.fecha_conversion
                           INTO  gr_captura.fecha_liquida
                           FROM  safre_af:dis_provision    a,
                                 safre_af:ret_det_datamart b
                          WHERE  a.folio              =  gr_captura.folio
                            AND  a.tipo_movimiento  IN ( 800, 810, 815 )
                            AND  a.nss                =  b.nss
                            AND  a.consecutivo_lote   =  b.id_registro
                            AND  b.estado             =  gr_edo.provisionado
                          GROUP  BY 1

                         IF  SQLCA.SQLCODE = 0 THEN
                             DISPLAY BY NAME gr_captura.fecha_liquida
                         ELSE
                             ERROR "NO EXISTE INFORMACION PARA ESTE FOLIO"
                             SLEEP 2
                             ERROR ""
                             NEXT FIELD folio
                         END IF
                END IF

          AFTER FIELD fecha_liquida
                IF gr_captura.fecha_liquida IS NULL THEN
                         IF  gr_captura.folio IS NULL THEN
                             ERROR "DEBE INDICAR FOLIO O FECHA DE PRELIQUIDACION"
                             SLEEP 2
                             ERROR ""
                             NEXT FIELD folio
                         END IF
                ELSE
                         SELECT  COUNT(*)
                           INTO  ls_cuantos
                           FROM  safre_af:dis_provision a,
                                 safre_af:ret_det_datamart b
                          WHERE  a.fecha_conversion     =  gr_captura.fecha_liquida
                            AND  a.tipo_movimiento     IN  ( 800, 810, 815 ) 
                            AND  a.nss                =  b.nss
                            AND  a.consecutivo_lote   =  b.id_registro
                            AND  b.estado             =  gr_edo.provisionado 

                         IF  SQLCA.SQLCODE = 0 THEN
                             DISPLAY BY NAME gr_captura.folio
                         ELSE
                             ERROR "NO EXISTE INFORMACION PARA ESTA FECHA"
                             SLEEP 2
                             ERROR ""
                             NEXT FIELD folio
                         END IF
                END IF

          ON KEY (ESC)
                 IF    gr_captura.folio         IS NULL
                  AND  gr_captura.fecha_liquida IS NULL THEN
                       ERROR "DEBE INDICAR FOLIO O FECHA DE PRELIQUIDACION"
                       SLEEP 2
                       ERROR ""
                       NEXT FIELD folio
                 END IF

                 SELECT  a.fecha_conversion
                   INTO  gr_captura.fecha_liquida
                   FROM  safre_af:dis_provision    a,
                         safre_af:ret_det_datamart b
                  WHERE  a.folio              =  gr_captura.folio
                    AND  a.tipo_movimiento  IN ( 800, 810, 815 )
                    AND  a.nss                =  b.nss
                    AND  a.consecutivo_lote   =  b.id_registro
                    AND  b.estado             =  gr_edo.provisionado
                  GROUP  BY 1

                 IF  SQLCA.SQLCODE <> 0 THEN
                     ERROR "NO EXISTE INFORMACION PARA ESTE FOLIO"
                     SLEEP 2
                     ERROR ""
                     NEXT FIELD folio
                 END IF
                 EXIT INPUT

        ON KEY (INTERRUPT, CONTROL-C)
                PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
                EXIT PROGRAM
    END INPUT

    DISPLAY " PROCESANDO INFORMACION " --AT 19,1 ATTRIBUTE(REVERSE)
    CALL Arma_Consulta()

    LET ejecuta = "cd ",g_param_dis.ruta_rescate  CLIPPED,    
               "; cat ", G_IMPRE_MOV CLIPPED, " ",
               G_IMPRE_DET CLIPPED, " > ", G_IMPRE      
    
    RUN ejecuta     
    
   LET ejecuta = "cd ",g_param_dis.ruta_rescate  CLIPPED,    
               "; rm ", G_IMPRE_DET CLIPPED,
               "; rm ", G_IMPRE_MOV CLIPPED

    RUN ejecuta

    LET v_permisos = " chmod 777 ",G_IMPRE
    RUN v_permisos
        
    DISPLAY " ARCHIVO:", G_IMPRE --CLIPPED AT 18,1 ATTRIBUTE(REVERSE)
    PROMPT  " PROCESO FINALIZADO... < ENTER > PARA SALIR " FOR CHAR enter
    CLOSE WINDOW RETL7091

END FUNCTION


#---------------------------------------------------------------------------
# FUNCION QUE ARMA EL QUERY
#---------------------------------------------------------------------------
FUNCTION Arma_Consulta()
#aq---------------------------------
    DEFINE
        where_clause          CHAR(250) ,
        v_permisos            CHAR(110) 
	      
    DEFINE 
	v_cont_reg            ,
	v_folio_interno       INTEGER 

    DEFINE l_record RECORD
        tipo_retiro           SMALLINT                             ,
        nss                   LIKE dis_provision.nss               ,
        folio                 LIKE dis_provision.folio             ,
        consecutivo_lote      LIKE dis_provision.consecutivo_lote  ,
        siefore               LIKE dis_provision.siefore           
    END RECORD

    DEFINE lr_montos   RECORD   
    	  tipo_movimiento       SMALLINT                              ,    	     
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
    DEFINE  cont_siefore          SMALLINT, 
            cual_siefore          SMALLINT 
    DEFINE  indica                CHAR(1)
    DEFINE  lr_paso               RECORD
    	      tipo_movimiento       SMALLINT,
            subcuenta             SMALLINT,
            siefore               SMALLINT,
            acciones              DECIMAL(22,6),
            pesos                 DECIMAL(22,6)
    END RECORD
    DEFINE  ls_subcta             SMALLINT
    
    LET G_IMPRE_MOV = g_param_dis.ruta_listados CLIPPED,"/",
                  g_usuario CLIPPED,
                  ".DER_OTORGA_ENC"

    LET G_IMPRE_DET = g_param_dis.ruta_listados CLIPPED,"/",
                  g_usuario CLIPPED,
                  ".DER_OTORGA"

    LET G_IMPRE = g_param_dis.ruta_listados CLIPPED,"/",
                  g_usuario CLIPPED,
                  ".RPT_PROV_DEROTO",
                  ".",
                  HOY USING "DDMMYYYY"
    
 START REPORT rpt_cuenta_imp TO G_IMPRE_DET
    CALL  f_ini()
    ERROR " PROCESANDO... "
    
    --------------------------------------------------
    --SELECCION DE DATOS REQUERIDOS DE dis_provision
    --------------------------------------------------
    LET select_stmt5 = "SELECT  tipo_movimiento,                                ",
                       "        nss, folio, consecutivo_lote, siefore           ",
                       "  FROM  dis_provision                                   ",
                       " WHERE  fecha_conversion      =   ?                     ",
                       "   AND  folio                 =   ?                     ", 
                       "   AND  subcuenta IN (1,2,4,5,6,7,8,9)                  ",
                       "   AND  tipo_movimiento IN (800, 810, 815)              ",
                       " GROUP  BY 1,2,3,4,5 ",
                       " ORDER  BY 1,2,5     "

    PREPARE  cust_stmt1     FROM      select_stmt5
    DECLARE  rept_cur     CURSOR FOR  cust_stmt1
        
    LET v_cont_reg  =  0
    FOREACH rept_cur     USING  gr_captura.fecha_liquida,
                                gr_captura.folio
                          INTO  l_record.tipo_retiro      ,
                                l_record.nss              ,
                                l_record.folio            , 
                                l_record.consecutivo_lote ,
                                l_record.siefore          

        ERROR " PROCESANDO REGISTROS... "
        
        LET v_cont_reg = v_cont_reg + 1
        LET v_folio_interno = l_record.folio

    # MLM-2182 Se agrega el encabezado para los totales por tipo de movimiento
    -----------------------------------------------------------------
    -- SELECCIONA LA INFORNACION REQUERIDA PARA EL ENCABEZADO
    -- DE TOTALES POR TIPO DE MOVIMIENTO
    -----------------------------------------------------------------
    #MLM-2182    	
    START REPORT rpt_total_tpomov TO G_IMPRE_MOV    
    
    LET lc_query  = " SELECT tipo_movimiento       ,         ",
                    "        subcuenta             ,         ",
                    "        SUM(monto_en_acciones)         ",
                   -- "        SUM(monto_en_pesos)             ",
                    " FROM   safre_af:dis_provision          ",
                    " WHERE  folio            =  ?       ",
                    " AND  tipo_movimiento IN (800, 810, 815)",
                    " GROUP BY 1,2                           ",
                    " ORDER BY 1                             "

    PREPARE  eje_montos_tot     FROM       lc_query
    DECLARE  cur_montos_tot     CURSOR FOR  eje_montos_tot    
    
    # Inicializando el arreglo
    FOR l_cont = 1 TO 3
       INITIALIZE g_tot_tpomov[l_cont].* TO NULL
       
       LET g_tot_tpomov[l_cont].total      = 0
       LET g_tot_tpomov[l_cont].ret97      = 0
       LET g_tot_tpomov[l_cont].cv         = 0
       LET g_tot_tpomov[l_cont].cs         = 0
       LET g_tot_tpomov[l_cont].viv97      = 0
       LET g_tot_tpomov[l_cont].viv92      = 0
       LET g_tot_tpomov[l_cont].part_viv97 = 0
       LET g_tot_tpomov[l_cont].part_viv92 = 0
       LET g_tot_tpomov[l_cont].ret92      = 0
    END FOR     	
    	
    	FOREACH cur_montos_tot USING l_record.folio
    		                     INTO 
    		                          l_tipo_movimiento, 
                                  l_subcuenta,
                                  l_monto

      CASE 
      	 WHEN  l_tipo_movimiento = 800
			           LET l_cont = 1
         WHEN l_tipo_movimiento = 810
         	       LET l_cont = 2
         WHEN l_tipo_movimiento = 815
         	       LET l_cont = 3
       END CASE			
			                          
       LET g_tot_tpomov[l_cont].tpo_mov = l_tipo_movimiento
       LET g_tot_tpomov[l_cont].total   = g_tot_tpomov[l_cont].total + l_monto
             
       CASE
          WHEN l_subcuenta = 1    # Retiro 91
               LET g_tot_tpomov[l_cont].ret97 = l_monto
          WHEN l_subcuenta = 2 OR # Cesantia y Vejez
               l_subcuenta = 6 OR
               l_subcuenta = 9           
               LET g_tot_tpomov[l_cont].cv    = g_tot_tpomov[l_cont].cv + l_monto            
          WHEN l_subcuenta = 5    # Cuota Social
               LET g_tot_tpomov[l_cont].cs    = l_monto
          WHEN l_subcuenta = 7    # Retiro 92
               LET g_tot_tpomov[l_cont].ret92 = l_monto
       END CASE
    END FOREACH
    
    OUTPUT TO REPORT rpt_total_tpomov(l_record.*,v_folio_interno)             # MLM-2182 Se agrega el parámetro para saber que se trata de los totales por Tipo movimiento
    FINISH REPORT rpt_total_tpomov  
    # FIN MLM-2182    

        --SELECCION DE DATOS REQUERIDOS DE ret_monto_siefore y ret_solicitud_tx
        
        ERROR " SELECCIONANDO MONTOS... "

        LET    lr_montos.acciones_ret97  =  0 
        LET    lr_montos.acciones_cv     =  0 
        LET    lr_montos.acciones_cs     =  0 
        LET    lr_montos.acciones_ret92  =  0 
        LET    lr_sol_tx.saldo_viv97     =  0
        LET    lr_sol_tx.saldo_viv97_p   =  0
        LET    lr_sol_tx.saldo_viv92     =  0
        LET    lr_sol_tx.saldo_viv92_p   =  0
        FOREACH  cur_montos USING   l_record.nss        ,
                                    l_record.consecutivo_lote,
                                    l_record.folio,
                                    l_record.siefore
                             INTO   lr_paso.tipo_movimiento,
                                    lr_paso.subcuenta,
                                    lr_paso.acciones ,
                                    lr_paso.pesos

           IF   lr_paso.acciones IS NULL THEN
                 LET lr_paso.acciones = 0
           END IF

           IF   lr_paso.pesos IS NULL THEN
                 LET lr_paso.pesos = 0
           END IF

           LET lr_montos.tipo_movimiento = lr_paso.tipo_movimiento
           
           CASE     WHEN  lr_paso.subcuenta   =  1
                          LET  lr_montos.acciones_ret97     =   lr_montos.acciones_ret97
                                                            +   lr_paso.acciones
                    WHEN  lr_paso.subcuenta   =  2
                      OR  lr_paso.subcuenta   =  6
                      OR  lr_paso.subcuenta   =  9
                          LET  lr_montos.acciones_cv        =   lr_montos.acciones_cv   
                                                            +   lr_paso.acciones
                    WHEN  lr_paso.subcuenta   =  5
                          LET  lr_montos.acciones_cs        =   lr_montos.acciones_cs      
                                                            +   lr_paso.acciones
                    WHEN  lr_paso.subcuenta   =  7
                          LET  lr_montos.acciones_ret92     =   lr_montos.acciones_ret92  
                                                            +   lr_paso.acciones
                    ---WHEN  lr_paso.subcuenta   =  4
                    ---      LET  lr_sol_tx.saldo_viv97        =   lr_sol_tx.saldo_viv97 
                    ---                                        +   lr_paso.acciones
                    ---      LET  lr_sol_tx.saldo_viv97_p      =   lr_sol_tx.saldo_viv97_p
                    ---                                        +   lr_paso.pesos  
                    ---WHEN  lr_paso.subcuenta   =  8
                    ---      LET  lr_sol_tx.saldo_viv92        =   lr_sol_tx.saldo_viv92 
                    ---                                        +   lr_paso.acciones
                    ---      LET  lr_sol_tx.saldo_viv92_p      =   lr_sol_tx.saldo_viv92_p
                    ---                                        +   lr_paso.pesos  
           END CASE
        END FOREACH

        ### Aqui tengo duda .... mandarla a Franco ..     
        LET      ls_subcta                =  4
        EXECUTE  p_sel_viv            USING  l_record.nss        ,
                                             l_record.consecutivo_lote,
                                             l_record.folio,
                                             ls_subcta
                                       INTO  lr_paso.acciones,
                                             lr_paso.pesos
        IF   STATUS = NOTFOUND   THEN
             LET  lr_sol_tx.saldo_viv97        =  0                         
             LET  lr_sol_tx.saldo_viv97_p      =  0
        ELSE
             LET  lr_sol_tx.saldo_viv97        =   lr_paso.acciones
             LET  lr_sol_tx.saldo_viv97_p      =   lr_paso.pesos  
        END IF
  
        LET      ls_subcta                =  8
        EXECUTE  p_sel_viv            USING  l_record.nss        ,
                                             l_record.consecutivo_lote,
                                             l_record.folio,
                                             ls_subcta
                                       INTO  lr_paso.acciones,
                                             lr_paso.pesos
        IF   STATUS = NOTFOUND   THEN
             LET  lr_sol_tx.saldo_viv92        =   0
             LET  lr_sol_tx.saldo_viv92_p      =   0
        ELSE
             LET  lr_sol_tx.saldo_viv92        =   lr_paso.acciones
             LET  lr_sol_tx.saldo_viv92_p      =   lr_paso.pesos
        END IF

        IF    lr_montos.acciones_ret97    IS  NULL  
          OR  lr_montos.acciones_ret97     =  " "     THEN
	      LET lr_montos.acciones_ret97 =  0
        END IF  

        IF    lr_montos.acciones_cv       IS NULL 
          OR  lr_montos.acciones_cv = " " THEN
              LET lr_montos.acciones_cv = 0
        END IF  

        IF    lr_montos.acciones_cs       IS NULL 
          OR  lr_montos.acciones_cs = " " THEN
              LET lr_montos.acciones_cs = 0
        END IF  

        IF    lr_montos.acciones_ret92 IS NULL  
          OR  lr_montos.acciones_ret92 = " " THEN
              LET lr_montos.acciones_ret92 = 0
        END IF 

        IF    lr_sol_tx.saldo_viv97    IS NULL  
          OR  lr_sol_tx.saldo_viv97    = " " THEN
              LET lr_sol_tx.saldo_viv97  = 0
        END IF 

        IF    lr_sol_tx.saldo_viv97_p  IS NULL  
          OR  lr_sol_tx.saldo_viv97_p  = " " THEN
              LET lr_sol_tx.saldo_viv97_p   = 0
        END IF 

        IF    lr_sol_tx.saldo_viv92    IS NULL  
          OR  lr_sol_tx.saldo_viv92    = " " THEN
              LET lr_sol_tx.saldo_viv92   = 0 
        END IF 

        IF    lr_sol_tx.saldo_viv92_p    IS NULL  
          OR  lr_sol_tx.saldo_viv92_p    = " " THEN
              LET lr_sol_tx.saldo_viv92_p   = 0 
        END IF 

        LET       lr_montos.siefore                         =   l_record.siefore   
        LET       lr_sol_tx.saldo_viv72                     =   0 
        LET       lr_sol_tx.tipo_seguro                     =   0
        LET       lr_sol_tx.regimen                         =   0   
        LET       lr_sol_tx.tipo_prestacion                 =   0 
        LET       lr_sol_rx.diag_registro                   =   0 

        FOREACH  p_sel_dat          USING  l_record.nss
                                     INTO  lr_sol_tx.tipo_pension,
                                           lr_sol_tx.tipo_seguro, 
                                           lr_sol_tx.tipo_prestacion,
                                           lr_sol_tx.regimen,
                                           lr_sol_rx.estado_sub_viv,
                                           lr_sol_rx.diag_registro 
        END FOREACH 

        IF   STATUS = NOTFOUND    THEN
             LET   lr_sol_tx.tipo_pension                    =  0 
             LET   lr_sol_tx.tipo_seguro                     =  0
             LET   lr_sol_tx.tipo_prestacion                 =  0
             LET   lr_sol_tx.regimen                         =  0
             LET   lr_sol_rx.estado_sub_viv                  =  0 
        END IF 

        ERROR " GENERANDO DETALLE  "

        CALL   f_Acumula(l_record.*, lr_montos.*, lr_sol_tx.* ) 

        OUTPUT TO REPORT rpt_cuenta_imp(l_record.*   ,
                                        lr_montos.*  ,
                                        lr_sol_tx.*  ,
                                        lr_sol_rx.*  ,    
                                        f_opera      ,
                                        v_folio_interno)
    
    END FOREACH  

    IF ( v_cont_reg <= 0 ) THEN
         ERROR "   NO EXISTEN REGISTROS PARA ESTE CRITERIO...   "
         SLEEP 4
         ERROR " "
         EXIT PROGRAM
    END IF 

   FINISH REPORT rpt_cuenta_imp

    ERROR ""

    LET v_permisos = " chmod 777 ",G_IMPRE_DET
    RUN v_permisos
        
    PROMPT "  DESEA GENERAR IMPRESION (S/N)?  "
    FOR CHAR aux_pausa

    IF aux_pausa MATCHES "[Ss]" THEN
       LET impresion = "lp ",G_IMPRE_DET
       RUN impresion
    END IF
END FUNCTION

REPORT rpt_total_tpomov (l_record,v_folio2) 
    DEFINE l_record RECORD
        tipo_retiro           SMALLINT                             ,
        nss                   LIKE dis_provision.nss               ,
        folio                 LIKE dis_provision.folio             ,
        consecutivo_lote      LIKE dis_provision.consecutivo_lote  ,
        siefore               LIKE dis_provision.siefore           
    END RECORD

    DEFINE lr_montos   RECORD
    	  tipo_movimiento       SMALLINT                              ,    	
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
        nombre_siefore      CHAR(04) ,
        r_nombre            CHAR(60) ,
        r_nom_rep           CHAR(07)

    DEFINE #loc #smallint
        ind                 , 
        i                   SMALLINT

    DEFINE rpt_afore RECORD
        codigo_afore        LIKE safre_af:tab_afore_local.codigo_afore ,
        razon_social        LIKE safre_af:tab_afore_local.razon_social
    END RECORD

    DEFINE reg             ARRAY[7] OF RECORD
        cont_nss_uni      SMALLINT
    END RECORD  

    DEFINE lr_tot_fin_sb1 ,
           lr_tot_fin_sb3 ,
           lr_tot_fin_sb4 ,
           lr_tot_fin_sb5 ,
           lr_tot_fin_sb10 ,
           lr_tot_fin_sb11 ,
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
        PAGE LENGTH   22
    
--    ORDER BY l_record.tipo_retiro     ,
--             l_record.nss             ,
--             lr_montos.siefore     
 
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

        PRINT '\033E\033(10U\033&l1O\033&k2S\033&18d',
            '\033e\033(s23H'

        SELECT codigo_afore           ,
               razon_social
        INTO   rpt_afore.codigo_afore , 
               rpt_afore.razon_social
        FROM safre_af:tab_afore_local

        LET hoy       = TODAY
        LET r_nom_rep = "RETL709"

        LET  f_opera   =  gr_captura.fecha_liquida 
        LET  f_trans   =  gr_captura.fecha_liquida


        IF rpt_afore.codigo_afore = 532 THEN
            LET encabezado = "SUBDIRECCION DE BENEFICIOS"
        ELSE 
            LET encabezado = "   MODULO DE RETIROS      "     
        END IF   

        PRINT COLUMN 067,encabezado,
            '\033015'
        SKIP 1 LINE

        PRINT COLUMN 054,"    REPORTE DE PROVISION DE DERECHO OTORGADO       ",
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

        PRINT COLUMN 1,"\332",L10,L1,
                       "\302",L10,L10,L10,L2,L1,
                       "\302",L5,L1,
                       "\302",L5,L2,
                       "\302",L5,L2,
                       "\302",L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,
                              L10,L10,L5,L5,L2,L2,L2,L2,
                       "\277",
            '\033015'


        PRINT COLUMN 1,"|     TIPO         |",
                       "                   | ",
                       "       R97      |",
                       "       CV       |",
                       "      CS        |",
                       "  V I V  97     |",
                       "  V I V  92     |",
                       "P A R T  VIV97  |",
                       "P A R T  VIV92  |",
                       "      R92       |",
            '\033015'

        PRINT COLUMN 1,
                       "|  MOVIMIENTO      |",
                       "      TOTAL        | ",
                       "                |",
                       "                |",
                       "                |",
                       "                |",
                       "                |",
                       "                |",
                       "                |",
                       "                |",
            '\033015'
        
        PRINT COLUMN 1,"\332",L10,L1,
                       "\302",L10,L10,L10,L2,L1,
                       "\302",L5,L1,
                       "\302",L5,L2,
                       "\302",L5,L2,
                       "\302",L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,
                              L10,L10,L5,L5,L2,L2,L2,L2,
                       "\277",
            '\033015'

     ---------------------------------
     ON EVERY ROW
     ---------------------------------      
              
           FOR l_cont = 1 TO 3
           	
              IF g_tot_tpomov[l_cont].tpo_mov IS NOT NULL THEN
                  PRINT COLUMN 006,g_tot_tpomov[l_cont].tpo_mov       USING "###"                  ,  
                        COLUMN 020,g_tot_tpomov[l_cont].total         USING "#########&.&&&&&&"    ,
                        COLUMN 039,g_tot_tpomov[l_cont].ret97         USING "#########&.&&&&&&"    ,
                        COLUMN 058,g_tot_tpomov[l_cont].cv            USING "#########&.&&&&&&"    ,
                        COLUMN 073,g_tot_tpomov[l_cont].cs            USING "#########&.&&&&&&"    , 
                        COLUMN 080,g_tot_tpomov[l_cont].viv97         USING "#########&.&&&&&&"    ,    
                        COLUMN 098,g_tot_tpomov[l_cont].viv92         USING "#########&.&&&&&&"    ,
                        COLUMN 117,g_tot_tpomov[l_cont].part_viv97    USING "#########&.&&&&&&"    ,
                        COLUMN 136,g_tot_tpomov[l_cont].part_viv92    USING "#########&.&&&&&&"    ,
                        COLUMN 155,g_tot_tpomov[l_cont].ret92         USING "#########&.&&&&&&"     
              END IF   
           END FOR 
                          
                          
        PRINT COLUMN 1,"\332",L10,L1,
                       "\302",L10,L10,L10,L2,L1,
                       "\302",L5,L1,
                       "\302",L5,L2,
                       "\302",L5,L2,
                       "\302",L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,
                              L10,L10,L5,L5,L2,L2,L2,L2,
                       "\277",
            '\033015'
            
            # FIN MLM-2182                      
END REPORT

REPORT rpt_cuenta_imp(l_record,lr_montos,lr_sol_tx,lr_sol_rx,f_opera2,v_folio2)
#rci---------------------------------
    DEFINE l_record RECORD
        tipo_retiro           SMALLINT                             ,
        nss                   LIKE dis_provision.nss               ,
        folio                 LIKE dis_provision.folio             ,
        consecutivo_lote      LIKE dis_provision.consecutivo_lote  ,
        siefore               LIKE dis_provision.siefore           
    END RECORD

    DEFINE lr_montos   RECORD
    	  tipo_movimiento       SMALLINT                              ,
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
        nombre_siefore      CHAR(04) ,
        r_nombre            CHAR(60) ,
        r_nom_rep           CHAR(07)

    DEFINE #loc #smallint
        ind                 , 
        i                   SMALLINT

    DEFINE rpt_afore RECORD
        codigo_afore        LIKE safre_af:tab_afore_local.codigo_afore ,
        razon_social        LIKE safre_af:tab_afore_local.razon_social
    END RECORD

    DEFINE reg             ARRAY[7] OF RECORD
        cont_nss_uni      SMALLINT
    END RECORD  

    DEFINE lr_tot_fin_sb1 ,
           lr_tot_fin_sb3 ,
           lr_tot_fin_sb4 ,
           lr_tot_fin_sb5 ,
           lr_tot_fin_sb10 ,
           lr_tot_fin_sb11 ,
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
    
--    ORDER BY l_record.tipo_retiro     ,
--             l_record.nss             ,
--             lr_montos.siefore     
 
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

        PRINT '\033E\033(10U\033&l1O\033&k2S\033&18d',
            '\033e\033(s23H'

        SELECT codigo_afore           ,
               razon_social
        INTO   rpt_afore.codigo_afore , 
               rpt_afore.razon_social
        FROM safre_af:tab_afore_local

        LET hoy       = TODAY
        LET r_nom_rep = "RETL709"

        LET  f_opera   =  gr_captura.fecha_liquida 
        LET  f_trans   =  gr_captura.fecha_liquida


        IF rpt_afore.codigo_afore = 532 THEN
            LET encabezado = "SUBDIRECCION DE BENEFICIOS"
        ELSE 
            LET encabezado = "   MODULO DE RETIROS      "     
        END IF   

        PRINT COLUMN 067,encabezado,
            '\033015'
        SKIP 1 LINE

        PRINT COLUMN 054,"    REPORTE DE PROVISION DE DERECHO OTORGADO       ",
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

   
        PRINT COLUMN 1,"\332",L10,L1,
                       "\302",L10,L10,L10,L2,L1,
                       "\302",L5,L1,
                       "\302",L5,L2,
                       "\302",L5,L2,
                       "\302",L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,
                              L10,L10,L5,L5,L2,L2,L2,L2,
           --                 L10,L10,L10,L5,L5,L1,
           --          "\302",L5,
           --          "\302",L2,L2,
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
                       "\302",L10,L5,
                       "\302",L10,L2,L2,
                       "\302",L10,L2,L2,
                       "\302",L10,L5,
                       "\302",L10,L5,
                       "\302",L10,L2,L2,
             --        "\302",L10,L2,L2,
                       "\302",L5,
                       "\302",L2,L2,"|",
            '\033015'
	
        PRINT COLUMN 1,"|    NSS    |",
	               "       NOMBRE DEL TRABAJADOR     |",
		       "  DE  |",
		       "  DE   |",
		       "  DE   |",
           "     TIPO     |",
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
	--JGH   "              |",
                       "DIAG.|",
		       "REG.|",
            '\033015'

	 PRINT COLUMN 1,"|           |",
                        "                                 |",
			"      |",
			"       |",
			"       |",
			"  MOVIMIENTO  |",
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
	--JGH	        "              |",
                        "     |",
                        "    |",
            '\033015'

         PRINT COLUMN 1,"|           |",
			"                                 |",
			"SEGURO|",
			"PENSION|",
			"PRESTA.|",
			"              |",
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
	--JGH		"              |",
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
            --JGH       "\301",L10,L2,L2,
                        "\301",L5,
                        "\301",L2,L2,
                        "\331",
            '\033015'

         SKIP 1 LINE
     ---------------------------------
     ON EVERY ROW
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
                ELSE 
                   IF  lr_montos.siefore = 11   THEN 
                       LET  nombre_siefore = 'SB11'
                       LET  cont_tipo_retiro11 = cont_tipo_retiro11 + 1 
                   ELSE 
                       IF  lr_montos.siefore = 10   THEN
                           LET  nombre_siefore = 'SB10'
                           LET  cont_tipo_retiro10 = cont_tipo_retiro10 + 1
                       END IF
                   END IF
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
                COLUMN 071,lr_montos.tipo_movimiento ,   
                COLUMN 085,nombre_siefore            ,
                COLUMN 093,lr_montos.acciones_ret97 + 
                           lr_montos.acciones_cv    + 
                           lr_montos.acciones_cs    + 
                           lr_montos.acciones_ret92  USING "#######&.&&&&&&" ,
                COLUMN 109,lr_montos.acciones_ret97  USING "#######&.&&&&&&" ,
                COLUMN 125,lr_montos.acciones_cv     USING "#######&.&&&&&&" ,
                COLUMN 141,lr_montos.acciones_cs     USING "#######&.&&&&&&" , 
                COLUMN 156,lr_sol_tx.saldo_viv97_p   USING "#######&.&&"     , 
                COLUMN 171,lr_sol_tx.saldo_viv92_p   USING "#######&.&&"     ,
                COLUMN 187,lr_sol_tx.saldo_viv97     USING "#######&.&&&&&&" , 
                COLUMN 201,lr_sol_tx.saldo_viv92     USING "#######&.&&&&&&" ,
                COLUMN 219,lr_montos.acciones_ret92  USING "#######&.&&&&&&" ,
       --       COLUMN 234,lr_sol_tx.saldo_viv72     USING "########.##"     ,
       --       COLUMN 240,lr_sol_rx.diag_registro                           ,
       --       COLUMN 219,lr_sol_tx.regimen,                              
                COLUMN 225,lr_sol_rx.diag_registro                           ,
                COLUMN 232,lr_sol_tx.regimen,                              
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


FUNCTION f_Acumula(l_record,lr_montos, lr_sol_tx)

    DEFINE l_record RECORD
        tipo_retiro           SMALLINT                             ,
        nss                   LIKE dis_provision.nss               ,
        folio                 LIKE dis_provision.folio             ,
        consecutivo_lote      LIKE dis_provision.consecutivo_lote  ,
        siefore               LIKE dis_provision.siefore
    END RECORD

    DEFINE lr_montos   RECORD
    	  tipo_movimiento       SMALLINT                              ,    	 
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

    DEFINE ind         SMALLINT

    IF    l_record.tipo_retiro   =  800 THEN 
          LET   ind     =  1 
    END IF 
    IF    l_record.tipo_retiro   =  810 THEN 
          LET   ind     =  2 
    END IF 
    IF    l_record.tipo_retiro   =  815 THEN 
          LET   ind     =  3 
    END IF 

    IF    lr_montos.acciones_ret97    IS NULL   
     OR   lr_montos.acciones_ret97     =  0   THEN 
          LET    lr_montos.acciones_ret97  =  0
    END IF
    IF    lr_montos.acciones_cv       IS NULL   
     OR   lr_montos.acciones_cv        =  0   THEN 
          LET    lr_montos.acciones_cv     =  0
    END IF
    IF    lr_montos.acciones_cs       IS NULL   
     OR   lr_montos.acciones_cs        =  0   THEN 
          LET    lr_montos.acciones_cs     =  0
    END IF
    IF    lr_montos.acciones_ret92    IS NULL   
     OR   lr_montos.acciones_ret92     =  0   THEN 
          LET    lr_montos.acciones_ret92  =  0
    END IF
    IF    lr_montos.acciones_ret97    IS NULL   
     OR   lr_montos.acciones_ret97     =  0   THEN 
          LET    lr_montos.acciones_ret97  =  0
    END IF
    IF    lr_sol_tx.saldo_viv97       IS NULL   
     OR   lr_sol_tx.saldo_viv97        =  0   THEN 
          LET    lr_sol_tx.saldo_viv97     =  0
    END IF
    IF    lr_sol_tx.saldo_viv97_p     IS NULL   
     OR   lr_sol_tx.saldo_viv97_p      =  0   THEN 
          LET    lr_sol_tx.saldo_viv97_p   =  0
    END IF
    IF    lr_sol_tx.saldo_viv92       IS NULL   
     OR   lr_sol_tx.saldo_viv92        =  0   THEN 
          LET    lr_sol_tx.saldo_viv92     =  0
    END IF
    IF    lr_sol_tx.saldo_viv92_p     IS NULL   
     OR   lr_sol_tx.saldo_viv92_p      =  0   THEN 
          LET    lr_sol_tx.saldo_viv92_p   =  0
    END IF
    IF    lr_montos.acciones_ret92    IS NULL   
     OR   lr_montos.acciones_ret92     =  0   THEN 
          LET    lr_montos.acciones_ret92  =  0
    END IF
    IF    lr_sol_tx.saldo_viv72       IS NULL   
     OR   lr_sol_tx.saldo_viv72        =  0   THEN 
          LET    lr_sol_tx.saldo_viv72     =  0
    END IF
  
    IF    l_record.siefore   =  1   THEN 

          --OBTIENE TOTALES POR TIPO DE RETIRO SIEFORE 1

          LET reg_s1[ind].tipo_ret = l_record.tipo_retiro
          LET reg_s1[ind].tot =   reg_s1[ind].tot  +  1
          	    
          LET reg_s1[ind].total    = reg_s1[ind].total
                                   +           (lr_montos.acciones_ret97 + 
                                                lr_montos.acciones_cv    + 
                                                lr_montos.acciones_cs    + 
                                                lr_montos.acciones_ret92)
          LET reg_s1[ind].r97      = reg_s1[ind].r97 
                                   +          (lr_montos.acciones_ret97)  
          LET reg_s1[ind].cv       = reg_s1[ind].cv 
                                   +          (lr_montos.acciones_cv   )  
          LET reg_s1[ind].cs       = reg_s1[ind].cs 
                                   +          (lr_montos.acciones_cs   )  
          LET reg_s1[ind].viv97    = reg_s1[ind].viv97 
                                   +          (lr_sol_tx.saldo_viv97    ) 
          LET reg_s1[ind].viv97p   = reg_s1[ind].viv97p 
                                   +          (lr_sol_tx.saldo_viv97_p  ) 
          LET reg_s1[ind].viv92    = reg_s1[ind].viv92 
                                   +          (lr_sol_tx.saldo_viv92    ) 
          LET reg_s1[ind].viv92p   = reg_s1[ind].viv92p 
                                   +          (lr_sol_tx.saldo_viv92_p  ) 
          LET reg_s1[ind].r92      = reg_s1[ind].r92 
                                   +          (lr_montos.acciones_ret92)  
          LET reg_s1[ind].viv72    = reg_s1[ind].viv72 
                                   +          (lr_sol_tx.saldo_viv72    ) 
     END IF 

    IF    l_record.siefore   =  2   THEN 

          --OBTIENE TOTALES POR TIPO DE RETIRO SIEFORE 2

          LET reg_s2[ind].tipo_ret = l_record.tipo_retiro
          LET reg_s2[ind].tot =   reg_s2[ind].tot  +  1

          LET reg_s2[ind].total    = reg_s2[ind].total
                                   +           (lr_montos.acciones_ret97 +
                                                lr_montos.acciones_cv    +
                                                lr_montos.acciones_cs    +
                                                lr_montos.acciones_ret92)
          LET reg_s2[ind].r97      = reg_s2[ind].r97
                                   +          (lr_montos.acciones_ret97)
          LET reg_s2[ind].cv       = reg_s2[ind].cv
                                   +          (lr_montos.acciones_cv   )
          LET reg_s2[ind].cs       = reg_s2[ind].cs
                                   +          (lr_montos.acciones_cs   )
          LET reg_s2[ind].viv97    = reg_s2[ind].viv97
                                   +          (lr_sol_tx.saldo_viv97    )
          LET reg_s2[ind].viv97p   = reg_s2[ind].viv97p
                                   +          (lr_sol_tx.saldo_viv97_p  )
          LET reg_s2[ind].viv92    = reg_s2[ind].viv92
                                   +          (lr_sol_tx.saldo_viv92    )
          LET reg_s2[ind].viv92p   = reg_s2[ind].viv92p
                                   +          (lr_sol_tx.saldo_viv92_p  )
          LET reg_s2[ind].r92      = reg_s2[ind].r92
                                   +          (lr_montos.acciones_ret92)
          LET reg_s2[ind].viv72    = reg_s2[ind].viv72
                                   +          (lr_sol_tx.saldo_viv72    )
     END IF 

    IF    l_record.siefore   =  3   THEN 

          --OBTIENE TOTALES POR TIPO DE RETIRO SIEFORE 3

          LET reg_s3[ind].tipo_ret = l_record.tipo_retiro
          LET reg_s3[ind].tot =   reg_s3[ind].tot  +  1

          LET reg_s3[ind].total    = reg_s3[ind].total
                                   +           (lr_montos.acciones_ret97 +
                                                lr_montos.acciones_cv    +
                                                lr_montos.acciones_cs    +
                                                lr_montos.acciones_ret92)
          LET reg_s3[ind].r97      = reg_s3[ind].r97
                                   +          (lr_montos.acciones_ret97)
          LET reg_s3[ind].cv       = reg_s3[ind].cv
                                   +          (lr_montos.acciones_cv   )
          LET reg_s3[ind].cs       = reg_s3[ind].cs
                                   +          (lr_montos.acciones_cs   )
          LET reg_s3[ind].viv97    = reg_s3[ind].viv97
                                   +          (lr_sol_tx.saldo_viv97    )
          LET reg_s3[ind].viv97p   = reg_s3[ind].viv97p
                                   +          (lr_sol_tx.saldo_viv97_p  )
          LET reg_s3[ind].viv92    = reg_s3[ind].viv92
                                   +          (lr_sol_tx.saldo_viv92    )
          LET reg_s3[ind].viv92p   = reg_s3[ind].viv92p
                                   +          (lr_sol_tx.saldo_viv92_p  )
          LET reg_s3[ind].r92      = reg_s3[ind].r92
                                   +          (lr_montos.acciones_ret92)
          LET reg_s3[ind].viv72    = reg_s3[ind].viv72
                                   +          (lr_sol_tx.saldo_viv72    )
     END IF

    IF    l_record.siefore   =  4   THEN 

          --OBTIENE TOTALES POR TIPO DE RETIRO SIEFORE 4

          LET reg_s4[ind].tipo_ret = l_record.tipo_retiro
          LET reg_s4[ind].tot =   reg_s4[ind].tot  +  1

          LET reg_s4[ind].total    = reg_s4[ind].total
                                   +           (lr_montos.acciones_ret97 +
                                                lr_montos.acciones_cv    +
                                                lr_montos.acciones_cs    +
                                                lr_montos.acciones_ret92)
          LET reg_s4[ind].r97      = reg_s4[ind].r97
                                   +          (lr_montos.acciones_ret97)
          LET reg_s4[ind].cv       = reg_s4[ind].cv
                                   +          (lr_montos.acciones_cv   )
          LET reg_s4[ind].cs       = reg_s4[ind].cs
                                   +          (lr_montos.acciones_cs   )
          LET reg_s4[ind].viv97    = reg_s4[ind].viv97
                                   +          (lr_sol_tx.saldo_viv97    )
          LET reg_s4[ind].viv97p   = reg_s4[ind].viv97p
                                   +          (lr_sol_tx.saldo_viv97_p  )
          LET reg_s4[ind].viv92    = reg_s4[ind].viv92
                                   +          (lr_sol_tx.saldo_viv92    )
          LET reg_s4[ind].viv92p   = reg_s4[ind].viv92p
                                   +          (lr_sol_tx.saldo_viv92_p  )
          LET reg_s4[ind].r92      = reg_s4[ind].r92
                                   +          (lr_montos.acciones_ret92)
          LET reg_s4[ind].viv72    = reg_s4[ind].viv72
                                   +          (lr_sol_tx.saldo_viv72    )
     END IF

    IF    l_record.siefore   =  5   THEN 

          --OBTIENE TOTALES POR TIPO DE RETIRO SIEFORE 5

          LET reg_s5[ind].tipo_ret = l_record.tipo_retiro
          LET reg_s5[ind].tot =   reg_s5[ind].tot  +  1

          LET reg_s5[ind].total    = reg_s5[ind].total
                                   +           (lr_montos.acciones_ret97 +
                                                lr_montos.acciones_cv    +
                                                lr_montos.acciones_cs    +
                                                lr_montos.acciones_ret92)
          LET reg_s5[ind].r97      = reg_s5[ind].r97
                                   +          (lr_montos.acciones_ret97)
          LET reg_s5[ind].cv       = reg_s5[ind].cv
                                   +          (lr_montos.acciones_cv   )
          LET reg_s5[ind].cs       = reg_s5[ind].cs
                                   +          (lr_montos.acciones_cs   )
          LET reg_s5[ind].viv97    = reg_s5[ind].viv97
                                   +          (lr_sol_tx.saldo_viv97    )
          LET reg_s5[ind].viv97p   = reg_s5[ind].viv97p
                                   +          (lr_sol_tx.saldo_viv97_p  )
          LET reg_s5[ind].viv92    = reg_s5[ind].viv92
                                   +          (lr_sol_tx.saldo_viv92    )
          LET reg_s5[ind].viv92p   = reg_s5[ind].viv92p
                                   +          (lr_sol_tx.saldo_viv92_p  )
          LET reg_s5[ind].r92      = reg_s5[ind].r92
                                   +          (lr_montos.acciones_ret92)
          LET reg_s5[ind].viv72    = reg_s5[ind].viv72
                                   +          (lr_sol_tx.saldo_viv72    )
     END IF

    IF    l_record.siefore   =  10   THEN 

          --OBTIENE TOTALES POR TIPO DE RETIRO SIEFORE 10

          LET reg_s10[ind].tipo_ret = l_record.tipo_retiro
          LET reg_s10[ind].tot =   reg_s10[ind].tot  +  1

          LET reg_s10[ind].total    = reg_s10[ind].total
                                   +           (lr_montos.acciones_ret97 +
                                                lr_montos.acciones_cv    +
                                                lr_montos.acciones_cs    +
                                                lr_montos.acciones_ret92)
          LET reg_s10[ind].r97      = reg_s10[ind].r97
                                   +          (lr_montos.acciones_ret97)
          LET reg_s10[ind].cv       = reg_s10[ind].cv
                                   +          (lr_montos.acciones_cv   )
          LET reg_s10[ind].cs       = reg_s10[ind].cs
                                   +          (lr_montos.acciones_cs   )
          LET reg_s10[ind].viv97    = reg_s10[ind].viv97
                                   +          (lr_sol_tx.saldo_viv97    )
          LET reg_s10[ind].viv97p   = reg_s10[ind].viv97p
                                   +          (lr_sol_tx.saldo_viv97_p  )
          LET reg_s10[ind].viv92    = reg_s10[ind].viv92
                                   +          (lr_sol_tx.saldo_viv92    )
          LET reg_s10[ind].viv92p   = reg_s10[ind].viv92p
                                   +          (lr_sol_tx.saldo_viv92_p  )
          LET reg_s10[ind].r92      = reg_s10[ind].r92
                                   +          (lr_montos.acciones_ret92)
          LET reg_s10[ind].viv72    = reg_s10[ind].viv72
                                   +          (lr_sol_tx.saldo_viv72    )
     END IF


    IF    l_record.siefore   =  11   THEN 

          --OBTIENE TOTALES POR TIPO DE RETIRO SIEFORE 11

          LET reg_s11[ind].tipo_ret = l_record.tipo_retiro
          LET reg_s11[ind].tot =   reg_s11[ind].tot  +  1

          LET reg_s11[ind].total    = reg_s11[ind].total
                                   +           (lr_montos.acciones_ret97 +
                                                lr_montos.acciones_cv    +
                                                lr_montos.acciones_cs    +
                                                lr_montos.acciones_ret92)
          LET reg_s11[ind].r97      = reg_s11[ind].r97
                                   +          (lr_montos.acciones_ret97)
          LET reg_s11[ind].cv       = reg_s11[ind].cv
                                   +          (lr_montos.acciones_cv   )
          LET reg_s11[ind].cs       = reg_s11[ind].cs
                                   +          (lr_montos.acciones_cs   )
          LET reg_s11[ind].viv97    = reg_s11[ind].viv97
                                   +          (lr_sol_tx.saldo_viv97    )
          LET reg_s11[ind].viv97p   = reg_s11[ind].viv97p
                                   +          (lr_sol_tx.saldo_viv97_p  )
          LET reg_s11[ind].viv92    = reg_s11[ind].viv92
                                   +          (lr_sol_tx.saldo_viv92    )
          LET reg_s11[ind].viv92p   = reg_s11[ind].viv92p
                                   +          (lr_sol_tx.saldo_viv92_p  )
          LET reg_s11[ind].r92      = reg_s11[ind].r92
                                   +          (lr_montos.acciones_ret92)
          LET reg_s11[ind].viv72    = reg_s11[ind].viv72
                                   +          (lr_sol_tx.saldo_viv72    )
     END IF

END FUNCTION 


FUNCTION  f_ini()
  DEFINE   ind       SMALLINT 
   
  LET      ind = 1 
  FOR   ind = 1 TO 7 
        LET       reg_s1[ind].tipo_ret                  =    0
        LET       reg_s1[ind].tot                       =    0
        LET       reg_s1[ind].tipo_ret                  =    0
        LET       reg_s1[ind].total                     =    0
        LET       reg_s1[ind].r97                       =    0
        LET       reg_s1[ind].cv                        =    0
        LET       reg_s1[ind].cs                        =    0
        LET       reg_s1[ind].viv97                     =    0
        LET       reg_s1[ind].viv97p                    =    0
        LET       reg_s1[ind].viv92                     =    0
        LET       reg_s1[ind].viv92p                    =    0
        LET       reg_s1[ind].r92                       =    0
        LET       reg_s1[ind].viv72                     =    0

        LET       reg_s2[ind].tipo_ret                  =    0
        LET       reg_s2[ind].tot                       =    0
        LET       reg_s2[ind].tipo_ret                  =    0
        LET       reg_s2[ind].total                     =    0
        LET       reg_s2[ind].r97                       =    0
        LET       reg_s2[ind].cv                        =    0
        LET       reg_s2[ind].cs                        =    0
        LET       reg_s2[ind].viv97                     =    0
        LET       reg_s2[ind].viv97p                    =    0
        LET       reg_s2[ind].viv92                     =    0
        LET       reg_s2[ind].viv92p                    =    0
        LET       reg_s2[ind].r92                       =    0
        LET       reg_s2[ind].viv72                     =    0

        LET       reg_s3[ind].tipo_ret                  =    0
        LET       reg_s3[ind].tot                       =    0
        LET       reg_s3[ind].tipo_ret                  =    0
        LET       reg_s3[ind].total                     =    0
        LET       reg_s3[ind].r97                       =    0
        LET       reg_s3[ind].cv                        =    0
        LET       reg_s3[ind].cs                        =    0
        LET       reg_s3[ind].viv97                     =    0
        LET       reg_s3[ind].viv97p                    =    0
        LET       reg_s3[ind].viv92                     =    0
        LET       reg_s3[ind].viv92p                    =    0
        LET       reg_s3[ind].r92                       =    0
        LET       reg_s3[ind].viv72                     =    0

        LET       reg_s4[ind].tipo_ret                  =    0
        LET       reg_s4[ind].tot                       =    0
        LET       reg_s4[ind].tipo_ret                  =    0
        LET       reg_s4[ind].total                     =    0
        LET       reg_s4[ind].r97                       =    0
        LET       reg_s4[ind].cv                        =    0
        LET       reg_s4[ind].cs                        =    0
        LET       reg_s4[ind].viv97                     =    0
        LET       reg_s4[ind].viv97p                    =    0
        LET       reg_s4[ind].viv92                     =    0
        LET       reg_s4[ind].viv92p                    =    0
        LET       reg_s4[ind].r92                       =    0
        LET       reg_s4[ind].viv72                     =    0

        LET       reg_s5[ind].tipo_ret                  =    0
        LET       reg_s5[ind].tot                       =    0
        LET       reg_s5[ind].tipo_ret                  =    0
        LET       reg_s5[ind].total                     =    0
        LET       reg_s5[ind].r97                       =    0
        LET       reg_s5[ind].cv                        =    0
        LET       reg_s5[ind].cs                        =    0
        LET       reg_s5[ind].viv97                     =    0
        LET       reg_s5[ind].viv97p                    =    0
        LET       reg_s5[ind].viv92                     =    0
        LET       reg_s5[ind].viv92p                    =    0
        LET       reg_s5[ind].r92                       =    0
        LET       reg_s5[ind].viv72                     =    0

        LET       reg_s10[ind].tipo_ret                  =    0
        LET       reg_s10[ind].tot                       =    0
        LET       reg_s10[ind].tipo_ret                  =    0
        LET       reg_s10[ind].total                     =    0
        LET       reg_s10[ind].r97                       =    0
        LET       reg_s10[ind].cv                        =    0
        LET       reg_s10[ind].cs                        =    0
        LET       reg_s10[ind].viv97                     =    0
        LET       reg_s10[ind].viv97p                    =    0
        LET       reg_s10[ind].viv92                     =    0
        LET       reg_s10[ind].viv92p                    =    0
        LET       reg_s10[ind].r92                       =    0
        LET       reg_s10[ind].viv72                     =    0

        LET       reg_s11[ind].tipo_ret                  =    0
        LET       reg_s11[ind].tot                       =    0
        LET       reg_s11[ind].tipo_ret                  =    0
        LET       reg_s11[ind].total                     =    0
        LET       reg_s11[ind].r97                       =    0
        LET       reg_s11[ind].cv                        =    0
        LET       reg_s11[ind].cs                        =    0
        LET       reg_s11[ind].viv97                     =    0
        LET       reg_s11[ind].viv97p                    =    0
        LET       reg_s11[ind].viv92                     =    0
        LET       reg_s11[ind].viv92p                    =    0
        LET       reg_s11[ind].r92                       =    0
        LET       reg_s11[ind].viv72                     =    0

    END FOR
END FUNCTION 
