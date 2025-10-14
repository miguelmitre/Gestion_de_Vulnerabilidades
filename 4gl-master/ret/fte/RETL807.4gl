###########################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                       #
#Propietario       => E.F.P.                                              #
#Programa RETL807  => Reporte de Provision de transferencia  de Recursos  #
#Fecha             => 23 DE FEBRERO DEL 2004.                             #
#Por               => MAURO MUNIZ CABALLERO / CLAUDIA URZUA ROSAS         #
#Actualizo         => JUAN CARLOS MENDOZA MORENO                          #
#Fecha Actualiza   => 29 DE JUNIO DE 2004                                 #
#Actualizacion     => IJR 08/SEP/05 Cambio de valuacion de vivienda al    #
#                  =>     primero de mes                                  #
#Sistema           => RET                                                 #
###########################################################################
DATABASE safre_af
GLOBALS
    DEFINE w_codigo_afore LIKE tab_afore_local.codigo_afore 
    DEFINE g_param_dis    RECORD LIKE seg_modulo.*

    DEFINE 
        HOY                    ,
        d_primero_mes          ,        --IJR 050908
        fecha_liquida          DATE
        
    DEFINE enter,aux_pausa     CHAR(001)
    DEFINE g_usuario           CHAR(008)
    DEFINE hora                CHAR(008)
    DEFINE G_LISTA             CHAR(300)
    DEFINE G_IMPRE             CHAR(300)
    DEFINE impresion           CHAR(300)

    DEFINE v RECORD
        fecha_corte DATE
    END RECORD

    DEFINE 
        where_clause         CHAR(250),
        v_habil_siguiente    CHAR(080)

    DEFINE 
        precio_de_liq   DECIMAL(19,14)

    DEFINE vparametros RECORD
        vfolio INTEGER ,
        vfecha DATE
    END RECORD

    DEFINE 
        folio_rx             ,
        sw10                 ,
        num_dia_habil        SMALLINT
        
    DEFINE fec_rx            DATE
    DEFINE maximo_folio      INTEGER 
    DEFINE select_stmt5      CHAR(2000)
    DEFINE ctos_regs         INTEGER 
    DEFINE cadena10          CHAR(010)  --IJR 050809
END GLOBALS


MAIN
    DEFER INTERRUPT
    OPTIONS PROMPT LINE LAST,
        INPUT WRAP,
        ACCEPT KEY control-o

    CALL startlog("RETL807.log")
    CALL inicio()     #i
    CALL Consulta()   #c

    PROMPT " PROCESO FINALIZADO ...    <ENTER> PARA SALIR " FOR CHAR enter
END MAIN


FUNCTION inicio()
#i---------------
    SELECT codigo_afore,USER
    INTO   w_codigo_afore,g_usuario
    FROM   tab_afore_local

    SELECT ruta_listados
    INTO   g_param_dis.ruta_listados
    FROM   seg_modulo 
    WHERE  modulo_cod='ret'

    LET HOY           = TODAY
    LET num_dia_habil = 1

    LET v_habil_siguiente = " EXECUTE FUNCTION fn_habil_siguiente ( ?,? )"
    PREPARE eje_habil FROM v_habil_siguiente

    DECLARE cur_habil CURSOR FOR eje_habil
    OPEN cur_habil USING HOY,
                         num_dia_habil
        FETCH cur_habil INTO fecha_liquida
    CLOSE cur_habil

    --OBTIENE EL 1ER DIA NATURAL DEL MES ACTUAL(MES EN QUE SE PROVISIONO)
    --IJR 050908

    LET cadena10 = MONTH(TODAY) USING "&&","/01/",YEAR(TODAY) USING "&&&&"
    LET d_primero_mes = cadena10

    --------------------------------------------------------------------------
    SELECT "OK"
    FROM   glo_valor_accion
  --WHERE  fecha_valuacion = fecha_liquida   --IJR 050908
    WHERE  fecha_valuacion = d_primero_mes
    AND    codigo_siefore = 11

    IF STATUS = NOTFOUND THEN
       ERROR " NO EXISTE PRECIO DE PARTICIPACION DE LA FECHA DE LIQUIDACION "
     --ERROR " NO EXISTE PRECIO DE PARTICIPACION DEL PRIMERO DE MES "
       SLEEP 3
       EXIT PROGRAM
    ELSE
       SELECT precio_del_dia
       INTO   precio_de_liq
       FROM   glo_valor_accion
     --WHERE  fecha_valuacion = fecha_liquida  --IJR 050908
       WHERE  fecha_valuacion = d_primero_mes
       AND    codigo_siefore = 11
    END IF
END FUNCTION


#---------------------------------------------------------------------------
# FUNCTION QUE ABRE LA PANTALLA DE CAPTURA
#---------------------------------------------------------------------------
FUNCTION Consulta()
#c-----------------
    DEFINE query        CHAR(1) 
    DEFINE verifica     CHAR(70)
    DEFINE select_stmt  CHAR(2000)
    DEFINE cuenta_regis INTEGER
    DEFINE pos          INTEGER   
    DEFINE l_fecha      DATE   

    CLEAR SCREEN

    OPEN WINDOW ventana_21 AT  6,4 WITH FORM "RETL8071" ATTRIBUTE( BORDER)
    DISPLAY "RETL807                     (Ctrl-C) Salir             (ESC) Ejecutar         " AT 1,1 ATTRIBUTE(REVERSE,green)

    LET int_flag = FALSE

    CONSTRUCT BY NAME where_clause ON dis_provision.folio,
                                      dis_provision.fecha_conversion 

        ON KEY(ESC)
           LET folio_rx = get_fldbuf(ret_transf_rx.folio)
           LET fec_rx   = get_fldbuf(dis_provision.fecha_conversion)

           LET int_flag =FALSE  
           ERROR "PROCESANDO INFORMACION...." 
           EXIT  CONSTRUCT

        WHILE TRUE
	   IF aux_pausa MATCHES "[SsNn]" THEN
	      IF aux_pausa MATCHES "[Ss]" THEN
	         EXIT WHILE
	      ELSE
	         PROMPT " PROCESO CANCELADO... <ENTER> PARA SALIR " FOR CHAR enter
	         EXIT PROGRAM
	      END IF
	   END IF
	END WHILE

        ON KEY(CONTROL-C)
           LET int_flag = TRUE
           EXIT CONSTRUCT 

    END CONSTRUCT

    IF int_flag THEN
        LET int_flag = FALSE
        CLOSE WINDOW ventana_21
        EXIT PROGRAM 
    END IF

    LET query        = 0
    LET maximo_folio = 0

    CALL arma_consulta(where_clause,query CLIPPED)

    IF int_flag THEN
        LET int_flag = FALSE
    END IF
END FUNCTION


#---------------------------------------------------------------------------
# FUNCION QUE ARMA EL QUERY
#---------------------------------------------------------------------------
FUNCTION arma_consulta(where_clause,query_var)
#aq-------------------------------------------
    DEFINE l_record RECORD
        tipo_retiro               LIKE ret_transf_rx.tipo_retiro       ,
        nss                       LIKE dis_provision.nss               ,
        folio                     LIKE dis_provision.folio             ,
        consecutivo_lote          LIKE dis_provision.consecutivo_lote  ,
        siefore                   LIKE ret_monto_siefore.siefore
    END RECORD
    	
    DEFINE lr_montos RECORD
        acc_ret97                 LIKE ret_monto_siefore.acciones_ret97 ,
        acc_cv                    LIKE ret_monto_siefore.acciones_cv    ,
        acc_cs                    LIKE ret_monto_siefore.acciones_cs    ,
        siefore                   LIKE ret_monto_siefore.siefore   
    END RECORD
    	
    DEFINE lr_transf RECORD	
        saldo_viv97               DECIMAL(16,6)                      ,
        saldo_viv97p              DECIMAL(16,2)                      ,
        tipo_seguro               LIKE ret_transf_rx.tipo_seguro     ,
        tipo_pension              LIKE ret_transf_rx.tipo_pension    ,
        regimen                   LIKE ret_transf_rx.regimen         ,
        diag_registro             LIKE ret_transf_rx.diag_registro   ,
        tipo_prestacion           LIKE ret_transf_rx.tipo_prestacion , 
        fecha_ini_pen             LIKE ret_transf_rx.fecha_ini_pen
    END RECORD         	  	

    DEFINE where_clause           CHAR(250)
    DEFINE v_permisos             CHAR(110)     
    DEFINE select_stmt            CHAR(2000)
    DEFINE pos, cual_siefore      SMALLINT
    DEFINE query_var, indica      CHAR(1)
    DEFINE cont_datos             INTEGER
    DEFINE cont_siefore           SMALLINT  
    DEFINE hora                   CHAR(08)

    LET hora = TIME

    LET G_IMPRE = g_param_dis.ruta_listados CLIPPED,"/",HOY USING "YYYYMMDD",
		  ".807"

    START REPORT rpt_cuenta_imp TO G_IMPRE

    LET select_stmt5 = " SELECT CASE WHEN tipo_movimiento = 800 THEN 'A' ",
                       "             WHEN tipo_movimiento = 810 THEN 'B' ",
                       "             WHEN tipo_movimiento = 815 THEN 'C' END, ",
                       " nss, folio, consecutivo_lote, siefore ",
                       " FROM dis_provision ",
                       " WHERE ",where_clause CLIPPED,
                       " AND subcuenta IN (1,2,5,6,9 ) ",
                       " AND tipo_movimiento in (800,810,815) ",
                       " AND   siefore IN(1,2,3,4,5) ",
		       " UNION ",
                       " SELECT CASE WHEN tipo_movimiento = 800 THEN 'A' ",
                       "             WHEN tipo_movimiento = 810 THEN 'B' ",
                       "             WHEN tipo_movimiento = 815 THEN 'C' END, ",
                       " nss, folio, consecutivo_lote, siefore ", 
                       " FROM dis_provision ",
                       " WHERE ",where_clause CLIPPED,
                       " AND subcuenta = 4 ",
                       " AND tipo_movimiento in (800,810,815) ",
                       " AND   siefore = 11 ",
                       " GROUP BY 1,2,3,4,5 ",
                       " ORDER BY 1,2,5,3,4 " 

    LET select_stmt5 = select_stmt5 CLIPPED

    PREPARE cust_stmt1 FROM select_stmt5
    DECLARE rept_cur CURSOR FOR cust_stmt1
     
    LET ctos_regs  =  0
    FOREACH rept_cur INTO l_record.tipo_retiro     , 
                          l_record.nss             ,
                          l_record.folio           ,
                          l_record.consecutivo_lote,
                          l_record.siefore         
                             
        ERROR "  PROCESANDO INFORMACION...  "

        LET ctos_regs = ctos_regs + 1

        SELECT COUNT(UNIQUE siefore)
        INTO   cont_siefore
        FROM   dis_provision
        WHERE  nss              = l_record.nss
        AND    consecutivo_lote = l_record.consecutivo_lote
	AND    folio            = l_record.folio
        AND    siefore         IN (1,2,3,4,5,11)

	IF cont_siefore = 1 THEN
           SELECT UNIQUE siefore 
           INTO   cual_siefore
           FROM   dis_provision
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
                SELECT B.saldo_viv97                 ,
                       B.saldo_viv97 * precio_de_liq ,
                       C.tipo_seguro                 ,
                       C.tipo_pension                ,
                       C.regimen                     ,
                       B.diag_registro               ,
                       C.tipo_prestacion             ,
                       C.fecha_ini_pen
                INTO   lr_transf.saldo_viv97     ,
                       lr_transf.saldo_viv97p    ,
                       lr_transf.tipo_seguro     ,
                       lr_transf.tipo_pension    ,
                       lr_transf.regimen         ,
                       lr_transf.diag_registro   ,
                       lr_transf.tipo_prestacion ,
                       lr_transf.fecha_ini_pen    
                FROM   ret_transf_tx     B ,
                       ret_transf_rx     C 
                WHERE  B.nss             = l_record.nss               --condicion
                AND    B.consecutivo     = l_record.consecutivo_lote  --condicion
                AND    B.folio           = l_record.folio             --condicion
                AND    B.nss             = C.nss
                AND    B.consecutivo     = C.consecutivo
                AND    B.folio           = C.folio

                SELECT A.acciones_ret97,
                       A.acciones_cv   ,
                       A.acciones_cs   ,
                       A.siefore
                INTO   lr_montos.acc_ret97,
                       lr_montos.acc_cv   ,
                       lr_montos.acc_cs   ,
                       lr_montos.siefore
                FROM   ret_monto_siefore A
                WHERE  A.nss             = l_record.nss
                AND    A.consecutivo     = l_record.consecutivo_lote
                AND    A.siefore         = l_record.siefore
                AND    A.folio           = l_record.folio

                SELECT COUNT(UNIQUE siefore)
                INTO   cont_siefore
                FROM   ret_monto_siefore
                WHERE  nss         = l_record.nss
                AND    consecutivo = l_record.consecutivo_lote
                AND    siefore     IN(1,2,3,4,5)
        
                --  duda -----
                --IF cont_siefore = 2 AND lr_montos.siefore = 2 then
                IF cont_siefore > 1 AND sw10 > 1 then
                    LET lr_transf.saldo_viv97  = 0
                    LET lr_transf.saldo_viv97p = 0
                END IF     

                OUTPUT TO REPORT rpt_cuenta_imp(l_record.*  ,
                                                lr_montos.* ,
                                                lr_transf.* 
                                               )
            END IF
        ELSE
            SELECT B.saldo_viv97                 ,
                   B.saldo_viv97 * precio_de_liq ,
                   C.tipo_seguro                 ,
                   C.tipo_pension                ,
                   C.regimen                     ,
                   B.diag_registro               ,
                   C.tipo_prestacion             ,
                   C.fecha_ini_pen
            INTO   lr_transf.saldo_viv97     ,
                   lr_transf.saldo_viv97p    ,
                   lr_transf.tipo_seguro     ,
                   lr_transf.tipo_pension    ,
                   lr_transf.regimen         ,
                   lr_transf.diag_registro   ,
                   lr_transf.tipo_prestacion ,
                   lr_transf.fecha_ini_pen    
            FROM   ret_transf_tx     B ,
                   ret_transf_rx     C 
            WHERE  B.nss             = l_record.nss               --condicion
            AND    B.consecutivo     = l_record.consecutivo_lote  --condicion
            AND    B.folio           = l_record.folio             --condicion
            AND    B.nss             = C.nss
            AND    B.consecutivo     = C.consecutivo
            AND    B.folio           = C.folio

            LET lr_montos.acc_ret97 = 0
            LET lr_montos.acc_cv    = 0
            LET lr_montos.acc_cs    = 0
            LET lr_montos.siefore   = 1

            OUTPUT TO REPORT rpt_cuenta_imp(l_record.*  ,
                                            lr_montos.* ,
                                            lr_transf.* 
                                           )
        END IF
    END FOREACH    	

    IF ( ctos_regs <= 0 ) THEN
        ERROR "NO EXISTEN REGISTROS PARA ESTE CRITERIO..."
        SLEEP 2
        EXIT PROGRAM
    END IF 

    FINISH REPORT rpt_cuenta_imp
    
    LET v_permisos = " chmod 777 ",G_IMPRE
    RUN v_permisos
      
    PROMPT "  DESEA GENERAR IMPRESION (S/N)?  "
    FOR CHAR aux_pausa

    IF aux_pausa MATCHES "[Ss]" THEN
        LET impresion = "lp ",G_IMPRE
        RUN impresion
    END IF
END FUNCTION
    	

REPORT rpt_cuenta_imp(l_record , lr_montos , lr_transf )
#rpt------------------------------
                      
    DEFINE l_record RECORD
        tipo_retiro          LIKE ret_transf_rx.tipo_retiro       ,
        nss                  LIKE dis_provision.nss               ,
        folio                LIKE dis_provision.folio             ,
        consecutivo_lote     LIKE dis_provision.consecutivo_lote  ,
        siefore              LIKE ret_monto_siefore.siefore
    END RECORD
    	
    DEFINE lr_montos RECORD
        acc_ret97            LIKE ret_monto_siefore.acciones_ret97 ,
        acc_cv               LIKE ret_monto_siefore.acciones_cv    ,
        acc_cs               LIKE ret_monto_siefore.acciones_cs    ,
        siefore              LIKE ret_monto_siefore.siefore   
    END RECORD

    DEFINE lr_transf RECORD	
        saldo_viv97          DECIMAL(16,6)                      ,
        saldo_viv97p         DECIMAL(16,2)                      ,
        tipo_seguro          LIKE ret_transf_rx.tipo_seguro     ,
        tipo_pension         LIKE ret_transf_rx.tipo_pension    ,
        regimen              LIKE ret_transf_rx.regimen         ,
        diag_registro        LIKE ret_transf_tx.diag_registro   ,
        tipo_prestacion      LIKE ret_transf_rx.tipo_prestacion , 
        fecha_ini_pen        LIKE ret_transf_rx.fecha_ini_pen
    END RECORD         	  	

    DEFINE reg_s1, reg_s2, reg_s3, reg_s4, reg_s5  ARRAY[6] OF RECORD
         tot                 INTEGER,
         tipo_ret            CHAR(1),
         total               DECIMAL(16,6),
         r97                 DECIMAL(16,6),
         cv                  DECIMAL(16,6),
         cs                  DECIMAL(16,6),
         r92                 DECIMAL(16,6),
         viv97               DECIMAL(16,6),
         viv97p              DECIMAL(16,2)
    END RECORD

    DEFINE r_datnss  RECORD
           nombre_afore      LIKE ret_transf_rx.nombre_afore    ,
           paterno_afore     LIKE ret_transf_rx.paterno_afore   ,
           materno_afore     LIKE ret_transf_rx.materno_afore   
    END RECORD

    DEFINE hoy               ,
	   f_opera           ,
	   f_trans           DATE
    DEFINE r_mto_reten       LIKE dis_provision.monto_en_pesos
    DEFINE r_nom_rep         CHAR(7)
    DEFINE r_nombre          CHAR(60)

    DEFINE rpt_afore  RECORD
        codigo_afore      LIKE safre_af:tab_afore_local.codigo_afore,
        razon_social      LIKE safre_af:tab_afore_local.razon_social
    END RECORD

    DEFINE reg_18 RECORD #reg_18
	conta             SMALLINT      , 
        total             DECIMAL(16,6) ,
        r97               DECIMAL(16,6) ,
        cv                DECIMAL(16,6) ,
        cs                DECIMAL(16,6) ,
        viv97p            DECIMAL(16,2) ,
        viv97             DECIMAL(16,6)
    END RECORD

    DEFINE reg_19 RECORD #reg_19
	conta             SMALLINT      , 
        total             DECIMAL(16,6) ,
        r97               DECIMAL(16,6) ,
        cv                DECIMAL(16,6) ,
        cs                DECIMAL(16,6) ,
        viv97p            DECIMAL(16,2) ,
        viv97             DECIMAL(16,6)
    END RECORD

    DEFINE reg_20 RECORD #reg_20
	conta             SMALLINT      , 
        total             DECIMAL(16,6) ,
        r97               DECIMAL(16,6) ,
        cv                DECIMAL(16,6) ,
        cs                DECIMAL(16,6) ,
        viv97p            DECIMAL(16,2) ,
        viv97             DECIMAL(16,6)
    END RECORD

    DEFINE reg_21 RECORD #reg_21
	conta             SMALLINT      , 
        total             DECIMAL(16,6) ,
        r97               DECIMAL(16,6) ,
        cv                DECIMAL(16,6) ,
        cs                DECIMAL(16,6) ,
        viv97p            DECIMAL(16,2) ,
        viv97             DECIMAL(16,6)
    END RECORD

    DEFINE reg_22 RECORD #reg_22
	conta             SMALLINT      , 
        total             DECIMAL(16,6) ,
        r97               DECIMAL(16,6) ,
        cv                DECIMAL(16,6) ,
        cs                DECIMAL(16,6) ,
        viv97p            DECIMAL(16,2) ,
        viv97             DECIMAL(16,6)
    END RECORD

    DEFINE reg_cnt ARRAY[6] OF RECORD
        cont_nss_uni      SMALLINT
    END RECORD  

    DEFINE 
        nombre_final         CHAR(50)   ,
        nombre_siefore       CHAR(03)   ,  
        encabezado           CHAR(30)   ,
        L1                   CHAR(1)    ,
        L2                   CHAR(2)    ,
        L5                   CHAR(5)    ,
        L10                  CHAR(10)   ,
        cont                 INTEGER    , 
        cont_tip             INTEGER    ,
        ind, i               SMALLINT

    DEFINE #loc #smallint
        cont_nss_fin         ,
        cont_nss_unicos      ,
	cont_tipo_retiro1    ,
	cont_tipo_retiro3    ,
	cont_tipo_retiro4    ,
	cont_tipo_retiro5    ,
	cont_tipo_retiro2    SMALLINT

 OUTPUT
      TOP MARGIN    0
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      --RIGHT MARGIN  200
      RIGHT MARGIN  0
      --PAGE LENGTH   90
      PAGE LENGTH   45
      ORDER BY l_record.tipo_retiro ,
               l_record.nss         ,
               lr_montos.siefore
 
 FORMAT
      FIRST PAGE HEADER

      LET reg_18.conta  = 0
      LET reg_18.total  = 0
      LET reg_18.r97    = 0
      LET reg_18.cv     = 0
      LET reg_18.cs     = 0
      LET reg_18.viv97p = 0
      LET reg_18.viv97  = 0

      LET reg_19.conta  = 0
      LET reg_19.total  = 0
      LET reg_19.r97    = 0
      LET reg_19.cv     = 0
      LET reg_19.cs     = 0
      LET reg_19.viv97p = 0
      LET reg_19.viv97  = 0

      LET reg_20.conta  = 0
      LET reg_20.total  = 0
      LET reg_20.r97    = 0
      LET reg_20.cv     = 0
      LET reg_20.cs     = 0
      LET reg_20.viv97p = 0
      LET reg_20.viv97  = 0

      LET reg_21.conta  = 0
      LET reg_21.total  = 0
      LET reg_21.r97    = 0
      LET reg_21.cv     = 0
      LET reg_21.cs     = 0
      LET reg_21.viv97p = 0
      LET reg_21.viv97  = 0

      LET reg_22.conta  = 0
      LET reg_22.total  = 0
      LET reg_22.r97    = 0
      LET reg_22.cv     = 0
      LET reg_22.cs     = 0
      LET reg_22.viv97p = 0
      LET reg_22.viv97  = 0

      LET  ind            = 0

      LET cont_nss_unicos = 0
      LET cont_nss_fin    = 0


      LET  L1  = "\304"
      LET  L2  = "\304\304"
      LET  L5  = "\304\304\304\304\304"
      LET  L10 = "\304\304\304\304\304\304\304\304\304\304"

      PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'
      PRINT COLUMN 1,'\033e\033(s218T\033(s15H\033(s7B'

      --PRINT '\033E\033(10U\033&l1O\033&k2S\033&18d',
       --   '\033e\033(s23H'

      SELECT codigo_afore, razon_social
        INTO rpt_afore.codigo_afore, rpt_afore.razon_social
        FROM safre_af:tab_afore_local

      LET hoy = TODAY
      LET r_nom_rep = "RETL807"
      
      SELECT fecha_operacion, fecha_valor_trans
      INTO   f_opera, f_trans
      FROM   ret_cza_lote
      WHERE  folio = l_record.folio 

      IF rpt_afore.codigo_afore = 532 THEN
          LET encabezado = "SUBDIRECCION DE BENEFICIOS"
      ELSE 
      	  LET encabezado = "   MODULO DE RETIROS      "     
      END IF 	  

      PRINT COLUMN 067,encabezado
        --    '\033015'
      SKIP 1 LINE
      PRINT COLUMN 054,"REPORTE DE PROVISION DE TRANSFERENCIA DE RECURSOS"
           -- '\033015'
      SKIP 3 LINE
      PRINT COLUMN 002,"AFORE               : ",rpt_afore.codigo_afore USING "###","  ",rpt_afore.razon_social CLIPPED 
           -- '\033015'
      PRINT
      PRINT COLUMN 002,"FOLIO INTERNO       : ",l_record.folio USING "<<<<<<",
	    COLUMN 126,"PROGRAMA          : ",r_nom_rep
          --  '\033015'
      PRINT
      PRINT COLUMN 002,"FECHA DE OPERACION  : ",f_opera USING "DD-MM-YYYY",
            COLUMN 126,"PAGINA            : ",pageno USING "####"
           -- '\033015'
      PRINT
      PRINT COLUMN 002,"FECHA DE LIQUIDACION: ",f_trans USING "DD-MM-YYYY",
            COLUMN 126,"FECHA DEL REPORTE : ",hoy USING "DD-MM-YYYY"
            --'\033015'

      PRINT COLUMN 1,'\033e\033(s218T\033(s22H\033(s7B'

      PRINT COLUMN 1,"\332",L10,L1,
                     "\302",L10,L10,L10,L5,
                     "\302",L5,L1,
                     "\302",L5,L2,
                     "\302",L5,L2,
                     "\302",L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L1,
                     "\302",L10,L5,
                     "\302",L10,L5,
                     "\302",L10,
                     "\277"
           -- '\033015'

      PRINT COLUMN 1,"|           |                                   | TIPO | TIPO  | TIPO  |                 MONTOS TRANSFERIDOS DE LA CUENTA INDIVIDUAL ( EN ACCIONES )                         |               |               |          |"
           -- '\033015'
      PRINT COLUMN 1,"|           |                                   |      |       |       \303",L5,"\302",L10,L5,"\302",L10,L5,"\302",L10,L5,"\302",L10,L5,"\302",L10,L5,"\302",L10,L5,"\303",L10,L5,"\303",L10,L5,"\303",L10,"|"
          --  '\033015'
      PRINT COLUMN 1,"|    NSS    |       NOMBRE DEL TRABAJADOR       |  DE  |  DE   |  DE   |     |     TOTAL     |      R97      |      CV       |      CS       |     V I V     |    V I V      | FECHA  INICIO |  DIAGNOSTICO  |  REGIMEN |"
           -- '\033015'
      PRINT COLUMN 1,"|           |                                   |      |       |       |     |               |               |               |               |               |               |               |               |          |"
          --  '\033015'
      PRINT COLUMN 1,"|           |                                   |SEGURO|PENSION|PRESTA.|SIEF.|   ACCIONES    |   ACCIONES    |   ACCIONES    |   ACCIONES    |    PESOS      |PARTICIPACIONES|  DE  PENSION  |               |          |"
           -- '\033015'

      PRINT COLUMN 1,"\300",L10,L1,
                     "\301",L10,L10,L10,L5,
                     "\301",L5,L1,
                     "\301",L5,L2,
                     "\301",L5,L2,
                     "\301",L5,
                     "\301",L10,L5,
                     "\301",L10,L5,
                     "\301",L10,L5,
                     "\301",L10,L5,
                     "\301",L10,L5,
                     "\301",L10,L5,
                     "\301",L10,L5,
                     "\301",L10,L5,
                     "\301",L10,
                     "\331"
           -- '\033015'

      SKIP 2 LINE

      PAGE HEADER

      PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'
      PRINT COLUMN 1,'\033e\033(s218T\033(s15H\033(s7B'

      --  PRINT '\033E\033(10U\033&l1O\033&k2S\033&18d',
       --     '\033e\033(s23H'

      SELECT codigo_afore, razon_social
      INTO rpt_afore.codigo_afore, rpt_afore.razon_social
      FROM safre_af:tab_afore_local

      LET hoy = TODAY
      LET r_nom_rep = "RETL807"
      
      SELECT fecha_operacion, fecha_valor_trans
      INTO   f_opera, f_trans
      FROM   ret_cza_lote
      WHERE  folio = l_record.folio 

      PRINT COLUMN 067,encabezado
        --    '\033015'
      SKIP 1 LINE
      PRINT COLUMN 054,"REPORTE DE PROVISION DE TRANSFERENCIA DE RECURSOS"
           -- '\033015'
      SKIP 3 LINE
      PRINT COLUMN 002,"AFORE               : ",rpt_afore.codigo_afore USING "###","  ",rpt_afore.razon_social CLIPPED 
          --  '\033015'
      PRINT
      PRINT COLUMN 002,"FOLIO INTERNO       : ",l_record.folio USING "<<<<<<",
	    COLUMN 126,"PROGRAMA          : ",r_nom_rep
           -- '\033015'
      PRINT
      PRINT COLUMN 002,"FECHA DE OPERACION  : ",f_opera USING "DD-MM-YYYY",
            COLUMN 126,"PAGINA            : ",pageno USING "####"
            --'\033015'
      PRINT
      PRINT COLUMN 002,"FECHA DE LIQUIDACION: ",f_trans USING "DD-MM-YYYY",
            COLUMN 126,"FECHA DEL REPORTE : ",hoy USING "DD-MM-YYYY"
            --'\033015'

      PRINT COLUMN 1,'\033e\033(s218T\033(s22H\033(s7B' 

      PRINT COLUMN 1,"\332",L10,L1,
                     "\302",L10,L10,L10,L5,
                     "\302",L5,L1,
                     "\302",L5,L2,
                     "\302",L5,L2,
                     "\302",L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L1,
		     "\302",L10,L5,
		     "\302",L10,L5,
                     "\302",L10,
                     "\277"
            --'\033015'

      PRINT COLUMN 1,"|           |                                   | TIPO | TIPO  | TIPO  |                 MONTOS TRANSFERIDOS DE LA CUENTA INDIVIDUAL ( EN ACCIONES )                         |               |               |          |"
          --  '\033015'
      PRINT COLUMN 1,"|           |                                   |      |       |       \303",L5,"\302",L10,L5,"\302",L10,L5,"\302",L10,L5,"\302",L10,L5,"\302",L10,L5,"\302",L10,L5,"\303",L10,L5,"\303",L10,l5,"\303",L10,"|"
          --  '\033015'
      PRINT COLUMN 1,"|    NSS    |       NOMBRE DEL TRABAJADOR       |  DE  |  DE   |  DE   |     |     TOTAL     |      R97      |      CV       |      CS       |     V I V     |     V I V     | FECHA  INICIO |  DIAGNOSTICO  |  REGIMEN |"
           -- '\033015'
      PRINT COLUMN 1,"|           |                                   |      |       |       |     |               |               |               |               |               |               |               |               |          |"
            --'\033015'
      PRINT COLUMN 1,"|           |                                   |SEGURO|PENSION|PRESTA.|SIEF.|   ACCIONES    |   ACCIONES    |   ACCIONES    |   ACCIONES    |    PESOS      |PARTICIPACIONES|  DE  PENSION  |               |          |"
            --'\033015'

      PRINT COLUMN 1,"\300",L10,L1,
                     "\301",L10,L10,L10,L5,
                     "\301",L5,L1,
                     "\301",L5,L2,
                     "\301",L5,L2,
                     "\301",L5,
                     "\301",L10,L5,
                     "\301",L10,L5,
                     "\301",L10,L5,
                     "\301",L10,L5,
                     "\301",L10,L5,
                     "\301",L10,L5,
                     "\301",L10,L5,
                     "\301",L10,L5,
                     "\301",L10,
                     "\331"
          --  '\033015'
      SKIP 2 LINE

    ON EVERY ROW
      INITIALIZE enter TO NULL

    AFTER GROUP OF lr_montos.siefore
    
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

        PRINT 
            COLUMN 002,l_record.nss                                    ,
            COLUMN 014,r_nombre[1,35]                                  ,
            COLUMN 052,lr_transf.tipo_seguro                           ,
            COLUMN 059,lr_transf.tipo_pension                          ,
            COLUMN 066,lr_transf.tipo_prestacion USING "&&"            ,
            COLUMN 074,nombre_siefore                                  ,
            COLUMN 079,lr_montos.acc_ret97 +
                       lr_montos.acc_cv    +
                       lr_montos.acc_cs        USING "#######&.&&&&&&" ,-- 1
            COLUMN 095,lr_montos.acc_ret97     USING "#######&.&&&&&&" ,-- 1
            COLUMN 111,lr_montos.acc_cv        USING "#######&.&&&&&&" ,-- 2,6,9
            COLUMN 127,lr_montos.acc_cs        USING "#######&.&&&&&&" ,-- 5
            COLUMN 143,lr_transf.saldo_viv97p  USING "#########&.&&"   ,--4
            COLUMN 159,lr_transf.saldo_viv97   USING "#######&.&&&&&&" ,--4p
	    COLUMN 177,lr_transf.fecha_ini_pen USING "DD-MM-YYYY"      ,
            COLUMN 194,lr_transf.diag_registro                         ,
            COLUMN 211,lr_transf.regimen
           -- '\033015'
        PRINT

     -----------------------------------
     BEFORE GROUP OF l_record.nss
     -----------------------------
         LET cont_nss_unicos = cont_nss_unicos + 1
         LET sw10 = 0

    -----------------------------------
    AFTER GROUP OF l_record.tipo_retiro
    -----------------------------------
        LET ind = ind + 1    -- Ocurrencia del arreglo
        
        SKIP 1 LINE
        
        --OBTIENE TOTALES POR TIPO DE RETIRO SIEFORE 1
        LET reg_s1[ind].tot = cont_tipo_retiro1 
        LET reg_s1[ind].tipo_ret = l_record.tipo_retiro
        LET reg_s1[ind].total    = GROUP SUM (lr_montos.acc_ret97 + 
                                              lr_montos.acc_cv    + 
                                              lr_montos.acc_cs  
                                              )
                                   WHERE l_record.siefore  = 1
        IF reg_s1[ind].total IS NULL OR reg_s1[ind].total = ' ' THEN
            LET reg_s1[ind].total = 0                              
        END IF    
                                   
        LET reg_s1[ind].r97      = GROUP SUM(lr_montos.acc_ret97)  
                                   WHERE lr_montos.siefore = 1
        IF reg_s1[ind].r97 IS NULL OR reg_s1[ind].r97 = ' ' THEN
            LET reg_s1[ind].r97 = 0                              
        END IF    
                                   
        LET reg_s1[ind].cv       = GROUP SUM(lr_montos.acc_cv   )  
                                   WHERE lr_montos.siefore = 1
        IF reg_s1[ind].cv IS NULL OR reg_s1[ind].cv = ' ' THEN
            LET reg_s1[ind].cv = 0                              
        END IF    
                                   
        LET reg_s1[ind].cs       = GROUP SUM(lr_montos.acc_cs   )  
                                   WHERE lr_montos.siefore = 1
        IF reg_s1[ind].cs IS NULL OR reg_s1[ind].cs = ' ' THEN
            LET reg_s1[ind].cs = 0                              
        END IF    
                                   
        LET reg_s1[ind].viv97    = GROUP SUM(lr_transf.saldo_viv97    ) 
                                   WHERE lr_montos.siefore = 1
        IF reg_s1[ind].viv97 IS NULL OR reg_s1[ind].viv97 = ' ' THEN
            LET reg_s1[ind].viv97 = 0                              
        END IF    
                                   
        LET reg_s1[ind].viv97p   = GROUP SUM(lr_transf.saldo_viv97p  ) 
                                   WHERE lr_montos.siefore = 1
        IF reg_s1[ind].viv97p IS NULL OR reg_s1[ind].viv97p = ' ' THEN
            LET reg_s1[ind].viv97p = 0                              
        END IF    
        
        --OBTIENE TOTALES POR TIPO DE RETIRO SIEFORE 2
         
        LET reg_s2[ind].tot = cont_tipo_retiro2 
        LET reg_s2[ind].tipo_ret = l_record.tipo_retiro

        LET reg_s2[ind].total    = GROUP SUM (lr_montos.acc_ret97 + 
                                              lr_montos.acc_cv  + 
                                              lr_montos.acc_cs  )
                                   WHERE l_record.siefore  = 2
        IF reg_s2[ind].total IS NULL OR reg_s2[ind].total = ' ' THEN
            LET reg_s2[ind].total = 0
        END IF                               

        LET reg_s2[ind].r97      = GROUP SUM(lr_montos.acc_ret97)  
                                   WHERE lr_montos.siefore = 2
        IF reg_s2[ind].r97 IS NULL OR reg_s2[ind].r97 = ' ' THEN
            LET reg_s2[ind].r97 = 0
        END IF                               

        LET reg_s2[ind].cv       = GROUP SUM(lr_montos.acc_cv   )  
                                   WHERE lr_montos.siefore = 2
        IF reg_s2[ind].cv IS NULL OR reg_s2[ind].cv = ' ' THEN
            LET reg_s2[ind].cv = 0
        END IF                               

        LET reg_s2[ind].cs       = GROUP SUM(lr_montos.acc_cs   )  
                                   WHERE lr_montos.siefore = 2
        IF reg_s2[ind].cs IS NULL OR reg_s2[ind].cs = ' ' THEN
            LET reg_s2[ind].cs = 0
        END IF                               

        LET reg_s2[ind].viv97    = GROUP SUM(lr_transf.saldo_viv97    ) 
                                   WHERE lr_montos.siefore = 2
        IF reg_s2[ind].viv97 IS NULL OR reg_s2[ind].viv97 = ' ' THEN
            LET reg_s2[ind].viv97 = 0
        END IF                               

        LET reg_s2[ind].viv97p   = GROUP SUM(lr_transf.saldo_viv97p  ) 
                                   WHERE lr_montos.siefore = 2
        IF reg_s2[ind].viv97p IS NULL OR reg_s2[ind].viv97p = ' ' THEN
            LET reg_s2[ind].viv97p = 0
        END IF                               
        
        --OBTIENE TOTALES POR TIPO DE RETIRO SIEFORE 3
         
        LET reg_s3[ind].tot = cont_tipo_retiro3 
        LET reg_s3[ind].tipo_ret = l_record.tipo_retiro

        LET reg_s3[ind].total    = GROUP SUM (lr_montos.acc_ret97 + 
                                              lr_montos.acc_cv  + 
                                              lr_montos.acc_cs  )
                                   WHERE l_record.siefore  = 3
        IF reg_s3[ind].total IS NULL OR reg_s3[ind].total = ' ' THEN
            LET reg_s3[ind].total = 0
        END IF                               

        LET reg_s3[ind].r97      = GROUP SUM(lr_montos.acc_ret97)  
                                   WHERE lr_montos.siefore = 3
        IF reg_s3[ind].r97 IS NULL OR reg_s3[ind].r97 = ' ' THEN
            LET reg_s3[ind].r97 = 0
        END IF                               

        LET reg_s3[ind].cv       = GROUP SUM(lr_montos.acc_cv   )  
                                   WHERE lr_montos.siefore = 3
        IF reg_s3[ind].cv IS NULL OR reg_s3[ind].cv = ' ' THEN
            LET reg_s3[ind].cv = 0
        END IF                               

        LET reg_s3[ind].cs       = GROUP SUM(lr_montos.acc_cs   )  
                                   WHERE lr_montos.siefore = 3
        IF reg_s3[ind].cs IS NULL OR reg_s3[ind].cs = ' ' THEN
            LET reg_s3[ind].cs = 0
        END IF                               

        LET reg_s3[ind].viv97    = GROUP SUM(lr_transf.saldo_viv97    ) 
                                   WHERE lr_montos.siefore = 3
        IF reg_s3[ind].viv97 IS NULL OR reg_s3[ind].viv97 = ' ' THEN
            LET reg_s3[ind].viv97 = 0
        END IF                               

        LET reg_s3[ind].viv97p   = GROUP SUM(lr_transf.saldo_viv97p  ) 
                                   WHERE lr_montos.siefore = 3
        IF reg_s3[ind].viv97p IS NULL OR reg_s3[ind].viv97p = ' ' THEN
            LET reg_s3[ind].viv97p = 0
        END IF                               

        --OBTIENE TOTALES POR TIPO DE RETIRO SIEFORE 4
         
        LET reg_s4[ind].tot = cont_tipo_retiro4 
        LET reg_s4[ind].tipo_ret = l_record.tipo_retiro

        LET reg_s4[ind].total    = GROUP SUM (lr_montos.acc_ret97 + 
                                              lr_montos.acc_cv  + 
                                              lr_montos.acc_cs  )
                                   WHERE l_record.siefore  = 4
        IF reg_s4[ind].total IS NULL OR reg_s4[ind].total = ' ' THEN
            LET reg_s4[ind].total = 0
        END IF                               

        LET reg_s4[ind].r97      = GROUP SUM(lr_montos.acc_ret97)  
                                   WHERE lr_montos.siefore = 4
        IF reg_s4[ind].r97 IS NULL OR reg_s4[ind].r97 = ' ' THEN
            LET reg_s4[ind].r97 = 0
        END IF                               

        LET reg_s4[ind].cv       = GROUP SUM(lr_montos.acc_cv   )  
                                   WHERE lr_montos.siefore = 4
        IF reg_s4[ind].cv IS NULL OR reg_s4[ind].cv = ' ' THEN
            LET reg_s4[ind].cv = 0
        END IF                               

        LET reg_s4[ind].cs       = GROUP SUM(lr_montos.acc_cs   )  
                                   WHERE lr_montos.siefore = 4
        IF reg_s4[ind].cs IS NULL OR reg_s4[ind].cs = ' ' THEN
            LET reg_s4[ind].cs = 0
        END IF                               

        LET reg_s4[ind].viv97    = GROUP SUM(lr_transf.saldo_viv97    ) 
                                   WHERE lr_montos.siefore = 4
        IF reg_s4[ind].viv97 IS NULL OR reg_s4[ind].viv97 = ' ' THEN
            LET reg_s4[ind].viv97 = 0
        END IF                               

        LET reg_s4[ind].viv97p   = GROUP SUM(lr_transf.saldo_viv97p  ) 
                                   WHERE lr_montos.siefore = 4
        IF reg_s4[ind].viv97p IS NULL OR reg_s4[ind].viv97p = ' ' THEN
            LET reg_s4[ind].viv97p = 0
        END IF                               

        --OBTIENE TOTALES POR TIPO DE RETIRO SIEFORE 5
         
        LET reg_s5[ind].tot = cont_tipo_retiro5 
        LET reg_s5[ind].tipo_ret = l_record.tipo_retiro

        LET reg_s5[ind].total    = GROUP SUM (lr_montos.acc_ret97 + 
                                              lr_montos.acc_cv  + 
                                              lr_montos.acc_cs  )
                                   WHERE l_record.siefore  = 5
        IF reg_s5[ind].total IS NULL OR reg_s5[ind].total = ' ' THEN
            LET reg_s5[ind].total = 0
        END IF                               

        LET reg_s5[ind].r97      = GROUP SUM(lr_montos.acc_ret97)  
                                   WHERE lr_montos.siefore = 5
        IF reg_s5[ind].r97 IS NULL OR reg_s5[ind].r97 = ' ' THEN
            LET reg_s5[ind].r97 = 0
        END IF                               

        LET reg_s5[ind].cv       = GROUP SUM(lr_montos.acc_cv   )  
                                   WHERE lr_montos.siefore = 5
        IF reg_s5[ind].cv IS NULL OR reg_s5[ind].cv = ' ' THEN
            LET reg_s5[ind].cv = 0
        END IF                               

        LET reg_s5[ind].cs       = GROUP SUM(lr_montos.acc_cs   )  
                                   WHERE lr_montos.siefore = 5
        IF reg_s5[ind].cs IS NULL OR reg_s5[ind].cs = ' ' THEN
            LET reg_s5[ind].cs = 0
        END IF                               

        LET reg_s5[ind].viv97    = GROUP SUM(lr_transf.saldo_viv97    ) 
                                   WHERE lr_montos.siefore = 5
        IF reg_s5[ind].viv97 IS NULL OR reg_s5[ind].viv97 = ' ' THEN
            LET reg_s5[ind].viv97 = 0
        END IF                               

        LET reg_s5[ind].viv97p   = GROUP SUM(lr_transf.saldo_viv97p  ) 
                                   WHERE lr_montos.siefore = 5
        IF reg_s5[ind].viv97p IS NULL OR reg_s5[ind].viv97p = ' ' THEN
            LET reg_s5[ind].viv97p = 0
        END IF                               

        -- ACUMULA EN ARREGLO CONTADOR DE NSS UNICOS POR T DE RETIRO
        
        LET reg_cnt[ind].cont_nss_uni = cont_nss_unicos 
        
        -------------------------------------------------------------
        -- IMPRESION DE TOTAL POR TIPO DE RETIRO PARA SIEFORE 1
        -------------------------------------------------------------
        IF reg_s1[ind].tot > 0 THEN
            PRINT COLUMN 1,"\332",L10,L1,
                           "\302",L10,L10,L10,L10,L10,L5,L2,L1,
                           "\302",L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                                  L10,L5,L10,L1,L1,
                           "\277" 
           -- '\033015'
        
            PRINT
                COLUMN 001,"|",reg_s1[ind].tot  USING "#####",
                COLUMN 013,"|",
                COLUMN 015,"TOTALES POR TIPO RETIRO  ",reg_s1[ind].tipo_ret,
                COLUMN 072,"|",
                COLUMN 074,"SB1",
                COLUMN 078,"|",
                COLUMN 079,reg_s1[ind].total            USING "#######&.&&&&&&" ,
                COLUMN 094,"|",
                COLUMN 095,reg_s1[ind].r97              USING "#######&.&&&&&&" ,-- 1
                COLUMN 110,"|",
                COLUMN 111,reg_s1[ind].cv                 USING "#######&.&&&&&&" ,-- 2,6,9
                COLUMN 126,"|",
                COLUMN 127,reg_s1[ind].cs                USING "#######&.&&&&&&" ,-- 5
                COLUMN 142,"|",
                COLUMN 143,reg_s1[ind].viv97p             USING "#########&.&&" ,--4
                COLUMN 158,"|",
                COLUMN 159,reg_s1[ind].viv97            USING "#######&.&&&&&&" ,--4
                COLUMN 174,"|",
                COLUMN 217,"|"
            --'\033015'
            
            PRINT COLUMN 1,"\300",L10,L1,
                   "\301",L10,L10,L10,L10,L10,L5,L2,L1,
                   "\301",L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                          L10,L5,L10,L1,L1,
                   "\331"
            --'\033015'
        
        END IF

        -------------------------------------------------------------
        -- IMPRESION DE TOTAL POR TIPO DE RETIRO PARA SIEFORE 2
        -------------------------------------------------------------
        IF reg_s2[ind].tot > 0 THEN
            PRINT COLUMN 1,"\332",L10,L1,
                           "\302",L10,L10,L10,L10,L10,L5,L2,L1,
                           "\302",L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                                  L10,L5,L10,L1,L1,
                           "\277"
            --'\033015'
        
            PRINT
                COLUMN 001,"|",reg_s2[ind].tot  USING "#####",
                COLUMN 013,"|",
                COLUMN 015,"TOTALES POR TIPO RETIRO  ",reg_s2[ind].tipo_ret,
                COLUMN 072,"|",
                COLUMN 074,"SB2",
                COLUMN 078,"|",
                COLUMN 079,reg_s2[ind].total   USING "#######&.&&&&&&" ,
                COLUMN 094,"|",
                COLUMN 095,reg_s2[ind].r97     USING "#######&.&&&&&&" ,-- 1
                COLUMN 110,"|",
                COLUMN 111,reg_s2[ind].cv      USING "#######&.&&&&&&" ,-- 2,6,9
                COLUMN 126,"|",
                COLUMN 127,reg_s2[ind].cs      USING "#######&.&&&&&&" ,-- 5
                COLUMN 142,"|",
                COLUMN 143,reg_s2[ind].viv97p  USING "#########&.&&" ,--4
                COLUMN 158,"|",
                COLUMN 159,reg_s2[ind].viv97   USING "#######&.&&&&&&" ,--4
                COLUMN 174,"|",
                COLUMN 217,"|"
            --'\033015'
            
            PRINT COLUMN 1,"\300",L10,L1,
                   "\301",L10,L10,L10,L10,L10,L5,L2,L1,
                   "\301",L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                          L10,L5,L10,L1,L1,
                   "\331"
            --'\033015'
        
        END IF

        -------------------------------------------------------------
        -- IMPRESION DE TOTAL POR TIPO DE RETIRO PARA SIEFORE 3
        -------------------------------------------------------------
        IF reg_s3[ind].tot > 0 THEN
            PRINT COLUMN 1,"\332",L10,L1,
                           "\302",L10,L10,L10,L10,L10,L5,L2,L1,
                           "\302",L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                                  L10,L5,L10,L1,L1,
                           "\277"
           -- '\033015'
        
            PRINT
                COLUMN 001,"|",reg_s3[ind].tot  USING "#####",
                COLUMN 013,"|",
                COLUMN 015,"TOTALES POR TIPO RETIRO  ",reg_s3[ind].tipo_ret,
                COLUMN 072,"|",
                COLUMN 074,"SB3",
                COLUMN 078,"|",
                COLUMN 079,reg_s3[ind].total   USING "#######&.&&&&&&" ,
                COLUMN 094,"|",
                COLUMN 095,reg_s3[ind].r97     USING "#######&.&&&&&&" ,-- 1
                COLUMN 110,"|",
                COLUMN 111,reg_s3[ind].cv      USING "#######&.&&&&&&" ,-- 2,6,9
                COLUMN 126,"|",
                COLUMN 127,reg_s3[ind].cs      USING "#######&.&&&&&&" ,-- 5
                COLUMN 142,"|",
                COLUMN 143,reg_s3[ind].viv97p  USING "#########&.&&" ,--4
                COLUMN 158,"|",
                COLUMN 159,reg_s3[ind].viv97   USING "#######&.&&&&&&" ,--4
                COLUMN 174,"|",
                COLUMN 217,"|"
            --'\033015'
            
            PRINT COLUMN 1,"\300",L10,L1,
                   "\301",L10,L10,L10,L10,L10,L5,L2,L1,
                   "\301",L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                          L10,L5,L10,L1,L1,
                   "\331"
            --'\033015'
        
        END IF
        -------------------------------------------------------------
        -- IMPRESION DE TOTAL POR TIPO DE RETIRO PARA SIEFORE 4
        -------------------------------------------------------------
        IF reg_s4[ind].tot > 0 THEN
            PRINT COLUMN 1,"\332",L10,L1,
                           "\302",L10,L10,L10,L10,L10,L5,L2,L1,
                           "\302",L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                                  L10,L5,L10,L1,L1,
                           "\277"
          --  '\033015'
        
            PRINT
                COLUMN 001,"|",reg_s4[ind].tot  USING "#####",
                COLUMN 013,"|",
                COLUMN 015,"TOTALES POR TIPO RETIRO  ",reg_s4[ind].tipo_ret,
                COLUMN 072,"|",
                COLUMN 074,"SB4",
                COLUMN 078,"|",
                COLUMN 079,reg_s4[ind].total   USING "#######&.&&&&&&" ,
                COLUMN 094,"|",
                COLUMN 095,reg_s4[ind].r97     USING "#######&.&&&&&&" ,-- 1
                COLUMN 110,"|",
                COLUMN 111,reg_s4[ind].cv      USING "#######&.&&&&&&" ,-- 2,6,9
                COLUMN 126,"|",
                COLUMN 127,reg_s4[ind].cs      USING "#######&.&&&&&&" ,-- 5
                COLUMN 142,"|",
                COLUMN 143,reg_s4[ind].viv97p  USING "#########&.&&" ,--4
                COLUMN 158,"|",
                COLUMN 159,reg_s4[ind].viv97   USING "#######&.&&&&&&" ,--4
                COLUMN 174,"|",
                COLUMN 217,"|"
            --'\033015'
            
            PRINT COLUMN 1,"\300",L10,L1,
                   "\301",L10,L10,L10,L10,L10,L5,L2,L1,
                   "\301",L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                          L10,L5,L10,L1,L1,
                   "\331"
           -- '\033015'
        
        END IF
        -------------------------------------------------------------
        -- IMPRESION DE TOTAL POR TIPO DE RETIRO PARA SIEFORE 5
        -------------------------------------------------------------
        IF reg_s5[ind].tot > 0 THEN
            PRINT COLUMN 1,"\332",L10,L1,
                           "\302",L10,L10,L10,L10,L10,L5,L2,L1,
                           "\302",L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                                  L10,L5,L10,L1,L1,
                           "\277"
            --'\033015'
        
            PRINT
                COLUMN 001,"|",reg_s5[ind].tot  USING "#####",
                COLUMN 013,"|",
                COLUMN 015,"TOTALES POR TIPO RETIRO  ",reg_s5[ind].tipo_ret,
                COLUMN 072,"|",
                COLUMN 074,"SB5",
                COLUMN 078,"|",
                COLUMN 079,reg_s5[ind].total   USING "#######&.&&&&&&" ,
                COLUMN 094,"|",
                COLUMN 095,reg_s5[ind].r97     USING "#######&.&&&&&&" ,-- 1
                COLUMN 110,"|",
                COLUMN 111,reg_s5[ind].cv      USING "#######&.&&&&&&" ,-- 2,6,9
                COLUMN 126,"|",
                COLUMN 127,reg_s5[ind].cs      USING "#######&.&&&&&&" ,-- 5
                COLUMN 142,"|",
                COLUMN 143,reg_s5[ind].viv97p  USING "#########&.&&" ,--4
                COLUMN 158,"|",
                COLUMN 159,reg_s5[ind].viv97   USING "#######&.&&&&&&" ,--4
                COLUMN 174,"|",
                COLUMN 217,"|"
           -- '\033015'
            
            PRINT COLUMN 1,"\300",L10,L1,
                   "\301",L10,L10,L10,L10,L10,L5,L2,L1,
                   "\301",L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                          L10,L5,L10,L1,L1,
                   "\331"
            --'\033015'
        
        END IF
        ---------------------------------------------------
        -- DETALLE DE TOTALES DE NSS UNICOS Y SUMA DE VIVIENDA
        ---------------------------------------------------
            PRINT COLUMN 1,"\332",L10,L1,
                           L10,L10,L10,L10,L10,L5,L2,L2,
                           L5,L1,
                           L10,L5,L1,
                           L10,L5,L1,
                           L10,L5,L1,
                           L10,L5,L1,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                                  L10,L5,L10,L1,L1,
                           "\277"
            --'\033015'

            PRINT 
                COLUMN 001,"|",
                COLUMN 015,"TOTAL DE NSS UNICOS :  ",cont_nss_unicos USING "#####",
                COLUMN 142,"|",
                COLUMN 143,reg_s1[ind].viv97p +
                           reg_s2[ind].viv97p +
                           reg_s3[ind].viv97p +
                           reg_s4[ind].viv97p +
                           reg_s5[ind].viv97p    USING "#########&.&&",
                COLUMN 158,"|",
                COLUMN 159,reg_s1[ind].viv97  +
                           reg_s2[ind].viv97  +
                           reg_s3[ind].viv97  +
                           reg_s4[ind].viv97  +
                           reg_s5[ind].viv97     USING "#######&.&&&&&&",
                COLUMN 174,"|",
                COLUMN 217,"|"
            --'\033015'

            PRINT COLUMN 1,"\300",L10,L1,
                           L10,L10,L10,L10,L10,L5,L2,L2,
                           L5,L1,
                           L10,L5,L1,
                           L10,L5,L1,
                           L10,L5,L1,
                           L10,L5,L1,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                                  L10,L5,L10,L1,L1,
                           "\331"
            --'\033015'
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

    --------------------------------
    ON LAST ROW
    --------------------------------
    --NEED 49 LINES
          
    SKIP 3 LINES

    PRINT "R E S U M E N "
            --'\033015'
    PRINT
          
    --IMPRESION DE TODOS LOS DETALLES DE CADA TIPO DE RETIRO

    FOR i = 1 TO 6
        -------------------------------------------------------------
        -- IMPRESION DE TOTAL FINAL POR TIPO DE RETIRO PARA SIEFORE 1
        -------------------------------------------------------------
        IF reg_s1[i].tot > 0 THEN
        PRINT COLUMN 1,"\332",L10,L1,
                       "\302",L10,L10,L10,L10,L10,L5,L2,L1,
                       "\302",L5,
                       "\302",L10,L5,
                       "\302",L10,L5,
                       "\302",L10,L5,
                       "\302",L10,L5,
                       "\302",L10,L5,
                       "\302",L10,L5,
                       "\302",L10,L5,
                              L10,L5,L10,L1,L1,
                       "\277"
            --'\033015'

            LET reg_18.conta   = reg_18.conta  + reg_s1[i].tot
            LET reg_18.total   = reg_18.total  + reg_s1[i].total
            LET reg_18.r97     = reg_18.r97    + reg_s1[i].r97
            LET reg_18.cv      = reg_18.cv     + reg_s1[i].cv
            LET reg_18.cs      = reg_18.cs     + reg_s1[i].cs
            LET reg_18.viv97p  = reg_18.viv97p + reg_s1[i].viv97p
            LET reg_18.viv97   = reg_18.viv97  + reg_s1[i].viv97

        PRINT
            COLUMN 001,"|",reg_s1[i].tot USING "#####",
            COLUMN 013,"|",
            COLUMN 015,"TOTALES POR TIPO RETIRO  ",reg_s1[i].tipo_ret,
            COLUMN 072,"|",
            COLUMN 074,"SB1",
            COLUMN 078,"|",
            COLUMN 079,reg_s1[i].total   USING "#######&.&&&&&&" ,
            COLUMN 094,"|",
            COLUMN 095,reg_s1[i].r97     USING "#######&.&&&&&&" ,-- 1
            COLUMN 110,"|",
            COLUMN 111,reg_s1[i].cv      USING "#######&.&&&&&&" ,-- 2,6,9
            COLUMN 126,"|",
            COLUMN 127,reg_s1[i].cs      USING "#######&.&&&&&&" ,-- 5
            COLUMN 142,"|",
            COLUMN 143,reg_s1[i].viv97p  USING "#########&.&&" ,--4
            COLUMN 158,"|",
            COLUMN 159,reg_s1[i].viv97   USING "#######&.&&&&&&" ,--4
            COLUMN 174,"|",
            COLUMN 217,"|"
            --'\033015'
        
        PRINT COLUMN 1,"\300",L10,L1,
               "\301",L10,L10,L10,L10,L10,L5,L2,L1,
               "\301",L5,
               "\301",L10,L5,
               "\301",L10,L5,
               "\301",L10,L5,
               "\301",L10,L5,
               "\301",L10,L5,
               "\301",L10,L5,
               "\301",L10,L5,
                      L10,L5,L10,L1,L1,
               "\331"
            --'\033015'
        
        END IF

        -------------------------------------------------------------
        -- IMPRESION DE TOTAL FINAL POR TIPO DE RETIRO PARA SIEFORE 2
        -------------------------------------------------------------
        IF reg_s2[i].tot > 0 THEN
            PRINT COLUMN 1,"\332",L10,L1,
                           "\302",L10,L10,L10,L10,L10,L5,L2,L1,
                           "\302",L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                                  L10,L5,L10,L1,L1,
                           "\277"
            --'\033015'
        
            LET reg_19.conta   = reg_19.conta  + reg_s2[i].tot
            LET reg_19.total   = reg_19.total  + reg_s2[i].total
            LET reg_19.r97     = reg_19.r97    + reg_s2[i].r97
            LET reg_19.cv      = reg_19.cv     + reg_s2[i].cv
            LET reg_19.cs      = reg_19.cs     + reg_s2[i].cs
            LET reg_19.viv97p  = reg_19.viv97p + reg_s2[i].viv97p
            LET reg_19.viv97   = reg_19.viv97  + reg_s2[i].viv97

            PRINT
                COLUMN 001,"|",reg_s2[i].tot USING "#####",
                COLUMN 013,"|",
                COLUMN 015,"TOTALES POR TIPO RETIRO  ",reg_s2[i].tipo_ret,
                COLUMN 072,"|",
                COLUMN 074,"SB2",
                COLUMN 078,"|",
                COLUMN 079,reg_s2[i].total   USING "#######&.&&&&&&" ,
                COLUMN 094,"|",
                COLUMN 095,reg_s2[i].r97     USING "#######&.&&&&&&" ,-- 1
                COLUMN 110,"|",
                COLUMN 111,reg_s2[i].cv      USING "#######&.&&&&&&" ,-- 2,6,9
                COLUMN 126,"|",
                COLUMN 127,reg_s2[i].cs      USING "#######&.&&&&&&" ,-- 5
                COLUMN 142,"|",
                COLUMN 143,reg_s2[i].viv97p  USING "#########&.&&" ,--4
                COLUMN 158,"|",
                COLUMN 159,reg_s2[i].viv97   USING "#######&.&&&&&&" ,--4
                COLUMN 174,"|",
                COLUMN 217,"|"
            --'\033015'
            
            PRINT COLUMN 1,"\300",L10,L1,
                   "\301",L10,L10,L10,L10,L10,L5,L2,L1,
                   "\301",L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                          L10,L5,L10,L1,L1,
                   "\331"
            --'\033015'

        END IF

        -------------------------------------------------------------
        -- IMPRESION DE TOTAL FINAL POR TIPO DE RETIRO PARA SIEFORE 3
        -------------------------------------------------------------
        IF reg_s3[i].tot > 0 THEN
            PRINT COLUMN 1,"\332",L10,L1,
                           "\302",L10,L10,L10,L10,L10,L5,L2,L1,
                           "\302",L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                                  L10,L5,L10,L1,L1,
                           "\277"
            --'\033015'
        
            LET reg_20.conta   = reg_20.conta  + reg_s3[i].tot
            LET reg_20.total   = reg_20.total  + reg_s3[i].total
            LET reg_20.r97     = reg_20.r97    + reg_s3[i].r97
            LET reg_20.cv      = reg_20.cv     + reg_s3[i].cv
            LET reg_20.cs      = reg_20.cs     + reg_s3[i].cs
            LET reg_20.viv97p  = reg_20.viv97p + reg_s3[i].viv97p
            LET reg_20.viv97   = reg_20.viv97  + reg_s3[i].viv97

            PRINT
                COLUMN 001,"|",reg_s3[i].tot USING "#####",
                COLUMN 013,"|",
                COLUMN 015,"TOTALES POR TIPO RETIRO  ",reg_s3[i].tipo_ret,
                COLUMN 072,"|",
                COLUMN 074,"SB3",
                COLUMN 078,"|",
                COLUMN 079,reg_s3[i].total   USING "#######&.&&&&&&" ,
                COLUMN 094,"|",
                COLUMN 095,reg_s3[i].r97     USING "#######&.&&&&&&" ,-- 1
                COLUMN 110,"|",
                COLUMN 111,reg_s3[i].cv      USING "#######&.&&&&&&" ,-- 2,6,9
                COLUMN 126,"|",
                COLUMN 127,reg_s3[i].cs      USING "#######&.&&&&&&" ,-- 5
                COLUMN 142,"|",
                COLUMN 143,reg_s3[i].viv97p  USING "#########&.&&" ,--4
                COLUMN 158,"|",
                COLUMN 159,reg_s3[i].viv97   USING "#######&.&&&&&&" ,--4
                COLUMN 174,"|",
                COLUMN 217,"|"
            --'\033015'
            
            PRINT COLUMN 1,"\300",L10,L1,
                   "\301",L10,L10,L10,L10,L10,L5,L2,L1,
                   "\301",L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                          L10,L5,L10,L1,L1,
                   "\331"
            --'\033015'
        END IF
        -------------------------------------------------------------
        -- IMPRESION DE TOTAL FINAL POR TIPO DE RETIRO PARA SIEFORE 4
        -------------------------------------------------------------
        IF reg_s4[i].tot > 0 THEN
            PRINT COLUMN 1,"\332",L10,L1,
                           "\302",L10,L10,L10,L10,L10,L5,L2,L1,
                           "\302",L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                                  L10,L5,L10,L1,L1,
                           "\277"
            --'\033015'
        
            LET reg_21.conta   = reg_21.conta  + reg_s4[i].tot
            LET reg_21.total   = reg_21.total  + reg_s4[i].total
            LET reg_21.r97     = reg_21.r97    + reg_s4[i].r97
            LET reg_21.cv      = reg_21.cv     + reg_s4[i].cv
            LET reg_21.cs      = reg_21.cs     + reg_s4[i].cs
            LET reg_21.viv97p  = reg_21.viv97p + reg_s4[i].viv97p
            LET reg_21.viv97   = reg_21.viv97  + reg_s4[i].viv97

            PRINT
                COLUMN 001,"|",reg_s4[i].tot USING "#####",
                COLUMN 013,"|",
                COLUMN 015,"TOTALES POR TIPO RETIRO  ",reg_s4[i].tipo_ret,
                COLUMN 072,"|",
                COLUMN 074,"SB4",
                COLUMN 078,"|",
                COLUMN 079,reg_s4[i].total   USING "#######&.&&&&&&" ,
                COLUMN 094,"|",
                COLUMN 095,reg_s4[i].r97     USING "#######&.&&&&&&" ,-- 1
                COLUMN 110,"|",
                COLUMN 111,reg_s4[i].cv      USING "#######&.&&&&&&" ,-- 2,6,9
                COLUMN 126,"|",
                COLUMN 127,reg_s4[i].cs      USING "#######&.&&&&&&" ,-- 5
                COLUMN 142,"|",
                COLUMN 143,reg_s4[i].viv97p  USING "#########&.&&" ,--4
                COLUMN 158,"|",
                COLUMN 159,reg_s4[i].viv97   USING "#######&.&&&&&&" ,--4
                COLUMN 174,"|",
                COLUMN 217,"|"
            --'\033015'
            
            PRINT COLUMN 1,"\300",L10,L1,
                   "\301",L10,L10,L10,L10,L10,L5,L2,L1,
                   "\301",L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                          L10,L5,L10,L1,L1,
                   "\331"
            --'\033015'
        END IF
        -------------------------------------------------------------
        -- IMPRESION DE TOTAL FINAL POR TIPO DE RETIRO PARA SIEFORE 5
        -------------------------------------------------------------
        IF reg_s5[i].tot > 0 THEN
            PRINT COLUMN 1,"\332",L10,L1,
                           "\302",L10,L10,L10,L10,L10,L5,L2,L1,
                           "\302",L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                                  L10,L5,L10,L1,L1,
                           "\277"
            --'\033015'
        
            LET reg_22.conta   = reg_22.conta  + reg_s5[i].tot
            LET reg_22.total   = reg_22.total  + reg_s5[i].total
            LET reg_22.r97     = reg_22.r97    + reg_s5[i].r97
            LET reg_22.cv      = reg_22.cv     + reg_s5[i].cv
            LET reg_22.cs      = reg_22.cs     + reg_s5[i].cs
            LET reg_22.viv97p  = reg_22.viv97p + reg_s5[i].viv97p
            LET reg_22.viv97   = reg_22.viv97  + reg_s5[i].viv97

            PRINT
                COLUMN 001,"|",reg_s5[i].tot USING "#####",
                COLUMN 013,"|",
                COLUMN 015,"TOTALES POR TIPO RETIRO  ",reg_s5[i].tipo_ret,
                COLUMN 072,"|",
                COLUMN 074,"SB5",
                COLUMN 078,"|",
                COLUMN 079,reg_s5[i].total   USING "#######&.&&&&&&" ,
                COLUMN 094,"|",
                COLUMN 095,reg_s5[i].r97     USING "#######&.&&&&&&" ,-- 1
                COLUMN 110,"|",
                COLUMN 111,reg_s5[i].cv      USING "#######&.&&&&&&" ,-- 2,6,9
                COLUMN 126,"|",
                COLUMN 127,reg_s5[i].cs      USING "#######&.&&&&&&" ,-- 5
                COLUMN 142,"|",
                COLUMN 143,reg_s5[i].viv97p  USING "#########&.&&" ,--4
                COLUMN 158,"|",
                COLUMN 159,reg_s5[i].viv97   USING "#######&.&&&&&&" ,--4
                COLUMN 174,"|",
                COLUMN 217,"|"
            --'\033015'
            
            PRINT COLUMN 1,"\300",L10,L1,
                   "\301",L10,L10,L10,L10,L10,L5,L2,L1,
                   "\301",L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                          L10,L5,L10,L1,L1,
                   "\331"
            --'\033015'
        END IF

        IF reg_cnt[i].cont_nss_uni > 0 THEN   

         ---------------------------------------------------
         -- DETALLE DE TOTALES DE NSS UNICOS Y SUMA DE VIVIENDA
         ---------------------------------------------------
            PRINT COLUMN 1,"\332",L10,L1,
                           L10,L10,L10,L10,L10,L5,L2,L2,
                           L5,L1,
                           L10,L5,L1,
                           L10,L5,L1,
                           L10,L5,L1,
                           L10,L5,L1,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                                  L10,L5,L10,L1,L1,
                           "\277"
            --'\033015'
            PRINT 
                COLUMN 001,"|",
                COLUMN 015,"TOTAL DE NSS UNICOS : ",reg_cnt[i].cont_nss_uni USING "#####",
                COLUMN 142,"|",
                COLUMN 143,reg_s1[i].viv97p +
                           reg_s2[i].viv97p +
                           reg_s3[i].viv97p +
                           reg_s4[i].viv97p +
                           reg_s5[i].viv97p    USING "#########&.&&",
                COLUMN 158,"|",
                COLUMN 159,reg_s1[i].viv97  +
                           reg_s2[i].viv97  +
                           reg_s3[i].viv97  +
                           reg_s4[i].viv97  +
                           reg_s5[i].viv97     USING "#######&.&&&&&&",
                COLUMN 174,"|",
                COLUMN 217,"|"
           -- '\033015'

            PRINT COLUMN 1,"\300",L10,L1,
                           L10,L10,L10,L10,L10,L5,L2,L2,
                           L5,L1,
                           L10,L5,L1,
                           L10,L5,L1,
                           L10,L5,L1,
                           L10,L5,L1,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                                  L10,L5,L10,L1,L1,
                           "\331"
            --'\033015'
            SKIP 1 LINE
        END IF 

    END FOR  -- fin impresion  	

      SKIP 2 LINE

      -- IMPRIME TOTAL A LIQUIDAR SIEFORE PARA SIEFORE 1
      --------------------------------------------------
        PRINT COLUMN 1,"\332",L10,L1,
                       "\302",L10,L10,L10,L10,L10,L5,L2,L1,
                       "\302",L5,
                       "\302",L10,L5,
                       "\302",L10,L5,
                       "\302",L10,L5,
                       "\302",L10,L5,
                       "\302",L10,L5,
                       "\302",L10,L5,
                       "\302",L10,L5,
                              L10,L5,L10,L1,L1,
                       "\277"
            --'\033015'
        PRINT
            COLUMN 001,"|",reg_18.conta USING "#####",
            COLUMN 013,"|",
            COLUMN 015,"TOTAL LIQUIDADO:         " ,
            COLUMN 072,"|",
            COLUMN 074,"SB1",
            COLUMN 078,"|",
            COLUMN 079,reg_18.total   USING "#######&.&&&&&&" ,
            COLUMN 094,"|",
            COLUMN 095,reg_18.r97     USING "#######&.&&&&&&" ,-- 1
            COLUMN 110,"|",
            COLUMN 111,reg_18.cv      USING "#######&.&&&&&&" ,-- 2,6,9
            COLUMN 126,"|",
            COLUMN 127,reg_18.cs      USING "#######&.&&&&&&" ,-- 5
            COLUMN 142,"|",
            COLUMN 143,reg_18.viv97p  USING "#########&.&&" ,--4
            COLUMN 158,"|",
            COLUMN 159,reg_18.viv97   USING "#######&.&&&&&&" ,--4
            COLUMN 174,"|",
            COLUMN 217,"|"
            --'\033015'
        
        PRINT COLUMN 1,"\300",L10,L1,
               "\301",L10,L10,L10,L10,L10,L5,L2,L1,
               "\301",L5,
               "\301",L10,L5,
               "\301",L10,L5,
               "\301",L10,L5,
               "\301",L10,L5,
               "\301",L10,L5,
               "\301",L10,L5,
               "\301",L10,L5,
                      L10,L5,L10,L1,L1,
               "\331"
            --'\033015'

      -- IMPRIME TOTAL A LIQUIDAR SIEFORE PARA SIEFORE 2
      --------------------------------------------------
            PRINT COLUMN 1,"\332",L10,L1,
                           "\302",L10,L10,L10,L10,L10,L5,L2,L1,
                           "\302",L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                                  L10,L5,L10,L1,L1,
                           "\277"
            --'\033015'
        
            PRINT
                COLUMN 001,"|",reg_19.conta USING "#####"          ,
                COLUMN 013,"|"                                        , 
                COLUMN 015,"TOTAL LIQUIDADO:        "                 ,
                COLUMN 072,"|"                                        ,
                COLUMN 074,"SB2"                                      ,
                COLUMN 078,"|"                                        ,
                COLUMN 079,reg_19.total   USING "#######&.&&&&&&",
                COLUMN 094,"|",                
                COLUMN 095,reg_19.r97     USING "#######&.&&&&&&",-- 1
                COLUMN 110,"|",                
                COLUMN 111,reg_19.cv      USING "#######&.&&&&&&",-- 2,6,9
                COLUMN 126,"|",                
                COLUMN 127,reg_19.cs      USING "#######&.&&&&&&",-- 5
                COLUMN 142,"|",                
                COLUMN 143,reg_19.viv97p  USING "#########&.&&"  ,--4
                COLUMN 158,"|",                
                COLUMN 159,reg_19.viv97   USING "#######&.&&&&&&",--4
                COLUMN 174,"|"                                        ,
                COLUMN 217,"|"
            --'\033015'
            
            PRINT COLUMN 1,"\300",L10,L1,
                   "\301",L10,L10,L10,L10,L10,L5,L2,L1,
                   "\301",L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                          L10,L5,L10,L1,L1,
                   "\331"
            --'\033015'

      -- IMPRIME TOTAL A LIQUIDAR SIEFORE PARA SIEFORE 3
      --------------------------------------------------
            PRINT COLUMN 1,"\332",L10,L1,
                           "\302",L10,L10,L10,L10,L10,L5,L2,L1,
                           "\302",L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                                  L10,L5,L10,L1,L1,
                           "\277"
            --'\033015'
        
            PRINT
                COLUMN 001,"|",reg_20.conta USING "#####"          ,
                COLUMN 013,"|"                                        , 
                COLUMN 015,"TOTAL LIQUIDADO:        "                 ,
                COLUMN 072,"|"                                        ,
                COLUMN 074,"SB3"                                      ,
                COLUMN 078,"|"                                        ,
                COLUMN 079,reg_20.total   USING "#######&.&&&&&&",
                COLUMN 094,"|",                
                COLUMN 095,reg_20.r97     USING "#######&.&&&&&&",-- 1
                COLUMN 110,"|",                
                COLUMN 111,reg_20.cv      USING "#######&.&&&&&&",-- 2,6,9
                COLUMN 126,"|",                
                COLUMN 127,reg_20.cs      USING "#######&.&&&&&&",-- 5
                COLUMN 142,"|",                
                COLUMN 143,reg_20.viv97p  USING "#########&.&&"  ,--4
                COLUMN 158,"|",                
                COLUMN 159,reg_20.viv97   USING "#######&.&&&&&&",--4
                COLUMN 174,"|"                                        ,
                COLUMN 217,"|"
           -- '\033015'
            
            PRINT COLUMN 1,"\300",L10,L1,
                   "\301",L10,L10,L10,L10,L10,L5,L2,L1,
                   "\301",L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                          L10,L5,L10,L1,L1,
                   "\331"
            --'\033015'

      -- IMPRIME TOTAL A LIQUIDAR SIEFORE PARA SIEFORE 4
      --------------------------------------------------
            PRINT COLUMN 1,"\332",L10,L1,
                           "\302",L10,L10,L10,L10,L10,L5,L2,L1,
                           "\302",L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                                  L10,L5,L10,L1,L1,
                           "\277"
            --'\033015'
        
            PRINT
                COLUMN 001,"|",reg_21.conta USING "#####"          ,
                COLUMN 013,"|"                                        , 
                COLUMN 015,"TOTAL LIQUIDADO:        "                 ,
                COLUMN 072,"|"                                        ,
                COLUMN 074,"SB4"                                      ,
                COLUMN 078,"|"                                        ,
                COLUMN 079,reg_21.total   USING "#######&.&&&&&&",
                COLUMN 094,"|",                
                COLUMN 095,reg_21.r97     USING "#######&.&&&&&&",-- 1
                COLUMN 110,"|",                
                COLUMN 111,reg_21.cv      USING "#######&.&&&&&&",-- 2,6,9
                COLUMN 126,"|",                
                COLUMN 127,reg_21.cs      USING "#######&.&&&&&&",-- 5
                COLUMN 142,"|",                
                COLUMN 143,reg_21.viv97p  USING "#########&.&&"  ,--4
                COLUMN 158,"|",                
                COLUMN 159,reg_21.viv97   USING "#######&.&&&&&&",--4
                COLUMN 174,"|"                                        ,
                COLUMN 217,"|"
            --'\033015'
            
            PRINT COLUMN 1,"\300",L10,L1,
                   "\301",L10,L10,L10,L10,L10,L5,L2,L1,
                   "\301",L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                          L10,L5,L10,L1,L1,
                   "\331"
            --'\033015'

      -- IMPRIME TOTAL A LIQUIDAR SIEFORE PARA SIEFORE 5
      --------------------------------------------------
            PRINT COLUMN 1,"\332",L10,L1,
                           "\302",L10,L10,L10,L10,L10,L5,L2,L1,
                           "\302",L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                                  L10,L5,L10,L1,L1,
                           "\277"
            --'\033015'
        
            PRINT
                COLUMN 001,"|",reg_22.conta USING "#####"          ,
                COLUMN 013,"|"                                        , 
                COLUMN 015,"TOTAL LIQUIDADO:        "                 ,
                COLUMN 072,"|"                                        ,
                COLUMN 074,"SB5"                                      ,
                COLUMN 078,"|"                                        ,
                COLUMN 079,reg_22.total   USING "#######&.&&&&&&",
                COLUMN 094,"|",                
                COLUMN 095,reg_22.r97     USING "#######&.&&&&&&",-- 1
                COLUMN 110,"|",                
                COLUMN 111,reg_22.cv      USING "#######&.&&&&&&",-- 2,6,9
                COLUMN 126,"|",                
                COLUMN 127,reg_22.cs      USING "#######&.&&&&&&",-- 5
                COLUMN 142,"|",                
                COLUMN 143,reg_22.viv97p  USING "#########&.&&"  ,--4
                COLUMN 158,"|",                
                COLUMN 159,reg_22.viv97   USING "#######&.&&&&&&",--4
                COLUMN 174,"|"                                        ,
                COLUMN 217,"|"
            --'\033015'
            
            PRINT COLUMN 1,"\300",L10,L1,
                   "\301",L10,L10,L10,L10,L10,L5,L2,L1,
                   "\301",L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                   "\301",L10,L5,
                          L10,L5,L10,L1,L1,
                   "\331"
            --'\033015'

             ---------------------------------------------------
             -- DETALLE DE TOTALES DE NSS UNICOS Y SUMA DE VIVIENDA
             ---------------------------------------------------
            PRINT COLUMN 1,"\332",L10,L1,
                           L10,L10,L10,L10,L10,L5,L2,L2,
                           L5,L1,
                           L10,L5,L1,
                           L10,L5,L1,
                           L10,L5,L1,
                           L10,L5,L1,
                           "\302",L10,L5,
                           "\302",L10,L5,
                           "\302",L10,L5,
                                  L10,L5,L10,L1,L1,
                           "\277"
            --'\033015'

                 PRINT 
                     COLUMN 001,"|",
                     COLUMN 015,"TOTAL DE NSS UNICOS : ",cont_nss_fin USING "#####",
                     COLUMN 142,"|",
                     COLUMN 143,reg_18.viv97p +
                                reg_19.viv97p +
                                reg_20.viv97p +
                                reg_21.viv97p +
                                reg_22.viv97p  USING "#########&.&&",
                     COLUMN 158,"|",
                     COLUMN 159,reg_18.viv97  +
                                reg_19.viv97  +
                                reg_20.viv97  +
                                reg_21.viv97  +
                                reg_22.viv97   USING "#######&.&&&&&&",
                     COLUMN 174,"|",
                     COLUMN 217,"|"
            --'\033015'

            PRINT COLUMN 1,"\300",L10,L1,
                           L10,L10,L10,L10,L10,L5,L2,L2,
                           L5,L1,
                           L10,L5,L1,
                           L10,L5,L1,
                           L10,L5,L1,
                           L10,L5,L1,
                           "\301",L10,L5,
                           "\301",L10,L5,
                           "\301",L10,L5,
                                  L10,L5,L10,L1,L1,
                           "\331"
            --'\033015'
END REPORT

