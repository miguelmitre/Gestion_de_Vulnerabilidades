###########################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                       #
#Propietario       => E.F.P.                                              #
#Programa RETL810  => Reporte de Liquidacion de transferencia de Recursos #
#                     (en acciones)                                       #
#Fecha             => 23 DE FEBRERO DEL 2004.                             #
#Por               => MAURO MUNIZ CABALLERO / CLAUDIA URZUA ROSAS         #
#Actualizo         => JUAN CARLOS MENDOZA MORENO                          #
#Sistema           => RET                                                 #
###########################################################################
DATABASE safre_af
GLOBALS
    DEFINE w_codigo_afore LIKE tab_afore_local.codigo_afore 
    DEFINE g_param_dis    RECORD LIKE seg_modulo.*

    DEFINE HOY       DATE
    DEFINE enter     CHAR(1)
    DEFINE aux_pausa CHAR(1)
    DEFINE g_usuario CHAR(8)
    DEFINE hora      CHAR(8)
    DEFINE G_LISTA   CHAR(300)
    DEFINE G_IMPRE   CHAR(300)
    DEFINE impresion CHAR(300)

    DEFINE v RECORD
        fecha_corte DATE
    END RECORD

    DEFINE where_clause CHAR(250)

    DEFINE vparametros RECORD
        vfolio INTEGER,
        vfecha DATE
    END RECORD

    DEFINE folio_rx          SMALLINT
    DEFINE sw10              SMALLINT
    DEFINE fec_rx            DATE
    DEFINE maximo_folio      INTEGER 
    DEFINE select_stmt5      CHAR(2000)
    DEFINE ctos_regs         INTEGER 
END GLOBALS


MAIN
    DEFER INTERRUPT
    OPTIONS PROMPT LINE LAST,
        INPUT WRAP,
        ACCEPT KEY control-o

    CALL startlog("RETL810.log")
    CALL inicio()     #i
    CALL Consulta()   #c
 
    PROMPT "  PROCESO FINALIZADO...  <ENTER>  PARA SALIR " FOR CHAR enter
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

    LET HOY = TODAY
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

    OPEN WINDOW ventana_21 AT 10,4 WITH FORM "RETL8101" ATTRIBUTE( BORDER)
    DISPLAY "RETL810                     (Ctrl-C) Salir             (ESC) Ejecutar         " AT 1,1 ATTRIBUTE(REVERSE,green)

    LET int_flag = FALSE

    CONSTRUCT BY NAME where_clause ON dis_cuenta.folio,
                                      dis_cuenta.fecha_conversion 

        ON KEY(ESC)
           LET folio_rx = get_fldbuf(ret_transf_rx.folio)
           LET fec_rx   = get_fldbuf(dis_cuenta.fecha_conversion)

           LET int_flag =FALSE  
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
        tipo_retiro          LIKE ret_transf_rx.tipo_retiro   ,
        tipo_movimiento      LIKE dis_cuenta.tipo_movimiento  ,
        nss                  LIKE dis_cuenta.nss              ,
        subcuenta            LIKE dis_cuenta.subcuenta        ,
        folio                LIKE dis_cuenta.folio            ,
        consecutivo_lote     LIKE dis_cuenta.consecutivo_lote ,
        siefore              LIKE dis_cuenta.siefore          ,
        monto_en_acciones    LIKE dis_cuenta.monto_en_acciones,
        fecha_conversion     LIKE dis_cuenta.fecha_conversion
    END RECORD

    DEFINE lr_ret_transf  RECORD
        tipo_seguro          LIKE ret_transf_rx.tipo_seguro     ,
        tipo_pension         LIKE ret_transf_rx.tipo_pension    ,
        tipo_prestacion      LIKE ret_transf_rx.tipo_prestacion ,
        regimen              LIKE ret_transf_rx.regimen         ,
        fecha_ini_pen        LIKE ret_transf_rx.fecha_ini_pen   ,
        diag_registro        LIKE ret_transf_tx.diag_registro
    END RECORD     
        
    DEFINE where_clause      CHAR(250)
    DEFINE pos,cont_siefore  SMALLINT
    DEFINE cual_siefore      SMALLINT
    DEFINE query_var,indica  CHAR(1)
    DEFINE cont_datos        INTEGER
    DEFINE hora              CHAR(08)
    DEFINE v_permisos        CHAR(110)
    
    LET hora = TIME

    LET G_IMPRE = g_param_dis.ruta_listados CLIPPED,"/",HOY USING "YYYYMMDD",
                 ".810"

    START REPORT rpt_cuenta_imp TO G_IMPRE

    LET select_stmt5 = " SELECT CASE WHEN tipo_movimiento = 800 THEN 'A'      ",
                       "             WHEN tipo_movimiento = 810 THEN 'B'      ",
                       "             WHEN tipo_movimiento = 815 THEN 'C' END, ",
                       "        tipo_movimiento,nss,subcuenta,folio,consecutivo_lote, ",
                       "        siefore,monto_en_acciones,fecha_conversion ", 
                       " FROM dis_cuenta ",
                       " WHERE ",where_clause CLIPPED,
                       " AND dis_cuenta.subcuenta IN (1,2,5,6,9) ",
                       " AND dis_cuenta.tipo_movimiento in (800,810,815) ", 
                       " AND siefore IN (1,2,3,4,5) ",
		       " UNION ",
                       " SELECT CASE WHEN tipo_movimiento = 800 THEN 'A'      ",
                       "             WHEN tipo_movimiento = 810 THEN 'B'      ",
                       "             WHEN tipo_movimiento = 815 THEN 'C' END, ",
                       "        tipo_movimiento,nss,subcuenta,folio,consecutivo_lote, ",
                       "        siefore,monto_en_acciones,fecha_conversion ", 
                       " FROM dis_cuenta ",
                       " WHERE ",where_clause CLIPPED,
                       " AND dis_cuenta.subcuenta = 4 ",
                       " AND dis_cuenta.tipo_movimiento in (800,810,815) ", 
                       " AND siefore = 11 ",  
                       " GROUP BY 1,2,3,4,5,6,7,8,9 ",
                       " ORDER BY 1,2,3,4,5,6,7,8,9 "

       LET select_stmt5 = select_stmt5 CLIPPED

       PREPARE cust_stmt1 FROM select_stmt5
       DECLARE rept_curw CURSOR FOR cust_stmt1
        
       LET ctos_regs  =  0
       FOREACH rept_curw INTO l_record.tipo_retiro      ,
                              l_record.tipo_movimiento  ,
                              l_record.nss              ,
                              l_record.subcuenta        ,
                              l_record.folio            ,
                              l_record.consecutivo_lote ,
                              l_record.siefore          ,
                              l_record.monto_en_acciones,
                              l_record.fecha_conversion

          ERROR "  PROCESANDO INFORMACION...  "                              
          LET ctos_regs = ctos_regs + 1

          --SELECCION DE DATOS REQUERIDOS DE ret_transf_tx y ret_transf_rx
        
          SELECT B.tipo_seguro     ,
                 B.tipo_pension    ,
                 B.tipo_prestacion ,
                 B.regimen         ,
                 B.fecha_ini_pen   ,
                 A.diag_registro        
          INTO   lr_ret_transf.tipo_seguro     ,
                 lr_ret_transf.tipo_pension    ,
                 lr_ret_transf.tipo_prestacion ,
                 lr_ret_transf.regimen         ,
                 lr_ret_transf.fecha_ini_pen   ,
                 lr_ret_transf.diag_registro       
          FROM   ret_transf_tx     A ,
                 ret_transf_rx     B 
          WHERE  A.nss             = l_record.nss               --condicion
          AND    A.consecutivo     = l_record.consecutivo_lote  --condicion
	  AND    A.folio           = l_record.folio
          AND    A.nss             = B.nss                      --join 1
          AND    A.consecutivo     = B.consecutivo              --Join 1
          AND    A.folio           = B.folio                    --Join 1

	  SELECT COUNT(UNIQUE siefore)
	  INTO   cont_siefore
	  FROM   dis_cuenta
	  WHERE  nss              = l_record.nss
	  AND    consecutivo_lote = l_record.consecutivo_lote
	  AND    folio            = l_record.folio
	  AND    siefore         IN (1,2,3,4,5,11)

	  IF cont_siefore = 1 THEN
	     SELECT UNIQUE siefore
	     INTO   cual_siefore
	     FROM   dis_cuenta
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
                OUTPUT TO REPORT rpt_cuenta_imp(l_record.*      ,
                                                lr_ret_transf.* 
                                                )
             END IF
          ELSE
	     LET l_record.monto_en_acciones = 0
	     LET l_record.siefore = 1
             OUTPUT TO REPORT rpt_cuenta_imp(l_record.*      ,
                                             lr_ret_transf.* 
                                             )
          END IF
    END FOREACH

    IF ( ctos_regs <= 0 ) THEN
       ERROR "NO EXISTEN REGISTROS PARA ESTE CRITERIO..."
       SLEEP 2
       ERROR " "
    END IF 

    FINISH REPORT rpt_cuenta_imp

    LET v_permisos = " chmod 777 ",G_IMPRE     
    RUN v_permisos                             

    PROMPT "  DESEA GENERAR IMPRESION (S/N)?  " 
    FOR CHAR aux_pausa

    IF aux_pausa MATCHES "[Ss]" THEN
       LET impresion = "lp ",G_IMPRE
      -- LET impresion = "vi ",G_IMPRE
       RUN impresion
    END IF
END FUNCTION


REPORT rpt_cuenta_imp(l_record      ,
                      lr_ret_transf 
                     )
#rpt---------------------------------
    DEFINE l_record           RECORD
        tipo_retiro           LIKE ret_transf_rx.tipo_retiro   ,
        tipo_movimiento       LIKE dis_cuenta.tipo_movimiento  ,
        nss                   LIKE dis_cuenta.nss              ,
        subcuenta             LIKE dis_cuenta.subcuenta        ,
        folio                 LIKE dis_cuenta.folio            ,
        consecutivo_lote      LIKE dis_cuenta.consecutivo_lote ,
        siefore               LIKE dis_cuenta.siefore          ,
        monto_en_acciones     LIKE dis_cuenta.monto_en_acciones,
        fecha_conversion      LIKE dis_cuenta.fecha_conversion
    END RECORD

    DEFINE lr_ret_transf      RECORD
        tipo_seguro           LIKE ret_transf_rx.tipo_seguro     ,
        tipo_pension          LIKE ret_transf_rx.tipo_pension    ,
        tipo_prestacion       LIKE ret_transf_rx.tipo_prestacion ,
        regimen               LIKE ret_transf_rx.regimen         ,
        fecha_ini_pen         LIKE ret_transf_rx.fecha_ini_pen   ,
        diag_registro         LIKE ret_transf_tx.diag_registro
    END RECORD     
        
    DEFINE lr_precio_acc      RECORD
        precio_acc_sb1        LIKE dis_cuenta.precio_accion ,
        precio_acc_sb2        LIKE dis_cuenta.precio_accion ,
        precio_acc_sb3        LIKE dis_cuenta.precio_accion ,
        precio_acc_sb4        LIKE dis_cuenta.precio_accion ,
        precio_acc_sb5        LIKE dis_cuenta.precio_accion
    END RECORD

    DEFINE lr_montos          RECORD
        siefore               SMALLINT      ,
        total                 DECIMAL(16,6) ,
        r97                   DECIMAL(16,6) ,
        cv                    DECIMAL(16,6) ,
        cs                    DECIMAL(16,6) ,
        viv97                 DECIMAL(16,6) 
    END RECORD

    DEFINE r_datnss           RECORD
           nombre_afore       LIKE ret_transf_rx.nombre_afore,
           paterno_afore      LIKE ret_transf_rx.paterno_afore,
           materno_afore      LIKE ret_transf_rx.materno_afore
    END RECORD

    DEFINE rpt_afore          RECORD
        codigo_afore          LIKE safre_af:tab_afore_local.codigo_afore,
        razon_social          LIKE safre_af:tab_afore_local.razon_social
    END RECORD

    DEFINE nombre_siefore     CHAR(03)
    DEFINE encabezado         CHAR(30)
    DEFINE r_nombre           CHAR(60)
    DEFINE hoy                DATE
    DEFINE r_nom_rep          CHAR(7)
    DEFINE f_opera            DATE
    DEFINE f_trans            DATE

    DEFINE 
        nombre_final          CHAR(50) ,
        L1                    CHAR(1)  ,
        L2                    CHAR(2)  ,
        L5                    CHAR(5)  ,
        L10                   CHAR(10) ,
        cont                  INTEGER  ,  
        cont_tip              INTEGER  ,
        ind, i                SMALLINT

    DEFINE reg_s1, reg_s2, reg_s3, reg_s4, reg_s5     ARRAY[4] OF RECORD --1xcada tpo ret, la 4a x tots
           tot                INTEGER,
           tipo_ret           CHAR(1),
           total              LIKE dis_cuenta.monto_en_acciones ,
           r97                LIKE dis_cuenta.monto_en_acciones ,
           cv                 LIKE dis_cuenta.monto_en_acciones ,
           cs                 LIKE dis_cuenta.monto_en_acciones ,
           viv97              LIKE dis_cuenta.monto_en_acciones 
    END RECORD

    DEFINE reg_cnt ARRAY[4] OF RECORD
        cont_nss_uni      SMALLINT
    END RECORD  

    DEFINE 
        cont_nss_fin          ,
        cont_nss_unicos       ,
        cont_siefore          SMALLINT 

    OUTPUT
        TOP MARGIN 0
        BOTTOM MARGIN 0
        LEFT MARGIN   0
        --RIGHT MARGIN  200
        RIGHT MARGIN  0
        --PAGE LENGTH   90 
        PAGE LENGTH   45 

    ORDER BY l_record.tipo_retiro ,
             l_record.nss         ,
             l_record.siefore

    FORMAT
      FIRST PAGE HEADER

      LET  L1  = "\304"
      LET  L2  = "\304\304"
      LET  L5  = "\304\304\304\304\304"
      LET  L10 = "\304\304\304\304\304\304\304\304\304\304"

      PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'
      PRINT COLUMN 1,'\033e\033(s218T\033(s12H\033(s7B'

      --PRINT '\033E\033(10U\033&l1O\033&k2S\033&18d',
       --   '\033e\033(s23H'

      SELECT codigo_afore         , 
             razon_social
      INTO   rpt_afore.codigo_afore , 
             rpt_afore.razon_social
      FROM   safre_af:tab_afore_local

      LET hoy       = TODAY
      LET r_nom_rep = "RETL810"
      
      LET ind             = 0

      LET cont_nss_unicos = 0
      LET cont_nss_fin    = 0
        
      SELECT fecha_operacion   , 
             fecha_valor_trans
      INTO   f_opera           ,
             f_trans
      FROM   ret_cza_lote
      WHERE  folio = l_record.folio

      LET lr_precio_acc.precio_acc_sb1 = 0 
      LET lr_precio_acc.precio_acc_sb2 = 0 
      LET lr_precio_acc.precio_acc_sb3 = 0 
      LET lr_precio_acc.precio_acc_sb4 = 0 
      LET lr_precio_acc.precio_acc_sb5 = 0 

      SELECT precio_del_dia
      INTO   lr_precio_acc.precio_acc_sb1
      FROM   glo_valor_accion
      WHERE  fecha_valuacion = l_record.fecha_conversion
      AND    codigo_siefore  = 1
      GROUP BY 1

      SELECT precio_del_dia
      INTO   lr_precio_acc.precio_acc_sb2
      FROM   glo_valor_accion
      WHERE  fecha_valuacion = l_record.fecha_conversion
      AND    codigo_siefore  = 2
      GROUP BY 1
      
      SELECT precio_del_dia
      INTO   lr_precio_acc.precio_acc_sb3
      FROM   glo_valor_accion
      WHERE  fecha_valuacion = l_record.fecha_conversion
      AND    codigo_siefore  = 3
      GROUP BY 1

      SELECT precio_del_dia
      INTO   lr_precio_acc.precio_acc_sb4
      FROM   glo_valor_accion
      WHERE  fecha_valuacion = l_record.fecha_conversion
      AND    codigo_siefore  = 4
      GROUP BY 1

      SELECT precio_del_dia
      INTO   lr_precio_acc.precio_acc_sb5
      FROM   glo_valor_accion
      WHERE  fecha_valuacion = l_record.fecha_conversion
      AND    codigo_siefore  = 5
      GROUP BY 1

      IF rpt_afore.codigo_afore = 532 THEN
          LET encabezado = "SUBDIRECCION DE BENEFICIOS"
      ELSE 
          LET encabezado = "   MODULO DE RETIROS      "     
      END IF

      PRINT COLUMN 050,encabezado
          --  '\033015'
      SKIP 1 LINE
      PRINT COLUMN 039,"REPORTE DE LIQUIDACION DE TRANSFERENCIA DE RECURSOS"
           -- '\033015'
      SKIP 2 LINE
      PRINT COLUMN 002,"AFORE               : ",rpt_afore.codigo_afore USING "###","  ",rpt_afore.razon_social CLIPPED 
           -- '\033015'
      PRINT
      PRINT COLUMN 002,"FOLIO INTERNO       : ",l_record.folio USING "<<<<<<",
            COLUMN  95,"PROGRAMA          : ",r_nom_rep
           -- '\033015'
      PRINT
      PRINT COLUMN 002,"FECHA OPERACION     : ",f_opera USING "DD-MM-YYYY",
            COLUMN 045,"VALOR ACCION SB1 :",lr_precio_acc.precio_acc_sb1 USING "######&.&&&&&&" ,
            COLUMN 095,"PAGINA            : ",pageno USING "####"
            --'\033015'
      PRINT
      PRINT COLUMN 002,"FECHA DE LIQUIDACION: ",l_record.fecha_conversion USING "DD-MM-YYYY",
            COLUMN 045,"VALOR ACCION SB2 :",lr_precio_acc.precio_acc_sb2 USING "######&.&&&&&&" ,
            COLUMN 095,"FECHA DEL REPORTE : ",hoy USING "DD-MM-YYYY"
            --'\033015'
      PRINT
      PRINT COLUMN 045,"VALOR ACCION SB3 :",lr_precio_acc.precio_acc_sb3 USING "######&.&&&&&&"
           -- '\033015'
      PRINT
      PRINT COLUMN 045,"VALOR ACCION SB4 :",lr_precio_acc.precio_acc_sb4 USING "######&.&&&&&&"
           -- '\033015'
      PRINT
      PRINT COLUMN 045,"VALOR ACCION SB5 :",lr_precio_acc.precio_acc_sb5 USING "######&.&&&&&&"
            --'\033015'

      PRINT COLUMN 1,'\033e\033(s218T\033(s19H\033(s7B'

      PRINT COLUMN 1,"\332",L10,L1,
                     "\302",L10,L10,L10,L2,
                     "\302",L5,L1,
                     "\302",L5,L2,
                     "\302",L5,L1,
                     "\302",L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L1,
                            L10,L5,
                     "\277"
           -- '\033015'

      PRINT COLUMN 1,"|           |                                | TIPO | TIPO  | TIPO |                     MONTOS TRANSFERIDOS DE LA CUENTA INDIVIDUAL ( EN ACCIONES )                                              |"
           -- '\033015'
      PRINT COLUMN 1,"|           |                                |      |       |      \303",L2,L2,"\0302",L10,L5,"\302",L10,L5,"\302",L10,L5,"\302",L10,L5,"\302",L10,L5,"\302",L10,L5,L2,"\302",L10,L2,L1,"\302",L5,L2,L2,"|"
           -- '\033015'
      PRINT COLUMN 1,"|    NSS    |     NOMBRE DEL TRABAJADOR      |  DE  |  DE   |  DE  |    |     TOTAL     |      R97      |      CV       |      CS       |     V I V     |  FECHA  INICIO  | DIAGNOSTICO | REGIMEN |"
          --  '\033015'
      PRINT COLUMN 1,"|           |                                |      |       |      |    |               |               |               |               |               |                 |             |         |"
            --'\033015'
      PRINT COLUMN 1,"|           |                                |SEGURO|PENSION|PRESTA| SB |   ACCIONES    |   ACCIONES    |   ACCIONES    |   ACCIONES    |   ACCIONES    |  DE  PENSION    |             |         |"
            --'\033015'

      PRINT COLUMN 1,"\300",L10,L1,
                     "\301",L10,L10,L10,L2,
                     "\301",L5,L1,
                     "\301",L5,L2,
                     "\301",L5,L1,
                     "\301",L2,L2,
                     "\301",L10,L5,
                     "\301",L10,L5,
                     "\301",L10,L5,
                     "\301",L10,L5,
                     "\301",L10,L5,
                     "\301",L10,L5,L2,
                     "\301",L10,L2,L1,
                     "\301",L5,L2,L2,
                     "\331"
            --'\033015'

      --INICIALIZACION DE TOTALES
      FOR i=1 TO 4
          LET reg_s1[i].tot      = 0 
          LET reg_s1[i].tipo_ret = 0 
          LET reg_s1[i].total    = 0 
          LET reg_s1[i].r97      = 0 
          LET reg_s1[i].cv       = 0 
          LET reg_s1[i].cs       = 0 
          LET reg_s1[i].viv97    = 0 
          --------------------------
          LET reg_s2[i].tot      = 0 
          LET reg_s2[i].tipo_ret = 0 
          LET reg_s2[i].total    = 0 
          LET reg_s2[i].r97      = 0 
          LET reg_s2[i].cv       = 0 
          LET reg_s2[i].cs       = 0 
          LET reg_s2[i].viv97    = 0 
          --------------------------
          LET reg_s3[i].tot      = 0 
          LET reg_s3[i].tipo_ret = 0 
          LET reg_s3[i].total    = 0 
          LET reg_s3[i].r97      = 0 
          LET reg_s3[i].cv       = 0 
          LET reg_s3[i].cs       = 0 
          LET reg_s3[i].viv97    = 0 
          --------------------------
          LET reg_s4[i].tot      = 0 
          LET reg_s4[i].tipo_ret = 0 
          LET reg_s4[i].total    = 0 
          LET reg_s4[i].r97      = 0 
          LET reg_s4[i].cv       = 0 
          LET reg_s4[i].cs       = 0 
          LET reg_s4[i].viv97    = 0 
          --------------------------
          LET reg_s5[i].tot      = 0 
          LET reg_s5[i].tipo_ret = 0 
          LET reg_s5[i].total    = 0 
          LET reg_s5[i].r97      = 0 
          LET reg_s5[i].cv       = 0 
          LET reg_s5[i].cs       = 0 
          LET reg_s5[i].viv97    = 0 
      END FOR
      
      SKIP 2 LINE

    -------------
    PAGE HEADER
    -------------
      LET  L1  = "\304"
      LET  L2  = "\304\304"
      LET  L5  = "\304\304\304\304\304"
      LET  L10 = "\304\304\304\304\304\304\304\304\304\304"

      PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'
      PRINT COLUMN 1,'\033e\033(s218T\033(s12H\033(s7B'

      --PRINT '\033E\033(10U\033&l1O\033&k2S\033&18d',
       --   '\033e\033(s23H'

      SELECT codigo_afore, 
             razon_social
        INTO rpt_afore.codigo_afore, 
             rpt_afore.razon_social
        FROM safre_af:tab_afore_local

      LET hoy       = TODAY
      LET r_nom_rep = "RETL810"

      SELECT fecha_operacion, fecha_valor_trans
      INTO   f_opera, f_trans
      FROM ret_cza_lote
      WHERE folio = l_record.folio

      PRINT COLUMN 050,encabezado
           -- '\033015'
      SKIP 1 LINE
      PRINT COLUMN 039,"REPORTE DE LIQUIDACION DE TRANSFERENCIA DE RECURSOS"
            --'\033015'
      SKIP 2 LINE
      PRINT COLUMN 002,"AFORE               : ",rpt_afore.codigo_afore USING "###","  ",rpt_afore.razon_social CLIPPED 
            --'\033015'
      PRINT
      PRINT COLUMN 002,"FOLIO INTERNO       : ",l_record.folio USING "<<<<<<",
            COLUMN  95,"PROGRAMA          : ",r_nom_rep
            --'\033015'
      PRINT
      PRINT COLUMN 002,"FECHA OPERACION     : ",f_opera USING "DD-MM-YYYY",
            COLUMN 045,"VALOR ACCION SB1 :",lr_precio_acc.precio_acc_sb1 USING "######&.&&&&&&" ,
            COLUMN 095,"PAGINA            : ",pageno USING "####"
           -- '\033015'
      PRINT
      PRINT COLUMN 002,"FECHA DE LIQUIDACION: ",l_record.fecha_conversion USING "DD-MM-YYYY",
            COLUMN 045,"VALOR ACCION SB2 :",lr_precio_acc.precio_acc_sb2 USING "######&.&&&&&&" ,
            COLUMN 095,"FECHA DEL REPORTE : ",hoy USING "DD-MM-YYYY"
            --'\033015'
      PRINT
      PRINT COLUMN 045,"VALOR ACCION SB3 :",lr_precio_acc.precio_acc_sb3 USING "######&.&&&&&&"
           -- '\033015'
      PRINT
      PRINT COLUMN 045,"VALOR ACCION SB4 :",lr_precio_acc.precio_acc_sb4 USING "######&.&&&&&&"
           -- '\033015'
      PRINT
      PRINT COLUMN 045,"VALOR ACCION SB5 :",lr_precio_acc.precio_acc_sb5 USING "######&.&&&&&&"
            --'\033015'

     PRINT COLUMN 1,'\033e\033(s218T\033(s19H\033(s7B'

      PRINT COLUMN 1,"\332",L10,L1,
                     "\302",L10,L10,L10,L2,
                     "\302",L5,L1,
                     "\302",L5,L2,
                     "\302",L5,L1,
                     "\302",L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L1,
                            L10,L5,
                     "\277"
          --  '\033015'

      PRINT COLUMN 1,"|           |                                | TIPO | TIPO  | TIPO |                     MONTOS TRANSFERIDOS DE LA CUENTA INDIVIDUAL ( EN ACCIONES )                                              |"
            --'\033015'
      PRINT COLUMN 1,"|           |                                |      |       |      \303",L2,L2,"\0302",L10,L5,"\302",L10,L5,"\302",L10,L5,"\302",L10,L5,"\302",L10,L5,"\302",L10,L5,L2,"\302",L10,L2,L1,"\302",L5,L2,L2,"|"
            --'\033015'
      PRINT COLUMN 1,"|    NSS    |     NOMBRE DEL TRABAJADOR      |  DE  |  DE   |  DE  |    |     TOTAL     |      R97      |      CV       |      CS       |     V I V     |  FECHA  INICIO  | DIAGNOSTICO | REGIMEN |"
            --'\033015'
      PRINT COLUMN 1,"|           |                                |      |       |      |    |               |               |               |               |               |                 |             |         |"
           -- '\033015'
      PRINT COLUMN 1,"|           |                                |SEGURO|PENSION|PRESTA| SB |   ACCIONES    |   ACCIONES    |   ACCIONES    |    ACCIONES   |   ACCIONES    |  DE  PENSION    |             |         |"
            --'\033015'

      PRINT COLUMN 1,"\300",L10,L1,
                     "\301",L10,L10,L10,L2,
                     "\301",L5,L1,
                     "\301",L5,L2,
                     "\301",L5,L1,
                     "\301",L2,L2,
                     "\301",L10,L5,
                     "\301",L10,L5,
                     "\301",L10,L5,
                     "\301",L10,L5,
                     "\301",L10,L5,
                     "\301",L10,L5,L2,
                     "\301",L10,L2,L1,
                     "\301",L5,L2,L2,
                     "331"
           -- '\033015'

      SKIP 2 LINE

    -------------------------------
    AFTER GROUP OF l_record.siefore
    -------------------------------    
        SELECT nombres,
               paterno,
               materno
        INTO r_datnss.nombre_afore,
             r_datnss.paterno_afore,
             r_datnss.materno_afore
        FROM afi_mae_afiliado
        WHERE n_seguro = l_record.nss

        LET r_nombre = r_datnss.paterno_afore CLIPPED,' ',
                       r_datnss.materno_afore CLIPPED,' ',
                       r_datnss.nombre_afore CLIPPED
                            
        
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
        ELSE
         IF l_record.siefore = 2 THEN
            LET  nombre_siefore = "SB2"
         ELSE
          IF l_record.siefore = 3 THEN
             LET  nombre_siefore = "SB3"
          ELSE
           IF l_record.siefore = 4 THEN
              LET  nombre_siefore = "SB4"
           ELSE
            IF l_record.siefore = 5 THEN
               LET  nombre_siefore = "SB5"
            END IF
           END IF
          END IF
         END IF
        END IF

        LET lr_montos.total = GROUP SUM(l_record.monto_en_acciones)
        IF lr_montos.total IS NULL OR lr_montos.total = ' ' THEN
            LET lr_montos.total  = 0
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

        SELECT SUM(monto_en_acciones)
        INTO   lr_montos.viv97
        FROM   dis_cuenta
        WHERE  nss       = l_record.nss
        AND    folio     = l_record.folio
        AND    subcuenta = 4
        AND    siefore   = 11

        IF lr_montos.viv97 IS NULL OR lr_montos.viv97 = ' ' THEN
            LET lr_montos.viv97 = 0
        END IF     

        SELECT COUNT(UNIQUE siefore)                      
        INTO   cont_siefore                               
        FROM   dis_cuenta                          
        WHERE  nss              = l_record.nss                 
        AND    consecutivo_lote = l_record.consecutivo_lote    
        AND    siefore          IN(1,2,3,4,5)
        AND    folio            = l_record.folio                   
                                                          
        ------  duda -------
        --IF cont_siefore = 2 AND l_record.siefore = 2 then
        IF cont_siefore > 1 AND sw10 > 1 then
            LET lr_montos.viv97 = 0                
        END IF                                            

        PRINT 
            COLUMN 002,l_record.nss               ,
            COLUMN 014,r_nombre[1,35]             ,
            COLUMN 049,lr_ret_transf.tipo_seguro  ,
            COLUMN 057,lr_ret_transf.tipo_pension ,
            COLUMN 064,lr_ret_transf.tipo_prestacion  USING "#&" ,
            COLUMN 070,nombre_siefore                        ,
            COLUMN 074,lr_montos.total USING "#######&.&&&&&&",-- 1
            COLUMN 090,lr_montos.r97   USING "#######&.&&&&&&"   , -- 1
            COLUMN 106,lr_montos.cv    USING "#######&.&&&&&&"   , -- 2,6,9
            COLUMN 122,lr_montos.cs    USING "#######&.&&&&&&"   , -- 5
            COLUMN 138,lr_montos.viv97 USING "#######&.&&&&&&"   , --4
            COLUMN 158,lr_ret_transf.fecha_ini_pen USING "DD/MM/YYYY" ,
            COLUMN 173,lr_ret_transf.diag_registro ,
            COLUMN 191,lr_ret_transf.regimen
            --'\033015'
        PRINT 
    
        --A C U M U L A D O S  (POR TIPO DE RETIRO)
        
        CASE l_record.tipo_retiro
             WHEN "A" LET ind = 1
             WHEN "B" LET ind = 2
             WHEN "C" LET ind = 3
        END CASE

        IF l_record.siefore = 1 THEN
           LET reg_s1[ind].tot      = reg_s1[ind].tot    + 1 
           LET reg_s1[ind].tipo_ret = l_record.tipo_retiro
           --ACULUMADO POR TIPO DE RETIRO SIEFORE 1
           LET reg_s1[ind].total    = reg_s1[ind].total  + lr_montos.total  
           LET reg_s1[ind].r97      = reg_s1[ind].r97    + lr_montos.r97   
           LET reg_s1[ind].cv       = reg_s1[ind].cv     + lr_montos.cv    
           LET reg_s1[ind].cs       = reg_s1[ind].cs     + lr_montos.cs
           LET reg_s1[ind].viv97    = reg_s1[ind].viv97  + lr_montos.viv97
           --ACUMULADO PARA EL TOTAL TOTAL SIEFORE 1
           LET reg_s1[4].tot        = reg_s1[4].tot      + 1 
           LET reg_s1[4].total      = reg_s1[4].total    + lr_montos.total  
           LET reg_s1[4].r97        = reg_s1[4].r97      + lr_montos.r97   
           LET reg_s1[4].cv         = reg_s1[4].cv       + lr_montos.cv    
           LET reg_s1[4].cs         = reg_s1[4].cs       + lr_montos.cs
           LET reg_s1[4].viv97      = reg_s1[4].viv97    + lr_montos.viv97      
        ELSE
         IF l_record.siefore = 2 THEN
            LET reg_s2[ind].tot      = reg_s2[ind].tot    + 1 
            LET reg_s2[ind].tipo_ret = l_record.tipo_retiro
            --ACULUMADO POR TIPO DE RETIRO SIEFORE 2
            LET reg_s2[ind].total    = reg_s2[ind].total  + lr_montos.total  
            LET reg_s2[ind].r97      = reg_s2[ind].r97    + lr_montos.r97   
            LET reg_s2[ind].cv       = reg_s2[ind].cv     + lr_montos.cv    
            LET reg_s2[ind].cs       = reg_s2[ind].cs     + lr_montos.cs
            LET reg_s2[ind].viv97    = reg_s2[ind].viv97  + lr_montos.viv97           
            --ACUMULADO PARA EL TOTAL TOTAL SIEFORE 2
            LET reg_s2[4].tot        = reg_s2[4].tot      + 1 
            LET reg_s2[4].total      = reg_s2[4].total    + lr_montos.total  
            LET reg_s2[4].r97        = reg_s2[4].r97      + lr_montos.r97   
            LET reg_s2[4].cv         = reg_s2[4].cv       + lr_montos.cv    
            LET reg_s2[4].cs         = reg_s2[4].cs       + lr_montos.cs
            LET reg_s2[4].viv97      = reg_s2[4].viv97    + lr_montos.viv97     
         ELSE
          IF l_record.siefore = 3 THEN
             LET reg_s3[ind].tot      = reg_s3[ind].tot    + 1 
             LET reg_s3[ind].tipo_ret = l_record.tipo_retiro
             --ACUMULADO POR TIPO DE RETIRO SIEFORE 3
             LET reg_s3[ind].total    = reg_s3[ind].total  + lr_montos.total  
             LET reg_s3[ind].r97      = reg_s3[ind].r97    + lr_montos.r97   
             LET reg_s3[ind].cv       = reg_s3[ind].cv     + lr_montos.cv    
             LET reg_s3[ind].cs       = reg_s3[ind].cs     + lr_montos.cs
             LET reg_s3[ind].viv97    = reg_s3[ind].viv97  + lr_montos.viv97           
             --ACUMULADO PARA EL TOTAL TOTAL SIEFORE 3
             LET reg_s3[4].tot        = reg_s3[4].tot      + 1 
             LET reg_s3[4].total      = reg_s3[4].total    + lr_montos.total  
             LET reg_s3[4].r97        = reg_s3[4].r97      + lr_montos.r97   
             LET reg_s3[4].cv         = reg_s3[4].cv       + lr_montos.cv    
             LET reg_s3[4].cs         = reg_s3[4].cs       + lr_montos.cs
             LET reg_s3[4].viv97      = reg_s3[4].viv97    + lr_montos.viv97    
          ELSE
           IF l_record.siefore = 4 THEN
             LET reg_s4[ind].tot      = reg_s4[ind].tot    + 1 
             LET reg_s4[ind].tipo_ret = l_record.tipo_retiro
             --ACUMULADO POR TIPO DE RETIRO SIEFORE 4
             LET reg_s4[ind].total    = reg_s4[ind].total  + lr_montos.total  
             LET reg_s4[ind].r97      = reg_s4[ind].r97    + lr_montos.r97   
             LET reg_s4[ind].cv       = reg_s4[ind].cv     + lr_montos.cv    
             LET reg_s4[ind].cs       = reg_s4[ind].cs     + lr_montos.cs
             LET reg_s4[ind].viv97    = reg_s4[ind].viv97  + lr_montos.viv97           
             --ACUMULADO PARA EL TOTAL TOTAL SIEFORE 4
             LET reg_s4[4].tot        = reg_s4[4].tot      + 1 
             LET reg_s4[4].total      = reg_s4[4].total    + lr_montos.total  
             LET reg_s4[4].r97        = reg_s4[4].r97      + lr_montos.r97   
             LET reg_s4[4].cv         = reg_s4[4].cv       + lr_montos.cv    
             LET reg_s4[4].cs         = reg_s4[4].cs       + lr_montos.cs
             LET reg_s4[4].viv97      = reg_s4[4].viv97    + lr_montos.viv97    
           ELSE
            IF l_record.siefore = 5 THEN
              LET reg_s5[ind].tot      = reg_s5[ind].tot    + 1 
              LET reg_s5[ind].tipo_ret = l_record.tipo_retiro
              --ACUMULADO POR TIPO DE RETIRO SIEFORE 5
              LET reg_s5[ind].total    = reg_s5[ind].total  + lr_montos.total  
              LET reg_s5[ind].r97      = reg_s5[ind].r97    + lr_montos.r97   
              LET reg_s5[ind].cv       = reg_s5[ind].cv     + lr_montos.cv    
              LET reg_s5[ind].cs       = reg_s5[ind].cs     + lr_montos.cs
              LET reg_s5[ind].viv97    = reg_s5[ind].viv97  + lr_montos.viv97           
              --ACUMULADO PARA EL TOTAL TOTAL SIEFORE 5
              LET reg_s5[4].tot        = reg_s5[4].tot      + 1 
              LET reg_s5[4].total      = reg_s5[4].total    + lr_montos.total  
              LET reg_s5[4].r97        = reg_s5[4].r97      + lr_montos.r97   
              LET reg_s5[4].cv         = reg_s5[4].cv       + lr_montos.cv    
              LET reg_s5[4].cs         = reg_s5[4].cs       + lr_montos.cs
              LET reg_s5[4].viv97      = reg_s5[4].viv97    + lr_montos.viv97   
            END IF
           END IF
          END IF
         END IF
        END IF

     -----------------------------------
     BEFORE GROUP OF l_record.nss
     -----------------------------------
       LET cont_nss_unicos = cont_nss_unicos + 1
       LET sw10 = 0    
    ------------------------------------
    AFTER GROUP OF l_record.tipo_retiro
    ------------------------------------
       CASE l_record.tipo_retiro
            WHEN "A" LET ind = 1
            WHEN "B" LET ind = 2
            WHEN "C" LET ind = 3
       END CASE
          
       PRINT   
  
    -------------------------------------------------------------
    -- IMPRESION DE TOTAL POR TIPO DE RETIRO PARA SIEFORE 1
    -------------------------------------------------------------
    IF reg_s1[ind].tot > 0 THEN
 
        PRINT COLUMN 1,"\332",L10,L1,
                       "\302",L10,L10,L10,L5,L2,L2,L5,L1,L5,L2,L2,
                       "\302",L2,L2,     
                       "\302",L10,L5,
                       "\302",L10,L5,
                       "\302",L10,L5,
                       "\302",L10,L5,
                       "\302",L10,L5,
                       "\277"
            --'\033015'

             PRINT
                 COLUMN 001,"|",reg_s1[ind].tot   USING "#######"        ,
                 COLUMN 013,"|",
                 COLUMN 019,"TOTALES POR TIPO RETIRO ",reg_s1[ind].tipo_ret,
                 COLUMN 068,"|",
                 COLUMN 070,"SB1",
                 COLUMN 073,"|",
                 COLUMN 074,reg_s1[ind].total   USING "#######&.&&&&&&",
                 COLUMN 089,"|",
                 COLUMN 090,reg_s1[ind].r97     USING "#######&.&&&&&&",
                 COLUMN 105,"|",
                 COLUMN 106,reg_s1[ind].cv      USING "#######&.&&&&&&",
                 COLUMN 121,"|",
                 COLUMN 122,reg_s1[ind].cs      USING "#######&.&&&&&&",
                 COLUMN 137,"|",
                 COLUMN 138,reg_s1[ind].viv97   USING "#######&.&&&&&&",
                 COLUMN 153,"|"
            --'\033015'

        PRINT COLUMN 1,"\300",L10,L1,
                      "\30",L10,L10,L10,L5,L2,L2,L5,L1,L5,L2,L2,
                      "\301",L2,L2,     
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\331"
            --'\033015'
          END IF                            

    -------------------------------------------------------------
    -- IMPRESION DE TOTAL POR TIPO DE RETIRO PARA SIEFORE 2
    -------------------------------------------------------------
    IF reg_s2[ind].tot > 0 THEN
 
        PRINT COLUMN 1,"\332",L10,L1,
                      "\302",L10,L10,L10,L5,L2,L2,L5,L1,L5,L2,L2,
                      "\302",L2,L2,     
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\277"
            --'\033015'

             PRINT
                 COLUMN 001,"|",reg_s2[ind].tot   USING "#######"        ,
                 COLUMN 013,"|",
                 COLUMN 019,"TOTALES POR TIPO RETIRO ",reg_s2[ind].tipo_ret,
                 COLUMN 068,"|",
                 COLUMN 070,"SB2",
                 COLUMN 073,"|",
                 COLUMN 074,reg_s2[ind].total   USING "#######&.&&&&&&",
                 COLUMN 089,"|",
                 COLUMN 090,reg_s2[ind].r97     USING "#######&.&&&&&&",
                 COLUMN 105,"|",
                 COLUMN 106,reg_s2[ind].cv      USING "#######&.&&&&&&",
                 COLUMN 121,"|",
                 COLUMN 122,reg_s2[ind].cs      USING "#######&.&&&&&&",
                 COLUMN 137,"|",
                 COLUMN 138,reg_s2[ind].viv97   USING "#######&.&&&&&&",
                 COLUMN 153,"|"
            --'\033015'

        PRINT COLUMN 1,"\300",L10,L1,
                      "\30",L10,L10,L10,L5,L2,L2,L5,L1,L5,L2,L2,
                      "\301",L2,L2,     
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\331"
           -- '\033015'
          END IF                            

    -------------------------------------------------------------
    -- IMPRESION DE TOTAL POR TIPO DE RETIRO PARA SIEFORE 3
    -------------------------------------------------------------
    IF reg_s3[ind].tot > 0 THEN
 
        PRINT COLUMN 1,"\332",L10,L1,
                      "\302",L10,L10,L10,L5,L2,L2,L5,L1,L5,L2,L2,
                      "\302",L2,L2,     
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\277"
            --'\033015'

             PRINT
                 COLUMN 001,"|",reg_s3[ind].tot   USING "#######"        ,
                 COLUMN 013,"|",
                 COLUMN 019,"TOTALES POR TIPO RETIRO ",reg_s3[ind].tipo_ret,
                 COLUMN 068,"|",
                 COLUMN 070,"SB3",
                 COLUMN 073,"|",
                 COLUMN 074,reg_s3[ind].total   USING "#######&.&&&&&&",
                 COLUMN 089,"|",
                 COLUMN 090,reg_s3[ind].r97     USING "#######&.&&&&&&",
                 COLUMN 105,"|",
                 COLUMN 106,reg_s3[ind].cv      USING "#######&.&&&&&&",
                 COLUMN 121,"|",
                 COLUMN 122,reg_s3[ind].cs      USING "#######&.&&&&&&",
                 COLUMN 137,"|",
                 COLUMN 138,reg_s3[ind].viv97   USING "#######&.&&&&&&",
                 COLUMN 153,"|"
            --'\033015'

        PRINT COLUMN 1,"\300",L10,L1,
                      "\30",L10,L10,L10,L5,L2,L2,L5,L1,L5,L2,L2,
                      "\301",L2,L2,     
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\331"
           -- '\033015'
          END IF                            
    -------------------------------------------------------------
    -- IMPRESION DE TOTAL POR TIPO DE RETIRO PARA SIEFORE 4
    -------------------------------------------------------------
    IF reg_s4[ind].tot > 0 THEN
 
        PRINT COLUMN 1,"\332",L10,L1,
                      "\302",L10,L10,L10,L5,L2,L2,L5,L1,L5,L2,L2,
                      "\302",L2,L2,     
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\277"
            --'\033015'

             PRINT
                 COLUMN 001,"|",reg_s4[ind].tot   USING "#######"        ,
                 COLUMN 013,"|",
                 COLUMN 019,"TOTALES POR TIPO RETIRO ",reg_s4[ind].tipo_ret,
                 COLUMN 068,"|",
                 COLUMN 070,"SB4",
                 COLUMN 073,"|",
                 COLUMN 074,reg_s4[ind].total   USING "#######&.&&&&&&",
                 COLUMN 089,"|",
                 COLUMN 090,reg_s4[ind].r97     USING "#######&.&&&&&&",
                 COLUMN 105,"|",
                 COLUMN 106,reg_s4[ind].cv      USING "#######&.&&&&&&",
                 COLUMN 121,"|",
                 COLUMN 122,reg_s4[ind].cs      USING "#######&.&&&&&&",
                 COLUMN 137,"|",
                 COLUMN 138,reg_s4[ind].viv97   USING "#######&.&&&&&&",
                 COLUMN 153,"|"
            --'\033015'

        PRINT COLUMN 1,"\300",L10,L1,
                      "\30",L10,L10,L10,L5,L2,L2,L5,L1,L5,L2,L2,
                      "\301",L2,L2,     
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\331"
            --'\033015'
          END IF                            
    -------------------------------------------------------------
    -- IMPRESION DE TOTAL POR TIPO DE RETIRO PARA SIEFORE 5
    -------------------------------------------------------------
    IF reg_s5[ind].tot > 0 THEN
 
        PRINT COLUMN 1,"\332",L10,L1,
                      "\302",L10,L10,L10,L5,L2,L2,L5,L1,L5,L2,L2,
                      "\302",L2,L2,     
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\277"
            --'\033015'

             PRINT
                 COLUMN 001,"|",reg_s5[ind].tot   USING "#######"        ,
                 COLUMN 013,"|",
                 COLUMN 019,"TOTALES POR TIPO RETIRO ",reg_s5[ind].tipo_ret,
                 COLUMN 068,"|",
                 COLUMN 070,"SB5",
                 COLUMN 073,"|",
                 COLUMN 074,reg_s5[ind].total   USING "#######&.&&&&&&",
                 COLUMN 089,"|",
                 COLUMN 090,reg_s5[ind].r97     USING "#######&.&&&&&&",
                 COLUMN 105,"|",
                 COLUMN 106,reg_s5[ind].cv      USING "#######&.&&&&&&",
                 COLUMN 121,"|",
                 COLUMN 122,reg_s5[ind].cs      USING "#######&.&&&&&&",
                 COLUMN 137,"|",
                 COLUMN 138,reg_s5[ind].viv97   USING "#######&.&&&&&&",
                 COLUMN 153,"|"
            --'\033015'

        PRINT COLUMN 1,"\300",L10,L1,
                      "\30",L10,L10,L10,L5,L2,L2,L5,L1,L5,L2,L2,
                      "\301",L2,L2,     
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\331"
            --'\033015'
          END IF                            

        -- ACUMULA EN ARREGLO CONTADOR DE NSS UNICOS POR T DE RETIRO
        
        LET reg_cnt[ind].cont_nss_uni = cont_nss_unicos 

        ---------------------------------------------------
        -- DETALLE DE TOTALES DE NSS UNICOS Y SUMA DE VIVIENDA
        ---------------------------------------------------
            PRINT COLUMN 1,"\332",L10,L1,
                           L10,L10,L10,L10,L10,L5,
                           L5,     
                           L10,L5,L1,
                           L10,L5,L1,
                           L10,L5,L1,
                           L1,L10,L5,
                           "\302",L10,L5,
                           "\277"
            --'\033015'

            PRINT 
                COLUMN 001,"|",
                COLUMN 019,"TOTAL DE NSS UNICOS :  ",cont_nss_unicos USING "#####",
                COLUMN 137,"|",
                COLUMN 138,reg_s1[ind].viv97 +
                           reg_s2[ind].viv97 +
                           reg_s3[ind].viv97 +
                           reg_s4[ind].viv97 +
                           reg_s5[ind].viv97    USING "#######&.&&&&&&",
                COLUMN 153,"|"
            --'\033015'

            PRINT COLUMN 1,"\300",L10,L1,
                           L10,L10,L10,L10,L10,L5,
                           L5,     
                           L10,L5,L1,
                           L10,L5,L1,
                           L10,L5,L1,
                           L1,L10,L5,
                           "\301",L10,L5,
                           "\331"
            --'\033015'
 
        SKIP 1 LINE
        
        LET cont_nss_unicos   = 0

     -----------------------------------
     AFTER GROUP OF l_record.nss
     -----------------------------------
         LET cont_nss_fin = cont_nss_fin + 1

   --------------
   ON LAST ROW
   --------------
       --NEED 49 LINES
 
       SKIP 3 LINES
       
       PRINT "R E S U M E N "
           -- '\033015'
       PRINT
       
       --IMPRESION DE TODOS LOS DETALLES DE CADA TIPO DE RETIRO, CINCO SIEFORES

       FOR i = 1 TO 3
           -------------------------------------------------------------
           -- IMPRESION DE TOTAL FINAL POR TIPO DE RETIRO PARA SIEFORE 1
           -------------------------------------------------------------
           IF reg_s1[i].tot > 0 THEN

              PRINT COLUMN 1,"\332",L10,L1,
                             "\302",L10,L10,L10,L5,L2,L2,L5,L1,L5,L2,L2,
                             "\302",L2,L2,     
                             "\302",L10,L5,
                             "\302",L10,L5,
                             "\302",L10,L5,
                             "\302",L10,L5,
                             "\302",L10,L5,
                             "\277"
            --'\033015'

              PRINT
                 COLUMN 001,"|",reg_s1[i].tot   USING "#######"        ,
                 COLUMN 013,"|",
                 COLUMN 019,"TOTALES POR TIPO RETIRO ",reg_s1[i].tipo_ret,
                 COLUMN 068,"|",
                 COLUMN 070,"SB1",
                 COLUMN 073,"|",
                 COLUMN 074,reg_s1[i].total   USING "#######&.&&&&&&",
                 COLUMN 089,"|",
                 COLUMN 090,reg_s1[i].r97     USING "#######&.&&&&&&",
                 COLUMN 105,"|",
                 COLUMN 106,reg_s1[i].cv      USING "#######&.&&&&&&",
                 COLUMN 121,"|",
                 COLUMN 122,reg_s1[i].cs      USING "#######&.&&&&&&",
                 COLUMN 137,"|",
                 COLUMN 138,reg_s1[i].viv97   USING "#######&.&&&&&&",
                 COLUMN 153,"|"
           -- '\033015'

              PRINT COLUMN 1,"\300",L10,L1,
                             "\30",L10,L10,L10,L5,L2,L2,L5,L1,L5,L2,L2,
                             "\301",L2,L2,     
                             "\301",L10,L5,
                             "\301",L10,L5,
                             "\301",L10,L5,
                             "\301",L10,L5,
                             "\301",L10,L5,
                             "\331"
            --'\033015'
           END IF

           -------------------------------------------------------------
           -- IMPRESION DE TOTAL POR TIPO DE RETIRO PARA SIEFORE 2
           -------------------------------------------------------------
           IF reg_s2[i].tot > 0 THEN
 
              PRINT COLUMN 1,"\332",L10,L1,
                             "\302",L10,L10,L10,L5,L2,L2,L5,L1,L5,L2,L2,
                             "\302",L2,L2,     
                             "\302",L10,L5,
                             "\302",L10,L5,
                             "\302",L10,L5,
                             "\302",L10,L5,
                             "\302",L10,L5,
                             "\277"
            --'\033015'

              PRINT
                 COLUMN 001,"|",reg_s2[i].tot   USING "#######"        ,
                 COLUMN 013,"|",
                 COLUMN 019,"TOTALES POR TIPO RETIRO ",reg_s2[i].tipo_ret,
                 COLUMN 068,"|",
                 COLUMN 070,"SB2",
                 COLUMN 073,"|",
                 COLUMN 074,reg_s2[i].total   USING "#######&.&&&&&&",
                 COLUMN 089,"|",
                 COLUMN 090,reg_s2[i].r97     USING "#######&.&&&&&&",
                 COLUMN 105,"|",
                 COLUMN 106,reg_s2[i].cv      USING "#######&.&&&&&&",
                 COLUMN 121,"|",
                 COLUMN 122,reg_s2[i].cs      USING "#######&.&&&&&&",
                 COLUMN 137,"|",
                 COLUMN 138,reg_s2[i].viv97   USING "#######&.&&&&&&",
                 COLUMN 153,"|"
            --'\033015'

              PRINT COLUMN 1,"\300",L10,L1,
                             "\30",L10,L10,L10,L5,L2,L2,L5,L1,L5,L2,L2,
                             "\301",L2,L2,     
                             "\301",L10,L5,
                             "\301",L10,L5,
                             "\301",L10,L5,
                             "\301",L10,L5,
                             "\301",L10,L5,
                             "\331"
            --'\033015'
           END IF                            

           -------------------------------------------------------------
           -- IMPRESION DE TOTAL POR TIPO DE RETIRO PARA SIEFORE 3
           -------------------------------------------------------------
           IF reg_s3[i].tot > 0 THEN
 
              PRINT COLUMN 1,"\332",L10,L1,
                             "\302",L10,L10,L10,L5,L2,L2,L5,L1,L5,L2,L2,
                             "\302",L2,L2,     
                             "\302",L10,L5,
                             "\302",L10,L5,
                             "\302",L10,L5,
                             "\302",L10,L5,
                             "\302",L10,L5,
                             "\277"
            --'\033015'

              PRINT
                 COLUMN 001,"|",reg_s3[i].tot   USING "#######"        ,
                 COLUMN 013,"|",
                 COLUMN 019,"TOTALES POR TIPO RETIRO ",reg_s3[i].tipo_ret,
                 COLUMN 068,"|",
                 COLUMN 070,"SB3",
                 COLUMN 073,"|",
                 COLUMN 074,reg_s3[i].total   USING "#######&.&&&&&&",
                 COLUMN 089,"|",
                 COLUMN 090,reg_s3[i].r97     USING "#######&.&&&&&&",
                 COLUMN 105,"|",
                 COLUMN 106,reg_s3[i].cv      USING "#######&.&&&&&&",
                 COLUMN 121,"|",
                 COLUMN 122,reg_s3[i].cs      USING "#######&.&&&&&&",
                 COLUMN 137,"|",
                 COLUMN 138,reg_s3[i].viv97   USING "#######&.&&&&&&",
                 COLUMN 153,"|"
            --'\033015'

              PRINT COLUMN 1,"\300",L10,L1,
                             "\30",L10,L10,L10,L5,L2,L2,L5,L1,L5,L2,L2,
                             "\301",L2,L2,     
                             "\301",L10,L5,
                             "\301",L10,L5,
                             "\301",L10,L5,
                             "\301",L10,L5,
                             "\301",L10,L5,
                             "\331"
           -- '\033015'
           END IF                            
           -------------------------------------------------------------
           -- IMPRESION DE TOTAL POR TIPO DE RETIRO PARA SIEFORE 4
           -------------------------------------------------------------
           IF reg_s4[i].tot > 0 THEN
 
              PRINT COLUMN 1,"\332",L10,L1,
                             "\302",L10,L10,L10,L5,L2,L2,L5,L1,L5,L2,L2,
                             "\302",L2,L2,     
                             "\302",L10,L5,
                             "\302",L10,L5,
                             "\302",L10,L5,
                             "\302",L10,L5,
                             "\302",L10,L5,
                             "\277"
            --'\033015'

              PRINT
                 COLUMN 001,"|",reg_s4[i].tot   USING "#######"        ,
                 COLUMN 013,"|",
                 COLUMN 019,"TOTALES POR TIPO RETIRO ",reg_s4[i].tipo_ret,
                 COLUMN 068,"|",
                 COLUMN 070,"SB4",
                 COLUMN 073,"|",
                 COLUMN 074,reg_s4[i].total   USING "#######&.&&&&&&",
                 COLUMN 089,"|",
                 COLUMN 090,reg_s4[i].r97     USING "#######&.&&&&&&",
                 COLUMN 105,"|",
                 COLUMN 106,reg_s4[i].cv      USING "#######&.&&&&&&",
                 COLUMN 121,"|",
                 COLUMN 122,reg_s4[i].cs      USING "#######&.&&&&&&",
                 COLUMN 137,"|",
                 COLUMN 138,reg_s4[i].viv97   USING "#######&.&&&&&&",
                 COLUMN 153,"|"
           -- '\033015'

              PRINT COLUMN 1,"\300",L10,L1,
                             "\30",L10,L10,L10,L5,L2,L2,L5,L1,L5,L2,L2,
                             "\301",L2,L2,     
                             "\301",L10,L5,
                             "\301",L10,L5,
                             "\301",L10,L5,
                             "\301",L10,L5,
                             "\301",L10,L5,
                             "\331"
            --'\033015'
           END IF                            
           -------------------------------------------------------------
           -- IMPRESION DE TOTAL POR TIPO DE RETIRO PARA SIEFORE 5
           -------------------------------------------------------------
           IF reg_s5[i].tot > 0 THEN
 
              PRINT COLUMN 1,"\332",L10,L1,
                             "\302",L10,L10,L10,L5,L2,L2,L5,L1,L5,L2,L2,
                             "\302",L2,L2,     
                             "\302",L10,L5,
                             "\302",L10,L5,
                             "\302",L10,L5,
                             "\302",L10,L5,
                             "\302",L10,L5,
                             "\277"
           -- '\033015'

              PRINT
                 COLUMN 001,"|",reg_s5[i].tot   USING "#######"        ,
                 COLUMN 013,"|",
                 COLUMN 019,"TOTALES POR TIPO RETIRO ",reg_s5[i].tipo_ret,
                 COLUMN 068,"|",
                 COLUMN 070,"SB5",
                 COLUMN 073,"|",
                 COLUMN 074,reg_s5[i].total   USING "#######&.&&&&&&",
                 COLUMN 089,"|",
                 COLUMN 090,reg_s5[i].r97     USING "#######&.&&&&&&",
                 COLUMN 105,"|",
                 COLUMN 106,reg_s5[i].cv      USING "#######&.&&&&&&",
                 COLUMN 121,"|",
                 COLUMN 122,reg_s5[i].cs      USING "#######&.&&&&&&",
                 COLUMN 137,"|",
                 COLUMN 138,reg_s5[i].viv97   USING "#######&.&&&&&&",
                 COLUMN 153,"|"
           -- '\033015'

              PRINT COLUMN 1,"\300",L10,L1,
                             "\30",L10,L10,L10,L5,L2,L2,L5,L1,L5,L2,L2,
                             "\301",L2,L2,     
                             "\301",L10,L5,
                             "\301",L10,L5,
                             "\301",L10,L5,
                             "\301",L10,L5,
                             "\301",L10,L5,
                             "\331"
            --'\033015'
           END IF                            

           ---------------------------------------------------
           -- DETALLE DE TOTALES DE NSS UNICOS Y SUMA DE VIVIENDA
           ---------------------------------------------------
           IF reg_cnt[i].cont_nss_uni > 0 THEN 
                PRINT COLUMN 1,"\332",L10,L1,
                               L10,L10,L10,L10,L10,L5,
                               L5,     
                               L10,L5,L1,
                               L10,L5,L1,
                               L10,L5,L1,
                               L1,L10,L5,
                               "\302",L10,L5,
                               "\277"
           -- '\033015'
                
                PRINT 
                    COLUMN 001,"|",
                    COLUMN 019,"TOTAL DE NSS UNICOS :  ",reg_cnt[i].cont_nss_uni USING "#####",
                    COLUMN 137,"|",
                    COLUMN 138,reg_s1[i].viv97 +
                               reg_s2[i].viv97 +
                               reg_s3[i].viv97 +
                               reg_s4[i].viv97 +
                               reg_s5[i].viv97    USING "#######&.&&&&&&",
                    COLUMN 153,"|"
            --'\033015'
                
                PRINT COLUMN 1,"\300",L10,L1,
                               L10,L10,L10,L10,L10,L5,
                               L5,     
                               L10,L5,L1,
                               L10,L5,L1,
                               L10,L5,L1,
                               L1,L10,L5,
                               "\301",L10,L5,
                               "\331"
            --'\033015'
           END IF 
       END FOR

       SKIP 2 LINES 

       --------------------------------------------------
       -- IMPRIME TOTAL A LIQUIDAR SIEFORE PARA SIEFORE 1
       --------------------------------------------------
       PRINT COLUMN 1,"\332",L10,L1,
                      "\302",L10,L10,L10,L5,L2,L2,L5,L1,L5,L2,L2,
                      "\302",L2,L2,     
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\277"
            --'\033015'
       PRINT
              COLUMN 001,"|", reg_s1[4].tot    USING "#######"        ,
              COLUMN 013,"|",
              COLUMN 019,"TOTALES A TRANSFERIR: " ,
              COLUMN 068,"|",
              COLUMN 070,"SB1",
              COLUMN 073,"|",
              COLUMN 074,reg_s1[4].total   USING "#######&.&&&&&&",
              COLUMN 089,"|",
              COLUMN 090,reg_s1[4].r97     USING "#######&.&&&&&&",
              COLUMN 105,"|",
              COLUMN 106,reg_s1[4].cv      USING "#######&.&&&&&&",
              COLUMN 121,"|",
              COLUMN 122,reg_s1[4].cs      USING "#######&.&&&&&&",
              COLUMN 137,"|",
              COLUMN 138,reg_s1[4].viv97   USING "#######&.&&&&&&",
              COLUMN 153,"|"
            --'\033015'

       PRINT COLUMN 1,"\300",L10,L1,
                      "\30",L10,L10,L10,L5,L2,L2,L5,L1,L5,L2,L2,
                      "\301",L2,L2,     
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\331"
            --'\033015'

       --------------------------------------------------
       -- IMPRIME TOTAL A LIQUIDAR SIEFORE PARA SIEFORE 2
       --------------------------------------------------
       PRINT COLUMN 1,"\332",L10,L1,
                      "\302",L10,L10,L10,L5,L2,L2,L5,L1,L5,L2,L2,
                      "\302",L2,L2,     
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\277"
            --'\033015'

       PRINT COLUMN 001,"|", reg_s2[4].tot    USING "#######"        ,
             COLUMN 013,"|",
             COLUMN 019,"TOTALES A TRANSFERIR: ",
             COLUMN 068,"|",
             COLUMN 070,"SB2",
             COLUMN 073,"|",
             COLUMN 074,reg_s2[4].total   USING "#######&.&&&&&&",
             COLUMN 089,"|",
             COLUMN 090,reg_s2[4].r97     USING "#######&.&&&&&&",
             COLUMN 105,"|",
             COLUMN 106,reg_s2[4].cv      USING "#######&.&&&&&&",
             COLUMN 121,"|",
             COLUMN 122,reg_s2[4].cs      USING "#######&.&&&&&&",
             COLUMN 137,"|",
             COLUMN 138,reg_s2[4].viv97   USING "#######&.&&&&&&",
             COLUMN 153,"|"
            --'\033015'

       PRINT COLUMN 1,"\300",L10,L1,
                      "\30",L10,L10,L10,L5,L2,L2,L5,L1,L5,L2,L2,
                      "\301",L2,L2,     
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\331"
            --'\033015'

       --------------------------------------------------
       -- IMPRIME TOTAL A LIQUIDAR SIEFORE PARA SIEFORE 3
       --------------------------------------------------
       PRINT COLUMN 1,"\332",L10,L1,
                      "\302",L10,L10,L10,L5,L2,L2,L5,L1,L5,L2,L2,
                      "\302",L2,L2,     
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\277"
            --'\033015'

       PRINT COLUMN 001,"|", reg_s3[4].tot    USING "#######"        ,
             COLUMN 013,"|",
             COLUMN 019,"TOTALES A TRANSFERIR: ",
             COLUMN 068,"|",
             COLUMN 070,"SB3",
             COLUMN 073,"|",
             COLUMN 074,reg_s3[4].total   USING "#######&.&&&&&&",
             COLUMN 089,"|",
             COLUMN 090,reg_s3[4].r97     USING "#######&.&&&&&&",
             COLUMN 105,"|",
             COLUMN 106,reg_s3[4].cv      USING "#######&.&&&&&&",
             COLUMN 121,"|",
             COLUMN 122,reg_s3[4].cs      USING "#######&.&&&&&&",
             COLUMN 137,"|",
             COLUMN 138,reg_s3[4].viv97   USING "#######&.&&&&&&",
             COLUMN 153,"|"
            --'\033015'

       PRINT COLUMN 1,"\300",L10,L1,
                      "\30",L10,L10,L10,L5,L2,L2,L5,L1,L5,L2,L2,
                      "\301",L2,L2,     
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\331"
            --'\033015'
       --------------------------------------------------
       -- IMPRIME TOTAL A LIQUIDAR SIEFORE PARA SIEFORE 4
       --------------------------------------------------
       PRINT COLUMN 1,"\332",L10,L1,
                      "\302",L10,L10,L10,L5,L2,L2,L5,L1,L5,L2,L2,
                      "\302",L2,L2,     
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\277"
            --'\033015'

       PRINT COLUMN 001,"|", reg_s4[4].tot    USING "#######"        ,
             COLUMN 013,"|",
             COLUMN 019,"TOTALES A TRANSFERIR: ",
             COLUMN 068,"|",
             COLUMN 070,"SB4",
             COLUMN 073,"|",
             COLUMN 074,reg_s4[4].total   USING "#######&.&&&&&&",
             COLUMN 089,"|",
             COLUMN 090,reg_s4[4].r97     USING "#######&.&&&&&&",
             COLUMN 105,"|",
             COLUMN 106,reg_s4[4].cv      USING "#######&.&&&&&&",
             COLUMN 121,"|",
             COLUMN 122,reg_s4[4].cs      USING "#######&.&&&&&&",
             COLUMN 137,"|",
             COLUMN 138,reg_s4[4].viv97   USING "#######&.&&&&&&",
             COLUMN 153,"|"
            --'\033015'

       PRINT COLUMN 1,"\300",L10,L1,
                      "\30",L10,L10,L10,L5,L2,L2,L5,L1,L5,L2,L2,
                      "\301",L2,L2,     
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\331"
            --'\033015'
       --------------------------------------------------
       -- IMPRIME TOTAL A LIQUIDAR SIEFORE PARA SIEFORE 5
       --------------------------------------------------
       PRINT COLUMN 1,"\332",L10,L1,
                      "\302",L10,L10,L10,L5,L2,L2,L5,L1,L5,L2,L2,
                      "\302",L2,L2,     
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\302",L10,L5,
                      "\277"
            --'\033015'

       PRINT COLUMN 001,"|", reg_s5[4].tot    USING "#######"        ,
             COLUMN 013,"|",
             COLUMN 019,"TOTALES A TRANSFERIR: ",
             COLUMN 068,"|",
             COLUMN 070,"SB5",
             COLUMN 073,"|",
             COLUMN 074,reg_s5[4].total   USING "#######&.&&&&&&",
             COLUMN 089,"|",
             COLUMN 090,reg_s5[4].r97     USING "#######&.&&&&&&",
             COLUMN 105,"|",
             COLUMN 106,reg_s5[4].cv      USING "#######&.&&&&&&",
             COLUMN 121,"|",
             COLUMN 122,reg_s5[4].cs      USING "#######&.&&&&&&",
             COLUMN 137,"|",
             COLUMN 138,reg_s5[4].viv97   USING "#######&.&&&&&&",
             COLUMN 153,"|"
            --'\033015'

       PRINT COLUMN 1,"\300",L10,L1,
                      "\30",L10,L10,L10,L5,L2,L2,L5,L1,L5,L2,L2,
                      "\301",L2,L2,     
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\301",L10,L5,
                      "\331"
            --'\033015'
       ---------------------------------------------------
       -- DETALLE DE TOTALES DE NSS UNICOS Y SUMA DE VIVIENDA
       ---------------------------------------------------
       PRINT COLUMN 1,"\332",L10,L1,
                      L10,L10,L10,L10,L10,L5,
                      L5,     
                      L10,L5,L1,
                      L10,L5,L1,
                      L10,L5,L1,
                      L1,L10,L5,
                      "\302",L10,L5,
                      "\277"
            --'\033015'
        PRINT 
            COLUMN 001,"|",
            COLUMN 015,"TOTAL DE NSS UNICOS : ",cont_nss_fin USING "#####",
            COLUMN 137,"|",
            COLUMN 138,reg_s1[4].viv97 +
                       reg_s2[4].viv97 +
                       reg_s3[4].viv97 +
                       reg_s4[4].viv97 +
                       reg_s5[4].viv97    USING "#######&.&&&&&&",
            COLUMN 153,"|"
            --'\033015'

       PRINT COLUMN 1,"\300",L10,L1,
                      L10,L10,L10,L10,L10,L5,
                      L5,     
                      L10,L5,L1,
                      L10,L5,L1,
                      L10,L5,L1,
                      L1,L10,L5,
                      "\301",L10,L5,
                      "\331"
            --'\033015'

END REPORT
