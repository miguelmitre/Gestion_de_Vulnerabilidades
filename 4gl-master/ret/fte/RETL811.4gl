################################################################################
#Owner             => E.F.P.                                                   #
#Programa RETL811  => REPORTE MENSUAL DEL 2% DE RCV RETIRO 97                  #
#Fecha creacion    => 06 DE ABRIL DE 2004                                      #
#By                => STEFANIE DANIELA VERA PIÑA                               #
#Sistema           => RET                                                      #
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE #glo 
        g_param_dis           RECORD LIKE seg_modulo.*

    DEFINE reg1 RECORD #glo #reg1
        nss                   LIKE ret_solicitud_tx.nss            ,
	consecutivo           LIKE ret_solicitud_tx.consecutivo    ,
        tipo_seguro           LIKE ret_solicitud_tx.tipo_seguro    ,
        tipo_pension          LIKE ret_solicitud_tx.tipo_pension   ,
	monto_en_pesos        DECIMAL(10,2)                        ,
	monto_en_acciones     LIKE dis_cuenta.monto_en_acciones    ,
	fecha_conversion      DATE
    END RECORD 

    DEFINE #glo #char 
        enter                 CHAR(001) , 
        G_LISTA  	      CHAR(100) ,
        lp       	      CHAR(200) ,
        COMANDO  	      CHAR(400) ,
	c10fecha_corte        CHAR(010) ,
	mes_fecha_corte       CHAR(002) ,
	dia_fecha_corte       CHAR(002) ,
        c10_fecha_corte       CHAR(010) ,
	impresion             CHAR(300) ,
	g_usuario             CHAR(008) ,
        L1                    CHAR(001) ,
        L5                    CHAR(005) ,
        L10                   CHAR(010) ,
        L11                   CHAR(011) ,
        L18                   CHAR(018)

    DEFINE #glo #smallint
        sw_15                 SMALLINT

    DEFINE #glo #date 
	fecha_ini             ,
	fecha_corte           ,
        HOY                   DATE   

    DEFINE #glo #integer
        con_reg1              INTEGER

END GLOBALS

MAIN
    OPTIONS
        PROMPT LINE LAST

    CALL init() #i
    OPEN WINDOW retl811 AT 4,4 WITH FORM "RETL8111" ATTRIBUTE (BORDER)
    DISPLAY "                               < CTRL-C >                                      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETL811         REPORTE MENSUAL DEL 2% DE RCV RETIRO 97                       " AT 3,1 ATTRIBUTE (REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE (REVERSE)

    INPUT BY NAME fecha_corte WITHOUT DEFAULTS
        AFTER FIELD fecha_corte                                    
            IF fecha_corte  IS NULL THEN                         
                ERROR "    LA FECHA NO PUEDE SER NULA" ATTRIBUTE(NORMAL) 
                NEXT FIELD fecha_corte                                  
            END IF                                                 

            LET c10_fecha_corte = fecha_corte
	    LET c10_fecha_corte = c10_fecha_corte[01,02],
		 	          "/01/",c10_fecha_corte[07,10]                        
            LET fecha_ini = c10_fecha_corte

            SELECT unique "OK"
            FROM   dis_cuenta
            WHERE  fecha_conversion BETWEEN fecha_ini AND fecha_corte
	    AND    subcuenta       = 1
	    AND    tipo_movimiento = 830

            IF STATUS = NOTFOUND THEN 
                ERROR "    NO EXISTEN OPERACIONES DEL 2% EN ESTE MES"
		ATTRIBUTE(NORMAL)
                NEXT FIELD fecha_corte                                  
            END IF

        ON KEY (ESC)  
            IF fecha_corte  IS NULL THEN                         
                ERROR "    LA FECHA NO PUEDE SER NULA" ATTRIBUTE(NORMAL) 
                NEXT FIELD fecha_corte                                  
            END IF                                                 

            LET c10_fecha_corte = fecha_corte
	    LET c10_fecha_corte = c10_fecha_corte[01,02],
		 	          "/01/",c10_fecha_corte[07,10]                        
            LET fecha_ini = c10_fecha_corte

            SELECT unique "OK"
            FROM   dis_cuenta
            WHERE  fecha_conversion BETWEEN  fecha_ini AND fecha_corte
	    AND    subcuenta       = 1
	    AND    tipo_movimiento = 830

            IF STATUS = NOTFOUND THEN 
                ERROR "    NO EXISTEN OPERACIONES DEL 2% EN ESTE MES"
		ATTRIBUTE(NORMAL)
                NEXT FIELD fecha_corte                                  
            END IF
            
            EXIT INPUT 

        ON KEY (CONTROL-C)
	    PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
	    EXIT PROGRAM                                                        

        ON KEY (INTERRUPT)
	    PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
	    EXIT PROGRAM                                                        
    END INPUT

    WHILE TRUE
        PROMPT " ESTA SEGURO S/N ? " FOR CHAR enter
        IF enter MATCHES "[SsNn]" THEN
            IF enter MATCHES "[Ss]" THEN
                EXIT WHILE
            ELSE
		PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " 
		FOR CHAR enter
		EXIT PROGRAM
           END IF
        END IF
    END WHILE 

    CALL primer_paso()  #pp

    PROMPT " DESEA GENERAR IMPRESION S/N ? " FOR CHAR enter

    CLOSE WINDOW retl811

    LET COMANDO = "chmod 777 ",g_param_dis.ruta_listados CLIPPED,"/",
		  HOY USING "YYYYMMDD",".811"

    RUN COMANDO

    IF enter matches "[Ss]" THEN 
       LET impresion = "lp ",G_LISTA
       RUN impresion
    END IF

    OPEN WINDOW retl811 AT 4,4 WITH FORM "RETL8111" ATTRIBUTE (BORDER)
    DISPLAY "                               < CTRL-C >                                      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETL811            REPORTE MENSUAL DEL 2% DE RCV RETIRO 97                    " AT 3,1 ATTRIBUTE (REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE (REVERSE)

    DISPLAY fecha_corte TO fecha_corte                    

    IF enter matches "[Ss]" THEN 
       PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " FOR CHAR enter
    ELSE
       PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
    END IF

    CLOSE WINDOW retl811
END MAIN

FUNCTION init()
#i-------------
    LET HOY = TODAY

    LET sw_15 = 0

    SELECT USER      
    INTO   g_usuario
    FROM   tab_afore_local                                                     

    SELECT ruta_listados
    INTO   g_param_dis.ruta_listados
    FROM   seg_modulo
    WHERE  modulo_cod='ret'

    LET c10_fecha_corte = HOY USING "mmddyyyy"

    LET mes_fecha_corte = c10_fecha_corte[01,02] 

    CALL num_dias_mes(mes_fecha_corte)
    RETURNING dia_fecha_corte  

    LET c10_fecha_corte = mes_fecha_corte ,
		          dia_fecha_corte ,
		          c10_fecha_corte[05,08]

    LET fecha_corte = c10_fecha_corte

    LET L1  = "\304"
    LET L5  = "\304\304\304\304\304"
    LET L10 = "\304\304\304\304\304\304\304\304\304\304"
    LET L11 = "\304\304\304\304\304\304\304\304\304\304\304"
    LET L18 = 
    "\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304"

END FUNCTION

FUNCTION primer_paso()
#pp-------------------
    INITIALIZE reg1.* TO NULL

    LET G_LISTA = g_param_dis.ruta_listados CLIPPED,"/",
                  HOY USING "YYYYMMDD",".811"

    START REPORT listado_1 TO G_LISTA
    DECLARE cur_1 CURSOR FOR
    SELECT A.nss                    ,
           A.consecutivo            ,
	   A.tipo_seguro            ,
	   A.tipo_pension           ,
	   SUM(B.monto_en_pesos)    ,
	   SUM(B.monto_en_acciones) ,
	   B.fecha_conversion
    FROM   ret_solicitud_tx A , dis_cuenta B
    WHERE  A.nss              = B.nss
    AND    A.consecutivo      = B.consecutivo_lote
    AND    A.estado_solicitud = 8
    AND    A.tipo_retiro      = "E"
    AND    B.subcuenta        = 1
    AND    B.tipo_movimiento  IN (10,830)
    AND    B.fecha_conversion BETWEEN fecha_ini AND fecha_corte
    GROUP BY 1,2,3,4,7
    ORDER BY 1

    FOREACH cur_1 INTO reg1.*
        LET con_reg1 = con_reg1 + 1   

        DISPLAY "NRO. DE RETIROS TOTALES POR 2% DE RCV: ",
	        con_reg1 AT 12,18

        OUTPUT TO REPORT listado_1(reg1.*)
    END FOREACH
    FINISH REPORT listado_1
END FUNCTION
    
REPORT listado_1(reg1)
#l1-------------------
    DEFINE reg1 RECORD #loc #reg1
        nss                   LIKE ret_solicitud_tx.nss            ,
	consecutivo           LIKE ret_solicitud_tx.consecutivo    ,
        tipo_seguro           LIKE ret_solicitud_tx.tipo_seguro    ,
        tipo_pension          LIKE ret_solicitud_tx.tipo_pension   ,
	monto_en_pesos        DECIMAL(10,2)                        ,
	monto_en_acciones     LIKE dis_cuenta.monto_en_acciones    ,
	fecha_conversion      DATE
    END RECORD

    DEFINE reg2 RECORD #loc #reg2
        paterno               CHAR(30) ,
        materno               CHAR(30) ,
        nombres               CHAR(30)
    END RECORD

    DEFINE reg3 RECORD #loc #reg3
        paterno               CHAR(30) ,
        materno               CHAR(30) ,
        nombres               CHAR(30)
    END RECORD

    DEFINE #loc  #smallint
	vtipo_pago            ,
        i                     SMALLINT

    DEFINE #loc #integer
	 cuantos              ,
         total_nss            INTEGER

    DEFINE #loc  #char     
         nombre_bien          CHAR(45),
         vnom_benef           CHAR(45),
         vnom_trabaj          CHAR(45),
         v_paterno            CHAR(30),
         v_materno            CHAR(30),
         v_nombres            CHAR(30)

    OUTPUT
        PAGE LENGTH   90
        LEFT MARGIN    0
        RIGHT MARGIN 150
        TOP MARGIN     0
        BOTTOM MARGIN  0

    FORMAT
    PAGE HEADER

     PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'

     PRINT COLUMN 1,'\033e\033(s218T\033(s17H\033(s7B'

     PRINT COLUMN 52,"S U B D I R E C C I O N  D E  B E N E F I C I O S"
     SKIP 1 LINES

     PRINT COLUMN 52,"          REPORTE MENSUAL RETIRO 97              "

     SKIP 3 LINES

     PRINT COLUMN 133,"PROG. : RETL811"

     PRINT
     PRINT COLUMN 133,"PAGINA:",PAGENO USING "##"

     PRINT
     PRINT COLUMN 1,"PERIODO : ",fecha_ini   USING "DD/MM/YYYY"," A ",
				 fecha_corte USING "DD/MM/YYYY" ,
	   COLUMN 133,"FECHA : ", TODAY USING "DD/MM/YYYY"

     PRINT '\033e\033(s218T\033(s17H\033(s7B'

     PRINT COLUMN 1,"\332",L10,L1,
                    "\302",L10,L10,L10,L10,
                    "\302",L10,L10,L10,L10,
                    "\302",L5,L1,
                    "\302",L5,L1,L1,
                    "\302",L10,L1,L1,
                    "\302",L10,L5,
                    "\302",L10,L1,L1,
                    "\277"                             

     PRINT COLUMN 1,"|           |",
		    "                                        |", 
		    "                                        |",
		    " TIPO |",
		    " TIPO  |",
		    "  IMPORTE   |",
		    "    IMPORTE    |",
                    "  FECHA     |"
		    
     PRINT COLUMN 1,"|    NSS    |",
                    "         NOMBRE DEL TRABAJADOR          |",
		    "      PERSONAS INSTRUCCION DE PAGO      |",
		    "  DE  |",
		    "  DE   |",
		    "    NETO    |",
		    "      EN       |",
                    "   DEL      |"

     PRINT COLUMN 1,"|           |",
		    "                                        |", 
		    "                                        |",
		    "SEGURO|",
		    "PENSION|",
		    "  EN PESOS  |",
		    "    ACCIONES   |",
                    "  RETIRO    |"

 
     PRINT COLUMN 1,"\300",L10,L1,
                    "\301",L10,L10,L10,L10,
                    "\301",L10,L10,L10,L10,
                    "\301",L5,L1,
                    "\301",L5,L1,L1,
                    "\301",L10,L1,L1,
                    "\301",L10,L5,
                    "\301",L10,L1,L1,
                    "\331"                             
   ON EVERY ROW
       LET total_nss   = total_nss   + 1

       DECLARE cur_2 CURSOR FOR
       SELECT paterno,
              materno,
              nombres
       FROM   afi_mae_afiliado 
       WHERE  n_seguro = reg1.nss

       FOREACH cur_2 INTO reg3.paterno,
                          reg3.materno,
                          reg3.nombres

       END FOREACH            

       LET vnom_trabaj = reg3.paterno CLIPPED," ",
		         reg3.materno CLIPPED," ",
			 reg3.nombres

       SELECT "OK"
       FROM   ret_beneficiario
       WHERE  nss         = reg1.nss
       AND    consecutivo = reg1.consecutivo
       GROUP BY 1

       IF STATUS = NOTFOUND THEN
	   LET vnom_benef = vnom_trabaj
       ELSE
           DECLARE cur_3 CURSOR FOR
           SELECT paterno,
                  materno,
                  nombres
           FROM   ret_beneficiario 
           WHERE  nss         = reg1.nss
           AND    consecutivo = reg1.consecutivo    

           FOREACH  cur_3 INTO reg2.paterno,
                               reg2.materno,
                               reg2.nombres
           END FOREACH            

           LET vnom_benef = reg2.paterno CLIPPED," ",
		   	    reg2.materno CLIPPED," ",
			    reg2.nombres
       END IF

       IF  reg1.tipo_pension <> "VI"  
       AND reg1.tipo_pension <> "VO"
       AND reg1.tipo_pension <> "OR"
       AND reg1.tipo_pension <> "AS" THEN

           PRINT     
           COLUMN 002,reg1.nss                                        ,
           COLUMN 015,vnom_trabaj       CLIPPED                       ,
           COLUMN 056,vnom_benef        CLIPPED                       ,
           COLUMN 098,reg1.tipo_seguro                                ,
           COLUMN 105,reg1.tipo_pension                               ,
           COLUMN 111,reg1.monto_en_pesos    USING "#########.##"     ,
           COLUMN 124,reg1.monto_en_acciones USING "#########.######" ,
           COLUMN 141,reg1.fecha_conversion  USING "DD/MM/YYYY"       

       ELSE
           PRINT
           PRINT                                                                   
           COLUMN 002,reg1.nss                                    ,
           COLUMN 015,vnom_trabaj       CLIPPED                   ,
           COLUMN 056,"BENEFICIARIOS"   CLIPPED                   ,
           COLUMN 098,reg1.tipo_seguro                            ,
           COLUMN 105,reg1.tipo_pension                           ,
           COLUMN 111,reg1.monto_en_pesos    USING "#########.##"     ,
           COLUMN 124,reg1.monto_en_acciones USING "#########.######" ,
           COLUMN 141,reg1.fecha_conversion  USING "DD/MM/YYYY"       

           SELECT "OK"
           FROM   ret_beneficiario
           WHERE  nss         = reg1.nss
           AND    consecutivo = reg1.consecutivo
           GROUP BY 1
	   
	   IF STATUS = NOTFOUND THEN
		LET v_paterno = reg3.paterno
		LET v_materno = reg3.materno
		LET v_nombres = reg3.nombres

		PRINT
		    COLUMN 056,v_paterno CLIPPED," ",
			       v_materno CLIPPED," ",
			       v_nombres

           ELSE
               DECLARE cur_4 CURSOR FOR
               SELECT paterno,
	              materno,
	              nombres
               FROM   ret_beneficiario
	       WHERE  nss         = reg1.nss
	       AND    consecutivo = reg1.consecutivo    
 
               FOREACH cur_4 INTO v_paterno,
                                  v_materno,
                                  v_nombres

                   PRINT
                   COLUMN 056,v_paterno CLIPPED," ",
                              v_materno CLIPPED," ",
		              v_nombres
               END FOREACH     
           END IF

	   SKIP 1 LINE
       END IF

       IF lineno > 87 THEN                                                   
           SKIP TO TOP OF PAGE                                                
       END IF                                                                
                                                                           
   ON LAST ROW 
                                                                               
     SKIP 3 LINES

     PRINT COLUMN 1,"\332",L10,L1,
                    "\302",L10,L10,L10,L10,
                    "\302",L10,L10,L10,L10,
                    "\302",L5,L1,
                    "\302",L5,L1,L1,
                    "\302",L10,L1,L1,
                    "\302",L10,L5,L1,
                    "\302",L10,L1,L1,
                    "\277"

     PRINT
         COLUMN 001,"|  ", total_nss       USING "#######","  |"        ,
         COLUMN 022,"T O T A L E S     :             |",
                    "                                        |",
		    "      |",
		    "       |",
         COLUMN 111,sum(reg1.monto_en_pesos)     USING "#########.##","|",
         COLUMN 124,sum(reg1.monto_en_acciones)  USING "#########.######","|" ,
		    "            |"

     PRINT

         COLUMN 1,"\300",L10,L1,
                  "\301",L10,L10,L10,L10,
                  "\301",L10,L10,L10,L10,
                  "\301",L5,L1,
                  "\301",L5,L1,L1,
                  "\301",L10,L1,L1,
                  "\301",L10,L5,L1,
                  "\301",L10,L1,L1,
                  "\331"
   PAGE TRAILER

      PRINT COLUMN 001,"REPORTE : ",HOY USING "YYYYMMDD",".811"          
END REPORT 

FUNCTION num_dias_mes(mes)
#ndm----------------------
    DEFINE 
        mes       CHAR(02) ,
        num_dias  CHAR(02)

    IF  LENGTH(mes) = 1 THEN
	LET mes = '0',mes
    END IF

    CASE mes
        WHEN "01"  
            LET num_dias = 31
        WHEN "02"  
            LET num_dias = 28
        WHEN "03"  
            LET num_dias = 31
        WHEN "04"  
            LET num_dias = 30
        WHEN "05"  
            LET num_dias = 31
        WHEN "06"  
            LET num_dias = 30
        WHEN "07"  
            LET num_dias = 31
        WHEN "08"  
            LET num_dias = 31
        WHEN "09"  
            LET num_dias = 30
        WHEN "10" 
            LET num_dias = 31
        WHEN "11" 
            LET num_dias = 30
        WHEN "12" 
            LET num_dias = 31
    END CASE
RETURN num_dias
END FUNCTION
