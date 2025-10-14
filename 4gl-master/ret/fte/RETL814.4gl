################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                            #
#Propietario       => E.F.P.                                                   #
#Programa RETL813  => REPORTE PARA CONSAR ANEXO II (RETIRO PARCIAL)            #
#Fecha             => 04 DE JUNIO DE 2004                                      #
#Por               => JUAN CARLOS MENDOZA MORENO                               #
#Actualizo         =>                                                          #
#Fecha Actualiza   =>                                                          #
#Sistema           => RET                                                      #
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE #glo  
        g_param_dis  RECORD LIKE seg_modulo.*

    DEFINE reg_1 RECORD #glo #reg_1
        nss                   LIKE dis_cuenta.nss             ,
        subcuenta             LIKE dis_cuenta.subcuenta       ,
        tipo_movimiento       LIKE dis_cuenta.tipo_movimiento ,
        importe_rcv           LIKE dis_cuenta.monto_en_pesos  
    END RECORD

    DEFINE #glo #char 	
        c10_fecha_fin         CHAR(010) ,
        dia_fecha_fin         CHAR(002) ,
        enter                 CHAR(001) ,
        G_LISTA               CHAR(100) ,
        G_LISTA_PLANO         CHAR(100) ,
        lp                    CHAR(200) ,
        COMANDO               CHAR(400) ,
        descripcion           CHAR(050) ,
        impresion             CHAR(300) ,
        mes_fecha_fin         CHAR(002) ,
        nom_plano             CHAR(030) ,
        nom_reporte           CHAR(030) ,
        L1                    CHAR(001) ,
        L5                    CHAR(005) ,
        L10                   CHAR(010) 

    DEFINE #glo #date     
        hoy                   ,   
	HOY2                  ,
        fecha_ini             ,    
        fecha_fin             DATE

    DEFINE #glo #smallint
        cod_afore             SMALLINT
END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS PROMPT LINE LAST 

    CALL init() #i

    OPEN WINDOW retl8141 AT 4,4 WITH FORM "RETL8141" ATTRIBUTE (BORDER)
    DISPLAY "                               < CTRL-C >                                      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETL814         REPORTE RETIRO PARCIAL ANEXO II CONSAR                        " AT 3,1 ATTRIBUTE (REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE (REVERSE)

    DISPLAY BY NAME fecha_ini

    INPUT BY NAME fecha_fin WITHOUT DEFAULTS
        AFTER FIELD fecha_fin
            IF fecha_fin  IS NULL THEN
                ERROR "    LA FECHA NO PUEDE SER NULA" ATTRIBUTE(NORMAL)
                NEXT FIELD fecha_fin
            END IF

            LET c10_fecha_fin   = fecha_fin
            LET c10_fecha_fin   = c10_fecha_fin[01,02],
                                  "/01/",c10_fecha_fin[07,10]

            LET fecha_ini = c10_fecha_fin

            DISPLAY BY NAME fecha_ini

{svera
        ON KEY (ESC)

            SELECT "OK"
            FROM   dis_cuenta
	    WHERE  fecha_conversion BETWEEN fecha_ini AND  fecha_fin
            AND    subcuenta       in(1,2,5,6,9)
            AND    tipo_movimiento IN(870,875)
   
            IF STATUS = NOTFOUND THEN
                ERROR " NO SE ENCONTRARON REGISTROS "
                EXIT PROGRAM
            END IF
svera}
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
                LET nom_plano   = fecha_fin USING "YYYYMM","P",".txt"
                LET nom_reporte = fecha_fin USING "YYYYMM","P",".lst"
                EXIT WHILE
            ELSE
                PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR "
                FOR CHAR enter
                EXIT PROGRAM
           END IF
        END IF
    END WHILE
  
    DISPLAY "PROCESANDO INFORMACION " AT 20,2 ATTRIBUTE(REVERSE)

    CALL primer_paso()  #pp

    DISPLAY "ARCHIVO GENERADO EN LA RUTA "  AT  9,24
    DISPLAY g_param_dis.ruta_listados       AT 10,24

    DISPLAY "NOMBRE DEL ARCHIVO PLANO     " AT 12,24
    DISPLAY nom_plano                       AT 13,24
 
--  DISPLAY "NOMBRE DEL ARCHIVO REPORTE   " AT 15,24
--  DISPLAY nom_reporte                     AT 16,24

--  PROMPT " DESEA GENERAR IMPRESION S/N ? " FOR CHAR enter

    PROMPT " PROCESO FINALIZADO ...<ENTER> PARA SALIR " FOR CHAR enter
    EXIT PROGRAM

--  CLOSE WINDOW retl8141
 
    LET COMANDO = "chmod 777 ",g_param_dis.ruta_listados CLIPPED,"/",
                  nom_plano               
    RUN COMANDO

--  IF enter matches "[Ss]" THEN
--     LET impresion = "lp ",G_LISTA
--     RUN impresion
--  END IF
    CLOSE WINDOW retl8141
END MAIN

FUNCTION init()
#i-------------
    LET HOY = TODAY

    SELECT ruta_listados
    INTO   g_param_dis.ruta_listados
    FROM   seg_modulo
    WHERE  modulo_cod='ret'

    SELECT codigo_afore ,
           razon_social
    INTO   cod_afore    ,
           descripcion
    FROM   tab_afore_local

    LET c10_fecha_fin = HOY USING "MMDDYYYY"

    LET mes_fecha_fin = c10_fecha_fin[01,02]

    CALL num_dias_mes(mes_fecha_fin)
    RETURNING dia_fecha_fin

    LET c10_fecha_fin = mes_fecha_fin ,
                        dia_fecha_fin ,
                        c10_fecha_fin[05,08]

    LET fecha_fin = c10_fecha_fin

    LET c10_fecha_fin = fecha_fin
    LET c10_fecha_fin = c10_fecha_fin[01,02],
                        "/01/",c10_fecha_fin[07,10]

    LET fecha_ini = c10_fecha_fin

    LET L1  = "\304"
    LET L5  = "\304\304\304\304\304"
    LET L10 = "\304\304\304\304\304\304\304\304\304\304"
END FUNCTION


FUNCTION primer_paso()        
#pp------------------       
    DEFINE reg_aux  RECORD #loc #reg_aux
        tot_matrimonio        INTEGER                        ,
        tot_desempleo         INTEGER                        ,
        monto_matrimonio      LIKE dis_cuenta.monto_en_pesos ,
        monto_desempleo       LIKE dis_cuenta.monto_en_pesos
    END RECORD  

    DEFINE reg2 RECORD #loc #reg2
	monto_pesos_ret97_d ,
	monto_pesos_est_d   ,
	monto_pesos_esp_d   ,
	monto_pesos_cv_d    ,
        monto_pesos_so_d    ,             
	monto_pesos_ret97_m ,
	monto_pesos_est_m   ,
	monto_pesos_esp_m   ,
	monto_pesos_cv_m    ,
        monto_pesos_so_m    DECIMAL(10,2)
    END RECORD

    DEFINE #loc #decimal
        vd6_monto_pesos_cv_m   LIKE dis_cuenta.monto_en_pesos ,
        vd6_monto_pesos_est_m  LIKE dis_cuenta.monto_en_pesos ,
	vd6_monto_pesos_esp_m  LIKE dis_cuenta.monto_en_pesos ,
        vd6_monto_pesos_cv_d   LIKE dis_cuenta.monto_en_pesos ,
        vd6_monto_pesos_est_d  LIKE dis_cuenta.monto_en_pesos ,
	vd6_monto_pesos_esp_d  LIKE dis_cuenta.monto_en_pesos ,
	vd2monto_en_pesos      DECIMAL(10,2)

    DEFINE #loc #char
	vnss                CHAR(11)

    INITIALIZE reg_1.*   TO NULL
    INITIALIZE reg_aux.* TO NULL

--  LET G_LISTA = g_param_dis.ruta_listados CLIPPED,"/",nom_reporte

--  START REPORT listado_1 TO G_LISTA

    LET G_LISTA_PLANO = g_param_dis.ruta_listados CLIPPED,"/",nom_plano  

    START REPORT listado_2 TO G_LISTA_PLANO
        DECLARE cur_1 CURSOR FOR
        SELECT nss                ,
	       subcuenta          ,
	       tipo_movimiento    ,	
               sum(monto_en_pesos)
        FROM   dis_cuenta
	WHERE  fecha_conversion BETWEEN fecha_ini AND  fecha_fin
        AND    subcuenta        IN(1,5)
        AND    tipo_movimiento  IN (870,875)
        GROUP BY 1,2,3
  
        #ff
        LET reg_aux.tot_matrimonio     = 0
        LET reg_aux.tot_desempleo      = 0
        LET reg_aux.monto_matrimonio   = 0
        LET reg_aux.monto_desempleo    = 0
        LET reg2.monto_pesos_ret97_d   = 0
        LET reg2.monto_pesos_cv_d      = 0
        LET reg2.monto_pesos_so_d      = 0
	LET vd6_monto_pesos_cv_d       = 0
        LET vd6_monto_pesos_est_d      = 0
        LET vd6_monto_pesos_esp_d      = 0
        LET reg2.monto_pesos_ret97_m   = 0
        LET reg2.monto_pesos_cv_m      = 0
        LET reg2.monto_pesos_so_m      = 0
	LET vd6_monto_pesos_cv_m       = 0
        LET vd6_monto_pesos_est_m      = 0
        LET vd6_monto_pesos_esp_m      = 0

        FOREACH cur_1 INTO reg_1.*
            IF reg_1.tipo_movimiento = 870 THEN
                CASE reg_1.subcuenta 
		    WHEN 1
			LET reg2.monto_pesos_ret97_m = reg2.monto_pesos_ret97_m +
                                                       reg_1.importe_rcv
		    WHEN 5 
                        LET reg2.monto_pesos_so_m    = reg2.monto_pesos_so_m +
						       reg_1.importe_rcv
                END CASE
						  
            ELSE
                CASE reg_1.subcuenta 
		    WHEN 1
			LET reg2.monto_pesos_ret97_d = reg2.monto_pesos_ret97_d +
                                                       reg_1.importe_rcv
		    WHEN 5 
                        LET reg2.monto_pesos_so_d    = reg2.monto_pesos_so_d +
						       reg_1.importe_rcv
                END CASE
            END IF
        END FOREACH

	 DECLARE cur_3 CURSOR FOR
	 SELECT A.nss,ROUND(SUM(NVL(A.monto_en_pesos,0)),2)
	 FROM   dis_cuenta A
	 WHERE  A.fecha_conversion BETWEEN fecha_ini AND fecha_fin
	 AND    A.subcuenta        IN (2,6,9)
	 AND    A.tipo_movimiento  = 870
	 GROUP BY 1

         FOREACH cur_3 INTO vnss,vd2monto_en_pesos
             LET reg2.monto_pesos_cv_m = reg2.monto_pesos_cv_m +
					 vd2monto_en_pesos
         END FOREACH
    
	 DECLARE cur_4 CURSOR FOR
	 SELECT A.nss,ROUND(SUM(NVL(A.monto_en_pesos,0)),2)
	 FROM   dis_cuenta A
	 WHERE  A.fecha_conversion BETWEEN fecha_ini AND fecha_fin
	 AND    A.subcuenta        IN (2,6,9)
	 AND    A.tipo_movimiento  = 875
	 GROUP BY 1

         FOREACH cur_4 INTO vnss,vd2monto_en_pesos
             LET reg2.monto_pesos_cv_d = reg2.monto_pesos_cv_d +
					 vd2monto_en_pesos
         END FOREACH

        LET reg_aux.monto_matrimonio = reg2.monto_pesos_ret97_m +
				       reg2.monto_pesos_so_m    +
	                               reg2.monto_pesos_cv_m				
        LET reg_aux.monto_desempleo  = reg2.monto_pesos_ret97_d +
				       reg2.monto_pesos_so_d    +
     			               reg2.monto_pesos_cv_d



        SELECT COUNT(UNIQUE nss)
        INTO   reg_aux.tot_matrimonio 
        FROM   dis_cuenta
        WHERE  fecha_conversion BETWEEN fecha_ini AND  fecha_fin
        AND    subcuenta       in(1,2,5,6,9)
        AND    tipo_movimiento = 870

        SELECT COUNT(UNIQUE nss)
        INTO   reg_aux.tot_desempleo
        FROM   dis_cuenta
        WHERE  fecha_conversion BETWEEN fecha_ini AND  fecha_fin
        AND    subcuenta       in(1,2,5,6,9)
        AND    tipo_movimiento = 875

--      OUTPUT TO REPORT listado_1(reg_aux.*)
        OUTPUT TO REPORT listado_2(reg_aux.*)

    FINISH REPORT listado_2
--  FINISH REPORT listado_1
END FUNCTION

REPORT listado_1(reg_aux)
#l1---------------------
    DEFINE reg_aux  RECORD #loc #reg_aux
        tot_matrimonio        INTEGER                        ,
        tot_desempleo         INTEGER                        ,
        monto_matrimonio      LIKE dis_cuenta.monto_en_pesos ,
        monto_desempleo       LIKE dis_cuenta.monto_en_pesos
    END RECORD  

    OUTPUT
        PAGE LENGTH    90
        LEFT MARGIN     0
        RIGHT MARGIN  150
        TOP MARGIN      0
        BOTTOM MARGIN   0

    FORMAT
    PAGE HEADER
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'

        PRINT
            COLUMN 1, '\033e\033(s218T\033(s11H\033(s7B'

        PRINT
            COLUMN 46, "SUBDIRECCION DE BENEFICIOS"
        SKIP 1 LINES

        PRINT
            COLUMN 37, "REPORTE PARA CONSAR ANEXO II (RETIRO PARCIAL)"
        SKIP 1 LINES 

        PRINT
            COLUMN 1,'\033e\033(s218T\033(s12H\033(s7N'

        PRINT
            COLUMN 016,"AFORE  : ",descripcion CLIPPED,
            COLUMN 092,"PROGRAMA :   RETL814"
    
        SKIP 1 LINES
        PRINT
            COLUMN 016,"CLAVE  : ",cod_afore USING "###",
            COLUMN 092,"PAGINA   :",pageno USING "##########"

        SKIP 1 LINES
        PRINT
            COLUMN 092,"FECHA    :",hoy USING "DD-MM-YYYY"
    
        SKIP 5 LINES
        PRINT '\033e\033(s218T\033(s14H\033(s7B'

        PRINT
            COLUMN 18,"\332",L10,L10,L10,L10,L10,L5,L1,
                      "\302",L10,L10,L10,L10,L10,L5,L1,L1,"\277"

        PRINT
            COLUMN 18,"|                  M A T R I M O N I O                   |","                    D E S E M P L E O                    |"

        PRINT
            COLUMN 18,"\300",L10,L10,L10,L10,L10,L5,L1,
                      "\301",L10,L10,L10,L10,L10,L5,L1,L1,"\331"

        PRINT
            COLUMN 18,"\332",L10,L10,L5,L1,L1,          
	  	      "\302",L10,L10,L5,L1,L1,L1,
	  	      "\302",L10,L10,L5,L1,L1,L1,
	  	      "\302",L10,L10,L5,L1,L1,L1,
		      "\277"

        PRINT
            COLUMN 18,"|                           |",
		      "                            |",
		      "                            |",
		      "                            |"

        PRINT
            COLUMN 18,"|  TRABAJADORES LIQUIDADOS  |",
		      "   RCV (MONTOS LIQUIDADOS)  |",
		      "   TRABAJADORES LIQUIDADOS  |",
		      "   RCV (MONTOS LIQUIDADOS)  |"

        PRINT
            COLUMN 18,"|                           |",
		      "                            |",
		      "                            |",
		      "                            |"

        PRINT
            COLUMN 18,"\300",L10,L10,L5,L1,L1,          
	  	      "\301",L10,L10,L5,L1,L1,L1,
	  	      "\301",L10,L10,L5,L1,L1,L1,
	  	      "\301",L10,L10,L5,L1,L1,L1,
		      "\331"
    ON EVERY ROW
        SKIP 1 LINES 
        PRINT
            COLUMN 024, reg_aux.tot_matrimonio                       ,
            COLUMN 052, reg_aux.monto_matrimonio USING "#########.##",
            COLUMN 078, reg_aux.tot_desempleo                        ,
            COLUMN 110, reg_aux.monto_desempleo  USING "#########.##" 
END REPORT 

REPORT listado_2(reg_aux)
#l2---------------------
    DEFINE reg_aux  RECORD #loc #reg_aux
        tot_matrimonio        INTEGER                        ,
        tot_desempleo         INTEGER                        ,
        monto_matrimonio      LIKE dis_cuenta.monto_en_pesos ,
        monto_desempleo       LIKE dis_cuenta.monto_en_pesos
    END RECORD  

    OUTPUT
        PAGE LENGTH    90
        LEFT MARGIN     0
        RIGHT MARGIN  150
        TOP MARGIN      0
        BOTTOM MARGIN   0

    FORMAT
    PAGE HEADER
        PRINT
            COLUMN 01,cod_afore,"|",
                      descripcion,"|",
                      reg_aux.tot_matrimonio,"|",
                      reg_aux.monto_matrimonio USING "#########.##","|",
                      reg_aux.tot_desempleo,"|",
                      reg_aux.monto_desempleo  USING "#########.##","|" 
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
