################################################################################
#Proyecto          => SISTEMA DE AFORES. ( MEXICO )                            #
#Propietario       => E.F.P.                                                   #
#Programa RETL815  => REPORTE PARA CONSAR ANEXO III (RETIRO POR TRANSFERENCIA) #
#Fecha             => 04 DE JUNIO DEL 2004                                     #
#Por               => VERONICA LOPEZ SANCHEZ                                   #
#Sistema           => RET                                                      #
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE #glo #g_param
        g_param  RECORD       LIKE seg_modulo.*
    
    DEFINE reg_1 RECORD #glo #reg_1
        tipo_retiro           INTEGER
    END RECORD

    DEFINE reg_3 RECORD #glo #reg_3
        cve_afore              LIKE tab_afore_local.codigo_afore ,
        nom_afore              LIKE tab_afore_local.razon_social ,
        Atot_reg               INTEGER                           ,
        Aimpt_ret_97           DECIMAL(16,2)                     ,
        Aimpt_ces_vej          DECIMAL(16,2)                     ,
        Aimpt_cuo_soc          DECIMAL(16,2)                     ,
        Aimpt_viv_97           DECIMAL(16,2)                     ,
        Btot_reg               INTEGER                           ,
        Bimpt_ret_97           DECIMAL(16,2)                     ,
        Bimpt_ces_vej          DECIMAL(16,2)                     ,
        Bimpt_cuo_soc          DECIMAL(16,2)                     ,
        Bimpt_viv_97           DECIMAL(16,2)                     ,
        Ctot_reg               INTEGER                           ,
        Cimpt_ret_97           DECIMAL(16,2)                     ,
        Cimpt_ces_vej          DECIMAL(16,2)                     ,
        Cimpt_cuo_soc          DECIMAL(16,2)                     ,
        Cimpt_viv_97           DECIMAL(16,2) 
    END RECORD

    DEFINE #glo #date
        fecha_ini             ,
        fecha_fin             ,
        HOY2                  ,
        HOY                   DATE

    DEFINE #glo #char
        c10_fecha_fin         CHAR(010) , 
        dia_fecha_fin         CHAR(002) ,
        nom_plano             CHAR(030) ,
        L1                    CHAR(001) ,
        L5                    CHAR(005) ,
        L10                   CHAR(010) ,
        c50_razon_social      CHAR(050) ,
        enter                 CHAR(001) ,
        G_LISTA_PLANO         CHAR(200) ,
        G_LISTA               CHAR(200) ,
        mes_fecha_fin         CHAR(002) ,
        nom_arch              CHAR(100) ,
        LP                    CHAR(500) ,
        g_usuario             CHAR(008) ,
        hora1                 CHAR(008) ,
        hora                  CHAR(008) ,
        COMANDO               CHAR(400) 
   

    DEFINE #glo #smallint
        cod_afore             SMALLINT

    DEFINE #glo #integer
        vnss_liquidados       INTEGER
END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        ACCEPT KEY CONTROL-I ,
        PROMPT LINE LAST      

    CALL init()   #i

    OPEN WINDOW retl8151 AT 4,4 WITH FORM "RETL8151" ATTRIBUTE(BORDER)

    DISPLAY " < ESC > Acepta Datos             < Ctrl-C > Salir                             " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETL815    REPORTE PARA CONSAR ANEXO III (RETIRO POR TRANSFERENCIA)                  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY hoy USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

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
                    LET nom_plano   = fecha_fin USING "YYYYMM","T",".txt"
                    LET nom_arch    = fecha_fin USING "YYYYMM","T",".lst"
                    EXIT WHILE
                ELSE
                    PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR "
                    FOR CHAR enter
                    EXIT PROGRAM
               END IF
            END IF
        END WHILE

       DISPLAY "PROCESANDO INFORMACION " AT 20,2 ATTRIBUTE(REVERSE)

    CALL segundo_paso()       #sp

    DISPLAY "ARCHIVO GENERADO EN LA RUTA "  AT  9,24
    DISPLAY g_param.ruta_listados       AT 10,24

    DISPLAY "NOMBRE DEL ARCHIVO PLANO     " AT 12,24
    DISPLAY nom_plano                       AT 13,24

{svera
    DISPLAY "NOMBRE DEL ARCHIVO REPORTE   " AT 15,24
    DISPLAY nom_arch                     AT 16,24
svera}

--svera    PROMPT " DESEA GENERAR IMPRESION S/N ? " FOR CHAR enter
    PROMPT " PROCESO FINALIZADO ... <ENTER> PARA SALIR " FOR CHAR enter

    CLOSE WINDOW retl8151

    LET COMANDO = "chmod 777 ",g_param.ruta_listados CLIPPED,"/",
                  nom_plano

{svera
    LET COMANDO = "chmod 777 ",g_param.ruta_listados CLIPPED,"/",
                  nom_arch
svera}

    RUN COMANDO

{svera
    IF enter matches "[Ss]" THEN
        LET LP = "lp ", G_LISTA 
        RUN LP
    END IF
svera}
END MAIN


FUNCTION init()
#i-------------
    LET HOY   = TODAY
    LET hora1 = TIME
    LET hora  = hora1
    LET hora1 = hora[1,2],hora[4,5]

    SELECT codigo_afore ,
           razon_social
    INTO   cod_afore    ,
           c50_razon_social
    FROM   tab_afore_local

    SELECT *        ,
	   USER
    INTO  g_param.* ,
	  g_usuario
    FROM  seg_modulo
    WHERE modulo_cod = "ret"

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

    INITIALIZE reg_1.* TO NULL

    INITIALIZE reg_3.* TO NULL

    LET reg_3.Atot_reg        = 0            
    LET reg_3.Aimpt_ret_97    = 0   
    LET reg_3.Aimpt_ces_vej   = 0
    LET reg_3.Aimpt_cuo_soc   = 0
    LET reg_3.Aimpt_viv_97    = 0
    LET reg_3.Btot_reg        = 0
    LET reg_3.Bimpt_ret_97    = 0
    LET reg_3.Bimpt_ces_vej   = 0
    LET reg_3.Bimpt_cuo_soc   = 0     
    LET reg_3.Bimpt_viv_97    = 0
    LET reg_3.Ctot_reg        = 0
    LET reg_3.Cimpt_ret_97    = 0
    LET reg_3.Cimpt_ces_vej   = 0
    LET reg_3.Cimpt_cuo_soc   = 0
    LET reg_3.Cimpt_viv_97    = 0

    LET L1 = "\304"
    LET L5 = "\304\304\304\304\304"
    LET L10 = "\304\304\304\304\304\304\304\304\304\304"
END FUNCTION


FUNCTION segundo_paso()
#sp-------------------

--svera    LET G_LISTA = g_param.ruta_listados CLIPPED,"/",nom_arch   

        LET G_LISTA_PLANO = g_param.ruta_listados CLIPPED,"/",nom_plano  

        START REPORT listado_1 TO G_LISTA_PLANO  
            DECLARE cur_1 CURSOR FOR
            SELECT tipo_movimiento
            FROM   dis_cuenta
            WHERE  fecha_conversion BETWEEN fecha_ini AND  fecha_fin
            AND    tipo_movimiento  IN(800,810,815)
            AND    subcuenta        IN(1,2,4,5,6,9)
            GROUP BY 1

            FOREACH cur_1 INTO reg_1.*
                OUTPUT TO REPORT listado_1(reg_1.*)
            END FOREACH
        FINISH REPORT listado_1

--svera    LET G_LISTA_PLANO = g_param.ruta_listados CLIPPED,"/",nom_plano  

    LET reg_3.cve_afore = cod_afore
    LET reg_3.nom_afore = c50_razon_social

    #ff
    START REPORT listado_2 TO G_LISTA_PLANO
        OUTPUT TO REPORT listado_2(reg_3.*)
    FINISH REPORT listado_2
END FUNCTION


REPORT listado_1(reg_1)
#l1-------------------
    DEFINE 	reg_1  RECORD
                tipo_retiro       INTEGER   
    END RECORD

    DEFINE      reg_2  RECORD
                monto_pesos_r     ,
                monto_pesos_cv    ,
                monto_pesos_cs    ,
                monto_pesos_viv97 DECIMAL(10,2)
    END RECORD
 
    DEFINE #loc #char
        vnss                      CHAR(11) ,
        vtipo_retiro              CHAR(01)
 
    DEFINE #loc #date   
	vfecha_conversion         DATE

    DEFINE #loc #smallint
	vcont_nss                 ,
        vsubcuenta                SMALLINT
 
    DEFINE vd6_monto_pesos_cv   LIKE dis_cuenta.monto_en_pesos ,
           vd6_monto_pesos_est  LIKE dis_cuenta.monto_en_pesos ,
           vd6_monto_pesos_esp  LIKE dis_cuenta.monto_en_pesos ,
           vd6_suma             LIKE dis_cuenta.monto_en_pesos ,
           vmonto_en_pesos      DECIMAL(16,6)  ,
	   vd2monto_en_pesos    DECIMAL(10,2)
  
    DEFINE vtipo_movimiento CHAR(1)

    OUTPUT
        PAGE LENGTH    60
        LEFT MARGIN     0
        RIGHT MARGIN  150
        TOP MARGIN      0
        BOTTOM MARGIN   0

    FORMAT
        PAGE HEADER

        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'

        PRINT
            COLUMN 1, '\033e\033(s218T\033(s12H\033(s7B'

        PRINT
            COLUMN 51, "SUBDIRECCION DE BENEFICIOS"
            SKIP 1 LINE 

        PRINT
            COLUMN 34, "REPORTE  PARA  CONSAR  ANEXO III  (RETIRO POR TRANSFERENCIA)"
            SKIP 7 LINE
  
        PRINT
            COLUMN 014,"AFORE    : ",c50_razon_social CLIPPED,
            COLUMN 087,"PROGRAMA          :   RETL815"

        PRINT
        PRINT
            COLUMN 014,"CLAVE    : ",cod_afore USING "###", 
            COLUMN 087,"PAGINA            : ",pageno USING "####"
        PRINT
        PRINT 
            COLUMN 087,"FECHA DEL REPORTE : ",HOY USING "DD-MM-YYYY"

    BEFORE GROUP OF reg_1.tipo_retiro

    PRINT '\033e\033(s218T\033(s14H\033(s7B'

    LET vnss_liquidados = 0
    LET vcont_nss       = 0

    DECLARE  cur_4 CURSOR FOR
    SELECT COUNT(UNIQUE nss),fecha_conversion
    FROM   dis_cuenta
    WHERE  fecha_conversion BETWEEN fecha_ini AND  fecha_fin
    AND    tipo_movimiento = reg_1.tipo_retiro
    GROUP BY 2

    FOREACH cur_4 INTO vcont_nss,vfecha_conversion

        LET vnss_liquidados =  vnss_liquidados + vcont_nss

    END FOREACH

    CASE reg_1.tipo_retiro
	WHEN 800
	    LET vtipo_movimiento    = "A"
        WHEN 810
            LET vtipo_movimiento    = "B"
        WHEN 815
            LET vtipo_movimiento = "C"
    END CASE 

    SKIP 4 LINES
    
    PRINT 
        COLUMN 16,"Tipo Retiro : ",vtipo_movimiento

    PRINT 
        COLUMN 15,"\332",L5,L10,L1,L1,L1,L1,
		    "\302",L10,L10,L10,L10,L5,L1,L1,L10,L10,L10,
		    "\302",L10,L5,L10,
		    "\277"

      PRINT 
          COLUMN 15,"|                   |                                   RCV                                       |       VIVIENDA          |"
      PRINT 
          COLUMN 15,"|                   ","\303",L5,L10,L10,"\302",L10,L10,L5,"\302",L10,L10,L5,"\303",L10,L10,L5
      PRINT 
          COLUMN 15,"|    REGISTROS      |            R            |           CV            |           CS            |         VIV 97          |"

    PRINT 
        COLUMN 15,"\300",L5,L10,L1,L1,L1,L1,
          	  "\301",L10,L10,L5,
		  "\301",L10,L10,L5,
		  "\301",L10,L10,L5,
		  "\301",L10,L10,L5,
		  "\331"

    ON EVERY ROW

    LET reg_2.monto_pesos_r      = 0
    LET reg_2.monto_pesos_cv     = 0
    LET reg_2.monto_pesos_cs     = 0
    LET reg_2.monto_pesos_viv97  = 0
    LET vd6_monto_pesos_cv       = 0
    LET vd6_monto_pesos_est      = 0
    LET vd6_monto_pesos_esp      = 0

    DECLARE cur_2 CURSOR FOR
    SELECT nss       ,
           subcuenta ,
           NVL(SUM(monto_en_pesos),0)
    FROM   dis_cuenta 
    WHERE  fecha_conversion BETWEEN fecha_ini AND fecha_fin
    AND    subcuenta        IN(1,4,5)
    AND    tipo_movimiento  = reg_1.tipo_retiro
    GROUP BY 1,2

    FOREACH cur_2 INTO vnss,vsubcuenta,vmonto_en_pesos
        CASE vsubcuenta
            WHEN 1
                LET reg_2.monto_pesos_r     = reg_2.monto_pesos_r +
                                              vmonto_en_pesos
            WHEN 5
                LET reg_2.monto_pesos_cs    = reg_2.monto_pesos_cs +
                                              vmonto_en_pesos
            WHEN 4
                LET reg_2.monto_pesos_viv97 = reg_2.monto_pesos_viv97 +
                                              vmonto_en_pesos
        END CASE 
    END FOREACH 
 
    DECLARE cur_3 CURSOR FOR
    SELECT A.nss,ROUND(SUM(NVL(A.monto_en_pesos,0)),2)
    FROM   dis_cuenta A
    WHERE  A.fecha_conversion BETWEEN fecha_ini AND fecha_fin
    AND    A.subcuenta        IN (2,6,9)
    AND    A.tipo_movimiento  = reg_1.tipo_retiro
    GROUP BY 1

    FOREACH cur_3 INTO vnss,vd2monto_en_pesos
	LET reg_2.monto_pesos_cv = reg_2.monto_pesos_cv +
				   vd2monto_en_pesos
    END FOREACH

    CASE reg_1.tipo_retiro
         WHEN 800
              LET vtipo_movimiento    = "A"
              LET reg_3.Atot_reg      = vnss_liquidados
              LET reg_3.Aimpt_ret_97  = reg_2.monto_pesos_r
              LET reg_3.Aimpt_ces_vej = reg_2.monto_pesos_cv
              LET reg_3.Aimpt_cuo_soc = reg_2.monto_pesos_cs
              LET reg_3.Aimpt_viv_97  = reg_2.monto_pesos_viv97
         WHEN 810
              LET vtipo_movimiento    = "B"
              LET reg_3.Btot_reg      = vnss_liquidados 
              LET reg_3.Bimpt_ces_vej = reg_2.monto_pesos_cv
              LET reg_3.Bimpt_cuo_soc = reg_2.monto_pesos_cs
              LET reg_3.Bimpt_viv_97  = reg_2.monto_pesos_viv97
         WHEN 815
              LET vtipo_movimiento = "C"
              LET reg_3.Ctot_reg      = vnss_liquidados 
              LET reg_3.Cimpt_ret_97  = reg_2.monto_pesos_r
              LET reg_3.Cimpt_ces_vej = reg_2.monto_pesos_cv
              LET reg_3.Cimpt_cuo_soc = reg_2.monto_pesos_cs
              LET reg_3.Cimpt_viv_97  = reg_2.monto_pesos_viv97
    END CASE 
    
    PRINT
    PRINT 
        COLUMN 024, vnss_liquidados         USING "####"         ,
        COLUMN 040, reg_2.monto_pesos_r     USING "#######&.&&"  ,
        COLUMN 066, reg_2.monto_pesos_cv    USING "#######&.&&"  ,
        COLUMN 092, reg_2.monto_pesos_cs    USING "#######&.&&"  ,
        COLUMN 117, reg_2.monto_pesos_viv97 USING "#######&.&&"  
END REPORT

REPORT listado_2(reg3)
#l2------------------ 
    DEFINE reg3 RECORD
        cve_afore             LIKE tab_afore_local.codigo_afore ,
        nom_afore             LIKE tab_afore_local.razon_social ,
        Atot_reg              INTEGER                           ,
        Aimpt_ret_97          DECIMAL(16,2)                     ,
        Aimpt_ces_vej         DECIMAL(16,2)                     ,
        Aimpt_cuo_soc         DECIMAL(16,2)                     ,
        Aimpt_viv_97          DECIMAL(16,2)                     ,
        Btot_reg              INTEGER                           ,
        Bimpt_ret_97          DECIMAL(16,2)                     ,
        Bimpt_ces_vej         DECIMAL(16,2)                     ,
        Bimpt_cuo_soc         DECIMAL(16,2)                     ,
        Bimpt_viv_97          DECIMAL(16,2)                     ,
        Ctot_reg              INTEGER                           ,
        Cimpt_ret_97          DECIMAL(16,2)                     ,
        Cimpt_ces_vej         DECIMAL(16,2)                     ,
        Cimpt_cuo_soc         DECIMAL(16,2)                     ,
        Cimpt_viv_97          DECIMAL(16,2)
    END RECORD
         
    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW
        PRINT 
            COLUMN 001,reg3.cve_afore                         ,"|" ,
                       reg3.nom_afore                         ,"|" ,
                       reg3.Atot_reg                          ,"|" ,
		       reg3.Aimpt_ret_97  USING "#######&.&&" ,"|" ,
                       reg3.Aimpt_ces_vej USING "#######&.&&" ,"|" ,
                       reg3.Aimpt_cuo_soc USING "#######&.&&" ,"|" ,
                       reg3.Aimpt_viv_97  USING "#######&.&&" ,"|" ,
                       reg3.Btot_reg                          ,"|" ,
                       reg3.Bimpt_ces_vej USING "#######&.&&" ,"|" ,
                       reg3.Bimpt_cuo_soc USING "#######&.&&" ,"|" ,
                       reg3.Bimpt_viv_97  USING "#######&.&&" ,"|" ,
                       reg3.Ctot_reg                          ,"|" ,
                       reg3.Cimpt_ret_97  USING "#######&.&&" ,"|" ,
                       reg3.Cimpt_ces_vej USING "#######&.&&" ,"|" ,
                       reg3.Cimpt_cuo_soc USING "#######&.&&" ,"|" ,
                       reg3.Cimpt_viv_97  USING "#######&.&&" ,"|"
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
