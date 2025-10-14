################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )                            #
#Propietario       => E.F.P.                                                   #
#Programa RETL813  => Reporte para CONSAR anexo 1 (Retiro por Disposicion)     #
#Fecha             => 04 DE JUNIO DE 2004                                      #
#Por               => STEFANIE DANIELA VERA PIÑA                               #
#Sistema           => RET                                                      #
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE #glo #g_param 
        g_param  RECORD LIKE seg_modulo.*

    DEFINE reg1 RECORD #loc #reg1
        tipo_retiro        INTEGER
    END RECORD

    DEFINE reg4 RECORD #loc #reg4
        cve_afore            LIKE tab_afore_local.codigo_afore,
        nom_afore            LIKE tab_afore_local.razon_social, 
        tipo_retiro_d                                         ,
        tipo_retiro_e                                         ,
        tipo_retiro_f                                         ,
        tipo_retiro_g                                         ,
        tipo_retiro_h                                         ,
        tipo_retiro_j        CHAR(1)                          ,
        num_reg_ret97                                         ,
        nss_liquidados_d                                      ,
        nss_liquidados_e                                      ,
        nss_liquidados_f                                      ,
        nss_liquidados_g                                      ,
        nss_liquidados_h                                      ,
        nss_liquidados_j     INTEGER                          ,
        monto_pesos_ret97_d                                   ,
        monto_pesos_ret97_e                                   ,
        monto_pesos_ret97_f                                   ,
        monto_pesos_ret97_g                                   ,
        monto_pesos_ret97_h                                   ,
        monto_pesos_ret97_j                                   ,
        monto_pesos_cv_d                                      ,
        monto_pesos_cv_e                                      ,
        monto_pesos_cv_f                                      ,
        monto_pesos_cv_g                                      ,
        monto_pesos_cv_h                                      ,
        monto_pesos_cv_j                                      ,
        monto_pesos_so_d                                      ,
        monto_pesos_so_e                                      ,
        monto_pesos_so_f                                      ,
        monto_pesos_so_g                                      ,
        monto_pesos_so_h                                      ,
        monto_pesos_so_j                                      ,
        monto_pesos_ret92_d                                   , 
        monto_pesos_ret92_e                                   , 
        monto_pesos_ret92_f                                   , 
        monto_pesos_ret92_g                                   , 
        monto_pesos_ret92_h                                   , 
        monto_pesos_ret92_j                                   , 
        monto_pesos_v97_d                                     ,
        monto_pesos_v97_e                                     ,
        monto_pesos_v97_f                                     ,
        monto_pesos_v97_g                                     ,
        monto_pesos_v97_h                                     ,
        monto_pesos_v97_j                                     ,
        monto_pesos_v92_d                                     ,      
        monto_pesos_v92_e                                     ,      
        monto_pesos_v92_f                                     ,      
        monto_pesos_v92_g                                     ,      
        monto_pesos_v92_h                                     ,      
        monto_pesos_v92_j                                     ,      
        monto_pesos_v72_d                                     ,
        monto_pesos_v72_e                                     ,
        monto_pesos_v72_f                                     ,   
        monto_pesos_v72_g                                     ,
        monto_pesos_v72_h                                     ,   
        monto_pesos_v72_j    DECIMAL(10,2)
    END RECORD

    DEFINE #glo #char 	
        ch                 CHAR(0110) ,
        COMANDO            CHAR(0400) ,
        c10_fecha_corte    CHAR(0010) ,
        descripcion        CHAR(0050) ,
        dia_fecha_corte    CHAR(0002) ,
        enter              CHAR(0001) ,
        G_LISTA            CHAR(0100) ,
        G_LISTA2           CHAR(0100) ,
        impresion          CHAR(0300) ,
        mes_fecha_corte    CHAR(0002) ,
        nom_plano          CHAR(0030) ,
        nom_reporte        CHAR(0030) ,
        L1                 CHAR(0001) ,
        L5                 CHAR(0005) ,
        L10                CHAR(0010) 

    DEFINE #glo #integer 	
        vnss_liquidados    INTEGER

    DEFINE #glo #date     
        HOY                ,
        HOY2               ,
        fecha_corte        , 
        fecha_ini          ,
	vfecha_conversion  ,
        vfecha_fin         DATE   

    DEFINE #glo #smallint
        cod_afore          ,
	vcont_nss          SMALLINT

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS PROMPT LINE LAST,
	INPUT WRAP

    CALL init() #i
    OPEN WINDOW retl8131 AT 4,4 WITH FORM "RETL8131" ATTRIBUTE (BORDER)
    DISPLAY "                               < CTRL-C >                                      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETL813       REPORTE MENSUAL DE DISPOSICION DE RECURSOS                    " AT 3,1 ATTRIBUTE (REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE (REVERSE)

    DISPLAY BY NAME fecha_ini

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

            DISPLAY BY NAME fecha_ini  

{svera
        ON KEY (ESC)
            IF fecha_corte  IS NULL THEN
                ERROR "    LA FECHA NO PUEDE SER NULA" ATTRIBUTE(NORMAL)
                NEXT FIELD fecha_corte
            END IF

            LET c10_fecha_corte = fecha_corte
            LET c10_fecha_corte = c10_fecha_corte[01,02],
                                  "/01/",c10_fecha_corte[07,10]

            LET fecha_ini = c10_fecha_corte
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
                LET nom_plano   = fecha_corte USING "YYYYMM","D",".txt"
                LET nom_reporte = fecha_corte USING "YYYYMM","D",".lst"
                EXIT WHILE
            ELSE
                PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR "
                FOR CHAR enter
                EXIT PROGRAM
           END IF
        END IF
    END WHILE

    DISPLAY " PROCESANDO INFORMACION " AT 20,1 ATTRIBUTE(REVERSE)

    CALL primer_paso()  #pp GENERA REPORTE

    DISPLAY "ARCHIVO GENERADO EN LA RUTA "  AT  9,25
    DISPLAY g_param.ruta_listados           AT 10,25

    DISPLAY "NOMBRE DEL ARCHIVO PLANO     " AT 12,25
    DISPLAY nom_plano                       AT 13,25

{svera
    DISPLAY "NOMBRE DEL ARCHIVO REPORTE   " AT 15,25
    DISPLAY nom_reporte                     AT 16,25
svera}

--svera    PROMPT " DESEA GENERAR IMPRESION S/N ? " FOR CHAR enter
    PROMPT " PROCESO FINALIZADO ... <ENTER> PARA SALIR " FOR CHAR enter

    CLOSE WINDOW retl8131

{svera
    LET COMANDO = "chmod 777 ",g_param.ruta_listados CLIPPED,"/",nom_reporte
    RUN COMANDO

    IF enter matches "[Ss]" THEN
       LET impresion = "lp ",G_LISTA
       RUN impresion
    END IF
svera}
END MAIN


FUNCTION init()
#i-------------
    INITIALIZE reg1.* TO NULL
    INITIALIZE reg4.* TO NULL 

    LET reg4.num_reg_ret97          = 0        
    LET reg4.nss_liquidados_d       = 0      
    LET reg4.nss_liquidados_e       = 0    
    LET reg4.nss_liquidados_f       = 0   
    LET reg4.nss_liquidados_g       = 0    
    LET reg4.nss_liquidados_h       = 0     
    LET reg4.nss_liquidados_j       = 0 
    LET reg4.monto_pesos_ret97_d    = 0      
    LET reg4.monto_pesos_ret97_e    = 0
    LET reg4.monto_pesos_ret97_f    = 0
    LET reg4.monto_pesos_ret97_g    = 0
    LET reg4.monto_pesos_ret97_h    = 0
    LET reg4.monto_pesos_ret97_j    = 0
    LET reg4.monto_pesos_cv_d       = 0
    LET reg4.monto_pesos_cv_e       = 0
    LET reg4.monto_pesos_cv_f       = 0  
    LET reg4.monto_pesos_cv_g       = 0
    LET reg4.monto_pesos_cv_h       = 0
    LET reg4.monto_pesos_cv_j       = 0
    LET reg4.monto_pesos_so_d       = 0
    LET reg4.monto_pesos_so_e       = 0                    
    LET reg4.monto_pesos_so_f       = 0                     
    LET reg4.monto_pesos_so_g       = 0           
    LET reg4.monto_pesos_so_h       = 0   
    LET reg4.monto_pesos_so_j       = 0
    LET reg4.monto_pesos_ret92_d    = 0
    LET reg4.monto_pesos_ret92_e    = 0
    LET reg4.monto_pesos_ret92_f    = 0
    LET reg4.monto_pesos_ret92_g    = 0
    LET reg4.monto_pesos_ret92_h    = 0
    LET reg4.monto_pesos_ret92_j    = 0             
    LET reg4.monto_pesos_v97_d      = 0
    LET reg4.monto_pesos_v97_e      = 0
    LET reg4.monto_pesos_v97_f      = 0
    LET reg4.monto_pesos_v97_g      = 0
    LET reg4.monto_pesos_v97_h      = 0
    LET reg4.monto_pesos_v97_j      = 0
    LET reg4.monto_pesos_v92_d      = 0
    LET reg4.monto_pesos_v92_e      = 0     
    LET reg4.monto_pesos_v92_f      = 0
    LET reg4.monto_pesos_v92_g      = 0
    LET reg4.monto_pesos_v92_h      = 0
    LET reg4.monto_pesos_v92_j      = 0
    LET reg4.monto_pesos_v72_d      = 0
    LET reg4.monto_pesos_v72_e      = 0     
    LET reg4.monto_pesos_v72_f      = 0
    LET reg4.monto_pesos_v72_g      = 0         
    LET reg4.monto_pesos_v72_h      = 0
    LET reg4.monto_pesos_v72_j      = 0

    CALL STARTLOG("RETL813.log")    

    LET HOY         = TODAY

    SELECT ruta_listados
    INTO   g_param.ruta_listados
    FROM   seg_modulo
    WHERE  modulo_cod='ret'

    SELECT codigo_afore ,
           razon_social
    INTO   cod_afore    ,
           descripcion
    FROM   tab_afore_local

    LET c10_fecha_corte = HOY USING "MMDDYYYY"

    LET mes_fecha_corte = c10_fecha_corte[01,02]

    CALL num_dias_mes(mes_fecha_corte)
    RETURNING dia_fecha_corte

    LET c10_fecha_corte = mes_fecha_corte ,
                          dia_fecha_corte ,
                          c10_fecha_corte[05,08]

    LET fecha_corte = c10_fecha_corte

    LET c10_fecha_corte = fecha_corte
    LET c10_fecha_corte = c10_fecha_corte[01,02],
                          "/01/",c10_fecha_corte[07,10]

    LET fecha_ini = c10_fecha_corte

    LET L1  = "\304"
    LET L5  = "\304\304\304\304\304"
    LET L10 = "\304\304\304\304\304\304\304\304\304\304"

END FUNCTION

FUNCTION primer_paso()
#pp-------------------

    ----- REPORTE -----
--svera    LET G_LISTA = g_param.ruta_listados CLIPPED,"/",nom_reporte
    LET G_LISTA2 = g_param.ruta_listados CLIPPED,"/",nom_plano  

    START REPORT listado_1 TO G_LISTA2
    DECLARE cur_1 CURSOR FOR
    SELECT  tipo_movimiento
    FROM    dis_cuenta
    WHERE   fecha_conversion BETWEEN fecha_ini AND fecha_corte
    AND     tipo_movimiento  IN (820,830,840,850,860,880)
    GROUP BY 1
    ORDER BY 1

    FOREACH cur_1 INTO reg1.*
        OUTPUT TO REPORT listado_1(reg1.*)
    END FOREACH
    FINISH REPORT listado_1

    ----- ARCHIVO PLANO -----  
--svera    LET G_LISTA2 = g_param.ruta_listados CLIPPED,"/",nom_plano

    LET reg4.cve_afore = cod_afore
    LET reg4.nom_afore = descripcion
    LET vcont_nss      = 0

    DECLARE cur_5 CURSOR FOR
    SELECT COUNT(UNIQUE nss),fecha_conversion
    FROM   dis_cuenta
    WHERE  subcuenta        = 1
    AND    tipo_movimiento  = 830
    AND    fecha_conversion BETWEEN fecha_ini AND fecha_corte
    GROUP BY 2

    FOREACH cur_5 INTO vcont_nss,vfecha_conversion

	LET reg4.num_reg_ret97 = reg4.num_reg_ret97 + vcont_nss

    END FOREACH

    START REPORT listado_2 TO G_LISTA2
    OUTPUT TO REPORT listado_2(reg4.*)

    LET ch = "chmod 777 ",g_param.ruta_listados CLIPPED,"/",nom_plano
    run ch

    FINISH REPORT listado_2
END FUNCTION

REPORT listado_1(reg1)
#l1-------------------
    DEFINE reg1 RECORD #loc #reg1
        tipo_retiro         INTEGER 
    END RECORD

    DEFINE reg2 RECORD #loc #reg2
        monto_pesos_ret97   ,
        monto_pesos_est     ,
        monto_pesos_esp     ,
        monto_pesos_cv      ,
        monto_pesos_so      ,
        monto_pesos_ret92   ,
        monto_pesos_v97     ,
        monto_pesos_v92     , 
        monto_pesos_v72     DECIMAL(10,2)
    END RECORD

    DEFINE #loc #char
        vnss                CHAR(11) ,
        vtipo_retiro        CHAR(01)

    DEFINE #loc #smallint
        vsiefore            ,
        vsubcuenta          SMALLINT

    DEFINE #loc #decimal
        vd6_monto_pesos_cv   LIKE dis_cuenta.monto_en_pesos ,
        vd6_monto_pesos_est  LIKE dis_cuenta.monto_en_pesos ,
        vd6_monto_pesos_esp  LIKE dis_cuenta.monto_en_pesos ,
        vmonto_en_pesos      DECIMAL(16,6) ,
        vd2monto_en_pesos    DECIMAL(10,2)

    OUTPUT
        PAGE LENGTH   90
        LEFT MARGIN    0
        RIGHT MARGIN 150
        TOP MARGIN     0
        BOTTOM MARGIN  0

    FORMAT
    PAGE HEADER
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'

        PRINT COLUMN 1, '\033e\033(s218T\033(s11H\033(s7B'

        PRINT COLUMN 50, "SUBDIRECCION DE BENEFICIOS"

        SKIP 1 LINES
        PRINT COLUMN 37, "REPORTE  PARA CONSAR ANEXO 1 (RETIRO POR DISPOSICION)"

        SKIP 1 LINES
        PRINT COLUMN 1,'\033e\033(s218T\033(s12H\033(s7N'

        PRINT COLUMN 004,"AFORE  : ",descripcion CLIPPED,
              COLUMN 103,"PROGRAMA :   RETL813"

        SKIP 1 LINES
        PRINT COLUMN 004,"CLAVE  : ",cod_afore USING "###",
              COLUMN 103,"PAGINA   :",pageno USING "##########"

        SKIP 1 LINES
        PRINT COLUMN 103,"FECHA    :",HOY USING "DD-MM-YYYY"

     BEFORE GROUP OF reg1.tipo_retiro

	 LET vnss_liquidados = 0
	 LET vcont_nss       = 0

	 DECLARE cur_6 CURSOR FOR
         SELECT COUNT(UNIQUE nss),fecha_conversion
         FROM   dis_cuenta
         WHERE  fecha_conversion BETWEEN fecha_ini AND fecha_corte
         AND    tipo_movimiento = reg1.tipo_retiro
         GROUP BY 2

	 FOREACH cur_6 INTO vcont_nss,vfecha_conversion

	     LET vnss_liquidados  = vnss_liquidados +  vcont_nss

	 END FOREACH
  
         CASE reg1.tipo_retiro
              WHEN 820
                  LET vtipo_retiro = "D"
              WHEN 830
                  LET vtipo_retiro = "E"
              WHEN 840
                  LET vtipo_retiro = "F"
              WHEN 850
                  LET vtipo_retiro = "G"
              WHEN 860
                  LET vtipo_retiro = "H"
              WHEN 880
                  LET vtipo_retiro = "J"
         END CASE

         SKIP 5 LINES
         PRINT '\033e\033(s218T\033(s14H\033(s7B'

         PRINT COLUMN 05,"TIPO RETIRO :  ",vtipo_retiro

         PRINT COLUMN 04,"\332",L10,L5,
                         "\302",L10,L1,L1,L5,
                         "\302",L10,L1,L1,L5,
                         "\302",L10,L1,L1,L5,
                         "\302",L10,L1,L1,L5,
                         "\302",L10,L1,L1,L5,
                         "\302",L10,L1,L1,L5,
                         "\302",L10,L1,L1,L5,
                         "\277"

         PRINT COLUMN 04,"|               |",
                         "                 |",
                         "                 |",
                         "                 |",
                         "                 |",
                         "                 |",
                         "                 |",
                         "                 |"

         PRINT COLUMN 04,"|TRAB.LIQUIDADOS|",
                         "    RETIRO 97    |",
                         "       CV        |",
                         "       CS        |",
                         "    RETIRO 92    |",
                         "   VIVIENDA 97   |",
                         "   VIVIENDA 92   |",
                         "   VIVIENDA 72   |"

         PRINT COLUMN 04,"|               |",
                         "                 |",
                         "                 |",
                         "                 |",
                         "                 |",
                         "                 |",
                         "                 |",
                         "                 |",
                         "                 |"

         PRINT COLUMN 04,"\300",L10,L5,
                         "\301",L10,L1,L1,L5,
                         "\301",L10,L1,L1,L5,
                         "\301",L10,L1,L1,L5,
                         "\301",L10,L1,L1,L5,
                         "\301",L10,L1,L1,L5,
                         "\301",L10,L1,L1,L5,
                         "\301",L10,L1,L1,L5,
                         "\331"
     ON EVERY ROW

         LET reg2.monto_pesos_ret97   = 0
         LET reg2.monto_pesos_cv      = 0
         LET reg2.monto_pesos_so      = 0
         LET reg2.monto_pesos_ret92   = 0
         LET reg2.monto_pesos_v97     = 0
         LET reg2.monto_pesos_v92     = 0
         LET reg2.monto_pesos_v72     = 0
         LET vd6_monto_pesos_cv       = 0
         LET vd6_monto_pesos_est      = 0
         LET vd6_monto_pesos_esp      = 0

         DECLARE cur_2 CURSOR FOR
         SELECT A.nss,A.subcuenta,A.siefore,SUM(NVL(A.monto_en_pesos,0)+
                                                NVL(B.monto_en_pesos,0))
         FROM   dis_cuenta A, OUTER  dis_cuenta B
         WHERE  A.fecha_conversion BETWEEN fecha_ini AND fecha_corte
         AND    A.subcuenta        IN (1,5,7)
         AND    A.tipo_movimiento  = reg1.tipo_retiro
         AND    A.folio            = B.folio
         AND    A.subcuenta        = B.subcuenta
         AND    A.siefore          = B.siefore   
         AND    B.tipo_movimiento  = 10
         AND    A.nss              = B.nss
         AND    A.consecutivo_lote = B.consecutivo_lote
         GROUP BY 1,2,3

         FOREACH cur_2 INTO vnss,vsubcuenta,vsiefore,vmonto_en_pesos
             CASE vsubcuenta
                 WHEN 1
                     LET reg2.monto_pesos_ret97 = reg2.monto_pesos_ret97 +
                                                  vmonto_en_pesos  
                 WHEN 5
                     LET reg2.monto_pesos_so    = reg2.monto_pesos_so +
                                                  vmonto_en_pesos
                 WHEN 7
                     LET reg2.monto_pesos_ret92 = reg2.monto_pesos_ret92 +
                                                  vmonto_en_pesos
             END CASE 

         END FOREACH 
	 
         DECLARE cur_3 CURSOR FOR
         SELECT A.nss,A.siefore,ROUND(SUM(NVL(A.monto_en_pesos,0)+
                                          NVL(B.monto_en_pesos,0)),2)
         FROM   dis_cuenta A, OUTER  dis_cuenta B
         WHERE  A.fecha_conversion BETWEEN fecha_ini AND fecha_corte
         AND    A.subcuenta        IN (2,6,9) 
         AND    A.tipo_movimiento  = reg1.tipo_retiro
         AND    A.folio            = B.folio
         AND    A.subcuenta        = B.subcuenta
         AND    A.siefore          = B.siefore   
         AND    B.tipo_movimiento  = 10
         AND    A.nss              = B.nss
         AND    A.consecutivo_lote = B.consecutivo_lote
         GROUP BY 1,2

         FOREACH cur_3 INTO vnss,vsiefore,vd2monto_en_pesos

             LET reg2.monto_pesos_cv = reg2.monto_pesos_cv +
				       vd2monto_en_pesos
				       
         END FOREACH 

         DECLARE cur_4 CURSOR FOR
         SELECT A.nss,A.subcuenta,NVL(SUM(A.monto_en_pesos),0)
         FROM   dis_cuenta A
         WHERE  A.fecha_conversion BETWEEN fecha_ini AND fecha_corte
         AND    A.subcuenta        IN (4,8)
         AND    A.tipo_movimiento  = reg1.tipo_retiro
         GROUP BY 1,2

         FOREACH cur_4 INTO vnss,vsubcuenta,vmonto_en_pesos
             CASE vsubcuenta
                 WHEN 4
                     LET reg2.monto_pesos_v97 = reg2.monto_pesos_v97 + 
                                                vmonto_en_pesos
                 WHEN 8
                     LET reg2.monto_pesos_v92 = reg2.monto_pesos_v92 +
                                                vmonto_en_pesos
             END CASE 
         END FOREACH 

         LET reg2.monto_pesos_v72 = 0   

         CASE reg1.tipo_retiro
              WHEN 820
                  LET reg4.tipo_retiro_d        = "D"
                  LET reg4.nss_liquidados_d     = vnss_liquidados
                  LET reg4.monto_pesos_ret97_d  = reg2.monto_pesos_ret97 
                  LET reg4.monto_pesos_cv_d     = reg2.monto_pesos_cv
                  LET reg4.monto_pesos_so_d     = reg2.monto_pesos_so
                  LET reg4.monto_pesos_ret92_d  = reg2.monto_pesos_ret92
                  LET reg4.monto_pesos_v97_d    = reg2.monto_pesos_v97
                  LET reg4.monto_pesos_v92_d    = reg2.monto_pesos_v92
                  LET reg4.monto_pesos_v72_d    = reg2.monto_pesos_v72
              WHEN 830
                  LET reg4.tipo_retiro_e        = "E"
                  LET reg4.nss_liquidados_e     = vnss_liquidados
                  LET reg4.monto_pesos_ret97_e  = reg2.monto_pesos_ret97
                  LET reg4.monto_pesos_cv_e     = reg2.monto_pesos_cv
                  LET reg4.monto_pesos_so_e     = reg2.monto_pesos_so
                  LET reg4.monto_pesos_ret92_e  = reg2.monto_pesos_ret92
                  LET reg4.monto_pesos_v97_e    = reg2.monto_pesos_v97
                  LET reg4.monto_pesos_v92_e    = reg2.monto_pesos_v92
                  LET reg4.monto_pesos_v72_e    = reg2.monto_pesos_v72
              WHEN 840
                  LET reg4.tipo_retiro_f        = "F"
                  LET reg4.nss_liquidados_f     = vnss_liquidados
                  LET reg4.monto_pesos_ret97_f  = reg2.monto_pesos_ret97
                  LET reg4.monto_pesos_cv_f     = reg2.monto_pesos_cv
                  LET reg4.monto_pesos_so_f     = reg2.monto_pesos_so
                  LET reg4.monto_pesos_ret92_f  = reg2.monto_pesos_ret92
                  LET reg4.monto_pesos_v97_f    = reg2.monto_pesos_v97
                  LET reg4.monto_pesos_v92_f    = reg2.monto_pesos_v92
                  LET reg4.monto_pesos_v72_f    = reg2.monto_pesos_v72
              WHEN 850
                  LET reg4.tipo_retiro_g        = "G"
                  LET reg4.nss_liquidados_g     = vnss_liquidados
                  LET reg4.monto_pesos_ret97_g  = reg2.monto_pesos_ret97
                  LET reg4.monto_pesos_cv_g     = reg2.monto_pesos_cv
                  LET reg4.monto_pesos_so_g     = reg2.monto_pesos_so
                  LET reg4.monto_pesos_ret92_g  = reg2.monto_pesos_ret92
                  LET reg4.monto_pesos_v97_g    = reg2.monto_pesos_v97
                  LET reg4.monto_pesos_v92_g    = reg2.monto_pesos_v92
                  LET reg4.monto_pesos_v72_g    = reg2.monto_pesos_v72
              WHEN 860
                  LET reg4.tipo_retiro_h        = "H"
                  LET reg4.nss_liquidados_h     = vnss_liquidados
                  LET reg4.monto_pesos_ret97_h  = reg2.monto_pesos_ret97
                  LET reg4.monto_pesos_cv_h     = reg2.monto_pesos_cv
                  LET reg4.monto_pesos_so_h     = reg2.monto_pesos_so
                  LET reg4.monto_pesos_ret92_h  = reg2.monto_pesos_ret92
                  LET reg4.monto_pesos_v97_h    = reg2.monto_pesos_v97
                  LET reg4.monto_pesos_v92_h    = reg2.monto_pesos_v92
                  LET reg4.monto_pesos_v72_h    = reg2.monto_pesos_v72
              WHEN 880
                  LET reg4.tipo_retiro_j        = "J"
                  LET reg4.nss_liquidados_j     = vnss_liquidados
                  LET reg4.monto_pesos_ret97_j  = reg2.monto_pesos_ret97
                  LET reg4.monto_pesos_cv_j     = reg2.monto_pesos_cv
                  LET reg4.monto_pesos_so_j     = reg2.monto_pesos_so
                  LET reg4.monto_pesos_ret92_j  = reg2.monto_pesos_ret92
                  LET reg4.monto_pesos_v97_j    = reg2.monto_pesos_v97
                  LET reg4.monto_pesos_v92_j    = reg2.monto_pesos_v92
                  LET reg4.monto_pesos_v72_j    = reg2.monto_pesos_v72
         END CASE

         SKIP 1 LINES
         PRINT COLUMN 008, vnss_liquidados        USING "####"         ,
               COLUMN 021, reg2.monto_pesos_ret97 USING "#######&.&&"  ,
               COLUMN 039, reg2.monto_pesos_cv    USING "#######&.&&"  ,
               COLUMN 057, reg2.monto_pesos_so    USING "#######&.&&"  ,
               COLUMN 075, reg2.monto_pesos_ret92 USING "#######&.&&"  ,
               COLUMN 093, reg2.monto_pesos_v97   USING "#######&.&&"  , 
               COLUMN 111, reg2.monto_pesos_v92   USING "#######&.&&"  ,
               COLUMN 129, reg2.monto_pesos_v72   USING "#######&.&&"   

         IF vtipo_retiro = "H" THEN
            SKIP TO TOP OF PAGE
         END IF
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

REPORT listado_2(reg5)
#l2-------------------
    DEFINE reg5 RECORD #loc #reg5
        cve_afore            LIKE tab_afore_local.codigo_afore ,
        nom_afore            LIKE tab_afore_local.razon_social ,
        tipo_retiro_d                                          ,
        tipo_retiro_e                                          ,
        tipo_retiro_f                                          ,
        tipo_retiro_g                                          ,
        tipo_retiro_h                                          ,
        tipo_retiro_j        CHAR(1)                           ,
        num_reg_ret97                                          ,
        nss_liquidados_d                                       ,
        nss_liquidados_e                                       ,
        nss_liquidados_f                                       ,
        nss_liquidados_g                                       ,
        nss_liquidados_h                                       ,
        nss_liquidados_j     INTEGER                           ,
        monto_pesos_ret97_d                                    ,
        monto_pesos_ret97_e                                    ,
        monto_pesos_ret97_f                                    ,
        monto_pesos_ret97_g                                    ,
        monto_pesos_ret97_h                                    ,
        monto_pesos_ret97_j                                    ,
        monto_pesos_cv_d                                       ,
        monto_pesos_cv_e                                       ,
        monto_pesos_cv_f                                       ,
        monto_pesos_cv_g                                       ,
        monto_pesos_cv_h                                       ,
        monto_pesos_cv_j                                       ,
        monto_pesos_so_d                                       ,
        monto_pesos_so_e                                       ,
        monto_pesos_so_f                                       ,
        monto_pesos_so_g                                       ,
        monto_pesos_so_h                                       ,
        monto_pesos_so_j                                       ,
        monto_pesos_ret92_d                                    , 
        monto_pesos_ret92_e                                    , 
        monto_pesos_ret92_f                                    , 
        monto_pesos_ret92_g                                    , 
        monto_pesos_ret92_h                                    , 
        monto_pesos_ret92_j                                    , 
        monto_pesos_v97_d                                      ,
        monto_pesos_v97_e                                      ,
        monto_pesos_v97_f                                      ,
        monto_pesos_v97_g                                      ,
        monto_pesos_v97_h                                      ,
        monto_pesos_v97_j                                      ,
        monto_pesos_v92_d                                      ,      
        monto_pesos_v92_e                                      ,      
        monto_pesos_v92_f                                      ,      
        monto_pesos_v92_g                                      ,      
        monto_pesos_v92_h                                      ,      
        monto_pesos_v92_j                                      ,      
        monto_pesos_v72_d                                      ,
        monto_pesos_v72_e                                      ,
        monto_pesos_v72_f                                      ,   
        monto_pesos_v72_g                                      ,
        monto_pesos_v72_h                                      ,   
        monto_pesos_v72_j    DECIMAL(10,2)
    END RECORD

    DEFINE #loc #char
        v2tipo_retiro        CHAR(1)

    OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW
        PRINT 
            COLUMN 001, reg5.cve_afore                               ,"|" ,
                        reg5.nom_afore                               ,"|" ,
                        reg5.nss_liquidados_d                        ,"|" ,
                        reg5.monto_pesos_ret97_d USING "#######&.&&" ,"|" ,
                        reg5.monto_pesos_cv_d    USING "#######&.&&" ,"|" ,
                        reg5.monto_pesos_so_d    USING "#######&.&&" ,"|" ,
                        reg5.monto_pesos_ret92_d USING "#######&.&&" ,"|" ,
                        reg5.monto_pesos_v97_d   USING "#######&.&&" ,"|" ,
                        reg5.monto_pesos_v92_d   USING "#######&.&&" ,"|" ,
                        reg5.monto_pesos_v72_d   USING "#######&.&&" ,"|" ,
                        reg5.nss_liquidados_e                        ,"|" ,
                        reg5.monto_pesos_ret97_e USING "#######&.&&" ,"|" ,
                        reg5.monto_pesos_cv_e    USING "#######&.&&" ,"|" ,
                        reg5.monto_pesos_so_e    USING "#######&.&&" ,"|" ,
                        reg5.monto_pesos_ret92_e USING "#######&.&&" ,"|" ,
                        reg5.monto_pesos_v97_e   USING "#######&.&&" ,"|" ,
                        reg5.monto_pesos_v92_e   USING "#######&.&&" ,"|" ,
                        reg5.monto_pesos_v72_e   USING "#######&.&&" ,"|" ,
                        reg5.nss_liquidados_f                        ,"|" ,
                        reg5.monto_pesos_ret97_f USING "#######&.&&" ,"|" ,
                        reg5.monto_pesos_cv_f    USING "#######&.&&" ,"|" ,
                        reg5.monto_pesos_so_f    USING "#######&.&&" ,"|" ,
                        reg5.monto_pesos_ret92_f USING "#######&.&&" ,"|" ,
                        reg5.monto_pesos_v97_f   USING "#######&.&&" ,"|" ,
                        reg5.monto_pesos_v92_f   USING "#######&.&&" ,"|" ,
                        reg5.monto_pesos_v72_f   USING "#######&.&&" ,"|" ,
                        reg5.nss_liquidados_g                        ,"|" ,
                        reg5.monto_pesos_ret97_g USING "#######&.&&" ,"|" ,
                        reg5.monto_pesos_cv_g    USING "#######&.&&" ,"|" ,
                        reg5.monto_pesos_so_g    USING "#######&.&&" ,"|" ,
                        reg5.monto_pesos_ret92_g USING "#######&.&&" ,"|" ,
                        reg5.monto_pesos_v97_g   USING "#######&.&&" ,"|" ,
                        reg5.monto_pesos_v92_g   USING "#######&.&&" ,"|" ,
                        reg5.nss_liquidados_h                        ,"|" ,
                        reg5.monto_pesos_ret97_h USING "#######&.&&" ,"|" ,
                        reg5.monto_pesos_cv_h    USING "#######&.&&" ,"|" ,
                        reg5.monto_pesos_so_h    USING "#######&.&&" ,"|" ,
                        reg5.monto_pesos_ret92_h USING "#######&.&&" ,"|" ,
                        reg5.monto_pesos_v97_h   USING "#######&.&&" ,"|" ,
                        reg5.monto_pesos_v92_h   USING "#######&.&&" ,"|" ,
                        reg5.nss_liquidados_j                        ,"|" ,
                        reg5.monto_pesos_ret97_j USING "#######&.&&" ,"|" ,
                        reg5.monto_pesos_cv_j    USING "#######&.&&" ,"|" ,
                        reg5.monto_pesos_so_j    USING "#######&.&&" ,"|" ,
                        reg5.monto_pesos_ret92_j USING "#######&.&&" ,"|" ,
                        reg5.monto_pesos_v97_j   USING "#######&.&&" ,"|" ,
                        reg5.monto_pesos_v92_j   USING "#######&.&&" ,"|" ,
                        reg5.num_reg_ret97                           ,"|" 
END REPORT
