################################################################################
#Proyecto          => SISTEMA DE AFORES.( SAFRE )                              #
#Owner             => E. F. P.                                                 #
#Programa RETL802  => REPORTE DE RETIRO PARCIAL PREVIO A LA OPER.16(PRES.7)    #
#Por               => MIGUEL ANGEL HERNANDEZ MARTINEZ                          #
#Fecha             => 04 DE MAYO DE 2000                                       #
#Actualiza         => JUAN CARLOS MENDOZA MORENO                               #
#Fecha actualiza   => 29 DE NOVIEMBRE DE 2004                                  #
#Sistema           => RET                                                      #
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE 
        w_codigo_afore     LIKE tab_afore_local.codigo_afore 

    DEFINE #glo #w_tabafore #w_paramgrales
        w_paramgrales         RECORD LIKE seg_modulo.*,
        w_tabafore            RECORD LIKE tab_afore_local.*,
        g_lp_impresoras       RECORD LIKE tab_cmd_impresora.*

    DEFINE reg_1 RECORD #glo #reg_1
        folio_oper12          INTEGER
    END RECORD

    DEFINE 
        HOY                   DATE
        
    DEFINE #glo #char
        nombre_siefore        CHAR(003) ,
        nom_impresora         CHAR(020) ,
        G_LISTA               CHAR(100) ,
        usuario               CHAR(008) ,
        enter                 CHAR(001)

    DEFINE
        precio_accion         DECIMAL(16,6)      

    DEFINE
        sw_1                  SMALLINT,
        cont_reg_av           INTEGER

    DEFINE #loc #
        vprecio_sb1           ,
        vprecio_sb2           ,
        vprecio_sb3           ,
        vprecio_sb4           ,
        vprecio_sb5           DECIMAL(16,6)
        
    DEFINE #loc #date
        vfecha_genera         DATE
        

END GLOBALS


MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        MESSAGE LINE LAST    ,
        ACCEPT KEY CONTROL-I

    CALL init() #i
    CALL STARTLOG("RETL802.log")
    OPEN WINDOW retl8021 AT 4,4 WITH FORM "RETL8021" ATTRIBUTE (BORDER)
    DISPLAY "                               < Ctrl-C >                                      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETL802   REPORTE, PROVISION RETIRO PARCIAL POR MATRIMONIO                    " AT 3,1 ATTRIBUTE (REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE (REVERSE)

    INPUT BY NAME reg_1.folio_oper12 WITHOUT DEFAULTS
        AFTER FIELD folio_oper12                                   
            IF reg_1.folio_oper12 IS NULL THEN                         
                ERROR "  EL FOLIO NO PUEDE SER NULO  " ATTRIBUTE(NORMAL)
                NEXT FIELD folio_oper12                                
            END IF

            SELECT 'OK'
            FROM ret_parcial 
            WHERE @folio = reg_1.folio_oper12
            GROUP BY 1
            IF STATUS = NOTFOUND THEN
                MESSAGE "  NO EXISTE INFORMACION PARA ESTE FOLIO  "
                ATTRIBUTE(REVERSE)
                SLEEP 2
                MESSAGE "" 
                NEXT FIELD folio_oper12
            END IF

        ON KEY (ESC)
            IF reg_1.folio_oper12 IS NULL THEN                         
                ERROR "  EL FOLIO NO PUEDE SER NULO " ATTRIBUTE(NORMAL)
                NEXT FIELD folio_oper12                                
            ELSE
                  EXIT INPUT
            END IF

        ON KEY (CONTROL-C)
            PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
            EXIT PROGRAM

        ON KEY (INTERRUPT)
            PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR enter
            EXIT PROGRAM
    END INPUT

    DISPLAY " PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)
    CALL primer_paso()  #pp
    CALL segundo_paso() #sp
    CLOSE WINDOW retl8021
    CALL tercer_paso()  #tp
    PROMPT " PROCESO FINALIZADO... < ENTER > PARA SALIR " FOR CHAR enter
END MAIN


FUNCTION init()
#i-------------
    SELECT codigo_afore 
    INTO   w_codigo_afore 
    FROM   tab_afore_local

    LET HOY = TODAY

    SELECT *, USER
    INTO   w_tabafore.*, usuario
    FROM   tab_afore_local
                                                                               
    SELECT comando_impresion
    INTO   nom_impresora
    FROM   tab_cmd_impresora
    WHERE  codigo_afore = w_tabafore.codigo_afore

    SELECT MAX(folio)
    INTO   reg_1.folio_oper12
    FROM   ret_parcial

    SELECT *                                                             
    INTO   w_paramgrales.*                                               
    FROM   seg_modulo
    WHERE  modulo_cod = 'ret'

    LET G_LISTA = w_paramgrales.ruta_listados CLIPPED,"/",HOY USING "DDMMYYYY",".802"
END FUNCTION


FUNCTION primer_paso()
#pp--------------------
    DEFINE reg_11 RECORD #loc #reg_11
        nss                   LIKE ret_parcial_tx.nss          ,
        consecutivo           LIKE ret_parcial.consecutivo     ,
        impt_autorizado       LIKE ret_parcial.impt_autorizado 
    END RECORD

    DEFINE reg_7 RECORD #loc #reg_7
        siefore               LIKE dis_cuenta.siefore
    END RECORD

    DEFINE #loc #char
        nombre_siefore        CHAR(03)

    DEFINE #loc #dec 
        monto_accion_so_sb1       DECIMAL(16,6) ,
        ld6_acc_autorizada_sb1    DECIMAL(16,6) ,

        monto_accion_so_sb2       DECIMAL(16,6) ,
        ld6_acc_autorizada_sb2    DECIMAL(16,6) ,

        monto_accion_so_sb3       DECIMAL(16,6) ,
        ld6_acc_autorizada_sb3    DECIMAL(16,6) ,

        monto_accion_so_sb4       DECIMAL(16,6) ,
        ld6_acc_autorizada_sb4    DECIMAL(16,6) ,

        monto_accion_so_sb5       DECIMAL(16,6) ,
        ld6_acc_autorizada_sb5    DECIMAL(16,6) ,

        dif_pagar_cuo_soc         DECIMAL(16,6) ,
        pes_a_pagar               DECIMAL(16,6) ,
        
        impt_autorizado_sb1       DECIMAL(16,6) ,
        impt_autorizado_sb2       DECIMAL(16,6) ,
        impt_autorizado_sb3       DECIMAL(16,6) ,
        impt_autorizado_sb4       DECIMAL(16,6) ,
        impt_autorizado_sb5       DECIMAL(16,6) 
        

    SELECT  "OK"
    FROM    ret_parcial A
    WHERE   A.folio            = reg_1.folio_oper12
    AND     A.tipo_prestacion  = 7
    AND     A.estado_solicitud = 4			#carbiar estado a 4
    GROUP BY 1
    IF STATUS = NOTFOUND THEN
        MESSAGE "NO EXISTE INFORMACION."
        ATTRIBUTE(REVERSE)
        SLEEP 2
        MESSAGE "" 
        LET sw_1 = 0
        EXIT PROGRAM
    END IF

    DECLARE cur_3 CURSOR FOR
    SELECT  A.nss           ,
            A.consecutivo   ,
            A.impt_autorizado
    FROM    ret_parcial A    
    WHERE   A.folio           = reg_1.folio_oper12
    AND     A.tipo_prestacion = 7
    AND     A.estado_solicitud = 4			#cambiar estado a 4

    START REPORT listado_1 TO G_LISTA
    FOREACH cur_3 INTO reg_11.*
        LET cont_reg_av        = cont_reg_av + 1

        SELECT fecha_genera
        INTO   vfecha_genera
        FROM   ret_parcial
        WHERE  folio       = reg_1.folio_oper12
        AND    nss         = reg_11.nss
        AND    consecutivo = reg_11.consecutivo
        GROUP BY 1
        
        SELECT precio_del_dia
        INTO   vprecio_sb1
        FROM   glo_valor_accion
        WHERE  fecha_valuacion = vfecha_genera
        AND    codigo_siefore  = 1

        SELECT precio_del_dia
        INTO   vprecio_sb2
        FROM   glo_valor_accion
        WHERE  fecha_valuacion = vfecha_genera
        AND    codigo_siefore  = 2

        SELECT precio_del_dia
        INTO   vprecio_sb3
        FROM   glo_valor_accion
        WHERE  fecha_valuacion = vfecha_genera
        AND    codigo_siefore  = 3

        SELECT precio_del_dia
        INTO   vprecio_sb4
        FROM   glo_valor_accion
        WHERE  fecha_valuacion = vfecha_genera
        AND    codigo_siefore  = 4

        SELECT precio_del_dia
        INTO   vprecio_sb5
        FROM   glo_valor_accion
        WHERE  fecha_valuacion = vfecha_genera
        AND    codigo_siefore  = 5

        SELECT SUM(monto_en_acciones)
        INTO   monto_accion_so_sb1
        FROM   dis_cuenta
        WHERE  nss             = reg_11.nss
        AND    subcuenta       = 5 ---CUOTA SOCIAL---
        AND    tipo_movimiento > 0
        AND    siefore         = 1

        IF monto_accion_so_sb1 IS NULL THEN
            LET monto_accion_so_sb1 = 0
        END IF

        SELECT SUM(monto_en_acciones)
        INTO   monto_accion_so_sb2
        FROM   dis_cuenta
        WHERE  nss             = reg_11.nss
        AND    subcuenta       = 5 ---CUOTA SOCIAL---
        AND    tipo_movimiento > 0
        AND    siefore         = 2

        IF monto_accion_so_sb2 IS NULL THEN
            LET monto_accion_so_sb2 = 0
        END IF

        SELECT SUM(monto_en_acciones)
        INTO   monto_accion_so_sb3
        FROM   dis_cuenta
        WHERE  nss             = reg_11.nss
        AND    subcuenta       = 5 ---CUOTA SOCIAL---
        AND    tipo_movimiento > 0
        AND    siefore         = 3

        IF monto_accion_so_sb3 IS NULL THEN
            LET monto_accion_so_sb3 = 0
        END IF

        SELECT SUM(monto_en_acciones)
        INTO   monto_accion_so_sb4
        FROM   dis_cuenta
        WHERE  nss             = reg_11.nss
        AND    subcuenta       = 5 ---CUOTA SOCIAL---
        AND    tipo_movimiento > 0
        AND    siefore         = 4

        IF monto_accion_so_sb4 IS NULL THEN
            LET monto_accion_so_sb4 = 0
        END IF

        SELECT SUM(monto_en_acciones)
        INTO   monto_accion_so_sb5
        FROM   dis_cuenta
        WHERE  nss             = reg_11.nss
        AND    subcuenta       = 5 ---CUOTA SOCIAL---
        AND    tipo_movimiento > 0
        AND    siefore         = 5

        IF monto_accion_so_sb5 IS NULL THEN
            LET monto_accion_so_sb5 = 0
        END IF

        IF reg_11.impt_autorizado > 0 THEN
           IF monto_accion_so_sb1 > 0 THEN
              LET ld6_acc_autorizada_sb1 = reg_11.impt_autorizado /
                                        vprecio_sb1
           END IF
           IF monto_accion_so_sb2 > 0 THEN
              LET ld6_acc_autorizada_sb2 = reg_11.impt_autorizado /
                                        vprecio_sb2
           END IF
           IF monto_accion_so_sb3 > 0 THEN
              LET ld6_acc_autorizada_sb3 = reg_11.impt_autorizado /
                                        vprecio_sb3
           END IF
           IF monto_accion_so_sb4 > 0 THEN
              LET ld6_acc_autorizada_sb4 = reg_11.impt_autorizado /
                                        vprecio_sb4
           END IF
           IF monto_accion_so_sb5 > 0 THEN
              LET ld6_acc_autorizada_sb5 = reg_11.impt_autorizado /
                                        vprecio_sb5
           END IF
        END IF
        
        -------      duda ------------------
         { IF ld6_acc_autorizada_sb1 <= monto_accion_so_sb1 THEN
--        	  LET impt_autorizado_sb1 = reg_11.impt_autorizado * vprecio_sb1
        	  LET impt_autorizado_sb1 = ld6_acc_autorizada_sb1 * vprecio_sb1
            OUTPUT TO REPORT listado_1(reg_1.folio_oper12     ,
                                       reg_11.consecutivo     ,
                                       reg_11.nss             ,
                                       "SB1",
--                                       reg_11.impt_autorizado ,
                                       impt_autorizado_sb1,
                                       ld6_acc_autorizada_sb1 ,
                                       monto_accion_so_sb1) #l1
        ELSE 
            IF monto_accion_so_sb1 > 0 THEN
--            	  LET impt_autorizado_sb1 = reg_11.impt_autorizado * vprecio_sb1
            	  LET impt_autorizado_sb1 = monto_accion_so_sb1 * vprecio_sb1
                OUTPUT TO REPORT listado_1(reg_1.folio_oper12     ,
                                           reg_11.consecutivo     ,
                                           reg_11.nss             ,
                                           "SB1",
                                           impt_autorizado_sb1    ,
--                                           reg_11.impt_autorizado ,
                                           monto_accion_so_sb1,
                                           monto_accion_so_sb1) #l1
            ELSE
		LET monto_accion_so_sb1 = 0
            END IF                                        

            LET  dif_pagar_cuo_soc      = ld6_acc_autorizada_sb1 - 
                                          monto_accion_so_sb1
            LET  ld6_acc_autorizada_sb2 = (dif_pagar_cuo_soc * vprecio_sb1) / 
                                           vprecio_sb2
--            LET impt_autorizado_sb2 = reg_11.impt_autorizado * vprecio_sb2
            LET impt_autorizado_sb2 = ld6_acc_autorizada_sb2 * vprecio_sb2
            
            OUTPUT TO REPORT listado_1(reg_1.folio_oper12     ,
                                       reg_11.consecutivo     ,
                                       reg_11.nss             ,
                                       "SB2",
                                       impt_autorizado_sb2   ,
--                                       reg_11.impt_autorizado ,
                                       ld6_acc_autorizada_sb2 ,
                                       monto_accion_so_sb2) #l1

        END IF }
        ------      duda        ------
        IF monto_accion_so_sb1  > 0 THEN
           LET impt_autorizado_sb1 = ld6_acc_autorizada_sb1 * vprecio_sb1
           OUTPUT TO REPORT listado_1(reg_1.folio_oper12     ,
                                           reg_11.consecutivo     ,
                                           reg_11.nss             ,
                                           "SB1",
                                           impt_autorizado_sb1    ,
                                           ld6_acc_autorizada_sb1 ,
                                           monto_accion_so_sb1) #l1
        ELSE
         IF monto_accion_so_sb2   > 0 THEN
            LET impt_autorizado_sb2 = ld6_acc_autorizada_sb2 * vprecio_sb2
            OUTPUT TO REPORT listado_1(reg_1.folio_oper12     ,
                                           reg_11.consecutivo     ,
                                           reg_11.nss             ,
                                           "SB2",
                                           impt_autorizado_sb2    ,
                                           ld6_acc_autorizada_sb2 ,
                                           monto_accion_so_sb2) #l1
         ELSE
          IF monto_accion_so_sb3  >  0 THEN
             LET impt_autorizado_sb3 = ld6_acc_autorizada_sb3 * vprecio_sb3
             OUTPUT TO REPORT listado_1(reg_1.folio_oper12     ,
                                           reg_11.consecutivo     ,
                                           reg_11.nss             ,
                                           "SB3",
                                           impt_autorizado_sb3    ,
                                           ld6_acc_autorizada_sb3 ,
                                           monto_accion_so_sb3) #l1
          ELSE
           IF monto_accion_so_sb4    > 0 THEN
              LET impt_autorizado_sb4 = ld6_acc_autorizada_sb4 * vprecio_sb4
              OUTPUT TO REPORT listado_1(reg_1.folio_oper12     ,
                                           reg_11.consecutivo     ,
                                           reg_11.nss             ,
                                           "SB4",
                                           impt_autorizado_sb4    ,
                                           ld6_acc_autorizada_sb4 ,
                                           monto_accion_so_sb4) #l1
         
           ELSE
            IF monto_accion_so_sb5    > 0 THEN
               LET impt_autorizado_sb5 = ld6_acc_autorizada_sb5 * vprecio_sb5
               OUTPUT TO REPORT listado_1(reg_1.folio_oper12     ,
                                           reg_11.consecutivo     ,
                                           reg_11.nss             ,
                                           "SB5",
                                           impt_autorizado_sb5    ,
                                           ld6_acc_autorizada_sb5 ,
                                           monto_accion_so_sb5) #l1
            END IF
           END IF
          END IF
         END IF
        END IF
    END FOREACH
    FINISH REPORT listado_1
END FUNCTION

FUNCTION segundo_paso()
#sp-------------------
    DEFINE #loc #char
        lp                    CHAR(100),
	permisos              CHAR(100)

    WHILE TRUE
       PROMPT " DESEA IMPRIMIR  S/N " FOR CHAR enter
       IF enter MATCHES "[SsNn]" THEN
         IF enter MATCHES "[Ss]" THEN

            LET lp = "lp ",G_LISTA CLIPPED
          --  LET lp = "vi ",G_LISTA CLIPPED
            RUN lp
            RETURN
         ELSE 
            PROMPT " PROCESO FINALIZADO... < ENTER > PARA SALIR " FOR CHAR enter
            EXIT PROGRAM
         END IF
	 LET permisos = "chmod 777 ",G_LISTA CLIPPED
	 RUN permisos
       END IF
    END WHILE
END FUNCTION

FUNCTION tercer_paso()
#tp-------------------
    OPEN WINDOW retl8021 AT 4,4 WITH FORM "RETL8021" ATTRIBUTE (BORDER)
    DISPLAY "                               < Ctrl-C >                                      " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETL802   REPORTE, PROVISION RETIRO PARCIAL POR MATRIMONIO                    " AT 3,1 ATTRIBUTE (REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE (REVERSE)
    DISPLAY BY NAME reg_1.folio_oper12
END FUNCTION

REPORT listado_1(vfolio_oper12       , 
                 vconsecutivo        , 
                 vnss                , 
                 vnombre_siefore     ,
                 vimpt_autorizado    , 
                 vld6_acc_autorizada , 
                 vmonto_accion_so) #l1
#l1--------------------

    DEFINE 
        vfolio_oper12         INTEGER                           ,
        vconsecutivo          LIKE ret_parcial.consecutivo      ,
        vnss                  CHAR(11)                          ,
        vnombre_siefore       CHAR(03)                          ,
        vimpt_autorizado      LIKE ret_parcial.impt_autorizado  , 
        vld6_acc_autorizada   DECIMAL(16,6)                     ,
        vmonto_accion_so      DECIMAL(16,6)                        

    DEFINE 
        vnombres              CHAR(40),
        vpaterno              CHAR(40),
        vmaterno              CHAR(40)
         
    DEFINE #loc #date
        d_fecha_operacion     DATE,
        nombre_final          CHAR(50)

    DEFINE 
        vimpt_autorizado_sb1      LIKE ret_parcial.impt_autorizado  , 
        vld6_acc_autorizada_sb1   DECIMAL(16,6)                     ,
        vmonto_accion_so_sb1      DECIMAL(16,6)                     ,

        vimpt_autorizado_sb2      LIKE ret_parcial.impt_autorizado  , 
        vld6_acc_autorizada_sb2   DECIMAL(16,6)                     ,
        vmonto_accion_so_sb2      DECIMAL(16,6)                     ,

        vimpt_autorizado_sb3      LIKE ret_parcial.impt_autorizado  , 
        vld6_acc_autorizada_sb3   DECIMAL(16,6)                     ,
        vmonto_accion_so_sb3      DECIMAL(16,6)                     ,

        vimpt_autorizado_sb4      LIKE ret_parcial.impt_autorizado  , 
        vld6_acc_autorizada_sb4   DECIMAL(16,6)                     ,
        vmonto_accion_so_sb4      DECIMAL(16,6)                     ,

        vimpt_autorizado_sb5      LIKE ret_parcial.impt_autorizado  , 
        vld6_acc_autorizada_sb5   DECIMAL(16,6)                     ,
        vmonto_accion_so_sb5      DECIMAL(16,6)                     


    DEFINE #loc #char
        encabezado            CHAR(60) ,
        var2                  CHAR(10) ,
        var1                  CHAR(10) ,
        L1                    CHAR(01) ,
        L2                    CHAR(02) ,
        L3                    CHAR(03) ,
        L4                    CHAR(04) ,
        L5                    CHAR(05) ,
        L6                    CHAR(06) ,
        L7                    CHAR(07) ,
        L8                    CHAR(08) ,
        L9                    CHAR(09) ,
        L10                   CHAR(10) ,
        L11                   CHAR(11) 

    DEFINE #loc #integer
        cont_nss_unicos       ,
        total_nss_sb1         ,
        total_nss_sb2         ,
        total_nss_sb3         ,
        total_nss_sb4         ,
        total_nss_sb5         INTEGER

    OUTPUT
        --PAGE LENGTH   90
        PAGE LENGTH   45
        LEFT MARGIN    0
        --RIGHT MARGIN 150
        RIGHT MARGIN   0
        TOP MARGIN     0
        BOTTOM MARGIN  0

    FORMAT
    FIRST PAGE HEADER
        LET L1  = "\304"
        LET L2  = "\304\304"
        LET L3  = "\304\304\304"
        LET L4  = "\304\304\304\304"
        LET L5  = "\304\304\304\304\304"
        LET L6  = "\304\304\304\304\304\304"
        LET L7  = "\304\304\304\304\304\304\304"
        LET L8  = "\304\304\304\304\304\304\304\304"
        LET L9 = "\304\304\304\304\304\304\304\304\304"
        LET L10 = "\304\304\304\304\304\304\304\304\304\304"
        LET L11 = "\304\304\304\304\304\304\304\304\304\304\304"


        SELECT fecha_operacion
        INTO   d_fecha_operacion
        FROM   ret_cza_lote  #
        WHERE  folio = reg_1.folio_oper12
        IF status = NOTFOUND THEN
            LET d_fecha_operacion = ""
        END IF

        LET vimpt_autorizado_sb1    = 0 
        LET vld6_acc_autorizada_sb1 = 0
        LET vmonto_accion_so_sb1    = 0
                                    
        LET vimpt_autorizado_sb2    = 0 
        LET vld6_acc_autorizada_sb2 = 0
        LET vmonto_accion_so_sb2    = 0

        LET vimpt_autorizado_sb3    = 0 
        LET vld6_acc_autorizada_sb3 = 0
        LET vmonto_accion_so_sb3    = 0

        LET vimpt_autorizado_sb4    = 0 
        LET vld6_acc_autorizada_sb4 = 0
        LET vmonto_accion_so_sb4    = 0

        LET vimpt_autorizado_sb5    = 0 
        LET vld6_acc_autorizada_sb5 = 0
        LET vmonto_accion_so_sb5    = 0

        IF w_codigo_afore = 532 THEN
            LET encabezado = "S U B D I R E C C I O N    D E    R E T I R O S"
        ELSE 
        	  LET encabezado = "      M O D U L O   D E   R E T I R O S        "     
        END IF 	  

        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'
        PRINT COLUMN 1,'\033e\033(s218T\033(s12H\033(s7B'

        --PRINT '\033E\033(10U\033&l1O\033&k2S\033&18d',
        --    '\033e\033(s16H'

        PRINT COLUMN 045,encabezado
         --   '\033015'

        SKIP 1 LINES

        PRINT COLUMN 045,"           RETIRO PARCIAL MATRIMONIO"
            --'\033015'

        SKIP 4 LINES

        PRINT
        COLUMN 001,"FECHA DEL LOTE    : ",d_fecha_operacion USING"DD/MM/YYYY",
        COLUMN 108,"PROG.    : RETL802"
            --'\033015'
        PRINT

        PRINT
        COLUMN 1,"FOLIO INTERNO     : ",vfolio_oper12 USING"##########",
        COLUMN 108,"PAGINA   :    ",PAGENO USING "####"
           -- '\033015'
        PRINT

        PRINT
        COLUMN 1,"TIPO DE OPERACION : REPORTE DE PROVISION EN ACCIONES DE RETIRO PARCIAL / AYUDA DE GASTOS DE MATRIMONIO     ",
        COLUMN 108,"FECHA : ", HOY USING "DD/MM/YYYY"
            --'\033015'
       PRINT
       PRINT
            COLUMN 1,"FECHA GENERA      : ",vfecha_genera USING "DD/MM/YYYY"   ,
            COLUMN 034,"VALOR ACCION SB1 :",vprecio_sb1 USING "######&.&&&&&&" ,
            COLUMN 074,"VALOR ACCION SB2 :",vprecio_sb2 USING "######&.&&&&&&" 
            --'\033015'
       PRINT
     PRINT  COLUMN 034,"VALOR ACCION SB3 :",vprecio_sb3 USING "######&.&&&&&&" ,
            COLUMN 074,"VALOR ACCION SB4 :",vprecio_sb4 USING "######&.&&&&&&" 
            --'\033015'
       PRINT
     PRINT  COLUMN 034,"VALOR ACCION SB5 :",vprecio_sb5 USING "######&.&&&&&&" 
            --'\033015'

        PRINT '\033e\033(s218T\033(s14H\033(s7B'

        PRINT
        COLUMN 001,"\332",L10,
                   "\302",L11,
                   "\302",L10,L10,L10,L10,
                   "\302",L9,
                   "\302",L11,
                   "\302",L5,
                   "\302",L9,
                   "\302",L10,L6,
                   "\302",L10,L3,
                   "\302",L10,L5,
                   "\277"
          --  '\033015'

        PRINT
        COLUMN 001,"|          |           |",
        COLUMN 065,"|",
        COLUMN 075,"|",
        COLUMN 087,"|",
        COLUMN 093,"|",
        COLUMN 095," PESOS ",
        COLUMN 103,"|",
        COLUMN 120,"|",
        COLUMN 123,"ACCIONES",
        COLUMN 134,"|",
        COLUMN 150,"|"
           -- '\033015'

        PRINT
        COLUMN 1,"|  CONSEC. |    NSS    |        NOMBRE DEL ",
                 "TRABAJADOR           |         |",
        COLUMN 087,"|",    
        COLUMN 093,"|",    
        COLUMN 098,"A",
        COLUMN 103,"|",     
        COLUMN 120,"|",     
        COLUMN 122,"     A    ", 
        COLUMN 134,"|",     
        COLUMN 140,"100%",  
        COLUMN 150,"|"
           -- '\033015'

        PRINT
        COLUMN 01,"|          |           |",
        COLUMN 65,"| SIEFORE",
        COLUMN 75,"|DIAGNOSTICO",
        COLUMN 86,"|PRES.",
        COLUMN 80,"|  PAGAR  |                |    PAGAR    ",
                  "| CUOTA SOCIAL  |"
            --'\033015'
                                                                               
        PRINT
        COLUMN 1,"\300",L10,
                 "\301",L11,
                 "\301",L10,L10,L10,L10,
                 "\301",L9,
                 "\301",L11,
                 "\301",L5,
                 "\301",L9,
                 "\301",L10,L6,
                 "\301",L10,L3,
                 "\301",L10,L5,
                 "\331"
           -- '\033015'

    PAGE HEADER
        LET L1  = "\304"
        LET L2  = "\304\304"
        LET L3  = "\304\304\304"
        LET L4  = "\304\304\304\304"
        LET L5  = "\304\304\304\304\304"
        LET L6  = "\304\304\304\304\304\304"
        LET L7  = "\304\304\304\304\304\304\304"
        LET L8  = "\304\304\304\304\304\304\304\304"
        LET L9 = "\304\304\304\304\304\304\304\304\304"
        LET L10 = "\304\304\304\304\304\304\304\304\304\304"
        LET L11 = "\304\304\304\304\304\304\304\304\304\304\304"


        SELECT fecha_operacion
        INTO   d_fecha_operacion
        FROM   ret_cza_lote  #
        WHERE  folio = reg_1.folio_oper12
        IF status = NOTFOUND THEN
            LET d_fecha_operacion = ""
        END IF

        IF w_codigo_afore = 532 THEN
            LET encabezado = "S U B D I R E C C I O N    D E    R E T I R O S"
        ELSE 
        	  LET encabezado = "      M O D U L O   D E   R E T I R O S        "     
        END IF 	  

        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'
        PRINT COLUMN 1,'\033e\033(s218T\033(s12H\033(s7B'

        --PRINT '\033E\033(10U\033&l1O\033&k2S\033&18d',
         --   '\033e\033(s16H'

        PRINT COLUMN 045,encabezado
            --'\033015'

        SKIP 1 LINES

        PRINT COLUMN 045,"           RETIRO PARCIAL MATRIMONIO"
            --'\033015'

        SKIP 4 LINES

        PRINT
        COLUMN 001,"FECHA DEL LOTE    : ",d_fecha_operacion USING"DD/MM/YYYY",
        COLUMN 108,"PROG.    : RETL802"
           -- '\033015'

        PRINT
        COLUMN 1,"FOLIO INTERNO     : ",vfolio_oper12 USING"##########",
        COLUMN 108,"PAGINA   :    ",PAGENO USING "####"
        --    '\033015'
        PRINT

        PRINT
        COLUMN 1,"TIPO DE OPERACION : REPORTE DE PROVISION EN ACCIONES DE RETIRO PARCIAL / AYUDA DE GASTOS DE MATRIMONIO     ",
        COLUMN 108,"FECHA : ", HOY USING "DD/MM/YYYY"
         --   '\033015'
       PRINT
       PRINT
            COLUMN 1,"FECHA GENERA      : ",vfecha_genera USING "DD/MM/YYYY"   ,
            COLUMN 034,"VALOR ACCION SB1 :",vprecio_sb1 USING "######&.&&&&&&" ,
            COLUMN 074,"VALOR ACCION SB2 :",vprecio_sb2 USING "######&.&&&&&&" 
            --'\033015'
        PRINT
     PRINT  COLUMN 034,"VALOR ACCION SB3 :",vprecio_sb3 USING "######&.&&&&&&" ,
            COLUMN 074,"VALOR ACCION SB4 :",vprecio_sb4 USING "######&.&&&&&&" 
            --'\033015'
     PRINT  COLUMN 034,"VALOR ACCION SB5 :",vprecio_sb5 USING "######&.&&&&&&" 
            --'\033015'
        PRINT

        PRINT '\033e\033(s218T\033(s14H\033(s7B'

        PRINT
        COLUMN 001,"\332",L10,
                   "\302",L11,
                   "\302",L10,L10,L10,L10,
                   "\302",L9,
                   "\302",L11,
                   "\302",L5,
                   "\302",L9,
                   "\302",L10,L6,
                   "\302",L10,L3,
                   "\302",L10,L5,
                   "\277"
          --  '\033015'

        PRINT
        COLUMN 001,"|          |           |",
        COLUMN 065,"|",
        COLUMN 075,"|",
        COLUMN 087,"|",
        COLUMN 093,"|",
        COLUMN 095," PESOS ",
        COLUMN 103,"|",
        COLUMN 120,"|",
        COLUMN 123,"ACCIONES",
        COLUMN 134,"|",
        COLUMN 150,"|"
           -- '\033015'

        PRINT
        COLUMN 1,"|  CONSEC. |    NSS    |        NOMBRE DEL ",
                 "TRABAJADOR           |         |",
        COLUMN 087,"|",    
        COLUMN 093,"|",    
        COLUMN 098,"A",
        COLUMN 103,"|",     
        COLUMN 120,"|",     
        COLUMN 122,"     A    ", 
        COLUMN 134,"|",     
        COLUMN 140,"100%",  
        COLUMN 150,"|"
            --'\033015'

        PRINT
        COLUMN 01,"|          |           |",
        COLUMN 65,"| SIEFORE",
        COLUMN 75,"|DIAGNOSTICO",
        COLUMN 86,"|PRES.",
        COLUMN 80,"|  PAGAR  |                |    PAGAR    ",
                  "| CUOTA SOCIAL  |"
            --'\033015'
                                                                               
        PRINT
        COLUMN 1,"\300",L10,
                 "\301",L11,
                 "\301",L10,L10,L10,L10,
                 "\301",L9,
                 "\301",L11,
                 "\301",L5,
                 "\301",L9,
                 "\301",L10,L6,
                 "\301",L10,L3,
                 "\301",L10,L5,
                 "\331"
            --'\033015'

    ON EVERY ROW
        INITIALIZE nombre_final, vnombres, vpaterno, vmaterno TO NULL
        SELECT @nombres, @paterno, @materno
        INTO vnombres, vpaterno, vmaterno
        FROM afi_mae_afiliado
        WHERE @n_seguro = vnss 
        LET nombre_final = vpaterno CLIPPED, " ",
                           vmaterno CLIPPED, " ",
                           vnombres CLIPPED

        PRINT                                                                 
        PRINT                                                                 
        COLUMN 004,vconsecutivo           USING "######"        ,
        COLUMN 013,vnss                                         ,
        COLUMN 026,nombre_final           CLIPPED               ,
        COLUMN 069,vnombre_siefore                              ,
        COLUMN 089,"7"                    USING "&"             ,
        COLUMN 094,vimpt_autorizado       USING "######.&&"     ,
        COLUMN 121,vld6_acc_autorizada    USING "######.&&&&&&" ,
        COLUMN 136,vmonto_accion_so       USING "######.&&&&&&" 
            --'\033015'

        IF vnombre_siefore = "SB1" THEN
            LET total_nss_sb1           = total_nss_sb1 + 1
            LET vimpt_autorizado_sb1    = vimpt_autorizado_sb1    + vimpt_autorizado
            LET vld6_acc_autorizada_sb1 = vld6_acc_autorizada_sb1 + vld6_acc_autorizada
            LET vmonto_accion_so_sb1    = vmonto_accion_so_sb1    + vmonto_accion_so
        ELSE 
         IF vnombre_siefore = "SB2" THEN
            LET total_nss_sb2           = total_nss_sb2 + 1
            LET vimpt_autorizado_sb2    = vimpt_autorizado_sb2    + vimpt_autorizado
            LET vld6_acc_autorizada_sb2 = vld6_acc_autorizada_sb2 + vld6_acc_autorizada
            LET vmonto_accion_so_sb2    = vmonto_accion_so_sb2    + vmonto_accion_so
         ELSE
          IF vnombre_siefore = "SB3" THEN
             LET total_nss_sb3           = total_nss_sb3 + 1
             LET vimpt_autorizado_sb3    = vimpt_autorizado_sb3    + vimpt_autorizado
             LET vld6_acc_autorizada_sb3 = vld6_acc_autorizada_sb3 + vld6_acc_autorizada
             LET vmonto_accion_so_sb3    = vmonto_accion_so_sb3    + vmonto_accion_so
          ELSE
           IF vnombre_siefore = "SB4" THEN
              LET total_nss_sb4           = total_nss_sb4 + 1
              LET vimpt_autorizado_sb4    = vimpt_autorizado_sb4    + vimpt_autorizado
              LET vld6_acc_autorizada_sb4 = vld6_acc_autorizada_sb4 + vld6_acc_autorizada
              LET vmonto_accion_so_sb4    = vmonto_accion_so_sb4    + vmonto_accion_so
           ELSE
            IF vnombre_siefore = "SB5" THEN
               LET total_nss_sb5           = total_nss_sb5 + 1
               LET vimpt_autorizado_sb5    = vimpt_autorizado_sb5    + vimpt_autorizado
               LET vld6_acc_autorizada_sb5 = vld6_acc_autorizada_sb5 + vld6_acc_autorizada
               LET vmonto_accion_so_sb5    = vmonto_accion_so_sb5    + vmonto_accion_so
            END IF
           END IF
          END IF
         END IF
        END IF 
      
        {IF lineno > 45 THEN
           SKIP TO TOP OF PAGE
        END IF}

    ON LAST ROW
        SKIP 3 LINES                                                          

        -------------------
        ---TOTALES POR SB1
        -------------------
        IF total_nss_sb1 > 0 THEN 
            PRINT
            COLUMN 1,"\332",L10,
                     "\302",L11,
                     "\302",L10,L10,L10,L10,
                     "\302",L9,
                     "\302",L11,
                     "\302",L5,
                     "\302",L9,
                     "\302",L10,L6,
                     "\302",L10,L3,
                     "\302",L10,L5,
                     "\277"
            --'\033015'
            
            PRINT                                                                 
            COLUMN 001,"|          |"                  ,
            COLUMN 015,total_nss_sb1 USING "#######","  |" ,
            COLUMN 045,"T O T A L E S    : "           ,
            COLUMN 065,"|"                             ,
            COLUMN 069,"SB1"                           ,
            COLUMN 075,"|"                             ,
            COLUMN 087,"|"                             ,
            COLUMN 093,"|"                             ,
            COLUMN 094,vimpt_autorizado_sb1       USING "######.&&"     ,
            COLUMN 103,"|"                             ,
            COLUMN 120,"|"                             ,
            COLUMN 121,vld6_acc_autorizada_sb1 USING "######.&&&&&&" ,
            COLUMN 134,"|"                             ,
            COLUMN 136,vmonto_accion_so_sb1  USING "######.&&&&&&" ,
            COLUMN 150,"|"
            --'\033015'
                                                                                 
            PRINT
            COLUMN 1,"\300",L10,
                     "\301",L11,
                     "\301",L10,L10,L10,L10,
                     "\301",L9,
                     "\301",L11,
                     "\301",L5,
                     "\301",L9,
                     "\301",L10,L6,
                     "\301",L10,L3,
                     "\301",L10,L5,
                     "\331"
            --'\033015'
        END IF 

        -------------------
        ---TOTALES POR SB2
        -------------------
        IF total_nss_sb2 > 0 THEN
            PRINT
            COLUMN 1,"\332",L10,
                     "\302",L11,
                     "\302",L10,L10,L10,L10,
                     "\302",L9,
                     "\302",L11,
                     "\302",L5,
                     "\302",L9,
                     "\302",L10,L6,
                     "\302",L10,L3,
                     "\302",L10,L5,
                     "\277"
            --'\033015'
            
            PRINT                                                                 
            COLUMN 001,"|          |"                  ,
            COLUMN 015,total_nss_sb2 USING "#######","  |" ,
            COLUMN 045,"T O T A L E S    : "           ,
            COLUMN 065,"|"                             ,
            COLUMN 069,"SB2"                           ,
            COLUMN 075,"|"                             ,
            COLUMN 087,"|"                             ,
            COLUMN 093,"|"                             ,
            COLUMN 094,vimpt_autorizado_sb2       USING "######.&&"     ,
            COLUMN 103,"|"                             ,
            COLUMN 120,"|"                             ,
            COLUMN 121,vld6_acc_autorizada_sb2 USING "######.&&&&&&" ,
            COLUMN 134,"|"                             ,
            COLUMN 136,vmonto_accion_so_sb2  USING "######.&&&&&&" ,
            COLUMN 150,"|"
           -- '\033015'
                                                                                 
            PRINT
            COLUMN 1,"\300",L10,
                     "\301",L11,
                     "\301",L10,L10,L10,L10,
                     "\301",L9,
                     "\301",L11,
                     "\301",L5,
                     "\301",L9,
                     "\301",L10,L6,
                     "\301",L10,L3,
                     "\301",L10,L5,
                     "\331"
            --'\033015'
        END IF 
        -------------------
        ---TOTALES POR SB3
        -------------------
        IF total_nss_sb3 > 0 THEN
            PRINT
            COLUMN 1,"\332",L10,
                     "\302",L11,
                     "\302",L10,L10,L10,L10,
                     "\302",L9,
                     "\302",L11,
                     "\302",L5,
                     "\302",L9,
                     "\302",L10,L6,
                     "\302",L10,L3,
                     "\302",L10,L5,
                     "\277"
           -- '\033015'
            
            PRINT                                                                 
            COLUMN 001,"|          |"                  ,
            COLUMN 015,total_nss_sb3 USING "#######","  |" ,
            COLUMN 045,"T O T A L E S    : "           ,
            COLUMN 065,"|"                             ,
            COLUMN 069,"SB3"                           ,
            COLUMN 075,"|"                             ,
            COLUMN 087,"|"                             ,
            COLUMN 093,"|"                             ,
            COLUMN 094,vimpt_autorizado_sb3       USING "######.&&"     ,
            COLUMN 103,"|"                             ,
            COLUMN 120,"|"                             ,
            COLUMN 121,vld6_acc_autorizada_sb3 USING "######.&&&&&&" ,
            COLUMN 134,"|"                             ,
            COLUMN 136,vmonto_accion_so_sb3  USING "######.&&&&&&" ,
            COLUMN 150,"|"
            --'\033015'
                                                                                 
            PRINT
            COLUMN 1,"\300",L10,
                     "\301",L11,
                     "\301",L10,L10,L10,L10,
                     "\301",L9,
                     "\301",L11,
                     "\301",L5,
                     "\301",L9,
                     "\301",L10,L6,
                     "\301",L10,L3,
                     "\301",L10,L5,
                     "\331"
            --'\033015'
        END IF 
        -------------------
        ---TOTALES POR SB4
        -------------------
        IF total_nss_sb4 > 0 THEN
            PRINT
            COLUMN 1,"\332",L10,
                     "\302",L11,
                     "\302",L10,L10,L10,L10,
                     "\302",L9,
                     "\302",L11,
                     "\302",L5,
                     "\302",L9,
                     "\302",L10,L6,
                     "\302",L10,L3,
                     "\302",L10,L5,
                     "\277"
            --'\033015'
            
            PRINT                                                                 
            COLUMN 001,"|          |"                  ,
            COLUMN 015,total_nss_sb4 USING "#######","  |" ,
            COLUMN 045,"T O T A L E S    : "           ,
            COLUMN 065,"|"                             ,
            COLUMN 069,"SB4"                           ,
            COLUMN 075,"|"                             ,
            COLUMN 087,"|"                             ,
            COLUMN 093,"|"                             ,
            COLUMN 094,vimpt_autorizado_sb4       USING "######.&&"     ,
            COLUMN 103,"|"                             ,
            COLUMN 120,"|"                             ,
            COLUMN 121,vld6_acc_autorizada_sb4 USING "######.&&&&&&" ,
            COLUMN 134,"|"                             ,
            COLUMN 136,vmonto_accion_so_sb4  USING "######.&&&&&&" ,
            COLUMN 150,"|"
            --'\033015'
                                                                                 
            PRINT
            COLUMN 1,"\300",L10,
                     "\301",L11,
                     "\301",L10,L10,L10,L10,
                     "\301",L9,
                     "\301",L11,
                     "\301",L5,
                     "\301",L9,
                     "\301",L10,L6,
                     "\301",L10,L3,
                     "\301",L10,L5,
                     "\331"
            --'\033015'
        END IF 
        -------------------
        ---TOTALES POR SB5
        -------------------
        IF total_nss_sb5 > 0 THEN
            PRINT
            COLUMN 1,"\332",L10,
                     "\302",L11,
                     "\302",L10,L10,L10,L10,
                     "\302",L9,
                     "\302",L11,
                     "\302",L5,
                     "\302",L9,
                     "\302",L10,L6,
                     "\302",L10,L3,
                     "\302",L10,L5,
                     "\277"
            --'\033015'
            
            PRINT                                                                 
            COLUMN 001,"|          |"                  ,
            COLUMN 015,total_nss_sb5 USING "#######","  |" ,
            COLUMN 045,"T O T A L E S    : "           ,
            COLUMN 065,"|"                             ,
            COLUMN 069,"SB5"                           ,
            COLUMN 075,"|"                             ,
            COLUMN 087,"|"                             ,
            COLUMN 093,"|"                             ,
            COLUMN 094,vimpt_autorizado_sb5       USING "######.&&"     ,
            COLUMN 103,"|"                             ,
            COLUMN 120,"|"                             ,
            COLUMN 121,vld6_acc_autorizada_sb5 USING "######.&&&&&&" ,
            COLUMN 134,"|"                             ,
            COLUMN 136,vmonto_accion_so_sb5  USING "######.&&&&&&" ,
            COLUMN 150,"|"
            --'\033015'
                                                                                 
            PRINT
            COLUMN 1,"\300",L10,
                     "\301",L11,
                     "\301",L10,L10,L10,L10,
                     "\301",L9,
                     "\301",L11,
                     "\301",L5,
                     "\301",L9,
                     "\301",L10,L6,
                     "\301",L10,L3,
                     "\301",L10,L5,
                     "\331"
            --'\033015'
        END IF 

        SELECT COUNT(UNIQUE nss)
        INTO   cont_nss_unicos
        FROM   ret_parcial
        WHERE  folio            = reg_1.folio_oper12
        AND    tipo_prestacion  = 7
        AND    estado_solicitud = 4			#cambiar estado a 4
        
        IF cont_nss_unicos > 0 THEN 
            PRINT
            COLUMN 1,"\332",L10,
                     L11,L1,
                     L10,L10,L10,L10,L1,
                     L9,L1,
                     L11,L1,
                     L5,L1,
                     L9,L1,
                     L10,L6,L1,
                     L10,L3,L1,
                     L10,L5,L1,
                     "\277"
           -- '\033015'

            PRINT 
                COLUMN 001,"|",
                COLUMN 045,"TOTAL DE NSS UNICOS: ", cont_nss_unicos USING "#######",
                COLUMN 150,"|"
            --'\033015'

            PRINT 
            COLUMN 1,"\300",L10,
                     L11,L1,
                     L10,L10,L10,L10,L1,
                     L9,L1,
                     L11,L1,
                     L5,L1,
                     L9,L1,
                     L10,L6,L1,
                     L10,L3,L1,
                     L10,L5,L1,
                     "\331"
            --'\033015'
        END IF 
   PAGE TRAILER 

      PRINT COLUMN 001,"REPORTE : ",HOY USING "DDMMYYYY",".802"
            --'\033015'

END REPORT 
