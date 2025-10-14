################################################################################
#Proyecto          => SISTEMA DE AFORES ( MEXICO )                             #
#Owner             => E.F.P.                                                   #
#Programa RETL808  => REPORTE PREVIO AL CALCULO DE ISR EN LA LIQUIDACION (05)  #
#By                => FRANCO ESTEBAN ULLOA VIDELA                              #
#Fecha actualiz.   => 12 DE MARZO DEL 2004                                     #
#Actualizacion     => FRANCO ESTEBAN ULLOA VIDELA                              #
#Sistema           => RET                                                      #
#Linea modificada  => 25 DE MARZO 2004  (JLSC)                                 #
################################################################################
DATABASE safre_af 
GLOBALS
    DEFINE #glo #g_seg_modulo #g_lp_impresoras
        g_seg_modulo          RECORD LIKE seg_modulo.*                 

    DEFINE reg_1 RECORD #glo #reg_1
	n_seguro              LIKE ret_solicitud_tx.nss               ,
	tipo_retiro           LIKE ret_solicitud_tx.tipo_retiro       ,
	consecutivo           LIKE ret_solicitud_tx.consecutivo       ,
        estad_cod             LIKE tab_codpos.estad_cod               ,
        deleg_cod             LIKE tab_codpos.deleg_cod               ,
        monto_sm              LIKE tabsalario_minimo2.monto_sm        ,
	zona_cod              LIKE tabsalario_minimo2.zona_cod        ,
        semanas_cotizadas     LIKE ret_solicitud_tx.semanas_cotizadas
    END RECORD

    DEFINE #glo #date
        HOY                   DATE

    DEFINE #glo #char
	enter                 CHAR(001) ,
        sel_where             CHAR(400) ,
        g_lista               CHAR(100) ,
        COMANDO               CHAR(100) ,
        lp                    CHAR(100) ,
        desc_diag             CHAR(040)

    DEFINE #glo #integer
	xfolio                INTEGER
END GLOBALS


MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP          ,
        PROMPT LINE LAST    ,
        ACCEPT KEY CONTROL-I
         
    CALL init() #i
    OPEN WINDOW retl8081 AT 2,2 WITH FORM "RETL8081" ATTRIBUTE(BORDER)
    DISPLAY "                             < Ctrl-C > Salir                                  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETL808     REPORTE DE VALIDACION PREVIO AL CALCULO DE ISR                    " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

    INPUT BY NAME xfolio 
        AFTER FIELD xfolio
            IF xfolio IS NULL THEN
                ERROR "  FOLIO NO PUEDE SER NULO " ATTRIBUTE(REVERSE)
                NEXT FIELD xfolio
            ELSE
                SELECT "OK"
                FROM   ret_solicitud_tx
                WHERE  folio       = xfolio
                GROUP BY 1

                IF STATUS = NOTFOUND THEN
                    ERROR "  FOLIO INEXISTENTE " ATTRIBUTE(REVERSE)
                    NEXT FIELD xfolio
                END IF
            END IF

        ON KEY (ESC)
            IF xfolio IS NULL THEN
                ERROR "  FOLIO NO PUEDE SER NULO " ATTRIBUTE(REVERSE)
                NEXT FIELD xfolio
            ELSE
                SELECT "OK"
                FROM   ret_solicitud_tx
                WHERE  folio       = xfolio
                GROUP BY 1

                IF STATUS = NOTFOUND THEN
                    ERROR "  FOLIO INEXISTENTE " ATTRIBUTE(REVERSE)
                    NEXT FIELD xfolio
                END IF
            END IF

            WHILE TRUE
                PROMPT " ESTA SEGURO S/N " FOR CHAR enter
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
	    EXIT INPUT

        ON KEY (INTERRUPT)
            PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR ENTER
            EXIT PROGRAM

    END INPUT

    DISPLAY " PROCESANDO INFORMACION " AT 21,1 ATTRIBUTE(REVERSE)
    
    CALL proceso_principal()  #pp

    LET COMANDO = "chmod 777 ",g_lista
    RUN COMANDO

    DISPLAY " REPORTE GENERADO EN : ",g_lista AT 18,11

    PROMPT " DESEA GENERAR IMPRESION (S/N)?  "
    FOR CHAR enter

    IF enter MATCHES "[SsNn]" THEN
        IF enter MATCHES "[Ss]" THEN
            LET lp = "lp ",g_lista
            RUN lp
            CLOSE WINDOW retl8081
        ELSE
            DISPLAY "                        " AT 19,1 
            PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " 
	    FOR CHAR enter
            CLOSE WINDOW retl8081
            EXIT PROGRAM
        END IF
    END IF
END MAIN


FUNCTION init()
#i------------
    LET HOY = TODAY

    LET int_flag = 0       
    INITIALIZE xfolio TO NULL

    SELECT *     
    INTO   g_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = 'ret'
   
    LET g_lista = g_seg_modulo.ruta_listados CLIPPED,
		 "/",HOY USING "YYYYMMDD",".",TIME
END FUNCTION


FUNCTION proceso_principal()
#pp-------------------------

    START REPORT listado_1 TO g_lista

    DECLARE cur_1 CURSOR FOR
     SELECT nss        ,
	    tipo_retiro,
            consecutivo
       FROM ret_solicitud_tx
      WHERE folio = xfolio

    FOREACH cur_1 INTO reg_1.n_seguro   ,
		       reg_1.tipo_retiro,
		       reg_1.consecutivo

        LET reg_1.estad_cod = 0
        LET reg_1.deleg_cod = 0

        SELECT B.estado   ,
               B.delega    
        INTO   reg_1.estad_cod,
               reg_1.deleg_cod
        FROM   afi_mae_afiliado A, afi_domicilio B --, tab_codpos C
        WHERE  A.n_seguro       = reg_1.n_seguro
        AND    A.n_seguro       = B.nss
        AND    A.n_folio        = B.n_folio
        AND    A.tipo_solicitud = B.tipo_solicitud
        AND    B.marca_envio    = "X"
 
        LET reg_1.monto_sm = 0
        LET reg_1.zona_cod = 0

        SELECT B.monto_sm,
               B.zona_cod
        INTO   reg_1.monto_sm ,
               reg_1.zona_cod
        FROM   tab_zona_geo A, tabsalario_minimo2 B
        WHERE  A.estad_cod = reg_1.estad_cod
        AND    A.deleg_cod = reg_1.deleg_cod
        AND    A.zona_cod  = B.zona_cod 
	AND    B.fecha_hasta_sm IS NULL

        LET reg_1.semanas_cotizadas = 0

        SELECT semanas_cotizadas
        INTO   reg_1.semanas_cotizadas
        FROM   ret_solicitud_tx
        WHERE  nss         = reg_1.n_seguro
        AND    consecutivo = reg_1.consecutivo

        OUTPUT TO REPORT listado_1(reg_1.*) #l1
    END FOREACH
 FINISH REPORT listado_1
END FUNCTION


REPORT listado_1(reg_1)
#l1--------------------
    DEFINE reg_1 RECORD #glo #reg_1
	n_seguro              LIKE ret_solicitud_tx.nss               ,
	tipo_retiro           LIKE ret_solicitud_tx.tipo_retiro       ,
	consecutivo           LIKE ret_solicitud_tx.consecutivo       ,
        estad_cod             LIKE tab_codpos.estad_cod               ,
        deleg_cod             LIKE tab_codpos.deleg_cod               ,
        monto_sm              LIKE tabsalario_minimo2.monto_sm        ,
	zona_cod              LIKE tabsalario_minimo2.zona_cod        ,
        semanas_cotizadas     LIKE ret_solicitud_tx.semanas_cotizadas
    END RECORD

    DEFINE L1              CHAR(01),
           L5              CHAR(05),
           L10             CHAR(10)

    OUTPUT
        PAGE LENGTH   90
        LEFT MARGIN    0
        RIGHT MARGIN 200
        TOP MARGIN     0
        BOTTOM MARGIN  0
        TOP OF PAGE "^L"

    FORMAT
    PAGE HEADER
        LET L1  = "\304"
        LET L5  = "\304\304\304\304\304"
        LET L10 = "\304\304\304\304\304\304\304\304\304\304"

        PRINT '\033e\033(10U\033&l10\033&k2S\033&l12d\033(s12H'

        PRINT COLUMN 1,'\033e\033(s218T\033(s12H\033(s7B'

        PRINT COLUMN 31,"G E R E N C I A   D E   R E T I R O S"

        SKIP 1 LINES

        PRINT COLUMN 15,"REPORTE DE VALIDACION PREVIAS AL CALCULO DE RETENCION DE IMPUESTOS "

        SKIP 4 LINES

        PRINT COLUMN 1,"FECHA REPORTE   : ",HOY USING "DD/MM/YYYY",
              COLUMN 70,"PROG.   : RETL808"
        PRINT
        PRINT COLUMN 1,"FOLIO INTERNO   : ",xfolio 
  
        PRINT
        PRINT COLUMN 1 ,"TIPO DE REPORTE : ",reg_1.tipo_retiro,
              COLUMN 70,"PAGINA  :    ",PAGENO USING "####"

        PRINT COLUMN 1,'\033e\033(s218T\033(s15H\033(s7B'
 
        PRINT COLUMN 1,"\332",L10,L1,"\302",L10,L1,L1,L1,"\302",L5,L1,"\302",L10,"\302",L10,L5,L1,"\302",L10,L5,L1,L1,L1,"\302",L10,L10,"\277"
     
        PRINT COLUMN 1,"|    NSS    | CONSECUTIVO |ESTADO|    CP    |      SMGZ      | ZONA GEOGRAFICA  |  SEMANAS COTIZADAS |"       

        PRINT COLUMN 1,"\300",L10,L1,"\301",L10,L1,L1,L1,"\301",L5,L1,"\301",L10,"\301",L10,L5,L1,"\301",L10,L5,L1,L1,L1,"\301",L10,L10,"\331"
 
   ON EVERY ROW
        PRINT                                                                 
            COLUMN 002,reg_1.n_seguro                        ,
            COLUMN 014,reg_1.consecutivo USING"##########"   ,
            COLUMN 026,reg_1.estad_cod   USING"######"       ,
            COLUMN 035,reg_1.deleg_cod   USING"######"       ,
            COLUMN 045,reg_1.monto_sm    USING"########.&&"  ,
            COLUMN 068,reg_1.zona_cod                        ,
            COLUMN 088,reg_1.semanas_cotizadas
END REPORT 
