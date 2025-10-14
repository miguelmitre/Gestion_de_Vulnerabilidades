###########################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                        #
#Programa RETL016  => REPORTE DE RETIROS DE APORTACIONES VOLUNTARIAS      #
#Fecha             => 8 DE MAYO 2003                                      #
#By                => DMR                                                 #
#Actualizo         => STEFANIE DANIELA VERA PIÑA                          #
#Fecha             => 14-DICIEMBRE-2007                                   #
#Sistema           => RET 		                                  #
###########################################################################
DATABASE safre_af
GLOBALS

    DEFINE #glo #char
        enter              CHAR(1),
        aux_pausa          CHAR(1), 
        COMANDO            CHAR(100),
        g_usuario          CHAR(8),
        g_lista            CHAR(100),
        lp                 CHAR(100),
        x_busca            CHAR(500),
        txt_1              CHAR(500)

    DEFINE #glo #date
        hoy                DATE
        
    DEFINE #glo #decimal
        monto_neto         ,
        tot_monto_neto     ,
        monto_ret          ,
        tot_monto_ret      ,
        monto_bruto        ,
        tot_monto_bruto    DEC(16,6)

    DEFINE #glo #integer   
        cont               ,
        folio_liq          INTEGER   
            
    DEFINE #glo #smallint
        s_codigo_afore     ,
        sw                 ,
        vsubcta            SMALLINT 
         
    DEFINE reg_1  RECORD
           nss             CHAR(11),
           folio_liq       INTEGER,
           fecha_liq       DATE   ,
           consecutivo     INTEGER
    END RECORD


    DEFINE g_afore         RECORD LIKE tab_afore_local.*
    DEFINE s_modulo        RECORD LIKE seg_modulo.* 
 
END GLOBALS


MAIN
  OPTIONS
  PROMPT LINE LAST,
  MESSAGE LINE LAST,
  ERROR LINE LAST

  CALL init() #i
  OPEN WINDOW retl016 AT 4,4 WITH FORM "RETL0161" ATTRIBUTE (BORDER)
    DISPLAY "                            < Ctrl-C > Salir                                   "   AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETL016   REPORTE DE RETIROS DE APORTACIONES VOLUNTARIAS                      " AT 3,1 ATTRIBUTE (REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE (REVERSE)

    LET int_flag = 0

    CONSTRUCT BY NAME x_busca ON A.n_seguro         , 
                                 A.n_folio_liq      ,
                                 A.fecha_ult_ret    
                                  
       ON KEY (CONTROL-C)
          IF int_flag = 0 THEN
             LET int_flag = 1  
             EXIT CONSTRUCT
          END IF

       ON KEY (INTERRUPT)
          IF int_flag = 0 THEN
             LET int_flag = 1  
             EXIT CONSTRUCT
          END IF

       ON KEY(ESC)
          LET int_flag = FALSE
          EXIT CONSTRUCT
 
    END CONSTRUCT

    IF int_flag = 1 THEN
       PROMPT " PROCESO CANCELADO ... < ENTER > PARA SALIR " FOR CHAR enter
       EXIT PROGRAM
    ELSE
       LET cont = 0
       LET txt_1="SELECT  a.n_seguro, a.n_folio_liq, a.fecha_ult_ret, ",
                 "a.consecutivo ",
               " FROM   ret_cta_vol a ",
               " WHERE ",x_busca CLIPPED,
               " ORDER BY 3,2,1 " 
 
       PREPARE bus_2 FROM txt_1
       DECLARE cur_2 CURSOR FOR bus_2
       FOREACH cur_2 INTO reg_1.*
          LET cont = cont + 1
       END FOREACH     
       IF cont IS NULL OR cont = 0 THEN
          PROMPT " NO EXISTEN REGISTROS ... < ENTER > PARA SALIR " 
	  FOR CHAR enter
	  EXIT PROGRAM
       END IF
    END IF

    WHILE TRUE
        PROMPT " ESTA SEGURO S/N ? " FOR CHAR aux_pausa
        IF aux_pausa MATCHES "[SsNn]" THEN
           IF aux_pausa MATCHES "[Ss]" THEN
              EXIT WHILE
           ELSE
              PROMPT " PROCESO CANCELADO ... < ENTER > PARA SALIR " 
	      FOR CHAR enter
	      EXIT PROGRAM
	   END IF
        END IF
    END WHILE 

    CALL primer_paso() #pp

    PROMPT " DESEA GENERAR IMPRESION S/N ? " FOR CHAR aux_pausa
    CLOSE WINDOW retl016

    LET COMANDO = "chmod 777 ",s_modulo.ruta_listados CLIPPED,
   		     "/",HOY USING "DDMMYYYY",".016"

    RUN COMANDO
    CLEAR SCREEN 

    IF aux_pausa matches "[Ss]" THEN 
       LET lp = "lp '",g_lista CLIPPED,"'" 
       RUN lp
    END IF

  OPEN WINDOW retl016 AT 4,4 WITH FORM "RETL0161" ATTRIBUTE (BORDER)
    DISPLAY "                            < Ctrl-C > Salir                                   "   AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETL016         REPORTE DE RETIROS DE APORTACIONES VOLUNTARIAS                " AT 3,1 ATTRIBUTE (REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE (REVERSE)

    DISPLAY  reg_1.nss, reg_1.folio_liq, reg_1.fecha_liq 
             TO n_seguro, n_folio_liq, fecha_ult_ret
    IF aux_pausa matches "[Ss]" THEN 
        PROMPT " PROCESO FINALIZADO... < ENTER > PARA SALIR "
        FOR CHAR enter
    ELSE
        PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR "
	FOR CHAR enter
    END IF

  CLOSE WINDOW retl016
END MAIN


FUNCTION init()
#i-------------
    LET HOY  = TODAY

    SELECT *
    INTO  s_modulo.*
    FROM  seg_modulo
    WHERE modulo_cod = "ret" 

    SELECT codigo_afore, USER
    INTO   s_codigo_afore, g_usuario
    FROM   tab_afore_local

    LET tot_monto_neto = 0
    LET tot_monto_ret  = 0
    LET tot_monto_bruto= 0
    INITIALIZE reg_1.* to NULL
END FUNCTION


FUNCTION primer_paso()
#pp-------------------
   
   LET cont        = 0
   LET monto_neto  = 0
   LET monto_ret   = 0
   LET monto_bruto = 0

   LET g_lista = s_modulo.ruta_listados CLIPPED,"/",
                 HOY USING "DDMMYYYY",".016"

   DISPLAY "  PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)
   SLEEP 1

   START REPORT listado TO g_lista

     LET txt_1="SELECT  a.n_seguro, a.n_folio_liq, a.fecha_ult_ret, ",
                       "a.consecutivo ",
               " FROM   ret_cta_vol a ",
               " WHERE ",x_busca CLIPPED,
               " ORDER BY 3,2,1 "
 
      PREPARE bus_1 FROM txt_1
      DECLARE cur_1 CURSOR FOR bus_1
      FOREACH cur_1 INTO reg_1.*
         LET cont = cont + 1

         DECLARE cur_3 CURSOR FOR 
         SELECT subcuenta
         FROM   dis_cuenta
         WHERE  nss              = reg_1.nss
         AND    fecha_conversion = reg_1.fecha_liq
         AND    folio            = reg_1.folio_liq
         GROUP BY 1

         FOREACH cur_3 INTO vsubcta 
             SELECT SUM(monto_en_pesos)
             INTO   monto_neto
             FROM   dis_cuenta
             WHERE  nss              = reg_1.nss
             AND    subcuenta        = vsubcta
             AND    tipo_movimiento  = 490 
             AND    fecha_conversion = reg_1.fecha_liq
             AND    folio            = reg_1.folio_liq

             SELECT SUM(monto_en_pesos)
             INTO   monto_ret
             FROM   dis_cuenta
             WHERE  nss              = reg_1.nss
             AND    subcuenta        = vsubcta
             AND    tipo_movimiento  = 10 
             AND    fecha_conversion = reg_1.fecha_liq
             AND    folio            = reg_1.folio_liq

             OUTPUT TO REPORT listado(reg_1.*,monto_neto, monto_ret,vsubcta)
         END FOREACH
      END FOREACH
   FINISH REPORT listado
END FUNCTION


REPORT listado(reg_1,monto_neto, monto_ret,vsubcta)
#l-------------------------------------------------

    DEFINE #loc #char 
        pat,mat,nom   CHAR(20),
        nombre_final  CHAR(50),
        L1            CHAR(01),
	L2	      CHAR(02),
        L5            CHAR(05),
        L10           CHAR(10)
        
    DEFINE #loc #decimal
        monto_neto    ,
        monto_ret     ,
        monto_bruto   ,
        precio_dia    DEC(16,6)
        
    DEFINE #loc #integer
        total_nss     INTEGER
    
    DEFINE #loc #smallint
        i             ,
        vsubcta       SMALLINT 
        
    DEFINE reg_1  RECORD
           nss              CHAR(11),
           folio_liq        INTEGER ,
           fecha_liq        DATE    ,
           consecutivo      INTEGER
    END RECORD
   
    
    OUTPUT
        PAGE LENGTH   83
	LEFT MARGIN    0
	RIGHT MARGIN 120
	TOP MARGIN     0
	BOTTOM MARGIN  0

    FORMAT
    PAGE HEADER
 
         LET  L1  = "\304"
         LET  L2  = "\304\304"
         LET  L5  = "\304\304\304\304\304"
         LET  L10 = "\304\304\304\304\304\304\304\304\304\304"

         PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'
         PRINT COLUMN 1,'\033e\033(s218T\033(s12H\033(s7B'

        ## PRINT COLUMN  50, "GERENCIA DE SEGUROS Y RETIROS",

         PRINT COLUMN  48, "G E R E N C I A   D E   R E T I R O S"
         SKIP 1 LINES

         PRINT COLUMN  48, " RETIRO DE APORTACIONES VOLUNTARIAS "
         SKIP 2 LINES

         PRINT COLUMN 106, "PROG.   :    RETL016"

         PRINT
         PRINT COLUMN   1,"TIPO DE OPERACION : REPORTE DE RETIROS DE APORTACIONES VOLUNTARIAS (PESOS)",
               COLUMN 106, "PAGINA  :       ",PAGENO USING "####"
         PRINT
	 PRINT COLUMN 106,"FECHA   : ",TODAY USING "DD/MM/YYYY"

         PRINT COLUMN 1,'\033e\033(s218T\033(s17H\033(s7B'

         PRINT COLUMN 1,"\332",L10,L1,
                         "\302",L10,L10,L10,L10,L5,L1,L1,L1,L1,
                         "\302",L10,
                         "\302",L5,L1,
                         "\302",L10,L5,L1,L1,L1,L1,
                         "\302",L10,L5,L1,L1,L1,L1,
                         "\302",L10,L5,L1,L1,L1,L1,L1,
                         "\302",L5,L1,L1,L1,L1,
                         "\302",L10,L1,L1,
                         "\302",L10,L1,
                         "\277"

         PRINT COLUMN 1,"|    NSS    ",
                        "|               NOMBRE DEL TRABAJADOR             ",
                        "|  PRECIO  ",
                        "|SUBCTA",
                        "|     MONTO NETO    ",
                        "|  MONTO RETENCION  ",
                        "|     MONTO BRUTO    ",
                        "|  FOLIO  ",
                        "|   FECHA    ",
                        "|CONSECUTIVO|"

         PRINT COLUMN 1,"|           ",
                         "|                                                 ",
                         "|  DEL DIA ",
                         "|      ",
                         "|                   ",
                         "|                   ",
                         "|                    ",
                         "|         ",
                         "|LIQUIDACION ",
                         "|           |"

         PRINT COLUMN 1,"\300",L10,L1,
                        "\301",L10,L10,L10,L10,L5,L1,L1,L1,L1,
                        "\301",L10,
                        "\301",L5,L1,
                        "\301",L10,L5,L1,L1,L1,L1,
                        "\301",L10,L5,L1,L1,L1,L1,
                        "\301",L10,L5,L1,L1,L1,L1,L1,
                        "\301",L5,L1,L1,L1,L1,
                        "\301",L10,L1,L1,
                        "\301",L10,L1,
                        "\331"

ON EVERY ROW
    INITIALIZE nombre_final TO NULL
    SELECT paterno,materno,nombres 
    INTO   pat, mat, nom
    FROM   afi_mae_afiliado
    WHERE  n_seguro = reg_1.nss
 
    LET nombre_final = pat CLIPPED," ",mat CLIPPED," ",nom              
                            
    LET monto_bruto = monto_neto + monto_ret           
    LET total_nss = total_nss + 1

    LET tot_monto_neto  = tot_monto_neto + monto_neto
    LET tot_monto_ret   = tot_monto_ret  + monto_ret
    LET tot_monto_bruto = tot_monto_bruto+ monto_bruto

    SELECT precio_del_dia 
    INTO   precio_dia
    FROM   glo_valor_accion
    WHERE  fecha_valuacion = reg_1.fecha_liq 
    AND    codigo_siefore  = 1  

         PRINT
         PRINT
         COLUMN   2,reg_1.nss,
         COLUMN  14,nombre_final,
         COLUMN  65,precio_dia  USING "##.######",
         COLUMN  72,vsubcta,
         COLUMN  82,monto_neto   USING "###########.######",
         COLUMN  102,monto_ret   USING "###########.######",
         COLUMN  122,monto_bruto USING "###########.######",
         COLUMN  131,reg_1.folio_liq USING "##########",
         COLUMN  154,reg_1.fecha_liq USING "DD/MM/YYYY",
         COLUMN  165,reg_1.consecutivo

         IF lineno > 83 THEN    
            SKIP TO TOP OF PAGE 
         END IF                 

ON LAST ROW

         SKIP 3 LINES
         PRINT COLUMN 1,"\332",L10,L1,
                        "\302",L10,L10,L10,L10,L5,L2,L2,
                        "\302",L10,L5,L2,
                        "\302",L10,L5,L2,L2,
                        "\302",L10,L5,L2,L2,
                        "\302",L10,L5,L2,L1,
                        "\302",L10,L1,L10,L1,L1,L1,L10,L1,L1,
                        "\277"

         PRINT
         COLUMN   1,"|", 
         COLUMN  13,"| TOTAL DE REGISTROS : ", total_nss USING "########",
         COLUMN  63,"|",  
         COLUMN  81,"|", COLUMN 82, tot_monto_neto USING "###########.######",
         COLUMN 101,"|", tot_monto_ret   USING "###########.######",
         COLUMN 121,"|", tot_monto_bruto USING "###########.######",
         COLUMN 139,"| ",
         COLUMN 177,"|"

         PRINT COLUMN 1,"\300",L10,L1,
			"\301",L10,L10,L10,L10,L5,L2,L2,
			"\301",L10,L5,L2,
			"\301",L10,L5,L2,L2,
			"\301",L10,L5,L2,L2,
			"\301",L10,L5,L2,L1,
			"\301",L10,L1,L10,L1,L1,L1,L10,L1,L1,
			"\331"

   PAGE TRAILER
      PRINT COLUMN 001,"REPORTE : ",HOY USING "DDMMYYYY",".016"    
END REPORT 

