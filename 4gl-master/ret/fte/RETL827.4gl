################################################################################
#Proyecto          => SISTEMA DE AFORE ( SAFRE )                               #
#Owner             => E.F.P.                                                   #
#Programa RETL827  => REPORTE DE RETIROS POR DESEMPLEO                         #
#Fecha creacion    => 5 JUNIO 2009                                             #
#By                => JGHM APEYRON                                             #
#------------------------------------------------------------------------------#
#Modificacion    => CPL-1922   17-Marzo-2015    Alejandro Chagoya Salazar      #
#Descripcion     => En el rubro "Tipo B", se consideren los retiros tipo A y B #
#                => y con tipo pago igual a dos.                               #
################################################################################

DATABASE safre_af
GLOBALS
   DEFINE pRepDes             DATE
   DEFINE pRepHas             DATE
   DEFINE gc_Afore            CHAR(60)
   DEFINE gc_RutaR            CHAR(100)          
   DEFINE gc_coman            CHAR(100)          
   
   DEFINE gd_TtTt_cas         INTEGER
   DEFINE gd_TtTt_imp         DECIMAL(15,2)
   DEFINE gd_TrTt_cas         INTEGER
   DEFINE gd_TrTt_imp         DECIMAL(15,2)
   DEFINE gd_TrDa_cas         INTEGER
   DEFINE gd_TrDa_imp         DECIMAL(15,2)
   DEFINE gd_TrA1_cas         INTEGER
   DEFINE gd_TrA1_imp         DECIMAL(15,2)
   DEFINE gd_TrB1_cas         INTEGER
   DEFINE gd_TrB1_imp         DECIMAL(15,2)
   DEFINE gd_Tr11_cas         INTEGER
   DEFINE gd_T111_imp         DECIMAL(15,2)
   DEFINE gd_Tr90_cas         INTEGER
   DEFINE gd_T190_imp         DECIMAL(15,2)

   DEFINE gd_CoTt_cas         INTEGER
   DEFINE gd_CoTt_imp         DECIMAL(15,2)
   DEFINE gd_CoA1_cas         INTEGER
   DEFINE gd_CoA1_imp         DECIMAL(15,2)
   DEFINE gd_CoB1_cas         INTEGER
   DEFINE gd_CoB1_imp         DECIMAL(15,2)
   DEFINE gd_Co11_cas         INTEGER
   DEFINE gd_Co11_imp         DECIMAL(15,2)
   DEFINE gd_Co90_cas         INTEGER
   DEFINE gd_Co90_imp         DECIMAL(15,2)

   DEFINE gd_NeTt_cas         INTEGER
   DEFINE gd_NeTt_imp         DECIMAL(15,2)
   DEFINE gd_NeA1_cas         INTEGER
   DEFINE gd_NeA1_imp         DECIMAL(15,2)
   DEFINE gd_NeB1_cas         INTEGER
   DEFINE gd_NeB1_imp         DECIMAL(15,2)
   DEFINE gd_Ne11_cas         INTEGER
   DEFINE gd_Ne11_imp         DECIMAL(15,2)
   DEFINE gd_Ne90_cas         INTEGER
   DEFINE gd_Ne90_imp         DECIMAL(15,2)

   DEFINE enter               CHAR(01)
   DEFINE Hoy                 DATE
   DEFINE fecha               DATE

END GLOBALS


MAIN
   DEFINE li_captura SMALLINT

   DEFER INTERRUPT
   OPTIONS PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY CONTROL-O

    CALL STARTLOG("RETL827.log")
    CALL f_Prepara()
    LET  Hoy   = TODAY 

    OPEN WINDOW f_RETL827 AT 2,2 WITH FORM "RETL8271" ATTRIBUTE(BORDER)
    DISPLAY " < Esc > Ejecutar <Ctrl-C> Salir                                               " AT 1,  1 ATTRIBUTE(REVERSE)
    DISPLAY " RETL827           INFORME DE RETIROS PARCIALES POR DESEMPLEO                  " AT 3,  1 ATTRIBUTE(REVERSE)
    DISPLAY Hoy                                                           USING "DD/MM/YYYY"  AT 3, 65 ATTRIBUTE(REVERSE)

    LET  li_captura = ""
    CALL f_CapturaFecha() RETURNING li_captura

    IF li_captura = 0 THEN
       ERROR "UN MOMENTO, PROCESANDO INFORMACION DEL PERIODO ", pRepDes, " AL ", pRepHas
       CALL  f_GeneraRep() 
    ELSE
       ERROR "NO HAY INFORMACION EN EL PERIODO ....."
       SLEEP 2
    END IF
    ERROR "                                    "
    PROMPT "  TERMINO, ENTER Para Continuar " FOR CHAR enter
    CLOSE WINDOW f_RETL827

END MAIN


FUNCTION f_Prepara()
    DEFINE  lc_query                        CHAR(3000)
    DEFINE  lr_afoloc           RECORD LIKE tab_afore_local.*
    DEFINE  lr_modulo           RECORD LIKE seg_modulo.*

    ---   Obtener la AFORE
    LET     lc_query  = "     SELECT  *                                   ",
                        "       FROM  tab_afore_local                     "
    PREPARE p_selafo            FROM  lc_query 
    EXECUTE p_selafo            INTO  lr_afoloc.*
    LET     gc_afore            =     lr_afoloc.razon_social

    ---   Obtener la ruta del Reporte 
    LET     lc_query  = "     SELECT  *                                   ",
                        "       FROM  seg_modulo                          ",
                        "      WHERE  modulo_cod = 'ret'                  "
    PREPARE p_selrut            FROM  lc_query 
    EXECUTE p_selrut            INTO  lr_modulo.*

    LET     gc_Afore  = lr_afoloc.razon_social
    LET     gc_RutaR  = lr_modulo.ruta_listados 

    --Ruta y nombre del report 
    LET     gc_RutaR  = gc_RutaR CLIPPED,
                        "/RETL827", 
                        YEAR(TODAY) USING "&&&&", MONTH(TODAY) USING "&&", DAY(TODAY) USING "&&", 
                        ".txt"

    LET     lc_query  = "     SELECT ret.nss,                                       ",
                        "            ret.consecutivo,                               ",
                        "            ret.tipo_pago,                                 ",
                        "            SUM(cta.monto_en_pesos)                        ",
                        "       FROM ret_parcial ret,                               ",
                        "            dis_cuenta  cta                                ",
                        "      WHERE ret.nss                = cta.nss               ",
                        "        AND ret.consecutivo        = cta.consecutivo_lote  ",
                        "        AND cta.tipo_movimiento    = 875                   ",
                        "        AND ret.tipo_desempleo     = 'D'                   ",
                        "        AND cta.fecha_conversion   BETWEEN ? AND ?         ",
                        "      GROUP BY 1, 2, 3                                     "
    PREPARE p_seltra            FROM  lc_query 
    DECLARE d_seltra      CURSOR FOR  p_seltra

    LET     lc_query  = "     SELECT ret.nss,                                       ",
                        "            ret.consecutivo,                               ",
                        "            ret.tipo_pago,                                 ",
                        "            SUM(cta.monto_en_pesos)                        ",
                        "       FROM ret_parcial ret,                               ",
                        "            dis_cuenta  cta                                ",
                        "      WHERE ret.nss                = cta.nss               ",
                        "        AND ret.consecutivo        = cta.consecutivo_lote  ",
                        "        AND cta.tipo_movimiento    = 878                   ",
                        "        AND ret.tipo_desempleo     = 'C'                   ",
                        "        AND cta.fecha_conversion   BETWEEN ? AND ?         ",
                        "      GROUP BY 1, 2, 3                                     "
    PREPARE p_selcom            FROM  lc_query 
    DECLARE d_selcom      CURSOR FOR  p_selcom

    LET     lc_query  = "     SELECT COUNT(*)                                       ",
                        "       FROM ret_parcial                                    ",
                        "      WHERE nss                    = ?                     ",
                        "        AND consecutivo            = ?                     ",
                        "        AND (salario_base_cot * 90 ) = pago_desempleo      ",
                        "        AND  salario_base_cot      > 0                     " 
    PREPARE p_chesal            FROM  lc_query 

    LET     lc_query  = "     SELECT ret.nss,                                       ",
                        "            ret.consecutivo,                               ",
                        "            ret.tipo_pago,                                 ",
                        "            SUM(cta.monto_en_pesos)                        ",
                        "       FROM ret_parcial ret,                               ",
                        "            dis_cuenta  cta                                ",
                        "      WHERE ret.nss                = cta.nss               ",
                        "        AND ret.consecutivo        = cta.consecutivo_lote  ",
                        "        AND cta.tipo_movimiento    IN (876, 877)           ",
                        "        AND ret.tipo_desempleo     IN ('A', 'B')           ",
                        "        AND ret.tipo_pago          = 1                     ",
                        "        AND cta.fecha_conversion   BETWEEN ? AND ?         ",
                        "      GROUP BY 1, 2, 3                                     "
    PREPARE p_selneA            FROM  lc_query
    DECLARE d_selneA      CURSOR FOR  p_selneA

    LET     lc_query  = "     SELECT ret.nss,                                       ",
                        "            ret.consecutivo,                               ",
                        "            ret.tipo_pago,                                 ",
                        "            SUM(cta.monto_en_pesos)                        ",
                        "       FROM ret_parcial ret,                               ",
                        "            dis_cuenta  cta                                ",
                        "      WHERE ret.nss                = cta.nss               ",
                        "        AND ret.consecutivo        = cta.consecutivo_lote  ",
                        "        AND cta.tipo_movimiento    IN (876, 877)           ",
                        "        AND ret.tipo_desempleo     IN ('A', 'B')           ", #CPL-1922
                        "        AND ret.tipo_pago          = 2                     ",
                        "        AND cta.fecha_conversion   BETWEEN ? AND ?         ",
                        "      GROUP BY 1, 2, 3                                     "
    PREPARE p_selneB            FROM  lc_query
    DECLARE d_selneB      CURSOR FOR  p_selneB

    LET     lc_query  = "      SELECT COUNT(*)                                      ",
                        "        FROM ret_ctr_pago                                  ",
                        "       WHERE nss                    = ?                    ",
                        "         AND consecutivo            = ?                    ",
                        "         AND mto_1                  = mto_pago             "
    PREPARE p_chene9            FROM  lc_query
END FUNCTION


FUNCTION f_CapturaFecha()
   DEFINE   li_captura                      SMALLINT
   DEFINE   fechades                        DATE
   DEFINE   fechahas                        DATE

   LET      li_captura      =   0

   INPUT BY NAME fechades, fechahas WITHOUT DEFAULTS
      BEFORE INPUT 
         LET  fechades = TODAY
         LET  fechahas = TODAY

      AFTER FIELD fechades
      	 IF fechades IS NULL THEN
      	 	  ERROR "CAPTURE UNA FECHA INICIAL "
      	 	  SLEEP 3
      	 	  ERROR ""
      	 	  NEXT FIELD fechades
      	 END IF

      AFTER FIELD fechahas
      	 IF fechahas IS NULL THEN
      	 	  ERROR "CAPTURE UNA FECHA FINAL "
      	 	  SLEEP 3
      	 	  ERROR ""
      	 	  NEXT FIELD fechahas
      	 END IF

      ON KEY (CONTROL-C, INTERRUPT)
      	 LET li_captura = 1
      	 EXIT INPUT

      ON KEY (ESC)
      	 IF fechahas   <  fechades  THEN
      	 	  ERROR "LA FECHA FINAL NO PUEDE SER MENOR A FECHA INICIAL "
      	 	  SLEEP 3
      	 	  NEXT FIELD fechahas
      	 END IF
      	 LET li_captura = 0
      	 EXIT INPUT
   END INPUT

   LET    pRepDes   = fechades
   LET    pRepHas   = fechahas
   RETURN li_captura
END FUNCTION


FUNCTION f_GeneraRep() 
    DEFINE lr_dat_tra         RECORD
           nss                LIKE ret_parcial.nss,
           consecutivo        LIKE ret_parcial.consecutivo,
           tipo_pago          LIKE ret_parcial.tipo_pago,
           monto_en_pesos     LIKE dis_cuenta.monto_en_pesos
    END RECORD
    DEFINE ls_cuantos         INTEGER

    LET  gd_TtTt_cas         =   0
    LET  gd_TtTt_imp         =   0
    LET  gd_TrTt_cas         =   0
    LET  gd_TrTt_imp         =   0
    LET  gd_TrDa_cas         =   0
    LET  gd_TrDa_imp         =   0
    LET  gd_TrA1_cas         =   0
    LET  gd_TrA1_imp         =   0
    LET  gd_TrB1_cas         =   0
    LET  gd_TrB1_imp         =   0
    LET  gd_Tr11_cas         =   0
    LET  gd_T111_imp         =   0
    LET  gd_Tr90_cas         =   0
    LET  gd_T190_imp         =   0

    LET  gd_CoTt_cas         =   0
    LET  gd_CoTt_imp         =   0
    LET  gd_CoA1_cas         =   0
    LET  gd_CoA1_imp         =   0
    LET  gd_CoB1_cas         =   0
    LET  gd_CoB1_imp         =   0
    LET  gd_Co11_cas         =   0
    LET  gd_Co11_imp         =   0
    LET  gd_Co90_cas         =   0
    LET  gd_Co90_imp         =   0

    LET  gd_NeTt_cas         =   0
    LET  gd_NeTt_imp         =   0
    LET  gd_NeA1_cas         =   0
    LET  gd_NeA1_imp         =   0
    LET  gd_NeB1_cas         =   0
    LET  gd_NeB1_imp         =   0
    LET  gd_Ne11_cas         =   0
    LET  gd_Ne11_imp         =   0
    LET  gd_Ne90_cas         =   0
    LET  gd_Ne90_imp         =   0

    ----- Foreach para TRANSITO
    FOREACH d_seltra   USING  pRepDes, pRepHas
                        INTO  lr_dat_tra.* 
       IF   lr_dat_tra.tipo_pago      =   1   THEN 
            --- Acumula los TA
            LET        gd_TrA1_cas    =   gd_TrA1_cas +   1
            LET        gd_TrA1_imp    =   gd_TrA1_imp +   lr_dat_tra.monto_en_pesos
       END IF
       IF   lr_dat_tra.tipo_pago      =   2   THEN 
            --- Acumula los TB
            LET        gd_TrB1_cas    =   gd_TrB1_cas +   1
            LET        gd_TrB1_imp    =   gd_TrB1_imp +   lr_dat_tra.monto_en_pesos

            --- Aqui hay q hacer otra pregunta o busqueda para subdividir 
            LET        ls_cuantos     = 0
            EXECUTE    p_chesal                       USING lr_dat_tra.nss,
                                                            lr_dat_tra.consecutivo
                                                       INTO ls_cuantos

            IF         ls_cuantos     >  0 THEN 
                 LET        gd_Tr90_cas    =   gd_Tr90_cas +   1
                 LET        gd_T190_imp    =   gd_T190_imp +   lr_dat_tra.monto_en_pesos
            ELSE
                 LET        gd_Tr11_cas    =   gd_Tr11_cas +   1
                 LET        gd_T111_imp    =   gd_T111_imp +   lr_dat_tra.monto_en_pesos
            END IF

       END IF
       IF   lr_dat_tra.tipo_pago      =   3   THEN 
            --- Acumula los DA
            LET        gd_TrDa_cas    =   gd_TrDa_cas +   1
            LET        gd_TrDa_imp    =   gd_TrDa_imp +   lr_dat_tra.monto_en_pesos
       END IF
       LET             gd_TrTt_cas    =   gd_TrTt_cas +   1
       LET             gd_TrTt_imp    =   gd_TrTt_imp +   lr_dat_tra.monto_en_pesos
    END FOREACH

    ----- Foreach para COMPLEMENTARIOS --- 
    FOREACH d_selcom       USING  pRepDes, pRepHas
                            INTO  lr_dat_tra.* 
       IF   lr_dat_tra.tipo_pago      =   1   THEN 
            --- Acumula los A  
            LET        gd_CoA1_cas    =   gd_CoA1_cas +   1
            LET        gd_CoA1_imp    =   gd_CoA1_imp +   lr_dat_tra.monto_en_pesos
       END IF
       IF   lr_dat_tra.tipo_pago      =   2   THEN 
            --- Acumula los B
            LET        gd_CoB1_cas    =   gd_CoB1_cas +   1
            LET        gd_CoB1_imp    =   gd_CoB1_imp +   lr_dat_tra.monto_en_pesos

            --- Aqui hay q hacer otra pregunta o busqueda para subdividir 
            LET        ls_cuantos     = 0
            EXECUTE    p_chesal                       USING lr_dat_tra.nss,
                                                            lr_dat_tra.consecutivo
                                                       INTO ls_cuantos

            IF         ls_cuantos     >  0 THEN 
                 LET        gd_Co90_cas    =   gd_Co90_cas +   1
                 LET        gd_Co90_imp    =   gd_Co90_imp +   lr_dat_tra.monto_en_pesos
            ELSE
                 LET        gd_Co11_cas    =   gd_Co11_cas +   1
                 LET        gd_Co11_imp    =   gd_Co11_imp +   lr_dat_tra.monto_en_pesos
            END IF

       END IF
       LET             gd_CoTt_cas    =   gd_CoTt_cas +   1
       LET             gd_CoTt_imp    =   gd_CoTt_imp +   lr_dat_tra.monto_en_pesos
    END FOREACH 

    ----- Foreach Nuevo criterio A1
    FOREACH d_selneA   USING  pRepDes, pRepHas
                        INTO  lr_dat_tra.*
       LET             gd_NeTt_cas    =   gd_NeTt_cas +   1
       LET             gd_NeTt_imp    =   gd_NeTt_imp +   lr_dat_tra.monto_en_pesos
       LET             gd_NeA1_cas    =   gd_NeA1_cas +   1 
       LET             gd_NeA1_imp    =   gd_NeA1_imp +   lr_dat_tra.monto_en_pesos 
    END FOREACH

    ----- Foreach Nuevo criterio B1
    FOREACH d_selneB   USING  pRepDes, pRepHas
                        INTO  lr_dat_tra.*
       LET             gd_NeTt_cas    =   gd_NeTt_cas +   1
       LET             gd_NeTt_imp    =   gd_NeTt_imp +   lr_dat_tra.monto_en_pesos
       LET             gd_NeB1_cas    =   gd_NeB1_cas +   1 
       LET             gd_NeB1_imp    =   gd_NeB1_imp +   lr_dat_tra.monto_en_pesos 

       LET        ls_cuantos     = 0
       EXECUTE    p_chene9                       USING lr_dat_tra.nss,
                                                       lr_dat_tra.consecutivo
                                                  INTO ls_cuantos

       IF         ls_cuantos     >  0 THEN 
            LET        gd_Ne90_cas    =   gd_Ne90_cas +   1
            LET        gd_Ne90_imp    =   gd_Ne90_imp +   lr_dat_tra.monto_en_pesos
       ELSE
            LET        gd_Ne11_cas    =   gd_Ne11_cas +   1
            LET        gd_Ne11_imp    =   gd_Ne11_imp +   lr_dat_tra.monto_en_pesos
       END IF
    END FOREACH

    ---- Acumula el total General
    LET     gd_TtTt_cas          = gd_TtTt_cas + gd_TrTt_cas + gd_CoTt_cas + gd_NeTt_cas 
    LET     gd_TtTt_imp          = gd_TtTt_imp + gd_TrTt_imp + gd_CoTt_imp + gd_NeTt_imp 

    -- Despliega datos
    CALL F_despliega()

END FUNCTION


FUNCTION f_despliega()

    OPEN WINDOW f_RETL8272 AT 2,2 WITH FORM "RETL8272" ATTRIBUTE(BORDER)
    DISPLAY " < Ctrl-T > Imprimir <Ctrl-C> Salir                                            " AT 1,  1 ATTRIBUTE(REVERSE)
    DISPLAY " RETL827           RETIROS PARCIALES POR DESEMPLEO                             " AT 3,  1 ATTRIBUTE(REVERSE)
    DISPLAY Hoy                                                           USING "DD/MM/YYYY"  AT 3, 65 ATTRIBUTE(REVERSE)
    DISPLAY pRepDes                                                       USING "DD/MM/YYYY"  AT 4, 31 ATTRIBUTE(REVERSE)
    DISPLAY pRepHas                                                       USING "DD/MM/YYYY"  AT 4, 47 ATTRIBUTE(REVERSE)

    DISPLAY   gd_TrDa_cas, gd_TrDa_imp, gd_TrA1_cas, gd_TrA1_imp,
              gd_TrB1_cas, gd_TrB1_imp, gd_Tr11_cas, gd_T111_imp,
              gd_Tr90_cas, gd_T190_imp,
              gd_CoA1_cas, gd_CoA1_imp, gd_CoB1_cas, gd_CoB1_imp, 
              gd_Co11_cas, gd_Co11_imp, gd_Co90_cas, gd_Co90_imp,
              gd_NeA1_cas, gd_NeA1_imp, gd_NeB1_cas, gd_NeB1_imp, 
              gd_Ne11_cas, gd_Ne11_imp, gd_Ne90_cas, gd_Ne90_imp,

              gd_TrTt_cas, gd_TrTt_imp, gd_CoTt_cas, gd_CoTt_imp,
              gd_NeTt_cas, gd_NeTt_imp, gd_TtTt_cas, gd_TtTt_imp
         TO   TrDacas, TrDaimp, TrTacas, TrTaimp,
              TrTbcas, TrTbimp, Tr11cas, Tr11imp,
              Tr90cas, Tr90imp,
              CoTacas, CoTaimp, CoTbcas, CoTbimp, 
              Co11cas, Co11imp, Co90cas, Co90imp,
              NeTacas, NeTaimp, NeTbcas, NeTbimp, 
              Ne11cas, Ne11imp, Ne90cas, Ne90imp,

              TrTtcas, TrTtimp, CoTtcas, CoTtimp,
              NeTtcas, NeTtimp, TtTtcas, TtTtimp

    MENU "RETL827"

         COMMAND KEY ("I") "Imprimir" "Geenerar Impresión"
            --- Inicia el reporte 
            START REPORT r_RetParDes TO gc_RutaR
            OUTPUT TO REPORT r_RetParDes()
            FINISH REPORT r_RetParDes
            ERROR "ARCHIVO GENERADO: ", gc_RutaR CLIPPED          
            SLEEP 6
            LET     gc_coman   = " lp ", gc_RutaR
            RUN gc_coman
            ERROR "SE GENERO LA IMPRESION "     

         COMMAND KEY ("S") "Salir" "Terminar Proceso"
              EXIT MENU

    END MENU
    CLOSE WINDOW f_RETL8272 
END FUNCTION


REPORT r_RetParDes()

   OUTPUT
      TOP MARGIN    0
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   5

   FORMAT
      FIRST PAGE HEADER


         PRINT COLUMN 01, "                   RETIROS PARCIALES POR DESEMPLEO ", "                   ",
                          Hoy USING "DD/MM/YYYY"
         PRINT COLUMN 01, "  ", gc_afore 
         PRINT COLUMN 01, "  ", "Semana del : ", pRepDes        USING "DD/MM/YYYY",
                          " Al : ", pRepHas        USING "DD/MM/YYYY"
         PRINT COLUMN 01, "                                         ", "        Monto          Trabajadores"

      ON EVERY ROW

         PRINT COLUMN 01, "                                "
         PRINT COLUMN 01, " EN TRANSITO                    ",  
                          gd_TrTt_imp               USING  "###,###,###,###,##&.&&",  "         ",
                          gd_TrTt_cas               USING  "#,###,###,##&"
         PRINT COLUMN 01, "   Derecho Anterior             ",  
                          gd_TrDa_imp               USING  "###,###,###,###,##&.&&",  "         ",
                          gd_TrDa_cas               USING  "#,###,###,##&"
         PRINT COLUMN 01, "   Tipo A                       ",  
                          gd_TrA1_imp               USING  "###,###,###,###,##&.&&",  "         ",
                          gd_TrA1_cas               USING  "#,###,###,##&"
         PRINT COLUMN 01, "   Tipo B                       ",  
                          gd_TrB1_imp               USING  "###,###,###,###,##&.&&",  "         ",
                          gd_TrB1_cas               USING  "#,###,###,##&"
         PRINT COLUMN 01, "     11.5% Saldo RCV            ",  
                          gd_T111_imp               USING  "###,###,###,###,##&.&&",  "         ",
                          gd_Tr11_cas               USING  "#,###,###,##&"
         PRINT COLUMN 01, "     90 días SBC                ",  
                          gd_T190_imp               USING  "###,###,###,###,##&.&&",  "         ", 
                          gd_Tr90_cas               USING  "#,###,###,##&"

         PRINT COLUMN 01, "                                "
         PRINT COLUMN 01, " COMPLEMENTARIOS                ",  
                          gd_CoTt_imp               USING  "###,###,###,###,##&.&&",  "         ", 
                          gd_CoTt_cas               USING  "#,###,###,##&"
         PRINT COLUMN 01, "   Tipo A                       ",  
                          gd_CoA1_imp               USING  "###,###,###,###,##&.&&",  "         ", 
                          gd_CoA1_cas               USING  "#,###,###,##&"
         PRINT COLUMN 01, "   Tipo B                       ",  
                          gd_CoB1_imp               USING  "###,###,###,###,##&.&&",  "         ",
                          gd_CoB1_cas               USING  "#,###,###,##&"
         PRINT COLUMN 01, "     11.5% Saldo RCV            ",  
                          gd_Co11_imp               USING  "###,###,###,###,##&.&&",  "         ",
                          gd_Co11_cas               USING  "#,###,###,##&"
         PRINT COLUMN 01, "     90 días SBC                ",  
                          gd_Co90_imp               USING  "###,###,###,###,##&.&&",  "         ",
                          gd_Co90_cas               USING  "#,###,###,##&"

         PRINT COLUMN 01, "                                "
         PRINT COLUMN 01, " NUEVO ESQUEMA                  ",  
                          gd_NeTt_imp               USING  "###,###,###,###,##&.&&",  "         ", 
                          gd_NeTt_cas               USING  "#,###,###,##&"
         PRINT COLUMN 01, "   Tipo A                       ",  
                          gd_NeA1_imp               USING  "###,###,###,###,##&.&&",  "         ", 
                          gd_NeA1_cas               USING  "#,###,###,##&"
         PRINT COLUMN 01, "   Tipo B                       ",  
                          gd_NeB1_imp               USING  "###,###,###,###,##&.&&",  "         ", 
                          gd_NeB1_cas               USING  "#,###,###,##&"
         PRINT COLUMN 01, "     11.5% Saldo RCV            ",  
                          gd_Ne11_imp               USING  "###,###,###,###,##&.&&",  "         ", 
                          gd_Ne11_cas               USING  "#,###,###,##&"
         PRINT COLUMN 01, "     90 días SBC                ",  
                          gd_Ne90_imp               USING  "###,###,###,###,##&.&&",  "         ",
                          gd_Ne90_cas               USING  "#,###,###,##&"

      ON LAST ROW
         PRINT COLUMN 01, "                   ",
                          gd_TtTt_imp               USING  "###,###,###,###,##&.&&",  "         ",
                          gd_TtTt_cas               USING  "#,###,###,##&"
END REPORT
