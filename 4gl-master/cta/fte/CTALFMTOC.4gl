#*************************************************************************
#*******
#******* PROGRAMA : CTAB1131.4gl
#*******
#******* OBJETIVO : Saldo de NSS en proceso de Transferencia de Decimos
#*******            Registros identificados como en proceso de transferencia
#*******            y que aun no se ha concluido dicha  transferencia.
#*******            ANEXO C
#*******
#******* ELABORO  : Claudia U.
#******* FECHA    : 25-JULIO-2005
#*******
#*************************************************************************


DATABASE safre_af


DEFINE p_fecha_corte      ,
       g_fecha_provision  DATE

DEFINE p_folio            ,
       v_ctas             INTEGER

DEFINE g_siefore_sb2      SMALLINT

DEFINE g_precio_sb1       ,
       g_precio_sb2       DECIMAL(19,14)

DEFINE g_ruta_listado     CHAR(40) ,
       g_listado          CHAR(100),        
       g_imprime          CHAR(150),        
       g_usuario          CHAR(12) ,
       g_cve_afore        SMALLINT ,
       g_nom_afore        CHAR(50) 

DEFINE reg_saldo_grupo    RECORD
       acc_rcv_sb1            DECIMAL(24,6),
       acc_sar92_sb1          DECIMAL(24,6),
       acc_sarissste_sb1      DECIMAL(24,6),
       acc_vol_sb1            DECIMAL(24,6),
       acc_acr_sb1            DECIMAL(24,6),
       acc_rcv_sb2            DECIMAL(24,6),
       acc_sar92_sb2          DECIMAL(24,6),
       acc_sarissste_sb2      DECIMAL(24,6),
       acc_acr_sb2            DECIMAL(24,6)
       END RECORD

DEFINE reg_saldo_sb1      RECORD
       acc_rcv            DECIMAL(24,6),
       acc_sar92          DECIMAL(24,6),
       acc_sarissste      DECIMAL(24,6),
       acc_vol            DECIMAL(24,6),
       acc_acr            DECIMAL(24,6)
       END RECORD

DEFINE v_acc_vol_sb1      DECIMAL(24,6)

DEFINE reg_saldo_sb2      RECORD
       acc_rcv            DECIMAL(24,6),
       acc_sar92          DECIMAL(24,6),
       acc_sarissste      DECIMAL(24,6),
       acc_vol            DECIMAL(24,6),
       acc_acr            DECIMAL(24,6)
       END RECORD

MAIN
    LET p_folio = ARG_VAL(1)
    LET p_fecha_corte = ARG_VAL(2)

    CALL fn_inicia_variables ()
    CALL fn_formato_c ( p_fecha_corte ) 
    LET g_imprime = "lp ",g_listado CLIPPED
    RUN g_imprime

END MAIN

FUNCTION fn_inicia_variables ()

DEFINE   v_saldo_sub_sie     CHAR(100)


LET v_saldo_sub_sie = "EXECUTE FUNCTION fn_saldo_sub_sie ( ?,?,?,? )"

LET g_siefore_sb2   =   2

PREPARE eje_saldo_sie  FROM  v_saldo_sub_sie

SELECT ruta_listados,
       USER
  INTO g_ruta_listado,
       g_usuario
  FROM safre_af:seg_modulo
 WHERE modulo_cod = "cta"

SELECT codigo_afore,
       razon_social
  INTO g_cve_afore,
       g_nom_afore
  FROM tab_afore_local

LET g_listado = g_ruta_listado CLIPPED,"/",g_usuario CLIPPED,
                     ".FORMATO-C.",TODAY USING "DDMMYYYY"


END FUNCTION


FUNCTION fn_formato_c( l_fecha_corte )

DEFINE  l_fecha_corte     DATE
DEFINE  reg_decimo        RECORD
        no_transferencia  SMALLINT,
        nss               CHAR(11),
        subcuenta         SMALLINT,
        siefore           SMALLINT,
        monto_en_acciones      DECIMAL(24,6),
        fecha_conversion  DATE         ,
        acciones_sb2      DECIMAL(24,6)
        END RECORD

DEFINE  r_subcuenta        SMALLINT

DEFINE  r_saldo_acciones   ,
        r_saldo_pesos      DECIMAL(22,6)

---  Identifica  NSS
      SELECT UNIQUE cs.nss, cs.no_transferencia
        FROM cta_his_decimo cs
       WHERE cs.folio = p_folio
        INTO TEMP nss_decimos

   DECLARE c_iden  CURSOR FOR
      SELECT cs.no_transferencia,
             cs.nss             , 
             t.subcuenta       , 
             t.siefore         , 
             t.monto_en_acciones,
             t.fecha_conversion 
        FROM nss_decimos   cs,
             safre_tmp:tmp_saldo_corte t,
             safre_tmp:cta_formato_nss a
       WHERE cs.nss = a.nss
         AND cs.nss = t.nss
         AND t.subcuenta NOT IN  (4,8,14)    
         AND t.siefore = 1
       ORDER BY 1,2

   START REPORT r_decimos TO g_listado

   FOREACH c_iden INTO reg_decimo.*

      DECLARE c_saldo CURSOR FOR eje_saldo_sie

--  Obtiene el saldo actual de cada subcuenta  en SB2

         OPEN c_saldo USING reg_decimo.nss,
                            reg_decimo.subcuenta,
                            g_siefore_sb2       ,
                            reg_decimo.fecha_conversion
         FETCH c_saldo INTO r_subcuenta,
                            r_saldo_acciones,
                            r_saldo_pesos

         IF r_saldo_acciones IS NULL THEN
            LET r_saldo_acciones = 0
         END IF

         IF r_saldo_pesos IS NULL THEN
            LET r_saldo_pesos = 0
         END IF
       CLOSE c_saldo
       FREE  c_saldo

   LET reg_decimo.acciones_sb2  = r_saldo_acciones

   OUTPUT TO REPORT r_decimos ( reg_decimo.*)

   END FOREACH
   
   CLOSE c_iden
   FREE  c_iden

   FINISH REPORT r_decimos

END FUNCTION


REPORT r_decimos ( reg_decimo )

DEFINE  reg_decimo        RECORD
        no_transferencia  SMALLINT,
        nss               CHAR(11),
        subcuenta         SMALLINT,
        siefore           SMALLINT,
        monto_en_acciones      DECIMAL(24,6),
        fecha_conversion  DATE         ,
        acciones_sb2      DECIMAL(24,6)
        END RECORD

OUTPUT
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0
        PAGE LENGTH 40

ORDER EXTERNAL BY reg_decimo.no_transferencia,
                  reg_decimo.nss        

FORMAT
PAGE HEADER
    SELECT precio_del_dia
      INTO g_precio_sb1
      FROM glo_valor_accion
     WHERE codigo_siefore  = 1
       AND fecha_valuacion = reg_decimo.fecha_conversion

    SELECT precio_del_dia
      INTO g_precio_sb2
      FROM glo_valor_accion
     WHERE codigo_siefore  = 2
       AND fecha_valuacion = reg_decimo.fecha_conversion

    PRINT '\033e\033(1OU\033&l1O\033&k2S\033&l18d\033(s18H'
    PRINT COLUMN 1, "__________________________________________________",
                    "__________________________________________________",
                    "__________________________________________________",
                    "__________________________________________________"

    PRINT COLUMN 1 , "ANEXO C",
          COLUMN 147 , "PRECIO SB1 :",g_precio_sb1 USING "#####.##############"
    PRINT COLUMN 147 , "PRECIO SB2 :",g_precio_sb2 USING "#####.##############"
    PRINT COLUMN 147 , "FECHA      :    ",TODAY USING "DD/MM/YYYY"
    PRINT
    PRINT COLUMN 64, "SALDO DE TRABAJADORES EN PROCESO DE DECIMOS"
    PRINT
    PRINT COLUMN 1, "__________________________________________________",
                    "__________________________________________________",
                    "__________________________________________________",
                    "__________________________________________________"
    PRINT
    PRINT COLUMN 1  ,"DECIMO"          ,
          COLUMN 8  ,"No DE CUENTAS"   ,
          COLUMN 65 ,"SB1"             ,
          COLUMN 141,"SB2"
    PRINT COLUMN 32 ,"RCV"             ,
          COLUMN 49 ,"SAR92"           ,
          COLUMN 62 ,"SARISSSTE "      ,
          COLUMN 86 ,"AV"              ,
          COLUMN 103 ,"ACR"            ,
          COLUMN 123,"RCV"             ,
          COLUMN 140,"SAR92 "          ,
          COLUMN 153,"SARISSSTE"       ,
          COLUMN 176,"ACR "
    PRINT COLUMN 1, "__________________________________________________",
                    "__________________________________________________",
                    "__________________________________________________",
                    "__________________________________________________"

BEFORE GROUP OF reg_decimo.nss

    LET reg_saldo_grupo.acc_rcv_sb1       = 0
    LET reg_saldo_grupo.acc_sar92_sb1     = 0
    LET reg_saldo_grupo.acc_sarissste_sb1 = 0
    LET reg_saldo_grupo.acc_vol_sb1       = 0
    LET reg_saldo_grupo.acc_acr_sb1       = 0

    LET reg_saldo_grupo.acc_rcv_sb2       = 0
    LET reg_saldo_grupo.acc_sar92_sb2     = 0
    LET reg_saldo_grupo.acc_sarissste_sb2 = 0
    LET reg_saldo_grupo.acc_acr_sb2       = 0

BEFORE GROUP OF reg_decimo.no_transferencia

    LET reg_saldo_sb1.acc_rcv         = 0
    LET reg_saldo_sb1.acc_sar92       = 0
    LET reg_saldo_sb1.acc_sarissste   = 0
    LET reg_saldo_sb1.acc_vol         = 0
    LET reg_saldo_sb1.acc_acr         = 0

    LET reg_saldo_sb2.* = reg_saldo_sb1.*
    LET v_ctas = 0

ON EVERY ROW
   CASE
        WHEN reg_decimo.subcuenta = 1 OR
             reg_decimo.subcuenta = 2 OR
             reg_decimo.subcuenta = 5 OR
             reg_decimo.subcuenta = 6 OR
             reg_decimo.subcuenta = 9
          

             LET reg_saldo_grupo.acc_rcv_sb1 = 
                 reg_saldo_grupo.acc_rcv_sb1 + reg_decimo.monto_en_acciones

             LET reg_saldo_grupo.acc_rcv_sb2 = 
                 reg_saldo_grupo.acc_rcv_sb2 + reg_decimo.acciones_sb2

        WHEN reg_decimo.subcuenta = 7

             LET reg_saldo_grupo.acc_sar92_sb1 = 
                 reg_saldo_grupo.acc_sar92_sb1 + reg_decimo.monto_en_acciones

             LET reg_saldo_grupo.acc_sar92_sb2 = 
                 reg_saldo_grupo.acc_sar92_sb2 + reg_decimo.acciones_sb2

        WHEN reg_decimo.subcuenta = 11 OR
             reg_decimo.subcuenta = 12

             LET reg_saldo_grupo.acc_acr_sb1  = 
                 reg_saldo_grupo.acc_acr_sb1 + reg_decimo.monto_en_acciones

             LET reg_saldo_grupo.acc_acr_sb2  = 
                 reg_saldo_grupo.acc_acr_sb2 + reg_decimo.acciones_sb2

        WHEN reg_decimo.subcuenta = 3 OR
             reg_decimo.subcuenta = 10

             LET reg_saldo_grupo.acc_vol_sb1  = 
                 reg_saldo_grupo.acc_vol_sb1 + reg_decimo.monto_en_acciones

        WHEN reg_decimo.subcuenta = 13

             LET reg_saldo_grupo.acc_sarissste_sb1 = 
                 reg_saldo_grupo.acc_sarissste_sb1 + reg_decimo.monto_en_acciones

             LET reg_saldo_grupo.acc_sarissste_sb2 = 
                 reg_saldo_grupo.acc_sarissste_sb2 + reg_decimo.acciones_sb2
    END CASE


AFTER GROUP OF reg_decimo.nss

       LET v_acc_vol_sb1 = 0

       SELECT sum(c.monto_en_acciones)
         INTO v_acc_vol_sb1
         FROM safre_tmp:tmp_saldo_corte c
        WHERE c.nss = reg_decimo.nss
          AND c.subcuenta in (3,10)

       IF v_acc_vol_sb1 IS NULL THEN
          LET v_acc_vol_sb1 = 0
       END IF

   ---  Subtotal por tipo_transferencia
       LET reg_saldo_sb1.acc_rcv        = reg_saldo_sb1.acc_rcv +
                                         reg_saldo_grupo.acc_rcv_sb1
       LET reg_saldo_sb1.acc_sar92     = reg_saldo_sb1.acc_sar92 +
                                         reg_saldo_grupo.acc_sar92_sb1
       LET reg_saldo_sb1.acc_sarissste = reg_saldo_sb1.acc_sarissste +
                                         reg_saldo_grupo.acc_sarissste_sb1
       LET reg_saldo_sb1.acc_vol       = reg_saldo_sb1.acc_vol +
                                         v_acc_vol_sb1
       LET reg_saldo_sb1.acc_acr       = reg_saldo_sb1.acc_acr +
                                         reg_saldo_grupo.acc_acr_sb1

       LET reg_saldo_sb2.acc_rcv       = reg_saldo_sb2.acc_rcv +
                                         reg_saldo_grupo.acc_rcv_sb2
       LET reg_saldo_sb2.acc_sar92     = reg_saldo_sb2.acc_sar92 +
                                         reg_saldo_grupo.acc_sar92_sb2
       LET reg_saldo_sb2.acc_sarissste = reg_saldo_sb2.acc_sarissste +
                                         reg_saldo_grupo.acc_sarissste_sb2
       LET reg_saldo_sb2.acc_acr       = reg_saldo_sb2.acc_acr +
                                         reg_saldo_grupo.acc_acr_sb2

       LET v_ctas = v_ctas + 1

AFTER GROUP OF reg_decimo.no_transferencia

    PRINT COLUMN 1 , reg_decimo.no_transferencia   USING "#&",
          COLUMN 13, v_ctas                        USING "#######&",
          COLUMN 18, reg_saldo_sb1.acc_rcv         USING "###########&.&&&&&&",
          COLUMN 37, reg_saldo_sb1.acc_sar92       USING "###########&.&&&&&&",
          COLUMN 56, reg_saldo_sb1.acc_sarissste   USING "#########&.&&&&&&",
          COLUMN 72, reg_saldo_sb1.acc_vol         USING "##########&.&&&&&&",
          COLUMN 90, reg_saldo_sb1.acc_acr         USING "##########&.&&&&&&",
          COLUMN 109,reg_saldo_sb2.acc_rcv         USING "###########&.&&&&&&",
          COLUMN 128,reg_saldo_sb2.acc_sar92       USING "###########&.&&&&&&",
          COLUMN 147,reg_saldo_sb2.acc_sarissste   USING "#########&.&&&&&&",
          COLUMN 163,reg_saldo_sb2.acc_acr         USING "##########&.&&&&&&"

ON LAST ROW
    PRINT COLUMN 1, "__________________________________________________",
                    "__________________________________________________",
                    "__________________________________________________",
                    "__________________________________________________"

END REPORT
