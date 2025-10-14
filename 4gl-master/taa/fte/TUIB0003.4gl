###########################################################################
#Proyecto           => AFORE ( MEXICO )                                   #
#Propietario        => E.F.P.                                             #
#Programa TUIB0003  => LIQUIDACIÓN  TRASPASO POR UNIFICACIÓN  ISSSTE-IMSS #
#Autor              => JOSE FRANCISCO LUGO CORNEJO                        #
#Fecha Elaboración  => 25 OCTUBRE  2010                                   #
#Sistema            => TUIB (TRASPASO POR POR UNIFICACIÓN  ISSSTE-IMSS)   #
###########################################################################

DATABASE   safre_af

GLOBALS
   DEFINE
            g_today                         DATE
   DEFINE
            g_siefore                       ,
            g_siefore_max                   SMALLINT
   DEFINE
            g_procesados                    INTEGER
   DEFINE
            g_nss                           CHAR(011),
            g_enter                         CHAR(001),
            g_ejecuta                       CHAR(300)

   DEFINE  reg_tui_ctr_folio            RECORD  LIKE  tui_ctr_folio.*
   DEFINE  g_valor_sie        ARRAY[50]    OF   DEC(19,14)

END GLOBALS

GLOBALS "GLOB_REPS.4gl"

MAIN
   OPTIONS
            PROMPT   LINE   LAST,
            ACCEPT   KEY    CONTROL-I
            DEFER    INTERRUPT
   CALL     STARTLOG('TUIB0003.log')
   CALL     F_001_inicio()
   CALL     F_500_proceso()
END MAIN

FUNCTION    F_001_inicio()
   LET      g_today                  =  TODAY
   SELECT   *,user
     INTO   g_seg_modulo.*,g_usuario
     FROM   seg_modulo
    WHERE   modulo_cod               =  'taa';
   SELECT   MAX(codigo_siefore)   INTO  g_siefore_max  
     FROM   safre_af:tab_siefore_local
    WHERE   codigo_siefore         NOT  IN(0,11);    
END FUNCTION

FUNCTION    F_500_proceso()
   DEFINE   l_texto_query                 CHAR(500)
   OPEN     WINDOW    TUIB0003   AT 2,2  WITH  FORM  "TUIB0003"  ATTRIBUTE(BORDER)
   DISPLAY  " <Esc> Continuar     < CTRL-C > Salir                        ",
            "                         "   AT  1,1   ATTRIBUTE(REVERSE)
   DISPLAY  g_today    USING  "DD-MM-YYYY","           "   AT  1,67 ATTRIBUTE(REVERSE)
   DISPLAY  "TUIB0003   LIQUIDA SALDOS DE TRASPASO POR UNIFICACIÓN ISSSTE A IMSS           ",
            "                  "  AT  3,1  ATTRIBUTE(REVERSE)
   DISPLAY  "           <<    PRECIOS DEL DIA DEL RÉGIMEN DE INVERSIÓN     >>                "   AT  12,1 ATTRIBUTE(REVERSE)
   DECLARE  cur_liqui       CURSOR    FOR      
   SELECT   a.*
     FROM   tui_ctr_folio a
    WHERE   a.fecha_liquidacion    BETWEEN  g_today    AND   g_today  + 5
      AND   a.estado                  =  102
   FOREACH  cur_liqui      INTO   reg_tui_ctr_folio.*
             SELECT   COUNT(*)
               INTO   g_total_cuentas
               FROM   tui_sdo_uni_issste
              WHERE   folio              =  reg_tui_ctr_folio.folio
                AND   estado             =  102
            DISPLAY  g_total_cuentas                TO  FORMONLY.l_registros
            DISPLAY  reg_tui_ctr_folio.folio     TO  FORMONLY.folio
            DISPLAY  reg_tui_ctr_folio.fecha_presentacion    TO
                     FORMONLY.fecha_presentacion
            DISPLAY  reg_tui_ctr_folio.fecha_provision       TO
                     FORMONLY.fecha_provision
            DISPLAY  reg_tui_ctr_folio.fecha_liquidacion     TO
                     FORMONLY.fecha_liquidacion 
            CALL     F_010_trae_valor_accion()
            WHILE    TRUE
                     PROMPT   "ESTA SEGURO DE GENERAR EL PROCESO [S/N] ? " FOR g_enter
                     IF       g_enter    MATCHES    "[sS]"      THEN
                              EXIT     FOREACH
                     ELSE
                              LET      reg_tui_ctr_folio.folio     =  0
                     END IF
                     CONTINUE  FOREACH
            END WHILE
   END FOREACH
   IF       reg_tui_ctr_folio.folio              IS  NULL   OR
            reg_tui_ctr_folio.folio               =  0      THEN
            PROMPT   "  No hay folio para Liquidar  Teclee <<Enter para Salir>>" 
                     FOR  g_enter
            EXIT  PROGRAM
   END IF
   DISPLAY  "  PROCESANDO  INFORMACION  ......                                "
               AT   19,1   ATTRIBUTE(REVERSE)
   CALL     F_520_liquida()
   CALL     F_530_arma_globales_para_reporte()
   CALL     genera_reporte()
   DISPLAY  "                                                                  "
               AT   18,1
   UPDATE   tui_ctr_folio
      SET   estado                     =  103
    WHERE   folio                      =  reg_tui_ctr_folio.folio
      AND   estado                     =  102;
   PROMPT  "  PROCESO FINALIZADO, TECLEE  <Enter>  Para Salir         "
            ATTRIBUTE(REVERSE)     FOR    g_enter
   CLOSE  WINDOW   TUIB0003
END FUNCTION


FUNCTION   F_010_trae_valor_accion()
   DEFINE   l_val_acc                     DEC(19,14),
            l_fecha_parti                 ,
            l_fec_valuacion               DATE
   LET      l_fecha_parti              = 
            MDY(MONTH(reg_tui_ctr_folio.fecha_liquidacion),
           "01",YEAR(reg_tui_ctr_folio.fecha_liquidacion))
   FOR      g_siefore                  =  1     TO  50
            LET    g_valor_sie  [g_siefore]        =  0
   END FOR
   DECLARE  cur_val_acc   CURSOR     FOR
    SELECT  codigo_siefore,fecha_valuacion,precio_del_dia
      FROM  glo_valor_accion 
     WHERE (fecha_valuacion            =  reg_tui_ctr_folio.fecha_liquidacion 
        OR  fecha_valuacion            =  l_fecha_parti)
       AND  codigo_siefore             >  0
   FOREACH  cur_val_acc      INTO    g_siefore,l_fec_valuacion,l_val_acc
            IF      g_siefore          =  12     THEN
                    IF      l_fec_valuacion        <>  l_fecha_parti    THEN
                            CONTINUE   FOREACH
                    END IF
            ELSE
            IF      g_siefore          <>  12     THEN
                    IF      l_fec_valuacion        <>
                            reg_tui_ctr_folio.fecha_liquidacion     THEN
                            CONTINUE   FOREACH
                    END IF
            END IF
            END IF
            LET     g_valor_sie  [g_siefore]        =  l_val_acc
   END FOREACH
   DISPLAY  g_valor_sie [1]           TO  FORMONLY.valor_sie_b1       
   DISPLAY  g_valor_sie [2]           TO  FORMONLY.valor_sie_b2
   DISPLAY  g_valor_sie [3]           TO  FORMONLY.valor_sie_b3
   DISPLAY  g_valor_sie [4]           TO  FORMONLY.valor_sie_b4
   DISPLAY  g_valor_sie [5]           TO  FORMONLY.valor_sie_b5
   DISPLAY  g_valor_sie [6]           TO  FORMONLY.valor_sie_b6
   DISPLAY  g_valor_sie [12]          TO  FORMONLY.valor_parti
   DISPLAY  g_valor_sie [13]          TO  FORMONLY.valor_udis
END FUNCTION

FUNCTION    F_520_liquida()
   DEFINE   l_fecha_prov                  DATE
   DEFINE   l_texto_liq                   CHAR(500)
   DEFINE   l_scta                        ,
            l_scta_fin                    ,
            verifica_liq                  SMALLINT
   LET      g_procesados         =  0
   LET      l_texto_liq      =  ' EXECUTE  FUNCTION  fn_liquida(?,?,?,?,?) '
   PREPARE  qry_liq            FROM  l_texto_liq
   DECLARE  c_liquida        CURSOR   FOR
   SELECT   UNIQUE(A.nss)
     FROM   tui_sdo_uni_issste   A
    WHERE   A.folio               =  reg_tui_ctr_folio.folio
      AND   A.estado              =  102
    ORDER   BY   1
   FOREACH   c_liquida    INTO  g_nss
            LET      g_procesados         =  g_procesados     +  1
            DISPLAY  g_procesados        TO  FORMONLY.l_procesados
            DECLARE  c_subcuenta    CURSOR  FOR
            SELECT   UNIQUE  (subcuenta)
              FROM   dis_provision
             WHERE   folio             =  reg_tui_ctr_folio.folio
             FOREACH  c_subcuenta       INTO   l_scta
                     DECLARE  cur_liq      CURSOR  FOR  qry_liq
                     FOREACH  cur_liq
                              USING    reg_tui_ctr_folio.folio,
                                       g_nss       ,
                                       l_scta      ,
                                       reg_tui_ctr_folio.fecha_liquidacion  ,
                                       g_today
                                 INTO  verifica_liq
                     END FOREACH
             END FOREACH
             UPDATE   tui_sdo_uni_issste
                SET   estado           =  103
              WHERE   folio            =  reg_tui_ctr_folio.folio
                AND   nss              =  g_nss;
             UPDATE   tui_int_uni_issste
                SET   estado           =  103
              WHERE   folio            =  reg_tui_ctr_folio.folio
                AND   nss              =  g_nss;
   END FOREACH
END FUNCTION

FUNCTION    F_530_arma_globales_para_reporte()
   DEFINE   l_tipo_desc                    CHAR(30)
   LET      hoy                         =  g_today 
   LET      g_tabname                   = 'dis_provision   b '
   LET      g_folio                     =  reg_tui_ctr_folio.folio
   LET      g_fecha_accion              =  reg_tui_ctr_folio.fecha_liquidacion
   LET        l_tipo_desc      =  "UNIFICACIÓN ISSSTE-IMSS"
   LET      g_tipo_desc1                = 'TUIB0003'," ",l_tipo_desc CLIPPED,
            " ","POR TIPO DE TRASPASO"
   LET      g_tipo_desc2                = 'TUIB0003' ," ",l_tipo_desc CLIPPED,
            " ","POR SUBCUENTA"
   LET      g_nombre_programa           =  "TUIB0003"
   LET      g_tip_rep                   =  "LIQ" 
END FUNCTION

