###########################################################################
#Proyecto           => AFORE ( MEXICO )                                   #
#Propietario        => E.F.P.                                             #
#Programa TUIB0002  => PROVISION   TRASPASO  POR UNIFICACIÓN  ISSSTE-IMSS #
#Autor              => JOSE FRANCISCO LUGO CORNEJO                        #
#Fecha Elaboración  => 25 OCTUBRE  2010                                   #
#Sistema            => TUIB (TRASPASO POR POR UNIFICACIÓN  ISSSTE-IMSS)   #
###########################################################################

DATABASE   safre_af

GLOBALS
   DEFINE
            g_fecha_liq_parti               ,
            g_today                         DATE
   DEFINE
            g_tipo_mov                      ,  
            g_cero                          ,
            g_scta                          ,
            g_siefore                       ,
            g_siefore_max                   SMALLINT
   DEFINE
            g_folio_sua                     ,
            g_procesados                    INTEGER,
            g_acciones                      ,
            g_pesos                         DEC(16,6)
   DEFINE
            g_nom_tab_taa_cd_det            CHAR(040),  
            g_enter                         CHAR(001),
            g_id_aportante                  CHAR(012),
            g_ejecuta                       CHAR(300)

    DEFINE  g_sdo                RECORD
            nss                             CHAR(11),
            int_viv92                               ,
            int_viv08                               ,
            sar92                                   ,
            viv92                                   ,
            viv08                               DECIMAL(16,6)
                                 END RECORD
     DEFINE   g_int_sar92       DECIMAL(16,6)

   DEFINE  reg_tui_ctr_folio            RECORD  LIKE  tui_ctr_folio.*
   DEFINE  g_prov                       RECORD  LIKE  dis_provision.*
   DEFINE  g_valor_sie        ARRAY[50]    OF   DEC(19,14)

END GLOBALS

GLOBALS "GLOB_REPS.4gl"

MAIN
   OPTIONS
            PROMPT   LINE   LAST,
            ACCEPT   KEY    CONTROL-I
            DEFER    INTERRUPT
   CALL     STARTLOG('TUIB0002.log')
   CALL     F_001_inicio()
   CALL     F_500_proceso()
END MAIN

FUNCTION    F_001_inicio()
   LET      g_today                  =  TODAY
   LET      g_cero                   =  0
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
   OPEN     WINDOW    TUIB0002   AT 2,2  WITH  FORM  "TUIB0002"  ATTRIBUTE(BORDER)
   DISPLAY  " <Esc> Continuar     < CTRL-C > Salir                        ",
            "                         "   AT  1,1   ATTRIBUTE(REVERSE)
   DISPLAY  g_today    USING  "DD-MM-YYYY","           "   AT  1,67 ATTRIBUTE(REVERSE)
   DISPLAY  "TUIB0002   PROVISIONA SALDOS DE TRASPASO POR UNIFICACIÓN ISSSTE A IMSS           ",
            "                  "  AT  3,1  ATTRIBUTE(REVERSE)
   DISPLAY  "           <<    PRECIOS DEL DIA DEL RÉGIMEN DE INVERSIÓN     >>                "   AT  12,1 ATTRIBUTE(REVERSE)
   DECLARE  cur_liqui       CURSOR    FOR      
   SELECT   a.*
     FROM   tui_ctr_folio a
    WHERE   a.fecha_liquidacion    BETWEEN  g_today -3   AND   g_today  + 5
      AND   a.estado                  =  101
   FOREACH  cur_liqui      INTO   reg_tui_ctr_folio.*
             SELECT   COUNT(*)
               INTO   g_total_cuentas
               FROM   tui_sdo_uni_issste
              WHERE   folio              =  reg_tui_ctr_folio.folio
                AND   estado             =  101
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
            PROMPT   "  No hay folio para Provisión Teclee <<Enter para Salir>>" 
                     FOR  g_enter
            EXIT  PROGRAM
   END IF
   DISPLAY  "  PROCESANDO  INFORMACION  ......                                "
               AT   19,1   ATTRIBUTE(REVERSE)
   CALL     F_520_provisiona()
   CALL     F_530_arma_globales_para_reporte()
   CALL     genera_reporte()
   DISPLAY  "                                                                  "
               AT   18,1
   UPDATE   tui_ctr_folio
      SET   estado                     =  102
    WHERE   folio                      =  reg_tui_ctr_folio.folio
      AND   estado                     =  101;
   PROMPT  "  PROCESO FINALIZADO, TECLEE  <Enter>  Para Salir         "
            ATTRIBUTE(REVERSE)     FOR    g_enter
   CLOSE  WINDOW   TUIB0002
END FUNCTION


FUNCTION   F_010_trae_valor_accion()
   DEFINE   l_val_acc                     DEC(19,14),
            l_fec_valuacion               DATE
   LET      g_fecha_liq_parti              = 
            MDY(MONTH(reg_tui_ctr_folio.fecha_liquidacion),
           "01",YEAR(reg_tui_ctr_folio.fecha_liquidacion))
   FOR      g_siefore                  =  1     TO  50
            LET    g_valor_sie  [g_siefore]        =  0
   END FOR
   DECLARE  cur_val_acc   CURSOR     FOR
    SELECT  codigo_siefore,fecha_valuacion,precio_del_dia
      FROM  glo_valor_accion 
     WHERE (fecha_valuacion            =  reg_tui_ctr_folio.fecha_liquidacion 
        OR  fecha_valuacion            =  g_fecha_liq_parti)
       AND  codigo_siefore             NOT IN(  0,11)
   FOREACH  cur_val_acc      INTO    g_siefore,l_fec_valuacion,l_val_acc
            IF      g_siefore          =  12     THEN
                    IF      l_fec_valuacion        <>  g_fecha_liq_parti    THEN
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

FUNCTION    F_520_provisiona()
   DEFINE   l_fecha_prov                  DATE
   DEFINE   l_texto_query                 CHAR(500)
   DEFINE   l_texto_prov                  CHAR(500)
   DEFINE   l_scta                        ,
            l_scta_fin                    ,
            verifica_liq                  SMALLINT
   INITIALIZE    g_prov.*   TO NULL
   LET      g_prov.estado              =  6
   LET      g_prov.tipo_movimiento     =  1
   LET      g_prov.folio               =  reg_tui_ctr_folio.folio
   LET      g_prov.curp                =  ''
   LET      g_prov.folio_sua           =  ''
   LET      g_prov.dias_cotizados      =  0
   LET      g_prov.fecha_conversion    =  g_today
   LET      g_prov.sucursal            =  ''
   LET      g_prov.fecha_proceso       =  g_today
   LET      g_prov.usuario             =  g_usuario
   LET      g_prov.fecha_archivo       =  reg_tui_ctr_folio.fecha_presentacion
   LET      g_prov.etiqueta            =  0
   LET      g_prov.id_aportante        =  "UNI_ISSSTE"
   LET      g_procesados          =  0
   DECLARE  c_reporte        CURSOR   FOR
   SELECT   A.nss,A.int_viv92,A.int_viv08,A.sar92,A.viv92,A.viv08
     FROM   tui_sdo_uni_issste   A
    WHERE   A.folio               =  reg_tui_ctr_folio.folio
    ORDER   BY   1
   FOREACH   c_reporte    INTO  g_sdo.*
           LET       g_int_sar92          =  0
           SELECT    int_sar92
             INTO    g_int_sar92
             FROM    tui_int_uni_issste
             WHERE   nss                  =  g_sdo.nss
                AND  folio                =  reg_tui_ctr_folio.folio;
            LET      g_procesados         =  g_procesados     +  1
            DISPLAY  g_procesados        TO  FORMONLY.l_procesados
            UPDATE   tui_sdo_uni_issste
               SET   estado               =  102
             WHERE   folio                =  reg_tui_ctr_folio.folio
               AND   nss                  =  g_sdo.nss;
            UPDATE   tui_int_uni_issste
               SET   estado               =  102
             WHERE   folio                =  reg_tui_ctr_folio.folio
               AND   nss                  =  g_sdo.nss;
            LET      g_prov.nss           =  g_sdo.nss
            LET      g_prov.fecha_valor   =  g_fecha_liq_parti
            LET      g_prov.tipo_movimiento    =  1
            LET      g_prov.subcuenta          =  14
            LET      g_prov.siefore            =  12
            LET      g_prov.precio_accion      =  g_valor_sie[12]
            LET      g_prov.monto_en_acciones  =  g_sdo.int_viv92
            LET      g_prov.monto_en_pesos     =  
                     g_sdo.int_viv92           *  g_valor_sie[12]
            IF       g_prov.monto_en_acciones   <>  0   THEN
                     INSERT   INTO  dis_provision  VALUES (g_prov.*)
            END IF
            LET      g_sdo.viv92               =  0
            LET      g_prov.tipo_movimiento    =  1
            LET      g_prov.monto_en_acciones  =  g_sdo.viv92 / g_valor_sie[12]
            LET      g_prov.monto_en_pesos     =  g_sdo.viv92
            IF       g_prov.monto_en_pesos    <>  0   THEN
                     INSERT   INTO  dis_provision  VALUES (g_prov.*)
            END IF
            LET      g_prov.subcuenta          =  35
            LET      g_prov.tipo_movimiento    =  1
            LET      g_prov.monto_en_acciones  = g_sdo.int_viv08
            LET      g_prov.monto_en_pesos   = g_sdo.int_viv08 * g_valor_sie[12]
            IF       g_prov.monto_en_acciones <>  0   THEN
                     INSERT   INTO  dis_provision  VALUES (g_prov.*)
            END IF
            LET      g_sdo.viv08               =  0
            LET      g_prov.tipo_movimiento    =  1
            LET      g_prov.monto_en_acciones  =  g_sdo.viv08 / g_valor_sie[12]
            LET      g_prov.monto_en_pesos     =  g_sdo.viv08
            IF       g_prov.monto_en_pesos    <>  0   THEN
                     INSERT   INTO  dis_provision  VALUES (g_prov.*)
            END IF
            LET      g_prov.fecha_valor         =  g_today
            LET      g_prov.subcuenta           =  13
            SELECT   cta.codigo_siefore 
              INTO   g_siefore
              FROM   cta_regimen  cta
             WHERE   cta.nss                   =  g_prov.nss   
               AND   cta.subcuenta             =  13
            LET      g_prov.siefore            =  g_siefore
            LET      g_prov.precio_accion      =  g_valor_sie[g_siefore]
            LET      g_prov.tipo_movimiento    =  1
            LET      g_prov.monto_en_acciones  =
                     g_sdo.sar92               /  g_valor_sie[g_siefore]
            LET      g_prov.monto_en_pesos     =  g_sdo.sar92
            IF       g_prov.monto_en_pesos    <>  0   THEN
                     INSERT   INTO  dis_provision  VALUES (g_prov.*)
            END IF
            LET      g_prov.tipo_movimiento    =  3
            LET      g_prov.monto_en_acciones  = 
                     g_int_sar92           /  g_valor_sie[g_siefore]
            LET      g_prov.monto_en_pesos     =  g_int_sar92
            IF       g_prov.monto_en_pesos    <>  0   THEN
                     INSERT   INTO  dis_provision  VALUES (g_prov.*)
            END IF
            INITIALIZE     g_sdo.*    TO  NULL
   END FOREACH
END FUNCTION

FUNCTION    F_530_arma_globales_para_reporte()
   DEFINE   l_tipo_desc                    CHAR(30)
   LET      hoy                         =  g_today 
   LET      g_tabname                   = 'dis_provision   b '
   LET      g_folio                     =  reg_tui_ctr_folio.folio
   LET      g_fecha_accion              =  reg_tui_ctr_folio.fecha_liquidacion
   LET        l_tipo_desc      =  "UNIFICACIÓN ISSSTE-IMSS"
   LET      g_tipo_desc1                = 'TUIB0002'," ",l_tipo_desc CLIPPED,
            " ","POR TIPO DE TRASPASO"
   LET      g_tipo_desc2                = 'TUIB0002' ," ",l_tipo_desc CLIPPED,
            " ","POR SUBCUENTA"
   LET      g_nombre_programa           =  "TUIB0002"
   LET      g_tip_rep                   =  "PROV" 
END FUNCTION

