###########################################################################
#Proyecto          => AFORE ( MEXICO )                                    #
#Propietario       => E.F.P.                                              #
#Programa          => TCAAL004                                            #
#Objetivo          => GENERA REPORTES GLOBAL,ORIGEN DE TRASPASO Y DETALLE #
#Elaborado         => 23/06/2011                                          #
#Autor             => JOSE  FRANCISCO  LUGO  CORNEJO                      #
###########################################################################
#Modificacion      => REQ_EFPS-165 19/09/2011                             #
#                    Se modifica identificacion de cuentas con Anexo "C"  #
#                    y marcaje de cuentas en proceso de traspaso          #
###########################################################################
DATABASE  safre_af
GLOBALS
   
   DEFINE   g_today                  ,
            g_fec_liquida            DATE,

            g_nss                    CHAR(11),
            g_curp                   CHAR(18),
            g_enter                  CHAR(01),
            g_usuario                CHAR(008),
            g_titulo                 CHAR(012),
            g_tip_rep                CHAR(04),
            g_hora                   CHAR(005),
            g_tab_cuenta             CHAR(030),
            g_comando                CHAR(1000),
            g_sql                    CHAR(1200),

            g_tipo_saldo             ,
            g_tipo_reporte           INTEGER,
          
            g_imprime                SMALLINT

    DEFINE  g_seg_modulo            RECORD LIKE seg_modulo.*

    DEFINE  g_afore       RECORD
            afore_cod     CHAR(03),
            raz_social    CHAR(50)
                      END  RECORD

   DEFINE   g_reg           RECORD
            folio                         LIKE safre_af:dis_cuenta.folio,
            subcuenta                     LIKE safre_af:dis_cuenta.subcuenta,
            siefore                       SMALLINT,
            pesos                         DECIMAL(16,6),
            acciones                      DECIMAL(16,6)
                         END RECORD

 DEFINE   g_desc          ARRAY[50]      OF    RECORD  --siefore
            nom_siefore                         CHAR(010),
            nom_subcuenta                       CHAR(020)
                                          END   RECORD

END GLOBALS

MAIN
   OPTIONS
            INPUT    WRAP          ,
            PROMPT   LINE  LAST ,
            ACCEPT   KEY   CONTROL-I
#           DEFER    INTERRUPT

    CALL       f_010_inicio()
    CALL       f_200_proceso()
END MAIN

FUNCTION  f_010_inicio()
   DEFINE   l_i                 SMALLINT,
            l_desc              CHAR(30)
  
   CALL     STARTLOG("TCAAL004.log")
   LET      g_today                 =  TODAY
   LET      g_hora                  =  TIME
   SELECT   codigo_afore,razon_social
     INTO   g_afore.afore_cod,g_afore.raz_social
     FROM   tab_afore_local;

   DECLARE  cur_subcuenta_t   CURSOR  FOR
   SELECT   subct_cod,subct_desc
     FROM   tab_subcuenta
    WHERE   subct_cod                >  0
   FOREACH  cur_subcuenta_t   INTO   l_i,l_desc
            LET     g_desc[l_i].nom_subcuenta   =   l_desc
   END FOREACH
   DECLARE  cur_siefore_t     CURSOR  FOR
   SELECT   codigo_siefore,razon_social
     FROM   tab_siefore_local
    WHERE   codigo_siefore           >  0
   FOREACH  cur_siefore_t     INTO   l_i,l_desc
            LET     g_desc[l_i].nom_siefore   =   l_desc
   END FOREACH
END FUNCTION

FUNCTION f_200_proceso()
   DEFINE   l_hay                   SMALLINT
   OPEN     WINDOW    ventana_1      AT  2,2
            WITH      FORM    "TCAAL004"     ATTRIBUTE(BORDER)
   DISPLAY  "<TCAAL004>    <<GENERA REPORTES DE TRASPASOS AFORE CEDENTE>>         ",
            "          "    AT  1,2   ATTRIBUTE(REVERSE)
   DISPLAY  g_today     USING   "DD-MM-YY"   AT   1,68   ATTRIBUTE(REVERSE)
   DISPLAY  "       <Esc> Genera Reporte            <Ctrl-c> Salir",
            "                         "      AT   3,2    ATTRIBUTE(REVERSE)
   SELECT   MAX(fecha_liquidacion),user
     INTO   g_fec_liquida,g_usuario
     FROM   taa_cd_ctr_folio
    WHERE   estado          IN(102,103);
   SELECT   *
     INTO   g_seg_modulo.*
     FROM   seg_modulo
    WHERE   modulo_cod                =  'taa';
   LET      g_tipo_saldo              =  NULL
   LET      g_tipo_reporte            =  NULL
   LET      g_imprime                 =  NULL
   INPUT   BY  NAME    g_fec_liquida,g_tipo_saldo,g_tipo_reporte ,g_imprime
                        WITHOUT DEFAULTS
            AFTER    FIELD   g_fec_liquida
                     IF      g_fec_liquida     IS    NULL    THEN
                             ERROR "  LA FECHA NO PUEDE SER NULA... "
                             NEXT  FIELD  g_fec_liquida
                     ELSE
                             SELECT  COUNT(*)
                               INTO  l_hay
                               FROM  taa_cd_ctr_folio
                              WHERE  fecha_liquidacion    =  g_fec_liquida
                             IF      NOT     l_hay      THEN
                                     ERROR  "  NO HAY FOLIOS CON ESTA FECHA...          "
                                     NEXT  FIELD  g_fec_liquida
                             END IF
                     END IF

            AFTER    FIELD   g_tipo_saldo
                     IF      g_tipo_saldo      IS    NULL    OR
                             g_tipo_saldo       =  0         THEN
                             ERROR "  SOLO PUEDE SER  1-PROVISIÓN  o 2-LIQUIDACIÓN... "
                             NEXT  FIELD  g_tipo_saldo
                     END IF
            AFTER    FIELD   g_tipo_reporte
                     IF      g_tipo_reporte    IS    NULL    OR
                             g_tipo_reporte     =  0         THEN
                             ERROR "  SOLO PUEDE SER 1-GLOBAL, 2-TIPO TRASP, 3-DETALLE ó 9-TODOS.... "
                             NEXT  FIELD  g_tipo_reporte
                     END IF
            ON KEY (ESC)
                     IF      g_tipo_saldo      IS    NULL    OR
                             g_tipo_saldo       =  0         THEN
                             ERROR "  SOLO PUEDE SER  1-PROVISIÓN  o 2-LIQUIDACIÓN... "
                             NEXT  FIELD  g_tipo_saldo
                     END IF
                     IF      g_tipo_reporte    IS    NULL    OR
                             g_tipo_reporte     =  0         THEN
                             ERROR "  SOLO PUEDE SER 1-GLOBAL, 2-TIPO TRASP, 3-DETALLE ó 9-TODOS.... "
                             NEXT  FIELD  g_tipo_reporte
                     END IF
                     IF      g_tipo_saldo         =  1     THEN
                              SELECT  COUNT(UNIQUE c.folio)
                                INTO  l_hay
                                FROM  taa_cd_ctr_folio c, dis_provision  d
                               WHERE  fecha_liquidacion    =  g_fec_liquida
                                 AND  c.folio              =  d.folio
                              IF      NOT     l_hay      THEN
                                      ERROR  "  NO HAY PROVISION CON ESTA FECHA...          "
                                      NEXT  FIELD  g_fec_liquida
                              END IF
                     ELSE
                              SELECT  COUNT(UNIQUE c.folio)
                                INTO  l_hay
                                FROM  taa_cd_ctr_folio c, dis_cuenta  d
                               WHERE  fecha_liquidacion    =  g_fec_liquida
                                 AND  c.folio              =  d.folio
                              IF      NOT     l_hay      THEN
                                      ERROR  "  NO HAY LIQUIDACIÓN CON ESTA FECHA...          "
                                      NEXT  FIELD  g_fec_liquida
                              END IF
                      END IF
                     EXIT  INPUT

   END      INPUT

   ERROR "  GENERANDO  REPORTES...                         "
   IF       g_tipo_saldo            =  1      THEN
            LET      g_tab_cuenta        =  "dis_provision  d"
            LET      g_titulo            =  "PROVISION"
   ELSE
            LET      g_tab_cuenta        =  "dis_cuenta  d"
            LET      g_titulo            =  "LIQUIDACION"
   END IF

   LET      g_sql                 = 
           'SELECT  d.siefore,SUM(d.monto_en_pesos    * -1),',
           '                  SUM(d.monto_en_acciones * -1) ',
           '  FROM  taa_cd_ctr_folio c, ',g_tab_cuenta,
           ' WHERE  c.fecha_liquidacion    =  ',"'",g_fec_liquida,"'",
           '   AND  c.folio                =  d.folio ',
           '   AND  c.fecha_liquidacion    =  d.fecha_conversion ',
           '   AND  d.estado              <>  106 ',
           ' GROUP  BY  1 ORDER  BY  1;'
   PREPARE   sql_siefore       FROM   g_sql

   LET      g_sql                 = 
           'SELECT  d.subcuenta,SUM(d.monto_en_pesos  * -1),',
           '                  SUM(d.monto_en_acciones * -1) ',
           '  FROM  taa_cd_ctr_folio c, ',g_tab_cuenta,
           ' WHERE  c.fecha_liquidacion    =  ',"'",g_fec_liquida,"'",
           '   AND  c.folio                =  d.folio ',
           '   AND  c.fecha_liquidacion    =  d.fecha_conversion ',
           '   AND  d.estado              <>  106 ',
           ' GROUP  BY  1 ORDER  BY  1;'
   PREPARE   sql_subcuenta     FROM   g_sql
   LET      g_sql                 =
        'SELECT  CASE ',
        '    WHEN a.tipo_traspaso IN(1,4) THEN ',"'",'1).-NORMALES',"'",
        '    WHEN a.tipo_traspaso IN(2) THEN   ',"'",'2).-COMPLEMENTARIOS',"'",
        '        END CASE,',
        '        CASE   WHEN   b.tipo_traspaso',
        '                        NOT   IN(21,71,72,73,83,84,85)',
        '                        THEN  ',"'",'IMSS',"'",
        '               WHEN   b.tipo_traspaso',
        '                              IN(71,72,73,83,84,85)',
        '                        THEN  "ISSSTE"',
        '               WHEN   b.tipo_traspaso  =  21',
        '                        THEN  ',"'",'ORIGEN 21',"'",
        '        END CASE,d.siefore,',
        '        SUM( monto_en_pesos * -1),SUM( monto_en_acciones * -1)',
        '  FROM   taa_cd_ctr_folio a,taa_cd_det_cedido b,', g_tab_cuenta,
        ' WHERE   a.fecha_liquidacion      =   ',"'",g_fec_liquida,"'",
        '   AND   a.tipo_traspaso         IN(1,2,4)',
        '   AND   a.folio                  =  d.folio',
        '   AND   a.fecha_liquidacion      =  d.fecha_conversion ',
        '   AND   d.estado           NOT  IN(106)',
        '   AND   b.n_seguro               =  d.nss',
        '   AND   b.estado            IN(12,102,103)',
        '   AND   EXISTS (SELECT  1   FROM  taa_cd_tipo_traspaso t',
        '                  WHERE  b.tipo_traspaso  =  t.tipo_traspaso)',
        '  GROUP   BY  1,2,3',
        '  ORDER   BY  1,2,3;'
   PREPARE   sql_anexo     FROM   g_sql
   IF       g_tipo_reporte          =  1   OR
            g_tipo_reporte          =  9   THEN
            CALL    f_110_arma_global()
   END IF
   IF       g_tipo_reporte          =  2   OR
            g_tipo_reporte          =  9   THEN
            CALL    f_120_arma_origen_traspaso()
   END IF
   IF       g_tipo_reporte          =  3   OR
            g_tipo_reporte          =  9   THEN
            CALL    f_130_arma_detalle()
   END IF
   PROMPT  "  FIN  DE  PROGRAMA TECLEE <ENTER> PARA SALIR?...  "  FOR  g_enter
   CLOSE    WINDOW    ventana_1
END FUNCTION

FUNCTION    f_110_arma_global()
   DEFINE   l_variable                 ,
            l_lista                    CHAR(500)
   LET      l_variable    =
            g_seg_modulo.ruta_listados CLIPPED,"/",g_usuario CLIPPED,'.',
	    "TCAAL004_GLOBAL",g_today   USING "DDMMYY"    CLIPPED
   START REPORT   f_210_imprime_global  TO  l_variable       
   LET      g_comando                  =  "chmod 777 ",l_variable CLIPPED

   DISPLAY  "REPORTE GLOBAL : ",l_variable    AT  14,1   ATTRIBUTE  (REVERSE)
   LET      g_sql                 = 
           'SELECT  c.folio,d.subcuenta,d.siefore, ',
           '       SUM(d.monto_en_pesos * -1),SUM(d.monto_en_acciones * -1) ',
           '  FROM  taa_cd_ctr_folio c, ',g_tab_cuenta,
           ' WHERE  c.fecha_liquidacion    =  ',"'",g_fec_liquida,"'",
           '   AND  c.folio                =  d.folio ',
           '   AND  c.fecha_liquidacion    =  d.fecha_conversion ',
           '   AND  d.estado              <>  106 ',
           ' GROUP  BY  1,2,3 '
   LET      g_sql              =   g_sql   CLIPPED
   PREPARE  sql_global      FROM   g_sql
   DECLARE  cur_global    CURSOR   FOR   sql_global   
   FOREACH  cur_global      INTO   g_reg.*
            OUTPUT    TO  REPORT  f_210_imprime_global(g_reg.*)
   END FOREACH
   IF       g_imprime       THEN
            LET      l_lista        =  "lp ",l_variable
            RUN      l_lista
   END IF
   FINISH   REPORT     f_210_imprime_global      
   RUN      g_comando
   ERROR "  REPORTE  GLOBAL  GENERADO...                         "
END FUNCTION

FUNCTION    f_120_arma_origen_traspaso()
   DEFINE   l_variable                 ,
            l_lista                    CHAR(1000)
   LET      l_variable    =
            g_seg_modulo.ruta_listados CLIPPED,"/",g_usuario CLIPPED,'.',
	    "TCAAL004_ORIGEN",g_today   USING "DDMMYY"   CLIPPED
   START REPORT   f_220_imprime_origen_traspaso   TO  l_variable
   LET      g_comando                  =  "chmod 777 ",l_variable CLIPPED
   DISPLAY  "REPORTE ORIGEN : ",l_variable    AT  15,1   ATTRIBUTE  (REVERSE)
   LET      g_sql                          = 
           'SELECT  tr.folio,a.tipo_traspaso,d.siefore, ',
           '        SUM(d.monto_en_pesos * -1),SUM(d.monto_en_acciones * -1) ',
           '  FROM  taa_cd_ctr_folio tr,taa_cd_det_cedido  a  ,',g_tab_cuenta ,
           ' WHERE  tr.fecha_liquidacion    =  ',"'",g_fec_liquida,"'",
           '   AND  tr.folio                =  d.folio   ',
           '   AND  tr.fecha_liquidacion    =  d.fecha_conversion ',
           '   AND  d.estado               <>  106  ',
           '   AND  a.n_seguro              =  d.nss   ',
           '   AND  a.estado               IN (12,102, 103)',
           ' GROUP  BY  1,2,3 ',
           ' ORDER  BY  1,2,3 '
   PREPARE  sql_origen      FROM    g_sql             
   DECLARE  cur_origen    CURSOR  FOR  sql_origen
   FOREACH  cur_origen      INTO  g_reg.*
            OUTPUT    TO  REPORT  f_220_imprime_origen_traspaso(g_reg.*)
   END FOREACH
   IF       g_imprime       THEN
            LET      l_lista        =  "lp ",l_variable
            RUN      l_lista
   END IF
   FINISH   REPORT     f_220_imprime_origen_traspaso       
   RUN      g_comando
   ERROR "  REPORTE  POR ORIGEN GENERADO...                         "
END FUNCTION

FUNCTION    f_130_arma_detalle()
   DEFINE   l_variable                 ,
            l_lista                    CHAR(1000)

   DEFINE   l_reg            RECORD
            n_seguro                   LIKE  taa_cd_det_cedido.n_seguro,
            n_unico                    LIKE  taa_cd_det_cedido.n_unico,
            pesos                      DEC(16,6),
            acciones                   DEC(16,6),
            folio                      LIKE  taa_cd_det_cedido.folio,
            origen                     SMALLINT,
            tipo_traspaso              LIKE  taa_cd_det_cedido.tipo_traspaso,
            afore                      LIKE  tab_afore.afore_cod,
            nom_afore                  LIKE  tab_afore.afore_desc,
            fecha_trasp                LIKE  taa_cd_det_cedido.fecha_trasp,
            siefore                    SMALLINT
                        END  RECORD
   LET      l_variable    =
            g_seg_modulo.ruta_listados CLIPPED,"/",g_usuario CLIPPED,'.',
	    "TCAAL004_DETALLE",g_today   USING "DDMMYY"   CLIPPED
             CREATE   TEMP  TABLE   tmp_nss_proc 
             (nss                   CHAR(11));

   START REPORT   f_230_imprime_detalle      TO  l_variable
   LET      g_comando                  =  "chmod 777 ",l_variable CLIPPED

   DISPLAY  "REPORTE DETALLE: ",l_variable    AT  16,1   ATTRIBUTE  (REVERSE)
   LET      l_variable                     = 
            'SELECT  a.n_seguro,a.n_unico,SUM(d.monto_en_pesos * -1), ',
            '        SUM(d.monto_en_acciones * -1),d.folio,c.tipo_traspaso, ',
            '        a.tipo_traspaso, e.afore_cod,e.afore_desc , ',
            '        d.fecha_conversion, d.siefore ',
            '  FROM  taa_cd_det_cedido  a, taa_cd_ctr_folio c, ',g_tab_cuenta ,
            '        ,OUTER   tab_afore e ',
            ' WHERE  c.fecha_liquidacion        =  ',"'",g_fec_liquida,"'",
            '   AND  c.folio                    =  d.folio ',
            '   AND  c.fecha_liquidacion        =  d.fecha_conversion ',
            '   AND  d.estado                  <>  106  ',
            '   AND  a.n_seguro                 =  d.nss ',
            '   AND  a.estado                  IN (12,102, 103) ',
            '   AND  a.ident_lote_solici[3,5]   =   e.afore_cod ',
            ' GROUP  BY  1,2,5,6,7,8,9,10,11 ',
            ' ORDER  BY  1,2;'
   PREPARE  sql_detalle      FROM    l_variable
   DECLARE  cur_detalle    CURSOR  FOR  sql_detalle
   FOREACH  cur_detalle      INTO  l_reg.*
            OUTPUT   TO    REPORT  f_230_imprime_detalle(l_reg.*)
            INSERT    INTO  tmp_nss_proc  VALUES  (l_reg.n_seguro);
   END FOREACH
   IF       g_imprime       THEN
            LET      l_lista        =  "lp ",l_variable
            RUN      l_lista
   END IF
   FINISH   REPORT     f_230_imprime_detalle       
   RUN      g_comando
   ERROR "  REPORTE  POR DETALLE GENERADO...                         "
END FUNCTION

REPORT   f_210_imprime_global(s)
   DEFINE  l_sie                         ,
           l_sta                         SMALLINT,
           l_pesos                       ,
           l_acciones                    DECIMAL(16,6)
   DEFINE  s             RECORD
           folio                         LIKE safre_af:dis_cuenta.folio,
           subcuenta                     LIKE safre_af:dis_cuenta.subcuenta,
           siefore                       SMALLINT,
           pesos                         DECIMAL(16,6),
           acciones                      DECIMAL(16,6)
                         END RECORD
    OUTPUT
	TOP MARGIN    0
	BOTTOM MARGIN 0
	LEFT MARGIN   0
	RIGHT MARGIN  0
	PAGE LENGTH   90
      ORDER   BY  s.siefore,s.folio,s.subcuenta
    FORMAT
       PAGE HEADER
          PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
          PRINT COLUMN  02,'<TCAAL004>' ,
                COLUMN  20,'REPORTE GLOBAL DE TRASPASOS AFORE CEDENTE ',
                COLUMN  70,g_today            USING  'DD-MM-YYYY',
                COLUMN  85,"Pagina:  ",PAGENO USING "<<<<"
          PRINT COLUMN  20,'DE LA ',g_titulo,' DEL DIA (dd-mm-aaaa): ',
                           g_fec_liquida      USING  'DD-MM-YYYY'
          PRINT COLUMN 02,"__________________________________________________",
                          "_____________________________________________"

          SKIP 1 LINE
          
        PRINT  COLUMN 02,"ADMINISTRADORA: ","     ",g_afore.afore_cod
                     USING    "###","   ",g_afore.raz_social

        SKIP 1 LINE
        PRINT COLUMN 02,"__________________________________________________",
                          "_____________________________________________"
               
        SKIP 1 LINE

        PRINT COLUMN 05,"FOLIO",
              COLUMN 12,"SUBCUENTA",
              COLUMN 35,"SIEFORE",
              COLUMN 58,"MONTO EN PESOS",
              COLUMN 80,"MONTO EN ACCIONES"

        PRINT COLUMN 02,"--------------------------------------------------",
                        "-----------------------------------------------"

    ON EVERY ROW
        LET      l_sta        =  s.subcuenta
        LET      l_sie        =  s.siefore
        PRINT COLUMN  01,s.folio            USING "########" ,
              COLUMN  12,s.subcuenta        USING "##" ,' ',
                      g_desc[l_sta].nom_subcuenta ,
              COLUMN  35,s.siefore          USING "##" ,' ',
                      g_desc[l_sie].nom_siefore ,
              COLUMN  50,s.pesos            USING "---,---,---,--&.######",
              COLUMN  75,s.acciones         USING "---,---,---,--&.######"

        AFTER GROUP  OF s.siefore

        PRINT COLUMN 02,"--------------------------------------------------",
                        "-----------------------------------------------"

        LET      l_sie        =  s.siefore
        PRINT COLUMN  2,"TOTALES POR SIEFORE===>  ",
              COLUMN  35,s.siefore          USING "##" ,' ',
                      g_desc[l_sie].nom_siefore ,
              COLUMN  50,GROUP SUM(s.pesos)     USING  "---,---,---,--&.######",
              COLUMN  75,GROUP SUM(s.acciones)  USING  "---,---,---,--&.######"
         SKIP 1 LINES
  ON  LAST  ROW
      SKIP 1 LINES
      PRINT COLUMN 02,"--------------------------------------------------",
                        "-----------------------------------------------"
      PRINT COLUMN  2,"               ***   TOTAL POR SIEFORE   ***   "
      SKIP 1 LINES
      DECLARE  cur_siefore   CURSOR   FOR   sql_siefore  
      FOREACH  cur_siefore     INTO   l_sie,l_pesos,l_acciones
               PRINT COLUMN  35,l_sie        USING "##" ,' ',
                             g_desc[l_sie].nom_siefore ,
                     COLUMN  50,l_pesos      USING "---,---,---,--&.######",
                     COLUMN  75,l_acciones   USING "---,---,---,--&.######"
      END FOREACH
      SKIP 1 LINES
      PRINT COLUMN 02,"--------------------------------------------------",
                        "-----------------------------------------------"
      PRINT COLUMN  2,"               ***   TOTAL POR SUBCUENTA   *** "
      SKIP 1 LINES
      DECLARE  cur_subcuenta    CURSOR   FOR   sql_subcuenta   
      FOREACH  cur_subcuenta   INTO   l_sta,l_pesos,l_acciones
               PRINT COLUMN  10,l_sta        USING "##" ,' ',
                             g_desc[l_sta].nom_subcuenta ,
                     COLUMN  50,l_pesos      USING "---,---,---,--&.######",
                     COLUMN  75,l_acciones   USING "---,---,---,--&.######"
      END FOREACH
END REPORT

REPORT   f_220_imprime_origen_traspaso(s)
   DEFINE  l_sie                         ,
           l_sta                         SMALLINT,
           l_pesos                       ,
           l_acciones                    DECIMAL(16,6),
           l_desc                        CHAR(20),
           l_desc1                       CHAR(20),
           l_desc2                       CHAR(10)

   DEFINE  s             RECORD
           folio                         LIKE safre_af:dis_cuenta.folio,
           tipo_traspaso                 SMALLINT,
           siefore                       SMALLINT,
           pesos                         DECIMAL(16,6),
           acciones                      DECIMAL(16,6)
                         END RECORD
    OUTPUT
	TOP MARGIN    0
	BOTTOM MARGIN 0
	LEFT MARGIN   0
	RIGHT MARGIN  0
	PAGE LENGTH   90
      ORDER   BY  s.siefore,s.folio,s.tipo_traspaso
    FORMAT
       PAGE HEADER
          PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
          PRINT COLUMN  02,'<TCAAL004>' ,
                COLUMN  20,'REPORTE POR ORIGEN DE TRASPASOS AFORE CEDENTE ',
                COLUMN  70,g_today            USING  'DD-MM-YYYY',
                COLUMN  85,"Pagina:  ",PAGENO USING "<<<<"
          PRINT COLUMN  20,'DE LA ',g_titulo,' DEL DIA (dd-mm-aaaa): ',
                           g_fec_liquida      USING  'DD-MM-YYYY'
          PRINT COLUMN 02,"__________________________________________________",
                          "_____________________________________________"

          SKIP 1 LINE
          
        PRINT  COLUMN 02,"ADMINISTRADORA: ","     ",g_afore.afore_cod
                     USING    "###","   ",g_afore.raz_social

        SKIP 1 LINE
        PRINT COLUMN 02,"__________________________________________________",
                          "_____________________________________________"
               
        SKIP 1 LINE

        PRINT COLUMN 05,"FOLIO",
              COLUMN 12,"ORIGEN ",
              COLUMN 35,"SIEFORE",
              COLUMN 58,"MONTO EN PESOS",
              COLUMN 80,"MONTO EN ACCIONES"

        PRINT COLUMN 02,"--------------------------------------------------",
                        "-----------------------------------------------"

    ON EVERY ROW
        LET      l_sie        =  s.siefore
        SELECT   descripcion
          INTO   l_desc
          FROM   taa_cd_tipo_traspaso
         WHERE   tipo_traspaso               =  s.tipo_traspaso;
        PRINT COLUMN  02,s.folio            USING "########" ,
              COLUMN  12,s.tipo_traspaso    USING "##" ,' ',l_desc,
              COLUMN  35,s.siefore          USING "##" ,' ',
                      g_desc[l_sie].nom_siefore ,
              COLUMN  50,s.pesos            USING "---,---,---,--&.######",
              COLUMN  75,s.acciones         USING "---,---,---,--&.######"

        AFTER GROUP  OF s.siefore

        PRINT COLUMN 02,"--------------------------------------------------",
                        "-----------------------------------------------"

        LET      l_sie        =  s.siefore
        PRINT COLUMN  2,"TOTALES POR SIEFORE===>  ",
              COLUMN  35,s.siefore          USING "##" ,' ',
                      g_desc[l_sie].nom_siefore ,
            COLUMN  50,GROUP SUM(s.pesos)     USING  "---,---,---,--&.######",
            COLUMN  75,GROUP SUM(s.acciones)  USING  "---,---,---,--&.######"
         SKIP 1 LINES
  ON  LAST  ROW
      SKIP 1 LINES
      PRINT COLUMN 02,"--------------------------------------------------",
                        "-----------------------------------------------"
      PRINT COLUMN  2,"               ***   TOTAL POR SIEFORE   ***   "
      SKIP 1 LINES
      DECLARE  cur_siefore_or   CURSOR   FOR   sql_siefore  
      FOREACH  cur_siefore_or  INTO   l_sie,l_pesos,l_acciones
               PRINT COLUMN  35,l_sie        USING "##" ,' ',
                             g_desc[l_sie].nom_siefore ,
                     COLUMN  50,l_pesos      USING "---,---,---,--&.######",
                     COLUMN  75,l_acciones   USING "---,---,---,--&.######"
      END FOREACH
      SKIP 1 LINES
      PRINT COLUMN 02,"--------------------------------------------------",
                        "-----------------------------------------------"
      PRINT COLUMN  2,"               ***   TOTAL  POR SUBCUENTA   *** "
      SKIP 1 LINES
      DECLARE  cur_subcuenta_or    CURSOR   FOR   sql_subcuenta   
      FOREACH  cur_subcuenta_or   INTO   l_sta,l_pesos,l_acciones
               PRINT COLUMN  12,l_sta        USING "##" ,' ',
                             g_desc[l_sta].nom_subcuenta ,
                     COLUMN  50,l_pesos      USING "---,---,---,--&.######",
                     COLUMN  75,l_acciones   USING "---,---,---,--&.######"
      END FOREACH
      SKIP 2 LINES
      PRINT COLUMN  2,"     ANEXO  64  "
      SKIP 1 LINES
      DECLARE  cur_anexo    CURSOR   FOR   sql_anexo   
      FOREACH  cur_anexo    INTO  l_desc1,l_desc2,l_sie,l_pesos,l_acciones
               PRINT COLUMN  1,l_desc1,l_desc2,
                     COLUMN  35,l_sie        USING "##" ,' ',
                             g_desc[l_sie].nom_siefore ,
                     COLUMN  50,l_pesos      USING "---,---,---,--&.######",
                     COLUMN  75,l_acciones   USING "---,---,---,--&.######"
      END FOREACH

END REPORT

REPORT   f_230_imprime_detalle(s)
   DEFINE  l_sie                         ,
           l_sta                         SMALLINT,
           l_origen                      CHAR(10),
           l_pesos                       ,
           l_acciones                    DECIMAL(16,6),
           l_proc1                       ,
           l_procesados                  INTEGER
   DEFINE  s             RECORD
            n_seguro                   LIKE  taa_cd_det_cedido.n_seguro,
            n_unico                    LIKE  taa_cd_det_cedido.n_unico,
            pesos                      DEC(16,6),
            acciones                   DEC(16,6),
            folio                      LIKE  taa_cd_det_cedido.folio,
            origen                     SMALLINT,
            tipo_traspaso              LIKE  taa_cd_det_cedido.tipo_traspaso,
            afore                      LIKE  tab_afore.afore_cod,
            nom_afore                  CHAR(10),
            fecha_trasp                LIKE  taa_cd_det_cedido.fecha_trasp,
            siefore                    SMALLINT
                        END  RECORD
    OUTPUT
	TOP MARGIN    0
	BOTTOM MARGIN 0
	LEFT MARGIN   0
	RIGHT MARGIN  0
	PAGE LENGTH   90
 #    ORDER   BY  s.folio,s.n_seguro
    FORMAT
       PAGE HEADER
          PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
          PRINT COLUMN  02,'<TCAAL004>' ,
                COLUMN  20,'REPORTE POR DETALLE DE TRASPASOS AFORE CEDENTE ',
                COLUMN  90,g_today            USING  'DD-MM-YYYY',
                COLUMN 110,"Pagina:  ",PAGENO USING "<<<<"
          PRINT COLUMN   20,'DE LA ',g_titulo,' DEL DIA (dd-mm-aaaa): ',
                           g_fec_liquida      USING  'DD-MM-YYYY'
          PRINT COLUMN 02,"__________________________________________________",
                          "_____________________________________________",
                          "______________________________________________"

          SKIP 1 LINE
          
        PRINT  COLUMN 02,"ADMINISTRADORA: ","     ",g_afore.afore_cod
                     USING    "###","   ",g_afore.raz_social

        SKIP 1 LINE
        PRINT COLUMN 02,"__________________________________________________",
                          "_____________________________________________",
                          "______________________________________________"
               
        SKIP 1 LINE

       PRINT  COLUMN  01,'N_SEGURO',
              COLUMN  14,'N_UNICO',
              COLUMN  48,'PESOS',
              COLUMN  65,'ACCIONES',
              COLUMN  78,'FOLIO',
              COLUMN  85,'ORIGEN',
              COLUMN  95,'TIPO_TRA',
              COLUMN 107,'A F O R E',
              COLUMN 125,'FECHA_TRASP',
              COLUMN 137,'SIEFORE'


        PRINT COLUMN 02,"--------------------------------------------------",
                        "-----------------------------------------------",
                        "------------------------------------------------"

    ON EVERY ROW
        LET      l_sie        =  s.siefore
        LET      l_origen    =  'PROMOTOR'
        IF       s.origen     =  4     THEN
                 LET     l_origen      =  'DIVERSOS'
        END IF
        PRINT COLUMN  001, s.n_seguro          ,
              COLUMN  014, s.n_unico           ,
              COLUMN  035, s.pesos             USING "---,---,--&.######",
              COLUMN  055, s.acciones          USING "---,---,--&.######",
              COLUMN  075, s.folio             USING "########" ,
              COLUMN  085, l_origen            ,
              COLUMN  095, s.tipo_traspaso     ,
              COLUMN  107, s.afore             USING  '&&&',
              COLUMN  111, s.nom_afore         ,
              COLUMN  125, s.fecha_trasp       USING 'DD-MM-YYYY',
              COLUMN  137, g_desc[l_sie].nom_siefore
  ON  LAST  ROW
      SKIP 1 LINES
      PRINT COLUMN 02,"--------------------------------------------------",
                        "-----------------------------------------------",
                        "------------------------------------------------"
      PRINT COLUMN  2,"               ***   TOTAL GLOBAL POR SIEFORE   ***   "
      SKIP 1 LINES
      DECLARE  cur_siefore_det   CURSOR   FOR   sql_siefore  
      FOREACH  cur_siefore_det     INTO   l_sie,l_pesos,l_acciones
               PRINT COLUMN  35,l_sie        USING "##" ,' ',
                             g_desc[l_sie].nom_siefore ,
                     COLUMN  50,l_pesos      USING "---,---,---,--&.######",
                     COLUMN  75,l_acciones   USING "---,---,---,--&.######"
      END FOREACH
      SKIP 1 LINES
      PRINT COLUMN 02,"--------------------------------------------------",
                        "-----------------------------------------------",
                        "------------------------------------------------"
      PRINT COLUMN  2,"               ***   TOTAL GLOBAL POR SUBCUENTA   *** "
      SKIP 1 LINES
      DECLARE  cur_subcuenta_det    CURSOR   FOR   sql_subcuenta   
      FOREACH  cur_subcuenta_det   INTO   l_sta,l_pesos,l_acciones
               PRINT COLUMN  10,l_sta        USING "##" ,' ',
                             g_desc[l_sta].nom_subcuenta ,
                     COLUMN  50,l_pesos      USING "---,---,---,--&.######",
                     COLUMN  75,l_acciones   USING "---,---,---,--&.######"
      END FOREACH
      SKIP 1 LINES
      SELECT   COUNT(UNIQUE  n_seguro)
        INTO   l_proc1        
        FROM   taa_cd_det_comple
       WHERE   fecha_liquida     =  g_fec_liquida
         AND   estado            =  103
         AND   n_seguro   NOT  IN(SELECT  nss  FROM  tmp_nss_proc);
      SELECT   COUNT(UNIQUE  n_seguro)
        INTO   l_procesados
        FROM   taa_cd_det_cedido   
       WHERE   fecha_trasp       =  g_fec_liquida
         AND   estado            =  103
         AND   n_seguro   NOT  IN(SELECT  nss  FROM  tmp_nss_proc);
      LET      l_proc1            =  l_procesados   +  l_proc1
      PRINT COLUMN 2," SIN DETALLE SALDO CERO ===> ",l_proc1 USING "###,###"  
      SELECT   COUNT(UNIQUE  nss)
        INTO   l_procesados
        FROM   tmp_nss_proc;
      PRINT COLUMN 2," REGISTROS  CON  DETALLE ==> ",l_procesados USING "###,###"  
      LET      l_procesados       =  l_procesados   +  l_proc1
      PRINT COLUMN 2," TOTAL  CTAS. LIQUIDADAS ==> ",l_procesados USING "###,###"  
      
END REPORT
