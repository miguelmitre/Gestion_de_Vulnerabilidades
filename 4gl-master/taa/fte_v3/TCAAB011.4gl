##########################################################################
#Proyecto          => AFORE ( MEXICO )                                   #
#Propietario       => E.F.P.                                             #
#Programa TCAAB011  => CONSULTA NSS TRASPASO DE AFORE CEDENTE            #
#Fecha             => 06 DE OCTUBRE DEL 2004                             #
#Autor             => JOSE FRANCISCO  LUGO  CORNEJO                      #
#Sistema           => TCAA                                               #
##########################################################################

DATABASE    safre_af

GLOBALS
   DEFINE   g_fecha_presentacion                    ,
            HOY                                     ,	
            g_fecha_liquidacion                     DATE    
   DEFINE   lastkey                                 ,
            tot                                     ,
            consec                                  ,
            arr                                     ,
            band                                    ,
            i                                       ,
            arr_c                                   ,
            liquidada                               ,
            total_pa                                ,
            g_tipo_traspaso                         ,
            g_dis_pesos                             ,
            pos                                     ,
            linea                                   SMALLINT
   DEFINE   reg       ARRAY[20000]  OF  RECORD
            indice                            CHAR(01),
            consec                            INTEGER,
            nss                               CHAR(11),
            curp                              CHAR(18),
            acc_rcv                           DEC(16,6),
            part_viv                          DEC(16,6)
    END RECORD
    DEFINE
            g_afore            RECORD  LIKE   tab_afore_local.*,
            g_paramgrales      RECORD  LIKE   seg_modulo.*
    DEFINE
            enter                             CHAR(1),
            HORA                              CHAR(8),
            g_usuario                         CHAR(8) ,
            n_seguro                          CHAR(11),
            sel_where                         CHAR(1000),
            cla_where                         CHAR(1000)
    DEFINE  l_sal_pesos       ARRAY[15]   OF  DEC(16,6)
    DEFINE  l_sal_acciones    ARRAY[15]   OF  DEC(16,6)
    DEFINE  g_folio                           INTEGER
END GLOBALS

MAIN
    DEFINE   salida                        SMALLINT
    OPTIONS
             PROMPT    LINE  LAST, 
             INPUT     WRAP
    CALL     F_001_inicio()
    LET      salida               =  1
    WHILE    salida               =  TRUE
             CALL      F_020_despliega_folios() 
                       RETURNING    salida
    END WHILE
END MAIN

FUNCTION    F_001_inicio()
   DEFINE   l_qry                       CHAR(1000)
   CALL     STARTLOG  ("TCAAB011.log")
   LET      HOY                      =  TODAY
   LET      HORA                     =  TIME
   SELECT   *, USER
     INTO   g_paramgrales.*, g_usuario
     FROM   seg_modulo
    WHERE   modulo_cod               =  'taa';
   SELECT   a.estado
     INTO   liquidada
     FROM   safre_af:taa_cd_edo_cedente  a
    WHERE   a.descripcion            = "LIQUIDADA"
      AND   a.tipo                   =  3;
    LET     l_qry    =  " SELECT  TRIM(a.nombre)||' '||TRIM(a.paterno)||' '|| ",
                        "         TRIM(a.materno),",
                        " a.rfc,b.finicta,a.tipo_traspaso,c.descripcion ",
                        " FROM  taa_cd_det_cedido a, afi_mae_afiliado  b ,",
                        " taa_cd_tipo_traspaso c ",
                        " WHERE  a.folio     = ?",
                        " AND    a.n_seguro  = ?",
                        " AND    a.n_seguro  = b.n_seguro",
                        " AND    a.tipo_traspaso = c.tipo_traspaso;" 
     LET    l_qry       =  l_qry    CLIPPED
     PREPARE   qry_nombre      FROM  l_qry
     LET    l_qry    =  " SELECT  subcuenta,SUM(monto_en_acciones),",
                        "         SUM(monto_en_pesos)",
                        " FROM    dis_cuenta",
                        " WHERE   folio    = ?",
                        " AND     nss      = ?",
                        " AND     subcuenta    > 0 ",
                        " AND     tipo_movimiento  <>  0",
                        " GROUP   BY  1;" 
     LET    l_qry       =  l_qry    CLIPPED
     PREPARE   qry_saldos      FROM  l_qry
END FUNCTION

FUNCTION    F_020_despliega_folios()
   DEFINE   f_b  SMALLINT
   DEFINE   arr_taa_cd_ctr_folio  ARRAY[10000]  OF  RECORD
            folio                     LIKE  taa_cd_ctr_folio.folio, 
            tipo_traspaso             LIKE  taa_cd_ctr_folio.tipo_traspaso, 
            fecha_liquidacion         LIKE  taa_cd_ctr_folio.fecha_liquidacion 
                                        END RECORD
   DEFINE   reg_taa_cd_ctr_folio RECORD
            folio                      LIKE   taa_cd_ctr_folio.folio, 
            tipo_traspaso              LIKE   taa_cd_ctr_folio.tipo_traspaso, 
            fecha_liquidacion          LIKE   taa_cd_ctr_folio.fecha_liquidacion
                                        END RECORD
   LET      f_b               =  1
   OPEN     WINDOW    taab0112     AT   3,2
            WITH FORM "TCAAB0112" ATTRIBUTE(BORDER)
   DISPLAY  "<Enter>Seleccionar           <Ctrl -C>Salir           ",
            "                  " AT 1,2 ATTRIBUTE(REVERSE)
   DISPLAY  HOY USING "DD-MM-YYYY" AT 1,64 ATTRIBUTE(REVERSE)
   DISPLAY  "TCAAB0112     CONSULTA TRASPASOS LIQUIDADOS  AFORE CEDENTE   ",
            "                 " AT 4,2 ATTRIBUTE(REVERSE)
   DECLARE  cur_dsply_folio CURSOR FOR
   SELECT   A.folio,A.tipo_traspaso,A.fecha_liquidacion
     FROM   safre_af:taa_cd_ctr_folio A
    WHERE   A.estado            = liquidada
    ORDER   BY         A.folio   DESC
   LET      i                   =  1
   FOREACH  cur_dsply_folio         INTO  reg_taa_cd_ctr_folio.*
            LET      arr_taa_cd_ctr_folio[i].*   =  reg_taa_cd_ctr_folio.*
            LET      i                           =  i   +   1
   END FOREACH
   CALL     SET_COUNT(i-1)
   DISPLAY  ARRAY    arr_taa_cd_ctr_folio       TO  sr_ctr_folio.*
            ON  KEY(ESC)
                     LET      arr_c                  =  ARR_CURR()
            ON  KEY(RETURN)
                     LET      arr_c                  =  ARR_CURR()
                     LET      g_folio                = 
                              arr_taa_cd_ctr_folio[arr_c].folio
                     LET      g_tipo_traspaso        =
                              arr_taa_cd_ctr_folio[arr_c].tipo_traspaso
                     CALL     F_100_proceso_principal()
            ON KEY(INTERRUPT)
                     LET      f_b                    =  0
                     EXIT     DISPLAY
   END DISPLAY
   RETURN    f_b   
END FUNCTION

FUNCTION    F_100_proceso_principal()
   OPEN     WINDOW      ventana_1       AT  2,2   
            WITH  FORM  "TCAAB0111"     ATTRIBUTE( BORDER)
   DISPLAY  " TCAAB011      CONSULTA TRASPASOS LIQUIDADOS  AFORE CEDENTE   ",
            "                   " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY  HOY     USING  "DD-MM-YYYY"     AT  3,64  ATTRIBUTE(REVERSE)
   CALL     F_160_dibuja_pantalla(6,11,9)
   CALL     F_160_dibuja_pantalla(6,11,22)
   CALL     F_160_dibuja_pantalla(6,11,42)
   CALL     F_160_dibuja_pantalla(6,11,60)
   CALL     F_160_dibuja_pantalla(16,20,27)
   CALL     F_160_dibuja_pantalla(16,20,53)
   MENU     " TRASPASOS "
            COMMAND  "Consulta de Traspasos"
                     "     <Esc>  Continua     <Crtl-c> Salir  "
                     CLEAR    FORM
                     CALL     F_120_Consulta()
            COMMAND  "Salir " " Salir de Programa"
                     EXIT MENU
   END MENU
   CLOSE    WINDOW   ventana_1
END FUNCTION

FUNCTION    F_110_Inicializa()
   DEFINE   j                             SMALLINT
   INITIALIZE  reg                    TO  NULL
   FOR      j         =   1           TO  5  
            DISPLAY   reg[j].*        TO  scr_1[j].*   ATTRIBUTE (NORMAL)
   END FOR
   CLEAR    FORM
END FUNCTION

FUNCTION    F_120_Consulta()
   DEFINE   sali                           ,
            linea                          SMALLINT
   LET      int_flag                    =  FALSE
   LET      sali                        =  0
   CALL     F_110_Inicializa()
   CONSTRUCT  cla_where ON  a.n_seguro
                      FROM  n_seguro 
            ON  KEY(ESC)
                     LET      int_flag         =  FALSE 
                     EXIT     CONSTRUCT
            ON  KEY(INTERRUPT)
                     LET      int_flag         =  FALSE 
                     ERROR    "BUSQUEDA CANCELADA..."
                     SLEEP    2
                     CLEAR    FORM
                     LET      sali             =  1
                     EXIT     CONSTRUCT 
   END CONSTRUCT
   IF       sali             =  1      THEN
            RETURN    
   END IF
   ERROR     "   PROCESANDO  INFORMACION ........."
   IF        g_tipo_traspaso         =  1    THEN
             LET      sel_where    = " SELECT  a.n_seguro,a.n_unico",
                       " FROM    taa_cd_det_cedido a ",
                       " WHERE   a.folio     =",g_folio , 
                       "   AND   a.estado    =",liquidada , 
                       "   AND ",cla_where CLIPPED,
                       " ORDER BY 2,1 " 
   ELSE
              LET      sel_where    = " SELECT  b.n_seguro,b.n_unico",
                       " FROM    taa_cd_det_comple a ,",
                       "         taa_cd_det_cedido b  ",
                       " WHERE   a.folio     =",g_folio , 
                       "   AND   a.n_seguro  = b.n_seguro ",
                       "   AND   a.estado    =",liquidada , 
                       "   AND ",cla_where CLIPPED,
                       " ORDER BY 2,1 " 
   END IF
   LET      sel_where       =  sel_where CLIPPED
   PREPARE  qry_consul         FROM  sel_where 
   DECLARE  cursor_c      CURSOR  FOR  qry_consul
   LET      pos             =  1
   FOREACH  cursor_c      INTO  reg[pos].nss,reg[pos].curp
            LET      reg[pos].consec       =  pos
            SELECT   SUM(monto_en_acciones)
              INTO   reg[pos].acc_rcv
              FROM   safre_af:dis_cuenta
             WHERE   folio           =  g_folio
               AND   nss             =  reg[pos].nss
               AND   subcuenta     NOT  IN (4,8,14) ;
            SELECT   SUM(monto_en_acciones)
              INTO   reg[pos].part_viv
              FROM   safre_af:dis_cuenta
             WHERE   nss              =  reg[pos].nss
               AND   folio            =  g_folio  
               AND   subcuenta       IN (4,8) ;
            LET      pos              =  pos + 1
   END FOREACH
   IF       pos                       =  1    THEN
            PROMPT     "    NO HAY FOLIOS LIQUIDADOS     "  FOR  enter
   END IF
   LET      tot                       =  pos     -  1
   SELECT   a.fecha_presentacion,a.fecha_liquidacion
     INTO   g_fecha_presentacion,g_fecha_liquidacion
     FROM   taa_cd_ctr_folio a
    WHERE   a.folio                     =  g_folio
   DISPLAY  g_folio                    TO  FORMONLY.folio
   DISPLAY  tot                        TO  FORMONLY.t_reg
   DISPLAY  g_fecha_presentacion       TO  FORMONLY.fecha_presentacion
   DISPLAY  g_fecha_liquidacion        TO  FORMONLY.fecha_liquidacion 
   CALL     SET_COUNT(pos-1)
   IF      (pos - 1)                   >=  1    THEN
            INPUT   ARRAY  reg  WITHOUT DEFAULTS  FROM  scr_1.*
                     BEFORE   ROW
                              LET      arr_c              =  ARR_CURR()
                              LET      total_pa           =  ARR_COUNT()
                              LET      linea              =  SCR_LINE()
                              IF       arr_c              >  total_pa THEN
                                       LET      band           =  TRUE
                                       EXIT     INPUT
                              END IF
                              DISPLAY  reg[arr_c].*      TO  scr_1[linea].*
                                       ATTRIBUTE(REVERSE) 
                              CALL     F_130_arma_saldos()
                     AFTER    FIELD    indice
                              IF       reg[arr_c].indice   IS  NOT  NULL   THEN
                                       LET       reg[arr_c].indice      =  NULL
                                       DISPLAY  BY  NAME     reg[arr_c].indice
                              END IF
                              IF       arr_c             >= (total_pa) THEN
                                       LET      lastkey                     =
                                                FGL_LASTKEY()
                                       IF      ((lastkey                    = 
                                                FGL_KEYVAL("down"))        OR
                                               (lastkey                    =
                                                FGL_KEYVAL("return"))      OR
                                               (lastkey                    =
                                                FGL_KEYVAL("tab"))         OR
                                               (lastkey                    =
                                                 FGL_KEYVAL("right")))     THEN

                                                ERROR    "No hay mas opciones",
                                                         " en esa direccion."
                                                NEXT     FIELD     indice
                                      END IF
                              END IF
                     AFTER    ROW
                              LET      arr_c              =  ARR_CURR()
                              LET      linea              =  SCR_LINE()
                              DISPLAY  reg[arr_c].*      TO  scr_1[linea].*
                     ON  KEY  (CONTROL-N)
                              CALL     F_150_muestra_saldos()
                END INPUT
   ELSE
                     ERROR   " REGISTROS CON ESAS CONDICIONES NO EXISTEN       "
                     SLEEP   2
                     ERROR   ""
            END IF
END FUNCTION

FUNCTION    F_130_arma_saldos()
   DEFINE   l_scta                        SMALLINT
   DEFINE   l_nombre                      CHAR(50)
   DEFINE   l_rfc                         CHAR(13)
   DEFINE   l_finicta                     DATE
   DEFINE   l_tipo_traspaso               CHAR(02)
   DEFINE   l_descripcion                 CHAR(40)
   DEFINE   l_pesos                       ,
            l_acciones                    DEC(16,6)
   LET      g_dis_pesos                   =  1
   EXECUTE  qry_nombre     USING  g_folio,reg[arr_c].nss
            INTO     l_nombre,l_rfc,l_finicta,l_tipo_traspaso,l_descripcion 
   FOR      l_scta             =   1     TO  15
            LET      l_sal_pesos[l_scta]          =  0
            LET      l_sal_acciones[l_scta]       =  0
   END FOR
   DECLARE  cur_saldos     CURSOR   FOR  qry_saldos
   OPEN     cur_saldos      USING   g_folio,reg[arr_c].nss
   FOREACH  cur_saldos       INTO   l_scta,l_acciones,l_pesos
            LET      l_sal_acciones[l_scta]       =  l_acciones
            LET      l_sal_pesos[l_scta]          =  l_pesos
   END FOREACH
   DISPLAY  l_nombre               TO  FORMONLY.nombre
   DISPLAY  l_rfc                  TO  FORMONLY.rfc
   DISPLAY  l_finicta              TO  FORMONLY.fec_afil
   DISPLAY  l_tipo_traspaso        TO  FORMONLY.tipo_traspaso
   DISPLAY  l_descripcion          TO  FORMONLY.descripcion
   CALL     F_150_muestra_saldos()
END FUNCTION

FUNCTION    F_150_muestra_saldos()
   IF       g_dis_pesos              THEN
            DISPLAY  "                            MONTOS  EN  PESOS       ",
                     "                                 "
                     AT    15,1   ATTRIBUTE(REVERSE)
            DISPLAY  "      <Control-N>   PARA VER MONTOS EN ACC_RCV/PART_VIV",
                     "                                    "
                      AT    21,1  ATTRIBUTE(REVERSE)
            DISPLAY  l_sal_pesos[1]         TO  FORMONLY.saldo1
            DISPLAY  l_sal_pesos[2]         TO  FORMONLY.saldo2
            DISPLAY  l_sal_pesos[3]         TO  FORMONLY.saldo3
            DISPLAY  l_sal_pesos[4]         TO  FORMONLY.saldo4
            DISPLAY  l_sal_pesos[5]         TO  FORMONLY.saldo5
            DISPLAY  l_sal_pesos[6]         TO  FORMONLY.saldo6
            DISPLAY  l_sal_pesos[7]         TO  FORMONLY.saldo7
            DISPLAY  l_sal_pesos[8]         TO  FORMONLY.saldo8
            DISPLAY  l_sal_pesos[9]         TO  FORMONLY.saldo9
            DISPLAY  l_sal_pesos[10]        TO  FORMONLY.saldo10
            DISPLAY  l_sal_pesos[11]        TO  FORMONLY.saldo11
            DISPLAY  l_sal_pesos[12]        TO  FORMONLY.saldo12
            DISPLAY  l_sal_pesos[13]        TO  FORMONLY.saldo13
            DISPLAY  l_sal_pesos[14]        TO  FORMONLY.saldo14
            LET      g_dis_pesos             =  0
   ELSE
            DISPLAY  "          MONTOS  EN  ACCIONES  RCV  Y  PARTICIPACIONES",
                     " VIVIENDA               "   AT  15,1  ATTRIBUTE(REVERSE)

            DISPLAY  "              <Control-N> PARA VER MONTOS EN PESOS                                               "          AT  21,1  ATTRIBUTE(REVERSE)
            DISPLAY  l_sal_acciones[1]      TO  FORMONLY.saldo1
            DISPLAY  l_sal_acciones[2]      TO  FORMONLY.saldo2
            DISPLAY  l_sal_acciones[3]      TO  FORMONLY.saldo3
            DISPLAY  l_sal_acciones[4]      TO  FORMONLY.saldo4
            DISPLAY  l_sal_acciones[5]      TO  FORMONLY.saldo5
            DISPLAY  l_sal_acciones[6]      TO  FORMONLY.saldo6
            DISPLAY  l_sal_acciones[7]      TO  FORMONLY.saldo7
            DISPLAY  l_sal_acciones[8]      TO  FORMONLY.saldo8
            DISPLAY  l_sal_acciones[9]      TO  FORMONLY.saldo9
            DISPLAY  l_sal_acciones[10]     TO  FORMONLY.saldo10
            DISPLAY  l_sal_acciones[11]     TO  FORMONLY.saldo11
            DISPLAY  l_sal_acciones[12]     TO  FORMONLY.saldo12
            DISPLAY  l_sal_acciones[13]     TO  FORMONLY.saldo13
            DISPLAY  l_sal_acciones[14]     TO  FORMONLY.saldo14
            LET      g_dis_pesos             =  1
   END IF
END FUNCTION 

FUNCTION    F_160_dibuja_pantalla(ini_vert,fin_vert,pos_hor)
   DEFINE   i                                ,
            ini_vert                         ,
            fin_vert                         ,
            pos_hor                          SMALLINT
   FOR      i                    =    ini_vert       TO   fin_vert
            DISPLAY  "|"              AT  i,pos_hor
   END FOR
END FUNCTION
