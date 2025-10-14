############################################################################
#Proyecto          => AFORE ( MEXICO )                                     #
#Propietario       =>  E.F.P.                                              #
#Programa TCAAL010  => CONSULTA NSS TRASPASO DE AFORE CEDENTE              #
#Fecha             => 10 DE OCTUBRE DE 2004                                #
#Autor             => JOSE  FRANCISCO  LUGO  CORNEJO                       #
############################################################################
DATABASE    safre_af
GLOBALS
   DEFINE   lastkey                      ,
            provisionada                 ,
            g_trasp_comple               ,
            tot                          ,
            band                         ,
            g_scta                       ,
            i                            ,
            g_tipo_traspaso              ,
            arr_c                        ,
            total_pa                     ,
            pos                          ,
            linea                        SMALLINT
   DEFINE   reg ARRAY[20000]   OF  RECORD
            indice                       CHAR(01),
            consec                       INTEGER,
            nss                          CHAR(11),
            curp                         CHAR(18),
            rcv                          DEC(10,2),
            viv                          DEC(10,2)
   END RECORD
   DEFINE   n_seguro                     CHAR(11)
   DEFINE
            g_afore       RECORD LIKE tab_afore_local.*,
            g_paramgrales RECORD LIKE seg_modulo.*
   DEFINE   fecha_presentacion           ,
            fecha_liquidacion            ,
            HOY                          DATE
   DEFINE
            enter                        CHAR(1),
            HORA                         CHAR(8),
            g_usuario                    CHAR(8) 
   DEFINE
            sel_where                    CHAR(1000),
            cla_where                    CHAR(1000)
   DEFINE   sal_nss     ARRAY[15] OF     DEC(10,2) 
   DEFINE   sal_det     ARRAY[15] OF     DEC(10,2) 
   DEFINE   sal_sum     ARRAY[15] OF     DEC(10,2) 
   DEFINE   sal_dif     ARRAY[15] OF     DEC(10,2) 
   DEFINE   g_monto_en_pesos             DEC(10,2)
   DEFINE   g_folio                      ,
            arr                          ,
            consec                       INTEGER
END GLOBALS

MAIN
   OPTIONS
            PROMPT    LINE     LAST, 
            INPUT     WRAP
   CALL     f_001_inicio()
   CALL     f_200_proceso()
END MAIN

FUNCTION    f_001_inicio()
   DEFINE   l_qry                    CHAR(1000)
   CALL     STARTLOG("TCAAL010.log")
   LET      HOY                   =  TODAY
   LET      HORA                  =  TIME
   FOR      g_scta                =  1      TO  15
            LET        sal_det[g_scta]       =  0
            LET        sal_sum[g_scta]       =  0
            LET        sal_dif[g_scta]       =  0
   END FOR
   SELECT   *, USER
     INTO   g_paramgrales.*, g_usuario
     FROM   seg_modulo
    WHERE   modulo_cod            = 'taa';
   SELECT   a.estado
     INTO   provisionada
     FROM   safre_af:taa_cd_edo_cedente  a
    WHERE   a.descripcion         = "PROVISIONADA"
      AND   a.tipo                =  3;
   SELECT   a.estado
     INTO   g_trasp_comple
     FROM   safre_af:taa_cd_edo_cedente  a
    WHERE   a.descripcion         = "TRASPASO COMPLEMENTARIO"
      AND   a.tipo                =  3;
   LET      l_qry                 =
            " SELECT   UNIQUE(b.rfc),b.tipo_traspaso,",
            "          TRIM(b.nombre)||' '||TRIM(b.paterno)||' '|| ",
            "          TRIM(b.materno),",
            "          c.finicta,d.descripcion ",
            "   FROM   safre_tmp:taa_cd_saldos_arch  a ,",
            "          taa_cd_det_cedido     b ,",
            "          afi_mae_afiliado      c ,",
            "          taa_cd_tipo_traspaso  d  ",
            "  WHERE   a.folio               =  ?",
            "    AND   a.nss                 =  ?",
            "    AND   a.nss                 =  b.n_seguro",
            "    AND   a.nss                 =  c.n_seguro",
            "    AND   a.estado              =  ",provisionada,
            "    AND   b.tipo_traspaso       =  d.tipo_traspaso;" 
   LET      l_qry                  =  l_qry    CLIPPED
   PREPARE  qry_1           FROM  l_qry
   LET      l_qry                  =
            " SELECT   monto_en_pesos",
            "   FROM   safre_tmp:taa_cd_saldos_arch",
            "  WHERE   folio            =  ?",
            "  AND     nss              =  ?",
            "  AND     estado           =  ",provisionada,
            "  AND     subcuenta        =  ?"
   LET      l_qry                 =  l_qry    CLIPPED
   PREPARE  qry_2              FROM  l_qry
END FUNCTION

FUNCTION    f_200_proceso()
   OPEN     WINDOW   ventana_1          AT  2,2
   WITH     FORM     "TCAAL0101"        ATTRIBUTE( BORDER)
   DISPLAY  " TCAAL010      CONSULTA MONTOS DEL ARCHIVO DE SALDOS                                     " AT 2,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "DD-MM-YYYY" AT 3,64 ATTRIBUTE(REVERSE)
   MENU     " TRASPASOS "
            COMMAND  "Normal" " Normales   <Esc>=Continua  <Ctrl N> SUMARIOS  "
                      CLEAR    FORM
                      LET      g_tipo_traspaso            =  1
                      CALL     f_210_consulta()
            COMMAND  "Comple" " Comple.    <Esc>=Continua  <Ctrl N> SUMARIOS  "
                      LET      g_tipo_traspaso            =  2
                      CLEAR    FORM
                      CALL     f_210_consulta()
            COMMAND  "Internet" " Internet  <Esc>=Continua <Ctrl N> SUMARIOS  "
                      CLEAR    FORM
                      LET      g_tipo_traspaso            =  3
                      CALL     f_210_consulta()
            COMMAND   "Salir " " Salir de Programa"
                     EXIT MENU
   END MENU
   CLOSE    WINDOW   ventana_1
END FUNCTION

FUNCTION   f_210_consulta()
   DEFINE   sali                           ,
            linea                          SMALLINT
   LET      int_flag                       =  FALSE
   LET      sali                           =  0
   LET      g_folio                        =  0
   SELECT   MAX(a.folio) 
     INTO   g_folio
     FROM   safre_tmp:taa_cd_sum_arch  a,
            safre_af:taa_cd_ctr_folio  b
     WHERE  a.folio                        =  b.folio
       AND  b.tipo_traspaso                =  g_tipo_traspaso
   IF       g_folio                       IS  NULL     THEN
            PROMPT  "  NO  HAY  FOLIO  PROVISIONADO  <enter>Salir  " FOR enter
            EXIT    PROGRAM
   END IF
   CALL     f_230_muestra_totales()
   CONSTRUCT  cla_where        ON  a.nss
                             FROM  nss 
            ON  KEY(ESC)
                     LET      int_flag               =  FALSE 
                     EXIT     CONSTRUCT
            ON  KEY(INTERRUPT)
                     LET      int_flag               =  FALSE 
                     ERROR    "   BUSQUEDA  CANCELADA......          "
                     SLEEP    2
                     CLEAR    FORM
                     LET      sali                   =  1
                     EXIT     CONSTRUCT 
    END CONSTRUCT
    IF       sali                     =  1     THEN
             RETURN    
    END IF
    ERROR    " PROCESANDO  INFORMACION .........         "
    LET      sel_where     =  " SELECT   UNIQUE(b.n_seguro),b.n_unico",
                              " FROM     safre_tmp:taa_cd_saldos_arch a , ",
                              "          safre_af:taa_cd_det_cedido b ",
                              " WHERE    a.folio      =",g_folio , 
                              "   AND   ",cla_where CLIPPED,
                              "   AND    a.nss        =  b.n_seguro ",
                              "   AND   (b.estado     =  ",provisionada , 
                              "    OR    b.estado     =  ",g_trasp_comple , ")",
                              " ORDER    BY   1 " 

    LET      sel_where                =  sel_where    CLIPPED
    PREPARE  qry_consul            FROM  sel_where 
    DECLARE  cursor_c       CURSOR  FOR  qry_consul
    LET      pos                      =  1
    FOREACH  cursor_c              INTO  reg[pos].nss,reg[pos].curp
             LET      reg[pos].consec           =  pos
             LET      reg[pos].rcv              =  0
             LET      reg[pos].viv              =  0
             FOR      g_scta                    =  1      TO  15
                      LET      g_monto_en_pesos          =  0 
                      EXECUTE  qry_2        USING  g_folio,reg[pos].nss,g_scta
                                             INTO  g_monto_en_pesos
                      IF       g_scta          <>  4          AND
                               g_scta          <>  8          AND
                               g_scta          <>  14         THEN
                               LET      reg[pos].rcv       =
                                        reg[pos].rcv       +  g_monto_en_pesos
                      ELSE
                               LET      reg[pos].viv       =
                                        reg[pos].viv       +  g_monto_en_pesos
                      END IF
             END FOR
             LET      pos                  =  pos   +  1
    END FOREACH
    LET      tot                      =  pos   -  1
    SELECT   a.fecha_envio_saldos,a.fecha_liquidacion
      INTO   fecha_presentacion,fecha_liquidacion
      FROM   taa_cd_ctr_folio a
     WHERE   a.folio                  =  g_folio
    DISPLAY  g_folio                  TO  FORMONLY.folio
    DISPLAY  tot                      TO  FORMONLY.t_reg
    DISPLAY  fecha_presentacion       TO  FORMONLY.fecha_presentacion
    DISPLAY  fecha_liquidacion        TO  FORMONLY.fecha_liquidacion 
    DISPLAY  BY NAME  fecha_liquidacion
    CALL     SET_COUNT(pos-1)
    IF      (pos - 1)                   >=  1    THEN
             INPUT    ARRAY  reg  WITHOUT DEFAULTS  FROM  scr_1.*
                      BEFORE  ROW
                              LET      arr_c               =  ARR_CURR()
                              LET      total_pa            =  ARR_COUNT()
                              LET      linea               =  SCR_LINE()
                              IF       arr_c               >  total_pa THEN
                                       LET      band         =  TRUE
                                       EXIT INPUT
                              END IF
                              DISPLAY  reg[arr_c].*       TO
                                       scr_1[linea].*  ATTRIBUTE(REVERSE) 
                              CALL     f_220_arma_saldos()
                      AFTER   FIELD    indice
                              IF       reg[arr_c].indice  IS  NOT  NULL  THEN
                                       LET      reg[arr_c].indice       =  NULL
                                       DISPLAY  BY  NAME     reg[arr_c].indice
                              END IF
                              IF       arr_c              >=  (total_pa) THEN
                                       LET      lastkey      =  FGL_LASTKEY()
                                       IF     ((lastkey      =
                                                FGL_KEYVAL("down"))    OR
                                               (lastkey      = 
                                                FGL_KEYVAL("return"))  OR
                                               (lastkey      =
                                                FGL_KEYVAL("tab"))     OR
                                               (lastkey     = 
                                                FGL_KEYVAL("right")))    THEN
                                                ERROR    "No hay mas opciones",
                                                         " en esa direccion."
                                                NEXT  FIELD    indice
                                       END IF
                              END IF
                      AFTER ROW
                              LET      arr_c             =  ARR_CURR()
                              LET      linea             =  SCR_LINE()
                              DISPLAY  reg[arr_c].*     TO  scr_1[linea].*
                                       ON  KEY  (CONTROL-N)
                                               CALL     f_230_muestra_totales()
             END INPUT
    ELSE
             PROMPT " NO EXISTEN REGISTROS .... <enter> Salir" FOR enter
    END IF
END FUNCTION

FUNCTION    f_220_arma_saldos()
   DEFINE   l_scta                   SMALLINT
   DEFINE   l_nombre                 CHAR(50)
   DEFINE   l_rfc                    CHAR(13)
   DEFINE   l_finicta                DATE
   DEFINE   l_tipo_traspaso          CHAR(02)
   DEFINE   l_descripcion            CHAR(40)
   EXECUTE  qry_1          USING  g_folio,reg[arr_c].nss
                            INTO  l_rfc,l_tipo_traspaso,l_nombre,
                                  l_finicta,l_descripcion 
   FOR      g_scta             =  1        TO  15
            LET      g_monto_en_pesos    =  0
            EXECUTE  qry_2           USING  g_folio,reg[arr_c].nss,g_scta
                                      INTO  g_monto_en_pesos
            LET      sal_nss[g_scta]     =  g_monto_en_pesos
   END FOR
   DISPLAY  l_nombre               TO  FORMONLY.nombre
   DISPLAY  l_rfc                  TO  FORMONLY.rfc
   DISPLAY  l_finicta              TO  FORMONLY.fec_afil
   DISPLAY  l_tipo_traspaso        TO  FORMONLY.tipo_traspaso
   DISPLAY  l_descripcion          TO  FORMONLY.descripcion
   DISPLAY  "                        SALDOS  DEL  ARCHIVO  POR  SUBCUENTA   ",
            "                          "     AT   16,1    ATTRIBUTE(REVERSE)
   DISPLAY   sal_nss[1]            TO  FORMONLY.scta_1
   DISPLAY   sal_nss[2]            TO  FORMONLY.scta_2_6_9
   DISPLAY   sal_nss[3]            TO  FORMONLY.scta_3
   DISPLAY   sal_nss[5]            TO  FORMONLY.scta_5
   DISPLAY   sal_nss[7]            TO  FORMONLY.scta_7
   DISPLAY   sal_nss[10]           TO  FORMONLY.scta_10
   DISPLAY   sal_nss[11]           TO  FORMONLY.scta_11
   DISPLAY   sal_nss[12]           TO  FORMONLY.scta_12
   DISPLAY   sal_nss[13]           TO  FORMONLY.scta_13
   DISPLAY   sal_nss[4]            TO  FORMONLY.scta_4
   DISPLAY   sal_nss[8]            TO  FORMONLY.scta_8
   DISPLAY   sal_nss[14]           TO  FORMONLY.scta_14
   DISPLAY   sal_nss[15]           TO  FORMONLY.scta_15
END FUNCTION 

FUNCTION    f_230_muestra_totales()
   DEFINE   f_b                        SMALLINT
   LET      f_b                        =  1
   DECLARE  c_det     CURSOR  FOR
   SELECT   subcuenta,SUM(monto_en_pesos)
     FROM   safre_tmp:taa_cd_saldos_arch
    WHERE   folio                    =  g_folio
      AND   estado                   =  provisionada  
    GROUP   BY  1
   FOREACH  c_det      INTO   g_scta,g_monto_en_pesos
        LET      sal_det[g_scta]        =  g_monto_en_pesos
   END FOREACH
   DECLARE  c_sum     CURSOR  FOR
   SELECT   subcuenta,monto_en_pesos
     FROM   safre_tmp:taa_cd_sum_arch
    WHERE   folio                    =  g_folio
   FOREACH  c_sum         INTO  g_scta,g_monto_en_pesos
            LET      sal_sum[g_scta]        =  g_monto_en_pesos
   END FOREACH
   FOR      g_scta           =  1     TO  15
            LET      sal_dif[g_scta]   =  sal_det[g_scta]   -  sal_sum[g_scta]
   END FOR
   OPEN WINDOW  taal0104   AT   2,2  WITH  FORM  "TCAAL0104"  ATTRIBUTE(BORDER)
   DISPLAY  "TCAAB0104   <<RESUMEN DEL DETALLE Y SUMARIOS DEL ARCHIVO ",
            "DE  SALDOS>>              "     AT   1,2      ATTRIBUTE(REVERSE)
   DISPLAY  "              TOTALES  DEL  DETALLE  DEL  ARCHIVO  DE  SALDOS  ",
            "                        "     AT   3,2      ATTRIBUTE(REVERSE)
   DISPLAY  "              TOTALES  DEL  SUMARIO  DEL  ARCHIVO  DE  SALDOS  ",
            "                        "     AT    9,2     ATTRIBUTE(REVERSE)
   DISPLAY  "              DIFERENCIA  EN  ARCHIVO  ENTRE  DETALLE  Y  SUMAR",
            "IO                      "     AT   15,2     ATTRIBUTE(REVERSE)
   DISPLAY  sal_det[1]         TO  FORMONLY.sal_1_det 
   DISPLAY  sal_det[2]         TO  FORMONLY.sal_2_det
   DISPLAY  sal_det[3]         TO  FORMONLY.sal_3_det
   DISPLAY  sal_det[5]         TO  FORMONLY.sal_5_det
   DISPLAY  sal_det[7]         TO  FORMONLY.sal_7_det
   DISPLAY  sal_det[10]        TO  FORMONLY.sal_10_det
   DISPLAY  sal_det[11]        TO  FORMONLY.sal_11_det
   DISPLAY  sal_det[12]        TO  FORMONLY.sal_12_det
   DISPLAY  sal_det[13]        TO  FORMONLY.sal_13_det
   DISPLAY  sal_det[4]         TO  FORMONLY.sal_4_det
   DISPLAY  sal_det[8]         TO  FORMONLY.sal_8_det
   DISPLAY  sal_det[14]        TO  FORMONLY.sal_14_det
   DISPLAY  sal_det[15]        TO  FORMONLY.sal_15_det
   DISPLAY  sal_sum[1]         TO  FORMONLY.sal_1_sum
   DISPLAY  sal_sum[2]         TO  FORMONLY.sal_2_sum
   DISPLAY  sal_sum[3]         TO  FORMONLY.sal_3_sum
   DISPLAY  sal_sum[5]         TO  FORMONLY.sal_5_sum
   DISPLAY  sal_sum[7]         TO  FORMONLY.sal_7_sum
   DISPLAY  sal_sum[10]        TO  FORMONLY.sal_10_sum
   DISPLAY  sal_sum[11]        TO  FORMONLY.sal_11_sum
   DISPLAY  sal_sum[12]        TO  FORMONLY.sal_12_sum
   DISPLAY  sal_sum[13]        TO  FORMONLY.sal_13_sum
   DISPLAY  sal_sum[4]         TO  FORMONLY.sal_4_sum
   DISPLAY  sal_sum[8]         TO  FORMONLY.sal_8_sum
   DISPLAY  sal_sum[14]        TO  FORMONLY.sal_14_sum
   DISPLAY  sal_sum[15]        TO  FORMONLY.sal_15_sum
   DISPLAY  sal_dif[1]         TO  FORMONLY.sal_1_dif
   DISPLAY  sal_dif[2]         TO  FORMONLY.sal_2_dif
   DISPLAY  sal_dif[3]         TO  FORMONLY.sal_3_dif
   DISPLAY  sal_dif[5]         TO  FORMONLY.sal_5_dif
   DISPLAY  sal_dif[7]         TO  FORMONLY.sal_7_dif
   DISPLAY  sal_dif[10]        TO  FORMONLY.sal_10_dif 
   DISPLAY  sal_dif[11]        TO  FORMONLY.sal_11_dif 
   DISPLAY  sal_dif[12]        TO  FORMONLY.sal_12_dif 
   DISPLAY  sal_dif[13]        TO  FORMONLY.sal_13_dif 
   DISPLAY  sal_dif[4]         TO  FORMONLY.sal_4_dif
   DISPLAY  sal_dif[8]         TO  FORMONLY.sal_8_dif
   DISPLAY  sal_dif[14]        TO  FORMONLY.sal_14_dif 
   DISPLAY  sal_dif[15]        TO  FORMONLY.sal_15_dif 
   PROMPT   "  TECLEE  <ENTER>  PARA  SALIR    "    FOR   enter
   CLOSE    WINDOW   taal0104
END FUNCTION 
