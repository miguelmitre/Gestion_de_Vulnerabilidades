#################################################################################
#Propietario       => E.F.P.                                                    #
#Programa TCAAB0171=> PANTALLA LANZADORA DEL PROCESO TCAAB0171(AF-AF COMP)      #
#Sistema           => TCAA                                                      #
#Autor             => JOSE FRANCISCO LUGO CORNEJO                               #
#Creado            => 26 ENERO  2010                                            #
#################################################################################
#Modificacion      => Se crea version especial TODAS las cuentas                #
#13/julio/2012     => Alejandro Chagoya Salazar CPL-942                         #
#################################################################################

DATABASE safre_af

GLOBALS
    DEFINE    g_seg_modulo           RECORD LIKE safre_af:seg_modulo.*
    DEFINE    lanza_proceso          CHAR(1000),
              g_afore_cod            CHAR(003),
              paso                   CHAR(200)
    DEFINE    g_enter                CHAR(001)

    DEFINE    reg_taa_cd_ctr_folio   RECORD  LIKE safre_af:taa_cd_ctr_folio.*

    DEFINE    g_fecha_liq_parti                 ,
              g_fecha_saldo_parti               DATE

    DEFINE    g_provisionada                    ,
              g_siefore                         ,
              g_siefore_ini                     ,
              g_siefore_fin                     ,
              g_procedente                      ,
              g_liquidada                       SMALLINT

    DEFINE    g_cuentas_pendientes              INTEGER
    DEFINE    g_sie_inf       ARRAY[40]    OF    RECORD
              precio_accion                     DEC(19,14)
                                      END   RECORD

END GLOBALS

GLOBALS "TCAAB0171S.4gl"  #  Definicion  de  SQL

MAIN
    OPTIONS INPUT WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I
    CALL     STARTLOG("TCAAB0171.log")
    CALL     F_010_inicio()
END MAIN
 
FUNCTION  F_010_inicio()
#i----------------------

    CALL    arma_querys_TCAAB017S()
    CALL    F_015_prepara_querys_TCAAB017()
    LET     g_today                =  TODAY
    LET     g_siefore_ini          =  1
    LET     g_procedente           =  101
    LET     g_liquidada            =  103
    OPEN WINDOW TCAAB017 AT 2,2  WITH FORM "TCAAB017" ATTRIBUTE(BORDER)
    DISPLAY  "TCAAB0171           TRASPASOS  AFORE-AFORE (CEDENTE)                          " AT  2,2 ATTRIBUTE(REVERSE)   
    DISPLAY  "     CLASIFICA CUENTAS PARA TRASPASO AFO-AFO CEDENTE COMPLEMENTARIO " AT   4,2 ATTRIBUTE(REVERSE)   
    DISPLAY  g_today USING "DD-MM-YYYY" AT 2,60 ATTRIBUTE(REVERSE)
    CALL     F_020_muestra_folio_comple()
    IF       g_cuentas_pendientes       >  0   THEN
             WHILE   TRUE
                     PROMPT    " YA EXISTEN CUENTAS CLASIFICADAS CON <N>",
                               " CANCELA O <S> RECLASIFICA [S/N]? "
                               FOR      g_enter
                     IF        g_enter      MATCHES   "[sSnN]"       THEN
                               IF       g_enter      MATCHES   "[nN]"    THEN
                                        ERROR  "PROCESO CANCELADO" ATTRIBUTE(REVERSE) 
                                        SLEEP  2
                                        EXIT  PROGRAM
                               ELSE
                                        DELETE    FROM   safre_tmp:taa_cd_op_12
                                        EXIT  WHILE
                               END IF
                     END  IF
            END      WHILE
    END IF
    LET    lanza_proceso = "nohup fglgo TCAAB0171L ",
                                  reg_taa_cd_ctr_folio.fecha_envio_saldos," ",
                                  g_fecha_liq_parti                      ," ",
                                  g_fecha_saldo_parti                    ," ",
                                  g_fecha_trasp_desde                    ," ",
                                  g_today

    LET    paso = g_seg_modulo.ruta_envio CLIPPED,
           "/"                 ,
           "nohup_TCAAB0171."       ,
           g_today  USING  "DD-MM-YYYY"
    LET    lanza_proceso  = lanza_proceso CLIPPED, " 1>",paso CLIPPED," 2>&1 &"

    RUN lanza_proceso
END FUNCTION

FUNCTION   F_015_prepara_querys_TCAAB017()
   PREPARE  sql_06           FROM  g_sql_06
   PREPARE  sql_07           FROM  g_sql_07
   PREPARE  sql_07_2         FROM  g_sql_07_2
   PREPARE  sql_10           FROM  g_sql_10
   PREPARE  sql_13           FROM  g_sql_13
   PREPARE  sql_21           FROM  g_sql_21
   EXECUTE  sql_10           INTO  g_seg_modulo.*
END FUNCTION

FUNCTION  F_020_muestra_folio_comple()
    DEFINE    l_registros             INTEGER 
    PREPARE  sql_38               FROM  g_sql_38
    EXECUTE  sql_38          INTO     g_siefore_fin

    CALL    F_010_1_arma_fechas_folio()

    DISPLAY BY NAME reg_taa_cd_ctr_folio.fecha_envio_saldos
    DISPLAY BY NAME reg_taa_cd_ctr_folio.fecha_liquidacion

    CALL     F_022_precio_acc_parti()
    FOR      g_siefore             =  g_siefore_ini      TO  g_siefore_fin
            IF       g_siefore           =  11  OR
                     g_siefore           =  12  THEN
                     CONTINUE  FOR
            END IF
            IF       g_sie_inf[g_siefore].precio_accion        =  0   THEN
                     ERROR    "  NO HAY PRECIO DE ACCION SIEFORE RCV BASICA ",
                               g_siefore   USING   "##","                     "
                     PROMPT   "TECLEE ENTER PARA SALIR..."     FOR  g_enter
                     EXIT     PROGRAM
            END IF
    END FOR

    IF       g_sie_inf[11].precio_accion          =  0  THEN
             ERROR "NO HAY PRECIO DE PARTICIPACION DE HOY :"
             PROMPT "TECLEE ENTER PARA SALIR..." FOR g_enter
             EXIT PROGRAM
    END IF
    IF       g_sie_inf[12].precio_accion          =  0  THEN
             ERROR "NO HAY PRECIO DE AIVS DE HOY :"
             PROMPT "TECLEE ENTER PARA SALIR..." FOR g_enter
             EXIT PROGRAM
    END IF
    DISPLAY   g_sie_inf[1].precio_accion   TO  g_precio_accion_b1
    DISPLAY   g_sie_inf[2].precio_accion   TO  g_precio_accion_b2
    DISPLAY   g_sie_inf[3].precio_accion   TO  g_precio_accion_b3
    DISPLAY   g_sie_inf[4].precio_accion   TO  g_precio_accion_b4
    DISPLAY   g_sie_inf[5].precio_accion   TO  g_precio_accion_b5
    DISPLAY   g_sie_inf[6].precio_accion   TO  g_precio_accion_b6
    DISPLAY   g_sie_inf[11].precio_accion  TO  precio_parti
    DISPLAY   g_sie_inf[12].precio_accion  TO  precio_aivs
    DISPLAY   g_sie_inf[13].precio_accion  TO  precio_udis


    LET       int_flag                  =  FALSE
    LET       g_fecha_trasp_desde       =  g_today
    INPUT BY NAME g_fecha_trasp_desde   WITHOUT  DEFAULTS
       BEFORE  FIELD  g_fecha_trasp_desde
                IF      g_fecha_trasp_desde    =  g_today   THEN
                        ERROR  "  CON  FECHA DE HOY GENERA UNICAMANTE",
                               "  COMPLEMENTARIOS  DE  << INDEBIDOS >>        "
                END IF
       AFTER   FIELD  g_fecha_trasp_desde
               IF     g_fecha_trasp_desde   IS  NULL    THEN
                       ERROR  " LA  FECHA  NO  PUEDE SER  NULA  "
               END IF
               IF      g_fecha_trasp_desde    <  g_today   THEN
                       ERROR  "  CON  FECHA MENOR A HOY GENERA  TODO",
                              " INCLUYENDO INDEBIDOS SI LOS HAY    "
               END IF

               NEXT    FIELD   g_fecha_trasp_desde
       ON KEY (ESC)
               LET     int_flag      =  FALSE
               EXIT INPUT
       ON KEY (INTERRUPT)
               LET    int_flag       =  TRUE
               ERROR"Proceso cancelado ..."
               SLEEP 2
               EXIT PROGRAM
    END INPUT


    EXECUTE  sql_06  USING  g_fecha_trasp_desde
                     INTO   l_registros

   EXECUTE  sql_21           INTO  g_cuentas_pendientes
                             USING  g_fecha_trasp_desde
    DISPLAY  BY  NAME  l_registros

END FUNCTION

FUNCTION  F_010_1_arma_fechas_folio()
   SELECT  UNIQUE  fecha_envio_saldos,fecha_liquidacion
   INTO    reg_taa_cd_ctr_folio.fecha_envio_saldos,
           reg_taa_cd_ctr_folio.fecha_liquidacion
   FROM    safre_af:taa_cd_ctr_folio
   WHERE   fecha_envio_saldos    BETWEEN  g_today   AND  g_today  +  5
   AND     tipo_traspaso            =  1
   IF      STATUS                   =   NOTFOUND     THEN
           ERROR "  NO EXISTE  FOLIO PROCEDENTE PARA TOMAR LAS FECHAS PARA EL PROCESO  "
           PROMPT "  TECLEE ENTER PARA SALIR DEL PROGRAMA  ? "  FOR  g_enter
           EXIT PROGRAM
   END IF
           LET     g_fecha_liq_parti        =
           MDY(MONTH(reg_taa_cd_ctr_folio.fecha_liquidacion), "01" ,
           YEAR(reg_taa_cd_ctr_folio.fecha_liquidacion))

   LET     g_fecha_saldo_parti   =
           MDY(MONTH(g_today),"01",YEAR(g_today))

   LET     reg_taa_cd_ctr_folio.calculo_interes          =  1

END FUNCTION

FUNCTION  F_022_precio_acc_parti()
   DEFINE  l_siefore_rcv                     SMALLINT
   DEFINE  l_nom_siefore                     CHAR(008),
           l_razon_social                    CHAR(040),
           l_precio_accion                   DEC(11,6)

   DECLARE  cur_sie     CURSOR    FOR   sql_07
   OPEN     cur_sie     USING     g_today 
   FOREACH  cur_sie     INTO      g_siefore,l_precio_accion
       LET     g_sie_inf[g_siefore].precio_accion    =  l_precio_accion
   END FOREACH
   DECLARE  cur_sie_viv      CURSOR   FOR   sql_07_2
   OPEN     cur_sie_viv       USING   g_fecha_liq_parti
   FOREACH  cur_sie_viv        INTO   g_siefore,l_precio_accion
            LET      g_sie_inf[g_siefore].precio_accion   =  l_precio_accion
   END FOREACH

END FUNCTION
