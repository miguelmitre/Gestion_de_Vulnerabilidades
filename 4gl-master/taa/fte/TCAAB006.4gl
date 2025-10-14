#Proyecto          => AFORE ( MEXICO )             
#Propietario       => E.F.P.                      
#Programa TCAAB006 => PANTALLA LANZADORA DEL PROCESO TCAAB006(AF-AF COMP)
#Sistema           => TCAA                     
#Autor             => JESUS YAÑEZ MORENO
#Creado            => 14 OCTUBRE 2004
###########################################################################

DATABASE safre_af

GLOBALS
    DEFINE    g_seg_modulo           RECORD LIKE safre_af:seg_modulo.*
    DEFINE    lanza_proceso          CHAR(1000),
              g_afore_cod            CHAR(003),
              paso                   CHAR(200)
    DEFINE    g_enter                CHAR(001)

    DEFINE    reg_taa_cd_ctr_folio   RECORD  LIKE safre_af:taa_cd_ctr_folio.*

    DEFINE    g_fecha_liq_parti                 ,
              g_fecha_saldo_parti               ,
              g_fecha_trasp_desde               ,
              g_today                           DATE

    DEFINE    g_provisionada                    ,
              g_siefore                         ,
              g_siefore_fin                     ,
              g_liquidada                       SMALLINT

    DEFINE    g_precio_parti                    DEC(19,14)

    DEFINE    g_sie_inf       ARRAY[40]    OF    RECORD
              nom_siefore                       CHAR(008),
              precio_accion                     DEC(11,6)
                                      END   RECORD

END GLOBALS

GLOBALS "TCAAB006S.4gl"  #  Definicion  de  SQL

MAIN
    OPTIONS INPUT WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I

    CALL     arma_querys_TCAAB006S()
    CALL     F_010_inicio()

    WHILE TRUE
       PROMPT "ESTA SEGURO DE GENERAR EL PROCESO [S/N] ? " FOR g_enter
         IF g_enter  MATCHES "[sSnN]" THEN
            IF g_enter  MATCHES "[sS]" THEN
               EXIT WHILE
            ELSE
               ERROR  "PROCESO CANCELADO" ATTRIBUTE(REVERSE) 
               SLEEP  2
               EXIT  PROGRAM
            END IF
         END IF
    END WHILE
LET lanza_proceso = "nohup fglgo TCAAB006L ",
                                  reg_taa_cd_ctr_folio.folio             ," ",
                                  reg_taa_cd_ctr_folio.fecha_envio_saldos," ",
                                  reg_taa_cd_ctr_folio.fecha_liquidacion ," ",
                                  g_fecha_liq_parti                      ," ",
                                  g_fecha_saldo_parti                    ," ",
                                  g_fecha_trasp_desde                    ," ",
                                  g_today

LET paso = g_seg_modulo.ruta_envio CLIPPED,
           "/"                 ,
           "nohup_TCAAB006."       ,
           reg_taa_cd_ctr_folio.folio USING"&&&&&&&"
LET lanza_proceso  = lanza_proceso CLIPPED, " 1>",paso CLIPPED," 2>&1 &"

RUN lanza_proceso

END MAIN
 
FUNCTION  F_010_inicio()
#i----------------------

    DEFINE  l_folios_pendientes    SMALLINT
   PREPARE  sql_03    FROM  g_sql_03
   PREPARE  sql_04    FROM  g_sql_04
   PREPARE  sql_06    FROM  g_sql_06
   PREPARE  sql_07    FROM  g_sql_07
   PREPARE  sql_07_2  FROM  g_sql_07_2
   PREPARE  sql_10    FROM  g_sql_10
   PREPARE  sql_13    FROM  g_sql_13
   PREPARE  sql_21    FROM  g_sql_21

   EXECUTE  sql_10    INTO    g_seg_modulo.*

    LET     g_today                =  TODAY

    OPEN WINDOW TCAAB006 AT 2,2  WITH FORM "TCAAB006" ATTRIBUTE(BORDER)
    DISPLAY  "TCAAB006            TRASPASOS  AFORE-AFORE (CEDENTE)                          " AT  2,2 ATTRIBUTE(REVERSE)   
    DISPLAY  "     GENERACION DE ARCHIVO DE TRASPASOS COMPLEMENTARIOS  CEDENTE                   " AT   4,2 ATTRIBUTE(REVERSE)   
    DISPLAY  g_today USING "DD-MM-YYYY" AT 2,60 ATTRIBUTE(REVERSE)
    
    EXECUTE  sql_03      INTO  g_provisionada
    EXECUTE  sql_04      INTO  g_liquidada
    EXECUTE  sql_21      INTO  l_folios_pendientes   USING  g_provisionada
    IF       l_folios_pendientes        THEN
             PROMPT  "  YA EXISTE FOLIO PROVISIONADO <enter> Salir" FOR g_enter
             EXIT PROGRAM 
    END IF
    CALL F_020_muestra_folio_comple()
END FUNCTION

FUNCTION  F_020_muestra_folio_comple()
    DEFINE    l_registros             INTEGER 
    PREPARE  sql_38               FROM  g_sql_38
    EXECUTE  sql_38          INTO     g_siefore_fin

    CALL    F_010_1_arma_fechas_folio()

    DISPLAY BY NAME reg_taa_cd_ctr_folio.fecha_envio_saldos
    DISPLAY BY NAME reg_taa_cd_ctr_folio.fecha_liquidacion
    DISPLAY BY NAME reg_taa_cd_ctr_folio.folio 

    CALL     F_022_precio_acc_parti()
    IF       g_sie_inf[1].precio_accion        =  0   THEN
             ERROR "NO HAY PRECIO DE ACCION  DE BASICA 1:"
             PROMPT "TECLEE ENTER PARA SALIR..." FOR g_enter
             EXIT  PROGRAM
    END IF
    IF       g_sie_inf[2].precio_accion        =  0   THEN
             ERROR "NO HAY PRECIO DE ACCION  DE BASICA 2:"
             PROMPT "TECLEE ENTER PARA SALIR..." FOR g_enter
             EXIT  PROGRAM
    END IF
    IF       g_siefore_fin                     =  31      THEN
             IF       g_sie_inf[31].precio_accion        =  0   THEN
                      ERROR "NO HAY PRECIO DE ACCION  DE BASICA 31:"
                      PROMPT "TECLEE ENTER PARA SALIR..." FOR g_enter
                      EXIT  PROGRAM
             END IF
    END IF
    IF       g_precio_parti          =  0  THEN
             ERROR "NO HAY PRECIO DE PARTICIPACION DE HOY :"
             PROMPT "TECLEE ENTER PARA SALIR..." FOR g_enter
             EXIT PROGRAM
    END IF

    DISPLAY   g_sie_inf[1].precio_accion  TO  g_precio_accion_b1
    DISPLAY   g_sie_inf[2].precio_accion  TO  g_precio_accion_b2
    DISPLAY   g_sie_inf[3].precio_accion  TO  g_precio_accion_b3
    DISPLAY   g_sie_inf[4].precio_accion  TO  g_precio_accion_b4
    DISPLAY   g_sie_inf[5].precio_accion  TO  g_precio_accion_b5
    DISPLAY   g_sie_inf[6].precio_accion  TO  g_precio_accion_b6
    DISPLAY BY NAME  g_precio_parti

    LET int_flag = FALSE

    INPUT BY NAME g_fecha_trasp_desde
       AFTER FIELD g_fecha_trasp_desde
           IF     g_fecha_trasp_desde   IS  NULL    THEN
                  ERROR  " LA  FECHA  NO  PUEDE SER  NULA  "
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

    EXECUTE  sql_06  USING  g_liquidada        ,
                            g_fecha_trasp_desde
                     INTO   l_registros

    DISPLAY  BY  NAME  l_registros

END FUNCTION

FUNCTION  F_010_1_arma_fechas_folio()

   SELECT  UNIQUE  fecha_envio_saldos,fecha_liquidacion
   INTO    reg_taa_cd_ctr_folio.fecha_envio_saldos,
           reg_taa_cd_ctr_folio.fecha_liquidacion
   FROM    safre_af:taa_cd_ctr_folio
   WHERE   fecha_envio_saldos    BETWEEN  g_today   AND  g_today  +  6
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

   INSERT  INTO    glo_folio      VALUES  (0)
   SELECT  MAX(folio)
     INTO  reg_taa_cd_ctr_folio.folio
     FROM  glo_folio
END FUNCTION

FUNCTION  F_022_precio_acc_parti()
   DEFINE  l_siefore_rcv                     ,
           l_siefore_viv                     SMALLINT
   DEFINE  l_nom_siefore                     CHAR(008),
           l_razon_social                    CHAR(040),
           l_precio_accion                   DEC(11,6)
   FOR     g_siefore     =  1  TO  g_siefore_fin
           LET     g_sie_inf[g_siefore].nom_siefore      =  NULL
           LET     g_sie_inf[g_siefore].precio_accion    =  0
   END FOR
   EXECUTE  sql_13      INTO      g_afore_cod,l_razon_social

   DECLARE  cur_sie     CURSOR    FOR   sql_07
   OPEN     cur_sie     USING     g_today ,g_afore_cod
   FOREACH  cur_sie     INTO      g_siefore,l_nom_siefore,l_precio_accion
       LET     g_sie_inf[g_siefore].nom_siefore      =  l_nom_siefore  CLIPPED
       LET     g_sie_inf[g_siefore].precio_accion    =  l_precio_accion
   END FOREACH
   LET     l_siefore_viv         =  11
   EXECUTE  sql_07_2    USING    g_fecha_liq_parti ,
                                 l_siefore_viv
                        INTO     g_precio_parti

   IF       STATUS               =  NOTFOUND  THEN
            LET      g_precio_parti     =  0
   END IF
END FUNCTION
