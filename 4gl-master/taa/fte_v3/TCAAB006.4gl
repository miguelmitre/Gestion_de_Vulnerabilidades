#Proyecto          => AFORE ( MEXICO )             
#Propietario       => E.F.P.                      
#Programa TCAAB006 => PANTALLA LANZADORA DEL PROCESO TCAAB006(AF-AF COMP)
#Sistema           => TCAA                     
#Autor             => JESUS YAÑEZ MORENO
#Creado            => 14 OCTUBRE 2004
#  Version:  090909  IMPLEM. del desc. total de cred. en garantia         #
###########################################################################

DATABASE safre_af

GLOBALS
    DEFINE    lanza_proceso          CHAR(1000),
              paso                   CHAR(200)

END GLOBALS

GLOBALS "TCAAB006S.4gl"  #  Definicion  de  SQL

MAIN
    OPTIONS INPUT WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I
    CALL     STARTLOG("TCAAB006.log")
    CALL     F_010_inicio()
END MAIN
 
FUNCTION  F_010_inicio()
   CALL     arma_querys_TCAAB006S()
   CALL     F_920_trae_parametros()
   LET      g_siefore_ini          =  1
   OPEN WINDOW TCAAB006 AT 2,2  WITH FORM "TCAAB006" ATTRIBUTE(BORDER)
   DISPLAY  "TCAAB006            TRASPASOS  AFORE-AFORE (CEDENTE)                          " AT  2,2 ATTRIBUTE(REVERSE)   
   DISPLAY  "     GENERACION DE ARCHIVO DE TRASPASOS COMPLEMENTARIOS  CEDENTE                   " AT   4,2 ATTRIBUTE(REVERSE)   
   DISPLAY  g_today USING "DD-MM-YYYY" AT 2,60 ATTRIBUTE(REVERSE)
   CALL     F_020_muestra_folio_comple()
   CALL     F_930_arma_precios_acc_parti()
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
                        reg_ctr_folio.folio             ," ",
                        reg_ctr_folio.fecha_envio_saldos," ",
                        reg_ctr_folio.fecha_liquidacion ," ",
                        g_fecha_liq_parti                      ," ",
                        g_fecha_saldo_parti                    ," ",
                        g_today

   LET     paso        =  g_seg_modulo.ruta_envio CLIPPED,
           "/"                 ,
           "nohup_TCAAB006."       ,
           reg_ctr_folio.folio USING"&&&&&&&"
   LET     lanza_proceso  = lanza_proceso CLIPPED, " 1>",paso CLIPPED," 2>&1 &"
   DISPLAY  " ARCHIVO nohup :",paso    AT  20,1    ATTRIBUTE(REVERSE)
   PROMPT   " PARA MONITOREAR EL PROCESO: TECLEE  <Enter> PARA CONTINUAR ?... "
             FOR   g_enter
   RUN     lanza_proceso
END FUNCTION
 
FUNCTION  F_020_muestra_folio_comple()
   DEFINE    l_registros             INTEGER 
   CALL    F_010_1_arma_fechas_folio()
   PREPARE  sql_06          FROM  g_sql_06
   EXECUTE  sql_06          INTO  l_registros
   IF      l_registros             =  0      THEN
           PROMPT  " NO HAY REGISTROS PARA COMPLEMENTARIOS TECLEE <ENTER> PARA SALIR "
                      FOR  g_enter
            EXIT   PROGRAM
    END IF

    DISPLAY  "COMPLEMENTARIO  (OP 12)"         TO  tipo_traspaso
    DISPLAY  BY  NAME  l_registros
    DISPLAY reg_ctr_folio.fecha_envio_saldos   TO  fecha_envio_saldos
    DISPLAY reg_ctr_folio.fecha_liquidacion    TO  fecha_liquidacion
    DISPLAY reg_ctr_folio.folio                TO  folio
END FUNCTION

FUNCTION  F_010_1_arma_fechas_folio()

   SELECT  UNIQUE  fecha_envio_saldos,fecha_liquidacion
   INTO    reg_ctr_folio.fecha_envio_saldos,
           reg_ctr_folio.fecha_liquidacion
   FROM    safre_af:taa_cd_ctr_folio
   WHERE   fecha_envio_saldos    BETWEEN  g_today   AND  g_today  +  5
   AND     tipo_traspaso           IN(1,4)
   IF      STATUS                   =   NOTFOUND     THEN
           ERROR "  NO EXISTE  FOLIO PROCEDENTE PARA TOMAR LAS FECHAS PARA EL PROCESO  "
           PROMPT "  TECLEE ENTER PARA SALIR DEL PROGRAMA  ? "  FOR  g_enter
           EXIT PROGRAM
   END IF
           LET     g_fecha_liq_parti        =
           MDY(MONTH(reg_ctr_folio.fecha_liquidacion), "01" ,
           YEAR(reg_ctr_folio.fecha_liquidacion))

   LET     g_fecha_saldo_parti   =
           MDY(MONTH(g_today),"01",YEAR(g_today))

   LET     reg_ctr_folio.calculo_interes          =  1

   INSERT  INTO    glo_folio      VALUES  (0)
   SELECT  MAX(folio)
     INTO  reg_ctr_folio.folio
     FROM  glo_folio
END FUNCTION

