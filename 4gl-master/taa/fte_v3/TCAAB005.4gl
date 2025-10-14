###########################################################################
#Proyecto          => AFORE ( MEXICO )             
#Propietario       => E.F.P.                      
#Programa TCAAB005 => PANTALLA LANZADORA DEL PROCESO TCAAB005L(OP. 09 )
#Sistema           => TCAA                     
#Autor             => JOSE FRANCISCO LUGO CORNEJO
#Creado            => 31 MARZO  2010
###########################################################################

DATABASE safre_af

GLOBALS
    DEFINE    lanza_proceso          CHAR(1000)

END GLOBALS

GLOBALS "TCAAB005S.4gl"  #  Definicion  de  SQL

MAIN
    OPTIONS INPUT WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I
    CALL     STARTLOG("TCAAB005L.log")
    CALL     F_010_inicio()
END MAIN
 
FUNCTION    F_010_inicio()
   DEFINE   l_archivo                     CHAR(100)
   CALL     F_910_arma_querys_TCAAB005S()
   CALL     F_920_trae_parametros()
   CALL     F_015_prepara_querys()
   CALL     F_030_busca_folio_pendiente()
   LET      lanza_proceso       =  "nohup  fglgo   TCAAB005L ",
                                   reg_ctr_folio.folio
   LET      l_archivo           =  g_seg_modulo.ruta_envio    CLIPPED, "/",
           "nohup_TCAAB005."       ,
            reg_ctr_folio.folio    USING   "&&&&&&&"
   LET      lanza_proceso     =  lanza_proceso CLIPPED, " 1>",
            l_archivo     CLIPPED," 2>&1 &"
   DISPLAY  " ARCHIVO nohup :",l_archivo    AT  19,1    ATTRIBUTE(REVERSE)
   PROMPT   " PARA MONITOREAR EL PROCESO: TECLEE  <Enter> PARA CONTINUAR ?... "
             FOR   g_enter
   RUN      lanza_proceso
END FUNCTION

FUNCTION    F_015_prepara_querys()
   DEFINE   l_folios_pendientes              SMALLINT
   PREPARE  sql_05          FROM  g_sql_05
   PREPARE  sql_06          FROM  g_sql_06
   PREPARE  sql_21          FROM  g_sql_21
   PREPARE  sql_53          FROM  g_sql_53
END FUNCTION

FUNCTION    F_030_busca_folio_pendiente()
   DEFINE   l_verifica_prov                     INTEGER
   DEFINE   l_tot_registros                     INTEGER
   OPEN     WINDOW    TCAAB005       AT  2,2
            WITH      FORM     "TCAAB005"  ATTRIBUTE(BORDER)
   DISPLAY  "TCAAB005            TRASPASOS  AFORE-AFORE (CEDENTE)       ",
            "                   "    AT  2,2   ATTRIBUTE(REVERSE)
   DISPLAY  "     GENERACION DE ARCHIVO DE TRASPASOS DE CUENTAS INDIVIDUALES ",
            "                   "    AT  4,2   ATTRIBUTE(REVERSE)
   DISPLAY  g_today     USING  "DD-MM-YYYY"   AT  2,60  ATTRIBUTE(REVERSE)

   DECLARE cur_sql_05 CURSOR FOR sql_05

   FOREACH cur_sql_05 USING g_recibido
                      INTO  reg_ctr_folio.*
         EXECUTE  sql_06 USING reg_ctr_folio.folio,g_recibido
                         INTO l_tot_registros

         CASE reg_ctr_folio.tipo_traspaso
         WHEN 1
            DISPLAY "PROMOTOR" TO FORMONLY.tipo_traspaso
            EXIT CASE
         WHEN 3
            DISPLAY "INTERNET" TO FORMONLY.tipo_traspaso
            EXIT CASE
         WHEN 4
            DISPLAY "DIVERSOS" TO FORMONLY.tipo_traspaso
            EXIT CASE
         END CASE

            DISPLAY BY NAME reg_ctr_folio.fecha_presentacion
            DISPLAY BY NAME reg_ctr_folio.fecha_envio_saldos
            DISPLAY BY NAME reg_ctr_folio.fecha_liquidacion
            DISPLAY BY NAME l_tot_registros
            DISPLAY BY NAME reg_ctr_folio.folio

            WHILE TRUE

                 PROMPT " ES EL FOLIO DESEADO TECLEE [S/N] ?..." FOR g_enter
                 IF g_enter MATCHES "[sSnN]" THEN
                    IF g_enter MATCHES "[sS]" THEN
                       EXIT FOREACH
                    ELSE
                       LET reg_ctr_folio.folio = 0
                       CONTINUE  FOREACH
                    END IF
                 END  IF

            END WHILE

   END FOREACH

   IF (reg_ctr_folio.folio IS NULL OR 
       reg_ctr_folio.folio = cero   ) THEN

            ERROR  "  NO  HAY  ARCHIVO  PENDIENTE  PARA  GENERAR:   "
            PROMPT "  TECLEE ENTER PARA SALIR..."  FOR  g_enter
            EXIT PROGRAM

   END IF

   LET g_fecha_liq_parti = MDY(MONTH(reg_ctr_folio.fecha_liquidacion),
                           "01",
                           YEAR(reg_ctr_folio.fecha_liquidacion))

   LET g_fecha_saldo_parti = MDY(MONTH(g_today),"01",YEAR(g_today))

   CALL F_930_arma_precios_acc_parti()

   WHILE TRUE
       PROMPT "ESTA SEGURO DE GENERAR EL PROCESO [S/N] ? " FOR g_enter
         IF g_enter MATCHES "[sSnN]" THEN
            IF g_enter MATCHES "[sS]" THEN
               EXECUTE  sql_53
               USING reg_ctr_folio.folio
               INTO  l_verifica_prov

               IF (l_verifica_prov = 0 OR l_verifica_prov IS NULL) THEN
                 EXIT WHILE
               ELSE
                 PROMPT "ERROR.. REGISTROS PROVISIONADOS LLAME A EFP <Enter> ",
                        " para salir..." FOR g_enter 
                  EXIT PROGRAM
               END IF
            ELSE
               EXIT PROGRAM
            END IF
         END  IF
   END WHILE

END FUNCTION

