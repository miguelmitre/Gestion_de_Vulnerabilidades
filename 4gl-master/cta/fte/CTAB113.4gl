#############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                          #
#Propietario       => E.F.P.                                                #
#Programa CTAB113  => PROVISION DE TRANSFERENCIA DE DECIMOS                 #
#Sistema           => CTA                                                   #
#Autor             => EDUARDO JOAQUIN RESENDIZ MEDINA                       #
#Fecha             => 03 DE JUNIO DE 2005                                   #
#Fecha actualiza   => 14 DE ENERO DE 2008                                   #
#Autor             => MAURO MUÑIZ CABALLERO                                 #
#    ADECUACIONES POR ULTIMO DECIMO ANTES DE CORTE POR EDAD                 #
#############################################################################

DATABASE safre_af

GLOBALS

   DEFINE v_decision      CHAR(1)
   DEFINE v_fecha_corte   DATE
   DEFINE HOY             DATE
   DEFINE g_existencia    SMALLINT
   DEFINE g_criterio      SMALLINT
   DEFINE g_proceso_cod   SMALLINT
   DEFINE g_cont_existe   INTEGER
   DEFINE g_folio         INTEGER
   DEFINE v_time          DATETIME HOUR TO SECOND
   DEFINE fecha_inicio    DATETIME HOUR TO SECOND
   DEFINE hora_inicio     DATETIME HOUR TO SECOND
   DEFINE fecha_fin       DATETIME HOUR TO SECOND
   DEFINE hora_fin        DATETIME HOUR TO SECOND
   DEFINE fecha_fin2      DATE
   DEFINE lr_decimo       RECORD LIKE cta_his_nss_decimo.*

   DEFINE v_subcuenta     SMALLINT
   DEFINE v_grupo         SMALLINT
   DEFINE v_apuntador     SMALLINT
   DEFINE fn_saldo        CHAR(50)
   DEFINE tot_acc_sie1    DECIMAL(19,6)
   DEFINE deci_acc        DECIMAL(19,6)
   DEFINE var_usr         CHAR(8)
   DEFINE vg_tm           CHAR(10)
   DEFINE G_IMPRE         CHAR(300)
   DEFINE g_param_cta     RECORD LIKE seg_modulo.*
   DEFINE v_precio_rcv1   DECIMAL(10,6)
   DEFINE v_precio_rcv2   DECIMAL(10,6)
   DEFINE v_instruccion   CHAR(200)
   DEFINE total_pesos1    DECIMAL(22,6)
   DEFINE total_acciones1 DECIMAL(22,6)
   DEFINE total_pesos2    DECIMAL(22,6)
   DEFINE total_acciones2 DECIMAL(22,6)
   DEFINE v_afore    LIKE tab_afore_local.codigo_afore

   DEFINE enter CHAR(1)

END GLOBALS

MAIN

    DEFER INTERRUPT
    OPTIONS PROMPT LINE LAST -1

    CALL STARTLOG("CTAB113.log")
    CALL inicio()
    CALL MENU()

    CLOSE WINDOW w_menu
    CLEAR SCREEN
    --SET EXPLAIN ON

    --CALL proceso_principal()

END MAIN

FUNCTION inicio()

   LET g_cont_existe = 0
   LET HOY           = TODAY
   LET v_time        = CURRENT HOUR TO SECOND
   LET fecha_inicio  = CURRENT
   LET hora_inicio   = v_time
   LET v_subcuenta   = 2
   LET v_grupo       = 0
   LET v_apuntador   = 1
   LET fecha_fin     = NULL

   SELECT USER
     INTO var_usr
     FROM systables
    WHERE tabid=1;

END FUNCTION

FUNCTION MENU()

    LET vg_tm         = "MENU "

--    CURRENT WINDOW IS w_menu
    OPEN WINDOW  w_menu AT 4,4 WITH FORM "CTAB1131" ATTRIBUTES(BORDER)
    DISPLAY " CTAB113           PROVISION TRANSFERENCIA DE DECIMOS                          " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING"DD-MM-YYYY" AT 3,64 ATTRIBUTE(REVERSE)

    MENU vg_tm
         COMMAND "Montos previos" "Despliega los Montos globales por Subcuenta"
            CALL proceso_mto_prev() 
         COMMAND "Provision" "Ejecuta por Nohup el proceso de provision"
            CALL proceso_principal()
         COMMAND "Salir" "Salir del Programa"
            EXIT MENU
            EXIT PROGRAM -1
    END MENU

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    DEFINE vcap          CHAR(1) ,
           ejecuta       CHAR(250),
           mensaje       CHAR(100),
           v_proceso_cod SMALLINT,
           v_fecha_fin   DATE

    LET mensaje = "FECHA DE CORTE CORRECTA (S/N):"

    OPEN WINDOW  w_prov AT 4,4 WITH FORM "CTAB1133" ATTRIBUTES(BORDER)
    DISPLAY " CTAB113           PROVISION TRANSFERENCIA DE DECIMOS                          " AT 3,1 ATTRIBUTE(REVERSE)

   {
    IF MONTH(TODAY) <= 6 THEN
        LET v_fecha_corte = MDY( 6,30,YEAR(TODAY) )
    ELSE
        LET v_fecha_corte = MDY( 12,31,YEAR(TODAY) )
    END IF
   }

    LET v_fecha_corte = "12/31/2007"

    INPUT BY NAME v_fecha_corte,
                  vcap  WITHOUT DEFAULTS

    BEFORE  FIELD v_fecha_corte
           DISPLAY HOY USING"DD-MM-YYYY" AT 3,64 ATTRIBUTE(REVERSE)

    AFTER FIELD v_fecha_corte
         IF v_fecha_corte IS NULL THEN
            ERROR "Fecha de corte no puede ser nula."
            SLEEP 3
            ERROR ""
            NEXT FIELD v_fecha_corte
         ELSE
            IF fn_valida_proceso ( v_fecha_corte ) THEN
               DISPLAY BY NAME g_folio
               NEXT FIELD vcap
            ELSE
               NEXT FIELD v_fecha_corte
            END IF
         END IF

    BEFORE FIELD vcap
         DISPLAY BY NAME mensaje

    AFTER FIELD vcap

         IF vcap NOT MATCHES "[SsNn]" THEN
             ERROR " SOLO (S/N)"
             SLEEP 3
             ERROR ""
             NEXT FIELD vcap
         ELSE
             IF vcap MATCHES "[Nn]" THEN
                 CLEAR FORM
                 LET v_fecha_corte = ''

                 DISPLAY BY NAME v_fecha_corte

                 NEXT FIELD v_fecha_corte
             ELSE
---  Inserta inicio del proceso de provision.

               INSERT INTO cta_ctr_decimo 
                                   VALUES ( v_fecha_corte,
                                            v_time       ,
                                            ''           ,
                                            g_folio      ,
                                            var_usr      ,
                                            g_proceso_cod
                                          )
                 EXIT INPUT
             END IF

         END IF

       ON KEY (INTERRUPT)
          LET vcap = "N"
          LET int_flag = FALSE
          EXIT INPUT

       ON KEY (CONTROL-C)
          LET vcap = "N"
          LET int_flag = FALSE
          EXIT INPUT

    END INPUT

    IF vcap = "S" THEN
       ERROR "EJECUTANDO PROGRAMA"
       SLEEP 3
       ERROR ""
       LET ejecuta = "nohup time fglgo CTAB1131 "CLIPPED, 
                     " '",v_fecha_corte,"'"," '",HOY,"' ",g_folio,
                     " 1> ",var_usr CLIPPED,".salida 2> ",var_usr CLIPPED,
                     ".error &"
       RUN ejecuta
       ERROR "EJECUTANDO PROGRAMA CTAB1131 POR NOHUP."
       SLEEP 3
       ERROR ""
    END IF

    CLOSE WINDOW w_prov

END FUNCTION

########################### MONTOS PREVIOS #################################
FUNCTION proceso_mto_prev()

    DEFINE arr_suma       ARRAY[100] OF RECORD
           subcuenta      SMALLINT,
           acciones2      DECIMAL(22,6),
           pesos2         DECIMAL(22,6),
           acciones1      DECIMAL(22,6),
           pesos1         DECIMAL(22,6)
    END RECORD

    LET HOY = TODAY
    CURRENT WINDOW IS SCREEN
    OPEN WINDOW ventana_2 AT 4,4 WITH FORM "CTAB1132" ATTRIBUTE(BORDER)

    DISPLAY " CTAB113                                                                  " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY " Ctrl-C > Salir                                        Ctrl-P > Imprimir  " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING"DD-MM-YYYY" AT 3,63 ATTRIBUTE(REVERSE)

    SELECT @precio_del_dia
      INTO v_precio_rcv1
      FROM glo_valor_accion
     WHERE @fecha_valuacion = HOY 
       AND @codigo_siefore  = 1

       IF SQLCA.SQLCODE = NOTFOUND THEN
           ERROR " PRECIOS NO CAPTURADOS PARA HOY EN SB1..."
           SLEEP 2
           ERROR ""
       END IF

    SELECT @precio_del_dia
      INTO v_precio_rcv2
      FROM glo_valor_accion
     WHERE @fecha_valuacion = HOY
       AND @codigo_siefore  = 2

       IF SQLCA.SQLCODE = NOTFOUND THEN
           ERROR ""
           SLEEP 1 
           ERROR " PRECIOS NO CAPTURADOS PARA HOY EN SB2..."
           SLEEP 2
           ERROR ""
       END IF

   {
    LET v_instruccion = "SELECT subcuenta,SUM(monto_en_acciones) ",
                        "FROM cta_saldo_decimo a, cta_ctr_cuenta b ",
                        "WHERE a.nss = b.nss ",
                        "AND   a.nss NOT IN( SELECT c.nss FROM cta_act_marca c WHERE c.marca_cod IN(120,130,150)) "
                        "AND b.ind_transferencia = 1 ",
                        "GROUP BY 1 ",
                        "ORDER BY 1"
   }

    LET v_instruccion = "SELECT subcuenta,SUM(monto_en_acciones) ",
                        "FROM dis_cuenta a, cta_ctr_cuenta b ",
                        "WHERE a.nss = b.nss ",
                        "AND a.siefore = 2 ",
                        "AND b.ind_transferencia = 1 ",
                        "GROUP BY 1 ",
                        "ORDER BY 1"

    PREPARE sumas FROM v_instruccion

    DECLARE cur_sumas CURSOR FOR sumas

    LET v_apuntador     = 1
    LET total_acciones2 = 0.0
    LET total_pesos2    = 0.0
    LET total_acciones1 = 0.0
    LET total_pesos1    = 0.0
    LET arr_suma[v_apuntador].acciones2 = 0.0
    LET arr_suma[v_apuntador].pesos2    = 0.0
    LET arr_suma[v_apuntador].acciones1 = 0.0
    LET arr_suma[v_apuntador].pesos1    = 0.0

    FOREACH cur_sumas INTO arr_suma[v_apuntador].*

       LET arr_suma[v_apuntador].pesos2    = arr_suma[v_apuntador].acciones2 * v_precio_rcv2
       LET arr_suma[v_apuntador].pesos1    = arr_suma[v_apuntador].pesos2
       LET arr_suma[v_apuntador].acciones1 = arr_suma[v_apuntador].pesos1 / v_precio_rcv1

       IF arr_suma[v_apuntador].acciones2 = "" OR
          arr_suma[v_apuntador].acciones2 IS NULL THEN
          LET total_acciones2 = 0.0
       ELSE
          LET total_acciones2 = total_acciones2 + arr_suma[v_apuntador].acciones2
       END IF

       IF arr_suma[v_apuntador].pesos2 = "" OR
          arr_suma[v_apuntador].pesos2 IS NULL THEN
          LET total_pesos2 = 0.0
       ELSE
          LET total_pesos2    = total_pesos2 + arr_suma[v_apuntador].pesos2
       END IF

       IF arr_suma[v_apuntador].acciones1 = "" OR
          arr_suma[v_apuntador].acciones1 IS NULL THEN
          LET total_acciones1 = 0.0
       ELSE
          LET total_acciones1 = total_acciones1 + arr_suma[v_apuntador].acciones1
       END IF

       IF arr_suma[v_apuntador].pesos1 = "" OR
          arr_suma[v_apuntador].pesos1 IS NULL THEN
          LET total_pesos1 = 0.0
       ELSE
          LET total_pesos1    = total_pesos1    + arr_suma[v_apuntador].pesos1
       END IF

       LET v_apuntador = v_apuntador + 1

    END FOREACH

    LET v_apuntador = v_apuntador - 1
    CALL set_count(v_apuntador)
    DISPLAY total_acciones2 TO tot_acciones2
    DISPLAY total_pesos2    TO tot_pesos2
    DISPLAY total_acciones1 TO tot_acciones1
    DISPLAY total_pesos1    TO tot_pesos1
    DISPLAY v_precio_rcv2   TO precio_sb2
    DISPLAY v_precio_rcv1   TO precio_sb1

    DISPLAY ARRAY arr_suma  TO arr_decimos.*
       ON KEY (CONTROL-P)
       CALL impresion_reporte()
       ON KEY (CONTROL-C)
          LET int_flag = FALSE
          EXIT DISPLAY
    END DISPLAY    
    CLOSE WINDOW ventana_2
    RETURN
END FUNCTION

############################## REPORTE ######################################
FUNCTION impresion_reporte()

    DEFINE hora           CHAR(8)
    DEFINE G_IMPRE        CHAR(300)
    DEFINE c_impre        CHAR(300)
    DEFINE gimpresion     CHAR(300)

    DEFINE z              RECORD
           subcuenta      LIKE cta_saldo_decimo.subcuenta,
           acciones2      DECIMAL(22,6),
           pesos2         DECIMAL(22,6),
           acciones1      DECIMAL(22,6),
           pesos1         DECIMAL(22,6)
    END RECORD

   SELECT ruta_listados
     INTO g_param_cta.ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'cta'

   SELECT codigo_afore
     INTO v_afore
     FROM tab_afore_local

    LET hora = TIME

    LET G_IMPRE = g_param_cta.ruta_listados CLIPPED,"/",
                  var_usr CLIPPED,
                  ".MONTOS_PREVIOS.",hoy USING "DDMMYY"

    START REPORT montos_previos_rpt TO G_IMPRE

   {
    LET v_instruccion = "SELECT subcuenta,SUM(monto_en_acciones) ",
                        "FROM cta_saldo_decimo a, cta_ctr_cuenta b ",
                        "WHERE a.nss = b.nss ",
                        "AND b.ind_transferencia = 1 ",
                        "GROUP BY 1 ",
                        "ORDER BY 1"
   }

    LET v_instruccion = "SELECT subcuenta,SUM(monto_en_acciones) ",
                        "FROM dis_cuenta a, cta_ctr_cuenta b ",
                        "WHERE a.nss = b.nss ",
                        "AND a.siefore = 2 ",
                        "AND b.ind_transferencia = 1 ",
                        "GROUP BY 1 ",
                        "ORDER BY 1"

    PREPARE sumas_rpt FROM v_instruccion

    DECLARE cur_sumas_rpt CURSOR FOR sumas_rpt

    LET total_acciones2 = 0.0
    LET total_pesos2    = 0.0
    LET total_acciones1 = 0.0
    LET total_pesos1    = 0.0
    LET z.acciones2 = 0.0
    LET z.pesos2    = 0.0
    LET z.acciones1 = 0.0
    LET z.pesos1    = 0.0

    FOREACH cur_sumas_rpt INTO z.*

       LET z.pesos2    = z.acciones2 * v_precio_rcv2
       LET z.pesos1    = z.pesos2
       LET z.acciones1 = z.pesos1 / v_precio_rcv1

       IF z.acciones2 = "" or
          z.acciones2 IS NULL THEN
          LET total_acciones2 = 0.0 
       ELSE
          LET total_acciones2 = total_acciones2 + z.acciones2
       END IF

       IF z.pesos2 = "" or
          z.pesos2 IS NULL THEN 
          LET total_pesos2 = 0.0
       ELSE
          LET total_pesos2    = total_pesos2 + z.pesos2
       END IF

       IF z.acciones1 = "" or
          z.acciones1 IS NULL then
          LET total_acciones1 = 0.0
       ELSE
          LET total_acciones1 = total_acciones1 + z.acciones1
       END IF

       IF z.pesos1 = "" or
          z.pesos1 IS NULL THEN
          LET total_pesos1 = 0.0
       ELSE
          LET total_pesos1    = total_pesos1    + z.pesos1
       END IF

    OUTPUT TO REPORT montos_previos_rpt(z.*)

    END FOREACH
    FINISH REPORT montos_previos_rpt

    LET gimpresion = "lp ",G_IMPRE
    --LET gimpresion = "vi ",G_IMPRE
    RUN gimpresion

    ERROR "REPORTE IMPRESO" 
--    SLEEP 2
--    ERROR ""

    LET c_impre = ' chmod 777 ', G_IMPRE CLIPPED
    RUN c_impre

END FUNCTION

FUNCTION fn_valida_proceso(l_fecha_corte)

DEFINE  l_fecha_corte     DATE
DEFINE  v_fecha_fin       DATE

DEFINE  v_status          SMALLINT

LET v_status = TRUE

---   Verifica que haya finalizado el proceso anterior
---   Provision .

    SELECT c.proceso_cod,
           c.fecha_fin  
      INTO g_proceso_cod,
           v_fecha_fin  
      FROM cta_ctr_decimo c
     WHERE c.fecha_corte = l_fecha_corte
       AND c.proceso_cod = 2

   IF g_proceso_cod IS NULL OR
      g_proceso_cod = 0 THEN
      ERROR " NO EXISTE EL PROCESO ANTERIOR PARA LA FECHA DE CORTE."
      SLEEP 3
      ERROR ""
      LET v_status = FALSE
   ELSE 
      IF v_fecha_fin IS NULL OR
         v_fecha_fin = "12/31/1899" THEN
         ERROR " PROCESO ANTERIOR AUN NO FINALIZA."
         SLEEP 3
         ERROR ""
         LET v_status = FALSE
      ELSE
         LET g_proceso_cod = NULL
         LET v_fecha_fin   = NULL

         SELECT c.proceso_cod,
                c.fecha_fin  
           INTO g_proceso_cod,
                v_fecha_fin  
           FROM cta_ctr_decimo c
          WHERE c.fecha_corte = l_fecha_corte
            AND c.proceso_cod = 3

         IF ( v_fecha_fin IS NULL OR
              v_fecha_fin = "12/31/1899" ) AND
              g_proceso_cod = 3 THEN
              ERROR "PROCESO YA EJECUTANDOSE"
              SLEEP 3
              ERROR ""
              LET v_status = FALSE
         ELSE
            IF ( v_fecha_fin IS NOT NULL AND
                 v_fecha_fin <> "12/31/1899" ) AND 
                 g_proceso_cod = 3 THEN

                 ERROR "PROCESO YA EJECUTADO."
                 SLEEP 3
                 ERROR ""
                 LET v_status = FALSE
            ELSE
             
               SET LOCK MODE TO WAIT
               LOCK TABLE glo_folio IN EXCLUSIVE MODE
               INSERT INTO glo_folio VALUES (0)
               
               SELECT MAX(folio)
                 INTO g_folio
                 FROM glo_folio

               ---UNLOCK TABLE glo_folio

               LET g_proceso_cod = 3

            END IF
         END IF
      END IF
   END IF

RETURN v_status

END FUNCTION


REPORT montos_previos_rpt(z_rpt)

    DEFINE z_rpt      RECORD
           subcuenta  LIKE cta_saldo_decimo.subcuenta,
           acciones2  DECIMAL(22,6),
           pesos2     DECIMAL(22,6),
           acciones1  DECIMAL(22,6),
           pesos1     DECIMAL(22,6)
    END RECORD

    OUTPUT
        TOP MARGIN 1
        BOTTOM MARGIN 0
        LEFT MARGIN 0
        RIGHT MARGIN 0
        PAGE LENGTH 60

    FORMAT
        PAGE HEADER
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT COLUMN 01,"AFORE: ",v_afore,
              COLUMN 90,TODAY USING "dd-mm-yyyy"
        SKIP 2 LINE
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT COLUMN 08,"               REPORTE DE MONTOS PREVIOS                    "
        SKIP 1 LINE 
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT COLUMN 01,"---------------------------------------------------------------------",
              COLUMN 01,"---------------------------------"
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT COLUMN 5,"PRECIO ACCION SB2 :",v_precio_rcv2,
              COLUMN 25,"            PRECIO ACCION SB1 :",v_precio_rcv1
        PRINT COLUMN 01,"---------------------------------------------------------------------",
              COLUMN 01,"---------------------------------"
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        SKIP 1 LINE 
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT COLUMN 01,"SUBCTA.",
              COLUMN 23,"ACCIONES",
              COLUMN 50,"PESOS",
              COLUMN 71,"ACCIONES",
              COLUMN 97,"PESOS"
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT COLUMN 01,"---------------------------------------------------------------------",
              COLUMN 01,"---------------------------------"
        SKIP 1 LINE

     ON EVERY ROW
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT COLUMN 01,z_rpt.subcuenta,
              COLUMN 02,z_rpt.acciones2,
              COLUMN 10,z_rpt.pesos2,
              COLUMN 30,z_rpt.acciones1,
              COLUMN 50,z_rpt.pesos1

     ON LAST ROW
        SKIP 1 LINE
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT COLUMN 01,"---------------------------------------------------------------------",
              COLUMN 01,"---------------------------------"
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT COLUMN 01,"TOTALES:",
              COLUMN 15, SUM(z_rpt.acciones2) USING "########&.######",
              COLUMN 39, SUM(z_rpt.pesos2)    USING "########&.######",
              COLUMN 63, SUM(z_rpt.acciones1) USING "########&.######",
              COLUMN 87, SUM(z_rpt.pesos1)    USING "########&.######"

     PAGE TRAILER
      SKIP 2 LINE
      PRINT COLUMN 90,"Pagina:",PAGENO USING "<<<<"
      PAUSE "Presione enter para continuar."

END REPORT

