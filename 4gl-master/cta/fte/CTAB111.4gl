##############################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                            #
#Propietario       => E.F.P.                                                 #
#Programa CTAB111  => EJECUTA CTAB1111                                       #
#Por               => EDUARDO JOAQUIN RESENDIZ MEDINA                        #
#Fecha creacion    => 16 DE JUNIO DE 2005                                    #
##############################################################################
DATABASE safre_af

GLOBALS

    DEFINE g_param_taa RECORD LIKE seg_modulo.*

    DEFINE
        fecha_corte    DATE,
        HOY            DATE,
        v_fecha_anio   CHAR(4),
        v_desc_fecha   CHAR(10),
        v_fecha_comp1  CHAR(10),
        v_fecha_comp2  CHAR(10),
        v_fecha_comp3  CHAR(10),
        v_fecha_comp4  CHAR(10),
        fecha_final    CHAR(10),
        v_fecha_fin2   DATE,
        v_fecha_fin4   DATE

    DEFINE
        enter         CHAR(1),
        ejecuta       CHAR(120),
        instrucc      CHAR(150)

    DEFINE v_decision CHAR(1)
    DEFINE vcap       CHAR(1)
    DEFINE v_cap      CHAR(1)

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS INPUT WRAP,
    PROMPT LINE LAST

    CALL STARTLOG("CTAB111.log")
    CALL proceso_principal()

END MAIN

FUNCTION proceso_principal()

    DEFINE cadena CHAR(100)

    LET HOY          = TODAY

    OPEN WINDOW win_1 AT 4,4 WITH FORM "CTAB1111" ATTRIBUTE(BORDER)
    DISPLAY "CTAB111      PROCESO INICIAL DE TRANSFERENCIA DE DECIMOS            " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY " < CTRL-C > Salir " AT 1,26
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,60 ATTRIBUTE(REVERSE)

    LET v_desc_fecha = HOY
    LET v_fecha_anio = v_desc_fecha[7,10]
    LET v_fecha_comp1 = "01/01/",v_fecha_anio
    LET v_fecha_comp2 = "06/30/",v_fecha_anio
    LET v_fecha_comp3 = "07/01/",v_fecha_anio
    LET v_fecha_comp4 = "12/31/",v_fecha_anio
    LET v_fecha_fin2  = v_fecha_comp2
    LET v_fecha_fin4  = v_fecha_comp4

    IF HOY >= v_fecha_comp1 AND
       hoy <= v_fecha_comp2 THEN
       LET fecha_final = "06/30/",v_fecha_anio
       LET fecha_corte = fecha_final
       DISPLAY BY NAME fecha_corte
    ELSE
       IF HOY >= v_fecha_comP3 AND
          HOY <= v_fecha_comp4 THEN
          LET fecha_final = "31/12/",v_fecha_anio
          LET fecha_corte = fecha_final
          DISPLAY BY NAME fecha_corte
       END IF
    END IF

   WHILE TRUE

   PROMPT "FECHA CORRECTA ? (S/N) :" FOR vcap

     IF UPSHIFT(vcap) = "S" THEN
          LET instrucc = "SELECT fecha_corte,proceso_cod ",
                         "FROM cta_ctr_decimo ",
                         "WHERE proceso_cod = 1 ",
                         "AND fecha_corte = ","'",fecha_corte,"'"

          PREPARE primera_ins FROM instrucc
          EXECUTE primera_ins

          IF SQLCA.SQLCODE = 0 THEN
             ERROR "PROCESO CANCELADO, YA EXISTE UN PROCESO PARA ESTA FECHA... "
             SLEEP 3
             EXIT WHILE
             --EXIT PROGRAM -1
          ELSE

             ERROR "EJECUTANDO PROGRAMA"
             SLEEP 3
             ERROR ""
             LET ejecuta = "nohup time fglgo CTAB1111 "CLIPPED, " '",fecha_corte,"'"," ","&"CLIPPED
             RUN ejecuta
             ERROR "PROGRAMA EJECUTADO"
             SLEEP 3
             ERROR ""

             PROMPT "PRESIONE <enter> PARA SALIR" FOR  v_cap
             EXIT WHILE
             --EXIT PROGRAM -1

          END IF
     ELSE
          IF UPSHIFT(vcap) = "N" THEN
             CALL captura_fecha()
             PROMPT "PRESIONE <enter> PARA SALIR" FOR  v_cap
             EXIT WHILE
             --EXIT PROGRAM -1
          ELSE
             IF UPSHIFT(vcap) <> "S" OR
                UPSHIFT(vcap) <> "N" OR
                UPSHIFT(vcap) =  ""  OR
                UPSHIFT(vcap) IS NULL THEN
                 ERROR "Solo debe presionar S o N"
                 SLEEP 3
                 ERROR ""
             END IF
          END IF
     END IF
 END WHILE

END FUNCTION

FUNCTION captura_fecha()

    LET fecha_corte = ""

    INPUT BY NAME fecha_corte WITHOUT DEFAULTS

    AFTER FIELD fecha_corte
        IF fecha_corte IS NULL THEN
            ERROR "FECHA CORTE NO PUEDE SER NULA"
            NEXT FIELD fecha_corte
        ELSE
            IF fecha_corte = v_fecha_fin2 OR
               fecha_corte = v_fecha_fin4 THEN
               LET instrucc = "SELECT fecha_corte,proceso_cod ",
                              "FROM cta_ctr_decimo ",
                              "WHERE proceso_cod = 1 ",
                              "AND fecha_corte = ","'",fecha_corte,"'"

               PREPARE segunda_ins FROM instrucc
               EXECUTE segunda_ins

               IF SQLCA.SQLCODE = 0 THEN
                  ERROR "PROCESO CANCELADO, YA EXISTE UN PROCESO PARA ESTA FECHA..."
                  SLEEP 3
                  ERROR ""
                  EXIT PROGRAM -1
               END IF

               ERROR "EJECUTANDO PORGRAMA"
               SLEEP 3
               ERROR ""
               LET ejecuta = "nohup time fglgo CTAB1111 "CLIPPED, " '",fecha_corte,"'"," ","&"CLIPPED
               RUN ejecuta
               ERROR "PROGRAMA EJECUTADO"
               SLEEP 3
               ERROR ""
               RETURN
            ELSE
               ERROR "FECHA CAPTURADA ERRONEA"
               SLEEP 2
               ERROR ""
               NEXT FIELD fecha_corte
               PROMPT "ENTER PARA SALIR" FOR vcap

            END IF
        END IF

    ON KEY ( INTERRUPT, CONTROL - C )
       ERROR "PROCESO INTERRUMPIDO..."
       SLEEP 3
       ERROR ""
    EXIT PROGRAM

    EXIT INPUT
    END INPUT

    RETURN

END FUNCTION


