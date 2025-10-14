###############################################################################
#Proyecto          => SISTEMA DE AFORES ( MEXICO )                            #
#Modulo            => PRO                                                     #
#Programa PROM003  => CONSULTA SOLICITUDES DE CANDIDATOS A PROMOTOR           #
#Por               => FRANCO ESTEBAN ULLOA VIDELA                             #
#Fecha             => 8 DE ENERO DEL 2000                                     #
#Actualizacion     => LAURA EUGENIA CORTES GUZMAN                             #
#Fecha actualiz.   => 28 DE OCTUBRE DEL 2004                                  #
#Modificado Por    => Isabel Fonseca Frias                                    #
#Fecha             => 28 de Febrero del 2008                                  #
#Observacion       => Se agregaron modificaciones de acuerdo a MPT version    #
#                  => 3.0   (v1)                                              #
#Modificado Por    => Isabel Fonseca Frias                                    #
#Fecha             => 28 de septiembre del 2009                               #
#Observacion       => Se agregaron modificaciones de acuerdo a MPT            #
#                  => con fecha 29/07/2009                                    #
#                  => (v10)                                                   #
###############################################################################
DATABASE safre_af 
GLOBALS
    DEFINE  reg  RECORD #glo #reg
              status_interno       LIKE pro_solicitud.status_interno,
              desc_status_corta    LIKE pro_status_interno.desc_status_corta,
              nro_solicitud        LIKE pro_solicitud.nro_solicitud,
              codven               LIKE pro_solicitud.codven,
              cod_promotor         LIKE pro_solicitud.cod_promotor,
              seguro               LIKE pro_solicitud.seguro,
              rfc                  LIKE pro_solicitud.rfc,
              unico                LIKE pro_solicitud.unico,
              paterno              LIKE pro_solicitud.paterno,
              materno              LIKE pro_solicitud.materno,
              nombres              LIKE pro_solicitud.nombres,
              diag_proceso         LIKE pro_solicitud.diag_proceso,
              fnaci                LIKE pro_solicitud.fnaci,
              fecha_baja           LIKE pro_solicitud.fecha_baja,
              fingre               LIKE pro_solicitud.fingre,
              fenvio               LIKE pro_solicitud.fenvio,
              fecha_registro       LIKE pro_solicitud.fecha_registro,
              fecha_proceso        LIKE pro_solicitud.fecha_proceso,
              resuelva             LIKE pro_solicitud.resuelva,
              horas_capacit        LIKE pro_solicitud.horas_capacit,
--              tipo_recibo          LIKE pro_solicitud.tipo_recibo,
              escolar              LIKE pro_solicitud.escolar,
              sexo                 CHAR (01),                      --(v10)
              edo_naci             CHAR (02),                      --(v10)
              calle                LIKE pro_solicitud.calle,
              numero               LIKE pro_solicitud.numero,
              dpto                 LIKE pro_solicitud.dpto,
              codpos               LIKE pro_solicitud.codpos,
              colonia              LIKE pro_solicitud.colonia,
              deleg                LIKE pro_solicitud.deleg,
              delegdesc            CHAR (20),
              ciudad               LIKE pro_solicitud.ciudad,
              ciudaddesc           CHAR (20),
              estado               LIKE pro_solicitud.estado,
              estadodesc           CHAR (20),
              fono                 LIKE pro_solicitud.fono
            END RECORD
        
    DEFINE reg2 RECORD #glo #reg2
             nro_solicitud         LIKE pro_solicitud.nro_solicitud,
             codven                LIKE pro_solicitud.codven,
             cod_promotor          LIKE pro_solicitud.cod_promotor,
             seguro                LIKE pro_solicitud.seguro,
             rfc                   LIKE pro_solicitud.rfc,
             unico                 LIKE pro_solicitud.unico,
             paterno               LIKE pro_solicitud.paterno,
             materno               LIKE pro_solicitud.materno,
             nombres               LIKE pro_solicitud.nombres,
             diag_proceso          LIKE pro_solicitud.diag_proceso,
             fecha_baja            LIKE pro_solicitud.fecha_baja,
             fingre                LIKE pro_solicitud.fingre,
             fenvio                LIKE pro_solicitud.fenvio,
             fecha_registro        LIKE pro_solicitud.fecha_registro,
             fecha_proceso         LIKE pro_solicitud.fecha_proceso,
             resuelva              LIKE pro_solicitud.resuelva,
             horas_capacit         LIKE pro_solicitud.horas_capacit,
--             tipo_recibo           LIKE pro_solicitud.tipo_recibo,
             escolar               LIKE pro_solicitud.escolar,
             sexo                  CHAR(1),                   --(v10)
             edo_naci              CHAR(2),                   --(v10)
             calle                 LIKE pro_solicitud.calle,
             numero                LIKE pro_solicitud.numero,
             dpto                  LIKE pro_solicitud.dpto,
             codpos                LIKE pro_solicitud.codpos,
             colonia               LIKE pro_solicitud.colonia,
             deleg                 LIKE pro_solicitud.deleg,
             delegdesc             CHAR(20),
             ciudad                LIKE pro_solicitud.ciudad,
             ciudaddesc            CHAR(20),
             estado                LIKE pro_solicitud.estado,
             estadodesc            CHAR(20),
             fono                  LIKE pro_solicitud.fono,
             fnaci                 LIKE pro_solicitud.fnaci,
             consecutivo           INTEGER            ,
             folio                 LIKE pro_solicitud.folio,
             agenc_cod             LIKE pro_solicitud.agenc_cod,
             status                LIKE pro_solicitud.status,
             status_interno        LIKE pro_solicitud.status_interno,
             num_lote              LIKE pro_solicitud.num_lote,
             nip                   LIKE pro_solicitud.nip,
             sup                   LIKE pro_solicitud.sup,
             nivel                 LIKE pro_solicitud.nivel 
           END RECORD

    DEFINE l_record ARRAY[20000] OF RECORD #glo #l_record
             status_interno        LIKE pro_solicitud.status_interno,
             desc_status_corta     CHAR(25),
             nro_solicitud         LIKE pro_solicitud.nro_solicitud,
             codven                LIKE pro_solicitud.codven,
             cod_promotor          LIKE    pro_solicitud.cod_promotor,
             seguro                LIKE pro_solicitud.seguro,
             rfc                   LIKE pro_solicitud.rfc,
             unico                 LIKE pro_solicitud.unico,
             paterno               LIKE pro_solicitud.paterno,
             materno               LIKE pro_solicitud.materno,
             nombres               LIKE pro_solicitud.nombres,
             diag_proceso          LIKE pro_solicitud.diag_proceso,
             fnaci                 LIKE pro_solicitud.fnaci,
             fecha_baja            LIKE pro_solicitud.fecha_baja,
             fingre                LIKE pro_solicitud.fingre,
             fenvio                LIKE pro_solicitud.fenvio,
             fecha_registro        LIKE pro_solicitud.fecha_registro,
             fecha_proceso         LIKE pro_solicitud.fecha_proceso,
             resuelva              LIKE pro_solicitud.resuelva,
             horas_capacit         LIKE pro_solicitud.horas_capacit,
             escolar               LIKE pro_solicitud.escolar,
             sexo                  CHAR(1),                     --(v10)
             edo_naci              CHAR(2),                     --(v10) 
--             tipo_recibo           LIKE pro_solicitud.tipo_recibo,
             calle                 LIKE pro_solicitud.calle,
             numero                LIKE pro_solicitud.numero,
             dpto                  LIKE pro_solicitud.dpto,
             codpos                LIKE pro_solicitud.codpos,
             colonia               LIKE pro_solicitud.colonia,
             deleg                 LIKE pro_solicitud.deleg,
             delegdesc             CHAR(20),
             ciudad                LIKE pro_solicitud.ciudad,
             ciudaddesc            CHAR(20),
             estado                LIKE pro_solicitud.estado,
             estadodesc            CHAR(20),
             fono                  LIKE pro_solicitud.fono
           END RECORD

    DEFINE arr_rfc ARRAY[100] OF RECORD #glo #arr_rfc 
               cod_promotor     LIKE    pro_solicitud.cod_promotor
           END RECORD
     
    DEFINE HOY                   DATE,
           HOY2                  DATETIME YEAR TO SECOND,
           sn                    CHAR(1)    ,
           salida                CHAR(1)    ,
           cla_where             CHAR(300)  ,
           gerrflag              CHAR(30)   ,
           desestad              CHAR(30)   ,
           desestad1             CHAR(30)   ,
           sel_where             CHAR(1000) ,
           enter                 CHAR(1)    ,
           usuario               CHAR(8)   ,
           digito                ,
           sw_1                  ,
           sw_2                  ,
           sw_3                  ,
           pos                   SMALLINT  ,
           vmotivo_suspende      CHAR(2) ,
           vfecha_suspende       DATE    ,
           vstatus               SMALLINT,
           vcod_promotor         CHAR(10),
           vcod_promotor2        CHAR(10),
           ventro                CHAR(1) ,
           v_sexo                CHAR(1) ,
           v_edo_naci            CHAR(2) 

END GLOBALS 

MAIN
    DEFER INTERRUPT
    OPTIONS 
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("PROM003.log")

    LET HOY = TODAY
    OPEN WINDOW prom0031 AT 2,3 WITH FORM "PROM0031" ATTRIBUTE(BORDER)
    DISPLAY " PROM003      SOLICITUDES DE REGISTRO DE AGENTES ",
            "PROMOTORES                    " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                     DOMICILIO   DE   CORRESPONDEN",
            "CIA                          " AT 14,1 ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

    MENU "MENU"
         COMMAND "Agrega" "Agrega solicitud del Promotor"
                 CALL inicializa()
                 CALL agrega() #a 
         COMMAND "Consulta" "Consulta solicitud del Promotor"
                 CALL inicializa()
                 CALL consulta() #c
         COMMAND "Modifica" "Modifica datos de la solicitud del Promotor"
                 CALL inicializa()
                 CALL modifica() 
         COMMAND "Elimina" "Elimina solicitud del Promotor"
                 CALL inicializa()
                 CALL elimina()
         COMMAND "Reactivar 1"     --Reactiva Bajas 
                 "Reactiva solicitud de Promotor por Diag. "
                 CALL inicializa()
                 CALL reactiva_1() #r
         COMMAND "RevReactiv" "Reversa la Reactivacion 1"
                 CALL inicializa()
                 CALL reverso_react()
--         COMMAND "Reactivar 2"    -- Reactiva Rechazos               --(v10)
--                 "Reactiva las Bajas y Revalidaciones no exitosas   "
--                 CALL inicializa()
--                 CALL reactiva_2() #r2
         COMMAND "Reenviar-Rechazos" 
                 "Reenviar solicitudes rechazadas de Promotor"
                 CALL inicializa()
                 CALL reenviar_rechazos() #rr
         COMMAND "Confirma" "Confirmar la recepcion de registros con diferencias"
                 CALL inicializa()
                 CALL confirma_recep()
         COMMAND "Salir" "Salir mantenimiento solicitudes de Promotores"
                 EXIT MENU
    END MENU

    CLOSE WINDOW prom0031
END MAIN

FUNCTION inicializa()
#i------------------
   SELECT USER,*
   INTO   usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "pro"

    LET salida = NULL
    INITIALIZE reg.* TO NULL
END FUNCTION
 
FUNCTION agrega()     
#a---------------
   DEFINE cont_1            SMALLINT,
          ult_consecutivo   INTEGER,
          rfc_2             CHAR(10)

    DEFINE wf_error       SMALLINT,
           aa             CHAR(01),
           xxx            SMALLINT,
           x_fecha        CHAR(10),
           j_fecha        DATE,
           xx_fecha       DATE,
           z_fecha        CHAR(10),
           mm             ,
           dd             ,
           aaa            CHAR(02),
           desciuda       CHAR(18),
           dessup         CHAR(18),
           desnivel       CHAR(18),
           desdeleg       CHAR(18),
           v_1            SMALLINT,
           val_1          CHAR(80),
           sexo_cur       CHAR(01),
           dig_curp       SMALLINT,
           i, long        SMALLINT,
           caracteres     SMALLINT,
           ayo_s          SMALLINT,
           calle          CHAR(40),
           espe           CHAR(40),
           pasa           SMALLINT,
           pasa_curp      SMALLINT,
           desc_err       CHAR(60),
           ayo_x          CHAR(10),
           ayo_1          SMALLINT,
           ayo_2          SMALLINT,
           si_esta        SMALLINT,
           rfc_arma       CHAR(10),
           mcod_promo     CHAR(10),
           merror         CHAR(02),
           mdigito_prom   SMALLINT

    DEFINE vrfc      CHAR(13),
           s_rfc     CHAR(10)

    INITIALIZE reg.* TO NULL

    LET sw_3              = 0
    LET sw_2              = 0
    LET ventro            = 0

    IF int_flag THEN
        LET int_flag = FALSE
    END IF

    WHILE NOT int_flag
      DISPLAY " ALTA " AT 1,71 ATTRIBUTE(NORMAL)
      DISPLAY "ESC: Graba,  Ctrl-C: Salir  ","" AT 2,1

    LET  reg.fingre = HOY
    LET  reg.fecha_baja = NULL

    DISPLAY BY NAME reg.fingre

    INPUT BY NAME reg.codven         ,
                  reg.cod_promotor      ,
                  reg.seguro  ,  
                  reg.rfc  ,
                  reg.unico  ,
                  reg.paterno         ,
                  reg.materno         ,
                  reg.nombres           ,
                  reg.diag_proceso ,
                  reg.fnaci  ,
                  reg.fecha_baja        ,
                  reg.fingre  ,
                  reg.fenvio     ,
                  reg.fecha_registro    ,
                  reg.fecha_proceso ,
                  reg.resuelva         ,
                  reg.horas_capacit     ,
                  reg.escolar     ,
                  reg.calle  ,
                  reg.numero  ,
                  reg.dpto  ,
                  reg.codpos  ,
                  reg.colonia  ,
                  reg.deleg  ,
                  reg.ciudad     ,
                  reg.estado            ,
                  reg.fono          WITHOUT DEFAULTS  

         BEFORE FIELD codven          
            IF sw_3 = 0 THEN
                SELECT MAX(consecutivo)+1
                INTO   ult_consecutivo
                FROM   pro_consecutivo

                INSERT INTO pro_consecutivo VALUES (ult_consecutivo)

                LET reg.nro_solicitud = ult_consecutivo
                LET reg.fingre        = HOY
                LET sw_2            = 0                                            
                DISPLAY BY NAME reg.nro_solicitud,
                                reg.fingre 
                LET sw_3 = 1
            END IF

      AFTER FIELD codven   
            IF reg.codven IS NOT NULL THEN
               LET si_esta = 0

               SELECT "a.X" FROM pro_solicitud a
               WHERE a.codven = reg.codven
               IF STATUS <> NOTFOUND THEN
                  LET si_esta = 1
               END IF

               SELECT "a.X" FROM pro_mae_promotor a
               WHERE a.codven = reg.codven
               IF STATUS <> NOTFOUND THEN
                  LET si_esta = 1
               END IF

               IF si_esta = 1 THEN
                  ERROR "  EL NUMERO DE NOMINA YA SE ENCUENTRA DADO DE ALTA "
                  LET si_esta = 0
                  NEXT FIELD codven
               END IF
            END IF 

      AFTER FIELD cod_promotor   
        IF reg.cod_promotor IS NULL THEN
           LET reg.fecha_baja = "01/01/1900"
           DISPLAY BY NAME reg.fecha_baja
        END IF

        SELECT "a.OK" 
        FROM   pro_solicitud a
        WHERE  a.cod_promotor = reg.cod_promotor
        GROUP BY 1

        IF STATUS <> NOTFOUND THEN
            ERROR "  CODIGO PROMOTOR YA INGRESADO EN OTRA SOLICITUD" 
                  ATTRIBUTE(NORMAL)
            NEXT FIELD cod_promotor
        END IF

        SELECT "b.X" FROM   pro_mae_promotor b
        WHERE  b.cod_promotor = reg.cod_promotor
        AND    b.status       IN(1,4)

        IF STATUS <> NOTFOUND THEN
            ERROR "  CODIGO PROMOTOR YA EXISTE EN MAESTRO" ATTRIBUTE(NORMAL)
            NEXT FIELD cod_promotor
        END IF

        LET long = 0
        IF reg.cod_promotor IS NOT NULL THEN
           IF LENGTH(reg.cod_promotor) < 10 THEN
              ERROR "  EL CODIGO DE PROMOTOR CONTIENE MENOS DE 10 CARACTERES"
              ATTRIBUTE(NORMAL)
              NEXT FIELD cod_promotor
           ELSE

              INITIALIZE mcod_promo,merror TO NULL
              LET mdigito_prom = 0
              LET mcod_promo = reg.cod_promotor

              FOR i = 1 TO 10
                  IF mcod_promo[i] <> "0" AND
                     mcod_promo[i] <> "1" AND
                     mcod_promo[i] <> "2" AND
                     mcod_promo[i] <> "3" AND
                     mcod_promo[i] <> "4" AND
                     mcod_promo[i] <> "5" AND
                     mcod_promo[i] <> "6" AND
                     mcod_promo[i] <> "7" AND
                     mcod_promo[i] <> "8" AND
                     mcod_promo[i] <> "9" THEN
                     LET mdigito_prom = 1
                     EXIT FOR
                  END IF
              END FOR

              IF mdigito_prom = 1 THEN
                 ERROR "  EL CODIGO DE PROMOTOR NO ES VALIDO "
                 NEXT FIELD cod_promotor
              END IF

              INITIALIZE mcod_promo,merror TO NULL
              LET mdigito_prom = 0

              CALL Digito_prom(reg.cod_promotor CLIPPED) 
                   RETURNING mcod_promo, merror,mdigito_prom
              IF merror NOT MATCHES "00" THEN
                 ERROR "  DIGITO VERIFICADOR INVALIDO, EL DIGITO DEBE SER : ",
                       mdigito_prom ATTRIBUTE(NORMAL)
                 WHILE TRUE
                     PROMPT " PROMOTOR INCORRECTO DEBE DE SER :",
                            mcod_promo,
                            "... DESEA CONTINUAR  S/N ?"
                     FOR CHAR enter

                     IF enter MATCHES "[SsNn]" THEN
                        EXIT WHILE
                     END IF
                 END WHILE

                 IF enter MATCHES "[Ss]" THEN
                    NEXT FIELD seguro
                 ELSE
                     NEXT FIELD cod_promotor
                 END IF

              ELSE
                    NEXT FIELD seguro
              END IF
           END IF
        END IF

      AFTER FIELD seguro
        IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
            NEXT FIELD cod_promotor
        END IF

        LET sw_1 = 0
        IF reg.seguro IS NULL OR reg.seguro = " " THEN
            ERROR "  DEBE INGRESAR RFC CON HOMOCLAVE " ATTRIBUTE(NORMAL)
            NEXT FIELD rfc
        END IF

        LET sw_1 = 1
        IF reg.seguro IS NOT NULL AND reg.seguro[1,1] <> " " THEN
            IF LENGTH(reg.seguro) <> 11 THEN
              ERROR "  DEBE INGRESAR N.S.S. COMPLETO" ATTRIBUTE(NORMAL)
              NEXT FIELD seguro
            END IF

            CALL  digito_verif(reg.seguro[1,10],10)
                  RETURNING digito

            IF digito = 32000 THEN
              ERROR "  N.S.S. FORMADO CON DATOS INCORRECTOS" ATTRIBUTE(NORMAL)
              NEXT FIELD seguro
             END IF

          IF LENGTH(reg.seguro) = 11 AND
              digito <> reg.seguro[11] THEN
              ERROR "  DIGITO VERIFICADOR NO ES EL CORRECTO"
                     ATTRIBUTE(NORMAL)
              SLEEP 2
              WHILE TRUE
                          PROMPT " DESEA CONTINUAR   S/N  ?"
                          FOR CHAR enter

                          IF enter MATCHES "[SsNn]" THEN
                             EXIT WHILE
                          END IF
               END WHILE
             
                      IF enter MATCHES "[Ss]" THEN
                         NEXT FIELD rfc
                      ELSE
                         NEXT FIELD seguro
                      END IF


              NEXT FIELD seguro
          END IF

          IF reg.seguro[11] <> "1" AND
             reg.seguro[11] <> "2" AND
             reg.seguro[11] <> "3" AND
             reg.seguro[11] <> "4" AND
             reg.seguro[11] <> "5" AND
             reg.seguro[11] <> "6" AND
             reg.seguro[11] <> "7" AND
             reg.seguro[11] <> "8" AND
             reg.seguro[11] <> "9" AND
             reg.seguro[11] <> "0" THEN
              ERROR "N.S.S. formado con datos incorrectos"
              NEXT FIELD seguro
          END IF

        ELSE
          ERROR "  NO PUEDE SER NULO O COMENZAR CON BLANCOS "
          NEXT FIELD seguro
        END IF                         

                 SELECT "a.X" FROM pro_solicitud a
                 WHERE a.seguro = reg.seguro
                 IF SQLCA.SQLCODE = 0 THEN
                    ERROR "YA SE ENCUENTRA REGISTRADO ESTE NSS "
                    NEXT FIELD seguro
                 ELSE
                    SELECT "a.X" FROM pro_mae_promotor a
                    WHERE a.seguro = reg.seguro
                    IF SQLCA.SQLCODE = 0 THEN
                       ERROR "YA SE ENCUENTRA REGISTRADO ESTE NSS "
                       NEXT FIELD seguro
                    END IF
                 END IF 
                 NEXT FIELD rfc
  
      AFTER FIELD rfc
        IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
           NEXT FIELD seguro
        END IF

        IF reg.rfc IS NULL  OR  reg.rfc = " " THEN
                        ERROR "  RFC NO puede ser NULO" ATTRIBUTE(NORMAL)
               NEXT FIELD rfc
        END IF

        IF LENGTH(reg.rfc CLIPPED) <> 10 AND
           LENGTH(reg.rfc CLIPPED) <> 13 THEN
           ERROR "  DEBE INGRESAR R.F.C. COMPLETO" ATTRIBUTE(NORMAL)
           NEXT FIELD rfc
        END IF

        IF NOT valida_fecha_rfc(reg.rfc[5,10]) THEN
           ERROR "Formato de RFC Incorrecto ( ----AAMMDD*** )"
           NEXT FIELD rfc
        ELSE
           WHENEVER ERROR CONTINUE
             LET aaa     = reg.rfc[5,6]
             LET mm      = reg.rfc[7,8]
             LET dd      = reg.rfc[9,10]
             LET z_fecha = mm,"/",dd,"/19",aaa
             LET j_fecha = z_fecha

             IF j_fecha IS NULL THEN
                 ERROR "  FECHA INVALIDA EN RFC" ATTRIBUTE(NORMAL)
                 NEXT FIELD rfc
             END IF
           WHENEVER ERROR STOP

           IF reg.fnaci IS NULL OR reg.fnaci = " " THEN
               INITIALIZE ayo_x TO NULL
               LET ayo_s = 0
               LET ayo_1 = 0
               LET ayo_2 = 0

               LET ayo_x = HOY 
               LET ayo_1 = ayo_x[7,10] 
               LET ayo_x = reg.fnaci 
               LET ayo_2 = ayo_x[7,10] 
               LET ayo_s = ayo_1 - ayo_2

               IF ayo_s < 14 THEN
                  ERROR "  FECHA INVALIDA EN RFC TIENE MENOS DE 14 AÑOS "
                  ATTRIBUTE(NORMAL)
                  NEXT FIELD rfc
               END IF

               LET reg.fnaci = z_fecha
               DISPLAY BY NAME reg.fnaci
           END IF
        END IF

        IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
           FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
           LET v_1 = 0
           INITIALIZE val_1 TO NULL
           CALL verifica_rfc(reg.rfc[1,4])
                                RETURNING v_1,val_1 #ve--
           IF v_1 = 1 THEN
              ERROR "R.F.C. ",val_1 CLIPPED
              NEXT FIELD rfc
           END IF

           IF reg.seguro IS NOT NULL THEN
              IF reg.seguro[5,6] <> reg.rfc[5,6] THEN
                 ERROR "El AÑO DEL NSS Y EL RFC SON DIFERENTES "
                 ATTRIBUTE(NORMAL)
                 SLEEP 3
                 ERROR ""
              END IF 
           END IF
           NEXT FIELD unico
        END IF

        IF sw_1 = 0 THEN
           IF LENGTH(reg.rfc) < 13 THEN
              ERROR " "
              ERROR "  DEBIO HABER INGRESADO EL RFC CON HOMOCLAVE" 
                    ATTRIBUTE(NORMAL)
           END IF
        END IF

        IF reg.rfc IS NOT NULL OR reg.rfc[1,2] <> "  " THEN
           LET v_1 = 0
           INITIALIZE val_1 TO NULL
           CALL verifica_rfc(reg.rfc[1,4])
                                RETURNING v_1,val_1 #ve--
           IF v_1 = 1 THEN
              ERROR "R.F.C. ",val_1 CLIPPED
              NEXT FIELD rfc
           END IF

           IF reg.seguro IS NOT NULL THEN
              IF reg.seguro[5,6] <> reg.rfc[5,6] THEN
                 ERROR "El AÑO DEL NSS Y EL RFC SON DIFERENTES "
                 ATTRIBUTE(NORMAL)
                 SLEEP 3
                 ERROR ""
              END IF 
           END IF
           NEXT FIELD unico

           IF NOT valida_fecha_rfc(reg.rfc[5,10]) THEN
              ERROR "Formato de RFC Incorrecto ( ----AAMMDD*** )"
              ATTRIBUTE(NORMAL)
              NEXT FIELD rfc
           ELSE
              WHENEVER ERROR CONTINUE
                LET aaa     = reg.rfc[5,6]
                LET mm      = reg.rfc[7,8]
                LET dd      = reg.rfc[9,10]
                LET z_fecha = mm,"/",dd,"/19",aaa
                LET j_fecha = z_fecha

                IF j_fecha IS NULL THEN
                    ERROR "  FECHA INVALIDA EN RFC" ATTRIBUTE(NORMAL)
                    NEXT FIELD rfc
                END IF
              WHENEVER ERROR STOP

              IF reg.fnaci IS NULL OR reg.fnaci = " " THEN
                 INITIALIZE ayo_x TO NULL
                 LET ayo_s = 0
                 LET ayo_1 = 0
                 LET ayo_2 = 0

                 LET ayo_x = HOY
                 LET ayo_1 = ayo_x[7,10]
                 LET ayo_x = reg.fnaci
                 LET ayo_2 = ayo_x[7,10]
                 LET ayo_s = ayo_1 - ayo_2

                 IF ayo_s < 14 THEN
                    ERROR "  FECHA INVALIDA EN RFC TIENE MENOS DE 14 AÑOS "
                    ATTRIBUTE(NORMAL)
                    NEXT FIELD rfc
                 END IF

                 LET reg.fnaci = z_fecha
                 DISPLAY BY NAME reg.fnaci
              END IF
           END IF

        END IF

        IF  reg.rfc IS NOT NULL THEN
            LET vrfc = reg.rfc[1,10]
            CALL rfc_promotor(vrfc)
        END IF

      AFTER FIELD unico   
        IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
           IF reg.seguro IS NOT NULL AND reg.unico IS NOT NULL THEN
              IF reg.seguro[5,6] <> reg.rfc[5,6] AND 
                 reg.rfc[5,6] <> reg.unico[5,6]  THEN
                 ERROR "El AÑO DEL NSS, RFC Y CURP SON DIFERENTES "
                 ATTRIBUTE(NORMAL)
                 SLEEP 3
                 ERROR ""
              END IF 
           END IF

           IF reg.seguro IS NULL THEN
              IF reg.rfc[5,6] <> reg.unico[5,6] THEN
                 ERROR "El AÑO DEL RFC Y CURP SON DIFERENTES "
                 ATTRIBUTE(NORMAL)
                 SLEEP 3
                 ERROR ""
              END IF
           END IF
           NEXT FIELD rfc
        END IF

        IF LENGTH(reg.unico) < 18 AND
           LENGTH(reg.unico) > 0  THEN
            ERROR "Debe ingresar CURP completa"
            ATTRIBUTE(NORMAL)
            NEXT FIELD unico
        ELSE
            IF reg.unico[1] <> " " OR
               reg.unico IS NOT NULL THEN
                IF reg.unico[11] = "H" THEN
                    LET sexo_cur = "1"
                ELSE
                    LET sexo_cur = "2"
                END IF

                CALL valida_est_curp(reg.unico)
                RETURNING pasa_curp, desc_err
                IF pasa_curp = 1 THEN
                   ERROR "", desc_err
                   LET pasa_curp = 0
                   NEXT FIELD unico
                END IF

                CALL var_dig_curp(reg.unico) RETURNING pasa, dig_curp
                IF pasa = 0 THEN
                  ERROR "Digito Verificador Invalido curp, el digito es : ",
                  dig_curp
                  LET pasa = 0
                  NEXT FIELD unico
                END IF
            ELSE
                LET sexo_cur = " "
            END IF
        END IF

        IF reg.unico[1] = " " OR reg.unico IS NULL THEN
            ERROR "Debe ingresar CURP correcta"
            NEXT FIELD unico
        END IF

        IF reg.seguro IS NOT NULL AND reg.unico IS NOT NULL THEN
           IF reg.seguro[5,6] <> reg.rfc[5,6] AND
              reg.rfc[5,6] <> reg.unico       THEN
              ERROR "El AÑO DEL NSS, RFC Y CURP SON DIFERENTES "
              ATTRIBUTE(NORMAL)
              SLEEP 3
              ERROR ""
           END IF
        END IF

        IF reg.seguro IS NULL THEN
           IF reg.rfc[5,6] <> reg.unico[5,6] THEN
              ERROR "El AÑO DEL RFC Y CURP SON DIFERENTES "
              ATTRIBUTE(NORMAL)
              SLEEP 3
             ERROR ""
           END IF

           IF reg.rfc[1,4] <> reg.unico[1,4] THEN
              ERROR "  LOS DATOS DEL RFC Y CURP SON DIFERENTES "
              ATTRIBUTE(NORMAL)
              SLEEP 3
              ERROR ""
           END IF
        END IF

--(V10) inicia
        CALL curp(reg.unico) RETURNING reg.sexo,
                                       reg.edo_naci

        DISPLAY BY NAME   reg.sexo
        DISPLAY BY NAME   reg.edo_naci

--(V10) finaliza 

        NEXT FIELD paterno

      AFTER FIELD paterno   
        IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
           IF reg.seguro IS NOT NULL AND reg.unico IS NOT NULL THEN
              IF reg.seguro[5,6] <> reg.rfc[5,6] AND
                 reg.rfc[5,6] <> reg.unico       THEN
                 ERROR "El AÑO DEL NSS, RFC Y CURP SON DIFERENTES "
                 ATTRIBUTE(NORMAL)
                 SLEEP 3
                 ERROR ""
              END IF

              IF reg.rfc[1,4] <> reg.unico[1,4] THEN
                 ERROR "  LOS DATOS DEL RFC Y CURP SON DIFERENTES "
                 ATTRIBUTE(NORMAL)
                 SLEEP 3
                 ERROR ""
              END IF
           END IF

           IF reg.seguro IS NULL THEN
              IF reg.rfc[5,6] <> reg.unico[5,6] THEN
                 ERROR "El AÑO DEL RFC Y CURP SON DIFERENTES "
                 ATTRIBUTE(NORMAL)
                 SLEEP 3
                 ERROR ""
              END IF
           END IF

           NEXT FIELD unico
        END IF

        IF reg.paterno  IS NULL OR reg.paterno[1] = " " THEN
           ERROR "  EL APELLIDO PATERNO DEBE SER INGRESADO"
                 ATTRIBUTE(NORMAL)
           NEXT FIELD paterno  
        END IF

      AFTER FIELD materno   
        IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
           NEXT FIELD paterno
        END IF

        IF reg.materno[1] = " "  OR reg.materno[1,2] = " ." OR
           reg.materno[1] = "."  OR reg.materno[1,2] = ".." OR 
           reg.materno[1,2] = "X " OR reg.materno[1,2] = " X" OR 
           reg.materno[1,2] = ".X"  THEN
           ERROR "  EL APELLIDO MATERNO NO PUEDE SER UN PUNTO o X "
                 ATTRIBUTE(NORMAL) 
           NEXT FIELD materno  
        END IF

      AFTER FIELD nombres      
        IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
           NEXT FIELD materno
        END IF

      IF reg.nombres  IS NULL OR reg.nombres[1] = " " THEN
         ERROR "  EL NOMBRE ES REQUERIDO" ATTRIBUTE(NORMAL)
         NEXT FIELD nombres  
      END IF

      INITIALIZE rfc_arma TO NULL

      CALL arma_clave_rfc(reg.paterno,
                          reg.materno,
                          reg.nombres,
                          reg.fnaci) RETURNING rfc_arma #rac

      IF reg.rfc <> rfc_arma THEN
         ERROR "  NO COINCIDE EL NOMBRE CON EL RFC " ATTRIBUTE(NORMAL)
         SLEEP 2
         ERROR ""
      END IF

    AFTER FIELD fnaci
      IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
         NEXT FIELD nombres
      END IF

      IF reg.fnaci  IS NULL THEN
         ERROR "  LA FECHA DE NACIMIENTOE ES REQUERIDA" ATTRIBUTE(NORMAL)
         NEXT FIELD fnaci    
      END IF

      IF reg.fnaci IS NOT NULL THEN
         INITIALIZE ayo_x TO NULL
         LET ayo_s = 0
         LET ayo_1 = 0
         LET ayo_2 = 0

         LET ayo_x = HOY
         LET ayo_1 = ayo_x[7,10]
         LET ayo_x = reg.fnaci
         LET ayo_2 = ayo_x[7,10]
         LET ayo_s = ayo_1 - ayo_2

         IF ayo_s < 14 THEN
            ERROR "  EL PROMOTOR TIENE MENOS DE 14 AÑOS "
            NEXT FIELD fnaci
         END IF
      END IF

---valida rfc
      CALL valida_rfc(reg.paterno,
                      reg.materno,
                      reg.nombres,
                      reg.fnaci)
           RETURNING  s_rfc

      LET rfc_2 = reg.rfc[1,10]

      IF s_rfc <> rfc_2 THEN
         WHILE TRUE 
             PROMPT " RFC INCORRECTO DEBE DE SER :",
                    s_rfc,
                    "... DESEA CONTINUAR  S/N ?"
             FOR CHAR enter

             IF enter MATCHES "[SsNn]" THEN
                EXIT WHILE
             END IF
         END WHILE
   
         IF enter MATCHES "[Ss]" THEN
            INITIALIZE ayo_x TO NULL
            LET ayo_s = 0
            LET ayo_1 = 0
            LET ayo_2 = 0

            LET ayo_x = HOY
            LET ayo_1 = ayo_x[7,10]
            LET ayo_x = reg.fnaci
            LET ayo_2 = ayo_x[7,10]
            LET ayo_s = ayo_1 - ayo_2

            IF ayo_s < 14 THEN
               ERROR "  EL PROMOTOR TIENE MENOS DE 14 AÑOS "
               NEXT FIELD fnaci
            END IF
            NEXT FIELD fecha_baja
         ELSE
            NEXT FIELD rfc
         END IF

      END IF
--- fin valida rfc 

    AFTER FIELD fecha_baja
      IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
         NEXT FIELD nombres
      END IF

      IF reg.codven IS NOT NULL AND reg.fecha_baja IS NULL THEN
         ERROR "  DEBE INGRESAR LA FECHA DE BAJA DE ULTIMA AFORE " 
               ATTRIBUTE(NORMAL)
         NEXT FIELD fecha_baja
      END IF

      IF reg.codven IS NULL THEN
         LET reg.fecha_baja = "01/01/1900"
         DISPLAY reg.fecha_baja TO fecha_baja
         NEXT FIELD fingre
      END IF

      IF reg.fecha_baja > HOY THEN
         ERROR "  FECHA NO PUEDE SER SUPERIOR A LA ACTUAL " 
         ATTRIBUTE(NORMAL)
         NEXT FIELD fecha_baja
      END IF

    AFTER FIELD resuelva        
      IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
         NEXT FIELD fecha_proceso
      END IF

      IF reg.resuelva  IS NULL  OR  reg.resuelva = " " THEN
         ERROR "  CAMPO NO PUEDE SER NULO " ATTRIBUTE(NORMAL)
         NEXT FIELD resuelva    
      END IF

      -- En el mpt con fecha 29072009 se especifica que la calificacion 
      -- debera ser menor o igual a 100 

      IF reg.resuelva < 000 OR reg.resuelva > 100 THEN              --(v10)
         ERROR "  ERROR...CALIFICACION DEBE SER MENOR O IGUAL A 100" --(v10)
         NEXT FIELD resuelva
      END IF                                                             

    AFTER FIELD horas_capacit
      IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
         NEXT FIELD resuelva
      END IF

      IF reg.horas_capacit < 0 THEN
         ERROR "  NUMERO DE HORAS DE CAPACITACION NO PUEDE SER",
         "  MENOR A 0 "
         ATTRIBUTE(NORMAL)
         NEXT FIELD horas_capacit
      END IF

    AFTER FIELD escolar
                IF reg.escolar IS NULL THEN
                    CALL grado_escolar()
                    RETURNING reg.escolar

                    IF reg.escolar IS NULL THEN
                        DISPLAY reg.escolar TO escolar
                        NEXT FIELD escolar
                    ELSE
                        DISPLAY reg.escolar TO escolar
                        NEXT FIELD calle       
                    END IF
                ELSE
                    IF reg.escolar MATCHES "[ABCDEFGHIJKL]" THEN
                       NEXT FIELD calle       
                    ELSE
                        ERROR "NO EXISTE EL GRADO DE ESCOLARIDAD"
                        NEXT FIELD escolar
                    END IF
                END IF

    AFTER FIELD calle    
      IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
         NEXT FIELD horas_capacit
      END IF

      IF reg.calle IS NOT NULL THEN
         LET caracteres = 0

         IF reg.calle[1] = "[" OR reg.calle[1] = '"'  OR
            reg.calle[1] = "]" OR reg.calle[1] = "#"  OR
            reg.calle[1] = "$" OR reg.calle[1] = "%"  OR
            reg.calle[1] = "&" OR reg.calle[1] = "="  OR
            reg.calle[1] = "/" OR reg.calle[1] = "?"  OR
            reg.calle[1] = "-" OR reg.calle[1] = "'"  OR
            reg.calle[1] = "(" OR reg.calle[1] = ")"  OR
            reg.calle[1] = "^" OR reg.calle[1] = "!"  OR
            reg.calle[1] = "~" OR reg.calle[1] = "_"  OR
            reg.calle[1] = ":" OR reg.calle[1] = "."  OR
            reg.calle[1] = "," OR reg.calle[1] = ";"  OR
            reg.calle[1] = "<" OR reg.calle[1] = ">"  OR
            reg.calle[1] = "@" OR reg.calle[1] = "|"  OR
            reg.calle[1] = "{" OR reg.calle[1] = "}"  OR
            reg.calle[1] = "+" OR reg.calle[1] = "*"  OR
            reg.calle[1] = "`" OR reg.calle[1] = "¿"  OR
            reg.calle[1] = "¡" OR reg.calle[1] = "Ä"  OR
            reg.calle[1] = "É" OR reg.calle[1] = "Í"  OR
            reg.calle[1] = "Ó" OR reg.calle[1] = "Ú"  OR
            reg.calle[1] = "¨" OR reg.calle[1] = "Ä"  OR
            reg.calle[1] = "Ë" OR reg.calle[1] = "Ï"  OR
            reg.calle[1] = "Ö" OR reg.calle[1] = "Ö"  OR
            reg.calle[1] = "Ü" OR reg.calle[1] = "´"  OR
            reg.calle[1] = "Á" THEN
      
            ERROR "  EL DATO NO PUEDE COMENZAR CON PUNTO O CARACTER ESPECIAL"
            ATTRIBUTE(NORMAL)
            SLEEP 2
            LET caracteres = 0
            NEXT FIELD calle
         END IF

         LET calle = reg.calle CLIPPED
         LET long = LENGTH(calle CLIPPED)

         IF caracteres = 0 THEN
            FOR i = 1 TO long
               IF calle[i,i] = "[" OR calle[i,i] = '"'  OR
                  calle[i,i] = "]" OR calle[i,i] = "#"  OR
                  calle[i,i] = "$" OR calle[i,i] = "%"  OR
                  calle[i,i] = "&" OR calle[i,i] = "="  OR
                  calle[i,i] = "/" OR calle[i,i] = "?"  OR
                  calle[i,i] = "-" OR calle[i,i] = "'"  OR
                  calle[i,i] = "(" OR calle[i,i] = ")"  OR
                  calle[i,i] = "^" OR calle[i,i] = "!"  OR
                  calle[i,i] = "~" OR calle[i,i] = "_"  OR
                  calle[i,i] = ":" OR calle[i,i] = "."  OR
                  calle[i,i] = "," OR calle[i,i] = ";"  OR
                  calle[i,i] = "<" OR calle[i,i] = ">"  OR
                  calle[i,i] = "@" OR calle[i,i] = "|"  OR
                  calle[i,i] = "{" OR calle[i,i] = "}"  OR
                  calle[i,i] = "+" OR calle[i,i] = "*"  OR
                  calle[i,i] = "`" OR calle[i,i] = "¿"  OR
                  calle[i,i] = "¡" OR calle[i,i] = "Ä"  OR
                  calle[i,i] = "É" OR calle[i,i] = "Í"  OR
                  calle[i,i] = "Ó" OR calle[i,i] = "Ú"  OR
                  calle[i,i] = "¨" OR calle[i,i] = "Ä"  OR
                  calle[i,i] = "Ë" OR calle[i,i] = "Ï"  OR
                  calle[i,i] = "Ö" OR calle[i,i] = "Ö"  OR
                  calle[i,i] = "Ü" OR calle[i,i] = "´"  OR
                  calle[i,i] = "Á" THEN
         
                  LET espe[i,i] = calle[i,i]
                  LET caracteres = caracteres + 1
               END IF
            END FOR

            IF caracteres > 2 THEN
               ERROR "  EL DATO NO PUEDE TENER VARIOS CARACTER ESPECIAL"
               ATTRIBUTE(NORMAL)
               SLEEP 2
               LET caracteres = 0
               NEXT FIELD calle
            END IF
         END IF

         IF caracteres = 0 THEN
            FOR i = 1 TO long
               IF calle[i,i] = "." THEN
                  IF calle[i,i+1] = ".." OR
                     calle[i,i+1] = "./" OR
                     calle[i,i+1] = ".$" OR
                     calle[i,i+1] = ".{" OR
                     calle[i,i+1] = ".[" OR
                     calle[i,i+1] = ".}" OR
                     calle[i,i+1] = ".%" OR
                     calle[i,i+1] = ".?" OR
                     calle[i,i+1] = ".!" OR
                     calle[i,i+1] = '."' OR
                     calle[i,i+1] = ".&" OR
                     calle[i,i+1] = ".(" OR
                     calle[i,i+1] = ".)" OR
                     calle[i,i+1] = ".=" OR
                     calle[i,i+1] = ".|" OR
                     calle[i,i+1] = ".@" OR
                     calle[i,i+1] = ".<" OR
                     calle[i,i+1] = ".>" OR
                     calle[i,i+1] = ".;" OR
                     calle[i,i+1] = ".:" OR
                     calle[i,i+1] = ".," OR
                     calle[i,i+1] = ".+" OR
                     calle[i,i+1] = ".-" THEN
                     LET caracteres = 999
                     EXIT FOR
                  END IF
               END IF

               IF caracteres = 0 THEN
                  IF calle[i,i+1] = "##" OR
                     calle[i,i+1] = "%%" OR
                     calle[i,i+1] = "$$" OR
                     calle[i,i+1] = "//" OR
                     calle[i,i+1] = "((" OR
                     calle[i,i+1] = "()" OR
                     calle[i,i+1] = "))" OR
                     calle[i,i+1] = '!"' OR
                     calle[i,i+1] = "|!" OR
                     calle[i,i+1] = "==" OR
                     calle[i,i+1] = "==" OR
                     calle[i,i+1] = "==" THEN
                     LET caracteres = 999
                     EXIT FOR
                  END IF
               END IF
            END FOR
         END IF
      END IF

      IF caracteres > 2 AND caracteres < 40 OR caracteres = 999 THEN
         ERROR "  LA CALLE CONTIENE VARIOS CARACTERES NO VALIDOS "
         ATTRIBUTE(NORMAL)
         SLEEP 2
         LET caracteres = 0
         NEXT FIELD calle
      END IF

      IF reg.calle IS NULL OR reg.calle[1] = " " THEN
         ERROR "  EL DATO NO DEBE SER NULO O COMENZAR CON BLANCO, REINGRESE ..."
         ATTRIBUTE(NORMAL)
         NEXT FIELD calle
      END IF

    AFTER FIELD numero    
      IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
         NEXT FIELD calle
      END IF

      IF reg.numero    IS NULL  OR reg.numero[1] = " " THEN
         ERROR "  DATO NO PUEDE SER NULO O COMENZAR CON BLANCO REINGRESE ..."
         ATTRIBUTE(NORMAL)  
         NEXT FIELD numero
      END IF

    AFTER FIELD dpto    
      IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
         NEXT FIELD numero
      END IF

      IF reg.dpto IS NOT NULL  AND reg.dpto[1] = " " THEN
         ERROR "  DATO NO PUEDE SER NULO O COMENZAR CON BLANCO REINGRESE ..."
         ATTRIBUTE(NORMAL)  
         NEXT FIELD numero
      END IF

    AFTER FIELD codpos     
      IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
         NEXT FIELD numero
      END IF

      IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
         NEXT FIELD fono
      END IF

      IF reg.codpos IS NULL THEN
         CALL despliega_codigo_postal() #dcp
              RETURNING reg.codpos  ,
                        reg.colonia ,
                        reg.deleg   ,
                        desdeleg    ,
                        reg.ciudad  ,
                        desciuda    ,
                        reg.estado  ,
                        desestad

         IF reg.colonia IS NULL THEN
            ERROR "  ESTE CODIGO POSTAL NO EXISTE EN EL CATALOGO" 
            ATTRIBUTE(NORMAL)
            NEXT FIELD codpos
         END IF
      ELSE
                SELECT "X" 
                FROM   tab_codpos
                WHERE  cpos_cod = reg.codpos

                IF STATUS = 100 THEN
                    ERROR "  COD. POST. NO EXISTE EN CATALOGO, PON",
                          "  VALOR NULO P/DESPLEGAR PANTALLA DE CODIGOS"
                          ATTRIBUTE(NORMAL)
                    NEXT FIELD codpos
                END IF

                CALL Despliega_colonias(reg.codpos)
                  RETURNING reg.colonia ,
                              reg.deleg   ,
                              desdeleg    ,
                              reg.ciudad  ,
                              desciuda    ,
                              reg.estado  ,
                              desestad
      END IF

      DISPLAY BY NAME reg.colonia,reg.deleg,reg.ciudad,reg.estado

      DISPLAY desdeleg, desciuda, desestad TO 
              delegdesc,ciudaddesc,estadodesc

    AFTER FIELD fono
      IF reg.fono IS NOT NULL THEN
         IF reg.fono[1,3] = "000"  OR reg.fono[1] = " " THEN
            ERROR "   DATO INVALIDO PARA EL TELEFONO "
                  ATTRIBUTE(NORMAL)
            NEXT FIELD fono
         END IF
      END IF

    ON KEY( ESC )
      IF reg.cod_promotor IS NULL THEN
         LET reg.fecha_baja = "01/01/1900"
         DISPLAY BY NAME reg.fecha_baja
      END IF

      LET sw_1 = 1
      IF reg.seguro IS NOT NULL AND
         reg.seguro <> " " THEN

         IF LENGTH(reg.seguro) <> 11 THEN
             ERROR "  Debe ingresar N.S.S. COMPLETO" ATTRIBUTE(NORMAL)
             NEXT FIELD seguro
         END IF

         CALL  digito_verif(reg.seguro[1,10],10)
               RETURNING digito

         IF digito = 32000 THEN
            ERROR "  N.S.S. solo contiene digitos" ATTRIBUTE(NORMAL)
            NEXT FIELD seguro
         END IF

         IF LENGTH(reg.seguro) = 11 AND
            digito <> reg.seguro[11] THEN
             ERROR "  Digito Verificador Invalido" ATTRIBUTE(NORMAL)
             NEXT FIELD seguro
         END IF
      END IF                         
  
      IF reg.rfc IS NULL  OR  reg.rfc = " " THEN
         ERROR "  RFC NO puede ser NULO" ATTRIBUTE(NORMAL)
         NEXT FIELD rfc
      END IF 

      IF reg.rfc IS NOT NULL AND reg.rfc <> " " THEN
          IF LENGTH(reg.rfc) < 10 THEN
             ERROR "  Debe ingresar R.F.C. completo" ATTRIBUTE(NORMAL)
             NEXT FIELD rfc
          END IF

          IF sw_1 = 0 THEN
              IF LENGTH(reg.rfc) < 13 THEN
                 ERROR " "
                 ERROR "  DEBIO haber INGRESADO el RFC con HOMOCLAVE" 
                 ATTRIBUTE(NORMAL)
               END IF
          END IF

          IF NOT valida_fecha_rfc(reg.rfc[5,10]) THEN
             ERROR "  El formato del RFC en la parte numerica ",
                   "  esta incorrecta" ATTRIBUTE(NORMAL)
             NEXT FIELD rfc
          END IF

          LET aaa     = reg.rfc[5,6]
          LET mm      = reg.rfc[7,8]
          LET dd      = reg.rfc[9,10]
          LET z_fecha = mm,"/",dd,"/19",aaa
          LET j_fecha = z_fecha

          IF j_fecha IS NULL THEN
              ERROR "  FECHA INVALIDA DE RFC" ATTRIBUTE(NORMAL)
              NEXT FIELD rfc
          END IF

          IF reg.fnaci IS NULL OR reg.fnaci = " " THEN
              LET reg.fnaci = z_fecha
              DISPLAY BY NAME reg.fnaci
          END IF
      END IF

      IF reg.unico IS NOT NULL AND reg.unico <> " " THEN
          IF LENGTH(reg.unico) < 18 THEN
             ERROR "  Debe ingresar CURP completo" ATTRIBUTE(NORMAL)
             NEXT FIELD unico
          END IF
      END IF

      IF reg.paterno  IS NULL THEN
         ERROR "  El apellido paterno debe ser ingresado" 
         ATTRIBUTE(NORMAL) 
         NEXT FIELD paterno  
      END IF

--(v10)      IF reg.materno  IS NULL THEN
--(v10)         ERROR "  El apellido materno debe ser ingresado"
--(v10)                              ATTRIBUTE(NORMAL) 
--(v10)         NEXT FIELD materno  
--(v10)      END IF

      IF reg.nombres  IS NULL THEN
         ERROR "  El nombre es requerido" ATTRIBUTE(NORMAL)
         NEXT FIELD nombres  
      END IF

      IF reg.fnaci IS NOT NULL THEN
         LET xx_fecha = reg.fnaci
         IF xx_fecha IS NULL THEN
             ERROR "  Valor invalido en la ",
                   "  fecha de nacimiento"  ATTRIBUTE(NORMAL)
             NEXT FIELD fnaci
         END IF
      END IF

      IF reg.fecha_baja > HOY THEN
         ERROR "  FECHA NO PUEDE SER SUPERIOR A LA ACTUAL "
         ATTRIBUTE(NORMAL)
         NEXT FIELD fecha_baja
      END IF

      IF reg.codven IS NULL THEN
         LET reg.fecha_baja = "01/01/1900"
         DISPLAY reg.fecha_baja TO fecha_baja
      END IF

      IF reg.fecha_baja IS NULL AND reg.codven IS NOT NULL THEN
         ERROR "  DEBE INGRESAR LA FECHA DE BAJA DE ULTIMA AFORE " 
         ATTRIBUTE(NORMAL)
         NEXT FIELD fecha_baja
      END IF

      IF reg.resuelva  IS NULL  OR  reg.resuelva = " " THEN
         ERROR "  Campo no puede ser nulo " ATTRIBUTE(NORMAL)
         NEXT FIELD resuelva    
      END IF

      IF reg.horas_capacit < 0 THEN
         ERROR "  NUMERO DE HORAS DE CAPACITACION NO PUEDE SER",
         "  MENOR A 0 " ATTRIBUTE(NORMAL)
         NEXT FIELD horas_capacit
      END IF

      IF reg.escolar IS NULL THEN
          CALL grado_escolar()
          RETURNING reg.escolar

          IF reg.escolar IS NULL THEN
              DISPLAY reg.escolar TO escolar
              NEXT FIELD escolar
          ELSE
              DISPLAY reg.escolar TO escolar
          END IF
      ELSE
          IF reg.escolar NOT MATCHES "[ABCDEFGHIJKL]" THEN
              ERROR "NO EXISTE EL GRADO DE ESCOLARIDAD"
              NEXT FIELD escolar
          END IF
      END IF

      IF reg.calle IS NULL THEN
         ERROR "  Dato no puede ser nulo reingrese ..."
         ATTRIBUTE(NORMAL) 
         NEXT FIELD calle   
      END IF

      IF reg.numero    IS NULL THEN
         ERROR "  Dato no puede ser nulo reingrese ..."  
         ATTRIBUTE(NORMAL) 
         NEXT FIELD numero
      END IF

      DISPLAY BY NAME reg.colonia,reg.deleg,reg.ciudad,reg.estado

      IF reg.rfc IS NULL  OR  reg.rfc = " " THEN
         ERROR "  RFC NO puede ser NULO" ATTRIBUTE(NORMAL)
         NEXT FIELD rfc
      ELSE
             LET vrfc = reg.rfc[1,10]
             CALL rfc_promotor(vrfc)
      END IF

      IF ventro = 1 THEN 
         WHILE TRUE
             PROMPT "DESEA CONFIRMAR LA  CAPTURA AUN CON EL  ",
                    "RFC YA EXISTENTE S/N?"
             FOR CHAR enter

             IF enter MATCHES "[SsNn]" THEN   #1
                IF enter MATCHES "[Ss]" THEN   #2

                     INSERT INTO pro_consecutivo 
                            VALUES (reg.nro_solicitud)

                -- Valida si el apellido materno es igual a nulo le agregara
                -- de forma automatica N/A. --(v10)

                IF reg.materno      = " "    OR
                   reg.materno      IS NULL  OR
                   reg.materno      = ""     THEN

                   LET reg.materno  = "N/A"

                END IF

                -- fin (v10)

                     INSERT INTO pro_solicitud 
                            VALUES (0                     ,
                                    reg.codven            ,
                                    reg.seguro            ,
                                    0                     ,#nip
                                    0                     ,#agenc_cod
                                    reg.unico             ,
                                    reg.rfc               ,
                                    reg.paterno           ,
                                    reg.materno           ,
                                    reg.nombres           ,
                                    reg.fecha_baja        ,
                                    reg.fingre            ,
                                    reg.fenvio            ,
                                    reg.calle             ,
                                    reg.numero            ,
                                    reg.dpto              ,
                                    reg.colonia           ,
                                    reg.deleg             ,
                                    reg.ciudad            ,
                                    reg.estado            ,
                                    reg.codpos            ,
                                    reg.fono              ,
                                    0                     ,#sup
                                    0                     ,#nivel
                                    reg.resuelva          ,
                                    reg.horas_capacit     ,
                                    reg.fnaci             ,
                                    reg.diag_proceso      ,
                                    reg.fecha_registro    ,
                                    0                     ,#status
                                    reg.nro_solicitud     ,
                                    0                     ,#status_interno
                                    reg.fecha_proceso     ,
                                    0                     ,#num_lote
                                    reg.cod_promotor      ,
                                    0                     ,#tipo_recibo
                                    reg.escolar            #grado escolar
                                )
                       
                     LET gerrflag = FALSE
         
                     IF STATUS < 0 THEN
                        LET wf_error = STATUS
                        LET gerrflag = TRUE
                     END IF
 
                     IF NOT gerrflag THEN  #3
                        ERROR   "  REGISTRO AGREGADO" ATTRIBUTE(NORMAL) SLEEP 1
                        ERROR   ""
                         INITIALIZE reg.* TO NULL
                         CLEAR FORM
                         EXIT WHILE
                     ELSE
                         ERROR "  Error=",wf_error,", fallo la insercion ..."
                         ATTRIBUTE(NORMAL)
                         EXIT WHILE
                     END IF  #3

                ELSE  #2
                    DELETE FROM pro_consecutivo
                    WHERE  consecutivo = ult_consecutivo           

                    EXIT WHILE
                END IF #2
             END IF   #1
         END WHILE
         CALL inicializa()
         CLEAR FORM
         LET int_flag = TRUE
         EXIT INPUT

      ELSE 

         -- Valida si el apellido materno es igual a nulo le agregara
         -- de forma automatica N/A. --(v10)

         IF reg.materno      = " "    OR
            reg.materno      IS NULL  OR
            reg.materno      = ""     THEN

            LET reg.materno  = "N/A"

         END IF

         -- fin (v10)

            INSERT INTO pro_solicitud 
            VALUES (0                     ,
                    reg.codven            ,
                    reg.seguro            ,
                    0                     ,#nip
                    0                     ,#agenc_cod
                    reg.unico             ,
                    reg.rfc               ,
                    reg.paterno           ,
                    reg.materno           ,
                    reg.nombres           ,
                    reg.fecha_baja        ,
                    reg.fingre            ,
                    reg.fenvio            ,
                    reg.calle             ,
                    reg.numero            ,
                    reg.dpto              ,
                    reg.colonia           ,
                    reg.deleg             ,
                    reg.ciudad            ,
                    reg.estado            ,
                    reg.codpos            ,
                    reg.fono              ,
                    0                     ,#sup
                    0                     ,#nivel
                    reg.resuelva          ,
                    reg.horas_capacit     ,
                    reg.fnaci             ,
                    reg.diag_proceso      ,
                    reg.fecha_registro    ,
                    0                     ,#status
                    reg.nro_solicitud     ,
                    0                     ,#status_interno
                    reg.fecha_proceso     ,
                    0                     ,#num_lote
                    reg.cod_promotor      , 
                    0                     ,#tipo_recibo
                    reg.escolar            #grado escolar
                   )

            LET gerrflag = FALSE
         
            IF STATUS < 0 THEN
                LET wf_error = STATUS
                LET gerrflag = TRUE
            END IF
 
            IF NOT gerrflag THEN  #3
                ERROR   "  REGISTRO AGREGADO" ATTRIBUTE(NORMAL) SLEEP 1
                ERROR   ""
                INITIALIZE reg.* TO NULL
                CLEAR FORM
            ELSE
                 ERROR "  Error=",wf_error,", fallo la insercion ..."
                 ATTRIBUTE(NORMAL)
            END IF  #3

            CALL Inicializa()
            LET sw_3= 0
            NEXT FIELD codven

      END IF

     ON KEY(INTERRUPT,CONTROL-C)
         DELETE
         FROM   pro_consecutivo
         WHERE  consecutivo = ult_consecutivo

         CALL inicializa()
         CLEAR FORM
         LET int_flag = TRUE
         EXIT INPUT

    END INPUT
END WHILE

END FUNCTION 

FUNCTION curp(x_curp)                     --(v10)
#cr----------------
    DEFINE 
           x_curp                CHAR(18) 


        LET v_sexo     = " "
        LET v_edo_naci = "  "

        -- Obtiene la clave de sexo
        LET v_sexo     = x_curp[11,11]
        CASE v_sexo 
             WHEN "M"
                LET v_sexo = 2 
             WHEN "H"
                LET v_sexo = 1 
        END CASE

        -- Obtiene la clave de la entidad de nacimiento
        LET v_edo_naci = x_curp[12,13]
        
        SELECT cve_ent
        INTO   v_edo_naci   
        FROM   pro_ent_naci   
        WHERE  cod_ent = v_edo_naci 
           
--(v10) finaliza        
   RETURN v_sexo,
          v_edo_naci  

END FUNCTION 
FUNCTION consulta()
#c-----------------
    DEFINE #loc #smallint
        cont_reg              SMALLINT

    LET pos = 2
    IF (pos-1) >= 1 THEN
        CALL SET_COUNT(pos-1)

        LET HOY      = TODAY

        OPEN WINDOW pantalla1 AT 2,3 WITH FORM "PROM0032" ATTRIBUTE(BORDER)
        DISPLAY " PROM003      SOLICITUDES DE REGISTRO DE AGENTES ",
                "PROMOTORES                    " AT 2,1 ATTRIBUTE(REVERSE)
        DISPLAY "                     DOMICILIO   DE   CORRESPON",
                "DENCIA                          " AT 14,1 ATTRIBUTE(REVERSE) 
        DISPLAY HOY USING "DD-MM-YYYY" AT 2,66 ATTRIBUTE(REVERSE)

        LET int_flag = FALSE

        DISPLAY " CONSULTA " AT 1,67 ATTRIBUTE(NORMAL)
        DISPLAY "ESC: Consultar,  Ctrl-C: Salir  " AT 1,2

        CONSTRUCT cla_where ON   nro_solicitud,
                                 codven,
                                 cod_promotor,
                                 seguro,
                                 rfc,
                                 paterno, 
                                 materno,
                                 nombres
                            FROM nro_solicitud,
                                 codven,
                                 cod_promotor,
                                 seguro,
                                 rfc,
                                 paterno,materno,nombres
            ON KEY (ESC)
                LET int_flag = FALSE
                EXIT CONSTRUCT

            ON KEY (control-c)
                LET int_flag = TRUE
                EXIT CONSTRUCT
        END CONSTRUCT

        IF int_flag = TRUE THEN
             LET int_flag = FALSE
             CLEAR SCREEN
             CLOSE WINDOW pantalla1
             RETURN
        END IF

        LET sel_where = " SELECT A.status_interno,",
                        " E.desc_status_corta,",
                        " A.nro_solicitud,",
                        " A.codven,",
                        " A.cod_promotor,",
                        " A.seguro,",
                        " A.rfc,",
                        " A.unico,",
                        " A.paterno,",
                        " A.materno,",
                        " A.nombres,",
                        " A.diag_proceso,",
                        " A.fnaci,",
                        " A.fecha_baja,",
                        " A.fingre,",
                        " A.fenvio,",
                        " A.fecha_registro,",
                        " A.fecha_proceso,",
                        " A.resuelva,",
                        " A.horas_capacit,",
                        " A.escolar    ,",
                        " ' '         ,",     --(v10)
                        " ' '    ,",     --(v10)
##                        " A.tipo_recibo,",
                        " A.calle,",
                        " A.numero,",
                        " A.dpto,",
                        " A.codpos,",
                        " A.colonia,",
                        " A.deleg,",
                        " B.deleg_desc,",
                        " A.ciudad,",
                        " C.ciudad_desc,",
                        " A.estado,",
                        " D.estad_desc,",
                        " A.fono ",
                        " FROM   pro_solicitud A,",
                        " OUTER(tab_delegacion B,tab_ciudad C,",
                        " tab_estado D,pro_status_interno E) ",
                        " WHERE  ",cla_where CLIPPED,
                        " AND    B.deleg_cod  = A.deleg ",
                        " AND    C.ciudad_cod   = A.ciudad ",
                        " AND    D.estad_cod  = A.estado ",
                        " AND    E.status_interno = A.status_interno ",
                        " ORDER BY 3,6 "

        PREPARE query FROM sel_where
        DECLARE cursor_3 CURSOR FOR query

        LET pos = 1

        FOREACH cursor_3 INTO l_record[pos].*
             SELECT desc_status_corta
             INTO   l_record[pos].desc_status_corta
             FROM   pro_status_interno
             WHERE  status_interno = l_record[pos].status_interno

             SELECT A.deleg_desc
             INTO   l_record[pos].delegdesc
             FROM   tab_delegacion A
             WHERE  A.deleg_cod = l_record[pos].deleg  

             SELECT A.ciudad_desc   
             INTO   l_record[pos].ciudaddesc
             FROM   tab_ciudad A
             WHERE  A.ciudad_cod = l_record[pos].ciudad  

             SELECT A.estad_desc  
             INTO   l_record[pos].estadodesc
             FROM   tab_estado A
             WHERE  A.estad_cod = l_record[pos].estado  

             IF l_record[pos].fecha_baja = "01/01/1900" THEN
                LET l_record[pos].fecha_baja = " "
             END IF

--(v10) inicia              

       CALL curp(l_record[pos].unico) RETURNING l_record[pos].sexo,
                                       l_record[pos].edo_naci
--(v10) finaliza              
        
             IF pos = 200 THEN
                 LET pos = pos + 1
                 ERROR "  EL ARREGLO HA SIDO SOBREPASADO" ATTRIBUTE(NORMAL)
                 EXIT FOREACH
             END IF
             LET pos = pos + 1
        END FOREACH

        LET cont_reg = pos-1  
                                                                         
        DISPLAY "REGISTROS CARGADOS ",cont_reg ," " AT 21,50 
        ATTRIBUTE(REVERSE) 

        IF (pos-1) >= 1 THEN
             CALL SET_COUNT(pos-1)
             DISPLAY ARRAY l_record TO scr_1.*
                 ON KEY (INTERRUPT)
                     EXIT DISPLAY
             END DISPLAY
             CLOSE WINDOW pantalla1
        ELSE
             ERROR "  REGISTROS DE PROMOTORES....NO EXISTE" ATTRIBUTE(NORMAL)
             SLEEP 3
             ERROR ""
             CLOSE WINDOW pantalla1
        END IF
     END IF
     CLEAR SCREEN
END FUNCTION

FUNCTION modifica()
#m-----------------
    LET pos = 2
    IF (pos-1) >= 1 THEN
        CALL SET_COUNT(pos-1)

        LET HOY      = TODAY

        OPEN WINDOW pantalla2 AT 2,3 WITH FORM "PROM0032" ATTRIBUTE(BORDER)
        DISPLAY " PROM003      SOLICITUDES DE REGISTRO DE AGENTES PROMOTORES                    " AT 2,1 ATTRIBUTE(REVERSE)
        DISPLAY "                     DOMICILIO   DE   CORRESPONDENCIA                          " AT 14,1 ATTRIBUTE(REVERSE) 
        DISPLAY HOY USING "DD-MM-YYYY" AT 2,66 ATTRIBUTE(REVERSE)

        LET int_flag = FALSE

        DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(NORMAL)
        DISPLAY " ESC: Consultar  Ctrl-C: Salir  <ENTER> Seleccion de",
                " registro " AT 1,1
 
        WHILE TRUE
            CALL construccion() #c
            RETURNING salida

            IF salida = "S" THEN
                CALL inicializa()
                CLEAR FORM
            ELSE
                LET salida = NULL
                CALL inicializa()
                EXIT WHILE
            END IF
        END WHILE
    END IF

    CLOSE WINDOW pantalla2
    CLEAR SCREEN
END FUNCTION

FUNCTION construccion()
#c--------------------
    DEFINE x_fecha        CHAR(10) ,
           desciuda       CHAR(18) ,
           dessup         CHAR(18) ,
           desnivel       CHAR(18) ,
           desdeleg       CHAR(18) ,
           aaa            CHAR(02) ,
           z_fecha        CHAR(10) ,
           mm             CHAR(02) ,
           dd             CHAR(02) ,
           st_int         SMALLINT ,

           xx_fecha       DATE     ,
           j_fecha        DATE     ,

           xxx            SMALLINT , 
           cont_reg       SMALLINT ,

           vrfc           CHAR(13),
           v_1            SMALLINT,
           val_1          CHAR(80),
           sexo_cur       CHAR(01),
           dig_curp       SMALLINT,
           i, long        SMALLINT,
           caracteres     SMALLINT,
           ayo_s          SMALLINT,
           calle          CHAR(40),
           espe           CHAR(40),
           pasa           SMALLINT,
           pasa_curp      SMALLINT,
           desc_err       CHAR(60),
           ayo_x          CHAR(10),
           ayo_1          SMALLINT,
           ayo_2          SMALLINT
 
        CONSTRUCT cla_where ON   nro_solicitud,
                                 codven,
                                 cod_promotor,
                                 seguro,
                                 rfc,
                                 paterno,
                                 materno,
                                 nombres
                            FROM nro_solicitud,
                                 codven,
                                 cod_promotor,
                                 seguro,
                                 rfc,
                                 paterno,
                                 materno,
                                 nombres
            ON KEY (ESC)
                LET int_flag = FALSE
                EXIT CONSTRUCT
            ON KEY (control-c)
                LET int_flag = TRUE
                EXIT CONSTRUCT
        END CONSTRUCT

        IF int_flag = TRUE THEN
             LET int_flag = FALSE
             CLEAR SCREEN
             LET salida = "N"
             RETURN salida
        END IF

        LET sel_where = " SELECT A.status_interno,",
                        "        E.desc_status_corta,",    
                        "        A.nro_solicitud,",
                        "        A.codven,",
                        "        A.cod_promotor,",
                        "        A.seguro,",
                        "        A.rfc,",
                        "        A.unico,",
                        "        A.paterno,",
                        "        A.materno,",
                        "        A.nombres,",
                        "        A.diag_proceso,",
                        "        A.fnaci,",
                        "        A.fecha_baja,",
                        "        A.fingre,",
                        "        A.fenvio,",
                        "        A.fecha_registro,",
                        "        A.fecha_proceso,",
                        "        A.resuelva,",
                        "        A.horas_capacit,",
                        "        A.escolar,",
                        "        ' ',",         --(v10)
                        "        ' ',",      --(v10)
##                        "        A.tipo_recibo,",
                        "        A.calle,",
                        "        A.numero,",
                        "        A.dpto,",
                        "        A.codpos,",
                        "        A.colonia,",
                        "        A.deleg,",
                        "        B.deleg_desc,",
                        "        A.ciudad,",
                        "        C.ciudad_desc,",
                        "        A.estado,",
                        "        D.estad_desc,",
                        "        A.fono",
                        " FROM   pro_solicitud A,",
                        " OUTER(tab_delegacion B,tab_ciudad C,",
                        " tab_estado D,pro_status_interno E) ",
                        " WHERE  ",cla_where CLIPPED,
                        " AND    B.deleg_cod  = A.deleg ",
                        " AND    C.ciudad_cod   = A.ciudad ",
                        " AND    D.estad_cod  = A.estado ",
                        " AND    E.status_interno = A.status_interno ",
                        " ORDER BY 3,6 "
        LET pos = 1

        PREPARE query2 FROM sel_where
        DECLARE cursor_4 CURSOR FOR query2

        FOREACH cursor_4 INTO l_record[pos].*
             SELECT desc_status_corta
             INTO   l_record[pos].desc_status_corta
             FROM   pro_status_interno
             WHERE  status_interno = l_record[pos].status_interno

             SELECT A.deleg_desc
             INTO   l_record[pos].delegdesc
             FROM   tab_delegacion A
             WHERE  A.deleg_cod = l_record[pos].deleg  

             SELECT A.ciudad_desc   
             INTO   l_record[pos].ciudaddesc
             FROM   tab_ciudad A
             WHERE  A.ciudad_cod = l_record[pos].ciudad  

             SELECT A.estad_desc  
             INTO   l_record[pos].estadodesc
             FROM   tab_estado A
             WHERE  A.estad_cod = l_record[pos].estado  
--(v10) inicia

             CALL curp(l_record[pos].unico) RETURNING l_record[pos].sexo,
                                                      l_record[pos].edo_naci
--(v10) finaliza


             IF pos = 200 THEN
                 LET pos = pos + 1
                 ERROR "  EL ARREGLO HA SIDO SOBREPASADO" ATTRIBUTE(NORMAL)
                 EXIT FOREACH
             END IF
             LET pos = pos + 1
        END FOREACH

        LET cont_reg = pos-1  
                                                                         
        DISPLAY "REGISTROS CARGADOS ",cont_reg ," " AT 21,50 
        ATTRIBUTE(REVERSE) 

        IF (pos-1) >= 1 THEN
           CALL SET_COUNT(pos-1)
           DISPLAY ARRAY l_record TO scr_1.*
              ON KEY (CONTROL-M)
                 LET pos = ARR_CURR()

                 IF l_record[pos].status_interno = 0  OR
                    l_record[pos].status_interno = 10 OR
                    l_record[pos].status_interno = 20 OR
                    l_record[pos].status_interno = 7 THEN

                    LET reg.nro_solicitud  = l_record[pos].nro_solicitud
                    LET reg.codven         = l_record[pos].codven
                    LET reg.seguro         = l_record[pos].seguro
                    LET reg.rfc            = l_record[pos].rfc
                    LET reg.unico          = l_record[pos].unico
                    LET reg.paterno        = l_record[pos].paterno
                    LET reg.materno        = l_record[pos].materno
                    LET reg.nombres        = l_record[pos].nombres
                    LET reg.diag_proceso   = l_record[pos].diag_proceso
                    LET reg.fnaci          = l_record[pos].fnaci
                    LET reg.fecha_baja     = l_record[pos].fecha_baja
                    LET reg.fingre         = l_record[pos].fingre
                    LET reg.fenvio         = l_record[pos].fenvio
                    LET reg.fecha_registro = l_record[pos].fecha_registro
                    LET reg.fecha_proceso  = l_record[pos].fecha_proceso
                    LET reg.resuelva       = l_record[pos].resuelva
                    LET reg.sexo           = l_record[pos].sexo        --(v10)
                    LET reg.edo_naci       = l_record[pos].edo_naci    --(v10)
                    LET reg.horas_capacit  = l_record[pos].horas_capacit
                    LET reg.escolar        = l_record[pos].escolar      
##                    LET reg.tipo_recibo    = l_record[pos].tipo_recibo  
                    LET reg.calle          = l_record[pos].calle
                    LET reg.numero         = l_record[pos].numero
                    LET reg.dpto           = l_record[pos].dpto
                    LET reg.codpos         = l_record[pos].codpos
                    LET reg.colonia        = l_record[pos].colonia
                    LET reg.deleg          = l_record[pos].deleg
                    LET reg.delegdesc      = l_record[pos].delegdesc
                    LET reg.ciudad         = l_record[pos].ciudad
                    LET reg.ciudaddesc     = l_record[pos].ciudaddesc
                    LET reg.estado         = l_record[pos].estado
                    LET reg.estadodesc     = l_record[pos].estadodesc
                    LET reg.fono           = l_record[pos].fono
                    LET reg.cod_promotor   = l_record[pos].cod_promotor
                    EXIT DISPLAY
                 ELSE
                         CALL pregunta3()
                         IF sn MATCHES "[Ss]" THEN
                             LET salida = "S" 
                             EXIT DISPLAY
                         ELSE
                             LET salida = "N" 
                             EXIT DISPLAY
                         END IF
                 END IF

              ON KEY (CONTROL-C,INTERRUPT)
                     LET salida = "N" 
                     EXIT DISPLAY
           END DISPLAY

           CASE salida
                 WHEN "S"
                     RETURN salida
                 WHEN "N"
                     RETURN salida
                 OTHERWISE
                     EXIT CASE
           END CASE
        ELSE
             ERROR "  REGISTROS DE PROMOTORES....NO EXISTE" ATTRIBUTE(NORMAL)
             SLEEP 1
             ERROR ""
             LET salida = "N" 
             RETURN salida
        END IF

        DISPLAY " ESC: Grabar     Ctrl-C: Salir  ","" AT 1,1
        INPUT BY NAME reg.nro_solicitud THRU reg.fono  WITHOUT DEFAULTS
            BEFORE FIELD nro_solicitud
                    NEXT FIELD codven

            AFTER FIELD codven   
                 IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                     NEXT FIELD fono
                 END IF

            AFTER FIELD cod_promotor   
                 IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                     NEXT FIELD codven
                 END IF
                 IF reg.cod_promotor IS NULL THEN
                        LET reg.fecha_baja = "01/01/1900"
                        DISPLAY BY NAME reg.fecha_baja
                            NEXT FIELD seguro
                 END IF

            AFTER FIELD seguro
                 IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                     NEXT FIELD cod_promotor
                 END IF

                 LET sw_1 = 0
                 IF reg.seguro IS NULL OR reg.seguro = " " THEN
                     ERROR "  Debe ingresar RFC CON HOMOCLAVE " 
                           ATTRIBUTE(NORMAL)
                     NEXT FIELD rfc
                 END IF

                 LET sw_1 = 1

                 IF reg.seguro IS NOT NULL AND reg.seguro <> " " THEN
                    IF LENGTH(reg.seguro) <> 11 THEN
                        ERROR "  Debe ingresar N.S.S. completo" 
                                      ATTRIBUTE(NORMAL)
                        NEXT FIELD seguro
                    END IF
       
                    CALL  digito_verif(reg.seguro[1,10],10)
                    RETURNING digito
       
                    IF digito = 32000 THEN
                        ERROR "  N.S.S. solo contiene digitos"
                                      ATTRIBUTE(NORMAL)
                        NEXT FIELD seguro
                    END IF
       
                    IF LENGTH(reg.seguro)=11 AND digito <> reg.seguro[11] THEN
                       ERROR "  DIGITO VERIFICADOR INVALIDO, EL DIGITO DEBE ",
                             "SER :",
                             digito ATTRIBUTE(NORMAL)
                       SLEEP 2
                       NEXT FIELD seguro
                    END IF

                    IF reg.seguro[11] <> "1" AND
                       reg.seguro[11] <> "2" AND
                       reg.seguro[11] <> "3" AND
                       reg.seguro[11] <> "4" AND
                       reg.seguro[11] <> "5" AND
                       reg.seguro[11] <> "6" AND
                       reg.seguro[11] <> "7" AND
                       reg.seguro[11] <> "8" AND
                       reg.seguro[11] <> "9" AND
                       reg.seguro[11] <> "0" THEN
                        ERROR "N.S.S. solo contiene digitos"
                        NEXT FIELD seguro
                    END IF

                 END IF                         

            AFTER FIELD rfc
                 IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                     NEXT FIELD seguro
                 END IF

                 IF reg.rfc IS NULL  OR  reg.rfc = " " THEN
                     ERROR "  RFC NO puede ser NULO"  ATTRIBUTE(NORMAL)
                     NEXT FIELD rfc
                 END IF


              IF LENGTH(reg.rfc CLIPPED) <> 10 AND
                 LENGTH(reg.rfc CLIPPED) <> 13 THEN
                 ERROR "  DEBE INGRESAR R.F.C. COMPLETO" ATTRIBUTE(NORMAL)
                 NEXT FIELD rfc
              END IF
      
              IF NOT valida_fecha_rfc(reg.rfc[5,10]) THEN
                 ERROR "Formato de RFC Incorrecto ( ----AAMMDD*** )"
                 NEXT FIELD rfc
              ELSE
                 WHENEVER ERROR CONTINUE
                   LET aaa     = reg.rfc[5,6]
                   LET mm      = reg.rfc[7,8]
                   LET dd      = reg.rfc[9,10]
                   LET z_fecha = mm,"/",dd,"/19",aaa
                   LET j_fecha = z_fecha
      
                   IF j_fecha IS NULL THEN
                       ERROR "  FECHA INVALIDA EN RFC" ATTRIBUTE(NORMAL)
                       NEXT FIELD rfc
                   END IF
                 WHENEVER ERROR STOP
      
                 IF reg.fnaci IS NULL OR reg.fnaci = " " THEN
                     INITIALIZE ayo_x TO NULL
                     LET ayo_s = 0
                     LET ayo_1 = 0
                     LET ayo_2 = 0
      
                     LET ayo_x = HOY 
                     LET ayo_1 = ayo_x[7,10] 
                     LET ayo_x = reg.fnaci 
                     LET ayo_2 = ayo_x[7,10] 
                     LET ayo_s = ayo_1 - ayo_2
      
                     IF ayo_s < 14 THEN
                        ERROR "  FECHA INVALIDA EN RFC TIENE MENOS DE 14 AÑOS "
                        ATTRIBUTE(NORMAL)
                        NEXT FIELD rfc
                     END IF
      
                     LET reg.fnaci = z_fecha
                     DISPLAY BY NAME reg.fnaci
                 END IF
              END IF
      
              IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
                 FGL_LASTKEY() = FGL_KEYVAL("RIGHT") THEN
                 LET v_1 = 0
                 INITIALIZE val_1 TO NULL
                 CALL verifica_rfc(reg.rfc[1,4])
                                      RETURNING v_1,val_1 #ve--
                 IF v_1 = 1 THEN
                    ERROR "R.F.C. ",val_1 CLIPPED
                    NEXT FIELD rfc
                 END IF
      
                 IF reg.seguro IS NOT NULL THEN
                    IF reg.seguro[5,6] <> reg.rfc[5,6] THEN
                       ERROR "El AÑO DEL NSS Y EL RFC SON DIFERENTES "
                       ATTRIBUTE(NORMAL)
                       SLEEP 3
                       ERROR ""
                    END IF 
                 END IF
                 NEXT FIELD unico
              END IF
      
              IF sw_1 = 0 THEN
                 IF LENGTH(reg.rfc) < 13 THEN
                    ERROR " "
                    ERROR "  DEBIO HABER INGRESADO EL RFC CON HOMOCLAVE" 
                          ATTRIBUTE(NORMAL)
                 END IF
              END IF
      
              IF reg.rfc IS NOT NULL OR reg.rfc[1,2] <> "  " THEN
                 LET v_1 = 0
                 INITIALIZE val_1 TO NULL
                 CALL verifica_rfc(reg.rfc[1,4])
                                      RETURNING v_1,val_1 #ve--
                 IF v_1 = 1 THEN
                    ERROR "R.F.C. ",val_1 CLIPPED
                    NEXT FIELD rfc
                 END IF
      
                 IF reg.seguro IS NOT NULL THEN
                    IF reg.seguro[5,6] <> reg.rfc[5,6] THEN
                       ERROR "El AÑO DEL NSS Y EL RFC SON DIFERENTES "
                       ATTRIBUTE(NORMAL)
                       SLEEP 3
                       ERROR ""
                    END IF 
                 END IF
                 NEXT FIELD unico
      
                 IF NOT valida_fecha_rfc(reg.rfc[5,10]) THEN
                    ERROR "Formato de RFC Incorrecto ( ----AAMMDD*** )"
                    ATTRIBUTE(NORMAL)
                    NEXT FIELD rfc
                 ELSE
                    WHENEVER ERROR CONTINUE
                      LET aaa     = reg.rfc[5,6]
                      LET mm      = reg.rfc[7,8]
                      LET dd      = reg.rfc[9,10]
                      LET z_fecha = mm,"/",dd,"/19",aaa
                      LET j_fecha = z_fecha
      
                      IF j_fecha IS NULL THEN
                          ERROR "  FECHA INVALIDA EN RFC" ATTRIBUTE(NORMAL)
                          NEXT FIELD rfc
                      END IF
                    WHENEVER ERROR STOP
      
                    IF reg.fnaci IS NULL OR reg.fnaci = " " THEN
                       INITIALIZE ayo_x TO NULL
                       LET ayo_s = 0
                       LET ayo_1 = 0
                       LET ayo_2 = 0
      
                       LET ayo_x = HOY
                       LET ayo_1 = ayo_x[7,10]
                       LET ayo_x = reg.fnaci
                       LET ayo_2 = ayo_x[7,10]
                       LET ayo_s = ayo_1 - ayo_2
      
                       IF ayo_s < 14 THEN
                          ERROR "  FECHA INVALIDA EN RFC TIENE MENOS DE 14 AÑOS "
                          ATTRIBUTE(NORMAL)
                          NEXT FIELD rfc
                       END IF
      
                       LET reg.fnaci = z_fecha
                       DISPLAY BY NAME reg.fnaci
                    END IF
                 END IF
      
              END IF
      
              IF  reg.rfc IS NOT NULL THEN
                  LET vrfc = reg.rfc[1,10]
                  CALL rfc_promotor(vrfc)
              END IF


            AFTER FIELD unico   
             IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                IF reg.seguro IS NOT NULL AND reg.unico IS NOT NULL THEN
                   IF reg.seguro[5,6] <> reg.rfc[5,6] AND 
                      reg.rfc[5,6] <> reg.unico[5,6]  THEN
                      ERROR "El AÑO DEL NSS, RFC Y CURP SON DIFERENTES "
                      ATTRIBUTE(NORMAL)
                      SLEEP 3
                      ERROR ""
                   END IF 
                END IF
     
                IF reg.seguro IS NULL THEN
                   IF reg.rfc[5,6] <> reg.unico[5,6] THEN
                      ERROR "El AÑO DEL RFC Y CURP SON DIFERENTES "
                      ATTRIBUTE(NORMAL)
                      SLEEP 3
                      ERROR ""
                   END IF
                END IF
                NEXT FIELD rfc
             END IF
     
             IF LENGTH(reg.unico) < 18 AND
                LENGTH(reg.unico) > 0  THEN
                 ERROR "Debe ingresar CURP completa"
                 ATTRIBUTE(NORMAL)
                 NEXT FIELD unico
             ELSE
                 IF reg.unico[1] <> " " OR
                    reg.unico IS NOT NULL THEN
                     IF reg.unico[11] = "H" THEN
                         LET sexo_cur = "1"
                     ELSE
                         LET sexo_cur = "2"
                     END IF
     
                     CALL valida_est_curp(reg.unico)
                     RETURNING pasa_curp, desc_err
                     IF pasa_curp = 1 THEN
                        ERROR "", desc_err
                        LET pasa_curp = 0
                        NEXT FIELD unico
                     END IF
     
                     CALL var_dig_curp(reg.unico) RETURNING pasa, dig_curp
                     IF pasa = 0 THEN
                       ERROR "Digito Verificador Invalido curp, el digito es : ",
                       dig_curp
                       LET pasa = 0
                       NEXT FIELD unico
                     END IF
                 ELSE
                     LET sexo_cur = " "
                 END IF
             END IF
     
             IF reg.unico[1] = " " OR reg.unico IS NULL THEN
                 ERROR "Debe ingresar CURP correcta"
                 NEXT FIELD unico
             END IF
     
             IF reg.seguro IS NOT NULL AND reg.unico IS NOT NULL THEN
                IF reg.seguro[5,6] <> reg.rfc[5,6] AND
                   reg.rfc[5,6] <> reg.unico       THEN
                   ERROR "El AÑO DEL NSS, RFC Y CURP SON DIFERENTES "
                   ATTRIBUTE(NORMAL)
                   SLEEP 3
                   ERROR ""
                END IF
             END IF
     
             IF reg.seguro IS NULL THEN
                IF reg.rfc[5,6] <> reg.unico[5,6] THEN
                   ERROR "El AÑO DEL RFC Y CURP SON DIFERENTES "
                   ATTRIBUTE(NORMAL)
                   SLEEP 3
                  ERROR ""
                END IF
             END IF
     
             NEXT FIELD paterno

            AFTER FIELD paterno   
             IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                IF reg.seguro IS NOT NULL AND reg.unico IS NOT NULL THEN
                   IF reg.seguro[5,6] <> reg.rfc[5,6] AND
                      reg.rfc[5,6] <> reg.unico       THEN
                      ERROR "El AÑO DEL NSS, RFC Y CURP SON DIFERENTES "
                      ATTRIBUTE(NORMAL)
                      SLEEP 3
                      ERROR ""
                   END IF
                END IF

                IF reg.seguro IS NULL THEN
                   IF reg.rfc[5,6] <> reg.unico[5,6] THEN
                      ERROR "El AÑO DEL RFC Y CURP SON DIFERENTES "
                      ATTRIBUTE(NORMAL)
                      SLEEP 3
                      ERROR ""
                   END IF
                END IF
                NEXT FIELD unico
             END IF

             IF reg.paterno  IS NULL OR reg.paterno[1] = " " THEN
                ERROR "  EL APELLIDO PATERNO DEBE SER INGRESADO"
                      ATTRIBUTE(NORMAL)
                NEXT FIELD paterno
             END IF

            AFTER FIELD materno   
             IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD paterno
             END IF

             IF reg.materno[1] = " "  OR reg.materno[1,2] = " ." OR
                reg.materno[1] = "."  OR reg.materno[1,2] = ".." OR
                reg.materno[1] = "X " OR reg.materno[1,2] = " X" OR
                reg.materno[1,2] = ".X"  THEN
                ERROR "  EL APELLIDO MATERNO NO PUEDE SER UN PUNTO o X "
                      ATTRIBUTE(NORMAL)
                NEXT FIELD materno
             END IF


            AFTER FIELD nombres      
             IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD materno
             END IF

             IF reg.nombres  IS NULL OR reg.nombres[1] = " " THEN
                ERROR "  EL NOMBRE ES REQUERIDO" ATTRIBUTE(NORMAL)
                NEXT FIELD nombres
             END IF

            BEFORE FIELD fingre     
                 IF reg.fingre IS NULL THEN
                     LET reg.fingre = HOY
                     DISPLAY BY NAME reg.fingre
                 END IF

            AFTER FIELD fingre     
                 IF reg.fingre IS NULL THEN
                     ERROR "  Se requiere la fecha del ",
                           "  llenado de la solicitud" ATTRIBUTE(NORMAL)
                     NEXT FIELD fingre   
                 END IF

            AFTER FIELD resuelva        
                 IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                     NEXT FIELD fingre
                 END IF

                 IF reg.resuelva  IS NULL  OR  reg.resuelva = " " THEN
                     ERROR "  Campo no puede ser nulo " ATTRIBUTE(NORMAL)
                     NEXT FIELD resuelva    
              END IF

      -- En el mpt con fecha 29072009 se especifica que la calificacion
      -- debera ser menor o igual a 100

               IF reg.resuelva < 000 OR reg.resuelva > 100 THEN     --(v10)
                 ERROR "ERROR...CALIFICACION DEBE SER MENOR O IGUAL A 100"--(v10)
                 NEXT FIELD resuelva
               END IF
                                                                  
             AFTER FIELD escolar
                IF reg.escolar IS NULL THEN
                    CALL grado_escolar()
                    RETURNING reg.escolar

                    IF reg.escolar IS NULL THEN
                        DISPLAY reg.escolar TO escolar
                        NEXT FIELD escolar
                    ELSE
                        DISPLAY reg.escolar TO escolar
--                        NEXT FIELD sexo      --(v10)
                        NEXT FIELD calle     --(v10)
                    END IF
                ELSE
                    IF reg.escolar MATCHES "[ABCDEFGHIJKL]" THEN
--                       NEXT FIELD sexo      --(v10)
                       NEXT FIELD calle      --(v10)
                    ELSE
                        ERROR "NO EXISTE EL GRADO DE ESCOLARIDAD"
                        NEXT FIELD escolar
                    END IF
                END IF

             AFTER FIELD calle    
                 IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                     NEXT FIELD resuelva
##                     NEXT FIELD horas_capacit
                 END IF


                  IF reg.calle IS NOT NULL THEN
                     LET caracteres = 0
            
                     IF reg.calle[1] = "[" OR reg.calle[1] = '"'  OR
                        reg.calle[1] = "]" OR reg.calle[1] = "#"  OR
                        reg.calle[1] = "$" OR reg.calle[1] = "%"  OR
                        reg.calle[1] = "&" OR reg.calle[1] = "="  OR
                        reg.calle[1] = "/" OR reg.calle[1] = "?"  OR
                        reg.calle[1] = "-" OR reg.calle[1] = "'"  OR
                        reg.calle[1] = "(" OR reg.calle[1] = ")"  OR
                        reg.calle[1] = "^" OR reg.calle[1] = "!"  OR
                        reg.calle[1] = "~" OR reg.calle[1] = "_"  OR
                        reg.calle[1] = ":" OR reg.calle[1] = "."  OR
                        reg.calle[1] = "," OR reg.calle[1] = ";"  OR
                        reg.calle[1] = "<" OR reg.calle[1] = ">"  OR
                        reg.calle[1] = "@" OR reg.calle[1] = "|"  OR
                        reg.calle[1] = "{" OR reg.calle[1] = "}"  OR
                        reg.calle[1] = "+" OR reg.calle[1] = "*"  OR
                        reg.calle[1] = "`" OR reg.calle[1] = "¿"  OR
                        reg.calle[1] = "¡" OR reg.calle[1] = "Ä"  OR
                        reg.calle[1] = "É" OR reg.calle[1] = "Í"  OR
                        reg.calle[1] = "Ó" OR reg.calle[1] = "Ú"  OR
                        reg.calle[1] = "¨" OR reg.calle[1] = "Ä"  OR
                        reg.calle[1] = "Ë" OR reg.calle[1] = "Ï"  OR
                        reg.calle[1] = "Ö" OR reg.calle[1] = "Ö"  OR
                        reg.calle[1] = "Ü" OR reg.calle[1] = "´"  OR
                        reg.calle[1] = "Á" THEN
                  
                        ERROR "  EL DATO NO PUEDE COMENZAR CON PUNTO O ",
                           "CARACTER ESPECIAL"
                        ATTRIBUTE(NORMAL)
                        SLEEP 2
                        LET caracteres = 0
                        NEXT FIELD calle
                     END IF
            
                     LET calle = reg.calle CLIPPED
                     LET long = LENGTH(calle CLIPPED)
            
                     IF caracteres = 0 THEN
                        FOR i = 1 TO long
                           IF calle[i,i] = "[" OR calle[i,i] = '"'  OR
                              calle[i,i] = "]" OR calle[i,i] = "#"  OR
                              calle[i,i] = "$" OR calle[i,i] = "%"  OR
                              calle[i,i] = "&" OR calle[i,i] = "="  OR
                              calle[i,i] = "/" OR calle[i,i] = "?"  OR
                              calle[i,i] = "-" OR calle[i,i] = "'"  OR
                              calle[i,i] = "(" OR calle[i,i] = ")"  OR
                              calle[i,i] = "^" OR calle[i,i] = "!"  OR
                              calle[i,i] = "~" OR calle[i,i] = "_"  OR
                              calle[i,i] = ":" OR calle[i,i] = "."  OR
                              calle[i,i] = "," OR calle[i,i] = ";"  OR
                              calle[i,i] = "<" OR calle[i,i] = ">"  OR
                              calle[i,i] = "@" OR calle[i,i] = "|"  OR
                              calle[i,i] = "{" OR calle[i,i] = "}"  OR
                              calle[i,i] = "+" OR calle[i,i] = "*"  OR
                              calle[i,i] = "`" OR calle[i,i] = "¿"  OR
                              calle[i,i] = "¡" OR calle[i,i] = "Ä"  OR
                              calle[i,i] = "É" OR calle[i,i] = "Í"  OR
                              calle[i,i] = "Ó" OR calle[i,i] = "Ú"  OR
                              calle[i,i] = "¨" OR calle[i,i] = "Ä"  OR
                              calle[i,i] = "Ë" OR calle[i,i] = "Ï"  OR
                              calle[i,i] = "Ö" OR calle[i,i] = "Ö"  OR
                              calle[i,i] = "Ü" OR calle[i,i] = "´"  OR
                              calle[i,i] = "Á" THEN
                     
                              LET espe[i,i] = calle[i,i]
                              LET caracteres = caracteres + 1
                           END IF
                        END FOR
            
                        IF caracteres > 2 THEN
                           ERROR "  EL DATO NO PUEDE TENER VARIOS CARACTER ",
                                 "ESPECIAL"
                           ATTRIBUTE(NORMAL)
                           SLEEP 2
                           LET caracteres = 0
                           NEXT FIELD calle
                        END IF
                     END IF
            
                     IF caracteres = 0 THEN
                        FOR i = 1 TO long
                           IF calle[i,i] = "." THEN
                              IF calle[i,i+1] = ".." OR
                                 calle[i,i+1] = "./" OR
                                 calle[i,i+1] = ".$" OR
                                 calle[i,i+1] = ".{" OR
                                 calle[i,i+1] = ".[" OR
                                 calle[i,i+1] = ".}" OR
                                 calle[i,i+1] = ".%" OR
                                 calle[i,i+1] = ".?" OR
                                 calle[i,i+1] = ".!" OR
                                 calle[i,i+1] = '."' OR
                                 calle[i,i+1] = ".&" OR
                                 calle[i,i+1] = ".(" OR
                                 calle[i,i+1] = ".)" OR
                                 calle[i,i+1] = ".=" OR
                                 calle[i,i+1] = ".|" OR
                                 calle[i,i+1] = ".@" OR
                                 calle[i,i+1] = ".<" OR
                                 calle[i,i+1] = ".>" OR
                                 calle[i,i+1] = ".;" OR
                                 calle[i,i+1] = ".:" OR
                                 calle[i,i+1] = ".," OR
                                 calle[i,i+1] = ".+" OR
                                 calle[i,i+1] = ".-" THEN
                                 LET caracteres = 999
                                 EXIT FOR
                              END IF
                           END IF
            
                           IF caracteres = 0 THEN
                              IF calle[i,i+1] = "##" OR
                                 calle[i,i+1] = "%%" OR
                                 calle[i,i+1] = "$$" OR
                                 calle[i,i+1] = "//" OR
                                 calle[i,i+1] = "((" OR
                                 calle[i,i+1] = "()" OR
                                 calle[i,i+1] = "))" OR
                                 calle[i,i+1] = '!"' OR
                                 calle[i,i+1] = "|!" OR
                                 calle[i,i+1] = "==" OR
                                 calle[i,i+1] = "==" OR
                                 calle[i,i+1] = "==" THEN
                                 LET caracteres = 999
                                 EXIT FOR
                              END IF
                           END IF
                        END FOR
                     END IF
                  END IF
            
                  IF caracteres > 2 AND caracteres < 40 OR caracteres = 999 THEN
                     ERROR "  LA CALLE CONTIENE VARIOS CARACTERES NO VALIDOS "
                     ATTRIBUTE(NORMAL)
                     SLEEP 2
                     LET caracteres = 0
                     NEXT FIELD calle
                  END IF
            
                  IF reg.calle IS NULL OR reg.calle[1] = " " THEN
                     ERROR "  EL DATO NO DEBE SER NULO O COMENZAR CON BLANCO, ",
                           "REINGRESE ..."
                     ATTRIBUTE(NORMAL)
                     NEXT FIELD calle
                  END IF

             AFTER FIELD numero    
                 IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                     NEXT FIELD calle
                 END IF

                 IF reg.numero IS NULL THEN
                     ERROR "  Dato no puede ser nulo reingrese ..."                                        ATTRIBUTE(NORMAL)
                     NEXT FIELD numero
                 END IF

             AFTER FIELD dpto    
                 IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                     NEXT FIELD numero
                 END IF

             AFTER FIELD codpos     
                 IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                     NEXT FIELD dpto
                 END IF

                 IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
                     NEXT FIELD fono
                 END IF

                 IF reg.codpos IS NULL THEN
                     CALL despliega_codigo_postal() #dcp
                                    RETURNING reg.codpos,   
                                              reg.colonia,   
                                              reg.deleg, 
                                              desdeleg, 
                                              reg.ciudad, 
                                              desciuda, 
                                              reg.estado,
                                              desestad
                     IF reg.colonia IS NULL THEN
                         ERROR "  Este Codigo Postal no existe en el catalogo"
                               ATTRIBUTE(NORMAL)
                         NEXT FIELD codpos
                     END IF
                 ELSE
                     SELECT "X" 
                     FROM   tab_codpos
                     WHERE  cpos_cod = reg.codpos

                     IF STATUS = 100 THEN
                         ERROR "  Cod. Post. no existe en catalogo, pon",
                               "  valor NULO p/desplegar pantalla de Codigos"
                               ATTRIBUTE(NORMAL)
                         NEXT FIELD codpos
                     END IF

                     CALL Despliega_colonias(reg.codpos)
                          RETURNING reg.colonia, 
                                    reg.deleg, 
                                    desdeleg, 
                                    reg.ciudad, 
                                    desciuda, 
                                    reg.estado,
                                    desestad
              END IF

                 DISPLAY BY NAME reg.colonia,reg.deleg,reg.ciudad,reg.estado
                 DISPLAY desdeleg,desciuda,desestad TO 
                         delegdesc,ciudaddesc,estadodesc

             AFTER FIELD fono
                 IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                     NEXT FIELD estado
                 END IF
    
             ON KEY( ESC )

                SELECT "X" 
                FROM tab_codpos
                WHERE cpos_cod = reg.codpos

                IF STATUS = 100 THEN
                    ERROR "  Cod. Post. no existe en catalogo, pon valor",
                          "  NULO p/desplegar pantalla de Codigos"
                          ATTRIBUTE(NORMAL)
                    NEXT FIELD codpos
                END IF

                IF reg.colonia  IS NULL THEN
                    ERROR "  Dato no puede ser nulo reingrese ..." 
                            ATTRIBUTE(NORMAL)
                    NEXT FIELD  codpos
             END IF

             IF reg.unico IS NOT NULL AND reg.unico <> " " THEN
                IF LENGTH(reg.unico) < 18 THEN
                    ERROR "  Debe ingresar CURP completo" ATTRIBUTE(NORMAL)
                       NEXT FIELD unico
                END IF
             END IF

             IF LENGTH(reg.unico) < 18 AND
                LENGTH(reg.unico) > 0  THEN
                 ERROR "Debe ingresar CURP completa"
                 ATTRIBUTE(NORMAL)
                 NEXT FIELD unico
             END IF

             IF reg.resuelva IS NULL THEN
                  ERROR "  Dato no puede ser nulo reingrese" 
                          ATTRIBUTE(NORMAL)
                  NEXT FIELD resuelva
             END IF

         -- En el mpt con fecha 29072009 se especifica que la calificacion
         -- debera ser menor o igual a 100

             IF reg.resuelva < 000 OR reg.resuelva > 100 THEN          --(v10)
                ERROR "ERROR...CALIFICACION DEBE SER MENOR O IGUAL A 100"--(v10)
                NEXT FIELD resuelva
             END IF

             IF reg.paterno  IS NULL OR reg.paterno[1] = " " OR
                reg.paterno[1] = "." THEN
                    ERROR "  Dato no puede ser nulo o blanco o caracter ",
                          "reingrese ..."
                          ATTRIBUTE(NORMAL)
                    NEXT FIELD paterno  
             END IF

             IF reg.materno[1] = " "  OR reg.materno[1,2] = " ." OR
                reg.materno[1] = "."  OR reg.materno[1,2] = ".." OR
                reg.materno[1] = "X " OR reg.materno[1,2] = " X" OR
                reg.materno[1,2] = ".X"  THEN
                ERROR "  EL APELLIDO MATERNO NO PUEDE SER UN PUNTO o X "
                 ATTRIBUTE(NORMAL)
                NEXT FIELD materno
             END IF

             IF reg.nombres  IS NULL OR reg.nombres[1] = " " OR
                reg.nombres[1] = "." THEN
                    ERROR "  Dato no puede ser nulo o blanco o caracter ",
                          "reingrese ..."
                          ATTRIBUTE(NORMAL)
                    NEXT FIELD nombres  
             END IF

             IF reg.fingre IS NULL THEN
                    ERROR "  Dato no puede ser nulo reingrese ..."
                          ATTRIBUTE(NORMAL)
                    NEXT FIELD fingre   
             END IF

             IF reg.fecha_baja > HOY THEN
                    ERROR "  FECHA NO PUEDE SER SUPERIOR A LA ACTUAL "
                          ATTRIBUTE(NORMAL) 
                    NEXT FIELD fecha_baja
             END IF

             IF reg.horas_capacit < 0 THEN
                    ERROR "  NUMERO DE HORAS DE CAPACITACION NO PUEDE",
                          "  SER MENOR A 0 " ATTRIBUTE(NORMAL)
                    NEXT FIELD horas_capacit  
             END IF


                IF reg.escolar IS NULL THEN
                    CALL grado_escolar()
                    RETURNING reg.escolar

                    IF reg.escolar IS NULL THEN
                        DISPLAY reg.escolar TO escolar
                        NEXT FIELD escolar
                    ELSE
                        DISPLAY reg.escolar TO escolar
                        NEXT FIELD calle
                    END IF
                ELSE
                    IF reg.escolar NOT MATCHES "[ABCDEFGHIJKL]" THEN
                        ERROR "NO EXISTE EL GRADO DE ESCOLARIDAD"
                        NEXT FIELD escolar
                    END IF
                END IF

             IF reg.calle IS NULL THEN
                    ERROR "  Dato no puede ser nulo reingrese ..."
                          ATTRIBUTE(NORMAL)
                    NEXT FIELD calle   
             END IF

             IF reg.numero    IS NULL THEN
                    ERROR "  Dato no puede ser nulo reingrese ..."
                          ATTRIBUTE(NORMAL)
                    NEXT FIELD numero
             END IF

             IF reg.colonia  IS NULL THEN
                    ERROR "  Dato no puede ser nulo reingrese ..."
                          ATTRIBUTE(NORMAL)  
                    NEXT FIELD colonia  
             END IF

             IF reg.codpos  IS NULL THEN
                    ERROR "  Dato no puede ser nulo reingrese ..."
                          ATTRIBUTE(NORMAL)
                    NEXT FIELD codpos    
             END IF

             IF reg.deleg  IS NULL THEN
                    ERROR "  Dato no puede ser nulo reingrese ..."
                          ATTRIBUTE(NORMAL)
                    NEXT FIELD deleg    
             END IF

             IF reg.ciudad   IS NULL THEN
                    ERROR "  Dato no puede ser nulo reingrese ..."
                          ATTRIBUTE(NORMAL)
                    NEXT FIELD ciudad  
             END IF

             IF reg.estado   IS NULL THEN
                    ERROR "  Dato no puede ser nulo reingrese ..."
                          ATTRIBUTE(NORMAL) 
                    NEXT FIELD estado  
             END IF

             IF reg.codpos   IS NULL THEN
                    ERROR "  Dato no puede ser nulo reingrese ..." 
                          ATTRIBUTE(NORMAL)
                    NEXT FIELD codpos  
             END IF

             IF reg.fnaci  IS NULL THEN
                    ERROR " dato no puede ser nulo reingrese ..."
                          ATTRIBUTE(NORMAL)
                    NEXT FIELD fnaci    
             END IF

             IF reg.rfc IS NULL THEN
                    ERROR "  El R.F.C. es requerido" ATTRIBUTE(NORMAL)
                    NEXT FIELD rfc
             ELSE
                   LET vrfc = reg.rfc[1,10]
                   CALL rfc_promotor(vrfc)
             END IF                 

             IF ventro = 1 THEN
                   WHILE TRUE
                   PROMPT "DESEA CONFIRMAR LA ACTUALIZACION AUN ",
                          "CON EL  RFC YA EXISTENTE S/N?" FOR CHAR enter  
                   IF enter MATCHES "[SsNn]" THEN   #1
                      IF enter MATCHES "[Ss]" THEN   #2          

                         -- Valida si el apellido materno es igual a nulo le agregara
                         -- de forma automatica N/A. --(v10)
   
                         IF reg.materno      = " "    OR
                            reg.materno      IS NULL  OR
                            reg.materno      = ""     THEN

                            LET reg.materno  = "N/A"

                         END IF

                         -- fin (v10)

                         UPDATE pro_solicitud 
                         SET    codven        = reg.codven        ,
                                paterno       = reg.paterno       ,
                                materno       = reg.materno       ,
                                nombres       = reg.nombres       ,
                                fecha_baja    = reg.fecha_baja    ,
                                fnaci         = reg.fnaci         ,
                                unico         = reg.unico         ,
                                seguro        = reg.seguro        ,
                                rfc           = reg.rfc           ,
                                calle         = reg.calle         ,
                                numero        = reg.numero        ,
                                dpto          = reg.dpto          ,
                                colonia       = reg.colonia       ,
                                codpos        = reg.codpos        ,
                                deleg         = reg.deleg         ,
                                ciudad        = reg.ciudad        ,
                                estado        = reg.estado        ,
                                fono          = reg.fono          ,
                                resuelva      = reg.resuelva      ,
                                horas_capacit = reg.horas_capacit ,
                                cod_promotor  = reg.cod_promotor ,
                                escolar       = reg.escolar     
                         WHERE  nro_solicitud = reg.nro_solicitud 
                         AND    status_interno IN (0,7,10,20)
                 
                         ERROR "  REGISTRO MODIFICADO..." ATTRIBUTE(NORMAL)
                         SLEEP 2
                         EXIT WHILE
                      ELSE
                         ERROR "  MODIFICACION CANCELADA" ATTRIBUTE(NORMAL)
                         SLEEP 3
                         EXIT WHILE
                      END IF
                    END IF
                   END WHILE 
                   EXIT INPUT 

             ELSE
                   CALL pregunta1()
                   IF sn MATCHES "[Ss]" THEN

                      -- Valida si el apellido materno es igual a nulo le agregara
                      -- de forma automatica N/A. --(v10)

                      IF reg.materno      = " "    OR
                         reg.materno      IS NULL  OR
                         reg.materno      = ""     THEN

                         LET reg.materno  = "N/A"

                      END IF

                      -- fin (v10)

                      UPDATE pro_solicitud
                      SET    codven        = reg.codven        ,
                             paterno       = reg.paterno       ,
                             materno       = reg.materno       ,
                             nombres       = reg.nombres       ,
                             fecha_baja    = reg.fecha_baja    ,
                             fnaci         = reg.fnaci         ,
                             unico         = reg.unico         ,
                             seguro        = reg.seguro        ,
                             rfc           = reg.rfc           ,
                             calle         = reg.calle         ,
                             numero        = reg.numero        ,
                             dpto          = reg.dpto          ,
                             colonia       = reg.colonia       ,
                             codpos        = reg.codpos        ,
                             deleg         = reg.deleg         ,
                             ciudad        = reg.ciudad        ,    
                             estado        = reg.estado        ,
                             fono          = reg.fono          ,
                             resuelva      = reg.resuelva      ,
                             horas_capacit = reg.horas_capacit ,
                             cod_promotor  = reg.cod_promotor ,
                             escolar       = reg.escolar     
--                             sexo          = reg.sexo        ,  --(v10)
--                             edo_nacio     = reg.edo_naci       --(v10)
                       WHERE  nro_solicitud = reg.nro_solicitud 
                         AND    status_interno IN (0,7,10,20)

                         ERROR "  REGISTRO MODIFICADO..." ATTRIBUTE(NORMAL)
                         SLEEP 2
                   ELSE
                         ERROR "  MODIFICACION CANCELADA" ATTRIBUTE(NORMAL)
                         SLEEP 3
                   END IF

                   ERROR ""

                   LET salida = "N"
                   EXIT INPUT 

             END IF

          ON KEY (CONTROL-C,INTERRUPT)
             LET salida = "N"
             EXIT INPUT 
      END INPUT 

      RETURN salida

END FUNCTION

FUNCTION elimina()
#e---------------
    LET pos = 2
    IF (pos-1) >= 1 THEN
        CALL SET_COUNT(pos-1)

        LET HOY      = TODAY

        OPEN WINDOW pantalla3 AT 2,3 WITH FORM "PROM0032" ATTRIBUTE(BORDER)
        DISPLAY " PROM003      SOLICITUDES DE REGISTRO DE AGENTES ",
                "PROMOTORES                    " AT 2,1 ATTRIBUTE(REVERSE)
        DISPLAY "                     DOMICILIO   DE   CORRESPONDEN",
                "CIA                          " AT 14,1 ATTRIBUTE(REVERSE) 
        DISPLAY HOY USING "DD-MM-YYYY" AT 2,66 ATTRIBUTE(REVERSE)

        LET int_flag = FALSE

        DISPLAY "ESC: Consultar    Ctrl-C: Salir    <ENTER> Elimina" AT 1,2
        DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(NORMAL)

        WHILE TRUE
            CALL construccion1() #c1
            RETURNING salida

            IF salida = "S" THEN
                CALL inicializa()
                CLEAR FORM
            ELSE
                LET salida = NULL
                CALL inicializa()
                EXIT WHILE
            END IF
        END WHILE
    END IF

    CLOSE WINDOW pantalla3
    CLEAR SCREEN
END FUNCTION

FUNCTION construccion1()
#c1--------------------
    DEFINE
    cont_reg                  SMALLINT

    CONSTRUCT cla_where ON   nro_solicitud,
                             codven,
                             cod_promotor,
                             seguro,
                             rfc,
                             paterno,
                             materno,
                             nombres
                        FROM nro_solicitud,
                             codven,
                             cod_promotor,
                             seguro,
                             rfc,
                             paterno,
                             materno,
                             nombres
        ON KEY (ESC)
            LET int_flag = FALSE
            EXIT CONSTRUCT

        ON KEY (control-c)
            LET int_flag = TRUE
            EXIT CONSTRUCT
    END CONSTRUCT

    IF int_flag = TRUE THEN
        LET int_flag = FALSE
        CLEAR SCREEN
        LET salida = "N"
        RETURN salida
    END IF

    LET sel_where = " SELECT A.status_interno ,",
                    "        E.desc_status_corta,",     
                    "        A.nro_solicitud,",
                    "        A.codven,",
                    "        A.cod_promotor,",
                    "        A.seguro,",
                    "        A.rfc,",
                    "        A.unico,",
                    "        A.paterno,",
                    "        A.materno,",
                    "        A.nombres,",
                    "        A.diag_proceso,",
                    "        A.fnaci,",
                    "        A.fecha_baja,",
                    "        A.fingre,",
                    "        A.fenvio,",
                    "        A.fecha_registro,",
                    "        A.fecha_proceso,",
                    "        A.resuelva,",
                    "        A.horas_capacit,",
                    "        A.escolar    ,",
                    "        ''               ,",          --(v10)
                    "        ''               ,",          --(v10)
##                    "        A.tipo_recibo,",
                    "        A.calle,",
                    "        A.numero,",
                    "        A.dpto,",
                    "        A.codpos,",
                    "        A.colonia,",
                    "        A.deleg,",
                    "        B.deleg_desc,",
                    "        A.ciudad,",
                    "        C.ciudad_desc,",
                    "        A.estado,",
                    "        D.estad_desc,",
                    "        A.fono ",
                    " FROM   pro_solicitud A,",
                    " OUTER(tab_delegacion B,tab_ciudad C,",
                    " tab_estado D,pro_status_interno E) ",
                    " WHERE  ",cla_where CLIPPED,
                    " AND    B.deleg_cod  = A.deleg ",
                    " AND    C.ciudad_cod   = A.ciudad ",
                    " AND    D.estad_cod  = A.estado ",
                    " AND    E.status_interno = A.status_interno ",
                    " ORDER BY 3,6 "

    PREPARE query3 FROM sel_where

    DECLARE cursor_5 CURSOR FOR query3

    LET pos = 1

    FOREACH cursor_5 INTO l_record[pos].*
        SELECT desc_status_corta
        INTO   l_record[pos].desc_status_corta
        FROM   pro_status_interno
        WHERE  status_interno = l_record[pos].status_interno

        SELECT A.deleg_desc
        INTO   l_record[pos].delegdesc
        FROM   tab_delegacion A
        WHERE  A.deleg_cod = l_record[pos].deleg  

        SELECT A.ciudad_desc   
        INTO   l_record[pos].ciudaddesc
        FROM   tab_ciudad A
        WHERE  A.ciudad_cod = l_record[pos].ciudad  

        SELECT A.estad_desc  
        INTO   l_record[pos].estadodesc
        FROM   tab_estado A
        WHERE  A.estad_cod = l_record[pos].estado  

--(v10) inicia

       CALL curp(l_record[pos].unico) RETURNING l_record[pos].sexo,
                                       l_record[pos].edo_naci
--(v10) finaliza


        IF pos = 200 THEN
            LET pos = pos + 1
            ERROR "  EL ARREGLO HA SIDO SOBREPASADO" ATTRIBUTE(NORMAL)
            EXIT FOREACH
        END IF
        LET pos = pos + 1
    END FOREACH

    LET cont_reg = pos-1  
                                                                         
    DISPLAY "REGISTROS CARGADOS ",cont_reg ," " AT 21,50 
    ATTRIBUTE(REVERSE) 

    IF (pos-1) >= 1 THEN
        CALL SET_COUNT(pos-1)
        DISPLAY ARRAY l_record TO scr_1.*
           ON KEY (CONTROL-M)
               LET pos = ARR_CURR()

               IF l_record[pos].status_interno = 0 OR       
                  l_record[pos].status_interno = 7 OR     
                  l_record[pos].status_interno = 10 THEN     

                  LET reg.nro_solicitud  =  l_record[pos].nro_solicitud
                  LET reg.codven         =  l_record[pos].codven
                  LET reg.seguro         =  l_record[pos].seguro
                  LET reg.rfc            =  l_record[pos].rfc
                  LET reg.unico          =  l_record[pos].unico
                  LET reg.paterno        =  l_record[pos].paterno
                  LET reg.materno        =  l_record[pos].materno
                  LET reg.nombres        =  l_record[pos].nombres
                  LET reg.diag_proceso   =  l_record[pos].diag_proceso
                  LET reg.fnaci          =  l_record[pos].fnaci
                  LET reg.fecha_baja     =  l_record[pos].fecha_baja
                  LET reg.fingre         =  l_record[pos].fingre
                  LET reg.fenvio         =  l_record[pos].fenvio
                  LET reg.fecha_registro =  l_record[pos].fecha_registro
                  LET reg.fecha_proceso  =  l_record[pos].fecha_proceso
                  LET reg.resuelva       =  l_record[pos].resuelva
                  LET reg.horas_capacit  =  l_record[pos].horas_capacit
                  LET reg.escolar        =  l_record[pos].escolar      
                  LET reg.sexo           =  l_record[pos].sexo     --(v10)      
                  LET reg.edo_naci       =  l_record[pos].edo_naci --(v10)     
--                  LET reg.tipo_recibo    =  l_record[pos].tipo_recibo  
                  LET reg.calle          =  l_record[pos].calle
                  LET reg.numero         =  l_record[pos].numero
                  LET reg.dpto           =  l_record[pos].dpto
                  LET reg.codpos         =  l_record[pos].codpos
                  LET reg.colonia        =  l_record[pos].colonia
                  LET reg.deleg          =  l_record[pos].deleg
                  LET reg.delegdesc      =  l_record[pos].delegdesc
                  LET reg.ciudad         =  l_record[pos].ciudad
                  LET reg.ciudaddesc     =  l_record[pos].ciudaddesc
                  LET reg.estado         =  l_record[pos].estado
                  LET reg.estadodesc     =  l_record[pos].estadodesc
                  LET reg.fono           =  l_record[pos].fono
                  EXIT DISPLAY
               ELSE
                   CALL pregunta4()                 

                   IF sn MATCHES "[Ss]" THEN        
                       LET salida = "S"             
                       EXIT DISPLAY
                   ELSE                             
                       LET salida = "N"             
                       EXIT DISPLAY
                   END IF                           
               END IF

           ON KEY (INTERRUPT)
               LET salida = "N"             
               EXIT DISPLAY
        END DISPLAY

        CASE salida
            WHEN "S"
                RETURN salida
            WHEN "N"
                RETURN salida
            OTHERWISE
                EXIT CASE
        END CASE
    ELSE
        ERROR "  REGISTROS DE PROMOTORES....NO EXISTE" ATTRIBUTE(NORMAL)
        SLEEP 1
        ERROR ""
        LET salida = "N"             
        RETURN salida                
    END IF

    DISPLAY BY NAME reg.nro_solicitud THRU reg.fono
    WHILE TRUE
       CALL pregunta()
       IF sn MATCHES "[SsNn]" THEN
          EXIT WHILE
       END IF
    END WHILE   

    IF sn MATCHES "[Ss]" THEN
        DELETE
        FROM   pro_solicitud 
        WHERE   status_interno IN (0,7,10)
        AND    nro_solicitud  = reg.nro_solicitud

        DELETE
        FROM   pro_consecutivo
        WHERE  consecutivo = reg.nro_solicitud                              

        ERROR "  REGISTRO ELIMINADO " ATTRIBUTE(NORMAL)
        SLEEP 2
        LET salida = "N"   
     ELSE
        ERROR "  PROCESO CANCELADO " ATTRIBUTE(NORMAL) 
        SLEEP 2
        LET salida = "N"
        ERROR " "
        CLEAR FORM
     END IF

    RETURN salida
END FUNCTION 

FUNCTION reactiva_1()
#r-----------------

    LET pos = 2
    IF (pos-1) >= 1 THEN
        CALL SET_COUNT(pos-1)

        LET HOY      = TODAY

        OPEN WINDOW pantalla4 AT 2,3 WITH FORM "PROM0032" ATTRIBUTE(BORDER)
        DISPLAY " PROM003      SOLICITUDES DE REGISTRO DE AGENTES ",
                "PROMOTORES                    " AT 2,1 ATTRIBUTE(REVERSE)
        DISPLAY "                     DOMICILIO   DE   CORRESPONDEN",
                "CIA                          " AT 14,1 ATTRIBUTE(REVERSE) 
        DISPLAY HOY USING "DD-MM-YYYY" AT 2,66 ATTRIBUTE(REVERSE)

        LET int_flag = FALSE
        DISPLAY " REACTIVAR " AT 1,66 ATTRIBUTE(NORMAL)
        DISPLAY "ESC: Consultar,  Ctrl-C: Salir  " AT 1,2

        WHILE TRUE
            CALL construc_reactiva() #cr
            RETURNING salida

            IF salida = "S" THEN
                CALL inicializa()
                CLEAR FORM
            ELSE
                LET salida = NULL
                CALL inicializa()
                EXIT WHILE
            END IF
        END WHILE
    END IF

    CLOSE WINDOW pantalla4
    CLEAR SCREEN
END FUNCTION

FUNCTION construc_reactiva()
#cr---------------------

    DEFINE 
        xxx        SMALLINT, 
        cont_reg              SMALLINT,
        st_int                SMALLINT,
        i                     SMALLINT

    DEFINE #loc #char
        x_fecha               CHAR(10) ,
        desciuda              CHAR(18) ,
        dessup                CHAR(18) ,
        desnivel              CHAR(18) ,
        desdeleg              CHAR(18) ,
        aaa                   CHAR(02) ,
        z_fecha               CHAR(10) ,
        mm                    CHAR(02) ,
        dd                    CHAR(02) ,
        l_motivo_suspende     CHAR(02) ,
        l_diag_proceso        CHAR(02)

    DEFINE #glo #date
        xx_fecha              DATE     ,
        j_fecha               DATE

    DEFINE reg1     RECORD
        folio                 INTEGER,
        agenc_cod             CHAR(10),
        status                SMALLINT,
        status_interno        SMALLINT,
        num_lote              INTEGER,
        nip                   SMALLINT,
        sup                   CHAR(10),
        nivel                 SMALLINT
    END RECORD

    DEFINE
        vrfc                  CHAR(13),
        vcodven               CHAR(10)
                                         
        CONSTRUCT cla_where ON   nro_solicitud,codven,cod_promotor,seguro,rfc,
                                 paterno,materno,nombres
                            FROM nro_solicitud,codven,cod_promotor,seguro,rfc,
                                 paterno,materno,nombres
            ON KEY (ESC)
                LET int_flag = FALSE
                EXIT CONSTRUCT
            ON KEY (control-c)
                LET int_flag = TRUE
                EXIT CONSTRUCT
         END CONSTRUCT

         IF int_flag = TRUE THEN
             LET int_flag = FALSE
             CLEAR SCREEN
             LET salida = "N"
             RETURN salida
         END IF

         LET sel_where = " SELECT A.status_interno,",
                         " E.desc_status_corta,",    
                         " A.nro_solicitud,",
                         " A.codven,",
                         " A.cod_promotor,",
                         " A.seguro,",
                         " A.rfc,",
                         " A.unico,",
                         " A.paterno,",
                         " A.materno,",
                         " A.nombres,",
                         " A.diag_proceso,",
                         " A.fnaci,",
                         " A.fecha_baja,",
                         " A.fingre,",
                         " A.fenvio,",
                         " A.fecha_registro,",
                         " A.fecha_proceso,",
                         " A.resuelva,",
                         " A.horas_capacit,",
                         " A.escolar  ,",
                         " ' '   ,",               --(v10)
                         " ' '   ,",               --(v10)
##                         " A.tipo_recibo,",
                         " A.calle,",
                         " A.numero,",
                         " A.dpto,",
                         " A.codpos,",
                         " A.colonia,",
                         " A.deleg,",
                         " B.deleg_desc,",
                         " A.ciudad,",
                         " C.ciudad_desc,",
                         " A.estado,",
                         " D.estad_desc,",
                         " A.fono ",
                         " FROM   pro_solicitud A,OUTER(tab_delegacion B, ",
                         " tab_ciudad C,tab_estado D,pro_status_interno E) ",
                         " WHERE  ",cla_where CLIPPED,
                         " AND    B.deleg_cod  = A.deleg ",
                         " AND    C.ciudad_cod   = A.ciudad ",
                         " AND    D.estad_cod  = A.estado ",
                         " AND    E.status_interno = A.status_interno ",
                         " ORDER BY 3,6 "

         PREPARE query4 FROM sel_where

         DECLARE cursor_6 CURSOR FOR query4

         LET pos = 1

         FOREACH cursor_6 INTO l_record[pos].*

             SELECT desc_status_corta
             INTO   l_record[pos].desc_status_corta
             FROM   pro_status_interno
             WHERE  status_interno = l_record[pos].status_interno

             SELECT A.deleg_desc
             INTO   l_record[pos].delegdesc
             FROM   tab_delegacion A
             WHERE  A.deleg_cod = l_record[pos].deleg

             SELECT A.ciudad_desc
             INTO   l_record[pos].ciudaddesc
             FROM   tab_ciudad A
             WHERE  A.ciudad_cod = l_record[pos].ciudad

             SELECT A.estad_desc
             INTO   l_record[pos].estadodesc
             FROM   tab_estado A
             WHERE  A.estad_cod = l_record[pos].estado                       
--(v10) inicia

             CALL curp(l_record[pos].unico) RETURNING l_record[pos].sexo,
                                                      l_record[pos].edo_naci
--(v10) finaliza


             IF pos = 200 THEN
                 LET pos = pos + 1
                 ERROR "  EL ARREGLO HA SIDO SOBREPASADO" ATTRIBUTE(NORMAL)
                 EXIT FOREACH
             END IF

             LET pos = pos + 1
         END FOREACH

         LET cont_reg = pos-1  
                                                                         
         DISPLAY "REGISTROS CARGADOS ",cont_reg ," " AT 21,50 
         ATTRIBUTE(REVERSE) 

         IF (pos-1) >= 1 THEN
             CALL SET_COUNT(pos-1)
             DISPLAY ARRAY l_record TO scr_1.*
                 ON KEY (CONTROL-M)
                     LET pos = ARR_CURR()

                     SELECT "OK"
                     FROM   pro_mae_promotor D
                     WHERE  D.cod_promotor = l_record[pos].cod_promotor

                     IF  STATUS = NOTFOUND  THEN
                         CALL pregunta6() -----Promotor inexistente en BD

                         IF sn MATCHES "[Ss]" THEN
                            LET salida = "S"
                            EXIT DISPLAY
                         ELSE
                               LET salida = "N"
                               EXIT DISPLAY
                         END IF
                     END IF

                     IF l_record[pos].status_interno = 7 THEN
                        PROMPT " PROMOTOR YA ESTA EN PROCESO DE ",
                               "REACTIVACION... DESEA REACTIVAR OTRO S/N ? "
                           ATTRIBUTE(REVERSE)
                    FOR sn
                           ATTRIBUTE(REVERSE)
                           IF sn MATCHES "[Ss]" THEN
                              LET salida = "S"
                               EXIT DISPLAY
                           ELSE
                                 LET salida = "N"
                                 EXIT DISPLAY
                           END IF
                     END IF

                     IF l_record[pos].status_interno = 8 THEN
                        PROMPT " PROMOTOR YA ENVIADO PARA REACTIVA",
                               "CION... DESEA REACTIVAR OTRO S/N ? "
                        ATTRIBUTE(REVERSE) FOR sn

                        IF sn MATCHES "[Ss]" THEN
                           LET salida = "S"
                           EXIT DISPLAY
                        ELSE
                           LET salida = "N"
                           EXIT DISPLAY
                        END IF
                     END IF

                     IF l_record[pos].status_interno = 5 
                     OR l_record[pos].status_interno = 62 THEN  --(v10)

--(v10) Inici 

                        LET    l_motivo_suspende      =     NULL
                        LET    l_diag_proceso         =     NULL

                        SELECT p.motivo_suspende      ,
                               p.diag_proceso
                        INTO   l_motivo_suspende      ,
                               l_diag_proceso
                        FROM   pro_mae_promotor p
                        WHERE  p.cod_promotor = l_record[pos].cod_promotor
                        AND    p.status  IN (2,3)

                        IF ( ( l_motivo_suspende  =  "3C" ) OR
                             ( l_motivo_suspende  =  "3D" ) OR
                             ( l_motivo_suspende  =  "3E" ) OR
                             ( l_motivo_suspende  =  "3T" ) )  THEN
                           IF ( l_diag_proceso = "7E" ) OR
                              ( l_diag_proceso = "7X" ) THEN
                           ELSE
                              CALL pregunta8() --el promotor no esta previamente desbloqueado
                              IF sn MATCHES "[Ss]" THEN
                                 LET salida = "S"
                                 EXIT DISPLAY
                              ELSE
                                 LET salida = "N"
                                 EXIT DISPLAY
                             END IF

                           END IF

                        END IF


--(v10) Finaliza 
                        SELECT "OK"
                        FROM   pro_mae_promotor D
                        WHERE  D.cod_promotor = l_record[pos].cod_promotor
                        AND    D.status  IN (2,3)
   
                        IF  STATUS <> NOTFOUND  THEN

 
                           LET reg.nro_solicitud  = l_record[pos].nro_solicitud
                           LET reg2.nro_solicitud = l_record[pos].nro_solicitud
                           LET reg.codven         = l_record[pos].codven
                           LET reg2.codven        = l_record[pos].codven
                           LET vcodven            = l_record[pos].codven
                           LET reg.seguro         = l_record[pos].seguro
                           LET reg2.seguro        = l_record[pos].seguro
                           LET reg.rfc            = l_record[pos].rfc
                           LET reg2.rfc           = l_record[pos].rfc
                           LET reg.unico          = l_record[pos].unico
                           LET reg2.unico         = l_record[pos].unico
                           LET reg.paterno        = l_record[pos].paterno
                           LET reg2.paterno       = l_record[pos].paterno
                           LET reg.materno        = l_record[pos].materno   
                           LET reg2.materno       = l_record[pos].materno   
                           LET reg.nombres        = l_record[pos].nombres
                           LET reg2.nombres       = l_record[pos].nombres
                           LET reg.diag_proceso   = l_record[pos].diag_proceso
                           LET reg2.diag_proceso  = l_record[pos].diag_proceso
                           LET reg.fnaci          = l_record[pos].fnaci
                           LET reg2.fnaci         = l_record[pos].fnaci
                           LET reg.fecha_baja     = l_record[pos].fecha_baja
                           LET reg2.fecha_baja    = l_record[pos].fecha_baja
                           LET reg.fingre         = l_record[pos].fingre
                           LET reg2.fingre        = l_record[pos].fingre
                           LET reg.fenvio         = l_record[pos].fenvio
                           LET reg2.fenvio        = l_record[pos].fenvio
                           LET reg.fecha_registro = l_record[pos].fecha_registro
                           LET reg2.fecha_registro= l_record[pos].fecha_registro
                           LET reg.fecha_proceso  = l_record[pos].fecha_proceso
                           LET reg2.fecha_proceso = l_record[pos].fecha_proceso
                           LET reg.resuelva       = l_record[pos].resuelva
                           LET reg2.resuelva      = l_record[pos].resuelva
                           LET reg.horas_capacit = l_record[pos].horas_capacit
                           LET reg2.horas_capacit = l_record[pos].horas_capacit
                           LET reg2.escolar       = l_record[pos].escolar      
                           LET reg.escolar       = l_record[pos].escolar      
                           LET reg.sexo           = l_record[pos].sexo      --(v10)
                           LET reg2.sexo          = l_record[pos].sexo      --(v10)
                           LET reg.edo_naci       = l_record[pos].edo_naci  --(v10)
                           LET reg2.edo_naci      = l_record[pos].edo_naci  --(v10)
##                           LET reg2.tipo_recibo   = l_record[pos].tipo_recibo  
                           LET reg.calle          = l_record[pos].calle
                           LET reg2.calle         = l_record[pos].calle
                           LET reg.numero         = l_record[pos].numero
                           LET reg2.numero        = l_record[pos].numero
                           LET reg.dpto           = l_record[pos].dpto
                           LET reg2.dpto          = l_record[pos].dpto
                           LET reg.codpos         = l_record[pos].codpos
                           LET reg2.codpos        = l_record[pos].codpos
                           LET reg.colonia        = l_record[pos].colonia
                           LET reg2.colonia       = l_record[pos].colonia
                           LET reg.deleg          = l_record[pos].deleg
                           LET reg2.deleg         = l_record[pos].deleg
                           LET reg.delegdesc      = l_record[pos].delegdesc
                           LET reg2.delegdesc     = l_record[pos].delegdesc
                           LET reg.ciudad         = l_record[pos].ciudad
                           LET reg2.ciudad        = l_record[pos].ciudad
                           LET reg.ciudaddesc     = l_record[pos].ciudaddesc
                           LET reg2.ciudaddesc    = l_record[pos].ciudaddesc
                           LET reg.estado         = l_record[pos].estado
                           LET reg2.estado        = l_record[pos].estado
                           LET reg.estadodesc     = l_record[pos].estadodesc
                           LET reg2.estadodesc    = l_record[pos].estadodesc
                           LET reg.fono           = l_record[pos].fono
                           LET reg2.fono          = l_record[pos].fono
                           LET reg.cod_promotor   = l_record[pos].cod_promotor
                           LET reg2.cod_promotor  = l_record[pos].cod_promotor
                           EXIT DISPLAY
                         ELSE
                            CALL pregunta5()   --el promotor esta activo
                            IF sn MATCHES "[Ss]" THEN
                               LET salida = "S"
                               EXIT DISPLAY
                            ELSE
                               LET salida = "N"
                               EXIT DISPLAY
                            END IF
                         END IF                                 
                     ELSE
                         CALL pregunta7() --Promotor inexistente
                         IF sn MATCHES "[Ss]" THEN
                             LET salida = "S"
                             EXIT DISPLAY
                         ELSE
                             LET salida = "N"
                             EXIT DISPLAY
                         END IF
                     END IF

                 ON KEY (CONTROL-C,INTERRUPT)
                     LET salida = "N"
                     EXIT DISPLAY
             END DISPLAY

             CASE salida
                 WHEN "S"
                     RETURN salida
                 WHEN "N"
                     RETURN salida
                 OTHERWISE
                     EXIT CASE
             END CASE
         ELSE
             ERROR "  REGISTROS DE PROMOTORES....NO EXISTE" ATTRIBUTE(NORMAL)
             SLEEP 3
             ERROR ""
             LET salida = "N"
             RETURN salida
         END IF

         SELECT B.folio,
                B.agenc_cod,
                B.status,
                B.status_interno,
                B.num_lote,
                B.nip,
                B.sup,
                B.nivel
         INTO   reg2.folio,
                reg2.agenc_cod,
                reg2.status, 
                reg2.status_interno, 
                reg2.num_lote,  
                reg2.nip,  
                reg2.sup,  
                reg2.nivel 
         FROM   pro_solicitud B
         WHERE  B.cod_promotor = reg.cod_promotor 

         SELECT D.status,
                D.motivo_suspende
         INTO   vstatus,
                vmotivo_suspende
         FROM   pro_mae_promotor D
         WHERE  D.seguro = reg.seguro
         AND    D.cod_promotor = reg.cod_promotor

         CASE vmotivo_suspende
            WHEN "2F"
               SELECT fecha_suspende + 2  UNITS MONTH
               INTO   vfecha_suspende
               FROM   pro_mae_promotor
               WHERE  seguro = reg.seguro
               AND    cod_promotor = reg.cod_promotor

               IF HOY >= vfecha_suspende THEN
                  EXIT CASE
               ELSE
                  PROMPT " NO SE PUEDE REACTIVAR POR MOTIVO 2F... ",
                         "DESEA REACTIVAR OTRO REGISTRO S/N ? " 
                  ATTRIBUTE(REVERSE)
                  FOR sn
                  ATTRIBUTE(REVERSE)

                  IF sn MATCHES "[Ss]" THEN
                     LET salida = "S"
                  ELSE
                     LET salida = "N"
                  END IF

                  CASE salida
                     WHEN "S"
                        RETURN salida
                     WHEN "N"
                        RETURN salida
                     OTHERWISE
                        EXIT CASE
                  END CASE
               END IF

            WHEN "6A"
               SELECT fecha_suspende + 6 UNITS MONTH --(v1)
               INTO   vfecha_suspende
               FROM   pro_mae_promotor
               WHERE  seguro = reg.seguro
               AND    cod_promotor = reg.cod_promotor

               IF HOY >= vfecha_suspende THEN
                  EXIT CASE
               ELSE
                  PROMPT " NO SE PUEDE REACTIVAR POR MOTIVO 6A... ",
                         "DESEA REACTIVAR OTRO REGISTRO S/N ? " 
                  ATTRIBUTE(REVERSE)
                  FOR sn
                  ATTRIBUTE(REVERSE)

                  IF sn MATCHES "[Ss]" THEN
                     LET salida = "S"
                  ELSE
                     LET salida = "N"
                  END IF

                  CASE salida
                     WHEN "S"
                        RETURN salida
                     WHEN "N"
                        RETURN salida
                     OTHERWISE
                        EXIT CASE
                  END CASE
               END IF

            WHEN "6B"
               SELECT (fecha_suspende) + 6 --(v1)
               INTO   vfecha_suspende
               FROM   pro_mae_promotor
               WHERE  seguro = reg.seguro
               AND    cod_promotor = reg.cod_promotor

               IF HOY >= vfecha_suspende THEN
                  EXIT CASE
               ELSE
                  PROMPT " NO SE PUEDE REACTIVAR POR MOTIVO 6B... ",
                         "DESEA REACTIVAR OTRO REGISTRO S/N ? "  
                                                                          
                  ATTRIBUTE(REVERSE)
                  FOR sn
                  ATTRIBUTE(REVERSE)

                  IF sn MATCHES "[Ss]" THEN
                     LET salida = "S"
                  ELSE
                     LET salida = "N"
                  END IF

                  CASE salida
                     WHEN "S"
                        RETURN salida
                     WHEN "N"
                        RETURN salida
                     OTHERWISE
                        EXIT CASE
                  END CASE
               END IF
         END CASE                                                                 
         DISPLAY " ESC: Grabar     Ctrl-C: Salir  ","" AT 1,1
         INPUT BY NAME reg.nro_solicitud THRU reg.fono  WITHOUT DEFAULTS
             BEFORE FIELD nro_solicitud
                      NEXT FIELD codven

             AFTER FIELD codven
                                                                               
                 IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                     NEXT FIELD fono
                 END IF

             AFTER FIELD cod_promotor
                 IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                     NEXT FIELD codven
                 END IF
                    IF reg.cod_promotor IS NULL THEN
                        LET reg.fecha_baja = "01/01/1900"
                        DISPLAY BY NAME reg.fecha_baja
                        NEXT FIELD seguro
                    END IF

             AFTER FIELD seguro
                 IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                     NEXT FIELD cod_promotor
                 END IF

                 LET sw_1 = 0
                 IF reg.seguro IS NULL OR reg.seguro = " " THEN
                     ERROR "  Debe ingresar RFC CON HOMOCLAVE " 
                           ATTRIBUTE(NORMAL)
                     NEXT FIELD rfc
                 END IF

                 LET sw_1 = 1
                 IF reg.seguro IS NOT NULL AND reg.seguro <> " " THEN
                     IF LENGTH(reg.seguro) <> 11 THEN
                         ERROR "  Debe ingresar N.S.S. compLETo"
                               ATTRIBUTE(NORMAL) 
                         NEXT FIELD seguro
                     END IF

                     CALL  digito_verif(reg.seguro[1,10],10)
                     RETURNING digito

                     IF digito = 32000 THEN
                         ERROR "  N.S.S. solo contiene digitos" 
                               ATTRIBUTE(NORMAL)
                         NEXT FIELD seguro
                     END IF

                     IF LENGTH(reg.seguro)=11 AND digito <> reg.seguro[11] THEN
                         ERROR "  Digito Verificador Invalido"
                               ATTRIBUTE(NORMAL)                                                 
                         NEXT FIELD seguro
                     END IF
                 END IF

             AFTER FIELD rfc
                 IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                     NEXT FIELD seguro
                 END IF

                 IF reg.rfc IS NULL  OR  reg.rfc = " " THEN
                     ERROR "  RFC NO puede ser NULO" ATTRIBUTE(NORMAL)
                     NEXT FIELD rfc
                 END IF

                 IF reg.rfc IS NOT NULL AND reg.rfc <> " " THEN
                     IF LENGTH(reg.rfc) < 10 THEN
                         ERROR "  Debe ingresar R.F.C. completo"
                               ATTRIBUTE(NORMAL) 
                         NEXT FIELD rfc
                     END IF

                     IF sw_1 = 0 THEN
                         IF LENGTH(reg.rfc) < 13 THEN
                             ERROR " "
                             ERROR "  DEBIO haber INGRESADO el RFC con ",
                                   "HOMOCLAVE" ATTRIBUTE(NORMAL)
                         END IF
                     END IF

                     IF NOT valida_fecha_rfc(reg.rfc[5,10]) THEN
                         ERROR "  El formato del RFC en la parte numerica ",
                               "  esta incorrecto" ATTRIBUTE(NORMAL)
                         NEXT FIELD rfc
                     END IF

                     LET aaa     = reg.rfc[5,6]
                     LET mm      = reg.rfc[7,8]
                     LET dd      = reg.rfc[9,10]
                     LET z_fecha = mm,"/",dd,"/19",aaa
                     LET j_fecha = z_fecha

                     IF j_fecha IS NULL THEN
                         ERROR "  Fecha Invalida en RFC" ATTRIBUTE(NORMAL)
                         NEXT FIELD rfc
                     END IF

                     IF reg.fnaci IS NULL OR reg.fnaci = " " THEN
                                                                               
                         LET reg.fnaci = z_fecha
                         DISPLAY BY NAME reg.fnaci
                     END IF
                 END IF

              AFTER FIELD unico
                  IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                      NEXT FIELD unico
                  END IF                                                       
                  IF reg.unico IS NOT NULL AND reg.unico <> " " THEN
                      IF LENGTH(reg.unico) < 18 THEN
                          ERROR "  Debe ingresar CURP completo" 
                                ATTRIBUTE(NORMAL)
                          NEXT FIELD unico
                      END IF
                  END IF

             AFTER FIELD paterno
                 IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                     NEXT FIELD rfc
                 END IF

                 IF reg.paterno  IS NULL THEN
                     ERROR "  El apellido paterno debe ser ingresado"
                           ATTRIBUTE(NORMAL)
                     NEXT FIELD paterno
                 END IF

             AFTER FIELD materno
                 IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                     NEXT FIELD paterno
                 END IF

                                                                               
                 IF reg.materno MATCHES  "."  THEN
                     ERROR "  EL APELLIDO MATERNO NO PUEDE SER UN PUNTO "
                           ATTRIBUTE(NORMAL)
                     NEXT FIELD materno
                 END IF

             AFTER FIELD nombres
                 IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                     NEXT FIELD materno
                 END IF

                 IF reg.nombres  IS NULL THEN
                     ERROR "  El nombre es requerido" ATTRIBUTE(NORMAL)
                     NEXT FIELD nombres                               
                 END IF

             BEFORE FIELD fingre
                 IF reg.fingre IS NULL THEN
                     LET reg.fingre = HOY
                     DISPLAY BY NAME reg.fingre
                 END IF
                                                                        
             AFTER FIELD fingre
                 IF reg.fingre IS NULL THEN
                     ERROR "  Se requiere la fecha del ",
                           "  llenado de la solicitud" ATTRIBUTE(NORMAL)
                     NEXT FIELD fingre
                 END IF

             AFTER FIELD resuelva
                 IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                     NEXT FIELD fingre
                 END IF

                 IF reg.resuelva  IS NULL  OR  reg.resuelva = " " THEN
                     ERROR "  Campo no puede ser nulo " ATTRIBUTE(NORMAL)
                     NEXT FIELD resuelva
                 END IF


           -- En el mpt con fecha 29072009 se especifica que la calificacion
           -- debera ser menor o igual a 100

               IF reg.resuelva < 000 OR reg.resuelva > 100 THEN    --(v10)
                  ERROR "ERROR...CALIFICACION DEBE SER MENOR O IGUAL A 100"--(v10)
                  NEXT FIELD resuelva
               END IF

             AFTER FIELD escolar
                IF reg.escolar IS NULL THEN
                    CALL grado_escolar()
                    RETURNING reg.escolar

                    IF reg.escolar IS NULL THEN
                        DISPLAY reg.escolar TO escolar
                        NEXT FIELD escolar
                    ELSE
                        DISPLAY reg.escolar TO escolar
--                        NEXT FIELD sexo    --(v10)
                        NEXT FIELD calle    --(v10)
                    END IF
                ELSE
                    IF reg.escolar MATCHES "[ABCDEFGHIJKL]" THEN
--                       NEXT FIELD sexo  --(v10)
                       NEXT FIELD calle  --(v10)
                    ELSE
                        ERROR "NO EXISTE EL GRADO DE ESCOLARIDAD"
                        NEXT FIELD escolar
                    END IF
                END IF


             AFTER FIELD calle
                 IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                     NEXT FIELD escolar
                 END IF

                 IF reg.calle IS NULL THEN
                     ERROR "  Dato no puede ser nulo reingrese ..."
                           ATTRIBUTE(NORMAL)
                     NEXT FIELD calle 
                 END IF

             AFTER FIELD numero
                 IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                     NEXT FIELD calle
                 END IF

                 IF reg.numero IS NULL THEN
                     ERROR "  Dato no puede ser nulo reingrese ..."
                           ATTRIBUTE(NORMAL)
                     NEXT FIELD numero                                        
                 END IF

             AFTER FIELD dpto
                 IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                     NEXT FIELD numero
                 END IF

             AFTER FIELD codpos
                 IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                     NEXT FIELD dpto
                 END IF

                 IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
                     NEXT FIELD fono
                 END IF

                 IF reg.codpos IS NULL THEN
                     CALL despliega_codigo_postal() #dcp
                     RETURNING reg.codpos,
                               reg.colonia,
                               reg.deleg,
                               desdeleg,
                               reg.ciudad,                                     
                               desciuda,
                               reg.estado,
                               desestad
                     IF reg.colonia IS NULL THEN
                         ERROR "  Este Codigo Postal no existe en el catalogo"
                               ATTRIBUTE(NORMAL) 
                         NEXT FIELD codpos
                     END IF
                 ELSE
                     SELECT "X"
                     FROM   tab_codpos
                     WHERE  cpos_cod = reg.codpos

                     IF STATUS = 100 THEN
                         ERROR "  Cod. Post. no existe en catalogo, pon",
                               "  valor NULO p/desplegar pantalla de Codigos"
                               ATTRIBUTE(NORMAL) 
                         NEXT FIELD codpos
                     END IF

                     CALL Despliega_colonias(reg.codpos)
                     RETURNING
                              reg.colonia,                            
                              reg.deleg,
                              desdeleg,
                              reg.ciudad,
                              desciuda,
                              reg.estado,
                              desestad
                 END IF

                 DISPLAY BY NAME reg.colonia,reg.deleg,reg.ciudad,reg.estado
                 DISPLAY desdeleg,desciuda,desestad TO
                         delegdesc,ciudaddesc,estadodesc

             AFTER FIELD fono
                 IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                     NEXT FIELD estado
                 END IF

            ON KEY( ESC )

                SELECT "X"                                           
                FROM tab_codpos
                WHERE cpos_cod = reg.codpos

                IF STATUS = 100 THEN
                    ERROR "  Cod. Post. no existe en catalogo, pon valor",
                          "  NULO p/desplegar pantalla de Codigos"
                          ATTRIBUTE(NORMAL)
                    NEXT FIELD codpos
                END IF

                IF reg.colonia  IS NULL THEN
                    ERROR "  Dato no puede ser nulo reingrese ..." 
                          ATTRIBUTE(NORMAL)
                    NEXT FIELD  codpos
                END IF

                IF reg.unico IS NOT NULL AND reg.unico <> " " THEN
                    IF LENGTH(reg.unico) < 18 THEN
                        ERROR "  Debe ingresar CURP completo" ATTRIBUTE(NORMAL)
                        NEXT FIELD unico
                    END IF
                END IF

                IF reg.resuelva IS NULL THEN
                    ERROR "  Dato no puede ser nulo reingrese"
                          ATTRIBUTE(NORMAL)
                    NEXT FIELD resuelva
                END IF

                IF reg.paterno  IS NULL THEN
                    ERROR "  Dato no puede ser nulo reingrese ..."
                          ATTRIBUTE(NORMAL)
                    NEXT FIELD paterno
                END IF

                IF reg.nombres  IS NULL THEN
                    ERROR "  Dato no puede ser nulo reingrese ..."
                          ATTRIBUTE(NORMAL)
                    NEXT FIELD nombres
                END IF

                IF reg.fingre IS NULL THEN
                    ERROR "  Dato no puede ser nulo reingrese ..."
                          ATTRIBUTE(NORMAL)
                    NEXT FIELD fingre
                END IF

                IF reg.fecha_baja > HOY THEN
                    ERROR "  FECHA NO PUEDE SER SUPERIOR A LA ACTUAL "
                          ATTRIBUTE(NORMAL) 
                    NEXT FIELD fecha_baja
                END IF

                IF reg.horas_capacit < 0 THEN
                    ERROR "  NUMERO DE HORAS DE CAPACITACION NO PUEDE",
                          "  SER MENOR A 0 " ATTRIBUTE(NORMAL)
                    NEXT FIELD horas_capacit
                END IF

                IF reg.escolar IS NULL THEN
                    CALL grado_escolar()
                    RETURNING reg.escolar

                    IF reg.escolar IS NULL THEN
                        DISPLAY reg.escolar TO escolar
                        NEXT FIELD escolar
                    ELSE
                        DISPLAY reg.escolar TO escolar
--                        NEXT FIELD sexo   --(v10)
                        NEXT FIELD  calle  --(v10)
                    END IF
                ELSE
                    IF reg.escolar NOT MATCHES "[ABCDEFGHIJKL]" THEN
                        ERROR "NO EXISTE EL GRADO DE ESCOLARIDAD"
                        NEXT FIELD escolar
                    END IF
                END IF

                IF reg.calle IS NULL THEN
                    ERROR "  Dato no puede ser nulo reingrese ..."
                          ATTRIBUTE(NORMAL)
                    NEXT FIELD calle
                                                                      
                END IF

                IF reg.numero    IS NULL THEN
                    ERROR "  Dato no puede ser nulo reingrese ..." 
                          ATTRIBUTE(NORMAL) 
                    NEXT FIELD numero
                END IF

                IF reg.colonia  IS NULL THEN
                    ERROR "  Dato no puede ser nulo reingrese ..."
                          ATTRIBUTE(NORMAL)
                    NEXT FIELD colonia
                END IF

                IF reg.codpos  IS NULL THEN
                    ERROR "  Dato no puede ser nulo reingrese ..."
                          ATTRIBUTE(NORMAL)
                    NEXT FIELD codpos
                END IF

                IF reg.deleg  IS NULL THEN
                    ERROR "  Dato no puede ser nulo reingrese ..."
                          ATTRIBUTE(NORMAL)
                    NEXT FIELD deleg
                END IF

                IF reg.ciudad   IS NULL THEN
                    ERROR "  Dato no puede ser nulo reingrese ..."
                          ATTRIBUTE(NORMAL)
                    NEXT FIELD ciudad
                END IF

                IF reg.estado   IS NULL THEN
                    ERROR "  Dato no puede ser nulo reingrese ..."
                          ATTRIBUTE(NORMAL)
                    NEXT FIELD estado
                END IF

                IF reg.codpos   IS NULL THEN
                    ERROR "  Dato no puede ser nulo reingrese ..."
                          ATTRIBUTE(NORMAL)
                    NEXT FIELD codpos
                END IF

                IF reg.fnaci  IS NULL THEN
                    ERROR "  Dato no puede ser nulo reingrese ..."
                    ATTRIBUTE(NORMAL)
                    NEXT FIELD fnaci
                END IF

                IF reg.rfc IS NULL THEN
                   ERROR "  El R.F.C. es requerido" ATTRIBUTE(NORMAL)
                   NEXT FIELD rfc
               ELSE
                  LET vrfc = reg.rfc[1,10]
                  CALL rfc_promotor(vrfc)
               END IF
                IF ventro = 1 THEN
                   WHILE TRUE                   
                   PROMPT "DESEA CONFIRMAR LA REACTIVACION  S/N?" FOR CHAR enter    
                   IF enter MATCHES "[SsNn]" THEN   #1
                      IF enter MATCHES "[Ss]" THEN   #2           
                         LET  HOY2 = CURRENT

                         -- Valida si el apellido materno es igual a nulo le agregara
                         -- de forma automatica N/A. --(v10)

                         IF reg.materno      = " "    OR
                            reg.materno      IS NULL  OR
                            reg.materno      = ""     THEN

                            LET reg.materno  = "N/A"

                         END IF

                         -- fin (v10)

                         INSERT INTO pro_his_reactiva 
                                VALUES ( reg2.folio,
                                         reg2.codven,  
                                         reg2.seguro,
                                         reg2.nip,
                                         reg2.agenc_cod,
                                         reg2.unico,
                                         reg2.rfc,
                                         reg2.paterno,
                                         reg2.materno,
                                         reg2.nombres,
                                         reg2.fecha_baja,
                                         reg2.fingre,
                                         reg2.fenvio, 
                                         reg2.calle, 
                                         reg2.numero, 
                                         reg2.dpto, 
                                         reg2.colonia,
                                         reg2.deleg, 
                                         reg2.ciudad, 
                                         reg2.estado, 
                                         reg2.codpos, 
                                         reg2.fono, 
                                         reg2.sup, 
                                         reg2.nivel, 
                                         reg2.resuelva,
                                         reg2.horas_capacit,
                                         reg2.fnaci,           
                                         reg2.diag_proceso,
                                         reg2.fecha_registro,
                                         reg2.status,
                                         reg2.nro_solicitud,
                                         reg2.status_interno,
                                         reg2.fecha_proceso,
                                         reg2.num_lote,
                                         reg2.cod_promotor,
                                         0, -- tipo_recibo
                                         reg2.escolar,
                                         HOY2,
                                         USER 
                                       )       

                   -- Valida si el apellido materno es igual a nulo le agregara
                   -- de forma automatica N/A. --(v10)

                   IF reg.materno      = " "    OR
                      reg.materno      IS NULL  OR
                      reg.materno      = ""     THEN

                      LET reg.materno  = "N/A"

                   END IF

                   -- fin (v10)


                         UPDATE pro_solicitud             
                         SET    codven        = reg.codven        ,
                                paterno       = reg.paterno       ,
                                materno       = reg.materno       ,
                                nombres       = reg.nombres       ,
                                fecha_baja    = reg.fecha_baja    ,
                                fnaci         = reg.fnaci         ,
                                unico         = reg.unico         ,
                                seguro        = reg.seguro        ,
                                rfc           = reg.rfc           ,
                                calle         = reg.calle         ,
                                numero        = reg.numero        ,
                                dpto          = reg.dpto          ,
                                colonia       = reg.colonia       ,
                                codpos        = reg.codpos        ,
                                deleg         = reg.deleg         ,
                                ciudad        = reg.ciudad        ,
                                estado        = reg.estado        ,
                                fono          = reg.fono          ,
                                resuelva      = reg.resuelva      ,
                                horas_capacit = reg.horas_capacit ,
                                status_interno= 7                 ,  
                                cod_promotor  = reg.cod_promotor  ,
                                escolar       = reg.escolar       
                         WHERE  nro_solicitud = reg.nro_solicitud              
                         AND    status_interno in (5,62)            --(v10)

                         ERROR "  REGISTRO REACTIVADO..." ATTRIBUTE(NORMAL)
                         SLEEP 2
                         EXIT WHILE
                      ELSE
                         ERROR "  REACTIVACION CANCELADA" ATTRIBUTE(NORMAL)
                         SLEEP 3
                         EXIT WHILE
                      END IF
                   END IF
                   END WHILE
                   EXIT INPUT  
              ELSE
                PROMPT "ESTA SEGURO DE REACTIVAR S/N ? " FOR CHAR sn
                IF sn MATCHES "[Ss]" THEN                                    
                   LET  HOY2 = CURRENT 

                   -- Valida si el apellido materno es igual a nulo le agregara
                   -- de forma automatica N/A. --(v10)

                   IF reg.materno      = " "    OR
                      reg.materno      IS NULL  OR
                      reg.materno      = ""     THEN

                      LET reg.materno  = "N/A"

                   END IF

                   -- fin (v10)

                   INSERT INTO pro_his_reactiva 
                          VALUES ( reg2.folio,
                                   reg2.codven,  
                                   reg2.seguro,
                                   reg2.nip,
                                   reg2.agenc_cod,
                                   reg2.unico,
                                   reg2.rfc,
                                   reg2.paterno,
                                   reg2.materno,
                                   reg2.nombres,
                                   reg2.fecha_baja,
                                   reg2.fingre,
                                   reg2.fenvio, 
                                   reg2.calle, 
                                   reg2.numero, 
                                   reg2.dpto, 
                                   reg2.colonia,
                                   reg2.deleg, 
                                   reg2.ciudad, 
                                   reg2.estado, 
                                   reg2.codpos, 
                                   reg2.fono, 
                                   reg2.sup, 
                                   reg2.nivel, 
                                   reg2.resuelva,
                                   reg2.horas_capacit,
                                   reg2.fnaci,           
                                   reg2.diag_proceso,
                                   reg2.fecha_registro,
                                   reg2.status,
                                   reg2.nro_solicitud,
                                   reg2.status_interno,
                                   reg2.fecha_proceso,
                                   reg2.num_lote,
                                   reg2.cod_promotor,
                                   0,  -- tipo_recibo
                                   reg2.escolar, 
                                   HOY2,
                                   USER
                                 )       

                    UPDATE pro_solicitud
                    SET    codven        = reg.codven        ,
                           paterno       = reg.paterno       ,
                           materno       = reg.materno       ,
                           nombres       = reg.nombres       ,
                           fecha_baja    = reg.fecha_baja    ,
                           fnaci         = reg.fnaci         ,
                           unico         = reg.unico         ,
                           seguro        = reg.seguro        ,
                           rfc           = reg.rfc           ,
                           calle         = reg.calle         ,
                           numero        = reg.numero        ,
                           dpto          = reg.dpto          ,
                           colonia       = reg.colonia       ,
                           codpos        = reg.codpos        ,
                           deleg         = reg.deleg         ,
                           ciudad        = reg.ciudad        ,
                           estado        = reg.estado        ,
                           fono          = reg.fono          ,
                           resuelva      = reg.resuelva      ,
                           horas_capacit = reg.horas_capacit ,         
                           status_interno= 7                 ,
                           cod_promotor  = reg.cod_promotor  ,
                           escolar       = reg.escolar       
                          WHERE  nro_solicitud = reg.nro_solicitud
                          AND    status_interno in (5,62)                     

                      ERROR "  REGISTRO REACTIVADO..." ATTRIBUTE(NORMAL)
                         SLEEP 2
                      ELSE
                         ERROR "  REACTIVACION CANCELADA" ATTRIBUTE(NORMAL)
                         SLEEP 3
                      END IF               
                ERROR ""

                LET salida = "N"
                EXIT INPUT

        END IF
            ON KEY (INTERRUPT)
                LET salida = "N"
                EXIT INPUT
            END INPUT

            RETURN salida
                                                                            
END FUNCTION  
#######################REVERSO ####################
FUNCTION reverso_react()

   LET pos = 2
   IF (pos-1) >= 1 THEN   #if_1
      CALL SET_COUNT(pos-1)
      LET HOY      = TODAY

      OPEN WINDOW pantalla5 AT 2,3 WITH FORM "PROM0032" ATTRIBUTE(BORDER)
      DISPLAY " PROM003      SOLICITUDES DE REGISTRO DE AGENTES ",
              "PROMOTORES                    " AT 2,1 ATTRIBUTE(REVERSE)
      DISPLAY "                     DOMICILIO   DE   CORRESPONDEN",
              "CIA                          " AT 14,1 ATTRIBUTE(REVERSE) 
      DISPLAY HOY USING "DD-MM-YYYY" AT 2,66 ATTRIBUTE(REVERSE)

      LET int_flag = FALSE 

      DISPLAY "ESC: Consultar    Ctrl-C: Salir    <ENTER> Reversar" AT 1,2
      DISPLAY " REVERSO REACTIVACION " AT 1,55 ATTRIBUTE(NORMAL)

      WHILE TRUE
         CALL construc_rev() RETURNING salida

         IF salida = "S" THEN
            CALL inicializa()
            CLEAR FORM
         ELSE
            LET salida = NULL
            CALL inicializa()
            EXIT WHILE
         END IF
      END WHILE
   END IF

   CLOSE WINDOW pantalla5
   CLEAR SCREEN
END FUNCTION                        

FUNCTION construc_rev() 
#cr--------------------
    DEFINE #loc #char
           x_busca               CHAR(050),
           txt_1                 CHAR(999)

    DEFINE ton INTEGER

    DEFINE #loc #smallint 
           arr_c                 ,   
    pos                   , 
           cont_reg              SMALLINT

    DEFINE vfecha_reactiva    DATETIME YEAR TO SECOND,
           vcod_promotor      LIKE pro_solicitud.cod_promotor

    DEFINE rev_react RECORD    LIKE pro_his_reactiva.*

    LET ton =0

    CONSTRUCT BY NAME x_busca ON codven,cod_promotor,paterno,materno,nombres
         ON KEY (ESC)
            LET int_flag = FALSE
            EXIT CONSTRUCT

         ON KEY (control-c)
            LET int_flag = TRUE
            EXIT CONSTRUCT
    END CONSTRUCT

      IF int_flag = TRUE THEN          
         LET int_flag = FALSE
         CLEAR SCREEN
         LET salida = "N"
         RETURN salida    
      END IF                       

      LET sel_where = " SELECT A.status_interno,",
                      " E.desc_status_corta,",
                      " A.nro_solicitud,",
                      " A.codven,",
                      " A.cod_promotor,",
                      " A.seguro,",
                      " A.rfc,",
                      " A.unico,",
                      " A.paterno,",
                      " A.materno,",
                      " A.nombres,",
                      " A.diag_proceso,",
                      " A.fnaci,",
                      " A.fecha_baja,",
                      " A.fingre,",
                      " A.fenvio,",
                      " A.fecha_registro,",
                      " A.fecha_proceso,",
                      " A.resuelva,",
                      " A.horas_capacit,",
                      " A.escolar,",
                      " ' ',",                --(v10)
                      " ' ',",                --(v10)
##                      " A.tipo_recibo,",
                      " A.calle,",
                      " A.numero,",                    
                      " A.dpto,",
                      " A.codpos,",
                      " A.colonia,",
                      " A.deleg,",
                      " B.deleg_desc,",
                      " A.ciudad,",
                      " C.ciudad_desc,",
                      " A.estado,",
                      " D.estad_desc,",
                      " A.fono ",
                      " FROM   pro_solicitud A,OUTER(tab_delegacion B, ",
                      " tab_ciudad C,tab_estado D,pro_status_interno E) ",
                      " WHERE  A.status_interno = 7",
                      " AND    B.deleg_cod  = A.deleg ",
                      " AND    C.ciudad_cod   = A.ciudad ",
                      " AND    D.estad_cod  = A.estado ",
                      " AND    E.status_interno = A.status_interno ",
                      " AND    ",x_busca  CLIPPED,
                      " ORDER BY 3,6 "                                      


      PREPARE query_rv FROM sel_where
      DECLARE cur_rev CURSOR FOR query_rv

      LET pos = 1

      FOREACH cur_rev INTO l_record[pos].*

         SELECT desc_status_corta
         INTO   l_record[pos].desc_status_corta
         FROM   pro_status_interno
         WHERE  status_interno = l_record[pos].status_interno

         SELECT A.deleg_desc
         INTO   l_record[pos].delegdesc
         FROM   tab_delegacion A
         WHERE  A.deleg_cod = l_record[pos].deleg

         SELECT A.ciudad_desc
         INTO   l_record[pos].ciudaddesc
         FROM   tab_ciudad A
         WHERE  A.ciudad_cod = l_record[pos].ciudad

         SELECT A.estad_desc
         INTO   l_record[pos].estadodesc
         FROM   tab_estado A
         WHERE  A.estad_cod = l_record[pos].estado

--(v10) inicia

         CALL curp(l_record[pos].unico) RETURNING l_record[pos].sexo,
                                                  l_record[pos].edo_naci
--isabel
--(v10) finaliza


         IF pos = 200 THEN
            LET pos = pos + 1
            ERROR "  EL ARREGLO HA SIDO SOBREPASADO" ATTRIBUTE(NORMAL)
            EXIT FOREACH
         END IF

         LET pos = pos + 1

      END FOREACH

      LET cont_reg = pos-1

      DISPLAY "REGISTROS CARGADOS ",cont_reg ," " AT 21,50
      ATTRIBUTE(REVERSE) 

      IF (pos-1) >= 1 THEN       
         CALL SET_COUNT(pos-1)
         DISPLAY ARRAY l_record TO scr_1.*
            ON KEY ( CONTROL-M)
               LET pos = ARR_CURR()
               LET vcod_promotor = l_record[pos].cod_promotor
               LET reg.nro_solicitud  =  l_record[pos].nro_solicitud
               LET reg.codven         =  l_record[pos].codven
               LET reg.seguro         =  l_record[pos].seguro
               LET reg.rfc            =  l_record[pos].rfc
               LET reg.unico          =  l_record[pos].unico
               LET reg.paterno        =  l_record[pos].paterno
               LET reg.materno        =  l_record[pos].materno
               LET reg.nombres        =  l_record[pos].nombres
               LET reg.diag_proceso   =  l_record[pos].diag_proceso
               LET reg.fnaci          =  l_record[pos].fnaci
               LET reg.fecha_baja     =  l_record[pos].fecha_baja
               LET reg.fingre         =  l_record[pos].fingre
               LET reg.fenvio         =  l_record[pos].fenvio
               LET reg.fecha_registro =  l_record[pos].fecha_registro
               LET reg.fecha_proceso  =  l_record[pos].fecha_proceso
               LET reg.resuelva       =  l_record[pos].resuelva
               LET reg.horas_capacit  =  l_record[pos].horas_capacit
               LET reg.escolar        =  l_record[pos].escolar      
               LET reg.sexo           =  l_record[pos].sexo      --(v10)      
               LET reg.edo_naci       =  l_record[pos].edo_naci  --(v10)      
##               LET reg.tipo_recibo    =  l_record[pos].tipo_recibo  
               LET reg.calle          =  l_record[pos].calle
               LET reg.numero         =  l_record[pos].numero
               LET reg.dpto           =  l_record[pos].dpto
               LET reg.codpos         =  l_record[pos].codpos
               LET reg.colonia        =  l_record[pos].colonia
               LET reg.deleg          =  l_record[pos].deleg
               LET reg.delegdesc      =  l_record[pos].delegdesc
               LET reg.ciudad         =  l_record[pos].ciudad
               LET reg.ciudaddesc     =  l_record[pos].ciudaddesc
               LET reg.estado         =  l_record[pos].estado
               LET reg.estadodesc     =  l_record[pos].estadodesc
               LET reg.fono           =  l_record[pos].fono
               EXIT DISPLAY
       
            ON KEY (INTERRUPT)
               LET salida = "N"
               EXIT DISPLAY
         END DISPLAY

         CASE salida
             WHEN "S"
                      RETURN salida
             WHEN "N"
                      RETURN salida      
             OTHERWISE
                      EXIT CASE
         END CASE                 
      ELSE
         ERROR "  REGISTROS DE PROMOTORES....NO EXISTE" ATTRIBUTE(NORMAL)
         SLEEP 1
         ERROR ""
         LET salida = "N"
         RETURN salida            
      END IF

      DISPLAY BY NAME reg.nro_solicitud THRU reg.fono     

      WHILE TRUE 
         PROMPT "ESTA SEGURO DE RESTAURAR  S/N ?" FOR CHAR enter
            IF enter MATCHES "[SsNn]" THEN
               EXIT WHILE
            END IF
      END WHILE

      IF enter MATCHES "[Ss]" THEN
         SELECT MAX(fecha_reactiva)
         INTO   vfecha_reactiva
         FROM   pro_his_reactiva
         WHERE  cod_promotor = l_record[pos].cod_promotor

         SELECT A.*                                                              --(v1)
          INTO  rev_react.*
          FROM  pro_his_reactiva A 
          WHERE A.fecha_reactiva = vfecha_reactiva
          AND   cod_promotor     = l_record[pos].cod_promotor

          UPDATE pro_solicitud             
             SET    folio         = rev_react.folio           , 
                    codven        = rev_react.codven          ,
                    seguro        = rev_react.seguro          ,
                    nip           = rev_react.nip             ,
                    agenc_cod     = rev_react.agenc_cod       ,
                    unico         = rev_react.unico           ,
                    rfc           = rev_react.rfc             ,
                    paterno       = rev_react.paterno         ,
                    materno       = rev_react.materno         ,
                    nombres       = rev_react.nombres         ,
                    fecha_baja    = rev_react.fecha_baja      ,
                    fingre        = rev_react.fingre          ,
                    fenvio        = rev_react.fenvio          ,
                    calle         = rev_react.calle           ,
                    numero        = rev_react.numero          ,
                    dpto          = rev_react.dpto            ,
                    colonia       = rev_react.colonia         ,
                    deleg         = rev_react.deleg           ,
                    ciudad        = rev_react.ciudad          , 
                    estado        = rev_react.estado          ,
                    codpos        = rev_react.codpos          ,
                    fono          = rev_react.fono            ,
                    sup           = rev_react.sup             ,
                    nivel         = rev_react.nivel           ,
                    resuelva      = rev_react.resuelva        ,
                    horas_capacit = rev_react.horas_capacit   ,
                    fnaci         = rev_react.fnaci           ,
                    diag_proceso  = rev_react.diag_proceso    ,
                    fecha_registro= rev_react.fecha_registro  ,
                    status        = rev_react.status          ,
                    nro_solicitud = rev_react.nro_solicitud   ,
                    status_interno= rev_react.status_interno  ,
                    fecha_proceso = rev_react.fecha_proceso   ,
                    num_lote      = rev_react.num_lote        ,
                    cod_promotor  = rev_react.cod_promotor    
              WHERE cod_promotor  = l_record[pos].cod_promotor
  
              DELETE
              FROM   pro_his_reactiva
              WHERE  fecha_reactiva  = vfecha_reactiva
              AND    cod_promotor    = l_record[pos].cod_promotor

              DISPLAY "REGISTRO RESTAURADO"
                  AT 21,2 ATTRIBUTE(REVERSE)
              SLEEP 3
              LET salida = "N"   
      ELSE
         ERROR "PROCESO DE RESTAURAR,CANCELADO"
         SLEEP 2
         LET salida = "N"
         ERROR " "
         INITIALIZE rev_react.* TO NULL
         CLEAR FORM
      END IF
   RETURN salida
END FUNCTION  
##############FIN REVERSO #################################

FUNCTION reenviar_rechazos()
#rr-------------------------
    DEFINE #loc #date
        xx_fecha                  ,
        j_fecha                          DATE

    DEFINE #loc #char
        desciuda                         CHAR(18) ,
        desdeleg                         CHAR(18) ,
        z_fecha                   CHAR(10) ,
        mm                               CHAR(02) ,
        dd                               CHAR(02) ,
        aaa                   CHAR(02)

    DEFINE #loc #smallint
        xxx                   ,
        cont_reg                         SMALLINT

    DEFINE reg1 RECORD #loc #reg1
        folio                            INTEGER  ,
        agenc_cod                        CHAR(10) ,
        status                           SMALLINT ,
        status_interno                   SMALLINT ,
        num_lote                         INTEGER
    END RECORD

    LET HOY      = TODAY
    OPEN WINDOW prom0034 AT 2,3 WITH FORM "PROM0034" ATTRIBUTE(BORDER)
    DISPLAY " PROM003      SOLICITUDES DE REGISTRO DE AGENTES ",
            "PROMOTORES                    " AT 2,1 ATTRIBUTE(REVERSE)
    DISPLAY "                     DOMICILIO   DE   CORRESPONDEN",
            "CIA                          " AT 14,1 ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING "DD-MM-YYYY" AT 2,66 ATTRIBUTE(REVERSE)

    DISPLAY " REENVIO-RECHAZOS " AT 1,60 ATTRIBUTE(REVERSE)
    DISPLAY "ESC: Consultar,  Ctrl-C: Salir  " AT 1,1

    WHILE TRUE
       CONSTRUCT cla_where ON   nro_solicitud,codven,cod_promotor,seguro,rfc,
                                paterno,materno,nombres
                           FROM nro_solicitud,codven,cod_promotor,seguro,rfc,
                                paterno,materno,nombres
           ON KEY (ESC)
               LET int_flag = FALSE
               EXIT CONSTRUCT

           ON KEY (control-c)
               LET int_flag = TRUE
               EXIT CONSTRUCT
       END CONSTRUCT

       IF int_flag = TRUE THEN
           LET int_flag = FALSE
           CLEAR SCREEN
           CLOSE WINDOW prom0034
           RETURN
       END IF

    #ff
       LET sel_where = " SELECT A.status_interno,",
                       " E.desc_status_corta,",    
                       " A.nro_solicitud,",
                       " A.codven,",
                       " A.cod_promotor,",
                       " A.seguro,",
                       " A.rfc,",
                       " A.unico,",
                       " A.paterno,",
                       " A.materno,",
                       " A.nombres,",
                       " A.diag_proceso,",
                       " A.fnaci,",
                       " A.fecha_baja,",
                       " A.fingre,",
                       " A.fenvio,",
                       " A.fecha_registro,",
                       " A.fecha_proceso,",
                       " A.resuelva,",
                       " A.horas_capacit,",
                       " A.escolar    ,",
                       " ' '    ,",             --(v10)
                       " ' '    ,",             --(v10)
##                       " A.tipo_recibo,",
                       " A.calle,",
                       " A.numero,",
                       " A.dpto,",
                       " A.codpos,",
                       " A.colonia,",
                       " A.deleg,",
                       " B.deleg_desc,",
                       " A.ciudad,",
                       " C.ciudad_desc,",
                       " A.estado,",
                       " D.estad_desc,",
                       " A.fono ",
                       " FROM   pro_solicitud A,OUTER(tab_delegacion B,",
                       " tab_ciudad C,tab_estado D,pro_status_interno E) ",
                       " WHERE  ",cla_where CLIPPED,
                       " AND    A.diag_proceso IN('1B','1C','1D',",
                       "'1E','1G','1H','1I','1J','1L','1Q','1S',",
                       "'9A','9B','9C','9D')",                --(v10)
                       " AND    B.deleg_cod  = A.deleg ",
                       " AND    C.ciudad_cod   = A.ciudad ",
                       " AND    D.estad_cod  = A.estado ",
                       " AND    E.status_interno = A.status_interno ",
                       " ORDER BY 3,6 "

    PREPARE pre_1 FROM sel_where
    DECLARE cursor_70 CURSOR FOR pre_1

    LET pos = 1

    FOREACH cursor_70 INTO l_record[pos].*

--(v10) inicia

       CALL curp(l_record[pos].unico) RETURNING l_record[pos].sexo,
                                                l_record[pos].edo_naci
--isabel
--(v10) finaliza

        IF pos = 200 THEN
            LET pos = pos + 1
            ERROR "  EL ARREGLO HA SIDO SOBREPASADO" ATTRIBUTE(NORMAL)
            EXIT FOREACH
        END IF
        LET pos = pos + 1
    END FOREACH

    LET cont_reg = pos-1  
                                                                         
    DISPLAY "REGISTROS CARGADOS ",cont_reg ," " AT 21,50 ATTRIBUTE(REVERSE) 

    IF (pos-1) >= 1 THEN
        CALL SET_COUNT(pos-1)
        LET sw_1 = 0

        DISPLAY ARRAY l_record TO scr_1.*
            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()
               LET reg.nro_solicitud  =  l_record[pos].nro_solicitud
               LET reg.codven         =  l_record[pos].codven
               LET reg.seguro         =  l_record[pos].seguro
               LET reg.rfc            =  l_record[pos].rfc
               LET reg.unico          =  l_record[pos].unico
               LET reg.paterno        =  l_record[pos].paterno
               LET reg.materno        =  l_record[pos].materno
               LET reg.nombres        =  l_record[pos].nombres
               LET reg.diag_proceso   =  l_record[pos].diag_proceso
               LET reg.fnaci          =  l_record[pos].fnaci
               LET reg.fecha_baja     =  l_record[pos].fecha_baja
               LET reg.fingre         =  l_record[pos].fingre
               LET reg.fenvio         =  l_record[pos].fenvio
               LET reg.fecha_registro =  l_record[pos].fecha_registro
               LET reg.fecha_proceso  =  l_record[pos].fecha_proceso
               LET reg.resuelva       =  l_record[pos].resuelva
               LET reg.horas_capacit  =  l_record[pos].horas_capacit
               LET reg.escolar        =  l_record[pos].escolar      
               LET reg.sexo           =  l_record[pos].sexo       -- (v10)      
               LET reg.edo_naci       =  l_record[pos].edo_naci   --(v10)      
##               LET reg.tipo_recibo    =  l_record[pos].tipo_recibo  
               LET reg.calle          =  l_record[pos].calle
               LET reg.numero         =  l_record[pos].numero
               LET reg.dpto           =  l_record[pos].dpto
               LET reg.codpos         =  l_record[pos].codpos
               LET reg.colonia        =  l_record[pos].colonia
               LET reg.deleg          =  l_record[pos].deleg
               LET reg.delegdesc      =  l_record[pos].delegdesc
               LET reg.ciudad         =  l_record[pos].ciudad
               LET reg.ciudaddesc     =  l_record[pos].ciudaddesc
               LET reg.estado         =  l_record[pos].estado
               LET reg.estadodesc     =  l_record[pos].estadodesc
               LET reg.fono           =  l_record[pos].fono
               LET reg.cod_promotor   =  l_record[pos].cod_promotor
               EXIT DISPLAY
      
            ON KEY (INTERRUPT)
                LET sw_1 = 1
                EXIT DISPLAY                
        END DISPLAY
    ELSE
        PROMPT "NO EXISTEN REGISTROS PARA CARGAR...<ENTER> PARA CONTINUAR "
        FOR CHAR enter
        LET sw_1 = 1
    END IF

    CALL actualiza_datos(reg.*) #ad

    CLEAR FORM
    END WHILE
END FUNCTION


FUNCTION actualiza_datos(reg)
#ad--------------------------
    DEFINE  reg  RECORD #loc #reg
        status_interno      LIKE pro_solicitud.status_interno,
        desc_status_corta   LIKE pro_status_interno.desc_status_corta,
        nro_solicitud       LIKE pro_solicitud.nro_solicitud,
        codven              LIKE pro_solicitud.codven,
        cod_promotor        LIKE pro_solicitud.cod_promotor,
        seguro              LIKE pro_solicitud.seguro,
        rfc                 LIKE pro_solicitud.rfc,
        unico               LIKE pro_solicitud.unico,
        paterno             LIKE pro_solicitud.paterno,
        materno             LIKE pro_solicitud.materno,
        nombres             LIKE pro_solicitud.nombres,
        diag_proceso        LIKE pro_solicitud.diag_proceso,
        fnaci               LIKE pro_solicitud.fnaci,
        fecha_baja          LIKE pro_solicitud.fecha_baja,
        fingre              LIKE pro_solicitud.fingre,
        fenvio              LIKE pro_solicitud.fenvio,
        fecha_registro      LIKE pro_solicitud.fecha_registro,
        fecha_proceso       LIKE pro_solicitud.fecha_proceso,
        resuelva            LIKE pro_solicitud.resuelva,
        horas_capacit       LIKE pro_solicitud.horas_capacit,
##        tipo_recibo         LIKE pro_solicitud.tipo_recibo   ,
        escolar             LIKE pro_solicitud.escolar       ,
        sexo                CHAR (1)              ,   --(v10)
        edo_naci            CHAR (2)              , --(v10)
        calle               LIKE pro_solicitud.calle,
        numero              LIKE pro_solicitud.numero,
        dpto                LIKE pro_solicitud.dpto,
        codpos              LIKE pro_solicitud.codpos,
        colonia             LIKE pro_solicitud.colonia,
        deleg               LIKE pro_solicitud.deleg,
        delegdesc           CHAR(20),
        ciudad              LIKE pro_solicitud.ciudad,
        ciudaddesc          CHAR(20),
        estado              LIKE pro_solicitud.estado,
        estadodesc          CHAR(20),
        fono                LIKE pro_solicitud.fono
    END RECORD,

    xx_fecha             ,
    j_fecha              DATE ,
    desciuda             CHAR(18) ,
    desdeleg             CHAR(18) ,
    z_fecha              CHAR(10) ,
    mm                   CHAR(02) ,
    dd                   CHAR(02) ,
    aaa                  CHAR(02)

    INPUT BY NAME reg.nro_solicitud THRU reg.fono  WITHOUT DEFAULTS
        BEFORE FIELD nro_solicitud
           NEXT FIELD codven

        AFTER FIELD codven   
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD codven
            END IF

            IF reg.codven IS NULL THEN
                LET reg.fecha_baja = "01/01/1900"
                DISPLAY BY NAME reg.fecha_baja
                NEXT FIELD cod_promotor
            END IF

        AFTER FIELD cod_promotor
            IF reg.cod_promotor IS NULL THEN
               LET reg.fecha_baja = "01/01/1900"
               DISPLAY BY NAME reg.fecha_baja
            END IF

            SELECT "OK"
            FROM   pro_solicitud
            WHERE  cod_promotor = reg.cod_promotor
            GROUP BY 1

            SELECT "X"
            FROM   pro_mae_promotor
            WHERE  cod_promotor = reg.cod_promotor
            AND    status       IN(1,4)

            IF STATUS <> NOTFOUND THEN
               ERROR "  CODIGO PROMOTOR YA EXISTE EN MAESTRO" ATTRIBUTE(NORMAL)
               NEXT FIELD cod_promotor
            END IF

         AFTER FIELD seguro
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD codven
            END IF

            LET sw_1 = 0
            IF reg.seguro IS NULL OR reg.seguro = " " THEN
                ERROR "  Debe ingresar RFC CON HOMOCLAVE " ATTRIBUTE(NORMAL)
                NEXT FIELD rfc
            END IF

            LET sw_1 = 1
            IF reg.seguro IS NOT NULL AND reg.seguro <> " " THEN
                IF LENGTH(reg.seguro) <> 11 THEN
                    ERROR "  Debe ingresar N.S.S. completo" ATTRIBUTE(NORMAL)
                    NEXT FIELD seguro
                END IF

                CALL  digito_verif(reg.seguro[1,10],10)
                       RETURNING digito

                IF digito = 32000 THEN
                    ERROR "  N.S.S. solo contiene digitos" ATTRIBUTE(NORMAL)
                    NEXT FIELD seguro
                END IF

                IF LENGTH(reg.seguro)=11 AND digito <> reg.seguro[11] THEN
                    ERROR "  Digito Verificador Invalido" ATTRIBUTE(NORMAL)
                    NEXT FIELD seguro
                END IF
            END IF                         

        AFTER FIELD rfc
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD seguro
            END IF

            IF reg.rfc IS NULL  OR  reg.rfc = " " THEN
                ERROR "  RFC NO puede ser NULO" ATTRIBUTE(NORMAL)
                   NEXT FIELD rfc
            END IF

            IF reg.rfc IS NOT NULL AND reg.rfc <> " " THEN
                IF LENGTH(reg.rfc) < 10 THEN
                    ERROR "  Debe ingresar R.F.C. completo" ATTRIBUTE(NORMAL)
                       NEXT FIELD rfc
                END IF

                IF sw_1 = 0 THEN
                    IF LENGTH(reg.rfc) < 13 THEN
                        ERROR " "
                        ERROR "  DEBIO haber INGRESADO el RFC con HOMOCLAVE"
                              ATTRIBUTE(NORMAL)
                    END IF
                END IF

                IF NOT valida_fecha_rfc(reg.rfc[5,10]) THEN
                    ERROR "  El formato del RFC en la parte numerica ",
                          "  esta incorrecto" ATTRIBUTE(NORMAL)
                    NEXT FIELD rfc
                END IF

                LET aaa     = reg.rfc[5,6]
                LET mm      = reg.rfc[7,8]
                LET dd      = reg.rfc[9,10]
                LET z_fecha = mm,"/",dd,"/19",aaa
                LET j_fecha = z_fecha

                IF j_fecha IS NULL THEN
                    ERROR "  Fecha Invalida en RFC" ATTRIBUTE(NORMAL)
                    NEXT FIELD rfc
                END IF

                IF reg.fnaci IS NULL OR reg.fnaci = " " THEN
                    LET reg.fnaci = z_fecha
                    DISPLAY BY NAME reg.fnaci
                END IF
            END IF

        AFTER FIELD paterno   
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD rfc
            END IF

            IF reg.paterno  IS NULL THEN
                ERROR "  El apellido paterno debe ser ingresado"
                      ATTRIBUTE(NORMAL)
                NEXT FIELD paterno  
            END IF

        AFTER FIELD materno   
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD paterno
            END IF

--(v10)            IF reg.materno  IS NULL THEN
--(v10)                ERROR "  El apellido materno debe ser ingresado" 
--(v10)                             ATTRIBUTE(NORMAL)
--(v10)                NEXT FIELD materno  
--(v10)            END IF

        AFTER FIELD nombres      
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD materno
            END IF

            IF reg.nombres  IS NULL THEN
                ERROR "  El nombre es requerido" ATTRIBUTE(NORMAL)
                NEXT FIELD nombres  
            END IF

        AFTER FIELD fnaci
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD nombres
            END IF

            IF reg.fnaci IS NOT NULL THEN
                LET xx_fecha = reg.fnaci
                IF xx_fecha IS NULL THEN
                    ERROR "  Valor invalido en la fecha de nacimiento" 
                          ATTRIBUTE(NORMAL)
                    NEXT FIELD fnaci
                END IF
            END IF

        BEFORE FIELD fingre     
            IF reg.fingre IS NULL THEN
                LET reg.fingre = HOY
                DISPLAY BY NAME reg.fingre
            END IF

        AFTER FIELD fingre     
            IF reg.fingre IS NULL THEN
                ERROR "  Se requiere la fecha del llenado de la solicitud"
                             ATTRIBUTE(NORMAL)
                NEXT FIELD fingre   
            END IF

        AFTER FIELD resuelva        
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD fingre
            END IF

            IF reg.resuelva  IS NULL  OR  reg.resuelva = " " THEN
                ERROR "  Campo no puede ser nulo " ATTRIBUTE(NORMAL)  
                NEXT FIELD resuelva    
            END IF

        AFTER FIELD escolar     
                IF reg.escolar IS NULL THEN
                    CALL grado_escolar()
                    RETURNING reg.escolar

                    IF reg.escolar IS NULL THEN
                        DISPLAY reg.escolar TO escolar
                        NEXT FIELD escolar
                    ELSE
                        DISPLAY reg.escolar TO escolar
--                        NEXT FIELD sexo    -- (v10)
                        NEXT FIELD calle     -- (v10)
                    END IF
                ELSE
                    IF reg.escolar MATCHES "[ABCDEFGHIJKL]" THEN
--                       NEXT FIELD sexo  --(v10)
                       NEXT FIELD calle  --(v10)
                    ELSE
                        ERROR "NO EXISTE EL GRADO DE ESCOLARIDAD"
                        NEXT FIELD escolar
                    END IF
                END IF

        AFTER FIELD calle    
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD tipo_recibo
##                NEXT FIELD horas_capacit
            END IF

            IF reg.calle IS NULL THEN
                ERROR "  Dato no puede ser nulo reingrese ..." ATTRIBUTE(NORMAL)
                NEXT FIELD calle   
         END IF

        AFTER FIELD numero    
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD calle
            END IF

            IF reg.numero IS NULL THEN
                ERROR "  Dato no puede ser nulo reingrese ..." ATTRIBUTE(NORMAL)
                NEXT FIELD numero
         END IF

        AFTER FIELD dpto    
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD numero
            END IF

        AFTER FIELD codpos     
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD dpto
            END IF

            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
                NEXT FIELD fono
            END IF

            IF reg.codpos IS NULL THEN
                CALL despliega_codigo_postal() #dcp
                RETURNING reg.codpos,   
                          reg.colonia,   
                          reg.deleg, 
                          desdeleg, 
                          reg.ciudad, 
                          desciuda, 
                          reg.estado,
                          desestad

                IF reg.colonia IS NULL THEN
                    ERROR "  Este Codigo Postal no existe en el catalogo"
                          ATTRIBUTE(NORMAL)
                    NEXT FIELD codpos
                END IF
            ELSE
                SELECT "X" 
                FROM   tab_codpos
                WHERE  cpos_cod = reg.codpos

                IF STATUS = 100 THEN
                    ERROR "  Cod. Post. no existe en catalogo, pon",
                          "  valor NULO p/desplegar pantalla de Codigos"
                          ATTRIBUTE(NORMAL)
                    NEXT FIELD codpos
                END IF

                CALL Despliega_colonias(reg.codpos)
                RETURNING reg.colonia, 
                          reg.deleg, 
                          desdeleg, 
                          reg.ciudad, 
                          desciuda, 
                          reg.estado,
                          desestad
         END IF

        DISPLAY BY NAME reg.colonia,reg.deleg,reg.ciudad,reg.estado
        DISPLAY desdeleg,desciuda,desestad TO 
                delegdesc,ciudaddesc,estadodesc

        AFTER FIELD fono
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD estado
            END IF
    
        ON KEY( ESC )
            SELECT "X" 
            FROM tab_codpos
            WHERE cpos_cod = reg.codpos

            IF STATUS = 100 THEN
                ERROR "  Cod. Post. no existe en catalogo, pon valor",
                      "  NULO p/desplegar pantalla de Codigos" ATTRIBUTE(NORMAL)
                NEXT FIELD codpos
            END IF

            IF reg.colonia  IS NULL THEN
                ERROR "  Dato no puede ser nulo reingrese ..." ATTRIBUTE(NORMAL)
                NEXT FIELD  codpos
         END IF

         IF reg.rfc IS NULL THEN
             ERROR "  El R.F.C. es requerido" ATTRIBUTE(NORMAL)
             NEXT FIELD rfc
         END IF
  
         IF reg.resuelva IS NULL THEN
             ERROR "  Dato no puede ser nulo reingrese" ATTRIBUTE(NORMAL) 
             NEXT FIELD resuelva
         END IF

         IF reg.paterno  IS NULL THEN
                ERROR "  Dato no puede ser nulo reingrese ..." ATTRIBUTE(NORMAL)
                NEXT FIELD paterno  
         END IF

         IF reg.nombres  IS NULL THEN
                ERROR "  Dato no puede ser nulo reingrese ..." ATTRIBUTE(NORMAL)
                NEXT FIELD nombres  
         END IF

         IF reg.fingre IS NULL THEN
                ERROR "  Dato no puede ser nulo reingrese ..." ATTRIBUTE(NORMAL)
                NEXT FIELD fingre   
         END IF

         IF reg.fecha_baja > HOY THEN
                ERROR "  FECHA NO PUEDE SER SUPERIOR A LA ACTUAL"
                      ATTRIBUTE(NORMAL)
                NEXT FIELD fecha_baja
         END IF

         IF reg.horas_capacit < 0 THEN
                ERROR "  NUMERO DE HORAS DE CAPACITACION NO PUEDE",
                      "  SER MENOR A 0 " ATTRIBUTE(NORMAL)
                NEXT FIELD horas_capacit  
         END IF

                IF reg.escolar IS NULL THEN
                    CALL grado_escolar()
                    RETURNING reg.escolar

                    IF reg.escolar IS NULL THEN
                        DISPLAY reg.escolar TO escolar
                        NEXT FIELD escolar
                    ELSE
                        DISPLAY reg.escolar TO escolar
--                        NEXT FIELD sexo  --- (v10)
                        NEXT FIELD calle  --- (v10)
                    END IF
                ELSE
                    IF reg.escolar NOT MATCHES "[ABCDEFGHIJKL]" THEN
                        ERROR "NO EXISTE EL GRADO DE ESCOLARIDAD"
                        NEXT FIELD escolar
                    END IF
                END IF


         IF reg.calle IS NULL THEN
                ERROR "  Dato no puede ser nulo reingrese ..." ATTRIBUTE(NORMAL)
                NEXT FIELD calle   
         END IF

         IF reg.numero    IS NULL THEN
                ERROR "  Dato no puede ser nulo reingrese ..." ATTRIBUTE(NORMAL)
                NEXT FIELD numero
         END IF

         IF reg.colonia  IS NULL THEN
                ERROR "  Dato no puede ser nulo reingrese ..." ATTRIBUTE(NORMAL)
                NEXT FIELD colonia  
         END IF

         IF reg.codpos  IS NULL THEN
                ERROR "  Dato no puede ser nulo reingrese ..." ATTRIBUTE(NORMAL)
                NEXT FIELD codpos    
         END IF

         IF reg.deleg  IS NULL THEN
                ERROR "  Dato no puede ser nulo reingrese ..." ATTRIBUTE(NORMAL)
                NEXT FIELD deleg    
         END IF

         IF reg.ciudad   IS NULL THEN
                ERROR "  Dato no puede ser nulo reingrese ..." ATTRIBUTE(NORMAL)
                NEXT FIELD ciudad  
         END IF

         IF reg.estado   IS NULL THEN
                ERROR "  Dato no puede ser nulo reingrese ..." ATTRIBUTE(NORMAL)
                NEXT FIELD estado  
         END IF

         IF reg.codpos   IS NULL THEN
                ERROR "  Dato no puede ser nulo reingrese ..." ATTRIBUTE(NORMAL)
                NEXT FIELD codpos  
         END IF

         IF reg.fnaci  IS NULL THEN
                ERROR "  Dato no puede ser nulo reingrese ..." ATTRIBUTE(NORMAL)
                NEXT FIELD fnaci    
         END IF
 
         WHILE TRUE
                PROMPT " ESTA SEGURO DE QUERER REENVIAR LA SOLICITUD  S/N "
                FOR CHAR enter

                IF enter MATCHES "[Ss]" THEN

                    UPDATE pro_solicitud 
                    SET    folio          = ""                ,
                           codven         = reg.codven        ,
                           cod_promotor   = reg.cod_promotor  ,
                           seguro         = reg.seguro        ,
                           unico          = reg.unico         ,
                           rfc            = reg.rfc           ,
                           paterno        = reg.paterno       ,
                           materno        = reg.materno       ,
                           nombres        = reg.nombres       ,
                           fecha_baja     = reg.fecha_baja    ,
                           fingre         = reg.fingre        ,
                           fenvio         = ""                ,
                           calle          = reg.calle         ,
                           numero         = reg.numero        ,
                           dpto           = reg.dpto          ,
                           colonia        = reg.colonia       ,
                           deleg          = reg.deleg         ,
                           ciudad         = reg.ciudad        ,
                           estado         = reg.estado        ,
                           codpos         = reg.codpos        ,
                           fono           = reg.fono          ,
                           resuelva       = reg.resuelva      ,
                           horas_capacit  = reg.horas_capacit ,
                           escolar        = reg.escolar       ,
##                           tipo_recibo    = reg.tipo_recibo   ,
                           fnaci          = reg.fnaci         ,
                           diag_proceso   = ""                ,
                           fecha_registro = ""                ,
                           status         = 0                 ,
                           status_interno = 20                ,
                           num_lote       = ""
                    WHERE  nro_solicitud  = reg.nro_solicitud
                    AND    diag_proceso IN('1B','1C','1D','1E','1G','1H','1I','1J','1L','1Q','1S','9A', '9B', '9C', '9D')   --(v10)
                    ERROR "  REGISTRO MODIFICADO..."  ATTRIBUTE(NORMAL)
                    SLEEP 2
                    EXIT INPUT
                ELSE
                    EXIT INPUT
         END IF
            END WHILE
            RETURN

        ON KEY (INTERRUPT)
     EXIT INPUT 
    END INPUT 

END FUNCTION


FUNCTION despliega()
#d-----------------
    DEFINE
        desciuda             CHAR(18) ,
        dessup               CHAR(18) ,
        desnivel             CHAR(18) ,
        desdeleg             CHAR(18) 
   
        SELECT A.deleg_desc  
        INTO   desdeleg
        FROM   tab_delegacion A
        WHERE  A.deleg_cod = reg.deleg  

        DISPLAY reg.deleg TO deleg 
        DISPLAY desdeleg  TO des_deleg    

        SELECT A.ciudad_desc
        INTO   desciuda
        FROM   tab_ciudad A
        WHERE  A.ciudad_cod = reg.ciudad  

        DISPLAY reg.ciudad TO ciudad
        DISPLAY desciuda   TO des_ciuda  

 SELECT A.estad_desc
        INTO   desestad
        FROM   tab_estado A
        WHERE  A.estad_cod = reg.estado  

        DISPLAY reg.estado  TO estado
        DISPLAY desestad    TO des_estad 
END FUNCTION

FUNCTION pregunta()
    PROMPT "ESTA SEGURO DE ELIMINAR S/N ? " FOR CHAR sn
END FUNCTION

FUNCTION pregunta1()
    PROMPT "ESTA SEGURO DE MODIFICAR S/N ? " FOR CHAR sn
END FUNCTION

FUNCTION pregunta2()
    PROMPT " Desea reactivar el promotor S/N ? " FOR sn
END FUNCTION

FUNCTION pregunta3()
    PROMPT " NO SE PUEDE MODIFICAR... DESEA MODIFICAR OTRO REGISTRO S/N ? "
    ATTRIBUTE(REVERSE)
    FOR sn
    ATTRIBUTE(REVERSE)
END FUNCTION

FUNCTION pregunta4()
    PROMPT " NO SE PUEDE ELIMINAR... DESEA ELIMINAR OTRO REGISTRO S/N ? "
    ATTRIBUTE(REVERSE)
    FOR sn
    ATTRIBUTE(REVERSE)
END FUNCTION

FUNCTION pregunta5()
#p5-----------------
    PROMPT " EL PROMOTOR ESTA ACTIVO... DESEA REACTIVAR OTRO REGISTRO S/N ? "
    ATTRIBUTE(REVERSE)
    FOR sn 
    ATTRIBUTE(REVERSE)
END FUNCTION

FUNCTION pregunta6()
#p6-----------------
    PROMPT " PROMOTOR INEXISTENTE EN EL MAESTRO... DESEA REACTIVAR OTRO REGISTRO S/N ? "    
    ATTRIBUTE(REVERSE)
    FOR sn
    ATTRIBUTE(REVERSE)
END FUNCTION    


FUNCTION pregunta7()
#p6-----------------
    PROMPT " PROMOTOR INEXISTENTE EN EL MAESTRO... DESEA REACTIVAR OTRO REGISTRO S/N ? "    
    ATTRIBUTE(REVERSE)
    FOR sn
    ATTRIBUTE(REVERSE)
END FUNCTION    

FUNCTION pregunta8()
#p8-----------------
    PROMPT "NO ESTA DESBLOQUEADO POR 7E o 7X... DESEA REACTIVAR OTRO REGISTRO S/N ? "    
    ATTRIBUTE(REVERSE)
    FOR sn
    ATTRIBUTE(REVERSE)
END FUNCTION    

FUNCTION rfc_promotor(f_vrfc)
#rp-------------------------

DEFINE f_vrfc CHAR(010)
DEFINE pos  smallint
DEFINE arr_rfc ARRAY[100] OF RECORD #glo #arr_rfc 
            cod_promotor   LIKE    pro_solicitud.cod_promotor            
       END RECORD

       DECLARE cur_rfc CURSOR FOR
               SELECT  unique  cod_promotor 
               FROM    pro_solicitud
               WHERE   rfc[1,10] = f_vrfc
  
               LET pos = 1
       FOREACH cur_rfc INTO arr_rfc[pos].*
               LET pos = pos + 1
       END FOREACH

       IF (pos-1) >= 1 THEN
          CALL SET_COUNT(pos-1)
          LET ventro = 1
          OPEN WINDOW vent_rfc AT 9,19 WITH FORM "PROM0033" ATTRIBUTE( BORDER) 
          DISPLAY "             <Ctrl-c> Salir                     " 
                  AT 1,1 ATTRIBUTE(REVERSE)  

          DISPLAY ARRAY arr_rfc TO scr_1.*               
                    ON KEY (INTERRUPT)
                       EXIT DISPLAY
                    ON KEY (control-c)
                       EXIT DISPLAY
          END DISPLAY
          CLOSE WINDOW vent_rfc 
       END IF 

END FUNCTION

FUNCTION confirma_recep()
#cor---------------------
    LET pos = 2
    IF (pos-1) >= 1 THEN
        CALL SET_COUNT(pos-1)

        LET HOY      = TODAY

   OPEN WINDOW pantalla6 AT 2,3 WITH FORM "PROM0035" ATTRIBUTE(BORDER)
   DISPLAY " PROM003               DATOS RECIBIDOS DE CONSAR                            " AT 2,1 ATTRIBUTE(REVERSE)
   DISPLAY "                       DATOS CAPTURADOS EN SAFRE                            " AT 09,1 ATTRIBUTE(REVERSE) 
   DISPLAY HOY USING "DD-MM-YYYY" AT 2,66 ATTRIBUTE(REVERSE)

   LET int_flag = FALSE

   DISPLAY " CONFIRMA " AT 1,67 ATTRIBUTE(NORMAL)
   DISPLAY "ESC: Consultar       Ctrl-C: Salir       ",
           "Ctrl-B Aceptar registro " AT 1,2

        WHILE TRUE
            CALL construccion2() #c2
            RETURNING salida

            IF salida = "S" THEN
                CLEAR FORM
            ELSE
                LET salida = NULL
                EXIT WHILE
            END IF
        END WHILE
    END IF

    CLOSE WINDOW pantalla6
    CLEAR SCREEN
END FUNCTION

FUNCTION construccion2()
   DEFINE #loc #char
   hace_el_input         CHAR(300) ,
   vrfc_10               CHAR(10)  ,
   vrfc_13               CHAR(13)  ,
   vrfc_letras           CHAR(4)   ,  
   vdiag_proceso         CHAR(2)   ,
   marca_nombre          CHAR(1)   ,
   marca_paterno         CHAR(1)   ,
   marca_materno         CHAR(1)   ,
   marca_rfc             CHAR(1)    

   DEFINE #loc #integer
   vrfc_numeros          INTEGER

   DEFINE #loc #smallint
           s_tot_registros       ,
    arr_c                 ,
    scr_l                 ,
    cuenta                ,        
    pos                   SMALLINT

   DEFINE #loc #date     
   vfecha_registro        ,
   vfecha_proceso         DATE

   DEFINE reg_difer  RECORD     #registros_diferentes
          folio                 INTEGER,
   fecha_carga           DATE,
   fecha_registro        DATE,
   paterno               CHAR(40),
   materno               CHAR(40),
   nombre                CHAR(40),
   cod_promotor          CHAR(10),
   rfc_letras            CHAR(4),
   rfc_numeros           INTEGER,
   rfc_homonimia         CHAR(3),
   diag_proceso          CHAR(2),
   fecha_proceso         DATE
   END RECORD

   DEFINE reg_cap RECORD        #registros_capturados
                 status_interno        SMALLINT,
                 nro_solicitud         INTEGER,
                 paterno               CHAR(40),
                 materno               CHAR(40),
                 nombres               CHAR(40),
                 rfc                   CHAR(13),
                 cod_promotor          CHAR(10),
                 fecha_baja            DATE,
                 fingre                DATE,
                 fenvio                DATE
          END RECORD

   DEFINE reg_tot ARRAY[1000] OF RECORD  #reg_total
                 folio                 INTEGER,
                 fecha_carga           DATE,
                 fecha_registro        DATE,
                 paterno               CHAR(40),
                 materno               CHAR(40),
                 nombre                CHAR(40),
                 cod_promotor          CHAR(10),
                 rfc                   CHAR(13),
                 status_interno        SMALLINT,
                 desc_status_corta     CHAR(25),
                 nro_solicitud         INTEGER,
                 marca_paterno         CHAR(1),
                 marca_materno         CHAR(1),
                 marca_nombre          CHAR(1),
                 marca_rfc             CHAR(1),
                 paterno_2             CHAR(40),
                 materno_2             CHAR(40),
                 nombre_2              CHAR(40),
                 rfc_2                 CHAR(13),
                 cod_promotor_2        CHAR(10),
                 fecha_baja            DATE,
                 fingre                DATE,
                 fenvio                DATE
          END RECORD,
          max_row                      INTEGER

   LET sw_1       = 0
   LET sel_where  = NULL

   INITIALIZE reg_difer.* TO NULL
   INITIALIZE reg_cap.* TO NULL

   CONSTRUCT hace_el_input ON A.folio          ,
                              B.fecha_carga    
                         FROM folio            ,
                              fecha_carga      
      ON KEY (ESC)
         LET int_flag = FALSE
         EXIT CONSTRUCT
      ON KEY (control-c)
         LET int_flag = TRUE
         EXIT CONSTRUCT
   END CONSTRUCT

   IF int_flag = TRUE THEN
      LET int_flag = FALSE
      CLEAR SCREEN
      LET salida = "N"
      RETURN salida
   END IF

   LET sel_where = " SELECT A.folio    ,",
                   " B.fecha_carga     ,",
                   " A.fecha_registro  ,",    
                   " A.paterno         ,",
                   " A.materno         ,",
                   " A.nombre          ,",
                   " A.cod_promotor    ,",
                   " A.rfc_letras      ,",
                   " A.rfc_numeros     ,",   
                   " A.rfc_homonimia   ,",    
                   " A.diag_proceso    ,",    
                   " A.fecha_proceso    ",    
                   " FROM   pro_det_agte A, pro_cza_agte B ",
                   " WHERE ",hace_el_input CLIPPED,
                   " AND    A.status_interno in (6, 61) ",          --(v1)     
                   " AND    A.folio = B.folio ",
                   " AND    A.diag_proceso <> '1I' ", 
                   " GROUP BY 1,2,3,4,5,6,7,8,9,10,11,12" CLIPPED

   LET pos = 1
   LET cuenta = 1
   PREPARE cur1 FROM sel_where      
   DECLARE cursor_1 CURSOR FOR cur1

   FOREACH cursor_1 INTO reg_difer.*

      LET vrfc_13   = reg_difer.rfc_letras    CLIPPED       ,
          reg_difer.rfc_numeros   USING"&&&&&&" ,
                      reg_difer.rfc_homonimia CLIPPED

      LET vrfc_10   = reg_difer.rfc_letras    CLIPPED       ,
          reg_difer.rfc_numeros   USING"&&&&&&" 


      DECLARE cursor_con_2 CURSOR FOR
      SELECT  A.status_interno   ,
              A.nro_solicitud    ,
              A.paterno          ,
              A.materno          ,
              A.nombres          ,
              A.rfc              ,
              A.cod_promotor     ,
              A.fecha_baja       ,
              A.fingre           ,
              A.fenvio           
       FROM   pro_solicitud A
       WHERE   A.rfc[1,10] = vrfc_10                 
       AND     A.status_interno in (3,8,21)
       ORDER BY  2

       FOREACH cursor_con_2 INTO reg_cap.*

          SELECT desc_status_corta
          INTO   reg_tot[pos].desc_status_corta 
          FROM   pro_status_interno
          WHERE  status_interno = reg_cap.status_interno

          IF reg_difer.nombre <> reg_cap.nombres THEN
             LET  reg_tot[pos].marca_nombre = "X"
          END IF
      
          IF reg_difer.paterno <> reg_cap.paterno THEN
             LET  reg_tot[pos].marca_paterno = "X"
          END IF

          IF reg_difer.materno <> reg_cap.materno THEN
             LET  reg_tot[pos].marca_materno = "X"
          END IF

          IF vrfc_13 <> reg_cap.rfc THEN
             LET  reg_tot[pos].marca_rfc = "X"
          END IF

          LET reg_tot[pos].folio             =  reg_difer.folio
          LET reg_tot[pos].fecha_carga       =  reg_difer.fecha_carga
          LET reg_tot[pos].fecha_registro    =  reg_difer.fecha_registro
          LET reg_tot[pos].paterno           =  reg_difer.paterno
          LET reg_tot[pos].materno           =  reg_difer.materno
          LET reg_tot[pos].nombre            =  reg_difer.nombre
          LET reg_tot[pos].cod_promotor      =  reg_difer.cod_promotor     
          LET reg_tot[pos].rfc               =  vrfc_13 

          LET reg_tot[pos].status_interno    =  reg_cap.status_interno
          LET reg_tot[pos].nro_solicitud     =  reg_cap.nro_solicitud
          LET reg_tot[pos].paterno_2         =  reg_cap.paterno
          LET reg_tot[pos].materno_2         =  reg_cap.materno
          LET reg_tot[pos].nombre_2          =  reg_cap.nombres
          LET reg_tot[pos].cod_promotor_2    =  reg_cap.cod_promotor
          LET reg_tot[pos].rfc_2             =  reg_cap.rfc          
          LET reg_tot[pos].fecha_baja        =  reg_cap.fecha_baja
          LET reg_tot[pos].fingre            =  reg_cap.fingre
          LET reg_tot[pos].fenvio            =  reg_cap.fenvio

          LET pos = pos + 1
       END FOREACH
       LET cuenta = cuenta +1
   END FOREACH

   CALL SET_COUNT(pos-1)
   ERROR ""

   LET s_tot_registros = cuenta - 1
   IF (pos-1) >= 1 THEN
      INPUT ARRAY reg_tot WITHOUT DEFAULTS FROM scr_3.*
         BEFORE ROW
            LET arr_c = ARR_CURR()
            IF arr_c >= pos THEN
               ERROR "   NO HAY MAS REGISTROS HACIA ABAJO" ATTRIBUTE(NORMAL)
            END IF

            DISPLAY "REGISTROS CARGADOS ",s_tot_registros AT 20,50
            ATTRIBUTE(REVERSE) 


         ON KEY(CONTROL-B)
            LET arr_c = ARR_CURR()

            WHILE TRUE
               PROMPT " DESEA ACEPTAR EL REGISTRO S/N " FOR CHAR  enter
                IF enter MATCHES "[SsNn]" THEN
                   EXIT WHILE
                END IF
            END WHILE
 
        LET vrfc_letras  = reg_tot[arr_c].rfc[1,4]
        LET vrfc_numeros = reg_tot[arr_c].rfc[5,10]

        LET max_row = 0
        SELECT MAX(ROWID) INTO max_row
        FROM   pro_det_agte
        WHERE  nombre         = reg_tot[arr_c].nombre
        AND    paterno        = reg_tot[arr_c].paterno
        AND    materno        = reg_tot[arr_c].materno
        AND    rfc_letras     = vrfc_letras
        AND    rfc_numeros    = vrfc_numeros
        AND    status_interno = 6

        SELECT diag_proceso,
               fecha_registro,
               fecha_proceso
        INTO   vdiag_proceso,
               vfecha_registro,
               vfecha_proceso
        FROM   pro_det_agte
{        WHERE  nombre         = reg_tot[arr_c].nombre
        AND    paterno        = reg_tot[arr_c].paterno
        AND    materno        = reg_tot[arr_c].materno
        AND    rfc_letras     = vrfc_letras
        AND    rfc_numeros    = vrfc_numeros
        AND    status_interno = 6
        group by 1,2,3
 }
        WHERE  ROWID = max_row 
        IF  vdiag_proceso  <> "1A"
        AND vdiag_proceso  <> "1R" 
        AND vdiag_proceso  <> "1K" THEN
            LET reg_tot[arr_c].cod_promotor = reg_tot[arr_c].cod_promotor_2
        END IF

       IF enter MATCHES "[Ss]" THEN

          -- Valida si el apellido materno es igual a nulo le agregara
          -- de forma automatica N/A. --(v10)

          IF reg_tot[arr_c].materno     = " "    OR
             reg_tot[arr_c].materno     IS NULL  OR
             reg_tot[arr_c].materno     = ""     THEN

             LET reg_tot[arr_c].materno  = "N/A"

          END IF

          -- fin (v10)

               CASE reg_tot[arr_c].status_interno 
               WHEN "3"
                      UPDATE pro_solicitud
                      SET pro_solicitud.folio      = reg_tot[arr_c].folio ,
                          pro_solicitud.status         = 1,
                          pro_solicitud.status_interno = 4,
                          pro_solicitud.cod_promotor   = 
                                                   reg_tot[arr_c].cod_promotor,
                          pro_solicitud.paterno        = reg_tot[arr_c].paterno,
                          pro_solicitud.materno        = reg_tot[arr_c].materno,
                          pro_solicitud.nombres        = reg_tot[arr_c].nombre,
                          pro_solicitud.rfc            = reg_tot[arr_c].rfc,
                          pro_solicitud.diag_proceso   = vdiag_proceso,
                          pro_solicitud.fecha_registro = vfecha_registro,
                          pro_solicitud.fecha_proceso  = vfecha_proceso
                      WHERE pro_solicitud.nro_solicitud  = 
                            reg_tot[arr_c].nro_solicitud
                      AND pro_solicitud.status_interno = 3

                      UPDATE pro_det_agte
                         SET    status_interno = 9 
                         WHERE  nombre         = reg_tot[arr_c].nombre
                         AND    paterno        = reg_tot[arr_c].paterno
                         AND    materno        = reg_tot[arr_c].materno
                         AND    rfc_letras     = vrfc_letras
                         AND    rfc_numeros    = vrfc_numeros
                         AND    status_interno = 6
 

               WHEN "8"
                      UPDATE pro_solicitud
                         SET pro_solicitud.folio      = reg_tot[arr_c].folio ,
                             pro_solicitud.status         = 1,
                             pro_solicitud.status_interno = 40,
                             pro_solicitud.cod_promotor   = reg_tot[arr_c].cod_promotor,
                             pro_solicitud.paterno        = reg_tot[arr_c].paterno,
                             pro_solicitud.materno        = reg_tot[arr_c].materno,
                             pro_solicitud.nombres        = reg_tot[arr_c].nombre,
                             pro_solicitud.rfc            = reg_tot[arr_c].rfc,
                             pro_solicitud.diag_proceso   = vdiag_proceso,
                             pro_solicitud.fecha_registro = vfecha_registro,
                             pro_solicitud.fecha_proceso  = vfecha_proceso
                         WHERE 
                           pro_solicitud.nro_solicitud  = reg_tot[arr_c].nro_solicitud
                         AND pro_solicitud.status_interno = 8

                      UPDATE pro_det_agte
                         SET    status_interno = 9 
                         WHERE  nombre         = reg_tot[arr_c].nombre
                         AND    paterno        = reg_tot[arr_c].paterno
                         AND    materno        = reg_tot[arr_c].materno
                         AND    rfc_letras     = vrfc_letras
                         AND    rfc_numeros    = vrfc_numeros
                         AND    status_interno in (6,61)                --(v1)
 
               WHEN "21"
                      UPDATE pro_solicitud
                      SET pro_solicitud.folio      = reg_tot[arr_c].folio ,
                          pro_solicitud.status         = 1,
                          pro_solicitud.status_interno = 41,
                          pro_solicitud.cod_promotor   = 
                                                    reg_tot[arr_c].cod_promotor,
                          pro_solicitud.paterno        = reg_tot[arr_c].paterno,
                          pro_solicitud.materno        = reg_tot[arr_c].materno,
                          pro_solicitud.nombres        = reg_tot[arr_c].nombre,
                          pro_solicitud.rfc            = reg_tot[arr_c].rfc,
                          pro_solicitud.diag_proceso   = vdiag_proceso,
                          pro_solicitud.fecha_registro = vfecha_registro,
                          pro_solicitud.fecha_proceso  = vfecha_proceso
                      WHERE pro_solicitud.nro_solicitud  = 
                                                  reg_tot[arr_c].nro_solicitud
                      AND pro_solicitud.status_interno = 21
                      
                     UPDATE pro_det_agte
                     SET    status_interno = 9 
                     WHERE  nombre         = reg_tot[arr_c].nombre
                     AND    paterno        = reg_tot[arr_c].paterno
                     AND    materno        = reg_tot[arr_c].materno
                     AND    rfc_letras     = vrfc_letras
                     AND    rfc_numeros    = vrfc_numeros
                     AND    status_interno = 6
                     
               END CASE
 
                ERROR "  REGISTRO ACEPTADO..." ATTRIBUTE(NORMAL)
                SLEEP 2
             ELSE
                     ERROR "  PROCESO CANCELADO..." ATTRIBUTE(NORMAL)
                     SLEEP 3
             END IF

      ERROR ""

      LET salida = "N"

             EXIT INPUT
 
              ON KEY (INTERRUPT)
                 LET salida = "N"
          EXIT INPUT 
           END INPUT 

           RETURN salida

   ELSE
      ERROR "  REGISTROS DE PROMOTORES....NO EXISTE" ATTRIBUTE (NORMAL)
      SLEEP 1
      ERROR ""
      LET salida = "N" 
      RETURN salida
   END IF
END FUNCTION
