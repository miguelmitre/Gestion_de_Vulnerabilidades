#############################################################################
#Proyecto            => SISTEMA DE safre_af( MEXICO )
#Propietario         => E.F.P.
#Programa INTB0801   => PIDE ARCHIVO PARA LA CARGA  DEL RESULTADO DE CORRESPON-
#                       DENCIA VAN EXISTIR 2 TIPOS DE ARCHIVOS:
#result_corresp_T.100104.csv     1     ESTADO DE CUENTA  (CEDENTE).
#                  mmddayo
#result_corresp_R.100104.csv     2     NOTIFICACION DE TRASPASO (RECEPTORA).
#                  mmddayo
#result_corresp_A.100104.csv     3     NOTIFICACION DE AFILIACION
#                  mmddayo
#****El Nomb del archivo puede ir de 25 regs a 29 regs como maximo*****
#                       LA TABLA QUE SE LLENARIA SERIA LA DE safre_af:int_cd_det
#                       notifica(ACEPTADOS),safre_af:int_cd_rech_corre(RECHAZADO
#                       S)
#Por                 => LAURA EUGENIA CORTES GUZMAN
#Fecha creacion      => 18 DE OCTUBRE DEL 2004
#Modificado por      => LAURA EUGENIA CORTES GUZMAN
#Fecha Ult. Modif.   => 08 DE SEPTIEMBRE DEL 2005
#Modificado por      => Isabel Fonseca Frias 
#Fecha Ult. Modif.   => 12-11-2008 
#                    => Se agrega la carga de los NO AFILIADOS (v1) 
#Sistema             => INT
###############################################################################
DATABASE safre_af
GLOBALS
DEFINE param_ruta RECORD LIKE seg_modulo.*
DEFINE HOY             DATE
DEFINE enter           CHAR(001)
DEFINE nom_archivo     CHAR(029)
DEFINE letra           CHAR(001)
DEFINE mes             CHAR(02)
DEFINE dia             CHAR(02)
DEFINE ano             CHAR(04)
DEFINE fecha_t_r       CHAR(010)
DEFINE tipo_cons       CHAR(01)
DEFINE t_constancia    CHAR(03)
DEFINE ruta_y_arch     CHAR(100)
DEFINE cuantos         INTEGER
DEFINE num_d_reg       INTEGER
DEFINE bandera         SMALLINT
DEFINE g_usuario       CHAR(010)
DEFINE v_ultimo        INTEGER

END GLOBALS
MAIN
    OPTIONS INPUT WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT

    CALL STARTLOG ("INTB0801.log")

    CALL inicio()

    OPEN WINDOW ventana_1 AT 2,2 WITH 2 ROWS, 76 COLUMNS ATTRIBUTE(BORDER)
         MENU "CARGA CORREO DEVUELTO"
              COMMAND "Constancia 30221 "
                      "Carga archivo de Constancias de Traspasos 30221"
                      CALL proceso_principal(1)
                      CALL inicio()
                      CLEAR SCREEN
              COMMAND "Edos. Ctas " "Carga archivo Estados de Cuentas"
                      CALL proceso_principal(2)
                      CALL inicio()
                      CLEAR SCREEN
              COMMAND "Transf. Siefores" "Carga archivo de Folletos"
                      CALL proceso_principal(3)
                      CALL inicio()
                      CLEAR SCREEN
              COMMAND "Constancias AFILIACION "
                      "Carga archivo de Constancias de Afiliacion "
                      CALL proceso_principal(4)
                      CALL inicio()
                      CLEAR SCREEN

              COMMAND "Constancias NO AFILIADOS "                      --(v1)
                      "Carga archivo de Constancias de No Afiliacion " --(v1)
                      CALL proceso_principal(5)                        --(v1) 
                      CALL inicio()                                    --(v1)
                      CLEAR SCREEN                                     --(v1)   


              COMMAND "REVERSO ARCHIVOS" "Reverso de archivos por Folio"
                      CALL reverso()
                      CLEAR SCREEN
              COMMAND "CONSULTA" "Consulta los Reg. Cargados por fecha"
                      CALL consulta()
                      CLEAR SCREEN
              COMMAND "Salir " "Salir del Programa"
                      EXIT MENU
         END MENU

    CLOSE WINDOW ventana_1
    EXIT PROGRAM
END MAIN
#==============================================================================
#FUNCION DE INICIO
#==============================================================================
FUNCTION inicio()

 INITIALIZE param_ruta.*, HOY, enter, nom_archivo, letra, mes, dia, ano TO NULL
 INITIALIZE fecha_t_r, t_constancia, ruta_y_arch,g_usuario TO NULL
 LET cuantos = 0 LET num_d_reg = 0 LET bandera = 0 LET v_ultimo = 0

 LET HOY = TODAY

 SELECT *
 INTO param_ruta.*
 FROM seg_modulo
 WHERE modulo_cod = "int"

 SELECT USER
 INTO g_usuario
 FROM   tab_afore_local

 WHENEVER ERROR CONTINUE
 DROP TABLE tmp_corres

 CREATE TEMP TABLE tmp_corres( nss         CHAR(011),
                               identifica  CHAR(001),
                               facuse      DATE,
                               diag_corre  CHAR(002),
                               t_correo    CHAR(003)
                              )
 WHENEVER ERROR STOP

END FUNCTION
#==============================================================================
#FUNCION DE PROCESO PRINCIPAL
#==============================================================================
FUNCTION proceso_principal(recibo)

    DEFINE recibo, ban  SMALLINT

    LET ban = 0

    OPEN WINDOW INTB0801 AT 5,2 WITH FORM "INTB08011" ATTRIBUTE(BORDER)
    CASE recibo
    WHEN 1
         DISPLAY "INTB0801        CARGA RESULT. CORRESP.CONSTANCIA 30221 ",
                 "                        " AT 3,1 ATTRIBUTE(REVERSE)
    WHEN 2
         DISPLAY "INTB0801     CARGA RESULT. CORRESPONDENCIA DE EDOS.D",
                 "E CTAS.                    " AT 3,1 ATTRIBUTE(REVERSE)
    WHEN 3
         DISPLAY "INTB0801     CARGA RESULT. CORRESPONDENCIA DE TRASF.",
                 "DE SIEFORES                " AT 3,1 ATTRIBUTE(REVERSE)
    WHEN 4
         DISPLAY "INTB0801      CARGA RESULT. CORRESP.CONSTANCIA AFILIA",
                 "CION                      " AT 3,1 ATTRIBUTE(REVERSE)
    WHEN 5                                                             --(v1)
         DISPLAY "INTB0801        CARGA RESULT. CORRESP.CONSTANCIA NO AFILIA",
                 "CION                    " AT 3,1 ATTRIBUTE(REVERSE)  --(v1)


    END CASE

    DISPLAY "                             < CTRL-C > Sali",
            "r                                  " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING "DD-MM-YYYY" AT 3,64 ATTRIBUTE(REVERSE)

    INPUT BY NAME nom_archivo

       AFTER FIELD nom_archivo
       IF (nom_archivo IS NULL OR nom_archivo = "") THEN
          ERROR "ARCHIVO NO VALIDO...TECLEAR NOMBRE DE ARCHIVO VALIDO ..."
          SLEEP 3
          ERROR ""
          NEXT FIELD nom_archivo
       ELSE
          LET mes        = nom_archivo[18,19]
          LET dia        = nom_archivo[20,21]
          LET ano        = nom_archivo[22,23]
          LET fecha_t_r  = mes CLIPPED,"/",dia CLIPPED,"/","20",ano CLIPPED

      END IF

     ON KEY(ESC)
        EXIT INPUT

     ON KEY(INTERRUPT, CONTROL-C)
        PROMPT "Proceso Cancelado...<ENTER> para Salir " for char enter
        LET ban = 1
        EXIT INPUT

     END INPUT

     IF ban = 1 THEN
        CLOSE WINDOW INTB0801
        RETURN
     END IF

     WHILE TRUE
        PROMPT "Esta Seguro de Generar El Proceso [S/N] : ? " FOR enter
        IF enter MATCHES "[sSnN]" THEN
            IF enter MATCHES "[sS]" THEN
                EXIT WHILE
            ELSE
             PROMPT "Proceso Cancelado...<ENTER> para Salir" for char enter
                EXIT WHILE
            END IF
        END IF
     END WHILE

     IF enter MATCHES "[nN]" THEN
        CLOSE WINDOW INTB0801
        RETURN
     END IF
#=====CHECA SI ALGUNO DE LOS CAMPOS NO FUERON LLENADOS==========================

     IF (nom_archivo IS NULL OR nom_archivo = "") THEN
         ERROR "--NO-- LLENO EL CAMPO..."
         SLEEP 2
         ERROR ""

         PROMPT "Proceso Cancelado...<ENTER> para Salir" for char enter
         CLOSE WINDOW INTB0801
         RETURN
     END IF

#=====CHECA SI YA HAY ENVIOS CON  LA FECHA QUE TECLEO EL USUARIO================
     LET v_ultimo    = 0

     CASE recibo
     WHEN 1
              SELECT "OK" FROM int_cd_det_notifica
              WHERE fecha_traspaso  = fecha_t_r
                AND tipo_con        = "R"
                AND constancia      IN( "C17","C04")
                AND procesar        = 0
              GROUP BY 1
     WHEN 2
              SELECT "OK" FROM int_cd_det_notifica
              WHERE fecha_traspaso  = fecha_t_r
                AND tipo_con        = "T"
                AND constancia      IN("E3","E03")
                AND procesar        = 0
              GROUP BY 1
     WHEN 3
              SELECT "OK" FROM int_cd_det_notifica
              WHERE fecha_traspaso  = fecha_t_r
                AND tipo_con        = "F"
                AND procesar        = 0
              GROUP BY 1
     WHEN 4
              SELECT unique "OK" FROM int_cd_det_notifica
              WHERE fecha_traspaso  = fecha_t_r
                AND tipo_con        = "A"
                AND constancia      IN("C01","C02")
                AND procesar        = 0
              GROUP BY 1
     WHEN 5                                                        --(v1)
              SELECT UNIQUE "a.OK" FROM int_cd_det_notifica a          --(v1)
              WHERE a.fecha_traspaso  = fecha_t_r                      --(v1)
              AND   a.tipo_con        = "I"                            --(v1)
              AND   a.constancia      IN("N01")                        --(v1)
              AND   a.procesar        = 0                              --(v1)
     END CASE

     IF SQLCA.SQLCODE <> 0 THEN

          ERROR "PERIODO GENERADO CON ANTERIORIDAD ..."
          WHILE TRUE
             PROMPT "Desea regenerar proceso de Carga [S/N] : ? " FOR enter
             IF enter MATCHES "[sSnN]" THEN
                 IF enter MATCHES "[sS]" THEN

                    CASE recibo
                    WHEN 1
                          SELECT max(a.n_envios) + 1
                          INTO v_ultimo
                          FROM  int_cd_det_notifica a
                          WHERE a.fecha_traspaso  = fecha_t_r
                          AND   a.tipo_con        = "R"
                          AND   a.constancia      IN( "C17","C04")
                          AND   a.procesar        = 0
                    WHEN 2
                          SELECT max(a.n_envios) + 1
                          INTO v_ultimo
                          FROM  int_cd_det_notifica a
                          WHERE a.fecha_traspaso  = fecha_t_r
                          AND   a.tipo_con        = "T"
                          AND   a.constancia      IN("E3","E03")
                          AND   a.procesar        = 0
                    WHEN 3
                          SELECT max(a.n_envios) + 1
                          INTO v_ultimo
                          FROM  int_cd_det_notifica a
                          WHERE a.fecha_traspaso  = fecha_t_r
                          AND   a.tipo_con        = "F"
                          AND   a.procesar        = 0
                    WHEN 4
                          SELECT max(a.n_envios) + 1
                          INTO v_ultimo
                          FROM  int_cd_det_notifica a
                          WHERE a.fecha_traspaso  = fecha_t_r
                          AND   a.tipo_con        = "A"
                          AND   a.constancia      IN("C01","C02","C04")
                          AND   a.procesar        = 0
                    WHEN 5                                         --(v1)
                          SELECT max(a.n_envios) + 1               --(v1)
                          INTO v_ultimo                            --(v1)
                          FROM  int_cd_det_notifica a              --(v1)
                          WHERE a.fecha_traspaso  = fecha_t_r      --(v1)
                          AND   a.tipo_con        = "I"            --(v1)  
                          AND   a.constancia      IN("N01")        --(v1)
                          AND   a.procesar        = 0              --(v1)
                    END CASE

                    IF v_ultimo IS NULL OR v_ultimo = " " THEN
                       LET v_ultimo = 0
                    END IF

                    EXIT WHILE
                 ELSE
                    EXIT WHILE
                 END IF
             END IF
         END WHILE

         IF enter MATCHES "[nN]" THEN
            CLOSE WINDOW INTB0801
            RETURN
         END IF
     END IF

#===issss===========================================================================

    DISPLAY "PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)
    sleep 1

    LET nom_archivo    =  nom_archivo  CLIPPED

    CALL carga_arch_tmp(recibo) RETURNING bandera

       IF (bandera = 0) THEN # NO HAY ERROR EN EL ARCHIVO
          CALL valid_carga_tblas(recibo)
       ELSE
           CLOSE WINDOW INTB0801
           RETURN
       END IF

    CLOSE WINDOW INTB0801
    RETURN

END FUNCTION
#===============================================================================
#FUNCION PARA CARGAR EL ARCHIVO TECLEADO
#===============================================================================
FUNCTION carga_arch_tmp(recibo)
DEFINE recibo         SMALLINT

DEFINE f_reg                  RECORD
          nss         CHAR(011),
          identifica  CHAR(001),
          facuse      CHAR(010),
          diag_corre  CHAR(002),
          t_correo    CHAR(003)
       END RECORD
DEFINE long        INTEGER

      LET ruta_y_arch =  param_ruta.ruta_rescate CLIPPED,"/",nom_archivo CLIPPED

      LOAD FROM ruta_y_arch DELIMITER ","
      INSERT INTO tmp_corres

      LET cuantos   =  0
      LET num_d_reg =  0
      LET bandera   =  0

      SELECT COUNT(*)
         INTO cuantos
         FROM tmp_corres
      IF (cuantos = 0 OR cuantos IS NULL) THEN
         ERROR "NO EXISTE ARCHIVO DE CARGA..."
         SLEEP 2
         ERROR " "
         RETURN 1
      ELSE
         DISPLAY "TOTAL DE REGISTROS EN EL ARCHIVO:  ",cuantos AT 09,08

         IF recibo <> 5 THEN                                   --(v1)

            ERROR "Verificando Archivo plano..."
            SLEEP 2
            ERROR " "
   
            DECLARE c CURSOR FOR
   
               SELECT *
                  FROM tmp_corres
   
            FOREACH c INTO f_reg.*
   
               LET num_d_reg = num_d_reg + 1
               LET long = length(f_reg.diag_corre)
   
               IF (valida_reg(11,f_reg.nss,1) ) OR   #SE ENCONTRO ERROR EN EL ARCH
                  (valida_reg(long,f_reg.diag_corre USING "&&",1) ) THEN
                  LET bandera = 1
   
                  OPEN WINDOW w_error AT 14,15 WITH 6 ROWS,50 COLUMNS
                       ATTRIBUTE (BORDER)
   
                     DISPLAY  " ARCHIVO --NO-- CARGADO POR ENCONTRARSE ERRONEO   "
                              AT 01,01 ATTRIBUTE (REVERSE)
   
                     DISPLAY  "NUM DE LINEA: ",num_d_reg  AT 03,01
   
                     DISPLAY  "DESCRIP DE LA LINEA: ","nss: ",
                              f_reg.nss,"  ","diag: ",f_reg.diag_corre AT 04,01
                     DISPLAY  "__________________________________________________"
                              AT 05,01
   
                     PROMPT "TECLEE ENTER PARA SALIR..." FOR ENTER
                     CLOSE WINDOW w_error
                     RETURN 1

               ELSE                              #NO SE ENCONTRO ERROR EN EL ARCH
               END IF

            END FOREACH

         END IF                                                   --(v1) 

      END IF
      RETURN 0
END FUNCTION
#==========ESTA FUNCION PERMITE VERIFICAR QUE EL ARCHIVO NO CONTENGA CARACTERES=
#==========RAROS
FUNCTION valida_reg(long, cadena,tcadena)

DEFINE i        INTEGER 
DEFINE long     INTEGER 
DEFINE cadena   CHAR(100)
DEFINE tcadena  SMALLINT
DEFINE v_num    CHAR(01)

CASE tcadena
WHEN 1
        FOR i = 1 to long
           LET v_num = cadena[i,i]

           IF (v_num NOT MATCHES "[0-9]") THEN
                   RETURN TRUE
           END IF
        END FOR
WHEN 2
        FOR i = 1 to long
           LET v_num = cadena[i,i]

           IF (v_num NOT MATCHES "[A-Z]") THEN
                   RETURN TRUE
           END IF
        END FOR
WHEN 3
END CASE
   RETURN FALSE

END FUNCTION
#=====FUNCION QUE SIRVE PARA VALIDAR QUE EL NSS Y SU FECHA DE TRASPASO=========
#     SE ENCUENTREN EN LAS TABLAS HISTORICAS DE TRASPASOS TANTO PARA RECEPTORA
#     COMO PARA CEDENTE
#==============================================================================
FUNCTION valid_carga_tblas(recibo)
DEFINE recibo         SMALLINT

DEFINE f_regarch      RECORD
          nss         CHAR(011),
          identifica  CHAR(001),
          facuse      CHAR(010),
          diag_corre  CHAR(002),
          constancia  CHAR(003)
       END RECORD

DEFINE lnss             CHAR(011)
DEFINE lfecha_tra       DATE
DEFINE ltipo_con        CHAR(01)
DEFINE lconstancia      CHAR(03)
DEFINE ldiag_corre      SMALLINT
DEFINE lcve_ced_cuenta  CHAR(03)
DEFINE lfactualiza      DATE
DEFINE lusuario         CHAR(010)
DEFINE lidentifica      CHAR(001)
DEFINE lfec_acuse       DATE
DEFINE lcont_rech       INTEGER 
DEFINE lcont_acep       INTEGER 
DEFINE lprocesar        INTEGER 
DEFINE v_folio          INTEGER


ERROR "CARGANDO INFORMACION..."
SLEEP 5
ERROR " "

   LET v_folio      =  0
   LET lcont_rech   =  0
   LET lcont_acep   =  0

   INSERT INTO int_folio values(0)
   SELECT MAX(a.folio) INTO v_folio FROM int_folio a

   CASE recibo
   WHEN 2       #ESTADOS DE CUENTA cedente

          INITIALIZE f_regarch.* TO NULL

          DECLARE apt_edo_cta CURSOR FOR
             SELECT *
               FROM tmp_corres
          FOREACH apt_edo_cta INTO f_regarch.*
             LET lnss         = f_regarch.nss
             LET lfecha_tra   = fecha_t_r
             LET ltipo_con    = "T"
             LET lconstancia  = f_regarch.constancia
             LET ldiag_corre  = f_regarch.diag_corre
             LET lidentifica  = f_regarch.identifica
             LET lfec_acuse   = f_regarch.facuse

             SELECT UNIQUE cve_ced_cuenta
             INTO   lcve_ced_cuenta
             FROM   safre_af:taa_cd_det_cedido
             WHERE  n_seguro          =  lnss
               AND  fecha_trasp       =  lfecha_tra
               AND  estado   IN (103,12,99)
             IF (STATUS = NOTFOUND) THEN
                LET lcve_ced_cuenta = NULL
             END IF

             LET lfactualiza  = HOY
             LET lusuario     = g_usuario

             SELECT "X"
             FROM   safre_af:taa_cd_det_cedido
             WHERE  n_seguro          =  lnss
               AND  fecha_trasp       =  lfecha_tra
               AND  estado   IN (103,12,99)
             IF ( STATUS = NOTFOUND ) THEN
                LET lcont_rech   =  lcont_rech + 1

#====VERIFICA EL DIAGNOSTICO DE CORRESPONDENCIA SE ENCUENTRA EN EL CATALOGO=
                SELECT "OK"
                FROM   safre_af:int_cd_tab_nota
                WHERE  @diag_correo  = ldiag_corre
                IF (STATUS = NOTFOUND) THEN
                   LET ldiag_corre= 28 #No calificado
                END IF

                INSERT INTO safre_af:int_cd_rech_corre
                        VALUES ( lnss,
                                 lfecha_tra,
                                 ltipo_con,
                                 lconstancia,
                                 ldiag_corre,
                                 lcve_ced_cuenta,
                                 lfactualiza,
                                 lusuario   ,
                                 lidentifica,
                                 lfec_acuse ,
                                 v_ultimo   ,
                                 "",
                                 0,
                                 "",
                                 v_folio )

             ELSE
                LET lcont_acep   =  lcont_acep + 1

#====VERIFICA SI EL DIAGNOSTICO DE CORRESPONDENCIA SE ENCUENTRA EN EL CATALOGO=
                SELECT "OK"
                FROM   safre_af:int_cd_tab_nota
                WHERE  @diag_correo  = ldiag_corre
                IF (STATUS = NOTFOUND) THEN
                   LET ldiag_corre  = 28 #No calificado
                END IF

                IF ldiag_corre = 0 THEN
                   LET lprocesar = 1
                ELSE
                   LET lprocesar = 0
                END IF

                INSERT INTO safre_af:int_cd_det_notifica
                        VALUES ( lnss,
                                 lfecha_tra,
                                 ltipo_con,
                                 lconstancia,
                                 ldiag_corre,
                                 lcve_ced_cuenta,
                                 lfactualiza,
                                 lusuario   ,
                                 lidentifica,
                                 lfec_acuse ,
                                 v_ultimo   ,
                                 "",
                                 0,
                                 "",
                                 v_folio )
             END IF
          END FOREACH
   WHEN 1
          DECLARE r CURSOR FOR
             SELECT *
               FROM tmp_corres
          FOREACH r INTO f_regarch.*
             LET lnss         = f_regarch.nss
             LET lfecha_tra   = fecha_t_r
             LET ltipo_con    = "R"
             LET lconstancia  = f_regarch.constancia
             LET ldiag_corre  = f_regarch.diag_corre
             LET lidentifica  = f_regarch.identifica
             LET lfec_acuse   = f_regarch.facuse

             SELECT UNIQUE cve_ced_cuenta
             INTO   lcve_ced_cuenta
             FROM   safre_af:taa_viv_recepcion
             WHERE  nss               =  lnss
               AND  fecha_mov_banxico =  lfecha_tra
               AND  ident_operacion   =  "09"
             IF (STATUS = NOTFOUND) THEN
                LET lcve_ced_cuenta = NULL
             END IF
             LET lfactualiza  = HOY
             LET lusuario     = g_usuario

             SELECT "X"
             FROM   safre_af:taa_viv_recepcion
             WHERE  nss               =  lnss
               AND  fecha_mov_banxico =  lfecha_tra
               AND  ident_operacion   =  "09"
             IF ( STATUS = NOTFOUND ) THEN
                LET lcont_rech   =  lcont_rech + 1
#====VERIFICA SI EL DIAGNOSTICO DE CORRESPONDENCIA SE ENCUENTRA EN EL CATALOGO=
                SELECT "OK"
                FROM   safre_af:int_cd_tab_nota
                WHERE  @diag_correo  = ldiag_corre
                IF (STATUS = NOTFOUND) THEN
                   LET ldiag_corre  = 28 #No calificado
                END IF

                IF ldiag_corre = 0 THEN
                   LET lprocesar = 1
                ELSE
                   LET lprocesar = 0
                END IF
#==============================================================================

                INSERT INTO safre_af:int_cd_rech_corre
                        VALUES ( lnss,
                                 lfecha_tra,
                                 ltipo_con,
                                 lconstancia,
                                 ldiag_corre,
                                 lcve_ced_cuenta,
                                 lfactualiza,
                                 lusuario   ,
                                 lidentifica,
                                 lfec_acuse ,
                                 v_ultimo   ,
                                 "",
                                 0,
                                 "",
                                 v_folio )

             ELSE
                LET lcont_acep   =  lcont_acep + 1

#====VERIFICA SI EL DIAGNOSTICO DE CORRESPONDENCIA SE ENCUENTRA EN EL CATALOGO=
                SELECT "OK"
                FROM   safre_af:int_cd_tab_nota
                WHERE  @diag_correo  = ldiag_corre
                IF (STATUS = NOTFOUND) THEN
                   LET ldiag_corre  = 28 #No calificado
                END IF
#==============================================================================
                INSERT INTO safre_af:int_cd_det_notifica
                        VALUES ( lnss,
                                 lfecha_tra,
                                 ltipo_con,
                                 lconstancia,
                                 ldiag_corre,
                                 lcve_ced_cuenta,
                                 lfactualiza,
                                 lusuario   ,
                                 lidentifica,
                                 lfec_acuse ,
                                 v_ultimo   ,
                                 "",
                                 lprocesar,
                                 "",
                                 v_folio )
             END IF
          END FOREACH
         EXIT CASE

   WHEN 3   ## FOLLETOS
          DECLARE folleto CURSOR FOR
             SELECT *
               FROM tmp_corres
          FOREACH folleto INTO f_regarch.*
             LET lnss         = f_regarch.nss
             LET lfecha_tra   = fecha_t_r
             LET ltipo_con    = "F"
             LET lconstancia  = f_regarch.constancia
             LET ldiag_corre  = f_regarch.diag_corre
             LET lidentifica  = f_regarch.identifica
             LET lfec_acuse   = f_regarch.facuse
             LET lfactualiza  = HOY
             LET lusuario     = g_usuario
             LET lcont_rech   = 0 -- lcont_rech + 0
             LET lcont_acep   =  lcont_acep + 1

#====VERIFICA SI EL DIAGNOSTICO DE CORRESPONDENCIA SE ENCUENTRA EN EL CATALOGO=
                SELECT "OK"
                FROM   safre_af:int_cd_tab_nota
                WHERE  @diag_correo  = ldiag_corre
                IF (STATUS = NOTFOUND) THEN
                   LET ldiag_corre  = 28 #No calificado
                END IF
#==============================================================================
                INSERT INTO safre_af:int_cd_det_notifica
                        VALUES ( lnss,
                                 lfecha_tra,
                                 ltipo_con,
                                 lconstancia,
                                 ldiag_corre,
                                 lcve_ced_cuenta,
                                 lfactualiza,
                                 lusuario   ,
                                 lidentifica,
                                 lfec_acuse ,
                                 v_ultimo   ,
                                 "",
                                 lprocesar,
                                 "",
                                 v_folio )
         END FOREACH
         EXIT CASE

   WHEN 4   ## carta_30201 y afiliacion

          LET lcont_rech   = 0
          LET lcont_acep   = 0

          DECLARE carta_30201 CURSOR FOR
             SELECT *
               FROM tmp_corres
          FOREACH carta_30201 INTO f_regarch.*
             LET lnss         = f_regarch.nss
             LET lfecha_tra   = fecha_t_r
             LET ltipo_con    = "A"
             LET lconstancia  = f_regarch.constancia
             LET ldiag_corre  = f_regarch.diag_corre
             LET lidentifica  = f_regarch.identifica
             LET lfec_acuse   = f_regarch.facuse
             LET lcve_ced_cuenta = NULL
             LET lfactualiza  = HOY
             LET lusuario     = g_usuario


            SELECT UNIQUE "a.X" FROM int_ctr_carta a
            WHERE  a.fecha_registro  = lfecha_tra
            AND    a.docto_cod       IN('30201','30202')
            AND    a.nss             = lnss
            IF STATUS = NOTFOUND THEN
                LET lcont_rech   = lcont_rech + 1
                INSERT INTO safre_af:int_cd_rech_corre
                        VALUES ( lnss,
                                 lfecha_tra,
                                 ltipo_con,
                                 lconstancia,
                                 ldiag_corre,
                                 lcve_ced_cuenta,
                                 lfactualiza,
                                 lusuario   ,
                                 lidentifica,
                                 lfec_acuse ,
                                 v_ultimo   ,
                                 "",
                                 0,
                                 "",
                                 v_folio )
            ELSE

                LET lcont_acep   =  lcont_acep + 1


#====VERIFICA SI EL DIAGNOSTICO DE CORRESPONDENCIA SE ENCUENTRA EN EL CATALOGO=
                SELECT "OK"
                FROM   safre_af:int_cd_tab_nota
                WHERE  @diag_correo  = ldiag_corre
                IF (STATUS = NOTFOUND) THEN
                   LET ldiag_corre  = 28 #No calificado
                END IF
#==============================================================================
                INSERT INTO safre_af:int_cd_det_notifica
                        VALUES ( lnss,
                                 lfecha_tra,
                                 ltipo_con,
                                 lconstancia,
                                 ldiag_corre,
                                 lcve_ced_cuenta,
                                 lfactualiza,
                                 lusuario   ,
                                 lidentifica,
                                 lfec_acuse ,
                                 v_ultimo   ,
                                 "",
                                 lprocesar,
                                 "",
                                 v_folio )
            END IF
          END FOREACH
         EXIT CASE
   WHEN 5   ## carta_30501 NO afiliacion
          LET lcont_rech   = 0
          LET lcont_acep   = 0

          DECLARE carta_30501 CURSOR FOR
             SELECT *
               FROM tmp_corres
          FOREACH carta_30501 INTO f_regarch.*
             LET lnss         = f_regarch.nss
             LET lfecha_tra   = fecha_t_r
             LET ltipo_con    = "I"
             LET lconstancia  = f_regarch.constancia
             LET ldiag_corre  = f_regarch.diag_corre
             LET lidentifica  = f_regarch.identifica
             LET lfec_acuse   = f_regarch.facuse
             LET lfactualiza  = HOY
             LET lusuario     = g_usuario

             SELECT UNIQUE "a.X" FROM int_ctr_carta a
             WHERE  a.fecha_registro  = lfecha_tra
             AND    a.docto_cod       IN('30501')
             AND    a.nss             = lnss
             IF ( STATUS = NOTFOUND ) THEN   --(si no no lo encuentra)
                LET lcont_rech   =  lcont_rech + 1

                INSERT INTO safre_af:int_cd_rech_corre
                        VALUES ( lnss,
                                 lfecha_tra,
                                 ltipo_con,
                                 lconstancia,
                                 ldiag_corre,
                                 lcve_ced_cuenta,
                                 lfactualiza,
                                 lusuario   ,
                                 lidentifica,
                                 lfec_acuse ,
                                 v_ultimo   ,
                                 "",
                                 0,
                                 "",
                                 v_folio )

             ELSE
                LET lcont_acep   =  lcont_acep + 1

#====VERIFICA SI EL DIAGNOSTICO DE CORRESPONDENCIA SE ENCUENTRA EN EL CATALOGO=
                SELECT UNIQUE "OK"
                FROM   safre_af:int_cd_tab_nota
                WHERE  @diag_correo  = ldiag_corre
                IF (STATUS = NOTFOUND) THEN
                   LET ldiag_corre  = 28 #No calificado
                END IF
#==============================================================================
                INSERT INTO safre_af:int_cd_det_notifica
                        VALUES ( lnss,
                                 lfecha_tra,
                                 ltipo_con,
                                 lconstancia,
                                 ldiag_corre,
                                 lcve_ced_cuenta,
                                 lfactualiza,
                                 lusuario   ,
                                 lidentifica,
                                 lfec_acuse ,
                                 v_ultimo   ,
                                 "",
                                 lprocesar,
                                 "",
                                 v_folio )
             END IF
          END FOREACH
         EXIT CASE
   END CASE

   INSERT INTO int_arch_cordev VALUES(nom_archivo, TODAY,lusuario,v_folio)

   DISPLAY "TOTAL DE REGISTROS INSERTADOS COMO ACEPTADOS :  ",
           lcont_acep USING "###&"  AT 11,08
   DISPLAY "TOTAL DE REGISTROS INSERTADOS COMO RECHAZADOS:  ",
           lcont_rech USING "###&"  AT 13,08
   DISPLAY "F O L I O  : ",v_folio USING "#########&" AT 15,8

   PROMPT "PRESIONE <ENTER>...PARA CONTINUAR" FOR enter
   RETURN
END FUNCTION

FUNCTION reverso()
    DEFINE vfolio   INTEGER,
           lacep    INTEGER,
           lrech    INTEGER,
           respuesta  CHAR(1)

    LET vfolio = 0
    LET lacep = 0
    LET lrech = 0

    OPEN WINDOW INTB08012 AT 5,2 WITH FORM "INTB08012" ATTRIBUTE(BORDER)
         INPUT vfolio FROM FORMONLY.folio
               AFTER FIELD folio
                   IF vfolio IS NULL OR vfolio = "" THEN
                      ERROR "Digite correctamente el Folio a Reversar"
                      SLEEP 2
                      NEXT FIELD folio
                   ELSE
                       SELECT "a.X" FROM int_arch_cordev a
                       WHERE  a.folio  = vfolio
                       IF STATUS = NOTFOUND THEN
                          ERROR "NO EXISTE EL FOLIO "
                          SLEEP 2
                          NEXT FIELD folio
                       ELSE
                          INITIALIZE respuesta TO NULL
                          CALL confirma() RETURNING respuesta
                          IF respuesta MATCHES "[sS]" THEN

                             DELETE FROM int_arch_cordev
                             WHERE folio = vfolio

                             SELECT COUNT(*) INTO lacep FROM int_cd_det_notifica
                             WHERE folio = vfolio

                             DELETE FROM int_cd_det_notifica
                             WHERE folio = vfolio

                             SELECT COUNT(*) INTO lrech FROM int_cd_rech_corre
                             WHERE folio = vfolio

                             DELETE FROM int_cd_rech_corre
                             WHERE folio = vfolio

                             DELETE FROM int_folio
                             WHERE folio = vfolio

                             DISPLAY "TOTAL DE REGISTROS REVERSADOS COMO ACEPTADOS :  ",
                                     lacep USING "###&"  AT 11,08
                             DISPLAY "TOTAL DE REGISTROS REVERSADOS COMO RECHAZADOS:  ",
                                     lrech USING "###&"  AT 13,08
                             PROMPT "REVERSO PROCESADO SATISFACTORIAMENTE"
                                    FOR enter
                             EXIT INPUT
                          ELSE
                             LET vfolio = 0
                             NEXT FIELD folio
                          END IF

                       END IF
                   END IF
               ON KEY (CONTROL-C, INTERRUPT)
                  PROMPT "PROCESO CANCELADO...<ENTER> PARA CONTINUAR"
                         FOR enter
                  EXIT INPUT
         END INPUT
    CLOSE WINDOW INTB08012
END FUNCTION

FUNCTION confirma()
   WHILE TRUE
      PROMPT "DESEA HACER EL REVERSO S/N? " FOR enter
      IF enter MATCHES "[sSnN]" THEN
         EXIT WHILE
      END IF
   END WHILE

   RETURN enter
END FUNCTION

FUNCTION consulta()
    DEFINE fecha_inicio,
           fecha_final    DATE,
           tipo           CHAR(1),
           i, pos         INTEGER,

           con_por   RECORD
                indica_a   INTEGER,
                nss        CHAR(11),
                folio      DECIMAL(08,0),
                tip_sol    SMALLINT,
                paterno    CHAR(40),
                materno    CHAR(40),
                nombre     CHAR(40),
                factualiza DATE
           END RECORD,

           l_record   ARRAY[2000] OF RECORD
                indica_a       INTEGER,
                nss            CHAR(11),
                folio          DECIMAL(8,0),
                tip_sol        SMALLINT,
                nombre         CHAR(28),
                factualiza     DATE
           END RECORD,

           ban              SMALLINT,
           G_LISTA             CHAR(70),
           ejecuta             CHAR(100)

    LET ban = 0
    LET pos = 0
    LET HOY = TODAY

    INITIALIZE G_LISTA, ejecuta TO NULL
    OPEN WINDOW INTB08013 AT 5,2 WITH FORM "INTB08013" ATTRIBUTE(BORDER)
         INPUT fecha_inicio, fecha_final, tipo
               FROM FORMONLY.fecha_inicio, FORMONLY.fecha_final,FORMONLY.tipo

               AFTER FIELD fecha_inicio
                   IF fecha_inicio IS NULL OR fecha_inicio = "          " THEN
                      ERROR "La Fecha Inicio no puede ser blanco o nula"
                      NEXT FIELD fecha_inicio
                   END IF
                   IF fecha_inicio > TODAY THEN
                      ERROR "La Fecha Inicio no puede ser mayor al dia de hoy"
                      NEXT FIELD fecha_inicio
                   END IF
               AFTER FIELD fecha_final
                   IF fecha_final IS NULL OR fecha_final = "          " THEN
                      ERROR "La Fecha Final no puede ser blanco o nula"
                      NEXT FIELD fecha_final
                   END IF
                   IF fecha_final > TODAY THEN
                      ERROR "La Fecha Final no puede ser mayor al dia de hoy"
                      NEXT FIELD fecha_final
                   END IF
                   IF fecha_inicio > fecha_final THEN
                      ERROR "La Fecha Inicial no puede ser mayor a la F.Final"
                      NEXT FIELD fecha_final
                   END IF

               AFTER FIELD tipo
                   IF tipo NOT MATCHES "[RTDF]" THEN
                      ERROR "Digite correctamente el Tipo de Documento"
                      NEXT FIELD tipo
                   END IF


               ON KEY (ESC)
                   IF fecha_inicio IS NULL OR fecha_inicio = "          " THEN
                      ERROR "La Fecha Inicio no puede ser blanco o nula"
                      NEXT FIELD fecha_inicio
                   END IF
                   IF fecha_inicio > TODAY THEN
                      ERROR "La Fecha Inicio no puede ser mayor al dia de hoy"
                      NEXT FIELD fecha_inicio
                   END IF
                   IF fecha_final IS NULL OR fecha_final = "          " THEN
                      ERROR "La Fecha Final no puede ser blanco o nula"
                      NEXT FIELD fecha_final
                   END IF
                   IF fecha_final > TODAY THEN
                      ERROR "La Fecha Final no puede ser mayor al dia de hoy"
                      NEXT FIELD fecha_final
                   END IF
                   IF fecha_inicio > fecha_final THEN
                      ERROR "La Fecha Inicial no puede ser mayor a la F.Final"
                      NEXT FIELD fecha_final
                   END IF
                   IF tipo NOT MATCHES "[RTDF]" THEN
                      ERROR "Digite correctamente el Tipo de Documento"
                      NEXT FIELD tipo
                   END IF

                   LET ban = 0
                   EXIT INPUT

               ON KEY (CONTROL-C, INTERRUPT)
                   LET ban = 1
                  EXIT INPUT
         END INPUT


         IF ban = 1 THEN
            PROMPT "PROCESO CANCELADO...<ENTER> PARA CONTINUAR"
                   FOR enter
            RETURN
         END IF

         ERROR "Generando Informacion....."
         SLEEP 2

    CLOSE WINDOW INTB08013

    LET G_LISTA = param_ruta.ruta_listados CLIPPED,
                 "/",
                 g_usuario CLIPPED,
                 ".CORRES_CONSUL_",
                 TODAY USING "ddmmyy",
                 ".txt"
   START REPORT gen_rep TO G_LISTA
   DECLARE ap_porc CURSOR FOR
       SELECT a.tipo_con, a.nss,a.fecha_traspaso, a.constancia,
              a.fec_acuse, a.folio
       FROM   int_center_porce a
       WHERE  a.factualiza = fecha_con
       ORDER BY 1
       LET i = 0
   FOREACH ap_porc INTO con_por.*
        IF STATUS = NOTFOUND THEN
           ERROR "NO EXISTE INFORMACION....."
           SLEEP 3
        END IF


        LET pos = TRUE
        LET i = i + 1
        LET l_record[i].indica_a        = con_por.indica_a
        LET l_record[i].nss             = con_por.nss
        LET l_record[i].folio           = con_por.folio
        LET l_record[i].tip_sol         = con_por.tip_sol
        LET l_record[i].factualiza      = con_por.factualiza
        LET l_record[i].nombre          = con_por.paterno CLIPPED ," ",
                                          con_por.materno CLIPPED, " ",
                                          con_por.nombre
        OUTPUT TO REPORT gen_rep(con_por.*)

   END FOREACH


   FINISH REPORT gen_rep

   IF pos = TRUE THEN
     LET HOY = TODAY

     CALL SET_COUNT(i)
     OPEN WINDOW v3 AT 5,2 WITH FORM "INTB08064" ATTRIBUTE(BORDER)
     DISPLAY " INTB0806               CONSULTA EL PORCENTAJE C",
             "ARGADO                         " AT 3,1 ATTRIBUTE(REVERSE)

     DISPLAY " [CTRL-C] Salir                                 ",
             "           [CTRL-P]Imprime     " AT 1,1 ATTRIBUTE(REVERSE)

     DISPLAY HOY USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

     DISPLAY  ARRAY  l_record TO scr_1.*

           ON KEY(CONTROL-P)
              LET ejecuta = "lp ",G_LISTA CLIPPED
              RUN ejecuta

           ON KEY(CONTROL-C,INTERRUPT)
              EXIT DISPLAY

     END DISPLAY

     CLOSE WINDOW v3
   ELSE
      ERROR "No existe informacion....."
      SLEEP 3
   END IF

END FUNCTION


REPORT gen_rep(pide)
   DEFINE  pide   RECORD
                indica_a   INTEGER,
                nss        CHAR(11),
                folio      DECIMAL(08,0),
                tip_sol    SMALLINT,
                paterno    CHAR(40),
                materno    CHAR(40),
                nombre     CHAR(40),
                factualiza DATE
           END RECORD,

           nombre              CHAR(30)

     OUTPUT
          TOP MARGIN 1
          BOTTOM MARGIN 0
          LEFT MARGIN   0
          RIGHT MARGIN  0
          PAGE LENGTH  60
     FORMAT
          PAGE HEADER
             PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'

             PRINT COLUMN 23," REPORTE DEL PORCENTAJE DE REGISTROS "
             PRINT COLUMN 001,"Prog : INTB0806",
                   COLUMN 081,TODAY USING "dd/mm/yyyy"
             PRINT COLUMN 01,"----------------------------------------",
                   COLUMN 41,"----------------------------------------",
                   COLUMN 81,"----------"
             PRINT COLUMN 01,"N. S. S.   ",
                   COLUMN 16,"FOLIO",
                   COLUMN 30,"TIP.SOL.",
                   COLUMN 42,"N O M B R E ",
##                   COLUMN 77,"F.OPERACION"
                   COLUMN 77,"F.CARGA"

             PRINT COLUMN 16,"DIAGNOSTICO"

             PRINT COLUMN 16,"DIAGNOSTICO"
             PRINT COLUMN 01,"----------------------------------------",
                   COLUMN 41,"----------------------------------------",
                   COLUMN 81,"----------"
             SKIP 1 LINE

     ON EVERY ROW

             INITIALIZE nombre TO NULL

             LET nombre = pide.paterno CLIPPED," ",
                          pide.materno CLIPPED," ",
                          pide.nombre  CLIPPED

             PRINT COLUMN 01,pide.nss,
                   COLUMN 16,pide.folio,
                   COLUMN 33,pide.tip_sol,
                   COLUMN 42,nombre,
                   COLUMN 77,pide.factualiza USING "dd/mm/yyyy"

     ON LAST ROW
          SKIP 4 LINE
             PRINT COLUMN 01,"----------------------------------------",
                   COLUMN 41,"----------------------------------------",
                   COLUMN 81,"----------"
          PRINT COLUMN 01," Total de registros : ",COUNT(*) USING "<<<<<<<"
END REPORT
