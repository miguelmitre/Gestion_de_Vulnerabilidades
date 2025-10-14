##############################################################################
#Proyecto            => SISTEMA DE safre_af( MEXICO )
#Propietario         => E.F.P.
#Programa INTB0802   => GENERA ARCHIVO DE RESULTADO DE CORRESPONDENCIA
#         Tipo de Constancia:      T.-ESTADO DE CUENTA (CEDENTE)
#                                  R.-NOTIFICACION DE TRASPASO (RECEPTORA)
#                                  F.-TRANSF. DE SIEFORES
#                                  A.-NOTIFICACION DE AFILIACION
#Por                 => LAURA EUGENIA CORTES GUZMAN
#Fecha creacion      => 19 DE OCTUBRE DEL 2004
#Modificado por      => LAURA EUGENIA CORTES GUZMAN
#Fecha Ult. Modif.   => 28 DE OCTUBRE DEL 2004
#Actualizacion       => GENERA LAYOUT DE "RESULTADOS DE CORRESPONDENCIA"
#Sistema             => INT
###############################################################################
DATABASE safre_af
GLOBALS
DEFINE cod_afo         SMALLINT
DEFINE raz_social      CHAR(50)
DEFINE v_ruta          RECORD LIKE seg_modulo.*
DEFINE HOY             DATE
DEFINE hoy1            DATE
DEFINE f_traspaso      DATE
DEFINE f_envio         DATE
DEFINE enter           CHAR(01)
DEFINE reg_cza_01        RECORD
            tipo_registro     CHAR(02),
            ident_de_serv     CHAR(02),
            ident_de_oper     CHAR(02),
            tipo_de_ent_orig  CHAR(02),
            cve_ent_orig      CHAR(03),
            tipo_ent_destino  CHAR(02),
            cve_ent_destino   CHAR(03),
            ent_fed_env_lote  CHAR(03),
            fecha_presen      CHAR(08),
            consec_lote_dia   CHAR(03),
            cve_mod_recep     CHAR(02),
            cod_res_oper      CHAR(02),
            motivo_rech_lote  CHAR(09)
       END RECORD
DEFINE reg_cza_02        RECORD
            tipo_registro     CHAR(02),
            ident_de_serv     CHAR(02),
            ident_de_oper     CHAR(02),
            tipo_de_ent_orig  CHAR(02),
            cve_ent_orig      CHAR(03),
            tipo_ent_destino  CHAR(02),
            cve_ent_destino   CHAR(03),
            ent_fed_env_lote  CHAR(03),
            fecha_presen      CHAR(08),
            consec_lote_dia   CHAR(03),
            cve_mod_recep     CHAR(02),
            cod_res_oper      CHAR(02),
            motivo_rech_lote  CHAR(03)
       END RECORD
DEFINE g_cza             CHAR(100)
DEFINE g_det_2           CHAR(100)
DEFINE g_det_3           CHAR(100)

DEFINE reg_det_02        RECORD
          tipo_reg          CHAR(02),
          cont_serv         INTEGER,
          tipo_ent_emi_cta  CHAR(02),
          cve_ent_emi_cta   CHAR(03),
          nss_trab_afo      CHAR(11),
          fecha_liq_trasp   DATE,
          diag_correo       SMALLINT,
          afo_rece_o_tran   CHAR(01),
          constancia        CHAR(03)
       END RECORD

DEFINE reg_det_02_a        RECORD
          tipo_reg          CHAR(02),
          cont_serv         INTEGER,
          tipo_ent_emi_cta  CHAR(02),
          cve_ent_emi_cta   CHAR(03),
          nss_trab_afo      CHAR(11),
          fecha_liq_trasp   DATE,
          diag_correo       SMALLINT
       END RECORD

DEFINE  cont_reg          INTEGER

DEFINE  reg_det_03        RECORD
        tipo_registro     CHAR(02)
                          END RECORD
DEFINE  cat               CHAR(1500)
DEFINE  permisos          CHAR(1500)
DEFINE  r_num_lote        SMALLINT
DEFINE  tipo_con          SMALLINT
DEFINE  t_constancia      SMALLINT
DEFINE  cont_T            INTEGER
DEFINE  cont_R            INTEGER
DEFINE  cont_F            INTEGER
DEFINE  cont_I            INTEGER
DEFINE  num_con_arch      CHAR(02)
DEFINE  v_ultimo          SMALLINT
DEFINE  max_env_T         SMALLINT
DEFINE  max_env_R         SMALLINT
DEFINE  max_env_F         SMALLINT

DEFINE  v_arch            CHAR(50)
DEFINE  v_det             CHAR(50)
DEFINE  v_cza             CHAR(50)
DEFINE  recibo            SMALLINT
DEFINE  con_num           INTEGER
END GLOBALS

MAIN

    OPTIONS INPUT WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT
    CALL STARTLOG ("INTB0802.log")

    CALL inicio()

    OPEN WINDOW ventana_1 AT 2,2 WITH 3 ROWS, 76 COLUMNS ATTRIBUTE(BORDER)
         MENU "GENERA ARCHIVOS "
              COMMAND "PROCESAR TRASP." 
                      "Carga archivo de Constancias 30221 y Estado de Cuenta"
                      WHENEVER ERROR CONTINUE
                            DELETE FROM int_call_center
                      WHENEVER ERROR STOP

                      CALL proceso_principal(1)
                      CLEAR SCREEN
              COMMAND "PROCESAR AFIL." 
                      "Carga archivo de Constancia 30201"
                      WHENEVER ERROR CONTINUE
                            DELETE FROM int_call_center
                      WHENEVER ERROR STOP

                      CALL proceso_principal(3)
                      CLEAR SCREEN
              COMMAND "PROCESAR NO AFIL." 
                      "Carga archivo de Constancia 30201"
                      WHENEVER ERROR CONTINUE
                            DELETE FROM int_call_center
                      WHENEVER ERROR STOP

                      CALL proceso_principal(4)
                      CLEAR SCREEN
              COMMAND "Transf. Siefores" "Carga archivo de Folletos"
                      WHENEVER ERROR CONTINUE
                            DELETE FROM int_call_center
                      WHENEVER ERROR STOP
                      CALL proceso_principal(2)
                      CLEAR SCREEN
              COMMAND "Salir " "Salir del Programa"
                      EXIT MENU
         END MENU
    CLOSE WINDOW ventana_1

END MAIN
#==============================================================================
#FUNCION DE INICIO
#==============================================================================
FUNCTION inicio()

    LET HOY = TODAY

    SELECT codigo_afore,razon_social
    INTO cod_afo,raz_social
    FROM   tab_afore_local


    SELECT *
    INTO v_ruta.*
    FROM seg_modulo
    WHERE modulo_cod = "int"

    SELECT v.nom_arch, arch_det, arch_cza
        INTO  v_arch, v_det, v_cza
        FROM  tab_layout v
        WHERE v.layout_cod = 800

    LET v_det  = v_ruta.ruta_envio CLIPPED,"/",v_det CLIPPED
    LET v_cza  = v_ruta.ruta_envio CLIPPED,"/",v_cza CLIPPED
    LET v_arch = v_ruta.ruta_envio CLIPPED,"/", v_arch CLIPPED
{

    LET v_det  = v_det CLIPPED
    LET v_cza  = v_cza CLIPPED
    LET v_arch = v_arch CLIPPED
}
END FUNCTION
#==============================================================================
#FUNCION DE PROCESO PRINCIPAL
#==============================================================================
FUNCTION proceso_principal(recibo)

    DEFINE recibo     SMALLINT
    DEFINE ban        SMALLINT

    LET ban = 0
    OPEN WINDOW INTB0802 AT 4,4 WITH FORM "INTB08021" ATTRIBUTE(BORDER)
    CASE recibo
    WHEN 1
       DISPLAY "INTB0802          GENERA ARCHIVO CORRESP.EDO.CTA.Y 3",
               "0221                       " AT 3,1 ATTRIBUTE(REVERSE)
    WHEN 2
       DISPLAY "INTB0802         GENERACION DE ARCHIVO DE TRANSF. S",
               "IEFORE                      " AT 3,1 ATTRIBUTE(REVERSE)
    WHEN 3
       DISPLAY "INTB0802      GENERA ARCHIVO DE CORRESPONDENCIA ",
               "AFILIACION                     " AT 3,1 ATTRIBUTE(REVERSE)
    WHEN 4
       DISPLAY "INTB0802    GENERA ARCHIVO DE CORRESPONDENCIA ",
               "NO AFILIACION                       " AT 3,1 ATTRIBUTE(REVERSE)
    END CASE

    DISPLAY "                             < CTRL-C > Sali",
            "r                                  " AT 1,1 ATTRIBUTE(REVERSE)


    DISPLAY HOY USING "DD-MM-YYYY" AT 3,64 ATTRIBUTE(REVERSE)

    INPUT BY NAME f_traspaso,f_envio

    BEFORE FIELD f_traspaso

        CASE recibo
        WHEN 1
          SELECT MAX(a.fecha_traspaso)
            INTO f_traspaso
            FROM int_cd_det_notifica a
           WHERE a.tipo_con  IN ( "T","R")
           AND   a.constancia IN("E3","E03","C17","C04")
           AND   a.procesar         = 0
       WHEN 2
          SELECT MAX(a.fecha_traspaso)
            INTO f_traspaso
            FROM int_cd_det_notifica a
           WHERE a.tipo_con  IN ( "F" )
           AND   a.procesar         = 0
       WHEN 3
          SELECT MAX(a.fecha_traspaso)
            INTO f_traspaso
            FROM int_cd_det_notifica a
           WHERE a.tipo_con  IN ("A")
           AND   a.constancia IN("C02","C03")
           AND   a.procesar         = 0
       WHEN 4
          SELECT MAX(a.fecha_traspaso)
            INTO f_traspaso
            FROM int_cd_det_notifica a
           WHERE a.tipo_con  IN ("I")
           AND   a.constancia IN("N01")
           AND   a.procesar         = 0
       END CASE

       IF (STATUS = NOTFOUND) THEN
           LET f_traspaso = NULL
           DISPLAY BY NAME f_traspaso
       ELSE
           DISPLAY BY NAME f_traspaso
       END IF

     AFTER FIELD f_traspaso
        IF (f_traspaso IS NULL OR f_traspaso = 0) THEN
           DISPLAY  "LA FECHA DE TRASPASO NO PUEDE SER NULA VERIFIQUE..."
                     AT 19,1
           SLEEP 3
           DISPLAY  "" AT 19,1
           NEXT FIELD f_traspaso
        ELSE
           CASE recibo
           WHEN 1
              SELECT "a.X"
                FROM int_cd_det_notifica a
               WHERE a.fecha_traspaso   = f_traspaso
                 AND a.tipo_con  IN ( "T","R" )
                 AND a.constancia IN("E3","E03","C17","C04")
                 AND a.procesar         = 0
              GROUP BY 1
           WHEN 2
              SELECT "a.X"
                FROM int_cd_det_notifica a
               WHERE a.fecha_traspaso   = f_traspaso
                 AND a.tipo_con  = "F"
                 AND a.procesar         = 0
              GROUP BY 1
           WHEN 3
              SELECT "a.X"
                FROM int_cd_det_notifica a
##               WHERE a.fecha_traspaso   = f_traspaso
               WHERE a.tipo_con  = "A"
                 AND a.constancia IN("C02","C03")
                 AND a.procesar         = 0
              GROUP BY 1
           WHEN 4
              SELECT "a.X"
                FROM int_cd_det_notifica a
##               WHERE a.fecha_traspaso   = f_traspaso
               WHERE a.tipo_con  = "I"
                 AND a.constancia IN("N01")
                 AND a.procesar         = 0
              GROUP BY 1
           END CASE

           IF (STATUS = NOTFOUND) THEN
              DISPLAY "LA FECHA DE TRASPASO QUE TECLEO ES INVALIDA VERIFIQUE..."
                      AT 19,1
              SLEEP 3
              DISPLAY "" AT 19,1
              NEXT FIELD  f_traspaso
           ELSE
              NEXT FIELD  f_envio
           END IF
        END IF

     BEFORE FIELD f_envio
        LET f_envio = NULL
        DISPLAY BY NAME f_envio

     AFTER FIELD f_envio
        IF (f_envio IS NULL OR f_envio = 0) THEN
           DISPLAY  "LA FECHA DE ENVIO DE INFORMACION ES NULA VERIFIQUE..."
                     AT 19,1
           SLEEP 3
           DISPLAY  "" AT 19,1
           NEXT FIELD f_envio
        END IF

        IF f_envio < f_traspaso THEN
           DISPLAY "LA FECHA ENVIO NO PUEDE SER MENOR A LA DE TRASP./CERTIF.   "

           AT 19,1
           SLEEP 3
           DISPLAY  "" AT 19,1
           NEXT FIELD f_envio
        END IF
        EXIT INPUT

     ON KEY(ESC)
        IF (f_traspaso IS NULL OR f_traspaso = 0) THEN
           DISPLAY  "LA FECHA DE TRASPASO NO PUEDE SER NULA VERIFIQUE..."
                     AT 19,1
           SLEEP 3
           DISPLAY  "" AT 19,1
           NEXT FIELD f_traspaso
        ELSE
           CASE recibo
           WHEN 1
              SELECT "a.X"
                FROM int_cd_det_notifica a
               WHERE a.fecha_traspaso   = f_traspaso
                 AND a.tipo_con  IN ( "T","R" )
                 AND a.constancia IN("E3","E03","C17","C04")
                 AND a.procesar         = 0
              GROUP BY 1
           WHEN 2
              SELECT "a.X"
                FROM int_cd_det_notifica a
               WHERE a.fecha_traspaso   = f_traspaso
                 AND a.tipo_con  = "F"
                 AND a.procesar         = 0
              GROUP BY 1
           WHEN 3
              SELECT "a.X"
                FROM int_cd_det_notifica a
##               WHERE a.fecha_traspaso   = f_traspaso
               WHERE a.tipo_con  = "A"
                 AND a.constancia IN("C02","C03")
                 AND a.procesar         = 0
              GROUP BY 1
           WHEN 4
              SELECT "a.X"
                FROM int_cd_det_notifica a
##               WHERE a.fecha_traspaso   = f_traspaso
               WHERE a.tipo_con  = "I"
                 AND a.constancia IN("N01")
                 AND a.procesar         = 0
              GROUP BY 1
           END CASE

           IF (STATUS = NOTFOUND) THEN
              DISPLAY "LA FECHA DE TRASPASO QUE TECLEO ES INVALIDA VERIFIQUE..."
                      AT 19,1
              SLEEP 3
              DISPLAY "" AT 19,1
              NEXT FIELD  f_traspaso
           ELSE
              NEXT FIELD  f_envio
           END IF
        END IF

        IF (f_envio IS NULL OR f_envio = 0) THEN
           DISPLAY  "LA FECHA DE ENVIO DE INFORMACION ES NULA VERIFIQUE..."
                     AT 19,1
           SLEEP 3
           DISPLAY  "" AT 19,1
           NEXT FIELD f_envio
        END IF

        IF f_envio < f_traspaso THEN
           DISPLAY "LA FECHA ENVIO NO PUEDE SER MENOR A LA DE TRASP./CERTIF.   "

           AT 19,1
           SLEEP 3
           DISPLAY  "" AT 19,1
           NEXT FIELD f_envio
        END IF
        EXIT INPUT

     ON KEY(INTERRUPT, CONTROL-C)
        PROMPT "Proceso Cancelado...<ENTER> para Salir " for char enter
        LET ban = 1
        EXIT INPUT

     END INPUT

     IF ban = 1 THEN
        CLOSE WINDOW INTB0802
        RETURN
     END IF

     WHILE TRUE
        PROMPT "Esta Seguro de Generar El Proceso [S/N] : ? " FOR enter
        IF enter MATCHES "[sSnN]" THEN
            EXIT WHILE
        END IF
     END WHILE

     IF enter MATCHES "[nN]" THEN
        PROMPT "Proceso Cancelado...<ENTER> para Salir" for char enter
        CLOSE WINDOW INTB0802
        RETURN
     END IF

    DISPLAY "PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)

    IF (f_traspaso IS NULL OR f_traspaso = "") OR
       (f_envio IS NULL OR f_envio = "") THEN
       ERROR "--NO-- LLENO TODOS LOS CAMPOS..."
       SLEEP 2
       ERROR " "
       PROMPT "Proceso Cancelado...<ENTER> para Salir" for char enter
       CLOSE WINDOW INTB0802
       RETURN
    END IF

#===============================================================================
#TRAE LA ULTIMA CARGA DE INFORMACION DE ACUERDO A LA FECHA DE TRASPASO TECLEADA
#TANTO PARA (ESTADOS DE CUENTA),COMO PARA (NOTIFICACION DE TRASPASO)

    CASE recibo
    WHEN 1
       LET max_env_T = 0
       SELECT MAX(m.n_envios)
       INTO max_env_T
       FROM int_cd_det_notifica m
       WHERE m.fecha_traspaso   = f_traspaso
         AND m.tipo_con         = "T"
         AND m.constancia       = "E03"
         AND m.procesar         = 0
       IF (max_env_T IS NULL OR max_env_T = "") THEN
          LET max_env_T = 0
       END IF

       LET max_env_R = 0

       SELECT MAX(m.n_envios)
       INTO max_env_R
       FROM int_cd_det_notifica m
       WHERE m.fecha_traspaso   = f_traspaso
         AND m.tipo_con         = "R"
         AND m.constancia       IN("C17","C04")
         AND m.procesar         = 0

       IF (max_env_R IS NULL OR max_env_R = "") THEN
          LET max_env_R = 0
       END IF

       IF max_env_T IS NOT NULL OR max_env_T <> ""  OR
          max_env_R IS NOT NULL OR max_env_R <> ""  THEN
       ELSE
          DISPLAY  "--NO EXISTE INFORMACION--..." AT 19,1
          SLEEP 3
          DISPLAY  "" AT 19,1
          CLOSE WINDOW INTB0802
          RETURN
       END IF
    WHEN 2
       LET max_env_F = 0

       SELECT MAX(m.n_envios)
       INTO max_env_F
       FROM int_cd_det_notifica m
       WHERE m.fecha_traspaso   = f_traspaso
         AND m.tipo_con         = "F"
         AND m.procesar         = 0

       IF max_env_F IS NOT NULL OR max_env_F <> ""  THEN
          LET max_env_F = 0
       ELSE

          DISPLAY  "--NO EXISTE INFORMACION--..." AT 19,1
          SLEEP 3
          DISPLAY  "" AT 19,1
          CLOSE WINDOW INTB0802
          RETURN

          LET max_env_F = 0
       END IF
    WHEN 3
              LET max_env_T = 0
              LET max_env_R = 0

              SELECT MAX(m.n_envios)
              INTO max_env_R
              FROM int_cd_det_notifica m
##       WHERE m.fecha_traspaso   = f_traspaso
              WHERE m.tipo_con         = "A"
                AND m.constancia       IN("C02","C03")
                AND m.procesar         = 0

              IF (max_env_R IS NULL OR max_env_R = "") THEN
                 LET max_env_R = 0
              END IF

              IF max_env_R IS NOT NULL OR max_env_R <> ""  THEN
              ELSE
                 DISPLAY  "--NO EXISTE INFORMACION--..." AT 19,1
                 SLEEP 3
                 DISPLAY  "" AT 19,1
                 CLOSE WINDOW INTB0802
                 RETURN
              END IF

        WHEN 4
                LET max_env_T = 0
                LET max_env_R = 0
    
                SELECT MAX(m.n_envios)
                INTO max_env_R
                FROM int_cd_det_notifica m
                WHERE m.tipo_con         = "I"
                  AND m.constancia       IN("N01")
                  AND m.procesar         = 0
    
                IF (max_env_R IS NULL OR max_env_R = "") THEN
                   LET max_env_R = 0
                END IF
    
                IF max_env_R IS NOT NULL OR max_env_R <> ""  THEN
                ELSE
                   DISPLAY  "--NO EXISTE INFORMACION--..." AT 19,1
                   SLEEP 3
                   DISPLAY  "" AT 19,1
                   CLOSE WINDOW INTB0802
                   RETURN
                END IF
    END CASE

#===============================================================================

    CASE recibo
    WHEN 1
       CALL genera_encabezado()
       CALL genera_detalle_02(1)
       CALL genera_detalle_09()

       LET cat = "cat ",v_ruta.ruta_envio CLIPPED,"/01 ",
                         v_ruta.ruta_envio CLIPPED,"/02 ",
                         v_ruta.ruta_envio CLIPPED,"/09 >",
                         v_ruta.ruta_envio CLIPPED,"/",
                         "Correspo",f_envio USING"YYYYMMDD"
       RUN cat

       LET cat = "rm ",v_ruta.ruta_envio CLIPPED,"/01 "
       RUN cat

       LET cat = "rm ",v_ruta.ruta_envio CLIPPED,"/02 "
       RUN cat

       LET cat = "rm ",v_ruta.ruta_envio CLIPPED,"/09 "
       RUN cat

       LET permisos = 'cd ',v_ruta.ruta_envio CLIPPED,';',
                       'chmod 777 ',"Correspo",f_envio USING"YYYYMMDD"
       RUN permisos

       LET v_arch   = v_ruta.ruta_envio CLIPPED,"/",
                      "correo_dev",
                      f_envio USING "YYYYMMDD" CLIPPED,
                      ".800" CLIPPED

       LET cat = "cat ",v_cza CLIPPED, " ",
                        v_det CLIPPED, " > ",
                        v_arch CLIPPED
       RUN cat

       LET permisos = 'rm ',v_cza CLIPPED, " ", v_det CLIPPED
       RUN permisos

       LET permisos = 'cd ',v_ruta.ruta_envio CLIPPED,';',
                      'chmod 777 ',v_arch CLIPPED
       RUN permisos

       DISPLAY " RUTA DE ARCHIVOS :" AT 14,1 ATTRIBUTE (REVERSE)
       DISPLAY v_ruta.ruta_envio CLIPPED   AT 14,20

       DISPLAY " ARCHIVO PROCESAR :" AT 16,1 ATTRIBUTE (REVERSE)
       DISPLAY "Correspo",f_envio USING "YYYYMMDD" AT 16,21

       DISPLAY " ARCHIVO DATOS    :" AT 17,1 ATTRIBUTE (REVERSE)
       DISPLAY "correo_dev",
               f_envio USING "YYYYMMDD",
               ".800" CLIPPED AT 17,21

    WHEN 2
       CALL genera_detalle_02(2)
       LET cat = "rm ",v_ruta.ruta_envio CLIPPED,"/02 "
       RUN cat

       LET v_arch   = v_ruta.ruta_envio CLIPPED,"/",
                      "correo_dev",
                      f_envio USING "YYYYMMDD" CLIPPED,
                      ".800" CLIPPED

       LET cat = "cat ",v_cza CLIPPED, " ",
                        v_det CLIPPED, " > ",
                        v_arch CLIPPED
       RUN cat

       LET permisos = 'rm ',v_cza CLIPPED, " ", v_det CLIPPED
       RUN permisos

       LET permisos = 'cd ',v_ruta.ruta_envio CLIPPED,';',
                      'chmod 777 ',v_arch CLIPPED
       RUN permisos

       DISPLAY " RUTA DE ARCHIVOS :" AT 14,1 ATTRIBUTE (REVERSE)
       DISPLAY v_ruta.ruta_envio CLIPPED   AT 14,20

       DISPLAY "" AT 16,1

       DISPLAY " ARCHIVO DATOS    :" AT 17,1 ATTRIBUTE (REVERSE)
       DISPLAY "correo_dev",
               f_envio USING "YYYYMMDD",
               ".800" CLIPPED AT 17,21
    WHEN 3
            CALL genera_encabezado_2()
            CALL genera_detalle_02_a(1)
            CALL genera_detalle_09_a()
     
            LET cat = "cat ",v_ruta.ruta_envio CLIPPED,"/01 ",
                              v_ruta.ruta_envio CLIPPED,"/02 ",
                              v_ruta.ruta_envio CLIPPED,"/09 >",
                              v_ruta.ruta_envio CLIPPED,"/",
                              "Correspo_afilia.",f_envio USING"YYYYMMDD"
            RUN cat
     
            LET cat = "rm ",v_ruta.ruta_envio CLIPPED,"/01 "
            RUN cat
     
            LET cat = "rm ",v_ruta.ruta_envio CLIPPED,"/02 "
            RUN cat
     
            LET cat = "rm ",v_ruta.ruta_envio CLIPPED,"/09 "
            RUN cat
     
            LET permisos = 'cd ',v_ruta.ruta_envio CLIPPED,';',
                            'chmod 777 ',"Correspo_afilia.",
                            f_envio USING"YYYYMMDD"
            RUN permisos
{
            LET v_arch   = v_ruta.ruta_envio CLIPPED,"/",
                           "correo_dev",
                           f_envio USING "YYYYMMDD" CLIPPED,
                           ".800" CLIPPED
}
            LET cat = "cat ",v_cza CLIPPED, " ",
                             v_det CLIPPED, " > ",
                             v_arch CLIPPED
            RUN cat
     
            LET permisos = 'rm ',v_cza CLIPPED, " ", v_det CLIPPED
            RUN permisos
     
            LET permisos = 'cd ',v_ruta.ruta_envio CLIPPED,';',
                           'chmod 777 ',v_arch CLIPPED
            RUN permisos
     
            DISPLAY " RUTA DE ARCHIVOS :" AT 14,1 ATTRIBUTE (REVERSE)
            DISPLAY v_ruta.ruta_envio CLIPPED   AT 14,20
     
            DISPLAY " ARCHIVO PROCESAR :" AT 16,1 ATTRIBUTE (REVERSE)
            DISPLAY "Correspo_afilia.",f_envio USING "YYYYMMDD" AT 16,21
{
            DISPLAY " ARCHIVO DATOS    :" AT 17,1 ATTRIBUTE (REVERSE)
            DISPLAY "correo_dev",
                    f_envio USING "YYYYMMDD",
                    ".800" CLIPPED AT 17,21
}
    WHEN 4
           CALL genera_encabezado_4()
           CALL genera_detalle_02_n(1)
           CALL genera_detalle_09_a()
    
           LET cat = "cat ",v_ruta.ruta_envio CLIPPED,"/01 ",
                             v_ruta.ruta_envio CLIPPED,"/02 ",
                             v_ruta.ruta_envio CLIPPED,"/09 >",
                             v_ruta.ruta_envio CLIPPED,"/",
                             "Correspo_no_afilia.",f_envio USING"YYYYMMDD"
           RUN cat
    
           LET cat = "rm ",v_ruta.ruta_envio CLIPPED,"/01 "
           RUN cat
    
           LET cat = "rm ",v_ruta.ruta_envio CLIPPED,"/02 "
           RUN cat
    
           LET cat = "rm ",v_ruta.ruta_envio CLIPPED,"/09 "
           RUN cat
    
           LET permisos = 'cd ',v_ruta.ruta_envio CLIPPED,';',
                           'chmod 777 ',"Correspo_no_afilia.",
                           f_envio USING"YYYYMMDD"
           RUN permisos
    
           LET cat = "cat ",v_cza CLIPPED, " ",
                            v_det CLIPPED, " > ",
                            v_arch CLIPPED
           RUN cat
    
           LET permisos = 'rm ',v_cza CLIPPED, " ", v_det CLIPPED
           RUN permisos
    
           LET permisos = 'cd ',v_ruta.ruta_envio CLIPPED,';',
                          'chmod 777 ',v_arch CLIPPED
           RUN permisos
    
           DISPLAY " RUTA DE ARCHIVOS :" AT 14,1 ATTRIBUTE (REVERSE)
           DISPLAY v_ruta.ruta_envio CLIPPED   AT 14,20
    
           DISPLAY " ARCHIVO PROCESAR :" AT 16,1 ATTRIBUTE (REVERSE)
           DISPLAY "Correspo_no_afilia.",f_envio USING "YYYYMMDD" AT 16,21
    END CASE
    PROMPT " PROCESO FINALIZADO <ENTER> PARA SALIR " FOR CHAR Enter

    CLOSE WINDOW INTB0802

END FUNCTION
#===============================================================================
# FUNCION QUE GENERA ENCABEZADO 01
#===============================================================================
FUNCTION genera_encabezado()

#==========DETERMINA EL NUMERO CONSECUTIVO DEL LOTE==========================
         SELECT lotes_num INTO r_num_lote FROM safre_af:tab_lote
          WHERE lotes_fecha = HOY
            AND lotes_cod   = 6

         IF (STATUS = NOTFOUND) THEN
            LET r_num_lote =  1
            INSERT INTO safre_af:tab_lote  VALUES(HOY,6,"TRASPASOS",0,1)
         ELSE
            LET r_num_lote = r_num_lote + 1
         END IF

         UPDATE tab_lote
         SET    lotes_num   =  r_num_lote
         WHERE  lotes_fecha = HOY
           AND  lotes_cod   = 6

#================FIN DEL CALCULO DE LOTE=====================================

   LET reg_cza_01.tipo_registro     = "01"
   LET reg_cza_01.ident_de_serv     = "02"
   LET reg_cza_01.ident_de_oper     = "35"
   LET reg_cza_01.tipo_de_ent_orig  = "01"
   LET reg_cza_01.cve_ent_orig      = cod_afo
   LET reg_cza_01.tipo_ent_destino  = "03"
   LET reg_cza_01.cve_ent_destino   = "001"
   LET reg_cza_01.ent_fed_env_lote  = "009"
   LET reg_cza_01.fecha_presen      = f_envio USING "YYYYMMDD"
   LET reg_cza_01.consec_lote_dia   = r_num_lote
   LET reg_cza_01.cve_mod_recep     = "02"
   LET reg_cza_01.cod_res_oper      = "  "
   LET reg_cza_01.motivo_rech_lote  = "         "


   LET g_cza = v_ruta.ruta_envio CLIPPED,"/","01"

   START REPORT listado_1 TO g_cza
      OUTPUT TO REPORT listado_1(reg_cza_01.*)
   FINISH REPORT listado_1

END FUNCTION
#===============================================================================
# REPORTE PARA ENCABEZADO
#==============================================================================
REPORT listado_1(reg_cza_01)

DEFINE reg_cza_01        RECORD
       tipo_registro     CHAR(02),
       ident_de_serv     CHAR(02),
       ident_de_oper     CHAR(02),
       tipo_de_ent_orig  CHAR(02),
       cve_ent_orig      CHAR(03),
       tipo_ent_destino  CHAR(02),
       cve_ent_destino   CHAR(03),
       ent_fed_env_lote  CHAR(03),
       fecha_presen      CHAR(08),
       consec_lote_dia   CHAR(03),
       cve_mod_recep     CHAR(02),
       cod_res_oper      CHAR(02),
       motivo_rech_lote  CHAR(09)
                         END RECORD

   OUTPUT
        PAGE LENGTH 1
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0

   FORMAT
    ON EVERY ROW
        PRINT
            COLUMN 01,reg_cza_01.tipo_registro      ,
                      reg_cza_01.ident_de_serv      ,
                      reg_cza_01.ident_de_oper      ,
                      reg_cza_01.tipo_de_ent_orig   ,
                      reg_cza_01.cve_ent_orig       ,
                      reg_cza_01.tipo_ent_destino   ,
                      reg_cza_01.cve_ent_destino USING "&&&"   ,
                      reg_cza_01.ent_fed_env_lote,
                      reg_cza_01.fecha_presen       ,
                      reg_cza_01.consec_lote_dia USING "&&&",
                      reg_cza_01.cve_mod_recep      ,
                      reg_cza_01.cod_res_oper       ,
                      reg_cza_01.motivo_rech_lote USING "#########"  ,
                      77 SPACES
END REPORT
#==============================================================================
# FUNCION QUE GENERA EL DETALLE 02
#==============================================================================
FUNCTION genera_detalle_02(recibo)

DEFINE recibo            SMALLINT

DEFINE reg_det_02        RECORD
          tipo_reg          CHAR(02),  #02   en duro
          cont_serv         INTEGER,   #en duro
          tipo_ent_emi_cta  CHAR(02),  #01   en duro
          cve_ent_emi_cta   CHAR(03),  #cve_ced_cuenta en int_cd_det_notifica
          nss_trab_afo      CHAR(11),  #nss en int_cd_det_notifica
          fecha_liq_trasp   DATE,      #fecha traspaso en int_cd_det_notifica
          diag_correo       CHAR(02),  #nota_cod en int_cd_det_notifica
          afo_rece_o_tran   CHAR(01),  #tipo_con en int_cd_det_notifica
          constancia        CHAR(03)   #constancia en int_cd_det_notifica
      END RECORD

DEFINE edo                      SMALLINT

DEFINE p_dom      RECORD
              calle             CHAR(40),
              numero            CHAR(10),
              depto             CHAR(10),
              colonia           CHAR(60),
              codigo_postal     CHAR(5),
              municip_deleg     INTEGER,
              estado            SMALLINT,
              poblacion         SMALLINT
       END RECORD,

           domi                     DECIMAL(10,0),

           calle                    CHAR(40),
           colonia                  CHAR(60),
           depto                    CHAR(10),
           desc_delega              CHAR(40),
           estado                   CHAR(40),
           poblacion                CHAR(40),
           centro_reparto           CHAR(05),
           entidad                  CHAR(04),
           numero                   CHAR(10),
           sexo                     CHAR(01),
           desc_entidad             CHAR(30),
           tel_uno                  CHAR(15),
           tel_dos                  CHAR(15),
           correo                   CHAR(30),
           descri                   CHAR(40),
           edo_genera               CHAR(01),

           p_telefono      RECORD
               telefono             CHAR(40),
               cve_lada             CHAR(03),
               tel_cod              SMALLINT
           END RECORD,

           afi             RECORD
               rfc                  CHAR(13),
               paterno              CHAR(40),
               materno              CHAR(40),
               nombres              CHAR(40),
               tipo_solicitud       SMALLINT,
               n_folio              INTEGER
           END RECORD,

           nn_folio                 CHAR(10),
           ban, long, i,no_generar  SMALLINT,
           selec_tab                CHAR(1000),
           v_rep                    CHAR(2000)

   LET cont_T   =   0
   LET cont_R   =   0

   LET reg_det_02.cont_serv         =  0

   INITIALIZE selec_tab, v_rep, reg_det_02.* TO NULL

   START REPORT r_report TO v_cza
    LET v_rep = "VERIFICACIONN DE DATOS DEL TRABAJADOR POR CORREO DEVUELTO","|",
                f_traspaso USING "YYYYMMDD", "|",
                TODAY      USING "DDMMYY"  , "|"
    OUTPUT TO REPORT r_report(v_rep, 800, 1)
    FINISH REPORT r_report

    IF recibo = 2 THEN
       LET selec_tab = "SELECT cve_ced_cuenta,nss,fecha_traspaso, ",
                      " diag_correo,tipo_con,constancia ",
                      " FROM int_cd_det_notifica ",
                      " WHERE fecha_traspaso  = '", f_traspaso,"'",
                      " AND tipo_con        = 'F' ",
                      " AND n_envios        = ", max_env_F,
                      " AND procesar        = 0 "
    ELSE
        LET selec_tab = "SELECT cve_ced_cuenta,nss,fecha_traspaso, ",
                        " diag_correo,tipo_con,constancia ",
                        " FROM int_cd_det_notifica ",
                        " WHERE fecha_traspaso  = '",f_traspaso,"'",
                        " AND tipo_con        = 'T' ",
                        " AND n_envios        = ", max_env_T,
                        " AND procesar        = 0 ",
                        " UNION ALL ",
                        " SELECT cve_ced_cuenta,nss,fecha_traspaso, ",
                        " diag_correo,tipo_con,constancia ",
                        " FROM int_cd_det_notifica ",
                        " WHERE fecha_traspaso  = '", f_traspaso,"'",
                        " AND tipo_con        = 'R' ",
                        " AND n_envios        = ", max_env_R,
                        " AND procesar        = 0 ",
                        " ORDER BY 2,5 "
    END IF

    LET g_det_2 = v_ruta.ruta_envio CLIPPED,"/","02"

    START REPORT r_report TO v_det

    IF recibo = 1 THEN
       START REPORT listado_2 TO g_det_2
    END IF

    LET no_generar                   = 0
    LET reg_det_02.cont_serv = 0

   PREPARE apt_1 FROM selec_tab
   DECLARE cur_1 CURSOR FOR apt_1
   FOREACH cur_1 INTO reg_det_02.cve_ent_emi_cta,
                      reg_det_02.nss_trab_afo,
                      reg_det_02.fecha_liq_trasp,
                      reg_det_02.diag_correo,
                      reg_det_02.afo_rece_o_tran,
                      reg_det_02.constancia

              LET no_generar                   = 0
              LET reg_det_02.tipo_reg          = "02"
              LET reg_det_02.tipo_ent_emi_cta  = "01"

              LET reg_det_02.cont_serv         = reg_det_02.cont_serv + 1

              CASE reg_det_02.afo_rece_o_tran
                 WHEN "T"
                    LET cont_T  = cont_T + 1
                 WHEN "R"
                    LET cont_R  = cont_R + 1
                 WHEN "F"
                 OTHERWISE
               END CASE

      SELECT b.n_rfc, b.paterno, b.materno, b.nombres,
             b.tipo_solicitud, b.n_folio
        INTO afi.*
        FROM afi_mae_afiliado b
       WHERE b.n_seguro = reg_det_02.nss_trab_afo

      IF STATUS <> NOTFOUND THEN

         LET nn_folio = afi.n_folio USING "&&&&&&&&&&"

            INITIALIZE tel_uno, tel_dos, correo, p_telefono.* TO NULL
            LET ban = 0

            DECLARE apt_tel CURSOR FOR
                    SELECT n.telefono, n.cve_lada, n.tel_cod
                      FROM afi_telefono n
                     WHERE n.nss      =  reg_det_02.nss_trab_afo
                     AND   n.n_folio  =  afi.n_folio
                     AND   n.tipo_solicitud =  afi.tipo_solicitud
            FOREACH apt_tel INTO p_telefono.*

                IF p_telefono.tel_cod <> 7 THEN
                   IF p_telefono.telefono[1,1] <> " " THEN
                      LET ban = ban + 1
                      CASE ban
                           WHEN 1  LET tel_uno = p_telefono.cve_lada CLIPPED,
                                                 "-",
                                                 p_telefono.telefono CLIPPED
                           WHEN 2  LET tel_dos = p_telefono.cve_lada CLIPPED,
                                                 "-",
                                                 p_telefono.telefono CLIPPED
                      END CASE
                   END IF
                END IF

                IF p_telefono.tel_cod = 7 THEN
                   LET correo = p_telefono.telefono
                END IF
            END FOREACH

            LET long = 0
            LET i    = 0
            LET long = LENGTH(tel_uno CLIPPED)
            IF long < 13 THEN
               LET long = long + 1
               FOR i = long TO 13
                   LET tel_uno[i,i] = " "
               END FOR
            END IF

            LET long = 0
            LET i    = 0
            LET long = LENGTH(tel_dos CLIPPED)
            IF long < 13 THEN
               LET long = long + 1
               FOR i = long TO 13
                   LET tel_dos[i,i] = " "
               END FOR
            END IF

            LET long = 0
            LET i    = 0
            LET long = LENGTH(correo CLIPPED)
            IF long < 30 THEN
               LET long = long + 1
               FOR i = long TO 30
                   LET correo[i,i] = " "
               END FOR
            END IF

            IF tel_dos = "             " AND
               tel_uno = "             " THEN
               LET no_generar = 0
            END IF

         LET domi = 0
         SELECT MIN(s.ROWID)
                INTO  domi
                FROM  afi_domicilio s
                WHERE s.nss            = reg_det_02.nss_trab_afo
                AND   s.n_folio        = afi.n_folio
                AND   s.tipo_solicitud = afi.tipo_solicitud

         IF SQLCA.SQLCODE = 0 THEN
            INITIALIZE p_dom.*, calle, colonia, numero, depto  TO NULL
            SELECT o.calle, o.numero, o.depto, o.colonia, o.codpos,
                   o.delega, o.estado, o.ciudad
                   INTO  p_dom.*
                   FROM  afi_domicilio o
                   WHERE o.rowid = domi

            IF p_dom.calle IS NULL OR
               p_dom.calle =  "  " THEN
               LET calle = "                                        "
            ELSE
                LET long = 0
                LET i    = 0
                LET calle    = p_dom.calle
                LET long = LENGTH(calle)
                IF long < 40 THEN
                   LET long = long + 1
                   FOR i = long TO 40
                       LET calle[i,i] = " "
                   END FOR
                END IF
            END IF

            IF p_dom.numero IS NULL OR
               p_dom.numero =  "  " THEN
               LET numero = "          "
            ELSE
                LET long = 0
                LET i    = 0
                LET numero    = p_dom.numero
                LET long = LENGTH(numero)
                IF long < 10 THEN
                   LET long = long + 1
                   FOR i = long TO 10
                       LET numero[i,i] = " "
                   END FOR
                END IF
            END IF

            IF p_dom.depto IS NULL OR
               p_dom.depto =  "  " THEN
               LET p_dom.depto = "          "
            ELSE
                LET long = 0
                LET i    = 0
                LET depto    = p_dom.depto
                LET long = LENGTH(depto)
                IF long < 10 THEN
                   LET long = long + 1
                   FOR i = long TO 10
                       LET depto[i,i] = " "
                   END FOR
                END IF
            END IF

            IF p_dom.colonia IS NULL OR
               p_dom.colonia = " "   THEN
               LET colonia = "                                        ",
                             "                    "
            ELSE
                LET long = 0
                LET i    = 0
                LET colonia = p_dom.colonia
                LET long = LENGTH(colonia)
                IF long < 60 THEN
                   LET long = long + 1
                   FOR i = long TO 60
                       LET colonia[i,i] = " "
                   END FOR
                END IF
            END IF

            IF p_dom.codigo_postal IS NULL OR
               p_dom.codigo_postal = "  "  THEN
               LET p_dom.codigo_postal = "     "
            END IF

            IF p_dom.estado IS NULL OR
               p_dom.estado =  " "  THEN
               LET estado = "                                        "
            ELSE
                SELECT p.estad_desc INTO estado
                       FROM tab_estado p
                       WHERE p.estad_cod = p_dom.estado
                 IF SQLCA.SQLCODE != 0 THEN
                    LET estado = "                                        "
                 ELSE
                      LET long = 0
                      LET i    = 0
                      LET long = LENGTH(estado)
                      IF long < 40 THEN
                         LET long = long + 1
                         FOR i = long TO 40
                             LET estado[i,i] = " "
                         END FOR
                      END IF
                 END IF
            END IF

            IF p_dom.municip_deleg IS NULL OR
               p_dom.municip_deleg =  " "  THEN
               LET desc_delega = "                                        "
            ELSE
                SELECT q.deleg_desc INTO desc_delega
                       FROM tab_delegacion q
                       WHERE q.deleg_cod = p_dom.municip_deleg
                 IF SQLCA.SQLCODE != 0 THEN
                    LET desc_delega = "                                        "
                 ELSE
                      LET long = 0
                      LET i    = 0
                      LET long = LENGTH(desc_delega)
                      IF long < 40 THEN
                         LET long = long + 1
                         FOR i = long TO 40
                             LET desc_delega[i,i] = " "
                         END FOR
                      END IF
                 END IF
            END IF

            IF p_dom.poblacion IS NULL OR
               p_dom.poblacion =  " "  THEN
               LET poblacion = "                                        "
            ELSE
                SELECT q.ciudad_desc INTO poblacion
                       FROM  tab_ciudad q
                       WHERE q.ciudad_cod = p_dom.poblacion
                IF SQLCA.SQLCODE != 0 THEN
                    LET poblacion = "                                        "
                ELSE
                      LET long = 0
                      LET i    = 0
                      LET long = LENGTH(poblacion)
                      IF long < 40 THEN
                         LET long = long + 1
                         FOR i = long TO 40
                             LET poblacion[i,i] = " "
                         END FOR
                      END IF
                END IF
            END IF
         END IF
      END IF

      SELECT g.nota_descripcion INTO descri FROM int_cd_tab_nota g
       WHERE g.diag_correo = reg_det_02.diag_correo

      LET long = 0
      LET i    = 0
      LET long = LENGTH(descri CLIPPED)
      IF long < 40 THEN
         LET long = long + 1
         FOR i = long TO 40
            LET descri[i,i] = " "
         END FOR
      END IF

      LET v_rep = reg_det_02.nss_trab_afo           ,"|",
                  nn_folio                          ,"|",
                  afi.tipo_solicitud                ,"|",
                  afi.rfc                           ,"|",
                  afi.paterno                       ,"|",
                  afi.materno                       ,"|",
                  afi.nombres                       ,"|",
                  calle                             ,"|",
                  numero                            ,"|",
                  depto                             ,"|",
                  colonia                           ,"|",
                  poblacion                         ,"|",
                  p_dom.codigo_postal               ,"|",
                  estado                            ,"|",
                  tel_uno                           ,"|",
                  tel_dos                           ,"|",
                  correo                            ,"|",
                  reg_det_02.diag_correo USING "&&" ,"|",
                  descri                            ,"|",
                  reg_det_02.constancia             ,"|"

      IF recibo = 1 THEN
         IF tel_uno <> "             " THEN
            INSERT INTO int_call_center
                   VALUES( reg_det_02.nss_trab_afo           ,
                           nn_folio                          ,
                           afi.tipo_solicitud                ,
                           afi.rfc                           ,
                           afi.paterno                       ,
                           afi.materno                       ,
                           afi.nombres                       ,
                           calle                             ,
                           numero                            ,
                           depto                             ,
                           colonia                           ,
                           poblacion                         ,
                           p_dom.codigo_postal               ,
                           estado                            ,
                           tel_uno                           ,
                           tel_dos                           ,
                           correo                            ,
                           reg_det_02.diag_correo            ,
                           descri                            ,
                           reg_det_02.constancia             ,
                           f_traspaso
                         )
    
             OUTPUT TO REPORT r_report(v_rep,800,2)

         END IF

         OUTPUT TO REPORT listado_2(reg_det_02.*)
         LET cont_reg = cont_reg + 1

      ELSE
         IF tel_uno <> "             " THEN
            INSERT INTO int_call_center
                   VALUES( reg_det_02.nss_trab_afo           ,
                           nn_folio                          ,
                           afi.tipo_solicitud                ,
                           afi.rfc                           ,
                           afi.paterno                       ,
                           afi.materno                       ,
                           afi.nombres                       ,
                           calle                             ,
                           numero                            ,
                           depto                             ,
                           colonia                           ,
                           poblacion                         ,
                           p_dom.codigo_postal               ,
                           estado                            ,
                           tel_uno                           ,
                           tel_dos                           ,
                           correo                            ,
                           reg_det_02.diag_correo            ,
                           descri                            ,
                           reg_det_02.constancia             ,
                           f_traspaso
                         )
    
             OUTPUT TO REPORT r_report(v_rep,800,2)
             LET cont_F  = cont_F + 1
         END IF
         LET cont_reg = cont_reg + 1
      END IF

      UPDATE int_cd_det_notifica
         SET fec_procesar = TODAY,
             procesar     = 1
      WHERE  nss          = reg_det_02.nss_trab_afo
      AND    constancia     = reg_det_02.constancia    
      AND    fecha_traspaso = reg_det_02.fecha_liq_trasp
      AND    procesar       = 0
    

      END FOREACH

      DISPLAY "TOTAL DE REGISTROS GENERADOS:  ",cont_reg USING "##,##&" AT 09,14

      IF recibo = 1 THEN
         DISPLAY "TIPO DE CONSTANCIA" AT 10,22
         DISPLAY "T" AT 10,41 ATTRIBUTE (REVERSE)
         DISPLAY ": "," ",cont_T USING "##,##&"," REGS" CLIPPED AT 10,42

         DISPLAY "TIPO DE CONSTANCIA" AT 12,22
         DISPLAY "R" AT 12,41 ATTRIBUTE (REVERSE)
         DISPLAY ": "," ",cont_R USING "##,##&"," REGS" CLIPPED AT 12,42
      ELSE
         DISPLAY "TIPO DE CONSTANCIA" AT 12,22
         DISPLAY "F" AT 12,41 ATTRIBUTE (REVERSE)
         DISPLAY ": "," ",cont_F USING "##,##&"," REGS CON TELEF." CLIPPED AT 12,42
      END IF

   FINISH REPORT r_report

   IF recibo = 1 THEN
      FINISH REPORT listado_2
   END IF

END FUNCTION
#==============================================================================
# REPORTE PARA EL DETALLE 02
#==============================================================================
REPORT listado_2(reg_det_02)
DEFINE reg_det_02        RECORD
       tipo_reg          CHAR(02),      #02   en duro
       cont_serv         INTEGER,       #en duro
       tipo_ent_emi_cta  CHAR(02),      #01   en duro
       cve_ent_emi_cta   CHAR(03),      #cve_ced_cuenta en int_cd_det_notifica
       nss_trab_afo      CHAR(11),      #nss en int_cd_det_notifica
       fecha_liq_trasp   DATE,          #fecha traspaso en int_cd_det_notifica
       diag_correo    CHAR(02),      #nota_cod en int_cd_det_notifica
       afo_rece_o_tran   CHAR(01),      #tipo_constancia en int_cd_det_notifica
       constancia        CHAR(03)
      END RECORD
DEFINE fecha_liq_tra     INTEGER

    OUTPUT
        PAGE LENGTH 1
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW
      LET fecha_liq_tra  = reg_det_02.fecha_liq_trasp USING "YYYYMMDD"

       PRINT
          COLUMN 01,reg_det_02.tipo_reg          USING "&&"             ,
                    reg_det_02.cont_serv         USING "&&&&&&&&&&"     ,
                    reg_det_02.tipo_ent_emi_cta  USING "&&"             ,
                    reg_det_02.cve_ent_emi_cta   USING "&&&"            ,
                    reg_det_02.nss_trab_afo      USING "&&&&&&&&&&&"    ,
                    fecha_liq_tra   USING "&&&&&&&&",
                    reg_det_02.diag_correo    USING "&&",
                    reg_det_02.afo_rece_o_tran             ,
                    81 SPACES
END REPORT
#===============================================================================
# FUNCION QUE GENERA SUMARIO 09
#===============================================================================
FUNCTION genera_detalle_09()

DEFINE  reg_det_03        RECORD
        tipo_registro     CHAR(02)
                          END RECORD
   LET g_det_3 = v_ruta.ruta_envio CLIPPED,"/","09"
   START REPORT listado_3 TO g_det_3
      LET reg_det_03.tipo_registro = "09"

      OUTPUT TO REPORT listado_3 (reg_det_03.*)

   FINISH REPORT listado_3

END FUNCTION
#==============================================================================
# REPORTE PARA SUMARIO  09
#==============================================================================
REPORT listado_3(reg_det_03)
DEFINE  reg_det_03        RECORD
        tipo_registro     CHAR(02)
                          END RECORD

   OUTPUT
        PAGE LENGTH 1
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0

   FORMAT
    ON EVERY ROW
        PRINT
            COLUMN 01,reg_det_03.tipo_registro   USING "&&"    ,
                      cont_reg  USING "&&&&&&&&&"              ,
                      109 SPACES


END REPORT
#===============================================================================
# FUNCION QUE GENERA ENCABEZADO 02
#===============================================================================
FUNCTION genera_encabezado_2()

#==========DETERMINA EL NUMERO CONSECUTIVO DEL LOTE==========================
         SELECT lotes_num INTO r_num_lote FROM safre_af:tab_lote
          WHERE lotes_fecha = HOY
            AND lotes_cod   = 0

         IF (STATUS = NOTFOUND) THEN
            LET r_num_lote =  1
            INSERT INTO safre_af:tab_lote  VALUES(HOY,0,"AFILIACION",0,1)
         ELSE
            LET r_num_lote = r_num_lote + 1
         END IF

         UPDATE tab_lote
         SET    lotes_num   =  r_num_lote
         WHERE  lotes_fecha = HOY
           AND  lotes_cod   = 0

#================FIN DEL CALCULO DE LOTE=====================================

   LET reg_cza_02.tipo_registro     = "01"
   LET reg_cza_02.ident_de_serv     = "01"
   LET reg_cza_02.ident_de_oper     = "35"
   LET reg_cza_02.tipo_de_ent_orig  = "01"
   LET reg_cza_02.cve_ent_orig      = cod_afo
   LET reg_cza_02.tipo_ent_destino  = "03"
   LET reg_cza_02.cve_ent_destino   = "001"
   LET reg_cza_02.ent_fed_env_lote  = "009"
   LET reg_cza_02.fecha_presen      = f_envio USING "YYYYMMDD"
   LET reg_cza_02.consec_lote_dia   = r_num_lote
   LET reg_cza_02.cve_mod_recep     = "02"
   LET reg_cza_02.cod_res_oper      = "  "
   LET reg_cza_02.motivo_rech_lote  = "   "


   LET g_cza = v_ruta.ruta_envio CLIPPED,"/","01"

   START REPORT listado_1a TO g_cza
      OUTPUT TO REPORT listado_1a(reg_cza_02.*)
   FINISH REPORT listado_1a

END FUNCTION
#===============================================================================
# REPORTE PARA ENCABEZADO
#==============================================================================
REPORT listado_1a(reg_cza_02)

DEFINE reg_cza_02        RECORD
           tipo_registro     CHAR(02),
           ident_de_serv     CHAR(02),
           ident_de_oper     CHAR(02),
           tipo_de_ent_orig  CHAR(02),
           cve_ent_orig      CHAR(03),
           tipo_ent_destino  CHAR(02),
           cve_ent_destino   CHAR(03),
           ent_fed_env_lote  CHAR(03),
           fecha_presen      CHAR(08),
           consec_lote_dia   CHAR(03),
           cve_mod_recep     CHAR(02),
           cod_res_oper      CHAR(02),
           motivo_rech_lote  CHAR(03)
       END RECORD

   OUTPUT
        PAGE LENGTH 1
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0

   FORMAT
    ON EVERY ROW
        PRINT
            COLUMN 01,reg_cza_02.tipo_registro      ,
                      reg_cza_02.ident_de_serv      ,
                      reg_cza_02.ident_de_oper      ,
                      reg_cza_02.tipo_de_ent_orig   ,
                      reg_cza_02.cve_ent_orig       ,
                      reg_cza_02.tipo_ent_destino   ,
                      reg_cza_02.cve_ent_destino USING "&&&"   ,
                      reg_cza_02.ent_fed_env_lote,
                      reg_cza_02.fecha_presen       ,
                      reg_cza_02.consec_lote_dia USING "&&&",
                      reg_cza_02.cve_mod_recep      ,
                      reg_cza_02.cod_res_oper       ,
                      reg_cza_02.motivo_rech_lote   ,
                      83 SPACES
END REPORT
#===============================================================================
# FUNCION QUE GENERA ENCABEZADO 4 No Afiliados
#===============================================================================
FUNCTION genera_encabezado_4()

#==========DETERMINA EL NUMERO CONSECUTIVO DEL LOTE==========================
         SELECT lotes_num INTO r_num_lote FROM safre_af:tab_lote
          WHERE lotes_fecha = HOY
            AND lotes_cod   = 0

         IF (STATUS = NOTFOUND) THEN
            LET r_num_lote =  1
            INSERT INTO safre_af:tab_lote  VALUES(HOY,0,"NO AFILIACION",0,1)
         ELSE
            LET r_num_lote = r_num_lote + 1
         END IF

         UPDATE tab_lote
         SET    lotes_num   =  r_num_lote
         WHERE  lotes_fecha = HOY
           AND  lotes_cod   = 0

#================FIN DEL CALCULO DE LOTE=====================================

   LET reg_cza_02.tipo_registro     = "01"
   LET reg_cza_02.ident_de_serv     = "01"
   LET reg_cza_02.ident_de_oper     = "35"
   LET reg_cza_02.tipo_de_ent_orig  = "01"
   LET reg_cza_02.cve_ent_orig      = cod_afo
   LET reg_cza_02.tipo_ent_destino  = "03"
   LET reg_cza_02.cve_ent_destino   = "001"
   LET reg_cza_02.ent_fed_env_lote  = "009"
   LET reg_cza_02.fecha_presen      = f_envio USING "YYYYMMDD"
   LET reg_cza_02.consec_lote_dia   = r_num_lote
   LET reg_cza_02.cve_mod_recep     = "02"
   LET reg_cza_02.cod_res_oper      = "  "
   LET reg_cza_02.motivo_rech_lote  = "   "


   LET g_cza = v_ruta.ruta_envio CLIPPED,"/","01"

   START REPORT listado_1n TO g_cza
      OUTPUT TO REPORT listado_1n(reg_cza_02.*)
   FINISH REPORT listado_1n

END FUNCTION
#===============================================================================
# REPORTE PARA ENCABEZADO
#==============================================================================
REPORT listado_1n(reg_cza_04)

DEFINE reg_cza_04        RECORD
           tipo_registro     CHAR(02),
           ident_de_serv     CHAR(02),
           ident_de_oper     CHAR(02),
           tipo_de_ent_orig  CHAR(02),
           cve_ent_orig      CHAR(03),
           tipo_ent_destino  CHAR(02),
           cve_ent_destino   CHAR(03),
           ent_fed_env_lote  CHAR(03),
           fecha_presen      CHAR(08),
           consec_lote_dia   CHAR(03),
           cve_mod_recep     CHAR(02),
           cod_res_oper      CHAR(02),
           motivo_rech_lote  CHAR(03)
       END RECORD

   OUTPUT
        PAGE LENGTH 1
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0

   FORMAT
    ON EVERY ROW
        PRINT
            COLUMN 01,reg_cza_04.tipo_registro      ,
                      reg_cza_04.ident_de_serv      ,
                      reg_cza_04.ident_de_oper      ,
                      reg_cza_04.tipo_de_ent_orig   ,
                      reg_cza_04.cve_ent_orig       ,
                      reg_cza_04.tipo_ent_destino   ,
                      reg_cza_04.cve_ent_destino USING "&&&"   ,
                      reg_cza_04.ent_fed_env_lote,
                      reg_cza_04.fecha_presen       ,
                      reg_cza_04.consec_lote_dia USING "&&&",
                      reg_cza_04.cve_mod_recep      ,
                      88 SPACES
END REPORT
#==============================================================================
# FUNCION QUE GENERA EL DETALLE 02
#==============================================================================
FUNCTION genera_detalle_02_a(recibo)

DEFINE recibo            SMALLINT

DEFINE reg_det_02_a        RECORD
          tipo_reg          CHAR(02),  #02   en duro
          cont_serv         INTEGER,   #en duro
          tipo_ent_emi_cta  CHAR(02),  #01   en duro
          cve_ent_emi_cta   CHAR(03),  #cve_ced_cuenta en int_cd_det_notifica
          nss_trab_afo      CHAR(11),  #nss en int_cd_det_notifica
          fecha_certifica   DATE,      #fecha Cetrificacion del Registro
          diag_correo       CHAR(02),  #nota_cod en int_cd_det_notifica
          afo_rece_o_tran   CHAR(01),  #tipo_con en int_cd_det_notifica
          constancia        CHAR(03),
          folio             INTEGER 
      END RECORD

DEFINE p_dom      RECORD
              calle             CHAR(40),
              numero            CHAR(10),
              depto             CHAR(10),
              colonia           CHAR(60),
              codigo_postal     CHAR(5),
              municip_deleg     INTEGER,
              estado            SMALLINT,
              poblacion         SMALLINT
       END RECORD,

           domi                     DECIMAL(10,0),

           calle                    CHAR(40),
           colonia                  CHAR(60),
           depto                    CHAR(10),
           desc_delega              CHAR(40),
           estado                   CHAR(40),
           poblacion                CHAR(40),
           centro_reparto           CHAR(05),
           entidad                  CHAR(04),
           numero                   CHAR(10),
           sexo                     CHAR(01),
           desc_entidad             CHAR(30),
           tel_uno                  CHAR(15),
           tel_dos                  CHAR(15),
           correo                   CHAR(30),
           descri                   CHAR(40),
           edo_genera               CHAR(01),

           p_telefono      RECORD
               telefono             CHAR(40),
               cve_lada             CHAR(03),
               tel_cod              SMALLINT
           END RECORD,

           afi             RECORD
               rfc                  CHAR(13),
               paterno              CHAR(40),
               materno              CHAR(40),
               nombres              CHAR(40),
               tipo_solicitud       SMALLINT,
               n_folio              INTEGER
           END RECORD,

           nn_folio                 CHAR(10),
           ban, long, i,no_generar  SMALLINT,
           ss_w                     SMALLINT,
           selec_tab                CHAR(1000),
           v_rep                    CHAR(2000)

   LET cont_R   =   0
   LET ban      =   0
   LET long     =   0
   LET i        =   0
   LET ss_w     =   0

   LET reg_det_02_a.cont_serv         =  0

   INITIALIZE selec_tab, v_rep, reg_det_02_a.* TO NULL

        LET selec_tab = " SELECT nss,fecha_traspaso, ",
                        " diag_correo,tipo_con,constancia, folio ",
                        " FROM int_cd_det_notifica ",
##                        " WHERE fecha_traspaso  = '", f_traspaso,"'",
                        " WHERE tipo_con        = 'A' ",
##                        " AND n_envios        = ", max_env_R,
                        " AND procesar        = 0 ",
                        " AND constancia      IN('C02','C03')",
                        " ORDER BY 2,5 "

    LET g_det_2 = v_ruta.ruta_envio CLIPPED,"/","02"

      START REPORT listado_2a TO g_det_2

    LET no_generar                   = 0

   PREPARE apt_1a FROM selec_tab
   DECLARE cur_1a CURSOR FOR apt_1a
   FOREACH cur_1a INTO reg_det_02_a.nss_trab_afo,
                      reg_det_02_a.fecha_certifica,
                      reg_det_02_a.diag_correo,
                      reg_det_02_a.afo_rece_o_tran,
                      reg_det_02_a.constancia,
                      reg_det_02_a.folio

              INITIALIZE v_rep TO NULL

              LET no_generar                   = 0
              LET reg_det_02_a.tipo_reg          = "02"
              LET reg_det_02_a.tipo_ent_emi_cta  = "01"

              IF ss_w = 0 THEN
                 LET reg_det_02_a.cont_serv      = 1
                 LET ss_w = 1
              ELSE
                 LET reg_det_02_a.cont_serv      = reg_det_02_a.cont_serv + 1
              END IF

              LET con_num = reg_det_02_a.cont_serv

              CASE reg_det_02_a.afo_rece_o_tran
                 WHEN "T"
                    LET cont_T  = cont_T + 1
                 WHEN "A"
                    LET cont_R  = cont_R + 1
                 WHEN "F"
                 OTHERWISE
               END CASE

      SELECT b.n_rfc, b.paterno, b.materno, b.nombres,
             b.tipo_solicitud, b.n_folio
        INTO afi.*
        FROM afi_mae_afiliado b
       WHERE b.n_seguro = reg_det_02_a.nss_trab_afo
       AND   b.tipo_solicitud = 1

      IF STATUS <> NOTFOUND THEN

         LET nn_folio = afi.n_folio USING "&&&&&&&&&&"

            INITIALIZE tel_uno, tel_dos, correo, p_telefono.* TO NULL
            LET ban = 0

            DECLARE apt_tel_2 CURSOR FOR
                    SELECT n.telefono, n.cve_lada, n.tel_cod
                      FROM afi_telefono n
                     WHERE n.nss      =  reg_det_02_a.nss_trab_afo
                     AND   n.n_folio  =  afi.n_folio
                     AND   n.tipo_solicitud =  afi.tipo_solicitud
            FOREACH apt_tel_2 INTO p_telefono.*

                IF p_telefono.tel_cod <> 7 THEN
                   IF p_telefono.telefono[1,1] <> " " THEN
                      LET ban = ban + 1
                      CASE ban
                           WHEN 1  LET tel_uno = p_telefono.cve_lada CLIPPED,
                                                 "-",
                                                 p_telefono.telefono CLIPPED
                           WHEN 2  LET tel_dos = p_telefono.cve_lada CLIPPED,
                                                 "-",
                                                 p_telefono.telefono CLIPPED
                      END CASE
                   END IF
                END IF

                IF p_telefono.tel_cod = 7 THEN
                   LET correo = p_telefono.telefono
                END IF
            END FOREACH

            LET long = 0
            LET i    = 0
            LET long = LENGTH(tel_uno CLIPPED)
            IF long < 13 THEN
               LET long = long + 1
               FOR i = long TO 13
                   LET tel_uno[i,i] = " "
               END FOR
            END IF

            LET long = 0
            LET i    = 0
            LET long = LENGTH(tel_dos CLIPPED)
            IF long < 13 THEN
               LET long = long + 1
               FOR i = long TO 13
                   LET tel_dos[i,i] = " "
               END FOR
            END IF

            LET long = 0
            LET i    = 0
            LET long = LENGTH(correo CLIPPED)
            IF long < 30 THEN
               LET long = long + 1
               FOR i = long TO 30
                   LET correo[i,i] = " "
               END FOR
            END IF

            IF tel_dos = "             " AND
               tel_uno = "             " THEN
               LET no_generar = 0
            END IF

         LET domi = 0
         SELECT MIN(s.ROWID)
                INTO  domi
                FROM  afi_domicilio s
                WHERE s.nss            = reg_det_02_a.nss_trab_afo
                AND   s.n_folio        = afi.n_folio
                AND   s.tipo_solicitud = afi.tipo_solicitud

         IF SQLCA.SQLCODE = 0 THEN
            INITIALIZE p_dom.*, calle, colonia, numero, depto  TO NULL
            SELECT o.calle, o.numero, o.depto, o.colonia, o.codpos,
                   o.delega, o.estado, o.ciudad
                   INTO  p_dom.*
                   FROM  afi_domicilio o
                   WHERE o.rowid = domi

            IF p_dom.calle IS NULL OR
               p_dom.calle =  "  " THEN
               LET calle = "                                        "
            ELSE
                LET long = 0
                LET i    = 0
                LET calle    = p_dom.calle
                LET long = LENGTH(calle)
                IF long < 40 THEN
                   LET long = long + 1
                   FOR i = long TO 40
                       LET calle[i,i] = " "
                   END FOR
                END IF
            END IF

            IF p_dom.numero IS NULL OR
               p_dom.numero =  "  " THEN
               LET numero = "          "
            ELSE
                LET long = 0
                LET i    = 0
                LET numero    = p_dom.numero
                LET long = LENGTH(numero)
                IF long < 10 THEN
                   LET long = long + 1
                   FOR i = long TO 10
                       LET numero[i,i] = " "
                   END FOR
                END IF
            END IF

            IF p_dom.depto IS NULL OR
               p_dom.depto =  "  " THEN
               LET p_dom.depto = "          "
            ELSE
                LET long = 0
                LET i    = 0
                LET depto    = p_dom.depto
                LET long = LENGTH(depto)
                IF long < 10 THEN
                   LET long = long + 1
                   FOR i = long TO 10
                       LET depto[i,i] = " "
                   END FOR
                END IF
            END IF

            IF p_dom.colonia IS NULL OR
               p_dom.colonia = " "   THEN
               LET colonia = "                                        ",
                             "                    "
            ELSE
                LET long = 0
                LET i    = 0
                LET colonia = p_dom.colonia
                LET long = LENGTH(colonia)
                IF long < 60 THEN
                   LET long = long + 1
                   FOR i = long TO 60
                       LET colonia[i,i] = " "
                   END FOR
                END IF
            END IF

            IF p_dom.codigo_postal IS NULL OR
               p_dom.codigo_postal = "  "  THEN
               LET p_dom.codigo_postal = "     "
            END IF

            IF p_dom.estado IS NULL OR
               p_dom.estado =  " "  THEN
               LET estado = "                                        "
            ELSE
                SELECT p.estad_desc INTO estado
                       FROM tab_estado p
                       WHERE p.estad_cod = p_dom.estado
                 IF SQLCA.SQLCODE != 0 THEN
                    LET estado = "                                        "
                 ELSE
                      LET long = 0
                      LET i    = 0
                      LET long = LENGTH(estado)
                      IF long < 40 THEN
                         LET long = long + 1
                         FOR i = long TO 40
                             LET estado[i,i] = " "
                         END FOR
                      END IF
                 END IF
            END IF

            IF p_dom.municip_deleg IS NULL OR
               p_dom.municip_deleg =  " "  THEN
               LET desc_delega = "                                        "
            ELSE
                SELECT q.deleg_desc INTO desc_delega
                       FROM tab_delegacion q
                       WHERE q.deleg_cod = p_dom.municip_deleg
                 IF SQLCA.SQLCODE != 0 THEN
                    LET desc_delega = "                                        "
                 ELSE
                      LET long = 0
                      LET i    = 0
                      LET long = LENGTH(desc_delega)
                      IF long < 40 THEN
                         LET long = long + 1
                         FOR i = long TO 40
                             LET desc_delega[i,i] = " "
                         END FOR
                      END IF
                 END IF
            END IF

            IF p_dom.poblacion IS NULL OR
               p_dom.poblacion =  " "  THEN
               LET poblacion = "                                        "
            ELSE
                SELECT q.ciudad_desc INTO poblacion
                       FROM  tab_ciudad q
                       WHERE q.ciudad_cod = p_dom.poblacion
                IF SQLCA.SQLCODE != 0 THEN
                    LET poblacion = "                                        "
                ELSE
                      LET long = 0
                      LET i    = 0
                      LET long = LENGTH(poblacion)
                      IF long < 40 THEN
                         LET long = long + 1
                         FOR i = long TO 40
                             LET poblacion[i,i] = " "
                         END FOR
                      END IF
                END IF
            END IF
         END IF
      END IF

      SELECT g.nota_descripcion INTO descri FROM int_cd_tab_nota g
       WHERE g.diag_correo = reg_det_02_a.diag_correo

      LET long = 0
      LET i    = 0
      LET long = LENGTH(descri CLIPPED)
      IF long < 40 THEN
         LET long = long + 1
         FOR i = long TO 40
            LET descri[i,i] = " "
         END FOR
      END IF

      LET cont_reg = cont_reg + 1

      OUTPUT TO REPORT listado_2a(reg_det_02_a.*)

      UPDATE int_cd_det_notifica
      SET    procesar = 1,
             fec_procesar = TODAY
      WHERE  folio = reg_det_02_a.folio
      AND    nss   = reg_det_02_a.nss_trab_afo
      AND    procesar = 0

      END FOREACH

      DISPLAY "TOTAL DE REGISTROS GENERADOS:  ",cont_reg USING "##,##&" AT 09,14


         DISPLAY "TIPO DE CONSTANCIA" AT 12,22
         DISPLAY "A" AT 12,41 ATTRIBUTE (REVERSE)
         DISPLAY ": "," ",cont_R USING "##,##&"," REGS" CLIPPED AT 12,42

--   FINISH REPORT r_report

     FINISH REPORT listado_2a

END FUNCTION
#==============================================================================
# REPORTE PARA EL DETALLE 02
#==============================================================================
REPORT listado_2a(reg_det_02_a)
DEFINE reg_det_02_a        RECORD
          tipo_reg          CHAR(02),  #02   en duro
          cont_serv         INTEGER,   #en duro
          tipo_ent_emi_cta  CHAR(02),  #01   en duro
          cve_ent_emi_cta   CHAR(03),  #cve_ced_cuenta en int_cd_det_notifica
          nss_trab_afo      CHAR(11),  #nss en int_cd_det_notifica
          fecha_certifica   DATE,      #fecha Cetrificacion del Registro
          diag_correo       CHAR(02),  #nota_cod en int_cd_det_notifica
          afo_rece_o_tran   CHAR(01),  #tipo_con en int_cd_det_notifica
          constancia        CHAR(03),
          folio             INTEGER 
      END RECORD
DEFINE fecha_certi     INTEGER

    OUTPUT
        PAGE LENGTH 1
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW
      LET fecha_certi  = reg_det_02_a.fecha_certifica USING "YYYYMMDD"

       PRINT
          COLUMN 01,reg_det_02_a.tipo_reg          USING "&&"             ,
                    reg_det_02_a.cont_serv         USING "&&&&&&&&&&"     ,
                    reg_det_02_a.tipo_ent_emi_cta  USING "&&"             ,
                    cod_afo                        USING "&&&"            ,                                              
                    reg_det_02_a.nss_trab_afo      USING "&&&&&&&&&&&"    ,
                    reg_det_02_a.fecha_certifica   USING "YYYYMMDD"   ,
                    reg_det_02_a.diag_correo       USING "&&",
                    82 SPACES
END REPORT
#==============================================================================
# FUNCION QUE GENERA EL DETALLE 02 No Afiliados
#==============================================================================
FUNCTION genera_detalle_02_N(recibo)

DEFINE recibo            SMALLINT

DEFINE reg_det_02_n        RECORD
          tipo_reg          CHAR(02),  #02   en duro
          cont_serv         INTEGER,   #en duro
          tipo_ent_emi_cta  CHAR(02),  #01   en duro
          cve_ent_emi_cta   CHAR(03),  #cve_ced_cuenta en int_cd_det_notifica
          nss_trab_afo      CHAR(11),  #nss en int_cd_det_notifica
          fecha_certifica   DATE,      #fecha Cetrificacion del Registro
          diag_correo       CHAR(02),  #nota_cod en int_cd_det_notifica
          afo_rece_o_tran   CHAR(01),  #tipo_con en int_cd_det_notifica
          constancia        CHAR(03),
          folio             INTEGER 
      END RECORD

DEFINE p_dom      RECORD
              calle             CHAR(40),
              numero            CHAR(10),
              depto             CHAR(10),
              colonia           CHAR(60),
              codigo_postal     CHAR(5),
              municip_deleg     INTEGER,
              estado            SMALLINT,
              poblacion         SMALLINT
       END RECORD,

           domi                     DECIMAL(10,0),

           calle                    CHAR(40),
           colonia                  CHAR(60),
           depto                    CHAR(10),
           desc_delega              CHAR(40),
           estado                   CHAR(40),
           poblacion                CHAR(40),
           centro_reparto           CHAR(05),
           entidad                  CHAR(04),
           numero                   CHAR(10),
           sexo                     CHAR(01),
           desc_entidad             CHAR(30),
           tel_uno                  CHAR(15),
           tel_dos                  CHAR(15),
           correo                   CHAR(30),
           descri                   CHAR(40),
           edo_genera               CHAR(01),

           p_telefono      RECORD
               telefono             CHAR(40),
               cve_lada             CHAR(03),
               tel_cod              SMALLINT
           END RECORD,

           afi_n             RECORD
               rfc                  CHAR(13),
               curp                 CHAR(18),
               paterno              CHAR(40),
               materno              CHAR(40),
               nombres              CHAR(40),
               tipo_solicitud       SMALLINT,
               n_folio              DECIMAL(10,0)
           END RECORD,

           nn_folio                 CHAR(10),
           ban, long, i,no_generar  SMALLINT,
           ss_w                     SMALLINT,
           selec_tab                CHAR(1000),
           v_rep                    CHAR(2000)

   LET cont_R   =   0
   LET cont_I   =   0
   LET ban      =   0
   LET long     =   0
   LET i        =   0
   LET ss_w     =   0

   LET reg_det_02_n.cont_serv         =  0

   INITIALIZE selec_tab, v_rep, reg_det_02_n.* TO NULL

        LET selec_tab = " SELECT nss,fecha_traspaso, ",
                        " diag_correo,tipo_con,constancia, folio ",
                        " FROM int_cd_det_notifica ",
                        " WHERE tipo_con        = 'I' ",
                        " AND procesar        = 0 ",
                        " AND constancia      IN('N01')",
                        " ORDER BY 2,5 "

    LET g_det_2 = v_ruta.ruta_envio CLIPPED,"/","02"

      START REPORT listado_2N TO g_det_2

    LET no_generar                   = 0

   PREPARE apt_1n FROM selec_tab
   DECLARE cur_1n CURSOR FOR apt_1n
   FOREACH cur_1n INTO reg_det_02_n.nss_trab_afo,
                       reg_det_02_n.fecha_certifica,
                       reg_det_02_n.diag_correo,
                       reg_det_02_n.afo_rece_o_tran,
                       reg_det_02_n.constancia,
                       reg_det_02_n.folio

              INITIALIZE v_rep TO NULL

              LET no_generar                   = 0
              LET reg_det_02_n.tipo_reg          = "02"
              LET reg_det_02_n.tipo_ent_emi_cta  = "01"

              IF ss_w = 0 THEN
                 LET reg_det_02_n.cont_serv      = 1
                 LET ss_w = 1
              ELSE
                 LET reg_det_02_n.cont_serv      = reg_det_02_n.cont_serv + 1
              END IF

              LET con_num = reg_det_02_n.cont_serv


              CASE reg_det_02_n.afo_rece_o_tran
                 WHEN "T"
                    LET cont_T  = cont_T + 1
                 WHEN "A"
                    LET cont_R  = cont_R + 1
                 WHEN "I"
                    LET cont_I  = cont_I + 1
                 WHEN "F"
                 OTHERWISE
               END CASE

      SELECT b.n_rfc, b.n_unico, b.paterno, b.materno, b.nombres,
             b.tipo_solicitud, b.n_folio
        INTO afi_n.*
        FROM afi_mae_afiliado b
       WHERE b.n_seguro = reg_det_02_n.nss_trab_afo
       AND   b.tipo_solicitud = 8

      IF STATUS <> NOTFOUND THEN

         LET nn_folio = afi_n.n_folio USING "&&&&&&&&&&"

            INITIALIZE tel_uno, tel_dos, correo, p_telefono.* TO NULL
            LET ban = 0

            DECLARE apt_tel_2n CURSOR FOR
                    SELECT n.telefono, n.cve_lada, n.tel_cod
                      FROM afi_telefono n
                     WHERE n.nss      =  reg_det_02_n.nss_trab_afo
                     AND   n.n_folio  =  afi_n.n_folio
                     AND   n.tipo_solicitud =  afi_n.tipo_solicitud
            FOREACH apt_tel_2n INTO p_telefono.*

                IF p_telefono.tel_cod <> 7 THEN
                   IF p_telefono.telefono[1,1] <> " " THEN
                      LET ban = ban + 1
                      CASE ban
                           WHEN 1  LET tel_uno = p_telefono.cve_lada CLIPPED,
                                                 "-",
                                                 p_telefono.telefono CLIPPED
                           WHEN 2  LET tel_dos = p_telefono.cve_lada CLIPPED,
                                                 "-",
                                                 p_telefono.telefono CLIPPED
                      END CASE
                   END IF
                END IF

                IF p_telefono.tel_cod = 7 THEN
                   LET correo = p_telefono.telefono
                END IF
            END FOREACH

            LET long = 0
            LET i    = 0
            LET long = LENGTH(tel_uno CLIPPED)
            IF long < 13 THEN
               LET long = long + 1
               FOR i = long TO 13
                   LET tel_uno[i,i] = " "
               END FOR
            END IF

            LET long = 0
            LET i    = 0
            LET long = LENGTH(tel_dos CLIPPED)
            IF long < 13 THEN
               LET long = long + 1
               FOR i = long TO 13
                   LET tel_dos[i,i] = " "
               END FOR
            END IF

            LET long = 0
            LET i    = 0
            LET long = LENGTH(correo CLIPPED)
            IF long < 30 THEN
               LET long = long + 1
               FOR i = long TO 30
                   LET correo[i,i] = " "
               END FOR
            END IF

            IF tel_dos = "             " AND
               tel_uno = "             " THEN
               LET no_generar = 0
            END IF

         LET domi = 0
         SELECT MIN(s.ROWID)
                INTO  domi
                FROM  afi_domicilio s
                WHERE s.nss            = reg_det_02_n.nss_trab_afo
                AND   s.n_folio        = afi_n.n_folio
                AND   s.tipo_solicitud = afi_n.tipo_solicitud
                AND   s.marca_envio    = "X"

         IF SQLCA.SQLCODE = 0 THEN
            INITIALIZE p_dom.*, calle, colonia, numero, depto  TO NULL
            SELECT o.calle, o.numero, o.depto, o.colonia, o.codpos,
                   o.delega, o.estado, o.ciudad
                   INTO  p_dom.*
                   FROM  afi_domicilio o
                   WHERE o.rowid = domi

            IF p_dom.calle IS NULL OR
               p_dom.calle =  "  " THEN
               LET calle = "                                        "
            ELSE
                LET long = 0
                LET i    = 0
                LET calle    = p_dom.calle
                LET long = LENGTH(calle)
                IF long < 40 THEN
                   LET long = long + 1
                   FOR i = long TO 40
                       LET calle[i,i] = " "
                   END FOR
                END IF
            END IF

            IF p_dom.numero IS NULL OR
               p_dom.numero =  "  " THEN
               LET numero = "          "
            ELSE
                LET long = 0
                LET i    = 0
                LET numero    = p_dom.numero
                LET long = LENGTH(numero)
                IF long < 10 THEN
                   LET long = long + 1
                   FOR i = long TO 10
                       LET numero[i,i] = " "
                   END FOR
                END IF
            END IF

            IF p_dom.depto IS NULL OR
               p_dom.depto =  "  " THEN
               LET p_dom.depto = "          "
            ELSE
                LET long = 0
                LET i    = 0
                LET depto    = p_dom.depto
                LET long = LENGTH(depto)
                IF long < 10 THEN
                   LET long = long + 1
                   FOR i = long TO 10
                       LET depto[i,i] = " "
                   END FOR
                END IF
            END IF

            IF p_dom.colonia IS NULL OR
               p_dom.colonia = " "   THEN
               LET colonia = "                                        ",
                             "                    "
            ELSE
                LET long = 0
                LET i    = 0
                LET colonia = p_dom.colonia
                LET long = LENGTH(colonia)
                IF long < 60 THEN
                   LET long = long + 1
                   FOR i = long TO 60
                       LET colonia[i,i] = " "
                   END FOR
                END IF
            END IF

            IF p_dom.codigo_postal IS NULL OR
               p_dom.codigo_postal = "  "  THEN
               LET p_dom.codigo_postal = "     "
            END IF

            IF p_dom.estado IS NULL OR
               p_dom.estado =  " "  THEN
               LET estado = "                                        "
            ELSE
                SELECT p.estad_desc INTO estado
                       FROM tab_estado p
                       WHERE p.estad_cod = p_dom.estado
                 IF SQLCA.SQLCODE != 0 THEN
                    LET estado = "                                        "
                 ELSE
                      LET long = 0
                      LET i    = 0
                      LET long = LENGTH(estado)
                      IF long < 40 THEN
                         LET long = long + 1
                         FOR i = long TO 40
                             LET estado[i,i] = " "
                         END FOR
                      END IF
                 END IF
            END IF

            IF p_dom.municip_deleg IS NULL OR
               p_dom.municip_deleg =  " "  THEN
               LET desc_delega = "                                        "
            ELSE
                SELECT q.deleg_desc INTO desc_delega
                       FROM tab_delegacion q
                       WHERE q.deleg_cod = p_dom.municip_deleg
                 IF SQLCA.SQLCODE != 0 THEN
                    LET desc_delega = "                                        "
                 ELSE
                      LET long = 0
                      LET i    = 0
                      LET long = LENGTH(desc_delega)
                      IF long < 40 THEN
                         LET long = long + 1
                         FOR i = long TO 40
                             LET desc_delega[i,i] = " "
                         END FOR
                      END IF
                 END IF
            END IF

            IF p_dom.poblacion IS NULL OR
               p_dom.poblacion =  " "  THEN
               LET poblacion = "                                        "
            ELSE
                SELECT q.ciudad_desc INTO poblacion
                       FROM  tab_ciudad q
                       WHERE q.ciudad_cod = p_dom.poblacion
                IF SQLCA.SQLCODE != 0 THEN
                    LET poblacion = "                                        "
                ELSE
                      LET long = 0
                      LET i    = 0
                      LET long = LENGTH(poblacion)
                      IF long < 40 THEN
                         LET long = long + 1
                         FOR i = long TO 40
                             LET poblacion[i,i] = " "
                         END FOR
                      END IF
                END IF
            END IF
         END IF
      END IF

      SELECT g.nota_descripcion INTO descri FROM int_cd_tab_nota g
       WHERE g.diag_correo = reg_det_02_n.diag_correo

      LET long = 0
      LET i    = 0
      LET long = LENGTH(descri CLIPPED)
      IF long < 40 THEN
         LET long = long + 1
         FOR i = long TO 40
            LET descri[i,i] = " "
         END FOR
      END IF

      LET cont_reg = cont_reg + 1

      OUTPUT TO REPORT listado_2N(reg_det_02_n.*,afi_n.curp)

      UPDATE int_cd_det_notifica
      SET    procesar = 1,
             fec_procesar = TODAY
      WHERE  folio = reg_det_02_n.folio
      AND    nss   = reg_det_02_n.nss_trab_afo
      AND    procesar = 0

      END FOREACH

      DISPLAY "TOTAL DE REGISTROS GENERADOS:  ",cont_reg USING "##,##&" AT 09,14

      DISPLAY "TIPO DE CONSTANCIA" AT 12,22
      DISPLAY "I" AT 12,41 ATTRIBUTE (REVERSE)
      DISPLAY ": "," ",cont_I USING "##,##&"," REGS" CLIPPED AT 12,42

     FINISH REPORT listado_2N

END FUNCTION
#==============================================================================
# REPORTE PARA EL DETALLE 02 NO AFILIADO
#==============================================================================
REPORT listado_2N(reg_det_02_n,curp)
DEFINE reg_det_02_n        RECORD
          tipo_reg          CHAR(02),  #02   en duro
          cont_serv         INTEGER,   #en duro
          tipo_ent_emi_cta  CHAR(02),  #01   en duro
          cve_ent_emi_cta   CHAR(03),  #cve_ced_cuenta en int_cd_det_notifica
          nss_trab_afo      CHAR(11),  #nss en int_cd_det_notifica
          fecha_certifica   DATE,      #fecha Cetrificacion del Registro
          diag_correo       CHAR(02),  #nota_cod en int_cd_det_notifica
          afo_rece_o_tran   CHAR(01),  #tipo_con en int_cd_det_notifica
          constancia        CHAR(03),
          folio             INTEGER 
      END RECORD,

      curp                  CHAR(18),

      fecha_certi           INTEGER

    OUTPUT
        PAGE LENGTH 1
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0

    FORMAT
    ON EVERY ROW
      LET fecha_certi  = reg_det_02_n.fecha_certifica USING "YYYYMMDD"

       PRINT
          COLUMN 01,reg_det_02_n.tipo_reg          USING "&&"         ,
                    reg_det_02_n.cont_serv         USING "&&&&&&&&&&" ,
                    reg_det_02_n.tipo_ent_emi_cta  USING "&&"         ,
                    cod_afo                        USING "&&&"            ,                                              
                    curp,
                    reg_det_02_n.fecha_certifica   USING "YYYYMMDD"   ,
                    reg_det_02_n.diag_correo       USING "&&",
                    75 SPACES
END REPORT

#===============================================================================
# FUNCION QUE GENERA SUMARIO 09
#===============================================================================
FUNCTION genera_detalle_09_a()

DEFINE  reg_det_03a        RECORD
             tipo_registro     CHAR(02)
        END RECORD

   LET g_det_3 = v_ruta.ruta_envio CLIPPED,"/","09"

   START REPORT listado_3a TO g_det_3
      LET reg_det_03a.tipo_registro = "09"

      OUTPUT TO REPORT listado_3a (reg_det_03a.*,con_num)

   FINISH REPORT listado_3a

END FUNCTION
#==============================================================================
# REPORTE PARA SUMARIO  09
#==============================================================================
REPORT listado_3a(reg_det_03a,con_num)
DEFINE  reg_det_03a        RECORD
           tipo_registro     CHAR(02)
        END RECORD,

        con_num            INTEGER

   OUTPUT
        PAGE LENGTH 1
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0

   FORMAT
    ON EVERY ROW
        PRINT
            COLUMN 01,reg_det_03a.tipo_registro   USING "&&"    ,
                      con_num  USING "&&&&&&&&&"              ,
                      109 SPACES


END REPORT
