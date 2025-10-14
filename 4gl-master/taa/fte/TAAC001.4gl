###############################################################################
#Proyecto          => AFORE ( MEXICO )                                        #
#Propietario       => E.F.P.                                                  #
#Programa TAAC001  => RECIBE ARCHIVO SALDOS TRASPASO AFORE - AFORE            #
#                  => AFORE RECEPTORA                                         #
#Fecha creacion    => 31 DE ENERO DE 2001                                     #
#Autor             => MAURO MUNIZ CABALLERO                                   #
#Sistema           => TRA                                                     #
#Fecha actualiza   => 19 DE JULIO DE 2004                                     #
#Autor             => MAURO MUNIZ CABALLERO                                   #
#   Adecuaciones para circular 28-8 y participaciones                         #
#Fecha actualiza   => 3 DE DIEMBRE DE 2004                                    #
#Autor             => MAURO MUNIZ CABALLERO                                   #
#   Adecuaciones para circular 28-9 ( multisiefores )                         #
#Fecha actualiza   => 11 DE FEBRERO 2008                                      #
#Autor             => JOSUE LISANDRO HUERTA SIERRA                            #
#Ultima Actualiza  => 17 MAYO 2011                                            #
#   Adecuaciones para circular 69-2 ( multisiefores )                         #
#     Fecha Actualiza => 05-Mayo 2011  -- Alejandro Chagoya Salazar           #
#     TRASPASO POR CURP --Se generan 3 reportes nuevos                        #
#Modificacion         => JCPV 01/06/2011                                      #
#REQ:1323(1193)       => JCPV 27/08/2012 ADD tt 73                            #
# INV-2246            => 23/07/2013 se inhibe validacion de marca rech= 2V    #
#Req:1038             => JCPV 22/11/2012 Apertura de cuentas certificadas.    #
#CPL-1507             =>FSR 11/02/2014 Se agrega historico de solicitudes     #
#                      para aquellos trabajadores que hayan sido recibido     #
#                      mas de una vez y sean indebidos (21 y 73)              #
############################################################################# #
# fecha Actuali   => CPL-1982 22/05/2015                                      #
# Autor           => Cristian  Morales Roblero                                #
# Actualizacion   => Se hace la validacion desde el estado 70 para los tipos  #
#                    71                                                       #
#Req CPL-2089  => CMR 12/11/2015 se hace la correccion de la long del prompt  #
#Req CPL-2609  => DMR 21/06/2017 se pasan tablas temporales a fisicas para    #
#              => guardar las fechas de largo plazo y perspectiva largo plazo #
#                                                                             #
#Fecha modifica   => 15 de enero de 2020                                      #
#Autor            => Mauro Muñiz Caballero                                    #
#                    Verificación de registros que validan solicitudes        #
#                    de afiliación con status_interno 100 y registros de      #
#                    ident_operacion 12 traspasos complementarios             #
###############################################################################
DATABASE safre_af
  GLOBALS
    DEFINE bnd_proceso       SMALLINT
    DEFINE k                 SMALLINT
    DEFINE j                 SMALLINT

    DEFINE id_op             INTEGER
    DEFINE bandera           INTEGER
    DEFINE total             INTEGER
    DEFINE aceptados         INTEGER
    DEFINE rechazo           INTEGER
    DEFINE cuantos           INTEGER
    DEFINE s_codigo_afore    INTEGER
    DEFINE g_tip_solic       INTEGER
    DEFINE tot_total         INTEGER
    DEFINE tot_total_tt      INTEGER
    DEFINE tot_reg_rch       INTEGER
    DEFINE posicion          INTEGER
    DEFINE tot_reg_cgdos     INTEGER
    DEFINE tot_reg_acepts    INTEGER
    DEFINE tot_reg_rechs     INTEGER
    DEFINE vtot_cod_rech     INTEGER
    DEFINE key               INTEGER

    DEFINE value             CHAR
    DEFINE enter             CHAR(01)
    DEFINE g_tipo_arch       CHAR(02)
    DEFINE vbandera          CHAR(03)
    DEFINE vnum_cod_rech     CHAR(03)
    DEFINE fecha_cabeza      CHAR(08)
    DEFINE fecha_vivienda    CHAR(08)
    DEFINE fecha_solic       CHAR(08)
    DEFINE fecha_accion      CHAR(08)
    DEFINE f_lplazo          CHAR(08)
    DEFINE f_p_lplazo        CHAR(08)
    DEFINE fecha_red_bono    CHAR(08)
    DEFINE fecha_red_bono_bd CHAR(08)
    DEFINE v_siefore         CHAR(08)
    DEFINE g_usuario         CHAR(08)
    DEFINE v_hora            CHAR(08)
    DEFINE fecha_red_bono_10 CHAR(10)
    DEFINE fecha_red_bono_bd_10 CHAR(10)
    DEFINE fecha_cza_10      CHAR(10)
    DEFINE fecha_viv_10      CHAR(10)
    DEFINE fecha_solic_10    CHAR(10)
    DEFINE fecha_acc_10      CHAR(10)
    DEFINE f_lplazo_10       CHAR(10)
    DEFINE f_p_lplazo_10     CHAR(10)
    DEFINE g_busca_nss       CHAR(11)
    DEFINE generar           CHAR(20)
    DEFINE g_desc_tipo_arch  CHAR(35)
    DEFINE vdes_cod_rech     CHAR(50)
    DEFINE g_x               CHAR(50)
    DEFINE g_lista           CHAR(100)
    DEFINE g_lista_s         CHAR(100)
    DEFINE COMANDO           CHAR(100)
    DEFINE query_rch         CHAR(420)
    DEFINE v_sql             CHAR(300) --CPL3059
    DEFINE exe_fnacimiento   CHAR(300)
    DEFINE exq_sql3          CHAR(300)
    DEFINE archivo_traspaso  CHAR(500)

    DEFINE v_hoy             DATE

    DEFINE g_param_taa       RECORD LIKE seg_modulo.*

    #Las tablas TEMPORALES anteriores se dejan FIJAS #-MLM-3842 CPL-2609
    #020901 Encabezado Traspaso de Cuenta
    #021201 Encabezado Traspaso de Cuenta
    DEFINE reg_cza_tra_afo   RECORD LIKE safre_tmp:cza_tra_afo.*
   
    #020902 Detalle de Traspaso de Cuenta (Vivienda)
    #021202 Detalle de Traspaso de Cuenta (Vivienda)
    DEFINE reg_det_tra_viv   RECORD LIKE safre_tmp:det_tra_viv.*

    #020905 Detalle de Traspaso de Cuenta (RCV)
    #021205 Detalle de Traspaso de Cuenta (RCV)
    DEFINE reg_det_tra_rcv   RECORD LIKE safre_tmp:det_tra_rcv.*

    #020909 Sumario de Traspaso de Cuenta
    #021209 Sumario de Traspaso de Cuenta
    DEFINE reg_sum_tra_afo   RECORD LIKE safre_tmp:sum_tra_afo.*

    DEFINE v_ind_det_tra_rcv RECORD
      nom_archivo            CHAR(20),
      curp                   CHAR(18),
      nss                    CHAR(11),
      apaterno               CHAR(40),
      amaterno               CHAR(40),
      nombres                CHAR(40),
      fecha_recep_sol        DATE,
      cve_siefore_oss        SMALLINT,
      cve_siefore_desc       CHAR(08),
      f_aplica_oss           DATE,
      perfil_actual          SMALLINT,
      id_ind_vinculacion     SMALLINT,
      id_ind_aport_vol       SMALLINT,
      estado_reg             SMALLINT,
      cve_cod_rech           CHAR(03),
      fecha_alta             DATE,
      usuario_alta           CHAR(20)
    END RECORD
    #--FIN BLOQUE   MLM-3842 CPL-2609

    DEFINE datos             RECORD
      cve_cede               CHAR(03),
      des_afore              CHAR(25),
      tipo_trasp             CHAR(02),
      tot_parcial            INTEGER
    END RECORD

    DEFINE datos_tt          RECORD
      tipo_trasp             CHAR(02),
      des_tt                 CHAR(25),
      tot_parcial            INTEGER
    END RECORD

    DEFINE g_afore           RECORD LIKE tab_afore_local.*

    DEFINE reg_bat           RECORD
      pid                    INTEGER,
      proceso_cod            INTEGER,
      opera_cod              INTEGER,
      nombre_archivo         CHAR(25)
    END RECORD

    DEFINE arr_pant_rech     ARRAY[15000] OF RECORD
      n_seguro               CHAR(11),
      nombre                 CHAR(50),
      tipo_traspaso          CHAR(2),
      cve_ced_cuenta         CHAR(3),
      cve_cod_rech           CHAR(03)
    END RECORD

    DEFINE r_control         RECORD LIKE taa_ctr_traspaso.*
    DEFINE p_tablayout       RECORD LIKE safre_af:tab_layout.*
    DEFINE p_tablayout5      RECORD LIKE safre_af:tab_layout.*
    DEFINE p_tabcampo        RECORD LIKE safre_af:tab_campo.*
    DEFINE v_t_cza           SMALLINT
    DEFINE v_t_det2          SMALLINT
    DEFINE v_t_det5          SMALLINT
    DEFINE v_t_sum           SMALLINT
    DEFINE g_id_operacion    SMALLINT
    DEFINE v_id_operacion    CHAR(02)
    DEFINE g_f_pres_enc      CHAR(08)
    DEFINE g_f_presentacion  DATE
    DEFINE g_taa_ind_op09    RECORD
      nom_archivo            CHAR(20),
      curp                   CHAR(18),
      nss                    CHAR(11),
      ap_paterno             CHAR(40),
      ap_materno             CHAR(40),
      nombre                 CHAR(40),
      f_recepcion_sol        DATE,
      cve_siefore_oss        SMALLINT,
      cve_siefore_desc       CHAR(08),
      f_aplica_oss           DATE,
      perfil_actual          SMALLINT,
      ind_auto_vinc_am       SMALLINT,
      ind_apo_vol_ded        SMALLINT,
      edo_registro           SMALLINT,
      cve_cod_rech           CHAR(03),
      f_alta                 DATE,
      usuario_alta           CHAR(20)
    END RECORD

    DEFINE v_tot_reg_cza     SMALLINT
    DEFINE v_tot_reg_det2    DECIMAL(10,0)
    DEFINE v_tot_reg_det5    DECIMAL(10,0)
    DEFINE v_tot_reg_sum     SMALLINT

    DEFINE campo             RECORD
      cod_afore              SMALLINT,
      raz_social             CHAR(50),
      des_titulo             CHAR(23)
    END RECORD
END GLOBALS

MAIN
  --DISPLAY " "
  --DISPLAY ".1"

  CALL STARTLOG(FGL_GETENV("USER")||".TAAC001.LOG")
  CALL inicio()

  IF NOT bnd_proceso THEN
     DEFER INTERRUPT
       OPTIONS
       INPUT WRAP,
       PROMPT LINE LAST  ,
       ACCEPT KEY CONTROL-I
       CALL ventana_menu()
  ELSE
     CALL sube_archivo()
     CALL validacion_previa()
     CALL carga_tablas_tmp()     #OP13 PLUS no hay validacion, recupera n_seguro
     CALL lee_archivo_plano()    #OP13 PLUS LA VALIDACION ESTA IMPLICITA
     CALL actualiza_operacion()  #OP13 PLUS N/A
     CALL control_archivo()      #OP13 PLUS N/A
     CALL imprime_reporte()
     CALL genera_rep_sief_subc() ##ACS Mayo2011
     CALL genera_rep_siefore()   ##ACS Mayo2011
     CALL genera_rep_subcuenta() ##ACS Mayo2011
     CALL genera_rep_tipo_tra()
  END IF
END MAIN

FUNCTION inicio()
  LET reg_bat.pid            = ARG_VAL(1)
  LET reg_bat.proceso_cod    = ARG_VAL(2)
  LET reg_bat.opera_cod      = ARG_VAL(3)
  LET reg_bat.nombre_archivo = ARG_VAL(4)
  LET g_x                    = ' '
  LET bnd_proceso            = 0

  IF reg_bat.pid THEN
     DISPLAY "INICIANDO PROCESO"
     LET bnd_proceso = 1
  END IF

  WHENEVER ERROR CONTINUE --1
    DATABASE safre_tmp
    DROP TABLE tmp_pla_tras_af1
  WHENEVER ERROR STOP     --1

  CREATE TABLE tmp_pla_tras_af1 (n_registros  CHAR(730))

  CALL fn_borra_crea_tablas()

  DATABASE safre_af

  LET v_hoy  = TODAY
  LET v_hora = TIME

  SELECT *, USER
  INTO   g_param_taa.*, g_usuario
  FROM   seg_modulo
  WHERE  modulo_cod = 'taa'

  SELECT codigo_afore
  INTO   s_codigo_afore
  FROM   tab_afore_local

  WHENEVER ERROR CONTINUE  --2
    DROP TABLE tab_cod_rech
    CREATE TEMP TABLE tab_cod_rech(num_cod_rech     CHAR(03),
                                   des_cod_rech     CHAR(50),
                                   tot_cod_rech     INTEGER)
  WHENEVER ERROR STOP  --2 

  IF s_codigo_afore = 568 THEN --CPL
     INSERT INTO tab_cod_rech values("1V", "NSS VIV NO EXISTE SOLICITUD c/STATUS 50,70", 0)
     INSERT INTO tab_cod_rech values("1VM", "NSS VIV NO EXISTE EN EL MAESTRO c/STATUS 50,70", 0) #1038
  ELSE
     INSERT INTO tab_cod_rech values("1V", "NSS VIV NO EXISTE EN EL MTRO c/STATUS 50,70", 0)
  END IF

  INSERT INTO tab_cod_rech values("2V", "NSS VIV NO TIENE MARCA ACTUAL", 0)
  INSERT INTO tab_cod_rech values("3V", "NSS VIV NO EXISTE EN EL MTRO OP.12 ", 0) #1193
  INSERT INTO tab_cod_rech values("4U", "NSS UNI NO EXISTE EN EL MTRO", 0)

  IF s_codigo_afore = 568 THEN --CPL
     INSERT INTO tab_cod_rech values("5R", "NSS RCV NO EXISTE SOLICITUD c/STATUS 50,70", 0)
     INSERT INTO tab_cod_rech values("5RM", "NSS RCV NO EXISTE EN EL MAESTRO c/STATUS 50,70", 0) #1038
  ELSE
     INSERT INTO tab_cod_rech values("5R", "NSS RCV NO EXISTE EN EL MTRO OP.09 c/STATUS 50,70", 0)
  END IF

  INSERT INTO tab_cod_rech values("6R", "NSS RCV NO TIENE MARCA ACTUAL OP.09", 0)
  INSERT INTO tab_cod_rech values("7S", "SIEFORE RCV NO EXISTE", 0)
  INSERT INTO tab_cod_rech values("8D", "Dif. en Saldos VIV 97 sumario vs detalle", 0)
  INSERT INTO tab_cod_rech values("9D", "Dif. en Saldos VIV 92 sumario vs detalle", 0)
  INSERT INTO tab_cod_rech values("10D", "Dif. en Saldos ISSSTE 92 sumario vs detalle", 0)
  INSERT INTO tab_cod_rech values("11D", "Dif. en Saldos ISSSTE 08 sumario vs detalle", 0)
  INSERT INTO tab_cod_rech values("12D", "Dif. en Saldos Impt. BONO sumario vs detalle", 0)
  INSERT INTO tab_cod_rech values("13D", "Dif. en Saldos Subcta. RCV sumario vs detalle", 0)

  -- CPL-3053
  LET v_sql = " EXECUTE PROCEDURE marca_cuenta ( ?,?,?,?,?,?,?,? )"

  SELECT b.*
  INTO   p_tablayout.*
  FROM   safre_af:tab_layout b
  WHERE  b.layout_cod = 700
  AND    b.modulo_cod = "taa"

  SELECT b.*
  INTO   p_tablayout5.*
  FROM   safre_af:tab_layout b
  WHERE  b.layout_cod = 701
  AND    b.modulo_cod = "taa"

  LET v_t_cza  = 1
  LET v_t_det2 = 2
  LET v_t_det5 = 5
  LET v_t_sum  = 9

  SELECT a.codigo_afore, a.razon_social, b.afore_desc
  INTO   campo.cod_afore, campo.raz_social, campo.des_titulo
  FROM   tab_afore_local a, tab_afore b
  WHERE  a.codigo_afore = b.afore_cod
END FUNCTION       #inicio()

FUNCTION proceso_principal()
  DEFINE bnd_esc             SMALLINT

  LET bnd_esc = FALSE

  OPEN WINDOW ventana_1 AT 4,4 WITH FORM "TAAC0011" ATTRIBUTE(BORDER)
    DISPLAY " TAAC001       ARCHIVO DE SALDOS DE TRASPASOS AFORE RECEPTORA              " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "     <Enter y ESC.> Procesar                         < Ctrl-C > Salir      " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY v_hoy USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    INPUT BY NAME generar
      BEFORE INPUT
        SELECT 'X'
        FROM   taa_ctr_traspaso
        WHERE  ini_incorpora IS NOT NULL
        AND    ini_incorpora <> DATETIME (1899-12-31) YEAR TO DAY
        AND    fin_incorpora IS NULL
        GROUP BY 1
        IF SQLCA.SQLCODE = 0 THEN
           PROMPT "PROCESO DE INCORPORACION A HISTORICOS EJECUTANDOSE,[Enter] p/salir" FOR enter
           LET bnd_esc = TRUE
           EXIT INPUT
        END IF

      AFTER FIELD generar
        IF generar IS NULL THEN
           ERROR "Campo NO puede ser NULO"
           SLEEP 3
           ERROR ""
           NEXT FIELD generar
        ELSE
           LET reg_bat.nombre_archivo = generar

           SELECT 'X'
           FROM   taa_ctr_traspaso
           WHERE  nombre_archivo = generar
           GROUP BY 1
           IF SQLCA.SQLCODE = 0 THEN
              PROMPT "ARCHIVO YA FUE CARGADO,[Enter] p/salir" FOR enter
              LET bnd_esc = TRUE
              EXIT INPUT
           END IF

           CALL sube_archivo()
           EXIT INPUT
        END IF

        ON KEY (INTERRUPT)
           ERROR "PROCESO CANCELADO" ATTRIBUTE(REVERSE)
           SLEEP 2
           ERROR""
           LET bnd_esc = TRUE
           EXIT INPUT
    END INPUT

    IF bnd_esc THEN
       LET bnd_esc = FALSE
       CLOSE WINDOW ventana_1
       RETURN
    END IF

    ERROR "PROCESANDO INFORMACION"

    CALL validacion_previa()     #OP13 PLUS N/A
    CALL carga_tablas_tmp()      #OP13 PLUS no hay validacion, recupera n_seguro
    CALL lee_archivo_plano()     #OP13 PLUS LA VALIDACION ESTA IMPLICITA
    CALL imprime_reporte()
    CALL genera_rep_sief_subc()  ##ACS Mayo2011
    CALL genera_rep_siefore()    ##ACS Mayo2011
    CALL genera_rep_subcuenta()  ##ACS Mayo2011
    CALL genera_rep_tipo_tra()
    CALL control_archivo()

    ERROR ""
    PROMPT "Presione <enter> para finalizar " FOR enter

  CLOSE WINDOW ventana_1
END FUNCTION

FUNCTION sube_archivo()
  DEFINE v_ejecuta           CHAR(500)
  
  LET generar = reg_bat.nombre_archivo

  SELECT 'X'
  FROM   taa_ctr_traspaso
  WHERE  nombre_archivo = reg_bat.nombre_archivo
  AND    ini_incorpora IS NOT NULL
  AND    ini_incorpora <> DATETIME (1899-12-31) YEAR TO DAY
  GROUP BY 1
  IF SQLCA.SQLCODE = 0 THEN
     DISPLAY  "Program stopped, ARCHIVO YA PROCESADO"
     EXIT PROGRAM
  END IF

  LET archivo_traspaso = g_param_taa.ruta_rescate CLIPPED,"/",
                         reg_bat.nombre_archivo CLIPPED

  LET v_ejecuta = "sed -e '/^01/!d' ", 
                  g_param_taa.ruta_rescate CLIPPED, "/",
                  reg_bat.nombre_archivo   CLIPPED, " >",
                  g_param_taa.ruta_rescate CLIPPED, "/",
                  p_tablayout.arch_cza
  RUN v_ejecuta

  IF fn_trata_cza() != FALSE THEN
     LET v_ejecuta = "sed -e '/^02/!d' ",
                     g_param_taa.ruta_rescate CLIPPED, "/",
                     reg_bat.nombre_archivo   CLIPPED, " >",
                     g_param_taa.ruta_rescate CLIPPED, "/",
                     p_tablayout.arch_det
     RUN v_ejecuta

     LET v_ejecuta = "sed -e '/^05/!d' ",
                     g_param_taa.ruta_rescate CLIPPED, "/",
                     reg_bat.nombre_archivo   CLIPPED, " >",
                     g_param_taa.ruta_rescate CLIPPED, "/",
                     p_tablayout5.arch_det
     RUN v_ejecuta

     LET v_ejecuta = "sed -e '/^09/!d' ",
                     g_param_taa.ruta_rescate CLIPPED, "/",
                     reg_bat.nombre_archivo   CLIPPED, " >",
                     g_param_taa.ruta_rescate CLIPPED, "/",
                     p_tablayout.arch_sum
     RUN v_ejecuta

     CALL fn_salida(p_tablayout.layout_cod, v_t_det2, p_tablayout.arch_det, p_tablayout.tab_det)
     LET v_ejecuta = "cd ", g_param_taa.ruta_rescate CLIPPED,
                     "/;dbload -d safre_tmp -c ", p_tablayout.nom_arch,
                     " -l " CLIPPED," ", p_tablayout.arch_det CLIPPED,
                     ".err" CLIPPED," -e 100 -n 1000 -k >> taa_rec_status.log  "
     RUN v_ejecuta

     CALL fn_salida(p_tablayout5.layout_cod, v_t_det5, p_tablayout5.arch_det, p_tablayout5.tab_det)
     LET v_ejecuta = "cd ", g_param_taa.ruta_rescate CLIPPED,
                     "/;dbload -d safre_tmp -c ", p_tablayout5.nom_arch,
                     " -l " CLIPPED," ", p_tablayout5.arch_det CLIPPED,
                     ".err" CLIPPED," -e 100 -n 1000 -k >> taa_rec_status.log  "
     RUN v_ejecuta

     CALL fn_salida(p_tablayout.layout_cod, v_t_sum, p_tablayout.arch_sum, p_tablayout.tab_sum)
     LET v_ejecuta = "cd ", g_param_taa.ruta_rescate CLIPPED,
                     "/;dbload -d safre_tmp -c ", p_tablayout.nom_arch,
                     " -l " CLIPPED," ", p_tablayout.arch_sum CLIPPED,
                     ".err" CLIPPED," -e 100 -n 1000 -k >> taa_rec_status.log  "
     RUN v_ejecuta
  ELSE
     DISPLAY "Program stopped, Nombre arhivo incorrecto o vacio."
     EXIT PROGRAM
  END IF
END FUNCTION

FUNCTION fn_trata_cza()
#tc----------------
  DEFINE v_ejecuta           CHAR(500)
  DEFINE v_plano             CHAR(650)
  
  --CPL-3956
  WHENEVER ERROR CONTINUE 
    DROP TABLE taa_cta_rec_cza_p
  WHENEVER ERROR STOP   
  --CPL-3956
  
  CREATE TEMP TABLE taa_cta_rec_cza_p (v_plano CHAR(650))

  LET v_ejecuta = g_param_taa.ruta_rescate CLIPPED, "/", p_tablayout.arch_cza

  LOAD FROM v_ejecuta
  INSERT INTO taa_cta_rec_cza_p

  SELECT *
  INTO   v_plano
  FROM   taa_cta_rec_cza_p
  IF v_plano[03,04] <> "02" THEN  
     ERROR "IDENTIFICADOR DE SERVICIO INVALIDO"
     SLEEP 4
     EXIT PROGRAM
  END IF

  IF (v_plano[05,06] <> "09"  AND
      v_plano[05,06] <> "12") THEN  
     ERROR "IDENTIFICADOR DE OPERACION INVALIDO"
     SLEEP 4
     EXIT PROGRAM
  END IF

  LET g_id_operacion   = v_plano[05,06]
  LET g_f_pres_enc     = v_plano[20,27]
  LET g_f_presentacion = MDY(g_f_pres_enc[5,6],
                             g_f_pres_enc[7,8],
                             g_f_pres_enc[1,4])
  LET v_id_operacion   = g_id_operacion USING "&&"

  CALL fn_salida(p_tablayout.layout_cod, v_t_cza, p_tablayout.arch_cza, p_tablayout.tab_cza)
  LET v_ejecuta = "cd ", g_param_taa.ruta_rescate CLIPPED,
                  "/;dbload -d safre_tmp -c ", p_tablayout.nom_arch,
                  " -l " CLIPPED, " ", p_tablayout.arch_cza CLIPPED,
                  ".err" CLIPPED, " -e 100 -n 1000 -k  > taa_rec_status.log"
  RUN v_ejecuta

  RETURN TRUE
END FUNCTION

FUNCTION inicializa()
  LET reg_det_tra_rcv.cve_subcta_1 = NULL
  LET reg_det_tra_rcv.prctj_subc_1 = 0
  LET reg_det_tra_rcv.import_sub_1 = 0
  LET reg_det_tra_rcv.siefore_1    = NULL
  LET reg_det_tra_rcv.no_tot_acc_1 = 0
  LET reg_det_tra_rcv.precio_acc_1 = 0

  LET reg_det_tra_rcv.cve_subcta_2 = NULL
  LET reg_det_tra_rcv.prctj_subc_2 = 0
  LET reg_det_tra_rcv.import_sub_2 = 0
  LET reg_det_tra_rcv.siefore_2    = NULL
  LET reg_det_tra_rcv.no_tot_acc_2 = 0
  LET reg_det_tra_rcv.precio_acc_2 = 0

  LET reg_det_tra_rcv.cve_subcta_3 = NULL
  LET reg_det_tra_rcv.prctj_subc_3 = 0
  LET reg_det_tra_rcv.import_sub_3 = 0
  LET reg_det_tra_rcv.siefore_3    = NULL
  LET reg_det_tra_rcv.no_tot_acc_3 = 0
  LET reg_det_tra_rcv.precio_acc_3 = 0

  LET reg_det_tra_rcv.cve_subcta_4 = NULL
  LET reg_det_tra_rcv.prctj_subc_4 = 0
  LET reg_det_tra_rcv.import_sub_4 = 0
  LET reg_det_tra_rcv.siefore_4    = NULL
  LET reg_det_tra_rcv.no_tot_acc_4 = 0
  LET reg_det_tra_rcv.precio_acc_4 = 0

  LET reg_det_tra_rcv.cve_subcta_5 = NULL
  LET reg_det_tra_rcv.prctj_subc_5 = 0
  LET reg_det_tra_rcv.import_sub_5 = 0
  LET reg_det_tra_rcv.siefore_5    = NULL
  LET reg_det_tra_rcv.no_tot_acc_5 = 0
  LET reg_det_tra_rcv.precio_acc_5 = 0

  LET reg_det_tra_rcv.cve_subcta_6 = NULL
  LET reg_det_tra_rcv.prctj_subc_6 = 0
  LET reg_det_tra_rcv.import_sub_6 = 0
  LET reg_det_tra_rcv.siefore_6    = NULL
  LET reg_det_tra_rcv.no_tot_acc_6 = 0
  LET reg_det_tra_rcv.precio_acc_6 = 0

  LET reg_det_tra_rcv.cve_subcta_7 = NULL
  LET reg_det_tra_rcv.prctj_subc_7 = 0
  LET reg_det_tra_rcv.import_sub_7 = 0
  LET reg_det_tra_rcv.siefore_7    = NULL
  LET reg_det_tra_rcv.no_tot_acc_7 = 0
  LET reg_det_tra_rcv.precio_acc_7 = 0

  LET reg_det_tra_rcv.cve_subcta_8 = NULL
  LET reg_det_tra_rcv.prctj_subc_8 = 0
  LET reg_det_tra_rcv.import_sub_8 = 0
  LET reg_det_tra_rcv.siefore_8    = NULL
  LET reg_det_tra_rcv.no_tot_acc_8 = 0
  LET reg_det_tra_rcv.precio_acc_8 = 0
END FUNCTION

FUNCTION validacion_previa()
  DEFINE c2_tipo_registro    CHAR(02)
  DEFINE tipo_det            CHAR(02)

  DEFINE sw_1                SMALLINT
  DEFINE sw_2                SMALLINT
  DEFINE sw_9                SMALLINT

  DEFINE v_tot_detalle       DECIMAL(10,0)
  DEFINE v_tot_sumario       DECIMAL(10,0)

  ERROR "COMIENZA VALIDACION DE ESTRUCTURA PASO 1 DE 4" #MLM3842 CPL-2609
  SLEEP 2

  CALL fn_taa_ctr_proceso(1,1)

  DATABASE safre_tmp

    #Se buscan las cuentas inactivas:
    DROP TABLE tmp_cuentas_inactivas

    CREATE TABLE tmp_cuentas_inactivas
    (
        nss CHAR(11)
    )IN tmp_dbs2;

    INSERT INTO tmp_cuentas_inactivas
    SELECT nss FROM safre_af:cta_act_marca 
    WHERE marca_cod IN (120,130,150,151);

    CREATE INDEX xie1tmp_inctivas ON tmp_cuentas_inactivas (nss)

    UPDATE STATISTICS FOR TABLE tmp_cuentas_inactivas
    
    UPDATE STATISTICS FOR TABLE taa_cta_rec_cza

    CREATE INDEX ix_taa_cta_rec_det2_1 ON taa_cta_rec_det2(tpo_registro) 
    CREATE INDEX ix_taa_cta_rec_det2_2 ON taa_cta_rec_det2(nss) 
    CREATE INDEX ix_taa_cta_rec_det2_3 ON taa_cta_rec_det2(curp, tpo_registro)
    CREATE INDEX ix_taa_cta_rec_det2_4 ON taa_cta_rec_det2(tpo_traspaso)
    
    UPDATE STATISTICS FOR TABLE taa_cta_rec_det2

    CREATE INDEX ix_taa_cta_rec_det5_1 ON taa_cta_rec_det5(tpo_registro) 
    CREATE INDEX ix_taa_cta_rec_det5_2 ON taa_cta_rec_det5(nss)
    CREATE INDEX ix_taa_cta_rec_det5_3 ON taa_cta_rec_det5(curp)
    CREATE INDEX ix_taa_cta_rec_det5_4 ON taa_cta_rec_det5(tpo_traspaso) 
    UPDATE STATISTICS FOR TABLE taa_cta_rec_det5

    UPDATE STATISTICS FOR TABLE taa_cta_rec_sum

    LET v_tot_reg_cza  = 0
    LET v_tot_reg_det2 = 0
    LET v_tot_reg_det5 = 0
    LET v_tot_reg_sum  = 0

    SELECT COUNT(*)
    INTO   v_tot_reg_cza
    FROM   taa_cta_rec_cza
    IF v_tot_reg_cza  = 0    OR 
       v_tot_reg_cza IS NULL THEN
       IF bnd_proceso THEN
          DISPLAY "Program stopped, NO EXISTE ENCABEZADO"
       ELSE
          PROMPT "SE RECHAZA EL LOTE. NO EXISTE ENCABEZADO" FOR enter
       END IF
       EXIT PROGRAM
    END IF

    SELECT COUNT(*)
    INTO   v_tot_reg_det2
    FROM   taa_cta_rec_det2
    WHERE  tpo_registro = '02'
    IF v_tot_reg_det2  = 0    OR
       v_tot_reg_det2 IS NULL THEN
       IF bnd_proceso THEN
          DISPLAY "Program stopped, NO EXISTEN REGISTROS DE DETALLE"
       ELSE
          PROMPT "SE RECHAZA EL LOTE. NO EXISTEN REGISTROS DE DETALLE"
          FOR enter
       END IF
       EXIT PROGRAM
    END IF

    SELECT COUNT(*)
    INTO   v_tot_reg_det5
    FROM   taa_cta_rec_det5
    WHERE  tpo_registro = '05'
    IF v_tot_reg_det5 IS NULL THEN
       LET v_tot_reg_det5 = 0
    END IF

    SELECT COUNT(*)
    INTO   v_tot_reg_sum
    FROM   taa_cta_rec_sum
    IF v_tot_reg_sum  = 0    OR
       v_tot_reg_sum IS NULL THEN
       IF bnd_proceso THEN
          DISPLAY "Program stopped, NO EXISTE SUMARIO"
       ELSE
          PROMPT "SE RECHAZA EL LOTE. NO EXISTE SUMARIO" FOR enter
       END IF
       EXIT PROGRAM
    END IF 
 
    LET v_tot_detalle = v_tot_reg_det2 + v_tot_reg_det5

    SELECT cant_reg_detalle
    INTO   v_tot_sumario
    FROM   taa_cta_rec_sum
    IF v_tot_detalle <> v_tot_sumario THEN
       IF bnd_proceso THEN
          DISPLAY "Program stopped, TOTAL DE REGISTROS NO CORRESPONDE CON EL SUMARIO"
       ELSE
          PROMPT "SE RECHAZA EL LOTE, TOTAL DE REGISTROS NO CORRESPONDE CON EL SUMARIO" FOR enter
       END IF
       EXIT PROGRAM
    END IF

    LET tipo_det      = "02"
    LET v_tot_detalle = 0 

    SELECT COUNT(a.nss)
    INTO   v_tot_detalle
    FROM   taa_cta_rec_det2 a
    WHERE  a.nss NOT IN (SELECT b.nss
                         FROM   taa_cta_rec_det5 b)
    IF v_tot_detalle <> 0 THEN
       IF bnd_proceso THEN
          DISPLAY "Program stopped, NO CORRESPONDE EL TOTAL DEL DET-02 CON LOS DET-05"
       ELSE
          PROMPT "SE RECHAZA EL LOTE, NO CORRESPONDE EL TOTAL DEL DET-02 CON LOS DET-05" FOR enter
       END IF
       CALL imprime_rep_rech(tipo_det)

       EXIT PROGRAM
    END IF
     
    LET tipo_det      = "05"
    LET v_tot_detalle = 0 

    SELECT COUNT(a.nss)
    INTO   v_tot_detalle
    FROM   taa_cta_rec_det5 a
    WHERE  a.nss NOT IN (SELECT b.nss
                         FROM   taa_cta_rec_det2 b)
    IF v_tot_detalle <> 0 THEN
       IF bnd_proceso THEN
          DISPLAY "Program stopped, NO CORRESPONDE EL TOTAL DEL DET-05 CON LOS DET-02"
       ELSE
          PROMPT "SE RECHAZA EL LOTE, NO CORRESPONDE EL TOTAL DEL DET-05 CON LOS DET-02" FOR enter
       END IF
       CALL imprime_rep_rech(tipo_det)

       EXIT PROGRAM
    END IF
  DATABASE safre_af

  CALL fn_taa_ctr_proceso(1,2)

  ERROR "FINALIZA VALIDACION DE ESTRUCTURA SATISFACTORIAMENTE PASO 1 DE 4" #MLM-3842 CPL-2609
  SLEEP 3 #MLM-3842
END FUNCTION

FUNCTION imprime_rep_rech(detalle)
#---------------------------------
  DEFINE detalle             CHAR(02)
  DEFINE c3_nss              CHAR(11)
  DEFINE det_dif             CHAR(02)
  DEFINE rech_det            CHAR(40)

  LET v_hora  = TIME
  LET g_lista = g_param_taa.ruta_listados CLIPPED, "/", g_usuario CLIPPED,
                  ".afili_rec_", generar[4,5],"_rech.", v_hoy using "DDMMYY",
                  "_", v_hora[1,2], v_hora[4,5] CLIPPED

  LET tot_total = 0

  START REPORT listado_rech TO g_lista
    IF detalle = "02"  THEN
       LET det_dif = "05"

       DECLARE cur_3_2 CURSOR FOR
       SELECT UNIQUE(a.nss)
       FROM   safre_tmp:taa_cta_rec_det2 a
       WHERE  a.nss NOT IN (SELECT b.nss
                            FROM   safre_tmp:taa_cta_rec_det5 b)
       FOREACH cur_3_2 INTO c3_nss
         LET rech_det  = "EL NSS ", c3_nss CLIPPED, " NO TIENE DETALLE ", det_dif
         LET tot_total = tot_total + 1
         OUTPUT TO REPORT listado_rech(rech_det)
       END FOREACH
    ELSE
       LET det_dif = "02"

       DECLARE cur_3_5 CURSOR FOR
       SELECT UNIQUE(a.nss)
       FROM   safre_tmp:taa_cta_rec_det5 a
       WHERE  a.nss NOT IN (SELECT b.nss
                            FROM   safre_tmp:taa_cta_rec_det2 b)
       FOREACH cur_3_5 INTO c3_nss
         LET rech_det  = "EL NSS ", c3_nss CLIPPED, " NO TIENE DETALLE ", det_dif
         LET tot_total = tot_total + 1
         OUTPUT TO REPORT listado_rech(rech_det)
       END FOREACH
    END IF
  FINISH REPORT listado_rech

  LET tot_total = 0
END FUNCTION

REPORT listado_rech(detalle_imp)
#-------------------------------
  DEFINE
    detalle_imp              CHAR(40)

  DEFINE campo               RECORD
    des_titulo               CHAR(23)
  END RECORD

  OUTPUT
    PAGE LENGTH   60
    LEFT MARGIN   0
    RIGHT MARGIN  132
    TOP MARGIN    0
    BOTTOM MARGIN 0

  FORMAT
    PAGE HEADER

    SELECT b.afore_desc
    INTO   campo.des_titulo
    FROM   tab_afore b
    WHERE  b.afore_cod = 532

    PRINT COLUMN 4, "INFORMACION DE AFILIADOS TRASPASADOS A AFORE ", campo.des_titulo, v_hoy USING "dd/mm/yyyy"
    PRINT

    IF g_id_operacion = 9 THEN
       PRINT COLUMN 4, "                               TRASPASO NORMAL                               "
    ELSE
       PRINT COLUMN 4, "                           TRASPASO COMPLEMENTARIO                           "
    END IF

    PRINT
    PRINT COLUMN 4, "NOMBRE DEL ARCHIVO : ", generar, " IMCOMPLETOS LOS DETALLES "
    PRINT

    ON EVERY ROW
       PRINT COLUMN 4, detalle_imp
       IF lineno > 57 THEN
          SKIP TO TOP OF PAGE
       END IF

    ON LAST ROW
       PRINT
       PRINT COLUMN 4, " TOTAL ", tot_total USING "&&&&&&"
END REPORT

FUNCTION carga_tablas_tmp()
  DEFINE carga_reg           CHAR(730)
  DEFINE c2_ident_operacion  CHAR(002)
  DEFINE r_existe            SMALLINT
  DEFINE r_edad              SMALLINT
  DEFINE r_criterio          SMALLINT
  DEFINE r_ind_edad          SMALLINT
  DEFINE v_curp              CHAR(18)
  DEFINE v_rfc               CHAR(13)
  DEFINE v_fena              DATE
  DEFINE v_crea_fecha        DATE
  DEFINE v_sql               CHAR(500)
  DEFINE v_txt               CHAR(5000)
  DEFINE v_det_tra_viv       RECORD LIKE safre_tmp:det_tra_viv.*
  DEFINE v_det_tra_rcv       RECORD LIKE safre_tmp:det_tra_rcv.*

  LET c2_ident_operacion = ""

  ERROR "COMIENZA LA CARGA DE TABLAS PASO 2 DE 4" #MLM-3842 CPL-2609
  SLEEP 2

  CALL fn_taa_ctr_proceso(2,1)

  LET r_existe   = 0
  LET r_edad     = 0
  LET r_criterio = 0
  LET r_ind_edad = 0
  LET v_curp     = ""
  LET v_rfc      = ""
  LET v_fena     = TODAY

  PREPARE eje_prio0 FROM "SET PDQPRIORITY HIGH"
  EXECUTE eje_prio0
 
  LET v_sql = "\n INSERT INTO safre_tmp:tmp_afi_sol_tar ",
              "\n SELECT a.n_seguro, a.fentcons, a.n_unico, a.status_interno, a.tipo_solicitud ",
              "\n FROM afi_solicitud a ",
              "\n INNER JOIN safre_tmp:taa_cta_rec_det2 b ON (b.curp <> '' AND b.curp IS NOT NULL AND b.curp = a.n_unico) ",
              "\n WHERE  b.tpo_traspaso IN (28,71,72,73,74,75,76,78) "
  PREPARE ps_tmp_afi_tar FROM v_sql
  EXECUTE ps_tmp_afi_tar
  
  LET v_sql = "\n INSERT INTO safre_tmp:tmp_afi_sol_tar ",
              "\n SELECT a.n_seguro, a.fentcons, a.n_unico, a.status_interno, a.tipo_solicitud ",
              "\n FROM afi_mae_afiliado a ",
              "\n INNER JOIN safre_tmp:taa_cta_rec_det2 b ON (b.nss = a.n_seguro) ",
              "\n WHERE  b.tpo_traspaso IN (29,51,52,53,54) "
  PREPARE ps_tmp_mae_tar FROM v_sql
  EXECUTE ps_tmp_mae_tar

  DATABASE safre_tmp
    CREATE INDEX tmp_afi_sol_tar_1 ON tmp_afi_sol_tar(nss)
    CREATE INDEX tmp_afi_sol_tar_2 ON tmp_afi_sol_tar(curp)
    CREATE INDEX tmp_afi_sol_tar_3 ON tmp_afi_sol_tar(tpo_solicitud, edo_interno)
    UPDATE STATISTICS FOR TABLE tmp_afi_sol_tar
  DATABASE safre_af

  SELECT 'X'
  FROM   taa_ctr_traspaso
  WHERE  nombre_archivo = generar
  GROUP BY 1
  IF SQLCA.SQLCODE <> 0 THEN
     INITIALIZE r_control TO NULL

     LET r_control.nombre_archivo  = generar
     LET r_control.fecha_recepcion = CURRENT
     LET r_control.usr_recepcion   = g_usuario

     INSERT INTO taa_ctr_traspaso VALUES ( r_control.*)
  END IF

  --CPL-2919 se agregan indicadores
  -- se obtiene codigo de siefore
  LET v_sql = "\n SELECT FIRST 1 siefore_cod ",
              "\n FROM   tab_siefore ",
              "\n WHERE  siefore_desc = ? "

  PREPARE pre_obt_siefore FROM v_sql

  LET exe_fnacimiento = "EXECUTE PROCEDURE fn_fnacimiento(?,?)"
  LET exq_sql3        = "EXECUTE FUNCTION fn_valida_edad_sol(?,?)"

  PREPARE prp_fnacimiento FROM exe_fnacimiento
  DECLARE cur_fnacimiento CURSOR FOR prp_fnacimiento
  PREPARE prp_fecha_sol FROM exq_sql3

  #---020901 Encabezado Traspaso de Cuenta---#
  LET v_txt = ""
  LET v_txt = "\n INSERT INTO safre_tmp:cza_tra_afo ",
              "\n SELECT '", generar,             "',", 
              "\n        tpo_registro,              ",
              "\n        id_servicio,               ",
              "\n        id_operacion,              ",
              "\n        tpo_ent_origen,            ",
              "\n        cve_ent_origen,            ",
              "\n        tpo_ent_destino,           ",
              "\n        cve_ent_destino,           ",
              "\n        ef_envio_lote,             ",
              "\n        MDY(f_presentacion[5,6],   ",
              "\n            f_presentacion[7,8],   ",
              "\n            f_presentacion[1,4]),  ",
              "\n        consecutivo_lote,          ",
              "\n        cve_mod_rec,               ",
              "\n        cod_res_operacion,         ",
              "\n        motivo_rechazo             ",
              "\n FROM   safre_tmp:taa_cta_rec_cza  "
 
  LET v_txt = v_txt CLIPPED
  --display "insert cza_tra_afo: ", v_txt
  --sleep 10

  PREPARE q_qry1 FROM v_txt
  EXECUTE q_qry1
             
  #---020902 Detalle de Traspaso de Cuenta (Vivienda)---#
  LET v_txt = ""
  LET v_txt = "\n SELECT '", generar,                      "',",
              "\n        tpo_registro,                        ",
              "\n        '", v_id_operacion,               "',",
              "\n        cont_servicio,                       ",
              "\n        tpo_ent_rec_cta,                     ",
              "\n        cve_ent_rec_cta,                     ",
              "\n        tpo_ent_transf_cta,                  ",
              "\n        cve_ent_transf_cta,                  ",
              "\n        tpo_traspaso,                        ",
              "\n        MDY(f_presentacion[5,6],             ",
              "\n            f_presentacion[7,8],             ",
              "\n            f_presentacion[1,4]),            ",
              "\n        curp,                                ",
              "\n        curp_unificadora,                    ",
              "\n        nss,                                 ",
              "\n        nss_unificador,                      ",
              "\n        '',                                  ",
              "\n        ap_paterno,                          ",
              "\n        ap_materno,                          ",
              "\n        nombre,                              ",
              "\n        '',                                  ",
              "\n        '',                                  ",
              "\n        f_recepcion_sol[5,6] ||              ",
              "\n        f_recepcion_sol[7,8] ||              ",
              "\n        f_recepcion_sol[1,4],                ",
              "\n        '',                                  ",
              "\n        '',                                  ",
              "\n        '',                                  ",
              "\n        '',                                  ",
              "\n        '',                                  ",
              "\n        '',                                  ",
              "\n        apl_int_issste_08 / 1000000,         ",
              "\n        sdo_fnd_issste_08 / 100,             ",
              "\n        apl_int_vivenda_97 / 1000000,        ",
              "\n        sdo_vivienda_97 / 100,               ",
              "\n        aplic_int_issste_92 / 1000000,       ",
              "\n        sdo_issste_92 / 100,                 ",
              "\n        aplic_int_vivienda_92 / 1000000,     ",
              "\n        sdo_vivienda_92 / 100,               ",
              "\n        id_cred_issste,                      ",
              "\n        '',                                  ",
              "\n        '',                                  ",
              "\n        id_cred_vivienda,                    ",
              "\n        cod_res_operacion,                   ",
              "\n        diag_proceso,                        ",
              "\n        id_tpo_reg_pension,                  ",
              "\n        TO_CHAR(f_redencion_bono[5,6]) ||    ",
              "\n        TO_CHAR(f_redencion_bono[7,8]) ||    ",
              "\n        TO_CHAR(f_redencion_bono[1,4]),      ",
              "\n        importe_bono / 10000,                ",
              "\n        apl_int_issste_08_bd / 1000000,      ",
              "\n        aplic_int_viv_92_bd / 1000000,       ",
              "\n        TO_CHAR(f_redencion_bono_bd[5,6]) || ",
              "\n        TO_CHAR(f_redencion_bono_bd[7,8]) || ",
              "\n        TO_CHAR(f_redencion_bono_bd[1,4]),   ",
              "\n        importe_bono_bd / 10000,             ",
              "\n        num_aport_vivienda,                  ",
              "\n        dias_pag_cs,                         ",
              "\n        num_aport_issste,                    ",
              "\n        0,                                   ",
              "\n        '',                                  ",
              "\n        cve_siefore_oss,                     ",
              "\n        TO_CHAR(f_aplica_oss[5,6]) ||        ",
              "\n        TO_CHAR(f_aplica_oss[7,8]) ||        ",
              "\n        TO_CHAR(f_aplica_oss[1,4]),          ",
              "\n        ind_auto_vinc_am,                    ",
              "\n        ind_apo_vol_ded                      ", 
              "\n FROM   safre_tmp:taa_cta_rec_det2           " 

  LET v_txt = v_txt CLIPPED
  --display "insert det_tra_viv: ", v_txt
  --sleep 10

  PREPARE q_qry2 FROM v_txt
  DECLARE c_qr2 CURSOR FOR q_qry2
  FOREACH c_qr2 INTO v_det_tra_viv.*,
                     g_taa_ind_op09.cve_siefore_desc,
                     g_taa_ind_op09.f_aplica_oss,
                     g_taa_ind_op09.ind_auto_vinc_am,
                     g_taa_ind_op09.ind_apo_vol_ded
    ##Para Prestadora de Servicios
    IF v_det_tra_viv.cve_ced_cuenta = "531" THEN
       LET v_det_tra_viv.cve_ced_cuenta = v_det_tra_viv.cve_recep_cuenta
    END IF

    #--Validacion de Origenes de Traspaso NUEVOS en donde el NSS no viene.
    #--Solo viene la CURP por tal motivo hay que traernos el NSS.
    IF g_id_operacion = 9 THEN
       CASE v_det_tra_viv.tipo_traspaso
         --CPL-3053. Fondos generacionales
         #Traspaso A-A por Asignacion de Cuentas
         WHEN 51
           IF v_det_tra_viv.n_seguro IS NULL OR v_det_tra_viv.n_seguro = "" OR v_det_tra_viv.n_seguro = "           " THEN
              CALL fn_verifica_nss(v_det_tra_viv.tipo_traspaso, v_det_tra_viv.n_unico, 1)
              RETURNING v_det_tra_viv.n_seguro
           END IF
         #Traspaso A-A por Asignacion de Cuentas
         WHEN 52
           IF v_det_tra_viv.n_seguro IS NULL OR v_det_tra_viv.n_seguro = "" OR v_det_tra_viv.n_seguro = "           " THEN
              CALL fn_verifica_nss(v_det_tra_viv.tipo_traspaso, v_det_tra_viv.n_unico, 1)
              RETURNING v_det_tra_viv.n_seguro
           END IF
         #Traspaso por Asignacion por Prestadora de Servicios
         WHEN 53
           IF v_det_tra_viv.n_seguro IS NULL OR v_det_tra_viv.n_seguro = "" OR v_det_tra_viv.n_seguro = "           " THEN
              CALL fn_verifica_nss(v_det_tra_viv.tipo_traspaso, v_det_tra_viv.n_unico, 1)
              RETURNING v_det_tra_viv.n_seguro
           END IF
         #Traspaso por Reasignacion de cuentas por Prestadora de Servicios
         WHEN 54
           IF v_det_tra_viv.n_seguro IS NULL OR v_det_tra_viv.n_seguro = "" OR v_det_tra_viv.n_seguro = "           " THEN
              CALL fn_verifica_nss(v_det_tra_viv.tipo_traspaso, v_det_tra_viv.n_unico, 1)
              RETURNING v_det_tra_viv.n_seguro
           END IF

         #OP13 PLUS AQUI NO SE VALIDA SOLO SE RECUPERA EL N_SEGURO
         #PARA EL TIPO TRASPASO 71
         #Traspaso entre Administradoras por CURP a traves de Agente Promotor
         #ident_operacion 09 = NORMAL, 12 = COMPLEMENTARIAS
         WHEN 71
           CALL fn_verifica_nss(v_det_tra_viv.tipo_traspaso, v_det_tra_viv.n_unico, 2)
           RETURNING v_det_tra_viv.n_seguro

         #CPL-2046 Modif. Carga de la OP 09 tipo de traspaso 72 y 74
         #status de la solicitud sea mayor o igual a 100.
         #27-Julio-2015
         #Traspaso entre Administradoras por CURP a traves de medios Diversos
         WHEN 72
           CALL fn_verifica_nss(v_det_tra_viv.tipo_traspaso, v_det_tra_viv.n_unico, 2)
           RETURNING v_det_tra_viv.n_seguro

         #Traspaso Indebido por CURP.
         WHEN 73
           SELECT a.n_seguro
           INTO   v_det_tra_viv.n_seguro
           FROM   afi_mae_afiliado a                      #1323
           WHERE  a.status_interno >= 100                 #1323
           AND    a.n_unico         = v_det_tra_viv.n_unico

         #CPL-2046 Modif. Carga de la OP 09 tipo de traspaso 72 y 74
         #status de la solicitud sea mayor o igual a 100.
         #27-Julio-2015
         #Traspaso entre Administradoras por CURP a traves de Internet 
         WHEN 74 
           CALL fn_verifica_nss(v_det_tra_viv.tipo_traspaso, v_det_tra_viv.n_unico, 2)
           RETURNING v_det_tra_viv.n_seguro

         #Traspaso app por CURP
         WHEN 75
            IF v_det_tra_viv.n_seguro IS NULL OR v_det_tra_viv.n_seguro = "" OR v_det_tra_viv.n_seguro = "           " THEN
               CALL fn_verifica_nss(v_det_tra_viv.tipo_traspaso, v_det_tra_viv.n_unico, 2)
               RETURNING v_det_tra_viv.n_seguro
            END IF

         #Traspaso Web por CURP
         WHEN 76
            IF v_det_tra_viv.n_seguro IS NULL OR v_det_tra_viv.n_seguro = "" OR v_det_tra_viv.n_seguro = "           " THEN
               CALL fn_verifica_nss(v_det_tra_viv.tipo_traspaso, v_det_tra_viv.n_unico, 2)
               RETURNING v_det_tra_viv.n_seguro
            END IF

         WHEN 78 #Traspaso de Menor
            CALL fn_verifica_nss(reg_det_tra_viv.tipo_traspaso, reg_det_tra_viv.n_unico, 2)
            RETURNING reg_det_tra_viv.n_seguro
       END CASE
    ELSE
       IF v_det_tra_viv.n_seguro IS NULL OR v_det_tra_viv.n_seguro  = "" OR v_det_tra_viv.n_seguro  = "           " THEN
         #Primero busca la cuenta con NTI
          SELECT a.n_seguro
            INTO v_det_tra_viv.n_seguro
            FROM afi_mae_afiliado a                       
           WHERE a.n_unico = v_det_tra_viv.n_unico
           AND a.n_seguro[1] = 'I'
           AND a.n_seguro NOT IN (SELECT mar.nss FROM safre_tmp:tmp_cuentas_inactivas mar WHERE mar.nss = a.n_seguro)
           
           IF v_det_tra_viv.n_seguro IS NULL OR v_det_tra_viv.n_seguro  = "" OR v_det_tra_viv.n_seguro  = "           " THEN
            #Si no encuentra por NTI, se busca una cuenta con NSS
               SELECT a.n_seguro
               INTO v_det_tra_viv.n_seguro
               FROM afi_mae_afiliado a                       
              WHERE a.n_unico = v_det_tra_viv.n_unico
              AND a.n_seguro[1] <> 'I'
              AND a.n_seguro NOT IN (SELECT mar.nss FROM safre_tmp:tmp_cuentas_inactivas mar WHERE mar.nss = a.n_seguro)
           END IF
       END IF 
    END IF

    -- INV-5018 JGPS nuevos indicadores por Orden de Seleccion
    LET g_taa_ind_op09.nom_archivo     = generar
    LET g_taa_ind_op09.curp            = v_det_tra_viv.n_unico
    LET g_taa_ind_op09.nss             = v_det_tra_viv.n_seguro
    LET g_taa_ind_op09.ap_paterno      = v_det_tra_viv.paterno
    LET g_taa_ind_op09.ap_materno      = v_det_tra_viv.materno
    LET g_taa_ind_op09.nombre          = v_det_tra_viv.nombre
    LET g_taa_ind_op09.f_recepcion_sol = v_det_tra_viv.fecha_recep_solic

    -- Se busca en el catalogo de claves el valor
    EXECUTE pre_obt_siefore USING g_taa_ind_op09.cve_siefore_desc
                            INTO  g_taa_ind_op09.cve_siefore_oss

    -- Se calcula la siefore actual
    -- WHENEVER ERROR STOP --3    CPL-3956  SE COMENTA 
      EXECUTE prp_fecha_sol USING g_taa_ind_op09.nss, v_det_tra_viv.tipo_traspaso
                            INTO  v_crea_fecha

      OPEN cur_fnacimiento USING g_taa_ind_op09.nss, v_crea_fecha

      FETCH cur_fnacimiento INTO r_existe, r_edad, r_criterio, r_ind_edad, v_curp, v_rfc, v_fena

      CLOSE cur_fnacimiento

      LET g_taa_ind_op09.perfil_actual = r_ind_edad
      LET g_taa_ind_op09.edo_registro  = 0
      LET g_taa_ind_op09.f_alta        = v_hoy
      LET g_taa_ind_op09.usuario_alta  = g_usuario

      -- Se almacena informaciÃ³n con los nuevos indicadores
      INSERT INTO taa_indicadores_op09 VALUES(seq_taa_indicadores_op09.NEXTVAL, g_taa_ind_op09.*)

      IF (s_codigo_afore  = 568                        AND
          YEAR(v_det_tra_viv.fecha_presentacion > 0 )  OR
          s_codigo_afore <> 568)                       THEN
      ELSE
          IF s_codigo_afore = 568 THEN
             LET v_det_tra_viv.fecha_presentacion = ""
          END IF
      END IF

      IF s_codigo_afore = 568                      AND
         YEAR(v_det_tra_viv.fecha_recep_solic > 0) THEN
      ELSE
         LET v_det_tra_viv.fecha_recep_solic = NULL
      END IF

      IF v_det_tra_viv.fecha_red_bono IS NOT NULL OR
         v_det_tra_viv.fecha_red_bono  = ""       THEN
         IF (s_codigo_afore  = 568                    AND
             YEAR(v_det_tra_viv.fecha_red_bono = 0 )  OR
             s_codigo_afore <> 568)                   THEN
         ELSE
             IF s_codigo_afore = 568 THEN
                LET v_det_tra_viv.fecha_red_bono = NULL
             END IF
         END IF
      END IF

      LET v_det_tra_viv.estado_reg = 0

      IF v_det_tra_viv.ident_operacion = '12' THEN
         LET v_det_tra_viv.fecha_red_bono  = NULL
         LET v_det_tra_viv.importe_bono    = 0
         LET v_det_tra_viv.importe_bono_bd = 0
      END IF

      INSERT INTO safre_tmp:det_tra_viv VALUES (v_det_tra_viv.*)
  END FOREACH
  FREE c_qr2

  #----- 020905 Detalle de Traspaso de Cuenta (RCV) -----#
  CALL inicializa()
  LET v_txt = ""
  LET v_txt = "\n SELECT '", generar,               "',",
              "\n        tpo_registro,                 ",
              "\n        '", v_id_operacion,        "',",
              "\n        cont_servicio,                ",
              "\n        tpo_ent_rec_cta,              ",
              "\n        cve_ent_rec_cta,              ",
              "\n        tpo_ent_transf_cta,           ",
              "\n        cve_ent_transf_cta,           ",
              "\n        tpo_traspaso,                 ",
              --"\n        f_apo_vol_patron[5,6] ||      ",
              --"\n        f_apo_vol_patron[7,8] ||      ",
              --"\n        f_apo_vol_patron[1,4],        ",
              --"\n        f_apo_vol_vent[5,6] ||        ",
              --"\n        f_apo_vol_vent[7,8] ||        ",
              --"\n        f_apo_vol_vent[1,4],          ",
              "\n        f_apo_vol_patron,             ",  # CPL-3470 CPL-3253 Se inserta la fecha tal cual viene en el archivo
              "\n        f_apo_vol_vent,               ",  # CPL-3470 CPL-3253 Se inserta la fecha tal cual viene en el archivo
              "\n        MDY(f_apo_largo_plazo[5,6],   ",
              "\n            f_apo_largo_plazo[7,8],   ",
              "\n            f_apo_largo_plazo[1,4]),  ",
              "\n        MDY(f_apo_pers_lar_pla[5,6],  ",
              "\n            f_apo_pers_lar_pla[7,8],  ",
              "\n            f_apo_pers_lar_pla[1,4]), ",
              "\n        curp,                         ",
              "\n        nss,                          ",
              "\n        MDY(f_val_acciones[5,6],      ",
              "\n            f_val_acciones[7,8],      ",
              "\n            f_val_acciones[1,4]),     ",
              "\n        '',                           ",
              "\n        cve_sub_1,                    ",
              "\n        '',                           ",
              "\n        imp_sub_1 / 100,              ",
              "\n        siefore_1,                    ",
              "\n        num_tot_acc_1 / 1000000,      ",
              "\n        precio_acc_1 / 1000000,       ",
              "\n        cve_sub_2,                    ",
              "\n        '',                           ",
              "\n        imp_sub_2 / 100,              ",
              "\n        siefore_2,                    ",
              "\n        num_tot_acc_2 / 1000000,      ",
              "\n        precio_acc_2 / 1000000,       ",
              "\n        cve_sub_3,                    ",
              "\n        '',                           ",
              "\n        imp_sub_3 / 100,              ",
              "\n        siefore_3,                    ",
              "\n        num_tot_acc_3 / 1000000,      ",
              "\n        precio_acc_3 / 1000000,       ",
              "\n        cve_sub_4,                    ",
              "\n        '',                           ",
              "\n        imp_sub_4 / 100,              ",
              "\n        siefore_4,                    ",
              "\n        num_tot_acc_4 / 1000000,      ",
              "\n        precio_acc_4 / 1000000,       ",
              "\n        cve_sub_5,                    ",
              "\n        '',                           ",
              "\n        imp_sub_5 / 100,              ",
              "\n        siefore_5,                    ",
              "\n        num_tot_acc_5 / 1000000,      ",
              "\n        precio_acc_5 / 1000000,       ",
              "\n        cve_sub_6,                    ",
              "\n        '',                           ",
              "\n        imp_sub_6 / 100,              ",
              "\n        siefore_6,                    ",
              "\n        num_tot_acc_6 / 1000000,      ",
              "\n        precio_acc_6 / 1000000,       ",
              "\n        cve_sub_7,                    ",
              "\n        '',                           ",
              "\n        imp_sub_7 / 100,              ",
              "\n        siefore_7,                    ",
              "\n        num_tot_acc_7 / 1000000,      ",
              "\n        precio_acc_7 / 1000000,       ",
              "\n        cve_sub_8,                    ",
              "\n        '',                           ",
              "\n        imp_sub_8 / 100,              ",
              "\n        siefore_8,                    ",
              "\n        num_tot_acc_8 / 1000000,      ",
              "\n        precio_acc_8 / 1000000,       ",
              "\n        cod_res_operacion,            ",
              "\n        diag_proceso,                 ",
              "\n        per_ultima_apo,               ",
              "\n        ult_sal_diario_int,           ",
              "\n        '',                           ",
              "\n        ''                            ",
              "\n FROM   safre_tmp:taa_cta_rec_det5    " 

  LET v_txt = v_txt CLIPPED
  --display "insert det_tra_rcv: ", v_txt
  --sleep 10

  PREPARE q_qry5 FROM v_txt
  DECLARE c_qr5 CURSOR FOR q_qry5
  FOREACH c_qr5 INTO v_det_tra_rcv.*
    #--Validacion de Origenes de Traspaso NUEVOS en donde el NSS no viene.
    #--Solo viene la CURP por tal motivo hay que traernos el NSS.
    IF g_id_operacion = 9 THEN
       CASE v_det_tra_rcv.tipo_traspaso
         --CPL-3053. Fondos generacionales
         #Traspaso A-A por Asignacion de Cuentas
         WHEN 51
           IF v_det_tra_rcv.n_seguro IS NULL OR v_det_tra_rcv.n_seguro = "" OR v_det_tra_rcv.n_seguro = "           " THEN
              CALL fn_verifica_nss(v_det_tra_rcv.tipo_traspaso, v_det_tra_rcv.n_unico, 1)
              RETURNING v_det_tra_rcv.n_seguro
           END IF
         #Traspaso A-A por Asignacion de Cuentas
         WHEN 52
           IF v_det_tra_rcv.n_seguro IS NULL OR v_det_tra_rcv.n_seguro = "" OR v_det_tra_rcv.n_seguro = "           " THEN
              CALL fn_verifica_nss(v_det_tra_rcv.tipo_traspaso, v_det_tra_rcv.n_unico, 1)
              RETURNING v_det_tra_rcv.n_seguro
           END IF
         #Traspaso por Asignacion por Prestadora de Servicios
         WHEN 53
           IF v_det_tra_rcv.n_seguro IS NULL OR v_det_tra_rcv.n_seguro = "" OR v_det_tra_rcv.n_seguro = "           " THEN
              CALL fn_verifica_nss(v_det_tra_rcv.tipo_traspaso, v_det_tra_rcv.n_unico, 1)
              RETURNING v_det_tra_rcv.n_seguro
           END IF
         #Traspaso por Reasignacion de cuentas por Prestadora de Servicios
         WHEN 54
           IF v_det_tra_rcv.n_seguro IS NULL OR v_det_tra_rcv.n_seguro = "" OR v_det_tra_rcv.n_seguro = "           " THEN
              CALL fn_verifica_nss(v_det_tra_rcv.tipo_traspaso, v_det_tra_rcv.n_unico, 1)
              RETURNING v_det_tra_rcv.n_seguro
           END IF

         #Traspaso entre Administradoras por CURP a traves de Agente Promotor
         WHEN 71
           CALL fn_verifica_nss(v_det_tra_rcv.tipo_traspaso, v_det_tra_rcv.n_unico, 2)
           RETURNING v_det_tra_rcv.n_seguro

         #CPL-2046 Modif. Carga de la OP 09 tipo de traspaso 72 y 74
         #status de la solicitud sea mayor o igual a 100.
         #27-Julio-2015
         #Traspaso entre Administradoras por CURP a traves de medios Diversos
         WHEN 72
           CALL fn_verifica_nss(v_det_tra_rcv.tipo_traspaso, v_det_tra_rcv.n_unico, 2)
           RETURNING v_det_tra_rcv.n_seguro

         #Traspaso Indebido por CURP.
         WHEN 73
           SELECT a.n_seguro
           INTO   v_det_tra_rcv.n_seguro
           FROM   afi_mae_afiliado a                      #1323
           WHERE  a.status_interno >= 100                 #1323
           AND    a.n_unico           = v_det_tra_rcv.n_unico

         #Traspaso entre Administradoras por CURP a traves de Internet 
         WHEN 74 
           CALL fn_verifica_nss(v_det_tra_rcv.tipo_traspaso, v_det_tra_rcv.n_unico, 2)
           RETURNING v_det_tra_rcv.n_seguro

         #Traspaso app por CURP
         WHEN 75
            IF v_det_tra_rcv.n_seguro IS NULL OR v_det_tra_rcv.n_seguro = "" OR v_det_tra_rcv.n_seguro = "           " THEN
               CALL fn_verifica_nss(v_det_tra_rcv.tipo_traspaso, v_det_tra_rcv.n_unico, 2)
               RETURNING v_det_tra_rcv.n_seguro
            END IF

         #Traspaso Web por CURP
         WHEN 76
            IF v_det_tra_rcv.n_seguro IS NULL OR v_det_tra_rcv.n_seguro = "" OR v_det_tra_rcv.n_seguro = "           " THEN
               CALL fn_verifica_nss(v_det_tra_rcv.tipo_traspaso, v_det_tra_rcv.n_unico, 2)
               RETURNING v_det_tra_rcv.n_seguro
            END IF

         WHEN 78 #Traspaso de Menor
               CALL fn_verifica_nss(reg_det_tra_rcv.tipo_traspaso, reg_det_tra_rcv.n_unico, 2)
               RETURNING reg_det_tra_rcv.n_seguro
       END CASE
    ELSE
       IF v_det_tra_rcv.n_seguro IS NULL OR v_det_tra_rcv.n_seguro  = "" OR v_det_tra_rcv.n_seguro  = "           " THEN
          SELECT a.n_seguro
            INTO v_det_tra_rcv.n_seguro
            FROM afi_mae_afiliado a                       #1323
           WHERE a.n_unico = v_det_tra_rcv.n_unico
             AND a.n_seguro NOT IN (SELECT mar.nss FROM safre_tmp:tmp_cuentas_inactivas mar WHERE mar.nss = a.n_seguro)  -- CPL-3956
       END IF
    END IF

    IF v_det_tra_rcv.siefore_1 IS NOT NULL AND
       v_det_tra_rcv.siefore_1 <> ' '      THEN
       CALL valida_siefore(v_det_tra_rcv.siefore_1)
    END IF

    IF v_det_tra_rcv.siefore_2 IS NOT NULL AND
       v_det_tra_rcv.siefore_2 <> ' '      THEN
       CALL valida_siefore(v_det_tra_rcv.siefore_2)
    END IF

    IF v_det_tra_rcv.siefore_3 IS NOT NULL AND
       v_det_tra_rcv.siefore_3 <> ' '      THEN
       CALL valida_siefore(v_det_tra_rcv.siefore_3)
    END IF

    IF v_det_tra_rcv.siefore_4 IS NOT NULL AND
       v_det_tra_rcv.siefore_4 <> ' '      THEN
       CALL valida_siefore(v_det_tra_rcv.siefore_4)
    END IF

    IF v_det_tra_rcv.siefore_5 IS NOT NULL AND
       v_det_tra_rcv.siefore_5 <> ' '      THEN
       CALL valida_siefore(v_det_tra_rcv.siefore_5)
    END IF

    IF v_det_tra_rcv.siefore_6 IS NOT NULL AND
       v_det_tra_rcv.siefore_6 <> ' '      THEN
       CALL valida_siefore(v_det_tra_rcv.siefore_6)
    END IF

    IF v_det_tra_rcv.siefore_7 IS NOT NULL AND
       v_det_tra_rcv.siefore_7 <> ' '      THEN
       CALL valida_siefore(v_det_tra_rcv.siefore_7)
    END IF

    IF v_det_tra_rcv.siefore_8 IS NOT NULL AND
       v_det_tra_rcv.siefore_8 <> ' '      THEN
       CALL valida_siefore(v_det_tra_rcv.siefore_8)
    END IF

    LET v_det_tra_rcv.estado_reg = 0

    INSERT INTO safre_tmp:det_tra_rcv VALUES(v_det_tra_rcv.*)
    CALL inicializa()
  END FOREACH
  FREE c_qr5

  #---020909 Sumario de Traspaso de Cuenta---#
  LET v_txt = ""
  LET v_txt = "\n INSERT INTO safre_tmp:sum_tra_afo              ",
              "\n SELECT '", generar,                         "',", 
              "\n        tpo_registro,                           ",
              "\n        cant_reg_detalle,                       ",
              "\n        NVL(num_tot_apl_int_viv97 / 1000000,0), ",
              "\n        NVL(tot_sdo_viv97 / 100,0),             ",
              "\n        NVL(num_tot_apl_int_iss92 / 1000000,0), ",
              "\n        NVL(tot_sdo_issste92 / 100,0),          ",
              "\n        NVL(num_tot_apl_int_viv92 / 1000000,0), ",
              "\n        NVL(tot_sdo_viv92 / 100,0),             ",
              "\n        NVL(num_tot_apl_int_iss08 / 1000000,0), ",
              "\n        NVL(tot_sdo_issste08 / 100,0),          ",
              "\n        NVL(tot_imp_bono_issste / 10000,0),     ",
              "\n        cve_subcuenta_1,                        ",
              "\n        NVL(tot_sdo_sub_1 / 100,0),             ",
              "\n        cve_subcuenta_2,                        ",
              "\n        NVL(tot_sdo_sub_2 / 100,0),             ",
              "\n        cve_subcuenta_3,                        ",
              "\n        NVL(tot_sdo_sub_3 / 100,0),             ",
              "\n        cve_subcuenta_4,                        ",
              "\n        NVL(tot_sdo_sub_4 / 100,0),             ",
              "\n        cve_subcuenta_5,                        ",
              "\n        NVL(tot_sdo_sub_5 / 100,0),             ",
              "\n        cve_subcuenta_6,                        ",
              "\n        NVL(tot_sdo_sub_6 / 100,0),             ",
              "\n        cve_subcuenta_7,                        ",
              "\n        NVL(tot_sdo_sub_7 / 100,0),             ",
              "\n        cve_subcuenta_8,                        ",
              "\n        NVL(tot_sdo_sub_8 / 100,0),             ",
              "\n        cve_subcuenta_9,                        ",
              "\n        NVL(tot_sdo_sub_9 / 100,0),             ",
              "\n        cve_subcuenta_10,                       ",
              "\n        NVL(tot_sdo_sub_10 / 100,0),            ",
              "\n        cve_subcuenta_11,                       ",
              "\n        NVL(tot_sdo_sub_11 / 100,0),            ",
              "\n        cve_subcuenta_12,                       ",
              "\n        NVL(tot_sdo_sub_12 / 100,0),            ",
              "\n        cve_subcuenta_13,                       ",
              "\n        NVL(tot_sdo_sub_13 / 100,0),            ",
              "\n        cve_subcuenta_14,                       ",
              "\n        NVL(tot_sdo_sub_14 / 100,0),            ",
              "\n        cve_subcuenta_15,                       ",
              "\n        NVL(tot_sdo_sub_15 / 100,0),            ",
              "\n        cve_subcuenta_16,                       ",
              "\n        NVL(tot_sdo_sub_16 / 100,0),            ",
              "\n        cve_subcuenta_17,                       ",
              "\n        NVL(tot_sdo_sub_17 / 100,0),            ",
              "\n        cve_subcuenta_18,                       ",
              "\n        NVL(tot_sdo_sub_18 / 100,0),            ",
              "\n        cve_subcuenta_19,                       ",
              "\n        NVL(tot_sdo_sub_19 / 100,0),            ",
              "\n        cve_subcuenta_20,                       ",
              "\n        NVL(tot_sdo_sub_20 / 100,0),            ",
              "\n        cve_subcuenta_21,                       ",
              "\n        NVL(tot_sdo_sub_21 / 100,0),            ",
              "\n        cve_subcuenta_22,                       ",
              "\n        NVL(tot_sdo_sub_22 / 100,0),            ",
              "\n        cve_subcuenta_23,                       ",
              "\n        NVL(tot_sdo_sub_23 / 100,0),            ",
              "\n        cve_subcuenta_24,                       ",
              "\n        NVL(tot_sdo_sub_24 / 100,0),            ",
              "\n        cve_subcuenta_25,                       ",
              "\n        NVL(tot_sdo_sub_25 / 100,0),            ",
              "\n        cve_subcuenta_26,                       ",
              "\n        NVL(tot_sdo_sub_26 / 100,0),            ",
              "\n        cve_subcuenta_27,                       ",
              "\n        NVL(tot_sdo_sub_27 / 100,0),            ",
              "\n        cve_subcuenta_28,                       ",
              "\n        NVL(tot_sdo_sub_28 / 100,0)             ",
              "\n FROM   safre_tmp:taa_cta_rec_sum               "
 
  LET v_txt = v_txt CLIPPED
  --display "insert sum_tra_afo: ", v_txt
  --sleep 10

  PREPARE q_qry9 FROM v_txt
  EXECUTE q_qry9

  CALL fn_taa_ctr_proceso(2,2)
END FUNCTION

FUNCTION lee_archivo_plano()
  DEFINE  xcodigo_marca      SMALLINT
  DEFINE  xcodigo_rechazo    SMALLINT
  DEFINE  pmarca_entra       SMALLINT
  DEFINE  pestado_marca      SMALLINT
  DEFINE  pcodigo_rechazo    SMALLINT
  DEFINE  pmarca_causa       SMALLINT

  DEFINE v_corr              INTEGER 

  DEFINE vn_seguro           CHAR(11)
  DEFINE vts_org             SMALLINT
  DEFINE cont                INTEGER
  DEFINE vn_folio            DECIMAL(10,0)
  DEFINE vfol_org            DECIMAL(10,0)

  DEFINE reg_mto             RECORD LIKE afi_mae_afiliado.*
  DEFINE reg_dom             RECORD LIKE afi_domicilio.*
  DEFINE reg_tel             RECORD LIKE afi_telefono.*
  DEFINE reg_cor             RECORD LIKE afi_correo_elect.*
  DEFINE reg_ben             RECORD LIKE afi_beneficiario.*
  DEFINE reg_pat             RECORD LIKE afi_patron.*

  DEFINE reg_ide             RECORD
    n_seguro                 CHAR(11),
    n_folio                  DECIMAL(10,0),
    tipo_solicitud           SMALLINT,
    clave_identif            SMALLINT,
    identifica               CHAR(30),
    ocr_ife                  CHAR(13),
    fecha                    DATE,
    usuario                  CHAR(08)
  END RECORD

  DEFINE reg_his             RECORD
    n_seguro                 CHAR(11),
    n_unico                  CHAR(18),
    n_rfc                    CHAR(13),
    paterno                  CHAR(40),
    materno                  CHAR(40),
    nombres                  CHAR(40),
    fena                     DATE,
    n_folio                  DECIMAL(10,0),
    edo_civil                SMALLINT,
    localn                   SMALLINT,
    estadon                  SMALLINT,
    tiptr                    SMALLINT,
    cod_promotor             CHAR(10),
    sexo                     SMALLINT,
    n_operac                 DECIMAL(10),
    frecafor                 DATE,
    fentcons                 DATE,
    femision                 DATE,
    finitmte                 DATE,
    finicta                  DATE,
    status                   SMALLINT,
    agenc_cod                CHAR(10),
    status_interno           SMALLINT,
    nacionalidad             CHAR(03),
    tip_prob                 CHAR(01),
    fol_prob                 CHAR(10),
    doc_prob                 CHAR(16),
    ind_infonavit            CHAR(01),
    documento_1              CHAR(01),
    documento_2              CHAR(01),
    documento_3              CHAR(01),
    documento_4              CHAR(01),
    documento_5              CHAR(01),
    documento_6              CHAR(01),
    envio_dom                SMALLINT,
    entidad_curp             INTEGER,
    asigna_curp              SMALLINT,
    const_curp               SMALLINT,
    usuario                  CHAR(08),
    hora                     CHAR(08),
    status_captura           CHAR(15),
    tipo_solicitud           SMALLINT,
    fecha_elaboracion        DATE,
    lote                     SMALLINT,
    fecha_envio              DATE,
    cod_esq_comision         SMALLINT,
    ubicacion                DECIMAL(10,0),
    fecha_1a_afil            DATE,
    indicador_c              CHAR(05),
    indicador_d              CHAR(05),
    indicador_e              CHAR(05),
    cod_error_origen         SMALLINT,
    folio_edo_cta            CHAR(08),
    cod_afore_ced            SMALLINT,
    salario_base_comis       DECIMAL(12,2),
    salario_actual           DECIMAL(12,2),
    fecha_actualiza_sa       DATE,
    coduni_n1                CHAR(10),
    indicador_comision       SMALLINT,
    codven                   CHAR(10),
    coor_captura             SMALLINT,
    lote_captura             SMALLINT,
    folio_captura            SMALLINT,
    sello_electronico        CHAR(24),
    factualiza               DATE,
    nombre_archivo           CHAR(20)
  END RECORD

  LET bandera   = 0
  LET cont      = 0
  LET vn_seguro = NULL

  ERROR "COMIENZA LA VALIDACION DE LA INFORMACION PASO 3 DE 4" #MLM-3842 CPL-2609
  SLEEP 2

  CALL fn_taa_ctr_proceso(3,1)

  DATABASE safre_tmp
    DROP TABLE tmp_afi_sol_tar;

    UPDATE STATISTICS FOR TABLE det_tra_rcv;
    UPDATE STATISTICS FOR TABLE det_tra_viv;

    LET v_sql = "\n INSERT INTO tmp_afi_sol_val ",
                "\n SELECT a.n_seguro, a.n_unico, a.tipo_solicitud, a.n_folio, a.frecafor, a.status_interno ",
                "\n FROM   safre_af:afi_solicitud a, ",
                "\n        det_tra_viv b ",
                "\n WHERE  a.n_seguro = b.n_seguro "
    PREPARE ps_tmp_afi_val FROM v_sql
    EXECUTE ps_tmp_afi_val

    LET v_sql = "\n INSERT INTO tmp_afi_mae_val ",
                "\n SELECT a.n_seguro, a.tipo_solicitud ",
                "\n FROM   safre_af:afi_mae_afiliado a, ",
                "\n        det_tra_viv b ",
                "\n WHERE  a.n_seguro = b.n_seguro "
    PREPARE ps_tmp_afi_mae FROM v_sql
    EXECUTE ps_tmp_afi_mae

    CREATE INDEX tmp_afi_sol_val_1 ON tmp_afi_sol_val(nss)
    CREATE INDEX tmp_afi_sol_val_2 ON tmp_afi_sol_val(curp)
    CREATE INDEX tmp_afi_sol_val_3 ON tmp_afi_sol_val(tpo_solicitud)
    CREATE INDEX tmp_afi_sol_val_4 ON tmp_afi_sol_val(edo_interno)
    UPDATE STATISTICS FOR TABLE tmp_afi_sol_val;

    CREATE INDEX tmp_afi_mae_val_1 ON tmp_afi_mae_val(nss)
    CREATE INDEX tmp_afi_mae_val_3 ON tmp_afi_mae_val(tpo_solicitud)
    UPDATE STATISTICS FOR TABLE tmp_afi_mae_val;
  DATABASE safre_af

  DECLARE cur_viv CURSOR FOR
  SELECT *
  FROM   safre_tmp:det_tra_viv
  WHERE  nom_archivo = generar  #MLM-3842 CPL-2609
  FOREACH cur_viv INTO reg_det_tra_viv.*
    # Traspaso Receptora(Normal)
    IF reg_det_tra_viv.ident_operacion = '09' THEN 
       ----- Preapertura de cuenta traspasos indebidos -----
       # 21 TRASPASOS INDEBIDOS APERTURA CUENTAS TRASPASOS INDEBIDOS
       # 73 Traspaso Indebido por CURP
       IF reg_det_tra_viv.tipo_traspaso = '21' OR
          reg_det_tra_viv.tipo_traspaso = '73' THEN
          LET g_tip_solic = NULL

          IF reg_det_tra_viv.tipo_traspaso = '21' THEN #TRASPASOS INDEBIDOS APERTURA CUENTAS TRASPASOS INDEBIDOS
             LET vn_folio    = 0
             LET g_tip_solic = 13 #TRASPASO INDEBIDO POR NSS

             SELECT 'X'
             FROM   safre_tmp:tmp_afi_sol_val a
             WHERE  a.nss             =   reg_det_tra_viv.n_seguro
             AND    a.tpo_solicitud   =   g_tip_solic #13 TRASPASO INDEBIDO POR NSS
             AND    a.edo_interno    IN (70, 75, 100)
             AND    a.f_captura       =   reg_det_tra_viv.fecha_presentacion
             GROUP BY 1 #CPL-2389
          ELSE #73 Traspaso Indebido por CURP
             LET vn_folio    = 0
             LET g_tip_solic = 17 #TRASPASO INDEBIDO POR CURP

             SELECT 'X'
             FROM   safre_tmp:tmp_afi_sol_val a
             WHERE  a.curp            =   reg_det_tra_viv.n_unico
             AND    a.tpo_solicitud   =   g_tip_solic #17 TRASPASO INDEBIDO POR CURP
             AND    a.edo_interno    IN (70, 75, 100)
             AND    a.f_captura       =   reg_det_tra_viv.fecha_presentacion
             GROUP BY 1 #CPL-2389
          END IF

          IF SQLCA.SQLCODE <> 0 THEN # NO LO ENCONTRO CON ESTAS CARACTERISTICAS
             SELECT MAX(a.n_folio)
             INTO   vn_folio
             FROM   afi_solicitud a
             WHERE  a.tipo_solicitud = g_tip_solic #13 TRASPASO INDEBIDO POR NSS . 17  TRASPASO INDEBIDO POR CURP
             IF vn_folio IS NULL OR
                vn_folio  = 0    THEN
                LET vn_folio = 1
             ELSE
                LET vn_folio = vn_folio + 1
             END IF

             IF reg_det_tra_viv.tipo_traspaso = '21' THEN #TRASPASOS INDEBIDOS APERTURA CUENTAS TRASPASOS INDEBIDOS
                INITIALIZE reg_mto.* TO NULL
                INITIALIZE reg_his.* TO NULL

                SELECT *
                INTO   reg_mto.*
                FROM   afi_mae_afiliado m
                WHERE  m.n_seguro = reg_det_tra_viv.n_seguro
                IF s_codigo_afore = 568 THEN --CPL
                   #CPL-1507 INICIO
                   SELECT *
                   INTO   reg_his.*
                   FROM   afi_solicitud
                   WHERE  n_folio        = reg_mto.n_folio
                   AND    tipo_solicitud = g_tip_solic
                   AND    n_seguro       = reg_det_tra_viv.n_seguro
                   #CPL-1507 FIN
                END IF
             ELSE #73 Traspaso Indebido por CURP
                INITIALIZE reg_mto.* TO NULL
                INITIALIZE reg_his.* TO NULL

                SELECT *
                INTO   reg_mto.*
                FROM   afi_mae_afiliado m
                WHERE  m.n_unico = reg_det_tra_viv.n_unico
                IF s_codigo_afore = 568 THEN --CPL
                   #CPL-1507 INICIO
                   SELECT *
                   INTO   reg_his.*
                   FROM   afi_solicitud
                   WHERE  n_folio        = reg_mto.n_folio
                   AND    tipo_solicitud = g_tip_solic
                   AND    n_unico        = reg_det_tra_viv.n_unico
                   #CPL-1507 FIN
                END IF
             END IF

             #-- INSERTA EN AFI SOLICITUD --
             LET vfol_org                  = reg_mto.n_folio
             LET vts_org                   = reg_mto.tipo_solicitud

             LET reg_mto.n_folio           = vfol_org          #732
             LET reg_mto.tipo_solicitud    = g_tip_solic #13  TRASPASO INDEBIDO POR NSS . 17  TRASPASO INDEBIDO POR CURP
             LET reg_mto.frecafor          = reg_det_tra_viv.fecha_presentacion
             LET reg_mto.fentcons          = reg_det_tra_viv.fecha_presentacion
             LET reg_mto.fecha_elaboracion = reg_det_tra_viv.fecha_presentacion
             LET reg_mto.status_interno    = 70 #ESPERA DE RECURSOS
             LET reg_mto.indicador_e       = vfol_org
             LET reg_mto.fecha_envio       = null
             LET reg_mto.tip_prob          = 6
             LET reg_mto.fol_prob          = null
             LET reg_mto.doc_prob          = null
             LET reg_mto.femision          = null
             LET reg_mto.paterno           = reg_det_tra_viv.paterno
             LET reg_mto.materno           = reg_det_tra_viv.materno
             LET reg_mto.nombres           = reg_det_tra_viv.nombre
             LET reg_mto.n_rfc             = reg_det_tra_viv.rfc
             LET reg_mto.ind_infonavit     = reg_det_tra_viv.ident_garantia
             LET reg_mto.cod_afore_ced     = reg_det_tra_viv.cve_ced_cuenta
             LET reg_mto.usuario           = g_usuario
             LET reg_mto.cod_promotor      = '0000000000'
             LET reg_mto.codven            = '0000000000'

             IF reg_mto.n_unico   IS NULL OR
                reg_mto.n_unico[1] = " "  THEN
                IF reg_det_tra_viv.n_unico    IS NOT NULL AND
                   reg_det_tra_viv.n_unico[1] <> " "      THEN
                   LET reg_mto.n_unico = reg_det_tra_viv.n_unico
                END IF
             ELSE
                IF reg_det_tra_viv.n_unico    IS NOT NULL        AND
                   reg_det_tra_viv.n_unico[1] <> " "             AND
                   reg_det_tra_viv.n_unico    <> reg_mto.n_unico THEN
                   LET reg_mto.n_unico = reg_det_tra_viv.n_unico
                END IF
             END IF

             IF reg_mto.cod_esq_comision IS NULL THEN
                LET reg_mto.cod_esq_comision = 0
             END IF

             IF s_codigo_afore = 568 THEN --CPL
                #CPL-1507
                #YA QUE SE PRESENTA EL CASO EN QUE PUEDEN RECIBIRSE VARIAS VECES CUENTAS POR
                #MEDIO DE TRASPASO INDEBIDO SE VERIFICARa SI EXISTE EL REGISTRO, EN CASO DE SER ASI
                #SE PASA AL HISTORICO (afi_his_solicitud) Y SE PERMITE LA CREACIoN DE UNA NUEVA SOLICITUD
                IF reg_his.n_folio IS NOT NULL AND 
                   reg_his.n_folio <> 0        THEN
                   LET reg_his.factualiza     = v_hoy;
                   LET reg_his.nombre_archivo = generar;

                   INSERT INTO afi_his_solicitud VALUES(reg_his.*)

                   IF SQLCA.SQLERRD[3] <> 0 THEN #una vez que actualice
                      DELETE
                      FROM   afi_solicitud
                      WHERE  n_folio        = reg_mto.n_folio
                      AND    tipo_solicitud = g_tip_solic
                      AND    n_unico        = reg_det_tra_viv.n_unico

                      DELETE
                      FROM   afi_solicitud
                      WHERE  n_folio        = reg_mto.n_folio
                      AND    tipo_solicitud = g_tip_solic
                      AND    n_unico        = reg_det_tra_viv.n_seguro
                   END IF
                END IF
             END IF

             INSERT INTO afi_solicitud VALUES (reg_mto.*)

             LET g_busca_nss = NULL

             IF reg_det_tra_viv.tipo_traspaso = '21' THEN #TRASPASOS INDEBIDOS APERTURA CUENTAS TRASPASOS INDEBIDOS
                LET g_busca_nss =  reg_det_tra_viv.n_seguro #13 TRASPASO INDEBIDO POR NSS
             ELSE #73 Traspaso Indebido por CURP
                LET g_busca_nss = reg_mto.n_seguro          #17 TRASPASO INDEBIDO POR CURP
             END IF

             #-- INSERTA EN AFI DOMICILIO --
             DECLARE cur_dom CURSOR FOR
             SELECT *
             FROM   afi_domicilio d
             WHERE  d.nss            = g_busca_nss
             AND    d.n_folio        = vfol_org
             AND    d.tipo_solicitud = vts_org
             FOREACH cur_dom INTO reg_dom.*
               LET reg_dom.n_folio        = vfol_org     #732
               LET reg_dom.tipo_solicitud = g_tip_solic

               IF s_codigo_afore = 568 THEN --CPL
                  SELECT UNIQUE "X"
                  FROM   afi_domicilio d
                  WHERE  d.nss              = g_busca_nss
                  AND    d.n_folio          = vfol_org
                  AND    d.tipo_solicitud   = g_tip_solic
                  IF SQLCA.SQLCODE <> 0 THEN
                     INSERT INTO afi_domicilio VALUES (reg_dom.*)
                  END IF
               ELSE
                  INSERT INTO afi_domicilio VALUES (reg_dom.*)
               END IF
             END FOREACH

             CLOSE cur_dom
             FREE cur_dom

             #-- INSERTA EN AFI TELEFONO --
             DECLARE cur_tel CURSOR FOR
             SELECT *
             FROM   afi_telefono t
             WHERE  t.n_folio        = vfol_org
             AND    t.tipo_solicitud = vts_org
             AND    t.nss            = g_busca_nss
             FOREACH cur_tel INTO reg_tel.*
               LET reg_tel.n_folio        = vfol_org     #732
               LET reg_tel.tipo_solicitud = g_tip_solic

               IF s_codigo_afore = 568 THEN --CPL
                  SELECT UNIQUE "X"
                  FROM   afi_telefono t
                  WHERE  t.n_folio        = vfol_org
                  AND    t.tipo_solicitud = g_tip_solic
                  AND    t.nss            = g_busca_nss
                  IF SQLCA.SQLCODE <> 0 THEN
                     INSERT INTO afi_telefono VALUES (reg_tel.*)
                  END IF
               ELSE
                  INSERT INTO afi_telefono VALUES (reg_tel.*)
               END IF
             END FOREACH

             CLOSE cur_tel
             FREE cur_tel

             #-- INSERTA EN AFI afi_correo_elect --
             DECLARE cur_cor CURSOR FOR
             SELECT *
             FROM   afi_correo_elect m
             WHERE  m.n_folio        = vfol_org
             AND    m.tipo_solicitud = vts_org
             AND    m.nss            = g_busca_nss
             FOREACH cur_cor INTO reg_cor.*
               LET reg_cor.n_folio        = vfol_org    #732
               LET reg_cor.tipo_solicitud = g_tip_solic

               IF s_codigo_afore = 568 THEN --CPL
                  SELECT UNIQUE "X"
                  FROM   afi_correo_elect m
                  WHERE  m.n_folio        = vfol_org
                  AND    m.tipo_solicitud = g_tip_solic
                  AND    m.nss            = g_busca_nss
                  IF SQLCA.SQLCODE <> 0 THEN
                     INSERT INTO afi_correo_elect VALUES (reg_cor.*)
                  END IF
               ELSE
                  INSERT INTO afi_correo_elect VALUES (reg_cor.*)
               END IF
             END FOREACH

             CLOSE cur_cor
             FREE cur_cor

             #-- INSERTA EN AFI afi_beneficiario --
             DECLARE cur_ben CURSOR FOR
             SELECT *
             FROM   afi_mae_benefici b
             WHERE  b.n_seguro       = g_busca_nss
             AND    b.n_folio        = vfol_org
             AND    b.tipo_solicitud = vts_org
             FOREACH cur_ben INTO reg_ben.*
               LET reg_ben.n_folio        = vfol_org    #732
               LET reg_ben.tipo_solicitud = g_tip_solic

               IF s_codigo_afore = 568 THEN --CPL
                  SELECT UNIQUE "X"
                  FROM   afi_beneficiario b
                  WHERE  b.n_folio        = vfol_org
                  AND    b.tipo_solicitud = g_tip_solic
                  AND    b.n_seguro       = g_busca_nss
                  IF SQLCA.SQLCODE <> 0 THEN
                     INSERT INTO afi_beneficiario VALUES (reg_ben.*)
                   END IF
               ELSE
                  INSERT INTO afi_beneficiario VALUES (reg_ben.*)
               END IF
             END FOREACH

             CLOSE cur_ben
             FREE cur_ben

             #-- INSERTA EN AFI afi_patron --
             DECLARE cur_pat CURSOR FOR
             SELECT *
             FROM   afi_mae_patron p
             WHERE  p.n_folio        = vfol_org
             AND    p.tipo_solicitud = vts_org
             AND    p.n_seguro       = g_busca_nss
             FOREACH cur_pat INTO reg_pat.*
               LET reg_pat.n_folio        = vfol_org     #732
               LET reg_pat.tipo_solicitud = g_tip_solic

               IF s_codigo_afore = 568 THEN --CPL
                  SELECT UNIQUE "X"
                  FROM   afi_patron p
                  WHERE  p.n_folio        = vfol_org
                  AND    p.tipo_solicitud = g_tip_solic
                  AND    p.n_seguro       = g_busca_nss
                  IF SQLCA.SQLCODE <> 0 THEN
                      INSERT INTO afi_patron VALUES (reg_pat.*)
                  END IF
               ELSE
                  INSERT INTO afi_patron VALUES (reg_pat.*)
               END IF
             END FOREACH

             CLOSE cur_pat
             FREE cur_pat

             #-- INSERTA EN AFI afi_ctr_identif --
             DECLARE cur_ide CURSOR FOR
             SELECT *
             FROM   afi_ctr_identif i
             WHERE  i.n_folio        = vfol_org
             AND    i.tipo_solicitud = vts_org
             AND    i.n_seguro       = g_busca_nss
             FOREACH cur_ide INTO reg_ide.*
               LET reg_ide.n_folio        = vfol_org    #732
               LET reg_ide.tipo_solicitud = g_tip_solic

               IF s_codigo_afore = 568 THEN --CPL
                  SELECT UNIQUE "X"
                  FROM   afi_ctr_identif i
                  WHERE  i.n_folio        = vfol_org
                  AND    i.tipo_solicitud = g_tip_solic
                  AND    i.n_seguro       = g_busca_nss
                  IF SQLCA.SQLCODE <> 0 THEN
                     INSERT INTO afi_ctr_identif VALUES (reg_ide.*)
                  END IF
               ELSE
                  INSERT INTO afi_ctr_identif VALUES (reg_ide.*)
               END IF
             END FOREACH

             CLOSE cur_ide
             FREE cur_ide

             LET bandera  = 0
             LET vbandera = 0
          ELSE
             LET bandera  = 0
             LET vbandera = 0
          END IF --FIN VALIDA EXISTE SOLICITUD APERTURA CUENTAS TRASPASOS INDEBIDOS
       ELSE #--ORIGEN DE TRASPASO <> DE 21 Y 73
          #OP13 PLUS
          #AQUI DEBEN APARECER LAS CUENTAS YA APERTURADAS
          #TRASPASO 01 Y 71
          SELECT "X"
          FROM   safre_tmp:tmp_afi_mae_val ma
          WHERE  ma.nss = reg_det_tra_viv.n_seguro
          GROUP BY 1
          IF SQLCA.SQLCODE <> 0 THEN  # NO LO ENCONTRO
             IF s_codigo_afore = 568 THEN --CPL
                LET bandera  = 1
                LET vbandera = "1VM"                                #1038
             END IF

             SELECT "X"
             FROM   safre_tmp:tmp_afi_sol_val ms
             WHERE  ms.nss          = reg_det_tra_viv.n_seguro
             AND    ms.edo_interno IN (50,70)
             GROUP BY 1
             IF SQLCA.SQLCODE <> 0 THEN
                IF (s_codigo_afore  = 568    AND  --CPL
                    vbandera       <> "1VM") OR
                    s_codigo_afore <> 568    THEN --CPL
                    LET bandera  = 1
                    LET vbandera = "1V"
                END IF
             END IF
          END IF --FIN VALIDA EXISTE EN EL MAESTRO

          #permite pasar la carga de archivo de TAA para asignados de la prestadora
          IF bandera THEN
             IF reg_det_tra_viv.cve_ced_cuenta = 531 THEN
                SELECT "X"
                FROM   safre_tmp:tmp_afi_mae_val ma
                WHERE  ma.nss           = reg_det_tra_viv.n_seguro
                AND    ma.tpo_solicitud = 5
                GROUP BY 1
                IF STATUS <> NOTFOUND THEN
                   LET bandera  = 0
                   LET vbandera = 0
                END IF
             END IF
          END IF

          IF bandera THEN
             IF reg_det_tra_viv.tipo_traspaso = '83' OR
                reg_det_tra_viv.tipo_traspaso = '84' OR
                reg_det_tra_viv.tipo_traspaso = '85' OR
                reg_det_tra_viv.tipo_traspaso = '51' OR 
                reg_det_tra_viv.tipo_traspaso = '53' THEN -- CPL3053. nuevos fondos generaciones

                SELECT "X"
                FROM   safre_tmp:tmp_afi_mae_val ma
                WHERE  ma.nss = reg_det_tra_viv.n_seguro
                GROUP BY 1
                IF STATUS <> NOTFOUND THEN
                   LET bandera  = 0
                   LET vbandera = 0
                END IF
             END IF
          END IF --FIN VALIDA EXISTE EN EL MAESTRO TIPO TRASP 83,84 y 85
       END IF -- FIN VALIDA TIPO TRASPASO INDEBIDO

       LET vn_seguro = reg_det_tra_viv.n_seguro
    # TIPO OPERACION 12 Traspaso Receptora(Complementaria)
    ELSE 
       IF reg_det_tra_viv.tipo_traspaso <> '73' THEN
          IF reg_det_tra_viv.nss_unificador IS NULL OR
              reg_det_tra_viv.nss_unificador = "           " THEN
               SELECT "X"
                 FROM afi_mae_afiliado ma
                WHERE ma.n_seguro = reg_det_tra_viv.n_seguro
               IF SQLCA.SQLCODE <> 0 THEN
                   LET vn_seguro = reg_det_tra_viv.n_seguro
                   LET bandera = 1
                   LET vbandera = "3V"
               ELSE
                   LET vn_seguro = reg_det_tra_viv.n_seguro
                   LET bandera   = 0
                   LET vbandera  = 0
               END IF
           ELSE
              IF reg_det_tra_viv.nss_unificador <> reg_det_tra_viv.n_seguro THEN
                 SELECT "X"
                   FROM afi_mae_afiliado ma
                  WHERE ma.n_seguro = reg_det_tra_viv.nss_unificador
                 IF SQLCA.SQLCODE <> 0 THEN
                     SELECT "X"
                       FROM afi_mae_afiliado ma
                      WHERE ma.n_seguro = reg_det_tra_viv.n_seguro
                     IF SQLCA.SQLCODE <> 0 THEN
                         LET vn_seguro = reg_det_tra_viv.n_seguro
                         LET bandera = 1
                         LET vbandera = "4U"
                     ELSE
                         LET vn_seguro                   = reg_det_tra_viv.n_seguro
                         LET reg_det_tra_viv.nss_cedente = reg_det_tra_viv.nss_unificador
                         LET bandera = 0
                         LET vbandera = 0
                     END IF
                 ELSE
                     LET vn_seguro                   = reg_det_tra_viv.nss_unificador
                     LET reg_det_tra_viv.nss_cedente = reg_det_tra_viv.n_seguro
                     LET bandera = 0
                     LET vbandera = 0
                 END IF
              END IF
           END IF
       ELSE
          LET vn_seguro = reg_det_tra_viv.n_seguro      #1383
       END IF
    END IF  
    # FIN DE VALIDACION DE TIPO OPERACION 09 RECEPTORA NORMAL Y TIPO DE OPERACION 12 RECEPTORA COMPLEMENTARIA

    IF bandera THEN
       UPDATE safre_tmp:det_tra_viv
       SET    n_seguro      = vn_seguro,
              nss_cedente   = reg_det_tra_viv.nss_cedente,
              estado_reg    = 2,
              cve_cod_rech  = vbandera
       WHERE  n_seguro      = reg_det_tra_viv.n_seguro
       AND    cont_servicio = reg_det_tra_viv.cont_servicio
       AND    nom_archivo   = generar    #MLM-3842 CPL-2609

       -- se actualizan a rechazado tabla de indicadores
       UPDATE taa_indicadores_op09
       SET    estado_reg   = 2,
              cve_cod_rech = vbandera       
       WHERE  nss          = reg_det_tra_viv.n_seguro
       AND    nom_archivo  = generar  
    ELSE
       UPDATE safre_tmp:det_tra_viv
       SET    n_seguro      = vn_seguro,
              nss_cedente   = reg_det_tra_viv.nss_cedente,
              estado_reg    = 1
       WHERE  n_seguro      = reg_det_tra_viv.n_seguro
       AND    cont_servicio = reg_det_tra_viv.cont_servicio
       AND    nom_archivo   = generar    #MLM-3842 CPL-2609

       -- se actualizan a rechazado tabla de indicadores
       UPDATE taa_indicadores_op09
       SET    estado_reg  = 1       
       WHERE  nss         = reg_det_tra_viv.n_seguro
       AND    nom_archivo = generar

       --CPL-3053-- se realiza marcaje de Cuentas   
       IF reg_det_tra_rcv.tipo_traspaso = 53 THEN
          LET pmarca_entra    = 290
          LET pestado_marca   = 0
          LET pcodigo_rechazo = 0
          LET v_corr          = 0 
          LET pmarca_causa    = 290

          LET v_sql = ""
          LET v_sql = "EXECUTE PROCEDURE marca_cuenta(", "'",reg_det_tra_viv.n_seguro,"'",
                                                         ",",pmarca_entra,
                                                         ",",v_corr,
                                                         ",",pestado_marca,
                                                         ",",pcodigo_rechazo,
                                                         ",",pmarca_causa,
                                                         ",","'","'", ",",
                                                         "'",g_usuario,"'",")"
          LET v_sql = v_sql CLIPPED

          PREPARE clausula_spl_290 FROM v_sql
          DECLARE cursor_marca_290 CURSOR FOR clausula_spl_290
          OPEN cursor_marca_290
            FETCH cursor_marca_290 INTO xcodigo_marca, xcodigo_rechazo
          CLOSE cursor_marca_290
       END IF

       IF reg_det_tra_rcv.tipo_traspaso = 54 THEN
          LET pmarca_entra    = 298
          LET pestado_marca   = 0
          LET pcodigo_rechazo = 0
          LET v_corr          = 0 
          LET pmarca_causa    = 298

          LET v_sql = ""
          LET v_sql = "EXECUTE PROCEDURE marca_cuenta(", "'",reg_det_tra_viv.n_seguro,"'",
                                                         ",",pmarca_entra,
                                                         ",",v_corr,
                                                         ",",pestado_marca,
                                                         ",",pcodigo_rechazo,
                                                         ",",pmarca_causa,
                                                         ",","'","'", ",",
                                                         "'",g_usuario,"'",")"
          LET v_sql = v_sql CLIPPED

          PREPARE clausula_spl_298 FROM v_sql
          DECLARE cursor_marca_298 CURSOR FOR clausula_spl_298
          OPEN cursor_marca_298
            FETCH cursor_marca_298 INTO xcodigo_marca, xcodigo_rechazo
          CLOSE cursor_marca_298   
       END IF
    END IF

    LET bandera  = 0
    LET vbandera = 0

    DECLARE cur_rcv CURSOR FOR
    SELECT *
    FROM   safre_tmp:det_tra_rcv
    WHERE  n_seguro    = reg_det_tra_viv.n_seguro
    AND    nom_archivo = generar #MLM-3842 CPL-2609
    FOREACH cur_rcv INTO reg_det_tra_rcv.*
      IF reg_det_tra_rcv.ident_operacion = '09' THEN
         #OP13 PLUS
         #AQUI DEBEN APARECER LAS CUENTAS YA APERTURADAS
         #TRASPASO 01 Y 71
         SELECT "X"
         FROM   safre_tmp:tmp_afi_mae_val
         WHERE  nss = reg_det_tra_rcv.n_seguro
         GROUP BY 1
         IF SQLCA.SQLCODE <> 0 THEN # NO LO ENCONTRO
            IF (s_codigo_afore  = 568    AND --CPL
                vbandera       <> "5RM") OR
                s_codigo_afore <> 568    THEN --CPL
                LET bandera  = 1
                LET vbandera = "5RM"                                #1038
            END IF

            SELECT "X"
            FROM   safre_tmp:tmp_afi_sol_val
            WHERE  nss          = reg_det_tra_rcv.n_seguro
            AND    edo_interno IN (50,70)
            GROUP BY 1
            IF SQLCA.SQLCODE <> 0 THEN
               LET bandera  = 1
               LET vbandera = "5R"
            END IF
         END IF

         #permite pasar la carga de archivo de TAA para asignados de la prestadora
         IF bandera THEN
            IF reg_det_tra_viv.cve_ced_cuenta = 531 THEN
               SELECT "X"
               FROM   safre_tmp:tmp_afi_mae_val ma
               WHERE  ma.nss           = reg_det_tra_viv.n_seguro
               AND    ma.tpo_solicitud = 5
               GROUP BY 1
               IF STATUS <> NOTFOUND THEN
                  LET bandera  = 0
                  LET vbandera = 0
               END IF
            END IF
         END IF

         IF bandera THEN
            IF reg_det_tra_viv.tipo_traspaso = '83' OR
               reg_det_tra_viv.tipo_traspaso = '84' OR
               reg_det_tra_viv.tipo_traspaso = '85' OR
               reg_det_tra_viv.tipo_traspaso = '51' OR 
               reg_det_tra_viv.tipo_traspaso = '53'  THEN -- CPL3053. nuevos fondos generaciones

               SELECT "X"
               FROM   safre_tmp:tmp_afi_mae_val ma
               WHERE  ma.nss = reg_det_tra_viv.n_seguro
               GROUP BY 1
               IF STATUS <> NOTFOUND THEN
                  LET bandera  = 0
                  LET vbandera = 0
               END IF
            END IF
         END IF --FIN VALIDA EXISTE EN EL MAESTRO TIPO TRASP 83,84 y 85
      END IF

      IF bandera THEN
         UPDATE safre_tmp:det_tra_rcv
         SET    n_seguro      = vn_seguro,
                estado_reg    = 2,
                cve_cod_rech  = vbandera
         WHERE  n_seguro      = reg_det_tra_rcv.n_seguro
         AND    cont_servicio = reg_det_tra_rcv.cont_servicio
         AND    nom_archivo   = generar    #MLM-3842 CPL-2609
      ELSE
         UPDATE safre_tmp:det_tra_rcv
         SET    n_seguro      = vn_seguro,
                estado_reg    = 1
         WHERE  n_seguro      = reg_det_tra_rcv.n_seguro
         AND    cont_servicio = reg_det_tra_rcv.cont_servicio
         AND    nom_archivo   = generar    #MLM-3842 CPL-2609
      END IF

      LET bandera  = 0
      LET vbandera = 0
      CALL inicializa()
    END FOREACH

    CLOSE cur_rcv
    FREE cur_rcv

    LET bandera = 0
    LET vbandera = 0
  END FOREACH

  CLOSE cur_viv
  FREE cur_viv

  DATABASE safre_tmp
    DROP TABLE tmp_afi_sol_val;
    DROP TABLE tmp_afi_mae_val;
  DATABASE safre_af

  CALL fn_taa_ctr_proceso(3,2)
END FUNCTION     #lee_archivo_plano()

FUNCTION imprime_reporte()
  INITIALIZE datos.* TO NULL
 
  LET v_hora = TIME

  SELECT *
  INTO   g_afore.*
  FROM   tab_afore_local

  LET g_lista = g_param_taa.ruta_listados CLIPPED, "/", g_usuario CLIPPED, ".afili_rec_", g_id_operacion USING "&&", ".", v_hoy USING "DDMMYY", "_", v_hora[1,2], v_hora[4,5] CLIPPED

  START REPORT listado TO g_lista
    DECLARE cur1 CURSOR for
    SELECT a.cve_ced_cuenta, b.afore_desc, a.tipo_traspaso, COUNT(*)
    FROM   safre_tmp:det_tra_viv a, tab_afore b   --REVISAR
    WHERE  a.tipo_registro  = "02"
    AND    a.cve_ced_cuenta = b.afore_cod
    AND    a.nom_archivo    = generar    #MLM-3842 CPL-2609
    GROUP BY 1,2,3
    ORDER BY 1,3
    IF STATUS = NOTFOUND THEN
       IF bnd_proceso THEN
          DISPLAY "Program stopped, NO SE IMPRIME REPORTE"
       ELSE
          ERROR "NO SE IMPRIME REPORTE"
          SLEEP 2
       END IF
    ELSE
       FOREACH cur1 into datos.*
         LET tot_total = tot_total + datos.tot_parcial
         OUTPUT TO REPORT listado(datos.*)
       END FOREACH
    END IF
  FINISH REPORT listado

  LET g_lista = g_param_taa.ruta_listados CLIPPED, "/", g_usuario CLIPPED, ".afili_rec_", g_id_operacion USING "&&", ".", v_hoy using "DDMMYY"

  IF s_codigo_afore <> 564 THEN --MLM
     LET COMANDO = "lp ",g_lista  #MLM-1642
     RUN COMANDO # MLM-1642
  END IF

  IF bnd_proceso THEN
     DISPLAY "REPORTE GENERADO"
  ELSE
     ERROR "TERMINA PROCESO DE INFORMACION, VERIFICA RECHAZOS"
     SLEEP 2
  END IF
END FUNCTION

REPORT listado(datos)
  DEFINE datos               RECORD
    cve_cede                 CHAR(03),
    des_afore                CHAR(25),
    tipo_trasp               CHAR(02),
    tot_parcial              INTEGER
  END RECORD

  OUTPUT
    PAGE LENGTH 60
    LEFT MARGIN 0
    RIGHT MARGIN 132
    TOP MARGIN 0
    BOTTOM MARGIN 0

  FORMAT
    PAGE HEADER
      PRINT COLUMN 4, '\033e\033(s218T\033(s11H\033(s7B',"INFORMACION DE AFILIADOS TRASPASADOS A AFORE ", campo.des_titulo, v_hoy USING "dd/mm/yyyy"

      IF id_op = 9 THEN
         PRINT COLUMN 4, '\033e\033(s218T\033(s11H\033(s7B',"                               TRASPASO NORMAL                               "
      ELSE
         PRINT COLUMN 4, '\033e\033(s218T\033(s11H\033(s7B',"                           TRASPASO COMPLEMENTARIO                           "
      END IF

      PRINT COLUMN 4, '\033e\033(s218T\033(s11H\033(s7B',"NOMBRE DEL ARCHIVO : ", generar, "    FECHA DE PRESENTACION : ", g_f_presentacion USING "dd/mm/yyyy"
      PRINT
      PRINT COLUMN 1, '\033e\033(s218T\033(s11H\033(s7N'
      PRINT COLUMN 2, campo.cod_afore, "     ", campo.raz_social
      PRINT
      PRINT COLUMN 1, "\332\304\304\304\304\304\304\304\302\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\302\304\304\304\304\304\304\304\304\302\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\277"
      PRINT COLUMN 1, "\263",COLUMN 3,"CLAVE ",COLUMN 8,"\263",COLUMN 20,"AFORE",COLUMN 43,"\263",COLUMN 45,"ORIGEN",COLUMN 52,"\263",COLUMN 54,"AFILIADOS RECIBIDOS",COLUMN 76,"\263"
      PRINT COLUMN 1, "\300\304\304\304\304\304\304\304\301\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\301\304\304\304\304\304\304\304\304\301\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\331"

    ON EVERY ROW
       PRINT PRINT
       COLUMN 04, datos.cve_cede USING "&&&",
       COLUMN 11, datos.des_afore,
       COLUMN 47, datos.tipo_trasp,
       COLUMN 60, datos.tot_parcial USING "&&&&&&"

       PRINT
       IF lineno > 57 THEN
          SKIP TO TOP OF PAGE
       END IF

    ON LAST ROW
       PRINT COLUMN 1, "\332\304\304\304\304\304\304\304\302\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\302\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\277"
       PRINT COLUMN 1, "\263",COLUMN 3,"TOTAL ","\263 ",COLUMN 52,"\263",COLUMN 60, tot_total USING "&&&&&&",COLUMN 76,"\263"
       PRINT COLUMN 1, "\300\304\304\304\304\304\304\304\301\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\301\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\331"
END REPORT

FUNCTION actualiza_operacion()
  UPDATE bat_ctr_operacion
  SET    estado_cod  = 4,
         fecha_fin   = CURRENT,
         nom_archivo = reg_bat.nombre_archivo
  WHERE  pid         = reg_bat.pid
  AND    proceso_cod = reg_bat.proceso_cod
  AND    opera_cod   = reg_bat.opera_cod
END FUNCTION

FUNCTION ventana_menu()
  OPEN WINDOW ventana_2 AT 4,4 WITH 19 ROWS, 74 COLUMNS ATTRIBUTE(BORDER, PROMPT LINE LAST-1)
    DISPLAY " TAAC001     ARCHIVO DE SALDOS DE TRASPASOS AFORE RECEPTORA                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "------------------------------------------------------------------------------" AT 4,1
    DISPLAY v_hoy USING " DD-MM-YYYY " AT 3,61 ATTRIBUTE(REVERSE)

    MENU "MENU "
      COMMAND "Carga " "Carga Archivo de Saldos TAA Receptora"
        CALL proceso_principal()
      COMMAND "Ver Rechazos" "Consulta Rechazos del Archivo de Saldos"
        CALL valida_rechazos()
      COMMAND "Salir"
        EXIT PROGRAM
    END MENU
  CLOSE WINDOW ventana_2
END FUNCTION

FUNCTION valida_rechazos()
  DEFINE v_fecha_presenta    DATE
  DEFINE v_fecha_liq         DATE

  #MLM-4029
  WHILE TRUE
    PROMPT "Nombre Archivo:  " FOR generar

    LET cuantos = 0

    SELECT COUNT(*)
    INTO   cuantos
    FROM   safre_tmp:det_tra_viv  --REVISAR
    WHERE  nom_archivo = generar #PST-1628
    IF cuantos = 0 THEN
       ERROR "Archivo Inexistente..."  SLEEP 2
       ERROR ""
       EXIT WHILE
    ELSE
       EXIT WHILE
    END IF
  END WHILE

  SELECT fecha_presentacion
  INTO   v_fecha_presenta
  FROM   safre_tmp:cza_tra_afo
  WHERE  nom_archivo     = generar  #MLM-3842 CPL-2609
  AND   (ident_operacion = "09"
  OR     ident_operacion = "12")     #1193
  IF STATUS = NOTFOUND THEN
     PROMPT "El archivo cargado no es de traspasos ordinarios.[Enter] Para salir" FOR enter
     RETURN
  ELSE
     SELECT COUNT(*)
     INTO   cuantos
     FROM   safre_tmp:det_tra_viv
     WHERE  estado_reg <> 1
     AND    nom_archivo = generar  #MLM-3842 CPL-2609
     IF cuantos > 0  THEN
        --CALL fechas(v_fecha_presenta) RETURNING v_fecha_liq
        --IF v_hoy <= v_fecha_liq THEN
        CALL ventana_rechazos()
     ELSE
        PROMPT "NO EXISTEN RECHAZOS. [Enter] Para salir." FOR enter
        RETURN
     END IF
  END IF
END FUNCTION

FUNCTION ventana_rechazos()
  DEFINE contador            INTEGER

  LET tot_reg_rch = 0
  LET posicion    = 1
  LET k           = 0

  UPDATE tab_cod_rech
  SET    tot_cod_rech = 0

  LET query_rch = " SELECT b.n_seguro,               ",
                  "        trim(b.paterno)||' '||trim(b.materno)||' '||trim(b.nombre), ",
                  "        b.tipo_traspaso,          ",
                  "        b.cve_ced_cuenta,         ",
                  "        b.cve_cod_rech            ",
                  " FROM   safre_tmp:det_tra_viv b   ",
                  " WHERE  b.estado_reg     <> 1     ",
                  " AND    b.nom_archivo     = ?     ",      #MLM-3842 CPL-2609
                  " AND   (b.ident_operacion = '09'  ",
                  " OR     b.ident_operacion = '12' )"       #1193

  PREPARE prep_rechazados FROM query_rch
  DECLARE cur_pant_rch CURSOR FOR prep_rechazados
  FOREACH cur_pant_rch USING generar INTO arr_pant_rech[posicion].*  #MLM-3842 CPL-2609
    LET vnum_cod_rech  = arr_pant_rech[posicion].cve_cod_rech

    UPDATE tab_cod_rech
    SET    tot_cod_rech = tot_cod_rech + 1
    WHERE  num_cod_rech = vnum_cod_rech

    LET posicion = posicion + 1

    IF posicion > 15000 THEN
       PROMPT "Se rebasa la capacidad del arreglo,presione [enter] para continuar" FOR enter
       SLEEP 4
       LET posicion = posicion - 1
       EXIT FOREACH
    END IF
  END FOREACH

  CLOSE cur_pant_rch
  FREE cur_pant_rch

  INITIALIZE arr_pant_rech[posicion].* TO NULL
  LET tot_reg_rch = posicion - 1

  IF tot_reg_rch >= 1 THEN
     OPEN WINDOW ventana_3 AT 4,4 WITH FORM "TAAC0012" ATTRIBUTE(BORDER)
       DISPLAY " TAAC001      ARCHIVO DE SALDOS DE TRASPASOS AFORE RECEPTORA                    " AT 2,1 ATTRIBUTE(REVERSE)
       DISPLAY "         < Ctrl-C > Salir    <Ctrl-P> Imprimir    <Ctrl-V> Ver CVE-CD-RCH       " AT 1,1 ATTRIBUTE(REVERSE)
       DISPLAY v_hoy USING " DD-MM-YYYY " AT 2,62 ATTRIBUTE(REVERSE)
       DISPLAY BY NAME tot_reg_rch

       CALL SET_COUNT (tot_reg_rch)

       DISPLAY ARRAY arr_pant_rech TO scr_1.*
         ON KEY (Control-p)
            LET v_hora  = TIME
            LET g_lista = g_param_taa.ruta_listados CLIPPED, "/", g_usuario CLIPPED,
                          ".reg_rech_", v_hoy using "DDMMYY", v_hora[1,2], v_hora[4,5] CLIPPED

            START REPORT imprime_rechazados TO g_lista
              FOR contador = 1 TO tot_reg_rch
                  OUTPUT TO REPORT imprime_rechazados (arr_pant_rech[contador].*, tot_reg_rch)
              END FOR
            FINISH REPORT imprime_rechazados

            ERROR "REPORTE GENERADO"
            SLEEP 2

         ON KEY (INTERRUPT)
            EXIT DISPLAY

         ON KEY (Control-v)
            LET j = ARR_CURR()
            LET vnum_cod_rech = arr_pant_rech[j].cve_cod_rech

            SELECT des_cod_rech
            INTO   vdes_cod_rech
            FROM   tab_cod_rech
            WHERE  num_cod_rech = vnum_cod_rech

            DISPLAY vdes_cod_rech AT 20,4 ATTRIBUTE(REVERSE)
            SLEEP 3
            DISPLAY g_x AT 20,4
       END DISPLAY

       IF int_flag = TRUE THEN
          CLOSE WINDOW ventana_3
          LET int_flag = FALSE
       END IF
  ELSE
     #ERROR "El archivo no tiene rechazos."
     #SLEEP 3
     #ERROR ""
  END IF

  RETURN
END FUNCTION       #ventana_rechazos()

REPORT imprime_rechazados(h, reg_total)
  DEFINE reg_total           INTEGER

  DEFINE h                   RECORD
    n_seguro                 LIKE safre_tmp:det_tra_viv.n_seguro,
    nombre                   CHAR(150),
    tipo_traspaso            LIKE safre_tmp:det_tra_viv.tipo_traspaso,
    cve_ced_cuenta           CHAR(03),
    cve_cod_rech             CHAR(03)
  END RECORD

  OUTPUT
    TOP MARGIN 1
    BOTTOM MARGIN 0
    LEFT MARGIN 0
    RIGHT MARGIN 0
    PAGE LENGTH 60

  FORMAT
    PAGE HEADER
      PRINT COLUMN 03, campo.cod_afore,
            COLUMN 11, campo.raz_social,
            COLUMN 68, TODAY USING "DD-MM-YYYY"
      SKIP 2 LINE
      PRINT COLUMN 10, "REGISTROS RECHAZADOS DE SALDOS DE TRASPASOS AFORE RECEPTORA"
      SKIP 2 LINE
      PRINT COLUMN 01, "--------------------------------------------------------------------------------"
      PRINT COLUMN 03, "NSS",
            COLUMN 26, "NOMBRE",
            COLUMN 60, "TIPO",
            COLUMN 67, "AFORE",
            COLUMN 76, "COD."
      PRINT COLUMN 57, "TRASPASO",
            COLUMN 67, "CEDENTE",
            COLUMN 76, "RECH."
      PRINT COLUMN 01, "--------------------------------------------------------------------------------"
      SKIP 1 LINE

      ON EVERY ROW
         PRINT h.n_seguro,
               COLUMN 13, h.nombre CLIPPED,
               COLUMN 60, h.tipo_traspaso,
               COLUMN 68, h.cve_ced_cuenta,
               COLUMN 76, h.cve_cod_rech

      ON LAST ROW
         SKIP 2 LINES
         PRINT COLUMN 30, "Total registros rechazados : ", reg_total USING "<<<<"
         PRINT
         PRINT COLUMN 02, "Total por Codigo de Rechazo"

         DECLARE c_t CURSOR FOR
         SELECT  num_cod_rech, des_cod_rech, tot_cod_rech
         FROM    tab_cod_rech
         FOREACH c_t INTO vnum_cod_rech, vdes_cod_rech, vtot_cod_rech
           PRINT COLUMN 02, vnum_cod_rech,
                 COLUMN 05, vdes_cod_rech,
                 COLUMN 40, vtot_cod_rech USING "##,##&"
         END FOREACH

         CLOSE c_t
         FREE c_t
END REPORT

FUNCTION control_archivo()
  DEFINE v_existe_arh        SMALLINT

  LET rechazo      = 0
  LET v_existe_arh = 0

  CALL fn_taa_ctr_proceso(4,1)

  SELECT COUNT(*)
  INTO   rechazo
  FROM   safre_tmp:det_tra_viv
  WHERE  estado_reg  = 2
  AND    nom_archivo = generar  #MLM-3842 CPL-2609
  IF rechazo > 0 THEN
     LET total = 0

     SELECT COUNT(*)
     INTO   total
     FROM   safre_tmp:det_tra_viv  --REVISAR
     WHERE  nom_archivo = generar  #MLM-3842 CPL-2609

     ERROR "COMIENZA CONTROL DE ARCHIVO PASO 4 DE 4" #MLM-3842 CPL-2609
     SLEEP 2

     IF total = rechazo THEN
     ELSE
       SELECT 'X'
       FROM   taa_ctr_traspaso
       WHERE  nombre_archivo = generar
       GROUP BY 1
       IF SQLCA.SQLCODE <> 0 THEN
          INITIALIZE r_control TO NULL

          LET r_control.nombre_archivo     = generar
          LET r_control.id_operacion       = g_id_operacion USING "&&"
          LET r_control.fecha_presentacion = g_f_presentacion
          LET r_control.fecha_recepcion    = CURRENT
          LET r_control.usr_recepcion      = g_usuario

          INSERT INTO taa_ctr_traspaso VALUES (r_control.*)
       END IF
     END IF

     ERROR "HUBO RECHAZOS, PREPARANDO INFORMACION"
     SLEEP 1
     CALL ventana_rechazos()
  ELSE
     CALL valida_saldo()

     LET total = 0

     SELECT COUNT(*)
     INTO   total
     FROM   safre_tmp:det_tra_viv  --REVISAR
     WHERE  nom_archivo = generar  #MLM-3842 CPL-2609

     LET aceptados = total - rechazo

     SELECT COUNT(*)
     INTO   v_existe_arh 
     FROM   taa_ctr_traspaso
     WHERE  (nombre_archivo     = generar
     OR      fecha_presentacion = g_f_presentacion)
     IF v_existe_arh = 0 THEN
        INITIALIZE r_control TO NULL

        LET r_control.nombre_archivo     = generar
        LET r_control.id_operacion       = g_id_operacion USING "&&"
        LET r_control.fecha_presentacion = g_f_presentacion
        LET r_control.fecha_recepcion    = CURRENT
        LET r_control.usr_recepcion      = g_usuario

        INSERT INTO taa_ctr_traspaso VALUES (r_control.*)
     ELSE
        UPDATE taa_ctr_traspaso
        SET    id_operacion       = v_id_operacion,    -- FALLA 
               fecha_presentacion = g_f_presentacion
        WHERE  nombre_archivo     = generar
        OR     fecha_presentacion = g_f_presentacion
     END IF

     #PROMPT "PROCESO FINALIZADO NORMALMENTE " FOR enter
  END IF

  LET g_tipo_arch      = NULL
  LET g_desc_tipo_arch = NULL

  SELECT A.ident_operacion
  INTO   g_tipo_arch
  FROM   safre_tmp:cza_tra_afo A
  WHERE  A.nom_archivo = generar  #MLM-3842 CPL-2609
  GROUP BY 1

  CASE g_tipo_arch
    WHEN 09 LET g_desc_tipo_arch = "Traspaso Receptora(Normal)"
    WHEN 12 LET g_desc_tipo_arch = "Traspaso Receptora(Complementario)"
    OTHERWISE
  END CASE

  DISPLAY "Nombre del Proceso : ", g_desc_tipo_arch  CLIPPED AT 08,05

  #--- Despliegue de TOTAL DE REGISTROS CARGADOS ---#
  DISPLAY "Total de Registros Cargados  : ", v_tot_reg_det2  AT 10,12

  #--- Despliegue de TOTAL DE REGISTROS  RECHAZADOS ---#
  LET tot_reg_rechs = 0

  SELECT COUNT(*)
  INTO   tot_reg_rechs
  FROM   safre_tmp:det_tra_viv
  WHERE  estado_reg  = 2
  AND    nom_archivo = generar  #MLM-3842 CPL-2609

  DISPLAY "Total de Registros Rechazados: ", tot_reg_rechs AT 11,12

  CALL fn_taa_ctr_proceso(4,2)

  PROMPT "PROCESO FINALIZADO NORMALMENTE" FOR enter
END FUNCTION

FUNCTION valida_siefore(v_sie)
  DEFINE v_sie               CHAR(08)

  SELECT 'X'
  FROM   tab_siefore
  WHERE  siefore_desc = v_sie
  GROUP BY 1
  IF SQLCA.SQLCODE <> 0 THEN
     IF bnd_proceso THEN
        DISPLAY "Program stopped. SIEFORE ", v_sie, " NO EXISTE EN EL CATALOGO"
        UPDATE safre_tmp:det_tra_viv
        SET    estado_reg   = 2,
               cve_cod_rech = "7S"
        WHERE  nom_archivo  = generar  #MLM-3842 CPL-2609
     ELSE
        PROMPT "LA SIEFORE ", v_sie, " NO EXISTE EN EL CATALOGO. [Enter] PARA SALIR"
        FOR enter
        UPDATE safre_tmp:det_tra_viv
        SET    estado_reg   = 2,
               cve_cod_rech = "7S"
        WHERE  nom_archivo  = generar  #MLM-3842 CPL-2609
     END IF

     EXIT PROGRAM
  END IF
END FUNCTION

FUNCTION valida_saldo()
  DEFINE
    v_subcta                 CHAR(02),
    v_saldo                  DECIMAL(15,2),
    c_subcta                 CHAR(02),
    c_saldo                  DECIMAL(15,2),
    dif_saldo                DECIMAL(15,2)

  DEFINE v_cadena            CHAR(5000)
  DEFINE v_indx0             SMALLINT
  DEFINE r_val_rcv           RECORD
    v_folio                  INTEGER,
    v_cont_servicio          DECIMAL(10,0),
    v_cve_ced_cuenta         CHAR(03),
    v_tipo_traspaso          SMALLINT,
    v_fecha_mov_banxico      DATE,
    v_n_seguro               CHAR(11),
    v_cve_subcta             CHAR(02),
    v_import_sub             DECIMAL(15,2),
    v_no_tot_acc             DECIMAL(16,6),
    v_siefore                CHAR(08),
    v_precio_acc             DECIMAL(15,6)
  END RECORD

  DEFINE v_indx2             SMALLINT

  LET v_cadena     = ""
    
  DATABASE safre_tmp
    CREATE TEMP TABLE saldos_val (subcta CHAR(02),
                                  saldo  DECIMAL(16,6))

    CREATE TEMP TABLE valida_rcv (folio                INTEGER      ,
                                  cont_servicio        DECIMAL(10,0),
                                  cve_ced_cuenta       CHAR(03)     ,
                                  tipo_traspaso        SMALLINT     ,
                                  fecha_mov_banxico    DATE         ,
                                  n_seguro             CHAR(11)     ,
                                  cve_subcta           CHAR(02)     ,
                                  import_sub           DECIMAL(15,2),
                                  no_tot_acc           DECIMAL(16,6),
                                  siefore              CHAR(08)     ,
                                  precio_acc           DECIMAL(15,6));

    INITIALIZE r_val_rcv.* TO NULL

    FOR v_indx0 = 1 TO 8
        LET v_cadena = "\n SELECT 0, ",
                       "\n        cont_servicio, ", 
                       "\n        cve_ced_cuenta, ",
                       "\n        tipo_traspaso, ",
                       "\n        TODAY, ",
                       "\n        n_seguro, ",
                       "\n        cve_subcta_", v_indx0 USING "#", ",",
                       "\n        import_sub_", v_indx0 USING "#", ",",
                       "\n        no_tot_acc_", v_indx0 USING "#", ",",
                       "\n        siefore_", v_indx0 USING "#", ",",
                       "\n        precio_acc_", v_indx0 USING "#",
                       "\n FROM   det_tra_rcv ",
                       "\n WHERE    nom_archivo = '", generar, "' ",
                        "\n AND  cve_subcta_", v_indx0 USING "#", " IS NOT NULL ;"

        PREPARE str_vrcv FROM v_cadena
        DECLARE cur_vrcv CURSOR FOR str_vrcv
        FOREACH cur_vrcv INTO r_val_rcv.* 
          INSERT INTO valida_rcv VALUES(r_val_rcv.*)
        END FOREACH

        CLOSE cur_vrcv
        FREE cur_vrcv
    END FOR

    CREATE INDEX valida_rcv2 ON valida_rcv(n_seguro);
    CREATE INDEX valida_rcv3 ON valida_rcv(cve_subcta);
    UPDATE STATISTICS FOR TABLE valida_rcv

    CALL val_saldos_rep()

    #######VALIDACION BLOQUE SUMARIO SUBCTA 1 a 28 #######
    LET v_cadena = ""

    FOR v_indx2 = 1 TO 28
        IF v_indx2 <= 9 THEN
           LET v_cadena = "\n SELECT cve_subcta_", v_indx2 USING "#", " ,tot_subcta_", v_indx2 USING "#",
                          "\n FROM   safre_tmp:sum_tra_afo ",
                          "\n WHERE  nom_archivo = '", generar, "'"
        ELSE
           LET v_cadena = "\n SELECT cve_subcta_", v_indx2 USING "##", " ,tot_subcta_", v_indx2 USING "##",
                          "\n FROM   safre_tmp:sum_tra_afo ",
                          "\n WHERE  nom_archivo = '", generar, "'"
        END IF
  
        PREPARE str_vsum FROM v_cadena
        DECLARE cur_vsum CURSOR FOR str_vsum
        FOREACH cur_vsum INTO v_subcta, v_saldo
          IF v_saldo IS NOT NULL AND
             v_saldo  > 0        THEN
             CALL compara_saldo(v_subcta, v_saldo)
          ELSE
             LET v_subcta = NULL
             LET v_saldo  = NULL
          END IF
        END FOREACH

        CLOSE cur_vsum
        FREE cur_vsum
    END FOR

    #######VALIDACION SALDO VIVIENDA 97 #######
    SELECT tot_sal_viv97
    INTO   v_saldo
    FROM   safre_tmp:sum_tra_afo
    WHERE  nom_archivo = generar  #MLM-3842 CPL-2609
    IF v_saldo IS NOT NULL AND v_saldo > 0 THEN
       SELECT SUM(sdo_viv_97)
       INTO   c_saldo
       FROM   safre_tmp:det_tra_viv
       WHERE  nom_archivo = generar  #MLM-3842 CPL-2609

       LET dif_saldo = c_saldo - v_saldo

       IF dif_saldo <> 0 THEN
          IF bnd_proceso THEN
             DISPLAY "Program stopped. DIFERENCIAS EN EL SALDO DE LA SUBCUENTA VIV 97"
             UPDATE safre_tmp:det_tra_viv
             SET    estado_reg   = 2,
                    cve_cod_rech = "8D"
             WHERE  nom_archivo  = generar  #MLM-3842 CPL-2609
          ELSE
             PROMPT "EXISTEN DIFERENCIAS EN EL SALDO DE LA SUBCUENTA VIV 97. [Enter] PARA SALIR"
             FOR enter
             UPDATE safre_tmp:det_tra_viv
             SET    estado_reg   = 2,
                    cve_cod_rech = "8D"
             WHERE  nom_archivo  = generar  #MLM-3842 CPL-2609
          END IF

          EXIT PROGRAM
       END IF
    ELSE
       LET v_subcta = NULL
       LET v_saldo  = NULL
    END IF

    #######VALIDACION SALDO VIVIENDA 92 #######
    SELECT tot_sal_viv92
    INTO   v_saldo
    FROM   safre_tmp:sum_tra_afo
    WHERE  nom_archivo = generar  #MLM-3842 CPL-2609
    IF v_saldo IS NOT NULL AND v_saldo > 0 THEN
       SELECT SUM(sdo_viv_92)
       INTO   c_saldo
       FROM   safre_tmp:det_tra_viv
       WHERE  nom_archivo = generar  #MLM-3842 CPL-2609

       LET dif_saldo = c_saldo - v_saldo

       IF dif_saldo <> 0 THEN
          IF bnd_proceso THEN
             DISPLAY "Program stopped. DIFERENCIAS EN EL SALDO DE LA SUBCUENTA VIV 92"
             UPDATE safre_tmp:det_tra_viv
             SET    estado_reg   = 2,
                    cve_cod_rech = "9D"
             WHERE  nom_archivo  = generar  #MLM-3842 CPL-2609
          ELSE
             PROMPT "EXISTEN DIFERENCIAS EN EL SALDO DE LA SUBCUENTA VIV 92. [Enter] PARA SALIR"
             FOR enter
             UPDATE safre_tmp:det_tra_viv
             SET    estado_reg   = 2,
                    cve_cod_rech = "9D"
             WHERE  nom_archivo  = generar  #MLM-3842 CPL-2609
          END IF

          EXIT PROGRAM
       END IF
    ELSE
       LET v_subcta = NULL
       LET v_saldo  = NULL
    END IF

    #######VALIDACION SALDO VIVIENDA ISSSTE 92 #######
    SELECT tot_sal_issste
    INTO   v_saldo
    FROM   safre_tmp:sum_tra_afo
    WHERE  nom_archivo = generar  #MLM-3842 CPL-2609
    IF v_saldo IS NOT NULL AND v_saldo > 0 THEN
       SELECT SUM(sdo_issste)
       INTO   c_saldo
       FROM   safre_tmp:det_tra_viv
       WHERE  nom_archivo = generar  #MLM-3842 CPL-2609

       LET dif_saldo = c_saldo - v_saldo

       IF dif_saldo <> 0 THEN
          IF bnd_proceso THEN
             DISPLAY "Program stopped. DIFERENCIAS EN EL SALDO DE LA SUBCUENTA VIV ISSSTE 92"

             UPDATE safre_tmp:det_tra_viv
             SET    estado_reg   = 2,
                    cve_cod_rech = "10D"
             WHERE  nom_archivo  = generar  #MLM-3842 CPL-2609
          ELSE
             PROMPT "HAY DIFERENCIAS EN SALDO DE LA SUBCTA VIV ISSSTE 92. [Enter] PARA SALIR" FOR enter  #cpl-2089

             UPDATE safre_tmp:det_tra_viv
             SET    estado_reg   = 2,
                    cve_cod_rech = "10D"
             WHERE  nom_archivo  = generar  #MLM-3842 CPL-2609
          END IF

          EXIT PROGRAM
       END IF
    ELSE
       LET v_subcta = NULL
       LET v_saldo  = NULL
    END IF

    #######VALIDACION SALDO VIVIENDA ISSSTE 08 #######
    SELECT tot_sal_issste_08
    INTO   v_saldo
    FROM   safre_tmp:sum_tra_afo
    WHERE  nom_archivo = generar  #MLM-3842 CPL-2609
    IF v_saldo IS NOT NULL AND v_saldo > 0 THEN
       SELECT SUM(sdo_issste_08)
       INTO   c_saldo
       FROM   safre_tmp:det_tra_viv
       WHERE  nom_archivo = generar  #MLM-3842 CPl-2609

       LET dif_saldo = c_saldo - v_saldo

       IF dif_saldo <> 0 THEN
          IF bnd_proceso THEN
             DISPLAY "Program stopped. DIFERENCIAS EN EL SALDO DE LA SUBCUENTA VIV ISSSTE 2008"
             UPDATE safre_tmp:det_tra_viv
             SET    estado_reg   = 2,
                    cve_cod_rech = "11D"
             WHERE  nom_archivo  = generar  #MLM-3842 CPL-2609
          ELSE
             PROMPT "HAY DIFERENCIAS EN EL SALDO DE LA SUBCTA VIV ISSSTE. [Enter] PARA SALIR"
             FOR enter
             UPDATE safre_tmp:det_tra_viv
             SET    estado_reg   = 2,
                    cve_cod_rech = "11D"
             WHERE  nom_archivo  = generar  #MLM-3842 CPL-2609
          END IF

          EXIT PROGRAM
       END IF
    ELSE
       LET v_subcta = NULL
       LET v_saldo  = NULL
    END IF

    #######IMPORTE BONO #######
    SELECT tot_impt_bono_issste
    INTO   v_saldo
    FROM   safre_tmp:sum_tra_afo
    WHERE  nom_archivo = generar  #MLM-3842 CPL-2609
    IF v_saldo IS NOT NULL AND v_saldo > 0 THEN
       SELECT SUM(importe_bono)
       INTO   c_saldo
       FROM   safre_tmp:det_tra_viv
       WHERE  nom_archivo = generar  #MLM-3842 CPL-2609

       LET dif_saldo = c_saldo - v_saldo

       IF dif_saldo <> 0 THEN
          IF bnd_proceso THEN
             DISPLAY "Program stopped. DIFERENCIAS EN EL IMPORTE DEL BONO"
             UPDATE safre_tmp:det_tra_viv
             SET    estado_reg   = 2,
                    cve_cod_rech = "12D"
             WHERE  nom_archivo  = generar  #MLM-3842 CPL-2609
          ELSE
             PROMPT "EXISTEN DIFERENCIAS EN EL IMPORTE DEL BONO. [Enter] PARA SALIR" FOR enter
             UPDATE safre_tmp:det_tra_viv
             SET    estado_reg   = 2,
                    cve_cod_rech = "12D"
             WHERE  nom_archivo  = generar  #MLM-3842 CPl-2609
          END IF

       EXIT PROGRAM
       END IF
    ELSE
       LET v_subcta = NULL
       LET v_saldo  = NULL
    END IF
  DATABASE safre_af
END FUNCTION

FUNCTION compara_saldo(sum_subcta, sum_saldo)
  DEFINE
    sum_subcta               CHAR(02),
    sum_saldo                DECIMAL(15,2),
    det_subcta               CHAR(02),
    det_saldo                DECIMAL(15,2),
    dif_saldo                DECIMAL(15,2)

  SELECT cve_subcta, SUM(import_sub)
  INTO   det_subcta, det_saldo
  FROM   valida_rcv
  WHERE  cve_subcta = sum_subcta
  GROUP BY 1

  LET dif_saldo = det_saldo - sum_saldo

  IF dif_saldo <> 0 THEN
     IF bnd_proceso THEN
        DISPLAY "Program stopped. DIFERENCIAS EN EL SALDO DE LA SUBCUENTA ", det_subcta

        UPDATE safre_tmp:det_tra_viv
        SET    estado_reg   = 2,
               cve_cod_rech = "13D"
        WHERE  nom_archivo  = generar  #MLM-3842 CPL-2609
     ELSE
        PROMPT "EXISTEN DIFERENCIAS EN EL SALDO DE LA SUBCUENTA ", det_subcta CLIPPED,"[Enter] PARA SALIR" FOR enter

        UPDATE safre_tmp:det_tra_viv
        SET    estado_reg   = 2,
               cve_cod_rech = "13D"
        WHERE  nom_archivo  = generar  #MLM-3842 CPL-2609
     END IF
 
     EXIT PROGRAM
  END IF
END FUNCTION

FUNCTION fechas(diaActual)
  DEFINE
    diaTmp                   DATE,
    contador                 SMALLINT,
    diaActual                DATE,
    numDias                  SMALLINT

  LET diaTmp = diaActual

  FOR contador = 1 TO 4
      IF contador = 1 THEN
         CALL habil_siguiente(diaTmp) RETURNING diaTmp
      ELSE
         LET diaTmp = diaTmp + 1 UNITS DAY
         CALL habil_siguiente(diaTmp) RETURNING diaTmp
      END IF
  END FOR

  RETURN diaTmp
END FUNCTION

FUNCTION habil_siguiente(diaActual)
  DEFINE
    diaTmp                   DATE,
    contador                 SMALLINT,
    diaActual                DATE

  DEFINE
    diaHabilSig              DATE,
    diaSemana                SMALLINT,
    feriado                  SMALLINT,
    finSemana                SMALLINT

  LET diaHabilSig = diaActual

  WHILE TRUE
    LET feriado   = 0
    LET finSemana = 0
    LET diaSemana = WEEKDAY(diaHabilSig)

    IF diaSemana = 0 OR diaSemana = 6 THEN
       LET finSemana = 1
    END IF

    SELECT *
    FROM   tab_feriado
    WHERE  feria_fecha = diaHabilSig
    IF STATUS <> NOTFOUND THEN
       LET feriado = 1
    END IF

    IF feriado = 1 OR finSemana = 1 THEN
       LET diaHabilSig = diaHabilSig + 1 UNITS DAY
    ELSE
       EXIT WHILE
    END IF
  END WHILE

  RETURN diaHabilSig
END FUNCTION

FUNCTION val_saldos_rep()
  DEFINE
    cve_subcta_d             CHAR(02),
    det_saldo_d              DECIMAL(15,2),
    cve_subcta_s             CHAR(02),
    tot_saldo_s              DECIMAL(15,2)

  DEFINE v_cadena            CHAR(5000)
  DEFINE v_indx              SMALLINT
  DEFINE v_cve_subcta        CHAR(02)
  DEFINE v_tot_subcta        DECIMAL(16,6)

  LET v_cadena     = ""
  LET v_cve_subcta = ""
  LET v_tot_subcta = 0

  FOR v_indx = 1 TO 28
      IF v_indx <= 9 THEN 
         LET v_cadena = "\n SELECT cve_subcta_", v_indx USING "#", " ,tot_subcta_", v_indx USING "#", 
                        "\n FROM   safre_tmp:sum_tra_afo ",
                        "\n WHERE  nom_archivo = '", generar, "'",                        
                        "\n AND    cve_subcta_", v_indx USING "#", " <> ''"

      ELSE
         LET v_cadena = "\n SELECT cve_subcta_", v_indx USING "##", " ,tot_subcta_", v_indx USING "##", 
                        "\n FROM   safre_tmp:sum_tra_afo ",
                        "\n WHERE  nom_archivo = '", generar, "'",
                        "\n AND    cve_subcta_", v_indx USING "##", " <> ''"

      END IF

      PREPARE str_vsr FROM v_cadena
      DECLARE cur_vsr CURSOR FOR str_vsr 
      FOREACH cur_vsr INTO v_cve_subcta, v_tot_subcta
        INSERT INTO saldos_val VALUES(v_cve_subcta, v_tot_subcta)
      END FOREACH

      CLOSE cur_vsr
      FREE cur_vsr
  END FOR

  CREATE INDEX saldos_val1 ON saldos_val(subcta)

  UPDATE STATISTICS FOR TABLE saldos_val

  LET v_hora    = TIME
  LET g_lista_s = g_param_taa.ruta_listados CLIPPED, "/", g_usuario CLIPPED,
                  ".saldo_rec_",generar[4,5], "_dif.", v_hoy using "DDMMYY",
                  "_", v_hora[1,2], v_hora[4,5] CLIPPED

  START REPORT listado_saldos TO g_lista_s
    DECLARE c_saldo CURSOR FOR
    SELECT  *
    FROM    safre_tmp:saldos_val
    ORDER BY 1
    FOREACH c_saldo INTO cve_subcta_s, tot_saldo_s
      SELECT cve_subcta, SUM(import_sub)
      INTO   cve_subcta_d, det_saldo_d
      FROM   safre_tmp:valida_rcv
      WHERE  cve_subcta = cve_subcta_s
      GROUP BY 1
  
      OUTPUT TO REPORT listado_saldos(cve_subcta_s, tot_saldo_s,
                                      cve_subcta_d, det_saldo_d)
    END FOREACH

    CLOSE c_saldo
    FREE c_saldo
  FINISH REPORT listado_saldos
END FUNCTION

REPORT listado_saldos(rcve_sub_s, rsaldo_s, rcve_sub_d, rsaldo_d)
  DEFINE
    rcve_sub_s               CHAR(02),
    rsaldo_s                 DECIMAL(15,2),
    rcve_sub_d               CHAR(02),
    rsaldo_d                 DECIMAL(15,2)

  DEFINE
    rtot_sal_viv97_s         DECIMAL(15,2),
    rsdo_viv_97_d            DECIMAL(15,2),
    rtot_sal_viv92_s         DECIMAL(15,2),
    rsdo_viv_92_d            DECIMAL(15,2),
    rtot_sal_issste_s        DECIMAL(15,2),
    rsdo_issste_d            DECIMAL(15,2),
    rtot_sal_issste_08_s     DECIMAL(15,2),
    rsdo_issste_08_d         DECIMAL(15,2),
    rtot_impt_bono_s         DECIMAL(15,2),
    rsdo_impt_bono_d         DECIMAL(15,2)

  OUTPUT
    PAGE LENGTH 60
    LEFT MARGIN 0
    RIGHT MARGIN 132
    TOP MARGIN 0
    BOTTOM MARGIN 0

  FORMAT
    PAGE HEADER
      PRINT COLUMN 04, "INFORMACION DE SALDOS TRASPASADOS A AFORE ", campo.des_titulo, v_hoy USING "dd/mm/yyyy"
      IF g_id_operacion = 9 THEN
         PRINT COLUMN 04, "                               TRASPASO NORMAL                               "
      ELSE
         PRINT COLUMN 04, "                           TRASPASO COMPLEMENTARIO                           "
      END IF

      PRINT COLUMN 04, "NOMBRE DEL ARCHIVO : ", generar, "    FECHA DE PRESENTACION : ", g_f_presentacion USING "dd/mm/yyyy"
      PRINT
      PRINT COLUMN 02, campo.cod_afore, "     ", campo.raz_social
      PRINT
      PRINT
      PRINT COLUMN 03, "RCV"
      PRINT COLUMN 03, "CVE ",
            COLUMN 11, "SALDO SUMARIO",
            COLUMN 32, "CVE",
            COLUMN 40, "SALDO DETALLE",
            COLUMN 59, "DIFERENCIA"

    ON EVERY ROW
       PRINT COLUMN 04, rcve_sub_s            USING "&&",
             COLUMN 11, rsaldo_s              USING "###,###,###,##&.&&",
             COLUMN 32, rcve_sub_d            USING "&&",
             COLUMN 40, rsaldo_d              USING "###,###,###,##&.&&",
             COLUMN 59, (rsaldo_s - rsaldo_d) USING "###,###,###,##&.&&"

       PRINT IF lineno > 57 THEN
                SKIP TO TOP OF PAGE
             END IF

    ON LAST ROW
       PRINT COLUMN 03, "TOT SUM: ",
             COLUMN 12, SUM(rsaldo_s) USING "###,###,###,##&.&&",
             COLUMN 34, "TOT DET: ",
             COLUMN 44, SUM(rsaldo_d) USING "###,###,###,##&.&&"
       PRINT COLUMN 03, "TOT DIFERENCIA: ",
             COLUMN 20, (SUM(rsaldo_s) - SUM(rsaldo_d)) USING "###,###,###,##&.&&"

       PRINT
       PRINT
       PRINT
       PRINT COLUMN 03, "VIVIENDA"
       PRINT COLUMN 03, "CVE ",
             COLUMN 11, "SALDO SUMARIO",
             COLUMN 32, "CVE",
             COLUMN 40, "SALDO DETALLE",
             COLUMN 59, "DIFERENCIA"

       SELECT tot_sal_viv97
       INTO   rtot_sal_viv97_s
       FROM   safre_tmp:sum_tra_afo
       WHERE  nom_archivo = generar  #MLM-3842 CPL-2609

       SELECT SUM(sdo_viv_97)
       INTO   rsdo_viv_97_d
       FROM   safre_tmp:det_tra_viv
       WHERE  nom_archivo = generar  #MLM-3842 CPL-2609

       SELECT tot_sal_viv92
       INTO   rtot_sal_viv92_s
       FROM   safre_tmp:sum_tra_afo
       WHERE  nom_archivo = generar  #MLM-3842 CPL-2609

       SELECT SUM(sdo_viv_92)
       INTO   rsdo_viv_92_d
       FROM   safre_tmp:det_tra_viv
       WHERE  nom_archivo = generar  #MLM-3842 CPL-2609

       SELECT tot_sal_issste
       INTO   rtot_sal_issste_s
       FROM   safre_tmp:sum_tra_afo
       WHERE  nom_archivo = generar  #MLM-3842 CPL-2609

       SELECT SUM(sdo_issste)
       INTO   rsdo_issste_d
       FROM   safre_tmp:det_tra_viv
       WHERE  nom_archivo = generar  #MLM-3842 CPL-2609

       SELECT tot_sal_issste_08
       INTO   rtot_sal_issste_08_s
       FROM   safre_tmp:sum_tra_afo
       WHERE  nom_archivo = generar  #MLM-3842 CPL-2609

       SELECT SUM(sdo_issste_08)
       INTO   rsdo_issste_08_d
       FROM   safre_tmp:det_tra_viv
       WHERE  nom_archivo = generar  #MLM-3842 CPL-2609

       SELECT tot_impt_bono_issste
       INTO   rtot_impt_bono_s
       FROM   safre_tmp:sum_tra_afo
       WHERE  nom_archivo = generar  #MLM-3842 CPL-2609

       SELECT SUM(importe_bono)
       INTO   rsdo_impt_bono_d
       FROM   safre_tmp:det_tra_viv
       WHERE  nom_archivo = generar  #MLM-3842 CPL-2609

       PRINT COLUMN 04, "04",
             COLUMN 11, rtot_sal_viv97_s                   USING "###,###,###,##&.&&",
             COLUMN 32, "04",
             COLUMN 40, rsdo_viv_97_d                      USING "###,###,###,##&.&&",
             COLUMN 59, (rtot_sal_viv97_s - rsdo_viv_97_d) USING "###,###,###,##&.&&"

       PRINT COLUMN 04, "09",
             COLUMN 11, rtot_sal_viv92_s                   USING "###,###,###,##&.&&",
             COLUMN 32, "09",
             COLUMN 40, rsdo_viv_92_d                      USING "###,###,###,##&.&&",
             COLUMN 59, (rtot_sal_viv92_s - rsdo_viv_92_d) USING "###,###,###,##&.&&"

       PRINT COLUMN 04, "16",
             COLUMN 11, rtot_sal_issste_s                   USING "###,###,###,##&.&&",
             COLUMN 32, "16",
             COLUMN 40, rsdo_issste_d                       USING "###,###,###,##&.&&",
             COLUMN 59, (rtot_sal_issste_s - rsdo_issste_d) USING "###,###,###,##&.&&"

       PRINT COLUMN 04, "26",
             COLUMN 11, rtot_sal_issste_08_s                      USING "###,###,###,##&.&&",
             COLUMN 32, "26",
             COLUMN 40, rsdo_issste_08_d                          USING "###,###,###,##&.&&",
             COLUMN 59, (rtot_sal_issste_08_s - rsdo_issste_08_d) USING "###,###,###,##&.&&"

       PRINT
       PRINT COLUMN 03, "TOT SUM: ",
             COLUMN 12, (rtot_sal_viv97_s  + rtot_sal_viv92_s +
                         rtot_sal_issste_s + rtot_sal_issste_08_s) USING "###,###,###,##&.&&",
             COLUMN 34, "TOT DET: ",
             COLUMN 44, (rsdo_viv_97_d + rsdo_viv_92_d +
                         rsdo_issste_d + rsdo_issste_08_d) USING "###,###,###,##&.&&"

       PRINT COLUMN 03, "TOT DIFERENCIA: ",
             COLUMN 20, ((rtot_sal_viv97_s  + rtot_sal_viv92_s +
                          rtot_sal_issste_s + rtot_sal_issste_08_s) -
                         (rsdo_viv_97_d     + rsdo_viv_92_d +
                          rsdo_issste_d     + rsdo_issste_08_d)) USING "###,###,###,##&.&&"

       PRINT
       PRINT
       PRINT
       PRINT COLUMN 03, "BONO ISSSTE"
       PRINT COLUMN 03, "CVE ",
             COLUMN 11, "SALDO SUMARIO",
             COLUMN 32, "CVE",
             COLUMN 40, "SALDO DETALLE",
             COLUMN 59, "DIFERENCIA"

       PRINT COLUMN 04, "27",
             COLUMN 11, rtot_impt_bono_s                      USING "###,###,###,##&.&&",
             COLUMN 32, "27",
             COLUMN 40, rsdo_impt_bono_d                      USING "###,###,###,##&.&&",
             COLUMN 59, (rtot_impt_bono_s - rsdo_impt_bono_d) USING "###,###,###,##&.&&"

       PRINT
       PRINT COLUMN 03, "TOT SUM: ",
             COLUMN 12, rtot_impt_bono_s                    USING "###,###,###,##&.&&",
             COLUMN 34, "TOT DET: ",
             COLUMN 44, rsdo_impt_bono_d                    USING "###,###,###,##&.&&"

       PRINT COLUMN 03, "TOT DIFERENCIA: ",
             COLUMN 20, (rtot_impt_bono_s) - (rsdo_impt_bono_d) USING "###,###,###,##&.&&"
END REPORT

FUNCTION genera_rep_tipo_tra()
  INITIALIZE datos_tt.* TO NULL

  LET v_hora = TIME

  SELECT *
  INTO   g_afore.*
  FROM   tab_afore_local

  LET g_lista = g_param_taa.ruta_listados CLIPPED, "/", g_usuario CLIPPED, ".det_tipo_tra_", g_id_operacion USING "&&", ".", v_hoy USING "DDMMYY", "_", v_hora[1,2], v_hora[4,5] CLIPPED

  START REPORT listado_tip_tra TO g_lista
    DECLARE cur21 CURSOR FOR
    SELECT a.tipo_traspaso, b.descripcion, COUNT(*)
    FROM   safre_tmp:det_tra_viv a, tab_tipo_traspaso b  --REVISAR
    WHERE  a.tipo_registro = "02"
    AND    a.tipo_traspaso = b.tipo_traspaso
    AND    b.id_opera      = '09'
    AND    a.nom_archivo   = generar  #MLM-3842 CPL-2609
    GROUP BY 1,2
    ORDER BY 1,2
    IF STATUS = NOTFOUND THEN
       IF bnd_proceso THEN
          DISPLAY "Program stopped, NO SE IMPRIME REPORTE"
       ELSE
          ERROR "NO SE IMPRIME REPORTE"
          SLEEP 2
       END IF
    ELSE
       FOREACH cur21 INTO datos_tt.*
         LET tot_total_tt = tot_total_tt + datos_tt.tot_parcial

         OUTPUT TO REPORT listado_tip_tra(datos_tt.*)
       END FOREACH

       CLOSE cur21
       FREE cur21
    END IF
  FINISH REPORT listado_tip_tra

  LET g_lista = g_param_taa.ruta_listados CLIPPED, "/", g_usuario CLIPPED, ".det_tipo_tra_", g_id_operacion USING "&&", ".", v_hoy using "DDMMYY", "_", v_hora[1,2], v_hora[4,5] CLIPPED

  IF s_codigo_afore <> 564 THEN --MLM
     LET COMANDO = "lp ",g_lista #MLM-1642
     RUN COMANDO #MLM-1642
  END IF

  IF bnd_proceso THEN
     DISPLAY "REPORTE GENERADO"
  ELSE
     ERROR "TERMINO PROCESO DE INFORMACION, CARGA RECHAZOS"
     SLEEP 2
  END IF
END FUNCTION

REPORT listado_tip_tra(datos_tt)
  DEFINE datos_tt            RECORD
    tipo_trasp               CHAR(02),
    des_tt                   CHAR(25),
    tot_parcial              INTEGER
  END RECORD

  OUTPUT
    PAGE LENGTH 60
    LEFT MARGIN 0
    RIGHT MARGIN 132
    TOP MARGIN 0
    BOTTOM MARGIN 0

  FORMAT
    PAGE HEADER
      PRINT COLUMN 04, '\033e\033(s218T\033(s11H\033(s7B',"INFORMACION DE AFILIADOS TRASPASADOS A AFORE ", campo.des_titulo, v_hoy USING "dd/mm/yyyy"
        IF id_op = 9 THEN
           PRINT COLUMN 04, '\033e\033(s218T\033(s11H\033(s7B',"                               TRASPASO NORMAL                               "
        ELSE
           PRINT COLUMN 04, '\033e\033(s218T\033(s11H\033(s7B',"                           TRASPASO COMPLEMENTARIO                           "
        END IF

      PRINT COLUMN 04, '\033e\033(s218T\033(s11H\033(s7B',"NOMBRE DEL ARCHIVO : ", generar, "    FECHA DE PRESENTACION : ", g_f_presentacion USING "dd/mm/yyyy"
      PRINT
      PRINT COLUMN 01, '\033e\033(s218T\033(s11H\033(s7N'
      PRINT COLUMN 02, campo.cod_afore, "     ", campo.raz_social
      PRINT
      PRINT COLUMN 01, "\332\304\304\304\304\304\304\304\302\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\302\304\304\304\304\304\304\304\304\302\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\277"
      PRINT COLUMN 01, "\263",COLUMN 3,"ORIGEN",COLUMN 8,"\263",COLUMN 20,"Detalle",COLUMN 43,"\263",COLUMN 45,"",COLUMN 52,"\263",COLUMN 54,"REGISTROS",COLUMN 76,"\263"
      PRINT COLUMN 01, "\300\304\304\304\304\304\304\304\301\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\301\304\304\304\304\304\304\304\304\301\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\331"

    ON EVERY ROW
       PRINT PRINT
       COLUMN 04, datos_tt.tipo_trasp USING "&&",
       COLUMN 11, datos_tt.des_tt,
       COLUMN 60, datos_tt.tot_parcial USING "&&&&&&"

       PRINT IF lineno > 57 THEN
                SKIP TO TOP OF PAGE
             END IF

    ON LAST ROW
       PRINT COLUMN 01, "\332\304\304\304\304\304\304\304\302\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\302\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\277"
       PRINT COLUMN 01, "\263",COLUMN 3,"TOTAL ","\263 ",COLUMN 52,"\263",COLUMN 60, tot_total_tt USING "&&&&&&",COLUMN 76,"\263"
       PRINT COLUMN 01, "\300\304\304\304\304\304\304\304\301\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\301\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\331"
END REPORT

##############################################################################
FUNCTION genera_rep_sief_subc()        ##ACS Mayo2011
  DEFINE r_sub RECORD
    siefore_desc             LIKE safre_af:tab_siefore.siefore_desc,
    siefore_cod              LIKE safre_af:tab_siefore.siefore_cod,
    subct_cod                LIKE safre_af:tab_subcuenta.subct_cod,
    subct_desc               LIKE safre_af:tab_subcuenta.subct_desc,
    monto                    DECIMAL(15,2)
  END RECORD

  DEFINE cadena              CHAR(2200)
  DEFINE x                   SMALLINT

  INITIALIZE r_sub.*, cadena TO NULL

  LET v_hora  = TIME
  LET g_lista = g_param_taa.ruta_listados CLIPPED, "/", g_usuario CLIPPED,
                ".REP_SIEF_SUBC.", v_hoy using "DDMMYY", v_hora[1,2], v_hora[4,5] CLIPPED

  DATABASE safre_tmp
    START REPORT report_sief_subc TO g_lista
      FOR x = 1 TO 8 ---##For de 8 por los campos de la tabla
          LET cadena = "SELECT siefore_", x USING "#", ",cve_subcta_", x USING "#", ",",
                       "       SUM(import_sub_", x USING "#", ") ",
                       " FROM  safre_tmp:det_tra_rcv",
                       " WHERE   nom_archivo                   = '", generar,"'", #MLM-3842 CPL_2609
                       " AND cve_subcta_", x USING "#", " != ''",
                       " GROUP BY 1,2",
                       " ORDER BY 1"

          --display "cadena "
          --display cadena sleep 3
          PREPARE str1 FROM cadena
          DECLARE cur_subc  CURSOR FOR str1
          FOREACH cur_subc INTO r_sub.siefore_desc, r_sub.subct_cod,r_sub.monto
            SELECT a.siefore_cod 
            INTO   r_sub.siefore_cod
            FROM   safre_af:tab_siefore a
            WHERE  a.siefore_desc = r_sub.siefore_desc
            AND    a.siefore_cod  = r_sub.subct_cod -- CPL-2921. cambio para traer solo un registro

            SELECT TRIM(b.subct_desc) 
            INTO   r_sub.subct_desc
            FROM   safre_af:tab_subcuenta b
            WHERE  b.subct_cod = r_sub.subct_cod

            OUTPUT TO REPORT report_sief_subc(r_sub.*, x)
          END FOREACH

          CLOSE cur_subc
          FREE cur_subc
      END FOR
    FINISH REPORT report_sief_subc
  DATABASE safre_af

  IF s_codigo_afore <> 564 THEN --MLM
     LET COMANDO = "lp ",g_lista #MLM-1642
     RUN COMANDO #MLM-1642
  END IF

  IF bnd_proceso THEN
     DISPLAY "REPORTE GENERADO"
  ELSE
     ERROR "TERMINA PROCESO DE INFORMACION,VERIFICA RECHAZOS"
     SLEEP 2
  END IF
END FUNCTION

##############################################################################
REPORT report_sief_subc(r_subc, i)        ##ACS Mayo2011
  DEFINE r_subc              RECORD
    siefore_desc             LIKE safre_af:tab_siefore.siefore_desc,
    siefore_cod              LIKE safre_af:tab_siefore.siefore_cod,
    subct_cod                LIKE safre_af:tab_subcuenta.subct_cod,
    subct_desc               CHAR(40),
    monto                    DECIMAL(15,2)
  END RECORD

  DEFINE i                   SMALLINT

  OUTPUT
    TOP MARGIN 1
    BOTTOM MARGIN 0
    LEFT MARGIN 0
    RIGHT MARGIN 0
    PAGE LENGTH 60

  ORDER BY r_subc.siefore_cod, r_subc.subct_cod

  FORMAT
    PAGE HEADER
      PRINT COLUMN 03, campo.cod_afore,
            COLUMN 11, campo.raz_social,
            COLUMN 68, TODAY USING "DD-MM-YYYY"
      SKIP 1 LINE
      PRINT COLUMN 10, "REPORTE DE SALDOS POR SIEFORE/SUBCUENTA"
      SKIP 1 LINE
      PRINT COLUMN 01, "-------------------------------------------------------------------------------------"
      PRINT COLUMN 03, "SIEFORE",
            COLUMN 18, "SUBCUENTA",
            COLUMN 65, "MONTO EN PESOS"
      PRINT COLUMN 01, "-------------------------------------------------------------------------------------"
      SKIP 1 LINE

    ON EVERY ROW
       PRINT COLUMN 03, r_subc.siefore_cod USING "<<<",
             COLUMN 07, r_subc.siefore_desc CLIPPED,
             COLUMN 18, r_subc.subct_cod using "<<<",
             COLUMN 22, r_subc.subct_desc CLIPPED,
             COLUMN 65, r_subc.monto USING "###,###,###,##&.&&"

    AFTER GROUP OF r_subc.siefore_cod
      SKIP 1 LINE
      PRINT COLUMN 03, "TOTAL SIEFORE ", r_subc.siefore_cod USING "<<<",
            COLUMN 65, GROUP SUM(r_subc.monto) USING "###,###,###,##&.&&"
      SKIP 1 LINE

    PAGE TRAILER
      SKIP 2 LINE
      PRINT COLUMN 60," Pagina : ", PAGENO USING "<<<<<"
END REPORT

##############################################################################
FUNCTION genera_rep_siefore()    ##ACS Mayo2011
  DEFINE r_sub               RECORD
    siefore_desc             LIKE safre_af:tab_siefore.siefore_desc,
    siefore_cod              LIKE safre_af:tab_siefore.siefore_cod,
    acciones                 DECIMAL(22,6),
    monto                    DECIMAL(15,2),
    cuenta                   INTEGER        #JCPV
  END RECORD

  DEFINE cadena              CHAR(2000)
  DEFINE v_sql               CHAR(1000)
  DEFINE x                   SMALLINT
  DEFINE siefore             SMALLINT
  DEFINE afore               SMALLINT

  INITIALIZE r_sub.*, cadena TO NULL
  LET siefore = 0
  LET afore   = 0

  SELECT a.codigo_afore
  INTO   afore
  FROM   safre_af:tab_afore_local a

  LET v_hora  = TIME
  LET g_lista = g_param_taa.ruta_listados CLIPPED, "/", g_usuario CLIPPED, ".REP_SAL_SIEF.", v_hoy using "DDMMYY", v_hora[1,2], v_hora[4,5] CLIPPED

  DATABASE safre_tmp
    CREATE TEMP TABLE tmp_siefore (siefore_desc CHAR(08),
                                   siefore_cod  SMALLINT,
                                   acciones     DECIMAL(22,6),
                                   monto        DECIMAL(15,2),
                                   cuenta       INTEGER)        #JCPV
   
    FOR x = 1 TO 8 ---##For de 8 por los campos de la tabla        
        LET cadena = "SELECT siefore_", x USING "#", ", ",
                     "       SUM(import_sub_", x USING "#","), ",
                     "       SUM(no_tot_acc_", x USING "#","), ",
                     "       COUNT(*) ",
                     " FROM  safre_tmp:det_tra_rcv",
                     " WHERE nom_archivo = '", generar, "'", #MLM-3842 CPL-2609
                     " AND cve_subcta_", x USING "#", " != ''",
                     " GROUP BY 1",
                     " ORDER BY 1"

        --display "cadena "
        --display cadena sleep 3
        PREPARE str2 FROM cadena
        DECLARE cur_sub1 CURSOR FOR str2
        FOREACH cur_sub1 INTO r_sub.siefore_desc, r_sub.monto, r_sub.acciones, r_sub.cuenta
          SELECT a.siefore_cod
          INTO   r_sub.siefore_cod
          FROM   safre_af:tab_siefore a
          WHERE  a.siefore_desc = r_sub.siefore_desc

          INSERT INTO tmp_siefore VALUES(r_sub.*)
        END FOREACH

        CLOSE cur_sub1
        FREE cur_sub1
    END FOR-- para cada una de las siefores

    INITIALIZE r_sub.* TO NULL

    START REPORT report_siefore TO g_lista
      DECLARE cur_sub2 CURSOR FOR
      SELECT UNIQUE(b.siefore_cod)
      FROM   safre_af:tab_siefore b
      WHERE  b.afore_cod = afore
      ORDER BY 1
      FOREACH cur_sub2 INTO siefore
        DECLARE cur_sub3 CURSOR FOR
        SELECT  c.siefore_desc, c.siefore_cod, SUM(c.acciones), SUM(c.monto), SUM(c.cuenta)
        FROM    tmp_siefore c
        WHERE   c.siefore_cod = siefore
        GROUP BY 1,2
        FOREACH cur_sub3 INTO r_sub.*
          OUTPUT TO REPORT report_siefore(r_sub.*)
        END FOREACH

        CLOSE cur_sub3
        FREE cur_sub3
      END FOREACH

      CLOSE cur_sub2
      FREE cur_sub2
    FINISH REPORT report_siefore
  DATABASE safre_af

  IF s_codigo_afore = 562 THEN
     LET COMANDO = "lp ",g_lista #MLM-1642
     RUN COMANDO #MLM-1642
  END IF

  IF bnd_proceso THEN
      DISPLAY "REPORTE GENERADO"
  ELSE
      ERROR "TERMINA PROCESO DE INFORMACION, VERIFICA RECHAZOS"
      SLEEP 2
  END IF
END FUNCTION

##############################################################################
REPORT report_siefore(r_sub1)    ##ACS Mayo2011
  DEFINE r_sub1              RECORD
    siefore_desc             LIKE safre_af:tab_siefore.siefore_desc,
    siefore_cod              LIKE safre_af:tab_siefore.siefore_cod,
    acciones                 DECIMAL(22,6),
    monto                    DECIMAL(15,2),
    cuenta                   INTEGER        #JCPV
  END RECORD

  DEFINE siefore             LIKE safre_af:tab_siefore.siefore_desc
  DEFINE titulo              CHAR(100)

  OUTPUT
    TOP MARGIN 1
    BOTTOM MARGIN 0
    LEFT MARGIN 0
    RIGHT MARGIN 0
    PAGE LENGTH 60

  ORDER BY r_sub1.siefore_cod

  FORMAT
    PAGE HEADER
      PRINT COLUMN 03, campo.cod_afore,
            COLUMN 11, campo.raz_social,
            COLUMN 68, TODAY USING "DD-MM-YYYY"
      SKIP 1 LINE
      PRINT COLUMN 10, "REPORTE DE SALDOS TOTALES POR SIEFORE"
      SKIP 1 LINE

      PRINT COLUMN 01, "-------------------------------------------------------------------------------------"
      PRINT COLUMN 03, "SIEFORE",
            COLUMN 25, "ACCIONES",
            COLUMN 50, "MONTO EN PESOS",
            COLUMN 70,  "# DE REGISTROS"
      PRINT COLUMN 01, "-------------------------------------------------------------------------------------"
      SKIP 1 LINE

    ON EVERY ROW
       PRINT COLUMN 03, r_sub1.siefore_cod USING "<<<",
             COLUMN 07, r_sub1.siefore_desc CLIPPED,
             COLUMN 18, r_sub1.acciones  USING "###,###,###,###,##&.&&&&&&",
             COLUMN 45, r_sub1.monto USING "###,###,###,##&.&&",
             COLUMN 70, r_sub1.cuenta USING "####&"

    ON LAST ROW
       SKIP 1 LINE
       PRINT COLUMN 02, "TOTALES",
             COLUMN 45, SUM(r_sub1.monto) USING "###,###,###,##&.&&",
             COLUMN 70, SUM(r_sub1.cuenta) USING "####&"
       SKIP 1 LINE

    PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ", PAGENO USING "<<<<<"
END REPORT

##############################################################################
FUNCTION genera_rep_subcuenta()    ##ACS Mayo2011
  DEFINE r_subcuenta         RECORD
    cve_sub                  SMALLINT,
    concepto                 CHAR(15),
    monto                    DECIMAL(18,4),
    acciones                 DECIMAL(22,6),
    cuenta                   INTEGER        #JCPV
  END RECORD

  LET v_hora  = TIME
  LET g_lista = g_param_taa.ruta_listados CLIPPED, "/", g_usuario CLIPPED, ".REP_SAL_SUBC.", v_hoy using "DDMMYY", v_hora[1,2], v_hora[4,5] CLIPPED

  DATABASE safre_tmp
    INITIALIZE r_subcuenta.* TO NULL

    START report report_subcuenta TO g_lista
      SELECT 1, "VIVIENDA 92", SUM(a.sdo_viv_92), SUM(a.partic_viv92), COUNT(*)
      INTO   r_subcuenta.*
      FROM   det_tra_viv a
      WHERE  a.nom_archivo = generar  #MLM-3842 CPL-2609      
      AND    a.sdo_viv_92 != 0

      OUTPUT TO REPORT report_subcuenta(r_subcuenta.*)

      SELECT 2, "VIVIENDA 97", SUM(b.sdo_viv_97), SUM(b.partic_viv97), COUNT(*)
      INTO   r_subcuenta.*
      FROM   det_tra_viv b
      WHERE  b.nom_archivo = generar #MLM-3842 CPL-2609
      AND    b.sdo_viv_97 != 0

      OUTPUT TO REPORT report_subcuenta(r_subcuenta.*)

      SELECT 3, "FOVISSSTE 92", SUM(c.sdo_issste), SUM(aivs_issste), COUNT(*)
      INTO   r_subcuenta.*
      FROM   det_tra_viv c
      WHERE  c.nom_archivo = generar #MLM-3842 CPL-2609      
      AND    c.sdo_issste != 0


      OUTPUT TO REPORT report_subcuenta(r_subcuenta.*)

      SELECT 4, "FOVISSSTE 08", SUM(d.sdo_issste_08), SUM(aivs_issste_08), COUNT(*)
      INTO   r_subcuenta.*
      FROM   det_tra_viv d
      WHERE  d.nom_archivo    = generar #MLM-3842 CPL-2609      
      AND    d.sdo_issste_08 != 0

      OUTPUT TO REPORT report_subcuenta(r_subcuenta.*)

      SELECT 5, "BONOS ISSSTE", SUM(e.importe_bono), 0, COUNT(*)
      INTO   r_subcuenta.*
      FROM   det_tra_viv e
      WHERE  e.nom_archivo   = generar #MLM-3842 CPL-2609
      AND    e.importe_bono != 0


      OUTPUT TO REPORT report_subcuenta(r_subcuenta.*)
    FINISH REPORT report_subcuenta
  DATABASE safre_af

  IF s_codigo_afore = 562 THEN
     LET COMANDO = "lp ",g_lista #MLM-1642
     RUN COMANDO #MLM-1642
  END IF

  IF bnd_proceso THEN
     DISPLAY "REPORTE GENERADO"
  ELSE
     ERROR "TERMINA PROCESO DE INFORMACION, VERIFICA RECHAZOS"
     SLEEP 2
  END IF
END FUNCTION

REPORT report_subcuenta(r_sub4)    ##ACS Mayo2011
  DEFINE r_sub4              RECORD
    cve_sub                  SMALLINT,
    concepto                 CHAR(15),
    monto                    DECIMAL(18,4),
    acciones                 DECIMAL(22,6),
    cuenta                   INTEGER        #JCPV
  END RECORD

  OUTPUT
    TOP MARGIN 1
    BOTTOM MARGIN 0
    LEFT MARGIN 0
    RIGHT MARGIN 0
    PAGE LENGTH 60

  ORDER BY r_sub4.cve_sub

  FORMAT
    PAGE HEADER
      PRINT COLUMN 03, campo.cod_afore,
            COLUMN 11, campo.raz_social,
            COLUMN 68, TODAY USING "DD-MM-YYYY"
      SKIP 1 LINE
      PRINT COLUMN 10, "REPORTE DE SALDOS TOTALES POR SUBCUENTA"
      SKIP 1 LINE

      PRINT COLUMN 01, "-------------------------------------------------------------------------------------"
      PRINT COLUMN 03, "SUBCUENTAS",
            COLUMN 30, "ACCIONES",
            COLUMN 56, "MONTO EN PESOS",
            COLUMN 73, "# DE REGISTROS"
      PRINT COLUMN 01, "-------------------------------------------------------------------------------------"
      SKIP 1 LINE

    ON EVERY ROW
       PRINT COLUMN 03, r_sub4.concepto CLIPPED,
             COLUMN 17, r_sub4.acciones  USING "###,###,###,###,##&.&&&&&&",
             COLUMN 48, r_sub4.monto USING "###,###,###,###,##&.&&&&",
             COLUMN 75, r_sub4.cuenta USING "####&"

    ON LAST ROW
       SKIP 1 LINE
       PRINT COLUMN 02, "TOTALES",
             COLUMN 53, SUM(r_sub4.monto) USING "###,###,###,##&.&&&&",
             COLUMN 75, SUM(r_sub4.cuenta) USING "####&"
       SKIP 1 LINE

    PAGE TRAILER
      SKIP 2 LINE
      PRINT COLUMN 60," Pagina : ", PAGENO USING "<<<<<"
END REPORT

FUNCTION fn_verifica_nss(l_tt, l_curp, l_val)
  DEFINE l_tt                SMALLINT
  DEFINE l_ts                VARCHAR(15)
  DEFINE l_st_int            SMALLINT
  DEFINE l_val               SMALLINT

  DEFINE l_nss               CHAR(11)
  DEFINE l_curp              CHAR(18)
  DEFINE l_st_2              CHAR(70)
  DEFINE l_sql               CHAR(500)

  DEFINE l_fentcons          DATE

  LET l_st_int = 100

  IF l_val = 1 THEN
     IF l_tt = 51 OR l_tt = 53 THEN
        LET l_ts = 1
     ELSE
        LET l_ts = 5
     END IF

     LET l_sql = "\n SELECT FIRST 1 a.nss, a.f_certificacion ",
                 "\n FROM   safre_tmp:tmp_afi_sol_tar a ",
                 "\n WHERE  a.nss      NOT IN (SELECT mar.nss FROM safre_tmp:tmp_cuentas_inactivas mar ",
                 "\n                          WHERE mar.nss = a.nss)  ",
                 "\n AND    a.curp          = '", l_curp, "'",
                 "\n AND    a.tpo_solicitud = ", l_ts, 
                 "\n AND    a.edo_interno  >= ", l_st_int,
                 "\n ORDER BY a.f_certificacion DESC"
  END IF

  IF l_val = 2 THEN
     IF l_tt = 72 THEN
        LET l_ts   = 16
        LET l_st_2 = " AND (a.edo_interno IN (70,50) OR a.edo_interno >= 100)"
     ELSE
        IF l_tt = 74 THEN
           LET l_ts   = 18
           LET l_st_2 = " AND (a.edo_interno IN (70,50) OR a.edo_interno >= 100)"
        ELSE
           IF l_tt = 78 THEN
               LET l_ts   = 35
               LET l_st_2 = " AND (a.edo_interno IN (70,50) OR a.edo_interno >= 100)"
           ELSE
               IF (l_tt = 75 OR l_tt = 76) THEN 
                  LET l_ts   = "28,29,42,43"   ##Traspaso APP por CURP
                  LET l_st_2 = " AND (a.edo_interno IN (70,50) OR a.edo_interno >= 100)"
               ELSE
                  LET l_ts   = 15
                  LET l_st_2 = " AND a.edo_interno >= 70"
               END IF
          END IF 
        END IF
     END IF

     LET l_st_2 = l_st_2 CLIPPED

     LET l_sql  = "\n SELECT FIRST 1 a.nss, a.f_certificacion ",
                  "\n FROM   safre_tmp:tmp_afi_sol_tar a ",
                  "\n WHERE  a.curp          = '",l_curp,"'",
                  "\n AND    a.tpo_solicitud in (",l_ts,")",
                  "\n", l_st_2,
                  "\n ORDER BY a.f_certificacion DESC"
  END IF

  LET l_sql = l_sql CLIPPED
  --display l_sql

  PREPARE prp_v_nss1 FROM l_sql
  EXECUTE prp_v_nss1 INTO l_nss, l_fentcons

  RETURN l_nss
END FUNCTION

FUNCTION fn_salida(v_layout, v_tipo_reg, v_nom_arch, v_tabla)
#s--------------------------------------------
  DEFINE v_layout            SMALLINT
  DEFINE v_tabla             CHAR(30)
  DEFINE v_tipo_reg          SMALLINT
  DEFINE nom_archivo_dat     CHAR(60)
  DEFINE nom_archivo_cmd     CHAR(60)
  DEFINE v_nom_arch          CHAR(60)

  LET nom_archivo_cmd = g_param_taa.ruta_rescate CLIPPED, "/",
                        p_tablayout.nom_arch
  LET nom_archivo_dat = g_param_taa.ruta_rescate CLIPPED, "/",
                        v_nom_arch

  START REPORT r_report_db TO nom_archivo_cmd
    OUTPUT TO REPORT r_report_db(nom_archivo_dat,
                                 v_layout,
                                 v_tipo_reg, #cza, det, sum
                                 v_tabla)    #nombre de la tabla a insertar
  FINISH REPORT r_report_db
END FUNCTION

REPORT r_report_db(v_arch, v_codigo, v_reg, v_tabla)
  DEFINE v_arch              CHAR(110) 
  DEFINE v_tabla             CHAR(110)  
  DEFINE v_campo             CHAR(110)
  DEFINE v_reg               SMALLINT
  DEFINE v_codigo            SMALLINT
  DEFINE v_ban               SMALLINT
  DEFINE i                   SMALLINT
  DEFINE j                   SMALLINT
  DEFINE p_tabcampo          RECORD LIKE safre_af:tab_campo.*

  OUTPUT
    PAGE   LENGTH 1
    LEFT   MARGIN 0
    RIGHT  MARGIN 0
    TOP    MARGIN 0
    BOTTOM MARGIN 0

  FORMAT
    ON EVERY ROW
       PRINT COLUMN 1, "FILE ", "\"", v_arch CLIPPED , "\" ("
                       DECLARE cur_db CURSOR  FOR
                       SELECT  a.*  
                       FROM    safre_af:tab_campo a
                       WHERE   a.layout_cod = v_codigo
                       AND     a.tipo_reg   = v_reg
                       ORDER BY 3

                       LET v_ban = false

                       FOREACH cur_db INTO p_tabcampo.*
                         IF v_ban THEN
                            PRINT COLUMN 32, ","
                         END IF

                         PRINT COLUMN  1, p_tabcampo.campo_desc CLIPPED;
                         PRINT COLUMN 20, p_tabcampo.pos_ini CLIPPED,
                                         "-" CLIPPED,
                                         p_tabcampo.pos_fin USING "&&&&" CLIPPED;
                         LET v_ban = TRUE
                       END FOREACH
       PRINT COLUMN 32 , ");";
       PRINT
       PRINT
       PRINT COLUMN 1,"INSERT INTO  ", v_tabla CLIPPED, ";"
END REPORT

FUNCTION fn_borra_crea_tablas()

  DEFINE v_cadena            CHAR(1000)

  LET v_cadena = ''

  DATABASE safre_tmp

  WHENEVER ERROR CONTINUE;  --4   CPL-3956  SE SEPARA POR TABLA 
      LET v_cadena = " DROP TABLE taa_cta_rec_cza; "
      PREPARE stm_dtie1 FROM v_cadena
      EXECUTE stm_dtie1;
  WHENEVER ERROR STOP; --4 CPL-3956  SE SEPARA POR TABLA 

  WHENEVER ERROR CONTINUE;  --5 CPL-3956  SE SEPARA POR TABLA 
      LET v_cadena = " DROP TABLE taa_cta_rec_det2; "
      PREPARE stm_dtie2 FROM v_cadena
      EXECUTE stm_dtie2;
  WHENEVER ERROR STOP; --5  CPL-3956  SE SEPARA POR TABLA  

WHENEVER ERROR CONTINUE;  --6 CPL-3956  SE SEPARA POR TABLA 
      LET v_cadena = " DROP TABLE taa_cta_rec_det5; "
      PREPARE stm_dtie3 FROM v_cadena
      EXECUTE stm_dtie3;
  WHENEVER ERROR STOP; --6 CPL-3956  SE SEPARA POR TABLA 
  
  WHENEVER ERROR CONTINUE;  --7 CPL-3956  SE SEPARA POR TABLA 
      LET v_cadena = " DROP TABLE taa_cta_rec_sum; "
      PREPARE stm_dtie4 FROM v_cadena
      EXECUTE stm_dtie4;
  WHENEVER ERROR STOP; --7  CPL-3956  SE SEPARA POR TABLA 
  
  WHENEVER ERROR CONTINUE;  --8  CPL-3956  SE SEPARA POR TABLA 
      LET v_cadena = " DROP TABLE tmp_afi_sol_tar; "
      PREPARE stm_dtie5 FROM v_cadena
      EXECUTE stm_dtie5;
  WHENEVER ERROR STOP; --8  CPL-3956  SE SEPARA POR TABLA 
  
  WHENEVER ERROR CONTINUE;  --9  CPL-3956  SE SEPARA POR TABLA 
      LET v_cadena = " DROP TABLE tmp_afi_sol_val; "
      PREPARE stm_dtie6 FROM v_cadena
      EXECUTE stm_dtie6;
  WHENEVER ERROR STOP; --9  CPL-3956  SE SEPARA POR TABLA 
  
  WHENEVER ERROR CONTINUE;  --10  CPL-3956  SE SEPARA POR TABLA 
      LET v_cadena = " DROP TABLE tmp_afi_mae_val; "
      PREPARE stm_dtie7 FROM v_cadena
      EXECUTE stm_dtie7;
  WHENEVER ERROR STOP; --10  CPL-3956  SE SEPARA POR TABLA 

  CREATE TABLE taa_cta_rec_cza (tpo_registro      CHAR(02),
                                id_servicio       CHAR(02),
                                id_operacion      CHAR(02),
                                tpo_ent_origen    CHAR(02),
                                cve_ent_origen    CHAR(03),
                                tpo_ent_destino   CHAR(02),
                                cve_ent_destino   CHAR(03),
                                ef_envio_lote     CHAR(03),
                                f_presentacion    CHAR(08),
                                consecutivo_lote  SMALLINT,
                                cve_mod_rec       CHAR(02),
                                cod_res_operacion CHAR(02),
                                motivo_rechazo    CHAR(09))

  CREATE TABLE taa_cta_rec_det2 (tpo_registro          CHAR(02),
                                 cont_servicio         DECIMAL(10,0),
                                 tpo_ent_rec_cta       CHAR(02),
                                 cve_ent_rec_cta       CHAR(03),
                                 tpo_ent_transf_cta    CHAR(02),
                                 cve_ent_transf_cta    CHAR(03),
                                 tpo_traspaso          CHAR(02),
                                 f_presentacion        CHAR(08),
                                 curp                  CHAR(18),
                                 curp_unificadora      CHAR(18),
                                 nss                   CHAR(11),
                                 nss_unificador        CHAR(11),
                                 ap_paterno            CHAR(40),
                                 ap_materno            CHAR(40),
                                 nombre                CHAR(40),
                                 f_recepcion_sol       CHAR(08),
                                 apl_int_issste_08     DECIMAL(15,0),
                                 sdo_fnd_issste_08     DECIMAL(15,0),
                                 apl_int_vivenda_97    DECIMAL(15,0),
                                 sdo_vivienda_97       DECIMAL(15,0),
                                 aplic_int_issste_92   DECIMAL(15,0),
                                 sdo_issste_92         DECIMAL(15,0),
                                 aplic_int_vivienda_92 DECIMAL(15,0),
                                 sdo_vivienda_92       DECIMAL(15,0),
                                 id_cred_issste        CHAR(01),
                                 id_cred_vivienda      CHAR(01),
                                 cod_res_operacion     CHAR(02),
                                 diag_proceso          CHAR(15),
                                 id_tpo_reg_pension    CHAR(02),
                                 f_redencion_bono      CHAR(08),
                                 importe_bono          DECIMAL(16,0),
                                 apl_int_issste_08_bd  DECIMAL(15,0),
                                 aplic_int_viv_92_bd   DECIMAL(15,0),
                                 f_redencion_bono_bd   CHAR(08),
                                 importe_bono_bd       DECIMAL(16,0),
                                 num_aport_vivienda    DECIMAL(5,0),
                                 dias_pag_cs           DECIMAL(5,0),
                                 num_aport_issste      DECIMAL(5,0),
                                 cve_siefore_oss       CHAR(08),
                                 f_aplica_oss          CHAR(08),
                                 ind_auto_vinc_am      CHAR(01),
                                 ind_apo_vol_ded       CHAR(01))

  CREATE TABLE taa_cta_rec_det5 (tpo_registro          CHAR(02),
                                 cont_servicio         DECIMAL(10,0),
                                 tpo_ent_rec_cta       CHAR(02),
                                 cve_ent_rec_cta       CHAR(03),
                                 tpo_ent_transf_cta    CHAR(02),
                                 cve_ent_transf_cta    CHAR(03),
                                 tpo_traspaso          CHAR(02),
                                 f_apo_vol_patron      CHAR(08),
                                 f_apo_vol_vent        CHAR(08),
                                 f_apo_largo_plazo     CHAR(08),
                                 f_apo_pers_lar_pla    CHAR(08),
                                 curp                  CHAR(18),
                                 nss                   CHAR(11),
                                 f_val_acciones        CHAR(08),
                                 cve_sub_1             CHAR(02),
                                 imp_sub_1             DECIMAL(15,0),
                                 siefore_1             CHAR(08),
                                 num_tot_acc_1         DECIMAL(16,0),
                                 precio_acc_1          DECIMAL(11,0),
                                 cve_sub_2             CHAR(02),
                                 imp_sub_2             DECIMAL(15,0),
                                 siefore_2             CHAR(08),
                                 num_tot_acc_2         DECIMAL(16,0),
                                 precio_acc_2          DECIMAL(11,0),
                                 cve_sub_3             CHAR(02),
                                 imp_sub_3             DECIMAL(15,0),
                                 siefore_3             CHAR(08),
                                 num_tot_acc_3         DECIMAL(16,0),
                                 precio_acc_3          DECIMAL(11,0),
                                 cve_sub_4             CHAR(02),
                                 imp_sub_4             DECIMAL(15,0),
                                 siefore_4             CHAR(08),
                                 num_tot_acc_4         DECIMAL(16,0),
                                 precio_acc_4          DECIMAL(11,0),
                                 cve_sub_5             CHAR(02),
                                 imp_sub_5             DECIMAL(15,0),
                                 siefore_5             CHAR(08),
                                 num_tot_acc_5         DECIMAL(16,0),
                                 precio_acc_5          DECIMAL(11,0),
                                 cve_sub_6             CHAR(02),
                                 imp_sub_6             DECIMAL(15,0),
                                 siefore_6             CHAR(08),
                                 num_tot_acc_6         DECIMAL(16,0),
                                 precio_acc_6          DECIMAL(11,0),
                                 cve_sub_7             CHAR(02),
                                 imp_sub_7             DECIMAL(15,0),
                                 siefore_7             CHAR(08),
                                 num_tot_acc_7         DECIMAL(16,0),
                                 precio_acc_7          DECIMAL(11,0),
                                 cve_sub_8             CHAR(02),
                                 imp_sub_8             DECIMAL(15,0),
                                 siefore_8             CHAR(08),
                                 num_tot_acc_8         DECIMAL(16,0),
                                 precio_acc_8          DECIMAL(11,0),
                                 cod_res_operacion     CHAR(02),
                                 diag_proceso          CHAR(15),
                                 per_ultima_apo        CHAR(08),
                                 ult_sal_diario_int    DECIMAL(15,0))

  CREATE TABLE taa_cta_rec_sum (tpo_registro          CHAR(02),
                                cant_reg_detalle      DECIMAL(9,0),
                                num_tot_apl_int_viv97 DECIMAL(18,0),
                                tot_sdo_viv97         DECIMAL(15,0),
                                num_tot_apl_int_iss92 DECIMAL(18,0),
                                tot_sdo_issste92      DECIMAL(15,0),
                                num_tot_apl_int_viv92 DECIMAL(18,0),
                                tot_sdo_viv92         DECIMAL(15,0),
                                num_tot_apl_int_iss08 DECIMAL(18,0),
                                tot_sdo_issste08      DECIMAL(15,0),
                                tot_imp_bono_issste   DECIMAL(17,0),
                                cve_subcuenta_1       CHAR(02),
                                tot_sdo_sub_1         DECIMAL(15,0),
                                cve_subcuenta_2       CHAR(02),
                                tot_sdo_sub_2         DECIMAL(15,0),
                                cve_subcuenta_3       CHAR(02),
                                tot_sdo_sub_3         DECIMAL(15,0),
                                cve_subcuenta_4       CHAR(02),
                                tot_sdo_sub_4         DECIMAL(15,0),
                                cve_subcuenta_5       CHAR(02),
                                tot_sdo_sub_5         DECIMAL(15,0),
                                cve_subcuenta_6       CHAR(02),
                                tot_sdo_sub_6         DECIMAL(15,0),
                                cve_subcuenta_7       CHAR(02),
                                tot_sdo_sub_7         DECIMAL(15,0),
                                cve_subcuenta_8       CHAR(02),
                                tot_sdo_sub_8         DECIMAL(15,0),
                                cve_subcuenta_9       CHAR(02),
                                tot_sdo_sub_9         DECIMAL(15,0),
                                cve_subcuenta_10      CHAR(02),
                                tot_sdo_sub_10        DECIMAL(15,0),
                                cve_subcuenta_11      CHAR(02),
                                tot_sdo_sub_11        DECIMAL(15,0),
                                cve_subcuenta_12      CHAR(02),
                                tot_sdo_sub_12        DECIMAL(15,0),
                                cve_subcuenta_13      CHAR(02),
                                tot_sdo_sub_13        DECIMAL(15,0),
                                cve_subcuenta_14      CHAR(02),
                                tot_sdo_sub_14        DECIMAL(15,0),
                                cve_subcuenta_15      CHAR(02),
                                tot_sdo_sub_15        DECIMAL(15,0),
                                cve_subcuenta_16      CHAR(02),
                                tot_sdo_sub_16        DECIMAL(15,0),
                                cve_subcuenta_17      CHAR(02),
                                tot_sdo_sub_17        DECIMAL(15,0),
                                cve_subcuenta_18      CHAR(02),
                                tot_sdo_sub_18        DECIMAL(15,0),
                                cve_subcuenta_19      CHAR(02),
                                tot_sdo_sub_19        DECIMAL(15,0),
                                cve_subcuenta_20      CHAR(02),
                                tot_sdo_sub_20        DECIMAL(15,0),
                                cve_subcuenta_21      CHAR(02),
                                tot_sdo_sub_21        DECIMAL(15,0),
                                cve_subcuenta_22      CHAR(02),
                                tot_sdo_sub_22        DECIMAL(15,0),
                                cve_subcuenta_23      CHAR(02),
                                tot_sdo_sub_23        DECIMAL(15,0),
                                cve_subcuenta_24      CHAR(02),
                                tot_sdo_sub_24        DECIMAL(15,0),
                                cve_subcuenta_25      CHAR(02),
                                tot_sdo_sub_25        DECIMAL(15,0),
                                cve_subcuenta_26      CHAR(02),
                                tot_sdo_sub_26        DECIMAL(15,0),
                                cve_subcuenta_27      CHAR(02),
                                tot_sdo_sub_27        DECIMAL(15,0),
                                cve_subcuenta_28      CHAR(02),
                                tot_sdo_sub_28        DECIMAL(15,0))

  CREATE TABLE tmp_afi_sol_tar (nss                   CHAR(11),
                                f_certificacion       DATE, 
                                curp                  CHAR(18),
                                edo_interno           SMALLINT,
                                tpo_solicitud         SMALLINT)

  CREATE TABLE tmp_afi_sol_val (nss                   CHAR(11),
                                curp                  CHAR(18),
                                tpo_solicitud         SMALLINT,
                                fol_solicitud         DECIMAL(10,0),
                                f_captura             DATE, 
                                edo_interno           SMALLINT)

  CREATE TABLE tmp_afi_mae_val (nss                   CHAR(11),
                                tpo_solicitud         SMALLINT)
END FUNCTION

FUNCTION fn_taa_ctr_proceso(v_etapa, v_accion)
  DEFINE v_etapa             SMALLINT
  DEFINE v_accion            SMALLINT
  DEFINE v_cadena            CHAR(500)
  DEFINE v_id_trc_ctr_pro    DECIMAL(9,0)

  LET v_cadena = ""

  DATABASE safre_tmp
    IF v_accion = 1 THEN 
       IF v_etapa = 1 THEN
          LET v_cadena = "\n INSERT INTO tmp_taa_ctr_pro VALUES (seq_trc_ctr_pro.NEXTVAL, '", generar CLIPPED, "',", v_etapa, ", CURRENT, '','", g_usuario, "','", v_hoy, "')"
          LET v_cadena = v_cadena CLIPPED
          PREPARE ps_ins_ctr_pro1 FROM v_cadena
          EXECUTE ps_ins_ctr_pro1
       ELSE
          SELECT MAX(id_trc_ctr_pro)
          INTO   v_id_trc_ctr_pro
          FROM   tmp_taa_ctr_pro
          WHERE  nombre_archivo = generar
          AND    f_actualiza    = v_hoy
  
          LET v_cadena = "\n INSERT INTO tmp_taa_ctr_pro VALUES (", v_id_trc_ctr_pro, ", '", generar CLIPPED, "',", v_etapa, ", CURRENT, '','", g_usuario, "','", v_hoy, "')"
          LET v_cadena = v_cadena CLIPPED
          PREPARE ps_ins_ctr_pro2 FROM v_cadena
          EXECUTE ps_ins_ctr_pro2
       END IF
    ELSE
       SELECT MAX(id_trc_ctr_pro)
       INTO   v_id_trc_ctr_pro
       FROM   tmp_taa_ctr_pro
       WHERE  nombre_archivo = generar
       AND    f_actualiza    = v_hoy

       LET v_cadena = "\n UPDATE tmp_taa_ctr_pro ",
                      "\n SET    fin            = CURRENT ",
                      "\n WHERE  id_trc_ctr_pro = ", v_id_trc_ctr_pro,
                      "\n AND    nombre_archivo = '", generar CLIPPED, "'",
                      "\n AND    cod_etapa      = ", v_etapa,
                      "\n AND    f_actualiza    = '", v_hoy, "'"
       LET v_cadena = v_cadena CLIPPED
       PREPARE ps_ins_ctr_pro3 FROM v_cadena
       EXECUTE ps_ins_ctr_pro3
    END IF
  DATABASE safre_af
END FUNCTION
