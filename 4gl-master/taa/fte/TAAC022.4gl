#############################################################################
#Proyecto          => AFORE ( MEXICO )                                       #
#Propietario       => E.F.P.                                                 #
#Programa TAAC022  => RECIBE ARCHIVO SALDOS PREVIOS TRASPASO AFORE - AFORE   #
#                  => AFORE RECEPTORA                                        #
#Fecha creacion    => 29 DE DICIEMBRE 2008                                   #
#Autor             => FERNANDO HERRERA HERNANDEZ                             #
#   Adecuaciones para circular 28-20 ( LIQUIDACION BIMESTRAL )               #
#REQ:966           => JCPV 14/agosto/2012 Increase ARRAY capacity            #
#REQ:1097          => JCPV Se elimina la validacion de la marca rechazo 42   #
##############################################################################

DATABASE safre_af

GLOBALS

   DEFINE g_param_taa         RECORD LIKE seg_modulo.*

   DEFINE reg_cza_sdo_previo  RECORD
         folio                INTEGER  ,
         tipo_registro        CHAR(02) ,
         ident_servicio       CHAR(02) ,
         ident_operacion      CHAR(02) ,
         tipo_ent_origen      CHAR(02) ,
         cve_ent_origen       CHAR(03) ,
         tipo_ent_destino     CHAR(02) ,
         cve_ent_destino      CHAR(03) ,
         fecha_presentacion   DATE     ,
         consec_lote_dia      SMALLINT ,
         fecha_proceso        DATE     ,
         usuario              CHAR(08)
    END RECORD

    DEFINE reg_det_sdo_previo RECORD
         folio                INTEGER       ,
         tipo_registro        CHAR(02)      ,
         cve_operacion        CHAR(02)      ,
         tipo_traspaso        CHAR(02)      ,
         nss                  CHAR(11)      ,
         curp                 CHAR(18)      ,
         cve_subcta           CHAR(02)      ,
         siefore              CHAR(03)      ,
         saldo                DECIMAL(15,2) ,
         num_acciones         DECIMAL(22,6) ,
         fecha_valor          DATE          ,
         periodo_ult_aport    CHAR(6)       ,
         ult_sal_diario       DECIMAL(16,6) ,
         estado_reg           SMALLINT      ,
         fecha_proceso        DATE          ,
         usuario              CHAR(08)
    END RECORD

    DEFINE reg_sum_sdo_previo RECORD
         folio                INTEGER       ,
         tipo_registro        CHAR(02)      ,
         num_reg_det          INTEGER       ,
         fecha_proceso        DATE          ,
         usuario              CHAR(08)
    END RECORD

    DEFINE
        HOY                   DATE

    DEFINE 
        fecha_cabeza          CHAR(008) ,
        fecha_cza_10          CHAR(010) ,
        fecha_valor           CHAR(008) ,
        fecha_valor_10        CHAR(010) ,
        fecha_accion          CHAR(008) ,
        fecha_acc_10          CHAR(010) ,
        enter                 CHAR(001) ,
        generar               CHAR(020) ,
        archivo_traspaso      CHAR(500)

    DEFINE 
        id_op                 ,
        bandera               ,
        total                 ,
        aceptados             ,
        rechazo               ,
        cuantos               ,
        s_codigo_afore        INTEGER

    DEFINE datos RECORD
        siefore              CHAR(03),
        #afore_desc           CHAR(25),
        tipo_traspaso        CHAR(02),
        total                INTEGER  
    END RECORD

    DEFINE 
        g_usuario            CHAR(8),
        hora                 CHAR(8),
        g_lista              CHAR(100),
        COMANDO              CHAR(100)

    DEFINE
        tot_total            INTEGER  

    DEFINE g_afore           RECORD LIKE tab_afore_local.*

    DEFINE reg_bat           RECORD
        pid                  INTEGER,
        proceso_cod          INTEGER,
        opera_cod            INTEGER,
        nombre_archivo       CHAR(25)
    END RECORD

    DEFINE
        bnd_proceso          SMALLINT

    DEFINE arr_pant_rech     ARRAY[20000] OF RECORD        #966
       nss                   LIKE det_tra_sdo_previo.nss,
       folio                 LIKE det_tra_sdo_previo.folio,
       curp                  LIKE det_tra_sdo_previo.curp,
       tipo_traspaso         LIKE det_tra_sdo_previo.tipo_traspaso,
       fecha_valor           LIKE det_tra_sdo_previo.fecha_valor,
       estado_reg            LIKE det_tra_sdo_previo.estado_reg,
       desc_edo_reg          CHAR(20)
    END RECORD

    DEFINE tot_reg_rch       ,
           posicion          INTEGER

    DEFINE query_rch         CHAR(1000)

    DEFINE r_control         RECORD LIKE taa_ctr_traspaso_previo.*

    DEFINE vfolio            INTEGER

    DEFINE v_salario         DECIMAL(16,6)

    DEFINE cla_where         CHAR(1000)

END GLOBALS

MAIN

    DISPLAY " "
    DISPLAY ".1"

    CALL STARTLOG('TAAC022.log')
    CALL inicio()

    IF NOT bnd_proceso THEN
       DEFER INTERRUPT
         OPTIONS 
         INPUT WRAP,
         PROMPT LINE LAST  
         #ACCEPT KEY CONTROL-I
       CALL ventana_menu()
    ELSE
       CALL sube_archivo()
       CALL validacion_previa()
       CALL carga_tablas()
       CALL lee_archivo()
       CALL actualiza_operacion()
       CALL control_archivo()
       CALL imprime_reporte()
    END IF

END MAIN

FUNCTION inicio()

  LET reg_bat.pid            = ARG_VAL(1)
  LET reg_bat.proceso_cod    = ARG_VAL(2)
  LET reg_bat.opera_cod      = ARG_VAL(3)
  LET reg_bat.nombre_archivo = ARG_VAL(4)

  LET bnd_proceso = 0

  IF reg_bat.pid THEN
     DISPLAY "INICIANDO PROCESO ..."
     LET bnd_proceso = 1
  END IF

  WHENEVER ERROR CONTINUE
     DATABASE safre_tmp

     DROP TABLE tmp_pla_sdo_pre1
  WHENEVER ERROR STOP

  CREATE TABLE tmp_pla_sdo_pre1 (n_registros  CHAR(100))

  DATABASE safre_af

  LET HOY  = TODAY 
  LET hora = TIME

  SELECT *, USER
  INTO   g_param_taa.*, g_usuario
  FROM   seg_modulo
  WHERE  modulo_cod = 'taa'

  SELECT codigo_afore
  INTO   s_codigo_afore
  FROM   tab_afore_local

  LET reg_cza_sdo_previo.fecha_proceso = HOY
  LET reg_det_sdo_previo.fecha_proceso = HOY
  LET reg_sum_sdo_previo.fecha_proceso = HOY

  LET reg_cza_sdo_previo.usuario       = g_usuario
  LET reg_det_sdo_previo.usuario       = g_usuario
  LET reg_sum_sdo_previo.usuario       = g_usuario

END FUNCTION

FUNCTION proceso_principal()

  DEFINE bnd_esc SMALLINT

  LET bnd_esc = FALSE

  OPEN WINDOW ventana_1 AT 4,4 WITH FORM "TAAC0011" ATTRIBUTE(BORDER)
  DISPLAY " TAAC022  ARCHIVO DE SALDOS PREVIOS TRASPASO AFORE RECEPTORA               " AT 3,1 ATTRIBUTE(REVERSE)
  DISPLAY "                           < Ctrl-C > Salir                                " AT 1,1 ATTRIBUTE(REVERSE)

  DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

  INPUT BY NAME generar

    BEFORE INPUT
      SELECT 'X'
        FROM taa_ctr_traspaso_previo
       WHERE ini_incorpora IS NOT NULL
         AND ini_incorpora <> DATETIME (1899-12-31) YEAR TO DAY
         AND fin_incorpora IS NULL
      GROUP BY 1

      IF SQLCA.SQLCODE = 0 THEN
         PROMPT "PROCESO DE INCORPORACION ARCHIVO SALDOS PREVIO ",
                " EJECUTANDOSE,[Enter] p/salir" FOR enter
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
         SELECT 'X'
           FROM taa_ctr_traspaso_previo
          WHERE nombre_archivo = generar
            #AND ini_incorpora IS NOT NULL
            #AND ini_incorpora <> DATETIME (1899-12-31) YEAR TO DAY
         GROUP BY 1

         IF SQLCA.SQLCODE = 0 THEN
            PROMPT "ARCHIVO YA PROCESADO,[Enter] p/salir" FOR enter
            LET bnd_esc = TRUE
            EXIT INPUT
         END IF

         LET archivo_traspaso = g_param_taa.ruta_rescate CLIPPED,"/",
                                generar CLIPPED

         DATABASE safre_tmp
           LOAD FROM archivo_traspaso INSERT INTO tmp_pla_sdo_pre1
         DATABASE safre_af

         SELECT COUNT(*)
           INTO cuantos
           FROM safre_tmp:tmp_pla_sdo_pre1

         IF cuantos = 0 THEN
            DISPLAY  " NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO " 
            AT 19,2 ATTRIBUTE(REVERSE)
            SLEEP 3
            NEXT FIELD generar
         ELSE
            EXIT INPUT
         END IF
      END IF

    ON KEY (INTERRUPT)
       ERROR "PROCESO CANCELADO" ATTRIBUTE(REVERSE)
       SLEEP 2
       ERROR ""
       LET bnd_esc = TRUE
       EXIT INPUT

  END INPUT

  IF bnd_esc THEN
     LET bnd_esc = FALSE
     CLOSE WINDOW ventana_1
     RETURN
  END IF

  ERROR "PROCESANDO INFORMACION "

  CALL validacion_previa()
  CALL carga_tablas()
  CALL lee_archivo()
  CALL imprime_reporte()
  CALL control_archivo()

  ERROR ""

  PROMPT "Presione <enter> para finalizar " FOR enter

  CLOSE WINDOW ventana_1

  DELETE FROM safre_tmp:tmp_pla_sdo_pre1
END FUNCTION

FUNCTION sube_archivo()

  LET generar = reg_bat.nombre_archivo

  SELECT 'X'
    FROM taa_ctr_traspaso_previo
   WHERE nombre_archivo = reg_bat.nombre_archivo
     #AND ini_incorpora IS NOT NULL
     #AND ini_incorpora <> DATETIME (1899-12-31) YEAR TO DAY
  GROUP BY 1

  IF SQLCA.SQLCODE = 0 THEN
     DISPLAY  "Program stopped, ARCHIVO YA PROCESADO"
     EXIT PROGRAM
  END IF

  LET archivo_traspaso = g_param_taa.ruta_rescate CLIPPED,"/",
                         reg_bat.nombre_archivo CLIPPED

  DATABASE safre_tmp
    LOAD FROM archivo_traspaso INSERT INTO tmp_pla_sdo_pre1
  DATABASE safre_af

  SELECT count(*)
  INTO   cuantos
  FROM   safre_tmp:tmp_pla_sdo_pre1

  IF cuantos = 0 OR
     cuantos IS NULL THEN
     DISPLAY  "Program stopped, NOMBRE DE ARCHIVO INCORRECTO O VACIO"
     EXIT PROGRAM
  END IF

END FUNCTION

FUNCTION validacion_previa()

  DEFINE
    c2_tipo_registro CHAR(2),
    tipo_det         CHAR(2)

  DEFINE
    sw_1             ,
    sw_2             ,
    sw_9             SMALLINT

  DEFINE
    tot_detalle      ,
    tot_sumario      DECIMAL(10,0)

  DECLARE cur_2 CURSOR FOR
  SELECT UNIQUE(n_registros[1,2])
  FROM   safre_tmp:tmp_pla_sdo_pre1

  LET sw_1 = 0
  LET sw_2 = 0
  LET sw_9 = 0

  FOREACH cur_2 INTO c2_tipo_registro
    CASE c2_tipo_registro
      WHEN "01" LET sw_1 = 1
      WHEN "02" LET sw_2 = 1
      WHEN "09" LET sw_9 = 1
    END CASE
  END FOREACH

  IF sw_1 = 0 THEN
     IF bnd_proceso THEN
        DISPLAY "Program stopped, NO EXISTE ENCABEZADO"
     ELSE
        PROMPT "SE RECHAZA EL LOTE. NO EXISTE ENCABEZADO" FOR enter
     END IF

     EXIT PROGRAM
  END IF

  IF sw_2 = 0 THEN
     IF bnd_proceso THEN
        DISPLAY "Program stopped, NO EXISTEN REGISTROS DE DETALLE"
     ELSE
        PROMPT "SE RECHAZA EL LOTE. NO EXISTEN REGISTROS DE DETALLE" 
        FOR enter
     END IF

     EXIT PROGRAM
  END IF

  IF sw_9 = 0 THEN
     IF bnd_proceso THEN
        DISPLAY "Program stopped, NO EXISTE SUMARIO"
     ELSE
        PROMPT "SE RECHAZA EL LOTE. NO EXISTE SUMARIO" FOR enter
     END IF

     EXIT PROGRAM
  END IF

  SELECT COUNT(*)
    INTO tot_detalle
    FROM safre_tmp:tmp_pla_sdo_pre1
  WHERE n_registros[1,2] IN ( "02")

  SELECT n_registros[3,11]
    INTO tot_sumario
    FROM safre_tmp:tmp_pla_sdo_pre1
   WHERE n_registros[1,2] = "09"

  IF tot_detalle <> tot_sumario THEN
     IF bnd_proceso THEN
        DISPLAY "Program stopped, TOTAL DE REGISTROS NO CORRESPONDE", 
                " CON EL SUMARIO"
     ELSE
        PROMPT "SE RECHAZA EL LOTE, TOTAL DE REGISTROS NO CORRESPONDE ",
               " CON EL SUMARIO" FOR enter
     END IF
     EXIT PROGRAM
  END IF

END FUNCTION

FUNCTION carga_tablas()
  DEFINE carga_reg            CHAR(100)
  DEFINE c2_ident_operacion   CHAR(002)

  LET c2_ident_operacion = ""

  SELECT MAX(folio)
  INTO   vfolio
  FROM   taa_ctr_traspaso_previo
  IF STATUS = NOTFOUND THEN
     LET vfolio = 1
  ELSE
     LET vfolio = vfolio + 1 
  END IF
  
  IF vfolio IS NULL THEN
     LET vfolio = 1  
  END IF

  LET reg_cza_sdo_previo.folio = vfolio
  LET reg_det_sdo_previo.folio = vfolio
  LET reg_sum_sdo_previo.folio = vfolio

  DISPLAY  " FOLIO DE RECEPCION DEL ARCHIVO DE SALDOS PREVIOS: ", vfolio
  AT 18,2 ATTRIBUTE(REVERSE)

  DECLARE cur_1 CURSOR FOR
  SELECT * 
    FROM safre_tmp:tmp_pla_sdo_pre1

  FOREACH cur_1 INTO carga_reg

     #---ENCABEZADO SOLICITUD TRASPASO AUTOMATICO---#

     IF carga_reg[1,2] = "01" AND carga_reg[5,6] = "29" THEN
        LET c2_ident_operacion = "01"
        LET reg_cza_sdo_previo.tipo_registro      = carga_reg[01,02] 
        LET reg_cza_sdo_previo.ident_servicio     = carga_reg[03,04] 
        LET reg_cza_sdo_previo.ident_operacion    = carga_reg[05,06] 
        LET reg_cza_sdo_previo.tipo_ent_origen    = carga_reg[07,08] 
        LET reg_cza_sdo_previo.cve_ent_origen     = carga_reg[09,11] 
        LET reg_cza_sdo_previo.tipo_ent_destino   = carga_reg[12,13] 
        LET reg_cza_sdo_previo.cve_ent_destino    = carga_reg[14,16] 
        LET fecha_cabeza                          = carga_reg[20,27] 
        LET reg_cza_sdo_previo.consec_lote_dia    = carga_reg[28,30] 

        LET fecha_cza_10 = fecha_cabeza[5,6],"/",
                           fecha_cabeza[7,8],"/",
                           fecha_cabeza[1,4]

        LET reg_cza_sdo_previo.fecha_presentacion = fecha_cza_10

        INSERT INTO cza_tra_sdo_previo VALUES(reg_cza_sdo_previo.*)

        LET id_op                            = 
            reg_cza_sdo_previo.ident_operacion 
        LET reg_det_sdo_previo.cve_operacion = 
            reg_cza_sdo_previo.ident_operacion
     END IF

     #---DETALLE SOLICITUD TRASPASOS AUT---#

     IF carga_reg[1,2] = "02" AND c2_ident_operacion = "01" THEN
        LET reg_det_sdo_previo.tipo_registro        = carga_reg[001,002]
        LET reg_det_sdo_previo.cve_operacion        = carga_reg[003,004]
        LET reg_det_sdo_previo.tipo_traspaso        = carga_reg[005,006]
        LET reg_det_sdo_previo.nss                  = carga_reg[007,017]
        LET reg_det_sdo_previo.curp                 = carga_reg[018,035]
        LET reg_det_sdo_previo.cve_subcta           = carga_reg[036,037]
        LET reg_det_sdo_previo.siefore              = carga_reg[038,040]
        LET reg_det_sdo_previo.saldo                = carga_reg[041,055]
        LET reg_det_sdo_previo.num_acciones         = carga_reg[056,071]
        LET fecha_valor                             = carga_reg[072,079]
        LET reg_det_sdo_previo.periodo_ult_aport    = carga_reg[080,085]
        LET v_salario                               = carga_reg[086,100]

        LET reg_det_sdo_previo.ult_sal_diario       = v_salario / 100

        LET fecha_valor_10 = fecha_valor[5,6],"/",
                             fecha_valor[7,8],"/",
                             fecha_valor[1,4]
        LET reg_det_sdo_previo.fecha_valor = fecha_valor_10

        LET reg_det_sdo_previo.num_acciones = 
            reg_det_sdo_previo.num_acciones / 1000000

        LET reg_det_sdo_previo.saldo        = 
            reg_det_sdo_previo.saldo        / 100

        LET reg_det_sdo_previo.estado_reg   = 100

        INSERT INTO det_tra_sdo_previo VALUES(reg_det_sdo_previo.*)

        IF reg_det_sdo_previo.siefore IS NOT NULL AND
           reg_det_sdo_previo.siefore <> ' '      THEN
           {CALL valida_siefore(reg_det_sdo_previo.siefore,
                               reg_det_sdo_previo.folio,
                               reg_det_sdo_previo.nss)}
        END IF
     END IF

     #---SUMARIO SOLICITUD TRASPASO AUTOMATICO---#

     IF carga_reg[1,2] = "09" AND c2_ident_operacion = "01" THEN
        LET c2_ident_operacion = " "
        LET reg_sum_sdo_previo.tipo_registro       = carga_reg[001,002]
        LET reg_sum_sdo_previo.num_reg_det         = carga_reg[003,011]

        INSERT INTO sum_tra_sdo_previo VALUES(reg_sum_sdo_previo.*)
     END IF
  END FOREACH
END FUNCTION

FUNCTION lee_archivo()
  DEFINE vn_seguro  CHAR(11)
  DEFINE vts_org    SMALLINT
  DEFINE cont       INTEGER
  DEFINE vn_folio   DECIMAL(10,0)
  DEFINE vfol_org   DECIMAL(10,0)
  DEFINE vst        SMALLINT

  LET bandera   = 0
  LET cont      = 0
  LET vn_seguro = NULL

  INITIALIZE reg_det_sdo_previo.* TO NULL

  DECLARE cur_pre CURSOR FOR
  SELECT *
  FROM   det_tra_sdo_previo
  WHERE  folio = vfolio
  FOREACH cur_pre INTO reg_det_sdo_previo.*

    IF reg_det_sdo_previo.cve_operacion = '29' THEN
      IF (reg_det_sdo_previo.tipo_traspaso <> 21 AND
           reg_det_sdo_previo.tipo_traspaso <> 38) THEN
        IF  reg_det_sdo_previo.tipo_traspaso <> 71 AND
        	  reg_det_sdo_previo.tipo_traspaso <> 72 THEN         
        	  	          	  
          SELECT "X"
          FROM   afi_mae_afiliado ma
          WHERE  ma.n_seguro = reg_det_sdo_previo.nss

          IF SQLCA.SQLCODE <> 0 THEN
             SELECT "X"
             FROM   afi_solicitud ms
             WHERE  ms.n_seguro = reg_det_sdo_previo.nss
             AND    ms.status_interno in(50,70)
             GROUP BY 1

             IF SQLCA.SQLCODE <> 0 THEN
                LET vst     = 41
                LET bandera = 1
             END IF
          ELSE
             SELECT "X"                            
             FROM   cta_act_marca cc
             WHERE  cc.nss = reg_det_sdo_previo.nss
             AND    cc.marca_cod IN(SELECT b.marca_resulta
                                    FROM   tab_marca b
                                    WHERE  b.ind_habilita = 1)
             GROUP BY 1
---- 1098 ->
            { IF SQLCA.SQLCODE <> 0 THEN
                LET vst     = 42 
                LET bandera = 1
             END IF }
             
          END IF --FIN VALIDA EXISTE EN EL MAESTRO
        ELSE
        	
          SELECT "X"
          FROM   afi_mae_afiliado ma
          WHERE  ma.n_unico = reg_det_sdo_previo.curp
          GROUP  BY 1                                                 #1116

          IF SQLCA.SQLCODE <> 0 THEN
             SELECT "X"
             FROM   afi_solicitud ms
             WHERE  ms.n_unico        = reg_det_sdo_previo.curp
             AND    ms.status_interno in(50,70)
             GROUP BY 1

             IF SQLCA.SQLCODE <> 0 THEN
                LET vst     = 41
                LET bandera = 1
             END IF
          END IF                      
        END IF  #tt <> 71
      END IF   #tt <> 21 38
    END IF     #operacion 29

    IF bandera THEN
       UPDATE det_tra_sdo_previo
          SET estado_reg = vst
        WHERE nss        = reg_det_sdo_previo.nss
          AND folio      = reg_det_sdo_previo.folio
    END IF

    LET bandera = 0
  END FOREACH
END FUNCTION

FUNCTION imprime_reporte()

  SELECT codigo_afore
    FROM tab_afore_local

  INITIALIZE datos.* TO NULL

  SELECT *
    INTO g_afore.*
    FROM tab_afore_local

  LET g_lista = g_param_taa.ruta_listados CLIPPED, "/",g_usuario CLIPPED,
                ".afili_prev_",reg_cza_sdo_previo.ident_operacion,
                ".",HOY using "DDMMYY"

  START REPORT listado TO g_lista

    DECLARE cur1 CURSOR for
    {SELECT a.siefore, b.afore_desc, a.tipo_traspaso, count(*)
    FROM   det_tra_sdo_previo a, tab_afore b, tab_siefore c
    WHERE  a.siefore       = c.siefore_cod
    AND    c.afore_cod     = b.afore_cod
    AND    a.tipo_registro = "02"
    GROUP BY 1,2,3
    ORDER BY 1,3}
    
    SELECT a.siefore, a.tipo_traspaso, count(*)
      FROM   det_tra_sdo_previo a
     WHERE  a.tipo_registro = "02"
       AND  a.folio         = vfolio
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
       FOREACH cur1 into datos.*
         LET tot_total = tot_total + datos.total
         OUTPUT TO REPORT listado(datos.*)
       END FOREACH 
    END IF 

  FINISH REPORT listado

  LET g_lista = g_param_taa.ruta_listados CLIPPED, "/",g_usuario CLIPPED,
                ".afili_prev_",reg_cza_sdo_previo.ident_operacion,
                ".",HOY using "DDMMYY"

  LET COMANDO = "lp ",g_lista
  RUN COMANDO

  IF bnd_proceso THEN
     DISPLAY "SE CONCLUYE REPORTE"
  ELSE
     ERROR "SE CONCLUYE PROCESO"
     SLEEP 2
  END IF

END FUNCTION

REPORT listado(datos)

  DEFINE datos      RECORD
    siefore         CHAR(3),
    #afore_desc      CHAR(25),
    tipo_traspaso   CHAR(2),
    total           INTEGER  
  END RECORD

  DEFINE campo      RECORD
    cod_afore       SMALLINT,
    raz_social      CHAR(50),
    des_titulo      CHAR(23)
  END RECORD

  OUTPUT
    PAGE LENGTH   60
    LEFT MARGIN   0
    RIGHT MARGIN  132
    TOP MARGIN    0
    BOTTOM MARGIN 0

  FORMAT
  PAGE HEADER

    SELECT a.codigo_afore, a.razon_social, b.afore_desc
    INTO   campo.cod_afore, campo.raz_social, campo.des_titulo
    FROM   tab_afore_local a, tab_afore b
    WHERE  a.codigo_afore = b.afore_cod

    PRINT COLUMN 4,'\033e\033(s218T\033(s11H\033(s7B',"INFORMACION DE SALDOS PREVIOS DE TRASPASO A AFORE ",campo.des_titulo, HOY

    IF id_op = 29 THEN
       PRINT COLUMN 4,'\033e\033(s218T\033(s11H\033(s7B',"                               SALDOS PREVIOS                                "
    ELSE
       PRINT COLUMN 4,'\033e\033(s218T\033(s11H\033(s7B',"                           SALDOS DESCONOCIDOS                               "
    END IF

    PRINT COLUMN 4,'\033e\033(s218T\033(s11H\033(s7B',"NOMBRE DEL ARCHIVO : ",generar, "    FECHA DE PRESENTACION : ",fecha_cza_10

    PRINT
    PRINT COLUMN 1,'\033e\033(s218T\033(s11H\033(s7N'
    PRINT COLUMN 2,campo.cod_afore,"     ",campo.raz_social
    PRINT
    PRINT COLUMN 1,"\332\304\304\304\304\304\304\304\302\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\302\304\304\304\304\304\304\304\304\302\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\277"
    PRINT COLUMN 1,"\263",COLUMN 3,"SIEFO ",COLUMN 8,"\263",COLUMN 20,"AFORE",COLUMN 43,"\263",COLUMN 45,"ORIGEN",COLUMN 52,"\263",COLUMN 54,"AFILIADOS RECIBIDOS",COLUMN 76,"\263"
    PRINT COLUMN 1,"\300\304\304\304\304\304\304\304\301\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\301\304\304\304\304\304\304\304\304\301\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\331"

  ON EVERY ROW

     PRINT PRINT
       COLUMN 4,datos.siefore,
       #COLUMN 11,datos.afore_desc,
       COLUMN 47,datos.tipo_traspaso,
       COLUMN 60,datos.total USING "&&&&&&"

     PRINT 
       IF lineno > 57 THEN
          SKIP TO TOP OF PAGE
       END IF

  ON LAST ROW
     PRINT COLUMN 1,"\332\304\304\304\304\304\304\304\302\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\302\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\277"
     PRINT COLUMN 1,"\263",COLUMN 3,"TOTAL ","\263 ",COLUMN 52,"\263",COLUMN 60,tot_total USING "&&&&&&",COLUMN 76,"\263"
     PRINT COLUMN 1,"\300\304\304\304\304\304\304\304\301\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\301\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\304\331"

END REPORT

FUNCTION actualiza_operacion()

  UPDATE bat_ctr_operacion
  SET    estado_cod       = 4,
         fecha_fin        = CURRENT,
         nom_archivo      = reg_bat.nombre_archivo
  WHERE  pid              = reg_bat.pid
  AND    proceso_cod      = reg_bat.proceso_cod
  AND    opera_cod        = reg_bat.opera_cod

END FUNCTION

FUNCTION ventana_menu()

  OPEN WINDOW ventana_2 AT 4,4 WITH 19 ROWS, 74 COLUMNS ATTRIBUTE(BORDER, PROMPT LINE LAST-1)
  DISPLAY " TAAC022  ARCHIVO DE SALDOS PREVIOS DE TRASPASO AFORE RECEPTORA               " AT 3,1 ATTRIBUTE(REVERSE)
  DISPLAY "------------------------------------------------------------------------------" AT 4,1
  DISPLAY HOY USING " DD-MM-YYYY " AT 3,61 ATTRIBUTE(REVERSE)

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
  DEFINE v_fecha_presenta      DATE
  DEFINE v_fecha_liq           DATE

  SELECT max(fecha_presentacion)
    INTO v_fecha_presenta
    FROM cza_tra_sdo_previo 
   WHERE ident_operacion = "29"

  IF STATUS = NOTFOUND THEN
     PROMPT "El archivo cargado no es de traspasos previos.[Enter] ",
            "Para salir." FOR enter
     RETURN
  ELSE
     CALL fechas(v_fecha_presenta) RETURNING v_fecha_liq
     #IF HOY <= v_fecha_liq THEN
        CALL ventana_rechazos()
     {ELSE
        PROMPT "EL ARCHIVO YA FUE CARGADO, NO EXISTEN RECHAZOS. ",
               "[Enter] Para salir." FOR enter
        RETURN
     END IF}
  END IF

END FUNCTION

FUNCTION ventana_rechazos()
  DEFINE contador       INTEGER

  LET tot_reg_rch = 0
  LET posicion    = 1

  OPEN WINDOW ventana_3 AT 4,4 WITH FORM "TAAC0221" ATTRIBUTE(BORDER)
     DISPLAY " TAAC022  ARCHIVO DE SALDOS PREVIOS DE TRASPASO AFORE RECEPTORA                 " AT 3,1 ATTRIBUTE(REVERSE)
     DISPLAY "         < Ctrl-C > Salir                           <Ctrl-P> Imprimir           " AT 1,1 ATTRIBUTE(REVERSE)
     DISPLAY HOY USING " DD-MM-YYYY " AT 3,62 ATTRIBUTE(REVERSE)

  LET int_flag = FALSE

  CONSTRUCT cla_where ON folio,
                         nss 
                    FROM folio,
                         nss

  IF int_flag = TRUE THEN
     LET int_flag = FALSE
     ERROR "BUSQUEDA CANCELADA..."
     SLEEP 2
     ERROR ""
     CLEAR SCREEN
     RETURN
  END IF

  LET query_rch = " SELECT unique b.nss, b.folio, b.curp,",
                  " b.tipo_traspaso, b.fecha_valor,",
                  " b.estado_reg, CASE WHEN b.estado_reg = 41",
                  " THEN 'STATUS SOL INCORRECTA'",
                  " WHEN b.estado_reg = 40",
                  " THEN 'SIEFORE NO EXISTE EN CAT'",
                  " WHEN b.estado_reg = 42",
                  " THEN 'SIN MARCA DE REINGRESO'",
                  " END CASE",
                  " FROM det_tra_sdo_previo b",
                  " WHERE  ", cla_where CLIPPED,
                  " AND b.cve_operacion = '29'",
                  " AND b.estado_reg   <> 100",
                  " ORDER BY b.fecha_valor desc, b.nss"

  LET query_rch = query_rch CLIPPED

  PREPARE prep_rechazados FROM query_rch
  DECLARE cur_pant_rch CURSOR FOR prep_rechazados

  FOREACH cur_pant_rch INTO arr_pant_rech[posicion].*
     LET posicion = posicion + 1
  END FOREACH

  INITIALIZE arr_pant_rech[posicion].* TO NULL
  LET tot_reg_rch = posicion - 1

  IF tot_reg_rch >= 1 THEN

     DISPLAY BY NAME tot_reg_rch

     CALL SET_COUNT (tot_reg_rch)
     DISPLAY ARRAY arr_pant_rech TO scr_1.*

       ON KEY (Control-p)
          LET hora = TIME
          LET g_lista = g_param_taa.ruta_listados CLIPPED, "/",
                        g_usuario CLIPPED, ".reg_rech_",
                        HOY using "DDMMYY",hora[1,2],hora[4,5] CLIPPED

          START REPORT imprime_rechazados TO g_lista
            FOR contador = 1 TO tot_reg_rch
                OUTPUT TO REPORT imprime_rechazados 
                (arr_pant_rech[contador].*,tot_reg_rch)
            END FOR
          FINISH REPORT imprime_rechazados

          ERROR "REPORTE GENERADO"
          SLEEP 2

       ON KEY (INTERRUPT)
          EXIT DISPLAY

     END DISPLAY

     IF int_flag = TRUE THEN
        CLOSE WINDOW ventana_3
        LET int_flag = FALSE
        RETURN
     END IF

  ELSE
     ERROR "El archivo no tiene rechazos."
     SLEEP 3
     ERROR ""
     CLOSE WINDOW ventana_3
  END IF
  RETURN

END FUNCTION

REPORT imprime_rechazados(h,reg_total)
  DEFINE reg_total         INTEGER

  DEFINE h RECORD
         nss               LIKE det_tra_sdo_previo.nss,
         folio             LIKE det_tra_sdo_previo.folio,
         curp              LIKE det_tra_sdo_previo.curp,
         tipo_traspaso     LIKE det_tra_sdo_previo.tipo_traspaso,
         fecha_valor       LIKE det_tra_sdo_previo.fecha_valor,
         estado_reg        LIKE det_tra_sdo_previo.estado_reg,
         desc_edo_reg      CHAR(20)
  END RECORD

  DEFINE campo RECORD
    cod_afore   SMALLINT,
    raz_social  CHAR(50)
  END RECORD

  OUTPUT
    TOP MARGIN 1
    BOTTOM MARGIN 0
    LEFT MARGIN 0
    RIGHT MARGIN 0
    PAGE LENGTH 60

  FORMAT

    PAGE HEADER
      SELECT a.codigo_afore,a.razon_social
      INTO campo.cod_afore,campo.raz_social
      FROM tab_afore_local a


      PRINT COLUMN 03, campo.cod_afore,
            COLUMN 11, campo.raz_social,
            COLUMN 68,TODAY USING "DD-MM-YYYY"
      SKIP 2 LINE
      PRINT COLUMN 10,"REGISTROS RECHAZADOS DE SALDOS DE TRASPASOS AFORE RECEPTORA"
      PRINT
      PRINT COLUMN 10,"FOLIO: ", h.folio
      SKIP 2 LINE
      PRINT COLUMN 01,"--------------------------------------------------------------------------------"
      PRINT COLUMN 03,"NSS",
            COLUMN 18,"CURP",
            COLUMN 30,"TIPO",
            COLUMN 40,"FECHA",
            COLUMN 52,"ESTADO"
      PRINT COLUMN 30,"TRASPASO",
            COLUMN 40,"VALOR",
            COLUMN 52,"REGISTRO"
      PRINT COLUMN 01,"--------------------------------------------------------------------------------"
      SKIP 1 LINE

    ON EVERY ROW
       PRINT h.nss,
         COLUMN 13, h.curp CLIPPED,
         COLUMN 30, h.tipo_traspaso,
         COLUMN 40, h.fecha_valor,
         COLUMN 52, h.estado_reg USING "##",
         COLUMN 55, h.desc_edo_reg

    ON LAST ROW
       SKIP 2 LINES
       PRINT COLUMN 30, "Total registros rechazados : ",reg_total USING "<<<<"

END REPORT

FUNCTION control_archivo()

  SELECT COUNT(unique nss)
    INTO rechazo
    FROM det_tra_sdo_previo
-- WHERE estado_reg <> 100
   WHERE folio      =  vfolio
  IF rechazo > 0 THEN

     SELECT COUNT(unique nss)
       INTO total
       FROM det_tra_sdo_previo
       WHERE folio = vfolio                                           #1097

     SELECT COUNT(unique nss)
       INTO rechazo
       FROM det_tra_sdo_previo
      WHERE folio = vfolio                                            #1097
      AND   estado_reg <> 100

     LET aceptados = total - rechazo
     SELECT 'X'
       FROM taa_ctr_traspaso_previo
      WHERE nombre_archivo     = generar
        AND fecha_presentacion = reg_cza_sdo_previo.fecha_presentacion
     GROUP BY 1
     IF SQLCA.SQLCODE <> 0 THEN

        INITIALIZE r_control TO NULL
        LET r_control.nombre_archivo     = generar
        LET r_control.id_operacion       = reg_cza_sdo_previo.ident_operacion
        LET r_control.fecha_presentacion = reg_cza_sdo_previo.fecha_presentacion
        LET r_control.fecha_recepcion    = CURRENT
        LET r_control.usr_recepcion      = g_usuario
        LET r_control.folio              = vfolio

        INSERT INTO taa_ctr_traspaso_previo VALUES ( r_control.*)
     ELSE
        UPDATE taa_ctr_traspaso_previo
           SET fecha_recepcion    = CURRENT,
               usr_recepcion      = g_usuario
         WHERE nombre_archivo     = generar
           AND fecha_presentacion = reg_cza_sdo_previo.fecha_presentacion
     END IF

     PROMPT "PROCESO FINALIZADO CON ", rechazo, " REGISTROS RECHAZADOS "
     FOR enter
     CALL ventana_rechazos()
  ELSE
     SELECT COUNT(*)
       INTO total
       FROM det_tra_sdo_previo
       WHERE folio = vfolio                                           #1097

     LET aceptados = total - rechazo

     SELECT 'X'
       FROM taa_ctr_traspaso_previo
      WHERE nombre_archivo     = generar
        AND fecha_presentacion = reg_cza_sdo_previo.fecha_presentacion
     GROUP BY 1

     IF SQLCA.SQLCODE <> 0 THEN
        INITIALIZE r_control TO NULL
        LET r_control.nombre_archivo     = generar
        LET r_control.id_operacion       = reg_cza_sdo_previo.ident_operacion
        LET r_control.fecha_presentacion = reg_cza_sdo_previo.fecha_presentacion
        LET r_control.fecha_recepcion    = CURRENT
        LET r_control.usr_recepcion      = g_usuario

        INSERT INTO taa_ctr_traspaso_previo VALUES ( r_control.*)
     ELSE
        UPDATE taa_ctr_traspaso_previo
           SET fecha_recepcion    = CURRENT,
               usr_recepcion      = g_usuario
         WHERE nombre_archivo     = generar
           AND fecha_presentacion = reg_cza_sdo_previo.fecha_presentacion
     END IF

     PROMPT "PROCESO FINALIZADO NORMALMENTE " FOR enter
  END IF

END FUNCTION

FUNCTION valida_siefore(v_sie, v_folio, v_nss)
  DEFINE v_sie   CHAR(8),
         v_folio INTEGER,
         v_nss   CHAR(11)

  SELECT 'X'
    FROM tab_siefore
   WHERE siefore_desc = v_sie
  GROUP BY 1

  IF SQLCA.SQLCODE <> 0 THEN
     IF bnd_proceso THEN
        DISPLAY "Program stopped. SIEFORE ",v_sie, " NO EXISTE EN EL CATALOGO"

        UPDATE det_tra_sdo_previo 
           SET   estado_reg = 40
         WHERE folio      = v_folio
           AND   nss        = v_nss
     ELSE
        PROMPT "LA SIEFORE ",v_sie, 
        " NO EXISTE EN EL CATALOGO. [Enter] PARA SALIR" FOR enter

        UPDATE det_tra_sdo_previo 
           SET   estado_reg = 40
         WHERE folio      = v_folio
           AND   nss        = v_nss
     END IF
  END IF

END FUNCTION

FUNCTION fechas(diaActual)
  DEFINE
    diaTmp    DATE,
    contador  SMALLINT,
    diaActual DATE,
    numDias   SMALLINT

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
    diaTmp      DATE,
    contador    SMALLINT,
    diaActual   DATE

  DEFINE
    diaHabilSig DATE,
    diaSemana   SMALLINT,
    feriado     SMALLINT,
    finSemana   SMALLINT

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
