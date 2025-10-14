#############################################################################
#Proyecto          => AFORES ( MEXICO )                                     #
#Propietario       => E.F.P.                                                #
#Programa AFIC027  => CARGA DE ARCHIVO ACTIVACION NO AFILIADOS              #
#Sistema           => AFI                                                   #
#Autor             => FERNANDO HERRERA HERNANDEZ                            #
#Fecha             => 16 DE ABRIL DE 2006                                   #
#Modificacion      => VERONICA LOPEZ SANCHEZ                                #
#Fecha Modif.      => 26 DE JULIO DE 2006                                   #
#Modificacion      => FERNANDO HERRERA HERNANDEZ                            #
#Fecha Modif.      => 08 DE MARZO DE 2008 MULTISIEFORES                     #
#############################################################################

DATABASE safre_af

GLOBALS

    DEFINE reg_sol        RECORD LIKE afi_sol_activacion.*
    DEFINE reg_mae        RECORD LIKE afi_mae_afiliado.*
    DEFINE g_paramgrales  RECORD LIKE seg_modulo.*

    DEFINE
      vresp               CHAR(01),
      aux_pausa           CHAR(01),
      enter               CHAR(01),
      vnss                CHAR(11),
      vcurp               CHAR(18),
      vcod_operacion      CHAR(02),
      vmot_rech1          CHAR(03),
      vmot_rech2          CHAR(03),
      vmot_rech3          CHAR(03),
      vfecha_envio        DATE,
      varchivo            CHAR(14),
      generar             CHAR(20),
      operacion           CHAR(40),
      carga               CHAR(50),
      corr                CHAR(100),
      vn_folio          DECIMAL(10,0)

    DEFINE
      vhoy,
      HOY,
      xx_fecha            DATE

    DEFINE
      contar_det,
      bnd_proceso         SMALLINT

    DEFINE
      total_reg,
      g_plano_act_sol,
      vtotal,
      vaprobados,
      vrechazados,
      vpendientes         INTEGER

    DEFINE reg_bat        RECORD
      pid                 INTEGER,
      proceso_cod         INTEGER,
      opera_cod           INTEGER,
      nombre_archivo      CHAR(25)
    END RECORD

    DEFINE
      opc                 CHAR(1),
      vmarca_entra        SMALLINT,
      g_usuario           CHAR(8),
      ejecuta             CHAR(300),
      vnom_afore          CHAR(122),
      vnom_proce          CHAR(122)

    DEFINE
      reg_carta           RECORD LIKE safre_af:int_ctr_carta.*,
      consulta_carta      CHAR(120)

    DEFINE
      con_curp            SMALLINT,
      sin_curp            SMALLINT

    DEFINE
      vfolio              INTEGER,
      vfolio_sua          INTEGER,
      vvalor1             DECIMAL(18,6),
      vvalor2             DECIMAL(18,6),
      vvalor11            DECIMAL(18,6),
      vfecha_viv          DATE

    DEFINE reg_cta RECORD LIKE dis_cuenta.*
    DEFINE reg_nva RECORD LIKE dis_cuenta.*
    
    #### Liquidacion multisiefores ####
    DEFINE
      vsubcuenta          SMALLINT,
      vgrupo              SMALLINT,
      dia                 INTEGER,
      dia1                INTEGER,
      vfecha_inicio       DATE,
      vfecha_inicio1      DATE,
      vfecha_final        DATE,
      vfecha1             DATE,
      fecha_liquida       DATE,
      fecha_liquida_viv   DATE,
      vsaldo              CHAR(100),
      vprov_cargo         CHAR(100),
      vprov_abono         CHAR(100),
      indebidas           CHAR(100),
      vtransferencia      CHAR(100),
      status_ind          SMALLINT  
    #### Liquidacion multisiefores ####
    
END GLOBALS

MAIN

  DISPLAY " "
  DISPLAY ".1"

  CALL STARTLOG('AFIC027.log')
  CALL inicio() #i

  IF NOT bnd_proceso THEN
     DEFER INTERRUPT
       OPTIONS INPUT WRAP,
       PROMPT LINE LAST,
       ACCEPT KEY CONTROL-I

       CALL proceso_principal()   #pp
  ELSE
     CALL rescata_valores()     #rv
     CALL actualiza_operacion() #rv
  END IF

END MAIN

FUNCTION inicio()
#i---------------

  LET reg_bat.pid         = ARG_VAL(1)
  LET reg_bat.proceso_cod = ARG_VAL(2)
  LET reg_bat.opera_cod   = ARG_VAL(3)
  LET bnd_proceso         = 0

  IF reg_bat.pid THEN
     DISPLAY "INICIANDO PROCESO ..."
     LET bnd_proceso = 1
  END IF

  LET HOY      = TODAY
  LET generar  = "S"

  SELECT *, USER
  INTO   g_paramgrales.*, g_usuario
  FROM   seg_modulo
  WHERE  modulo_cod = 'afi'

  CREATE TEMP TABLE plano_act_sol (n_registros CHAR(200))

  LET vfecha_viv = MDY(MONTH(HOY),1,YEAR(HOY))

  SELECT precio_del_dia
  INTO   vvalor1
  FROM   glo_valor_accion
  WHERE  fecha_valuacion = HOY
  AND    codigo_siefore  = 1

  SELECT precio_del_dia
  INTO   vvalor2
  FROM   glo_valor_accion
  WHERE  fecha_valuacion = HOY
  AND    codigo_siefore  = 2

  SELECT precio_del_dia
  INTO   vvalor11
  FROM   glo_valor_accion
  WHERE  fecha_valuacion = vfecha_viv
  AND    codigo_siefore  = 11

  INSERT INTO glo_folio VALUES (0)

  SELECT MAX(folio)
  INTO   vfolio
  FROM   glo_folio

  LET con_curp = 0
  LET sin_curp = 0
  LET reg_cta.tipo_movimiento  = 222
  LET reg_cta.folio            = vfolio
  LET reg_cta.consecutivo_lote = 1
  LET reg_cta.folio_sua        = ''
  LET reg_cta.fecha_pago       = HOY
  LET reg_cta.fecha_valor      = HOY
  LET reg_cta.fecha_conversion = HOY
  LET reg_cta.dias_cotizados   = 0
  LET reg_cta.sucursal         = ''
  LET reg_cta.id_aportante     = 'CARGO ACTIVACION'
  LET reg_cta.estado           = 5
  LET reg_cta.fecha_proceso    = HOY
  LET reg_cta.usuario          = g_usuario
  LET reg_cta.fecha_archivo    = HOY
  LET reg_cta.etiqueta         = 0

  LET reg_nva.tipo_movimiento  = 22
  LET reg_nva.folio            = vfolio
  LET reg_nva.consecutivo_lote = 1
  LET reg_nva.folio_sua        = ''
  LET reg_nva.fecha_pago       = HOY
  LET reg_nva.fecha_valor      = HOY
  LET reg_nva.fecha_conversion = HOY
  LET reg_nva.dias_cotizados   = 0
  LET reg_nva.sucursal         = ''
  LET reg_nva.id_aportante     = 'ABONO ACTIVACION'
  LET reg_nva.estado           = 5
  LET reg_nva.fecha_proceso    = HOY
  LET reg_nva.usuario          = g_usuario
  LET reg_nva.fecha_archivo    = HOY
  LET reg_nva.etiqueta         = 0

END FUNCTION
  
FUNCTION crea_tablas()
#ct-------------------

  WHENEVER ERROR CONTINUE
     DATABASE safre_tmp

     DROP TABLE cza_sol_act
     DROP TABLE det_sol_act
     DROP TABLE sum_sol_act
  WHENEVER ERROR STOP

  CREATE TABLE cza_sol_act
    (campo1              CHAR(02),
     campo2              CHAR(02),
     campo3              CHAR(02),
     campo4              CHAR(02),
     campo5              CHAR(03),
     campo6              CHAR(02),
     campo7              CHAR(03),
     campo8              CHAR(08),
     campo9              CHAR(03),
     campo10             CHAR(08),
     campo11             CHAR(01),
     campo12             CHAR(15));

  CREATE TABLE det_sol_act
    (tipo_registro       CHAR(02),
     id_servicio         CHAR(02),
     clave_operacion     CHAR(02),
     nss_solicitud       CHAR(11),
     curp_solicitud      CHAR(18),
     paterno             CHAR(40),
     materno             CHAR(40),
     nombres             CHAR(40),
     cod_resul_op        CHAR(02),
     motivo_rch1         CHAR(03),
     motivo_rch2         CHAR(03),
     motivo_rch3         CHAR(03));

  CREATE TABLE sum_sol_act
    (campo1              CHAR(2),
     campo2              CHAR(9),
     campo3              CHAR(9))

  DATABASE safre_af

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

  OPEN WINDOW ventana_1 AT 4,4 WITH FORM "AFIC0041" ATTRIBUTE(BORDER)
  DISPLAY " AFIC027    CARGA DE ARCHIVO RESPUESTA ACTIVACION NO AFIL                          " AT 3,1 ATTRIBUTE(REVERSE)
  DISPLAY "                             Ctrl-C > Salir                                        " AT 1,1 ATTRIBUTE(REVERSE)
  DISPLAY HOY USING "dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)

  DISPLAY g_paramgrales.ruta_rescate AT 6,10

  INPUT BY NAME generar

    AFTER FIELD generar
      IF generar IS NULL THEN
         ERROR "Campo NO puede ser NULO"
         NEXT FIELD generar
      END IF

      SELECT nombre_archivo
      INTO   varchivo
      FROM   afi_ctr_arh_reg
      WHERE  nombre_archivo = generar

      IF STATUS <> NOTFOUND THEN
         ERROR "ARCHIVO YA PROCESADO"
         SLEEP 1
         ERROR " "
         INITIALIZE generar TO NULL
         CLEAR FORM
         NEXT FIELD generar
      END IF

      LET carga = NULL
      LET carga = g_paramgrales.ruta_rescate CLIPPED,"/", generar CLIPPED

      WHENEVER ERROR CONTINUE
         LOAD FROM carga DELIMITER ","
         INSERT INTO plano_act_sol
      WHENEVER ERROR STOP

      SELECT COUNT(*)
      INTO   g_plano_act_sol
      FROM   plano_act_sol

      IF g_plano_act_sol IS NULL OR
         g_plano_act_sol = 0 THEN
         ERROR "NOMBRE DE ARCHIVO INCORRECTO O VACIO, VERIFIQUE"
         SLEEP 3
         EXIT PROGRAM
      END IF

      ERROR "Procesando Informacion"

      CALL rescata_valores()

      EXIT PROGRAM

    ON KEY (control-b)
       CALL despliega_archivos()

  END INPUT

END FUNCTION

FUNCTION rescata_valores()
#rd-----------------------

  CALL crea_tablas()        #ct
  CALL actualiza_datos()    #ad
  CALL revisa_datos()       #rd
  CALL guarda_activacion()  #ga
  CALL lista_err()          #le
  CALL actualiza_nss()      #an    

END FUNCTION

FUNCTION actualiza_datos()
#-------------------------

  DEFINE
    cont_reg      INTEGER

  DEFINE
    carga_reg     CHAR(300)

  DEFINE
    campo_011     CHAR(02),
    campo_012     CHAR(02),
    campo_013     CHAR(02),
    campo_014     CHAR(02),
    campo_015     CHAR(03),
    campo_016     CHAR(02),
    campo_017     CHAR(03),
    campo_018     CHAR(08),
    campo_019     CHAR(03),
    campo_110     CHAR(08),
    campo_111     CHAR(01),
    campo_112     CHAR(15),

    campo_01      CHAR(02),
    campo_02      CHAR(10),
    campo_03      CHAR(02),
    campo_04      CHAR(11),
    campo_05      CHAR(18),
    campo_06      CHAR(40),
    campo_07      CHAR(40),
    campo_08      CHAR(40),
    campo_09      CHAR(02),
    campo_10      CHAR(03),
    campo_11      CHAR(03),
    campo_12      CHAR(03),

    campo_201     CHAR(2),
    campo_202     CHAR(9),
    campo_203     CHAR(9)

    LET cont_reg = 0

    SELECT COUNT(*)
    INTO   total_reg
    FROM   plano_act_sol

    DECLARE cursor_1 CURSOR FOR
    SELECT  *
    FROM    plano_act_sol

    FOREACH cursor_1 INTO carga_reg
      LET cont_reg = cont_reg + 1

      IF cont_reg = 1 THEN
         LET campo_011 = carga_reg[001,002]
         LET campo_012 = carga_reg[003,004]
         LET campo_013 = carga_reg[005,006]
         LET campo_014 = carga_reg[007,008]
         LET campo_015 = carga_reg[009,011]
         LET campo_016 = carga_reg[012,013]
         LET campo_017 = carga_reg[014,016]
         LET campo_018 = carga_reg[017,024]
         LET campo_019 = carga_reg[025,027]
         LET campo_110 = carga_reg[028,035]
         LET campo_111 = carga_reg[036,036]
         LET campo_112 = carga_reg[037,051]

         INSERT INTO safre_tmp:cza_sol_act
         VALUES (campo_011,
                 campo_012,
                 campo_013,
                 campo_014,
                 campo_015,
                 campo_016,
                 campo_017,
                 campo_018,
                 campo_019,
                 campo_110,
                 campo_111,
                 campo_112)
      END IF

      IF cont_reg <> total_reg AND cont_reg <> 1 THEN
         LET campo_01 = carga_reg[001,002]
         LET campo_02 = carga_reg[003,004]
         LET campo_03 = carga_reg[005,006]
         LET campo_04 = carga_reg[007,017]
         LET campo_05 = carga_reg[018,035]
         LET campo_06 = carga_reg[036,075]
         LET campo_07 = carga_reg[076,115]
         LET campo_08 = carga_reg[116,155]
         LET campo_09 = carga_reg[156,157]
         LET campo_10 = carga_reg[158,160]
         LET campo_11 = carga_reg[161,163]
         LET campo_12 = carga_reg[164,166]
            

         INSERT INTO safre_tmp:det_sol_act
         VALUES (campo_01,
                 campo_02,
                 campo_03,
                 campo_04,
                 campo_05,
                 campo_06,
                 campo_07,
                 campo_08,
                 campo_09,
                 campo_10,
                 campo_11,
                 campo_12)
      END IF

      IF cont_reg = total_reg        THEN
         LET campo_201 = carga_reg[001,002]
         LET campo_202 = carga_reg[003,011]
         LET campo_203 = carga_reg[012,020]

         INSERT INTO safre_tmp:sum_sol_act
         VALUES (campo_201,
                 campo_202,
                 campo_203)
      END IF

    END FOREACH

END FUNCTION

FUNCTION revisa_datos()
#----------------------

  DEFINE
    rechazo_lote CHAR(9),
    rechazo_deta CHAR(3),
    l_reg        RECORD LIKE tab_rch_lote.*,
    x_reg        RECORD LIKE tab_rdeta.*,
    aux_pausa    CHAR(1)


  DEFINE
    rechazo_09   CHAR(02),
    rechazo_001  CHAR(02),
    rechazo_002  CHAR(02),
    rechazo_003  CHAR(02)

  DEFINE
    uno          CHAR(03),
    dos          CHAR(03),
    tre          CHAR(03),
    cua          CHAR(03),
    cin          CHAR(03),
    l_status_int SMALLINT

  LET contar_det = 1

  # ENCABEZADO #
  SELECT campo1,
         campo2,
         campo3,
         campo12
  INTO   rechazo_001,
         rechazo_002,
         rechazo_003,
         rechazo_lote
  FROM safre_tmp:cza_sol_act

  SELECT *
  INTO   l_reg.*
  FROM   tab_rch_lote
  WHERE  rlote_cod = rechazo_lote

  IF STATUS  <> NOTFOUND THEN
     IF bnd_proceso THEN
        DISPLAY "Program stopped, ERROR DE PROCESO,        NO PUEDE CONTINUAR "
        EXIT PROGRAM
     ELSE
        CLEAR SCREEN
        DISPLAY l_reg.rlote_cod AT 10,1
        DISPLAY l_reg.rlote_desc_c AT 11,1
        PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR" FOR aux_pausa
        EXIT PROGRAM
     END IF
  END IF

  IF rechazo_001 <> "01" THEN
     IF bnd_proceso THEN
        DISPLAY "Program stopped, Tipo de registro debe ser 01 en encabezado"
        EXIT PROGRAM
     ELSE
        CLEAR SCREEN
        DISPLAY "Tipo de registro debe ser 01 en ENCABEZADO" AT 10,1
        PROMPT "ERROR DE PROCESO NO        PUEDE CONTINUAR" FOR aux_pausa
        EXIT PROGRAM
     END IF
  END IF

  IF rechazo_002 <> "01" THEN
     IF bnd_proceso THEN
        DISPLAY "Program stopped, Identificador de servicio debe ser 01 ",
                "en encabezado"
        EXIT PROGRAM
     ELSE
        CLEAR SCREEN
        DISPLAY "Identificador de Servicio debe ser 01 en ENCABEZADO" AT 10,1
        PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR" FOR aux_pausa
        EXIT PROGRAM
     END IF
  END IF

  IF rechazo_003 <> "63" THEN
     IF bnd_proceso THEN
        DISPLAY "Program stopped, Identificador de operacion debe ser ",
                "63 en encabezado"
        EXIT PROGRAM
     ELSE
        CLEAR SCREEN
        DISPLAY "Identificador de Operacion debe ser 63 en ENCABEZADO" AT 10,1
        PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR" FOR aux_pausa
        EXIT PROGRAM
     END IF
  END IF

  # SUMARIO #

  SELECT campo1
  INTO   rechazo_09
  FROM   safre_tmp:sum_sol_act

  IF rechazo_09 <> "09" THEN
     IF bnd_proceso THEN
        DISPLAY "Program stopped, Tipo registro debe ser 09 en resumen"
        EXIT PROGRAM
     ELSE
        CLEAR SCREEN
        DISPLAY "Tipo de registro debe ser 09 en RESUMEN" AT 10,1
        PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR" FOR aux_pausa
        EXIT PROGRAM
     END IF
  END IF

END FUNCTION

FUNCTION guarda_activacion()
#----------------------------

  OPEN WINDOW w_curp AT 8,4 WITH FORM "AFIC0082"
  ATTRIBUTE (BORDER)

  LET vhoy = TODAY

  SELECT COUNT(*)
  INTO   vtotal
  FROM   safre_tmp:det_sol_act

  DECLARE c_curp CURSOR FOR
  SELECT b.nss,
         b.curp,
         a.motivo_rch1,
         a.motivo_rch2,
         a.motivo_rch3,
         a.cod_resul_op,
         b.fecha_envio
  FROM   safre_tmp:det_sol_act a, afi_sol_activacion b
  WHERE  a.curp_solicitud = b.curp
  AND    b.status_interno = 30
  AND    b.cod_operacion  = 0
  ORDER BY a.curp_solicitud

  FOREACH c_curp INTO vnss,
                      vcurp,
                      vmot_rech1,
                      vmot_rech2,
                      vmot_rech3,
                      vcod_operacion,
                      vfecha_envio

    SELECT *
      INTO reg_mae.*
      FROM afi_mae_afiliado
     WHERE n_unico        = vcurp
       AND tipo_solicitud = 8

    LET vn_folio = reg_mae.n_folio

    CASE vcod_operacion
      WHEN '02'
        LET vrechazados = vrechazados + 1
                
        CALL actualiza_02()
        CALL despliega_totales()
      WHEN '01'
        LET vaprobados = vaprobados + 1

        CALL actualiza_01()
        CALL despliega_totales()
      OTHERWISE
        LET vaprobados = vaprobados + 1

        CALL actualiza_01()
        CALL despliega_totales()
    END CASE
  END FOREACH

  INSERT INTO afi_ctr_arh_reg
  VALUES (generar, vaprobados, vrechazados, vpendientes, 0, 0, vhoy)

  CALL despliega_totales()

  IF NOT bnd_proceso THEN
     ERROR ""
     PROMPT "Proceso finalizado satisfactoriamente, [Enter] Para Continuar "
     ATTRIBUTE (REVERSE)
     FOR vresp ATTRIBUTE (REVERSE)

     CLOSE WINDOW w_curp
  ELSE
     DISPLAY "Programa finalizado satisfactoriamente"
  END IF

  RETURN

END FUNCTION

FUNCTION actualiza_02()
#a02-------------------

  UPDATE afi_sol_activacion
  SET    status_interno   = 40,
         cod_operacion    = vcod_operacion,
         fecha_activacion = HOY
  WHERE  curp             = vcurp   
  AND    cod_operacion    = 0
        
  INSERT INTO afi_rch_activacion
  VALUES (vnss, vcurp, vcod_operacion, vmot_rech1, vmot_rech2, vmot_rech3, 
          HOY, vfecha_envio, HOY, g_usuario)

END FUNCTION

FUNCTION actualiza_01()
#a01-------------------

  UPDATE afi_sol_activacion
  SET    status_interno   = 100,
         cod_operacion    = '01',
         fecha_activacion = NULL
  WHERE  curp             = vcurp   
  AND    cod_operacion    = 0
    
  -- Verificar si ya tiene su regimen aperturado correctamente.
  -- Enviar a historico de afiliados (No perder el NTI o NSS de la solicitud).
  -- Actualizar tipo de solicitud de 8 a 1 y NSS en todas las tablas. MAL
  -- Actualizar tipo de solicitud de 8 a 10 y NSS en todas las tablas. MMC
    
END FUNCTION

FUNCTION despliega_totales()
#dt-------------------------

  IF NOT bnd_proceso THEN
     DISPLAY "                  DATOS A PROCESAR                 "
     AT 8,1 ATTRIBUTE ( REVERSE )

     DISPLAY "Total de Registros del lote : ",
              vtotal USING "#######&" AT 9,15 ATTRIBUTE ( BOLD )

     DISPLAY "Registros actualizados      : ",
              vaprobados USING "#######&" AT 10,15 ATTRIBUTE ( BOLD )

     DISPLAY "Registros rechazados        : ",
              vrechazados USING "#######&" AT 11,15 ATTRIBUTE ( BOLD )

     DISPLAY "Registros pendientes        : ",
              vpendientes USING "#######&" AT 12,15 ATTRIBUTE ( BOLD )

     DISPLAY "Folio Liquidacion: ", vfolio USING "&&&&&&&&" 
     AT 14,15 ATTRIBUTE ( BOLD )
  ELSE
     DISPLAY "                  DATOS A PROCESAR                 "

     DISPLAY "Total de Registros del lote  : ",
              vtotal USING "#######&"

     DISPLAY "Registros actualizados       : ",
              vaprobados USING "#######&"

     DISPLAY "Registros rechazados         : ",
              vrechazados USING "#######&"

     DISPLAY "Registros pendientes         : ",
              vpendientes USING "#######&"
  END IF

END FUNCTION

FUNCTION lista_err()
#-------------------

  DEFINE hora          CHAR(8)
  DEFINE HOY           DATE
  DEFINE vcod_rechazo  CHAR(8)
  DEFINE vcont         INTEGER

  DEFINE gr_curp RECORD
    nss                CHAR(11),
    curp               LIKE afi_mae_afiliado.n_unico,
    paterno            CHAR(13),
    materno            CHAR(13),
    nombres            CHAR(16),
    mot_rech1          CHAR(03),
    mot_rech2          CHAR(03),
    mot_rech3          CHAR(03),
    des_rechazo        CHAR(49)
  END RECORD

  DEFINE G_LISTA       CHAR(200)
  DEFINE G_IMPRIME     CHAR(200)

  DEFINE resp          CHAR(1)

  LET hora = TIME
  LET HOY  = TODAY

  LET G_LISTA = g_paramgrales.ruta_listados CLIPPED,"/",
                g_usuario CLIPPED,".RECHAZOS_SOL_ACT.",
                HOY USING "dd-mm-yy","_",hora CLIPPED

  START REPORT listado TO G_LISTA

  DECLARE c_carga CURSOR FOR
  SELECT b.nss_solicitud,
         b.curp_solicitud,
         b.paterno,
         b.materno,
         b.nombres,
         b.motivo_rch1,
         b.motivo_rch2,
         b.motivo_rch3,
         a.rdeta_desc_c
  FROM   safre_af:tab_rdeta a,
         safre_tmp:det_sol_act b,
         safre_af:afi_mae_afiliado c
  WHERE  c.n_unico      = b.curp_solicitud
  AND    b.motivo_rch1  = a.rdeta_cod
  AND    b.cod_resul_op = "02"
  AND    a.modulo_cod   = 'afi'
  ORDER BY 2,1

  FOREACH c_carga INTO gr_curp.*
    OUTPUT TO REPORT listado(gr_curp.*)
  END FOREACH

  FINISH REPORT listado

  ERROR "LISTADO GENERADO" SLEEP 2

  LET G_LISTA = "chmod 777 ",g_paramgrales.ruta_listados CLIPPED,"/",
                 g_usuario CLIPPED, ".RECHAZOS_SOL_ACT.",
                 HOY USING "dd-mm-yy","_",hora CLIPPED
  RUN G_LISTA

  LET G_IMPRIME = "lp ",g_paramgrales.ruta_listados CLIPPED,"/",
                   g_usuario CLIPPED,".RECHAZOS_SOL_ACT.",
                   HOY USING "dd-mm-yy","_",hora CLIPPED
  RUN G_IMPRIME

END FUNCTION

REPORT listado(rpt)
#l-----------------

  DEFINE rpt RECORD
    nss                CHAR(11),
    curp               LIKE afi_mae_afiliado.n_unico,
    paterno            CHAR(13),
    materno            CHAR(13),
    nombres            CHAR(16),
    mot_rech1          CHAR(03),
    mot_rech2          CHAR(03),
    mot_rech3          CHAR(03),
    des_rechazo        CHAR(49)
  END RECORD

  DEFINE
    l_estado           CHAR(16),
    aux_sexo           CHAR(10),
    razon_social       CHAR(40),
    vcont              SMALLINT

  OUTPUT
    PAGE LENGTH   90
    TOP  MARGIN   0
    BOTTOM MARGIN 0
    LEFT MARGIN   0
    RIGHT MARGIN  0

  FORMAT
  PAGE HEADER

    PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
    PRINT
      COLUMN 001,"========================================"            ,
      COLUMN 040,"========================================"     ,
      COLUMN 080,"========================================"     ,
      COLUMN 120,"========================================"     ,
      COLUMN 160,"====="
    PRINT
      COLUMN 001,razon_social                                     ,
      COLUMN 140,"FECHA :",HOY USING "DD/MM/YYYY"

    PRINT
      COLUMN 001,"AFIC027"                                     ,
      COLUMN 035," E S T A D O   D E   A C T I V A C I O N  ",
                 "R E C H A Z A D O S   P O R   P R O C E S A R",
      COLUMN 140,"NUM.PAG.:",pageno USING "####,###"
      SKIP 1 LINE
    PRINT
      COLUMN 001,"----------------------------------------"     ,
      COLUMN 040,"----------------------------------------"     ,
      COLUMN 080,"----------------------------------------"     ,
      COLUMN 120,"----------------------------------------"     ,
      COLUMN 160,"-----"
      SKIP 1 LINES
    PRINT
      COLUMN 001, "    Curp",
      COLUMN 020, "NSS",
      COLUMN 038, "Ap. Paterno",
      COLUMN 054, "Ap. Materno",
      COLUMN 070, "Nombres",
      COLUMN 088, "Desc. Rechazo",
      COLUMN 148,        "Codigo Rechazo"
    PRINT
      COLUMN 001,"========================================" ,
      COLUMN 040,"========================================" ,
      COLUMN 080,"========================================" ,
      COLUMN 120,"========================================" ,
      COLUMN 160,"====="

  ON EVERY ROW
     LET vcont = vcont + 1

     PRINT
       COLUMN 001, rpt.curp,
       COLUMN 025, rpt.nss,
       COLUMN 038, rpt.paterno,
       COLUMN 054, rpt.materno,
       COLUMN 070, rpt.nombres,
       COLUMN 083, rpt.des_rechazo,
       COLUMN 157, rpt.mot_rech1

     AFTER GROUP OF rpt.mot_rech1
       SKIP 1 LINES
       PRINT
         COLUMN 145, "================"
       PRINT
         COLUMN 125, "TOTAL POR CODIGO DE RECHAZO     :", 
         GROUP COUNT(*) USING "#####"
         SKIP 2 LINES

END REPORT

FUNCTION despliega_archivos()
#da--------------------------

  DEFINE
    aux_pausa CHAR(1),
    HOY       DATE,
    SW_1      SMALLINT

  OPTIONS PROMPT LINE LAST,
    INPUT WRAP,
    ACCEPT KEY CONTROL-O

  WHENEVER ERROR STOP

  LET HOY = TODAY

  OPEN WINDOW window_1 AT 2,2 WITH FORM "AFIM0231" ATTRIBUTE(BORDER)
  DISPLAY " AFIC027                      CONSULTA REGISTROS PROCESADOS                    " AT 3,1
  ATTRIBUTE(REVERSE)
  DISPLAY HOY USING "dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)

  MENU "CONSULTA ARCHIVOS RESPUESTA PROCESADOS"
    COMMAND "Consulta" "Consultar Archivos PROCESAR"
      CALL Consulta()
    COMMAND "Salir" "Salir del Programa"
      EXIT MENU
  END MENU

  CLOSE WINDOW window_1

END FUNCTION

FUNCTION Consulta()
#C-----------------

  DEFINE ga_record ARRAY[3000] OF RECORD
    nombre_archivo LIKE afi_ctr_arh_proc.nombre_archivo,
    total_reg      LIKE afi_ctr_arh_proc.total_reg,
    total_aprob    LIKE afi_ctr_arh_proc.total_aprob,
    total_rech     LIKE afi_ctr_arh_proc.total_rech,
    total_dup      LIKE afi_ctr_arh_proc.total_pend,
    fecha_proceso  LIKE afi_ctr_arh_proc.fecha_proceso
  END RECORD

  DEFINE
    vcount,
    vtotal,
    vaprobados,
    vrechazos,
    vduplicados,
    vtot_curp      INTEGER

  DEFINE
    pos            SMALLINT

  SELECT COUNT(*)
  INTO   vcount
  FROM   afi_ctr_arh_reg

  SELECT SUM(@total_reg)
  INTO   vtotal
  FROM   afi_ctr_arh_reg

  SELECT SUM(@total_aprob)
  INTO   vaprobados
  FROM   afi_ctr_arh_reg

  SELECT SUM(@total_rech)
  INTO   vrechazos
  FROM   afi_ctr_arh_reg

  SELECT SUM(@total_pend)
  INTO   vduplicados
  FROM   afi_ctr_arh_reg

  DISPLAY "                                                                                    " AT 1,1
  DISPLAY "  CTRL-C cancela                                                                    " AT 2,1
  DISPLAY " CONSULTA " AT 2,65

  DECLARE curp_12 CURSOR FOR
  SELECT @nombre_archivo, @total_reg, @total_aprob, @total_rech, @total_pend,
         @fecha_proceso
  FROM afi_ctr_arh_reg
  ORDER BY 6 DESC

  LET pos = 1

  FOREACH curp_12 INTO ga_record[pos].*
    LET pos = pos + 1
  END FOREACH

  IF (pos-1) >= 1 THEN
     CALL  SET_COUNT(pos-1)

     DISPLAY BY NAME vcount
     DISPLAY BY NAME vtotal
     DISPLAY BY NAME vaprobados
     DISPLAY BY NAME vrechazos
     DISPLAY BY NAME vduplicados

     DISPLAY ARRAY ga_record TO scr_1.*

     ON KEY (INTERRUPT)
        CLEAR FORM
        EXIT DISPLAY

     END DISPLAY
  ELSE
     ERROR "ARCHIVO DE PROCESAR VACIO"
  END IF

END FUNCTION
#######################################################################
FUNCTION actualiza_operacion()
#ao---------------------------

  UPDATE bat_ctr_operacion
  SET    estado_operacion = 4,
         fecha_fin        = CURRENT,
         nombre_archivo   = nom_afi
  WHERE  pid              = reg_bat.pid
  AND    proceso_cod      = reg_bat.proceso_cod
  AND    opera_cod        = reg_bat.opera_cod

  UPDATE bat_ctr_proceso
  SET    estado_proceso   = 4,
         fecha_fin        = CURRENT
  WHERE  pid              = reg_bat.pid
  AND    proceso_cod      = reg_bat.proceso_cod

END FUNCTION

#################################################################
FUNCTION det_carta(cdocto_cod, cnss, cn_folio) #dc
  DEFINE cdocto_cod SMALLINT
  DEFINE cnss       CHAR(11)
  DEFINE cn_folio          DECIMAL(10,0)

  LET reg_carta.docto_cod      = cdocto_cod
  LET reg_carta.nss            = cnss 
  LET reg_carta.n_folio        = cn_folio
  LET reg_carta.tipo_solicitud = 10
  LET reg_carta.fecha_registro = TODAY
  LET reg_carta.opera_cod      = NULL
  LET reg_carta.edo_genera     = 10
  LET reg_carta.fecha_genera   = TODAY
  LET reg_carta.hora_genera    = TIME
  LET reg_carta.lote_genera    = 0
  LET reg_carta.consecutivo    = 0
  LET reg_carta.id_sepomex     = 0

  LET consulta_carta = "EXECUTE PROCEDURE inserta_carta (?,?,?,?,?,?,",
                                                        "?,?,?,?,?,?)"
  PREPARE exe_sql FROM consulta_carta
  EXECUTE exe_sql USING reg_carta.*

  INITIALIZE reg_carta.* TO NULL

END FUNCTION
#################################################################

FUNCTION actualiza_nss()
#ac-----------------------

  DEFINE 
    enter               CHAR(1),
    generar             CHAR(1),
    aux_pausa           CHAR(1),
    opc                 CHAR(1),
    g_usuario           CHAR(8),
    HORA                CHAR(8),
    vnss                CHAR(11),
    operacion           CHAR(40),
    v_sql_1             CHAR(50),
    v_sql_2             CHAR(50),
    HOY                 DATE,
    f_ini_tmte          DATE 

  DEFINE w_aux  RECORD
    n_seguro            LIKE afi_solicitud.n_seguro,
    n_unico             LIKE afi_solicitud.n_unico ,
    n_rfc               LIKE afi_solicitud.n_rfc   ,
    paterno             LIKE afi_solicitud.paterno ,
    materno             LIKE afi_solicitud.materno ,
    nombres             LIKE afi_solicitud.nombres ,
    fena                LIKE afi_solicitud.fena    ,
    sexo                LIKE afi_solicitud.sexo    ,
    frecafor            LIKE afi_solicitud.frecafor,
    status_interno      SMALLINT
  END RECORD

  DEFINE g_afore        RECORD LIKE tab_afore_local.*
  DEFINE gr_ctanssreg   RECORD LIKE cta_nss_regimen.*

  DEFINE
    bnd_proceso         SMALLINT ,
    pestado_marca       SMALLINT ,
    pcodigo_rechazo     SMALLINT ,
    ejecuta             CHAR(300),
    xcodigo_marca       SMALLINT ,
    xcodigo_rechazo     SMALLINT ,
    pmarca_entra        SMALLINT,
    pmarca_causa        SMALLINT,
    pfecha_causa        SMALLINT,
    xmarca_esado        SMALLINT,
    edo_proc            SMALLINT

  DEFINE consulta_carta CHAR(120)

  DEFINE reg_carta      RECORD LIKE int_ctr_carta.*
  DEFINE mae            RECORD LIKE afi_mae_afiliado.*
  DEFINE act            RECORD LIKE afi_sol_activacion.*

  DEFINE 
    dom                 RECORD LIKE afi_domicilio.*,
    cor                 RECORD LIKE afi_correo_elect.*,
    tel                 RECORD LIKE afi_telefono.*,
    pat                 RECORD LIKE afi_patron.* ,
    ben                 RECORD LIKE afi_mae_benefici.*,
    ide                 RECORD LIKE afi_ctr_identif.*,
    obs                 RECORD LIKE afi_ctr_observa.*,
    cta                 RECORD LIKE cta_ctr_cuenta.*

  DEFINE 
    mensaje             CHAR(050),
    G_LISTA             CHAR(300)

  DEFINE 
    i                   ,
    cont                ,
    HAY                 ,
    v_porc              SMALLINT,
    v_existe            ,
    v_edad              ,
    v_criterio          ,
    v_ind_edad          SMALLINT,        
    v_crea_fecha        DATE,        
    v_tipo_proc         ,
    v_tipo_trasp        ,
    v_medio             ,
    v_cve_siefore       ,
    v_cve_sief_i        ,
    v_cve_sief_f        ,
    v_rechazo           SMALLINT,
    v_folioatencion     INTEGER

  DEFINE 
    v_curp              CHAR(18),
    v_rfc               CHAR(13),
    v_fena              DATE

  DEFINE regrowid       RECORD 
    v_rowid             DECIMAL(10,0)
  END RECORD  

  DEFINE v_afisolreg    RECORD
    nss                 LIKE afi_solicitud_regimen.nss,
    fol                 LIKE afi_solicitud_regimen.n_folio,
    ts                  LIKE afi_solicitud_regimen.tipo_solicitud,
    edo                 LIKE afi_solicitud_regimen.estado
  END RECORD

  DEFINE
    ant_nss             CHAR(11),
    ant_tipo_sol        SMALLINT

  WHENEVER ERROR CONTINUE
  DATABASE safre_tmp
    DROP TABLE safre_tmp:afi_ind
  WHENEVER ERROR STOP

  CREATE TABLE safre_tmp:afi_ind
    (n_seguro          CHAR(11) ,
     n_unico           CHAR(18) ,
     n_rfc             CHAR(13) ,
     paterno           CHAR(40) ,
     materno           CHAR(40) ,
     nombres           CHAR(40) ,
     fena              DATE     ,
     sexo              SMALLINT ,
     frecafor          DATE     ,
     status_interno    SMALLINT);

  DATABASE safre_af

  SELECT *, USER
  INTO   g_afore.*, g_usuario
  FROM   tab_afore_local

  LET HOY       = TODAY
  LET HORA      = TIME

  LET operacion = 'ACTIVACION NO AFILIADOS'

  INITIALIZE reg_carta.*    TO NULL  
  INITIALIZE gr_ctanssreg.* TO NULL

  LET v_sql_1 = "EXECUTE FUNCTION fn_fnacimiento(?,?)"
  LET v_sql_2 = "EXECUTE FUNCTION fn_regimen_inv(?,?,?,?,?,?)"

  LET v_tipo_trasp = 5
  LET v_tipo_proc  = 1
  LET v_medio      = 10

  PREPARE stmt1 FROM v_sql_1
  PREPARE stmt2 FROM v_sql_2

  LET mensaje = "NO SE HICIERON TRASPASOS AL MAESTRO DE AFILIADOS"

  DECLARE cursor_a CURSOR FOR 
  SELECT rowid, A.* 
  FROM   afi_sol_activacion A
  WHERE  A.status_interno   = 100
  AND    A.cod_operacion    = '01'
  AND    A.fecha_activacion IS NULL
  ORDER  BY nss

  FOREACH cursor_a INTO regrowid.v_rowid, act.*

    LET HAY = FALSE

    SELECT COUNT(*)
    INTO   HAY
    FROM   afi_mae_afiliado m
    WHERE  m.n_unico        = act.curp
    AND    m.tipo_solicitud = 8

    IF HAY THEN
       SELECT *
       INTO   mae.*
       FROM   afi_mae_afiliado ma
       WHERE  ma.n_unico        = act.curp
       AND    ma.tipo_solicitud = 8

       LET ant_nss      = mae.n_seguro
       LET ant_tipo_sol = mae.tipo_solicitud
       LET con_curp     = con_curp + 1

       IF SQLCA.SQLCODE = 0 THEN
          INSERT INTO afi_his_afiliado VALUES (mae.*)

          IF SQLCA.SQLCODE = 0 THEN
             DELETE
             FROM   afi_mae_afiliado
             WHERE  n_unico        = act.curp
             AND    tipo_solicitud = 8
             AND    n_folio        = mae.n_folio
          END IF

          SELECT b.*
          INTO   cta.*
          FROM   cta_ctr_cuenta b
          WHERE  b.nss = mae.n_seguro

          IF cta.nss THEN
             INSERT INTO cta_his_cuenta VALUES (cta.*)
          END IF

          LET HAY = FALSE
       END IF
    END IF

    IF NOT HAY THEN
       LET mae.n_seguro       = act.nss
       LET mae.tipo_solicitud = 10

       INSERT INTO afi_mae_afiliado VALUES(mae.*)

       INSERT INTO afi_solicitud VALUES(mae.*)

       IF SQLCA.SQLCODE <> 0 THEN
          INSERT INTO safre_tmp:nss_dup VALUES (act.nss)
       END IF

       ----- Domicilio
       DECLARE c_dom CURSOR FOR
       SELECT *
       FROM   afi_domicilio d
       WHERE  d.nss             = ant_nss
       AND    d.n_folio         = mae.n_folio
       AND    d.tipo_solicitud  = 8
       FOREACH c_dom INTO dom.*
         LET dom.nss            = mae.n_seguro 
         LET dom.tipo_solicitud = 10
         INSERT INTO afi_domicilio VALUES(dom.*)
       END FOREACH

       DELETE
       FROM   afi_domicilio
       WHERE  nss            = ant_nss
       AND    n_folio        = mae.n_folio
       AND    tipo_solicitud = 8

       ----- Correo Electronico
       DECLARE c_cor CURSOR FOR
       SELECT *
       FROM   afi_correo_elect c
       WHERE  c.nss             = ant_nss
       AND    c.n_folio         = mae.n_folio
       AND    c.tipo_solicitud  = 8
       FOREACH c_cor INTO cor.*
         LET cor.nss            = mae.n_seguro
         LET cor.tipo_solicitud = 10
         INSERT INTO afi_correo_elect VALUES(cor.*)
       END FOREACH

       DELETE
       FROM   afi_correo_elect
       WHERE  nss            = ant_nss
       AND    n_folio        = mae.n_folio
       AND    tipo_solicitud = 8

       ----- Telefono
       DECLARE c_tel CURSOR FOR
       SELECT *
       FROM   afi_telefono f
       WHERE  f.nss             = ant_nss
       AND    f.n_folio         = mae.n_folio
       AND    f.tipo_solicitud  = 8
       FOREACH c_tel INTO tel.*
         LET tel.nss            = mae.n_seguro
         LET tel.tipo_solicitud = 10
         INSERT INTO afi_telefono VALUES(tel.*)
       END FOREACH

       DELETE
       FROM   afi_telefono
       WHERE  nss            = ant_nss
       AND    n_folio        = mae.n_folio
       AND    tipo_solicitud = 8

       ----- Patron
       DECLARE c_pat CURSOR FOR
       SELECT *
       FROM   afi_patron p
       WHERE  p.n_seguro        = ant_nss
       AND    p.n_folio         = mae.n_folio
       AND    p.tipo_solicitud  = 8
       FOREACH c_pat INTO pat.*
         LET pat.n_seguro       = mae.n_seguro
         LET pat.tipo_solicitud = 10
         INSERT INTO afi_patron VALUES(pat.*)
       END FOREACH

       DELETE
       FROM   afi_patron
       WHERE  n_seguro       = ant_nss
       AND    n_folio        = mae.n_folio
       AND    tipo_solicitud = 8

       ----- Beneficiario
       DECLARE c_ben CURSOR FOR
       SELECT *
       FROM   afi_mae_benefici b
       WHERE  b.n_seguro        = ant_nss
       AND    b.n_folio         = mae.n_folio
       AND    b.tipo_solicitud  = 8
       FOREACH c_ben INTO ben.*
         LET ben.n_seguro       = mae.n_seguro
         LET ben.tipo_solicitud = 10
         INSERT INTO afi_mae_benefici VALUES(ben.*)
       END FOREACH

       DELETE
       FROM   afi_mae_benefici
       WHERE  n_seguro       = ant_nss
       AND    n_folio        = mae.n_folio
       AND    tipo_solicitud = 8

       ----- Identificacion
       DECLARE c_ide CURSOR FOR
       SELECT *
       FROM   afi_ctr_identif id
       WHERE  id.n_seguro       = ant_nss
       AND    id.n_folio        = mae.n_folio
       AND    id.tipo_solicitud = 8
       FOREACH c_ben INTO ide.*
         LET ide.n_seguro       = mae.n_seguro
         LET ide.tipo_solicitud = 10
         INSERT INTO afi_ctr_identif VALUES(ide.*)
       END FOREACH

       DELETE
       FROM   afi_ctr_identif
       WHERE  n_seguro       = ant_nss
       AND    n_folio        = mae.n_folio
       AND    tipo_solicitud = 8

       ----- Observaciones
       DECLARE c_obs CURSOR FOR
       SELECT *
       FROM   afi_ctr_observa ob
       WHERE  ob.nss            = ant_nss
       AND    ob.n_folio        = mae.n_folio
       AND    ob.tipo_solicitud = 8
       FOREACH c_ben INTO obs.*
         LET obs.nss            = mae.n_seguro
         LET obs.tipo_solicitud = 10
         INSERT INTO afi_ctr_observa VALUES(obs.*)
       END FOREACH

       DELETE
       FROM   afi_ctr_observa
       WHERE  nss            = ant_nss
       AND    n_folio        = mae.n_folio
       AND    tipo_solicitud = 8

-- 1076094
-- SALDOS CARGO - ABONO   

       SELECT "X"
       FROM   cta_ctr_cuenta
       WHERE  cta_ctr_cuenta.nss = act.nss

       IF SQLCA.SQLCODE = 0 THEN
          UPDATE cta_ctr_cuenta
          SET    fecha_pri_rcv      = NULL,
                 fecha_ult_rcv      = NULL,
                 fecha_pri_general  = NULL,
                 fecha_ult_general  = NULL,
                 fecha_vol_pat      = NULL,
                 fecha_vol_ven      = NULL,
                 ind_actividad      = 1,
                 fecha_actividad    = HOY,
                 ind_edad           = 0,
                 fecha_edad         = HOY,
                 criterio_edad      = 0,
                 ind_transferencia  = 0,
                 fecha_ind_transf   = HOY,
                 ind_saldo_cero     = 0,
                 fecha_saldo_cero   = NULL,
                 estado_impresion   = 0,
                 periodo_ult_aporte = NULL,
                 dias_cotizados     = 0,
                 ult_sal_integrado  = 0,
                 tipo_informe       = 0,
                 fecha_informe      = NULL,
                 fecha_registro     = HOY,
                 usuario            = g_usuario
          WHERE  nss                = act.nss
       ELSE
          INSERT INTO cta_ctr_cuenta  #------ Control cuenta
          VALUES (act.nss,            #nss
                  "",                 #fecha_pri_rcv
                  "",                 #fecha_ult_rcv
                  "01/01/0001",       #fecha_pri_general
                  "",                 #fecha_ult_general
                  "",                 #fecha_vol_pat
                  "",                 #fecha_vol_ven
                  0,                  #ind_saldo_cero
                  "",                 #fecha_saldo_cero
                  1,                  #ind_actividad
                  HOY,                #fecha_actividad
                  0,                  #ind_edad
                  HOY,                #fecha_edad
                  0,                  #criterio_edad
                  0,                  #ind_transferencia
                  HOY,                #fecha_ind_transf
                  0,                  #estado_impresion,
                  "",                 #periodo_ult_aporte
                  0,                  #dias_cotizados
                  0,                  #ult_sal_integrado
                  0,                  #tipo_informe
                  "",                 #fecha_informe
                  HOY,                #fecha_registro
                  g_usuario           #usuario
                  )
       END IF

       LET v_crea_fecha = HOY

       DECLARE curs1 CURSOR FOR stmt1
       OPEN  curs1 USING act.nss, v_crea_fecha
       FETCH curs1 INTO v_existe, v_edad, v_criterio, v_ind_edad,
                        v_curp, v_rfc, v_fena
       CLOSE curs1

       DECLARE curs2 CURSOR FOR stmt2
       OPEN curs2 USING act.nss,
	                v_ind_edad,
	                v_ind_edad,
	                v_tipo_proc,
	                v_tipo_trasp,
	                v_medio
       FETCH curs2 INTO v_existe, v_ind_edad, v_rechazo, v_folioatencion
       CLOSE curs2
       
       IF v_rechazo <> 0 THEN
          INSERT INTO safre_tmp:rch_apertura
          VALUES (act.nss,v_rechazo)
       END IF

       UPDATE afi_sol_activacion 
       SET    fecha_activacion = TODAY
       WHERE  curp             = act.curp 
       AND    cod_operacion    = '01'
       AND    status_interno   = 100
       AND    fecha_activacion IS NULL

    END IF

    LET HORA = TIME

    INSERT INTO afi_ctr_logico
    VALUES (mae.n_folio,
            mae.tipo_solicitud,
            act.nss,
            mae.status_interno,
            g_usuario,
            HOY,
            HORA,
            operacion)

    INSERT INTO safre_tmp:afi_ind
    VALUES (act.nss            ,
            act.curp           ,
            mae.n_rfc          ,
            mae.paterno        ,
            mae.materno        ,
            mae.nombres        ,
            mae.fena           ,
            mae.sexo           ,
            mae.frecafor       ,
            mae.status_interno
           )

    --- PROCESO DE LIQUIDACION (CARGO ABONO) A CUENTA NUEVA

    LET reg_cta.nss              = ant_nss
    LET reg_cta.curp             = act.curp

    LET reg_nva.nss              = act.nss
    LET reg_nva.curp             = act.curp
    
    #### Liquidacion multisiefores ####
    CALL liquidacion()
    #### Liquidacion multisiefores ####

    #### Liquidacion vieja ####
    {DECLARE cur_nti CURSOR FOR
    SELECT c.subcuenta, 
           c.siefore,
           sum(c.monto_en_acciones), 
           sum(c.monto_en_pesos)
    FROM   dis_cuenta c
    WHERE  c.nss = ant_nss
    GROUP BY 1,2

    FOREACH cur_nti INTO reg_cta.subcuenta, 
                         reg_cta.siefore,
                         reg_cta.monto_en_acciones,
                         reg_cta.monto_en_pesos

        IF reg_cta.subcuenta <> 14 THEN
           IF reg_cta.subcuenta = 4 OR
              reg_cta.subcuenta = 8 THEN
              LET reg_cta.fecha_valor = vfecha_viv  
              LET reg_cta.monto_en_pesos = reg_cta.monto_en_acciones * vvalor11
           ELSE
              IF reg_cta.siefore = 1 THEN
                  LET reg_cta.monto_en_pesos = reg_cta.monto_en_acciones * 
                                               vvalor1
                  LET reg_cta.precio_accion  = vvalor1
              ELSE 
                  LET reg_cta.monto_en_pesos = reg_cta.monto_en_acciones * 
                                               vvalor2
                  LET reg_cta.precio_accion  = vvalor2
              END IF
           END IF

           LET reg_cta.monto_en_acciones = reg_cta.monto_en_acciones * (-1)
        ELSE 
           LET reg_cta.monto_en_acciones = 0
           LET reg_cta.siefore           = 0
           LET reg_cta.fecha_valor       = vfecha_viv  
           LET reg_cta.precio_accion     = 0
        END IF

        LET reg_nva.monto_en_pesos = reg_cta.monto_en_pesos
        LET reg_cta.monto_en_pesos = reg_cta.monto_en_pesos * (-1)

        LET reg_nva.subcuenta      = reg_cta.subcuenta
        LET reg_nva.monto_en_pesos = reg_cta.monto_en_pesos

        IF reg_nva.subcuenta <> 14 THEN
           IF reg_nva.subcuenta = 4 OR
              reg_nva.subcuenta = 8 THEN
              LET reg_nva.fecha_valor = vfecha_viv  
              LET reg_nva.monto_en_pesos = reg_nva.monto_en_acciones * vvalor11
           ELSE
              SELECT cr.codigo_siefore
              INTO   reg_nva.siefore
              FROM   cta_regimen cr 
              WHERE  cr.nss       = act.nss
              AND    cr.subcuenta = reg_nva.subcuenta

              IF reg_nva.siefore = 1 THEN
                  LET reg_nva.monto_en_acciones = reg_nva.monto_en_pesos / 
                                                  vvalor1
                  LET reg_nva.precio_accion     = vvalor1
              ELSE 
                  LET reg_nva.monto_en_acciones = reg_nva.monto_en_pesos / 
                                                  vvalor2
                  LET reg_nva.precio_accion     = vvalor2
              END IF
           END IF
        ELSE 
           LET reg_nva.monto_en_acciones = 0
           LET reg_nva.siefore           = 0
           LET reg_nva.fecha_valor       = vfecha_viv  
           LET reg_nva.precio_accion     = 0
        END IF

        INSERT INTO dis_cuenta VALUES(reg_cta.*)
        INSERT INTO dis_cuenta VALUES(reg_nva.*)

        LET reg_cta.monto_en_acciones = 0
        LET reg_cta.monto_en_pesos    = 0
        LET reg_nva.monto_en_acciones = 0
        LET reg_nva.monto_en_pesos    = 0
        LET reg_cta.precio_accion     = 0
        LET reg_nva.precio_accion     = 0
    END FOREACH}
    #### Liquidacion vieja ####

    {IF (act.finitmte IS NULL) OR 
        (act.finitmte = '')    THEN}
        LET reg_carta.docto_cod = 30501 
        CALL det_carta(30501, act.nss, mae.n_folio) #dc
    {ELSE
        LET reg_carta.docto_cod = 30502
        CALL det_carta(30502, act.nss, mae.n_folio) #dc
    END IF}

    SELECT "X"
    FROM   safre_af:rec_solicitud mr
    WHERE  mr.n_seguro   = act.nss
    AND    mr.origen_rec <> 1
    GROUP BY 1

    IF SQLCA.SQLCODE = 0 THEN
       UPDATE safre_af:rec_solicitud
       SET    safre_af:rec_solicitud.origen_rec = 1
       WHERE  safre_af:rec_solicitud.n_seguro   = act.nss
       AND    safre_af:rec_solicitud.origen_rec <> 1
    END IF      
  END FOREACH

  SELECT "X"
  FROM   safre_tmp:afi_ind
  GROUP BY 1

  LET G_LISTA = g_paramgrales.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                ".ENVIA_ACTIVA_MAESTRO." CLIPPED,
                HOY USING "dd-mm-yy","_",HORA CLIPPED

  IF STATUS = NOTFOUND THEN 
     START REPORT listado_1 TO G_LISTA
       OUTPUT TO REPORT listado_1(mensaje)
     FINISH REPORT listado_1
  ELSE
     DECLARE cur_1 CURSOR FOR
     SELECT *
     FROM   safre_tmp:afi_ind

     START REPORT listado_2 TO G_LISTA
       FOREACH cur_1 INTO w_aux.*
         OUTPUT TO REPORT listado_2(w_aux.*)
       END FOREACH
     FINISH REPORT listado_2
  END IF 

  LET G_LISTA = "chmod 777 ",g_paramgrales.ruta_listados CLIPPED,
		"/",g_usuario CLIPPED,".ENVIA_ACTIVA_MAESTRO." CLIPPED,
                HOY USING "dd-mm-yy","_",HORA CLIPPED
  RUN G_LISTA

  CALL despliega_apertura()

  PROMPT "Proceso finalizado, [Enter] para salir" FOR enter

END FUNCTION

FUNCTION despliega_apertura()
#dr--------------------------
  DEFINE total_resp SMALLINT

  LET total_resp = con_curp + sin_curp

  IF bnd_proceso THEN
     DISPLAY "          NTI INCORPORADOS AL MAESTRO DE AFILIADOS           "
     DISPLAY "Total de Registros Incorporados : ", total_resp USING "#####&"

     DISPLAY "Registros con curp              : ", con_curp   USING "#####&"

     DISPLAY "Registros sin curp              : ", sin_curp   USING "#####&"
  ELSE
     DISPLAY "          NTI INCORPORADOS AL MAESTRO DE AFILIADOS           "
     AT 10,1 ATTRIBUTE(REVERSE)

     DISPLAY "Tot Reg Incorporados: ", total_resp USING "#####&"
     AT 11,45

     DISPLAY "Registros con curp  : ", con_curp   USING "#####&"
     AT 12,45

     DISPLAY "Registros sin curp  : ", sin_curp   USING "#####&"
     AT 13,45

     DISPLAY "                                                                               " 
     AT 14,45
     DISPLAY "FOLIO LIQUIDACION: ", vfolio USING "&&&&&&&&"
     AT 15,45
     DISPLAY "                                                                               " 
     AT 16,45

  END IF

END FUNCTION

REPORT listado_1(mensaje)
#------------------------

  DEFINE
    mensaje             CHAR(50)

  OUTPUT
    LEFT MARGIN   0
    RIGHT MARGIN  0
    TOP MARGIN    0
    BOTTOM MARGIN 0

  FORMAT
  PAGE HEADER
    PRINT
      COLUMN 03,"========================================",
      COLUMN 40,"==================================="
    PRINT
      COLUMN 20," TRASPASO NO AFILIADOS AL MAESTRO DE AFILIADOS ACTIVACION"
    PRINT
      COLUMN 03,"----------------------------------------",
      COLUMN 40,"-----------------------------------"
    PRINT
      COLUMN 01,"N S S  "  ,
      COLUMN 13,"CURP   "  ,
      COLUMN 33,"R.F.C. "  ,
      COLUMN 48,"Paterno"  ,
      COLUMN 68,"Materno"  ,
      COLUMN 88,"Nombres"       
    PRINT
      COLUMN 05,"Fecha Nac."     ,
      COLUMN 17,"Sexo"           ,
      COLUMN 28,"Fecha Frecafor" ,
      COLUMN 43,"Edo. Afiliado" 
    PRINT
      COLUMN 03,"========================================",
      COLUMN 40,"==================================="
    PRINT 
    PRINT 
    PRINT 
      COLUMN 15,mensaje

END REPORT

REPORT listado_2(w_aux)
#----------------------

  DEFINE w_aux  RECORD
    n_seguro            LIKE afi_solicitud.n_seguro,
    n_unico             LIKE afi_solicitud.n_unico ,
    n_rfc               LIKE afi_solicitud.n_rfc   ,
    paterno             LIKE afi_solicitud.paterno ,
    materno             LIKE afi_solicitud.materno ,
    nombres             LIKE afi_solicitud.nombres ,
    fena                LIKE afi_solicitud.fena    ,
    sexo                LIKE afi_solicitud.sexo    ,
    frecafor            LIKE afi_solicitud.frecafor,
    status_interno      SMALLINT
  END RECORD

  DEFINE 
    l_estado            CHAR(16) ,
    aux_sexo            CHAR(10)

  DEFINE 
    cont                INTEGER

  OUTPUT
    LEFT MARGIN   0
    RIGHT MARGIN  0
    TOP MARGIN    0
    BOTTOM MARGIN 0

  FORMAT
    PAGE HEADER
      PRINT
        COLUMN 03,"========================================",
        COLUMN 40,"==================================="
      PRINT
        COLUMN 20," TRASPASO NO AFILIADOS AL MAESTRO DE AFILIADOS LIB PEND"
      PRINT
        COLUMN 03,"----------------------------------------",
        COLUMN 40,"-----------------------------------"
      PRINT
        COLUMN 01,"N S S  "       ,
        COLUMN 13,"CURP   "       ,
        COLUMN 33,"R.F.C. "       ,
        COLUMN 48,"Paterno"       ,
        COLUMN 68,"Materno"       ,
        COLUMN 88,"Nombres"       
      PRINT
        COLUMN 05,"Fecha Nac."    ,
        COLUMN 17,"Sexo"          ,
        COLUMN 28,"Fecha Frecafor",
        COLUMN 43,"Edo. Afiliado" 
      PRINT
        COLUMN 03,"========================================",
        COLUMN 40,"==================================="

    ON EVERY ROW
       IF w_aux.n_unico IS NULL OR w_aux.n_unico = " " THEN
	  LET l_estado = NULL
       END IF
       CASE w_aux.status_interno
	 WHEN   0 LET l_estado = "CAPTURADO"
	 WHEN  10 LET l_estado = "IMCOMPLETO"
	 WHEN  20 LET l_estado = "COMPLETO"
	 WHEN  30 LET l_estado = "ENIVIADO"
	 WHEN  40 LET l_estado = "RECHAZADO"
	 WHEN  50 LET l_estado = "PENDIENTE"
	 WHEN  55 LET l_estado = "ACLARACION"
	 WHEN  60 LET l_estado = "APROBADO"
	 WHEN 100 LET l_estado = "REGISTRADO"
       END CASE

       SELECT sexo_desc 
       INTO   aux_sexo 
       FROM   tab_sexo
       WHERE  sexo_cod = w_aux.sexo

       PRINT
         COLUMN 01,w_aux.n_seguro                   ,
         COLUMN 13,w_aux.n_unico                    ,
         COLUMN 33,w_aux.n_rfc                      ,
         COLUMN 48,w_aux.paterno CLIPPED            ,
         COLUMN 68,w_aux.materno CLIPPED            ,
         COLUMN 88,w_aux.nombres CLIPPED
       PRINT
         COLUMN 05,w_aux.fena  USING "dd-mm-yyyy"   ,
         COLUMN 17,aux_sexo    ,
         COLUMN 28,w_aux.frecafor USING "dd-mm-yyyy",
         COLUMN 43,l_estado CLIPPED
    
    ON LAST ROW
       SELECT COUNT(*)
       INTO   cont
       FROM   safre_tmp:afi_ind

       PRINT
       PRINT
       PRINT
         COLUMN 03,"----------------------------------------",
         COLUMN 40,"-----------------------------------"

       PRINT
       PRINT
         COLUMN 03,"NUMERO TOTAL DE REGISTROS ---> ",cont
END REPORT

#############################################
FUNCTION liquidacion()

   DEFINE vcontador        INTEGER,
          xsubcuenta       SMALLINT,
          xsiefore         SMALLINT,
          xacciones        DECIMAL(16,6),
          xpesos           DECIMAL(16,6)

   DEFINE cam_siefore_rcv  SMALLINT,
          inf_siefore_rcv  SMALLINT,
          x_tipo_traspaso  SMALLINT,
          x_medio          CHAR(2),
          x_existe         SMALLINT,
          x_rechazo        SMALLINT,
          x_folio          INTEGER

   DEFINE flag             SMALLINT,
          ind_siefore      SMALLINT,
          ind_decimos_nss  SMALLINT,
          ind_decimos_cta1 SMALLINT

   DEFINE xregimen         SMALLINT

   #### Liquidacion multisiefores ####
   LET fecha_liquida     = habil_siguiente(vfecha1)
  
   LET vsaldo            = " EXECUTE FUNCTION fn_saldo_dia (?,?,?,?) "
   PREPARE saldo_dia     FROM vsaldo
  
   LET vprov_cargo       = " EXECUTE FUNCTION fn_prov_cargo (?,?,?,?,?,?,?,?,?,?,?) "
   PREPARE cargo         FROM vprov_cargo
  
   LET vprov_abono       = " EXECUTE FUNCTION fn_prov_abono_sie (?,?,?,?,?,?,?,?,?,?,?) "
   PREPARE abono         FROM vprov_abono
  
   LET indebidas         = " EXECUTE FUNCTION fn_sol_transf_sie (?,?,?,?,?,?) "
   PREPARE vindebidas    FROM indebidas
  
   LET vtransferencia    = " EXECUTE FUNCTION fn_ind_transferencia(?,?,?) "
   PREPARE transferencia FROM vtransferencia
   #### Liquidacion multisiefores ####

   LET dia                 = DAY(fecha_liquida)
   LET vfecha_final        = fecha_liquida - dia UNITS DAY
   LET dia1                = DAY(vfecha_final) - 1
   LET vfecha_inicio       = vfecha_final  - dia1 UNITS DAY
   LET fecha_liquida_viv   = fecha_liquida - dia UNITS DAY + 1 UNITS DAY
   LET vfecha_inicio1      = vfecha_inicio + 1 UNITS DAY

   LET x_tipo_traspaso     = 4
   LET x_medio             = "10"

   LET ind_decimos_nss     = 0
   LET ind_decimos_cta1    = 0

   LET vcontador           = ""
   LET flag                = 0
   LET ind_siefore         = 1

   DECLARE s_saldo CURSOR FOR saldo_dia
   FOREACH s_saldo USING reg_cta.nss,
                         vsubcuenta,
                         vgrupo,
                         HOY
                   INTO  xsubcuenta,
                         xsiefore,
                         xacciones,
                         xpesos

     SELECT "X"                             
     FROM   cta_regimen
     WHERE  nss            = reg_nva.nss    ---nss activado
     AND    codigo_siefore = xsiefore       ---siefore_no_afiliado
     AND    subcuenta      = xsubcuenta     ---subcuenta_no_afiliado
     GROUP BY 1
            
     IF SQLCA.SQLCODE = 0 THEN             
        LET xregimen = 0
     ELSE
        LET xregimen = 1
 
        LET inf_siefore_rcv = xsiefore

        SELECT codigo_siefore
        INTO   cam_siefore_rcv
        FROM   cta_regimen
        WHERE  nss            =  reg_nva.nss 
        AND    codigo_siefore <> xsiefore
        AND    subcuenta      =  xsubcuenta
        GROUP BY 1
     END IF

     IF xacciones > 0 THEN
        CALL liquida_cuenta(reg_cta.nss,
                            xsubcuenta,
                            xsiefore,
                            xacciones,
                            xpesos,
                            0,
                            0,
                            0)
                            
                            {reg_3.cve_ent_cta1, --- afore cedente
                            reg_3.estado,
                            reg_3.consecutivo1)}

        CALL integra_cuenta(reg_nva.nss,
                            xsubcuenta,
                            xsiefore,
                            xacciones,
                            xpesos,
                            0,
                            0,
                            0)
                            
                            {reg_3.cve_ent_cta1, --- afore cedente
                            reg_3.estado,
                            vcontador)}

        IF xsubcuenta = 4 OR xsubcuenta = 8 THEN 
           LET flag = 0 
        ELSE
           LET flag = 1 
        END IF

        IF xsiefore = 2 THEN
           LET ind_siefore = 1
        END IF
     ELSE
        IF xsubcuenta = 14 AND xpesos > 0 THEN
           CALL liquida_cuenta(reg_cta.nss,
                               xsubcuenta,
                               xsiefore,
                               xacciones,
                               xpesos,
                               0,
                               0,
                               0)
                               
                               {reg_3.cve_ent_cta1, --- afore cedente
                               reg_3.estado,
                               reg_3.consecutivo1)}

           CALL integra_cuenta(reg_nva.nss,
                               xsubcuenta,
                               xsiefore,
                               xacciones,
                               xpesos,
                               0,
                               0,
                               0)
                               
                               {reg_3.cve_ent_cta1, --- afore cedente
                               reg_3.estado,
                               vcontador)}
        END IF
     END IF

     IF flag = 1 AND xregimen = 1 THEN

        SELECT "X"
        FROM   tes_solicitud
        WHERE  nss             = reg_nva.nss
        AND    folio_solicitud = vfolio
        AND    estado          = 100
        GROUP BY 1

        IF SQLCA.SQLCODE <> 0 THEN
           DECLARE s_indebidas CURSOR FOR vindebidas
           OPEN    s_indebidas USING reg_nva.nss,     ---nss
                                     inf_siefore_rcv, --siefore_ced
                                     cam_siefore_rcv, --siefore_rec
                                     x_tipo_traspaso,
                                     vfolio,          --folio liquidacion
                                     x_medio
           FETCH s_indebidas INTO    x_existe,
                                     x_rechazo,
                                     x_folio
           CLOSE s_indebidas 
        END IF                                      
     END IF

     LET  xsubcuenta = 0
     LET  xsiefore   = 0
     LET  xacciones  = 0
     LET  xpesos     = 0
     
   END FOREACH

   CALL ind_transferencia(reg_cta.nss)
            
   CALL registra_liq()
   
END FUNCTION
#####################################################################
FUNCTION liquida_cuenta(reg_4)
  DEFINE reg_4 RECORD
         nss               CHAR(11),
         subcuenta         INTEGER,
         siefore           SMALLINT,
         acciones          DECIMAL(16,6),
         pesos             DECIMAL(16,6),
         cedente           CHAR(03),
         estado            SMALLINT,
         consecutivo       INTEGER
  END RECORD

  DEFINE xcargo            SMALLINT
  DEFINE xliq              SMALLINT

  DEFINE opc               CHAR(1)

  LET reg_4.pesos    = reg_4.pesos    * -1
  LET reg_4.acciones = reg_4.acciones * -1

  IF  reg_4.subcuenta = 14 THEN
      LET  reg_4.acciones    = 0
  END IF

  IF (reg_4.subcuenta = 3 OR reg_4.subcuenta = 10) THEN
     UPDATE cta_saldo_vol
     SET    saldo_acciones = 0,
            fecha_saldo    = fecha_liquida,
            usuario        = g_usuario
     WHERE  nss            = reg_4.nss
     AND    subcuenta      = reg_4.subcuenta
     AND    fecha_saldo   <= fecha_liquida
  END IF
     
  DECLARE s_cargo CURSOR FOR cargo
  OPEN    s_cargo USING vfolio,
                        vfolio_sua,
                        reg_4.nss,
                        reg_4.subcuenta,
                        reg_cta.tipo_movimiento,
                        reg_4.consecutivo,
                        reg_4.siefore,
                        reg_4.acciones,
                        reg_4.pesos,
                        reg_cta.id_aportante,
                        HOY
  FETCH   s_cargo INTO  xcargo
  CLOSE   s_cargo

  IF xcargo < 0 THEN
     ERROR "No se provisiono la subcuenta ",reg_4.subcuenta CLIPPED,
           " del NSS: ",reg_4.nss CLIPPED," del folio ",vfolio CLIPPED
     SLEEP 3 
     EXIT PROGRAM
  END IF

END FUNCTION
#####################################################################
FUNCTION integra_cuenta(reg_5)
  DEFINE reg_5 RECORD
         nss               CHAR(11),
         subcuenta         INTEGER,
         siefore           SMALLINT,
         acciones          DECIMAL(16,6),
         pesos             DECIMAL(16,6),
         cedente           CHAR(03),
         estado            SMALLINT,
         consecutivo       INTEGER
  END RECORD

  DEFINE xabono        SMALLINT
  DEFINE xliq          SMALLINT
  DEFINE vsiefore      SMALLINT
  DEFINE txt_cla       CHAR(200)

  IF reg_5.subcuenta = 14 THEN
     LET  reg_5.acciones    = 0
  END IF

  IF (reg_5.subcuenta = 3   OR
      reg_5.subcuenta = 10) THEN

     LET vsiefore = ""

     SELECT a.codigo_siefore
     INTO   vsiefore
     FROM   cta_regimen a
     WHERE  a.nss = reg_5.nss
     AND    a.subcuenta = reg_5.subcuenta

     LET txt_cla = "EXECUTE PROCEDURE crea_saldo_vol(",
                    vfolio,",",
                    '"',reg_5.nss,'"',",",
                    vsiefore,",",
                    reg_5.subcuenta,",",
                    '"',fecha_liquida,'"',",",
                    '"',fecha_liquida,'"',",",
                    reg_5.pesos,",",
                    reg_5.acciones,",",
                    '"',g_usuario,'"'," )"
     LET txt_cla = txt_cla CLIPPED

     PREPARE claexe FROM txt_cla
     EXECUTE claexe
  END IF
   
  DECLARE s_abono CURSOR FOR abono
  OPEN    s_abono USING vfolio,
                        vfolio_sua,
                        reg_5.nss,
                        reg_5.subcuenta,
                        reg_5.siefore,
                        reg_nva.tipo_movimiento,
                        reg_5.consecutivo,
                        reg_5.acciones,
                        reg_5.pesos,
                        reg_nva.id_aportante,
                        HOY
  FETCH   s_abono INTO  xabono
  CLOSE   s_abono

  IF xabono < 0 THEN
     ERROR "No se provisiono la subcuenta ",reg_5.subcuenta CLIPPED,
           " del NSS: ",reg_5.nss CLIPPED," del folio ",vfolio CLIPPED
     SLEEP 3 
     EXIT PROGRAM
  END IF

END FUNCTION
#####################################################################
FUNCTION ind_transferencia(nss_cta1)
  DEFINE x_ind_transferencia  SMALLINT,
         nss_cta1             CHAR(11)

  LET x_ind_transferencia = 7

  DECLARE s_indicador CURSOR FOR transferencia
  OPEN    s_indicador USING nss_cta1           , ---nss
                            x_ind_transferencia, ---ind_transferencia
                            fecha_liquida        ---fecha_ind_transf
     FETCH s_indicador INTO status_ind
  CLOSE   s_indicador

END FUNCTION
#############################################
FUNCTION habil_siguiente(diaActual)
  DEFINE diaTmp        DATE,
         contador      SMALLINT,
         diaActual     DATE
   
  DEFINE diaHabilSig   DATE,
         diaSemana     SMALLINT,
         feriado       SMALLINT,
         finSemana     SMALLINT

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

END FUNCTION #habil_siguiente
#############################################
FUNCTION registra_liq()

  INSERT INTO dis_cuenta
  SELECT *
  FROM   dis_provision
  WHERE  folio = vfolio

END FUNCTION
#############################################
