############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                         #
#Propietario       => E.F.P.                                               #
#Programa AFIC034  => CARGA DE ARCHIVO NOTIFICACION DOMICILIOS OP.17       #
#Sistema           => AFI.                                                 #
#Autor             => FERNANDO HERRERA HERNANDEZ                           #
#Fecha             => 22 DE FEBRERO DE 2007                                #
############################################################################

DATABASE safre_af

GLOBALS

  DEFINE generar            CHAR(20)
  DEFINE hoy                DATE
  DEFINE xx_fecha           DATE
  DEFINE diaSig             DATE
  DEFINE enter              CHAR(1)
  DEFINE aux_pausa          CHAR(1)
  DEFINE sello              CHAR(24)
  DEFINE varchivo           CHAR(40)
  DEFINE carga              CHAR(60)
  DEFINE ejecuta            CHAR(100)
  DEFINE corr               CHAR(100)
  DEFINE total_reg          SMALLINT
  DEFINE g_plano1           SMALLINT
  DEFINE aceptar            SMALLINT
  DEFINE rechazar           SMALLINT
  DEFINE g_paramgrales      RECORD LIKE seg_modulo.*

  DEFINE reg_det            RECORD
    tipo_registro           CHAR(02),
    tipo_ent_not            CHAR(02),
    tipo_trabajador         CHAR(01),
    nss_afore               CHAR(11),
    curp                    CHAR(18), 
    calle                   CHAR(65),
    no_ext                  CHAR(15),
    no_int                  CHAR(15),
    colonia                 CHAR(65),
    delegacion              CHAR(65),
    cp                      CHAR(05),
    entidad                 CHAR(65),
    pais                    CHAR(03),
    ind_tel1                CHAR(03),              ---erm op 17
    telefono1               CHAR(15),
    extension1              CHAR(05),              ---erm op 17
    ind_tel2                CHAR(03),              ---erm op 17
    telefono2               CHAR(15),
    extension2              CHAR(05),              ---erm op 17
    correo_e                CHAR(40),              ---erm op 17
    fecha_act               CHAR(08),
    hora_act                CHAR(08),
    cod_operacion           CHAR(02),
    diag_proceso            CHAR(15)  
  END RECORD 

  DEFINE
    x_fecha,f1,f2,f3        CHAR(10),
    vfecha_act              DATE

  DEFINE reg_bat            RECORD
    pid                     INTEGER,
    proceso_cod             INTEGER,
    opera_cod               INTEGER,
    nombre_archivo          CHAR(25)
  END RECORD

  DEFINE 
    bnd_proceso             SMALLINT,
    aux_status_interno      SMALLINT,
    cuantos                 INTEGER

  DEFINE
    c_periodo               CHAR(10),
    f_periodo               DATE

  DEFINE
    g_usuario               CHAR(08)

  DEFINE G_LISTA            CHAR(300)
  DEFINE reg_ctr_dom RECORD LIKE afi_ctr_domicilio.*
  DEFINE desc_rechazo       LIKE tab_rdeta.rdeta_desc_c
  DEFINE ldiag_proceso      CHAR(03)

  DEFINE vrowid             INTEGER
--->erm 06 Sep marcado por SALVADOR
  DEFINE vtelefono1                 ,
         vtelefono2         CHAR(13),
         vcve_lada1                 ,
         vcve_lada2         CHAR(3) ,
         long_tel1                  ,
         long_tel2          SMALLINT,
         vtel1              CHAR(10),
         vtel2              CHAR(10),
         ind_tipo_tel1      CHAR(3) ,
         ind_tipo_tel2      CHAR(3) ,
         extension1         CHAR(5),
         extension2         CHAR(5)
--<

END GLOBALS

MAIN

    DISPLAY " "
    DISPLAY ".1"

    CALL STARTLOG('AFIC034.log')
    CALL inicio() #i
    CALL crea_tablas()

    IF NOT bnd_proceso THEN
       DEFER INTERRUPT
       OPTIONS INPUT WRAP,
         PROMPT LINE LAST,
         ACCEPT KEY CONTROL-I
       CALL proceso_principal()   #pp
    ELSE
       CALL sube_archivo()
       CALL rescata_valores()     #rv
       CALL actualiza_bat_f(0)    #ab
    END IF

END MAIN 

FUNCTION inicio()
#----------------

    LET reg_bat.pid            = ARG_VAL(1)
    LET reg_bat.proceso_cod    = ARG_VAL(2)
    LET reg_bat.opera_cod      = ARG_VAL(3)
    LET reg_bat.nombre_archivo = ARG_VAL(4)

    LET bnd_proceso = 0

    IF reg_bat.pid THEN
       DISPLAY "INICIANDO PROCESO ..."
       LET bnd_proceso = 1
    END IF

    LET hoy      = TODAY
    LET aceptar  = 0
    LET rechazar = 0

    SELECT *, USER
    INTO   g_paramgrales.*, g_usuario
    FROM   seg_modulo
    WHERE  modulo_cod = 'afi'

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 4,4 WITH FORM "AFIC0341" ATTRIBUTE(BORDER)
      DISPLAY " AFIC034      CARGA ARCHIVO NOTIFICACION DOMICILIOS OP. 17                     " AT 3,1 ATTRIBUTE(REVERSE)
      DISPLAY "                           < Ctrl-C > Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)
      DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)
  
      DISPLAY g_paramgrales.ruta_rescate AT 7,10

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
          LET carga = g_paramgrales.ruta_rescate CLIPPED,"/",generar CLIPPED

          WHENEVER ERROR CONTINUE
            LOAD FROM carga INSERT INTO plano1
          WHENEVER ERROR STOP

          SELECT COUNT(*)
          INTO   g_plano1
          FROM   plano1

          IF g_plano1 IS NULL OR g_plano1 = 0 THEN
             ERROR "NOMBRE DE ARCHIVO INCORRECTO O VACIO, REVISE."
             SLEEP 5
             EXIT PROGRAM
          END IF

          ERROR "Procesando Informacion"
          CALL rescata_valores()

          PROMPT "Proceso finalizado, [Enter] p/salir" FOR enter

          EXIT PROGRAM

      END INPUT

END FUNCTION

FUNCTION rescata_valores()
#rv-----------------------

    CALL actualiza_datos()
    CALL revisa_datos()
    CALL despliega_resultados()
    --CALL genera_reporte()

END FUNCTION

FUNCTION sube_archivo()
#sa-------------------

    LET carga = g_paramgrales.ruta_rescate CLIPPED,"/",
                reg_bat.nombre_archivo CLIPPED

   SELECT nombre_archivo
    INTO   varchivo
    FROM   afi_ctr_arh_reg
    WHERE  nombre_archivo = reg_bat.nombre_archivo

    IF STATUS <> NOTFOUND THEN
        DISPLAY "Program stopped, ARCHIVO YA PROCESADO"
        EXIT PROGRAM
    END IF

    LOAD FROM carga INSERT INTO plano1

    SELECT count(*)
    INTO   cuantos
    FROM   plano1
    IF cuantos = 0 OR
       cuantos IS NULL THEN
       DISPLAY  "Program stopped, NOMBRE DE ARCHIVO INCORRECTO O VACIO"
       EXIT PROGRAM
    END IF

END FUNCTION

FUNCTION crea_tablas()
#ct-------------------

  CREATE TEMP TABLE plano1
    (n_registros  CHAR(500))

  CREATE TEMP TABLE cza_dom
    (tipo_registro        CHAR(02),
     id_servicio          CHAR(02),
     id_operacion         CHAR(02),
     tipo_origen          CHAR(02),
     cve_origen           CHAR(03),
     tipo_destino         CHAR(02),
     cve_destino          CHAR(03),
     fecha_entrega        CHAR(08),
     consec_lote          CHAR(03),
     cod_result_op        CHAR(02),
     motivo_rechazo       CHAR(09))

  CREATE TEMP TABLE det_dom
    (tipo_registro        CHAR(02),
     tipo_ent_not         CHAR(02),
     tipo_trabajador      CHAR(01),
     nss_afore            CHAR(11),
     curp_afore           CHAR(18),
     calle                CHAR(65),
     no_ext               CHAR(15),
     no_int               CHAR(15),
     colonia              CHAR(65),
     delegacion           CHAR(65),
     cp                   CHAR(05),
     entidad              CHAR(65),
     pais                 CHAR(03),
     prefijo1             CHAR(03),             ---erm op 17
     telefono1            CHAR(10),             ---erm op 17 (de 15 a 10 pos)
     extension1           CHAR(05),             ---erm op 17
     prefijo2             CHAR(03),             ---erm op 17
     telefono2            CHAR(10),             ---erm op 17 (de 15 a 10 pos)
     extension2           CHAR(05),             ---erm op 17
     correo_e             CHAR(40),             ---erm op 17
     fecha_act            CHAR(08),
     hora_act             CHAR(08),
     cod_operacion        CHAR(02),
     diag_proceso         CHAR(15))

  CREATE TEMP TABLE sum_dom
    (tipo_registro        CHAR(02),
     cantidad_reg         CHAR(09))

END FUNCTION

FUNCTION actualiza_datos()
#-------------------------

  DEFINE
    cont_reg   SMALLINT,
    carga_reg  CHAR(500)

  DEFINE
    campo_011  CHAR(02),
    campo_012  CHAR(02),
    campo_013  CHAR(02),
    campo_014  CHAR(02),
    campo_015  CHAR(03),
    campo_016  CHAR(02),
    campo_017  CHAR(03),
    campo_018  CHAR(08),
    campo_019  CHAR(03),
    campo_110  CHAR(02),
    campo_111  CHAR(09),

    campo_01   CHAR(02),
    campo_02   CHAR(02),
    campo_03   CHAR(01),
    campo_04   CHAR(11),
    campo_05   CHAR(18),
    campo_06   CHAR(65),
    campo_07   CHAR(15),
    campo_08   CHAR(15),
    campo_09   CHAR(65),
    campo_10   CHAR(65),
    campo_11   CHAR(05),
    campo_12   CHAR(65),
    campo_13   CHAR(03),
    campo_14   CHAR(03),             ---erm op 17
    campo_15   CHAR(10),             ---erm op 17 (de 15 a 10 pos)
    campo_16   CHAR(05),             ---erm op 17
    campo_17   CHAR(03),             ---erm op 17
    campo_18   CHAR(10),             ---erm op 17 (de 15 a 10 pos)
    campo_19   CHAR(05),             ---erm op 17
    campo_20   CHAR(40),             ---erm op 17
		campo_22   CHAR(08),             ---SALVADOR FALTARON ESTOS
    campo_23   CHAR(08),             ---SALVADOR FALTARON ESTOS
    campo_24   CHAR(02),             ---SALVADOR FALTARON ESTOS
    campo_25   CHAR(15),             ---SALVADOR FALTARON ESTOS

    campo_201  CHAR(02),
    campo_202  CHAR(09)

    LET cont_reg = 0

    SELECT COUNT(*)
    INTO   total_reg
    FROM   plano1

    DECLARE cursor_1 CURSOR FOR 
    SELECT  * 
    FROM    plano1
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
         LET campo_110 = carga_reg[028,029]
         LET campo_111 = carga_reg[030,038]

         INSERT INTO cza_dom
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
                 campo_111)
      END IF

      IF cont_reg <> total_reg AND cont_reg <> 1 THEN
         LET campo_01 = carga_reg[001,002]
         LET campo_02 = carga_reg[003,004]
         LET campo_03 = carga_reg[005,005]
         LET campo_04 = carga_reg[006,016]
         LET campo_05 = carga_reg[017,034]
         LET campo_06 = carga_reg[035,099]
         LET campo_07 = carga_reg[100,114]
         LET campo_08 = carga_reg[115,129]
         LET campo_09 = carga_reg[130,194]
         LET campo_10 = carga_reg[195,259]
         LET campo_11 = carga_reg[260,264]
         LET campo_12 = carga_reg[265,329]
         LET campo_13 = carga_reg[330,332]
         LET campo_14 = carga_reg[333,335]             ---erm op 17
         LET campo_15 = carga_reg[336,345]             ---erm op 17
         LET campo_16 = carga_reg[346,350]             ---erm op 17
         LET campo_17 = carga_reg[351,353]             ---erm op 17
         LET campo_18 = carga_reg[354,363]             ---erm op 17
         LET campo_19 = carga_reg[364,368]             ---erm op 17
         LET campo_20 = carga_reg[369,408]             ---erm op 17
				 LET campo_22 = carga_reg[413,420]             ---SALVADOR FALTARON ESTOS
         LET campo_23 = carga_reg[421,428]             ---SALVADOR FALTARON ESTOS
         LET campo_24 = carga_reg[429,430]             ---SALVADOR FALTARON ESTOS
         LET campo_25 = carga_reg[431,445]             ---SALVADOR FALTARON ESTOS

         INSERT INTO det_dom
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
                 campo_12,
                 campo_13,
                 campo_14,
                 campo_15,
                 campo_16,
                 campo_17,
                 campo_18,
                 campo_19,
								 campo_20,  ---SALVADOR FALTARON ESTOS
                 campo_22,  ---SALVADOR FALTARON ESTOS
                 campo_23,  ---SALVADOR FALTARON ESTOS
                 campo_24,  ---SALVADOR FALTARON ESTOS
                 campo_25)  ---SALVADOR FALTARON ESTOS
      END IF

      IF cont_reg = total_reg THEN
         LET campo_201 = carga_reg[001,002]
         LET campo_202 = carga_reg[003,011]

         INSERT INTO sum_dom
         VALUES (campo_201,
                 campo_202)
      END IF
    END FOREACH

END FUNCTION

FUNCTION revisa_datos()
#----------------------

  DEFINE 
    aux_pausa    CHAR(1),
    rechazo_lote CHAR(3),
    rechazo_deta CHAR(3),
    l_reg RECORD LIKE tab_rch_lote.* ,
    x_reg RECORD LIKE tab_rdeta.* 

  DEFINE 
    rechazo_09   CHAR(02),
    rechazo_001  CHAR(02),
    rechazo_002  CHAR(02),
    rechazo_003  CHAR(02),
    rechazo_010  CHAR(02),
    rechazo_011  CHAR(09)

  DEFINE 
    uno          CHAR(3),
    dos          CHAR(3),
    tre          CHAR(3),
    cua          CHAR(3),
    cin          CHAR(3),
    diag         CHAR(3)

  DEFINE
    l_status_int ,
    vtot_reg     ,
    v_delega     ,
    v_ciudad     ,
    v_estado     ,
    v_concurr    ,
    v_ban_ins    SMALLINT,
    v_nss        CHAR(11),
    lfactualiza  DATE

  DEFINE reg_ctr_dom RECORD LIKE afi_ctr_domicilio.*

  DEFINE vmarca_envio CHAR(1)


  # ENCABEZADO #
  SELECT tipo_registro ,
         id_servicio   , 
         id_operacion  ,
         cod_result_op , 
         motivo_rechazo
  INTO   rechazo_001,
         rechazo_002,
         rechazo_003,
         rechazo_010,
         rechazo_011
  FROM   cza_dom

  SELECT *
  INTO   l_reg.*
  FROM   tab_rch_lote
  WHERE  rlote_cod = rechazo_lote

  IF STATUS  <> NOTFOUND THEN
     CLEAR SCREEN
     DISPLAY l_reg.rlote_cod AT 10,1
     DISPLAY l_reg.rlote_desc_c AT 11,1
     PROMPT "ERROR DE PROCESO, NO PUEDE CONTINUAR PROCESAR" FOR aux_pausa
     EXIT PROGRAM
  END IF

  IF rechazo_001 <> "01" THEN
     IF bnd_proceso THEN
        DISPLAY "Program stopped, Tipo de registro debe ser 01 en ENCABEZADO"
        EXIT PROGRAM
     ELSE
        CLEAR SCREEN
        DISPLAY "Tipo de registro debe ser 01 en ENCABEZADO" AT 10,1
        PROMPT "ERROR DE PROCESO, NO PUEDE CONTINUAR PROCESAR" FOR aux_pausa
        EXIT PROGRAM
     END IF
  END IF

  IF rechazo_002 <> "01" THEN
     IF bnd_proceso THEN
        DISPLAY "Program stopped, Identificador de servicio debe ser 01",
                " en ENCABEZADO"
        EXIT PROGRAM
     ELSE
        CLEAR SCREEN
        DISPLAY "Identificador de servicio ser 01 en ENCABEZADO" AT 10,1
        PROMPT "ERROR DE PROCESO, NO PUEDE CONTINUAR PROCESAR" FOR aux_pausa
        EXIT PROGRAM
     END IF
  END IF

  IF rechazo_003 <> "17" THEN
     IF bnd_proceso THEN
        DISPLAY "Program stopped, Identificador de operacion debe ser 17",
                " en ENCABEZADO" 
        EXIT PROGRAM
     ELSE
        CLEAR SCREEN
        DISPLAY "Identificador de operacion debe ser 17 en ENCABEZADO" 
        AT 10,1
        PROMPT "ERROR DE PROCESO, NO PUEDE CONTINUAR PROCESAR" FOR aux_pausa
        EXIT PROGRAM
     END IF
  END IF

  IF rechazo_010 = "02" THEN
     CLEAR SCREEN
     DISPLAY "Codigo de Resultado es 02 LOTE RECHAZADO" AT 10,1
     DISPLAY "Motivo de Rechazo de Lote: ", rechazo_003 AT 11,1
     PROMPT "ERROR DE PROCESO, NO PUEDE CONTINUAR PROCESAR" FOR aux_pausa
     EXIT PROGRAM
  END IF

  # SUMARIO #
  SELECT tipo_registro
  INTO   rechazo_09 
  FROM   sum_dom

  IF rechazo_09 <> "09" THEN
     IF bnd_proceso THEN
        DISPLAY "Program stopped, Tipo de registro debe ser 09 en RESUMEN" 
        EXIT PROGRAM
     ELSE
        CLEAR SCREEN
        DISPLAY "Tipo de registro debe ser 09 en RESUMEN" AT 10,1
        PROMPT "ERROR DE PROCESO, NO PUEDE CONTINUAR PROCESAR" FOR aux_pausa
        EXIT PROGRAM
     END IF
  END IF

  # DETALLE #
  LET G_LISTA = g_paramgrales.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                ".RECH_NOT_DOM_OP17." CLIPPED, HOY USING "ddmmyy" CLIPPED

  START REPORT listado_1 TO G_LISTA

  LET v_concurr = 0

  DECLARE cursor_2 CURSOR FOR 
  SELECT *
  FROM   det_dom
  ORDER BY tipo_ent_not, nss_afore, curp_afore, cod_operacion ASC,
  fecha_act DESC, hora_act DESC
  FOREACH cursor_2 INTO reg_det.*

    LET v_ban_ins = 0

    IF (reg_det.fecha_act IS NOT NULL) AND
       (reg_det.fecha_act[1] <> ' ')   THEN
        LET x_fecha = reg_det.fecha_act[5,6], "/",
                      reg_det.fecha_act[7,8], "/",
                      reg_det.fecha_act[1,4]
    ELSE
        LET x_fecha = NULL
    END IF
 
    LET vfecha_act = x_fecha
 
    LET ldiag_proceso = reg_det.diag_proceso[1,3]

    IF reg_det.tipo_trabajador = '2' THEN
       SELECT unique a.nti
         INTO reg_det.nss_afore
         FROM cta_ctr_reg_ind a
        WHERE a.curp = reg_det.curp
    END IF

    SELECT a.rdeta_desc_c
      INTO desc_rechazo
      FROM tab_rdeta a
     WHERE a.rdeta_cod  = ldiag_proceso
       AND a.modulo_cod = 'afi'

    IF ldiag_proceso = "000" THEN
       LET desc_rechazo = 'ACEPTADO'
    END IF
   
    OUTPUT TO REPORT listado_1(reg_det.nss_afore, 
                               reg_det.curp, 
                               reg_det.cod_operacion, 
                               reg_det.diag_proceso,
                               desc_rechazo)


    IF reg_det.tipo_ent_not = '01' THEN #01 AFORE
   
       IF reg_det.cod_operacion = '01' THEN
          LET l_status_int = 100
          LET aceptar      = aceptar  + 1

          UPDATE afi_mae_afiliado 
          SET    documento_6 = 3
          WHERE  n_seguro    = reg_det.nss_afore

       ELSE
          LET l_status_int = 40
          LET rechazar     = rechazar + 1
       END IF

       SELECT MAX(factualiza)
       INTO   lfactualiza
       FROM   afi_ctr_domicilio
       WHERE  (nss           = reg_det.nss_afore
       OR     curp           = reg_det.curp)
       AND    status_interno = 30 

       SELECT MAX(ROWID)
       INTO   vrowid
       FROM   afi_ctr_domicilio
       WHERE  (nss           = reg_det.nss_afore
       OR     curp           = reg_det.curp)
       AND    status_interno = 30 
       AND    factualiza     = lfactualiza

       SELECT *
       INTO   reg_ctr_dom.*
       FROM   afi_ctr_domicilio
       WHERE  (nss           = reg_det.nss_afore
       OR     curp           = reg_det.curp)
       AND    status_interno = 30 
       AND    factualiza     = lfactualiza
       AND    ROWID          = vrowid

       INSERT INTO afi_ctr_domicilio
       VALUES ( reg_ctr_dom.nss,                  --nss
                reg_ctr_dom.curp,                 --curp
                reg_ctr_dom.n_folio,              --n_folio
                reg_ctr_dom.tipo_solicitud,       --tipo_solicitud
                reg_ctr_dom.calle,                --calle
                reg_ctr_dom.numero,               --numero exterior
                reg_ctr_dom.depto,                --numero interior
                reg_ctr_dom.colonia,              --colonia
                reg_ctr_dom.delega,               --delegacion
                reg_ctr_dom.ciudad,               --ciudad
                reg_ctr_dom.estado,               --estado
                reg_ctr_dom.codpos,               --codigo_postal
                reg_ctr_dom.dom_cod,              --codigo_domicilio
                reg_ctr_dom.pais_cod,             --codigo_pais
                reg_ctr_dom.marca_envio,          --marca_envio
                reg_ctr_dom.tipo_comp_dom,        --tipo_comprobante_domicilio
                reg_ctr_dom.fecha_comp_dom,       --fecha_comprobante_domicilio
                reg_ctr_dom.ind_tipo_tel1,        --indicador tipo telefono1        ---erm op 17
                reg_ctr_dom.telefono1,            --telefono 1
                reg_ctr_dom.extension1,           --extension1                      ---erm op 17
                reg_ctr_dom.ind_tipo_tel2,        --indicador tipo telefono2        ---erm op 17
                reg_ctr_dom.telefono2,            --telefono 2
                reg_ctr_dom.extension2,           --extension2                      ---erm op 17         --telefono 2
                reg_ctr_dom.tipo_trabajador,      --tipo_trabajador
                reg_ctr_dom.fecha_envio,          --fecha_envio
                reg_det.cod_operacion,            --codigo_resultado
                reg_det.diag_proceso,             --diagnostico_proceso
                l_status_int,                     --status_interno
                g_usuario,                        --usuario
                TODAY )                           --fecha_actualizacion

    ELSE #03 PROCESAR

      IF reg_det.tipo_trabajador = '1' THEN
         SELECT COUNT(*)
         INTO   vtot_reg
         FROM   det_dom
         WHERE  nss_afore  = reg_det.nss_afore
      END IF

      IF reg_det.tipo_trabajador = '2' THEN
         SELECT COUNT(*)
         INTO   vtot_reg
         FROM   det_dom
         WHERE  curp_afore = reg_det.curp
      END IF

      IF vtot_reg = 1 THEN

         IF reg_det.cod_operacion = '01' THEN
            LET vmarca_envio = "X"
            LET l_status_int = 101
            LET aceptar      = aceptar  + 1
  
            UPDATE afi_mae_afiliado 
            SET    documento_6 = 3
            WHERE  n_seguro    = reg_det.nss_afore

            LET v_ban_ins = 1

         ELSE
            LET l_status_int = 42
            LET rechazar     = rechazar + 1
            LET v_ban_ins    = 0
         END IF

         LET vtot_reg = 0

      ELSE

 #prompt reg_det.nss_afore, " ",  reg_det.curp, " ", reg_det.tipo_trabajador, " ", reg_det.tipo_ent_not, " ", reg_det.fecha_act, " ", reg_det.hora_act, " tot_reg", vtot_reg, " concurr", v_concurr," ", reg_det.cod_operacion for aux_pausa
        IF v_concurr = 0 THEN
           IF reg_det.cod_operacion = '01' THEN
              LET vmarca_envio = "X"
              LET l_status_int = 101
              LET aceptar      = aceptar  + 1
    
              UPDATE afi_mae_afiliado 
              SET    documento_6 = 3
              WHERE  n_seguro    = reg_det.nss_afore

              LET v_ban_ins = 1

           ELSE
              LET l_status_int = 42
              LET rechazar     = rechazar + 1
              LET v_ban_ins    = 0
           END IF

           LET v_concurr = v_concurr + 1
           LET vtot_reg  = 0

        ELSE
           LET v_concurr = v_concurr + 1

           IF reg_det.cod_operacion = '01' THEN
              LET vmarca_envio = ""
              LET l_status_int = 102
              LET aceptar      = aceptar  + 1
              LET v_ban_ins    = 0
       
           ELSE
              LET l_status_int = 42
              LET rechazar     = rechazar + 1
              LET v_ban_ins    = 0
           END IF

           IF vtot_reg = v_concurr THEN
              LET v_concurr = 0
           END IF

           LET vtot_reg  = 0
        END IF
      END IF     

      IF reg_det.tipo_trabajador = '2' THEN
         SELECT unique nti
           INTO reg_det.nss_afore
           FROM cta_ctr_reg_ind
          WHERE curp = reg_det.curp
      END IF

      SELECT n_seguro, n_folio, tipo_solicitud
      INTO   reg_det.nss_afore, reg_ctr_dom.n_folio, reg_ctr_dom.tipo_solicitud 
      FROM   afi_mae_afiliado
      WHERE  n_seguro = reg_det.nss_afore

      LET v_delega = 0
      LET v_ciudad = 0
      LET v_estado = 0

      SELECT deleg_cod, ciudad_cod, estad_cod
      INTO   v_delega, v_ciudad, v_estado 
      FROM   tab_codpos
      WHERE  cpos_cod = reg_det.cp

      IF v_ban_ins = 1 THEN

         INSERT INTO afi_domicilio
         VALUES (reg_det.nss_afore                   , --nss     
                 reg_ctr_dom.n_folio                 , --n_folio
                 reg_ctr_dom.tipo_solicitud          , --tipo_solicitud
                 reg_det.calle                       , --calle
                 ""                                  , --entre calle
                 ""                                  , --y calle
                 reg_det.no_ext                      , --numero
                 reg_det.no_int                      , --depto
                 reg_det.colonia                     , --colonia
                 v_delega                            , --delega
                 v_ciudad                            , --ciudad
                 v_estado                            , --estado
                 reg_det.cp                          , --codpos
                 1                                   , --dom_cod
                 reg_det.pais                        , --pais_cod
                 vmarca_envio                        , --marca_envio
                 #""                                  , --tipo_comp_dom
                 #""                                  , --fecha_comp_dom
                 g_usuario                           , --usuario
                 HOY                                 ) --factualiza

         IF (reg_det.telefono1 IS NOT NULL)             AND
            (reg_det.telefono1[1] <> ' ')               AND
            (reg_det.telefono1    <> "000000000000000") THEN

--->erm op 17 
            LET long_tel1 = LENGTH(reg_det.telefono1 CLIPPED)
            IF long_tel1  = 10 THEN
                LET vcve_lada1 = reg_det.telefono1 [1,2]
                SELECT 'X'
                FROM   tab_tel_numeracion
                WHERE  cld = vcve_lada1
                GROUP BY 1
                IF SQLCA.SQLCODE <> NOTFOUND THEN
                    LET vcve_lada1 = vcve_lada1 CLIPPED
                    LET vtelefono1 = reg_det.telefono1[3,10]
                ELSE
                    LET vcve_lada1 = reg_det.telefono1 [1,3]
                    SELECT 'X'
                    FROM   tab_tel_numeracion
                    WHERE  cld = vcve_lada1
                    GROUP BY 1
                    IF SQLCA.SQLCODE <> NOTFOUND THEN
                       LET vcve_lada1 = vcve_lada1 CLIPPED
                       LET vtelefono1 = reg_det.telefono1[4,10]
                    END IF
                END IF
            ELSE
                IF long_tel1 = 13 THEN
                   LET vtelefono1    = reg_det.telefono1 CLIPPED
                   LET ind_tipo_tel1 = reg_det.telefono1[1,3]
                   LET vtel1         = reg_det.telefono1[4,13]
                   INITIALIZE  vcve_lada1 TO NULL
                END IF
            END IF
---<

            INSERT INTO afi_telefono
            VALUES (reg_det.nss_afore                   , --nss
                    reg_ctr_dom.n_folio                 , --n_folio
                    reg_ctr_dom.tipo_solicitud          , --tipo_solicitud
                    reg_det.pais                        , --pais_cod
                    -- ""                                  , --clave_lada ---SALVADOR erm op 17
                    vcve_lada1                          , --clave lada         ---erm op 17
                    ""                                  , --extension
                    --reg_det.telefono1                   , --telefono
                    vtelefono1                          , --telefono            --erm op 17
                    "1"                                 , --tel_cod
                    ""                                  , --hora_ini,                 ---28-18
                    ""                                  , --tipo_hora_ini,            ---28-18
                    ""                                  , --hora_fin,                 ---28-18
                    ""                                  , --tipo_hora_fin,            ---28-18
                    ""                                  , --dia,                      ---28-18
                    g_usuario                           , --usuario
                    HOY                                 ) --factualiza
         END IF
  
         IF (reg_det.telefono2 IS NOT NULL)             AND
            (reg_det.telefono2[1] <> ' ')               AND
            (reg_det.telefono2    <> "000000000000000") THEN
--->erm op 17
            LET long_tel2 = LENGTH(reg_det.telefono2 CLIPPED)
            IF long_tel2  = 10 THEN
                LET vcve_lada2 = reg_det.telefono2 [1,2]
                SELECT 'X'
                FROM   tab_tel_numeracion
                WHERE  cld = vcve_lada2
                GROUP BY 1
                IF SQLCA.SQLCODE <> NOTFOUND THEN
                    LET vcve_lada2 = vcve_lada2 CLIPPED
                    LET vtelefono2 = reg_det.telefono2[3,10]
                ELSE
                    LET vcve_lada2 = reg_det.telefono2[1,3]
                    SELECT 'X'
                    FROM   tab_tel_numeracion
                    WHERE  cld = vcve_lada2
                    GROUP BY 1
                    IF SQLCA.SQLCODE <> NOTFOUND THEN
                       LET vcve_lada2 = vcve_lada2 CLIPPED
                       LET vtelefono2 = reg_det.telefono2[4,10]
                    END IF
                END IF
            ELSE
                IF long_tel2 = 13 THEN
                   LET vtelefono2    = reg_det.telefono2 CLIPPED
                   LET ind_tipo_tel2 = reg_det.telefono2[1,3]
                   LET vtel2         = reg_det.telefono2[4,13]
                   INITIALIZE  vcve_lada2 TO NULL
                END IF
            END IF
---<

            INSERT INTO afi_telefono
            VALUES (reg_det.nss_afore                   , --nss
                    reg_ctr_dom.n_folio                 , --n_folio
                    reg_ctr_dom.tipo_solicitud          , --tipo_solicitud
                    reg_det.pais                        , --pais_cod
                    -- ""                                  , --clave_lada ---SALVADOR erm op 17
                     vcve_lada2                          , --clave lada        ---erm op 17
                    ""                                  , --extension
                    --reg_det.telefono2                   , --telefono
                    vtelefono2                          , --telefono          ---erm op 17
                    "2"                                 , --tel_cod
                    ""                                  , --hora_ini,                 ---28-18
                    ""                                  , --tipo_hora_ini,            ---28-18
                    ""                                  , --hora_fin,                 ---28-18
                    ""                                  , --tipo_hora_fin,            ---28-18
                    ""                                  , --dia,                      ---28-18
                    g_usuario                           , --usuario
                    HOY                                 ) --factualiza
         END IF
      END IF

      INSERT INTO afi_ctr_domicilio
      VALUES ( reg_det.nss_afore,                --nss
               reg_det.curp,                     --curp
               reg_ctr_dom.n_folio,              --n_folio
               reg_ctr_dom.tipo_solicitud,       --tipo_solicitud
               reg_det.calle,                    --calle
               reg_det.no_ext,                   --numero exterior
               reg_det.no_int,                   --numero interior
               reg_det.colonia,                  --colonia
               v_delega,                         --delegacion
               v_ciudad,                         --ciudad
               v_estado,                         --estado
               reg_det.cp,                       --codigo_postal
               1,                                --codigo_domicilio
               reg_det.pais,                     --codigo_pais
               vmarca_envio,                     --marca_envio
               "",                               --tipo_comprobante_domicilio
               "",                               --fecha_comprobante_domicilio
               ind_tipo_tel1,                     --indicador tipo telefono1 (044 o 045)   ---op 17
               --reg_det.telefono1,                --telefono 1
               vtel1,                            --telefono 1                              ---op 17
               "",                               --extension 1                             ---op 17
               ind_tipo_tel2,                    --indicador tipo telefono2 (044 o 045)    ---op 17
               --reg_det.telefono2,                --telefono 2
               vtel2,                            --telefono 2                              ---op 17
               "",                               --extension2                              ---op 17
               reg_det.tipo_trabajador,          --tipo_trabajador
               "",                               --fecha_envio
               reg_det.cod_operacion,            --codigo_resultado
               reg_det.diag_proceso,             --diagnostico_proceso
               l_status_int,                     --status_interno
               g_usuario,                        --usuario
               TODAY )                           --fecha_actualizacion
 
    END IF

    INSERT INTO afi_not_domicilio 
    VALUES ( reg_det.nss_afore,                --nss
             reg_det.curp,                     --curp
             reg_ctr_dom.n_folio,              --n_folio
             reg_ctr_dom.tipo_solicitud,       --tipo_solicitud
             reg_det.tipo_ent_not,             --tipo_entidad_notif
             reg_det.tipo_trabajador,          --tipo_trabajador
             reg_det.calle,                    --calle
             reg_det.no_ext,                   --numero exterior
             reg_det.no_int,                   --numero interior 
             reg_det.colonia,                  --colonia
             reg_det.delegacion,               --delegacion
             reg_det.cp,                       --codigo_postal
             reg_det.entidad,                  --estado
             reg_det.pais,                     --codigo_pais
             reg_det.ind_tel1,                 --indicador tipo telefono1 (044 o 045)   ---op 17
             reg_det.telefono1,                --telefono1
             "",                               --extension 1                             ---op 17
             reg_det.ind_tel2,                 --indicador tipo telefono2 (044 o 045)    ---op 17
             reg_det.telefono2,                --telefono2
             "",                               --extension2                              ---op 17
             vfecha_act,                       --fecha_actualiza
             reg_det.hora_act,                 --hora_actualiza
             reg_det.cod_operacion,            --codigo_resultado
             reg_det.diag_proceso,             --diagnostico_proceso
             g_usuario,                        --usuario
             TODAY)                            --fecha_actualiza

  END FOREACH

  FINISH REPORT listado_1

  DISPLAY "Reporte:  : ", G_LISTA AT 13,01

END FUNCTION

FUNCTION despliega_resultados()
#dr----------------------------

  DEFINE total_resp SMALLINT

  LET total_resp = aceptar + rechazar 

  IF bnd_proceso THEN
     DISPLAY "                TOTAL REGISTROS RECIBIDOS                "

     DISPLAY "Total de Registros del lote: ", total_resp USING "#######&"

     DISPLAY "Registros aceptados        : ", aceptar    USING "#######&"

     DISPLAY "Registros rechazados       : ", rechazar   USING "#######&"
  ELSE
     DISPLAY "                TOTAL REGISTROS RECIBIDOS                 " 
     AT 7,10 ATTRIBUTE ( REVERSE )

     DISPLAY "Total de Registros del lote     : ",
     total_resp USING "#######&" AT 9,15 ATTRIBUTE ( CYAN )

     DISPLAY "Registros aceptados             : ",
     aceptar    USING "#######&" AT 10,15 ATTRIBUTE ( CYAN )

     DISPLAY "Registros rechazados            : ",
     rechazar   USING "#######&" AT 11,15 ATTRIBUTE ( CYAN ) 
  END IF

  INSERT INTO afi_ctr_arh_reg 
  VALUES (generar,
          aceptar,
          rechazar,
          0,
          0,
          0,
          HOY)

END FUNCTION

FUNCTION actualiza_bat_f(v_folio)
#ab-----------------------------

  DEFINE v_cat          CHAR(600),
         vv_fecha_log   CHAR(030),
         vv_prog        CHAR(010),
         paso           CHAR(100)

  DEFINE v_fecha_log    DATETIME YEAR TO SECOND
  
  DEFINE v_folio        INTEGER
  DEFINE reg_ruta       RECORD LIKE seg_modulo.*

  SELECT A.*
  INTO   reg_ruta.*
  FROM   seg_modulo A
  WHERE  modulo_cod = "bat"
 
  UPDATE bat_ctr_operacion
  SET    folio            = NULL ,      
         estado_cod       = 4    ,
         fecha_fin        = CURRENT,
         nom_archivo      = reg_bat.nombre_archivo
  WHERE  pid              = reg_bat.pid
  AND    proceso_cod      = reg_bat.proceso_cod
  AND    opera_cod        = reg_bat.opera_cod

  UPDATE bat_ctr_proceso
  SET    folio            = NULL ,      
         estado_cod       = 4    ,
         fecha_fin        = CURRENT
  WHERE  pid              = reg_bat.pid
  AND    proceso_cod      = reg_bat.proceso_cod
  
  UPDATE bat_tmp_predecesor
  SET    bandera_ejecuta  = 1
  WHERE  pid_prod         = reg_bat.pid
  AND    proceso_cod_prod = reg_bat.proceso_cod
  AND    opera_cod_prod   = reg_bat.opera_cod
  
  LET v_fecha_log  = CURRENT
  LET vv_fecha_log = v_fecha_log
  
  SELECT A.programa_cod 
  INTO   vv_prog 
  FROM   bat_ctr_operacion A
  WHERE  A.pid         = reg_bat.pid
  AND    A.proceso_cod = reg_bat.proceso_cod
  AND    A.opera_cod   = reg_bat.opera_cod
  
  LET paso  = "nohup:"            ,
              reg_bat.pid         USING"&&&&&",":",
              reg_bat.proceso_cod USING"&&&&&",":",
              reg_bat.opera_cod   USING"&&&&&"
  
  LET v_cat = "echo '"                ,
              vv_fecha_log[1,4]       ,   
              vv_fecha_log[6,7]       ,  
              vv_fecha_log[9,10]      ,  
              vv_fecha_log[12,13]     ,   
              vv_fecha_log[15,16]     ,    
              vv_fecha_log[18,19]     ,
              "|"                    ,
              vv_prog  CLIPPED        ,
              "|"                     ,
              "FINOK"                ,
              "|"                     ,
              reg_ruta.ruta_listados CLIPPED,  
              "/"                     ,
              paso CLIPPED            ,
              "'"                     ,
              " >> "                  ,
              reg_ruta.ruta_envio CLIPPED ,
              "/"                     ,
              "aad_safre.log"
  
  LET v_cat = v_cat CLIPPED
  RUN v_cat

END FUNCTION

{FUNCTION genera_reporte()

  DEFINE G_LISTA            CHAR(300)
  DEFINE reg_ctr_dom RECORD LIKE afi_ctr_domicilio.*
  DEFINE desc_rechazo       LIKE tab_rdeta.rdeta_desc_c
  DEFINE ldiag_proceso      CHAR(03)

  LET G_LISTA = g_paramgrales.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                ".RECH_NOT_DOM_MAS." CLIPPED, HOY USING "ddmmyy" CLIPPED

  START REPORT listado_1 TO G_LISTA
 
  DECLARE c_dom CURSOR FOR
  SELECT *
    FROM afi_ctr_domicilio
   WHERE status_interno = 20 
     AND cod_resultado  = 2
   ORDER BY diag_proceso
  FOREACH c_dom INTO reg_ctr_dom.*

    LET ldiag_proceso = reg_ctr_dom.diag_proceso[1,3]

    SELECT a.rdeta_desc_c
      INTO desc_rechazo
      FROM tab_rdeta a
     WHERE a.rdeta_cod  = ldiag_proceso
       AND a.modulo_cod = 'afi'

    OUTPUT TO REPORT listado_1(reg_ctr_dom.*, desc_rechazo)

  END FOREACH

  FINISH REPORT listado_1

  DISPLAY "Reporte:  : ", G_LISTA AT 13,01

END FUNCTION}

REPORT listado_1(lreg_ctr_dom, ldesc_rechazo)
#----------------------
  DEFINE lreg_ctr_dom RECORD 
    nss           CHAR(11),
    curp          CHAR(18),
    cod_resultado CHAR(02),
    diag_proceso  CHAR(15)
  END RECORD
  DEFINE ldesc_rechazo       LIKE tab_rdeta.rdeta_desc_c
  DEFINE lcont               INTEGER

  OUTPUT
    LEFT MARGIN   0
    RIGHT MARGIN  0
    TOP MARGIN    0
    BOTTOM MARGIN 0

  FORMAT
    FIRST PAGE HEADER
      PRINT
        COLUMN 03,"========================================",
        COLUMN 40,"==================================="
      PRINT
        COLUMN 20," RECHAZOS NOTIFICACION DE DOMICILIOS OP. 17"
      PRINT
        COLUMN 03,"----------------------------------------",
        COLUMN 40,"-----------------------------------"
      PRINT

      PRINT
        COLUMN 01,"N S S  "       ,
        COLUMN 13,"CURP   "       ,
        COLUMN 33,"COD RES"       ,
        COLUMN 41,"DIAG PROCESO"  ,
        COLUMN 58,"DESC"       
      PRINT
        COLUMN 03,"========================================",
        COLUMN 40,"==================================="

    ON EVERY ROW
      PRINT
        COLUMN 01,lreg_ctr_dom.nss                   ,
        COLUMN 13,lreg_ctr_dom.curp                  ,
        COLUMN 33,lreg_ctr_dom.cod_resultado         ,
        COLUMN 41,lreg_ctr_dom.diag_proceso CLIPPED  ,
        COLUMN 58,ldesc_rechazo CLIPPED

    ON LAST ROW
      SELECT COUNT(*)
        INTO lcont
        FROM afi_ctr_domicilio
       WHERE status_interno IN (40,42)
         AND cod_resultado  = '02'
         AND factualiza     = TODAY

      PRINT
      PRINT
      PRINT
        COLUMN 03,"----------------------------------------",
        COLUMN 40,"-----------------------------------"

      PRINT
      PRINT
        COLUMN 03,"NUMERO TOTAL DE REGISTROS ---> ",lcont
END REPORT

