############################################################################
#Propietario       => E.F.P.                                               #
#Programa ACRC101  => RECIBE ARCHIVOS DE TRANSFERENCIA DE ACREDITADOS      #
#                     FOVISSSTE                                            #
#Fecha creacion    => JULIO 2007                                           #
#Por               => JOSUÉ LISANDRO HUERTA SIERRA                         #
#Descripcion       => Recibe archivo para devolución de saldos fovisste    #
#Fecha actualiz.   => 9 Octubre 2008                                       #
#Actualizacion .   => SDVP & JRC                                           #
#Fecha actualiz.   =>                                                      #
############################################################################
DATABASE safre_af
GLOBALS
    DEFINE reg_cza_dev_iss RECORD
           tipo_registro           CHAR(2) ,
           ident_servicio          CHAR(2) ,
           ident_operacion         CHAR(2) ,
           tipo_ent_origen         CHAR(2) ,
           cve_ent_origen          CHAR(3) ,
           tipo_ent_destino        CHAR(2) ,
           cve_ent_destino         CHAR(3) ,
           ent_fed_envio_lote      CHAR(3) ,
           fecha_presentacion      DATE    ,
           consec_lote_dia         SMALLINT,
           cve_mod_recepcion       CHAR(2)
    END RECORD

    DEFINE reg_det_dev_iss RECORD
           tipo_registro           CHAR(2)      ,
           cont_servicio           DECIMAL(10,0),
           tipo_ent_emisora        CHAR(2)      ,
           cve_ent_emisora         CHAR(3)      ,
           tipo_ent_receptora      CHAR(2)      ,
           cve_ent_receptora       CHAR(3)      ,
           tipo_transferencia      CHAR(2)      ,
           fecha_presentacion      DATE         ,
           fecha_movimiento        DATE         ,
           curp_fovissste          CHAR(18)     ,
           nss_fovissste           CHAR(11),
           rfc_fovissste           CHAR(13)     ,
           paterno_fovissste       CHAR(40)     ,
           materno_fovissste       CHAR(40)     ,
           nombres_fovissste       CHAR(40)     ,
           ident_lote_solicitud    CHAR(16)     ,
           aivs_92                 DECIMAL(18,6),
           valor_aivs_92           DECIMAL(25,14),
           importe_fov_92          DECIMAL(15,2),
           aivs_08                 DECIMAL(18,6),
			  valor_aivs_08           DECIMAL(25,14),
			  importe_fov_08          DECIMAL(15,2),
           tipo_movimiento         SMALLINT
    END RECORD

    DEFINE reg_sum_dev_iss RECORD
           tipo_registro          CHAR(2)      ,
           fecha_presentacion     DATE,
           cant_reg_det           DECIMAL(9,0) ,
           sum_aivs_92            DECIMAL(22,6),
           sum_importe_fov_92     DECIMAL(15,2),
			  sum_aivs_08            DECIMAL(22,6),
			  sum_importe_fov_08     DECIMAL(15,2)
    END RECORD

    DEFINE
        HOY                   ,
        hoy2                  ,
        vfecha_liquida        DATE

    DEFINE
        enter                 CHAR(1)   ,
        generar               CHAR(20)  ,
        archivo_traspaso      CHAR(200) ,
        c_paso_cza            CHAR(8)   ,
        c_paso_det            CHAR(8)   ,
        c_fecha_cza           CHAR(10)  ,
        c_fecha_det           CHAR(10)  ,
        hora                  CHAR(5)   ,
        g_usuario             CHAR(8)   ,
        v_desmarca            CHAR(300) ,
        desc_afo              CHAR(25)  ,
        g_lista               CHAR(100) ,
        c_fecha_mov           CHAR(10)  ,
        comando               CHAR(100)

    DEFINE
        pmarca_causa          ,
        bnd_proceso           ,
        edo_proc              ,
        vmarca_estado         ,
        vcodigo_rechazo       ,
        xcodigo_marca         ,
        xcodigo_rechazo       SMALLINT,
        cont_archivo          ,
        s_codigo_afore        ,
        vfolio                ,
        tot_nss_rechazo       , 
        vcorrelativo          INTEGER

    DEFINE g RECORD
        total                 INTEGER,
        tipo_transferencia    CHAR(2),
        descripcion           CHAR(20)
    END RECORD

    DEFINE reg_bat RECORD
        pid            INTEGER,
        proceso_cod    INTEGER,
        opera_cod      INTEGER,
        nombre_archivo CHAR(25)
    END RECORD

    DEFINE reg_aboctas    RECORD LIKE dis_provision.*
    DEFINE g_seg_modulo   RECORD LIKE seg_modulo.*

    DEFINE registro RECORD
        nss                 LIKE dis_cuenta.nss,
        subcuenta           LIKE dis_cuenta.subcuenta,
        fecha_valor         LIKE dis_cuenta.fecha_valor,
        fecha_conversion    LIKE dis_cuenta.fecha_conversion,
        monto_en_acciones   LIKE dis_cuenta.monto_en_acciones,
        monto_en_pesos      LIKE dis_cuenta.monto_en_pesos,
        descripcion         CHAR(25)
    END RECORD
END GLOBALS

MAIN
    DISPLAY " "
    DISPLAY ".1"

    CALL STARTLOG("ACRC102.log")
    CALL inicio()

    IF NOT bnd_proceso THEN
        DEFER INTERRUPT
        OPTIONS INPUT WRAP, PROMPT LINE LAST, ACCEPT KEY CONTROL-I
        CALL proceso_principal()
    ELSE
        CALL sube_archivo()
        CALL crea_tablas()
        CALL validacion_previa()
        CALL lee_archivo_plano()
        CALL actualiza_operacion()
    END IF

    CALL impresion_reporte()
    CALL obtener_folio()
    CALL genera_det_tras_afo()
    CALL genera_historicos()
    CALL genera_reporte()
END MAIN

FUNCTION inicio()
    LET reg_bat.pid            = ARG_VAL(1)
    LET reg_bat.proceso_cod    = ARG_VAL(2)
    LET reg_bat.opera_cod      = ARG_VAL(3)
    LET reg_bat.nombre_archivo = ARG_VAL(4)

    LET bnd_proceso  = 0
    LET edo_proc     = 1
    LET pmarca_causa = 0
    LET HOY          = TODAY
    LET hora         = TIME

    IF reg_bat.pid THEN
        DISPLAY "INICIANDO PROCESO ..."
        LET bnd_proceso = 1
    END IF

    SELECT codigo_afore, razon_social, USER
    INTO   s_codigo_afore, desc_afo, g_usuario
    FROM   tab_afore_local

    SELECT * INTO g_seg_modulo.*
    FROM  seg_modulo
    WHERE modulo_cod = 'acr'

    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp

        DROP TABLE tmp_pla_dev_iss

        CREATE TABLE tmp_pla_dev_iss
          (n_registros           CHAR(730))

        DATABASE safre_af
    WHENEVER ERROR STOP
END FUNCTION

FUNCTION proceso_principal()
   OPEN WINDOW ventana_1 AT 4,4 WITH FORM "ACRC1021" ATTRIBUTE(BORDER)
   DISPLAY " ACRC102      RECIBE DEVOLUCION DE SALDOS FOVISSSTE                        " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY "                           < Ctrl-C > Salir                                " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY TO fecha ATTRIBUTE( REVERSE )

   INPUT BY NAME generar
       AFTER FIELD generar
           IF generar IS NULL THEN
               ERROR "Campo NO puede ser NULO"
               NEXT FIELD generar
           END IF

           SELECT "X" FROM acr_ctr_arh a
           WHERE  a.nombre_archivo = generar

           IF SQLCA.SQLCODE = 0 THEN
               PROMPT "ARCHIVO YA PROCESADO,[Enter] p/salir" FOR enter
               EXIT PROGRAM
           END IF

           WHENEVER ERROR CONTINUE

           LET archivo_traspaso = g_seg_modulo.ruta_rescate CLIPPED,
                                  "/",generar CLIPPED

           LOAD FROM archivo_traspaso INSERT INTO safre_tmp:tmp_pla_dev_iss

               SELECT count(*) INTO cont_archivo
               FROM   safre_tmp:tmp_pla_dev_iss

               IF cont_archivo = 0 OR cont_archivo IS NULL THEN
                   DISPLAY  " NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO "
                   AT 19,2 ATTRIBUTE(REVERSE)
                   SLEEP 3
                   NEXT FIELD generar
               ELSE
                   EXIT INPUT
               END IF
           WHENEVER ERROR STOP

       ON KEY (INTERRUPT)
           DISPLAY "PROCESO CANCELADO" AT 19,1 ATTRIBUTE(REVERSE)
           SLEEP 3
           EXIT PROGRAM
   END INPUT 
   ERROR "PROCESANDO INFORMACION "
   CALL crea_tablas()
   CALL validacion_previa()
   CALL lee_archivo_plano()
END FUNCTION

FUNCTION sube_archivo()
    LET archivo_traspaso = g_seg_modulo.ruta_rescate CLIPPED,"/",
                           reg_bat.nombre_archivo CLIPPED

    SELECT "X" FROM acr_ctr_arh a
    WHERE  a.nombre_archivo = reg_bat.nombre_archivo

    LOAD FROM archivo_traspaso INSERT INTO safre_tmp:tmp_pla_dev_iss

    SELECT count(*) INTO cont_archivo
    FROM   safre_tmp:tmp_pla_dev_iss

    IF cont_archivo = 0 OR cont_archivo IS NULL THEN
        DISPLAY  "Program stopped, NOMBRE DE ARCHIVO INCORRECTO O VACIO"
        EXIT PROGRAM
    END IF
END FUNCTION

#########################################################################
#FUNCION crea_tablas, crea las tablas temporales para la carga del      #
#        archivo: cza_tra_dev_iss, det_tra_dev_iss, sum_tra_dev_iss     #
#                                                                       #
#########################################################################
FUNCTION crea_tablas()
    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp
        DROP TABLE cza_tra_dev_iss
        DROP TABLE det_tra_dev_iss
        DROP TABLE sum_tra_dev_iss
    WHENEVER ERROR STOP

    CREATE TABLE cza_tra_dev_iss (
        tipo_registro         CHAR(2) ,
        ident_servicio        CHAR(2) ,
        ident_operacion       CHAR(2) ,
        tipo_ent_origen       CHAR(2) ,
        cve_ent_origen        CHAR(3) ,
        tipo_ent_destino      CHAR(2) ,
        cve_ent_destino       CHAR(3) ,
        ent_fed_envio_lote    CHAR(3) ,
        fecha_presentacion    DATE    ,
        consec_lote_dia       SMALLINT,
        cve_mod_recepcion     CHAR(2) 
       );

    CREATE TABLE det_tra_dev_iss (
        tipo_registro         CHAR(2)       ,
        cont_servicio         DECIMAL(10,0) ,
        tipo_ent_emisora      CHAR(2)       ,
        cve_ent_emisora       CHAR(3)       ,
        tipo_ent_receptora    CHAR(2)       ,
        cve_ent_receptora     CHAR(3)       ,
        tipo_transferencia    CHAR(2)       ,
        fecha_presentacion    DATE          ,
        fecha_movimiento      DATE          ,
        curp_fovissste        CHAR(18)      ,
        nss_fovissste         CHAR(11)      ,
        rfc_fovissste         CHAR(13)      ,
        paterno_fovissste     CHAR(40)      ,
        materno_fovissste     CHAR(40)      ,
        nombres_fovissste     CHAR(40)      ,
        ident_lote_solicitud  CHAR(16)      ,
        aivs_92               DECIMAL(18,6) ,
        valor_aivs_92         DECIMAL(25,14),
        importe_fov_92        DECIMAL(15,2) ,
        aivs_08               DECIMAL(18,6) ,
        valor_aivs_08         DECIMAL(25,14),
        importe_fov_08        DECIMAL(15,2) ,
        tipo_movimiento       SMALLINT      ,
        estado                SMALLINT
       );

    CREATE TABLE sum_tra_dev_iss (
         tipo_registro        CHAR(02),
         fecha_presentacion   DATE,
         cant_reg_det         DECIMAL(9,0),
         sum_aivs_92          DECIMAL(22,6),
         sum_importe_fov_92   DECIMAL(15,2),
         sum_aivs_08          DECIMAL(22,6),
         sum_importe_fov_08   DECIMAL(15,2)
       );

     DATABASE safre_af
END FUNCTION

FUNCTION validacion_previa()
    DEFINE
        c2_tipo_registro      CHAR(2)
    DEFINE
        sw_1                  ,
        sw_2                  ,
        sw_9                  SMALLINT

    DECLARE cur_2 CURSOR FOR
		 SELECT UNIQUE(n_registros[1,2]) FROM safre_tmp:tmp_pla_dev_iss

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
            DISPLAY "Program stopped, SE RECHAZA EL LOTE. NO EXISTE ENCABEZADO"
        ELSE
            PROMPT "SE RECHAZA EL LOTE. NO EXISTE ENCABEZADO" FOR enter
            EXIT PROGRAM
        END IF
    END IF

    IF sw_2 = 0 THEN
        IF bnd_proceso THEN
            DISPLAY "Program stopped, SE RECHAZA EL LOTE. NO EXISTE DETALLE"
        ELSE
            PROMPT "SE RECHAZA EL LOTE. NO EXISTEN REGISTROS DE DETALLE" 
            FOR enter
            EXIT PROGRAM
        END IF
    END IF

    IF sw_9 = 0 THEN
        IF bnd_proceso THEN
            DISPLAY "Program stopped, SE RECHAZA EL LOTE. NO EXISTE SUMARIO"
        ELSE
            PROMPT "SE RECHAZA EL LOTE. NO EXISTE SUMARIO" FOR enter
            EXIT PROGRAM
        END IF
    END IF
END FUNCTION

FUNCTION lee_archivo_plano()
    DEFINE
        carga_reg            CHAR(730) ,
        c2_ident_operacion   CHAR(2)   ,
        c_paso_fecha_mov     CHAR(8)  ,
        aux_valor_aivs_92,
		  aux_valor_aivs_08 		DECIMAL(30,14)

    DECLARE cur_tmp_dev CURSOR FOR
		 SELECT * FROM safre_tmp:tmp_pla_dev_iss

    LET c2_ident_operacion = ""

    FOREACH cur_tmp_dev INTO carga_reg
               #---ENCABEZADO SOLICITUD DEVOLUCION DE SALDO FOVISSSTE---#
        IF carga_reg[5,6] = "94" THEN
            LET c2_ident_operacion = "01"
            LET reg_cza_dev_iss.tipo_registro     = carga_reg[001,002]
            LET reg_cza_dev_iss.ident_servicio    = carga_reg[003,004]
            LET reg_cza_dev_iss.ident_operacion   = carga_reg[005,006]
            LET reg_cza_dev_iss.tipo_ent_origen   = carga_reg[007,008]
            LET reg_cza_dev_iss.cve_ent_origen    = carga_reg[009,011]
            LET reg_cza_dev_iss.tipo_ent_destino  = carga_reg[012,013]
            LET reg_cza_dev_iss.cve_ent_destino   = carga_reg[014,016]
            LET reg_cza_dev_iss.ent_fed_envio_lote= carga_reg[017,019]
            LET c_paso_cza                        = carga_reg[020,027]
            LET reg_cza_dev_iss.consec_lote_dia   = carga_reg[028,030]
            LET reg_cza_dev_iss.cve_mod_recepcion = carga_reg[031,032]
            LET c_fecha_cza = c_paso_cza[5,6],"/",
                              c_paso_cza[7,8],"/",
                              c_paso_cza[1,4]
            LET reg_cza_dev_iss.fecha_presentacion = c_fecha_cza

            INSERT INTO safre_tmp:cza_tra_dev_iss VALUES(reg_cza_dev_iss.*)
        END IF

                #---DETALLE DE SOLICITUD DE TRANSFERENCIA---#
        IF carga_reg[1,2] = "02" AND c2_ident_operacion = "01" THEN
            LET reg_det_dev_iss.tipo_registro         = carga_reg[001,002]
            LET reg_det_dev_iss.cont_servicio         = carga_reg[003,012]
            LET reg_det_dev_iss.tipo_ent_emisora      = carga_reg[013,014]
            LET reg_det_dev_iss.cve_ent_emisora       = carga_reg[015,017]
            LET reg_det_dev_iss.tipo_ent_receptora    = carga_reg[018,019]
            LET reg_det_dev_iss.cve_ent_receptora     = carga_reg[020,022]
            LET reg_det_dev_iss.tipo_transferencia    = carga_reg[023,024]
            LET c_paso_det                            = carga_reg[025,032]
            LET c_paso_fecha_mov                      = carga_reg[033,040]
            LET reg_det_dev_iss.curp_fovissste        = carga_reg[041,058]
            LET reg_det_dev_iss.nss_fovissste         = carga_reg[059,069]
            LET reg_det_dev_iss.rfc_fovissste         = carga_reg[085,097]
            LET reg_det_dev_iss.paterno_fovissste     = carga_reg[098,137]
            LET reg_det_dev_iss.materno_fovissste     = carga_reg[138,177]
            LET reg_det_dev_iss.nombres_fovissste     = carga_reg[178,217]
            LET reg_det_dev_iss.ident_lote_solicitud  = carga_reg[240,255]
            LET reg_det_dev_iss.aivs_92               = carga_reg[475,489]
            LET aux_valor_aivs_92                     = carga_reg[490,509]
            LET reg_det_dev_iss.importe_fov_92        = carga_reg[510,524]

            LET reg_det_dev_iss.tipo_movimiento       = carga_reg[540,541]

            LET reg_det_dev_iss.aivs_08               = carga_reg[542,556]
            LET aux_valor_aivs_08                     = carga_reg[557,576]
            LET reg_det_dev_iss.importe_fov_08        = carga_reg[577,591]

            LET c_fecha_det = c_paso_det[5,6],"/",
                              c_paso_det[7,8],"/",
                              c_paso_det[1,4]
            LET reg_det_dev_iss.fecha_presentacion  = c_fecha_det

            LET c_fecha_mov = c_paso_fecha_mov[5,6],"/",
                              c_paso_fecha_mov[7,8],"/",
                              c_paso_fecha_mov[1,4]
            LET reg_det_dev_iss.fecha_movimiento  = c_fecha_mov

            LET reg_det_dev_iss.aivs_92        = reg_det_dev_iss.aivs_92 / 1000000
            LET reg_det_dev_iss.importe_fov_92 = reg_det_dev_iss.importe_fov_92 / 100
            LET reg_det_dev_iss.valor_aivs_92  = aux_valor_aivs_92 / 100000000000000.0

            LET reg_det_dev_iss.aivs_08        = reg_det_dev_iss.aivs_08 / 1000000
            LET reg_det_dev_iss.importe_fov_08 = reg_det_dev_iss.importe_fov_08 / 100
            LET reg_det_dev_iss.valor_aivs_08  = aux_valor_aivs_08 / 100000000000000.0

            INSERT INTO safre_tmp:det_tra_dev_iss VALUES(reg_det_dev_iss.*,0)
        END IF

                #---SUMARIO DE SOLICITUD DE TRANSFERENCIA---#
        IF carga_reg[1,2] = "09" AND c2_ident_operacion = "01" THEN
            LET c2_ident_operacion = ""
            LET reg_sum_dev_iss.tipo_registro        = carga_reg[001,002]
            LET reg_sum_dev_iss.cant_reg_det         = carga_reg[003,011]
            LET reg_sum_dev_iss.sum_aivs_92          = carga_reg[042,059]
            LET reg_sum_dev_iss.sum_importe_fov_92   = carga_reg[060,074]
            LET reg_sum_dev_iss.sum_aivs_08          = carga_reg[075,092]
            LET reg_sum_dev_iss.sum_importe_fov_08   = carga_reg[093,107]

            LET reg_sum_dev_iss.fecha_presentacion  = reg_det_dev_iss.fecha_presentacion
            LET reg_sum_dev_iss.sum_aivs_92         = reg_sum_dev_iss.sum_aivs_92 / 1000000
            LET reg_sum_dev_iss.sum_importe_fov_92  = reg_sum_dev_iss.sum_importe_fov_92/100
            LET reg_sum_dev_iss.sum_aivs_08         = reg_sum_dev_iss.sum_aivs_08 / 1000000
            LET reg_sum_dev_iss.sum_importe_fov_08  = reg_sum_dev_iss.sum_importe_fov_08/100

            INSERT INTO safre_tmp:sum_tra_dev_iss VALUES(reg_sum_dev_iss.*)
        END IF
    END FOREACH
END FUNCTION

FUNCTION actualiza_operacion()
    UPDATE bat_ctr_operacion
    SET    estado_operacion = 4,
           fecha_fin        = CURRENT,
           nom_archivo      = reg_bat.nombre_archivo
    WHERE  pid              = reg_bat.pid
    AND    proceso_cod      = reg_bat.proceso_cod
    AND    opera_cod        = reg_bat.opera_cod
END FUNCTION

FUNCTION impresion_reporte()
    DEFINE G_IMPRE        CHAR(300)
    DEFINE c_impre        CHAR(300)
    DEFINE gimpresion     CHAR(300)

    LET hora    = TIME
    LET G_IMPRE = g_seg_modulo.ruta_listados CLIPPED, "/", g_usuario CLIPPED,
                 ".SOL_DEV_ISS.", HOY USING "DDMMYY",
                 "_", hora[1,2], hora[4,5] CLIPPED

    START REPORT det_dev_iss_imp TO G_IMPRE
        OUTPUT TO REPORT det_dev_iss_imp( g.* )
    FINISH REPORT det_dev_iss_imp

    LET gimpresion = "lp ", G_IMPRE
    RUN gimpresion

    LET c_impre = ' chmod 777 ', G_IMPRE CLIPPED
    RUN c_impre
END FUNCTION

REPORT det_dev_iss_imp(g)
  DEFINE tot_reg INTEGER
  DEFINE  g RECORD
    total               INTEGER,
    tipo_transferencia  CHAR(2),
    descripcion         CHAR(20)
  END RECORD

  OUTPUT
    TOP MARGIN 1
    BOTTOM MARGIN 0
    LEFT MARGIN 0
    RIGHT MARGIN 0
    PAGE LENGTH 60

  FORMAT
    PAGE HEADER

      SELECT COUNT(*)
      INTO   tot_reg
      FROM   safre_tmp:det_tra_dev_iss

      PRINT COLUMN 01,desc_afo,
            COLUMN 68,TODAY USING "DD-MM-YYYY"
      SKIP 2 LINE
      PRINT COLUMN 04,"TOTAL DE REGISTROS SOLICITADOS PARA DEVOLUCION DE SALDO FOVISSSTE"
      SKIP 2 LINE
      PRINT COLUMN 08,"NOMBRE ARCHIVO RECIBIDO : ", generar
      SKIP 1 LINE 
      PRINT COLUMN 01,"-------------------------------------------------------------"
      PRINT COLUMN 01,"TOTAL DE REGISTROS",
            COLUMN 22,"TIPO DE MOVIMIENTO",
            COLUMN 50,"DESCRIPCION"
      PRINT COLUMN 01,"-------------------------------------------------------------"
      SKIP 1 LINE 
    ON EVERY ROW

      DECLARE cur_reporte CURSOR FOR
          SELECT COUNT(*), tipo_transferencia
          FROM safre_tmp:det_tra_dev_iss
          GROUP BY 2
          ORDER BY 2

      FOREACH cur_reporte INTO g.total, g.tipo_transferencia
          CASE g.tipo_transferencia
              WHEN "05" LET g.descripcion = "TRANSF CTAS FOVISSSTE"
          END CASE

          PRINT COLUMN 10,g.total,
                COLUMN 32,g.tipo_transferencia,
                COLUMN 45,g.descripcion
      END FOREACH

     ON LAST ROW
        SKIP 4 LINES
        PRINT COLUMN 2, "Total registros solicitados : ",
        tot_reg USING "<<<<"

     PAGE TRAILER
      SKIP 2 LINE
      PRINT COLUMN 60,"Pagina:",PAGENO USING "<<<<"
      PAUSE "Presione enter para continuar...."
END REPORT

FUNCTION impresion_reporte_fechpres()
  DEFINE G_IMPRE        CHAR(300)
  DEFINE c_impre        CHAR(300)
  DEFINE gimpresion     CHAR(300)

  DEFINE  h  RECORD
      nss        CHAR(11),
      fech_pres  DATE,
      cod_edo    SMALLINT,
      desc_edo   CHAR(30)
  END RECORD

  LET hora = TIME

  LET G_IMPRE = g_seg_modulo.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
              ".DEV_SAL_ISS_FECHPRES.",HOY USING "DDMMYY",
               "_",hora[1,2],hora[4,5] CLIPPED

  START REPORT det_iss_imp_fechpres TO G_IMPRE

  DECLARE cur_reporte2 CURSOR FOR
     SELECT nss_fovissste,fecha_presentacion,estado
     FROM   safre_tmp:det_tra_dev_iss
     WHERE  estado <> 0
     ORDER BY 1

  FOREACH cur_reporte2 INTO h.*
     SELECT rechazo_desc INTO h.desc_edo FROM tab_rch_marca
     WHERE  rechazo_cod = h.cod_edo

     OUTPUT TO REPORT det_iss_imp_fechpres(h.*)
  END FOREACH
  FINISH REPORT det_iss_imp_fechpres

  LET gimpresion = "lp ",G_IMPRE
  RUN gimpresion

  LET c_impre = ' chmod 777 ', G_IMPRE CLIPPED
  RUN c_impre
END FUNCTION

REPORT det_iss_imp_fechpres(h)
  DEFINE h     RECORD
    nss         CHAR(11),
    fecha_pres  DATE,
    cod_edo     SMALLINT,
    desc_edo    CHAR(30)
  END RECORD

  DEFINE tot_reg INTEGER

  OUTPUT
    TOP MARGIN 1
    BOTTOM MARGIN 0
    LEFT MARGIN 0
    RIGHT MARGIN 0
    PAGE LENGTH 60

  FORMAT
    PAGE HEADER
      SELECT COUNT(*)
      INTO   tot_reg
      FROM   safre_tmp:det_tra_dev_iss
      WHERE  estado <> 0

      PRINT COLUMN 01,desc_afo,
            COLUMN 68,TODAY USING "DD-MM-YYYY"
      SKIP 2 LINE
      PRINT COLUMN 08,"DEVOLUCION DE SALDO REGISTROS DE TRANSFERENCIA DE ACREDITADOS FOVISSSTE"
      SKIP 2 LINE
      PRINT COLUMN 08,"NOMBRE ARCHIVO RECIBIDO : ", generar
      SKIP 1 LINE
      PRINT COLUMN 01,"--------------------------------------------------------------------------------------"
      PRINT COLUMN 05,"N. SEGURO",
            COLUMN 20,"FECHA PRESENTACION",
            COLUMN 40,"ESTADO",
            COLUMN 50,"DESC. ESTADO"
      PRINT COLUMN 01,"--------------------------------------------------------------------------------------"
      SKIP 1 LINE
 ON EVERY ROW

      PRINT 
             COLUMN 05,h.nss,
             COLUMN 25,h.fecha_pres USING "DD-MM-YYYY",
             COLUMN 37,h.cod_edo,
             COLUMN 50,h.desc_edo

    ON LAST ROW
       SKIP 4 LINES
       PRINT COLUMN 2, "Total registros solicitados : ",
       tot_reg USING "<<<<"

    PAGE TRAILER
       SKIP 2 LINE
       PRINT COLUMN 60,"Pagina:",PAGENO USING "<<<<"
       PAUSE "Presione enter para continuar...."
END REPORT

#########################################################################
#FUNCION obtener_folio, genera el folio para el tipo 13                 #
#########################################################################
FUNCTION obtener_folio()
    CURRENT WINDOW IS ventana_1
    INPUT BY NAME vfecha_liquida
         AFTER FIELD vfecha_liquida
             IF vfecha_liquida IS NULL THEN
                 NEXT FIELD vfecha_liquida
             ELSE
                 IF vfecha_liquida > HOY THEN
                     ERROR "LA FECHA DE LIQUIDACION DEBE SER MENOR O IGUAL AL DIA DE HOY"
                     SLEEP 2
                     NEXT FIELD vfecha_liquida
                 ELSE
                     EXIT INPUT
                 END IF
             END IF
         ON KEY (INTERRUPT)
             ERROR "PROCESO CANCELADO"
             SLEEP 3
             EXIT PROGRAM
    END INPUT

    LET hoy2  = MDY(MONTH(vfecha_liquida),1,YEAR(vfecha_liquida))

	 {
    SELECT folio INTO vfolio FROM taa_folio
    WHERE  MONTH(fecha) = MONTH(vfecha_liquida)
    AND    YEAR(fecha)  = YEAR(vfecha_liquida)
    AND    tipo         = 13

    IF STATUS = NOTFOUND THEN
	 }

	 INSERT INTO glo_folio VALUES (0)#folio
	 SELECT MAX(folio) INTO vfolio FROM glo_folio
	 INSERT INTO taa_folio VALUES(vfolio,13,HOY,g_usuario)

    --END IF

    DISPLAY BY NAME vfolio
    DISPLAY c_fecha_det TO vfecha_presentacion
END FUNCTION

#########################################################################
#FUNCION genera_det_tras_afo, inserta en la tabla_disprovision          #
#         los saldos asi como ejecuta la desmarca y/o la                #
#         devolucion de saldos.                                         #
#########################################################################
FUNCTION genera_det_tras_afo()
    DEFINE
        vprecio_accion     DECIMAL(22,14),
        dia_semana         SMALLINT,
        vcurp_fovissste    CHAR(18),
        vcont_servicio     DECIMAL(10,0),
        vnss_afore         CHAR(11),
		  fec_conv				DATE

	 IF s_codigo_afore = 564 THEN
	 	LET fec_conv = TODAY
	 ELSE
	 	LET fec_conv = vfecha_liquida
	 END IF
    DECLARE cur_devol CURSOR FOR
		 SELECT * FROM safre_tmp:det_tra_dev_iss
    FOREACH cur_devol INTO reg_det_dev_iss.*
    CALL valida_curp_nss( reg_det_dev_iss.nss_fovissste,
		reg_det_dev_iss.curp_fovissste ) RETURNING vnss_afore,vcurp_fovissste

		  IF( reg_det_dev_iss.aivs_92 > 0 ) THEN
			  INSERT INTO dis_provision VALUES
				 (edo_proc,                                 #tipo_movimiento
					14,                                      #subcuenta
					12,                                      #siefore
				  vfolio,                                   #folio
				  reg_det_dev_iss.cont_servicio,            #consecutivo_lote
				  vnss_afore,                               #nss
				  reg_det_dev_iss.curp_fovissste,           #curp
				  "FOV",                                    #folio_sua
				  vfecha_liquida,                           #fecha_pago
				  hoy2,                                     #fecha_valor
				  fec_conv,                                 #fecha_conversion
				  reg_det_dev_iss.importe_fov_92,           #monto_en_pesos
				  reg_det_dev_iss.aivs_92,                  #monto_en_acciones
				  reg_det_dev_iss.valor_aivs_92,            #precio_accion
				  0,                                        #dias_cotizados
				  "",                                       #sucursal
				  "DEV-FOV",                                #id_aportante
				  5,                                        #estado
				  TODAY,                                    #fecha_proceso
				  g_usuario,                                #usuario
				  reg_det_dev_iss.fecha_presentacion,       #fecha_archivo
				  0 )                                       #etiqueta
			END IF

		  IF( reg_det_dev_iss.aivs_08 > 0 ) THEN
			  INSERT INTO dis_provision VALUES
				 (edo_proc,                                 #tipo_movimiento
					35,                                      #subcuenta
					12,                                      #siefore
				  vfolio,                                   #folio
				  reg_det_dev_iss.cont_servicio,            #consecutivo_lote
				  vnss_afore,                               #nss
				  reg_det_dev_iss.curp_fovissste,           #curp
				  "FOV",                                    #folio_sua
				  vfecha_liquida,                           #fecha_pago
				  hoy2,                                     #fecha_valor
				  fec_conv,                                 #fecha_conversion
				  reg_det_dev_iss.importe_fov_08,           #monto_en_pesos
				  reg_det_dev_iss.aivs_08,                  #monto_en_acciones
				  reg_det_dev_iss.valor_aivs_08,            #precio_accion
				  0,                                        #dias_cotizados
				  "",                                       #sucursal
				  "DEV-FOV",                                #id_aportante
				  5,                                        #estado
				  TODAY,                                    #fecha_proceso
				  g_usuario,                                #usuario
				  reg_det_dev_iss.fecha_presentacion,       #fecha_archivo
				  0 )                                       #etiqueta
			END IF

    CASE reg_det_dev_iss.tipo_movimiento
        WHEN "03"
            CALL desmarca_cuenta ( vnss_afore )

        WHEN "04"
            CALL desmarca_cuenta( vnss_afore )

				IF( reg_det_dev_iss.aivs_92 > 0 ) THEN
					CALL devolucion_saldo( reg_det_dev_iss.cont_servicio,
												 vnss_afore,
												 reg_det_dev_iss.curp_fovissste,
												 reg_det_dev_iss.fecha_movimiento,
												 reg_det_dev_iss.importe_fov_92, 
												 reg_det_dev_iss.aivs_92,
												 reg_det_dev_iss.valor_aivs_92, 14 )
				END IF

				IF( reg_det_dev_iss.aivs_08 > 0 ) THEN
					CALL devolucion_saldo( reg_det_dev_iss.cont_servicio,
												 vnss_afore,
												 reg_det_dev_iss.curp_fovissste,
												 reg_det_dev_iss.fecha_movimiento,
												 reg_det_dev_iss.importe_fov_08, 
												 reg_det_dev_iss.aivs_08,
												 reg_det_dev_iss.valor_aivs_08, 35 )
				END IF

        WHEN "05"
				IF( reg_det_dev_iss.aivs_92 > 0 ) THEN
					CALL devolucion_saldo( reg_det_dev_iss.cont_servicio,
												 vnss_afore,
												 reg_det_dev_iss.curp_fovissste,
												 reg_det_dev_iss.fecha_movimiento,
												 reg_det_dev_iss.importe_fov_92, 
												 reg_det_dev_iss.aivs_92,
												 reg_det_dev_iss.valor_aivs_92, 14 )
				END IF

				IF( reg_det_dev_iss.aivs_08 > 0 ) THEN
					CALL devolucion_saldo( reg_det_dev_iss.cont_servicio,
												 vnss_afore,
												 reg_det_dev_iss.curp_fovissste,
												 reg_det_dev_iss.fecha_movimiento,
												 reg_det_dev_iss.importe_fov_08, 
												 reg_det_dev_iss.aivs_08,
												 reg_det_dev_iss.valor_aivs_08, 35 )
				END IF

        OTHERWISE
            PROMPT "El tipo de movimiento para el NSS:", vnss_afore,
					" no existe. [Enter] Para salir." FOR ENTER
            DELETE FROM dis_provision
					WHERE folio = vfolio AND fecha_proceso = TODAY
            EXIT PROGRAM
    END CASE
    END FOREACH
    DISPLAY "DEVOLUCION TERMINADA"
END FUNCTION

#########################################################################
#FUNCION genera_historicos, inserta los valores de las tablas temporales#
#        en las tablas historicas: acr_devol_issste, acr_cza_dev_issste #
#        acr_det_dev_issste, acr_sum_dev_issste                         #
#########################################################################
FUNCTION genera_historicos()
    DEFINE vtotal_reg           INTEGER
    DEFINE ident_servicio       CHAR(2)
    DEFINE importe              DECIMAL(25,6)
    DEFINE fecha_liquidacion    DATE
    DEFINE impt_aport_acept 	  DECIMAL(25,6)
    DEFINE impt_inter_acept 	  DECIMAL(17,2)
    DEFINE fecha_archivo        DATE
    DEFINE reg_det              RECORD LIKE safre_tmp:det_tra_dev_iss.*
    DEFINE reg_cza              RECORD LIKE safre_tmp:cza_tra_dev_iss.*
    DEFINE reg_sum              RECORD LIKE safre_tmp:sum_tra_dev_iss.*
    DEFINE bnd_ins_devol        SMALLINT

    LET bnd_ins_devol = FALSE

    DECLARE cursor_cza CURSOR FOR
		 SELECT * FROM safre_tmp:cza_tra_dev_iss
    FOREACH cursor_cza INTO reg_cza.*
        SELECT "X" FROM acr_cza_dev_issste
        WHERE  folio = vfolio
        AND    fecha_presentacion = reg_cza.fecha_presentacion

        IF STATUS = NOTFOUND THEN
            INSERT INTO acr_cza_dev_issste VALUES (vfolio, reg_cza.*,0)
            LET ident_servicio     = reg_cza.ident_servicio
            LET fecha_archivo      = reg_cza.fecha_presentacion
        END IF
    END FOREACH

    DECLARE cursor_sum CURSOR FOR
		 SELECT * FROM safre_tmp:sum_tra_dev_iss
    FOREACH cursor_sum INTO reg_sum.*
        SELECT "X" FROM acr_sum_dev_issste
        WHERE  folio = vfolio
        GROUP BY 1
        IF STATUS = NOTFOUND THEN
            INSERT INTO acr_sum_dev_issste VALUES ( vfolio, reg_sum.* )
            LET bnd_ins_devol = TRUE
        END IF
    END FOREACH

    DECLARE cursor_det CURSOR FOR
		 SELECT * FROM safre_tmp:det_tra_dev_iss
    FOREACH cursor_det INTO reg_det.*
        SELECT "X" FROM acr_det_dev_issste
        WHERE  folio = vfolio
        AND    (nss_fovissste = reg_det.nss_fovissste OR curp_fovissste = reg_det.curp_fovissste)
        AND    fecha_presentacion = reg_det.fecha_presentacion

        IF STATUS = NOTFOUND THEN
            INSERT INTO acr_det_dev_issste VALUES ( vfolio, reg_det.* )
            LET fecha_liquidacion  = reg_det.fecha_movimiento
        END IF
    END FOREACH

    IF bnd_ins_devol THEN
        SELECT SUM(sum_aivs_92 + sum_aivs_08) INTO impt_aport_acept
        FROM safre_tmp:sum_tra_dev_iss

        SELECT SUM(sum_importe_fov_92 + sum_importe_fov_08)
		  INTO impt_inter_acept
        FROM safre_tmp:sum_tra_dev_iss

        LET importe = impt_aport_acept + impt_inter_acept

        INSERT INTO acr_devol_issste VALUES(
                                           vfolio,
                                           "02",
                                           ident_servicio,
                                           "             58",
                                           importe,
                                           fecha_liquidacion,
                                           impt_aport_acept,
                                           impt_inter_acept,
                                           1,
                                           fecha_archivo,
                                           TODAY )
     END IF

    IF bnd_proceso THEN
        DISPLAY "CARGA DE HISTORICO TERMINADA"
    ELSE
        DISPLAY "CARGA DE HISTORICO TERMINADA" AT 19,1 ATTRIBUTE(REVERSE)
    END IF

    SELECT COUNT(*) INTO vtotal_reg FROM safre_tmp:det_tra_acr_iss

    INSERT INTO acr_ctr_arh
		 VALUES( generar, vtotal_reg, vtotal_reg, 0, 0, today, g_usuario )
END FUNCTION

#########################################################################
#FUNCION genera_reporte, genera el reporte de los registros que les     #
#        fue liquidadado su saldo                                       #
#########################################################################
FUNCTION genera_reporte()
    DECLARE reprec_1 CURSOR FOR
    SELECT a.nss,
           a.subcuenta,
           a.fecha_valor,
           a.fecha_conversion,
           sum(a.monto_en_acciones),
           sum(a.monto_en_pesos)
    FROM   dis_cuenta a
    WHERE  a.folio           = vfolio 
    AND    a.subcuenta       IN ( 14, 35 )
    AND    a.tipo_movimiento = edo_proc
    GROUP BY 1,2,3,4
    ORDER BY 1,2

    LET g_lista = g_seg_modulo.ruta_listados CLIPPED, "/",
                  g_usuario CLIPPED,
                  ".DEV_SALDO_ISS.",HOY USING "DDMMYY","_",
                  hora[1,2],hora[4,5]

    START REPORT reporte_1 TO g_lista

    FOREACH reprec_1 INTO registro.*
        CASE registro.subcuenta
            WHEN "14" LET registro.descripcion = "FONDO DE VIVIENDA 14"

            WHEN "35" LET registro.descripcion = "FONDO DE VIVIENDA 08"
        END CASE
        OUTPUT TO REPORT reporte_1 ( registro.*, vfolio, hoy, c_fecha_mov )
    END FOREACH
    FINISH REPORT reporte_1
    LET comando = "lp ", g_lista
    RUN comando
END FUNCTION

REPORT reporte_1( registro, vfolio, hoy, hoy2 )
    DEFINE registro RECORD
        nss               LIKE dis_cuenta.nss,
        subcuenta         LIKE dis_cuenta.subcuenta         ,
        fecha_valor       LIKE dis_cuenta.fecha_valor       ,
        fecha_conversion  LIKE dis_cuenta.fecha_conversion  ,
        monto_en_acciones LIKE dis_cuenta.monto_en_acciones ,
        monto_en_pesos    LIKE dis_cuenta.monto_en_pesos    ,
        descripcion       CHAR(25)
    END RECORD
    DEFINE vfolio INTEGER
    DEFINE hoy2   DATE
    DEFINE hoy    DATE
    DEFINE tot_reg INTEGER

    OUTPUT
        PAGE LENGTH 60
        LEFT MARGIN 0
        RIGHT MARGIN 132
        TOP MARGIN 0
        BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER
		 SELECT COUNT(*) INTO tot_reg FROM acr_det_dev_issste d
		 WHERE  d.folio = vfolio
		 AND    d.estado = 0

       PRINT
        SKIP 2 LINES
        PRINT
            COLUMN 50, hoy USING "DD/MM/YYYY"
        PRINT
        PRINT
            COLUMN 10,
               "CVE. ", s_codigo_afore USING "&&&","      ", desc_afo CLIPPED
        PRINT "------------------------------------------------------------"
        PRINT "FOLIO TRANFERENCIA: ",COLUMN 49, vfolio
        PRINT "FECHA LIQUIDACION : ",COLUMN 50, vfecha_liquida
        PRINT "------------------------------------------------------------"
        PRINT "  NSS",
        COLUMN 20, "SUBCUENTA",
        COLUMN 35, "PARTICIPACIONES",
        COLUMN 52, "MONTO PESOS"
        PRINT "------------------------------------------------------------"

    SKIP 2 LINES
    ON EVERY ROW
        PRINT registro.nss,
        COLUMN 13, registro.subcuenta USING "###",
        COLUMN 17, registro.descripcion CLIPPED,
        COLUMN 35, registro.monto_en_acciones USING "$$$$$$$$$&.&&&&&&",
        COLUMN 50, registro.monto_en_pesos    USING "$$$$$$$$$&.&&&&&&"

    ON LAST ROW 
    PRINT"------------------------------------------------------------"    
    PRINT COLUMN 35, SUM(registro.monto_en_acciones) USING "$$$$$$$$$&.&&&&&&",
          COLUMN 50, SUM(registro.monto_en_pesos)    USING "$$$$$$$$$&.&&&&&&"
    PRINT
    PRINT COLUMN 2, "Total registros a enviar : ", tot_reg
END REPORT

#########################################################################
#FUNCION valida_curp_nss, valida que exista el nss y la curp en la tabla#
#        afi_mae_afiliado                                               #
#########################################################################
FUNCTION valida_curp_nss(valida_nss,valida_curp)
    DEFINE
        valida_nss          CHAR(11),
        valida_curp         CHAR(18),
        regre_nss           CHAR(11),
        regre_curp          CHAR(18)

    IF (valida_nss IS NULL OR valida_nss = ' ') AND 
       (valida_curp IS NOT NULL AND valida_curp <> ' ') THEN #Contiene la curp pero no el NSS
           SELECT a.n_seguro
           INTO   regre_nss
           FROM   afi_mae_afiliado a
           WHERE  a.n_unico = valida_curp
           RETURN regre_nss, valida_curp
    END IF

    IF (valida_nss IS NOT NULL AND valida_nss <> ' ') AND 
       (valida_curp IS NULL OR valida_curp = ' ' )THEN #Contiene el NSS pero no la CURP
           SELECT a.n_unico
           INTO   regre_curp
           FROM   afi_mae_afiliado a
           WHERE  a.n_seguro = valida_nss
           RETURN valida_nss,regre_curp
    END IF

    IF (valida_nss IS NOT NULL AND valida_nss <> ' ')AND #Si contiene NSS y CURP se verifica que la CURP
       (valida_curp IS NOT NULL AND valida_curp <> ' ')THEN #y NSS sean el mismos en la tabla de afiliados
           SELECT a.n_unico
           INTO   regre_curp
           FROM   afi_mae_afiliado a
           WHERE  a.n_seguro = valida_nss

           IF regre_curp IS NULL OR regre_curp <> valida_curp THEN
                   SELECT a.n_seguro
                   INTO   regre_nss
                   FROM   afi_mae_afiliado a
                   WHERE  a.n_unico = valida_curp
                   RETURN regre_nss,valida_curp
           ELSE
                   RETURN valida_nss, valida_curp
           END IF
     END IF

      IF (valida_nss IS NULL OR valida_nss = ' ') AND 
         (valida_curp IS NULL OR valida_curp = ' ') THEN #Si CURP y NSS no tienen valor
               PROMPT "El archivo no contiene CURP, ni NSS. [Enter] para salir" FOR enter
               EXIT PROGRAM
      END IF
END FUNCTION

#########################################################################
#FUNCION desmarca_cuenta, hace la desmarca para el valor 231        ,   #
#                                                                       #
#########################################################################
FUNCTION desmarca_cuenta(vnss)
    DEFINE
        vnss          CHAR(11),
        vcorrelativo  INTEGER,
        pestado_marca SMALLINT,
        v_marca       SMALLINT

    SELECT c.correlativo
    INTO   vcorrelativo
    FROM   cta_act_marca c
    WHERE  c.nss = vnss
    AND    c.marca_cod = v_marca

    LET pestado_marca = 0
    LET v_marca       = 231
    LET v_desmarca    = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"

    PREPARE eje_desmarca FROM v_desmarca

    EXECUTE eje_desmarca
    USING vnss,
          v_marca,
          vcorrelativo,
          pestado_marca,
          pmarca_causa,
          g_usuario
END FUNCTION 

#########################################################################
#FUNCION devolucion_saldo, inserta en la tabla dis_cuenta los saldos,   #
#        recibe como parametros las variales vcont_servicio, vnss_afore,#
#        vcurp_fovissste, vfecha_movimiento, vimporte_vivienda          #
#########################################################################
FUNCTION devolucion_saldo( vcont_servicio, vnss_afore, vcurp_fovissste,
	vfecha_movimiento, l_impviv, l_acc, l_pacc, l_scta )
	DEFINE 
		vcont_servicio    LIKE acr_det_dev_issste.cont_servicio,
		vcurp_fovissste   LIKE acr_det_dev_issste.curp_fovissste,
		vfecha_movimiento LIKE acr_det_dev_issste.fecha_movimiento,
		l_impviv          DECIMAL(15,2),
		l_acc					DECIMAL(18,6),
		l_pacc            DECIMAL(25,14),
		l_scta            SMALLINT,
		vnss_afore        CHAR(11),
		fec_conv				DATE

		 IF s_codigo_afore = 564 THEN
			LET fec_conv = TODAY
		 ELSE
			LET fec_conv = vfecha_liquida
		 END IF

        INSERT INTO dis_cuenta VALUES
          ( edo_proc,                                #tipo_movimiento
            l_scta,                                  #subcuenta
             12,                                     #siefore
           vfolio,                                   #folio
           vcont_servicio,                           #consecutivo_lote
           vnss_afore,                               #nss
           vcurp_fovissste,                          #curp
           "FOV",                                    #folio_sua
           vfecha_liquida,                           #fecha_pago
           hoy2,                                     #fecha_valor
           fec_conv,                                 #fecha_conversion
           l_impviv,                                 #monto_en_pesos
           l_acc,                                    #monto_en_acciones
           l_pacc,                                   #precio_accion
           0,                                        #dias_cotizados
           "",                                       #sucursal
           "DEV-FOV",                                #id_aportante
           5,                                        #estado
           TODAY,                                    #fecha_proceso
           g_usuario,                                #usuario
           reg_det_dev_iss.fecha_presentacion,       #fecha_archivo
           0 )                                       #etiqueta
END FUNCTION
