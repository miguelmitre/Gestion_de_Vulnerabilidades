############################################################################
#Propietario       => E.F.P.                                               #
#Programa ACRC101  => RECIBE ARCHIVOS DE TRANSFERENCIA DE ACREDITADOS      #
#                     FOVISSSTE                                            #
#Fecha creacion    => JULIO 2007                                           #
#Por               => JOSUÉ LISANDRO HUERTA SIERRA                         #
#Fecha actualiz.   =>                                                      #
#Actualizacion .   =>                                                      #
#Fecha actualiz.   =>                                                      #
############################################################################
DATABASE safre_af
GLOBALS
   DEFINE reg_cza_tra_acr_iss RECORD
      tipo_registro           CHAR(2),
      ident_servicio          CHAR(2),
      ident_operacion         CHAR(2),
      tipo_ent_origen         CHAR(2),
      cve_ent_origen          CHAR(3),
      tipo_ent_destino        CHAR(2),
      cve_ent_destino         CHAR(3),
      ent_fed_envio_lote      CHAR(3),
      fecha_presentacion      DATE   ,
      consec_lote_dia         SMALLINT,
      cve_mod_recepcion       CHAR(2)
    END RECORD

   DEFINE reg_det_tra_acr_iss RECORD
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
      nss_fovissste           CHAR(11)     ,
      rfc_fovissste           CHAR(13)     ,
      paterno_fovissste       CHAR(40)     ,
      materno_fovissste       CHAR(40)     ,
      nombres_fovissste       CHAR(40)     ,
      ident_lote_solicitud    CHAR(16)     ,
      aivs_92                 DECIMAL(18,6),
      valor_aivs_92           DECIMAL(25,14),
      importe_fov_92          DECIMAL(15,2),
      tipo_movimiento         SMALLINT,
      aivs_08                 DECIMAL(18,6),
      valor_aivs_08           DECIMAL(25,14),
      importe_fov_08          DECIMAL(15,2)
   END RECORD

   DEFINE reg_sum_tra_acr_iss RECORD
      tipo_registro           CHAR(2)      ,
      fecha_movimiento        DATE         ,
      cant_reg_det            DECIMAL(9,0) ,
      sum_aivs_92             DECIMAL(22,6),
      sum_importe_fov_92      DECIMAL(15,2),
      sum_aivs_08             DECIMAL(22,6),
      sum_importe_fov_08      DECIMAL(15,2)
   END RECORD

   DEFINE g RECORD
      total                   INTEGER,
      tipo_transferencia      CHAR(2),
      descripcion             CHAR(20)
   END RECORD

   DEFINE reg_bat RECORD
      pid                     INTEGER,
      proceso_cod             INTEGER,
      opera_cod               INTEGER,
      nombre_archivo          CHAR(25)
   END RECORD

   DEFINE registro RECORD
      subcuenta               LIKE dis_cuenta.subcuenta,
      fecha_valor             LIKE dis_cuenta.fecha_valor,
      fecha_conversion        LIKE dis_cuenta.fecha_conversion,
      monto_en_acciones       LIKE dis_cuenta.monto_en_acciones,
      monto_en_pesos          LIKE dis_cuenta.monto_en_pesos,
      descripcion             CHAR(15)
    END RECORD

   DEFINE reg_aboctas    RECORD LIKE dis_provision.*
   DEFINE g_seg_modulo   RECORD LIKE seg_modulo.*

   DEFINE
      enter                   CHAR(1)  ,
      generar                 CHAR(20) ,
      archivo_traspaso        CHAR(200),
      c_paso_cza              CHAR(8)  ,
      c_paso_det              CHAR(8)  ,
      c_fecha_cza             CHAR(10) ,
      c_fecha_det             CHAR(10) ,
      hora                    CHAR(5)  ,
      g_usuario               CHAR(8)  ,
      ejecuta                 CHAR(300),
      vnss_afore              CHAR(11) ,
      desc_afo                CHAR(25) ,
      g_lista                 CHAR(100),
      c_fecha_mov             CHAR(10) ,
      comando                 CHAR(100)

   DEFINE
      HOY                     ,
      vfecha_liquida          ,
      hoy2                    DATE

   DEFINE
      tot_pesos_rechazo       DECIMAL(23,6)

   DEFINE
      band                    ,
      pmarca_causa            ,
      bnd_proceso             ,
      edo_proc                ,
      vmarca_estado           ,
      vcodigo_rechazo         ,
      xcodigo_marca           ,
      xcodigo_rechazo         SMALLINT,
      cuantos                 ,
      s_codigo_afore          ,
      vfolio                  ,
      bnd_rechazo_liq         ,
      tot_nss_rechazo         INTEGER

END GLOBALS


MAIN

   DISPLAY " "
   DISPLAY ".1"
   
   CALL STARTLOG (FGL_GETENV("USER")||".ACRC101.log")
   CALL inicio()

   IF NOT bnd_proceso THEN
      DEFER INTERRUPT
      OPTIONS INPUT WRAP,
      PROMPT LINE LAST  ,
      ACCEPT KEY CONTROL-I

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

   IF bnd_proceso THEN
      DISPLAY "CARGA DE HISTORICO TERMINADA"
   ELSE
      ERROR "CARGA DE HISTORICO TERMINADA"
      SLEEP 3
   END IF
   DISPLAY "                                      "
END MAIN


FUNCTION inicio()
#----------------

   LET reg_bat.pid            = ARG_VAL(1)
   LET reg_bat.proceso_cod    = ARG_VAL(2)
   LET reg_bat.opera_cod      = ARG_VAL(3)
   LET reg_bat.nombre_archivo = ARG_VAL(4)

   LET bnd_proceso  = 0
   LET edo_proc     = 231
   LET pmarca_causa = 0
   LET HOY   = TODAY
   LET hora  = TIME

   IF reg_bat.pid THEN
      DISPLAY "INICIANDO PROCESO ..."
      LET bnd_proceso = 1
   END IF

   SELECT codigo_afore,
          razon_social,
          USER
   INTO   s_codigo_afore,
          desc_afo,
          g_usuario
   FROM   tab_afore_local

   SELECT *
   INTO   g_seg_modulo.*
   FROM   seg_modulo
   WHERE  modulo_cod = 'acr'

   WHENEVER ERROR CONTINUE
      DATABASE safre_tmp

      DROP TABLE tmp_pla_acr_iss

      CREATE TABLE tmp_pla_acr_iss
      (n_registros           CHAR(730))

      DATABASE safre_af
   WHENEVER ERROR STOP

END FUNCTION


FUNCTION proceso_principal()
#---------------------------

   OPEN WINDOW ventana_1 AT 4,4 WITH FORM "ACRC1011" ATTRIBUTE(BORDER)
   DISPLAY " ACRC101  RECIBE SOLICITUDES TRANSF ACRED FOVISSSTE                        " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY "                           < Ctrl-C > Salir                                " AT 1,1 ATTRIBUTE(REVERSE)

   DISPLAY HOY TO fecha ATTRIBUTE(REVERSE)

   INPUT BY NAME generar
      AFTER FIELD generar
         IF generar IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD generar
         END IF

         SELECT "X"
         FROM   acr_ctr_arh a
         WHERE  a.nombre_archivo = generar

         IF SQLCA.SQLCODE = 0 THEN
            PROMPT "ARCHIVO YA PROCESADO,[Enter] p/salir" FOR enter
            EXIT PROGRAM
         END IF

         WHENEVER ERROR CONTINUE

         LET archivo_traspaso = g_seg_modulo.ruta_rescate CLIPPED,
                                "/",generar CLIPPED

         LOAD FROM archivo_traspaso INSERT INTO safre_tmp:tmp_pla_acr_iss

         SELECT count(*)
         INTO   cuantos
         FROM   safre_tmp:tmp_pla_acr_iss

         IF cuantos = 0 THEN
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

   ERROR "RECIBIENDO ARCHIVO..."

   CALL crea_tablas()
   CALL validacion_previa()
   CALL lee_archivo_plano()

END FUNCTION


FUNCTION sube_archivo()
#----------------------

   LET archivo_traspaso = g_seg_modulo.ruta_rescate CLIPPED,"/",
                          reg_bat.nombre_archivo CLIPPED

   SELECT "X"
   FROM   acr_ctr_arh a
   WHERE  a.nombre_archivo = reg_bat.nombre_archivo

   IF SQLCA.SQLCODE = 0 THEN
      PROMPT "ARCHIVO YA PROCESADO,[Enter] p/salir" FOR enter
      EXIT PROGRAM
   END IF

   LOAD FROM archivo_traspaso INSERT INTO safre_tmp:tmp_pla_acr_iss

   SELECT count(*)
   INTO   cuantos
   FROM   safre_tmp:tmp_pla_acr_iss

   IF cuantos = 0 OR
      cuantos IS NULL THEN
      DISPLAY  "Program stopped, NOMBRE DE ARCHIVO INCORRECTO O VACIO"
      EXIT PROGRAM
   END IF

END FUNCTION


FUNCTION crea_tablas()
#---------------------

   WHENEVER ERROR CONTINUE
      DATABASE safre_tmp
      DROP TABLE cza_tra_acr_iss
      DROP TABLE det_tra_acr_iss
      DROP TABLE sum_tra_acr_iss
   WHENEVER ERROR STOP

   CREATE TABLE cza_tra_acr_iss
   (tipo_registro         CHAR(2) ,
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

   CREATE TABLE det_tra_acr_iss 
   (tipo_registro         CHAR(2)       ,
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
    tipo_movimiento       SMALLINT      ,
    aivs_08               DECIMAL(18,6) ,
    valor_aivs_08         DECIMAL(25,14),
    importe_fov_08        DECIMAL(15,2) ,
    estado                SMALLINT
   );

   CREATE TABLE sum_tra_acr_iss
   (tipo_registro         CHAR(02)      ,
    fecha_presentacion    DATE          ,
    cant_reg_det          DECIMAL(9,0)  ,
    sum_aivs_92           DECIMAL(22,6) ,
    sum_importe_fov_92    DECIMAL(15,2) ,
    sum_aivs_08           DECIMAL(22,6) ,
    sum_importe_fov_08    DECIMAL(15,2)
   );

   DATABASE safre_af

END FUNCTION


FUNCTION validacion_previa()
#---------------------------

   DEFINE
      c2_tipo_registro      CHAR(2)

   DEFINE
      sw_1                  ,
      sw_2                  ,
      sw_9                  SMALLINT

   DECLARE cur_2 CURSOR FOR
   SELECT UNIQUE(n_registros[1,2])
   FROM   safre_tmp:tmp_pla_acr_iss

   LET sw_1 = 0
   LET sw_2 = 0
   LET sw_9 = 0

   FOREACH cur_2 INTO c2_tipo_registro
      CASE c2_tipo_registro
         WHEN "01"
            LET sw_1 = 1
         WHEN "02"
            LET sw_2 = 1
         WHEN "09"
            LET sw_9 = 1
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
#---------------------------

   DEFINE
      bnd_proc             ,
      cont                 INTEGER

   DEFINE
      carga_reg            CHAR(730),
      c2_ident_operacion   CHAR(2),
      c_paso_fecha_mov     CHAR(10),
      aux_valor_aivs_92    ,
      aux_valor_aivs_08    DECIMAL(30,14)

   DECLARE cur_tmp_acr CURSOR FOR
   SELECT *
   FROM safre_tmp:tmp_pla_acr_iss

   LET cont = 0
   LET c2_ident_operacion = ""

   FOREACH cur_tmp_acr INTO carga_reg

      LET cont = cont + 1

      #---ENCABEZADO SOLICITUD DE TRANSFERENCIA FOVISSSTE---#
      IF carga_reg[5,6] = "94" THEN
         LET c2_ident_operacion = "01"
         LET reg_cza_tra_acr_iss.tipo_registro     = carga_reg[001,002]
         LET reg_cza_tra_acr_iss.ident_servicio    = carga_reg[003,004]
         LET reg_cza_tra_acr_iss.ident_operacion   = carga_reg[005,006]
         LET reg_cza_tra_acr_iss.tipo_ent_origen   = carga_reg[007,008]
         LET reg_cza_tra_acr_iss.cve_ent_origen    = carga_reg[009,011]
         LET reg_cza_tra_acr_iss.tipo_ent_destino  = carga_reg[012,013]
         LET reg_cza_tra_acr_iss.cve_ent_destino   = carga_reg[014,016]
         LET reg_cza_tra_acr_iss.ent_fed_envio_lote= carga_reg[017,019]
         LET c_paso_cza                            = carga_reg[020,027]
         LET reg_cza_tra_acr_iss.consec_lote_dia   = carga_reg[028,030]
         LET reg_cza_tra_acr_iss.cve_mod_recepcion = carga_reg[031,032]

         LET c_fecha_cza = c_paso_cza[5,6],"/",
                           c_paso_cza[7,8],"/",
                           c_paso_cza[1,4]

         LET reg_cza_tra_acr_iss.fecha_presentacion = c_fecha_cza

         INSERT INTO safre_tmp:cza_tra_acr_iss VALUES(reg_cza_tra_acr_iss.*)
      END IF

      #---DETALLE DE SOLICITUD DE TRANSFERENCIA---#
      IF carga_reg[1,2] = "02" AND c2_ident_operacion = "01" THEN
         LET reg_det_tra_acr_iss.tipo_registro         = carga_reg[001,002]
         LET reg_det_tra_acr_iss.cont_servicio         = carga_reg[003,012]
         LET reg_det_tra_acr_iss.tipo_ent_emisora      = carga_reg[013,014]
         LET reg_det_tra_acr_iss.cve_ent_emisora       = carga_reg[015,017]
         LET reg_det_tra_acr_iss.tipo_ent_receptora    = carga_reg[018,019]
         LET reg_det_tra_acr_iss.cve_ent_receptora     = carga_reg[020,022]
         LET reg_det_tra_acr_iss.tipo_transferencia    = carga_reg[023,024]
         LET c_paso_det                                = carga_reg[025,032]
         LET c_paso_fecha_mov                          = carga_reg[033,040]
         LET reg_det_tra_acr_iss.curp_fovissste        = carga_reg[041,058]
         LET reg_det_tra_acr_iss.nss_fovissste         = carga_reg[059,069]
         LET reg_det_tra_acr_iss.rfc_fovissste         = carga_reg[085,097]
         LET reg_det_tra_acr_iss.paterno_fovissste     = carga_reg[098,137]
         LET reg_det_tra_acr_iss.materno_fovissste     = carga_reg[138,177]
         LET reg_det_tra_acr_iss.nombres_fovissste     = carga_reg[178,217]
         LET reg_det_tra_acr_iss.ident_lote_solicitud  = carga_reg[240,255]
         LET reg_det_tra_acr_iss.aivs_92               = carga_reg[475,489]
         LET aux_valor_aivs_92                         = carga_reg[490,509]
         LET reg_det_tra_acr_iss.importe_fov_92        = carga_reg[510,524]

         LET reg_det_tra_acr_iss.tipo_movimiento       = carga_reg[540,541]

         LET reg_det_tra_acr_iss.aivs_08               = carga_reg[542,556]
         LET aux_valor_aivs_08                         = carga_reg[557,576]
         LET reg_det_tra_acr_iss.importe_fov_08        = carga_reg[577,591]

         LET c_fecha_det = c_paso_det[5,6],"/",
                           c_paso_det[7,8],"/",
                           c_paso_det[1,4]
         LET reg_det_tra_acr_iss.fecha_presentacion  = c_fecha_det

         LET c_fecha_mov = c_paso_fecha_mov[5,6],"/",
                           c_paso_fecha_mov[7,8],"/",
                           c_paso_fecha_mov[1,4]
         LET reg_det_tra_acr_iss.fecha_movimiento  = c_fecha_mov

         LET reg_det_tra_acr_iss.aivs_92        = reg_det_tra_acr_iss.aivs_92 / 1000000
         LET reg_det_tra_acr_iss.importe_fov_92 = reg_det_tra_acr_iss.importe_fov_92 /100
         LET reg_det_tra_acr_iss.valor_aivs_92  = aux_valor_aivs_92 / 100000000000000.0

         LET reg_det_tra_acr_iss.aivs_08        = reg_det_tra_acr_iss.aivs_08 / 1000000
         LET reg_det_tra_acr_iss.importe_fov_08 = reg_det_tra_acr_iss.importe_fov_08 /100
         LET reg_det_tra_acr_iss.valor_aivs_08  = aux_valor_aivs_08 / 100000000000000.0

         INSERT INTO safre_tmp:det_tra_acr_iss VALUES( reg_det_tra_acr_iss.*, 0 )
      END IF

      #---SUMARIO DE SOLICITUD DE TRANSFERENCIA---#
      IF carga_reg[1,2] = "09" AND c2_ident_operacion = "01" THEN
         LET c2_ident_operacion = ""
         LET reg_sum_tra_acr_iss.tipo_registro      = carga_reg[001,002]
         LET reg_sum_tra_acr_iss.cant_reg_det       = carga_reg[003,011]
         LET reg_sum_tra_acr_iss.sum_aivs_92        = carga_reg[042,059]
         LET reg_sum_tra_acr_iss.sum_importe_fov_92 = carga_reg[060,074]
         LET reg_sum_tra_acr_iss.sum_aivs_08        = carga_reg[075,092]
         LET reg_sum_tra_acr_iss.sum_importe_fov_08 = carga_reg[093,107]

         LET reg_sum_tra_acr_iss.fecha_movimiento     = c_fecha_mov
         LET reg_sum_tra_acr_iss.sum_aivs_92          = reg_sum_tra_acr_iss.sum_aivs_92 / 1000000
         LET reg_sum_tra_acr_iss.sum_importe_fov_92 = reg_sum_tra_acr_iss.sum_importe_fov_92/100
         LET reg_sum_tra_acr_iss.sum_aivs_08          = reg_sum_tra_acr_iss.sum_aivs_08 / 1000000
         LET reg_sum_tra_acr_iss.sum_importe_fov_08 = reg_sum_tra_acr_iss.sum_importe_fov_08/100

         INSERT INTO safre_tmp:sum_tra_acr_iss VALUES(reg_sum_tra_acr_iss.*)
      END IF
   END FOREACH
END FUNCTION


FUNCTION actualiza_operacion()
#------------------------------

   UPDATE bat_ctr_operacion
   SET    estado_operacion = 4,
          fecha_fin        = CURRENT,
          nom_archivo      = reg_bat.nombre_archivo
   WHERE  pid              = reg_bat.pid
   AND    proceso_cod      = reg_bat.proceso_cod
   AND    opera_cod        = reg_bat.opera_cod

END FUNCTION


FUNCTION impresion_reporte()
#---------------------------

   DEFINE w_codigo_afore LIKE tab_afore_local.codigo_afore

   DEFINE
      G_IMPRE        CHAR(300),
      c_impre        CHAR(300),
      gimpresion     CHAR(300)

   SELECT  codigo_afore
   INTO    w_codigo_afore
   FROM    tab_afore_local

   LET G_IMPRE = g_seg_modulo.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                 ".SOL_ACR_ISS.",hoy USING "DDMMYY",
                 "_",hora[1,2],hora[4,5] CLIPPED

   START REPORT det_tra_acr_iss_imp TO G_IMPRE

   OUTPUT TO REPORT det_tra_acr_iss_imp(g.*)

   FINISH REPORT det_tra_acr_iss_imp

   LET gimpresion = "lp ",G_IMPRE
   RUN gimpresion

   LET c_impre = ' chmod 777 ', G_IMPRE CLIPPED
   RUN c_impre

END FUNCTION


REPORT det_tra_acr_iss_imp(g)
#----------------------------

   DEFINE i RECORD LIKE tab_afore_local.*

   DEFINE g RECORD
      total               INTEGER,
      tipo_transferencia  CHAR(2),
      descripcion         CHAR(40)
   END RECORD

   DEFINE
      tot_reg             INTEGER

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN 0
      RIGHT MARGIN 0
      PAGE LENGTH 60

   FORMAT
      PAGE HEADER
         SELECT razon_social
         INTO   i.razon_social
         FROM   tab_afore_local

         SELECT COUNT(*)
         INTO   tot_reg
         FROM   safre_tmp:det_tra_acr_iss

         PRINT COLUMN 01,i.razon_social,
               COLUMN 68,TODAY USING "dd-mm-yyyy"       

         SKIP 2 LINE

         PRINT COLUMN 04,"TOTAL DE REGISTROS SOLICITADOS PARA TRANSFERENCIA DE ACREDITADOS FOVISSSTE"

         SKIP 2 LINE

         PRINT COLUMN 08,"NOMBRE ARCHIVO RECIBIDO : ", generar

         SKIP 1 LINE

         PRINT COLUMN 01,"-----------------------------------------------------------------------------------------"
         PRINT COLUMN 01,"TOTAL DE REGISTROS",
               COLUMN 22,"TIPO DE MOVIMIENTO",
               COLUMN 50,"DESCRIPCION"
         PRINT COLUMN 01,"-----------------------------------------------------------------------------------------"
         SKIP 1 LINE


      ON EVERY ROW

         DECLARE cursor CURSOR FOR
         SELECT COUNT(*),
                tipo_movimiento
         FROM   safre_tmp:det_tra_acr_iss
         GROUP BY 2
         ORDER BY 2

         FOREACH cursor INTO g.total,
                             g.tipo_transferencia

            CASE g.tipo_transferencia
               WHEN "1"
                  LET g.descripcion = "MARCA DE LA CUENTA Y TRANSFERENCIA"
               WHEN "2"
                  LET g.descripcion = "TRANSFERENCIA"
            END CASE

            PRINT COLUMN 10,g.total,
                  COLUMN 32,g.tipo_transferencia,
                  COLUMN 46,g.descripcion

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


FUNCTION obtener_folio()
#-----------------------

   DEFINE
      vfecha_presentacion   DATE

   ERROR ""

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

   ERROR "PROCESANDO INFORMACION ..."

   LET hoy2  = MDY(MONTH(vfecha_liquida),1,YEAR(vfecha_liquida))

   SELECT t.fecha_presentacion
   INTO vfecha_presentacion
   FROM safre_tmp:cza_tra_acr_iss t

{
   SELECT folio
   INTO vfolio
   FROM taa_folio
   WHERE  fecha = vfecha_presentacion
   AND    tipo = 12

   IF SQLCA.SQLCODE <> 0 THEN
}
      INSERT INTO glo_folio VALUES (0)  # folio de liquidacion

      SELECT MAX(folio)
      INTO vfolio
      FROM glo_folio

      INSERT INTO taa_folio VALUES(vfolio,12,vfecha_presentacion,g_usuario)

    --END IF

   DISPLAY BY NAME vfolio
   DISPLAY BY NAME vfecha_presentacion

END FUNCTION


FUNCTION genera_det_tras_afo()
#-----------------------------

   DEFINE
      vcurp_fovissste    CHAR(18)

   DEFINE
      fec_conv           DATE
      
   DEFINE
      vprecio_accion     DECIMAL(22,14),
      vcont_servicio     DECIMAL(10,0) ,
      p_accion           DECIMAL(19,14),
      lacc_liq14         DECIMAL(18,6) ,
      lacc_liq35         DECIMAL(18,6)

   DEFINE
      vtipo_movimiento   ,
      b_accion           ,
      lband              ,
      lsubcta            ,
      lb_saldo14         ,
      lb_saldo35         SMALLINT


   LET tot_nss_rechazo    = 0
   LET tot_pesos_rechazo  = 0

   DECLARE cur_1 CURSOR FOR
   SELECT *
   FROM safre_tmp:det_tra_acr_iss

   FOREACH cur_1 INTO reg_det_tra_acr_iss.*
      LET lb_saldo14 = TRUE
      LET lb_saldo35 = TRUE
      
      IF reg_det_tra_acr_iss.tipo_movimiento = 1 OR
         reg_det_tra_acr_iss.tipo_movimiento = 2 THEN

         CALL valida_curp_nss (reg_det_tra_acr_iss.nss_fovissste,
                               reg_det_tra_acr_iss.curp_fovissste) 
                               RETURNING vnss_afore,vcurp_fovissste,lband
         IF lband = FALSE THEN
            UPDATE safre_tmp:det_tra_acr_iss              #valor curp y/o nss INCORRECTO
            SET    estado  = 5
            WHERE  nss_fovissste  = reg_det_tra_acr_iss.nss_fovissste
            AND    curp_fovissste = reg_det_tra_acr_iss.curp_fovissste
            AND    cont_servicio  = reg_det_tra_acr_iss.cont_servicio
         ELSE  
                        # Actualizando el valor del NSS y/o CURP 
            UPDATE safre_tmp:det_tra_acr_iss 
            SET    nss_fovissste        = vnss_afore,
                   curp_fovissste       = vcurp_fovissste
            WHERE  nss_fovissste        = reg_det_tra_acr_iss.nss_fovissste
            AND    curp_fovissste       = reg_det_tra_acr_iss.curp_fovissste
            AND    cont_servicio        = reg_det_tra_acr_iss.cont_servicio
            AND    fecha_presentacion   = reg_det_tra_acr_iss.fecha_presentacion
            AND    ident_lote_solicitud = reg_det_tra_acr_iss.ident_lote_solicitud
            
            # Validando saldo subcuenta 14
            IF reg_det_tra_acr_iss.aivs_92 > 0 THEN
               LET lsubcta = 14
               CALL valida_monto_acciones( vnss_afore,
                                           reg_det_tra_acr_iss.aivs_92,
                                           lsubcta )
               RETURNING lacc_liq14, lb_saldo14
               
               LET reg_det_tra_acr_iss.aivs_92        = lacc_liq14 * (-1)
               LET reg_det_tra_acr_iss.importe_fov_92 = (reg_det_tra_acr_iss.aivs_92 * reg_det_tra_acr_iss.valor_aivs_92) 
            END IF            
            # Validando saldo subcuenta 35
            IF reg_det_tra_acr_iss.aivs_08 > 0 THEN
               LET lsubcta = 35
               CALL valida_monto_acciones( vnss_afore,
                                           reg_det_tra_acr_iss.aivs_08,
                                           lsubcta )
               RETURNING lacc_liq35, lb_saldo35
               
               LET reg_det_tra_acr_iss.aivs_08        = lacc_liq35 * (-1)
               LET reg_det_tra_acr_iss.importe_fov_08 = reg_det_tra_acr_iss.aivs_08 * reg_det_tra_acr_iss.valor_aivs_08
            END IF
            
            # Validando saldo Cero
            IF (reg_det_tra_acr_iss.importe_fov_92 = 0 AND 
                reg_det_tra_acr_iss.importe_fov_08 = 0 ) THEN
               
               UPDATE safre_tmp:det_tra_acr_iss
               SET   estado  = 2
               WHERE nss_fovissste  = reg_det_tra_acr_iss.nss_fovissste
               AND   curp_fovissste = reg_det_tra_acr_iss.curp_fovissste
            ELSE  # IF Saldo cero
               # Se valida que se tenga saldo suficiente en la subcuenta 14 y 35
               IF lb_saldo14 AND lb_saldo35 THEN
                  LET fec_conv = vfecha_liquida
                  # Provisionando el movimiento de la subcuenta 14
                  IF reg_det_tra_acr_iss.importe_fov_92 <> 0 THEN
                     INSERT INTO dis_provision
                     VALUES
                     (231,                                       #tipo_movimiento
                      14,                                        #subcuenta
                      12,                                        #siefore
                      vfolio,                                    #folio
                      reg_det_tra_acr_iss.cont_servicio,         #consecutivo_lote
                      vnss_afore,                                #nss
                      vcurp_fovissste,                           #curp
                      "FOV",                                     #folio_sua
                      vfecha_liquida,                            #fecha_pago
                      hoy2,                                      #fecha_valor
                      fec_conv,                                  #fecha_conversion
                      reg_det_tra_acr_iss.importe_fov_92,        #monto_en_pesos
                      reg_det_tra_acr_iss.aivs_92,               #monto_en_acciones
                      reg_det_tra_acr_iss.valor_aivs_92,         #precio_accion
                      0,                                         #dias_cotizados
                      "",                                        #sucursal
                      "ACR-ISSSTE",                              #id_aportante
                      5,                                         #estado
                      TODAY,                                     #fecha_proceso
                      g_usuario,                                 #usuario
                      reg_det_tra_acr_iss.fecha_presentacion,    #fecha_archivo
                      0 )                                        #etiqueta
                  END IF 
                  
                  # Provisionando el movimiento de la subcuenta 35
                  IF reg_det_tra_acr_iss.importe_fov_08 <> 0 THEN
                     INSERT INTO dis_provision
                     VALUES
                     (231,                                       #tipo_movimiento
                      35,                                        #subcuenta
                      12,                                        #siefore
                      vfolio,                                    #folio
                      reg_det_tra_acr_iss.cont_servicio,         #consecutivo_lote
                      vnss_afore,                                #nss
                      vcurp_fovissste,                           #curp
                      "FOV",                                     #folio_sua
                      vfecha_liquida,                            #fecha_pago
                      hoy2,                                      #fecha_valor
                      fec_conv,                                  #fecha_conversion
                      reg_det_tra_acr_iss.importe_fov_08,        #monto_en_pesos
                      reg_det_tra_acr_iss.aivs_08,               #monto_en_acciones
                      reg_det_tra_acr_iss.valor_aivs_08,         #precio_accion
                      0,                                         #dias_cotizados
                      "",                                        #sucursal
                      "ACR-ISSSTE",                              #id_aportante
                      5,                                         #estado
                      TODAY,                                     #fecha_proceso
                      g_usuario,                                 #usuario
                      reg_det_tra_acr_iss.fecha_presentacion,    #fecha_archivo
                      0 )                                        #etiqueta
                  END IF
               ELSE  # IF Saldo Insuficiente
                  UPDATE safre_tmp:det_tra_acr_iss        #aivs INCORRECTAS
                  SET   estado  = 6
                  WHERE nss_fovissste  = reg_det_tra_acr_iss.nss_fovissste
                  AND   curp_fovissste = reg_det_tra_acr_iss.curp_fovissste
               END IF   # IF Saldo Insuficiente   
            END IF   # IF Saldo cero 
         END IF   # IF CURP 
      ELSE   # IF Tipo de Movimiento
         UPDATE safre_tmp:det_tra_acr_iss            #Tipo movimiento INCORRECTO
         SET   estado  = 3
         WHERE nss_fovissste  = reg_det_tra_acr_iss.nss_fovissste
         AND   curp_fovissste = reg_det_tra_acr_iss.curp_fovissste
      END IF # IF Tipo de Movimiento
   END FOREACH

   WHILE TRUE
      ERROR""
      PROMPT "DESEA CONTINUAR CON LA LIQUIDACION S/N?" FOR enter

      IF enter <> "S" AND
         enter <> "s" AND
         enter <> "n" AND
         enter <> "N" THEN

         ERROR "SOLO INDIQUE S o N "
         SLEEP 3
         ERROR ""
         CONTINUE WHILE
      ELSE
         IF enter = "N" OR
            enter = "n" THEN

            ERROR "PROCESO CANCELADO."
            SLEEP 3
            ERROR ""

            DELETE
            FROM dis_provision
            WHERE folio = vfolio

            EXIT PROGRAM
         END IF

         EXIT WHILE
      END IF
   END WHILE

   ERROR"PROCESANDO LIQUIDACION"

   DECLARE cur_marcaje CURSOR FOR
   SELECT curp_fovissste,
          cont_servicio,
          nss_fovissste,
          tipo_movimiento
   FROM safre_tmp:det_tra_acr_iss
   WHERE estado = 0

   FOREACH cur_marcaje INTO vcurp_fovissste,
                            vcont_servicio,
                            vnss_afore,
                            vtipo_movimiento

      IF vtipo_movimiento = 1 THEN

         SELECT "X"
         FROM cta_act_marca
         WHERE nss = vnss_afore
         AND marca_cod = edo_proc

         IF SQLCA.SQLCODE <> 0 THEN
            LET vmarca_estado   = 0
            LET vcodigo_rechazo = 0

            LET ejecuta = "EXECUTE PROCEDURE marca_cuenta(",
                          "'",vnss_afore,"'",
                          ",",edo_proc,
                          ",",vcont_servicio,
                          ",",vmarca_estado,
                          ",",vcodigo_rechazo,
                          ",",pmarca_causa,
                          ",","'","'",",",
                          "'",g_usuario,"'",")"

            LET ejecuta = ejecuta CLIPPED

            PREPARE clausula_spl FROM ejecuta
            DECLARE cursor_marca CURSOR FOR clausula_spl

            OPEN cursor_marca

            FETCH cursor_marca INTO xcodigo_marca, xcodigo_rechazo

            CLOSE cursor_marca

            UPDATE safre_tmp:det_tra_acr_iss
            SET    estado         = xcodigo_rechazo
            WHERE  curp_fovissste = vcurp_fovissste

         END IF
      END IF
   END FOREACH

   DECLARE cur_ins_discta CURSOR FOR
   SELECT p.*
   FROM dis_provision p
   WHERE p.folio = vfolio

   FOREACH cur_ins_discta INTO reg_aboctas.*

      CALL valida_suficiencia(reg_aboctas.nss,
                              reg_aboctas.subcuenta,
                              reg_aboctas.monto_en_acciones)

      INSERT INTO dis_cuenta
      VALUES (reg_aboctas.*)
   END FOREACH 

   ERROR "LIQUIDACION TERMINADA"
   SLEEP 2

END FUNCTION


FUNCTION valida_monto_acciones( v_nss, v_accs, v_subcta )
#--------------------------------------------------------
   DEFINE
      v_nss           CHAR(11)
   
   DEFINE
      v_accs          DECIMAL(25,14),
      v_subcta        SMALLINT
	    
   DEFINE
      ldiff           DECIMAL(18,6)

   DEFINE
      reg_prov        RECORD
                         nss                 LIKE dis_cuenta.nss,
                         subcuenta           LIKE dis_cuenta.subcuenta,
                         monto_en_acciones   LIKE dis_cuenta.monto_en_acciones
                      END RECORD

   SELECT c.nss, c.subcuenta, NVL( SUM( c.monto_en_acciones ), 0 )
   INTO  reg_prov.*
   FROM  dis_cuenta c
   WHERE c.nss = v_nss
   AND   c.subcuenta = v_subcta
   GROUP BY 1,2
   
   IF reg_prov.monto_en_acciones IS NULL THEN
      LET reg_prov.monto_en_acciones = 0
   END IF
    
   LET ldiff = reg_prov.monto_en_acciones - v_accs

   IF ldiff < -1 THEN
      DISPLAY "NSS ",v_nss," CON DIFERENCIA :",ldiff
   END IF

   IF ldiff = 0 THEN
      RETURN v_accs, TRUE
   END IF

   IF ldiff > 0 THEN
      IF ldiff > 1 THEN
         RETURN v_accs, TRUE
      ELSE
         RETURN reg_prov.monto_en_acciones, TRUE
      END IF
   END IF

   IF ldiff < 0 THEN
      LET ldiff = ldiff *(-1)

      IF ldiff > 1 THEN
         RETURN reg_prov.monto_en_acciones, FALSE
      ELSE
         RETURN reg_prov.monto_en_acciones, TRUE
      END IF
   END IF
END FUNCTION


FUNCTION genera_historicos()
#---------------------------

   DEFINE reg_det RECORD LIKE safre_tmp:det_tra_acr_iss.*
   DEFINE reg_cza RECORD LIKE safre_tmp:cza_tra_acr_iss.*
   DEFINE reg_sum RECORD LIKE safre_tmp:sum_tra_acr_iss.*

   DEFINE
      vtotal_reg   INTEGER

   DECLARE cursor_cza CURSOR FOR
   SELECT *
   FROM   safre_tmp:cza_tra_acr_iss

   FOREACH cursor_cza INTO reg_cza.*
      SELECT "X"
      FROM   acr_cza_ced_issste
      WHERE  folio = vfolio
      AND    fecha_presentacion = reg_cza.fecha_presentacion

      IF STATUS = NOTFOUND THEN
         INSERT INTO acr_cza_ced_issste VALUES (vfolio, reg_cza.*)
      END IF
   END FOREACH

   DECLARE cursor_sum CURSOR FOR
   SELECT *
   FROM   safre_tmp:sum_tra_acr_iss

   FOREACH cursor_sum INTO reg_sum.*
      SELECT "X"
      FROM   acr_sum_ced_issste
      WHERE  folio = vfolio
      GROUP BY 1

      IF STATUS = NOTFOUND THEN
         INSERT INTO acr_sum_ced_issste
         VALUES (vfolio, reg_sum.*)
      END IF
   END FOREACH

   DECLARE cursor_det CURSOR FOR
   SELECT *
   FROM safre_tmp:det_tra_acr_iss
   #WHERE estado = 0                # CPL-1809

   FOREACH cursor_det INTO reg_det.*
      SELECT "X"
      FROM   acr_det_ced_issste
      WHERE  folio = vfolio
      AND    nss_fovissste = reg_det.nss_fovissste
      AND    fecha_presentacion = reg_det.fecha_presentacion
      AND    cont_servicio = reg_det.cont_servicio 

      IF STATUS = NOTFOUND THEN

         LET reg_det.estado = 1

         INSERT INTO acr_det_ced_issste
         VALUES (vfolio, reg_det.*)

      END IF

   END FOREACH

   SELECT COUNT(*)
   INTO   vtotal_reg
   FROM   safre_tmp:det_tra_acr_iss

   INSERT INTO acr_ctr_arh
   VALUES(generar,vtotal_reg,vtotal_reg,0,0,today,g_usuario)

END FUNCTION


FUNCTION genera_reporte()
#------------------------

   DEFINE reg_liquida RECORD
      nss                LIKE dis_cuenta.nss,
      curp               LIKE dis_cuenta.curp,
      subcuenta          LIKE dis_cuenta.subcuenta,
      fecha_valor        LIKE dis_cuenta.fecha_valor,
      fecha_conversion   LIKE dis_cuenta.fecha_conversion,
      monto_en_acciones  LIKE dis_cuenta.monto_en_acciones,
      monto_en_pesos     LIKE dis_cuenta.monto_en_pesos
   END RECORD

   DEFINE reg_det RECORD LIKE safre_tmp:det_tra_acr_iss.*

   DEFINE
      vnss               CHAR(11),
      vcurp_fovissste    CHAR(18)

   DECLARE cur_gene_rep CURSOR FOR
   SELECT t.*
   FROM safre_tmp:det_tra_acr_iss t
   WHERE t.estado = 0

   LET g_lista = g_seg_modulo.ruta_listados CLIPPED, "/",
                 g_usuario CLIPPED,
                 ".LIQ_ACRED_ISS.",hoy USING "DDMMYY","_",
                 hora[1,2],hora[4,5]

   START REPORT reporte_1 TO g_lista

   FOREACH cur_gene_rep INTO reg_det.*
      LET band = FALSE

      DECLARE cursor01 CURSOR FOR
      SELECT a.nss,
             a.curp,
             a.subcuenta,
             a.fecha_valor,
             a.fecha_conversion,
             a.monto_en_acciones,
             a.monto_en_pesos
      INTO   reg_liquida.*
      FROM   dis_cuenta a
      WHERE  a.nss = vnss_afore
      AND    a.curp = vcurp_fovissste
      AND    a.folio = vfolio
      AND    a.subcuenta IN ( 14, 35 )
      AND    a.tipo_movimiento = 231
      ORDER BY 1, 3

      FOREACH cursor01 INTO reg_liquida.*
         OUTPUT TO REPORT reporte_1( reg_liquida.* )
      END FOREACH

      FREE cursor01
   END FOREACH

   FINISH REPORT reporte_1

   DECLARE cur_rep_rech CURSOR FOR
   SELECT t.*
   FROM safre_tmp:det_tra_acr_iss t
   WHERE t.estado <> 0

   LET g_lista = g_seg_modulo.ruta_listados CLIPPED, "/", g_usuario CLIPPED,
                 ".RECH_ACR_ISS.",hoy USING "DDMMYY", "_", hora[1,2],hora[4,5]

   START REPORT rep_rechazados TO g_lista

   FOREACH cur_rep_rech INTO reg_det.*
      OUTPUT TO REPORT rep_rechazados( reg_det.* )
   END FOREACH

   FINISH REPORT rep_rechazados

END FUNCTION


REPORT reporte_1( reg_liq )
#--------------------------


   DEFINE reg_liq RECORD
      nss                 LIKE dis_cuenta.nss,
      curp                LIKE dis_cuenta.curp,
      subcuenta           LIKE dis_cuenta.subcuenta,
      fecha_valor         LIKE dis_cuenta.fecha_valor,
      fecha_conversion    LIKE dis_cuenta.fecha_conversion,
      monto_en_acciones   LIKE dis_cuenta.monto_en_acciones,
      monto_en_pesos      LIKE dis_cuenta.monto_en_pesos
   END RECORD

   DEFINE
      tot_reg             INTEGER

   OUTPUT
      PAGE LENGTH 60
      LEFT MARGIN 0
      RIGHT MARGIN 150
      TOP MARGIN 0
      BOTTOM MARGIN 0

   FORMAT
      PAGE HEADER
         SELECT COUNT(*)
         INTO tot_reg
         FROM acr_det_ced_issste d
         WHERE d.folio = vfolio AND d.estado = 1

         PRINT

         SKIP 2 LINES

         PRINT COLUMN 95, hoy USING "DD/MM/YYYY"
         PRINT
         PRINT COLUMN 10, "CVE. ", s_codigo_afore USING "&&&", "      ", desc_afo CLIPPED
         PRINT "FOLIO TRANSFERENCIA: ", COLUMN 49, vfolio
         PRINT "FECHA LIQUIDACION  : ", COLUMN 50, reg_liq.fecha_conversion
         PRINT "------------------------------------------------------------------------------------------------------------"
         PRINT COLUMN 24, "REPORTE DE REGISTROS LIQUIDADOS PROCESO ACREDITADOS FOVISSSTE"
         PRINT "------------------------------------------------------------------------------------------------------------"
         PRINT COLUMN 06, "NSS",
               COLUMN 21, "C U R P",
               COLUMN 42, "SUBCUENTA",
               COLUMN 64, "MONTO PESOS",
               COLUMN 93, "MONTO ACCIONES"
         PRINT "------------------------------------------------------------------------------------------------------------"

         SKIP 1 LINES

      ON EVERY ROW
         IF reg_liq.subcuenta = 14 THEN
            PRINT COLUMN 02, reg_liq.nss,
                  COLUMN 16, reg_liq.curp,
                  COLUMN 46, reg_liq.subcuenta,
                  COLUMN 60, reg_liq.monto_en_pesos USING "$$$$$$$$$&.&&&&&&",
                  COLUMN 91, reg_liq.monto_en_acciones USING "$$$$$$$$$&.&&&&&&"

                  LET band = TRUE
         ELSE
            IF band THEN
               PRINT COLUMN 60, reg_liq.monto_en_pesos USING "$$$$$$$$$&.&&&&&&",
                     COLUMN 91, reg_liq.monto_en_acciones USING "$$$$$$$$$&.&&&&&&"
            ELSE
               PRINT COLUMN 02, reg_liq.nss,
                     COLUMN 16, reg_liq.curp,
                     COLUMN 46, reg_liq.subcuenta,
                     COLUMN 60, reg_liq.monto_en_pesos USING "$$$$$$$$$&.&&&&&&",
                     COLUMN 91, reg_liq.monto_en_acciones USING "$$$$$$$$$&.&&&&&&"
            END IF
         END IF

         AFTER GROUP OF reg_liq.nss
            LET band = FALSE

      ON LAST ROW
         PRINT "------------------------------------------------------------------------------------------------------------"
         PRINT "      TOTAL:",
               COLUMN 60, SUM(reg_liq.monto_en_pesos) USING "$$$$$$$$$&.&&&&&&",
               COLUMN 91, SUM(reg_liq.monto_en_acciones) USING "$$$$$$$$$&.&&&&&&"
         PRINT
         PRINT COLUMN 2, "Total registros liquidados : ", tot_reg

END REPORT


REPORT rep_rechazados( reg )
#---------------------------

   DEFINE reg RECORD LIKE safre_tmp:det_tra_acr_iss.*

   DEFINE
      tot_reg   INTEGER

   OUTPUT
      PAGE LENGTH 60
      LEFT MARGIN 0
      RIGHT MARGIN 120
      TOP MARGIN 0
      BOTTOM MARGIN 0

   FORMAT
      PAGE HEADER

         SELECT COUNT(*)
         INTO tot_reg
         FROM  safre_tmp:det_tra_acr_iss
         WHERE estado <> 0

         PRINT

         SKIP 1 LINES

         PRINT
               COLUMN 85, hoy USING "DD/MM/YYYY"
         PRINT
         PRINT COLUMN 5, "CVE. ", s_codigo_afore USING "&&&","      ", desc_afo CLIPPED
         PRINT "---------------------------------------------------------------------------------------------------------"
         PRINT "                  REPORTE DE REGISTROS RECHAZADOS PROCESO ACREDITADOS FOVISSSTE"
         PRINT "---------------------------------------------------------------------------------------------------------"
         PRINT COLUMN 05, "NSS",
               COLUMN 21, "CURP",
               COLUMN 39, "MONTO PESOS FOV-92",
               COLUMN 61, "MONTO PESOS FOV-08",
               COLUMN 86, "ESTADO DE RECHAZO"
         PRINT "---------------------------------------------------------------------------------------------------------"

         SKIP 1 LINES

      ON EVERY ROW
         PRINT COLUMN 02, reg.nss_fovissste,
               COLUMN 16, reg.curp_fovissste,
               COLUMN 40, reg.importe_fov_92 USING "$$$$$$$$$&.&&&&&&",
               COLUMN 62, reg.importe_fov_08 USING "$$$$$$$$$&.&&&&&&",
               COLUMN 93, reg.estado


      ON LAST ROW
         PRINT "---------------------------------------------------------------------------------------------------------"
         PRINT "       TOTAL:",
               COLUMN 40, SUM(reg.importe_fov_92) USING "$$$$$$$$$&.&&&&&&",
               COLUMN 62, SUM(reg.importe_fov_08) USING "$$$$$$$$$&.&&&&&&"
         PRINT
         PRINT COLUMN 2, "Total registros rechazados : ", tot_reg

END REPORT


FUNCTION valida_curp_nss(valida_nss, valida_curp)
#------------------------------------------------

   DEFINE
      valida_nss    CHAR(11),
      valida_curp   CHAR(18),
      regre_nss     CHAR(11),
      regre_curp    CHAR(18)
   
   DEFINE
      bfound        SMALLINT

   INITIALIZE regre_nss  TO NULL
   INITIALIZE regre_curp TO NULL
                
   IF valida_nss = '           ' OR valida_nss = '' THEN
      INITIALIZE valida_nss  TO NULL
   END IF
   IF valida_curp = '                  ' OR valida_curp = '' THEN
      INITIALIZE valida_curp  TO NULL
   END IF
   
   
   IF valida_nss IS NULL AND 
      valida_curp IS NOT NULL THEN

      SELECT a.n_seguro
      INTO   regre_nss
      FROM   afi_mae_afiliado a
      WHERE  a.n_unico = valida_curp
      
      IF regre_nss IS NULL OR regre_nss = "" OR regre_nss = "           " THEN
         ERROR "EL CURP ",valida_curp," NO TIENE ASOCIADO UN NSS CORRECTO !!"
         RETURN valida_nss, valida_curp, FALSE
      ELSE
         RETURN regre_nss, valida_curp, TRUE
      END IF
   END IF

   IF valida_nss IS NOT NULL AND
      valida_curp IS NULL THEN

      SELECT a.n_unico
      INTO   regre_curp
      FROM   afi_mae_afiliado a
      WHERE  a.n_seguro = valida_nss
      
      IF regre_curp IS NULL OR regre_curp = "" OR regre_curp = "           " THEN
         ERROR "EL NSS ",valida_nss," NO TIENE ASOCIADO UN CURP CORRECTO !!"
         RETURN valida_nss,valida_curp, FALSE
      ELSE
         RETURN valida_nss,regre_curp, TRUE
      END IF
   END IF

   IF valida_nss IS NOT NULL AND
      valida_curp IS NOT NULL THEN
      
      CALL valida_curp_nss (valida_nss,regre_curp) 
           RETURNING regre_nss, regre_curp, bfound
           
      IF bfound = FALSE THEN
         RETURN valida_nss,valida_curp, FALSE
      ELSE 
         IF valida_curp <> regre_curp THEN
            ERROR "EL CURP ",valida_curp," NO COINSIDE CON EL REGISTRADO EN EL MAESTRO DE AFILIADOS ", regre_curp, " PARA EL NSS ", valida_nss
            RETURN valida_nss,valida_curp, FALSE
         ELSE
            RETURN regre_nss, regre_curp, TRUE
         END IF
      END IF
   END IF

   IF valida_nss IS NULL AND 
      valida_curp IS NULL THEN #Si CURP y NSS no tiene valor

       PROMPT "El archivo no contiene CURP, ni NSS. [Enter] para salir" FOR enter
       EXIT PROGRAM
   END IF

END FUNCTION


FUNCTION valida_suficiencia(val_suf)
#-----------------------------------

   DEFINE val_suf RECORD
      nss               CHAR(11),
      subcuenta         SMALLINT,
      monto_acciones    DECIMAL(16,6)
   END RECORD

   DEFINE
      vmonto_acciones   DECIMAL(16,6)

   LET vmonto_acciones = 0

   SELECT NVL(SUM(A.monto_en_acciones),0)
   INTO   vmonto_acciones
   FROM   dis_cuenta A
   WHERE  A.nss = val_suf.nss
   AND    A.subcuenta = val_suf.subcuenta

    IF val_suf.monto_acciones > vmonto_acciones THEN
       PROMPT " INSUFICIENCIA DE SALDO PARA EL NSS: ",val_suf.nss
       FOR CHAR enter
       EXIT PROGRAM
    END IF

END FUNCTION