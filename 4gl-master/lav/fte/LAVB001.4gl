################################################################################
#Project           => SAFRE (Mexico)                                           #
#Owner             => E.F.P.                                                   #
#Programa          => Proceso lavado de dinero                                 #
#Fecha creacion    => 31 de octubre de 2002                                    #
#Por               => MIGUEL ANGEL HERNANDEZ MARTINEZ                          #
#Modificado por    => OMAR SANDOVAL BADILLO                                    #
#Fecha modifi      => 02 de julio de 2004                                      #
#Fecha modifi      => 07 de julio de 2004                                      #
#Fecha modifi      => 22 de julio de 2004                                      #
#Por               => OMAR SANDOVAL BADILLO                                    #
#Fecha modifi      => 25 de agosto de 2004                                     #
#Fecha modifi      =< 27 de enero de 2005   #270105                            #
#Fecha modifi      =< 01 de febrero de 2005                                    #
#Fecha modifi      =< 16 de junio de 2005   #clasifica                         #
#Sistema           => LAV                                                      #
##CPL-1356         => Obtiene la nacionalidad según catálogo lav_det_lavado    #
#CPL-1595          => FSR 09/04/2014 SE AGREGAN LAS SUBCUENTAS 12,16,21,23,25  #
#                  => ,27,29  PARA LAS OPERACIONES RELEVANTES.                 #
################################################################################
DATABASE safre_af
GLOBALS
   DEFINE afi_nombres        CHAR(40),
          afi_paterno        CHAR(40),
          afi_materno        CHAR(40),
          afi_n_rfc          CHAR(13),
          afi_n_unico        CHAR(18),
          afi_fena           DATE,
          afi_n_folio        DECIMAL(11,0),
          afi_tipo_solicitud SMALLINT

   DEFINE reg_1              RECORD
          fecha_inicio       DATE,
          fecha_fin          DATE,
          lav_operacion      SMALLINT
   END RECORD

   DEFINE lav_oper_desc      CHAR(20),
          x_condicion        SMALLINT,
          x_condicion_desc   CHAR(20)

   DEFINE reg_5 ARRAY[500]   OF RECORD
          folio              DECIMAL(11,0),
          nss                CHAR(11),
          nombre             CHAR(20),
          tipo_operacion     CHAR(2),
          fecha_operacion    DATE,
          monto_en_pesos     DECIMAL(16,6)
   END RECORD

   DEFINE reg_7   ARRAY[500] OF RECORD
          trap               CHAR(1),
          folio              DECIMAL(11,0),
          nss                CHAR(11),
          nombre             CHAR(20),
          tipo_operacion     CHAR(2),
          fecha_operacion    DATE,
          monto_en_pesos     DECIMAL(16,6),
          condicion_cod      SMALLINT,
          estado             SMALLINT
   END RECORD

   DEFINE reg_8   ARRAY[30000] OF RECORD
          folio              DECIMAL(11,0),
          nss                CHAR(11),
          nombre             CHAR(20),
          tipo_operacion     CHAR(2),
          fecha_operacion    DATE,
          monto_en_pesos     DECIMAL(16,6),
          condicion_cod      SMALLINT,
          seleccion          CHAR(1),
          sele               CHAR(1)
   END RECORD

   DEFINE reg_2 ARRAY[30000]   OF RECORD
          folio              LIKE lav_det_lavado.folio,
          nss                LIKE lav_det_lavado.nss,
          consecutivo_reg    LIKE lav_det_lavado.consecutivo_reg,
          tipo_reporte       LIKE lav_det_lavado.tipo_reporte,
          periodo_reporte    LIKE lav_det_lavado.periodo_reporte,
          org_supervisor     LIKE lav_det_lavado.org_supervisor,
          clave_financiera   LIKE lav_det_lavado.clave_financiera,
          localidad          LIKE lav_det_lavado.localidad,
          sucursal           LIKE lav_det_lavado.sucursal,
          tipo_operacion     LIKE lav_det_lavado.tipo_operacion,
          instrumento_mone   LIKE lav_det_lavado.instrumento_mone,
          monto_en_pesos     LIKE lav_det_lavado.monto_en_pesos,
          moneda             LIKE lav_det_lavado.moneda,
          fecha_operacion    LIKE lav_det_lavado.fecha_operacion,
          fecha_deteccion    LIKE lav_det_lavado.fecha_deteccion,
          nacionalidad       LIKE lav_det_lavado.nacionalidad,
          tipo_persona       LIKE lav_det_lavado.tipo_persona,
          actividad_eco      LIKE lav_det_lavado.actividad_eco,
          condicion_cod      LIKE lav_det_lavado.condicion_cod,
          estado             LIKE lav_det_lavado.estado,
          usuario            LIKE lav_det_lavado.usuario,
          factualiza         LIKE lav_det_lavado.factualiza
   END RECORD

   DEFINE reg_9 ARRAY[5000]   OF RECORD
          trap               CHAR(1),
          folio              LIKE lav_det_lavado.folio,
          nss                LIKE lav_det_lavado.nss,
          consecutivo_reg    LIKE lav_det_lavado.consecutivo_reg,
          tipo_reporte       LIKE lav_det_lavado.tipo_reporte,
          periodo_reporte    LIKE lav_det_lavado.periodo_reporte,
          org_supervisor     LIKE lav_det_lavado.org_supervisor,
          clave_financiera   LIKE lav_det_lavado.clave_financiera,
          localidad          LIKE lav_det_lavado.localidad,
          sucursal           LIKE lav_det_lavado.sucursal,
          tipo_operacion     LIKE lav_det_lavado.tipo_operacion,
          instrumento_mone   LIKE lav_det_lavado.instrumento_mone,
          monto_en_pesos     LIKE lav_det_lavado.monto_en_pesos,
          moneda             LIKE lav_det_lavado.moneda,
          fecha_operacion    LIKE lav_det_lavado.fecha_operacion,
          fecha_deteccion    LIKE lav_det_lavado.fecha_deteccion,
          nacionalidad       LIKE lav_det_lavado.nacionalidad,
          tipo_persona       LIKE lav_det_lavado.tipo_persona,
          actividad_eco      LIKE lav_det_lavado.actividad_eco,
          condicion_cod      LIKE lav_det_lavado.condicion_cod,
          estado             LIKE lav_det_lavado.estado,
          usuario            LIKE lav_det_lavado.usuario,
          factualiza         LIKE lav_det_lavado.factualiza
   END RECORD

   DEFINE reg_6              RECORD
          lav_operacion      SMALLINT,
          deteccion_fini     DATE,
          deteccion_ffin     DATE,
          periodo_reporte    CHAR(8)
          #folio              INTEGER
   END RECORD

   DEFINE g_reg RECORD LIKE lav_det_lavado.*

   DEFINE total_reg          SMALLINT,
          total_monto        DECIMAL(16,6)

   DEFINE cla_where          CHAR(100),
          sel_where          CHAR(100),
          pos                SMALLINT,
          x_folio            DECIMAL(11,0),
          vruta_envio        CHAR(40),
          vruta_listados     CHAR(40)

   DEFINE reg                RECORD
          hora               CHAR(6)
   END RECORD

   DEFINE arr_c              SMALLINT,
          arr_l              SMALLINT,
          arr_t              SMALLINT,
          i                  SMALLINT,
          ii                 SMALLINT,
          totala             INTEGER,
          totalc             DECIMAL(16,6),
          totald             DECIMAL(16,6)

   DEFINE HOY                DATE

   DEFINE opc                CHAR(01),
          vmenos             SMALLINT

   DEFINE vprecio_accion     DECIMAL(16,6),
          consecutivo        SMALLINT

   DEFINE txt_sel            CHAR(400),
          txt_pdq            CHAR(100),
          g_impre            CHAR(200),
          x_status           CHAR(1),
          dias               SMALLINT,
          x_operacion        CHAR(1),
          x_periodo          CHAR(6),
          x_fecha            DATE,
          x_sw               SMALLINT,
          g_lista            CHAR(300),
          hora               CHAR(08),
          sw_1               SMALLINT

   DEFINE errvar             CHAR(250)

   DEFINE ga_1 ARRAY[5000]    OF RECORD
          trap               CHAR(1),
          folio              LIKE  lav_det_lavado.folio,
          nss                LIKE  lav_det_lavado.nss,
          nombres            LIKE  afi_mae_afiliado.nombres,
          tipo_operacion     LIKE  lav_det_lavado.tipo_operacion,
          fecha_movimiento   LIKE  lav_det_lavado.fecha_operacion,
          monto_en_pesos     LIKE  lav_det_lavado.monto_en_pesos,
          condicion_cod      LIKE  lav_det_lavado.condicion_cod,
          estado             LIKE  lav_det_lavado.estado
   END RECORD

   DEFINE ga_2 ARRAY[1000]    OF RECORD
          --trapp              CHAR(1),
          tipo_movimiento    LIKE dis_cuenta.tipo_movimiento,
          fecha_conversion   LIKE dis_cuenta.fecha_conversion,
          monto_en_pesos     LIKE dis_cuenta.monto_en_pesos
   END RECORD

   DEFINE x_error             CHAR(500)

   DEFINE x_cod_afo           CHAR(3)

   DEFINE respuesta           CHAR(1)

   DEFINE vusuario            CHAR(08)

END GLOBALS
################################################################################
MAIN
   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY CONTROL-I
      DEFER INTERRUPT

   CALL STARTLOG(FGL_GETENV("USER")||".LAVB001.log")

   LET HOY = TODAY

   OPEN WINDOW ventana AT 2,2 WITH FORM "LAVB0011" ATTRIBUTE(BORDER)
   DISPLAY " LAVB001                    LAVADO DE DINERO                                 " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "dd/mm/yyyy " AT 3,67 ATTRIBUTE(REVERSE)
   DISPLAY " Detectar registros < ESC >                                                  " AT 4,1 ATTRIBUTE(REVERSE)

   CALL inicia()

   MENU " LAVADO DE DINERO"
      COMMAND "Detecta" " Detecta lavado de dinero por operacion"
         CALL proceso()
      COMMAND "Consulta" " Consulta lavado de dinero por operacion"
         CALL consulta()
{
      COMMAND KEY(L) "cLasificacion" " Consulta lavado de dinero por operacion"
         CALL clasifica()
}
      COMMAND KEY (E) "sEleccion" " Seleccion de registros de oper. Inusuales y Preocupantes"
         CALL seleccion()
      {COMMAND "Preocupantes" " Deteccion de registros de oper. Preocupantes de forma manual"
         CALL preocupante_manual()} #CPL-1467
      COMMAND "Genera archivo" " Generacion de archivo por operacion"
         CALL archivo()
      COMMAND "Reporte" " Reportes de lavado de dinero por operacion"
         CALL reporte()
      COMMAND "Salir" " Salir del Programa"
         EXIT MENU
   END MENU

   CLOSE WINDOW ventana
END MAIN
###############################################################################
FUNCTION inicia()

   INITIALIZE reg_1.* TO NULL
   INITIALIZE reg_6.* TO NULL
   INITIALIZE lav_oper_desc TO NULL

   DISPLAY BY NAME reg_1.*
   DISPLAY BY NAME lav_oper_desc

   SELECT codigo_afore
   INTO   x_cod_afo
   FROM   tab_afore_local

   SELECT ruta_envio,
          ruta_listados,
          USER
   INTO   vruta_envio,
          vruta_listados,
          vusuario
   FROM   seg_modulo
   WHERE  modulo_cod = "lav"

   LET txt_sel = " SELECT * ",
                 " FROM   lav_det_lavado ",
                 " WHERE  folio = ? ",
                 " AND    estado = 10 ",
                 " ORDER BY 2 "
   PREPARE query1 FROM txt_sel

   LET txt_sel = " SELECT nombres,",
                        " paterno,",
                        " materno,",
                        " n_rfc,",
                        " n_unico,",
                        " fena,",
                        " n_folio,",
                        " tipo_solicitud ",
                 " FROM   afi_mae_afiliado ",
                 " WHERE  n_seguro = ? "
   PREPARE query2 FROM txt_sel

   LET txt_sel = " SELECT folio,",
                        " tipo_movimiento,",
                        " nss,",
                        " SUM(monto_en_pesos),",
                        " fecha_conversion,",
                        " sucursal ",
                 " FROM   dis_cuenta ",
                 " WHERE  subcuenta IN(10,12,16,21,23,25,27,29) ",  #CPL-1595 
                 " AND    tipo_movimiento IN (1,10,490) ",
                 " AND    fecha_conversion BETWEEN ? AND ? ",
                 " AND    id_aportante[1,3] NOT IN ('IC-','MQ-','MC-','UM-','UQ-','MA-','MS-','AF-','AC-','UR-','US-','AA-','AS-','UY-','UZ-','COR')",
                 "GROUP BY 1,2,3,5,6",
                 " ORDER BY 3,5 "
                 
   PREPARE query3 FROM txt_sel

   LET txt_sel = " SELECT condicion_cod,",
                        " monto_desde,",
                        " monto_hasta,",
                        " moneda_cod ",
                 " FROM   lav_condicion ",
                 " WHERE  lav_reporte = ? ",
                 " ORDER BY 1 "
   PREPARE query4 FROM txt_sel

   LET txt_sel = " SELECT tipo_cambio ",
                 " FROM   tab_tipo_cambio ",
                 " WHERE  moneda_cod = ? ",
                 #" AND    fecha_aplica BETWEEN ? AND ? "
                 " AND    fecha_aplica = ? "
   PREPARE query5 FROM txt_sel

   LET txt_sel = " SELECT condicion_cod ",
                 " FROM   lav_det_lavado ",
                 " WHERE  folio = ? ",
                 " AND    nss = ? ",
                 " GROUP BY 1 "
   PREPARE query6 FROM txt_sel

   LET txt_sel = " SELECT nss,",
                        " tipo_operacion,",
                        " SUM(monto_en_pesos) ",
                 " FROM   lav_det_lavado ",
                 " WHERE  folio = ? ",
                 " AND    estado = 40 ",
                 " GROUP BY 1,2"
   PREPARE query7 FROM txt_sel

   LET txt_sel = " SELECT nss,",
                        " consecutivo_reg,",
                        " tipo_operacion,",
                        " condicion_cod ",
                 " FROM   lav_det_lavado ",
                 " WHERE  folio = ? ",
                 " ORDER BY 2 DESC "
   PREPARE query8 FROM txt_sel

   LET txt_sel = " INSERT INTO lav_det_lavado ",
                 " VALUES (?,",
                         " ?,",
                         " ?,",
                         " ?,",
                         " ?,",
                         " ?,",
                         " ?,",
                         " ?,",
                         " ?,",
                         " ?,",
                         " ?,",
                         " ?,",
                         " ?,",
                         " ?,",
                         " ?,",
                         " ?,",
                         " ?,",
                         " ?,",
                         " ?,",
                         " ?,",
                         " USER,",
                         " TODAY)"
   PREPARE query9 FROM txt_sel

   LET txt_sel = " UPDATE lav_det_lavado ",
                 " SET    estado = 10,",
                        " monto_en_pesos = ? ,",
                        " condicion_cod = ? ",
                 " WHERE  folio = ? ",
                 " AND    nss = ? ",
                 " AND    tipo_operacion = ? "
   PREPARE query10 FROM txt_sel

   LET txt_sel = " DELETE FROM lav_det_lavado ",
                 " WHERE  folio  = ? ",
                 " AND    estado = 40 "
   PREPARE query11 FROM txt_sel

   LET txt_sel = " UPDATE lav_det_lavado ",
                 " SET    consecutivo_reg = ? ",
                 " WHERE  folio = ? ",
                 " AND    nss = ? ",
                 " AND    consecutivo_reg = ? ",
                 " AND    condicion_cod = ? ",
                 " AND    tipo_operacion = ? "
   PREPARE query12 FROM txt_sel

   LET txt_sel = " SELECT calle,",
                        " numero,",
                        " depto,",
                        " codpos,",
                        " colonia,",
                        " ciudad,",
                        " delega,",
                        " estado ",
                 " FROM   afi_domicilio ",
                 " WHERE  nss = ? ",
                 " AND    n_folio = ? ",
                 " AND    tipo_solicitud = ? "
   PREPARE query13 FROM txt_sel

   LET txt_sel = " SELECT casfim_cod ",
                 " FROM   tab_local_casfim ",
                 " WHERE  deleg_cod = ? "
   PREPARE query14 FROM txt_sel

   LET txt_sel = " SELECT MIN(rowid) ",
                 " FROM   afi_telefono ",
                 " WHERE  nss = ? ",
                 " AND    n_folio = ? ",
                 " AND    tipo_solicitud = ? ",
                 " AND    tel_cod <> 7 "
   PREPARE query15 FROM txt_sel

   LET txt_sel = " SELECT 'X' ",
                 " FROM   lav_det_lavado ",
                 " WHERE  tipo_reporte = ? ",
                 " AND    periodo_reporte = ? ",
                 " GROUP BY 1 "
   PREPARE query16 FROM txt_sel

   LET txt_sel = " SELECT * ",                 #270105
                 " FROM   lav_det_lavado ",
                 " WHERE  tipo_reporte = ? ",
                 " AND    periodo_reporte[1,6] = ? ",
                 " AND    fecha_operacion BETWEEN ? AND ? ",
                 #" AND    folio = ? ",
                 " AND    estado IN (10,20) ",
                 " ORDER BY 2,3 "
   PREPARE query17 FROM txt_sel

   LET txt_sel = " UPDATE lav_det_lavado ",       #270105
                 " SET    estado = 30 ",
                 #" WHERE  folio = ? ",
                 " WHERE  tipo_reporte = ? ",
                 " AND    fecha_operacion BETWEEN ? AND ? ",
                 " AND    nss = ? ",
                 " AND    consecutivo_reg = ? "
   PREPARE query20 FROM txt_sel

   LET txt_sel = " SELECT descripcion ",
                 " FROM   lav_desc_oper ",
                 " WHERE  folio = ? ",
                 " AND    nss = ? ",
                 " AND    consecutivo_reg = ? ",
                 " AND    tipo_operacion = ? "
   PREPARE query25 FROM txt_sel

   LET txt_sel = " SELECT razones ",
                 " FROM   lav_razones_oper ",
                 " WHERE  folio = ? ",
                 " AND    nss = ? ",
                 " AND    consecutivo_reg = ? ",
                 " AND    tipo_operacion = ? "
   PREPARE query26 FROM txt_sel

   LET txt_sel = " SELECT numero_cuenta,",
                        " institucion,",
                        " nombre,",
                        " paterno,",
                        " materno ",
                 " FROM   lav_relacion_per ",
                 " WHERE  folio = ? ",
                 " AND    nss = ? ",
                 " AND    consecutivo_reg = ? ",
                 " AND    tipo_operacion = ? "
   PREPARE query27 FROM txt_sel

   LET txt_sel = " INSERT INTO lav_desc_oper ",
                 " VALUES (?,",
                         " ?,",
                         " ?,",
                         " ?,",
                         " ?,",
                         " ?,",
                         " USER,",
                         " TODAY) "
   PREPARE query28 FROM txt_sel

   LET txt_sel = " INSERT INTO lav_razones_oper ",
                 " VALUES (?,",
                         " ?,",
                         " ?,",
                         " ?,",
                         " ?,",
                         " ?,",
                         " USER,",
                         " TODAY) "
   PREPARE query29 FROM txt_sel

   LET txt_sel = " SELECT consecutivo_cuenta,",
                        " numero_cuenta,",
                        " institucion,",
                        " nombre,",
                        " paterno,",
                        " materno ",
                 " FROM   lav_relacion_per ",
                 " WHERE  folio = ? ",
                 " AND    nss = ? ",
                 " AND    consecutivo_reg = ? ",
                 " AND    tipo_operacion = ? "
   PREPARE query30 FROM txt_sel

   LET txt_sel = " SELECT id_aportante,",
                        " nss,",
                        " monto_en_pesos ",
                       #" SUM(monto_en_pesos) ",
                 " FROM   dis_cuenta ",
                 " WHERE  subcuenta = 10 ",
                 " AND    tipo_movimiento IN (1,10,490) ",
                 " AND    fecha_conversion BETWEEN ? AND ? ",
                 " AND    id_aportante[1,3] NOT IN ('IC-','MQ-','MC-','UM-','UQ-','MA-','MS-','AF-','AC-','UR-','US-','AA-','AS-','UY-','UZ-','COR')",
                 " GROUP BY 1,2,3 "
   PREPARE query31 FROM txt_sel

   LET txt_sel = " SELECT id_aportante,",
                        " nss,",
                        #" SUM(monto_en_pesos),",
                        " monto_en_pesos,",
                        " fecha_conversion",
                 " FROM   dis_cuenta ",
                 " WHERE  nss = ? ",
                 " AND    subcuenta = 10 ",
                 " AND    fecha_conversion BETWEEN ? AND ? ",
                 " AND    monto_en_pesos >= ? ",
                 " AND    id_aportante = ? ",
                 " GROUP BY 1,2,3,4 "
   PREPARE query32 FROM txt_sel

   LET txt_sel = " SELECT TRIM(telefono) ",
                 " FROM   afi_telefono ",
                 " WHERE  nss = ? ",
                 " AND    n_folio = ? ",
                 " AND    tipo_solicitud = ? ",
                 " AND    rowid = ? "
   PREPARE query33 FROM txt_sel

   LET txt_sel = " SELECT id_aportante,",
                        " nss,",
                        " SUM(monto_en_pesos),",
                        " fecha_conversion ",
                        #" sucursal ",
                 " FROM   dis_cuenta ",
                 " WHERE    subcuenta = 10 ",
                 " AND    tipo_movimiento IN (1,10,490) ",
                 " AND  fecha_conversion BETWEEN ? AND ? ",
                 " AND    id_aportante[1,3] NOT IN ('IC-','MQ-','MC-','UM-','UQ-','MA-','MS-','AF-','AC-','UR-','US-','AA-','AS-','UY-','UZ-','COR')",
                 " GROUP BY 1,2,4 "
   PREPARE query34 FROM txt_sel

   LET txt_sel = " SELECT MAX(fecha_recepcion) ",
                 " FROM   dis_det_aporte ",
                 " WHERE  n_seguro = ? "
   PREPARE query_sal1 FROM txt_sel

   LET txt_sel = " SELECT MAX(consec_reg_lote) ",
                 " FROM   dis_det_aporte ",
                 " WHERE  n_seguro = ? ",
                 " AND    fecha_recepcion = ? "
   PREPARE query_sal2 FROM txt_sel

   LET txt_sel = " SELECT (ult_salario_diario/100) * 30 ",
                 " FROM   dis_det_aporte ",
                 " WHERE  n_seguro = ? ",
                 " AND    consec_reg_lote = ? ",
                 " AND    fecha_recepcion = ? "
   PREPARE query_sal3 FROM txt_sel

   LET txt_pdq = "SET PDQPRIORITY HIGH"
   PREPARE pdq_1 FROM txt_pdq

   LET txt_pdq = "SET PDQPRIORITY LOW"
   PREPARE pdq_2 FROM txt_pdq

END FUNCTION
#############################################################################
FUNCTION proceso()

   LET int_flag = FALSE

   INPUT BY NAME reg_1.lav_operacion,
                 reg_1.fecha_inicio,
                 reg_1.fecha_fin  WITHOUT DEFAULTS

      AFTER FIELD lav_operacion
         IF reg_1.lav_operacion IS NULL THEN
            ERROR "Tipo de operacion no puede ser NULA"
            NEXT FIELD lav_operacion
         END IF
         ERROR ""

         SELECT reporte_desc
         INTO   lav_oper_desc
         FROM   tab_tipo_reporte
         WHERE  lav_operacion = reg_1.lav_operacion

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR "TIPO DE OPERACION NO EXISTE"
            NEXT FIELD lav_operacion
         END IF
         ERROR ""

         DISPLAY BY NAME lav_oper_desc

         NEXT FIELD fecha_inicio

      AFTER FIELD fecha_inicio
         IF reg_1.fecha_inicio IS NULL THEN
            ERROR "Fecha de inicio no puede ser NULA"
            NEXT FIELD fecha_inicio
         END IF
         ERROR ""

         #LET reg_1.fecha_inicio = MDY(MONTH(reg_1.fecha_inicio),1,YEAR(reg_1.fecha_inicio))

         CALL habil_siguiente(reg_1.fecha_inicio)
              RETURNING x_fecha

         IF reg_1.lav_operacion = 100 OR
            reg_1.lav_operacion = 200 THEN

            SELECT "X"
            FROM   tab_tipo_cambio
            WHERE  fecha_aplica = x_fecha

            IF SQLCA.SQLCODE <> 0 THEN
               ERROR "NO HAY TIPO DE CAMBIO PARA EL PERIODO DE CALCULO"
               NEXT FIELD fecha_inicio
            END IF
            ERROR ""
         END IF

        { CALL fin_mes(reg_1.fecha_inicio)
              RETURNING reg_1.fecha_fin,dias} #CPL-1595 Se omitio, ya que no tomaba la fecha si se colocaban tres meses, 
                                              # solo consideraba el ultimo día del mes de la fecha inicio

         DISPLAY BY NAME reg_1.fecha_fin

      AFTER FIELD fecha_fin
         IF reg_1.fecha_fin IS NULL THEN
            ERROR "Fecha de fin no puede ser NULA"
            NEXT FIELD fecha_fin
         END IF
         ERROR ""

         IF reg_1.lav_operacion = 100 THEN
            LET x_operacion = 1
         END IF

         IF reg_1.lav_operacion = 200 THEN
            LET x_operacion = 2
         END IF

         IF reg_1.lav_operacion = 300 THEN
            LET x_operacion = 3
         END IF

         LET x_periodo = reg_1.fecha_inicio USING "YYYYMM"

         IF reg_1.fecha_fin IS NOT NULL THEN
            DECLARE cursor_cue CURSOR FOR query16

            OPEN cursor_cue USING x_operacion,x_periodo

            FETCH cursor_cue INTO x_status
            CLOSE cursor_cue

            IF x_status IS NOT NULL THEN
               ERROR "YA HAY REGISTROS EN EL RANGO DE FECHAS CON LA OPERACION ...",lav_oper_desc
               SLEEP 3
               ERROR ""
               NEXT FIELD fecha_inicio
            END IF
         END IF

      ON KEY (ESC)
         IF reg_1.lav_operacion IS NULL THEN
            ERROR "Tipo de operacion no puede ser NULA"
            NEXT FIELD lav_operacion
         END IF
         ERROR ""

         IF reg_1.fecha_inicio IS NULL THEN
            ERROR "Fecha de inicio no puede ser NULA"
            NEXT FIELD fecha_inicio
         END IF
         ERROR ""

         IF reg_1.fecha_fin IS NULL THEN
            ERROR "Fecha de fin no puede ser NULA"
            NEXT FIELD fecha_fin
         END IF
         ERROR ""

         LET int_flag = FALSE
         EXIT INPUT

      ON KEY (CONTROL-C)
         LET int_flag = TRUE
         EXIT INPUT
   END INPUT

   IF int_flag = TRUE THEN
      LET int_flag = FALSE
      ERROR "BUSQUEDA CANCELADA..."

      INITIALIZE reg_1.* TO NULL
      DISPLAY BY NAME reg_1.*

      INITIALIZE lav_oper_desc TO NULL
      DISPLAY BY NAME lav_oper_desc

      CLEAR SCREEN
      RETURN
   END IF

   ERROR "PROCESANDO INFORMACION ... "

   CALL detecta(reg_1.fecha_inicio,
                reg_1.fecha_fin,
                reg_1.lav_operacion)
        RETURNING x_folio,x_sw

   IF x_sw = 1 THEN
      IF reg_1.lav_operacion = 100 THEN
         CALL despliega(x_folio)
      ELSE
         ERROR ""
         PROMPT " FOLIO: ",x_folio USING "<<<<<<","  OTORGADO PARA LA OPERACION ",lav_oper_desc CLIPPED ATTRIBUTE(REVERSE)
         FOR opc ATTRIBUTE (REVERSE)
      END IF
   ELSE
      DELETE
      FROM  lav_folio
      WHERE folio = x_folio

      ERROR ""

      PROMPT " NO HAY REGISTROS EN EL PERIODO CAPTURADO,PARA LA OPERACION ",lav_oper_desc CLIPPED ATTRIBUTE(REVERSE)
      FOR opc ATTRIBUTE(REVERSE)
   END IF

   CLEAR SCREEN
END FUNCTION
#######################################################################
FUNCTION despliega(x_folio)

   DEFINE x_folio     DECIMAL(11,0),
          i           SMALLINT

   OPEN WINDOW ventana1 AT 10,2 WITH FORM "LAVB0012"
   DISPLAY "               REGISTRO DETECTADOS EN OPERACION RELEVANTE                   " AT 1,1 ATTRIBUTE(REVERSE)

   DECLARE cur_individual CURSOR FOR query1

   LET i = 1

   FOREACH cur_individual USING x_folio INTO reg_2[i].*

      LET reg_5[i].folio           = reg_2[i].folio
      LET reg_5[i].nss             = reg_2[i].nss
      LET reg_5[i].tipo_operacion  = reg_2[i].tipo_operacion
      LET reg_5[i].fecha_operacion = reg_2[i].fecha_operacion
      LET reg_5[i].monto_en_pesos  = reg_2[i].monto_en_pesos

      DECLARE cursor_fx CURSOR FOR query2
      OPEN cursor_fx USING reg_2[i].nss
      FETCH cursor_fx INTO afi_nombres,
                           afi_paterno,
                           afi_materno,
                           afi_n_rfc,
                           afi_n_unico,
                           afi_fena,
                           afi_n_folio,
                           afi_tipo_solicitud
      CLOSE cursor_fx

      LET reg_5[i].nombre = afi_nombres CLIPPED," ",afi_paterno CLIPPED
      LET i = i + 1
   END FOREACH
   ERROR ""

   IF (i-1) >= 1 THEN
      CALL SET_COUNT(i-1)

      DISPLAY ARRAY reg_5 TO scr_1.*
         ON KEY (CONTROL-C)
            EXIT DISPLAY
      END DISPLAY
   ELSE
      ERROR "REGISTROS DE LA OPERACION ",lav_oper_desc," ... VACIO"
      SLEEP 2
      ERROR ""
   END IF

   CLEAR SCREEN
   CLOSE WINDOW ventana1
END FUNCTION
#############################################################################
FUNCTION detecta(x_fecha_inicio,
                 x_fecha_fin,
                 x_tipo_reporte)

   DEFINE det RECORD LIKE lav_det_lavado.*

   DEFINE x_tipo_reporte       SMALLINT,
          xx_tipo_reporte      CHAR(1),
          x_fecha_inicio       DATE,
          x_fecha_fin          DATE,
          x_fecha_periodo_ini  CHAR(10),
          x_fecha_periodo_fin  CHAR(10),
          x_fecha_periodo      CHAR(10),
          xx_fecha_periodo     CHAR(8),
          vfolio               DECIMAL(11,0),
          pos                  SMALLINT,
          i                    SMALLINT

   DEFINE reg_3                RECORD
          folio                DECIMAL(11,0),
          subcuenta            SMALLINT,
          tipo_movimiento      SMALLINT,
          id_aportante         CHAR(11),
          nss                  CHAR(11),
          monto_en_pesos       DECIMAL(16,6),
          fecha_conversion     DATE,
          sucursal             CHAR(10)
   END RECORD

   DEFINE x_condicion_cod      SMALLINT,
          x_monto_desde        DECIMAL(16,6),
          x_monto_hasta        DECIMAL(16,6),
          x_moneda_cod         SMALLINT,
          x_tipo_cambio        DECIMAL(16,6),
          x_valida             DECIMAL(16,6),
          x_valida2            DECIMAL(16,6),
          x_tipo_operacion     CHAR(2)

   DEFINE reg_4 RECORD
          condicion_cod        SMALLINT,
          monto_desde          DECIMAL(16,6),
          monto_hasta          DECIMAL(16,6),
          moneda_cod           SMALLINT
   END RECORD

   DEFINE xxx_nss              CHAR(11),
          xxx_monto_en_pesos   DECIMAL(16),
          xxx_tipo_operacion   CHAR(2),
          xxx_condicion        SMALLINT

   DEFINE act_folio            DECIMAL(11,0),
          act_nss              CHAR(11),
          act_consecutivo_reg  INTEGER,
          act_tipo_operacion   CHAR(2),
          act_condicion_cod    SMALLINT

   DEFINE x_fecha_recepcion    DATE,
          x_salario_mensual    DECIMAL(16,6),
          numero_salarios      SMALLINT,
          x_valida3            DECIMAL(16,6),
          x_valida4            DECIMAL(16,6)

   DEFINE hoy2                 DATE,
          x_fecha_aplica       DATE

   DEFINE x_ano_cuenta         CHAR(15),
          xx_sw                SMALLINT

   DEFINE x_fecha_ult_ret        DATE,
          x_fecha_primer_aporte  DATE,
          f_comparador           DATE,
          x_salario_mes_df       DECIMAL(16,6)

   DEFINE reg_210                RECORD
          tipo_movimiento        SMALLINT,
          id_aportante           CHAR(11),
          nss                    CHAR(11),
          sum_monto_en_pesos     DECIMAL(16,6)
   END RECORD

   DEFINE afi_nombres            CHAR(40),
          afi_paterno            CHAR(40),
          afi_materno            CHAR(40),
          afi_n_rfc              CHAR(13),
          afi_n_unico            CHAR(18),
          afi_fena               DATE,
          afi_n_folio            DECIMAL(11,0),
          afi_tipo_solicitud     SMALLINT

   DEFINE dom_calle              CHAR(40),
          dom_numero             CHAR(10),
          dom_depto              CHAR(10),
          dom_codpos             CHAR(5),
          dom_colonia            CHAR(60),
          dom_ciudad             INTEGER,
          dom_delega             INTEGER,
          dom_estado             INTEGER

   DEFINE x_estado_sucursal      SMALLINT,
          x_fecha_valida1        DATE,
          x_fecha_valida2        DATE

   DEFINE nss_isr                CHAR(11),
          prueba_monto           DECIMAL(16,6),
          prueba_monto2          DECIMAL(16,6),
          fecha_conv_isr         DATE

   DEFINE x_min_rowid            INTEGER

   DEFINE ind_doc                CHAR(2),
          x_plaza                CHAR(30)

   DEFINE x_sucursal       SMALLINT,
          xx_plaza          CHAR(6),
          x_estado_cod     SMALLINT,
          x_entidad_origen  CHAR(3)

   DEFINE xx_mayor         DECIMAL(16,6),
          cuenta           SMALLINT,
          x_cuenta_nss     CHAR(11)

   DEFINE xxxx_nss              CHAR(11),
          xx_fecha_recepcion    DATE,
          x_max_consecutivo     INTEGER,
          xx_salario_mes_df     DECIMAL(16,6),
          p_iso3                CHAR(3) #variable donde se guarda la naciondad recuperada de afi_mae_afiliado

   LET xx_sw = 0

   INSERT INTO lav_folio
   VALUES(0)

   SELECT MAX(folio)
   INTO   vfolio
   FROM   lav_folio
######################################oper 1
   IF x_tipo_reporte = 100 THEN

      LET x_fecha_periodo = x_fecha_fin USING "YYYYMM"

      DECLARE cursor_fx10 CURSOR FOR query4
      OPEN cursor_fx10 USING x_tipo_reporte
      FETCH cursor_fx10 INTO x_condicion_cod,
                             x_monto_desde,
                             x_monto_hasta,
                             x_moneda_cod
      CLOSE cursor_fx10

      LET hoy2 = x_fecha_inicio

      #LET hoy2 = MDY(MONTH(hoy2),1,YEAR(hoy2))

      LET x_fecha_aplica = habil_siguiente(hoy2)

      DECLARE cursor_fx11 CURSOR FOR query5
      OPEN cursor_fx11 USING x_moneda_cod,x_fecha_aplica
      FETCH cursor_fx11 INTO x_tipo_cambio
      CLOSE cursor_fx11

      LET x_valida = x_monto_desde * x_tipo_cambio

      IF x_tipo_reporte = 100 THEN
         LET xx_tipo_reporte = "1"
      END IF

      EXECUTE pdq_1

      LET pos = 1
      LET prueba_monto = 0
      LET prueba_monto2 = 0
      LET nss_isr = " "

      DECLARE cursor_1 CURSOR FOR query3
      FOREACH cursor_1 USING x_fecha_inicio,
                             x_fecha_fin
                       INTO  reg_3.folio,
                             reg_3.tipo_movimiento,
                             reg_3.nss,
                             reg_3.monto_en_pesos,
                             reg_3.fecha_conversion,
                             reg_3.sucursal

         IF reg_3.tipo_movimiento = 490 OR reg_3.tipo_movimiento = 10 THEN

            LET x_tipo_operacion = "21"
            LET reg_3.monto_en_pesos = reg_3.monto_en_pesos * (-1)

            IF nss_isr = " " THEN
               LET prueba_monto = 0
               LET nss_isr = reg_3.nss
            END IF

            IF reg_3.nss MATCHES nss_isr THEN

               LET fecha_conv_isr = reg_3.fecha_conversion

               IF prueba_monto > x_valida THEN
                  LET prueba_monto = prueba_monto + reg_3.monto_en_pesos
                  LET prueba_monto2 = prueba_monto
               ELSE
                  LET prueba_monto = prueba_monto + reg_3.monto_en_pesos
                  LET prueba_monto2 = prueba_monto
               END IF

               CONTINUE FOREACH
            END IF

            IF prueba_monto2 >= x_valida THEN

               LET xx_sw = 1

               LET det.folio            = vfolio
               LET det.consecutivo_reg  = pos
               LET det.tipo_reporte     = xx_tipo_reporte
               LET det.periodo_reporte  = x_fecha_periodo
               LET det.org_supervisor   = "001004"
               LET det.clave_financiera = "004",x_cod_afo CLIPPED
               LET det.localidad        = "01001002"
               LET det.sucursal         = "00000000"
               LET det.tipo_operacion   = x_tipo_operacion
               LET det.instrumento_mone = "01"
               LET det.nss              = nss_isr
               LET det.monto_en_pesos   = prueba_monto2     --- reg_3.monto_en_pesos
               LET det.moneda           = "1"
               LET det.fecha_operacion  = fecha_conv_isr
               LET det.fecha_deteccion  = NULL
              -- LET det.nacionalidad     = "1" #CPL-1356
               LET det.tipo_persona     = "1"
               LET det.actividad_eco    = "8944098"
               LET det.condicion_cod    = x_condicion_cod
               LET det.estado           = 10
              
             LET det.nacionalidad = "" 
              
              #CPL-1356
               SELECT nacionalidad
               INTO p_iso3
               FROM afi_mae_afiliado
               WHERE n_seguro =det.nss
               
               IF p_iso3 IS NULL THEN 
                LET p_iso3 = 0
               END IF 
               
               SELECT 'X'
               FROM tab_pais
               WHERE pais_cod = p_iso3
               
               IF SQLCA.SQLCODE <> 0 THEN
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
               ELSE 
             	 
             	 SELECT iso_2
               INTO det.nacionalidad
               FROM tab_pais_lav                   
               WHERE iso_3 = p_iso3
               
               IF det.nacionalidad IS NULL THEN 
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES EN LAVADO, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
              END IF    
            
            END IF 
              
              ##CPL-1356
               
               EXECUTE query9 USING det.folio,
                                    det.nss,
                                    det.consecutivo_reg,
                                    det.tipo_reporte,
                                    det.periodo_reporte,
                                    det.org_supervisor,
                                    det.clave_financiera,
                                    det.localidad,
                                    det.sucursal,
                                    det.tipo_operacion,
                                    det.instrumento_mone,
                                    det.monto_en_pesos,
                                    det.moneda,
                                    det.fecha_operacion,
                                    det.fecha_deteccion,
                                    det.nacionalidad,
                                    det.tipo_persona,
                                    det.actividad_eco,
                                    det.condicion_cod,
                                    det.estado

               LET pos = pos + 1

               LET nss_isr = " "
               LET prueba_monto = 0
               LET prueba_monto2 = 0
               LET nss_isr = reg_3.nss

               IF nss_isr = reg_3.nss THEN
                  LET prueba_monto = prueba_monto + reg_3.monto_en_pesos
                  CONTINUE FOREACH
               END IF
            ELSE
               LET nss_isr = " "
               LET prueba_monto = 0
               LET prueba_monto2 = 0
               LET nss_isr = reg_3.nss

               IF nss_isr = reg_3.nss THEN
                  LET prueba_monto = prueba_monto + reg_3.monto_en_pesos
                  CONTINUE FOREACH
               END IF
            END IF
         ELSE

            IF nss_isr <> reg_3.nss THEN
               IF prueba_monto2 >= x_valida THEN

                  LET xx_sw = 1

                  LET det.folio            = vfolio
                  LET det.consecutivo_reg  = pos
                  LET det.tipo_reporte     = xx_tipo_reporte
                  LET det.periodo_reporte  = x_fecha_periodo
                  LET det.org_supervisor   = "001004"
                  LET det.clave_financiera = "004",x_cod_afo CLIPPED
                  LET det.localidad        = "01001002"
                  LET det.sucursal         = "00000000"
                  LET det.tipo_operacion   = x_tipo_operacion
                  LET det.instrumento_mone = "01"
                  LET det.nss              = nss_isr
                  LET det.monto_en_pesos   = prueba_monto2     --- reg_3.monto_en_pesos
                  LET det.moneda           = "1"
                  LET det.fecha_operacion  = fecha_conv_isr
                  LET det.fecha_deteccion  = NULL
                 -- LET det.nacionalidad     = "1" #CPL-1356
                  LET det.tipo_persona     = "1"
                  LET det.actividad_eco    = "8944098"
                  LET det.condicion_cod    = x_condicion_cod
                  LET det.estado           = 10
             
             LET det.nacionalidad = "" 
              
              #CPL-1356
               SELECT nacionalidad
               INTO p_iso3
               FROM afi_mae_afiliado
               WHERE n_seguro =det.nss
               
               IF p_iso3 IS NULL THEN 
                LET p_iso3 = 0
               END IF 
               
               SELECT 'X'
               FROM tab_pais
               WHERE pais_cod = p_iso3
               
               IF SQLCA.SQLCODE <> 0 THEN
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
             ELSE 
             	 
             	 SELECT iso_2
               INTO det.nacionalidad
               FROM tab_pais_lav                   
               WHERE iso_3 = p_iso3
               
               IF det.nacionalidad IS NULL THEN 
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES EN LAVADO, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
              END IF    
            
            END IF 
              
              ##CPL-1356
              
                  EXECUTE query9 USING det.folio,
                                       det.nss,
                                       det.consecutivo_reg,
                                       det.tipo_reporte,
                                       det.periodo_reporte,
                                       det.org_supervisor,
                                       det.clave_financiera,
                                       det.localidad,
                                       det.sucursal,
                                       det.tipo_operacion,
                                       det.instrumento_mone,
                                       det.monto_en_pesos,
                                       det.moneda,
                                       det.fecha_operacion,
                                       det.fecha_deteccion,
                                       det.nacionalidad,
                                       det.tipo_persona,
                                       det.actividad_eco,
                                       det.condicion_cod,
                                       det.estado

                  LET pos = pos + 1

                  LET nss_isr = " "
                  LET prueba_monto = 0
                  LET prueba_monto2 = 0
                  LET nss_isr = reg_3.nss
               END IF
            ELSE
               IF prueba_monto2 >= x_valida THEN

                  LET xx_sw = 1

                  LET det.folio            = vfolio
                  LET det.consecutivo_reg  = pos
                  LET det.tipo_reporte     = xx_tipo_reporte
                  LET det.periodo_reporte  = x_fecha_periodo
                  LET det.org_supervisor   = "001004"
                  LET det.clave_financiera = "004",x_cod_afo CLIPPED
                  LET det.localidad        = "01001002"
                  LET det.sucursal         = "00000000"
                  LET det.tipo_operacion   = x_tipo_operacion
                  LET det.instrumento_mone = "01"
                  LET det.nss              = nss_isr
                  LET det.monto_en_pesos   = prueba_monto2     --- reg_3.monto_en_pesos
                  LET det.moneda           = "1"
                  LET det.fecha_operacion  = fecha_conv_isr
                  LET det.fecha_deteccion  = NULL
                --  LET det.nacionalidad     = "1" #CPL-1356
                  LET det.tipo_persona     = "1"
                  LET det.actividad_eco    = "8944098"
                  LET det.condicion_cod    = x_condicion_cod
                  LET det.estado           = 10
              
            LET det.nacionalidad = "" 
              
              #CPL-1356
               SELECT nacionalidad
               INTO p_iso3
               FROM afi_mae_afiliado
               WHERE n_seguro =det.nss
               
               IF p_iso3 IS NULL THEN 
                LET p_iso3 = 0
               END IF 
               
               SELECT 'X'
               FROM tab_pais
               WHERE pais_cod = p_iso3
               
               IF SQLCA.SQLCODE <> 0 THEN
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
             ELSE 
             	 
             	 SELECT iso_2
               INTO det.nacionalidad
               FROM tab_pais_lav                   
               WHERE iso_3 = p_iso3
               
               IF det.nacionalidad IS NULL THEN 
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES EN LAVADO, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
              END IF    
            
            END IF 
              
              ##CPL-1356
                  EXECUTE query9 USING det.folio,
                                       det.nss,
                                       det.consecutivo_reg,
                                       det.tipo_reporte,
                                       det.periodo_reporte,
                                       det.org_supervisor,
                                       det.clave_financiera,
                                       det.localidad,
                                       det.sucursal,
                                       det.tipo_operacion,
                                       det.instrumento_mone,
                                       det.monto_en_pesos,
                                       det.moneda,
                                       det.fecha_operacion,
                                       det.fecha_deteccion,
                                       det.nacionalidad,
                                       det.tipo_persona,
                                       det.actividad_eco,
                                       det.condicion_cod,
                                       det.estado

                  LET pos = pos + 1

                  LET nss_isr = " "
                  LET prueba_monto  = 0
                  LET prueba_monto2 = 0
                  LET nss_isr = reg_3.nss
               END IF
            END IF

            LET x_tipo_operacion = "20"
            LET prueba_monto     = reg_3.monto_en_pesos
         END IF

         IF prueba_monto >= x_valida AND x_tipo_operacion = "20" THEN

            LET xx_sw = 1

            LET det.folio            = vfolio
            LET det.consecutivo_reg  = pos
            LET det.tipo_reporte     = xx_tipo_reporte
            LET det.periodo_reporte  = x_fecha_periodo
            LET det.org_supervisor   = "001004"
            LET det.clave_financiera = "004",x_cod_afo CLIPPED
            LET det.localidad        = "01001002"
            LET det.sucursal         = "00000000"
            LET det.tipo_operacion   = x_tipo_operacion
            LET det.instrumento_mone = "01"
            LET det.nss              = reg_3.nss
            LET det.monto_en_pesos   = prueba_monto     --- reg_3.monto_en_pesos
            LET det.moneda           = "1"
            LET det.fecha_operacion  = reg_3.fecha_conversion
            LET det.fecha_deteccion  = NULL
          --  LET det.nacionalidad     = "1"#CPL-1356
            LET det.tipo_persona     = "1"
            LET det.actividad_eco    = "8944098"
            LET det.condicion_cod    = x_condicion_cod
            LET det.estado           = 10

            LET det.nacionalidad = "" 
              
              #CPL-1356
               SELECT nacionalidad
               INTO p_iso3
               FROM afi_mae_afiliado
               WHERE n_seguro =det.nss
               
               IF p_iso3 IS NULL THEN 
                LET p_iso3 = 0
               END IF 
               
               SELECT 'X'
               FROM tab_pais
               WHERE pais_cod = p_iso3
               
               IF SQLCA.SQLCODE <> 0 THEN
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
             ELSE 
             	 
             	 SELECT iso_2
               INTO det.nacionalidad
               FROM tab_pais_lav                   
               WHERE iso_3 = p_iso3
               
               IF det.nacionalidad IS NULL THEN 
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES EN LAVADO, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
              END IF    
            
            END IF 
              
              ##CPL-1356
               
            EXECUTE query9 USING det.folio,
                                 det.nss,
                                 det.consecutivo_reg,
                                 det.tipo_reporte,
                                 det.periodo_reporte,
                                 det.org_supervisor,
                                 det.clave_financiera,
                                 det.localidad,
                                 det.sucursal,
                                 det.tipo_operacion,
                                 det.instrumento_mone,
                                 det.monto_en_pesos,
                                 det.moneda,
                                 det.fecha_operacion,
                                 det.fecha_deteccion,
                                 det.nacionalidad,
                                 det.tipo_persona,
                                 det.actividad_eco,
                                 det.condicion_cod,
                                 det.estado

            LET pos = pos + 1

         END IF

         LET nss_isr = " "
         LET reg_3.monto_en_pesos = 0
         LET prueba_monto = 0

      END FOREACH

      IF reg_3.tipo_movimiento = 490 OR reg_3.tipo_movimiento = 10 THEN
         LET x_tipo_operacion = "21"

         IF prueba_monto2 >= x_valida THEN

            LET xx_sw = 1

            LET det.folio            = vfolio
            LET det.consecutivo_reg  = pos
            LET det.tipo_reporte     = xx_tipo_reporte
            LET det.periodo_reporte  = x_fecha_periodo
            LET det.org_supervisor   = "001004"
            LET det.clave_financiera = "004",x_cod_afo CLIPPED
            LET det.localidad        = "01001002"
            LET det.sucursal         = "00000000"
            LET det.tipo_operacion   = x_tipo_operacion
            LET det.instrumento_mone = "01"
            LET det.nss              = nss_isr
            LET det.monto_en_pesos   = prueba_monto2     --- reg_3.monto_en_pesos
            LET det.moneda           = "1"
            LET det.fecha_operacion  = fecha_conv_isr
            LET det.fecha_deteccion  = NULL
        --    LET det.nacionalidad     = "1"#CPL-1356
            LET det.tipo_persona     = "1"
            LET det.actividad_eco    = "8944098"
            LET det.condicion_cod    = x_condicion_cod
            LET det.estado           = 10

            LET det.nacionalidad = "" 
              
              #CPL-1356
               SELECT nacionalidad
               INTO p_iso3
               FROM afi_mae_afiliado
               WHERE n_seguro =det.nss
               
               IF p_iso3 IS NULL THEN 
                LET p_iso3 = 0
               END IF 
               
               SELECT 'X'
               FROM tab_pais
               WHERE pais_cod = p_iso3
               
               IF SQLCA.SQLCODE <> 0 THEN
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
             ELSE 
             	 
             	 SELECT iso_2
               INTO det.nacionalidad
               FROM tab_pais_lav                   
               WHERE iso_3 = p_iso3
               
               IF det.nacionalidad IS NULL THEN 
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES EN LAVADO, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
              END IF    
            
            END IF 
              
              ##CPL-1356
               
            EXECUTE query9 USING det.folio,
                                 det.nss,
                                 det.consecutivo_reg,
                                 det.tipo_reporte,
                                 det.periodo_reporte,
                                 det.org_supervisor,
                                 det.clave_financiera,
                                 det.localidad,
                                 det.sucursal,
                                 det.tipo_operacion,
                                 det.instrumento_mone,
                                 det.monto_en_pesos,
                                 det.moneda,
                                 det.fecha_operacion,
                                 det.fecha_deteccion,
                                 det.nacionalidad,
                                 det.tipo_persona,
                                 det.actividad_eco,
                                 det.condicion_cod,
                                 det.estado
         END IF
      END IF

--      EXECUTE pdq_2
   END IF
######################################oper 2
   IF x_tipo_reporte = 200 THEN

      LET x_fecha_periodo = x_fecha_fin USING "YYYYMMDD"

      IF x_tipo_reporte = 200 THEN
         LET xx_tipo_reporte = "2"
      END IF

      DECLARE cursor_4 CURSOR FOR query4
      LET i = 1
      FOREACH cursor_4 USING x_tipo_reporte
                       INTO reg_4.condicion_cod,
                            reg_4.monto_desde,
                            reg_4.monto_hasta,
                            reg_4.moneda_cod

         LET hoy2 = x_fecha_inicio
         LET x_fecha_aplica = habil_siguiente(hoy2)

         IF reg_4.condicion_cod = 210 THEN
            LET pos = 1
            LET prueba_monto = 0
            LET prueba_monto2 = 0
            LET nss_isr = " "

            DECLARE c_ursor_1 CURSOR FOR query3
            FOREACH c_ursor_1 USING x_fecha_inicio,
                                    x_fecha_fin
                              INTO  reg_3.folio,
                                    reg_3.tipo_movimiento,
                                    reg_3.nss,
                                    reg_3.monto_en_pesos,
                                    reg_3.fecha_conversion,
                                    reg_3.sucursal

               DECLARE cursor_fx1 CURSOR FOR query5
               OPEN cursor_fx1 USING reg_4.moneda_cod,
                                     reg_3.fecha_conversion
               FETCH cursor_fx1 INTO x_tipo_cambio
               CLOSE cursor_fx1

               LET x_valida  = reg_4.monto_desde * x_tipo_cambio
               LET x_valida2 = reg_4.monto_hasta * x_tipo_cambio
               LET xx_mayor  = x_valida

            IF reg_3.tipo_movimiento = 490 OR reg_3.tipo_movimiento = 10 THEN
               LET x_tipo_operacion = "21"
               LET reg_3.monto_en_pesos = reg_3.monto_en_pesos * (-1)

               IF nss_isr = " " THEN
                  LET prueba_monto = 0
                  LET nss_isr = reg_3.nss
               END IF

               IF reg_3.nss MATCHES nss_isr THEN
                  LET fecha_conv_isr = reg_3.fecha_conversion

                  IF prueba_monto > x_valida THEN
                     LET prueba_monto = prueba_monto + reg_3.monto_en_pesos
                     LET prueba_monto2 = prueba_monto
                  ELSE
                     LET prueba_monto = prueba_monto + reg_3.monto_en_pesos
                     LET prueba_monto2 = prueba_monto
                  END IF

                  CONTINUE FOREACH
               END IF

               IF prueba_monto2 >= x_valida THEN
                  LET xx_sw = 1

                  LET det.folio            = vfolio
                  LET det.consecutivo_reg  = pos
                  LET det.tipo_reporte     = xx_tipo_reporte
                  LET det.periodo_reporte  = x_fecha_periodo
                  LET det.org_supervisor   = "001004"
                  LET det.clave_financiera = "004",x_cod_afo CLIPPED
                  LET det.localidad        = "01001002"
                  LET det.sucursal         = "00000000"
                  LET det.tipo_operacion   = x_tipo_operacion
                  LET det.instrumento_mone = "01"
                  LET det.nss              = nss_isr
                  LET det.monto_en_pesos   = prueba_monto2     --- reg_3.monto_en_pesos
                  LET det.moneda           = "1"
                  LET det.fecha_operacion  = fecha_conv_isr
                  LET det.fecha_deteccion  = NULL
             --     LET det.nacionalidad     = "1" #CPL-1356
                  LET det.tipo_persona     = "1"
                  LET det.actividad_eco    = "8944098"
               #LET det.condicion_cod    = x_condicion_cod
                  LET det.condicion_cod    = 210
                  LET det.estado           = 10

            LET det.nacionalidad = "" 
              
              #CPL-1356
               SELECT nacionalidad
               INTO p_iso3
               FROM afi_mae_afiliado
               WHERE n_seguro =det.nss
               
               IF p_iso3 IS NULL THEN 
                LET p_iso3 = 0
               END IF 
               
               SELECT 'X'
               FROM tab_pais
               WHERE pais_cod = p_iso3
               
               IF SQLCA.SQLCODE <> 0 THEN
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
             ELSE 
             	 
             	 SELECT iso_2
               INTO det.nacionalidad
               FROM tab_pais_lav                   
               WHERE iso_3 = p_iso3
               
               IF det.nacionalidad IS NULL THEN 
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES EN LAVADO, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
              END IF    
            
            END IF 
              
              ##CPL-1356
               
                  EXECUTE query9 USING det.folio,
                                       det.nss,
                                       det.consecutivo_reg,
                                       det.tipo_reporte,
                                       det.periodo_reporte,
                                       det.org_supervisor,
                                       det.clave_financiera,
                                       det.localidad,
                                       det.sucursal,
                                       det.tipo_operacion,
                                       det.instrumento_mone,
                                       det.monto_en_pesos,
                                       det.moneda,
                                       det.fecha_operacion,
                                       det.fecha_deteccion,
                                       det.nacionalidad,
                                       det.tipo_persona,
                                       det.actividad_eco,
                                       det.condicion_cod,
                                       det.estado

                  LET pos = pos + 1

                  LET nss_isr = " "
                  LET prueba_monto = 0
                  LET prueba_monto2 = 0
                  LET nss_isr = reg_3.nss

                  IF nss_isr = reg_3.nss THEN
                     LET prueba_monto = prueba_monto + reg_3.monto_en_pesos
                     CONTINUE FOREACH
                  END IF
               ELSE

                  LET nss_isr = " "
                  LET prueba_monto = 0
                  LET prueba_monto2 = 0
                  LET nss_isr = reg_3.nss

                  IF nss_isr = reg_3.nss THEN
                     LET prueba_monto = prueba_monto + reg_3.monto_en_pesos
                     CONTINUE FOREACH
                  END IF
               END IF
            ELSE
--aqui
               IF nss_isr <> reg_3.nss THEN
                  IF nss_isr <> " " THEN
                  IF prueba_monto2 >= x_valida THEN

                     LET xx_sw = 1

                     LET det.folio            = vfolio
                     LET det.consecutivo_reg  = pos
                     LET det.tipo_reporte     = xx_tipo_reporte
                     LET det.periodo_reporte  = x_fecha_periodo
                     LET det.org_supervisor   = "001004"
                     LET det.clave_financiera = "004",x_cod_afo CLIPPED
                     LET det.localidad        = "01001002"
                     LET det.sucursal         = "00000000"
                     LET det.tipo_operacion   = x_tipo_operacion
                     LET det.instrumento_mone = "01"
                     LET det.nss              = nss_isr
                     LET det.monto_en_pesos   = prueba_monto2     --- reg_3.monto_en_pesos
                     LET det.moneda           = "1"
                     LET det.fecha_operacion  = fecha_conv_isr
                     LET det.fecha_deteccion  = NULL
                   --  LET det.nacionalidad     = "1" #CPL-1356
                     LET det.tipo_persona     = "1"
                     LET det.actividad_eco    = "8944098"
                     LET det.condicion_cod    = 210
                     LET det.estado           = 10

            LET det.nacionalidad = "" 
              
              #CPL-1356
               SELECT nacionalidad
               INTO p_iso3
               FROM afi_mae_afiliado
               WHERE n_seguro =det.nss
               
               IF p_iso3 IS NULL THEN 
                LET p_iso3 = 0
               END IF 
               
               SELECT 'X'
               FROM tab_pais
               WHERE pais_cod = p_iso3
               
               IF SQLCA.SQLCODE <> 0 THEN
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
             ELSE 
             	 
             	 SELECT iso_2
               INTO det.nacionalidad
               FROM tab_pais_lav                   
               WHERE iso_3 = p_iso3
               
               IF det.nacionalidad IS NULL THEN 
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES EN LAVADO, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
              END IF    
            
            END IF 
              
              ##CPL-1356
                     EXECUTE query9 USING det.folio,
                                          det.nss,
                                          det.consecutivo_reg,
                                          det.tipo_reporte,
                                          det.periodo_reporte,
                                          det.org_supervisor,
                                          det.clave_financiera,
                                          det.localidad,
                                          det.sucursal,
                                          det.tipo_operacion,
                                          det.instrumento_mone,
                                          det.monto_en_pesos,
                                          det.moneda,
                                          det.fecha_operacion,
                                          det.fecha_deteccion,
                                          det.nacionalidad,
                                          det.tipo_persona,
                                          det.actividad_eco,
                                          det.condicion_cod,
                                          det.estado

                     LET pos = pos + 1

                     LET nss_isr = " "
                     LET prueba_monto = 0
                     LET prueba_monto2 = 0
                     LET nss_isr = reg_3.nss
                  END IF
                  END IF
               ELSE
                  IF prueba_monto2 >= x_valida THEN

                     LET xx_sw = 1

                     LET det.folio            = vfolio
                     LET det.consecutivo_reg  = pos
                     LET det.tipo_reporte     = xx_tipo_reporte
                     LET det.periodo_reporte  = x_fecha_periodo
                     LET det.org_supervisor   = "001004"
                     LET det.clave_financiera = "004",x_cod_afo CLIPPED
                     LET det.localidad        = "01001002"
                     LET det.sucursal         = "00000000"
                     LET det.tipo_operacion   = x_tipo_operacion
                     LET det.instrumento_mone = "01"
                     LET det.nss              = nss_isr
                     LET det.monto_en_pesos   = prueba_monto2     --- reg_3.monto_en_pesos
                     LET det.moneda           = "1"
                     LET det.fecha_operacion  = fecha_conv_isr
                     LET det.fecha_deteccion  = NULL
                   --  LET det.nacionalidad     = "1" #CPL-1356
                     LET det.tipo_persona     = "1"
                     LET det.actividad_eco    = "8944098"
                  #LET det.condicion_cod    = x_condicion_cod
                     LET det.condicion_cod    = 210
                     LET det.estado           = 10
              
            LET det.nacionalidad = "" 
              
              #CPL-1356
               SELECT nacionalidad
               INTO p_iso3
               FROM afi_mae_afiliado
               WHERE n_seguro =det.nss
               
               IF p_iso3 IS NULL THEN 
                LET p_iso3 = 0
               END IF 
               
               SELECT 'X'
               FROM tab_pais
               WHERE pais_cod = p_iso3
               
               IF SQLCA.SQLCODE <> 0 THEN
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
             ELSE 
             	 
             	 SELECT iso_2
               INTO det.nacionalidad
               FROM tab_pais_lav                   
               WHERE iso_3 = p_iso3
               
               IF det.nacionalidad IS NULL THEN 
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES EN LAVADO, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
              END IF    
            
            END IF 
              
              ##CPL-1356
                     EXECUTE query9 USING det.folio,
                                          det.nss,
                                          det.consecutivo_reg,
                                          det.tipo_reporte,
                                          det.periodo_reporte,
                                          det.org_supervisor,
                                          det.clave_financiera,
                                          det.localidad,
                                          det.sucursal,
                                          det.tipo_operacion,
                                          det.instrumento_mone,
                                          det.monto_en_pesos,
                                          det.moneda,
                                          det.fecha_operacion,
                                          det.fecha_deteccion,
                                          det.nacionalidad,
                                          det.tipo_persona,
                                          det.actividad_eco,
                                          det.condicion_cod,
                                          det.estado

                    LET pos = pos + 1

                    LET nss_isr = " "
                    LET prueba_monto  = 0
                    LET prueba_monto2 = 0
                    LET nss_isr = reg_3.nss
                 END IF
              END IF

              LET x_tipo_operacion = "20"
              LET prueba_monto     = reg_3.monto_en_pesos
           END IF

           IF prueba_monto >= x_valida AND x_tipo_operacion = "20" THEN

              LET xx_sw = 1

              LET det.folio            = vfolio
              LET det.consecutivo_reg  = pos
              LET det.tipo_reporte     = xx_tipo_reporte
              LET det.periodo_reporte  = x_fecha_periodo
              LET det.org_supervisor   = "001004"
              LET det.clave_financiera = "004",x_cod_afo CLIPPED
              LET det.localidad        = "01001002"
              LET det.sucursal         = "00000000"
              LET det.tipo_operacion   = x_tipo_operacion
              LET det.instrumento_mone = "01"
              LET det.nss              = reg_3.nss
              LET det.monto_en_pesos   = prueba_monto     --- reg_3.monto_en_pesos
              LET det.moneda           = "1"
              LET det.fecha_operacion  = reg_3.fecha_conversion
              LET det.fecha_deteccion  = NULL
             -- LET det.nacionalidad     = "1" #CPL-1356
              LET det.tipo_persona     = "1"
              LET det.actividad_eco    = "8944098"
            #LET det.condicion_cod    = x_condicion_cod
              LET det.condicion_cod    = 210
              LET det.estado           = 10
            
            LET det.nacionalidad = "" 
              
              #CPL-1356
               SELECT nacionalidad
               INTO p_iso3
               FROM afi_mae_afiliado
               WHERE n_seguro =det.nss
               
               IF p_iso3 IS NULL THEN 
                LET p_iso3 = 0
               END IF 
               
               SELECT 'X'
               FROM tab_pais
               WHERE pais_cod = p_iso3
               
               IF SQLCA.SQLCODE <> 0 THEN
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
             ELSE 
             	 
             	 SELECT iso_2
               INTO det.nacionalidad
               FROM tab_pais_lav                   
               WHERE iso_3 = p_iso3
               
               IF det.nacionalidad IS NULL THEN 
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES EN LAVADO, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
              END IF    
            
            END IF 
              
              ##CPL-1356
              EXECUTE query9 USING det.folio,
                                   det.nss,
                                   det.consecutivo_reg,
                                   det.tipo_reporte,
                                   det.periodo_reporte,
                                   det.org_supervisor,
                                   det.clave_financiera,
                                   det.localidad,
                                   det.sucursal,
                                   det.tipo_operacion,
                                   det.instrumento_mone,
                                   det.monto_en_pesos,
                                   det.moneda,
                                   det.fecha_operacion,
                                   det.fecha_deteccion,
                                   det.nacionalidad,
                                   det.tipo_persona,
                                   det.actividad_eco,
                                   det.condicion_cod,
                                   det.estado

              LET pos = pos + 1

           END IF

           LET nss_isr = " "
           LET reg_3.monto_en_pesos = 0
           LET prueba_monto = 0

        END FOREACH

        IF reg_3.tipo_movimiento = 490 OR reg_3.tipo_movimiento = 10 THEN
           LET x_tipo_operacion = "21"

           IF prueba_monto2 >= x_valida THEN

              LET xx_sw = 1

              LET det.folio            = vfolio
              LET det.consecutivo_reg  = pos
              LET det.tipo_reporte     = xx_tipo_reporte
              LET det.periodo_reporte  = x_fecha_periodo
              LET det.org_supervisor   = "001004"
              LET det.clave_financiera = "004",x_cod_afo CLIPPED
              LET det.localidad        = "01001002"
              LET det.sucursal         = "00000000"
              LET det.tipo_operacion   = x_tipo_operacion
              LET det.instrumento_mone = "01"
              LET det.nss              = nss_isr
              LET det.monto_en_pesos   = prueba_monto2     --- reg_3.monto_en_pesos
              LET det.moneda           = "1"
              LET det.fecha_operacion  = fecha_conv_isr
              LET det.fecha_deteccion  = NULL
             -- LET det.nacionalidad     = "1" #CPL-1356
              LET det.tipo_persona     = "1"
              LET det.actividad_eco    = "8944098"
            #LET det.condicion_cod    = x_condicion_cod
              LET det.condicion_cod    = 210
              LET det.estado           = 10
            
            LET det.nacionalidad = "" 
              
              #CPL-1356
               SELECT nacionalidad
               INTO p_iso3
               FROM afi_mae_afiliado
               WHERE n_seguro =det.nss
               
               IF p_iso3 IS NULL THEN 
                LET p_iso3 = 0
               END IF 
               
               SELECT 'X'
               FROM tab_pais
               WHERE pais_cod = p_iso3
               
               IF SQLCA.SQLCODE <> 0 THEN
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
             ELSE 
             	 
             	 SELECT iso_2
               INTO det.nacionalidad
               FROM tab_pais_lav                   
               WHERE iso_3 = p_iso3
               
               IF det.nacionalidad IS NULL THEN 
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES EN LAVADO, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
              END IF    
            
            END IF 
              
              ##CPL-1356

              EXECUTE query9 USING det.folio,
                                   det.nss,
                                   det.consecutivo_reg,
                                   det.tipo_reporte,
                                   det.periodo_reporte,
                                   det.org_supervisor,
                                   det.clave_financiera,
                                   det.localidad,
                                   det.sucursal,
                                   det.tipo_operacion,
                                   det.instrumento_mone,
                                   det.monto_en_pesos,
                                   det.moneda,
                                   det.fecha_operacion,
                                   det.fecha_deteccion,
                                   det.nacionalidad,
                                   det.tipo_persona,
                                   det.actividad_eco,
                                   det.condicion_cod,
                                   det.estado
           END IF
        END IF
     END IF

     IF reg_4.condicion_cod = 220 THEN
        LET pos = 1
        DECLARE cursor_230 CURSOR FOR query3
        FOREACH cursor_230 USING x_fecha_inicio,
                                 x_fecha_fin
                            INTO reg_3.folio,
                                 reg_3.tipo_movimiento,
                                 reg_3.nss,
                                 reg_3.monto_en_pesos,
                                 reg_3.fecha_conversion,
                                 reg_3.sucursal

           SELECT nss,
                  COUNT(*)
           INTO   x_cuenta_nss,
                  cuenta
           FROM   dis_cuenta
           WHERE  nss = reg_3.nss
           AND    subcuenta IN (3,10)
           AND    tipo_movimiento  = 1
           AND    fecha_conversion BETWEEN x_fecha_inicio AND x_fecha_fin
           AND    id_aportante[1,3] NOT IN ('MQ-','MC-','UM-','UQ-','MA-','MS-','AF-','AC-','UR-','US-','AA-','AS-','UY-','UZ-','COR')
           GROUP BY 1

           IF reg_4.monto_desde = cuenta THEN
              CONTINUE FOREACH
           END IF

           IF reg_3.tipo_movimiento = 10 OR reg_3.tipo_movimiento = 490 THEN
              CONTINUE FOREACH
           END IF

           LET x_tipo_operacion = "20"

           LET xx_sw = 1

           LET det.folio            = vfolio
           LET det.consecutivo_reg  = pos
           LET det.tipo_reporte     = xx_tipo_reporte
           LET det.periodo_reporte  = x_fecha_periodo
           LET det.org_supervisor   = "001004"
           LET det.clave_financiera = "004",x_cod_afo CLIPPED
           LET det.localidad        = "01001002"
           LET det.sucursal         = "00000000"
           LET det.tipo_operacion   = x_tipo_operacion
           LET det.instrumento_mone = "01"
           LET det.nss              = reg_3.nss
           LET det.monto_en_pesos   = reg_3.monto_en_pesos
           LET det.moneda           = "1"
           LET det.fecha_operacion  = reg_3.fecha_conversion
           LET det.fecha_deteccion  = NULL
        --   LET det.nacionalidad     = "1" #CPL-1356
           LET det.tipo_persona     = "1"
           LET det.actividad_eco    = "8944098"
           LET det.condicion_cod    = 220
           LET det.estado           = 10

            LET det.nacionalidad = "" 
              
              #CPL-1356
               SELECT nacionalidad
               INTO p_iso3
               FROM afi_mae_afiliado
               WHERE n_seguro =det.nss
               
               IF p_iso3 IS NULL THEN 
                LET p_iso3 = 0
               END IF 
               
               SELECT 'X'
               FROM tab_pais
               WHERE pais_cod = p_iso3
               
               IF SQLCA.SQLCODE <> 0 THEN
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
             ELSE 
             	 
             	 SELECT iso_2
               INTO det.nacionalidad
               FROM tab_pais_lav                   
               WHERE iso_3 = p_iso3
               
               IF det.nacionalidad IS NULL THEN 
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES EN LAVADO, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
              END IF    
            
            END IF 
              
              ##CPL-1356
               
           EXECUTE query9 USING det.folio,
                                det.nss,
                                det.consecutivo_reg,
                                det.tipo_reporte,
                                det.periodo_reporte,
                                det.org_supervisor,
                                det.clave_financiera,
                                det.localidad,
                                det.sucursal,
                                det.tipo_operacion,
                                det.instrumento_mone,
                                det.monto_en_pesos,
                                det.moneda,
                                det.fecha_operacion,
                                det.fecha_deteccion,
                                det.nacionalidad,
                                det.tipo_persona,
                                det.actividad_eco,
                                det.condicion_cod,
                                det.estado
           LET pos = pos + 1
        END FOREACH
     END IF

     IF reg_4.condicion_cod = 240 THEN
        DECLARE cursor_240a CURSOR FOR query34
        LET pos = 1
        FOREACH cursor_240a USING x_fecha_inicio,
                                  x_fecha_fin
                             INTO reg_3.id_aportante,
                                  reg_3.nss,
                                  reg_3.monto_en_pesos,
                                  reg_3.fecha_conversion,
                                  reg_3.sucursal

          IF reg_3.id_aportante = "RETIRO" THEN
             LET x_tipo_operacion = "21"
             CONTINUE FOREACH
          ELSE
             LET x_tipo_operacion = "20"
          END IF

          DECLARE cursor_240aafi CURSOR FOR query2
          OPEN cursor_240aafi USING reg_3.nss
          FETCH cursor_240aafi INTO afi_nombres,
                                    afi_paterno,
                                    afi_materno,
                                    afi_n_rfc,
                                    afi_n_unico,
                                    afi_fena,
                                    afi_n_folio,
                                    afi_tipo_solicitud

          CLOSE cursor_240aafi

          DECLARE cursor_240dom CURSOR FOR query13
               OPEN cursor_240dom USING reg_3.nss,
                                        afi_n_folio,
                                        afi_tipo_solicitud
               FETCH cursor_240dom INTO dom_calle,
                                        dom_numero,
                                        dom_depto,
                                        dom_codpos,
                                        dom_colonia,
                                        dom_ciudad,
                                        dom_estado
               CLOSE cursor_240dom

               DECLARE cursor_sal1 CURSOR FOR query_sal1
               OPEN cursor_sal1 USING reg_3.nss
               FETCH cursor_sal1 INTO xx_fecha_recepcion
               CLOSE cursor_sal1

               DECLARE cursor_sal2 CURSOR FOR query_sal2
               OPEN cursor_sal2 USING reg_3.nss,
                                      xx_fecha_recepcion
               FETCH cursor_sal2 INTO x_max_consecutivo
               CLOSE cursor_sal2

               DECLARE cursor_sal3 CURSOR FOR query_sal3
               OPEN cursor_sal3 USING reg_3.nss,
                                      x_max_consecutivo,
                                      xx_fecha_recepcion
               FETCH cursor_sal3 INTO xx_salario_mes_df
               CLOSE cursor_sal3

               LET numero_salarios = reg_4.monto_desde

               LET x_valida4 = xx_salario_mes_df * numero_salarios

               IF reg_3.monto_en_pesos > x_valida4 THEN

                  LET xx_sw = 1

                  LET det.folio            = vfolio
                  LET det.consecutivo_reg  = pos
                  LET det.tipo_reporte     = xx_tipo_reporte
                  LET det.periodo_reporte  = x_fecha_periodo
                  LET det.org_supervisor   = "001004"
                  LET det.clave_financiera = "004",x_cod_afo CLIPPED
                  LET det.localidad        = "01001002"
                  LET det.sucursal         = "00000000"
                  LET det.tipo_operacion   = x_tipo_operacion
                  LET det.instrumento_mone = "01"
                  LET det.nss              = reg_3.nss
                  LET det.monto_en_pesos   = reg_3.monto_en_pesos
                  LET det.moneda           = "1"
                  LET det.fecha_operacion  = reg_3.fecha_conversion
                  LET det.fecha_deteccion  = NULL
               --   LET det.nacionalidad     = "1" #CPL-1356
                  LET det.tipo_persona     = "1"
                  LET det.actividad_eco    = "8944098"
                  #LET det.condicion_cod    = 240
                  LET det.condicion_cod    = reg_4.condicion_cod
                  LET det.estado           = 10

            LET det.nacionalidad = "" 
              
              #CPL-1356
               SELECT nacionalidad
               INTO p_iso3
               FROM afi_mae_afiliado
               WHERE n_seguro =det.nss
               
               IF p_iso3 IS NULL THEN 
                LET p_iso3 = 0
               END IF 
               
               SELECT 'X'
               FROM tab_pais
               WHERE pais_cod = p_iso3
               
               IF SQLCA.SQLCODE <> 0 THEN
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
             ELSE 
             	 
             	 SELECT iso_2
               INTO det.nacionalidad
               FROM tab_pais_lav                   
               WHERE iso_3 = p_iso3
               
               IF det.nacionalidad IS NULL THEN 
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES EN LAVADO, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
              END IF    
            
            END IF 
              
              ##CPL-1356
               
                  EXECUTE query9 USING det.folio,
                                       det.nss,
                                       det.consecutivo_reg,
                                       det.tipo_reporte,
                                       det.periodo_reporte,
                                       det.org_supervisor,
                                       det.clave_financiera,
                                       det.localidad,
                                       det.sucursal,
                                       det.tipo_operacion,
                                       det.instrumento_mone,
                                       det.monto_en_pesos,
                                       det.moneda,
                                       det.fecha_operacion,
                                       det.fecha_deteccion,
                                       det.nacionalidad,
                                       det.tipo_persona,
                                       det.actividad_eco,
                                       det.condicion_cod,
                                       det.estado

                  LET pos = pos + 1

                  LET reg_3.monto_en_pesos = 0
               END IF
            END FOREACH
         END IF
         LET i = i + 1
      END FOREACH
   END IF

##########################oper 3
   IF x_tipo_reporte = 300 THEN

      LET x_fecha_periodo = x_fecha_fin USING "YYYYMMDD"

      IF x_tipo_reporte = 300 THEN
         LET xx_tipo_reporte = "3"
      END IF

      DECLARE cursor_4a CURSOR FOR query4

      LET i = 1

      FOREACH cursor_4a USING x_tipo_reporte
                        INTO  reg_4.condicion_cod,
                              reg_4.monto_desde,
                              reg_4.monto_hasta,
                              reg_4.moneda_cod

         LET hoy2 = x_fecha_inicio

         LET x_fecha_aplica = habil_siguiente(hoy2)

         IF reg_4.condicion_cod = 310 THEN

            LET pos = 1
            LET prueba_monto = 0
            LET prueba_monto2 = 0
            LET nss_isr = " "

            DECLARE c_ursor_13 CURSOR FOR query3

            FOREACH c_ursor_13 USING x_fecha_inicio,
                                     x_fecha_fin
                               INTO  reg_3.folio,
                                     reg_3.tipo_movimiento,
                                     reg_3.nss,
                                     reg_3.monto_en_pesos,
                                     reg_3.fecha_conversion,
                                     reg_3.sucursal

               SELECT "X"
               FROM   com_nomina
               WHERE  nss = reg_3.nss
               GROUP BY 1

               IF SQLCA.SQLCODE <> 0 THEN
                  CONTINUE FOREACH
               END IF

               DECLARE cursor_fx1a CURSOR FOR query5
               OPEN cursor_fx1a USING reg_4.moneda_cod,
                                      reg_3.fecha_conversion
               FETCH cursor_fx1a INTO x_tipo_cambio
               CLOSE cursor_fx1a

               LET x_valida  = reg_4.monto_desde * x_tipo_cambio
               LET x_valida2 = reg_4.monto_hasta * x_tipo_cambio
               LET xx_mayor  = x_valida

            IF reg_3.tipo_movimiento = 490 OR reg_3.tipo_movimiento = 10 THEN

               LET x_tipo_operacion = "21"
               LET reg_3.monto_en_pesos = reg_3.monto_en_pesos * (-1)

               IF nss_isr = " " THEN
                  LET prueba_monto = 0
                  LET nss_isr = reg_3.nss
               END IF

               IF reg_3.nss MATCHES nss_isr THEN

                  LET fecha_conv_isr = reg_3.fecha_conversion

                  IF prueba_monto > x_valida THEN
                     LET prueba_monto = prueba_monto + reg_3.monto_en_pesos
                     LET prueba_monto2 = prueba_monto
                  ELSE
                     LET prueba_monto = prueba_monto + reg_3.monto_en_pesos
                     LET prueba_monto2 = prueba_monto
                  END IF

                  CONTINUE FOREACH
               END IF

               IF prueba_monto2 >= x_valida THEN

                  LET xx_sw = 1

                  LET det.folio            = vfolio
                  LET det.consecutivo_reg  = pos
                  LET det.tipo_reporte     = xx_tipo_reporte
                  LET det.periodo_reporte  = x_fecha_periodo
                  LET det.org_supervisor   = "001004"
                  LET det.clave_financiera = "004",x_cod_afo CLIPPED
                  LET det.localidad        = "01001002"
                  LET det.sucursal         = "00000000"
                  LET det.tipo_operacion   = x_tipo_operacion
                  LET det.instrumento_mone = "01"
                  LET det.nss              = nss_isr
                  LET det.monto_en_pesos   = prueba_monto2     --- reg_3.monto_en_pesos
                  LET det.moneda           = "1"
                  LET det.fecha_operacion  = fecha_conv_isr
                  LET det.fecha_deteccion  = NULL
                 -- LET det.nacionalidad     = "1" #CPL-1356
                  LET det.tipo_persona     = "1"
                  LET det.actividad_eco    = "8944098"
                  #LET det.condicion_cod    = x_condicion_cod
                  LET det.condicion_cod    = 310
                  LET det.estado           = 10
            
           LET det.nacionalidad = "" 
              
              #CPL-1356
               SELECT nacionalidad
               INTO p_iso3
               FROM afi_mae_afiliado
               WHERE n_seguro =det.nss
               
               IF p_iso3 IS NULL THEN 
                LET p_iso3 = 0
               END IF 
               
               SELECT 'X'
               FROM tab_pais
               WHERE pais_cod = p_iso3
               
               IF SQLCA.SQLCODE <> 0 THEN
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
             ELSE 
             	 
             	 SELECT iso_2
               INTO det.nacionalidad
               FROM tab_pais_lav                   
               WHERE iso_3 = p_iso3
               
               IF det.nacionalidad IS NULL THEN 
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES EN LAVADO, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
              END IF    
            
            END IF 
              
              ##CPL-1356
                  EXECUTE query9 USING det.folio,
                                       det.nss,
                                       det.consecutivo_reg,
                                       det.tipo_reporte,
                                       det.periodo_reporte,
                                       det.org_supervisor,
                                       det.clave_financiera,
                                       det.localidad,
                                       det.sucursal,
                                       det.tipo_operacion,
                                       det.instrumento_mone,
                                       det.monto_en_pesos,
                                       det.moneda,
                                       det.fecha_operacion,
                                       det.fecha_deteccion,
                                       det.nacionalidad,
                                       det.tipo_persona,
                                       det.actividad_eco,
                                       det.condicion_cod,
                                       det.estado

                  LET pos = pos + 1

                  LET nss_isr = " "
                  LET prueba_monto = 0
                  LET prueba_monto2 = 0
                  LET nss_isr = reg_3.nss

                  IF nss_isr = reg_3.nss THEN
                     LET prueba_monto = prueba_monto + reg_3.monto_en_pesos
                     CONTINUE FOREACH
                  END IF
               ELSE
                  LET nss_isr = " "
                  LET prueba_monto = 0
                  LET prueba_monto2 = 0
                  LET nss_isr = reg_3.nss

                  IF nss_isr = reg_3.nss THEN
                     LET prueba_monto = prueba_monto + reg_3.monto_en_pesos
                     CONTINUE FOREACH
                  END IF
               END IF
            ELSE
               IF nss_isr <> reg_3.nss THEN
                  IF prueba_monto2 >= x_valida THEN

                     LET xx_sw = 1

                     LET det.folio            = vfolio
                     LET det.consecutivo_reg  = pos
                     LET det.tipo_reporte     = xx_tipo_reporte
                     LET det.periodo_reporte  = x_fecha_periodo
                     LET det.org_supervisor   = "001004"
                     LET det.clave_financiera = "004",x_cod_afo CLIPPED
                     LET det.localidad        = "01001002"
                     LET det.sucursal         = "00000000"
                     LET det.tipo_operacion   = x_tipo_operacion
                     LET det.instrumento_mone = "01"
                     LET det.nss              = nss_isr
                     LET det.monto_en_pesos   = prueba_monto2     --- reg_3.monto_en_pesos
                     LET det.moneda           = "1"
                     LET det.fecha_operacion  = fecha_conv_isr
                     LET det.fecha_deteccion  = NULL
                   --  LET det.nacionalidad     = "1" #CPL-1356
                     LET det.tipo_persona     = "1"
                     LET det.actividad_eco    = "8944098"
                     #LET det.condicion_cod    = x_condicion_cod
                     LET det.condicion_cod    = 310
                     LET det.estado           = 10

            LET det.nacionalidad = "" 
              
              #CPL-1356
               SELECT nacionalidad
               INTO p_iso3
               FROM afi_mae_afiliado
               WHERE n_seguro =det.nss
               
               IF p_iso3 IS NULL THEN 
                LET p_iso3 = 0
               END IF 
               
               SELECT 'X'
               FROM tab_pais
               WHERE pais_cod = p_iso3
               
               IF SQLCA.SQLCODE <> 0 THEN
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
             ELSE 
             	 
             	 SELECT iso_2
               INTO det.nacionalidad
               FROM tab_pais_lav                   
               WHERE iso_3 = p_iso3
               
               IF det.nacionalidad IS NULL THEN 
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES EN LAVADO, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
              END IF    
            
            END IF 
              
              ##CPL-1356
               
                     EXECUTE query9 USING det.folio,
                                          det.nss,
                                          det.consecutivo_reg,
                                          det.tipo_reporte,
                                          det.periodo_reporte,
                                          det.org_supervisor,
                                          det.clave_financiera,
                                          det.localidad,
                                          det.sucursal,
                                          det.tipo_operacion,
                                          det.instrumento_mone,
                                          det.monto_en_pesos,
                                          det.moneda,
                                          det.fecha_operacion,
                                          det.fecha_deteccion,
                                          det.nacionalidad,
                                          det.tipo_persona,
                                          det.actividad_eco,
                                          det.condicion_cod,
                                          det.estado

                     LET pos = pos + 1

                     LET nss_isr = " "
                     LET prueba_monto = 0
                     LET prueba_monto2 = 0
                     LET nss_isr = reg_3.nss
                  END IF
               ELSE
                  IF prueba_monto2 >= x_valida THEN

                     LET xx_sw = 1

                     LET det.folio            = vfolio
                     LET det.consecutivo_reg  = pos
                     LET det.tipo_reporte     = xx_tipo_reporte
                     LET det.periodo_reporte  = x_fecha_periodo
                     LET det.org_supervisor   = "001004"
                     LET det.clave_financiera = "004",x_cod_afo CLIPPED
                     LET det.localidad        = "01001002"
                     LET det.sucursal         = "00000000"
                     LET det.tipo_operacion   = x_tipo_operacion
                     LET det.instrumento_mone = "01"
                     LET det.nss              = nss_isr
                     LET det.monto_en_pesos   = prueba_monto2 --- reg_3.monto_en_pesos
                     LET det.moneda           = "1"
                     LET det.fecha_operacion  = fecha_conv_isr
                     LET det.fecha_deteccion  = NULL
                    -- LET det.nacionalidad     = "1" #CPL-1356
                     LET det.tipo_persona     = "1"
                     LET det.actividad_eco    = "8944098"
                     #LET det.condicion_cod    = x_condicion_cod
                     LET det.condicion_cod    = 310
                     LET det.estado           = 10
            
            LET det.nacionalidad = "" 
              
              #CPL-1356
               SELECT nacionalidad
               INTO p_iso3
               FROM afi_mae_afiliado
               WHERE n_seguro =det.nss
               
               IF p_iso3 IS NULL THEN 
                LET p_iso3 = 0
               END IF 
               
               SELECT 'X'
               FROM tab_pais
               WHERE pais_cod = p_iso3
               
               IF SQLCA.SQLCODE <> 0 THEN
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
             ELSE 
             	 
             	 SELECT iso_2
               INTO det.nacionalidad
               FROM tab_pais_lav                   
               WHERE iso_3 = p_iso3
               
               IF det.nacionalidad IS NULL THEN 
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES EN LAVADO, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
              END IF    
            
            END IF 
              
              ##CPL-1356
               
                     EXECUTE query9 USING det.folio,
                                          det.nss,
                                          det.consecutivo_reg,
                                          det.tipo_reporte,
                                          det.periodo_reporte,
                                          det.org_supervisor,
                                          det.clave_financiera,
                                          det.localidad,
                                          det.sucursal,
                                          det.tipo_operacion,
                                          det.instrumento_mone,
                                          det.monto_en_pesos,
                                          det.moneda,
                                          det.fecha_operacion,
                                          det.fecha_deteccion,
                                          det.nacionalidad,
                                          det.tipo_persona,
                                          det.actividad_eco,
                                          det.condicion_cod,
                                          det.estado

                    LET pos = pos + 1

                    LET nss_isr = " "
                    LET prueba_monto  = 0
                    LET prueba_monto2 = 0
                    LET nss_isr = reg_3.nss
                 END IF
              END IF

              LET x_tipo_operacion = "20"
              LET prueba_monto     = reg_3.monto_en_pesos
           END IF

           IF prueba_monto >= x_valida AND x_tipo_operacion = "20" THEN

              LET xx_sw = 1

              LET det.folio            = vfolio
              LET det.consecutivo_reg  = pos
              LET det.tipo_reporte     = xx_tipo_reporte
              LET det.periodo_reporte  = x_fecha_periodo
              LET det.org_supervisor   = "001004"
              LET det.clave_financiera = "004",x_cod_afo CLIPPED
              LET det.localidad        = "01001002"
              LET det.sucursal         = "00000000"
              LET det.tipo_operacion   = x_tipo_operacion
              LET det.instrumento_mone = "01"
              LET det.nss              = reg_3.nss
              LET det.monto_en_pesos   = prueba_monto   --- reg_3.monto_en_pesos
              LET det.moneda           = "1"
              LET det.fecha_operacion  = reg_3.fecha_conversion
              LET det.fecha_deteccion  = NULL
             -- LET det.nacionalidad     = "1" #CPL-1356
              LET det.tipo_persona     = "1"
              LET det.actividad_eco    = "8944098"
              #LET det.condicion_cod    = x_condicion_cod
              LET det.condicion_cod    = 310
              LET det.estado           = 10

             LET det.nacionalidad = "" 
              
              #CPL-1356
               SELECT nacionalidad
               INTO p_iso3
               FROM afi_mae_afiliado
               WHERE n_seguro =det.nss
               
               IF p_iso3 IS NULL THEN 
                LET p_iso3 = 0
               END IF 
               
               SELECT 'X'
               FROM tab_pais
               WHERE pais_cod = p_iso3
               
               IF SQLCA.SQLCODE <> 0 THEN
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
             ELSE 
             	 
             	 SELECT iso_2
               INTO det.nacionalidad
               FROM tab_pais_lav                   
               WHERE iso_3 = p_iso3
               
               IF det.nacionalidad IS NULL THEN 
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES EN LAVADO, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
              END IF    
            
            END IF 
              
              ##CPL-1356
               
              EXECUTE query9 USING det.folio,
                                   det.nss,
                                   det.consecutivo_reg,
                                   det.tipo_reporte,
                                   det.periodo_reporte,
                                   det.org_supervisor,
                                   det.clave_financiera,
                                   det.localidad,
                                   det.sucursal,
                                   det.tipo_operacion,
                                   det.instrumento_mone,
                                   det.monto_en_pesos,
                                   det.moneda,
                                   det.fecha_operacion,
                                   det.fecha_deteccion,
                                   det.nacionalidad,
                                   det.tipo_persona,
                                   det.actividad_eco,
                                   det.condicion_cod,
                                   det.estado

              LET pos = pos + 1

           END IF

           LET nss_isr = " "
           LET reg_3.monto_en_pesos = 0
           LET prueba_monto = 0

        END FOREACH

        IF reg_3.tipo_movimiento = 490 OR reg_3.tipo_movimiento = 10 THEN
           LET x_tipo_operacion = "21"

           IF prueba_monto2 >= x_valida THEN

              LET xx_sw = 1

              LET det.folio            = vfolio
              LET det.consecutivo_reg  = pos
              LET det.tipo_reporte     = xx_tipo_reporte
              LET det.periodo_reporte  = x_fecha_periodo
              LET det.org_supervisor   = "001004"
              LET det.clave_financiera = "004",x_cod_afo CLIPPED
              LET det.localidad        = "01001002"
              LET det.sucursal         = "00000000"
              LET det.tipo_operacion   = x_tipo_operacion
              LET det.instrumento_mone = "01"
              LET det.nss              = nss_isr
              LET det.monto_en_pesos   = prueba_monto2     --- reg_3.monto_en_pesos
              LET det.moneda           = "1"
              LET det.fecha_operacion  = fecha_conv_isr
              LET det.fecha_deteccion  = NULL
           --   LET det.nacionalidad     = "1" #CPL-1356
              LET det.tipo_persona     = "1"
              LET det.actividad_eco    = "8944098"
              #LET det.condicion_cod    = x_condicion_cod
              LET det.condicion_cod    = 310
              LET det.estado           = 10

             LET det.nacionalidad = "" 
              
              #CPL-1356
               SELECT nacionalidad
               INTO p_iso3
               FROM afi_mae_afiliado
               WHERE n_seguro =det.nss
               
               IF p_iso3 IS NULL THEN 
                LET p_iso3 = 0
               END IF 
               
               SELECT 'X'
               FROM tab_pais
               WHERE pais_cod = p_iso3
               
               IF SQLCA.SQLCODE <> 0 THEN
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
             ELSE 
             	 
             	 SELECT iso_2
               INTO det.nacionalidad
               FROM tab_pais_lav                   
               WHERE iso_3 = p_iso3
               
               IF det.nacionalidad IS NULL THEN 
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES EN LAVADO, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
              END IF    
            
            END IF 
              
              ##CPL-1356
               
              EXECUTE query9 USING det.folio,
                                   det.nss,
                                   det.consecutivo_reg,
                                   det.tipo_reporte,
                                   det.periodo_reporte,
                                   det.org_supervisor,
                                   det.clave_financiera,
                                   det.localidad,
                                   det.sucursal,
                                   det.tipo_operacion,
                                   det.instrumento_mone,
                                   det.monto_en_pesos,
                                   det.moneda,
                                   det.fecha_operacion,
                                   det.fecha_deteccion,
                                   det.nacionalidad,
                                   det.tipo_persona,
                                   det.actividad_eco,
                                   det.condicion_cod,
                                   det.estado
           END IF
        END IF
     END IF

     IF reg_4.condicion_cod = 310 THEN

        LET pos = 1

        DECLARE cursor_320a CURSOR FOR query3
        FOREACH cursor_320a USING x_fecha_inicio,
                                  x_fecha_fin
                            INTO  reg_3.folio,
                                  reg_3.tipo_movimiento,
                                  reg_3.nss,
                                  reg_3.monto_en_pesos,
                                  reg_3.fecha_conversion,
                                  reg_3.sucursal

           SELECT "X"
           FROM   com_nomina
           WHERE  nss = reg_3.nss
           GROUP BY 1

           IF SQLCA.SQLCODE <> 0 THEN
              CONTINUE FOREACH
           END IF

           SELECT nss,
                  COUNT(*)
           INTO   x_cuenta_nss,
                  cuenta
           FROM   dis_cuenta
           WHERE  nss = reg_3.nss
           AND    subcuenta IN (3,10)
           AND    tipo_movimiento  = 1
           AND    fecha_conversion BETWEEN x_fecha_inicio AND x_fecha_fin
           AND    id_aportante[1,3] NOT IN ('MQ-','MC-','UM-','UQ-','MA-','MS-','AF-','AC-','UR-','US-','AA-','AS-','UY-','UZ-','COR')
           GROUP BY 1

           IF reg_4.monto_desde = cuenta THEN
              CONTINUE FOREACH
           END IF

           IF reg_3.tipo_movimiento = 10 OR reg_3.tipo_movimiento = 490 THEN
              CONTINUE FOREACH
           END IF

           LET x_tipo_operacion = "20"

           LET xx_sw = 1

           LET det.folio            = vfolio
           LET det.consecutivo_reg  = pos
           LET det.tipo_reporte     = xx_tipo_reporte
           LET det.periodo_reporte  = x_fecha_periodo
           LET det.org_supervisor   = "001004"
           LET det.clave_financiera = "004",x_cod_afo CLIPPED
           LET det.localidad        = "01001002"
           LET det.sucursal         = "00000000"
           LET det.tipo_operacion   = x_tipo_operacion
           LET det.instrumento_mone = "01"
           LET det.nss              = reg_3.nss
           LET det.monto_en_pesos   = reg_3.monto_en_pesos
           LET det.moneda           = "1"
           LET det.fecha_operacion  = reg_3.fecha_conversion
           LET det.fecha_deteccion  = NULL
         --  LET det.nacionalidad     = "1" #CPL-1356
           LET det.tipo_persona     = "1"
           LET det.actividad_eco    = "8944098"
           LET det.condicion_cod    = 310
           LET det.estado           = 10
             
             LET det.nacionalidad = "" 
              
              #CPL-1356
               SELECT nacionalidad
               INTO p_iso3
               FROM afi_mae_afiliado
               WHERE n_seguro =det.nss
               
               IF p_iso3 IS NULL THEN 
                LET p_iso3 = 0
               END IF 
               
               SELECT 'X'
               FROM tab_pais
               WHERE pais_cod = p_iso3
               
               IF SQLCA.SQLCODE <> 0 THEN
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
             ELSE 
             	 
             	 SELECT iso_2
               INTO det.nacionalidad
               FROM tab_pais_lav                   
               WHERE iso_3 = p_iso3
               
               IF det.nacionalidad IS NULL THEN 
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES EN LAVADO, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
              END IF    
            
            END IF 
              
              ##CPL-1356
               
           EXECUTE query9 USING det.folio,
                                det.nss,
                                det.consecutivo_reg,
                                det.tipo_reporte,
                                det.periodo_reporte,
                                det.org_supervisor,
                                det.clave_financiera,
                                det.localidad,
                                det.sucursal,
                                det.tipo_operacion,
                                det.instrumento_mone,
                                det.monto_en_pesos,
                                det.moneda,
                                det.fecha_operacion,
                                det.fecha_deteccion,
                                det.nacionalidad,
                                det.tipo_persona,
                                det.actividad_eco,
                                det.condicion_cod,
                                det.estado
           LET pos = pos + 1
        END FOREACH
     END IF

     IF reg_4.condicion_cod = 310 THEN

        DECLARE cursor_330a CURSOR FOR query3

        LET pos = 1

        FOREACH cursor_330a USING x_fecha_inicio,
                                  x_fecha_fin
                            INTO reg_3.folio,
                                 reg_3.tipo_movimiento,
                                 reg_3.nss,
                                 reg_3.monto_en_pesos,
                                 reg_3.fecha_conversion,
                                 reg_3.sucursal

           SELECT "X"
           FROM   com_nomina
           WHERE  nss = reg_3.nss
           GROUP BY 1

           IF SQLCA.SQLCODE <> 0 THEN
              CONTINUE FOREACH
           END IF

           IF reg_3.tipo_movimiento = 490 OR reg_3.tipo_movimiento = 10 THEN
              LET x_tipo_operacion = "21"
              LET reg_3.monto_en_pesos = reg_3.monto_en_pesos * (-1)

               IF nss_isr = " " THEN
                  LET prueba_monto = 0
                  LET nss_isr = reg_3.nss
               END IF

               IF reg_3.nss MATCHES nss_isr THEN

                  LET fecha_conv_isr = reg_3.fecha_conversion

                  IF prueba_monto > x_valida THEN
                     LET prueba_monto = prueba_monto + reg_3.monto_en_pesos
                     LET prueba_monto2 = prueba_monto
                  ELSE
                     LET prueba_monto = prueba_monto + reg_3.monto_en_pesos
                     LET prueba_monto2 = prueba_monto
                  END IF

                  CONTINUE FOREACH
               END IF
           ELSE
              LET x_tipo_operacion = "20"
           END IF

           DECLARE cursor_340afi CURSOR FOR query2
           OPEN cursor_340afi USING reg_3.nss
              FETCH cursor_340afi INTO afi_nombres,
                                       afi_paterno,
                                       afi_materno,
                                       afi_n_rfc,
                                       afi_n_unico,
                                       afi_fena,
                                       afi_n_folio,
                                       afi_tipo_solicitud
           CLOSE cursor_340afi
           DECLARE cursor_340dom CURSOR FOR query13

           OPEN cursor_340dom USING reg_3.nss,
                                    afi_n_folio,
                                    afi_tipo_solicitud
              FETCH cursor_340dom INTO dom_calle,
                                       dom_numero,
                                       dom_depto,
                                       dom_codpos,
                                       dom_colonia,
                                       dom_ciudad,
                                       dom_estado
           CLOSE cursor_340dom

           LET xx_sw = 1

           LET det.folio            = vfolio
           LET det.consecutivo_reg  = pos
           LET det.tipo_reporte     = xx_tipo_reporte
           LET det.periodo_reporte  = x_fecha_periodo
           LET det.org_supervisor   = "001004"
           LET det.clave_financiera = "004",x_cod_afo CLIPPED
           LET det.localidad        = "01001002"
           LET det.sucursal         = "00000000"
           LET det.tipo_operacion   = x_tipo_operacion
           LET det.instrumento_mone = "01"
           LET det.nss              = reg_3.nss
           LET det.monto_en_pesos   = reg_3.monto_en_pesos
           LET det.moneda           = "1"
           LET det.fecha_operacion  = reg_3.fecha_conversion
           LET det.fecha_deteccion  = NULL
       --    LET det.nacionalidad     = "1" #CPL-1356
           LET det.tipo_persona     = "1"
           LET det.actividad_eco    = "8944098"
           LET det.condicion_cod    = 310
           LET det.estado           = 10

             LET det.nacionalidad = "" 
              
              #CPL-1356
               SELECT nacionalidad
               INTO p_iso3
               FROM afi_mae_afiliado
               WHERE n_seguro =det.nss
               
               IF p_iso3 IS NULL THEN 
                LET p_iso3 = 0
               END IF 
               
               SELECT 'X'
               FROM tab_pais
               WHERE pais_cod = p_iso3
               
               IF SQLCA.SQLCODE <> 0 THEN
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
             ELSE 
             	 
             	 SELECT iso_2
               INTO det.nacionalidad
               FROM tab_pais_lav                   
               WHERE iso_3 = p_iso3
               
               IF det.nacionalidad IS NULL THEN 
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES EN LAVADO, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
              END IF    
            
            END IF 
              
              ##CPL-1356
               
           EXECUTE query9 USING det.folio,
                                det.nss,
                                det.consecutivo_reg,
                                det.tipo_reporte,
                                det.periodo_reporte,
                                det.org_supervisor,
                                det.clave_financiera,
                                det.localidad,
                                det.sucursal,
                                det.tipo_operacion,
                                det.instrumento_mone,
                                det.monto_en_pesos,
                                det.moneda,
                                det.fecha_operacion,
                                det.fecha_deteccion,
                                det.nacionalidad,
                                det.tipo_persona,
                                det.actividad_eco,
                                det.condicion_cod,
                                det.estado

           LET pos = pos + 1

           LET nss_isr = " "
           LET reg_3.monto_en_pesos = 0
           LET prueba_monto = 0

        END FOREACH
     END IF

     IF reg_4.condicion_cod = 310 THEN

        DECLARE cursor_340a CURSOR FOR query34

        LET pos = 1

        FOREACH cursor_340a USING x_fecha_inicio,
                                  x_fecha_fin
                             INTO reg_3.id_aportante,
                                  reg_3.nss,
                                  reg_3.monto_en_pesos,
                                  reg_3.fecha_conversion,
                                  reg_3.sucursal

          SELECT "X"
          FROM   com_nomina
          WHERE  nss = reg_3.nss
          GROUP BY 1

          IF SQLCA.SQLCODE <> 0 THEN
             CONTINUE FOREACH
          END IF

          IF reg_3.id_aportante = "RETIRO" THEN
             LET x_tipo_operacion = "21"
             CONTINUE FOREACH
          ELSE
             LET x_tipo_operacion = "20"
          END IF

          DECLARE cursor_340aafi CURSOR FOR query2

          OPEN cursor_340aafi USING reg_3.nss

          FETCH cursor_340aafi INTO afi_nombres,
                                    afi_paterno,
                                    afi_materno,
                                    afi_n_rfc,
                                    afi_n_unico,
                                    afi_fena,
                                    afi_n_folio,
                                    afi_tipo_solicitud

          CLOSE cursor_340aafi

          DECLARE cursor_340aadom CURSOR FOR query13
               OPEN cursor_340dom USING reg_3.nss,
                                        afi_n_folio,
                                        afi_tipo_solicitud
               FETCH cursor_340dom INTO dom_calle,
                                        dom_numero,
                                        dom_depto,
                                        dom_codpos,
                                        dom_colonia,
                                        dom_ciudad,
                                        dom_estado
               CLOSE cursor_340dom

               DECLARE cursor_sal13 CURSOR FOR query_sal1
               OPEN cursor_sal13 USING reg_3.nss
               FETCH cursor_sal13 INTO xx_fecha_recepcion
               CLOSE cursor_sal13

               DECLARE cursor_sal23 CURSOR FOR query_sal2
               OPEN cursor_sal23 USING reg_3.nss,
                                      xx_fecha_recepcion
               FETCH cursor_sal23 INTO x_max_consecutivo
               CLOSE cursor_sal23

               DECLARE cursor_sal33 CURSOR FOR query_sal3
               OPEN cursor_sal33 USING reg_3.nss,
                                      x_max_consecutivo,
                                      xx_fecha_recepcion
               FETCH cursor_sal33 INTO xx_salario_mes_df
               CLOSE cursor_sal33

               LET numero_salarios = reg_4.monto_desde

               LET x_valida4 = xx_salario_mes_df * numero_salarios

               IF reg_3.monto_en_pesos > x_valida4 THEN

                  LET xx_sw = 1

                  LET det.folio            = vfolio
                  LET det.consecutivo_reg  = pos
                  LET det.tipo_reporte     = xx_tipo_reporte
                  LET det.periodo_reporte  = x_fecha_periodo
                  LET det.org_supervisor   = "001004"
                  LET det.clave_financiera = "004",x_cod_afo CLIPPED
                  LET det.localidad        = "01001002"
                  LET det.sucursal         = "00000000"
                  LET det.tipo_operacion   = x_tipo_operacion
                  LET det.instrumento_mone = "01"
                  LET det.nss              = reg_3.nss
                  LET det.monto_en_pesos   = reg_3.monto_en_pesos
                  LET det.moneda           = "1"
                  LET det.fecha_operacion  = reg_3.fecha_conversion
                  LET det.fecha_deteccion  = NULL
              --    LET det.nacionalidad     = "1" #CPL-1356
                  LET det.tipo_persona     = "1"
                  LET det.actividad_eco    = "8944098"
                  #LET det.condicion_cod    = 310
                  LET det.condicion_cod    = reg_4.condicion_cod
                  LET det.estado           = 10

             LET det.nacionalidad = "" 
              
              #CPL-1356
               SELECT nacionalidad
               INTO p_iso3
               FROM afi_mae_afiliado
               WHERE n_seguro =det.nss
               
               IF p_iso3 IS NULL THEN 
                LET p_iso3 = 0
               END IF 
               
               SELECT 'X'
               FROM tab_pais
               WHERE pais_cod = p_iso3
               
               IF SQLCA.SQLCODE <> 0 THEN
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
             ELSE 
             	 
             	 SELECT iso_2
               INTO det.nacionalidad
               FROM tab_pais_lav                   
               WHERE iso_3 = p_iso3
               
               IF det.nacionalidad IS NULL THEN 
               DISPLAY "SE DEBE ACTUALIZAR EL CATÁLOGO DE PAISES EN LAVADO, EL CODIGO", p_iso3, "NO EXISTE"
               	LET det.nacionalidad = "ND"
              END IF    
            
            END IF 
              
              ##CPL-1356
               
                  EXECUTE query9 USING det.folio,
                                       det.nss,
                                       det.consecutivo_reg,
                                       det.tipo_reporte,
                                       det.periodo_reporte,
                                       det.org_supervisor,
                                       det.clave_financiera,
                                       det.localidad,
                                       det.sucursal,
                                       det.tipo_operacion,
                                       det.instrumento_mone,
                                       det.monto_en_pesos,
                                       det.moneda,
                                       det.fecha_operacion,
                                       det.fecha_deteccion,
                                       det.nacionalidad,
                                       det.tipo_persona,
                                       det.actividad_eco,
                                       det.condicion_cod,
                                       det.estado

                  LET pos = pos + 1

                  LET reg_3.monto_en_pesos = 0
               END IF
            END FOREACH
         END IF
         LET i = i + 1
      END FOREACH
   END IF

{ #desabilitadas
      DECLARE cursor_301 CURSOR FOR query34

      LET pos = 1

      FOREACH cursor_301 USING x_fecha_inicio,
                               x_fecha_fin
                         INTO  reg_3.id_aportante,
                               reg_3.nss,
                               reg_3.monto_en_pesos,
                               reg_3.fecha_conversion
                               #reg_3.sucursal

         SELECT "X"
         FROM   com_nomina
         WHERE  nss = reg_3.nss
         GROUP BY 1

         IF SQLCA.SQLCODE <> 0 THEN
            CONTINUE FOREACH
         END IF

         IF reg_3.id_aportante = "RETIRO" THEN
            LET x_tipo_operacion = "21"
            LET reg_3.monto_en_pesos = reg_3.monto_en_pesos * (-1)
         ELSE
            LET x_tipo_operacion = "20"
         END IF

         LET xx_sw = 1

         LET det.folio            = vfolio
         LET det.consecutivo_reg  = pos
         LET det.tipo_reporte     = xx_tipo_reporte
         LET det.periodo_reporte  = x_fecha_periodo
         LET det.org_supervisor   = "001004"
         LET det.clave_financiera = "004",x_cod_afo CLIPPED
         LET det.localidad        = "01001002"
         LET det.sucursal         = "00000000"
         LET det.tipo_operacion   = x_tipo_operacion
         LET det.instrumento_mone = "01"
         LET det.nss              = reg_3.nss
         LET det.monto_en_pesos   = reg_3.monto_en_pesos
         LET det.moneda           = "1"
         LET det.fecha_operacion  = reg_3.fecha_conversion
         LET det.fecha_deteccion  = NULL
         LET det.nacionalidad     = "1"
         LET det.tipo_persona     = "1"
         LET det.actividad_eco    = "8944098"
         LET det.condicion_cod    = x_condicion_cod
         LET det.estado           = 10

         EXECUTE query9 USING det.folio,
                              det.nss,
                              det.consecutivo_reg,
                              det.tipo_reporte,
                              det.periodo_reporte,
                              det.org_supervisor,
                              det.clave_financiera,
                              det.localidad,
                              det.sucursal,
                              det.tipo_operacion,
                              det.instrumento_mone,
                              det.monto_en_pesos,
                              det.moneda,
                              det.fecha_operacion,
                              det.fecha_deteccion,
                              det.nacionalidad,
                              det.tipo_persona,
                              det.actividad_eco,
                              det.condicion_cod,
                              det.estado

         LET pos = pos + 1

         LET reg_3.monto_en_pesos = 0

      END FOREACH
   END IF
}
##########################
   DECLARE cursor_7 CURSOR FOR query8

   LET pos = 1

   FOREACH cursor_7 USING vfolio INTO act_nss,
                                      act_consecutivo_reg,
                                      act_tipo_operacion,
                                      act_condicion_cod

      WHENEVER ERROR CONTINUE

      EXECUTE query12 USING pos,
                            vfolio,
                            act_nss,
                            act_consecutivo_reg,
                            act_condicion_cod,
                            act_tipo_operacion

      IF SQLCA.SQLCODE < 0 THEN
         LET x_error = "UPDATE lav_det_lavado:",
                       " nss: ",act_nss,
                       " folio: ",vfolio,
                       " consecutivo_reg: ",act_consecutivo_reg,
                       err_get(SQLCA.SQLCODE)

         CALL errorlog(x_error CLIPPED)

         ERROR "NO SE PUEDE ACTUALIZAR EL REGISTRO LLAME A SISTEMAS"
         PROMPT "" FOR opc
         EXIT PROGRAM
      END IF
      WHENEVER ERROR STOP

      LET pos = pos + 1

   END FOREACH
--osb
{
   INSERT INTO lav_fechas_periodo
   VALUES(vfolio,
          xx_tipo_reporte,
          x_fecha_inicio,
          x_fecha_fin)
}
   RETURN vfolio,
          xx_sw

END FUNCTION
################################################################
FUNCTION crea_tabla_temp()

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_cuenta_lav

      CREATE TEMP TABLE tmp_cuenta_lav
      (nss               CHAR(11),
       tipo_movimiento   SMALLINT,
       fecha_conversion  DATE,
       monto_en_pesos    DECIMAL(16,6)
      )

   WHENEVER ERROR STOP
END FUNCTION
################################################################
FUNCTION consulta()

   DEFINE tipo_reporte        SMALLINT,
          x_nss               CHAR(11),
          periodo_reporte     CHAR(8),
          pos                 SMALLINT,
          row_cnt             SMALLINT

   DEFINE sql_stat            INTEGER,
          item_row_cnt        SMALLINT,
          cont_inp            SMALLINT,
          cur_row             SMALLINT,
          scr_row             SMALLINT,
          p_cur_row           SMALLINT

   DEFINE w          SMALLINT,
          x_ano      SMALLINT,
          xxx_ano    SMALLINT,
          ano_cuenta CHAR(4),
          sql_text   CHAR(650)

   CALL crea_tabla_temp()

   OPEN WINDOW ventana3 AT 5,2 WITH FORM "LAVB0019"

   DISPLAY " < ESC > Consultar                                                           " AT 1,1 ATTRIBUTE(REVERSE)

   DISPLAY "                         CONSULTA DE OPERACIONES                             " AT 6,1 ATTRIBUTE(REVERSE)

   LET int_flag = FALSE

   CONSTRUCT cla_where ON tipo_reporte,
                          nss,
                          periodo_reporte
                     FROM tipo_reporte,
                          x_nss,
                          periodo_reporte

      ON KEY (ESC)
         ERROR "PROCESANDO INFORMACION..."
         LET int_flag = FALSE
         EXIT CONSTRUCT
      ON KEY (CONTROL-C)
         LET int_flag = TRUE
         EXIT CONSTRUCT
   END CONSTRUCT

   IF int_flag = TRUE THEN
      LET int_flag = FALSE
      ERROR "BUSQUEDA CANCELADA..."
      CLEAR SCREEN
      CLOSE WINDOW ventana3
      RETURN
   END IF

   LET txt_sel = " SELECT '',* ",
                 " FROM   lav_det_lavado ",
                 " WHERE  ",cla_where CLIPPED,
                 " ORDER BY 3,4 "

   PREPARE query18 FROM txt_sel

   DECLARE cursor_consulta CURSOR FOR query18

   LET i = 1
   LET x_ano = YEAR (TODAY)

   FOREACH cursor_consulta INTO reg_9[i].*

      LET ga_1[i].trap             = reg_9[i].trap
      LET ga_1[i].folio            = reg_9[i].folio
      LET ga_1[i].nss              = reg_9[i].nss
      LET ga_1[i].tipo_operacion   = reg_9[i].tipo_operacion
      LET ga_1[i].fecha_movimiento = reg_9[i].fecha_operacion
      LET ga_1[i].monto_en_pesos   = reg_9[i].monto_en_pesos
      LET ga_1[i].condicion_cod    = reg_9[i].condicion_cod
      LET ga_1[i].estado           = reg_9[i].estado

      LET xxx_ano = YEAR (ga_1[i].fecha_movimiento)

      LET w = 0

      WHENEVER ERROR CONTINUE

      FOR w = 1997 TO xxx_ano

         LET ano_cuenta = w

         IF ano_cuenta = x_ano THEN
            LET ano_cuenta = "   "
         END IF

         LET sql_text =  " INSERT INTO tmp_cuenta_lav ",
                      " SELECT nss,",
                              "tipo_movimiento,",
                              "fecha_conversion,",
                              "monto_en_pesos ",
                      " FROM   dis_cuenta",ano_cuenta[3,4] CLIPPED,
                      " WHERE  nss = ","'",ga_1[i].nss CLIPPED,"'",
                      " AND    subcuenta in (3,10) ",
                      " AND    tipo_movimiento IN (1,10,490) ",
           "AND    id_aportante[1,3] NOT IN ('MQ-','MC-','UM-','UQ-','MA-','MS-','AF-','AC-','UR-','US-','AA-','AS-','UY-','UZ-','COR')"

         PREPARE cta_execute FROM sql_text

         EXECUTE cta_execute
      END FOR


      DECLARE cursor_confx CURSOR FOR query2
      OPEN cursor_confx USING reg_9[i].nss
      FETCH cursor_confx INTO afi_nombres,
                              afi_paterno,
                              afi_materno,
                              afi_n_rfc,
                              afi_n_unico,
                              afi_fena,
                              afi_n_folio,
                              afi_tipo_solicitud
      CLOSE cursor_confx

      LET ga_1[i].nombres = afi_nombres CLIPPED," ",afi_paterno CLIPPED

      LET i = i + 1
   END FOREACH

   CREATE INDEX tmp_cuenta_1 ON tmp_cuenta_lav (nss,
                                            tipo_movimiento,
                                            fecha_conversion)

   UPDATE STATISTICS FOR TABLE tmp_cuenta_lav

   WHENEVER ERROR STOP

   ERROR ""

   LET i = i - 1

   IF (i) >= 1 THEN
      CALL SET_COUNT(i)

      LET cont_inp = TRUE

      WHILE (cont_inp = TRUE)

         INPUT ARRAY ga_1 WITHOUT DEFAULTS FROM scr_1.*
         ATTRIBUTES(MAXCOUNT = i,COUNT = i)

            AFTER ROW
               LET cur_row = ARR_CURR()
               LET scr_row = SCR_LINE()

               DISPLAY ga_1[cur_row].* TO scr_1[scr_row].*

            BEFORE ROW
               LET cur_row = ARR_CURR()
               LET scr_row = SCR_LINE()

               IF (cur_row = i + 1) THEN
                  LET cont_inp = TRUE
--                  EXIT INPUT
               ELSE
                  LET scr_row = SCR_LINE()

                  DISPLAY ga_1[cur_row].* TO scr_1[scr_row].*
                  ATTRIBUTE (REVERSE)

                  LET cont_inp = FALSE

                  CALL proc_items(cur_row)
                       RETURNING sql_stat,
                                 item_row_cnt
               END IF

            ON KEY(CONTROL-V)
               CALL disp_array_items(item_row_cnt)

            ON KEY (CONTROL-M)
               LET i = ARR_CURR()

               IF reg_2[i].tipo_reporte = 1 THEN
                  ERROR "ESTE TIPO DE OPERACION NO TIENE DETALLE..."
               ELSE
                  CALL consulta_desc(ga_1[i].folio,
                                     ga_1[i].nss,
                                     reg_9[i].consecutivo_reg,
                                     reg_9[i].tipo_reporte
                                     )
               END IF

            ON KEY (CONTROL-C)
               EXIT INPUT
         END INPUT
      END WHILE
   ELSE
      ERROR "REGISTROS DE LA OPERACION... VACIO"
      SLEEP 2
      ERROR ""
   END IF

   DELETE FROM  tmp_cuenta_lav

   CLEAR SCREEN
   CLOSE WINDOW ventana3

END FUNCTION
###############################################################
FUNCTION clasifica()

   DEFINE tipo_reporte        SMALLINT,
          x_nss               CHAR(11),
          periodo_reporte     CHAR(8),
          pos                 SMALLINT,
          row_cnt             SMALLINT

   DEFINE sql_stat            INTEGER,
          item_row_cnt        SMALLINT,
          cont_inp            SMALLINT,
          cur_row             SMALLINT,
          scr_row             SMALLINT,
          p_cur_row           SMALLINT

   DEFINE w          SMALLINT,
          x_ano      SMALLINT,
          xxx_ano    SMALLINT,
          ano_cuenta CHAR(4),
          sql_text   CHAR(650),
          salir      SMALLINT

   DEFINE ga_1x ARRAY[5000]    OF RECORD
          folio              DECIMAL(11,0),
          nss                CHAR(11),
          nombres            LIKE  afi_mae_afiliado.nombres,
          tipo_operacion     LIKE  lav_det_lavado.tipo_operacion,
          fecha_movimiento   DATE,
          monto_en_pesos     DECIMAL(10,2),
          tipo_validacion    SMALLINT,
          tipo_clasifica     SMALLINT
   END RECORD

   CALL crea_tabla_temp()

   OPEN WINDOW ventana3x AT 5,2 WITH FORM "LAVB00110"
   DISPLAY " < ESC > Detectar registros " AT 1,1
   DISPLAY "                       CLASIFICACION DE TIPO DE PERSONA                        " AT 2,1 ATTRIBUTE(REVERSE)

   LET int_flag = FALSE

   CONSTRUCT cla_where ON tipo_reporte,
                          nss,
                          periodo_reporte
                     FROM tipo_reporte,
                          x_nss,
                          periodo_reporte

      ON KEY (ESC)
         ERROR "PROCESANDO INFORMACION..."
         LET int_flag = FALSE
         EXIT CONSTRUCT
      ON KEY (CONTROL-C)
         LET int_flag = TRUE
         EXIT CONSTRUCT
   END CONSTRUCT

   IF int_flag = TRUE THEN
      LET int_flag = FALSE
      ERROR "BUSQUEDA CANCELADA..."
      CLEAR SCREEN
      CLOSE WINDOW ventana3x
      RETURN
   END IF

   LET salir = FALSE
   WHILE NOT salir

      INITIALIZE ga_1x TO NULL

      LET txt_sel = " SELECT '',* ",
                    " FROM   lav_det_lavado ",
                    " WHERE  ",cla_where CLIPPED,
                    " ORDER BY 1,4 "

      PREPARE query18x FROM txt_sel
      DECLARE cursor_clasifica SCROLL CURSOR FOR query18x

      LET i = 1

      FOREACH cursor_clasifica INTO reg_9[i].*
         IF reg_9[i].folio = 0 OR reg_9[i].folio IS NULL THEN
            EXIT FOREACH
         END IF

         LET ga_1x[i].folio            = reg_9[i].folio
         LET ga_1x[i].nss              = reg_9[i].nss
         LET ga_1x[i].tipo_operacion   = reg_9[i].tipo_operacion
         LET ga_1x[i].fecha_movimiento = reg_9[i].fecha_operacion
         LET ga_1x[i].monto_en_pesos   = reg_9[i].monto_en_pesos
         LET ga_1x[i].tipo_validacion  = reg_9[i].condicion_cod

         SELECT a.tipo_clasifica
         INTO   ga_1x[i].tipo_clasifica
         FROM   lav_det_clasifica a
         WHERE  a.nss   = reg_9[i].nss
         AND    a.folio = reg_9[i].folio
         AND    a.tipo_reporte    = reg_9[i].tipo_reporte
         AND    a.tipo_validacion = reg_9[i].condicion_cod

         DECLARE cursor_x CURSOR FOR query2
         OPEN cursor_x USING reg_9[i].nss
         FETCH cursor_x INTO afi_nombres,
                             afi_paterno,
                             afi_materno,
                             afi_n_rfc,
                             afi_n_unico,
                             afi_fena,
                             afi_n_folio,
                             afi_tipo_solicitud
         CLOSE cursor_x

         LET ga_1x[i].nombres = afi_nombres CLIPPED," ",
                                afi_paterno CLIPPED," ",
                                afi_materno CLIPPED
         LET i = i + 1
      END FOREACH

      ERROR ""
      LET i = i - 1

      IF (i) >= 1 THEN
         CALL SET_COUNT(i)
         DISPLAY "                      SELECCION DEL REGISTRO A CLASIFICAR                      " AT 6,1 ATTRIBUTE(REVERSE)

         DISPLAY ARRAY ga_1x TO scr_1.*
            ON KEY (CONTROL-M)
               LET i = ARR_CURR()

               SELECT "X"
               FROM   lav_det_clasifica b
               WHERE  b.folio = ga_1x[i].folio
               AND    b.nss   = ga_1x[i].nss
               AND    b.tipo_reporte    = reg_9[i].tipo_reporte
               AND    b.tipo_validacion = reg_9[i].condicion_cod
               AND    b.tipo_clasifica  = ga_1x[i].tipo_clasifica

               IF SQLCA.SQLCODE = 0 THEN
                  ERROR "EL REGISTRO NO PUEDE SER CLASIFICADO MAS DE UNA VEZ "
                  SLEEP 2
               ELSE
                  CALL clasifica_desc(ga_1x[i].nss,
                                      reg_9[i].tipo_reporte,
                                      ga_1x[i].nombres)
                  EXIT DISPLAY
               END IF

            ON KEY (INTERRUPT)
               LET salir = TRUE
               EXIT DISPLAY
         END DISPLAY
      ELSE
         ERROR "NO SE ENCONTRARON REGISTRON ..."
         SLEEP 2
         EXIT WHILE
      END IF
   END WHILE

   CLEAR SCREEN
   CLOSE WINDOW ventana3x

END FUNCTION
###############################################################
FUNCTION proc_items(p_cur_row)

   DEFINE sql_stat      INTEGER,
          p_cur_row     SMALLINT,
          item_row_cnt  SMALLINT

   CALL sel_ord_item(p_cur_row)
        RETURNING sql_stat,
                  item_row_cnt

   CALL disp_four_items()

   RETURN sql_stat,
          item_row_cnt

END FUNCTION
###############################################################
FUNCTION disp_array_items(p_item_row_cnt)

   DEFINE p_item_row_cnt SMALLINT

   CALL SET_COUNT(p_item_row_cnt)

   DISPLAY ARRAY ga_2 TO scr_2.*
      ON KEY(ESC)
         EXIT DISPLAY
   END DISPLAY
END FUNCTION
#############################################################
FUNCTION sel_ord_item(p_cur_row)

   DEFINE sql_text   CHAR(650),
          sql_stat   INTEGER,
          p_cur_row  SMALLINT,
          row_cnt    SMALLINT,
          x_tablas   CHAR(20)
{
   DEFINE w          SMALLINT,
          x_ano      SMALLINT,
          xxx_ano    SMALLINT,
          ano_cuenta CHAR(4)
}

   CALL null_items()
{
   CALL crea_tabla_temp()

   LET x_ano = YEAR (TODAY)

   LET xxx_ano = YEAR (ga_1[p_cur_row].fecha_movimiento)

   LET w = 0

   WHENEVER ERROR CONTINUE

   FOR w = 1997 TO xxx_ano

      LET ano_cuenta = w

      IF ano_cuenta = x_ano THEN
         LET ano_cuenta = "   "
      END IF

      LET sql_text =  " INSERT INTO tmp_cuenta_lav ",
                      " SELECT nss,",
                              "tipo_movimiento,",
                              "fecha_conversion,",
                              "monto_en_pesos ",
                      " FROM   dis_cuenta",ano_cuenta[3,4] CLIPPED,
                      " WHERE  nss = ","'",ga_1[p_cur_row].nss CLIPPED,"'",
                      " AND    subcuenta in (3,10) ",
                      " AND    tipo_movimiento IN (1,10,490) ",
           "AND    id_aportante[1,3] NOT IN ('MQ-','MC-','UM-','UQ-','MA-','MS-','AF-','AC-','UR-','US-','AA-','AS-','UY-','UZ-','COR')"

      PREPARE cta_execute FROM sql_text

      EXECUTE cta_execute
   END FOR

   CREATE INDEX tmp_cuenta_1 ON tmp_cuenta_lav (nss,
                                               tipo_movimiento,
                                               fecha_conversion)

   UPDATE STATISTICS FOR TABLE tmp_cuenta_lav

   WHENEVER ERROR STOP
}
   LET sql_text = " SELECT CASE ",
                        " WHEN tipo_movimiento = 1 THEN 20 ",
                        " WHEN tipo_movimiento = 490 THEN 21 ",
                        " END AS tipo_movimiento ,",
                        " fecha_conversion,",
                        " monto_en_pesos ",
                  " FROM  tmp_cuenta_lav ",
                  " WHERE nss   = ? ",
                  " AND   tipo_movimiento IN (1,490) ",
                  " AND   fecha_conversion <= ? ",
                  " GROUP BY 1,2,3 ",
                  " ORDER BY 2 DESC " CLIPPED

   PREPARE sel_item_stmt FROM sql_text
   DECLARE sel_item_curs CURSOR FOR sel_item_stmt

   WHENEVER ERROR CONTINUE

LET ga_1[p_cur_row].fecha_movimiento = MDY(MONTH(ga_1[p_cur_row].fecha_movimiento),DAY(ga_1[p_cur_row].fecha_movimiento),YEAR(ga_1[p_cur_row].fecha_movimiento))

   OPEN sel_item_curs USING ga_1[p_cur_row].nss,
                            ga_1[p_cur_row].fecha_movimiento

   WHENEVER ERROR STOP

   LET sql_stat = SQLCA.SQLCODE

   LET row_cnt = 1

   WHILE ((NOT sql_stat) AND (row_cnt<= 100))

      WHENEVER ERROR CONTINUE

      FETCH sel_item_curs INTO ga_2[row_cnt].*

      WHENEVER ERROR STOP

      LET sql_stat = SQLCA.SQLCODE

      IF (NOT sql_stat) THEN
         LET row_cnt = row_cnt + 1
      END IF
   END WHILE

   IF (sql_stat = 100) THEN
      LET sql_stat = 0
   END IF

   RETURN sql_stat,
          row_cnt - 1
END FUNCTION
################################################################
FUNCTION disp_four_items()

   DEFINE i SMALLINT

   FOR i = 1 TO 3
      DISPLAY ga_2[i].* TO scr_2[i].*
   END FOR
END FUNCTION
###############################################################
FUNCTION null_items()

   DEFINE i SMALLINT

   INITIALIZE ga_2[1].* TO NULL

   FOR i = 2 TO 100
      LET ga_2[i].* = ga_2[1].*
   END FOR
END FUNCTION
###############################################################
FUNCTION clasifica_desc(l_rec_nss,
                        tipo_reporte,
                        l_rec_nombres)

   DEFINE l_rec_nss            CHAR(11),
          tipo_reporte         SMALLINT,
          tipo_clasifica       SMALLINT,
          l_rec_nombres        CHAR(32),
          pos                  SMALLINT,
          xreporte_desc        CHAR(20)

   DEFINE intimidado             ,
          sobornado              ,
          reportados             ,
          falsos                 ,
          niegan                 ,
          buscados               CHAR(1)

   OPEN WINDOW ventana7 AT 4,2 WITH FORM "LAVB00111"
   DISPLAY " LAVB001                    LAVADO DE DINERO                                 " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "dd/mm/yyyy " AT 3,67 ATTRIBUTE(REVERSE)
   DISPLAY " < ESC > Grabar registro                                    < Ctrl-C > Salir   " AT 2,1
   DISPLAY "                       CLASIFICACION DE TIPO DE PERSONA                        " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY "                      TIPOS DE CLASIFICACION DE PERSONAS                       " AT 6,1 ATTRIBUTE(REVERSE)

   CASE tipo_reporte
      WHEN 1 LET tipo_reporte = 100
      WHEN 2 LET tipo_reporte = 200
      WHEN 3 LET tipo_reporte = 300
   END CASE

   SELECT reporte_desc
   INTO   xreporte_desc
   FROM   tab_tipo_reporte
   WHERE  lav_operacion = tipo_reporte

   DISPLAY l_rec_nss     TO nss
   DISPLAY tipo_reporte  TO lav_operacion
   DISPLAY xreporte_desc TO lav_oper_desc
   DISPLAY l_rec_nombres TO nombre

   CASE tipo_reporte
      WHEN 100 LET tipo_reporte = 1
      WHEN 200 LET tipo_reporte = 2
      WHEN 300 LET tipo_reporte = 3
   END CASE

   LET tipo_clasifica = NULL

   INPUT BY NAME intimidado,
                 sobornado,
                 reportados,
                 falsos,
                 niegan,
                 buscados WITHOUT DEFAULTS

      AFTER FIELD intimidado
         IF intimidado IS NULL THEN
         ELSE
            LET tipo_clasifica = 1
            NEXT FIELD intimidado
         END IF

      AFTER FIELD sobornado
         IF sobornado IS NULL THEN
         ELSE
            LET tipo_clasifica = 2
            NEXT FIELD sobornado
         END IF

      AFTER FIELD reportados
         IF reportados IS NULL THEN
         ELSE
            LET tipo_clasifica = 3
            NEXT FIELD reportados
         END IF

      AFTER FIELD falsos
         IF falsos IS NULL THEN
         ELSE
            LET tipo_clasifica = 4
            NEXT FIELD falsos
         END IF

      AFTER FIELD niegan
         IF niegan IS NULL THEN
         ELSE
            LET tipo_clasifica = 5
            NEXT FIELD niegan
         END IF

      AFTER FIELD buscados
         IF buscados IS NULL THEN
         ELSE
            LET tipo_clasifica = 6
            NEXT FIELD buscados
         END IF

      ON KEY(ESC)
         IF tipo_clasifica IS NULL THEN
            ERROR "DEBE SELECCIONAR UNA OPCION ..."
         ELSE
            SELECT "X"
            FROM   lav_det_clasifica
            WHERE  folio = reg_9[i].folio
            AND    nss   = l_rec_nss
            AND    tipo_reporte    = tipo_reporte
            AND    tipo_validacion = reg_9[i].condicion_cod

            IF SQLCA.SQLCODE = 0 THEN
               UPDATE lav_det_clasifica
               SET    tipo_clasifica = tipo_clasifica
               WHERE  folio = reg_9[i].folio
               AND    nss   = l_rec_nss
               AND    tipo_reporte    = tipo_reporte
               AND    tipo_validacion = reg_9[i].condicion_cod

               EXIT INPUT
            ELSE
               INSERT INTO lav_det_clasifica
                           VALUES(reg_9[i].folio,
                                  l_rec_nss,
                                  tipo_reporte,
                                  reg_9[i].condicion_cod,
                                  tipo_clasifica,
                                  TODAY,
                                  USER)
               EXIT INPUT
            END IF
         END IF

      ON KEY(CONTROL-C)
         EXIT INPUT
   END INPUT

   CLOSE WINDOW ventana7
END FUNCTION
################################################################
FUNCTION consulta_desc(l_rec_folio,
                       l_rec_nss,
                       l_rec_consecutivo,
                       l_rec_tipo_reporte)

   DEFINE l_rec_folio           DECIMAL(11,0),
          l_rec_nss             CHAR(11),
          l_rec_consecutivo     INTEGER,
          l_rec_tipo_reporte    CHAR(1),
          pos                   SMALLINT,
          xfecha_deteccion      DATE

   DEFINE l_record ARRAY[500] OF RECORD
          descripcion        CHAR(60)
   END RECORD

   DEFINE l_record1 ARRAY[500] OF RECORD
          razones             CHAR(60)
   END RECORD

   DEFINE x_descripcion       CHAR(500)

   OPEN WINDOW ventana5 AT 5,2 WITH FORM "LAVB0016"
   DISPLAY "                                                              (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
   DISPLAY "                       DESCRIPCION DE DETECCION                              " AT 3,1 ATTRIBUTE(REVERSE,BOLD)
   DISPLAY "DESCRIPCION                                        (Ctrl-C) Consulta Razones " AT 10,1 ATTRIBUTE(REVERSE)
   DISPLAY "RAZONES                                                       (Ctrl-C) Salir " AT 13,1 ATTRIBUTE(REVERSE)
   DISPLAY "                   (Ctrl-N) Consulta de cuentas relacionadas                 " AT 18,1 ATTRIBUTE(REVERSE)

   DECLARE cursor_con_des CURSOR FOR query2
   OPEN cursor_con_des USING l_rec_nss
   FETCH cursor_con_des INTO afi_nombres,
                             afi_paterno,
                             afi_materno,
                             afi_n_rfc,
                             afi_n_unico,
                             afi_fena,
                             afi_n_folio,
                             afi_tipo_solicitud
   CLOSE cursor_con_des

   DISPLAY l_rec_nss   TO nss
   DISPLAY afi_nombres TO nombre
   DISPLAY afi_paterno TO paterno
   DISPLAY afi_materno TO materno

   SELECT fecha_deteccion
   INTO   xfecha_deteccion
   FROM   lav_det_lavado
   WHERE  folio           = l_rec_folio
   AND    nss             = l_rec_nss
   AND    consecutivo_reg = l_rec_consecutivo
   AND    tipo_reporte    = l_rec_tipo_reporte

   DISPLAY xfecha_deteccion TO fecha_deteccion

   DECLARE cursor_descripcion CURSOR FOR query25

   LET pos = 1

   FOREACH cursor_descripcion USING l_rec_folio,
                                    l_rec_nss,
                                    l_rec_consecutivo,
                                    l_rec_tipo_reporte
                               INTO l_record[pos].descripcion

      LET pos = pos + 1
   END FOREACH

   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)

      DISPLAY ARRAY l_record TO scr_1.*
         ON KEY (CONTROL-N)
            CALL consulta_personas(l_rec_folio,
                                   l_rec_nss,
                                   l_rec_consecutivo,
                                   l_rec_tipo_reporte
                                  )
         ON KEY (CONTROL-C)
            EXIT DISPLAY
      END DISPLAY
   ELSE
      ERROR "REGISTROS DESCRIPCION... VACIO"
      SLEEP 2
      ERROR ""
   END IF

   DECLARE cursor_razones CURSOR FOR query26

   LET pos = 1

   FOREACH cursor_razones USING l_rec_folio,
                                l_rec_nss,
                                l_rec_consecutivo,
                                l_rec_tipo_reporte
                           INTO l_record1[pos].razones

      LET pos = pos + 1
   END FOREACH

   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)

      DISPLAY ARRAY l_record1 TO scr_2.*
         ON KEY (CONTROL-N)
            CALL consulta_personas(l_rec_folio,
                                   l_rec_nss,
                                   l_rec_consecutivo,
                                   l_rec_tipo_reporte
                                  )
         ON KEY (CONTROL-C)
            EXIT DISPLAY
      END DISPLAY
   ELSE
      ERROR "REGISTROS DESCRIPCION... VACIO"
      SLEEP 2
      ERROR ""
   END IF

   CLOSE WINDOW ventana5

END FUNCTION
###############################################################
FUNCTION consulta_personas(l_per_folio,
                           l_per_nss,
                           l_per_consecutivo,
                           l_per_tipo_reporte)

   DEFINE l_per_folio           DECIMAL(11,0),
          l_per_nss             CHAR(11),
          l_per_consecutivo     INTEGER,
          l_per_tipo_reporte    CHAR(1),
          pos                   SMALLINT,
          arr_t                 SMALLINT

   DEFINE l_record3 ARRAY[50] OF RECORD
          numero_cuenta       CHAR(16),
          institucion         CHAR(6),
          nombre              CHAR(40),
          paterno             CHAR(40),
          materno             CHAR(40)
   END RECORD

   OPEN WINDOW ventana6 AT 13,2 WITH FORM "LAVB0017"
   DISPLAY "                                                              (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
   DISPLAY "              INFORMACION DE PERSONAS O CUENTAS RELACIONADAS                 " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

   DECLARE cursor_personas CURSOR FOR query27

   LET pos = 1

   FOREACH cursor_personas USING l_per_folio,
                                 l_per_nss,
                                 l_per_consecutivo,
                                 l_per_tipo_reporte
                            INTO l_record3[pos].*

      LET pos = pos + 1
   END FOREACH

   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)

      LET arr_t = ARR_COUNT()

      DISPLAY "Registros en contrados:                                                      " AT 10,1 ATTRIBUTE(REVERSE)
      DISPLAY arr_t AT 10,25 ATTRIBUTE(REVERSE)

      DISPLAY ARRAY l_record3 TO scr_1.*
         ON KEY (CONTROL-C)
            EXIT DISPLAY
      END DISPLAY
   ELSE
      ERROR "REGISTROS DE PERSONAS RELACIONADAS... VACIO"
      SLEEP 2
      ERROR ""
   END IF

   CLOSE WINDOW ventana6
END FUNCTION
###############################################################
FUNCTION preocupante_manual()

   DEFINE l_preocupante ARRAY[100] OF RECORD
          vfolio           DECIMAL(11,0),
          vnss             CHAR(11),
          vperiodo         CHAR(08),
          voperacion       CHAR(02),
          vfecha_operacion DATE,
          vmonto_pesos     DECIMAL(16,6),
          vfecha_deteccion DATE
   END RECORD

   DEFINE des  ARRAY[100] OF RECORD
          vdescripcion     CHAR(100)
   END RECORD

   DEFINE raz  ARRAY[100] OF RECORD
          vrazones         CHAR(100)
   END RECORD

   DEFINE total_desc         SMALLINT,
          total_razones      SMALLINT,
          i,ii,iii           SMALLINT,
          v_coNtador         SMALLINT

   DEFINE cur_row            SMALLINT,
          scr_row            SMALLINT,
          row_cnt            SMALLINT,
          cont_inp           SMALLINT,
          inserta            SMALLINT

   DEFINE arr_1  ,
          arr_2  ,
          arr_3  SMALLINT

   DEFINE l_fijos  RECORD
          xtipo_reporte     CHAR(1),
          xorg_supervisor   CHAR(6),
          xclave_financiera CHAR(6),
          xlocalidad        CHAR(8),
          xsucursal         CHAR(1),
          xinstrumento_mone CHAR(2),
          xmoneda           CHAR(1),
          xnacionalidad     CHAR(1),
          xtipo_persona     CHAR(1),
          xactividad_eco    CHAR(7),
          xcondicion_cod    SMALLINT,
          xestado           SMALLINT
   END RECORD

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_det_lavado
      DROP TABLE tmp_desc_oper
      DROP TABLE tmp_razones_oper
      DROP TABLE tmp_relacion_per

      SELECT *
      FROM   lav_det_lavado
      WHERE  1 = 0
      INTO TEMP tmp_det_lavado

      SELECT *
      FROM   lav_relacion_per
      WHERE  1 = 0
      INTO TEMP tmp_relacion_per

      SELECT *
      FROM   lav_razones_oper
      WHERE  1 = 0
      INTO TEMP tmp_razones_oper

      SELECT *
      FROM   lav_desc_oper
      WHERE  1 = 0
      INTO TEMP tmp_desc_oper
   WHENEVER ERROR STOP

   LET l_fijos.xtipo_reporte     = 3
   LET l_fijos.xorg_supervisor   = "001004"
   LET l_fijos.xclave_financiera = "004558"
   LET l_fijos.xlocalidad        = "01001002"
   LET l_fijos.xsucursal         = "00000000"
   LET l_fijos.xinstrumento_mone = "01"
   LET l_fijos.xmoneda           = "1"
   LET l_fijos.xnacionalidad     = "1"
   LET l_fijos.xtipo_persona     = "1"
   LET l_fijos.xactividad_eco    = "8944098"
   LET l_fijos.xcondicion_cod    = 310
   LET l_fijos.xestado           = 20

   OPEN WINDOW v_310 AT 5,2 WITH FORM "LAVB00112" --ATTRIBUTES(BORDER)
   DISPLAY " < Ctrl-G > Termina de Agrgar Registros                       <Ctrl-C> Salir   " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " DESCRIPCION                                    (Esc) Grabar descripcion       " AT 8,1 ATTRIBUTE(REVERSE)
   DISPLAY " RAZONES                                        (Esc) Grabar descripcion       " AT 12,1 ATTRIBUTE(REVERSE)

   LET v_contador = 1

   INPUT ARRAY l_preocupante FROM scr_preocupante.*

      BEFORE ROW
         LET cur_row = ARR_CURR()
         LET scr_row = SCR_LINE()
         LET row_cnt = ARR_COUNT()

      BEFORE FIELD vfolio
--         INSERT INTO lav_folio
--         VALUES (0)

         SELECT MAX(folio) + 1
         INTO   l_preocupante[cur_row].vfolio
         FROM   lav_folio

--LET l_preocupante[cur_row].vfolio = 10
         DISPLAY BY NAME l_preocupante[cur_row].vfolio

      AFTER FIELD vnss
         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD NEXT
         END IF

         IF l_preocupante[cur_row].vnss IS NULL OR
            l_preocupante[cur_row].vnss = " "   THEN
               ERROR "EL CAMPO NU PUEDE SER NULO "
               NEXT FIELD vnss
         ELSE
           IF LENGTH(l_preocupante[cur_row].vnss) < 11 THEN
              ERROR "EL NSS DEBE CONTENER 11 DIGITOS "
              NEXT FIELD vnss
           END IF
         END IF

      BEFORE FIELD vperiodo
         LET l_preocupante[cur_row].vperiodo = HOY USING "YYYYMMDD"
         DISPLAY BY NAME l_preocupante[cur_row].vperiodo

      AFTER FIELD vperiodo
         IF l_preocupante[cur_row].vperiodo IS NULL THEN
            ERROR "EL CAMPO NO PUEDE SER NULO "
            NEXT FIELD vperiodo
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
            NEXT FIELD PREVIOUS
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD NEXT
         END IF

      AFTER FIELD voperacion
         IF l_preocupante[cur_row].voperacion IS NULL THEN
            ERROR "EL CAMPO NO PUEDE SER NULO "
            NEXT FIELD voperacion
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
            NEXT FIELD PREVIOUS
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD NEXT
         END IF

      BEFORE FIELD vfecha_operacion
         LET l_preocupante[cur_row].vfecha_operacion = HOY
         DISPLAY BY NAME l_preocupante[cur_row].vfecha_operacion

      AFTER FIELD vfecha_operacion
         IF l_preocupante[cur_row].vfecha_operacion IS NULL THEN
            ERROR "EL CAMPO NO PUEDE SER NULO "
            NEXT FIELD vfecha_operacion
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
            NEXT FIELD PREVIOUS
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD NEXT
         END IF

      AFTER FIELD vmonto_pesos
         IF l_preocupante[cur_row].vmonto_pesos IS NULL THEN
            ERROR "EL CAMPO NO PUEDE SER NULO "
            NEXT FIELD vmonto_pesos
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
            NEXT FIELD PREVIOUS
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD NEXT
         END IF

      BEFORE FIELD vfecha_deteccion
         LET l_preocupante[cur_row].vfecha_deteccion = HOY
         DISPLAY BY NAME l_preocupante[cur_row].vfecha_deteccion

      AFTER FIELD vfecha_deteccion
         IF l_preocupante[cur_row].vfecha_deteccion IS NULL THEN
            ERROR "EL CAMPO NO PUEDE SER NULO "
            NEXT FIELD vfecha_deteccion
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
            NEXT FIELD PREVIOUS
         END IF

         IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") THEN
            NEXT FIELD NEXT
         END IF

##bloque de descripcion y razones
      INPUT ARRAY des FROM scr_1.*
         ON KEY (ESC)
            LET total_desc = ARR_COUNT()

            IF total_desc - 1 = 0 THEN
               ERROR " Descripcion de Solicitud NO PUEDE SER NULA",
                     ", usar [Enter] al final de cada linea  "
            ELSE
               EXIT INPUT
            END IF
      END INPUT

      INPUT ARRAY raz FROM scr_2.*
         ON KEY (ESC)
            LET total_razones = ARR_COUNT()
            IF total_razones - 1 = 0 THEN
               ERROR " Descripcion de Solicitud NO PUEDE SER NULA",
                     ", usar [Enter] al final de cada linea  "
            ELSE
               EXIT INPUT
            END IF
      END INPUT
##

      FOR arr_1 = 1 TO cur_row
          INSERT INTO tmp_det_lavado
          VALUES(l_preocupante[cur_row].vfolio      ,
                 l_preocupante[cur_row].vnss        ,
                 v_contador                         , --consecutivo
                 l_fijos.xtipo_reporte              ,
                 l_preocupante[cur_row].vperiodo    ,
                 l_fijos.xorg_supervisor            ,
                 l_fijos.xclave_financiera          ,
                 l_fijos.xlocalidad                 ,
                 l_fijos.xsucursal                  ,
                 l_preocupante[cur_row].voperacion  ,
                 l_fijos.xinstrumento_mone          ,
                 l_preocupante[cur_row].vmonto_pesos,
                 l_fijos.xmoneda                    ,
                 l_preocupante[cur_row].vfecha_operacion ,
                 l_preocupante[cur_row].vfecha_deteccion ,
                 l_fijos.xnacionalidad              ,
                 l_fijos.xtipo_persona              ,
                 l_fijos.xactividad_eco             ,
                 l_fijos.xcondicion_cod             ,
                 l_fijos.xestado                    ,
                 vusuario                           ,
                 HOY)

          FOR arr_2 = 1 TO total_desc
              IF des[arr_2].vdescripcion IS NULL OR
                 des[arr_2].vdescripcion = " "   THEN
                 EXIT FOR
              END IF

              INSERT INTO tmp_desc_oper
              VALUES(l_preocupante[cur_row].vfolio      ,
                     l_preocupante[cur_row].vnss        ,
                     v_contador                         , -- consecutivo
                     l_fijos.xtipo_reporte              ,
                     arr_2                              ,
                     des[arr_2].vdescripcion            ,
                     vusuario                           ,
                     HOY)
          END FOR

          FOR arr_3 = 1 TO total_razones
              IF raz[arr_3].vrazones IS NULL OR
                 raz[arr_3].vrazones = " "   THEN
                 EXIT FOR
              END IF

              INSERT INTO tmp_razones_oper
              VALUES(l_preocupante[cur_row].vfolio      ,
                     l_preocupante[cur_row].vnss        ,
                     v_contador                         , -- consecutivo
                     l_fijos.xtipo_reporte              ,
                     arr_3                              ,
                     raz[arr_3].vrazones                ,
                     vusuario                           ,
                     HOY)
          END FOR
      END FOR

      PROMPT "DESEA INGRESAR OTRO REGISTRO S/N ?: " FOR opc
      IF opc MATCHES "[sSNn]" THEN
         IF opc MATCHES "[sS]" THEN
            LET v_contador = v_contador + 1
            INITIALIZE l_preocupante[cur_row].vnss        TO NULL
            INITIALIZE l_preocupante[cur_row].vperiodo    TO NULL
            INITIALIZE l_preocupante[cur_row].voperacion  TO NULL
            INITIALIZE l_preocupante[cur_row].vfecha_operacion  TO NULL
            INITIALIZE l_preocupante[cur_row].vmonto_pesos      TO NULL
            INITIALIZE l_preocupante[cur_row].vfecha_deteccion  TO NULL
            CLEAR FORM
            NEXT FIELD vnss
         ELSE
            ERROR "PRESIONE <Ctrl-G> PARA GRABAR LOS REGISTROS "
            NEXT FIELD vnss
         END IF
      END IF

      ON KEY(CONTROL-G)
         LET v_contador = 0

         INSERT INTO lav_det_lavado
         SELECT *
         FROM   tmp_det_lavado

         INSERT INTO lav_desc_oper
         SELECT *
         FROM   tmp_desc_oper

         INSERT INTO lav_razones_oper
         SELECT *
         FROM   tmp_razones_oper

         INSERT INTO lav_folio VALUES(l_preocupante[cur_row].vfolio)

         LET g_impre = vruta_envio CLIPPED,"/",l_fijos.xtipo_reporte,
                                           "004","558",
                                           l_preocupante[cur_row].vperiodo[3,8],
                                           ".004"

         START REPORT archivo_lavado TO g_impre
         LET ii = 1

         DECLARE cursor_pre CURSOR FOR
         SELECT *
         FROM   lav_det_lavado
         WHERE  folio = l_preocupante[cur_row].vfolio
         AND    tipo_reporte = l_fijos.xtipo_reporte
         AND    estado = 20

         FOREACH cursor_pre INTO g_reg.*
            IF g_reg.tipo_reporte = 3 AND g_reg.estado = 10 THEN
               CONTINUE FOREACH
            END IF

            OUTPUT TO REPORT archivo_lavado(g_reg.*,ii)
            LET ii = ii + 1
            EXECUTE query20 USING g_reg.tipo_reporte,
                                  l_preocupante[cur_row].vfecha_operacion,
                                  l_preocupante[cur_row].vfecha_operacion,
                                  g_reg.nss,
                                  g_reg.consecutivo_reg
         END FOREACH

         FINISH REPORT archivo_lavado
         ERROR "ARCHIVO GENERADO..."
         SLEEP 2
         ERROR ""

         WHENEVER ERROR CONTINUE
            DROP TABLE tmp_det_lavado
            DROP TABLE tmp_desc_oper
            DROP TABLE tmp_razones_oper
         WHENEVER ERROR STOP

         EXIT INPUT

      ON KEY(INTERRUPT)
         LET cont_inp = FALSE
         ERROR "DETECCION MANUAL DE PREOCUPANTES CANCELADA ..."

         WHENEVER ERROR CONTINUE
            DROP TABLE tmp_det_lavado
            DROP TABLE tmp_desc_oper
            DROP TABLE tmp_razones_oper
         WHENEVER ERROR STOP

         DELETE FROM lav_folio
         WHERE  folio = l_preocupante[cur_row].vfolio

         EXIT INPUT
      END INPUT

   CLOSE WINDOW v_310

END FUNCTION
###############################################################
FUNCTION seleccion()

   DEFINE x_tipo_reporte      CHAR(1),
          x_nss               CHAR(11),
          x_periodo_reporte   CHAR(6),
          pos                 SMALLINT,
          sw                  SMALLINT,
          lastkey             SMALLINT,
          i                   SMALLINT,
          ii                  SMALLINT

   OPEN WINDOW ventana4 AT 5,2 WITH FORM "LAVB0015"
   DISPLAY " < ESC > Consultar                                                           " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY "                         SELECCION DE OPERACIONES                            " AT 6,1 ATTRIBUTE(REVERSE)

   LET int_flag = FALSE

   CONSTRUCT cla_where ON tipo_reporte,
                          nss,
                          periodo_reporte
                     FROM x_tipo_reporte,
                          x_nss,
                          x_periodo_reporte

      ON KEY (ESC)
         ERROR "PROCESANDO INFORMACION..."
         LET int_flag = FALSE
         EXIT CONSTRUCT
      ON KEY (control-c)
         LET int_flag = TRUE
         EXIT CONSTRUCT
   END CONSTRUCT

   IF int_flag = TRUE THEN
      LET int_flag = FALSE
      ERROR "BUSQUEDA CANCELADA..."
      CLEAR SCREEN
      CLOSE WINDOW ventana4
      RETURN
   END IF

   LET txt_sel = " SELECT * ",
                 " FROM   lav_det_lavado ",
                 " WHERE  ",cla_where CLIPPED,
                 " AND    estado IN (0,10) ",
                 " ORDER BY 2,3 "

   PREPARE query19 FROM txt_sel

   DECLARE cursor_seleccion CURSOR FOR query19

   LET i = 1

   FOREACH cursor_seleccion INTO reg_2[i].*

      IF reg_2[i].tipo_reporte = "1" THEN
         CONTINUE FOREACH
      END IF

      LET reg_8[i].folio           = reg_2[i].folio
      LET reg_8[i].nss             = reg_2[i].nss
      LET reg_8[i].tipo_operacion  = reg_2[i].tipo_operacion
      LET reg_8[i].fecha_operacion = reg_2[i].fecha_operacion
      LET reg_8[i].monto_en_pesos  = reg_2[i].monto_en_pesos
      LET reg_8[i].condicion_cod   = reg_2[i].condicion_cod

      DECLARE cursor_selfx CURSOR FOR query2

      OPEN cursor_selfx USING reg_2[i].nss

      FETCH cursor_selfx INTO afi_nombres,
                              afi_paterno,
                              afi_materno,
                              afi_n_rfc,
                              afi_n_unico,
                              afi_fena,
                              afi_n_folio,
                              afi_tipo_solicitud

      CLOSE cursor_selfx

      LET reg_8[i].nombre = afi_nombres CLIPPED," ",afi_paterno CLIPPED
      LET reg_8[i].sele   = " "

      LET i = i + 1
   END FOREACH

   ERROR ""

   LET i = i - 1

   IF (i-1) >= 1 THEN
      CALL SET_COUNT(i-1)

      LET arr_c = 0
      LET arr_l = 0
      LET arr_t = 0

      INPUT ARRAY reg_8 WITHOUT DEFAULTS FROM scr_1.*
      ATTRIBUTES(MAXCOUNT = i,COUNT = i)

         BEFORE FIELD seleccion
            LET arr_c = ARR_CURR()
            LET arr_l = SCR_LINE()
            LET arr_t = ARR_COUNT()

         AFTER FIELD seleccion
            IF reg_8[arr_c].nss IS NULL THEN
               ERROR " FIN ARREGLO "
               LET arr_c = ARR_CURR()-1
               LET arr_l = SCR_LINE()-1

            END IF

            IF reg_8[arr_c].seleccion MATCHES "X" THEN
               IF reg_8[arr_c].sele IS NULL OR
                  reg_8[arr_c].sele      = " " THEN

                  LET reg_8[arr_c].sele = "A"

                  CALL captura_descripcion(reg_8[arr_c].folio,
                                           reg_8[arr_c].nss,
                                           reg_2[arr_c].consecutivo_reg,
                                           reg_2[arr_c].tipo_reporte,
                                           reg_8[arr_c].condicion_cod)
                       RETURNING sw

                  IF sw = 0 THEN
                     LET reg_8[arr_c].sele = " "
                     LET reg_8[arr_c].seleccion = " "
                  END IF
               ELSE
                  ERROR "Registro Actualizado "
               END IF
            END IF

            IF reg_8[arr_c].sele = "A" THEN
               IF reg_8[arr_c].seleccion IS NULL OR
                  reg_8[arr_c].seleccion = " " THEN
                  LET reg_8[arr_c].seleccion = "X"
               END IF
            END IF

         ON KEY (INTERRUPT)
            INITIALIZE reg_8[i].* TO NULL

            FOR ii = 1 TO arr_c
               LET reg_8[ii].seleccion = " "
               DISPLAY BY NAME reg_8[ii].seleccion
               LET reg_8[ii].sele = " "
               DISPLAY BY NAME reg_8[ii].sele
            END FOR

            FOR ii = 1 TO arr_l
               LET reg_8[ii].seleccion = " "
               DISPLAY BY NAME reg_8[ii].seleccion
               LET reg_8[ii].sele = " "
               DISPLAY BY NAME reg_8[ii].sele
            END FOR

            FOR ii = 1 TO arr_t
               LET reg_8[ii].seleccion = " "
               DISPLAY BY NAME reg_8[ii].seleccion
               LET reg_8[ii].sele = " "
               DISPLAY BY NAME reg_8[ii].sele
            END FOR

            EXIT INPUT

         ON KEY (CONTROL-C)
            INITIALIZE reg_8[i].* TO NULL

            FOR ii = 1 TO arr_c
               LET reg_8[ii].seleccion = " "
               DISPLAY BY NAME reg_8[ii].seleccion
               LET reg_8[ii].sele = " "
               DISPLAY BY NAME reg_8[ii].sele
            END FOR

            FOR ii = 1 TO arr_l
               LET reg_8[ii].seleccion = " "
               DISPLAY BY NAME reg_8[ii].seleccion
               LET reg_8[ii].sele = " "
               DISPLAY BY NAME reg_8[ii].sele
            END FOR

            FOR ii = 1 TO arr_t
               LET reg_8[ii].seleccion = " "
               DISPLAY BY NAME reg_8[ii].seleccion
               LET reg_8[ii].sele = " "
               DISPLAY BY NAME reg_8[ii].sele
            END FOR

            EXIT INPUT

      END INPUT

      CLOSE WINDOW ventana4
   ELSE
      ERROR "REGISTROS DE LA OPERACION... VACIO"
      SLEEP 2
      ERROR ""
      CLOSE WINDOW ventana4
   END IF

   CLEAR SCREEN
END FUNCTION
################################################################
FUNCTION captura_descripcion(x_folio,
                             x_nss,
                             x_consecutivo,
                             x_tipo_reporte,
                             x_condicion_cod)

   DEFINE x_folio            DECIMAL(11,0),
          x_nss              CHAR(11),
          x_consecutivo      SMALLINT,
          x_tipo_reporte     CHAR(1),
          x_condicion_cod    SMALLINT

   DEFINE afi_nombres        CHAR(40),
          afi_paterno        CHAR(40),
          afi_materno        CHAR(40),
          afi_n_rfc          CHAR(13),
          afi_n_unico        CHAR(18),
          afi_fena           DATE,
          afi_n_folio        DECIMAL(11,0),
          afi_tipo_solicitud SMALLINT

   DEFINE reg_9 RECORD
          nss                CHAR(11),
          nombre             CHAR(40),
          paterno            CHAR(40),
          materno            CHAR(40),
          fecha_deteccion    DATE
   END RECORD

   DEFINE des ARRAY[100]  OF RECORD
          descripcion     CHAR(100)
   END RECORD

   DEFINE raz ARRAY[100]  OF RECORD
          razone          CHAR(100)
   END RECORD

   DEFINE total_desc         SMALLINT,
          total_razones      SMALLINT,
          i                  SMALLINT,
          ii                 SMALLINT,
          sw1                SMALLINT,
          x_estado           SMALLINT

   DEFINE aux_pausa          CHAR(1),
          ant                SMALLINT

   OPEN WINDOW ventana5 AT 5,2 WITH FORM "LAVB0016"
   DISPLAY "                                                              (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
   DISPLAY "                       DESCRIPCION DE DETECCION                              " AT 3,1 ATTRIBUTE(REVERSE,BOLD)
   DISPLAY " DESCRIPCION                                        (ESC) Siguiente pantalla " AT 10,1 ATTRIBUTE(REVERSE)
   DISPLAY " RAZONES                                            (ESC) Siguiente pantalla " AT 13,1 ATTRIBUTE(REVERSE)

   LET reg_9.fecha_deteccion = TODAY

   INPUT BY NAME reg_9.* WITHOUT DEFAULTS

      BEFORE FIELD nss

         DECLARE cursor_marfx CURSOR FOR query2

         OPEN cursor_marfx USING x_nss

         FETCH cursor_marfx INTO afi_nombres,
                                 afi_paterno,
                                 afi_materno,
                                 afi_n_rfc,
                                 afi_n_unico,
                                 afi_fena,
                                 afi_n_folio,
                                 afi_tipo_solicitud

         CLOSE cursor_marfx

         LET reg_9.nss     = x_nss       CLIPPED
         LET reg_9.nombre  = afi_nombres CLIPPED
         LET reg_9.paterno = afi_paterno CLIPPED
         LET reg_9.materno = afi_materno CLIPPED

         DISPLAY BY NAME reg_9.nss
         DISPLAY BY NAME reg_9.nombre
         DISPLAY BY NAME reg_9.paterno
         DISPLAY BY NAME reg_9.materno

         NEXT FIELD fecha_deteccion

      AFTER FIELD fecha_deteccion
         IF reg_9.fecha_deteccion IS NULL THEN
            ERROR "Fecha deteccion no puede ser NULA.."
            NEXT FIELD fecha_deteccion
         END IF
         ERROR ""

         DECLARE descripcion_cur CURSOR FOR
         SELECT descripcion
         FROM   lav_txt_desc
         WHERE  condicion_cod = x_condicion_cod

         LET  aux_pausa = "N"
         LET pos = 1

         FOREACH descripcion_cur INTO des[pos].*
            LET pos = pos + 1
         END FOREACH

         IF pos > 1 THEN
            LET ant = pos - 1

            IF ant = 1 THEN
               ERROR "Descripcion : ", ant, " LINEA"
            ELSE
               ERROR "Descripcion : ", ant, " LINEAS"
            END IF

            --DISPLAY "<Enter> para Actualizar" AT 12,2

            CALL SET_COUNT (pos-1)

            DISPLAY ARRAY des TO scr_1.*
               ON KEY ( CONTROL-M )
                  LET aux_pausa  = "S"
                  EXIT DISPLAY

               ON KEY ( ESCAPE )
                  LET aux_pausa = "S"
                  EXIT DISPLAY

               ON KEY ( INTERRUPT )
                  LET aux_pausa = "N"
                  EXIT DISPLAY
            END DISPLAY
         ELSE
            ERROR "NO EXISTE DESCRIPCION DE CONDICION PREDETERMINADA"
            LET aux_pausa  = "S"
         END IF

         IF aux_pausa MATCHES "[Ss]" THEN
            INPUT ARRAY des WITHOUT DEFAULTS FROM scr_1.*

               ON KEY ( ESC )
                  LET total_desc = ARR_COUNT()

                  IF total_desc - 1 = 0 THEN
                     ERROR " Descripcion de Solicitud NO PUEDE SER NULA",
                           ", usar [Enter] al final de cada linea  "
                  ELSE
                     EXIT INPUT
                  END IF
               ON KEY ( CONTROL-N )
                  ERROR " Grabar descripcion de Solicitud usando [Esc]"
            END INPUT
         END IF

         DECLARE razones_cur CURSOR FOR
         SELECT descripcion
         FROM   lav_txt_razones
         WHERE  condicion_cod = x_condicion_cod

         LET  aux_pausa = "N"
         LET pos = 1

         FOREACH razones_cur INTO raz[pos].*
            LET pos = pos + 1
         END FOREACH

         IF pos > 1 THEN
            LET ant = pos - 1

            IF ant = 1 THEN
               ERROR "Descripcion : ", ant, " LINEA"
            ELSE
               ERROR "Descripcion : ", ant, " LINEAS"
            END IF

            --DISPLAY "<Enter> para Actualizar" AT 12,2

            CALL SET_COUNT (pos-1)

            DISPLAY ARRAY raz TO scr_2.*
               ON KEY ( CONTROL-M )
                  LET aux_pausa  = "S"
                  EXIT DISPLAY

               ON KEY ( ESCAPE )
                  LET aux_pausa = "S"
                  EXIT DISPLAY

               ON KEY ( INTERRUPT )
                  LET aux_pausa = "N"
                  EXIT DISPLAY
            END DISPLAY
         ELSE
            ERROR "NO EXISTE RAZONES DE DESCRIPCION PREDETERMINADA"
            LET aux_pausa  = "S"
         END IF

         IF aux_pausa MATCHES "[Ss]" THEN
            INPUT ARRAY raz WITHOUT DEFAULTS FROM scr_2.*

               ON KEY ( ESC )
                  LET total_razones = ARR_COUNT()

                  IF total_razones - 1 = 0 THEN
                     ERROR " Descripcion de Solicitud NO PUEDE SER NULA",
                           ", usar [Enter] al final de cada linea  "
                  ELSE
                     EXIT INPUT
                  END IF
               ON KEY ( CONTROL-N )
                  ERROR " Grabar descripcion de Solicitud usando [Esc]"
            END INPUT
         END IF

         LET total_desc = total_desc - 1

         FOR i = 1 TO total_desc
            IF des[i].descripcion IS NOT NULL THEN

               EXECUTE query28 USING x_folio,            -- folio
                                     x_nss,              -- nss
                                     x_consecutivo,      -- consecutivo_reg
                                     x_tipo_reporte,     -- tipo_operacion
                                     i,                  -- linea_desc
                                     des[i].descripcion  -- descripcion

            END IF
         END FOR

         LET total_razones = total_razones - 1

         FOR ii = 1 TO total_razones
            IF raz[ii].razone IS NOT NULL THEN

               EXECUTE query29 USING x_folio,            -- folio
                                     x_nss,              -- nss
                                     x_consecutivo,      -- consecutivo_reg
                                     x_tipo_reporte,     -- tipo_operacion
                                     ii,                 -- linea_razon
                                     raz[ii].razone      -- razones

            END IF
         END FOR

         PROMPT "Esta seguro de grabar la descripcion [S/N] ? " FOR opc

         IF opc MATCHES "[Nn]" THEN

            DELETE FROM lav_desc_oper
            WHERE folio           = x_folio
            AND   nss             = x_nss
            AND   consecutivo_reg = x_consecutivo
            AND   tipo_operacion  = x_tipo_reporte

            DELETE FROM lav_razones_oper
            WHERE folio           = x_folio
            AND   nss             = x_nss
            AND   consecutivo_reg = x_consecutivo
            AND   tipo_operacion  = x_tipo_reporte

            INITIALIZE des[i].* TO NULL

            INITIALIZE raz[ii].* TO NULL

            ERROR "PROCESO CANCELADOS..."
            SLEEP 2
            ERROR ""
            EXIT INPUT
         ELSE
            LET x_estado = 20

            UPDATE lav_det_lavado
            SET    fecha_deteccion = reg_9.fecha_deteccion,
                   estado          = x_estado,
                   usuario         = USER,
                   factualiza      = TODAY
            WHERE  folio           = x_folio
            AND    nss             = x_nss
            AND    consecutivo_reg = x_consecutivo
            AND    tipo_reporte    = x_tipo_reporte

            ERROR "REGISTROS AGREGADOS..."
            SLEEP 2
            ERROR ""
            LET sw1 = 1

            CALL captura_relacion(x_folio,
                                  x_nss,
                                  x_consecutivo,
                                  x_tipo_reporte)
            EXIT INPUT
         END IF

      ON KEY (INTERRUPT)
         LET sw1 = 0
         CLEAR WINDOW ventana5
         EXIT INPUT

      ON KEY (CONTROL-C)
         LET sw1 = 0
         CLEAR WINDOW ventana5
         EXIT INPUT
   END INPUT

   CLOSE WINDOW ventana5

   RETURN sw1
END FUNCTION
################################################################
FUNCTION captura_relacion(r_folio,
                          r_nss,
                          r_consecutivo,
                          r_tipo_reporte)

   DEFINE r_folio            DECIMAL(11,0),
          r_nss              CHAR(11),
          r_consecutivo      SMALLINT,
          r_tipo_reporte     CHAR(1),
          pos                SMALLINT

   DEFINE reg_10 RECORD
          numero_cuenta      CHAR(16),
          institucion        CHAR(6),
          nombre             CHAR(40),
          paterno            CHAR(40),
          materno            CHAR(40)
   END RECORD

   OPEN WINDOW ventana6 AT 13,2 WITH FORM "LAVB0017"
   DISPLAY "                                                              (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
   DISPLAY "              INFORMACION DE PERSONAS O CUENTAS RELACIONADAS                 " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

   LET int_flag = FALSE

   LET pos = 1

   INPUT BY NAME reg_10.* WITHOUT DEFAULTS

      AFTER FIELD numero_cuenta
         IF reg_10.numero_cuenta IS NULL THEN
            ERROR "NUEMRO DE CUENTA DE PERSONA RELACIONADA NO PUEDE SER NULO.."
            NEXT FIELD numero_cuenta
         END IF
         ERROR ""

      AFTER FIELD institucion
         IF reg_10.institucion IS NULL THEN
            ERROR "INSTITUCION NO PUEDE SER NULO.."
            NEXT FIELD institucion
         END IF
         ERROR ""

      AFTER FIELD nombre
         IF reg_10.nombre IS NULL THEN
            ERROR "NOMBRE NO PUEDE SER NULO.."
            NEXT FIELD nombre
         END IF
         ERROR ""

      AFTER FIELD paterno
         IF reg_10.paterno IS NULL THEN
            ERROR "PATERNO NO PUEDE SER NULO.."
            NEXT FIELD paterno
         END IF
         ERROR ""

      AFTER FIELD materno
         IF reg_10.materno IS NULL THEN
            ERROR "MATERNO NO PUEDE SER NULO.."
            NEXT FIELD materno
         END IF
         ERROR ""

         PROMPT "Desea ingresar la informacion capturada [S/N] ? " FOR opc

         IF opc MATCHES "[Ss]" THEN

            INSERT INTO lav_relacion_per
            VALUES (r_folio,              -- folio
                    r_nss,                -- nss
                    r_consecutivo,        -- consecutivo_reg
                    r_tipo_reporte,       -- tipo_operacion
                    pos,                  -- consecutivo_cuenta
                    reg_10.numero_cuenta, -- numero_cuenta
                    reg_10.institucion,   -- institucion
                    reg_10.nombre,        -- nombre
                    reg_10.paterno,       -- paterno
                    reg_10.materno,       -- materno
                    USER,                 -- usuario
                    TODAY                 -- factualiza
                   )

            ERROR "REGISTRO AGREGADO..."
            SLEEP 2
            ERROR ""
            LET pos = pos + 1
         END IF

         PROMPT "Desea capturar informacion de otra cuenta [S/N] ? " FOR opc

         IF opc MATCHES "[Ss]" THEN

            INITIALIZE reg_10.* TO NULL

            DISPLAY BY NAME reg_10.*

            NEXT FIELD numero_cuenta
         ELSE
            EXIT INPUT
         END IF

      ON KEY (CONTROL-C)
         LET int_flag = TRUE
         EXIT INPUT
   END INPUT

   IF int_flag = TRUE THEN
      LET int_flag = FALSE
      ERROR "CAPTURA DE CUENTA RELACIONADA CANCELADA..."
      CLEAR WINDOW ventana6
      CLOSE WINDOW ventana6
      RETURN
   END IF

   CLOSE WINDOW ventana6
END FUNCTION
################################################################
FUNCTION archivo()

   DEFINE i,
          pos               SMALLINT,
          x_tipo_reporte    CHAR(1),
          x_periodo_reporte CHAR(6),
          x_dia_periodo     DATE,
          x_periodo         CHAR(8),
          x_folio           DECIMAL(11,0)

   INITIALIZE reg_6.* TO NULL

   OPEN WINDOW ventana_2 AT 5,2 WITH FORM "LAVB0013"
   DISPLAY " (ESC) Generar archivo                                        (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
   DISPLAY "                             ARCHIVO DE RESPUESTA                            " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

   LET x_dia_periodo = TODAY USING "YYYYMMDD"
   LET x_periodo = x_dia_periodo
   LET int_flag = FALSE

   INPUT BY NAME reg_6.lav_operacion,                     #270105
                 reg_6.deteccion_fini,
                 reg_6.deteccion_ffin WITHOUT DEFAULTS

      AFTER FIELD lav_operacion

         IF reg_6.lav_operacion IS NULL THEN
            ERROR "TIPO DE OPERACION NO PUEDE SER NULO.."
            NEXT FIELD lav_operacion
         END IF
         ERROR ""

         SELECT reporte_desc
         INTO   lav_oper_desc
         FROM   tab_tipo_reporte
         WHERE  lav_operacion = reg_6.lav_operacion

         DISPLAY BY NAME lav_oper_desc

      AFTER FIELD deteccion_fini                        #270105
         IF reg_6.deteccion_fini IS NULL THEN
            ERROR "FECHA INICIO NO PUEDE SER NULO..."
            NEXT FIELD deteccion_fini
         END IF
         ERROR ""

         IF reg_6.deteccion_fini > TODAY THEN
            ERROR "LA FECHA INICIO NO PUEDE SER MAYOR AL HOY"
            SLEEP 2
            ERROR ""
            NEXT FIELD deteccion_fini
         END IF

      AFTER FIELD deteccion_ffin                        #270105
         IF reg_6.deteccion_ffin IS NULL THEN
            ERROR "FECHA FIN NO PUEDE SER NULO..."
            NEXT FIELD deteccion_ffin
         END IF
         ERROR ""

         IF reg_6.deteccion_ffin > TODAY THEN
            ERROR "LA FECHA FIN NO PUEDE SER MAYOR AL HOY"
            SLEEP 2
            ERROR ""
            NEXT FIELD deteccion_ffin
         END IF

         IF reg_6.deteccion_fini > reg_6.deteccion_ffin THEN
            ERROR "LA FECHA INICIO NO PUEDE SER MAYOR A LA FECHA FIN"
            SLEEP 2
            ERROR ""
            NEXT FIELD deteccion_fini
         END IF

      ON KEY (ESC)                                   #270105
         IF reg_6.lav_operacion IS NULL THEN
            ERROR "EL CAMPO NO PUEDE SER NULO..."
            NEXT FIELD deteccion_fini
         END IF

         IF reg_6.deteccion_fini IS NULL THEN
            ERROR "LA FECHA DE INICIO NO PUEDE SER NULA..."
            NEXT FIELD deteccion_fini
         END IF

         IF reg_6.deteccion_ffin IS NULL THEN
            ERROR "LA FECHA FIN NO PUEDE SER NULA..."
            NEXT FIELD deteccion_ffin
         END IF

         IF reg_6.lav_operacion = 100 THEN
            LET x_tipo_reporte = 1
         ELSE
            IF reg_6.lav_operacion = 200 THEN
               LET x_tipo_reporte = 2
            ELSE
               IF reg_6.lav_operacion = 300 THEN
                  LET x_tipo_reporte = 3
               END IF
            END IF
         END IF

         IF x_tipo_reporte = 1 THEN
            SELECT "X"
            FROM   lav_det_lavado
            WHERE  tipo_reporte = x_tipo_reporte
            AND    fecha_operacion BETWEEN reg_6.deteccion_fini
                                       AND reg_6.deteccion_ffin
            AND    estado <> 30
            GROUP BY 1
         ELSE
            IF x_tipo_reporte = 2 OR x_tipo_reporte = 3 THEN
               SELECT "X"
               FROM   lav_det_lavado
               WHERE  tipo_reporte = x_tipo_reporte
               AND    fecha_operacion BETWEEN reg_6.deteccion_fini
                                          AND reg_6.deteccion_ffin
               AND estado = 20
               GROUP BY 1
            END IF
         END IF

         IF SQLCA.SQLCODE = 100 THEN
            ERROR "NO EXISTEN REGISTROS PARA GENERAR ARCHIVO"
            SLEEP 2
            ERROR ""
            NEXT FIELD deteccion_fini
         END IF

         LET int_flag = FALSE
         EXIT INPUT
      ON KEY (CONTROL-C)
         LET int_flag = TRUE
         EXIT INPUT
   END INPUT

   IF int_flag = TRUE THEN
      LET int_flag = FALSE
      ERROR "BUSQUEDA CANCELADA..."
      SLEEP 2
      ERROR ""
      CLEAR SCREEN
      CLOSE WINDOW ventana_2
      RETURN
   END IF

   ERROR "PROCESANDO INFORMACION ... "

   IF reg_6.lav_operacion = 100 THEN
      LET x_tipo_reporte = 1

      SELECT MAX(periodo_reporte)
      INTO   x_periodo_reporte
      #INTO  reg_6.periodo_reporte
      FROM   lav_det_lavado
      WHERE  tipo_reporte = x_tipo_reporte
      AND    fecha_operacion BETWEEN reg_6.deteccion_fini
                                 AND reg_6.deteccion_ffin
      AND    estado IN (10,20)

      #LET x_periodo_reporte = reg_6.periodo_reporte[1,6]
      LET reg_6.periodo_reporte = x_periodo_reporte
   END IF

   IF reg_6.lav_operacion = 200 THEN
      LET x_tipo_reporte = 2

      LET reg_6.periodo_reporte = HOY USING "YYYYMMDD"
      #LET x_periodo_reporte = reg_6.periodo_reporte[1,6]

      UPDATE lav_det_lavado
      SET    periodo_reporte = reg_6.periodo_reporte
      WHERE  tipo_reporte = x_tipo_reporte
      AND    fecha_operacion BETWEEN reg_6.deteccion_fini
                                 AND reg_6.deteccion_ffin
      #AND    folio = reg_6.folio
      AND    estado = 20

      LET x_periodo_reporte = reg_6.periodo_reporte
   END IF

   IF reg_6.lav_operacion = 300 THEN
      LET x_tipo_reporte = 3

      LET reg_6.periodo_reporte = HOY USING "YYYYMMDD"
      #LET x_periodo_reporte = reg_6.periodo_reporte[1,6]

      UPDATE lav_det_lavado
      SET    periodo_reporte = reg_6.periodo_reporte
      WHERE  tipo_reporte = x_tipo_reporte
      AND    fecha_operacion BETWEEN reg_6.deteccion_fini
                                 AND reg_6.deteccion_ffin
      #AND    folio = reg_6.folio
      AND    estado = 20

      LET x_periodo_reporte = reg_6.periodo_reporte
   END IF

   IF reg_6.lav_operacion = 100 THEN
      LET g_impre = vruta_envio CLIPPED,"/",x_tipo_reporte CLIPPED,
                                            "004",x_cod_afo CLIPPED,
                                            reg_6.periodo_reporte[3,6],
                                            ".004"

   ELSE
      LET g_impre = vruta_envio CLIPPED,"/",x_tipo_reporte CLIPPED,
                                            "004",x_cod_afo CLIPPED,
                                            reg_6.periodo_reporte[3,8],
                                            ".004"
   END IF

   START REPORT archivo_lavado TO g_impre

   DECLARE cursor_arc CURSOR FOR query17

   LET pos = 1

   FOREACH cursor_arc USING x_tipo_reporte,
                            x_periodo_reporte,
                            reg_6.deteccion_fini,
                            reg_6.deteccion_ffin
                            #reg_6.folio
                      INTO  g_reg.*


      IF g_reg.tipo_reporte = 2 AND g_reg.estado = 10 THEN
         CONTINUE FOREACH
      END IF

      IF g_reg.tipo_reporte = 3 AND g_reg.estado = 10 THEN
         CONTINUE FOREACH
      END IF

      OUTPUT TO REPORT archivo_lavado(g_reg.*,pos)

      LET pos = pos + 1

      EXECUTE query20 USING #g_reg.folio,               #270105
                            g_reg.tipo_reporte,
                            reg_6.deteccion_fini,
                            reg_6.deteccion_ffin,
                            g_reg.nss,
                            g_reg.consecutivo_reg

   END FOREACH

   WHILE TRUE
      PROMPT "Generado: ",g_impre CLIPPED,
             " presione 'S' salir " ATTRIBUTE(REVERSE)
      FOR respuesta ATTRIBUTE(REVERSE)

         IF respuesta MATCHES "[Ss]" THEN
            FINISH REPORT archivo_lavado

            ERROR "ARCHIVO GENERADO..."
            SLEEP 1
            ERROR ""

            EXIT WHILE
          END IF
   END WHILE

   CLEAR WINDOW ventana_2
   CLOSE WINDOW ventana_2

END FUNCTION
################################################################################
REPORT archivo_lavado(g_reg,consecutivo)

   DEFINE g_reg RECORD LIKE lav_det_lavado.*

   DEFINE consecutivo        SMALLINT,
          pos                SMALLINT

   DEFINE afi_nombres        CHAR(40),
          afi_paterno        CHAR(40),
          afi_materno        CHAR(40),
          afi_n_rfc          CHAR(13),
          afi_n_unico        CHAR(18),
          afi_fena           DATE,
          afi_n_folio        DECIMAL(11,0),
          afi_tipo_solicitud SMALLINT

   DEFINE dom_calle          CHAR(40),
          dom_numero         CHAR(10),
          dom_depto          CHAR(10),
          dom_codpos         CHAR(5),
          dom_colonia        CHAR(60),
          dom_ciudad         SMALLINT,
          dom_delega         INTEGER,
          dom_estado         SMALLINT

   DEFINE x_domicilio        CHAR(60),
          #x_colonia         CHAR(70),
          x_colonia          CHAR(30),
          x_ciudad_desc      CHAR(40),
          x_rowid_tel        INTEGER,
          x_telefono         CHAR(40),
          x_casfim_cod       INTEGER

   DEFINE l_record3          RECORD
          consecutivo_cuenta SMALLINT,
          numero_cuenta      CHAR(16),
          institucion        CHAR(6),
          nombre             CHAR(40),
          paterno            CHAR(40),
          materno            CHAR(40)
   END RECORD

   DEFINE l ARRAY[50]        OF RECORD
          descripcion        CHAR(60)
   END RECORD

   DEFINE ll ARRAY[50]       OF RECORD
          razones            CHAR(60)
   END RECORD

   DEFINE x_descripcion      CHAR(1000),
          x_razones          CHAR(1000)

define opc char(1)
   OUTPUT
      PAGE LENGTH 1
      LEFT MARGIN 0
      RIGHT MARGIN 0
      TOP MARGIN 0
      BOTTOM MARGIN 0

   FORMAT

      ON EVERY ROW
         DECLARE cursor_afi CURSOR FOR query2

         OPEN cursor_afi USING g_reg.nss

         FETCH cursor_afi INTO afi_nombres,
                               afi_paterno,
                               afi_materno,
                               afi_n_rfc,
                               afi_n_unico,
                               afi_fena,
                               afi_n_folio,
                               afi_tipo_solicitud

         IF afi_n_unico IS NULL THEN
            LET afi_n_unico = "XXXXXXXXXXXXXXXXXX"
         END IF

         CLOSE cursor_afi

         DECLARE cursor_dom CURSOR FOR query13

         OPEN cursor_dom USING g_reg.nss,
                               afi_n_folio,
                               afi_tipo_solicitud

         FETCH cursor_dom INTO dom_calle,
                               dom_numero,
                               dom_depto,
                               dom_codpos,
                               dom_colonia,
                               dom_ciudad,
                               dom_delega,
                               dom_estado

         CLOSE cursor_dom

         LET x_domicilio = dom_calle  CLIPPED," ",
                           dom_numero CLIPPED," ",
                           dom_depto  CLIPPED,"C.P. ",
                           dom_codpos CLIPPED

         LET x_colonia = dom_delega

         DECLARE cursor_ciu CURSOR FOR query14
         OPEN cursor_ciu USING dom_delega
         FETCH cursor_ciu INTO x_casfim_cod
         CLOSE cursor_ciu

         DECLARE cursor_rowid_tel CURSOR FOR query15
         OPEN cursor_rowid_tel USING g_reg.nss,
                                     afi_n_folio,
                                     afi_tipo_solicitud
         FETCH cursor_rowid_tel INTO x_rowid_tel
         CLOSE cursor_rowid_tel

         DECLARE cursor_tel CURSOR FOR query33
         OPEN cursor_tel USING g_reg.nss,
                               afi_n_folio,
                               afi_tipo_solicitud,
                               x_rowid_tel
         FETCH cursor_tel INTO x_telefono
         CLOSE cursor_tel

         IF g_reg.tipo_reporte = "1" THEN
         	  PRINT g_reg.tipo_reporte,";",                              --#01
                  g_reg.periodo_reporte CLIPPED,";",                   --#02
                  consecutivo USING "&&&&&&",";",                      --#03
                  g_reg.org_supervisor CLIPPED,";",                    --#04
                  g_reg.clave_financiera CLIPPED,";",                  --#05
                  g_reg.localidad CLIPPED,";",                         --#06
                  g_reg.sucursal CLIPPED,";",                          --#07
                  g_reg.tipo_operacion,";",                            --#08
                  g_reg.instrumento_mone,";",                          --#09
                  g_reg.nss,";",                                       --#10
                  g_reg.monto_en_pesos USING "&&&&&&&&&&&&&&.&&",";",  --#11
                  g_reg.moneda CLIPPED,";",                            --#12
                  g_reg.fecha_operacion USING "YYYYMMDD",";",          --#13
                  "",";",                                              --#14
                  g_reg.nacionalidad CLIPPED,";",                      --#15
                  g_reg.tipo_persona CLIPPED,";",                      --#16
                  ";",                                                 --#17
                  afi_nombres CLIPPED,";",                             --#18
                  afi_paterno CLIPPED,";",                             --#19
                  afi_materno CLIPPED,";",                             --#20
                  afi_n_rfc CLIPPED,";",                               --#21
                  afi_n_unico CLIPPED,";",                             --#22
                  afi_fena USING "YYYYMMDD",";",                       --#23
                  x_domicilio CLIPPED,";",                             --#24
                  x_colonia CLIPPED,";",                               --#25
                  x_casfim_cod USING "&&&&&&&&",";",                   --#26
                  x_telefono CLIPPED,";",                              --#27
                  g_reg.actividad_eco,";",                             --#28
                  ";",                                                 --#29
                  ";",                                                 --#30
                  ";",                                                 --#31
                  ";",                                                 --#32
                  ";",                                                 --#33
                  ";",                                                 --#34
                  ";",                                                 --#35
                  ";",                                                 --#36
                  ";",                                                 --#37
                  ";",                                                 --#38
                  ";",                                                 --#39
                  ";",                                                 --#40
                  ";"                                                  --#41
         ELSE
            DECLARE cursor_desc_repor CURSOR FOR query25

            LET pos = 1
            LET x_descripcion = NULL
            LET x_razones = NULL

            FOREACH cursor_desc_repor USING g_reg.folio,
                                            g_reg.nss,
                                            g_reg.consecutivo_reg,
                                            g_reg.tipo_reporte
                                       INTO l[pos].descripcion

               IF pos = 1 THEN
                  LET x_descripcion = x_descripcion CLIPPED,l[pos].descripcion CLIPPED," "
               ELSE
                  LET x_descripcion = x_descripcion CLIPPED," ",l[pos].descripcion CLIPPED
               END IF

               LET pos = pos + 1
            END FOREACH

            DECLARE cursor_razon_repor CURSOR FOR query26

            LET pos = 1

            FOREACH cursor_razon_repor USING g_reg.folio,
                                             g_reg.nss,
                                             g_reg.consecutivo_reg,
                                             g_reg.tipo_reporte
                                        INTO ll[pos].razones

               IF pos = 1 THEN
                  LET x_razones = x_razones CLIPPED,ll[pos].razones CLIPPED," "
               ELSE
                  LET x_razones = x_razones CLIPPED," ",ll[pos].razones CLIPPED
               END IF

               LET pos = pos + 1
            END FOREACH

            SELECT "X"
            FROM   lav_relacion_per
            WHERE  folio = g_reg.folio
            AND    nss = g_reg.nss
            AND    consecutivo_reg = g_reg.consecutivo_reg
            AND    tipo_operacion = g_reg.tipo_reporte
            GROUP BY 1

            IF SQLCA.SQLCODE = 0 THEN
            	 DECLARE cursor_personas_repor CURSOR FOR query30

               LET pos = 1

               FOREACH cursor_personas_repor USING g_reg.folio,
                                                   g_reg.nss,
                                                   g_reg.consecutivo_reg,
                                                   g_reg.tipo_reporte
                                              INTO l_record3.*

                  PRINT g_reg.tipo_reporte,";",                             --#01
                        g_reg.periodo_reporte CLIPPED,";",                  --#02
                        consecutivo USING "&&&&&&",";",                     --#03
                        g_reg.org_supervisor CLIPPED,";",                   --#04
                        g_reg.clave_financiera CLIPPED,";",                 --#05
                        g_reg.localidad CLIPPED,";",                        --#06
                        g_reg.sucursal CLIPPED,";",                         --#07
                        g_reg.tipo_operacion,";",                           --#08
                        g_reg.instrumento_mone,";",                         --#09
                        g_reg.nss,";",                                      --#10
                        g_reg.monto_en_pesos USING "&&&&&&&&&&&&&&.&&",";", --#11
                        g_reg.moneda CLIPPED,";",                           --#12
                        g_reg.fecha_operacion USING "YYYYMMDD",";",         --#13
                        g_reg.fecha_deteccion USING "YYYYMMDD",";",         --#14
                        g_reg.nacionalidad CLIPPED,";",                     --#15
                        g_reg.tipo_persona CLIPPED,";",                     --#16
                        ";",                                                --#17
                        afi_nombres CLIPPED,";",                            --#18
                        afi_paterno CLIPPED,";",                            --#19
                        afi_materno CLIPPED,";",                            --#20
                        afi_n_rfc CLIPPED,";",                              --#21
                        afi_n_unico CLIPPED,";",                            --#22
                        afi_fena USING "YYYYMMDD",";",                      --#23
                        x_domicilio CLIPPED,";",                            --#24
                        x_colonia CLIPPED,";",                              --#25
                        x_casfim_cod USING "&&&&&&&&",";",                  --#26
                        x_telefono CLIPPED,";",                             --#27
                        g_reg.actividad_eco,";",                            --#28
                        ";",                                                --#29
                        ";",                                                --#30
                        ";",                                                --#31
                        ";",                                                --#32
                        ";",                                                --#33
                        l_record3.consecutivo_cuenta USING "&&",";",        --#34
                        l_record3.numero_cuenta CLIPPED,";",                --#35
                        l_record3.institucion CLIPPED,";",                  --#36
                        l_record3.nombre CLIPPED,";",                       --#37
                        l_record3.paterno CLIPPED,";",                      --#38
                        l_record3.materno CLIPPED,";",                      --#39
                        x_descripcion CLIPPED,";",                          --#40
                        x_razones CLIPPED,";"                               --#41

                  LET pos = pos + 1
               END FOREACH
            ELSE
               PRINT g_reg.tipo_reporte,";",                                --#01
                     g_reg.periodo_reporte CLIPPED,";",                     --#02
                     consecutivo USING "&&&&&&",";",                        --#03
                     g_reg.org_supervisor CLIPPED,";",                      --#04
                     g_reg.clave_financiera CLIPPED,";",                    --#05
                     g_reg.localidad CLIPPED,";",                           --#06
                     g_reg.sucursal CLIPPED,";",                            --#07
                     g_reg.tipo_operacion,";",                              --#08
                     g_reg.instrumento_mone,";",                            --#09
                     g_reg.nss,";",                                         --#10
                     g_reg.monto_en_pesos USING "&&&&&&&&&&&&&&.&&",";",    --#11
                     g_reg.moneda CLIPPED,";",                              --#12
                     g_reg.fecha_operacion USING "YYYYMMDD",";",            --#13
                     g_reg.fecha_deteccion USING "YYYYMMDD",";",            --#14
                     g_reg.nacionalidad CLIPPED,";",                        --#15
                     g_reg.tipo_persona CLIPPED,";",                        --#16
                     ";",                                                   --#17
                     afi_nombres CLIPPED,";",                               --#18
                     afi_paterno CLIPPED,";",                               --#19
                     afi_materno CLIPPED,";",                               --#20
                     afi_n_rfc CLIPPED,";",                                 --#21
                     afi_n_unico CLIPPED,";",                               --#22
                     afi_fena USING "YYYYMMDD",";",                         --#23
                     x_domicilio CLIPPED,";",                               --#24
                     x_colonia CLIPPED,";",                                 --#25
                     x_casfim_cod USING "&&&&&&&&",";",                     --#26
                     x_telefono CLIPPED,";",                                --#27
                     g_reg.actividad_eco,";",                               --#28
                     ";",                                                   --#29
                     ";",                                                   --#30
                     ";",                                                   --#31
                     ";",                                                   --#32
                     ";",                                                   --#33
                     ";",                                                   --#34
                     ";",                                                   --#35
                     ";",                                                   --#36
                     ";",                                                   --#37
                     ";",                                                   --#38
                     ";",                                                   --#39
                     x_descripcion CLIPPED,";",                             --#40
                     x_razones CLIPPED,";"                                  --#41
            END IF
         END IF
END REPORT
#########################################################################
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
END FUNCTION
###############################################################
FUNCTION fin_mes(fecha)

   DEFINE mes          SMALLINT,
          fecha        DATE,
          bisiesto     ,
          ano          ,
          v_ban        ,
          ult_dia      SMALLINT,
          v_fecha      CHAR(12),
          v_ano        ,
          v_mes        ,
          v_ult_dia    CHAR(5)

   LET v_ban = FALSE
   LET ano = YEAR(fecha)
   LET bisiesto =  ano MOD 4

   IF bisiesto = 0 THEN
      LET v_ban = TRUE
   END IF

   LET mes = MONTH(fecha)

   IF mes = 4 OR mes = 6 OR mes = 9 OR mes = 11 THEN
      LET ult_dia = 30
   ELSE
      IF mes = 2 THEN
         IF v_ban THEN
            LET ult_dia = 29
         ELSE
            LET ult_dia = 28
         END IF
      ELSE
         LET ult_dia = 31
      END IF
   END IF

   LET v_mes  = mes
   LET v_ano = ano
   LET v_ult_dia = ult_dia
   LET v_fecha = v_mes CLIPPED,"/",v_ult_dia CLIPPED, "/", v_ano
   LET fecha = v_fecha

   RETURN fecha,ult_dia
END FUNCTION
###########################################################################
FUNCTION  reporte()

   DEFINE reg_report       RECORD
          folio            DECIMAL(11,0),
          nss              CHAR(11),
          tipo_reporte     SMALLINT,
          periodo_reporte  CHAR(8),
          reporte_desc     CHAR(40),
          fecha_operacion  DATE,
          tipo_operacion   SMALLINT,
          desc_operacion   CHAR(7),
          unico            CHAR(18),
          rfc              CHAR(13),
          paterno          CHAR(40),
          materno          CHAR(40),
          nombres          CHAR(40),
          nom_com          CHAR(100),
          monto_en_pesos   DECIMAL(16,6),
          razon_social     CHAR (50),
          condicion_cod    SMALLINT
   END RECORD

   DEFINE vtipo_reporte    CHAR(4)
   DEFINE xxtipo_reporte   CHAR(1),
          usuario          CHAR(8)

   OPEN WINDOW ventana8 AT 5,2 WITH FORM "LAVB0018"

   DISPLAY " < Ctrl-C > Salir       Reporte de lavado de dinero                            " AT 2,1 ATTRIBUTE(REVERSE)

   SELECT USER
   INTO   usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "lav"

   LET int_flag = FALSE
   LET xxtipo_reporte = NULL

   INPUT BY NAME  xxtipo_reporte,                       #270105
                  reg_6.deteccion_fini,
                  reg_6.deteccion_ffin
                  WITHOUT DEFAULTS

      AFTER FIELD xxtipo_reporte                #270105
         IF xxtipo_reporte IS NULL THEN
            ERROR "EL tipo de reporte no puede ser NULO"
            NEXT FIELD xxtipo_reporte
         END IF

      AFTER FIELD deteccion_fini
         IF reg_6.deteccion_fini IS NULL THEN
            ERROR "LA FECHA INICIO NO DEBE SER NULA..."
            SLEEP 2
            ERROR ""
            NEXT FIELD deteccion_fini
         ELSE
            IF reg_6.deteccion_fini > TODAY THEN
               ERROR "LA FECHA INICIO NO PUEDE SER MAYOR A HOY"
               NEXT FIELD deteccion_fini
            END IF
         END IF

      AFTER FIELD deteccion_ffin
         IF reg_6.deteccion_ffin IS NULL THEN
            ERROR "LA FECHA FIN NO PUEDE SER NULA..."
            SLEEP 2
            ERROR ""
            NEXT FIELD deteccion_ffin
         ELSE
            IF reg_6.deteccion_ffin > TODAY THEN
               ERROR "LA FECHA FIN NO PUEDE SER MAYOR A HOY"
               NEXT FIELD deteccion_ffin
            END IF

            IF reg_6.deteccion_fini > reg_6.deteccion_ffin THEN
               ERROR "LA FECHA INICIO NO PUEDE SER MAYOR A LA FECHA FIN"
               NEXT FIELD deteccion_fini
            END IF
         END IF

         LET g_impre = vruta_listados CLIPPED, "/",
                       usuario CLIPPED,".LAV_REPORT",
                       ".",hoy USING "dd-mm-yyyy"

         START REPORT listado TO g_impre
            DECLARE  cur_report CURSOR FOR
            SELECT A.nss,
                   A.fecha_operacion,
                   A.tipo_operacion,
                   A.tipo_reporte,
                   A.periodo_reporte,
                   A.condicion_cod,
                   A.monto_en_pesos
            FROM   lav_det_lavado A
            WHERE  A.tipo_reporte = xxtipo_reporte
            AND    A.fecha_operacion BETWEEN reg_6.deteccion_fini
                                         AND reg_6.deteccion_ffin
            #AND    A.fecha_operacion >= reg_6.deteccion_fini
            #AND    A.fecha_operacion <= reg_6.deteccion_ffin
            ORDER BY 3,7 desc

            FOREACH cur_report INTO reg_report.nss,
                                    reg_report.fecha_operacion,
                                    reg_report.tipo_operacion,
                                    reg_report.tipo_reporte,
                                    reg_report.periodo_reporte,
                                    reg_report.condicion_cod,
                                    reg_report.monto_en_pesos

               LET    vtipo_reporte  = reg_report.tipo_reporte  * 100

               SELECT  razon_social
               INTO    reg_report.razon_social
               FROM    tab_afore_local

               SELECT  reporte_desc
               INTO    reg_report.reporte_desc
               FROM    tab_tipo_reporte
               WHERE   lav_operacion  = vtipo_reporte

               OUTPUT TO REPORT listado(reg_report.*)
               IF reg_report.tipo_reporte <> 1 THEN
                  EXIT FOREACH
               END IF
            END FOREACH
         FINISH REPORT listado

      ON KEY (ESC)
         IF reg_report.folio IS NULL THEN
            ERROR "EL folio no puede ser NULO"
            NEXT FIELD folio
         END IF

         LET int_flag = FALSE
        -- EXIT INPUT

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "BUSQUEDA CANCELADA..."
         SLEEP 2
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana8
         RETURN
      END IF

      WHILE TRUE
         PROMPT " DESEA IMPRIMIR S/N " FOR CHAR opc
            IF opc  MATCHES "[SsNn]" THEN
               IF opc MATCHES "[Ss]" THEN
                  LET g_lista = "lp ",g_impre
                  #LET g_lista = "vi ",g_impre
                  RUN g_lista

                  ERROR " LISTADO GENERADO "
                  SLEEP 1

                  WHILE TRUE
                     PROMPT "Generado:",g_impre CLIPPED,
                            " presione 'S' salir " ATTRIBUTE(REVERSE)
                            FOR respuesta ATTRIBUTE(REVERSE)

                     IF respuesta MATCHES "[Ss]" THEN
                        EXIT WHILE
                     END IF
                  END WHILE

                  EXIT WHILE
               ELSE
                  ERROR " PROCESO CANCELADO..."
                  SLEEP 2

                  EXIT WHILE
               END IF
            END IF
      END WHILE

      ERROR ""
      EXIT INPUT

      ON KEY (INTERRUPT)
         PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR opc
         EXIT INPUT

      ON KEY (CONTROL-C)
         PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR opc
         EXIT INPUT
   END INPUT


   CLOSE WINDOW ventana8
END FUNCTION
########################################################################
REPORT listado(reg_report)

   DEFINE reg_report       RECORD
          folio            DECIMAL(11,0),
          nss              CHAR(11),
          tipo_reporte     SMALLINT,
          periodo_reporte  CHAR(8),
          reporte_desc     CHAR(40),
          fecha_operacion  DATE,
          tipo_operacion   SMALLINT,
          desc_operacion   CHAR(7),
          unico            CHAR(18),
          rfc              CHAR(13),
          paterno          CHAR(40),
          materno          CHAR(40),
          nombres          CHAR(40),
          nom_com          CHAR(100),
          monto_en_pesos   DECIMAL(16,6),
          razon_social     CHAR(50),
          condicion_cod    SMALLINT
   END RECORD

   DEFINE L1               CHAR(01),
          L2               CHAR(02),
          L3               CHAR(03),
          L4               CHAR(04),
          L5               CHAR(05),
          L6               CHAR(06),
          L7               CHAR(07),
          L8               CHAR(08),
          L9               CHAR(09),
          L10              CHAR(10)

   DEFINE cont_1           SMALLINT,
          cuantos          SMALLINT,
          vtotal           SMALLINT,
          vtotal1          SMALLINT,
          vtotal2          SMALLINT

   DEFINE total_op_20      SMALLINT,
          total_op_21      SMALLINT,
          total_monto_20   DECIMAL(16,6),
          total_monto_21   DECIMAL(16,6)

   DEFINE total_nss        INTEGER,
          x_condicion_cod  SMALLINT,
          x_tipo_operacion SMALLINT,
          pos              SMALLINT,
          x_operacion_desc CHAR(10)

   DEFINE descripcion1      SMALLINT,
          descripcion_1     CHAR(25),
          descripcion2      SMALLINT,
          descripcion_2     CHAR(25),
          xxxx_fecha_inicio DATE,
          xxxx_fecha_fin    DATE

   DEFINE xx_condicion_cod1 SMALLINT,
          xx_condicion_cod2 SMALLINT,
          ccod              SMALLINT,
          nss_report        CHAR(11),
          x_210             CHAR(3),
          x_220             CHAR(3),
          x_230             CHAR(3),
          x_240             CHAR(3),
          xx_condicion_cod  CHAR(11),
          sepa_210          CHAR(1),
          sepa_220          CHAR(1)

   DEFINE fecha_deteccion_1    DATE


   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   90

   FORMAT

   PAGE HEADER
      LET  L1  = "\304"
      LET  L2  = "\304\304"
      LET  L3  = "\304\304\304"
      LET  L4  = "\304\304\304\304"
      LET  L5  = "\304\304\304\304\304"
      LET  L10 = "\304\304\304\304\304\304\304\304\304\304"
   LET fecha_deteccion_1 = TODAY

      PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'

      PRINT COLUMN 1,'\033e\033(s218T\033(s12H\033(s7B'

      PRINT COLUMN 1,reg_report.razon_social  CLIPPED,
            COLUMN 52," MEDIDAS PREVENTIVAS ",
            COLUMN 106,"FECHA: ",TODAY USING "DD/MM/YYYY"

      SKIP 1 LINES
--osb
{
      SELECT fecha_inicio,fecha_fin
      INTO   xxxx_fecha_inicio,
             xxxx_fecha_fin
      FROM   lav_fechas_periodo
      WHERE  folio = reg_report.folio
      AND    tipo_reporte = reg_report.tipo_reporte
}
      IF reg_report.reporte_desc = "INUSUAL" THEN
         let reg_report.reporte_desc = " "
      END IF

      PRINT COLUMN 1, "LAVB001",
            COLUMN 50,"Cumplimiento de Criterios",reg_report.reporte_desc CLIPPED,
            COLUMN 106,"FOLIO: ",reg_report.folio USING "<<<<<"

      SKIP 1 LINES
--osb
--      PRINT COLUMN 1, "PERIODO OPERACION: ",xxxx_fecha_inicio USING "DD/MM/YYYY"                                          ,":", xxxx_fecha_fin USING "DD/MM/YYYY" CLIPPED
      PRINT COLUMN 1, "PERIODO OPERACION: ",fecha_deteccion_1 USING "DD/MM/YYYY" CLIPPED

      SKIP 4 LINES

      PRINT COLUMN 1,'\033e\033(s218T\033(s16H\033(s7B'

      IF reg_report.tipo_reporte = 1 THEN
         PRINT COLUMN 1,"\332",L10,L3,
                        "\302",L10,L1,
                        "\302",L5,L5,L1,
                        "\302",L5,L5,L3,
                        "\302",L10,L5,L3,L4,
                        "\277"

         PRINT COLUMN 1,"|   NUMERO    |    NSS    |   TIPO    | CRITERIO DE |        MONTO         | "
         PRINT COLUMN 1,"| CONSECUTIVO |           | OPERACION | VALIDACION  |                      | "

         PRINT COLUMN 1,"\300",L10,L3,
                        "\301",L10,L1,
                        "\301",L5,L5,L1,
                        "\301",L5,L5,L3,
                        "\301",L10,L5,L3,L4,
                        "\331"
      ELSE
         PRINT COLUMN 1,"\332",L10,L3,
                        "\302",L10,L1,
                        "\302",L10,L10,L10,L10,L10,
                        "\302",L5,L5,L1,
                        "\302",L5,L5,
                        "\302",L5,L5,L3,
                        "\302",L5,L5,L1,
                        "\302",L10,L5,L3,L2,
                        "\277"

         PRINT COLUMN 1,"|   NUMERO    |    NSS    |                    NOMBRE                        |   FECHA   |  TIPO    | CRITERIO DE | NUMERO    |      MONTO         | "
         PRINT COLUMN 1,"| CONSECUTIVO |           |                                                  | DETECCION |OPERACION | VALIDACION  | DEPOSITOS |                    | "

         PRINT COLUMN 1,"\300",L10,L3,
                        "\301",L10,L1,
                        "\301",L10,L10,L10,L10,L10,
                        "\301",L5,L5,L1,
                        "\301",L5,L5,
                        "\301",L5,L5,L3,
                        "\301",L5,L5,L1,
                        "\301",L10,L5,L3,L2,
                        "\331"

      END IF

   ON EVERY ROW

      IF reg_report.tipo_operacion = 20 THEN
         LET x_operacion_desc = "APORTE"
      ELSE
         LET x_operacion_desc = "RETIRO"
      END IF

      IF reg_report.tipo_reporte = 1 THEN
         LET total_nss = total_nss + 1

         PRINT

         PRINT COLUMN 006,total_nss USING "<<<<<",
               COLUMN 016,reg_report.nss,
               COLUMN 030,x_operacion_desc CLIPPED,
               COLUMN 045,reg_report.condicion_cod USING "<<<",
               COLUMN 058,reg_report.monto_en_pesos USING "#########&.&&"

      ELSE

         DECLARE cur_02 CURSOR FOR              #270105
         SELECT nss,
                fecha_operacion,
                tipo_operacion,
                condicion_cod,
                COUNT(*),
                monto_en_pesos
                #SUM(monto_en_pesos)
         FROM   lav_det_lavado
         #WHERE  folio = reg_report.folio
         WHERE  fecha_operacion BETWEEN reg_6.deteccion_fini
                                    AND reg_6.deteccion_ffin
         AND    tipo_reporte = reg_report.tipo_reporte
         GROUP BY 1,2,3,4,6
         #ORDER BY 3,6 desc,4
         ORDER BY 1,6 desc,4

{
         DECLARE cur_02 CURSOR FOR
         SELECT nss,
                tipo_operacion,
                COUNT(*),
                SUM(monto_en_pesos)
         FROM   lav_det_lavado
         WHERE  folio = reg_report.folio
         AND    tipo_reporte = reg_report.tipo_reporte
         GROUP BY 1,2
}
         LET total_nss = 1

         FOREACH cur_02 INTO reg_report.nss,
                             reg_report.fecha_operacion,
                             reg_report.tipo_operacion,
                             reg_report.condicion_cod,
                             cuantos,
                             reg_report.monto_en_pesos

            DECLARE cur_002 CURSOR FOR            #270105
            SELECT fecha_operacion,
                   condicion_cod
            FROM   lav_det_lavado
            #WHERE  folio = reg_report.folio
            WHERE fecha_operacion BETWEEN reg_6.deteccion_fini
                                      AND reg_6.deteccion_ffin
            AND    nss = reg_report.nss
            --AND    tipo_reporte = reg_report.tipo_reporte
            GROUP BY 1,2

            LET ccod = 1
{
            FOREACH cur_002 INTO reg_report.fecha_operacion,
                                 reg_report.condicion_cod


               CASE reg_report.condicion_cod
                  WHEN 210
                     LET x_210 = 210
                     LET sepa_210 = " "
                  WHEN 220
                     LET x_220 = 220
                     LET sepa_220 = " "
                  WHEN 230
                     LET x_230 = 230
                  WHEN 240
                     LET x_240 = 240
               END CASE

               LET ccod = ccod + 1
            END FOREACH

            LET xx_condicion_cod = x_210,sepa_210,x_220,sepa_220,x_230.x_240
            LET x_210 = " "
            LET x_220 = " "
            LET x_230 = " "
            LET x_240 = " "
            LET sepa_210 = " "
            LET sepa_220 = " "
            LET sepa_230 = " "
            LET xx_condicion_cod = xx_condicion_cod CLIPPED
}
             SELECT nombres,
                    paterno,
                    materno
             INTO   reg_report.nombres,
                    reg_report.paterno,
                    reg_report.materno
             FROM   afi_mae_afiliado
             WHERE  n_seguro = reg_report.nss

             LET  reg_report.nom_com = reg_report.paterno CLIPPED," ",
                                       reg_report.materno CLIPPED," ",
                                       reg_report.nombres

             PRINT

             IF reg_report.tipo_operacion = 20 THEN
                LET x_operacion_desc = "APORTE"
             ELSE
                LET x_operacion_desc = "RETIRO"
             END IF

             PRINT COLUMN 006,total_nss USING "<<<<<",
                   COLUMN 016,reg_report.nss,
                   COLUMN 029,reg_report.nom_com CLIPPED,
                   COLUMN 079,reg_report.fecha_operacion USING "DD/MM/YYYY",
                   COLUMN 093,x_operacion_desc CLIPPED ,
                   #COLUMN 103,xx_condicion_cod,
                   COLUMN 107,reg_report.condicion_cod USING "<<<",
                   COLUMN 120,cuantos USING "<<<",
                   COLUMN 133,reg_report.monto_en_pesos USING "#########&.&&"

             LET total_nss = total_nss + 1
         END FOREACH
      END IF

   ON LAST ROW
      SKIP 4 LINE

      IF reg_report.tipo_reporte = 1 THEN

         DECLARE cur_03 CURSOR FOR             #270105
         SELECT tipo_operacion,
                COUNT(*),
                SUM(monto_en_pesos)
         FROM   lav_det_lavado
         #WHERE  folio = reg_report.folio
         WHERE fecha_operacion BETWEEN reg_6.deteccion_fini
                                   AND reg_6.deteccion_ffin
         AND    tipo_reporte = reg_report.tipo_reporte
         GROUP BY 1
         ORDER BY 1

         LET pos = 0

         FOREACH cur_03 INTO x_tipo_operacion,
                             total_op_20,
                             total_monto_20

            IF x_tipo_operacion = 20 THEN
               LET x_operacion_desc = "APORTE"
            ELSE
               LET x_operacion_desc = "RETIRO"
            END IF

            IF pos = 0 THEN
               PRINT COLUMN 1,"\332",L10,L3,
                              L1,L10,L1,
                              "\302",L5,L5,L1,
                              "\302",L5,L5,L3,
                              "\302",L10,L5,L3,L4,
                              "\277"

               PRINT COLUMN 01,"|    Totales por tipo     |",
                     COLUMN 30,x_operacion_desc CLIPPED,
                     COLUMN 39,"|",
                     COLUMN 45,total_op_20 USING "<<<",
                     COLUMN 53,"|",
                     COLUMN 58,total_monto_20 USING "#########&.&&",
                     COLUMN 76,"|"

               PRINT COLUMN 1,"\300",L10,L3,
                              L1,L10,L1,
                              "\301",L5,L5,L1,
                              "\301",L5,L5,L3,
                              "\301",L10,L5,L3,L4,
                              "\331"
            ELSE
               PRINT COLUMN 27,"\332",L10,L1,
                               "\302",L5,L5,L3,
                               "\302",L5,L5,L3,L5,L3,L1,
                               "\277"

               PRINT COLUMN 27,"|",
                     COLUMN 30,x_operacion_desc CLIPPED,
                     COLUMN 39,"|",
                     COLUMN 45,total_op_20 USING "<<<",
                     COLUMN 53,"|",
                     COLUMN 58,total_monto_20 USING "#########&.&&",
                     COLUMN 70,"|"

               PRINT COLUMN 27,"\300",L10,L1,
                               "\301",L5,L5,L3,
                               "\301",L5,L5,L3,L5,L3,L1,
                               "\331"
            END IF

            LET pos = pos + 1
         END FOREACH

      END IF

      IF reg_report.tipo_reporte = 2 THEN
         DECLARE cur_04 CURSOR FOR                  #270105
         SELECT condicion_cod,
                #tipo_operacion,
                COUNT(nss),
                SUM(monto_en_pesos)
         FROM   lav_det_lavado
         #WHERE  folio = reg_report.folio
         WHERE fecha_operacion BETWEEN reg_6.deteccion_fini
                                   AND reg_6.deteccion_ffin
         AND    tipo_reporte = reg_report.tipo_reporte
         GROUP BY 1
         ORDER BY 2,1

         LET pos = 0

         FOREACH cur_04 INTO x_condicion_cod,
                             #x_tipo_operacion,
                             total_op_20,
                             total_monto_20
{
            IF x_tipo_operacion = 20 THEN
               LET x_operacion_desc = "APORTE"
            ELSE
               LET x_operacion_desc = "RETIRO"
            END IF
}
            IF pos = 0 THEN
               PRINT COLUMN 1,"\332",L10,L3,L10,L10,L10,L10,L10,L1,
                              L2,L10,L1,
                              L1,L5,L5,
                              "\302",L5,L5,
                              #"\302",
                              L1,L5,L5,L3,
                              "\302",L5,L5,L1,
                              "\302",L10,L5,L3,L2,
                              "\277"

               PRINT COLUMN 01,"|                                                                  TOTALES POR CRITERIOS |",
                     #COLUMN 93,x_operacion_desc CLIPPED,
                     #COLUMN 101,"|",
                     COLUMN 107,x_condicion_cod USING "<<<",
                     COLUMN 115,"|",
                     COLUMN 120,total_op_20 USING "<<<",
                     COLUMN 127,"|",
                     COLUMN 133,total_monto_20 USING "#########&.&&",
                     COLUMN 148,"|"

               PRINT COLUMN 1,"\300",L10,L3,L10,L10,L10,L10,L10,L1,
                              L2,L10,L1,
                              L1,L5,L5,
                              "\301",L5,L5,
                              #"\301",
                              L1,L5,L5,L3,
                              "\301",L5,L5,L1,
                              "\301",L10,L5,L3,L2,
                              "\331"
            ELSE
               PRINT COLUMN 90,"\332",L5,L5,
                               #"\302",L5,L5,L3,
                               L1,L5,L5,L3,
                               "\302",L5,L5,L1,
                               "\302",L10,L5,L3,L2,
                               "\277"

               PRINT COLUMN 90,"|",
                     #COLUMN 93,x_operacion_desc CLIPPED,
                     #COLUMN 101,"|",
                     COLUMN 107,x_condicion_cod USING "<<<",
                     COLUMN 115,"|",
                     COLUMN 120,total_op_20 USING "<<<",
                     COLUMN 127,"|",
                     COLUMN 133,total_monto_20 USING "#########&.&&",
                     COLUMN 148,"|"

               PRINT COLUMN 90,"\300",L5,L5,
                               #"\301",
                               L1,L5,L5,L3,
                               "\301",L5,L5,L1,
                               "\301",L10,L5,L3,L2,
                               "\331"

            END IF
            LET pos = pos + 1
         END FOREACH
--      END IF

      SKIP 1 LINE

      DECLARE cur_descripcion CURSOR FOR                  #270105
      SELECT a.condicion_cod,
             b.condicion_desc
      FROM   lav_det_lavado a, lav_condicion b
      #WHERE  a.folio = reg_report.folio
      WHERE fecha_operacion BETWEEN reg_6.deteccion_fini
                                AND reg_6.deteccion_ffin
      AND    a.tipo_reporte = reg_report.tipo_reporte
      AND    a.condicion_cod = b.condicion_cod
      GROUP BY 1,2
      ORDER BY 1,2

      LET pos = 0

      FOREACH cur_descripcion INTO descripcion1,
                                   descripcion_2

         IF pos = 0 THEN

            PRINT COLUMN 1,"\332",L10,L3,L10,L10,L10,L10,L10,L1,
                           L2,L10,L1,
                           L1,L5,L5,
                           "\302",L5,L5,
                           "\302",L5,L5,L3,
                           L5,L5,L1,L1,L1,
                           L10,L5,L3,L2,
                           "\277"

            PRINT COLUMN 01,"|                                                               DESCRIPCION DE CRITERIOS |",
                  COLUMN 94,descripcion1 USING "<<<",
                  COLUMN 101,"|",
                  COLUMN 103,descripcion_2,
                  COLUMN 148,"|"

            PRINT COLUMN 1,"\300",L10,L3,L10,L10,L10,L10,L10,L1,
                           L2,L10,L1,
                           L1,L5,L5,
                           "\301",L5,L5,
                           "\301",L5,L5,L3,
                           L5,L5,L1,L1,L1,
                           L10,L5,L3,L2,
                           "\331"
         ELSE
            PRINT COLUMN 90,"\332",L5,L5,
                            "\302",L5,L5,L3,
                            L5,L5,L1,L2,
                            L10,L5,L3,L2,
                            "\277"

            PRINT COLUMN 90,"|",
                  COLUMN 94,descripcion1 USING "<<<",
                  COLUMN 101,"|",
                  COLUMN 103,descripcion_2,
                  COLUMN 148,"|"

            PRINT COLUMN 90,"\300",L5,L5,
                            "\301",L5,L5,L3,
                            L5,L5,L1,L2,
                            L10,L5,L3,L2,
                            "\331"
         END IF
         LET pos = pos + 1
      END FOREACH
   END IF

      IF reg_report.tipo_reporte = 3 THEN

         DECLARE cur_05 CURSOR FOR                #270105
         SELECT COUNT(*),
                SUM(monto_en_pesos)
         FROM   lav_det_lavado
         #WHERE  folio = reg_report.folio
         WHERE fecha_operacion BETWEEN reg_6.deteccion_fini
                                   AND reg_6.deteccion_ffin
         AND    tipo_reporte = reg_report.tipo_reporte

         FOREACH cur_05 INTO total_op_20,
                             total_monto_20

               PRINT COLUMN 101,"\332",L5,L5,L3,
                               "\302",L5,L5,L1,
                               "\302",L10,L5,L3,L2,
                               "\277"

               PRINT COLUMN 101,"|     TOTALES |",
                     COLUMN 120,total_op_20 USING "<<<",
                     COLUMN 127,"|",
                     COLUMN 133,total_monto_20 USING "#########&.&&",
                     COLUMN 148,"|"

               PRINT COLUMN 101,"\300",L5,L5,L3,
                               "\301",L5,L5,L1,
                               "\301",L10,L5,L3,L2,
                               "\331"

         END FOREACH
      END IF
END REPORT
########################################################################
