################################################################################
#Propietario       => E.F.P.
#Programa CTAB0407 => Carga, Valida archivo de direcciones.
#Por               => Silveria Contreras Garcia
#Fecha Elaboracion => 11 Mayo del 2009
#Act por           => Armando Rdriguez
#Fecha Actualizacion>  5 enero 2010
#Sistema           => CTA
#Descripcion       => Lee, valida y carga archivo con direccion del patron.
#               e inserta registros en tablas cta_nss_edo_cta y cta_id_datos
#Fecha Modificacion => 11 septiembre 2009
#Descripcion       => Se modifica programa, por cambio de LayOut circular 73
# seccion II, modificaciones al LayOut.
################################################################################
DATABASE safre_af
GLOBALS

    DEFINE
                g_bat   RECORD LIKE dis_ctrl_proceso.*,
                vconsecutivo            INTEGER,
                vhora_final             CHAR(08),
                vresultado              CHAR(50),
                vetapa_cod              SMALLINT,
                hoy                     DATE,
                gusuario                CHAR(08),
                vreporte                CHAR(200),
                vcont_rech              INTEGER,
                vcont_acep              INTEGER,
                vfecha_lote             CHAR(08),
                vhora_inicial            CHAR(08)

 DEFINE   reg_datos_dir     RECORD
     nss                char(11),
     domicilio_pat      char(40),
     ciudad_pat         char(40),
     cod_pos_pat        char(5),
     curp               char(18),
     n_folio            decimal(10,0),
     nombre             char(40),
     paterno            char(40),
     materno            char(40),
     rfc                char(13),
     fecha_nacimiento   date,
     fentcons           date,
     entidad_fed        char(20)
   END RECORD

   DEFINE
     reg_modulo         RECORD LIKE seg_modulo.*,
     log1                 CHAR(40),
     fecha_periodo        DATE,
     v_archivo_direcciones CHAR(200),
     nom_archivo           CHAR(50),
     v_nombre              CHAR(120),
     v_folio_envio         INTEGER,
     v_sql_1               CHAR(50),
     v_tot_registro        DECIMAL(10),
     v_tot_reg_asignados   DECIMAL(10),
     tot_reg_edo_cta       DECIMAL(10),
     tot_reg_id_datos      DECIMAL(10),
     tot_sin_aporte        DECIMAL(10),
     v_tot_asignados       DECIMAL(10),
     v_tot_aportacion      DECIMAL(10),
     v_resultado           CHAR(50),
     v_tot_sin_dom         DECIMAL(10),
     v_estado              CHAR(40)

    DEFINE
        ejecuta             CHAR(300),
        v_salida            CHAR(500),
        v_error             CHAR(500),
        v_exporta           CHAR(500),
        v_generar           CHAR(300),
        v_generar1          CHAR(300),
        v_generar2          CHAR(100),
        aceptar             INTEGER

   DEFINE  x_dia           INTEGER,
           x_fecha_corte   DATE,
           x_corte         INTEGER,
           y_corte         CHAR(4),
           xcorte          CHAR(6)

  ### Variables para manejo de estatus   07 Ene 10
  DEFINE   gi_afore                          LIKE   tab_afore_local.codigo_afore
  DEFINE   gi_sin_domicilio                  SMALLINT
  DEFINE   gi_con_domicilio                  SMALLINT
  DEFINE   gi_status                         SMALLINT

  DEFINE   gi_consec_lote                    INTEGER
  DEFINE   gi_folio_envio                    INTEGER

  DEFINE   gc_folio_pre                      CHAR(9)

  DEFINE gs_tipo_trab             ,
         gs_ind_activo            ,
         gs_ind_bono              SMALLINT

END GLOBALS
################################################################################
MAIN
   OPTIONS INPUT WRAP, PROMPT LINE LAST, ACCEPT KEY CONTROL-I
      DEFER INTERRUPT

   LET      gi_con_domicilio       =  64
   LET      gi_sin_domicilio       =  65
   LET      gi_status              =  0

   LET      gi_consec_lote        =    0
   LET      gi_folio_envio        =    0

   LET gs_tipo_trab    = 5
   LET gs_ind_activo   = 1
   LET gs_ind_bono     = 0

   -- Se reciben parametros
   LET nom_archivo   = ARG_VAL(1)
   LET fecha_periodo = ARG_VAL(2)

   ### Obtener la clave de la afore local para decisión de insert/update 08 Ene 10
   SELECT  codigo_afore
     INTO  gi_afore
     FROM  tab_afore_local


   SELECT *,USER
   INTO   reg_modulo.*,
          gusuario
   FROM   seg_modulo
   WHERE  modulo_cod = "cta"

   LET  log1 = gusuario CLIPPED, ".CTAB0407.log"
   CALL STARTLOG( log1 )
   LET  hoy        = TODAY
   CALL Inicializa()
   CALL Proceso()
END MAIN
################################################################################
FUNCTION Inicializa()
  LET hoy = TODAY

  LET vhora_final = TIME
  DISPLAY "hora inicia crear tablas ",hoy," ",vhora_final

  LET v_tot_aportacion = 0
  LET v_tot_asignados  = 0
  LET v_tot_sin_dom    = 0

  WHENEVER ERROR CONTINUE
   DATABASE safre_tmp

   DROP   TABLE tmp_domicilio_233
   CREATE TABLE tmp_domicilio_233
      (nss           CHAR(11),
       reg_patronal  CHAR(11),
       rfc_patron    CHAR(13),
       razon_soc_pat CHAR(50),
       domicilio     CHAR(40),
       poblacion     CHAR(40),
       codigo_postal CHAR(5),
       entidad_fed   CHAR(21),
       telefono      CHAR(15),
       curp_trabajador CHAR(18),
       ultimo_per_pago CHAR(6)
      )
   -- CUR crear indice cuando la tabla este cargada
   -- CREATE INDEX idx_nss_233 ON tmp_domicilio_233(nss)

   DROP   TABLE tmp_asignados
   CREATE TABLE tmp_asignados
    (nss  CHAR (11)
    )
   CREATE INDEX idx_nss_asig  ON tmp_asignados(nss)

   -- Carga asignados
   SELECT  n_seguro
   FROM    safre_tmp:cuota
   WHERE   tipo_solicitud = 5
   INTO TEMP xnss_cuota

   SELECT COUNT(*)
   INTO   v_tot_asignados
   FROM   xnss_cuota

   DISPLAY " Total de nss asignados : ",v_tot_asignados

    -- Se crea tabla con nss con aportacon en segundo periodo
   DROP   TABLE tmp_aportacion
   CREATE TABLE tmp_aportacion
    (nss  CHAR (11)
    )
   --  CUR - Generar el indice despues de cargar la tabla
   -- CREATE INDEX idx_nss_ap  on tmp_aportacion(nss)

   DATABASE safre_af
   WHENEVER ERROR STOP

   --calcula periodo pago
   LET  x_dia   = DAY(hoy)
   LET  x_fecha_corte = hoy - x_dia UNITS DAY
   LET  x_corte = MONTH(fecha_periodo)
   LET  y_corte = YEAR(fecha_periodo)

   CASE x_corte
      WHEN 4
        LET xcorte = y_corte, "02" CLIPPED
      WHEN 8
        LET xcorte = y_corte, "06" CLIPPED
      WHEN 12
        LET xcorte = y_corte, "10" CLIPPED
   END CASE

   SELECT unique n_seguro
   FROM   dis_det_aporte
   WHERE  periodo_pago = xcorte
   INTO TEMP xnss_aporte


   INSERT INTO  safre_tmp:tmp_aportacion
   SELECT a.n_seguro
   FROM   xnss_cuota a,
          xnss_aporte b
   WHERE  b.n_seguro = a.n_seguro

   -- CUR  crear indice y ejecutar estadisticas

   DATABASE safre_tmp

   CREATE INDEX idx_nss_ap  on tmp_aportacion(nss)
   UPDATE STATISTICS FOR TABLE tmp_aportacion

   DATABASE safre_af

   SELECT count(*)
   INTO   v_tot_aportacion
   FROM   xnss_aporte

   SELECT count(*)
   INTO   v_tot_aportacion
   FROM   safre_tmp:tmp_aportacion

   DISPLAY "Total de NSS asignados con aportacion :",v_tot_aportacion

   LET vhora_final = TIME
   DISPLAY "hora termina obtener filtros ",hoy," ",vhora_final

   LET  v_tot_reg_asignados = 0
   LET  v_tot_registro      = 0
   LET  tot_reg_id_datos    = 0
   LET  tot_reg_edo_cta     = 0
   LET  v_resultado         = ""
   LET  v_folio_envio       = 0
   LET  tot_sin_aporte      = 0
END FUNCTION
################################################################################
FUNCTION Proceso()
   DEFINE enter   CHAR(1)

   LET hoy = TODAY

   SELECT MAX(consecutivo)
   INTO   vconsecutivo
   FROM   dis_ctrl_proceso
   WHERE  fecha_proceso = hoy
   AND    proceso_cod   = 'CTAB0407'
   AND    etapa_cod     = 1

   SQL
      SELECT FIRST 1 *
      INTO   $g_bat.*
      FROM   dis_ctrl_proceso
      WHERE  fecha_proceso = $hoy
      AND    proceso_cod   = 'CTAB00407'
      AND    etapa_cod     = 1        -- ETAPA 1
      AND    consecutivo   = $vconsecutivo
   END SQL

   DISPLAY " llega  nom_archivo  :",nom_archivo
   DISPLAY " llega  fecha_periodo  :",fecha_periodo

   LET vhora_final = TIME
   DISPLAY "Inicia proceso ",hoy," ",vhora_final
   DISPLAY " *** Lectura y Carga de Archivo de direcciones patronales **"

   IF  nom_archivo   IS NOT NULL
   AND fecha_periodo IS NOT NULL THEN

      CALL CargaArchivo()
      CALL sube_datos()

      LET  gc_folio_pre  = gi_afore             USING '&&&',
                           fecha_periodo USING 'MMYYYY'

      --Actualiza tabla de control
      LET vhora_final = TIME
      DISPLAY "Termina cargar archivo  ",hoy,"  ",vhora_final
      LET v_resultado = "TOT. REG. CARGADOS TABLA tmp_domicilio :", v_tot_registro USING "#,###,###"

      UPDATE  dis_ctrl_proceso
      SET     resultado  = v_resultado,
              hora_final = vhora_final
      WHERE   parametro1  =   nom_archivo
      AND     proceso_cod = "CTAB0407"
      AND     etapa_cod   = 1

      CALL Proceso_principal()
      LET  hoy = today

      DISPLAY " TOTAL DE REGISTROS EN EL ARCHIVO     :",v_tot_registro
      DISPLAY " TOTAL DE NSS ASIGNADOS SIN DOMICILIO :",v_tot_sin_dom
      DISPLAY " TOTAL DE NSS ASIGNADOS CON DOMICILIO :",tot_reg_edo_cta
      --DISPLAY " TOTAL REG. INSERTADOS EN TABLA cta_nss_edo_cta :",tot_reg_edo_cta
      DISPLAY " TOTAL REG. INSERTADOS EN TABLA cta_id_datos :",tot_reg_id_datos

      LET v_resultado = "Total cta_nss_edo_cta : ",tot_reg_edo_cta USING "#,###,###"," Total cta_id_datos :",tot_reg_id_datos USING "#,###,###"

      LET vhora_final = TIME
      --Actualiza tabla de control
      UPDATE dis_ctrl_proceso
      SET    resultado  = v_resultado,
             hora_final = vhora_final
      WHERE  parametro1  =   nom_archivo
      AND    proceso_cod = "CTAB0407"
      AND    etapa_cod   = 2
   ELSE
      DISPLAY " Los Parametros Deben Ser Diferente de NULL "
   END IF

   LET vhora_final = TIME
   DISPLAY " ***  Fin del Proceso ***  ",hoy,"  ",vhora_final
END FUNCTION
################################################################################
FUNCTION CargaArchivo()
 # ca--------------------------
  LET v_generar = 'FILE "',nom_archivo CLIPPED,'"'
  LET v_generar1 = "cd ",reg_modulo.ruta_rescate CLIPPED,";echo '",v_generar CLIPPED,"' > c_file_carga "
  RUN v_generar1

  LET v_generar2 = "cd ",reg_modulo.ruta_rescate CLIPPED,";cat c_file_carga     file_tab_domicilio_233 > c_file_carga_tabla "
  RUN v_generar2

  LET ejecuta = "cd ",reg_modulo.ruta_rescate CLIPPED,"; ls > archivos1" CLIPPED
  RUN ejecuta

  WHENEVER ERROR CONTINUE
      DATABASE   safre_tmp
      DROP TABLE archivos_cta
  WHENEVER ERROR STOP

  CREATE TABLE   archivos_cta
   (campo  CHAR(300))

  DATABASE safre_af

  LET ejecuta = reg_modulo.ruta_rescate CLIPPED,"/archivos1" CLIPPED
  LOAD FROM ejecuta
  INSERT INTO safre_tmp:archivos_cta
  SELECT  "X"
  FROM    safre_tmp:archivos_cta
  WHERE   campo = nom_archivo

  IF STATUS =  100 THEN
     DISPLAY "NOMBRE DE ARCHIVO INCORRECTO"
     LET INT_FLAG = TRUE
  END IF
END FUNCTION
################################################################################
FUNCTION sube_datos()
  DEFINE   lc_ejecuta               CHAR (200)

  LET v_salida =  gusuario CLIPPED,".salida"
  LET v_error  =  gusuario CLIPPED,".error"


  LET ejecuta = "cd ",reg_modulo.ruta_rescate CLIPPED,
                "/; dbload -d safre_tmp -c c_file_carga_tabla -l c_file_carga_tabla.log  -e 100000 -n 100000 -k ",
                "1>",v_salida CLIPPED, " 2>", v_error
  RUN ejecuta
  {
  c_file_carga_tabla.log
  archivos1
  c_file_carga
  c_file_carga_tabla
  }

  ###  Cambiar los permisos a los archivos para que cualquier usuario pueda ejecutar posteriormente 07 ene 10
  LET lc_ejecuta = "cd ",reg_modulo.ruta_rescate CLIPPED, " ; ",
                   " chmod 777 c_file_carga_tabla.log "
  RUN lc_ejecuta

  LET lc_ejecuta = "cd ",reg_modulo.ruta_rescate CLIPPED, " ; ",
                   " chmod 777 archivos1 "
  RUN lc_ejecuta

  LET lc_ejecuta = "cd ",reg_modulo.ruta_rescate CLIPPED, " ; ",
                   " chmod 777 c_file_carga "
  RUN lc_ejecuta

  LET lc_ejecuta = "cd ",reg_modulo.ruta_rescate CLIPPED, " ; ",
                   " chmod 777 c_file_carga_tabla "
  RUN lc_ejecuta

  DATABASE safre_tmp
  CREATE INDEX idx_nss_233 ON tmp_domicilio_233(nss)
  UPDATE STATISTICS FOR TABLE  tmp_domicilio_233
  -- CUR esta tabla esta vacia   UPDATE STATISTICS FOR TABLE  tmp_aportacion
  DATABASE safre_af

  SELECT COUNT(*)
  INTO v_tot_registro
  FROM safre_tmp:tmp_domicilio_233

  DISPLAY " TOTAL DE REGISTROS  EN ARCHIVO : ", v_tot_registro
END FUNCTION
################################################################################
FUNCTION Proceso_principal()
  DEFINE lc_query     CHAR(1500)
  DEFINE ls_bnd_folio SMALLINT

  ### Genero PREPARES de todo el proceso
  LET    lc_query      =  " SELECT  nss                                 ",
                          "   FROM  safre_tmp:tmp_aportacion            ",
                          "  WHERE  nss  NOT IN ( SELECT nss            ",
                          "                         FROM cta_id_datos)  "

  PREPARE    p_SelTmAp         FROM      lc_query
  DECLARE    d_SelTmAp       CURSOR  FOR p_SelTmAp

  LET    lc_query      =  "  SELECT dom.domicilio,                      ",
                          "         dom.poblacion,                      ",
                          "         dom.codigo_postal,                  ",
                          "         dom.entidad_fed,                    ",
                          "         afi.n_folio,                        ",
                          "         afi.nombres,                        ",
                          "         afi.paterno,                        ",
                          "         afi.materno,                        ",
                          "         afi.n_rfc,                          ",
                          "         afi.n_unico,                        ",
                          "         afi.fena,                           ",
                          "         afi.fentcons                        ",
                          "    FROM safre_tmp:tmp_domicilio_233  dom,   ",
                          "         safre_af:afi_mae_afiliado    afi    ",
                          --"   OUTER safre_af:afi_mae_afiliado    afi    ",
                          "   WHERE dom.nss = ?              ",
                          "     AND dom.nss = afi.n_seguro              "

  PREPARE    p_SelDmAf         FROM      lc_query
  --DECLARE    d_SelDmAf       CURSOR  FOR p_SelDmAf

  LET      lc_query = " EXECUTE  FUNCTION fn_fnacimiento(?, ?)  "
  PREPARE  p_fn_fnac     FROM   lc_query

  LET ls_bnd_folio = 0

  IF gi_afore = 568  THEN     --- Afore COPPEL
     LET lc_query = " EXECUTE  FUNCTION fn_folio_edocta_unificado(?, ?, ?) "
     LET ls_bnd_folio = 1
  END IF

  IF gi_afore = 562  THEN     --- Afore INVERCAP
     LET lc_query = " EXECUTE  FUNCTION fn_folio_edocta_unificado(?, ?) "
     LET ls_bnd_folio = 1
  END IF

  IF ls_bnd_folio = 1 THEN
  	 PREPARE p_fn_foli FROM lc_query
  END IF

  -- Actualiza bitacora del proceso
  LET vhora_inicial = TIME
  LET vhora_final   = ""

  INSERT INTO dis_ctrl_proceso
     VALUES ( hoy,                      -- fecha_proceso
              "CTAB0407",               -- proceso_cod
              2,                        -- etapa_cod
              vhora_inicial,            -- hora_inicial
              vhora_final,              -- hora_final
              nom_archivo,              -- parametro1
              fecha_periodo,            -- parametro2
              NULL,                     -- parametro3
              NULL,                     -- parametro4
              NULL,                     -- parametro5
              NULL,                     -- folio
              NULL,                     -- resultado
              "safre",                  -- usuario
              0 )                       -- consecutivo

  DISPLAY " ..... PROCESANDO INFORMACION ......"
  --DISPLAY "***** Listado de NSS sin domicilio   *****"



  -- CUR El maximo consecutivo y el folio se obtienen solo al principio
  -- el contador se lleva en el programa

  SELECT   MAX(consec_lote)
    INTO   gi_consec_lote
    FROM   cta_nss_edo_cta

  IF gi_consec_lote IS  NULL THEN
     LET gi_consec_lote  =  0
  END IF

  SELECT   MAX(folio_envio)
    INTO   gi_folio_envio
    FROM   cta_nss_edo_cta

  IF gi_folio_envio IS NULL THEN
     LET gi_folio_envio = 0
  END IF

  ### Cambia rutina siguiente por esto 07 Ene 10
  --Con aportacion sin domicilio en CTA_ID_DATOS
  FOREACH    d_SelTmAp         INTO      reg_datos_dir.nss
  	  --Busca domicilio DAPSIG
      EXECUTE     p_SelDmAf   USING      reg_datos_dir.nss
                               INTO      reg_datos_dir.domicilio_pat,
                                         reg_datos_dir.ciudad_pat,
                                         reg_datos_dir.cod_pos_pat,
                                         reg_datos_dir.entidad_fed,
                                         reg_datos_dir.n_folio,
                                         reg_datos_dir.nombre,
                                         reg_datos_dir.paterno,
                                         reg_datos_dir.materno,
                                         reg_datos_dir.rfc,
                                         reg_datos_dir.curp,
                                         reg_datos_dir.fecha_nacimiento,
                                         reg_datos_dir.fentcons

      IF   STATUS = NOTFOUND    THEN
      	   --Activos sin domicilio en DAPASIG
           CALL   f_Acc66()
      ELSE
           LET v_nombre = reg_datos_dir.nombre  CLIPPED, " ",
                          reg_datos_dir.paterno CLIPPED, " ",
                          reg_datos_dir.materno CLIPPED
           --Activos con domicilio en DAPASIG
           CALL   f_Acc64Ins()
           LET  tot_reg_edo_cta   = tot_reg_edo_cta  +  1
           LET  tot_reg_id_datos  = tot_reg_id_datos + 1
      END IF
  END FOREACH

  UPDATE STATISTICS FOR TABLE cta_nss_edo_cta
  UPDATE STATISTICS FOR TABLE cta_id_datos

END FUNCTION

--ACTIVOS CON DOMICILIO EN DAPASIG
### Función para actualizar estatus -- no esta en cta_id_datos y si esta en tmp_domicilio 08 Ene 10
FUNCTION   f_Acc64Ins()
  DEFINE   lc_folio                        CHAR(20)
  DEFINE   li_IndEdad                      SMALLINT

  ###IF    gi_afore   =  516  THEN     --- Afore XXI
  ###IF    gi_afore   =  532  THEN     --- Afore HSBC
  ###IF    gi_afore   =  578  THEN     --- Afore PEIS

  IF    gi_afore   =  564  OR       --- Afore METLIFE
        gi_afore   =  574  OR       --- Afore SCOTIA
     	  gi_afore   =  568  OR       --- Afore COPPEL
     	  gi_afore   =  562  THEN     --- Afore INVERCAP
        ### Update en cta_nss_edo_cta
        ### Insert en cta_id_datos
        UPDATE   cta_nss_edo_cta
           SET   estado        = gi_con_domicilio,
                 STATUS        = 90,
                 ind_actividad = 1
         WHERE   nss          =    reg_datos_dir.nss
  END IF

  #Cuatrimestre2 2010
  #Todos los asignados ya estan en la tabla

  {IF    gi_afore   =  562  THEN     --- Afore INVERCAP
        ### folio_envio       = max+1 de tabla cta_nss_edo_cta
        ### consecutivo_lote  = max+1 de tabla cta_nss_edo_cta
        ### folio = codigo afore, fecha corte, nss
        ### Insert en cta_id_datos
        ### Insert en cta_nss_edo_cta

        LET   gi_consec_lote  =   gi_consec_lote   +    1
        LET   gi_folio_envio  =   gi_folio_envio   +    1

        LET      li_IndEdad            =    f_VerIndEdad()
        LET      lc_folio              =    f_Verfolio(1, gi_consec_lote)

        INSERT   INTO cta_nss_edo_cta  VALUES (gi_folio_envio,                  -- folio_envio
                                               reg_datos_dir.nss,               -- nss
                                               reg_datos_dir.cod_pos_pat,       -- cod_postal
                                               fecha_periodo,                   -- fecha_generacion
                                               gi_consec_lote,                  -- consec_lote
                                               gi_con_domicilio,                -- estado
                                               gi_status,                       -- status
                                               li_IndEdad,                      -- ind_edad
                                               lc_folio,                        -- folio
                                               gs_tipo_trab   ,                 --tipo_trab
                                               gs_ind_activo  ,                 --ind_actividad
                                               gs_ind_bono                      --ind_bono
                                              )
  END IF}

  --COPPEL NO genera folios para los asignados
  {IF    gi_afore   =  568  THEN     --- Afore COPPEL
        ### folio_envio       = max+1 de tabla cta_nss_edo_cta
        ### consecutivo_lote  = max+1 de tabla cta_nss_edo_cta
        ### folio = function fn_folio_edocta(f_cuatrim, consecutivo_lote)
        ### Insert en cta_nss_edo_cta
        ### Insert en cta_id_datos

        LET   gi_consec_lote  =   gi_consec_lote   +    1
        LET   gi_folio_envio  =   gi_folio_envio   +    1

        LET      li_IndEdad            =    f_VerIndEdad()
        LET      lc_folio              =    f_Verfolio(2, gi_consec_lote)
        INSERT   INTO cta_nss_edo_cta  VALUES (gi_folio_envio,                  -- folio_envio
                                               reg_datos_dir.nss,               -- nss
                                               reg_datos_dir.cod_pos_pat,       -- cod_postal
                                               fecha_periodo,                   -- fecha_generacion
                                               gi_consec_lote,                  -- consec_lote
                                               gi_con_domicilio,                -- estado
                                               gi_status,                       -- status
                                               li_IndEdad,                      -- ind_edad
                                               lc_folio                         -- folio
                                              )
  END IF}

  INSERT INTO cta_id_datos    VALUES(reg_datos_dir.nss,               -- nss
                                     reg_datos_dir.curp,              -- curp
                                     reg_datos_dir.n_folio,           -- n_folio
                                     v_nombre,                        -- nombre
                                     reg_datos_dir.rfc,               -- rfc
                                     "1",                             -- infonavit
                                     reg_datos_dir.fentcons,          -- fentcons
                                     reg_datos_dir.domicilio_pat,     -- calle
                                     "",                              -- colonia
                                     reg_datos_dir.ciudad_pat,        -- delegacion
                                     reg_datos_dir.entidad_fed,       -- estado
                                     "",                              -- ciudad
                                     reg_datos_dir.cod_pos_pat,       -- postal_cod
                                     "00000"
                                     )
END FUNCTION


--ACTIVOS SIN DOMICILIO EN DAPASIG
### Funcion para actualizar estatus -- no esta en cta_id_datos y no esta en tmp_domicilio 08 Ene 10
### Esta accion se realiza solo en METLIFE Y SCOTIA por definicion
FUNCTION   f_Acc66()
  DEFINE   lc_folio                        CHAR(20)
  DEFINE   li_IndEdad                      SMALLINT

    IF  gi_afore   =  564  OR       --- Afore METLIFE
        gi_afore   =  574  OR       --- Afore SCOTIA
        gi_afore   =  568  OR       --- Afore COPPEL
        gi_afore   =  562  THEN     --- Afore INVERCAP
        UPDATE   cta_nss_edo_cta
           SET   estado        = gi_sin_domicilio,
                 STATUS        = 90              ,
                 ind_actividad = 1
         WHERE   nss     =  reg_datos_dir.nss

        IF  SQLCA.SQLERRD[3] <> 0 THEN
            LET      v_tot_sin_dom  =      v_tot_sin_dom  +  1
            --DISPLAY  "NSS: ",              reg_datos_dir.nss
        END IF

    END IF

    #Cuatrimestre2 2010
    #Todos los asignados ya estan en la tabla
    {IF    gi_afore   =  562  THEN     --- Afore INVERCAP
          ### consecutivo_lote  = max+1 de tareg_datos_dir.nss,bla cta_nss_edo_cta
          ### folio = codigo afore, fecha corte, nss
          ### Insert en cta_nss_edo_cta


          LET   gi_consec_lote  =   gi_consec_lote   +    1
          LET   gi_folio_envio  =   gi_folio_envio   +    1

          LET      li_IndEdad            =    f_VerIndEdad()
          LET      lc_folio              =    f_Verfolio(1, gi_consec_lote)

          INSERT   INTO cta_nss_edo_cta  VALUES (gi_folio_envio,                  -- folio_envio
                                                 reg_datos_dir.nss,               -- nss
                                                 ' ',                             -- cod_postal
                                                 fecha_periodo,                   -- fecha_generacion
                                                 gi_consec_lote,                  -- consec_lote
                                                 gi_sin_domicilio,                -- estado
                                                 gi_status,                       -- status
                                                 li_IndEdad,                      -- ind_edad
                                                 lc_folio,                        -- folio
                                                 gs_tipo_trab   ,                 --tipo_trab
                                                 gs_ind_activo  ,                 --ind_actividad
                                                 gs_ind_bono                      --ind_bono
                                                )
    END IF}

    --COPPEL NO genera folios para los asignados
    {IF    gi_afore   =  568  THEN     --- Afore COPPEL
          ### consecutivo_lreg_datos_dir.nss,ote  = max+1 de tabla cta_nss_edo_cta
          ### folio = function fn_folio_edocta(f_cuatrim, consecutivo_lote)
          ### Insert en cta_nss_edo_cta

          LET   gi_consec_lote  =   gi_consec_lote   +    1
          LET   gi_folio_envio  =   gi_folio_envio   +    1

          LET      li_IndEdad            =    f_VerIndEdad()
          LET      lc_folio              =    f_Verfolio(2, gi_consec_lote)
          INSERT   INTO cta_nss_edo_cta  VALUES (gi_folio_envio,                  -- folio_envio
                                                 reg_datos_dir.nss,               -- nss
                                                 ' ',                             -- cod_postal
                                                 fecha_periodo,                   -- fecha_generacion
                                                 gi_consec_lote,                  -- consec_lote
                                                 gi_sin_domicilio,                -- estado
                                                 gi_status,                       -- status
                                                 li_IndEdad,                      -- ind_edad
                                                 lc_folio                         -- folio
                                                )
    END IF}

END FUNCTION

### Funcion para obtener campo int_edad 8 Ene 10

FUNCTION f_VerIndEdad()
  DEFINE     existe           SMALLINT
  DEFINE     edad             SMALLINT
  DEFINE     criterio         SMALLINT
  DEFINE     siefore          SMALLINT
  DEFINE     curp             CHAR(18)
  DEFINE     rfc              CHAR(13)
  DEFINE     fena             DATE

  EXECUTE  p_fn_fnac    USING   reg_datos_dir.nss,
                                fecha_periodo
                         INTO   existe,
                                edad,
                                criterio,
                                siefore,
                                curp,
                                rfc,
                                fena

  RETURN siefore
END FUNCTION

### Funcion para obtener folio ... condicion 1  8 Ene 10

FUNCTION f_Verfolio(ls_cve, li_consecutivo)

  DEFINE     lc_folio         CHAR(20)
  DEFINE     ls_cve           SMALLINT
  DEFINE     li_consecutivo   INTEGER

  IF         ls_cve      =  1  THEN
  	   --INVERCAP
       ### folio = codigo afore, fecha corte, nss

       -- CUR se elimina realizar la conversion del mes y el anio de la fecha_periodo por cada registro
       EXECUTE p_fn_foli      USING  reg_datos_dir.nss,
                                     fecha_periodo
                               INTO  lc_folio
  ELSE
  	   --COPPEL
       ### folio = function fn_folio_edocta(f_cuatrim, consecutivo_lote)
       EXECUTE p_fn_foli      USING  fecha_periodo,
                                     li_consecutivo
                               INTO  lc_folio

  END IF

  RETURN     lc_folio
END FUNCTION


