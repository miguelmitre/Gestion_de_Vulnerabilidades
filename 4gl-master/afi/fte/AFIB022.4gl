#############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                          #
#Propietario       => E.F.P.                                                #
#Programa AFIB022  => GENERACION DE ARCHIVO PLANO PARA PROCESAR (INDEPEND)  #
#Sistema           => AFI (NO AFILIADOS)                                    #
#Autor             => FERNANDO HERRERA HERNANDEZ                            #
#Fecha             => 29 DE JUNIO DE 2005                                   #
#Actualizacion     => EDUARDO RESENDIZ MEDINA 13 MARZO 2006 listado_3 / 4   #
#CPL-1876          => FSR 11/02/2015  CUO II                                #
#############################################################################

DATABASE safre_af

GLOBALS

    DEFINE g_reg RECORD
      generar                      CHAR(1)
    END RECORD

    DEFINE 
      g_opcion                     CHAR(2)

    DEFINE w_aux  RECORD
      n_folio                      LIKE afi_solicitud.n_folio,
      n_seguro                     LIKE afi_solicitud.n_seguro,
      n_unico                      LIKE afi_solicitud.n_unico,
      n_rfc                        LIKE afi_solicitud.n_rfc,
      paterno                      LIKE afi_solicitud.paterno,
      materno                      LIKE afi_solicitud.materno,
      nombres                      LIKE afi_solicitud.nombres,
      fena                         DATE,
      cod_promotor                 LIKE afi_solicitud.cod_promotor,
      sexo                         LIKE afi_solicitud.sexo,
      estadon                      LIKE afi_solicitud.estadon,
      tipo_administracion          CHAR(02),
      tipo_deposito                CHAR(02)
    END RECORD

    DEFINE k_aux  RECORD
      tipo_reg                     CHAR(02),
      con_dservicio                CHAR(10),
      clave_doperacion             CHAR(02),
      fech_recep_afor              CHAR(08),
      folio_solicitud              CHAR(10)
    END RECORD

    DEFINE
      HOY                          DATE,
      HOYDIA                       DATE,
      num                          SMALLINT,
      HAY_REGISTROS                SMALLINT,
      vsoli_env                    SMALLINT,
      enter                        CHAR(1),
      HORA                         CHAR(8),
      consec                       CHAR(10),
      consecd                      CHAR(10),      
      nom_afi                      CHAR(100),
      comm                         CHAR(500),
      list_salida                  CHAR(500)

    DEFINE x_lotes RECORD          LIKE tab_lote.*
    DEFINE g_afore RECORD          LIKE tab_afore_local.*
    DEFINE g_seg_modulo RECORD     LIKE seg_modulo.*

    DEFINE g_usuario               CHAR(8)
    DEFINE G_LISTA                 CHAR(100)
    DEFINE G_LISTA1                CHAR(100)
    DEFINE G_LISTA3                CHAR(150)
    DEFINE G_LISTA4                CHAR(150)
    DEFINE G_LISTA5                CHAR(150)
    DEFINE G_IMPRE                 CHAR(100)
    DEFINE G_IMPRE4                CHAR(150)
    DEFINE h_corr                  SMALLINT
    DEFINE h_corrd                 SMALLINT    
    DEFINE vchar                   CHAR(1)

    DEFINE reg_bat RECORD
      pid                          INTEGER,
      proceso_cod                  INTEGER,
      opera_cod                    INTEGER,
      nombre_archivo               CHAR(25)
    END RECORD

    DEFINE bnd_proceso             SMALLINT,
           cont_acep               SMALLINT,
           cont_tot                SMALLINT

    DEFINE rech_prom               SMALLINT
    DEFINE vfecha_proceso          DATE
    DEFINE total_enviadas          INTEGER

    --- CPL-1876  CUO  Feb 2015 
    DEFINE g_dom       CHAR(100)
    DEFINE gs_idope    SMALLINT       --#CUO II     
    

END GLOBALS

--- Feb 2015 CPL-1876
GLOBALS 'AFIB0001.4gl'


MAIN

    DISPLAY " "
    DISPLAY ".1"

    CALL STARTLOG(FGL_GETENV('USER')||'AFIB022.log')
    CALL inicio() #i

    IF NOT bnd_proceso THEN
       DEFER INTERRUPT
       OPTIONS INPUT WRAP,
         PROMPT LINE LAST,
         ACCEPT KEY CONTROL-I

       CALL proceso_principal() #pp
    ELSE
       CALL rescata_valores()   #rv
       CALL actualiza_bat_f(0)  #rv
    END IF

END MAIN

FUNCTION inicio()
#i--------------

    LET reg_bat.pid            = ARG_VAL(1)
    LET reg_bat.proceso_cod    = ARG_VAL(2)
    LET reg_bat.opera_cod      = ARG_VAL(3)

    LET bnd_proceso            = 0
    LET rech_prom              = 0
    LET cont_acep              = 0
    LET cont_tot               = 0

    IF reg_bat.pid THEN
       DISPLAY "INICIANDO PROCESO ..."
       LET bnd_proceso = 1
    END IF
    

    LET   gs_idope             =  10       --#CUO II CPL-1876 Feb 2015    

    LET HOY                    = TODAY
    LET HORA                   = TIME
    LET HOYDIA                 = TODAY
    LET num                    = 1

    LET g_reg.generar          = "S"

    LET k_aux.tipo_reg         = "02"  
    LET k_aux.con_dservicio    = "          "  
    LET k_aux.clave_doperacion = "01" 
    LET k_aux.fech_recep_afor  = "       " 
    LET k_aux.folio_solicitud  = "          "

    SELECT * 
    INTO   g_seg_modulo.* 
    FROM   seg_modulo
    WHERE  modulo_cod = 'afi'

    SELECT *, USER
    INTO   g_afore.*, g_usuario
    FROM   tab_afore_local

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

  OPEN WINDOW ventana_1 AT 4,4 WITH FORM "AFIB0221" ATTRIBUTE(BORDER)
  DISPLAY " AFIB022  GENERACION ARCHIVO CERTIFICACION (NO AFILIADOS)                      " AT 3,1 ATTRIBUTE(REVERSE)
  DISPLAY "                           < Ctrl-C > Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)
  DISPLAY HOY USING"dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)

  DISPLAY g_seg_modulo.ruta_envio AT 10,9 

  INPUT BY NAME g_reg.generar

    AFTER FIELD generar
      IF g_reg.generar IS NULL THEN
         ERROR "Campo NO puede ser NULO"
         NEXT FIELD generar
      END IF

      IF g_reg.generar  MATCHES "[Ss]" THEN
         CALL rescata_valores()

         IF NOT HAY_REGISTROS THEN
            IF bnd_proceso THEN
               DISPLAY "Program stopped, NO HAY REGISTROS PARA CERTIFICAR"
               EXIT PROGRAM
            ELSE
               ERROR "NO HAY REGISTROS PARA CERTIFICAR"
               SLEEP 3
               EXIT PROGRAM
            END IF
         END IF

         EXIT INPUT
      ELSE
         ERROR "PROCESO CANCELADO" SLEEP 2
         EXIT PROGRAM
      END IF

      ON KEY ( INTERRUPT )
         ERROR "PROCESO CANCELADO" SLEEP 2
         EXIT PROGRAM
  END INPUT

  IF rech_prom = 0 THEN
     PROMPT "NOMBRE ARCHIVO PLANO : ", comm CLIPPED,
            " /[Enter] para salir" FOR enter
  ELSE
     PROMPT "NOMBRE ARCHIVO PLANO : ", comm CLIPPED,
            " CON ", rech_prom USING "####&",
            " REGISTROS CON PROMOTORES NO ACTIVOS, /[Enter] para salir"
     FOR enter
  END IF

END FUNCTION

FUNCTION rescata_valores()
#rv----------------------

  DEFINE recha SMALLINT

  --- CPL-1876 CUO  Feb 2015 
  DEFINE  lr_d         RECORD
        nss            CHAR(11),
        n_folio        DECIMAL(10,0),
        tipo_sol       SMALLINT,
        proceso        CHAR(02)
                       END RECORD  

  LET HAY_REGISTROS = FALSE

  SELECT COUNT(*) 
  INTO   HAY_REGISTROS 
  FROM   afi_solicitud
  WHERE  status_interno = 20 #CON TODOS LOS DOCTOS
  AND    tipo_solicitud = 8 

  IF HAY_REGISTROS THEN
     SELECT * 
     INTO   x_lotes.* 
     FROM   tab_lote
     WHERE  lotes_cod   = 10
     AND    lotes_fecha = HOY

     IF SQLCA.SQLCODE = 0 THEN
        UPDATE tab_lote 
        SET    lotes_fecha       = HOY,
               lotes_correlativo = lotes_correlativo + 1
        WHERE  lotes_cod         = 10
	AND    lotes_fecha       = HOY
     ELSE
        INSERT INTO tab_lote
        VALUES(HOY,
               10,
               'NO AFILIADOS',
               1,
               1)
     END IF

     SELECT lotes_correlativo
     INTO   consec 
     FROM   tab_lote
     WHERE  lotes_cod   = 10
     AND    lotes_fecha = HOY


     LET  h_corrd    =  consec      --# CUO II CPL-1876 Feb 2014
     LET  consecd    =  consec      --# CUO II CPL-1876 Feb 2014

     LET vsoli_env = HAY_REGISTROS

     IF NOT bnd_proceso THEN
        DISPLAY "SOLICITUDES A SER ENVIADAS ", vsoli_env AT 16,20
        SLEEP 3
     ELSE
        DISPLAY "SOLICITUDES A SER ENVIADAS ", vsoli_env 
     END IF

     DECLARE curs_1 CURSOR FOR
     SELECT A.n_folio,
            A.n_seguro,
            A.n_unico,
            A.n_rfc,
            A.paterno,
            A.materno,
            A.nombres,
            A.fena,
            A.cod_promotor,
            A.sexo,
            A.estadon
     FROM   afi_solicitud A
     WHERE  A.status_interno = 20 #CON TODOS LOS DOCTOS
     AND    A.tipo_solicitud = 8

     DISPLAY "Procesando Informacion"

     LET comm = g_seg_modulo.ruta_envio CLIPPED,
                "/E",HOY USING "yyyymmdd" CLIPPED,
                consec CLIPPED, ".IND" CLIPPED

     DISPLAY "Archivo: ", comm CLIPPED

     LET nom_afi = "E",HOY USING "yyyymmdd" CLIPPED,
                   consec CLIPPED, ".IND" CLIPPED

     LET G_LISTA = g_seg_modulo.ruta_listados CLIPPED,
                   "/",g_usuario CLIPPED, "ARCH_CERT_REG_IND."
                   CLIPPED,HOY USING "dd-mm-yy","_",
                   consec, HORA CLIPPED

    LET G_LISTA3 = g_seg_modulo.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                   ".REPORTE_CERT8." CLIPPED,
                   HOY USING "dd-mm-yy",".",HORA CLIPPED

    LET G_LISTA4 = g_seg_modulo.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                   ".REPORTE_APECTA." CLIPPED,
                   HOY USING "dd-mm-yy",".",HORA CLIPPED

    LET G_LISTA5 = g_seg_modulo.rutA_listados CLIPPED,
                    "/",g_usuario CLIPPED,
                    ".NOA_CERTRECH_PRO." CLIPPED,
                    HOY USING "ddmmyy","_",consec CLIPPED

     --- CPL-1876 CUO Feb 2015 
     LET    g_dom      = g_seg_modulo.ruta_envio CLIPPED,
                         "/T",HOYDIA USING "yyyymmdd" CLIPPED,
                         consecd CLIPPED, ".IND" CLIPPED
     START  REPORT       listado_d   TO   g_dom

     START REPORT listado_2 TO G_LISTA
     START REPORT listado   TO comm
     START REPORT listado_3 TO G_LISTA3   
     START REPORT listado_4 TO G_LISTA4  
     START REPORT listado_5 TO G_LISTA5

     FOREACH curs_1 INTO w_aux.*
       LET cont_tot = cont_tot + 1
       DISPLAY "PROCESANDO  " , cont_tot , "  DE  ", vsoli_env AT 11,9
       LET HAY_REGISTROS = TRUE

       LET k_aux.folio_solicitud = w_aux.n_folio

       IF w_aux.materno IS NULL OR
          w_aux.materno MATCHES '  *' THEN
          LET w_aux.materno = "N/A"
       END IF

       SELECT MAX(fecha_proceso) 
       INTO   vfecha_proceso
       FROM   cta_ctr_reg_ind
       WHERE  @curp = w_aux.n_unico
       AND    @nti  = w_aux.n_seguro

       SELECT @tipo_administracion, @ind_deposito
       INTO   w_aux.tipo_administracion, w_aux.tipo_deposito
       FROM   cta_ctr_reg_ind
       WHERE  @curp          = w_aux.n_unico
       AND    @nti           = w_aux.n_seguro
       AND    @fecha_proceso = vfecha_proceso

       SELECT NVL(A.status,3)
       INTO   recha
       FROM   pro_mae_promotor A
       WHERE  A.cod_promotor = w_aux.cod_promotor

       IF recha = 2 OR
          recha = 3 THEN
          LET rech_prom = rech_prom + 1
 
          OUTPUT TO REPORT listado_5(w_aux.n_seguro        ,
                                     w_aux.n_unico         ,
                                     k_aux.folio_solicitud ,
                                     w_aux.paterno         ,
                                     w_aux.materno         ,
                                     w_aux.nombres         ,
                                     w_aux.cod_promotor    ,
                                     recha
                                     )

          UPDATE afi_solicitud
          SET    status_interno  = 21
          WHERE  @n_seguro       = w_aux.n_seguro
          AND    @n_folio        = w_aux.n_folio
          AND    @tipo_solicitud = 8
          #AND    @status_interno = 20

          CALL inserta_afi_ctr(21)

          UPDATE solicitudafi
          SET    stamcertificafi = '21-Rechazo x promotor invalido'
          WHERE  nsolicitud      = w_aux.n_folio

          CONTINUE FOREACH
       END IF

       LET total_enviadas = vsoli_env - rech_prom
       DISPLAY "TOTAL DE SOLICITUDES  ENVIADAS        ", total_enviadas AT 16,20
       DISPLAY "TOTAL DE RECHAZADAS POR VAL. PROMOTOR ", rech_prom      AT 17,20
       DISPLAY "ARCH RECH PROM: ", G_LISTA5 CLIPPED                     AT 18,1

       LET cont_acep = cont_acep + 1

       IF cont_acep = 9998 THEN

          UPDATE tab_lote 
          SET    lotes_correlativo = lotes_correlativo + 1
          WHERE  lotes_cod         = 10
          AND    lotes_fecha       = HOY

          SELECT lotes_correlativo
          INTO   consec 
          FROM   tab_lote
          WHERE  lotes_cod   = 10
          AND    lotes_fecha = HOY

          FINISH REPORT listado

          LET cont_acep = 0
          LET comm = g_seg_modulo.ruta_envio CLIPPED,
                     "/E",HOY USING "yyyymmdd" CLIPPED,
                     consec CLIPPED, ".IND" CLIPPED

          START REPORT listado   TO comm
       END IF

       OUTPUT TO REPORT listado  (k_aux.tipo_reg        ,
                                  k_aux.con_dservicio   ,
                                  k_aux.clave_doperacion,
                                  w_aux.n_seguro        ,
                                  w_aux.n_unico         ,
                                  w_aux.n_rfc           ,
                                  w_aux.paterno         ,
                                  w_aux.materno         ,
                                  w_aux.nombres         ,
                                  w_aux.fena            ,
                                  w_aux.cod_promotor    ,
                                  k_aux.fech_recep_afor ,
                                  k_aux.folio_solicitud ,
                                  w_aux.sexo            ,
                                  w_aux.estadon         ,
                                  w_aux.tipo_administracion,
                                  w_aux.tipo_deposito)

       OUTPUT TO REPORT listado_2(k_aux.tipo_reg,
                                  k_aux.con_dservicio,
                                  k_aux.clave_doperacion,
                                  w_aux.n_seguro,
                                  w_aux.n_unico,
                                  w_aux.n_rfc,
                                  w_aux.paterno,
                                  w_aux.materno,
                                  w_aux.nombres,
                                  w_aux.fena,
                                  w_aux.cod_promotor,
                                  k_aux.fech_recep_afor,
                                  k_aux.folio_solicitud,
                                  w_aux.sexo,
                                  w_aux.estadon,
                                  w_aux.tipo_administracion,
                                  w_aux.tipo_deposito)

       --- Incluyendo CPL-1876 CUO  Feb 2015 
       LET    lr_d.nss              =  w_aux.n_seguro
       LET    lr_d.n_folio          =  k_aux.folio_solicitud
       LET    lr_d.tipo_sol         =  8               
       LET    lr_d.proceso          =  '01'
       OUTPUT TO REPORT listado_d(lr_d.*)                                  

       IF recha <> 2 AND
          recha <> 3 THEN
          UPDATE afi_solicitud
          SET    status_interno = 30,
                 lote           = h_corr,
                 fecha_envio    = HOYDIA
          WHERE  status_interno = 20
          AND    n_seguro       = w_aux.n_seguro
          AND    n_folio        = k_aux.folio_solicitud
          AND    tipo_solicitud = 8

          CALL inserta_afi_ctr(30)
       END IF
     END FOREACH

     DISPLAY "SOLICITUDES A SER ENVIADAS ", vsoli_env AT 16,20

     OUTPUT TO REPORT listado_3(vsoli_env)   ---erm 13 Marzo 2006
     OUTPUT TO REPORT listado_4(vsoli_env)   ---erm 13 Marzo 2006

     FINISH REPORT listado
     FINISH REPORT listado_2
     FINISH REPORT listado_3
     FINISH REPORT listado_4
     FINISH REPORT listado_5
     FINISH REPORT listado_d     

     LET G_IMPRE = "lp ", G_LISTA3 CLIPPED
     {g_seg_modulo.ruta_listados CLIPPED,
                   "/",g_usuario CLIPPED,
                   ".REPORTE_CERT8." CLIPPED,
                   HOY USING "dd-mm-yy",".",HORA CLIPPED}
     RUN G_IMPRE

     LET G_IMPRE = ""
     LET G_IMPRE4 = "lp ", G_LISTA4 CLIPPED
     {g_seg_modulo.ruta_listados CLIPPED,
                   "/",g_usuario CLIPPED,
                   ".REPORTE_APECTA." CLIPPED,
                   HOY USING "dd-mm-yy",".",HORA CLIPPED}
     RUN G_IMPRE4

     CALL limpia_nulos()

  ELSE
     IF bnd_proceso THEN
        DISPLAY "NO HAY SOLICITUDES A ENVIAR"
     END IF
  END IF

END FUNCTION

FUNCTION inserta_afi_ctr(status_interno)
#iac----------------------

  DEFINE fecha_envio    DATE
  DEFINE cod_afore_ced  SMALLINT
  DEFINE status_interno SMALLINT
  DEFINE fentcons       DATE
  DEFINE nom_comp       CHAR(50)
  DEFINE ind_envio      SMALLINT

  SELECT MAX(p.ind_envio)
  INTO   ind_envio
  FROM   afi_ctr_solicitud p
  WHERE  p.n_seguro       = w_aux.n_seguro
  AND    p.n_folio        = w_aux.n_folio
  AND    p.tipo_solicitud = 8
  IF STATUS = NOTFOUND THEN
     IF status_interno = 30 THEN
        LET ind_envio  = 1
     ELSE
        LET ind_envio  = 0
     END IF
  ELSE
     IF status_interno = 30 THEN
	LET ind_envio  = ind_envio + 1
     END IF
  END IF

  SELECT p.fecha_envio, p.cod_afore_ced, p.fentcons
  INTO   fecha_envio, cod_afore_ced, fentcons
  FROM   afi_solicitud p
  WHERE  p.n_seguro       = w_aux.n_seguro
  AND    p.n_folio        = w_aux.n_folio
  AND    p.tipo_solicitud = 8
 
  LET nom_comp = w_aux.paterno CLIPPED, "$",
		 w_aux.materno CLIPPED, "$",
		 w_aux.nombres CLIPPED

  INSERT INTO afi_ctr_solicitud
  VALUES(8                           , #tipo_solicitud
         w_aux.n_folio               , #folio_solicitud
         w_aux.n_seguro              , #NSS
	 nom_comp                    , #Nombre completo afore
	 status_interno              , #status_interno
         fecha_envio                 , #fecha_envio
         today                       , #fecha_recepcion
         cod_afore_ced               , #codigo_afore_cedente
         ''                          , #ind_nss_modif
         fentcons                    , #fecha_certificacion
         w_aux.n_unico               , #CURP
         0                           , #Indicador CURP modif
         w_aux.n_rfc                 , #RFC
         ''                          , #Nombre completo bd
         ''                          , #Nombre completo procanase
         w_aux.fena                  , #Fecha nacimiento
         w_aux.cod_promotor          , #Codigo del promotor
         w_aux.sexo                  , #Sexo
         w_aux.estadon               , #Estado de Nacimiento
         ''                          , #Fecha primera afiliacion
         ''                          , #Fecha alta aceptada
         ''                          , #Clave afore afiliacion
         ''                          , #Nacionalidad
         ''                          , #Tipo doc prob
         ''                          , #Fol doc prob
         ''                          , #Doc prob
         ind_envio                   , #Indicador envio
	 ''                          , #Indicador nombre
	 0                           , #Codigo operacion
         0                           , #Diag proceso
	 ''                          , #Agenc cod
	 g_usuario                   , #Usuario
	 today                       )

END FUNCTION

REPORT listado(w_aux)
#--------------------

    DEFINE w_aux  RECORD
      tipo_reg            CHAR(02),
      con_dservicio       CHAR(10),
      clave_doperacion    CHAR(02),
      n_seguro            LIKE afi_solicitud.n_seguro,
      n_unico             LIKE afi_solicitud.n_unico,
      n_rfc               LIKE afi_solicitud.n_rfc,
      paterno             CHAR(40),
      materno             CHAR(40),
      nombres             CHAR(40),
      fena                DATE,
      cod_promotor        LIKE afi_solicitud.cod_promotor,
      fech_recep_afor     CHAR(08),
      folio_solicitud     CHAR(08),
      sexo                LIKE afi_solicitud.sexo,
      estadon             LIKE afi_solicitud.estadon,
      tipo_administracion CHAR(02),
      tipo_deposito       CHAR(02)
    END RECORD

    DEFINE 
      nombre_comp         ,
      nombre_comp2        CHAR(120)

    DEFINE
      dia                 CHAR(2),
      mes                 CHAR(2),
      ano                 CHAR(4),
      dia1                CHAR(2),
      mes1                CHAR(2),
      ano1                CHAR(4)

    DEFINE
      hoy                 CHAR(8)

    DEFINE x_afore RECORD LIKE tab_afore_local.*

    DEFINE 
      tot_char            CHAR(9),
      num10               CHAR(10)

    DEFINE 
      tot                 ,
      var                 ,
      cod_err_ori         SMALLINT

    DEFINE d_dp           DECIMAL(16,0)

    OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   2

    FORMAT
    FIRST PAGE HEADER
      LET hoy = YEAR(TODAY) USING "&&&&",
                MONTH(TODAY)USING "&&"  ,
                DAY(TODAY)  USING "&&"

      SELECT * 
      INTO   x_afore.*
      FROM   tab_afore_local

      SELECT lotes_correlativo
      INTO   h_corr
      FROM   tab_lote
      WHERE  lotes_cod   = 10
      AND    lotes_fecha = TODAY

      IF h_corr IS NULL THEN
         LET h_corr = 0
      END IF

      PRINT COLUMN 001,"01",
                       "01",
                       "50",
                       "01",
                       x_afore.codigo_afore USING "&&&",
                       "03",
                       "001",
                       hoy,
                       h_corr USING "&&&",  #CORRELATIVO DE ENVIO
                       543 SPACES

    ON EVERY ROW
      LET dia   = DAY(w_aux.fena)                 USING "&&"
      LET mes   = MONTH(w_aux.fena)               USING "&&"
      LET ano   = YEAR(w_aux.fena)                USING "&&&&"
      LET dia1  = DAY(TODAY)                      USING "&&"
      LET mes1  = MONTH(TODAY)                    USING "&&"
      LET ano1  = YEAR(TODAY)                     USING "&&&&"

      PRINT
        COLUMN 1,"02"                                         ,
                 010 SPACES                                   ,
                 "51"                                         ,
                 011 SPACES                                   ,
                 w_aux.n_unico                                ,
                 w_aux.n_rfc                                  ,
                 w_aux.paterno                                ,
                 w_aux.materno                                ,
                 w_aux.nombres                                ,
                 ano,mes,dia                                  ,
                 w_aux.cod_promotor        USING "&&&&&&&&&&" ,
                 18 SPACES                                    ,
                 w_aux.sexo                USING "&"          ,
                 w_aux.estadon             USING "&&"         ,
                 w_aux.tipo_administracion USING "&&"         ,
                 77 SPACES                                    ,
                 w_aux.tipo_deposito       USING "&&"         ,
                 274 SPACES

        LET num = num + 1

    ON LAST ROW
       LET tot      = 0
       LET tot      = count(*)
       LET tot_char = tot USING "&&&&&&&&&"

       PRINT
         COLUMN 001, "09"                               ,
                     20 SPACES                          ,
                     tot_char                           ,
                     "000000000"                        ,
                     530 SPACES

         SELECT 'X'
         FROM   est_det_diario edd
         WHERE  edd.fecha_detalle  = HOYDIA 
         AND    edd.nombre_archivo = nom_afi

         IF SQLCA.SQLCODE <> 0 THEN
            INSERT INTO est_det_diario
            VALUES (HOYDIA,
                    30,
                    tot,
                    8,
                    nom_afi)
         END IF

END REPORT

REPORT listado_2(w_aux)
#----------------------

  DEFINE w_aux RECORD
    tipo_reg            CHAR(02),
    con_dservicio       CHAR(10),
    clave_doperacion    CHAR(02),
    n_seguro            LIKE afi_solicitud.n_seguro,
    n_unico             LIKE afi_solicitud.n_unico,
    n_rfc               LIKE afi_solicitud.n_rfc,
    paterno             CHAR(40),
    materno             CHAR(40),
    nombres             CHAR(40),
    fena                DATE,
    cod_promotor        LIKE afi_solicitud.cod_promotor,
    fech_recep_afor     CHAR(08),
    folio_solicitud     CHAR(08),
    sexo                LIKE afi_solicitud.sexo,
    estadon             LIKE afi_solicitud.estadon,
    tipo_administracion CHAR(02),
    tipo_deposito       CHAR(02)
  END RECORD

  DEFINE
    nombre_comp         ,
    nombre_comp2        CHAR(120)

  DEFINE
    dia                 CHAR(02),
    mes                 CHAR(02),
    ano                 CHAR(04),
    dia1                CHAR(02),
    mes1                CHAR(02),
    ano1                CHAR(04),
    hoy                 CHAR(08),
    num10               CHAR(10),
    tot_char            CHAR(09)

  DEFINE x_afore RECORD LIKE tab_afore_local.*

  DEFINE tot            SMALLINT

  OUTPUT
    LEFT MARGIN   0
    RIGHT MARGIN  0
    TOP MARGIN    0
    BOTTOM MARGIN 0
    PAGE LENGTH   66

  FORMAT
    PAGE HEADER
      LET hoy = YEAR(TODAY) USING "&&&&",
                MONTH(TODAY)USING "&&"  ,
                DAY(TODAY)  USING "&&"

      SELECT * 
      INTO   x_afore.*
      FROM   tab_afore_local

      PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
      PRINT
        COLUMN 01,"=================================================",
        COLUMN 50,"=============================="

      PRINT
        COLUMN 01,x_afore.razon_social  CLIPPED                      ,
        COLUMN 35,"LISTA DE AFILIADOS NO AFILIADOS"                ,
        COLUMN 60,"FECHA     :",TODAY     USING "DD-MM-YYYY"

      PRINT
        COLUMN 01,"AFIB022"                                          ,
        COLUMN 35,"ENVIADOS  A PROCERSAR"                            ,
        COLUMN 60,"Nro.PAGINA:",PAGENO    USING "##########"

      PRINT
        COLUMN 01,"=================================================",
        COLUMN 50,"=============================="

      PRINT
        COLUMN 01,"NSS "        ,
        COLUMN 17,"CURP"        ,
        COLUMN 38,"RFC"         ,
        COLUMN 54,"A.PATERNO"   ,
        COLUMN 72,"A.MATERNO"

      PRINT
        COLUMN 01,"NOMBRES"     ,
        COLUMN 24,"F.NAC"       ,
        COLUMN 37,"CVE PROMOTOR",
        COLUMN 76,"SEXO"        ,
        COLUMN 80,"ENT.NAC"


      PRINT
        COLUMN 01,"-------------------------------------------------",
        COLUMN 50,"------------------------------"

      ON EVERY ROW
        LET dia   = DAY(w_aux.fena)        USING"&&"
        LET mes   = MONTH(w_aux.fena)      USING"&&"
        LET ano   = YEAR(w_aux.fena)       USING "&&&&"
        LET dia1  = DAY(TODAY)             USING"&&"
        LET mes1  = MONTH(TODAY)           USING"&&"
        LET ano1  = YEAR(TODAY)            USING "&&&&"
        LET num10 = "          "
        LET num10 = num                    USING "&&&&&&&&&&"

        PRINT
          COLUMN 01,w_aux.n_seguro       ,
          COLUMN 17,w_aux.n_unico        ,
          COLUMN 38,w_aux.n_rfc          ,
          COLUMN 54,w_aux.paterno [1,15] ,
          COLUMN 72,w_aux.materno [1,15]

        PRINT
          COLUMN 01,w_aux.nombres [1,20]  ,
          COLUMN 24,ano,mes,dia           ,
          COLUMN 37,w_aux.cod_promotor    USING "&&&&&&&&&&" ,
          COLUMN 63,k_aux.folio_solicitud USING "&&&&&&&&&&" ,
          COLUMN 76,w_aux.sexo            USING "&"          ,
          COLUMN 80,w_aux.estadon         USING "&&"         

END REPORT

REPORT listado_3(vsolv_env)    ---erm 13 Marzo 2006

  DEFINE
    cont_reg INTEGER,
    l_sol    CHAR(12),
    g_nombre CHAR(200),
    vtip_sol SMALLINT,
    vsolv_env SMALLINT

  OUTPUT
    LEFT MARGIN   0
    RIGHT MARGIN  0
    TOP MARGIN    0
    BOTTOM MARGIN 0

  FORMAT
--    PAGE HEADER
--    ON EVERY ROW

    ON LAST ROW

      PRINT
        COLUMN 03,"========================================",
        COLUMN 40,"==================================================="
      PRINT
        COLUMN 15," GENERACION DE ARCHIVO CERTIFICACION "
      PRINT
        COLUMN 03,"----------------------------------------",
        COLUMN 40,"---------------------------------------------------"
      SKIP 2 LINE
      PRINT
        COLUMN 03,"Fecha                  : ", TODAY USING "dd-mm-yyyy"
      SKIP 1 LINE
      PRINT 
        COLUMN 03,"Tipo                   : ", "8", "  ","NO AFILIADOS"
      SKIP 1 LINE
      PRINT
        COLUMN 03,"Clave Operador         : ", g_usuario CLIPPED
      SKIP 1 LINE
      PRINT
        COLUMN 03,"Nombre Archivo Generado: ",nom_afi CLIPPED
      SKIP 1 LINE
      PRINT
        COLUMN 03,"Hora de Generacion     : ",HORA CLIPPED
      SKIP 1 LINE
      PRINT 
        COLUMN 03,"No. Registros a Enviar : ", vsolv_env USING "#####"
      SKIP 1 LINE
      PRINT
        COLUMN 03,"Ruta de Rescate        : ", g_seg_modulo.ruta_envio CLIPPED
      SKIP 1 LINE
      PRINT
        COLUMN 03,"Nombre y Ruta Reporte a Detalle :"
      PRINT
        COLUMN 05,G_LISTA CLIPPED
      SKIP 2 LINE
      PRINT
        COLUMN 03,"========================================",
        COLUMN 40,"==================================================="

END REPORT

REPORT listado_4(vsolv_env)    ---erm 13 Marzo 2006

  DEFINE
    cont_reg INTEGER,
    l_sol    CHAR(12),
    g_nombre CHAR(200),
    vtip_sol SMALLINT,
    vsolv_env SMALLINT

  OUTPUT
    LEFT MARGIN   0
    RIGHT MARGIN  0
    TOP MARGIN    0
    BOTTOM MARGIN 0

  FORMAT
--    PAGE HEADER
--    ON EVERY ROW

    ON LAST ROW

      PRINT
        COLUMN 03,"========================================",
        COLUMN 40,"==================================================="
      PRINT
        COLUMN 15,"    APERTURA DE LA CUENTA"
      PRINT
        COLUMN 03,"----------------------------------------",
        COLUMN 40,"---------------------------------------------------"
      SKIP 2 LINE
      PRINT
        COLUMN 03,"Fecha                  : ", TODAY USING "dd-mm-yyyy"
      SKIP 1 LINE
      PRINT 
        COLUMN 03,"Tipo                   : ", "8", "  ","NO AFILIADOS"
      SKIP 1 LINE
      PRINT
        COLUMN 03,"Clave Operador         : ", g_usuario CLIPPED
      SKIP 1 LINE
      PRINT
        COLUMN 03,"Nombre Archivo Procesar: ","__________________________________"
      SKIP 1 LINE
      PRINT
        COLUMN 03,"Hora de Generacion     : ",HORA CLIPPED
      SKIP 1 LINE
      PRINT 
        COLUMN 03,"No. Registros Aperturados : ", vsolv_env USING "#####"
      SKIP 1 LINE
      PRINT
        COLUMN 03,"No. Registros con CURP asignada   : ", vsolv_env USING "#####"
      SKIP 1 LINE
      PRINT
        COLUMN 03,"No. Registros con CURP no asignada: "
      SKIP 1 LINE
      PRINT
        COLUMN 03,"Registros con Marca de 56 anios   : "
      SKIP 1 LINE
      PRINT
        COLUMN 03,"Registros sin Marca de 56 anios   : "
      SKIP 1 LINE
      PRINT
        COLUMN 03,"Nombre y Ruta Reporte a Detalle :"
      PRINT
        COLUMN 05,G_LISTA CLIPPED
      SKIP 2 LINE
      PRINT
        COLUMN 03,"========================================",
        COLUMN 40,"==================================================="

END REPORT

FUNCTION limpia_nulos()

    LET comm = "cd " ,g_seg_modulo.ruta_envio CLIPPED,
               "/ ; sed -e '/^$/d' ","E",HOY USING "yyyymmdd" CLIPPED,
               consec CLIPPED,".IND > informix" 
    RUN comm

    LET comm = "cd " ,g_seg_modulo.ruta_envio CLIPPED,
               "/ ; mv informix  ","E",HOY USING "yyyymmdd" CLIPPED,
               consec CLIPPED,".IND " 
    RUN comm

END FUNCTION

FUNCTION actualiza_bat_f(v_folio)
#ab-----------------------------

  DEFINE 
    v_cat          CHAR(600),
    vv_fecha_log   CHAR(030),
    vv_prog        CHAR(010),
    paso           CHAR(100)

  DEFINE 
    v_fecha_log    DATETIME YEAR TO SECOND

  DEFINE 
    v_folio        INTEGER

  DEFINE 
    reg_ruta       RECORD LIKE seg_modulo.*

  SELECT A.*
  INTO   reg_ruta.*
  FROM   seg_modulo A
  WHERE  modulo_cod = "bat"
 
  UPDATE bat_ctr_operacion
  SET    folio       = NULL ,      
         estado_cod  = 4    ,
         fecha_fin   = CURRENT,
         nom_archivo = nom_afi
  WHERE  pid         = reg_bat.pid
  AND    proceso_cod = reg_bat.proceso_cod
  AND    opera_cod   = reg_bat.opera_cod

  UPDATE bat_ctr_proceso
  SET    folio       = NULL ,      
         estado_cod  = 4    ,
         fecha_fin   = CURRENT,
         nom_archivo = nom_afi
  WHERE  pid         = reg_bat.pid
  AND    proceso_cod = reg_bat.proceso_cod

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

  LET paso = "nohup:"                     ,
      reg_bat.pid         USING"&&&&&",":",
      reg_bat.proceso_cod USING"&&&&&",":",
      reg_bat.opera_cod   USING"&&&&&"
  
  LET v_cat = "echo '"                      ,
              vv_fecha_log[1,4]             ,   
              vv_fecha_log[6,7]             ,  
              vv_fecha_log[9,10]            ,  
              vv_fecha_log[12,13]           ,   
              vv_fecha_log[15,16]           ,    
              vv_fecha_log[18,19]           ,
              "|"                           ,
              vv_prog  CLIPPED              ,
              "|"                           ,
              "FINOK"                       ,
              "|"                           ,
              reg_ruta.ruta_listados CLIPPED,  
              "/"                           ,
              paso CLIPPED                  ,
              "'"                           ,
              " >> "                        ,
              reg_ruta.ruta_envio CLIPPED   ,
              "/"                           ,
              "aad_safre.log"

  LET v_cat = v_cat CLIPPED
  RUN v_cat

END FUNCTION

REPORT listado_5(ln_seguro, ln_unico, lfolio_solicitud, lpaterno, lmaterno,
                 lnombres, lcod_promotor, lstatus_prom )
#----------------------

  DEFINE
   ln_seguro            CHAR(11)  ,
   ln_unico             CHAR(18)  ,
   lfolio_solicitud     CHAR(08)  ,
   lpaterno             CHAR(40)  ,
   lmaterno             CHAR(40)  ,
   lnombres             CHAR(40)  ,
   lcod_promotor        CHAR(10)  ,
   lstatus_prom         SMALLINT  ,
   lnombre_comp         CHAR(120)

   DEFINE
     x_afore RECORD LIKE tab_afore_local.*

   DEFINE
     tot SMALLINT

   OUTPUT
     LEFT   MARGIN 0
     RIGHT  MARGIN 0
     TOP    MARGIN 0
     BOTTOM MARGIN 0
     PAGE   LENGTH 66

   FORMAT
   PAGE HEADER

        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'

        SELECT *
        INTO   x_afore.*
        FROM   tab_afore_local

        PRINT
            COLUMN 001,"=================================================",
            COLUMN 050,"=================================================",
                       "==========="
        PRINT
            COLUMN 001,x_afore.razon_social  CLIPPED                      ,
            COLUMN 035,"LISTA RECHAZOS REG NO AFIL   A CERT POR PROMOTOR" ,
            COLUMN 088,"FECHA     : ",TODAY USING "DD-MM-YYYY"
        PRINT
            COLUMN 001,"AFIB006"                                          ,
            COLUMN 035,"RECHAZO POR PROMOTOR"                             ,
            COLUMN 088,"No.PAGINA :",PAGENO USING "##########"
        PRINT
            COLUMN 001,"=================================================",
            COLUMN 050,"=================================================",
                       "=========="

        PRINT
            COLUMN 001,"NSS "        ,
            COLUMN 013,"CURP"        ,
            COLUMN 032,"FOLIO SOL"   ,
            COLUMN 042,"NOMBRE"      ,
            COLUMN 093,"COD PROM"    ,
            COLUMN 104,"STATUS"

        PRINT
            COLUMN 001,"-------------------------------------------------",
            COLUMN 050,"-------------------------------------------------",
                       "-----------"

    ON EVERY ROW
        LET lnombre_comp = NULL
        LET lnombre_comp = lnombres CLIPPED, " ",
                           lpaterno CLIPPED, " ",
                           lmaterno CLIPPED

        PRINT
            COLUMN 001,ln_seguro               ,
            COLUMN 013,ln_unico                ,
            COLUMN 032,lfolio_solicitud        ,
            COLUMN 042,lnombre_comp     CLIPPED,
            COLUMN 093,lcod_promotor           ,
            COLUMN 104,lstatus_prom

END REPORT

--# 
REPORT listado_d(lr_d)
  DEFINE  lr_d         RECORD
        nss            CHAR(11),
        n_folio        DECIMAL(10,0),
        tipo_sol       SMALLINT,
        proceso        CHAR(02)
                       END RECORD

    DEFINE
        d    CHAR(02)  ,
        m    CHAR(02)  ,
        a    CHAR(04)
    DEFINE
        dia  CHAR(02)  ,
        mes  CHAR(02)  ,
        ano  CHAR(04)  ,
        dia1 CHAR(02)  ,
        mes1 CHAR(02)  ,
        ano1 CHAR(04)

    DEFINE
        hoy_env  CHAR(8),
        tot_char CHAR(9),
        num10d   CHAR(10)

    DEFINE
        tot         ,
        cod_err_ori ,
        var         SMALLINT

    DEFINE
       ltel1        CHAR(10),
       ltel2        CHAR(10),
       lind_tel1    CHAR(03),
       lind_tel2    CHAR(03),
       lexten1      CHAR(05),
       lexten2      CHAR(05),
       lcorreo_e    CHAR(40),
       lpais        CHAR(03),
       lfol_ife     CHAR(13),
       locr_ife     CHAR(13),
       lhr_loc_tel1 CHAR(08),
       ltel_dia1    CHAR(01),
       lhr_loc_tel2 CHAR(08),
       ltel_dia2    CHAR(01)

    DEFINE x_afore RECORD LIKE tab_afore_local.*

    OUTPUT
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0
        PAGE LENGTH 2

    FORMAT
    FIRST PAGE HEADER

        SELECT * 
        INTO   x_afore.*
        FROM   tab_afore_local

        LET hoy_env = YEAR(HOYDIA) USING "&&&&",
                      MONTH(HOYDIA)USING "&&"  ,
                      DAY(HOYDIA)  USING "&&"

        PRINT COLUMN 001,"01",
                         "01",
                         "33",
                         "01",
                         x_afore.codigo_afore USING "&&&",
                         "03",
                         "001",
                         hoy_env,
                         h_corrd USING "&&&",  #CORRELATIVO DE ENVIO
                         473 SPACES

    ON EVERY ROW

        LET dia1   = DAY(TODAY)   USING "&&"
        LET mes1   = MONTH(TODAY) USING "&&"
        LET ano1   = YEAR(TODAY)  USING "&&&&"
        LET num10d = num          USING "&&&&&&&&&&"
        FOR    gi_x          =  1    TO  100
               INITIALIZE   ga_d[gi_x]   TO   NULL            --- Incluyendo MLM2768 CUO  Sep 2014  
        END FOR

        CALL   fn_ObtOp33(gs_idope, lr_d.*)
        IF     gi_tra        >  0    THEN
               FOR   gi_x       =  1  TO   gi_tra
                     PRINT  COLUMN 1, ga_d[gi_x].lin
               END FOR
               LET   tot         =   tot  + gi_tra
        END IF

    ON LAST ROW
        LET tot_char = tot USING "&&&&&&&&&"

        PRINT COLUMN 001, "09"      ,
                          tot_char  ,
                          489 SPACES

END REPORT


