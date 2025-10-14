###########################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                        #
#Propietario       => E.F.P.                                              #
#Programa SEPB006  => GENERACION DE ARCHIVO CERTIFICACION TRASPASOS       #
#Sistema           => SEP                                                 #
#Autor             => MAURO MUNIZ CABALLERO                               #
#Fecha             => 18 DE ENERO DE 2001                                 #
#Actualizacion     => MAURO MUNIZ CABALLERO (Proceso batch)               #
#Fecha_actualiza   => 21 DE AGOSTO DE 2002                                #
#Actualizacion     => MAURO MUNIZ CABALLERO (Adecuaciones circ 28-5)      #
#Fecha_actualiza   => 22 DE DICIEMBRE DE 2003                             #
#Actualizacion     => MAURO MUNIZ CABALLERO (Adecuaciones circ 28-7)      #
#                     GENERACION ARCHIVO DOMICILIOS                       #
###########################################################################

DATABASE safre_af

GLOBALS

	 DEFINE g_reg RECORD
		  generar CHAR(1)
	 END RECORD
	 DEFINE w_aux  RECORD
		  n_seguro      LIKE afi_solicitud.n_seguro     ,
		  n_unico       LIKE afi_solicitud.n_unico      ,
		  n_rfc         LIKE afi_solicitud.n_rfc        ,
		  paterno       LIKE afi_solicitud.paterno      ,
		  materno       LIKE afi_solicitud.materno      ,
		  nombres       LIKE afi_solicitud.nombres      ,
		  fena          DATE                            ,
		  cod_promotor  LIKE afi_solicitud.cod_promotor ,
		  sexo          LIKE afi_solicitud.sexo         ,
		  estadon       LIKE afi_solicitud.estadon      ,
		  n_folio       LIKE afi_solicitud.n_folio      ,
		  frecafor      LIKE afi_solicitud.frecafor     ,
		  nacionalidad  LIKE afi_solicitud.nacionalidad ,
		  tip_prob      LIKE afi_solicitud.tip_prob     ,
		  fol_prob      LIKE afi_solicitud.fol_prob     ,
		  doc_prob      LIKE afi_solicitud.doc_prob     ,
		  ind_infonavit LIKE afi_solicitud.ind_infonavit,
		  cod_error_ori SMALLINT                        ,
		  folio_edo_cta CHAR(8)                         ,
		  cod_afore_ced SMALLINT                        ,
		  id_reenvio    CHAR(1)                         ,
		  fecha_envio   DATE                            ,
		  femision      DATE                            ,
		  fecha_elaboracion DATE
	 END RECORD
 
	 DEFINE k_aux  RECORD
		  tipo_reg          CHAR(02) ,
		  con_dservicio     CHAR(10) ,
		  clave_doperacion  CHAR(02) ,
		  fech_recep_afor   CHAR(08) ,
		  folio_solicitud   CHAR(10)
	 END RECORD

	 DEFINE 
		  HOY             DATE ,
		  HOYDIA          DATE ,
		  fecha_hasta     DATE

	 DEFINE
		  num             SMALLINT  ,
		  HAY_REGISTROS   SMALLINT  ,
		  h_corr          SMALLINT  ,
		  h_corrd         SMALLINT  ,
		  bnd_proceso     SMALLINT  ,
		  diaSemana       SMALLINT  ,
		  vsoli_env       INTEGER 

	 DEFINE
		  enter           CHAR(1)   ,
		  g_opcion        CHAR(2)   ,
		  g_usuario       CHAR(8)   ,
		  HORA            CHAR(8)   ,
		  consec          CHAR(10)  ,
		  consecd         CHAR(10)  ,
		  G_LISTA         CHAR(100) ,
		  nom_taa         CHAR(500) ,
		  comm            CHAR(500) ,
        list_salida     CHAR(500)

    DEFINE x_lotes      RECORD LIKE tab_lote.*
    DEFINE g_afore      RECORD LIKE tab_afore_local.*
    DEFINE g_seg_modulo RECORD LIKE seg_modulo.*

    DEFINE reg_bat RECORD
        pid            INTEGER,
        proceso_cod    INTEGER,
        opera_cod      INTEGER,
        nombre_archivo CHAR(25)
    END RECORD

    DEFINE reg_dom RECORD
        tipo_reg    CHAR(2),
        cont_serv   DECIMAL(10,0),
        tipo_recep  CHAR(2),
        cve_recep   CHAR(3),
        nss_afore   CHAR(11),
        calle       CHAR(65),
        no_ext      CHAR(15),
        no_int      CHAR(15),
        colonia     CHAR(65),
        delegacion  CHAR(65),
        cp          CHAR(5),
        ent_fed     CHAR(65)
    END RECORD

    DEFINE lote_dom    SMALLINT
    DEFINE vdelegacion CHAR(65)
    DEFINE vent_fed    CHAR(65)

    DEFINE g_dom       CHAR(100)

END GLOBALS

MAIN

    DISPLAY " "
    DISPLAY ".1"


    CALL STARTLOG('SEPB006.log')
    CALL inicio()

    IF NOT bnd_proceso THEN
        DEFER INTERRUPT
        OPTIONS INPUT WRAP,
        PROMPT LINE LAST,
        ACCEPT KEY CONTROL-I

        CALL proceso_principal()   #pp
    ELSE
        CALL fecha_max()
        CALL rescata_valores()     #rv
        CALL actualiza_bat_f(0) #ao
    END IF


END MAIN

FUNCTION inicio()
#----------------

    LET reg_bat.pid         = ARG_VAL(1)
    LET reg_bat.proceso_cod = ARG_VAL(2)
    LET reg_bat.opera_cod   = ARG_VAL(3)

    LET bnd_proceso = 0

    IF reg_bat.pid THEN
        DISPLAY "INICIANDO PROCESO ..."
        LET bnd_proceso = 1
    END IF 

    LET g_reg.generar          = "S"
    LET k_aux.tipo_reg         = "02"  
    LET k_aux.con_dservicio    = "          "
    LET k_aux.clave_doperacion = "01" 
    LET k_aux.fech_recep_afor  = "       " 
    LET k_aux.folio_solicitud  = "          "
    LET	g_opcion               = "02"

    SELECT *
    INTO   g_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = 'sep'

    SELECT *,USER
    INTO   g_afore.*,g_usuario 
    FROM   tab_afore_local

    LET reg_dom.tipo_reg   = '02'
    LET reg_dom.tipo_recep = '01'
    LET reg_dom.cve_recep  =  g_afore.codigo_afore USING '&&&'

    LET HOY    = TODAY
    LET HORA   = TIME
    LET num    = 1

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 4,4 WITH FORM "SEPB0061" ATTRIBUTE(BORDER)

    DISPLAY " SEPB006     GENERA LOTE CERTIFICACION REGISTRO SEPARACION                     " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                               < Ctrl-C > Salir                                " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)
    DISPLAY g_seg_modulo.ruta_envio AT 10,10

    INPUT BY NAME g_reg.generar
        AFTER FIELD generar

        IF g_reg.generar IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD generar
        END IF

        IF g_reg.generar  MATCHES "[Ss]" THEN
            CALL fecha_max()
            CALL rescata_valores()

            IF NOT HAY_REGISTROS THEN
                ERROR "NO HAY REGISTROS PARA PROCESAR..."
                SLEEP 3 
                EXIT PROGRAM
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
    ERROR""
    PROMPT "PROCESO CONCLUIDO [Enter] para integrar lote... " ATTRIBUTE(REVERSE)
    FOR enter

END FUNCTION

FUNCTION rescata_valores()
#rv-----------------------

    LET HAY_REGISTROS = FALSE

    SELECT COUNT(*)
    INTO   HAY_REGISTROS
    FROM   afi_solicitud a, afi_domicilio o
    WHERE  a.status_interno = 10 #CON TODOS LOS DOCTOS
    AND    a.tipo_solicitud = 6 #AFILIADO POR TRASPASO
    AND    a.frecafor      <= fecha_hasta
    AND    a.n_seguro       = o.nss
    AND    a.n_folio        = o.n_folio
    AND    a.tipo_solicitud = o.tipo_solicitud
    AND    o.marca_envio    = "X"

    IF HAY_REGISTROS THEN
        SELECT t.*
        INTO   x_lotes.*
        FROM   tab_lote t
        WHERE  t.lotes_cod = 2
        AND    lotes_fecha = HOYDIA

        IF SQLCA.SQLCODE = 0 THEN
            UPDATE tab_lote
            SET    lotes_correlativo = lotes_correlativo + 1
            WHERE  lotes_cod = 2
            AND    lotes_fecha = HOYDIA
        ELSE
            INSERT INTO tab_lote
            VALUES(HOYDIA     ,
                   2          ,
                   'AFILIADOS',
                   1          ,
                   1)
        END IF

        SELECT tl.lotes_correlativo
        INTO   h_corr
        FROM   tab_lote tl
        WHERE  tl.lotes_cod = 2
        AND    tl.lotes_fecha = HOYDIA

        LET consec = h_corr

        UPDATE tab_lote
        SET    lotes_correlativo = lotes_correlativo + 1
        WHERE  lotes_cod = 2
        AND    lotes_fecha = HOYDIA

        SELECT l.lotes_correlativo
        INTO   h_corrd
        FROM   tab_lote l
        WHERE  l.lotes_cod = 2
        AND    l.lotes_fecha = HOYDIA

        LET consecd = h_corrd

        LET vsoli_env = hay_registros

        IF NOT bnd_proceso THEN
            DISPLAY "SOLICITUDES A SER ENVIADAS ", vsoli_env AT 15,20 
        ELSE
            DISPLAY "SOLICITUDES A SER ENVIADAS ", vsoli_env 
        END IF

        LET HAY_REGISTROS = FALSE

        DECLARE curs_1 CURSOR FOR
        SELECT A.n_seguro     ,
               A.n_unico      ,
               A.n_rfc        ,
               A.paterno      ,
               A.materno      ,
               A.nombres      ,
               A.fena         ,
               A.cod_promotor ,
               A.sexo         ,
               A.estadon      ,
               A.n_folio      ,
               A.frecafor     ,
               A.nacionalidad ,
               A.tip_prob     ,
               A.fol_prob     ,
               A.doc_prob     ,
               A.ind_infonavit,
               A.cod_error_origen,
               ' ',
               A.cod_afore_ced,
               A.documento_2,
               A.fecha_envio,
               A.femision,
               A.fecha_elaboracion,
               O.calle,
               O.numero,
               O.depto,
               O.colonia,
               D.deleg_desc,
               O.codpos,
               E.estad_desc
        FROM   afi_solicitud A,
        OUTER (afi_domicilio O,
        OUTER  tab_estado E,
        OUTER  tab_delegacion D)
        WHERE  A.status_interno  = 10 #CON TODOS LOS DOCTOS
        AND    A.tipo_solicitud  =  6 --solicitud de traspaso
        AND    A.frecafor       <= fecha_hasta
        AND    A.n_seguro           = O.nss
        AND    A.n_folio            = O.n_folio
        AND    A.tipo_solicitud     = O.tipo_solicitud
        AND    O.marca_envio        = "X"
        AND    E.estad_cod          = O.estado
        AND    D.estad_cod          = O.estado
        AND    D.deleg_cod          = O.delega

        DISPLAY "PROCESANDO INFORMACION " AT 16,2

        LET comm = g_seg_modulo.ruta_envio CLIPPED,
                   "/E",HOYDIA USING "yyyymmdd" CLIPPED,
                   consec CLIPPED, ".AFI" CLIPPED

        DISPLAY "Archivo : ", comm CLIPPED AT 16,2

        LET nom_taa = "E",HOYDIA USING "yyyymmdd" CLIPPED,
                      consec CLIPPED, ".AFI" CLIPPED

        LET G_LISTA = g_seg_modulo.ruta_listados CLIPPED,
                      "/",g_usuario CLIPPED,
                      ".ARCHIVO_TRASP_CERT." CLIPPED,
                      HOY USING "dd-mm-yy","_",consec CLIPPED,
                      "_",HORA CLIPPED

--        LET g_dom   = g_seg_modulo.ruta_envio CLIPPED,
                      --"/T",HOYDIA USING "yyyymmdd" CLIPPED,
                      --consecd CLIPPED, ".DOM" CLIPPED

        START REPORT listado_2 TO G_LISTA
       -- START REPORT listado_d TO g_dom
        START REPORT listado TO comm

        FOREACH curs_1 INTO w_aux.*,
                            reg_dom.calle,
                            reg_dom.no_ext,
                            reg_dom.no_int,
                            reg_dom.colonia,
                            reg_dom.delegacion,
                            reg_dom.cp,
                            reg_dom.ent_fed

        LET HAY_REGISTROS = TRUE
        LET k_aux.folio_solicitud = w_aux.n_folio

        IF w_aux.materno IS NULL OR
           w_aux.materno MATCHES '  *' THEN
            LET w_aux.materno = "N/A"
        END IF 

        LET w_aux.folio_edo_cta = '        '

        LET reg_dom.nss_afore = w_aux.n_seguro

        OUTPUT TO REPORT listado(k_aux.tipo_reg        ,
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
                                 w_aux.frecafor        ,
                                 w_aux.nacionalidad    ,
                                 w_aux.tip_prob        ,
                                 w_aux.fol_prob        ,
                                 w_aux.doc_prob        ,
                                 w_aux.ind_infonavit   ,
                                 w_aux.cod_error_ori   ,
                                 w_aux.folio_edo_cta   ,
                                 w_aux.cod_afore_ced   ,
                                 w_aux.id_reenvio      ,
                                 w_aux.femision        ,
                                 w_aux.fecha_elaboracion
                                )

       -- OUTPUT TO REPORT listado_d(reg_dom.*)

        OUTPUT TO REPORT listado_2(k_aux.tipo_reg        ,
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
                                   w_aux.frecafor        ,
                                   w_aux.nacionalidad    ,
                                   w_aux.tip_prob        ,
                                   w_aux.fol_prob        ,
                                   w_aux.doc_prob        ,
                                   w_aux.ind_infonavit   ,
                                   w_aux.cod_error_ori   ,
                                   w_aux.folio_edo_cta   ,
                                   w_aux.cod_afore_ced
                                  )

        update sep_det_reg_sol_reclamante
        set    estado = 51  ,
               fecha_proceso = today
        where  nss = w_aux.n_seguro 
        and    estado   = 5

        LET num = num + 1

        END FOREACH

        FINISH REPORT listado
       -- FINISH REPORT listado_d
        FINISH REPORT listado_2
   
        CALL limpia_nulos()

        UPDATE afi_solicitud 
        SET    afi_solicitud.status_interno = 30,
               afi_solicitud.lote = h_corr,
               afi_solicitud.fecha_envio = HOYDIA
        WHERE  afi_solicitud.status_interno = 10
        AND    afi_solicitud.tipo_solicitud = 6
        AND    afi_solicitud.frecafor       <= fecha_hasta
    ELSE
        IF bnd_proceso THEN
            DISPLAY "Program stopped, NO HAY SOLICITUDES A ENVIAR"
        END IF
    END IF

END FUNCTION

REPORT listado(w_aux)
#--------------------

    DEFINE w_aux RECORD
        tipo_reg            CHAR(02) ,
        con_dservicio       CHAR(10) ,
        clave_doperacion    CHAR(02) ,
        n_seguro            LIKE afi_solicitud.n_seguro ,
        n_unico             LIKE afi_solicitud.n_unico  ,
        n_rfc               LIKE afi_solicitud.n_rfc    ,
        paterno             CHAR(40) ,
        materno             CHAR(40) ,
        nombres             CHAR(40) ,
        fena                DATE     ,
        cod_promotor        LIKE afi_solicitud.cod_promotor ,
        fech_recep_afor     CHAR(08) ,
        folio_solicitud     CHAR(08) ,
        sexo                LIKE afi_solicitud.sexo         ,
        estadon             LIKE afi_solicitud.estadon      ,
        frecafor            LIKE afi_solicitud.frecafor     ,
        nacionalidad        LIKE afi_solicitud.nacionalidad ,
        tip_prob            LIKE afi_solicitud.tip_prob     ,
        fol_prob            LIKE afi_solicitud.fol_prob     ,
        doc_prob            LIKE afi_solicitud.doc_prob     ,
        ind_infonavit       LIKE afi_solicitud.ind_infonavit,
        cod_error_ori       SMALLINT ,
        folio_edo_cta       CHAR(8)  ,
        cod_afore_ced       SMALLINT,
        id_reenvio          CHAR(1),
        femision            DATE ,
        fecha_elaboracion   DATE
    END RECORD

    DEFINE 
        nombre_comp         ,
        nombre_comp2        CHAR(120)

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
        num10    CHAR(10)

    DEFINE 
        x_afore RECORD LIKE tab_afore_local.*

    DEFINE 
        tot         ,
        cod_err_ori ,
        var         SMALLINT

    OUTPUT
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0
        PAGE LENGTH 66

    FORMAT
    FIRST PAGE HEADER
        LET hoy_env = YEAR(HOYDIA) USING "&&&&",
                      MONTH(HOYDIA)USING "&&"  ,
                      DAY(HOYDIA)  USING "&&"

        SELECT * 
        INTO   x_afore.*
        FROM   tab_afore_local

        PRINT COLUMN 001,"01",
                         "01",
                         "10",
                         "01",
                         x_afore.codigo_afore USING "&&&",
                         "03",
                         "001",
                         hoy_env,
                         "09 ",
                         h_corr USING "&&&",  #CORRELATIVO DE ENVIO
                         g_opcion USING "&&", #MEDIO POR DONDE SE ENVIA ARCHIVO
                         538 SPACES

    ON EVERY ROW

        LET a     = YEAR (w_aux.fecha_elaboracion)  USING "&&&&"
        LET m     = MONTH (w_aux.fecha_elaboracion) USING "&&"
        LET d     = DAY (w_aux.fecha_elaboracion)   USING "&&"
        LET dia   = DAY(w_aux.fena)        USING"&&"
        LET mes   = MONTH(w_aux.fena)      USING"&&"
        LET ano   = YEAR(w_aux.fena)       USING "&&&&"
        LET dia1  = DAY(TODAY)             USING"&&"
        LET mes1  = MONTH(TODAY)           USING"&&"
        LET ano1  = YEAR(TODAY)            USING "&&&&"
        LET num10 = num                    USING "&&&&&&&&&&"

        CASE w_aux.ind_infonavit
            WHEN 'S'  LET var = 1
            WHEN 'N'  LET var = 0
            WHEN '0'  LET var = 0
            WHEN '1'  LET var = 1
            WHEN '2'  LET var = 0
            OTHERWISE LET var = 0
        END CASE

        IF w_aux.tip_prob <> 5 THEN
            LET w_aux.n_unico = "                  "
        END IF

        PRINT
            COLUMN 1,"02"                   ,
                     num10                  ,
                     "33"                   ,
                     w_aux.n_seguro         ,
                     w_aux.n_unico          ,
                     w_aux.n_rfc            ,
                     w_aux.paterno          ,
                     w_aux.materno          ,
                     w_aux.nombres          ,
                     ano,mes,dia            ,
                     28 SPACES              ,
                     --w_aux.cod_promotor     USING "&&&&&&&&&&" ,
                     --a,m,d                  ,
                     --k_aux.folio_solicitud  USING "&&&&&&&&&&" ,
                     w_aux.sexo             USING "&"          ,
                     w_aux.estadon          USING "&&"         ,
                     var                    USING "&"          ,
                     w_aux.nacionalidad     ,
                     w_aux.tip_prob         ,
                  --   w_aux.fol_prob         ,
                  --   w_aux.doc_prob         ,
                  --   w_aux.cod_error_ori    USING "&&&&"       ,
                  --   w_aux.cod_afore_ced    USING "&&&"        ,
                  --   w_aux.id_reenvio                          ,
                     351 SPACES                                
                  --   w_aux.femision         USING "YYYYMMDD"

    ON LAST ROW
        LET tot = 0
        LET tot = count(*)
        LET tot_char = tot USING "&&&&&&&&&"

        PRINT COLUMN 001, "09"                                   ,
                          "01"                                   ,
                          x_afore.codigo_afore USING "&&&"       ,
                          hoy_env                                ,
                          h_corr USING "&&&"                     ,
                          "01"                                   ,
                          "10"                                   ,
                          tot_char                               ,
                         #"000000000",  total de solicitudes respondidas
                          539 SPACES

        SELECT 'X'
        FROM   est_det_diario edd
        WHERE  edd.fecha_detalle  = HOYDIA
        AND    edd.nombre_archivo = nom_taa

        IF SQLCA.SQLCODE <> 0 THEN
            INSERT INTO est_det_diario
            VALUES (HOYDIA,
                    30,
                    tot,
                    2,
                    nom_taa)
        END IF

END REPORT

REPORT listado_d(reg_dom)
#------------------------


    DEFINE reg_dom RECORD
        tipo_reg    CHAR(2),
        cont_serv   DECIMAL(10,0),
        tipo_recep  CHAR(2),
        cve_recep   CHAR(3),
        nss_afore   CHAR(11),
        calle       CHAR(65),
        no_ext      CHAR(15),
        no_int      CHAR(15),
        colonia     CHAR(65),
        delegacion  CHAR(65),
        cp          CHAR(5),
        ent_fed     CHAR(65)
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

    OUTPUT
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0
        PAGE LENGTH 66

    FORMAT
    FIRST PAGE HEADER
        LET hoy_env = YEAR(HOYDIA) USING "&&&&",
                      MONTH(HOYDIA)USING "&&"  ,
                      DAY(HOYDIA)  USING "&&"

        PRINT COLUMN 001,"01",
                         "02",
                         "33",
                         "01",
                         reg_dom.cve_recep USING "&&&",
                         "03",
                         "001",
                         "09 ",
                         hoy_env,
                         h_corrd USING "&&&",  #CORRELATIVO DE ENVIO
                         g_opcion USING "&&",  #MEDIO POR DONDE SE ENVIA ARCHIVO
                         338 SPACES

    ON EVERY ROW

        LET dia1   = DAY(TODAY)   USING"&&"
        LET mes1   = MONTH(TODAY) USING"&&"
        LET ano1   = YEAR(TODAY)  USING "&&&&"
        LET num10d = num          USING "&&&&&&&&&&"

        PRINT
            COLUMN 1,reg_dom.tipo_reg       ,
                     num10d                 ,
                     reg_dom.tipo_recep     ,
                     reg_dom.cve_recep      ,
                     reg_dom.nss_afore      ,
                     reg_dom.calle          ,
                     reg_dom.no_ext         ,
                     reg_dom.no_int         ,
                     reg_dom.colonia        ,
                     reg_dom.delegacion     ,
                     reg_dom.cp             ,
                     reg_dom.ent_fed        ,
                     47 SPACES

    ON LAST ROW
        LET tot = 0
        LET tot = count(*)
        LET tot_char = tot USING "&&&&&&&&&"

        PRINT COLUMN 001, "09"      ,
                          tot_char  ,
                          359 SPACES

END REPORT

REPORT listado_2(w_aux)
#----------------------

    DEFINE w_aux  RECORD
        tipo_reg            CHAR(02)                    ,
        con_dservicio       CHAR(10)                    ,
        clave_doperacion    CHAR(02)                    ,
        n_seguro            LIKE afi_solicitud.n_seguro ,
        n_unico             LIKE afi_solicitud.n_unico  ,
        n_rfc               LIKE afi_solicitud.n_rfc    ,
        paterno             CHAR(40)                    ,
        materno             CHAR(40)                    ,
        nombres             CHAR(40)                    ,
        fena                DATE                        ,
        cod_promotor        LIKE afi_solicitud.cod_promotor,
        fech_recep_afor     CHAR(08)                    ,
        folio_solicitud     CHAR(08)                    ,
        sexo                LIKE afi_solicitud.sexo     ,
        estadon             LIKE afi_solicitud.estadon  ,
        frecafor            LIKE afi_solicitud.frecafor ,
        nacionalidad        LIKE afi_solicitud.nacionalidad ,
        tip_prob            LIKE afi_solicitud.tip_prob ,
        fol_prob            LIKE afi_solicitud.fol_prob ,
        doc_prob            LIKE afi_solicitud.doc_prob ,
        ind_infonavit       LIKE afi_solicitud.ind_infonavit,
        cod_error_ori       SMALLINT,
        folio_edo_cta       CHAR(8),
        cod_afore_ced       SMALLINT
    END RECORD

    DEFINE 
        nombre_comp  ,
        nombre_comp2 CHAR(120)

    DEFINE
        d CHAR(02),
        m CHAR(02),
        a CHAR(04)

    DEFINE
        dia      CHAR(2),
        mes      CHAR(2),
        ano      CHAR(4),
        dia1     CHAR(2),
        mes1     CHAR(2),
        ano1     CHAR(4),
        hoy      CHAR(8),
        num10    CHAR(10),
        tot_char CHAR(9)

    DEFINE 
        x_afore RECORD LIKE tab_afore_local.*

    DEFINE 
        tot SMALLINT

    OUTPUT
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0
        PAGE LENGTH 66

    FORMAT
    PAGE HEADER
        LET hoy = YEAR(TODAY) USING "&&&&",
                  MONTH(TODAY)USING "&&"  ,
                  DAY(TODAY)  USING "&&"

        SELECT * 
        INTO   x_afore.*
        FROM   tab_afore_local

        PRINT
            COLUMN 01,"=================================================",
            COLUMN 50,"=============================="
        PRINT
            COLUMN 01,x_afore.razon_social  CLIPPED                      ,
            COLUMN 35,"LISTA DE AFILIADOS"                               ,
            COLUMN 60,"FECHA     :",TODAY     USING "DD-MM-YYYY"
        PRINT
            COLUMN 01,"SEPB006"                                          ,
            COLUMN 35,"ENVIADOS A PROCESAR"                              ,
            COLUMN 60,"No.PAGINA:",PAGENO    USING "##########"
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
            COLUMN 37,"CVE.PROM"    ,
            COLUMN 50,"F.RECEP"     ,
            COLUMN 63,"F.SOLIC"     ,
            COLUMN 76,"SEXO"        ,
            COLUMN 80,"ENT.NAC"

        PRINT 
            COLUMN 01,"CRED.INFONAVIT"      ,
            COLUMN 17,"NAC."                ,
            COLUMN 22,"TIP.PROB"            ,
            COLUMN 32,"FOL.PROB"            ,
            COLUMN 42,"DOC.PROB"            ,
            COLUMN 59,"FOLIO EDO CTA"       ,
            COLUMN 72,"AFO. CED."

        PRINT
            COLUMN 01,"-------------------------------------------------",
            COLUMN 50,"------------------------------"

    ON EVERY ROW
        LET a     = YEAR (w_aux.frecafor)  using "&&&&"
        LET m     = MONTH (w_aux.frecafor) using "&&"
        LET d     = DAY (w_aux.frecafor)   using "&&"
        LET dia   = DAY(w_aux.fena)        USING"&&"
        LET mes   = MONTH(w_aux.fena)      USING"&&"
        LET ano   = YEAR(w_aux.fena)       USING "&&&&"
        LET dia1  = DAY(TODAY)             USING"&&"
        LET mes1  = MONTH(TODAY)           USING"&&"
        LET ano1  = YEAR(TODAY)            USING "&&&&"
        LET num10 = "          "
        LET num10 = num                    USING "&&&&&&&&&&"

        PRINT
            COLUMN 01,w_aux.n_seguro                            ,
            COLUMN 17,w_aux.n_unico                             ,
            COLUMN 38,w_aux.n_rfc                               ,
            COLUMN 54,w_aux.paterno [1,15]                      ,
            COLUMN 72,w_aux.materno [1,15]                       
        PRINT
            COLUMN 01,w_aux.nombres [1,20]                      ,
            COLUMN 24,ano,mes,dia                               ,
            COLUMN 37,w_aux.cod_promotor           USING "&&&&&&&&&&" ,
            COLUMN 50,a,m,d                                     ,
            COLUMN 63,k_aux.folio_solicitud  USING "&&&&&&&&&&" ,
            COLUMN 76,w_aux.sexo             USING "&"          ,
            COLUMN 80,w_aux.estadon          USING "&&"

        PRINT
            COLUMN 01,w_aux.ind_infonavit                      ,
            COLUMN 17,w_aux.nacionalidad                       ,
            COLUMN 22,w_aux.tip_prob                           ,
            COLUMN 32,w_aux.fol_prob                           ,
            COLUMN 42,w_aux.doc_prob                           ,
            COLUMN 59,w_aux.folio_edo_cta    USING "&&&&&&&&"  ,
            COLUMN 72,w_aux.cod_afore_ced    USING "&&&"

END REPORT

FUNCTION Tipo_de_envio()
#te---------------------

    DEFINE z_reg ARRAY[6] OF RECORD
        cod  CHAR(2),
        desc CHAR(20)
    END RECORD

    DEFINE i SMALLINT

    LET z_reg[1].cod = "01" LET z_reg[1].desc = "CINTA"
    LET z_reg[2].cod = "02" LET z_reg[2].desc = "TRA"
    LET z_reg[3].cod = "03" LET z_reg[3].desc = "EN LINEA"
    LET z_reg[4].cod = "04" LET z_reg[4].desc = "LIB.PENDIENTE"
    LET z_reg[5].cod = "05" LET z_reg[5].desc = "CARTUCHO"
    LET z_reg[6].cod = "06" LET z_reg[6].desc = "DAT"

    OPEN WINDOW cent AT 11,25 WITH FORM "SEPB0022" ATTRIBUTE(BORDER)

    DISPLAY "       MEDIOS  DE  ENVIO      " AT 2,1 ATTRIBUTE(REVERSE)

    FOR i = 1 TO 6
        DISPLAY z_reg[i].* TO scr_1[i].*
    END FOR

    INPUT BY NAME g_opcion
        AFTER FIELD g_opcion
        IF g_opcion IS NULL THEN
            ERROR "OPCION NO PUEDE SER NULA"
            NEXT FIELD g_opcion
        END IF

        IF g_opcion <> "01" AND
           g_opcion <> "02" AND
           g_opcion <> "03" AND
           g_opcion <> "04" AND
           g_opcion <> "05" AND
           g_opcion <> "06" THEN
            ERROR "Opcion ERRONEA, reingrese"
            NEXT FIELD g_opcion
        ELSE
            EXIT INPUT
        END IF

    END INPUT

    CLOSE WINDOW cent

END FUNCTION

FUNCTION limpia_nulos()
#ln--------------------

    LET comm = "cd " ,g_seg_modulo.ruta_envio CLIPPED,
               "/ ; sed -e '/^$/d' ","E",HOYDIA USING "yyyymmdd" CLIPPED,
               consec CLIPPED,".AFI > ",g_seg_modulo.ruta_envio CLIPPED,
               "/pso " 
     RUN comm

    LET comm = "mv ",g_seg_modulo.ruta_envio CLIPPED,"/pso ",
               g_seg_modulo.ruta_envio CLIPPED,
               "/E",HOYDIA USING "yyyymmdd" CLIPPED,
               consec CLIPPED,".AFI " 

    RUN comm

--    LET comm = "cd " ,g_seg_modulo.ruta_envio CLIPPED,
               --"/ ; sed -e '/^$/d' ","T",HOYDIA USING "yyyymmdd" CLIPPED,
               --consecd CLIPPED,".DOM > informix" 
    --RUN comm

    --LET comm = "cd " ,g_seg_modulo.ruta_envio CLIPPED,
               --"/ ; mv informix  ","T",HOYDIA USING "yyyymmdd" CLIPPED,
               --consecd CLIPPED,".DOM " 
   -- RUN comm

END FUNCTION

FUNCTION fecha_max()
#fh-------------------

    DEFINE diaSemana SMALLINT

    LET diaSemana = WEEKDAY(HOY)

    LET fecha_hasta = HOY # - diaSemana UNITS DAY

    CALL fechas(fecha_hasta) RETURNING HOYDIA
 LET HOYDIA = TODAY
END FUNCTION

FUNCTION actualiza_bat_f(v_folio)
#ab-----------------------------

define v_cat          CHAR(600),
       vv_fecha_log   CHAR(030),
       vv_prog        CHAR(010),
       paso           CHAR(100)

define v_fecha_log DATETIME YEAR TO SECOND

define v_folio  integer
define reg_ruta RECORD LIKE seg_modulo.*

SELECT A.*
INTO reg_ruta.*
FROM  seg_modulo A
WHERE modulo_cod = "bat"
 
UPDATE bat_ctr_operacion
set    folio      = NULL ,      
       estado_cod = 4    ,
       fecha_fin  = CURRENT,
       nom_archivo = nom_taa
WHERE pid         = reg_bat.pid
and   proceso_cod = reg_bat.proceso_cod
and   opera_cod   = reg_bat.opera_cod

UPDATE bat_ctr_proceso
set    folio       = NULL ,      
       estado_cod  = 4    ,
       fecha_fin   = CURRENT,
       nom_archivo = nom_taa
WHERE  pid         = reg_bat.pid
and    proceso_cod = reg_bat.proceso_cod

UPDATE bat_tmp_predecesor
SET    bandera_ejecuta  = 1
WHERE  pid_prod         = reg_bat.pid
AND    proceso_cod_prod = reg_bat.proceso_cod
AND    opera_cod_prod   = reg_bat.opera_cod

LET v_fecha_log = CURRENT
LET vv_fecha_log = v_fecha_log

SELECT A.programa_cod 
INTO   vv_prog 
FROM   bat_ctr_operacion A
WHERE  A.pid         = reg_bat.pid
AND    A.proceso_cod = reg_bat.proceso_cod
AND    A.opera_cod   = reg_bat.opera_cod

LET paso = "nohup:"            ,
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

FUNCTION fechas(diaActual)
#sdh----------------------

    DEFINE
        diaTmp    DATE,
        contador  SMALLINT,
        diaActual DATE,
        numDias   SMALLINT

    LET diaTmp = diaActual

    FOR contador = 1 TO 3
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
#hs--------------------------------

    DEFINE
        diaTmp    DATE,
        contador  SMALLINT,
        diaActual DATE

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

