###########################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                        #
#Propietario       => E.F.P.                                              #
#Programa AFIB006  => GENERACION DE ARCHIVO CERTIFICACION TRASPASOS       #
#Sistema           => AFI                                                 #
#Autor             => MAURO MUNIZ CABALLERO                               #
#Fecha             => 18 DE ENERO DE 2001                                 #
#Actualizacion     => MAURO MUNIZ CABALLERO (Proceso batch)               #
#Fecha_actualiza   => 21 DE AGOSTO DE 2002                                #
#Actualizacion     => MAURO MUNIZ CABALLERO (Adecuaciones circ 28-5)      #
#Fecha_actualiza   => 22 DE DICIEMBRE DE 2003                             #
#Actualizacion     => MAURO MUNIZ CABALLERO (Adecuaciones circ 28-7)      #
#                     GENERACION ARCHIVO DOMICILIOS                       #
#Fecha_actualiza   => 13 DE NOVIEMBRE DE 2006                             #
#Actualizacion     => FERNANDO HERRERA HERNANDEZ (Circ 28-13)             #
#                     SE ELIMINA COD OPERACION 06 (ACEPTADA)              #
#Actualizacion     => FERNANDO HERRERA HERNANDEZ (Adecuaciones 28-16)     #
#Fecha_actualiza   => 24 DE MARZO DE 2007                                 #
#Actualizacion     => FERNANDO HERRERA HERNANDEZ (Circular 28-18)         #
#Fecha actualiza   => 24 DE JUNIO DE 2008                                 #
#                  => 22 Dic 2009 FOLIO EDOCTA - EDUARDO RESENDIZ MEDINA  #
#                  => 3 Agosto 2010 CIRCULAR UNICA EDUARDO RESENDIZ MEDINA#
#                  => 24 Dic 2010 Cambios layout EDUARDO RESENDIZ MEDINA  #
#                  => 24 Marzo 2011 EDUARDO RESENDIZ MEDINA TAA X CURP    #
###########################################################################
#coppel
#Modificado        = JGHM -    Oct 2014  CPL1713 CUO Datos de Expediente Ident   # 
#                  - Formato op.4 y op.33                                        #
##################################################################################
#Modificado        = JGHM -    FEB 2015  CPL1713 CUO II Expediente  CUO          # 
#                  - Nuevo Formato op.4   g_cta.t_opera,                                       #
##################################################################################

DATABASE safre_af

GLOBALS

    DEFINE g_reg RECORD
        fecha_envio DATE,
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
        --folio_edo_cta CHAR(8)                         ,
        folio_edo_cta CHAR(20)                        ,   --folio edocta
        cuatrim_emision DECIMAL(5,0)                  ,
        cod_afore_ced SMALLINT                        ,
        id_reenvio    CHAR(1)                         ,
        fecha_envio   DATE                            ,
        femision      DATE                            ,
        fecha_elaboracion DATE                        ,
        documento_3   CHAR(1)                         ,
        folio_saftv   CHAR(10)                        ,
        cve_rend_neto CHAR(1)
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
        num2            SMALLINT  ,
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
        G_LISTA1        CHAR(100) ,
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
    DEFINE vfecha_envio DATE

    DEFINE g_dom       CHAR(100)
    DEFINE vtot_solv3  INTEGER

    DEFINE rech_prom   SMALLINT

    DEFINE vtot_img    INTEGER
    DEFINE vdiv_img    INTEGER
    DEFINE vgrado_afo  CHAR(25)
    DEFINE venv_img    INTEGER
    
    DEFINE reg_tel     RECORD LIKE afi_telefono.*
    DEFINE vtel1       CHAR(40),
           vtel2       CHAR(40)
    DEFINE vcont_tel   SMALLINT
    DEFINE vcc_tel     INTEGER
    DEFINE vtotal_tel  SMALLINT

    DEFINE vind_tel1   CHAR(03),
           vind_tel2   CHAR(03),
           vexten1     CHAR(05),
	   vexten2     CHAR(05),
           vcorreo_e   CHAR(40),
           vpais       CHAR(03),
           vfol_ife    CHAR(13),
           vocr_ife    CHAR(13),
           vtam        SMALLINT,
           vf          SMALLINT,
           mfec_ident  DATE,
           mrowid_ide  INTEGER,
           vcc_cor     INTEGER,
           tot_reenv   INTEGER,
           vcc_t_c     INTEGER,
           ban_tel     SMALLINT,
           ban_cor     SMALLINT,
           ban_t_c     SMALLINT    

    DEFINE mf_correo   DATE,
           mrowid_corr INTEGER,
           vtelefono1  CHAR(8),
           vtelefono2  CHAR(8)

    DEFINE g_hay RECORD
           n_folio  LIKE afi_solicitud.n_folio ,
           n_seguro LIKE afi_solicitud.n_seguro,
           paterno  LIKE afi_solicitud.paterno ,
           materno  LIKE afi_solicitud.materno ,
           nombres  LIKE afi_solicitud.nombres 
    END RECORD
    DEFINE g_dif    CHAR(150)
    
    DEFINE tel_hora_ini1 CHAR(05),
           tel_hora_fin1 CHAR(05),
           tel_dia1      CHAR(01),
           tel_hora_ini2 CHAR(05),
           tel_hora_fin2 CHAR(05),
           tel_dia2      CHAR(01),
           hr_loc_tel1   CHAR(08),
           hr_loc_tel2   CHAR(08),
           v_sql_1       CHAR(50),
           v_criterio    SMALLINT,
           v_edad        SMALLINT

     DEFINE vtipo_solicitud SMALLINT,
            cont_acep       SMALLINT
     DEFINE vind_36m CHAR(1)
     DEFINE vfolio_cert CHAR(20)       --#CUO II CPL1713
     DEFINE gs_idope    SMALLINT       --#CUO II CPL1713
END GLOBALS

--- Oct 2014 CPL1713  
GLOBALS 'AFIB0001.4gl'

MAIN

    DISPLAY " "
    DISPLAY ".1"

    CALL STARTLOG(FGL_GETENV('USER')||'AFIB006.log')
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
    WHERE  modulo_cod = 'afi'

    SELECT *,USER
    INTO   g_afore.*,g_usuario 
    FROM   tab_afore_local

    LET reg_dom.tipo_reg   = '02'
    LET reg_dom.tipo_recep = '01'
    LET reg_dom.cve_recep  =  g_afore.codigo_afore USING '&&&'

    LET    gs_idope        =  2           --# CPL1713  CUO II 2-Traspaso
    LET HOY    = TODAY
    LET HORA   = TIME
    LET num    = 0
    LET num2   = 1

    LET venv_img  = 0
    LET vcont_tel = 0
    LET vcc_tel   = 0
    LET vtam      = 0
    LET vcc_cor   = 0
    LET tot_reenv = 0
    LET vcc_t_c   = 0
    LET ban_tel   = 0
    LET ban_cor   = 0
    LET ban_t_c   = 0
    
    UPDATE afi_ctr_identif
    SET clave_identif = 1
    WHERE n_folio IN (SELECT nsolicitud
                     FROM solicitudafi
                    WHERE nsolicitud   = n_folio
                      AND ididnoficial = 41)
    AND clave_identif = 5;

    LET v_sql_1 = "EXECUTE PROCEDURE fn_edad_multisie(?,?)"
    PREPARE stmt1 FROM v_sql_1
    
END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 4,4 WITH FORM "AFIB0061" ATTRIBUTE(BORDER)

    DISPLAY " AFIB006     GENERACION ARCHIVO CERTIFICACION TRASPASOS                        " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                               < Ctrl-C > Salir                                " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)
    DISPLAY g_seg_modulo.ruta_envio AT 10,10

    INPUT BY NAME g_reg.*
--->captura fecha envio 11052009
        AFTER FIELD fecha_envio

        IF g_reg.fecha_envio IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD fecha_envio
        END IF

        AFTER FIELD generar

        IF g_reg.generar IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD generar
        END IF

        IF g_reg.generar  MATCHES "[Ss]" THEN
            CALL fecha_max()
            CALL rescata_valores()          
            DISPLAY "                                                                               " AT 14,02

            DISPLAY "GRADO CONTROL AFORE        ", vgrado_afo AT 11,02

            DISPLAY "TOTAL SOLIC A SER ENVIADAS ", vsoli_env
            USING ">&&&&&&&&" AT 12,02
            DISPLAY "TOTAL SOLIC REENVIADOS     ", vsoli_env
            USING ">&&&&&&&&" AT 13,02
            DISPLAY "TOTAL SOLIC RECH(BAJA PROM)", rech_prom 
            USING ">&&&&&&&&" AT 14,02 
            DISPLAY "TOTAL SOLIC PROCEDENTES    ", num 
            USING ">&&&&&&&&" AT 15,02

            DISPLAY "TOTAL SOLIC CON ID VAL IMG ", vtot_img
            USING ">&&&&&&&&" AT 12,39
            DISPLAY "TOTAL SOLIC CON TELEFONO   ", vcc_tel
            USING ">&&&&&&&&" AT 13,39
            DISPLAY "TOTAL SOLIC CON CORREO ELEC", vcc_cor
            USING ">&&&&&&&&" AT 14,39
            DISPLAY "TOTAL SOLIC CON TEL Y CORR ", vcc_t_c
            USING ">&&&&&&&&" AT 15,39          

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

    IF rech_prom = 0 THEN
        PROMPT "NOMBRE ARCHIVO PLANO : ", nom_taa CLIPPED,
        " /[Enter] para salir" FOR enter
    ELSE
        PROMPT "NOM ARCHIVO : ", nom_taa CLIPPED,
        " CON ", rech_prom USING "####&",
        " REGS CON PROMOTORES NO ACTIVOS, /[Enter] para salir"
        FOR enter
    END IF

    PROMPT "NOMBRE ARCHIVO PLANO : ", nom_taa CLIPPED,
           " /[Enter] para salir" FOR enter  

END FUNCTION

FUNCTION rescata_valores()
#rv-----------------------

    DEFINE recha            SMALLINT
    DEFINE ind_rechprom     SMALLINT
    DEFINE vmotivo_suspende CHAR(02)
    DEFINE vfecha_suspende  DATE
    DEFINE vfecha_baja,
           val_fecha_prom   DATE

  DEFINE  lr_d         RECORD 
        nss            CHAR(11),
        n_folio        DECIMAL(10,0),
        tipo_sol       SMALLINT,
        proceso        CHAR(02) 
                       END RECORD 

    LET HAY_REGISTROS = FALSE

    SELECT @descripcion
    INTO   vgrado_afo
    FROM   tab_grado_ctr_afo
    WHERE  fecha_fin IS  NULL

    SELECT COUNT(*)
    INTO   vtot_solv3
    FROM   afi_solicitud A
    WHERE  A.status_interno = 20
    AND    A.tipo_solicitud IN (2,15)
    AND    A.frecafor      <= fecha_hasta

    SELECT COUNT(*)
    INTO   HAY_REGISTROS
    FROM   afi_solicitud A,
           afi_domicilio O,
           tab_estado E,
           tab_delegacion D,
           tab_codpos F,
           afi_folio_edocta G           --folio edocta
    WHERE  A.status_interno  = 20 #CON TODOS LOS DOCTOS
    AND    A.tipo_solicitud  IN (2,15) --solicitud de traspaso
    AND    A.frecafor       <= fecha_hasta
    AND    A.n_seguro           = O.nss
    AND    A.n_folio            = O.n_folio
    AND    A.tipo_solicitud     = O.tipo_solicitud
    AND    O.marca_envio        = "X"
    AND    E.estad_cod          = O.estado
    AND    D.estad_cod          = O.estado
    AND    D.deleg_cod          = O.delega
    AND    O.codpos             = F.cpos_cod
    AND    G.nss                = A.n_seguro
    AND    G.n_folio            = A.n_folio
    AND    G.tipo_solicitud     = A.tipo_solicitud
{dexomentar
    IF vtot_solv3 <> HAY_REGISTROS THEN
--->erm 21 Diciembre 2007

       LET g_dif   = g_seg_modulo.ruta_listados CLIPPED,
                     "/",g_usuario,"_DIFERENCIAS_",HOYDIA USING "yyyymmdd" CLIPPED,
                     ".txt" CLIPPED

       START REPORT listado_dif TO g_dif

       DECLARE c_hay CURSOR FOR
         SELECT A.n_folio,
                A.n_seguro,
                A.paterno,
                A.materno,
                A.nombres
         FROM   afi_solicitud A
         WHERE  A.status_interno = 20
         AND    A.tipo_solicitud = 2
         AND    A.frecafor      <= fecha_hasta
         AND    A.n_folio NOT IN (SELECT A.n_folio
                                   FROM   afi_solicitud A,
                                          afi_domicilio O,
                                          tab_estado E,
                                          tab_delegacion D,
                                          tab_codpos F
                                   WHERE  A.status_interno  = 20
                                   AND    A.tipo_solicitud  =  2
                                   AND    A.frecafor       <= fecha_hasta
                                   AND    A.n_seguro        = O.nss
                                   AND    A.n_folio         = O.n_folio
                                   AND    A.tipo_solicitud  = O.tipo_solicitud
                                   AND    O.marca_envio     = "X"
                                   AND    E.estad_cod       = O.estado
                                   AND    D.estad_cod       = O.estado
                                   AND    D.deleg_cod       = O.delega
                                   AND    O.codpos          = F.cpos_cod)
       FOREACH c_hay INTO g_hay.*
          OUTPUT TO REPORT listado_dif(g_hay.*)
       END FOREACH
       FINISH REPORT listado_dif
       ERROR  "Consulte Reporte en: ", g_dif
---<
       PROMPT "Existen diferencias. Sol Val: ", vtot_solv3, 
       " Generar Archivo: ", HAY_REGISTROS, " <ENTER> para salir." FOR enter
       ERROR "Proceso cancelado. Informe a su area de sistemas por favor."
       SLEEP 4
       EXIT PROGRAM
    END IF
  }
    SELECT COUNT(*)
    INTO   tot_reenv    
    FROM   afi_solicitud  A,
    OUTER  (afi_domicilio O,
    OUTER  tab_estado     E,
    OUTER  tab_delegacion D)
    WHERE  A.status_interno     = 20 #CON TODOS LOS DOCTOS
    AND    A.tipo_solicitud     IN (2,15) --solicitud de traspaso
    AND    A.n_seguro           = O.nss
    AND    A.n_folio            = O.n_folio
    AND    A.tipo_solicitud     = O.tipo_solicitud
    AND    O.marca_envio        = "X"
    AND    E.estad_cod          = O.estado
    AND    D.estad_cod          = O.estado
    AND    D.deleg_cod          = O.delega
    AND    A.fecha_envio       IS NOT NULL    

    IF HAY_REGISTROS THEN

        {IF vgrado_afo = "MEDIO" THEN
           LET vdiv_img = (HAY_REGISTROS + 1) / 2
           LET vtot_img = vdiv_img
        END IF}

        SELECT t.*
        INTO   x_lotes.*
        FROM   tab_lote t
        WHERE  t.lotes_cod = 2
        AND    lotes_fecha = HOYDIA

        IF SQLCA.SQLCODE = 0 THEN
            UPDATE tab_lote
            SET    lotes_correlativo = lotes_correlativo + 1
            WHERE  lotes_cod         = 2
            AND    lotes_fecha       = HOYDIA
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
        WHERE  tl.lotes_cod   = 2
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

        LET consecd   = h_corrd

        LET vsoli_env = hay_registros

        IF NOT bnd_proceso THEN
            DISPLAY "                                                                               " AT 14,02
            DISPLAY "GRADO CONTROL AFORE        ", vgrado_afo AT 11,02

            DISPLAY "TOTAL SOLIC A SER ENVIADAS ", vsoli_env
            USING ">&&&&&&&&" AT 12,02
            DISPLAY "TOTAL SOLIC REENVIADOS     ", vsoli_env
            USING ">&&&&&&&&" AT 13,02
            DISPLAY "TOTAL SOLIC RECH(BAJA PROM)", rech_prom
            USING ">&&&&&&&&" AT 14,02
            DISPLAY "TOTAL SOLIC PROCEDENTES    ", num
            USING ">&&&&&&&&" AT 15,02	

            DISPLAY "TOTAL SOLIC CON ID VAL IMG ", vtot_img
            USING ">&&&&&&&&" AT 12,39
            DISPLAY "TOTAL SOLIC CON TELEFONO   ", vcc_tel
            USING ">&&&&&&&&" AT 13,39
            DISPLAY "TOTAL SOLIC CON CORREO ELEC", vcc_cor
            USING ">&&&&&&&&" AT 14,39
            DISPLAY "TOTAL SOLIC CON TEL Y CORR ", vcc_t_c
            USING ">&&&&&&&&" AT 15,39
            
        ELSE
            DISPLAY "                                                                               " AT 14,02
            DISPLAY "GRADO CONTROL AFORE        ", vgrado_afo AT 11,02

            DISPLAY "TOTAL SOLIC A SER ENVIADAS ", vsoli_env
            USING ">&&&&&&&&" AT 12,02
            DISPLAY "TOTAL SOLIC REENVIADOS     ", vsoli_env
            USING ">&&&&&&&&" AT 13,02
            DISPLAY "TOTAL SOLIC RECH(BAJA PROM)", rech_prom
            USING ">&&&&&&&&" AT 14,02
            DISPLAY "TOTAL SOLIC PROCEDENTES    ", num
            USING ">&&&&&&&&" AT 15,02	

            DISPLAY "TOTAL SOLIC CON ID VAL IMG ", vtot_img
            USING ">&&&&&&&&" AT 12,39
            DISPLAY "TOTAL SOLIC CON TELEFONO   ", vcc_tel
            USING ">&&&&&&&&" AT 13,39
            DISPLAY "TOTAL SOLIC CON CORREO ELEC", vcc_cor
            USING ">&&&&&&&&" AT 14,39
            DISPLAY "TOTAL SOLIC CON TEL Y CORR ", vcc_t_c
            USING ">&&&&&&&&" AT 15,39            
        END IF

        LET HAY_REGISTROS       = FALSE

        DECLARE curs_1 CURSOR FOR
        SELECT A.n_seguro          ,
               A.n_unico           ,
               A.n_rfc             ,
               A.paterno           ,
               A.materno           ,
               A.nombres           ,
               A.fena              ,
               A.cod_promotor      ,
               A.sexo              ,
               A.estadon           ,
               A.n_folio           ,
               A.frecafor          ,
               A.nacionalidad      ,
               A.tip_prob          ,
               A.fol_prob          ,
               A.doc_prob          ,
               A.ind_infonavit     ,
               A.cod_error_origen  ,
               --' ',
               F.folio_edo_cta,             --foledocta
               F.cuatrim_emision   ,
               A.cod_afore_ced     ,
               A.documento_2       ,
               A.fecha_envio       ,
               A.femision          ,
               A.fecha_elaboracion ,
               A.documento_3       ,
               ' '                 ,
               ' '                 ,
               O.calle             ,
               O.numero            ,
               O.depto             ,
               O.colonia           ,
               D.deleg_desc        ,
               O.codpos            ,
               E.estad_desc        ,
               O.pais_cod          ,
               A.tipo_solicitud
        FROM   afi_solicitud      A,
               afi_folio_edocta   F,        ---foledocta
               afi_domicilio      O,
               tab_estado         E,
               tab_delegacion     D
        WHERE  A.status_interno  = 20 #CON TODOS LOS DOCTOS
        AND    A.tipo_solicitud  IN (2,15) --solicitud de traspaso
#       AND    A.frecafor       <= fecha_hasta
        AND    A.n_seguro           = O.nss
        AND    A.n_folio            = O.n_folio
        AND    A.tipo_solicitud     = O.tipo_solicitud
        AND    O.marca_envio        = "X"
        AND    E.estad_cod          = O.estado
        AND    D.estad_cod          = O.estado
        AND    D.deleg_cod          = O.delega
        AND    F.nss                = A.n_seguro               --foledocta
        AND    F.n_folio            = A.n_folio
        AND    F.tipo_solicitud     = A.tipo_solicitud
        DISPLAY "PROCESANDO INFORMACION "

        LET comm = g_seg_modulo.ruta_envio CLIPPED,
                   "/E",HOYDIA USING "yyyymmdd" CLIPPED,
                   consec CLIPPED, ".TAA" CLIPPED

        DISPLAY "Archivo : ", comm CLIPPED

        LET nom_taa = "E",HOYDIA USING "yyyymmdd" CLIPPED,
                      consec CLIPPED, ".TAA" CLIPPED

        LET G_LISTA = g_seg_modulo.ruta_listados CLIPPED,
                      "/",g_usuario CLIPPED,
                      ".ARCHIVO_TRASP_CERT." CLIPPED,
                      HOY USING "dd-mm-yy","_",consec CLIPPED,
                      "_",HORA CLIPPED

        LET G_LISTA1 = g_seg_modulo.ruta_listados CLIPPED,
                      "/",g_usuario CLIPPED,
                      ".TRASP_CERTRECH_PRO." CLIPPED,
                      HOY USING "ddmmyy","_",consec CLIPPED

        LET g_dom   = g_seg_modulo.ruta_envio CLIPPED,
                      "/T",HOYDIA USING "yyyymmdd" CLIPPED,
                      consecd CLIPPED, ".DOM" CLIPPED

        START REPORT listado_3 TO G_LISTA1
        START REPORT listado_2 TO G_LISTA
        START REPORT listado_d TO g_dom
        START REPORT listado TO comm

        FOREACH curs_1 INTO w_aux.*,
                            reg_dom.calle,
                            reg_dom.no_ext,
                            reg_dom.no_int,
                            reg_dom.colonia,
                            reg_dom.delegacion,
                            reg_dom.cp,
                            reg_dom.ent_fed,
                            vpais,
                            vtipo_solicitud

        LET HAY_REGISTROS = TRUE
        LET k_aux.folio_solicitud = w_aux.n_folio

        {SELECT "X"
        FROM   tab_grado_ctr_afo
        WHERE  fecha_fin   IS NULL
        AND    descripcion = 'BAJO'
        IF STATUS <> NOTFOUND THEN
           LET w_aux.documento_3 = 1
        END IF

        SELECT "X"
        FROM   tab_grado_ctr_afo
        WHERE  fecha_fin   IS NULL
        AND    descripcion = 'ALTO'
        IF STATUS <> NOTFOUND THEN
           LET w_aux.documento_3 = ' '
        END IF}
        
        IF w_aux.id_reenvio <> '1' AND
	   w_aux.id_reenvio <> '2' THEN
	   LET w_aux.id_reenvio = ' '
	END IF

        IF w_aux.materno IS NULL OR
           w_aux.materno MATCHES '  *' THEN
            LET w_aux.materno = "N/A"
        END IF 

        --LET w_aux.folio_edo_cta = '        '     --foledocta

        LET reg_dom.nss_afore = w_aux.n_seguro

        SELECT NVL(A.status,3), motivo_suspende, fecha_suspende,fecha_baja
        INTO   recha, vmotivo_suspende, vfecha_suspende,vfecha_baja
        FROM   pro_mae_promotor A
        WHERE  A.cod_promotor = w_aux.cod_promotor

        IF recha = 2 OR
           recha = 3 THEN

            LET ind_rechprom = 1

            IF vfecha_suspende IS NULL THEN
               LET val_fecha_prom = vfecha_baja
            ELSE
               LET val_fecha_prom = vfecha_suspende
            END IF
 
            #IF recha                    = 2               AND
            --IF w_aux.fecha_elaboracion  < vfecha_suspende AND 
            IF w_aux.fecha_elaboracion  <= val_fecha_prom AND
               (vmotivo_suspende        = '2C'            OR
                vmotivo_suspende        = '2E'            OR
                vmotivo_suspende        = '2G'            OR
                vmotivo_suspende        = '2H'            OR
                vmotivo_suspende        = '2I'            OR
                vmotivo_suspende        = '2J'            OR
                vmotivo_suspende        = '2L'            OR
                vmotivo_suspende        = '2M'            OR
                vmotivo_suspende        = '2T'            OR
                vmotivo_suspende        = '3C'            OR
                vmotivo_suspende        = '3D'            OR
                vmotivo_suspende        = '3E'            OR
                vmotivo_suspende        = '3T'            OR

                vmotivo_suspende        = '6C')           THEN
               LET ind_rechprom = 0
            END IF
               
            IF ind_rechprom = 1 THEN 
               LET rech_prom = rech_prom + 1

               OUTPUT TO REPORT listado_3(w_aux.n_seguro,
                                          w_aux.n_unico,
                                          k_aux.folio_solicitud,
                                          w_aux.paterno,
                                          w_aux.materno,
                                          w_aux.nombres,
                                          w_aux.cod_promotor,
			         	  recha                 ,
                                          vmotivo_suspende      ,
                                          --vfecha_suspende       ,
                                          val_fecha_prom          ,
                                          w_aux.fecha_elaboracion
                                          )
               UPDATE afi_solicitud
               SET    status_interno = 21
               WHERE  n_seguro       = w_aux.n_seguro
               AND    n_folio        = w_aux.n_folio
               AND    tipo_solicitud IN (2,15)


               UPDATE solicitudafi
               SET    stamcertificafi = '21-Rechazo x promotor invalido'
               WHERE  nsolicitud      = w_aux.n_folio

               #DISPLAY "TOTAL SOLIC RECH(BAJA PROM)", rech_prom 
               #USING ">&&&&&&&&" AT 14,02 

                CALL inserta_afi_ctr(21)

                CONTINUE FOREACH
            END IF
        END IF

        {CASE vgrado_afo
          WHEN "ALTO"  LET w_aux.documento_3 = ' '
            LET vtot_img = 0
          WHEN "MEDIO"
            LET venv_img = venv_img + 1
            IF venv_img <= vtot_img THEN
               LET w_aux.documento_3 = 1
            ELSE
               LET w_aux.documento_3 = ' '
            END IF
          WHEN "BAJO"  LET w_aux.documento_3 = 1
            LET vtot_img = vtot_img + 1
        END CASE

        UPDATE afi_solicitud SET documento_3 = w_aux.documento_3
        WHERE  afi_solicitud.n_seguro        = w_aux.n_seguro
        AND    afi_solicitud.n_folio         = k_aux.folio_solicitud}

        {IF w_aux.documento_3 = 1 THEN
            LET vtot_img = vtot_img + 1
        END IF}

        IF w_aux.id_reenvio <> '1' AND
           w_aux.id_reenvio <> '2' THEN
           LET w_aux.id_reenvio = ' '
        END IF
          
        IF w_aux.id_reenvio = '0' THEN
           LET w_aux.id_reenvio = ' '
        END IF

        IF (w_aux.cod_afore_ced IS NULL)        OR
           (w_aux.cod_afore_ced MATCHES "[ *]") THEN
           LET w_aux.cod_afore_ced = 568
        END IF
        
        SELECT a.folio_saftv[11,20], a.cve_rend_neto
          INTO w_aux.folio_saftv, w_aux.cve_rend_neto
          FROM afi_folio_saftv a
         WHERE a.nss            = w_aux.n_seguro
           AND a.n_folio        = w_aux.n_folio
           AND a.tipo_solicitud IN (2,15)
        IF STATUS = NOTFOUND THEN
           {LET w_aux.folio_saftv   = NULL
           LET w_aux.cve_rend_neto = NULL}
           
           CALL calcula_edad_sie()
           
           LET w_aux.folio_saftv   = w_aux.n_folio
           LET w_aux.cve_rend_neto = v_criterio
           
        END IF

         -->Trab con mas de 2 TAA en 36 M
        INITIALIZE vind_36m TO NULL
        SELECT 'X'
        FROM afi_ctr_mas_dos_tra
        WHERE nss = w_aux.n_seguro
        AND    n_folio         = w_aux.n_folio
        AND    tipo_solicitud = 2

        IF SQLCA.SQLCODE <> NOTFOUND THEN
           LET vind_36m = "1"
        ELSE
           LET vind_36m = " "
        END IF

        LET cont_acep = cont_acep + 1

        IF cont_acep = 9998 THEN
           SELECT * 
           INTO   x_lotes.* 
           FROM   tab_lote
           WHERE  lotes_cod   = 2
           AND    lotes_fecha = HOYDIA

           IF SQLCA.SQLCODE = 0 THEN

               SELECT lotes_correlativo
               INTO   h_corr 
               FROM   tab_lote
               WHERE  lotes_cod   = 2
               AND    lotes_fecha = HOYDIA

               LET consec = h_corr

               UPDATE tab_lote 
               SET    lotes_correlativo = lotes_correlativo + 1
               WHERE  lotes_cod         = 2
               AND    lotes_fecha       = HOYDIA
           END IF

           FINISH REPORT listado

           LET cont_acep = 0
           --LET consec = h_corr
           LET comm = g_seg_modulo.ruta_envio CLIPPED,
                      "/E",HOYDIA USING "yyyymmdd" CLIPPED,
                      consec CLIPPED, ".TAA" CLIPPED
                                
           START REPORT listado   TO comm
        END IF

        --CPL1713
        INITIALIZE vfolio_cert TO NULL
        SELECT folio_cert_trasp
        INTO   vfolio_cert
        FROM   afi_solicitud_adicionales
        WHERE  nss             = w_aux.n_seguro
        AND    n_folio         = w_aux.n_folio
        AND    tipo_solicitud  IN (2, 15)

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
                                 w_aux.cuatrim_emision ,        ---foledocta
                                 w_aux.cod_afore_ced   ,
                                 w_aux.id_reenvio      ,
                                 w_aux.femision        ,
                                 w_aux.fecha_elaboracion,
                                 w_aux.documento_3      ,
                                 w_aux.folio_saftv      ,
                                 w_aux.cve_rend_neto    ,
                                 vind_36m               ,
                                 vfolio_cert            )

        SELECT COUNT(*)
          INTO vtotal_tel
          FROM afi_telefono t
         WHERE t.nss            = w_aux.n_seguro
           AND t.n_folio        = w_aux.n_folio
           AND t.tipo_solicitud IN (2,15)
           AND t.tel_cod NOT IN (7)
        IF vtotal_tel = 0 THEN
           LET vtel1         = NULL
           LET vind_tel1     = NULL
           LET vexten1       = NULL
           LET vtel2         = NULL
           LET vind_tel2     = NULL
           LET vexten2       = NULL
           
           LET tel_hora_ini1 = NULL
	   LET tel_hora_fin1 = NULL
	   LET tel_dia1      = NULL
	   LET tel_hora_ini2 = NULL
	   LET tel_hora_fin2 = NULL
           LET tel_dia2      = NULL
        ELSE
           DECLARE curs_2 CURSOR FOR
           SELECT t.*
             FROM afi_telefono t
            WHERE t.nss            = w_aux.n_seguro
              AND t.n_folio        = w_aux.n_folio
              AND t.tipo_solicitud IN (2,15)
              AND t.tel_cod NOT IN (7)
            ORDER BY t.tel_cod
           FOREACH curs_2 INTO reg_tel.*
           
             IF vtotal_tel = 1 THEN
                IF reg_tel.cve_lada[1] = 0 THEN
                   LET reg_tel.cve_lada = reg_tel.cve_lada[2,3]
                END IF
                LET vcc_tel       = vcc_tel + 1
                LET vtel1         = reg_tel.cve_lada  CLIPPED, 
                                    reg_tel.telefono  CLIPPED
                LET vexten1       = reg_tel.extension CLIPPED
                LET tel_hora_ini1 = reg_tel.tel_hora_ini
		LET tel_hora_fin1 = reg_tel.tel_hora_fin
		LET tel_dia1      = reg_tel.tel_dia
                LET ban_tel       = 1                
                --EXIT FOREACH
--->erm 25 ayo 2007
        #VALIDA CELULAR 1
                IF reg_tel.tel_cod = 4 THEN
                   IF reg_tel.cve_lada[1]  = 0 THEN
                      LET reg_tel.cve_lada = reg_tel.cve_lada[2,3]
                   END IF
                   --LET vcc_tel = vcc_tel + 1
                   LET vtel1         = reg_tel.cve_lada CLIPPED, 
                                       reg_tel.telefono  CLIPPED
                   LET tel_hora_ini1 = reg_tel.tel_hora_ini
		   LET tel_hora_fin1 = reg_tel.tel_hora_fin
                   LET tel_dia1      = reg_tel.tel_dia

                   LET vtam = LENGTH(vtel1 CLIPPED)

                   IF vtam = 13 THEN
                      IF (vtel1[4,5] = '55') THEN
                         LET vind_tel1 = '044'
                         LET vtel1     = vtel1[4,vtam]
                      ELSE
                         LET vind_tel1 = '045'
                         LET vtel1     = vtel1[4,vtam]
                      END IF
                   END IF
                   IF vtam = 10 THEN
                      IF (vtel1[1,2] = '55') THEN
                         LET vind_tel1 = '044'
                      ELSE
                         LET vind_tel1 = '045'
                      END IF
                   END IF
                ELSE
                   LET vind_tel1 = NULL
                   #LET vexten1   = NULL
                END IF
                EXIT FOREACH
---<
             ELSE
                LET vcont_tel = vcont_tel + 1

                IF vcont_tel = 1 THEN
                   IF reg_tel.cve_lada[1] = 0 THEN
                      LET reg_tel.cve_lada = reg_tel.cve_lada[2,3]
                   END IF
                   LET vtel1         = reg_tel.cve_lada  CLIPPED,
                                       reg_tel.telefono  CLIPPED
                   LET tel_hora_ini1 = reg_tel.tel_hora_ini
		   LET tel_hora_fin1 = reg_tel.tel_hora_fin
                   LET tel_dia1      = reg_tel.tel_dia                                 
                   LET vexten1       = reg_tel.extension CLIPPED
                   LET ban_tel       = 1                   
                   LET vcc_tel       = vcc_tel + 1
                ELSE
                   IF reg_tel.cve_lada[1] = 0 THEN
                      LET reg_tel.cve_lada = reg_tel.cve_lada[2,3]
                   END IF
                   LET vtel2         = reg_tel.cve_lada  CLIPPED,
                                       reg_tel.telefono  CLIPPED
                   LET vexten2       = reg_tel.extension CLIPPED
                   LET tel_hora_ini2 = reg_tel.tel_hora_ini
		   LET tel_hora_fin2 = reg_tel.tel_hora_fin
		   LET tel_dia2      = reg_tel.tel_dia

                   IF (vtel1 <> vtel2)          OR 
                      (vtel1 NOT MATCHES vtel2) THEN
                      LET vcont_tel = 0
                      
                      #VALIDA CELULAR 2
                      IF reg_tel.tel_cod = 4 THEN
                         IF reg_tel.cve_lada[1] = 0 THEN
                            LET reg_tel.cve_lada = reg_tel.cve_lada[2,3]
                         END IF
                         --LET vcc_tel = vcc_tel + 1
                         LET vtel2         = reg_tel.cve_lada CLIPPED, 
                                             reg_tel.telefono CLIPPED
                         LET tel_hora_ini2 = reg_tel.tel_hora_ini
			 LET tel_hora_fin2 = reg_tel.tel_hora_fin
			 LET tel_dia2      = reg_tel.tel_dia
                   
                         LET vtam = LENGTH(vtel2 CLIPPED)
                         IF vtam = 13 THEN
                            IF (vtel2[4,5] = '55') THEN
                               LET vind_tel2 = '044'
                               LET vtel2     = vtel2[4,vtam]
                            ELSE
                               LET vind_tel2 = '045'
                               LET vtel2     = vtel2[4,vtam]
                            END IF
                         END IF
                         IF vtam = 10 THEN
                            IF (vtel2[1,2] = '55') THEN
                               LET vind_tel2 = '044'
                            ELSE
                               LET vind_tel2 = '045'
                            END IF
                         END IF
                      ELSE
                         LET vind_tel2 = NULL
                      END IF

                      INITIALIZE reg_tel.tel_cod TO NULL          ---erm 22 Mayo 2007
                      # LET ban_tel = 1
                      EXIT FOREACH
                   ELSE
                      LET vtel2     = NULL
                      LET vind_tel2 = NULL
                   END IF
                END IF
             END IF
           END FOREACH
        END IF
{#comentado erm 22 Mayo 2007
        #VALIDA CELULAR 1
        IF reg_tel.tel_cod = 4 THEN
           LET vtam = LENGTH(vtel1 CLIPPED)
           IF vtam = 13 THEN
              IF (vtel1[4,5] = '55') THEN
                 LET vind_tel1 = '044'
                 LET vtel1     = vtel1[4,vtam]
              ELSE
                 LET vind_tel1 = '045'
                 LET vtel1     = vtel1[4,vtam]
              END IF
           END IF
           IF vtam = 10 THEN
              IF (vtel1[1,2] = '55') THEN
                 LET vind_tel1 = '044'
              ELSE
                 LET vind_tel1 = '045'
              END IF
           END IF
        ELSE
           LET vind_tel1 = NULL
        END IF
} #cierra comentario erm 22 Mayo 2007
        #OBTIENE CORREO ELECTRONICO
        LET vcorreo_e = NULL
    
        SELECT MAX(factualiza)
          INTO mf_correo
          FROM afi_correo_elect a
         WHERE a.nss            = w_aux.n_seguro
           AND a.n_folio        = k_aux.folio_solicitud
           AND a.tipo_solicitud = 2
           AND a.marca_envio    = 'X'
           AND a.cod_correo_e   = 1

        SELECT MAX(rowid)
          INTO mrowid_corr
          FROM afi_correo_elect a
         WHERE a.nss            = w_aux.n_seguro
           AND a.n_folio        = k_aux.folio_solicitud
           AND a.tipo_solicitud = 2
           AND a.marca_envio    = 'X'
           AND a.cod_correo_e   = 1
           AND a.factualiza     = mf_correo

        SELECT a.correo_elect
          INTO vcorreo_e
          FROM afi_correo_elect a
         WHERE a.nss            = w_aux.n_seguro
           AND a.n_folio        = k_aux.folio_solicitud
           AND a.tipo_solicitud IN (2,15)
           AND a.marca_envio    = 'X'
           AND a.cod_correo_e   = 1
           AND a.factualiza     = mf_correo
           AND a.rowid          = mrowid_corr

       IF vcorreo_e IS NOT NULL AND 
           --ban_tel = 0           THEN
           ban_tel = 1           THEN              ---erm 22 mayo 2007
           LET vcc_cor = vcc_cor + 1
           LET ban_cor = 1

           {UPDATE afi_correo_elect  
              SET fenvio_correo = today
            WHERE nss            = w_aux.n_seguro
              AND n_folio        = k_aux.folio_solicitud
              AND tipo_solicitud = 2
              AND marca_envio    = 'X'
              AND cod_correo_e   = 1

           UPDATE afi_correo_e_his  
              SET fenvio_correo = today
            WHERE nss            = w_aux.n_seguro
              AND n_folio        = k_aux.folio_solicitud
              AND tipo_solicitud = 2
              AND marca_envio    = 'X'
              AND cod_correo_e   = 1}
        END IF

--->erm 22 Mayo 2007
        IF vcorreo_e IS NOT NULL AND 
           ban_tel = 0           THEN
           LET vcc_cor = vcc_cor + 1
           LET ban_cor = 0
        END IF
---<

        IF ban_tel = 1 AND
           ban_cor = 1 THEN
           LET vcc_t_c = vcc_t_c  + 1
        END IF           
     
        #VALIDA CODIGO PAIS
        LET vf = 0
     
        FOR vf = 1 TO 3    
            IF (vpais[vf,vf] = 1 OR vpais[vf,vf] = 2) OR
               (vpais[vf,vf] = 3 OR vpais[vf,vf] = 4) OR
               (vpais[vf,vf] = 5 OR vpais[vf,vf] = 6) OR
               (vpais[vf,vf] = 7 OR vpais[vf,vf] = 8) OR
               (vpais[vf,vf] = 9 OR vpais[vf,vf] = 0) THEN
               LET vpais = 'MEX'
            END IF
        END FOR
     
        IF vpais IS NULL THEN
           LET vpais = 'MEX'
        END IF
     
        #OBTIENE IDENTIFICACION
        SELECT MAX(fecha)
          INTO mfec_ident
          FROM afi_ctr_identif a
         WHERE a.n_seguro       = w_aux.n_seguro
           AND a.n_folio        = k_aux.folio_solicitud
           AND a.tipo_solicitud IN (2,15)
           AND a.clave_identif  = 1
     
        SELECT MAX(rowid)
          INTO mrowid_ide
          FROM afi_ctr_identif a
         WHERE a.n_seguro       = w_aux.n_seguro
           AND a.n_folio        = k_aux.folio_solicitud
           AND a.tipo_solicitud IN (2,15)
           AND a.clave_identif  = 1
           AND a.fecha          = mfec_ident
     
        SELECT a.identifica, a.ocr_ife
          INTO vfol_ife, vocr_ife
          FROM afi_ctr_identif a
         WHERE a.n_seguro       = w_aux.n_seguro
           AND a.n_folio        = k_aux.folio_solicitud
           AND a.tipo_solicitud IN (2,15)
           AND a.clave_identif  = 1
           AND a.fecha          = mfec_ident
           AND a.rowid          = mrowid_ide

        SELECT ocrife
          INTO vocr_ife
          FROM solicitudafi
         WHERE nsolicitud     = k_aux.folio_solicitud
           AND idtposolicitud = 27

        UPDATE afi_ctr_identif 
           SET ocr_ife        = vocr_ife
         WHERE n_seguro       = w_aux.n_seguro
           AND n_folio        = k_aux.folio_solicitud
           AND tipo_solicitud IN (2,15)
           AND clave_identif  = 1
           AND fecha          = mfec_ident
           AND rowid          = mrowid_ide

----erm 23 Mayo 2007
        SELECT LPAD(TRIM(a.identifica),13) ,LPAD(TRIM(a.ocr_ife),13)
          INTO vfol_ife, vocr_ife
          FROM afi_ctr_identif a
         WHERE a.n_seguro       = w_aux.n_seguro
           AND a.n_folio        = k_aux.folio_solicitud
           AND a.tipo_solicitud IN (2,15)
           AND a.clave_identif  = 1
           AND a.fecha          = mfec_ident
           AND a.rowid          = mrowid_ide
        IF STATUS = NOTFOUND THEN
           LET vfol_ife = "0000000000000"
           LET vocr_ife = "0000000000000"
        {ELSE
           IF (vfol_ife IS NULL)        OR  
              (vfol_ife MATCHES "[ *]") THEN
              LET vfol_ife = "0000000000000"
           END IF

           IF vfol_ife like  "%CREDENCIAL%" THEN
              LET vfol_ife = "0000000000000"
           END IF
   
           IF (vocr_ife IS NULL)        OR  
              (vocr_ife MATCHES "[ *]") THEN
              LET vocr_ife = "0000000000000"
           END IF}
        END IF

        IF tel_hora_ini1 IS NOT NULL THEN
	   CALL val_puntos(tel_hora_ini1, tel_hora_fin1) RETURNING hr_loc_tel1
	ELSE
	   LET hr_loc_tel1 = NULL
	END IF
	
	IF tel_hora_ini2 IS NOT NULL THEN
	   CALL val_puntos(tel_hora_ini2, tel_hora_fin2) RETURNING hr_loc_tel2 
	ELSE
	   LET hr_loc_tel2 = NULL
	END IF
{
        --OUTPUT TO REPORT listado_d(reg_dom.*,
        --                           vind_tel1, 
        --                           vtel1, 
        --                           vexten1,
        --                           vind_tel2,
        --                           vtel2,
        --                           vexten2,
        --                           vcorreo_e,
        --                           vpais,
        --                           vfol_ife,
        --                           vocr_ife,
        --                           hr_loc_tel1,
        --                           tel_dia1,
        --                           hr_loc_tel2,
        --                           tel_dia2
        --                          )
}
        LET    lr_d.nss              =  w_aux.n_seguro
        LET    lr_d.n_folio          =  k_aux.folio_solicitud 
        LET    lr_d.tipo_sol         =  vtipo_solicitud
        LET    lr_d.proceso          =  '01'
        OUTPUT TO REPORT listado_d(lr_d.*)

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
                                   w_aux.cuatrim_emision ,      --foledocta
                                   w_aux.cod_afore_ced   ,
                                   w_aux.documento_3
                                  )


        --->foledocta
        UPDATE afi_folio_edocta
        SET    fecha_envio     = TODAY
        WHERE  nss             = w_aux.n_seguro
        AND    n_folio         = w_aux.n_folio
        AND    tipo_solicitud IN (2,15)
        ---<foledocta

        LET num  = num + 1
        LET num2 = num2 + 1
        
        LET vtel1     = NULL 
	LET vind_tel1 = NULL
	LET vexten1   = NULL
	LET vtel2     = NULL
	LET vind_tel2 = NULL
	LET vexten2   = NULL
	LET vcont_tel = 0
        LET vfol_ife  = NULL
        LET vocr_ife  = NULL
        LET ban_tel   = 0            ---erm 22 Mayo 2007

        {DISPLAY "TOTAL SOLIC PROCEDENTES    ", num 
        USING ">&&&&&&&&" AT 15,02}

        {IF recha <> 2 AND
           recha <> 3 THEN}
           UPDATE afi_solicitud 
           SET    afi_solicitud.status_interno     = 30,
                  afi_solicitud.lote               = h_corr,
                  afi_solicitud.fecha_envio        = HOYDIA
           WHERE  afi_solicitud.status_interno     = 20
           AND    afi_solicitud.n_seguro           = w_aux.n_seguro
           AND    afi_solicitud.n_folio            = w_aux.n_folio
           AND    afi_solicitud.tipo_solicitud     = vtipo_solicitud
           AND    afi_solicitud.frecafor          <= fecha_hasta        

           CALL inserta_afi_ctr(30)
        #END IF	

        #CALL inserta_afi_ctr(30)

        END FOREACH

        FINISH REPORT listado
        FINISH REPORT listado_d
        FINISH REPORT listado_2
        FINISH REPORT listado_3

        CALL limpia_nulos()

    ELSE
        IF bnd_proceso THEN
            DISPLAY "Program stopped, NO HAY SOLICITUDES A ENVIAR"
        END IF
    END IF

END FUNCTION

REPORT listado(w_aux,lind_36m,vfolio_cert)
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
        folio_solicitud     CHAR(10) ,
        sexo                LIKE afi_solicitud.sexo         ,
        estadon             LIKE afi_solicitud.estadon      ,
        frecafor            LIKE afi_solicitud.frecafor     ,
        nacionalidad        LIKE afi_solicitud.nacionalidad ,
        tip_prob            LIKE afi_solicitud.tip_prob     ,
        fol_prob            LIKE afi_solicitud.fol_prob     ,
        doc_prob            LIKE afi_solicitud.doc_prob     ,
        ind_infonavit       LIKE afi_solicitud.ind_infonavit,
        cod_error_ori       SMALLINT ,
        --folio_edo_cta       CHAR(8)  ,      --foledocta
        folio_edo_cta       CHAR(20)  ,
        cuatrim_emision     DECIMAL(5,0),
        cod_afore_ced       SMALLINT,
        id_reenvio          CHAR(1),
        femision            DATE ,
        fecha_elaboracion   DATE,
        documento_3         CHAR(1),
        folio_saftv         CHAR(10),
        cve_rend_neto       CHAR(1)
    END RECORD

    DEFINE lind_36m CHAR(1)
    DEFINE vfolio_cert char(20)            --CPL1713

    DEFINE origen_tras      CHAR(02)       --taa x curp

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

    DEFINE
        vfecha_mes  CHAR(02),
        vfecha_dia  CHAR(02),
        vfecha_anio CHAR(04),
        vfecha_comp CHAR(10)

    OUTPUT
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0
        PAGE LENGTH 66

    FORMAT
    FIRST PAGE HEADER
{        LET hoy_env = YEAR(HOYDIA) USING "&&&&",
                      MONTH(HOYDIA)USING "&&"  ,
                      DAY(HOYDIA)  USING "&&"}

        LET hoy_env = YEAR(g_reg.fecha_envio) USING "&&&&",
                      MONTH(g_reg.fecha_envio)USING "&&"  ,
                      DAY(g_reg.fecha_envio)  USING "&&"

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
                         --698 SPACES             --foledocta
                         368 SPACES               --taa x curp

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
        LET num10 = num2                   USING "&&&&&&&&&&"

        CASE w_aux.ind_infonavit
            WHEN 'S'  LET var = 1
            WHEN 'N'  LET var = 2
            WHEN '0'  LET var = 2
            WHEN '1'  LET var = 1
            WHEN '2'  LET var = 2
            OTHERWISE LET var = 2
        END CASE

        IF w_aux.tip_prob <> 5 THEN
            LET w_aux.n_unico = "                  "
        END IF

        #LET w_aux.femision = w_aux.femision + 30

        LET vfecha_anio    = YEAR(w_aux.fecha_elaboracion)

        IF DAY(w_aux.fecha_elaboracion) <= 14 THEN
           LET vfecha_mes = MONTH(w_aux.fecha_elaboracion)
        ELSE
           LET vfecha_mes = MONTH(w_aux.fecha_elaboracion)
           IF vfecha_mes = 12 THEN
              LET vfecha_mes = '01'
              LET vfecha_anio = YEAR(w_aux.fecha_elaboracion) + 1
           ELSE
              LET vfecha_mes = MONTH(w_aux.fecha_elaboracion) + 1
           END IF
        END IF

        LET vfecha_mes     = vfecha_mes USING "&&"
        LET vfecha_dia     = 14

        LET vfecha_comp    = vfecha_mes, "/", vfecha_dia, "/",
                             vfecha_anio

        LET w_aux.femision = vfecha_comp

        IF LENGTH(w_aux.folio_edo_cta) <> 20 THEN
           SELECT LPAD ( trim(folio_edo_cta), 20,' ')
           INTO   w_aux.folio_edo_cta
           FROM   afi_folio_edocta
           WHERE  nss      = w_aux.n_seguro
           AND    n_folio  = w_aux.folio_solicitud
           AND    tipo_solicitud IN (2,15)
           AND    fecha_envio = TODAY
        END IF


        -- CPL1713 
        IF  LENGTH(vfolio_cert)  <>  20  THEN
            INITIALIZE vfolio_cert TO NULL 
            SELECT  LPAD ( trim(folio_cert_trasp), 20)
              INTO  vfolio_cert
              FROM  afi_solicitud_adicionales
             WHERE  nss             =  w_aux.n_seguro
               AND  n_folio         =  w_aux.folio_solicitud
               AND  tipo_solicitud IN (2,15)
        END IF        

-->taa x curp
        IF vtipo_solicitud = 2 THEN           #traspaso normal 
           LET origen_tras = '01'
        END IF
        IF vtipo_solicitud = 15 THEN
           LET origen_tras = '71'
           LET w_aux.n_seguro = '           '
        END IF
--<

        PRINT
            COLUMN 1,"02"                   ,            #TIPO DE REGISTRO
                     num10                  ,            #CONTADOR DE  SERVICIO 
                     "04"                   ,            #CLAVE DE LA OPERACION
                     w_aux.n_seguro         ,            #NSS
                     w_aux.n_unico          ,            #CURP
                     --13 SPACES              ,
                     origen_tras USING "&&" ,            #Origen / tipo del traspaso             --taa x curp
                     w_aux.paterno          ,            #APELLIDO PATERNO DEL TRABAJADOR
                     w_aux.materno          ,            #APELLIDO MATERNO DEL TRABAJADOR
                     w_aux.nombres          ,            #NOMBRES DEL TRABAJADOR
                     --8 SPACES               ,
                     w_aux.cod_promotor     USING "&&&&&&&&&&" ,     --taa x curp #CLAVE DE AGENTE PROMOTOR                              
                     a,m,d                  ,          #fecha firma (elab) --taa x curp #FECHA DE FIRMA DE LA SOLICITUD POR EL TRABAJADOR
                     k_aux.folio_solicitud  USING "&&&&&&&&&&" ,   #FOLIO DE LA SOLICITUD
                     w_aux.cod_afore_ced    USING "&&&"        ,   #ADMINISTRADORA TRANSFERENTE
                     --35 SPACES              ,                        --taa x curp
                     --w_aux.folio_saftv      USING "&&&&&&&&&&" ,   #FILLER
                     --#w_aux.cod_afore_ced    USING "&&&"        ,
                     --w_aux.id_reenvio                          ,
                     --45 SPACES                                 ,     --taa x curp
                     108 SPACES                                 ,       --taa x curp
                     --w_aux.cve_rend_neto    USING "#"          ,
                     --211 SPACES                                ,     --taa x curp
                     --63 SPACES                                ,        --taa x curp
                     --w_aux.documento_3      USING "&"          ,
                     --45 SPACES                                 ,     --taa x curp
                     w_aux.femision         USING "YYYYMMDD"   ,      #FECHA FINAL DE VIGENCIA DE DOCUMENTO  DE  RENDIMIENTOS
                     w_aux.folio_edo_cta                       ,      #FOLIO DE ESTADO DE CUENTA ADMINISTRADORA TRANSFERENTE
                     1 SPACES                                  ,     --36m  #IDETIFICADOR DE BONO ISSSTE
    --               lind_36m                                  ,     --36m   Se inhibe CPL1713
    --               66 SPACES                                               Aumenta en 1 por lind_36m
                     vfolio_cert                               , #FOLIO PARA TRASPASO
                     47 SPACES 

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
                          --699 SPACES                      ---foledocta
                          369 SPACES                        --taa x curp

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

REPORT listado_d(lr_d)
--- REPORT listado_d(reg_dom, lind_tel1, ltel1, lexten1, lind_tel2, ltel2, lexten2,
---                  lcorreo_e, lpais, lfol_ife, locr_ife, lhr_loc_tel1, ltel_dia1,
---                  lhr_loc_tel2, ltel_dia2)
#------------------------
  DEFINE  lr_d         RECORD 
        nss            CHAR(11),
        n_folio        DECIMAL(10,0),
        tipo_sol       SMALLINT,
        proceso        CHAR(02) 
                       END RECORD 

--    DEFINE reg_dom RECORD
--        tipo_reg    CHAR(2),
--        cont_serv   DECIMAL(10,0),
--        tipo_recep  CHAR(2),
--        cve_recep   CHAR(3),
--        nss_afore   CHAR(11),
--        calle       CHAR(65),
--        no_ext      CHAR(15),
--        no_int      CHAR(15),
--        colonia     CHAR(65),
--        delegacion  CHAR(65),
--        cp          CHAR(5),
--        ent_fed     CHAR(65)
--    END RECORD

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

    OUTPUT
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0
        PAGE LENGTH 2

    FORMAT
    FIRST PAGE HEADER
{        LET hoy_env = YEAR(HOYDIA) USING "&&&&",
                      MONTH(HOYDIA)USING "&&"  ,
                      DAY(HOYDIA)  USING "&&"}

        LET hoy_env = YEAR(g_reg.fecha_envio) USING "&&&&",
                      MONTH(g_reg.fecha_envio)USING "&&"  ,
                      DAY(g_reg.fecha_envio)  USING "&&"

        PRINT COLUMN 001,"01",
                         "01",
                         "33",
                         "01",
                         reg_dom.cve_recep USING "&&&",
                         "03",
                         "001",
       --                "09",
                         hoy_env,
                         h_corrd USING "&&&",  #CORRELATIVO DE ENVIO
       --                g_opcion USING "&&",  #MEDIO POR DONDE SE ENVIA ARCHIVO
       --                698 SPACES
                         473 SPACES

    ON EVERY ROW

        LET dia1   = DAY(TODAY)   USING"&&"
        LET mes1   = MONTH(TODAY) USING"&&"
        LET ano1   = YEAR(TODAY)  USING "&&&&"
        LET num10d = num2         USING "&&&&&&&&&&"

        {IF lfol_ife = "0000000000000" AND 
           locr_ife = "0000000000000" THEN
        ELSE    
           LET lfol_ife = lfol_ife USING "#############"
           LET locr_ife = locr_ife USING "#############"
        END IF}

        --- CPL1713 no genera un registro, genera n regs, 02 datos particulares (1)
        ---                                               03 domicilio laboral  (n)
        ---                                               04 referencias        (n)
        ---                                               05 beneficiarios      (n)
        ---PRINT
        ---    COLUMN 1,reg_dom.tipo_reg       ,
        ---             num10d                 ,
        ---             reg_dom.tipo_recep     ,
        ---             reg_dom.cve_recep      ,
        ---             reg_dom.nss_afore      ,
        ---             reg_dom.calle          ,
        ---             reg_dom.no_ext         ,
        ---             reg_dom.no_int         ,
        ---             reg_dom.colonia        ,
        ---             reg_dom.delegacion     ,
        ---             reg_dom.cp             ,
        ---             reg_dom.ent_fed        ,
        ---             17 SPACES              ,
        ---             lind_tel1              ,
        ---             ltel1 USING "&&&&&&&&&&",
        ---		lexten1                ,
        ---		lind_tel2              ,
        ---		ltel2 USING "&&&&&&&&&&",
        ---		lexten2                ,
        ---		lcorreo_e              ,
        ---		lpais                  ,
{		     10 SPACES              ,          ---circ unica
		     lfol_ife               ,
		     locr_ife               ,
		     lhr_loc_tel1 USING "&&&&&&&&",
		     ltel_dia1                    ,
		     lhr_loc_tel2 USING "&&&&&&&&",
		     ltel_dia2,		     
                     257 SPACES}
        ---             311 SPACES
   
        --- CPL1713 no genera un registro, genera n regs
        FOR    gi_x          =  1    TO  100 
               INITIALIZE   ga_d[gi_x]   TO   NULL 
        END FOR 

        CALL   fn_ObtOp33(gs_idope, lr_d.*)    
        IF     gi_tra        >  0    THEN 
               FOR   gi_x       =  1  TO   gi_tra 
                     PRINT  COLUMN 1, ga_d[gi_x].lin 
               END FOR 
               LET   tot         =   tot  + gi_tra
        END IF     
    ON LAST ROW

        LET tot = 0
        LET tot = count(*)
        LET tot_char = tot USING "&&&&&&&&&"

        PRINT COLUMN 001, "09"      ,
                          tot_char  ,
                          489 SPACES

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
        --folio_edo_cta       CHAR(8),
        folio_edo_cta       CHAR(20)  ,       ---foledocta
        cuatrim_emision     DECIMAL(5,0),
        cod_afore_ced       SMALLINT,
        documento_3         CHAR(1)
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
            COLUMN 01,"AFIB006"                                          ,
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
        LET num10 = num2                   USING "&&&&&&&&&&"

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
            COLUMN 59,w_aux.folio_edo_cta    USING "&&&&&&&&&&&&&&&&&&&&"  ,        ---foledocta
            COLUMN 72,w_aux.cod_afore_ced    USING "&&&"

END REPORT

REPORT listado_3(ln_seguro, ln_unico, lfolio_solicitud, lpaterno, lmaterno,
		 lnombres, lcod_promotor, lstatus_prom, lmotivo_suspende,
                 lfecha_suspende, lfecha_elaboracion)
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
   lnombre_comp         CHAR(120) ,
   lmotivo_suspende     CHAR(02)  ,
   lfecha_suspende      DATE      ,
   lfecha_elaboracion   DATE

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
            COLUMN 035,"LISTA RECHAZOS REG TRASPASOS A CERT POR PROMOTOR" ,
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
	    COLUMN 104,"STATUS"      ,
            COLUMN 116,"MOT"         ,
            COLUMN 119,"F SUSPENDE"  ,
            COLUMN 130,"F FIRMA"

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
	    COLUMN 104,lstatus_prom            ,
            COLUMN 116,lmotivo_suspende        ,
            COLUMN 119,lfecha_suspende         ,
            COLUMN 130,lfecha_elaboracion

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

    OPEN WINDOW cent AT 11,25 WITH FORM "AFIB0022" ATTRIBUTE(BORDER)

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
               consec CLIPPED,".TAA > informix" 
    RUN comm

    LET comm = "cd " ,g_seg_modulo.ruta_envio CLIPPED,
               "/ ; mv informix  ","E",HOYDIA USING "yyyymmdd" CLIPPED,
               consec CLIPPED,".TAA " 
    RUN comm

    LET comm = "cd " ,g_seg_modulo.ruta_envio CLIPPED,
               "/ ; sed -e '/^$/d' ","T",HOYDIA USING "yyyymmdd" CLIPPED,
               consecd CLIPPED,".DOM > informix" 
    RUN comm

    LET comm = "cd " ,g_seg_modulo.ruta_envio CLIPPED,
               "/ ; mv informix  ","T",HOYDIA USING "yyyymmdd" CLIPPED,
               consecd CLIPPED,".DOM " 
    RUN comm

END FUNCTION

FUNCTION fecha_max()
#fh-------------------

    DEFINE diaSemana SMALLINT

    LET diaSemana = WEEKDAY(HOY)

    LET fecha_hasta = HOY - diaSemana UNITS DAY

    CALL fechas(fecha_hasta) RETURNING HOYDIA

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

    FOR contador = 1 TO 5
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

FUNCTION inserta_afi_ctr(status_interno)
#iac----------------------

    DEFINE cod_afore_ced  SMALLINT
    DEFINE status_interno SMALLINT
    DEFINE fentcons       DATE
    DEFINE nom_comp       CHAR(50)
    DEFINE ind_envio      SMALLINT
    DEFINE vagenc_cod     CHAR(10)

    SELECT MAX(p.ind_envio)
    INTO   ind_envio
    FROM   afi_ctr_solicitud p
    WHERE  p.n_seguro       = w_aux.n_seguro
    AND    p.n_folio        = w_aux.n_folio
    AND    p.tipo_solicitud = vtipo_solicitud   --taa x curp
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

    SELECT p.fecha_envio, p.cod_afore_ced, p.fentcons, p.agenc_cod
    INTO   vfecha_envio, cod_afore_ced, fentcons, vagenc_cod
    FROM   afi_solicitud p
    WHERE  p.n_seguro       = w_aux.n_seguro
    AND    p.n_folio        = w_aux.n_folio
    AND    p.tipo_solicitud = vtipo_solicitud   --taa x curp

    IF vfecha_envio IS NULL THEN
       LET vfecha_envio = HOYDIA
    END IF

    LET nom_comp = w_aux.paterno CLIPPED, "$",
                   w_aux.materno CLIPPED, "$",
                   w_aux.nombres CLIPPED

    INSERT INTO afi_ctr_solicitud
    VALUES(vtipo_solicitud             , #tipo_solicitud    --taa x curp
           w_aux.n_folio               , #folio_solicitud
           w_aux.n_seguro              , #NSS
           nom_comp                    , #Nombre completo afore
           status_interno              , #status_interno
           vfecha_envio                , #fecha_envio
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
           w_aux.nacionalidad          , #Nacionalidad
           w_aux.tip_prob              , #Tipo doc prob
           w_aux.fol_prob              , #Fol doc prob
           w_aux.doc_prob              , #Doc prob
           ind_envio                   , #Indicador envio
           ''                          , #Indicador nombre
           0                           , #Codigo operacion
           0                           , #Diag proceso
	   vagenc_cod                  , #Agenc cod
           g_usuario                   , #Usuario
           today                       )

END FUNCTION

REPORT listado_dif(l_dif)
#----------------------

    DEFINE l_dif  RECORD
        n_folio             LIKE afi_solicitud.n_folio  ,
        n_seguro            LIKE afi_solicitud.n_seguro ,
        paterno             LIKE afi_solicitud.paterno  ,
        materno             LIKE afi_solicitud.materno  ,
        nombres             LIKE afi_solicitud.nombres
    END RECORD

    DEFINE 
        nombre_comp  ,
        nombre_comp2 CHAR(120),
        hoy_env  CHAR(8)

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
{        LET hoy_env = (TODAY) USING "&&&&",
                       MONTH(TODAY)USING "&&"  ,
                       DAY(TODAY)  USING "&&"}

        LET hoy_env = YEAR(g_reg.fecha_envio) USING "&&&&",
                      MONTH(g_reg.fecha_envio)USING "&&"  ,
                      DAY(g_reg.fecha_envio)  USING "&&"

        SELECT * 
        INTO   x_afore.*
        FROM   tab_afore_local

        PRINT
            COLUMN 01,"=================================================",
            COLUMN 50,"=============================="
        PRINT
            COLUMN 01,x_afore.razon_social  CLIPPED                      ,
            COLUMN 27,"*LISTA AFILIADOS CON DIFERENCIAS*"                ,
            COLUMN 63,"FECHA :",TODAY     USING "DD-MM-YYYY"
        PRINT
            COLUMN 01,"AFIB006"                                          ,
            COLUMN 35,"EN DOMICILIOS"                                    ,
            COLUMN 63,"No.PAGINA:",PAGENO    USING "#######"
        PRINT
            COLUMN 01,"=================================================",
            COLUMN 50,"=============================="

        PRINT 
            COLUMN 05,"FOLIO "      ,
            COLUMN 16,"NSS"         ,
            COLUMN 27,"A.PATERNO"   ,
            COLUMN 45,"A.MATERNO"   ,
            COLUMN 65,"NOMBRES"

        PRINT
            COLUMN 01,"-------------------------------------------------",
            COLUMN 50,"------------------------------"

    ON EVERY ROW

        PRINT
            COLUMN 01,l_dif.n_folio                            ,
            COLUMN 13,l_dif.n_seguro                           ,
            COLUMN 27,l_dif.paterno [1,15]                     ,
            COLUMN 45,l_dif.materno [1,15]                     ,
            COLUMN 60,l_dif.nombres [1,20]

END REPORT

FUNCTION val_puntos(vtel_hora_ini, vtel_hora_fin)
#vc-------------------------
  DEFINE vtel_hora_ini CHAR(05)
  DEFINE vtel_hora_fin CHAR(05)
  DEFINE chr_loc_tel   CHAR(10)
  DEFINE rhr_loc_tel   CHAR(10) 
  DEFINE mhr_loc_tel   CHAR(08) 
  DEFINE vtam          INTEGER 
  DEFINE vf            INTEGER 

  LET vtam = 0
  LET vf   = 0

  IF vtel_hora_ini IS NOT NULL THEN
     LET chr_loc_tel = vtel_hora_ini, vtel_hora_fin
     LET chr_loc_tel = chr_loc_tel CLIPPED

     LET mhr_loc_tel = chr_loc_tel[1,2], chr_loc_tel[4,5],
                       chr_loc_tel[6,7], chr_loc_tel[9,10]
  ELSE
     LET mhr_loc_tel = NULL
  END IF
  
  {LET vtam = length(chr_loc_tel)

  #Valida puntos
  FOR vf = 1 TO vtam
      IF (chr_loc_tel[vf,vf] =  ':') THEN
      ELSE
        LET rhr_loc_tel[vf,vf] = chr_loc_tel[vf,vf] CLIPPED
        LET mhr_loc_tel        = rhr_loc_tel
        display rhr_loc_tel sleep 1
      END IF
  END FOR
  #LET rhr_loc_tel = rhr_loc_tel CLIPPED
  prompt "Salida mhr_loc_tel: " for mhr_loc_tel}

  RETURN mhr_loc_tel 

END FUNCTION

FUNCTION calcula_edad_sie()

  #LET v_crea_fecha = HOY

  DECLARE curs1 CURSOR FOR stmt1
  OPEN  curs1 USING w_aux.fena, w_aux.fecha_elaboracion
  FETCH curs1 INTO v_edad, v_criterio
  CLOSE curs1

  SELECT ind_edad
  INTO   v_criterio
  FROM tab_rango_edad
  WHERE v_edad BETWEEN edad_min AND edad_max;

  LET v_edad     = v_edad using "##"
  LET v_criterio = v_criterio using "##"

END FUNCTION
