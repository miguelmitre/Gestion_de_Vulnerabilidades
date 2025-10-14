#############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                          #
#Propietario       => E.F.P.                                                #
#Programa AFIB002  => GENERACION DE ARCHIVO PLANO PARA PROCESAR             #
#Sistema           => AFI                                                   #
#Autor             => MAURO MUNIZ CABALLERO                                 #
#Fecha             => 17 DE ENERO DE 2001                                   #
#Actualizacion     => MAURO MUNIZ CABALLERO (Proceso batch)                 #
#Fecha             => 07 DE AGOSTO DE 2004                                  #
#Actualizacion     => FERNANDO HERRERA HERNANDEZ (Circular 28-8)            #
#Actualizacion     => FERNANDO HERRERA HERNANDEZ (Circular 7-10)            #
#Actualizacion     => EDUARDO RESENDIZ MEDINA 13 MARZO 2006  listado_3      #
#############################################################################

#Modificado        = JGHM -    Oct 2014  CPL1713 CUO Datos de Expediente Ident   #
#                  - Incluir la generacion de la op.33                           #
#                  - Actualizar  Feb 2015 actualizar mods metlife                #
##################################################################################

DATABASE safre_af

GLOBALS

    DEFINE g_reg RECORD 
        generar CHAR(1)
    END RECORD

    DEFINE g_opcion CHAR(2)

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
        cod_error_ori SMALLINT,
        coor_captura  LIKE afi_solicitud.coor_captura
    END RECORD

    DEFINE k_aux  RECORD
        tipo_reg            CHAR(02)  ,
        con_dservicio       CHAR(10)  ,
        clave_doperacion    CHAR(02)  ,
        fech_recep_afor     CHAR(08)  ,
        folio_solicitud     CHAR(10)
    END RECORD

   DEFINE
       HOY           DATE     ,
       HOYDIA        DATE     ,
       fecha_hasta   DATE     ,
       num           SMALLINT ,
       HAY_REGISTROS SMALLINT ,
       h_corr        SMALLINT ,
       h_corrd       SMALLINT ,
       vsoli_env     SMALLINT ,
       enter         CHAR(1)  ,
       HORA          CHAR(8)  ,
       consec        CHAR(10) ,
       consecd       CHAR(10) ,
       nom_afi       CHAR(100),
       comm          CHAR(500),
       list_salida   CHAR(500)

    DEFINE x_lotes RECORD LIKE tab_lote.*
    DEFINE g_afore RECORD LIKE tab_afore_local.*
    DEFINE g_seg_modulo RECORD LIKE seg_modulo.*

    DEFINE g_usuario CHAR(8)
    DEFINE G_LISTA   CHAR(100)
    DEFINE G_LISTA1  CHAR(100)
    DEFINE G_IMPRE   CHAR(100)
    DEFINE G_IMPRE2  CHAR(100)
    DEFINE vchar     CHAR(1)
    DEFINE G_LISTA3  CHAR(200)

    DEFINE reg_bat RECORD
        pid            INTEGER,
        proceso_cod    INTEGER,
        opera_cod      INTEGER,
        nombre_archivo CHAR(25)
    END RECORD

    DEFINE bnd_proceso SMALLINT

    DEFINE cont_reg     SMALLINT,
           cont_acep    SMALLINT
    DEFINE vfecha_envio DATE
    DEFINE rech_prom    SMALLINT
    DEFINE vcoor_cap   CHAR(1)

    --- CPL1713  CUO  Oct 2014 
    DEFINE g_dom       CHAR(100)
    DEFINE gs_idope    SMALLINT       --#CUO II CPL1713

END GLOBALS

--- Oct 2014 CPL1713
GLOBALS 'AFIB0001.4gl'

MAIN

    DISPLAY " "
    DISPLAY ".1"

    CALL STARTLOG(FGL_GETENV('USER')||'AFIB002.log')
    CALL inicio() #i

    IF NOT bnd_proceso THEN
        DEFER INTERRUPT
        OPTIONS INPUT WRAP,
            PROMPT LINE LAST,
            ACCEPT KEY CONTROL-I

        CALL proceso_principal() #pp
    ELSE
        --CALL fecha_max()
        CALL rescata_valores()   #rv
        CALL actualiza_bat_f(0)  #rv
    END IF

END MAIN

FUNCTION inicio()
#i--------------

    LET reg_bat.pid            = ARG_VAL(1)
    LET reg_bat.proceso_cod    = ARG_VAL(2)
    LET reg_bat.opera_cod      = ARG_VAL(3)

    LET bnd_proceso = 0

    IF reg_bat.pid THEN
        DISPLAY "INICIANDO PROCESO ..."
        LET bnd_proceso = 1
    END IF

    LET HOY    = TODAY
    LET HORA   = TIME
    LET HOYDIA = TODAY
    LET num    = 1

    LET g_reg.generar = "S"
    LET    gs_idope            =  10          --#CPL1713  CUO II 10 - Registro 

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

    LET cont_reg = 0
    LET cont_acep = 0

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 4,4 WITH FORM "AFIB0021" ATTRIBUTE(BORDER)
    DISPLAY " AFIB002  GENERACION ARCHIVO CERTIFICACION (POR REGISTRO)                      " AT 3,1 ATTRIBUTE(REVERSE)
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
            --CALL fecha_max()
            --CALL Tipo_de_envio()
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

    PROMPT "NOMBRE ARCHIVO PLANO : ", comm CLIPPED,
           " /[Enter] para salir" FOR enter

END FUNCTION

FUNCTION rescata_valores()
#rv----------------------

    DEFINE recha SMALLINT

    --- CPL1713 CUO  Oct 2914 
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
    #AND    frecafor      <= fecha_hasta
    AND    tipo_solicitud = 1

    IF HAY_REGISTROS THEN
        SELECT * 
        INTO   x_lotes.* 
        FROM   tab_lote
        WHERE  lotes_cod = 10
        AND    lotes_fecha = HOYDIA

        IF SQLCA.SQLCODE = 0 THEN
            UPDATE tab_lote 
            SET    lotes_correlativo = lotes_correlativo + 1
            WHERE  lotes_cod         = 10
            AND    lotes_fecha       = HOYDIA
        ELSE
            INSERT INTO tab_lote
            VALUES(HOYDIA     ,
                   10          ,
                   'AFILIADOS',
                   1          ,
                   1)
        END IF

        SELECT lotes_correlativo
        INTO   h_corr 
        FROM   tab_lote
        WHERE  lotes_cod   = 10
        AND    lotes_fecha = HOYDIA

        LET consec = h_corr

        UPDATE tab_lote
        SET    lotes_correlativo = lotes_correlativo + 1
        WHERE  lotes_cod         = 10
        AND    lotes_fecha       = HOYDIA

        SELECT l.lotes_correlativo
        INTO   h_corrd
        FROM   tab_lote l
        WHERE  l.lotes_cod   = 10
        AND    l.lotes_fecha = HOYDIA

        LET consecd = h_corrd

        LET vsoli_env = HAY_REGISTROS

        IF NOT bnd_proceso THEN
            DISPLAY "SOLICITUDES A SER ENVIADAS ", vsoli_env AT 15,20
        ELSE
            DISPLAY "SOLICITUDES A SER ENVIADAS ", vsoli_env 
        END IF

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
               A.coor_captura
        FROM   afi_solicitud A
        WHERE  A.status_interno = 20 #CON TODOS LOS DOCTOS
        AND    A.tipo_solicitud = 1
        #AND    A.frecafor      <= fecha_hasta

        DISPLAY "Procesando Informacion"

        LET comm = g_seg_modulo.ruta_envio CLIPPED,
                   "/E",HOYDIA USING "yyyymmdd" CLIPPED,
                   consec CLIPPED, ".AFI" CLIPPED

        DISPLAY "Archivo: ", comm CLIPPED

        LET nom_afi = "E",HOYDIA USING "yyyymmdd" CLIPPED,
                      consec CLIPPED, ".AFI" CLIPPED

        LET G_LISTA  = g_seg_modulo.ruta_listados CLIPPED,
                       "/",g_usuario CLIPPED, "ARCHIVO_CERT_REG."
                       CLIPPED,HOY USING "dd-mm-yy","_",
                       consec CLIPPED, HORA CLIPPED

        LET G_LISTA1 = g_seg_modulo.ruta_listados CLIPPED,
                       "/",g_usuario CLIPPED,
                       ".REG_CERTRECH_PRO." CLIPPED,
                       HOY USING "ddmmyy","_",consec CLIPPED

       LET G_LISTA3  = g_seg_modulo.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                       ".REPORTE_CERT." CLIPPED,
                       HOY USING "dd-mm-yy",".",HORA CLIPPED

       --- CPL1713 CUO OCT 2014 
        LET    g_dom      = g_seg_modulo.ruta_envio CLIPPED,
                            "/T",HOYDIA USING "yyyymmdd" CLIPPED,
                            consecd CLIPPED, ".DOM" CLIPPED
        START  REPORT       listado_d   TO   g_dom
 
        START REPORT listado   TO comm
        START REPORT listado_1 TO G_LISTA1
        START REPORT listado_2 TO G_LISTA
        START REPORT listado_3 TO G_LISTA3


        FOREACH curs_1 INTO w_aux.*
            LET HAY_REGISTROS = TRUE
            LET cont_reg      = cont_reg + 1

            LET k_aux.folio_solicitud = w_aux.n_folio

            IF w_aux.materno IS NULL OR
               w_aux.materno MATCHES '  *' THEN
                LET w_aux.materno = "N/A"
            END IF

            SELECT NVL(A.status,3)
            INTO   recha
            FROM   pro_mae_promotor A
            WHERE  A.cod_promotor = w_aux.cod_promotor

            IF recha = 2 OR
               recha = 3 THEN
                LET rech_prom = rech_prom + 1

                OUTPUT TO REPORT listado_1(w_aux.n_seguro        ,
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
                AND    @tipo_solicitud = 1
                #AND    @status_interno = 20

                UPDATE solicitudafi
                SET    stamcertificafi = '21-Rechazo x promotor invalido'
                WHERE  nsolicitud      = w_aux.n_folio

              CALL inserta_afi_ctr(21)

              INSERT INTO afi_ctr_logico
              VALUES (k_aux.folio_solicitud,
                      1,
                      w_aux.n_seguro,
                      21,
                      g_usuario,
                      HOY,
                      HORA,
                      "BAJA PROMOTOR") 

              CONTINUE FOREACH
            END IF
 
            --IF bnd_proceso THEN
                LET g_opcion = '02'
            --END IF
                      
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
                          consec CLIPPED, ".AFI" CLIPPED
                                    
               START REPORT listado   TO comm
            END IF

            --- Incluyendo CPL1713 CUO  Sep 2014  
            LET    lr_d.nss              =  w_aux.n_seguro
            LET    lr_d.n_folio          =  k_aux.folio_solicitud
            LET    lr_d.tipo_sol         =  1               
            LET    lr_d.proceso          =  '01'
            OUTPUT TO REPORT listado_d(lr_d.*)


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
                                     w_aux.coor_captura)


            OUTPUT TO REPORT listado_2(k_aux.tipo_reg      ,
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
                                     w_aux.cod_error_ori)

          CALL inserta_afi_ctr(30)

        END FOREACH

        OUTPUT TO REPORT listado_3(cont_reg, vsoli_env)   ---erm 13 Marzo 2006

        FINISH REPORT listado
        FINISH REPORT listado_1
        FINISH REPORT listado_2
        FINISH REPORT listado_3
        FINISH REPORT listado_d

        LET cont_reg = cont_reg - rech_prom
        
        IF NOT bnd_proceso THEN
            DISPLAY "SOLICITUDES A SER ENVIADAS ", cont_reg  AT 15,20
            DISPLAY "SOLICITUDES RECH PROMOTOR  ", rech_prom AT 16,20
            IF rech_prom <> 0 THEN
               DISPLAY "NOM ARCH PROM: ", G_LISTA1 CLIPPED   AT 17,1
            END IF
            PROMPT "Presione [enter] para continuar." FOR enter
        ELSE
            DISPLAY "SOLICITUDES A SER ENVIADAS ", cont_reg
        END IF

        LET G_IMPRE = "lp ", g_seg_modulo.ruta_listados CLIPPED,
                      "/",g_usuario CLIPPED, "ARCHIVO_CERT_REG."
                      CLIPPED,HOY USING "dd-mm-yy","_",
                      consec CLIPPED, HORA CLIPPED
        --RUN G_IMPRE

        LET G_IMPRE2 = "lp ", g_seg_modulo.ruta_listados CLIPPED,
                      "/",g_usuario CLIPPED,
                      ".REPORTE_CERT1." CLIPPED,
                      HOY USING "dd-mm-yy",".",HORA CLIPPED
        --RUN G_IMPRE2

        CALL limpia_nulos()

        UPDATE afi_solicitud
        SET    status_interno = 30,
               lote           = h_corr,
               fecha_envio    = HOYDIA
        WHERE  status_interno = 20
        AND    tipo_solicitud = 1
        #AND    frecafor      <= fecha_hasta

    ELSE
        IF bnd_proceso THEN
            DISPLAY "Program stopped, NO HAY SOLICITUDES A ENVIAR"
        END IF
    END IF

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
        frecafor            LIKE afi_solicitud.frecafor,
        nacionalidad        LIKE afi_solicitud.nacionalidad,
        tip_prob            LIKE afi_solicitud.tip_prob,
        fol_prob            LIKE afi_solicitud.fol_prob,
        doc_prob            LIKE afi_solicitud.doc_prob,
        ind_infonavit       LIKE afi_solicitud.ind_infonavit,
        cod_error_ori       SMALLINT,
        coor_captura        LIKE afi_solicitud .coor_captura
    END RECORD

    DEFINE 
        nombre_comp  ,
        nombre_comp2 CHAR(120)

    DEFINE
        d CHAR(2),
        m CHAR(2),
        a CHAR(4)

    DEFINE
        dia  CHAR(2),
        mes  CHAR(2),
        ano  CHAR(4),
        dia1 CHAR(2),
        mes1 CHAR(2),
        ano1 CHAR(4)

    DEFINE
        hoy CHAR(8),
        vhoy DATE

    DEFINE x_afore RECORD LIKE tab_afore_local.*

    DEFINE 
        hoy_env  CHAR(8),
        tot_char CHAR(9),
        num10    CHAR(10)

    DEFINE 
        tot         ,
        var         ,
        cod_err_ori SMALLINT

    DEFINE d_dp   DECIMAL(16,0)

    OUTPUT
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0
        PAGE LENGTH 2

    FORMAT
    FIRST PAGE HEADER
        LET hoy_env = YEAR(HOYDIA) USING "&&&&",
                      MONTH(HOYDIA)USING "&&"  ,
                      DAY(HOYDIA)  USING "&&"

        SELECT * 
        INTO   x_afore.*
        FROM   tab_afore_local

        SELECT max(lotes_correlativo)
        INTO   h_corr
        FROM   tab_lote
        WHERE  lotes_cod = 10
        AND    lotes_fecha = vhoy

        PRINT COLUMN 001,"01",
                         "01",
                         "10",
                         "01",
                         x_afore.codigo_afore USING "&&&",
                         "03",
                         "001",
                         hoy_env,
--                         x_afore.estado USING "&&", " ",
                         '19 ',
                         h_corr USING "&&&",  #CORRELATIVO DE ENVIO
                         g_opcion USING "&&", #MEDIO POR DONDE SE ENVIA ARCHIVO
                         538 SPACES

    ON EVERY ROW

        LET a     = YEAR (w_aux.frecafor)  USING "&&&&"
        LET m     = MONTH (w_aux.frecafor) USING "&&"
        LET d     = DAY (w_aux.frecafor)   USING "&&"
        LET dia   = DAY(w_aux.fena)        USING"&&"
        LET mes   = MONTH(w_aux.fena)      USING"&&"
        LET ano   = YEAR(w_aux.fena)       USING "&&&&"
        LET dia1  = DAY(TODAY)             USING"&&"
        LET mes1  = MONTH(TODAY)           USING"&&"
        LET ano1  = YEAR(TODAY)            USING "&&&&"
        LET num10 = "          "
        LET num10 = num                    USING "&&&&&&&&&&"

        IF w_aux.ind_infonavit = "S" THEN
            LET var = 1
        ELSE
            LET var = 0
        END IF

        IF w_aux.tip_prob <> 5 THEN
            LET w_aux.n_unico = "                  " 
        END IF

        IF w_aux.tip_prob = 1 THEN
            LET d_dp           = w_aux.doc_prob
            LET w_aux.doc_prob = d_dp USING "&&&&&&&&&&&&&&&&"
        END IF

        IF w_aux.tip_prob = 5 THEN
            LET w_aux.doc_prob = w_aux.n_unico[1,16]
        END IF

        IF w_aux.coor_captura = 1 THEN
           LET vcoor_cap  = "1"
        END IF

        IF w_aux.coor_captura IS NULL THEN
           LET vcoor_cap  = " "
        END IF

        PRINT
            COLUMN 1,"02"                   ,
                     num10                  ,
                     "01"                   ,
                     w_aux.n_seguro         ,
                     w_aux.n_unico          ,
                     w_aux.n_rfc            ,
                     w_aux.paterno          ,
                     w_aux.materno          ,
                     w_aux.nombres          ,
                     ano,mes,dia            ,
                     w_aux.cod_promotor     USING "&&&&&&&&&&" ,
                     a,m,d                  ,
                     k_aux.folio_solicitud  USING "&&&&&&&&&&" ,
                     w_aux.sexo             USING "&"          ,
                     w_aux.estadon          USING "&&"         ,
                     --var                    USING "&"          ,
                     vcoor_cap              ,
                     w_aux.nacionalidad     ,
                     w_aux.tip_prob         ,
                     w_aux.fol_prob         USING "&&&&&&&&&&",
                     w_aux.doc_prob         ,
                    -- w_aux.cod_error_ori    USING "&&&&"       ,
                    -- 3 SPACES               ,
                    -- '00000000'             ,
                     324 SPACES

        LET num = num + 1

    ON LAST ROW
        LET tot = 0
        LET tot = count(*)
        LET tot_char = tot USING "&&&&&&&&&"

        PRINT
            COLUMN 001, "09"                               ,
                        "01"                               ,
                        x_afore.codigo_afore USING "&&&"   ,
                        hoy_env                            ,
                        h_corr USING "&&&"                 ,
                        "01"                               ,
                        "10"                               ,
                        tot_char                           ,
                        "000000000"                        ,
                        #total de solicitudes respondidas
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
                    1,
                    nom_afi)
        END IF

END REPORT

FUNCTION Respalda_diskette()
#---------------------------

    DEFINE aux_pausa CHAR(1)

    WHILE TRUE
        PROMPT "Inserte diskette para enviar informacion y presione <ENTER> "
        FOR aux_pausa
        ERROR "Traspasando Informacion desde Base de Datos a Diskette"
        LET comm = "doscp ",comm CLIPPED," a:" CLIPPED
        RUN comm
        ERROR "Proceso Finalizo en forma exitosa" sleep 2
        EXIT WHILE
    END WHILE

END FUNCTION

REPORT listado_2(w_aux)
#----------------------

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
        frecafor            LIKE afi_solicitud.frecafor,
        nacionalidad        LIKE afi_solicitud.nacionalidad,
        tip_prob            LIKE afi_solicitud.tip_prob,
        fol_prob            LIKE afi_solicitud.fol_prob,
        doc_prob            LIKE afi_solicitud.doc_prob,
        ind_infonavit       LIKE afi_solicitud.ind_infonavit,
        cod_error_ori       SMALLINT
    END RECORD

    DEFINE
        nombre_comp  ,
        nombre_comp2 CHAR(120)

    DEFINE
        d     CHAR(02),
        m     CHAR(02),
        a     CHAR(04)

    DEFINE
        dia       CHAR(02) ,
        mes       CHAR(02) ,
        ano       CHAR(04) ,
        dia1      CHAR(02) ,
        mes1      CHAR(02) ,
        ano1      CHAR(04) ,
        hoy       CHAR(8)  ,
        num10     CHAR(10) ,
        tot_char  CHAR(9)

    DEFINE x_afore RECORD LIKE tab_afore_local.*

    DEFINE tot SMALLINT

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

        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT
            COLUMN 01,"=================================================",
            COLUMN 50,"=============================="
        PRINT
            COLUMN 01,x_afore.razon_social  CLIPPED                      ,
            COLUMN 35,"LISTA DE AFILIADOS"                               ,
            COLUMN 60,"FECHA     :",TODAY     USING "DD-MM-YYYY"
        PRINT
            COLUMN 01,"AFIB002"                                          ,
            COLUMN 35,"ENVIADOS  A PROCERSAR"                               ,
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
            COLUMN 50,"F.RECEP"     ,
            COLUMN 63,"F.SOLIC"     ,
            COLUMN 76,"SEXO"        ,
            COLUMN 80,"ENT.NAC"

        PRINT
            COLUMN 01,"CRED.INFONAVIT",
            COLUMN 17,"NAC."          ,
            COLUMN 22,"TIP.PROB"      ,
            COLUMN 32,"FOL.PROB"      ,
            COLUMN 42,"DOC.PROB"

        PRINT
            COLUMN 01,"-------------------------------------------------",
            COLUMN 50,"------------------------------"

        ON EVERY ROW
            LET a     = YEAR (w_aux.frecafor)  USING "&&&&"
            LET m     = MONTH (w_aux.frecafor) USING "&&"
            LET d     = DAY (w_aux.frecafor)   USING "&&"
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
            COLUMN 50,a,m,d                 ,
            COLUMN 63,k_aux.folio_solicitud USING "&&&&&&&&&&" ,
            COLUMN 76,w_aux.sexo            USING "&"          ,
            COLUMN 80,w_aux.estadon         USING "&&"         

        PRINT
            COLUMN 01,w_aux.ind_infonavit ,
            COLUMN 17,w_aux.nacionalidad  ,
            COLUMN 22,w_aux.tip_prob      ,
            COLUMN 32,w_aux.fol_prob      ,
            COLUMN 42,w_aux.doc_prob

END REPORT

REPORT listado_3(cont_reg,vsolv_env)    ---erm 13 Marzo 2006

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


    LET g_nombre = g_usuario CLIPPED,
                   ".SOLIC_VALIDA." CLIPPED,
                   HOY USING "dd-mm-yy",".",HORA CLIPPED,".txt"


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
        COLUMN 03,"Tipo                   : ", "1", "  ","REGISTRO"
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
        COLUMN 10,G_LISTA CLIPPED
      SKIP 2 LINE
      PRINT
        COLUMN 03,"========================================",
        COLUMN 40,"==================================================="



END REPORT

FUNCTION Tipo_de_envio()

    DEFINE z_reg ARRAY[6] OF RECORD
        cod CHAR(2),
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

    LET comm = "cd " ,g_seg_modulo.ruta_envio CLIPPED,
               "/ ; sed -e '/^$/d' ","E",HOYDIA USING "yyyymmdd" CLIPPED,
               consec CLIPPED,".AFI > informix" 
    RUN comm

    LET comm = "cd " ,g_seg_modulo.ruta_envio CLIPPED,
               "/ ; mv informix  ","E",HOYDIA USING "yyyymmdd" CLIPPED,
               consec CLIPPED,".AFI " 
    RUN comm

END FUNCTION

FUNCTION fecha_max()
#fh-------------------

    DEFINE diaSemana SMALLINT

    LET diaSemana = WEEKDAY(HOY)

    LET fecha_hasta = HOY - diaSemana UNITS DAY

    CALL fechas(fecha_hasta) RETURNING HOYDIA

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
       nom_archivo = nom_afi
WHERE pid         = reg_bat.pid
and   proceso_cod = reg_bat.proceso_cod
and   opera_cod   = reg_bat.opera_cod

UPDATE bat_ctr_proceso
set    folio       = NULL ,      
       estado_cod  = 4    ,
       fecha_fin   = CURRENT,
       nom_archivo = nom_afi
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

FUNCTION inserta_afi_ctr(status_interno)
#iac----------------------

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
    AND    p.tipo_solicitud = 1
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
    INTO   vfecha_envio, cod_afore_ced, fentcons
    FROM   afi_solicitud p
    WHERE  p.n_seguro       = w_aux.n_seguro
    AND    p.n_folio        = w_aux.n_folio
    AND    p.tipo_solicitud = 1

    IF vfecha_envio IS NULL THEN
       LET vfecha_envio = HOYDIA
    END IF

    LET nom_comp = w_aux.paterno CLIPPED, "$",
                   w_aux.materno CLIPPED, "$",
                   w_aux.nombres CLIPPED

    INSERT INTO afi_ctr_solicitud
    VALUES(1                           , #tipo_solicitud
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
	   ''                          , #Agenc cod
           g_usuario                   , #Usuario
           today                       )

END FUNCTION


REPORT listado_1(ln_seguro, ln_unico, lfolio_solicitud, lpaterno, lmaterno,
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
            COLUMN 035,"LISTA RECHAZOS REG INICIAL   A CERT POR PROMOTOR" ,
            COLUMN 088,"FECHA     : ",TODAY USING "DD-MM-YYYY"
        PRINT
            COLUMN 001,"AFIB002"                                          ,
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
    DEFINE         vhoy DATE

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
        LET vhoy = TODAY

        SELECT max(lotes_correlativo)
        INTO   h_corrd
        FROM   tab_lote
        WHERE  lotes_cod = 10
        AND    lotes_fecha = vhoy

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

        LET dia1   = DAY(TODAY)   USING"&&"
        LET mes1   = MONTH(TODAY) USING"&&"
        LET ano1   = YEAR(TODAY)  USING "&&&&"
        LET num10d = num          USING "&&&&&&&&&&"
        
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
        LET tot_char = tot USING "&&&&&&&&&"

        PRINT COLUMN 001, "09"      ,
                          tot_char  ,
                          489 SPACES

END REPORT

