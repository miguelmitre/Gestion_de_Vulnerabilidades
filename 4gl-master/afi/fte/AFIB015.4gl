###########################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                        #
#Propietario       => E.F.P.                                              #
#Programa AFIB015  => GENERACION DE ARCHIVO ARCHIVO MASIVO DE DOMICILIOS  #
#Sistema           => AFI                                                 #
#Autor             => MAURO MUNIZ CABALLERO                               #
#Fecha             => 7 DE ENERO DE 2004                                  #
###########################################################################

DATABASE safre_af

GLOBALS

    DEFINE 
        generar  SMALLINT,
        tipo_des CHAR(20)

    DEFINE 
        g_opcion CHAR(2),
        carga    CHAR(50),
        ejecuta  CHAR(100)

    DEFINE w_aux  RECORD
       n_seguro       LIKE afi_mae_afiliado.n_seguro     ,
       n_folio        LIKE afi_mae_afiliado.n_folio      ,
       tipo_solicitud LIKE afi_mae_afiliado.tipo_solicitud       
   END RECORD

   DEFINE k_aux  RECORD
       tipo_reg          CHAR(02) ,
       con_dservicio     CHAR(10) ,
       clave_doperacion  CHAR(02) ,
       fech_recep_afor   CHAR(08) ,
       folio_solicitud   CHAR(10)
   END RECORD

   DEFINE 
       fecha_1         DATE      ,
       fecha_2         DATE      ,
       fecha_3         DATE      ,
       HOY             DATE      ,
       HOYDIA          DATE      ,
       fecha_hasta     DATE      ,
       num             INTEGER  ,
       v_fecha_max     DATE      ,
       HAY_REGISTROS   INTEGER  ,
       h_corr          INTEGER  ,
       vsoli_env       INTEGER   ,
       enter           CHAR(1)   ,
       g_usuario       CHAR(8)   ,
       HORA            CHAR(8)   ,
       consec          CHAR(10)  ,
       G_LISTA         CHAR(100) ,
       nom_taa         CHAR(500) ,
       comm            CHAR(500) ,
       list_salida     CHAR(500) ,
       reenvio         CHAR(01)

    DEFINE x_lotes RECORD LIKE tab_lote.*
    DEFINE g_afore       RECORD LIKE tab_afore_local.*
    DEFINE g_seg_modulo  RECORD LIKE seg_modulo.*

    DEFINE reg_bat RECORD
        pid            INTEGER,
        proceso_cod    INTEGER,
        opera_cod      INTEGER,
        nombre_archivo CHAR(25)
    END RECORD

    DEFINE bnd_proceso INTEGER 

    --- circ 28-7
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
       pais        CHAR(3),            ---#op 15
        ent_fed     CHAR(65)
    END RECORD

    DEFINE h_corrd     INTEGER
    DEFINE consecd     CHAR(10)
    DEFINE lote_dom    INTEGER
    DEFINE vdelegacion CHAR(65)
    DEFINE vent_fed    CHAR(65)

    DEFINE g_dom       CHAR(100) 

    DEFINE reg_cte RECORD
        nss CHAR(11)
    END RECORD

    DEFINE reg_pro RECORD
        nss CHAR(11)
    END RECORD

    DEFINE g_plano1 INTEGER

    DEFINE
        g_imprime  CHAR(200),
        gimpresion CHAR(200)

   DEFINE tot_no_cte INTEGER

	 DEFINE vtelefono  CHAR(13)              #op 15
   DEFINE vtelefono1 CHAR(10)              #op 15
   DEFINE vtelefono2 CHAR(10)              #op 15
   DEFINE vtel1      ,                     #op 15
          vtel2      CHAR(13)              #op 15
   DEFINE vextension CHAR(5)              #op 15
   DEFINE vind_tel1  CHAR(3)               #op 15
   DEFINE vind_tel2  CHAR(3)               #op 15
   DEFINE vprefijo1  CHAR(3)               #op 15
   DEFINE vprefijo2  CHAR(3)               #op 15
   DEFINE vcorreo_elect CHAR(100)          #op 15
   DEFINE vcod_correo_e CHAR(2)            #op 15
   DEFINE vcve_lada1    CHAR(3)            #op 15
   DEFINE vcve_lada2    CHAR(3)            #op 15
   DEFINE reg_tel       RECORD LIKE afi_telefono.*
   DEFINE vexten1       CHAR(5)            #op 15
   DEFINE vexten2       CHAR(5)            #op 15
   DEFINE vtotal_tel    SMALLINT           #op 15
   DEFINE vcc_tel       SMALLINT           #op 15
   DEFINE ban_tel       SMALLINT           #op 15
   DEFINE vtam          SMALLINT           #op 15
   DEFINE vcont_tel     SMALLINT           #op 15
   DEFINE cont_correo   SMALLINT           #op 15

END GLOBALS

MAIN

    DISPLAY " "
    DISPLAY ".1"

    CALL STARTLOG("AFIB015.log")
    CALL inicio()

    IF NOT bnd_proceso THEN
        DEFER INTERRUPT
            OPTIONS INPUT WRAP,
            PROMPT LINE LAST

        CALL proceso_principal()   #pp
    ELSE
        CALL rescata_valores()     #rv
    END IF

END MAIN

FUNCTION inicio()
#i---------------

    DEFINE vpdq CHAR(100)

    LET reg_bat.pid         = ARG_VAL(1)
    LET reg_bat.proceso_cod = ARG_VAL(2)
    LET reg_bat.opera_cod   = ARG_VAL(3)

    LET vpdq = "set pdqpriority high"

    PREPARE eje_pdq FROM vpdq

    EXECUTE eje_pdq

    LET bnd_proceso = 0

    LET g_opcion    = '02'

    IF reg_bat.pid THEN
        DISPLAY "INICIANDO PROCESO ..."
        LET bnd_proceso = 1
    END IF 

    LET k_aux.tipo_reg         = "02"  
    LET g_opcion               = "02"  
    LET k_aux.con_dservicio    = "          "
    LET k_aux.clave_doperacion = "01" 
    LET k_aux.fech_recep_afor  = "       " 
    LET k_aux.folio_solicitud  = "          "

    SELECT *
    INTO   g_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = 'afi'

    SELECT *,USER
    INTO   g_afore.*,g_usuario 
    FROM   tab_afore_local

    LET HOY    = TODAY
    LET HORA   = TIME
    LET HOYDIA = TODAY
    LET num    = 0

    --- circ 28-7
    LET reg_dom.tipo_reg   = '02'
    LET reg_dom.tipo_recep = '01'
    LET reg_dom.cve_recep  =  g_afore.codigo_afore USING '&&&'

    CREATE TEMP TABLE nss_ced_afi
        (nss CHAR(11))

    CREATE TEMP TABLE nss_no_cte_afi
        (nss  CHAR(11),
         tipo CHAR(15))

     {SELECT a.fentcons
     INTO   fecha_1
     FROM   afi_env_domicilio a

     IF SQLCA.SQLCODE = 100 THEN
         LET fecha_1 = '01/01/1997'

         INSERT INTO afi_env_domicilio
         VALUES ('01/01/1997')
     END IF}

     LET fecha_3 = fecha_1 + 1 UNITS DAY
     LET fecha_2 = TODAY

     CREATE TEMP TABLE nss_dom
     (nss CHAR(11))

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 4,4 WITH FORM "AFIB0151" ATTRIBUTE(BORDER)

    DISPLAY " AFIB015       GENERACION DE ARCHIVO DE DOMCIILIOS                             " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                               < Ctrl-C > Salir                                " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)

    INPUT BY NAME generar
        AFTER FIELD generar

        IF generar IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD generar
        END IF

        IF generar <> 1 AND
           generar <> 2 THEN
            ERROR "Archivo solo es (1) Masivo/Normal, (2) Modificaciones"
            SLEEP 2
            NEXT FIELD generar
            ERROR ""
        END IF

        IF generar = 1 OR
           generar = 2 THEN
            IF generar = 1 THEN
                LET tipo_des = 'MASIVO/NORMAL'

                 DISPLAY "PERIODO A ENVIAR : ", fecha_1 USING "DD/MM/YYYY",
                         " a ", fecha_2 USING "DD/MM/YYYY" AT 14,14
            ELSE
                LET tipo_des = 'MODIFICACIONES'
            END IF

            DISPLAY BY NAME tipo_des

            WHILE TRUE
                PROMPT "¿Esta seguro de procesar el archivo [S/N]? "
                ATTRIBUTES(reverse)
                FOR enter

                IF enter MATCHES "[SsNn]" THEN
                    IF enter MATCHES "[Nn]" THEN
                        ERROR "PROCESO CANCELADO" 
                        SLEEP 2
                        RETURN
                    ELSE
                        EXIT WHILE
                    END IF
                ELSE
                    DISPLAY "Solo debe presionar (S) Si o (N) No" AT 19,2
                END IF
            END WHILE
        END IF

        IF generar = 1 THEN
            CALL rescata_nss()
        END IF

        CALL rescata_valores()

        IF NOT HAY_REGISTROS THEN
            ERROR "NO HAY REGISTROS PARA PROCESAR..."
            SLEEP 3
            EXIT PROGRAM
        END IF

        EXIT INPUT

        ON KEY ( INTERRUPT )
            ERROR "PROCESO CANCELADO" SLEEP 2
            EXIT PROGRAM
    END INPUT

    IF HAY_REGISTROS THEN
        PROMPT "NOMBRE ARCHIVO PLANO : ", nom_taa CLIPPED,
               " /[Enter] para salir" FOR enter  

        IF NOT bnd_proceso THEN
            DISPLAY "DOMICILIOS GENERADOS      : ", num AT 17,14 
        ELSE
            DISPLAY "DOMICILIOS GENERADOS      : ", num 
        END IF
    END IF

    {IF generar = 1 THEN
        UPDATE afi_env_domicilio
        SET    fentcons = fecha_2 
    END IF}

    LET g_imprime = g_seg_modulo.ruta_listados CLIPPED,
                    "/",g_usuario CLIPPED,
                    ".DOM_MASIVO.",HOY USING "yymmdd",
                    HORA[1,2],HORA[4,5],HORA[7,8] CLIPPED

    START REPORT rep_dom_masivo TO g_imprime
        OUTPUT TO REPORT rep_dom_masivo()
    FINISH REPORT rep_dom_masivo

    LET gimpresion = 'lp ', g_imprime
    RUN gimpresion

END FUNCTION

FUNCTION rescata_nss()
#rn-------------------

    INSERT INTO nss_ced_afi
    SELECT UNIQUE c.n_seguro
    FROM   taa_det_cedido c
    WHERE  c.fecha_trasp <= fecha_2
    AND    c.estado IN(10,12)

    INSERT INTO nss_ced_afi
    SELECT UNIQUE u.nss_cta1
    FROM   uni_unificado u
    WHERE  u.estado = 100
    AND    u.fliquida <= fecha_2

    CREATE INDEX nss_ced_afi1
    ON nss_ced_afi(nss)

    UPDATE STATISTICS FOR TABLE nss_ced_afi

    INSERT INTO nss_dom
    SELECT p.n_seguro
    FROM   afi_mae_afiliado p
    WHERE  p.n_seguro NOT IN(SELECT i.nss
                             FROM   nss_ced_afi i)

    CREATE INDEX nss_dom1 ON nss_dom(nss)

    UPDATE STATISTICS FOR TABLE nss_dom

END FUNCTION

FUNCTION rescata_valores()
#rv-----------------------

    DEFINE 
        cont_dom INTEGER,
        cont_tel INTEGER            ---erm 06 Septiembre 2007

    DEFINE
        sel_where CHAR(1000)

    LET cont_dom      = 0
		LET cont_tel      = 0            ---erm 06 Septiembre 2007
    LET cont_correo   = 0            ---erm 06 Septiembre 2007
    LET HAY_REGISTROS = FALSE

    IF generar = 1 THEN
        SELECT COUNT(*)
        INTO   HAY_REGISTROS
        FROM   afi_mae_afiliado a, afi_domicilio o, nss_dom p
        WHERE  a.fentcons BETWEEN fecha_3 AND fecha_2
        AND    a.n_seguro       = o.nss 
        AND    a.n_folio        = o.n_folio 
        AND    a.tipo_solicitud = o.tipo_solicitud
        AND    a.tipo_solicitud <> 5
        AND    a.n_seguro       = p.nss
        AND    o.marca_envio    = "X"
    ELSE
        SELECT COUNT(*)
        INTO   HAY_REGISTROS
        FROM   afi_mae_modifica
        WHERE  cod_operacion = 12
        AND    fecha_actualiza IS NULL
    END IF

    IF HAY_REGISTROS THEN
        SELECT max(@lotes_fecha)
        INTO   v_fecha_max  
        FROM   tab_lote
        WHERE  @lotes_cod = 2

        IF v_fecha_max <> HOY OR
           v_fecha_max IS NULL THEN
            INSERT INTO tab_lote
            VALUES(HOY        ,
                   2          ,
                   'AFILIADOS',
                   1          ,
                   1)            
        ELSE
            UPDATE tab_lote 
            SET    lotes_correlativo = lotes_correlativo + 1
            WHERE  lotes_cod = 2
            AND    lotes_fecha = HOY
        END IF
{
        --- circ 28-7
        SELECT tl.lotes_correlativo
        INTO   h_corr
        FROM   tab_lote tl
        WHERE  tl.lotes_cod = 2
        AND    tl.lotes_fecha = HOY

        LET consec = h_corr
}
        ---30042009
        DECLARE c_lotes CURSOR FOR
        SELECT tl.lotes_correlativo
        FROM   tab_lote tl
        WHERE  tl.lotes_cod = 2
        AND    tl.lotes_fecha = HOY
        GROUP BY 1
        FOREACH c_lotes INTO   h_corr
        EXIT FOREACH
        END FOREACH


        LET vsoli_env = hay_registros

        IF NOT bnd_proceso THEN
            DISPLAY "DOMICILIOS A SER ENVIADOS : ", vsoli_env AT 16,14 
        ELSE
            DISPLAY "DOMICILIOS A SER ENVIADOS : ", vsoli_env 
        END IF

        lET HAY_REGISTROS = FALSE

        IF generar = 1 THEN
            LET sel_where = " SELECT A.n_seguro,   ",
                            "        O.calle,      ",
                            "        O.numero,     ",
                            "        O.depto,      ",
                            "        O.colonia,    ",
                            "        D.deleg_desc, ",
                            "        O.codpos,     ",
                            "        O.pais_cod,   ",        ##op 15
                            "        E.estad_desc, ",
                            "        A.n_folio,    ",        ##op 15
                            "        A.tipo_solicitud",      ##op 15
                            " FROM   afi_mae_afiliado A, ",
                            "        nss_dom N,          ",
                            "        afi_domicilio O,    ",
                            " OUTER  tab_estado E,       ",
                            " OUTER  tab_delegacion D    ",
                            " WHERE  A.fentcons BETWEEN  ", "'",fecha_3,"'",
                            " AND    ", "'",fecha_2,"'",
                            " AND    A.n_seguro           = N.nss ",
                            " AND    A.n_seguro           = O.nss ",
                            " AND    A.n_folio            = O.n_folio ",
                            " AND    A.tipo_solicitud     = O.tipo_solicitud ",
                            " AND    A.tipo_solicitud     <> 5 ",
                            " AND    O.marca_envio        = ","'","X","'",
                            " AND    E.estad_cod          = O.estado ",
                            " AND    D.estad_cod          = O.estado ",
                            " AND    D.deleg_cod          = O.delega " 
        ELSE
            LET sel_where = " SELECT A.n_seguro,   ",
                            "        O.calle,      ",
                            "        O.numero,     ",
                            "        O.depto,      ",
                            "        O.colonia,    ",
                            "        D.deleg_desc, ",
                            "        O.codpos,     ",
                            "        O.pais_cod,   ",         ##op 15
                            "        E.estad_desc, ",
                            "        A.n_folio,    ",        ##op 15
                            "        A.tipo_solicitud",      ##op 15
                            " FROM   afi_mae_modifica A, ",
                            "        afi_domicilio O,    ",
                            " OUTER  tab_estado E,       ",
                            " OUTER  tab_delegacion D    ",
                            " WHERE  A.n_seguro           = O.nss            ",
                            " AND    A.n_folio            = O.n_folio        ",
                            " AND    A.tipo_solicitud     = O.tipo_solicitud ",
                            " AND    A.fecha_actualiza    IS NULL            ",
                            " AND    A.cod_operacion      = 12               ",
                            " AND    O.marca_envio        = ","'","X","'",
                            " AND    E.estad_cod          = O.estado         ",
                            " AND    D.estad_cod          = O.estado         ",
                            " AND    D.deleg_cod          = O.delega         "
        END IF

        LET sel_where = sel_where CLIPPED

        DISPLAY "PROCESANDO INFORMACION "

        LET comm = g_seg_modulo.ruta_envio CLIPPED,
                   "/M",HOY USING "yyyymmdd" CLIPPED,
                   consec CLIPPED, ".DOM" CLIPPED

        DISPLAY "Archivo : ", comm CLIPPED

        LET nom_taa = "M",HOYDIA USING "yyyymmdd" CLIPPED,
                      consec CLIPPED, ".DOM" CLIPPED

        START REPORT listado_d TO comm         --- circ 28-7

        PREPARE qry_genera FROM sel_where
        DECLARE curs_1 CURSOR FOR qry_genera

        FOREACH curs_1 INTO reg_dom.nss_afore,
                            reg_dom.calle,
                            reg_dom.no_ext,
                            reg_dom.no_int,
                            reg_dom.colonia,
                            reg_dom.delegacion,
                            reg_dom.cp,
                            reg_dom.pais,           ##op 15
                            reg_dom.ent_fed,
                            w_aux.n_folio,          ##op 15
                            w_aux.tipo_solicitud    ##op 15

            LET HAY_REGISTROS = TRUE

            LET num = num + 1
--->erm #op 15
            IF reg_dom.pais IS NOT NULL OR
               reg_dom.pais <> "   " THEN
               SELECT "X"
               FROM tab_pais
               WHERE pais_cod = reg_dom.pais

               IF SQLCA.SQLCODE = 100 THEN
                  LET reg_dom.pais = "MEX"
               END IF
            ELSE
               LET reg_dom.pais = "MEX"
            END IF

##################################################################################
            SELECT COUNT(*)
              INTO vtotal_tel
              FROM afi_telefono t
               WHERE  t.nss            = reg_dom.nss_afore
               AND    t.n_folio        = w_aux.n_folio
               AND    t.tipo_solicitud = w_aux.tipo_solicitud
               AND    t.tel_cod        NOT IN (7)

               LET vtel1 = "0000000000"
               LET vind_tel1  = NULL
               LET vexten1    = NULL
               LET vtel2 = "0000000000"
               LET vind_tel2  = NULL
               LET vexten2    = NULL
               LET vcont_tel  = 0

            IF vtotal_tel = 0 THEN

            ELSE
               DECLARE curs_2 CURSOR FOR
               SELECT DISTINCT(t.nss),t.n_folio,t.tipo_solicitud,t.pais_cod,
                               t.cve_lada, t.extension, t.telefono,t.tel_cod, 
                               t.usuario, t.factualiza
                 FROM afi_telefono t
                WHERE t.nss            = reg_dom.nss_afore
                  AND t.n_folio        = w_aux.n_folio
                  AND t.tipo_solicitud = w_aux.tipo_solicitud
                  AND t.tel_cod        NOT IN (7)
                ORDER BY t.tel_cod
               FOREACH curs_2 INTO reg_tel.*

                 IF vtotal_tel    = 1 THEN

                    IF reg_tel.tel_cod <> 4 THEN
                       IF reg_tel.cve_lada IS NOT NULL THEN
                          IF reg_tel.cve_lada[1] = 0 THEN
                             LET reg_tel.cve_lada = reg_tel.cve_lada[2,3] CLIPPED
                          END IF
                       END IF

                       LET vcc_tel   = vcc_tel + 1
                       LET vtel1   = reg_tel.cve_lada CLIPPED,
                                     reg_tel.telefono  CLIPPED
                       LET vexten1   = reg_tel.extension CLIPPED
                       LET ban_tel   = 1
                    END IF
            #VALIDA CELULAR 1
                    IF reg_tel.tel_cod = 4 THEN
                       IF reg_tel.cve_lada IS NOT NULL THEN
                          IF reg_tel.cve_lada[1] = 0 THEN
                             LET reg_tel.cve_lada = reg_tel.cve_lada[2,3] CLIPPED
                          END IF
                       END IF
                       LET vtel1 = reg_tel.cve_lada CLIPPED,
                                   reg_tel.telefono  CLIPPED
                       LET vtam  = LENGTH(vtel1 CLIPPED)

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
                       LET vind_tel1  = NULL
                    END IF
                    EXIT FOREACH
                 ELSE
                    LET vcont_tel = vcont_tel + 1

                    IF vcont_tel = 1 THEN
                       IF reg_tel.tel_cod <> 4 THEN
                          IF reg_tel.cve_lada IS NOT NULL THEN
                             IF reg_tel.cve_lada[1] = 0 THEN
                                LET reg_tel.cve_lada = reg_tel.cve_lada[2,3]
                             END IF
                          END IF
                          LET vtel1   = reg_tel.cve_lada CLIPPED,
                                        reg_tel.telefono  CLIPPED
                          LET vexten1 = reg_tel.extension CLIPPED
                          LET ban_tel = 1
                          LET vcc_tel = vcc_tel + 1
                       ELSE
                          IF reg_tel.tel_cod = 4 THEN
                             IF reg_tel.cve_lada IS NOT NULL THEN
                                IF reg_tel.cve_lada[1] = 0 THEN
                                   LET reg_tel.cve_lada = reg_tel.cve_lada[2,3] CLIPPED
                                END IF
                             END IF
                             LET vtel1 = reg_tel.cve_lada CLIPPED,
                                         reg_tel.telefono  CLIPPED
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
                             LET vind_tel1  = NULL
                          END IF
                       END IF
                    ELSE
                       IF reg_tel.tel_cod <> 4 THEN
                          IF reg_tel.cve_lada[1] = 0 THEN
                             LET reg_tel.cve_lada = reg_tel.cve_lada[2,3]
                          END IF
                          LET vtel2   = reg_tel.cve_lada CLIPPED,
                                        reg_tel.telefono  CLIPPED
                          LET vexten2   = reg_tel.extension CLIPPED
                        END IF

                          #VALIDA CELULAR 2
                          IF reg_tel.tel_cod = 4 THEN
                             IF reg_tel.cve_lada IS NOT NULL THEN
                                IF reg_tel.cve_lada[1] = 0 THEN
                                   LET reg_tel.cve_lada = reg_tel.cve_lada[2,3] CLIPPED
                                END IF
                             END IF
                             LET vtel2 = reg_tel.cve_lada CLIPPED,
                                         reg_tel.telefono  CLIPPED
--                             LET vtel2 = reg_tel.telefono  CLIPPED
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

                          INITIALIZE reg_tel.tel_cod TO NULL
                          LET ban_tel = 1

                          IF (vtel1 <> vtel2)          OR
                             (vtel1 NOT MATCHES vtel2) THEN
                             LET vcont_tel = 0
                          ELSE
                             LET vtel2     = '0000000000'
                             LET vind_tel2 = NULL
                             LET vexten2   = NULL
                          END IF

                          EXIT FOREACH

                    INITIALIZE reg_tel.tel_cod TO NULL
                    END IF
                 END IF
               END FOREACH
            END IF

            IF (vtel1 IS NULL) OR
               (vtel1 MATCHES "[ *]")THEN
                  LET vtel1 = "0000000000"
            END IF

            IF (vtel2 IS NULL) OR
               (vtel2 MATCHES "[ *]") THEN
                  LET vtel2 = "0000000000"
            END IF
#################################################################################

            LET cont_correo = 0
            INITIALIZE vcorreo_elect TO NULL
            DECLARE c_correo CURSOR FOR
               SELECT F.correo_elect, F.cod_correo_e
               FROM   afi_correo_elect F
               WHERE  F.nss            = reg_dom.nss_afore
               AND    F.n_folio        = w_aux.n_folio
               AND    F.tipo_solicitud = w_aux.tipo_solicitud
--               AND   F.cod_correo_e IN (1,2)
               ORDER BY 2
            FOREACH c_correo INTO vcorreo_elect,vcod_correo_e

               LET cont_correo = cont_correo + 1
               IF cont_tel = 1 THEN
                  EXIT FOREACH
               END IF
           END FOREACH

---<

            --OUTPUT TO REPORT listado_d(reg_dom.*)
						OUTPUT TO REPORT listado_d(reg_dom.*,vtel1,vtel2,
                                       vind_tel1,vind_tel2,vcorreo_elect)     ---erm #op 15

            IF generar = 2 THEN
                UPDATE afi_mae_modifica
                SET    fecha_actualiza = TODAY
                WHERE  n_seguro        = reg_dom.nss_afore
                AND    fecha_actualiza IS NULL
                AND    cod_operacion   = 12
            END IF
        END FOREACH

        FINISH REPORT listado_d

        CALL limpia_nulos()
    ELSE
        IF bnd_proceso THEN
            DISPLAY "program stopped, NO HAY SOLICITUDES A ENVIAR"
        END IF
    END IF

END FUNCTION

REPORT listado_d(reg_dom,fono1,fono2,pref1,pref2,correo_e) #op 15 SALVADOR
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
				pais        CHAR(3),                ##op 15
        ent_fed     CHAR(65)
    END RECORD

DEFINE        
        fono1       CHAR(10),               ##op 15
        fono2       CHAR(10),               ##op 15
        fono3       CHAR(10),               ##op 15
        pref1       CHAR(3),                ##op 15
        pref2       CHAR(3),                ##op 15
        correo_e    CHAR(100),              ##op 15
        vcorreo     CHAR(40)                ##op 15

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
        var         INTEGER

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
                         "01",
                         "15",
                         "01",
                         reg_dom.cve_recep USING "&&&",
                         "03",
                         "001",
                         "09 ",
                         hoy_env,
                         h_corr  USING "&&&",  #CORRELATIVO DE ENVIO
                         g_opcion USING "&&",  #MEDIO POR DONDE SE ENVIA ARCHIVO
                         468 SPACES            #op 15

    ON EVERY ROW

        LET dia1   = DAY(TODAY)   USING"&&"
        LET mes1   = MONTH(TODAY) USING"&&"
        LET ano1   = YEAR(TODAY)  USING "&&&&"
        LET num10d = num          USING "&&&&&&&&&&"
				LET vcorreo = correo_e    CLIPPED # op 15 SALVADOR

        PRINT
            COLUMN 1,reg_dom.tipo_reg       ,
                     num10d                 ,
                     '15'                   ,
                     3 SPACES               ,
                     reg_dom.nss_afore      ,
                     reg_dom.calle          ,
                     reg_dom.no_ext         ,
                     reg_dom.no_int         ,
                     reg_dom.colonia        ,
                     reg_dom.delegacion     ,
                     reg_dom.cp             ,
                     reg_dom.ent_fed        ,
                     2 SPACES               ,        #op 15 (codigo resultado operacion)
                     15 SPACES              ,        #op 15 (diagnostico del proceso)
                     reg_dom.pais           ,        #op 15 
                     pref1                  ,        #op 15 (indicador tipo telefono 1)
                     fono1 USING "&&&&&&&&&&",        #op 15 (telefono 1)       ---erm 22 Oct 2007 (+ using)
                     5 SPACES               ,        #op 15 (extension telefono 1)
                     pref2                  ,        #op 15 (indicador tipo telefono 2)
                     fono2 USING "&&&&&&&&&&",        #op 15 (telefono 2)       ---erm 22 Oct 2007 (+ using)
                     5 SPACES               ,        #op 15 (extension telefono 2)
                     vcorreo                ,        #op 15 (correo electronico)
                     81 SPACES                       #op 15 

    ON LAST ROW
        LET tot = 0
        LET tot = count(*)
        LET tot_char = tot USING "&&&&&&&&&"

        PRINT COLUMN 001, "09"      ,
                          tot_char  ,
                          489 SPACES                 ##op 15

END REPORT

FUNCTION limpia_nulos()
#ln--------------------

    LET comm = "cd " ,g_seg_modulo.ruta_envio CLIPPED,
               "/ ; sed -e '/^$/d' ","M",HOYDIA USING "yyyymmdd" CLIPPED,
               consec CLIPPED,".DOM > informix" 
    RUN comm

    LET comm = "cd " ,g_seg_modulo.ruta_envio CLIPPED,
               "/ ; mv informix  ","M",HOYDIA USING "yyyymmdd" CLIPPED,
               consec CLIPPED,".DOM " 
    RUN comm

END FUNCTION

REPORT rep_dom_masivo()
#rr-------------------

   DEFINE reg_no_cte RECORD
       nss   CHAR(11),
       tipo  CHAR(15)
   END RECORD

   OUTPUT
      PAGE   LENGTH     90
      TOP    MARGIN      0
      BOTTOM MARGIN      0
      LEFT   MARGIN      0
      RIGHT  MARGIN      0

   FORMAT
      FIRST PAGE HEADER
         PRINT COLUMN 03,"CVE AFORE ", g_afore.codigo_afore USING "&&&",
                         "  ",g_afore.razon_social ,
               COLUMN 68, hoy USING "DD-MM-YYYY"
         SKIP 2 LINE
         PRINT COLUMN 07,"--------------------------------------------------------------------"
         PRINT COLUMN 13,"CIFRAS CONTROL ARCHIVO MASIVO DE DOMICILIOS DE AFILIADOS"
         PRINT COLUMN 07,"--------------------------------------------------------------------"
         SKIP 2 LINE

      ON EVERY ROW
         PRINT COLUMN 10, "TOTAL REGISTROS "
         SKIP 2 LINE

         PRINT COLUMN 10, "DOMICILIOS A SER ENVIADOS : ", vsoli_env
                          USING "#######&"
         PRINT COLUMN 10, "DOMICILIOS GENERADOS      : ", num 
                          USING "#######&"

         SKIP 2 LINE

     {
      ON LAST ROW
          PRINT COLUMN 10, "TOTAL REGISTROS NO ENCONTRADOS EN LA B.D. : ",
                            tot_no_cte USING "#####&"
          SKIP 2 LINE
          PRINT COLUMN 10, "NSS",
                COLUMN 25, "DESCRIPCION"
          PRINT COLUMN 07,"--------------------------------------------------------------------"
          PRINT

          DECLARE cur_no_cte CURSOR FOR
          SELECT *
          FROM   nss_no_cte_afi
          ORDER BY 2,1

          FOREACH cur_no_cte INTO reg_no_cte.*

          PRINT COLUMN 10,reg_no_cte.nss,
                COLUMN 25,reg_no_cte.tipo

          END FOREACH

          CLOSE cur_no_cte

          FREE cur_no_cte
         }

END REPORT

