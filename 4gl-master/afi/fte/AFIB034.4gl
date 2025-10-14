###########################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                        #
#Propietario       => E.F.P.                                              #
#Programa AFIB034  => GENERACION DE ARCHIVO NOTIFICACION DE DOMICILIOS    #
#Sistema           => AFI                                                 #
#Autor             => FERNANDO HERRERA HERNANDEZ                          #
#Fecha             => 02 DE FEBRERO DE 2007                               #
#Modifica          => 04 SEPTIEMBRE 2007                                  #
#Autor Modifica    => EDUARDO J. RESENDIZ MEDINA  HOMOLOGACION OP 15 - 17 #
###########################################################################

DATABASE safre_af

GLOBALS

    DEFINE g_reg RECORD
        generar           CHAR(1)
    END RECORD

    DEFINE fecha_ini      DATE,
           fecha_fin      DATE,
           fecha_ini2     DATE,      --ene 11
           fecha_fin2     DATE       --ene 11

    DEFINE w_aux  RECORD
        n_seguro          LIKE afi_mae_afiliado.n_seguro          ,
        n_unico           LIKE afi_mae_afiliado.n_unico           ,
        tipo_solicitud    LIKE afi_mae_afiliado.tipo_solicitud    ,
        n_folio           LIKE afi_mae_afiliado.n_folio           ,
        documento_6       LIKE afi_mae_afiliado.documento_6
    END RECORD
 
    DEFINE k_aux  RECORD
        tipo_reg          CHAR(02) ,
        con_dservicio     CHAR(10) ,
        clave_doperacion  CHAR(02) ,
        fech_recep_afor   CHAR(08) ,
        folio_solicitud   CHAR(10)
    END RECORD

    DEFINE 
        HOY               DATE ,
        HOYDIA            DATE ,
        fecha_hasta       DATE

    DEFINE
        num               INTEGER   ,
        HAY_REGISTROS     INTEGER   ,
        HAY_REGISTROS1    INTEGER   ,
        h_corr            SMALLINT  ,
        h_corrd           SMALLINT  ,
        bnd_proceso       SMALLINT  ,
        diaSemana         SMALLINT  ,
        vsoli_env         INTEGER   ,
        vtotal_trasp      INTEGER   ,   --11 ene
        vtotal_reg        INTEGER       --11 ene

    DEFINE
        enter             CHAR(1)   ,
        g_opcion          CHAR(2)   ,
        g_usuario         CHAR(8)   ,
        HORA              CHAR(8)   ,
        consec            CHAR(10)  ,
        consecd           CHAR(10)  ,
        G_LISTA           CHAR(100) ,
        G_LISTA1          CHAR(100) ,
        G_LISTA2          CHAR(100) ,
        G_LISTA3          CHAR(100) ,
        nom_mdom          CHAR(500) ,
        comm              CHAR(500) ,
        list_salida       CHAR(500)

    DEFINE x_lotes        RECORD LIKE tab_lote.*
    DEFINE g_afore        RECORD LIKE tab_afore_local.*
    DEFINE g_seg_modulo   RECORD LIKE seg_modulo.*

    DEFINE reg_bat RECORD
        pid               INTEGER,
        proceso_cod       INTEGER,
        opera_cod         INTEGER,
        nombre_archivo    CHAR(25)
    END RECORD

    DEFINE reg_dom RECORD
        tipo_reg          CHAR(2),
        cont_serv         DECIMAL(10,0),
        tipo_recep        CHAR(2),
        cve_recep         CHAR(3),
        nss_afore         CHAR(11),
        curp_afore        CHAR(18),
        tipo_trabajador   CHAR(01),
        calle             CHAR(65),
        no_ext            CHAR(15),
        no_int            CHAR(15),
        colonia           CHAR(65),
        delegacion        CHAR(65),
        cp                CHAR(5),
        ent_fed           CHAR(65),
        pais_cod          CHAR(3)
    END RECORD

    DEFINE reg_dom2 RECORD LIKE afi_domicilio.*

    DEFINE lote_dom       SMALLINT
    DEFINE vdelegacion    CHAR(65)
    DEFINE vent_fed       CHAR(65)

    DEFINE g_dom          CHAR(100)

    DEFINE rech_prom      SMALLINT
    DEFINE xx_fecha       DATE
    DEFINE diaSig         DATE 
    DEFINE numrep         CHAR(10)

    DEFINE vtot_img       INTEGER
    DEFINE vdiv_img       INTEGER
    DEFINE vgrado_afo     CHAR(25)
    DEFINE venv_img       INTEGER

    DEFINE reg_tel        RECORD LIKE afi_telefono.*

    DEFINE vtel1          CHAR(15),
           vtel2          CHAR(15)
    DEFINE vcont_tel      SMALLINT
    DEFINE vcc_tel        INTEGER
    DEFINE vtotal_tel     INTEGER
    DEFINE tam1           SMALLINT
    DEFINE i              SMALLINT
    DEFINE ntel1          CHAR(15)
    DEFINE ntel_val1      CHAR(15)
    DEFINE tam2           SMALLINT
    DEFINE j              SMALLINT
    DEFINE ntel2          CHAR(15)
    DEFINE ntel_val2      CHAR(15)
    DEFINE vpref1         CHAR(3)         ---erm op 17
    DEFINE vpref2         CHAR(3)         ---erm op 17
    DEFINE vtotal_correo  SMALLINT        ---erm op 17
    DEFINE vcorreo        CHAR(100)       ---erm op 17
    DEFINE reg_correo     RECORD LIKE afi_correo_elect.*      ---erm op 17
    DEFINE vcont_correo   SMALLINT        ---erm op 17
    define cont_acep      SMALLINT

END GLOBALS

MAIN

    DISPLAY " "
    DISPLAY ".1"

    CALL STARTLOG('AFIB034.log')
    CALL inicio()

    IF NOT bnd_proceso THEN
        DEFER INTERRUPT
        OPTIONS INPUT WRAP,
        PROMPT LINE LAST,
        ACCEPT KEY CONTROL-I

        CALL proceso_principal()   #pp
    ELSE
        #CALL fecha_max(3)
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

    SELECT *
    INTO   g_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = 'afi'

    SELECT *,USER
    INTO   g_afore.*,g_usuario 
    FROM   tab_afore_local

    LET g_reg.generar          = "S"
    LET k_aux.tipo_reg         = "02"  
    LET k_aux.con_dservicio    = "          "
    LET k_aux.clave_doperacion = "01" 
    LET k_aux.fech_recep_afor  = "       " 
    LET k_aux.folio_solicitud  = "          "
    LET g_opcion               = "02"

    LET reg_dom.tipo_reg       = '02'
    LET reg_dom.tipo_recep     = '01'
    LET reg_dom.cve_recep      =  g_afore.codigo_afore USING '&&&'

    LET HOY                    = TODAY
    LET HORA                   = TIME
    LET HORA                   = HORA[1,2], HORA[4,5], HORA[7,8]
    LET num                    = 0
    LET numrep                 = 1
    LET HOYDIA                 = TODAY

    LET venv_img               = 0
    LET vcont_tel              = 0
    LET vcc_tel                = 0
    LET tam1                   = 0
    LET i                      = 0
    LET tam2                   = 0
    LET j                      = 0
    LET vcont_correo           = 0             ---erm op 17
    LET vtotal_correo          = 0             ---erm op 17
    LET cont_acep              = 0

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 4,4 WITH FORM "AFIB0341" ATTRIBUTE(BORDER)

    DISPLAY " AFIB034  GENERACION ARCHIVO NOTIFICACION DOMICILIOS OP.17                     " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                               < Ctrl-C > Salir                                " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)
    DISPLAY g_seg_modulo.ruta_envio AT 10,10

    INPUT BY NAME fecha_ini, fecha_fin,fecha_ini2, fecha_fin2     --11 ene
        AFTER FIELD fecha_ini
          IF fecha_ini IS NULL THEN
             ERROR "Campo NO puede ser NULO"
             NEXT FIELD fecha_ini
          ELSE
             NEXT FIELD fecha_fin
          END IF

        AFTER FIELD fecha_fin
          IF fecha_fin IS NULL THEN
             ERROR "Campo NO puede ser NULO"
             NEXT FIELD fecha_ini
          END IF
 -->ene 11
         AFTER FIELD fecha_ini2
          IF fecha_ini2 IS NULL THEN
             ERROR "Campo NO puede ser NULO"
             NEXT FIELD fecha_ini2
          ELSE
             NEXT FIELD fecha_fin2
          END IF

        AFTER FIELD fecha_fin2
          IF fecha_fin2 IS NULL THEN
             ERROR "Campo NO puede ser NULO"
             NEXT FIELD fecha_ini2
          END IF
--<ene 11
          IF fecha_ini > fecha_fin OR 
             fecha_ini2 > fecha_fin2 THEN       --ene 11
             ERROR "Fecha inicio NO puede ser mayor a la fecha fin."
             NEXT FIELD fecha_ini
          ELSE
             #CALL fecha_max(3)
             CALL rescata_valores()
             DISPLAY "GRADO CONTROL AFORE        ", vgrado_afo AT 12,30
             DISPLAY "SOLICITUDES A SER ENVIADAS ", vsoli_env
             USING ">&&&&&&&&" AT 13,30 
             DISPLAY "SOLICITUDES REGISTRO       ", vtotal_reg
             USING ">&&&&&&&&" AT 14,30
             --DISPLAY "SOLICITUDES CON ID IMG     ", vtot_img
             DISPLAY "SOLICITUDES TRASPASO       ", vtotal_trasp   --11ENE
             USING ">&&&&&&&&" AT 15,30
             DISPLAY "TOTAL SOLICITUDES CON TEL  ", vcc_tel
             USING ">&&&&&&&&" AT 16,30

             IF NOT HAY_REGISTROS THEN
                 ERROR "NO HAY REGISTROS PARA PROCESAR..."
                 SLEEP 3 
                 EXIT PROGRAM
             END IF

             EXIT INPUT
          END IF

          ON KEY ( INTERRUPT )
             ERROR "PROCESO CANCELADO" SLEEP 2
             EXIT PROGRAM
    END INPUT

    IF rech_prom = 0 THEN
        PROMPT "NOMBRE ARCHIVO PLANO : ", nom_mdom CLIPPED,
        " /[Enter] para salir" FOR enter
    ELSE
        PROMPT "NOMBRE ARCHIVO PLANO : ", nom_mdom CLIPPED,
        " CON ", rech_prom USING "####&",
        " REGISTROS CON PROMOTORES NO ACTIVOS, /[Enter] para salir"
        FOR enter
    END IF             

END FUNCTION

FUNCTION rescata_valores()
#rv-----------------------

    DEFINE recha SMALLINT

    LET HAY_REGISTROS = FALSE

    DECLARE c_cuantos CURSOR FOR
    #Liquidados diferentes Reg Inicial
    SELECT COUNT(*)
    FROM   afi_mae_afiliado A,
    OUTER  (afi_domicilio O,
    OUTER  tab_estado E,
    OUTER  tab_delegacion D)
    WHERE  A.n_seguro           = O.nss
    AND    A.n_folio            = O.n_folio
    AND    A.tipo_solicitud     = O.tipo_solicitud
    AND    A.tipo_solicitud    IN (3,6,7,8,10,11,12) 
    AND    A.finicta           >= fecha_ini 
    AND    A.finicta           <= fecha_fin
    AND    O.marca_envio        = "X"
    AND    E.estad_cod          = O.estado
    AND    D.estad_cod          = O.estado
    AND    D.deleg_cod          = O.delega

    UNION

    #Reg Inicial
    SELECT COUNT(*)
    FROM   afi_mae_afiliado A,
    OUTER  (afi_domicilio   O,
    OUTER  tab_estado       E,
    OUTER  tab_delegacion   D)
    WHERE  A.n_seguro           = O.nss
    AND    A.n_folio            = O.n_folio
    AND    A.tipo_solicitud     = O.tipo_solicitud
    AND    A.tipo_solicitud     = 1 
    AND    A.n_seguro         NOT IN (SELECT T.nss 
                                      FROM   taa_viv_recepcion T)
    AND    A.fentcons          >= fecha_ini 
    AND    A.fentcons          <= fecha_fin
    AND    O.marca_envio        = "X"
    AND    E.estad_cod          = O.estado
    AND    D.estad_cod          = O.estado
    AND    D.deleg_cod          = O.delega

    UNION

-->ene 11 TRASPASOS
    SELECT COUNT(*)
    FROM   afi_mae_afiliado A,
    OUTER  (afi_domicilio O,
    OUTER  tab_estado E,
    OUTER  tab_delegacion D)
    WHERE  A.n_seguro           = O.nss
    AND    A.n_folio            = O.n_folio
    AND    A.tipo_solicitud     = O.tipo_solicitud
    --AND    A.tipo_solicitud     = 2
    AND    A.tipo_solicitud     IN (2,9,15,16,18)
    AND    A.finicta           >= fecha_ini2
    AND    A.finicta           <= fecha_fin2
    AND    O.marca_envio        = "X"
    AND    E.estad_cod          = O.estado
    AND    D.estad_cod          = O.estado
    AND    D.deleg_cod          = O.delega
--<ene 11

    UNION

    #Reg Inicial asignados otra afore
    SELECT COUNT(*)
    FROM   afi_mae_afiliado A,
    OUTER  (afi_domicilio   O,
    OUTER  tab_estado       E,
    OUTER  tab_delegacion   D)
    WHERE  A.n_seguro           = O.nss
    AND    A.n_folio            = O.n_folio
    AND    A.tipo_solicitud     = O.tipo_solicitud
    AND    A.tipo_solicitud     = 1 
    AND    A.n_seguro          IN (SELECT T.nss 
                                   FROM   taa_viv_recepcion T
                                   WHERE  T.tipo_traspaso = 51)
    AND    A.finicta           >= fecha_ini 
    AND    A.finicta           <= fecha_fin
    AND    O.marca_envio        = "X"
    AND    E.estad_cod          = O.estado
    AND    D.estad_cod          = O.estado
    AND    D.deleg_cod          = O.delega

    UNION

    #Reenvio                          
    SELECT COUNT(*)
    FROM   afi_mae_afiliado A,
    OUTER  (afi_domicilio O,
    OUTER  tab_estado E,
    OUTER  tab_delegacion D)
    WHERE  A.n_seguro           = O.nss
    AND    A.n_folio            = O.n_folio
    AND    A.tipo_solicitud     = O.tipo_solicitud
    --AND    A.tipo_solicitud    IN (1,2,3,6,8,10,11,12) 
    AND    A.tipo_solicitud    IN (1,2,3,6,8,9,10,11,12,15,16,17,18)
    AND    A.documento_6        = 2
    AND    O.marca_envio        = "X"
    AND    E.estad_cod          = O.estado
    AND    D.estad_cod          = O.estado
    AND    D.deleg_cod          = O.delega

    FOREACH c_cuantos INTO HAY_REGISTROS1

      LET HAY_REGISTROS = HAY_REGISTROS + HAY_REGISTROS1

    END FOREACH

    IF HAY_REGISTROS THEN
       SELECT t.*
       INTO   x_lotes.*
       FROM   tab_lote t
       WHERE  t.lotes_cod = 17
       AND    lotes_fecha = HOYDIA

       IF SQLCA.SQLCODE = 0 THEN
          UPDATE tab_lote
          SET    lotes_correlativo = lotes_correlativo + 1
          WHERE  lotes_cod         = 17
          AND    lotes_fecha       = HOYDIA
       ELSE
          INSERT INTO tab_lote
          VALUES(HOYDIA              ,
                 17                  ,
                 'NOT DOM RECURRENTE',
                 1                   ,
                 1)
       END IF

       SELECT tl.lotes_correlativo
       INTO   h_corr
       FROM   tab_lote tl
       WHERE  tl.lotes_cod   = 17
       AND    tl.lotes_fecha = HOYDIA

       LET consec = h_corr

       UPDATE tab_lote
       SET    lotes_correlativo = lotes_correlativo + 1
       WHERE  lotes_cod         = 17
       AND    lotes_fecha       = HOYDIA

       SELECT l.lotes_correlativo
       INTO   h_corrd
       FROM   tab_lote l
       WHERE  l.lotes_cod   = 17
       AND    l.lotes_fecha = HOYDIA

       LET consecd = h_corrd

       LET vsoli_env = hay_registros

-->ene 11
    SELECT COUNT(*)
    INTO   vtotal_trasp
    FROM   afi_mae_afiliado A,
    OUTER  (afi_domicilio O,
    OUTER  tab_estado E,
    OUTER  tab_delegacion D)
    WHERE  A.n_seguro           = O.nss
    AND    A.n_folio            = O.n_folio
    AND    A.tipo_solicitud     = O.tipo_solicitud
    --AND    A.tipo_solicitud     = 2
    AND    A.tipo_solicitud     IN (2,9,15,16,18)
    AND    A.finicta           >= fecha_ini2
    AND    A.finicta           <= fecha_fin2
    AND    O.marca_envio        = "X"
    AND    E.estad_cod          = O.estado
    AND    D.estad_cod          = O.estado
    AND    D.deleg_cod          = O.delega

       DISPLAY "                                                           ", vgrado_afo AT 12,2
       DISPLAY "                                                           ", vgrado_afo AT 13,2
       DISPLAY "                                                                           "  AT 14,1
       DISPLAY "                                                           ", vgrado_afo AT 15,2
       DISPLAY "                                                           ", vgrado_afo AT 16,2
--<ene 11

       IF NOT bnd_proceso THEN
          LET vtotal_reg = vtotal_trasp - vsoli_env           --11 ene
          DISPLAY "GRADO CONTROL AFORE        ", vgrado_afo AT 12,30
          DISPLAY "SOLICITUDES A SER ENVIADAS ", vsoli_env
          USING ">&&&&&&&&" AT 13,30
          DISPLAY "SOLICITUDES REGISTRO       ", vtotal_reg
          USING ">&&&&&&&&" AT 14,30
          --DISPLAY "SOLICITUDES CON ID IMG     ", vtot_img
          DISPLAY "SOLICITUDES TRASPASO       ", vtotal_trasp   --11ENE
          USING ">&&&&&&&&" AT 15,30
          DISPLAY "TOTAL SOLICITUDES CON TEL  ", vcc_tel
          USING ">&&&&&&&&" AT 16,30
       ELSE
          LET vtotal_reg = vtotal_trasp - vsoli_env             --11 ene
          DISPLAY "GRADO CONTROL AFORE        ", vgrado_afo AT 12,20
          DISPLAY "SOLICITUDES A SER ENVIADAS ", vsoli_env
          USING ">&&&&&&&&" AT 13,30
          DISPLAY "SOLICITUDES REGISTRO       ", vtotal_reg
          USING ">&&&&&&&&" AT 14,30
          --DISPLAY "SOLICITUDES CON ID IMG     ", vtot_img
          DISPLAY "SOLICITUDES TRASPASO       ", vtotal_trasp   --11ENE
          USING ">&&&&&&&&" AT 15,30
          DISPLAY "TOTAL SOLICITUDES CON TEL  ", vcc_tel
          USING ">&&&&&&&&" AT 16,30
       END IF

       LET HAY_REGISTROS = FALSE

       DECLARE curs_1 CURSOR FOR
       #Liquidados diferentes Reg Inicial
       SELECT A.n_seguro       ,
              A.n_unico        ,
              A.tipo_solicitud ,
              A.n_folio        ,
              A.documento_6    ,
              O.calle          ,
              O.numero         ,
              O.depto          ,
              O.colonia        ,
              D.deleg_desc     ,
              NVL(O.codpos,0)  ,
              E.estad_desc     ,
              O.pais_cod       ,
              O.*
       FROM   afi_mae_afiliado A,
       OUTER (afi_domicilio    O,
       OUTER  tab_estado       E,
       OUTER  tab_delegacion   D)
       WHERE  A.n_seguro           = O.nss
       AND    A.n_folio            = O.n_folio
       AND    A.tipo_solicitud     = O.tipo_solicitud
       AND    A.tipo_solicitud    IN (3,6,7,8,10,11,12) 
       AND    A.finicta           >= fecha_ini
       AND    A.finicta           <= fecha_fin
       AND    O.marca_envio        = "X"
       AND    E.estad_cod          = O.estado
       AND    D.estad_cod          = O.estado
       AND    D.deleg_cod          = O.delega

       UNION
 
       #Reg Inicial
       SELECT A.n_seguro       ,
              A.n_unico        ,
              A.tipo_solicitud ,
              A.n_folio        ,
              A.documento_6    ,
              O.calle          ,
              O.numero         ,
              O.depto          ,
              O.colonia        ,
              D.deleg_desc     ,
              NVL(O.codpos,0)  ,
              E.estad_desc     ,
              O.pais_cod       ,
              O.*
       FROM   afi_mae_afiliado A,
       OUTER (afi_domicilio    O,
       OUTER  tab_estado       E,
       OUTER  tab_delegacion   D)
       WHERE  A.n_seguro           = O.nss
       AND    A.n_folio            = O.n_folio
       AND    A.tipo_solicitud     = O.tipo_solicitud
       AND    A.tipo_solicitud     = 1 
       AND    A.n_seguro         NOT IN (SELECT T.nss
                                         FROM   taa_viv_recepcion T)
       AND    A.fentcons          >= fecha_ini
       AND    A.fentcons          <= fecha_fin
       AND    O.marca_envio        = "X"
       AND    E.estad_cod          = O.estado
       AND    D.estad_cod          = O.estado
       AND    D.deleg_cod          = O.delega

       UNION

        #TAA
        SELECT A.n_seguro       ,
              A.n_unico        ,
              A.tipo_solicitud ,
              A.n_folio        ,
              A.documento_6    ,
              O.calle          ,
              O.numero         ,
              O.depto          ,
              O.colonia        ,
              D.deleg_desc     ,
              NVL(O.codpos,0)  ,
              E.estad_desc     ,
              O.pais_cod       ,
              O.*
       FROM   afi_mae_afiliado A,
       OUTER (afi_domicilio    O,
       OUTER  tab_estado       E,
       OUTER  tab_delegacion   D)
       WHERE  A.n_seguro           = O.nss
       AND    A.n_folio            = O.n_folio
       AND    A.tipo_solicitud     = O.tipo_solicitud
       --AND    A.tipo_solicitud     = 2
       AND    A.tipo_solicitud     IN (2,9,15,16,18)
       AND    A.finicta           >= fecha_ini2
       AND    A.finicta           <= fecha_fin2
       AND    O.marca_envio        = "X"
       AND    E.estad_cod          = O.estado
       AND    D.estad_cod          = O.estado
       AND    D.deleg_cod          = O.delega

       UNION

       #Reg Inicial asignados otra afore
       SELECT A.n_seguro       ,
              A.n_unico        ,
              A.tipo_solicitud ,
              A.n_folio        ,
              A.documento_6    ,
              O.calle          ,
              O.numero         ,
              O.depto          ,
              O.colonia        ,
              D.deleg_desc     ,
              NVL(O.codpos,0)  ,
              E.estad_desc     ,
              O.pais_cod       ,
              O.*
       FROM   afi_mae_afiliado A,
       OUTER (afi_domicilio    O,
       OUTER  tab_estado       E,
       OUTER  tab_delegacion   D)
       WHERE  A.n_seguro           = O.nss
       AND    A.n_folio            = O.n_folio
       AND    A.tipo_solicitud     = O.tipo_solicitud
       AND    A.tipo_solicitud     = 1 
       AND    A.n_seguro           IN (SELECT T.nss
                                       FROM   taa_viv_recepcion T
                                       WHERE  T.tipo_traspaso = 51)
       AND    A.finicta           >= fecha_ini
       AND    A.finicta           <= fecha_fin
       AND    O.marca_envio        = "X"
       AND    E.estad_cod          = O.estado
       AND    D.estad_cod          = O.estado
       AND    D.deleg_cod          = O.delega

       UNION

       #Reenvio
       SELECT A.n_seguro       ,
              A.n_unico        ,
              A.tipo_solicitud ,
              A.n_folio        ,
              A.documento_6    ,
              O.calle          ,
              O.numero         ,
              O.depto          ,
              O.colonia        ,
              D.deleg_desc     ,
              NVL(O.codpos,0)  ,
              E.estad_desc     ,
              O.pais_cod       ,
              O.*
       FROM   afi_mae_afiliado A,
       OUTER (afi_domicilio    O,
       OUTER  tab_estado       E,
       OUTER  tab_delegacion   D)
       WHERE  A.n_seguro           = O.nss
       AND    A.n_folio            = O.n_folio
       AND    A.tipo_solicitud     = O.tipo_solicitud
       --AND    A.tipo_solicitud    IN (1,2,3,6,8,10,11,12)
       AND    A.tipo_solicitud    IN (1,2,3,6,8,9,10,11,12,15,16,17,18) 
       AND    A.documento_6        = 2
       AND    O.marca_envio        = "X"
       AND    E.estad_cod          = O.estado
       AND    D.estad_cod          = O.estado
       AND    D.deleg_cod          = O.delega

       DISPLAY "PROCESANDO INFORMACION "

       LET comm = g_seg_modulo.ruta_envio CLIPPED,
                  "/E",HOYDIA USING "yyyymmdd" CLIPPED,
                  consec CLIPPED, ".DOM_OP17" CLIPPED

       DISPLAY "Archivo : ", comm CLIPPED

       LET nom_mdom = "E",HOYDIA USING "yyyymmdd" CLIPPED,
                      consec CLIPPED, ".DOM_OP17" CLIPPED

       START REPORT listado_d TO comm

       FOREACH curs_1 INTO w_aux.*,
                           reg_dom.calle,
                           reg_dom.no_ext,
                           reg_dom.no_int,
                           reg_dom.colonia,
                           reg_dom.delegacion,
                           reg_dom.cp,
                           reg_dom.ent_fed,
                           reg_dom.pais_cod,
                           reg_dom2.*

         LET HAY_REGISTROS         = TRUE
         LET reg_dom.nss_afore     = w_aux.n_seguro
         LET reg_dom.curp_afore    = w_aux.n_unico

         IF (w_aux.tipo_solicitud = 8)  OR 
            (w_aux.tipo_solicitud = 12) OR
            (w_aux.tipo_solicitud = 15) OR
            (w_aux.tipo_solicitud = 16) OR
            (w_aux.tipo_solicitud = 17) OR
            (w_aux.tipo_solicitud = 18) THEN
            LET reg_dom.nss_afore       = NULL
            LET reg_dom.tipo_trabajador = "2"
         ELSE
            LET reg_dom.tipo_trabajador = "1"
         END IF

         IF (w_aux.tipo_solicitud = 11) OR
            (w_aux.tipo_solicitud = 12) THEN
            SELECT a.calle,
                   a.numero,
                   a.depto,
                   a.codpos,
                   a.colonia,
                   a.delega,
                   #a.estado,
                   b.estad_desc,
                   #a.pais,
                   c.pais_desc
            INTO   reg_dom.calle,
                   reg_dom.no_ext,
                   reg_dom.no_int,
                   reg_dom.cp,
                   reg_dom.colonia,
                   reg_dom.delegacion,
                   reg_dom.ent_fed,
                   reg_dom.pais_cod
            FROM   afi_det_reg_internet a,
                   tab_estado b,
                   tab_pais c
            WHERE  a.n_seguro = w_aux.n_seguro 
            AND    a.n_folio  = w_aux.n_folio
            AND    a.estado   = b.estad_cod
            AND    a.pais     = c.pais_cod
         END IF

         IF (w_aux.tipo_solicitud = 9) OR
            (w_aux.tipo_solicitud = 16) OR
            (w_aux.tipo_solicitud = 18) THEN
            SELECT a.calle,
                   a.numero,
                   a.depto,
                   a.codpos,
                   a.colonia,
                   a.delega,
                   #a.estado,
                   b.estad_desc,
                   #a.pais,
                   c.pais_desc
            INTO   reg_dom.calle,
                   reg_dom.no_ext,
                   reg_dom.no_int,
                   reg_dom.cp,
                   reg_dom.colonia,
                   reg_dom.delegacion,
                   reg_dom.ent_fed,
                   reg_dom.pais_cod
            FROM   afi_det_internet a,
                   tab_estado b,
                   tab_pais c
            WHERE  a.nss      = w_aux.n_seguro 
            AND    a.n_folio  = w_aux.n_folio
            AND    a.estado   = b.estad_desc
            AND    c.pais_cod = 'MEX'
         END IF

         SELECT COUNT(*)
           INTO vtotal_tel
           FROM afi_telefono t
          WHERE t.nss            = w_aux.n_seguro
            AND t.n_folio        = w_aux.n_folio
            AND t.tipo_solicitud = w_aux.tipo_solicitud
            --AND t.tel_cod        NOT IN (4,7)
            AND t.tel_cod        NOT IN (7)
            LET vtel1  = "000000000000000"
            LET vtel2  = "000000000000000"
            INITIALIZE vpref1 TO NULL
            INITIALIZE vpref2 TO NULL
         IF vtotal_tel = 0 THEN
            LET vtel1  = "0000000000"
            LET vtel2  = "0000000000"
         ELSE
            DECLARE curs_2 CURSOR FOR
            SELECT t.*
              FROM afi_telefono t
             WHERE t.nss            = w_aux.n_seguro
               AND t.n_folio        = w_aux.n_folio
               AND t.tipo_solicitud = w_aux.tipo_solicitud
               --AND t.tel_cod        NOT IN (4,7)
               AND t.tel_cod        NOT IN (7)            ---erm op 17
            ORDER BY t.tel_cod
            FOREACH curs_2 INTO reg_tel.*
   
              IF vtotal_tel = 1 THEN

                 #QUITA - y " "
                 LET tam1 = LENGTH(reg_tel.telefono CLIPPED)
                 FOR i = 1 TO tam1
                   IF (reg_tel.telefono[i,i] <> "-") AND
                      (reg_tel.telefono[i,i] <> " ") THEN
                      --LET ntel1 = ntel1 CLIPPED, reg_tel.telefono[i,i]             ---erm op 17
                      IF reg_tel.tel_cod <> 4 THEN                                   ---erm op 17          
                         LET ntel1 = ntel1 CLIPPED,reg_tel.telefono[i,i]             ---erm op 17
                      ELSE                                                           ---erm op 17
                         LET ntel1 = ntel1 CLIPPED, reg_tel.telefono[i,i]            ---erm op 17
                      END IF                                                         ---erm op 17
                   END IF
                 END FOR
                 #QUITA - y " "

                    IF reg_tel.tel_cod = 4 THEN
                       IF reg_tel.cve_lada IS NOT NULL THEN
                          IF reg_tel.cve_lada[1] = 0 THEN
                             LET reg_tel.cve_lada = reg_tel.cve_lada[2,3] CLIPPED
                          END IF
                       END IF
                       LET ntel1 = reg_tel.cve_lada CLIPPED, ntel1
                       LET tam1  = LENGTH(ntel1)
                       IF tam1 = 10 THEN
                          IF (ntel1[1,2] = '55') THEN
                             LET vpref1  = '044'
                             LET ntel1   = ntel1 CLIPPED
                          ELSE
                             LET vpref1  = '045'
                             LET ntel1   = ntel1 CLIPPED
                          END IF
                       END IF
                       IF tam1 = 13 THEN
                          IF (ntel1[4,5] = '55') THEN
                             LET vpref1  = '044'
                             LET ntel1   = ntel1[4,tam1]   #antes tam2
                          ELSE
                             LET vpref1  = '045'
                             LET ntel1   = ntel1[4,tam1]   #antes tam2
                          END IF
                       END IF

                    ELSE

                       IF reg_tel.cve_lada IS NOT NULL THEN
                          IF reg_tel.cve_lada[1] = 0 THEN
                             LET reg_tel.cve_lada = reg_tel.cve_lada[2,3] CLIPPED
                          END IF
                       END IF
                       INITIALIZE vpref1 TO NULL          ---erm 23 oct 2007

                       LET ntel1 = reg_tel.cve_lada CLIPPED, ntel1
                    END IF

                 #Valida criterios
                 CALL val_criterio(ntel1) RETURNING ntel1
                 #Valida criterios

                 LET vtel1   = ntel1 CLIPPED
                 LET ntel1   = NULL
                 EXIT FOREACH
              ELSE
                 LET vcont_tel = vcont_tel + 1
    
                 IF vcont_tel = 1 THEN
                    #QUITA - y " "
                    LET tam1 = LENGTH(reg_tel.telefono CLIPPED)
                    FOR i = 1 TO tam1
                      IF (reg_tel.telefono[i,i] <> "-") AND
                         (reg_tel.telefono[i,i] <> " ") THEN
                         --LET ntel1 = ntel1 CLIPPED, reg_tel.telefono[i,i]              ---erm op 17
                          IF reg_tel.tel_cod <> 4 THEN                                   ---erm op 17          
                             LET ntel1 = ntel1 CLIPPED, reg_tel.telefono[i,i]            ---erm op 17
                          ELSE                                                           ---erm op 17
                             LET ntel1 = ntel1 CLIPPED, reg_tel.telefono[i,i]            ---erm op 17
                          END IF                                                         ---erm op 17
                      END IF
                    END FOR
                    #QUITA - y " "
--->erm op 17
                    IF reg_tel.tel_cod = 4 THEN
                       IF reg_tel.cve_lada IS NOT NULL THEN
                          IF reg_tel.cve_lada[1] = 0 THEN
                             LET reg_tel.cve_lada = reg_tel.cve_lada[2,3] CLIPPED
                          END IF
                       END IF
                       LET ntel1 = reg_tel.cve_lada CLIPPED, ntel1
                       LET tam1  = LENGTH(ntel1)
                       IF tam1 = 10 THEN
                          IF (ntel1[1,2] = '55') THEN
                             LET vpref1  = '044'
                             LET ntel1   = ntel1 CLIPPED
                          ELSE
                             LET vpref1  = '045'
                             LET ntel1   = ntel1 CLIPPED
                          END IF
                       END IF
                       IF tam1 = 13 THEN
                          IF (ntel1[4,5] = '55') THEN
                             LET vpref1  = '044'
                             LET ntel1   = ntel1[4,tam1]
                          ELSE
                             LET vpref1  = '045'
                             LET ntel1   = ntel1[4,tam1]
                          END IF
                       END IF

                    ELSE

                       IF reg_tel.cve_lada IS NOT NULL THEN
                          IF reg_tel.cve_lada[1] = 0 THEN
                             LET reg_tel.cve_lada = reg_tel.cve_lada[2,3] CLIPPED
                          END IF
                       END IF
                       INITIALIZE vpref1 TO NULL              ---erm 23 oct 2007

                       LET ntel1 = reg_tel.cve_lada CLIPPED, ntel1
                    END IF
---<
                    #Valida criterios
                    CALL val_criterio(ntel1) RETURNING ntel1
                    #Valida criterios

                    IF ntel1 = "000000000000000" THEN
                       LET vcont_tel = 0
                       LET ntel1     = NULL                    ---erm op 17
                       --LET ntel1 = "0000000000"                  ---erm op 17
                       CONTINUE FOREACH
                    END IF

                    LET vtel1   = ntel1 CLIPPED
                    LET ntel1   = NULL
                 ELSE
                    #QUITA - y " "
                    LET tam2 = LENGTH(reg_tel.telefono CLIPPED)
                    FOR j = 1 TO tam2
                      IF (reg_tel.telefono[j,j] <> "-") AND
                         (reg_tel.telefono[j,j] <> " ") THEN
                         --LET ntel2 = ntel2 CLIPPED, reg_tel.telefono[j,j]      ---erm op 17
                          IF reg_tel.tel_cod <> 4 THEN                                   ---erm op 17          
                             LET ntel2 = ntel2 CLIPPED, reg_tel.telefono[j,j]            ---erm op 17
                          ELSE                                                           ---erm op 17
                             LET ntel2 = ntel2 CLIPPED, reg_tel.telefono[j,j]            ---erm op 17
                          END IF                                                         ---erm op 17
                      END IF
                    END FOR
                    #QUITA - y " "
--->erm op 17
                    IF reg_tel.tel_cod = 4 THEN
                       IF reg_tel.cve_lada IS NOT NULL THEN
                          IF reg_tel.cve_lada[1] = 0 THEN
                             LET reg_tel.cve_lada = reg_tel.cve_lada[2,3] CLIPPED
                          END IF
                       END IF
                       LET ntel2 = reg_tel.cve_lada CLIPPED, ntel2
                       LET tam2  = LENGTH(ntel2)
                       IF tam2 = 10 THEN
                          IF (ntel2[1,2] = '55') THEN
                             LET vpref2  = '044'
                             LET ntel2   = ntel2 CLIPPED
                          ELSE
                             LET vpref2  = '045'
                             LET ntel2   = ntel2 CLIPPED
                          END IF
                       END IF
                       IF tam2 = 13 THEN
                          IF (ntel2[4,5] = '55') THEN
                             LET vpref2  = '044'
                             LET ntel2   = ntel2[4,tam2]
                          ELSE
                             LET vpref2  = '045'
                             LET ntel2   = ntel2[4,tam2]
                          END IF
                       END IF

                    ELSE
                     --->erm 23 Oct 2007
                       IF reg_tel.cve_lada IS NOT NULL THEN
                          IF reg_tel.cve_lada[1] = 0 THEN
                             LET reg_tel.cve_lada = reg_tel.cve_lada[2,3] CLIPPED
                          END IF
                       END IF
                       INITIALIZE vpref2 TO NULL          ---erm 23 oct 2007
                     ---<
                       LET ntel2 = reg_tel.cve_lada CLIPPED, ntel2
                    END IF
---<
                    #Valida criterios
                    CALL val_criterio(ntel2) RETURNING ntel2
                    #Valida criterios

                    IF ntel2 = "000000000000000" THEN
                       LET ntel2 = NULL                     ---op 17
                       --LET ntel2 = "0000000000"               ---op 17
                       CONTINUE FOREACH
                    END IF
 
                    LET vtel2 = ntel2 CLIPPED
                    LET ntel2 = NULL
                    IF (vtel1 <> vtel2)          OR 
                       (vtel1 NOT MATCHES vtel2) THEN
                       LET vcont_tel = 0
                       EXIT FOREACH
                    ELSE
                       --LET vtel2 = "000000000000000"      ---erm op 17
                       LET vtel2 = "0000000000"             ---erm op 17
                    END IF
                 END IF
              END IF
            END FOREACH
         END IF

--->erm op 17

         SELECT COUNT(*)
           INTO vtotal_correo
           FROM afi_correo_elect t
          WHERE t.nss            = w_aux.n_seguro
            AND t.n_folio        = w_aux.n_folio
            AND t.tipo_solicitud = w_aux.tipo_solicitud
            AND t.marca_envio    = 'X'

         IF vtotal_correo = 0 THEN
            INITIALIZE vcorreo  TO NULL
         ELSE
            DECLARE curs_correo CURSOR FOR
            SELECT t.*
              FROM afi_correo_elect t
             WHERE t.nss            = w_aux.n_seguro
               AND t.n_folio        = w_aux.n_folio
               AND t.tipo_solicitud = w_aux.tipo_solicitud
               AND t.marca_envio    = 'X'
            FOREACH curs_correo INTO reg_correo.*

              IF vtotal_correo = 1 THEN
                  LET vcorreo = reg_correo.correo_elect
                  EXIT FOREACH
              ELSE
                 LET vcont_correo = vcont_correo + 1
              END IF
            END FOREACH
         END IF

            IF (vcorreo IS NULL) OR
               (vcorreo MATCHES "[ *]") THEN
                  INITIALIZE vcorreo TO NULL
            END IF
---<

         IF (vtel1 IS NULL) OR
	    (vtel1 = "")     THEN
	    --LET vtel1 = '000000000000000'       ---erm op 17
	    LET vtel1 = '0000000000'              ---erm op 17
	    INITIALIZE vpref1 TO NULL             ---erm 23 Oct 2007
	 END IF
	 
	 IF (vtel2 IS NULL) OR
	    (vtel2 = "")    THEN
	    --LET vtel2 = '000000000000000'       ---erm op 17
	    LET vtel2 = '0000000000'              ---erm op 17
	    INITIALIZE vpref2 TO NULL             ---erm 23 Oct 2007
         END IF

         IF (vtel1 <> '0000000000') OR       ---erm op 17 (de 15 a 10 posiciones)
            (vtel2 <> '0000000000') THEN     ---erm op 17 (de 15 a 10 posiciones)
            LET vcc_tel = vcc_tel + 1
         END IF

         IF (vtel1  = '0000000000') AND      ---erm op 17 (de 15 a 10 posiciones)
            (vtel2 <> '0000000000') THEN     ---erm op 17 (de 15 a 10 posiciones)
            LET vtel1 = vtel2
            LET vtel2 = '0000000000'         ---erm op 17 (de 15 a 10 posiciones)
         END IF
--->erm op 17
         IF (vpref1 IS NULL) OR
            (vpref1 MATCHES "[ *]") THEN
               INITIALIZE vpref1 TO NULL
         END IF

         IF (vpref2 IS NULL) OR
            (vpref2 MATCHES "[ *]") THEN
               INITIALIZE vpref2 TO NULL
         END IF
---<
         IF (reg_dom.calle IS NULL)        OR
            (reg_dom.calle MATCHES "[ *]") THEN
            LET reg_dom.calle = "N/A"
         END IF 
   
         IF (reg_dom.cp IS NULL)        OR
            (reg_dom.cp MATCHES "[ *]") OR 
            (reg_dom.cp = 0)            THEN
             LET reg_dom.cp = "N/A"
         END IF

         IF (reg_dom.colonia IS NULL)        OR
            (reg_dom.colonia MATCHES "[ *]") THEN
            LET reg_dom.colonia = "N/A"
         END IF

         IF (reg_dom.delegacion IS NULL)        OR
            (reg_dom.delegacion MATCHES "[ *]") THEN
            LET reg_dom.delegacion = "N/A"
         END IF

         IF (reg_dom.ent_fed IS NULL)        OR
            (reg_dom.ent_fed MATCHES "[ *]") THEN
            LET reg_dom.ent_fed = "N/A"
         END IF

         IF (reg_dom.pais_cod IS NULL)        OR
            (reg_dom.pais_cod MATCHES "[ *]") THEN
            LET reg_dom.pais_cod = "MEX"
         ELSE
            SELECT 'X'
              FROM tab_pais
             WHERE pais_cod = reg_dom.pais_cod
            IF STATUS = NOTFOUND THEN
               LET reg_dom.pais_cod = 'MEX'
            END IF
         END IF

         IF (reg_dom.no_ext IS NULL)        OR
            (reg_dom.no_ext MATCHES "[ *]") THEN
            LET reg_dom.no_ext = 'N/A'
         END IF

         IF (reg_dom.no_int IS NULL)        OR
            (reg_dom.no_int MATCHES "[ *]") THEN
            LET reg_dom.no_int = 'N/A'
         END IF

         LET cont_acep = cont_acep + 1

         IF cont_acep = 9998 THEN
            SELECT * 
            INTO   x_lotes.* 
            FROM   tab_lote
            WHERE  lotes_cod   = 17
            AND    lotes_fecha = HOYDIA
 
            IF SQLCA.SQLCODE = 0 THEN
 
                SELECT lotes_correlativo
                INTO   h_corr 
                FROM   tab_lote
                WHERE  lotes_cod   = 17
                AND    lotes_fecha = HOYDIA
 
                LET consec = h_corr
 
                UPDATE tab_lote 
                SET    lotes_correlativo = lotes_correlativo + 1
                WHERE  lotes_cod         = 17
                AND    lotes_fecha       = HOYDIA
            END IF
 
            FINISH REPORT listado_d
 
            LET cont_acep = 0
            --LET consec = h_corr
            LET comm = g_seg_modulo.ruta_envio CLIPPED,
                       "/E",HOYDIA USING "yyyymmdd" CLIPPED,
                       consec CLIPPED, ".DOM_OP17" CLIPPED
                                 
            START REPORT listado_d   TO comm
         END IF

         OUTPUT TO REPORT listado_d(reg_dom.*, vtel1, vtel2,vpref1,vpref2,vcorreo)

         IF w_aux.documento_6 = 2 THEN                         ---erm 24 Mayo 2007
            {UPDATE afi_ctr_domicilio                           ---erm 24 Mayo 2007
            SET    status_interno = 31,                        ---erm 24 Mayo 2007
                   usuario        = g_usuario,                 ---erm 24 Mayo 2007
                   factualiza     = TODAY                      ---erm 24 Mayo 2007
            WHERE  nss            = w_aux.n_seguro             ---erm 24 Mayo 2007
            AND    n_folio        = w_aux.n_folio              ---erm 24 Mayo 2007
            AND    tipo_solicitud = w_aux.tipo_solicitud       ---erm 24 Mayo 2007}
            INSERT INTO afi_ctr_domicilio 
            VALUES ( w_aux.n_seguro,                --nss
                     w_aux.n_unico,                 --curp
                     w_aux.n_folio,                 --n_folio
                     w_aux.tipo_solicitud,          --tipo_solicitud
                     reg_dom2.calle,                --calle
                     reg_dom2.numero,               --numero exterior
                     reg_dom2.depto,                --numero interior
                     reg_dom2.colonia,              --colonia
                     reg_dom2.delega,               --delegacion
                     reg_dom2.ciudad,               --ciudad
                     reg_dom2.estado,               --estado
                     reg_dom2.codpos,               --codigo_postal
                     reg_dom2.dom_cod,              --codigo_domicilio
                     reg_dom2.pais_cod,             --codigo_pais
                     reg_dom2.marca_envio,          --marca_envio 
                     #reg_dom2.tipo_comp_dom,        --tipo_comprobante_domicilio
                     #reg_dom2.fecha_comp_dom,       --fecha_comprobante_domicilio
                     "",                           --tipo_comprobante_domicilio
                     "",                           --fecha_comprobante_domicilio
                     vpref1,                        --ind_tipo_tel1     ---erm op 17
                     vtel1,                         --telefono 1
                     "",                            --extension1        ---erm op 17
                     vpref2,                        --ind_tipo_tel2     ---erm op 17
                     vtel2,                         --telefono 2
                     "",                            --extension2        ---erm op 17
                     reg_dom.tipo_trabajador,       --tipo_trabajador
                     TODAY,                         --fecha_envio
                     "",                            --codigo_resultado
                     "",                            --diagnostico_proceso
                     31,                            --status_interno
                     g_usuario,                     --usuario
                     TODAY )                        --fecha_actualizacion
         ELSE                                                  ---erm 24 Mayo 2007
            INSERT INTO afi_ctr_domicilio 
            VALUES ( w_aux.n_seguro,                --nss
                     w_aux.n_unico,                 --curp
                     w_aux.n_folio,                 --n_folio
                     w_aux.tipo_solicitud,          --tipo_solicitud
                     reg_dom2.calle,                --calle
                     reg_dom2.numero,               --numero exterior
                     reg_dom2.depto,                --numero interior
                     reg_dom2.colonia,              --colonia
                     reg_dom2.delega,               --delegacion
                     reg_dom2.ciudad,               --ciudad
                     reg_dom2.estado,               --estado
                     reg_dom2.codpos,               --codigo_postal
                     reg_dom2.dom_cod,              --codigo_domicilio
                     reg_dom2.pais_cod,             --codigo_pais
                     reg_dom2.marca_envio,          --marca_envio 
                     #reg_dom2.tipo_comp_dom,        --tipo_comprobante_domicilio
                     #reg_dom2.fecha_comp_dom,       --fecha_comprobante_domicilio
                     "",                           --tipo_comprobante_domicilio
                     "",                           --fecha_comprobante_domicilio
                     vpref1,                        --ind_tipo_tel1     ---erm op 17
                     vtel1,                         --telefono 1
                     "",                            --extension1        ---erm op 17
                     vpref2,                        --ind_tipo_tel2     ---erm op 17
                     vtel2,                         --telefono 2
                     "",                            --extension2        ---erm op 17
                     reg_dom.tipo_trabajador,       --tipo_trabajador
                     TODAY,                         --fecha_envio
                     "",                            --codigo_resultado
                     "",                            --diagnostico_proceso
                     30,                            --status_interno
                     g_usuario,                     --usuario
                     TODAY )                        --fecha_actualizacion
         END IF                                               ---erm 24 Mayo 2007

         IF w_aux.documento_6 = 2 THEN                        ---erm 24 Mayo 2007
            UPDATE afi_mae_afiliado                           ---erm 24 Mayo 2007
            SET    documento_6    = 4                         ---erm 24 Mayo 2007
            WHERE  n_folio        = w_aux.n_folio             ---erm 24 Mayo 2007
            AND    tipo_solicitud = w_aux.tipo_solicitud      ---erm 24 Mayo 2007
         ELSE                                                 ---erm 24 Mayo 2007
            UPDATE afi_mae_afiliado 
            SET    documento_6    = 1
            WHERE  n_folio        = w_aux.n_folio
            AND    tipo_solicitud = w_aux.tipo_solicitud
         END IF                                               ---erm 24 Mayo 2007
   
         LET num       = num    + 1
         LET numrep    = numrep + 1

         LET vtel1     = "000000000000000"
         LET vtel2     = "000000000000000"
         LET vcont_tel = 0

         DISPLAY "SOLICITUDES PROCEDENTES    ", num USING ">&&&&&&&&" AT 17,30 

       END FOREACH

       FINISH REPORT listado_d

       CALL limpia_nulos()

    ELSE
       IF bnd_proceso THEN
          DISPLAY "Program stopped, NO HAY SOLICITUDES A ENVIAR"
       END IF
    END IF

END FUNCTION

REPORT listado_d(reg_dom, ltel1, ltel2,prefijo1,prefijo2,correo)
#------------------------
    DEFINE reg_dom RECORD
        tipo_reg          CHAR(2),
        cont_serv         DECIMAL(10,0),
        tipo_recep        CHAR(2),
        cve_recep         CHAR(3),
        nss_afore         CHAR(11),
        curp_afore        CHAR(18),
        tipo_trabajador   CHAR(01),
        calle             CHAR(65),
        no_ext            CHAR(15),
        no_int            CHAR(15),
        colonia           CHAR(65),
        delegacion        CHAR(65),
        cp                CHAR(5),
        ent_fed           CHAR(65),
        pais_cod          CHAR(3)
    END RECORD

    DEFINE
        d                 CHAR(02)  ,
        m                 CHAR(02)  ,
        a                 CHAR(04)

    DEFINE
        dia               CHAR(02)  ,
        mes               CHAR(02)  ,
        ano               CHAR(04)  ,
        dia1              CHAR(02)  ,
        mes1              CHAR(02)  ,
        ano1              CHAR(04)

    DEFINE 
        hoy_env           CHAR(8),
        tot_char          CHAR(9),
        num10d            CHAR(10)

    DEFINE
        tot               ,
        cod_err_ori       ,
        var               INTEGER

    DEFINE 
       ltel1              CHAR(15),
       ltel2              CHAR(15)

    DEFINE 
       --rtel1              DECIMAL(15,0),
       --rtel2              DECIMAL(15,0)
       rtel1              CHAR(10),
       rtel2              CHAR(10)
--->erm op 17
    DEFINE
       prefijo1           CHAR(3),
       prefijo2           CHAR(3),
       correo             CHAR(100),
       vcorreo            CHAR(50)
---<

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
                       "17",
                       "01",
                       reg_dom.cve_recep USING "&&&",
                       "03",
                       "001",
                       hoy_env,
                       h_corrd USING "&&&",  #CORRELATIVO DE ENVIO
                       473 SPACES

    ON EVERY ROW
      LET dia1   = DAY(TODAY)   USING"&&"
      LET mes1   = MONTH(TODAY) USING"&&"
      LET ano1   = YEAR(TODAY)  USING "&&&&"
      LET num10d = numrep       USING "&&&&&&&&&&"
      LET rtel1  = ltel1        USING "&&&&&&&&&&"          ---erm 23 oct 2007 (using)
      LET rtel2  = ltel2        USING "&&&&&&&&&&"          ---erm 23 oct 2007 (using)
      LET vcorreo = correo      CLIPPED

--->erm 10 Agosto 2007
      IF (reg_dom.pais_cod IS NULL)      OR
         (reg_dom.pais_cod MATCHES " *") OR
         (reg_dom.pais_cod = "N/A")      THEN
            LET reg_dom.pais_cod = "MEX"
      END IF
---<

      PRINT COLUMN 001,reg_dom.tipo_reg             ,
                       "01"                         ,
                       reg_dom.tipo_trabajador      ,
                       reg_dom.nss_afore            ,
                       reg_dom.curp_afore           ,
                       reg_dom.calle                ,
                       reg_dom.no_ext               ,
                       reg_dom.no_int               ,
                       reg_dom.colonia              ,
                       reg_dom.delegacion           ,
                       reg_dom.cp                   ,
                       reg_dom.ent_fed              ,
                       reg_dom.pais_cod             ,
                       prefijo1 USING "&&&"         ,            ---erm op 17
                       rtel1 USING "&&&&&&&&&&"     ,            ---erm op 17 (de 15 a 10 posiciones)
                       5 SPACES                     ,            ---erm op 17 (extension)
                       prefijo2 USING "&&&"         ,            ---erm op 17
                       rtel2 USING "&&&&&&&&&&"     ,            ---erm op 17 (de 15 a 10 posiciones)
                       5 SPACES                     ,            ---erm op 17 (extension)
                       vcorreo                      ,            ---erm op 17
                       82 SPACES

    ON LAST ROW
      LET tot = 0
      LET tot = count(*)
      LET tot_char = tot USING "&&&&&&&&&"

      PRINT COLUMN 001,"09"      ,
                       tot_char  ,
                       489 SPACES

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
               consec CLIPPED,".DOM_OP17 > informix" 
    RUN comm

    LET comm = "cd " ,g_seg_modulo.ruta_envio CLIPPED,
               "/ ; mv informix  ","E",HOYDIA USING "yyyymmdd" CLIPPED,
               consec CLIPPED,".DOM_OP17 " 
    RUN comm

END FUNCTION

FUNCTION fecha_max(vadel)
#fh-------------------

    DEFINE diaSemana SMALLINT
    DEFINE vadel     SMALLINT

    LET diaSemana = WEEKDAY(HOY)

    LET fecha_hasta = HOY - diaSemana UNITS DAY

    CALL fechas(fecha_hasta,3) RETURNING HOYDIA

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
       nom_archivo = nom_mdom
WHERE pid         = reg_bat.pid
and   proceso_cod = reg_bat.proceso_cod
and   opera_cod   = reg_bat.opera_cod

UPDATE bat_ctr_proceso
set    folio       = NULL ,      
       estado_cod  = 4    ,
       fecha_fin   = CURRENT,
       nom_archivo = nom_mdom
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

FUNCTION fechas(diaActual,vdias_habil)
#sdh----------------------

    DEFINE
        diaTmp      DATE,
        contador    SMALLINT,
        diaActual   DATE,
        numDias     SMALLINT,
	vdias_habil SMALLINT

    LET diaTmp = diaActual

    FOR contador = 1 TO vdias_habil
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

FUNCTION val_criterio(vtel)
#vc-------------------------
  DEFINE vtel CHAR(15)
  DEFINE rtel CHAR(15)
  DEFINE vtam SMALLINT
  DEFINE vf   SMALLINT

  LET vtam = 0
  LET vf   = 0

 
  LET vtam = LENGTH(vtel CLIPPED)

  #Valida longitud
  IF (vtam <> 10) AND
     (vtam <> 12) AND
     (vtam <> 13) THEN
     LET rtel = "000000000000000"
     RETURN rtel
  ELSE 
    
    #Valida letras
    FOR vf = 1 TO vtam
        IF (vtel[vf,vf] = 'A') OR  (vtel[vf,vf] = 'B') OR
           (vtel[vf,vf] = 'C') OR  (vtel[vf,vf] = 'D') OR
           (vtel[vf,vf] = 'E') OR  (vtel[vf,vf] = 'F') OR
           (vtel[vf,vf] = 'G') OR  (vtel[vf,vf] = 'H') OR
           (vtel[vf,vf] = 'I') OR  (vtel[vf,vf] = 'J') OR
           (vtel[vf,vf] = 'K') OR  (vtel[vf,vf] = 'L') OR
           (vtel[vf,vf] = 'M') OR  (vtel[vf,vf] = 'N') OR
           (vtel[vf,vf] = 'Ñ') OR  (vtel[vf,vf] = 'O') OR
           (vtel[vf,vf] = 'P') OR  (vtel[vf,vf] = 'Q') OR
           (vtel[vf,vf] = 'R') OR  (vtel[vf,vf] = 'S') OR
           (vtel[vf,vf] = 'T') OR  (vtel[vf,vf] = 'U') OR
           (vtel[vf,vf] = 'V') OR  (vtel[vf,vf] = 'W') OR
           (vtel[vf,vf] = 'X') OR  (vtel[vf,vf] = 'Y') OR
           (vtel[vf,vf] = 'Z') THEN
           LET rtel = "000000000000000"
        END IF
    END FOR

    IF (rtel = "000000000000000") THEN
       RETURN rtel 
    END IF
    #Valida letras

    #Valida 10 posiciones
    IF vtam = 10 THEN
       LET rtel = vtel
       RETURN rtel
    END IF
    #Valida 10 posiciones

    #Valida 12 posiciones
    IF vtam = 12 THEN
       IF (vtel[1,2] = '01') THEN
          LET rtel = vtel[3,12]
       ELSE
          LET rtel = "000000000000000"
       END IF
       RETURN rtel
    END IF
    #Valida 12 posiciones

    #Valida 13 posiciones
    IF vtam = 13 THEN
       IF (vtel[1,3] = '044') THEN
          LET rtel = vtel[4,13]
       ELSE
          LET rtel = "000000000000000"
       END IF
       RETURN rtel
    END IF
    #Valida 13 posiciones

  END IF

END FUNCTION
