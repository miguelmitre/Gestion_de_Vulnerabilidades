############################################################################
#Propietario       => E.F.P.                                               #
#Programa AFIC030  => RECIBE ARCHIVOS OPERACION 52 NOTIFICACION DE CUENTAS #
#                     PROCESO REGISTRO POR INTERNET                        #
#Fecha creacion    => 19 DE JUNIO DE 2006                                  #
#Por               => FERNANDO HERRERA HERNANDEZ                           #
#Sistema           => AFI                                                  #
#Actualizacion     => Eduardo Resendiz Medina   12-SEP-2011 NUEVO LAYOUT   #
############################################################################

DATABASE safre_af

GLOBALS

    DEFINE reg_cza_internet     RECORD
        tipo_registro           CHAR(2)       ,
        ident_servicio          CHAR(2)       ,
        ident_operacion         CHAR(2)       ,
        tipo_ent_origen         CHAR(2)       ,
        cve_ent_origen          CHAR(3)       ,
        tipo_ent_destino        CHAR(2)       ,
        cve_ent_destino         CHAR(3)       ,
        fecha_transferencia     CHAR(8)       ,
        entidad_federativa      CHAR(3)       ,         #12092011
        consec_lote_dia         SMALLINT      ,
        fecha_presentacion      DATE
    END RECORD

    DEFINE reg_det_internet     RECORD
        tipo_registro           CHAR(2)       ,
        cont_servicio           DECIMAL(10,0) ,
        cve_operacion           CHAR(2)       ,
        n_folio                 DECIMAL(10,0) , #09122011
        n_seguro                CHAR(11)      ,
        curp                    CHAR(18)      ,
        n_rfc                   CHAR(13)      ,
        paterno                 CHAR(40)      ,
        materno                 CHAR(40)      ,
        nombres                 CHAR(40)      ,
        fena                    DATE          ,
        cod_operacion           CHAR(02)      ,
        fecha_rec_sol_elect     DATE          ,
        sexo                    SMALLINT      ,
        estadon                 SMALLINT      ,
        nacionalidad            CHAR(3)       ,
        tipo_trab               CHAR(1)       ,
        tipo_admon              CHAR(2)       ,
        calle                   CHAR(65)      ,
        numero                  CHAR(10)      ,
        depto                   CHAR(10)      ,
        colonia                 CHAR(65)      ,
        delega                  CHAR(50)      ,
        estado                  CHAR(2)       ,
        pais                    CHAR(3)       ,
        codpos                  CHAR(5)       ,
        tel_casa                CHAR(12)      ,
        correo_elect            CHAR(50)      ,
        profesion               CHAR(40)      ,
        actividad               CHAR(02)      ,
        paterno_ben1            CHAR(40)      ,
        materno_ben1            CHAR(40)      ,
        nombres_ben1            CHAR(40)      ,
        porcentaje1             CHAR(3)       ,
        paterno_ben2            CHAR(40)      ,
        materno_ben2            CHAR(40)      ,
        nombres_ben2            CHAR(40)      ,
        porcentaje2             CHAR(3)       ,
        paterno_ben3            CHAR(40)      ,
        materno_ben3            CHAR(40)      ,
        nombres_ben3            CHAR(40)      ,
        porcentaje3             CHAR(3)       ,
        tipo_reg_noa            CHAR(1)       ,
        fecha_cert              DATE          ,
        fecha_alta              DATE          ,
        fol_sol                 DECIMAL(10,0)
    END RECORD

    DEFINE reg_sum_internet     RECORD
        tipo_registro           CHAR(2)       ,
        ident_servicio          CHAR(2)       ,
        ident_operacion         CHAR(2)       ,
        reg_not_afil            CHAR(9)       , #09122011
        reg_not_no_afil         CHAR(9)       , #09122011
        tot_reg_not             CHAR(9)       ,   #09122011
        fecha_presentacion      DATE
    END RECORD

    DEFINE g_param_afi RECORD   LIKE seg_modulo.*

    DEFINE
        HOY                     ,
        d_fecha_cza             ,
        d_fecha_det             DATE

    DEFINE
        v_hora                  DATETIME HOUR TO SECOND 

    DEFINE
          enter                 CHAR(1)     ,
          genera                CHAR(25)    ,
          archivo_traspaso      CHAR(200)   ,
          v_fecha_cza           CHAR(8)     ,
          f_fecha_cza           CHAR(10)    ,
          v_fecha_det           CHAR(8)     ,
          f_fecha_det           CHAR(10)    ,
          v_fecha_fena          CHAR(8)     ,
          f_fecha_fena          CHAR(10)    ,
          v_fecha_sol           CHAR(8)     ,
          f_fecha_sol           CHAR(10)    ,
          v_folio               DECIMAL(8)  ,
          v_nombre              CHAR(120)   ,
          v_horario             CHAR(8)     ,
          f_horario             CHAR(10)    ,
          v_fecha_prim_afi      CHAR(8)     ,
          f_fecha_prim_afi      CHAR(10)    ,
          v_fecha_actualiza_sa  CHAR(8)     ,
          f_fecha_actualiza_sa  CHAR(10)    ,
          v_fecha_cert          CHAR(8)     ,
          f_fecha_cert          CHAR(10)    ,
          v_fecha_alta          CHAR(8)     ,
          f_fecha_alta          CHAR(10)    ,
          v_fecha_rec           CHAR(8)     ,
          f_fecha_rec           CHAR(10)

    DEFINE
        cuantos                 ,
        s_codigo_afore          ,
        v_delega                ,
        v_ciudad                ,
        v_estado                ,
        cont                    SMALLINT

    DEFINE g RECORD
        nss                     CHAR(11)     ,
        paterno                 CHAR(40)     ,
        materno                 CHAR(40)     ,
        nombres                 CHAR(40)
    END RECORD

    DEFINE
        g_usuario               CHAR(8)      ,
        vrazon_social           CHAR(25)     ,
        archivo_op              CHAR(100)    ,
        v_cod_correoe           CHAR(1)      ,
        v_correo_e              CHAR(100)    ,
        carga                   CHAR(150)    ,
        v_marca                 CHAR(1)      ,
        v_cod_telefono          CHAR(1)      ,
        v_telefono              CHAR(40)     ,
        folio                   INTEGER      ,
        v_salario               DECIMAL(7,2)

    DEFINE varchivo             CHAR(25)
    DEFINE vtipo_solicitud      SMALLINT 

    DEFINE ejecuta              CHAR(300)
    DEFINE v_aux_n_seguro       CHAR(11)
    DEFINE vactividad_cod       SMALLINT
    DEFINE vactividad_desc      CHAR(50)
    DEFINE vprofesion_cod       SMALLINT

    DEFINE sin_curp             SMALLINT
    DEFINE con_curp             SMALLINT

    DEFINE bnd_proceso          SMALLINT

    DEFINE 
      reg_carta                 RECORD LIKE safre_af:int_ctr_carta.*,
      consulta_carta            CHAR(120)

    DEFINE
      cont_reg                  SMALLINT,
      cont_reg_noa              SMALLINT,
      cont_reg_total            SMALLINT

    DEFINE diaSig            DATE
    DEFINE vpend             SMALLINT

END GLOBALS

MAIN

    DEFER INTERRUPT
    OPTIONS 
        INPUT WRAP,
        PROMPT LINE LAST  ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("AFIC030.log")
    CALL inicio()
    CALL proceso_principal()

END MAIN

FUNCTION proceso_principal()
#pp-------------------------

   OPEN WINDOW ventana_1 AT 4,4 WITH FORM "AFIC0231" ATTRIBUTE(BORDER)
   DISPLAY " AFIC030  RECEPCION ARCHIVO OP 52 REGISTRO POR INTERNET                        " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY "                           < Ctrl-C > Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

   INPUT BY NAME genera
     AFTER FIELD genera
       IF genera IS NULL THEN
          ERROR "Campo NO puede ser NULO"
          NEXT FIELD genera
       END IF

       SELECT @nombre_archivo
       INTO   varchivo
       FROM   afi_ctr_arh_internet
       WHERE  @nombre_archivo = genera

       IF STATUS <> NOTFOUND THEN
          ERROR "ARCHIVO YA PROCESADO"
          SLEEP 1
          ERROR " "
          INITIALIZE genera TO NULL
          CLEAR FORM
          NEXT FIELD genera
       END IF

       WHENEVER ERROR CONTINUE
          LET archivo_op = g_param_afi.ruta_rescate CLIPPED,
                           "/",genera CLIPPED

          LET carga = "echo 'LOAD FROM ", archivo_op CLIPPED,
                      " INSERT INTO safre_tmp:tmp_sol_cert_internet' | dbaccess safre_tmp"
          RUN carga

          SELECT count(*)
          INTO   cuantos
          FROM   safre_tmp:tmp_sol_cert_internet

          IF cuantos = 0 THEN
             DISPLAY  " NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO "
             AT 19,2 ATTRIBUTE(REVERSE)
             SLEEP 3
             NEXT FIELD genera
          ELSE
             EXIT INPUT
          END IF
       WHENEVER ERROR STOP

     ON KEY (INTERRUPT)
        DISPLAY "PROCESO CANCELADO" AT 19,1 ATTRIBUTE(REVERSE)
        SLEEP 2
        EXIT PROGRAM
   END INPUT

   ERROR "PROCESANDO INFORMACION "

   CALL validacion_previa() #vp
   CALL lee_archivo_plano() #lap
   CALL impresion_reporte() #ir
   CALL apertura_cuenta()

   INSERT INTO afi_ctr_arh_internet VALUES (genera, cont_reg_total, HOY)

   PROMPT "Proceso finalizado, [Enter] para salir" FOR enter

END FUNCTION

FUNCTION inicio()
#i---------------

    LET HOY             = TODAY
    LET v_hora          = CURRENT HOUR TO SECOND

    LET bnd_proceso     = 1

    LET cont_reg        = 0
    LET cont_reg_noa    = 0
    LET cont_reg_total  = 0


    SELECT codigo_afore, razon_social
    INTO   s_codigo_afore, vrazon_social
    FROM   tab_afore_local

    SELECT *, USER 
    INTO   g_param_afi.*, g_usuario
    FROM   seg_modulo
    WHERE  modulo_cod = 'afi'

    WHENEVER ERROR CONTINUE
       DATABASE safre_tmp
         DROP TABLE tmp_sol_cert_internet

         CREATE TABLE tmp_sol_cert_internet
         (n_registros CHAR(920))

       DATABASE safre_af
    WHENEVER ERROR STOP

END FUNCTION

FUNCTION validacion_previa()
#vp-------------------------
#vp Valida que los registros en sus primeras 2 posiciones contenga
# 01 cza, 02 det, 09 sumario

  DEFINE
    c2_tipo_registro      CHAR(2)

  DEFINE
    sw_1                  ,
    sw_2                  ,
    sw_9                  SMALLINT

  DECLARE cur_2 CURSOR FOR
  SELECT UNIQUE(n_registros[1,2])
  FROM   safre_tmp:tmp_sol_cert_internet

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
     PROMPT "SE RECHAZA EL LOTE. NO EXISTE ENCABEZADO" FOR enter
     EXIT PROGRAM
  END IF

  IF sw_2 = 0 THEN
     PROMPT "SE RECHAZA EL LOTE. NO EXISTEN REGISTROS DE DETALLE" FOR enter
     EXIT PROGRAM
  END IF

  IF sw_9 = 0 THEN
     PROMPT "SE RECHAZA EL LOTE. NO EXISTE SUMARIO" FOR enter
     EXIT PROGRAM
  END IF

END FUNCTION

FUNCTION lee_archivo_plano()
#lap------------------------

  DEFINE 
    cont                 SMALLINT

  DEFINE 
    carga_reg            CHAR(920) ,
    c2_ident_operacion   CHAR(2)

  DEFINE
    vstatus_interno      SMALLINT,
    long_telefono        SMALLINT,
    serie_lada           SMALLINT 


  DECLARE cur_1 CURSOR FOR
  SELECT  * 
  FROM    safre_tmp:tmp_sol_cert_internet

  LET cont  = 0
  LET vpend = 0
  LET c2_ident_operacion = ""

  FOREACH cur_1 INTO carga_reg
    LET cont = cont + 1

    #---ENCABEZADO ---#
    IF carga_reg[5,6] = "52" THEN
       IF cont = 1 THEN
          LET c2_ident_operacion = "52"
          LET reg_cza_internet.tipo_registro      = carga_reg[001,002] 
          LET reg_cza_internet.ident_servicio     = carga_reg[003,004]
          LET reg_cza_internet.ident_operacion    = carga_reg[005,006]
          LET reg_cza_internet.tipo_ent_origen    = carga_reg[007,008]
          LET reg_cza_internet.cve_ent_origen     = carga_reg[009,011]
          LET reg_cza_internet.tipo_ent_destino   = carga_reg[012,013]
          LET reg_cza_internet.cve_ent_destino    = carga_reg[014,016]
          LET v_fecha_cza                         = carga_reg[017,024]
          LET reg_cza_internet.entidad_federativa = carga_reg[025,027]
          LET reg_cza_internet.consec_lote_dia    = carga_reg[028,030]
          LET f_fecha_cza                         = v_fecha_cza[5,6],"/",
                                                    v_fecha_cza[7,8],"/",
                                                    v_fecha_cza[1,4] 
          LET reg_cza_internet.fecha_presentacion = f_fecha_cza

          INSERT INTO afi_cza_reg_internet VALUES(reg_cza_internet.*)
       END IF
    ELSE
       IF cont = 1 THEN
          ERROR "El identificador del proceso no es de Not Sol Reg por internet"
          SLEEP 3
          EXIT PROGRAM
       END IF
    END IF

    #---DETALLE---#
    IF carga_reg[1,2] = "02" AND c2_ident_operacion = "52" THEN
       LET reg_det_internet.tipo_registro        = carga_reg[001,002]
       LET reg_det_internet.cont_servicio        = carga_reg[003,012]
       LET reg_det_internet.cve_operacion        = carga_reg[013,014]
       LET reg_det_internet.n_seguro             = carga_reg[015,025]
       LET reg_det_internet.curp                 = carga_reg[026,043]
       LET reg_det_internet.n_rfc                = carga_reg[044,056]
       LET reg_det_internet.paterno              = carga_reg[057,096]
       LET reg_det_internet.materno              = carga_reg[097,136]
       LET reg_det_internet.nombres              = carga_reg[137,176]
       LET v_fecha_fena                          = carga_reg[177,184]
       LET reg_det_internet.cod_operacion        = carga_reg[185,186]
       LET v_fecha_rec                           = carga_reg[187,194]
       --LET reg_det_internet.fol_sol              = carga_reg[195,204] #12092011
       LET reg_det_internet.fol_sol              = ""                   #12092011
       LET reg_det_internet.sexo                 = carga_reg[205,205]
       LET reg_det_internet.estadon              = carga_reg[206,207]
       LET reg_det_internet.nacionalidad         = carga_reg[209,211]
       LET reg_det_internet.tipo_trab            = carga_reg[212,212]
       --LET reg_det_internet.tipo_admon           = carga_reg[213,214] #12092011
       LET reg_det_internet.tipo_admon           = ""                   #12092011
       LET reg_det_internet.calle                = carga_reg[215,279]
       LET reg_det_internet.numero               = carga_reg[280,289]
       LET reg_det_internet.depto                = carga_reg[290,299]
       LET reg_det_internet.colonia              = carga_reg[300,364]
       LET reg_det_internet.delega               = carga_reg[365,414]
       LET reg_det_internet.estado               = carga_reg[415,416]
       LET reg_det_internet.pais                 = carga_reg[417,419]
       LET reg_det_internet.codpos               = carga_reg[420,424]
       LET reg_det_internet.tel_casa             = carga_reg[425,436]
       LET reg_det_internet.correo_elect         = carga_reg[437,486]
       LET reg_det_internet.profesion            = ""    #12092011 carga_reg[487,526]
       LET reg_det_internet.actividad            = ""    #12092011 carga_reg[527,528]
       LET reg_det_internet.paterno_ben1         = ""    #12092011 carga_reg[529,568]
       LET reg_det_internet.materno_ben1         = ""    #12092011 carga_reg[569,608]
       LET reg_det_internet.nombres_ben1         = ""    #12092011 carga_reg[609,648]
       LET reg_det_internet.porcentaje1          = ""    #12092011 carga_reg[649,651]
       LET reg_det_internet.paterno_ben2         = ""    #12092011 carga_reg[652,691]
       LET reg_det_internet.materno_ben2         = ""    #12092011 carga_reg[692,731]
       LET reg_det_internet.nombres_ben2         = ""    #12092011 carga_reg[732,771]
       LET reg_det_internet.porcentaje2          = ""    #12092011 carga_reg[772,774]
       LET reg_det_internet.paterno_ben3         = ""    #12092011 carga_reg[775,814]
       LET reg_det_internet.materno_ben3         = ""    #12092011 carga_reg[815,854]
       LET reg_det_internet.nombres_ben3         = ""    #12092011 carga_reg[855,894]
       LET reg_det_internet.porcentaje3          = ""    #12092011 carga_reg[895,897]
       LET reg_det_internet.tipo_reg_noa         = ""    #12092011 carga_reg[899,899]}
       LET v_fecha_cert                          = carga_reg[900,907]
       LET v_fecha_alta                          = carga_reg[908,915]

       LET f_fecha_fena                          = v_fecha_fena[5,6],"/",
                                                   v_fecha_fena[7,8],"/",
                                                   v_fecha_fena[1,4] 
       LET reg_det_internet.fena                 = f_fecha_fena

       LET f_fecha_rec                           = v_fecha_rec[5,6],"/",
                                                   v_fecha_rec[7,8],"/",
                                                   v_fecha_rec[1,4] 
       LET reg_det_internet.fecha_rec_sol_elect  = f_fecha_rec

       LET f_fecha_cert                          = v_fecha_cert[5,6],"/",
                                                   v_fecha_cert[7,8],"/",
                                                   v_fecha_cert[1,4] 
       LET reg_det_internet.fecha_cert           = f_fecha_cert

       LET f_fecha_alta                          = v_fecha_alta[5,6],"/",
                                                   v_fecha_alta[7,8],"/",
                                                   v_fecha_alta[1,4] 
       LET reg_det_internet.fecha_alta           = f_fecha_alta

       LET vtipo_solicitud = NULL

       CASE reg_det_internet.tipo_trab
         WHEN 1
           LET vtipo_solicitud = 11
               IF reg_det_internet.cod_operacion  = "61" THEN
                  LET vpend = vpend + 1
                  LET cont_reg        = cont_reg + 1
               ELSE
                  LET cont_reg        = cont_reg + 1
               END IF
         WHEN 2
           LET vtipo_solicitud = 12
           LET cont_reg_noa    = cont_reg_noa + 1
   
           IF reg_det_internet.n_seguro IS NOT NULL OR
              reg_det_internet.n_seguro <> ""       THEN

           ELSE
              LET ejecuta = "EXECUTE PROCEDURE fn_obtiene_nti(",
                              "'", reg_det_internet.curp, "')"
              LET ejecuta = ejecuta CLIPPED

              PREPARE eje_obtiene FROM ejecuta
              DECLARE cur_obtiene CURSOR FOR eje_obtiene
              OPEN    cur_obtiene
              FETCH   cur_obtiene INTO v_aux_n_seguro
              CLOSE   cur_obtiene

              LET reg_det_internet.n_seguro = v_aux_n_seguro
           END IF

           INSERT INTO cta_ctr_reg_ind
                     VALUES (
                      reg_det_internet.curp            , #curp
                      reg_det_internet.n_seguro        , #n_seguro
                      ""                               , #nss_issste
                      reg_det_internet.tipo_reg_noa    , #tipo_trab_ind
                      reg_det_internet.tipo_admon      , #tipo_administracion
                      ""                               , #ind_deposito
                      HOY                              , #fecha_proceso
                      g_usuario                          #usuario
                      )

       END CASE

       LET v_folio = NULL

       SELECT MAX(n_folio)
       INTO   v_folio
       FROM   afi_solicitud
       WHERE  tipo_solicitud = vtipo_solicitud

       IF v_folio IS NULL THEN 
          LET v_folio = 1
       ELSE 
          LET v_folio = v_folio + 1
       END IF

       LET reg_det_internet.n_folio = v_folio

       INSERT INTO afi_det_reg_internet VALUES(reg_det_internet.*)

       CALL habil_siguiente(reg_det_internet.fecha_cert) RETURNING diaSig

       CASE reg_det_internet.cod_operacion 
         WHEN "01" LET vstatus_interno = 60
         WHEN "60" LET vstatus_interno = 60
         --WHEN "61" LET vstatus_interno = 70
         WHEN "61" LET vstatus_interno = 65
       END CASE

       INSERT INTO afi_solicitud
       VALUES (reg_det_internet.n_seguro           , --n_seguro
               reg_det_internet.curp               , --n_unico
               reg_det_internet.n_rfc              , --n_rfc
               reg_det_internet.paterno            , --paterno
               reg_det_internet.materno            , --materno
               reg_det_internet.nombres            , --nombres
               reg_det_internet.fena               , --fena
               v_folio                             , --v_folio
               0                                   , --edo_civil
               ""                                  , --localn
               reg_det_internet.estado             , --estadon
               0                                   , --tiptr
               "0000000000"                        , --cod_promotor
               reg_det_internet.sexo               , --sexo
               0                                   , --n_operac
               reg_det_internet.fecha_rec_sol_elect, --frecafor
               reg_det_internet.fecha_cert         , --fentcons
               ""                                  , --femision
               ""                                  , --finitmte
--               ""                                  , --finicta
               diaSig                              , --finicta
               reg_det_internet.cod_operacion      , --status
               0                                   , --agenc_cod
               vstatus_interno                     , --status_interno
               reg_det_internet.nacionalidad       , --nacionalidad
               6                                   , --tip_prob
               ""                                  , --fol_prob
               ""                                  , --doc_prob
               0                                   , --ind_infonavit
               ""                                  , --documento_1
               ""                                  , --documento_2
               ""                                  , --documento_3
               ""                                  , --documento_4
               ""                                  , --documento_5
               ""                                  , --documento_6
               ""                                  , --envio_dom
               ""                                  , --entidad_curp
               ""                                  , --asigna_curp
               ""                                  , --const_curp
               g_usuario                           , --usuario
               v_hora                              , --hora
               ""                                  , --status_captura
               vtipo_solicitud                     , --tipo_solicitud 
               reg_det_internet.fecha_rec_sol_elect, --fecha_elaboracion
               reg_cza_internet.consec_lote_dia    , --lote
               reg_det_internet.fecha_rec_sol_elect, --fecha_envio
               0                                   , --cod_esq_comision
               ""                                  , --ubicacion
               reg_det_internet.fecha_cert         , --fecha_1a_afil
               ""                                  , --indicador_c
               ""                                  , --indicador_d
               ""                                  , --indicador_e
               ""                                  , --cod_error_origen
               ""                                  , --folio_edo_cta
               ""                                  , --cod_afore_ced
               0                                   , --salario_base_comis
               0                                   , --salario_actual
               ""                                  , --fecha_actualiza_sa
               0                                   , --coduni_n1
               0                                   , --indicador_comision
               "0000000000"                        , --codven
               ""                                  , --cod_captura
               ""                                  , --lote_captura
               ""                                  , --folio_captura
               v_fecha_cert                        ) --sello_electronico

       LET v_delega = 0
       LET v_ciudad = 0
       LET v_estado = 0

       SELECT estad_cod,deleg_cod,ciudad_cod
       INTO   v_estado,v_delega,v_ciudad
       FROM   tab_codpos
       WHERE  cpos_cod = reg_det_internet.codpos

       IF reg_det_internet.pais IS NULL OR
          reg_det_internet.pais = "   " THEN 
          LET reg_det_internet.pais = "MEX"
       END IF

       INSERT INTO afi_domicilio
       VALUES (reg_det_internet.n_seguro           , --n_seguro
               v_folio                             , --n_folio
               vtipo_solicitud                     , --tipo_solicitud
               reg_det_internet.calle              , --calle
               ""                                  , --calle_ref1
               ""                                  , --calle_ref2
               reg_det_internet.numero             , --numero
               reg_det_internet.depto              , --depto
               reg_det_internet.colonia            , --colonia
               v_delega                            , --delega
               v_ciudad                            , --ciudad
               v_estado                            , --estado
               reg_det_internet.codpos             , --codpos
               1                                   , --dom_cod
               --"MEX"                                  , --pais_cod
               reg_det_internet.pais               , --pais_cod
               "X"                                 , --marca_envio
               --""                                  , --tipo_comp_dom   --coop
               --""                                  , --fecha_comp_dom  --coop
               g_usuario                           , --usuario
               HOY                                 ) --factualiza

       LET v_nombre = reg_det_internet.paterno CLIPPED," ",
                      reg_det_internet.materno CLIPPED," ",
                      reg_det_internet.nombres CLIPPED

       INSERT INTO afi_ctr_solicitud
       VALUES (vtipo_solicitud                     , --tipo_solicitud
               v_folio                             , --n_folio
               reg_det_internet.n_seguro           , --n_seguro
               v_nombre                            , --nombre_trab_afo
               vstatus_interno                     , --status_interno
               reg_det_internet.fecha_rec_sol_elect, --fecha_envio
               reg_det_internet.fecha_rec_sol_elect, --fecha_recepcion
               ""                                  , --cve_afore_ced
               ""                                  , --ind_nss_modif
               reg_det_internet.fecha_cert         , --fentcons
               ""                                  , --curp_oficial
               ""                                  , --ind_curp_modif
               ""                                  , --rfc_trab_bd
               ""                                  , --nombre_trab_bd
               ""                                  , --nombre_trab_pro
               ""                                  , --fena_trab_bd
               ""                                  , --codven_bd
               ""                                  , --sexo_bd
               ""                                  , --estadon_bd
               reg_det_internet.fecha_alta         , --fecha_prim_afi
               reg_det_internet.fecha_alta         , --fecha_alta_act
               ""                                  , --cve_afore_afi
               ""                                  , --nacion_bd
               ""                                  , --tipo_prob
               ""                                  , --folio_prob
               ""                                  , --doc_prob
               ""                                  , --ind_envio
               ""                                  , --ind_nombre
               ""                                  , --cod_operacion
               ""                                  , --diag_proceso
               ""                                  , --agenc_cod   ---pversion ara coppel HABILITAR
               g_usuario                           , --usuario
               HOY                                 ) --factualiza

       LET v_cod_telefono = NULL
       LET v_telefono     = NULL

       IF reg_det_internet.tel_casa = carga_reg[425,436] THEN
          LET v_cod_telefono = "1"
          LET v_telefono = reg_det_internet.tel_casa

          LET long_telefono = LENGTH(v_telefono) 
          IF long_telefono = 10 THEN
             IF v_telefono[1,2] = 55 THEN
                LET serie_lada = v_telefono[1,2]
                LET v_telefono = v_telefono[3,10]
             ELSE
                LET serie_lada = v_telefono[1,3]
                LET v_telefono = v_telefono[4,10]
             END IF
          END IF

          INSERT INTO afi_telefono
          VALUES (reg_det_internet.n_seguro           , --nss
                  v_folio                             , --n_folio
                  vtipo_solicitud                     , --tipo_solicitud
                  52                                  , --pais_cod
                  serie_lada                          , --clave_lada #12092011
                  ""                                  , --extension
                  v_telefono                          , --telefono
                  v_cod_telefono                      , --tel_cod
                  "" , #hora_ini
                  "" , #tel_tp_hora_ini
                  "" , #tel_hora_fin
                  "" , #tel_tp_hora_fin
                  "" , #tel_dia
                  g_usuario                           , --usuario
                  HOY                                 ) --factualiza
       END IF

       LET v_cod_correoe = NULL
       LET v_correo_e    = NULL 
       LET v_marca       = 'X'
       INITIALIZE folio TO NULL

       IF reg_det_internet.correo_elect = carga_reg[437,486] THEN
          IF (reg_det_internet.correo_elect IS NULL OR
              reg_det_internet.correo_elect = " ") THEN
          ELSE
              LET v_cod_correoe = "1"
              LET v_correo_e = reg_det_internet.correo_elect
              INSERT INTO afi_correo_elect
              VALUES (reg_det_internet.n_seguro           , --nss
                      v_folio                             , --n_folio
                      vtipo_solicitud                     , --tipo_solicitud
                      v_cod_correoe                       , --cod_correo_e
                      v_correo_e                          , --correo_elec
                      v_marca                             , --marca_envio
                      folio                               , --folio
                      #""                                  , --fenvio_correo
                      HOY                                 , --factualiza
                      g_usuario                           ) --usuario
          END IF
       END IF
{ #comentado 13092011
       IF reg_det_internet.paterno_ben1 = carga_reg[529,568] THEN
          IF (reg_det_internet.paterno_ben1 IS NULL OR
              reg_det_internet.paterno_ben1 = " ")  THEN
          ELSE
             INSERT INTO afi_beneficiario
             VALUES (reg_det_internet.n_seguro            , --nss
                     v_folio                              , --n_folio
                     vtipo_solicitud                      , --tipo_solicitud
                     11                                   , --parentesco
                     reg_det_internet.paterno_ben1        , --paterno
                     reg_det_internet.materno_ben1        , --materno
                     reg_det_internet.nombres_ben1        , --nombres
                     ""                                   , --fena
                     0                                    , --sexo
                     reg_det_internet.porcentaje1         , --porcentaje
                     1                                    ) --consecutivo 
          END IF
       END IF

       IF reg_det_internet.paterno_ben2 = carga_reg[652,691] THEN
          IF (reg_det_internet.paterno_ben2 IS NULL OR
              reg_det_internet.paterno_ben2 = " ")  THEN
          ELSE
             INSERT INTO afi_beneficiario
             VALUES (reg_det_internet.n_seguro            , --nss
                     v_folio                              , --n_folio
                     vtipo_solicitud                      , --tipo_solicitud
                     11                                   , --parentesco
                     reg_det_internet.paterno_ben2        , --paterno
                     reg_det_internet.materno_ben2        , --materno
                     reg_det_internet.nombres_ben2        , --nombres
                     ""                                   , --fena
                     0                                    , --sexo
                     reg_det_internet.porcentaje2         , --porcentaje
                     2                                    ) --consecutivo 
          END IF
       END IF

       IF reg_det_internet.paterno_ben3 = carga_reg[775,814] THEN
          IF (reg_det_internet.paterno_ben3 IS NULL OR
              reg_det_internet.paterno_ben3 = " ")  THEN
          ELSE
             INSERT INTO afi_beneficiario
             VALUES (reg_det_internet.n_seguro            , --nss
                     v_folio                              , --n_folio
                     vtipo_solicitud                      , --tipo_solicitud
                     11                                   , --parentesco
                     reg_det_internet.paterno_ben3        , --paterno
                     reg_det_internet.materno_ben3        , --materno
                     reg_det_internet.nombres_ben3        , --nombres
                     ""                                   , --fena
                     0                                    , --sexo
                     reg_det_internet.porcentaje3         , --porcentaje
                     3                                    ) --consecutivo 
          END IF
       END IF

       IF (reg_det_internet.profesion IS NULL OR
           reg_det_internet.profesion = " ")  THEN
       ELSE
           SELECT a.profesion_cod
             INTO vprofesion_cod
             FROM tab_profesion a
            WHERE "%reg_det_internet.profesion%" LIKE a.profesion_desc
           IF STATUS = NOTFOUND THEN
              LET vprofesion_cod = 9999
           END IF 
       END IF

       IF (reg_det_internet.actividad IS NULL OR
           reg_det_internet.actividad = " ")  THEN
           SELECT 'X'
             FROM tab_actividad a
            WHERE a.actividad_cod = reg_det_internet.actividad
           IF STATUS = NOTFOUND THEN
              LET reg_det_internet.actividad = 14
           END IF 
       ELSE
           INSERT INTO afi_ctr_actividad 
           VALUES (reg_det_internet.n_seguro               , --nss
                   v_folio                                 , --n_folio
                   vtipo_solicitud                         , --tipo_solicitud
                   vprofesion_cod                          , --profesion
                   reg_det_internet.actividad              , --actividad
                   g_usuario                               , --usuario
                   HOY                                     ) --factualiza
       END IF}
    END IF

    #---SUMARIO---#

    IF carga_reg[1,2] = "09" AND c2_ident_operacion = "52" THEN
       LET c2_ident_operacion = ""
       LET reg_sum_internet.tipo_registro      = carga_reg[001,002]
       --LET reg_sum_internet.reg_not_afil       = carga_reg[007,015]  #12092011
       --LET reg_sum_internet.reg_not_no_afil    = carga_reg[016,024]  #12092011
       LET reg_sum_internet.reg_not_afil       = "" #12092011
       LET reg_sum_internet.reg_not_no_afil    = "" #12092011
       LET reg_sum_internet.tot_reg_not        = carga_reg[025,033]
       LET reg_sum_internet.fecha_presentacion = 
           reg_cza_internet.fecha_presentacion
 
       INSERT INTO afi_sum_reg_internet VALUES(reg_sum_internet.*)
    END IF

  END FOREACH

  LET cont = cont - 2

END FUNCTION

FUNCTION impresion_reporte()

    DEFINE G_IMPRE        CHAR(300)
    DEFINE c_impre        CHAR(300)
    DEFINE gimpresion     CHAR(300)
    DEFINE hora           CHAR (08)

    LET G_IMPRE = g_param_afi.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                  ".NOTI_CUENTAS_OP_52.",hoy USING "DD-MM-YYYY"

    START REPORT det_noti_cuentas_rpt TO  G_IMPRE

    OUTPUT TO REPORT det_noti_cuentas_rpt(g.*)

    FINISH REPORT det_noti_cuentas_rpt

    LET c_impre = ' chmod 777 ', G_IMPRE CLIPPED
    RUN c_impre

    LET gimpresion = "lp ",G_IMPRE
    --LET gimpresion = "vi ",G_IMPRE
    RUN gimpresion

END FUNCTION

REPORT det_noti_cuentas_rpt(g)

    DEFINE g RECORD
        nss                 CHAR(11),
        paterno             CHAR(40),
        materno             CHAR(40),
        nombres             CHAR(40)
    END RECORD

    DEFINE v_nombre_comp    CHAR(120)
    DEFINE fecha_max        DATE
    DEFINE contador         SMALLINT
    DEFINE contador_afi_sol SMALLINT

    OUTPUT
        TOP MARGIN 1
        BOTTOM MARGIN 0
        LEFT MARGIN 0
        RIGHT MARGIN 0
        PAGE LENGTH 60

    FORMAT
        PAGE HEADER

        PRINT COLUMN 01,s_codigo_afore,"  ",vrazon_social CLIPPED,
              COLUMN 68,TODAY USING "dd-mm-yyyy"
        SKIP 2 LINE
        PRINT COLUMN 08,"REGISTROS RECIBIDOS POR OP.52 NOTIFICACION DE CUENTAS "
        SKIP 1 LINE
        PRINT COLUMN 01,"           REGISTROS GUARDADOS EN EL DETALLE DE INTERNET             "
        PRINT COLUMN 01,"---------------------------------------------------------------------"
        PRINT COLUMN 02,"NSS",
              COLUMN 15,"NOMBRE"
        PRINT COLUMN 01,"---------------------------------------------------------------------"
        SKIP 1 LINE
    ON EVERY ROW

     DECLARE cur_rep CURSOR FOR
     SELECT  a.n_seguro,a.paterno,a.materno,a.nombres
     FROM    afi_det_reg_internet a

     LET contador = 0
     FOREACH cur_rep INTO g.nss,
                          g.paterno,
                          g.materno,
                          g.nombres

        LET v_nombre_comp = g.paterno CLIPPED, " ",g.materno CLIPPED, " ",g.nombres CLIPPED
        LET contador = contador + 1

        PRINT COLUMN 02,g.nss,
              COLUMN 15,v_nombre_comp

     END FOREACH

     SELECT COUNT(*) 
     INTO contador_afi_sol
     FROM afi_det_reg_internet

     PAGE TRAILER
        SKIP 2 LINE
        PRINT COLUMN 10,"Total Registros Leidos del Detalle de Internet : ",contador USING "####"
        PRINT COLUMN 10,"Total de Registros Guardados en afi_solicitud  : ",contador_afi_sol USING "####"
        PRINT COLUMN 60,"Pagina:",PAGENO USING "<<<<"

        PAUSE "Presione enter para continuar."

END REPORT

FUNCTION apertura_cuenta()
#ac-----------------------

   DEFINE 
       enter       CHAR(1),
       generar     CHAR(1),
       aux_pausa   CHAR(1),
       opc         CHAR(1),
       g_usuario   CHAR(8),
       HORA        CHAR(8),
       vnss        CHAR(11),
       operacion   CHAR(40),
       v_sql_1     CHAR(50),
       v_sql_2     CHAR(50),
       HOY         DATE,
       f_ini_tmte  DATE 

    DEFINE w_aux  RECORD
        n_seguro            LIKE afi_solicitud.n_seguro,
        n_unico             LIKE afi_solicitud.n_unico ,
        n_rfc               LIKE afi_solicitud.n_rfc   ,
        paterno             LIKE afi_solicitud.paterno ,
        materno             LIKE afi_solicitud.materno ,
        nombres             LIKE afi_solicitud.nombres ,
        fena                LIKE afi_solicitud.fena    ,
        sexo                LIKE afi_solicitud.sexo    ,
        frecafor            LIKE afi_solicitud.frecafor,
        status_interno      SMALLINT
    END RECORD

    DEFINE g_afore       RECORD LIKE tab_afore_local.*
    DEFINE g_aficefa     RECORD LIKE afi_icefa.*
    DEFINE gr_ctanssreg  RECORD LIKE cta_nss_regimen.*

    DEFINE
        bnd_proceso     SMALLINT ,
        pestado_marca   SMALLINT ,
        pcodigo_rechazo SMALLINT ,
        ejecuta         CHAR(300),
        xcodigo_marca   SMALLINT ,
        xcodigo_rechazo SMALLINT ,
        pmarca_entra    SMALLINT,
        pmarca_causa    SMALLINT,
        pfecha_causa    SMALLINT,
	xmarca_esado    SMALLINT,
	edo_proc        SMALLINT

    DEFINE consulta_carta CHAR(120)

    DEFINE reg_carta RECORD LIKE int_ctr_carta.*
    DEFINE afi       RECORD LIKE afi_solicitud.*

    DEFINE 
        pat RECORD LIKE afi_patron.* ,
        ben RECORD LIKE afi_beneficiario.*,
        mae RECORD LIKE afi_mae_afiliado.*,
        cta RECORD LIKE cta_ctr_cuenta.*

    DEFINE 
        mensaje   CHAR(050),
        G_LISTA   CHAR(300)

    DEFINE 
        i             ,
        cont          ,
        HAY           ,
        v_porc        SMALLINT,
        v_existe      ,
        v_es_menor    ,
        v_criterio    SMALLINT,        
        v_crea_fecha  DATE,        
        v_tipo_proc   ,
	v_tipo_trasp  ,
        v_medio       ,
        v_cve_siefore ,
        v_cve_sief_i  ,
        v_cve_sief_f  ,
        v_edad        ,
        v_rechazo     SMALLINT,
        v_folioatencion INTEGER

    DEFINE regrowid RECORD 
       v_rowid   DECIMAL(10,0)
    END RECORD  

    DEFINE v_afisolreg RECORD
         nss   LIKE afi_solicitud_regimen.nss,
         fol   LIKE afi_solicitud_regimen.n_folio,
         ts    LIKE afi_solicitud_regimen.tipo_solicitud,
         edo   LIKE afi_solicitud_regimen.estado
    END RECORD

    DEFINE 
           v_ind_edad    SMALLINT

    DEFINE
           v_curp        CHAR(18),
           v_rfc         CHAR(13),
           v_fena        DATE

    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp
        DROP TABLE safre_tmp:afi_ind
    WHENEVER ERROR STOP

    CREATE TABLE safre_tmp:afi_ind
       (n_seguro CHAR(11) ,
        n_unico  CHAR(18) ,
        n_rfc    CHAR(13) ,
        paterno  CHAR(40) ,
        materno  CHAR(40) ,
        nombres  CHAR(40) ,
        fena     DATE     ,
        sexo     SMALLINT ,
        frecafor DATE     ,
        status_interno  SMALLINT);

    DATABASE safre_af

    SELECT *, USER
    INTO   g_afore.*, g_usuario
    FROM   tab_afore_local

    LET HOY      = TODAY
    LET HORA     = TIME

    LET operacion = 'ALTA EN MAESTRO DE AFILIADOS'

    INITIALIZE reg_carta.* TO NULL  
    INITIALIZE gr_ctanssreg.* TO NULL
    LET con_curp = 0
    LET sin_curp = 0

    --LET v_sql_1 = "EXECUTE PROCEDURE fn_edad(?,?)"
    LET v_sql_1 = "EXECUTE PROCEDURE fn_fnacimiento(?,?)"
    LET v_sql_2 = "EXECUTE PROCEDURE fn_regimen_inv(?,?,?,?,?,?)"

    PREPARE stmt1 FROM v_sql_1
    PREPARE stmt2 FROM v_sql_2

    LET mensaje = "NO SE HICIERON TRASPASOS AL MAESTRO DE AFILIADOS"

    DECLARE cursor_a CURSOR FOR 
    SELECT rowid,A.* 
    FROM   afi_solicitud A
    WHERE  A.status_interno = 60
    AND    A.tipo_solicitud in (11,12)
    ORDER  BY n_seguro

    FOREACH cursor_a INTO  regrowid.v_rowid,afi.*

       LET reg_det_internet.n_seguro            = afi.n_seguro
       LET reg_det_internet.n_folio             = afi.n_folio
       LET vtipo_solicitud                      = afi.tipo_solicitud
       LET reg_det_internet.fecha_rec_sol_elect = afi.fentcons

       IF afi.n_unico IS NULL THEN
          LET sin_curp = sin_curp + 1
       ELSE
          LET con_curp = con_curp + 1
       END IF

       LET HAY = FALSE

        SELECT COUNT(*)
        INTO   HAY
        FROM   afi_mae_afiliado m
        WHERE  m.n_seguro = afi.n_seguro

        IF HAY THEN
            SELECT *
            INTO   mae.*
            FROM   afi_mae_afiliado ma
            WHERE  ma.n_seguro = afi.n_seguro

            IF SQLCA.SQLCODE = 0 THEN
                INSERT INTO afi_his_afiliado VALUES (mae.*)

                IF SQLCA.SQLCODE = 0 THEN
                    DELETE
                    FROM   afi_mae_afiliado
                    WHERE  n_seguro = afi.n_seguro
                END IF

                SELECT b.*
                INTO   cta.*
                FROM   cta_ctr_cuenta b
                WHERE  b.nss = afi.n_seguro

                IF cta.nss THEN
                    INSERT INTO cta_his_cuenta VALUES (cta.*)
                END IF

                IF mae.tipo_solicitud = 5 THEN
                    LET afi.finitmte = mae.fentcons
                    LET afi.fentcons = afi.fentcons

                    UPDATE afi_det_asignado
                    SET    fecha_afiliacion = afi.fentcons,
                           estado_asignado  = 100
                    WHERE  n_seguro = mae.n_seguro
                    AND    n_folio  = mae.n_folio
                    AND    tipo_solicitud = mae.tipo_solicitud
                END IF

                LET HAY = FALSE
            END IF
        END IF

        LET afi.status_interno = 100
        LET afi.status_captura = 100

        IF NOT HAY THEN
            IF afi.n_unico IS NOT NULL AND
               afi.n_unico <> "                  " AND
                LENGTH(afi.n_unico) = 18 THEN
                LET afi.status_interno = 200
                LET afi.status_captura = 0
            ELSE
                LET afi.status_interno = 100
                LET afi.status_captura = 0
            END IF

            LET afi.status = NULL 

            INSERT INTO afi_mae_afiliado VALUES(afi.*)

            IF SQLCA.SQLCODE <> 0 THEN
                INSERT INTO safre_tmp:nss_dup VALUES (afi.n_seguro)
            END IF

            INSERT INTO afi_mae_patron      -------- Patrones
            SELECT *
            FROM   afi_patron
            WHERE  n_folio = afi.n_folio
            AND    tipo_solicitud = afi.tipo_solicitud

           INSERT INTO afi_mae_benefici    -------- Beneficiarios
           SELECT *
           FROM   afi_beneficiario a
           WHERE  a.n_seguro = afi.n_seguro
           #AND    a.n_folio  = afi.n_folio

           #IF afi.tipo_solicitud = 11 THEN

              LET v_crea_fecha = HOY

              DECLARE curs1 CURSOR FOR stmt1
   
              OPEN  curs1 USING afi.n_seguro, v_crea_fecha
   
              --FETCH curs1 INTO v_existe, v_es_menor, v_criterio
              FETCH curs1 INTO v_existe, v_edad, v_criterio, v_ind_edad,
                               v_curp, v_rfc, v_fena
              CLOSE curs1

              IF v_es_menor = 0 THEN
                  LET v_cve_sief_f = 2
              ELSE
                  LET v_cve_sief_f = 1
              END IF

	      IF mae.tipo_solicitud = 5 THEN
                  LET v_tipo_trasp = 5
	      ELSE
                  LET v_tipo_trasp = 6
	      END IF

	      LET v_tipo_proc = 1
              LET v_medio     = 10

              SELECT "X"
              FROM   cta_ctr_cuenta
              WHERE  cta_ctr_cuenta.nss = afi.n_seguro
   
              IF SQLCA.SQLCODE = 0 THEN
                   UPDATE cta_ctr_cuenta
                   SET    fecha_pri_rcv      = NULL,
                          fecha_ult_rcv      = NULL,
                          fecha_pri_general  = NULL,
                          fecha_ult_general  = NULL,
                          fecha_vol_pat      = NULL,
                          fecha_vol_ven      = NULL,
                          ind_actividad      = 1,
                          fecha_actividad    = HOY,
                          ind_edad           = v_es_menor, #ejrm
                          fecha_edad         = HOY,        #ejrm
                          criterio_edad      = v_criterio, #ejrm
                          ind_transferencia  = 0,          #ejrm
                          fecha_ind_transf   = HOY,        #ejrm
                          ind_saldo_cero     = 0,
                          fecha_saldo_cero   = NULL,
                          estado_impresion   = 0,
                          periodo_ult_aporte = NULL,
                          dias_cotizados     = 0,
                          ult_sal_integrado  = 0,
                          tipo_informe       = 0,
                          fecha_informe      = NULL,
                          fecha_registro     = HOY,
                          usuario            = g_usuario
                   WHERE  nss                = afi.n_seguro
               ELSE
                   INSERT INTO cta_ctr_cuenta     #------ Control cuenta
                   VALUES (afi.n_seguro,       #nss
                           "",                 #fecha_pri_rcv
                           "",                 #fecha_ult_rcv
                           "01/01/0001",       #fecha_pri_general
                           "",                 #fecha_ult_general
                           "",                 #fecha_vol_pat
                           "",                 #fecha_vol_ven
                           0,                  #ind_saldo_cero
                           "",                 #fecha_saldo_cero
                           1,                  #ind_actividad
                           HOY,                #fecha_actividad
                           v_es_menor,         #ind_edad
                           HOY,                #fecha_edad
                           v_criterio,         #criterio_edad
                           0,                  #ind_transferencia
                           HOY,                #fecha_ind_transf
                           0,                  #estado_impresion,
                           "",                 #periodo_ult_aporte
                           0,                  #dias_cotizados
                           0,                  #ult_sal_integrado
                           0,                  #tipo_informe
                           "",                 #fecha_informe
                           HOY,                #fecha_registro
                           g_usuario           #usuario
                           )
               END IF

               DECLARE curs2 CURSOR FOR stmt2

               OPEN curs2 USING afi.n_seguro,
                                v_ind_edad,
                                v_ind_edad,
                                v_tipo_proc,
                                v_tipo_trasp,
                                v_medio
   
               FETCH curs2 INTO v_existe, v_edad, v_rechazo, v_folioatencion
   
               CLOSE curs2
           #END IF
       END IF

        LET HORA = TIME

        INSERT INTO afi_ctr_logico
        VALUES (afi.n_folio,
                afi.tipo_solicitud,
                afi.n_seguro,
                afi.status_interno,
                g_usuario,
                HOY,
                HORA,
                operacion)

        INSERT INTO safre_tmp:afi_ind
        VALUES (afi.n_seguro       ,
                afi.n_unico        ,
                afi.n_rfc          ,
                afi.paterno        ,
                afi.materno        ,
                afi.nombres        ,
                afi.fena           ,
                afi.sexo           ,
                afi.frecafor       ,
                afi.status_interno
                )

        UPDATE afi_solicitud 
        SET    afi_solicitud.status         = 100 ,
               afi_solicitud.status_interno = 100 ,
               afi_solicitud.status_captura = 100 ,
               fecha_1a_afil                = afi.fentcons      ---erm 
        WHERE  afi_solicitud.n_seguro       = afi.n_seguro
        AND    afi_solicitud.n_folio        = afi.n_folio
        AND    afi_solicitud.tipo_solicitud = afi.tipo_solicitud

        LET reg_det_internet.n_seguro            = afi.n_seguro
        LET reg_det_internet.n_folio             = afi.n_folio
        LET vtipo_solicitud                      = afi.tipo_solicitud
        LET reg_det_internet.fecha_rec_sol_elect = afi.fentcons

        {IF (afi.finitmte IS NULL) OR 
           (afi.finitmte = '')    THEN}
            LET reg_carta.docto_cod = 30501 
            CALL det_carta(30501) #dc
        {ELSE
            LET reg_carta.docto_cod = 30502
            CALL det_carta(30502) #dc
        END IF}

        SELECT "X"
        FROM   safre_af:rec_solicitud mr
        WHERE  mr.n_seguro   = afi.n_seguro
        AND    mr.origen_rec <> 1
        GROUP BY 1

        IF SQLCA.SQLCODE = 0 THEN
            UPDATE safre_af:rec_solicitud
            SET    safre_af:rec_solicitud.origen_rec = 1
            WHERE  safre_af:rec_solicitud.n_seguro = afi.n_seguro
            AND    safre_af:rec_solicitud.origen_rec <> 1
        END IF      
    END FOREACH

    SELECT "X"
    FROM   safre_tmp:afi_ind
    GROUP BY 1

    LET G_LISTA = g_param_afi.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                  ".ENVIA_IND_MAESTRO." CLIPPED,
                  HOY USING "dd-mm-yy","_",HORA CLIPPED

    IF STATUS = NOTFOUND THEN 
        START REPORT listado_1 TO G_LISTA
            OUTPUT TO REPORT listado_1(mensaje)
        FINISH REPORT listado_1
    ELSE
        DECLARE cur_1a CURSOR FOR
        SELECT *
        FROM   safre_tmp:afi_ind

        START REPORT listado_2 TO G_LISTA
            FOREACH cur_1a INTO w_aux.*
                OUTPUT TO REPORT listado_2(w_aux.*)
            END FOREACH
        FINISH REPORT listado_2
    END IF 

    LET G_LISTA = "chmod 777 ",g_param_afi.ruta_listados CLIPPED,
		 "/",g_usuario CLIPPED,".ENVIA_IND_MAESTRO." CLIPPED,
                  HOY USING "dd-mm-yy","_",HORA CLIPPED
    RUN G_LISTA

    CALL despliega_apertura()

    #PROMPT "Proceso finalizado, [Enter] para salir"
    #FOR enter

END FUNCTION

FUNCTION despliega_apertura()
#dr--------------------------

    DEFINE total_resp SMALLINT

    LET total_resp = con_curp + sin_curp

    IF bnd_proceso = 0 THEN
        DISPLAY "          NSS INCORPORADOS AL MAESTRO DE AFILIADOS           "
        DISPLAY "Total de Registros Incorporados : ", total_resp USING "#####&"

        DISPLAY "Registros con curp              : ", con_curp   USING "#####&"

        DISPLAY "Registros sin curp              : ", sin_curp   USING "#####&"
    ELSE
        DISPLAY "          NSS INCORPORADOS AL MAESTRO DE AFILIADOS           "
        AT 10,1 ATTRIBUTE(REVERSE)

        LET cont_reg_total = cont_reg + cont_reg_noa
        DISPLAY "Tot Reg             :                                                          " AT 11,1

        DISPLAY "Tot Reg             : ", cont_reg_total USING "#####&"
        AT 11,1

        DISPLAY "Tot Reg Cert 1er Reg: ", cont_reg       USING "#####&"
        AT 12,1

        DISPLAY "Tot Reg Cert No AFil: ", cont_reg_noa   USING "#####&"
        AT 13,1

        DISPLAY "Tot Reg Incorporados: ", total_resp USING "#####&"
        AT 11,32

        DISPLAY "Registros con curp  : ", con_curp   USING "#####&"
        AT 12,32

        DISPLAY "Registros sin curp  : ", sin_curp   USING "#####&"
        AT 13,32
#12092111
        DISPLAY "En Espera  Recursos : ", vpend   USING "#####&"
        AT 14,1

        DISPLAY "                                                                               " 
        AT 14,32
        DISPLAY "                                                                               " 
        AT 15,32
        DISPLAY "                                                                               " 
        AT 16,32

    END IF

END FUNCTION
FUNCTION det_carta(vdocto_cod) #dc
#dc---------------------
  DEFINE vdocto_cod SMALLINT

    LET reg_carta.docto_cod      = vdocto_cod
    LET reg_carta.nss            = reg_det_internet.n_seguro
    LET reg_carta.n_folio        = reg_det_internet.n_folio
    LET reg_carta.tipo_solicitud = vtipo_solicitud 
    LET reg_carta.fecha_registro = reg_det_internet.fecha_rec_sol_elect
    LET reg_carta.opera_cod      = NULL
    LET reg_carta.edo_genera     = 10
    LET reg_carta.fecha_genera   = TODAY
    LET reg_carta.hora_genera    = TIME
    LET reg_carta.lote_genera    = 0
    LET reg_carta.consecutivo    = 0
    LET reg_carta.id_sepomex     = 0

    LET consulta_carta = "EXECUTE PROCEDURE inserta_carta (?,?,?,?,?,?,",
                                                           "?,?,?,?,?,?)"
    PREPARE exe_sql FROM consulta_carta
    EXECUTE exe_sql USING reg_carta.*

    INITIALIZE reg_carta.* TO NULL

END FUNCTION
REPORT listado_1(mensaje)
#------------------------

    DEFINE
        mensaje             CHAR(50)

    OUTPUT
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER
        PRINT
            COLUMN 03,"========================================",
            COLUMN 40,"==================================="
        PRINT
            COLUMN 20," TRASPASO INTERNET REG AL MAESTRO DE AFILIADOS"
        PRINT
            COLUMN 03,"----------------------------------------",
            COLUMN 40,"-----------------------------------"
        PRINT
            COLUMN 01,"N T I  "  ,
            COLUMN 13,"CURP   "  ,
            COLUMN 33,"R.F.C. "  ,
            COLUMN 48,"Paterno"  ,
            COLUMN 68,"Materno"  ,
            COLUMN 88,"Nombres"
        PRINT
            COLUMN 05,"Fecha Nac."     ,
            COLUMN 17,"Sexo"           ,
            COLUMN 28,"Fecha Frecafor" ,
            COLUMN 43,"Edo. Afiliado"
        PRINT
            COLUMN 03,"========================================",
            COLUMN 40,"==================================="
        PRINT
        PRINT
        PRINT
            COLUMN 15,mensaje

END REPORT
REPORT listado_2(w_aux)
#----------------------

    DEFINE w_aux  RECORD
        n_seguro            LIKE afi_solicitud.n_seguro,
        n_unico             LIKE afi_solicitud.n_unico ,
        n_rfc               LIKE afi_solicitud.n_rfc   ,
        paterno             LIKE afi_solicitud.paterno ,
        materno             LIKE afi_solicitud.materno ,
        nombres             LIKE afi_solicitud.nombres ,
        fena                LIKE afi_solicitud.fena    ,
        sexo                LIKE afi_solicitud.sexo    ,
        frecafor            LIKE afi_solicitud.frecafor,
        status_interno      SMALLINT
    END RECORD

    DEFINE
        l_estado    CHAR(16) ,
        aux_sexo    CHAR(10)

    DEFINE
        cont        INTEGER

    OUTPUT
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER
        PRINT
            COLUMN 03,"========================================",
            COLUMN 40,"==================================="
        PRINT
            COLUMN 20," TRASPASO NO AFILIADOS AL MAESTRO DE AFILIADOS"
        PRINT
            COLUMN 03,"----------------------------------------",
            COLUMN 40,"-----------------------------------"
        PRINT
            COLUMN 01,"N T I  "       ,
            COLUMN 13,"CURP   "       ,
            COLUMN 33,"R.F.C. "       ,
            COLUMN 48,"Paterno"       ,
            COLUMN 68,"Materno"       ,
            COLUMN 88,"Nombres"
        PRINT
            COLUMN 05,"Fecha Nac."    ,
            COLUMN 17,"Sexo"          ,
            COLUMN 28,"Fecha Frecafor",
            COLUMN 43,"Edo. Afiliado"
        PRINT
            COLUMN 03,"========================================",
            COLUMN 40,"==================================="
    ON EVERY ROW
        IF w_aux.n_unico IS NULL OR w_aux.n_unico = " " THEN
           LET l_estado = NULL
        END IF
        CASE w_aux.status_interno
             WHEN   0 LET l_estado = "CAPTURADO"
             WHEN  10 LET l_estado = "IMCOMPLETO"
             WHEN  20 LET l_estado = "COMPLETO"
             WHEN  30 LET l_estado = "ENIVIADO"
             WHEN  40 LET l_estado = "RECHAZADO"
             WHEN  50 LET l_estado = "PENDIENTE"
             WHEN  55 LET l_estado = "ACLARACION"
             WHEN  60 LET l_estado = "APROBADO"
             WHEN 100 LET l_estado = "REGISTRADO"
        END CASE

        SELECT sexo_desc
        INTO   aux_sexo
        FROM   tab_sexo
        WHERE  sexo_cod = w_aux.sexo

        PRINT
            COLUMN 01,w_aux.n_seguro                   ,
            COLUMN 13,w_aux.n_unico                    ,
            COLUMN 33,w_aux.n_rfc                      ,
            COLUMN 48,w_aux.paterno CLIPPED            ,
            COLUMN 68,w_aux.materno CLIPPED            ,
            COLUMN 88,w_aux.nombres CLIPPED
        PRINT
            COLUMN 05,w_aux.fena  USING "dd-mm-yyyy"   ,
            COLUMN 17,aux_sexo    ,
            COLUMN 28,w_aux.frecafor USING "dd-mm-yyyy",
            COLUMN 43,l_estado CLIPPED

    ON LAST ROW
           SELECT COUNT(*)
           INTO   cont
           FROM   safre_tmp:afi_ind

        PRINT
        PRINT
        PRINT
            COLUMN 03,"----------------------------------------",
            COLUMN 40,"-----------------------------------"

        PRINT
        PRINT
           COLUMN 03,"NUMERO TOTAL DE REGISTROS ---> ",cont
END REPORT

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
