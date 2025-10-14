#################################################################################
##Proyecto           => SYSTEMA DE AFORES ( MEXICO )                            #
##Owner              => E.F.P.                                                  #
##Programa RETM920   => Administrar Solicitudes de Retiro                       #
##Sistema            => SAFRE                                                   #
##Fecha Creacion     => 1 Oct 2009                                              #
##By                 => JGHM                                                    #
#Fecha actualiz.   => 13 DE JUNIO DE 2012                                       #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                  => Se agrega el tipo de retiro I a los tipos validos de      #
#                     retiros totales ISSSTE                                    #
#################################################################################
#Fecha Actualiz.   => 22/01/22015                                               #
#Actualizacion     =>Cesar D. Chavéz  Mtz.                                      #
#                  => Se agrega solicitud de constancia en las pantallas de     #
#                     Agregar, Consulta, Modificacion y Elimina para los retiros#
#                     Totales y parciales CPL-1844                              #
#################################################################################
DATABASE safre_af

GLOBALS "RETM921.4gl"

DEFINE    li_consecutivo    INTEGER

MAIN
    DEFER INTERRUPT
    OPTIONS
      INPUT WRAP           ,
      PROMPT LINE LAST     ,
      ACCEPT KEY CONTROL-I

    CALL STARTLOG("RETM920.log")
    CALL f_inicial()

    OPEN   WINDOW RETM9201 AT 2,2 WITH FORM "RETM9201"                                               ATTRIBUTE(BORDER)

    CASE gc_opc
       WHEN "A"
          CALL  f_inicializa()
          CALL  f_agrega()
          EXIT  PROGRAM
       WHEN "C"
          CALL  f_inicializa()
          CALL  f_inicial()
          IF    gc_tipo_retiro    = 'F'   THEN
                CALL  f_detalle1(gi_consecutivo)
          ELSE
                CALL  f_detalle(gi_consecutivo)
          END IF
          EXIT  PROGRAM
       WHEN "M"
          CALL  f_inicializa()
          CALL  f_inicial()
          IF    gc_tipo_retiro    = 'F'   THEN
                CALL  f_detalle1(gi_consecutivo)
          ELSE
                CALL  f_detalle(gi_consecutivo)
          END IF
          EXIT  PROGRAM
       WHEN "E"
          CALL  f_inicializa()
          CALL  f_inicial()
          IF    gc_tipo_retiro    = 'F'   THEN
                CALL  f_detalle1(gi_consecutivo)
          ELSE
                CALL  f_detalle(gi_consecutivo)
          END IF
          EXIT  PROGRAM
    END CASE

    IF    gc_opc  IS NULL
     OR   gc_opc  = " " THEN
          MENU "Retiros"
             BEFORE MENU
                --HIDE OPTION ALL
                --SHOW OPTION "Agrega", "Consulta", "Modificar", "Salida"
                CALL  f_inicializa()

             COMMAND KEY("A") "Agrega"    "Agrega una Solicitud de Retiro"
                CALL  f_agrega()

             COMMAND KEY("C") "Consulta"  "Consultar un Trámite de Retiro"
                LET   gc_opc = 'C'
                CALL  f_consulta() RETURNING gi_consecutivo

             COMMAND KEY("T") "Modificar"  "Modificar un Trámite de Retiro"
                CALL  f_inicial()
                LET   gc_opc = 'M'
                CALL  f_consulta() RETURNING gi_consecutivo
                IF    gc_tipo_retiro    = 'F'   THEN
                      CALL  f_detalle1(gi_consecutivo)
                ELSE
                      CALL  f_detalle(gi_consecutivo)
                END IF

              COMMAND KEY("E") "Eliminar"  "Eliminar un Trámite de Retiro"
                CALL  f_inicial()
                LET   gc_opc = 'E'
                CALL  f_consulta() RETURNING gi_consecutivo
                IF    gc_tipo_retiro    = 'F'   THEN
                      CALL  f_detalle1(gi_consecutivo)
                ELSE
                      CALL  f_detalle(gi_consecutivo)
                END IF

              COMMAND KEY("S") "Salida"   "Regresa al Menu"
                EXIT MENU
          END MENU
    END IF

    CLOSE  WINDOW RETM9201
END MAIN


### Inicio de proceso, lee posible parametro
### Datos iniciales
FUNCTION f_inicial()

  LET        gc_opc           = ARG_VAL(1)
  LET        gc_curp          = ARG_VAL(2)
  LET        gc_nti           = ARG_VAL(3)
  LET        gc_tipo_retiro   = ARG_VAL(4)
  LET        gi_consecutivo   = ARG_VAL(5)
  LET        gd_today         = TODAY

  SELECT cod_tramite
  INTO   gs_disp_issste
  FROM   tab_tipo_tramite
  WHERE  descripcion    = "DISPOSICION ISSSTE"

  SELECT cod_tramite
  INTO   gs_par_issste
  FROM   tab_tipo_tramite
  WHERE  descripcion    = "RETIRO PARCIAL ISSSTE"

  LET        gc_query   =  " SELECT  *                        ",
                           "   FROM  afi_mae_afiliado         ",
                           "  WHERE  n_unico           =   ?  "
             --JG quito    "    AND  tipo_solicitud    =   8  "
  PREPARE    p_SelAfi          FROM  gc_query
  DECLARE    d_SelAfi        CURSOR  FOR p_SelAfi

  LET        gc_query   =  " SELECT  *                        ",
                           "   FROM  afi_mae_afiliado         ",
                           "  WHERE  n_unico           =   ?  ",
                           "    AND  n_seguro          =   ?  "
             -- JG quito   "    AND  tipo_solicitud    =   8  "
  PREPARE    p_SelAfi1         FROM  gc_query
  DECLARE    d_SelAfi1       CURSOR  FOR p_SelAfi1

  LET        gc_query   =  " SELECT  *                        ",
                           "   FROM  afi_mae_afiliado         ",
                           "  WHERE  n_seguro          =   ?  "
  PREPARE    p_SelAfi_nss      FROM  gc_query
  DECLARE    d_SelAfi_nss    CURSOR  FOR p_SelAfi

  LET        gc_query   =  " SELECT  a.tipo_administracion, nss_issste  ",
                           "   FROM  cta_ctr_reg_ind a                  ",
                           "  WHERE  a.curp             = ?             "
  PREPARE    p_ObTInv          FROM  gc_query
  DECLARE    d_ObTInv        CURSOR  FOR p_ObTInv

  LET        gc_query   =  " SELECT  a.tipo_trab_ind||'-'||b.desc_tipo_trab_ind ",
                           "   FROM  cta_ctr_reg_ind   a,                       ",
                           "         tab_tipo_trab_ind b                        ",
                           "  WHERE  a.tipo_trab_ind   = b.tipo_trab_ind        ",
                           "    AND  a.curp             = ?                     "
  PREPARE    p_ObTTra          FROM  gc_query
  DECLARE    d_ObTTra        CURSOR  FOR p_ObTTra

  LET        gc_query   =  " SELECT  a.tipo_solicitud||'-'||a.desc_solicitud   ",
                           "   FROM  tab_tipo_solic a                          ",
                           "  WHERE  a.tipo_solicitud   = ?                    "
  PREPARE    p_ObtTSol         FROM  gc_query
  DECLARE    d_ObtTSol       CURSOR  FOR p_ObtTSol

  LET        gc_query   =  " SELECT  tipo_retiro,                                    ",
                           "         tipo_retiro||'-'||descripcion                   ",
                           "   FROM  tab_ret_issste                                  ",
                           "  WHERE  cod_tramite = ", gs_disp_issste
--                           "  WHERE  tipo_retiro    IN ('A','B','C','D','E','K','I') "
  PREPARE    p_ObtTRe1         FROM  gc_query
  DECLARE    d_ObtTRe1       CURSOR  FOR p_ObtTRe1

  LET        gc_query   =  " SELECT  tipo_retiro,                              ",
                           "         tipo_retiro||'-'||descripcion             ",
                           "   FROM  tab_ret_issste                            ",
                           "  WHERE  tipo_retiro      =  'F'                   "
  PREPARE    p_ObtTRe2         FROM  gc_query
  DECLARE    d_ObtTRe2       CURSOR  FOR p_ObtTRe2

  LET        gc_query   =  " SELECT  *                                         ",
                           "   FROM  ret_datamart_issste                       ",
                           "  WHERE  curp             =  ?                     ",
                           "    AND  tipo_retiro      =  ?                     ",
                           "  ORDER  BY sec_pension                            "
  PREPARE    p_ObtDaMa         FROM  gc_query
  DECLARE    d_ObtDaMa       CURSOR  FOR p_ObtDaMa

   LET        gc_query   =  " SELECT  *                                         ",
                           "   FROM  ret_datamart_par_issste                   ",
                           "  WHERE  curp             =  ?                     ",
                           "  ORDER  BY sec_pension                            "
  PREPARE    p_ObtDaMa1        FROM  gc_query
  DECLARE    d_ObtDaMa1      CURSOR  FOR p_ObtDaMa1

  LET        gc_query   =  " SELECT  COUNT(*)                                  ",
                           "   FROM  ret_matriz_derecho_issste                 ",
                           "  WHERE  tipo_retiro      =  ?                     ",
                           "    AND  regimen          =  ?                     ",
                           "    AND  tipo_seguro      =  ?                     ",
                           "    AND  tipo_pension     =  ?                     ",
                           "    AND  tipo_prestacion  =  ?                     "
  PREPARE    p_ObtMaDe         FROM  gc_query

  LET        gc_query   =  " SELECT  mat.regimen,      reg.descripcion[1,17]   ",
                           "   FROM  ret_matriz_derecho_issste  mat,           ",
                           "  OUTER  tab_regimen_issste         reg            ",
                           "  WHERE  mat.tipo_retiro     =  ?                  ",
                           --"    AND  mat.regimen         = 'RO'                ",
                           "    AND  mat.regimen         =  reg.regimen        "
  PREPARE    p_SelReg          FROM  gc_query
  DECLARE    d_SelReg        CURSOR  FOR p_SelReg

  LET        gc_query   =  " SELECT  mat.tipo_seguro,  seg.descripcion[1,25]   ",
                           "   FROM  ret_matriz_derecho_issste  mat,           ",
                           "  OUTER  tab_seguro_issste          seg            ",
                           "  WHERE  mat.tipo_retiro     =  ?                  ",
                           "    AND  mat.regimen         =  ?                  ",
                           "    AND  mat.tipo_seguro     =  seg.clave          "
  PREPARE    p_SelSeg          FROM  gc_query
  DECLARE    d_SelSeg        CURSOR  FOR p_SelSeg

  LET        gc_query   =  " SELECT  mat.tipo_pension,  pen.descripcion[1,25]  ",
                           "   FROM  ret_matriz_derecho_issste  mat,           ",
                           "  OUTER  tab_pension_issste         pen            ",
                           "  WHERE  mat.tipo_retiro     =  ?                  ",
                           "    AND  mat.regimen         =  ?                  ",
                           "    AND  mat.tipo_seguro     =  ?                  ",
                           "    AND  mat.tipo_pension    =  pen.tipo_pension   "
  PREPARE    p_SelPen          FROM  gc_query
  DECLARE    d_SelPen        CURSOR  FOR p_SelPen

  LET        gc_query   =  " SELECT  mat.cve_pension,   cve.desc_corta[1,25]   ",
                           "  FROM  ret_matriz_derecho_issste  mat,            ",
                           " OUTER  tab_cve_pen_issste         cve             ",
                           " WHERE  mat.tipo_retiro     =  ?                   ",
                           "   AND  mat.regimen         =  ?                   ",
                           "   AND  mat.tipo_seguro     =  ?                   ",
                           "   AND  mat.tipo_pension    =  ?                   ",
                           "   AND  mat.cve_pension     =  cve.cve_pension     "
  PREPARE    p_SelCve          FROM  gc_query
  DECLARE    d_SelCve        CURSOR  FOR p_SelCve

  LET        gc_query   =  " SELECT  mat.tipo_prestacion, pre.descripcion[1,25] ",
                           "   FROM  ret_matriz_derecho_issste  mat,            ",
                           "  OUTER  tab_prestacion_issste      pre             ",
                           "  WHERE  mat.tipo_retiro     =  ?                   ",
                           "    AND  mat.regimen         =  ?                   ",
                           "    AND  mat.tipo_seguro     =  ?                   ",
                           "    AND  mat.tipo_pension    =  ?                   ",
                           "    AND  mat.cve_pension     =  ?                   ",
                           "    AND  mat.tipo_prestacion =  pre.tipo_prestacion "
  PREPARE    p_SelPre          FROM  gc_query
  DECLARE    d_SelPre        CURSOR  FOR p_SelPre

  LET        gc_query   =  " SELECT  grupo                                      ",
                           "   FROM  ret_matriz_derecho_issste  mat             ",
                           "  WHERE  mat.tipo_retiro     =  ?                   ",
                           "    AND  mat.regimen         =  ?                   ",
                           "    AND  mat.tipo_seguro     =  ?                   ",
                           "    AND  mat.tipo_pension    =  ?                   ",
                           "    AND  mat.cve_pension     =  ?                   ",
                           "    AND  mat.tipo_prestacion =  ?                   "
  PREPARE    p_SelGpo          FROM  gc_query
  DECLARE    d_SelGpo        CURSOR  FOR p_SelGpo

  LET        gc_query   =  " SELECT  COUNT(*)              ",
                           "   FROM  tab_doc_prob          ",
                           "  WHERE  docprob_cod   = ?     "
  PREPARE    p_SelDoc          FROM  gc_query

  LET        gc_query   =  " INSERT INTO  ret_sol_issste_tx                     ",
             "  VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
  PREPARE    p_InsRetiro       FROM  gc_query

  LET        gc_query   =  " INSERT INTO  ret_solicitante                       ",
                           "  VALUES (?,?,?,?,?,?)                              "
  PREPARE    p_InsSolici       FROM  gc_query

  LET        gc_query   =  " SELECT  paren_desc                                 ",
                           "   FROM  tab_parentesco                             ",
                           "  WHERE  paren_cod <> 12                            "
  PREPARE    p_SelBen          FROM  gc_query
  DECLARE    d_SelBen        CURSOR  FOR p_SelBen

  LET        gc_query    =  " SELECT  *                            ",
                            "   FROM  ret_solicitante              ",
                            "  WHERE  consecutivo   = ?            "
  PREPARE    p_SelRetS          FROM  gc_query

  LET        gc_query    =  "  UPDATE ret_solicitante   SET   apellido_paterno  =  ?,  ",
                            "                                 apellido_materno  =  ?,  ",
                            "                                 nombres           =  ?,  ",
                            "                                 paren_cod         =  ?,  ",
                            "                                 telefono          =  ?   ",
                            "   WHERE consecutivo                               =  ?   "
  PREPARE    p_UpdSoli          FROM  gc_query

  LET        gc_query   =   "  UPDATE ret_sol_issste_tx SET   fecha_solicitud   =  ?,  ",
                            "                                 folio_solicitud   =  ?,  ",
                            "                                 semanas_cotizadas =  ?,  ",
                            "                                 estad_cod         =  ?,  ",
                            "                                 deleg_cod         =  ?,  ",
                            "                                 fecha_modifica    =  ?,  ",
                            "                                 usuario_modifica  =  ?   ",
                            "   WHERE consecutivo                               =  ?   "
  PREPARE    p_UpdReti          FROM  gc_query

  LET        gc_query   =   "  UPDATE ret_parcial_issste SET  fecha_solicitud   =  ?,  ",
                            "                                 folio_solicitud   =  ?,  ",
                            "                                 fecha_modifica    =  ?,  ",
                            "                                 usuario_modifica  =  ?   ",
                            "   WHERE consecutivo                               =  ?   "
  PREPARE    p_UpdRetiPar1       FROM gc_query

  LET        gc_query   =   "  UPDATE ret_monto_par_issste SET  f_ultimo_aporte     =  ?,  ",
                            "                                   mto_ultimo_aporte   =  ?,  ",
                            "                                   mto_18ultimo_aporte =  ?,  ",
                            "                                   mto_10p_sarissste   =  ?,  ",
                            "                                   mto_a_pagar         =  ?   ",
                            "   WHERE consecutivo                                   =  ?   "
  PREPARE    p_UpdRetiPar2       FROM gc_query

  LET        gc_query   =   "  UPDATE ret_monto_par_issste SET  salario_base_cot    =  ?,  ",
                            "                                   mto_75sbc           =  ?,  ",
                            "                                   mto_10p_rcv         =  ?,  ",
                            "                                   mto_a_pagar         =  ?   ",
                            "   WHERE consecutivo                                   =  ?   "
  PREPARE    p_UpdRetiPar3       FROM gc_query

  LET       gc_query   =   "  UPDATE ret_sol_issste_tx SET   fecha_solicitud   =  ?,  ",
                            "                                 folio_solicitud   =  ?,  ",
                            "                                 aseguradora       =  ?,  ",
                            "                                 actuario          =  ?,  ",
                            "                                 num_plan_privado  =  ?,  ",
                            "                                 semanas_cotizadas =  ?,  ",
                            "                                 estad_cod         =  ?,  ",
                            "                                 deleg_cod         =  ?,  ",
                            "                                 fecha_modifica    =  ?,  ",
                            "                                 usuario_modifica  =  ?   ",
                            "   WHERE consecutivo                               =  ?   "
  PREPARE    p_UpdRet1          FROM  gc_query

  LET        gc_query   =   "  UPDATE ret_sol_issste_tx SET   fecha_solicitud   =  ?,  ",
                            "                                 folio_solicitud   =  ?,  ",
                            "                                 cve_doc_probatorio=  ?,  ",
                            "                                 semanas_cotizadas =  ?,  ",
                            "                                 estad_cod         =  ?,  ",
                            "                                 deleg_cod         =  ?,  ",
                            "                                 fecha_modifica    =  ?,  ",
                            "                                 usuario_modifica  =  ?   ",
                            "   WHERE consecutivo                               =  ?   "
  PREPARE    p_UpdRet2          FROM  gc_query

  LET        gc_query   =   "  UPDATE ret_sol_issste_tx SET   fecha_solicitud   =  ?,  ",
                            "                                 folio_solicitud   =  ?,  ",
                            "                                 fecha_ini_pen     =  ?,  ",
                            "                                 fecha_resolucion  =  ?,  ",
                            "                                 periodo_pago      =  ?,  ",
                            "                                 estad_cod         =  ?,  ",
                            "                                 deleg_cod         =  ?,  ",
                            "                                 fecha_modifica    =  ?,  ",
                            "                                 usuario_modifica  =  ?   ",
                            "   WHERE consecutivo                               =  ?   "
  PREPARE    p_UpdRet3          FROM  gc_query

  SELECT  codigo_afore,  USER
    INTO  gs_afores,     gc_usuario
    FROM  tab_afore_local

  IF      gs_afores         = 578   THEN
     LET  gc_query      =   "  SELECT  COUNT(*)                      ",
                            "    FROM  rec_solicitud                 ",
                            "   WHERE  folio_rec     =  ?            ",
                            "     AND  tipo_id       =  'S'          "
     PREPARE  p_SelCAV           FROM  gc_query
  END IF

  LET        gc_query   =   "  SELECT  movimiento                     ",
                            "    FROM  tab_ret_issste                 ",
                            "   WHERE  tipo_retiro    =  ?            ",
                            "     AND  cod_tramite    = ", gs_disp_issste
  PREPARE    d_BusTipRet         FROM  gc_query

  LET        gc_query   =   "  SELECT  marca_cod                      ",
                            "    FROM  cta_act_marca                  ",
                            "   WHERE  nss            =  ?            "
  PREPARE    p_BusMar            FROM  gc_query
  DECLARE    d_BusMar          CURSOR  FOR p_BusMar

  LET        gc_query   =   "  SELECT  rechazo_cod                    ",
                            "    FROM  cta_convivencia                ",
                            "   WHERE  marca_activa   =  ?            ",
                            "     AND  marca_entra    =  ?            "
  PREPARE    d_BusRech           FROM  gc_query

  LET        gc_query   =   "  SELECT  *                              ",
                            "    FROM  tab_agrupa_subcta              ",
                            "   WHERE  grupo          =  ?            "
  PREPARE    p_BusCta            FROM  gc_query
  DECLARE    d_BusCta          CURSOR  FOR p_BusCta


  LET        gc_query   =   " EXECUTE PROCEDURE fn_saldo_dia_isss ( ?,?,?,? ) "
  PREPARE    p_ExeSal            FROM  gc_query

  LET        gc_query   =   " SELECT descripcion           ",
                            "   FROM tab_regimen_issste    ",
                            "  WHERE regimen =   ?         "
  PREPARE    p_BusRegimen        FROM  gc_query

  LET        gc_query   =   " SELECT descripcion           ",
                            "   FROM tab_seguro_issste     ",
                            "  WHERE clave   =   ?         "
  PREPARE    p_BusSeguro         FROM  gc_query

  LET        gc_query   =   " SELECT descripcion           ",
                            "   FROM tab_pension_issste    ",
                            "  WHERE tipo_pension  = ?     "
  PREPARE    p_BusTipoPension    FROM  gc_query

  LET        gc_query   =   " SELECT desc_corta            ",
                            "   FROM tab_cve_pen_issste    ",
                            "  WHERE cve_pension   = ?     "
  PREPARE    p_BusCvePension     FROM  gc_query

  LET        gc_query   =   " SELECT descripcion           ",
                            "   FROM tab_prestacion_issste ",
                            "  WHERE tipo_prestacion  = ?  "
  PREPARE    p_BusPrestacion     FROM  gc_query

END FUNCTION


### Inicializa valores tomados de parametro
FUNCTION f_inicializa()

  ### Inicia datos del retiro

  INITIALIZE  gr_ret_sol_issste_tx.*                    TO  NULL
  INITIALIZE  gr_ret_solicitante.*                      TO  NULL
  INITIALIZE  gr_ret_parcial_issste.*                   TO  NULL
  INITIALIZE  gr_ret_monto_par_issste.*                 TO  NULL
  LET         gr_ret_sol_issste_tx.folio                =   0
  LET         gr_ret_parcial_issste.folio               =   0
  LET         gr_ret_monto_par_issste.folio             =   0
  LET         gr_ret_sol_issste_tx.fecha_solicitud      =   TODAY
  LET         gr_ret_parcial_issste.fecha_solicitud     =   TODAY
  LET         gr_ret_sol_issste_tx.fecha_captura        =   TODAY
  LET         gr_ret_parcial_issste.fecha_captura       =   TODAY
  LET         gr_ret_sol_issste_tx.usuario_captura      =   gc_usuario
  LET         gr_ret_parcial_issste.usuario_captura     =   gc_usuario
  LET         gr_ret_sol_issste_tx.fecha_modifica       =   ""
  LET         gr_ret_parcial_issste.fecha_modifica      =   ""
  LET         gr_ret_sol_issste_tx.usuario_modifica     =   ""
  LET         gr_ret_parcial_issste.usuario_modifica    =   ""
  LET         gr_ret_sol_issste_tx.fecha_confirma       =   ""
  LET         gr_ret_parcial_issste.fecha_confirma      =   ""
  LET         gr_ret_sol_issste_tx.usuario_confirma     =   ""
  LET         gr_ret_parcial_issste.usuario_confirma    =   ""
  LET         gr_ret_sol_issste_tx.codigo_rechazo       =   0
  LET         gr_ret_sol_issste_tx.estado_solicitud     =   10
  LET         gr_ret_parcial_issste.estado_solicitud    =   10
  LET         gs_Grabe                                  =   0 ### 0 - No graba 1 - Ya grabo

  #CPL-1844
  INITIALIZE gr_constancia.constancia TO NULL

END FUNCTION


### Agrega un retiro
FUNCTION f_agrega()
    DEFINE     lc_curp                CHAR(18)
    DEFINE     lr_captura RECORD
    	   nss                 CHAR(11)
    	END RECORD
    DEFINE     ls_encon               SMALLINT
    DEFINE     lc_tipo_inversion      CHAR(100)
    DEFINE     lc_tipo_trabajador     CHAR(100)
    DEFINE     lc_tipo_solicitud      CHAR(100)
    DEFINE     lc_nss_issste          CHAR(11)
    DEFINE     lc_rfc                 CHAR(13)
    DEFINE     li_x                   SMALLINT
    DEFINE     li_y                   SMALLINT
    DEFINE     ls_lei_DMar            INTEGER
    DEFINE     ls_DMar                SMALLINT
    DEFINE     ls_Made                SMALLINT
    DEFINE     ls_Cancela             SMALLINT
    DEFINE     lc_opci                CHAR(01)
    DEFINE     ls_ResMar              SMALLINT
    DEFINE     ls_VerMarca            SMALLINT
    DEFINE     ls_VerSaldo            SMALLINT
    DEFINE     lc_Mensaje             CHAR(100)
    DEFINE     lc_grupo               LIKE  ret_sol_issste_tx.grupo

    DEFINE     ra_mod ARRAY[005] OF RECORD
        codigo                        CHAR(01),
        descripcion                   CHAR(50)
    END RECORD
    DEFINE     ra_tip ARRAY[020] OF RECORD
        tipo                          CHAR(03),
        descripcion                   CHAR(50)
    END RECORD
    DEFINE     esp  CHAR(1)
    DEFINE lr_valida_par RECORD
       nss CHAR(11)
    END RECORD

    LET      gs_Muerto             =      0
    OPEN   WINDOW RETM9202 AT 2,2 WITH FORM "RETM9202"                                                ATTRIBUTE(BORDER)

    DISPLAY "[Ctrl-C] Cancelar                                                               " AT 01,1 ATTRIBUTE(REVERSE)
    DISPLAY "DATOS GENERALES DEL AFILIADO                                                    " AT 03,1 ATTRIBUTE(REVERSE)
    DISPLAY "DATOS GENERALES DE AFILIACION                                                   " AT 09,1 ATTRIBUTE(REVERSE)
    DISPLAY "DATOS GENERALES DE LA SOLICITUD DE RETIRO                                       " AT 13,1 ATTRIBUTE(REVERSE)

    LET       ls_encon             =      0
    LET       ls_Cancela           =      0
    LET       gs_Grabe             =      0 ### 0 - No graba 1 - Ya grabo

    IF    gc_curp     IS NULL
     OR   gc_curp     =  " "  THEN
     	   INITIALIZE lr_captura.nss, lc_curp TO NULL

     	   INPUT BY NAME lc_curp,
     	   	             lr_captura.nss

            AFTER FIELD lc_curp
               IF lc_curp IS NOT NULL THEN
                 -- CPL-1571
                 -- Se crea la funcion para validar los NSS que vengan en el CURP asociado
                  CALL f_obtiene_afiliado(lc_curp)
                      RETURNING gr_afi_mae_afiliado.*, ls_encon

                  IF  ls_encon             =      0   THEN
                      CALL   f_DespMen("CURP No registrada en Base De Datos o cuenta NO ACTIVA ....   ")
                      INITIALIZE lr_captura.nss, lc_curp TO NULL
                      DISPLAY BY NAME lr_captura.nss, lc_curp
                      NEXT FIELD nss
                  ELSE
                      LET  gr_ret_sol_issste_tx.nss                =  gr_afi_mae_afiliado.n_seguro

                      LET  gr_ret_sol_issste_tx.curp               =  gr_afi_mae_afiliado.n_unico
                      LET  lc_rfc                                  =  gr_afi_mae_afiliado.n_rfc
                      LET  gr_ret_sol_issste_tx.rfc                =  gr_afi_mae_afiliado.n_rfc
                      LET  gr_ret_sol_issste_tx.paterno_afore      =  gr_afi_mae_afiliado.paterno
                      LET  gr_ret_sol_issste_tx.materno_afore      =  gr_afi_mae_afiliado.materno
                      LET  gr_ret_sol_issste_tx.nombre_afore       =  gr_afi_mae_afiliado.nombres
                      LET  gr_ret_sol_issste_tx.fecha_nacimiento   =  gr_afi_mae_afiliado.fena

                      LET  gr_ret_parcial_issste.nss               =  gr_afi_mae_afiliado.n_seguro
                      LET  gr_ret_parcial_issste.curp              =  gr_afi_mae_afiliado.n_unico
                      LET  gr_ret_monto_par_issste.curp            =  gr_afi_mae_afiliado.n_unico
                      LET  gr_ret_parcial_issste.nombre            =  gr_afi_mae_afiliado.nombres
                      LET  gr_ret_parcial_issste.apellido_paterno  =  gr_afi_mae_afiliado.paterno
                      LET  gr_ret_parcial_issste.apellido_materno  =  gr_afi_mae_afiliado.materno

                      CALL f_ObTInver(gr_ret_sol_issste_tx.curp)   RETURNING lc_tipo_inversion,
                                                                             lc_nss_issste

                      LET  gr_ret_parcial_issste.num_issste        =  lc_nss_issste
                      LET  lc_tipo_trabajador                      =  f_ObTTraba(gr_ret_sol_issste_tx.curp)
                      LET  lc_tipo_solicitud                       =  f_ObTSolic(gr_afi_mae_afiliado.tipo_solicitud)

                      EXIT INPUT
                  END IF
            	 END IF

            AFTER FIELD nss
            	 IF lr_captura.nss IS NULL THEN
            	 	  IF lc_curp IS NULL THEN
               	     ERROR "DEBE INDICAR NSS O CURP DEL TRABAJADOR"
               	     SLEEP 2
               	     ERROR ""
               	     NEXT FIELD lc_curp
               	  END IF
            	 ELSE
            	 	  #Realizar búsqueda con NSS
                  SELECT *
                  INTO   gr_afi_mae_afiliado.*
                  FROM   afi_mae_afiliado
                  WHERE  n_seguro = lr_captura.nss

                  IF  SQLCA.SQLCODE             <>     0   THEN
                      CALL   f_DespMen("NSS No registrado en Base De Datos o cuenta NO ACTIVA ....   ")
                      INITIALIZE lr_captura.nss, lc_curp TO NULL
                      DISPLAY BY NAME lr_captura.nss, lc_curp
                      NEXT FIELD nss
                  ELSE
                      LET  gr_ret_sol_issste_tx.nss                =  gr_afi_mae_afiliado.n_seguro

                      LET  gr_ret_sol_issste_tx.curp               =  gr_afi_mae_afiliado.n_unico
                      LET  lc_rfc                                  =  gr_afi_mae_afiliado.n_rfc
                      LET  gr_ret_sol_issste_tx.rfc                =  gr_afi_mae_afiliado.n_rfc
                      LET  gr_ret_sol_issste_tx.paterno_afore      =  gr_afi_mae_afiliado.paterno
                      LET  gr_ret_sol_issste_tx.materno_afore      =  gr_afi_mae_afiliado.materno
                      LET  gr_ret_sol_issste_tx.nombre_afore       =  gr_afi_mae_afiliado.nombres
                      LET  gr_ret_sol_issste_tx.fecha_nacimiento   =  gr_afi_mae_afiliado.fena

                      LET  gr_ret_parcial_issste.nss               =  gr_afi_mae_afiliado.n_seguro
                      LET  gr_ret_parcial_issste.curp              =  gr_afi_mae_afiliado.n_unico
                      LET  gr_ret_monto_par_issste.curp            =  gr_afi_mae_afiliado.n_unico
                      LET  gr_ret_parcial_issste.nombre            =  gr_afi_mae_afiliado.nombres
                      LET  gr_ret_parcial_issste.apellido_paterno  =  gr_afi_mae_afiliado.paterno
                      LET  gr_ret_parcial_issste.apellido_materno  =  gr_afi_mae_afiliado.materno

                      CALL f_ObTInver(gr_ret_sol_issste_tx.curp)   RETURNING lc_tipo_inversion,
                                                                             lc_nss_issste

                      LET  gr_ret_parcial_issste.num_issste        =  lc_nss_issste
                      LET  lc_tipo_trabajador                      =  f_ObTTraba(gr_ret_sol_issste_tx.curp)
                      LET  lc_tipo_solicitud                       =  f_ObTSolic(gr_afi_mae_afiliado.tipo_solicitud)

                      EXIT INPUT
                  END IF
            	 END IF

            ON KEY (CONTROL-C,  INTERRUPT)
                   CALL   f_DespMen("Cancelo el proceso ...... ")
                   LET    ls_Cancela = 1
                   LET    gs_Grabe   = 1
                   EXIT INPUT
         END INPUT
    ELSE
         LET        lc_curp            =   gc_curp
         DISPLAY    lc_curp           TO   lc_curp
         ### Verificar la curp
         FOREACH    d_SelAfi1      USING   lc_curp,
                                           gc_nti
                                    INTO   gr_afi_mae_afiliado.*
             LET    ls_encon           =   ls_encon  + 1
         END FOREACH

         IF  ls_encon                  =   0   THEN
             CALL   f_DespMen("CURP No registrada en Base De Datos o cuenta NO ACTIVA ....   ")
             LET    ls_Cancela         =   1
         ELSE
             LET    gr_ret_sol_issste_tx.nss                =  gr_afi_mae_afiliado.n_seguro
             LET    gr_ret_sol_issste_tx.curp               =  gr_afi_mae_afiliado.n_unico
             LET    lc_rfc                                  =  gr_afi_mae_afiliado.n_rfc
             LET    gr_ret_sol_issste_tx.rfc                =  gr_afi_mae_afiliado.n_rfc
             LET    gr_ret_sol_issste_tx.paterno_afore      =  gr_afi_mae_afiliado.paterno
             LET    gr_ret_sol_issste_tx.materno_afore      =  gr_afi_mae_afiliado.materno
             LET    gr_ret_sol_issste_tx.nombre_afore       =  gr_afi_mae_afiliado.nombres
             LET    gr_ret_sol_issste_tx.fecha_nacimiento   =  gr_afi_mae_afiliado.fena

             LET    gr_ret_parcial_issste.nss               =  gr_afi_mae_afiliado.n_seguro
             LET    gr_ret_parcial_issste.curp              =  gr_afi_mae_afiliado.n_unico
             LET    gr_ret_monto_par_issste.curp            =  gr_afi_mae_afiliado.n_unico
             LET    gr_ret_parcial_issste.nombre            =  gr_afi_mae_afiliado.nombres
             LET    gr_ret_parcial_issste.apellido_paterno  =  gr_afi_mae_afiliado.paterno
             LET    gr_ret_parcial_issste.apellido_materno  =  gr_afi_mae_afiliado.materno

             CALL   f_ObTInver(gr_ret_sol_issste_tx.curp)   RETURNING lc_tipo_inversion,
                                                                      lc_nss_issste

             LET    gr_ret_parcial_issste.num_issste        =  lc_nss_issste
             LET    lc_tipo_trabajador                      =  f_ObTTraba(gr_ret_sol_issste_tx.curp)
             LET    lc_tipo_solicitud                       =  f_ObTSolic(gr_afi_mae_afiliado.tipo_solicitud)
         END IF
    END IF

    LET     ls_DMar      = 0
    IF      ls_Cancela   = 0  THEN
        DISPLAY BY NAME
                gr_ret_sol_issste_tx.nss,
                lc_nss_issste,
                lc_rfc,
                gr_ret_sol_issste_tx.paterno_afore,
                gr_ret_sol_issste_tx.materno_afore,
                gr_ret_sol_issste_tx.nombre_afore,
                gr_ret_sol_issste_tx.fecha_nacimiento,
                lc_tipo_inversion,
                lc_tipo_trabajador,
                lc_tipo_solicitud

        ### Si la CURP existe cargar modalidad, no existe tabla ... tons a pie
        LET      li_x                           = 1
        LET      ra_mod[li_x].codigo            = "A"
        LET      ra_mod[li_x].descripcion       = "RETIRO TOTAL"
        LET      li_x                           = 2
        LET      ra_mod[li_x].codigo            = "B"
        LET      ra_mod[li_x].descripcion       = "RETIRO PARCIAL"

        CALL     SET_COUNT(li_x)
        DISPLAY  ARRAY ra_mod      TO scr_1.*
         
         ON KEY  ( CONTROL-C,  INTERRUPT)
                 LET li_x           = ARR_CURR()
                 EXIT DISPLAY

         ON KEY  ( CONTROL-M )
                 LET  li_x          = ARR_CURR()
                 EXIT DISPLAY
        END DISPLAY

        ### cargar tipo, tab_ret_issste
        IF       ra_mod[li_x].codigo                = 'A'  THEN
                 LET   li_y                         =  1
                 FOREACH   d_ObtTRe1                INTO   ra_tip[li_y].*
                     LET   li_y                     =  li_y    + 1
                 END FOREACH
        ELSE
                 LET   li_y                         =  1
                 FOREACH   d_ObtTRe2                INTO   ra_tip[li_y].*
                       LET   li_y                     =  li_y    + 1
                 END FOREACH
        END IF

        CALL     SET_COUNT(li_y-1)
        DISPLAY  ARRAY ra_tip      TO scr_2.*
            ATTRIBUTE(CURRENT ROW DISPLAY = "REVERSE")
             
             ON  KEY ( CONTROL-C,  INTERRUPT)
                    LET li_y           = ARR_CURR()
                    EXIT DISPLAY

             ON  KEY ( CONTROL-M )
                    LET li_y           = ARR_CURR()
                    EXIT DISPLAY
        END DISPLAY

        LET      gr_ret_sol_issste_tx.tipo_retiro      = ra_tip[li_y].tipo

        IF       gr_ret_sol_issste_tx.tipo_retiro     = 'F'   THEN
             ### Validar contra datamart Parciale 0 ---- no existe 1 no es 101   2- ok
             LET      ls_DMar           =     0
             CALL     f_VerData(lc_curp, gr_ret_sol_issste_tx.tipo_retiro)    RETURNING   ls_DMar
             IF       ls_DMar                 = 0
               OR     ls_DMar                 = 1   THEN
                      ###  No graba solicitud con error -- no existe en datamart
                      ###  CALL  f_Grabar(1)
                      CALL    f_DespMen("No Se Graba, Registro CURP NO EXISTE EN DATAMART Parciales")
                      LET     ls_Cancela        = 1
             ELSE
                      ### Validar contra matriz de derechos
                      -- Se toma directamente de datamart el NSS para poder obtener el dato correcto
                      -- CPL-1468

                      --LET gr_ret_parcial_issste.nss             = gr_DatMarPar.nss
                      LET gr_ret_parcial_issste.sec_pension     = gr_DatMarPar.sec_pension
                      LET gr_ret_parcial_issste.regimen         = gr_DatMarPar.regimen
                      LET gr_ret_parcial_issste.tipo_seguro     = gr_DatMarPar.tipo_seguro
                      LET gr_ret_parcial_issste.num_issste      = gr_DatMarPar.nss_issste
                      CALL f_VerMaDePar() RETURNING  ls_Made

                      IF ls_Made =  0   THEN
                            ### Tipo de retiro no existe en matriz derechos
                            ### CALL  f_Grabar(1)
                            CALL  f_DespMen("Tipo de Retiro no existe en MATRIZ DE DERECHOS")
                            LET   ls_Cancela        = 1
                      END IF
             END IF
        ELSE
             ### Validar contra datamart          0 ---- no existe 1 no es 101   2- ok
             LET     ls_DMar           =     0
             CALL    f_VerData(lc_curp, gr_ret_sol_issste_tx.tipo_retiro)    RETURNING   ls_DMar


             IF      ls_DMar                 = 0   THEN

#-- jgj
#-- se cambia porque se estara dando de alta directamente un retiro I

                     IF    gs_afores <> 578
                      AND  ls_DMar         =  0
                      AND  ls_Cancela      =  0    THEN
                           ### Tipo de retiro no existe en matriz derechos
                           ### CALL  f_Grabar(1)
                           CALL  f_DespMen("Tipo de Retiro no existe en MATRIZ DE DERECHOS")
                           LET   ls_Cancela        = 1
                     END IF
             ELSE
                IF   ls_DMar                 = 1   THEN
                     ### Solicitud esta rechazada en datamart
                     ### CALL  f_Grabar(1)
                     CALL    f_DespMen("No Se Graba, Registro EN DATAMART NO ACEPTADO")
                     LET     ls_Cancela        = 1
                ELSE

                     ### Validar contra matriz de derechos               ---- on existe
                     LET     gr_ret_sol_issste_tx.sec_pension      = gr_DatMar.sec_pension
                     LET     gr_ret_sol_issste_tx.regimen          = gr_DatMar.regimen
                     LET     gr_ret_sol_issste_tx.tipo_seguro      = gr_DatMar.tipo_seguro
                     LET     gr_ret_sol_issste_tx.tipo_pension     = gr_DatMar.tipo_pension
                     LET     gr_ret_sol_issste_tx.cve_pension      = gr_DatMar.cve_pension
                     LET     gr_ret_sol_issste_tx.tipo_prestacion  = gr_DatMar.tipo_prestacion
                     LET     gr_ret_sol_issste_tx.fecha_ini_pen    = gr_DatMar.fecha_ini_pen
                     LET     gr_ret_sol_issste_tx.fecha_resolucion = gr_DatMar.fecha_resolucion
                     LET     gr_ret_sol_issste_tx.semanas_cotizadas= gr_DatMar.semanas_cotizadas

                     LET     gr_ret_parcial_issste.regimen         = gr_DatMar.regimen
                     LET     gr_ret_parcial_issste.tipo_seguro     = gr_DatMar.tipo_seguro
                     LET     gr_ret_parcial_issste.num_concesion   = gr_DatMar.num_concesion
                     LET     gr_ret_parcial_issste.diag_procesar   = 0

                     CALL    f_VerMaDe(ra_tip[li_y].tipo)            RETURNING  ls_Made
                     IF      ls_Made         =  0   THEN
                             ### Tipo de retiro no existe en matriz derechos
                             ### CALL  f_Grabar(1)
                             CALL  f_DespMen("Tipo de Retiro no existe en MATRIZ DE DERECHOS")
                             LET   ls_Cancela        = 1
                     END IF
                END IF
             END IF
        END IF
    END IF

    ###  Verificar maracaje
    IF   ls_Cancela                    =   0 THEN
         LET   ls_VerMarca             =   0
         CALL  f_VerMarca(gr_ret_sol_issste_tx.tipo_retiro, gr_ret_sol_issste_tx.nss)   RETURNING ls_VerMarca, lc_Mensaje
         IF    ls_VerMarca             =   1  THEN
               CALL  f_DespMen(lc_Mensaje)
               LET   ls_Cancela        =   1
         ELSE
               LET   ls_Cancela        =   0
         END IF
    END IF


    IF   ls_Cancela                    =   0 THEN
         IF    gr_ret_sol_issste_tx.tipo_retiro       =  'F'    THEN
               FOREACH    d_SelGpo          USING     gr_ret_sol_issste_tx.tipo_retiro,
                                                      gr_DatMarPar.regimen,
                                                      gr_DatMarPar.tipo_seguro,
                                                      gr_DatMarPar.tipo_pension,
                                                      gr_DatMarPar.cve_pension,
                                                      gr_DatMarPar.tipo_prestacion
                                             INTO     lc_grupo
                  LET    gr_ret_parcial_issste.grupo  =   lc_grupo
               END FOREACH
               LET   ls_VerSaldo             =   0

               #CPL-1539
        	     #Identificar el nss con saldo
        	     DECLARE cur_nss CURSOR FOR
               SELECT n_seguro
               FROM   afi_mae_afiliado
               WHERE  n_unico = lc_curp
               ORDER BY 1

               FOREACH cur_nss INTO lr_valida_par.nss
                  CALL f_VerSaldo(lr_valida_par.nss, gr_ret_parcial_issste.grupo)
                  RETURNING ls_VerSaldo, lc_Mensaje

                  IF ls_VerSaldo = 1 THEN
                     #No hay saldo para el nss actial
                  ELSE
                  	 IF gr_ret_parcial_issste.nss <> lr_valida_par.nss THEN
                  	 	  #Actualizar datos con el nss bueno
                  	 	  SELECT *
                        INTO   gr_afi_mae_afiliado.*
                        FROM   afi_mae_afiliado
                        WHERE  n_seguro = lr_valida_par.nss

                  	 	  LET  gr_ret_sol_issste_tx.nss                =  lr_valida_par.nss

                        LET  gr_ret_sol_issste_tx.curp               =  gr_afi_mae_afiliado.n_unico
                        LET  lc_rfc                                  =  gr_afi_mae_afiliado.n_rfc
                        LET  gr_ret_sol_issste_tx.rfc                =  gr_afi_mae_afiliado.n_rfc
                        LET  gr_ret_sol_issste_tx.paterno_afore      =  gr_afi_mae_afiliado.paterno
                        LET  gr_ret_sol_issste_tx.materno_afore      =  gr_afi_mae_afiliado.materno
                        LET  gr_ret_sol_issste_tx.nombre_afore       =  gr_afi_mae_afiliado.nombres
                        LET  gr_ret_sol_issste_tx.fecha_nacimiento   =  gr_afi_mae_afiliado.fena

                        LET  gr_ret_parcial_issste.nss               =  gr_afi_mae_afiliado.n_seguro
                        LET  gr_ret_parcial_issste.curp              =  gr_afi_mae_afiliado.n_unico
                        LET  gr_ret_monto_par_issste.curp            =  gr_afi_mae_afiliado.n_unico
                        LET  gr_ret_parcial_issste.nombre            =  gr_afi_mae_afiliado.nombres
                        LET  gr_ret_parcial_issste.apellido_paterno  =  gr_afi_mae_afiliado.paterno
                        LET  gr_ret_parcial_issste.apellido_materno  =  gr_afi_mae_afiliado.materno

                        DISPLAY BY NAME gr_ret_sol_issste_tx.nss             ,
                                        lc_nss_issste                        ,
                                        lc_rfc                               ,
                                        gr_ret_sol_issste_tx.paterno_afore   ,
                                        gr_ret_sol_issste_tx.materno_afore   ,
                                        gr_ret_sol_issste_tx.nombre_afore    ,
                                        gr_ret_sol_issste_tx.fecha_nacimiento,
                                        lc_tipo_inversion                    ,
                                        lc_tipo_trabajador                   ,
                                        lc_tipo_solicitud
                     END IF
                     EXIT FOREACH
                  END IF
               END FOREACH

               #Verificar accion del saldo luego de validar todos los nss
               --CALL f_VerSaldo(gr_ret_parcial_issste.nss, gr_ret_parcial_issste.grupo)
               --RETURNING ls_VerSaldo, lc_Mensaje

               IF    ls_VerSaldo             =   1  THEN
                     CALL  f_DespMen(lc_Mensaje)
                     LET   ls_Cancela        =   1
               ELSE
                     LET   ls_Cancela        =   0
               END IF
         END IF

         IF    gr_ret_sol_issste_tx.tipo_retiro       =  'A'
          OR   gr_ret_sol_issste_tx.tipo_retiro       =  'B'
          OR   gr_ret_sol_issste_tx.tipo_retiro       =  'I'
          OR   gr_ret_sol_issste_tx.tipo_retiro       =  'K' THEN
               FOREACH    d_SelGpo          USING     gr_ret_sol_issste_tx.tipo_retiro,
                                                      gr_ret_sol_issste_tx.regimen,
                                                      gr_ret_sol_issste_tx.tipo_seguro,
                                                      gr_ret_sol_issste_tx.tipo_pension,
                                                      gr_ret_sol_issste_tx.cve_pension,
                                                      gr_ret_sol_issste_tx.tipo_prestacion
                                             INTO     lc_grupo
                  LET    gr_ret_sol_issste_tx.grupo   =   lc_grupo
               END FOREACH
               LET   ls_VerSaldo             =   0
               CALL  f_VerSaldo(gr_ret_sol_issste_tx.nss, gr_ret_sol_issste_tx.grupo)         RETURNING ls_VerSaldo, lc_Mensaje
               IF    ls_VerSaldo             =   1  THEN
                     CALL  f_DespMen(lc_Mensaje)
                     LET   ls_Cancela        =   1
               ELSE
                     LET   ls_Cancela        =   0
               END IF
        END IF
    END IF

    IF   ls_Cancela        =   0 THEN
         ### En este momento asigno  consecutivo
         IF    gr_ret_sol_issste_tx.tipo_retiro   = 'F'   THEN
               LET   gr_ret_parcial_issste.consecutivo   =  f_AsiCons()
               LET   gr_ret_monto_par_issste.consecutivo =  gr_ret_parcial_issste.consecutivo
               LET   gr_ret_solicitante.consecutivo      =  gr_ret_parcial_issste.consecutivo
         ELSE
               LET   gr_ret_sol_issste_tx.consecutivo    =  f_AsiCons()
               LET   gr_ret_solicitante.consecutivo      =  gr_ret_sol_issste_tx.consecutivo
         END IF
{
         IF gr_ret_sol_issste_tx.tipo_retiro = 'I' THEN
            LET gr_ret_sol_issste_tx.tipo_prestacion = 12
         END IF
}
         CALL   f_solicitud()

         IF    gs_Grabe   = 0
          AND  gs_Cance   = 0     THEN
               CALL f_Grabar(2)
         END IF
    END IF

    CLOSE  WINDOW RETM9202
END FUNCTION


### Detalle de Solicitud
FUNCTION  f_solicitud()
  DEFINE     ls_algo              SMALLINT

  CASE
      WHEN  gr_ret_sol_issste_tx.tipo_retiro   =  'A'    CALL   f_DespAB()
      WHEN  gr_ret_sol_issste_tx.tipo_retiro   =  'B'    CALL   f_DespAB()
      WHEN  gr_ret_sol_issste_tx.tipo_retiro   =  'I'    CALL   f_DespAB()
      WHEN  gr_ret_sol_issste_tx.tipo_retiro   =  'C'    CALL   f_DespC()
      WHEN  gr_ret_sol_issste_tx.tipo_retiro   =  'D'    CALL   f_DespD()
      WHEN  gr_ret_sol_issste_tx.tipo_retiro   =  'E'    CALL   f_DespE()
      WHEN  gr_ret_sol_issste_tx.tipo_retiro   =  'K'    CALL   f_DespK()
      WHEN  gr_ret_sol_issste_tx.tipo_retiro   =  'F'    CALL   f_DespF()
  END CASE
END FUNCTION


### Obtener tipo de inversión de una curp
FUNCTION f_ObTInver(lc_curp)
  DEFINE     lc_curp              CHAR(18)
  DEFINE     lc_tipo_inver        CHAR(100)
  DEFINE     lc_nss_issste        CHAR(11)

  FOREACH    d_ObTInv             USING     lc_curp
                                  INTO      lc_tipo_inver,
                                            lc_nss_issste
  END FOREACH

  IF  STATUS = NOTFOUND   THEN
      LET    lc_tipo_inver    = ""
  END IF
  IF         lc_tipo_inver = '01'   THEN
      LET    lc_tipo_inver  = lc_tipo_inver CLIPPED||'-AFORE SIEFORE'
  END IF

  IF         lc_tipo_inver = '07'   THEN
      LET    lc_tipo_inver  = lc_tipo_inver CLIPPED||'-AFORE-BANXICO (Cuenta ISSSTE)'
  END IF

  IF         lc_tipo_inver = '08'   THEN
      LET    lc_tipo_inver  = lc_tipo_inver CLIPPED||'-AFORE-SIEFORE y AFORE-BANXICO'
  END IF

  RETURN     lc_tipo_inver,
             lc_nss_issste
END FUNCTION


### Obtener tipo de trabajador de curp
FUNCTION  f_ObTTraba(lc_curp)
  DEFINE     lc_curp              CHAR(18)
  DEFINE     lc_tipo_traba        CHAR(100)

  FOREACH    d_ObTTra             USING     lc_curp
                                  INTO      lc_tipo_traba
  END FOREACH

  IF  STATUS = NOTFOUND   THEN
      LET    lc_tipo_traba        =         ""
  END IF
  RETURN     lc_tipo_traba
END FUNCTION


### Obtener tipo solicitud
FUNCTION  f_ObTSolic(ls_tipo)
  DEFINE     ls_tipo              SMALLINT
  DEFINE     lc_tipo_sol          CHAR(100)

  FOREACH    d_ObtTSol            USING     ls_tipo
                                  INTO      lc_tipo_sol
  END FOREACH

  IF  STATUS = NOTFOUND   THEN
      LET    lc_tipo_sol          =         ""
  END IF
  RETURN     lc_tipo_sol
END FUNCTION


### Validar exista DataMart
FUNCTION  f_VerData(lc_curp, lc_tipo_retiro)
  DEFINE     lc_curp              CHAR(18)
  DEFINE     lc_tipo_retiro       CHAR(01)
  DEFINE     ls_lei_DMar          INTEGER
  DEFINE     ls_Resu              SMALLINT
  DEFINE     lr_DatMar            RECORD LIKE safre_af:ret_datamart_issste.*

  LET         ls_lei_DMar       =         0
  IF          lc_tipo_retiro    =        'F'    THEN
       INITIALIZE  gr_DatMarPar.*        TO     NULL
       FOREACH     d_ObtDaMa1         USING     lc_curp
                                       INTO     gr_DatMarPar.*
           LET     ls_lei_DMar            =     ls_lei_DMar + 1
       END FOREACH
  ELSE
       INITIALIZE  gr_DatMar.*           TO     NULL
       FOREACH     d_ObtDaMa          USING     lc_curp,
                                                lc_tipo_retiro
                                       INTO     lr_DatMar.*
           LET     ls_lei_DMar            =     ls_lei_DMar + 1
           LET     gr_DatMar.*            =     lr_DatMar.*
       END FOREACH
  END IF

  IF      lc_tipo_retiro          = 'F'   THEN
      IF      STATUS = NOTFOUND
       OR     ls_lei_DMar             = 0   THEN

              ### no existe en datamart
              LET      ls_Resu        = 0
       ELSE
         IF   gr_DatMarPar.diag_datamart <>  101   THEN

              ###estatus diferente de aceptado en datamart
              LET      ls_Resu        = 1
         ELSE
              ### Continua correcto
              LET      ls_Resu        = 2
         END IF
      END IF
  ELSE
      IF      STATUS = NOTFOUND
       OR     ls_lei_DMar             = 0   THEN

              ### no existe en datamart
              LET      ls_Resu        = 0
       ELSE
         IF   gr_DatMar.diag_datamart <>  101   THEN

              ###estatus diferente de aceptado en datamart
              LET      ls_Resu        = 1
         ELSE
              ### Continua correcto
              LET      ls_Resu        = 2
         END IF
      END IF
  END IF

  ### Cambio de validación si retiro CDE no importa que no este en datamart es correcto
  IF      gr_ret_sol_issste_tx.tipo_retiro   =   "C"
   OR     gr_ret_sol_issste_tx.tipo_retiro   =   "D"
   OR     gr_ret_sol_issste_tx.tipo_retiro   =   "E"   THEN
          INITIALIZE  gr_DatMar.*            TO  NULL
          LET         ls_Resu                =   2
  END IF
  RETURN  ls_Resu
END FUNCTION


### Valida exista en Matriz
FUNCTION f_VerMaDe(lc_tipo)
  DEFINE     lc_tipo          CHAR(01)
  DEFINE     ls_Resu          SMALLINT

  LET        ls_Resu          = 0
  EXECUTE    p_ObtMaDe        USING   lc_tipo,
                                      gr_DatMar.regimen,
                                      gr_DatMar.tipo_seguro,
                                      gr_DatMar.tipo_pension,
                                      gr_DatMar.tipo_prestacion
                              INTO    ls_Resu
  IF    STATUS  = NOTFOUND   THEN
        LET     ls_Resu   = 0
  ELSE
        LET     ls_Resu   = 1
  END IF
  ### Cambio de validación si retiro CDE no importa que no este en datamart es correcto
  ### por lo tanto sus valores son nullos y no estaran en matriz
  IF      gr_ret_sol_issste_tx.tipo_retiro   =   "C"
   OR     gr_ret_sol_issste_tx.tipo_retiro   =   "D"
   OR     gr_ret_sol_issste_tx.tipo_retiro   =   "E"   THEN
          INITIALIZE gr_DatMar.*             TO NULL
          LET         ls_Resu                =   1
  END IF
  RETURN   ls_Resu
END FUNCTION


### Valida exista en Matriz Parcial
FUNCTION f_VerMaDePar()
  DEFINE     lc_tipo          CHAR(01)
  DEFINE     ls_Resu          SMALLINT
  DEFINE     la_efe           CHAR(01)

  LET        ls_Resu          = 0
  LET        la_efe           = 'F'
  EXECUTE    p_ObtMaDe        USING   la_efe,
                                      gr_DatMarPar.regimen,
                                      gr_DatMarPar.tipo_seguro,
                                      gr_DatMarPar.tipo_pension,
                                      gr_DatMarPar.tipo_prestacion
                              INTO    ls_Resu
  IF    STATUS  = NOTFOUND   THEN
        LET     ls_Resu   = 0
  ELSE
        LET     ls_Resu   = 1
  END IF

  RETURN   ls_Resu
END FUNCTION


###  Calcula consecutivo
FUNCTION  f_AsiCons()
  DEFINE     ls_Resu              INTEGER

  SELECT   MAX(consecutivo)
    INTO   ls_Resu
    FROM   ret_consecutivo

  LET   ls_Resu  = ls_Resu + 1

  INSERT   INTO ret_consecutivo VALUES( ls_Resu )

  RETURN   ls_Resu

END FUNCTION


### Grabar la solicitu
FUNCTION  f_Grabar(ls_clave)
  DEFINE     ls_clave            SMALLINT
  DEFINE     v_ejecuta           CHAR(100)
  DEFINE     ls_cuantos          SMALLINT
  DEFINE     ls_ResMar           SMALLINT

  ### Clave 1 graba capturada con error
  ### Clave 2 graba por seleccion de Grabar y gs_Grabe  = 1 ya grabo  tons debe ser 0

  IF    gs_Grabe = 0     THEN
        IF    ls_clave    = 1    THEN
              ###  Graba solicitud capturada con errores en validación ...
              ###  Inactiva por el momento
              LET  gs_Grabe = 1
        ELSE
              CALL   f_DespMen("Guardando información .... Espere un momento   ")

              LET  ls_ResMar    = 0
              CALL f_marcaje(gr_ret_sol_issste_tx.nss,
                             gr_ret_sol_issste_tx.consecutivo,
                             gr_ret_sol_issste_tx.tipo_retiro)   RETURNING ls_ResMar

              IF   ls_ResMar        =   0  THEN

                   -- CPL-1696
                   -- Se almacena primero el retiro para poder modificar a los 
                   -- beneficiarios
                   EXECUTE   p_InsRetiro     USING  gr_ret_sol_issste_tx.*

                   SELECT  COUNT(*)
                     INTO  ls_cuantos
                     FROM  ret_beneficiario
                    WHERE  nss         = gr_ret_sol_issste_tx.nss
                      AND  consecutivo = gr_ret_sol_issste_tx.consecutivo

                   IF  STATUS = NOTFOUND
                     OR ls_cuantos = 0 THEN

                        LET   v_ejecuta = "fglgo RETM810 ", gr_ret_sol_issste_tx.nss CLIPPED,         " ",
                                                            gr_ret_sol_issste_tx.consecutivo CLIPPED, " ",
                                                            "A", " ",
                                                            "1", " ",
                                                            gc_num_cuenta
                        RUN   v_ejecuta
                   END IF

                   IF      gs_afores         = 578   THEN
                       IF      gr_ret_sol_issste_tx.deleg_cod IS NOT NULL
                         AND   gr_ret_sol_issste_tx.estad_cod IS NOT NULL THEN
    	                     LET gr_ret_sol_issste_tx.deleg_cod =   gr_ret_sol_issste_tx.estad_cod * 1000
                                                                +   gr_ret_sol_issste_tx.deleg_cod
                       END IF
                   END IF

                   EXECUTE   p_InsSolici     USING  gr_ret_solicitante.*

                   #CPL-1844
                   INSERT INTO ret_sol_issste_constancia VALUES(gr_ret_sol_issste_tx.nss        ,
                                                                gr_ret_sol_issste_tx.consecutivo,
                                                                gr_constancia.constancia)
                   LET   gs_Grabe = 1
              END IF
        END IF
  END IF

END FUNCTION

###  Despliega información de AyB
FUNCTION  f_DespAB()
  DEFINE     ls_encon             SMALLINT
  DEFINE     ls_Cancela           SMALLINT
  DEFINE     lc_Paren             CHAR(100)
  DEFINE     lc_telef             CHAR(50)
  DEFINE     fecha_solicitud      DATE
  DEFINE     folio                LIKE   ret_sol_issste_tx.folio_solicitud
  DEFINE     nuevo                CHAR(01)
  DEFINE     paterno_afore        CHAR(100)
  DEFINE     materno_afore        CHAR(100)
  DEFINE     nombre_afore         CHAR(100)
  DEFINE     semanas_cotizadas    LIKE   ret_sol_issste_tx.semanas_cotizadas
  DEFINE     lc_grupo             LIKE   ret_matriz_derecho_issste.grupo
  DEFINE     ls_folio             SMALLINT
  DEFINE     lc_estad_desc        CHAR(100)
  DEFINE     lc_deleg_desc        CHAR(100)

  OPEN   WINDOW RETM9203 AT 2,2 WITH FORM "RETM9203"                                                 ATTRIBUTE(BORDER)

  DISPLAY "[Ctrl-C] Cancelar                                                               " AT 01,1 ATTRIBUTE(REVERSE)
  DISPLAY "DATOS DEL SOLICITANTE                                                           " AT 03,1 ATTRIBUTE(REVERSE)
  DISPLAY "DATOS PARTICULARES DE LA SOLICITUD DE RETIRO                                    " AT 07,1 ATTRIBUTE(REVERSE)
  DISPLAY "DATOS DE CONTROL INTERNO                                                        " AT 18,1 ATTRIBUTE(REVERSE)

  LET       ls_encon             =      0
  LET       ls_Cancela           =      0
  LET       lc_Paren             =      '12-TITULAR '
  LET       lc_telef             =      0

  IF      gs_afores         = 578   THEN
       CALL    f_VerCAV(gr_ret_sol_issste_tx.folio_solicitud)   RETURNING  ls_folio
  ELSE
       LET     folio  = gr_ret_sol_issste_tx.folio_solicitud
  END IF

  DISPLAY BY NAME
          gr_ret_sol_issste_tx.paterno_afore,
          gr_ret_sol_issste_tx.materno_afore,
          gr_ret_sol_issste_tx.nombre_afore,
          lc_Paren,
          lc_telef,

          gr_ret_sol_issste_tx.sec_pension,
          gr_ret_sol_issste_tx.semanas_cotizadas,

          gr_ret_sol_issste_tx.consecutivo,
          gr_ret_sol_issste_tx.usuario_captura,
          gr_ret_sol_issste_tx.fecha_captura,
          gr_ret_sol_issste_tx.fecha_solicitud,
          folio

  DISPLAY gr_ret_sol_issste_tx.folio_solicitud TO folio
  --- PEIS DISPLAY gc_num_cuenta                        TO num_cuenta

  CALL  f_InpSemCot2(gr_ret_sol_issste_tx.semanas_cotizadas,
                     gr_ret_solicitante.telefono,
                     gr_ret_sol_issste_tx.estad_cod,
                     gr_ret_sol_issste_tx.deleg_cod,
                     gr_ret_sol_issste_tx.regimen,
                     gr_ret_sol_issste_tx.tipo_seguro,
                     gr_ret_sol_issste_tx.tipo_pension,
                     gr_ret_sol_issste_tx.cve_pension,
                     gr_ret_sol_issste_tx.tipo_prestacion)
                     RETURNING gr_ret_sol_issste_tx.semanas_cotizadas,
                               gr_ret_solicitante.telefono,
                               gr_ret_sol_issste_tx.estad_cod,
                               gr_ret_sol_issste_tx.deleg_cod

  IF    gs_Cance   = 0 THEN
      FOREACH    d_SelGpo          USING      gr_ret_sol_issste_tx.tipo_retiro,
                                              gr_ret_sol_issste_tx.regimen,
                                              gr_ret_sol_issste_tx.tipo_seguro,
                                              gr_ret_sol_issste_tx.tipo_pension,
                                              gr_ret_sol_issste_tx.cve_pension,
                                              gr_ret_sol_issste_tx.tipo_prestacion
                                   INTO       lc_grupo
          LET    gr_ret_sol_issste_tx.grupo   =   lc_grupo
      END FOREACH
      CALL  f_InputComun(gr_ret_sol_issste_tx.*, gr_ret_solicitante.*) RETURNING gr_ret_sol_issste_tx.*, gr_ret_solicitante.*
  END IF

CLOSE WINDOW RETM9203

END FUNCTION


### Despliega información de C
FUNCTION  f_DespC()
  DEFINE     ls_encon             SMALLINT
  DEFINE     ls_Cancela           SMALLINT
  DEFINE     lc_Paren             CHAR(100)
  DEFINE     lc_telef             CHAR(50)
  DEFINE     fecha_solicitud      DATE
  DEFINE     folio                LIKE   ret_sol_issste_tx.folio_solicitud
  DEFINE     nuevo                CHAR(01)
  DEFINE     paterno_afore        CHAR(100)
  DEFINE     materno_afore        CHAR(100)
  DEFINE     nombre_afore         CHAR(100)
  DEFINE     aseguradora          LIKE ret_sol_issste_tx.aseguradora
  DEFINE     actuario             LIKE ret_sol_issste_tx.actuario
  DEFINE     num_plan_privado     LIKE ret_sol_issste_tx.num_plan_privado
  DEFINE     semanas_cotizadas    LIKE ret_sol_issste_tx.semanas_cotizadas
  DEFINE     ls_folio             SMALLINT
  DEFINE     lc_estad_desc        CHAR(100)
  DEFINE     lc_deleg_desc        CHAR(100)

  OPEN   WINDOW RETM9204 AT 2,2 WITH FORM "RETM9204"                                               ATTRIBUTE(BORDER)

  DISPLAY "[Esc] Terminar [Ctrl-C] Cancelar                                                " AT 01,1 ATTRIBUTE(REVERSE)
  DISPLAY "DATOS DEL SOLICITANTE                                                           " AT 03,1 ATTRIBUTE(REVERSE)
  DISPLAY "DATOS PARTICULARES DE LA SOLICITUD DE RETIRO                                    " AT 07,1 ATTRIBUTE(REVERSE)
  DISPLAY "DATOS DE CONTROL INTERNO                                                        " AT 18,1 ATTRIBUTE(REVERSE)

  LET       ls_encon             =      0
  LET       ls_Cancela           =      0
  LET       lc_Paren             =      '12-TITULAR '
  LET       lc_telef             =      0

  IF      gs_afores         = 578   THEN
       CALL    f_VerCAV(gr_ret_sol_issste_tx.folio_solicitud)   RETURNING  ls_folio
  ELSE
       LET     folio  = gr_ret_sol_issste_tx.folio_solicitud
  END IF

  DISPLAY BY NAME
          gr_ret_sol_issste_tx.paterno_afore,
          gr_ret_sol_issste_tx.materno_afore,
          gr_ret_sol_issste_tx.nombre_afore,
          lc_Paren,
          lc_telef,

          gr_ret_sol_issste_tx.regimen,
          gr_ret_sol_issste_tx.tipo_seguro,
          gr_ret_sol_issste_tx.tipo_pension,
          gr_ret_sol_issste_tx.cve_pension,
          gr_ret_sol_issste_tx.tipo_prestacion,
          gr_ret_sol_issste_tx.aseguradora,
          gr_ret_sol_issste_tx.actuario,
          gr_ret_sol_issste_tx.num_plan_privado,
          gr_ret_sol_issste_tx.semanas_cotizadas,

          gr_ret_sol_issste_tx.consecutivo,
          gr_ret_sol_issste_tx.usuario_captura,
          gr_ret_sol_issste_tx.fecha_captura,
          gr_ret_sol_issste_tx.fecha_solicitud,
          folio

  --- PEIS DISPLAY gc_num_cuenta                        TO  num_cuenta
  DISPLAY gr_ret_sol_issste_tx.folio_solicitud TO folio

  ### Input de los datos del C
  CALL  f_InpdelC("C")
  DISPLAY "[Esc] Continuar [Ctrl-C] Cancelar                                                " AT 01,1 ATTRIBUTE(REVERSE)

  IF    gs_Cance   = 0 THEN
        ### Input de los datos q faltan
        CALL  f_MasdelC(gr_ret_sol_issste_tx.*,
                        gr_ret_solicitante.telefono)      RETURNING gr_ret_sol_issste_tx.*,
                                                                    gr_ret_solicitante.telefono
  END IF

  IF    gs_Cance   = 0 THEN
        ### Input de los datos comunes
        CALL  f_InputComun(gr_ret_sol_issste_tx.*, gr_ret_solicitante.*) RETURNING gr_ret_sol_issste_tx.*, gr_ret_solicitante.*
  END IF

  CLOSE WINDOW RETM9204
END FUNCTION


### Despliega información D
FUNCTION  f_DespD()
  DEFINE     ls_encon             SMALLINT
  DEFINE     ls_Cancela           SMALLINT
  DEFINE     lc_Paren             CHAR(100)
  DEFINE     lc_telef             CHAR(50)
  DEFINE     fecha_solicitud      DATE
  DEFINE     folio                LIKE   ret_sol_issste_tx.folio_solicitud
  DEFINE     nuevo                CHAR(01)
  DEFINE     paterno_afore        CHAR(100)
  DEFINE     materno_afore        CHAR(100)
  DEFINE     nombre_afore         CHAR(100)
  DEFINE     aseguradora          LIKE ret_sol_issste_tx.aseguradora
  DEFINE     actuario             LIKE ret_sol_issste_tx.actuario
  DEFINE     num_plan_privado     LIKE ret_sol_issste_tx.num_plan_privado
  DEFINE     semanas_cotizadas    LIKE ret_sol_issste_tx.semanas_cotizadas
  DEFINE     ls_folio             SMALLINT
  DEFINE     lc_estad_desc        CHAR(100)
  DEFINE     lc_deleg_desc        CHAR(100)

  OPEN   WINDOW RETM9205 AT 2,2 WITH FORM "RETM9205"                                                 ATTRIBUTE(BORDER)

  DISPLAY "[Esc] Terminar [Ctrl-C] Cancelar                                                " AT 01,1 ATTRIBUTE(REVERSE)
  DISPLAY "DATOS DEL SOLICITANTE                                                           " AT 03,1 ATTRIBUTE(REVERSE)
  DISPLAY "DATOS PARTICULARES DE LA SOLICITUD DE RETIRO                                    " AT 07,1 ATTRIBUTE(REVERSE)
  DISPLAY "DATOS DE CONTROL INTERNO                                                        " AT 18,1 ATTRIBUTE(REVERSE)

  LET       ls_encon             =      0
  LET       ls_Cancela           =      0
  LET       lc_Paren             =      '12-TITULAR '
  LET       lc_telef             =      0

  IF      gs_afores         = 578   THEN
       CALL    f_VerCAV(gr_ret_sol_issste_tx.folio_solicitud)   RETURNING  ls_folio
  ELSE
       LET     folio  = gr_ret_sol_issste_tx.folio_solicitud
  END IF

  DISPLAY BY NAME
          gr_ret_sol_issste_tx.paterno_afore,
          gr_ret_sol_issste_tx.materno_afore,
          gr_ret_sol_issste_tx.nombre_afore,
          lc_Paren,
          lc_telef,

          gr_ret_sol_issste_tx.regimen,
          gr_ret_sol_issste_tx.tipo_seguro,
          gr_ret_sol_issste_tx.tipo_pension,
          gr_ret_sol_issste_tx.cve_pension,
          gr_ret_sol_issste_tx.tipo_prestacion,
          gr_ret_sol_issste_tx.semanas_cotizadas,

          gr_ret_sol_issste_tx.consecutivo,
          gr_ret_sol_issste_tx.usuario_captura,
          gr_ret_sol_issste_tx.fecha_captura,
          gr_ret_sol_issste_tx.fecha_solicitud,
          folio

  --- PEIS DISPLAY gc_num_cuenta                        TO num_cuenta
  DISPLAY gr_ret_sol_issste_tx.folio_solicitud TO folio

  ### Input de los datos del D
  CALL  f_InpdelC("D")
  DISPLAY "[Esc] Terminar [Ctrl-C] Cancelar                                                " AT 01,1 ATTRIBUTE(REVERSE)

  IF    gs_Cance   = 0 THEN
        ### Input de los datos q faltan
        DISPLAY "[Esc] Continuar [Ctrl-C] Cancelar                                                " AT 01,1 ATTRIBUTE(REVERSE)
        CALL  f_InpSemCot(gr_ret_sol_issste_tx.semanas_cotizadas,
                          gr_ret_solicitante.telefono,
                          gr_ret_sol_issste_tx.estad_cod,
                          gr_ret_sol_issste_tx.deleg_cod)
                          RETURNING gr_ret_sol_issste_tx.semanas_cotizadas,
                                    gr_ret_solicitante.telefono,
                                    gr_ret_sol_issste_tx.estad_cod,
                                    gr_ret_sol_issste_tx.deleg_cod
  END IF
  IF    gs_Cance   = 0 THEN
        ### Input de los datos comunes
        CALL  f_InputComun(gr_ret_sol_issste_tx.*, gr_ret_solicitante.*) RETURNING gr_ret_sol_issste_tx.*, gr_ret_solicitante.*
  END IF

  CLOSE WINDOW RETM9205

END FUNCTION


### Despliega información E
FUNCTION  f_DespE()
  DEFINE     ls_encon             SMALLINT
  DEFINE     ls_Cancela           SMALLINT
  DEFINE     lc_Paren             CHAR(100)
  DEFINE     lc_telef             CHAR(50)
  DEFINE     fecha_solicitud      DATE
  DEFINE     folio                LIKE   ret_sol_issste_tx.folio_solicitud
  DEFINE     nuevo                CHAR(01)
  DEFINE     paterno_afore        CHAR(100)
  DEFINE     materno_afore        CHAR(100)
  DEFINE     nombre_afore         CHAR(100)
  DEFINE     aseguradora          LIKE ret_sol_issste_tx.aseguradora
  DEFINE     actuario             LIKE ret_sol_issste_tx.actuario
  DEFINE     num_plan_privado     LIKE ret_sol_issste_tx.num_plan_privado
  DEFINE     semanas_cotizadas    LIKE ret_sol_issste_tx.semanas_cotizadas
  DEFINE     cve_docto            LIKE ret_sol_issste_tx.cve_doc_probatorio
  DEFINE     fecha_nacimiento     LIKE ret_sol_issste_tx.fecha_nacimiento
  DEFINE     ls_DocPro            SMALLINT
  DEFINE     ls_folio             SMALLINT
  DEFINE     lc_estad_desc        CHAR(100)
  DEFINE     lc_deleg_desc        CHAR(100)

  OPEN   WINDOW RETM9206 AT 2,2 WITH FORM "RETM9206"                                                 ATTRIBUTE(BORDER)

  DISPLAY "[Esc] Terminar [Ctrl-C] Cancelar                                                " AT 01,1 ATTRIBUTE(REVERSE)
  DISPLAY "DATOS DEL SOLICITANTE                                                           " AT 03,1 ATTRIBUTE(REVERSE)
  DISPLAY "DATOS PARTICULARES DE LA SOLICITUD DE RETIRO                                    " AT 07,1 ATTRIBUTE(REVERSE)
  DISPLAY "DATOS DE CONTROL INTERNO                                                        " AT 18,1 ATTRIBUTE(REVERSE)

  LET       ls_encon             =      0
  LET       ls_Cancela           =      0
  LET       lc_Paren             =      '12-TITULAR '
  LET       lc_telef             =      0

  IF      gs_afores         = 578   THEN
       CALL    f_VerCAV(gr_ret_sol_issste_tx.folio_solicitud)   RETURNING  ls_folio
  ELSE
       LET     folio  = gr_ret_sol_issste_tx.folio_solicitud
  END IF

  DISPLAY gr_ret_sol_issste_tx.cve_doc_probatorio    TO  scr_doc[1].cve_docto
  DISPLAY BY NAME
          gr_ret_sol_issste_tx.paterno_afore,
          gr_ret_sol_issste_tx.materno_afore,
          gr_ret_sol_issste_tx.nombre_afore,
          lc_Paren,
          lc_telef,

          gr_ret_sol_issste_tx.regimen,
          gr_ret_sol_issste_tx.tipo_seguro,
          gr_ret_sol_issste_tx.tipo_pension,
          gr_ret_sol_issste_tx.cve_pension,
          gr_ret_sol_issste_tx.tipo_prestacion,
          -- gr_ret_sol_issste_tx.cve_doc_probatorio,
          gr_ret_sol_issste_tx.fecha_nacimiento,
          gr_ret_sol_issste_tx.semanas_cotizadas,

          gr_ret_sol_issste_tx.consecutivo,
          gr_ret_sol_issste_tx.usuario_captura,
          gr_ret_sol_issste_tx.fecha_captura,
          gr_ret_sol_issste_tx.fecha_solicitud,
          folio

  --- PEIS DISPLAY gc_num_cuenta                        TO num_cuenta
  DISPLAY gr_ret_sol_issste_tx.folio_solicitud TO folio

  ### Input de los datos del E
  CALL  f_InpdelC("E")
  DISPLAY "[Esc] Terminar [Ctrl-C] Cancelar                                                " AT 01,1 ATTRIBUTE(REVERSE)

  IF    gs_Cance   = 0 THEN
        ### Input de los datos q faltan
        DISPLAY "[Esc] Continuar [Ctrl-C] Cancelar                                                " AT 01,1 ATTRIBUTE(REVERSE)
        CALL  f_InpMasE(gr_ret_sol_issste_tx.*,
                        gr_ret_solicitante.telefono)  RETURNING gr_ret_sol_issste_tx.*,
                                                                gr_ret_solicitante.telefono
  END IF

  IF    gs_Cance   = 0 THEN
        ### Input de los datos comunes
        CALL  f_InputComun(gr_ret_sol_issste_tx.*, gr_ret_solicitante.*) RETURNING gr_ret_sol_issste_tx.*, gr_ret_solicitante.*
  END IF

  CLOSE WINDOW RETM9206
END FUNCTION


### Despliega información K
FUNCTION f_DespK()
  DEFINE     ls_encon             SMALLINT
  DEFINE     ls_Cancela           SMALLINT
  DEFINE     lc_Paren             CHAR(100)
  DEFINE     lc_telef             CHAR(50)
  DEFINE     fecha_solicitud      DATE
  DEFINE     folio                LIKE   ret_sol_issste_tx.folio_solicitud
  DEFINE     nuevo                CHAR(01)
  DEFINE     paterno_afore        CHAR(100)
  DEFINE     materno_afore        CHAR(100)
  DEFINE     nombre_afore         CHAR(100)
  DEFINE     sec_pension          LIKE ret_sol_issste_tx.sec_pension
  DEFINE     fecha_ini_pen        LIKE ret_sol_issste_tx.fecha_ini_pen
  DEFINE     fecha_resolucion     LIKE ret_sol_issste_tx.fecha_resolucion
  DEFINE     semanas_cotizadas    LIKE ret_sol_issste_tx.semanas_cotizadas
  DEFINE     periodo_pago         LIKE ret_sol_issste_tx.periodo_pago
  DEFINE     ls_DocPro            SMALLINT
  DEFINE     lc_periodo_pago      CHAR(10)
  DEFINE     ls_folio             SMALLINT
  DEFINE     lc_estad_desc        CHAR(100)
  DEFINE     lc_deleg_desc        CHAR(100)

  OPEN   WINDOW RETM9207 AT 2,2 WITH FORM "RETM9207"                                                 ATTRIBUTE(BORDER)


  DISPLAY "[Esc] Terminar [Ctrl-C] Cancelar                                                " AT 01,1 ATTRIBUTE(REVERSE)
  DISPLAY "DATOS DEL SOLICITANTE                                                           " AT 03,1 ATTRIBUTE(REVERSE)
  DISPLAY "DATOS PARTICULARES DE LA SOLICITUD DE RETIRO                                    " AT 07,1 ATTRIBUTE(REVERSE)
  DISPLAY "DATOS DE CONTROL INTERNO                                                        " AT 18,1 ATTRIBUTE(REVERSE)

  LET       ls_encon             =      0
  LET       ls_Cancela           =      0
  LET       lc_Paren             =      '12-TITULAR '
  LET       lc_telef             =      0

  IF      gs_afores         = 578   THEN
       CALL    f_VerCAV(gr_ret_sol_issste_tx.folio_solicitud)   RETURNING  ls_folio
  ELSE
       LET     folio  = gr_ret_sol_issste_tx.folio_solicitud
  END IF

  DISPLAY BY NAME
          gr_ret_sol_issste_tx.paterno_afore,
          gr_ret_sol_issste_tx.materno_afore,
          gr_ret_sol_issste_tx.nombre_afore,
          lc_Paren,
          lc_telef,

          gr_ret_sol_issste_tx.regimen,
          gr_ret_sol_issste_tx.tipo_seguro,
          gr_ret_sol_issste_tx.tipo_pension,
          gr_ret_sol_issste_tx.cve_pension,
          gr_ret_sol_issste_tx.tipo_prestacion,
          gr_ret_sol_issste_tx.sec_pension,
          gr_ret_sol_issste_tx.fecha_ini_pen,
          gr_ret_sol_issste_tx.fecha_resolucion,
          gr_ret_sol_issste_tx.periodo_pago,

          gr_ret_sol_issste_tx.consecutivo,
          gr_ret_sol_issste_tx.usuario_captura,
          gr_ret_sol_issste_tx.fecha_captura,
          gr_ret_sol_issste_tx.fecha_solicitud,
          folio

  --- PEIS DISPLAY gc_num_cuenta                        TO num_cuenta
  DISPLAY gr_ret_sol_issste_tx.folio_solicitud TO folio

  ### Input de los datos del K
  CALL  f_InpdelC("K")
  DISPLAY "[Esc] Terminar [Ctrl-C] Cancelar                                                " AT 01,1 ATTRIBUTE(REVERSE)

  IF    gs_Cance   = 0 THEN
        ### Input de los datos q faltan
        DISPLAY "[Esc] Continuar [Ctrl-C] Cancelar                                                " AT 01,1 ATTRIBUTE(REVERSE)
        CALL f_InpMasK(gr_ret_sol_issste_tx.*,
                       gr_ret_solicitante.telefono)  RETURNING gr_ret_sol_issste_tx.*,
                                                               gr_ret_solicitante.telefono
  END IF

  IF    gs_Cance   = 0 THEN
        ### Input de los datos comunes
        CALL  f_InputComun(gr_ret_sol_issste_tx.*, gr_ret_solicitante.*) RETURNING gr_ret_sol_issste_tx.*, gr_ret_solicitante.*
  END IF

  CLOSE WINDOW RETM9207
END FUNCTION


### Despliega y captura datos comunes
FUNCTION f_InputComun(gr_ret_sol_issste_tx, gr_ret_solicitante)
  DEFINE     gr_ret_sol_issste_tx     RECORD LIKE ret_sol_issste_tx.*
  DEFINE     gr_ret_solicitante       RECORD LIKE ret_solicitante.*
  DEFINE     fecha_solicitud          DATE
  DEFINE     folio                    LIKE   ret_sol_issste_tx.folio_solicitud
  DEFINE     lc_Paren                 CHAR(100)
  DEFINE     lc_Telef                 CHAR(100)
  DEFINE     paterno_afore            CHAR(100)
  DEFINE     materno_afore            CHAR(100)
  DEFINE     nombre_afore             CHAR(100)
  DEFINE     li_x1                    SMALLINT
  DEFINE     la_benef                 ARRAY[15]   OF RECORD
     paren_desc                       CHAR(100)
  END RECORD
  DEFINE     ls_paren_cod             LIKE  tab_parentesco.paren_cod
  DEFINE     ls_folio                 SMALLINT
  DEFINE     ls_recaptura_folio       SMALLINT
  DEFINE     num_cuenta               LIKE  ret_beneficiario.num_cuenta

  LET   paterno_afore               = ""
  LET   materno_afore               = ""
  LET   nombre_afore                = ""
  LET   lc_Paren                    = ""
  LET   lc_Telef                    = "0"

  IF      gr_ret_sol_issste_tx.tipo_pension = 'VI'
   OR     gr_ret_sol_issste_tx.tipo_pension = 'VO'
   OR     gr_ret_sol_issste_tx.tipo_pension = 'OR'
   OR     gr_ret_sol_issste_tx.tipo_pension = 'AS'  THEN

       LET   gs_Cance        =    0
       LET   gs_Muerto       =    1
       DISPLAY "[Ctrl-C] Cancelar                                                              " AT 01,1 ATTRIBUTE(REVERSE)
       WHILE TRUE
            INPUT BY NAME paterno_afore,
                          materno_afore,
                          nombre_afore
                          WITHOUT DEFAULTS

               BEFORE FIELD paterno_afore
                  LET   paterno_afore     = gr_ret_solicitante.apellido_paterno

               BEFORE FIELD materno_afore
                  LET   materno_afore     = gr_ret_solicitante.apellido_materno

               BEFORE FIELD nombre_afore
                  LET   nombre_afore      = gr_ret_solicitante.nombres

               ON KEY (CONTROL-C,  INTERRUPT)
                  LET   gs_Cance = 1
                  EXIT  INPUT

               AFTER INPUT
                  IF  paterno_afore    IS NULL  THEN
                      CALL   f_DespMen("APELLIDO PATERNO No puede quedar en blanco ")
                      NEXT FIELD paterno_afore
                  ELSE
                      LET  gr_ret_solicitante.apellido_paterno  = paterno_afore
                  END IF

                  IF  materno_afore    IS NULL  THEN
                      CALL   f_DespMen("APELLIDO MATERNO No puede quedar en blanco ")
                      NEXT FIELD materno_afore
                  ELSE
                      LET  gr_ret_solicitante.apellido_materno  = materno_afore
                  END IF

                  IF  nombre_afore     IS NULL  THEN
                      CALL   f_DespMen("NOMBRE No puede quedar en blanco ")
                      NEXT FIELD nombre_afore
                  ELSE
                      LET  gr_ret_solicitante.nombres  = nombre_afore
                  END IF

                  LET   gs_Cance = 0
                  EXIT INPUT

               AFTER FIELD paterno_afore
                  IF  paterno_afore    IS NULL  THEN
                      CALL   f_DespMen("APELLIDO PATERNO No puede quedar en blanco ")
                      NEXT FIELD paterno_afore
                  ELSE
                      LET   gr_ret_solicitante.apellido_paterno = paterno_afore
                  END IF

               AFTER FIELD materno_afore
                  IF  materno_afore    IS NULL  THEN
                      CALL   f_DespMen("APELLIDO MATERNO No puede quedar en blanco ")
                      NEXT FIELD materno_afore
                  ELSE
                      LET   gr_ret_solicitante.apellido_materno = materno_afore
                  END IF

               AFTER FIELD nombre_afore
                  IF  nombre_afore     IS NULL  THEN
                      CALL   f_DespMen("NOMBRE No puede quedar en blanco ")
                      NEXT FIELD nombre_afore
                  ELSE
                      LET   gr_ret_solicitante.nombres = nombre_afore
                      EXIT INPUT
                  END IF

            END INPUT
            IF  paterno_afore    IS NULL
             OR materno_afore    IS NULL
             OR nombre_afore     IS NULL THEN
                IF    gs_Cance = 1   THEN
                     EXIT WHILE
                END IF
            ELSE
                EXIT WHILE
            END IF

       END WHILE
       IF         gs_Cance          =          0   THEN
            DISPLAY "[Flechas] Seleccionar [Ctrl-C] Cancelar                                         " AT 01,1 ATTRIBUTE(REVERSE)
            LET        li_x1             =          1
            FOREACH    d_SelBen          INTO       la_benef[li_x1].*
                LET    li_x1             =          li_x1   +  1
            END FOREACH
            CALL SET_COUNT(li_x1 - 1)
            DISPLAY ARRAY la_benef  TO   scr_1.*
                 ON KEY ( CONTROL-C,  INTERRUPT)
                      LET li_x1          = ARR_CURR()
                      EXIT DISPLAY

                 ON KEY ( CONTROL-M )
                      LET    li_x1          = ARR_CURR()
                 EXIT DISPLAY
            END DISPLAY
            SELECT  paren_cod
              INTO  ls_paren_cod
              FROM  tab_parentesco
             WHERE  paren_desc = la_benef[li_x1].paren_desc
            LET     gr_ret_solicitante.paren_cod       = ls_paren_cod
       END IF
  ELSE
       LET  gr_ret_solicitante.apellido_paterno  = gr_ret_sol_issste_tx.paterno_afore
       LET  gr_ret_solicitante.apellido_materno  = gr_ret_sol_issste_tx.materno_afore
       LET  gr_ret_solicitante.nombres           = gr_ret_sol_issste_tx.nombre_afore
       LET  gr_ret_solicitante.paren_cod         = 12
  END IF

  #CPL-1844
  #Agregar captura de constancia
  LET  gs_Cance    = 0
  DISPLAY "[ESC] Continuar  [Ctrl-C] Cancelar                                               " AT 01,1 ATTRIBUTE(REVERSE)
  WHILE TRUE
     LET  num_cuenta     = gc_num_cuenta
     LET  folio          = gr_ret_sol_issste_tx.folio_solicitud
     DISPLAY     folio               TO     folio
     DISPLAY     fecha_solicitud     TO     fecha_solicitud
     ### DISPLAY     num_cuenta          TO     num_cuenta
     INPUT BY NAME    folio                   ,
                      fecha_solicitud         ,
                      gr_constancia.constancia
                      WITHOUT DEFAULTS

        BEFORE FIELD fecha_solicitud
           IF   gr_ret_sol_issste_tx.fecha_solicitud    IS NULL
            OR  gr_ret_sol_issste_tx.fecha_solicitud    =  " "  THEN
                LET  fecha_solicitud = TODAY
           ELSE
                LET  fecha_solicitud = gr_ret_sol_issste_tx.fecha_solicitud
           END IF
        DISPLAY fecha_solicitud     TO fecha_solicitud

        BEFORE FIELD folio
           LET    folio                =  gr_ret_sol_issste_tx.folio_solicitud
           LET    ls_recaptura_folio   =  1     --- para k recapture
           DISPLAY folio               TO folio

        #CPL-1844
        AFTER FIELD constancia
        	 IF gr_constancia.constancia IS NULL THEN
              CALL   f_DespMen("Debe indicar si se solicitó constancia")
              NEXT   FIELD constancia
           ELSE
           	  IF gr_constancia.constancia <> 'S' AND
           	  	 gr_constancia.constancia <> 's' AND
           	  	 gr_constancia.constancia <> 'N' AND
           	  	 gr_constancia.constancia <> 'n' THEN

           	  	 CALL   f_DespMen("Solo indique S/N")
                 NEXT   FIELD constancia
           	  END IF
        	 END IF

        ON KEY (ESC)
           IF   fecha_solicitud  >  TODAY  THEN
                CALL   f_DespMen("FECHA DE SOLICITUD No puede ser mayor que fecha del dia ")
                NEXT   FIELD fecha_solicitud
           ELSE
               LET   gr_ret_sol_issste_tx.fecha_solicitud = fecha_solicitud
           END IF

           #CPL-1844
           IF gr_constancia.constancia IS NULL THEN
              CALL   f_DespMen("Debe indicar si se solicitó constancia")
              NEXT   FIELD constancia
           ELSE
           	  IF gr_constancia.constancia <> 'S' AND
           	  	 gr_constancia.constancia <> 's' AND
           	  	 gr_constancia.constancia <> 'N' AND
           	  	 gr_constancia.constancia <> 'n' THEN

           	  	 CALL   f_DespMen("Solo indique S/N")
                 NEXT   FIELD constancia
           	  END IF
        	 END IF

           IF   gs_afores     = 578  THEN
               IF   folio   IS NULL  THEN
                    CALL   f_DespMen("FOLIO No puede ser blanco  ")
                    NEXT FIELD folio
               ELSE
                    LET   ls_folio          =  0
                    CALL  f_VerCAV(folio)   RETURNING  ls_folio
                    IF    ls_folio   = 0   THEN
                    ELSE
                          CALL   f_DespMen("FOLIO DE SOLICITUD no existe en CAV")
                          NEXT FIELD folio
                    END IF
                    LET   gr_ret_sol_issste_tx.folio_solicitud = folio
               END IF
           END IF
           LET   gr_ret_sol_issste_tx.folio_solicitud = folio
           LET   gs_Cance = 0
           EXIT INPUT

        ON KEY (CONTROL-C,  INTERRUPT)
           LET   gs_Cance = 1
           EXIT  INPUT

        AFTER  FIELD  fecha_solicitud
           IF   fecha_solicitud  >  TODAY  THEN
                CALL   f_DespMen("FECHA DE SOLICITUD No puede mayor que fecha del dia ")
                NEXT FIELD fecha_solicitud
           END IF
           LET   gr_ret_sol_issste_tx.fecha_solicitud = fecha_solicitud

        AFTER FIELD  folio
           ### Validar y Recaptura
           IF   gs_afores     = 578  THEN
               ###  Validar folio contra tabla de rec_solicitud
               IF   folio   IS NULL  THEN
                    CALL   f_DespMen("FOLIO No puede ser blanco  ")
                    NEXT FIELD folio
               ELSE
                    LET   ls_folio          =  0
                    CALL  f_VerCAV(folio)   RETURNING  ls_folio
                    IF    ls_folio   = 0   THEN
                    ELSE
                          CALL   f_DespMen("FOLIO DE SOLICITUD no existe en CAV")
                          NEXT FIELD folio
                    END IF
               END IF
               LET   gr_ret_sol_issste_tx.folio_solicitud = folio

               ###  Validamos si ha cambiado el folio gi_folio es el que buscamos en rec_solicitud
               IF      gi_folio                  <>    gr_ret_sol_issste_tx.folio_solicitud
                 OR    ls_recaptura_folio        =  1       THEN
                     LET    ls_recaptura_folio   =  1
               END IF

               IF   ls_recaptura_folio  =   1    THEN
                    LET  ls_folio     =  0
                    CALL f_Recaptura_Folio(gr_ret_sol_issste_tx.tipo_retiro, gr_ret_sol_issste_tx.folio_solicitud)
                                 RETURNING ls_folio

                   IF   ls_folio <> 0 THEN
                        LET             ls_recaptura_folio = 1
                        LET             gr_ret_sol_issste_tx.folio_solicitud  = 0
                        LET             folio   = 0
                        DISPLAY gr_ret_sol_issste_tx.folio_solicitud TO folio
                        NEXT FIELD      folio
                   ELSE
                        LET             ls_recaptura_folio = 0
                        LET             gi_folio    = gr_ret_sol_issste_tx.folio_solicitud
                   END IF
                   LET   gr_ret_sol_issste_tx.folio_solicitud = folio
               ELSE
                   LET   gr_ret_sol_issste_tx.folio_solicitud = folio
               END IF
           END IF
           LET   gs_Cance = 0
           LET   gr_ret_sol_issste_tx.folio_solicitud = folio

     END INPUT
     IF   fecha_solicitud IS NULL    THEN
          IF  gs_Cance = 1   THEN
               EXIT WHILE
          END IF
     ELSE
         EXIT WHILE
     END IF

  END WHILE

  RETURN gr_ret_sol_issste_tx.*,
         gr_ret_solicitante.*
END FUNCTION


### Captura información especifica de tramite C
FUNCTION  f_InpdelC(lc_tipo_tramite)
  DEFINE     lc_tipo_tramite   CHAR(03)
  DEFINE     li_x1             SMALLINT
  DEFINE     li_x2             SMALLINT
  DEFINE     li_x3             SMALLINT
  DEFINE     li_x4             SMALLINT
  DEFINE     li_x5             SMALLINT
  DEFINE     li_x6             SMALLINT
  DEFINE     la_regimen        ARRAY[50]    OF  RECORD
             regimen           CHAR(03),
             regimen_des       CHAR(50)
             END RECORD
  DEFINE     la_seguro         ARRAY[50]    OF  RECORD
             tipo_seguro       CHAR(02),
             descricpion       CHAR(50)
             END RECORD
  DEFINE     la_pension        ARRAY[50]    OF  RECORD
             tipo_pension      CHAR(02),
             descricpion       CHAR(50)
             END RECORD
  DEFINE     la_clave          ARRAY[50]    OF  RECORD
             cve_pension       CHAR(02),
             desc_corta        CHAR(100)
             END RECORD
  DEFINE     la_prestacion     ARRAY[50]    OF  RECORD
             tipo_prestacion   SMALLINT,
             descripcion       CHAR(50)
             END RECORD
  DEFINE     ls_entro          SMALLINT
  DEFINE     lc_grupo          LIKE  ret_sol_issste_tx.grupo
  DEFINE     ls_VerSaldo       SMALLINT
  DEFINE     lc_Mensaje        CHAR(100)

  DEFINE     la_spaces ARRAY[2] OF RECORD
  	         regimen           CHAR(03),
             regimen_des       CHAR(50)
  END RECORD

  LET        la_spaces[2].regimen      = ""
  LET        la_spaces[2].regimen_des  = ""

  DISPLAY "[Flechas] Seleccionar [ENTER] Aceptar [Ctrl-C] Cancelar                            " AT 01,1 ATTRIBUTE(REVERSE)
  LET        ls_entro          =          0
  LET        li_x1             =          1
  FOREACH    d_SelReg          USING      lc_tipo_tramite
                               INTO       la_regimen[li_x1].*
      LET    li_x1             =          li_x1   +  1
  END FOREACH
  CALL SET_COUNT(li_x1 - 1)

  IF      li_x1   =  2    THEN
     DISPLAY  la_regimen[1].*  TO   scr_reg.*
     LET    li_x1          = 1
     LET    gr_ret_sol_issste_tx.regimen           = la_regimen[1].regimen
  ELSE
  	 ERROR "SELECCIONE EL REGIMEN"
     DISPLAY ARRAY la_regimen  TO   scr_reg.*
          ON KEY ( CONTROL-C,  INTERRUPT)
               LET    li_x1          = ARR_CURR()
               LET    ls_entro       = 1
               EXIT DISPLAY

          ON KEY ( CONTROL-M )
               LET    li_x1          = ARR_CURR()
               LET    gr_ret_sol_issste_tx.regimen           = la_regimen[li_x1].regimen

               LET la_spaces[1].regimen      = la_regimen[li_x1].regimen
               LET la_spaces[1].regimen_des  = la_regimen[li_x1].regimen_des

               DISPLAY  la_spaces[1].* TO scr_reg[1].*
               DISPLAY  la_spaces[2].* TO scr_reg[2].*
               EXIT DISPLAY
     END DISPLAY
     ERROR ""
  END IF

  IF         ls_entro          =          0   THEN
       LET        li_x2             =          1
       FOREACH    d_SelSeg          USING      lc_tipo_tramite,
                                               la_regimen[li_x1].regimen
                                    INTO       la_seguro[li_x2].*
           LET    li_x2             =          li_x2   +  1
       END FOREACH
       CALL SET_COUNT(li_x2 - 1)
       IF   li_x2   = 2 THEN
           DISPLAY  la_seguro[1].*  TO   scr_seg.*
           LET    li_x2          = 1
           LET    gr_ret_sol_issste_tx.tipo_seguro       = la_seguro[1].tipo_seguro
       ELSE
           DISPLAY ARRAY la_seguro  TO   scr_seg.*
                ON KEY ( CONTROL-C,  INTERRUPT)
                     LET li_x2          = ARR_CURR()
                     LET ls_entro       = 1
                     EXIT DISPLAY

                ON KEY ( CONTROL-M )
                     LET    li_x2          = ARR_CURR()
                     LET    gr_ret_sol_issste_tx.tipo_seguro       = la_seguro[li_x2].tipo_seguro
                     EXIT DISPLAY
           END DISPLAY
       END IF
  END IF

  IF         ls_entro          =          0   THEN
       LET        li_x3             =          1
       FOREACH    d_SelPen          USING      lc_tipo_tramite,
                                               la_regimen[li_x1].regimen,
                                               la_seguro[li_x2].tipo_seguro
                                    INTO       la_pension[li_x3].*
           LET    li_x3             =          li_x3   +  1
       END FOREACH
       CALL SET_COUNT(li_x3 - 1)
       IF   li_x3   =  2  THEN
           DISPLAY la_pension[1].*   TO   scr_pen.*
           LET    li_x3          = 1
           LET    gr_ret_sol_issste_tx.tipo_pension      = la_pension[1].tipo_pension
       ELSE
           DISPLAY ARRAY la_pension  TO   scr_pen.*
                ON KEY ( CONTROL-C,  INTERRUPT)
                     LET li_x3          = ARR_CURR()
                     LET ls_entro       = 1
                     EXIT DISPLAY

                ON KEY ( CONTROL-M )
                     LET    li_x3          = ARR_CURR()
                     LET    gr_ret_sol_issste_tx.tipo_pension      = la_pension[li_x3].tipo_pension
                     EXIT DISPLAY
           END DISPLAY
       END IF
  END IF

  IF         ls_entro          =          0    THEN
       LET        li_x4             =          1
       FOREACH    d_SelCve          USING      lc_tipo_tramite,
                                               la_regimen[li_x1].regimen,
                                               la_seguro[li_x2].tipo_seguro,
                                               la_pension[li_x3].tipo_pension
                                    INTO       la_clave[li_x4].*
           LET    li_x4             =          li_x4   +  1
       END FOREACH
       CALL SET_COUNT(li_x4 - 1)
       IF   li_x4   =  2   THEN
            DISPLAY la_clave[1].*   TO   scr_cve.*
            LET    li_x4          = 1
            LET    gr_ret_sol_issste_tx.cve_pension       = la_clave[1].cve_pension
       ELSE
            DISPLAY ARRAY la_clave  TO   scr_cve.*
                 ON KEY ( CONTROL-C,  INTERRUPT)
                      LET li_x4          = ARR_CURR()
                      LET ls_entro       = 1
                      EXIT DISPLAY

                 ON KEY ( CONTROL-M )
                      LET    li_x4          = ARR_CURR()
                      LET    gr_ret_sol_issste_tx.cve_pension       = la_clave[li_x4].cve_pension
                      EXIT DISPLAY
            END DISPLAY
       END IF
  END IF

  If         ls_entro          =          0   THEN
       LET        li_x5             =          1
       FOREACH    d_SelPre          USING      lc_tipo_tramite,
                                               la_regimen[li_x1].regimen,
                                               la_seguro[li_x2].tipo_seguro,
                                               la_pension[li_x3].tipo_pension,
                                               la_clave[li_x4].cve_pension
                                    INTO       la_prestacion[li_x5].*
           LET    li_x5             =          li_x5   +  1
       END FOREACH
       CALL SET_COUNT(li_x5 - 1)
       IF   li_x5   =  2   THEN
           DISPLAY  la_prestacion[1].*  TO   scr_pre.*
           LET    li_x5          = 1
           LET    gr_ret_sol_issste_tx.tipo_prestacion   = la_prestacion[1].tipo_prestacion
       ELSE
           DISPLAY ARRAY la_prestacion  TO   scr_pre.*
                ON KEY ( CONTROL-C,  INTERRUPT)
                     LET li_x5          = ARR_CURR()
                     LET ls_entro       = 1
                     EXIT DISPLAY

                ON KEY ( CONTROL-M )
                     LET    li_x5          = ARR_CURR()
                     LET    gr_ret_sol_issste_tx.tipo_prestacion   = la_prestacion[li_x5].tipo_prestacion
                     EXIT DISPLAY
           END DISPLAY
       END IF

       FOREACH    d_SelGpo          USING      lc_tipo_tramite,
                                               la_regimen[li_x1].regimen,
                                               la_seguro[li_x2].tipo_seguro,
                                               la_pension[li_x3].tipo_pension,
                                               la_clave[li_x4].cve_pension,
                                               la_prestacion[li_x5].tipo_prestacion
                                    INTO       lc_grupo
           LET    gr_ret_sol_issste_tx.grupo   =   lc_grupo
       END FOREACH
       IF   gr_ret_sol_issste_tx.tipo_retiro     = 'C'
        OR  gr_ret_sol_issste_tx.tipo_retiro     = 'D'
        OR  gr_ret_sol_issste_tx.tipo_retiro     = 'E' THEN
        ### El tipo CDE no se revisan en datamart
            LET   ls_VerSaldo             =   0
            CALL  f_VerSaldo(gr_ret_sol_issste_tx.nss, gr_ret_sol_issste_tx.grupo)         RETURNING ls_VerSaldo, lc_Mensaje
            IF    ls_VerSaldo             =   1  THEN
                  CALL  f_DespMen(lc_Mensaje)
                  LET   ls_entro          =   1
            ELSE
                  LET   ls_entro          =   0
            END IF
      END IF
  END IF

  IF ls_entro = 0  THEN
          LET  gs_Cance         =  0
  ELSE
          LET  gs_Cance         =  1
  END IF

END FUNCTION


### Busca documento probatorio
FUNCTION f_BuscaDPro(ls_cve_doc)
  DEFINE    ls_cve_doc                 SMALLINT
  DEFINE    ls_Resul                   SMALLINT
  DEFINE    ls_Cuantos                 SMALLINT
  DEFINE    ls_entro                   SMALLINT
  DEFINE    li_x1                      SMALLINT
  DEFINE    lc_desc                    CHAR(100)

  DEFINE     la_spaces                 ARRAY[1]    OF  RECORD
             cve_docto                 INTEGER,
             docto_desc                CHAR(50)
             END RECORD
  DEFINE     la_documento              ARRAY[200]    OF  RECORD
             cve_docto                 INTEGER,
             docto_desc                CHAR(50)
             END RECORD

  LET        la_spaces[1].cve_docto   = ""
  LET        la_spaces[1].docto_desc  = ""
  DISPLAY "[Flechas] Seleccionar [ENTER] Aceptar [Ctrl-C] Cancelar                            " AT 01,1 ATTRIBUTE(REVERSE)
  LET lc_desc = " "
  LET    gc_query    = 'SELECT *             ',
                       '  FROM tab_doc_prob  '

  PREPARE    p_SelDocs         FROM  gc_query
  DECLARE    d_SelDocs       CURSOR  FOR p_SelDocs

  LET        ls_entro          =          0
  LET        li_x1             =          1
  FOREACH    d_SelDocs         INTO       la_documento[li_x1].*
      LET    li_x1             =          li_x1   +  1
  END FOREACH
  CALL SET_COUNT(li_x1 - 1)


  IF      li_x1   =  2    THEN
     DISPLAY  la_documento[1].*  TO   scr_doc.*
     LET    li_x1          = 1
     LET    gr_ret_sol_issste_tx.cve_doc_probatorio           = la_documento[1].cve_docto
  ELSE
     DISPLAY ARRAY la_documento      TO   scr_doc.*
          ON KEY ( CONTROL-C,  INTERRUPT)
               LET    li_x1          =    ARR_CURR()
               LET    ls_entro       = 1
               EXIT DISPLAY

          ON KEY ( CONTROL-M )
               LET    li_x1          =    ARR_CURR()
               LET    gr_ret_sol_issste_tx.cve_doc_probatorio           = la_documento[li_x1].cve_docto
               EXIT DISPLAY
     END DISPLAY
  END IF

  DISPLAY  la_documento[li_x1].*        TO  scr_doc[1].*
  DISPLAY  la_spaces[1].*               TO  scr_doc[2].*
  DISPLAY  la_spaces[1].*               TO  scr_doc[3].*
  LET    ls_Resul                       =   la_documento[li_x1].cve_docto
  LET    lc_desc                        =   la_documento[li_x1].docto_desc

  DISPLAY "[Esc] Continuar  [Ctrl-C] Cancelar                                                " AT 01,1 ATTRIBUTE(REVERSE)
  RETURN ls_Resul
END FUNCTION


### Crear proceso_pago
FUNCTION f_PeridoPago(ld_fecha)
  DEFINE     ld_fecha         DATE
  DEFINE     mes              INTEGER
  DEFINE     ano              INTEGER
  DEFINE     residuo          INTEGER
  DEFINE     periodo          CHAR(06)

  LET  mes             = MONTH(ld_fecha)
  LET  ano             = YEAR (ld_fecha)
  LET  residuo         = mes MOD 2
  IF   residuo         > 0 THEN
       LET mes         = mes + 1
  END IF
  LET periodo          = ano USING "&&&&",mes USING "&&"
  RETURN periodo
END FUNCTION



### Input de semanas cotizadas
FUNCTION f_InpSemCot(semanas_cotizadas, lc_Telef, lc_estad_cod, lc_deleg_cod)
  DEFINE   semanas_cotizadas      LIKE ret_sol_issste_tx.semanas_cotizadas
  DEFINE   lc_Telef               LIKE ret_solicitante.telefono
  DEFINE   lc_estad_cod           LIKE ret_sol_issste_tx.estad_cod
  DEFINE   lc_estad_desc          CHAR(100)
  DEFINE   lc_deleg_cod           LIKE ret_sol_issste_tx.deleg_cod
  DEFINE   lc_deleg_desc          CHAR(100)


  DISPLAY "[Esc] Continuar  [Ctrl-C] Cancelar                                                " AT 01,1 ATTRIBUTE(REVERSE)
  LET   gs_Cance = 0
  WHILE  TRUE
     DISPLAY      lc_Telef              TO   lc_Telef
     DISPLAY      semanas_cotizadas     TO   semanas_cotizadas
     INPUT BY NAME lc_Telef,
                   semanas_cotizadas
                   WITHOUT DEFAULTS

        BEFORE FIELD  lc_Telef
           IF   lc_Telef                           IS NULL
            OR  lc_Telef                           = " "   THEN
             LET  lc_Telef        = 0
           END IF
           DISPLAY      lc_Telef     TO   lc_Telef

        BEFORE FIELD semanas_cotizadas
             LET   semanas_cotizadas = semanas_cotizadas
             DISPLAY      semanas_cotizadas     TO   semanas_cotizadas

        ON KEY (CONTROL-C,  INTERRUPT)
             LET   gs_Cance = 1
             EXIT  INPUT

        ON KEY (ESC)
             IF   lc_Telef        IS NULL
              OR  lc_Telef        = " "   THEN
                  CALL   f_DespMen("TELEFONO No puede ser un blanco ")
                  NEXT FIELD lc_Telef
             END IF
             IF  semanas_cotizadas         IS NULL  THEN
                 CALL   f_DespMen("SEMANAS COTIZADAS No puede quedar en blanco ")
                 NEXT FIELD semanas_cotizadas
             END IF
             EXIT INPUT

        AFTER FIELD semanas_cotizadas
             IF  semanas_cotizadas         IS NULL  THEN
                 CALL   f_DespMen("SEMANAS COTIZADAS No puede quedar en blanco ")
                 NEXT FIELD semanas_cotizadas
             END IF

        AFTER FIELD lc_Telef
             IF   lc_Telef        IS NULL
              OR  lc_Telef        = " "   THEN
                  CALL   f_DespMen("TELEFONO No puede ser un blanco ")
                  NEXT FIELD lc_Telef
             END IF

             LET   gs_Cance    = 0

     END INPUT
     IF  lc_Telef             IS NULL
      OR lc_Telef             =  " "
      OR semanas_cotizadas    IS NULL
      OR semanas_cotizadas    =  " "   THEN
         IF    gs_Cance = 1   THEN
               EXIT WHILE
         END IF
     ELSE
         EXIT WHILE
     END IF
  END WHILE

  RETURN semanas_cotizadas, lc_Telef, lc_estad_cod, lc_deleg_cod
END FUNCTION


### Input de semanas cotizadas
FUNCTION f_InpSemCot2(semanas_cotizadas, lc_Telef, lc_estad_cod, lc_deleg_cod,
                       regimen, tipo_seguro, tipo_pension, cve_pension, tipo_prestacion)
  DEFINE   semanas_cotizadas      LIKE ret_sol_issste_tx.semanas_cotizadas
  DEFINE   lc_Telef               LIKE ret_solicitante.telefono
  DEFINE   lc_estad_cod           LIKE ret_sol_issste_tx.estad_cod
  DEFINE   lc_estad_desc          CHAR(100)
  DEFINE   lc_deleg_cod           LIKE ret_sol_issste_tx.deleg_cod
  DEFINE   lc_deleg_desc          CHAR(100)
  DEFINE     regimen              LIKE ret_sol_issste_tx.regimen
  DEFINE     tipo_seguro          LIKE ret_sol_issste_tx.tipo_seguro
  DEFINE     tipo_pension         LIKE ret_sol_issste_tx.tipo_pension
  DEFINE     cve_pension          LIKE ret_sol_issste_tx.cve_pension
  DEFINE     tipo_prestacion      LIKE ret_sol_issste_tx.tipo_prestacion
  DEFINE     lc_regimen_desc      CHAR(100)
  DEFINE     lc_seguro_desc       CHAR(100)
  DEFINE     lc_tipopension_desc  CHAR(100)
  DEFINE     lc_cvepension_desc   CHAR(100)
  DEFINE     lc_prestacion_desc   CHAR(100)

  DISPLAY "[Esc] Continuar [Ctrl-C] Cancelar                                                " AT 01,1 ATTRIBUTE(REVERSE)
  LET   gs_Cance = 0
  WHILE  TRUE



     LET     lc_regimen_desc     = f_VerRegimen(    regimen)
     LET     lc_seguro_desc      = f_VerSeguro(     tipo_seguro)
     LET     lc_tipopension_desc = f_VertipoPension(tipo_pension)
     LET     lc_cvepension_desc  = f_VerCvePension( cve_pension)
     LET     lc_prestacion_desc  = f_VerPrestacion( tipo_prestacion)

     DISPLAY regimen                                    TO  regimen
     DISPLAY lc_regimen_desc                            TO  regimen_desc
     DISPLAY tipo_seguro                                TO  tipo_seguro
     DISPLAY lc_seguro_desc                             TO  seguro_desc
     DISPLAY tipo_pension                               TO  tipo_pension
     DISPLAY lc_tipopension_desc                        TO  tipo_pension_desc
     DISPLAY cve_pension                                TO  cve_pension
     DISPLAY lc_cvepension_desc                         TO  cve_pension_desc
     DISPLAY tipo_prestacion                            TO  tipo_prestacion
     DISPLAY lc_prestacion_desc                         TO  tipo_prestacion_desc
     LET        lc_Telef       =   lc_Telef
     DISPLAY    lc_Telef      TO   lc_Telef





     INPUT BY NAME lc_Telef
                   WITHOUT DEFAULTS

        BEFORE FIELD  lc_Telef
           IF   lc_Telef                           IS NULL
            OR  lc_Telef                           = " "   THEN
             LET  lc_Telef        = 0
           ELSE
             LET  lc_Telef        = lc_Telef
           END IF

        ON KEY (CONTROL-C,  INTERRUPT)
             LET   gs_Cance = 1
             EXIT  INPUT

        ON KEY (ESC)
             IF   lc_Telef        IS NULL
              OR  lc_Telef        = " "   THEN
                  CALL   f_DespMen("TELEFONO No puede ser un blanco ")
                  NEXT FIELD lc_Telef
             END IF
             EXIT INPUT

        AFTER FIELD lc_Telef
             IF   lc_Telef        IS NULL
              OR  lc_Telef        = " "   THEN
                  CALL   f_DespMen("TELEFONO No puede ser un blanco ")
                  NEXT FIELD lc_Telef
             END IF
             LET   gs_Cance    = 0

     END INPUT
     IF  lc_Telef             IS NULL
      OR lc_Telef             =  " "  THEN
         IF    gs_Cance = 1   THEN
               EXIT WHILE
         END IF
     ELSE
         EXIT WHILE
     END IF
  END WHILE

  RETURN semanas_cotizadas, lc_Telef, lc_estad_cod, lc_deleg_cod
END FUNCTION


###   Input del tipo C
FUNCTION  f_MasdelC(gr_ret_sol_issste_tx, lc_Telef)
  DEFINE     gr_ret_sol_issste_tx   RECORD LIKE ret_sol_issste_tx.*
  DEFINE     aseguradora            LIKE   ret_sol_issste_tx.aseguradora
  DEFINE     actuario               LIKE   ret_sol_issste_tx.actuario
  DEFINE     num_plan_privado       LIKE   ret_sol_issste_tx.num_plan_privado
  DEFINE     semanas_cotizadas      LIKE   ret_sol_issste_tx.semanas_cotizadas
  DEFINE     lc_Telef               LIKE   ret_solicitante.telefono
  DEFINE     lc_estad_cod           LIKE   ret_sol_issste_tx.estad_cod
  DEFINE     lc_deleg_cod           LIKE   ret_sol_issste_tx.deleg_cod
  DEFINE     lc_estad_desc          CHAR(100)
  DEFINE     lc_deleg_desc          CHAR(100)

  LET   gs_Cance   = 0
  DISPLAY "[Esc] Continuar [Ctrl-C] Cancelar                                                " AT 01,1 ATTRIBUTE(REVERSE)
  WHILE TRUE
     LET   aseguradora            =    gr_ret_sol_issste_tx.aseguradora
     LET   actuario               =    gr_ret_sol_issste_tx.actuario


     LET    num_plan_privado       =    gr_ret_sol_issste_tx.num_plan_privado
     LET    semanas_cotizadas      =    gr_ret_sol_issste_tx.semanas_cotizadas
     DISPLAY    lc_Telef           TO   lc_Telef


     DISPLAY    aseguradora        TO   aseguradora
     DISPLAY    actuario           TO   actuario
     DISPLAY    num_plan_privado   TO   num_plan_privado
     DISPLAY    semanas_cotizadas  TO   semanas_cotizadas
     INPUT BY NAME lc_Telef,
                   aseguradora,
                   actuario,
                   num_plan_privado,
                   semanas_cotizadas
                   WITHOUT DEFAULTS

        BEFORE FIELD  lc_Telef
           IF     lc_Telef          IS NULL
            OR    lc_Telef          =  " "   THEN
             LET  lc_Telef          =  0
           END IF
           DISPLAY  lc_Telef            TO   lc_Telef

        BEFORE FIELD  aseguradora
           LET   aseguradora            =    gr_ret_sol_issste_tx.aseguradora
           DISPLAY     aseguradora      TO   aseguradora

        BEFORE FIELD  actuario
           LET   actuario               =    gr_ret_sol_issste_tx.actuario
           DISPLAY    actuario          TO   actuario

        BEFORE FIELD  num_plan_privado
           LET   num_plan_privado       =    gr_ret_sol_issste_tx.num_plan_privado
           DISPLAY    num_plan_privado  TO  num_plan_privado

        BEFORE FIELD  semanas_cotizadas
           LET   semanas_cotizadas      =    gr_ret_sol_issste_tx.semanas_cotizadas
           DISPLAY    semanas_cotizadas TO  semanas_cotizadas

        ON KEY (CONTROL-C,  INTERRUPT)
           LET   gs_Cance = 1
           EXIT  INPUT

        ON KEY(ESC)
           IF   lc_Telef        IS  NULL
            OR  lc_Telef        =   " "   THEN
                CALL   f_DespMen("TELEFONO No puede ser un blanco ")
                NEXT FIELD lc_Telef
           ELSE
                LET  lc_Telef                           = lc_Telef
           END IF
           LET   gr_ret_sol_issste_tx.aseguradora       = aseguradora
           LET   gr_ret_sol_issste_tx.actuario          = actuario
           LET   gr_ret_sol_issste_tx.num_plan_privado  = num_plan_privado
           IF    semanas_cotizadas    IS NULL  THEN
                 CALL   f_DespMen("SEMANAS COTIZADAS No puede quedar en blanco ")
                 NEXT FIELD semanas_cotizadas
           ELSE
                 LET   gr_ret_sol_issste_tx.semanas_cotizadas  = semanas_cotizadas
           END IF

           LET   gs_Cance = 0
           EXIT INPUT

        AFTER FIELD aseguradora
           LET   gr_ret_sol_issste_tx.aseguradora       = aseguradora

        AFTER FIELD actuario
           LET   gr_ret_sol_issste_tx.actuario          = actuario

        AFTER FIELD num_plan_privado
           LET   gr_ret_sol_issste_tx.num_plan_privado  = num_plan_privado

        AFTER FIELD  lc_Telef
           IF   lc_Telef        IS NULL
            OR  lc_Telef        = " "   THEN
                CALL   f_DespMen("TELEFONO No puede ser un blanco ")
                NEXT FIELD lc_Telef
           END IF

        AFTER FIELD semanas_cotizadas
           IF    semanas_cotizadas         IS NULL  THEN
                 CALL   f_DespMen("SEMANAS COTIZADAS No puede quedar en blanco ")
                 NEXT FIELD semanas_cotizadas
           ELSE
                 LET   gr_ret_sol_issste_tx.semanas_cotizadas  = semanas_cotizadas
           END IF
           LET   gs_Cance    = 0

     END INPUT
     IF   lc_Telef               IS NULL
      OR  semanas_cotizadas      IS NULL  THEN
          IF   gs_Cance = 1      THEN
               EXIT  WHILE
          END IF
     ELSE
          EXIT WHILE
     END IF
  END WHILE

  RETURN gr_ret_sol_issste_tx.*, lc_Telef
END FUNCTION


### Mas Input del E
FUNCTION  f_InpMasE(gr_ret_sol_issste_tx, lc_Telef)
  DEFINE     gr_ret_sol_issste_tx   RECORD LIKE ret_sol_issste_tx.*
  DEFINE     cve_docto                          SMALLINT
  DEFINE     semanas_cotizadas                  LIKE  ret_sol_issste_tx.semanas_cotizadas
  DEFINE     ls_DocPro                          SMALLINT
  DEFINE     lc_Telef                           LIKE  ret_solicitante.telefono
  DEFINE     lc_estad_cod           LIKE   ret_sol_issste_tx.estad_cod
  DEFINE     lc_deleg_cod           LIKE   ret_sol_issste_tx.deleg_cod
  DEFINE     lc_estad_desc          CHAR(100)
  DEFINE     lc_deleg_desc          CHAR(100)
  DEFINE     lc_Desc                CHAR(100)

  DISPLAY "[Esc] Continuar [Ctrl-C] Cancelar                                                " AT 01,1 ATTRIBUTE(REVERSE)
  WHILE TRUE




      LET    cve_docto                    =   gr_ret_sol_issste_tx.cve_doc_probatorio
      LET    semanas_cotizadas            =   gr_ret_sol_issste_tx.semanas_cotizadas
      DISPLAY       lc_Telef             TO   lc_Telef


      DISPLAY       cve_docto            TO   cve_docto
      DISPLAY       semanas_cotizadas    TO   semanas_cotizadas
      INPUT BY NAME lc_Telef,
                    cve_docto,
                    semanas_cotizadas
                    WITHOUT DEFAULTS

        BEFORE FIELD  lc_Telef
           IF     lc_Telef          IS NULL
            OR    lc_Telef          =  " "   THEN
             LET  lc_Telef          =  0
           END IF
           LET  lc_Telef          =  lc_Telef
           DISPLAY       lc_Telef             TO   lc_Telef

         BEFORE FIELD cve_docto
              LET  cve_docto             =  gr_ret_sol_issste_tx.cve_doc_probatorio
              DISPLAY   gr_ret_sol_issste_tx.cve_doc_probatorio   TO  scr_doc[1].cve_docto
              DISPLAY   lc_Desc                                   TO  scr_doc[1].docto_desc

         BEFORE FIELD semanas_cotizadas
              LET  semanas_cotizadas     =  gr_ret_sol_issste_tx.semanas_cotizadas
              DISPLAY       semanas_cotizadas    TO   semanas_cotizadas

         AFTER FIELD  lc_Telef
              IF   lc_Telef        IS NULL
               OR  lc_Telef        = " "   THEN
                   CALL   f_DespMen("TELEFONO No puede ser un blanco ")
                   NEXT FIELD lc_Telef
              END IF

         AFTER FIELD cve_docto
              LET ls_DocPro     = 0
              LET ls_DocPro     = f_BuscaDPro(cve_docto)
              IF  ls_DocPro     = 0  THEN
                  CALL   f_DespMen("CLAVE DOCUMENTO PROBATORIO Es incorrecto ")
                  NEXT FIELD cve_docto
              ELSE
              	  LET cve_docto = ls_DocPro
                  LET   gr_ret_sol_issste_tx.cve_doc_probatorio  = cve_docto
              END IF

         AFTER FIELD semanas_cotizadas
              IF  semanas_cotizadas         IS NULL  THEN
                  CALL   f_DespMen("SEMANAS COTIZADAS No puede quedar en blanco ")
                  NEXT FIELD semanas_cotizadas
              END IF
              LET   gr_ret_sol_issste_tx.semanas_cotizadas  = semanas_cotizadas
              LET   gs_Cance    = 0

         ON KEY (CONTROL-C,  INTERRUPT)
              LET   gs_Cance = 1
              EXIT  INPUT

         ON KEY (ESC)
              IF   lc_Telef        IS NULL
               OR  lc_Telef        = " "   THEN
                   CALL   f_DespMen("TELEFONO No puede ser un blanco ")
                   NEXT FIELD lc_Telef
              END IF

              ####LET ls_DocPro     = 0
              ####LET ls_DocPro     = f_BuscaDPro(cve_doc_probatorio)
              ####IF  ls_DocPro     = 0  THEN
              ####    CALL   f_DespMen("CLAVE DOCUMENTO PROBATORIO Es incorrecto ")
              ####    NEXT FIELD cve_doc_probatorio
              ####ELSE
              ####    LET   gr_ret_sol_issste_tx.cve_doc_probatorio  = cve_doc_probatorio
              ####END IF

              IF  semanas_cotizadas    IS NULL  THEN
                  CALL   f_DespMen("SEMANAS COTIZADAS No puede quedar en blanco ")
                  NEXT FIELD semanas_cotizadas
              ELSE
                  LET   gr_ret_sol_issste_tx.semanas_cotizadas  = semanas_cotizadas
              END IF
              LET   gs_Cance = 0
              EXIT INPUT



      END INPUT
      IF   lc_Telef              IS NULL
       OR  cve_docto             IS NULL THEN
       ###OR  semanas_cotizadas     IS NULL  THEN
          IF    gs_Cance = 1   THEN
                EXIT WHILE
          END IF
      ELSE
          EXIT WHILE
      END IF

  END WHILE
  RETURN gr_ret_sol_issste_tx.*, lc_Telef
END FUNCTION


### Input de los K
FUNCTION f_InpMasK(gr_ret_sol_issste_tx, lc_Telef)
  DEFINE     gr_ret_sol_issste_tx      RECORD LIKE ret_sol_issste_tx.*
  DEFINE     sec_pension        LIKE   ret_sol_issste_tx.sec_pension
  DEFINE     fecha_ini_pen      LIKE   ret_sol_issste_tx.fecha_ini_pen
  DEFINE     fecha_resolucion   LIKE   ret_sol_issste_tx.fecha_resolucion
  DEFINE     periodo_pago       LIKE   ret_sol_issste_tx.periodo_pago
  DEFINE     lc_periodo_pago    LIKE   ret_sol_issste_tx.periodo_pago
  DEFINE     lc_Telef           LIKE   ret_solicitante.telefono
  DEFINE     lc_estad_cod           LIKE   ret_sol_issste_tx.estad_cod
  DEFINE     lc_deleg_cod           LIKE   ret_sol_issste_tx.deleg_cod
  DEFINE     lc_estad_desc          CHAR(100)
  DEFINE     lc_deleg_desc          CHAR(100)

  DISPLAY "[Esc] Continuar [Ctrl-C] Cancelar                                                " AT 01,1 ATTRIBUTE(REVERSE)
  WHILE TRUE




      LET    sec_pension                   =     gr_DatMar.sec_pension
      LET    fecha_ini_pen                 =     gr_DatMar.fecha_ini_pen
      LET    fecha_resolucion              =     gr_DatMar.fecha_resolucion
      LET    periodo_pago                  =     lc_periodo_pago
      DISPLAY       lc_Telef              TO     lc_Telef


      DISPLAY       sec_pension           TO     sec_pension
      DISPLAY       fecha_ini_pen         TO     fecha_ini_pen
      DISPLAY       fecha_resolucion      TO     fecha_resolucion
      DISPLAY       periodo_pago          TO     periodo_pago
      INPUT BY NAME lc_Telef,
                    sec_pension,
                    fecha_ini_pen,
                    fecha_resolucion,
                    periodo_pago
                    WITHOUT DEFAULTS

         BEFORE FIELD sec_pension
              LET   sec_pension = gr_DatMar.sec_pension
              DISPLAY       sec_pension         TO  sec_pension

         BEFORE FIELD fecha_ini_pen
              LET   fecha_ini_pen = gr_DatMar.fecha_ini_pen
              DISPLAY       fecha_ini_pen       TO  fecha_ini_pen

         BEFORE FIELD fecha_resolucion
              LET   fecha_resolucion = gr_DatMar.fecha_resolucion
              DISPLAY       fecha_resolucion    TO  fecha_resolucion

         BEFORE FIELD periodo_pago
              CALL   f_PeridoPago(gr_ret_sol_issste_tx.fecha_ini_pen)     RETURNING lc_periodo_pago
              LET    periodo_pago       =     lc_periodo_pago
              DISPLAY       periodo_pago        TO  periodo_pago

         BEFORE FIELD lc_Telef
              IF     lc_Telef          IS NULL
               OR    lc_Telef          =  " "   THEN
                LET  lc_Telef          =  0
              ELSE
                LET  lc_Telef          =  lc_Telef
              END IF

         ON KEY (CONTROL-C,  INTERRUPT)
              LET   gs_Cance = 1
              EXIT  INPUT

         ON KEY (ESC)
              IF   lc_Telef        IS NULL
               OR  lc_Telef        = " "   THEN
                   CALL   f_DespMen("TELEFONO No puede ser un blanco ")
                   NEXT FIELD lc_Telef
              END IF

              IF  sec_pension          IS NULL
               OR sec_pension          = 0      THEN
                  CALL   f_DespMen("SECUENCIA PENSION no puede ser blanco o cero ")
                  NEXT FIELD sec_pension
              ELSE
                  LET   gr_ret_sol_issste_tx.sec_pension       = sec_pension
              END IF

              IF  fecha_ini_pen     IS NULL  THEN
                  CALL   f_DespMen("FECHA INICIO PENSION No puede quedar en blanco ")
                  NEXT FIELD fecha_ini_pen
              ELSE
                  LET   gr_ret_sol_issste_tx.fecha_ini_pen  = fecha_ini_pen
              END IF

              IF  fecha_resolucion     IS NULL  THEN
                  CALL   f_DespMen("FECHA RESOLUCION No puede quedar en blanco ")
                  NEXT FIELD fecha_resolucion
              ELSE
                  LET   gr_ret_sol_issste_tx.fecha_resolucion  = fecha_resolucion
              END IF

              CALL   f_PeridoPago(gr_ret_sol_issste_tx.fecha_ini_pen)     RETURNING lc_periodo_pago
              LET    periodo_pago       =     lc_periodo_pago
              IF  periodo_pago          IS NULL
               OR periodo_pago          = 0      THEN
                  CALL   f_DespMen("PERIODO DE PAGO no puede ser blanco o cero ")
                  NEXT FIELD periodo_pago
              ELSE
                  LET   gr_ret_sol_issste_tx.periodo_pago       = periodo_pago
              END IF
              LET   gs_Cance = 0
              EXIT INPUT

         AFTER FIELD lc_Telef
           IF   lc_Telef        IS NULL
            OR  lc_Telef        = " "   THEN
                CALL   f_DespMen("TELEFONO No puede ser un blanco ")
                NEXT FIELD lc_Telef
           END IF

         AFTER FIELD sec_pension
              IF  sec_pension          IS NULL
               OR sec_pension          = 0      THEN
                  CALL   f_DespMen("SECUENCIA PENSION no puede ser blanco o cero ")
                  NEXT FIELD sec_pension
              ELSE
                  LET   gr_ret_sol_issste_tx.sec_pension       = sec_pension
              END IF

         AFTER FIELD fecha_ini_pen
              IF  fecha_ini_pen     IS NULL  THEN
                  CALL   f_DespMen("FECHA INICIO PENSION No puede quedar en blanco ")
                  NEXT FIELD fecha_ini_pen
              ELSE
                  LET   gr_ret_sol_issste_tx.fecha_ini_pen  = fecha_ini_pen
              END IF

         AFTER FIELD fecha_resolucion
              IF  fecha_resolucion     IS NULL  THEN
                  CALL   f_DespMen("FECHA RESOLUCION No puede quedar en blanco ")
                  NEXT FIELD fecha_resolucion
              ELSE
                  LET   gr_ret_sol_issste_tx.fecha_resolucion  = fecha_resolucion
              END IF

         AFTER FIELD periodo_pago
              IF  periodo_pago          IS NULL
               OR periodo_pago          = 0      THEN
                  CALL   f_DespMen("PERIODO DE PAGO no puede ser blanco o cero ")
                  NEXT FIELD periodo_pago
              ELSE
                  LET   gr_ret_sol_issste_tx.periodo_pago       = periodo_pago
              END IF

      END INPUT

      IF   sec_pension           IS NULL
       OR  sec_pension           = 0
       OR  fecha_ini_pen         IS NULL
       OR  fecha_resolucion      IS NULL
       OR  periodo_pago          IS NULL
       OR  periodo_pago          = 0      THEN
           IF   gs_Cance = 1   THEN
                EXIT WHILE
           END IF
      ELSE
           EXIT WHILE
      END IF

  END WHILE
  RETURN gr_ret_sol_issste_tx.*, lc_Telef
END FUNCTION


### Modifica Datos Solicitante
### Esta rutina es igual que la rutina de alta, parece que algun momneto sera distinta .
FUNCTION f_ModifDaSol(lr_ret_sol_issste_tx)
  DEFINE      lr_ret_sol_issste_tx      RECORD LIKE ret_sol_issste_tx.*
  DEFINE      gr_ret_sol_issste_tx      RECORD LIKE ret_sol_issste_tx.*
  DEFINE      gr_ret_solicitante        RECORD LIKE ret_solicitante.*
  DEFINE      li_procede                SMALLINT

  #CPL-1844
  DEFINE ls_constancia SMALLINT

  LET        li_procede       =  0
  IF  lr_ret_sol_issste_tx.estado_solicitud   = 10  THEN
      ### Se puede modificar
      LET    li_procede    =  0
      LET    gr_ret_sol_issste_tx.*    =  lr_ret_sol_issste_tx.*
  ELSE
      CALL   f_DespMen("Solicitud no puede ser Modificada ...... ")
      LET    li_procede    =  1
  END IF

  EXECUTE     p_SelRetS          USING lr_ret_sol_issste_tx.consecutivo
                                 INTO  gr_ret_solicitante.*
  #CPL-1844
  LET ls_constancia = 0

  IF  li_procede           =  0    THEN
      ### Actualiza primera parte
      LET   gr_ret_sol_issste_tx.fecha_modifica      = TODAY
      LET   gr_ret_sol_issste_tx.usuario_modifica    = gc_usuario
      LET   gs_Cance                                 = 0

      IF    gs_Cance       =  0  THEN
            IF    gr_ret_sol_issste_tx.tipo_retiro = 'A'
             OR   gr_ret_sol_issste_tx.tipo_retiro = 'B'
             OR   gr_ret_sol_issste_tx.tipo_retiro = 'I' THEN
                  CALL  f_InpSemCot2(gr_ret_sol_issste_tx.semanas_cotizadas,
                                     gr_ret_solicitante.telefono,
                                     gr_ret_sol_issste_tx.estad_cod,
                                     gr_ret_sol_issste_tx.deleg_cod,
                                     gr_ret_sol_issste_tx.regimen,
                                     gr_ret_sol_issste_tx.tipo_seguro,
                                     gr_ret_sol_issste_tx.tipo_pension,
                                     gr_ret_sol_issste_tx.cve_pension,
                                     gr_ret_sol_issste_tx.tipo_prestacion)
                                     RETURNING gr_ret_sol_issste_tx.semanas_cotizadas,
                                               gr_ret_solicitante.telefono,
                                               gr_ret_sol_issste_tx.estad_cod,
                                               gr_ret_sol_issste_tx.deleg_cod
                  IF    gs_Cance =  0  THEN
                        CALL  f_InputComun(gr_ret_sol_issste_tx.*,
                                           gr_ret_solicitante.*)
                                           RETURNING gr_ret_sol_issste_tx.*,
                                                     gr_ret_solicitante.*
                        IF    gs_Cance =  0  THEN
                            EXECUTE    p_UpdSoli        USING gr_ret_solicitante.apellido_paterno,
                                                              gr_ret_solicitante.apellido_materno,
                                                              gr_ret_solicitante.nombres,
                                                              gr_ret_solicitante.paren_cod,
                                                              gr_ret_solicitante.telefono,
                                                              gr_ret_sol_issste_tx.consecutivo

                            EXECUTE    p_UpdReti        USING gr_ret_sol_issste_tx.fecha_solicitud,
                                                              gr_ret_sol_issste_tx.folio_solicitud,
                                                              gr_ret_sol_issste_tx.semanas_cotizadas,
                                                              gr_ret_sol_issste_tx.estad_cod,
                                                              gr_ret_sol_issste_tx.deleg_cod,
                                                              gr_ret_sol_issste_tx.fecha_modifica,
                                                              gr_ret_sol_issste_tx.usuario_modifica,
                                                              gr_ret_sol_issste_tx.consecutivo
                            #CPL-1844
                            LET ls_constancia = 1
                        END IF
                  END IF
            END IF

            IF    gr_ret_sol_issste_tx.tipo_retiro = 'C'  THEN
                  CALL  f_MasdelC(gr_ret_sol_issste_tx.*,
                                  gr_ret_solicitante.telefono)       RETURNING gr_ret_sol_issste_tx.*,
                                                                               gr_ret_solicitante.telefono
                  IF    gs_Cance =  0  THEN
                        CALL  f_InputComun(gr_ret_sol_issste_tx.*,
                                           gr_ret_solicitante.*)    RETURNING gr_ret_sol_issste_tx.*,
                                                                              gr_ret_solicitante.*
                        IF    gs_Cance =  0  THEN
                            EXECUTE    p_UpdSoli        USING gr_ret_solicitante.apellido_paterno,
                                                              gr_ret_solicitante.apellido_materno,
                                                              gr_ret_solicitante.nombres,
                                                              gr_ret_solicitante.paren_cod,
                                                              gr_ret_solicitante.telefono,
                                                              gr_ret_sol_issste_tx.consecutivo

                            EXECUTE    p_UpdRet1        USING gr_ret_sol_issste_tx.fecha_solicitud,
                                                              gr_ret_sol_issste_tx.folio_solicitud,
                                                              gr_ret_sol_issste_tx.aseguradora,
                                                              gr_ret_sol_issste_tx.actuario,
                                                              gr_ret_sol_issste_tx.num_plan_privado,
                                                              gr_ret_sol_issste_tx.semanas_cotizadas,
                                                              gr_ret_sol_issste_tx.estad_cod,
                                                              gr_ret_sol_issste_tx.deleg_cod,
                                                              gr_ret_sol_issste_tx.fecha_modifica,
                                                              gr_ret_sol_issste_tx.usuario_modifica,
                                                              gr_ret_sol_issste_tx.consecutivo
                            #CPL-1844
                            LET ls_constancia = 1
                        END IF
                  END IF
            END IF

            IF    gr_ret_sol_issste_tx.tipo_retiro = 'D'  THEN
                  CALL  f_InpSemCot(gr_ret_sol_issste_tx.semanas_cotizadas,
                                    gr_ret_solicitante.telefono,
                                    gr_ret_sol_issste_tx.estad_cod,
                                    gr_ret_sol_issste_tx.deleg_cod)
                                    RETURNING gr_ret_sol_issste_tx.semanas_cotizadas,
                                              gr_ret_solicitante.telefono,
                                              gr_ret_sol_issste_tx.estad_cod,
                                              gr_ret_sol_issste_tx.deleg_cod
                  IF    gs_Cance =  0  THEN
                        CALL  f_InputComun(gr_ret_sol_issste_tx.*,
                                           gr_ret_solicitante.*)    RETURNING gr_ret_sol_issste_tx.*,
                                                                              gr_ret_solicitante.*
                        IF    gs_Cance =  0  THEN
                             EXECUTE    p_UpdSoli        USING gr_ret_solicitante.apellido_paterno,
                                                               gr_ret_solicitante.apellido_materno,
                                                               gr_ret_solicitante.nombres,
                                                               gr_ret_solicitante.paren_cod,
                                                               gr_ret_solicitante.telefono,
                                                               gr_ret_sol_issste_tx.consecutivo

                             EXECUTE    p_UpdReti        USING gr_ret_sol_issste_tx.fecha_solicitud,
                                                               gr_ret_sol_issste_tx.folio_solicitud,
                                                               gr_ret_sol_issste_tx.semanas_cotizadas,
                                                               gr_ret_sol_issste_tx.estad_cod,
                                                               gr_ret_sol_issste_tx.deleg_cod,
                                                               gr_ret_sol_issste_tx.fecha_modifica,
                                                               gr_ret_sol_issste_tx.usuario_modifica,
                                                               gr_ret_sol_issste_tx.consecutivo
                             #CPL-1844
                             LET ls_constancia = 1
                        END IF
                  END IF
            END IF

            IF    gr_ret_sol_issste_tx.tipo_retiro = 'E'  THEN
                  CALL  f_InpMasE(gr_ret_sol_issste_tx.*,
                                  gr_ret_solicitante.telefono) RETURNING gr_ret_sol_issste_tx.*,
                                                                         gr_ret_solicitante.telefono
                  IF    gs_Cance =  0  THEN
                        CALL  f_InputComun(gr_ret_sol_issste_tx.*,
                                           gr_ret_solicitante.*)    RETURNING gr_ret_sol_issste_tx.*,
                                                                              gr_ret_solicitante.*
                        IF    gs_Cance =  0  THEN
                             EXECUTE    p_UpdSoli        USING gr_ret_solicitante.apellido_paterno,
                                                               gr_ret_solicitante.apellido_materno,
                                                               gr_ret_solicitante.nombres,
                                                               gr_ret_solicitante.paren_cod,
                                                               gr_ret_solicitante.telefono,
                                                               gr_ret_sol_issste_tx.consecutivo

                             EXECUTE    p_UpdRet2        USING gr_ret_sol_issste_tx.fecha_solicitud,
                                                               gr_ret_sol_issste_tx.folio_solicitud,
                                                               gr_ret_sol_issste_tx.cve_doc_probatorio,
                                                               gr_ret_sol_issste_tx.semanas_cotizadas,
                                                               gr_ret_sol_issste_tx.estad_cod,
                                                               gr_ret_sol_issste_tx.deleg_cod,
                                                               gr_ret_sol_issste_tx.fecha_modifica,
                                                               gr_ret_sol_issste_tx.usuario_modifica,
                                                               gr_ret_sol_issste_tx.consecutivo
                            #CPL-1844
                            LET ls_constancia = 1
                        END IF
                  END IF
            END IF

            IF    gr_ret_sol_issste_tx.tipo_retiro = 'K'  THEN
                  CALL  f_InpMasK(gr_ret_sol_issste_tx.*,
                                  gr_ret_solicitante.telefono)  RETURNING gr_ret_sol_issste_tx.*,
                                                                          gr_ret_solicitante.telefono
                  IF    gs_Cance =  0  THEN
                        CALL  f_InputComun(gr_ret_sol_issste_tx.*,
                                           gr_ret_solicitante.*)    RETURNING gr_ret_sol_issste_tx.*,
                                                                              gr_ret_solicitante.*
                        IF    gs_Cance =  0  THEN
                            EXECUTE    p_UpdSoli        USING gr_ret_solicitante.apellido_paterno,
                                                              gr_ret_solicitante.apellido_materno,
                                                              gr_ret_solicitante.nombres,
                                                              gr_ret_solicitante.paren_cod,
                                                              gr_ret_solicitante.telefono,
                                                              gr_ret_sol_issste_tx.consecutivo

                            EXECUTE    p_UpdRet3        USING gr_ret_sol_issste_tx.fecha_solicitud,
                                                              gr_ret_sol_issste_tx.folio_solicitud,
                                                              gr_ret_sol_issste_tx.fecha_ini_pen,
                                                              gr_ret_sol_issste_tx.fecha_resolucion,
                                                              gr_ret_sol_issste_tx.periodo_pago,
                                                              gr_ret_sol_issste_tx.estad_cod,
                                                              gr_ret_sol_issste_tx.deleg_cod,
                                                              gr_ret_sol_issste_tx.fecha_modifica,
                                                              gr_ret_sol_issste_tx.usuario_modifica,
                                                              gr_ret_sol_issste_tx.consecutivo

                            #CPL-1844
                            LET ls_constancia = 1
                        END IF
                  END IF
            END IF

      END IF
  END IF

  #CPL-1844
  IF ls_constancia <> 0 THEN
     UPDATE ret_sol_issste_constancia
     SET    constancia = gr_constancia.constancia
     WHERE  nss         = gr_ret_sol_issste_tx.nss
     AND    consecutivo = gr_ret_sol_issste_tx.consecutivo
  END IF
END FUNCTION


### Elimina Solicitud, Solicitante, Beneficiarios
FUNCTION f_ElimDaSol(lr_ret_sol_issste_tx)
  DEFINE      lr_ret_sol_issste_tx      RECORD LIKE ret_sol_issste_tx.*
  DEFINE      li_procede                SMALLINT
  DEFINE      lc_Resp                   CHAR(01)

  LET        li_procede       =  0
  IF  lr_ret_sol_issste_tx.estado_solicitud   = 10
   OR lr_ret_sol_issste_tx.estado_solicitud   = 20  THEN
      ### Se puede eliminar
      LET    li_procede    =  0
  ELSE
      CALL   f_DespMen("Solicitud no puede ser Eliminada ...... ")
      LET    li_procede    =  1
  END IF

  IF    li_procede  =   0  THEN
       PROMPT " ESTA SEGURO QUE DESEA ELIMINAR LA SOLICITUD Si/No " FOR CHAR lc_Resp
       IF     lc_Resp    = 's'
        OR    lc_Resp    = 'S'  THEN
           LET    li_procede    =  0
       ELSE
           LET    li_procede    =  1
       END IF

       IF  li_procede           =  0    THEN
           ### Borra solciitud, solicitante, beneficiarios y marcas
           DELETE  FROM ret_sol_issste_tx
            WHERE  consecutivo  = lr_ret_sol_issste_tx.consecutivo

           DELETE  FROM ret_solicitante
            WHERE  consecutivo  = lr_ret_sol_issste_tx.consecutivo

           DELETE  FROM ret_beneficiario
            WHERE  consecutivo  = lr_ret_sol_issste_tx.consecutivo

           DELETE  FROM cta_act_marca
            WHERE  nss          = lr_ret_sol_issste_tx.nss
              AND  correlativo  = lr_ret_sol_issste_tx.consecutivo

           DELETE  FROM cta_his_marca
            WHERE  nss          = lr_ret_sol_issste_tx.nss
              AND  correlativo  = lr_ret_sol_issste_tx.consecutivo

            #CPL-1844
            DELETE  FROM  ret_sol_issste_constancia
            WHERE  nss         = lr_ret_sol_issste_tx.nss
            AND    consecutivo = lr_ret_sol_issste_tx.consecutivo
       END IF
  END IF

END FUNCTION


### Verifica folio en CAV
FUNCTION f_VerCAV(ls_folio)
  DEFINE      ls_folio         INTEGER
  DEFINE      ls_cuantos       INTEGER
  DEFINE      lc_tipo_id       CHAR(01)

  LET         ls_cuantos      = 0
  LET         lc_tipo_id      = 'S'
  EXECUTE     p_SelCAV                   USING ls_folio
                                         INTO  ls_cuantos

  IF          ls_cuantos                 > 0   THEN
     LET      gi_folio                   = ls_folio
     CALL     f_obtiene_datos_cav(ls_folio, lc_tipo_id)
                        RETURNING gc_num_cuenta, gr_ret_sol_issste_tx.fecha_solicitud
     --- PEIS DISPLAY gc_num_cuenta   TO  num_cuenta
     RETURN 0
  ELSE
     RETURN 1
  END IF

END FUNCTION


### Ver marcaje
FUNCTION f_VerMarca(li_tipo_retiro, lc_nti)
 DEFINE  ls_VerMarca               SMALLINT
 DEFINE  lc_Mensaje                CHAR(100)
 DEFINE  li_tipo_movimiento        LIKE tab_ret_issste.movimiento
 DEFINE  li_marca_activa           LIKE cta_act_marca.marca_cod
 DEFINE  li_rechazo_cod            LIKE cta_convivencia.rechazo_cod
 DEFINE  li_tipo_retiro            LIKE tab_ret_issste.tipo_retiro
 DEFINE  lc_nti                    LIKE cta_act_marca.nss
 DEFINE  esp    CHAR(01)

 LET      ls_VerMarca              =       0
 EXECUTE  d_BusTipRet              USING   li_tipo_retiro
                                   INTO    li_tipo_movimiento

 FOREACH  d_BusMar                 USING   lc_nti
                                   INTO    li_marca_activa
    EXECUTE   d_BusRech            USING   li_marca_activa,
                                           li_tipo_movimiento
                                   INTO    li_rechazo_cod

    IF         li_rechazo_cod IS NOT NULL
     AND       li_rechazo_cod <> 0 THEN
               LET   ls_VerMarca = 1
               LET   lc_Mensaje = "La cuenta se encuentra en otro Proceso Operativo, Marca Act: ", li_marca_activa USING "##&"
               EXIT FOREACH
    ELSE
               LET   ls_VerMarca = 0
    END IF
 END FOREACH

 RETURN ls_VerMarca,
        lc_Mensaje

END FUNCTION


### Ver saldo mayor a ceros
FUNCTION f_VerSaldo(lc_nss, li_grupo)
 DEFINE   lc_nss                 CHAR(18)
 DEFINE   li_grupo               LIKE          ret_sol_issste_tx.grupo
 DEFINE   lr_Subcta              RECORD LIKE   tab_agrupa_subcta.*
 DEFINE   lr_regresa             RECORD
          subcuenta              LIKE          dis_cuenta.subcuenta,
          siefore                LIKE          dis_cuenta.siefore,
          monto_en_acciones      LIKE          dis_cuenta.monto_en_acciones,
          monto_en_pesos         LIKE          dis_cuenta.monto_en_pesos
          END RECORD
 DEFINE   li_SumaSaldo           DECIMAL(18,6)
 DEFINE   ls_VerSaldo            SMALLINT
 DEFINE   ls_cero                SMALLINT
 DEFINE   lc_Mensaje             CHAR(100)
 DEFINE   ld_today               DATE

 LET      li_SumaSaldo             =   0
 LET      ld_today                 =   TODAY
 LET      ls_cero                  =   0

 FOREACH  d_BusCta             USING   li_grupo
                               INTO    lr_Subcta.*

    DECLARE  d_ExeSal                CURSOR FOR p_ExeSal
    OPEN     d_ExeSal                 USING     lc_nss              , # nss
                                                lr_Subcta.subcuenta , # subcuenta
                                                ls_cero             , #
                                                ld_today              # fecha
    FETCH    d_ExeSal                  INTO     lr_regresa.*
    CLOSE    d_ExeSal

    IF       lr_regresa.monto_en_pesos   IS NULL
     OR      lr_regresa.monto_en_pesos   < 0 THEN
    ELSE
         LET      li_SumaSaldo         =     li_SumaSaldo   +  lr_regresa.monto_en_pesos
    END IF
    LET   lr_regresa.monto_en_pesos   = 0
    LET   lr_regresa.monto_en_acciones   = 0
 END FOREACH

 IF  li_SumaSaldo    = 0
  OR li_SumaSaldo    < 0   THEN
     LET   ls_VerSaldo     = 1
     LET   lc_Mensaje      = "No hay Saldo en Cuentas para el Tipo de Retiro solicitado, grupo ", li_grupo
 ELSE
     LET   ls_VerSaldo     = 0
 END IF

 RETURN  ls_VerSaldo,
         lc_Mensaje
END FUNCTION


-- Recupera los valores de captura CAV
FUNCTION f_obtiene_datos_cav(p_folio_sol, p_tipo_id)
    DEFINE
        p_folio_sol         LIKE rec_solicitud.folio_rec    ,
        p_tipo_id           LIKE rec_solicitud.tipo_id
    DEFINE
        lc_num_cuenta       LIKE ret_beneficiario.num_cuenta        ,
        ld_fecha_reclamo    LIKE ret_sol_issste_tot.fecha_solicitud

    -- Obtiene numero de cuenta bansefi
    SELECT bansefi
    INTO   lc_num_cuenta
    FROM   solicitud_complemento
    WHERE  folio_rec = p_folio_sol
    AND    tipo_id   = p_tipo_id

    -- Obtiene fecha de reclamo
    SELECT freclamo
    INTO   ld_fecha_reclamo
    FROM   rec_solicitud
    WHERE  folio_rec = p_folio_sol
    AND    tipo_id   = p_tipo_id

    RETURN lc_num_cuenta, ld_fecha_reclamo
END FUNCTION


### Recapturar folio
FUNCTION  f_Recaptura_Folio(li_tipo_retiro, li_folio_solicitud_ori)
   DEFINE li_tipo_retiro         SMALLINT
   DEFINE li_folio_solicitud_ori INTEGER

   DEFINE lr_captura RECORD
          folio_solicitud INTEGER,
          tipo_id         CHAR(1)
   END RECORD

   DEFINE li_recaptura    SMALLINT
   DEFINE lc_cadena_inv   CHAR(15)
   DEFINE lc_cadena_ori   CHAR(15)
   DEFINE li_length       SMALLINT
   DEFINE li_cont         SMALLINT
   DEFINE lc_paso         CHAR(1)

   LET li_recaptura = 0
   LET lr_captura.tipo_id = "S"

   LET lc_cadena_ori = li_folio_solicitud_ori USING "<<<<<<<<<<<<<<<"
   LET li_cont = 1

   FOR li_length = LENGTH (lc_cadena_ori) TO 1 STEP -1
          LET lc_cadena_inv[li_cont,li_cont] = lc_cadena_ori[li_length,li_length]
          LET li_cont = li_cont + 1
   END FOR

   OPEN WINDOW RETM9208 AT 16,6 WITH FORM "RETM9208" ATTRIBUTE(BORDER)

   DISPLAY "  RECAPTURE EL FOLIO DE SOLICITUD " AT 2,1 ATTRIBUTE(REVERSE)

   INPUT BY NAME lr_captura.folio_solicitud,
                 lr_captura.tipo_id
                 WITHOUT DEFAULTS

     AFTER FIELD folio_solicitud
                 IF lr_captura.folio_solicitud IS NULL OR
                          lr_captura.folio_solicitud = " " THEN

                          ERROR "DEBE CAPTURAR EL FOLIO DE SOLICITUD"
                          SLEEP 3
                          ERROR ""
                          NEXT FIELD folio_solicitud
                 ELSE
                          EXIT INPUT
                 END IF

          ON KEY (CONTROL-C, INTERRUPT)
                 LET lr_captura.folio_solicitud = -1
                 EXIT INPUT

   END INPUT

   IF lr_captura.folio_solicitud <> lc_cadena_inv THEN
          LET li_recaptura = 1
          ERROR "LOS FOLIOS NO COINCIDEN, VERIFIQUE LA INFORMACION"
          SLEEP 2
          ERROR ""
   END IF

   CLOSE WINDOW RETM9208

   RETURN li_recaptura

END FUNCTION


### Obtener desc de estado
FUNCTION f_VerEstad(lc_estad_cod)
 DEFINE  lc_estad_cod                CHAR(03)
 DEFINE  lc_estad_desc               CHAR(100)

  SELECT  estad_desc
    INTO  lc_estad_desc
    FROM  tab_estado
   WHERE  estad_cod = lc_estad_cod

  IF STATUS = NOTFOUND  THEN
     LET lc_estad_desc = 'No hay descripción    '
  END IF

  RETURN lc_estad_desc
END FUNCTION


### Obtener desc de deleg/municipio
FUNCTION f_VerDeleg(lc_estad_cod, lc_deleg_cod)
 DEFINE   lc_estad_cod               INTEGER
 DEFINE   lc_deleg_cod               INTEGER
 DEFINE   lc_deleg_desc              CHAR(100)

  SELECT  deleg_desc
    INTO  lc_deleg_desc
    FROM  tab_delegacion
   WHERE  estad_cod = lc_estad_cod
     AND  deleg_cod = (lc_estad_cod * 1000 + lc_deleg_cod )

  IF  STATUS = NOTFOUND  THEN
  END IF

  RETURN lc_deleg_desc
END FUNCTION


FUNCTION  f_VerRegimen(regimen)
  DEFINE  regimen                 LIKE  ret_sol_issste_tx.regimen
  DEFINE  regimen_desc            CHAR(100)

  EXECUTE p_BusRegimen            USING  regimen
                                  INTO   regimen_desc

  IF      STATUS = NOTFOUND   THEN
          LET  regimen_desc  = ' NO hay Descripción .. '
  END IF

  RETURN  regimen_desc
END FUNCTION


FUNCTION  f_VerSeguro(seguro)
  DEFINE  seguro                  LIKE  ret_sol_issste_tx.tipo_seguro
  DEFINE  seguro_desc             CHAR(100)

  EXECUTE p_BusSeguro             USING  seguro
                                  INTO   seguro_desc

  IF      STATUS = NOTFOUND   THEN
          LET  seguro_desc  = ' NO hay Descripción .. '
  END IF

  RETURN  seguro_desc
END FUNCTION


FUNCTION  f_VertipoPension(TipoPension)
  DEFINE  TipoPension             LIKE  ret_sol_issste_tx.tipo_pension
  DEFINE  TipoPension_desc        CHAR(100)

  EXECUTE p_BusTipoPension        USING  TipoPension
                                  INTO   TipoPension_desc

  IF      STATUS = NOTFOUND   THEN
          LET  TipoPension_desc  = ' NO hay Descripción .. '
  END IF

  RETURN  TipoPension_desc
END FUNCTION


FUNCTION  f_VerCvePension(CvePension)
  DEFINE  CvePension              LIKE  ret_sol_issste_tx.cve_pension
  DEFINE  CvePension_desc         CHAR(100)

  EXECUTE p_BusCvePension         USING  CvePension
                                  INTO   CvePension_desc

  IF      STATUS = NOTFOUND   THEN
          LET  CvePension_desc  = ' NO hay Descripción .. '
  END IF

  RETURN  CvePension_desc
END FUNCTION


FUNCTION  f_VerPrestacion(Prestacion)
  DEFINE  Prestacion              LIKE  ret_sol_issste_tx.tipo_prestacion
  DEFINE  Prestacion_desc         CHAR(100)

  EXECUTE p_BusPrestacion         USING  Prestacion
                                  INTO   Prestacion_desc

  IF      STATUS = NOTFOUND   THEN
          LET  Prestacion_desc  = ' NO hay Descripción .. '
  END IF

  RETURN  Prestacion_desc
END FUNCTION


### Modificación a solicitud de retiro
FUNCTION f_ModifDaSol1(lr_ret_parcial_issste, lr_ret_monto_par_issste)
  DEFINE      lr_ret_parcial_issste      RECORD LIKE ret_parcial_issste.*
  DEFINE      lr_ret_monto_par_issste    RECORD LIKE ret_monto_par_issste.*
  DEFINE      gr_ret_parcial_issste      RECORD LIKE ret_parcial_issste.*
  DEFINE      gr_ret_monto_par_issste    RECORD LIKE ret_monto_par_issste.*
  DEFINE      gr_ret_solicitante         RECORD LIKE ret_solicitante.*
  DEFINE      li_procede                 SMALLINT

  #CPL-1844
  DEFINE ls_constancia SMALLINT

  #CPL-1844
  LET ls_constancia = 0

  LET        li_procede       =  0
  IF  lr_ret_parcial_issste.estado_solicitud   = 10  THEN
      ### Se puede modificar
      LET    li_procede    =  0
      LET    gr_ret_parcial_issste.*    =  lr_ret_parcial_issste.*
      LET    gr_ret_monto_par_issste.*  =  lr_ret_monto_par_issste.*
  ELSE
      CALL   f_DespMen("Solicitud no puede ser Modificada ...... ")
      LET    li_procede    =  1
  END IF

  IF  li_procede           =  0    THEN
      ### Actualiza primera parte
      LET   gr_ret_parcial_issste.fecha_modifica      = TODAY
      LET   gr_ret_parcial_issste.usuario_modifica    = gc_usuario
      LET   gs_Cance                                  = 0

      IF    gr_ret_parcial_issste.regimen  = 'DT'   THEN
            CALL  f_InpdelDT(gr_ret_parcial_issste.*, gr_ret_monto_par_issste.*, gr_ret_solicitante.*)
                  RETURNING  gr_ret_parcial_issste.*,
                             gr_ret_monto_par_issste.*,
                             gr_ret_solicitante.*

            IF    gs_Cance                        =     0   THEN
                  ###...IF      gs_afores         =   578   THEN
                  ###...    IF      gr_ret_sol_issste_tx.deleg_cod IS NOT NULL
                  ###...      AND   gr_ret_sol_issste_tx.estad_cod IS NOT NULL THEN
                  ###...        LET gr_ret_sol_issste_tx.deleg_cod =   gr_ret_sol_issste_tx.estad_cod * 1000
                  ###...                                           +   gr_ret_sol_issste_tx.deleg_cod
                  ###...    END IF
                  ###...END IF

                  EXECUTE    p_UpdRetiPar1    USING gr_ret_parcial_issste.fecha_solicitud,
                                                    gr_ret_parcial_issste.folio_solicitud,
                                                    ### gr_ret_parcial_issste.estad_cod,
                                                    ### gr_ret_parcial_issste.deleg_cod,
                                                    gr_ret_parcial_issste.fecha_modifica,
                                                    gr_ret_parcial_issste.usuario_modifica,
                                                    gr_ret_parcial_issste.consecutivo
                  EXECUTE    p_UpdRetiPar2    USING gr_ret_monto_par_issste.f_ultimo_aporte,
                                                    gr_ret_monto_par_issste.mto_ultimo_aporte,
                                                    gr_ret_monto_par_issste.mto_18ultimo_aporte,
                                                    gr_ret_monto_par_issste.mto_10p_sarissste,
                                                    gr_ret_monto_par_issste.mto_a_pagar,
                                                    gr_ret_monto_par_issste.consecutivo
                  LET        gr_ret_solicitante.paren_cod    = 12
                  EXECUTE    p_UpdSoli        USING gr_ret_solicitante.apellido_paterno,
                                                    gr_ret_solicitante.apellido_materno,
                                                    gr_ret_solicitante.nombres,
                                                    gr_ret_solicitante.paren_cod,
                                                    gr_ret_solicitante.telefono,
                                                    gr_ret_parcial_issste.consecutivo

                  #CPL-1844
                  LET ls_constancia = 1
            END IF
      ELSE
            CALL  f_InputComun2(gr_ret_parcial_issste.*, gr_ret_monto_par_issste.*, gr_ret_solicitante.*)
                  RETURNING     gr_ret_parcial_issste.*,
                                gr_ret_monto_par_issste.*,
                                gr_ret_solicitante.*

            IF    gs_Cance                        =     0   THEN
                  ###...IF      gs_afores         =   578   THEN
                  ###...    IF      gr_ret_sol_issste_tx.deleg_cod IS NOT NULL
                  ###...      AND   gr_ret_sol_issste_tx.estad_cod IS NOT NULL THEN
                  ###...        LET gr_ret_sol_issste_tx.deleg_cod =   gr_ret_sol_issste_tx.estad_cod * 1000
                  ###...                                           +   gr_ret_sol_issste_tx.deleg_cod
                  ###...    END IF
                  ###...END IF

                  EXECUTE    p_UpdRetiPar1    USING gr_ret_parcial_issste.fecha_solicitud,
                                                    gr_ret_parcial_issste.folio_solicitud,
                                                    ### gr_ret_parcial_issste.estad_cod,
                                                    ### gr_ret_parcial_issste.deleg_cod,
                                                    gr_ret_parcial_issste.fecha_modifica,
                                                    gr_ret_parcial_issste.usuario_modifica,
                                                    gr_ret_parcial_issste.consecutivo
                  EXECUTE    p_UpdRetiPar3    USING gr_ret_monto_par_issste.salario_base_cot,
                                                    gr_ret_monto_par_issste.mto_75sbc,
                                                    gr_ret_monto_par_issste.mto_10p_rcv,
                                                    gr_ret_monto_par_issste.mto_a_pagar,
                                                    gr_ret_monto_par_issste.consecutivo
                  LET        gr_ret_solicitante.paren_cod    = 12
                  EXECUTE    p_UpdSoli        USING gr_ret_solicitante.apellido_paterno,
                                                    gr_ret_solicitante.apellido_materno,
                                                    gr_ret_solicitante.nombres,
                                                    gr_ret_solicitante.paren_cod,
                                                    gr_ret_solicitante.telefono,
                                                    gr_ret_parcial_issste.consecutivo

                  #CPL-1844
                  LET ls_constancia = 1
            END IF
      END IF
  END IF

  #CPL-1844
  IF ls_constancia <> 0 THEN
     UPDATE ret_sol_issste_constancia
     SET    constancia = gr_constancia.constancia
     WHERE  nss         = gr_ret_parcial_issste.nss
     AND    consecutivo = gr_ret_parcial_issste.consecutivo
  END IF
END FUNCTION


### Elimina Solicitud Parcial, Solicitante, Beneficiarios
FUNCTION f_ElimDaSol1(lr_ret_parcial_issste)
  DEFINE      lr_ret_parcial_issste     RECORD LIKE ret_parcial_issste.*
  DEFINE      li_procede                SMALLINT
  DEFINE      lc_Resp                   CHAR(01)

  LET        li_procede       =  0
  IF  lr_ret_parcial_issste.estado_solicitud   = 10
   OR lr_ret_parcial_issste.estado_solicitud   = 20  THEN
      ### Se puede eliminar
      LET    li_procede    =  0
  ELSE
      CALL   f_DespMen("Solicitud no puede ser Eliminada ...... ")
      LET    li_procede    =  1
  END IF

  IF    li_procede  =   0  THEN
       PROMPT " ESTA SEGURO QUE DESEA ELIMINAR LA SOLICITUD Si/No " FOR CHAR lc_Resp
       IF     lc_Resp    = 's'
        OR    lc_Resp    = 'S'  THEN
           LET    li_procede    =  0
       ELSE
           LET    li_procede    =  1
       END IF

       IF  li_procede           =  0    THEN
           ### Borra solciitud, solicitante, beneficiarios y marcas
           DELETE  FROM ret_parcial_issste
            WHERE  consecutivo  = lr_ret_parcial_issste.consecutivo

           DELETE  FROM ret_monto_par_issste
            WHERE  consecutivo  = lr_ret_parcial_issste.consecutivo

           DELETE  FROM ret_solicitante
            WHERE  consecutivo  = lr_ret_parcial_issste.consecutivo

           DELETE  FROM ret_beneficiario
            WHERE  consecutivo  = lr_ret_parcial_issste.consecutivo

           DELETE  FROM cta_act_marca
            WHERE  nss          = lr_ret_parcial_issste.nss
              AND  correlativo  = lr_ret_parcial_issste.consecutivo

           DELETE  FROM cta_his_marca
            WHERE  nss          = lr_ret_parcial_issste.nss
              AND  correlativo  = lr_ret_parcial_issste.consecutivo

           #CPL-1844
           DELETE  FROM  ret_sol_issste_constancia
           WHERE  nss         = lr_ret_parcial_issste.nss
           AND    consecutivo = lr_ret_parcial_issste.consecutivo
       END IF
  END IF

END FUNCTION

#---------------------------------------------------------------------------#
# f_obtiene_afiliado : Obtiene el registro activo en la tabla de afiliados  #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_afiliado(pc_curp)

    DEFINE pc_curp LIKE ret_sol_issste_tx.curp

    DEFINE lr_afi_mae_afiliado RECORD LIKE afi_mae_afiliado.*

    DEFINE ld_saldo_acc LIKE dis_cuenta.monto_en_acciones

    DEFINE
        ls_afiliado                 SMALLINT

    -- -----------------------------------------------------------------------------

    INITIALIZE lr_afi_mae_afiliado.* TO NULL

    LET ls_afiliado     = FALSE
    LET ld_saldo_acc    = 0

    FOREACH d_SelAfi USING pc_curp
                     INTO  lr_afi_mae_afiliado.*

        SELECT NVL(SUM(monto_en_acciones), 0)
        INTO   ld_saldo_acc
        FROM   dis_cuenta
        WHERE  nss  = lr_afi_mae_afiliado.n_seguro

        IF ld_saldo_acc <= 0 THEN
            LET ls_afiliado = FALSE
            INITIALIZE lr_afi_mae_afiliado.* TO NULL
            CONTINUE FOREACH
        ELSE
            LET ls_afiliado = TRUE
            EXIT FOREACH
        END IF

    END FOREACH

    RETURN lr_afi_mae_afiliado.*, ls_afiliado

END FUNCTION

################################################################################