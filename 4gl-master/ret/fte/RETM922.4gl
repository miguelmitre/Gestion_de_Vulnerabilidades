
################################################################################
#Proyecto          => SISTEMA DE AFORES ( SAFRE )                              #
#Propietario       => E.F.P.                                                   #
#Programa RETM922  => Consulta de Retiros ISSSTE                               #
#Fecha creacion    => 12 Oct 2009                                              #
###By                => JGHM                                                     #
#Sistema           => RET                                                      #
#Programa          => RETM922  --- Funciones de Consulta                       #
################################################################################
#Fecha Actualiz.   => 22/01/22015                                               #
#Actualizacion     =>Cesar D. Chavéz  Mtz.                                      #
#                  => Se agrega solicitud de constancia en las pantallas de     #
#                     Agregar, Consulta, Modificacion y Elimina para los retiros#
#                     Totales y parciales CPL-1844                              #
#################################################################################

DATABASE safre_af

GLOBALS  "RETM921.4gl"

### Prepare general
FUNCTION f_preparac()
   DEFINE  lc_query              CHAR(1500)

   --- SELECCIONA Retiros
   LET        lc_query    =  " SELECT  *                         ",
                             "   FROM  ret_sol_issste_tx         ",
                             "  WHERE  consecutivo        =   ?  "
   PREPARE    p_SelRet1          FROM  lc_query
   DECLARE    d_SelRet1        CURSOR  FOR  p_SelRet1

   LET        lc_query    =  " SELECT  *                         ",
                             "   FROM  ret_parcial_issste        ",
                             "  WHERE  consecutivo        =   ?  "
   PREPARE    p_SelRet2          FROM  lc_query
   DECLARE    d_SelRet2        CURSOR  FOR  p_SelRet2

   LET        lc_query    =  " SELECT  *                         ",
                             "   FROM  ret_monto_par_issste      ",
                             "  WHERE  consecutivo        =   ?  "
   PREPARE    p_SelRet3          FROM  lc_query
   DECLARE    d_SelRet3        CURSOR  FOR  p_SelRet3

   LET        gc_query    =  " SELECT  *                        ",
                             "   FROM  afi_mae_afiliado         ",
                             "  WHERE  n_unico           =   ?  "
   PREPARE    p_SelAfi1          FROM  gc_query
   DECLARE    d_SelAfi1        CURSOR  FOR p_SelAfi1

   LET        gc_query    =  " SELECT  tipo_retiro,                              ",
                             "         tipo_retiro||'-'||descripcion             ",
                             "   FROM  tab_ret_issste                            ",
                             "  WHERE  tipo_retiro    = ?                        "
   PREPARE    p_ObtTRe2          FROM  gc_query
   DECLARE    d_ObtTRe2        CURSOR  FOR p_ObtTRe2

   LET        gc_query    =  " SELECT  nss                    ",
                             "   FROM  ret_sol_issste_tx      ",
                             "  WHERE  consecutivo    = ?     "
   PREPARE    p_BusNSS           FROM  gc_query

   LET        gc_query    =  " SELECT  a.telefono,                  ",
                             "         b.paren_desc                 ",
                             "   FROM  ret_solicitante a,           ",
                             "         tab_parentesco  b            ",
                             "  WHERE  a.paren_cod   = b.paren_cod  ",
                             "    AND  a.consecutivo = ?            "
   PREPARE    p_BusTel           FROM  gc_query

   LET        gc_query    =  " SELECT  *                            ",
                             "   FROM  ret_solicitante              ",
                             "  WHERE  consecutivo   = ?            "
   PREPARE    p_SelRetS          FROM  gc_query

END FUNCTION


### Prepare de busqueda
FUNCTION f_preparad()
    deFINE  lc_query            CHAR(1500)

    --- Detalle Retiros con busqueda
    LET     lc_query    = "  SELECT a.curp,                       ",
                          "         a.consecutivo,                ",
                          "         a.sec_pension,                ",
                          "         a.fecha_resolucion,           ",
                          "         a.fecha_ini_pen,              ",
                          "         a.regimen,                    ",
                          "         a.tipo_seguro,                ",
                          "         a.tipo_pension,               ",
                          "         a.tipo_prestacion,            ",
                          "         a.tipo_retiro                 ",
                          "    FROM ret_sol_issste_tx a,          ",
                          "   OUTER cta_ctr_reg_ind   b           ",
                          "   WHERE a.curp    = b.curp            "
    LET     gc_query      =    lc_query CLIPPED, gc_query1 CLIPPED,
                              ' ORDER BY consecutivo DESC '

    PREPARE p_SelRet           FROM gc_query
    DECLARE d_SelRet      CURSOR FOR  p_SelRet

    --- Detalle Retiros Parciales con busqueda
    LET     lc_query    = "  SELECT a.curp,                       ",
                          "         a.consecutivo,                ",
                          "         a.sec_pension,                ",
                          "         ' ',                          ",
                          "         ' ',                          ",
                          "         a.regimen,                    ",
                          "         a.tipo_seguro,                ",
                          "         ' ',                          ",
                          "         0,                            ",
                          "         'F'                           ",
                          "    FROM ret_parcial_issste  a,        ",
                          "   OUTER cta_ctr_reg_ind   b           ",
                          "   WHERE a.curp     = b.curp           "
    LET     gc_query      =    lc_query CLIPPED, gc_query1 CLIPPED,
                              ' ORDER BY consecutivo DESC '

    PREPARE p_SelRetP          FROM gc_query
    DECLARE d_SelRetP     CURSOR FOR  p_SelRetP

    #CPL-1844
    LET lc_query = " SELECT constancia                 ",
                   " FROM   ret_sol_issste_constancia ",
                   " WHERE  nss         = ?           ",
                   " AND    consecutivo = ?           "
    PREPARE get_constancia FROM lc_query
END FUNCTION


### Consulta
FUNCTION f_consulta()
   DEFINE  lr_retiro      RECORD LIKE  ret_sol_issste_tx.*

   DEFINE  lar_ret        ARRAY [1500] OF RECORD
           curp                     LIKE ret_sol_issste_tx.curp,
           consecutivo              LIKE ret_sol_issste_tx.consecutivo,
           sec_pension              LIKE ret_sol_issste_tx.sec_pension,
           fecha_resolucion         LIKE ret_sol_issste_tx.fecha_resolucion,
           fecha_ini_pen            LIKE ret_sol_issste_tx.fecha_ini_pen,
           regimen                  LIKE ret_sol_issste_tx.regimen,
           tipo_seguro              LIKE ret_sol_issste_tx.tipo_seguro,
           tipo_pension             LIKE ret_sol_issste_tx.tipo_pension,
           tipo_prestacion          LIKE ret_sol_issste_tx.tipo_prestacion,
           tipo_retiro              LIKE ret_sol_issste_tx.tipo_retiro
   END RECORD
   DEFINE  li_i                     SMALLINT
   DEFINE  li_j                     SMALLINT
   DEFINE  li_consecutivo           LIKE ret_sol_issste_tx.consecutivo
   DEFINE  lc_truena                SMALLINT

   CALL f_preparac()
   CALL f_busqueda()    RETURNING  lc_truena
   IF   lc_truena = 0  THEN
        CALL f_preparad()
        LET       li_i        =      1
        FOREACH d_SelRet      INTO   lar_ret[li_i].*
            LET   li_i   = li_i   + 1
             IF   li_i   > 1499   THEN
                  Display " Consulta limitada a 1500 movimientos "
                  EXIT FOREACH
             END IF
        END FOREACH
        FOREACH d_SelRetP     INTO   lar_ret[li_i].*
            LET   li_i   = li_i   + 1
             IF   li_i   > 1499   THEN
                  Display " Consulta limitada a 1500 movimientos "
                  EXIT FOREACH
             END IF
        END FOREACH

        CALL SET_COUNT(li_i - 1)
        OPEN WINDOW RETM9222      AT 02,02 WITH FORM "RETM9222"                                            ATTRIBUTE(BORDER)
        DISPLAY " [Ctrl-M] Detalle  [Ctrl-F] Beneficiarios  [Ctrl-C] Salir                     " AT 01, 03 ATTRIBUTE(REVERSE)
        DISPLAY "                              Sec   Fecha    Fecha Inic     Tip Tip Tip Tipo  " AT 03, 01 ATTRIBUTE(REVERSE)
        DISPLAY "        C U R P       Consecu Pen Resolución    Pensión Reg Seg Pen Pre Reti  " AT 04, 01 ATTRIBUTE(REVERSE)
        DISPLAY "_____________________________________________________________________________ " AT 05, 01 ATTRIBUTE(NORMAL)

        INPUT ARRAY lar_ret WITHOUT DEFAULTS FROM l_reg1.*

             BEFORE FIELD consecutivo
                 LET li_j      = ARR_CURR()
                 LET li_consecutivo   = lar_ret[li_j].consecutivo

             AFTER FIELD consecutivo
                 LET li_j      = ARR_CURR()
                 LET li_consecutivo   = lar_ret[li_j].consecutivo

             ON KEY (ESC)
                 LET  li_j      = ARR_CURR()
                 LET  li_consecutivo  = lar_ret[li_j].consecutivo
                 EXIT INPUT

             ON KEY (control-m)
                 LET  li_j      = ARR_CURR()
                 LET  li_consecutivo  = lar_ret[li_j].consecutivo
                 IF   gc_opc = 'C'
                  OR  gc_opc = 'M'
                  OR  gc_opc = 'E'  THEN
                      IF   lar_ret[li_j].tipo_retiro    = 'F'   THEN
                           CALL f_detalle1(li_consecutivo)
                      ELSE
                           CALL f_detalle(li_consecutivo)
                      END IF
                 END IF
                 EXIT INPUT

             ON KEY (control-f)
                 LET  li_j      = ARR_CURR()
                 LET  li_consecutivo  = lar_ret[li_j].consecutivo
                 CALL f_beneficiarios(li_consecutivo)

             ON KEY (INTERRUPT)
                 LET  li_consecutivo = 0
                 EXIT INPUT

             ON KEY (control-c)
                 LET  li_consecutivo = 0
                 EXIT INPUT

        END INPUT
        CLOSE WINDOW RETM9222
    END IF
    RETURN li_consecutivo

END FUNCTION


### Busqueda
FUNCTION f_busqueda()
    DEFINE    nss          CHAR(11)
    DEFINE    nss_issste   CHAR(11)
    DEFINE    curp         CHAR(18)
    DEFINE    paterno_afore          SMALLINT
    DEFINE    materno_afore          DATE
    DEFINE    nombre_afore           SMALLINT
    DEFINE    lc_truena              SMALLINT

    OPEN WINDOW RETM9221      AT 02,02 WITH FORM "RETM9221"                                            ATTRIBUTE(BORDER)
    DISPLAY " [Esc] Terminar [Ctrl-C] Cancelar                                             " AT 01, 03 ATTRIBUTE(REVERSE)
    DISPLAY " ESTABLEZCA EL CRITERIO DE BUSQUEDA                                           " AT 05, 01 ATTRIBUTE(REVERSE)

    LET   lc_truena = 0
    WHILE TRUE
        LET       nss            = ""
        LET       nss_issste     = ""
        LET       curp           = ""
        LET       paterno_afore            = ""
        LET       materno_afore            = ""
        LET       nombre_afore   = ""
        LET       gc_query1         = ""

        INPUT BY NAME nss, nss_issste, curp, paterno_afore, materno_afore, nombre_afore
                 WITHOUT DEFAULTS
           AFTER FIELD nss
                 IF        nss    <> " "   THEN
                     LET       gc_query1  =  " AND a.nss                =      '", nss CLIPPED, "'"
                 END IF

           AFTER FIELD nss_issste
                 IF        nss_issste    <> " "   THEN
                     LET       gc_query1  =  " AND b.nss_issste         =      '", nss_issste CLIPPED, "'"
                 END IF

           AFTER FIELD curp
                 IF        curp   <> " "   THEN
                     LET       gc_query1  =  " AND a.curp               =      '", curp CLIPPED, "'"
                 END IF

           AFTER FIELD paterno_afore
                 IF        paterno_afore    <> " "   THEN
                     LET       gc_query1  =  " AND a.paterno_afore      =      '", paterno_afore CLIPPED, "'"
                 END IF

           AFTER FIELD materno_afore
                 IF        materno_afore    <> " "   THEN
                     LET       gc_query1  =  " AND a.materno_afore      =      '", materno_afore CLIPPED, "'"
                 END IF

           AFTER FIELD nombre_afore
                 IF        nombre_afore    <> " "   THEN
                     LET       gc_query1  =  " AND a.nombre_afore       =      '", nombre_afore CLIPPED, "'"
                 END IF

           ON KEY(ESC)
                 EXIT INPUT

           ON KEY(INTERRUPT)
                 LET       gc_query1  = ""
                 LET       lc_truena  = 1
                 EXIT INPUT
        END INPUT

       IF   lc_truena    = 0  THEN
            IF   gc_query1    IS NULL
             OR  gc_query1    =  " "  THEN
                 ERROR "Es necesario definir un criterio ...... "
                 SLEEP 3
                 ERROR " "
            ELSE
                 EXIT WHILE
            END IF
       ELSE
            EXIT WHILE
       END IF
   END WHILE
   CLOSE WINDOW RETM9221
   RETURN   lc_truena
END FUNCTION


### Consulta detalle
FUNCTION f_detalle(li_consecutivo)
  DEFINE     li_consecutivo                    INTEGER
  DEFINE     lr_retiro                         RECORD LIKE ret_sol_issste_tx.*
  DEFINE     li_lei                            SMALLINT
  DEFINE     ls_encon                          SMALLINT
  DEFINE     lc_tipo_inversion                 CHAR(100)
  DEFINE     lc_tipo_trabajador                CHAR(100)
  DEFINE     lc_tipo_solicitud                 CHAR(100)
  DEFINE     lc_nss_issste                     CHAR(11)
  DEFINE     lc_curp                           CHAR(18)
  DEFINE     lc_rfc                            CHAR(13)
  DEFINE     ra_mod                            ARRAY[005] OF RECORD
      codigo                                   CHAR(01),
      descripcion                              CHAR(50)
  END RECORD
  DEFINE     ra_tip                            ARRAY[020] OF RECORD
      tipo                                     CHAR(03),
      descripcion                              CHAR(50)
  END RECORD
  DEFINE     ra_par                            ARRAY[002] OF RECORD
      descripcion                              CHAR(50)
  END RECORD
  DEFINE     ls_regresa                        SMALLINT
  DEFINE     lc_Paren                          CHAR(50)
  DEFINE     lc_Telef                          CHAR(20)
  DEFINE     folio                             LIKE  ret_sol_issste_tx.folio_solicitud
  DEFINE     ls_folio                          SMALLINT
  DEFINE     lc_estad_desc                     CHAR(100)
  DEFINE     lc_deleg_desc                     CHAR(100)
  DEFINE     v_ejecuta                         CHAR(150)

  DEFINE     lc_regimen_desc                   CHAR(100)
  DEFINE     lc_seguro_desc                    CHAR(100)
  DEFINE     lc_tipopension_desc               CHAR(100)
  DEFINE     lc_cvepension_desc                CHAR(100)
  DEFINE     lc_prestacion_desc                CHAR(100)

  CALL   f_preparac()
  OPEN   WINDOW RETM9202 AT 2,2 WITH FORM "RETM9202"                                                  ATTRIBUTE(BORDER)
  DISPLAY "[Ctrl-M] Continuar  [Ctrl-C] Cancelar                                           " AT 01,1 ATTRIBUTE(REVERSE)
  DISPLAY "DATOS GENERALES DEL AFILIADO                                                    " AT 03,1 ATTRIBUTE(REVERSE)
  DISPLAY "DATOS GENERALES DE AFILIACION                                                   " AT 09,1 ATTRIBUTE(REVERSE)
  DISPLAY "DATOS GENERALES DE LA SOLICITUD DE RETIRO                                       " AT 13,1 ATTRIBUTE(REVERSE)

  FOREACH    d_SelRet1                         USING  li_consecutivo
                                               INTO   lr_retiro.*
      LET    li_lei                         =  li_lei + 1
  END FOREACH
  LET       ls_encon                        =  0
  FOREACH   d_SelAfi1      USING  lr_retiro.curp
                            INTO  gr_afi_mae_afiliado.*
      LET    ls_encon                       =  ls_encon  + 1
  END FOREACH
  IF  ls_encon                              =  0   THEN
  ELSE
      LET  lc_rfc                           =  gr_afi_mae_afiliado.n_rfc
  END IF
  CALL f_ObTInver(lr_retiro.curp)   RETURNING  lc_tipo_inversion,
                                               lc_nss_issste
  LET  lc_tipo_trabajador                   =  f_ObTTraba(lr_retiro.curp)
  LET  lc_tipo_solicitud                    =  f_ObTSolic(gr_afi_mae_afiliado.tipo_solicitud)
  LET  lc_curp                              =  lr_retiro.curp
  IF   lr_retiro.tipo_pension     =  'VI'
   OR  lr_retiro.tipo_pension     =  'VO'
   OR  lr_retiro.tipo_pension     =  'OR'
   OR  lr_retiro.tipo_pension     =  'AS'
   OR  lr_retiro.tipo_pension     =  'CU'
   OR  lr_retiro.tipo_pension     =  'CO'    THEN
       LET   gs_Muerto            =  1
  ELSE
       LET   gs_Muerto            =  0
  END IF

  #CPL-1844
  #Al recuperar el dato en la variable global no se requieren más asignaciones
  #de variable
  EXECUTE get_constancia USING lr_retiro.nss        ,
                               lr_retiro.consecutivo
                         INTO  gr_constancia.constancia

  DISPLAY BY NAME
             lc_curp,
             lr_retiro.nss,
             lc_nss_issste,
             lc_rfc,
             lr_retiro.paterno_afore,
             lr_retiro.materno_afore,
             lr_retiro.nombre_afore,
             lr_retiro.fecha_nacimiento,
             lc_tipo_inversion,
             lc_tipo_trabajador,
             lc_tipo_solicitud

 IF    lr_retiro.tipo_retiro = 'A'
   OR  lr_retiro.tipo_retiro = 'B'
   OR  lr_retiro.tipo_retiro = 'C'
   OR  lr_retiro.tipo_retiro = 'D'
   OR  lr_retiro.tipo_retiro = 'E'
   OR  lr_retiro.tipo_retiro = 'I'
   OR  lr_retiro.tipo_retiro = 'K'   THEN
       LET        ra_mod[1].codigo            = "A"
       LET        ra_mod[1].descripcion       = "RETIRO TOTAL"
 ELSE
       LET        ra_mod[1].codigo            = "B"
       LET        ra_mod[1].descripcion       = "RETIRO PARCIAL"
 END IF
 CALL SET_COUNT(1)
 DISPLAY   ra_mod[1].descripcion    TO     modal
 FOREACH   d_ObtTRe2                USING  lr_retiro.tipo_retiro
                                    INTO   ra_tip[1].*
 END FOREACH
 CALL SET_COUNT(1)
 LET     ls_regresa            =  0
 DISPLAY ARRAY ra_tip      TO scr_2.*
   ON KEY ( CONTROL-C,  INTERRUPT)
          LET     ls_regresa   =  0
          EXIT DISPLAY
   ON KEY ( CONTROL-M )
          LET     ls_regresa   =  1
          EXIT DISPLAY
 END DISPLAY
 CLOSE  WINDOW RETM9202

 IF     ls_regresa =  1  THEN     ---- Siguiente detalle
      EXECUTE   p_BusTel               USING lr_retiro.consecutivo
                                       INTO  lc_Telef,
                                             lc_Paren
      LET      ra_par[1].descripcion   =  lc_paren
      CASE
          WHEN  lr_retiro.tipo_retiro   =  'A'
                OPEN   WINDOW RETM9203 AT 2,2 WITH FORM "RETM9203"                                                 ATTRIBUTE(BORDER)
                DISPLAY "[Ctrl-M Continuar [Ctrl-C] Cancelar                                                 " AT 01,1 ATTRIBUTE(REVERSE)
                DISPLAY "DATOS DEL SOLICITANTE                                                           " AT 03,1 ATTRIBUTE(REVERSE)
                DISPLAY "DATOS PARTICULARES DE LA SOLICITUD DE RETIRO                                    " AT 07,1 ATTRIBUTE(REVERSE)
                DISPLAY "DATOS DE CONTROL INTERNO                                                        " AT 18,1 ATTRIBUTE(REVERSE)

                IF      gs_afores         = 578   THEN
                     LET     folio  = lr_retiro.folio_solicitud
                     CALL    f_VerCAV(lr_retiro.folio_solicitud)   RETURNING  ls_folio
                ELSE
                     LET     folio  = lr_retiro.folio_solicitud
                END IF
                LET     lc_estad_desc  = f_VerEstad(lr_retiro.estad_cod)
                LET     lc_deleg_desc  = f_VerDeleg(lr_retiro.estad_cod, lr_retiro.deleg_cod)

                LET     lc_regimen_desc     = f_VerRegimen(    lr_retiro.regimen)
                LET     lc_seguro_desc      = f_VerSeguro(     lr_retiro.tipo_seguro)
                LET     lc_tipopension_desc = f_VertipoPension(lr_retiro.tipo_pension)
                LET     lc_cvepension_desc  = f_VerCvePension( lr_retiro.cve_pension)
                LET     lc_prestacion_desc  = f_VerPrestacion( lr_retiro.tipo_prestacion)

                DISPLAY lr_retiro.regimen                                    TO  regimen
                DISPLAY lc_regimen_desc                            TO  regimen_desc
                DISPLAY lr_retiro.tipo_seguro                                TO  tipo_seguro
                DISPLAY lc_seguro_desc                             TO  seguro_desc
                DISPLAY lr_retiro.tipo_pension                               TO  tipo_pension
                DISPLAY lc_tipopension_desc                        TO  tipo_pension_desc
                DISPLAY lr_retiro.cve_pension                                TO  cve_pension
                DISPLAY lc_cvepension_desc                         TO  cve_pension_desc
                DISPLAY lr_retiro.tipo_prestacion                            TO  tipo_prestacion
                DISPLAY lc_prestacion_desc                         TO  tipo_prestacion_desc

                DISPLAY BY NAME
                                 lr_retiro.paterno_afore,
                                 lr_retiro.materno_afore,
                                 lr_retiro.nombre_afore,
                                 lc_Paren,
                                 lc_Telef,

                                 lr_retiro.sec_pension,
                                 lr_retiro.semanas_cotizadas,

                                 lr_retiro.consecutivo,
                                 lr_retiro.usuario_captura,
                                 lr_retiro.fecha_captura,
                                 lr_retiro.fecha_solicitud,
                                 folio                    ,
                                 gr_constancia.constancia #CPL-1844 Ret A

                --- PEIS DISPLAY gc_num_cuenta                        TO  num_cuenta
                DISPLAY lr_retiro.folio_solicitud            TO folio

                DISPLAY  ARRAY ra_par      TO  scr_1.*
                   ON KEY ( CONTROL-C,  INTERRUPT)
                         LET     ls_regresa   =  0
                         EXIT DISPLAY
                   ON KEY ( CONTROL-M )
                         IF      gc_opc    = 'M'   THEN
                              CALL    f_ModifDaSol(lr_retiro.*)
                              LET     ls_regresa   =  1
                              EXIT DISPLAY
                         ELSE
                            IF   gc_opc    = 'E'   THEN
                              CALL    f_ElimDaSol(lr_retiro.*)
                              LET     ls_regresa   =  1
                              EXIT DISPLAY
                            ELSE
                              LET     ls_regresa   =  1
                              EXIT DISPLAY
                            END IF
                         END IF
                END DISPLAY
                CLOSE  WINDOW RETM9203

          WHEN  (lr_retiro.tipo_retiro   =  'B') OR (lr_retiro.tipo_retiro   =  'I')
                OPEN   WINDOW RETM9203 AT 2,2 WITH FORM "RETM9203"                                                 ATTRIBUTE(BORDER)
                DISPLAY "[Ctrl-M Continuar [Ctrl-C] Cancelar                                                " AT 01,1 ATTRIBUTE(REVERSE)
                DISPLAY "DATOS DEL SOLICITANTE                                                           " AT 03,1 ATTRIBUTE(REVERSE)
                DISPLAY "DATOS PARTICULARES DE LA SOLICITUD DE RETIRO                                    " AT 07,1 ATTRIBUTE(REVERSE)
                DISPLAY "DATOS DE CONTROL INTERNO                                                        " AT 18,1 ATTRIBUTE(REVERSE)

                IF      gs_afores         = 578   THEN
                     LET     folio  = lr_retiro.folio_solicitud
                     CALL    f_VerCAV(lr_retiro.folio_solicitud)   RETURNING  ls_folio
                ELSE
                     LET     folio  = lr_retiro.folio_solicitud
                END IF


                LET     lc_estad_desc  = f_VerEstad(lr_retiro.estad_cod)
                LET     lc_deleg_desc  = f_VerDeleg(lr_retiro.estad_cod, lr_retiro.deleg_cod)

                LET     lc_regimen_desc     = f_VerRegimen(    lr_retiro.regimen)
                LET     lc_seguro_desc      = f_VerSeguro(     lr_retiro.tipo_seguro)
                LET     lc_tipopension_desc = f_VertipoPension(lr_retiro.tipo_pension)
                LET     lc_cvepension_desc  = f_VerCvePension( lr_retiro.cve_pension)
                LET     lc_prestacion_desc  = f_VerPrestacion( lr_retiro.tipo_prestacion)

                DISPLAY lr_retiro.regimen                                    TO  regimen
                DISPLAY lc_regimen_desc                            TO  regimen_desc
                DISPLAY lr_retiro.tipo_seguro                                TO  tipo_seguro
                DISPLAY lc_seguro_desc                             TO  seguro_desc
                DISPLAY lr_retiro.tipo_pension                               TO  tipo_pension
                DISPLAY lc_tipopension_desc                        TO  tipo_pension_desc
                DISPLAY lr_retiro.cve_pension                                TO  cve_pension
                DISPLAY lc_cvepension_desc                         TO  cve_pension_desc
                DISPLAY lr_retiro.tipo_prestacion                            TO  tipo_prestacion
                DISPLAY lc_prestacion_desc                         TO  tipo_prestacion_desc
                DISPLAY BY NAME
                                 lr_retiro.paterno_afore,
                                 lr_retiro.materno_afore,
                                 lr_retiro.nombre_afore,
                                 lc_Paren,
                                 lc_Telef,

                                 lr_retiro.sec_pension,
                                 lr_retiro.semanas_cotizadas,

                                 lr_retiro.consecutivo,
                                 lr_retiro.usuario_captura,
                                 lr_retiro.fecha_captura,
                                 lr_retiro.fecha_solicitud,
                                 folio,
                                 gr_constancia.constancia #CPL-1844 Ret B

                --- PEIS DISPLAY gc_num_cuenta                        TO  num_cuenta
                DISPLAY lr_retiro.folio_solicitud            TO folio

                DISPLAY  ARRAY ra_par      TO  scr_1.*
                   ON KEY ( CONTROL-C,  INTERRUPT)
                         LET     ls_regresa   =  0
                         EXIT DISPLAY
                   ON KEY ( CONTROL-M )
                         IF      gc_opc    = 'M'   THEN
                              CALL    f_ModifDaSol(lr_retiro.*)
                              LET     ls_regresa   =  1
                              EXIT DISPLAY
                         ELSE
                            IF   gc_opc    = 'E'   THEN
                              CALL    f_ElimDaSol(lr_retiro.*)
                              LET     ls_regresa   =  1
                              EXIT DISPLAY
                            ELSE
                              LET     ls_regresa   =  1
                              EXIT DISPLAY
                            END IF
                         END IF
                END DISPLAY
                CLOSE  WINDOW RETM9203

          WHEN  lr_retiro.tipo_retiro   =  'C'
                OPEN   WINDOW RETM9204 AT 2,2 WITH FORM "RETM9204"                                               ATTRIBUTE(BORDER)
                DISPLAY "[Ctrl-M Continuar [Ctrl-C] Cancelar                                                 " AT 01,1 ATTRIBUTE(REVERSE)
                DISPLAY "DATOS DEL SOLICITANTE                                                           " AT 03,1 ATTRIBUTE(REVERSE)
                DISPLAY "DATOS PARTICULARES DE LA SOLICITUD DE RETIRO                                    " AT 07,1 ATTRIBUTE(REVERSE)
                DISPLAY "DATOS DE CONTROL INTERNO                                                        " AT 18,1 ATTRIBUTE(REVERSE)

                IF      gs_afores         = 578   THEN
                     LET     folio  = lr_retiro.folio_solicitud
                     CALL    f_VerCAV(lr_retiro.folio_solicitud)   RETURNING  ls_folio
                ELSE
                     LET     folio  = lr_retiro.folio_solicitud
                END IF

                DISPLAY BY NAME
                                 lr_retiro.paterno_afore,
                                 lr_retiro.materno_afore,
                                 lr_retiro.nombre_afore,
                                 lc_Paren,
                                 lc_telef,

                                 lr_retiro.regimen,
                                 lr_retiro.tipo_seguro,
                                 lr_retiro.tipo_pension,
                                 lr_retiro.cve_pension,
                                 lr_retiro.tipo_prestacion,
                                 lr_retiro.aseguradora,
                                 lr_retiro.actuario,
                                 lr_retiro.num_plan_privado,
                                 lr_retiro.semanas_cotizadas,

                                 lr_retiro.consecutivo,
                                 lr_retiro.usuario_captura,
                                 lr_retiro.fecha_captura,
                                 lr_retiro.fecha_solicitud,
                                 folio,
                                 gr_constancia.constancia #CPL-1844 Ret C

                --- PEIS DISPLAY gc_num_cuenta                        TO  num_cuenta
                DISPLAY lr_retiro.folio_solicitud            TO folio

                DISPLAY  ARRAY ra_par      TO  scr_1.*
                   ON KEY ( CONTROL-C,  INTERRUPT)
                         LET     ls_regresa   =  0
                         EXIT DISPLAY
                   ON KEY ( CONTROL-M )
                         IF      gc_opc    = 'M'   THEN
                              CALL    f_ModifDaSol(lr_retiro.*)
                              LET     ls_regresa   =  1
                              EXIT DISPLAY
                         ELSE
                            IF   gc_opc    = 'E'   THEN
                              CALL    f_ElimDaSol(lr_retiro.*)
                              LET     ls_regresa   =  1
                              EXIT DISPLAY
                            ELSE
                              LET     ls_regresa   =  1
                              EXIT DISPLAY
                            END IF
                         END IF
                END DISPLAY
                CLOSE  WINDOW RETM9204

          WHEN  lr_retiro.tipo_retiro   =  'D'
                OPEN   WINDOW RETM9205 AT 2,2 WITH FORM "RETM9205"                                                 ATTRIBUTE(BORDER)
                DISPLAY "[Ctrl-M Continuar [Ctrl-C] Cancelar                                                 " AT 01,1 ATTRIBUTE(REVERSE)
                DISPLAY "DATOS DEL SOLICITANTE                                                           " AT 03,1 ATTRIBUTE(REVERSE)
                DISPLAY "DATOS PARTICULARES DE LA SOLICITUD DE RETIRO                                    " AT 07,1 ATTRIBUTE(REVERSE)
                DISPLAY "DATOS DE CONTROL INTERNO                                                        " AT 18,1 ATTRIBUTE(REVERSE)

                IF      gs_afores         = 578   THEN
                     LET     folio  = lr_retiro.folio_solicitud
                     CALL    f_VerCAV(lr_retiro.folio_solicitud)   RETURNING  ls_folio
                ELSE
                     LET     folio  = lr_retiro.folio_solicitud
                END IF

                DISPLAY BY NAME
                                lr_retiro.paterno_afore,
                                lr_retiro.materno_afore,
                                lr_retiro.nombre_afore,
                                lc_Paren,
                                lc_telef,

                                lr_retiro.regimen,
                                lr_retiro.tipo_seguro,
                                lr_retiro.tipo_pension,
                                lr_retiro.cve_pension,
                                lr_retiro.tipo_prestacion,
                                lr_retiro.semanas_cotizadas,

                                lr_retiro.consecutivo,
                                lr_retiro.usuario_captura,
                                lr_retiro.fecha_captura,
                                lr_retiro.fecha_solicitud,
                                folio,
                                gr_constancia.constancia #CPL-1844 Ret D

                --- PEIS DISPLAY gc_num_cuenta                        TO  num_cuenta
                DISPLAY lr_retiro.folio_solicitud            TO folio

                DISPLAY  ARRAY ra_par      TO  scr_1.*
                   ON KEY ( CONTROL-C,  INTERRUPT)
                         LET     ls_regresa   =  0
                         EXIT DISPLAY
                   ON KEY ( CONTROL-M )
                         IF      gc_opc    = 'M'   THEN
                              CALL    f_ModifDaSol(lr_retiro.*)
                              LET     ls_regresa   =  1
                              EXIT DISPLAY
                         ELSE
                            IF   gc_opc    = 'E'   THEN
                              CALL    f_ElimDaSol(lr_retiro.*)
                              LET     ls_regresa   =  1
                              EXIT DISPLAY
                            ELSE
                              LET     ls_regresa   =  1
                              EXIT DISPLAY
                            END IF
                         END IF
                END DISPLAY
                CLOSE  WINDOW RETM9205

          WHEN  lr_retiro.tipo_retiro   =  'E'
                OPEN   WINDOW RETM9206 AT 2,2 WITH FORM "RETM9206"                                                 ATTRIBUTE(BORDER)
                DISPLAY "[Ctrl-M Continuar [Ctrl-C] Cancelar                                                 " AT 01,1 ATTRIBUTE(REVERSE)
                DISPLAY "DATOS DEL SOLICITANTE                                                           " AT 03,1 ATTRIBUTE(REVERSE)
                DISPLAY "DATOS PARTICULARES DE LA SOLICITUD DE RETIRO                                    " AT 07,1 ATTRIBUTE(REVERSE)
                DISPLAY "DATOS DE CONTROL INTERNO                                                        " AT 18,1 ATTRIBUTE(REVERSE)

                IF      gs_afores         = 578   THEN
                     LET     folio  = lr_retiro.folio_solicitud
                     CALL    f_VerCAV(lr_retiro.folio_solicitud)   RETURNING  ls_folio
                ELSE
                     LET     folio  = lr_retiro.folio_solicitud
                END IF

                DISPLAY     lr_retiro.cve_doc_probatorio      TO    scr_doc[1].cve_docto
                DISPLAY BY NAME
                                      lr_retiro.paterno_afore,
                                      lr_retiro.materno_afore,
                                      lr_retiro.nombre_afore,
                                      lc_Paren,
                                      lc_telef,

                                      lr_retiro.regimen,
                                      lr_retiro.tipo_seguro,
                                      lr_retiro.tipo_pension,
                                      lr_retiro.cve_pension,
                                      lr_retiro.tipo_prestacion,
                             --       lr_retiro.cve_doc_probatorio,
                                      lr_retiro.fecha_nacimiento,
                                      lr_retiro.semanas_cotizadas,

                                      lr_retiro.consecutivo,
                                      lr_retiro.usuario_captura,
                                      lr_retiro.fecha_captura,
                                      lr_retiro.fecha_solicitud,
                                      folio,
                                      gr_constancia.constancia #CPL-1844 Ret E

                --- PEIS DISPLAY gc_num_cuenta                        TO  num_cuenta
                DISPLAY lr_retiro.folio_solicitud            TO folio

                DISPLAY  ARRAY ra_par      TO  scr_1.*
                   ON KEY ( CONTROL-C,  INTERRUPT)
                         LET     ls_regresa   =  0
                         EXIT DISPLAY
                   ON KEY ( CONTROL-M )
                         IF      gc_opc    = 'M'   THEN
                              CALL    f_ModifDaSol(lr_retiro.*)
                              LET     ls_regresa   =  1
                              EXIT DISPLAY
                         ELSE
                            IF   gc_opc    = 'E'   THEN
                              CALL    f_ElimDaSol(lr_retiro.*)
                              LET     ls_regresa   =  1
                              EXIT DISPLAY
                            ELSE
                              LET     ls_regresa   =  1
                              EXIT DISPLAY
                            END IF
                         END IF
                END DISPLAY
                CLOSE  WINDOW RETM9206

          WHEN  lr_retiro.tipo_retiro   =  'K'
                OPEN   WINDOW RETM9207 AT 2,2 WITH FORM "RETM9207"                                                 ATTRIBUTE(BORDER)
                DISPLAY "[Ctrl-M Continuar [Ctrl-C] Cancelar                                                 " AT 01,1 ATTRIBUTE(REVERSE)
                DISPLAY "DATOS DEL SOLICITANTE                                                           " AT 03,1 ATTRIBUTE(REVERSE)
                DISPLAY "DATOS PARTICULARES DE LA SOLICITUD DE RETIRO                                    " AT 07,1 ATTRIBUTE(REVERSE)
                DISPLAY "DATOS DE CONTROL INTERNO                                                        " AT 18,1 ATTRIBUTE(REVERSE)

                IF      gs_afores         = 578   THEN
                     LET     folio  = lr_retiro.folio_solicitud
                     CALL    f_VerCAV(lr_retiro.folio_solicitud)   RETURNING  ls_folio
                ELSE
                     LET     folio  = lr_retiro.folio_solicitud
                END IF

                DISPLAY BY NAME
                                 lr_retiro.paterno_afore,
                                 lr_retiro.materno_afore,
                                 lr_retiro.nombre_afore,
                                 lc_Paren,
                                 lc_telef,

                                 lr_retiro.regimen,
                                 lr_retiro.tipo_seguro,
                                 lr_retiro.tipo_pension,
                                 lr_retiro.cve_pension,
                                 lr_retiro.tipo_prestacion,
                                 lr_retiro.sec_pension,
                                 lr_retiro.fecha_ini_pen,
                                 lr_retiro.fecha_resolucion,
                                 lr_retiro.periodo_pago,

                                 lr_retiro.consecutivo,
                                 lr_retiro.usuario_captura,
                                 lr_retiro.fecha_captura,
                                 lr_retiro.fecha_solicitud,
                                 folio,
                                 gr_constancia.constancia #CPL-1844 Ret K

                --- PEIS DISPLAY gc_num_cuenta                        TO  num_cuenta
                DISPLAY lr_retiro.folio_solicitud            TO folio

                DISPLAY  ARRAY ra_par      TO  scr_1.*
                   ON KEY ( CONTROL-C,  INTERRUPT)
                         LET     ls_regresa   =  0
                         EXIT DISPLAY
                   ON KEY ( CONTROL-M )
                         IF      gc_opc    = 'M'   THEN
                              CALL    f_ModifDaSol(lr_retiro.*)
                              LET     ls_regresa   =  1
                              EXIT DISPLAY
                         ELSE
                            IF   gc_opc    = 'E'   THEN
                              CALL    f_ElimDaSol(lr_retiro.*)
                              LET     ls_regresa   =  1
                              EXIT DISPLAY
                            ELSE
                              LET     ls_regresa   =  1
                              EXIT DISPLAY
                            END IF
                         END IF
                END DISPLAY
                CLOSE  WINDOW RETM9207

          WHEN  lr_retiro.tipo_retiro   =  'F'
      END CASE

 END IF

END FUNCTION


### LLamada a beneficiarios
FUNCTION f_beneficiarios(li_consecutivo)
  DEFINE    li_consecutivo               INTEGER
  DEFINE    v_ejecuta                    CHAR(300)
  DEFINE    lc_nss                       CHAR(11)

  EXECUTE   p_BusNSS                     USING   li_consecutivo
                                         INTO    lc_nss
  LET   v_ejecuta = "fglgo RETM810 ", lc_nss CLIPPED, " ", li_consecutivo CLIPPED," ",'C'
  RUN   v_ejecuta

END FUNCTION


### detalle de parciales
FUNCTION  f_detalle1(li_consecutivo)
  DEFINE     li_consecutivo                    INTEGER
  DEFINE     lr_retiro2                        RECORD LIKE ret_parcial_issste.*
  DEFINE     lr_retiro3                        RECORD LIKE ret_monto_par_issste.*
  DEFINE     li_lei2                           SMALLINT
  DEFINE     li_lei3                           SMALLINT
  DEFINE     ls_encon                          SMALLINT
  DEFINE     lc_tipo_inversion                 CHAR(100)
  DEFINE     lc_tipo_trabajador                CHAR(100)
  DEFINE     lc_tipo_solicitud                 CHAR(100)
  DEFINE     lc_nss_issste                     CHAR(11)
  DEFINE     lc_curp                           CHAR(18)
  DEFINE     lc_rfc                            CHAR(13)
  DEFINE     ra_mod                            ARRAY[005] OF RECORD
      codigo                                   CHAR(01),
      descripcion                              CHAR(50)
  END RECORD
  DEFINE     ra_tip                            ARRAY[020] OF RECORD
      tipo                                     CHAR(03),
      descripcion                              CHAR(50)
  END RECORD
  DEFINE     ra_par                            ARRAY[002] OF RECORD
      descripcion                              CHAR(50)
  END RECORD
  DEFINE     ls_regresa                        SMALLINT
  DEFINE     lc_Paren                          CHAR(50)
  DEFINE     lc_Telef                          CHAR(20)
  DEFINE     folio                             LIKE  ret_sol_issste_tx.folio_solicitud
  DEFINE     ls_folio                          SMALLINT
  DEFINE     lc_estad_desc                     CHAR(100)
  DEFINE     lc_deleg_desc                     CHAR(100)
  DEFINE     v_ejecuta                         CHAR(150)

  DEFINE     lc_regimen_desc                   CHAR(100)
  DEFINE     lc_seguro_desc                    CHAR(100)
  DEFINE     lc_tipopension_desc               CHAR(100)
  DEFINE     lc_cvepension_desc                CHAR(100)
  DEFINE     lc_prestacion_desc                CHAR(100)
  DEFINE     lc_laf                            CHAR(001)

  CALL   f_preparac()
  OPEN   WINDOW RETM9202 AT 2,2 WITH FORM "RETM9202"                                                  ATTRIBUTE(BORDER)
  DISPLAY "[Ctrl-M] Continuar  [ctrl-T] Beneficiarios  [Ctrl-C] Cancelar                   " AT 01,1 ATTRIBUTE(REVERSE)
  DISPLAY "DATOS GENERALES DEL AFILIADO                                                    " AT 03,1 ATTRIBUTE(REVERSE)
  DISPLAY "DATOS GENERALES DE AFILIACION                                                   " AT 09,1 ATTRIBUTE(REVERSE)
  DISPLAY "DATOS GENERALES DE LA SOLICITUD DE RETIRO                                       " AT 13,1 ATTRIBUTE(REVERSE)

  FOREACH    d_SelRet2                         USING  li_consecutivo
                                               INTO   lr_retiro2.*
      ###LET lr_retiro.deleg_cod = lr_retiro.deleg_cod - (lr_retiro.estad_cod * 1000)
      LET    li_lei2                        =  li_lei2 + 1
  END FOREACH

  FOREACH    d_SelRet3                         USING  li_consecutivo
                                               INTO   lr_retiro3.*
      LET    li_lei3                        =   li_lei3 + 1
  END FOREACH

  LET       ls_encon                        =  0
  FOREACH   d_SelAfi1      USING  lr_retiro2.curp
                            INTO  gr_afi_mae_afiliado.*
      LET    ls_encon                       =   ls_encon  + 1
  END FOREACH
  IF  ls_encon                              =   0   THEN
  ELSE
      LET  lc_rfc                           =   gr_afi_mae_afiliado.n_rfc
  END IF
  CALL f_ObTInver(lr_retiro2.curp)   RETURNING  lc_tipo_inversion,
                                                lc_nss_issste
  LET  lc_tipo_trabajador                   =   f_ObTTraba(lr_retiro2.curp)
  LET  lc_tipo_solicitud                    =   f_ObTSolic(gr_afi_mae_afiliado.tipo_solicitud)
  LET  lc_curp                              =   lr_retiro2.curp
  LET  gs_Muerto                            =   0

  #CPL-1844
  #Al recuperar el dato en la variable global no se requieren más asignaciones
  #de variable
  EXECUTE get_constancia USING lr_retiro2.nss        ,
                               lr_retiro2.consecutivo
                         INTO  gr_constancia.constancia

  DISPLAY    lr_retiro2.apellido_paterno    TO  paterno_afore
  DISPLAY    lr_retiro2.apellido_materno    TO  materno_afore
  DISPLAY    lr_retiro2.nombre              TO  nombre_afore
  DISPLAY BY NAME
             lc_curp,
             lr_retiro2.nss,
             lc_nss_issste,
             lc_rfc,
             lc_tipo_inversion,
             lc_tipo_trabajador,
             lc_tipo_solicitud

 LET         ra_mod[1].codigo            = "B"
 LET         ra_mod[1].descripcion       = "RETIRO PARCIAL"
 CALL SET_COUNT(1)
 DISPLAY     ra_mod[1].descripcion        TO     modal
 LET         lc_laf                       =      'F'
 FOREACH     d_ObtTRe2                USING  lc_laf
                                       INTO   ra_tip[1].*
 END FOREACH
 CALL SET_COUNT(1)
 LET         ls_regresa            =  0
 DISPLAY ARRAY ra_tip      TO scr_2.*
   ON KEY ( CONTROL-C,  INTERRUPT)
          LET     ls_regresa   =  0
          EXIT DISPLAY
   ON KEY ( CONTROL-M )
          LET     ls_regresa   =  1
          EXIT DISPLAY
   ### Incluir consulta a beneficiarios
   ON KEY ( CONTROL-T )
          IF   gc_opc = 'M'    THEN
               LET   v_ejecuta = "fglgo RETM810 ", lr_retiro2.nss CLIPPED,         " ",
                                                   lr_retiro2.consecutivo CLIPPED, " ",
                                                   "M", " ",
                                                   "1", " ",
                                                   gc_num_cuenta
               RUN   v_ejecuta
          END IF
          IF   gc_opc = 'C'    THEN
               LET   v_ejecuta = "fglgo RETM810 ", lr_retiro2.nss CLIPPED,         " ",
                                                   lr_retiro2.consecutivo CLIPPED, " ",
                                                   "C", " ",
                                                   "1", " ",
                                                   gc_num_cuenta
               RUN   v_ejecuta
          END IF
 END DISPLAY
 CLOSE  WINDOW RETM9202

 IF   ls_regresa =  1  THEN            ---- Siguiente detalle
      EXECUTE   p_BusTel               USING lr_retiro2.consecutivo
                                       INTO  lc_Telef,
                                             lc_Paren
      LET      ra_par[1].descripcion   =  lc_paren
      CASE
          WHEN  lr_retiro2.regimen   =  'DT'
                OPEN   WINDOW RETM9209 AT 2,2 WITH FORM "RETM9209"                                                 ATTRIBUTE(BORDER)
                ### DISPLAY "[Cntrl-O] Modificar  [Ctrl-T] Eliminar  [Ctrl-C] Terminar                       " AT 01,1 ATTRIBUTE(REVERSE)
                DISPLAY "[Ctrl-M Continuar [Ctrl-C] Cancelar                                             " AT 01,1 ATTRIBUTE(REVERSE)
                DISPLAY "DATOS DEL SOLICITANTE                                                           " AT 03,1 ATTRIBUTE(REVERSE)
                DISPLAY "DATOS PARTICULARES DE LA SOLICITUD DE RETIRO                                    " AT 07,1 ATTRIBUTE(REVERSE)
                DISPLAY "DATOS DE CONTROL INTERNO                                                        " AT 18,1 ATTRIBUTE(REVERSE)

                IF      gs_afores         = 578   THEN
                     LET     folio  = lr_retiro2.folio_solicitud
                     CALL    f_VerCAV(lr_retiro2.folio_solicitud)   RETURNING  ls_folio
                ELSE
                     LET     folio  = lr_retiro2.folio_solicitud
                END IF
                ####LET     lc_estad_desc  = f_VerEstad(lr_retiro.estad_cod)
                ####LET     lc_deleg_desc  = f_VerDeleg(lr_retiro.estad_cod, lr_retiro.deleg_cod)

                LET     lc_regimen_desc     = f_VerRegimen(    lr_retiro2.regimen)
                LET     lc_seguro_desc      = f_VerSeguro(     lr_retiro2.tipo_seguro)

                DISPLAY lr_retiro2.regimen                                    TO  regimen
                DISPLAY lc_regimen_desc                            TO  regimen_des
                DISPLAY lr_retiro2.tipo_seguro                                TO  tipo_seguro
                DISPLAY lc_seguro_desc                             TO  tipo_seguro_des

                DISPLAY BY NAME
                                 lr_retiro2.apellido_paterno,
                                 lr_retiro2.apellido_materno,
                                 lr_retiro2.nombre,
                                 lc_Paren,
                                 lc_Telef,

                                 lr_retiro2.sec_pension,
                                 lr_retiro2.num_issste,
                                 lr_retiro3.f_ultimo_aporte,
                                 lr_retiro3.mto_ultimo_aporte,
                                 lr_retiro3.mto_18ultimo_aporte,
                                 lr_retiro3.mto_10p_sarissste,
                                 lr_retiro3.mto_a_pagar,

                                 lr_retiro2.consecutivo,
                                 lr_retiro2.usuario_captura,
                                 lr_retiro2.fecha_captura,
                                 lr_retiro2.fecha_solicitud,
                                 folio,
                                 gr_constancia.constancia #CPL-1844 DT

                --- PEIS DISPLAY gc_num_cuenta                        TO  num_cuenta
                ###DISPLAY lr_retiro.estad_cod                  TO  lc_estad_cod
                ###DISPLAY lr_retiro.deleg_cod                  TO  lc_deleg_cod
                ###DISPLAY lc_estad_desc                        TO  lc_estad_desc
                ###DISPLAY lc_deleg_desc                        TO  lc_deleg_desc

                DISPLAY  ARRAY ra_par      TO  scr_1.*
                   ON KEY ( CONTROL-C,  INTERRUPT)
                         LET     ls_regresa   =  0
                         EXIT DISPLAY
                   ON KEY ( CONTROL-M )
                         IF      gc_opc    = 'M'   THEN
                              CALL    f_ModifDaSol1(lr_retiro2.*, lr_retiro3.*)
                              LET     ls_regresa   =  1
                              EXIT DISPLAY
                         ELSE
                            IF   gc_opc    = 'E'   THEN
                              CALL    f_ElimDaSol1(lr_retiro2.*)
                              LET     ls_regresa   =  1
                              EXIT DISPLAY
                            ELSE
                              LET     ls_regresa   =  1
                              EXIT DISPLAY
                            END IF
                         END IF
                END DISPLAY
                CLOSE  WINDOW RETM9209

          WHEN  lr_retiro2.regimen   =  'RO'
                OPEN   WINDOW RETM9210 AT 2,2 WITH FORM "RETM9210"                                                 ATTRIBUTE(BORDER)
                DISPLAY "[Ctrl-M Continuar [Ctrl-C] Cancelar                                                 " AT 01,1 ATTRIBUTE(REVERSE)
                DISPLAY "DATOS DEL SOLICITANTE                                                           " AT 03,1 ATTRIBUTE(REVERSE)
                DISPLAY "DATOS PARTICULARES DE LA SOLICITUD DE RETIRO                                    " AT 07,1 ATTRIBUTE(REVERSE)
                DISPLAY "DATOS DE CONTROL INTERNO                                                        " AT 18,1 ATTRIBUTE(REVERSE)

                IF      gs_afores         = 578   THEN
                     LET     folio  = lr_retiro2.folio_solicitud
                     CALL    f_VerCAV(lr_retiro2.folio_solicitud)   RETURNING  ls_folio
                ELSE
                     LET     folio  = lr_retiro2.folio_solicitud
                END IF
                ####LET     lc_estad_desc  = f_VerEstad(lr_retiro.estad_cod)
                ####LET     lc_deleg_desc  = f_VerDeleg(lr_retiro.estad_cod, lr_retiro.deleg_cod)

                LET     lc_regimen_desc     = f_VerRegimen(    lr_retiro2.regimen)
                LET     lc_seguro_desc      = f_VerSeguro(     lr_retiro2.tipo_seguro)

                DISPLAY lr_retiro2.regimen                                    TO  regimen
                DISPLAY lc_regimen_desc                            TO  regimen_des
                DISPLAY lr_retiro2.tipo_seguro                                TO  tipo_seguro
                DISPLAY lc_seguro_desc                             TO  tipo_seguro_des
                DISPLAY lc_Telef                                   TO  telefono
                DISPLAY lr_retiro3.mto_a_pagar                     TO  ld_TotPag

                DISPLAY BY NAME
                                 lr_retiro2.apellido_paterno,
                                 lr_retiro2.apellido_materno,
                                 lr_retiro2.nombre,
                                 lc_Paren,

                                 lr_retiro2.sec_pension,
                                 lr_retiro2.num_issste,
                                 lr_retiro3.salario_base_cot,
                                 lr_retiro3.mto_75sbc,
                                 lr_retiro3.mto_10p_rcv,

                                 lr_retiro2.consecutivo,
                                 lr_retiro2.usuario_captura,
                                 lr_retiro2.fecha_captura,
                                 lr_retiro2.fecha_solicitud,
                                 folio,
                                 gr_constancia.constancia #CPL-1844 RO

                --- PEIS DISPLAY gc_num_cuenta                        TO  num_cuenta
                ###DISPLAY lr_retiro.estad_cod                  TO  lc_estad_cod
                ###DISPLAY lr_retiro.deleg_cod                  TO  lc_deleg_cod
                ###DISPLAY lc_estad_desc                        TO  lc_estad_desc
                ###DISPLAY lc_deleg_desc                        TO  lc_deleg_desc

                DISPLAY  ARRAY ra_par      TO  scr_1.*
                   ON KEY ( CONTROL-C,  INTERRUPT)
                         LET     ls_regresa   =  0
                         EXIT DISPLAY
                   ON KEY ( CONTROL-M )
                         IF      gc_opc    = 'M'   THEN
                              CALL    f_ModifDaSol1(lr_retiro2.*, lr_retiro3.*)
                              LET     ls_regresa   =  1
                              EXIT DISPLAY
                         ELSE
                            IF   gc_opc    = 'E'   THEN
                              CALL    f_ElimDaSol1(lr_retiro2.*)
                              LET     ls_regresa   =  1
                              EXIT DISPLAY
                            ELSE
                              LET     ls_regresa   =  1
                              EXIT DISPLAY
                            END IF
                         END IF
                END DISPLAY
                CLOSE  WINDOW RETM9210
      END CASE

 END IF

END FUNCTION
