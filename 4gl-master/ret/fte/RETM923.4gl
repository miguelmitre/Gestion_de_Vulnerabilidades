################################################################################
##Proyecto           => SYSTEMA DE AFORES ( MEXICO )                          ##
##Owner              => E.F.P.                                                ##
##Programa RETM920   => Administrar Solicitudes de Retiro Parciales           ##
##Sistema            => SAFRE                                                 ##
##Fecha Creacion     => 27 Oct 2009                                           ##
##By                 => JGHM                                                  ##
##Fecha Modifica     =>                                                       ##
################################################################################
##Fecha Actualiz. => 22/01/22015                                              ##
##Actualizacion   =>Cesar D. Chavéz  Mtz.                                     ##
##                => Se agrega solicitud de constancia en las pantallas de    ##
##                  Agregar, Consulta, Modificacion y Elimina para los retiros##
##                   Totales y parciales CPL-1844                             ##
################################################################################

DATABASE safre_af

GLOBALS "RETM921.4gl"

### Despliega información F (Parciales)
FUNCTION f_DespF()
  DEFINE     ls_encon             SMALLINT
  DEFINE     ls_Cancela           SMALLINT
  DEFINE     lc_Paren             CHAR(100)
  DEFINE     lc_telef             CHAR(50)
  DEFINE     fecha_solicitud      DATE
  DEFINE     folio                LIKE   ret_parcial_issste.folio
  DEFINE     nuevo                CHAR(01)
  DEFINE     ls_DocPro            SMALLINT
  DEFINE     lc_periodo_pago      CHAR(10)
  DEFINE     ld_TotPag            LIKE   ret_monto_par_issste.mto_a_pagar

  CALL f_inicial()
  IF     gr_ret_parcial_issste.regimen     =  'DT'       THEN
       OPEN   WINDOW RETM9209 AT 2,2 WITH FORM "RETM9209"                                                 ATTRIBUTE(BORDER)
  ELSE
       OPEN   WINDOW RETM9210 AT 2,2 WITH FORM "RETM9210"                                                 ATTRIBUTE(BORDER)
  END IF

  DISPLAY "[Esc] Terminar [Ctrl-C] Cancelar                                                " AT 01,1 ATTRIBUTE(REVERSE)
  DISPLAY "DATOS DEL SOLICITANTE                                                           " AT 03,1 ATTRIBUTE(REVERSE)
  DISPLAY "DATOS PARTICULARES DE LA SOLICITUD DE RETIRO                                    " AT 07,1 ATTRIBUTE(REVERSE)
  DISPLAY "DATOS DE CONTROL INTERNO                                                        " AT 18,1 ATTRIBUTE(REVERSE)

  LET       ls_encon             =      0
  LET       ls_Cancela           =      0
  LET       lc_Paren             =      '12-TITULAR '
  LET       folio                =      gr_ret_parcial_issste.folio

  IF     gr_ret_parcial_issste.regimen     =  'DT'       THEN
         LET    ld_TotPag  =  gr_ret_monto_par_issste.mto_a_pagar
         DISPLAY   gr_ret_solicitante.telefono    TO lc_telef
         DISPLAY   ld_TotPag                      TO mto_a_pagar
         DISPLAY BY NAME
            gr_ret_parcial_issste.apellido_paterno,
            gr_ret_parcial_issste.apellido_materno,
            gr_ret_parcial_issste.nombre,
            lc_Paren,

            gr_ret_parcial_issste.sec_pension,
            gr_ret_parcial_issste.regimen,
            gr_ret_parcial_issste.tipo_seguro,
            gr_ret_parcial_issste.num_issste,
            gr_ret_monto_par_issste.f_ultimo_aporte,
            gr_ret_monto_par_issste.mto_ultimo_aporte,
            gr_ret_monto_par_issste.mto_18ultimo_aporte,
            gr_ret_monto_par_issste.mto_10p_sarissste,

            gr_ret_parcial_issste.consecutivo,
            gr_ret_parcial_issste.usuario_captura,
            gr_ret_parcial_issste.fecha_captura,
            gr_ret_parcial_issste.fecha_solicitud,
            gr_ret_parcial_issste.folio
  ELSE
         LET    ld_TotPag  =  gr_ret_monto_par_issste.mto_a_pagar
         DISPLAY BY NAME
            gr_ret_parcial_issste.apellido_paterno,
            gr_ret_parcial_issste.apellido_materno,
            gr_ret_parcial_issste.nombre,
            lc_Paren,
            gr_ret_solicitante.telefono,

            gr_ret_parcial_issste.sec_pension,
            gr_ret_parcial_issste.regimen,
            gr_ret_parcial_issste.tipo_seguro,
            gr_ret_parcial_issste.num_issste,
            gr_ret_monto_par_issste.salario_base_cot,
            gr_ret_monto_par_issste.mto_75sbc,
            gr_ret_monto_par_issste.mto_10p_rcv,
            ld_TotPag,

            gr_ret_parcial_issste.consecutivo,
            gr_ret_parcial_issste.usuario_captura,
            gr_ret_parcial_issste.fecha_captura,
            gr_ret_parcial_issste.fecha_solicitud,
            gr_ret_parcial_issste.folio
  END IF

  ### Input de los datos del F parcial regimen DT

  DISPLAY "[Esc] Terminar [Ctrl-C] Cancelar                                                " AT 01,1 ATTRIBUTE(REVERSE)
  LET   gs_Cance   = 0
  IF    gr_ret_parcial_issste.regimen     =  'DT'       THEN
        CALL  f_InpdelDT(gr_ret_parcial_issste.*, gr_ret_monto_par_issste.*, gr_ret_solicitante.*)
              RETURNING  gr_ret_parcial_issste.*,
                         gr_ret_monto_par_issste.*,
                         gr_ret_solicitante.*

  ELSE
        CALL  f_InpdelRO()
  END IF

  IF    gr_ret_parcial_issste.regimen     =  'DT'       THEN
        CLOSE WINDOW RETM9209
  ELSE
        CLOSE WINDOW RETM9210
  END IF
  IF    gs_Grabe   = 0
   AND  gs_Cance   = 0     THEN
        CALL f_Grabar2()
  END IF

END FUNCTION


#---------------------------------------------------------------------------#
# f_InpdelDT : Valida y captura los datos para el retiro parcial ISSSTE     #
#              con regimen DT                                               #
#---------------------------------------------------------------------------#
FUNCTION f_InpdelDT(lr_ret_parcial_issste, lr_ret_monto_par_issste, lr_ret_solicitante)

  DEFINE     lr_ret_parcial_issste    RECORD LIKE ret_parcial_issste.*
  DEFINE     lr_ret_monto_par_issste  RECORD LIKE ret_monto_par_issste.*
  DEFINE     lr_ret_solicitante       RECORD LIKE ret_solicitante.*
  DEFINE     ls_subcuenta             LIKE   dis_cuenta.subcuenta
  DEFINE     ls_tipo_movimiento       LIKE   dis_cuenta.tipo_movimiento
  DEFINE     ls_folio                 SMALLINT
  DEFINE     ld_Fecha                 DATE
  DEFINE     li_Monto                 DECIMAL(16,6)
  DEFINE     li_Monto18               DECIMAL(16,6)
  DEFINE     ld_SaldoSar              DECIMAL(16,6)
  DEFINE     lc_Telef                 CHAR(30)

    -- -----------------------------------------------------------------------------

    LET gr_ret_parcial_issste.*     = lr_ret_parcial_issste.*
    LET gr_ret_monto_par_issste.*   = lr_ret_monto_par_issste.*
    LET gr_ret_solicitante.*        = lr_ret_solicitante.*

    IF gs_afores = 578 THEN
        LET ls_subcuenta    =   19
    ELSE
        LET ls_subcuenta    =   13
    END IF

    LET ls_tipo_movimiento = 1

    CALL  f_BuscaUltApo(gr_ret_parcial_issste.nss       ,
                        ls_subcuenta                    ,
                        ls_tipo_movimiento              ,
                        gr_DatMarPar.salario_promedio
                       )
    RETURNING ld_Fecha, li_Monto

    CALL f_Buscael10(gr_ret_parcial_issste.nss, ls_subcuenta, ls_tipo_movimiento)
        RETURNING ld_SaldoSar

    LET gr_ret_monto_par_issste.f_ultimo_aporte         = ld_Fecha
    LET gr_ret_monto_par_issste.mto_ultimo_aporte       = li_Monto
    LET gr_ret_monto_par_issste.mto_18ultimo_aporte     = li_Monto * 18
    LET gr_ret_monto_par_issste.mto_10p_sarissste       = ld_SaldoSar * 0.10

    IF   ld_SaldoSar < gr_ret_monto_par_issste.mto_18ultimo_aporte THEN
        CALL f_DespMen("Saldo subcuenta SARISSSTE menor a último aporte SARISSSTE x 18, según normativa")
        LET gr_ret_monto_par_issste.mto_a_pagar = 0
        LET gs_Cance                            = 1
    ELSE
        LET gr_ret_monto_par_issste.mto_a_pagar = gr_ret_monto_par_issste.mto_10p_sarissste
        LET lc_Telef                            = "0"
        LET gs_Cance                            = 0
    END IF

    DISPLAY gr_ret_parcial_issste.sec_pension           TO sec_pension
    DISPLAY gr_ret_parcial_issste.regimen               TO regimen
    DISPLAY gr_ret_parcial_issste.tipo_seguro           TO tipo_seguro
    DISPLAY gr_ret_parcial_issste.num_issste            TO num_issste
    DISPLAY gr_ret_monto_par_issste.f_ultimo_aporte     TO f_ultimo_aporte
    DISPLAY gr_ret_monto_par_issste.mto_ultimo_aporte   TO mto_ultimo_aporte
    DISPLAY gr_ret_monto_par_issste.mto_18ultimo_aporte TO mto_18ultimo_aporte
    DISPLAY gr_ret_monto_par_issste.mto_10p_sarissste   TO mto_10p_sarissste
    DISPLAY gr_ret_monto_par_issste.mto_a_pagar         TO mto_a_pagar

    IF gs_Cance = 0 THEN
        CALL f_InputComun1(gr_ret_parcial_issste.*, gr_ret_monto_par_issste.*, gr_ret_solicitante.*)
            RETURNING gr_ret_parcial_issste.*   ,
                      gr_ret_monto_par_issste.* ,
                      gr_ret_solicitante.*
    END IF

    RETURN gr_ret_parcial_issste.*      ,
           gr_ret_monto_par_issste.*    ,
           gr_ret_solicitante.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_BuscaUltApo : Busca la ultima aportacion en la subcuenta de SAR.        #
#                 En caso de venir de un traspaso, realiza el calculo de    #
#                 la aportacion (COPPEL - CPL-1435)                         #
#---------------------------------------------------------------------------#
FUNCTION  f_BuscaUltApo(pr_solicitud)

    DEFINE pr_solicitud RECORD
        nss                 LIKE ret_parcial_issste.nss                     ,
        subcuenta           LIKE dis_cuenta.subcuenta                       ,
        tipo_movimiento     LIKE dis_cuenta.tipo_movimiento                 ,
        salario_promedio    LIKE ret_datamart_par_issste.salario_promedio
    END RECORD

    DEFINE ld_aportacion    LIKE dis_cuenta.monto_en_pesos
    DEFINE ls_conse         LIKE dis_cuenta.consecutivo_lote

    DEFINE
        ldt_fecha_conv          DATE

    DEFINE
        li_encuentra            INTEGER

    -- -----------------------------------------------------------------------------

    -- Verifica si el NSS proviene de un traspaso (CPL-1435)
    SELECT "OK"
    FROM   taa_rcv_recepcion
    WHERE  nss             = pr_solicitud.nss
    AND    ident_operacion = "09"
    GROUP BY 1

    IF STATUS <> NOTFOUND THEN
        LET ldt_fecha_conv  = gd_today
        LET ld_aportacion   = ( (pr_solicitud.salario_promedio/30) * 60) * 0.02
    ELSE
        LET gc_query    = "  SELECT  fecha_conversion, consecutivo_lote, sum(monto_en_pesos)  ",
                          "    FROM  dis_cuenta                                               ",
                          "   WHERE  nss             = ?                                      ",
                          "     AND  subcuenta       = ?                                      ",
                          "     AND  tipo_movimiento = ?                                      ",
                          "   GROUP  BY 1, 2                                                  ",
                          "   ORDER  BY 1, 2                                                  "

        PREPARE p_SelCta FROM gc_query
        DECLARE d_SelCta CURSOR FOR p_SelCta

        FOREACH d_SelCta USING pr_solicitud.nss             ,
                               pr_solicitud.subcuenta       ,
                               pr_solicitud.tipo_movimiento
                         INTO  ldt_fecha_conv   ,
                               ls_conse         ,
                               ld_aportacion

            LET li_encuentra    = li_encuentra + 1

        END FOREACH

        IF STATUS = NOTFOUND THEN
            LET ldt_fecha_conv  = ""
            LET ld_aportacion   = 0
        END IF
    END IF -- Proviene de un traspaso

    RETURN ldt_fecha_conv   ,
           ld_aportacion

END FUNCTION


### Busca saldo SAR para el 10
FUNCTION  f_Buscael10( lc_nss, ls_subcuenta, ls_tipo_movimiento)
  DEFINE        lc_nss                      LIKE  ret_parcial_issste.nss
  DEFINE        ls_subcuenta                LIKE  dis_cuenta.subcuenta
  DEFINE        ls_tipo_movimiento          LIKE  dis_cuenta.tipo_movimiento
  DEFINE        li_Monto                    DECIMAL(16,6)
  DEFINE        li_Monto_Sal                DECIMAL(16,6)
  DEFINE        li_Pesos                    DECIMAL(16,6)
  DEFINE        ld_fecha                    DATE

  LET           ld_fecha    = TODAY


  WHENEVER ERROR CONTINUE
  DROP TABLE  tmp_saldo
  WHENEVER ERROR STOP

  LET     gc_query     =  " SELECT  nss,                                ",
                          "         subcuenta,                          ",
                          "         siefore  ,                          ",
                          "         fecha_conversion,                   ",
                          "         SUM(monto_en_acciones) acciones,    ",
                          "         SUM(monto_en_pesos   ) pesos        ",
                          "   FROM  dis_cuenta                          ",
                          "  WHERE  nss             =  ?                ",
                          "    AND  subcuenta       =  ?                ",
                          "    AND  tipo_movimiento >  0                ",
                          "  GROUP  BY 1, 2, 3, 4                       ",
                          "   INTO  TEMP tmp_saldo                      "
  PREPARE    p_CreSal         FROM  gc_query

  EXECUTE    p_CreSal          USING   lc_nss,
                                       ls_subcuenta

  LET     gc_query     =  " SELECT  siefore,                            ",
                          "         acciones * precio_del_dia,          ",
                          "         pesos                               ",
                          "   FROM  tmp_saldo,                          ",
                          "         glo_valor_accion                    ",
                          "  WHERE  codigo_siefore    =   siefore       ",
                          "    AND  fecha_valuacion   =   ?             ",
                          "    AND  siefore           <>  6             ",
                          "    AND  fecha_conversion  <=  ?             "
  PREPARE    p_VerSal         FROM  gc_query
  DECLARE    d_VerSal       CURSOR FOR  p_VerSal

  LET        li_Monto      =   0
  LET        li_Monto_Sal  =   0
  FOREACH    d_VerSal          USING   ld_fecha,
                                       ld_fecha
                                INTO   gi_siefore,
                                       li_Monto_Sal,
                                       li_Pesos
      IF     ls_subcuenta  = 19    THEN
             LET    li_Monto      =   li_Monto   +   li_Pesos
      ELSE
             LET    li_Monto      =   li_Monto   +   li_Monto_Sal
      END IF
  END FOREACH

  IF  STATUS = NOTFOUND   THEN
      LET      li_Monto    = 0
  END IF

  RETURN  li_Monto
END FUNCTION


### Input de datos de parcial = normal pero otras tablas
FUNCTION f_InputComun1(gr_ret_parcial_issste, gr_ret_monto_par_issste, gr_ret_solicitante)
  DEFINE     gr_ret_parcial_issste    RECORD LIKE ret_parcial_issste.*
  DEFINE     gr_ret_monto_par_issste  RECORD LIKE ret_monto_par_issste.*
  DEFINE     gr_ret_solicitante       RECORD LIKE ret_solicitante.*
  DEFINE     fecha_solicitud          DATE
  DEFINE     folio                    LIKE   ret_parcial_issste.folio_solicitud
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
  DEFINE     num_cuenta               LIKE   ret_beneficiario.num_cuenta
  DEFINE     lc_estad_cod             LIKE   ret_sol_issste_tx.estad_cod
  DEFINE     lc_deleg_cod             LIKE   ret_sol_issste_tx.deleg_cod
  DEFINE     lc_estad_desc            CHAR(100)
  DEFINE     lc_deleg_desc            CHAR(100)

  LET   paterno_afore                 =    ""
  LET   materno_afore                 =    ""
  LET   nombre_afore                  =    ""
  LET   lc_Paren                      =    ""
  LET   lc_Telef                      =    "0"
  LET   gs_Muerto                     =    1
  LET   gs_Cance                      =    0

  LET     gr_ret_solicitante.apellido_paterno  =   gr_ret_parcial_issste.apellido_paterno
  LET     gr_ret_solicitante.apellido_materno  =   gr_ret_parcial_issste.apellido_materno
  LET     gr_ret_solicitante.nombres           =   gr_ret_parcial_issste.nombre
  LET     gr_ret_solicitante.paren_cod         =   12
  ###--SELECT  paren_desc
  ###--  INTO  ls_paren_cod
  ###--  FROM  tab_parentesco
  ###-- WHERE  paren_desc                           =   la_benef[li_x1].paren_desc
  ###--LET     gr_ret_solicitante.paren_cod         =   ls_paren_cod

  DISPLAY "[Esc] Grabar     [Ctrl-C] Cancelar                                               " AT 01,1 ATTRIBUTE(REVERSE)
  WHILE TRUE
     ###LET    lc_estad_cod                 =   gr_ret_parcial_issste.estad_cod
     ###LET    lc_deleg_cod                 =   gr_ret_parcial_issste.deleg_cod
     ###LET    lc_estad_desc                =   f_VerEstad(lc_estad_cod)
     ###LET    lc_deleg_desc                =   f_VerDeleg(lc_estad_cod, lc_deleg_cod)
     LET    num_cuenta                   =   gc_num_cuenta
     LET    folio                        =   gr_ret_parcial_issste.folio_solicitud
     DISPLAY     folio                   TO  folio
     DISPLAY     fecha_solicitud         TO  fecha_solicitud
     DISPLAY     num_cuenta              TO  num_cuenta
     IF          lc_Telef                IS  NULL
      OR         lc_Telef                =   " "   THEN
       LET       lc_Telef                =   0
     ELSE
       LET       lc_Telef                =   gr_ret_solicitante.telefono
     END IF
     DISPLAY     lc_Telef                TO  lc_Telef
     IF   fecha_solicitud                IS  NULL
      OR  fecha_solicitud                =   " "  THEN
          LET  fecha_solicitud           =   TODAY
     ELSE
          LET  fecha_solicitud           =   gr_ret_parcial_issste.fecha_solicitud
     END IF
     DISPLAY   fecha_solicitud           TO  fecha_solicitud
     LET    folio                     =   gr_ret_parcial_issste.folio_solicitud
     ###DISPLAY     lc_estad_cod            TO  lc_estad_cod
     ###DISPLAY     lc_deleg_cod            TO  lc_deleg_cod

     #CPL-1844
     #Agregar captura de constancia
     INPUT BY NAME    lc_Telef       ,
     ###                 lc_estad_cod,
     ###                 lc_deleg_cod,
                      folio          ,
                      fecha_solicitud,
                      num_cuenta     ,
                      gr_constancia.constancia
                      WITHOUT DEFAULTS

        BEFORE FIELD  lc_Telef
           IF             lc_Telef          IS   NULL
            OR            lc_Telef          =    " "   THEN
             LET          lc_Telef          =    0
           ELSE
             LET          lc_Telef          =    lc_Telef
           END IF
           DISPLAY        lc_Telef          TO  lc_Telef

        BEFORE FIELD num_cuenta
           LET  num_cuenta     = gc_num_cuenta

        BEFORE FIELD fecha_solicitud
           IF   gr_ret_parcial_issste.fecha_solicitud    IS NULL
            OR  gr_ret_parcial_issste.fecha_solicitud    =  "12/31/1899"
            OR  gr_ret_parcial_issste.fecha_solicitud    =  " "  THEN
                LET  fecha_solicitud        = TODAY
           ELSE
                LET  fecha_solicitud        =   gr_ret_parcial_issste.fecha_solicitud
           END IF
           DISPLAY   fecha_solicitud        TO  fecha_solicitud

        BEFORE FIELD folio
           LET    folio                     =   gr_ret_parcial_issste.folio_solicitud
           LET    ls_recaptura_folio        =   1     --- para k recapture

        ###BEFORE FIELD  lc_estad_cod
        ###   LET    lc_estad_desc          =   f_VerEstad(lc_estad_cod)

        ###BEFORE FIELD  lc_deleg_cod
        ###   LET    lc_deleg_desc          =   f_VerDeleg(lc_estad_cod, lc_deleg_cod)

        ON KEY (ESC)
           IF   lc_Telef        IS  NULL
            OR  lc_Telef        =   " "   THEN
                CALL   f_DespMen("TELEFONO No puede ser un blanco ")
                NEXT FIELD lc_Telef
           END IF
           LET       gr_ret_solicitante.telefono           =  lc_Telef

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

           IF   fecha_solicitud  >  TODAY  THEN
                CALL   f_DespMen("FECHA DE SOLICITUD No puede mayor que fecha del dia ")
                NEXT   FIELD fecha_solicitud
           ELSE
               LET   gr_ret_parcial_issste.fecha_solicitud =  fecha_solicitud
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
                     LET   gr_ret_parcial_issste.folio_solicitud = folio
                END IF
           END IF

           ###   IF   lc_estad_cod IS NOT NULL THEN
           ###        LET  lc_estad_desc  = f_VerEstad(lc_estad_cod)
           ###        DISPLAY BY NAME lc_estad_desc
           ###   END IF
           ###   IF   lc_deleg_cod IS NOT NULL THEN
           ###        LET  lc_deleg_desc  = f_VerDeleg(lc_estad_cod, lc_deleg_cod)
           ###        DISPLAY BY NAME lc_deleg_desc
           ###   END IF

           LET    gr_ret_parcial_issste.folio_solicitud = folio
           LET    gs_Cance = 0
           EXIT   INPUT

        ON KEY (CONTROL-C,  INTERRUPT)
           LET  gs_Cance = 1
           EXIT INPUT

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

        AFTER  FIELD  fecha_solicitud
           IF   fecha_solicitud  >  TODAY  THEN
                CALL   f_DespMen("FECHA DE SOLICITUD No puede mayor que fecha del dia ")
                NEXT FIELD fecha_solicitud
           ELSE
                LET   gr_ret_parcial_issste.fecha_solicitud = fecha_solicitud
           END IF

        AFTER FIELD  folio
           ### Validar y Recaptura
           IF  gs_afores     = 578  THEN
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
               LET   gr_ret_parcial_issste.folio_solicitud = folio

               ###  Validamos si ha cambiado el folio gi_folio es el que buscamos en rec_solicitud
               IF      gi_folio                  <>    gr_ret_parcial_issste.folio_solicitud
                 OR    ls_recaptura_folio        =  1       THEN
                     LET    ls_recaptura_folio   =  1
               END IF

               IF   ls_recaptura_folio  =   1    THEN
                    LET  ls_folio     =  0
                    CALL f_Recaptura_Folio('F', gr_ret_parcial_issste.folio_solicitud)
                                 RETURNING ls_folio

                   IF   ls_folio <> 0 THEN
                        LET             ls_recaptura_folio = 1
                        LET             gr_ret_parcial_issste.folio_solicitud  = 0
                        LET             folio   = 0
                        DISPLAY gr_ret_parcial_issste.folio_solicitud TO folio
                        NEXT FIELD      folio
                   ELSE
                        LET             ls_recaptura_folio = 0
                        LET             gi_folio    = gr_ret_parcial_issste.folio_solicitud
                   END IF
                   LET  gr_ret_parcial_issste.folio_solicitud = folio
               ELSE
                   LET  gr_ret_parcial_issste.folio_solicitud = folio
               END IF
           END IF

        AFTER FIELD  num_cuenta
           IF   num_cuenta IS NULL
            OR  num_cuenta = ' '   THEN
                CALL   f_DespMen("NUMERO DE CUENTA No puede ser blanco ")
                NEXT FIELD num_cuenta
           END IF

        AFTER FIELD  lc_Telef
           IF   lc_Telef        IS NULL
            OR  lc_Telef        = " "   THEN
                CALL   f_DespMen("TELEFONO No puede ser un blanco ")
                NEXT FIELD lc_Telef
           END IF
           LET       gr_ret_solicitante.telefono     =  lc_Telef

        ###AFTER FIELD lc_estad_cod
        ###   IF   lc_estad_cod IS NOT NULL THEN
        ###        LET  lc_estad_desc  = f_VerEstad(lc_estad_cod)
        ###        DISPLAY BY NAME lc_estad_desc
        ###   END IF
        ###   LET  gr_ret_parcial_issste.estad_cod   = lc_estad_cod

        ###AFTER FIELD lc_deleg_cod
        ###   IF   lc_deleg_cod IS NOT NULL THEN
        ###        LET  lc_deleg_desc  = f_VerDeleg(lc_estad_cod, lc_deleg_cod)
        ###        IF   lc_deleg_desc IS NULL   THEN
        ###             CALL   f_DespMen("Clave de la DELEGACION NO CORRESPONDE AL Estado  ")
        ###             NEXT FIELD lc_estad_cod
        ###        END IF
        ###        DISPLAY BY NAME lc_deleg_desc
        ###   END IF
        ###   IF   lc_estad_cod IS NOT NULL
        ###    AND lc_deleg_cod IS     NULL   THEN
        ###        CALL   f_DespMen("Capturo Estado, Delegación no puede ser blanco   ")
        ###        NEXT FIELD lc_estad_cod
        ###   END IF
        ###   LET  gr_ret_parcial_issste.deleg_cod   = lc_deleg_cod

     END INPUT
     IF   fecha_solicitud IS NULL    THEN
          IF  gs_Cance = 1   THEN
               EXIT WHILE
          END IF
     ELSE
         EXIT WHILE
     END IF

  END WHILE

  RETURN gr_ret_parcial_issste.*,
         gr_ret_monto_par_issste.*,
         gr_ret_solicitante.*
END FUNCTION


### Grabar retiros parciales
FUNCTION f_Grabar2()
  DEFINE     v_ejecuta           CHAR(100)
  DEFINE     ls_cuantos          SMALLINT
  DEFINE     ls_ResMar           SMALLINT

  ### Clave 1 graba capturada con error
  ### Clave 2 graba por seleccion de Grabar y gs_Grabe  = 1 ya grabo  tons debe ser 0

  LET        gc_query   =  "  INSERT INTO  ret_parcial_issste                     ",
                           "  VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
  PREPARE    p_InsRetiroPar     FROM  gc_query

  LET        gc_query   =  "  INSERT INTO  ret_monto_par_issste                   ",
                           "  VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)                  "
  PREPARE    p_InsRetMonPar     FROM  gc_query

  LET        gc_query   =  "  INSERT INTO  ret_solicitante                        ",
                           "  VALUES (?,?,?,?,?,?)                                "
  PREPARE    p_InsSolici        FROM  gc_query

  IF    gs_Grabe = 0     THEN
        CALL   f_DespMen("Guardando información .... Espere un momento   ")
        LET  ls_ResMar    = 0
        CALL f_marcaje(gr_ret_parcial_issste.nss,
                       gr_ret_parcial_issste.consecutivo,
                       "F")   RETURNING ls_ResMar

        IF   ls_ResMar        =   0  THEN
             SELECT  COUNT(*)
               INTO  ls_cuantos
               FROM  ret_beneficiario
              WHERE  nss         = gr_ret_parcial_issste.nss
                AND  consecutivo = gr_ret_parcial_issste.consecutivo

             IF  STATUS = NOTFOUND
               OR ls_cuantos = 0 THEN
                  LET   v_ejecuta = "fglgo RETM810 ", gr_ret_parcial_issste.nss CLIPPED,         " ",
                                                      gr_ret_parcial_issste.consecutivo CLIPPED, " ",
                                                      "A", " ",
                                                      "1", " ",
                                                      gc_num_cuenta
                  RUN   v_ejecuta
             END IF

             IF      gs_afores         = 578   THEN
                 IF      gr_ret_parcial_issste.deleg_cod IS NOT NULL
                   AND   gr_ret_parcial_issste.estad_cod IS NOT NULL THEN
                     LET gr_ret_parcial_issste.deleg_cod =   gr_ret_parcial_issste.estad_cod * 1000
                                                         +   gr_ret_parcial_issste.deleg_cod
                 END IF
             END IF

             LET     gr_ret_solicitante.paren_cod         =   12
             LET     gr_ret_monto_par_issste.siefore      = gi_siefore
             EXECUTE   p_InsRetiroPar     USING  gr_ret_parcial_issste.*
             EXECUTE   p_InsRetMonPar     USING  gr_ret_monto_par_issste.*
             EXECUTE   p_InsSolici        USING  gr_ret_solicitante.*

             #CPL-1844
             INSERT INTO ret_sol_issste_constancia VALUES(gr_ret_parcial_issste.nss        ,
                                                          gr_ret_parcial_issste.consecutivo,
                                                          gr_constancia.constancia)

             LET       gs_Grabe = 1
        END IF
  END IF

END FUNCTION


### CAptura de retiro RO
FUNCTION  f_InpdelRO()
  DEFINE     lr_ret_parcial_issste    RECORD LIKE ret_parcial_issste.*
  DEFINE     lr_ret_monto_par_issste  RECORD LIKE ret_monto_par_issste.*
  DEFINE     lr_ret_solicitante       RECORD LIKE ret_solicitante.*
  DEFINE     ls_subcuenta             LIKE   dis_cuenta.subcuenta
  DEFINE     ls_tipo_movimiento       LIKE   dis_cuenta.tipo_movimiento
  DEFINE     li_Monto                 DECIMAL(16,6)
  DEFINE     ld_Saldo30               DECIMAL(16,6)
  DEFINE     ld_Saldo31               DECIMAL(16,6)
  DEFINE     ld_Saldo32               DECIMAL(16,6)
  DEFINE     ld_SaldoRCV              DECIMAL(16,6)
  DEFINE     lc_Telef                 CHAR(30)

  LET   ls_subcuenta                                =   30
  LET   ls_tipo_movimiento                          =    1
  CALL  f_Buscael10(gr_ret_parcial_issste.nss, ls_subcuenta, ls_tipo_movimiento) RETURNING ld_Saldo30
  LET   ls_subcuenta                                =   31
  LET   ls_tipo_movimiento                          =    1
  CALL  f_Buscael10(gr_ret_parcial_issste.nss, ls_subcuenta, ls_tipo_movimiento) RETURNING ld_Saldo31
  LET   ls_subcuenta                                =   32
  LET   ls_tipo_movimiento                          =    1
  CALL  f_Buscael10(gr_ret_parcial_issste.nss, ls_subcuenta, ls_tipo_movimiento) RETURNING ld_Saldo32

  LET   ld_SaldoRCV       = 0
  LET   ld_SaldoRCV                                 =  ld_Saldo30
                                                    +  ld_Saldo31
                                                    +  ld_Saldo32
  LET   gr_ret_monto_par_issste.mto_10p_rcv         =  ld_SaldoRCV * .10
  LET   gr_ret_monto_par_issste.salario_base_cot    =  0
  LET   gr_ret_monto_par_issste.mto_75sbc           =  0
  LET   gr_ret_monto_par_issste.mto_a_pagar         =  0


  DISPLAY   gr_ret_parcial_issste.sec_pension                     TO   sec_pension
  DISPLAY   gr_ret_parcial_issste.regimen                         TO   regimen
  DISPLAY   gr_ret_parcial_issste.tipo_seguro                     TO   tipo_seguro
  DISPLAY   gr_ret_parcial_issste.num_issste                      TO   num_issste
  DISPLAY   gr_ret_monto_par_issste.salario_base_cot              TO   salario_base_cot
  DISPLAY   gr_ret_monto_par_issste.mto_75sbc                     TO   mto_75sbc
  DISPLAY   gr_ret_monto_par_issste.mto_10p_rcv                   TO   mto_10p_rcv
  DISPLAY   gr_ret_monto_par_issste.mto_a_pagar                   TO   ld_TotPag

  CALL  f_InputComun2(gr_ret_parcial_issste.*, gr_ret_monto_par_issste.*, gr_ret_solicitante.*)
        RETURNING gr_ret_parcial_issste.*,
                  gr_ret_monto_par_issste.*,
                  gr_ret_solicitante.*

END FUNCTION


### Input del RO
FUNCTION  f_InputComun2(lr_ret_parcial_issste, lr_ret_monto_par_issste, lr_ret_solicitante)
  DEFINE     lr_ret_parcial_issste    RECORD LIKE ret_parcial_issste.*
  DEFINE     lr_ret_monto_par_issste  RECORD LIKE ret_monto_par_issste.*
  DEFINE     lr_ret_solicitante       RECORD LIKE ret_solicitante.*
  DEFINE     fecha_solicitud          DATE
  DEFINE     folio                    LIKE   ret_parcial_issste.folio_solicitud
  DEFINE     lc_Paren                 CHAR(100)
  DEFINE     lc_Telef                 CHAR(100)
  DEFINE     Telefono                 CHAR(100)
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
  DEFINE     num_cuenta               LIKE   ret_beneficiario.num_cuenta
  DEFINE     lc_estad_cod             LIKE   ret_sol_issste_tx.estad_cod
  DEFINE     lc_deleg_cod             LIKE   ret_sol_issste_tx.deleg_cod
  DEFINE     lc_estad_desc            CHAR(100)
  DEFINE     lc_deleg_desc            CHAR(100)
  DEFINE     salario_base_cot         LIKE   ret_monto_par_issste.salario_base_cot

  LET   paterno_afore               =    ""
  LET   materno_afore               =    ""
  LET   nombre_afore                =    ""
  LET   lc_Paren                    =    ""
  LET   lc_Telef                    =    "0"
  LET   gs_Muerto                   =    1
  LET   gs_Cance                    =    0

  LET     lr_ret_solicitante.apellido_paterno  =   lr_ret_parcial_issste.apellido_paterno
  LET     lr_ret_solicitante.apellido_materno  =   lr_ret_parcial_issste.apellido_materno
  LET     lr_ret_solicitante.nombres           =   lr_ret_parcial_issste.nombre
  LET     lr_ret_solicitante.paren_cod         =   12
  LET     gr_ret_solicitante.paren_cod         =   12

  ### SELECT  paren_cod
  ###   INTO  ls_paren_cod
  ###   FROM  tab_parentesco
  ###  WHERE  paren_desc = la_benef[li_x1].paren_desc
  LET     lr_ret_solicitante.paren_cod         =   ls_paren_cod

  DISPLAY "[Esc] Grabar     [Ctrl-C] Cancelar                                               " AT 01,1 ATTRIBUTE(REVERSE)
  WHILE TRUE
     ###LET    lc_estad_cod                 =   lr_ret_parcial_issste.estad_cod
     ###LET    lc_deleg_cod                 =   lr_ret_parcial_issste.deleg_cod
     ###LET    lc_estad_desc                =   f_VerEstad(lc_estad_cod)
     ###LET    lc_deleg_desc                =   f_VerDeleg(lc_estad_cod, lc_deleg_cod)
     LET    num_cuenta                   =   gc_num_cuenta
     LET    folio                        =   lr_ret_parcial_issste.folio_solicitud
     DISPLAY     folio                   TO  folio
     IF   lr_ret_parcial_issste.fecha_solicitud    IS NULL
      OR  lr_ret_parcial_issste.fecha_solicitud    =  " "  THEN
          LET  fecha_solicitud = TODAY
     ELSE
          LET  fecha_solicitud = lr_ret_parcial_issste.fecha_solicitud
     END IF
     DISPLAY     fecha_solicitud         TO  fecha_solicitud
     DISPLAY     num_cuenta              TO  num_cuenta
     IF          Telefono                IS  NULL
      OR         Telefono                =   " "   THEN
         LET     Telefono                =   0
     ELSE
         LET     Telefono                =   lr_ret_solicitante.telefono
     END IF
     DISPLAY Telefono         TO Telefono
     ###DISPLAY     lc_estad_cod            TO  lc_estad_cod
     ###DISPLAY     lc_deleg_cod            TO  lc_deleg_cod

     INPUT BY NAME    Telefono        ,
                      salario_base_cot,
     ###                 lc_estad_cod,
     ###                 lc_deleg_cod,
                      folio          ,
                      fecha_solicitud,
                      num_cuenta     ,
                      gr_constancia.constancia
                      WITHOUT DEFAULTS

        BEFORE FIELD  Telefono
           IF     Telefono          IS NULL
            OR    Telefono          =  " "   THEN
             LET  Telefono          =  0
           ELSE
             LET  Telefono          =  lr_ret_solicitante.telefono
           END IF
           DISPLAY Telefono         TO Telefono

        BEFORE FIELD num_cuenta
           LET  num_cuenta     = gc_num_cuenta

        BEFORE FIELD fecha_solicitud
           IF   lr_ret_parcial_issste.fecha_solicitud    IS NULL
            OR  lr_ret_parcial_issste.fecha_solicitud    =  "12/31/1899"
            OR  lr_ret_parcial_issste.fecha_solicitud    =  " "  THEN
                LET  fecha_solicitud = TODAY
           ELSE
                LET  fecha_solicitud = lr_ret_parcial_issste.fecha_solicitud
           END IF
           DISPLAY   fecha_solicitud     TO  fecha_solicitud

        BEFORE FIELD folio
           LET    folio                =  lr_ret_parcial_issste.folio_solicitud
           LET    ls_recaptura_folio   =  1     --- para k recapture

        BEFORE FIELD salario_base_cot
           LET salario_base_cot = gr_DatMarPar.salario_promedio/30

        ###BEFORE FIELD  lc_estad_cod
        ###   LET    lc_estad_desc    = f_VerEstad(lc_estad_cod)

        ###BEFORE FIELD  lc_deleg_cod
        ###   LET    lc_deleg_desc    = f_VerDeleg(lc_estad_cod, lc_deleg_cod)

        ON KEY (ESC)
           IF   Telefono        IS NULL
            OR  Telefono        = " "   THEN
                CALL   f_DespMen("TELEFONO No puede ser un blanco ")
                NEXT FIELD Telefono
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

           IF   fecha_solicitud  >  TODAY  THEN
                CALL   f_DespMen("FECHA DE SOLICITUD No puede mayor que fecha del dia ")
                NEXT   FIELD fecha_solicitud
           ELSE
               LET   lr_ret_parcial_issste.fecha_solicitud = fecha_solicitud
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
                     LET   lr_ret_parcial_issste.folio_solicitud = folio
                END IF
           END IF
           IF   salario_base_cot    IS NULL
            OR  salario_base_cot    =  0    THEN
                CALL   f_DespMen(" SBC NO puede ser ceros ")
                NEXT FIELD salario_base_cot
           END IF
           LET  lr_ret_monto_par_issste.salario_base_cot        =     salario_base_cot
           LET  lr_ret_monto_par_issste.mto_75sbc               =     salario_base_cot * 75
           IF   lr_ret_monto_par_issste.mto_75sbc               <     lr_ret_monto_par_issste.mto_10p_rcv  THEN
                LET   lr_ret_monto_par_issste.mto_a_pagar       =     lr_ret_monto_par_issste.mto_75sbc
           ELSE
                LET   lr_ret_monto_par_issste.mto_a_pagar       =     lr_ret_monto_par_issste.mto_10p_rcv
           END IF
           DISPLAY    lr_ret_monto_par_issste.salario_base_cot  TO    salario_base_cot
           DISPLAY    lr_ret_monto_par_issste.mto_75sbc         TO    mto_75sbc
           DISPLAY    lr_ret_monto_par_issste.mto_a_pagar       TO    ld_TotPag

        ###   IF   lc_estad_cod IS NOT NULL THEN
        ###        LET  lc_estad_desc  = f_VerEstad(lc_estad_cod)
        ###        DISPLAY BY NAME lc_estad_desc
        ###   END IF
        ###   IF   lc_deleg_cod IS NOT NULL THEN
        ###        LET  lc_deleg_desc  = f_VerDeleg(lc_estad_cod, lc_deleg_cod)
        ###        DISPLAY BY NAME lc_deleg_desc
        ###   END IF

           LET  lr_ret_parcial_issste.folio_solicitud = folio
           LET  gs_Cance = 0
           EXIT INPUT

        ON KEY (CONTROL-C,  INTERRUPT)
           LET  gs_Cance = 1
           EXIT INPUT

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

        AFTER  FIELD  salario_base_cot
           IF   salario_base_cot    IS NULL
            OR  salario_base_cot    =  0    THEN
                CALL   f_DespMen(" SBC NO puede ser ceros ")
                NEXT FIELD salario_base_cot
           END IF
           LET  lr_ret_monto_par_issste.salario_base_cot    =    salario_base_cot
           LET  lr_ret_monto_par_issste.mto_75sbc           =    salario_base_cot * 75
           IF   lr_ret_monto_par_issste.mto_75sbc           <    lr_ret_monto_par_issste.mto_10p_rcv  THEN
                LET   lr_ret_monto_par_issste.mto_a_pagar   =    lr_ret_monto_par_issste.mto_75sbc
           ELSE
                LET   lr_ret_monto_par_issste.mto_a_pagar   =    lr_ret_monto_par_issste.mto_10p_rcv
           END IF
           DISPLAY    lr_ret_monto_par_issste.salario_base_cot  TO    salario_base_cot
           DISPLAY    lr_ret_monto_par_issste.mto_75sbc         TO    mto_75sbc
           DISPLAY    lr_ret_monto_par_issste.mto_a_pagar       TO    ld_TotPag

        AFTER  FIELD  fecha_solicitud
           IF   fecha_solicitud  >  TODAY  THEN
                CALL   f_DespMen("FECHA DE SOLICITUD No puede mayor que fecha del dia ")
                NEXT FIELD fecha_solicitud
           ELSE
                LET   lr_ret_parcial_issste.fecha_solicitud = fecha_solicitud
           END IF

        AFTER FIELD  folio
           ### Validar y Recaptura
           IF  gs_afores     = 578  THEN
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
               LET   lr_ret_parcial_issste.folio_solicitud = folio

               ###  Validamos si ha cambiado el folio gi_folio es el que buscamos en rec_solicitud
               IF      gi_folio                  <>    lr_ret_parcial_issste.folio_solicitud
                 OR    ls_recaptura_folio        =  1       THEN
                     LET    ls_recaptura_folio   =  1
               END IF

               IF   ls_recaptura_folio  =   1    THEN
                    LET  ls_folio     =  0
                    CALL f_Recaptura_Folio('F', lr_ret_parcial_issste.folio_solicitud)
                                 RETURNING ls_folio

                   IF   ls_folio <> 0 THEN
                        LET             ls_recaptura_folio = 1
                        LET             lr_ret_parcial_issste.folio_solicitud  = 0
                        LET             folio   = 0
                        DISPLAY lr_ret_parcial_issste.folio_solicitud TO folio
                        NEXT FIELD      folio
                   ELSE
                        LET             ls_recaptura_folio = 0
                        LET             gi_folio    = lr_ret_parcial_issste.folio_solicitud
                   END IF
                   LET  lr_ret_parcial_issste.folio_solicitud = folio
               ELSE
                   LET  lr_ret_parcial_issste.folio_solicitud = folio
               END IF
           END IF

        AFTER FIELD  num_cuenta
           IF   num_cuenta IS NULL
            OR  num_cuenta = ' '   THEN
                CALL   f_DespMen("NUMERO DE CUENTA No puede ser blanco ")
                NEXT FIELD num_cuenta
           END IF

        AFTER FIELD  Telefono
           IF   Telefono        IS NULL
            OR  Telefono        = " "   THEN
                CALL   f_DespMen("TELEFONO No puede ser un blanco ")
                NEXT FIELD Telefono
           END IF
           LET  lr_ret_solicitante.telefono = Telefono

        ###AFTER FIELD lc_estad_cod
        ###   IF   lc_estad_cod IS NOT NULL THEN
        ###        LET  lc_estad_desc  = f_VerEstad(lc_estad_cod)
        ###        DISPLAY BY NAME lc_estad_desc
        ###   END IF
        ###   LET  lr_ret_parcial_issste.estad_cod   = lc_estad_cod

        ###AFTER FIELD lc_deleg_cod
        ###   IF   lc_deleg_cod IS NOT NULL THEN
        ###        LET  lc_deleg_desc  = f_VerDeleg(lc_estad_cod, lc_deleg_cod)
        ###        IF   lc_deleg_desc IS NULL   THEN
        ###             CALL   f_DespMen("Clave de la DELEGACION NO CORRESPONDE AL Estado  ")
        ###             NEXT FIELD lc_estad_cod
        ###        END IF
        ###        DISPLAY BY NAME lc_deleg_desc
        ###   END IF
        ###   IF   lc_estad_cod IS NOT NULL
        ###    AND lc_deleg_cod IS     NULL   THEN
        ###        CALL   f_DespMen("Capturo Estado, Delegación no puede ser blanco   ")
        ###        NEXT FIELD lc_estad_cod
        ###   END IF
        ###   LET  lr_ret_parcial_issste.deleg_cod   = lc_deleg_cod

     END INPUT
     IF   fecha_solicitud IS NULL    THEN
          IF  gs_Cance = 1   THEN
               EXIT WHILE
          END IF
     ELSE
         EXIT WHILE
     END IF

  END WHILE

  RETURN lr_ret_parcial_issste.*,
         lr_ret_monto_par_issste.*,
         lr_ret_solicitante.*
END FUNCTiON
