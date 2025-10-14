################################################################################
#Owner             => E.F.P.                                                   #
#Programa RETC728  => LIQUIDACION DE DISPOSICION                               #
#Fecha creacion    => 16 DE OCTUBRE DE 2009                                    #
#Fecha Nvo Retiro  => Noviembre 2011                                           #
#By                => JAVIER GONZALEZ JERONIMO                                 #
#Fecha actualiz.   => Dic, 2011                                                #
#Actualizacion     => JGHM                                                     #
#                  => IJR - 06/FEB/2012                                        #
#Sistema           => Retiros, punto 2.5 Disposiciones                         #
################################################################################
DATABASE safre_af

GLOBALS
    DEFINE gr_edo RECORD
        pre_liq     LIKE ret_estado_issste.estado_solicitud ,
        liquidado   LIKE ret_estado_issste.estado_solicitud
    END RECORD

    DEFINE gr_folios RECORD
        fec_prel    DATE                         ,
        total       LIKE ret_sol_issste_tx.folio ,
        parcial     LIKE ret_sol_issste_tx.folio ,
        transfer    LIKE ret_sol_issste_tx.folio
    END RECORD

    DEFINE  HOY                   DATE
    DEFINE  enter                 CHAR(001) ,
            gc_usuario            CHAR(015)
    DEFINE  gs_flag               ,
            gs_codigo_afore       SMALLINT
    DEFINE  ga_fecha              DATE 
    DEFINE  ga_fol                ARRAY[5] OF RECORD 
            gs_fol                INTEGER  
    END RECORD  
    DEFINE  gc_query              CHAR(1500) 
    DEFINE  gc_men                CHAR(20) 
    DEFINE  gc_lin_men            SMALLINT 
    define  g_tecla               CHAR(1)
   
END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("RETC728.log")
    CALL init() 
    CALL f_despliega_info() 
    

END MAIN


FUNCTION init()
#i-------------
   DEFINE   lc_prepare      CHAR(300)

   LET HOY               =  TODAY
   SELECT  codigo_afore     ,
           USER
     INTO  gs_codigo_afore  ,
           gc_usuario
     FROM  tab_afore_local

   ----- ESTADOS DE S0LICITUD -----
   SELECT  A.estado_solicitud
     INTO  gr_edo.pre_liq
     FROM  ret_estado A
    WHERE  A.descripcion = "PRELIQUIDADO DISPOSICION"

   SELECT  A.estado_solicitud
     INTO  gr_edo.liquidado
     FROM  ret_estado A
    WHERE  A.descripcion = "LIQUIDADO DISPOSICION"

   ----- DESMARCA -----
   LET lc_prepare = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"
   PREPARE eje_desmarca FROM lc_prepare

   LET   gc_query  =  ' SELECT  '||'''x'''||', B.folio, COUNT(*)                       ',
                      '   FROM  ret_solicitud_tx    A,                                 ',
                      '         ret_preliquida      B                                  ',
                      '  WHERE  b.fecha_conversion             =  ?                    ',
                      '    AND  A.estado_solicitud             =       '||gr_edo.pre_liq,
                      '    AND  B.tipo_movimiento              IN (820, 830)           ',
                      '    AND  A.consecutivo                  =  B.consecutivo_lote   ',
                      '    AND  A.nss                          =  B.nss                ',
                      '  GROUP  BY 1, 2                                                '
   PREPARE  p_sel_fecha        FROM  gc_query
   DECLARE  d_sel_fecha  CURSOR FOR  p_sel_fecha

   LET   gc_query  =  ' SELECT  folio_abono        ',
                      '   FROM  ret_folio          ',
                      '  WHERE  folio_cargo   =  ? '
   PREPARE  p_sel_fol          FROM  gc_query

   LET   gc_query  =  ' SELECT  UNIQUE a.nss       ,                    ',
                      '         b.marca_cod        ,                    ',
                      '         b.correlativo      ,                    ',
                      '         0                ,                      ',
                      '         0                                       ',
                      '   FROM  ret_preliquida   a,                     ',
                      '         cta_act_marca    b                      ',
                      '  WHERE  a.folio            =  ?                 ', 
                      '    AND  a.tipo_movimiento  IN (820, 830, 921, 800, 810, 815)   ',
                      '    AND  b.marca_cod        IN (820, 830, 921, 800, 810, 815)   ',
                      '    AND  a.nss              =  b.nss             '
   PREPARE  p_sel_mar          FROM  gc_query
   DECLARE  d_sel_mar    CURSOR FOR  p_sel_mar  

   LET   gc_query  =  ' SELECT  UNIQUE(curp)    ,                ',
                      '         nss,                             ',
                      '         consecutivo_lote                 ',
                      '   FROM  ret_preliquida                   ',
                      '  WHERE  folio            =  ?            ',
                      '    AND  tipo_movimiento  IN (820, 830)   ', 
                      '  ORDER  BY 1                             '
   PREPARE  p_sel_cue          FROM  gc_query
   DECLARE  d_sel_cue    CURSOR FOR  p_sel_cue  

   LET   gc_query  =  ' UPDATE  ret_solicitud_tx                  ',
                      '    SET  estado_solicitud     =   ?        ',
                      '  WHERE  nss                  =   ?        ',
                      '    AND  consecutivo          =   ?        ',
                      '    AND  estado_solicitud     =   ?        '
   PREPARE  p_upd_sol          FROM  gc_query

   LET   gc_query  =  ' UPDATE  ret_preliquida                    ',
                      '    SET  estado               =   ?        ',
                      '  WHERE  folio                =   ?        ',
                      '    AND  tipo_movimiento  IN (921, 922)    ', 
                      '    AND  estado               =   ?        '
   PREPARE  p_upd_ret          FROM  gc_query

   LET   gc_query  =  ' SELECT  UNIQUE b.fecha_conversion                              ',
                      '   FROM  ret_solicitud_tx    A,                                 ',
                      '         ret_preliquida      B                                  ',
                      '  WHERE  b.folio                        =  ?                    ',
                      '    AND  A.estado_solicitud             =       '||gr_edo.pre_liq,
                      '    AND  B.tipo_movimiento              IN (820, 830)           ',
                      '    AND  A.consecutivo                  =  B.consecutivo_lote   ',
                      '    AND  A.nss                          =  B.nss                '
   PREPARE  p_sel_fec          FROM  gc_query
END FUNCTION


FUNCTION f_despliega_info()

   DEFINE ld_fecha_conversion    DATE    
   DEFINE i                      SMALLINT
   DEFINE lr_info RECORD
          sel                    CHAR(1) ,
          folio                  INTEGER 
          END RECORD

   DEFINE ls_cont             ,
          ls_flag             SMALLINT

   DEFINE la_fol                       ARRAY[5] OF RECORD
          marca                          CHAR(1),
          folio                          INTEGER
          END RECORD
   DEFINE  cuantos                      INTEGER 
   DEFINE  ls_sale                      SMALLINT
   DEFINE  ls_folio_abo                 INTEGER
   DEFINE  lc_fecha_conversion          CHAR(10)
   DEFINE  ln_year                      SMALLINT
   DEFINE  ln_month                     SMALLINT
   DEFINE  ln_day                       SMALLINT
   DEFINE  v_pos                        SMALLINT
   DEFINE  v_entro                      SMALLINT

   FOR   i = 1 TO 5
         LET  la_fol[i].marca         =  NULL
         LET  la_fol[i].folio         =  NULL
   END FOR
   LET   cuantos                      =  NULL

   OPEN WINDOW RETC7281 AT 2,2 WITH FORM "RETC7281"                                                ATTRIBUTE (BORDER)
   DISPLAY " <CTRL-C> SALIR                                             <ESC> EJECUTAR    " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " RETC728        LIQUIDACION DE DISPOSICION EN PESOS                           " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

   LET ls_flag             =  1
   LET ld_fecha_conversion =  HOY

   WHILE TRUE

         INPUT ld_fecha_conversion WITHOUT DEFAULTS FROM fecha_conversion

             AFTER  FIELD fecha_conversion
                IF  ld_fecha_conversion  IS NULL THEN
                    LET  ls_sale   =  2
                    EXIT INPUT
                END IF

                LET i  =  1
                LET lc_fecha_conversion = ld_fecha_conversion
                LET ln_year             = lc_fecha_conversion[7, 10]
                LET ln_month            = lc_fecha_conversion[1,  2]
                LET ln_day              = lc_fecha_conversion[4,  5]
                
                FOREACH d_sel_fecha USING  ld_fecha_conversion INTO la_fol[i].*, cuantos
                    LET  i  =  i + 1
                END FOREACH

                IF SQLCA.SQLCODE = NOTFOUND OR i = 1 THEN
                    ERROR ' No hay información para la fecha solicitada '
                    NEXT  FIELD fecha_conversion
                ELSE
                    LET  ls_sale   =  1
                END IF
                EXIT  INPUT

            ON KEY (CONTROL-C)
                LET  ls_sale   =  0
                EXIT INPUT

         END INPUT

         IF  ls_sale    =  0  THEN
             EXIT WHILE
         END IF
         
LET i = i -1
         CALL  SET_COUNT(i)

         LET  ls_sale   =  0
         
         INPUT ARRAY  la_fol WITHOUT  DEFAULTS FROM  scr_reg.*


             BEFORE  FIELD  folio
                IF   ls_sale  = 1   THEN 
                     NEXT FIELD  sel
                END IF

             AFTER  FIELD  folio 
                IF   ld_fecha_conversion IS NULL  THEN
                     LET  ld_fecha_conversion =  Fnc_FechaFolio(la_fol[1].folio)
                     DISPLAY  ld_fecha_conversion  TO fecha_conversion
                END IF

             ON KEY (CONTROL-C)
                LET  ls_sale   =  0
                EXIT INPUT
                  
             ON KEY (ESC)
                LET v_pos = ARR_CURR()
                LET la_fol[v_pos].marca = get_fldbuf(sel)
                IF   ld_fecha_conversion IS NULL  THEN
                     ERROR  ' ES NECESARIO CAPTURAR UN FOLIO VALIDO  ... '       
                     NEXT FIELD folio
                ELSE 
                     LET  ls_sale   =  1
                END IF
                EXIT INPUT
         END INPUT

         IF  ls_sale    =  0  THEN
         ELSE
             CALL f_abre_ventana()
             FOR  i  =  1  TO 5
                  IF   la_fol[i].marca     =    'x'  THEN
                       LET  gc_lin_men        =  10
                       LET  gc_men            =  ' Folio .. ', la_fol[i].folio, ' '
                       DISPLAY "                                                   "  AT 5,5
                       CALL f_liquida(la_fol[i].folio)       
                       DISPLAY "                                                   "  AT 5,5
                       CALL f_act_estado_sol(la_fol[i].folio)
                       DISPLAY "                                                   "  AT 5,5
                       CALL f_desmarca_cta(la_fol[i].folio)
                       DISPLAY "                                                   "  AT 5,5
                       ### EXECUTE  p_sel_fol       USING  la_fol[i].folio
                       ###                           INTO  ls_folio_abo
                       ### LET  gc_men            =  ' ABONO ', ls_folio_abo, ' '
                       ### CALL f_liquida(ls_folio_abo)
                       ### DISPLAY "                                                   "  AT 5,5
                       ### No existe solicitud de folio abono 
                       ### CALL f_act_estado_sol(ls_folio_abo)
                       DISPLAY " PROCESO TERMINADO                                 "  AT 5,5
                       SLEEP 3
                       LET   ls_sale   = 1
                       LET v_entro = 1
                  END IF
             END FOR
             IF v_entro = 0 THEN
                PROMPT "NO SE SELECCIONO NINGUN FOLIO, PRESIONE <ENTER> PARA SALIR: " FOR CHAR g_tecla
             END IF
             CLOSE WINDOW RETC7282
         END IF
         EXIT WHILE
   END WHILE
   CLOSE WINDOW RETC7281
END FUNCTION


FUNCTION f_abre_ventana()
    OPEN WINDOW RETC7282 AT 2,2 WITH FORM "RETC7282" ATTRIBUTE(BORDER)
    DISPLAY "                                                                   RETIROS  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC7282      LIQUIDACION DE DISPOSICION EN PESOS                          " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)
END FUNCTION


FUNCTION f_liquida(ls_folio)
  DEFINE  ls_folio                    INTEGER 

  DISPLAY "LIQUIDANDO RECURSOS ... ", gc_men  AT gc_lin_men, 5
  LET  gc_lin_men  = gc_lin_men + 1
  SLEEP 3
   INSERT  INTO  dis_cuenta
   SELECT  *
     FROM  ret_preliquida
    WHERE  folio              =    ls_folio
      AND  tipo_movimiento    IN   (820, 830)
  SLEEP 3
END FUNCTION


FUNCTION f_desmarca_cta(ls_folio)
  DEFINE  ls_folio          ,
          li_desm           INTEGER
  DEFINE  lr_desmarca       RECORD
          nss               LIKE  cta_his_marca.nss          ,
          movimiento        LIKE  cta_his_marca.marca_cod    ,
          consec            LIKE  cta_his_marca.correlativo  ,
          edo_causa         LIKE  cta_his_marca.estado_marca ,
          marca_causa       LIKE  cta_his_marca.marca_causa
  END RECORD
    
  LET  li_desm = 0
  DISPLAY "DESMARCANDO CUENTAS ... ", gc_men AT gc_lin_men,5
  LET  gc_lin_men  = gc_lin_men + 1

  FOREACH  d_sel_mar                USING  ls_folio 
                                     INTO  lr_desmarca.*
         EXECUTE eje_desmarca        USING  lr_desmarca.*, 
                                            gc_usuario
        LET   li_desm = li_desm + 1
  END FOREACH
  DISPLAY  "CUENTAS DESMARCADAS ... ", li_desm AT gc_lin_men,10
  LET  gc_lin_men  = gc_lin_men + 1
END FUNCTION


FUNCTION f_act_estado_sol(ls_folio)
  DEFINE  ls_folio            INTEGER
  DEFINE  la_rets             INTEGER
  DEFINE  lr_datos            RECORD
          curp                LIKE dis_provision.curp             ,
          nss                 LIKE dis_provision.nss              ,
          consec              LIKE dis_provision.consecutivo_lote
  END RECORD
  DEFINE  ls_cont             SMALLINT

  DISPLAY "IDENTIFICANDO SOLICITUDES ... ", gc_men  AT gc_lin_men, 5
  LET  la_rets   =  0
  LET  gc_lin_men  = gc_lin_men + 1
  FOREACH  d_sel_cue     USING  ls_folio
                          INTO  lr_datos.*
        EXECUTE  p_upd_sol         USING  gr_edo.liquidado,
                                          lr_datos.nss,
                                          lr_datos.consec,
                                          gr_edo.pre_liq
       LET  la_rets  =  la_rets + 1
  END FOREACH
  DISPLAY "SOLICITUDES ACTUALIZADAS ... ", la_rets AT gc_lin_men, 5
  LET  gc_lin_men  = gc_lin_men + 1

  ### No actualiza preliquida Nov 2011
  ### EXECUTE  p_upd_ret        USING  gr_edo.liquidado,
  ###                                  ls_folio,   
  ###                                  gr_edo.pre_liq
END FUNCTION


FUNCTION  Fnc_FechaFolio(ls_folio)
  DEFINE  ls_folio                     INTEGER 
  DEFINE  ld_fecha                     DATE 
  
  EXECUTE  p_sel_fec    USING  ls_folio 
                         INTO  ld_fecha 

  IF  STATUS  =  NOTFOUND  THEN 
      LET ld_fecha  =  '' 
      ERROR  ' No existe el folio solicitado ' 
  END IF 
  RETURN  ld_fecha
END FUNCTION 



