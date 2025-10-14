#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETC710  => LIQUIDACION DE RECURSOS A DESINVERTIR                     #
#Fecha creacion    => 16 DE OCTUBRE DE 2009                                     #
#Fecha Nvo Retiro  => Octubre 2011                                              #
#By                => JAVIER GONZALEZ JERONIMO                                  #
#Fecha actualiz.   => Nov, 2011                                                 #
#Actualizacion     => JGHM                                                      #
#Sistema           => Retiros, punto 2.5                                        #
#------------------------------MODIFICACIONES-----------------------------------#
#Requerimiento     => EFPS-211                                                  #
#Fecha y Autor     => 19-Oct-2012  -  Alejandro Chagoya Salazar                 #
#Descripcion       => Se agrega la actualizacion del folio en ret_folio         #
#------------------------------MODIFICACIONES-----------------------------------#
#Requerimiento     => EFPS-225                                                  #
#Fecha y Autor     => 18-Ene-2013  -  Alejandro Chagoya Salazar                 #
#Descripcion       => Se cambia actualizacion del estado de 104 a 106 para      #
#                  => considerar las cuentas sin saldo a desinvertir            #
#                  => y se agrega log por usuario                               #
#################################################################################
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
    DEFINE  gc_lin_men            SMALLINT,
            m_log                 CHAR(30)

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I

#EFPS-225
    LET gc_usuario = FGL_GETENV("USER")
    LET m_log = gc_usuario CLIPPED, ".RETC710.log"
    CALL STARTLOG(m_log CLIPPED)

    CALL init() 
    CALL f_despliega_info() 

END MAIN

FUNCTION init()
#i-------------
   DEFINE   lc_prepare      CHAR(300)

   LET HOY               =  TODAY

   SELECT  codigo_afore
     INTO  gs_codigo_afore 
     FROM  tab_afore_local

   ----- ESTADOS DE S0LICITUD -----
   SELECT  A.estado_solicitud
     INTO  gr_edo.pre_liq
     FROM  ret_estado A
    WHERE  A.descripcion = "PRELIQUIDADO SALDO"

   SELECT  A.estado_solicitud
     INTO  gr_edo.liquidado
     FROM  ret_estado A
    WHERE  A.descripcion = "LIQUIDADO SALDO"

   ----- DESMARCA -----
   LET lc_prepare = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"
   PREPARE eje_desmarca FROM lc_prepare

   --- LET   gc_query  =  ' SELECT  '||'''x'''||', A.folio_cargo, COUNT(*)                 ',
   ---                    '   FROM  ret_solicitud_saldo A,                                 ',
   ---                    '         ret_preliquida      B                                  ',
   ---                    '  WHERE  YEAR(A.f_recep_procesar)       =  ?                    ',
   ---                    '    AND  MONTH(A.f_recep_procesar)      =  ?                    ',
   ---                    '    AND  DAY(A.f_recep_procesar)        =  ?                    ',
   ---                    '    AND  A.estado_solicitud             =       '||gr_edo.pre_liq,
   ---                    '    AND  B.tipo_movimiento              =  921                  ',
   ---                    '    AND  A.folio_cargo                  =  B.folio              ',
   ---                    '    AND  A.id_solicitud_saldo           =  B.consecutivo_lote   ',
   ---                    '    AND  A.nss                          =  B.nss                ',
   ---                    '  GROUP  BY 1, 2                                                '
   --- PREPARE  p_sel_fecha        FROM  gc_query
   --- DECLARE  d_sel_fecha  CURSOR FOR  p_sel_fecha

   LET   gc_query  =  ' SELECT  UNIQUE A.folio_cargo                                   ',
                      '   FROM  ret_solicitud_saldo A,                                 ',
                      '         OUTER ret_preliquida      B                            ',
                      '  WHERE  A.estado_solicitud             =       '||gr_edo.pre_liq,
                      '    AND  B.tipo_movimiento              =  921                  ',
                      '    AND  A.folio_cargo                  =  B.folio              ',
                      '    AND  A.id_solicitud_saldo           =  B.consecutivo_lote   ',
                      '    AND  A.nss                          =  B.nss                ',
                      '    AND  A.folio_cargo                  =  ?                    '
   PREPARE  p_exi_fol          FROM  gc_query
   DECLARE  d_exi_fol    CURSOR FOR  p_exi_fol  

   LET   gc_query  =  ' SELECT  folio_abono        ',
                      '   FROM  ret_folio          ',
                      '  WHERE  folio_cargo   =  ? '
   PREPARE  p_sel_fol          FROM  gc_query

   ---- Solo existe marca para tipo_movimiento 921
   LET   gc_query  =  ' SELECT  UNIQUE(nss)      ,               ',
                      '         tipo_movimiento  ,               ',
                      '         consecutivo_lote ,               ',
                      '         0                ,               ',
                      '         0                                ',
                      '   FROM  ret_preliquida                   ',
                      '  WHERE  folio            =  ?            ', 
                      '    AND  tipo_movimiento  =  921          ',
                      '  ORDER  BY 1                             '
   PREPARE  p_sel_mar          FROM  gc_query
   DECLARE  d_sel_mar    CURSOR FOR  p_sel_mar  

#EFPS-225 - INI
   LET   gc_query  =  ' SELECT  nss,                    \n',
                      '         id_solicitud_saldo      \n ',
                      '   FROM  ret_solicitud_saldo     \n ',
                      '  WHERE  folio_cargo       = ?   \n ',
                      '    AND  estado_solicitud  = ?   \n '

   PREPARE  p_sel_cue          FROM  gc_query
   DECLARE  d_sel_cue    CURSOR FOR  p_sel_cue  

   LET   gc_query  =  ' UPDATE  ret_solicitud_saldo      \n ',
                      '    SET  estado_solicitud     = ? \n ',
                      '  WHERE  id_solicitud_saldo   = ? \n ',
                      '  AND    folio_cargo          = ? \n',
                      '  AND    estado_solicitud     = ? \n '

   PREPARE  p_upd_sol          FROM  gc_query
#EFPS-225 - FIN

   LET   gc_query  =  ' UPDATE  ret_preliquida                    ',
                      '    SET  estado               =   ?        ',
                      '  WHERE  folio                =   ?        ',
                      '    AND  tipo_movimiento  IN (921, 922)    ', 
                      '    AND  estado               =   ?        '
   PREPARE  p_upd_ret          FROM  gc_query

   LET   gc_query  =  ' SELECT  UNIQUE f_recep_procesar     ',
                      '   FROM  ret_solicitud_saldo         ',
                      '  WHERE  folio_cargo          =   ?  ',
                      '    AND  estado_solicitud     =      '||gr_edo.pre_liq
   PREPARE  p_sel_fer          FROM  gc_query
   DECLARE  d_sel_fer    CURSOR FOR  p_sel_fer  
END FUNCTION


FUNCTION f_despliega_info()
    DEFINE  ld_fecha_conversion    DATE    
    DEFINE  i                      SMALLINT
    DEFINE  lr_info RECORD
            sel                    CHAR(1) ,
            folio                  INTEGER 
    END RECORD

    DEFINE  ls_cont             ,
            ls_flag             SMALLINT

   DEFINE  la_fol                       ARRAY[5] OF RECORD
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
   DEFINE  ls_folio                     INTEGER

   FOR   i = 1 TO 5
         LET  la_fol[i].marca         =  NULL
         LET  la_fol[i].folio         =  NULL
   END FOR
   LET   cuantos                      =  NULL

   LET   ls_folio                     =  0

   OPEN WINDOW RETC7101 AT 4,4 WITH FORM "RETC7101"                                                ATTRIBUTE (BORDER)
   DISPLAY " <CTRL-C> SALIR                                             <ESC> EJECUTAR    " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " RETC710        LIQUIDACION DE RECURSOS A DESINVERTIR                         " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

   LET  ls_flag             =  1
   LET  ld_fecha_conversion =  HOY
   WHILE TRUE

         --- INPUT  ld_fecha_conversion
         ---        WITHOUT DEFAULTS
         ---  FROM  fecha_conversion

         ---     AFTER  FIELD fecha_conversion
         ---        IF  ld_fecha_conversion  IS NULL THEN
         ---            LET  ls_sale   =  2
         ---            EXIT INPUT
         ---        END IF

         ---        LET  i  =  1
         ---        LET      lc_fecha_conversion =   ld_fecha_conversion
         ---        LET      ln_year             =   lc_fecha_conversion[7, 10]
         ---        LET      ln_month            =   lc_fecha_conversion[1,  2]
         ---        LET      ln_day              =   lc_fecha_conversion[4,  5]
         ---        FOREACH  d_sel_fecha      USING  ln_year,
         ---                                         ln_month,
         ---                                         ln_day
         ---                                   INTO  la_fol[i].*, cuantos
         ---            LET  i  =  i + 1
         ---        END FOREACH

         ---        IF   SQLCA.SQLCODE  =  NOTFOUND
         ---         OR  i              =  1         THEN
         ---            ERROR ' No hay información para la fecha solicitada '
         ---            NEXT  FIELD fecha_conversion
         ---        ELSE
         ---            CALL  SET_COUNT(i - 1)
         ---            LET  ls_sale   =  1
         ---        END IF
         ---        EXIT  INPUT
         ---    ON KEY (CONTROL-C)
         ---        LET  ls_sale   =  0
         ---        EXIT INPUT
         --- END INPUT

         --- IF  ls_sale    =  0  THEN
         ---     EXIT WHILE
         --- END IF

         --- LET  ls_sale   =  0
         --- INPUT  ARRAY  la_fol
         ---      WITHOUT  DEFAULTS
         ---         FROM  scr_reg.*

         ---     BEFORE  FIELD  folio
         ---        IF   ls_sale  = 1   THEN 
         ---             NEXT FIELD  sel
         ---        END IF

         ---     AFTER  FIELD  folio 
         ---        IF   ld_fecha_conversion IS NULL  THEN
         ---             LET  ld_fecha_conversion =  Fnc_FechaFolio(la_fol[1].folio)
         ---             DISPLAY  ld_fecha_conversion  TO fecha_conversion
         ---             IF   ld_fecha_conversion    IS NULL  THEN 
         ---                  ERROR ' FOLIO INEXISTENTE ... ' 
         ---                  NEXT FIELD folio 
         ---             END IF 
         ---        END IF

         ---     ON KEY (CONTROL-C)
         ---        LET  ls_sale   =  0
         ---        EXIT INPUT

         ---     ON KEY (ESC)
         ---        IF   ld_fecha_conversion IS NULL  THEN
         ---             ERROR  ' ES NECESARIO CAPTURAR UN FOLIO VALIDO  ... '       
         ---             NEXT FIELD folio
         ---        ELSE 
         ---             LET  ls_sale   =  1
         ---        END IF
         ---        EXIT INPUT
         --- END INPUT

         INPUT  ls_folio
                WITHOUT DEFAULTS
          FROM  ls_folio

             AFTER  FIELD ls_folio
                IF   ls_folio        IS  NULL 
                 OR  ls_folio         =  0     THEN
                     LET  ls_sale     =  0
                     ERROR ' CAPTURE UN FOLIO VALIDO  ... ' 
                     NEXT  FIELD  ls_folio 
                END IF

             ON KEY (CONTROL-C)
                LET  ls_sale   =  0
                EXIT INPUT
                
             ON KEY (ESC)
                LET  ls_folio         =  Fnc_Folio(ls_folio)
                IF   ls_folio        IS  NULL  
                 OR  ls_folio         =  0     THEN
                     ERROR ' CAPTURE UN FOLIO VALIDO  ... ' 
                     NEXT FIELD          ls_folio
                ELSE 
                     LET  ls_sale   =  1
                END IF
                EXIT INPUT

         END INPUT

         IF  ls_sale    =  0  THEN

         ELSE
             CALL f_abre_ventana()
             --- FOR  i  =  1  TO 5
             ---      IF   la_fol[i].marca     =    'x'  THEN
                       LET  gc_lin_men        =  10
                       LET  gc_men            =  ' FOLIO CARGO ', ls_folio, ' '
                       DISPLAY "                                                            "  AT 5,5
                       CALL f_liquida(ls_folio)       
                       DISPLAY "                                                            "  AT 5,5
                       LET gc_men = ""
                       CALL f_act_estado_sol(ls_folio)
                       DISPLAY "                                                            "  AT 5,5
                       ### Se elimina desmarcar cuentas Nov 2011
                       ### CALL f_desmarca_cta(ls_folio)
                       ### DISPLAY "                                                   "  AT 5,5
                       EXECUTE  p_sel_fol       USING  ls_folio
                                                 INTO  ls_folio_abo
                       LET  gc_men            =  ' FOLIO ABONO ', ls_folio_abo, ' '
                       CALL f_liquida(ls_folio_abo)
                       DISPLAY "                                                            "  AT 5,5
                       ### No existe solicitud de folio abono 
                       ### CALL f_act_estado_sol(ls_folio_abo)
                       CALL f_act_folio(ls_folio, ls_folio_abo)   ##EFPS-211 -INI
                       DISPLAY " PROCESO TERMINADO                                 "  AT 5,5
                       SLEEP 3
                       LET   ls_sale   = 1
             ---      END IF
             --- END FOR
             CLOSE WINDOW RETC7102
         END IF
         EXIT WHILE
   END WHILE
   PROMPT " PROCESO FINALIZADO ... < Enter > PARA SALIR " FOR CHAR enter
   CLOSE WINDOW RETC7101
END FUNCTION

###########################################################
FUNCTION f_abre_ventana()
    OPEN WINDOW RETC7102 AT 4,4 WITH FORM "RETC7102" ATTRIBUTE(BORDER)
    DISPLAY "                                                                   RETIROS  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC7102      LIQUIDACION DE RECURSOS A DESINVERTIR                        " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)
END FUNCTION

###########################################################
FUNCTION f_liquida(ls_folio)
  DEFINE  ls_folio                    INTEGER 

  DISPLAY "LIQUIDANDO RECURSOS ... ", gc_men  AT gc_lin_men, 5
  LET  gc_lin_men  = gc_lin_men + 1
  SLEEP 3
   INSERT  INTO  dis_cuenta
   SELECT  *
     FROM  ret_preliquida
    WHERE  folio              =    ls_folio
      AND  tipo_movimiento    IN   (921, 922)
      AND  subcuenta NOT IN (4, 8) 
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
  DISPLAY  "CUENTAS DESMARCADAS ... ", gc_men, li_desm AT gc_lin_men,10
  LET  gc_lin_men  = gc_lin_men + 1
END FUNCTION

###########################################################
FUNCTION f_act_estado_sol(ls_folio)
  DEFINE  ls_folio            DECIMAL(20,0)
  DEFINE  la_rets             INTEGER
  DEFINE  lr_datos            RECORD
          nss                 LIKE safre_af:ret_solicitud_saldo.nss,
          id                  LIKE safre_af:ret_solicitud_saldo.id_solicitud_saldo
  END RECORD
  DEFINE  ls_cont             SMALLINT

  DISPLAY "IDENTIFICANDO SOLICITUDES ... ", gc_men  AT gc_lin_men, 5
  LET  la_rets   =  0
  LET  gc_lin_men  = gc_lin_men + 1

  FOREACH  d_sel_cue     USING  ls_folio,gr_edo.pre_liq
                          INTO  lr_datos.*
        EXECUTE  p_upd_sol         USING  gr_edo.liquidado,
                                          lr_datos.id,
                                          ls_folio,
                                          gr_edo.pre_liq
       LET  la_rets  =  la_rets + SQLCA.SQLERRD[3]
  END FOREACH
  DISPLAY "SOLICITUDES ACTUALIZADAS ... ", gc_men, la_rets AT gc_lin_men, 5
  LET  gc_lin_men  = gc_lin_men + 1

  ### No actualiza preliquida Nov 2011
  ### EXECUTE  p_upd_ret        USING  gr_edo.liquidado,
  ###                                  ls_folio,   
  ###                                  gr_edo.pre_liq
END FUNCTION

###########################################################
FUNCTION  Fnc_FechaFolio(ls_folio)
  DEFINE  ls_folio                     INTEGER 
  DEFINE  ld_fecha                     DATE 
  DEFINE  ls_encon                     SMALLINT

  LET     ls_encon    =   0  
  FOREACH   d_sel_fer    USING  ls_folio 
                          INTO  ld_fecha 
      LET ls_encon           =  ls_encon +  1 
  END FOREACH 

  IF  STATUS  =  NOTFOUND  
   OR ls_encon      =  0   THEN 
      LET ld_fecha  =  NULL
      ERROR  ' No existe el folio solicitado ' 
  END IF 
  RETURN  ld_fecha
END FUNCTION 

###########################################################
FUNCTION  Fnc_Folio(ls_folio)
  DEFINE  ls_folio                     INTEGER
  DEFINE  ls_encon                     SMALLINT

  LET       ls_encon         =  0
  FOREACH   d_exi_fol    USING  ls_folio
                          INTO  ls_folio
      LET ls_encon           =  ls_encon +  1
  END FOREACH

  IF  STATUS  =  NOTFOUND
   OR ls_encon               =  0   THEN
      LET ls_folio           =  NULL
      ERROR  ' No existe el folio solicitado '
  END IF
  RETURN  ls_folio
END FUNCTION

#EFPS-211 -INI
###########################################################
FUNCTION f_act_folio(p_folio_c, p_folio_a)
DEFINE    p_folio_c,
          p_folio_a   DECIMAL(15,0)

  UPDATE safre_af:ret_folio SET estado_folio = gr_edo.liquidado
  WHERE folio_cargo = p_folio_c
  AND folio_abono = p_folio_a
  AND estado_folio  = gr_edo.pre_liq

IF SQLCA.SQLERRD[3] != 1 THEN
   PROMPT "ERROR AL ACTUALIZAR FOLIO,AVISE A SISTEMAS ... < Enter > PARA CONTINUAR " FOR CHAR enter
END IF
END FUNCTION

#EFPS-211 -FIN