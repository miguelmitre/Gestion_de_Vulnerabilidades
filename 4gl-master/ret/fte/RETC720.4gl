#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETC720  => LIQUIDACION DE DERECHO OTORGADO                           #
#Fecha creacion    => NOVIEMBRE DE 2011                                         #
#By                => JOSE GONZALO HERNANDEZ                                    #
#Fecha actualiz.   => 3 DE MAYO DE 2012                                         #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                     - El programa no realizaba la desmarca de las cuentas de  #
#                       transferencias una vez que este se liquidaba            #
#                     - Correcciones menores en la presentacion del programa    #
#Sistema           => RETIROS                                                   #
#################################################################################
#Requerimiento     => MLMVUA-4                                                  #
#Fecha y Autor     => 31-Oct-2012  Cristina Abasolo Tapia                       #
#Descripcion       => Se identifica el tipo de reinversión que afecte a la      #
#                     cuenta, previamente desinvertida                          #
#################################################################################
#Requerimiento     => MLM-2045                                                  #
#Fecha y Autor     => 15-Ago-2013  Cristina Abasolo Tapia                       #
#Descripcion       => Se sustituye el parametro de busqueda fecha por           #
#                     folio a liquidar.                                         #
#################################################################################
#Modificacion      => Se agrega la nueva marca para Reinversion 922 y se        #
#                  => desmarca la 921, cuando se inserta la solicitud           #
#Autor             => Cristina Abasolo Tapia                                    #
#Fecha             => 27 Noviembre 2013                                         #
#Requerimiento     => MLM-2179                                                  #
#################################################################################


DATABASE safre_af

GLOBALS
    #DEFINE gar_precio_acc ARRAY [20] OF RECORD #CPL-2001
    DEFINE gar_precio_acc ARRAY [99] OF RECORD
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE gr_edo RECORD
        pre_liq           LIKE ret_estado.estado_solicitud ,
        liquidado         LIKE ret_estado.estado_solicitud ,
        reinvertido       LIKE ret_estado.estado_solicitud ,
        liquidado_derecho LIKE ret_estado.estado_solicitud ,
        sol_reinversion   LIKE ret_estado.estado_solicitud ,
        liquida_saldo     LIKE ret_estado.estado_solicitud 
    END RECORD
    
    DEFINE m_edoRe RECORD
        tipo_a     SMALLINT,
        tipo_b     SMALLINT,
        tipo_c     SMALLINT,
        tipo_d     SMALLINT,
        tipo_s     SMALLINT,
        tipo_u     SMALLINT
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
    
    DEFINE  gs_cod_tramite        ,
            gs_flag               ,
            gs_codigo_afore       SMALLINT
    
    DEFINE  ga_fecha              DATE 
    DEFINE  ga_fol                ARRAY[5] OF RECORD 
            gs_fol                INTEGER  
    END RECORD  
    DEFINE  gc_query              CHAR(1500),
            gc_error              CHAR(1000)
    DEFINE m_zero                 SMALLINT
    DEFINE m_folio                INTEGER
    DEFINE m_cuantos              VARCHAR(15)
   
END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG(FGL_GETENV("USER")||".RETC720.log") 
    CALL init() 
    CALL f_despliega_info() 

END MAIN


FUNCTION init()
#i-------------
    DEFINE   lc_prepare      CHAR(300)

    LET HOY               =  TODAY
    LET m_zero = 0
    LET m_cuantos = 0
    
    CALL f_obtiene_precios_accion(HOY)
    
    SELECT  codigo_afore     ,
            USER
      INTO  gs_codigo_afore  ,
            gc_usuario
      FROM  tab_afore_local

    ----- CODIGO DE TRAMITE -----
    SELECT cod_tramite
    INTO   gs_cod_tramite
    FROM   tab_tipo_tramite
    WHERE  descripcion = "TRANSFERENCIA"

    ----- ESTADOS DE SOLICITUD -----
    SELECT  A.estado_solicitud
      INTO  gr_edo.pre_liq
      FROM  ret_estado A
     WHERE  A.descripcion = "PRELIQUIDADO DERECHO"

    SELECT  A.estado_solicitud
      INTO  gr_edo.liquidado
      FROM  ret_estado A
     WHERE  A.descripcion = "LIQUIDADO DERECHO"

    SELECT  A.estado_solicitud
      INTO  gr_edo.reinvertido
      FROM  ret_estado A
     WHERE  A.descripcion = "REINVERTIDO"
    
    SELECT A.estado_solicitud   #ACS 106
    INTO   gr_edo.liquida_saldo
    FROM   ret_estado A
    WHERE  A.descripcion = "LIQUIDADO SALDO"
     
     SELECT A.estado_solicitud 
     INTO   gr_edo.liquidado_derecho         #119
     FROM   ret_estado A
     WHERE A.descripcion = "LIQUIDADO TOTAL DERECHO"
     
     SELECT A.estado_solicitud 
     INTO   gr_edo.sol_reinversion     #140
     FROM   ret_estado A
     WHERE A.descripcion = "SOLICITUD REINVERSION"
     
     SELECT id_tipo_origen_reinv             #10
     INTO  m_edoRe.tipo_a 
     FROM ret_tipo_origen_reinv 
     WHERE desc_origen_reinv = "RETIRO TIPO A"
     
     SELECT id_tipo_origen_reinv             #11
     INTO  m_edoRe.tipo_b 
     FROM ret_tipo_origen_reinv 
     WHERE desc_origen_reinv = "RETIRO TIPO B"

     SELECT id_tipo_origen_reinv             #12
     INTO  m_edoRe.tipo_c
     FROM ret_tipo_origen_reinv 
     WHERE desc_origen_reinv = "RETIRO TIPO C"

     SELECT id_tipo_origen_reinv             #13
     INTO  m_edoRe.tipo_d 
     FROM ret_tipo_origen_reinv 
     WHERE desc_origen_reinv = "RETIRO TIPO D"

     SELECT id_tipo_origen_reinv             #14
     INTO  m_edoRe.tipo_s
     FROM ret_tipo_origen_reinv 
     WHERE desc_origen_reinv = "RETIRO TIPO S"

     SELECT id_tipo_origen_reinv             #15
     INTO  m_edoRe.tipo_u
     FROM ret_tipo_origen_reinv 
     WHERE desc_origen_reinv = "RETIRO TIPO U"


    ----- DESMARCA -----
    LET lc_prepare = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"
    PREPARE eje_desmarca FROM lc_prepare

    LET lc_prepare = " "
    
    ----- MARCA -----
    LET lc_prepare = " EXECUTE PROCEDURE safre_af:marca_cuenta(?,?,?,?,?,?,?,?)"
    PREPARE eje_marca FROM lc_prepare


    ----- INSERTA EDO CUENTA PENSIONADO -----
    LET lc_prepare = " EXECUTE FUNCTION fn_inserta_edo_cta_pen(?,?,?,?,?,?) "
    PREPARE eje_edo_cta_pen FROM lc_prepare


    LET   gc_query  =  ' SELECT  '||''' '''||', a.folio, COUNT(*)             ',
                       '   FROM  safre_af:ret_preliquida   a,                 ',
                       '         safre_af:ret_det_datamart b                  ',
                       '  WHERE  a.tipo_movimiento  IN (800,810,815,805,806)  ',  #CPL-2324
                       '    AND  a.nss                =  b.nss                ',
                       '    AND  a.consecutivo_lote   =  b.id_registro        ',
                       '    AND  b.estado             =  ', gr_edo.pre_liq,
                       '    AND  a.fecha_conversion   =  ?                    ',
                       '  GROUP BY 1, 2                         '
    PREPARE  p_sel_fecha        FROM  gc_query
    DECLARE  d_sel_fecha  CURSOR FOR  p_sel_fecha
 
    LET   gc_query  =  ' SELECT  folio_abono        ',
                       '   FROM  ret_folio          ',
                       '  WHERE  folio_cargo   =  ? '
    PREPARE  p_sel_fol          FROM  gc_query

    # CAT MLM-1652 Se modifica los queries que obtiene las solicitudes para liquidar, contemplando las
    #              que no presentan movimientos en ret_preliquida

    LET   gc_query  =  ' SELECT  UNIQUE(nss)      ,               ',
                       '     CASE WHEN tipo_retiro = "A" THEN 800 ',
                       '          WHEN tipo_retiro = "B" THEN 810 ', #CPL-2324 No se usa mas, no se modifica
                       '          WHEN tipo_retiro = "C" THEN 815 ',
                       '          WHEN tipo_retiro = "U" THEN 805 ',
                       '     END                  ,               ',
                       '         id_registro      ,               ',
                       '         0                ,               ',
                       '         0                                ',
                       '   FROM  ret_det_datamart                 ',
                       '  WHERE  folio            =  ?            ', 
                       '    AND  tipo_retiro IN ("A","B","C","U") ',
                       '    AND  estado           =  ', gr_edo.pre_liq,
                       '  ORDER  BY 1                             '
    PREPARE  p_sel_mar          FROM  gc_query
    DECLARE  d_sel_mar    CURSOR FOR  p_sel_mar  
    
    LET   gc_query  =  " SELECT  UNIQUE a.nss,                                ",
                       "         a.id_registro,                               ",
                       "         a.curp,                                      ",
                       "         a.tipo_retiro                                ",
                       "   FROM  safre_af:ret_det_datamart a                  ",
                       "   LEFT JOIN safre_af:ret_preliquida  b               ",
                       "   ON (a.id_registro = b.consecutivo_lote             ",
                       "   AND b.tipo_movimiento  IN (800,810,815,805,806)    ", #CPL-2324
                       "   AND a.nss         = b.nss )                        ",
                       "  WHERE a.folio              =  ?                     ",
                       "   AND  a.tipo_retiro IN ('A','B','C','U')            ",
                       "   AND  a.estado           =  ", gr_edo.pre_liq,
                       "  ORDER  BY 1                                         "
    PREPARE  p_sel_cue          FROM  gc_query
    DECLARE  d_sel_cue    CURSOR FOR  p_sel_cue  
    
    # CAT MLM-1652
 
    LET   gc_query  =  ' UPDATE  ret_det_datamart                  ',
                       '    SET  estado               =   ?        ',
                       '  WHERE  nss                  =   ?        ',
                       '    AND  id_registro          =   ?        ',
                       '    AND  estado               =   ?        '
    PREPARE  p_upd_sol          FROM  gc_query
 
    LET   gc_query  =  ' UPDATE  ret_trans_imss                    ',
                       '    SET  estado_solicitud     =   ?        ',
                       '  WHERE  nss                  =   ?        ',
                       '    AND  folio                =   ?        ',
                       '    AND  estado_solicitud     =   ?        '
    PREPARE  p_upd_ret          FROM  gc_query

    LET   gc_query  =  ' SELECT  UNIQUE a.fecha_conversion                    ',
                       '   FROM  safre_af:ret_preliquida   a,                 ',
                       '         safre_af:ret_det_datamart b                  ',
                       '  WHERE  a.tipo_movimiento  IN (800,810,815,805,806)  ',  #CPL-2324
                       '    AND  a.nss                =  b.nss                ',
                       '    AND  a.consecutivo_lote   =  b.id_registro        ',
                       '    AND  b.estado             =  ', gr_edo.pre_liq,
                       '    AND  a.folio              =  ?                    '
    PREPARE  p_sel_fec          FROM  gc_query
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

   DEFINE  la_fol                       ARRAY[3] OF RECORD
         marca                          CHAR(1),
         folio                          INTEGER
   END RECORD
   DEFINE  cuantos                      INTEGER 
   DEFINE  ls_sale                      SMALLINT
   DEFINE  ls_equis                     SMALLINT   

   FOR   i = 1 TO 3
         LET  la_fol[i].marca         =  NULL
         LET  la_fol[i].folio         =  NULL
   END FOR
   LET   cuantos                      =  NULL

   OPEN WINDOW RETC7201 AT 4,4 WITH FORM "RETC7201"                                                ATTRIBUTE (BORDER)
   DISPLAY " <CTRL-C> SALIR                                             <ESC> EJECUTAR    " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " RETC720        LIQUIDACION DE DERECHO OTORGADO                               " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

   #MLM-2045 Se implementa el cambio de la fecha por folio - CAT
   INPUT m_folio  WITHOUT DEFAULTS FROM folio

   AFTER FIELD folio
      IF m_folio IS NULL OR m_folio = 0 THEN
            ERROR "ERROR, INGRESE UN FOLIO VALIDO"
            NEXT FIELD folio
      END IF
      IF f_cuenta() = 0 THEN
          ERROR "NO EXISTE INFORMACION PARA ESTE FOLIO"
          NEXT FIELD folio
      END IF

   AFTER INPUT
      IF m_folio IS NULL OR m_folio = 0 THEN
            ERROR "ERROR, INGRESE UN FOLIO VALIDO"
            NEXT FIELD folio
      END IF
      IF f_cuenta() = 0 THEN
          ERROR "NO EXISTE INFORMACION PARA ESTE FOLIO"
          NEXT FIELD folio
      END IF

   ON KEY (ESC)

      IF m_folio IS NULL OR m_folio = 0 THEN
            ERROR "ERROR, INGRESE UN FOLIO VALIDO"
            NEXT FIELD folio
      END IF
      IF f_cuenta() = 0 THEN
          ERROR "NO EXISTE INFORMACION PARA ESTE FOLIO"
          NEXT FIELD folio
      END IF
      EXIT INPUT

   ON KEY (CONTROL-C, INTERRUPT)
      ERROR "PROCESO CANCELADO" SLEEP 2
      ERROR ""
      EXIT PROGRAM

   END INPUT

    WHILE TRUE
      PROMPT "SE LIQUIDARAN ", m_cuantos, " CUENTAS. ¿ESTA SEGURO? (S/N)  " FOR CHAR enter
      IF enter MATCHES "[SsNn]" THEN
        IF enter MATCHES "[Ss]" THEN
           CALL f_abre_ventana()
           DISPLAY "                                                        "  AT 5,5
           CALL f_liquida(m_folio)
           DISPLAY "                                                        "  AT 5,5
           CALL f_act_estado_sol(m_folio)
           DISPLAY "                                                        "  AT 5,5
           DISPLAY "                                                        "  AT 6,5
           #CALL f_desmarca_cta(m_folio)
           EXIT WHILE
        ELSE
           ERROR "PROCESO CANCELADO" SLEEP 2
           ERROR ""
           EXIT PROGRAM
        END IF   
      ELSE
        ERROR "SOLO PRESIONE S o N"
      END IF  
    END WHILE
   #MLM-2045
   
   PROMPT "PROCESO DE LIQUIDACION TERMINADO CORRECTAMENTE ... ENTER PARA FINALIZAR" FOR CHAR enter
   
   CLOSE WINDOW RETC7202
   CLOSE WINDOW RETC7201
END FUNCTION


FUNCTION f_abre_ventana()
    OPEN WINDOW RETC7202 AT 4,4 WITH FORM "RETC7202" ATTRIBUTE(BORDER)
    DISPLAY "                                                                   RETIROS  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC7202      LIQUIDACION DE DERECHO OTORGADO                              " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)
END FUNCTION


FUNCTION f_liquida(ls_folio)

  DEFINE  ls_folio                    INTEGER 
  
    DEFINE lr_reinversion  RECORD
        subcuenta           SMALLINT     ,
        siefore             SMALLINT     ,
        monto_acc           LIKE dis_cuenta.monto_en_acciones,
        monto_pesos         LIKE dis_cuenta.monto_en_pesos
    END RECORD

    DEFINE lr_exe_saldo RECORD 
        nss             LIKE ret_det_datamart.nss           ,
        siefore         SMALLINT                            ,
        grupo           SMALLINT                            ,
        fecha           DATE
    END RECORD
    
    DEFINE ls_consecutivo LIKE dis_cuenta.consecutivo_lote
    
    DEFINE
        ls_siefore_orig         SMALLINT

  DISPLAY "LIQUIDANDO DERECHO OTORGADO ...           " AT 5,5

  SET LOCK MODE TO WAIT;
  LOCK TABLE dis_cuenta IN EXCLUSIVE MODE;

      INSERT  INTO  dis_cuenta
      SELECT  *
        FROM  ret_preliquida
       WHERE  folio              =    ls_folio
         AND  tipo_movimiento    IN (800,805,806,810,815)   #CPL-2324
         AND  consecutivo_lote IN (SELECT id_registro FROM ret_det_datamart
                                   WHERE folio = ls_folio
                                   AND estado = gr_edo.pre_liq)

  UNLOCK TABLE dis_cuenta;
  SET LOCK MODE TO NOT WAIT;

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
  DISPLAY "DESMARCANDO CUENTAS ... " AT 5,5

  FOREACH  d_sel_mar                USING  ls_folio 
                                     INTO  lr_desmarca.*
        EXECUTE eje_desmarca        USING  lr_desmarca.*, 
                                           gc_usuario
        LET   li_desm = li_desm + 1
        DISPLAY  "CUENTAS DESMARCADAS ... ", li_desm AT 11,14
    
  END FOREACH
END FUNCTION


FUNCTION f_act_estado_sol(ls_folio)
  DEFINE  ls_folio            INTEGER
  DEFINE  la_rets             INTEGER
  DEFINE  la_marca            INTEGER
  DEFINE  li_sol_saldo        INT
  DEFINE  ls_tpo_retiro       SMALLINT
  DEFINE  lr_datos            RECORD
          nss                 LIKE  ret_preliquida.nss              ,
          consecutivo_lote    LIKE  ret_preliquida.consecutivo_lote , 
          curp                LIKE  dis_provision.curp              ,
          tipo_retiro         CHAR(1)
  END RECORD
  
  DEFINE  l_msj             CHAR(500)
  
  DEFINE  l_marca   RECORD   LIKE safre_af:cta_act_marca.*
  
    DEFINE  
        ls_cont             ,
        ls_estado           SMALLINT,
        ls_marca            SMALLINT,
        ls_cod_rechazo      SMALLINT

    DEFINE ld_mto_pesos_10 LIKE dis_cuenta.monto_en_acciones

    DEFINE  lr_desmarca       RECORD
            nss               LIKE  cta_his_marca.nss          ,
            movimiento        LIKE  cta_his_marca.marca_cod    ,
            consec            LIKE  cta_his_marca.correlativo  ,
            edo_causa         LIKE  cta_his_marca.estado_marca ,
            marca_causa       LIKE  cta_his_marca.marca_causa
    END RECORD

    DEFINE lr_trans_imss    RECORD LIKE ret_trans_imss.*
    
    DEFINE  l_cero SMALLINT
    
    INITIALIZE lr_trans_imss.*, l_marca.*, l_msj, li_sol_saldo TO NULL

  DISPLAY "IDENTIFICANDO DATAMART  ... " AT 5,5
  DISPLAY "DESMARCANDO CUENTAS ... " AT 6,5
  
  LET  la_rets   =  0
  LET  la_marca  =  0
  LET  l_cero    =  0
  
  FOREACH  d_sel_cue     USING  ls_folio
                          INTO  lr_datos.*
        
        LET ld_mto_pesos_10 = 0
        
        # INI MLM-2179 Se desmarca de Transferencia
        SELECT * INTO l_marca.* 
        FROM cta_act_marca
        WHERE nss = lr_datos.nss 
        AND marca_cod IN (800,805,806,810,815)  #CPL-2324
        
        IF STATUS <> NOTFOUND  THEN
           EXECUTE eje_desmarca USING
                lr_datos.nss,       --pnss 
                l_marca.marca_cod,  --pmarca_entra 
                l_marca.correlativo,--pcorrelativo 
                l_cero,             --pestado_marca
                l_cero,             --pmarca_causa 
                gc_usuario         --pusuario     
                
           LET  la_marca  =  la_marca + 1
        ELSE
            LET l_msj = "NO SE HA ENCONTRADO LA MARCA DE TRANSFERENCIA DEL NSS: ", lr_datos.nss,
            " CORRELATIVO", l_marca.correlativo
            LET l_msj = l_msj CLIPPED
            CALL ERRORLOG(l_msj)
        END IF
          
        SELECT NVL(SUM(monto_en_pesos),0)
        INTO   ld_mto_pesos_10
        FROM   dis_cuenta
        WHERE  nss      = lr_datos.nss
        AND    siefore  = 10

        INITIALIZE lr_trans_imss.* TO NULL
        SELECT *
        INTO   lr_trans_imss.*
        FROM   ret_trans_imss
        WHERE  nss          = lr_datos.nss
        AND    folio        = ls_folio
        AND    consecutivo  = lr_datos.consecutivo_lote
            
        IF ld_mto_pesos_10 <= 0 THEN
            -- Si ya no existe saldo en la siefore 10 se desmarca la solicitud de saldo
            -- y se coloca como reinvertido
            
            #LET ls_estado   = gr_edo.reinvertido
            
            LET lr_desmarca.nss             = lr_trans_imss.nss
            LET lr_desmarca.movimiento      = 921
            LET lr_desmarca.consec          = lr_trans_imss.consec_sol_saldo
            LET lr_desmarca.edo_causa       = 0
            LET lr_desmarca.marca_causa     = 0
            
            # CAT MLMVUA-4 Se actualiza ret_solicitud_saldo a estado = 119
            SET LOCK MODE TO WAIT;   #CPL-2218  Modo de espera y bloqueo exclusivo
            LOCK TABLE ret_solicitud_saldo IN EXCLUSIVE MODE;

                UPDATE ret_solicitud_saldo SET estado_solicitud = gr_edo.liquidado_derecho
                WHERE id_solicitud_saldo = lr_trans_imss.consec_sol_saldo            

            UNLOCK TABLE ret_solicitud_saldo;
            SET LOCK MODE TO NOT WAIT;   #CPL-2218
            # CAT MLMVUA-4 
            
            EXECUTE eje_desmarca        USING  lr_desmarca.*, 
                                               gc_usuario
        ELSE 
            #LET ls_estado   = gr_edo.liquidado 
            LET ls_marca = 922
            # CAT MLMVUA-4 
            
            CASE lr_datos.tipo_retiro
                  WHEN 'A'
                         LET ls_tpo_retiro = m_edoRe.tipo_a
                  WHEN 'B'   
                         LET ls_tpo_retiro = m_edoRe.tipo_b
                  WHEN 'C'
                         LET ls_tpo_retiro = m_edoRe.tipo_c
                  WHEN 'D'
                         LET ls_tpo_retiro = m_edoRe.tipo_d
                  WHEN 'S'
                         LET ls_tpo_retiro = m_edoRe.tipo_s
                  WHEN 'U'
                         LET ls_tpo_retiro = m_edoRe.tipo_u
            END CASE

           SELECT id_solicitud_saldo INTO li_sol_saldo
           FROM ret_solicitud_saldo
           WHERE nss = lr_trans_imss.nss
           AND estado_solicitud = gr_edo.liquida_saldo
           
           SELECT "X" FROM safre_af:ret_reinversion
           WHERE id_solicitud_saldo = lr_trans_imss.consec_sol_saldo
           GROUP BY 1
           
          IF SQLCA.SQLCODE = 100 AND li_sol_saldo IS NOT NULL THEN -- NO existe sol de reinversion para evitar duplicar

              SET LOCK MODE TO WAIT;
              LOCK TABLE ret_reinversion IN EXCLUSIVE MODE;

                  WHENEVER ERROR CONTINUE
                  
                  # Se inserta una solicitud en ret_reinversion
                  INSERT INTO ret_reinversion VALUES (m_zero,lr_trans_imss.consec_sol_saldo, CURRENT, ls_tpo_retiro, gr_edo.sol_reinversion)

                  IF SQLCA.SQLCODE != 0 THEN
                      LET gc_error = "NO EXISTE SOLICITUD EN ret_solicitud_saldo PARA EL NSS: ",lr_desmarca.nss CLIPPED, 
                                     " CON EL ID SALDO: ",lr_trans_imss.consec_sol_saldo
                      CALL ERRORLOG(gc_error CLIPPED)
                      CONTINUE FOREACH
                  END IF

                  WHENEVER ERROR STOP
              UNLOCK TABLE ret_reinversion;
              SET LOCK MODE TO NOT WAIT;

              #INI MLM-2179 CAT
              #Se inserta la marca 922
              DECLARE cur_marca CURSOR FOR eje_marca
              OPEN cur_marca USING 
                       lr_trans_imss.nss,                 #pnss           
                       ls_marca,                          #pmarca_entra   
                       lr_trans_imss.consec_sol_saldo,    #pcorrelativo   
                       m_zero,                            #pestado_marca  
                       m_zero,                            #pcodigo_rechazo
                       ls_marca,                          #pmarca_causa   
                       HOY,                               #pfecha_causa   
                       gc_usuario                         #pusuario  
              FETCH cur_marca INTO ls_marca, ls_cod_rechazo
              
              IF ls_cod_rechazo != 0 THEN
                 LET gc_error = "NO SE MARCÓ 922-REINVERTIDOS, NSS : ", lr_desmarca.nss CLIPPED
                 CALL ERRORLOG(gc_error CLIPPED)  
              END IF
              
              CLOSE cur_marca 
              #FIN MLM-2179
           END IF --NO existe sol de reinversion para evitar duplicar

        END IF

        LET ls_estado   = gr_edo.liquidado  

        -- Se inserta en la tabla de estado de cuenta
        EXECUTE eje_edo_cta_pen USING lr_trans_imss.nss         ,
                                      lr_trans_imss.regimen     ,
                                      gs_cod_tramite            ,
                                      HOY                       ,
                                      ls_folio                  ,
                                      lr_datos.consecutivo_lote
                                INTO  ls_cont

        -- Se actualiza datamart
        EXECUTE  p_upd_sol         USING  ls_estado      ,
                                          lr_datos.nss   ,
                                          lr_datos.consecutivo_lote,
                                          gr_edo.pre_liq

        -- Se actualiza ret_trans_imss
        EXECUTE  p_upd_ret        USING  ls_estado          ,
                                         lr_datos.nss       ,
                                         ls_folio           ,
                                         gr_edo.pre_liq

        LET  la_rets  =  la_rets + 1
        DISPLAY "CUENTAS DESMARCADAS ... ", la_marca AT 11,14
        DISPLAY "DATAMART  ACTUALIZADOS ... ", la_rets AT 12,14

  END FOREACH

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

#---------------------------------------------------------------------------#
# f_obtiene_precios_accion : Obtiene los precios de accion correspondientes #
#                            a la fecha indicada                            #
#---------------------------------------------------------------------------#
FUNCTION f_obtiene_precios_accion(fecha_precios)

    DEFINE #loc #date
        fecha_precios         DATE

    DEFINE #loc #char
        v_precios_accion      CHAR(100)

    DEFINE lr_precio_acc RECORD #loc #lr_precio_acc Arreglo para los precios_accion
        estado                SMALLINT     ,
        fecha                 DATE         ,
        siefore               SMALLINT     ,
        precio_dia            DECIMAL(16,6)
    END RECORD

    DEFINE #loc #char
        lc_mensaje            CHAR(100) ,
        lc_siefore            CHAR(002)

    DEFINE #loc #smallint
        li_cont               SMALLINT

    -- -----------------------------------------------------------------------------

    LET li_cont = 0

    LET v_precios_accion = " EXECUTE FUNCTION fn_verifica_precio_accion (?)"
    PREPARE eje_precios_accion FROM v_precios_accion

    DECLARE c_precios CURSOR FOR eje_precios_accion
    FOREACH c_precios USING fecha_precios
                      INTO lr_precio_acc.*

        -- Solo se valida que existan los precios de las siefores basicas (1 a 5)   #CPL-2001 y la 90
        IF ((lr_precio_acc.siefore <= 10 OR lr_precio_acc.siefore = 90) AND lr_precio_acc.estado <> 0) THEN
            LET lc_siefore = lr_precio_acc.siefore
            LET lc_mensaje = " FALTAN PRECIOS ACCION: DIA ", fecha_precios, ", SIEFORE: ", lc_siefore, " ... <ENTER> PARA SALIR " CLIPPED
            LET lc_mensaje = lc_mensaje CLIPPED
            PROMPT lc_mensaje FOR CHAR enter
            EXIT PROGRAM
        ELSE
            -- Se almacena cada precio de accion el lugar correspondiente
            -- dentro del arreglo de precios.
            LET li_cont                   = lr_precio_acc.siefore
            LET gar_precio_acc[li_cont].* = lr_precio_acc.*
        END IF

    END FOREACH

END FUNCTION


#################################################################################
# MLM-2045 - CAT 

FUNCTION f_cuenta()

LET m_cuantos = 0

  SELECT COUNT(*) INTO m_cuantos
  FROM  ret_det_datamart a
  WHERE a.folio = m_folio
  AND   a.estado = gr_edo.pre_liq
  AND   a.tipo_retiro IN ("A", "B", "C", "U")

RETURN m_cuantos

END FUNCTION
