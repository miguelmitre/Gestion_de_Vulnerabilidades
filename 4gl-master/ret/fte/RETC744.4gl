################################################################################
#Owner             => E.F.P.                                                   #
#Programa RETC744  => LIQUIDACION DE REINVERSION                               #
#Fecha creacion    => 16 DE OCTUBRE DE 2009                                    #
#Fecha Nvo Retiro  => Diciembre 2011                                           #
#By                => JAVIER GONZALEZ JERONIMO                                 #
#Fecha actualiz.   => Dic, 2011                                                #
#Actualizacion     => JGHM                                                     #
#Sistema           => Retiros, punto 2.5r, Reinversion                         #
################################################################################
#Actualizacion     => Se reestructura programa Alejandro Chagoya  18/feb/2012  #
#------------------------------------------------------------------------------#
#Modificacion      => MLM-1511                                                 #
#Fecha y Autor     => 21-11-2012    Alejandro Chagoya Salazar                  #
#Descripcion       => Se agregan validaciones para evitar duplicidad           #
################################################################################
#Modificacion      => Se desmarcan las cuentas de las marcas 921(si la tiene)  #
#                  => y la marca 922 al finalizar la liquidacion               #
#Autor             => Cristina Abasolo Tapia                                   #
#Fecha             => 27 Noviembre 2013                                        #
#Requerimiento     => MLM-2179                                                 #
################################################################################

DATABASE safre_af

GLOBALS
    DEFINE gr_edo RECORD
        pre_liq     LIKE ret_estado_issste.estado_solicitud ,
        liquidado   LIKE ret_estado_issste.estado_solicitud,
        reinvertido LIKE ret_estado_issste.estado_solicitud
    END RECORD

    DEFINE gr_folios RECORD
        fec_prel    DATE                         ,
        total       LIKE ret_solicitud_tx.folio ,
        parcial     LIKE ret_solicitud_tx.folio ,
        transfer    LIKE ret_solicitud_tx.folio
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
    DEFINE  g_enter                CHAR(1)

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP         ,
        PROMPT LINE LAST   ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG(FGL_GETENV("USER")||".RETC744.log")   #ACS-28012014
    CALL init() 
    CALL f_prepara_liq() 

END MAIN

FUNCTION init()
#i-------------
   DEFINE   lc_prepare      CHAR(300)

   LET HOY               =  TODAY
   SELECT  codigo_afore     ,  USER
     INTO  gs_codigo_afore  , gc_usuario
     FROM  tab_afore_local

   ----- ESTADOS DE S0LICITUD -----
   SELECT  A.estado_solicitud
     INTO  gr_edo.pre_liq
     FROM  ret_estado A
    WHERE  A.descripcion MATCHES "PRELIQUIDADO*REINVERSION"

   SELECT  A.estado_solicitud
     INTO  gr_edo.liquidado
     FROM  ret_estado A
    WHERE  A.descripcion MATCHES "LIQUIDADO*REINVERSION"

   SELECT  A.estado_solicitud
     INTO  gr_edo.reinvertido
     FROM  ret_estado A
    WHERE  A.descripcion MATCHES "REINVERTIDO"

   ----- DESMARCA -----
   LET lc_prepare = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"
   PREPARE eje_desmarca FROM lc_prepare

END FUNCTION

######################################################
FUNCTION f_prepara_liq()
   DEFINE  folio                  INTEGER 
   DEFINE  cuantos                INTEGER 

   LET   cuantos =  0

   OPEN WINDOW RETC7441 AT 4,4 WITH FORM "RETC7441"                                                ATTRIBUTE (BORDER)
   DISPLAY " <CTRL-C> SALIR                                             <ESC> EJECUTAR    " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " RETC744        LIQUIDACION DE REINVERSION                                    " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

   DISPLAY HOY TO FORMONLY.fecha

     INPUT BY NAME folio        WITHOUT DEFAULTS
     AFTER FIELD folio
         IF folio IS NULL THEN
            ERROR " ESTE CAMPO NO PUEDE SER NULO ..."
            NEXT FIELD folio
         END IF
        IF f_cuenta(folio) = 0 THEN
           ERROR "NO HAY REGISTROS CON ESTE FOLIO"
           NEXT FIELD folio
        END IF   
     ON KEY (CONTROL-C, INTERRUPT)
         PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR " FOR CHAR g_enter
         EXIT PROGRAM

     ON KEY (ESC)
       -- se obtiene el valor del folio  
         IF folio IS NULL THEN
            ERROR " ESTE CAMPO NO PUEDE SER NULO ..."
            NEXT FIELD folio
         END IF
         #MLM-1511 INI
         IF f_cuenta(folio) = 0 THEN
            ERROR "NO HAY REGISTROS CON ESTE FOLIO"
            NEXT FIELD folio
         END IF
         #MLM-1511 FIN

       LET cuantos = f_cuenta(folio)
       WHILE TRUE
           PROMPT "SE LIQUIDARAN ", cuantos ," CUENTAS, ESTA SEGURO S/N ? " FOR CHAR g_enter
           IF g_enter MATCHES "[sSnN]" THEN
              IF g_enter MATCHES "[sS]" THEN
                 IF f_liquida(folio) = 1 THEN     #MLM-1511 FIN
                    NEXT FIELD folio
                 END IF 
                 EXIT INPUT
              ELSE
                 ERROR " PROCESO CANCELADO... "
                 NEXT FIELD folio
              END IF
           ELSE
               ERROR " SOLO PRESIONE S o N ..."
           END IF
       END WHILE
 END INPUT

   CLOSE WINDOW RETC7441
END FUNCTION

###########################################
FUNCTION f_abre_ventana()
    OPEN WINDOW RETC7442 AT 4,4 WITH FORM "RETC7442" ATTRIBUTE(BORDER)
    DISPLAY "                                                                   RETIROS  " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC7442      LIQUIDACION DE REINVERSION                                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)
END FUNCTION

#########################################
FUNCTION f_liquida(ls_folio)
  DEFINE  ls_folio          INTEGER 
  DEFINE  l_folio_a         INTEGER
  DEFINE  l_nss             CHAR(11)
  DEFINE  l_consec          INTEGER
  DEFINE  l_msj             CHAR(500)
  DEFINE  r_marca   RECORD   LIKE safre_af:cta_act_marca.*
  DEFINE  l_cero SMALLINT,
          l_cod  SMALLINT
  LET l_cero = 0
  LET l_cod = 30

INITIALIZE l_nss, l_consec, l_folio_a,l_msj,r_marca.*  TO NULL

SELECT folio_abono INTO l_folio_a
FROM  ret_folio
WHERE folio_cargo = ls_folio
AND estado_folio = gr_edo.pre_liq

IF STATUS = NOTFOUND THEN
   ERROR "NO SE HA ENCONTRADO FOLIO DE ABONO, VERIFIQUE"
   SLEEP 2
   ERROR ""
   RETURN 1  #MLM-1511 FIN
END IF

  DISPLAY "FOLIO ABONO  ",l_folio_a   AT 13, 5
  DISPLAY "LIQUIDANDO RECURSOS ... "  AT 14, 5


INSERT  INTO  dis_cuenta
SELECT * FROM ret_preliquida
 WHERE folio IN(ls_folio, l_folio_a)
   AND tipo_movimiento IN (923, 924)

DECLARE cur_act CURSOR FOR
SELECT UNIQUE nss, consecutivo_lote
FROM dis_cuenta
 WHERE folio IN(ls_folio, l_folio_a)
   AND tipo_movimiento IN (923, 924)

FOREACH cur_act INTO l_nss, l_consec

  # INI MLM-2179 Se desmarca si tiene la marca 921
  SELECT * INTO r_marca.* 
  FROM cta_act_marca
  WHERE nss = l_nss 
  AND marca_cod = 921
  AND correlativo = l_consec

  IF STATUS <> NOTFOUND  THEN
     EXECUTE eje_desmarca USING
          l_nss,              --pnss 
          r_marca.marca_cod,  --pmarca_entra 
          l_consec,           --pcorrelativo 
          l_cod,              --pestado_marca
          l_cero,             --pmarca_causa 
          gc_usuario         --pusuario     
  ELSE
      LET l_msj = "NO SE HA ENCONTRADO LA MARCA 921 NSS: ", l_nss,
      " CORRELATIVO", l_consec
      LET l_msj = l_msj CLIPPED
      CALL ERRORLOG(l_msj)
  END IF

  INITIALIZE r_marca.* TO NULL
  
  SELECT * INTO r_marca.* 
  FROM cta_act_marca
  WHERE nss = l_nss 
  AND marca_cod = 922
  AND correlativo = l_consec

  # Se desmarca si tiene la marca 922
  IF STATUS <> NOTFOUND  THEN
     EXECUTE eje_desmarca USING
          l_nss,              --pnss 
          r_marca.marca_cod,  --pmarca_entra 
          l_consec,           --pcorrelativo 
          l_cod,              --pestado_marca
          l_cero,             --pmarca_causa 
          gc_usuario          --pusuario     
  ELSE
      LET l_msj = "NO SE HA ENCONTRADO LA MARCA 922 NSS: ", l_nss,
      " CORRELATIVO", l_consec
      LET l_msj = l_msj CLIPPED
      CALL ERRORLOG(l_msj)
  END IF
  
  # FIN MLM-2179

  UPDATE ret_reinversion
  SET estado_solicitud = gr_edo.liquidado
  WHERE id_solicitud_saldo = l_consec
  AND estado_solicitud = gr_edo.pre_liq
  
  UPDATE ret_solicitud_saldo
  SET estado_solicitud = gr_edo.reinvertido
  WHERE nss = l_nss
  AND id_solicitud_saldo = l_consec
  AND estado_solicitud IN (106, 120)

END FOREACH 

    UPDATE ret_folio
    SET estado_folio = gr_edo.liquidado
    WHERE folio_cargo = ls_folio
    AND estado_folio = gr_edo.pre_liq

    PROMPT " PROCESO TERMINADO SATISFACTORIAMENTE ... <ENTER> PARA SALIR " FOR CHAR g_enter

RETURN 0
END FUNCTION

#########################################################
#Funcion que cuenta registros a liquidar
FUNCTION f_cuenta(p_folio)
DEFINE p_folio, l_cuenta INTEGER

LET l_cuenta = 0

SELECT COUNT(UNIQUE consecutivo_lote) INTO l_cuenta
FROM ret_preliquida
WHERE folio = p_folio
AND consecutivo_lote IN (SELECT id_solicitud_saldo FROM ret_reinversion
                         WHERE estado_solicitud = gr_edo.pre_liq )
AND tipo_movimiento in (923)

 RETURN l_cuenta
END FUNCTION 

