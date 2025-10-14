##############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                           #
#Propietario       => E.F.P.                                                 #
#Programa ACRB023  => CARGA ARCHIVO DISPERSION DEVOLUCION DE SALDOS ACRED.   #
#Fecha             => 21 DE ABRIL DE 1999                                    #
#Por               => MAURO MUNIZ CABALLERO                                  #
#Fecha actualiz.   =>                                                        #
#Por               =>                                                        #
#Sistema           => ACR                                                    #
#Actualizacion     => 05/Nov/2013  DMR   CPL-1434                            #
#Modificacion      => Se estandarizo version a MLM, STARTLOG personalizado   #
#                     Se elimina validacion de 1 folio por mes para evitar   #
#                     duplicidad al provisionar y liquidar                   #
##############################################################################
DATABASE safre_af

GLOBALS
   DEFINE
      g_param_taa         RECORD LIKE seg_modulo.*,
      w_codigo_afore             LIKE tab_afore_local.codigo_afore 

   DEFINE
      reg_cza             RECORD LIKE acr_cza_dev_cred.*,
      reg_det             RECORD LIKE acr_det_dev_cred.*,
      reg_sum             RECORD LIKE acr_sum_dev_cred.*

   DEFINE
      reg_cza_dev_sal_cre RECORD LIKE safre_tmp:cza_dev_sal_cre.*,
      reg_det_dev_sal_cre RECORD LIKE safre_tmp:det_dev_sal_cre.*,
      reg_sum_dev_sal_cre RECORD LIKE safre_tmp:sum_dev_sal_cre.*

   DEFINE
      reg_pagos           RECORD
                             folio             INTEGER,
                             tipo_registro     CHAR(2),
                             ident_servicio    CHAR(2),
                             ident_pago        CHAR(16),
                             importe           CHAR(15),
                             fecha_liquidacion DATE,
                             impt_aport_acept  CHAR(15),
                             impt_inter_acept  CHAR(15),
                             estado            CHAR(1),
                             fecha_archivo     DATE
                          END RECORD

   DEFINE
      r_prin              RECORD 
                             folio             INTEGER,
                             fecha_recepcion   DATE,
                             hora_recepcion    CHAR(8),
                             estado            SMALLINT,
                             fecha_estado      DATE,
                             hora_estado       CHAR(08)
                          END RECORD

   DEFINE 
      HOY                 ,
      d_fecha             ,
      vfecha_aux          DATE
 
   DEFINE
      enter               CHAR(1),
      generar             CHAR(12),
      ejecuta             CHAR(30),
      c_fecha             CHAR(10),
      c_tipo_registro     CHAR(2),
      c_fech_creac_lote   CHAR(10),
      nom_arch_generado   CHAR(21),
      vfecha_archivo      CHAR(10),
      opc                 CHAR(1),
      vusuario            CHAR(8)

   DEFINE 
      vfolio              ,
      vcont_apo           ,
      vcont_int           INTEGER

END GLOBALS


MAIN
   OPTIONS INPUT WRAP,
           PROMPT LINE LAST,
           ACCEPT KEY CONTROL-I
           DEFER INTERRUPT

   CALL STARTLOG(FGL_GETENV("USER")||".ACRC023.log")
   CALL inicio()
   CALL primera_pantalla()
END MAIN


FUNCTION primera_pantalla()
#--------------------------
   OPEN WINDOW ventana_1 AT 3,3 WITH 20 rows, 76 COLUMNS ATTRIBUTE(BORDER)
   DISPLAY " ACRB023    CARGA REGISTROS DEVOLUCION SALDOS CREDITO 43 BIS                   " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY "                              < Ctrl-C > Salir                                 " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

   PROMPT "DESEA EJECUTAR EL PROCESO ? [S/N]..." FOR opc
   IF opc MATCHES "[Ss]" THEN
      ERROR "PROCESANDO INFORMACION ..."
      SLEEP 2
      ERROR ""
      CALL proceso_principal() 
   ELSE
      ERROR " PROCESO CANCELADO !!!"
      SLEEP 2
      EXIT PROGRAM
   END IF

   PROMPT " PROCESO FINALIZADO...     [Enter] para Terminar    " FOR enter
END FUNCTION


FUNCTION inicio()
#--------------
   INITIALIZE reg_cza.*  TO NULL
   INITIALIZE reg_det.*  TO NULL
   INITIALIZE reg_sum.*  TO NULL

   SELECT codigo_afore, user
   INTO   w_codigo_afore, vusuario
   FROM   tab_afore_local

   SELECT *
   INTO   g_param_taa.*
   FROM   seg_modulo
   WHERE  modulo_cod = 'acr'
   
   LET HOY = TODAY
END FUNCTION


FUNCTION proceso_principal()
#adcc-----------------------
   DEFINE 
      vfecha              DATE,
      vfecha_ban          DATE,
      xfec_pre            DATE,
      cfecha              CHAR(10),
      cfecha_ban          CHAR(10),
      vhora               CHAR(8),
      cont_reg            ,
      total_reg           INTEGER,
      fol_pre             INTEGER
 
   LET vhora = TIME

   LET r_prin.folio           = vfolio
   LET r_prin.fecha_recepcion = TODAY   
   LET r_prin.hora_recepcion  = TIME
   LET r_prin.estado          = 1       
   LET r_prin.fecha_estado    = TODAY   
   LET r_prin.hora_estado     = TIME    

   SELECT fecha_presentacion
   INTO xfec_pre
   FROM safre_tmp:cza_dev_sal_cre

   SELECT MAX(folio)
   INTO fol_pre
   FROM  acr_cza_dev_cred
   WHERE fecha_presentacion = xfec_pre

   IF fol_pre IS NOT NULL AND fol_pre > 0 THEN
      ERROR " YA EXISTE CARGA DE DATOS HISTORICOS CON ESA FECHA PRESENTACION, FOLIO ",fol_pre

      PROMPT " DESEA REALIZAR NUEVAMENTE LA CARGA : ? " FOR enter
      IF UPSHIFT(enter) <> "S" THEN
         RETURN
      END IF
   END IF

   CALL borra_folio_previo(fol_pre)

   INSERT INTO glo_folio      #folio
   VALUES (0)

   SELECT MAX(folio)               
   INTO vfolio             
   FROM glo_folio                

   INSERT INTO taa_folio
   VALUES(vfolio,11,hoy,vusuario)

   DISPLAY "F O L I O              : ",vfolio USING "#######"   AT 09,05
   SLEEP 3

   SELECT *
   INTO   reg_cza_dev_sal_cre.*
   FROM   safre_tmp:cza_dev_sal_cre

   INSERT INTO acr_cza_dev_cred
   VALUES (vfolio, reg_cza_dev_sal_cre.*, 1)

   LET vcont_apo = 00

   DECLARE cur_3 CURSOR FOR 
   SELECT * FROM safre_tmp:det_dev_sal_cre

   FOREACH cur_3 INTO reg_det_dev_sal_cre.*

      LET cont_reg = cont_reg + 1

      IF reg_det_dev_sal_cre.tipo_registro= "02" THEN
         LET vcont_apo = vcont_apo + 1

         DISPLAY "Regs. con Devolucion de Saldos :",vcont_apo USING "#######" AT 11,5

         INSERT INTO acr_det_dev_cred
         VALUES(vfolio, reg_det_dev_sal_cre.*)
      END IF

   END FOREACH

   SELECT sum_sal_viv_97
   INTO reg_pagos.impt_aport_acept
   FROM safre_tmp:sum_dev_sal_cre

   SELECT int_viv_97
   INTO reg_pagos.impt_inter_acept
   FROM safre_tmp:sum_dev_sal_cre

   LET reg_pagos.folio              = vfolio
   LET reg_pagos.tipo_registro      = "02"
   LET reg_pagos.ident_servicio     = reg_cza_dev_sal_cre.ident_servicio
   LET reg_pagos.ident_pago         = "             58" 
   LET reg_pagos.fecha_liquidacion  = reg_det_dev_sal_cre.fecha_mov_banxico
   LET reg_pagos.estado             = 1 
   LET reg_pagos.fecha_archivo      = reg_cza_dev_sal_cre.fecha_presentacion

   LET reg_pagos.importe            = reg_pagos.impt_aport_acept +
                                      reg_pagos.impt_inter_acept

   INSERT INTO acr_devol_cred
   VALUES( reg_pagos.folio,
           reg_pagos.tipo_registro,
           reg_pagos.ident_servicio,
           reg_pagos.ident_pago,
           reg_pagos.importe,
           reg_pagos.fecha_liquidacion,
           reg_pagos.impt_aport_acept,
           reg_pagos.impt_inter_acept,
           reg_pagos.estado,
           reg_pagos.fecha_archivo,
           TODAY )

   SELECT *
   INTO   reg_sum_dev_sal_cre.*
   FROM   safre_tmp:sum_dev_sal_cre

   INSERT INTO acr_sum_dev_cred
   VALUES(vfolio, reg_sum_dev_sal_cre.*, 1)
END FUNCTION


FUNCTION borra_folio_previo(fol_ante)
DEFINE
   fol_ante    INTEGER

   DELETE FROM acr_cza_dev_cred
   WHERE folio = fol_ante

   DELETE FROM acr_det_dev_cred
   WHERE folio = fol_ante

   DELETE FROM acr_sum_dev_cred
   WHERE folio = fol_ante

   DELETE FROM taa_folio
   WHERE folio = fol_ante
END FUNCTION

