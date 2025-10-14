############################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                          #
#Propietario       => E.F.P.                                               #
#Programa ACRB024  => INCORPORAR REGISTROS DE DEVOLUCION ACRED             #
#Fecha creacion    => 21 DE ABRIL DE 1999                                  #
#Por               => MAURO MUNIZ CABALLERO                                #
#Fecha actualiz.   => 20 DE JULIO DE 2004                                  #
#Actualizacion     => MAURO MUNIZ CABALLERO                                #
#      Se adecua proceso para participaciones de vivienda                  #
#Sistema           => ACR                                                  #
#Actualizacion     => 05/Nov/2013  DMR   CPL-1434                          #
#Modificacion      => Se estandarizo version a MLM, STARTLOG personalizado #
#                     Se elimina validacion de 1 folio por mes para evitar #
#                     duplicidad al provisionar y liquidar                 #
############################################################################
DATABASE safre_af

GLOBALS
   DEFINE
      cza      RECORD LIKE acr_cza_dev_cred.*,
      det      RECORD LIKE acr_det_dev_cred.*,
      sum      RECORD LIKE acr_sum_dev_cred.*

   DEFINE
      HOY             DATE

   DEFINE
      cod_maq         CHAR(03),
      aportante       CHAR(11),
      HORA            CHAR(10),
      aux_pausa       CHAR(01),
      usuario         CHAR(08)

   DEFINE
      vprecio_accion  DECIMAL(22,14),
      vfecha_archivo  CHAR(10),
      opc             CHAR(1),
      enter           CHAR(1),
      vfolio          INTEGER

END GLOBALS


MAIN
   OPTIONS PROMPT LINE LAST
           DEFER INTERRUPT

   CALL STARTLOG(FGL_GETENV("USER")||".ACRC024.log")

   DISPLAY "              INCORPORAR REGISTROS DE DEVOLUCIONES          " AT 12,10 ATTRIBUTE(REVERSE)

   WHILE TRUE 
      PROMPT " [T]ransferencia devolucion saldos credito 43bis =>  " FOR opc

      IF opc MATCHES '[tT]' THEN
         EXIT WHILE
      END IF
   END WHILE

   IF opc = "t" THEN
      LET opc = "T"
   END IF

   PROMPT "FOLIO A PROVISIONAR: " FOR vfolio

   SELECT "OK"
   FROM  acr_det_dev_cred
   WHERE folio = vfolio
   GROUP BY 1
 
   IF SQLCA.SQLCODE <> 0 THEN
      ERROR " Ese FOLIO NO EXISTE como proceso de Devolucion de Saldos..."
      SLEEP 3
      ERROR ""
   ELSE
      SELECT "OK"
      FROM  dis_provision
      WHERE folio = vfolio
      GROUP BY 1

      IF SQLCA.SQLCODE = 0 THEN
         ERROR " Ese FOLIO ya fue PROVISIONADO ... SE CANCELA PROCESO ..."
         SLEEP 3
         ERROR ""
      ELSE
         PROMPT "DESEA GENERAR EL PROCESO ? [S/N] " FOR aux_pausa

         IF aux_pausa MATCHES "[Ss]" THEN
            ERROR "PROCESANDO INFORMACION ... ESPERE UN MOMENTO" ATTRIBUTE(BOLD)

            CALL proceso_principal() #pp
         ELSE
            EXIT PROGRAM
         END IF

         PROMPT "PROCESO FINALIZO NORMALMENTE. [Enter] PARA SALIR" FOR aux_pausa
      END IF
   END IF
END MAIN


FUNCTION proceso_principal()
#pp-------------------------
   DEFINE
      HAY_REGISTROS        ,
      num_reg_procesados   ,
      TODOS                SMALLINT

   LET HORA = TIME
   LET HOY  = DATE

   SELECT COUNT(*)
   INTO   TODOS
   FROM   acr_det_dev_cred
   WHERE  folio  = vfolio
   AND    tipo_registro = "02"
   AND    estado = 1

   DISPLAY "REGISTROS A PROCESAR: ",TODOS AT 15,10

   DECLARE cursor_1 CURSOR FOR 
   SELECT HD.*,
          USER
   FROM   acr_det_dev_cred HD
   WHERE  HD.folio         = vfolio
   AND    HD.estado        = 1
   AND    HD.tipo_registro = "02"

   LET HAY_REGISTROS      = FALSE
   LET num_reg_procesados = 1

   FOREACH cursor_1 INTO det.*,usuario
      SELECT 'X'
      FROM  afi_mae_afiliado A
      WHERE @n_seguro = det.n_seguro

      IF SQLCA.SQLCODE = 0 THEN
         DISPLAY "Registros Procesados ",num_reg_procesados AT 17,10
         DISPLAY "N.S.S. ",det.n_seguro at 18,10

         CALL dispersa(det.*)

        {
          CALL dispersa(det.*,det.partic_v92,det.sal_viv_92,8,1)

          CALL dispersa(det.*,0,det.int_viv_97,4,4)

          CALL dispersa(det.*,0,det.int_viv_92,8,4)
        }
      END IF

      LET num_reg_procesados = num_reg_procesados + 1
   END FOREACH

   UPDATE acr_cza_dev_cred
   SET    estado = 2
   WHERE  folio = vfolio
   AND    tipo_registro = "01"
   AND    estado = 1

   UPDATE acr_det_dev_cred
   SET    estado = 2
   WHERE  folio = vfolio
   AND    tipo_registro = "02"
   AND    estado = 1

   UPDATE acr_sum_dev_cred
   SET    estado = 2
   WHERE  folio= vfolio
   AND    tipo_registro = "09"
   AND    estado = 1

   UPDATE acr_devol_cred
   SET    estado = 2
   WHERE  folio= vfolio
   AND    estado = 1
END FUNCTION


FUNCTION dispersa(x_historico)
#d----------------------------
   DEFINE
      x_historico     RECORD LIKE acr_det_dev_cred.*,
      g_sie           RECORD LIKE cta_regimen.*,
      reg_prov        RECORD LIKE dis_provision.*

   DEFINE
      i               SMALLINT,
      txt             CHAR(100)

   DEFINE
      valor_en_pesos  DECIMAL(18,6),
      vpesos_cta      DECIMAL(18,2)

   LET txt = " SELECT * ",
             " FROM   cta_regimen ms ",
             " WHERE  ms.nss = ","'",x_historico.n_seguro,"'",
             " AND    ms.subcuenta   = ?"

   PREPARE cla_exe FROM txt

   LET reg_prov.estado           = 5
   LET reg_prov.tipo_movimiento  = 1
   LET reg_prov.folio            = x_historico.folio
   LET reg_prov.nss              = x_historico.n_seguro
   LET reg_prov.curp             = ''
   LET reg_prov.folio_sua        = ''
   LET reg_prov.dias_cotizados   = 0
   LET reg_prov.sucursal         = ''
   LET reg_prov.fecha_proceso    = TODAY
   LET reg_prov.usuario          = usuario
   LET reg_prov.fecha_archivo    = cza.fecha_presentacion
   LET reg_prov.etiqueta         = 0
   LET reg_prov.id_aportante     = "DEV-INF"
   LET reg_prov.fecha_pago       = x_historico.fecha_mov_banxico
   LET reg_prov.fecha_valor      = x_historico.fecha_mov_banxico
   LET reg_prov.fecha_conversion = x_historico.fecha_mov_banxico

   LET reg_prov.subcuenta = 4

   DECLARE cursor_4 CURSOR FOR cla_exe

   FOREACH cursor_4 USING reg_prov.subcuenta INTO g_sie.*

      SELECT precio_del_dia
      INTO   vprecio_accion
      FROM   glo_valor_accion
      WHERE  fecha_valuacion = x_historico.fecha_mov_banxico
      AND    codigo_siefore  = g_sie.codigo_siefore

      LET reg_prov.monto_en_acciones = x_historico.partic_v97
      LET reg_prov.monto_en_pesos    = x_historico.sal_viv_97 + x_historico.int_viv_97   
      LET reg_prov.siefore           = g_sie.codigo_siefore
      LET reg_prov.precio_accion     = vprecio_accion

   END FOREACH

   LET reg_prov.consecutivo_lote = x_historico.cont_servicio

   IF reg_prov.monto_en_pesos IS NOT NULL AND
      reg_prov.monto_en_pesos > 0 THEN
      INSERT INTO dis_provision
      VALUES(reg_prov.*)

      IF STATUS < 0 THEN
         ERROR "Error al Insertar Provision Devolucion de Saldos ",STATUS
      END IF
   END IF
END FUNCTION

