###############################################################################
#Proyecto          => Safre                                                   #
#Propietario       => E.F.P.                                                  #
#Modulo            => DIS.                                                    #
#Programa          => DISB090I                                                #
#Descripcion       => Genara pago 13 e inserta en dis_cuenta.                 #
#Fecha Inicio      => 27-abril-2005.                                          #
#Fecha Termino     =>                                                         #
#Autor             => GERARDO ALFONSO VEGA PAREDES                            #
###############################################################################

DATABASE safre_af

GLOBALS
   DEFINE g_fecha_corte DATE,
          g_usuario     CHAR(12)

   DEFINE vprecio_del_dia DECIMAL(22,14),
          vfactor         DECIMAL(22,14)

   DEFINE vfolio INTEGER

   DEFINE g_viv RECORD
      nss   CHAR(11),
      viv97 DECIMAL(22,6),
      viv92 DECIMAL(22,6) 
   END RECORD

   DEFINE vpart97 DECIMAL(22,6),
          vpart92 DECIMAL(22,6),
          vrem97  DECIMAL(22,6),
          vrem92  DECIMAL(22,6) 

   DEFINE nada CHAR(02)

END GLOBALS

MAIN

   CALL Inicializa()

   CALL Inserta_saldo()

END MAIN

FUNCTION Inicializa()

   LET g_fecha_corte = "03/31/2005"

   SELECT  USER
   INTO    g_usuario
   FROM    tab_afore_local

   SELECT precio_del_dia
   INTO   vprecio_del_dia
   FROM   glo_valor_accion
   WHERE  codigo_siefore = 11
   AND    fecha_valuacion = g_fecha_corte + 1

   LET vfactor = 0.04670936425562

   INSERT INTO glo_folio VALUES(0)
   SELECT MAX(folio)
   INTO   vfolio
   FROM   glo_folio

END FUNCTION

FUNCTION Inserta_saldo()

   DECLARE cur_ins CURSOR FOR
   SELECT *
   FROM   safre_tmp:tmp_saldo_viv

   FOREACH cur_ins INTO g_viv.*
      LET vpart97 = g_viv.viv97 * vfactor
      LET vrem97  = vpart97     * vprecio_del_dia

      LET vpart92 = g_viv.viv92 * vfactor
      LET vrem92  = vpart92     * vprecio_del_dia

      IF g_viv.viv97 = 0 AND g_viv.viv92 = 0 THEN
         LET nada = "no"
      ELSE
         IF g_viv.viv97 <> 0 AND g_viv.viv92 = 0 THEN
            INSERT INTO dis_cuenta VALUES(
               3,                  --tipo_movimiento
               4,                  --subcuenta
               11,                 --siefore
               vfolio,             --folio 
               0,                  --consecutivo
               g_viv.nss,          --nss
               null,               --curp
               null,               --folio_sua
               "04/01/2005",       --fecha_pago
               "04/01/2005",       --fecha_valor
               "04/01/2005",       --fecha_conversion
               vrem97,             --monto_en_pesos
               vpart97,            --moonto_en_acciones
               vprecio_del_dia,    --precio_accion 
               0,                  --dias_cotizados
               null,               --sucursal
               "REMANENTE",        --id_aportante
               6,                  --estado
               "04/01/2005",       --fecha_proceso
               g_usuario,          --usuario
               "04/01/2005",       --fecha_archivo
               1)                  --etiqueta
         END IF

         IF g_viv.viv97 = 0 AND g_viv.viv92 <> 0 THEN
            INSERT INTO dis_cuenta VALUES(
               3,                  --tipo_movimiento
               8,                  --subcuenta
               11,                 --siefore
               vfolio,             --folio
               0,                  --consecutivo
               g_viv.nss,          --nss
               null,               --curp
               null,               --folio_sua
               "04/01/2005",       --fecha_pago
               "04/01/2005",       --fecha_valor
               "04/01/2005",       --fecha_conversion
               vrem92,             --monto_en_pesos
               vpart92,            --moonto_en_acciones
               vprecio_del_dia,    --precio_accion
               0,                  --dias_cotizados
               null,               --sucursal
               "REMANENTE",        --id_aportante
               6,                  --estado
               "04/01/2005",       --fecha_proceso
               g_usuario,          --usuario
               "04/01/2005",       --fecha_archivo
               1)                  --etiqueta
         END IF

         IF g_viv.viv97 <> 0 AND g_viv.viv92 <> 0 THEN
            INSERT INTO dis_cuenta VALUES(
               3,                  --tipo_movimiento
               4,                  --subcuenta
               11,                 --siefore
               vfolio,             --folio
               0,                  --consecutivo
               g_viv.nss,          --nss
               null,               --curp
               null,               --folio_sua
               "04/01/2005",       --fecha_pago
               "04/01/2005",       --fecha_valor
               "04/01/2005",       --fecha_conversion
               vrem97,             --monto_en_pesos
               vpart97,            --moonto_en_acciones
               vprecio_del_dia,    --precio_accion
               0,                  --dias_cotizados
               null,               --sucursal
               "REMANENTE",        --id_aportante
               6,                  --estado
               "04/01/2005",       --fecha_proceso
               g_usuario,          --usuario
               "04/01/2005",       --fecha_archivo
               1)                  --etiqueta

            INSERT INTO dis_cuenta VALUES(
               3,                  --tipo_movimiento
               8,                  --subcuenta
               11,                 --siefore
               vfolio,             --folio
               0,                  --consecutivo
               g_viv.nss,          --nss
               null,               --curp
               null,               --folio_sua
               "04/01/2005",       --fecha_pago
               "04/01/2005",       --fecha_valor
               "04/01/2005",       --fecha_conversion
               vrem92,             --monto_en_pesos
               vpart92,            --moonto_en_acciones
               vprecio_del_dia,    --precio_accion
               0,                  --dias_cotizados
               null,               --sucursal
               "REMANENTE",        --id_aportante
               6,                  --estado
               "04/01/2005",       --fecha_proceso
               g_usuario,          --usuario
               "04/01/2005",       --fecha_archivo
               1)                  --etiqueta
         END IF
      END IF

   END FOREACH

END FUNCTION

