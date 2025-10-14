#***************************************************************************#
#Proyecto          => Afores                                                #
#Owner             => E.F.P                                                 #
#Programa EXCB004  => INDIVIDUALIZACION INTERESES AFORE                     #
#Fecha             => 28 de marzo de 2001.                                  #
#Por               => MIGUEL ANGEL HERNANDEZ MARTINEZ.                      #
#Actualizado       => 17 de septiembre de 2001                              #
#Sistema           => EXC                                                   #
#***************************************************************************#
DATABASE safre_af

FUNCTION EXCB004(reg_1,
                 vmonto_devolver,
                 vfecha_aplica,
                 vtasa_aplica,
                 subcuenta)

   DEFINE HOY               DATE,
          vmes_calculo      SMALLINT,
          vfecha_calculo    DATE,
          vpesos            DECIMAL(16,6),
          vmonto_acum       DECIMAL(16,6),
          aux_subct_desc    CHAR(4),
          usuario           CHAR(8),
          i,
          cont              INTEGER,
          dias_del_mes      INTEGER,
          primero           INTEGER,
          vtasa_opera       DECIMAL(11,6),
          vint_aplica       DECIMAL(11,6),
          vprimera_vez      CHAR(01),
          vfecha_masunmes   DATE,
          vtipo_movimiento  SMALLINT,
          cla_sel           CHAR(300)

   DEFINE l_reg  RECORD LIKE exc_exceso_int_viv.*

   DEFINE reg_1  RECORD LIKE exc_det_exceso.*

   DEFINE vmonto_devolver   DECIMAL(16,6),
          vfecha_aplica     DATE,
          vtasa_aplica      DECIMAL(16,6),
          subcuenta         SMALLINT,
          vfecha_valor      DATE

   SELECT USER
   INTO   usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "exc"

   LET HOY = TODAY

#===========================================================================#
#dispersa: MOVIMIENTOS MESES ANTERIORES PENDIENTES DE CALCULO Y MES ACTUAL  #
#===========================================================================#

   LET vfecha_valor = reg_1.fecha_valor_viv

   LET cont = 1
   LET vint_aplica = 0
   LET vmonto_acum  = 0

   LET vmes_calculo = MONTH (vfecha_valor) + 1  

   IF vmes_calculo = 13 THEN
      LET vfecha_calculo = MDY(1,1,YEAR(vfecha_valor)+1)
   ELSE
      LET vfecha_calculo = MDY(vmes_calculo,1,YEAR(vfecha_valor))
   END IF

   LET vprimera_vez = "S"

   WHILE TRUE
      LET cont = cont + 1

      LET cla_sel = " SELECT tasa_valor ",
                    " FROM   tab_tasa_remanente ",
                    " WHERE  tasa_fecha = ","'",vfecha_calculo,"'",
                    " AND    tasa_origen = 'VIV'"

      PREPARE claexe FROM cla_sel

      DECLARE cur_proceso CURSOR FOR claexe

      OPEN cur_proceso

         FETCH cur_proceso INTO vtasa_opera

         IF SQLCA.SQLCODE <> 0 THEN
            LET vtasa_opera = vtasa_aplica
         END IF
      CLOSE cur_proceso

      CALL calcula_dias(vfecha_calculo)
           RETURNING dias_del_mes 

      IF vprimera_vez = "S" THEN
         LET vint_aplica = vmonto_devolver *
                          (vfecha_calculo - vfecha_valor) *
                          (vtasa_opera / 36000)

         LET vmonto_acum = vmonto_acum +
                           vint_aplica +       #intereses
                           vmonto_devolver     #aporte

         LET vprimera_vez = "N"
      ELSE 
         LET vfecha_masunmes = vfecha_calculo-1 UNITS MONTH

         LET vint_aplica = vmonto_acum *
                          (vfecha_calculo - vfecha_masunmes)*
                          (vtasa_opera / 36000)

         LET vmonto_acum = vmonto_acum +
                           vint_aplica        #intereses
      END IF

      LET l_reg.tipo_movimiento   = 3
      LET l_reg.subcuenta         = subcuenta
      LET l_reg.siefore           = 0
      LET l_reg.folio             = reg_1.folio
      LET l_reg.nss               = reg_1.nss
      LET l_reg.curp              = reg_1.curp
      LET l_reg.folio_sua         = reg_1.folio_pago_sua
      LET l_reg.fecha_pago        = vfecha_calculo 
      LET l_reg.fecha_valor       = vfecha_calculo 
      LET l_reg.fecha_conversion  = vfecha_aplica
      LET l_reg.monto_en_pesos    = vint_aplica
      LET l_reg.monto_en_acciones = 0
      LET l_reg.precio_accion     = 0
      LET l_reg.dias_cotizados    = 0
      LET l_reg.sucursal          = ""

      IF l_reg.subcuenta = 4 OR l_reg.subcuenta = 8 THEN
         LET l_reg.id_aportante   = "INFONAVIT"
      ELSE
         LET l_reg.id_aportante   = "BANXICO"
      END IF

      LET l_reg.estado            = 5
      LET l_reg.fecha_proceso     = HOY
      LET l_reg.usuario           = usuario
      LET l_reg.etiqueta          = 1

      LET l_reg.fecha_archivo = NULL

      INSERT INTO exc_exceso_int_viv
      VALUES (l_reg.*)

      IF vfecha_calculo >= vfecha_aplica THEN
         EXIT WHILE
      END IF

      LET vfecha_calculo = vfecha_calculo + 1 UNITS MONTH

   END WHILE

   LET l_reg.tipo_movimiento   = 888
   LET l_reg.subcuenta         = subcuenta
   LET l_reg.siefore           = 0
   LET l_reg.folio             = reg_1.folio
   LET l_reg.nss               = reg_1.nss
   LET l_reg.curp              = reg_1.curp
   LET l_reg.folio_sua         = reg_1.folio_pago_sua
   LET l_reg.fecha_pago        = vfecha_aplica
   LET l_reg.fecha_valor       = vfecha_aplica
   LET l_reg.fecha_conversion  = vfecha_aplica
   LET l_reg.monto_en_pesos    = vmonto_acum
   LET l_reg.monto_en_acciones = 0
   LET l_reg.precio_accion     = 0
   LET l_reg.dias_cotizados    = 0
   LET l_reg.sucursal          = ""

   IF l_reg.subcuenta = 4 OR l_reg.subcuenta = 8 THEN
      LET l_reg.id_aportante   = "SALDO INFONAVIT"
   ELSE
      LET l_reg.id_aportante   = "SALDO BANXICO"
   END IF

   LET l_reg.estado            = 5
   LET l_reg.fecha_proceso     = HOY
   LET l_reg.usuario           = usuario
   LET l_reg.etiqueta          = 0

   LET l_reg.fecha_archivo = NULL

   INSERT INTO exc_exceso_int_viv
   VALUES (l_reg.*)

   RETURN vmonto_acum

END FUNCTION  
#***************************************************************************
FUNCTION calcula_dias(fecha)

   DEFINE fecha      DATE

   CASE MONTH(fecha)            #Regresa dias del mes anterior
      WHEN 1  RETURN 31
      WHEN 2  RETURN 31
      WHEN 3

         IF YEAR(fecha) MOD 4 = 0 THEN
            RETURN 29
         ELSE
            RETURN 28
         END IF

      WHEN 4  RETURN 31
      WHEN 5  RETURN 30
      WHEN 6  RETURN 31
      WHEN 7  RETURN 30
      WHEN 8  RETURN 31
      WHEN 9  RETURN 31
      WHEN 10 RETURN 30
      WHEN 11 RETURN 31
      WHEN 12 RETURN 30
   END CASE
END FUNCTION
