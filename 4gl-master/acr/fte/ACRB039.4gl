##############################################################################
#Proyecto          => AFORE ( MEXICO )                                       #
#Propietario       => E.F.P.                                                 # 
#Programa ACRB018  => INDIVIDUALIZACION INTERESES VIVIENDA TRASP. AF-AF.     # 
#Fecha             => 31 DE ENERO DE 2001                                    #
#Autor             => MAURO MUNIZ CABALLERO                                  #
#Sistema           => TAA                                                    #
##############################################################################
DATABASE safre_af
GLOBALS
   DEFINE g_pend RECORD
      fecha_valor DATE,
      monto_pesos DECIMAL(16,6)
   END RECORD

   DEFINE g_pend1 RECORD
      fecha_valor     DATE,
      nss             CHAR(11),
      curp            CHAR(18),
      subcuenta	      SMALLINT,
      monto_pesos     DECIMAL(16,6),
      tipo_movimiento SMALLINT
   END RECORD

   DEFINE l_reg RECORD LIKE safre_tmp:tmp_interes_cred.*

   DEFINE g_reg	RECORD
      fecha_aplica     DATE,
      saldo_prom_oper  DECIMAL(16,6),
      tasa_oper	       DECIMAL(10,6),
      monto_int_oper   DECIMAL(16,6),
      ctas_proc	       INTEGER,
      saldo_prom_afore DECIMAL(16,6),
      tasa_afore       DECIMAL(10,6),
      monto_int_afore  DECIMAL(16,6),
      fecha_dispersa   DATE
   END RECORD

   DEFINE
      hist_fecha_aplica	DATE,
      HOY               DATE,
      vfecha_pend       DATE,
      vfecha_pend1      DATE,
      vmes_calculo      SMALLINT,
      vfecha_calculo    DATE,
      vfecha_inicial    DATE,
      vfecha_final      DATE,
      vpesos            DECIMAL(16,6),
      vsaldo_viv        DECIMAL(16,6),
      vsaldo_viv1       DECIMAL(16,6),
      vint_saldo        DECIMAL(16,6),
      vpesos_888        DECIMAL(16,6),
      vpesos_8881       DECIMAL(16,6),
      vmonto_int_acum   DECIMAL(16,6),
      vmonto_acum       DECIMAL(16,6),
      max_folio_int     INTEGER
         
   DEFINE
      opc              CHAR(1),
      usuario	       CHAR(8),
      i, cont	       INTEGER,
      dias_del_mes     INTEGER,
      primero	       INTEGER,
      total_cuentas1   INTEGER,
      vtasa_opera      DECIMAL(11,6),
      vint_aplica      DECIMAL(11,6),
      inpc_t1, inpc_t2 DECIMAL(12,6),
      vfactor	       DECIMAL(12,6),
      vtab_tasa        DECIMAL(16,6),
      vprimera_vez     CHAR(01),
      vfecha_masunmes  DATE

END GLOBALS

MAIN

    OPTIONS 
        PROMPT LINE last,
        INPUT WRAP,
        ACCEPT KEY CONTROL-I,
        COMMENT LINE LAST

    LET vfecha_pend1 = ARG_VAL(1)

    SELECT USER
    INTO usuario
    FROM glo_parametro

    LET HOY = TODAY

    CALL agrega()

END MAIN

FUNCTION agrega()
#a---------------

   DEFINE
      mes,
      dia,
      ano integer,
      vsubcor_int CHAR(4)

   SELECT MAX(tasa_fecha)
     INTO hist_fecha_aplica
     FROM tab_tasa_ordinaria
    WHERE tasa_origen = 'VIV'

   IF hist_fecha_aplica IS NULL THEN
      LET ano = YEAR(hoy)
      LET mes = MONTH(HOY)-1
      IF mes = 0 THEN
         LET mes = 12
         LET ano = YEAR(hoy)-1
      END IF
      LET hist_fecha_aplica = MDY(mes,1,ano)
      LET primero = TRUE
   END IF

   CALL calcula_dias(hist_fecha_aplica) RETURNING dias_del_mes

         LET g_reg.fecha_aplica = MDY(MONTH(HOY),1,YEAR(HOY))

         SELECT tasa_valor
	  INTO g_reg.tasa_oper
           FROM tab_tasa_remanente
          WHERE tasa_fecha = g_reg.fecha_aplica
            AND tasa_origen = 'VIV'
      
         IF SQLCA.SQLCODE = 0 THEN
            DISPLAY "Tasa ya ingresada a la fecha: ",
                   g_reg.fecha_aplica USING "dd/mm/yyyy"
	ELSE
            DISPLAY "Tasa aun no ingresada a la fecha: ",
                   g_reg.fecha_aplica USING "dd/mm/yyyy"
         END IF

       DISPLAY ""

      DISPLAY "Procesando Informacion ..."

       DISPLAY ""

      LET max_folio_int = 1

      CALL borra_cuentasint_tra()
      CALL dispersa_interes()

END FUNCTION

FUNCTION dispersa_interes()
#di-------------------------

   DEFINE 
      l_reg RECORD LIKE safre_tmp:tmp_interes_cred.*,
      l_cta RECORD
         nss	       CHAR(11),
	 subcuenta     SMALLINT,
	 saldo_ind_sub DECIMAL(16,6)
      END RECORD

   DEFINE 
      #saldo_ind_sub DECIMAL(16,6),
      pesos1         DECIMAL(16,6),
      spdm_sub	     DECIMAL(16,6),
      vsaldo_tot     DECIMAL(16,6),
      monto_interes  DECIMAL(16,6),
      dias_mes       INTEGER

   #===========================================================================#
   #dispersa: MOVIMIENTOS MESES ANTERIORES PENDIENTES DE CALCULO Y MES ACTUAL  #
   #===========================================================================#

   LET g_reg.tasa_afore = g_reg.tasa_oper

   LET vfecha_pend  = g_reg.fecha_aplica 

   LET cont = 1

   DECLARE cursor_ant1 CURSOR FOR
   SELECT cta.fecha_valor,
          cta.nss,
          cta.curp,
          cta.subcuenta,
          cta.monto_en_pesos,
          cta.tipo_movimiento
     FROM dis_cuenta cta, acr_det_tra_cred tra
    WHERE cta.fecha_conversion BETWEEN vfecha_pend and vfecha_pend1
      AND cta.subcuenta = 4
      AND cta.tipo_movimiento <> 3
      AND cta.nss = tra.nss_afore
      AND tra.estado = 0
    UNION
   SELECT ctav.fecha_valor,
          ctav.nss,
          ctav.curp,
          ctav.subcuenta,
          ctav.monto_en_pesos,
          ctav.tipo_movimiento
     FROM cta_saldo_viv ctav, acr_det_tra_cred trav
    WHERE ctav.nss = trav.nss_afore
    ORDER BY 2 asc, 6 desc

   FOREACH cursor_ant1 INTO g_pend1.*
      LET vmonto_acum = 0
      LET vmes_calculo = month(g_pend1.fecha_valor) + 1  

      IF vmes_calculo = 13 THEN
         LET vfecha_calculo = MDY(1,1,YEAR(g_pend1.fecha_valor)+1)
      ELSE
         LET vfecha_calculo = MDY(vmes_calculo,1,YEAR(g_pend1.fecha_valor))
      END IF

      LET vprimera_vez = "S"

      WHILE TRUE
         LET cont = cont + 1

         SELECT tasa_valor
           INTO vtasa_opera 
           FROM tab_tasa_remanente ---tab_tasa_ordinaria
          WHERE tasa_fecha = vfecha_calculo
            AND tasa_origen = 'VIV'

             IF STATUS=NOTFOUND THEN
                LET vtasa_opera =g_reg.tasa_oper
             END IF
    
         CALL calcula_dias(vfecha_calculo) RETURNING dias_del_mes 

         IF vprimera_vez = "S" THEN
            LET vint_aplica = g_pend1.monto_pesos *
                              (vfecha_calculo - g_pend1.fecha_valor) *
       	        	      (vtasa_opera / 36000)

       	    LET vmonto_acum = vmonto_acum +
	         	      vint_aplica +       #intereses
			      g_pend1.monto_pesos #aporte

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
         LET l_reg.subcuenta         = g_pend1.subcuenta
         LET l_reg.siefore           = 0
         LET l_reg.folio             = 4
         LET l_reg.nss               = g_pend1.nss
         LET l_reg.curp              = g_pend1.curp
         LET l_reg.folio_sua         = 0
         LET l_reg.fecha_pago        = g_reg.fecha_aplica  
         LET l_reg.fecha_valor       = vfecha_calculo 
         LET l_reg.fecha_conversion  = g_reg.fecha_aplica 
         LET l_reg.monto_en_pesos    = vint_aplica
         LET l_reg.monto_en_acciones = 0
         LET l_reg.precio_accion     = 0
         LET l_reg.dias_cotizados    = 0
         LET l_reg.sucursal          = ""
         LET l_reg.id_aportante      = "INFONAVIT"
         LET l_reg.estado            = 5
         LET l_reg.fecha_proceso     = HOY
         LET l_reg.usuario           = usuario
         LET l_reg.etiqueta          = 1
              
         INSERT INTO safre_tmp:tmp_interes_cred VALUES (l_reg.*)
	
         IF vfecha_calculo >= vfecha_pend THEN 
             EXIT WHILE
         END IF

         LET vfecha_calculo = vfecha_calculo + 1 UNITS MONTH

      END WHILE

      SELECT monto_en_pesos
        INTO vpesos
        FROM safre_tmp:tmp_interes_cred
       WHERE nss             = g_pend1.nss
         AND subcuenta       = g_pend1.subcuenta
         AND tipo_movimiento = 888

      IF STATUS = NOTFOUND THEN
         LET l_reg.tipo_movimiento   = 888
         LET l_reg.subcuenta         = g_pend1.subcuenta
         LET l_reg.siefore           = 0
         LET l_reg.folio             = max_folio_int
         LET l_reg.nss               = g_pend1.nss
         LET l_reg.curp              = g_pend1.curp
         LET l_reg.folio_sua         = 0
         LET l_reg.fecha_pago        = vfecha_pend 
         LET l_reg.fecha_valor       = vfecha_pend 
         LET l_reg.fecha_conversion  = vfecha_pend 
         LET l_reg.monto_en_pesos    = vmonto_acum
         LET l_reg.monto_en_acciones = 0
         LET l_reg.precio_accion     = 0
         LET l_reg.dias_cotizados    = 0
         LET l_reg.sucursal          = ""
         LET l_reg.id_aportante      = "SALDO INFONAVIT"
         LET l_reg.estado            = 5
         LET l_reg.fecha_proceso     = HOY
         LET l_reg.usuario           = usuario
         LET l_reg.etiqueta          = 1

         INSERT INTO safre_tmp:tmp_interes_cred VALUES (l_reg.*)

      ELSE
         IF g_pend1.tipo_movimiento = 888 THEN

            UPDATE safre_tmp:tmp_interes_cred 
               SET monto_en_pesos   = monto_en_pesos + vint_aplica,
                   fecha_valor      = vfecha_pend, 
                   fecha_pago       = vfecha_pend, 
                   fecha_conversion = vfecha_pend, 
                   fecha_proceso    = HOY,
                   etiqueta         = 1
             WHERE nss              = g_pend1.nss
               AND subcuenta        = g_pend1.subcuenta
               AND tipo_movimiento  = 888
         ELSE
            UPDATE safre_tmp:tmp_interes_cred 
               SET monto_en_pesos   = monto_en_pesos + vmonto_acum,
                   fecha_valor      = vfecha_pend, 
                   fecha_pago       = vfecha_pend, 
                   fecha_conversion = vfecha_pend, 
                   fecha_proceso    = HOY,
                   etiqueta         = 1
             WHERE nss              = g_pend1.nss
               AND subcuenta        = g_pend1.subcuenta
               AND tipo_movimiento  = 888
         END IF
      END IF
  
   END FOREACH

END FUNCTION  

FUNCTION calcula_dias(fecha)
#cd------------------------
   DEFINE
      fecha DATE

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

FUNCTION borra_cuentasint_tra()
   WHENEVER ERROR CONTINUE
     DELETE FROM safre_tmp:tmp_interes_cred
   WHENEVER ERROR STOP
END FUNCTION
