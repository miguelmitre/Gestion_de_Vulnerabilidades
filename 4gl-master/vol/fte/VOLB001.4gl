#*********************************************************************#
#Proyecto      => safre_af                                            #
#Propietario   => E.F.P.                                              #
#Programa      => VOLB002.4gl                                         #
#Descripcion   => LIQUIDACION DE VOLUNTARIA VENTANILLA                #
#Elaborado por => LAURA EUGENIA CORTES GUZMAN                         #
#Fecha Elabora => 09 de Diciembre del 2005                            #
#Modificado por=> LAURA EUGENIA CORTES GUZMAN                         #
#Fecha Ult.Mod.=> 09 de Diciembre del 2005                            #
#Sistema       => VOL.                                                #
#*********************************************************************#
DATABASE  safre_af

GLOBALS
   DEFINE g_reg      RECORD
          folio              INTEGER,
          fecha_liquidacion  DATE
   END RECORD

   DEFINE gparam_dev RECORD LIKE seg_modulo.*

   DEFINE reg_1 RECORD LIKE    int_det_voluntaria.*

   DEFINE hoy                DATE,
          usuario            CHAR(08),
          opc                CHAR(01),
          letra              CHAR(01),
          ejecuta            CHAR(200),
          hora_inicial       CHAR(08),
          hora_final         CHAR(08),
          parametro          CHAR(3),
          vresultado         CHAR(40)

   DEFINE x_mes              INTEGER,
          x_ano              INTEGER,
          hoy2               DATE,
          dias               INTEGER

END GLOBALS
#*********************************************************************
MAIN
   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY CONTROL-I,
      COMMENT LINE LAST
      DEFER INTERRUPT

   CALL STARTLOG("VOLB002.log")

   SELECT *,
          USER
   INTO   gparam_dev.*, 
          usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "vol"

   LET hoy = TODAY

   OPEN WINDOW ap_vol0 AT 2,3 WITH 3 ROWS , 75 COLUMNS
   ATTRIBUTE(BORDER)
       MENU "LIQUIDACION "
            COMMAND "Voluntaria" "Liquidacion Voluntaria "
                    LET letra = "V"
                    EXIT MENU
            COMMAND "Complementaria" "Liquidacion Complentaria "
                    LET letra = "C"
                    EXIT MENU
            COMMAND "Salir" "Salir "
                    LET letra = "S"
                    EXIT MENU
       END MENU
   CLOSE WINDOW ap_vol0

   IF letra = "S" THEN
      EXIT PROGRAM
   END IF
   OPEN WINDOW ventana1 AT 2,3 WITH FORM "VOLB0021" ATTRIBUTE(BORDER)

   CASE letra
        WHEN 'V' DISPLAY " VOLB002            LIQUIDACION VOLUNTARIAS ",
                         "VENTANILLA                         " 
                         AT 3,1 ATTRIBUTE(REVERSE)
        WHEN 'C' DISPLAY " VOLB002          LIQUIDACION VOLUNTARIAS ",
                         "COMPLEMENTARIA                       "
                         AT 3,1 ATTRIBUTE(REVERSE)
   END CASE

   DISPLAY hoy USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

   MENU "LIQUIDACION " 
      COMMAND "Liquidacion" "Liquidacion de aportaciones "
         CALL liquidacion()
      COMMAND "Salir" "Salir de liquidacion ."
         EXIT MENU
   END MENU

   CLOSE WINDOW ventana1
END MAIN
#*********************************************************************
FUNCTION liquidacion()
   DEFINE enter  CHAR(1)

   LET INT_FLAG = FALSE

   LET g_reg.folio = NULL
   LET g_reg.fecha_liquidacion = TODAY

   INPUT BY NAME g_reg.folio,
                 g_reg.fecha_liquidacion WITHOUT DEFAULTS

      AFTER FIELD folio
         IF g_reg.folio IS NULL THEN
            ERROR "Este campo no puede ser nulo"
            NEXT FIELD folio
         END IF

	 SELECT UNIQUE "X" FROM int_det_voluntaria
         WHERE  folio = g_reg.folio
         AND    tipo_vol = letra

         IF SQLCA.SQLCODE <> 0 THEN
            IF letra = 'V' THEN
               PROMPT "FOLIO DE APORTACION VOLUNTARIA NO EXISTE" 
                      ATTRIBUTE (REVERSE)
                      FOR opc ATTRIBUTE (REVERSE)
            END IF
            IF letra = 'C' THEN
               PROMPT "FOLIO DE APORTACION COMPLEMENTARIA NO EXISTE" 
                      ATTRIBUTE (REVERSE)
                      FOR opc ATTRIBUTE (REVERSE)
            END IF
            NEXT FIELD folio
         END IF

         SELECT "X"
         FROM   int_det_voluntaria
         WHERE  folio = g_reg.folio
         AND    resul_operacion = "01"
         AND    estado = 1
         GROUP BY 1

         IF SQLCA.SQLCODE <> 0 THEN
            IF letra = 'V' THEN
               PROMPT "FOLIO DE APORTACION VOLUNTARIA NO TIENE REGISTROS ",
                      "PARA LIQUIDAR" ATTRIBUTE (REVERSE)
                      FOR opc ATTRIBUTE (REVERSE)
            END IF

            IF letra = 'C' THEN
               PROMPT "FOLIO DE APORTACION COMPLEMENTARIA NO TIENE REGISTROS ",
                      "PARA LIQUIDAR" ATTRIBUTE (REVERSE)
                      FOR opc ATTRIBUTE (REVERSE)
            END IF
            NEXT FIELD folio
         END IF

         IF letra = "V" THEN
            SELECT "X"
            FROM   dis_cuenta
            WHERE  folio = g_reg.folio
            AND    subcuenta = 10
            AND    tipo_movimiento = 1
            GROUP BY 1

            IF SQLCA.SQLCODE = 0 THEN
               PROMPT "FOLIO DE APORTACION VOLUNTARIA YA LIQUIDADO" 
                      ATTRIBUTE (REVERSE)
                      FOR opc ATTRIBUTE (REVERSE)
               NEXT FIELD folio
            END IF
            NEXT FIELD fecha_liquidacion
         ELSE
            SELECT "X"
            FROM   dis_cuenta
            WHERE  folio = g_reg.folio
            AND    subcuenta = 12
            AND    tipo_movimiento = 1
            GROUP BY 1

            IF SQLCA.SQLCODE = 0 THEN
               PROMPT "FOLIO DE APORTACION COMPLEMENTARIA YA LIQUIDADO" 
                         ATTRIBUTE (REVERSE)
                         FOR opc ATTRIBUTE (REVERSE)
               NEXT FIELD folio
            END IF
            NEXT FIELD fecha_liquidacion
         END IF

      AFTER FIELD fecha_liquidacion
         IF g_reg.fecha_liquidacion IS NULL THEN
            ERROR "La fecha de liquidacion no puede ser nulo"
            NEXT FIELD fecha_liquidacion
         END IF

         SELECT "X"
         FROM   glo_valor_accion
         WHERE  fecha_valuacion = g_reg.fecha_liquidacion
         AND    codigo_siefore  = 1
         GROUP BY 1

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR "Precio de accion no existe con la fecha capturada"
            NEXT FIELD fecha_liquidacion
         END IF

         ERROR ""

         EXIT INPUT
      ON KEY(INTERRUPT, CONTROL-C)
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT

   IF INT_FLAG THEN
      LET INT_FLAG = FALSE
      DISPLAY "                                                                         " AT 5,1
      CLEAR FORM
      CLEAR SCREEN
      RETURN
   END IF

   PROMPT "Desea ejecutar el proceso de liquidacion [S/N] ..." FOR opc

   IF opc MATCHES '[Ss]' THEN
      CALL ejecuta_liquidacion()

      ERROR ""
      PROMPT "PROCESO DE LIQUIDACION TERMINADO...<ENTER> para continuar"
	      FOR enter
   ELSE
      ERROR ""
      PROMPT "Proceso de liquidacion CANCELADO...<ENTER> para continual"
      FOR enter
   END IF

   DISPLAY "" AT 5,1

   CLEAR FORM
   CLEAR SCREEN
END FUNCTION
#*********************************************************************
FUNCTION ejecuta_liquidacion()

   DEFINE pos               INTEGER,
          consecutivo       INTEGER,
          x_id_aportante    CHAR(10),
          vprecio_accion    DECIMAL(16,6),
          x_monto_en_accion DECIMAL(16,6),
          vfecha_vol_ven    DATE,
          x_plaza           CHAR(3),
          x_num_sucursal    CHAR(3),
          x_sucursal        CHAR(6)

DEFINE x_siefore    SMALLINT,
       x_subcuenta  SMALLINT,
       x_usuario    CHAR(8),
       cla_spl    CHAR(500),
       sie_fo_re    SMALLINT

   ERROR "PROCESANDO LIQUIDACION"
   SLEEP 3

   DECLARE cur_sor1 CURSOR FOR
   SELECT *
   FROM   int_det_voluntaria a
   WHERE  a.folio = g_reg.folio
   AND    a.resul_operacion = "01"
   AND    a.estado    = 1
   FOR UPDATE

   FOREACH cur_sor1 INTO reg_1.*

      LET x_id_aportante = "VE-",reg_1.numero_sucursal USING "&&&"

      LET x_plaza = reg_1.num_plaza_cobro USING "&&&"
      LET x_num_sucursal = reg_1.numero_sucursal USING "&&&"

      LET x_sucursal = x_plaza,x_num_sucursal

      LET pos = pos + 1
      LET consecutivo = pos - 1

      IF letra = 'V' THEN
         LET sie_fo_re = 0
         SELECT a.codigo_siefore INTO sie_fo_re FROM cta_regimen a
         WHERE a.nss             = reg_1.nss
         AND   a.subcuenta       = 10

          LET x_monto_en_accion = 0
          LET vprecio_accion = 0

          SELECT precio_del_dia
          INTO   vprecio_accion
          FROM   glo_valor_accion
          WHERE  fecha_valuacion = g_reg.fecha_liquidacion
          AND    codigo_siefore  = sie_fo_re

          LET x_monto_en_accion = reg_1.monto_neto / vprecio_accion

         INSERT INTO dis_cuenta
         VALUES (1,                     ---tipo_movimiento
                 10,                    ---subcuenta
                 sie_fo_re,             ---siefore
                 reg_1.folio,           ---folio
                 consecutivo,           ---consecutivo_lote
                 reg_1.nss,             ---nss
                 " ",                   ---curp
                 " ",                   ---folio_sua
                 reg_1.fecha_aplicacion,---fecha_pago
                 reg_1.fecha_aplicacion,---fecha_valor
                 g_reg.fecha_liquidacion,       ---fecha_conversion
                 reg_1.monto_neto,      ---monto_en_pesos
                 x_monto_en_accion,     ---monto_en_acciones
                 vprecio_accion,        ---precio_accion
                 0,                     ---dias_cotizados
                 x_sucursal,            ---sucursal
                 x_id_aportante,        ---id_aportante
                 5,                     ---estado
                 TODAY,                 ---fecha_proceso
                 USER,                  ---usuario
                 NULL,                  ---fecha_archivo
                 0                      ---etiqueta
                )


       LET x_siefore = sie_fo_re
       LET x_subcuenta = 10
      ELSE
         ## Complementaria
         LET sie_fo_re = 0
         SELECT a.codigo_siefore INTO sie_fo_re FROM cta_regimen a
         WHERE a.nss             = reg_1.nss
         AND   a.subcuenta       = 12

          LET x_monto_en_accion = 0
          LET vprecio_accion = 0 

          SELECT a.precio_del_dia
          INTO   vprecio_accion
          FROM   glo_valor_accion a
          WHERE  a.fecha_valuacion = g_reg.fecha_liquidacion
          AND    a.codigo_siefore  = sie_fo_re

          LET x_monto_en_accion = reg_1.monto_neto / vprecio_accion

         INSERT INTO dis_cuenta
         VALUES (1,                     ---tipo_movimiento
                 12,                    ---subcuenta
                 sie_fo_re,             ---siefore
                 reg_1.folio,           ---folio
                 consecutivo,           ---consecutivo_lote
                 reg_1.nss,             ---nss
                 " ",                   ---curp
                 " ",                   ---folio_sua
                 reg_1.fecha_aplicacion,---fecha_pago
                 reg_1.fecha_aplicacion,---fecha_valor
                 g_reg.fecha_liquidacion,       ---fecha_conversion
                 reg_1.monto_neto,      ---monto_en_pesos
                 x_monto_en_accion,     ---monto_en_acciones
                 vprecio_accion,        ---precio_accion
                 0,                     ---dias_cotizados
                 x_sucursal,            ---sucursal
                 x_id_aportante,        ---id_aportante
                 5,                     ---estado
                 TODAY,                 ---fecha_proceso
                 USER,                  ---usuario
                 NULL,                  ---fecha_archivo
                 0                      ---etiqueta
                )


       LET x_siefore = sie_fo_re
       LET x_subcuenta = 12
      END IF

      UPDATE int_det_voluntaria
      SET    estado = 2
      WHERE  CURRENT OF cur_sor1
 
{
       LET cla_spl = "EXECUTE PROCEDURE crea_saldo_vol(",                
			                reg_1.folio,",",
					'"',reg_1.nss,'"',",",
					x_siefore,",",
					x_subcuenta,",",
					'"',reg_1.fecha_aplicacion,'"',",",
					'"',g_reg.fecha_liquidacion,'"',",",
					reg_1.monto_neto,",",
					x_monto_en_accion,",",
					'"',usuario,'"',")"
				
      LET cla_spl = cla_spl CLIPPED

      PREPARE claexe FROM cla_spl                                       
      EXECUTE claexe                                  

      UPDATE int_det_voluntaria
      SET    estado = 2
      WHERE  CURRENT OF cur_sor1
 
      SELECT fecha_vol_ven
      INTO   vfecha_vol_ven
      FROM   cta_ctr_cuenta
      WHERE  nss = reg_1.nss

      IF vfecha_vol_ven IS NULL OR
	 vfecha_vol_ven = " " THEN
         UPDATE cta_ctr_cuenta
         SET    fecha_vol_ven = g_reg.fecha_liquidacion
         WHERE  nss = reg_1.nss
      END IF

      UPDATE cta_ctr_cuenta
      SET    fecha_ult_general = g_reg.fecha_liquidacion
      WHERE  nss = reg_1.nss
}
      DISPLAY "REGISTROS LIQUIDADOS : ",pos USING "####&"
      AT 17,1
   END FOREACH

END FUNCTION
#*********************************************************************
