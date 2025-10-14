###############################################################################
#Proyecto          => safre_af                                                #
#Owner             => E.F.P                                                   #
#Programa DISB011  => CALCULO INTERESE EST EN TRANSITO PROVISION              #
#Created           => 3 de octrubre de 2001.                                  #
#By                => GERAROD ALFONSO VEGA PAREDES.                           #
#Sistema           => DIS                                                     #
###############################################################################

DATABASE safre_af

GLOBALS
   DEFINE g_reg RECORD
      fecha_aplica   DATE,
      fecha_accion   DATE,
      precio_accion  DECIMAL(16,6),
      folios         CHAR(50),
      fecha_dispersa DATE,
      ctas_proc      INTEGER,
      num_nss        INTEGER,
      total_interes  DECIMAL(20,6)
   END RECORD

   DEFINE
      hist_fecha_aplica DATE,
      x_precio_accion   FLOAT,
      max_folio_int     INTEGER,
      i,cont,mes,ano    INTEGER,
      mes_tasa,ano_tasa INTEGER,
      dias_del_mes      INTEGER,
      hoy               DATE,
      opc               CHAR(1),
      usuario           CHAR(8),
      sw                SMALLINT,
      vfecha_valor      DATE

END GLOBALS

MAIN
   OPTIONS 
      PROMPT LINE last,
      INPUT WRAP,
      ACCEPT KEY CONTROL-I,
      COMMENT LINE LAST

    SELECT USER
    INTO   usuario
    FROM   glo_parametro

    LET HOY=TODAY

   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "DISB0261" ATTRIBUTE(BORDER)
   DISPLAY " DISB026       PROVISION INTERESES EST, RECURSOS EN TRANSITO                  " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY "                     Intereses recibidos de Operadora                                        " AT 6,1 ATTRIBUTE(REVERSE)
   DISPLAY "                     Aplicacion de Intereses por Afore                                            " AT 13,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)
   DISPLAY "Fecha Proceso : ",hoy USING "dd-mm-yyyy" AT 5,03 

   MENU "INTERESES EST " 
      COMMAND "Aplicar" "Aplicacion Intereses EST Transito"
         CALL agrega()
      COMMAND "Salir"   "Salir del Programa"
         EXIT PROGRAM
   END MENU
   CLOSE WINDOW ventana_1
END MAIN

FUNCTION agrega()

   DEFINE
      mes,dia,ano integer,
      vsubcor_int CHAR(4)

   INPUT BY NAME g_reg.* 
      BEFORE FIELD fecha_aplica
          LET g_reg.fecha_aplica = HOY
          DISPLAY BY NAME g_reg.fecha_aplica

      LET g_reg.precio_accion = 1
      LET x_precio_accion = 1

      AFTER FIELD folios
          EXIT INPUT
          
    END INPUT

   PROMPT "Desea dispersar intereses [S/N] ? " FOR opc

   IF opc MATCHES "[Ss]" THEN
      ERROR "Procesando Informacion ..."
      CALL dispersa_interes()
      ERROR ""
   ELSE
      RETURN
   END IF
END FUNCTION

FUNCTION dispersa_interes ()
   DEFINE
      l_reg RECORD LIKE cta_interes_rcv.*

   DEFINE l_cta RECORD
      nss       CHAR(11),
      subcuenta SMALLINT,
      folio     INTEGER
   END RECORD

   DEFINE l_mov RECORD
      nss              CHAR(11),
      consecutivo_lote INTEGER,
      folio	       INTEGER,
      subcuenta	       SMALLINT,
      fecha_valor      DATE,
      fecha_conversion DATE,
      monto_en_pesos   DECIMAL(16,6)
   END RECORD

   DEFINE 
      interes         FLOAT,
      x_aporte_pesos  FLOAT,
      x_tasa_mes      FLOAT,
      x_fecha_calculo DATE,
      x_fecha_final   DATE,
      vfecha_final    DATE,
      x_fecha_limite  DATE,
      x_fecha_tasa    DATE,
      x_fecha_dias    DATE,
      cont	      INTEGER,
      vfecha_gub1     CHAR(08),
      vfecha_gub2     CHAR(10),
      vfecha_gub      DATE

   LET g_reg.fecha_dispersa = g_reg.fecha_aplica
   LET g_reg.ctas_proc = 0
   LET g_reg.total_interes = 0

   DISPLAY BY NAME g_reg.fecha_dispersa,
                   g_reg.ctas_proc,
                   g_reg.total_interes

   DECLARE cur_mov CURSOR FOR
   SELECT nss,
          consecutivo_lote,
          folio,
          subcuenta,
          fecha_valor,
          fecha_conversion,
          sum(monto_en_pesos)
     FROM cta_interes_rcv
    WHERE folio           = g_reg.folios
      AND subcuenta       IN (5,6,9)
      AND tipo_movimiento = 3
    GROUP BY 1,2,3,4,5,6

   FOREACH cur_mov INTO l_mov.*

      LET interes = 0
      LET g_reg.ctas_proc     = g_reg.ctas_proc + 1

      DISPLAY BY NAME g_reg.ctas_proc

      LET x_fecha_limite = l_mov.fecha_valor
      LET x_fecha_final  = l_mov.fecha_conversion
      LET x_aporte_pesos = l_mov.monto_en_pesos

      LET ano = YEAR(l_mov.fecha_valor)
      LET mes = MONTH(l_mov.fecha_valor) + 1
      IF mes = 13 THEN
         LET mes = 1
         LET ano = YEAR(l_mov.fecha_valor) + 1
      END IF

      LET x_fecha_calculo = MDY(mes,1,ano)

      WHILE TRUE
         LET ano_tasa = YEAR(x_fecha_limite)
         LET mes_tasa = MONTH(x_fecha_limite) + 1
         IF mes_tasa = 13 THEN
            LET mes_tasa = 1
            LET ano_tasa  = YEAR(x_fecha_limite) + 1
         END IF
       
         LET x_fecha_tasa = MDY(mes_tasa,1,ano_tasa)

------     LET x_fecha_tasa = MDY(MONTH(x_fecha_limite),1,YEAR(x_fecha_limite))

         SELECT tasa_valor
         INTO   x_tasa_mes
         FROM   tab_tasa_ordinaria
         WHERE  tasa_fecha = x_fecha_tasa
         AND    tasa_origen = "RCV"


         IF MONTH(x_fecha_limite) = MONTH(x_fecha_final) AND 
            YEAR(x_fecha_limite) = YEAR(x_fecha_final)   THEN 

            IF MONTH(x_fecha_limite) = MONTH(g_reg.fecha_aplica) AND
               YEAR(x_fecha_limite) = YEAR(g_reg.fecha_aplica)   THEN
               LET x_fecha_calculo = x_fecha_final + 1
            END IF


            LET interes = interes + 
(((interes + x_aporte_pesos)*(x_fecha_final-x_fecha_limite)*(x_tasa_mes/36000))+
                 ((interes)*(x_fecha_calculo-x_fecha_final)*(x_tasa_mes/36000)))

            IF x_fecha_calculo = x_fecha_final+1 THEN
               EXIT WHILE
            ELSE
               LET x_aporte_pesos = 0
               LET x_fecha_final  = g_reg.fecha_aplica
            END IF
         ELSE
            LET interes = interes + ((interes + x_aporte_pesos) *
                          (x_fecha_calculo - x_fecha_limite) *
                          (x_tasa_mes/36000))
         END IF

         LET x_fecha_limite  = x_fecha_calculo

         LET ano = YEAR(x_fecha_calculo)
         LET mes = MONTH(x_fecha_calculo) + 1
         IF mes = 13 THEN
            LET mes = 1
            LET ano = YEAR(x_fecha_calculo) + 1
         END IF
         LET x_fecha_calculo = MDY(mes,1,ano)

      END WHILE

      LET l_reg.tipo_movimiento   = 3
      LET l_reg.subcuenta         = l_mov.subcuenta
      LET l_reg.siefore           = 1
      LET l_reg.folio             = l_mov.folio
      LET l_reg.consecutivo_lote  = l_mov.consecutivo_lote
      LET l_reg.nss               = l_mov.nss
      LET l_reg.curp              = ""
      LET l_reg.folio_sua         = 0
      LET l_reg.fecha_pago        = g_reg.fecha_aplica  
      LET l_reg.fecha_valor       = g_reg.fecha_aplica  
      LET l_reg.fecha_conversion  = x_fecha_calculo   --g_reg.fecha_accion  
      LET l_reg.monto_en_pesos    = interes
      LET l_reg.monto_en_acciones = interes / x_precio_accion
      LET l_reg.precio_accion     = x_precio_accion
      LET l_reg.dias_cotizados    = 0
      LET l_reg.sucursal          = ""
      LET l_reg.id_aportante      = "BANXICO"
      LET l_reg.estado            = 7
      LET l_reg.fecha_proceso     = hoy
      LET l_reg.usuario           = usuario
      LET l_reg.fecha_archivo     = hoy
      LET l_reg.etiqueta          = 0
    
      IF l_reg.monto_en_pesos <> 0 THEN
         INSERT INTO cta_interes_rcv VALUES (l_reg.*)
      END IF
    
      LET g_reg.total_interes = g_reg.total_interes + interes
   END FOREACH


END FUNCTION  # dispersa_interes


FUNCTION calcula_dias(fecha)
   DEFINE
      fecha DATE,
      dias  INTEGER

   CASE MONTH(fecha)            #Regresa dias del mes anterior
       WHEN  1 LET dias=31
       WHEN  2 IF YEAR(fecha) MOD 4 = 0 THEN
                  LET dias = 29
               ELSE
                  LET dias = 28
               END IF
       WHEN  3 LET dias=31
       WHEN  4 LET dias=30
       WHEN  5 LET dias=31
       WHEN  6 LET dias=30
       WHEN  7 LET dias=31
       WHEN  8 LET dias=31
       WHEN  9 LET dias=30
       WHEN 10 LET dias=31
       WHEN 11 LET dias=30
       WHEN 12 LET dias=31
   END CASE

   RETURN dias
END FUNCTION

{
display "interes  : ",interes
display " ";sleep 1
display "limite   : ",x_fecha_limite
display "calculo  : ",x_fecha_calculo
display "final    : ",x_fecha_final  
display "valor    : ",l_mov.fecha_valor 
display "conver   : ",l_mov.fecha_conversion
display "aplica   : ",g_reg.fecha_aplica
display "tasa     : ",x_fecha_tasa   
display " "; sleep 2
}
