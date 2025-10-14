###############################################################################
#Proyecto          => safre_af                                                #
#Owner             => E.F.P  					              # 
#Programa DISB009  => INDIVIDUALIZACION INTERESES VIVIENDA                    # 
#Fecha             => 17 SEPTIEMBRE 1998		                      #
#By                => GERARDO ALFONSO VEGA PAREDES                            #
#Sistema           => DIS   					              #
###############################################################################
DATABASE safre_af
GLOBALS
   -----  Variables de captura  -----
   DEFINE
      hist_fecha_aplica	DATE,

      g_reg RECORD
         fecha_aplica     DATE,
         saldo_prom_oper  DECIMAL(16,6),
         tasa_oper	  DECIMAL(10,6),
         monto_int_oper   DECIMAL(16,6),
         ctas_proc	  INTEGER,
         saldo_prom_afore DECIMAL(16,6),
         tasa_afore       DECIMAL(10,6),
         monto_int_afore  DECIMAL(16,6),
         fecha_dispersa   DATE
      END RECORD,
      dias_del_mes  INTEGER,
      max_folio_int INTEGER

   -----  Variables de calculo interes  -----
   DEFINE g_pend1 RECORD
      fecha_valor     DATE,
      nss             CHAR(11),
      curp	      CHAR(18),
      subcuenta	      SMALLINT,
      monto_pesos     DECIMAL(16,6),
      tipo_movimiento SMALLINT
   END RECORD,

   vfecha_pend        DATE,
   vmes_calculo       SMALLINT,
   vfecha_calculo     DATE,
   vtasa_opera        DECIMAL(11,6),
   vprimera_vez       CHAR(01),
   vfecha_masunmes    DATE,
   vpesos             DECIMAL(16,6),
   vmonto_acum        DECIMAL(16,6),
   vint_aplica        DECIMAL(16,6)

   -----  Variables de calculo saldo promedio  -----
   DEFINE
      cta_saldo_prom RECORD LIKE cta_saldo_prom.*,
      vsaldo_prom  DECIMAL(16,6),
      inpc_t1, 
      inpc_t2  DECIMAL(16,6),
      vfactor  DECIMAL(16,6)

   -----  Variables de tablas   -----
   DEFINE l_reg RECORD LIKE dis_cuenta.*

   -----  Variables por omision  -----
   DEFINE
      hoy     DATE,
      opc     CHAR(1),
      usuario CHAR(8)
         
   -----  Variables ciclicas  -----
   DEFINE
      i, 
      cont INTEGER

   DEFINE hora_inicial CHAR(8)
   DEFINE hora_final  CHAR(8)

END GLOBALS

MAIN
   OPTIONS 
      PROMPT LINE last,
      INPUT WRAP,
      ACCEPT KEY CONTROL-I,
      COMMENT LINE LAST

   SELECT USER
     INTO usuario
     FROM glo_parametro

   LET HOY=TODAY

   CALL agrega()
END MAIN

###############################################################################
FUNCTION agrega()
DEFINE
   mes,
   dia,
   ano integer,
   vsubcor_int CHAR(4),
   nom_spl     CHAR(150)


   LET hora_inicial = TIME

   DISPLAY "Hora inicial ...",hora_inicial

---------------------------------------------------------------
--------------------- CAPTURA DE DATOS ------------------------

--   LET g_reg.tasa_oper = 5.6786

--   LET g_reg.fecha_aplica = "07/01/2001"

---------------------------------------------------------------


   --------LOCK TABLE dis_cuenta IN EXCLUSIVE MODE

   LET nom_spl = "EXECUTE PROCEDURE dispersa_interes (",
						g_reg.tasa_oper,",",
						"'",
						g_reg.fecha_aplica,
						"'",",",
						"'",
						usuario,"'",
						")" 

   LET nom_spl = nom_spl CLIPPED

   PREPARE eje_spl FROM nom_spl
   EXECUTE eje_spl
--------      UNLOCK TABLE dis_cuenta

   ERROR "TERMINO CALCULO INTERESES SATISFACTORIAMENTE"

   ERROR " "

   LET hora_final = TIME

   DISPLAY "Hora final ...",hora_final
END FUNCTION
