
#Modificacion Presentación Coppel
###########################################################################
#Proyecto          => AFORES                                              #
#Propietario       => E.F.P                                               # 
#Programa ACRB003  => INDIVIDUALIZACION INTERESES ACREDITADOS (230)       # 
#Fecha             => 3 DE ENERO DE 2001                                  #
#Por               => MAURO MUNIZ CABALLERO                               #
#Sistema           => ACR                                                 #
###########################################################################
DATABASE safre_af

GLOBALS

   DEFINE g_pend RECORD
      fecha_valor DATE,
      monto_pesos DECIMAL(16,6)
   END RECORD

   DEFINE g_pend1 RECORD LIKE dis_cuenta.*

   DEFINE l_reg RECORD LIKE dis_provision.*

   DEFINE g_reg	RECORD
      fecha_aplica     DATE,
      saldo_prom_oper  DECIMAL(16,6),
      tasa_oper	     DECIMAL(10,6),
      monto_int_oper   DECIMAL(16,6),
      ctas_proc	     INTEGER,
      saldo_prom_afore DECIMAL(16,6),
      tasa_afore       DECIMAL(10,6),
      monto_int_afore  DECIMAL(16,6),
      fecha_dispersa   DATE
   END RECORD

   DEFINE
      hist_fecha_aplica	DATE,
      HOY               DATE,
      vfecha_pend       DATE,
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
      max_folio_int     INTEGER,
      vmonto_en_pesos1  DECIMAL(16,6),
      aux_subct_desc    LIKE tab_subcuenta.subct_desc
         
   DEFINE
      opc	       CHAR(1),
      usuario	       CHAR(8),
      i, cont	       INTEGER,
      dias_del_mes     INTEGER,
      primero	       INTEGER,
      total_cuentas1   INTEGER,
      total_cuentas    INTEGER,
      vtasa_opera      DECIMAL(11,6),
      vint_aplica      DECIMAL(11,6),
      inpc_t1, inpc_t2 DECIMAL(12,6),
      vfactor	       DECIMAL(12,6),
      vprimera_vez     CHAR(01),
      vfecha_masunmes  DATE

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

   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "ACRB0031" ATTRIBUTE(BORDER) 
   DISPLAY " ACRB003                APLICACION INTERESES A SUBCUENTAS                      " AT 3,1 ATTRIBUTE(REVERSE)                                  
   DISPLAY " ACRB003         APLICACION INTERESES A SUBCUENTAS                             " AT 3,1 ATTRIBUTE(REVERSE)                                  
   DISPLAY "                     Intereses recibidos de Operadora                          " AT 6,1 ATTRIBUTE(REVERSE)                    

   DISPLAY "                     Validacion de Intereses por Afore                         " AT 13,1 ATTRIBUTE(REVERSE)              
   DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)            
   DISPLAY "Fecha Proceso : ",hoy USING "dd-mm-yyyy" AT 5,03            

	
   MENU "INTERESES " 
      COMMAND "Aplicar" "Aplicacion Intereses"
         CALL agrega()
      COMMAND "Salir"   "Salir del Programa"
         EXIT PROGRAM
   END MENU
   CLOSE WINDOW ventana_1
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
   WHERE  tasa_origen = 'VIV'

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

   INPUT BY NAME g_reg.* 
   
      BEFORE FIELD fecha_aplica
         LET g_reg.fecha_aplica = hist_fecha_aplica + dias_del_mes + 1
         LET g_reg.fecha_aplica = MDY(MONTH(g_reg.fecha_aplica),
                                      1,YEAR(g_reg.fecha_aplica))

         DISPLAY BY NAME g_reg.fecha_aplica

      AFTER FIELD fecha_aplica
         IF DAY(g_reg.fecha_aplica) <> 1 THEN
            ERROR "Los intereses deben aplicarse al primer dia del mes"
            NEXT FIELD fecha_aplica
         END IF

         NEXT FIELD saldo_prom_oper

      AFTER FIELD saldo_prom_oper
                        NEXT FIELD tasa_oper

      BEFORE FIELD tasa_oper
         SELECT tasa_valor
           INTO g_reg.tasa_oper
           FROM tab_tasa_ordinaria
          WHERE tasa_fecha = g_reg.fecha_aplica
            AND tasa_origen = 'VIV'

         IF SQLCA.SQLCODE = 0 THEN
            ERROR "Tasa ya ingresada a la fecha: ",
                   g_reg.fecha_aplica USING "dd/mm/yyyy"
         ELSE
            ERROR "Tasa aun no ingresada a la fecha: ",
                   g_reg.fecha_aplica USING "dd/mm/yyyy"
            SLEEP 3
            EXIT PROGRAM
         END IF

         DISPLAY BY NAME g_reg.tasa_oper

      AFTER FIELD monto_int_oper
            EXIT INPUT                                         
    END INPUT                                                  

   PROMPT "Desea dispersar intereses [S/N] ? " FOR opc

   IF opc MATCHES "[Ss]" THEN
      ERROR "Procesando Informacion ..."

      CALL dispersa_interes()

     ERROR ""                                                      
   END IF
END FUNCTION

FUNCTION dispersa_interes()
#di-------------------------
   DEFINE 
      vsubcuenta SMALLINT,
      vsubcor_int CHAR(04),
      l_reg RECORD LIKE dis_provision.* ,
      l_cta RECORD
         nss	       CHAR(11),
	 subcuenta     SMALLINT,
	 saldo_ind_sub DECIMAL(16,6)
      END RECORD

   DEFINE 
      saldo_ind_sub  DECIMAL(16,6),
      saldo_ind_sub1 DECIMAL(16,6),
      pesos1         DECIMAL(16,6),
      spdm_sub	     DECIMAL(16,6),
      vsaldo_tot     DECIMAL(16,6),
      monto_interes  DECIMAL(16,6),
      dias_mes       INTEGER,
      cla_sel        CHAR(500)

	DEFINE vfolio INTEGER

   #===========================================================================#
   #dispersa: MOVIMIENTOS MESES ANTERIORES PENDIENTES DE CALCULO Y MES ACTUAL  #
   #===========================================================================#

   SELECT MAX(folio)
   INTO   vfolio
   FROM   taa_folio
   WHERE  tipo = 7

   LET g_reg.tasa_afore = g_reg.tasa_oper
   LET vtasa_opera =g_reg.tasa_oper

   LET vfecha_pend = g_reg.fecha_aplica 

   LET cont = 1
 
      LET cla_sel = "SELECT * ",
                    "FROM dis_provision ",
                    "WHERE folio = ", vfolio," ",
                    "AND subcuenta IN (4,8) ",
                    "AND tipo_movimiento = 230 " CLIPPED
                    --"AND estado in (5,6) ",
                    --"AND fecha_valor = ","'",vfecha_pend,"'"," ",
                    --"AND etiqueta = 0 " CLIPPED
   
   PREPARE cla_exe FROM cla_sel
   DECLARE cursor_ant1 CURSOR FOR cla_exe

   FOREACH cursor_ant1 INTO g_pend1.*
      LET vmonto_acum = 0

      LET vfecha_calculo = g_pend1.fecha_valor

      LET vprimera_vez = "S"

      WHILE TRUE
         DISPLAY cont AT 1,36
         LET cont = cont + 1

         CALL calcula_dias(vfecha_calculo) RETURNING dias_del_mes 

         IF vprimera_vez = "S" THEN
            LET vint_aplica = g_pend1.monto_en_pesos *
                              dias_del_mes *
       	        	      (vtasa_opera / 36000)

       	    LET vmonto_acum = vmonto_acum +
	         	      vint_aplica +       #intereses
			      g_pend1.monto_en_pesos #aporte

            LET vprimera_vez = "N"
         ELSE 
            LET vfecha_masunmes = vfecha_calculo-1 UNITS MONTH
            LET vint_aplica = vmonto_acum *
                              (vfecha_calculo - vfecha_masunmes)*
       			      (vtasa_opera / 36000)

     	    LET vmonto_acum = vmonto_acum +
	   		      vint_aplica        #intereses
         END IF

         LET g_pend1.tipo_movimiento = 235
         LET g_pend1.monto_en_pesos  = vint_aplica

         INSERT INTO dis_provision VALUES (g_pend1.*)
	
         IF vfecha_calculo >= vfecha_pend THEN
             EXIT WHILE
         END IF
         LET vfecha_calculo = vfecha_calculo + 1 UNITS MONTH

      END WHILE

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

