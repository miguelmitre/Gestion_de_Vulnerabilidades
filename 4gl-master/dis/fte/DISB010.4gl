###############################################################################
#Proyecto          => SAFRE  ( MEXICO )                                       #
#Owner             => E.F.P  					              # 
#Programa DISB010  => INDIVIDUALIZACION DE COMISION POR SALDO                 #
#Fecha             => 12 Agosto 1997     				      #
#By                => HECTOR M. FERNANDEZ A.              		      #
#Actualizacion     => 7 Noviembre 2000   				      #
#By                => HECTOR M. FERNANDEZ A.              		      #
#Sistema           => DIS   					              #
#Fecha modify      =>  7 Marzo   2005.                                        #
#Autor             => Alejandro Ramirez                                       #
#Descripcion       => Adecuacion multisiefore                                 #
-------------------------------------------------------------------------------
#Fecha modifica    => 20 Diciembre 2005 Alejandro Ramirez                     #
#Descripcion       => (v3) Se ingresa la lectura a la siefore 3               #
-------------------------------------------------------------------------------
#Fecha modifica    => 25 Julio 2006     Alejandro Ramirez                     #
#Descripcion       => (v5) Sentencias para impedir se ejecuten 2 veces los pro#
-------------------------------------------------------------------------------
#Versión           => v6                                                      #
#Autor             => GERARDO ALFONSO VEGA PAREDES                            #
#Fecha             => 19 febrero 2008                                         #
#Descripción       => Adecuar programa para que tome n siefores               #
-------------------------------------------------------------------------------
#Versión           => v7                                                      #
#Autor             => GERARDO ALFONSO VEGA PAREDES                            #
#Fecha             => 11 JUNIO 2009.                                          #
#Descripción       => Se agrego cursor para buscar si ya exite la integracion #
#                  => ya que antes estaba con query y se tardaba mucho        #
#                  => tambien se agrego exit program para que no den mas de   #
#                  => enter y no se duplique la integracion y se cambio       #
#                  => "STATUS" por "SQLCA.SQLCODE"                            #
-------------------------------------------------------------------------------
DATABASE safre_af

GLOBALS 
   DEFINE h_reg RECORD LIKE cta_his_com_saldo.*

   DEFINE g_reg RECORD
                   fecha_aplica         DATE,
                   provision_siefore	DECIMAL(15,6),
                   ctas_proc		INTEGER,
                   posicion_total	DECIMAL(15,6),
                   fecha_corte   	DATE,
                   provision_afore	DECIMAL(15,6)
                END RECORD

   DEFINE
      hist_fecha_corte	    DATE,
      x_fecha_corte         DATE,
      HOY		    DATE,
      f_inicio	            DATETIME YEAR TO SECOND,
      USUARIO		    CHAR(8),
      opc                   CHAR(1),
      primero		    SMALLINT,
      i      		    SMALLINT,
      total_cuentas	    INTEGER,
      sentencia             CHAR(100),
      x_estado              SMALLINT

   DEFINE
      vprovision_siefore1   DECIMAL(15,6),   --v2
      vprovision_siefore2   DECIMAL(15,6),   --v2
      vprovision_siefore3   DECIMAL(15,6)    --v3

END GLOBALS


MAIN
   OPTIONS PROMPT LINE last,
           INPUT WRAP,
           ACCEPT KEY CONTROL-I,
           COMMENT LINE LAST
           DEFER INTERRUPT

   CALL startlog("disb010.log")

   CALL init()

   MENU "COMISION SOBRE SALDO" 

      COMMAND "Calcular"
              "Calcular Comision por Saldo individualizada"
              CALL calcula_comision()

      COMMAND "Integrar" 
              "Integrar comision calculada en Cuentas Individuales"
              CALL integra_comision()

      COMMAND "Historico" 
              "Consulta Historico de Provisiones"
              CALL despliega_historico()

      COMMAND "Salir"
	      "Salir del Programa"
              EXIT PROGRAM
   END MENU
   CLOSE WINDOW ventana_1
END MAIN


FUNCTION init()
   SELECT USER
   INTO   USUARIO
   FROM   glo_parametro

   LET HOY      = TODAY
   LET f_inicio = TODAY

   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "DISB0101" ATTRIBUTE(BORDER)
   DISPLAY " DISB010            INDIVIDUALIZACION DE COMISION POR SALDO                    " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY "                  Provision de Comision por Saldo Mensual                                    " AT 6,1 ATTRIBUTE(REVERSE)
   DISPLAY "              Conciliacion de Provision por Saldo Individual                                 " AT 11,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)
   DISPLAY "Fecha Proceso :     ",hoy USING "dd-mm-yyyy" AT 5,03 
END FUNCTION #in


FUNCTION calcula_comision()
   INITIALIZE g_reg.* TO NULL
   LET hist_fecha_corte = NULL

   SELECT MAX(fecha_corte), MIN(estado)
   INTO   hist_fecha_corte, x_estado 
   FROM   cta_his_com_saldo
 
   IF hist_fecha_corte IS NULL THEN
      SELECT MIN(fecha_corte), MIN(fecha_corte), 0
      INTO   hist_fecha_corte, x_fecha_corte, x_estado
      FROM   cta_corte_saldo

      LET primero = TRUE
   ELSE
      SELECT c.fecha_corte
      INTO   x_fecha_corte
      FROM   cta_corte_saldo c
      WHERE  c.fecha_aplica = hist_fecha_corte + 1
   END IF
 
   SELECT "x"                             --v5
   FROM   dis_ctrl_proceso                --v5
   WHERE  proceso_cod='DISB010C'          --v5
   AND    resultado like 'PROCE%'         --v5

   IF SQLCA.SQLCODE = NOTFOUND THEN       --v5
 
      CASE x_estado
         WHEN 0
            ERROR "ULTIMO PERIODO SIN CALCULAR: ", 
	          hist_fecha_corte USING "DD/MM/YYYY"
	          LET g_reg.fecha_corte = hist_fecha_corte
         WHEN 1
            ERROR "ULTIMO PERIODO SIN INTEGRAR: ", 
	          hist_fecha_corte USING "DD/MM/YYYY",
	          "  EJECUTAR OPCION Integrar Cuentas"
         WHEN 2
            ERROR "ULTIMO PERIODO INTEGRADO   : ",
	          hist_fecha_corte USING "DD/MM/YYYY"

	          SELECT c.fecha_corte
	          INTO   x_fecha_corte
	          FROM   cta_corte_saldo c
	          WHERE  c.fecha_aplica = hist_fecha_corte + 1

	          LET g_reg.fecha_corte = x_fecha_corte
      END CASE
      DISPLAY BY NAME g_reg.fecha_corte

      SELECT fecha_habil
      INTO   g_reg.fecha_aplica
      FROM   cta_corte_saldo
      WHERE  fecha_corte = g_reg.fecha_corte

--    LET g_reg.fecha_aplica = g_reg.fecha_corte + 1
    
      PROMPT "Desea verificar provision [S/N] ? " FOR opc
      IF opc MATCHES "[Ss]" THEN
         ERROR "Procesando Informacion ..."
         CALL dispersa_provision()
         ERROR ""
      ELSE
      END IF
   ELSE                                                             --v5
      PROMPT "HAY UN PROCESO EN EJECUCION, DAR ENTER PARA CONTINUAR..." for opc
   END IF                                                           --v5
END FUNCTION


FUNCTION inserta_proceso()  --v5
   DEFINE hora_inicial      CHAR(08)
   DEFINE hora_final        CHAR(08)
   DEFINE usuario           CHAR(08)

   LET hora_inicial = TIME
   LET hora_final   = NULL

   SELECT USER
   INTO   usuario
   FROM   seg_modulo
   WHERE  modulo_cod='dis'

   INSERT INTO dis_ctrl_proceso VALUES
      (TODAY,                   -- fecha_proceso
       "DISB010C",              -- proceso_cod
       1,                       -- etapa_cod
       hora_inicial,            -- hora_inicial
       hora_final,              -- hora_final
       "110",                   -- parametro1
       g_reg.fecha_aplica,      -- parametro2
       g_reg.fecha_corte,       -- parametro3
       NULL,                    -- parametro4
       NULL,          -------g_reg.monto_interes,     -- parametro5
       NULL,                    -- folio
       "PROCESANDO",            -- resultado
       usuario,                 -- usuario
       0)

   IF SQLCA.SQLCODE < 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso comis sdo ",STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF
END FUNCTION


FUNCTION dispersa_provision()
   DEFINE r_arr array[20] OF RECORD
                                siefore       SMALLINT,
                                provision_sie DECIMAL(15,6)    --v3
                             END RECORD

   DEFINE i SMALLINT

   LET g_reg.provision_siefore = 0

   DECLARE cur_sie CURSOR FOR
   SELECT codigo_siefore,
          0
   FROM   tab_siefore_local
   WHERE  codigo_siefore NOT IN (0,11,12,13)
   ORDER  BY 1

   LET i = 1

   FOREACH cur_sie INTO r_arr[i].*

      SELECT SUM(monto_comis_saldos / acc_circ_totales *
            (acc_circ_traba + acc_circ_traba_vol))
      INTO   r_arr[i].provision_sie
      FROM   glo_valor_accion
      WHERE  fecha_valuacion BETWEEN hist_fecha_corte AND g_reg.fecha_corte
      AND    codigo_siefore = r_arr[i].siefore

      LET g_reg.provision_siefore=g_reg.provision_siefore+r_arr[i].provision_sie
   
      LET i = i + 1
   END FOREACH

   IF g_reg.provision_siefore IS NULL THEN
      LET g_reg.provision_siefore = 0
   END IF

   DISPLAY BY NAME g_reg.fecha_corte,
                   g_reg.provision_siefore,
                   g_reg.posicion_total,
                   g_reg.ctas_proc,
                   g_reg.fecha_aplica

   ERROR ""
   WHILE TRUE
      PROMPT "Acepta parametros de calculo  [S/N] ? " FOR opc
      IF opc MATCHES "[SsNn]" THEN
         EXIT WHILE
      END IF
   END WHILE

   IF opc MATCHES "[Ss]" THEN
      CALL inserta_proceso()                       --v5

      ERROR "GENERANDO HISTORICO DE CONTROL ..."

      CALL crea_historico() #ch

      LET sentencia="nohup fglgo DISB010C.4gi 1>DISB010C.sal 2>DISB010C.err &"
      RUN sentencia
    
      ERROR "PROCESO EN EJECUCION, VERIFIQUE Historico PARA TERMINO"
   ELSE
      ERROR "PROCESO CANCELADO, VERIFIQUE CIFRAS DE SIEFORE"
   END IF
END FUNCTION


{  FUNCTION genera_saldos()
   LET hist_fecha_corte = NULL

   SELECT MAX(fecha_corte), MAX(estado)
   INTO   hist_fecha_corte, x_estado
   FROM   cta_his_com_saldo

   IF hist_fecha_corte IS NULL OR x_estado < 2  THEN
      ERROR "EXISTEN CALCULOS SIN INTEGRAR A dis_cuenta INDIVIDUALES"
   ELSE
      ERROR "ULTIMO PERIODO INTEGRADO: ",hist_fecha_corte USING "DD/MM/YYYY"

      WHILE TRUE
         PROMPT "Desea regenerar calculo de Saldos [S/N] ? " FOR opc

         IF opc MATCHES "[SsNn]" THEN
            EXIT WHILE
         END IF
      END WHILE

      IF opc MATCHES "[Ss]" THEN
         LET sentencia = "nohup fglgo DISB010S.4gi &"
         RUN sentencia

         ERROR "PROCESO EN EJECUCION"
      ELSE
         ERROR "PROCESO CANCELADO"
      END IF
   END IF
END FUNCTION #gs  } #EN REVISION


FUNCTION integra_comision()
   DEFINE xfecha_aplica DATE                                    --v5

   LET hist_fecha_corte = NULL

   SELECT MAX(fecha_corte), MIN(estado)
   INTO   hist_fecha_corte, x_estado 
   FROM   cta_his_com_saldo

   IF hist_fecha_corte IS NULL OR x_estado <> 1 THEN
      ERROR "NO EXISTEN CALCULOS POR INTEGRAR A dis_cuenta INDIVIDUALES"
   ELSE
      SELECT max(fecha_aplica)                               --v5
      INTO   xfecha_aplica                                   --v5
      FROM   cta_his_com_saldo                               --v5

      ERROR "BUSCANDO DATOS..."

      DECLARE cur_cta CURSOR FOR
      SELECT "x"                                             --v5
      FROM   dis_cuenta                                      --v5
      WHERE  tipo_movimiento = 110                           --v5
      AND    fecha_conversion = xfecha_aplica                --v5

      OPEN cur_cta
      FETCH cur_cta
      IF SQLCA.SQLCODE = NOTFOUND THEN                       --v5
         ERROR "ULTIMO PERIODO SIN INTEGRAR: ", 
	       hist_fecha_corte USING "DD/MM/YYYY"

         WHILE TRUE
            PROMPT "Desea integrar calculo en Cuentas Individuales [S/N] ? " FOR opc
            IF opc MATCHES "[SsNn]" THEN
               EXIT WHILE
            END IF
         END WHILE

         IF opc MATCHES "[Ss]" THEN
            LET sentencia = "nohup fglgo DISB010I.4gi &"
            RUN sentencia
	    ERROR "PROCESO EN EJECUCION, VERIFIQUE Historico PARA TERMINO"
            CLOSE cur_cta
            SLEEP 2
            EXIT PROGRAM   --v7
         ELSE
  	    ERROR "PROCESO CANCELADO,VERIFIQUE CIFRAS DE CALCULO EN Historico"
            CLOSE cur_cta
            SLEEP 2
            EXIT PROGRAM   --v7
         END IF
         ERROR ""
      ELSE                                                   --v5
         ERROR ""
         prompt "ESTE PERIODO YA HA SIDO INTEGRADO,ENTER PARA CONTINUAR" for opc
         CLOSE cur_cta
         EXIT PROGRAM   --v7
      END IF                                                 --v5
      CLOSE cur_cta
   END IF
END FUNCTION


FUNCTION crea_historico()
   LET h_reg.folio             = 1
   LET h_reg.siefore           = 1
   LET h_reg.subcuenta         = 0
   LET h_reg.fecha_corte       = g_reg.fecha_corte
   LET h_reg.fecha_aplica      = g_reg.fecha_aplica
   LET h_reg.posicion_total    = 0
   LET h_reg.provision_siefore = g_reg.provision_siefore
   LET h_reg.tasa_comis        = 0
   LET h_reg.ctas_proc         = 0
   LET h_reg.provision_afore   = 0
   LET h_reg.fecha_ini         = CURRENT YEAR TO SECOND
   LET h_reg.fecha_fin         = "" 
   LET h_reg.estado            = 0
   LET h_reg.usuario           = USUARIO

   DECLARE cur_comis CURSOR FOR
   SELECT siefore,                                     --v2
          subcuenta, 
          val_porcentaje
   FROM   dis_val_comision
   WHERE  tipo_comision = 110
   GROUP  BY 1,2,3                                     --v2
   ORDER  BY 1,2                                       --v2

   FOREACH cur_comis INTO h_reg.siefore,               --v2
                          h_reg.subcuenta, 
                          h_reg.tasa_comis

      CASE h_reg.siefore
         WHEN 1
            LET g_reg.provision_siefore = vprovision_siefore1    --v2
            EXIT CASE
         WHEN 2 
            LET g_reg.provision_siefore = vprovision_siefore2    --v2
            EXIT CASE
         WHEN 3
            LET g_reg.provision_siefore = vprovision_siefore3    --v3
            EXIT CASE
      END CASE

      SELECT "X"
      FROM   cta_his_com_saldo
      WHERE  siefore     = h_reg.siefore                      --v2 
      AND    subcuenta   = h_reg.subcuenta
      AND    fecha_corte = g_reg.fecha_corte
      AND    estado      = 0

      IF SQLCA.SQLCODE = NOTFOUND THEN
         INSERT INTO cta_his_com_saldo VALUES (h_reg.*)
      ELSE
         ERROR "REPROCESANDO CALCULO DEL MES"
         WHENEVER ERROR CONTINUE
            DELETE FROM cta_com_saldo
         WHENEVER ERROR STOP

	 UPDATE cta_his_com_saldo
	 SET    fecha_ini   = CURRENT YEAR TO SECOND
	 WHERE  siefore     = h_reg.siefore                   --v2 
         AND    subcuenta   = h_reg.subcuenta
	 AND    fecha_corte = g_reg.fecha_corte
	 AND    estado      = 0
      END IF
   END FOREACH
END FUNCTION


FUNCTION calcula_dias(fecha)
   DEFINE
      fecha		DATE,
      dias		SMALLINT

   CASE MONTH(fecha)            #Regresa dias del mes corriente
      WHEN  1   LET dias = 31
      WHEN  2   IF YEAR(fecha) MOD 4 = 0 THEN
                   LET dias = 29
                ELSE
                   LET dias = 28
                END IF
      WHEN  3   LET dias = 31
      WHEN  4   LET dias = 30
      WHEN  5   LET dias = 31
      WHEN  6   LET dias = 30
      WHEN  7   LET dias = 31
      WHEN  8   LET dias = 31
      WHEN  9   LET dias = 30
      WHEN 10   LET dias = 31
      WHEN 11   LET dias = 30
      WHEN 12   LET dias = 31
   END CASE

   RETURN dias
END FUNCTION


FUNCTION despliega_historico()
   DEFINE reg_h ARRAY[10000] OF RECORD
	     siefore	           SMALLINT,
	     subcuenta	           SMALLINT,
             fecha_corte           DATE,
             fecha_aplica  	   DATE,
             provision_siefore	   DECIMAL(20,6),
             provision_afore       DECIMAL(20,6),
             estado                SMALLINT
          END RECORD

   DEFINE pos	SMALLINT

   OPEN WINDOW vent_1 AT 07,02 WITH FORM "DISB0102" #ATTRIBUTE(BORDER)
   DISPLAY "                  Registro Historico de Provisiones Calculadas                             " AT 1,1 ATTRIBUTE(REVERSE)

   DECLARE cur_h CURSOR FOR
   SELECT siefore,
          subcuenta,
          fecha_corte,
          fecha_aplica,
          provision_siefore,
          provision_afore,
          estado
   FROM   cta_his_com_saldo
   ORDER  BY 3 DESC,1,2

   INITIALIZE reg_h TO NULL
   LET i = 1

   FOREACH cur_h INTO reg_h[i].*
      LET i = i + 1
   END FOREACH

   CALL SET_COUNT(i-1)
   DISPLAY ARRAY reg_h TO scr_h.*
      ON KEY (INTERRUPT)
         EXIT DISPLAY
   END DISPLAY

   CLOSE WINDOW vent_1
END FUNCTION

