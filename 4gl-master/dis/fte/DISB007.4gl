################################################################################
#Project           => SAFRE (Mexico)
#Owner             => E.F.P.
#Programa DISB007  => LIQUIDACION DE APORTACIONES / GENERACION SALDOS CTA.IND
#Fecha creacion    => 1 DE ENERO DE 1997
#By                => J. DAVID HERNANDEZ O.
#Fecha actualiz.   => 10 AGOSTO 1997
#Actualizacion     => HECTOR M. FERNANDEZ A.
#Sistema           => DIS 
################################################################################

DATABASE safre_af
GLOBALS
   DEFINE g_sal ARRAY[500] OF RECORD
      folio          INTEGER,
      fecha_archivo  DATE,
      aportacion     DECIMAL(16,2),
      comision       DECIMAL(16,2),
      seleccion      CHAR(01)
   END RECORD,

   arr_c        SMALLINT,
   arr_l        SMALLINT,
   i            SMALLINT,
   vsaldo_pesos DECIMAL(16,2),
   totala       DECIMAL(16,2),
   totalc       DECIMAL(16,2)

   DEFINE l_depositos RECORD
	   num    SMALLINT,
 	   cuenta SMALLINT,
 	   valor  DECIMAL(16,6)
   END RECORD

   DEFINE
      hoy,
      fecha1,
      fecha2,
      z_fecha DATE

   DEFINE
      aux_pausa,
      tipo,
      uno,
      dos,
      tres,
      enter CHAR(1)

   DEFINE
      salir,
      cont,
      sigue	SMALLINT

   DEFINE
      tipo_desc CHAR(30)

   DEFINE
      aa,
      ab,
      ac,
      ad	DECIMAL(16,6)

   DEFINE
      importe_liquida,
      importe_comision,
      importe_total DECIMAL(16,6)

   DEFINE
      tipo_liquida,
      tipo_comision CHAR(2)

   DEFINE
      valor_accion DECIMAL(16,6)

   DEFINE
      total_accion DECIMAL(16,6),
      opc          CHAR(01),
      vcont        INTEGER,
	   vmenos		 SMALLINT,
      g_param RECORD LIKE dis_parametro.*,
      vsalida  CHAR(200)

  DEFINE prioridad CHAR(25)
  DEFINE vfecha_pro DATE

  DEFINE ejecuta CHAR(200)

END GLOBALS

MAIN
    OPTIONS INPUT WRAP,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT 
   
    CALL STARTLOG("DISB007.log")

   SELECT *
     INTO g_param.*
     FROM dis_parametro
 
    LET HOY = TODAY
	 WHENEVER ERROR CONTINUE

        OPTIONS PROMPT LINE LAST

   OPEN WINDOW v1 AT 3,3 WITH FORM "DISB0071" ATTRIBUTE(BORDER)

   DISPLAY " DISB007               LIQUIDACION DE MOVIMIENTOS                                 " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "dd/mm/yyyy" AT 3,67 ATTRIBUTE(REVERSE)

   DISPLAY "[ESC] P/Procesar" AT 5,61

   MENU "LIQUIDACION"
      COMMAND "Normal" "Liquidacion Archivo Normal"
         CALL Inicializa()
         CALL liquida()
      COMMAND "Aclaraciones" "Liquidacion Archivo Aclaraciones"
         CALL Inicializa()
	 --CALL Aclaraciones()
      COMMAND "Salir" "Salir del Programa"
         EXIT PROGRAM
   END MENU

   CLOSE WINDOW v0
END MAIN

FUNCTION liquida()


 INPUT BY NAME tipo,uno,dos,fecha1,fecha2

    BEFORE FIELD tipo
	    ERROR "Tipo de liquidacion por acreditar:  (A) = Aportacion/Cuota  (I) = Intereses"

	    AFTER FIELD tipo
          DISPLAY "                      " AT 5,39
	       IF tipo IS NULL THEN
		       ERROR "Ingrese Tipo de Liquidacion:  (A) = Aportacion/Cuota   (I) = Intereses"
	       	 NEXT FIELD tipo
		    END IF
                CASE tipo
                     WHEN "A"
                      LET tipo_desc = "Aportaciones / Cuotas"
                     WHEN "I" 
                      LET tipo_desc = "Liquidacion Intereses"
                END CASE
                DISPLAY tipo_desc CLIPPED AT 5,39 ATTRIBUTE(REVERSE)
	        ERROR "Marque con 'X' la Subcuenta a Liquidar" 

            AFTER FIELD uno
               IF uno = "X" THEN
                  CALL despliega_saldos(tipo,"RCV",fecha2)
                  CALL despliega_deposito(tipo,"RCV",fecha2) RETURNING sigue
                  NEXT FIELD fecha1
               END IF

            AFTER FIELD dos
               IF dos = "X" THEN
                  CALL despliega_saldos(tipo,"EST",fecha2)
                  CALL despliega_deposito(tipo,"EST",fecha2) RETURNING sigue
                  NEXT FIELD fecha1
               END IF

	    BEFORE FIELD fecha1
	        LET fecha1 = HOY
	        DISPLAY BY NAME fecha1

	    AFTER FIELD fecha1
	        IF fecha1 IS NULL THEN
		    ERROR "Ingrese fecha de Pago"
	       	    NEXT FIELD fecha1
		END IF

	    BEFORE FIELD fecha2
	        LET fecha2 = TODAY
	        DISPLAY BY NAME fecha2

	    AFTER FIELD fecha2
	        IF fecha2 IS NULL THEN
		    ERROR "Ingrese fecha de Liquidacion"
	       	    NEXT FIELD fecha2
		END IF

    ON KEY ( ESC )
       LET salir = FALSE

	    IF uno IS NULL AND dos IS NULL AND tres IS NULL THEN
		    ERROR " A LO MENOS DEBE INGRESAR UNA OPCION"
		    NEXT FIELD uno
		 END IF
		 
		 IF fecha1 IS NULL THEN
		    ERROR "INGRESE FECHA DE PROCESO"
	       NEXT FIELD fecha1
		 END IF

		 IF fecha2 IS NULL THEN
		    ERROR "INGRESE FECHA DE LIQUIDACION"
	       NEXT FIELD fecha2
		 END IF

	    LET cont = 0

	    IF uno = "X" THEN
          LET cont = cont + 1 
       END IF
	    IF dos = "X" THEN
          LET cont = cont + 1
       END IF
	    IF tres = "X" THEN
          LET cont = cont + 1
       END IF

	    IF cont > 1 THEN
          ERROR"PUEDE PROCESAR UNA SOLA ACREDITACION A LA VEZ"
	       NEXT FIELD uno
	    END IF

       SELECT precio_del_dia
       INTO   valor_accion
       FROM   glo_valor_accion
       WHERE  fecha_valuacion = fecha2

       IF STATUS = NOTFOUND THEN
          ERROR "No existe valor de accion para la fecha de liquidacion : ",fecha2 USING "dd/mm/yyyy"
          NEXT FIELD fecha2
       END IF

       CASE
          WHEN uno = "X" 
             CALL despliega_deposito(tipo,"RCV",fecha2) RETURNING sigue
             IF NOT sigue THEN
                NEXT FIELD uno
             END IF
          WHEN dos = "X"
             CALL despliega_deposito(tipo,"EST",fecha2) RETURNING sigue
             IF NOT sigue THEN
                NEXT FIELD uno
             END IF
          WHEN tres = "X"
             CALL despliega_deposito(tipo,"VIV",fecha2) RETURNING sigue
             IF NOT sigue THEN
                NEXT FIELD uno
             END IF
       END CASE

		 EXIT INPUT

	 ON KEY ( INTERRUPT )
       LET salir = TRUE
	    EXIT INPUT
 END INPUT

 IF salir THEN
    RETURN
 END IF
		
 PROMPT "Desea Generar el proceso S/N ? " FOR CHAR aux_pausa

 IF aux_pausa NOT MATCHES "[Ss]" THEN
    CALL Inicializa()
    RETURN
 END IF

 ERROR "PROCESANDO INFORMACION .... ESPERE UN MOMENTO" ATTRIBUTE(BOLD)

 LET vmenos = arr_c - 1
---------------------------------------------------        FOR i= 1 to arr_c

 LET prioridad = "set pdqpriority high"
 PREPARE clapri FROM prioridad
 EXECUTE clapri

 FOR i= 1 to vmenos
    IF g_sal[i].seleccion = "X" THEN
       CALL Proceso_principal(g_sal[i].folio)
       CALL actualiza_estados(g_sal[i].folio)
    END IF
 END FOR
                                                             
---  ERROR "REHABILITANDO CUENTAS..."                          
---  LET ejecuta = "fglgo DISB028.4gi ",g_sal[1].folio CLIPPED 
---  RUN ejecuta                                               

  ERROR ""

   PROMPT "Proceso Finalizo Normalmente... Presione < ENTER > para Salir" FOR CHAR aux_pausa

 CLEAR FORM
 CLEAR SCREEN

 FOR i = 1 TO 100
    LET g_sal[i].folio  = NULL        
    LET g_sal[i].fecha_archivo = NULL
    LET g_sal[i].aportacion  = NULL 
    LET g_sal[i].comision = NULL   
    LET g_sal[i].seleccion  = NULL
 END FOR
 
END FUNCTION

FUNCTION inicia_saldos()
   INITIALIZE g_sal TO NULL
   FOR i = 1 TO 50
      DISPLAY g_sal[i].* TO scr_liquida[i].* 
   END FOR
END FUNCTION

FUNCTION despliega_saldos(tipo,aporte,fecha_liquida)

   DEFINE tipo		CHAR(1)
   DEFINE aporte	CHAR(3)
   DEFINE fecha_liquida	DATE

   OPEN WINDOW wsaldo AT 9,3 WITH FORM "DISB0072" ATTRIBUTE (BORDER)
   DISPLAY "DEPOSITOS PENDIENTES POR LIQUIDAR" AT 1,15
   DISPLAY aporte AT 1,55
   DISPLAY "-----------------------------------------------------------------------------" AT 2,1
   CALL inicia_saldos()

   LET tipo_liquida     = " "
   LET tipo_comision    = " "
   LET importe_liquida  = 0
   LET importe_comision = 0
   LET importe_total    = 0
   LET total_accion     = 0

   CASE
      WHEN tipo = "A" AND aporte = "RCV"
           LET tipo_liquida  = "41"
           LET tipo_comision = "11"
      WHEN tipo = "A" AND aporte = "EST"
           LET tipo_liquida  = "42"
           LET tipo_comision = "12"
      WHEN tipo = "A" AND aporte = "VIV"
           LET tipo_liquida  = "43"
      WHEN tipo = "I" AND aporte = "RCV"
           LET tipo_liquida  = "21"
           LET tipo_comision = "31"
      WHEN tipo = "I" AND aporte = "EST"
           LET tipo_liquida  = "22"
           LET tipo_comision = "32"
      WHEN tipo = "I" AND aporte = "VIV"
           LET tipo_liquida  = "23"
   END CASE

   DECLARE cur_saldo CURSOR FOR
   SELECT folio,fecha_archivo,impt_aport_acept,0
      FROM dis_dep_aporte
      WHERE ident_pago[14,15] = tipo_liquida 
        AND estado = 2
      ORDER BY 1,2

   LET totala = 0
   LET totalc = 0
   LET i = 1

   FOREACH cur_saldo INTO g_sal[i].*
      SELECT impt_aport_acept INTO g_sal[i].comision
         FROM dis_dep_aporte
         WHERE ident_pago[14,15] = tipo_comision 
         AND estado            = 2
         AND folio = g_sal[i].folio
      LET i = i + 1
   END FOREACH

   CALL SET_COUNT(i-1)

   INPUT ARRAY g_sal WITHOUT DEFAULTS FROM scr_liquida.*
      BEFORE FIELD seleccion
         LET arr_c = ARR_CURR()
         LET arr_l = SCR_LINE()

      AFTER FIELD seleccion
         IF g_sal[arr_c].aportacion IS NULL THEN
            LET g_sal[arr_c].aportacion = 0
         END IF
         IF g_sal[arr_c].comision IS NULL THEN
            LET g_sal[arr_c].comision = 0
         END IF

      ON KEY (ESC)
   	 FOR i = 1 to ARR_CURR()
             IF g_sal[i].seleccion = "X" THEN
                LET totala = totala + g_sal[i].aportacion
                LET totalc = totalc + g_sal[i].comision
             END IF
         END FOR
         DISPLAY BY NAME totala,totalc 

         PROMPT "Esta correcta la suma para liquidar [S/N] ?" FOR opc
         IF opc MATCHES "[Ss]" THEN
            EXIT INPUT
         END IF 

         FOR i = 1 TO arr_c 
             LET g_sal[i].seleccion = NULL
             LET totala = 0
             LET totalc = 0
             DISPLAY  g_sal[i].seleccion TO scr_liquida[i].seleccion
             DISPLAY BY NAME totala,totalc 
         END FOR

   END INPUT

   CLOSE WINDOW wsaldo
END FUNCTION

FUNCTION Proceso_principal(vfolio)
    DEFINE
        x_reg           RECORD LIKE dis_provision.*

    DEFINE
        text 		CHAR(1),
        vfolio          INTEGER,
        vfolio_nuevo    INTEGER,
		  nom_spl         CHAR(150)


   LET vfolio_nuevo = vfolio
   LET vfecha_pro = fecha1

    CASE 
    WHEN tipo = "A" AND uno = "X"                          # APORTE RCV
	LET nom_spl = "EXECUTE PROCEDURE liq_apor_rcv ( ",
						"'",fecha2,"'", ",",
						"'",vfecha_pro,"'",",",
						vfolio, ",",
						vfolio_nuevo, ",",
						valor_accion,
						")"

    WHEN tipo = "A" AND dos = "X"                          # APORTE ESTAT
			LET nom_spl = "EXECUTE PROCEDURE liq_apor_est ( ",
								"'",fecha2,"'",",",
								"'",vfecha_pro,"'",",",
								vfolio, ",",
								vfolio_nuevo, ",",
								valor_accion,
								")"

         CALL Actualiza_dias_cot(vfolio)

	 {DISPLAY "Inicia Proceso Principal"
	 DISPLAY "SPL:",nom_spl 
	 PROMPT " Proceso Principal :",tipo,dos FOR text}

--    WHEN tipo = "A" AND tres = "X"                         # APORTE VIV
--			LET nom_spl = "EXECUTE PROCEDURE liq_apor_viv ( ",
--								"'",fecha2,"'", ",",
--								"'",vfecha_pro,"'",",",
--								vfolio, ",",
--								vfolio_nuevo, ",",
--								valor_accion,
--								")"

    WHEN tipo = "I" AND uno = "X"                          # INTERES RCV
			LET nom_spl = "EXECUTE PROCEDURE liq_int_rcv ( ",
								"'",fecha2,"'", ",",
								"'",vfecha_pro,"'",",",
								vfolio, ",",
								vfolio_nuevo, ",",
								valor_accion,
								")"

    WHEN tipo = "I" AND dos = "X"                          # INTERES ESTAT
			LET nom_spl = "EXECUTE PROCEDURE liq_int_est ( ",
								"'",fecha2,"'", ",",
								"'",vfecha_pro,"'",",",
								vfolio, ",",
								vfolio_nuevo, ",",
								valor_accion,
								")"

--    WHEN tipo = "I" AND tres = "X"                         # INTERES VIV
--			LET nom_spl = "EXECUTE PROCEDURE liq_int_viv ( ",
--								"'",fecha2,"'", ",",
--								"'",vfecha_pro,"'",",",
--								vfolio, ",",
--								vfolio_nuevo, ",",
--								valor_accion,
--								")"

    END CASE

    LET nom_spl = nom_spl CLIPPED 
	 WHENEVER ERROR STOP
	 PREPARE eje_spl FROM nom_spl
	 EXECUTE eje_spl
	 WHENEVER ERROR CONTINUE
 
    LET vcont = 1

END FUNCTION

FUNCTION despliega_deposito(tipo,aporte,fecha_liquida)
     DEFINE tipo		CHAR(1)
     DEFINE aporte		CHAR(3)
     DEFINE fecha_liquida	DATE

     DISPLAY aporte AT 7,39 ATTRIBUTE(BOLD)


{
     LET tipo_liquida     = " "
     LET tipo_comision    = " "
     LET importe_liquida  = 0
     LET importe_comision = 0
     LET importe_total    = 0
     LET total_accion     = 0
}


       LET importe_liquida = totala
       LET importe_comision = totalc

       ERROR "Confirme Monto Total de Liquidacion Vs Deposito Bancario"

       LET importe_total = importe_liquida+importe_comision
       IF aporte <> "VIV" THEN
          LET total_accion  = importe_liquida / valor_accion
       ELSE
          LET total_accion  = 0
       END IF

       DISPLAY BY NAME importe_liquida,
                       importe_comision,
                       importe_total,
                       valor_accion,
                       total_accion
       RETURN TRUE 

END FUNCTION


FUNCTION actualiza_estados(vfolio)

   DEFINE vfolio	INTEGER

   UPDATE contproc
   SET    estado_proceso = 2
   WHERE  estado_proceso = 1

   UPDATE dis_dep_aporte
   SET estado = 3,                 # LIQUIDADO
       fech_liquidacion = fecha2   # FECHA VALOR ACCION 
   WHERE folio = vfolio
   AND ident_pago[14,15] in (tipo_liquida,tipo_comision)
   AND estado = 2

END FUNCTION

FUNCTION Inicializa()
     LET uno              = " "
     LET dos              = " "
     LET tres             = " "
     LET fecha1           = " "
     LET fecha2           = " "
     LET tipo_liquida     = 0
     LET tipo_comision    = 0
     LET importe_liquida  = 0
     LET importe_comision = 0
     LET importe_total    = 0
     LET total_accion     = 0

     DISPLAY BY NAME tipo             ,
                     uno              ,
                     dos              ,
                     tres             ,
                     fecha1           ,
                     fecha2           ,
                     --tipo_liquida     ,
                     --tipo_comision    ,
                     importe_liquida  ,
                     importe_comision ,
                     importe_total    ,
                     total_accion
                     
   CLEAR tipo,uno,dos,tres,fecha1,fecha2


   LET vmenos = 0
   LET i = 0
   LET arr_c = 0

END FUNCTION

FUNCTION Actualiza_dias_cot(vfolio)

   DEFINE vfolio INTEGER

   LET ejecuta = "nohup time fglgo DISB043.4gi ",vfolio CLIPPED," &" 
   RUN ejecuta                                                       

{
   DEFINE g_reg RECORD
      nss  CHAR(11),
      dias INTEGER
   END RECORD

   DECLARE cur_dias CURSOR FOR
   SELECT n_seguro,
          SUM(dias_cotz_bimestre)
   FROM   dis_det_aporte
   WHERE  folio = vfolio
   GROUP  BY 1

   FOREACH cur_dias INTO g_reg.*
      UPDATE cta_ctr_cuenta
      SET    dias_cotizados = dias_cotizados + g_reg.dias
      WHERE  nss = g_reg.nss
   END FOREACH
}
END FUNCTION
