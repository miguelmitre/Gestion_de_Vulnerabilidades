################################################################################
#Proyecto          => SISTEMA DE AFORE( SAFRE )                                #
#PROPIETARIO       => EFP                                                      #
#Programa VOLB005  => Generacion de archivo 1108                               #
#Fecha creacion    => 25 DE ENERO 2008                                         #
#Por               => JRC                                                      #
################################################################################
DATABASE safre_af
GLOBALS
	DEFINE
		n_regs			INTEGER,
		fecproc			DATE,
		nomarch			CHAR(120),
		fecini, fecfin	DATE,
		ar_sub			ARRAY[16] OF CHAR(02)
END GLOBALS 

MAIN
	DEFER INTERRUPT
	OPTIONS INPUT WRAP, PROMPT LINE LAST, ACCEPT KEY CONTROL-I

	LET fecproc = TODAY 
	OPEN WINDOW volb0051 AT 4, 4 WITH FORM "VOLB0051" ATTRIBUTE( BORDER )
	DISPLAY "<ESC> : Genera" AT 1, 2
	DISPLAY "<CTRL-C> : Salir" AT 1, 58
   DISPLAY " VOLB005               GENERACION DE ARCHIVO 1108              ",
		fecproc USING "DD-MM-YYYY " AT 3, 1 ATTRIBUTE( REVERSE )
	CALL ObtenPeriodoDefault() RETURNING fecini, fecfin
	CALL ExtraePeriodo()
	CLOSE WINDOW volb0051
END MAIN

FUNCTION ObtenPeriodoDefault()
	DEFINE
		ndias				ARRAY[12] OF SMALLINT,
		mes, anio		SMALLINT,
		feccad			CHAR(10)

	LET ndias[01] = 31     	#* Enero *
	LET ndias[02] = 28		#* Febrero no Bisiesto
	LET ndias[03] = 31		#* Marzo *
	LET ndias[04] = 30		#* Abril *
	LET ndias[05] = 31		#* Mayo  *
	LET ndias[06] = 30		#* Junio *
	LET ndias[07] = 31		#* Julio *
	LET ndias[08] = 31		#* Agosto *
	LET ndias[09] = 30		#* Septiembre *
	LET ndias[10] = 31		#* Octubre *
	LET ndias[11] = 30		#* Noviembre *
	LET ndias[12] = 31		#* Diciembre *

	LET fecproc = fecproc - INTERVAL(1) MONTH TO MONTH 
	LET mes = MONTH( fecproc )
	LET anio = YEAR( fecproc )
	
	LET feccad = mes USING "&&", "/01/", anio USING "&&&&"
	LET fecini = feccad
	IF( mes = 2 ) THEN   #* Es Febrero *
		IF( ( anio MOD 4 = 0 AND anio MOD 100 <> 0 ) 
			OR anio MOD 400  = 0 ) THEN    #* Es BISIESTO *
			LET ndias[02] = 29
		END IF
	END IF
	LET feccad = mes USING "&&", "/", ndias[ mes ] USING "&&", "/", 
		anio USING "&&&&"
	LET fecfin = feccad
	RETURN fecini, fecfin
END FUNCTION

FUNCTION ExtraePeriodo()
	DEFINE enter		CHAR( 1 )

   DISPLAY BY NAME fecini, fecfin
	INPUT BY NAME fecini, fecfin WITHOUT DEFAULTS
		AFTER FIELD fecini
			IF( fecini IS NULL ) THEN
				ERROR "La Fecha Inicial NO debe ser NULA."
				NEXT FIELD fecini
			END IF

		AFTER FIELD fecfin
			IF( fecfin IS NULL ) THEN
				ERROR "La Fecha Final NO debe ser NULA."
				NEXT FIELD fecfin
			END IF

		ON KEY ( ESC )
			IF( fecini IS NULL ) THEN
				ERROR "La Fecha Inicial NO debe ser NULA."
				NEXT FIELD fecini
			END IF

			IF( fecfin IS NULL ) THEN
				ERROR "La Fecha Final NO debe ser NULA."
				NEXT FIELD fecfin
			END IF

			IF( fecini > fecfin ) THEN
				ERROR "La Fecha Inicial NO debe ser MAYOR a la Fecha Final"
				NEXT FIELD fecini
			END IF

			WHILE TRUE
				PROMPT " ESTA SEGURO S/N ? " FOR CHAR enter
				IF enter MATCHES "[sSnN]" THEN
					IF enter MATCHES "[sS]" THEN
						EXIT WHILE
					ELSE
						PROMPT" PROCESO CANCELADO...<ENTER> PARA CONTINUAR "
							FOR CHAR enter
						EXIT PROGRAM
					END IF
				END IF
			END WHILE
			DISPLAY " PROCESANDO INFORMACION..." AT 19, 1 ATTRIBUTE( REVERSE )

			CALL GeneraArchivoPlano()
			DISPLAY "TOTAL REGISTROS PROCESADOS : ", n_regs AT 10, 5 
			DISPLAY "ARCHIVO GENERADO : ", nomarch AT 11, 5 ATTRIBUTE( BOLD )
			PROMPT " PROCESO FINALIZADO... < ENTER > PARA SALIR " FOR CHAR enter
			EXIT INPUT

		ON KEY ( INTERRUPT, CONTROL-C )
			CLEAR FORM
			ERROR " PROCESO CANCELADO..."
			SLEEP 3
			EXIT INPUT
	END INPUT
END FUNCTION

FUNCTION GeneraArchivoPlano()
	DEFINE
		RutaArch		LIKE seg_modulo.ruta_envio,
		scta   		SMALLINT,
		sscta  		CHAR(02),
		montoacc		LIKE dis_cuenta.monto_en_acciones,
		n_sief		SMALLINT,
		feccad		CHAR(10),
		fecha			DATE

	SELECT ruta_envio INTO RutaArch FROM seg_modulo
		WHERE modulo_cod = 'vol'

	LET feccad = MONTH( TODAY ) USING '&&', "/01/", YEAR( TODAY ) USING "&&&&"
	LET fecha = feccad
	CALL DiaHabilNo6( fecha, 6 ) RETURNING fecha

	LET nomarch = RutaArch CLIPPED, "/", fecha USING "YYYYMMDD", ".1108"
	START REPORT rep_aportaciones TO nomarch

	CREATE TEMP TABLE tab_aporta (
		subsubcta	CHAR(02),
		subcta 		SMALLINT,
		montacc 		DECIMAL(22, 6) );
	CALL InsSieforesEnCeros()

	#* Inserta en Tabla Temporal aportaciones del Periodo *
	DECLARE cursor00 CURSOR FOR
		SQL   
			SELECT 
				DECODE( subcuenta, 3, '06', 10, '05', 11, '04', 12, '03', 
				15, '02', 16, '01' ), 
				DECODE( siefore, 1, 8, 2, 20, 3, 32, 4, 44, 5, 56, 6, 80 ),
				SUM(monto_en_acciones)
				FROM dis_cuenta
				WHERE fecha_conversion BETWEEN $fecini AND $fecfin
					AND siefore IN ( 1, 2, 3, 4, 5, 6 )
					AND subcuenta IN ( 3, 10, 11, 12, 15, 16 )
				GROUP BY 2, 1
				ORDER BY 2, 1
		END SQL
	FOREACH cursor00 INTO sscta, scta, montoacc
		UPDATE tab_aporta SET montacc = montoacc
			WHERE subcta = scta AND subsubcta = sscta
	END FOREACH

	#* Obtiene No. de regs. (Aportaciones) *
	SELECT COUNT(*) INTO n_regs FROM tab_aporta

	SQL    #Obtiene No. de Siefores (Cuenta Mayor) *
		SELECT COUNT( DISTINCT subcta ) INTO $n_sief FROM tab_aporta
	END SQL

	LET n_regs = n_regs + n_sief + 1    #* Total de Regs mas Encabezado *

	#* Imprimer Encabezado *
	OUTPUT TO REPORT rep_aportaciones( 'E', 0, '', 0 )  
	
	DECLARE cursor01 CURSOR FOR
		SELECT * FROM tab_aporta
			ORDER BY 2, 1
	FOREACH cursor01 INTO sscta, scta, montoacc
		OUTPUT TO REPORT rep_aportaciones( 'D', scta, sscta, montoacc )
	END FOREACH
	FINISH REPORT rep_aportaciones
END FUNCTION

FUNCTION DiaHabilNo6( diaActual, numDiaHabil )
	DEFINE
		diaTmp, diaHabilSig,
		diaActual				DATE,
		cont_1, numDiaHabil,
		contador, diaSemana,
		feriado, finSemana 	SMALLINT

   LET cont_1 = 0
   LET diaHabilSig = diaActual
   WHILE TRUE
       LET feriado = 0
       LET finSemana = 0
       LET diaSemana = WEEKDAY(diaHabilSig)

       IF diaSemana = 0 OR diaSemana = 6 THEN
           LET finSemana = 1
       ELSE
           SELECT 1 FROM tab_feriado
				  WHERE feria_fecha = diaHabilSig
           IF STATUS <> NOTFOUND THEN
               LET feriado = 1
           END IF
       END IF
       IF feriado = 1 OR finSemana = 1 THEN
           LET diaHabilSig = diaHabilSig + 1 UNITS DAY
       ELSE
           LET cont_1 = cont_1 + 1
           IF cont_1 = numDiaHabil THEN
               EXIT WHILE
           ELSE
               LET diaHabilSig = diaHabilSig + 1 UNITS DAY
           END IF
       END IF
   END WHILE
   RETURN diaHabilSig
END FUNCTION

FUNCTION InsSieforesEnCeros()
	DEFINE
		sief, ssc, sc	SMALLINT,
		sscta				CHAR(02)

	LET ar_sub[01] = '08'
	LET ar_sub[02] = '12'
	LET ar_sub[03] = '20'
	LET ar_sub[04] = '24'
	LET ar_sub[05] = '32'
	LET ar_sub[06] = '36'
	LET ar_sub[07] = '44'
	LET ar_sub[08] = '48'
	LET ar_sub[09] = '56'
	LET ar_sub[10] = '60'
	LET ar_sub[11] = '80'
	LET ar_sub[12] = '81'
	LET ar_sub[13] = '82'
	LET ar_sub[14] = '85'
	LET ar_sub[15] = '86'
	LET ar_sub[16] = '87'

	FOR sc = 1 TO 16    #* Indice Subcuenta *
		FOR ssc = 1 TO 8   #* Indice SubSubcuenta *
			LET sscta = ssc USING "&&"
			LET sief = ar_sub[sc]
			INSERT INTO tab_aporta VALUES( sscta, sief, 0 )
		END FOR
	END FOR
END FUNCTION

REPORT rep_aportaciones( tipo, subcta, Ssubcta, montoacc )
	DEFINE
		tipo						CHAR( 1 ),
		codafo					SMALLINT,
		subcta, Ssubcta		CHAR(02),
		montoacc, SaldoFinal	LIKE dis_cuenta.monto_en_acciones,
		Formato					CHAR(16)

	OUTPUT
		PAGE LENGTH   1
		LEFT MARGIN   0
		RIGHT MARGIN  0
		TOP MARGIN    0
		BOTTOM MARGIN 0
		ORDER EXTERNAL BY subcta

	FORMAT
		
	ON EVERY ROW
		IF( tipo = 'E' ) THEN    #* Encabezado *
			SELECT codigo_afore INTO codafo FROM tab_afore_local
			PRINT COLUMN 01, "000",						#* Tipo de Registro *
				COLUMN 04, "1108",         			#* Tipo de Archivo  *
				COLUMN 08, "001",          			#* Tipo de Entidad  *
				COLUMN 11, codafo USING "&&&",		#* Entidad          *
				COLUMN 14, fecfin USING "YYYYMMDD",	#* Fecha de Operacion * 
				COLUMN 22, "043",							#* Tipo de Registro   *
				COLUMN 25, n_regs USING "&&&&&",	   #* No. de Regs. c/encab. * 
				COLUMN 30, 14 SPACES						#* FILLER        *
		ELSE                     #* Detalle    *
			LET montoacc = montoacc * 100
			LET Formato = '&&&&&&&&&&&&&&&&'    	#* Cantidades Positivas *
			IF( montoacc < 0 ) THEN
				LET Formato[1] = '-'    				#* Cantidades Negativas *
			END IF
			PRINT COLUMN 01, "301",						#* Tipo de registro *
				COLUMN 04, "7114",						#* Clave de Cuenta  *
				COLUMN 08, subcta USING "&&",			#* Cve. Subcuenta.  *
				COLUMN 10, Ssubcta,						#* Cve. Sub-Subcta. *
				COLUMN 12, montoacc USING Formato,  #* SubTotal *
				COLUMN 28, '0000000000000000'			#* Saldo Final NULO *
		END IF

	BEFORE GROUP OF subcta
		IF( tipo <> 'E' ) THEN
			LET SaldoFinal = 0
			SQL
				SELECT SUM(montacc) INTO $SaldoFinal FROM tab_aporta
					WHERE tab_aporta.subcta = $subcta
			END SQL
			LET Formato = '&&&&&&&&&&&&&&&&'     	#* Cantidades Positivas *
			IF( SaldoFinal < 0 ) THEN
				LET Formato[1] = '-'     				#* Cantidades Negativas *
			END IF
			LET SaldoFinal = SaldoFinal * 100
			PRINT COLUMN 01, "301",						#* Tipo de registro *
				COLUMN 04, "7114",						#* Clave de Cuenta  *
				COLUMN 08, subcta USING "&&",			#* Cve. Subcuenta.  *
				COLUMN 10, "00",   						#* Cve. Sub-Subcta. *
				COLUMN 12, '0000000000000000',		#* Monto SubTotal NULO   *
				COLUMN 28, SaldoFinal USING Formato #* Saldo Final *
		END IF
END REPORT
