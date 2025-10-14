###########################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                        #
#Propietario       => EFP                                                 #
#Programa AFIM016  => MANTENIMIENTO DE SIEFORES                           #
#Autor             => MAURO MUNIZ CABALLERO.      			  #
#Fecha             => 15 DE ENERO DE 2001                                 #
#Sistema           => AFI. 					          #
###########################################################################

DATABASE safre_af

GLOBALS

    DEFINE 
        aux_pausa CHAR(1),
        enter     CHAR(1),
        ACCION    CHAR(1),
        g_usuario CHAR(8),
        n         CHAR(11),
        u         CHAR(18),
        r         CHAR(13)

    DEFINE x_reg RECORD
        siefore1,
        siefore4,
        siefore7,
        siefore2,
        siefore5,
        siefore8,
        siefore3,
        siefore6,
        siefore9 SMALLINT
    END RECORD
  
    DEFINE 
        suma_linea1  ,
        suma_linea2  ,
        suma_linea3  ,
        bandera      ,
        band_afi     SMALLINT

    DEFINE 
        x_fena       DATE,
        HOY          DATE

    DEFINE g_afili RECORD
        n_seguro         CHAR(11),
        n_rfc            CHAR(13),
        n_unico          CHAR(18),
        paterno          CHAR(40),
        materno          CHAR(40),
        nombres          CHAR(40),
        fena             DATE,
        n_folio          DECIMAL(10,0),
        tipo_solicitud   SMALLINT,
        siefore1         ,
        siefore4         ,
        siefore7         ,
        siefore2         ,
        siefore5         ,
        siefore8         ,
        siefore3         ,
        siefore6         ,
        siefore9         SMALLINT
    END RECORD     

END GLOBALS

MAIN

    OPTIONS PROMPT LINE LAST,
        ACCEPT KEY CONTROL-I,	
        INPUT WRAP

	DEFER INTERRUPT

    CALL STARTLOG("AFIM016.log")
    CALL inicio()   #i
    CALL proceso_principal()

END MAIN

FUNCTION inicio()
#i---------------

    LET HOY      = TODAY
    LET bandera  = 0
    LET band_afi = 0
    LET g_afili.n_seguro       = ARG_VAL(1)
    LET g_afili.n_folio        = ARG_VAL(2)
    LET g_afili.tipo_solicitud = ARG_VAL(3)
    LET ACCION                 = ARG_VAL(4)

    IF g_afili.n_seguro = " " THEN
    ERROR "ESTE PROGRAMA SOLO PUEDE SER EJECUTADO DESDE SU MODULO PRINCIPAL" 
        SLEEP 3
	EXIT PROGRAM
    END IF

        SELECT a.n_rfc,
               a.n_unico,
               a.paterno,
               a.materno,
               a.nombres,
               a.fena,
               USER
        INTO   g_afili.n_rfc,
               g_afili.n_unico,
               g_afili.paterno,
               g_afili.materno,
               g_afili.nombres,
               g_afili.fena,
               g_usuario
        FROM   afi_mae_afiliado a
        WHERE  a.n_folio        = g_afili.n_folio
        AND    a.tipo_solicitud = g_afili.tipo_solicitud  

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW v1 AT 2,2 WITH FORM "AFIM0061" ATTRIBUTE(BORDER)
    DISPLAY  " AFIM016                    INGRESO DE SIEFORES                                " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)

    DISPLAY BY NAME g_afili.*

    CASE
        WHEN ACCION = "A" OR ACCION = "M" OR ACCION = "E" 
            MENU "SIEFORE"
	        COMMAND "Agrega"   "Agrega siefores"
	                CALL Inicializa()
	                CALL Agrega()
	                CALL Inicializa()
	        COMMAND "Modifica" "Modifica siefores"
	                CALL Inicializa()
	                CALL Modifica()
	                CALL Inicializa()
	        COMMAND "Consulta"  "Consulta siefores"
	                CALL Inicializa()
	                CALL Consulta()
	                CALL Inicializa()
	        COMMAND "Salir"    "Salir"
	                EXIT PROGRAM
	    END MENU

        WHEN ACCION = "C"
            MENU "SIEFORE"
	        COMMAND "Consulta"  "Consulta siefores"
	                CALL Inicializa()
	                CALL Consulta()
	                CALL Inicializa()
	        COMMAND "Salir"    "Salir"
	                EXIT PROGRAM
	    END MENU
    END CASE

END FUNCTION

FUNCTION Inicializa()
#I-------------------

    INITIALIZE x_reg.* TO NULL

    LET band_afi = 0

END FUNCTION
      
FUNCTION Agrega()
#A---------------

	DEFINE NULA SMALLINT
	
	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " [ Esc ] Grabar        [ Ctrl-c ] Salir sin Grabar" AT 2,1
	DISPLAY " AGREGA " AT 1,67 ATTRIBUTE(REVERSE)

	LET x_reg.siefore1 = 100
	LET x_reg.siefore2 = 100
	LET x_reg.siefore3 = 100

        LET x_reg.siefore4 = 0
        LET x_reg.siefore5 = 0
        LET x_reg.siefore6 = 0

        LET x_reg.siefore7 = 0
        LET x_reg.siefore8 = 0
        LET x_reg.siefore9 = 0

	INPUT BY NAME x_reg.* WITHOUT DEFAULTS
              BEFORE FIELD siefore1
              	     SELECT "X" 
                     FROM afi_mae_siefore
	             WHERE n_seguro = g_afili.n_seguro
                     AND   subcta = 1 
                     GROUP BY 1

                     IF STATUS <> 100 THEN
                        #ERROR "Ya existe porcentaje para esta subcuenta"
                        NEXT FIELD siefore4
                     END IF

	      AFTER FIELD siefore1
		     IF x_reg.siefore1 IS NULL THEN
			ERROR "Siefore R.C.V NO puede ser NULA"
		        NEXT FIELD siefore1
		     END IF
		     IF x_reg.siefore1 > 100 THEN
			ERROR "Siefore R.C.V NO puede ser mayor que el 100%"
		        NEXT FIELD siefore1
		     END IF
		    
	      AFTER FIELD siefore2
		     IF x_reg.siefore2 IS NULL THEN
			ERROR "Siefore Aportacion Voluntaria NO puede ser NULA"
		        NEXT FIELD siefore2
		     END IF
		     IF x_reg.siefore2 > 100 THEN
        ERROR "Siefore Aportacion Voluntaria NO puede ser mayor que el 100%"
		        NEXT FIELD siefore2
		     END IF

	      AFTER FIELD siefore3
		     IF x_reg.siefore3 IS NULL THEN
			ERROR "Siefore SAR92 NO puede ser NULA"
		        NEXT FIELD siefore3
		     END IF
		     IF x_reg.siefore3 > 100 THEN
			ERROR "Siefore SAR92 NO puede ser mayor que el 100%"
		        NEXT FIELD siefore3
		     END IF

	      AFTER FIELD siefore4
		     IF x_reg.siefore4 IS NULL THEN
			ERROR "Siefore R.C.V NO puede ser NULA"
		        NEXT FIELD siefore4
		     END IF
		     IF x_reg.siefore4 > 100 THEN
			ERROR "Siefore R.C.V NO puede ser mayor que el 100%"
		        NEXT FIELD siefore4
		     END IF

	      AFTER FIELD siefore5
		     IF x_reg.siefore5 IS NULL THEN
			ERROR "Siefore Aportacion Voluntaria NO puede ser NULA"
		        NEXT FIELD siefore5
		     END IF
		     IF x_reg.siefore5 > 100 THEN
	ERROR "Siefore Aportacion Voluntaria NO puede ser mayor que el 100%"
		        NEXT FIELD siefore5
		     END IF

	      AFTER FIELD siefore6
		     IF x_reg.siefore6 IS NULL THEN
			ERROR "Siefore SAR92 NO puede ser NULA"
		        NEXT FIELD siefore6
		     END IF
		     IF x_reg.siefore6 > 100 THEN
			ERROR "Siefore SAR92 NO puede ser mayor que el 100%"
		        NEXT FIELD siefore6
		     END IF

	      AFTER FIELD siefore7
		     IF x_reg.siefore7 IS NULL THEN
			ERROR "Siefore R.C.V NO puede ser NULA"
		        NEXT FIELD siefore7
		     END IF
		     IF x_reg.siefore7 > 100 THEN
			ERROR "Siefore R.C.V NO puede Mayor que el 100%"
		        NEXT FIELD siefore7
		     END IF
		 IF 100<>(x_reg.siefore1+x_reg.siefore4+x_reg.siefore7) THEN
		    ERROR "La suma de RCV NO es igual a 100%"
		    NEXT FIELD siefore1
	         END IF

      AFTER FIELD siefore8
	     IF x_reg.siefore5 IS NULL THEN
		ERROR "Siefore Aportacion Voluntaria NO puede ser NULA"
	        NEXT FIELD siefore8
	     END IF
	     IF x_reg.siefore8 > 100 THEN
	ERROR "Siefore Aportacion Voluntaria NO puede Mayor que el 100%"
	        NEXT FIELD siefore8
	     END IF
	 IF 100<>(x_reg.siefore2+x_reg.siefore5+x_reg.siefore8) THEN
	    ERROR "La suma de Aportacion voluntaria NO es igual al 100%"
	    NEXT FIELD siefore2
         END IF

      AFTER FIELD siefore9
	     IF x_reg.siefore9 IS NULL THEN
		ERROR "Siefore Retiro SAR92 NO puede ser NULA"
	        NEXT FIELD siefore9
	     END IF
	     IF x_reg.siefore9 > 100 THEN
		ERROR "Siefore SAR92 NO puede ser mayor que el 100%"
	        NEXT FIELD siefore9
	     END IF
	 IF 100<>(x_reg.siefore3+x_reg.siefore6+x_reg.siefore9) THEN
	    ERROR "La suma de SAR92 NO es igual a 100%"
	    NEXT FIELD siefore3
         END IF

      ON KEY ( ESC )
       	 SELECT "X" 
         FROM   afi_mae_siefore
         WHERE  n_seguro = g_afili.n_seguro
         AND    subcta = 1 
         GROUP BY 1
               IF STATUS <> 100 THEN
                  ERROR "Ya existe porcentaje para esta subcuenta"
                  NEXT FIELD siefore1
               END IF
	 IF 100<>(x_reg.siefore1+x_reg.siefore4+x_reg.siefore7) THEN
	    ERROR "La suma de RCV NO es igual a 100%"
	    NEXT FIELD siefore1
         END IF
	 IF 100<>(x_reg.siefore2+x_reg.siefore5+x_reg.siefore8) THEN
	    ERROR "La suma de aportacion voluntaria NO es igual a 100%"
	    NEXT FIELD siefore2
         END IF
	 IF 100<>(x_reg.siefore3+x_reg.siefore6+x_reg.siefore9) THEN
	    ERROR "La suma de SAR92 NO es igual a 100%"
	    NEXT FIELD siefore3
         END IF

	 CALL Pregunta()
         IF aux_pausa MATCHES "[Ss]" THEN
	     LET n = g_afili.n_seguro
	     LET u = g_afili.n_unico
	     LET r = g_afili.n_rfc
	    IF x_reg.siefore1 > 0 THEN
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,1,1,x_reg.siefore1)
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,2,1,x_reg.siefore1)
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,5,1,x_reg.siefore1)
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,6,1,x_reg.siefore1)
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,9,1,x_reg.siefore1)
	    END IF
	    IF x_reg.siefore4 > 0 THEN
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,1,2,x_reg.siefore4)
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,2,2,x_reg.siefore4)
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,5,2,x_reg.siefore4)
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,6,2,x_reg.siefore4)
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,9,2,x_reg.siefore4)
	    END IF
	    IF x_reg.siefore7 > 0 THEN
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,1,3,x_reg.siefore7)
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,2,3,x_reg.siefore7)
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,5,3,x_reg.siefore7)
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,6,3,x_reg.siefore7)
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,9,3,x_reg.siefore7)
	    END IF

	    INSERT INTO afi_mae_siefore VALUES (n,u,r,4,11,100)
	    INSERT INTO afi_mae_siefore VALUES (n,u,r,8,11,100)
	    INSERT INTO afi_mae_siefore VALUES (n,u,r,14,0,100)

	    IF x_reg.siefore2 > 0 THEN
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,3,1,x_reg.siefore2)
               INSERT INTO afi_mae_siefore VALUES (n,u,r,10,1,x_reg.siefore2)
               INSERT INTO afi_mae_siefore VALUES (n,u,r,11,1,x_reg.siefore2)
               INSERT INTO afi_mae_siefore VALUES (n,u,r,12,1,x_reg.siefore2)
	    END IF
	    IF x_reg.siefore5 > 0 THEN
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,3,2,x_reg.siefore5)
               INSERT INTO afi_mae_siefore VALUES (n,u,r,10,2,x_reg.siefore5)
               INSERT INTO afi_mae_siefore VALUES (n,u,r,11,2,x_reg.siefore5)
               INSERT INTO afi_mae_siefore VALUES (n,u,r,12,2,x_reg.siefore5)
	    END IF
	    IF x_reg.siefore8 > 0 THEN
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,3,3,x_reg.siefore8)
               INSERT INTO afi_mae_siefore VALUES (n,u,r,10,2,x_reg.siefore8)
               INSERT INTO afi_mae_siefore VALUES (n,u,r,11,2,x_reg.siefore8)
               INSERT INTO afi_mae_siefore VALUES (n,u,r,12,2,x_reg.siefore8)
	    END IF

	    IF x_reg.siefore3 > 0 THEN
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,7,1,x_reg.siefore3)
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,13,1,x_reg.siefore3)
	    END IF
	    IF x_reg.siefore6 > 0 THEN
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,7,2,x_reg.siefore6)
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,13,2,x_reg.siefore6)
	    END IF
	    IF x_reg.siefore9 > 0 THEN
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,7,3,x_reg.siefore9)
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,13,3,x_reg.siefore9)
	    END IF

	    ERROR "REGISTRO INGRESADO"
	 ELSE
	    ERROR "INGRESO CANCELADO"
	 END IF
	 SLEEP 2 ERROR ""
	 CALL Inicializa()
	 EXIT INPUT

    END INPUT

END FUNCTION

FUNCTION Pregunta()
#P-----------------

    PROMPT "Esta seguro S/N ? " FOR aux_pausa

END FUNCTION

FUNCTION Modifica()
#M-----------------

	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " [ Esc ] Modifica        [ Ctrl-c ] Salir sin Grabar" AT 2,1
	DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE)

	SELECT porcentaje 
        INTO   x_reg.siefore1 
        FROM   afi_mae_siefore
        WHERE  n_seguro= g_afili.n_seguro
        AND    cod_siefore = 1 
        AND    subcta = 1

	IF x_reg.siefore1 IS NULL THEN
	   LET x_reg.siefore1 = 0
	END IF

	SELECT porcentaje 
        INTO   x_reg.siefore4 
        FROM   afi_mae_siefore
        WHERE  n_seguro= g_afili.n_seguro
        AND    cod_siefore = 2 
        AND    subcta = 1

	IF x_reg.siefore4 IS NULL THEN
	   LET x_reg.siefore4 = 0
	END IF

	SELECT porcentaje 
        INTO   x_reg.siefore7 
        FROM   afi_mae_siefore
        WHERE  n_seguro= g_afili.n_seguro
        AND    cod_siefore = 3 
        AND    subcta = 1

	IF x_reg.siefore7 IS NULL THEN
	   LET x_reg.siefore7 = 0
	END IF
	
	SELECT porcentaje 
        INTO   x_reg.siefore2 
        FROM   afi_mae_siefore
        WHERE  n_seguro= g_afili.n_seguro
	AND    cod_siefore = 1 
	AND    subcta = 3

	IF x_reg.siefore2 IS NULL THEN
	   LET x_reg.siefore2 = 0
	END IF

	SELECT porcentaje 
        INTO   x_reg.siefore5 
        FROM   afi_mae_siefore
        WHERE  n_seguro= g_afili.n_seguro
        AND    cod_siefore = 2 
        AND    subcta = 3

	IF x_reg.siefore5 IS NULL THEN
	   LET x_reg.siefore5 = 0
	END IF

	SELECT porcentaje 
        INTO   x_reg.siefore8 
        FROM   afi_mae_siefore
        WHERE  n_seguro= g_afili.n_seguro
        AND    cod_siefore = 3 
        AND    subcta = 3

	IF x_reg.siefore8 IS NULL THEN
	   LET x_reg.siefore8 = 0
	END IF

	SELECT porcentaje 
        INTO   x_reg.siefore3 
        FROM   afi_mae_siefore
        WHERE  n_seguro= g_afili.n_seguro
        AND    cod_siefore = 1 
        AND    subcta = 7

	IF x_reg.siefore3 IS NULL THEN
	   LET x_reg.siefore3 = 0
	END IF

	SELECT porcentaje 
        INTO   x_reg.siefore6 
        FROM   afi_mae_siefore
        WHERE  n_seguro= g_afili.n_seguro
        AND    cod_siefore = 2 
        AND    subcta = 7

	IF x_reg.siefore6 IS NULL THEN
	   LET x_reg.siefore6 = 0
	END IF

	SELECT porcentaje 
        INTO   x_reg.siefore9 
        FROM   afi_mae_siefore
        WHERE  n_seguro= g_afili.n_seguro
        AND    cod_siefore = 3 
        AND    subcta = 7

	IF x_reg.siefore9 IS NULL THEN
	   LET x_reg.siefore9 = 0
	END IF

      INPUT BY NAME x_reg.* WITHOUT DEFAULTS

      AFTER FIELD siefore1
	     IF x_reg.siefore1 IS NULL THEN
		ERROR "Siefore R.C.V NO puede ser NULA"
	        NEXT FIELD siefore1
	     END IF
	     IF x_reg.siefore1 > 100 THEN
		ERROR "Siefore R.C.V NO puede Mayor que el 100%"
	        NEXT FIELD siefore1
	     END IF
	    
      AFTER FIELD siefore2
	     IF x_reg.siefore2 IS NULL THEN
		ERROR "Siefore Aportacion Voluntaria NO puede ser NULA"
	        NEXT FIELD siefore2
	     END IF

	     IF x_reg.siefore2 > 100 THEN
	ERROR "Siefore Aportacion Voluntaria NO puede Mayor que el 100%"
	        NEXT FIELD siefore2
	     END IF

      AFTER FIELD siefore3
	     IF x_reg.siefore3 IS NULL THEN
		ERROR "Siefore Retiro SAR92 NO puede ser NULA"
	        NEXT FIELD siefore3
	     END IF
 
	     IF x_reg.siefore3 > 100 THEN
		ERROR "Siefore Retiro SAR92 NO puede Mayor que el 100%"
	        NEXT FIELD siefore3
	     END IF

     AFTER FIELD siefore4
	     IF x_reg.siefore4 IS NULL THEN
		ERROR "Siefore R.C.V NO puede ser NULA"
	        NEXT FIELD siefore4
	     END IF

	     IF x_reg.siefore4 > 100 THEN
		ERROR "Siefore R.C.V NO puede Mayor que el 100%"
	        NEXT FIELD siefore4
	     END IF

      AFTER FIELD siefore5
	     IF x_reg.siefore5 IS NULL THEN
		ERROR "Siefore Aportacion Voluntaria NO puede ser NULA"
	        NEXT FIELD siefore5
	     END IF

	     IF x_reg.siefore5 > 100 THEN

		ERROR "Siefore Aportacion Voluntaria NO puede Mayor que el 100%"
	        NEXT FIELD siefore5
	     END IF

      AFTER FIELD siefore6
	     IF x_reg.siefore6 IS NULL THEN
		ERROR "Siefore Retiro SAR92 NO puede ser NULA"
	        NEXT FIELD siefore6
	     END IF

	     IF x_reg.siefore6 > 100 THEN
		ERROR "Siefore Retiro SAR92 NO puede Mayor que el 100%"
	        NEXT FIELD siefore6
	     END IF

      AFTER FIELD siefore7
	     IF x_reg.siefore7 IS NULL THEN
		ERROR "Siefore R.C.V NO puede ser NULA"
	        NEXT FIELD siefore7
	     END IF

	     IF x_reg.siefore7 > 100 THEN
		ERROR "Siefore R.C.V NO puede Mayor que el 100%"
	        NEXT FIELD siefore7
	     END IF

	     IF 100<>(x_reg.siefore1+x_reg.siefore4+x_reg.siefore7) THEN
	        ERROR "La suma de RCV NO es igual a 100%"
	        NEXT FIELD siefore1
             END IF

      AFTER FIELD siefore8
	     IF x_reg.siefore5 IS NULL THEN
		ERROR "Siefore Aportacion Voluntaria NO puede ser NULA"
	        NEXT FIELD siefore8
	     END IF

	     IF x_reg.siefore8 > 100 THEN
	ERROR "Siefore Aportacion Voluntaria NO puede Mayor que el 100%"
	        NEXT FIELD siefore8
             END IF

             IF 100<>(x_reg.siefore2+x_reg.siefore5+x_reg.siefore8) THEN
	        ERROR "La suma de AP.VOLUNTARIO NO es igual a 100%"
	        NEXT FIELD siefore2
             END IF

      AFTER FIELD siefore9
	     IF x_reg.siefore9 IS NULL THEN
		ERROR "Siefore Retiro SAR92 NO puede ser NULA"
	        NEXT FIELD siefore9
	     END IF

	     IF x_reg.siefore9 > 100 THEN
		ERROR "Siefore Retiro SAR92 NO puede Mayor que el 100%"
	        NEXT FIELD siefore9
	     END IF
    
	     IF 100<>(x_reg.siefore3+x_reg.siefore6+x_reg.siefore9) THEN
	        ERROR "La suma de S.RETIRO92 NO es igual a 100%"
	        NEXT FIELD siefore3
             END IF

      ON KEY ( ESC )
	 IF 100<>(x_reg.siefore1+x_reg.siefore4+x_reg.siefore7) THEN
	    ERROR "La suma de RCV NO es igual a 100%"
	    NEXT FIELD siefore1
         END IF

	 IF 100<>(x_reg.siefore2+x_reg.siefore5+x_reg.siefore8) THEN
	    ERROR "La suma de AP.VOL. NO es igual a 100%"
	    NEXT FIELD siefore2
         END IF

	 IF 100<>(x_reg.siefore3+x_reg.siefore6+x_reg.siefore9) THEN
	    ERROR "La suma de S.RETIROSAR92 NO es igual a 100%"
	    NEXT FIELD siefore3
         END IF

	 CALL Pregunta()
         IF aux_pausa MATCHES "[Ss]" THEN
	    DELETE FROM afi_mae_siefore
	    WHERE n_seguro = g_afili.n_seguro

	     LET n = g_afili.n_seguro
	     LET u = g_afili.n_unico
	     LET r = g_afili.n_rfc
	    IF x_reg.siefore1 > 0 THEN
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,1,1,x_reg.siefore1)
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,2,1,x_reg.siefore1)
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,5,1,x_reg.siefore1)
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,6,1,x_reg.siefore1)
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,9,1,x_reg.siefore1)
	    END IF
	    IF x_reg.siefore4 > 0 THEN
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,1,2,x_reg.siefore4)
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,2,2,x_reg.siefore4)
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,5,2,x_reg.siefore4)
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,6,2,x_reg.siefore4)
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,9,2,x_reg.siefore4)
	    END IF
	    IF x_reg.siefore7 > 0 THEN
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,1,3,x_reg.siefore7)
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,2,3,x_reg.siefore7)
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,5,3,x_reg.siefore7)
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,6,3,x_reg.siefore7)
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,9,3,x_reg.siefore7)
	    END IF

	    INSERT INTO afi_mae_siefore VALUES (n,u,r,4,11,100)
	    INSERT INTO afi_mae_siefore VALUES (n,u,r,8,11,100)
	    INSERT INTO afi_mae_siefore VALUES (n,u,r,14,0,100)

	    IF x_reg.siefore2 > 0 THEN
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,3,1,x_reg.siefore2)
               INSERT INTO afi_mae_siefore VALUES (n,u,r,10,2,x_reg.siefore2)
               INSERT INTO afi_mae_siefore VALUES (n,u,r,11,2,x_reg.siefore2)
               INSERT INTO afi_mae_siefore VALUES (n,u,r,12,2,x_reg.siefore2)
	    END IF

	    IF x_reg.siefore5 > 0 THEN
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,3,2,x_reg.siefore5)
               INSERT INTO afi_mae_siefore VALUES (n,u,r,10,2,x_reg.siefore5)
               INSERT INTO afi_mae_siefore VALUES (n,u,r,11,2,x_reg.siefore5)
               INSERT INTO afi_mae_siefore VALUES (n,u,r,12,2,x_reg.siefore5)
	    END IF

	    IF x_reg.siefore8 > 0 THEN
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,3,3,x_reg.siefore8)
               INSERT INTO afi_mae_siefore VALUES (n,u,r,10,2,x_reg.siefore8)
               INSERT INTO afi_mae_siefore VALUES (n,u,r,11,2,x_reg.siefore8)
               INSERT INTO afi_mae_siefore VALUES (n,u,r,12,2,x_reg.siefore8)
	    END IF

	    IF x_reg.siefore3 > 0 THEN
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,7,1,x_reg.siefore3)
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,13,1,x_reg.siefore3)
	    END IF

	    IF x_reg.siefore6 > 0 THEN
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,7,2,x_reg.siefore6)
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,13,2,x_reg.siefore6)
	    END IF

	    IF x_reg.siefore9 > 0 THEN
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,7,3,x_reg.siefore9)
	       INSERT INTO afi_mae_siefore VALUES (n,u,r,13,3,x_reg.siefore9)
	    END IF

	    ERROR "REGISTRO INGRESADO"
	 ELSE
	    ERROR "INGRESO CANCELADO"
	 END IF

	 SLEEP 2 ERROR ""

	 CALL Inicializa()

	 EXIT INPUT

    END INPUT

END FUNCTION

FUNCTION Consulta()
#C-----------------

	DISPLAY "" AT 1,1
	DISPLAY "" AT 2,1
	DISPLAY " [ Ctrl-c ] Salir " AT 2,1
	DISPLAY " CONSULTA " AT 1,67 ATTRIBUTE(REVERSE)

	SELECT porcentaje 
        INTO   x_reg.siefore1 
        FROM   afi_mae_siefore
        WHERE  n_seguro= g_afili.n_seguro
        AND    cod_siefore = 1 
        AND    subcta = 1

	IF x_reg.siefore1 IS NULL THEN
	   LET x_reg.siefore1 = 0
	END IF

	SELECT porcentaje 
        INTO   x_reg.siefore4 
        FROM   afi_mae_siefore
        WHERE  n_seguro= g_afili.n_seguro
        AND    cod_siefore = 2 
        AND    subcta = 1

	IF x_reg.siefore4 IS NULL THEN
	   LET x_reg.siefore4 = 0
	END IF

	SELECT porcentaje 
        INTO   x_reg.siefore7 
        FROM   afi_mae_siefore
        WHERE  n_seguro= g_afili.n_seguro
        AND    cod_siefore = 3 
        AND    subcta = 1

	IF x_reg.siefore7 IS NULL THEN
	   LET x_reg.siefore7 = 0
	END IF
	
	SELECT porcentaje 
        INTO   x_reg.siefore2 
        FROM   afi_mae_siefore
        WHERE  n_seguro= g_afili.n_seguro
        AND    cod_siefore = 1 
        AND    subcta = 3

	IF x_reg.siefore2 IS NULL THEN
	   LET x_reg.siefore2 = 0
	END IF

	SELECT porcentaje 
        INTO   x_reg.siefore5 
        FROM   afi_mae_siefore
        WHERE  n_seguro= g_afili.n_seguro
        AND    cod_siefore = 2 
        AND    subcta = 3

	IF x_reg.siefore5 IS NULL THEN
	   LET x_reg.siefore5 = 0
	END IF

	SELECT porcentaje 
        INTO   x_reg.siefore8 
        FROM   afi_mae_siefore
        WHERE  n_seguro= g_afili.n_seguro
        AND    cod_siefore = 3 
        AND    subcta = 3

	IF x_reg.siefore8 IS NULL THEN
	   LET x_reg.siefore8 = 0
	END IF

	SELECT porcentaje 
        INTO   x_reg.siefore3 
        FROM   afi_mae_siefore
        WHERE  n_seguro= g_afili.n_seguro
        AND    cod_siefore = 1 
        AND    subcta = 7

	IF x_reg.siefore3 IS NULL THEN
	   LET x_reg.siefore3 = 0
	END IF
  
	SELECT porcentaje 
        INTO   x_reg.siefore6 
        FROM   afi_mae_siefore
        WHERE  n_seguro= g_afili.n_seguro
        AND    cod_siefore = 2 
        AND    subcta = 7

	IF x_reg.siefore6 IS NULL THEN
	   LET x_reg.siefore6 = 0
	END IF

	SELECT porcentaje 
        INTO   x_reg.siefore9 
        FROM   afi_mae_siefore
        WHERE  n_seguro= g_afili.n_seguro
        AND    cod_siefore = 3 
        AND    subcta = 7

	IF x_reg.siefore9 IS NULL THEN
	   LET x_reg.siefore9 = 0
	END IF

	DISPLAY BY NAME x_reg.* 

	PROMPT "Presione < ENTER >" FOR aux_pausa

END FUNCTION

