################################################################################
#Project           => SAFRE (Mexico)                                           #
#Owner             => E.F.P.                                                   #
#Programa RETM010  => ELIMINACION SOLICITUDES DE RETIROS PARCIALES CADUCAS     #
#Fecha creacion    => 09 DE DIECIEMBRE DE 2003                                 #
#By                => JOSE LUIS SALDIVAR CARDOSO                               #
#Sistema           => RET                                                      #
################################################################################
DATABASE safre_af  
GLOBALS

    DEFINE g_sal ARRAY[500] OF RECORD #glo #g_sal
        n_seguro              CHAR(11)      ,
        tipo_prestacion       SMALLINT      ,
        fecha_resolucion      DATE          ,
	dias_transcurridos    SMALLINT      
    END RECORD

    DEFINE g_sal2 ARRAY[500] OF RECORD #glo #g_sal2
        n_seguro              CHAR(11)      ,
        tipo_prestacion       SMALLINT      ,
        fecha_resolucion      DATE          ,
	dias_transcurridos    SMALLINT      ,
	seleccion             CHAR(01)
    END RECORD

    DEFINE  reg_1            RECORD   #glo #reg_1
	confirmado           SMALLINT
    END RECORD

    DEFINE #glo #date
        HOY		      DATE  

    DEFINE #glo #char
        ch                    CHAR(100) ,
        opc                   CHAR(001) ,
        enter		      CHAR(001) 

    DEFINE #glo #smallint
	sw_1                  ,
        arr_c                 SMALLINT

    DEFINE #glo #smallint
        i                     ,
        cont		      INTEGER

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL init()
    OPEN WINDOW RETM0101 AT 3,3 WITH FORM "RETM0101" ATTRIBUTE (BORDER)
    DISPLAY "      < Ctrl-C > Salir                                ESC : Actualizar         " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETM010         SOLICITUDES DE RETIROS PARCIALES CADUCAS                      " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    call consulta()
    CLOSE WINDOW RETM0101
END MAIN

FUNCTION consulta()
#c-----------------
WHILE TRUE
    INITIALIZE g_sal TO NULL
    LET sw_1 = 0

    DECLARE cur_1     CURSOR FOR
    SELECT A.n_seguro         ,
           A.tipo_prestacion  ,
           A.fecha_resolucion ,
           0
    FROM   ret_resol_retiro A
    WHERE  A.tipo_prestacion IN (6,7)  
    AND    A.referencia <> 0
    AND    A.status     = reg_1.confirmado
    ORDER BY 1,2,3

    LET i      = 1
    FOREACH cur_1 INTO g_sal[i].*
	IF g_sal[i].tipo_prestacion = 6 THEN  ##DESEMPLEO
	    IF g_sal[i].fecha_resolucion <= (HOY-90) THEN
		LET g_sal2[i].n_seguro           = g_sal[i].n_seguro
		LET g_sal2[i].tipo_prestacion    = g_sal[i].tipo_prestacion
		LET g_sal2[i].fecha_resolucion   = g_sal[i].fecha_resolucion
		LET g_sal2[i].dias_transcurridos = (HOY - 
						    g_sal[i].fecha_resolucion)
                LET i = i + 1
            END IF
        END IF
	IF g_sal[i].tipo_prestacion = 7 THEN  ##MATRIMONIO
	    IF g_sal[i].fecha_resolucion <= (HOY-60)  THEN
		LET g_sal2[i].n_seguro           = g_sal[i].n_seguro
		LET g_sal2[i].tipo_prestacion    = g_sal[i].tipo_prestacion
		LET g_sal2[i].fecha_resolucion   = g_sal[i].fecha_resolucion
		LET g_sal2[i].dias_transcurridos = (HOY - 
						    g_sal[i].fecha_resolucion)
                LET i = i + 1
            END IF
        END IF
        
    END FOREACH
    CALL SET_COUNT(i-1)
    IF i = 1  THEN
	PROMPT " NO SE ENCONTRARON SOLICITUDES CADUCAS ...<ENTER>",
	       " PARA CONTINUAR" ATTRIBUTE(REVERSE) FOR CHAR enter
        EXIT PROGRAM
    END IF

    INPUT ARRAY g_sal2  WITHOUT DEFAULTS FROM scr_caducas.*

        BEFORE FIELD seleccion
            LET arr_c = ARR_CURR()
        AFTER FIELD seleccion
	    IF g_sal2[arr_c].seleccion <> "X"  THEN
		ERROR " DEBERA SELECCIONAR CON X "  
                NEXT FIELD seleccion
            END IF

        ON KEY (ESC)
   	    FOR i = 1 TO ARR_CURR()
                IF g_sal2[i].seleccion = "X" THEN
		    DELETE 
		    FROM  ret_resol_retiro
		    WHERE n_seguro         = g_sal2[i].n_seguro
		    AND   tipo_prestacion  = g_sal2[i].tipo_prestacion
		    AND   fecha_resolucion = g_sal2[i].fecha_resolucion
		    AND   @status          = reg_1.confirmado

       	            INITIALIZE g_sal2[i].* TO NULL
                    DISPLAY  g_sal2[i].* TO scr_caducas[i].*
	            ERROR " REGISTRO(S) ELIMINADO(S)..  "
	            ATTRIBUTE(REVERSE) SLEEP 1
	            ERROR ""
                END IF
            END FOR

            FOR i = 1 TO arr_c 
	        IF g_sal2[i].seleccion <> "X" THEN 
	       	    ERROR " DEBERA SELECCIONAR CON X "  
		    SLEEP 1
		    NEXT FIELD seleccion
                END IF
            END FOR
	    EXIT INPUT
            
        ON KEY (INTERRUPT)
	    ERROR " PROCESO CANCELADO  " 
	    ATTRIBUTE(REVERSE) SLEEP 2
	    LET sw_1 = 1
            EXIT INPUT

    END INPUT
    IF sw_1 = 1 THEN
        EXIT WHILE
    ELSE
        CONTINUE WHILE   
    END IF
END WHILE 
END FUNCTION

FUNCTION init()
#i-------------
    LET HOY = TODAY

    SELECT A.status
    INTO   reg_1.confirmado
    FROM   ret_status A
    WHERE  A.descripcion = "CONFIRMADO"

END FUNCTION

