################################################################################
#Proyecto          => SISTEMA DE safre_af( MEXICO )
#Owner             => E.F.P.
#Programa TRAM006  => CONSULTA A SOLICITUDES RECHAZADAS DE TRASPASOS ICEFA-AFORE
#By                => FRANCO E. ULLOA V.
#Fecha creacion    => 18 DE FEBRERO DE 1999
#Actualizacion     => FRANCO E. ULLOA V.
#Fecha actualiz.   => 26 DE MARZO DE 1999
#Sistema           => TRA
################################################################################

DATABASE safre_af
GLOBALS

    DEFINE master RECORD #glo #master
        nss                 LIKE safre_af:cta_rehabilitada.nss               ,
        fecha_rehabilita    LIKE safre_af:cta_rehabilitada.fecha_rehabilita ,
{ DETALLE }
        marca_causa         LIKE safre_af:cta_his_inhabilitada.marca_causa  ,
        marca_desc          LIKE tab_marca.marca_desc              , 
        fecha_ini           LIKE safre_af:cta_his_inhabilitada.fecha_ini    ,
        marca_cod           LIKE safre_af:cta_rehabilitada.marca_cod        ,
        marca_desc1         LIKE tab_marca.marca_desc              , 
        monto_retiro        LIKE safre_af:cta_rehabilitada.monto_retiro     ,
        monto_cesantia      LIKE safre_af:cta_rehabilitada.monto_cesantia   ,
        monto_voluntaria    LIKE safre_af:cta_rehabilitada.monto_voluntaria ,
        monto_vivienda97    LIKE safre_af:cta_rehabilitada.monto_vivienda97 , 
        monto_cuota_soc     LIKE safre_af:cta_rehabilitada.monto_cuota_soc  ,
        monto_sar           LIKE safre_af:cta_rehabilitada.monto_sar        ,
        monto_vivienda92    LIKE safre_af:cta_rehabilitada.monto_vivienda92       
    END RECORD

    DEFINE #glo #date
        HOY		      DATE

    DEFINE #glo #char
        k       char(500),
        enter                 CHAR(1)

    DEFINE #glo #smallint
	x_max                 ,
        cuenta		      INTEGER
END GLOBALS

MAIN
    OPTIONS
    PROMPT LINE LAST,
    INPUT WRAP,
    ACCEPT KEY control-i
	
    DEFER INTERRUPT

    LET HOY = TODAY
    OPEN WINDOW tram0111 AT 2,2 WITH FORM "TRAM0111" ATTRIBUTE(BORDER)
    DISPLAY " TRAM011                CONSULTA CUENTAS REHABILITADAS                         " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)


    MENU "CONSULTA :"
        COMMAND "Rehabilitadas" "Consulta cuentas rehabilitadas"
	    CALL Inicializa()
	    CALL consulta()
        COMMAND "Salir" "Salir del Programa"
	    EXIT PROGRAM
    END MENU
END MAIN

FUNCTION Inicializa()
#i-------------------

    INITIALIZE master.* TO  NULL
    DISPLAY BY NAME master.*

END FUNCTION

FUNCTION Esta_seguro()

	PROMPT "Esta seguro(a) S/N ? " FOR CHAR enter
	IF enter MATCHES "[sS]" THEN
	   RETURN TRUE
	END IF
	RETURN FALSE
END FUNCTION

FUNCTION consulta()
#c-----------------
    DEFINE #loc #char
        txt_1                 CHAR(1000),
        x_busca	   	      CHAR(500)

    DEFINE 
        HAY_REGISTROS         SMALLINT

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " CONSULTA " AT 1,65 ATTRIBUTE ( REVERSE )
    DISPLAY " [ Esc ] Para Iniciar la busqueda   [ Ctrl-c ] Salir " AT 2,1
    CONSTRUCT BY NAME x_busca ON b.nss               ,
                                 b.fecha_rehabilita   
        ON KEY (ESC )
            LET INT_FLAG = FALSE
            EXIT CONSTRUCT

        ON KEY (INTERRUPT )
            LET INT_FLAG = FALSE
	    RETURN

    END CONSTRUCT


LET txt_1 = 
" SELECT b.nss               ,",
"        b.fecha_rehabilita  ,",
"        a.marca_causa       ,",
"        d.marca_desc        ,",
"        a.fecha_ini         ,",
"        b.marca_cod         ,",
"        c.marca_desc        ,",
"        b.monto_retiro      ,",
"        b.monto_cesantia    ,",
"        b.monto_voluntaria  ,",
"        b.monto_vivienda97  ,",
"        b.monto_cuota_soc   ,",
"        b.monto_sar         ,",
"        b.monto_vivienda92   ",
" FROM  safre_af:cta_his_inhabilitada  a ,",
"       safre_af:cta_rehabilitada      b ,",
"       tab_marca             c ,",
"       tab_marca             d  ",
" WHERE ",x_busca CLIPPED ,
" AND   a.nss     = b.nss        ",
" AND   b.marca_cod   = c.marca_cod ",
" AND   a.marca_causa = d.marca_cod ",
" ORDER BY b.fecha_rehabilita DESC"

LET txt_1 = txt_1 CLIPPED


    PREPARE cur1 FROM txt_1
    DECLARE cursor_1 SCROLL CURSOR FOR cur1

    LET HAY_REGISTROS = FALSE
    LET x_max = 0

    FOREACH cursor_1 INTO master.*
	IF master.marca_cod = 1 THEN
	   LET master.marca_desc1 = "RECAUDACION"
	END IF
      
        LET HAY_REGISTROS = TRUE
        LET x_max = x_max + 1
    END FOREACH

    IF NOT HAY_REGISTROS THEN
        ERROR "NO HAY REGISTROS "
        CALL Inicializa()
    ELSE
        OPEN cursor_1 
        FETCH FIRST cursor_1 INTO master.*
	IF master.marca_cod = 1 THEN
	   LET master.marca_desc1 = "RECAUDACION"
	END IF
        LET cuenta = 1
        DISPLAY "        " AT 21,74
        DISPLAY "(",cuenta,"/",x_max,")" AT 21,71 ATTRIBUTE(REVERSE)
        DISPLAY BY NAME master.*
        CALL despliega()
    END IF
END FUNCTION

FUNCTION Despliega()
#d------------------

    MENU "CUENTAS REHABILITADAS"
        COMMAND "Primero"   "Primer Registro "
	    CALL primero()

	COMMAND "Siguiente" "Siguiente Registro "
	    CALL siguiente()

	COMMAND "Anterior"  "Anterior Registro "
	    CALL anterior()

	COMMAND "Ultimo"    "Ultimo Registro "
	    CALL ultimo()

	COMMAND "Finalizar"   "Salir de la Busqueda"
            DISPLAY "        " AT 21,74
	    CALL Inicializa()
	    EXIT MENU
    END MENU
END FUNCTION

FUNCTION primero()
#p----------------
    LET cuenta = 1
    DISPLAY "        " AT 21,74
    DISPLAY "(",cuenta,"/",x_max,")" AT 21,71 ATTRIBUTE(REVERSE)
    ERROR "USTED LLEGO AL INICIO DE LA BUSQUEDA"

    FETCH FIRST cursor_1 INTO master.*
	IF master.marca_cod = 1 THEN
	   LET master.marca_desc1 = "RECAUDACION"
	END IF
      DISPLAY BY NAME master.*
END FUNCTION

FUNCTION ultimo()
#u---------------
    LET cuenta = x_max
    DISPLAY "        " AT 21,74
    DISPLAY "(",cuenta,"/",x_max,")" AT 21,71 ATTRIBUTE(REVERSE)
    ERROR "USTED LLEGO AL FINAL DE LA BUSQUEDA"

    FETCH LAST cursor_1 INTO master.*
	IF master.marca_cod = 1 THEN
	   LET master.marca_desc1 = "RECAUDACION"
	END IF
        DISPLAY BY NAME master.*
END FUNCTION

FUNCTION siguiente()
#s------------------
    LET cuenta = cuenta + 1
    DISPLAY "        " AT 21,74
    DISPLAY "(",cuenta,"/",x_max,")" AT 21,71 ATTRIBUTE(REVERSE)

    FETCH NEXT cursor_1 INTO master.*
	IF master.marca_cod = 1 THEN
	   LET master.marca_desc1 = "RECAUDACION"
	END IF
        IF STATUS = NOTFOUND THEN
            ERROR "USTED LLEGO AL FINAL DE LA BUSQUEDA"
            LET cuenta = cuenta - 1
            DISPLAY "        " AT 21,74
            DISPLAY "(",cuenta,"/",x_max,")" AT 21,71 ATTRIBUTE(REVERSE)
        END IF

        DISPLAY BY NAME master.*
END FUNCTION

FUNCTION anterior()
#a-----------------
    LET cuenta = cuenta - 1
    DISPLAY "        " AT 21,74
    DISPLAY "(",cuenta,"/",x_max,")" AT 21,71 ATTRIBUTE(REVERSE)

    FETCH PREVIOUS cursor_1 INTO master.*
	IF master.marca_cod = 1 THEN
	   LET master.marca_desc1 = "RECAUDACION"
	END IF
        IF STATUS = NOTFOUND THEN
            ERROR "USTED LLEGO AL INICIO DE LA BUSQUEDA"
            LET cuenta = cuenta + 1
            DISPLAY "        " AT 21,74
            DISPLAY "(",cuenta,"/",x_max,")" AT 21,71 ATTRIBUTE(REVERSE)
        END IF

        DISPLAY BY NAME master.*
END FUNCTION

