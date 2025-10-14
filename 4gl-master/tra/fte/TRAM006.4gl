##############################################################################
#Owner             => E.F.P.
#Programa TRAM006  => CONSULTA SOLICITUDES RECHAZADAS DE TRASPASOS ICEFA-AFORE
#Fecha creacion    => 18 DE FEBRERO DE 1999
#By                => JESUS DAVID YANEZ MORENO
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Fecha de Mod      => 02 DE FEBRERO DEL 2005
#Sistema           => TRA-ICE-IMSS
##############################################################################
DATABASE safre_af
GLOBALS
    DEFINE master RECORD #glo #master
        folio                 LIKE tra_det_rechazo.folio             ,
        n_seguro              LIKE tra_det_rechazo.n_seguro          ,
        rfc                   LIKE tra_det_rechazo.rfc               ,
        cve_ced_cuenta        LIKE tra_det_rechazo.cve_ced_cuenta    ,
        fech_presentacion     LIKE tra_det_rechazo.fech_presentacion ,
        paterno               LIKE tra_det_rechazo.paterno           ,
        materno               LIKE tra_det_rechazo.materno           ,
        nombres               LIKE tra_det_rechazo.nombres           ,
{ DETALLE }
        n_seguro_ent          LIKE tra_det_rechazo.n_seguro_ent      ,
        rfc_ent               LIKE tra_det_rechazo.rfc_ent           ,
        nro_ctrl_icefa        LIKE tra_det_rechazo.nro_ctrl_icefa    ,
        nombre_trab_icefa     LIKE tra_det_rechazo.nombre_trab_icefa ,
        diag_proceso_1        LIKE tra_det_rechazo.diag_proceso_1    ,
        desc1                 CHAR(60)                            ,
        diag_proceso_2        LIKE tra_det_rechazo.diag_proceso_2    ,
        desc2                 CHAR(60)                            ,
        diag_proceso_3        LIKE tra_det_rechazo.diag_proceso_3    ,
        desc3                 CHAR(60)                            ,
        diag_proceso_4        LIKE tra_det_rechazo.diag_proceso_4    ,
        desc4                 CHAR(60)                            ,
        diag_proceso_5        LIKE tra_det_rechazo.diag_proceso_5    ,
        desc5                 CHAR(60)
    END RECORD

    DEFINE master_devol RECORD #glo #master_devol
        folio                 LIKE tra_det_devol.folio             ,
        n_seguro              LIKE tra_det_devol.n_seguro          ,
        rfc                   LIKE tra_det_devol.rfc               ,
        cve_ced_cuenta        LIKE tra_det_devol.cve_ced_cuenta    ,
        fech_solicitud        LIKE tra_det_devol.fech_solicitud    ,
        paterno               LIKE tra_det_devol.paterno           ,
        materno               LIKE tra_det_devol.materno           ,
        nombre                LIKE tra_det_devol.nombre            ,
{ DETALLE }
        n_seguro_ent          LIKE tra_det_devol.n_seguro_ent      ,
        rfc_ent               LIKE tra_det_devol.rfc_ent           ,
        nro_ctrl_icefa        LIKE tra_det_devol.nro_ctrl_icefa    ,
        nombre_trab_icefa     LIKE tra_det_devol.nombre_trab_icefa ,
        diag_proceso          LIKE tra_det_devol.diag_proceso      ,
        desc1                 CHAR(60)
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
    OPEN WINDOW tram0061 AT 2,2 WITH FORM "TRAM0061" ATTRIBUTE(BORDER)
    DISPLAY " TRAM006       CONSULTA RECHAZOS DE TRASPASO ICEFA-AFORE IMSS                  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

    MENU "CONSULTA :"
        COMMAND "Rechazos" "Consulta icefas rechazadas"
    DISPLAY " TRAM006       CONSULTA RECHAZOS DE TRASPASO ICEFA-AFORE IMSS                  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)
	    CALL Inicializa()
	    CALL rechazos() #r

        COMMAND "Devoluciones" "Consulta icefas devueltas"
    DISPLAY " TRAM006      CONSULTA DEVOLUCIONES DE TRASPASO ICEFA-AFORE IMSS               " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)
	    CALL Inicializa()
	    CALL devoluciones() #d

        COMMAND "No_atendidas" "Consulta de icefas no atendidas"
    DISPLAY " TRAM006     CONSULTA NO ATENDIDAS DE TRASPASO ICEFA-AFORE IMSS                " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)
	    CALL Inicializa()
	    CALL no_atendidas() #d

        COMMAND "Salir" "Salir del Programa"
	    EXIT PROGRAM
    END MENU
END MAIN

FUNCTION rechazos()
#r-----------------
    MENU "CONSULTA RECHAZOS DE ICEFAS "
        COMMAND "Consulta" "Consulta Rechazos de Icefas"
	    CALL Inicializa()
	    CALL consulta()

        COMMAND "Salir" "Salir del Programa"
	    EXIT MENU
    END MENU
END FUNCTION

FUNCTION Inicializa()
#i-------------------
    INITIALIZE master.* TO NULL
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
    CONSTRUCT BY NAME x_busca ON folio             ,
                                 n_seguro          ,
                                 rfc               ,
                                 cve_ced_cuenta    ,
                                 fech_presentacion ,
                                 paterno           ,
                                 materno           ,
                                 nombres        
        ON KEY (ESC )
            LET INT_FLAG = FALSE
            EXIT CONSTRUCT

        ON KEY (INTERRUPT )
            LET INT_FLAG = FALSE
	    RETURN

    END CONSTRUCT

    LET txt_1 = " SELECT folio             ,",
                "        n_seguro          ,",
                "        rfc               ,",
                "        cve_ced_cuenta    ,",
                "        fech_presentacion ,",
                "        paterno           ,",
                "        materno           ,",
                "        nombres           ,",
                "        n_seguro_ent      ,",
                "        rfc_ent           ,",
                "        nro_ctrl_icefa    ,",
                "        nombre_trab_icefa ,",
                "        diag_proceso_1,", '"','"',",",
                "        diag_proceso_2,",'"','"',",",
                "        diag_proceso_3,",'"','"',",",
                "        diag_proceso_4,",'"','"',",",

                "        diag_proceso_5,",'"','"'," ",

		" FROM   tra_det_rechazo ",
                " WHERE ",x_busca CLIPPED
    PREPARE cur1 FROM txt_1
    DECLARE cursor_1 SCROLL CURSOR FOR cur1

    LET HAY_REGISTROS = FALSE
    LET x_max = 0
    FOREACH cursor_1 INTO master.*
        LET HAY_REGISTROS = TRUE
        LET x_max = x_max + 1
    END FOREACH

    IF NOT HAY_REGISTROS THEN
        ERROR "NO HAY REGISTROS "
        CALL Inicializa()
    ELSE
        OPEN cursor_1 
        FETCH FIRST cursor_1 INTO master.*
        LET cuenta = 1
        DISPLAY "        " AT 20,70
        DISPLAY "(",cuenta,")" AT 20,70 ATTRIBUTE(REVERSE)
        #DISPLAY BY NAME master.*
        CALL rescata_diag(master.diag_proceso_1,
                          master.diag_proceso_2,
                          master.diag_proceso_3,
                          master.diag_proceso_4,
                          master.diag_proceso_5
                         ) 
      RETURNING master.desc1,
                master.desc2,
                master.desc3,
                master.desc4,
                master.desc5
      DISPLAY BY NAME master.*
        CALL despliega()
    END IF
END FUNCTION

FUNCTION Despliega()
#d------------------
    MENU "RECHAZOS"
        COMMAND "Primero"   "Primer Rechazo de Icefa encontrado"
	    CALL primero()

	COMMAND "Siguiente" "Siguiente Rechazo de Icefa encontrado"
	    CALL siguiente()

	COMMAND "Anterior"  "Anterior Rechazo de Icefa encontrado"
	    CALL anterior()

	COMMAND "Ultimo"    "Ultimo Rechazo de Icefa encontrado"
	    CALL ultimo()

	COMMAND "Finalizar"   "Salir de la Busqueda"
            DISPLAY "        " AT 20,70
	    CALL Inicializa()
	    EXIT MENU
    END MENU
END FUNCTION

FUNCTION primero()
#p----------------
    LET cuenta = 1
    DISPLAY "        " AT 20,70
    DISPLAY "(",cuenta,")" AT 20,70 ATTRIBUTE(REVERSE)
    ERROR "USTED LLEGO AL INICIO DE LA BUSQUEDA"

    FETCH FIRST cursor_1 INTO master.*
        CALL rescata_diag(master.diag_proceso_1,
                          master.diag_proceso_2,
                          master.diag_proceso_3,
                          master.diag_proceso_4,
                          master.diag_proceso_5
                         ) 
      RETURNING master.desc1,
                master.desc2,
                master.desc3,
                master.desc4,
                master.desc5
      DISPLAY BY NAME master.*
END FUNCTION

FUNCTION ultimo()
#u---------------
    LET cuenta = x_max
    DISPLAY "        " AT 20,70
    DISPLAY "(",cuenta,")" AT 20,70 ATTRIBUTE(REVERSE)
    ERROR "USTED LLEGO AL FINAL DE LA BUSQUEDA"

    FETCH LAST cursor_1 INTO master.*
        CALL rescata_diag(master.diag_proceso_1,
                          master.diag_proceso_2,
                          master.diag_proceso_3,
                          master.diag_proceso_4,
                          master.diag_proceso_5
                         ) 
        RETURNING master.desc1,
                  master.desc2,
                  master.desc3,
                  master.desc4,
                  master.desc5
        DISPLAY BY NAME master.*
END FUNCTION

FUNCTION siguiente()
#s------------------
    LET cuenta = cuenta + 1
    DISPLAY "        " AT 20,70
    DISPLAY "(",cuenta,")" AT 20,70 ATTRIBUTE(REVERSE)

    FETCH NEXT cursor_1 INTO master.*
        IF STATUS = NOTFOUND THEN
            ERROR "USTED LLEGO AL FINAL DE LA BUSQUEDA"
            LET cuenta = cuenta - 1
            DISPLAY "        " AT 20,70
            DISPLAY "(",cuenta,")" AT 20,70 ATTRIBUTE(REVERSE)
        END IF

        CALL rescata_diag(master.diag_proceso_1,
                          master.diag_proceso_2,
                          master.diag_proceso_3,
                          master.diag_proceso_4,
                          master.diag_proceso_5
                         ) 
        RETURNING master.desc1,
                  master.desc2,
                  master.desc3,
                  master.desc4,
                  master.desc5
        DISPLAY BY NAME master.*
END FUNCTION

FUNCTION anterior()
#a-----------------
    LET cuenta = cuenta - 1
    DISPLAY "        " AT 20,70
    DISPLAY "(",cuenta,")" AT 20,70 ATTRIBUTE(REVERSE)

    FETCH PREVIOUS cursor_1 INTO master.*
        IF STATUS = NOTFOUND THEN
            ERROR "USTED LLEGO AL INICIO DE LA BUSQUEDA"
            LET cuenta = cuenta + 1
            DISPLAY "        " AT 20,70
            DISPLAY "(",cuenta,")" AT 20,70 ATTRIBUTE(REVERSE)
        END IF

        CALL rescata_diag(master.diag_proceso_1,
                          master.diag_proceso_2,
                          master.diag_proceso_3,
                          master.diag_proceso_4,
                          master.diag_proceso_5
                         )
        RETURNING master.desc1,
                  master.desc2,
                  master.desc3,
                  master.desc4,
                  master.desc5
        DISPLAY BY NAME master.*
END FUNCTION

FUNCTION rescata_diag(a,b,c,d,e)
#rd-----------------------------
    DEFINE #loc #integer
        a,b,c,d,e             INTEGER

    DEFINE
        aa,bb,cc,dd,ee        CHAR(60)

    SELECT des_error 
    INTO   aa 
    FROM   tab_rch_icefa 
    WHERE  cod_error = a

    IF STATUS = NOTFOUND THEN
        LET aa = "NO EXISTE DESCRIPCION" 
    END IF

    SELECT des_error
    INTO   bb 
    FROM   tab_rch_icefa
    WHERE  cod_error = b

    IF STATUS = NOTFOUND THEN
        LET bb = "NO EXISTE DESCRIPCION"
    END IF

    SELECT des_error
    INTO   cc
    FROM   tab_rch_icefa 
    WHERE  cod_error = c

    IF STATUS = NOTFOUND THEN 
        LET cc = "NO EXISTE DESCRIPCION"
    END IF

    SELECT des_error
    INTO   dd 
    FROM   tab_rch_icefa
    WHERE  cod_error = d

    IF STATUS = NOTFOUND THEN
        LET dd = "NO EXISTE DESCRIPCION"
    END IF

    SELECT des_error 
    INTO   ee 
    FROM   tab_rch_icefa 
    WHERE  cod_error = e

    IF STATUS = NOTFOUND THEN 
        LET ee = "NO EXISTE DESCRIPCION" 
    END IF
    RETURN aa,bb,cc,dd,ee
END FUNCTION

----------------AQUI COMIENZA EL seg_modulo DE DEVOLUCIONES DE ICEFAS---------------

FUNCTION devoluciones()
#d---------------------
    OPEN WINDOW tram0062 AT 2,2 WITH FORM "TRAM0062" ATTRIBUTE(BORDER)
    DISPLAY " TRAM006    CONSULTA DEVOLUCIONES DE TRASPASO ICEFA-AFORE IMSS                 " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)

    MENU "CONSULTA DEVOLUCIONES DE ICEFAS "
        COMMAND "Consulta" "Consulta devoluciones de Icefas"
	    CALL inicializa_devol() #id
	    CALL consulta_devol() #cd

        COMMAND "Salir" "Salir del Programa"
	    EXIT MENU
    END MENU
    CLOSE WINDOW tram0062
END FUNCTION

FUNCTION inicializa_devol()
#id------------------------
    INITIALIZE master_devol.* TO NULL
    DISPLAY BY NAME master_devol.*
END FUNCTION

FUNCTION esta_seguro_devol()
#esd------------------------
    PROMPT "Esta seguro(a) S/N ? " FOR CHAR enter
    IF enter MATCHES "[sS]" THEN
       RETURN TRUE
    END IF
    RETURN FALSE
END FUNCTION

FUNCTION consulta_devol()
#cd----------------------
    DEFINE #loc #char
        txt_1                 ,
        x_busca	   	      CHAR(500)

    DEFINE 
        sw_1                  ,
        HAY_REGISTROS         SMALLINT

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " CONSULTA " AT 1,65 ATTRIBUTE ( REVERSE )
    DISPLAY " [ Esc ] Para Iniciar la busqueda   [ Ctrl-c ] Salir " AT 2,1

    LET sw_1 = 0
    CONSTRUCT BY NAME x_busca ON folio             ,
                                 n_seguro          ,
                                 rfc               ,
                                 cve_ced_cuenta    ,
                                 fech_solicitud    ,
                                 paterno           ,
                                 materno           ,
                                 nombre         
        ON KEY (ESC )
            LET INT_FLAG = FALSE
            EXIT CONSTRUCT

        ON KEY (INTERRUPT )
            LET INT_FLAG = FALSE
            LET sw_1 = 1
            EXIT CONSTRUCT

    END CONSTRUCT
#ff
    IF sw_1 = 1 THEN
        RETURN
    END IF

    LET txt_1 = " SELECT folio             ,",
                "        n_seguro          ,",
                "        rfc               ,",
                "        cve_ced_cuenta    ,",
                "        fech_solicitud    ,",
                "        paterno           ,",
                "        materno           ,",
                "        nombre            ,",
                "        n_seguro_ent      ,",
                "        rfc_ent           ,",
                "        nro_ctrl_icefa    ,",
                "        nombre_trab_icefa ,",
                "        status_registro,",
               '"','"',"",
		" FROM   tra_det_devol ",
                " WHERE ",x_busca CLIPPED
    PREPARE cur2 FROM txt_1
    DECLARE cursor_2 SCROLL CURSOR FOR cur2

    LET HAY_REGISTROS = FALSE
    LET x_max = 0
    FOREACH cursor_2 INTO master.*
        LET HAY_REGISTROS = TRUE
        LET x_max = x_max + 1
    END FOREACH

    IF NOT HAY_REGISTROS THEN
        ERROR "NO HAY REGISTROS "
        CALL inicializa_devol()
    ELSE
        OPEN cursor_2 
        FETCH FIRST cursor_2 INTO master_devol.*
        CALL rescata_diag_devol(master_devol.diag_proceso) #rdd
        RETURNING master_devol.desc1
        LET cuenta = 1
        DISPLAY "        " AT 20,70
        DISPLAY "(",cuenta,")" AT 20,70 ATTRIBUTE(REVERSE)
        DISPLAY BY NAME master_devol.*
        CALL despliega_devol() #dd
    END IF
END FUNCTION

FUNCTION despliega_devol()
#dd-----------------------
    MENU "RECHAZOS"
        COMMAND "Primero"   "Primera devolucion de Icefa encontrada"
	    CALL primero_devol() #pd

	COMMAND "Siguiente" "Siguiente devolucion de Icefa encontrada"
	    CALL siguiente_devol() #sd

	COMMAND "Anterior"  "Anterior devolucion de Icefa encontrada"
	    CALL anterior_devol() #ad

	COMMAND "Ultimo"    "Ultima devolucion de Icefa encontrada"
	    CALL ultimo_devol() #ud

	COMMAND "Finalizar"   "Salir de la Busqueda"
            DISPLAY "        " AT 20,70
	    CALL inicializa_devol() #id
	    EXIT MENU
    END MENU
END FUNCTION

FUNCTION primero_devol()
#pd---------------------
    LET cuenta = 1
    DISPLAY "        " AT 20,70
    DISPLAY "(",cuenta,")" AT 20,70 ATTRIBUTE(REVERSE)
    ERROR "USTED LLEGO AL INICIO DE LA BUSQUEDA"

    FETCH FIRST cursor_2 INTO master_devol.*
        CALL rescata_diag_devol(master_devol.diag_proceso) #rdd
        RETURNING master_devol.desc1
        DISPLAY BY NAME master_devol.*
END FUNCTION

FUNCTION ultimo_devol()
#ud--------------------
    LET cuenta = x_max
    DISPLAY "        " AT 20,70
    DISPLAY "(",cuenta,")" AT 20,70 ATTRIBUTE(REVERSE)
    ERROR "USTED LLEGO AL FINAL DE LA BUSQUEDA"

    FETCH LAST cursor_2 INTO master_devol.*
        CALL rescata_diag_devol(master_devol.diag_proceso) #rdd
        RETURNING master_devol.desc1
        DISPLAY BY NAME master_devol.*
END FUNCTION

FUNCTION siguiente_devol()
#sd-----------------------
    LET cuenta = cuenta + 1
    DISPLAY "        " AT 20,70
    DISPLAY "(",cuenta,")" AT 20,70 ATTRIBUTE(REVERSE)

    FETCH NEXT cursor_2 INTO master_devol.*
        IF STATUS = NOTFOUND THEN
            ERROR "USTED LLEGO AL FINAL DE LA BUSQUEDA"
            LET cuenta = cuenta - 1
            DISPLAY "        " AT 20,70
            DISPLAY "(",cuenta,")" AT 20,70 ATTRIBUTE(REVERSE)
        END IF

        CALL rescata_diag_devol(master_devol.diag_proceso) #rdd
        RETURNING master_devol.desc1
        DISPLAY BY NAME master_devol.*
END FUNCTION

FUNCTION anterior_devol()
#ad----------------------
    LET cuenta = cuenta - 1
    DISPLAY "        " AT 20,70
    DISPLAY "(",cuenta,")" AT 20,70 ATTRIBUTE(REVERSE)

    FETCH PREVIOUS cursor_2 INTO master_devol.*
        IF STATUS = NOTFOUND THEN
            ERROR "USTED LLEGO AL INICIO DE LA BUSQUEDA"
            LET cuenta = cuenta + 1
            DISPLAY "        " AT 20,70
            DISPLAY "(",cuenta,")" AT 20,70 ATTRIBUTE(REVERSE)
        END IF

        CALL rescata_diag_devol(master_devol.diag_proceso) #rdd
        RETURNING master_devol.desc1
        DISPLAY BY NAME master_devol.*
END FUNCTION

FUNCTION rescata_diag_devol(a)
#rdd--------------------------
    DEFINE #loc #integer
        a                     INTEGER

    DEFINE
        aa                    CHAR(60)

{
    IF a IS NULL THEN
        RETURN
    END IF
}
    SELECT des_devol 
    INTO   aa 
    FROM   tab_dev_icefa 
    WHERE  cod_devol = a
    GROUP BY 1

    IF STATUS = NOTFOUND THEN
        LET aa = "NO EXISTE DESCRIPCION" 
    END IF
    RETURN aa
END FUNCTION

FUNCTION no_atendidas()
#r-----------------

    DISPLAY " TRAM006     CONSULTA NO ATENDIDOS DE TRASPASO ICEFA-AFORE IMSS               " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)
    MENU "CONSULTA NO ATENDIDAS POR ICEFAS "
        COMMAND "Consulta" "Consulta No Atendidas de Icefas"
	    CALL Inicializa()
	    CALL consulta_na()
        COMMAND "Salir" "Salir del Programa"
	    EXIT MENU
    END MENU
END FUNCTION

FUNCTION consulta_na()
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
    CONSTRUCT  x_busca ON folio   , 
                          n_seguro ,
                          rfc     , 
                          cve_ced_cuenta  ,
                          fech_presentacion ,
                          paterno        ,
                          materno    ,
                          nombres  ,
			  orig_tipo_traspaso
		  FROM    folio,
			  n_seguro ,
			  rfc      ,
			  cve_ced_cuenta,
			  fech_presentacion,
			  paterno,
			  materno,
			  nombres ,
			  diag_proceso_1
        ON KEY (ESC )
            LET INT_FLAG = FALSE
            EXIT CONSTRUCT

        ON KEY (INTERRUPT )
            LET INT_FLAG = FALSE
	    RETURN

    END CONSTRUCT

    LET txt_1 = " SELECT folio             ,",
                "        n_seguro          ,",
                "        rfc               ,",
                "        cve_ced_cuenta    ,",
                "        fech_presentacion ,",
                "        paterno           ,",
                "        materno           ,",
                "        nombres           ,",
                "        n_seguro_ent_ced  ,",
                "        rfc_ent_ced       ,",
                "        nro_ctrl_icefa    ,",
                "        nom_comp_tra_icefa,",
		"        orig_tipo_traspaso ",

		" FROM   tra_det_no_aten ",
                " WHERE ",x_busca CLIPPED
    PREPARE cur8 FROM txt_1
    DECLARE cursor_8 SCROLL CURSOR FOR cur8

    LET HAY_REGISTROS = FALSE
    LET x_max = 0
    FOREACH cursor_8 INTO master.*
        LET HAY_REGISTROS = TRUE
        LET x_max = x_max + 1
    END FOREACH

    IF NOT HAY_REGISTROS THEN
        ERROR "NO HAY REGISTROS "
        CALL Inicializa()
    ELSE
        OPEN cursor_8 
        FETCH FIRST cursor_8 INTO master.*

        CASE master.diag_proceso_1 
	WHEN "01"
	  LET master.desc1 = "NO ATENDIDA"
	  EXIT CASE
	WHEN "10"
	  LET master.desc1 = "LIBERADA DE TRASPASO"
	  EXIT CASE
	WHEN "11"
	  LET master.desc1 = "NO ATENDIDA"
	  EXIT CASE
        OTHERWISE 
	  EXIT CASE
	END CASE

        LET cuenta = 1
        DISPLAY "        " AT 20,70
        DISPLAY "(",cuenta,")" AT 20,70 ATTRIBUTE(REVERSE)
        DISPLAY BY NAME master.*
        CALL despliega_na()
    END IF
END FUNCTION

FUNCTION despliega_na()
#d------------------

    MENU "RECHAZOS"
        COMMAND "Primero"   "Primer NA de Icefa encontrado"
	    CALL primero_na()

	COMMAND "Siguiente" "Siguiente NA de Icefa encontrado"
	    CALL siguiente_na()

	COMMAND "Anterior"  "Anterior NA de Icefa encontrado"
	    CALL anterior_na()

	COMMAND "Ultimo"    "Ultimo NA de Icefa encontrado"
	    CALL ultimo_na()

	COMMAND "Finalizar"   "Salir de la Busqueda"
            DISPLAY "        " AT 20,70
	    CALL Inicializa()
	    EXIT MENU
    END MENU
END FUNCTION


FUNCTION ultimo_na()
#u---------------
    LET cuenta = x_max
    DISPLAY "        " AT 20,70
    DISPLAY "(",cuenta,")" AT 20,70 ATTRIBUTE(REVERSE)
    ERROR "USTED LLEGO AL FINAL DE LA BUSQUEDA"

    FETCH LAST cursor_8 INTO master.*
        DISPLAY BY NAME master.*
END FUNCTION

FUNCTION siguiente_na()
#s------------------
    LET cuenta = cuenta + 1
    DISPLAY "        " AT 20,70
    DISPLAY "(",cuenta,")" AT 20,70 ATTRIBUTE(REVERSE)

    FETCH NEXT cursor_8 INTO master.*
        IF STATUS = NOTFOUND THEN
            ERROR "USTED LLEGO AL FINAL DE LA BUSQUEDA"
            LET cuenta = cuenta - 1
            DISPLAY "        " AT 20,70
            DISPLAY "(",cuenta,")" AT 20,70 ATTRIBUTE(REVERSE)
        END IF
        DISPLAY BY NAME master.*
END FUNCTION

FUNCTION anterior_na()
#a-----------------
    LET cuenta = cuenta - 1
    DISPLAY "        " AT 20,70
    DISPLAY "(",cuenta,")" AT 20,70 ATTRIBUTE(REVERSE)

    FETCH PREVIOUS cursor_8 INTO master.*
        IF STATUS = NOTFOUND THEN
            ERROR "USTED LLEGO AL INICIO DE LA BUSQUEDA"
            LET cuenta = cuenta + 1
            DISPLAY "        " AT 20,70
            DISPLAY "(",cuenta,")" AT 20,70 ATTRIBUTE(REVERSE)
        END IF
        DISPLAY BY NAME master.*
END FUNCTION

FUNCTION primero_na()
#p----------------
    LET cuenta = 1
    DISPLAY "        " AT 20,70
    DISPLAY "(",cuenta,")" AT 20,70 ATTRIBUTE(REVERSE)
    ERROR "USTED LLEGO AL INICIO DE LA BUSQUEDA"

    FETCH FIRST cursor_8 INTO master.*
      DISPLAY BY NAME master.*
END FUNCTION
