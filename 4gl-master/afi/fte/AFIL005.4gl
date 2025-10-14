########################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                     #
#Propietario       => E.F.P. 					       #
#Programa AFIL005  => CARTA DE ENVIO DE ACEPTACION AL AFILIADO (TRASP) #
#Sistema           => AFI  					       #
#Actualizacion     => MAURO MUNIZ CABALLERO.                           # 
#Fecha             => 19 DE ENERO DE 2001		               #
########################################################################
DATABASE safre_af
GLOBALS
	DEFINE w_aux  RECORD
            n_folio    LIKE afi_mae_afiliado.n_folio,
            n_seguro   LIKE afi_mae_afiliado.n_seguro,
            paterno    LIKE afi_mae_afiliado.paterno ,
            materno    LIKE afi_mae_afiliado.materno ,
            nombres    LIKE afi_mae_afiliado.nombres ,
            calle      LIKE afi_domicilio.calle ,
            numero     LIKE afi_domicilio.numero ,
            colonia    LIKE afi_domicilio.colonia ,
            delega     LIKE afi_domicilio.delega ,
            estado     LIKE afi_domicilio.estado ,
            codpos     LIKE afi_domicilio.codpos,
            ciudad     LIKE afi_domicilio.ciudad ,
            fentcons   LIKE afi_mae_afiliado.fentcons,
            telefono   LIKE afi_telefono.telefono 
	END RECORD

        DEFINE    g_reg RECORD 
               segur        CHAR(11),
               folio        CHAR(8),
               fecha        DATE
        END RECORD

        DEFINE parametrox      CHAR(5)
        DEFINE hoy             DATE
        DEFINE hora            CHAR(8)
        DEFINE desc            CHAR(50)
        DEFINE desc1           CHAR(50)
        DEFINE g_usuario       CHAR(8)
	DEFINE aux_pausa       CHAR(1)
	DEFINE g_afore	RECORD LIKE tab_afore_local.*
	DEFINE l_estado	       CHAR(16)
	DEFINE g_paramgrales   RECORD LIKE glo_parametro.*
	DEFINE G_LISTA         CHAR(100)
	DEFINE txt1  	       CHAR(600)

END GLOBALS

MAIN

      LET parametrox = ARG_VAL(1)

    OPTIONS INPUT WRAP  ,
    PROMPT LINE LAST    ,
    ACCEPT KEY CONTROL-I,
    FORM LINE 3
#DEFER INTERRUPT

    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "AFIL0051" ATTRIBUTE(BORDER)
    DISPLAY " AFIL005           CARTAS AFILIADOS POR TRASPASO ACEPTADOS                     " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY "                           < Ctrl-C > Salir                                " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)

       LET HOY = TODAY
       LET g_reg.fecha = TODAY
       LET hora = TIME

       INPUT BY NAME g_reg.* WITHOUT DEFAULTS
          AFTER FIELD segur
              IF g_reg.segur IS NULL THEN
                 NEXT FIELD folio
              ELSE 
                LET g_reg.fecha = NULL
               EXIT INPUT
              END IF
            AFTER FIELD folio
              IF fgl_lastkey() = fgl_keyval("UP") THEN
                 NEXT FIELD segur
              END IF
              IF g_reg.folio IS NULL THEN
                 NEXT FIELD fecha
              ELSE 
               LET g_reg.fecha = NULL
               EXIT INPUT
              END IF
            AFTER FIELD fecha
              IF fgl_lastkey() = fgl_keyval("UP") THEN
                 NEXT FIELD folio
              END IF
              IF g_reg.fecha IS NULL THEN 
                 NEXT FIELD segur
              END IF
           END INPUT
      WHILE TRUE

        CALL Pregunta()

	IF aux_pausa MATCHES "[SsNn]" THEN
           EXIT WHILE
        END IF
      END WHILE
	IF aux_pausa MATCHES "[Ss]" THEN
	    ERROR "Procesando Informacion.... Espere un momento"
	    SELECT *        ,
                   USER 
            INTO   g_afore.*,
                   g_usuario 
            FROM   tab_afore_local

           SELECT *
           INTO g_paramgrales.*
           FROM glo_parametro    

	    LET G_LISTA =  g_paramgrales.ruta_spool CLIPPED,"/",
                           g_usuario CLIPPED,".CARTAS_ACEP_TRASP.",
                           HOY USING "DD-MM-YY","_",hora CLIPPED
           START REPORT listado TO G_LISTA

     LET txt1 = "SELECT a.n_folio,a.n_seguro,a.paterno,a.materno,a.nombres ,",
                             " b.calle , b.numero, b.colonia , b.delega ,",
                             " b.estado, b.codpos, b.ciudad, a.fentcons ,",
                              "c.telefono ",
          " FROM afi_mae_afiliado a, OUTER (afi_domicilio b, afi_telefono c) ",
          " WHERE a.n_seguro = b.nss AND a.n_seguro = c.nss "

           IF g_reg.segur IS NOT NULL THEN
              LET txt1=txt1 CLIPPED," AND a.n_seguro = '",g_reg.segur,"'"
           END IF
           IF g_reg.folio IS NOT NULL THEN
              LET txt1=txt1 CLIPPED," AND a.n_folio = '",g_reg.folio,"'"
           END IF                                  
           IF g_reg.fecha IS NOT NULL THEN
              LET txt1=txt1 CLIPPED," AND a.fentcons = '",g_reg.fecha,"'"
           END IF

                PREPARE hhh FROM txt1
                DECLARE cur_1 CURSOR for hhh
                FOREACH cur_1 INTO w_aux.*
                    OUTPUT TO REPORT listado(w_aux.*)       
                END FOREACH 
            FINISH REPORT listado
                ERROR"LISTADO GENERADO" SLEEP 2
	    LET G_LISTA = "chmod 777 ",g_paramgrales.ruta_spool CLIPPED,"/",
                          g_usuario CLIPPED,".CARTAS_ACEP_TRASP.",
                          HOY USING "DD-MM-YY","_",hora CLIPPED
            RUN G_LISTA
        ELSE
            ERROR"PROCESO CANCELADO" SLEEP 2
        END IF                                  
	    EXIT PROGRAM
END MAIN

FUNCTION Pregunta()
#-----------------
    PROMPT "Desea Emitir Informe [S/N] ? " FOR aux_pausa
END FUNCTION

REPORT listado(w_aux)
#--------------------

	DEFINE w_aux  RECORD
            n_folio     LIKE afi_mae_afiliado.n_folio,
            n_seguro    LIKE afi_mae_afiliado.n_seguro,
            paterno     LIKE afi_mae_afiliado.paterno ,
            materno     LIKE afi_mae_afiliado.materno ,
            nombres     LIKE afi_mae_afiliado.nombres ,
            calle       LIKE afi_domicilio.calle ,
            numero      LIKE afi_domicilio.numero ,
            colonia     LIKE afi_domicilio.colonia ,
            delega      LIKE afi_domicilio.delega ,
            estado      LIKE afi_domicilio.estado ,
            codpos      LIKE afi_domicilio.codpos ,
            ciudad      LIKE afi_domicilio.ciudad ,
            fentcons    LIKE afi_mae_afiliado.fentcons ,
            telefono    LIKE afi_telefono.telefono
        END RECORD

	DEFINE 
            razon_social  CHAR(40)

        DEFINE w_col  RECORD
            status_interno  LIKE afi_mae_afiliado.status_interno ,
            dis_numero          CHAR(50)
       END RECORD

    OUTPUT
        PAGE LENGTH 60
	LEFT MARGIN 0
	RIGHT MARGIN 0
	TOP MARGIN 0
	BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER
        SELECT A.razon_social
        INTO   razon_social
        FROM   tab_afore_local A

	PRINT '\033e\033(12U\033&l0O\033&k0S\033&l6d\033(s0B'
        SKIP 4 lines
        PRINT
ON EVERY ROW
          SELECT ciudad_desc into desc 
          FROM   tab_ciudad
          WHERE  ciudad_cod = w_aux.ciudad

          SELECT estad_desc 
          INTO   desc1
          FROM   tab_estado
          WHERE  estad_cod = w_aux.estado

SKIP 2 LINES
         PRINT
            COLUMN 04,w_aux.nombres CLIPPED," ",w_aux.paterno CLIPPED," ",w_aux.materno CLIPPED ,"       ", w_aux.n_folio USING "&&&&&&"
        PRINT
            COLUMN 04,w_aux.calle CLIPPED," ",w_aux.numero
        PRINT
            COLUMN 04,"COL. ",w_aux.colonia CLIPPED
        PRINT
            COLUMN 04,w_aux.codpos USING "&&&&&"," ",desc CLIPPED
        PRINT
            COLUMN 04,desc1 CLIPPED
        SKIP 1 LINES
	PRINT '\033e\033(10U\033&l0O\033&k0S\033&l6d\033(s0B'
        PRINT
            COLUMN 04,"CONSTANCIA DE TRASPASO EN ADMINISTRADORA DE FONDOS PARA EL RETIRO. "
        SKIP 1 LINES
        PRINT
                      #123456789012345678901234567890123456789012345678901234567890123456789012
            COLUMN 04,"USTED HA QUEDADO  FORMALMENTE  REGISTRADO  EN  ESTA  ADMINISTRADORA EN"
        PRINT
            COLUMN 04,"CUMPLIMIENTO  A SU  SOLICITUD DE TRASPASO  PRESENTADA.  ESTE DOCUMENTO"
        PRINT
            COLUMN 04,"PODRA SER USADO EN LOS TRAMITES QUE REALICE CON DICHA ADMINISTRADORA." 
        PRINT
            COLUMN 04,"PARA TAL EFECTO VERIFIQUE QUE SUS DATOS PERSONALES ESTEN CORRECTAMENTE"
        PRINT
            COLUMN 04,"ESCRITOS Y  TOME  EN  CUENTA  QUE  AL  DOMICILIO  ESPECIFICADO EN ESTE"
        PRINT
            COLUMN 04,"DOCUMENTO  LE  SERAN  ENVIADOS  SUS  ESTADOS  DE  CUENTA  Y  DEMAS    "
        PRINT
            COLUMN 04,"INFORMACION  RELATIVA  A  SU  CUENTA  INDIVIDUAL.                     "
        PRINT
	PRINT '\033e\033(s218T\033&l0O\033&k0S\033&l6d\033(s9H\033(s7B'
        PRINT
            COLUMN 04,"NOMBRE : " ,w_aux.nombres CLIPPED," ",w_aux.paterno CLIPPED," ",w_aux.materno CLIPPED
        PRINT
            COLUMN 04,"NUMERO DE FOLIO DE SOLICITUD:  ",w_aux.n_folio USING "&&&&&&"
        PRINT
            COLUMN 04,"NUMERO DE SEGURIDAD SOCIAL: " ,w_aux.n_seguro USING "&&&&&&&&&&&"
        PRINT
            COLUMN 04,"FECHA DE APROBACION EN PROCESAR: ",w_aux.fentcons USING  "DD/MM/YYYY"
        PRINT
            COLUMN 04,"DOMICILIO: ",w_aux.calle CLIPPED," ",w_aux.numero
        PRINT
            COLUMN 04,"           Col. ",w_aux.colonia CLIPPED
        PRINT
            COLUMN 04,"           ",w_aux.codpos USING "&&&&&"," ",desc1 CLIPPED
        PRINT
            COLUMN 04,"           ",desc CLIPPED
        PRINT
            COLUMN 04,"TELEFONO:  ",w_aux.telefono USING "&&&&&&&"
	PRINT '\033e\033(14A\033&l0O\033&k0S\033&l6d\033(s0B'
        PRINT
 SKIP TO TOP OF PAGE
END REPORT

