########################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                     #
#Propietario       => E.F.P. 					       #
#Programa AFIL004  => CARTA DE ENVIO DE RECHAZO AL AFILIADO (REGISTRO) #
#Sistema           => AFI. 					       #
#Autor             => MAURO MUNIZ CABALLERO                            #
#Fecha             => 17 DE ENERO DE 2001	                       #
########################################################################
DATABASE safre_af
GLOBALS
	DEFINE w_aux  RECORD
            n_folio             LIKE afi_solicitud.n_folio ,
            paterno             LIKE afi_solicitud.paterno ,
            materno             LIKE afi_solicitud.materno ,
            nombres             LIKE afi_solicitud.nombres ,
            calle              LIKE afi_domicilio.calle ,
            numero               LIKE afi_domicilio.numero ,
            colonia            LIKE afi_domicilio.colonia ,
            delega             LIKE afi_domicilio.delega ,
            estado             LIKE afi_domicilio.estado ,
            codpos             LIKE afi_domicilio.codpos,
            ciudad             LIKE afi_domicilio.ciudad,
            observacion        CHAR(40),
            telefono               LIKE afi_telefono.telefono ,
            sexo                LIKE afi_solicitud.sexo 
	END RECORD
        DEFINE  g_reg    RECORD  
                segur          CHAR(11),
                folio          CHAR(10),
                fech           DATE 
        END RECORD

        DEFINE parametrox    CHAR(5)
        DEFINE hoy           DATE
        DEFINE hora          CHAR(8)
        DEFINE mes           SMALLINT
        DEFINE ano           SMALLINT
        DEFINE mesx          CHAR(10)
        DEFINE sexi          CHAR(10)
        DEFINE desc          CHAR(50)
        DEFINE desc1         CHAR(50)
        DEFINE g_usuario     CHAR(8)
	DEFINE aux_pausa     CHAR(1)
	DEFINE g_afore	     RECORD LIKE tab_afore_local.*
	DEFINE l_estado	     CHAR(16)
	DEFINE g_paramgrales RECORD LIKE glo_parametro.*
	DEFINE G_LISTA       CHAR(100)
	DEFINE txt1  	     CHAR(600)
END GLOBALS

MAIN

      LET HOY = TODAY
      LET g_reg.fech = TODAY
      LET hora = TIME

    OPTIONS INPUT WRAP  ,
    PROMPT LINE LAST    ,
    ACCEPT KEY CONTROL-I,
    FORM LINE 3
#DEFER INTERRUPT

    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "AFIL0041" ATTRIBUTE(BORDER)
    DISPLAY " AFIL004         CARTAS DE RECHAZOS DE AFILIACION (REGISTRO)                   " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY "                           < Ctrl-C > Salir                                " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)
       INPUT BY NAME g_reg.*  WITHOUT DEFAULTS
        AFTER FIELD segur
         IF g_reg.segur IS NULL THEN
           NEXT FIELD folio
         ELSE 
          LET g_reg.fech = NULL
          EXIT INPUT
         END IF
        AFTER FIELD folio
          IF fgl_lastkey() = fgl_keyval("UP") THEN
            NEXT FIELD segur
          END IF
         IF g_reg.folio is NULL THEN 
           NEXT FIELD fech
         ELSE 
          LET g_reg.fech = NULL
          EXIT INPUT
         END IF
        AFTER FIELD fech
          IF fgl_lastkey() = fgl_keyval("UP") THEN
            NEXT FIELD folio
          END IF
          IF g_reg.fech IS NULL THEN
            NEXT FIELD segur
         ELSE 
          EXIT INPUT
          END IF
        EXIT INPUT 
      END INPUT
        WHILE TRUE
        CALL Pregunta()
	IF aux_pausa MATCHES "[SsNn]" THEN
          exit while
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

	    LET G_LISTA =  g_paramgrales.ruta_spool CLIPPED,"/",g_usuario CLIPPED,".CARTAS_RECHAZOS_",HOY USING "DD-MM-YY","_",hora CLIPPED
           START REPORT listado TO G_LISTA

        LET txt1 = "SELECT  a.n_folio,a.paterno , a.materno , a.nombres ,",
                  " b.calle ,b.numero, b.colonia , b.delega ,",
                  " b.estado, b.codpos, b.ciudad, c.observacion,", 
                  " d.telefono, a.sexo", 
          " FROM afi_solicitud a , afi_rechaza_cert c, ",
          " OUTER (afi_domicilio b, afi_telefono d) "
          
         IF g_reg.segur IS NOT NULL THEN
           LET txt1 = txt1 CLIPPED," WHERE   a.n_seguro = c.n_seguro",
                                   " AND     a.n_seguro = '",g_reg.segur,"'",
                                   " AND a.n_seguro = b.nss " ,
                                   " AND a.n_seguro = d.nss " ,
                                   " AND a.tipo_solicitud = 1 "
         END IF
         IF g_reg.folio IS NOT NULL THEN
           LET txt1 = txt1 CLIPPED," WHERE   a.n_seguro = c.n_seguro",
                                   " AND     a.n_folio = '",g_reg.folio,"'",
                                   " AND a.n_seguro = b.nss " ,
                                   " AND a.n_seguro = d.nss " ,
                                   " AND a.tipo_solicitud = 1 "
         END IF
         IF g_reg.fech IS NOT NULL THEN
           LET txt1 = txt1 CLIPPED," WHERE   a.n_seguro = c.n_seguro",
                                   " AND     b.f_rechazo = '",g_reg.fech,"'",
                                   " AND a.n_seguro = b.nss " ,
                                   " AND a.n_seguro = d.nss " ,
                                   " AND a.tipo_solicitud = 1 "
         END IF
               PREPARE hhh from txt1
               DECLARE cur_1 CURSOR for hhh
                FOREACH cur_1 INTO w_aux.*
                    OUTPUT TO REPORT listado(w_aux.*)       
                END FOREACH 
            FINISH REPORT listado
                ERROR"LISTADO GENERADO" SLEEP 2
	    LET G_LISTA =  "chmod 777 ",g_paramgrales.ruta_spool CLIPPED ,"/",g_usuario CLIPPED,".CARTAS_RECHAZOS_",HOY USING "DD-MM-YY","_",hora CLIPPED
            RUN G_LISTA
        ELSE
            ERROR"PROCESO CANCELADO" SLEEP 2
        END IF                                  
	    EXIT PROGRAM
END MAIN

FUNCTION Pregunta()
#-----------------
    PROMPT "Desea Emitir Informe S/N ? " FOR aux_pausa
END FUNCTION

REPORT listado(w_aux)
#--------------------
	DEFINE w_aux  RECORD
            n_folio             LIKE afi_solicitud.n_folio ,
            paterno             LIKE afi_solicitud.paterno ,
            materno             LIKE afi_solicitud.materno ,
            nombres             LIKE afi_solicitud.nombres ,
            calle              LIKE afi_domicilio.calle ,
            numero               LIKE afi_domicilio.numero ,
            colonia            LIKE afi_domicilio.colonia ,
            delega             LIKE afi_domicilio.delega ,
            estado             LIKE afi_domicilio.estado ,
            codpos             LIKE afi_domicilio.codpos,
            ciudad             LIKE afi_domicilio.ciudad,
            observacion         CHAR(40),
            telefono               LIKE afi_telefono.telefono ,
            sexo                LIKE afi_solicitud.sexo 
    END RECORD
	DEFINE 
            razon_social                CHAR(40)
   DEFINE w_col   RECORD
            status_interno            LIKE afi_solicitud.status_interno ,
            numero          CHAR(50)
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

ON EVERY ROW
         LET hoy = TODAY
         Let mes=month(hoy)
         Let ano=year (hoy)
         CASE mes 
         WHEN 01  LET mesx = "ENERO"
         WHEN 02 LET mesx = "FEBRERO"
         WHEN 03 LET mesx = "MARZO"
         WHEN 04 LET mesx = "ABRIL"
         WHEN 05 LET mesx = "MAYO"
         WHEN 06 LET mesx = "JUNIO"
         WHEN 07 LET mesx = "JULIO"
         WHEN 08 LET mesx = "AGOSTO"
         WHEN 09 LET mesx = "SEPTIEMBRE"
         WHEN 10 LET mesx = "OCTUBRE"
         WHEN 11 LET mesx = "NOVIEMBRE"
         WHEN 12 LET mesx = "DICIEMBRE"
        END CASE

        CASE w_aux.sexo
        WHEN 1 LET sexi = "Sr. "
        WHEN 2 LET sexi = "Srita. "
        END CASE
        SKIP 1 lines
	PRINT '\033e\033(s218T\033(s7B'
        PRINT
          SELECT  ciudad_desc into desc from tab_ciudad
          WHERE  ciudad_cod = w_aux.ciudad

          SELECT  estad_desc into desc1 from tab_estado
          WHERE  estad_cod = w_aux.estado

        PRINT
            COLUMN 58,mesx CLIPPED, " ",ano
	PRINT '\033e\033(s218T\033(s9H\033(s7B'
         PRINT
         PRINT
            COLUMN 07,w_aux.nombres CLIPPED," ",w_aux.paterno CLIPPED," ",w_aux.materno CLIPPED , '\033e\033(s218T\033(s12H\033(s0B'
        PRINT
            COLUMN 09,w_aux.calle CLIPPED," ",w_aux.numero
        PRINT
            COLUMN 09,"COL. ",w_aux.colonia CLIPPED
        PRINT
            COLUMN 09,w_aux.codpos USING "&&&&&"," ",desc CLIPPED
        PRINT
            COLUMN 09,desc1 CLIPPED
        PRINT
            COLUMN 09,"TELEFONO : ",w_aux.telefono
        SKIP 1 LINES

	PRINT '\033e\033(s218T\033(s10H\033(s7B'
        PRINT
            COLUMN 08,sexi CLIPPED," ",w_aux.paterno CLIPPED ,":"
        PRINT
        PRINT
            COLUMN 08,"PRESENTE "
        SKIP 1 LINES
	PRINT '\033e\033(s218T\033(s12H\033(s0B'
        PRINT
        PRINT
            COLUMN 12,"POR MEDIO DE LA PRESENTE COMUNICAMOS A  USTED QUE NO HA PODIDO SER AFILIADO"
        PRINT
        PRINT
            COLUMN 08,"POR LA(S) SIGUIENTE(S) CAUSA(S): "
        PRINT
        PRINT
	PRINT '\033e\033(s218T\033(s12H\033(s7B'  
        PRINT
            COLUMN 08,w_aux.observacion [1,79]
        PRINT
            COLUMN 08,w_aux.observacion [80,100]
        PRINT
	PRINT '\033e\033(s218T\033(s12H\033(s0B'
        PRINT
        PRINT
            COLUMN 08,"PARA QUE ESTA SITUACION SE RESUELVA Y SU  AFILIACION A LA",'\033e\033(s218T\033(s9H\033(s7B' CLIPPED,"  AFORE BITAL ",'\033e\033(s218T\033(s12H\033(s0B'CLIPPED ," PUEDA "
        PRINT
        PRINT
            COLUMN 08,"LLEVARSE AL CABO, LE SOLICITAMOS QUE ACUDA A NUESTRA SUCURSAL MAS CERCANA A QUE"
        PRINT
        PRINT
            COLUMN 08,"CONTACTE A UNO DE NUESTROS AGENTES PROMOTORES, QUIENES ESTAN A SUS ORDENES."
        PRINT
        PRINT
        PRINT
        PRINT
        PRINT
            COLUMN 08,"AGRADECIENDO SU PREFERENCIA" 
        PRINT
        PRINT
	PRINT '\033e\033(s218T\033(s10H\033(s7B'
        PRINT
        PRINT
        PRINT
            COLUMN 08,"                     A T E N T A M E N T E "
        PRINT
        PRINT
        PRINT
        PRINT
        PRINT
        PRINT
 SKIP TO TOP OF PAGE
END REPORT
