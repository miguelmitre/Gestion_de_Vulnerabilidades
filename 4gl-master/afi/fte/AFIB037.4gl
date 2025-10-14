###########################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                        #
#Propietario       => E.F.P.                                              #
#Programa AFIB037  => GENERACION DE ARCHIVO CONSULTA NSS - PROCESAR       #
#Sistema           => AFI                                                 #
#Autor             => FERNANDO HERRERA HERNANDEZ                          #
#Fecha             => 09 DE MAYO DE 2007                                  #
###########################################################################

DATABASE safre_af

GLOBALS
  DEFINE g_reg       RECORD
    generar          CHAR(1)
  END RECORD

  DEFINE
    HOY              DATE,
    num              INTEGER,
    HAY_REGISTROS    INTEGER,
    enter            CHAR(1),
    g_usuario        CHAR(8),
    HORA             CHAR(8),
    comm             CHAR(500),
    comm1            CHAR(500)

  DEFINE 
    g_seg_modulo      RECORD LIKE seg_modulo.*

  --SV3
  DEFINE
    v_idtposolicitud  LIKE catalogosistema.idcatalogo,
    c_idtposolicitud  CHAR(40) 

  DEFINE 
    rg_arh     RECORD 
    n_foli     LIKE solicitudafi.nsolicitud,
    idtposol   LIKE solicitudafi.idtposolicitud,
    nssoc      LIKE datosacceso.valor,
    apaterno   LIKE persona.apellidopaterno,
    amaterno   LIKE persona.apellidomaterno,
    nombre     LIKE persona.nombre
    END RECORD,

    semaforo   SMALLINT,
    HOYDIA     DATE        

END GLOBALS

MAIN

  DISPLAY " "
  DISPLAY ".1"

  CALL STARTLOG('AFIB037.log')
  CALL inicio()

  DEFER INTERRUPT
  OPTIONS INPUT WRAP,
  PROMPT LINE LAST,
  ACCEPT KEY CONTROL-I

  CALL proceso_principal()   #pp

END MAIN

FUNCTION inicio()
#----------------

  SELECT *,USER
  INTO   g_seg_modulo.*,g_usuario
  FROM   seg_modulo
  WHERE  modulo_cod = 'afi'

  LET HOY    = TODAY
  LET HORA   = TIME
  LET num    = 0

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

  OPEN WINDOW ventana_1 AT 4,4 WITH FORM "AFIB0371" ATTRIBUTE(BORDER)

  DISPLAY " AFIB037     GENERACION ARCHIVO CONSULTA NSS PROCESAR                          " AT 3,1 ATTRIBUTE(REVERSE)
  DISPLAY "                               < Ctrl-C > Salir                                " AT 1,1 ATTRIBUTE(REVERSE)
  DISPLAY HOY USING"dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)
  DISPLAY g_seg_modulo.ruta_envio AT 10,10

  INPUT BY NAME g_reg.generar

    AFTER FIELD generar
      IF g_reg.generar IS NULL THEN
         ERROR "Campo NO puede ser NULO"
         NEXT FIELD generar
      END IF

      {IF g_reg.generar <> 1 AND g_reg.generar <> 2 THEN
         ERROR "Tipo de solicitudes a generar 1 Reg Ini y 2 Trasp."}

      IF g_reg.generar MATCHES "[nN]" THEN
       ERROR "PROCESO CANCELADO" SLEEP 2
       EXIT PROGRAM
      END IF

      IF g_reg.generar NOT MATCHES "[sS]" THEN
         ERROR "Debe teclear S o s"  SLEEP 2
         NEXT FIELD generar
      ELSE
         ERROR "Procesando informacion. . ."
         CALL rescata_valores()
         EXIT INPUT
      END IF

    ON KEY ( INTERRUPT )
       ERROR "PROCESO CANCELADO" SLEEP 2
       EXIT PROGRAM

  END INPUT

  PROMPT "NOMBRE ARCHIVO PLANO : ", comm CLIPPED,
  " /[Enter] para salir" FOR enter
  
END FUNCTION

FUNCTION rescata_valores()
#rv-----------------------

  LET HAY_REGISTROS = FALSE
  LET HOYDIA        = TODAY

  {SELECT idcatalogo 
    INTO v_idtposolicitud
    FROM catalogosistema
   WHERE cvenatural = g_reg.generar
     AND idpadre IN (SELECT idcatalogo 
                       FROM catalogosistema
                      WHERE cvenatural = "TPOSOLICITUD"
                        AND idpadre   IS NULL)

  LET c_idtposolicitud = v_idtposolicitud}

  SELECT COUNT(*) 
    INTO HAY_REGISTROS
    FROM solicitudafi sol      ,
   OUTER datosacceso dat1      ,
   OUTER persona pers          ,
         catalogosistema catsis
   #WHERE sol.idtposolicitud    = c_idtposolicitud
   WHERE sol.idtposolicitud    IN (26,27)
     AND sol.stamprocessafi    = 'Por Revisar'
     AND sol.stamcertificafi   = '15-Por Confirmar'
     AND sol.stamdigimageafi   = 'Completa Img'
     AND (sol.stamdocumentafi  = 'Espera Documentos MC'
      OR sol.stamdocumentafi   = 'Documentos Recibidos MC'
      OR sol.stamdocumentafi   = 'Documentos Validados MC')
     AND sol.idpersona         = dat1.idpersona
     AND sol.idpersona         = pers.idpersona
     AND sol.idtposolicitud    = catsis.idcatalogo
     AND (dat1.idtpoidnalterno IS NULL OR
         dat1.idtpoidnalterno IN (SELECT idtpoidnalterno
                                  FROM   tpoidnalterno
                                  WHERE  cvetpoidnalterno = 'NSS'))
     AND sol.fdoccomisionesfin >= TODAY

  LET comm  = g_seg_modulo.ruta_envio CLIPPED,
              "/E",HOYDIA USING "yyyymmdd" CLIPPED,
              ".SCT" CLIPPED

  LET comm1 = g_seg_modulo.ruta_envio CLIPPED,
              "/LIST",HOYDIA USING "yyyymmdd" CLIPPED,
              ".SCT" CLIPPED

  DISPLAY "Archivo : ", comm CLIPPED AT 14,5
 
  START REPORT listado  TO comm
  START REPORT listado1 TO comm1

  IF HAY_REGISTROS THEN

     #QUERY PRINCIPAL
     DECLARE c_1 CURSOR FOR 
     SELECT sol.nsolicitud,sol.idtposolicitud,dat1.valor,
            pers.apellidopaterno,pers.apellidomaterno,
            pers.nombre
       FROM solicitudafi sol,OUTER datosacceso dat1,
            OUTER persona pers, catalogosistema catsis
      WHERE sol.idtposolicitud    IN (26,27)
        AND sol.stamprocessafi    = 'Por Revisar'
        AND sol.stamcertificafi   = '15-Por Confirmar'
        AND sol.stamdigimageafi   = 'Completa Img'
        AND (sol.stamdocumentafi  = 'Espera Documentos MC'
         OR sol.stamdocumentafi   = 'Documentos Recibidos MC'
         OR sol.stamdocumentafi   = 'Documentos Validados MC')
        AND sol.idpersona         = dat1.idpersona
        AND sol.idpersona         = pers.idpersona
        AND sol.idtposolicitud    = catsis.idcatalogo
        AND (dat1.idtpoidnalterno IS NULL OR
             dat1.idtpoidnalterno IN (SELECT idtpoidnalterno
                                      FROM   tpoidnalterno
                                      WHERE  cvetpoidnalterno = 'NSS'))
        AND sol.fdoccomisionesfin >= TODAY

     FOREACH c_1 INTO rg_arh.n_foli, rg_arh.idtposol, rg_arh.nssoc,
                      rg_arh.apaterno, rg_arh.amaterno,rg_arh.nombre

       CASE rg_arh.idtposol
         WHEN 26 LET c_idtposolicitud = 1
         WHEN 27 LET c_idtposolicitud = 2
       END CASE

       CALL busca_ht_nss(rg_arh.n_foli, c_idtposolicitud, rg_arh.nssoc)
       RETURNING semaforo

       IF semaforo  = 0 THEN

          INSERT INTO afi_nss_consulta 
          VALUES(rg_arh.nssoc,
                 rg_arh.n_foli,
                 c_idtposolicitud,  
                 HOYDIA,
                 30,
                 NULL,
                 NULL,
                 g_usuario) 
          
          OUTPUT TO REPORT listado(rg_arh.n_foli,rg_arh.nssoc,rg_arh.apaterno,
                                   rg_arh.amaterno,rg_arh.nombre)

          OUTPUT TO REPORT listado1(rg_arh.n_foli,
                                    c_idtposolicitud,
                                    rg_arh.nssoc,
                                    rg_arh.apaterno,
                                    rg_arh.amaterno,
                                    rg_arh.nombre)

          LET num  = num + 1

       END IF

     END FOREACH

     DISPLAY "Total de registros: ", num AT 15,5 SLEEP 3

     ERROR "Proceso Finalizado, presione <ENTER> para continuar" 
     PROMPT "Nombre listado: ", comm1 FOR enter 
 
     FINISH REPORT listado
     FINISH REPORT listado1

     CALL limpia_nulos()

  ELSE
    DISPLAY "Program stopped, NO HAY SOLICITUDES A ENVIAR"  SLEEP 3
    EXIT PROGRAM
  END IF


END FUNCTION

FUNCTION  busca_ht_nss(n_foli, c_idtposolicitud, nssoc)
#bhn--------------------
  DEFINE n_foli           LIKE solicitudafi.nsolicitud,
         c_idtposolicitud CHAR(40),
         nssoc            LIKE datosacceso.valor,
         veces            SMALLINT

  LET veces = 0

  SELECT COUNT(*) 
    INTO veces
    FROM afi_nss_consulta
   WHERE nss            = nssoc
     AND n_folio        = n_foli
     AND tipo_solicitud = c_idtposolicitud

  IF veces IS NULL THEN
     LET veces = 0
  END IF 

  RETURN veces
END FUNCTION

REPORT listado(n_foli,nssoc,apaterno,amaterno,nombre)
#--------------------
  DEFINE n_foli   LIKE solicitudafi.nsolicitud,
         nssoc    LIKE datosacceso.valor,
         apaterno LIKE persona.apellidopaterno,
         amaterno LIKE persona.apellidomaterno,
         nombre   CHAR(40)

  OUTPUT
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0
        PAGE LENGTH 66

  FORMAT
    ON EVERY ROW
    PRINT COLUMN  01, nssoc USING "&&&&&&&&&&&",
          COLUMN  12, "01",
          COLUMN  14, apaterno,
          COLUMN  54, amaterno,
          COLUMN  94, nombre,
          COLUMN 134, "                                 ",
                      "                                  "

END REPORT

FUNCTION limpia_nulos()
#ln--------------------

    LET comm = "cd " ,g_seg_modulo.ruta_envio CLIPPED,
               "/ ; sed -e '/^$/d' ","E",HOYDIA USING "yyyymmdd" CLIPPED,
               ".SCT > informix"
    RUN comm

    LET comm = "cd " ,g_seg_modulo.ruta_envio CLIPPED,
               "/ ; mv informix  ","E",HOYDIA USING "yyyymmdd" CLIPPED,
               ".SCT "
    RUN comm

END FUNCTION

REPORT listado1(lfolio,ltipo_solicitud,lnss,lpaterno,lmaterno,lnombre)
#--------------------
  DEFINE lfolio          LIKE solicitudafi.nsolicitud,
         ltipo_solicitud SMALLINT,
         lnss            LIKE datosacceso.valor,
         lpaterno        LIKE persona.apellidopaterno,
         lmaterno        LIKE persona.apellidomaterno,
         lnombre         CHAR(40)

  OUTPUT
        LEFT MARGIN 0
        RIGHT MARGIN 0
        TOP MARGIN 0
        BOTTOM MARGIN 0
        PAGE LENGTH 66

  FORMAT
    FIRST PAGE HEADER
       PRINT COLUMN  01, "FOLIO|",
                         "TIPO SOLIC|",
                         "NSS|",
                         "PATERNO|",
                         "MATERNO|",
                         "NOMBRE|"
  
    ON EVERY ROW
       PRINT COLUMN  01, lfolio USING "##########",  "|",
                         ltipo_solicitud,            "|",
                         lnss   USING "&&&&&&&&&&&", "|",
                         lpaterno,                   "|",
                         lmaterno,                   "|",
                         lnombre

END REPORT
