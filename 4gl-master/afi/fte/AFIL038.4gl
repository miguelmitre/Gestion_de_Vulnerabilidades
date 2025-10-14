###########################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                        #
#Propietario       => E.F.P.                                              #
#Programa AFIL038  => CONSULTA NSS AFORE - PROCESAR                       #
#Sistema           => AFI                                                 #
#Autor             => PEDRO JIMENEZ FERRER                                #
#Fecha             => 31 DE JULIO DE 2007                                 #
#Modifico          => FERNANDO HERRERA HERNANDEZ                          #
#Fecha             => 10 DE AGOSTO DE 2007                                #
###########################################################################
DATABASE safre_af
GLOBALS
  DEFINE  
    tot_arhConsu        INTEGER,
    garr ARRAY[10500]   OF RECORD
                        id          INTEGER,
                        tpSol       SMALLINT,
                        nfol        DECIMAL(8,0),
                        nss         CHAR(11),
                        frecep      DATE,
                        fenvio      DATE,
                        st_int      SMALLINT,
                        cod_op      CHAR(02),
                        diag_proc   CHAR(10),
                        nomb        CHAR(66)
                        END RECORD,
    fecha_ini           DATE,
    fecha_fin           DATE,
    g_seg_modulo RECORD LIKE seg_modulo.*,
    g_usuario           CHAR(8),
    g_imprime           CHAR(200),
    gimpresion          CHAR(300),
    HOY                 DATE

END GLOBALS

MAIN

  DEFER INTERRUPT
  OPTIONS PROMPT LINE 20
  CALL inicio()
  CALL pantalla()

END MAIN

FUNCTION pantalla()

  OPEN WINDOW vtna_csta AT 2,2 WITH FORM "AFIL038" ATTRIBUTE(BORDER)
  DISPLAY " AFIL038           CONSULTA DE NSS EN ARCHIVO CONSULTA",        
          "               " AT 3,1 ATTRIBUTE(REVERSE)
  DISPLAY "                              < CTRL-C > Salir                                                               " AT 1,1 ATTRIBUTE(REVERSE)
  DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)

  INPUT BY NAME fecha_ini, fecha_fin WITHOUT DEFAULTS

    AFTER FIELD fecha_ini
      IF fecha_ini IS NULL THEN
         ERROR "Fecha inicial no puede ser nulo."
         NEXT FIELD fecha_ini
      ELSE
         NEXT FIELD fecha_fin
      END IF

    AFTER FIELD fecha_fin
      IF fecha_fin IS NULL THEN
         ERROR "Fecha final no puede ser nulo."
         NEXT FIELD fecha_ini
      END IF

      IF fecha_ini > fecha_fin THEN
         ERROR "La fecha inicial no puede ser mayor a la fecha final."
         NEXT FIELD fecha_fin
      ELSE
         CALL valida()

         IF tot_arhConsu > 0 THEN
            ERROR "  PROCESANDO INFORMACION.... " 
            SLEEP 3
            CALL datos()
         ELSE
            ERROR "  NO EXISTE  INFORMACION.... "
            SLEEP 3
         END IF
         EXIT INPUT
      END IF

    ON KEY ( INTERRUPT )
       EXIT PROGRAM

  END INPUT

END FUNCTION

FUNCTION inicio()
#----------------

  SELECT *,USER
  INTO   g_seg_modulo.*,g_usuario
  FROM   seg_modulo
  WHERE  modulo_cod = 'afi'

  LET fecha_ini = TODAY
  LET fecha_fin = TODAY
  LET HOY       = TODAY

END FUNCTION

FUNCTION valida()

  SELECT COUNT(*) INTO tot_arhConsu
    FROM afi_nss_consulta
   WHERE fecha_envio >= fecha_ini
    AND  fecha_envio <= fecha_fin

  IF tot_arhConsu IS NULL THEN
     LET tot_arhConsu = 0         
  END IF

END FUNCTION

FUNCTION datos()
  DEFINE 
    lr                 RECORD
                       tpSol       SMALLINT,
                       nfol        DECIMAL(8,0),
                       nss         CHAR(11),
                       fenvio      DATE,
                       st_int      SMALLINT,
                       cod_op      CHAR(02),
                       diag_proc   CHAR(15)
                       END RECORD,
    frecep             DATE,
    nombre             LIKE persona.nombre,
    apellidopaterno     LIKE persona.apellidopaterno,
    apellidomaterno     LIKE persona.apellidomaterno,
    nomb               CHAR(66),
    tipoSoli           SMALLINT,
    z                  SMALLINT

  LET z = 1 
 
  LET g_imprime = g_seg_modulo.ruta_listados CLIPPED,"/",
                     g_usuario CLIPPED, ".arhConsPro.",
                     HOY USING "DDMMYYYY" CLIPPED

  START REPORT rptArhCns TO g_imprime

  DECLARE cur_info CURSOR FOR
  SELECT anc.tipo_solicitud, anc.n_folio, anc.nss, anc.fecha_envio,
         anc.status_interno, anc.cod_operacion, anc.diag_proceso
    FROM afi_nss_consulta anc
   WHERE anc.fecha_envio >= fecha_ini
    AND  anc.fecha_envio <= fecha_fin
  ORDER BY anc.fecha_envio, anc.tipo_solicitud, anc.n_folio
  FOREACH cur_info INTO lr.tpSol, lr.nfol, lr.nss, lr.fenvio,
                        lr.st_int, lr.cod_op, lr.diag_proc

    CASE lr.tpSol
      WHEN 1 LET tipoSoli = 26
      WHEN 2 LET tipoSoli = 27 
    END CASE

    SELECT saf.frecepcion, per.nombre, per.apellidopaterno,
           per.apellidomaterno
      INTO frecep, nombre, apellidopaterno, apellidomaterno
      FROM solicitudafi saf, OUTER persona per
     WHERE saf.idtposolicitud = tipoSoli
       AND saf.nsolicitud     = lr.nfol
       AND saf.idpersona      = per.idpersona

    IF STATUS = NOTFOUND THEN

       LET frecep            = NULL
       LET nombre            = NULL
       LET apellidopaterno   = NULL
       LET apellidomaterno   = NULL
       LET nomb              = NULL
       LET garr[z].frecep    = frecep
       LET garr[z].nomb      = nomb CLIPPED

    END IF

    LET nomb = nombre CLIPPED, " ",
               apellidopaterno CLIPPED, " ",
               apellidomaterno CLIPPED

    LET garr[z].id        = z
    LET garr[z].tpSol     = lr.tpSol     
    LET garr[z].nfol      = lr.nfol          
    LET garr[z].nss       = lr.nss     
    LET garr[z].frecep    = frecep
    LET garr[z].fenvio    = lr.fenvio
    LET garr[z].st_int    = lr.st_int
    LET garr[z].cod_op    = lr.cod_op
    LET garr[z].diag_proc = lr.diag_proc
    LET garr[z].nomb      = nomb CLIPPED

    OUTPUT TO REPORT rptArhCns(z,lr.tpSol,lr.nfol,lr.nss,frecep,lr.fenvio,
                               lr.st_int,lr.cod_op,lr.diag_proc,nomb)

    LET z = z + 1   

  END FOREACH

  FINISH REPORT rptArhCns

  DISPLAY " < CTRL-P > Imprimir                       ",
          "              < CTRL-C > Salir " AT 1,3 ATTRIBUTE(REVERSE)

  DISPLAY " TOTAL DE REGISTROS : ", tot_arhConsu USING "###,###" 
          AT 20,25 ATTRIBUTE(REVERSE)


  CALL SET_COUNT(z-1)

  DISPLAY ARRAY garr TO consGA.* 
  END DISPLAY

  #ON KEY ( CONTROL-P )
    # LET gimpresion = "lp ", g_imprime
    # RUN gimpresion

END FUNCTION

REPORT rptArhCns(z,tpSol,nfol,nss,frecep,fenvio,st_int,cod_op,diag_proc,nomb)
  DEFINE z         INTEGER,
         tpSol     SMALLINT,
         nfol      DECIMAL(8,0),
         nss       CHAR(11),
         frecep    DATE,
         fenvio    DATE,
         st_int    SMALLINT,
         cod_op    CHAR(2),
         diag_proc CHAR(15),
         nomb      CHAR(66)
  OUTPUT
    LEFT MARGIN 0
    RIGHT MARGIN 0
    TOP MARGIN 0
    BOTTOM MARGIN 0
    PAGE LENGTH 66

  FORMAT

    FIRST PAGE HEADER
      PRINT COLUMN 01, "LISTADO DE REGISTROS EN ARC.CONS.PROC."
      PRINT COLUMN 01, "CONSECUTIVO|",
                       "TIPO SOL|",
                       "FOLIO SOL|",
                       "NSS|",
                       "FEC RECEP|",
                       "FEC ENVIO|",
                       "STATUS INT|",
                       "COD OP|",
                       "DIAG PROC|",
                       "NOMBRE"
    ON EVERY ROW
       PRINT COLUMN 01, z,                           "|",
                        tpSol,                       "|",
                        nfol,                        "|",
                        nss,                         "|",
                        frecep USING "dd/mm/yyyy",   "|",
                        fenvio USING "dd/mm/yyyy",   "|",
                        st_int,                      "|",
                        cod_op,                      "|",
                        diag_proc,                   "|",
                        nomb,                        "|"
       
    ON LAST ROW
       SKIP 2 LINE
       PRINT COLUMN 01, "==================================================",
             COLUMN 51, "==============================="
       PRINT COLUMN 05, "T  O  T  A  L  E  S  : ",
             COLUMN 37,  tot_arhConsu  USING "########&"
      

    {PAGE HEADER
            PRINT COLUMN 05, "Prog. : AFIL038"
            PRINT COLUMN 63, "Fecha : ",TODAY USING "dd/mm/yyyy"
            PRINT COLUMN 28, "LISTADO DE REGISTROS EN ARC.CONS.PROC."
       PRINT COLUMN 01, "==================================================",
             COLUMN 51, "==============================="
            PRINT COLUMN 03, "TPO_SOL",
                  COLUMN 13, "FOLIO ",
                  COLUMN 25, "   NSS     ",
                  COLUMN 38, "FEC_RECEP  ",
                  COLUMN 50, "       NOMBRE"

       PRINT COLUMN 01, "==================================================",
             COLUMN 51, "==============================="

  ON EVERY ROW
            PRINT COLUMN 03, tpSol,
                  COLUMN 13, nfol,
                  COLUMN 25, nss,
                  COLUMN 38, frecep USING "dd/mm/yyyy",
                  COLUMN 50, nomb
  ON LAST ROW
       SKIP 2 LINE
       PRINT COLUMN 01, "==================================================",
             COLUMN 51, "==============================="
       PRINT COLUMN 05, "T  O  T  A  L  E  S  : ",
             COLUMN 37,  tot_arhConsu  USING "########&"}
END REPORT
