############################################################################
#Proyecto          => Sistema de Afores. (MEXICO)                          #
#Propietario       => E.F.P                                                #
#Programa AFIL050  => PROCESO DE GENERACION ARCHIVO CONCILIACION - DIGIPRO #
#Sistema           => AFI.                                                 #
#Autor             => FERNANDO HERRERA HERNANDEZ                           #
#Fecha             => 12 DE SEPTIEMBRE DE 2006                             #
#Version           => Solo para METLIFE                                    #
############################################################################

DATABASE safre_af  

GLOBALS

  DEFINE reg_temp    RECORD 
    TipoSol          CHAR(1),
    Consecutivo      INTEGER,
    NoModulo         LIKE solicitudafi.idlote,
    FolioSol         DECIMAL(8,0),
    NSS              CHAR(11),
    NoPromotor       CHAR(10),
    NoReferencia     CHAR(2),
    idsolicitud      LIKE solicitudafi.idsolicitud
  END RECORD

  DEFINE g_afore     RECORD LIKE tab_afore_local.*
  DEFINE g_parametro RECORD LIKE seg_modulo.*

  DEFINE
    enter            CHAR(1),
    usuario          CHAR(8),
    HORA             CHAR(8),
    G_LISTA          CHAR(200),
    HOY              DATE,
    #generar          SMALLINT,
    generar          CHAR,
    total2           SMALLINT,
    fecha_ini        DATE,
    fecha_fin        DATE,
    nom_arch         CHAR(200),
    vtipsol          CHAR(3)

  --SV3
  DEFINE
    tot_solv3        INTEGER

END GLOBALS

MAIN

  DEFER INTERRUPT
    OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP

  CALL STARTLOG("AFIM050.log")
  CALL inicio()
  CALL proceso_principal()

END MAIN

FUNCTION inicio()
#i---------------

  LET HOY         = TODAY
  LET HORA        = TIME
  LET fecha_ini   = TODAY
  LET fecha_fin   = TODAY

  LET total2      = 0
  LET tot_solv3   = 0

  SELECT *, USER
  INTO   g_afore.*, usuario
  FROM   tab_afore_local

  SELECT *
  INTO   g_parametro.*
  FROM   seg_modulo
  WHERE  modulo_cod = 'afi'

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

  OPEN WINDOW ventana_1 AT 4,4 WITH FORM "AFIL0501" ATTRIBUTE(BORDER)
  DISPLAY " AFIL050     ARCHIVO CONCILIACION SOLICITUDES AFO-DIGIPRO                      " AT 3,1 ATTRIBUTE (REVERSE)
  DISPLAY "                           < Ctrl-C > Salir                                    " AT 1,2 ATTRIBUTE (REVERSE)
  DISPLAY HOY USING"dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)

  INPUT BY NAME generar, fecha_ini, fecha_fin WITHOUT DEFAULTS
    AFTER FIELD generar
      IF generar = 1 OR
         generar = 2 OR
         generar = 8 THEN
         CASE generar
           WHEN 1 LET vtipsol = 'REG'
           WHEN 2 LET vtipsol = 'TAA'
           WHEN 8 LET vtipsol = 'NOA'
         END CASE
         NEXT FIELD fecha_ini
      ELSE
         ERROR "Opcion solo puede ser 1)Reg. Inicial o 2)Traspaso",
               " u 8) Independiente"
         NEXT FIELD generar
      END IF

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

           SELECT COUNT(*)
           INTO   tot_solv3

                  {cs.descorta[1]               TipoSol,
                  "0"                           Consecutivo,
                  s.idlote                      NoModulo,
                  --cs.descorta                 NoModulo,
                  s.nsolicitud                  FolioSol,
                  p.datosacceso[1,11]           NSS,
                  da.valor                      NoPromotor,
                  ",,"                          NoReferencia
                  --""                          NSS_ISSSTE,
                  --""                          CURP,
                  --ce.cvenatural}

           FROM   solicitudafi s,
                  persona p,
                  catalogosistema cs, 
                  cestructuracom ce, 
                  datosacceso da, 
                  glote gl,
                  lotesolicitud ls
           WHERE  s.idpersona        = p.idpersona
           AND    s.idtposolicitud   = cs.idcatalogo
           AND    s.idcestructuracom = ce.idcestructuracom
           AND    s.idpromotor       = da.idpersona
           AND    da.idtpoidnalterno = 6
           AND    s.idsolicitud      = ls.idsolicitud
           AND    ls.idlote          = gl.idlote
           AND    gl.idnlote        IN (SELECT idnlote
                                        FROM   glote
                                        WHERE  idlote IN (SELECT idlote
                                                          FROM   solicitudafi))
           AND    gl.identreceptor  IN (SELECT idcatalogo 
                                        FROM   catalogogeneral
                                        WHERE  cvenatural = 'MEC')
           AND    gl.fvalida        >= fecha_ini
           AND    gl.fvalida        <= fecha_fin
           AND    cs.cvenatural      = generar;

          WHILE TRUE
            PROMPT "Total de solicitudes: ", tot_solv3, " es correcto? [S/N]: "
            FOR enter
            IF enter MATCHES "[Ss/Nn]" THEN
               IF enter MATCHES "[Ss]" THEN
                  EXIT WHILE
               ELSE
                  ERROR "Proceso cancelado. Informe a su area de sistemas ",
                  "por favor. " SLEEP 4
                  EXIT PROGRAM
               END IF
            ELSE
               ERROR "Solo debe presionar (S) Si o (N) No"
               SLEEP 3
               ERROR ""
            END IF
          END WHILE

          CALL actualiza_solicitud1() #td
          EXIT INPUT
        END IF

    ON KEY ( INTERRUPT )
       EXIT PROGRAM

  END INPUT

END FUNCTION

FUNCTION actualiza_solicitud1()

  LET G_LISTA = g_parametro.ruta_listados CLIPPED,"/",usuario CLIPPED,
                ".SOL_CONC_", vtipsol, "." CLIPPED,
                HOY USING "ddmmyy",".",HORA[1,2] CLIPPED, HORA[4,5] CLIPPED,
                HORA[7,8]
  LET nom_arch= G_LISTA CLIPPED

  START REPORT listado_2 TO G_LISTA

  DECLARE c_prin CURSOR FOR
  SELECT cs.descorta[1]                TipoSol     ,
         "0"                           Consecutivo ,
         #s.idlote                      NoModulo    ,
         ce.cvenatural                 NoModulo    ,
         s.nsolicitud                  FolioSol    ,
         p.datosacceso[1,11]           NSS         ,
         da.valor                      NoPromotor  ,
         ",,"                          NoReferencia,
         s.idsolicitud                 Idsolicitud
  FROM   solicitudafi s,
         persona p,
         catalogosistema cs, 
         cestructuracom ce, 
         datosacceso da, 
         glote gl,
         lotesolicitud ls
  WHERE  s.idpersona        = p.idpersona
  AND    s.idtposolicitud   = cs.idcatalogo
  AND    s.idcestructuracom = ce.idcestructuracom
  AND    s.idpromotor       = da.idpersona
  AND    da.idtpoidnalterno = 6
  AND    s.idsolicitud      = ls.idsolicitud
  AND    ls.idlote          = gl.idlote
  AND    gl.idnlote        IN (SELECT idnlote
                               FROM   glote
                               WHERE  idlote IN (SELECT idlote
                                                 FROM   solicitudafi))
  AND    gl.identreceptor  IN (SELECT idcatalogo 
                               FROM   catalogogeneral
                               WHERE  cvenatural = 'MEC')
  AND    gl.fvalida        >= fecha_ini
  AND    gl.fvalida        <= fecha_fin
  AND    cs.cvenatural      = generar;

  FOREACH c_prin INTO reg_temp.*

    LET total2               = total2 + 1
    LET reg_temp.Consecutivo = total2

    OUTPUT TO REPORT listado_2(reg_temp.*)

  END FOREACH

  FINISH REPORT listado_2

  LET G_LISTA = "chmod 777 ",g_parametro.ruta_listados CLIPPED,"/",
                usuario CLIPPED,
                ".SOL_CONC_", vtipsol, "." CLIPPED,
                HOY USING "ddmmyy",".",HORA[1,2] CLIPPED, HORA[4,5] CLIPPED,
                HORA[7,8]
  RUN G_LISTA

  DISPLAY "Total de solicitudes                                   : ",
  tot_solv3 AT 13,9

  DISPLAY "Total de solicitudes para envio a DIGIPRO              : ",
  total2 AT 15,9

  DISPLAY "Nombre archivo: ", nom_arch AT 16,1


  PROMPT "Proceso finalizado, [Enter] para salir" FOR enter

END FUNCTION

REPORT listado_2(reg_temp)
#l2-----------------------

  DEFINE reg_temp    RECORD
    TipoSol          CHAR(1),
    Consecutivo      INTEGER,
    NoModulo         LIKE solicitudafi.idlote,
    FolioSol         DECIMAL(8,0),
    NSS              CHAR(11),
    NoPromotor       CHAR(10),
    NoReferencia     CHAR(2),
    idsolicitud      LIKE solicitudafi.idsolicitud
  END RECORD

  DEFINE
    v_nss            CHAR(11),
    v_curp           CHAR(18),
    v_rfc            CHAR(13)

  DEFINE
    cont     INTEGER,
    l_estado CHAR(11)

  OUTPUT
    LEFT   MARGIN 0
    RIGHT  MARGIN 0
    TOP    MARGIN 0
    BOTTOM MARGIN 0
    PAGE   LENGTH 66

  FORMAT
    FIRST PAGE HEADER
       PRINT "FECHA ENTREGA,", HOY USING "YYYY-MM-DD", ",ENTREGADAS,",
       tot_solv3 CLIPPED

    ON EVERY ROW
       IF reg_temp.Tiposol = "N" THEN 

          LET reg_temp.Tiposol = "S" 

          SELECT dat1.Valor,
                 dat2.Valor,
                 dat3.Valor
          INTO   v_nss,
                 v_curp,
                 v_rfc
          FROM   SolicitudAfi sol,
          OUTER  DatosAcceso dat1,
          OUTER  DatosAcceso dat2,
          OUTER  DatosAcceso dat3,
          OUTER  persona pers,
                 CatalogoSistema catSis
          WHERE  sol.nsolicitud = reg_temp.idsolicitud
          AND    sol.idpersona  = dat1.idpersona
          AND    sol.idpersona  = dat2.idpersona
          AND    sol.idpersona  = dat3.idpersona
          AND    sol.idpersona  = pers.idpersona
          AND    sol.idTpoSolicitud = catSis.idCatalogo
          AND    (dat1.idTpoIdnAlterno IS NULL OR
                  dat1.idTpoIdnAlterno IN  (SELECT idTpoIdnAlterno
                                            FROM   tpoidnalterno
                                            WHERE  CveTpoIdnAlterno = 'NSS'))
          AND    (dat2.idTpoIdnAlterno IS NULL OR
                  dat2.idTpoIdnAlterno IN  (SELECT idTpoIdnAlterno
                                            FROM   tpoidnalterno
                                            WHERE  CveTpoIdnAlterno = 'CURP'))
          AND    (dat3.idTpoIdnAlterno IS NULL OR
                  dat3.idTpoIdnAlterno IN  (SELECT idTpoIdnAlterno
                                            FROM   tpoidnalterno
                                            WHERE  CveTpoIdnAlterno = 'RFC'))

          LET reg_temp.NSS = NULL

          PRINT reg_temp.TipoSol,                       ',',
                reg_temp.Consecutivo  USING "&&&&&&",   ',',
                reg_temp.NoModulo     USING "&&&&&",    ',',
                reg_temp.FolioSol     USING "&&&&&&&&", ',',
                reg_temp.NSS,                           ',',
                reg_temp.NoPromotor,                    
                reg_temp.NoReferencia                   ,
                                                        ',',
                v_curp,                                 ',',
                HOY USING "MM/DD/YYYY"
                 
       ELSE
          PRINT reg_temp.TipoSol,                       ',',
                reg_temp.Consecutivo  USING "&&&&&&",   ',',
                reg_temp.NoModulo     USING "&&&&&",    ',',
                reg_temp.FolioSol     USING "&&&&&&&&", ',',
                reg_temp.NSS,                           ',',
                reg_temp.NoPromotor,                    
                reg_temp.NoReferencia
       END IF

    ON LAST ROW
       PRINT "F"
END REPORT
