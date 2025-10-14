############################################################################
#Proyecto          => Sistema de Afores. (MEXICO)                          #
#Propietario       => E.F.P                                                #
#Programa AFIM019  => PROCESO DE VALIDACION DE CONTROL DOCUMENTAL          #
#Sistema           => AFI.                                                 #
#Autor             => MAURO MUNIZ CABALLERO                                #
#Fecha             => 8 DE MARZO DE 2002                                   #
#Modifico          => FERNANDO HERRERA HERNANDEZ                           #
#Fecha             => 29 DE AGOSTO DE 2005 (TRABAJADORES INDEPENDIENTES)   #
#Actualizado       => EDUARDO RESENDIZ MEDINA 14 JULIO 2006                #
#                     EN FUNCION actualiza_solicitud1                      #
############################################################################

DATABASE safre_af

GLOBALS

  DEFINE reg_valida  RECORD LIKE afi_valida_docto.*
  DEFINE reg_temp    RECORD LIKE afi_valida_docto.*
  DEFINE g_afore     RECORD LIKE tab_afore_local.*
  DEFINE g_parametro RECORD LIKE seg_modulo.*
  DEFINE g_param_int RECORD LIKE seg_modulo.*

  DEFINE
    enter            CHAR(1),
    usuario          CHAR(8),
    HORA             CHAR(8),
    valida_dg        CHAR(20),
    operacion        CHAR(40),
    G_LISTA          CHAR(200),
    G_LISTA2         CHAR(200),
    G_IMPRE          CHAR(200),
    HOY              DATE,
    generar          SMALLINT,
    total1           SMALLINT,
    total2           SMALLINT,
    total3           SMALLINT,
    total4           INTEGER,
    bnd_dc           SMALLINT,
    bnd_dg           SMALLINT,
    G_GENERA         CHAR(200),
    fecha_ini        DATE,
    fecha_fin        DATE

  --SV3
  DEFINE
    v_idtposolicitud LIKE catalogosistema.idcatalogo,
    tot_solv3        INTEGER,
    tot_reenvio      INTEGER,
    vgenerar         CHAR(2)      --taa x curp
 

END GLOBALS

MAIN

  DEFER INTERRUPT
    OPTIONS 
      PROMPT LINE LAST,
      INPUT WRAP

  CALL STARTLOG("AFIM019.log")
  CALL inicio()
  CALL proceso_principal()
 
END MAIN

FUNCTION inicio()
#i---------------

  LET HOY         = TODAY
  LET HORA        = TIME
  LET operacion   = 'VALIDA CAPTURA/CTR DOCUMENTAL'
  LET fecha_ini   = TODAY
  LET fecha_fin   = TODAY

  LET total1      = 0
  LET total2      = 0
  LET total3      = 0
  LET total4      = 0
  LET bnd_dc      = 0
  LET bnd_dg      = 0
  LET tot_solv3   = 0
  LET tot_reenvio = 0

  SELECT *, USER
  INTO   g_afore.*, usuario
  FROM   tab_afore_local

  SELECT *
  INTO   g_parametro.*
  FROM   seg_modulo
  WHERE  modulo_cod = 'afi'

  SELECT *
  INTO   g_param_int.*
  FROM   seg_modulo
  WHERE  modulo_cod = 'int'


END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

  OPEN WINDOW ventana_1 AT 4,4 WITH FORM "AFIM0191" ATTRIBUTE(BORDER)
  DISPLAY " AFIM019          CONFIRMACION SOLICITUDES ENVIO A CERTIFICAR                  " AT 3,1 ATTRIBUTE(REVERSE)

  DISPLAY "                                < Ctrl-C > Salir                               " AT 1,2 ATTRIBUTE(REVERSE)
  DISPLAY HOY USING"dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)

  INPUT BY NAME generar, fecha_ini, fecha_fin WITHOUT DEFAULTS
    AFTER FIELD generar

        IF generar = 1 OR
           generar = 8 THEN
         DISPLAY "                                                                        " AT 13,1 
         LET vgenerar = generar
          SELECT idcatalogo
            INTO v_idtposolicitud
            FROM catalogosistema
           WHERE cvenatural = vgenerar 
             AND idpadre IN ( SELECT idcatalogo
                                FROM catalogosistema
                               WHERE cvenatural="TPOSOLICITUD"
                                 AND idpadre IS NULL)

           SELECT COUNT(*)
           INTO   tot_solv3
           FROM   solicitudafi sa
           WHERE  sa.stamcertificafi    = "15-Por Confirmar"
           AND   (sa.stamdocumentafi    = "Documentos Recibidos MC"
           OR     sa.stamdocumentafi    = "Documentos Validados MC")
           AND    sa.stamprocessafi     = "Revisada Documentos"
           AND    sa.stamdigimageafi    = "Completa Img"
           AND    sa.stamresulvallogica = "Aceptada Val Doc"
           AND    sa.stamresulclasifdoc = "Aceptada Clasif Doc"
           AND    sa.stamfile           = "Por Enviar Arch Fisico"
           AND    sa.idtposolicitud     = v_idtposolicitud

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

      IF generar = 2 OR
         generar = 15 THEN
         LET vgenerar = generar
         NEXT FIELD fecha_ini
      ELSE
         ERROR "Opcion solo puede ser 1)Reg. Inicial o 2)Traspaso",
               " 8) Independiente o 15) TAA X CURP"
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

          SELECT idcatalogo
            INTO v_idtposolicitud
            FROM catalogosistema
           WHERE cvenatural = vgenerar 
             AND idpadre IN ( SELECT idcatalogo
                                FROM catalogosistema
                               WHERE cvenatural="TPOSOLICITUD"
                                 AND idpadre IS NULL)
  
          {SELECT COUNT(*)
            INTO tot_solv3
            FROM Solicitudafi
           WHERE stamcertificafi = "15-Por Confirmar"
             AND stamdocumentafi = "Documentos Recibidos MC"
             AND (stamprocessafi = "Revisada Documentos")
             #AND stamdigimageafi = "Completa Img"
             AND idtposolicitud  = v_idtposolicitud

          SELECT COUNT(*)
          INTO   tot_solv3
          FROM   glote g, lotesolicitud l, solicitudafi s
          WHERE  g.idlote         = l.idlote
          AND    l.idsolicitud    = s.idsolicitud
          AND    g.fvalida       >= fecha_ini
          AND    g.fvalida       <= fecha_fin
          AND    g.idnlote       IN (SELECT idnlote
                                     FROM   glote
                                     WHERE  idlote IN
                                     (SELECT idlote
                                      FROM   solicitudafi
                                      WHERE  s.stamcertificafi="15-Por Confirmar"
                                      AND    s.stamdocumentafi="Documentos Recibidos MC"
                                      AND    s.stamprocessafi ="Revisada Documentos"                             

                                      AND   stamdigimageafi='Completa Img'
                                      AND    s.idtposolicitud =v_idtposolicitud)
                                      )
          AND    g.identreceptor IN (SELECT idcatalogo
                                     FROM   catalogogeneral
                                     WHERE  cvenatural = 'MEC')
          AND    l.idsolicitud   IN (SELECT idsolicitud
                                     FROM   solicitudafi)}

           SELECT COUNT(*)
           INTO   tot_solv3
           FROM   solicitudafi sa
           WHERE  sa.stamcertificafi    = "15-Por Confirmar"
           AND   (sa.stamdocumentafi    = "Documentos Recibidos MC" 
           OR     sa.stamdocumentafi    = "Documentos Validados MC")
           AND    sa.stamprocessafi     = "Revisada Documentos"
           AND    sa.stamdigimageafi    = "Completa Img"
           AND    sa.stamresulvallogica = "Aceptada Val Doc"
           AND    sa.stamresulclasifdoc = "Aceptada Clasif Doc"
           AND    sa.stamfile           = "Por Enviar Arch Fisico"
           AND    sa.idtposolicitud     = v_idtposolicitud
           AND    sa.frecepcion        >= fecha_ini
           AND    sa.frecepcion        <= fecha_fin
 
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

   IF generar = 1 OR generar = 8 THEN
      LET G_GENERA = 'cd ',g_param_int.ruta_exp CLIPPED, 
                     '; fglgo INTPUENT3B ', generar USING '&'
      RUN G_GENERA RETURNING total4
   ELSE
      IF generar = 2 OR generar = 15 THEN
         LET G_GENERA = 'cd ',g_param_int.ruta_exp CLIPPED, 
                        '; fglgo INTPUENTE3 ', generar USING '#&' CLIPPED, ' ', fecha_ini, ' ',
                        fecha_fin
         RUN G_GENERA RETURNING total4
      END IF
   END IF

   LET total4 = total4 / 256
   LET total2 = 0   ---erm
   #LET total4 = 10

{
        INSERT INTO afi_valida_docto VALUES (reg_valida.*)

        INSERT INTO afi_ctr_logico
        VALUES (reg_valida.n_folio,
                reg_valida.tipo_solicitud,
                reg_valida.n_seguro,
                reg_valida.status_interno,
                usuario,
                HOY,
                HORA,
                operacion)

     ERROR ""

}
     LET G_LISTA = g_parametro.ruta_listados CLIPPED,"/",usuario CLIPPED,
                   ".SOLIC_VALIDA." CLIPPED,
                   HOY USING "dd-mm-yy",".",HORA CLIPPED

     LET G_LISTA2 = g_parametro.ruta_listados CLIPPED,"/",usuario CLIPPED,
                    ".ARCHIVO_VALIDA." CLIPPED,
                    HOY USING "dd-mm-yy",".",HORA CLIPPED

     SELECT COUNT(A.n_folio)
     INTO   tot_reenvio
     FROM   afi_solicitud A, afi_recepcion B
     WHERE  A.status_interno = 20
     AND    A.tipo_solicitud = generar
     AND    A.n_folio        = B.n_folio
     AND    A.tipo_solicitud = B.tipo_solicitud
     AND    A.n_seguro       = B.n_seguro
     AND    A.fecha_envio IS NOT NULL

     DECLARE c_prin CURSOR FOR 
     SELECT A.n_folio,
            A.tipo_solicitud,
            A.n_seguro,
            A.status_interno,
            B.estado_sol,
            B.estado_exp,
            0,
            0,
            0
     FROM   afi_solicitud A, afi_recepcion B
     WHERE  A.status_interno = 20
     AND    A.tipo_solicitud = generar
     AND    A.n_folio        = B.n_folio
     AND    A.tipo_solicitud = B.tipo_solicitud
     AND    A.n_seguro       = B.n_seguro

     START REPORT listado_2 TO G_LISTA   
     START REPORT listado_3 TO G_LISTA2  

     --FOREACH c_prin INTO g_princ.*
     FOREACH c_prin INTO reg_temp.*    ---erm

       OUTPUT TO REPORT listado_2(reg_temp.*)
       OUTPUT TO REPORT listado_3(reg_temp.*)
 
       LET total2 = total2 + 1
 
     END FOREACH

     FINISH REPORT listado_3
     FINISH REPORT listado_2

     LET G_LISTA = "chmod 777 ",g_parametro.ruta_listados CLIPPED,"/",
                   usuario CLIPPED,
                   ".SOLIC_VALIDA." CLIPPED,
                   HOY USING "dd-mm-yy",".",HORA CLIPPED
     RUN G_LISTA

     LET G_LISTA2 = "chmod 777 ",g_parametro.ruta_listados CLIPPED,"/",
                    usuario CLIPPED,
                    ".ARCHIVO_VALIDA." CLIPPED,
                    HOY USING "dd-mm-yy",".",HORA CLIPPED
     RUN G_LISTA2

     LET G_IMPRE  = "lp ",g_parametro.ruta_listados CLIPPED,"/",
                    usuario CLIPPED,
                   ".SOLIC_VALIDA." CLIPPED,
                   HOY USING "dd-mm-yy",".",HORA CLIPPED
     RUN G_IMPRE

  --END IF

     DISPLAY "Total de solicitudes validadas                         : ", 
     tot_solv3 AT 13,9
     DISPLAY "Total de solicitudes para reenvio                      : ", 
     tot_reenvio AT 14,9
     DISPLAY "Total de solicitudes validadas para envio a certificar : ", 
     total2 AT 15,9

     PROMPT "Proceso finalizado, [Enter] para salir" FOR enter

END FUNCTION

FUNCTION actualiza_solicitud()
#td---------------------------

  DEFINE pasa_imagen CHAR(100)

  WHENEVER ERROR CONTINUE
     DATABASE safre_tmp
       DROP TABLE tmp_valida_solic

       CREATE TEMP TABLE safre_tmp:tmp_valida_solic
         (n_folio          DECIMAL(10,0),
          tipo_solicitud  SMALLINT,
          n_seguro        CHAR(11),
          status_interno  SMALLINT,
          estado_sol      SMALLINT,
          estado_exp      SMALLINT,
          estado_dig      SMALLINT,
          usuario         CHAR(8) ,
          factualiza      DATE)
     DATABASE safre_af
  WHENEVER ERROR STOP

  SELECT COUNT(*)
  INTO   total1
  FROM   afi_solicitud A, afi_recepcion B
  WHERE  A.status_interno IN(15,17)  ---= 15
  AND    A.tipo_solicitud = generar
  AND    A.n_folio        = B.n_folio
  AND    A.tipo_solicitud = B.tipo_solicitud
  AND    A.n_seguro       = B.n_seguro

  IF total1 = 0 OR
     total1 IS NULL THEN
     ERROR "No hay solicitudes para validar vs control documental"
     SLEEP 4
  ELSE
     ERROR "Procesando Informacion... Espere un momento"

     DECLARE cursor_1 CURSOR FOR
     SELECT A.n_folio,
            A.tipo_solicitud,
            A.n_seguro,
            A.status_interno,
            B.estado_sol,
            B.estado_exp
     FROM   afi_solicitud A, afi_recepcion B
     WHERE  A.status_interno in(15,17) 
     AND    A.tipo_solicitud = generar
     AND    A.n_folio        = B.n_folio
     AND    A.tipo_solicitud = B.tipo_solicitud
     AND    A.n_seguro       = B.n_seguro

     FOREACH cursor_1 INTO reg_valida.n_folio,
                           reg_valida.tipo_solicitud,
                           reg_valida.n_seguro,
                           reg_valida.status_interno,
                           reg_valida.estado_sol,
                           reg_valida.estado_exp

       LET reg_valida.usuario    = usuario
       LET reg_valida.factualiza = HOY

       IF reg_valida.tipo_solicitud = 1 THEN
          LET reg_valida.estado_dig = 0

          IF reg_valida.estado_sol = 0 AND
             reg_valida.estado_exp = 0 THEN
             LET reg_valida.status_interno = 20

             UPDATE afi_solicitud
             SET    afi_solicitud.status_interno = 
                    reg_valida.status_interno
             WHERE  afi_solicitud.n_seguro       = reg_valida.n_seguro
             AND    afi_solicitud.n_folio        = reg_valida.n_folio
             AND    afi_solicitud.tipo_solicitud = 
                    reg_valida.tipo_solicitud

             LET total2 = total2 + 1
          ELSE
             LET total3 = total3 + 1
          END IF
        ELSE
          IF reg_valida.estado_sol = 0 AND
             reg_valida.estado_exp = 0 THEN
             LET bnd_dc = 0
          ELSE
             LET bnd_dc = 1
          END IF

          LET bnd_dg = 0

          IF bnd_dc = 0 AND
             bnd_dg = 0 THEN
             LET reg_valida.status_interno = 20

             UPDATE afi_solicitud
             SET    afi_solicitud.status_interno = reg_valida.status_interno
             WHERE  afi_solicitud.n_seguro       = reg_valida.n_seguro
             AND    afi_solicitud.n_folio        = reg_valida.n_folio
             AND    afi_solicitud.tipo_solicitud = reg_valida.tipo_solicitud

             LET total2 = total2 + 1
          END IF

          IF bnd_dg = 1 THEN
             LET reg_valida.status_interno = 17
             LET reg_valida.estado_dig     = 1

             UPDATE afi_solicitud
             SET    afi_solicitud.status_interno = reg_valida.status_interno
             WHERE  afi_solicitud.n_seguro       = reg_valida.n_seguro
             AND    afi_solicitud.n_folio        = reg_valida.n_folio
             AND    afi_solicitud.tipo_solicitud = reg_valida.tipo_solicitud

             LET total3 = total3 + 1
          END IF

          IF bnd_dc = 1 AND
             bnd_dg = 0 THEN
             LET total3 = total3 + 1
          END IF
        END IF

        INSERT INTO afi_valida_docto VALUES (reg_valida.*)

        INSERT INTO safre_tmp:tmp_valida_solic VALUES (reg_valida.*)

        INSERT INTO afi_ctr_logico
        VALUES (reg_valida.n_folio,
                reg_valida.tipo_solicitud,
                reg_valida.n_seguro,
                reg_valida.status_interno,
                usuario,
                HOY,
                HORA,
                operacion)

     END FOREACH

     ERROR ""

     DISPLAY "Total de solicitudes validadas para envio a certificar : ", 
     total2 AT 14,9

     PROMPT "Proceso finalizado, [Enter] para salir" FOR enter

     LET G_LISTA = g_parametro.ruta_listados CLIPPED,"/",usuario CLIPPED,
                   ".SOLIC_VALIDA." CLIPPED,
                   HOY USING "dd-mm-yy",".",HORA CLIPPED

     LET G_LISTA2 = g_parametro.ruta_listados CLIPPED,"/",usuario CLIPPED,
                    ".ARCHIVO_VALIDA." CLIPPED,
                    HOY USING "dd-mm-yy",".",HORA CLIPPED

     DECLARE cur_1 CURSOR FOR
     SELECT *
     FROM   safre_tmp:tmp_valida_solic
     ORDER BY status_interno, tipo_solicitud, n_folio

     START REPORT listado_2 TO G_LISTA
     START REPORT listado_3 TO G_LISTA2
     FOREACH cur_1 INTO reg_temp.*
       OUTPUT TO REPORT listado_2(reg_temp.*)
       OUTPUT TO REPORT listado_3(reg_temp.*)
     END FOREACH
     FINISH REPORT listado_3
     FINISH REPORT listado_2

     LET G_LISTA = "chmod 777 ",g_parametro.ruta_listados CLIPPED,"/",
 		   usuario CLIPPED,
                   ".SOLIC_VALIDA." CLIPPED,
                   HOY USING "dd-mm-yy",".",HORA CLIPPED
     RUN G_LISTA

     LET G_LISTA2 = "chmod 777 ",g_parametro.ruta_listados CLIPPED,"/",
		    usuario CLIPPED,
                    ".ARCHIVO_VALIDA." CLIPPED,
                    HOY USING "dd-mm-yy",".",HORA CLIPPED
     RUN G_LISTA2

     LET G_IMPRE  = "lp ",g_parametro.ruta_listados CLIPPED,"/",
 		   usuario CLIPPED,
                   ".SOLIC_VALIDA." CLIPPED,
                   HOY USING "dd-mm-yy",".",HORA CLIPPED
     RUN G_IMPRE
                    

  END IF

END FUNCTION

REPORT listado_2(reg_temp)
#l2-----------------------

  DEFINE reg_temp    RECORD LIKE afi_valida_docto.*

  DEFINE
    cont     INTEGER,
    l_estado CHAR(11)

  OUTPUT
    LEFT MARGIN   0
    RIGHT MARGIN  0
    TOP MARGIN    0
    BOTTOM MARGIN 0


  FORMAT
    PAGE HEADER
      PRINT
        COLUMN 03,"========================================",
        COLUMN 40,"==================================================="
      PRINT
        COLUMN 15," VALIDA DOCUMENTOS VS SOLICITUDES DE AFILIACION "
      PRINT
        COLUMN 03,"----------------------------------------",
        COLUMN 40,"---------------------------------------------------"
      PRINT
        COLUMN 33,"USUARIO QUE VALIDA : ", usuario
      PRINT
        COLUMN 03,"----------------------------------------",
        COLUMN 40,"---------------------------------------------------"
      PRINT
        COLUMN 05,"N.S.S  ",
        COLUMN 22,"FOLIO  ",
        COLUMN 34,"T S",
        COLUMN 40,"ST INT ",
        COLUMN 45,"DESC   ",
        COLUMN 62,"ST SOL ",
        COLUMN 69,"ST EXP ",
        COLUMN 77,"ST DIG ",
        COLUMN 85,"F.VALIDA"
      PRINT
        COLUMN 03,"========================================",
        COLUMN 40,"==================================================="

       LET cont = 0

    ON EVERY ROW

       LET cont = cont + 1

       CASE reg_temp.status_interno
         WHEN 17 LET l_estado = "NO DIGITALIZADA"
         WHEN 15 LET l_estado = "CONFIRMADA"
         WHEN 20 LET l_estado = "VALIDADA"
       END CASE

       PRINT
         COLUMN 05,reg_temp.n_seguro,
         COLUMN 20,reg_temp.n_folio,
         COLUMN 30,reg_temp.tipo_solicitud,
         COLUMN 38,reg_temp.status_interno,
         COLUMN 48,l_estado,
         COLUMN 65,reg_temp.estado_sol USING '&',
         COLUMN 72,reg_temp.estado_exp USING '&',
         COLUMN 80,reg_temp.estado_dig USING '&',
         COLUMN 85,HOY USING "DD/MM/YYYY"
 
    ON LAST ROW
       --SELECT COUNT(*)
       --INTO   cont
       --FROM   safre_tmp:tmp_valida_solic

       PRINT
       PRINT
       PRINT
         COLUMN 03,"----------------------------------------",
         COLUMN 40,"-----------------------------------"

       PRINT
       PRINT
         COLUMN 03,"NUMERO TOTAL DE REGISTROS ---> ",cont
END REPORT

REPORT listado_3(reg_temp)
#l3-----------------------

  DEFINE reg_temp  RECORD LIKE afi_valida_docto.*

    OUTPUT
      PAGE   LENGTH 90
      TOP    MARGIN 0
      BOTTOM MARGIN 0
      RIGHT  MARGIN 0
      LEFT   MARGIN 0

    FORMAT
      ON EVERY ROW
         PRINT COLUMN 001, reg_temp.*

END REPORT

FUNCTION carga_imagenes()
#ci----------------------

  DEFINE ejecuta CHAR(100)

  WHENEVER ERROR CONTINUE
    DATABASE safre_tmp
      DROP TABLE tmp_imagenes

      CREATE TABLE tmp_imagenes
      (nombre_arch   CHAR(30))

    DATABASE safre_af
  WHENEVER ERROR STOP

   LET ejecuta = NULL

   LET ejecuta = "cd /archivos/imagenes/receptora/validar; ls *tif > plano " 
		  CLIPPED
   RUN ejecuta

   LET ejecuta = NULL

   LET ejecuta = "chmod 777 /archivos/imagenes/receptora/validar/plano" CLIPPED
   RUN ejecuta

   LET ejecuta = NULL

   LET ejecuta = "/archivos/imagenes/receptora/validar/plano" CLIPPED

   LOAD FROM ejecuta INSERT INTO safre_tmp:tmp_imagenes

END FUNCTION

FUNCTION valida_imagen(validar)
#vi----------------------------

  DEFINE validar CHAR(20)

  SELECT "X"
  FROM   safre_tmp:tmp_imagen_c
  WHERE  nombre_arch MATCHES valida_dg
  GROUP BY 1

  IF SQLCA.SQLCODE = 0 THEN
     RETURN 0
  ELSE
     RETURN 1
  END IF

END FUNCTION

