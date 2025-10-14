################################################################################
#Proyecto          => SISTEMA DE AFORES.( MEXICO )                             #
#Owner             => E.F.P.                                                   #
#Sistema           => PRO.                                                     #
#Programa PROC001  => ENVIO DE PREPROMOTORES A CONSAR, PARA SER CERTIFICADOS   #
#Fecha             => 9 DE ENERO DE 1997                                       #
#By                => ROBERTO PALAFOX                                          #
#Fecha Modificacion=> 28 de Noviembre del 2006                                 #
#Modificado Por    => Laura Eugenia Cortes Guzman                              #
#Observacion       => Modificaciones de la Circular 5-8                        #
#Modificado Por    => Isabel Fonseca Frias                                     #
#Fecha             => 9 de Febrero del 2008                                    #
#Observacion       => Se agregaron modificaciones de acuerdo a MPT version     #
#                  => 3.0   (v1)                                               #
#Modificado Por    => Isabel Fonseca Frias                                     #
#Fecha Modificacion=> 20 de Agosto del 2008                                    #
#Observacion       => Se cambio el elemento de busquedad de cod_promtor a      #
#                  => codven, esto porque cuando el numero de promotor         #
#                  => se cambia en la reactivacion (en soliciitud) ya no       #
#                  => no es igual al que esta guardado en el maetro de         #
#                  => promotores                                               #
#                  => (v2)                                                     #
#Modificado Por    => Isabel Fonseca Frias                                     #
#Fecha             => 17-09-2009                                               #
#Observacion       => Se actualiza de acuerdo a layout del 27/07/09            #
#                  => (v10)                                                    #
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE #glo #reg_2
        reg_2                 RECORD LIKE pro_solicitud.*    ,
        rd                    RECORD LIKE tab_delegacion.*    ,
        rf                    RECORD LIKE tab_ciudad.*     ,
        g_afore               RECORD LIKE tab_afore_local.*    ,
        g_glo_parametro       RECORD LIKE seg_modulo.*

    DEFINE reg_3 RECORD #glo #reg_3
        tip_registr           CHAR(003) ,
        calle_numer           CHAR(040) ,
        deleg                 CHAR(030) ,
        ciudad                CHAR(030) ,
        estado                CHAR(002) ,
        cod_postal            CHAR(005) ,
        telefono              CHAR(100) ,
        res_examen            CHAR(002) ,
        num_age_pro           CHAR(010) ,
        fec_nacimie           CHAR(008)
    END RECORD

    DEFINE #glo #date
         fecha               ,
         HOY                 DATE

    DEFINE #glo #char
         borra_lineas        CHAR(200) ,
         ch                  CHAR(220) ,
         RUTA                CHAR(200) ,
         G_LISTA_1           CHAR(200) ,
         G_LISTA_2           CHAR(200) ,
         enter               CHAR(001) ,
         gl_si_no            CHAR(001) ,
         g_usuario           CHAR(008) ,
         HORA                CHAR(008) ,
         vestad_desc         CHAR(040) ,
         vtipo_tramite       CHAR(3)   ,
         v_sexo              CHAR(1)   ,                  --(v10)
         v_edo_naci          CHAR(2)                      --(v10)
END GLOBALS

    DEFINE #glo #integer
         nro_rechazado       ,
         nro_reactiva        ,
         nro_altas           ,                            --(v1)
         nro_reg             INTEGER                      --(v1)

MAIN
    CALL init() #i

    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("PROC001.log")

    OPEN WINDOW proc0011 AT 4,4 WITH FORM "PROC0011" ATTRIBUTE(BORDER)
    DISPLAY "                           < Ctrl-C > Salir",
            "                                    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " PROC001  GENERA ARCHIVO CON PROMOTORES PARA SER ",
            "CERTIFICADOS                  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    WHILE TRUE
        PROMPT " ESTA SEGURO S/N " FOR CHAR enter
        IF enter MATCHES "[SsNn]" THEN
            IF enter MATCHES "[Ss]" THEN
                LET nro_altas    = 0
                LET nro_reactiva = 0                            --(v1)
                LET nro_reg      = 0                            --(v1)


                SELECT COUNT(*)                                 --(v1)
                INTO   nro_altas                                --(v1)
                FROM   pro_solicitud A                          --(v1)
                WHERE  A.status_interno IN(0,20)                --(v1)
                AND    A.resuelva       >= 000                  --(v10)
                AND    A.resuelva       <= 100                  --(v10)


                SELECT COUNT(*)                                 --(v1)
                INTO   nro_reactiva                             --(v1)
                FROM   pro_solicitud A, pro_mae_promotor B      --(v1)
                WHERE  A.status_interno IN(7)                   --(v1)
                AND    A.resuelva       >= 000                  --(v10)
                AND    A.resuelva       <= 100                  --(v10)
                AND    B.codven = A.codven                      --(v2)
--                AND    B.diag_proceso not in ("7E", "7T")     --(v10)
                AND    B.diag_proceso not in ("7X")             --(v10)

                LET nro_reg = nro_altas + nro_reactiva          --(v1)

          IF nro_reg = 0 THEN                                   --(v1)
              PROMPT " NO HAY CANDIDATOS A PROMOTOR PARA ENVIAR...",
                           "<ENTER> PARA SALIR " FOR CHAR enter
              EXIT PROGRAM
          END IF
                EXIT WHILE
            ELSE
                PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
                EXIT PROGRAM
            END IF
        END IF
    END WHILE

    DISPLAY " PROCESANDO INFORMACION " AT 19,2 ATTRIBUTE(REVERSE)
    CALL primer_paso() #pp

    DISPLAY "                           < Ctrl-C > Salir",
            "                                    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " PROC001  GENERA ARCHIVO CON PROMOTORES PARA SER ",
            "CERTIFICADOS                  " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    DISPLAY " NRO DE SOLICITUDES PARA ALTAS          : ",nro_altas     AT 08,15
    DISPLAY " NRO DE SOLICITUDES PARA REACTIVACIONES : ",nro_reactiva  AT 10,15
    DISPLAY " NRO DE SOLICITUDES PARA REENVIOS       : ",nro_rechazado AT 12,15

    PROMPT " PROCESO FINALIZADO ...<ENTER> PARA SALIR " FOR CHAR enter

    CLOSE WINDOW proc0011

END MAIN

FUNCTION primer_paso()
#pp-------------------
    DECLARE cur_1 CURSOR FOR
       SELECT A.*                                                  --(v1)
       FROM   pro_solicitud A                                      --(v1
       WHERE  A.status_interno IN(0,20) -- 0 capturado,            --(v1)
--       AND    A.resuelva       >= 080  --20 capturado-rechazado  --(v10)
       AND    A.resuelva       >= 000                              --(v10)
       AND    A.resuelva       <= 100                              --(v10)

       UNION                                                       --(v1)
       SELECT A.*                                                  --(v1)
       FROM   pro_solicitud A, pro_mae_promotor B                  --(v1)
       WHERE  A.status_interno IN(7)     -- 7 capturado-reactivado --(v1)
       AND    A.resuelva       >= 000                              --(v10)
       AND    A.resuelva       <= 100                              --(v10)
       AND    B.codven = A.codven                                  --(v2)
--       AND    B.diag_proceso not in ("7E", "7T")                   --(v1)
       AND    B.diag_proceso not in ("7X")                         --(v10)


       LET G_LISTA_1     = RUTA CLIPPED,"/alt"
       LET G_LISTA_2     = RUTA CLIPPED,"/ALT"
       LET nro_altas     = 0
       LET nro_reactiva  = 0
       LET nro_rechazado = 0

       START REPORT listado_1 TO G_LISTA_1
    FOREACH cur_1 INTO reg_2.*
#ff
      LET reg_3.calle_numer = reg_2.calle  CLIPPED ," ",
            reg_2.numero CLIPPED ," ",
            reg_2.dpto

      SELECT A.* INTO rd.*  FROM tab_delegacion A
      WHERE  A.deleg_cod = reg_2.deleg

      IF STATUS = NOTFOUND THEN
          PROMPT " NO EXISTE DELEGACION ",reg_2.deleg USING"&&&&&&&&",
                 ". NUMERO DE NOMINA ",reg_2.codven
                FOR CHAR enter
          EXIT PROGRAM
      ELSE
               LET reg_3.deleg = rd.deleg_desc
      END IF

      SELECT * INTO rf.* FROM tab_ciudad
      WHERE  ciudad_cod = reg_2.ciudad

      IF STATUS = NOTFOUND THEN
          PROMPT " NO EXISTE CIUDAD ",reg_2.ciudad USING"########"
                FOR CHAR enter

          EXIT PROGRAM
      ELSE
          LET reg_3.ciudad = rf.ciudad_desc
      END IF

      LET reg_3.fec_nacimie = YEAR(reg_2.fnaci)  USING "&&&&",
                              MONTH(reg_2.fnaci) USING "&&"  ,
                              DAY(reg_2.fnaci)   USING "&&"

      IF  reg_3.ciudad IS NULL OR
          reg_3.ciudad = " "  THEN

          SELECT estad_desc INTO vestad_desc FROM tab_estado
          WHERE  estad_cod = reg_2.estado

          LET reg_3.ciudad = "LOCALIDAD DE ",vestad_desc
      END IF

      IF reg_2.cod_promotor IS NULL THEN
          LET vtipo_tramite = "000"
      ELSE
          LET vtipo_tramite = "999"
      END IF

      IF reg_2.materno = " " OR reg_2.materno IS NULL OR  --(v10)
         reg_2.materno = "" THEN                          --(v10)
         LET reg_2.materno = "N/A"                        --(v10)
      END IF                                              --(v10)


          OUTPUT TO REPORT
          listado_1 ("301"               ,
                     reg_2.nombres       ,
                     reg_2.paterno       ,
                     reg_2.materno       ,
                     reg_2.rfc           ,
                     reg_2.seguro        ,
                     reg_2.unico         ,
                     reg_3.calle_numer   ,
                     reg_2.colonia       ,
                     reg_3.deleg         ,
                     reg_3.ciudad        ,
                     reg_2.estado        ,
                     reg_2.codpos        ,
                     reg_2.fono          ,
                     reg_2.resuelva      ,
                     reg_2.cod_promotor  ,
                     reg_3.fec_nacimie   ,
                     reg_2.fecha_baja    ,
                     vtipo_tramite       ,
                     reg_2.escolar
                     ) #l1

            CASE reg_2.status_interno
                WHEN 0
                    LET nro_altas = nro_altas + 1

                   UPDATE pro_solicitud
                   SET    pro_solicitud.status_interno = 1            ,
                          pro_solicitud.status         = 1            ,
                          pro_solicitud.fenvio         = HOY
                   WHERE  pro_solicitud.status_interno = 0
                   AND    pro_solicitud.rfc            = reg_2.rfc

                    INSERT INTO pro_envio_alta
                           VALUES(reg_2.nro_solicitud,
                                  reg_2.codven,reg_2.cod_promotor,HOY,1)

                WHEN 7
                    LET nro_reactiva = nro_reactiva + 1

                   UPDATE pro_solicitud
                   SET    pro_solicitud.status_interno = 8            ,
                          pro_solicitud.status         = 1            ,
                          pro_solicitud.fenvio         = HOY
                   WHERE  pro_solicitud.status_interno = 7
                   AND    pro_solicitud.rfc            = reg_2.rfc

                    INSERT INTO pro_envio_alta
                           VALUES(reg_2.nro_solicitud,
                                  reg_2.codven,reg_2.cod_promotor,HOY,8)

                WHEN 20
                    LET nro_rechazado = nro_rechazado + 1

                   UPDATE pro_solicitud
                   SET    pro_solicitud.status_interno = 21 ,
                          pro_solicitud.status         = 1  ,
                          pro_solicitud.fenvio         = HOY
                   WHERE  pro_solicitud.status_interno = 20
                   AND    pro_solicitud.rfc            = reg_2.rfc

                    INSERT INTO pro_envio_alta
                           VALUES(reg_2.nro_solicitud,
                                  reg_2.codven,reg_2.cod_promotor,HOY,21)
            END CASE
    END FOREACH
    FINISH REPORT listado_1

    LET ch = "chmod 777 ",G_LISTA_1 CLIPPED
    RUN ch

    LET borra_lineas = "sed -e '/^$/d' ",G_LISTA_1 CLIPPED," > ",
                                         G_LISTA_2 CLIPPED
    RUN borra_lineas

    IF nro_altas > 0 THEN
        INSERT INTO pro_ctr_envio VALUES (HOY,"ALT",2,"",nro_altas,"")
    END IF

    IF nro_reactiva > 0 THEN
        INSERT INTO pro_ctr_envio VALUES (HOY,"REA",2,"",nro_reactiva,"")
    END IF

    IF nro_rechazado > 0 THEN
        INSERT INTO pro_ctr_envio VALUES (HOY,"REC",2,"",nro_rechazado,"")
    END IF
END FUNCTION

REPORT listado_1(r_301)
#l1--------------------
    DEFINE r_301 RECORD #loc #r_301
             tip_registr    CHAR(003) ,
             nombres        CHAR(040) ,
             paterno        CHAR(040) ,
             materno        CHAR(040) ,
             rfc            CHAR(013) ,
             nss            CHAR(011) ,
             curp           CHAR(018) ,
             calle_numer    CHAR(040) ,
             colonia        CHAR(030) ,
             deleg_munic    CHAR(030) ,
             ciudad         CHAR(030) ,
             estado         CHAR(002) ,
             codpos         CHAR(005) ,
             telefono       CHAR(100) ,
             res_examen     SMALLINT  ,
             cod_promotor   CHAR(010) ,
             fec_nacimie    CHAR(008) ,
             fecha_baja     DATE      ,
             tipo_tramite   CHAR(3)   ,
             escolar        CHAR(1)
    END RECORD

    DEFINE r_000  RECORD #loc #r_000
     tip_registr     CHAR(3) ,
     num_registr     CHAR(5) ,
     tam_registr     CHAR(3) ,
     tip_archivo     CHAR(3) ,
     cve_afore     CHAR(3) ,
     fec_transm      CHAR(8) ,
     lot_afore      CHAR(9)
    END RECORD

    DEFINE #loc #tab_afore_local
           clave_afore     LIKE tab_afore_local.codigo_afore

    OUTPUT
       LEFT MARGIN   0
       RIGHT MARGIN  0
       TOP MARGIN    0
       BOTTOM MARGIN 0
       PAGE LENGTH   2

    FORMAT
    ON EVERY ROW
        LET r_301.fec_nacimie = YEAR(reg_2.fnaci)  USING "&&&&",
                                MONTH(reg_2.fnaci) USING "&&" ,
                                DAY(reg_2.fnaci)   USING "&&"

--(v10) Se modifico de acuerdo al MPT del 19-07-2009

        -- Obtienen sexo y edo_naci, con CURP

        LET v_sexo     = NULL
        LET v_edo_naci = NULL

        CALL curp(r_301.curp) RETURNING v_sexo,
                                        v_edo_naci

        PRINT 
            COLUMN 001,r_301.tip_registr                           ,
                 COLUMN 004,r_301.nombres                          ,
                 COLUMN 044,r_301.paterno                          ,
                 COLUMN 084,r_301.materno                          ,
                 COLUMN 124,r_301.rfc                              ,
                 COLUMN 137,r_301.nss                              ,
                 COLUMN 148,r_301.curp                             ,
                 COLUMN 166,r_301.calle_numer                      ,
                 COLUMN 206,r_301.colonia                          ,
                 COLUMN 236,r_301.deleg_munic                      ,
                 COLUMN 266,r_301.ciudad                           ,
                 COLUMN 296,r_301.estado        USING "&&"         ,
                 COLUMN 298,r_301.codpos        USING "&&&&&"      ,
                 COLUMN 303,r_301.telefono                         ,
                 COLUMN 403,r_301.res_examen    USING "&&&"        ,
                 COLUMN 406,r_301.cod_promotor  USING "&&&&&&&&&&" ,
                 COLUMN 416,r_301.fec_nacimie                      ,
                 COLUMN 424,8 SPACES                               ,
                 COLUMN 432,r_301.tipo_tramite                     ,
                 COLUMN 435,r_301.escolar                          ,
                 COLUMN 436,v_sexo              USING "&"          , --(v10)
                 COLUMN 437,v_edo_naci          USING "&&"         , --(v10)
                 COLUMN 439,12 SPACES

END REPORT

FUNCTION init()
#i-------------
    LET HORA     = TIME
    LET HOY      = TODAY

    SELECT *         ,
           USER
    INTO   g_afore.* ,
           g_usuario
    FROM   tab_afore_local

    SELECT *
    INTO   g_glo_parametro.*
    FROM   seg_modulo
    WHERE  modulo_cod = "pro"

    LET RUTA = g_glo_parametro.ruta_envio CLIPPED
END FUNCTION

FUNCTION curp(x_curp)                     --(v10)
#cr----------------
    DEFINE
           x_curp                CHAR(18)


        LET v_sexo     = " "
        LET v_edo_naci = "  "

        -- Obtiene la clave de sexo
        LET v_sexo     = x_curp[11,11]
        CASE v_sexo
             WHEN "M"
                LET v_sexo = 2 
             WHEN "H"
                LET v_sexo = 1 
        END CASE

        -- Obtiene la clave de la entidad de nacimiento
        LET v_edo_naci = x_curp[12,13]


        SELECT cve_ent
        INTO   v_edo_naci
        FROM   pro_ent_naci
        WHERE  cod_ent = v_edo_naci

   RETURN v_sexo,
          v_edo_naci

END FUNCTION

