#************************************************************************#
#Proyecto          => Sistema de Afores( MEXICO )                        #
#Propietario       => EFP                                                #
#Programa          => PROC015                                            #
#Descripcion       => ENVIO DE SOLICITUDES DE REACTIVACION DE PROMOTORES #
#Sistema           => PRO                                                #
#Fecha             => 08-OCTUBRE-2007                                    #
#Por               => STEFANIE DANIELA VERA PIÃA                         #
#Modificado Por    => Isabel Fonseca Frias                               #
#Fecha             => 25-Febrero-2008                                    #
#Observacion       => Se agregaron modificaciones de acuerdo a MPT       #
#                  => version 3.0   (v1)                                 #
#************************************************************************#
DATABASE safre_af
GLOBALS
    DEFINE #glo #reg_2
        reg_2                 RECORD LIKE pro_solicitud.*  ,
        rd                    RECORD LIKE tab_delegacion.* ,
        rf                    RECORD LIKE tab_ciudad.*     ,
        g_seg_modulo          RECORD LIKE seg_modulo.*

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
         HOY                  DATE

    DEFINE #glo #char
         borra_lineas         CHAR(200) ,
         ch                   CHAR(220) ,
         RUTA                 CHAR(200) ,
         G_LISTA_1            CHAR(200) ,
         G_LISTA_2            CHAR(200) ,
         enter                CHAR(001) ,
         vestad_desc          CHAR(040)

    DEFINE #glo #integer
         nro_rechazado        ,
         nro_reactiva         ,
         nro_altas            INTEGER

END GLOBALS

MAIN
    CALL init() #i

    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    OPEN WINDOW proc0151 AT 4,4 WITH FORM "PROC0151" ATTRIBUTE(BORDER)
    DISPLAY "                           < Ctrl-C > Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " PROC015          ENVIO DE PROMOTORES PARA REACTIVACION                                     " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    WHILE TRUE
        PROMPT " ESTA SEGURO S/N ? " FOR CHAR enter
        IF enter MATCHES "[SsNn]" THEN
            IF enter MATCHES "[Ss]" THEN
                LET nro_altas    = 0

                SELECT COUNT(*)
                INTO   nro_altas
                FROM   pro_solicitud A, pro_mae_promotor B              --(v1)
                WHERE  A.status_interno  = 7
                AND    A.resuelva       >= 8
                AND    B.cod_promotor = A.cod_promotor                  --(v1)
                AND    B.diag_proceso in ("7E", "7T")                   --(v1)
                    

                IF nro_altas = 0 THEN
                    PROMPT " NO HAY PROMOTORES PARA ENVIAR...",
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
    CLOSE WINDOW proc0151

    OPEN WINDOW proc0151 AT 4,4 WITH FORM "PROC0151" ATTRIBUTE(BORDER)
    DISPLAY "                           < Ctrl-C > Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " PROC015          ENVIO DE PROMOTORES PARA REACTIVACION                                     " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    DISPLAY " NRO DE SOLICITUDES PARA REACTIVACIONES : ",nro_reactiva  AT 10,15

    PROMPT " PROCESO FINALIZADO ...<ENTER> PARA SALIR " FOR CHAR enter

    CLOSE WINDOW proc0151

END MAIN

FUNCTION primer_paso()
#pp-------------------
    DECLARE cur_1 CURSOR FOR
    SELECT A.*
    FROM   pro_solicitud A,pro_mae_promotor B                          --(v1)
    WHERE  A.status_interno  = 7
    AND    A.resuelva       >= 8
    AND    B.cod_promotor = A.cod_promotor                             --(v1)
    AND    B.diag_proceso in ("7E", "7T")                              --(v1)


    LET G_LISTA_1     = RUTA CLIPPED,"/ree"
    LET G_LISTA_2     = RUTA CLIPPED,"/REE"
    LET nro_reactiva  = 0

    START REPORT listado_1 TO G_LISTA_1
        FOREACH cur_1 INTO reg_2.*

            LET reg_3.calle_numer = reg_2.calle  CLIPPED ," ",
                                    reg_2.numero CLIPPED ," ",
                                    reg_2.dpto

            SELECT A.*
            INTO   rd.*
            FROM   tab_delegacion A
            WHERE  A.deleg_cod = reg_2.deleg

            IF STATUS = NOTFOUND THEN
                PROMPT " NO EXISTE DELEGACION ",reg_2.deleg USING"&&&&&&&&",
                       ". NUMERO DE NOMINA ",reg_2.codven
                FOR CHAR enter

                EXIT PROGRAM
            ELSE
                LET reg_3.deleg = rd.deleg_desc
            END IF


            SELECT *
            INTO   rf.*
            FROM   tab_ciudad
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

                IF  reg_3.ciudad IS NULL
                OR  reg_3.ciudad = " "  THEN

                    SELECT estad_desc
                    INTO   vestad_desc
                    FROM   tab_estado
                    WHERE  estad_cod = reg_2.estado

                    LET reg_3.ciudad = "LOCALIDAD DE ",vestad_desc
                END IF

                OUTPUT TO REPORT listado_1 ("308"               ,
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
                                            reg_2.escolar
                                           ) #l1

                LET nro_reactiva = nro_reactiva + 1

                UPDATE pro_solicitud
                SET    pro_solicitud.status_interno = 8            ,
                       pro_solicitud.status         = 1            ,
                       pro_solicitud.fenvio         = HOY
                WHERE  pro_solicitud.status_interno = 7
                AND    pro_solicitud.rfc            = reg_2.rfc

                INSERT INTO pro_envio_reac  
                VALUES(reg_2.nro_solicitud,
                       reg_2.codven,
                       reg_2.cod_promotor,
                       HOY,
                       8)

        END FOREACH
    FINISH REPORT listado_1

    LET ch = "chmod 777 ",G_LISTA_1 CLIPPED
    RUN ch

    LET borra_lineas = "sed -e '/^$/d' ",G_LISTA_1 CLIPPED," > ",
                                         G_LISTA_2 CLIPPED
    RUN borra_lineas

    IF nro_reactiva > 0 THEN
        INSERT INTO pro_ctr_envio VALUES (HOY,"REE",2,"",nro_reactiva,"")
    END IF

END FUNCTION

REPORT listado_1(r_308)
#l1--------------------
    DEFINE r_308 RECORD #loc #r_308
           tip_registr     CHAR(003) ,
           nombres         CHAR(040) ,
           paterno         CHAR(040) ,
           materno         CHAR(040) ,
           rfc             CHAR(013) ,
           nss             CHAR(011) ,
           curp            CHAR(018) ,
           calle_numer     CHAR(040) ,
           colonia         CHAR(030) ,
           deleg_munic     CHAR(030) ,
           ciudad          CHAR(030) ,
           estado          CHAR(002) ,
           codpos          CHAR(005) ,
           telefono        CHAR(100) ,
           res_examen      SMALLINT  ,
           cod_promotor    CHAR(010) ,
           fec_nacimie     CHAR(008) ,
           fecha_baja      DATE      ,
           escolar         CHAR(1)
    END RECORD

    DEFINE r_000  RECORD #loc #r_000
           tip_registr     CHAR(3) ,
           num_registr     CHAR(5) ,
           tam_registr     CHAR(3) ,
           tip_archivo     CHAR(3) ,
           cve_afore       CHAR(3) ,
           fec_transm      CHAR(8) ,
           lot_afore       CHAR(9)
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
        LET r_308.fec_nacimie = YEAR(reg_2.fnaci)  USING "&&&&",
                                MONTH(reg_2.fnaci) USING "&&" ,
                                DAY(reg_2.fnaci)   USING "&&"

        PRINT
            COLUMN 001,r_308.tip_registr                      ,
            COLUMN 004,r_308.cod_promotor  USING "&&&&&&&&&&" ,
            COLUMN 014,r_308.nombres                          ,
            COLUMN 054,r_308.paterno                          ,
            COLUMN 094,r_308.materno                          ,
            COLUMN 134,r_308.rfc                              ,
            COLUMN 147,r_308.nss                              ,
            COLUMN 158,r_308.curp                             ,
            COLUMN 176,r_308.calle_numer                      ,
            COLUMN 216,r_308.colonia                          ,
            COLUMN 246,r_308.deleg_munic                      ,
            COLUMN 276,r_308.ciudad                           ,
            COLUMN 306,r_308.estado        USING "&&"         ,
            COLUMN 308,r_308.codpos        USING "&&&&&"      ,
            COLUMN 313,r_308.telefono                         ,
            COLUMN 413,r_308.fec_nacimie                      ,
            COLUMN 421,r_308.res_examen    USING "&&"         ,
            COLUMN 423,r_308.escolar                          ,
            COLUMN 424,26 SPACES
END REPORT

FUNCTION init()
#i-------------
    LET HOY      = TODAY

    SELECT *
    INTO   g_seg_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "pro"

    LET RUTA = g_seg_modulo.ruta_envio CLIPPED
END FUNCTION
