#*******************************************************************#
#Proyecto          => Sistema de Afores.( MEXICO )                  #
#Propietario       => E.F.P.                                        #
#Programa          => DISB048                                       #
#Descripcion       => CARGA Y CONSULTA ARCHIVO ACL ESPECIAL         #
#Sistema           => DIS .                                         #
#Fecha             => 05/05/2003                                    #
#Por               => Laura Eugenia Cortes Guzman                   #
#Adecuaciones      => 18082003                                      #
#Modifico          => DMR 17/10/2014 CPL-1766 se quito de rm disb048#
#*******************************************************************#

DATABASE safre_af
GLOBALS
  DEFINE HOY           DATE,
         enter         CHAR(01),
         nom_archivo   CHAR(20),
         n_arch        CHAR(20),
         usuario       CHAR(08),
         ejecuta       CHAR(250),
         cuantos       INTEGER,
         long,i        INTEGER ,

         p_modulo      RECORD LIKE seg_modulo.*,
         p_tablayout   RECORD LIKE tab_layout.*

END GLOBALS

MAIN

  LET HOY = TODAY

  CALL inicio()

##  DEFER INTERRUPT
  OPTIONS INPUT WRAP,
          PROMPT LINE LAST,
          ACCEPT KEY CONTROL-I

    CALL STARTLOG("DISB048.log")

    OPEN WINDOW ventana_1 AT 2,2 WITH 3 ROWS, 76 COLUMNS ATTRIBUTE(BORDER)

         MENU "DISPERSION"
              COMMAND "Carga " "Carga del archivo"
                      WHENEVER ERROR CONTINUE
                         DATABASE safre_tmp
                            DROP TABLE dis_acl_visual
                            DROP TABLE dis_acl_visual2
                            DROP TABLE dis_nom_archivo

                            create table dis_nom_archivo
                            (
                               nombre   char(25)
                            );

                            create table dis_acl_visual
                            (
                              consecutivo          integer  ,
                              nss                  char(11) ,
                              rfc                  char(13) ,
                              curp                 CHAR(18),
                              nombre               char(50),
                              per_pago             char(06),
                              folio_pago_sua       char(06),
                              reg_patron_imss      char(11),
                              rfc_patron           char(13),
                              ident_viv_garantia   smallint,
                              resul_operacion      char(02),
                              marca                CHAR(1)
                            )

                            CREATE TABLE dis_acl_visual2
                            (
                               consecutivo    INTEGER
                            )

                      WHENEVER ERROR STOP
                      DATABASE safre_af

                      CALL carga()
              COMMAND "Consulta " "Consulta y Generacion de Archivo"
                      CALL consulta()
              COMMAND "Generar " "Generar Archivo"
                      CALL genera_archivo()
              COMMAND "Imprimir " "Imprimir"
                      CALL imprime()
              COMMAND "Salir " "Salir del Programa"
                      EXIT MENU
         END MENU

    CLOSE WINDOW ventana_1
END MAIN
#
FUNCTION inicio()
   INITIALIZE p_modulo.*, nom_archivo, n_arch TO NULL
   LET long = 0   LET i = 0

   SELECT *, USER
   INTO   p_modulo.*, usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "dis"

   SELECT *
   INTO   p_tablayout.*
   FROM   tab_layout
   WHERE  layout_cod = 200

END FUNCTION
#
FUNCTION carga()

    OPEN WINDOW vent_carga AT 5,2 WITH FORM "DISB0481" ATTRIBUTE(BORDER)

    DISPLAY " < ESC > Procesar                                          < Ctrl-C > Salir    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " DISB048             C A R G A   D E   A R C H I V O                           " AT 2,1 ATTRIBUTE(REVERSE)
    DISPlAY HOY USING"DD-MM-YYYY " AT 2,66 ATTRIBUTE(REVERSE)

      CREATE TEMP TABLE arch_dis
      (
         lista     CHAR(100)
      )

      LET ejecuta = "ls ",p_modulo.ruta_rescate CLIPPED, "/"

      LET ejecuta = ejecuta CLIPPED, " > arc_dis" CLIPPED
      RUN ejecuta

      LET ejecuta = "arc_dis"
      LOAD FROM ejecuta INSERT INTO arch_dis

      INPUT BY NAME nom_archivo WITHOUT DEFAULTS
            AFTER FIELD nom_archivo
                IF nom_archivo IS NULL THEN
                   ERROR "DEBE TECLEAR EL NOMBRE DEL ARCHIVO A PROCESAR ..."
                   NEXT FIELD nom_archivo
                END IF

                SELECT "OK" FROM arch_dis
                       WHERE lista = nom_archivo
                IF STATUS = NOTFOUND THEN
                   ERROR "NOMBRE DE ARCHIVO INCORRECTO ... NO EXISTE"
                   NEXT FIELD nom_archivo
                END IF

            ON KEY(ESC)
                IF nom_archivo IS NULL THEN
                   ERROR "DEBE TECLEAR EL NOMBRE DEL ARCHIVO A PROCESAR ..."
                   NEXT FIELD nom_archivo
                END IF

                SELECT "OK" FROM arch_dis
                       WHERE lista = nom_archivo
                IF STATUS = NOTFOUND THEN
                   ERROR "NOMBRE DE ARCHIVO INCORRECTO ... NO EXISTE"
                   NEXT FIELD nom_archivo
                END IF

                LET n_arch =nom_archivo CLIPPED

                DISPLAY " PROCESANDO INFORMACION...AGUARDE UN MOMENTO... "
                        AT 16,1 ATTRIBUTE(REVERSE)
                SLEEP 2
                IF recepciona_archivo() != FALSE THEN
                   LET cuantos = 0

                   INSERT INTO safre_tmp:dis_nom_archivo
                               VALUES(nom_archivo)

                   SELECT COUNT(*) INTO cuantos FROM safre_tmp:dis_acl_visual

                   DISPLAY "NUMERO DE REGISTROS : ", cuantos AT 16,1
                   PROMPT "PROCESO FINALIZADO...<ENTER> PARA CONTINUAR"
                   FOR enter
                ELSE
                   DISPLAY "VERIFIQUE, HUBO PROBLEMAS EN LA CARGA " AT 16,1
                   PROMPT "PROCESO FINALIZADO...<ENTER> PARA CONTINUAR"
                   FOR enter
                END IF
                EXIT INPUT

              ON KEY (CONTROL-C,INTERRUPT)
                   PROMPT "PROCESO CANCELADO...<ENTER> PARA CONTINUAR"
                   FOR enter
                 EXIT INPUT

      END INPUT
      DROP TABLE arch_dis
    CLOSE WINDOW vent_carga
END FUNCTION

FUNCTION recepciona_archivo()
#separo cabezera
    DEFINE   cuantos  INTEGER,
             reg_m    RECORD
                  consecutivo   integer,
                  nss           char(11),
                  rfc           char(13),
                  nombre        char(50)
             END RECORD

    LET cuantos = 0

    INITIALIZE reg_m.* TO NULL

    LET ejecuta = "sed -e '/^02/!d' ",
                  p_modulo.ruta_rescate CLIPPED,
                  "/",nom_archivo CLIPPED," > ",
                  p_modulo.ruta_rescate CLIPPED,
                  "/dis_48_2"
    RUN ejecuta

    -- separa archivo --
    LET ejecuta = "cd ",
                  p_modulo.ruta_rescate CLIPPED,
                  "/;","sh -x dis048.sh "
    RUN ejecuta

    CALL salida(1,p_tablayout.arch_det,p_tablayout.tab_det)

    DISPLAY "archivo ", p_tablayout.nom_arch
    START REPORT r_report_db TO p_tablayout.nom_arch

    OUTPUT TO REPORT r_report_db (p_tablayout.arch_det,
                                  p_tablayout.layout_cod,
                                  1,
                                  p_tablayout.tab_det
                                 )
    FINISH REPORT r_report_db

    LET ejecuta = "cd ",p_modulo.ruta_rescate CLIPPED,
                  "/;dbload -d safre_tmp -c ",
                  p_tablayout.nom_arch,
                  " -l " CLIPPED,
                  " ",
                  p_tablayout.arch_det CLIPPED,
                  ".err" CLIPPED, 
                  " -e 100 -n 1000 -k "
    RUN ejecuta

    LET ejecuta = "cd ",p_modulo.ruta_rescate CLIPPED,                # CPL-1766
                  "/; rm dis_48a dis_48b dis_48c dis_48d dis_48_1 ",
                  " dis_48_2 dis_48 dis_48e dis_48f dis_48g dis_48h ",
                  " dis_48i dis_48j "
    RUN ejecuta

    SELECT COUNT(*) INTO cuantos FROM safre_tmp:dis_acl_visual
    IF cuantos = 0 OR cuantos IS NULL THEN
       RETURN FALSE
    ELSE
       RETURN TRUE
    END IF

END FUNCTION
#
FUNCTION salida(v_tipo_reg,v_nom_arch,v_tabla)
    DEFINE v_tabla               CHAR(20),
           v_tipo_reg            SMALLINT,
           nom_archivo_dat       CHAR(60),
           nom_archivo_masiva    CHAR(60),
           v_nom_arch            CHAR(60)

    LET nom_archivo_masiva = p_modulo.ruta_rescate CLIPPED, "/",
                             p_tablayout.nom_arch
    LET nom_archivo_dat    = p_modulo.ruta_rescate CLIPPED, "/",
                             v_nom_arch

    START REPORT r_report_db TO nom_archivo_masiva

      OUTPUT TO REPORT r_report_db(nom_archivo_dat ,
                            p_tablayout.layout_cod,
                            v_tipo_reg,          #cza, det, sum
                            v_tabla              #nombre de la tabla a insertar
                           )

    FINISH REPORT r_report_db

END FUNCTION
#
FUNCTION consulta()
    DEFINE reg   RECORD
               conse         INTEGER ,
               nss           CHAR(11),
               rfc           CHAR(13),
               nombre        CHAR(50)
           END RECORD,

           arreglo ARRAY[5000] OF RECORD
               conse         INTEGER ,
               nss           CHAR(11),
               rfc_arch      CHAR(13),
               rfc_afi       CHAR(13),
               nombre_arch   CHAR(50),
               nombre_afi    CHAR(50),
               marca         CHAR(50)
           END RECORD,

           pos, son, pos1    INTEGER ,
           arr_c             INTEGER ,
           arr_l             INTEGER ,
           paterno           CHAR(40),
           materno           CHAR(40),
           nombre            CHAR(40),
           nombre_afi        CHAR(50),
           rfc_afi           CHAR(13)

    INITIALIZE paterno, materno, nombre, rfc_afi, nombre_afi, reg.* TO NULL

    LET pos   = 0        LET arr_c = 0        LET arr_l = 0
    LET i     = 0        LET son   = 0        LET long  = 0

    DISPLAY "AGUARDE UN MOMENTO" AT 2,1

    DECLARE con_dis CURSOR FOR
            SELECT a.consecutivo, a.nss, a.rfc, a.nombre 
              FROM safre_tmp:dis_acl_visual a
             WHERE a.marca IS NULL
                OR a.marca = " "
                OR a.marca = 0   --c22-11
             ORDER BY 1
    FOREACH con_dis INTO reg.*

       INITIALIZE paterno, materno, nombre, rfc_afi, nombre_afi TO NULL

       SELECT a.paterno, a.materno, a.nombres, a.n_rfc
       INTO   paterno, materno, nombre,  rfc_afi
       FROM   afi_mae_afiliado a
       WHERE  a.n_seguro = reg.nss

       IF STATUS = NOTFOUND THEN
          CONTINUE FOREACH
       END IF

       LET pos         = pos + 1

       LET nombre_afi  = paterno CLIPPED, " ",
                         materno CLIPPED, " ",
                         nombre  CLIPPED

       LET arreglo[pos].conse          = reg.conse
       LET arreglo[pos].nss            = reg.nss
       LET arreglo[pos].rfc_arch       = reg.rfc
       LET arreglo[pos].rfc_afi        = rfc_afi
       LET arreglo[pos].nombre_arch    = reg.nombre CLIPPED
       LET arreglo[pos].nombre_afi     = nombre_afi CLIPPED
       LET arreglo[pos].marca          = " "
    END FOREACH

    LET pos1 = pos
    IF (pos-1) < 1 THEN
       ERROR "NO HAY INFORMACION..."
       SLEEP 3
       RETURN
    END IF

    DISPLAY "" AT 2,1
    OPEN WINDOW vent_consu AT 5,2 WITH FORM "DISB0482" ATTRIBUTE(BORDER)

    DISPLAY "<ESC> Genera Archivo                                    <Ctrl-C> Salir         " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY "DISB048                     C O N S U L T A                                    " AT 2,1 ATTRIBUTE(REVERSE)

    DISPlAY HOY USING "DD-MM-YYYY " AT 2,66 ATTRIBUTE(REVERSE)
    CALL SET_COUNT(pos)
    INPUT ARRAY arreglo WITHOUT DEFAULTS FROM scr_1.*
          BEFORE FIELD marca
                 LET arr_c = ARR_CURR()
                 LET arr_l = SCR_LINE()

          AFTER FIELD marca
                IF pos < arr_c AND arreglo[arr_c].marca = "X" THEN
                   ERROR "El registro no puede ser seleccionado"
                   LET arreglo[arr_c].marca = " "
                END IF

          ON KEY (ESC)

             LET son = 0
             FOR i = 1 TO ARR_CURR()
                 IF arreglo[i].marca = "X" THEN
                    LET son = son + 1
                    INSERT INTO safre_tmp:dis_acl_visual2 
                                VALUES(arreglo[i].conse)
                 END IF
             END FOR

             DISPLAY "" AT 18,1
             IF son = 0 THEN
                PROMPT " PROCESO FINALIZADO...<ENTER> PARA CONTINUAR" FOR enter
             ELSE
                CALL actualiza()
                PROMPT " PROCESO FINALIZADO...<ENTER> PARA CONTINUAR" FOR enter
             END IF
             EXIT INPUT

          ON KEY (CONTROL-C,INTERRUPT)
                PROMPT "PROCESO CANCELADO...<ENTER> PARA CONTINUAR" FOR enter
                EXIT INPUT
    END INPUT

    CLOSE WINDOW vent_consu
END FUNCTION
########### ACTUALIZA

FUNCTION actualiza()
    DEFINE  re_g    RECORD LIKE safre_tmp:dis_acl_visual2.*

    DECLARE act_liza CURSOR FOR
        SELECT * FROM safre_tmp:dis_acl_visual2
        ORDER BY 1
    FOREACH act_liza INTO re_g.*
        UPDATE safre_tmp:dis_acl_visual
           SET marca = "1"
         WHERE consecutivo = re_g.consecutivo
    END FOREACH

END FUNCTION

########### GENERA ARCHIVO

FUNCTION genera_archivo()
    DEFINE   ger    RECORD LIKE safre_tmp:dis_acl_visual2.*,
             G_LISTA           CHAR(200),
             nombre1           CHAR(25),
             ban               INTEGER,
             ban1              INTEGER

    INITIALIZE ger.*, G_LISTA, n_arch TO NULL
    LET long = 0 LET i = 0 LET ban = 0   LET ban1 = 0

    OPEN WINDOW ven_genera AT 5,2 WITH FORM "DISB0483" ATTRIBUTE(BORDER)
    DISPLAY "                                                                               " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY "DISB048               G E N E R A     A R C H I V O                            " AT 2,1 ATTRIBUTE(REVERSE)
    DISPlAY HOY USING"DD-MM-YYYY " AT 2,66 ATTRIBUTE(REVERSE)


    DISPLAY "GENERANDO INFORMACION.... AGUARDE UNOS MOMENTOS...." AT 18,1
    SLEEP 3

    SELECT v.nombre INTO n_arch FROM safre_tmp:dis_nom_archivo v
    IF STATUS = NOTFOUND THEN
       ERROR " NO EXISTE INFORMACION PARA GENERAR EL ARCHIVO "
       SLEEP 3
       ERROR ""
       CLOSE WINDOW ven_genera
       RETURN
    END IF

    LET long = LENGTH(n_arch CLIPPED)
--    LET n_arch =n_arch[1,long-4]

    INITIALIZE nombre1 TO NULL

    FOR I = LONG TO 1 STEP -1
        IF n_arch[i,i] = "." THEN
           LET ban = ban + 1
        END IF
    END FOR

    FOR I = LONG TO 1 STEP -1
        IF n_arch[i,i] = "." THEN
           LET ban1 = ban1 + 1
           IF ban = ban1 THEN
              LET nombre1 = n_arch[1,i-1]
           END IF
        END IF
    END FOR

    LET n_arch = nombre1 CLIPPED

    LET G_LISTA = p_modulo.ruta_rescate CLIPPED, "/",n_arch CLIPPED,".con"

    START REPORT gen_cons_dis TO G_LISTA

    SELECT UNIQUE "X" FROM safre_tmp:dis_acl_visual2
    IF STATUS = NOTFOUND THEN
       ERROR " NO EXISTE INFORMACION PARA GENERAR EL ARCHIVO "
       SLEEP 3
       ERROR ""
       RETURN
    END IF

    DECLARE gen_1 CURSOR FOR
       SELECT * 
         FROM safre_tmp:dis_acl_visual2
        ORDER BY 1
    FOREACH gen_1 INTO ger.*
            OUTPUT TO REPORT gen_cons_dis(ger.*)
    END FOREACH

    FINISH REPORT gen_cons_dis
    DISPLAY "" AT 18,1
    DISPLAY " ARCHIVO GENERADO EN : ",G_LISTA CLIPPED AT 18,1
    PROMPT " PROCESO FINALIZADO...<ENTER> PARA CONTINUAR" FOR enter
    CLOSE WINDOW ven_genera


END FUNCTION
########### GENERA CONSECUTIVO

REPORT gen_cons_dis(consecutivo)

    DEFINE  consecutivo   INTEGER

     OUTPUT
          TOP MARGIN    0
          BOTTOM MARGIN 0
          LEFT MARGIN   0
          RIGHT MARGIN  0
          PAGE LENGTH   1
     FORMAT

     ON EVERY ROW
        PRINT COLUMN 1,consecutivo
END REPORT

########### IMPRIME
FUNCTION imprime()
    DEFINE   reg22    RECORD LIKE safre_tmp:dis_acl_visual.*,

             pos, son, pos1    INTEGER ,
             arr_c             INTEGER ,
             arr_l             INTEGER ,
             hora              CHAR(08),

             apo       RECORD
                 ident_viv        CHAR(01),
                 result_operacion CHAR(02),
                 ppago            CHAR(06),
                 fpsua            CHAR(06),
                 patron_i         CHAR(11),
                 patron_rfc       CHAR(13)
             END RECORD,

             afi       RECORD
                 paterno      CHAR(40),
                 materno      CHAR(40),
                 nombre       CHAR(40),
                 rfc          CHAR(13),
                 curp         CHAR(18)
             END RECORD,

             nombre_afi        CHAR(50),
             G_IMP             CHAR(200)

    DEFINE reg_rpt RECORD
        ident_viv_garantia     CHAR(1),
        result_operacion       CHAR(2),
        periodo_pago           CHAR(6),
        folio_pago_sua         CHAR(6),
        reg_patronal_imss      CHAR(11),
        rfc_patron             CHAR(13)
    END RECORD

    LET pos   = 0        LET arr_c = 0        LET arr_l = 0
    LET i     = 0        LET son   = 0        LET long  = 0

    INITIALIZE hora, nombre_afi, afi.*, reg22.*, apo.*,G_IMP TO NULL

    LET hora = TIME

    OPEN WINDOW vent_imprime AT 5,2 WITH FORM "DISB0483" ATTRIBUTE(BORDER)
    DISPLAY "                                                                               " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY "DISB048               G E N E R A   R E P O R T E                              " AT 2,1 ATTRIBUTE(REVERSE)
    DISPlAY HOY USING"DD-MM-YYYY " AT 2,66 ATTRIBUTE(REVERSE)

    LET G_IMP = p_modulo.ruta_rescate CLIPPED,usuario CLIPPED,
                ".ACL_ESP",TODAY USING "ddmmyy" CLIPPED,hora CLIPPED

    DISPLAY " GENERANDO INFORMACION.... AGUARDE UN MOMENTO.... " AT 18,1
            ATTRIBUTE(REVERSE)

    SELECT UNIQUE "X" from safre_tmp:dis_acl_visual
    IF STATUS = NOTFOUND THEN
       ERROR " NO EXISTE INFORMACION... "
       SLEEP 2
       ERROR ""
       CLOSE WINDOW vent_imprime
       RETURN
    END IF

    START REPORT genera_rr TO G_IMP

    DECLARE condis1 CURSOR FOR
            SELECT * from safre_tmp:dis_acl_visual
            ORDER BY 1
    FOREACH condis1 INTO reg22.*

       INITIALIZE afi.*, nombre_afi TO NULL

       SELECT a.paterno, a.materno, a.nombres, a.n_rfc, a.n_unico
       INTO   afi.*
       FROM   afi_mae_afiliado a
       WHERE  a.n_seguro = reg22.nss

       IF STATUS = NOTFOUND THEN
          CONTINUE FOREACH
       END IF

       LET pos         = pos + 1

       LET nombre_afi  = afi.paterno CLIPPED, " ",
                         afi.materno CLIPPED, " ",
                         afi.nombre  CLIPPED

       DECLARE cur_rpt CURSOR FOR
           SELECT ident_viv_garantia,
                  result_operacion,
                  max(periodo_pago),
                  max(folio_pago_sua),
                  max(reg_patronal_imss),
                  max(rfc_patron)
           FROM dis_det_aporte
           WHERE n_seguro = reg22.nss
           GROUP BY 1,2

        FOREACH cur_rpt INTO reg_rpt.*
           OUTPUT TO REPORT genera_rr(reg22.*, nombre_afi, afi.*, reg_rpt.*)
        END FOREACH
    END FOREACH

    FINISH REPORT genera_rr
    DISPLAY "" AT 18,1
    DISPLAY " ARCHIVO GENERADO EN : ", G_IMP CLIPPED AT 18,1
    PROMPT " PROCESO FINALIZADO.... < ENTER > PARA CONTINUAR...." for enter
    CLOSE WINDOW vent_imprime

END FUNCTION
########### GENERA REPORTE

REPORT genera_rr(reg22, nombre_afi, afi, apo)

    DEFINE   reg22    RECORD LIKE safre_tmp:dis_acl_visual.*,

             nombre_afi      CHAR(50),

             afi      RECORD
                paterno      CHAR(40),
                materno      CHAR(40),
                nombre       CHAR(40),
                rfc          CHAR(13),
                curp         CHAR(18)
             END RECORD,

             apo       RECORD
                 ident_viv        CHAR(01),
                 result_operacion CHAR(02),
                 ppago            CHAR(06),
                 fpsua            CHAR(06),
                 patron_i         CHAR(11),
                 patron_rfc       CHAR(13)
             END RECORD,

             L1              CHAR(01),
             L4              CHAR(04),
             L10             CHAR(10),
             hora            CHAR(08),
             enter           CHAR(1)
    OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   60

    FORMAT
      PAGE HEADER
           LET L1 = "\304"
           LET L4 = "\304\304\304\304"
           LET L10= "\304\304\304\304\304\304\304\304\304\304"

           PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'

           PRINT COLUMN 1,'\033e\033(s218T\033(s18H\033(s8B'

           PRINT COLUMN 076,"GERENCIA DE RECAUDACION Y RETIROS"
           PRINT COLUMN 077,"NSS EN ACLARACIONES ESPECIALES"
           PRINT COLUMN 180,"DISB048"
           PRINT COLUMN 179,"PAG. ",pageno using "##&"
           PRINT COLUMN 177, TODAY USING "dd/mm/yyyy"


        PRINT L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,
              L10,L10,L10,L10,L10,L10,L4

        PRINT COLUMN 001, "REG. ",
              COLUMN 006, "CONSECUTIVO",
              COLUMN 019, "N.S.S.",
              COLUMN 032, "N O M B R E ",
              COLUMN 075, "RFC TRABAJADOR",
              COLUMN 091, "CURP",
              COLUMN 111, "PER. PAGO",
              COLUMN 123, "FOLIO P.SUA",
              COLUMN 136, "REG.PAT.IMSS",
              COLUMN 150, "RFC PATRON",
              COLUMN 165, "VIV.GRANT.",
              COLUMN 177, "DIAG."

        PRINT L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,
              L10,L10,L10,L10,L10,L10

        SKIP 1 LINE

      ON EVERY ROW

        PRINT COLUMN 003, "1",
              COLUMN 009, reg22.consecutivo USING "#######&",
              COLUMN 019, reg22.nss,
              COLUMN 032, reg22.nombre[1,40],
              COLUMN 075, reg22.rfc,
              COLUMN 091, reg22.curp,
              COLUMN 111, reg22.per_pago,
              COLUMN 123, reg22.folio_pago_sua,
              COLUMN 136, reg22.reg_patron_imss,
              COLUMN 150, reg22.rfc_patron,
              COLUMN 168, reg22.ident_viv_garantia USING "#####",
              COLUMN 178, reg22.resul_operacion

        PRINT COLUMN 003, "2",
              COLUMN 019, reg22.nss,
              COLUMN 032, nombre_afi[1,40],
              COLUMN 075, afi.rfc,
              COLUMN 091, afi.curp,
              COLUMN 111, apo.ppago,
              COLUMN 123, apo.fpsua,
              COLUMN 136, apo.patron_i,
              COLUMN 150, apo.patron_rfc,
              COLUMN 168, apo.ident_viv,
              COLUMN 178, apo.result_operacion

        SKIP 1 LINE

   ON LAST ROW
        PRINT L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,
              L10,L10,L10,L10,L10,L10


END REPORT
