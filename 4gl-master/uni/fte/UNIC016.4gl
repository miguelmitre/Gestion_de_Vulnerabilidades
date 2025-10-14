###############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                            #
#Propietario       => EFP                                                     #
#Programa  UNIL001 => REPORTE DE POSIBLES CUENTAS A UNIFICAR                  #
#Fecha             => 16 DE AGOSTO DEL 2000                                   #
#Actualizado       => ARMANDO RODRIGUEZ CASTROPAREDES.                        #
#Fecha actualiza   => 12 de agosto de 2003.                                   #
#Actualizado por   => MIGUEL ANGEL HERNANDEZ MARTINEZ.                        #
#Fecha actualiza   => 09 de marzo  de 2005.                                   #
#Actualizado por   => OMAR SANDOVAL BADILLO.                                  #
###############################################################################
DATABASE safre_af
GLOBALS
   DEFINE reg_2 RECORD
          folio             INTEGER,
          status_convoca    CHAR(1),
          nss_uni           CHAR(11),
          rfc_uni           CHAR(13),
          paterno_uni       CHAR(40),
          materno_uni       CHAR(40),
          nombre_uni        CHAR(40),
          ident_movimiento  CHAR(2),
          num_ctas_asoc     SMALLINT,
          cve_ent_nss       CHAR(3),
          cve_afo_recep     CHAR(3),
          resul_operacion   CHAR(2),
          estado            SMALLINT,
          sello_electronico CHAR(24),
          n_folio           INTEGER
   END RECORD

   DEFINE reg_3 RECORD
          status_convoca    CHAR(1),
          nss_uni           CHAR(11),
          nss_cta1          CHAR(11),
          rfc_cta1          CHAR(13),
          cve_ent_cta1      CHAR(03),
          paterno_cta1      CHAR(40),
          materno_cta1      CHAR(40),
          nombre_cta1       CHAR(40),
          diag_unifica      CHAR(2),
          estado            SMALLINT,
          sello_electronico CHAR(24),
          n_folio           INTEGER
   END RECORD

   DEFINE det_uni RECORD
           total            INTEGER,
           clave            CHAR(03)
   END RECORD

   DEFINE det_cta  RECORD
          total             INTEGER,
          clave             CHAR(03)
   END RECORD

   DEFINE cont,
          cont1,
          i,
          vfolio2,
          vfolio,
          tot_registros     INTEGER

   DEFINE HOY               DATE

   DEFINE usuario           CHAR(8),
          G_LISTA           CHAR(500),
          G_LISTA1          CHAR(500),
          G_LISTA2          CHAR(100),
          G_LISTA3          CHAR(100),
          imprime           CHAR(100),
          borra             CHAR(100),
          cat               CHAR(500),
          cla_sel           CHAR(250),
          nss_unifica       CHAR(011),
          cve_ent_uni       CHAR(003),
          cve_ent_cta       CHAR(003),
          vcodigo_afore     CHAR(003),
          vrazon_social     CHAR(050),
          aux_pausa         CHAR(001),
          bold              CHAR(032),
          enter             CHAR(001)

   DEFINE g_paramgrales  RECORD LIKE seg_modulo.*

END GLOBALS
##############################################################
MAIN
   OPTIONS
      INPUT WRAP,
      PROMPT LINE LAST,
      ACCEPT KEY CONTROL-I

   DEFER INTERRUPT

   CALL STARTLOG("UNIC016.log")
   CALL inicio()
   CALL proceso_principal()

END MAIN
##############################################################
FUNCTION inicio()

   LET HOY = TODAY

   SELECT codigo_afore,
          razon_social
   INTO   vcodigo_afore,
          vrazon_social
   FROM   tab_afore_local

   LET bold  = '\033e\033(s218T\033(s12H\033(s7B'

END FUNCTION
##############################################################
FUNCTION proceso_principal()

   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "UNIC0161" ATTRIBUTE(BORDER)
   DISPLAY " UNIC016         REPORTE DE ESTADO DE PROCESO POR FOLIO                      " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY " [ Esc ] Iniciar                                            [ Ctrl-C ] Salir " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

   INPUT BY NAME vfolio

      AFTER FIELD vfolio
         IF vfolio IS NULL THEN
            ERROR " EL FOLIO NO PUEDE SER NULO "
            NEXT FIELD vfolio
         END IF

         SELECT "X"
         FROM   uni_cza_notifica
         WHERE  folio = vfolio
         GROUP BY 1

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR " NO EXISTE EL FOLIO "
            NEXT FIELD vfolio
         END IF

      ON KEY (ESC)
         IF vfolio IS NULL THEN
            ERROR " EL FOLIO NO PUEDE SER NULO "
            NEXT FIELD vfolio
         END IF

         SELECT "X"
         FROM   uni_cza_notifica
         WHERE  folio = vfolio
         GROUP BY 1

         IF SQLCA.SQLCODE <> 0 THEN
            ERROR " NO EXISTE EL FOLIO "
            NEXT FIELD vfolio
         END IF

         ERROR " PROCESANDO INFORMACION " 

         CALL genera_reporte()

         ERROR " REPORTE GENERADO "
         SLEEP 2
         EXIT PROGRAM
         ERROR ""

      ON KEY (INTERRUPT)
         ERROR " PROCESO CANCELADO "
         SLEEP 2
         EXIT PROGRAM

    END INPUT

    CLEAR WINDOW ventana_1
    CLOSE WINDOW ventana_1

END FUNCTION
##############################################################
FUNCTION pregunta()
   ERROR ""

   PROMPT " DESEA GENERAR EL ARCHIVO  S/N  ?  " FOR CHAR aux_pausa
END FUNCTION
##############################################################
FUNCTION genera_reporte()

    SELECT USER,
           *
    INTO   usuario,
           g_paramgrales.*
    FROM   seg_modulo
    WHERE  modulo_cod = "uni"

    LET G_LISTA = g_paramgrales.ruta_listados CLIPPED,
                  "/" CLIPPED,usuario CLIPPED,".Operacion_21" CLIPPED

    START REPORT listado_2 TO G_LISTA

       DECLARE cur_6 CURSOR FOR
       SELECT a.nss_uni,
              a.cve_ent_nss,
              b.cve_ent_cta1
       FROM   uni_unificador a,uni_unificado b
       WHERE  a.folio = vfolio
       AND    a.folio = b.folio
       AND    a.nss_uni = b.nss_uni

       FOREACH cur_6 INTO nss_unifica,
                          cve_ent_uni,
                          cve_ent_cta 

          LET cla_sel = "SELECT a.status_convoca,",
                               "a.nss_uni,",
                               "a.nss_cta1,",
                               "a.rfc_cta1,",
                               "a.cve_ent_cta1,",
                               "a.paterno_cta1,",
                               "a.materno_cta1,",
                               "a.nombre_cta1,",
                               "a.diag_unifica,",
                               "a.estado,",
                               "'',",
                               "'' ",
                        "FROM   uni_unificado a ",
                        "WHERE  a.folio = ? ",
                        "AND    a.nss_uni = ? ",
                        "GROUP BY 1,2,3,4,5,6,7,8,9,10 "

          PREPARE claexe FROM cla_sel
          DECLARE cur_3 CURSOR FOR claexe

          DECLARE cur_2 CURSOR FOR
          SELECT a.folio,
                 a.status_convoca,
                 a.nss_uni,
                 a.rfc_uni,
                 a.paterno_uni,
                 a.materno_uni,
                 a.nombre_uni,
                 a.ident_movimiento,
                 a.num_ctas_asoc,
                 a.cve_ent_nss,
                 a.cve_afo_recep,
                 a.resul_operacion,
                 a.estado,
                 "", 
                 ""
          FROM   uni_unificador a
          WHERE  a.folio   = vfolio
          AND    a.nss_uni = nss_unifica

          FOREACH cur_2 INTO reg_2.*
             IF reg_2.cve_ent_nss = vcodigo_afore THEN
                SELECT a.sello_electronico
                INTO   reg_2.sello_electronico
                FROM   afi_mae_afiliado a
                WHERE  a.n_seguro = reg_2.nss_uni
             ELSE 
                LET reg_2.sello_electronico = ""
             END IF

             OUTPUT TO REPORT listado_2(reg_2.*)

          END FOREACH
       END FOREACH
    FINISH REPORT listado_2

    --LET imprime = "vi ",G_LISTA
    LET imprime = "lp ",G_LISTA
    RUN imprime

END FUNCTION
###############################################################
REPORT listado_2(reg_2)

   DEFINE reg_2 RECORD
          folio             INTEGER,
          status_convoca    CHAR(1),
          nss_uni           CHAR(11),
          rfc_uni           CHAR(13),
          paterno_uni       CHAR(40),
          materno_uni       CHAR(40),
          nombre_uni        CHAR(40),
          ident_movimiento  CHAR(2),
          num_ctas_asoc     SMALLINT,
          cve_ent_nss       CHAR(3),
          cve_afo_recep     CHAR(3),
          resul_operacion   CHAR(2),
          estado            SMALLINT,
          sello_electronico CHAR(24),
          n_folio           INTEGER
   END RECORD

   DEFINE reg_3 RECORD
          status_convoca    CHAR(1),
          nss_uni           CHAR(11),
          nss_cta1          CHAR(11),
          rfc_cta1          CHAR(13),
          cve_ent_cta1      CHAR(03),
          paterno_cta1      CHAR(40),
          materno_cta1      CHAR(40),
          nombre_cta1       CHAR(40),
          diag_unifica      CHAR(2),
          estado            SMALLINT,
          sello_electronico CHAR(24),
          n_folio           INTEGER
   END RECORD

   DEFINE det_uni RECORD
          total            INTEGER,
          clave            CHAR(03)
   END RECORD

   DEFINE det_cta RECORD
          total             INTEGER,
          clave             CHAR(03)
   END RECORD

   DEFINE cont,
          cont1,
          i,
          vfolio2,
          vfolio,
          tot_registros     INTEGER

   DEFINE L1               CHAR(01),
          L2               CHAR(02),
          L3               CHAR(03),
          L4               CHAR(04),
          L5               CHAR(05),
          L6               CHAR(06),
          L7               CHAR(07),
          L8               CHAR(08),
          L9               CHAR(09),
          L10              CHAR(10)

   DEFINE x_descripcion       CHAR(40),
          x_descripcion_noti  CHAR(40),
          opc                 CHAR(1)

   DEFINE x_subtotal_uni      INTEGER,
          x_subtotal_cta      INTEGER,
          x_total             INTEGER

   OUTPUT
      PAGE LENGTH   80
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0

   FORMAT

   PAGE HEADER

      LET  L1  = "\304"
      LET  L2  = "\304\304"
      LET  L3  = "\304\304\304"
      LET  L4  = "\304\304\304\304"
      LET  L5  = "\304\304\304\304\304"
      LET  L10 = "\304\304\304\304\304\304\304\304\304\304"

      PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H','\033015'

      PRINT COLUMN 1,'\033e\033(s218T\033(s11H\033(s7B','\033015'

      PRINT COLUMN 004, vrazon_social,
            COLUMN 100, HOY USING "DD-MM-YYYY",'\033015'

      SKIP 1 LINE

      PRINT COLUMN 004,"UNIC016",
            COLUMN 040,"REPORTE DE POSIBLES CUENTAS A UNIFICAR",
            COLUMN 100, "P\240gina  ",pageno USING "##",'\033015'

      SKIP 1 LINE

      PRINT COLUMN 004,"Folio : ",reg_2.folio USING "#####",'\033015'

      SKIP 1 LINE

      PRINT COLUMN 1,'\033e\033(s218T\033(s12H\033(s7B','\033015'
      PRINT COLUMN 002,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,'\033015'

      SKIP 1 LINE

      PRINT COLUMN 002,"Tipo",
            COLUMN 007," Unificador ",
            COLUMN 022," Unificado ",
            COLUMN 034,"Resp.",
            COLUMN 041,"Ident.",
            COLUMN 048,"afo/ps",
            COLUMN 055,"edo",
            COLUMN 063,"Descipcion",
            COLUMN 085,"Afore recep.",
            COLUMN 102,"Diagnostico",'\033015'

      SKIP 1 LINE

      PRINT COLUMN 002,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,'\033015'

   #ON EVERY ROW
   AFTER GROUP OF reg_2.nss_uni
      SKIP 2 LINE

      SELECT descripcion
      INTO   x_descripcion
      FROM   uni_status
      WHERE  estado = reg_2.estado

      PRINT COLUMN 003,"02",
            COLUMN 008,reg_2.nss_uni,
            COLUMN 036,reg_2.status_convoca,
            COLUMN 043,reg_2.ident_movimiento,
            COLUMN 050,reg_2.cve_ent_nss,
            COLUMN 055,reg_2.estado USING "#&&",
            COLUMN 063,x_descripcion CLIPPED,
            COLUMN 090,reg_2.cve_afo_recep,'\033015'

      OPEN cur_3 USING reg_2.folio,
                       reg_2.nss_uni

      WHILE TRUE
         FETCH cur_3 INTO reg_3.*
            IF SQLCA.SQLCODE <> 0 THEN
               CLOSE cur_3
               EXIT WHILE
            END IF

         SKIP 1 LINE

         SELECT descripcion
         INTO   x_descripcion
         FROM   uni_status
         WHERE  estado = reg_3.estado

         SELECT descripcion
         INTO   x_descripcion_noti
         FROM   tab_confronta
         WHERE  diagnostico = reg_3.diag_unifica

         PRINT COLUMN 003,"03",
               COLUMN 022,reg_3.nss_cta1,
               COLUMN 036,reg_3.status_convoca,
               COLUMN 050,reg_3.cve_ent_cta1,
               COLUMN 055,reg_3.estado USING "#&&",
               COLUMN 063,x_descripcion CLIPPED,
               COLUMN 098,reg_3.diag_unifica,
               COLUMN 101,x_descripcion_noti CLIPPED,'\033015'

      END WHILE

   ON LAST ROW

      SKIP 2 LINE

      PRINT COLUMN 002,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,'\033015'

      SKIP 1 LINE

      PRINT COLUMN 1,'\033e\033(s218T\033(s14H\033(s7B','\033015'

      PRINT COLUMN 002,"UNIFICADORES",
            COLUMN 020,"CLAVE AFORE",
            COLUMN 037,"TOTAL",'\033015'

      DECLARE cur_4 CURSOR FOR
      SELECT COUNT(*),
             cve_ent_nss
      FROM   uni_unificador
      WHERE  folio = reg_2.folio
      GROUP BY 2

      LET i = 1
      LET x_subtotal_uni = 0

      FOREACH cur_4 INTO det_uni.*
         SKIP 1 LINE

         PRINT COLUMN 023,det_uni.clave USING "&&&", 
               COLUMN 039,det_uni.total USING "<<<<<",'\033015'

         LET x_subtotal_uni = x_subtotal_uni + det_uni.total

         LET i = i + 1
      END FOREACH

      PRINT COLUMN 018,L10,L10,L5,'\033015'

      PRINT COLUMN 002,"SUBTOTAL",
            COLUMN 039,x_subtotal_uni USING "<<<<<<",'\033015'

      SKIP 2 LINE

      PRINT COLUMN 002,"UNIFICADOS",
            COLUMN 020,"CLAVE AFORE",
            COLUMN 037,"TOTAL",'\033015'

      DECLARE cur_5 CURSOR FOR
      SELECT COUNT(*),
             cve_ent_cta1
      FROM   uni_unificado
      WHERE  folio = reg_2.folio
      GROUP BY 2

      LET i = 1
      LET x_subtotal_cta = 0

      FOREACH cur_5 INTO det_cta.*

         SKIP 1 LINE

         PRINT COLUMN 023,det_cta.clave USING "&&&", 
               COLUMN 039,det_cta.total USING "<<<<<",'\033015'

         LET x_subtotal_cta = x_subtotal_cta + det_cta.total

         LET i = i + 1
      END FOREACH

      PRINT COLUMN 018,L10,L10,L5,'\033015'

      PRINT COLUMN 002,"SUBTOTAL",
            COLUMN 039,x_subtotal_cta USING "<<<<<<",'\033015'

      SKIP 3 LINE

      LET x_total = x_subtotal_uni + x_subtotal_cta

      PRINT COLUMN 002,"TOTAL",
            COLUMN 039,x_total USING "<<<<<<",'\033015'

END REPORT
