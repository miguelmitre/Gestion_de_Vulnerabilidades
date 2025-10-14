###############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                            #
#Propietario       => EFP    			                              #
#Programa          => REPORTE DE CUENTAS UNIFICADAS                           #
#Fecha             => 16 DE AGOSTO DEL 2000                                   #
#Actualizado       => ARMANDO RODRIGUEZ CASTROPAREDES.                        #
#Fecha actualiza   => 13 de agosto de 2003.                                   #
#Actualizado por   => MIGUEL ANGEL HERNANDEZ MARTINEZ.                        #
###############################################################################
DATABASE safre_af
GLOBALS
        DEFINE enter char(001)
        DEFINE reg_2 RECORD #glo #reg_2
            folio            INTEGER,
            status_convoca   CHAR(1),
            nss_uni          CHAR(11),
            rfc_uni          CHAR(13),
            paterno_uni      CHAR(40),
            materno_uni      CHAR(40),
            nombre_uni       CHAR(40),
            ident_movimiento CHAR(2),
            num_ctas_asoc    SMALLINT,
            cve_ent_nss      CHAR(3),
            cve_afo_aclara   CHAR(3),
            resul_operacion  CHAR(2),
            fentcons         DATE,
            hora             CHAR(8),
            fliquida         DATE
        END RECORD
   
        DEFINE reg_3 RECORD
            folio              INTEGER,
            status_convoca     CHAR(1),
            nss_uni            CHAR(11),
            nss_cta1           CHAR(11),
            rfc_cta1           CHAR(13),
            cve_ent_cta1       CHAR(03),
            paterno_cta1       CHAR(40),
            materno_cta1       CHAR(40),
            nombre_cta1        CHAR(40),
            diag_unifica       CHAR(2),
            fentcons           DATE,
            sello_electronico  CHAR(24),
            fliquida           DATE
        END RECORD

        DEFINE det_uni ARRAY [500] OF RECORD
           total             INTEGER,
           clave             CHAR(03)
        END RECORD

        DEFINE det_cta ARRAY [500] OF RECORD
           total             INTEGER,
           clave             CHAR(03)
        END RECORD

        DEFINE #glo #integer
            cont              ,
            cont1             ,
            i                 ,
	         vfolio2           , 
	         vfolio            , 
            tot_registros     INTEGER

        DEFINE #glo #date
            HOY               DATE

        DEFINE #glo #char
	         G_LISTA	       	CHAR(500),
	         G_LISTA1	       	CHAR(500),
	         G_LISTA2	       	CHAR(100),
	         G_LISTA3	       	CHAR(100),
	         imprime        	CHAR(100),
	         borra          	CHAR(100),
	         cat            	CHAR(500),
	         aux_pausa       	CHAR(001),
	         char            	CHAR(001),
            bold              CHAR(032),
            raya              CHAR(120),
            cla_sel           CHAR(250),
            nss_unifica       CHAR(011),
            cve_ent_uni       CHAR(003),
            cve_ent_cta       CHAR(003),
            vcodigo_afore     CHAR(003),
            vrazon_social     CHAR(050)

	     DEFINE g_paramgrales   	RECORD LIKE seg_modulo.*

   DEFINE usuario    CHAR(8)

END GLOBALS
#####################################################################
MAIN
    OPTIONS INPUT WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT

    CALL STARTLOG("UNIC017.log")
    CALL inicio()  #i
    CALL proceso_principal()  #pp

END MAIN
#####################################################################
FUNCTION inicio()

    LET HOY = TODAY

    SELECT codigo_afore,
           razon_social
    INTO   vcodigo_afore,
           vrazon_social
    FROM   tab_afore_local

    LET bold  = '\033e\033(s218T\033(s12H\033(s7B'
    LET raya = '_____________________________________________',
               '_____________________________________________'

END FUNCTION
#####################################################################
FUNCTION proceso_principal()

   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "UNIC0161" ATTRIBUTE(BORDER)
   DISPLAY " UNIC017         REPORTE DE CUENTAS UNIFICADAS                               " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY " [ Esc ] Iniciar                                            [ Ctrl-C ] Salir " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

    INPUT BY NAME vfolio

      AFTER FIELD vfolio

         IF vfolio IS NULL THEN
            ERROR " EL FOLIO NO PUEDE SER NULO "
            NEXT FIELD vfolio
         END IF

       SELECT "X"
       FROM   uni_unificador
       WHERE  folio_liquida = vfolio
       GROUP BY 1

      IF STATUS = NOTFOUND THEN
        ERROR " NO EXISTE LIQUIDACION CON ESTE FOLIO "
        NEXT FIELD vfolio
      END IF

    ON KEY (ESC)

         IF vfolio IS NULL THEN
            ERROR " EL FOLIO NO PUEDE SER NULO "
            NEXT FIELD vfolio
         END IF

       SELECT "X"
       FROM   uni_unificador
       WHERE  folio_liquida = vfolio
       GROUP BY 1

      IF STATUS = NOTFOUND THEN
        ERROR " NO EXISTE LIQUIDACION CON ESTE FOLIO "
        NEXT FIELD vfolio
      END IF

    ERROR " PROCESANDO INFORMACION " 

      CALL genera_reporte() #gr

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
#####################################################################
FUNCTION pregunta()
    ERROR ""

    PROMPT " DESEA GENERAR EL ARCHIVO  S/N  ?  " for CHAR aux_pausa
END FUNCTION                                                         
#####################################################################
FUNCTION genera_reporte()

    SELECT USER,
           *
    INTO   usuario,
           g_paramgrales.*
    FROM   seg_modulo
    WHERE  modulo_cod = "uni"

    LET G_LISTA = g_paramgrales.ruta_listados CLIPPED,
                  "/" CLIPPED,usuario CLIPPED,".Operacion_22" CLIPPED

    START REPORT listado_2 TO G_LISTA

    DECLARE cur_6 CURSOR FOR

    SELECT a.nss_uni,
           a.cve_ent_nss,
           b.cve_ent_cta1
    FROM   uni_unificador a,uni_unificado b
    WHERE  a.folio_liquida = vfolio
    AND    a.folio_liquida = b.folio_liquida
    AND    a.nss_uni = b.nss_uni

    FOREACH cur_6 INTO nss_unifica,cve_ent_uni,cve_ent_cta 
           
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
               a.cve_afo_aclara,
               a.resul_operacion, #agrego
               " ", 
               " ", 
               a.fliquida
        FROM   uni_unificador a
        WHERE  a.folio_liquida = vfolio
        AND    a.nss_uni = nss_unifica

    FOREACH cur_2 INTO reg_2.*

            SELECT a.fentcons,
                   a.hora
            INTO   reg_2.fentcons,
                   reg_2.hora
            FROM   afi_mae_afiliado a
            WHERE  a.n_seguro = reg_2.nss_uni

        OUTPUT TO REPORT listado_2(reg_2.*) #l2
    END FOREACH

    END FOREACH

        FINISH REPORT listado_2

    #LET imprime = "vi ",G_LISTA
    LET imprime = "lp ",G_LISTA
    RUN imprime

END FUNCTION
#####################################################################
REPORT listado_2(reg_2)

   DEFINE reg_2 RECORD #glo #reg_2
          folio            INTEGER,
          status_convoca   CHAR(1),
          nss_uni          CHAR(11),
          rfc_uni          CHAR(13),
          paterno_uni      CHAR(40),
          materno_uni      CHAR(40),
          nombre_uni       CHAR(40),
          ident_movimiento CHAR(2),
          num_ctas_asoc    SMALLINT,
          cve_ent_nss      CHAR(3),
          cve_afo_aclara   CHAR(3),
          resul_operacion  CHAR(2),
          fentcons         DATE,
          hora             CHAR(8),
          fliquida         DATE
   END RECORD

   DEFINE reg_3 RECORD
          folio            INTEGER,
          status_convoca   CHAR(1),
          nss_uni          CHAR(11),
          nss_cta1         CHAR(11),
          rfc_cta1         CHAR(13),
          cve_ent_cta1     CHAR(03),
          paterno_cta1     CHAR(40),
          materno_cta1     CHAR(40),
          nombre_cta1      CHAR(40),
          diag_unifica     CHAR(2),
          fentcons         DATE, 
          sello_electronico CHAR(24),
          fliquida          DATE
   END RECORD

   DEFINE det_uni RECORD
          total            INTEGER,
          clave            CHAR(03)
   END RECORD

   DEFINE det_cta RECORD
          total             INTEGER,
          clave             CHAR(03)
   END RECORD

   DEFINE x_subtotal_uni      INTEGER,
          x_subtotal_cta      INTEGER,
          x_total             INTEGER

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

   OUTPUT
      PAGE LENGTH 60
      LEFT MARGIN 0
      RIGHT MARGIN 0
      TOP MARGIN 0
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

      PRINT COLUMN 004,"UNIC017",
            COLUMN 040,"REPORTE DE CUENTAS UNIFICADAS",
            COLUMN 100, "P\240gina  ",pageno USING "##",'\033015'

      SKIP 1 LINE

      PRINT COLUMN 004,"Folio liquidacion : ",vfolio USING "#####",'\033015'

      PRINT COLUMN 1,'\033e\033(s218T\033(s12H\033(s7B','\033015'

      PRINT COLUMN 002,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,'\033015'

      SKIP 1 LINE

      PRINT COLUMN 002,"Tipo",
            COLUMN 007," Unificador ",
            COLUMN 022," Unificado ",
            COLUMN 034,"Resp.",
            COLUMN 041,"Ident.",
            COLUMN 048,"afore/ps",
            COLUMN 058,"F. certifica",
            COLUMN 076,"Folio",
            COLUMN 085,"F. liquidacion",'\033015'

      SKIP 1 LINE

      PRINT COLUMN 002,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,L10,'\033015'

   #ON EVERY ROW
   AFTER GROUP OF reg_2.nss_uni

      SKIP 2 LINE

      PRINT COLUMN 003,"02",
            COLUMN 008,reg_2.nss_uni,
            COLUMN 036,reg_2.status_convoca,
            COLUMN 043,reg_2.ident_movimiento,
            COLUMN 050,reg_2.cve_ent_nss,
            COLUMN 060,reg_2.fentcons USING"DD/MM/YYYY",
            COLUMN 078,reg_2.folio USING "<<<<",
            COLUMN 088,reg_2.fliquida USING "DD/MM/YYYY",'\033015'


{
    LET cla_sel = "SELECT a.folio, ",
                         "a.status_convoca,",
                         "a.nss_uni, ",
                         "a.nss_cta1, ",
                         "a.rfc_cta1, ",
                         "a.cve_ent_cta1, ",
                         "a.paterno_cta1, ",
                         "a.materno_cta1, ",
                         "a.nombre_cta1, ",
                         "a.diag_unifica, ",
                         "' ',  ",
                         "' ',  ",
                         "a.fliquida ",
                  "FROM   uni_unificado a ",
                  "WHERE  a.folio_liquida = ? ",
                  "AND    a.nss_uni = ? "

    PREPARE claexe FROM cla_sel
    DECLARE cur_3 CURSOR FOR claexe

      OPEN cur_3 USING reg_2.folio,reg_2.nss_uni

      WHILE TRUE
         FETCH cur_3 INTO reg_3.*
            IF SQLCA.SQLCODE <> 0 THEN
               CLOSE cur_3
               EXIT WHILE
            END IF
}
    DECLARE cur_3 CURSOR FOR 
    SELECT a.folio,
           a.status_convoca,
           a.nss_uni,
           a.nss_cta1,
           a.rfc_cta1,
           a.cve_ent_cta1,
           a.paterno_cta1,
           a.materno_cta1,
           a.nombre_cta1,
           a.diag_unifica,
           " ",
           " ",
           a.fliquida
    FROM   uni_unificado a
    WHERE  a.folio_liquida = vfolio
    AND    a.nss_uni = reg_2.nss_uni

    FOREACH cur_3 INTO reg_3.*

            SELECT fentcons,
                   sello_electronico 
            INTO   reg_3.fentcons,
                   reg_3.sello_electronico
            FROM   afi_mae_afiliado
            WHERE  n_seguro = reg_3.nss_cta1


         SKIP 1 LINE

         PRINT COLUMN 003,"03",
               COLUMN 022,reg_3.nss_cta1,
               COLUMN 036,reg_3.status_convoca,
               COLUMN 050,reg_3.cve_ent_cta1,
               COLUMN 060,reg_3.fentcons USING"DD/MM/YYYY",
               COLUMN 078,reg_3.folio USING "<<<<",
               COLUMN 088,reg_3.fliquida USING "DD/MM/YYYY",'\033015'

     END FOREACH

--      END WHILE

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
      WHERE  folio_liquida = vfolio
      GROUP BY 2

      LET i = 1
      LET x_subtotal_uni = 0

      FOREACH cur_4 INTO det_uni.*

         SKIP 1 LINE

         PRINT COLUMN 023,det_uni.clave USING "<<<", 
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
      WHERE  folio_liquida = vfolio
      GROUP BY 2

      LET i = 1
      LET x_subtotal_cta = 0

      FOREACH cur_5 INTO det_cta.*

         SKIP 1 LINE

         PRINT COLUMN 023,det_cta.clave USING "<<<", 
               COLUMN 039,det_cta.total USING "<<<<<",'\033015'

         LET x_subtotal_cta = x_subtotal_cta + det_cta.total

         LET i = i + 1
      END FOREACH

      PRINT COLUMN 018,L10,L10,L5

      PRINT COLUMN 002,"SUBTOTAL",
            COLUMN 039,x_subtotal_cta USING "<<<<<<",'\033015'

      SKIP 3 LINE

      LET x_total = x_subtotal_uni + x_subtotal_cta

      PRINT COLUMN 002,"TOTAL",
            COLUMN 039,x_total USING "<<<<<<",'\033015'

END REPORT
