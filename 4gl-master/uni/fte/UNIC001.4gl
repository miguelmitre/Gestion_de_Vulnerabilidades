################################################################################
#Owner             => E.F.P                                                    #
#Programa UNIC001  => RECIBE ARCHIVOS DE UNIFICACION DE CUENTAS                #
#Fecha creacion    => 27 DE SEPTIEMBRE DE 1999                                 #
#By                => ARMANDO RODRIGUEZ CASTROPAREDES                          #
#Fecha actualiza   => 24 febrero 2003                                          #
#Sistema           => UNI                                                      #
#Modificado por    => OMAR SANDOVAL BADILLO                                    #
#Fecha modificacion=> 11 noviembre 2004                                        #
#Fecha modificacion=> 09 marzo del 2005                                        #
#verifica archivo previo a realizar la carga                                   #
################################################################################
# fecha Actuali   => CPL-1867 04/02/2015                                       #
# Autor           => Cristian  Morales Roblero                                 #
# Actualizacion   => se agrega tabla para el almacenaje de los nss que no se   #
#                    marcaron en UNIC009 y se emite reporte en UNIC001 con     #
#                    con los nss que no fueron marcados                        #
################################################################################

DATABASE safre_af
GLOBALS
    DEFINE
        g_param_uni           RECORD LIKE seg_modulo.*

    DEFINE reg_cza_notif  RECORD #reg_cza_notif
        tipo_registro         CHAR(02),
        ident_servicio        CHAR(02),
        ident_operacion       CHAR(02),
        tipo_ent_origen       CHAR(02),
        cve_ent_origen        CHAR(03),
        tipo_ent_destino      CHAR(02),
        cve_ent_destino       CHAR(03),
        fecha_presenta        DATE,
        consec_lote           SMALLINT,
        resulta_oper          CHAR(02),
        motivo_rechazo_1      CHAR(03)
    END RECORD

    DEFINE reg_det_unifica RECORD
        tipo_registro         CHAR(02),
        cont_servicio         INTEGER,
        tipo_ent_uni          CHAR(02),
        cve_ent_uni           CHAR(03),
        tipo_ent_nss          CHAR(02),
        cve_ent_nss           CHAR(03),
        curp_uni              CHAR(18),
        nss_uni               CHAR(11),
        rfc_uni               CHAR(13),
        paterno_uni           CHAR(40),
        materno_uni           CHAR(40),
        nombre_uni            CHAR(40),
        nombre_imss_uni       CHAR(50),
        sexo_uni              CHAR(01),
        ent_nac_uni           CHAR(02),
        fecha_nac_uni         DATE,
        tipo_documento        CHAR(01),
        cve_afo_recep         CHAR(03),
        --cve_afo_recep         SMALLINT,
        num_ctas_asoc         SMALLINT,
        status_convoca        CHAR(01),
        resul_operacion       CHAR(02),
        diag_proceso1         CHAR(03),
        diag_proceso2         CHAR(03),
        diag_proceso3         CHAR(03),
        diag_proceso4         CHAR(03),
        diag_proceso5         CHAR(03),
        ident_movimiento      CHAR(02),
        status_tra_nss        CHAR(02),
        status_ret_nss        CHAR(02),
        cve_afo_aclara        CHAR(03),
        --cve_afo_aclara       SMALLINT,
	id_credito_43         CHAR(01)
    END RECORD

    DEFINE reg_det_cta_uni RECORD
        tipo_registro         CHAR(02),
        cont_servicio         INTEGER,
        nss_uni               CHAR(11),
        tipo_ent_cta1         CHAR(02),
        cve_ent_cta1          CHAR(03),
        curp_cta1             CHAR(18),
        nss_cta1              CHAR(11),
        rfc_cta1              CHAR(13),
        paterno_cta1          CHAR(40),
        materno_cta1          CHAR(40),
        nombre_cta1           CHAR(40),
        nombre_imss_cta1      CHAR(50),
        sexo_cta1             CHAR(01),
        ent_nac_cta1          CHAR(02),
        fecha_nac_cta1        DATE,
        status_tra_cta1       CHAR(02),
        status_ret_cta1       CHAR(02),
        status_convoca        CHAR(01),
        nss_cta2              CHAR(11),
        diag_unifica          CHAR(02),
        resulta_operacion     CHAR(02),
        diag_proceso1         CHAR(03),
        diag_proceso2         CHAR(03),
        diag_proceso3         CHAR(03),
        diag_proceso4         CHAR(03),
        diag_proceso5         CHAR(03),
        cod_operacion         CHAR(02),
        diag_proceso          CHAR(03),
        cve_afo_aclara        CHAR(03),
        --cve_afo_aclara       SMALLINT,
	      id_credito_43_cta1    CHAR(01)
    END RECORD

    DEFINE reg_sum_notif RECORD #reg_cza_pe_05
        tipo_registro         CHAR(02),
        total_detalle         INTEGER,
        total_nss_uni         INTEGER,
        total_cta_uni         INTEGER
    END RECORD

    DEFINE
        HOY                   DATE

    DEFINE #char
        carga_reg             CHAR(330),
        usuario               CHAR(008),
        enter    	      CHAR(001),
        motivo_rechazo_1      CHAR(003),
        motivo_rechazo_2      CHAR(003),
        motivo_rechazo_3      CHAR(003),
        resulta_oper          CHAR(002),
        generar               CHAR(020),
        nombre_archivo        CHAR(020),
        archivo_unifica       CHAR(200),
        G_LISTA               CHAR(200),
        G_LISTA1              CHAR(200),
        c10_fecha_presenta    CHAR(010),
        c10_fecha_nac_uni     CHAR(010),
        c10_fecha_nac_cta1    CHAR(010)

    DEFINE #smallint
        s_recibido,
        cont_det  ,
        codigo    ,
        cuantos   ,
        cont                  INTEGER

    DEFINE #integer
        ultimo_folio          INTEGER

    DEFINE vruta_exp          CHAR(50),
           COMMA              CHAR(50),
           disp1              ,
           disp2              ,
           disp21             ,
           disp3              ,
           disp4              ,
           disp41             INTEGER

    DEFINE  gi_intra_responsable        INTEGER
    DEFINE  gi_extra_responsable        INTEGER
    DEFINE  gi_intra_noresponsable      INTEGER
    DEFINE  gi_extra_noresponsable      INTEGER
    DEFINE  gi_intra_fallecido          INTEGER
    DEFINE  gi_extra_fallecido          INTEGER
    DEFINE  gi_intra_inverso            INTEGER
    DEFINE  gi_extra_inverso            INTEGER
END GLOBALS
######################################################
MAIN
   OPTIONS
      PROMPT LINE LAST
      DEFER INTERRUPT

   CALL STARTLOG("UNIC001.log")

   SELECT ruta_fte
   INTO   vruta_exp
   FROM   seg_modulo
   WHERE modulo_cod = "uni"

   WHENEVER ERROR CONTINUE

      DROP TABLE tmp_pla_unifica
      CREATE TEMP TABLE tmp_pla_unifica
        (
         n_registros          CHAR(330)
        )

   WHENEVER ERROR STOP

   CALL init()
   OPEN WINDOW unic0011 AT 2,2 WITH FORM "UNIC0011" ATTRIBUTE(BORDER)
   DISPLAY "       [Esc]  Iniciar                                    [Ctrl-C]  Salir       " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " UNIC001         RECIBE ARCHIVOS DE UNIFICACION DE CUENTAS                                  " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

   INPUT BY NAME generar WITHOUT DEFAULTS
      BEFORE FIELD generar
         LET generar = NULL
         CLEAR FORM

      AFTER FIELD generar
         IF generar IS NULL THEN
	    ERROR "Campo NO puede ser NULO"
	    NEXT FIELD generar
	 END IF

         SELECT nombre
         INTO   nombre_archivo
         FROM   uni_ctr_archivo
         WHERE  nombre = generar

         IF nombre_archivo IS NOT NULL THEN
            ERROR " Este archivo ya se recibio "
	    SLEEP 2
	    ERROR ""
	    NEXT FIELD generar
         END IF

         WHENEVER ERROR CONTINUE
            SELECT *
            INTO   g_param_uni.*
            FROM   seg_modulo
	    WHERE  modulo_cod = "uni"

            LET archivo_unifica = g_param_uni.ruta_rescate CLIPPED,"/",
                                  generar CLIPPED

            LOAD FROM archivo_unifica DELIMITER "+"
            INSERT INTO tmp_pla_unifica

            SELECT count(*)
            INTO   cuantos
            FROM   tmp_pla_unifica

            IF cuantos = 0 THEN
               DISPLAY " NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO " AT 19,2 ATTRIBUTE(REVERSE)
               SLEEP 2
	       DISPLAY "                                              " AT 19,2
               NEXT FIELD generar
            ELSE
               EXIT INPUT
            END IF

         WHENEVER ERROR STOP

	 ON KEY (INTERRUPT)
            ERROR " PROCESO CANCELADO "
            SLEEP 2
            EXIT PROGRAM
            END INPUT

         ERROR " VALIDANDO ARCHIVO DE PROCESAR "
         SLEEP 2

         CALL valida_archivo()  #va

         ERROR " "
         ERROR " VALIDACION CONCLUIDA "
         SLEEP 2
         ERROR " PROCESANDO INFORMACION "
         SLEEP 2

         SELECT MAX(folio) + 1
         INTO   ultimo_folio
         FROM   uni_folio_trabajo

         IF ultimo_folio IS NULL THEN
            LET ultimo_folio = 1
         END IF

         INSERT INTO uni_folio_trabajo
            VALUES(ultimo_folio)

         DISPLAY "FOLIO NUMERO         : ",ultimo_folio  AT 6,9       ---JGHM 01Dic09 AT 11,9

         CALL lee_archivo_plano()

         #### JGHM 01 Dic 09 Display de nuevos contadores
         CALL f_indicadores()

         DISPLAY  "                           INTRA           EXTRA         "                                                     AT 10, 9 ATTRIBUTE(REVERSE)
         DISPLAY  "RESPONSABLE            .. ", gi_intra_responsable   USING "#,##&", "           ", gi_extra_responsable   USING "#,##&" AT 11, 9
         DISPLAY  "FALLECIDO              .. ", gi_intra_fallecido     USING "#,##&", "           ", gi_extra_fallecido     USING "#,##&" AT 12, 9
         DISPLAY  "INVERSO                .. ", gi_intra_inverso       USING "#,##&", "           ", gi_extra_inverso       USING "#,##&" AT 13, 9

         ERROR " PROCESO DE MARCAJE "
         SLEEP 2

         CALL UNIC009(ultimo_folio)
         RETURNING disp1,
                   disp2,
                   disp21,
                   disp3,
                   disp4,
                   disp41

         #### JGHM 01 Dic09  DISPLAY "                   REALIZA MARCAJE DE LA CUENTA INDIVIDUAL                     " AT 15,1 ATTRIBUTE(REVERSE)
         #### JGHM 01 Dic09  DISPLAY "                            SUJETA A UNIFICACION                               " AT 16,1 ATTRIBUTE(REVERSE)
         ERROR "" #CPL-1867
         DISPLAY "         REALIZA MARCAJE DE LA CUENTA INDIVIDUAL SUJETA A UNIFICACION            " AT 14,1 ATTRIBUTE(REVERSE)
         #### JGHM 01 Dic09 DISPLAY "NSS UNIFICADORES  ",disp1 AT 16,9
         #### JGHM 01 Dic09 DISPLAY "NSS UNIFICADORES MARCADOS  ",disp21 CLIPPED,
         #### JGHM 01 Dic09                      ", NO MARCADOS  ",disp2 AT 17,9
         #### JGHM 01 Dic09 DISPLAY "NSS UNIFICADOS    ",disp3 AT 18,9
         #### JGHM 01 Dic09 DISPLAY "NSS UNIFICADOS   MARCADOS  ",disp41 CLIPPED,
         #### JGHM 01 Dic09                      ", NO MARCADOS  ",disp4 AT 19,9

         DISPLAY "NSS UNIFICADORES : ",disp1 CLIPPED,
                 " MARCADOS .. ",     disp21   CLIPPED,
                 " NO MARCADOS .. ",  disp2    CLIPPED AT 16,9
         DISPLAY "NSS UNIFICADOS   : ",disp3 CLIPPED,
                 " MARCADOS .. ",     disp41   CLIPPED,
                 " NO MARCADOS .. ",  disp4    CLIPPED AT 17,9
         #CPL-1867 INICIO
         SELECT 'X'
         FROM tmp_uni_nss_sinmarca
         GROUP BY 1

         IF SQLCA.SQLCODE = 0 THEN
            CALL f_rep_nss()
            DISPLAY "ARCHIVO GENERADO ",G_LISTA CLIPPED AT 19,2
         END IF
         #CPL-1867 FIN
         PROMPT " PROCESO FINALIZADO. PRESIONE <ENTER> PARA SALIR "
         FOR CHAR enter

         CLOSE WINDOW unic0011
END MAIN
######################################################
FUNCTION init()
   LET HOY  = TODAY

   SELECT estado ,
          USER
   INTO   s_recibido ,
          usuario
   FROM   uni_status
   WHERE  descripcion = "RECIBIDO"

   SELECT codigo_afore
   INTO   codigo
   FROM   tab_afore_local

   LET    gi_intra_responsable   = 0
   LET    gi_extra_responsable   = 0
   LET    gi_intra_noresponsable = 0
   LET    gi_extra_noresponsable = 0
   LET    gi_intra_fallecido     = 0
   LET    gi_extra_fallecido     = 0
   LET    gi_intra_inverso       = 0
   LET    gi_extra_inverso       = 0
END FUNCTION
######################################################
FUNCTION lee_archivo_plano()

   DECLARE cur_1 CURSOR FOR
   SELECT  *
   FROM    tmp_pla_unifica

   LET cont            = 0
   LET cont_det        = 0

   FOREACH cur_1 INTO carga_reg
      DISPLAY " TOTAL REGISTROS UNIFICADORES   : ",cont     AT 7,8      ---- JGHM 1Dic09 AT 9,8
      DISPLAY " TOTAL REGISTROS UNIFICADOS     : ",cont_det AT 8,8      ---- JGHM 1Dic09 AT 10,8

            #---Encabezado por lote de notificaciones---#

      IF carga_reg[1,2] = "01" THEN
         CALL carga_cza() #cc
      END IF

            #---Detalle de nss unificadores---#

      IF carga_reg[1,2] = "02" THEN
         LET cont = cont + 1
         CALL carga_det_nss()  #cdn
      END IF

            #---Detalle de dis_cuenta unificadas---#

      IF carga_reg[1,2] = "03" THEN
         LET cont_det = cont_det + 1
         CALL carga_det_uni()  #cdu
      END IF
            #---Sumario de notificacion---#

      IF carga_reg[1,2] = "09" THEN
         LET reg_sum_notif.tipo_registro     = carga_reg[001,002]
         LET reg_sum_notif.total_detalle     = carga_reg[003,011]
         LET reg_sum_notif.total_nss_uni     = carga_reg[012,020]
         LET reg_sum_notif.total_cta_uni     = carga_reg[021,029]

         INSERT INTO uni_sum_notifica
            VALUES(ultimo_folio    ,
                   reg_sum_notif.* ,
                   s_recibido)
      ELSE
      END IF
   END FOREACH

   ### 1 Dic 2009    JGHM  y Miguel Hernández
   ### Se modifica el siguiente codigo no es correcto, glo_folio es tabla general de SAFRE
   ### solamante para registros de transacciones que utilizan el mismo folio .. que se asignan
   ### tanto al archivo como a los movs de liquidacion

   ###SELECT "OK"
   ###FROM   glo_folio
   ###WHERE  folio = ultimo_folio

   ###IF STATUS = NOTFOUND THEN
   ###   INSERT INTO glo_folio VALUES(ultimo_folio)
   ###END IF
END FUNCTION
######################################################
FUNCTION carga_cza()

   LET reg_cza_notif.tipo_registro      = carga_reg[001,002]
   LET reg_cza_notif.ident_servicio     = carga_reg[003,004]
   LET reg_cza_notif.ident_operacion    = carga_reg[005,006]
   LET reg_cza_notif.tipo_ent_origen    = carga_reg[007,008]
   LET reg_cza_notif.cve_ent_origen     = carga_reg[009,011]
   LET reg_cza_notif.tipo_ent_destino   = carga_reg[012,013]
   LET reg_cza_notif.cve_ent_destino    = carga_reg[014,016]
   LET c10_fecha_presenta               = carga_reg[021,022],"/",
                                          carga_reg[023,024],"/",
                                          carga_reg[017,020]
   LET reg_cza_notif.fecha_presenta     = c10_fecha_presenta
   LET reg_cza_notif.consec_lote        = carga_reg[025,027]
   LET reg_cza_notif.resulta_oper       = carga_reg[028,029]
   LET reg_cza_notif.motivo_rechazo_1   = carga_reg[030,032]

   INSERT INTO uni_cza_notifica
      VALUES(ultimo_folio,
             reg_cza_notif.*,
             s_recibido,
             HOY)

   INSERT INTO uni_ctr_archivo
      VALUES(generar,
             ultimo_folio,
             reg_cza_notif.fecha_presenta,
             reg_cza_notif.ident_operacion)

END FUNCTION
######################################################
FUNCTION carga_det_nss()
   DEFINE ls_afore SMALLINT

   LET reg_det_unifica.tipo_registro      = carga_reg[001,002]
   LET reg_det_unifica.cont_servicio      = carga_reg[003,012]
   LET reg_det_unifica.tipo_ent_uni       = carga_reg[013,014]
   LET reg_det_unifica.cve_ent_uni        = carga_reg[015,017]
   LET reg_det_unifica.tipo_ent_nss       = carga_reg[018,019]
   LET reg_det_unifica.cve_ent_nss        = carga_reg[020,022]
   LET reg_det_unifica.curp_uni           = carga_reg[023,040]
   LET reg_det_unifica.nss_uni            = carga_reg[041,051]
   LET reg_det_unifica.rfc_uni            = carga_reg[052,064]
   LET reg_det_unifica.paterno_uni        = carga_reg[065,104]
   LET reg_det_unifica.materno_uni        = carga_reg[105,144]
   LET reg_det_unifica.nombre_uni         = carga_reg[145,184]
   LET reg_det_unifica.nombre_imss_uni    = carga_reg[185,234]
   LET reg_det_unifica.sexo_uni           = carga_reg[235,235]
   LET reg_det_unifica.ent_nac_uni        = carga_reg[236,237]
   LET c10_fecha_nac_uni                  = carga_reg[242,243],"/",
                                            carga_reg[244,245],"/",
                                            carga_reg[238,241]
   LET reg_det_unifica.fecha_nac_uni      = c10_fecha_nac_uni
   LET reg_det_unifica.tipo_documento     = carga_reg[246,246]
   LET reg_det_unifica.cve_afo_recep      = carga_reg[247,249]
   LET reg_det_unifica.num_ctas_asoc      = carga_reg[250,251]
   LET reg_det_unifica.status_convoca     = carga_reg[268,268]
   LET reg_det_unifica.resul_operacion    = carga_reg[269,270]
   LET reg_det_unifica.diag_proceso1      = carga_reg[271,273]
   LET reg_det_unifica.diag_proceso2      = carga_reg[274,276]
   LET reg_det_unifica.diag_proceso3      = carga_reg[277,279]
   LET reg_det_unifica.diag_proceso4      = carga_reg[280,282]
   LET reg_det_unifica.diag_proceso5      = carga_reg[283,285]
   LET reg_det_unifica.ident_movimiento   = carga_reg[286,287]
   LET reg_det_unifica.status_tra_nss     = carga_reg[288,289]
   LET reg_det_unifica.status_ret_nss     = carga_reg[290,291]
   LET reg_det_unifica.cve_afo_aclara     = carga_reg[292,294]
   LET reg_det_unifica.id_credito_43      = carga_reg[295,295]

   #CPL-1738
   LET ls_afore                       = NULL
   LET ls_afore                       = reg_det_unifica.cve_afo_recep
   LET reg_det_unifica.cve_afo_recep  = valida_afore_fusion(ls_afore)

   LET ls_afore                       = NULL
   LET ls_afore                       = reg_det_unifica.cve_afo_aclara
   LET reg_det_unifica.cve_afo_aclara = valida_afore_fusion(ls_afore)

   LET reg_det_unifica.cve_afo_aclara = valida_afore_fusion(reg_det_unifica.cve_afo_aclara)

   IF reg_det_unifica.cve_afo_recep = '000' THEN
      LET reg_det_unifica.cve_afo_recep = codigo USING "&&&"
   END IF

   INSERT INTO uni_unificador
      VALUES(ultimo_folio,
             " ",
             reg_det_unifica.*,
             s_recibido,
             0,
             1,
	     " ",
	     " ")
END FUNCTION
######################################################
FUNCTION carga_det_uni()
   DEFINE ls_afore SMALLINT

   LET reg_det_cta_uni.tipo_registro      = carga_reg[001,002]
   LET reg_det_cta_uni.cont_servicio      = carga_reg[003,012]
   LET reg_det_cta_uni.nss_uni            = carga_reg[013,023]
   LET reg_det_cta_uni.tipo_ent_cta1      = carga_reg[024,025]
   LET reg_det_cta_uni.cve_ent_cta1       = carga_reg[026,028]
   LET reg_det_cta_uni.curp_cta1          = carga_reg[029,046]
   LET reg_det_cta_uni.nss_cta1           = carga_reg[047,057]
   LET reg_det_cta_uni.rfc_cta1           = carga_reg[058,070]
   LET reg_det_cta_uni.paterno_cta1       = carga_reg[071,110]
   LET reg_det_cta_uni.materno_cta1       = carga_reg[111,150]
   LET reg_det_cta_uni.nombre_cta1        = carga_reg[151,190]
   LET reg_det_cta_uni.nombre_imss_cta1   = carga_reg[191,240]
   LET reg_det_cta_uni.sexo_cta1          = carga_reg[241,241]
   LET reg_det_cta_uni.ent_nac_cta1       = carga_reg[242,243]
   LET c10_fecha_nac_cta1                 = carga_reg[248,249],"/",
                                            carga_reg[250,251],"/",
                                            carga_reg[244,247]
   LET reg_det_cta_uni.fecha_nac_cta1     = c10_fecha_nac_cta1
   LET reg_det_cta_uni.status_tra_cta1    = carga_reg[252,253]
   LET reg_det_cta_uni.status_ret_cta1    = carga_reg[254,255]
   LET reg_det_cta_uni.status_convoca     = carga_reg[256,256]
   LET reg_det_cta_uni.nss_cta2           = carga_reg[257,267]
   LET reg_det_cta_uni.diag_unifica       = carga_reg[268,269]
   LET reg_det_cta_uni.resulta_operacion  = carga_reg[270,271]
   LET reg_det_cta_uni.diag_proceso1      = carga_reg[272,274]
   LET reg_det_cta_uni.diag_proceso2      = carga_reg[275,277]
   LET reg_det_cta_uni.diag_proceso3      = carga_reg[278,280]
   LET reg_det_cta_uni.diag_proceso4      = carga_reg[281,283]
   LET reg_det_cta_uni.diag_proceso5      = carga_reg[284,286]
   LET reg_det_cta_uni.cve_afo_aclara     = carga_reg[287,289]
   LET reg_det_cta_uni.id_credito_43_cta1 = carga_reg[290,290]

   #CPL-1738
   LET ls_afore                       = NULL
   LET ls_afore                       = reg_det_cta_uni.cve_afo_aclara
   LET reg_det_cta_uni.cve_afo_aclara = valida_afore_fusion(ls_afore)

   INSERT INTO uni_unificado
      VALUES(ultimo_folio    ,
             " ",
             reg_det_cta_uni.* ,
             s_recibido,
             0,
             1,
	     " ",
	     " ")
END FUNCTION
######################################################
FUNCTION valida_archivo()

   DEFINE valida       CHAR(330),
          xnss         CHAR(11),
          xcta1        CHAR(11),
          nss_uni      CHAR(11),
          nss_cta1     CHAR(11),
          afore        CHAR(03),
          diagnostico  CHAR(02),
          xafore       CHAR(03),
          xdiag        CHAR(02),
          vtipo        CHAR(02),
          aux_pausa    CHAR(01),
          valida_txt   CHAR(100),
          xfolio       INTEGER,
          falla        INTEGER,
          vreciclado   INTEGER,
          cont         INTEGER,
          cont1        INTEGER

   DEFINE val RECORD
        nss            CHAR(11),
        tipo           CHAR(02),
        afore          CHAR(03),
        identificador  CHAR(02),
        convoca        CHAR(01)
   END RECORD

   DEFINE val2 RECORD
        nss_uni        CHAR(11),
        nss_cta1       CHAR(11),
        tipo_cta1      CHAR(02),
        afore_cta1     CHAR(03),
        convoca_cta1   CHAR(01)
   END RECORD

   DEFINE val3 RECORD
        nss_uni        CHAR(11),
        nss_cta1       CHAR(11),
        tipo_cta1      CHAR(02),
        afore_cta1     CHAR(03),
        convoca_cta1   CHAR(01)
   END RECORD

   CREATE TEMP TABLE  uni_verifica ( nss  CHAR(11),
                                     tipo CHAR(02),
                                     afore CHAR(3),
                                     identificador CHAR(2),
                                     convoca CHAR(1))
   CREATE TEMP TABLE  cta_verifica ( nss_uni CHAR(11),
                                     nss_cta1 CHAR(11),
                                     tipo_cta1 CHAR(02),
                                     afore_cta1 CHAR(3),
                                     convoca_cta1 CHAR(1))
   CREATE TEMP TABLE falla_verifica( nss  CHAR(11),
                                     tipo CHAR(02),
                                     afore CHAR(3),
                                     identificador CHAR(2),
                                     convoca CHAR(1),
                                     nss_uni CHAR(11),
                                     nss_cta1 CHAR(11),
                                     tipo_cta1 CHAR(02),
                                     afore_cta1 CHAR(3),
                                     convoca_cta1 CHAR(1),
                                     falla  SMALLINT)

   LET vtipo = "01"

   DECLARE val_1 CURSOR FOR
      SELECT  *
      FROM    tmp_pla_unifica
   FOREACH val_1 INTO valida

      CASE valida[1,2]
         WHEN "01"
	    IF valida[5,6] <> "21" THEN
	       ERROR "El archivo no corresponde a la operacion 21"
            END IF
	 WHEN "02"
            LET val.nss             = valida[041,051]
            LET val.tipo            = valida[018,019]
            LET val.afore           = valida[020,022]
            LET val.identificador   = valida[286,287]
            LET val.convoca         = valida[268,268]
            INSERT INTO uni_verifica VALUES(val.*)
	    INITIALIZE val.* TO NULL
	 WHEN "03"
            LET val2.nss_uni         = valida[013,023]
            LET val2.nss_cta1        = valida[047,057]
            LET val2.tipo_cta1       = valida[024,025]
            LET val2.afore_cta1      = valida[026,028]
            LET val2.convoca_cta1    = valida[256,256]
            INSERT INTO cta_verifica VALUES(val2.*)
	    INITIALIZE val2.* TO NULL
      END CASE
    END FOREACH

    LET xfolio = 0
    LET cont = 0
    LET cont1 = 0
    INITIALIZE val.* TO NULL
    INITIALIZE val2.* TO NULL

--------# convoca unificador

    DECLARE val_2 CURSOR FOR
       SELECT *
       FROM   uni_verifica a
       WHERE  a.afore = codigo
       AND    a.convoca = '1'
       UNION ALL
       #CPL-1738
       SELECT *
       FROM   uni_verifica a
       WHERE  a.afore   = '000'
       AND    a.convoca = '1'

       --ORDER BY a.nss

    FOREACH val_2 INTO val.*
--------# valida intra
	IF val.identificador = "01" THEN
           SELECT "X"
           FROM   uni_verifica s
           WHERE  s.nss      = val.nss
	   AND    s.afore    = codigo
	   AND    s.tipo     = vtipo
	   AND    s.identificador = "01"
	   GROUP BY 1

	   IF STATUS = NOTFOUND THEN
	      DECLARE val_3 CURSOR FOR
	         SELECT *
	         FROM   cta_verifica r
	         WHERE  r.nss_uni  = val.nss
	      FOREACH val_3 INTO val2.*
		 LET falla = 1
		 INSERT INTO falla_verifica VALUES(val.*,val2.*,falla)
                 INITIALIZE val2.* TO NULL
              END FOREACH
	   END IF

	   SELECT "X"
	   FROM   cta_verifica s
	   WHERE  s.nss_uni    = val.nss
	   AND    s.afore_cta1 = val.afore
	   AND    s.tipo_cta1  = val.tipo
	   GROUP BY 1

           IF STATUS = NOTFOUND THEN
	      DECLARE val_4 CURSOR FOR
	      SELECT *
	      FROM   cta_verifica u
	      WHERE  u.nss_uni  = val.nss

	      FOREACH val_4 INTO val2.*
	         LET falla = 2
		 INSERT INTO falla_verifica VALUES(val.*,val2.*,falla)
                 INITIALIZE val2.* TO NULL
	      END FOREACH
           END IF
        END IF

--------# valida extra si cuenta con algun rechazo definitivo
	IF val.identificador = "02" THEN
	   DECLARE val_5 CURSOR FOR
	   SELECT *
	   FROM   cta_verifica v
	   WHERE  v.nss_uni  = val.nss

	   FOREACH val_5 INTO val2.*
		SELECT "X"
		FROM   uni_unificado x
		WHERE  x.nss_uni  = val2.nss_uni
		AND    x.nss_cta1 = val2.nss_cta1
		AND    x.diag_unifica = "02"
		GROUP BY 1

	        IF STATUS <> NOTFOUND THEN
		    LET falla = 3
		    --INSERT INTO falla_verifica VALUES(val.*,val2.*,falla)
                    INITIALIZE val2.* TO NULL
		END IF
	    END FOREACH

--------# valida extra si se reciclo
            SELECT "X"
	    FROM   uni_recicla a
	    WHERE  a.nss_uni = val.nss
	    GROUP BY 1
	    IF STATUS <> NOTFOUND THEN
		DELETE
		FROM   uni_recicla
		WHERE  uni_recicla.nss_uni = val.nss
	    END IF
        END IF
    END FOREACH

    INITIALIZE val.* TO NULL
    INITIALIZE val2.* TO NULL

--------# convoca unificado
    DECLARE val_7 CURSOR FOR
        SELECT *
        FROM   uni_verifica b
	WHERE  b.convoca    = "0"
        --ORDER BY nss
    FOREACH val_7 INTO val.*
        DECLARE val_8 CURSOR FOR
            SELECT *
            FROM   cta_verifica c
	    WHERE  c.nss_uni      = val.nss
	    AND    c.afore_cta1   = codigo
	    AND    c.convoca_cta1 = 1
        FOREACH val_8 INTO val2.*

--------# valida extra si cuenta con algun rechazo definitivo
	    IF val.identificador = "02" THEN
		SELECT "X"
		FROM   uni_unificado d
		WHERE  d.nss_uni  = val3.nss_uni
		AND    d.nss_cta1 = val3.nss_cta1
		AND    d.diag_unifica = "02"
		GROUP BY 1

	        IF STATUS <> NOTFOUND THEN
		    LET falla = 3
		    --INSERT INTO falla_verifica VALUES(val.*,val2.*,falla)
		END IF
            END IF

--------# valida extra si se reciclo
            SELECT "X"
	    FROM   uni_recicla e
	    WHERE  e.nss_uni = val2.nss_uni
	    GROUP BY 1

	    IF STATUS <> NOTFOUND THEN
	        DELETE
	        FROM   uni_recicla
	        WHERE  uni_recicla.nss_uni = val2.nss_uni
	    END IF
        END FOREACH
    END FOREACH

---------# genera reporte de fallas

    LET   vreciclado = 0

    SELECT "X"
    FROM   falla_verifica
    GROUP BY 1

    IF STATUS <> NOTFOUND THEN
       ERROR "Existen errores en el archivo que se ha recibido"
       SLEEP 3
       ERROR "Imprimiendo lista de errores"
       CALL imprime_falla() #if
       ERROR ""
       SLEEP 1

       SELECT "X"
       FROM   uni_recicla
       GROUP BY 1

       IF STATUS <> NOTFOUND THEN
           SELECT count(*)
	   INTO   vreciclado
           FROM   uni_recicla

           ERROR "Imprimiendo nss ",vreciclado CLIPPED,
		 " no reciclados en el periodo "
           CALL imprime_recicladas() #ir
           ERROR ""
           SLEEP 1
	   EXIT PROGRAM
       END IF
   END IF

   SELECT "X"
   FROM   uni_recicla
   GROUP BY 1

   IF STATUS <> NOTFOUND THEN
       SELECT COUNT(*)
       INTO   vreciclado
       FROM   uni_recicla

       ERROR "Existen ",vreciclado CLIPPED,
             " de cuentas que no fueron recicladas"
       SLEEP 3

       WHILE TRUE
	   PROMPT " Desea cargar el Archivo  S/N ? "
           FOR CHAR aux_pausa
           IF aux_pausa MATCHES "[SsNn]" THEN
               EXIT WHILE
           END IF
       END WHILE
       IF aux_pausa MATCHES "[Ss]" THEN
           --CALL imprime_recicladas()
       ELSE
           CALL imprime_recicladas()
           ERROR "PROGRAMA FINALIZADO"
           SLEEP 2
           ERROR ""
           EXIT PROGRAM
       END IF
   END IF
END FUNCTION
######################################################
FUNCTION imprime_falla()

   DEFINE falla1 RECORD
        nss           CHAR(11),
        tipo          CHAR(02),
        afore         CHAR(3),
        identificador CHAR(2),
        convoca       CHAR(1),
        nss_uni       CHAR(11),
        nss_cta1      CHAR(11),
        tipo_cta1     CHAR(02),
        afore_cta1    CHAR(3),
        convoca_cta1  CHAR(1),
	falla         SMALLINT
   END RECORD

   DEFINE imprime    CHAR(200)

   LET G_LISTA = g_param_uni.ruta_rescate CLIPPED,"/" CLIPPED,
                "falla1" CLIPPED

   START REPORT listado_1 TO G_LISTA
   DECLARE fal_1 CURSOR FOR
   SELECT *
   FROM   falla_verifica
   ORDER BY 1

   FOREACH fal_1 INTO falla1.*
      OUTPUT TO REPORT listado_1(falla1.*) #l2
   END FOREACH

   FINISH REPORT listado_1

   LET G_LISTA = "cd ",g_param_uni.ruta_rescate CLIPPED,"/" CLIPPED,
                 ";sed -e '/^$/d' ","falla1" CLIPPED,
                 " >", HOY USING "YYYYMMDD",".uni10" CLIPPED
   RUN G_LISTA

   LET imprime = "vi ",G_LISTA
   #LET imprime = "lp ",G_LISTA
   #RUN imprime
   #LET borra   = "rm ",G_LISTA
   #RUN borra

END FUNCTION
######################################################
FUNCTION imprime_recicladas()
   DEFINE rec  RECORD LIKE uni_recicla.*
   DEFINE imprime   CHAR(200)

   LET G_LISTA1 = g_param_uni.ruta_rescate CLIPPED,"/" CLIPPED,
                  "recicla" CLIPPED

   START REPORT listado_2 TO G_LISTA1
   DECLARE rec_1 CURSOR FOR
   SELECT *
   FROM   uni_recicla
   ORDER BY 1

   FOREACH rec_1 INTO rec.*
      OUTPUT TO REPORT listado_2(rec.*) #l2
   END FOREACH

   FINISH REPORT listado_2

   LET G_LISTA1 = "cd ",g_param_uni.ruta_rescate CLIPPED,"/" CLIPPED,
                  ";sed -e '/^$/d' ","recicla" CLIPPED,
                  " >", HOY USING "YYYYMMDD",".uni11" CLIPPED
   RUN G_LISTA

   LET imprime = "vi ",G_LISTA1
   #LET imprime = "lp ",G_LISTA1
   #RUN imprime
   #LET borra   = "rm ",G_LISTA
   #RUN borra

END FUNCTION

FUNCTION f_indicadores()
   DEFINE   lr_indi                RECORD
            ident_movimiento       CHAR(02),
            status_convoca         SMALLINT,
            cuantos                INTEGER
            END RECORD
   DEFINE   lr_uni_fallecido       RECORD LIKE uni_fallecido.*
   DEFINE   lr_cta1_fallecido      RECORD
            nss_cta1               LIKE uni_fallecido.nss,
            cve_ent_cta1           LIKE uni_fallecido.cve_afore,
            tipo_ent_cta1          LIKE uni_fallecido.tipo_registro
            END RECORD

   DECLARE  sel_ind      CURSOR FOR
      SELECT  a.ident_movimiento idm,
              a.status_convoca scon,
              count(*)
        FROM  uni_unificador a
       WHERE  a.folio = ultimo_folio
         AND  a.cve_ent_nss   = codigo
         AND  a.status_convoca  IN (1,2,3)
       GROUP  BY 1,2
    UNION ALL
      SELECT  b.ident_movimiento, a.status_convoca,  count(*)
        FROM  uni_unificado a,
              uni_unificador b
       WHERE  a.nss_uni = b.nss_uni
         AND  a.folio     = b.folio
         AND  a.folio = ultimo_folio
         AND  a.cve_ent_cta1    = codigo
         AND  a.status_convoca  IN (1,2,3 )
       GROUP  BY 1,2
       ORDER  BY 1,2

    FOREACH    sel_ind    INTO   lr_indi.*
        IF      lr_indi.ident_movimiento   =  '01'    THEN
                IF      lr_indi.status_convoca      =   1  THEN
                        LET    gi_intra_responsable     =   gi_intra_responsable
                                                        +   lr_indi.cuantos
                ELSE
                        LET    gi_intra_fallecido       =   gi_intra_fallecido
                                                        +   lr_indi.cuantos
                END IF
        ELSE
                IF      lr_indi.status_convoca        =   1  THEN
                        LET    gi_extra_responsable   =   gi_extra_responsable
                                                      +   lr_indi.cuantos
                END IF
                IF      lr_indi.status_convoca        =   2  THEN
                        LET    gi_extra_fallecido     =   gi_extra_fallecido
                                                      +   lr_indi.cuantos
                END IF
                IF      lr_indi.status_convoca        =   3  THEN
                        LET    gi_extra_inverso       =   gi_extra_inverso
                                                      +   lr_indi.cuantos
                END IF
        END IF
    END FOREACH
    IF  gi_intra_fallecido    > 0
     OR gi_extra_fallecido    > 0   THEN

        DECLARE  sel_fall     CURSOR FOR
               SELECT  folio,
                       cont_servicio,
                       1 ,
                       nss_uni,
                       cve_ent_nss,
                       tipo_ent_nss
                 FROM  uni_unificador a
                WHERE  a.folio = ultimo_folio
                  AND  a.cve_ent_nss   = codigo
                  AND  a.status_convoca  = 2
         FOREACH  sel_fall    INTO  lr_uni_fallecido.*
             INSERT INTO uni_fallecido  VALUES(lr_uni_fallecido.*)
             DECLARE  sel_fall_cta1   CURSOR FOR
                    SELECT  nss_cta1,
                            cve_ent_cta1,
                            tipo_ent_cta1
                      FROM  uni_unificado a
                     WHERE  a.folio = ultimo_folio
                       AND  a.nss_uni  = lr_uni_fallecido.nss
             FOREACH  sel_fall_cta1   INTO lr_cta1_fallecido.*
                 LET    lr_uni_fallecido.tipo_nss      = 2
                 LET    lr_uni_fallecido.nss           = lr_cta1_fallecido.nss_cta1
                 LET    lr_uni_fallecido.cve_afore     = lr_cta1_fallecido.cve_ent_cta1
                 LET    lr_uni_fallecido.tipo_registro = lr_cta1_fallecido.tipo_ent_cta1
                 INSERT INTO uni_fallecido  VALUES(lr_uni_fallecido.*)
             END FOREACH
         END FOREACH

         DECLARE  sel_fhij    CURSOR FOR
               SELECT  a.folio,
                       a.cont_servicio,
                       1,
                       a.nss_uni,
                       a.cve_ent_nss,
                       a.tipo_ent_nss
                 FROM  uni_unificador a,
                       uni_unificado b
                WHERE  a.folio     = b.folio
                  AND  a.nss_uni   = b.nss_uni
                  AND  b.folio     = ultimo_folio
                  AND  b.cve_ent_cta1  = codigo
                  AND  b.status_convoca  = 2
         FOREACH  sel_fhij    INTO  lr_uni_fallecido.*
             INSERT INTO uni_fallecido  VALUES(lr_uni_fallecido.*)
             DECLARE  sel_fhij_cta1   CURSOR FOR
                    SELECT  nss_cta1,
                            cve_ent_cta1,
                            tipo_ent_cta1
                      FROM  uni_unificado a
                     WHERE  a.folio = ultimo_folio
                       AND  a.nss_uni  = lr_uni_fallecido.nss
             FOREACH  sel_fhij_cta1   INTO lr_cta1_fallecido.*
                 LET    lr_uni_fallecido.tipo_nss      = 2
                 LET    lr_uni_fallecido.nss           = lr_cta1_fallecido.nss_cta1
                 LET    lr_uni_fallecido.cve_afore     = lr_cta1_fallecido.cve_ent_cta1
                 LET    lr_uni_fallecido.tipo_registro = lr_cta1_fallecido.tipo_ent_cta1
                 INSERT INTO uni_fallecido  VALUES(lr_uni_fallecido.*)
             END FOREACH
         END FOREACH


    END IF
END FUNCTION


######################################################
REPORT listado_1(reg_2)
   DEFINE reg_2 RECORD
        nss           CHAR(11),
        tipo          CHAR(02),
        afore         CHAR(3),
        identificador CHAR(2),
        convoca       CHAR(1),
        nss_uni       CHAR(11),
        nss_cta1      CHAR(11),
        tipo_cta1     CHAR(02),
        afore_cta1    CHAR(3),
        convoca_cta1  CHAR(1),
	falla         SMALLINT
   END RECORD

   DEFINE vfalla     CHAR(20)

   OUTPUT
       PAGE LENGTH 60
       LEFT MARGIN 0
       RIGHT MARGIN 0
       TOP MARGIN 0
       BOTTOM MARGIN 0

   FORMAT
   ON EVERY ROW
      IF reg_2.falla = 1 THEN
	 LET vfalla = "inconsistencia en Intra Afore"
      END IF

      IF reg_2.falla = 2 THEN
         LET vfalla = "inconsistencia en Intra Afore"
      END IF

      IF reg_2.falla = 3 THEN
	 LET vfalla = "inconsistencia en Extra Afore"
      END IF

      PRINT
           COLUMN 001,reg_2.nss,
           COLUMN 012,reg_2.tipo,
           COLUMN 014,reg_2.afore,
           COLUMN 017,reg_2.identificador,
           COLUMN 019,reg_2.convoca,
           COLUMN 020,reg_2.nss_cta1,
           COLUMN 030,reg_2.tipo_cta1,
           COLUMN 032,reg_2.afore_cta1,
           COLUMN 035,reg_2.convoca_cta1,
           COLUMN 036,vfalla
END REPORT
######################################################
REPORT listado_2(reg_3)
   DEFINE reg_3  RECORD LIKE uni_recicla.*

   OUTPUT
       PAGE LENGTH 60
       LEFT MARGIN 0
       RIGHT MARGIN 0
       TOP MARGIN 0
       BOTTOM MARGIN 0

   FORMAT
   ON EVERY ROW
       PRINT
           COLUMN 001,reg_3.nss_uni,
           COLUMN 012,reg_3.folio USING "&&&&",
           COLUMN 016,reg_3.fecha_recicla USING "DD/MM/YYYY"
END REPORT
######################################################
FUNCTION valida_afore_fusion(ls_afore_ori)
   DEFINE ls_afore_ori    SMALLINT
   DEFINE ls_afore_fusion SMALLINT
   DEFINE lc_afore        CHAR(03)

   SELECT NVL(SUM(afore_fusion),0)
   INTO   ls_afore_fusion
   FROM   tab_afore
   WHERE  afore_cod = ls_afore_ori

   IF ls_afore_fusion <> 0 THEN
      LET lc_afore = ls_afore_fusion USING "&&&"
   ELSE
      LET lc_afore = ls_afore_ori    USING "&&&"
   END IF

   RETURN lc_afore
END FUNCTION
######################################################
FUNCTION f_rep_nss() #CPL-1867

   DEFINE
      ls_cont                 SMALLINT
      ,lr_marca              RECORD
         nss                  CHAR(11)
        ,tipo                 CHAR(10)
      END RECORD
   LET ls_cont = 1
   LET G_LISTA = g_param_uni.ruta_listados CLIPPED,"/" CLIPPED,usuario CLIPPED,".nss.",HOY USING "YYYYMMDD",".txt"
   START REPORT rep_nss TO G_LISTA
   DECLARE curs_marca CURSOR FOR
   SELECT nss
          ,tipo
   FROM tmp_uni_nss_sinmarca
   ORDER BY 2

   FOREACH curs_marca INTO lr_marca.*
      OUTPUT TO REPORT rep_nss(lr_marca.*)
   END FOREACH
   FINISH REPORT rep_nss
END FUNCTION
#################################################################################
REPORT rep_nss (lr_reporte)   #CPL-1867
   DEFINE
      lr_reporte              RECORD
         nss                  CHAR(11)
         ,tipo                CHAR(10)
      END RECORD

   OUTPUT

      PAGE LENGTH     2
      LEFT MARGIN     0
      RIGHT MARGIN    0
      TOP MARGIN      0
      BOTTOM MARGIN   0

   FORMAT
      FIRST PAGE HEADER
      PRINT
         "Tipo"
         ,"|"
         ,"NSS"
      ON EVERY ROW

         PRINT
         lr_reporte.tipo
         ,"|"
         ,lr_reporte.nss

      ON LAST ROW
         PRINT
            "Total de Registros : "
            ,"|"
            ,COUNT(*) USING "<<<<<"
END REPORT