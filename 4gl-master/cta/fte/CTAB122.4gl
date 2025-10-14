#############################################################################
#Proyecto            => SAFRE ( MEXICO )                                    #
#Propietario         => E.F.P.                                              #
#Programa            => ACTUALIZA REGISTROS SALDO CERO                      #
#Descripcion         => PROCESO DE ACTUALIZACIONES DE cta_saldo_cero        #
#Por                 => OMAR SABNDOVAL BADILLO                              #
#Fecha               => 13 de febrero de 2006                               #
#Actualizacion       => MAURO MUÑZ CABALLERO                               #
#Fecha Actualizacion => 18 DE ABRIL DE 2007                                 #
#Sistema             => CTA.                                                #
#############################################################################

DATABASE safre_af

GLOBALS

   DEFINE g_parametro RECORD LIKE seg_modulo.*

   DEFINE opc               CHAR(1),
          vcod_operacion    CHAR(2),
          cve_ent_origen    CHAR(3),
          v_usuario         CHAR(8),
          generar           CHAR(20),
          nombre_archivo    CHAR(20),
          vdesmarca         CHAR(100),
          vmarca            CHAR(200),
          archivo_saldo     CHAR(200),
          consulta_cta_sale CHAR(300),
          carga_reg         CHAR(330)

   DEFINE v_cae_rech        SMALLINT,
          existe            SMALLINT,
          vmarca_ent        SMALLINT,
          vtipo_solic       SMALLINT,
          xmarca            SMALLINT,
          xrechazo          SMALLINT,
          vcorrelativo      SMALLINT,
          vestado_marca     SMALLINT,
          vcodigo_rechazo   SMALLINT,
          vmarca_causa      SMALLINT

   DEFINE cont_rech         INTEGER,
          cont_marc         INTEGER,
          cont_proc         INTEGER,
          cuantos           INTEGER

   DEFINE hoy               DATE,
          vfecha_causa      DATE

   DEFINE vn_folio LIKE afi_mae_afiliado.n_folio

   DEFINE vcod_proceso      SMALLINT
   DEFINE vtipo_solicitud   SMALLINT
   DEFINE vcurp             CHAR(18)

   DEFINE g_impre           CHAR(300)   

END GLOBALS

MAIN

   CALL STARTLOG("CTAB122.log")
   CALL inicio()
   CALL proceso()

END MAIN

FUNCTION inicio()
#i---------------

   SELECT *,
          USER
   INTO   g_parametro.*,
          v_usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "cta"

   SELECT codigo_afore
   INTO   cve_ent_origen
   FROM   tab_afore_local

   LET hoy = TODAY

   LET vdesmarca = " EXECUTE PROCEDURE desmarca_cuenta (?,?,?,?,?,?)"
   PREPARE desmarca FROM vdesmarca

   LET vmarca = " EXECUTE PROCEDURE marca_cuenta (?,?,?,?,?,?,?,?)"
   PREPARE marcaje FROM vmarca
   
   --LET consulta_cta_sale  = "EXECUTE PROCEDURE fn_cuenta_saliente(?,?,?,?)"
   --PREPARE exe_ccs FROM consulta_cta_sale   

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_pla_saldo_cero
      CREATE TEMP TABLE tmp_pla_saldo_cero
        (n_registros CHAR(100))
   WHENEVER ERROR STOP

END FUNCTION

FUNCTION proceso()
#p----------------

   OPEN WINDOW ventana1 AT 2,2 WITH FORM "CTAB1221" ATTRIBUTE(BORDER)
   DISPLAY "      [Enter] Iniciar                                    [Ctrl-C]  Salir       " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " CTAB122         RECIBE ARCHIVOS DE CONFIRMACION SALDO CERO                    " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY hoy USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

   INPUT BY NAME generar WITHOUT DEFAULTS
      BEFORE FIELD generar
         LET generar = NULL
         CLEAR FORM

      AFTER FIELD generar
         IF generar IS NULL THEN
            ERROR " EL CAMPO NO PUEDE SER NULO ..."
            NEXT FIELD generar
         END IF

         SELECT nombre
         INTO   nombre_archivo
         FROM   cta_ctr_archivo
         WHERE  nombre = generar

         IF nombre_archivo IS NOT NULL THEN
            ERROR " ESTE ARCHIVO YA SE PROCESO ..."
            SLEEP 2
            ERROR ""
            NEXT FIELD generar
         END IF

         LET archivo_saldo = g_parametro.ruta_rescate CLIPPED,"/",
                             generar CLIPPED

         WHENEVER ERROR CONTINUE
            LOAD FROM archivo_saldo DELIMITER "+"
            INSERT INTO tmp_pla_saldo_cero
         WHENEVER ERROR CONTINUE

         SELECT count(*)
         INTO   cuantos
         FROM   tmp_pla_saldo_cero

         IF cuantos = 0 THEN
            ERROR " NOMBRE DE ARCHIVO INCORRECTO O VACIO "
            SLEEP 2
            ERROR ""
            NEXT FIELD generar
         ELSE
            EXIT INPUT
         END IF

     ON KEY (INTERRUPT)
         ERROR " PROCESO CANCELADO "
         SLEEP 2
         EXIT PROGRAM
         END INPUT

         ERROR " PROCESANDO INFORMACION "
         SLEEP 2

         LET existe = lee_archivo_plano()

         DISPLAY " TOTAL REGISTROS PROCESADOS : ",cont_proc AT 15,8
         DISPLAY " TOTAL REGISTROS MARCADOS   : ",cont_marc AT 16,8
         DISPLAY " TOTAL REGISTROS RECHAZADOS : ",cont_rech AT 17,8

         ERROR " PROCESO FINALIZADO. PRESIONE <ENTER> PARA SALIR "
         PROMPT "" FOR opc

         IF existe = 1 THEN
            INSERT INTO cta_ctr_archivo VALUES(generar,0,hoy,"99")
         END IF

         CLOSE WINDOW ventana1

END FUNCTION

FUNCTION lee_archivo_plano()
#lap------------------------

   DEFINE l_nss         CHAR(11),
          l_curp        CHAR(18),
          l_diagnostico CHAR(03),
          l_iden_reg    CHAR(03)

   DEFINE xcarga_reg RECORD LIKE cta_saldo_cero.*

   DEFINE v_estado_marca ,
          v_correlativo SMALLINT

   DEFINE v_cod_rch     CHAR(2) ---rechazo lote 
   DEFINE v_mot_rch     CHAR(3) ---rechazo lote
   DEFINE vfecha_corte  CHAR(10)

   DEFINE ffecha_corte  DATE
   DEFINE t_dias        SMALLINT
   DEFINE rut_desmar    CHAR(1)

   LET vmarca_ent    = 151
   LET vcod_proceso  = 50
   LET vcorrelativo  = 0
   LET vmarca_causa  = 0
   LET vfecha_causa  = ''
   LET cont_proc     = 0
   LET cont_marc     = 0
   LET cont_rech     = 0
   LET existe        = 0
   LET v_cod_rch     = ""
   LET v_mot_rch     = ""

   LET g_impre = g_parametro.ruta_listados CLIPPED,"/",v_usuario CLIPPED,
                 ".RECEP_ARCH_OP99_",hoy USING "ddmmyyyy" CLIPPED

   DISPLAY "Nombre reporte: ", g_impre AT 12,2

   START REPORT rpt_archivo TO g_impre
   DECLARE cur_1 CURSOR FOR
   SELECT  *
   FROM    tmp_pla_saldo_cero

   FOREACH cur_1 INTO carga_reg
      IF carga_reg[001,002] = "01" THEN
         IF carga_reg[005,006] <> "99" THEN
            ERROR "EL ARCHIVO NO ES DE SALDO CERO"
            SLEEP 4
            EXIT FOREACH
         ELSE
            LET v_cod_rch    = carga_reg[036,037]
            LET v_mot_rch    = carga_reg[038,040]
            LET vfecha_corte = carga_reg[017,024]

            LET vfecha_corte = vfecha_corte[5,6],"/",vfecha_corte[7,8],"/",vfecha_corte[1,4]
            LET ffecha_corte = vfecha_corte

            #LET t_dias = DAY(ffecha_corte)

            #LET ffecha_corte = ffecha_corte - t_dias
         END IF
      ELSE
         IF carga_reg[001,002] = "03" THEN
            LET l_nss         = carga_reg[003,013]
            LET l_curp        = carga_reg[014,031]
            LET l_diagnostico = carga_reg[044,046]
            LET l_iden_reg    = carga_reg[040,041]

            SELECT a.n_unico, a.n_folio, a.tipo_solicitud
            INTO   l_curp, vn_folio, vtipo_solic
            FROM   afi_mae_afiliado a
            WHERE  a.n_seguro        = l_nss
            AND    a.tipo_solicitud NOT IN (8,12)

            IF SQLCA.SQLCODE = 0 THEN
               LET existe = 1
            ELSE
               SELECT a.n_seguro, a.n_folio, a.tipo_solicitud
               INTO   l_nss, vn_folio, vtipo_solic
               FROM   afi_mae_afiliado a
               WHERE  a.n_unico         = l_curp
               AND    a.tipo_solicitud IN (8,12)

               IF SQLCA.SQLCODE = 0 THEN
                  LET existe = 1
               END IF
            END IF

            IF existe = 1 THEN
               LET xcarga_reg.nss           = l_nss
               LET xcarga_reg.curp          = l_curp
               LET xcarga_reg.diagnostico   = l_diagnostico
               LET xcarga_reg.fecha_proceso = hoy

               IF v_cod_rch = "02" AND 
                  (l_diagnostico IS NULL OR l_diagnostico = "   ") THEN
                  LET l_diagnostico = v_mot_rch
               END IF

               IF l_diagnostico <> "101" OR v_cod_rch = "02" THEN
                  LET cont_rech      = cont_rech + 1
                  LET v_estado_marca = 40

                  IF l_iden_reg = "03" THEN            # MARCAR 151   03 
                     UPDATE cta_ctr_cuenta
                     SET ind_saldo_cero = 0
                     WHERE nss = l_nss

                     LET vmarca_ent = 151
                     LET rut_desmar = "S"
                  ELSE                                 # MARCAR 160   04
                     LET vmarca_ent = 160
                     LET rut_desmar = "N"
                  END IF
               ELSE
                  LET cont_marc      = cont_marc + 1
                  LET v_estado_marca = 0

                  IF l_iden_reg = "03" THEN            # MARCAR 151   03 
                     LET vmarca_ent = 151
                     LET rut_desmar = "S"
                  ELSE                                 # MARCAR 160   04
                     LET vmarca_ent = 160
                     LET rut_desmar = "S"
                  END IF
               END IF --  DEL DIAGNOSTICO

               -----  DESMARCAJE SDO CERO MARCAJE DE CUENTA CANCELADA  -----

               IF rut_desmar = "S" THEN
                  EXECUTE desmarca USING l_nss         ,   # nss
                                         vmarca_ent    ,   # marca_entra
                                         v_correlativo ,   # correlativo
                                         v_estado_marca,   # estado_marca
                                         vmarca_ent    ,   # marca_causa
                                         v_usuario         # usuario
               END IF

               IF l_nss[1,1] <> " " THEN
                  UPDATE cta_saldo_cero
	          SET    diagnostico   = l_diagnostico
	          WHERE  nss           = l_nss
                  AND    (fecha_corte  = ffecha_corte
	          OR     fecha_proceso = ffecha_corte)
	       ELSE
	          IF l_curp[1,1] <> " " THEN
	             UPDATE cta_saldo_cero
		     SET    diagnostico   = l_diagnostico
		     WHERE  curp          = l_curp
                     AND    (fecha_corte  = ffecha_corte
	             OR     fecha_proceso = ffecha_corte)
	          END IF
	       END IF

               OUTPUT TO REPORT rpt_archivo(ffecha_corte, l_nss, l_curp,
                                            l_diagnostico,vn_folio,vtipo_solic)
	       
	       LET l_diagnostico = ""
	       	       
            END IF -- EXISTE EL REGISTRO
         END IF -- EL ARCHIVO ES DE SALDO CERO
      END IF -- EL ARCHIVO TIENE ENCABEZADO

      LET cont_proc     = cont_proc + 1
      LET l_diagnostico = ""
   END FOREACH
   FINISH REPORT rpt_archivo

   RETURN existe
END FUNCTION


REPORT rpt_archivo(lfecha_corte, lnss, lcurp, ldiagnostico, ln_folio, 
                   ltipo_solic)
  DEFINE 
    lfecha_corte      DATE,
    lnss              CHAR(11),
    lcurp             CHAR(18),
    ldiagnostico      CHAR(3),
    ln_folio          DECIMAL(10,0),
    ltipo_solic       SMALLINT,
    lnombre_completo  CHAR(120),
    ldesc_diagnostico CHAR(75)

  DEFINE
    nombres           CHAR(40),
    paterno           CHAR(40),
    materno           CHAR(40)

  DEFINE
    codigo_afore      SMALLINT,
    razon_social      CHAR(50)

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60

   ORDER BY ldiagnostico, lnss

   FORMAT
      PAGE HEADER
         PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'

         SELECT a.codigo_afore, a.razon_social
           INTO codigo_afore, razon_social
           FROM tab_afore_local a

         PRINT COLUMN 001,"AFORE : ", codigo_afore USING "###", " ",
                          razon_social CLIPPED
         PRINT
         PRINT COLUMN 001,"MODULO: ", g_parametro.modulo_desc CLIPPED
         PRINT
         PRINT COLUMN 001,"PROGRAMA: CTAB122 ",
               COLUMN 035,"LISTADO DE LA RECEPCION DEL ARCHIVO DE LA OP 99 - REGISTROS CON SALDO CERO",
               COLUMN 140,"FECHA: ",TODAY USING "dd-mm-yyyy"
         PRINT                  

         PRINT COLUMN 001,"FECHA CORTE: ", lfecha_corte USING "DD-MM-YYYY"

         PRINT

         PRINT COLUMN 001, "NSS",
               COLUMN 013, "CURP",
               COLUMN 034, "NOMBRE",
               COLUMN 097, "DIAGNOSTICO"

         SKIP 1 LINE

      ON EVERY ROW
         SELECT a.nombres, a.paterno, a.materno
         INTO   nombres, paterno, materno
         FROM   afi_mae_afiliado a
         WHERE  a.n_seguro  = lnss
         IF STATUS = NOTFOUND THEN
            SELECT a.nombre, a.paterno, a.materno
            INTO   nombres, paterno, materno
            FROM   afi_mae_afiliado a
            WHERE  a.n_unico = lcurp
            IF STATUS = NOTFOUND THEN
               LET nombres = NULL
               LET paterno = NULL
               LET materno = NULL  
            END IF
         END IF
     
         LET lnombre_completo = nombres CLIPPED, " ", 
                                paterno CLIPPED, " ",
                                materno CLIPPED

         SELECT a.desc_diagnostico
         INTO   ldesc_diagnostico
         FROM   tab_diag_sdo_cero a
         WHERE  a.diagnostico = ldiagnostico

         #AFTER GROUP OF lfecha_corte
           PRINT COLUMN 001, lnss CLIPPED,
                 COLUMN 013, lcurp CLIPPED,
                 COLUMN 034, lnombre_completo CLIPPED,
                 COLUMN 097, ldiagnostico CLIPPED,
                 COLUMN 101, ldesc_diagnostico CLIPPED 

         AFTER GROUP OF ldiagnostico
           #PRINT
           PRINT COLUMN 01," Total de registros : ",
           GROUP COUNT(*) USING "<<<<"
           PRINT

      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"

      ON LAST ROW

         PRINT COLUMN 001,"TOTAL REGISTROS         : ", cont_proc #USING "##,###,##$" 
         PRINT COLUMN 001,"TOTAL CUENTAS SALDO CERO: ", cont_marc #USING "##,###,##$"
         PRINT COLUMN 001,"TOTAL RECHAZOS          : ", cont_rech #USING "##,###,##$"

         SKIP 2 LINE

         PRINT COLUMN 01," Total de registros : ",
         COUNT(*) USING "<<<<"

END REPORT

#####################################################################

