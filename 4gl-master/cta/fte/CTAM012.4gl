######################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                   #
#Owner             => E.F.P.                                         #
#Programa CTAM012  => OBTENER DIFERENCIAS DE REGIMEN.                #
#Fecha             => 09 de Febrero de 2010.                         #
#Modificado por    => FERNANDO HERRERA HERNANDEZ                     #
#Sistema           => CTA.                                           #
######################################################################
DATABASE safre_af

GLOBALS
  DEFINE g_reg        RECORD
    nss               CHAR(11),
    ind_edad          SMALLINT,
    fecha_corte       DATE
  END RECORD

  DEFINE g_reg1       RECORD
    nss               CHAR(11),
    grupo_regimen     SMALLINT,
    codigo_siefore    SMALLINT
  END RECORD

  DEFINE
    vfecha_ident      DATE,
    vsiefore          SMALLINT,
    vacciones         DECIMAL(22,6)

  DEFINE g_reg2       RECORD
    nss               CHAR(11),        --cuota
    edad              SMALLINT,        --fnac
    criterio          SMALLINT,        --fnac
    ind_edad_ident    SMALLINT,        --fnac
    ind_edad          SMALLINT,        --ctr cta
    siefore_nss       SMALLINT,        --nss reg
    grupo_regimen     SMALLINT,        --nss reg
    siefore_sdo       SMALLINT,        --saldos
    monto_en_acciones DECIMAL(22,6),   --saldos
    fecha_corte       DATE,            --cuota
    fecha_proceso     DATE
  END RECORD

  DEFINE g_param_dis  RECORD LIKE seg_modulo.*

  DEFINE sw_1         SMALLINT,
    aux_pausa         CHAR(01),
    HOY               DATE,
    aux_estad_desc    CHAR(40),
    seg_usuario       CHAR(08),
    pos               SMALLINT,
    cla_where         CHAR(900),
    sel_where         CHAR(900),
    pos2              SMALLINT,
    cla_where2        CHAR(300),
    sel_where2        CHAR(300),
    cla_where3        CHAR(300),
    sel_where3        CHAR(300),
    v_sql_1           CHAR(300),
    g_lista           CHAR(300),
    g_impre           CHAR(300),
    g_lista2          CHAR(300),
    g_impre2          CHAR(300),
    g_impre3          CHAR(300),
    vfecha_ini        DATE,
    gtotal            INTEGER,
    l                 SMALLINT,
    vnss              CHAR(11),
    vcurp             CHAR(18),
    vtot_reg          INTEGER,
    vfactualiza_reg   DATE,
    vrowid_reg        INTEGER,
    sel_his           CHAR(2500)

  DEFINE
    v_existe          SMALLINT,
    v_edad            SMALLINT,
    v_criterio        SMALLINT,
    v_ind_edad        SMALLINT,
    v_curp            CHAR(18),
    v_rfc             CHAR(13),
    v_fena            DATE

END GLOBALS
##########################################################################
MAIN
  OPTIONS 
    PROMPT LINE LAST,
    INPUT WRAP,
    ACCEPT KEY CONTROL-O

  DEFER INTERRUPT

  CALL STARTLOG("CTAM012.log")
  CALL inicio()
  CALL proceso()

END MAIN
##########################################################################
FUNCTION inicio()

  SELECT USER,*
  INTO   seg_usuario
  FROM   seg_modulo
  WHERE  modulo_cod = 'cta'

  SELECT ruta_listados, modulo_desc
  INTO   g_param_dis.ruta_listados, g_param_dis.modulo_desc
  FROM   seg_modulo
  WHERE  modulo_cod = 'cta'

  LET v_sql_1 = "EXECUTE PROCEDURE fn_fnacimiento(?,?)"
  #PREPARE stmt1 FROM v_sql_1

  #LET vfecha_ident = '06/30/2009'
  LET vfecha_ident = TODAY

END FUNCTION
##########################################################################
FUNCTION proceso()

  LET HOY = TODAY

  OPEN WINDOW ventana_1 AT 3,3 WITH FORM "CTAM0121" ATTRIBUTE( BORDER)
    DISPLAY " CTAM012           OBTENER DIFERENCIAS DE REGIMEN                                  " AT 3,1 ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

    MENU "CONSULTA"
      COMMAND "Generar" "Generar proceso diferencias de regimen"
        CALL Inicializa()
        CALL Consulta()
      COMMAND "Salir" "Salir del Programa"
        EXIT MENU
    END MENU

END FUNCTION
#############################################################################
FUNCTION Inicializa()


  INITIALIZE g_reg1.*  TO NULL

  WHENEVER ERROR CONTINUE
    DATABASE safre_tmp
      DROP TABLE tmp_dif_regimen
      DROP TABLE tmp_sdos_gpo
  WHENEVER ERROR STOP

  CREATE TABLE tmp_dif_regimen (nss               CHAR(11),        --cuota
                                edad              SMALLINT,        --fnac 
                                criterio          SMALLINT,        --fnac 
                                ind_edad_ident    SMALLINT,        --fnac
                                ind_edad          SMALLINT,        --ctr cta
                                siefore_nss       SMALLINT,        --nss reg
                                grupo_regimen     SMALLINT,        --nss reg
                                siefore_sdo       SMALLINT,        --saldos
                                monto_en_acciones DECIMAL(22,6),   --saldos
                                fecha_corte       DATE,            --cuota
                                fecha_proceso     DATE)

   CREATE TABLE tmp_sdos_gpo   (nss               CHAR(11),
                                grupo             SMALLINT,
                                siefore           SMALLINT,
                                acciones          DECIMAL(22,6))

  DATABASE safre_af

  DISPLAY "                                                                               " AT 16,1

END FUNCTION
################################################################################
FUNCTION Consulta()

  DISPLAY "" AT 1,1
  DISPLAY "" AT 2,1
  DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
  DISPLAY " CONSULTA" AT 1,67 ATTRIBUTE(REVERSE,BOLD)

  ERROR "Generando informacion. . . " 

  LET vtot_reg = 0

  DECLARE cursor_p CURSOR FOR 
  SELECT nss,
         ind_edad,
         fecha_corte
  FROM   safre_tmp:cta_formato_nss
  FOREACH cursor_p INTO g_reg.*

    LET vtot_reg = vtot_reg + 1
    LET pos      = 0
     
    PREPARE stmt1 FROM v_sql_1
    DECLARE curs1 CURSOR FOR stmt1
    OPEN  curs1 USING g_reg.nss, vfecha_ident 
    FETCH curs1 INTO v_existe, 
                     v_edad, 
                     v_criterio, 
                     v_ind_edad,
                     v_curp, 
                     v_rfc, 
                     v_fena
    CLOSE curs1

    IF g_reg.ind_edad <> v_ind_edad THEN

       DECLARE cursor_1 CURSOR FOR
       SELECT nss, grupo_regimen, codigo_siefore
       FROM   cta_nss_regimen
       WHERE  nss            = g_reg.nss
       AND    grupo_regimen IN (1,2,4,7,8,9,10,11)
       FOREACH cursor_1 INTO g_reg1.*

         SELECT 'X'
         FROM   cta_det_sie_acep
         WHERE  nss        = g_reg.nss
         AND    cve_subcta = g_reg1.grupo_regimen
         GROUP BY 1
         IF STATUS = NOTFOUND THEN
            CALL extrae_saldos(g_reg.nss, 
                               g_reg1.grupo_regimen)

            SELECT  siefore,  acciones
            INTO    vsiefore, vacciones
            FROM    safre_tmp:tmp_sdos_gpo
            WHERE   nss   = g_reg.nss
            AND     grupo = g_reg1.grupo_regimen
            IF STATUS = NOTFOUND THEN
               LET vsiefore  = g_reg1.codigo_siefore
               LET vacciones = 0
            END IF
           
            LET g_reg2.nss               = g_reg.nss
            LET g_reg2.edad              = v_edad
            LET g_reg2.criterio          = v_criterio 
            LET g_reg2.ind_edad_ident    = v_ind_edad
            LET g_reg2.ind_edad          = g_reg.ind_edad
            LET g_reg2.siefore_nss       = g_reg1.codigo_siefore
            LET g_reg2.grupo_regimen     = g_reg1.grupo_regimen
            LET g_reg2.siefore_sdo       = vsiefore
            LET g_reg2.monto_en_acciones = vacciones
            LET g_reg2.fecha_corte       = g_reg.fecha_corte
            LET g_reg2.fecha_proceso     = HOY
   
            INSERT INTO safre_tmp:tmp_dif_regimen VALUES (g_reg2.*) 
         END IF
       END FOREACH
    END IF

    DISPLAY "Total registros: ", vtot_reg AT 10,10
  END FOREACH

  ERROR "Proceso finalizado."

END FUNCTION
################################################################################
FUNCTION extrae_saldos(vnss, vgrupo_regimen)

  DEFINE vnss              CHAR(11)
  DEFINE vgrupo_regimen    SMALLINT
 
  CASE vgrupo_regimen 
    WHEN 1
      INSERT INTO safre_tmp:tmp_sdos_gpo
      SELECT nss, 1, siefore, SUM(monto_en_acciones) acciones
      FROM   safre_tmp:tmp_saldo_corte
      WHERE  nss        = vnss
      AND    subcuenta IN (1,2,5,6,9,17,18)
      GROUP BY 1,2,3

    WHEN 2
      INSERT INTO safre_tmp:tmp_sdos_gpo
      SELECT nss, 2, siefore, SUM(monto_en_acciones) acciones
      FROM   safre_tmp:tmp_saldo_corte
      WHERE  nss        = vnss
      AND    subcuenta IN (11,12,24,25)
      GROUP BY 1,2,3

    WHEN 4
      INSERT INTO safre_tmp:tmp_sdos_gpo
      SELECT nss, 4, siefore, SUM(monto_en_acciones) acciones
      FROM   safre_tmp:tmp_saldo_corte
      WHERE  nss        = vnss
      AND    subcuenta IN (15,16,26,27)
      GROUP BY 1,2,3

    WHEN 7
      INSERT INTO safre_tmp:tmp_sdos_gpo
      SELECT nss, 7, siefore, SUM(monto_en_acciones) acciones
      FROM   safre_tmp:tmp_saldo_corte
      WHERE  nss        = vnss
      AND    subcuenta IN (7)
      GROUP BY 1,2,3

    WHEN 8
      INSERT INTO safre_tmp:tmp_sdos_gpo
      SELECT nss, 8, siefore, SUM(monto_en_acciones) acciones
      FROM   safre_tmp:tmp_saldo_corte
      WHERE  nss        = vnss
      AND    subcuenta IN (13)
      GROUP BY 1,2,3

    WHEN 9
      INSERT INTO safre_tmp:tmp_sdos_gpo
      SELECT nss, 9, siefore, SUM(monto_en_acciones) acciones
      FROM   safre_tmp:tmp_saldo_corte
      WHERE  nss        = vnss
      AND    subcuenta IN (20,21,28,29)
      GROUP BY 1,2,3

    WHEN 10
      INSERT INTO safre_tmp:tmp_sdos_gpo
      SELECT nss, 10, siefore, SUM(monto_en_acciones) acciones
      FROM   safre_tmp:tmp_saldo_corte
      WHERE  nss        = vnss
      AND    subcuenta IN (30,31,32)
      GROUP BY 1,2,3

    WHEN 11
      INSERT INTO safre_tmp:tmp_sdos_gpo
      SELECT nss, 11, siefore, SUM(monto_en_acciones) acciones
      FROM   safre_tmp:tmp_saldo_corte
      WHERE  nss        = vnss
      AND    subcuenta IN (33,34)
      GROUP BY 1,2,3
  END CASE 

END FUNCTION
################################################################################
{FUNCTION impresion(pos)

  DEFINE
    i,
    pos               SMALLINT

  LET g_impre = g_param_dis.ruta_listados CLIPPED,"/",seg_usuario CLIPPED,
                ".CONS_PROC_TRAB.", g_reg1.nss CLIPPED, "_",
                hoy USING "ddmmyyyy" CLIPPED

  DISPLAY "Nombre reporte: ", g_impre AT 19,1

  START REPORT rpt_tabrdeta TO g_impre

    FOR i = 1 TO (pos - 1)
       LET g_reg2.cve_op         = l_record2[i].cve_op
       LET g_reg2.desc_op        = l_record2[i].desc_op
       LET g_reg2.folio          = l_record2[i].folio
       LET g_reg2.tipo_solicitud = l_record2[i].tipo_solicitud
       LET g_reg2.estado         = l_record2[i].estado
       LET g_reg2.cve_cod_op     = l_record2[i].cve_cod_op
       LET g_reg2.desc_cod_op    = l_record2[i].desc_cod_op
       LET g_reg2.cve_diag       = l_record2[i].cve_diag
       LET g_reg2.desc_cve_diag  = l_record2[i].desc_cve_diag
       LET g_reg2.fecha_proc     = l_record2[i].fecha_proc

       IF g_reg2.cve_op IS NULL THEN
          EXIT FOR
       END IF

       OUTPUT TO REPORT rpt_tabrdeta(i, g_reg2.*)
    END FOR
  
    FINISH REPORT rpt_tabrdeta

    ERROR "LISTADO GENERADO..."
    SLEEP 2
    ERROR ""

    --LET g_lista = "lp ",g_impre
    --LET g_lista = "vi ",g_impre
    --RUN g_lista
END FUNCTION
#####################################################################

################################################################################
REPORT rpt_tabrdeta(li, lg_reg2)

   DEFINE li           SMALLINT

   DEFINE lg_reg2      RECORD
     cve_op            CHAR(002),
     desc_op           CHAR(003),
     folio             DECIMAL(10,0),
     tipo_solicitud    SMALLINT,
     estado            SMALLINT,
     cve_cod_op        CHAR(03),
     desc_cod_op       CHAR(080),
     cve_diag          CHAR(15),
     desc_cve_diag     CHAR(080),
     fecha_proc        DATE
   END RECORD

   DEFINE
     vdiag_proceso     CHAR(02),
     vdiagn_desc       CHAR(80)

   DEFINE lreg_proc    RECORD
     cve_modulo        CHAR(003),
     desc_modulo       CHAR(020),
     cve_proceso       CHAR(003),
     desc_proceso      CHAR(045),
     cve_operacion     CHAR(002),
     desc_operacion    CHAR(080)
   END RECORD

   DEFINE 
     codigo_afore      SMALLINT,
     razon_social      CHAR(50)

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
      PAGE HEADER
         PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d' 

         SELECT a.codigo_afore, a.razon_social
           INTO codigo_afore, razon_social
           FROM tab_afore_local a

         PRINT COLUMN 001,"AFORE : ", codigo_afore USING "###", " ",
                          razon_social CLIPPED
         PRINT
         PRINT COLUMN 001,"MODULO: ", g_param_dis.modulo_desc CLIPPED
         PRINT
         PRINT COLUMN 001,"PROGRAMA: CTAM012 ",
               COLUMN 065,"LISTADO DE CONSULTA HISTORIAL PROCESOS POR AFILIADO",
               COLUMN 140,"FECHA: ",TODAY USING "dd-mm-yyyy"
         PRINT

         PRINT COLUMN 001,"NSS: ",  g_reg1.nss,
               COLUMN 020,"CURP: ", g_reg1.curp,
               COLUMN 050,"RFC: ",  g_reg1.rfc
         PRINT COLUMN 001,"Nombre: ", g_reg1.nombre CLIPPED
         PRINT COLUMN 001,"Fecha Certificacion: ",   g_reg1.fecha_cert
                                                     USING "DD-MM-YYYY",
               COLUMN 035,"Fecha Apertura Cuenta: ", g_reg1.fecha_aper
                                                     USING "DD-MM-YYYY",
               COLUMN 072,"Fecha Nacimiento: ",      g_reg1.fecha_naci
                                                     USING "DD-MM-YYYY"
         PRINT COLUMN 001,"Edad: ", g_reg1.edad USING "<<<",
               COLUMN 012,"Regimen por Edad: ", g_reg1.reg_edad USING "<",
               COLUMN 035,"Regimen Actual: ", g_reg1.reg_actual USING "<"  
         SKIP 2 LINE

         PRINT COLUMN 001,"ID",
               COLUMN 007,"OPERACION",
               COLUMN 017,"FOLIO REF",
               COLUMN 027,"TIPO SOLICITUD",
               COLUMN 043,"ESTADO",
               COLUMN 051,"CODIGO OPERACION",
               COLUMN 076,"DIAGNOSTICO",
               COLUMN 132,"FECHA PROCESO"

      ON EVERY ROW
         PRINT COLUMN 001, li USING "<<<<<",
               COLUMN 007, lg_reg2.cve_op CLIPPED,
               COLUMN 010, lg_reg2.desc_op CLIPPED,
               COLUMN 017, lg_reg2.folio USING "<<<<<<<<<<",
               COLUMN 027, lg_reg2.tipo_solicitud USING "<<",
               COLUMN 043, lg_reg2.estado CLIPPED,
               COLUMN 051, lg_reg2.cve_cod_op CLIPPED,
               COLUMN 054, lg_reg2.desc_cod_op[1,20] CLIPPED,
               COLUMN 076, lg_reg2.cve_diag[1,3] CLIPPED,
               COLUMN 080, lg_reg2.desc_cve_diag[1,50] CLIPPED,
               COLUMN 132, lg_reg2.fecha_proc USING "DD-MM-YYYY" 

      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"

      ON LAST ROW
         SKIP 2 LINE
         PRINT COLUMN 01," Total de registros : ",
         COUNT(*) USING "<<<<"

         SKIP 2 LINE

         PRINT COLUMN 01,"Nomenclatura: "

         SKIP 1 LINE

         PRINT COLUMN 01,"Operacion / Proceso / Modulo"
         DECLARE l_1 CURSOR FOR  
         SELECT unique c.cve_modulo, c.desc_modulo,
                a.cve_proceso, a.desc_proceso,
                d.cve_operacion, d.desc_operacion, d.secuencia 
         FROM   tab_cta_proceso a, tmp_his_afi b, tab_cta_modulo c,
                tab_cta_operacion d
         WHERE  a.cve_proceso   = desc_op
         AND    d.cve_operacion = cve_op
         AND    a.cve_modulo    = c.cve_modulo
         AND    a.cve_proceso   = d.cve_proceso
         AND    b.secuencia     = d.secuencia
         ORDER BY d.cve_operacion, d.secuencia
         FOREACH l_1 INTO lreg_proc.*
           PRINT COLUMN 01,lreg_proc.cve_operacion  CLIPPED, " ",
                           lreg_proc.desc_operacion CLIPPED, " / ",
                           lreg_proc.cve_proceso    CLIPPED, " ",
                           lreg_proc.desc_proceso   CLIPPED, " / ",
                           lreg_proc.cve_modulo     CLIPPED, " ",
                           lreg_proc.desc_modulo    CLIPPED 
         END FOREACH

END REPORT
#####################################################################}
