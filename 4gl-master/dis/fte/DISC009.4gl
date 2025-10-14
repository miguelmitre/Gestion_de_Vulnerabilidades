##############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                            #
#                  => E.F.P.                                                  #
#Programa          => DISC009                                                 #
#Descripcion       => GENERA ARCHIVO DE INCONSISTENCIAS                       #
#Sistema           => DIS.                                                    #
#Fecha Inicio      => 31 julio 2000.                                          #
#Fecha Termino     =>                                                         #
#By                => GERARDO ALFONSO VEGA PAREDES                            #
###############################################################################

DATABASE safre_af

GLOBALS
   DEFINE

      g_param RECORD LIKE dis_parametro.*,

      g_cza RECORD
         tip_reg     CHAR(02),
         ident_servi CHAR(02),
         ident_opera CHAR(02), 
         tip_ent_ori CHAR(02),
         cve_ent_ori CHAR(03),
         tip_ent_des CHAR(02),
         cve_ent_des CHAR(03),
         fecha_lote  CHAR(08),
         consec_dia  CHAR(03),
         mod_archivo CHAR(02),
         result_oper CHAR(02),
         diag1       CHAR(03),
         diag2       CHAR(03),
         diag3       CHAR(03)
      END RECORD,

      g_det RECORD
         tip_reg     CHAR(02), 
         ident_servi CHAR(02),
         cve_afore   CHAR(03),
         reg_pat_ims CHAR(11),
         rfc_pat     CHAR(13),
         nss         CHAR(11),
         rfc_tra     CHAR(13), 
         nombre      CHAR(50),
         inconsisten CHAR(02),
         periodo_pag CHAR(08),
         result_oper CHAR(02),
         diag1       CHAR(03),
         diag2       CHAR(03),
         diag3       CHAR(03)
      END RECORD,

      g_sum RECORD
         tip_reg     CHAR(02),
         ident_servi CHAR(02),
         ident_opera CHAR(02), 
         tip_ent_ori CHAR(02),
         cve_ent_ori CHAR(03),
         tip_ent_des CHAR(02),
         cve_ent_des CHAR(03),
         fecha_lote  CHAR(08),
         consec_dia  CHAR(03),
         tot_reg     CHAR(08)
      END RECORD,

      vreg_pat_ims LIKE dis_det_aporte.reg_patronal_imss,
      vrfc_pat     LIKE dis_det_aporte.rfc_patron,
      vnss         LIKE dis_det_aporte.n_seguro,
      vrfc_tra     LIKE dis_det_aporte.n_rfc,
      vnombre      LIKE dis_det_aporte.nom_trabajador,
      vperiodo_pag LIKE dis_det_aporte.periodo_pago,
      vpaterno     LIKE afi_mae_afiliado.paterno,
      vmaterno     LIKE afi_mae_afiliado.materno,
      vnombres     LIKE afi_mae_afiliado.nombres,

      vcodigo_afore SMALLINT,
      hoy           DATE,
      vusuario      CHAR(08),
      opc           CHAR(01),
      vsalida       CHAR(200),
      xfecha_desde  CHAR(08),
      xfecha_hasta  CHAR(08),
      vcontador     INTEGER,
      ejecuta       CHAR(200),
      vconta        INTEGER,
      g_reg array[100] of record
         nombre char(01)
      end record
END GLOBALS

MAIN
   OPTIONS
      INPUT WRAP,
      PROMPT LINE LAST,
      ACCEPT KEY CONTROL-I
      DEFER INTERRUPT 

   CALL Inicializa()

   OPEN WINDOW ventana01 AT 2,2 WITH 20 ROWS, 78 COLUMNS ATTRIBUTE(BORDER)
   DISPLAY " DISC009     GENERACION DE ARCHIVO INCONSISTENCIAS RECAUDACION                 " AT 2,1 ATTRIBUTE(REVERSE)
   DISPLAY hoy USING 'DD-MM-YYYY' AT 2,68 ATTRIBUTE(REVERSE)
   DISPLAY "AL EJECUTAR ESTE PROCESO SERA GENERADO UN ARCHIVO LLAMADO" AT 6,7
   DISPLAY "AAAAMMDD.INCOAF DONDE : AAAA => ANO DE PROCESO" AT 09,7
   DISPLAY "                      : MM   => MES DE PROCESO" AT 10,7
   DISPLAY "                      : DD   => DIA DE PROCESO" AT 11,7

   PROMPT "Desea generar archivo [S/N] ..." FOR opc 
   IF opc MATCHES '[Ss]' THEN
      ERROR "PROCESANDO INFORMACION ..."
      CALL Crea_tabla()
      CALL Proceso_principal()
      ERROR ""
      PROMPT 'PROGRAMA FINALIZO CORRECTAMENTE OPRIMA ENTER P/SALIR' FOR opc
   END IF

   CLOSE WINDOW ventana01
END MAIN

FUNCTION Crea_tabla()

   WHENEVER ERROR CONTINUE
      DATABASE safre_tmp 
      DROP TABLE dis_numero  
      CREATE TABLE dis_numero
      (
       reg        integer,
       nss        char(11),
       trabajador char(50),
       folio      integer
      );
      DATABASE safre_af
   WHENEVER ERROR STOP

END FUNCTION

FUNCTION Inicializa()
   LET hoy = TODAY

   SELECT * 
     INTO g_param.*
     FROM dis_parametro

   SELECT codigo_afore
     INTO vcodigo_afore
     FROM tab_afore_local

   LET vcontador = 0
END FUNCTION

FUNCTION Proceso_principal()
   DEFINE
      vfehca       CHAR(10),
      vfecha_desde DATE,
      vfecha_hasta DATE,    
      cfecha_desde CHAR(10),
      cfecha_hasta CHAR(10),
      vdia         SMALLINT,
      vmes         SMALLINT,
      vano         SMALLINT

   LET vdia = DAY(hoy)
   LET vmes = MONTH(hoy)
   LET vano = YEAR(hoy)
 
---   IF vdia < 30 THEN
   IF vdia < 10 THEN
      LET vmes = vmes - 1
      IF vmes = 0 THEN
         LET vmes = 12 
         LET vano = vano - 1
      END IF
      CALL Ultimo_dia(vmes) RETURNING vdia
      LET vfecha_hasta = MDY(vmes,vdia,vano)
   ELSE
      CALL Ultimo_dia(vmes) RETURNING vdia
      LET vfecha_hasta = MDY(vmes,vdia,vano)
   END IF

   LET vfecha_desde = MDY(MONTH(vfecha_hasta),1,YEAR(vfecha_hasta))

   LET cfecha_desde = vfecha_desde
   LET cfecha_hasta = vfecha_hasta

   LET xfecha_desde = cfecha_desde[7,10],cfecha_desde[1,2],cfecha_desde[4,5]
   LET xfecha_hasta = cfecha_hasta[7,10],cfecha_hasta[1,2],cfecha_hasta[4,5]

   CALL Genera_cabeza()

   CALL Genera_detalle()

   CALL Genera_sumario()

   LET ejecuta = "cd ",g_param.ruta_envio CLIPPED,"/",
                 "; cat cza_incons det_incons sum_incons > ",
                 hoy USING 'YYYYMMDD',".INCOAF"
   RUN ejecuta

END FUNCTION

FUNCTION Genera_cabeza()

   LET vsalida = g_param.ruta_envio CLIPPED,"/cza_incons"

   START REPORT rep_cabeza TO vsalida
      OUTPUT TO REPORT rep_cabeza()
   FINISH REPORT rep_cabeza
END FUNCTION

REPORT rep_cabeza()
   DEFINE
      g_cza RECORD
         tip_reg     CHAR(02),
         ident_servi CHAR(02),
         ident_opera CHAR(02), 
         tip_ent_ori CHAR(02),
         cve_ent_ori CHAR(03),
         tip_ent_des CHAR(02),
         cve_ent_des CHAR(03),
         fecha_lote  CHAR(08),
         consec_dia  CHAR(03),
         mod_archivo CHAR(02),
         result_oper CHAR(02),
         diag1       CHAR(03),
         diag2       CHAR(03),
         diag3       CHAR(03)
      END RECORD

   OUTPUT
      TOP MARGIN  0
      LEFT MARGIN 0
      BOTTOM MARGIN 0
      PAGE LENGTH 1

   FORMAT 
      ON EVERY ROW
         LET g_cza.tip_reg     = "01"
         LET g_cza.ident_servi = "03"
         LET g_cza.ident_opera = "20"
         LET g_cza.tip_ent_ori = "01"
         LET g_cza.cve_ent_ori = vcodigo_afore USING '###'
         LET g_cza.tip_ent_des = "03"
         LET g_cza.cve_ent_des = "001"
         LET g_cza.fecha_lote  = hoy USING 'YYYYMMDD'
         LET g_cza.consec_dia  = "001"
         LET g_cza.mod_archivo = "02"
         LET g_cza.result_oper = "  "
         LET g_cza.diag1       = "   "
         LET g_cza.diag2       = "   "
         LET g_cza.diag3       = "   "

         PRINT COLUMN 01, g_cza.tip_reg,
                          g_cza.ident_servi,
                          g_cza.ident_opera,
                          g_cza.tip_ent_ori,
                          g_cza.cve_ent_ori,
                          g_cza.tip_ent_des,
                          g_cza.cve_ent_des,
                          g_cza.fecha_lote,
                          g_cza.consec_dia,
                          g_cza.mod_archivo,
                          g_cza.result_oper,
                          g_cza.diag1,
                          g_cza.diag2,
                          g_cza.diag3,
                          110 SPACES  
END REPORT

FUNCTION Genera_detalle()
define vfolio integer

   LET vconta = 0
   LET vsalida = g_param.ruta_envio CLIPPED,"/det_incons"

   LET g_det.tip_reg     = "02"
   LET g_det.ident_servi = "03"
   LET g_det.cve_afore   = vcodigo_afore USING '###'


   LET g_det.result_oper = "  "
   LET g_det.diag1       = "   "
   LET g_det.diag2       = "   "
   LET g_det.diag3       = "   "
 
   DECLARE cur_detalle CURSOR FOR
   SELECT h.reg_patronal_imss,
          h.rfc_patron,
          h.n_seguro,
          h.n_rfc,
          h.nom_trabajador,
          h.periodo_pago,
          m.paterno,
          m.materno,
          m.nombres,h.folio
     FROM dis_cza_aporte c,dis_det_aporte h,afi_mae_afiliado m
    WHERE c.fech_creac_lote BETWEEN xfecha_desde AND xfecha_hasta
      AND h.folio = c.folio
      AND h.n_seguro = m.n_seguro
		AND m.tipo_solicitud <> 5


   START REPORT rep_detalle TO vsalida
      FOREACH cur_detalle INTO vreg_pat_ims,
                               vrfc_pat,
                               vnss,
                               vrfc_tra,
                               vnombre,
                               vperiodo_pag,
                               vpaterno,
                               vmaterno,
                               vnombres,vfolio
         LET vpaterno = vpaterno CLIPPED
         LET vmaterno = vmaterno CLIPPED
         LET vnombres = vnombres CLIPPED

         let vconta = vconta + 1
         display "Contador ",vconta at 15,10
  insert into safre_tmp:dis_numero values(vconta,vnss,vnombre,vfolio)

         OUTPUT TO REPORT rep_detalle(vreg_pat_ims,vrfc_pat,vnss,vrfc_tra,vnombre,vperiodo_pag,vpaterno,vmaterno,vnombres,g_det.*)
      END FOREACH
   FINISH REPORT rep_detalle

END FUNCTION

REPORT rep_detalle(vreg_pat_ims,vrfc_pat,vnss,vrfc_tra,vnombre,vperiodo_pag,vpaterno,vmaterno,vnombres,g_det)
   DEFINE
      vreg_pat_ims LIKE dis_det_aporte.reg_patronal_imss,
      vrfc_pat     LIKE dis_det_aporte.rfc_patron,
      vnss         LIKE dis_det_aporte.n_seguro,
      vrfc_tra     LIKE dis_det_aporte.n_rfc,
      vnombre      LIKE dis_det_aporte.nom_trabajador,
      vperiodo_pag LIKE dis_det_aporte.periodo_pago,
      vpaterno     LIKE afi_mae_afiliado.paterno,
      vmaterno     LIKE afi_mae_afiliado.materno,
      vnombres     LIKE afi_mae_afiliado.nombres,
      xpaterno     LIKE afi_mae_afiliado.paterno,
      xmaterno     LIKE afi_mae_afiliado.materno,
      xnombres     LIKE afi_mae_afiliado.nombres,

   g_det RECORD
      tip_reg     CHAR(02), 
      ident_servi CHAR(02),
      cve_afore   CHAR(03),
      reg_pat_ims CHAR(11),
      rfc_pat     CHAR(13),
      nss         CHAR(11),
      rfc_tra     CHAR(13), 
      nombre      CHAR(50),
      inconsisten CHAR(02),
      periodo_pag CHAR(08),
      result_oper CHAR(02),
      diag1       CHAR(03),
      diag2       CHAR(03),
      diag3       CHAR(03)
   END RECORD,

   vimprime CHAR(01)

   OUTPUT
      TOP MARGIN 0
      LEFT MARGIN 0
      BOTTOM MARGIN 0
      PAGE LENGTH 1   
   FORMAT
      ON EVERY ROW

         IF vnombre = " " OR vnombre IS NULL THEN
            LET xpaterno = " "
            LET xmaterno = " "
            LET xnombres = " "
         ELSE
            CALL Separa_nombre() RETURNING xpaterno,xmaterno,xnombres
         END IF


         LET vimprime = "N"

         IF xnombres <> vnombres AND 
            xpaterno = vpaterno  AND
            xmaterno = vmaterno THEN
            LET g_det.inconsisten = "01"
            LET vimprime = "S"
         END IF
         IF xpaterno <> vpaterno AND
            xmaterno = vmaterno  AND
            xnombres = vnombres THEN
            LET g_det.inconsisten = "02"
            LET vimprime = "S"
         END IF
         IF xmaterno <> vmaterno AND
            xnombres = vnombres  AND
            xpaterno = vpaterno  THEN
            LET g_det.inconsisten = "03"
            LET vimprime = "S"
         END IF
         IF xnombres <> vnombres AND 
            xpaterno <> vpaterno AND
            xmaterno = vmaterno THEN
            LET g_det.inconsisten = "04"
            LET vimprime = "S"
         END IF
         IF xnombres <> vnombres AND 
            xpaterno =  vpaterno AND
            xmaterno <> vmaterno THEN
            LET g_det.inconsisten = "05"
            LET vimprime = "S"
         END IF
         IF xnombres =  vnombres AND 
            xpaterno <> vpaterno AND
            xmaterno <> vmaterno THEN
            LET g_det.inconsisten = "06"
            LET vimprime = "S"
         END IF
         IF xnombres <> vnombres AND 
            xpaterno <> vpaterno AND
            xmaterno <> vmaterno THEN
            LET g_det.inconsisten = "07"
            LET vimprime = "S"
         END IF

         IF vimprime = "S" THEN
            LET vcontador = vcontador + 1
            LET g_det.reg_pat_ims = vreg_pat_ims
            LET g_det.rfc_pat     = vrfc_pat
            LET g_det.nss         = vnss
            LET g_det.rfc_tra     = vrfc_tra
            LET g_det.periodo_pag = vperiodo_pag 

            LET g_det.nombre = vpaterno CLIPPED," ",
                               vmaterno CLIPPED," ",
                               vnombres CLIPPED
            PRINT COLUMN 1,
               g_det.tip_reg,
               g_det.ident_servi,
               g_det.cve_afore,
               g_det.reg_pat_ims,
               g_det.rfc_pat,
               g_det.nss,
               g_det.rfc_tra,
               g_det.nombre,
               g_det.inconsisten,
               g_det.periodo_pag,
               g_det.result_oper,
               g_det.diag1,
               g_det.diag2,
               g_det.diag3,
               24 SPACES
         END IF

END REPORT  

FUNCTION Separa_nombre()
   DEFINE
      tpaterno,
      tmaterno,
      tnombres CHAR(40),
      longitud SMALLINT,
      i,
      algo,
      pos      integer,
      pos2     integer,
      pos3     integer, 
      opc      CHAR(1),
      nom1     CHAR(40),
      nom2     CHAR(40),
      vinicio  integer
      
   LET algo = 0
   LET pos = 0

   LET longitud = LENGTH(vnombre)

   IF vnombre[1] ="$" AND vnombre[2]="$" THEN
      LET tpaterno = NULL
      LET tmaterno = NULL
      LET tnombres = NULL
   ELSE
      IF vnombre[1] = "$" AND vnombre[2] <> "$" THEN
	 LET vinicio = 2
      ELSE
	 LET vinicio = 1
      END IF

      IF vnombre[longitud]="$" AND vnombre[longitud-1]="$" THEN 
	 LET longitud = longitud - 2
      END IF

      IF vnombre[longitud]="$" THEN 
	 LET longitud = longitud - 1
      END IF

      FOR i=vinicio TO longitud

--- let g_reg[i].nombre = vnombre[i]
--- display "g_reg.nombre ",g_reg[i].nombre

         IF vnombre[i] = "$" OR vnombre[i] = " " OR vnombre[i] = "/" then



            IF (i-1) = 0 THEN
               LET i = 2
            END IF

            IF algo=0 then
               LET tpaterno = vnombre[vinicio,i-1]
               -------LET tpaterno = vnombre[1,i-1]
               LET pos = i+1
               LET algo = 1
            ELSE
               IF (vnombre[i] ="$" AND vnombre[i-1]="$") OR
                  (vnombre[i] ="/" AND vnombre[i-1]="/") OR
                  (vnombre[i] =" " AND vnombre[i-1]=" ") THEN 
                  LET tmaterno = ""
                  LET algo = 0
                  LET pos = i+1
               ELSE 
                  IF pos = i THEN
                     LET pos2 = pos
                  ELSE
                     LET pos2 = i-1
                  END IF

                ----  LET tmaterno = vnombre[pos,i-1]  
                  LET tmaterno = vnombre[pos,pos2]  
                  LET algo = 0
                  LET pos = i+1
               END IF
            END IF
         END IF
      END FOR

      LET nom2 = vnombre[i-1]

      IF nom1 = " " THEN
         LET i = i + 1
      END IF

---display "pos ",pos
---display "i   ",i
---prompt '' for opc 

      IF pos = i THEN
         LET pos3 = pos
      ELSE
         LET pos3 = i-1
      END IF

----      LET tnombres = vnombre[pos,i-1]
      LET tnombres = vnombre[pos,pos3]

      LET nom1 = vnombre[pos]
   END IF

   RETURN tpaterno,tmaterno,tnombres

END FUNCTION

FUNCTION Genera_sumario()
   LET vsalida = g_param.ruta_envio CLIPPED,"/sum_incons"

   START REPORT rep_sumario TO vsalida
      OUTPUT TO REPORT rep_sumario()
   FINISH REPORT rep_sumario
END FUNCTION

REPORT rep_sumario()
   DEFINE
      g_sum RECORD
         tip_reg     CHAR(02),
         ident_servi CHAR(02),
         ident_opera CHAR(02), 
         tip_ent_ori CHAR(02),
         cve_ent_ori CHAR(03),
         tip_ent_des CHAR(02),
         cve_ent_des CHAR(03),
         fecha_lote  CHAR(08),
         consec_dia  CHAR(03),
         tot_reg     CHAR(08)
      END RECORD
   OUTPUT
      TOP MARGIN  0
      LEFT MARGIN 0
      BOTTOM MARGIN 0
      PAGE LENGTH 1
   FORMAT 
      ON EVERY ROW
         LET g_sum.tip_reg     = "09"
         LET g_sum.ident_servi = "03"
         LET g_sum.ident_opera = "20"
         LET g_sum.tip_ent_ori = "01"
         LET g_sum.cve_ent_ori = vcodigo_afore USING '###'
         LET g_sum.tip_ent_des = "03"
         LET g_sum.cve_ent_des = "001"
         LET g_sum.fecha_lote  = hoy  USING 'YYYYMMDD'
         LET g_sum.consec_dia  = "001"
         LET g_sum.tot_reg     = vcontador

         PRINT COLUMN 01, g_sum.tip_reg,
                          g_sum.ident_servi,
                          g_sum.ident_opera,
                          g_sum.tip_ent_ori,
                          g_sum.cve_ent_ori,
                          g_sum.tip_ent_des,
                          g_sum.cve_ent_des,
                          g_sum.fecha_lote,
                          g_sum.consec_dia,
                          g_sum.tot_reg USING '&&&&&&&&',
                          115 SPACES  
END REPORT

FUNCTION Ultimo_dia(mes)  -- Regresa ultimo dia mes 
   DEFINE
      mes SMALLINT

   CASE mes
      WHEN 1  RETURN 31
      WHEN 2  
         IF YEAR(hoy) MOD 4 = 0 THEN
            RETURN 29
         ELSE
            RETURN 28 
         END IF
      WHEN 3  RETURN 31
      WHEN 4  RETURN 30
      WHEN 5  RETURN 31
      WHEN 6  RETURN 30
      WHEN 7  RETURN 31
      WHEN 8  RETURN 31
      WHEN 9  RETURN 30
      WHEN 10 RETURN 31
      WHEN 11 RETURN 30
      WHEN 12 RETURN 31
   END CASE
END FUNCTION
