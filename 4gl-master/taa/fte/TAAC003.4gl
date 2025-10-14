#############################################################################
#Proyecto          => AFORE ( MEXICO )                                      #
#Propietario       => E.F.P.                                                #
#Programa TAAC003  => RECIBE SOLICITUDES NO ATENDIDAS, AFORE RECEPTORA      #
#Autor             => MAURO MUNIZ CABALLERO                                 #
#Fecha creacion    => 31 DE ENERO DE 2001                                   #
#Sistema           => TRA                                                   #
#Modificacion      => DMR 27/Abr/2011  NUEVO LAYOUT Fecha emision 01/ABR/11 #
#Req:1402          => JCPV 11/06/2012                                       #
#############################################################################
DATABASE safre_af

GLOBALS
   DEFINE reg_cza_no_aten RECORD LIKE taa_cza_no_aten.*
   DEFINE reg_det_no_aten RECORD LIKE taa_det_no_aten.*
   DEFINE reg_sum_no_aten RECORD LIKE taa_sum_no_aten.*
   DEFINE g_param_taa     RECORD LIKE seg_modulo.* 
   DEFINE i               RECORD LIKE tab_afore_local.*      
   DEFINE reg_carta       RECORD LIKE int_ctr_carta.*

   DEFINE enter           CHAR(1)
   DEFINE g_usuario       CHAR(8)
   DEFINE generar         CHAR(20) 
   DEFINE archivo         CHAR(100) 
   DEFINE HOY             DATE         
   DEFINE emite           DATE         
   DEFINE fecha_ver       DATE         
   DEFINE fecha_pre       DATE         
   DEFINE cont_reg        SMALLINT
   DEFINE cuantos         SMALLINT
   DEFINE actual          SMALLINT
   DEFINE caduco          SMALLINT
   DEFINE consulta_carta  CHAR(120)
   DEFINE tipo_solic      SMALLINT
   DEFINE preg_impre      CHAR(1)
   DEFINE rcont_reg       SMALLINT

   DEFINE g RECORD
        nss               CHAR(11),
        curp              CHAR(18),
        tipo_traspaso     CHAR(02),
        afore             CHAR(03),
        rech_proc_operat  CHAR(03),
        f_recep_sol       DATE
   END RECORD
END GLOBALS


MAIN
   OPTIONS INPUT WRAP,
           PROMPT LINE LAST,
           ACCEPT KEY CONTROL-I
           DEFER INTERRUPT

   CALL STARTLOG("TAAC003.log")
   CALL inicio()            #i
   CALL proceso_principal() #pp

   PROMPT "Desea Imprimir el Reporte [S/N]: " FOR preg_impre

   CALL impresion_reporte() #pp

   PROMPT  "PROCESO FINALIZADO, PRESIONE ENTER PARA SALIR " FOR enter
END MAIN


FUNCTION inicio()
#i---------------

   LET HOY        = TODAY
   LET cont_reg   = 1
   LET preg_impre = 'N'
   LET rcont_reg  = 0

   SELECT *, user
   INTO  g_param_taa.*, g_usuario
   FROM  seg_modulo
   WHERE modulo_cod = 'taa'

   WHENEVER ERROR CONTINUE
      DATABASE safre_tmp

      DROP TABLE tmp_pla_no_aten
   WHENEVER ERROR STOP

   CREATE TABLE tmp_pla_no_aten
    (n_registros CHAR(730)) 

   DATABASE safre_af

   INITIALIZE reg_carta.* TO NULL
END FUNCTION


FUNCTION proceso_principal()
#pp-------------------------

   OPEN WINDOW TAAC0031 AT 4,4 WITH FORM "TAAC0031" ATTRIBUTE(BORDER)
   DISPLAY " TAAC003    RECIBE ARCHIVO DE SOLICITUDES NO ATENDIDAS OP 14                   " AT 3,1 ATTRIBUTE(REVERSE)   
   DISPLAY "                            < CTRL-C > Salir                                   " AT 1,1 ATTRIBUTE(REVERSE) 
   DISPLAY " SAFRE v2.0 (OP 14-1)" AT 2,1
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

   INPUT BY NAME generar WITHOUT DEFAULTS

      AFTER FIELD generar
         IF generar IS NULL THEN
            ERROR "NOMBRE DE ARCHIVO NO PUEDE SER NULO"
            NEXT FIELD generar
         END IF
 
         WHENEVER ERROR CONTINUE
         LET archivo = g_param_taa.ruta_rescate CLIPPED,"/",
                       generar CLIPPED

         LOAD FROM archivo INSERT INTO safre_tmp:tmp_pla_no_aten

         SELECT count(*)
         INTO cuantos
         FROM safre_tmp:tmp_pla_no_aten

         IF cuantos = 0 THEN
            DISPLAY  " NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO "
            AT 19,2 ATTRIBUTE(REVERSE)
            SLEEP 3
            NEXT FIELD generar
         ELSE
            EXIT INPUT
         END IF
         WHENEVER ERROR STOP    
      
      EXIT INPUT

        ON KEY (INTERRUPT)
           ERROR "PROCESO CANCELADO" ATTRIBUTE(REVERSE)
           SLEEP 2
           EXIT PROGRAM
   END INPUT

   DISPLAY "PROCESANDO INFORMACION " AT 19,1 ATTRIBUTE(REVERSE)

   CALL validacion_previa() #vp
   CALL lee_archivo_plano() #lap

   CLOSE WINDOW TAAC0031
END FUNCTION


FUNCTION lee_archivo_plano()
#lap------------------------

   DEFINE
        cont            SMALLINT,
        ident_operacion CHAR(2),
        cfecha_8        CHAR(8),
        cfecha_10       CHAR(10),
        cfecha_sol      CHAR(8),
        cfecha_sol1     CHAR(10),
        carga_reg       CHAR(730),
        cantidad_tt_01  SMALLINT,
        cantidad_tt_02  SMALLINT,
        cantidad_tt_21  SMALLINT,
        cantidad_tt_12  SMALLINT,
        cantidad_tt_51  SMALLINT,
        cantidad_tt_20  SMALLINT,
        cantidad_tt_24  SMALLINT,
        cantidad_tt_25  SMALLINT,
        cantidad_tt_38  SMALLINT,
        cantidad_tt_55  SMALLINT,
        cantidad_tt_57  SMALLINT,
        cantidad_tt_83  SMALLINT,
        cantidad_tt_84  SMALLINT,
        cantidad_tt_85  SMALLINT

   DECLARE cur_1 CURSOR FOR
   SELECT * 
   FROM   safre_tmp:tmp_pla_no_aten
   
   LET cont = 0
   LET ident_operacion = ""

   LET cantidad_tt_01 = 0
   LET cantidad_tt_02 = 0
   LET cantidad_tt_21 = 0
   LET cantidad_tt_12 = 0
   LET cantidad_tt_51 = 0
   LET cantidad_tt_20 = 0
   LET cantidad_tt_24 = 0
   LET cantidad_tt_25 = 0
   LET cantidad_tt_38 = 0
   LET cantidad_tt_55 = 0
   LET cantidad_tt_57 = 0
   LET cantidad_tt_83 = 0
   LET cantidad_tt_84 = 0
   LET cantidad_tt_85 = 0

   FOREACH cur_1 INTO carga_reg
      LET cont = cont + 1

                   #---ENCABEZADO SOLICITUD TRASPASO INDIVIDUAL---#

      IF carga_reg[1,2] = "01" AND carga_reg[5,6] = "14" THEN
         LET ident_operacion = "01"
         LET reg_cza_no_aten.tipo_registro     = carga_reg[001,002] 
         LET reg_cza_no_aten.ident_servicio    = carga_reg[003,004]
         LET reg_cza_no_aten.ident_operacion   = carga_reg[005,006]
         LET cfecha_8                          = carga_reg[017,024]

         LET cfecha_10 = cfecha_8[5,6],"/",cfecha_8[7,8],"/",cfecha_8[1,4]
         LET reg_cza_no_aten.fecha_presentacion = cfecha_10

         SELECT "X"
         FROM  taa_cza_no_aten
         WHERE fecha_presentacion = reg_cza_no_aten.fecha_presentacion

         IF STATUS = NOTFOUND THEN
            INSERT INTO taa_cza_no_aten VALUES(reg_cza_no_aten.*)
         END IF
      END IF

      IF carga_reg[1,2] = "01" AND carga_reg[5,6] <> "14" THEN
         LET ident_operacion = "02"
         PROMPT "EL ARCHIVO NO ES DE SOLICITUDES NO ATENDIDAS, ",
                "Presione [Enter] para salir" FOR enter
         EXIT PROGRAM
      END IF

                   #---DETALLE SOLICITUD TRASPASO INDIVIDUAL---#

      IF carga_reg[1,2] = "02" AND ident_operacion = "01" THEN
         LET reg_det_no_aten.tipo_registro      = carga_reg[001,002] 
         LET reg_det_no_aten.tipo_ced_cuenta    = carga_reg[018,019]
         LET reg_det_no_aten.cve_ced_cuenta     = carga_reg[020,022]
         LET reg_det_no_aten.tipo_traspaso      = carga_reg[023,024]
         LET cfecha_8                           = carga_reG[025,032]
         LET reg_det_no_aten.rech_proc_operat   = carga_reg[033,035]
         LET reg_det_no_aten.curp               = carga_reg[036,053]
         LET reg_det_no_aten.n_seguro           = carga_reg[054,064]
         LET reg_det_no_aten.paterno            = carga_reg[065,104]
         LET reg_det_no_aten.materno            = carga_reg[105,144]
         LET reg_det_no_aten.nombres            = carga_reg[145,184]
--            LET cfecha_sol                         = carga_reg[232,239]

         LET cfecha_10 = cfecha_8[5,6],"/",cfecha_8[7,8],"/",cfecha_8[1,4]
         LET reg_det_no_aten.fecha_presentacion = cfecha_10

--            LET cfecha_sol1 = cfecha_sol[5,6],"/",
--                              cfecha_sol[7,8],"/",
--                              cfecha_sol[1,4]
--            LET reg_det_no_aten.f_recep_sol = cfecha_sol1

         LET reg_det_no_aten.f_actualiza        = HOY
         LET reg_det_no_aten.usuario            = g_usuario
            
         SELECT "X"
         FROM  taa_det_no_aten
         WHERE n_seguro = reg_det_no_aten.n_seguro
         AND   fecha_presentacion = reg_det_no_aten.fecha_presentacion

         IF STATUS = NOTFOUND THEN
            INSERT INTO taa_det_no_aten VALUES(reg_det_no_aten.*)
---- 669 ---->            
           ELSE
            SELECT "X"
            FROM   taa_det_no_aten
            WHERE  curp = reg_det_no_aten.curp
            AND    fecha_presentacion = reg_det_no_aten.fecha_presentacion
            IF STATUS = NOTFOUND THEN
              INSERT INTO taa_det_no_aten VALUES(reg_det_no_aten.*)
            END IF
---- 669 <----            
         END IF

         LET reg_carta.fecha_registro = reg_det_no_aten.fecha_presentacion 
         LET reg_carta.docto_cod      = 30227
         LET reg_carta.opera_cod      = 'N'

         CASE reg_det_no_aten.tipo_traspaso
            WHEN '01' LET tipo_solic = 2
            WHEN '12' LET tipo_solic = 4
            WHEN '51' LET tipo_solic = 1
            WHEN '20' LET tipo_solic = 4
            WHEN '55' LET tipo_solic = 9
            WHEN '71' LET tipo_solic = 15
            WHEN '72' LET tipo_solic = 16
            WHEN '74' LET tipo_solic = 18
         END CASE

         CALL det_carta(tipo_solic) #dc
      END IF

                   #---SUMARIO SOLICITUD TRASPASO INDIVIDUAL---#

      IF carga_reg[1,2] = "09" AND ident_operacion = "01" THEN
         LET ident_operacion = ""
         LET reg_sum_no_aten.tipo_registro      = carga_reg[001,002]
         LET reg_sum_no_aten.cantidad_reg_det   = carga_reg[003,011]
         LET reg_sum_no_aten.fecha_presentacion =
             reg_cza_no_aten.fecha_presentacion

         SELECT "X"
         FROM  taa_sum_no_aten
         WHERE fecha_presentacion = reg_sum_no_aten.fecha_presentacion

         IF STATUS = NOTFOUND THEN
            INSERT INTO taa_sum_no_aten VALUES(reg_sum_no_aten.*)
         END IF

         LET cantidad_tt_01 = reg_sum_no_aten.cantidad_reg_det
--          LET cantidad_tt_02 = carga_reg[012,020]
--          LET cantidad_tt_21 = carga_reg[480,488]
--          LET cantidad_tt_12 = carga_reg[489,497]
--          LET cantidad_tt_51 = carga_reg[498,506]
--          LET cantidad_tt_20 = carga_reg[507,515] 
--          LET cantidad_tt_24 = carga_reg[516,524]
--          LET cantidad_tt_25 = carga_reg[525,533]
--          LET cantidad_tt_38 = carga_reg[534,542]
--          LET cantidad_tt_55 = carga_reg[543,551]
--          LET cantidad_tt_57 = carga_reg[552,560]
            --LET cantidad_tt_83 =
            --LET cantidad_tt_84 =
            --LET cantidad_tt_85 =

         IF cantidad_tt_01 IS NULL OR cantidad_tt_01 <> 0     THEN
            CALL inserta_sum_tt('09','01', cantidad_tt_01,
                                reg_sum_no_aten.fecha_presentacion,
                                HOY, g_usuario)
         END IF

         IF cantidad_tt_02 IS NULL OR cantidad_tt_02 <> 0     THEN
            CALL inserta_sum_tt('09','02', cantidad_tt_02,
                                reg_sum_no_aten.fecha_presentacion,
                                HOY, g_usuario)
         END IF
 
         IF cantidad_tt_21 IS NULL OR cantidad_tt_21 <> 0     THEN
            CALL inserta_sum_tt('09','21', cantidad_tt_21,
                                reg_sum_no_aten.fecha_presentacion,
                                HOY, g_usuario)
         END IF

         IF cantidad_tt_12 IS NULL OR cantidad_tt_12 <> 0     THEN
            CALL inserta_sum_tt('09','12', cantidad_tt_12,
                                reg_sum_no_aten.fecha_presentacion,
                                HOY, g_usuario)
         END IF

         IF cantidad_tt_51 IS NULL OR cantidad_tt_51 <> 0     THEN
            CALL inserta_sum_tt('09','51', cantidad_tt_51,
                                reg_sum_no_aten.fecha_presentacion,
                                HOY, g_usuario)
         END IF

         IF cantidad_tt_20 IS NULL OR cantidad_tt_20 <> 0     THEN
            CALL inserta_sum_tt('09','20', cantidad_tt_20,
                                reg_sum_no_aten.fecha_presentacion,
                                HOY, g_usuario)
         END IF

         IF cantidad_tt_24 IS NULL OR cantidad_tt_24 <> 0     THEN
            CALL inserta_sum_tt('09','24', cantidad_tt_24,
                                reg_sum_no_aten.fecha_presentacion,
                                HOY, g_usuario)
         END IF

         IF cantidad_tt_25 IS NULL OR cantidad_tt_25 <> 0     THEN
            CALL inserta_sum_tt('09','25', cantidad_tt_25,
                                reg_sum_no_aten.fecha_presentacion,
                                HOY, g_usuario)
         END IF

         IF cantidad_tt_38 IS NULL OR cantidad_tt_38 <> 0     THEN
            CALL inserta_sum_tt('09','38', cantidad_tt_38,
                                reg_sum_no_aten.fecha_presentacion,
                                HOY, g_usuario)
         END IF

         IF cantidad_tt_55 IS NULL OR cantidad_tt_55 <> 0     THEN
            CALL inserta_sum_tt('09','55', cantidad_tt_55,
                                reg_sum_no_aten.fecha_presentacion,
                                HOY, g_usuario)
         END IF

         IF cantidad_tt_57 IS NULL OR cantidad_tt_57 <> 0     THEN
            CALL inserta_sum_tt('09','57', cantidad_tt_57,
                                reg_sum_no_aten.fecha_presentacion,
                                HOY, g_usuario)
         END IF
      END IF

      LET cfecha_8       = null
      LET cfecha_10      = null
      LET cfecha_sol     = null
      LET cfecha_sol1    = null

      LET cantidad_tt_01 = 0
      LET cantidad_tt_02 = 0
      LET cantidad_tt_21 = 0
      LET cantidad_tt_12 = 0
      LET cantidad_tt_51 = 0
      LET cantidad_tt_20 = 0
      LET cantidad_tt_24 = 0
      LET cantidad_tt_25 = 0
      LET cantidad_tt_38 = 0
      LET cantidad_tt_55 = 0
      LET cantidad_tt_57 = 0
      LET cantidad_tt_83 = 0
      LET cantidad_tt_84 = 0
      LET cantidad_tt_85 = 0

   END FOREACH
END FUNCTION


FUNCTION inserta_sum_tt(vtipo_registro, vtipo_traspaso, vtot_reg_det,
                        vfecha_presentacion, vf_actualiza, vusuario)
#ist----------------------------------
  
   DEFINE 
      vtipo_registro           CHAR(02),
      vtipo_traspaso           CHAR(02),
      vtot_reg_det             SMALLINT,
      vfecha_presentacion      DATE,
      vf_actualiza             DATE,
      vusuario                 CHAR(08) 

   SELECT "X"
   FROM   taa_sum_no_aten_tt
   WHERE  fecha_presentacion = vfecha_presentacion
   AND    tipo_traspaso      = vtipo_traspaso

   IF STATUS = NOTFOUND THEN
      INSERT INTO taa_sum_no_aten_tt
      VALUES(vtipo_registro,
             vtipo_traspaso,
             vtot_reg_det,
             vfecha_presentacion,
             vf_actualiza,
             vusuario)
   END IF
END FUNCTION


FUNCTION validacion_previa()
#vp------------------------

   DEFINE
      c2_tipo_registro      CHAR(2)

   DEFINE
      sw_1                  ,
      sw_2                  ,
      sw_9                  SMALLINT            

   DECLARE cur_2 CURSOR FOR
   SELECT UNIQUE(n_registros[1,2])
   FROM   safre_tmp:tmp_pla_no_aten

   LET sw_1 = 0
   LET sw_2 = 0
   LET sw_9 = 0

   FOREACH cur_2 INTO c2_tipo_registro
      CASE c2_tipo_registro
         WHEN "01"
            LET sw_1 = 1
         WHEN "02"
            LET sw_2 = 1
         WHEN "09"
            LET sw_9 = 1
      END CASE
   END FOREACH

   IF sw_1 = 0 THEN
      PROMPT "SE RECHAZA EL LOTE. NO EXISTE ENCABEZADO" FOR enter
      EXIT PROGRAM
   END IF

   IF sw_2 = 0 THEN
      PROMPT "SE RECHAZA EL LOTE. NO EXISTEN REGISTROS DE DETALLE" FOR enter
      EXIT PROGRAM
   END IF

   IF sw_9 = 0 THEN
      PROMPT "SE RECHAZA EL LOTE. NO EXISTE SUMARIO" FOR enter
      EXIT PROGRAM
   END IF
END FUNCTION


FUNCTION impresion_reporte()
#ir-------------------------

   DEFINE w_codigo_afore LIKE tab_afore_local.codigo_afore

   DEFINE hora       CHAR(8)
   DEFINE g_usuario  CHAR(8)
   DEFINE G_IMPRE    CHAR(300)
   DEFINE gimpresion CHAR(300)

   LET hora = time
   LET hora = hora[1,2],hora[4,5]

   SELECT codigo_afore,USER
   INTO w_codigo_afore,g_usuario
   FROM tab_afore_local

   SELECT MAX(n.f_actualiza)
   INTO  fecha_ver
   FROM  taa_det_no_aten n
   WHERE n.fecha_presentacion = reg_det_no_aten.fecha_presentacion

   SELECT a.fecha_presentacion
   INTO  fecha_pre
   FROM  taa_det_no_aten a
   WHERE a.fecha_presentacion = reg_det_no_aten.fecha_presentacion
   AND   a.f_actualiza        = fecha_ver
   GROUP BY 1

   LET hora = TIME

   LET G_IMPRE = g_param_taa.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                 ".NO_ATEN_SOL.",hoy USING "DDMMYY", "_",hora[1,2] CLIPPED,
                 hora[4,5] CLIPPED, hora[7,8]

   START REPORT det_dev_sol TO  G_IMPRE
      DECLARE cursor CURSOR FOR
      SELECT a.n_seguro, a.curp, a.tipo_traspaso, a.cve_ced_cuenta,
             a.rech_proc_operat, a.f_recep_sol
      FROM   taa_det_no_aten a
      WHERE  a.f_actualiza = fecha_ver
      ORDER BY 3,1

      FOREACH cursor INTO g.*
         OUTPUT TO REPORT det_dev_sol(g.*)
      END FOREACH 
  
   FINISH REPORT det_dev_sol

   IF preg_impre MATCHES "[Ss]" THEN
      LET gimpresion = "lp ",G_IMPRE
      RUN gimpresion                                   
   END IF
END FUNCTION


REPORT det_dev_sol(g)
#dvsi--------------------

   DEFINE g RECORD
      nss              CHAR(11),
      curp             CHAR(18),
      tipo_traspaso    CHAR(02),
      afore            CHAR(03),
      rech_proc_operat CHAR(03),
      f_recep_sol      DATE
   END RECORD

   DEFINE
      vdescripcion_tt  CHAR(80),
      vdescripcion_afo CHAR(30),
      desc_rechazo_c   CHAR(60),
      total_tt         INTEGER

   DEFINE g1 RECORD
      tipo_traspaso    CHAR(02),
      descripcion      CHAR(80),
      tot_reg_det      SMALLINT
   END RECORD

   OUTPUT
      TOP MARGIN    1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60

   FORMAT
      PAGE HEADER
      SELECT razon_social
      INTO i.razon_social
      FROM tab_afore_local

      PRINT COLUMN  21,i.razon_social,
            COLUMN 126,TODAY USING "dd-mm-yyyy"
      SKIP 1 LINE
      PRINT COLUMN  48,"             TOTAL DE SOLICITUDES NO ATENDIDAS OP 14"
      PRINT COLUMN  48,"               TRASPASOS AFORE - AFORE (RECEPTORA)"
      SKIP 1 LINE
      PRINT COLUMN  38,"ARCHIVO : ", generar,
            COLUMN 100,"FECHA PRESENTACION : ", fecha_pre USING "DD/MM/YYYY"
      PRINT COLUMN  01,
"-------------------------------------------------------------------------------------------------------------------------------------------------------------"
      PRINT COLUMN  05,"ID",
            COLUMN  12,"NSS",
            COLUMN  27,"CURP",
            COLUMN  41,"TIPO TRASPASO",
            COLUMN  60,"A F O R E",
            COLUMN  78,"FECHA SOL",
            COLUMN  91,"RECH PROC OP",
            COLUMN 114,"  D E S C R I P C I O N  "
      PRINT COLUMN  01,
"-------------------------------------------------------------------------------------------------------------------------------------------------------------"

   ON EVERY ROW

      LET rcont_reg = rcont_reg + 1 

      SELECT a.descripcion
      INTO  vdescripcion_tt
      FROM  tab_tipo_traspaso a
      WHERE a.tipo_traspaso = g.tipo_traspaso 
      AND   a.id_opera      = '09'

      IF STATUS = NOTFOUND THEN
         LET vdescripcion_tt = NULL
      END IF

      SELECT a.afore_desc
      INTO  vdescripcion_afo
      FROM  tab_afore a
      WHERE a.afore_cod = g.afore

      IF STATUS = NOTFOUND THEN
         LET vdescripcion_afo = NULL
      END IF

      SELECT a.rdeta_desc_c
      INTO  desc_rechazo_c
      FROM  tab_rdeta a
      WHERE a.modulo_cod = "taa"
      AND   a.rdeta_cod  = g.rech_proc_operat

      IF STATUS = NOTFOUND THEN
         LET desc_rechazo_c = NULL
      END IF

      PRINT COLUMN  01,rcont_reg USING "#####&",
            COLUMN  08,g.nss,
            COLUMN  20,g.curp,
            COLUMN  39,g.tipo_traspaso USING "&&",
            COLUMN  42,vdescripcion_tt[1,15],
            COLUMN  58,g.afore,
            COLUMN  62,vdescripcion_afo[1,15],
            COLUMN  78,g.f_recep_sol,
            COLUMN  96,g.rech_proc_operat,
            COLUMN 104,desc_rechazo_c

   ON LAST ROW
      SKIP 4 LINES
      PRINT COLUMN 2, "Total de registros encontrados: ",
                      rcont_reg USING "<<<<"

      SKIP 2 LINES
      PRINT COLUMN 02, "TOTAL DE REGISTROS POR TIPO DE TRASPASO"
      PRINT
      PRINT COLUMN 02, "TIPO TRASPASO",
            COLUMN 23, "TOTAL"

--      DECLARE cursor1 CURSOR FOR
--      SELECT a.tipo_traspaso, b.descripcion, a.tot_reg_det
--      FROM  taa_sum_no_aten_tt a, tab_tipo_traspaso b
--      WHERE a.f_actualiza   = fecha_ver
--      AND   a.tipo_traspaso = b.tipo_traspaso
--      AND   b.id_opera      = '09'

      DECLARE cursor1 CURSOR FOR
      SELECT a.tipo_traspaso, b.descripcion, count(*)
      FROM  taa_det_no_aten    a, tab_tipo_traspaso b
      WHERE a.f_actualiza   = today
      AND   a.tipo_traspaso = b.tipo_traspaso
      AND   b.id_opera      = '09'
      GROUP BY 1,2
      ORDER BY 1

      FOREACH cursor1 INTO g1.*

         PRINT COLUMN 02,g1.tipo_traspaso,
               COLUMN 06,g1.descripcion[1,15],
               COLUMN 21,g1.tot_reg_det USING "###,##&"

         LET total_tt = total_tt + g1.tot_reg_det

       END FOREACH

       PRINT
       PRINT COLUMN 02,"SUMATORIA",
             COLUMN 21,total_tt USING "###,##&" 

   PAGE TRAILER
      SKIP 2 LINE
      PRINT COLUMN 60,"Pagina:",PAGENO USING "<<<<"
      PAUSE "Presione enter para continuar...."

END REPORT


####################################################################
FUNCTION det_carta(tipo_sol) #dc

   DEFINE fent     DATE
   DEFINE tipo_sol SMALLINT

   SELECT @tipo_solicitud, max(@fentcons)
   INTO  reg_carta.tipo_solicitud, fent
   FROM  afi_solicitud
   WHERE @n_seguro = reg_det_no_aten.n_seguro
   AND   @status_interno in (50,65,70)
   AND   @tipo_solicitud = tipo_sol
   GROUP BY 1

   SELECT @n_folio
   INTO  reg_carta.n_folio
   FROM  afi_solicitud
   WHERE @n_seguro = reg_det_no_aten.n_seguro
   AND   @status_interno in (50,65,70)
   AND   @tipo_solicitud = tipo_sol
   AND   @fentcons       = fent

   LET reg_carta.nss		= reg_det_no_aten.n_seguro
   LET reg_carta.opera_cod	= 'N'
   LET reg_carta.edo_genera 	= 10
   LET reg_carta.fecha_genera	= TODAY
   LET reg_carta.hora_genera	= TIME
   LET reg_carta.lote_genera	= 0
   LET reg_carta.consecutivo  	= 0
   LET reg_carta.id_sepomex 	= 0

   LET consulta_carta = "EXECUTE PROCEDURE inserta_carta (?,?,?,?,?,?,?,?,",
			"?,?,?,?)"

   PREPARE sql_exe FROM consulta_carta
   EXECUTE sql_exe USING reg_carta.*

   INITIALIZE reg_carta.* TO NULL

END FUNCTION
####################################################################

