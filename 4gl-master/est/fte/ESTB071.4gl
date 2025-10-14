###############################################################################	
#Proyecto	       => SISTEMA DE safre_af ( MEXICO )
#Owner                 => E.F.P
#Programa  ESTB071     => ESTE PROGRAMA GENERA EL ARCHIVO 1008(PLANO)  
#                         EL CUAL  CONTIENE LA INFORMACION SOBRE LAS
#                         SOLICITUDES DE TRASPASOS --NO ATENDIDAS--.
#Observaciones         => ESTE PROGRAMA ES LLAMADO  POR EL PROGRAMA ESTB070
#                         DEL CUAL RECIBE LOS PARAMETROS DE: FOLIO Y FECHA DE
#                         ENVIO.
#By                    => JESUS DAVID YANEZ MORENO
#Modificado By         => MARCO ANTONIO GONZALEZ ROJAS
#Fecha de Creacion     => 12 DE OCTUBRE   DEL 2004.
#Fecha de Modificacion => 23 DE NOVIEMBRE DEL 2004.
#                      => 30 DE AGOSTO DEL 2005.
#Objetivo de la Mod    => La Modificacion consistio en crear un REPORTE INTERNO
#                         para la Afore.(Para que hicieran sus Validaciones).
#Ultima de Modificacion=> 09 DE DICIEMBRE DEL 2005.
#Objetivo de la Mod    => La Modificacion consistio en dar de alta la Nva
#                         Afore Coppel para que la contemple la Estadistica.
#Sistema               => EST
################################################################################
DATABASE safre_af 
GLOBALS
   DEFINE tot_sie SMALLINT     #nueva mod
   DEFINE g_precio_viv dec(16,6)
   DEFINE  g_band            SMALLINT
   DEFINE  g_num_regs        INTEGER
   DEFINE  g_enter           CHAR(001)
   DEFINE  g_fecha_ini       DATE
   DEFINE  g_fecha_fin       DATE
   DEFINE  g_fecha_viv       DATE
   DEFINE  folio_ced        INTEGER

   DEFINE  g_fecha_envio    DATE 

	DEFINE  g_divisa ARRAY[14,2] OF RECORD
           subcuenta      smallint ,
			  siefore        char(003),
			  acciones       dec(16,6),
			  precio_accion  dec(16,6),
			  pesos          dec(16,2)#ORIGINALMENTE ASI
			##pesos          dec(12,2)#OJO EN REP PLANO DEFINIDO ASI
	END RECORD

	DEFINE  reg_cuenta RECORD 
				tipo_registro           char(003),
				origen_traspaso         char(003),
				fecha_inicio_periodo    char(008),
				fecha_fin_periodo       char(008),
				fecha_alta              char(008),
				n_seguro                char(011),
				nombres                 char(040),
				paterno                 char(040),
				materno                 char(040),
				curp                    char(018),
				rfc                     char(013),
            motivo_rechazo          char(004),
				salario_base            char(014),
				edad                    char(003),
				sexo                    char(001), 
				ident_trabajador        char(001)
	END RECORD
   DEFINE pso                       SMALLINT
   DEFINE g_sql_42                  char(100)			  
   DEFINE   g_reporte_p             char(600)
   DEFINE   g_reporte               char(600)
   DEFINE   g_rep_afo               char(600) 
   DEFINE   g_ruta  RECORD LIKE safre_af:seg_modulo.*
   DEFINE   g_hoy                   DATE
   DEFINE   g_codigo_afore          CHAR(003)
   DEFINE   g_fecha_h               DATETIME YEAR TO SECOND
   DEFINE cad_subcuentas            CHAR(300)
   DEFINE uni_subctas              CHAR(200)
   DEFINE cad_acciones             CHAR(300)
   DEFINE cad_monto                CHAR(300)
   DEFINE s                        smallint
   DEFINE nom_completo             CHAR(120) 
   DEFINE g_usuario                CHAR(010) 
	END GLOBALS

MAIN        

    OPTIONS INPUT WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT

   LET g_fecha_h = CURRENT
   LET folio_ced     = ARG_VAL(1)
   LET g_fecha_envio = ARG_VAL(2)
   CREATE TEMP TABLE orden(s_safre smallint, s_consar smallint)

   CREATE TEMP TABLE cve_sie(codigo_afore        smallint,
                             codigo_siefore      smallint,
                             codigo_siefore_cons smallint)

   INSERT INTO orden VALUES (1,1)
   INSERT INTO orden VALUES (2,8)
   INSERT INTO orden VALUES (3,13)
   INSERT INTO orden VALUES (4,17)
   INSERT INTO orden VALUES (5,10)
   INSERT INTO orden VALUES (7,3)
   INSERT INTO orden VALUES (8,15)
   INSERT INTO orden VALUES (10,12)
   INSERT INTO orden VALUES (11,24)
   INSERT INTO orden VALUES (12,23)
   INSERT INTO orden VALUES (13,21)
   INSERT INTO orden VALUES (14,22)

   INSERT INTO cve_sie VALUES(516,1,516)
   INSERT INTO cve_sie VALUES(532,1,532)
   INSERT INTO cve_sie VALUES(544,1,544)
   INSERT INTO cve_sie VALUES(556,1,556)
   INSERT INTO cve_sie VALUES(558,1,558)
   INSERT INTO cve_sie VALUES(562,1,562)
   INSERT INTO cve_sie VALUES(564,1,564)
   INSERT INTO cve_sie VALUES(568,1,568)
   INSERT INTO cve_sie VALUES(590,1,590)

   INSERT INTO cve_sie VALUES(516,2,002)
   INSERT INTO cve_sie VALUES(532,2,532)
   INSERT INTO cve_sie VALUES(544,2,002)
   INSERT INTO cve_sie VALUES(556,2,002)
   INSERT INTO cve_sie VALUES(558,2,002)
   INSERT INTO cve_sie VALUES(562,2,002)
   INSERT INTO cve_sie VALUES(564,2,002)
   INSERT INTO cve_sie VALUES(568,2,002)
   INSERT INTO cve_sie VALUES(590,2,002)

   DISPLAY " "
   DISPLAY "PROGRAMA: ESTB070 "
   DISPLAY "FOLIO   : ",folio_ced
   DISPLAY g_fecha_h,"INICIANDO PROCESO..."

   LET      g_band = 0
   LET      g_num_regs = 0
   LET      g_hoy = TODAY
   LET      g_sql_42  = 
            'EXECUTE FUNCTION fn_saldo_dia (?,?,?,?) '
 
   PREPARE sql_42 FROM g_sql_42
 
   SELECT a.* 
   INTO   g_ruta.*
   FROM   safre_af:seg_modulo a
   WHERE  a.modulo_cod = 'est'

   SELECT a.codigo_afore
   INTO   g_codigo_afore
   FROM   tab_afore_local a

   SELECT USER
   INTO   g_usuario
   FROM   safre_af:tab_afore_local

   CALL proceso_principal()
END MAIN

FUNCTION proceso_principal()
#pp-------------------------
    DEFINE l_txt3 CHAR(1000)

    SELECT A.fecha_liquidacion
    INTO   g_fecha_fin  
    FROM   taa_cd_ctr_folio A
    WHERE  A.folio in (folio_ced)
    AND    A.tipo_traspaso = 1
    AND    A.estado = 103

    LET g_fecha_viv = MDY(MONTH(g_fecha_fin),"01",YEAR(g_fecha_fin))

    IF YEAR(g_fecha_fin) < "1900" THEN
       CALL  cal_fecha(g_fecha_envio,2)  RETURNING  g_fecha_fin
       CALL  cal_fecha(g_fecha_fin,12) RETURNING  g_fecha_ini
    ELSE
       CALL  cal_fecha(g_fecha_fin,12) RETURNING  g_fecha_ini
       CALL  cal_fecha_avant(g_fecha_fin,2) RETURNING  g_fecha_envio
    END IF
  
 LET g_reporte_p = g_ruta.ruta_envio CLIPPED,"/",g_fecha_envio USING"YYYYMMDD",
                   "_AF_",g_codigo_afore,"_000.1008_p"

 LET g_reporte = g_ruta.ruta_envio CLIPPED,"/",g_fecha_envio USING"YYYYMMDD",
                  "_AF_",g_codigo_afore,"_000.1008" 

 LET g_rep_afo = g_ruta.ruta_envio CLIPPED,"/",g_usuario CLIPPED,".",                              g_fecha_envio USING"YYYYMMDD",".1008"

    START REPORT rpt_1 TO g_reporte_p
    START REPORT rep_afo TO g_rep_afo

    CALL arma_detalle(301)

    CALL arma_detalle(302)
    
    LET g_fecha_h = CURRENT
    DISPLAY g_fecha_h,"ESTADISTICA GENERADA..."

    IF g_num_regs = 0 THEN
        LET g_band = 1
        OUTPUT TO REPORT rpt_1(reg_cuenta.*,g_divisa[1,1].*)
    END IF

    FINISH REPORT rpt_1
    
    FINISH REPORT rep_afo 

LET g_num_regs = g_num_regs + 1
display g_num_regs
LET l_txt3 = "sed 's/^00000000/000",g_num_regs USING"&&&&&","/g' ",
             g_reporte_p CLIPPED ," > ",g_reporte
RUN l_txt3

LET l_txt3 = 'rm ',g_reporte_p
RUN l_txt3

END FUNCTION

FUNCTION arma_detalle(l_detalle)
--ad1--------------------------
    DEFINE l_motivo        smallint
    DEFINE l_subcta_fn     ,
           l_grupo_fn      smallint
    DEFINE l_txt           CHAR(800)
    DEFINE l_edad          smallint
    DEFINE l_fecha_pago    integer,
           l_folio_dis     integer,
           l_ult_salario_diario dec(16,2),
           l_max_conversion date,
           l_periodo        integer,
           l_actividad      smallint,
           l_precio_del_dia DEC(16,6)

    DEFINE i , j, k      smallint
    DEFINE l_detalle  smallint
    DEFINE reg_taa_cd_det_cedido RECORD LIKE safre_af:taa_cd_det_cedido.*
    DEFINE l_scta  SMALLINT
    DEFINE l_siefore SMALLINT
    DEFINE cve_siefore char(003)
    DEFINE l_monto  DEC(16,6)
    DEFINE l_pesos  DEC(16,2)

    DEFINE l_reg_afi_mae_afiliado RECORD 
           origen    CHAR(003)                          ,
           fentcons  LIKE safre_af:afi_mae_afiliado.fentcons ,
           n_seguro  LIKE safre_af:afi_mae_afiliado.n_seguro ,
           nombres   LIKE safre_af:afi_mae_afiliado.nombres  ,
           paterno   LIKE safre_af:afi_mae_afiliado.paterno  ,
           materno   LIKE safre_af:afi_mae_afiliado.materno  ,
           n_unico   LIKE safre_af:afi_mae_afiliado.n_unico  ,
           n_rfc     LIKE safre_af:afi_mae_afiliado.n_rfc    , 
           fena      LIKE safre_af:afi_mae_afiliado.fena     ,
           sexo      CHAR(001)
    END RECORD


    LET l_txt = 
    ' SELECT CASE WHEN a.tipo_traspaso = "01" THEN "001" ',
                ' WHEN a.tipo_traspaso = "12" THEN "002" ',
                ' WHEN a.tipo_traspaso = "51" THEN "003" ',
                ' WHEN a.tipo_traspaso = "20" THEN "004" ',
           ' END CASE   , ',
           ' b.fentcons , ',
           ' b.n_seguro , ',
           ' b.nombres  , ',
           ' b.paterno  , ',
           ' b.materno  , ',
           ' b.n_unico  , ',
           ' b.n_rfc      , ',
           ' b.fena     , ',
           ' CASE WHEN b.sexo = 1 THEN "H" ',
           '      WHEN b.sexo = 2 THEN "M" ',
           '      ELSE "H"                 ',
           ' END CASE                      ', 
    ' FROM   safre_af:taa_cd_det_cedido a,  ',
           ' safre_af:afi_mae_afiliado  b   ',
    ' WHERE  a.folio =  ? ', 
    ' AND    a.n_seguro = b.n_seguro        '

    IF l_detalle = 301 THEN 
       LET l_txt = l_txt CLIPPED , ' AND a.estado NOT IN (103,106,12) '
    ELSE 
       LET l_txt = l_txt CLIPPED , ' AND a.estado = 106 '
    END IF
    PREPARE qry_1 FROM l_txt
    DECLARE cur_1 CURSOR FOR qry_1

    FOREACH cur_1 USING folio_ced INTO l_reg_afi_mae_afiliado.*

    IF l_detalle = 302 THEN 

       SELECT a.diag_proceso[1,3]
       INTO   l_motivo
       FROM   safre_af:taa_cd_det_02_rch_sal a 
       WHERE  a.folio = folio_ced 
       AND    a.nss   = l_reg_afi_mae_afiliado.n_seguro
      
       IF l_motivo IS NULL THEN
          LET l_motivo = 0 
       ELSE
          IF l_motivo = "538" THEN
             CONTINUE FOREACH
          END IF 
       END IF 

       IF l_motivo = 471 THEN 
         SELECT a.diag_proceso[1,3]
         INTO   l_motivo
         FROM   safre_af:taa_cd_det_05_rch_sal a 
         WHERE  a.folio = folio_ced 
         AND    a.nss   = l_reg_afi_mae_afiliado.n_seguro

       IF l_motivo IS NULL THEN
          LET l_motivo = 0 
       END IF
      END IF
    END IF

    SELECT MAX(a.fech_pago)
    INTO   l_fecha_pago
    FROM   dis_det_aporte a    
    WHERE  a.n_seguro = l_reg_afi_mae_afiliado.n_seguro

    SELECT max(folio)
    INTO   l_folio_dis
    FROM   dis_det_aporte a    
    WHERE  a.n_seguro = l_reg_afi_mae_afiliado.n_seguro
    AND    a.fech_pago = l_fecha_pago


    SELECT max(a.ult_salario_diario)
    INTO   l_ult_salario_diario
    FROM   dis_det_aporte a    
    WHERE  a.folio      = l_folio_dis 
    AND    a.n_seguro   = l_reg_afi_mae_afiliado.n_seguro
    AND    a.fech_pago = l_fecha_pago


    IF l_ult_salario_diario IS NULL THEN
       LET l_ult_salario_diario = 0
    ELSE
       LET l_ult_salario_diario = l_ult_salario_diario / 100
    END IF


    SELECT max(fecha_conversion)
    INTO   l_max_conversion
    FROM   dis_cuenta a
    WHERE  a.nss = l_reg_afi_mae_afiliado.n_seguro
    AND    a.tipo_movimiento = 1

    LET l_periodo = ((g_fecha_fin - l_max_conversion) / 365);

    IF l_periodo = 0 THEN 
       LET l_actividad = 1 
    ELSE 
       LET l_actividad = 0 
    END IF

    IF l_reg_afi_mae_afiliado.fena = '01/01/0001' THEN
       LET l_reg_afi_mae_afiliado.fena= '11/18/1969';
    END IF

    IF l_reg_afi_mae_afiliado.fena IS NULL THEN
       LET l_reg_afi_mae_afiliado.fena = '11/18/1969';
    END IF

    IF l_reg_afi_mae_afiliado.fena = " " THEN
      LET l_reg_afi_mae_afiliado.fena = '11/18/1969';
    END IF

    CALL calcula_edad(l_reg_afi_mae_afiliado.fena,g_fecha_fin)
    RETURNING l_edad

    LET l_subcta_fn = 0
    LET l_grupo_fn  = 0

    CALL inicializa_reg()

    DECLARE  c_saldo_rcv  CURSOR  FOR  sql_42


    FOREACH  c_saldo_rcv  USING   l_reg_afi_mae_afiliado.n_seguro,
                                  l_subcta_fn                      ,
                                  l_grupo_fn                       ,
                                  g_fecha_fin
                          INTO    l_scta,
                                  l_siefore   ,
                                  l_monto     ,
                                  l_pesos

        IF        l_scta                  =  6      OR
                  l_scta                  =  9    THEN
                  LET      l_scta         =  2
        END IF

        IF        l_siefore              <>  2    THEN
                  LET     l_siefore       =  1
        END IF

        LET g_divisa[l_scta,l_siefore].acciones      =  
            g_divisa[l_scta,l_siefore].acciones + l_monto

        LET g_divisa[l_scta,l_siefore].pesos      =  
            g_divisa[l_scta,l_siefore].pesos + l_pesos

   END FOREACH 

LET reg_cuenta.tipo_registro = l_detalle 
LET reg_cuenta.origen_traspaso = l_reg_afi_mae_afiliado.origen
LET reg_cuenta.fecha_inicio_periodo = g_fecha_ini USING"YYYYMMDD"
LET reg_cuenta.fecha_fin_periodo = g_fecha_fin USING"YYYYMMDD"
LET reg_cuenta.fecha_alta = l_reg_afi_mae_afiliado.fentcons USING"YYYYMMDD"
LET reg_cuenta.n_seguro = l_reg_afi_mae_afiliado.n_seguro 
LET reg_cuenta.nombres = l_reg_afi_mae_afiliado.nombres
LET reg_cuenta.paterno = l_reg_afi_mae_afiliado.paterno
LET reg_cuenta.materno = l_reg_afi_mae_afiliado.materno
LET reg_cuenta.curp    = l_reg_afi_mae_afiliado.n_unico
LET reg_cuenta.rfc     = l_reg_afi_mae_afiliado.n_rfc 
LET reg_cuenta.salario_base = l_ult_salario_diario * 100 USING"&&&&&&&&&&&&&&"
LET reg_cuenta.edad    = l_edad USING"&&&" 
LET reg_cuenta.sexo    = l_reg_afi_mae_afiliado.sexo      
LET reg_cuenta.ident_trabajador =   l_actividad
LET reg_cuenta.motivo_rechazo = l_motivo USING"&&&&"

LET nom_completo = reg_cuenta.nombres CLIPPED," ",reg_cuenta.paterno CLIPPED," ",reg_cuenta.materno CLIPPED

LET tot_sie = 0
LET i = 0
LET j = 0
LET k = 0

DECLARE cur_orden CURSOR FOR  
SELECT a.s_safre,a.s_consar
FROM   orden a
ORDER BY a.s_consar 

SELECT count(unique a.siefore)
INTO tot_sie 
FROM dis_cuenta a
WHERE a.nss = reg_cuenta.n_seguro
AND   a.fecha_conversion <= g_fecha_fin
AND   a.siefore in (1,2)
AND   a.subcuenta not in (3,10,12,15)

CASE tot_sie 
WHEN 2
FOREACH cur_orden INTO i,k 
    FOR j = 1 TO 2
       IF (i <> 6 AND 
           i <> 9 ) THEN
        IF (i = 3 OR 
            i= 10 OR 
            i= 4  OR 
            i= 8  OR 
            i= 14) THEN
          IF j = 2 THEN 
             EXIT FOR
          END IF 
        END IF 
        OUTPUT TO REPORT rpt_1(reg_cuenta.*,g_divisa[i,j].*)
       END IF
    END FOR
END FOREACH
EXIT CASE
OTHERWISE

  LET pso = 0

  SELECT unique a.siefore 
  INTO   pso
  FROM   dis_cuenta a
  WHERE  a.nss = reg_cuenta.n_seguro 
  AND    a.siefore in (1,2)
  AND    a.subcuenta not in (3,10,12,15)
  AND    a.fecha_conversion <= g_fecha_fin

  IF (pso = 0 OR 
      pso IS NULL) THEN
      LET pso = 1
  END IF 

  FOREACH cur_orden INTO i,k 
       IF (i <> 6 AND 
           i <> 9 ) THEN
          IF (i = 4 OR 
             i = 8 OR 
             i = 14) THEN
               LET j = 1 
          ELSE 
               LET j = pso
          END IF
        OUTPUT TO REPORT rpt_1(reg_cuenta.*,g_divisa[i,j].*)
       END IF
  END FOREACH
EXIT CASE
END CASE

CALL inf_rep_afo()

END FOREACH

END FUNCTION
	

REPORT rpt_1(reg_cuenta,g_divisa_r)
#r1------------------------------

	DEFINE  reg_cuenta RECORD 
				tipo_registro           char(003),
				origen_traspaso         char(003),
				fecha_inicio_periodo    char(008),
				fecha_fin_periodo       char(008),
				fecha_alta              char(008),
				n_seguro                char(011),
				nombres                 char(040),
				paterno                 char(040),
				materno                 char(040),
				curp                    char(018),
				rfc                     char(013),
            motivo_rechazo          char(004),
				salario_base            char(014),
				edad                    char(003),
				sexo                    char(001), 
				ident_trabajador        char(001)
	END RECORD

	DEFINE g_divisa_r RECORD
           subcuenta      smallint ,
			  siefore        char(003),
			  acciones       dec(16,6),
			  precio_accion  dec(16,6),
			  pesos          dec(12,2)
	END RECORD

    OUTPUT
   PAGE LENGTH 100000
	LEFT MARGIN 0
	RIGHT MARGIN 0
	TOP MARGIN 0
	BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER 
        

    PRINT
    COLUMN 001,"000",
    COLUMN 004,g_num_regs USING"&&&&&",
    COLUMN 009,"269",
    COLUMN 012,"1008",
    COLUMN 016,"001",
    COLUMN 019,g_codigo_afore,
    COLUMN 022,g_fecha_envio USING"YYYYMMDD",
    COLUMN 030,240 SPACES

   ON EVERY ROW
   IF g_divisa_r.pesos IS NULL THEN LET g_divisa_r.pesos = 0 END IF
  IF g_band = 0 THEN
  --LET g_num_regs = g_num_regs + 1
  IF reg_cuenta.tipo_registro = 301 THEN  

     IF (g_divisa_r.subcuenta = 15 OR     #subcuentas consar viv92 y viv97
         g_divisa_r.subcuenta = 17) THEN
        IF ( g_divisa_r.siefore = g_codigo_afore ) THEN
           #15,17 de la Siefore 2 solo hay datos para la Siefore 1 de estas
           #subcuentas.
           LET g_num_regs = g_num_regs + 1

           PRINT
 	      COLUMN 001,reg_cuenta.tipo_registro             ,     
	      COLUMN 004,reg_cuenta.origen_traspaso           ,
	      COLUMN 007,reg_cuenta.fecha_inicio_periodo      ,
	      COLUMN 015,reg_cuenta.fecha_fin_periodo         ,
	      COLUMN 023,reg_cuenta.fecha_alta                ,
	      COLUMN 031,reg_cuenta.n_seguro                  ,
	      COLUMN 042,reg_cuenta.nombres                   ,
	      COLUMN 082,reg_cuenta.paterno                   ,
	      COLUMN 122,reg_cuenta.materno                   ,
	      COLUMN 162,reg_cuenta.curp                      ,
	      COLUMN 180,reg_cuenta.rfc                       ,
	      COLUMN 193,reg_cuenta.salario_base              ,
	      COLUMN 207,reg_cuenta.edad                      ,
	      COLUMN 210,reg_cuenta.sexo                      ,              
	      COLUMN 211,reg_cuenta.ident_trabajador          ,
              COLUMN 212,g_divisa_r.subcuenta             USING"&&&",
	      COLUMN 215,"000"            , --g_divisa_r.siefore                   , 
	      COLUMN 218,"0000000000000000", ---g_divisa_r.acciones 
	      COLUMN 234,"000000000000000" , --g_divisa_r.precio_accion * 1000000 USING"&&&&&&&&&&&&&&&" ,
  	      COLUMN 249,g_divisa_r.acciones * 
                         g_precio_viv  * 100 USING"&&&&&&&&&&&&&&", 
	      COLUMN 263,7 SPACES
        END IF
     END IF

     IF (g_divisa_r.subcuenta = 22) THEN  #subcuenta consar fovissste
         IF ( g_divisa_r.siefore = g_codigo_afore ) THEN
            #22 de la Siefore 2 solo hay datos para la Siefore 1 de estas
            #subcuentas.
           LET g_num_regs = g_num_regs + 1

            PRINT
   	       COLUMN 001,reg_cuenta.tipo_registro             ,     
   	       COLUMN 004,reg_cuenta.origen_traspaso           ,
	       COLUMN 007,reg_cuenta.fecha_inicio_periodo      ,
	       COLUMN 015,reg_cuenta.fecha_fin_periodo         ,
	       COLUMN 023,reg_cuenta.fecha_alta                ,
	       COLUMN 031,reg_cuenta.n_seguro                  ,
	       COLUMN 042,reg_cuenta.nombres                   ,
	       COLUMN 082,reg_cuenta.paterno                   ,
	       COLUMN 122,reg_cuenta.materno                   ,
	       COLUMN 162,reg_cuenta.curp                      ,
	       COLUMN 180,reg_cuenta.rfc                       ,
  	       COLUMN 193,reg_cuenta.salario_base              ,
	       COLUMN 207,reg_cuenta.edad                      ,
	       COLUMN 210,reg_cuenta.sexo                      ,              
	       COLUMN 211,reg_cuenta.ident_trabajador          ,
               COLUMN 212,g_divisa_r.subcuenta             USING"&&&",
	       COLUMN 215,"000"    ,         --g_divisa_r.siefore                   , 
	       COLUMN 218,"0000000000000000", ---g_divisa_r.acciones 
	       COLUMN 234,"000000000000000" , --g_divisa_r.precio_accion * 1000000 USING"&&&&&&&&&&&&&&&" ,
	       COLUMN 249,g_divisa_r.pesos * 100 USING "&&&&&&&&&&&&&&",
	       COLUMN 263,7 SPACES

        END IF
     END IF

     IF (g_divisa_r.subcuenta <> 15 AND  #subcuenas consar viv92,viv97,fovissste
         g_divisa_r.subcuenta <> 17 AND 
         g_divisa_r.subcuenta <> 22) THEN

           LET g_num_regs = g_num_regs + 1
        PRINT
	   COLUMN 001,reg_cuenta.tipo_registro             ,     
	   COLUMN 004,reg_cuenta.origen_traspaso           ,
	   COLUMN 007,reg_cuenta.fecha_inicio_periodo      ,
	   COLUMN 015,reg_cuenta.fecha_fin_periodo         ,
	   COLUMN 023,reg_cuenta.fecha_alta                ,
	   COLUMN 031,reg_cuenta.n_seguro                  ,
	   COLUMN 042,reg_cuenta.nombres                   ,
	   COLUMN 082,reg_cuenta.paterno                   ,
	   COLUMN 122,reg_cuenta.materno                   ,
	   COLUMN 162,reg_cuenta.curp                      ,
	   COLUMN 180,reg_cuenta.rfc                       ,
	   COLUMN 193,reg_cuenta.salario_base              ,
	   COLUMN 207,reg_cuenta.edad                      ,
	   COLUMN 210,reg_cuenta.sexo                      ,              
	   COLUMN 211,reg_cuenta.ident_trabajador          ,
           COLUMN 212,g_divisa_r.subcuenta             USING"&&&",
	   COLUMN 215,g_divisa_r.siefore  USING"&&&"       , 
	   COLUMN 218,g_divisa_r.acciones      * 1000000   USING"&&&&&&&&&&&&&&&&",     
	   COLUMN 234,g_divisa_r.precio_accion * 1000000   USING"&&&&&&&&&&&&&&&" ,
	   COLUMN 249,g_divisa_r.acciones * 
                      g_divisa_r.precio_accion * 100 USING"&&&&&&&&&&&&&&", 
	   COLUMN 263,7 SPACES
     END IF

  ELSE 
   IF (g_divisa_r.subcuenta = 15 OR  #subcuentas consar 
      g_divisa_r.subcuenta = 17) THEN
     IF ( g_divisa_r.siefore = g_codigo_afore ) THEN
        #15,17 de la Siefore 2 solo hay datos para la Siefore 1 de estas
        #subcuentas.
           LET g_num_regs = g_num_regs + 1
        PRINT
 	   COLUMN 001,reg_cuenta.tipo_registro             ,     
   	   COLUMN 004,reg_cuenta.origen_traspaso           ,
	   COLUMN 007,reg_cuenta.fecha_inicio_periodo      ,
	   COLUMN 015,reg_cuenta.fecha_fin_periodo         ,
	   COLUMN 023,reg_cuenta.fecha_alta                ,
	   COLUMN 031,reg_cuenta.n_seguro                  ,
    	   COLUMN 042,reg_cuenta.nombres                   ,
	   COLUMN 082,reg_cuenta.paterno                   ,
	   COLUMN 122,reg_cuenta.materno                   ,
   	   COLUMN 162,reg_cuenta.curp                      ,
	   COLUMN 180,reg_cuenta.rfc                       ,
           COLUMN 193,"001"                                ,
           COLUMN 196,reg_cuenta.motivo_rechazo            ,
  	   COLUMN 200,reg_cuenta.salario_base              ,
	   COLUMN 214,reg_cuenta.edad                      ,
	   COLUMN 217,reg_cuenta.sexo                      ,              
	   COLUMN 218,reg_cuenta.ident_trabajador          ,
           COLUMN 219,g_divisa_r.subcuenta             USING"&&&",
   	   COLUMN 222,"000" ,--g_divisa_r.siefore    
	   COLUMN 225,"0000000000000000" ,--g_divisa_r.acciones  * 1000000 USING"&&&&&&&&&&&&&&&&"     
	   COLUMN 241,"000000000000000" , --g_divisa_r.precio_accion * 1000000 USING"&&&&&&&&&&&&&&&" 
	   COLUMN 256,g_divisa_r.acciones * 
                      g_precio_viv    * 100 USING"&&&&&&&&&&&&&&" 
      END IF
   END IF

   IF (g_divisa_r.subcuenta = 22) THEN #subcuenta consar
     IF ( g_divisa_r.siefore = g_codigo_afore ) THEN
        #22 de la Siefore 2 solo hay datos para la Siefore 1 de estas
        #subcuentas.
        LET g_num_regs = g_num_regs + 1
        PRINT
	   COLUMN 001,reg_cuenta.tipo_registro             ,     
   	   COLUMN 004,reg_cuenta.origen_traspaso           ,
	   COLUMN 007,reg_cuenta.fecha_inicio_periodo      ,
	   COLUMN 015,reg_cuenta.fecha_fin_periodo         ,
	   COLUMN 023,reg_cuenta.fecha_alta                ,
	   COLUMN 031,reg_cuenta.n_seguro                  ,
	   COLUMN 042,reg_cuenta.nombres                   ,
	   COLUMN 082,reg_cuenta.paterno                   ,
	   COLUMN 122,reg_cuenta.materno                   ,
	   COLUMN 162,reg_cuenta.curp                      ,
	   COLUMN 180,reg_cuenta.rfc                       ,
           COLUMN 193,"001"                                ,
           COLUMN 196,reg_cuenta.motivo_rechazo            ,
	   COLUMN 200,reg_cuenta.salario_base              ,
	   COLUMN 214,reg_cuenta.edad                      ,
   	   COLUMN 217,reg_cuenta.sexo                      ,              
	   COLUMN 218,reg_cuenta.ident_trabajador          ,
           COLUMN 219,g_divisa_r.subcuenta             USING"&&&",
   	   COLUMN 222,"000",--g_divisa_r.siefore    
   	   COLUMN 225,"0000000000000000" ,--g_divisa_r.acciones  * 1000000 USING"&&&&&&&&&&&&&&&&"     
	   COLUMN 241,"000000000000000" , --g_divisa_r.precio_accion * 1000000 USING"&&&&&&&&&&&&&&&" 
	   COLUMN 256,g_divisa_r.pesos * 100 USING"&&&&&&&&&&&&&&"
       END IF 
   END IF

   IF (g_divisa_r.subcuenta <> 15 AND   #subcuentas consar 
       g_divisa_r.subcuenta <> 17 AND 
       g_divisa_r.subcuenta <> 22) THEN

        LET g_num_regs = g_num_regs + 1
   PRINT
	COLUMN 001,reg_cuenta.tipo_registro             ,     
	COLUMN 004,reg_cuenta.origen_traspaso           ,
	COLUMN 007,reg_cuenta.fecha_inicio_periodo      ,
	COLUMN 015,reg_cuenta.fecha_fin_periodo         ,
	COLUMN 023,reg_cuenta.fecha_alta                ,
	COLUMN 031,reg_cuenta.n_seguro                  ,
	COLUMN 042,reg_cuenta.nombres                   ,
	COLUMN 082,reg_cuenta.paterno                   ,
	COLUMN 122,reg_cuenta.materno                   ,
	COLUMN 162,reg_cuenta.curp                      ,
	COLUMN 180,reg_cuenta.rfc                       ,
        COLUMN 193,"001"                                ,
        COLUMN 196,reg_cuenta.motivo_rechazo            ,
	COLUMN 200,reg_cuenta.salario_base              ,
	COLUMN 214,reg_cuenta.edad                      ,
	COLUMN 217,reg_cuenta.sexo                      ,              
	COLUMN 218,reg_cuenta.ident_trabajador          ,
        COLUMN 219,g_divisa_r.subcuenta       USING"&&&",
	COLUMN 222,g_divisa_r.siefore         USING"&&&",  
	COLUMN 225,g_divisa_r.acciones      * 1000000   USING"&&&&&&&&&&&&&&&&",     
	COLUMN 241,g_divisa_r.precio_accion * 1000000   USING"&&&&&&&&&&&&&&&" ,
	COLUMN 256,g_divisa_r.acciones *  -- marca
        g_divisa_r.precio_accion * 100 USING"&&&&&&&&&&&&&&" 
   END IF

  END IF
  END IF
END REPORT

FUNCTION cal_fecha_avant(x_fecha,ciclo)
#cf-------------------------------

    DEFINE cc         SMALLINT
    DEFINE x_fecha    DATE
    DEFINE ciclo      SMALLINT

    DEFINE sig_fecha    DATE
    DEFINE dia_semana SMALLINT
    DEFINE ant_habil  SMALLINT

    LET sig_fecha  = x_fecha
    LET cc = 1

    WHILE cc <= ciclo 

        LET sig_fecha  = sig_fecha + 1

        LET dia_semana = WEEKDAY(sig_fecha)
        IF dia_semana = 0 OR dia_semana = 6 THEN
           CONTINUE WHILE
        ELSE  
           SELECT "ok" 
           FROM   tab_feriado
           WHERE  feria_fecha = sig_fecha
       
           IF STATUS <> NOTFOUND THEN
              CONTINUE WHILE
           ELSE 
	      LET cc = cc + 1
           END IF	
        END IF
    END WHILE
    RETURN sig_fecha
END FUNCTION

FUNCTION cal_fecha(x_fecha,ciclo)
#cf-------------------------------

    DEFINE cc         SMALLINT
    DEFINE x_fecha    DATE
    DEFINE ciclo      SMALLINT

    DEFINE sig_fecha    DATE
    DEFINE dia_semana SMALLINT
    DEFINE ant_habil  SMALLINT

    LET sig_fecha  = x_fecha
    LET cc = 1

    WHILE cc <= ciclo 

        LET sig_fecha  = sig_fecha - 1

        LET dia_semana = WEEKDAY(sig_fecha)
        IF dia_semana = 0 OR dia_semana = 6 THEN
           CONTINUE WHILE
        ELSE  
           SELECT "ok" 
           FROM   tab_feriado
           WHERE  feria_fecha = sig_fecha
       
           IF STATUS <> NOTFOUND THEN
              CONTINUE WHILE
           ELSE 
	      LET cc = cc + 1
           END IF	
        END IF
    END WHILE
    RETURN sig_fecha
END FUNCTION

FUNCTION inicializa_reg()
#ir----------------------
DEFINE l_txt_1 CHAR(600)
DEFINE i,j,k smallint

   DECLARE cur_ini_orden CURSOR FOR 
   SELECT a.s_safre,a.s_consar
   FROM   orden a
   ORDER BY s_safre

   FOREACH cur_ini_orden INTO i,k
    FOR j = 1 TO 2  
         LET g_divisa[i,j].subcuenta = k 

        LET l_txt_1 = 
        ' SELECT a.codigo_siefore_cons ',
        ' FROM   cve_sie a ', 
        ' WHERE  a.codigo_afore   = ? ',
        ' AND    a.codigo_siefore = ? ' 

        PREPARE qry_2 FROM l_txt_1
        EXECUTE qry_2 USING g_codigo_afore,j INTO g_divisa[i,j].siefore

        LET g_divisa[i,j].acciones = 0

        LET g_divisa[i,j].pesos    = 0

        SELECT a.precio_del_dia
        INTO g_divisa[i,j].precio_accion 
        FROM   glo_valor_accion a
        WHERE  a.fecha_valuacion = g_fecha_fin
        AND    a.codigo_siefore = j

        IF g_divisa[i,j].precio_accion  IS NULL THEN
            LET g_divisa[i,j].precio_accion  = 0
        END IF

    END FOR
   END FOREACH

        SELECT a.precio_del_dia
        INTO g_precio_viv
        FROM   glo_valor_accion a
        WHERE  a.fecha_valuacion = g_fecha_viv
        AND    a.codigo_siefore = 11
 END FUNCTION

FUNCTION calcula_edad(f_fena,f_fliq)

DEFINE       f_fena         ,
             f_fliq         , 
             fecha_control_1 DATE
DEFINE f_edad           SMALLINT
DEFINE y_fena  CHAR(004)
DEFINE y_afliq CHAR(004)

LET f_edad        = 0

LET y_fena  = YEAR(f_fena) 
LET y_afliq = YEAR(f_fliq) 

LET f_edad = y_afliq - y_fena

LET fecha_control_1 = MDY(MONTH(f_fena),
                      DAY(f_fena),
                      YEAR(f_fliq))

    IF fecha_control_1 IS NULL THEN
       LET fecha_control_1 = MDY(MONTH(f_fena),
                             DAY(f_fena - 1 UNITS DAY),
                             YEAR(f_fliq))
    END IF 
    IF  f_fliq < fecha_control_1 THEN
       LET f_edad = f_edad - 1
    END IF

RETURN  f_edad

END FUNCTION

FUNCTION inf_rep_afo()
#ira----------------------

   LET cad_subcuentas = "SUBCUENTAS",'\033(s7B',g_divisa[1,1].subcuenta,                                "          ",
    	        	g_divisa[7,1].subcuenta,"          ",
	                g_divisa[2,1].subcuenta,"          ",
       	        	g_divisa[5,1].subcuenta,"          ",
	 	        g_divisa[10,1].subcuenta,"          ",
		        g_divisa[3,1].subcuenta,"          ",
	    	        g_divisa[8,1].subcuenta,"          ",
		        g_divisa[4,1].subcuenta,"          ",
		        g_divisa[13,1].subcuenta,"          ",
		        g_divisa[14,1].subcuenta,"          ",
		        g_divisa[12,1].subcuenta,"          ",
		        g_divisa[11,1].subcuenta,"          "

   IF ( g_divisa[8,1].subcuenta = 15 OR  #subcuentas consar 
        g_divisa[4,1].subcuenta = 17  ) THEN

      IF ( g_divisa[8,1].pesos IS NULL OR g_divisa[4,1].pesos IS NULL ) THEN
         LET g_divisa[8,1].pesos  = 0
         LET g_divisa[4,1].pesos  = 0
      END IF 
      LET g_divisa[8,1].pesos     = g_divisa[8,1].acciones * g_precio_viv                                         --USING"&&&&&&&&&&&&&&"
      LET g_divisa[4,1].pesos     = g_divisa[4,1].acciones * g_precio_viv                                         --USING"&&&&&&&&&&&&&&"
      IF (reg_cuenta.tipo_registro = 301) THEN
         LET g_divisa[8,1].acciones  = "0000000000000000"
         LET g_divisa[4,1].acciones  = "0000000000000000"
      ELSE 
         LET g_divisa[8,1].acciones  = "000000000000000"
         LET g_divisa[4,1].acciones  = "000000000000000"
      END IF

   END IF

   IF (g_divisa[14,1].subcuenta = 22) THEN #subcuenta consar Fovissste
      IF ( g_divisa[14,1].pesos IS NULL ) THEN
         LET g_divisa[14,1].pesos  = 0
      END IF 
      IF (reg_cuenta.tipo_registro = 301) THEN
         LET g_divisa[14,1].acciones = "0000000000000000"
      ELSE  
         LET g_divisa[14,1].acciones = "000000000000000" 
      END IF
         LET g_divisa[14,1].pesos    = g_divisa[14,1].pesos USING"&&&&&&&&&&&&&&"
   END IF 

   IF (g_divisa[1,1].subcuenta = 1 OR  g_divisa[7,1].subcuenta = 3 OR
       g_divisa[2,1].subcuenta = 8 OR  g_divisa[5,1].subcuenta = 10 OR
       g_divisa[10,1].subcuenta = 12 OR  g_divisa[3,1].subcuenta = 13 OR
       g_divisa[13,1].subcuenta = 21 OR  g_divisa[12,1].subcuenta = 23 OR
       g_divisa[11,1].subcuenta = 24 ) THEN                                  
      IF ( g_divisa[1,1].pesos IS NULL OR g_divisa[7,1].pesos IS NULL OR
           g_divisa[2,1].pesos IS NULL OR g_divisa[5,1].pesos IS NULL OR
           g_divisa[10,1].pesos IS NULL OR g_divisa[3,1].pesos IS NULL OR
           g_divisa[13,1].pesos IS NULL OR g_divisa[12,1].pesos IS NULL OR
           g_divisa[11,1].pesos ) THEN
           LET g_divisa[1,1].pesos   = 0
           LET g_divisa[7,1].pesos   = 0
           LET g_divisa[2,1].pesos   = 0
           LET g_divisa[5,1].pesos   = 0
           LET g_divisa[10,1].pesos  = 0
           LET g_divisa[3,1].pesos   = 0
           LET g_divisa[13,1].pesos  = 0
           LET g_divisa[12,1].pesos  = 0
           LET g_divisa[11,1].pesos  = 0
      END IF 

      LET g_divisa[1,1].acciones = g_divisa[1,1].acciones 
      LET g_divisa[1,1].pesos    = g_divisa[1,1].acciones * g_divisa[1,1].precio_accion

      LET g_divisa[7,1].acciones = g_divisa[7,1].acciones
      LET g_divisa[7,1].pesos    = g_divisa[7,1].acciones * g_divisa[7,1].precio_accion 

      LET g_divisa[2,1].acciones = g_divisa[2,1].acciones 
      LET g_divisa[2,1].pesos    = g_divisa[2,1].acciones * g_divisa[2,1].precio_accion

      LET g_divisa[5,1].acciones = g_divisa[5,1].acciones
      LET g_divisa[5,1].pesos    = g_divisa[5,1].acciones * g_divisa[5,1].precio_accion

      LET g_divisa[10,1].acciones = g_divisa[10,1].acciones
      LET g_divisa[10,1].pesos    = g_divisa[10,1].acciones * g_divisa[10,1].precio_accion

      LET g_divisa[3,1].acciones = g_divisa[3,1].acciones 
      LET g_divisa[3,1].pesos    = g_divisa[3,1].acciones * g_divisa[3,1].precio_accion

      LET g_divisa[13,1].acciones = g_divisa[13,1].acciones
      LET g_divisa[13,1].pesos    = g_divisa[13,1].acciones * g_divisa[13,1].precio_accion

      LET g_divisa[12,1].acciones = g_divisa[12,1].acciones
      LET g_divisa[12,1].pesos    = g_divisa[12,1].acciones * g_divisa[12,1].precio_accion

      LET g_divisa[11,1].acciones = g_divisa[11,1].acciones
      LET g_divisa[11,1].pesos    = g_divisa[11,1].acciones * g_divisa[11,1].precio_accion

   END IF                                       

   LET cad_acciones = "ACCIONES ",g_divisa[1,1].acciones USING"#####&.######","   ",
	      g_divisa[7,1].acciones USING"#####&.######","   ",
	      g_divisa[2,1].acciones USING"#####&.######","   ",
	      g_divisa[5,1].acciones USING"#####&.######","   ",
	      g_divisa[10,1].acciones USING"#####&.######","   ",
	      g_divisa[3,1].acciones USING"#####&.######","   ",
	      g_divisa[8,1].acciones USING"#####&.######","   ",
	      g_divisa[4,1].acciones USING"#####&.######","   ",
	      g_divisa[13,1].acciones USING"#####&.######","   ",
	      g_divisa[14,1].acciones USING"#####&.######","   ",
	      g_divisa[12,1].acciones USING"#####&.######","   ",
	      g_divisa[11,1].acciones USING"#####&.######","   " 

   LET cad_monto = "PESOS    ",g_divisa[1,1].pesos USING"#####&.######","   ",
	      g_divisa[7,1].pesos USING"#####&.######","   ",
	      g_divisa[2,1].pesos USING"#####&.######","   ",
	      g_divisa[5,1].pesos USING"#####&.######","   ",
	      g_divisa[10,1].pesos USING"#####&.######","   ",
	      g_divisa[3,1].pesos USING"#####&.######","   ",
	      g_divisa[8,1].pesos USING"#####&.######","   ",
	      g_divisa[4,1].pesos USING"#####&.######","   ",
	      g_divisa[13,1].pesos USING"#####&.######","   ",
	      g_divisa[14,1].pesos USING"#####&.######","   ",
	      g_divisa[12,1].pesos USING"#####&.######","   ",
	      g_divisa[11,1].pesos USING"#####&.######","   " 
    
   OUTPUT TO REPORT rep_afo ( reg_cuenta.tipo_registro,reg_cuenta.n_seguro,                              nom_completo,cad_subcuentas,cad_acciones,cad_monto )
           
END FUNCTION

REPORT  rep_afo ( r_tipo_registro,r_n_seguro,r_nom_completo,r_cad_subctas,r_cad_acciones,r_cad_monto )
#r2------------------------------
        DEFINE r_tipo_registro           char(003)
        DEFINE r_n_seguro                char(011)
        DEFINE r_nom_completo            char(120)
        DEFINE r_cad_subctas             char(300)
        DEFINE r_cad_acciones            char(300)
        DEFINE r_cad_monto               char(300)

        DEFINE rpt_afore RECORD
               codigo_afore        LIKE safre_af:tab_afore_local.codigo_afore ,
               razon_social        LIKE safre_af:tab_afore_local.razon_social
        END RECORD
        
        DEFINE  r_hoy        date 
        DEFINE  r_nom_rep    char(07)
        DEFINE  tip_det      char(09)    

        OUTPUT
            TOP MARGIN       0
            BOTTOM MARGIN    0
            LEFT MARGIN      0
            RIGHT MARGIN   200 
            PAGE LENGTH     90
            ORDER BY r_tipo_registro,r_n_seguro
 
        FORMAT
         FIRST PAGE HEADER

            PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d\033(s12H'
            PRINT COLUMN 1,'\033e\033(s218T\033(s24H\033(s7B'

             SELECT codigo_afore,
                    razon_social
             INTO   rpt_afore.codigo_afore ,
                    rpt_afore.razon_social
             FROM safre_af:tab_afore_local
       
             LET r_hoy       = TODAY
             LET r_nom_rep = "ESTB070" 

            PRINT COLUMN  002,'__________________________________________________',
                  '__________________________________________________',
                  '__________________________________________________',
                  '__________________________________________________',
                  '_________________________________________'


            PRINT COLUMN 002,"AFORE               : ",rpt_afore.codigo_afore
                                                USING "###","  ",
                                                rpt_afore.razon_social CLIPPED
            SKIP 1 LINE

            PRINT COLUMN 002,"REPORTE SOBRE LAS SOLICITUDES DE TRASPASOS NO ATENDIDAS ANEXO(1008)",
       	          COLUMN 219,"PAGINA              : ",pageno USING "##"

            PRINT COLUMN 002,"PROGRAMA            : ",r_nom_rep
            PRINT COLUMN 002,"FOLIO               : ",folio_ced USING "<<<<<<<<<<"
            PRINT COLUMN 002,"FECHA GENERACION    : ",r_hoy USING "DD-MM-YYYY"
            PRINT COLUMN 002,"FECHA LIQUIDACION   : ",g_fecha_fin USING "DD-MM-YYYY"
            PRINT COLUMN 002,"FECHA ENVIO         : ",g_fecha_envio USING "DD-MM-YYYY"

            PRINT COLUMN 1,'\033e\033(s218T\033(s24H\033(s7B'
           
            PRINT COLUMN  002,'__________________________________________________',
                  '__________________________________________________',
                  '__________________________________________________',
                  '__________________________________________________',
                  '_________________________________________'

         SKIP 1 LINE
    
         BEFORE GROUP OF r_tipo_registro
            IF (r_tipo_registro = 301) THEN 
               PRINT
               PRINT COLUMN 002,'\033(s7B',"DETALLE 1:  SOLICITUDES SIN TRAMITE A NIVEL NSS",'\033(s7B'
               PRINT COLUMN 002,"            _","__________________________________"
               PRINT
            ELSE 
               PRINT
               PRINT COLUMN 002,'\033(s7B',"DETALLE 2:  SOLICITUDES RECHAZADAS A LA ADMINISTRADORA TRANSFERENTE POR LA EMPRESA OPERADORA A NIVEL NSS",'\033(s7B'
            PRINT COLUMN  002,"           "," ______________________________________",
                  '______________________________________________________'
               PRINT
            END IF

        BEFORE GROUP OF r_n_seguro
            
            SKIP 1 LINE
            PRINT COLUMN 002,'\033(s7B',"NSS:   ",'\033(s0B',r_n_seguro,
                          "      ",'\033(s7B',"NOMBRE:   ",'\033(s0B',r_nom_completo

        ON EVERY ROW                                              
           SKIP 1 LINE
           PRINT
                  COLUMN 002,'\033(s7B',r_cad_subctas,'\033(s7B'
           PRINT
                  COLUMN 002,'\033(s7B',r_cad_acciones,'\033(s7B'
           PRINT
                  COLUMN 002,'\033(s7B',r_cad_monto,'\033(s7B'
           PRINT

         AFTER GROUP OF r_tipo_registro      
           IF (r_tipo_registro = 301) THEN
              LET tip_det = "DETALLE 1"
           ELSE
              LET tip_det = "DETALLE 2"
           END IF
         
           PRINT
           PRINT
                  COLUMN 002,'\033(s7B',"TOTAL DE REGISTROS",'\033(s7B'," ",tip_det,": ",GROUP COUNT(*) USING "-----"
           PRINT

         ON LAST ROW

            SKIP 2 LINES
            PRINT 
                   COLUMN 002,'\033(s7B',"TOTAL DE REGISTROS",'\033(s7B'," ",": ",COUNT(*) USING "-----"
            PRINT 
                   
                     
END REPORT


