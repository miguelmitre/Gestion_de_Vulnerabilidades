##############################################################################
#Owner             => E.F.P.
#Programa TRAC075  => GENERA ARCHIVO PLANO INVITACIONES ISSSTE
#Fecha creacion    => 04 DE MAYO DE 1999
#By                => JESUS DAVID YANEZ MORENO
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Fecha de Mod      => 28 DE ENERO DEL 2005
#Ultima Mod        => ENERO DEL 2006.
#Sistema           => TRA-ICE-ISSSTE
##############################################################################
DATABASE safre_af  
GLOBALS

DEFINE vv_tipo_sol SMALLINT
DEFINE paterno  CHAR(040)
DEFINE materno  CHAR(040)
DEFINE nombre   CHAR(040)
DEFINE plano_carta  char(300)
DEFINE plano_carcza char(300)
DEFINE v_edo  smallint
DEFINE v_corr  integer
DEFINE txt_1 CHAR(800)
DEFINE qry CHAR(300)
DEFINE tot_envio INTEGER
DEFINE estado smallint
DEFINE v_format char(030)
DEFINE k integer
DEFINE u integer
DEFINE g_raz_social              LIKE    tab_afore_local.razon_social
DEFINE g_cod_afore               LIKE    tab_afore_local.codigo_afore
DEFINE x_busca CHAR(500)
DEFINE c8_usuario CHAR(008)
DEFINE cont INTEGER
DEFINE cont_asi INTEGER
DEFINE cont_acep INTEGER
DEFINE cont_dev INTEGER
DEFINE cont_dup INTEGER
DEFINE cont1 INTEGER
DEFINE reg_tra_det_atm_issste RECORD LIKE tra_det_atm_issste.*
DEFINE reg_afi_mae_afiliado RECORD LIKE afi_mae_afiliado.*
DEFINE reg_ruta RECORD LIKE seg_modulo.*
DEFINE reg_ruta_int RECORD LIKE seg_modulo.*
DEFINE RUTA CHAR(300)
DEFINE RUTA_F CHAR(300)
DEFINE HOY DATE
DEFINE reg_1 RECORD 
       cve_ced_cuenta CHAR(003),
       asignados      CHAR(001),
       t_lote         INTEGER 
END RECORD

DEFINE enter CHAR(001)
DEFINE v_row INTEGER
DEFINE HORA  CHAR(008)
DEFINE sw    SMALLINT
DEFINE cod_afore     LIKE tab_afore_local.codigo_afore
DEFINE raz_social    LIKE tab_afore_local.razon_social

DEFINE   detalle_1          RECORD
         tipo_carta           SMALLINT,----Agrego
         nss                  CHAR(11),
         rfc                  CHAR(13),
         nombre               CHAR(40),
         paterno              CHAR(40),
         materno              CHAR(40),
         calle                CHAR(40),
         numero               CHAR(10),
         depto                CHAR(10),
         colonia              CHAR(60),
         delegacion           INTEGER,----Agrego
         estado               CHAR(02),
         estado_desc          CHAR(35),
         ciudad               SMALLINT,
         ciudad_desc          CHAR(040),
         codpos               CHAR(05),
         ctro_reparto         INTEGER,----Agrego
         pais                 CHAR(03),
         nacionalidad         CHAR(03),
         fena                 DATE,
         estadon              CHAR(03),
         estadon_desc         CHAR(35),
	 n_unico              CHAR(18),
	 n_folio              DECIMAL(10,0),
	 telefono             CHAR(040),
         tel_trabajo          CHAR(040),----Agrego
	 extension            SMALLINT,
         cod_barras           CHAR(56),----Agrego
         fecha_genera         CHAR(08)----Agrego
END RECORD

DEFINE   dia                  DATE 

DEFINE pfecha_edo DATE

   DEFINE     ctos_02   INTEGER
   DEFINE     ctos_03   INTEGER
END GLOBALS

MAIN
    DEFER INTERRUPT      
    OPTIONS
    ACCEPT KEY CONTROL-I ,
    INPUT WRAP           ,
    PROMPT LINE LAST


   CALL init()

   OPEN WINDOW trac0752  AT 2,2 WITH FORM "TRAC0752" ATTRIBUTE(BORDER)

    MENU "GENERA LOTE DE CARTAS INVITACION ISSSTE"
    COMMAND "Genera" "Genera lote de cartas invitacion "
       CALL init()
       CALL uno()
    COMMAND "Consulta" "Consulta de envios de lotes de carta invitacion"
       CALL consulta_cartas_invi()
    COMMAND "Salir" "Salir del Programa"
          EXIT MENU
    END MENU
END MAIN

FUNCTION uno()
#u------------

   OPEN WINDOW trac0751  AT 2,2 WITH FORM "TRAC0751" ATTRIBUTE(BORDER)
   DISPLAY "TRAC075     GENERA CARTAS INVITACION TRASPASOS ICEFA-AFORE ISSSTE              " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY "                           < Ctrl-C > Salir                                              " AT 1,1 ATTRIBUTE(REVERSE)

   DISPLAY HOY USING"DD-MM-YYYY" AT 3,69 ATTRIBUTE(REVERSE)


  CONSTRUCT BY NAME x_busca ON a.folio_interno      ,
                    a.cve_ced_cuenta     ,
                    a.tipo_criterio      ,
                    a.sar_92_issste      ,
                    a.viv_92_issste      ,
                    a.fecha_edo
        ON KEY (ESC )
            LET INT_FLAG = FALSE
            EXIT CONSTRUCT

        ON KEY (INTERRUPT )
            LET INT_FLAG = FALSE
            RETURN

    END CONSTRUCT
   INPUT BY NAME reg_1.t_lote WITHOUT DEFAULTS

       AFTER FIELD t_lote
  
     WHILE TRUE
       PROMPT "ESTA SEGURO S/N ? " FOR CHAR enter
       IF enter MATCHES "[sSnN]" THEN
        IF enter MATCHES "[sS]" THEN
           EXIT INPUT
        ELSE
         DISPLAY "                                                                               " at 19,1
         DISPLAY"PROCESO CANCELADO " AT 19,2 ATTRIBUTE(REVERSE) SLEEP 2
         SLEEP 2
         DISPLAY "                                                                               " at 19,1
         EXIT PROGRAM 
        END IF
       END IF
     END WHILE

   ON KEY(INTERRUPT)
      PROMPT "PROCESO CANCELADO <ENTER> PARA SALIR..." for char enter
      EXIT PROGRAM

   END INPUT

   DISPLAY "PROCESANDO INFORMACION ...." AT 21,2 ATTRIBUTE(REVERSE)

   CALL primer_paso()

   DISPLAY "ARCHIVO GENERADO EN: ",reg_ruta_int.ruta_envio CLIPPED,
    		   	    	"/30401",
                                HOY USING"DDMMYY",".1" AT 19,2

   PROMPT "PROCESO FINALIZADO...<ENTER> PARA CONCLUIR .."
   FOR char enter

END FUNCTION

FUNCTION init()   
#i-------------
    LET HORA                             =                                 TIME
    LET HOY                              =                                 TODAY
    LET reg_1.t_lote                     =                                 0
    LET reg_1.cve_ced_cuenta             =                                 NULL
    LET reg_1.asignados                  =                                 NULL
    LET cont                             =                                 0
    LET cont_dup                         =                                 0
    LET cont_asi                         =                                 0
    LET cont_acep                        =                                 0
    LET cont_dev                         =                                 0
    LET sw                               =                                 0

    SELECT USER
    into c8_usuario
    from tab_afore_local

    SELECT * 
    INTO   reg_ruta.*
    FROM   seg_modulo   
    WHERE  modulo_cod = "tra"

    SELECT * 
    INTO   reg_ruta_int.*
    FROM   seg_modulo   
    WHERE  modulo_cod = "int"

    LET RUTA     = reg_ruta.ruta_listados CLIPPED,
                   "/",c8_usuario CLIPPED,".TRAC075.",
                   HOY USING"YYYYMMDD",".",HORA CLIPPED

    LET RUTA_F   = reg_ruta.ruta_listados CLIPPED ,
                   "/",c8_usuario CLIPPED,".TRAC075R.",
                   HOY USING"YYYYMMDD",".",HORA CLIPPED

    SELECT  codigo_afore,razon_social
    INTO    g_cod_afore,g_raz_social
    FROM    tab_afore_local

WHENEVER ERROR CONTINUE 
DROP TABLE paso_maestro
WHENEVER ERROR STOP

CREATE TEMP TABLE paso_maestro
  (
    cve_ced_cuenta char(3),
    n_seguro_ent char(11),
    rfc_ent char(13),
    nro_ctrl_icefa char(30),
    curp_ent char(18),
    nombre_ent char(120),
    n_seguro char(11),
    fecha_nacimiento date,
    marca_viv char(1),
    marca_retiro char(1),
    bimestres_acum smallint,
    rfc_patronal char(13),
    rmo_pag_issste char(12),
    nom_patron char(40),
    fecha_ult_apor date,
    sar_92_issste decimal(10,2),
    viv_92_issste decimal(10,2),
    nombre_siri char(40),
    paterno_siri char(40),
    materno_siri char(40),
    tipo_criterio smallint,
    estado smallint,
    fecha_edo date,
    diagnostico smallint,
    folio_interno integer,
    correlativo serial not null ,
    cad_valida char(8),
    liga_correlativo integer,
    fecha_genera date,
    usuario char(10)
  )
END FUNCTION

FUNCTION primer_paso()
#pp------------------

DEFINE ejecuta char(100)

CALL TRAC076()

END FUNCTION

FUNCTION TRAC076()
#ft--------------

   CALL inicio() 
   CALL bd_nssissste()

END FUNCTION

FUNCTION bd_nssissste()
#bdn-------------------
   DEFINE gr_nss_base CHAR(011) 

   LET  dia  = TODAY

   LET qry = "SELECT UNIQUE a.n_seguro,a.estado ",
             " FROM tra_det_atm_issste a ",
             "WHERE  ",x_busca CLIPPED    ,
             " AND    a.estado IN (160,164) "

      LET qry = qry CLIPPED


      PREPARE qry_sql FROM qry

      DECLARE bd_nssissste CURSOR FOR qry_sql

      FOREACH bd_nssissste INTO gr_nss_base,v_edo


      IF reg_1.t_lote > 0 THEN
         IF tot_envio >= reg_1.t_lote THEN
            EXIT FOREACH
         END IF
      END IF

    SELECT "30401",---Agregue
           A.n_seguro,
           A.n_rfc   ,
           A.nombres ,
           A.paterno , 
           A.materno , 
           B.calle   ,
           B.numero  ,
           B.depto   ,
           B.colonia ,
           B.delega  ,---Agregue
           B.estado  ,
           " "       ,
           B.ciudad  ,
           " "       ,
           B.codpos  ,
           " "       ,---Agregue centro_reparto
           B.pais_cod ,
           A.nacionalidad,
           A.fena    ,
           A.estadon ,
           " "       ,
	   A.n_unico ,
	   A.n_folio ,
	   " "       ,
           " "       ,---Agregue telefono Trabajo
	   " "       ,
           " "       ,---Agregue  cod_barras 
           " "       ,---Agregue  Fecha_genera
	   A.tipo_solicitud
    INTO   detalle_1.*,vv_tipo_sol 
    FROM   afi_mae_afiliado A ,  OUTER
           afi_domicilio    B 
    WHERE  A.n_seguro = B.nss
    AND    A.n_seguro = gr_nss_base
    AND    A.n_folio = B.n_folio
    AND    A.tipo_solicitud = B.tipo_solicitud 
    AND    B.marca_envio = "X"
 
---BUSQUEDA DE CENTRO DE REPARTO(X LO MIENTRAS SE RELLENARA CON CEROS)

     SELECT A.centro_reparto 
     INTO   detalle_1.ctro_reparto
     FROM   tab_reparto A
     WHERE  A.codigo_postal = detalle_1.codpos

---FIN DE BUSQUEDA DE CENTRO DE REPARTO


    SELECT A.estad_desc
    INTO   detalle_1.estado_desc
    FROM   tab_estado A
    WHERE  A.estad_cod = detalle_1.estado

    SELECT A.estad_desc
    INTO   detalle_1.estadon_desc
    FROM   tab_estado A
    WHERE  A.estad_cod = detalle_1.estadon

    SELECT A.ciudad_desc
    INTO   detalle_1.ciudad_desc
    FROM   tab_ciudad A
    WHERE  A.estad_cod  = detalle_1.estado
    AND    A.ciudad_cod = detalle_1.ciudad


    SELECT MAX(A.telefono) ,
	   MAX(A.extension)
    INTO   detalle_1.telefono ,
	   detalle_1.extension
    FROM   afi_telefono A
    WHERE  A.nss = gr_nss_base
    AND    A.n_folio = detalle_1.n_folio
    AND    A.tipo_solicitud = vv_tipo_sol

##==============================BUSCA campo (telefono de Trabajo)===============

    SELECT telefono
    INTO   detalle_1.tel_trabajo
    FROM   afi_telefono A
    WHERE  A.nss = gr_nss_base
    AND    A.n_folio = detalle_1.n_folio
    AND    A.tipo_solicitud = vv_tipo_sol
    AND    A.tel_cod = 2
    
    IF (STATUS = NOTFOUND) THEN
       LET detalle_1.tel_trabajo = ' '
    END IF 

##============================FIN DE BUSQUEDA de campo(telefono de Trabajo)=====

##==================INICIA Formateo de Fecha de Generacion======================

    LET detalle_1.fecha_genera = YEAR(dia)  USING "&&&&" CLIPPED,
                                 MONTH(dia) USING "&&"   CLIPPED,
                                 DAY(dia)   USING "&&"   CLIPPED
   
##=================FIN de Formateo de Fecha de Generacion=======================

##=================INICIO DE BUSQUEDA de campo(codigo de Barras)=##=============

  LET detalle_1.cod_barras[1,5]  = detalle_1.tipo_carta USING "&&&&&"  CLIPPED
  LET detalle_1.cod_barras[6,16] = detalle_1.nss               CLIPPED
  LET detalle_1.cod_barras[17,24]= detalle_1.fecha_genera     CLIPPED
  LET detalle_1.cod_barras[25,34]= detalle_1.n_folio USING "##########" 
  LET detalle_1.cod_barras[35]   = "1"                  
  LET detalle_1.cod_barras[36,53]= detalle_1.n_unico             
  LET detalle_1.cod_barras[54,56]= "000"                  
##=================FIN DE BUSQUEDA de campo(codigo de Barras)===================

       LET ctos_02 = ctos_02 + 1

   OUTPUT  TO REPORT rpt_issste( detalle_1.* )

   END FOREACH 

   FINISH REPORT rpt_issste 
#================INICIA REPORTE DE ENCABEZADO===================================

   LET plano_carcza =  reg_ruta.ruta_envio CLIPPED,
                      "/ENCA"

   START REPORT rpt_encab TO plano_carcza

      OUTPUT TO REPORT rpt_encab(ctos_02,tot_envio)

   FINISH REPORT rpt_encab
  

#================FIN DE REPORTE DE CABEZA======================================


   IF tot_envio = 0 THEN
      DISPLAY "NO HAY REGISTROS PARA GENERAR... ",tot_envio AT 17,2
   END IF

  CALL arma_plano()
END FUNCTION

REPORT rpt_issste ( r_detalle1 )
#ri----------------------------------------

DEFINE   r_detalle1          RECORD
         tipo_carta           SMALLINT,----Agrego 
         nss                  CHAR(11),
         rfc                  CHAR(13),
         nombre               CHAR(40),
         paterno              CHAR(40),
         materno              CHAR(40),
         calle                CHAR(40),
         numero               CHAR(10),
         depto                CHAR(10),
         colonia              CHAR(60),
         delegacion           INTEGER,----Agrego
         estado               CHAR(02),
         estado_desc          CHAR(35),
         ciudad               SMALLINT,
         ciudad_desc          CHAR(35),
         codpos               CHAR(05),
         ctro_reparto         INTEGER,----Agrego
         pais                 CHAR(03),
         nacionalidad         CHAR(03),
         fena                 DATE,
         estadon              CHAR(02),
         estadon_desc         CHAR(35) ,
	 n_unico              CHAR(18),
	 n_folio              decimal(10,0),
	 telefono             char(040),
         tel_trabajo          char(040),---Agrego 
	 extension            SMALLINT,
         cod_barras           CHAR(56),---Agrego
         fecha_genera         CHAR(08)---Agrego
  END RECORD

DEFINE   r_detalle2          RECORD
         n_seguro             CHAR(011),
         n_seguro_ent         CHAR(011),
         rfc_ent              CHAR(013),
         cve_ced_cuenta       CHAR(003),
         cve_desc             CHAR(015),
         nro_ctrl_icefa       CHAR(030),
         dependencia          CHAR(080),
         nombre_ent           CHAR(120),
	 sar_92_issste        DECIMAL(16,2),
	 viv_92_issste        DECIMAL(16,2),
	 rfc_patronal         LIKE safre_af:tra_det_atm_issste.rfc_patronal
 END RECORD
 DEFINE v_deleg_desc   CHAR(035)
DEFINE  fec_nac      CHAR(08)
DEFINE   r_folio_carta    INTEGER

   OUTPUT
        PAGE LENGTH   2
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

   FORMAT  

      ON EVERY ROW
#==============FORMATEA FECHA DE NACIMIENTO====================================
         LET fec_nac  = YEAR(r_detalle1.fena) USING "&&&&" CLIPPED,
                        MONTH(r_detalle1.fena) USING "&&" CLIPPED,
                        DAY(r_detalle1.fena) USING "&&"
#==============FIN DE FORMATEO================================================= 

      SELECT a.deleg_desc[1,35]
      INTO   v_deleg_desc
      FROM   safre_af:tab_delegacion a
      WHERE  a.deleg_cod = r_detalle1.delegacion


     INSERT INTO tra_ctr_carta 
     VALUES (r_detalle1.nss ,
	     0              , #serial consecutivo carta
	     1              ) #estado =1 "enviado"

      SELECT MAX(folio_carta) 
      INTO   r_folio_carta 
      FROM   tra_ctr_carta

      PRINT COLUMN 001,"02" USING "&&",
            COLUMN 003,r_detalle1.tipo_carta USING "&&&&&",
            COLUMN 008,r_detalle1.nss USING "&&&&&&&&&&&",
            COLUMN 019,r_detalle1.rfc,
            COLUMN 032,r_detalle1.nombre,
            COLUMN 072,r_detalle1.paterno,
            COLUMN 112,r_detalle1.materno,
            COLUMN 152,r_detalle1.calle,
            COLUMN 192,r_detalle1.numero,
            COLUMN 202,r_detalle1.depto,
            COLUMN 212,r_detalle1.colonia,
            COLUMN 272,v_deleg_desc,
            COLUMN 307,r_detalle1.estado USING "&&",
            COLUMN 309,r_detalle1.estado_desc,
            COLUMN 344,r_detalle1.ciudad USING "&&&&&",
            COLUMN 349,r_detalle1.ciudad_desc,
            COLUMN 384,r_detalle1.codpos,
            COLUMN 389,r_detalle1.ctro_reparto USING "#####",---Se agrego
            COLUMN 394,r_detalle1.pais,
            COLUMN 397,r_detalle1.nacionalidad ,
            COLUMN 400,fec_nac,
            COLUMN 408,r_detalle1.estadon USING "##",
            COLUMN 410,r_detalle1.estadon_desc ,
	    COLUMN 445,r_detalle1.n_unico,
	    COLUMN 463,r_detalle1.n_folio USING "##########",
	    COLUMN 473,r_detalle1.telefono,
            COLUMN 513,r_detalle1.tel_trabajo,---Se agrego
	    COLUMN 553,r_detalle1.extension USING"######",
            COLUMN 559,r_detalle1.cod_barras,---Se agrego
            COLUMN 615,r_detalle1.fecha_genera ,---Se agrego 
	    COLUMN 623,r_folio_carta USING"&&&&&&&&&"

LET txt_1 = 
        '  SELECT a.n_seguro       , ',
        '         a.n_seguro_ent   , ',
        '         a.rfc_ent        , ',
        '         a.cve_ced_cuenta , ',
        '         " "              , ',
        '         a.nro_ctrl_icefa , ',
        '         a.nom_patron  , ',
        '         a.nombre_ent     , ',
        '         a.sar_92_issste  , ',
        '         a.viv_92_issste  , ',
        '         a.rfc_patronal   , ',
        '         a.correlativo    , ',
        '         a.estado           ',
        '  FROM   tra_det_atm_issste a ',
        '  WHERE  ',x_busca CLIPPED ,
        '  AND  a.estado in (160,164) ',    
        '  AND  a.n_seguro = ','"',r_detalle1.nss CLIPPED,'"'

        PREPARE qry2 FROM txt_1 
        DECLARE cur_2 CURSOR FOR qry2

         FOREACH cur_2 INTO r_detalle2.*,v_corr,v_edo

         SELECT a.*    
         INTO   reg_afi_mae_afiliado.*
         FROM   safre_af:afi_mae_afiliado a
         WHERE  a.n_seguro = r_detalle1.nss
      
         LET paterno = r_detalle2.nombre_ent[1,40] 
         LET materno = r_detalle2.nombre_ent[41,80] 
         LET nombre  = r_detalle2.nombre_ent[81,120] 
    

         SELECT A.icefa_desc 
	 INTO   r_detalle2.cve_desc
	 FROM   safre_af:tab_icefa A
	 WHERE  A.icefa_cod = r_detalle2.cve_ced_cuenta


         INSERT INTO tra_carta_correlativo
	 VALUES (r_folio_carta ,
		 v_corr) 

                       INSERT INTO tra_mae_icefa_issste
                       VALUES(reg_afi_mae_afiliado.n_folio          ,
                              reg_afi_mae_afiliado.tipo_solicitud   ,
                              reg_afi_mae_afiliado.n_seguro         ,
                              r_detalle2.n_seguro_ent ,
                              r_detalle2.rfc_ent      ,
                              paterno   ,
                              materno   ,
                              nombre    ,
                              v_corr    ,
                              r_detalle2.cve_ced_cuenta ,
                              r_detalle2.nro_ctrl_icefa ,
                              TODAY,
                              "",
                              0                     ,#sar_92_issste
                              0                     ,#viv_92_issste
                              "61",                  #origen 61 por dflt
                              TODAY,
                              TODAY,
                              ""                     ,#lote_genera
                              ""                     ,#fecha_genera
                              20                     ,#status
                              2                      ,
                              0                      ,#correlativo
                              c8_usuario             ,
                              0                      ,#n_envios
                              ""                     ,#diagnostico
                              2                      ) # cve_sector
                            


    IF v_edo = 160 THEN
       UPDATE tra_det_atm_issste 
       SET    estado = 20,
              fecha_edo = TODAY,
              fecha_genera = TODAY
       WHERE  correlativo = v_corr
    END IF

    IF v_edo = 164 THEN
       UPDATE tra_det_atm_issste 
       SET    estado = 20 ,
              fecha_edo = TODAY,
              fecha_genera = TODAY
       WHERE  correlativo = v_corr
    END IF

      LET tot_envio = tot_envio + 1

      DISPLAY "REGISTROS GENERADOS ",tot_envio AT 17,2

         INSERT INTO paso_maestro VALUES(reg_tra_det_atm_issste.*)

         PRINT COLUMN 01,"03"                      ,
               COLUMN 03,"30401"                   ,
               COLUMN 08,r_detalle2.n_seguro       ,
               COLUMN 19,r_detalle2.n_seguro_ent   ,
               COLUMN 30,r_detalle2.rfc_ent        ,
               COLUMN 43,r_detalle2.cve_ced_cuenta ,
               COLUMN 46,r_detalle2.cve_desc       ,
               COLUMN 61,r_detalle2.nro_ctrl_icefa ,
               COLUMN 91,r_detalle2.dependencia    ,
	       COLUMN 171,r_detalle2.nombre_ent    ,
	      COLUMN 291,r_detalle2.sar_92_issste * 100 USING"&&&&&&&&&&&&&&&&",
	      COLUMN 307,r_detalle2.viv_92_issste * 100 USING"&&&&&&&&&&&&&&&&",
	      COLUMN 323,v_corr                         USING"&&&&&&&&&",
	      COLUMN 332,r_detalle2.rfc_patronal

         END FOREACH 

END REPORT

FUNCTION inicio()
#i-------------

   LET HOY = TODAY
   LET plano_carta =  reg_ruta.ruta_envio CLIPPED,
                      "/DET0203"

   START REPORT rpt_issste  TO plano_carta

   LET tot_envio = 0

END FUNCTION

FUNCTION consulta_cartas_invi()
#u------------
DEFINE cad CHAR(300)

LET cad = "fglgo TRAC076"
RUN cad

END FUNCTION
#===========================REPORTE  DE ENCABEZADO=============================
REPORT rpt_encab(r_ctos_02,r_tot_envio)

DEFINE diaa          DATE
DEFINE dia_hoy       CHAR(10) 
DEFINE r_ctos_02     INTEGER
DEFINE r_tot_envio   INTEGER
DEFINE r_num_regs    INTEGER
DEFINE r_num_lote    SMALLINT

   OUTPUT
        PAGE LENGTH   1
        LEFT MARGIN   0
        RIGHT MARGIN  0
        TOP MARGIN    0
        BOTTOM MARGIN 0

   FORMAT  
      ON EVERY ROW
         LET diaa = TODAY 

#==========DETERMINA EL NUMERO CONSECUTIVO DEL LOTE==========================
         SELECT lotes_num
         INTO r_num_lote
            FROM safre_af:tab_lote
         WHERE lotes_fecha = diaa
           AND lotes_cod = 6
         IF (STATUS = NOTFOUND) THEN
            LET r_num_lote =  1
            INSERT INTO safre_af:tab_lote  VALUES(diaa,6,"TRASPASOS",0,1)
         ELSE
            LET r_num_lote = r_num_lote + 1
         END IF

         UPDATE tab_lote
         SET lotes_num =  r_num_lote
         WHERE  lotes_fecha = diaa
           AND  lotes_cod   = 6

#================FIN DEL CALCULO DE LOTE=====================================


         LET dia_hoy  = DAY(diaa)   USING "&&","/" CLIPPED,
                        MONTH(diaa) USING "&&","/" CLIPPED,
                        YEAR(diaa) USING "&&&&" CLIPPED 

         LET r_num_regs = r_ctos_02 + r_tot_envio

      
      PRINT COLUMN 001,"01" USING "&&",
            COLUMN 003,"30401" USING "&&&&&",
         COLUMN 008,"CARTA INVITACION PARA TRASPASO DE RECURSOS SAR-ISSSTE                                                                   ",
            COLUMN 128,g_cod_afore USING "&&&",
            COLUMN 131,dia_hoy,
            COLUMN 141,r_num_lote USING "&&&&&&&&",
            COLUMN 149,r_num_regs USING "&&&&&&&&"
END REPORT
#===FUNCION QUE ARMA EL ARCHIVO PLANO CONCATENA ENCABEZADO Y DETALLE 02 Y 03====
FUNCTION arma_plano()
DEFINE comando   CHAR(250)

  LET comando =  "cat ", reg_ruta.ruta_envio CLIPPED, "/ENCA ",
                         reg_ruta.ruta_envio CLIPPED, "/DET0203 > ",  
			 reg_ruta_int.ruta_envio CLIPPED,"/30401",
                         HOY USING"DDMMYY",".1"

 RUN comando 


END FUNCTION
