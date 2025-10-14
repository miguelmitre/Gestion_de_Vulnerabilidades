###############################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )			      #
#Owner             => E.F.P.        					      #
#Programa          => COMC011                                                 # 
#Descripcion       => CONSULTA COMISIONES DE PROMOTORES                       #
#Fecha             =>                    				      #
#By                => JOSE ALEJANDRO RAMIREZ.        	                      #
#Sistema           => COM. 					              #
###############################################################################
DATABASE safre_af
GLOBALS
   DEFINE 

      g_param_dis     RECORD LIKE seg_modulo.* ,
      g_usuario       CHAR (08),


      g_reg0 RECORD
	fecha_corte   DATE
      END RECORD,

      
      g_reg88 ARRAY[9000] OF RECORD 
         cod_promotor       LIKE pro_mae_promotor.cod_promotor,
         nombre             CHAR(50),
         coduni_n1          LIKE com_comis_detalle.coduni_n1,
         n_folio            LIKE com_comis_detalle.n_folio, 
         nss                LIKE com_comis_detalle.nss,
         salario_base_comis LIKE com_comis_detalle.salario_base_comis,
         num_sm             LIKE com_comis_detalle.num_sm,
         tipo_solicitud     LIKE com_comis_detalle.tipo_solicitud
      END RECORD, 

      g_reg ARRAY[9000] OF RECORD 
         cod_promotor       LIKE pro_mae_promotor.cod_promotor, 
         nombre             CHAR(50),
         coduni_n1          LIKE com_comis_resumen.coduni_n1, 
         n_folio            LIKE com_comis_detalle.n_folio,
         nss                LIKE com_comis_detalle.nss,
         salario_base_comis LIKE com_comis_detalle.salario_base_comis,
         num_sm             LIKE com_comis_detalle.num_sm,
         tipo_solicitud     LIKE com_comis_detalle.tipo_solicitud
      END RECORD 
      
    --   nombre2            CHAR(50),

   DEFINE vpater               CHAR(60),
          vmater               CHAR(60),
          vnom                 CHAR(60),

   nombre CHAR(50),

      HOY	DATE,
      cla_sel 	CHAR(1500), 
      cla_sel2	CHAR(500), 
      cla_where CHAR(800), 
      vcomando  SMALLINT,
      comando   CHAR(300),
      columnas   CHAR(300),
      registros INTEGER,

    nuevo RECORD
      cod_promotor       LIKE pro_mae_promotor.cod_promotor,
      nombre             CHAR(50),
      coduni_n1          LIKE com_comis_detalle.coduni_n1,
      n_folio            LIKE com_comis_detalle.n_folio,
      nss                LIKE com_comis_detalle.nss,
   -- nombre2            CHAR(50),
      salario_base_comis LIKE com_comis_detalle.salario_base_comis,
      num_sm             LIKE com_comis_detalle.num_sm,
      tipo_solicitud     LIKE com_comis_detalle.tipo_solicitud
    END RECORD,

      g_grep_00          CHAR(1000),
      cont_de_registros  INTEGER,
      cont_promo         INTEGER,
      suma_sala_afi_tot  LIKE afi_mae_afiliado.salario_actual,
      suma_sumsm_afi_tot LIKE com_comis_detalle.num_sm,
      cont_sin_sala_afi_tot  INTEGER,
      cont_con_sala_tra_tot  INTEGER,
      suma_sala_tra_tot  LIKE afi_mae_afiliado.salario_actual,
      suma_sumsm_tra_tot LIKE com_comis_detalle.num_sm,
      cont_sin_sala_tra_tot INTEGER,
      tot_solicitud      INTEGER,
      tot_smg            LIKE afi_mae_afiliado.salario_actual,
      tot_calidad        LIKE com_comis_detalle.num_sm,
      tot_cal_afil       LIKE com_comis_detalle.num_sm,
      tot_cal_tras       LIKE com_comis_detalle.num_sm,
      total_soli         INTEGER,
      total_cali         LIKE com_comis_detalle.num_sm,
      total_cali_afi     LIKE com_comis_detalle.num_sm,
      total_cali_tra     LIKE com_comis_detalle.num_sm,
      total_smg          LIKE afi_mae_afiliado.salario_actual,
      cont_afil          INTEGER,
      vnom_jefe          CHAR(50),
      vcencos            CHAR(10),
      cla                CHAR(250),
      xfecha_corte       LIKE com_comis_resumen.fecha_corte, 
      vdescr             CHAR(40),
      vsalario_minimo    DECIMAL(12,6),
      vgrupo_superior    CHAR(40),
      vgrupo_maximo      CHAR(40),
      ENTER              CHAR(1)
        

END GLOBALS




MAIN
	OPTIONS PROMPT LINE LAST,
	        INPUT WRAP,
		ACCEPT KEY control-o
	
	DEFER INTERRUPT

        CALL STARTLOG("COMC011.log")

        --CREA TABLA TEMPORAL PARA EXTRAER EL RESUL DEL REPORTE Y
        --GUARDARLO HASTA QUE SE OBTIENE EL ARCHIVO PARA EXCEL
        CREATE TEMP TABLE tt_arch_excel
        (
          vgrupo_super       CHAR(40), 
          coduni_n1          CHAR(10),
          vdescri            CHAR(40),
          cont_de_promo      INTEGER,
          cont_con_sala_afi  INTEGER,
          suma_sala_afi      DECIMAL(12,2),
          suma_sumsm_afi     DECIMAL(6,2),
          cont_sin_sala_afi  INTEGER,
          tot_cal_afil       DECIMAL(6,2),
          cont_con_sala_tra  INTEGER,
          suma_sala_tra      DECIMAL(12,2),
          suma_sumsm_tra     DECIMAL(6,2),
          cont_sin_sala_tra  INTEGER,
          tot_cal_tras       DECIMAL(6,2),
          tot_solicitud      INTEGER,
          tot_smg            DECIMAL(12,2),
          tot_calidad        DECIMAL(6,2)
        )

        SELECT  ruta_rescate
        INTO    g_param_dis.ruta_rescate
        FROM    seg_modulo
        WHERE   modulo_cod='com'

        SELECT  USER
        INTO    g_usuario
        FROM    tab_afore_local

    --  LET g_param_dis.ruta_rescate = '/des/act/safre/com/fte'  ---ojo

        SELECT B.monto_sm
        INTO   vsalario_minimo
        FROM   tab_salario_minimo B
        WHERE  B.fecha_hasta_sm is null
        AND    B.fecha_desde_sm is not null

	LET HOY = TODAY

	OPEN WINDOW ventana_1 AT 2,2 WITH FORM "COMC0111" ATTRIBUTE( BORDER)
	DISPLAY " COMC011          CONSULTA  DE  COMISIONES GLOBAL                              " AT 3,1 ATTRIBUTE(REVERSE) 

	DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)

	DISPLAY "                                                                               " AT 8,1 ATTRIBUTE(REVERSE)


  	DISPLAY "                                                                               " AT 19,1 ATTRIBUTE(REVERSE)

	MENU "CONSULTA"
           COMMAND "Consulta" "Consulta Comisiones"
              CALL Inicializa2()
	      CALL Consulta()
           COMMAND "Salir" "Salir del Programa"
	      EXIT MENU
	END MENU
        CLOSE WINDOW ventana_1 
END MAIN

FUNCTION Inicializa()
    define i smallint
	INITIALIZE g_reg TO NULL 

        for i=1 to 1
--  DISPLAY g_reg[i].* TO scr_1[i].*
      DISPLAY g_reg[i].cod_promotor TO scr_1[i].cod_promotor
      DISPLAY g_reg[i].nombre TO scr_1[i].nombre
      DISPLAY g_reg[i].coduni_n1 TO scr_1[i].coduni_n1
      DISPLAY g_reg[i].n_folio TO scr_1[i].n_folio
      DISPLAY g_reg[i].nss TO scr_1[i].nss
  --  DISPLAY g_reg[i].nombre2 TO scr_1[i].nombre2
      DISPLAY g_reg[i].salario_base_comis TO scr_1[i].salario_base_comis
      DISPLAY g_reg[i].num_sm TO scr_1[i].num_sm
      DISPLAY g_reg[i].tipo_solicitud TO scr_1[i].tipo_solicitud

        end for

	LET registros = 0
        DISPLAY registros TO scr_3.*


        CLEAR SCREEN
END FUNCTION

FUNCTION Inicializa2()
   LET cla_where=NULL
   LET cla_sel=NULL
   LET cla    =NULL
   INITIALIZE g_reg0.* TO NULL    --g_reg2.* ojo
   DISPLAY BY NAME g_reg0.*        --,g_reg2.* ojo
END FUNCTION

FUNCTION Consulta()
   DEFINE
      pat CHAR(40),
      mat char(40),
      nom char(40),
      pos SMALLINT   

   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " [ESC] Procesar " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " [Ctrl-C] Salir " AT 1,63 ATTRIBUTE(REVERSE)


   LET INT_FLAG = FALSE

   INPUT BY NAME g_reg0.fecha_corte
   AFTER FIELD fecha_corte
         IF g_reg0.fecha_corte IS NULL THEN
            ERROR "Fecha NO puede ser NULA"
            NEXT FIELD fecha_corte
         END IF

   ON KEY (ESC)
   LET vcomando = 2
       EXIT INPUT
   ON KEY (INTERRUPT)
     LET vcomando=1
       EXIT INPUT
     END INPUT

       IF vcomando = 1 THEN
          LET INT_FLAG = FALSE
          ERROR "Operacion abortada"
          LET vcomando = 0
          RETURN
       END IF

       WHENEVER ERROR CONTINUE
       DROP TABLE tt_inicial

       WHENEVER ERROR STOP

       LET cla_sel =" SELECT cod_promotor,", 
                 " trim(pro_mae_promotor.paterno)||' '|| ",
                 " trim(pro_mae_promotor.materno)||' '|| ",
                 " trim(pro_mae_promotor.nombres) as nom, ",
                 " com_comis_detalle.coduni_n1, ",
                 " com_comis_detalle.n_folio, ",
                 " com_comis_detalle.nss, ",
                 " com_comis_detalle.salario_base_comis, ",
                 " com_comis_detalle.num_sm, ",
                 " com_comis_detalle.tipo_solicitud ",
                 " FROM   com_comis_detalle ,pro_mae_promotor ",
                 " WHERE  com_comis_detalle.codven = cod_promotor ",
                 " AND    com_comis_detalle.nivel = 1 ",
               "AND com_comis_detalle.fecha_corte='",g_reg0.fecha_corte CLIPPED,
                 "' GROUP BY 1,2,3,4,5,6,7,8 ",
                 " ORDER BY 1,2 ",
                 " INTO TEMP tt_inicial "

       PREPARE q1 FROM cla_sel
       EXECUTE q1

       DECLARE cursor_1 CURSOR FOR
       SELECT  * FROM tt_inicial


         DISPLAY "                                                 [Ctrl-I] Imprime                                    " AT 8,01 ATTRIBUTE(REVERSE)
	 CALL ver_arreglo()

END FUNCTION



#------------------------------------------------------------------
FUNCTION imprime()

  DEFINE hora       CHAR (08)
  DEFINE G_IMPRE    CHAR(300)
  DEFINE G_LISTA    CHAR(300)
  DEFINE impresion  CHAR(300)
  DEFINE i          SMALLINT
  DEFINE vdivide    CHAR(1)


  LET hora = TIME

  LET G_IMPRE = g_param_dis.ruta_rescate CLIPPED,"/",g_usuario CLIPPED,
                          ".REP_GLOBAL_",HOY USING "DDMMYYYY"
    
  LET G_LISTA = g_param_dis.ruta_rescate CLIPPED,"/",g_usuario CLIPPED,
                          ".ARCH_GLOBAL_",HOY USING "DDMMYYYY"


  -- PARA IMPRESION
  START REPORT rpt_cuenta_imp TO G_IMPRE

  LET cont_de_registros     = 0
  LET cont_promo            = 0
  LET suma_sala_afi_tot     = 0 
  LET suma_sumsm_afi_tot    = 0 
  LET cont_sin_sala_afi_tot = 0
  LET cont_con_sala_tra_tot = 0
  LET suma_sala_tra_tot     = 0
  LET suma_sumsm_tra_tot    = 0
  LET cont_sin_sala_tra_tot = 0
  LET tot_solicitud         = 0
  LET total_soli            = 0
  LET tot_smg               = 0
  LET total_smg             = 0
  LET tot_calidad           = 0
  LET tot_cal_afil          = 0
  LET tot_cal_tras          = 0
  LET total_cali            = 0
  LET total_cali_afi        = 0
  LET total_cali_tra        = 0
  LET cont_afil             = 0


  --Detalle de folios    -----------------------------------------------
  FOR i=1 TO registros     ----300


        LET nuevo.cod_promotor     =  g_reg[i].cod_promotor
        LET nuevo.nombre           =  g_reg[i].nombre
        LET nuevo.coduni_n1        =  g_reg[i].coduni_n1
        LET nuevo.n_folio          =  g_reg[i].n_folio  
        LET nuevo.nss              =  g_reg[i].nss

        LET nuevo.salario_base_comis  =  g_reg[i].salario_base_comis
        LET nuevo.num_sm              =  g_reg[i].num_sm
        LET nuevo.tipo_solicitud      =  g_reg[i].tipo_solicitud

   IF nuevo.cod_promotor IS NULL OR
      nuevo.cod_promotor = ' ' THEN
      EXIT FOR
   END IF

  ------------------------------------------------- 
       
   CALL obtiene_agrupacion_mayor(nuevo.coduni_n1) RETURNING vgrupo_superior,
                                                            vgrupo_maximo

   --Se agrega al archivo plano separado por pipes, la descr de la suscur
   LET     vdescr = ' '
   SELECT  nombre_uni_n1 
   INTO    vdescr
   FROM    com_nivel1
   WHERE   coduni_n1 = nuevo.coduni_n1 

   IF vdescr IS NULL OR vdescr = ' ' THEN

      SELECT  nombre_uni_n2
      INTO    vdescr
      FROM    com_nivel2
      WHERE   coduni_n2 = nuevo.coduni_n1

      IF vdescr IS NULL  OR vdescr = ' 'THEN

         SELECT  nombre_uni_n4
         INTO    vdescr
         FROM    com_nivel4
         WHERE   coduni_n4 = nuevo.coduni_n1

         IF vdescr IS NULL  OR vdescr = ' 'THEN
            LET vdescr = ' '
         END IF
      END IF
   END IF


   --PARA LA IMPRESION
   OUTPUT TO REPORT rpt_cuenta_imp(nuevo.cod_promotor,nuevo.nombre,
                                   nuevo.coduni_n1,nuevo.n_folio,
                                   nuevo.nss,
                                   nuevo.salario_base_comis,nuevo.num_sm,
                                   nuevo.tipo_solicitud,vdescr,vgrupo_superior,
                                   vgrupo_maximo)

   END FOR


   FINISH REPORT rpt_cuenta_imp
   ERROR "LISTADO 1 GENERADO...."
   SLEEP 2
   ERROR ""



LET impresion = "lp ",G_IMPRE
RUN impresion

   --SE GENERA EL ARCHIVO PARA EXCEL
   UNLOAD TO G_LISTA
   SELECT * FROM tt_arch_excel 
   ORDER BY 1,2

   LET columnas = "NIVEL SUPERIOR'|'COD_GERENCIAS'|'GERENCIAS'|'PROMOTORES'|'",
                  "AFILIACIONES CS'|'",
                  "SMG'|'VECES CALIDAD'|'",
                  "AFILIACIONES SS'|'CALI AFIL'|'",
                  "TRASPASOS CS'|'SMG'|'VECES CALIDAD'|'",
                  "TRASPASOS SS'|'CALI TRAS'|'",
                  "SUM AFIL_TRAS CS'|'SUMA AFIL_TRAS SMG'|'",
                  "SUMA AFIL_TRAS CALIDAD'|'"

   LET comando ="cd ",g_param_dis.ruta_rescate CLIPPED,"; echo ",columnas CLIPPED,"> alguno" CLIPPED
   RUN comando

   LET comando ="cd ",g_param_dis.ruta_rescate CLIPPED,"; cat ",G_LISTA CLIPPED,">> alguno" CLIPPED,";mv alguno ",G_LISTA CLIPPED
   RUN comando

END FUNCTION

 
REPORT rpt_cuenta_imp(nuevo,vdescri,vgrupo_sup,vgrupo_max)

DEFINE
   
   nuevo RECORD
      cod_promotor       LIKE pro_mae_promotor.cod_promotor,
      nombre             CHAR(50),
      coduni_n1          LIKE com_comis_detalle.coduni_n1,
      n_folio            LIKE com_comis_detalle.n_folio,
      nss                LIKE com_comis_detalle.nss,
      salario_base_comis LIKE com_comis_detalle.salario_base_comis,
      num_sm             LIKE com_comis_detalle.num_sm,
      tipo_solicitud     LIKE com_comis_detalle.tipo_solicitud
    END RECORD,

    cont_sin_sala        INTEGER,
    cont_con_sala        INTEGER,
    cont_con_sala_afi    INTEGER,
    suma_sala_afi        LIKE afi_mae_afiliado.salario_actual,
    cont_sin_sala_afi    INTEGER,
    cont_de_promo        INTEGER,
    suma_sumsm_afi       DECIMAL(8,2),
    cont_con_sala_tra    INTEGER,
    suma_sala_tra        LIKE afi_mae_afiliado.salario_actual,
    suma_sumsm_tra       DECIMAL(8,2),
    cont_sin_sala_tra    INTEGER,
    sumVSM               DECIMAL(8,2),
    vaporte              LIKE afi_mae_afiliado.salario_actual,
    vdescri              CHAR(40),
    vgrupo_sup           CHAR(40),
    vgrupo_max           CHAR(40),
    ultima_general       DATE

OUTPUT
      LEFT MARGIN 0
      RIGHT MARGIN 0
      TOP MARGIN 0
      BOTTOM MARGIN 0
      PAGE LENGTH 90
      ORDER BY vgrupo_max,vgrupo_sup,nuevo.coduni_n1,nuevo.cod_promotor
   FORMAT
      PAGE HEADER

 
      PRINT
      PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
      PRINT COLUMN 01,'\033(s7B',"COMC011",'\033(s0B'
      PRINT COLUMN 01,'\033(s7B',"REPORTE DE PRODUCCION MENSUAL GLOBAL",'\033(s0B'
      PRINT COLUMN 01,'\033(s7B',"FECHA EMISION:",'\033(s0B',today USING "dd-mm-yyyy",'\033(s7B',"   FECHA CORTE: ",'\033(s0B',g_reg0.fecha_corte
      SKIP 1 LINE


      PRINT COLUMN  001,'__________________________________________________',
                        '__________________________________________________',
                        '__________________________________________________',
                        '______________________________________'

      SKIP 1 LINE

      PRINT COLUMN 02,'\033(s7B',"           GERENCIAS      NUM PROMO  AFILI. CS   SMG     VECES CAL AFILI. SS  CAL.AFI  TRASP. CS  SMG  VECES CAL  TRASP. SS CAL.TRA   SUMA AFI/TRAS   SUMA SMG  SUMA CALIDAD",'\033(s0B'
      PRINT COLUMN  001,'__________________________________________________',
                        '__________________________________________________',
                        '__________________________________________________',
                        '______________________________________'


      BEFORE GROUP OF vgrupo_max

           PRINT
           SKIP 1 LINE
           PRINT COLUMN 018,'\033(s7B',vgrupo_max,'\033(s0B'


      BEFORE GROUP OF vgrupo_sup      

           PRINT
           PRINT
           PRINT
           SKIP 1 LINE
           PRINT COLUMN 008,'\033(s7B',vgrupo_sup,'\033(s0B'

      

      BEFORE GROUP OF nuevo.coduni_n1
      
      LET cont_con_sala     = 0
      LET cont_sin_sala     = 0
      LET cont_con_sala_afi = 0
      LET cont_sin_sala_afi = 0
      LET suma_sala_afi     = 0
      LET suma_sumsm_afi    = 0
      LET cont_con_sala_tra = 0
      LET suma_sala_tra     = 0
      LET suma_sumsm_tra    = 0
      LET cont_sin_sala_tra = 0
      LET sumVSM            = 0
      LET cont_de_promo = 0


      ON EVERY ROW
      ----SE VALIDA SI TIENE APORTACION
      LET ultima_general =''
      LET vaporte = 0

      DECLARE c_3 CURSOR FOR
        SELECT MAX(fecha_recepcion),ult_salario_diario/100
        FROM   dis_det_aporte
        WHERE  n_seguro = nuevo.nss
        GROUP BY 2
     -- ORDER BY 1,2 desc
        ORDER BY 1 desc
      FOREACH c_3 INTO ultima_general,vaporte
       EXIT FOREACH
      END FOREACH

      IF   vaporte IS NULL THEN
           LET vaporte = 0
      END IF

    
      ---SOLO SI YA TIENE APORTACION PLANCHARA EL SALARIO QUE TRAEMOS DE com_comis_detalle
      IF   vaporte > 0 THEN
           LET nuevo.salario_base_comis     = vaporte
           LET nuevo.num_sm     = vaporte / vsalario_minimo
      END IF


      -----PARA EL CONTADOR DE TRABAJADORES SIN SALARIO
      IF   nuevo.salario_base_comis = 0 THEN
           LET cont_sin_sala = cont_sin_sala + 1

           -----PARA EL CONTADOR DE AFILIACIONES SIN SALARIO
           IF   nuevo.tipo_solicitud = 1 THEN
                LET cont_sin_sala_afi = cont_sin_sala_afi + 1
           END IF
           -----PARA EL CONTADOR DE TRASPASOS SIN SALARIO
           IF   nuevo.tipo_solicitud = 2 THEN
                LET cont_sin_sala_tra = cont_sin_sala_tra + 1
           END IF
      END IF

      

      -----PARA EL CONTADOR DE TRABAJADORES CON SALARIO
      IF   nuevo.salario_base_comis > 0 THEN
           LET cont_con_sala = cont_con_sala + 1

           -----PARA EL CONTADOR DE AFILIACIONES CON SALARIO
           IF   nuevo.tipo_solicitud = 1 THEN
                LET cont_con_sala_afi = cont_con_sala_afi + 1
                LET suma_sala_afi     = suma_sala_afi + nuevo.salario_base_comis
                LET suma_sumsm_afi    = suma_sumsm_afi + nuevo.num_sm
           END IF
           -----PARA EL CONTADOR DE TRASPASOS CON SALARIO
           IF   nuevo.tipo_solicitud = 2 THEN
                LET cont_con_sala_tra = cont_con_sala_tra + 1
                LET suma_sala_tra     = suma_sala_tra + nuevo.salario_base_comis
                LET suma_sumsm_tra    = suma_sumsm_tra + nuevo.num_sm
           END IF
      END IF


      LET tot_solicitud = cont_con_sala_afi + cont_con_sala_tra 
      LET tot_smg       = suma_sala_afi     + suma_sala_tra 
      LET tot_calidad   = suma_sumsm_afi    + suma_sumsm_tra 
      LET tot_cal_afil  = suma_sumsm_afi    / cont_con_sala_afi 
      LET tot_cal_tras  = suma_sumsm_tra    / cont_con_sala_tra

      IF tot_cal_afil IS NULL THEN
         LET tot_cal_afil = 0.00
      END IF

      IF tot_cal_tras IS NULL THEN
         LET tot_cal_tras = 0.00
      END IF

      BEFORE GROUP OF nuevo.cod_promotor 

  --  LET cont_de_promo = cont_de_promo + 1 

      SELECT COUNT(*) 
      INTO   cont_de_promo
      FROM pro_mae_promotor
      WHERE agenc_cod=nuevo.coduni_n1

      IF cont_de_promo IS NULL THEN
         LET cont_de_promo = 0
      END IF


      AFTER GROUP OF nuevo.coduni_n1 


      PRINT COLUMN 002,nuevo.coduni_n1,
            COLUMN 013,vdescri CLIPPED,
            COLUMN 029,cont_de_promo   USING "######&",


            COLUMN 034,cont_con_sala_afi USING "######&",  --  -5
            COLUMN 041,suma_sala_afi   USING "#######&&.&&&", -- -4
            COLUMN 058,suma_sumsm_afi  USING "#####&.&&",
            COLUMN 065,cont_sin_sala_afi USING "######&",

            COLUMN 070,tot_cal_afil      USING "#####&.&&",

            COLUMN 083,cont_con_sala_tra USING "######&",
            COLUMN 092,suma_sala_tra   USING "#######&&.&&",
            COLUMN 109,suma_sumsm_tra  USING "#####&.&&",
            COLUMN 120,cont_sin_sala_tra USING "######&",

            COLUMN 130,tot_cal_tras      USING "#####&.&&",


            COLUMN 141,tot_solicitud   USING "####&",
            COLUMN 148,tot_smg         USING "##########&&.&&",
            COLUMN 165,tot_calidad     USING "#####&.&&" 
         
          
            LET sumVSM            = sumVSM    + nuevo.num_sm
            LET cont_de_registros = cont_de_registros + 1
            LET cont_afil         = cont_afil + cont_con_sala_afi  --alex 

   LET cont_promo        = cont_promo        + cont_de_promo             --alex 
   LET suma_sala_afi_tot = suma_sala_afi_tot + suma_sala_afi             --alex 
   LET suma_sumsm_afi_tot = suma_sumsm_afi_tot + suma_sumsm_afi          --alex 
   LET cont_sin_sala_afi_tot = cont_sin_sala_afi_tot + cont_sin_sala_afi --alex 
   LET cont_con_sala_tra_tot = cont_con_sala_tra_tot + cont_con_sala_tra --alex 
   LET suma_sala_tra_tot = suma_sala_tra_tot + suma_sala_tra             --alex 
   LET suma_sumsm_tra_tot = suma_sumsm_tra_tot + suma_sumsm_tra          --alex 
   LET cont_sin_sala_tra_tot = cont_sin_sala_tra_tot + cont_sin_sala_tra --alex 

   LET total_soli = total_soli + tot_solicitud                           --alex 
   LET total_smg  = total_smg  + tot_smg                                 --alex 
   LET total_cali = total_cali + tot_calidad                             --alex 
-- LET total_cali_afi = total_cali_afi + tot_cal_afil                    --alex 
-- LET total_cali_tra = total_cali_tra + tot_cal_tras                    --alex 
   LET total_cali_afi = suma_sumsm_afi_tot / cont_afil                   --alex 
   LET total_cali_tra = suma_sumsm_tra_tot / cont_con_sala_tra_tot       --alex 

            INSERT INTO tt_arch_excel
            VALUES(vgrupo_max,nuevo.coduni_n1,vdescri,cont_de_promo,
                   cont_con_sala_afi,suma_sala_afi,suma_sumsm_afi,
                   cont_sin_sala_afi,tot_cal_afil,cont_con_sala_tra,
                   suma_sala_tra,suma_sumsm_tra,cont_sin_sala_tra,
                   tot_cal_tras,tot_solicitud,tot_smg,tot_calidad)
              
       PAGE TRAILER
       SKIP 2 LINE
       PRINT COLUMN 67," Pagina : ",PAGENO USING "<<<<<"

       ON LAST ROW

       PRINT

       PRINT COLUMN 002,'__________________________    _____   ______    ________   ______   ______   ______   ______    _________    _______    ______      ______  ===================================='
       SKIP 1 LINE
       PRINT COLUMN 002,"TOTAL DE REGISTROS:",cont_de_registros USING "######&"," ",cont_promo USING "######&"," ",cont_afil USING "######&"," ",suma_sala_afi_tot USING "######&&.&&&"," ",suma_sumsm_afi_tot USING "#####&.&&"," ",cont_sin_sala_afi_tot USING "######&"," ",total_cali_afi USING "#####&.&&"," ",cont_con_sala_tra_tot USING "######&"," ",suma_sala_tra_tot USING "########&&.&&"," ",suma_sumsm_tra_tot USING "#####&.&&","  ",cont_sin_sala_tra_tot USING "######&","    ",total_cali_tra USING "#####&.&&"," ",total_soli USING "######&","  ",total_smg USING "##########&&.&&"," ",total_cali USING "#####&.&&"
 


END REPORT



FUNCTION obtiene_agrupacion_mayor(vcodigo)

 DEFINE vnivel1 SMALLINT
 DEFINE vnom_superior1 CHAR(40)
 DEFINE vnivel2 SMALLINT
 DEFINE vnom_superior2 CHAR(40)
 DEFINE vnivel3 SMALLINT
 DEFINE vnom_superior3 CHAR(40)
 DEFINE vnivel4 SMALLINT
 DEFINE vnom_superior4 CHAR(40)
 DEFINE vcodigo LIKE com_comis_detalle.coduni_n1

   SELECT  uni_superior_n1,nombre_uni_n1
   INTO    vnivel1,vnom_superior1
   FROM    com_nivel1
   WHERE   coduni_n1 = vcodigo

   IF vnivel1 IS NOT NULL THEN
 
      SELECT  uni_superior_n2,nombre_uni_n2
      INTO    vnivel2,vnom_superior2
      FROM    com_nivel2
      WHERE   coduni_n2 = vnivel1

       IF vnivel2 IS NULL THEN
          LET vnom_superior2 = " "
       END IF 

       SELECT  uni_superior_n3,nombre_uni_n3
       INTO    vnivel3,vnom_superior3
       FROM    com_nivel3
       WHERE   coduni_n3 = vnivel2

       IF vnivel3 IS NULL THEN
          LET vnom_superior3 = " "
       END IF

       SELECT  uni_superior_n4,nombre_uni_n4
       INTO    vnivel4,vnom_superior4
       FROM    com_nivel4
       WHERE   coduni_n4 = vnivel3

       IF vnivel4 IS NULL THEN
          LET vnom_superior4 = " "
       END IF

   ELSE
      LET vnom_superior2= " "
      LET vnom_superior4= " "
   END IF 

 RETURN vnom_superior2,vnom_superior4

END FUNCTION
 
FUNCTION ver_arreglo()
   DEFINE
    pat    CHAR(40),
    mat    char(40),
    nom    char(40),
    pos    SMALLINT,
    fhasta DATE,
    i      SMALLINT

    LET registros = 0


    LET cla_sel2="SELECT * FROM tt_inicial "
 
    ERROR "Buscando Informacion"   
    PREPARE claexe FROM cla_sel2
    DECLARE cursor_1111 CURSOR FOR claexe
      LET pos = 1
    FOREACH cursor_1111 INTO g_reg88[pos].*

             LET g_reg[pos].cod_promotor       = g_reg88[pos].cod_promotor 
             LET g_reg[pos].nombre             = g_reg88[pos].nombre 
             LET g_reg[pos].coduni_n1          = g_reg88[pos].coduni_n1 
             LET g_reg[pos].n_folio            = g_reg88[pos].n_folio 
             LET g_reg[pos].nss                = g_reg88[pos].nss 
             LET g_reg[pos].salario_base_comis = g_reg88[pos].salario_base_comis
             LET g_reg[pos].num_sm             = g_reg88[pos].num_sm 
             LET g_reg[pos].tipo_solicitud     = g_reg88[pos].tipo_solicitud

 
             LET registros = registros  + 1
	     LET pos = pos + 1
             IF pos >= 9000 THEN
                 ERROR "Sobrepaso la capacidad del arreglo"
                 EXIT FOREACH
             END IF 
         END FOREACH

         DISPLAY registros TO scr_3.* 

	 ERROR ""

	 CALL  SET_COUNT(pos-1)

	 IF (pos-1) >= 1 THEN
	    DISPLAY ARRAY g_reg TO scr_1.*


               ON KEY (CONTROL-I)
	          ERROR "PROCESANDO ..."
                  CALL imprime()
               ON KEY (INTERRUPT)
   DISPLAY "                                                                                                     " AT 8,01 ATTRIBUTE(REVERSE)  
                  EXIT DISPLAY
	    END DISPLAY
	 ELSE
	    ERROR "NO EXISTEN DATOS CON ESTAS CONDICIONES"
            SLEEP 3
            error ""
	 END IF
         call Inicializa()
         CLEAR SCREEN
END FUNCTION

-------------- query para probar
{
select coduni_n1,sum(salario_base_comis),sum(num_sm),
count(*) from com_comis_detalle
where fecha_corte='08/31/2004'
and tipo_solicitud=2
and salario_base_comis=0
and nivel=1
group by 1
order by 1
}
----------------
