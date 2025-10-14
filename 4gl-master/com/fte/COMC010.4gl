###############################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )  	                      #
#Owner             => E.F.P.        				              #
#Programa          => COMC010                                                 #
#Descripcion       => CONSULTA COMISIONES DE PROMOTORES                       #
#Fecha             =>                    			              #
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
         num_sm             LIKE com_comis_detalle.num_sm
      END RECORD, 

      g_reg ARRAY[9000] OF RECORD 
         cod_promotor       LIKE pro_mae_promotor.cod_promotor, 
         nombre             CHAR(50),
         coduni_n1          LIKE com_comis_resumen.coduni_n1, 
         n_folio            LIKE com_comis_detalle.n_folio,
         nss                LIKE com_comis_detalle.nss,
         nombre2            CHAR(50),
         salario_base_comis LIKE com_comis_detalle.salario_base_comis,
         num_sm             LIKE com_comis_detalle.num_sm
      END RECORD 
      
   DEFINE vpater               CHAR(60),
          vmater               CHAR(60),
          vnom                 CHAR(60),

   nombre CHAR(50),

      HOY	DATE,
      cla_sel 	CHAR(1500), 
      cla_sel2	CHAR(500), 
      cla_where CHAR(800), 
      vcomando  SMALLINT,
      comando   CHAR(200),
      columnas   CHAR(200),
      registros INTEGER,

    nuevo RECORD
      cod_promotor       LIKE pro_mae_promotor.cod_promotor,
      nombre             CHAR(50),
      coduni_n1          LIKE com_comis_detalle.coduni_n1,
      n_folio            LIKE com_comis_detalle.n_folio,
      nss                LIKE com_comis_detalle.nss,
      nombre2            CHAR(50),
      salario_base_comis LIKE com_comis_detalle.salario_base_comis,
      num_sm             LIKE com_comis_detalle.num_sm
    END RECORD,

      g_grep_00          CHAR(1000),
      cont_de_registros  INTEGER,
      tot_sdi            LIKE com_comis_detalle.salario_base_comis,
      tot_vsm            LIKE com_comis_detalle.num_sm,
      tot_cs             INTEGER,
      tot_ss             INTEGER,
      vnom_jefe          CHAR(50),
      vcencos            CHAR(10),
      cla                CHAR(250),
      xfecha_corte       LIKE com_comis_resumen.fecha_corte, 
      vdescr             CHAR(40),
      vsalario_minimo    DECIMAL(12,6)
        

END GLOBALS




MAIN
	OPTIONS PROMPT LINE LAST,
	        INPUT WRAP,
		ACCEPT KEY control-o
	
	DEFER INTERRUPT

        --CREA TABLA TEMPORAL PARA EXTRAER EL RESUL DEL REPORTE Y
        --GUARDARLO HASTA QUE SE OBTIENE EL ARCHIVO PARA EXCEL
        CREATE TEMP TABLE tt_arch_excel2
        (
          cod_promotor       CHAR(10),
          nombre_pro         CHAR(60),
          descri_suc         CHAR(40),
          n_folio            INTEGER,
          nss                CHAR(11),
          nombre_trabaja     CHAR(60),
          salario_base_comis DECIMAL(12,2),
          num_sm             DECIMAL(06,2)
        )




        SELECT  ruta_rescate
        INTO    g_param_dis.ruta_rescate
        FROM    seg_modulo
        WHERE   modulo_cod='com'

        SELECT  USER
        INTO    g_usuario
        FROM    tab_afore_local

     -- LET g_param_dis.ruta_rescate = '/des/act/safre/com/fte'

        SELECT B.monto_sm
        INTO   vsalario_minimo
        FROM   tab_salario_minimo B
        WHERE  B.fecha_hasta_sm is null
        AND    B.fecha_desde_sm is not null

	LET HOY = TODAY

	OPEN WINDOW ventana_1 AT 2,2 WITH FORM "COMC0101" ATTRIBUTE( BORDER)
	DISPLAY " COMC010         CONSULTA  DE  COMISIONES A DETALLE                            " AT 3,1 ATTRIBUTE(REVERSE) 

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
	   DISPLAY g_reg[i].* TO scr_1[i].*
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
                 " com_comis_detalle.num_sm ",
                 " FROM   com_comis_detalle ,pro_mae_promotor ",
                 " WHERE  com_comis_detalle.codven = cod_promotor ",
                 " AND    com_comis_detalle.nivel = 1 ",
     --  "and com_comis_detalle.coduni_n1 in ('0001M','0003') ",
               "AND com_comis_detalle.fecha_corte='",g_reg0.fecha_corte CLIPPED,
                 "' GROUP BY 1,2,3,4,5,6,7 ",
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
  DEFINE G_IMPRE    CHAR(500)
  DEFINE G_LISTA    CHAR(500)
  DEFINE impresion  CHAR(300)
  DEFINE i          SMALLINT
  DEFINE vdivide    CHAR(1)


  LET hora = TIME


  LET G_IMPRE = g_param_dis.ruta_rescate CLIPPED,"/",g_usuario CLIPPED,
                          ".REP_DETALLE_",HOY USING "DDMMYYYY"
    
  LET G_LISTA = g_param_dis.ruta_rescate CLIPPED,"/",g_usuario CLIPPED,
                          ".ARCH_DETALLE_",HOY USING "DDMMYYYY"


  -- PARA IMPRESION
  START REPORT rpt_cuenta_imp TO G_IMPRE

  LET cont_de_registros  = 0
  LET tot_sdi            = 0
  LET tot_vsm            = 0
  LET tot_cs             = 0
  LET tot_ss             = 0


  --Detalle de folios    -----------------------------------------------
  FOR i=1 TO registros


        LET nuevo.cod_promotor     =  g_reg[i].cod_promotor
        LET nuevo.nombre           =  g_reg[i].nombre
        LET nuevo.coduni_n1        =  g_reg[i].coduni_n1
        LET nuevo.n_folio          =  g_reg[i].n_folio  
        LET nuevo.nss              =  g_reg[i].nss

        LET vpater= ' '
        LET vmater= ' '
        LET vnom  = ' '
        SELECT paterno,materno,nombres
        INTO   vpater,vmater,vnom
        FROM   afi_mae_afiliado
        WHERE  n_seguro  = g_reg[i].nss 

        IF    vpater IS NULL THEN
              LET vpater = ' '
        END IF

        IF    vmater IS NULL THEN
              LET vmater = ' '
        END IF

        IF    vnom IS NULL THEN
              LET vnom = ' '
        END IF
        LET nuevo.nombre2  = vpater CLIPPED||' '||
                             vmater CLIPPED||' '||
                             vnom   CLIPPED

        LET nuevo.salario_base_comis  =  g_reg[i].salario_base_comis
        LET nuevo.num_sm              =  g_reg[i].num_sm

   IF nuevo.cod_promotor IS NULL OR
      nuevo.cod_promotor = ' ' THEN
      EXIT FOR
   END IF

  ------------------------------------------------- 
       

   -- Buscamos el nombre de los posibles jefes
   IF nuevo.nombre IS NULL OR nuevo.nombre= ' ' THEN

            -- Buscamos si se trata de un jefe
            LET vnom_jefe = ' '
            LET cla = " SELECT nombre_resp_uni,coduni_n1 ",
                      " FROM   com_respon_unidad,com_comis_resumen ",
                      " WHERE  cod_resp_uni = ",nuevo.cod_promotor, 
                      " AND    cod_resp_uni = com_comis_resumen.codven ",
                      " AND    fecha_corte  = '",xfecha_corte CLIPPED

            PREPARE claexe3 FROM cla
            DECLARE cur3 SCROLL CURSOR FOR claexe3
            OPEN cur3
            FETCH FIRST cur3 INTO vnom_jefe,vcencos
            CLOSE cur3


            IF vnom_jefe IS NOT NULL THEN
               LET nuevo.nombre  = vnom_jefe
               LET nuevo.coduni_n1   = vcencos
            ELSE
               LET nuevo.nombre  = ' '
               LET nuevo.coduni_n1   = ' '
            END IF
   END IF


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
                                   nuevo.nss,nuevo.nombre2,
                                   nuevo.salario_base_comis,nuevo.num_sm,vdescr)

   END FOR


   FINISH REPORT rpt_cuenta_imp
   ERROR "LISTADO 1 GENERADO...."
   SLEEP 2
   ERROR ""



LET impresion = "lp ",G_IMPRE
--RUN impresion


   UNLOAD TO G_LISTA
   SELECT * FROM tt_arch_excel2
   ORDER BY 1

   LET columnas = "NUM PROMO'|'NOM PROMOTOR'|'SUCURSAL'|'FOLIO'|'",
                  "NSS TRABAJADOR'|'NOM TRABAJA'|'",
                  "SDI'|'VSM'|'"

   LET comando ="cd ",g_param_dis.ruta_rescate CLIPPED,"; echo ",columnas CLIPPED,"> alguno2" CLIPPED
   RUN comando

   LET comando ="cd ",g_param_dis.ruta_rescate CLIPPED,"; cat ",G_LISTA CLIPPED,">> alguno2" CLIPPED,";mv alguno2 ",G_LISTA CLIPPED
   RUN comando





END FUNCTION


REPORT rpt_cuenta_imp(nuevo,vdescri)

DEFINE
   
   nuevo RECORD
      cod_promotor       LIKE pro_mae_promotor.cod_promotor,
      nombre             CHAR(50),
      coduni_n1          LIKE com_comis_detalle.coduni_n1,
      n_folio            LIKE com_comis_detalle.n_folio,
      nss                LIKE com_comis_detalle.nss,
      nombre2            CHAR(50),
      salario_base_comis LIKE com_comis_detalle.salario_base_comis,
      num_sm             LIKE com_comis_detalle.num_sm
    END RECORD,

    cont_sin_sala        INTEGER,
    cont_con_sala        INTEGER,
    sumVSM               DECIMAL(8,2),
    sumVSM2              DECIMAL(8,2),
    subtot_vsm           DECIMAL(8,2),
    subtot_sdi           LIKE afi_mae_afiliado.salario_actual,
    subtot_cs            INTEGER,
    subtot_ss            INTEGER,
    vaporte              LIKE afi_mae_afiliado.salario_actual,
    vdescri              CHAR(40),
    ultima_general       DATE

OUTPUT
      LEFT MARGIN 0
      RIGHT MARGIN 0
      TOP MARGIN 0
      BOTTOM MARGIN 0
      PAGE LENGTH 90
      ORDER BY nuevo.coduni_n1,nuevo.cod_promotor
   FORMAT
      PAGE HEADER

      PRINT
      PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'

      PRINT COLUMN 01,'\033(s7B',"COMC010",'\033(s0B'
   PRINT COLUMN 01,'\033(s7B',"REPORTE DE PRODUCCION MENSUAL DETALLE",'\033(s0B'
      PRINT COLUMN 01,'\033(s7B',"FECHA EMISION:",'\033(s0B',today USING "dd-mm-yyyy",'\033(s7B',"   FECHA CORTE: ",'\033(s0B',g_reg0.fecha_corte
      SKIP 1 LINE



      BEFORE GROUP OF nuevo.coduni_n1
      
      LET sumVSM            = 0
      LET subtot_vsm        = 0
      LET subtot_sdi        = 0
      LET subtot_cs         = 0
      LET subtot_ss         = 0
   

      PRINT
      PRINT COLUMN 01,'\033(s7B',"SUCURSAL:",'\033(s0B'," ",nuevo.coduni_n1," ",vdescri

      PRINT COLUMN  001,'__________________________________________________',
                        '__________________________________________________',
                        '__________________________________________________',
                        '______________________________________'

      SKIP 1 LINE

      PRINT COLUMN 02,'\033(s7B',"NUM PROMO  NOMBRE PROMOTOR                                      FOLIO     NSS TRABAJ  NOMBRE DEL TRABAJADOR                                            SDI        VSM",'\033(s0B'
      PRINT COLUMN  001,'__________________________________________________',
                        '__________________________________________________',
                        '__________________________________________________',
                        '______________________________________'
      SKIP 1 LINE


      BEFORE GROUP OF nuevo.cod_promotor 
      PRINT COLUMN 002,nuevo.cod_promotor CLIPPED,
            COLUMN 013,nuevo.nombre



      LET sumVSM2           = 0
      LET cont_con_sala     = 0
      LET cont_sin_sala     = 0


      ON EVERY ROW
      ----SE VALIDA SI TIENE APORTACION
      LET ultima_general =''
      LET vaporte = 0

      DECLARE c_3 CURSOR FOR
        SELECT MAX(fecha_recepcion),ult_salario_diario/100
        FROM   dis_det_aporte
        WHERE  n_seguro = nuevo.nss
        GROUP BY 2
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
      END IF

      

      -----PARA EL CONTADOR DE TRABAJADORES CON SALARIO
      IF   nuevo.salario_base_comis > 0 THEN
           LET cont_con_sala = cont_con_sala + 1
      END IF

      
      IF nuevo.salario_base_comis IS NULL OR 
         nuevo.salario_base_comis = ' ' THEN
         LET nuevo.salario_base_comis=0
      END IF

      IF sumVSM IS NULL OR sumVSM =' ' THEN
         LET sumVSM = 0.00
      END IF

      IF nuevo.num_sm IS NULL OR nuevo.num_sm =' ' THEN
         LET nuevo.num_sm = 0.00
      END IF



      PRINT COLUMN 064,nuevo.n_folio              USING "&&&&&&&&&",
            COLUMN 075,nuevo.nss          CLIPPED,
            COLUMN 088,nuevo.nombre2,
            COLUMN 140,nuevo.salario_base_comis   USING "##########&&.&&&&&&",
            COLUMN 152,nuevo.num_sm               USING "#####&.&&"
          
            IF nuevo.num_sm IS NULL OR nuevo.num_sm=' ' THEN
               LET nuevo.num_sm = 0
            END IF
 
            LET sumVSM            = sumVSM     + nuevo.num_sm
            LET cont_de_registros = cont_de_registros + 1
         LET sumVSM2           = sumVSM2    + nuevo.num_sm

         LET subtot_sdi           = subtot_sdi    + nuevo.salario_base_comis
         LET subtot_vsm           = subtot_vsm    + nuevo.num_sm


       INSERT INTO tt_arch_excel2
            VALUES(nuevo.cod_promotor,nuevo.nombre,vdescri,nuevo.n_folio,
                   nuevo.nss,nuevo.nombre2,nuevo.salario_base_comis,
                   nuevo.num_sm) 

       AFTER GROUP OF nuevo.cod_promotor

       
       --Para los subtotales de con salario y sin salario
       LET subtot_ss            = subtot_ss     + cont_sin_sala 
       LET subtot_cs            = subtot_cs     + cont_con_sala 


       PRINT COLUMN 126,'___________________________________________'
       SKIP 1 LINE
       PRINT COLUMN 130,"CON SALARIO:  ",cont_con_sala USING "########&"
       PRINT COLUMN 130,"SIN SALARIO:  ",cont_sin_sala USING "########&"
       PRINT COLUMN 130,"TOT SMG:                    ",sumVSM2 USING "#####&.&&"
       PRINT
       PRINT
       PRINT


      AFTER GROUP OF nuevo.coduni_n1

       --- PARA LOS TOTALES FINALES
       LET tot_sdi = tot_sdi + subtot_sdi  
       LET tot_vsm = tot_vsm + subtot_vsm 

       --Para los totales de con salario y sin salario
       LET tot_ss            = tot_ss     + subtot_ss 
       LET tot_cs            = tot_cs     + subtot_cs 

      PRINT COLUMN 126,'         Subtotales por sucursal           '
      PRINT COLUMN 121,'========================================================='
      PRINT COLUMN 130,"SUBTOT SDI:           ",subtot_sdi USING "##########&&.&&&&&&"
      PRINT COLUMN 130,"SUBTOT VSM:                 ",subtot_vsm USING "#####&.&&" 
      PRINT COLUMN 130,"SUBTOT CON SALARIO:      ",subtot_cs  USING "########&"
      PRINT COLUMN 130,"SUBTOT SIN SALARIO:      ",subtot_ss  USING "########&" 


       PAGE TRAILER
       SKIP 2 LINE
       PRINT COLUMN 67," Pagina : ",PAGENO USING "<<<<<"


       ON LAST ROW

       PRINT

       PRINT COLUMN 002,'                   TOTALES               '
       PRINT COLUMN 002,'___________________________________________'
       PRINT COLUMN 002,'___________________________________________'
       SKIP 1 LINE
       PRINT COLUMN 002,"TOTAL DE REGISTROS       ",cont_de_registros USING "#######&"
       PRINT COLUMN 002,"tot SDI              ",tot_sdi USING "##########&&.&&&&&&"
       PRINT COLUMN 002,"tot VSM                    ",tot_vsm USING "#####&.&&"
       PRINT COLUMN 002,"tot con salario         ",tot_cs USING "########&"
       PRINT COLUMN 002,"tot sin salario         ",tot_ss USING "########&"
 


END REPORT


 
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

 
             LET vpater= ' '
      LET vmater= ' '
      LET vnom  = ' '
      SELECT paterno,materno,nombres
      INTO   vpater,vmater,vnom
      FROM   afi_mae_afiliado
      WHERE  n_seguro  = g_reg[pos].nss

      IF    vpater IS NULL THEN
            LET vpater = ' '
      END IF

      IF    vmater IS NULL THEN
            LET vmater = ' '
      END IF

      IF    vnom IS NULL THEN
            LET vnom = ' '
      END IF
      LET g_reg[pos].nombre2 = vpater CLIPPED||' '||
                               vmater CLIPPED||' '||
                               vnom   CLIPPED
 

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
