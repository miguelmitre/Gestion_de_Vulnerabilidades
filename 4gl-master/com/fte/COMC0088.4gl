###############################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )			       #
#Owner             => E.F.P.        					       #
#Programa          => COMC008  
#Descripcion       => CONSULTA COMISIONES DE PROMOTORES
#Fecha             => 17 Marzo 2004.     				       #
#By                => JOSE ALEJANDRO RAMIREZ.        	                       #
#Sistema           => COM. 					               #
################################################################################
DATABASE safre_af
GLOBALS
   DEFINE 

      g_param_dis     RECORD LIKE seg_modulo.* ,
      w_codigo_afore  LIKE tab_afore_local.codigo_afore,
      g_usuario       CHAR (08),


      g_reg0 RECORD
	cod_tipo_prom SMALLINT,
        coduni_n1     CHAR(10),
        codven        char(10),
	fecha_corte   DATE,
        nombres       CHAR(40),
        paterno       CHAR(40), 
        materno       CHAR(40) 
      END RECORD,

      
      g_reg88 ARRAY[1000] OF RECORD 
         fcorte             LIKE com_comis_resumen.fecha_corte, 
         codven             LIKE com_comis_resumen.codven, 
         cod_tipo_prom      SMALLINT,
         vmod_pago          LIKE pro_mae_promotor.tipo_recibo,
         nombre             CHAR(50),
         Vagenc_cod         LIKE pro_mae_promotor.agenc_cod, 
         fingre             LIKE pro_mae_promotor.fingre,
         fecha_certifi      LIKE pro_mae_promotor.fecha_certifi,
         fecha_baja         LIKE pro_mae_promotor.fecha_baja,
         Vcodven            LIKE pro_mae_promotor.codven,
         total_comision     LIKE com_comis_resumen.total_comision
      END RECORD, 

      g_reg ARRAY[1000] OF RECORD 
         fcorte             LIKE com_comis_resumen.fecha_corte, 
         codven             LIKE com_comis_resumen.codven, 
         vtipo_pro          LIKE com_comis_resumen.cod_tipo_prom, 
         vmod_pago          LIKE pro_mae_promotor.tipo_recibo,
         nombre             CHAR(50),
         Vcodven            LIKE pro_mae_promotor.codven,

         fingre             LIKE pro_mae_promotor.fingre,
         fecha_certifi      LIKE pro_mae_promotor.fecha_certifi,
         fecha_baja         LIKE pro_mae_promotor.fecha_baja,
         Vagenc_cod         LIKE pro_mae_promotor.agenc_cod, 

         total_comision     LIKE com_comis_resumen.total_comision,
         vsubtot            LIKE com_comis_resumen.comis_calculada,
         vreten_isr         DECIMAL(6,2),
         vreten_iva         DECIMAL(6,2),
         vmto_pgo           DECIMAL(11,2)
      END RECORD, 
      

   nombre CHAR(50),

      vmenu     CHAR(01),
      aux_pausa	CHAR(1),
      HOY	DATE,
      SW_1      SMALLINT,
      cla_sel 	CHAR(1500), 
      cla_sel2	CHAR(500), 
      vaccion   smallint,
      cla_where CHAR(800), 
      vcomando  SMALLINT,
      opc 	CHAR(01),
      total     DECIMAL(12,2),
      pagada    DECIMAL(12,2),
      registros INTEGER,
      longitud  integer,

    nuevo RECORD
      codven             LIKE com_comis_resumen.codven,
      vmod_pago          CHAR(03),
      comision           LIKE com_comis_resumen.total_comision,
      subtotal_comi      LIKE com_comis_resumen.total_comision,
      vreten_isr         DECIMAL(6,2),
      vreten_iva         DECIMAL(6,2),
      vmto_pgo           DECIMAL(11,2),
      nombres            CHAR(60),
      nomina             LIKE pro_mae_promotor.codven,
      cencos             CHAR(10),
      tipo_promotor      LIKE com_comis_resumen.cod_tipo_prom,
      fecha_corte        LIKE com_comis_resumen.fecha_corte,
      fec_alta_emp       LIKE pro_mae_promotor.fingre,
      fec_alta_consar    LIKE pro_mae_promotor.fecha_certifi,
      fec_baja           LIKE pro_mae_promotor.fecha_baja
    END RECORD,

         g_grep_00                  ,
         g_grep_01                  ,
         g_grep_02                  ,
         g_grep_03                  ,
         g_grep_04                  ,
         g_grep_05                  ,
         g_grep_06                  ,
         g_grep_07                  ,
         g_grep_08                  ,
         g_grep_09                  ,
         g_grep_10                  ,
         g_grep_11                  ,
         g_grep_12                  CHAR(1000),

         vregis             INTEGER,
         sumcomi            DECIMAL(12,2),
         sumsub             DECIMAL(8,2),
         sumret_isr         DECIMAL(6,2),
         sumret_iva         DECIMAL(6,2),
         sumpago            DECIMAL(12,2),
         cont_de_registros  INTEGER,

         IVA                DECIMAL(6,2),
         SUB                DECIMAL(12,2),
         R_ISR              DECIMAL(6,2),
         R_IVA              DECIMAL(6,2),
         NETO               DECIMAL(12,2),

         vnom_jefe          CHAR(50),
         vcencos            CHAR(10),
         cla                CHAR(250),
         vcheca             INTEGER,
         xtipo_prom         LIKE com_comis_resumen.cod_tipo_prom,
         xagenc_cod         LIKE pro_mae_promotor.agenc_cod,
         xcodven            LIKE pro_mae_promotor.codven,
         xfecha_corte       LIKE com_comis_resumen.fecha_corte, 
         xnombres           LIKE pro_mae_promotor.nombres,
         xpaterno           LIKE pro_mae_promotor.paterno,
         xmaterno           LIKE pro_mae_promotor.materno,
         vvnom              CHAR(50),
         ban                SMALLINT,
         vnew_nom           CHAR(50),
         vdescr             CHAR(40)
        

END GLOBALS




MAIN
	OPTIONS PROMPT LINE LAST,
	        INPUT WRAP,
		ACCEPT KEY control-o
	
	DEFER INTERRUPT

        SELECT  ruta_rescate
        INTO    g_param_dis.ruta_rescate
        FROM    seg_modulo
        WHERE   modulo_cod='com'


        SELECT  codigo_afore,USER
        INTO    w_codigo_afore,g_usuario
        FROM    tab_afore_local

	LET HOY = TODAY
        LET vregis = 0

	OPEN WINDOW ventana_1 AT 2,2 WITH FORM "COMC0081" ATTRIBUTE( BORDER)
	DISPLAY " COMC008               CONSULTA  DE  COMISIONES                                " AT 3,1 ATTRIBUTE(REVERSE) 

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
        LET sw_1 = 0
	INITIALIZE g_reg TO NULL 

        for i=1 to 1
	   DISPLAY g_reg[i].* TO scr_1[i].*
        end for

	LET total = 0
	LET registros = 0
        DISPLAY registros,total TO scr_3.*

        CLEAR SCREEN
END FUNCTION

FUNCTION Inicializa2()
   LET cla_where=NULL
   LET cla_sel=NULL
   LET cla    =NULL
   INITIALIZE g_reg0.* TO NULL    --g_reg2.* ojo
   DISPLAY BY NAME g_reg0.*        --,g_reg2.* ojo
   LET vcheca = 0
   LET ban = 0
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
      CONSTRUCT BY NAME cla_where 
	 ON    com_comis_resumen.cod_tipo_prom,
               com_comis_resumen.coduni_n1,
               com_comis_resumen.codven,
               com_comis_resumen.fecha_corte,
               pro_mae_promotor.nombres, 
               pro_mae_promotor.paterno,
               pro_mae_promotor.materno

         ON KEY (ESC)

         LET xtipo_prom   = get_fldbuf(com_comis_resumen.cod_tipo_prom)
         LET xagenc_cod   = get_fldbuf(com_comis_resumen.coduni_n1)
         LET xcodven      = get_fldbuf(com_comis_resumen.codven)
         LET xfecha_corte = get_fldbuf(com_comis_resumen.fecha_corte)
         LET xnombres     = get_fldbuf(pro_mae_promotor.nombres)
         LET xpaterno     = get_fldbuf(pro_mae_promotor.paterno)
         LET xmaterno     = get_fldbuf(pro_mae_promotor.materno)

            LET vcomando = 2
            EXIT CONSTRUCT
         ON KEY (INTERRUPT)
            LET vcomando=1
            EXIT CONSTRUCT
       END CONSTRUCT

       IF vcomando = 1 THEN
          LET INT_FLAG = FALSE
          ERROR "Operacion abortada"
          LET vcomando = 0
          RETURN
       END IF

       WHENEVER ERROR CONTINUE
       DROP TABLE tt_inicial

       WHENEVER ERROR STOP

       -- DEFINE MIS QUERYS
       CALL define_querys_agrega_jefes()

       LET cla_sel =" SELECT fecha_corte, ",
                 " com_comis_resumen.codven cod1, ",
                 " com_comis_resumen.cod_tipo_prom, ",
                 " pro_mae_promotor.tipo_recibo, ",
                 " trim(pro_mae_promotor.paterno)||' '|| ",
                 " trim(pro_mae_promotor.materno)||' '|| ",
                 " trim(pro_mae_promotor.nombres) as nom, ",
                 " com_comis_resumen.coduni_n1, ",
                 " pro_mae_promotor.fingre, ",
                 " pro_mae_promotor.fecha_certifi, ",
                 " pro_mae_promotor.fecha_baja, ",
                 " pro_mae_promotor.codven cod2, ",
                 " SUM(com_comis_resumen.total_comision) tot ",
                 " FROM   com_comis_resumen ,pro_mae_promotor ",
                 " WHERE  com_comis_resumen.codven = cod_promotor ",
                 "AND ",cla_where CLIPPED,
            --   "AND com_comis_resumen.fecha_corte= ",xfecha_corte CLIPPED,
                 " GROUP BY 1,2,3,4,5,6,7,8,9,10 ",
                 " ORDER BY 1,2 ",
                 " INTO TEMP tt_inicial "

         PREPARE q1 FROM cla_sel
         EXECUTE q1


         SELECT COUNT(*) 
         INTO   vcheca
         FROM   tt_inicial

         IF vcheca = 0 
         THEN

            --VERIFICAMOS SI SE TRATA DE ALGUN JEFE
            --PARA LA BUSQUEDA ESPECIFICA DE UN JEFE SE REQUIERE:             
            --FECHA_CORTE Y NOMINA O NOMBRE
            IF  (xcodven        IS NOT NULL AND xcodven   <> ' ') AND
                (xfecha_corte   IS NOT NULL AND xfecha_corte <> ' ')
            THEN


               WHENEVER ERROR CONTINUE
               DROP TABLE tt_inicial

               WHENEVER ERROR STOP

               SELECT a.fecha_corte,
               a.codven cod1,
               a.cod_tipo_prom,
               ' ' tipo_recibo,
               c.nombre_resp_uni nom,
               a.coduni_n1 agenc_cod,
               ' ' fingre,
               ' ' fecha_certifi,
               ' ' fecha_baja,
               ' ' cod2,
               SUM(a.total_comision) tot
               FROM   com_comis_resumen a,com_respon_unidad c
               WHERE  a.codven = c.cod_resp_uni
               AND    a.fecha_corte = xfecha_corte
               AND    a.codven = xcodven
               AND    a.nivel <> 1
               GROUP BY 1,2,3,5,6
               INTO TEMP tt_inicial

               LET vcheca = 0
               SELECT COUNT(*)
               INTO   vcheca
               FROM   tt_inicial

               IF vcheca > 0 THEN
                  LET ban = 1
               END IF 

            ELSE
 
              IF  (xnombres       IS NOT NULL AND xnombres  <> ' ') AND
                  (xfecha_corte   IS NOT NULL AND xfecha_corte <> ' ') AND
                  vcheca = 0
              THEN              

               LET vvnom = ' '
               LET vvnom = xpaterno CLIPPED||' '||xmaterno CLIPPED||' '||
                           xnombres CLIPPED


               WHENEVER ERROR CONTINUE
               DROP TABLE tt_inicial

               WHENEVER ERROR STOP
               
               SELECT a.fecha_corte,
               a.codven cod1,
               a.cod_tipo_prom,
               ' ' tipo_recibo,
               c.nombre_resp_uni nom,
               a.coduni_n1 agenc_cod,
               ' ' fingre,
               ' ' fecha_certifi,
               ' ' fecha_baja,
               ' ' cod2,
               SUM(a.total_comision) tot
               FROM   com_comis_resumen a,com_respon_unidad c
               WHERE  a.codven = c.cod_resp_uni
               AND    a.fecha_corte = xfecha_corte
               AND    c.nombre_resp_uni = vvnom 
               AND    a.nivel <> 1
               GROUP BY 1,2,3,5,6
               INTO TEMP tt_inicial

                LET vcheca = 0
                SELECT COUNT(*)
                INTO   vcheca
                FROM   tt_inicial

                IF vcheca > 0 THEN
                   LET ban = 1
                END IF

              ELSE              

                 ERROR "NO EXISTEN DATOS CON ESTAS CONDICIONES..."
                 SLEEP 2
 
              END IF
            END IF
         END IF

         DISPLAY "                                                 [Ctrl-I] Imprime                                    " AT 8,01 ATTRIBUTE(REVERSE)  --@
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
                              ".RRH_",HOY USING "DDMMYYYY"
--                                "_",hora CLIPPED
    
    LET G_LISTA = g_param_dis.ruta_rescate CLIPPED,"/",g_usuario CLIPPED,
                              ".ARH_",HOY USING "DDMMYYYY"
--                                "_",hora CLIPPED


    -- PARA ARCHIVO
    START REPORT rpt_cuenta_arc TO G_LISTA
 
    -- PARA IMPRESION
    START REPORT rpt_cuenta_imp TO G_IMPRE

    LET cont_de_registros  = 0
    LET sumcomi            = 0           
    LET sumsub             = 0           
    LET sumret_isr         = 0           
    LET sumret_iva         = 0           
    LET sumpago            = 0           




   --Detalle de folios    -----------------------------------------------
   FOR i=1 TO 300 

        LET nuevo.codven           =  g_reg[i].codven
        CASE g_reg[i].vmod_pago
             WHEN 1
                  LET nuevo.vmod_pago = 'H'
             WHEN 2
                  LET nuevo.vmod_pago = 'F'
             WHEN 3
                  LET nuevo.vmod_pago = 'FM'
             OTHERWISE
                  LET nuevo.vmod_pago = ' '
        END CASE
        LET nuevo.comision         =  g_reg[i].total_comision
        LET nuevo.subtotal_comi    =  g_reg[i].vsubtot
        LET nuevo.vreten_isr       =  g_reg[i].vreten_isr
        LET nuevo.vreten_iva       =  g_reg[i].vreten_iva
        LET nuevo.vmto_pgo         =  g_reg[i].vmto_pgo 
        LET nuevo.nombres          =  g_reg[i].nombre
        LET nuevo.nomina           =  g_reg[i].Vcodven
        LET nuevo.cencos           =  g_reg[i].Vagenc_cod
        LET nuevo.tipo_promotor    =  g_reg[i].vtipo_pro
        LET nuevo.fecha_corte      =  g_reg[i].fcorte
        LET nuevo.fec_alta_emp     =  g_reg[i].fingre
        LET nuevo.fec_alta_consar  =  g_reg[i].fecha_certifi
        LET nuevo.fec_baja         =  g_reg[i].fecha_baja

   IF nuevo.codven IS NULL OR
      nuevo.codven = ' ' THEN
      EXIT FOR
   END IF

   LET vregis = vregis + 1
       
   -- Buscamos el nombre de los posibles jefes
   IF nuevo.nombres IS NULL OR nuevo.nombres= ' ' THEN

            -- Buscamos si se trata de un jefe
            LET vnom_jefe = ' '
            LET cla = " SELECT nombre_resp_uni,coduni_n1 ",
                      " FROM   com_respon_unidad,com_comis_resumen ",
                      " WHERE  cod_resp_uni = ",nuevo.codven, 
                      " AND    cod_resp_uni = com_comis_resumen.codven ",
                      " AND    fecha_corte  = '",nuevo.fecha_corte,"'" 

            PREPARE claexe3 FROM cla
            DECLARE cur3 SCROLL CURSOR FOR claexe3
            OPEN cur3
            FETCH FIRST cur3 INTO vnom_jefe,vcencos
            CLOSE cur3


            IF vnom_jefe IS NOT NULL THEN
               LET nuevo.nombres  = vnom_jefe
               LET nuevo.cencos   = vcencos
            ELSE
               LET nuevo.nombres  = ' '
               LET nuevo.cencos   = ' '
            END IF
   END IF


   --Se agrega al archivo plano separado por pipes, la descr de la suscur
   LET     vdescr = ' '
   SELECT  nombre_uni_n1 
   INTO    vdescr
   FROM    com_nivel1
   WHERE   coduni_n1 = nuevo.cencos 

   IF vdescr IS NULL OR vdescr = ' ' THEN

      SELECT  nombre_uni_n2
      INTO    vdescr
      FROM    com_nivel2
      WHERE   coduni_n2 = nuevo.cencos

      IF vdescr IS NULL  OR vdescr = ' 'THEN

         SELECT  nombre_uni_n4
         INTO    vdescr
         FROM    com_nivel4
         WHERE   coduni_n4 = nuevo.cencos

         IF vdescr IS NULL  OR vdescr = ' 'THEN
            LET vdescr = ' '
         END IF
      END IF
   END IF



   --PARA EL ARCHIVO
   OUTPUT TO REPORT rpt_cuenta_arc(nuevo.*,vdescr)
   
   --PARA LA IMPRESION
   OUTPUT TO REPORT rpt_cuenta_imp(nuevo.*)

   END FOR


   FINISH REPORT rpt_cuenta_arc
   FINISH REPORT rpt_cuenta_imp
   ERROR "LISTADO 1 GENERADO...."
   SLEEP 2
   ERROR ""



LET impresion = "lp ",G_IMPRE
RUN impresion

END FUNCTION

 
 
REPORT rpt_cuenta_arc(nuevo,vdes)

  DEFINE
   
    nuevo RECORD
      codven             LIKE com_comis_resumen.codven,
      vmod_pago          CHAR(03),
      comision           LIKE com_comis_resumen.total_comision,
      subtotal_comi      LIKE com_comis_resumen.total_comision,
      vreten_isr         DECIMAL(6,2),
      vreten_iva         DECIMAL(6,2),
      vmto_pgo           DECIMAL(11,2),
      nombres            CHAR(60),
      nomina             LIKE pro_mae_promotor.codven,
      cencos             CHAR(10),
      tipo_promotor      LIKE com_comis_resumen.cod_tipo_prom,
      fecha_corte        LIKE com_comis_resumen.fecha_corte,
      fec_alta_emp       LIKE pro_mae_promotor.fingre,
      fec_alta_consar    LIKE pro_mae_promotor.fecha_certifi,
      fec_baja           LIKE pro_mae_promotor.fecha_baja
    END RECORD,

         nombre             CHAR(60),
         vdes               CHAR(40)


  FORMAT
     PAGE HEADER
      PRINT COLUMN 001,"Num promotor |",
            COLUMN 014,"Nombre promotor |",
            COLUMN 030,"Nomina |",
            COLUMN 037,"Tipo promo|", 
            COLUMN 048,"Centro costos|",

            COLUMN 062,"Descr cencos|",

            COLUMN 075,"Fecha corte|",
            COLUMN 087,"Fecha alta empre |",
            COLUMN 105,"Fec alta consar |",
            COLUMN 122,"Fec baja |",
            COLUMN 132,"Comision |",
            COLUMN 142,"Subtotal |",
            COLUMN 152,"Reten ISR|",
            COLUMN 162,"Reten IVA|",
            COLUMN 172,"Pago comi|"

      ON EVERY ROW

      PRINT COLUMN 001,nuevo.codven CLIPPED,                    "|",
            COLUMN 014,nuevo.nombres CLIPPED,                   "|",
            COLUMN 065,nuevo.nomina CLIPPED,                    "|",
            COLUMN 076,nuevo.tipo_promotor,                     "|",
            COLUMN 083,nuevo.cencos,                            "|",
            COLUMN 095,vdes   CLIPPED,                          "|", 
            COLUMN 127,nuevo.fecha_corte  USING 'DD/MM/YYYY',   "|",
            COLUMN 138,nuevo.fec_alta_emp USING 'DD/MM/YYYY',   "|",
            COLUMN 150,nuevo.fec_alta_consar USING 'DD/MM/YYYY',"|",
            COLUMN 162,nuevo.fec_baja     USING 'DD/MM/YYYY',   "|",
            COLUMN 173,nuevo.comision,                          "|",
            COLUMN 189,nuevo.subtotal_comi,                     "|",
            COLUMN 206,nuevo.vreten_isr,                        "|",
            COLUMN 216,nuevo.vreten_iva,                        "|",
            COLUMN 225,nuevo.vmto_pgo USING "#######&.&&",      "|"

  END REPORT
 
 





REPORT rpt_cuenta_imp(nuevo)

  DEFINE
   
    nuevo RECORD
      codven             LIKE com_comis_resumen.codven,
      vmod_pago          CHAR(03),
      comision           LIKE com_comis_resumen.total_comision,
      subtotal_comi      LIKE com_comis_resumen.total_comision,
      vreten_isr         DECIMAL(6,2),
      vreten_iva         DECIMAL(6,2),
      vmto_pgo           DECIMAL(11,2),
      nombres            CHAR(60),
      nomina             LIKE pro_mae_promotor.codven,
      cencos             CHAR(10),
      tipo_promotor      LIKE com_comis_resumen.cod_tipo_prom,
      fecha_corte        LIKE com_comis_resumen.fecha_corte,
      fec_alta_emp       LIKE pro_mae_promotor.fingre,
      fec_alta_consar    LIKE pro_mae_promotor.fecha_certifi,
      fec_baja           LIKE pro_mae_promotor.fecha_baja
    END RECORD,


         nombre             CHAR(60)


 OUTPUT
      TOP MARGIN 0
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   90

  FORMAT
     PAGE HEADER

      PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
      PRINT COLUMN 01,'\033(s7B',"Pantalla COMC008",'\033(s0B',"                                                                                                                          ",'\033(s7B',"Fecha : ",'\033(s0B',today USING "dd-mm-yyyy"
      SKIP 2 LINE
      PRINT COLUMN 80,'\033(s7B',"Reporte de Comisiones",'\033(s0B'
      SKIP 1 LINE


      PRINT COLUMN  001,'__________________________________________________',
                        '__________________________________________________',
                        '__________________________________________________',
                        '______________________________________'

      SKIP 1 LINE


      PRINT COLUMN 01,'\033(s7B',"Num promo  Nombre promotor                    Nomina  T.pro Ccencos  Fec corte  Fec al emp  Fec al Con   Fec Baja   Comision   Subtot   Reten ISR  Reten IVA  Pgo Comisi",'\033(s0B'
      PRINT COLUMN  001,'__________________________________________________',
                        '__________________________________________________',
                        '__________________________________________________',
                        '______________________________________'
            SKIP 1 LINE


      ON EVERY ROW





      PRINT COLUMN 001,nuevo.codven CLIPPED,
            COLUMN 012,nuevo.nombres CLIPPED,
            COLUMN 048,nuevo.nomina USING "######",
            COLUMN 056,nuevo.tipo_promotor    USING "##",
            COLUMN 063,nuevo.cencos CLIPPED,          
            COLUMN 070,nuevo.fecha_corte      USING 'DD/MM/YYYY',
            COLUMN 082,nuevo.fec_alta_emp     USING 'DD/MM/YYYY',
            COLUMN 093,nuevo.fec_alta_consar  USING 'DD/MM/YYYY',
            COLUMN 105,nuevo.fec_baja         USING 'DD/MM/YYYY',
            COLUMN 116,nuevo.comision         USING "&&&&&&&&.&&",
            COLUMN 128,nuevo.subtotal_comi    USING "&&&&&&.&&",
            COLUMN 138,nuevo.vreten_isr       USING "&&&&&&.&&",
            COLUMN 148,nuevo.vreten_iva       USING "&&&&&&.&&",
            COLUMN 158,nuevo.vmto_pgo         USING "&&&&&&&&.&&"
             --72
 
            LET sumcomi    = sumcomi    + nuevo.comision
            LET sumsub     = sumsub     + nuevo.subtotal_comi
            LET sumret_isr = sumret_isr + nuevo.vreten_isr
            LET sumret_iva = sumret_iva + nuevo.vreten_iva
            LET sumpago    = sumpago    + nuevo.vmto_pgo

            LET cont_de_registros = cont_de_registros + 1
 
      ON LAST ROW

       PRINT COLUMN 114,'__________________________________________________________'
       SKIP 1 LINE
       PRINT COLUMN 001,"TOTAL DE REGISTROS: ",cont_de_registros,"                                                                     TOTALES:       ",sumcomi USING '&&&&&&&&.&&'," ",sumsub USING '&&&&&&.&&'," ",sumret_isr USING '&&&&&&.&&'," ",sumret_iva USING '&&&&&&.&&'," ",sumpago USING '&&&&&&&&.&&'

  END REPORT

 





FUNCTION ver_arreglo()
   DEFINE
    pat    CHAR(40),
    mat    char(40),
    nom    char(40),
    pos    SMALLINT,
    fhasta DATE,
    i      SMALLINT

    LET total = 0
    LET pagada = 0
    LET registros = 0

    CALL define_querys_agrega_jefes()

    --AQUI SE DECIDEN QUE JEFES ENTRAN CONFORME A LOS ARGUMENTOS DADOS
    IF (xtipo_prom     IS NULL OR xtipo_prom     = ' ') AND
       (xagenc_cod     IS NULL OR xagenc_cod     = ' ') AND
       (xcodven        IS NULL OR xcodven        = ' ') AND
       (xnombres       IS NULL OR xnombres       = ' ') AND
       (xpaterno       IS NULL OR xpaterno       = ' ') AND
       (xmaterno       IS NULL OR xmaterno       = ' ') AND 
        xfecha_corte   IS NOT NULL AND xfecha_corte <> ' ' AND
        ban = 0        -- si ban=0 entonces solo se agrega todo 
    THEN
  

        PREPARE  query_0    FROM	g_grep_00
        EXECUTE  query_0    USING	xfecha_corte


    ELSE

      IF ban = 0 THEN  --si esta ban permanecio en 0 no se individualizo
                       --la consulta
         
         ---- tipo_prom con todos ----------------------------------------
         ---- tipo_prom y fecha_corte no nulas
         IF xtipo_prom     IS NOT NULL	AND 
             xagenc_cod    IS NULL	AND 
             xcodven       IS NULL	AND
             xfecha_corte  IS NOT NULL	AND
            (xnombres      IS NULL	AND 
             xpaterno      IS NULL	AND
             xmaterno      IS NULL	)
        THEN

  
             PREPARE  query_1    FROM	g_grep_01
             EXECUTE  query_1    USING	xfecha_corte,xtipo_prom
        END IF


        ---- tipo_prom,cencos y fecha_corte no nulas
        IF xtipo_prom     IS NOT NULL	AND 
           xagenc_cod     IS NOT NULL	AND 
           xcodven        IS NULL	AND
           xfecha_corte   IS NOT NULL	AND
          (xnombres       IS NULL	AND 
           xpaterno       IS NULL	AND
           xmaterno       IS NULL	)
        THEN
           PREPARE  query_2    FROM	g_grep_02
           EXECUTE  query_2    USING	xfecha_corte, xtipo_prom, xagenc_cod 
        END IF

        ---- tipo_prom,cencos, nomina y fecha_corte no nulas
        IF xtipo_prom     IS NOT NULL	AND 
           xagenc_cod   IS NOT NULL	AND 
           xcodven         IS NOT NULL	AND
           xfecha_corte  IS NOT NULL	AND
          (xnombres      IS NULL	AND 
           xpaterno        IS NULL	AND
           xmaterno       IS NULL	)
       THEN
           PREPARE  query_3    FROM	g_grep_03
           EXECUTE  query_3    USING	xfecha_corte, xtipo_prom, xagenc_cod
       END IF

       ---- tipo_prom,cencos,nomina, nombre y fecha_corte no nulas
       IF  xtipo_prom     IS NOT NULL	AND 
           xagenc_cod   IS NOT NULL	AND 
           xcodven         IS NOT NULL	AND
           xfecha_corte  IS NOT NULL	AND
          (xnombres    IS NOT NULL	AND 
           xpaterno      IS NOT NULL	AND
           xmaterno     IS NOT NULL	)
       THEN
           LET vnew_nom = xpaterno CLIPPED||' '||xmaterno CLIPPED||
                          ' '||xnombres CLIPPED
           PREPARE  query_4    FROM	g_grep_04
           EXECUTE  query_4    USING	xfecha_corte, xtipo_prom, xagenc_cod, 
                                        xcodven, vnew_nom 

       END IF




       ---- cencos con todos --------------------------------------------------
       ---- fecha_corte y cencos  no nulas

       IF  xtipo_prom     IS NULL	AND 
           xagenc_cod   IS NOT NULL	AND 
           xcodven         IS NULL	AND
           xfecha_corte  IS NOT NULL	AND
          (xnombres      IS NULL	AND 
           xpaterno        IS NULL	AND
           xmaterno       IS NULL	)
       THEN
           PREPARE  query_5    FROM	g_grep_05
           EXECUTE  query_5    USING	xfecha_corte, xagenc_cod
       END IF



       ---- cencos, nomina y fecha_corte no nulas
       IF  xtipo_prom     IS NULL	AND 
           xagenc_cod   IS NOT NULL	AND 
           xcodven         IS NOT NULL	AND
           xfecha_corte  IS NOT NULL	AND
          (xnombres      IS NULL	AND 
           xpaterno        IS NULL	AND
           xmaterno       IS NULL	)
       THEN
           PREPARE  query_6    FROM	g_grep_06
           EXECUTE  query_6    USING	xfecha_corte,  xagenc_cod, xcodven
        END IF



       ---- cencos, nomina y fecha_corte no nulas
       IF xtipo_prom     IS NULL	AND 
           xagenc_cod   IS NOT NULL	AND 
           xcodven         IS NOT NULL	AND
           xfecha_corte  IS NOT NULL	AND
          (xnombres    IS NOT NULL	AND 
           xpaterno      IS NOT NULL	AND
           xmaterno     IS NOT NULL	)
       THEN

           LET vnew_nom = xpaterno CLIPPED||' '||xmaterno CLIPPED||' '
                          ||xnombres CLIPPED
           PREPARE  query_7    FROM	g_grep_07
           EXECUTE  query_7    USING	xfecha_corte, xagenc_cod, 
                                        xcodven, vnew_nom
       END IF


       ---- nomina con todos ---------------------------------------------------
       --fecha_corte y nomina  no nulas

       IF xtipo_prom     IS NULL	AND 
         xagenc_cod   IS NULL	AND 
           xcodven         IS NOT NULL	AND
           xfecha_corte  IS NOT NULL	AND
          (xnombres      IS NULL	AND 
           xpaterno        IS NULL	AND
           xmaterno       IS NULL	)
       THEN
           PREPARE  query_08    FROM	g_grep_08
           EXECUTE  query_08    USING	xfecha_corte, xcodven
        END IF


       --fecha_corte, tipo_prom, nomina  no nulas

       IF xtipo_prom     IS NOT NULL	AND 
           xagenc_cod   IS NULL	AND 
           xcodven         IS NOT NULL	AND
           xfecha_corte  IS NOT NULL	AND
          (xnombres      IS NULL	AND 
           xpaterno        IS NULL	AND
           xmaterno       IS NULL	)
       THEN
           PREPARE  query_09    FROM	g_grep_09
           EXECUTE  query_09    USING	xfecha_corte, xtipo_prom, xcodven
        END IF

       ---- nombre con todos -----------------------------------------------
       --fecha_corte y nombre  no nulas

       IF xtipo_prom     IS NULL	AND 
           xagenc_cod   IS NULL	AND 
           xcodven         IS NULL	AND
           xfecha_corte  IS NOT NULL	AND
          (xnombres    IS NOT NULL	AND 
           xpaterno      IS NOT NULL	AND
           xmaterno     IS NOT NULL	)
       THEN
           LET vnew_nom = xpaterno CLIPPED||' '||xmaterno CLIPPED||' '||
                          xnombres CLIPPED
           PREPARE  query_10    FROM	g_grep_10
           EXECUTE  query_10    USING	xfecha_corte, vnew_nom
        END IF


       --fecha_corte, tipo_prom y nombre  no nulas

       IF xtipo_prom     IS NOT NULL	AND 
           xagenc_cod   IS NULL	AND 
           xcodven         IS NULL	AND
           xfecha_corte  IS NOT NULL	AND
          (xnombres    IS NOT NULL	AND 
           xpaterno      IS NOT NULL	AND
           xmaterno     IS NOT NULL	)
       THEN
           LET vnew_nom = xpaterno CLIPPED||' '||xmaterno CLIPPED||' '||
                          xnombres CLIPPED
           PREPARE  query_11    FROM	g_grep_11
           EXECUTE  query_11    USING	xfecha_corte, xtipo_prom, vnew_nom
        END IF


       --fecha_corte, tipo_prom, cencos y nombre  no nulas

       IF xtipo_prom     IS NOT NULL	AND 
           xagenc_cod   IS NOT NULL	AND 
           xcodven         IS NULL	AND
           xfecha_corte  IS NOT NULL	AND
          (xnombres    IS NOT NULL	AND 
           xpaterno      IS NOT NULL	AND
           xmaterno     IS NOT NULL	)
       THEN
           LET vnew_nom = xpaterno CLIPPED||' '||xmaterno CLIPPED||' '||
                          xnombres CLIPPED
           PREPARE  query_12    FROM	g_grep_12
           EXECUTE  query_12    USING	xfecha_corte, xtipo_prom,
                                        xagenc_cod, vnew_nom
       END IF


      -----------------------------------------------------------------------
      -----------------------------------------------------------------------
      END IF  --de ban
    END IF

         LET cla_sel2="SELECT * FROM tt_inicial "
 
         ERROR "Buscando Informacion"   
         PREPARE claexe FROM cla_sel2
         DECLARE cursor_1111 CURSOR FOR claexe
	 LET pos = 1
	 FOREACH cursor_1111 INTO g_reg88[pos].*

             LET   IVA    = 0
             LET   SUB    = 0
             LET   R_ISR  = 0
             LET   R_IVA  = 0
             LET   NETO   = 0
             
             CASE  g_reg88[pos].vmod_pago
                       WHEN 1
                            LET IVA   = g_reg88[pos].total_comision * 0.15
                            LET SUB   = g_reg88[pos].total_comision + IVA 
                            LET R_ISR = g_reg88[pos].total_comision * 0.10 
                            LET R_IVA = g_reg88[pos].total_comision * 0.10 
                            LET NETO  = SUB - R_ISR - R_IVA 
                       WHEN 2
                            LET IVA   = g_reg88[pos].total_comision * 0.15
                            LET SUB   = g_reg88[pos].total_comision + IVA 
                            LET R_ISR = 0 
                            LET R_IVA = g_reg88[pos].total_comision * 0.10 
                            LET NETO  = SUB - R_IVA 
                       WHEN 3
                            LET IVA   = g_reg88[pos].total_comision * 0.15
                            LET SUB   = g_reg88[pos].total_comision + IVA 
                            LET R_ISR = 0 
                            LET R_IVA = 0 
                            LET NETO  = SUB + IVA 
                 OTHERWISE 
                            LET SUB   = 0
                            LET R_ISR = 0 
                            LET R_IVA = 0 
                            LET NETO  = g_reg88[pos].total_comision 
             END CASE
                            
             LET g_reg[pos].fcorte         = g_reg88[pos].fcorte
             LET g_reg[pos].codven         = g_reg88[pos].codven
             LET g_reg[pos].vtipo_pro      = g_reg88[pos].cod_tipo_prom
             LET g_reg[pos].vmod_pago      = g_reg88[pos].vmod_pago
             LET g_reg[pos].nombre         = g_reg88[pos].nombre
             LET g_reg[pos].Vagenc_cod     = g_reg88[pos].Vagenc_cod
             LET g_reg[pos].fingre         = g_reg88[pos].fingre
             LET g_reg[pos].fecha_certifi  = g_reg88[pos].fecha_certifi
             LET g_reg[pos].fecha_baja     = g_reg88[pos].fecha_baja
             LET g_reg[pos].Vcodven        = g_reg88[pos].Vcodven
             LET g_reg[pos].total_comision = g_reg88[pos].total_comision
             LET g_reg[pos].vsubtot        = SUB 
             LET g_reg[pos].vreten_isr     = R_ISR 
             LET g_reg[pos].vreten_iva     = R_IVA
             LET g_reg[pos].vmto_pgo       = NETO

	     LET total = total + g_reg[pos].vmto_pgo
                                      
             LET registros = registros  + 1
	     LET pos = pos + 1
             IF pos >= 9000 THEN
                 ERROR "Sobrepaso la capacidad del arreglo"
                 EXIT FOREACH
             END IF 
         END FOREACH

         DISPLAY registros,total TO scr_3.*

	 ERROR ""

	 CALL  SET_COUNT(pos-1)

	 IF (pos-1) >= 1 THEN
	    DISPLAY ARRAY g_reg TO scr_1.*

               ON KEY (CONTROL-I)
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

##----------------------------------------------------------------------------
## DEFINICION DE QUERYS
##----------------------------------------------------------------------------
FUNCTION define_querys_agrega_jefes()

--Entraran todos los jefes
LET g_grep_00 ="INSERT INTO tt_inicial ",
       "SELECT com_comis_resumen.fecha_corte,",
       "com_comis_resumen.codven cod1,",
       "com_comis_resumen.cod_tipo_prom,",
       "' ' tipo_recibo,",
       "com_respon_unidad.nombre_resp_uni nom,",
       "com_comis_resumen.coduni_n1 agenc_cod,",
       "' ' fingre,",
       "' ' fecha_certifi,",
       "' ' fecha_baja,",
       "' ' cod2,",
       "SUM(com_comis_resumen.total_comision) tot ",
       "FROM   com_comis_resumen,com_respon_unidad ",
       "WHERE  com_comis_resumen.codven = com_respon_unidad.cod_resp_uni ",
       "AND    com_comis_resumen.codven NOT IN ",
       "                           (SELECT distinct cod1 FROM tt_inicial) ",
       "AND    com_comis_resumen.fecha_corte = ? ",
       "AND    com_comis_resumen.nivel <> 1 ",
       "GROUP BY 1,2,3,5,6 "


--para tipo_promo
LET g_grep_01 =" INSERT INTO tt_inicial ", 
                 "SELECT a.fecha_corte,",
                 "a.codven cod1,",
                 "a.cod_tipo_prom,",
                 "' ' tipo_recibo,",
                 "b.nombre_resp_uni nom,",
                 "a.coduni_n1 agenc_cod,",
                 "' ' fingre,",
                 "' ' fecha_certifi,",
                 "' ' fecha_baja,",
                 "' ' codven,",
                 "SUM(a.total_comision) tot ",
                 "FROM   com_comis_resumen a,com_respon_unidad b ",
                 "WHERE  a.codven = b.cod_resp_uni ",
                 "AND a.fecha_corte = ? ",
                 " AND a.cod_tipo_prom = ? ",
                 " GROUP BY 1,2,3,5,6 "

LET g_grep_02 ="INSERT INTO tt_inicial ",
                 "SELECT a.fecha_corte,",
                 "a.codven cod1,",
                 "a.cod_tipo_prom,",
                 "' ' tipo_recibo,",
                 "b.nombre_resp_uni nom,",
                 "a.coduni_n1 agenc_cod,",
                 "' ' fingre,",
                 "' ' fecha_certifi,",
                 "' ' fecha_baja,",
                 "' ' codven,",
                 "SUM(a.total_comision) tot ",
                 "FROM   com_comis_resumen a,com_respon_unidad b ",
                 "WHERE  a.codven = b.cod_resp_uni ",
                 "AND a.fecha_corte = ? ",
                 " AND a.cod_tipo_prom = ? ",
                 " AND a.coduni_n1 = ? ",
                 " GROUP BY 1,2,3,5,6 "


LET g_grep_03 ="INSERT INTO tt_inicial ",
                 "SELECT a.fecha_corte,",
                 "a.codven cod1,",
                 "a.cod_tipo_prom,",
                 "' ' tipo_recibo,",
                 "b.nombre_resp_uni nom,",
                 "a.coduni_n1 agenc_cod,",
                 "' ' fingre,",
                 "' ' fecha_certifi,",
                 "' ' fecha_baja,",
                 "' ' codven,",
                 "SUM(a.total_comision) tot ",
                 "FROM   com_comis_resumen a,com_respon_unidad b ",
                 "WHERE  a.codven = b.cod_resp_uni ",
                 "AND a.fecha_corte = ? ",
                 " AND a.cod_tipo_prom = ? ",
                 " AND a.coduni_n1 = ? ",
                 " AND a.codven = ? ",
                 " GROUP BY 1,2,3,5,6 "


LET g_grep_04 ="INSERT INTO tt_inicial ",
                 "SELECT a.fecha_corte,",
                 "a.codven cod1,",
                 "a.cod_tipo_prom,",
                 "' ' tipo_recibo,",
                 "b.nombre_resp_uni nom,",
                 "a.coduni_n1 agenc_cod,",
                 "' ' fingre,",
                 "' ' fecha_certifi,",
                 "' ' fecha_baja,",
                 "' ' codven,",
                 "SUM(a.total_comision) tot ",
                 "FROM   com_comis_resumen a,com_respon_unidad b ",
                 "WHERE  a.codven = b.cod_resp_uni ",
                 "AND a.fecha_corte = ? ",
                 " AND a.cod_tipo_prom = ? ",
                 " AND a.coduni_n1 = ? ",
                 " AND a.codven = ? ",
                 " AND b.nombre_resp_uni = ? ",
                 " GROUP BY 1,2,3,5,6 "



--para cencos
--.............................................................................
LET g_grep_05 ="INSERT INTO tt_inicial ",
                 "SELECT a.fecha_corte,",
                 "a.codven cod1,",
                 "a.cod_tipo_prom,",
                 "' ' tipo_recibo,",
                 "b.nombre_resp_uni nom,",
                 "a.coduni_n1 agenc_cod,",
                 "' ' fingre,",
                 "' ' fecha_certifi,",
                 "' ' fecha_baja,",
                 "' ' codven,",
                 "SUM(a.total_comision) tot ",
                 "FROM   com_comis_resumen a,com_respon_unidad b ",
                 "WHERE  a.codven = b.cod_resp_uni ",
                 "AND a.fecha_corte = ? ",
                 " AND a.coduni_n1 = ? ",
                 " GROUP BY 1,2,3,5,6 "


LET g_grep_06 ="INSERT INTO tt_inicial ",
                 "SELECT a.fecha_corte,",
                 "a.codven cod1,",
                 "a.cod_tipo_prom,",
                 "' ' tipo_recibo,",
                 "b.nombre_resp_uni nom,",
                 "a.coduni_n1 agenc_cod,",
                 "' ' fingre,",
                 "' ' fecha_certifi,",
                 "' ' fecha_baja,",
                 "' ' codven,",
                 "SUM(a.total_comision) tot ",
                 "FROM   com_comis_resumen a,com_respon_unidad b ",
                 "WHERE  a.codven = b.cod_resp_uni ",
                 "AND a.fecha_corte = ? ",
                 " AND a.coduni_n1 = ? ",
                 " AND a.codven = ? ",
                 " GROUP BY 1,2,3,5,6 "

LET g_grep_07 ="INSERT INTO tt_inicial ",
                 "SELECT a.fecha_corte,",
                 "a.codven cod1,",
                 "a.cod_tipo_prom,",
                 "' ' tipo_recibo,",
                 "b.nombre_resp_uni nom,",
                 "a.coduni_n1 agenc_cod,",
                 "' ' fingre,",
                 "' ' fecha_certifi,",
                 "' ' fecha_baja,",
                 "' ' codven,",
                 "SUM(a.total_comision) tot ",
                 "FROM   com_comis_resumen a,com_respon_unidad b ",
                 "WHERE  a.codven = b.cod_resp_uni ",
                 "AND a.fecha_corte = ? ",
                 " AND a.agenc_cod = ? ",
                 " AND a.codven = ? ",
                 " AND b.nombre_resp_uni = ? ",
                 " GROUP BY 1,2,3,5,6 "

--para nomina
--..............................................................................

LET g_grep_08 ="INSERT INTO tt_inicial ",
                 "SELECT a.fecha_corte,",
                 "a.codven cod1,",
                 "a.cod_tipo_prom,",
                 "' ' tipo_recibo,",
                 "b.nombre_resp_uni nom,",
                 "a.coduni_n1 agenc_cod,",
                 "' ' fingre,",
                 "' ' fecha_certifi,",
                 "' ' fecha_baja,",
                 "' ' codven,",
                 "SUM(a.total_comision) tot ",
                 "FROM   com_comis_resumen a,com_respon_unidad b ",
                 "WHERE  a.codven = b.cod_resp_uni ",
                 "AND a.fecha_corte = ? ",
                 " AND a.codven = ? ",
                 " GROUP BY 1,2,3,5,6 "


LET g_grep_09 ="INSERT INTO tt_inicial ",
                 "SELECT a.fecha_corte,",
                 "a.codven cod1,",
                 "a.cod_tipo_prom,",
                 "' ' tipo_recibo,",
                 "b.nombre_resp_uni nom,",
                 "a.coduni_n1 agenc_cod,",
                 "' ' fingre,",
                 "' ' fecha_certifi,",
                 "' ' fecha_baja,",
                 "' ' codven,",
                 "SUM(a.total_comision) tot ",
                 "FROM   com_comis_resumen a,com_respon_unidad b ",
                 "WHERE  a.codven = b.cod_resp_uni ",
                 "AND a.fecha_corte = ? ",
                 "AND a.cod_tipo_prom = ? ",
                 " AND a.codven = ? ",
                 " GROUP BY 1,2,3,5,6 "


--para nombre
--..............................................................................

LET g_grep_10 ="INSERT INTO tt_inicial ",
                 "SELECT a.fecha_corte,",
                 "a.codven cod1,",
                 "a.cod_tipo_prom,",
                 "' ' tipo_recibo,",
                 "b.nombre_resp_uni nom,",
                 "a.coduni_n1 agenc_cod,",
                 "' ' fingre,",
                 "' ' fecha_certifi,",
                 "' ' fecha_baja,",
                 "' ' codven,",
                 "SUM(a.total_comision) tot ",
                 "FROM   com_comis_resumen a,com_respon_unidad b ",
                 "WHERE  a.codven = b.cod_resp_uni ",
                 "AND a.fecha_corte = ? ",
                 " AND b.nombre_resp_uni = ? ",
                 " GROUP BY 1,2,3,5,6 "

LET g_grep_11 ="INSERT INTO tt_inicial ",
                 "SELECT a.fecha_corte,",
                 "a.codven cod1,",
                 "a.cod_tipo_prom,",
                 "' ' tipo_recibo,",
                 "b.nombre_resp_uni nom,",
                 "a.coduni_n1 agenc_cod,",
                 "' ' fingre,",
                 "' ' fecha_certifi,",
                 "' ' fecha_baja,",
                 "' ' codven,",
                 "SUM(a.total_comision) tot ",
                 "FROM   com_comis_resumen a,com_respon_unidad b ",
                 "WHERE  a.codven = b.cod_resp_uni ",
                 "AND a.fecha_corte = ? ",
                 "AND a.cod_tipo_prom = ? ",
                 " AND b.nombre_resp_uni = ? ",
                 " GROUP BY 1,2,3,5,6 "


LET g_grep_12 ="INSERT INTO tt_inicial ",
                 "SELECT a.fecha_corte,",
                 "a.codven cod1,",
                 "a.cod_tipo_prom,",
                 "' ' tipo_recibo,",
                 "b.nombre_resp_uni nom,",
                 "a.coduni_n1 agenc_cod,",
                 "' ' fingre,",
                 "' ' fecha_certifi,",
                 "' ' fecha_baja,",
                 "' ' codven,",
                 "SUM(a.total_comision) tot ",
                 "FROM   com_comis_resumen a,com_respon_unidad b ",
                 "WHERE  a.codven = b.cod_resp_uni ",
                 "AND a.fecha_corte = ? ",
                 "AND a.cod_tipo_prom = ? ",
                 "AND a.agenc_cod = ? ",
                 " AND b.nombre_resp_uni = ? ",
                 " GROUP BY 1,2,3,5,6 "


END FUNCTION
