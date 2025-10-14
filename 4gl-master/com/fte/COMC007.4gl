###############################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )			       #
#Owner             => E.F.P.        					       #
#Programa          => COMC007  
#Descripcion       => CONSULTA COMISIONES DE PROMOTORES SOLO EN SU SUCURSAL
#Fecha             => 02 Abril 2004.     				       #
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
        agenc_cod     CHAR(10),
	fecha_corte   DATE
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
      cla_sel 	CHAR(500), 
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
         sucursal           CHAR(10),
         vvalor             CHAR(01)
        

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

      --LET g_param_dis.ruta_listados="/home/aramirez/ACTINVE/new_version" --ojo
      --LET g_param_dis.ruta_listados="/safre/com/fte/alex/pago_comer" --ojo

        SELECT  codigo_afore,USER
        INTO    w_codigo_afore,g_usuario
        FROM    tab_afore_local

	LET HOY = TODAY
        LET vregis = 0

	OPEN WINDOW ventana_1 AT 2,2 WITH FORM "COMC0071" ATTRIBUTE( BORDER)
	DISPLAY " COMC007               CONSULTA  DE  COMISIONES                                " AT 3,1 ATTRIBUTE(REVERSE) 

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

      CONSTRUCT cla_where 
	 ON    b.agenc_cod,
               a.fecha_corte
         FROM  pro_mae_promotor.agenc_cod,
               com_comis_resumen.fecha_corte


         ON KEY (ESC)
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
        

       -- LAS SIGUI VALIDACION ES PORQUE ES ATRAVES DEL COD PROMOTOR
       -- QUE SE REALIZA LA BUSQUEDA

       IF cla_where = " 1=1" THEN
          ERROR "Es preciso ingresar la sucursal"
          SLEEP 3
          LET INT_FLAG = FALSE
          ERROR ""
          LET vcomando = 0
          RETURN
       END IF

       IF cla_where[1,13] = "a.fecha_corte" THEN
          ERROR "Es preciso ingresar la sucursal "
          SLEEP 3
          LET INT_FLAG = FALSE
          ERROR ""
          LET vcomando = 0
          RETURN
       END IF

       IF cla_where[1,11] = "b.agenc_cod" THEN

          LET cla_sel= "SELECT 'x' ", 
                      "FROM   com_comis_resumen a,pro_mae_promotor b ",
                      " WHERE a.codven = b.cod_promotor ",
                      " AND ",cla_where CLIPPED,
                      " AND a.nivel = 1 ",
                      " GROUP BY 1 "

          PREPARE valida FROM cla_sel
          EXECUTE valida INTO vvalor 

          IF vvalor IS NULL THEN
             ERROR "La sucursal no existe "
             SLEEP 3
             LET INT_FLAG = FALSE
             ERROR ""
             LET vcomando = 0
             RETURN
          END IF
       END IF


       LET cla_sel ="SELECT b.agenc_cod,",
			   "a.fecha_corte ",
                    "FROM   com_comis_resumen a,pro_mae_promotor b ",
                    " WHERE a.codven = b.cod_promotor ", 
                    "AND ",cla_where CLIPPED,
                    " AND a.nivel = 1 ",
                 " GROUP BY 1,2 ",
                    " ORDER  BY 1,2" CLIPPED

         PREPARE claexe2 FROM cla_sel
         DECLARE cursor_2 SCROLL CURSOR FOR claexe2
         OPEN cursor_2
         CALL primer_row()

   DISPLAY "                                                 [Ctrl-I] Imprime                                    " AT 8,01 ATTRIBUTE(REVERSE)  --@

	 CALL ver_arreglo()
END FUNCTION

FUNCTION primer_row()
   FETCH FIRST cursor_2 INTO g_reg0.*      --,g_reg2.* ojo
   IF STATUS=100 THEN
      ERROR "No hay registros en esta direccion"
   ELSE
      CALL despliega_row()
   END IF
END FUNCTION

FUNCTION despliega_row()
 --DISPLAY BY NAME g_reg0.*     --g_reg2 ojo
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
                              ".REPPROD_",HOY USING "DD-MM-YYYY",
                                "_",hora CLIPPED
    
    LET G_LISTA = g_param_dis.ruta_rescate CLIPPED,"/",g_usuario CLIPPED,
                              ".ARCPROD_",HOY USING "DD-MM-YYYY",
                                "_",hora CLIPPED
   

    --LET G_LISTA = g_param_dis.ruta_listados CLIPPED,"/tro7"  --ojo
    --LET G_IMPRE = g_param_dis.ruta_listados CLIPPED,"/tro77"  --ojo

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

   --PARA EL ARCHIVO
   OUTPUT TO REPORT rpt_cuenta_arc(nuevo.*)
   
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

 
 
REPORT rpt_cuenta_arc(nuevo)

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


  FORMAT
     PAGE HEADER
      PRINT COLUMN 001,"Num promotor |",
            COLUMN 014,"Nombre promotor |",
            COLUMN 030,"Nomina |",
            COLUMN 037,"Tipo promo|", 
            COLUMN 048,"Centro costos|",
            COLUMN 062,"Fecha corte|",
            COLUMN 074,"Fecha alta empre |",
            COLUMN 091,"Fec alta consar |",
            COLUMN 107,"Fec baja |",
            COLUMN 117,"Comision |",
            COLUMN 127,"Subtotal |",
            COLUMN 137,"Reten ISR|",
            COLUMN 147,"Reten IVA|",
            COLUMN 157,"Pago comi|"

      ON EVERY ROW

      PRINT COLUMN 001,nuevo.codven CLIPPED,                    "|",
            COLUMN 014,nuevo.nombres CLIPPED,                   "|",
            COLUMN 065,nuevo.nomina,                            "|",
            COLUMN 076,nuevo.tipo_promotor,                     "|",
            COLUMN 083,nuevo.cencos,                            "|",
            COLUMN 095,nuevo.fecha_corte  USING 'DD/MM/YYYY',   "|",
            COLUMN 107,nuevo.fec_alta_emp USING 'DD/MM/YYYY',   "|",
            COLUMN 120,nuevo.fec_alta_consar USING 'DD/MM/YYYY',"|",
            COLUMN 133,nuevo.fec_baja     USING 'DD/MM/YYYY',   "|",
            COLUMN 144,nuevo.comision,                          "|",
            COLUMN 153,nuevo.subtotal_comi,                     "|",
            COLUMN 163,nuevo.vreten_isr,                        "|",
            COLUMN 173,nuevo.vreten_iva,                        "|",
            COLUMN 179,nuevo.vmto_pgo USING "#######&.&&",      "|"

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
      PRINT COLUMN 01,'\033(s7B',"Pantalla COMC007",'\033(s0B',"                                                                                                                          ",'\033(s7B',"Fecha : ",'\033(s0B',today USING "dd-mm-yyyy"
      SKIP 2 LINE
      PRINT COLUMN 80,'\033(s7B',"Reporte de Comisiones",'\033(s0B'
      SKIP 1 LINE


      PRINT COLUMN  001,'__________________________________________________',
                        '__________________________________________________',
                        '__________________________________________________',
                        '______________________________________'

      SKIP 1 LINE


      PRINT COLUMN 01,'\033(s7B',"Num promo  Nombre promotor                    Nomina  T.pro Ccencos  Fec corte  Fec al emp  Fec al Con   Fec Baja  Comision   Subtot   Reten ISR  Reten IVA  Pgo Comisi",'\033(s0B'
      PRINT COLUMN  001,'__________________________________________________',
                        '__________________________________________________',
                        '__________________________________________________',
                        '______________________________________'
            SKIP 1 LINE


      ON EVERY ROW

      PRINT COLUMN 001,nuevo.codven CLIPPED,
            COLUMN 013,nuevo.nombres CLIPPED,
            COLUMN 047,nuevo.nomina USING "######",
            COLUMN 055,nuevo.tipo_promotor    USING "##",
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
       PRINT COLUMN 001,"TOTAL DE REGISTROS: ",cont_de_registros,"                                                                      TOTALES:      ",sumcomi USING '&&&&&&&&.&&'," ",sumsub USING '&&&&&&.&&'," ",sumret_isr USING '&&&&&&.&&'," ",sumret_iva USING '&&&&&&.&&'," ",sumpago USING '&&&&&&&&.&&'

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

   LET cla_sel2="SELECT a.fecha_corte,", 
                 "a.codven,", 
                 "a.cod_tipo_prom,", 
                 "b.tipo_recibo,",
                 "trim(b.paterno)||' '||trim(b.materno)||' '||trim(b.nombres),",
                 "b.agenc_cod,",
                 "b.fingre,",
                 "b.fecha_certifi,",
                 "b.fecha_baja,",
                 "b.codven,",
                 "SUM(a.total_comision) ",
                "FROM   com_comis_resumen a,pro_mae_promotor b ",
                "WHERE  a.codven = b.cod_promotor ", 
                "AND ",cla_where CLIPPED,
                " GROUP BY 1,2,3,4,5,6,7,8,9,10 ",
     	       " ORDER BY 1,2" CLIPPED

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
