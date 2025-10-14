################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO                 		       #
#Owner             => E.F.P.        		                                     #
#Programa          => COMC021                                                  #
#Descripcion       => GENERACION DEL REPORTE DE PAGO DE COMISIONES A SUBDIREC. #
#                  => ADELANTO DEL 80% CONSOLIDADO                             #
#Fecha             => 02 Feb  2005.        	                                  #
#By                => JOSE ALEJANDRO RAMIREZ.        	                         #
#Sistema           => COM. 					                                     #
--------------------------------------------------------------------------------
#Datos a considerar=> comision = comis_pagada                                  #
#                  => adelanto = monto_comision                                #
#                  => salario_base_comis  c/cliente                            #
#Modify.           => Alejandro Ramirez 26 Abril 2005                          #
#Descr.            => (v5) Se cambio codven por num_interno                    #
#Descr.            => (v6) Se cambio num_interno por codven                    #
--------------------------------------------------------------------------------
#Modify.           => Alejandro Ramirez 5 May 2005                             #
#Descr.            => (v8) Piden se quite la columna del esquema de comision   #
#                  => y se sumen los valores.                                  #
#                  => -Tambien se solicito no mostrar los reg con mnto 0       #
################################################################################
DATABASE safre_af
GLOBALS
   DEFINE 

      enter           char(01),
      g_param_dis     RECORD LIKE seg_modulo.* ,
      w_codigo_afore  LIKE tab_afore_local.codigo_afore,
      g_usuario       CHAR (08),


      g_reg0 RECORD
	     fecha_corte  DATE,
        coduni_n1    CHAR(10)
      END RECORD,

      g_reg ARRAY[1000] OF RECORD 
        cod_resp_uni       LIKE com_dat_uni_com.cod_resp_uni, 
        coduni_n1          LIKE com_comis_detalle.coduni_n1,
	     vnom               CHAR(55)
      END RECORD, 

      g_reg3 ARRAY[1500] OF RECORD
        codven      	   LIKE com_comis_detalle.codven, --v6
        salario_base_comis LIKE com_comis_detalle.salario_base_comis,
	     num_sm             DECIMAL(10,2),
	     comis_pagada       LIKE com_comis_detalle.comis_pagada,
	     monto_comision     LIKE com_comis_detalle.monto_comision,
        cod_esq_comision   LIKE com_comis_detalle.cod_esq_comision
      END RECORD, 

      g_reg5 ARRAY[6] OF RECORD	
        coduni_n1      LIKE com_nivel1.coduni_n1,
        nombre         CHAR(50)
      END RECORD,

      nombre CHAR(50),

      vmenu     CHAR(01),
      aux_pausa	CHAR(1),
      HOY	DATE,
      SW_1      SMALLINT,
      cla_sel 	CHAR(1500), 
      cla_sel2	CHAR(800), 
      cla_sell	CHAR(800), 
      vaccion   smallint,
      cla_where CHAR(800), 
      vcomando  SMALLINT,
      opc 	CHAR(01),
      total     DECIMAL(12,2),
      pagada    DECIMAL(12,2),
      registros INTEGER,
      longitud  integer,

    l_record_1  ARRAY[700] OF RECORD
      n_folio     	    LIKE afi_solicitud.n_folio,
      n_seguro           LIKE afi_solicitud.n_seguro,
      tipo_solicitud     LIKE afi_solicitud.tipo_solicitud,
      frecafor  	    LIKE afi_solicitud.fentcons,
      vfecha_corte       LIKE com_comis_detalle.fecha_corte,
      salario_base_comis LIKE afi_solicitud.salario_base_comis,
      num_sm             LIKE com_comis_detalle.num_sm,
      monto_comision     LIKE com_comis_detalle.monto_comision
    END RECORD,
    
    vregis             INTEGER,
    cont_de_registros  INTEGER,
    sumtot_afil        INTEGER,
    sumtot_sm          DECIMAL(8,2),
    sumtot_promsm      DECIMAL(6,2),
    sumtot_comi        DECIMAL(12,2),
    vnom_jefe          CHAR(50)

    DEFINE fcodven           CHAR(10)      
    DEFINE xcoduni_n1        CHAR(10)      
    DEFINE xcoduni_n2        CHAR(10)      
    DEFINE xfecha_corte      CHAR(8)      
    DEFINE xfecha_corte2     DATE
        
    DEFINE g_grep_00         CHAR(1000)
    DEFINE g_grep_03         CHAR(1000)
    DEFINE columnas          CHAR(200)
    DEFINE comando           CHAR(200)
    DEFINE ejecuta           CHAR(200)
    DEFINE tott_num_sm        LIKE com_comis_detalle.num_sm
    DEFINE ban_solo_admon    SMALLINT

    DEFINE arr_nivsuper ARRAY[10] OF RECORD
     cve_respon    CHAR(10),
     cencos        CHAR(10),
     cod_puesto    INTEGER,
     esq_comision  INTEGER,
     res_uni       CHAR(10),   --v7
     codven        CHAR(10),   --v7
     nom_respon    CHAR(60),
     nom_unidad    CHAR(40),
     cencos_sup    CHAR(10)
    END RECORD


END GLOBALS

MAIN
	OPTIONS PROMPT LINE LAST,
        INPUT WRAP,
	ACCEPT KEY control-o
	
	DEFER INTERRUPT

        CALL STARTLOG("COMC021.log")

        SELECT  ruta_rescate
        INTO    g_param_dis.ruta_rescate
        FROM    seg_modulo
        WHERE   modulo_cod='com'

        SELECT  codigo_afore,USER
        INTO    w_codigo_afore,g_usuario
        FROM    tab_afore_local

	LET HOY = TODAY
        LET vregis = 0

  	OPEN WINDOW ventana_1 AT 2,2 WITH FORM "COMC0211" ATTRIBUTE( BORDER)
  	DISPLAY " COMC021  PAGO DE COMISIONES A SUBDIRECTORES ADELANTO DEL 80% CONSOL.                " AT 3,1 ATTRIBUTE(REVERSE) 

	DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)

	DISPLAY "                                                                               " AT 7,1 ATTRIBUTE(REVERSE)

	DISPLAY "                                                                               " AT 09,1 ATTRIBUTE(REVERSE)

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



        for i=1 to 9
	   DISPLAY g_reg[i].* TO scr_1[i].*
        end for

	LET total = 0
	LET registros = 0
        DISPLAY registros TO scr_3.*

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
      pos SMALLINT,
      xcoduni_n1 LIKE com_comis_detalle.coduni_n1,
      vvalor    CHAR(01) 

   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " [ESC] Procesar " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " [Ctrl-C] Salir " AT 1,63 ATTRIBUTE(REVERSE)



      LET INT_FLAG = FALSE
      CONSTRUCT cla_where 
	 ON    a.fecha_corte,
               a.coduni_n1
         FROM  com_comis_detalle.fecha_corte, 
               com_comis_detalle.coduni_n1

         ON KEY (ESC)

         LET xfecha_corte  = get_fldbuf(com_comis_detalle.fecha_corte)
         LET xcoduni_n1    = get_fldbuf(com_comis_detalle.coduni_n1)


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
    
       IF xfecha_corte IS NULL OR xfecha_corte= ' ' THEN
          ERROR "Es preciso ingresar la fecha de corte"
          SLEEP 3
          LET INT_FLAG = FALSE
          ERROR ""
          LET vcomando = 0
          RETURN
       END IF

       LET xfecha_corte2 = xfecha_corte[3,4]||'/'||xfecha_corte[1,2]||'/'||
                           xfecha_corte[5,8]

       IF xcoduni_n1 IS NULL THEN
          ERROR "Es preciso ingresar codigo de unidad"
          SLEEP 3
          LET INT_FLAG = FALSE
          ERROR ""
          LET vcomando = 0
          RETURN
       END IF

       LET ban_solo_admon = 0
      
       IF xcoduni_n1='1234567' THEN 
          LET cla_where = cla_where[1,26]
          LET ban_solo_admon = 1
       END IF 

         LET cla_sel ="SELECT 4,",
                      "coduni_n2,'",xfecha_corte2,
                      "', coduni_n2 ",
                      "FROM com_nivel2 ",
                      "WHERE coduni_n2='",xcoduni_n1 CLIPPED,
                      "' ORDER BY 2 "

         PREPARE claexe2 FROM cla_sel
         DECLARE cursor_2 SCROLL CURSOR FOR claexe2
         OPEN cursor_2

         CALL primer_row()
   DISPLAY " [Ctrl-I]Imprime  [Ctrl-P]Gerentes de la Subdireccion" AT 7,01 ATTRIBUTE(REVERSE)
	 CALL ver_arreglo(xcoduni_n1)

END FUNCTION


FUNCTION primer_row()

   FETCH FIRST cursor_2 INTO g_reg0.fecha_corte  --,g_reg2.* ojo
   IF STATUS=100 THEN
      ERROR "No hay registros en esta direccion "
   ELSE
      LET g_reg0.fecha_corte=xfecha_corte2
      LET g_reg0.coduni_n1=xcoduni_n1
      
      DISPLAY BY NAME g_reg0.*
   END IF

END FUNCTION



FUNCTION imprime(ban_agreg,ycoduni_n1)

   DEFINE G_IMPRE    CHAR(2000)
   DEFINE G_LISTA    CHAR(300)
   DEFINE impresion  CHAR(300)
   DEFINE tcodven    LIKE com_comis_detalle.codven --v6
   DEFINE ycoduni_n1 LIKE com_comis_detalle.coduni_n1
   DEFINE tcoduni_n1 LIKE com_comis_detalle.coduni_n1
   DEFINE ban_agreg  SMALLINT
   DEFINE vcod_esq_comision LIKE com_esq_comis.cod_esq_comision  --v8

   DEFINE nuevo RECORD 
      coduni_n1          LIKE com_comis_detalle.coduni_n1,
      salario_base_comis LIKE com_comis_detalle.salario_base_comis,
      num_sm             DECIMAL(10,2),
      monto_comision     LIKE com_comis_detalle.monto_comision,
      comis_pagada       LIKE com_comis_detalle.comis_pagada,
 --   cod_esq_comision   LIKE com_comis_detalle.cod_esq_comision,  --v8
      niv_sup            LIKE com_nivel1.uni_superior_n1
   END RECORD,

   mandar RECORD
      coduni_n1          LIKE com_comis_detalle.coduni_n1,
      salario_base_comis LIKE com_comis_detalle.salario_base_comis,
      num_sm             DECIMAL(10,2),
      monto_comision     LIKE com_comis_detalle.monto_comision,
      comis_pagada       LIKE com_comis_detalle.comis_pagada,
 --   cod_esq_comision   LIKE com_comis_detalle.cod_esq_comision,  --v8
      niv_sup            LIKE com_nivel1.uni_superior_n1
   END RECORD,

      xpaterno           CHAR(40),
      xmaterno           CHAR(40),
      xnombres           CHAR(40)

   DEFINE val_porcenx    DECIMAL(12,2)

    IF ban_agreg = 1 THEN

    ELSE



      WHENEVER ERROR CONTINUE
      DROP TABLE temp_listo

      LET tott_num_sm = 0

      CREATE TEMP TABLE temp_listo
      (
        codven char(10), 
        nom_pro char(50),
        cencos char(10),
        folio_clie integer,
        nss_clie char(11),
        nom_clie char(50),
        fec_certif date,
        sala_base_comis decimal(12,2),
        num_sm decimal(6,2),
        monto_comis decimal(12,2),
        comis_pagada decimal(12,2),
        edo_comis smallint,
        descr_edo CHAR(40),
        descr_causa CHAR(70)
      )

      WHENEVER ERROR STOP
  
      LET G_IMPRE = g_param_dis.ruta_rescate CLIPPED,"/",g_usuario CLIPPED,
                                ".R_COMC021_",HOY USING "DDMMYYYY"

      LET G_LISTA = g_param_dis.ruta_rescate CLIPPED,"/",g_usuario CLIPPED,
                                ".A_COMC021_",HOY USING "DDMMYYYY"

      -- PARA IMPRESION
      START REPORT rpt_cuenta_imp TO G_IMPRE

      WHENEVER ERROR CONTINUE
      DROP TABLE tmp_union
      WHENEVER ERROR STOP

      IF ban_solo_admon = 1 THEN  ---------------PARA TODOS LOS COD DE UNIDAD

         LET cla_sel="SELECT a.codven,a.coduni_n1,", --v6
                     "a.salario_base_comis,",
                     "a.num_sm,",
                     "a.comis_pagada,",
                     "a.monto_comision,",
                     "a.cod_esq_comision,",
                     "b.uni_superior_n1 ",
                     "FROM   com_comis_detalle a,", --quitar
                     "       com_nivel1 b ",
                     "WHERE a.coduni_n1 IN (SELECT coduni_n1 ",
                     "       FROM   com_nivel1) ",
                     " AND a.fecha_corte = '",xfecha_corte2 CLIPPED,
                     "' AND a.nivel = 1 ",
                     "  AND a.coduni_n1=b.coduni_n1 ",
                     "  AND a.num_sm < 26 ",
                     "  AND a.num_sm > 3 ",
                     "UNION ",-------------------- NO SE SUMAN LOS < 3 num_sm
                     "SELECT a.codven,a.coduni_n1,", --v6
                     "0 as salario_base_comis,",
                     "0 as num_sm,",
                     "a.comis_pagada,",
                     "a.monto_comision,",
                     "a.cod_esq_comision,",
                     "b.uni_superior_n1 ",
                     "FROM   com_comis_detalle a,", --quitar
                     "       com_nivel1 b ",
                     "WHERE a.coduni_n1 IN (SELECT coduni_n1 ",
                     "       FROM   com_nivel1) ",
                     " AND a.fecha_corte = '",xfecha_corte2 CLIPPED,
                     "' AND a.nivel = 1 ",
                     "  AND a.coduni_n1=b.coduni_n1 ",
                     "  AND a.num_sm < 3 ",
                     "UNION ",--------------------- SE TOPAN LOS MAYORES DE 25
                     "SELECT a.codven,a.coduni_n1,", --v6
                     "1170 as salario_base_comis,",
                     "25 as num_sm,",
                     "a.comis_pagada,",
                     "a.monto_comision,",
                     "a.cod_esq_comision,",
                     "b.uni_superior_n1 ",
                     "FROM   com_comis_detalle a,", --quitar
                     "       com_nivel1 b ",
                     "WHERE a.coduni_n1 IN (SELECT coduni_n1 ",
                     "       FROM   com_nivel1) ",
                     " AND a.fecha_corte = '",xfecha_corte2 CLIPPED,
                     "' AND a.nivel = 1 ",
                     "  AND a.coduni_n1=b.coduni_n1 ",
                     "  AND a.num_sm > 25 ",
                     "INTO TEMP tmp_union "

         ERROR "Preparando la informacion para imprimir..."
         PREPARE claexe3 FROM cla_sel
         EXECUTE claexe3


      ELSE ------------------------- PARA UN COD UNIDAD EN ESPECIFICO 

         LET cla_sel="SELECT a.coduni_n1,", --v5
                     "a.salario_base_comis,",
                     "a.num_sm,",
                     "a.comis_pagada,",
                     "a.monto_comision,",
                     "a.cod_esq_comision,",
                     "b.uni_superior_n1 ",
                     "FROM   com_comis_detalle a,", --quitar
                     "       com_nivel1 b ",
                     "WHERE  a.coduni_n1 IN (SELECT coduni_n1 ",      
                     "       FROM   com_nivel1 ",
                     "       WHERE  uni_superior_n1 = '",ycoduni_n1 CLIPPED,
                     "') AND a.fecha_corte = '",xfecha_corte2 CLIPPED,
                     "' AND a.nivel = 1 ",
                     "  AND a.coduni_n1=b.coduni_n1 ",
                     "  AND a.num_sm < 26 ",
                     "  AND a.num_sm > 3 ",
                     "UNION ",-------------------- NO SE SUMAN LOS < 3 num_sm
                     "SELECT a.coduni_n1,", --v5
                     "0 as salario_base_comis,",
                     "0 as num_sm,",
                     "a.comis_pagada,",
                     "a.monto_comision,",
                     "a.cod_esq_comision,",
                     "b.uni_superior_n1 ",
                     "FROM   com_comis_detalle a,", --quitar
                     "       com_nivel1 b ",
                     "WHERE  a.coduni_n1 IN (SELECT coduni_n1 ",
                     "       FROM   com_nivel1 ",
                     "       WHERE  uni_superior_n1 = '",ycoduni_n1 CLIPPED,
                     "') AND a.fecha_corte = '",xfecha_corte2 CLIPPED,
                     "' AND a.nivel = 1 ",
                     "  AND a.coduni_n1=b.coduni_n1 ",
                     "  AND a.num_sm < 3 ",
                     "UNION ",--------------------- SE TOPAN LOS MAYORES DE 25
                     "SELECT a.coduni_n1,", --v5
                     "1170 as salario_base_comis,",
                     "25 as num_sm,",
                     "a.comis_pagada,",
                     "a.monto_comision,",
                     "a.cod_esq_comision,",
                     "b.uni_superior_n1 ",
                     "FROM   com_comis_detalle a,", --quitar
                     "       com_nivel1 b ",
                     "WHERE  a.coduni_n1 IN (SELECT coduni_n1 ",
                     "       FROM   com_nivel1 ",
                     "       WHERE  uni_superior_n1 = '",ycoduni_n1 CLIPPED,
                     "') AND a.fecha_corte = '",xfecha_corte2 CLIPPED,
                     "' AND a.nivel = 1 ",
                     "  AND a.coduni_n1=b.coduni_n1 ",
                     "  AND a.num_sm > 25 ",
                     "INTO TEMP tmp_union "

         ERROR "Preparando la informacion para imprimir..."
         PREPARE claexe4 FROM cla_sel
         EXECUTE claexe4
 

      END IF

     DECLARE cursor_12 CURSOR FOR
     SELECT  coduni_n1,    --v5
             SUM(salario_base_comis),
             SUM(num_sm),
             SUM(comis_pagada),
             SUM(monto_comision),
          -- cod_esq_comision,  --v8
             uni_superior_n1
     FROM    tmp_union
     WHERE   monto_comision <>0   --v8
  -- GROUP BY 1,6,7  --v8
     GROUP BY 1,6
     ORDER BY 1,2

     FOREACH cursor_12 INTO  nuevo.coduni_n1,
                             nuevo.salario_base_comis,
                             nuevo.num_sm,
                             nuevo.comis_pagada,
                             nuevo.monto_comision,
                          -- nuevo.cod_esq_comision,  --v8
                             nuevo.niv_sup


       INITIALIZE mandar.* TO NULL

         LET mandar.coduni_n1           = nuevo.coduni_n1
         LET mandar.salario_base_comis  = nuevo.salario_base_comis
         LET mandar.num_sm              = nuevo.num_sm
         LET mandar.comis_pagada        = nuevo.comis_pagada
         LET mandar.monto_comision      = nuevo.monto_comision
--       LET mandar.cod_esq_comision    = nuevo.cod_esq_comision   --v8
         LET mandar.niv_sup             = nuevo.niv_sup           


         --Obtengo el porcentaje por promotor        --v8
         SELECT cod_esq_comision                     --v8
         INTO   vcod_esq_comision                    --v8
         FROM   com_esq_comis                        --v8
         WHERE  desc_esq_comision = 'GERENTES'       --v8
         
         IF vcod_esq_comision IS NULL THEN     --v8
            LET vcod_esq_comision = 0          --v8
         END IF                                      --v8
         CALL Porcentaje(vcod_esq_comision,mandar.num_sm)   --v8
                         RETURNING val_porcenx

         --Llamo a la funcion que trae los datos del nivel superior
         CALL  Obtine_niv_super(mandar.coduni_n1) 
               RETURNING arr_nivsuper[1].cencos_sup,
                         arr_nivsuper[1].res_uni,   --v7
                         arr_nivsuper[1].codven,   --v7
                         arr_nivsuper[1].nom_respon,
                         arr_nivsuper[1].nom_unidad,
                         arr_nivsuper[1].esq_comision,
                         arr_nivsuper[2].cencos_sup,
                         arr_nivsuper[2].res_uni,   --v7
                         arr_nivsuper[2].codven,   --v8
                         arr_nivsuper[2].nom_respon,
                         arr_nivsuper[2].nom_unidad

   LET vregis = vregis + 1

   -- PARA LA IMPRESION
 
   OUTPUT TO REPORT rpt_cuenta_imp(mandar.*,val_porcenx,
                                   arr_nivsuper[1].cencos_sup,
                                   arr_nivsuper[1].res_uni,   --v7
                                   arr_nivsuper[1].codven,    --v7
                                   arr_nivsuper[1].nom_respon,
                                   arr_nivsuper[1].nom_unidad,
                                   arr_nivsuper[1].esq_comision,
                                   arr_nivsuper[2].cencos_sup,
                                   arr_nivsuper[2].codven,    --v8
                                   arr_nivsuper[2].nom_respon,
                                   arr_nivsuper[2].nom_unidad)

   END FOREACH

   FINISH REPORT rpt_cuenta_imp
   ERROR "LISTADO 1 GENERADO...."
   SLEEP 2
   ERROR ""

{
   unload to G_LISTA
   select * from temp_listo order by 1,11 desc

   LET columnas = "COD PROMOTOR'|'NOM PROMOTOR'|'CENTRO COSTOS'|'FOLIO CLIEN'|'NSS CLIE'|',",
                  "NOM CLIEN'|'FEC CERTIF'|'SALAR BASE COMI'|'SAMS'|'MONTO COMIS'|'ESTADO COMIS'|'DESCRIP EDO'|'DESCRIP CAUSA"

   LET comando ="cd ",g_param_dis.ruta_rescate CLIPPED,"; echo ",columnas CLIPPED,"> alguno2" CLIPPED
   RUN comando

   LET comando ="cd ",g_param_dis.ruta_rescate CLIPPED,"; cat ",G_LISTA CLIPPED,">> alguno2" CLIPPED,";mv alguno2 ",G_LISTA CLIPPED
   RUN comando
} 

  LET impresion = "lp ",G_IMPRE
  RUN impresion


  END IF



END FUNCTION


REPORT rpt_cuenta_imp(lnuevo,val_porcen, niv_super,
                                         cod_ger,     --v7
                                         codven_ger,  --v7
                                         nom_ger,
                                         unidad_ger,
                                         esq_comi_ger,
                                         niv_super2,
                                         int_sub2,   --v8
                                         nom_sub,
                                         unidad_sub)

  DEFINE
    lnuevo RECORD
      coduni_n1          LIKE com_comis_detalle.coduni_n1,
      salario_base_comis LIKE com_comis_detalle.salario_base_comis,
      num_sm             DECIMAL(10,2),
      comis_pagada       LIKE com_comis_detalle.comis_pagada,
      monto_comision     LIKE com_comis_detalle.monto_comision,
   -- cod_esq_comision   LIKE com_comis_detalle.cod_esq_comision,  --v8
      niv_sup            LIKE com_nivel1.uni_superior_n1
   END RECORD

   DEFINE niv_super              CHAR(10)
   DEFINE cod_ger                CHAR(10)   --v7
   DEFINE codven_ger             CHAR(10)   --v7
   DEFINE nom_ger                CHAR(50)
   DEFINE unidad_ger             CHAR(50)
   DEFINE puesto_ger             SMALLINT
   DEFINE esq_comi_ger           SMALLINT
   
   DEFINE niv_super2             CHAR(10)
   DEFINE int_sub2               CHAR(10)
   DEFINE nom_sub                CHAR(50)
   DEFINE unidad_sub             CHAR(50)
   DEFINE puesto_sub             SMALLINT

   DEFINE vdescrip_edo           CHAR(40)
   DEFINE vcausa_edo             CHAR(100)

   DEFINE val_porcen             DECIMAL(12,2)
   DEFINE val_porcen_ger         DECIMAL(12,2)

      --enter                      char(01)
     
   DEFINE subt_num_sm        DECIMAL(12,2)
   DEFINE sams_ger           DECIMAL(12,2)
   DEFINE desc_esquema           CHAR(30) 
 OUTPUT
      TOP MARGIN 0
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   90
      ORDER BY niv_super,lnuevo.niv_sup,
               lnuevo.coduni_n1    --,lnuevo.cod_esq_comision desc   v8
  FORMAT
     PAGE HEADER

      PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
      PRINT COLUMN 02,'\033(s7B',"COMC021                                                                                                                                     ",today USING "dd-mm-yyyy",'\033(s0B'
      SKIP 2 LINE
      PRINT COLUMN 80,'\033(s7B',"REPORTE DE PAGO DE COMISIONES A SUBDIREC.",'\033(s0B'
      PRINT COLUMN 80,'\033(s7B',"ADELANTO DEL 80% DETALLE CONSOLIDADO",'\033(s0B'
      SKIP 1 LINE
 

    BEFORE GROUP OF lnuevo.niv_sup

      PRINT COLUMN  001,'__________________________________________________',
                        '__________________________________________________',
                        '__________________________________________________',
                        '_______________________'
       SKIP 1 LINE 
       PRINT COLUMN 002,'NOMBRE DEL SUBDIRECTOR '
       PRINT COLUMN 002,nom_sub
       PRINT COLUMN 002,'Subdireccion: ',unidad_sub
    -- PRINT COLUMN 002,'Grp venta:',niv_super2
       PRINT COLUMN 002,'Grp venta:',niv_super
       PRINT COLUMN 002,'No. INT.:',int_sub2   --v8  AQUI
    
      PRINT COLUMN  001,'__________________________________________________',
                        '__________________________________________________',
                        '__________________________________________________',
                        '_______________________'
       SKIP 2 LINE

      --  AFTER GROUP OF lnuevo.niv_sup    totales por gerencia

      PRINT COLUMN 01,'\033(s7B',"COD GERENTE   NOMBRE GERENTE                         GRP VENTA    FECHA CORTE      MASA SALARIAL    SAMS            COMISION         ADELANTO   ",'\033(s0B'


      BEFORE GROUP OF lnuevo.coduni_n1

      LET subt_num_sm = 0  
      LET sams_ger = 0  


    ON EVERY ROW
 
     {  --v8
      --Obtengo la descrip del esquema del ejecutivo
      LET desc_esquema = ''
      SELECT desc_esq_comision
      INTO   desc_esquema
      FROM   com_esq_comis
      WHERE  cod_esq_comision = lnuevo.cod_esq_comision

      IF desc_esquema IS NULL THEN
         LET desc_esquema = 'ESQUEMA CERO'
      END IF
     } --v8
 

      PRINT COLUMN 002,cod_ger,                     --v7
            COLUMN 013,nom_ger CLIPPED,
            COLUMN 057,lnuevo.coduni_n1 CLIPPED,
            COLUMN 067,xfecha_corte2 USING "DD-MM-YYYY",
            COLUMN 080,lnuevo.salario_base_comis   USING "#########&&.&&",
            COLUMN 098,lnuevo.num_sm               USING "####&.&&",
        --  COLUMN 111,desc_esquema,     --v8
            COLUMN 111,lnuevo.comis_pagada         USING "#########&&.&&",
            column 127,lnuevo.monto_comision       USING "#########&&.&&"
            


--    INSERT INTO temp_listo
--    VALUES(lnuevo.codven,lnuevo.nombre_pro,lnuevo.coduni_n1,lnuevo.n_folio,lnuevo.nss,lnuevo.nombre_cli,lnuevo.tipo_solicitud,lnuevo.salario_base_comis,lnuevo.num_sm,val_porcen,lnuevo.monto_comision)


      AFTER GROUP OF lnuevo.niv_sup

      LET sams_ger = sams_ger + GROUP SUM(lnuevo.num_sm)


      --Obtengo el porcen del gerente
      CALL Porcentaje(esq_comi_ger,sams_ger) RETURNING val_porcen_ger

      --Obtengo la descrip del esquema del gerente
      LET desc_esquema = ''
      SELECT desc_esq_comision 
      INTO   desc_esquema
      FROM   com_esq_comis
      WHERE  cod_esq_comision = esq_comi_ger

      IF desc_esquema IS NULL THEN
         LET desc_esquema = 'ESQUEMA CERO'
      END IF 

      SKIP  1 LINES
      PRINT COLUMN 080,'_________________________________TOTALES POR SUBDIRECCION____________________________________'
      PRINT COLUMN 080,GROUP SUM(lnuevo.salario_base_comis) USING "#########&&.&&","    ",GROUP SUM(lnuevo.num_sm) USING "####&.&&","     ",GROUP SUM(lnuevo.comis_pagada) * val_porcen_ger USING "#########&&.&&","  ",GROUP SUM(lnuevo.monto_comision) USING "#########&&.&&"," ",GROUP SUM(lnuevo.comis_pagada) USING "#########&&.&&"
      PRINT COLUMN 080,'    SUBT_SALA.   SUBT_SAMS           SUBT_COMIS.  SUBT_ANTICI.  COMISI_EJECU.'
      PRINT COLUMN 080,'    ESQUEMA:',desc_esquema
      SKIP  2 LINES



      SKIP TO TOP OF PAGE                          --tapado
      ---------------SKIP  2 LINES   OJO


      PAGE TRAILER                                        
      SKIP 2 LINE                                         
      PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"   


      ON LAST ROW

      PRINT COLUMN 080,'_______________________________________GRAN TOTAL :__________________________________________'
      PRINT COLUMN 080,'_____________________________________________________________________________________________'
      PRINT COLUMN 080,SUM(lnuevo.salario_base_comis) USING "#########&&.&&","    ",SUM(lnuevo.num_sm) USING "####&.&&","     ",SUM(lnuevo.comis_pagada) * val_porcen_ger USING "#########&&.&&","  ",SUM(lnuevo.monto_comision) USING "#########&&.&&"," ",SUM(lnuevo.comis_pagada) USING "#########&&.&&"
      PRINT COLUMN 080,'    TOTA_SALA.   TOTA_SAMS           TOTA_COMIS.   TOTA_ANTICI.  COMISI_EJECU.'
      PRINT COLUMN 080,'_____________________________________________________________________________________________'
      PRINT COLUMN 080,'_____________________________________________________________________________________________'

      SKIP  3 LINES

END REPORT





#-----------------------------------------------------------------------------
# Obtiene el query principal                       
#-----------------------------------------------------------------------------

FUNCTION ver_arreglo(ycoduni_n1)
   DEFINE
      pat    CHAR(40),
      mat    char(40),
      nom    char(40),
      pos    SMALLINT,
      fhasta DATE,
      i      SMALLINT,
      ycoduni_n1 CHAR(10),
      ban_agregados SMALLINT 

       LET total = 0
       LET pagada = 0
       LET registros = 0
       LET ban_agregados = 0

 
       IF ban_solo_admon = 1 THEN  --todo 
          LET cla_sel2=" SELECT dat.cod_resp_uni,  ",
                       " niv.coduni_n2,  ",
                       " res.nombre_resp_uni  ",
                       " FROM   com_nivel2        niv,  ",
                       "        com_dat_uni_com   dat,  ",
                       "        com_respon_unidad res   ",
                       " WHERE  dat.cod_uni      = niv.coduni_n2  ",
                       " AND    dat.nivel        = 2  ",
                       " AND    res.cod_resp_uni = dat.cod_resp_uni  ",
                       " GROUP BY 1,2,3 ", 
                       " ORDER BY 1,2 "
       ELSE

          LET cla_sel2=" SELECT dat.cod_resp_uni,  ",
                       " niv.coduni_n2,  ",
                       " res.nombre_resp_uni  ",
                       " FROM   com_nivel2        niv,  ",
                       "        com_dat_uni_com   dat,  ",
                       "        com_respon_unidad res   ",
                       " WHERE  niv.coduni_n2 = '",ycoduni_n1, 
                       "' AND    dat.cod_uni      = niv.coduni_n2  ",
                       " AND    dat.nivel        = 2  ",
                       " AND    res.cod_resp_uni = dat.cod_resp_uni  ",
                       " GROUP BY 1,2,3 ", 
                       " ORDER BY 1,2 "

       END IF
 

       ERROR "Buscando Informacion"

       PREPARE claexe FROM cla_sel2
       DECLARE cursor_1111 CURSOR FOR claexe
       LET pos = 1
       FOREACH cursor_1111 INTO g_reg[pos].*


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
                  LET i = ARR_CURR()
                  CALL imprime(ban_agregados,g_reg[i].coduni_n1)

               ON KEY (CONTROL-P)
                 LET i = ARR_CURR()
	         CALL Busca_gerentes(g_reg[i].coduni_n1)

               ON KEY (INTERRUPT)
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




#-----------------------------------------------------------------------------
# Obtiene los afiliados                            
#-----------------------------------------------------------------------------

FUNCTION Busca_gerentes(vcoduni_n1)

   DEFINE registros2 INTEGER
   DEFINE reg1       INTEGER
   DEFINE reg2       INTEGER,
          monto1     LIKE com_comis_detalle.monto_comision,
          monto2     LIKE com_comis_detalle.monto_comision,
          mto_comi   LIKE com_comis_detalle.monto_comision,
	       pos        SMALLINT,
     	    parte1     CHAR(26),
     	    parte2     CHAR(10),
	       parte3     CHAR(100),
          i          SMALLINT,
          vcodven    CHAR(10),
          vcoduni_n1 LIKE com_comis_detalle.coduni_n1,
          vcod_tipo  LIKE com_comis_detalle.cod_tipo_prom, 
          vfec_corte LIKE com_comis_detalle.fecha_corte,
          vestado    LIKE com_comis_detalle.estado_comision
   DEFINE calidad1   LIKE com_comis_detalle.num_sm
   DEFINE calidad2   LIKE com_comis_detalle.num_sm   



   OPEN WINDOW ventana_2 AT 9,2 WITH FORM "COMC0212" 

   DISPLAY "                         GERENTES DE LA SUBDIRECCION                          " AT 2,1 ATTRIBUTE(REVERSE) 


   DISPLAY "             [Ctrl-C] Salir                          [Enter] Nombre Gerente    " AT 04,1 ATTRIBUTE(REVERSE)

   DISPLAY "                                                                               " AT 10,1 ATTRIBUTE(REVERSE)


   LET registros2 = 0
   LET reg1 = 0
   LET reg2 = 0
   LET monto1      = 0
   LET monto2      = 0
   LET mto_comi    = 0

      LET cla_sel2="SELECT coduni_n1,SUM(salario_base_comis), ",
   		          "SUM(num_sm), ",
   		          "SUM(comis_pagada), ",
   		          "SUM(monto_comision), ",
                   "cod_esq_comision ",
                   "FROM   com_comis_detalle ",  --quitar
                   "WHERE  coduni_n1 IN (SELECT coduni_n1 ",
                   "       FROM   com_nivel1 ",
                   "       WHERE  uni_superior_n1 = '",vcoduni_n1 CLIPPED,
                   "') AND fecha_corte = '",xfecha_corte2 CLIPPED,
     	             "' AND nivel = 1 ",
                   "  AND monto_comision <> 0 ",    --v8
                   "  GROUP BY 1,6 ",
                   " ORDER BY coduni_n1,cod_esq_comision " CLIPPED

         ERROR "Buscando Informacion"

         PREPARE claexe_pend FROM cla_sel2
         DECLARE cursor_pend2 CURSOR FOR claexe_pend

	 LET pos = 1
	 FOREACH cursor_pend2 INTO g_reg3[pos].*
		 LET registros2 = registros2 + 1

	 LET monto1 = monto1 + g_reg3[pos].num_sm
	 LET reg1 = reg1 + 1

         --Para el monto de la comision
         LET mto_comi = mto_comi + g_reg3[pos].comis_pagada
 
         LET pos = pos + 1
         IF pos >= 9000 THEN
             ERROR "Sobrepaso la capacidad del arreglo"
             EXIT FOREACH
         END IF 
    END FOREACH

    DISPLAY registros2 TO scr_1.registros2
    DISPLAY mto_comi TO scr_1.mto_comi
    

	 ERROR ""

	 CALL  SET_COUNT(pos-1)

	 IF (pos-1) >= 1 THEN
	    DISPLAY ARRAY g_reg3 TO scr_0.*
               ON KEY (CONTROL-M)
                  LET i = ARR_CURR()
                  CALL Mostrar_nombre2(g_reg3[i].codven) --v6
               ON KEY (INTERRUPT)
                  EXIT DISPLAY
	    END DISPLAY
	 ELSE
	    ERROR "NO EXISTEN DATOS CON ESTAS CONDICIONES"
            SLEEP 3
            error ""
	 END IF
	 CLOSE WINDOW ventana_2
END FUNCTION




#-----------------------------------------------------------------------------
# Muestra el nombre de los gerentes                 
#-----------------------------------------------------------------------------

FUNCTION Mostrar_nombre2(vcoduni_n1)
   DEFINE vcoduni_n1 CHAR(10),
          pos     INTEGER

   OPEN WINDOW ventana001 AT 09,2 WITH FORM "COMC0214" 

   DISPLAY "                             NOMBRE DEL GERENTE                                " AT 2,1 ATTRIBUTE(REVERSE) 

   DISPLAY "                                                              [Ctrl-C] Salir   " AT 04,1 ATTRIBUTE(REVERSE)

   DISPLAY "                                                                               " AT 11,1 ATTRIBUTE(REVERSE)


   LET cla_sel2="SELECT niv.coduni_n1,res.nombre_resp_uni   ",
                "FROM   com_nivel1        niv,              ",
                "       com_dat_uni_com   dat,              ",
                "       com_respon_unidad res               ",
                "WHERE  niv.coduni_n1    = '",vcoduni_n1 CLIPPED,
                "' AND  dat.cod_uni      = niv.coduni_n1    ",
                "AND    dat.nivel        = 1                ",
                "AND    res.cod_resp_uni = dat.cod_resp_uni "

   ERROR "Buscando Informacion"

   PREPARE claexe_cte2 FROM cla_sel2
   DECLARE cursor_cte2 CURSOR FOR claexe_cte2
	LET pos = 1
	FOREACH cursor_cte2 INTO g_reg5[pos].*
	         LET pos = pos + 1
                 IF pos >= 900 THEN
                    ERROR "Sobrepaso la capacidad del arreglo"
                    EXIT FOREACH
                 END IF 
         END FOREACH

	 ERROR ""

	 CALL  SET_COUNT(pos-1)

	 IF (pos-1) >= 1 THEN
	    DISPLAY ARRAY g_reg5 TO scr_0.*
               ON KEY (INTERRUPT)
                  EXIT DISPLAY
	    END DISPLAY
	 ELSE
	    ERROR "NO EXISTEN DATOS CON ESTAS CONDICIONES"
            SLEEP 3
            error ""
	 END IF
	 CLOSE WINDOW ventana001
END FUNCTION




#-----------------------------------------------------------------------------
# Obtiene nivel superior
#-----------------------------------------------------------------------------

FUNCTION Obtine_niv_super(cencos)

 ----GENERA LOS NIVELES SUPERIORES
 DEFINE cencos CHAR(10)
 DEFINE valor ARRAY[10] OF RECORD
     cve_respon    CHAR(10),
     cencos        CHAR(10),
     cod_puesto    INTEGER,
     esq_comision  INTEGER,
     res_uni       CHAR(10),   --v7
     codven        CHAR(10),   --v7
     nom_respon    CHAR(60),
     nom_unidad    CHAR(40),
     cencos_sup    CHAR(10)
 END RECORD

 DEFINE opc CHAR(01)

 --JEFE NIVEL 1
 SELECT dat.cod_resp_uni,
        niv.coduni_n1,
        tab.cod_puesto,
        tab.cod_esq_comision,
        res.cod_resp_uni,  --v7
        res.codven,        --v7
        res.nombre_resp_uni,
        niv.nombre_uni_n1,
        niv.uni_superior_n1
 INTO 
        valor[1].cve_respon,
        valor[1].cencos,
        valor[1].cod_puesto,
        valor[1].esq_comision,
        valor[1].res_uni,  --v7
        valor[1].codven,  --v7
        valor[1].nom_respon,
        valor[1].nom_unidad,
        valor[1].cencos_sup
 FROM   com_nivel1        niv,
        com_dat_uni_com   dat,
        com_respon_unidad res,
        tab_puesto        tab
 WHERE  niv.coduni_n1    = cencos
 AND    dat.cod_uni      = niv.coduni_n1
 AND    dat.nivel        = 1
 AND    res.cod_resp_uni = dat.cod_resp_uni
 AND    res.puesto_resp  = tab.cod_puesto


 SELECT dat.cod_resp_uni,
        niv.coduni_n2,
        tab.cod_puesto,
        tab.cod_esq_comision,
        res.cod_resp_uni,  --v7
        res.codven,        --v8
        res.nombre_resp_uni,
        niv.nombre_uni_n2,
        niv.uni_superior_n2
 INTO 
        valor[2].cve_respon,
        valor[2].cencos,
        valor[2].cod_puesto,
        valor[2].esq_comision,
        valor[2].res_uni,  --v7
        valor[2].codven,  --v7
        valor[2].nom_respon,
        valor[2].nom_unidad,
        valor[2].cencos_sup
 FROM   com_nivel2        niv,
        com_dat_uni_com   dat,
        com_respon_unidad res,
        tab_puesto        tab
 WHERE  niv.coduni_n2    = valor[1].cencos_sup
 AND    dat.cod_uni      = niv.coduni_n2
 AND    dat.nivel        = 2
 AND    res.cod_resp_uni = dat.cod_resp_uni
 AND    res.puesto_resp  = tab.cod_puesto
 
 SELECT dat.cod_resp_uni,
        niv.coduni_n3,
        tab.cod_puesto,
        tab.cod_esq_comision,
        res.cod_resp_uni,  --v7
        res.codven,        --v7
        res.nombre_resp_uni,
        niv.nombre_uni_n3,
        niv.uni_superior_n3
 INTO 
        valor[3].cve_respon,
        valor[3].cencos,
        valor[3].cod_puesto,
        valor[3].esq_comision,
        valor[3].res_uni,  --v7
        valor[3].codven,  --v7
        valor[3].nom_respon,
        valor[3].nom_unidad,
        valor[3].cencos_sup
 FROM   com_nivel3        niv,
        com_dat_uni_com   dat,
        com_respon_unidad res,
        tab_puesto        tab
 WHERE  niv.coduni_n3    = valor[2].cencos_sup
 AND    dat.cod_uni      = niv.coduni_n3
 AND    dat.nivel        = 3
 AND    res.cod_resp_uni = dat.cod_resp_uni
 AND    res.puesto_resp  = tab.cod_puesto
 
 SELECT dat.cod_resp_uni,
        niv.coduni_n4,
        tab.cod_puesto,
        tab.cod_esq_comision,
        res.cod_resp_uni,  --v7
        res.codven,        --v7
        res.nombre_resp_uni,
        niv.nombre_uni_n4,
        niv.uni_superior_n4
 INTO 
        valor[4].cve_respon,
        valor[4].cencos,
        valor[4].cod_puesto,
        valor[4].esq_comision,
        valor[4].res_uni,  --v7
        valor[4].codven,  --v7
        valor[4].nom_respon,
        valor[4].nom_unidad,
        valor[4].cencos_sup
 FROM   com_nivel4        niv,
        com_dat_uni_com   dat,
        com_respon_unidad res,
        tab_puesto        tab
 WHERE  niv.coduni_n4    = valor[3].cencos_sup
 AND    dat.cod_uni      = niv.coduni_n4
 AND    dat.nivel        = 4
 AND    res.cod_resp_uni = dat.cod_resp_uni
 AND    res.puesto_resp  = tab.cod_puesto
 

 SELECT dat.cod_resp_uni,
        niv.coduni_n5,
        tab.cod_puesto,
        tab.cod_esq_comision,
        res.cod_resp_uni,  --v7
        res.codven,        --v7
        res.nombre_resp_uni,
        niv.nombre_uni_n5,
        niv.uni_superior_n5
 INTO 
        valor[5].cve_respon,
        valor[5].cencos,
        valor[5].cod_puesto,
        valor[5].esq_comision,
        valor[5].res_uni,  --v7
        valor[5].codven,  --v7
        valor[5].nom_respon,
        valor[5].nom_unidad,
        valor[5].cencos_sup
 FROM   com_nivel5        niv,
        com_dat_uni_com   dat,
        com_respon_unidad res,
        tab_puesto        tab
 WHERE  niv.coduni_n5    = valor[4].cencos_sup
 AND    dat.cod_uni      = niv.coduni_n5
 AND    dat.nivel        = 5
 AND    res.cod_resp_uni = dat.cod_resp_uni
 AND    res.puesto_resp  = tab.cod_puesto
 
 RETURN valor[1].cencos_sup,
        valor[1].res_uni,  --v7
        valor[1].codven,  --v7
        valor[1].nom_respon,
        valor[1].nom_unidad,
        valor[1].esq_comision,
        valor[2].cencos_sup,
        valor[2].res_uni,  --v7
        valor[2].codven,   --v8
        valor[2].nom_respon,
        valor[2].nom_unidad
        
END FUNCTION

#---------------------------------------------------------------
FUNCTION Porcentaje(vcod_esq_comision,vnum_sm)

  DEFINE vcod_esq_comision   LIKE com_cuadro_comis.cod_esq_comision
  DEFINE vnum_sm             LIKE com_cuadro_comis.monto_comision
  DEFINE vporcentaje         LIKE com_cuadro_comis.monto_comision

   LET vporcentaje = 0

   SELECT monto_comision
   INTO   vporcentaje
   FROM   com_cuadro_comis
   WHERE  cod_esq_comision = vcod_esq_comision
   AND    rango_desde     <= vnum_sm
   AND    rango_hasta     >= vnum_sm

   LET vporcentaje  = vporcentaje / 100 

   RETURN vporcentaje
END FUNCTION

