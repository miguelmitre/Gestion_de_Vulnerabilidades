################################################################################
#Proyecto          => SAFRE  Mexico
#Owner             => E.F.P.
#Programa COMM005  => MANTENEDOR DE TIPOS DE PROMOTOR
#Sistema           => COM
#By                => GERARDO ALFONSO VEGA PAREDES
#Fecha             => 26 febrero 1999
#By                => HECTOR FERNANDEZ ARCINIEGA
#Actualizacion     => 18 MARZO 1999
#By                => GERARDO ALFONSO VEGA PAREDES
#Actualizacion     => 16 enero 2001.
#By                => GERARDO ALFONSO VEGA PAREDES
#Actualizacion     => 13 agosto 2001.
#By                => GERARDO ALFONSO VEGA PAREDES
#Actualizacion     => 19 febrero 2003.
#Descripcion       => el campo prod_hasta se guarda anticipo porcentaje
################################################################################
DATABASE safre_af

GLOBALS
   DEFINE g_reg ARRAY[100] OF RECORD 
      cod_tipo_prom      LIKE com_tipo_promotor.cod_tipo_prom,
      desc_tipo          LIKE com_tipo_promotor.desc_tipo,
      red_cod            LIKE com_tipo_promotor.red_cod,
      canal_cod          LIKE com_tipo_promotor.canal_cod,
      puesto_cod         LIKE com_tipo_promotor.puesto_cod,
      indicador_comision LIKE com_tipo_promotor.indicador_comision,
      prod_desde         LIKE com_tipo_promotor.prod_desde,    --v2
      prod_hasta         LIKE com_tipo_promotor.prod_hasta     --v2
   END RECORD

   DEFINE g_reg2 ARRAY[100] OF RECORD 
      cod_tipo_prom      LIKE com_tipo_promotor.cod_tipo_prom,
      desc_tipo          LIKE com_tipo_promotor.desc_tipo,
      red_cod            LIKE com_tipo_promotor.red_cod,
      canal_cod          LIKE com_tipo_promotor.canal_cod,
      puesto_cod         LIKE com_tipo_promotor.puesto_cod,
      indicador_comision LIKE com_tipo_promotor.indicador_comision,
      prod_desde         LIKE com_tipo_promotor.prod_desde,  --v2
      prod_hasta         LIKE com_tipo_promotor.prod_hasta   --v2
   END RECORD

   DEFINE g_meta RECORD
      meta_afi1  LIKE com_tipo_promotor.meta_afi1,
      meta_afi2  LIKE com_tipo_promotor.meta_afi2,
      meta_afi3  LIKE com_tipo_promotor.meta_afi3,
      meta_tra1  LIKE com_tipo_promotor.meta_tra1,
      meta_tra2  LIKE com_tipo_promotor.meta_tra2,
      meta_tra3  LIKE com_tipo_promotor.meta_tra3,
      factualiza LIKE com_tipo_promotor.factualiza,
      usuario    LIKE com_tipo_promotor.usuario        
   END RECORD

   DEFINE g_meta2 RECORD
      meta_afi1 LIKE com_tipo_promotor.meta_afi1,
      meta_afi2 LIKE com_tipo_promotor.meta_afi2,
      meta_afi3 LIKE com_tipo_promotor.meta_afi3,
      meta_tra1 LIKE com_tipo_promotor.meta_tra1,
      meta_tra2 LIKE com_tipo_promotor.meta_tra2,
      meta_tra3 LIKE com_tipo_promotor.meta_tra3,
      usuario   LIKE com_tipo_promotor.usuario        
   END RECORD

   DEFINE g_per ARRAY[100] OF RECORD 
      cod_tipo_prom LIKE com_percepcion.cod_tipo_prom,
      percep_cod    LIKE com_percepcion.percep_cod,
      percep_desc   LIKE tab_percepcion.percep_desc,
      monto         LIKE com_percepcion.monto, 
      min_afili     LIKE com_percepcion.min_afili 
   END RECORD

   DEFINE g_id ARRAY[100] OF RECORD
      vrowid integer
   END RECORD

   DEFINE g_com_parametro RECORD LIKE com_parametro.*

   DEFINE arr_c     SMALLINT,
          scr_l     SMALLINT,
          arr_c2    SMALLINT,
          scr_l2    SMALLINT,
          arr_c3    SMALLINT,
          vren      SMALLINT, 
          vprom_ini SMALLINT,
          i         SMALLINT

   DEFINE x_red_desc      CHAR(50),
          x_canal_desc    CHAR(50),
          x_puesto_desc   CHAR(50),
          x_comision_desc CHAR(50),
          x_bono_desc     CHAR(50),
          x_ispt_desc     CHAR(50),
          x_premio_desc	  CHAR(50)

   DEFINE opc,          
          vcancela   CHAR(01),
          vhora      CHAR(08),
          cla_where  CHAR(300),
          cla_where2 CHAR(300)

   DEFINE r_ree RECORD
      vcod_tipo_prom  SMALLINT,
      nivel_desde     SMALLINT,
      nivel_hasta     SMALLINT,
      agenc_cod_desde CHAR(10),
      agenc_cod_hasta CHAR(10)
   END RECORD

   DEFINE vcont     INTEGER
   DEFINE hoy       DATE
   DEFINE usuario   CHAR(8)
   DEFINE aux_pausa CHAR(1)
   DEFINE fentcons  DATE

END GLOBALS

MAIN
   OPTIONS 
      PROMPT LINE LAST,
      INPUT  WRAP,
      ACCEPT KEY control-o

   DEFER INTERRUPT

   LET HOY = DATE

   SELECT *,
          user
   INTO   g_com_parametro.*,
          usuario
   FROM   com_parametro

   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "COMM0051" ATTRIBUTE( BORDER)
   DISPLAY " COMM005            MANTENIMIENTO A TIPOS DE PROMOTOR                          " AT 3,1 ATTRIBUTE(REVERSE) 
   DISPLAY hoy USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)
	
   MENU "TIPOS PROMOTOR"
      COMMAND "Agrega" "Agrega Tipos de Promotor"
	 CALL Inicializa()
	 CALL Agrega()
	 CALL Inicializa()
      COMMAND "Consulta" "Consulta Tipos de Promotor"
	 CALL Inicializa()
	 CALL Consulta()
	 CALL Inicializa()
      COMMAND "Modifica" "Modifica Tipos de Promotor"
	 CALL Inicializa()
	 CALL Modifica()
	 CALL Inicializa()
      COMMAND "Elimina" "Elimina Tipos de Promotor"
         CALL Inicializa()
	 CALL Elimina()
	 CALL Inicializa()
      --COMMAND "Reemplazar" "Reemplaza Tipos Promotor en Catalogo"
      --         CALL Inicializa()
      --         CALL Reemplazar()
      --         CALL Inicializa()
      COMMAND "Salir" "Salir del Programa"
	 EXIT MENU
   END MENU
   CLOSE WINDOW ventana_1
END MAIN

FUNCTION Inicializa()
   DEFINE i SMALLINT

   INITIALIZE g_reg  TO NULL
   INITIALIZE g_meta.* TO NULL
   INITIALIZE g_meta2.* TO NULL

   FOR i = 1 TO 13
      DISPLAY g_reg[i].* TO scr_1[i].*
   END FOR

   LET g_meta.meta_afi1  = 0
   LET g_meta.meta_afi2  = 0
   LET g_meta.meta_afi3  = 0
   LET g_meta.meta_tra1  = 0
   LET g_meta.meta_tra2  = 0
   LET g_meta.meta_tra3  = 0
   LET g_meta.factualiza = hoy
   LET g_meta.usuario    = usuario

   LET vcont = 0
END FUNCTION

FUNCTION Verifica_sn(VALOR)
   DEFINE VALOR CHAR(1)

   IF UPSHIFT(VALOR) NOT MATCHES "[SN]" THEN
      RETURN FALSE
   END IF

   RETURN TRUE

END FUNCTION

FUNCTION Agrega()
   DEFINE i SMALLINT

   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " AGREGA " AT 1,70 ATTRIBUTE(REVERSE,BOLD)
   DISPLAY "[Esc] Agrega  [Ctrl-p] Percepciones  [Ctrl-b] Metas  [Ctrl-c] Salir" AT 1,1 ATTRIBUTE(BOLD)

   INPUT ARRAY g_reg FROM scr_1.*
      BEFORE FIELD cod_tipo_prom
	      LET arr_c = ARR_CURR()
	      LET scr_l = SCR_LINE()

      AFTER FIELD cod_tipo_prom
	      IF g_reg[arr_c].cod_tipo_prom IS NULL THEN
	         ERROR "Tipo de Promotor NO puede ser NULO"
	         NEXT FIELD cod_tipo_prom
	      END IF

{--v2
	      FOR i = 1 TO arr_c-1
            IF g_reg[arr_c].cod_tipo_prom=g_reg[i].cod_tipo_prom THEN
	            ERROR "Tipo de Promotor YA Ingresado en el Arreglo"
	            LET g_reg[arr_c].cod_tipo_prom = NULL
	            DISPLAY g_reg[arr_c].cod_tipo_prom TO
	                    scr_1[scr_l].cod_tipo_prom
	            NEXT FIELD cod_tipo_prom
	         END IF
	      END FOR

	 SELECT "X" 
    FROM   com_tipo_promotor
	 WHERE  cod_tipo_prom = g_reg[arr_c].cod_tipo_prom
    IF STATUS <> NOTFOUND THEN
	    ERROR "Tipo de Promotor YA EXISTE"
	    LET g_reg[arr_c].cod_tipo_prom = NULL
	    DISPLAY g_reg[arr_c].cod_tipo_prom TO
	            scr_1[scr_l].cod_tipo_prom
	    NEXT FIELD cod_tipo_prom
	 END IF
}--v2

        LET vprom_ini = g_reg[1].cod_tipo_prom

      BEFORE FIELD desc_tipo
	      LET arr_c = ARR_CURR()
	      LET scr_l = SCR_LINE()

      AFTER FIELD desc_tipo
	 IF g_reg[arr_c].desc_tipo IS NULL THEN
	    ERROR "Descripcion Tipo de Promotor NO puede ser NULO"
	    NEXT FIELD desc_tipo
	 END IF

      BEFORE FIELD red_cod
	 LET arr_c = ARR_CURR()
	 LET scr_l = SCR_LINE()

      AFTER FIELD red_cod
	 IF g_reg[arr_c].red_cod IS NULL THEN
            CALL Despliega_red() 
            RETURNING g_reg[arr_c].red_cod

            DISPLAY g_reg[arr_c].red_cod TO scr_1[scr_l].red_cod
         ELSE
            SELECT red_desc
            INTO   x_red_desc
            FROM   tab_red
            WHERE  red_cod = g_reg[arr_c].red_cod
 
            IF STATUS = NOTFOUND THEN
               ERROR "No existe red"
               NEXT FIELD red_cod
            END IF

            ERROR " Red: ",x_red_desc CLIPPED
         END IF

      BEFORE FIELD canal_cod
	 LET arr_c = ARR_CURR()
	 LET scr_l = SCR_LINE()

      AFTER FIELD canal_cod
         IF g_reg[arr_c].canal_cod IS NULL THEN
            CALL Despliega_canal() 
            RETURNING g_reg[arr_c].canal_cod

            DISPLAY g_reg[arr_c].canal_cod TO scr_1[scr_l].canal_cod
         ELSE
            SELECT canal_desc
            INTO   x_canal_desc
            FROM   tab_canal
            WHERE  canal_cod = g_reg[arr_c].canal_cod

            IF STATUS = NOTFOUND THEN
               ERROR "No existe canal"
               NEXT FIELD canal_cod
            END IF

            ERROR " Canal: ",x_canal_desc CLIPPED
         END IF

      BEFORE FIELD puesto_cod
	 LET arr_c = ARR_CURR()
	 LET scr_l = SCR_LINE()

      AFTER FIELD puesto_cod
	 IF g_reg[arr_c].puesto_cod IS NULL THEN
            CALL Despliega_puesto() 
            RETURNING g_reg[arr_c].puesto_cod

            DISPLAY g_reg[arr_c].puesto_cod TO scr_1[scr_l].puesto_cod
         ELSE
            SELECT desc_puesto
            INTO   x_puesto_desc
            FROM   tab_puesto
            WHERE  cod_puesto = g_reg[arr_c].puesto_cod

            IF STATUS = NOTFOUND THEN
               ERROR "No existe puesto"
               NEXT FIELD puesto_cod
            END IF

            ERROR " Puesto: ",x_puesto_desc CLIPPED
         END IF

      BEFORE FIELD indicador_comision
	 LET arr_c = ARR_CURR()
	 LET scr_l = SCR_LINE()

      AFTER FIELD indicador_comision
         IF g_reg[arr_c].indicador_comision IS NULL THEN
            CALL Despliega_indi_comisiones() 
            RETURNING g_reg[arr_c].indicador_comision

            DISPLAY g_reg[arr_c].indicador_comision TO
                    scr_1[scr_l].indicador_comision
         ELSE
            SELECT desc_esq_comision
            INTO   x_comision_desc
            FROM   com_esq_comis
            WHERE cod_esq_comision = g_reg[arr_c].indicador_comision

            IF STATUS = NOTFOUND THEN
               ERROR "No existe esquema de comision"
               NEXT FIELD indicador_comision
            END IF

            ERROR " Comision: ",x_comision_desc CLIPPED
         END IF

      BEFORE FIELD prod_desde   --v2
	 LET arr_c = ARR_CURR()
	 LET scr_l = SCR_LINE()

      AFTER FIELD prod_desde    --v2
	 IF g_reg[arr_c].prod_desde IS NULL THEN
            ERROR "Produccion desde no puede ser nulo"
            DISPLAY g_reg[arr_c].indicador_comision TO
                    scr_1[scr_l].prod_hasta
            NEXT FIELD prod_desde
         END IF

      BEFORE FIELD prod_hasta    --v2
	      LET arr_c = ARR_CURR()
	      LET scr_l = SCR_LINE()

      AFTER FIELD prod_hasta     --v2
	      IF g_reg[arr_c].prod_hasta IS NULL THEN        --v2
            ERROR "Produccion hasta no puede ser nulo"
            NEXT FIELD prod_hasta
         END IF
         IF g_reg[arr_c].prod_hasta <= g_reg[arr_c].prod_desde THEN  --v2
            ERROR "La produccion hasta no puede ser menor a produccion desde"
            NEXT FIELD prod_hasta
         END IF

      ON KEY (INTERRUPT)
       	 FOR i = 1 TO arr_c3
    	    DELETE FROM com_percepcion 
            WHERE  cod_tipo_prom >= vprom_ini
            AND    cod_tipo_prom <= g_per[i].cod_tipo_prom
         END FOR

	 CALL Inicializa()

	 EXIT INPUT

      ON KEY ( ESC )
	 FOR i = 1 TO ARR_CURR()-1
	    IF g_reg[i].cod_tipo_prom IS NULL THEN
	       ERROR "Tipo de Promotor NO puede ser NULO"
	       NEXT FIELD cod_tipo_prom
	    END IF
{--v2
	    SELECT "X" FROM com_tipo_promotor
	    WHERE cod_tipo_prom = g_reg[i].cod_tipo_prom
	    IF STATUS <> NOTFOUND THEN
	       ERROR "Tipo de Promotor YA EXISTE"
	       NEXT FIELD cod_tipo_prom
	    END IF
}--v2
	    IF g_reg[i].desc_tipo IS NULL THEN
	       ERROR "Descripcion Tipo de Promotor NO puede ser NULO"
	       NEXT FIELD desc_tipo
	    END IF
	    IF g_reg[i].indicador_comision IS NULL THEN
	       ERROR "Indicador Comision NO puede ser NULO"
	       NEXT FIELD indicador_comision
	    END IF
	    IF g_reg[i].prod_desde IS NULL THEN   --v2
               ERROR "Produccion desde no puede ser nulo"
	       NEXT FIELD prod_desde
	    END IF
	    IF g_reg[i].prod_hasta IS NULL THEN   --v2
               ERROR "Produccion hasta no puede ser nulo"
               NEXT FIELD prod_hasta
	    END IF
            IF g_reg[arr_c].prod_hasta <= g_reg[arr_c].prod_desde THEN   --v2
               ERROR "La produccion hasta no puede ser menor a produccion desde"
               NEXT FIELD prod_hasta
            END IF

	    IF g_reg[i].red_cod IS NULL THEN
	       ERROR "Bono Fidelizacion NO puede ser NULO"
	       NEXT FIELD red_cod
	    END IF
	    IF g_reg[i].canal_cod IS NULL THEN
	       ERROR "Bono Fidelizacion NO puede ser NULO"
	       NEXT FIELD canal_cod 
	    END IF
	    IF g_reg[i].puesto_cod IS NULL THEN
	       ERROR "Bono Fidelizacion NO puede ser NULO"
	       NEXT FIELD puesto_cod
	    END IF
	 END FOR

	 FOR i = 1 TO ARR_CURR()
            IF g_reg[i].cod_tipo_prom IS NOT NULL THEN
	       INSERT INTO com_tipo_promotor
                  VALUES (g_reg[i].cod_tipo_prom,      --cod_tipo_prom
                          g_reg[i].desc_tipo,          --desc_tipo
                          g_reg[i].red_cod,            --red_cod
                          g_reg[i].canal_cod,          --canal_cod
                          g_reg[i].puesto_cod,         --puesto_cod
                          g_reg[i].indicador_comision, --indicador_comision
                          g_reg[i].prod_desde,         --prod_desde   --v2
                          g_reg[i].prod_hasta,         --prod_hasta   --v2
                          0,                           --indicador_ispt
                          g_meta.meta_afi1,            --meta_afi1
                          g_meta.meta_afi2,            --meta_afi2
                          g_meta.meta_afi3,            --meta_afi3
                          g_meta.meta_tra1,            --meta_tra1
                          g_meta.meta_tra2,            --meta_tra2
                          g_meta.meta_tra3,            --meta_tra3
                          0,                           --vigen_complemento
                          g_meta.factualiza,           --fecha_actualiza
                          g_meta.usuario)              --usuario
            END IF
	 END FOR

	 ERROR "REGISTRO(S) INGRESADO(S)" SLEEP 2 ERROR ""

	 CALL Inicializa()

	 EXIT INPUT

      ON KEY (CONTROL-P) 
         IF g_reg[arr_c].cod_tipo_prom IS NULL THEN
            ERROR "Primero capturar Tipo promotor"
         ELSE
            CALL percepciones("A")
         END IF 

      ON KEY (CONTROL-B) 
         IF g_reg[arr_c].cod_tipo_prom IS NULL THEN
            ERROR "Capturar Tipo promotor"
         ELSE
            CALL metas("A",                              --v2
                        g_reg[arr_c].cod_tipo_prom,      --v2
                        g_reg[arr_c].desc_tipo,          --v2
                        g_reg[arr_c].indicador_comision) --v2   
         END IF 
   END INPUT
END FUNCTION

FUNCTION percepciones(vcomando)
   DEFINE i        INTEGER,
          vcomando CHAR(01),
          cla_sel  CHAR(300),
          estado   CHAR(01)

   OPEN WINDOW ventana1 AT 9,2 WITH FORM "COMM0053" ATTRIBUTE(BORDER)
   DISPLAY "                           Percepciones Fijas                                  " AT 3,1 ATTRIBUTE(REVERSE) 
   DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)
   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   IF vcomando="A" THEN
      DISPLAY " [ Esc ] Agrega                [ Ctrl-c ] Salir" AT 1,1 
      DISPLAY " AGREGA " AT 1,70 ATTRIBUTE(REVERSE)
   ELSE
      DISPLAY " [ Esc ] Modifica              [ Ctrl-c  ] Salir" AT 1,1
      DISPLAY " MODIFICA " AT 1,68 ATTRIBUTE(REVERSE)
   END IF

   IF vcomando="A" THEN
      INPUT ARRAY g_per FROM scr_1.*

          BEFORE FIELD cod_tipo_prom
             LET arr_c2 = ARR_CURR()
	     LET scr_l2 = SCR_LINE()
             LET g_per[arr_c2].cod_tipo_prom = g_reg[arr_c].cod_tipo_prom 
             DISPLAY g_per[arr_c2].cod_tipo_prom TO scr_1[scr_l2].cod_tipo_prom

          BEFORE FIELD percep_cod
             LET arr_c2 = ARR_CURR()
	     LET scr_l2 = SCR_LINE()

          AFTER FIELD percep_cod
             IF g_per[arr_c2].percep_cod IS NULL THEN
                 CALL despliega_percepcion()
                 RETURNING g_per[arr_c2].percep_cod,g_per[arr_c2].percep_desc
             ELSE
                 SELECT percep_desc INTO g_per[arr_c2].percep_desc
                 FROM tab_percepcion
                 WHERE percep_cod = g_per[arr_c2].percep_cod
                 IF STATUS = 100 THEN
                     ERROR "No existe codigo de percepciones"
                     NEXT FIELD percep_cod
                 END IF
             END IF
             DISPLAY g_per[arr_c2].percep_cod,g_per[arr_c2].percep_desc 
                TO   scr_1[scr_l2].percep_cod,scr_1[scr_l2].percep_desc

          BEFORE FIELD monto
             LET arr_c2 = ARR_CURR()
	     LET scr_l2 = SCR_LINE()

          AFTER FIELD monto
             IF g_per[arr_c2].monto IS NULL THEN
                ERROR "El Monto no puede ser nulo"
                NEXT FIELD monto
             END IF

          BEFORE FIELD min_afili
             LET arr_c2 = ARR_CURR()
	     LET scr_l2 = SCR_LINE()

          AFTER FIELD min_afili
             IF g_per[arr_c2].min_afili IS NULL THEN
                ERROR "El minimo de solicitudes no puede ser nulo"
                NEXT FIELD monto
             END IF
    
          ON KEY ( ESC )
             LET arr_c2 = ARR_CURR()-1
             LET arr_c3 = ARR_CURR()
            
             FOR i = 1 TO arr_c2
	           INSERT INTO com_percepcion VALUES 
                      (
		        g_per[i].cod_tipo_prom,
                        g_per[i].percep_cod,
                        g_per[i].monto ,
		        g_per[i].min_afili)
             END FOR
             EXIT INPUT

          ON KEY ( INTERRUPT )
             EXIT INPUT

      END INPUT 

   ELSE # vcomando = "M"
    
      INPUT ARRAY g_per WITHOUT DEFAULTS FROM scr_1.*

          BEFORE FIELD cod_tipo_prom
             LET arr_c2 = ARR_CURR()
	     LET scr_l2 = SCR_LINE()
             LET g_per[arr_c2].cod_tipo_prom = g_reg[arr_c].cod_tipo_prom 
             DISPLAY g_per[arr_c2].cod_tipo_prom TO scr_1[scr_l2].cod_tipo_prom

          BEFORE FIELD percep_cod
             LET arr_c2 = ARR_CURR()
	     LET scr_l2 = SCR_LINE()
             #LET g_per[arr_c].percep_cod = null

          AFTER FIELD percep_cod
             IF g_per[arr_c2].percep_cod IS NULL THEN
                CALL despliega_percepcion()
                   RETURNING g_per[arr_c2].percep_cod,g_per[arr_c2].percep_desc
             ELSE
                SELECT percep_desc INTO g_per[arr_c2].percep_desc
                FROM tab_percepcion
                WHERE percep_cod = g_per[arr_c2].percep_cod
                IF STATUS = 100 THEN
                   ERROR "No existe codigo de percepciones"
                   NEXT FIELD percep_cod
                END IF
             END IF
             DISPLAY g_per[arr_c2].percep_cod,g_per[arr_c2].percep_desc 
                TO   scr_1[scr_l2].percep_cod,scr_1[scr_l2].percep_desc

          BEFORE FIELD monto
             LET arr_c2 = ARR_CURR()
	     LET scr_l2 = SCR_LINE()

          AFTER FIELD monto
             IF g_per[arr_c2].monto IS NULL THEN
                ERROR "El Monto no puede ser nulo"
                NEXT FIELD monto
             END IF

          BEFORE INSERT
             LET arr_c2 = ARR_CURR()
	     LET scr_l2 = SCR_LINE()

          AFTER INSERT
             LET arr_c2 = ARR_CURR()
	     LET scr_l2 = SCR_LINE()

          AFTER DELETE
             LET arr_c2 = ARR_CURR()
	     LET scr_l2 = SCR_LINE()

          ON KEY ( ESC )
             LET arr_c2 = ARR_COUNT()
             LET vcancela = "N"
             EXIT INPUT

          ON KEY ( INTERRUPT )
             LET vcancela="S"  ---C= cancelado
             EXIT INPUT

       END INPUT 
   END IF  #vcomando
    
   CLOSE WINDOW ventana1

END FUNCTION

FUNCTION metas(vcomando,cod_tipo_prom,desc_tipo,ind_com) --v2
   DEFINE
      i  	    	   INTEGER,
      vcomando		   CHAR(01),
      cod_tipo_prom	SMALLINT,
      x_tipo_prom	   SMALLINT,
      desc_tipo		CHAR(50),
      ind_com        SMALLINT

   OPEN WINDOW ventana_meta AT 9,2 WITH FORM "COMM0055" ATTRIBUTE(BORDER)
   DISPLAY "                    Metas de Venta por Tipo Promotor                           " AT 3,1 ATTRIBUTE(REVERSE) 
   DISPLAY hoy USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)
   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   IF vcomando="A" THEN
      DISPLAY " [ Esc ] Agrega                [ Ctrl-c ] Salir" AT 1,1
      DISPLAY " AGREGA " AT 1,70 ATTRIBUTE(REVERSE)
   ELSE
      DISPLAY " [Esc] Modifica     [Ctrl-v] Historico Metas     [Ctrl-c] Salir" AT 1,1
      DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE)
   END IF

   LET x_tipo_prom = cod_tipo_prom

   LET g_meta.meta_afi1  = 0
   LET g_meta.meta_afi2  = 0
   LET g_meta.meta_afi3  = 0
   LET g_meta.meta_tra1  = 0
   LET g_meta.meta_tra2  = 0
   LET g_meta.meta_tra3  = 0

   DISPLAY BY NAME cod_tipo_prom

   DISPLAY BY NAME desc_tipo

   IF vcomando="A" THEN
      INPUT BY NAME g_meta.meta_afi1,
                    g_meta.meta_afi2,
                    g_meta.meta_afi3,
                    g_meta.meta_tra1,
                    g_meta.meta_tra2,
                    g_meta.meta_tra3

          ON KEY ( ESC )
             EXIT INPUT

          ON KEY ( INTERRUPT )
             LET g_meta.meta_afi1 = 0
             LET g_meta.meta_afi2 = 0
             LET g_meta.meta_afi3 = 0
             LET g_meta.meta_tra1 = 0
             LET g_meta.meta_tra2 = 0
             LET g_meta.meta_tra3 = 0
             EXIT INPUT

      END INPUT 

   ELSE # vcomando = "M"
    
      SELECT t.meta_afi1,
             t.meta_afi2,
             t.meta_afi3,
             t.meta_tra1,
             t.meta_tra2,
             t.meta_tra3
      INTO   g_meta.meta_afi1,
             g_meta.meta_afi2,
             g_meta.meta_afi3,
             g_meta.meta_tra1,
             g_meta.meta_tra2,
             g_meta.meta_tra3
      FROM   com_tipo_promotor t
      WHERE  t.cod_tipo_prom = x_tipo_prom
      AND    t.indicador_comision = ind_com   --v2

      LET g_meta2.meta_afi1 = g_meta.meta_afi1
      LET g_meta2.meta_afi2 = g_meta.meta_afi2
      LET g_meta2.meta_afi3 = g_meta.meta_afi3
      LET g_meta2.meta_tra1 = g_meta.meta_tra1
      LET g_meta2.meta_tra2 = g_meta.meta_tra2
      LET g_meta2.meta_tra3 = g_meta.meta_tra3

      INPUT BY NAME g_meta.meta_afi1,
                    g_meta.meta_afi2,
                    g_meta.meta_afi3,
                    g_meta.meta_tra1,
                    g_meta.meta_tra2,
                    g_meta.meta_tra3 WITHOUT DEFAULTS

          BEFORE FIELD meta_afi1
             LET vhora = TIME

          ON KEY ( ESC )
             
             IF g_meta2.meta_afi1 <> g_meta.meta_afi1 OR
                g_meta2.meta_afi2 <> g_meta.meta_afi2 OR
                g_meta2.meta_afi3 <> g_meta.meta_afi3 OR
                g_meta2.meta_tra1 <> g_meta.meta_tra1 OR
                g_meta2.meta_tra2 <> g_meta.meta_tra2 OR
                g_meta2.meta_tra3 <> g_meta.meta_tra3 THEN

                INSERT INTO com_his_meta
                   VALUES (TODAY,
                           vhora,
                           x_tipo_prom,
                           g_meta2.meta_afi1,
                           g_meta2.meta_afi2,
                           g_meta2.meta_afi3,
                           g_meta2.meta_tra1,
                           g_meta2.meta_tra2,
                           g_meta2.meta_tra3,
                           g_meta.usuario,
                           ind_com)   --v2
             END IF
                           
             UPDATE com_tipo_promotor
             SET    meta_afi1 = g_meta.meta_afi1,
                    meta_afi2 = g_meta.meta_afi2,
                    meta_afi3 = g_meta.meta_afi3,
                    meta_tra1 = g_meta.meta_tra1,
                    meta_tra2 = g_meta.meta_tra2,
                    meta_tra3 = g_meta.meta_tra3,
                    usuario   = g_meta.usuario
             WHERE  com_tipo_promotor.cod_tipo_prom = x_tipo_prom
             AND    indicador_comision = ind_com   --v2
    
             EXIT INPUT

          ON KEY ( INTERRUPT )
             LET g_meta.meta_afi1 = 0
             LET g_meta.meta_afi2 = 0
             LET g_meta.meta_afi3 = 0
             LET g_meta.meta_tra1 = 0
             LET g_meta.meta_tra2 = 0
             LET g_meta.meta_tra3 = 0
             EXIT INPUT
          ON KEY (CONTROL-V)
             CALL despliega_hist_metas(x_tipo_prom,desc_tipo,ind_com) --v2
      END INPUT 

   END IF  #vcomando
    
   CLOSE WINDOW ventana_meta

END FUNCTION

FUNCTION despliega_hist_metas(tipo,desc,ind_com) --v2
   DEFINE 
      r_pro ARRAY[200] OF RECORD
         vfecha_cambio DATE,
         vhora_cambio  CHAR(08),
         vmeta_afi1    DECIMAL(12,2),
         vmeta_afi2    DECIMAL(12,2),
         vmeta_afi3    DECIMAL(12,2),
         vmeta_tra1    DECIMAL(12,2),
         vmeta_tra2    DECIMAL(12,2),
         vmeta_tra3    DECIMAL(12,2)

      END RECORD,
      i       INTEGER,
      tipo    INTEGER,
      desc    CHAR(50),
      ind_com SMALLINT   --v2

   OPEN WINDOW win1 AT 09,09 WITH FORM "COMM0057" ATTRIBUTE(BORDER)
   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " [Ctrl-C] Salir                                                          " AT 2,1 ATTRIBUTE (REVERSE)
   DISPLAY "Tipo Promotor ",tipo CLIPPED," ",desc CLIPPED AT 2,20 ATTRIBUTE(REVERSE)
   DISPLAY " HISTORICO " AT 2,56 ATTRIBUTE(REVERSE)

   DECLARE cur_metas CURSOR FOR
   SELECT fecha_cambio,
          hora_cambio,
          meta_afi1,
          meta_afi2,
          meta_afi3,
          meta_tra1,
          meta_tra2,
          meta_tra3
   FROM   com_his_meta
   WHERE  cod_tipo_prom = tipo
   AND    indicador_comision = ind_com   --v2
   ORDER  BY fecha_cambio,hora_cambio
   LET i = 1
   FOREACH cur_metas INTO r_pro[i].*
--      SELECT desc_tipo
--        INTO r_pro[i].vdesc_tipo
--        FROM com_tipo_promotor
--       WHERE cod_tipo_prom = r_pro[i].vcod_tipo_prom
      LET i = i + 1
   END FOREACH
   CALL SET_COUNT(i-1)
   DISPLAY ARRAY r_pro to scr_1.* 
      ON KEY (INTERRUPT)
         EXIT DISPLAY
   END DISPLAY
   CLOSE WINDOW win1
END FUNCTION

FUNCTION renumera_pantalla()
   DEFINE p_curr,
          p_total,
          sc_curr,
          sc_total,
          k   SMALLINT

   LET p_curr   = ARR_CURR()
   LET p_total  = ARR_COUNT()
   LET sc_curr  = SCR_LINE()
   FOR k = P_CURR TO p_total
      LET sc_curr = sc_curr + 1
   END FOR
END FUNCTION

FUNCTION despliega_impuestos()
   DEFINE aux_pausa		SMALLINT
   DEFINE cod      		DECIMAL(10,0)
	DEFINE desc		CHAR(60)
	DEFINE l_reg ARRAY[500] OF RECORD
	       codigo		SMALLINT,
	       descripcion	CHAR(50)
	END RECORD
	DEFINE pos		SMALLINT
	DEFINE x_buscar		CHAR(60)
	DEFINE x_texto		CHAR(200)

	OPEN WINDOW vent_x AT 07,12 WITH FORM "COMM0054" ATTRIBUTE(BORDER)
	DISPLAY "                    CATEGORIAS FISCALES                                        " AT 2,1 ATTRIBUTE(REVERSE)

	WHILE TRUE
	      LET x_texto = " SELECT cod_esq_ispt,desc_esq_ispt ",
			    " FROM com_esq_ispt ORDER BY 1"
	      PREPARE curper9 FROM x_texto
	      DECLARE per_9 CURSOR FOR curper9
	      LET pos = 1
	      FOREACH per_9 INTO l_reg[pos].*
		      #LET l_reg[pos].codigo = cod
		      #LET l_reg[pos].descripcion = desc
		      LET pos = pos + 1
		      IF pos >= 500 THEN
			 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
	      IF (pos-1) < 1 THEN
	         ERROR "ARCHIVO CATEGORIAS FISCALES..... VACIO"
	      END IF
	      CALL SET_COUNT(pos-1)
	      DISPLAY ARRAY l_reg TO scr_1.*
		      ON KEY ( INTERRUPT )
	--	         LET pos = 0
		         EXIT DISPLAY
		      ON KEY ( CONTROL-M )
		         LET pos = ARR_CURR()
		         EXIT DISPLAY
	      END DISPLAY
	      IF pos <> 0 THEN
	         EXIT WHILE
	      END IF
	END WHILE
	CLOSE WINDOW vent_x
	RETURN 
           l_reg[pos].codigo
END FUNCTION

FUNCTION despliega_percepcion()
   DEFINE aux_pausa		SMALLINT
   DEFINE cod      		DECIMAL(10,0)
	DEFINE desc		CHAR(60)
	DEFINE l_reg ARRAY[500] OF RECORD
	       codigo		SMALLINT,
	       descripcion	CHAR(50)
	END RECORD
	DEFINE pos		SMALLINT
	DEFINE x_buscar		CHAR(60)
	DEFINE x_texto		CHAR(200)

	OPEN WINDOW vent_x AT 07,12 WITH FORM "COMM0052" ATTRIBUTE(BORDER)
	DISPLAY "                       TIPOS DE PERCEPCION                                     " AT 2,1 ATTRIBUTE(REVERSE)

	WHILE TRUE
	      LET x_texto = " SELECT percep_cod,percep_desc ",
			    " FROM tab_percepcion  ORDER BY 1"
	      PREPARE curper1 FROM x_texto
	      DECLARE per_8 CURSOR FOR curper1
	      LET pos = 1
	      FOREACH per_8 INTO cod,desc
		      LET l_reg[pos].codigo = cod
		      LET l_reg[pos].descripcion = desc
		      LET pos = pos + 1
		      IF pos >= 500 THEN
			 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
	      IF (pos-1) < 1 THEN
	         ERROR "ARCHIVO PERCEPCIONES..... VACIO"
	      END IF
	      CALL SET_COUNT(pos-1)
	      DISPLAY ARRAY l_reg TO scr_1.*
		      ON KEY ( INTERRUPT )
	---	         LET pos = 0
		         EXIT DISPLAY
		      ON KEY ( CONTROL-M )
		         LET pos = ARR_CURR()
		         #LET l_reg[pos].codigo = cod
		         #LET l_reg[pos].descripcion = desc
		         EXIT DISPLAY
	      END DISPLAY
	      IF pos <> 0 THEN
	         EXIT WHILE
	      END IF
	END WHILE
	CLOSE WINDOW vent_x
	RETURN 
           l_reg[pos].codigo,
           l_reg[pos].descripcion
END FUNCTION

FUNCTION Despliega_indi_comisiones()
	DEFINE aux_pausa		SMALLINT
	DEFINE cod      		DECIMAL(10,0)
	DEFINE desc		CHAR(60)
	DEFINE l_reg ARRAY[500] OF RECORD
	       codigo		CHAR(10),
	       descripcion	CHAR(50)
	END RECORD
	DEFINE pos		SMALLINT
	DEFINE x_buscar		CHAR(60)
	DEFINE x_texto		CHAR(200)

	OPEN WINDOW vent_x AT 07,12 WITH FORM "COMM0052" ATTRIBUTE(BORDER)
	DISPLAY "                    ESQUEMAS DE COMISION                                      " AT 2,1 ATTRIBUTE(REVERSE)
	INPUT BY NAME x_buscar
	      AFTER FIELD x_buscar
		    IF x_buscar IS NULL THEN
		       ERROR "Descripcion a Buscar NO puede ser nulo"
		       NEXT FIELD x_buscar
		    ELSE
		       EXIT INPUT
		    END IF
	END INPUT
	WHILE TRUE
	      LET x_texto = " SELECT cod_esq_comision,desc_esq_comision ",
			    " FROM com_esq_comis WHERE desc_esq_comision MATCHES ",'"',x_buscar CLIPPED,'"' CLIPPED
	      PREPARE cur1001 FROM x_texto
	      DECLARE cur_8 CURSOR FOR cur1001
	      LET pos = 1
	      FOREACH cur_8 INTO cod,desc
		      LET l_reg[pos].codigo = cod
		      LET l_reg[pos].descripcion = desc
		      LET pos = pos + 1
		      IF pos >= 500 THEN
			 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
	      IF (pos-1) < 1 THEN
	         ERROR "ARCHIVO com_esq_comis..... VACIO"
	      END IF
	      CALL SET_COUNT(pos-1)
	      DISPLAY ARRAY l_reg TO scr_1.*
		      ON KEY ( INTERRUPT )
		         #RUN "cd /u/EXP/TAB;fglgo TABM003.4gi"
		     ---    LET pos = 0
		         EXIT DISPLAY
		      ON KEY ( CONTROL-M )
		         LET pos = ARR_CURR()
		         EXIT DISPLAY
	      END DISPLAY
	      IF pos <> 0 THEN
	         EXIT WHILE
	      END IF
	END WHILE
	CLOSE WINDOW vent_x
	RETURN l_reg[pos].codigo
END FUNCTION

FUNCTION Despliega_indi_bonos()
	DEFINE aux_pausa		SMALLINT
	DEFINE cod      		DECIMAL(10,0)
	DEFINE desc		CHAR(60)
	DEFINE l_reg ARRAY[500] OF RECORD
	       codigo		CHAR(10),
	       descripcion	CHAR(50)
	END RECORD
	DEFINE pos		SMALLINT
	DEFINE x_buscar		CHAR(60)
	DEFINE x_texto		CHAR(200)

	OPEN WINDOW vent_x AT 07,12 WITH FORM "COMM0052" ATTRIBUTE(BORDER)
	DISPLAY "                      ESQUEMAS DE BONO                                      " AT 2,1 ATTRIBUTE(REVERSE)
	INPUT BY NAME x_buscar
	      AFTER FIELD x_buscar
		    IF x_buscar IS NULL THEN
		       ERROR "Descripcion a Buscar NO puede ser nulo"
		       NEXT FIELD x_buscar
		    ELSE
		       EXIT INPUT
		    END IF
	END INPUT
	WHILE TRUE
	      LET x_texto = " SELECT cod_esq_bono,desc_esq_bono ",
			    " FROM com_esq_bono WHERE desc_esq_bono MATCHES ",'"',x_buscar CLIPPED,'"' CLIPPED
	      PREPARE cur1002 FROM x_texto
	      DECLARE cur_9 CURSOR FOR cur1002
	      LET pos = 1
	      FOREACH cur_9 INTO cod,desc
		      LET l_reg[pos].codigo = cod
		      LET l_reg[pos].descripcion = desc
		      LET pos = pos + 1
		      IF pos >= 500 THEN
			 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
			 EXIT FOREACH
		      END IF
	      END FOREACH
	      IF (pos-1) < 1 THEN
	         ERROR "ARCHIVO com_esq_bono..... VACIO"
	      END IF
	      CALL SET_COUNT(pos-1)
	      DISPLAY ARRAY l_reg TO scr_1.*
		      ON KEY ( INTERRUPT )
		         #RUN "cd /u/EXP/TAB;fglgo TABM003.4gi"
		    ---     LET pos = 0
		         EXIT DISPLAY
		      ON KEY ( CONTROL-M )
		         LET pos = ARR_CURR()
		         EXIT DISPLAY
	      END DISPLAY
	      IF pos <> 0 THEN
	         EXIT WHILE
	      END IF
	END WHILE
	CLOSE WINDOW vent_x
	RETURN l_reg[pos].codigo
END FUNCTION

FUNCTION Consulta()
   DEFINE i,pos SMALLINT

   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " CONSULTA " AT 1,66 ATTRIBUTE(REVERSE)
   DISPLAY "                                                  [Ctrl-c] Salir " AT 1,1 
   DISPLAY " [Ctrl-p] Percepciones       [Ctrl-b] Metas       [Ctrl-v] Historico T.Prom " AT 2,1 

   DECLARE cursor_1 CURSOR FOR 
   SELECT cod_tipo_prom,
          desc_tipo,
          red_cod,
          canal_cod,
          puesto_cod,
          indicador_comision,
          prod_desde,   --v2
          prod_hasta    --v2
   FROM   com_tipo_promotor
   ORDER  BY 1,7

   LET i = 1

   FOREACH cursor_1 INTO g_reg[i].*
      LET i = i + 1
   END FOREACH

   CALL SET_COUNT(i-1)

   DISPLAY ARRAY g_reg TO scr_1.*
      ON KEY ( INTERRUPT )
	 CALL Inicializa()
	 EXIT DISPLAY
      ON KEY (CONTROL-P)
         LET pos = ARR_CURR()
         CALL consulta_percep(g_reg[pos].cod_tipo_prom) 
      ON KEY (CONTROL-B)
         LET pos = ARR_CURR()
         CALL consulta_metas(g_reg[pos].cod_tipo_prom,
                             g_reg[pos].desc_tipo,
                             g_reg[pos].indicador_comision)   --v2
      ON KEY (CONTROL-V)
         LET pos = ARR_CURR()
         CALL despliega_historico(g_reg[pos].cod_tipo_prom,g_reg[pos].desc_tipo)
   END DISPLAY
END FUNCTION

FUNCTION consulta_percep(vtipo_prom)
   DEFINE j,i,vtipo_prom INTEGER 

   OPEN WINDOW ventana1 AT 9,2 WITH FORM "COMM0053" ATTRIBUTE(BORDER)
   DISPLAY "                           Percepciones Fijas                                  " AT 3,1 ATTRIBUTE(REVERSE) 
   DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)
   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " CONSULTA " AT 1,67 ATTRIBUTE(REVERSE,BOLD)
   DISPLAY "[ Ctrl-c ] Salir  " AT 1,1 ATTRIBUTE(BOLD)

   DECLARE cursor_0 CURSOR FOR 
   SELECT a.cod_tipo_prom,a.percep_cod,b.percep_desc,a.monto
   FROM   com_percepcion a,tab_percepcion b
   WHERE  a.percep_cod=b.percep_cod
   AND    a.cod_tipo_prom = vtipo_prom
   ORDER BY 1

   LET j = 1

   FOREACH cursor_0 INTO g_per[j].*
      LET j = j + 1
   END FOREACH

   CALL SET_COUNT(j-1)

   DISPLAY ARRAY g_per TO scr_1.*
      ON KEY ( INTERRUPT )
	 EXIT DISPLAY
   END DISPLAY
 
   CLOSE WINDOW ventana1 
END FUNCTION

FUNCTION consulta_metas(cod_tipo_prom,desc_tipo,ind_com) --v2
   DEFINE i  		      INTEGER,
          vcomando	   CHAR(01),
          cod_tipo_prom	SMALLINT,
          x_tipo_prom	SMALLINT,
          desc_tipo	   CHAR(50),
          ind_com       SMALLINT    --v2

   DEFINE g_met ARRAY[10] OF RECORD 
      meta_afi1 LIKE com_his_meta.meta_afi1,
      meta_afi2 LIKE com_his_meta.meta_afi2,
      meta_afi3 LIKE com_his_meta.meta_afi3,
      meta_tra1 LIKE com_his_meta.meta_tra1,
      meta_tra2 LIKE com_his_meta.meta_tra2,
      meta_tra3 LIKE com_his_meta.meta_tra3
   END RECORD

   OPEN WINDOW ventana_meta AT 9,2 WITH FORM "COMM0058" ATTRIBUTE(BORDER)
   DISPLAY "                    Metas de Venta por Tipo Promotor                           " AT 3,1 ATTRIBUTE(REVERSE) 
   DISPLAY hoy USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)
   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
      DISPLAY " [Ctrl-c] Salir                     [Ctrl-v] Historico Metas " AT 1,1
      DISPLAY " CONSULTA " AT 1,66 ATTRIBUTE(REVERSE)

   DISPLAY BY NAME cod_tipo_prom
   DISPLAY BY NAME desc_tipo

   LET x_tipo_prom = cod_tipo_prom

   SELECT t.meta_afi1,
          t.meta_afi2,
          t.meta_afi3,
          t.meta_tra1,
          t.meta_tra2,
          t.meta_tra3
   INTO   g_met[1].meta_afi1,
          g_met[1].meta_afi2,
          g_met[1].meta_afi3,
          g_met[1].meta_tra1,
          g_met[1].meta_tra2,
          g_met[1].meta_tra3
   FROM   com_tipo_promotor t
   WHERE  t.cod_tipo_prom = x_tipo_prom
   AND    indicador_comision = ind_com    --v2

   CALL SET_COUNT(1)

   DISPLAY ARRAY g_met to scr_meta.*
      ON KEY ( INTERRUPT )
         LET g_meta.meta_afi1 = 0
         LET g_meta.meta_afi2 = 0
         LET g_meta.meta_afi3 = 0
         LET g_meta.meta_tra1 = 0
         LET g_meta.meta_tra2 = 0
         LET g_meta.meta_tra3 = 0
         EXIT DISPLAY 
      ON KEY (CONTROL-V)
         CALL despliega_hist_metas(x_tipo_prom,desc_tipo,ind_com) --v2
   END DISPLAY

   CLOSE WINDOW ventana_meta
END FUNCTION

{
FUNCTION consulta_metas(cod_tipo_prom, desc_tipo)
#cm----------------------------------------------

   DEFINE j,i 		INTEGER,
          cod_tipo_prom	SMALLINT,
          desc_tipo	CHAR(50)

   SELECT t.meta_afi1,
          t.meta_afi2,
          t.meta_afi3
   INTO   g_meta.meta_afi1,
          g_meta.meta_afi2,
          g_meta.meta_afi3
   FROM   com_tipo_promotor t
   WHERE  t.cod_tipo_prom = cod_tipo_prom


   OPEN WINDOW ventana_meta AT 9,2 WITH FORM "COMM0055" ATTRIBUTE(BORDER)
   DISPLAY "                    Metas de Venta por Tipo Promotor                           " AT 3,1 ATTRIBUTE(REVERSE) 
   DISPLAY HOY USING "dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)
   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " CONSULTA " AT 1,67 ATTRIBUTE(REVERSE,BOLD)
   DISPLAY "[ Ctrl-c ] Salir     [ Ctrl-v ] Historico Metas " AT 1,1 ATTRIBUTE(BOLD)

   DISPLAY BY NAME cod_tipo_prom
   DISPLAY BY NAME desc_tipo

   DISPLAY BY NAME g_meta.meta_afi1,
                   g_meta.meta_afi2,
                   g_meta.meta_afi3

   PROMPT " [Enter] para regresar " FOR CHAR aux_pausa
   CLOSE WINDOW ventana_meta

END FUNCTION
}

FUNCTION Modifica()
   DEFINE i SMALLINT

   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " MODIFICA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)
   DISPLAY " [Esc]    Modifica                                [Ctrl-c] Salir " AT 1,1 

   DISPLAY " [Ctrl-p] Percepciones       [Ctrl-b] Metas       [Ctrl-v] Historico T.Prom " AT 2,1 

   DECLARE cursor_2 CURSOR FOR
   SELECT cod_tipo_prom,
          desc_tipo,
          red_cod,
          canal_cod,
          puesto_cod,
          indicador_comision,
          prod_desde,   --v2
          prod_hasta    --v2
   FROM   com_tipo_promotor
   ORDER  BY 1,7

   LET i = 1

   FOREACH cursor_2 INTO g_reg[i].*
      LET g_reg2[i].* = g_reg[i].*
      LET i = i + 1
   END FOREACH

   CALL SET_COUNT(i-1)

   INPUT ARRAY g_reg WITHOUT DEFAULTS FROM scr_1.*
      BEFORE FIELD cod_tipo_prom
	 LET arr_c = ARR_CURR()
	 LET scr_l = SCR_LINE()
	 NEXT FIELD desc_tipo

      BEFORE FIELD desc_tipo
         LET arr_c = ARR_CURR()
	 LET scr_l = SCR_LINE()

      AFTER FIELD desc_tipo
         IF g_reg[arr_c].desc_tipo IS NULL THEN
	    ERROR "Descripcion Tipo de Promotor NO puede ser NULO"
	    NEXT FIELD desc_tipo
	 END IF

      BEFORE FIELD red_cod
         LET arr_c = ARR_CURR()
         LET scr_l = SCR_LINE()

      AFTER FIELD red_cod
         IF g_reg[arr_c].red_cod IS NULL THEN
            CALL Despliega_red() 
            RETURNING g_reg[arr_c].red_cod
            DISPLAY g_reg[arr_c].red_cod TO
                    scr_1[scr_l].red_cod
         ELSE
            SELECT red_desc
            INTO   x_red_desc
            FROM   tab_red
            WHERE  red_cod = g_reg[arr_c].red_cod
            IF STATUS = NOTFOUND THEN
               ERROR "No existe red"
               NEXT FIELD red_cod
            END IF
            ERROR " Red: ",x_red_desc CLIPPED
         END IF

	      BEFORE FIELD canal_cod
		     LET arr_c = ARR_CURR()
		     LET scr_l = SCR_LINE()

	      AFTER FIELD canal_cod
		    IF g_reg[arr_c].canal_cod IS NULL THEN
                       CALL Despliega_canal() 
                            RETURNING g_reg[arr_c].canal_cod
                       DISPLAY g_reg[arr_c].canal_cod TO
                               scr_1[scr_l].canal_cod
                   ELSE
                       SELECT canal_desc
                       INTO   x_canal_desc
                       FROM   tab_canal
                       WHERE  canal_cod = g_reg[arr_c].canal_cod
 
                       IF STATUS = NOTFOUND THEN
                           ERROR "No existe canal"
                           NEXT FIELD canal_cod
                       END IF
                       ERROR " Canal: ",x_canal_desc CLIPPED
                   END IF

	      BEFORE FIELD puesto_cod
		     LET arr_c = ARR_CURR()
		     LET scr_l = SCR_LINE()

	      AFTER FIELD puesto_cod
		    IF g_reg[arr_c].puesto_cod IS NULL THEN
                        CALL Despliega_puesto() 
                             RETURNING g_reg[arr_c].puesto_cod
                        DISPLAY g_reg[arr_c].puesto_cod TO
                                scr_1[scr_l].puesto_cod

                    ELSE
                        SELECT desc_puesto
                        INTO   x_puesto_desc
                        FROM   tab_puesto
                        WHERE  cod_puesto = g_reg[arr_c].puesto_cod

                        IF STATUS = NOTFOUND THEN
                            ERROR "No existe puesto"
                            NEXT FIELD puesto_cod
                        END IF
                        ERROR " Puesto: ",x_puesto_desc CLIPPED
                    END IF

	      BEFORE FIELD indicador_comision
		     LET arr_c = ARR_CURR()
		     LET scr_l = SCR_LINE()

	      AFTER FIELD indicador_comision
                    SELECT "X" FROM com_esq_comis
                    WHERE COD_ESQ_COMISION = g_reg[arr_c].indicador_comision
	        IF status  = NOTFOUND THEN
                    CALL Despliega_indi_comisiones() 
                       Returning g_reg[arr_c].indicador_comision
                    DISPLAY g_reg[arr_c].indicador_comision TO
                                               scr_1[scr_l].indicador_comision
                ELSE
                   SELECT "X" FROM com_esq_comis
                   WHERE COD_ESQ_COMISION = g_reg[arr_c].indicador_comision
                   IF STATUS = NOTFOUND THEN
                      ERROR "No existe esquema de comision"
                      NEXT FIELD indicador_comision
                   END IF
                 END IF



      BEFORE FIELD prod_desde      --v2
         LET arr_c = ARR_CURR()
         LET scr_l = SCR_LINE()

      AFTER FIELD prod_desde      --v2
         IF g_reg[arr_c].prod_desde IS NULL THEN      --v2
            ERROR "Produccion desde no puede ser nulo"
            DISPLAY g_reg[arr_c].prod_hasta TO
                    scr_1[scr_l].prod_hasta
            NEXT FIELD prod_desde
         END IF
 
      BEFORE FIELD prod_hasta      --v2
         LET arr_c = ARR_CURR()
         LET scr_l = SCR_LINE()
 
      AFTER FIELD prod_hasta      --v2
         IF g_reg[arr_c].prod_hasta IS NULL THEN      --v2
            ERROR "Produccion hasta no puede ser nulo"
            NEXT FIELD prod_hasta
         END IF
         IF g_reg[arr_c].prod_hasta <= g_reg[arr_c].prod_desde THEN      --v2
            ERROR "La produccion hasta no puede ser menor a produccion desde"
            NEXT FIELD prod_hasta
         END IF

         DISPLAY g_reg[arr_c].prod_hasta      --v2
         TO   scr_1[scr_l].prod_hasta

      ON KEY ( INTERRUPT )
	      CALL Inicializa()
	      EXIT INPUT

      ON KEY ( ESC )
         FOR i = 1 TO ARR_CURR()-1
	         IF g_reg[i].desc_tipo IS NULL THEN
	            ERROR "Descripcion Tipo de Promotor NO puede ser NULO"
	            NEXT FIELD desc_tipo
	         END IF
            IF g_reg[i].indicador_comision IS NULL THEN
	            ERROR "Indicador Comision NO puede ser NULO"
	            NEXT FIELD indicador_comision
	         END IF
            IF g_reg[arr_c].prod_hasta > 100 THEN
               ERROR "El anticipo no puede ser mayor a 100"
               NEXT FIELD prod_hasta
            END IF
	         IF g_reg[i].prod_desde IS NULL THEN      --v2
	            ERROR "Indicador Bono NO puede ser NULO"
	            NEXT FIELD prod_desde
	         END IF
         END FOR

         LET vhora = TIME   

         FOR i = 1 TO ARR_CURR()
             IF g_reg2[i].desc_tipo          <> g_reg[i].desc_tipo OR
                g_reg2[i].red_cod            <> g_reg[i].red_cod OR
                g_reg2[i].canal_cod          <> g_reg[i].canal_cod OR
                g_reg2[i].puesto_cod         <> g_reg[i].puesto_cod OR
                g_reg2[i].indicador_comision <> g_reg[i].indicador_comision OR
                g_reg2[i].prod_desde     <> g_reg[i].prod_desde OR      --v2
                g_reg2[i].prod_hasta     <> g_reg[i].prod_hasta THEN    --v2
  
                INSERT INTO com_his_tipo_prom VALUES
                   (TODAY,                        --fecha_cambio
                    vhora,                        --hora_cambio
                    g_reg2[i].cod_tipo_prom,      --cod_tipo_prom
                    g_reg2[i].desc_tipo,          --desc_tipo
                    g_reg2[i].red_cod,            --red_cod
                    g_reg2[i].canal_cod,          --canal_cod
                    g_reg2[i].puesto_cod,         --puesto_cod
                    g_reg2[i].indicador_comision, --indicadro_comision
                    g_reg2[i].prod_desde,         --prod_desde  --v2
                    g_reg2[i].prod_hasta,         --prod_hasta  --v2
                    0,                            --indicador_ispt
                    0,                            --vigen_complemento
                    g_meta.usuario)               --usuario
             END IF
         END FOR

         FOR i = 1 TO ARR_CURR()
            IF g_reg[i].cod_tipo_prom IS NOT NULL THEN
		         UPDATE com_tipo_promotor 
               SET    desc_tipo          = g_reg[i].desc_tipo,
                      red_cod            = g_reg[i].red_cod,
                      canal_cod          = g_reg[i].canal_cod,
                      puesto_cod         = g_reg[i].puesto_cod,
 	                   indicador_comision = g_reg[i].indicador_comision,
 	                   prod_desde         = g_reg[i].prod_desde,  --v2
 	                   prod_hasta         = g_reg[i].prod_hasta,  --v2
                      factualiza         = g_meta.factualiza,
                      usuario            = g_meta.usuario
		         WHERE cod_tipo_prom      = g_reg[i].cod_tipo_prom 
               AND   indicador_comision = g_reg[i].indicador_comision
		      END IF
         END FOR

                 IF vcancela = "N" THEN
                    DELETE
                    FROM   com_percepcion
                    WHERE  cod_tipo_prom = g_reg[arr_c].cod_tipo_prom

                    FOR i = 1 TO arr_c2
                        IF g_per[i].percep_cod IS NOT NULL OR
                           g_per[i].monto IS NOT NULL THEN
                            INSERT INTO com_percepcion
                                 VALUES (g_per[i].cod_tipo_prom,
                                         g_per[i].percep_cod,
                                         g_per[i].monto ,
	                                 g_per[i].min_afili)
                        END IF
                    END FOR
                 END IF

		 ERROR "REGISTRO(S) MODIFICADO(S)" SLEEP 2 ERROR ""
		 CALL Inicializa()
		 EXIT INPUT

              ON KEY (CONTROL-P)
                 CALL modi_percepciones(g_reg[arr_c].cod_tipo_prom)

              ON KEY (CONTROL-B)
                 CALL metas("M",
                            g_reg[arr_c].cod_tipo_prom,
                            g_reg[arr_c].desc_tipo,
                            g_reg[arr_c].indicador_comision)   --v2
              ON KEY (CONTROL-V)
                 CALL despliega_historico(g_reg[arr_c].cod_tipo_prom,
                                          g_reg[arr_c].desc_tipo)
	END INPUT
END FUNCTION

FUNCTION despliega_historico(tipo,desc)
   DEFINE 
      r_pro ARRAY[200] OF RECORD
      vfecha_cambio      DATE,
      vhora_cambio       CHAR(08),
      desc_tipo          CHAR(50),
      red_cod            SMALLINT,
      canal_cod          SMALLINT,
      puesto_cod         SMALLINT,
      indicador_comision SMALLINT,
      prod_desde         SMALLINT,   --v2
      prod_hasta         SMALLINT    --v2
   END RECORD

   DEFINE i    INTEGER,
          tipo INTEGER,
          desc CHAR(50)

   OPEN WINDOW win1 AT 08,2 WITH FORM "COMM0056" 
   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " [Ctrl-C] Salir                                                          " AT 2,1 ATTRIBUTE (REVERSE)
   DISPLAY "Tipo Promotor ",tipo CLIPPED," ",desc CLIPPED AT 2,24 ATTRIBUTE(REVERSE)
   DISPLAY " HISTORICO " AT 2,67 ATTRIBUTE(REVERSE)

   DECLARE cur_prom CURSOR FOR
   SELECT fecha_cambio,
          hora_cambio,
          desc_tipo,
          red_cod,
          canal_cod,
          puesto_cod,
          indicador_comision,
          prod_desde,    --v2
          prod_hasta     --v2
   FROM   com_his_tipo_prom
   WHERE  cod_tipo_prom = tipo
   ORDER  BY fecha_cambio,hora_cambio

   LET i = 1
 
   FOREACH cur_prom INTO r_pro[i].*
--      SELECT desc_tipo
--        INTO r_pro[i].vdesc_tipo
--        FROM com_tipo_promotor
--       WHERE cod_tipo_prom = r_pro[i].vcod_tipo_prom
      LET i = i + 1
   END FOREACH

   CALL SET_COUNT(i-1)

   DISPLAY ARRAY r_pro to scr_1.* 
      ON KEY (INTERRUPT)
         EXIT DISPLAY
   END DISPLAY

   CLOSE WINDOW win1
END FUNCTION


FUNCTION modi_percepciones(vtipo_prom)
   DEFINE i,vtipo_prom SMALLINT

   DECLARE cursor2 CURSOR FOR 
   SELECT a.cod_tipo_prom,
          a.percep_cod,
          b.percep_desc,
          a.monto,
          a.min_afili,
          a.rowid
   FROM   com_percepcion a,tab_percepcion b
   WHERE  a.percep_cod=b.percep_cod
   AND    a.cod_tipo_prom = vtipo_prom
   ORDER BY 1

   LET i = 1
   FOREACH cursor2 INTO g_per[i].*,g_id[i].vrowid
      LET i = i + 1
   END FOREACH
   CALL SET_COUNT(i-1)
   LET vren = i
   CALL percepciones("M")
END FUNCTION

FUNCTION Elimina()
   DEFINE i SMALLINT

   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " ELIMINA " AT 1,69 ATTRIBUTE(REVERSE,BOLD)
   DISPLAY "[ Ctrl-c ] Salir" AT 1,1 ATTRIBUTE(BOLD)
   DISPLAY "[ Ctrl-B ] Elimina" AT 1,23 ATTRIBUTE(BOLD)

   DECLARE cursor_3 CURSOR FOR 
   SELECT cod_tipo_prom,
          desc_tipo,
          red_cod,
          canal_cod,
          puesto_cod,
          indicador_comision,
          prod_desde,          --v2
          prod_hasta           --v2
   FROM   com_tipo_promotor
   ORDER  BY 1,7

   LET i = 1

   FOREACH cursor_3 INTO g_reg[i].*
      LET i = i + 1
   END FOREACH

   CALL SET_COUNT(i-1)

   DISPLAY ARRAY g_reg TO scr_1.*
      ON KEY ( CONTROL-B )
	      LET i = ARR_CURR()

	      IF Esta_seguro() THEN
	         DELETE
            FROM  com_tipo_promotor
	         WHERE cod_tipo_prom      = g_reg[i].cod_tipo_prom
            AND   indicador_comision = g_reg[i].indicador_comision

            DELETE
            FROM   com_percepcion
            WHERE  cod_tipo_prom = g_reg[i].cod_tipo_prom

	         ERROR "REGISTRO ELIMINADO" SLEEP 2 ERROR ""
	      ELSE
	         ERROR "ELIMINAR CANCELADO" SLEEP 2 ERROR ""
	      END IF

	      CALL Inicializa()

         EXIT DISPLAY

      ON KEY ( INTERRUPT )

	      CALL Inicializa()

	      EXIT DISPLAY

   END DISPLAY

END FUNCTION

FUNCTION Esta_seguro()
   PROMPT "Desea Eliminar Registro S/N " FOR CHAR aux_pausa

   IF aux_pausa MATCHES "[sS]" THEN
      RETURN TRUE
   END IF

   RETURN FALSE
END FUNCTION

FUNCTION despliega_red()
   DEFINE aux_val		SMALLINT
   DEFINE l_reg ARRAY[100] OF RECORD
          codigo	INTEGER,
          descripcion	CHAR(50)
   END RECORD,
   x_x CHAR(100)

   DEFINE pos		SMALLINT

   OPEN WINDOW vent_1 AT 05,12 WITH FORM "COMM00510" ATTRIBUTE(BORDER)
   DISPLAY "                   R   E   D                             " AT 2,1 ATTRIBUTE(REVERSE)
    LET x_x = " SELECT red_cod,red_desc FROM tab_red ",
              " ORDER BY 1 " CLIPPED
      PREPARE curred FROM x_x
      DECLARE cur_red CURSOR FOR curred
      LET pos = 1
      FOREACH cur_red INTO l_reg[pos].*
	      LET pos = pos + 1
	      IF pos >= 100 THEN
		 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
		 EXIT FOREACH
	      END IF
      END FOREACH
      IF (pos-1) < 1 THEN
         ERROR "ARCHIVO... VACIO"
      END IF
      CALL SET_COUNT(pos-1)
      DISPLAY ARRAY l_reg TO scr_1.*
              ON KEY ( INTERRUPT )
	     --    LET pos = 0
	         EXIT DISPLAY
	      ON KEY ( CONTROL-M )
	         LET pos = ARR_CURR()
	         EXIT DISPLAY
      END DISPLAY
    CLOSE WINDOW vent_1
    RETURN l_reg[pos].codigo
END FUNCTION

FUNCTION despliega_canal()
   DEFINE aux_val		SMALLINT
   DEFINE l_reg ARRAY[100] OF RECORD
          codigo	INTEGER,
          descripcion	CHAR(50)
   END RECORD,
   x_x CHAR(100)

   DEFINE pos		SMALLINT

   OPEN WINDOW vent_2 AT 05,12 WITH FORM "COMM00510" ATTRIBUTE(BORDER)
   DISPLAY "                  C  A  N  A  L                          " AT 2,1 ATTRIBUTE(REVERSE)
    LET x_x = " SELECT canal_cod,canal_desc FROM tab_canal ",
              " ORDER BY 1 " CLIPPED
      PREPARE curcanal FROM x_x
      DECLARE cur_canal CURSOR FOR curcanal
      LET pos = 1
      FOREACH cur_canal INTO l_reg[pos].*
	      LET pos = pos + 1
	      IF pos >= 100 THEN
		 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
		 EXIT FOREACH
	      END IF
      END FOREACH
      IF (pos-1) < 1 THEN
         ERROR "ARCHIVO... VACIO"
      END IF
      CALL SET_COUNT(pos-1)
      DISPLAY ARRAY l_reg TO scr_1.*
              ON KEY ( INTERRUPT )
	         --LET pos = 0
	         EXIT DISPLAY
	      ON KEY ( CONTROL-M )
	         LET pos = ARR_CURR()
	         EXIT DISPLAY
      END DISPLAY
    CLOSE WINDOW vent_2
    RETURN l_reg[pos].codigo
END FUNCTION

FUNCTION despliega_puesto()
   DEFINE aux_val		SMALLINT
   DEFINE l_reg ARRAY[100] OF RECORD
          codigo	INTEGER,
          descripcion	CHAR(50)
   END RECORD,
   x_x CHAR(100)

   DEFINE pos		SMALLINT

   OPEN WINDOW vent_1 AT 05,12 WITH FORM "COMM00510" ATTRIBUTE(BORDER)
   DISPLAY "                P  U  E  S  T  O  S                         " AT 2,1 ATTRIBUTE(REVERSE)
    LET x_x = " SELECT cod_puesto,desc_puesto FROM tab_puesto ",
              " ORDER BY 1 " CLIPPED
      PREPARE curpuesto FROM x_x
      DECLARE cur_puesto CURSOR FOR curpuesto
      LET pos = 1
      FOREACH cur_puesto INTO l_reg[pos].*
	      LET pos = pos + 1
	      IF pos >= 100 THEN
		 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
		 EXIT FOREACH
	      END IF
      END FOREACH
      IF (pos-1) < 1 THEN
         ERROR "ARCHIVO... VACIO"
      END IF
      CALL SET_COUNT(pos-1)
      DISPLAY ARRAY l_reg TO scr_1.*
              ON KEY ( INTERRUPT )
--	         LET pos = 0
	         EXIT DISPLAY
	      ON KEY ( CONTROL-M )
	         LET pos = ARR_CURR()
	         EXIT DISPLAY
      END DISPLAY
    CLOSE WINDOW vent_1
    RETURN l_reg[pos].codigo
END FUNCTION

FUNCTION Reemplazar()

   OPEN WINDOW wree AT 7,2 WITH FORM "COMM0059" ATTRIBUTE(BORDER)
   DISPLAY " [ESC] Procesar                                                [Ctrl-c] Salir " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY "           Reemplaza Tipos de Promotor en Catalogo y/o Afiliaciones           " AT 3,1 ATTRIBUTE(REVERSE) 

   LET INT_FLAG = FALSE
   INPUT BY NAME r_ree.*
      AFTER FIELD vcod_tipo_prom
        IF r_ree.vcod_tipo_prom IS NULL THEN
           ERROR "Este codigo no puede ser NULO"
           NEXT FIELD vcod_tipo_prom
        END IF

      AFTER FIELD nivel_desde
         IF r_ree.nivel_desde IS NULL THEN
            ERROR "Este campo no puede ser NULO"
            NEXT FIELD nivel_desde
         END IF
      AFTER FIELD nivel_hasta
         IF r_ree.nivel_desde IS NULL THEN
            ERROR "Este campo no puede ser NULO"
            NEXT FIELD nivel_hasta
         END IF
      AFTER FIELD agenc_cod_desde
         IF r_ree.nivel_desde IS NULL THEN
            ERROR "Este campo no puede ser NULO"
            NEXT FIELD agenc_cod_desde
         END IF
      AFTER FIELD agenc_cod_hasta
         IF r_ree.nivel_desde IS NULL THEN
            ERROR "Este campo no puede ser NULO"
            NEXT FIELD agenc_cod_hasta
         END IF

        EXIT INPUT
      ON KEY (INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT

   IF INT_FLAG = TRUE THEN
      ERROR " Proceso Cancelado "
      LET INT_FLAG = FALSE
      CLOSE WINDOW wree
      RETURN
   END IF

   CONSTRUCT cla_where2 ON fentcons FROM fentcons
      AFTER FIELD fentcons
         EXIT CONSTRUCT
      ON KEY (INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT CONSTRUCT
   END CONSTRUCT

   IF INT_FLAG = TRUE THEN
      ERROR " Proceso Cancelado "
      LET INT_FLAG = FALSE
      CLOSE WINDOW wree
      RETURN
   END IF

   PROMPT "Genera proceso [S/N]... " FOR opc
   IF opc MATCHES '[sS]' THEN
      ERROR "Cambiando Informacion ..."
      CALL ejecuta_reemplazo()
      ERROR ""
   END IF

   CLOSE WINDOW wree
END FUNCTION

FUNCTION ejecuta_reemplazo()
   DEFINE cla_qry  CHAR(500),
          vcodven  DECIMAL(10,0),
          vcod_esq_comision SMALLINT

   LET cla_where2 = cla_where2 CLIPPED

   LET cla_qry = "UPDATE pro_mae_promotor ",
                    "SET nivel = ",r_ree.vcod_tipo_prom, 
                 " WHERE agenc_cod BETWEEN ","'",r_ree.agenc_cod_desde,"'",
                 "   AND ","'",r_ree.agenc_cod_hasta,"'",
                 "   AND nivel BETWEEN ",r_ree.nivel_desde,
                 "   AND ",r_ree.nivel_hasta

--display cla_qry
--exit program

   LET cla_qry = cla_qry CLIPPED

   PREPARE exe_mapro FROM cla_qry
   EXECUTE exe_mapro

   IF cla_where2 <> " 1=1 " THEN

      LET cla_qry = "SELECT a.codven,b.indicador_comision ",
                    " FROM pro_mae_promotor a,com_tipo_promotor b ",
                    " WHERE a.nivel = b.cod_tipo_prom ",
                    "   AND a.nivel = ",r_ree.vcod_tipo_prom,
                    "   AND a.agenc_cod BETWEEN ","'",r_ree.agenc_cod_desde,"'",
                    "   AND ","'",r_ree.agenc_cod_hasta,"'"

      PREPARE exe_codven FROM cla_qry
      DECLARE cur_codven CURSOR FOR exe_codven

      FOREACH cur_codven INTO vcodven,vcod_esq_comision
         LET cla_qry = "UPDATE afi_mae_afiliado",
                       "   SET cod_esq_comision = ",vcod_esq_comision,
                       " WHERE codven = ",vcodven,
                       "   AND indicador_comision = 0 ",
                       "   AND ",cla_where2 CLIPPED
         PREPARE exe_upmae FROM cla_qry 
         EXECUTE exe_upmae
      END FOREACH
    
   END IF

END FUNCTION
{
      LET cla_qry = "UPDATE afi_mae_afiliado ",
                       "SET cod_esq_comision=",
                           "(SELECT indicador_comision ",
                              "FROM com_tipo_promotor b,pro_mae_promotor a ",
                            " WHERE a.nivel = b.cod_tipo_prom ",
                              " AND ",cla_where CLIPPED,")",
                    " WHERE ",cla_where2 CLIPPED,
                      " AND codven = ",
                           "(SELECT a.codven ",
                              "FROM com_tipo_promotor b,pro_mae_promotor a ",
                             "WHERE a.nivel = b.cod_tipo_prom ",
                              " AND ",cla_where CLIPPED,")"

      LET cla_qry = cla_qry CLIPPED
display cla_qry
prompt '' for opc
}
