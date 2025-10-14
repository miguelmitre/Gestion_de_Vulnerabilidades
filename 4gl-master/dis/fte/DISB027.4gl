################################################################################
#Proyecto          => safre_af                                                 #
#Owner             => E.F.P                                                    #
#Programa DISB027  => LIQUIDACION INTERESES RCV EN TRANSITO                    #
#Created           => 11 DE MAYO de 1998                                       #
#By                => GERARDO ALFONSO VEGA PAREDES.                            #
#Sistema           => DIS                                                      #
#Modify            => 28 de Abril 2004 por Alejandro Ramirez                   #
#Descr.            => El programa de lanzamiento DISB052 ya solo ejecuta       #
#                  => la provision y la liquidacion de EST y ocupa una tabla   #
#                  => gemela temp tmp_pla_rcv   para la carga del arch..       #
-------------------------------------------------------------------------------
#Fecha modifi      => 21 Nov     2005                                          #
#Autor             => ALEJANDRO RAMIREZ                                        #
#Descripcion       => (v3) Se manda llamar el proceso que rehabilita ctasxint  #
################################################################################
DATABASE safre_af
GLOBALS

     DEFINE g_reg	RECORD
          	fecha_aplica	DATE,
            	fecha_accion	DATE,
		precio_accion	DECIMAL(16,6),
            	folios      	CHAR(50),
            	fecha_dispersa	DATE,
            	ctas_proc	INTEGER,
            	num_nss  	INTEGER,
		total_interes	DECIMAL(20,6)
            END RECORD

     DEFINE
         hist_fecha_aplica	DATE,
         x_precio_accion 	FLOAT,  #DECIMAL(16,6),
 	 max_folio_int		INTEGER,
         i, cont, mes, ano      INTEGER,
         dias_del_mes		INTEGER,
         HOY			DATE,
         opc			CHAR(1),
         usuario		CHAR(8),
         sw			SMALLINT,
	 contador               INTEGER,
	 tipo_reg               CHAR(02),
	 folio_bus              INTEGER,
	 fec                    CHAR(08),
	 cons                   CHAR(03),
	 tot_int_char           CHAR(11),
	 tot_nss_char           CHAR(11),
         resul_dis              CHAR(50),
	 hora_inicial           CHAR(08),
         hora_final             CHAR(08),
	 cla_sel                CHAR(1000),
	 vrow                   INTEGER,
	 cla_upd                CHAR(1000)
END GLOBALS


MAIN
	OPTIONS PROMPT LINE last,
        INPUT WRAP,
        ACCEPT KEY CONTROL-I,
	COMMENT LINE LAST

        LET g_reg.fecha_aplica  = ARG_VAL(1)
	LET g_reg.fecha_accion  = ARG_VAL(2)
	LET g_reg.precio_accion = ARG_VAL(3)
	LET x_precio_accion     = g_reg.precio_accion

        DISPLAY 'FECHA DE APLICACION :',g_reg.fecha_aplica
	DISPLAY 'FECHA APLICA  :',g_reg.fecha_aplica
	DISPLAY 'FECHA ACCION  :',g_reg.fecha_accion
	DISPLAY 'PRECIO ACCION :',g_reg.precio_accion

        SELECT USER
        INTO   usuario
        FROM   glo_parametro

        LET HOY=TODAY

        CALL dispersa_interes()
END MAIN


FUNCTION dispersa_interes ()
   DEFINE 
       l_reg   RECORD LIKE dis_cuenta.* ,

       l_cta   RECORD
                    nss		CHAR(11),
		    subcuenta	SMALLINT,
		    folio	INTEGER
               END RECORD,

        l_mov  RECORD LIKE dis_cuenta.*

    DEFINE 
        interes			FLOAT,  #DECIMAL(16,6),
        x_aporte_pesos		FLOAT,  #DECIMAL(16,6),
        x_tasa_mes		FLOAT,  #DECIMAL(16,6),
	x_fecha_calculo		DATE,
	x_fecha_final    	DATE,
	x_fecha_limite		DATE,
	x_fecha_tasa  		DATE,
	x_fecha_dias  		DATE,
	cont			INTEGER,
	cont_etapa              INTEGER

    DEFINE
        ejecuta                 CHAR(200)   --C22-6.6

    LET g_reg.fecha_dispersa = g_reg.fecha_aplica
    LET g_reg.ctas_proc      = 0
    LET g_reg.total_interes  = 0
    LET contador             = 0
    LET cont_etapa           = 1
    LET folio_bus            = 0
     
   DECLARE cursor_tmp CURSOR FOR

   SELECT n_registros[1,2],
          n_registros[24,31],
          n_registros[21,23]
   FROM   safre_tmp:tmp_pla_rcv
   WHERE  n_registros[93,93] = 3
   AND    n_registros[18] = '5'

   FOREACH cursor_tmp INTO tipo_reg,fec,cons

      LET contador = contador + 1

      SELECT a.folio
      INTO   folio_bus
      FROM   dis_cza_aporte a
      WHERE  a.fech_creac_lote = fec
      AND    a.lote_del_dia    = cons

      IF STATUS = NOTFOUND THEN           --c22-6.4
         LET folio_bus = 0                --c22-6.4
      END IF                              --c22-6.4

      IF folio_bus <> 0 THEN                  --c22-6.4

         LET g_reg.total_interes  = 0
         LET g_reg.num_nss        = 0

         LET cont_etapa = cont_etapa + 1

         DISPLAY 'FOLIO :', folio_bus
     
         CALL Ingresa_etapa(folio_bus,cont_etapa,'PROCESANDO')

         DECLARE cur_mov CURSOR FOR
         SELECT *
         FROM   cta_interes_rcv
         WHERE  folio           = folio_bus 
         AND    subcuenta       IN (5,6,9)
         AND    tipo_movimiento = 3

         FOREACH cur_mov INTO l_reg.*

            SELECT precio_del_dia                        --c22-6.12
            INTO   x_precio_accion                       --c22-6.12
            FROM   glo_valor_accion                      --c22-6.12
            WHERE  fecha_valuacion = g_reg.fecha_accion  --c22-6.12
            AND    codigo_siefore  = l_reg.siefore       --c22-6.12

            LET interes = 0
            LET g_reg.ctas_proc     = g_reg.ctas_proc + 1
            --DISPLAY BY NAME g_reg.ctas_proc

            LET l_reg.fecha_pago        = g_reg.fecha_aplica  
            LET l_reg.fecha_valor       = g_reg.fecha_aplica  
            LET l_reg.fecha_conversion  = g_reg.fecha_accion  
            LET l_reg.monto_en_acciones = l_reg.monto_en_pesos / x_precio_accion
            LET l_reg.precio_accion     = x_precio_accion
            LET l_reg.etiqueta          = 1
    
            IF l_reg.monto_en_pesos <> 0 THEN
               INSERT INTO dis_cuenta VALUES (l_reg.*)
            END IF
    
            LET g_reg.total_interes = g_reg.total_interes + l_reg.monto_en_pesos

         END FOREACH

         LET tot_int_char = g_reg.total_interes
         LET tot_nss_char = g_reg.num_nss
         LET resul_dis    = 'FINALIZADO ',
      	   	            'Interes : ',tot_int_char CLIPPED

         CALL Actualiza_etapa(folio_bus,cont_etapa,resul_dis)

         ---- Identifica nss que cambiaron de siefore ----            --c22-6.6
         LET ejecuta= "fglgo DISB078B.4gi ",l_reg.folio CLIPPED       --c22-6.6
         RUN ejecuta                                                  --c22-6.6

         ---- Rehabilita cuentas con interes          ----            --v3
         LET ejecuta= "fglgo DISB028B.4gi ",l_reg.folio CLIPPED       --v3
         RUN ejecuta                                                  --v3

      END IF --c22-6
   END FOREACH

   CALL Actualiza_etapa(0,1,'FINALIZADO')

   DATABASE safre_af

   DISPLAY 'FINALIZO PROCESO DE LIQUIDACION INTERESES EN TRANSITO EST'
END FUNCTION  # dispersa_interes


FUNCTION Ingresa_etapa(vfolio,vetapa_cod,vresultado)
   DEFINE
      vfolio        INTEGER,
      vetapa_cod    DECIMAL(2,0),
      vresultado    CHAR(50)

   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO dis_ctrl_proceso VALUES
      (TODAY,                   -- fecha_proceso
       "DISB027",               -- proceso_cod
       vetapa_cod,              -- etapa_cod
       hora_inicial,            -- hora_inicial
       hora_final,              -- hora_final
       g_reg.fecha_aplica,      -- parametro1
       NULL,                    -- parametro2
       NULL,                    -- parametro3
       NULL,                    -- parametro4
       NULL,                    -- parametro5
       vfolio,                  -- folio
       "PROCESANDO",            -- resultado
       usuario,                 -- usuario
       0)                       -- consecutivo

   IF STATUS < 0 THEN
      ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso etapa ",vetapa_cod," ",STATUS
      SLEEP 4
      EXIT PROGRAM
   END IF
END FUNCTION


FUNCTION Actualiza_etapa(vfolio,vetapa_cod,vresultado)
   DEFINE
      vfolio         INTEGER,
      vetapa_cod     DECIMAL(2,0),
      vresultado     CHAR(50)

   DEFINE
      vhora_inicial  CHAR(08),
      vhora_final    CHAR(08)

   LET cla_sel = "SELECT MAX(consecutivo) ",
                 " FROM dis_ctrl_proceso ",
                 " WHERE proceso_cod = 'DISB027' ",
                 " AND etapa_cod = ",vetapa_cod CLIPPED

   PREPARE claexe3 FROM cla_sel
   DECLARE cur_proceso3 CURSOR FOR claexe3
   OPEN cur_proceso3
      FETCH cur_proceso3 INTO vrow
   CLOSE cur_proceso3

   LET vhora_final   = TIME

   LET cla_upd ="UPDATE dis_ctrl_proceso ",
                "SET    dis_ctrl_proceso.hora_final = ","'",vhora_final,"'",",",
                "       dis_ctrl_proceso.folio      = ",vfolio,",",
                "       dis_ctrl_proceso.resultado  = ","'",vresultado,"'",
                " WHERE dis_ctrl_proceso.proceso_cod = 'DISB027' ",
                " AND   dis_ctrl_proceso.etapa_cod   = ",vetapa_cod,
                " AND   dis_ctrl_proceso.consecutivo = ",vrow CLIPPED
      PREPARE claexe35 FROM cla_upd
      EXECUTE claexe35
END FUNCTION


###############################################################################
FUNCTION calcula_dias(fecha)
#cd------------------------
    DEFINE
        fecha		DATE,
        dias            INTEGER

    CASE MONTH(fecha)            #Regresa dias del mes anterior
        WHEN  1   
            LET dias=31
        WHEN  2 #  IF YEAR(fecha) MOD 4 = 0 THEN

            #LET dias = 29
                  #ELSE
            LET dias = 28
                  #END IF
        WHEN  3   
            LET dias=31
          
        WHEN  4   
            LET dias=30
        WHEN  5 
            LET dias=31
        WHEN  6  
            LET dias=30
        WHEN  7  
            LET dias=31
        WHEN  8 
            LET dias=31
        WHEN  9  
            LET dias=30
        WHEN 10 
            LET dias=31
        WHEN 11 
            LET dias=30
        WHEN 12
            LET dias=31
    END CASE

    RETURN dias
END FUNCTION
           
################################################################## Fin DISB009 
