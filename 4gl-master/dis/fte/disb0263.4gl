###############################################################################
#Proyecto          => safre_af                                                #
#Owner             => E.F.P                                                   #
#Programa DISB011  => CALCULO INTERESE EST EN TRANSITO PROVISION              #
#Created           => 3 de octrubre de 2001.                                  #
#By                => GERAROD ALFONSO VEGA PAREDES.                           #
#Sistema           => DIS                                                     #
-------------------------------------------------------------------------------
#Fehca modif       => 03 diciembre 2004                                       #
#Autor             => GERARDO ALFONSO VEGA PAREDES Y ALEJANDRO RAMIREZ LARA   #
#Descripcion       => Adecuacion para que el calculo tome multisiefore        #
#Fecha modif       => 22 Mzo 2005                                             #
#Autor             => Alejandro Ramirez                                       #
#                  =>(v3) Se solicito considerar la fecha de conversion       #
#                  => de la misma manera como la toma RCV                     #
-------------------------------------------------------------------------------
#Fecha modifi      => 14 abril 2005                                           #
#Autor             => GERARDO ALFONSO VEGA PAREDES                            #
#Descripcion       => Cambio identificado con v5 el cual genera el mes        #
#                  => faltante cuando la fecha liquidacion es el ultimo dia   #
#                  => del mes.                                                #
###############################################################################

DATABASE safre_af

GLOBALS
   DEFINE g_reg RECORD
      fecha_aplica   DATE,
      nom_archivo    CHAR(20),
      fecha_accion   DATE,
      precio_accion  DECIMAL(16,6),
      folios         CHAR(50),
      fecha_dispersa DATE,
      ctas_proc      INTEGER,
      num_nss        INTEGER,
      total_interes  DECIMAL(20,6)
   END RECORD

   DEFINE
      hist_fecha_aplica DATE,
      x_precio_accion   FLOAT,
      max_folio_int     INTEGER,
      i,cont,mes,ano    INTEGER,
      mes_tasa,ano_tasa INTEGER,
      dias_del_mes      INTEGER,
      hoy               DATE,
      opc               CHAR(1),
      usuario           CHAR(12),
      sw                SMALLINT,
      vfecha_valor      DATE,
      fec               CHAR(08),
      cons              CHAR(03),
      vide              CHAR(01),    --c22-6.5
      vident_pago       CHAR(16),    --c22-6.7
      contador          INTEGER,
      tipo_reg          CHAR(02),
      folio_bus         INTEGER,
      tot_int_char      CHAR(11),
      tot_nss_char      CHAR(11),
      resul_dis         CHAR(50),
      cla_sel           CHAR(1000),
      cla_upd           CHAR(1000),
      vrow              INTEGER,
      hora_inicial      CHAR(08),
      hora_final        CHAR(08),
      vfecha_conversion DATE,       --v5
      vfecha_final      DATE,       --v5
      vult_dia          INTEGER,    --v5
      vmes              INTEGER,    --v5
      vano              INTEGER     --v5
END GLOBALS

MAIN
   OPTIONS 
      PROMPT LINE last,
      INPUT WRAP,
      ACCEPT KEY CONTROL-I,
      COMMENT LINE LAST

   LET g_reg.fecha_aplica  = ARG_VAL(1)
   LET g_reg.nom_archivo   = ARG_VAL(2)

   DISPLAY 'FECHA DE APLICACION :',g_reg.fecha_aplica

   SELECT USER
   INTO   usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "dis"

   LET HOY=TODAY

   LET g_reg.precio_accion = 1
   LET x_precio_accion = 1

   CALL dispersa_interes()

END MAIN


FUNCTION dispersa_interes ()
   DEFINE l_reg RECORD LIKE cta_interes_rcv.*

   DEFINE l_cta RECORD
      nss       CHAR(11),
      subcuenta SMALLINT,
      folio     INTEGER
   END RECORD

   DEFINE l_mov RECORD
      nss              CHAR(11),
      consecutivo_lote INTEGER,
      siefore          SMALLINT,   --c22-6
      folio	           INTEGER,
      subcuenta	     SMALLINT,
      fecha_valor      DATE,
      fecha_conversion DATE,
      monto_en_pesos   DECIMAL(16,6)
   END RECORD

   DEFINE 
      interes         FLOAT,
      x_aporte_pesos  FLOAT,
      x_tasa_mes      FLOAT,
      x_fecha_calculo DATE,
      x_fecha_final   DATE,
      vfecha_final    DATE,
      x_fecha_limite  DATE,
      x_fecha_tasa    DATE,
      x_fecha_dias    DATE,
      cont	          INTEGER,
      vfecha_gub1     CHAR(08),
      vfecha_gub2     CHAR(10),
      vfecha_gub      DATE,
      cont_etapa      INTEGER

   DEFINE vsiefore    SMALLINT

   LET g_reg.fecha_dispersa = g_reg.fecha_aplica
   LET g_reg.ctas_proc = 0
   LET g_reg.total_interes = 0
   LET cont_etapa          = 1

   LET folio_bus = 0    --c22-6.4

   DECLARE cursor_tmp CURSOR FOR

   SELECT n_registros[1,2],
          n_registros[24,31],
          n_registros[21,23],
          n_registros[93],          --c22-6.5
          n_registros[5,20]         --c22-6.7
   FROM   safre_tmp:tmp_pla_rcv
   WHERE  n_registros[93,93] =  3 --ESTATA
   AND    n_registros[18] = '5'
   GROUP  BY 1,2,3,4,5

   FOREACH cursor_tmp INTO tipo_reg,fec,cons,vide,vident_pago    --c22-6.7

      SELECT a.folio
      INTO   folio_bus
      FROM   dis_cza_aporte a
      WHERE  a.fech_creac_lote = fec
      AND    a.lote_del_dia    = cons

let folio_bus = 3100

      IF STATUS = NOTFOUND THEN           --c22-6.4
         LET folio_bus = 0                --c22-6.4
      END IF                              --c22-6.4

      IF folio_bus <> 0 THEN                  --c22-6.4

         SELECT MAX(fech_liquidacion)        --v5
         INTO   vfecha_conversion            --v5
         FROM   dis_dep_aporte               --v5
         WHERE  folio = folio_bus            --v5
         AND    ident_pago[14,15] = "42"     --v5

         LET g_reg.total_interes  = 0
         LET g_reg.num_nss        = 0

         LET cont_etapa = cont_etapa + 1


         DECLARE cur_mov CURSOR FOR
         SELECT nss,
                consecutivo_lote,
                siefore,           --c22-6
                folio,
                subcuenta,
                fecha_valor,
                fecha_conversion,
                sum(monto_en_pesos)
         FROM   dis_cuenta
         WHERE  folio           = folio_bus
         AND    subcuenta       IN (5,6,9)
         AND    tipo_movimiento IN (1,2)
         GROUP  BY 1,2,3,4,5,6,7   --c22-6

         FOREACH cur_mov INTO l_mov.*

            LET   vsiefore = null
            SELECT codigo_siefore                               --c22-6.6
            INTO   vsiefore                                     --c22-6.6
            FROM   cta_regimen                                  --c22-6.6
            WHERE  nss = l_mov.nss                              --c22-6.6
            AND    subcuenta = l_mov.subcuenta

            IF vsiefore IS NOT NULL THEN                        --c22-6.6
               LET l_mov.siefore = vsiefore                     --c22-6.6
            ELSE                                                --c22-6.6
               ERROR "NSS no existe en cta_regimen"             --c22-6.6
               SLEEP 5                                          --c22-6.6
               EXIT PROGRAM                                     --c22-6.6
            END IF                                              --c22-6.6

            LET interes = 0
            LET g_reg.ctas_proc     = g_reg.ctas_proc + 1

            -- DISPLAY BY NAME g_reg.ctas_proc 

            SELECT fecha_pago_gub
            INTO   vfecha_gub1
            FROM   dis_det_aporte
            WHERE  folio           = l_mov.folio
            AND    consec_reg_lote = l_mov.consecutivo_lote

            LET vfecha_gub2 = vfecha_gub1[5,6],"/",
                              vfecha_gub1[7,8],"/",
                              vfecha_gub1[1,4]

            LET vfecha_gub = vfecha_gub2

            LET l_mov.fecha_valor = vfecha_gub
      
            LET x_fecha_limite = vfecha_gub
            LET x_fecha_final  = l_mov.fecha_conversion
            LET x_aporte_pesos = l_mov.monto_en_pesos
      
            LET ano = YEAR(l_mov.fecha_valor)
            LET mes = MONTH(l_mov.fecha_valor) + 1
            IF mes = 13 THEN
               LET mes = 1
               LET ano = YEAR(l_mov.fecha_valor) + 1
            END IF
            LET x_fecha_calculo = MDY(mes,1,ano)

            LET l_reg.tipo_movimiento   = 3
            LET l_reg.subcuenta         = l_mov.subcuenta
            LET l_reg.siefore           = l_mov.siefore      --c22-6
            LET l_reg.folio             = l_mov.folio
            LET l_reg.consecutivo_lote  = l_mov.consecutivo_lote
            LET l_reg.nss               = l_mov.nss
            LET l_reg.curp              = ""
            LET l_reg.folio_sua         = 0
            LET l_reg.fecha_pago        = g_reg.fecha_aplica  
            LET l_reg.fecha_valor       = l_mov.fecha_valor
            LET l_reg.fecha_conversion  = x_fecha_limite
            LET l_reg.monto_en_pesos    = x_aporte_pesos
            LET l_reg.monto_en_acciones = interes / x_precio_accion
            LET l_reg.precio_accion     = x_precio_accion
            LET l_reg.dias_cotizados    = 0
            LET l_reg.sucursal          = ""
            LET l_reg.id_aportante      = "BANXICO"
            LET l_reg.estado            = 7
            LET l_reg.fecha_proceso     = hoy
            LET l_reg.usuario           = usuario
            LET l_reg.fecha_archivo     = hoy
            LET l_reg.etiqueta          = 0
    
            IF l_reg.monto_en_pesos <> 0 THEN

--               INSERT INTO safre_tmp:tmp_interes_rcv5 VALUES        --c22-6.2 
--               (l_reg.folio,5,l_reg.siefore,vide,vident_pago,interes)--c22-6.7

----               INSERT INTO safre_tmp:tmp_interes_sub  VALUES        --c22-6.5  
----  (l_reg.folio,l_reg.subcuenta,l_reg.siefore,vide,vident_pago,interes) --c22-6.7

               INSERT INTO safre_tmp:cta_interes_rcv VALUES (l_reg.*)
            END IF
    
            LET g_reg.total_interes = g_reg.total_interes + interes
         END FOREACH


         LET tot_int_char = g_reg.total_interes
         LET tot_nss_char = g_reg.num_nss

         LET resul_dis = 'FINALIZADO ',
                         'Interes : ',tot_int_char CLIPPED,' ',
	                 'Cuentas : ',tot_nss_char CLIPPED


      END IF         --c22-6.3
    
   END FOREACH


END FUNCTION


FUNCTION Ingresa_etapa(vfolio,vetapa_cod,vresultado)
   DEFINE
      vfolio     INTEGER,
      vetapa_cod DECIMAL(2,0),
      vresultado CHAR(50)

   LET hora_inicial = TIME
   LET hora_final   = NULL

   INSERT INTO dis_ctrl_proceso VALUES
    (TODAY,                   -- fecha_proceso
     "DISB026",               -- proceso_cod
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
     ERROR "ERROR AL INSERTA EN TABLA dis_ctrl_proceso etapa ",
  	    vetapa_cod," ",STATUS
	    SLEEP 4
	    EXIT PROGRAM
   END IF
END FUNCTION


FUNCTION Actualiza_etapa(vfolio,vetapa_cod,vresultado)
   DEFINE
      vfolio     INTEGER,
      vetapa_cod DECIMAL(2,0),
      vresultado CHAR(50)

   DEFINE
      vhora_inicial  CHAR(08),
      vhora_final    CHAR(08)

   LET cla_sel = "SELECT MAX(consecutivo) ",
                 " FROM dis_ctrl_proceso ",
                 " WHERE proceso_cod = 'DISB026' ",
                 " AND etapa_cod = ",vetapa_cod CLIPPED

   PREPARE claexe3 FROM cla_sel
   DECLARE cur_proceso3 CURSOR FOR claexe3
   OPEN cur_proceso3
      FETCH cur_proceso3 INTO vrow
   CLOSE cur_proceso3

   LET vhora_final   = TIME

 LET cla_upd = "UPDATE dis_ctrl_proceso ",
               "SET    dis_ctrl_proceso.hora_final = ","'",vhora_final,"'",",",
               "       dis_ctrl_proceso.folio      = ",vfolio,",",
               "       dis_ctrl_proceso.resultado  = ","'",vresultado,"'",
              " WHERE  dis_ctrl_proceso.proceso_cod  = 'DISB026' ",
              " AND    dis_ctrl_proceso.etapa_cod    = ",vetapa_cod,
              " AND    dis_ctrl_proceso.consecutivo = ",vrow CLIPPED
      PREPARE claexe35 FROM cla_upd
      EXECUTE claexe35

END FUNCTION

FUNCTION calcula_dias(fecha)
   DEFINE fecha DATE,
          dias  INTEGER

   CASE MONTH(fecha)            #Regresa dias del mes anterior
      WHEN  1 LET dias=31
      WHEN  2 IF YEAR(fecha) MOD 4 = 0 THEN
                 LET dias = 29
              ELSE
                 LET dias = 28
              END IF
      WHEN  3 LET dias=31
      WHEN  4 LET dias=30
      WHEN  5 LET dias=31
      WHEN  6 LET dias=30
      WHEN  7 LET dias=31
      WHEN  8 LET dias=31
      WHEN  9 LET dias=30
      WHEN 10 LET dias=31
      WHEN 11 LET dias=30
      WHEN 12 LET dias=31
   END CASE

   RETURN dias
END FUNCTION
