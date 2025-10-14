###############################################################################
#Proyecto          => safre_af                                                #
#Owner             => E.F.P                                                   #
#Programa DISB011  => CALCULO INTERESE RCV EN TRANSITO PROVISION              #
#Created           => 11 DE MAYO de 1998                                      #
#By                => HECTOR FERNANDEZ A.                                     #
#Sistema           => DIS                                                     #
#Modifico          => FRANCISCO JAVIER LARIOS HERNANDEZ.                      #
#Fecha             => 20/AGOSTO/2003.                                         #
#                  => PROVISIONA TODOS LOS FOLIOS DEL ARCHIVO DE INTERESES EN #
#                  => TRANSITO RCV DE PROCESAR.                               #
#                  => VERSION PRODUCTIVA.                                     #
#Modify            => JOSE ALEJANDRO RAMIREZ                                  #
#Descr.            => Se agrego al filtro del query principal un identi       #
#                  => que unicamente toma los registros para rcv evitando     #
#                  => que pasaran folios repetidos                            #
-------------------------------------------------------------------------------
#Fehca modif       => 03 diciembre 2004                                       #
#Autor             => GERARDO ALFONSO VEGA PAREDES Y ALEJANDRO RAMIREZ LARA   #
#Descripcion       => Adecuacion para que el calculo tome multisiefore        #
-------------------------------------------------------------------------------
#Fecha modifi      => 25 febrero 2005                                         #
#Autor             => GERARDO ALFONSO VEGA PAREDES                            #
#Descripcion       => Se adecuo programa debido a que en la pruebas de        #
#                  => PROCESAR en dic 2004 envio el ident_pago de dispersion  #
#                  => en el ident_pago de interses en transito por lo que     #
#                  => arreglo de raiz ya se duplcan folios por fecha gen arc  #
#                  => y lote vienen repetidos por separon voluntaria de rcv   #
#                  => y en las pruebas no lo mencionaron                      #
-------------------------------------------------------------------------------
#Fecha modifi      => 14 abril 2005                                           #
#Autor             => GERARDO ALFONSO VEGA PAREDES                            #
#Descripcion       => Cambio identificado con v5 el cual genera el mes        #
#                  => faltante cuando la fecha liquidacion es el ultimo dia   #
#                  => del mes.                                                #
-------------------------------------------------------------------------------
#Fecha modifi      => 24 junio 2005                                           #
#Autor             => Alejandro Ramirez (v2)                                  #
#Descripcion       => Pase a FLOAT las variables vret,vvol,vcom por dif 1 cent#
-------------------------------------------------------------------------------
#Fecha modifi      => 16 Mazo 2006 Por Alejandro Ramirez                      #
#Descripcion       => (c22-11) Se agregan 2 subcuentas de la circula 22-11    #
-------------------------------------------------------------------------------
#Fecha modifi      => 16 Junio 2008 Por DMR  (Linea 297)                      #
#Descripcion       => Se quito IF que no pasaba subcuenta 3,11,15 por el      #
#                     select de cta_regimen para colocar la siefore actual.   #
###############################################################################
DATABASE safre_af

GLOBALS
   DEFINE g_reg      RECORD
      fecha_aplica      DATE,
      nom_archivo       CHAR(20),
      fecha_accion      DATE,
      precio_accion     DECIMAL(16,6),
      folios            CHAR(50),
      fecha_dispersa    DATE,
      ctas_proc         INTEGER,
      num_nss           INTEGER,
      total_interes     DECIMAL(20,6)
    END RECORD

   DEFINE
      hist_fecha_aplica DATE,
      hora_inicial      CHAR(08),
      hora_final        CHAR(08),
      vrow              INTEGER,
      cla_sel           CHAR(1000),
      cla_upd           CHAR(1000),
      resul_dis         CHAR(50),
      folio_bus         INTEGER,
      tot_int_char      CHAR(11),
      tot_nss_char      CHAR(11),
      x_precio_accion   FLOAT,
      max_folio_int     INTEGER,
      i,cont,mes,ano    INTEGER,
      fec               CHAR(08),
      cons              CHAR(03),
      vide              CHAR(01),   --c22-6.5
      vident_pago       CHAR(16),   --c22-6.7
      vsub              SMALLINT,   --c22-6.8
      contador          INTEGER,
      tipo_reg          CHAR(02),
      mes_tasa,ano_tasa INTEGER,
      dias_del_mes      INTEGER,
      hoy               DATE,
      opc               CHAR(1),
      usuario           CHAR(12),
      sw                SMALLINT,
      vfecha_valor      DATE,
      query_1           CHAR(500),  --c22-6.8
      vfecha_conversion DATE,       --v5
      vfecha_final      DATE,       --v5
      vult_dia          INTEGER,    --v5
      vmes              INTEGER,    --v5
      vano              INTEGER     --v5
END GLOBALS


MAIN
   OPTIONS
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY CONTROL-I,
      COMMENT LINE LAST

   CALL STARTLOG("DISB011.log")

   LET g_reg.fecha_aplica  = ARG_VAL(1)
   LET g_reg.nom_archivo   = ARG_VAL(2)

   DISPLAY 'FECHA DE APLICACION :',g_reg.fecha_aplica
   DISPLAY 'NOMBRE DE ARCHIVO   :',g_reg.nom_archivo

   SELECT USER
   INTO   usuario
   FROM   seg_modulo
   WHERE  modulo_cod = "dis"

   LET hoy=TODAY

   LET g_reg.precio_accion = 1
   LET x_precio_accion     = 1

   CALL dispersa_interes ()
END MAIN


FUNCTION dispersa_interes ()
   DEFINE l_reg RECORD LIKE cta_interes_rcv.*

   DEFINE l_cta RECORD
      nss              CHAR(11),
      subcuenta        SMALLINT,
      folio            INTEGER
   END RECORD

   DEFINE l_mov RECORD
      nss              CHAR(11),
      consecutivo_lote INTEGER,     
      siefore          SMALLINT,
      folio            INTEGER,
      subcuenta        SMALLINT,
      fecha_valor      DATE,
      fecha_conversion DATE,
      monto_en_pesos   DECIMAL(16,6)
   END RECORD

   DEFINE 
      interes          DECIMAL(16,6),  --OJO2
      x_aporte_pesos   DECIMAL(16,6),  --OJO2
      x_tasa_mes       DECIMAL(10,6),  --OJO2
      x_fecha_calculo  DATE,
      x_fecha_final    DATE,
      vfecha_final     DATE,
      x_fecha_limite   DATE,
      x_fecha_tasa     DATE,
      x_fecha_dias     DATE,
      cont             INTEGER,
      cont_etapa       INTEGER

-- DEFINE vret         DECIMAL(15,6),  --c22-6.2
--        vvol         DECIMAL(15,6),  --c22-6.2
--        vcom         DECIMAL(15,6)   --c22-6.2

   DEFINE vret         FLOAT,          --c22-6.2 --v2
          vvol         FLOAT,          --c22-6.2 --v2
          vcom         FLOAT           --c22-6.2 --v2

   DEFINE vsiefore     SMALLINT        --c22-6.6

   WHENEVER ERROR CONTINUE
   DATABASE safre_tmp
   DROP TABLE tmp_interes_sub
   WHENEVER ERROR STOP

   SQL
      CREATE TABLE tmp_interes_sub
      (
       fol INTEGER,
       sub SMALLINT,
       sie SMALLINT,
       ide CHAR(01),
       idp CHAR(16),
       mto DECIMAL(16,6)
      ) LOCK MODE ROW;
   END SQL

   DATABASE safre_af

   LET g_reg.fecha_dispersa = g_reg.fecha_aplica
   LET g_reg.ctas_proc      = 0
   LET g_reg.total_interes  = 0
   LET contador             = 0
   LET cont_etapa           = 1

   LET vret = 0
   LET vvol = 0
   LET vcom = 0

   LET folio_bus = 0  --c22-6.4

   DECLARE cursor_tmp CURSOR FOR
   SELECT n_registros[1,2],
          n_registros[24,31],
          n_registros[21,23],
          n_registros[93],                   --c22-6.5
          n_registros[5,20],                 --c22-6.7
          n_registros[19]                    --c22-6.8
   FROM   safre_tmp:tmp_pla_rcv
   WHERE  n_registros[93,93] <> 3
   AND    n_registros[18] = '5'
   GROUP  BY 1,2,3,4,5

   FOREACH cursor_tmp INTO tipo_reg,
                           fec,
                           cons,
                           vide,
                           vident_pago,
                           vsub           --c22-6.8

      LET contador = contador + 1

      SELECT a.folio
      INTO   folio_bus
      FROM   dis_cza_aporte a
      WHERE  a.fech_creac_lote = fec
      AND    a.lote_del_dia    = cons

      IF STATUS = NOTFOUND THEN           --c22-6.4
         LET folio_bus = 0                --c22-6.4
      END IF                              --c22-6.4

      IF folio_bus <> 0 THEN              --c22-6.4

         SELECT MAX(fech_liquidacion)        --v5
         INTO   vfecha_conversion            --v5
         FROM   dis_dep_aporte               --v5
         WHERE  folio = folio_bus            --v5
         AND    ident_pago[14,15] = "41"     --v5

         LET g_reg.total_interes  = 0
         LET g_reg.num_nss        = 0
   
         LET cont_etapa = cont_etapa + 1

         DISPLAY 'FOLIO :', folio_bus
     
         CALL Ingresa_etapa(folio_bus,cont_etapa,'PROCESANDO')

         CASE                                                        --c22-6.8
            WHEN  vsub='1'    -----RCV 
               LET query_1 = "SELECT nss,                       ",
                             "       consecutivo_lote,          ",
                             "       siefore,                   ",
                             "       folio,                     ",
                             "       subcuenta,                 ",
                             "       fecha_valor,               ",
                             "       fecha_conversion,          ",
                             "       sum(monto_en_pesos)        ",
                             "FROM dis_cuenta                   ",
                             "WHERE folio           = ", folio_bus CLIPPED,
                             " AND subcuenta       IN (1,2,17,15) ", --c22-11
                             "AND tipo_movimiento IN (1,2,4)    ",
                             "GROUP BY 1,2,3,4,5,6,7            "
   
            WHEN  vsub='5'    -----VOL                               --c22-6.8
               LET query_1 = "SELECT nss,                       ",
                             "       consecutivo_lote,          ",
                             "       siefore,                   ",
                             "       folio,                     ",
                             "       subcuenta,                 ",
                             "       fecha_valor,               ",
                             "       fecha_conversion,          ",
                             "       sum(monto_en_pesos)        ",
                             "FROM dis_cuenta                   ",
                             "WHERE folio           = ", folio_bus CLIPPED,
                             " AND subcuenta        = 3         ",
                             "AND tipo_movimiento IN (1,2,4)    ",
                             "GROUP BY 1,2,3,4,5,6,7            "
            WHEN vsub='6'     -----COMPL                            --c22-112
               LET query_1 = "SELECT nss,                       ",
                             "       consecutivo_lote,          ",
                             "       siefore,                   ",
                             "       folio,                     ",
                             "       subcuenta,                 ",
                             "       fecha_valor,               ",
                             "       fecha_conversion,          ",
                             "       sum(monto_en_pesos)        ",
                             "FROM dis_cuenta                   ",
                             "WHERE folio           = ", folio_bus CLIPPED,
                             " AND subcuenta       = 11         ",
                             "AND tipo_movimiento IN (1,2,4)    ",
                             "GROUP BY 1,2,3,4,5,6,7            "
         END CASE
             
         PREPARE  query_ctas FROM  query_1
         DECLARE  cur_mov CURSOR FOR query_ctas
         FOREACH  cur_mov INTO l_mov.*

            LET vsiefore = null                                 --c22-112

            SELECT codigo_siefore                               --c22-6.6
            INTO   vsiefore                                     --c22-6.6
            FROM   cta_regimen                                  --c22-6.6
            WHERE  nss = l_mov.nss                              --c22-6.6
            AND    subcuenta = l_mov.subcuenta
   
            IF vsiefore IS NOT NULL THEN                        --c22-6.6
               LET l_mov.siefore = vsiefore                     --c22-6.6
            ELSE
               ERROR "NSS no existe en cta_regimen ",l_mov.nss  --c22-6.6
               SLEEP 5                                          --c22-6.6
               EXIT PROGRAM
            END IF                                              --c22-6.6

            LET interes         = 0
            LET g_reg.ctas_proc = g_reg.ctas_proc + 1
   
            LET x_fecha_limite  = l_mov.fecha_valor
            LET x_fecha_final   = l_mov.fecha_conversion
            LET x_aporte_pesos  = l_mov.monto_en_pesos
   
            LET ano = YEAR(l_mov.fecha_valor)
            LET mes = MONTH(l_mov.fecha_valor) + 1

            IF mes = 13 THEN
               LET mes = 1
               LET ano = YEAR(l_mov.fecha_valor) + 1
            END IF

            LET x_fecha_calculo = MDY(mes,1,ano)

            IF x_fecha_final = vfecha_conversion THEN                  --v5
               LET vano = YEAR(vfecha_conversion)                      --v5
               LET vmes = MONTH(vfecha_conversion) + 1                 --v5

               IF vmes = 13 THEN                                       --v5
                  LET vmes = 1                                         --v5
                  LET vano = YEAR(vfecha_conversion) + 1               --v5
               END IF                                                  --v5

               LET vfecha_final = MDY(vmes,1,vano)                     --v5
               CALL calcula_dias(vfecha_final) RETURNING vult_dia      --v5
               LET vfecha_final = MDY(vmes,vult_dia,vano)              --v5
            END IF                                                     --v5

            WHILE TRUE
               LET ano_tasa = YEAR(x_fecha_limite)
               LET mes_tasa = MONTH(x_fecha_limite) + 1

               IF mes_tasa = 13 THEN
                  LET mes_tasa = 1
                  LET ano_tasa  = YEAR(x_fecha_limite) + 1
               END IF
          
               LET x_fecha_tasa = MDY(mes_tasa,1,ano_tasa)
      
               SELECT tasa_valor
               INTO   x_tasa_mes
               FROM   tab_tasa_ordinaria
               WHERE  tasa_fecha = x_fecha_tasa
               AND    tasa_origen = "RCV"
      
               IF MONTH(x_fecha_limite) = MONTH(x_fecha_final) AND 
                  YEAR(x_fecha_limite) = YEAR(x_fecha_final)   THEN 
      
                  IF MONTH(x_fecha_limite) = MONTH(g_reg.fecha_aplica) AND
                     YEAR(x_fecha_limite) = YEAR(g_reg.fecha_aplica)   THEN
                     LET x_fecha_calculo = x_fecha_final + 1
                  END IF

                  LET interes = interes +
(((interes + x_aporte_pesos)*(x_fecha_final-x_fecha_limite)*(x_tasa_mes/36000))+
                 ((interes)*(x_fecha_calculo-x_fecha_final)*(x_tasa_mes/36000)))

                  IF x_fecha_final = vfecha_conversion THEN   --v5
                     let x_fecha_final = vfecha_final         --v5
                  END IF                                      --v5

                  IF x_fecha_calculo = x_fecha_final+1 THEN
                     EXIT WHILE
                  ELSE
                     LET x_aporte_pesos = 0
                     LET x_fecha_final  = g_reg.fecha_aplica
                  END IF
               ELSE
                  LET interes = interes + ((interes + x_aporte_pesos) *
                      (x_fecha_calculo - x_fecha_limite) *
                      (x_tasa_mes/36000))
               END IF

               LET x_fecha_limite  = x_fecha_calculo
   
               LET ano = YEAR(x_fecha_calculo)
               LET mes = MONTH(x_fecha_calculo) + 1

               IF mes = 13 THEN
                  LET mes = 1
                  LET ano = YEAR(x_fecha_calculo) + 1
               END IF

               LET x_fecha_calculo = MDY(mes,1,ano)
            END WHILE

            LET l_reg.tipo_movimiento   = 3
            LET l_reg.subcuenta         = l_mov.subcuenta
            LET l_reg.siefore           = l_mov.siefore      --c22-6
            LET l_reg.folio             = l_mov.folio
            LET l_reg.consecutivo_lote  = l_mov.consecutivo_lote
            LET l_reg.nss               = l_mov.nss
            LET l_reg.curp              = ""
            LET l_reg.folio_sua         = 0
            LET l_reg.fecha_pago        = g_reg.fecha_aplica  
            LET l_reg.fecha_valor       = g_reg.fecha_aplica  
            LET l_reg.fecha_conversion  = x_fecha_calculo
            LET l_reg.monto_en_pesos    = interes
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

            IF l_reg.monto_en_pesos > 0 THEN 
               CASE                                                  --c22-6.2
                  WHEN l_reg.subcuenta = 1 OR l_reg.subcuenta = 2 OR --c22-6.2
                       l_reg.subcuenta = 17 OR l_reg.subcuenta = 15  --c22-11
                     LET vret = interes                              --c22-6.2
                     INSERT INTO safre_tmp:tmp_interes_sub VALUES    --c22-6.5
   (l_reg.folio,l_reg.subcuenta,l_reg.siefore,vide,vident_pago,vret) --c22-6.7

                  WHEN l_reg.subcuenta = 3                           --c22-6.2
                     LET vvol = interes                              --c22-6.2
                     INSERT INTO safre_tmp:tmp_interes_sub VALUES    --c22-6.5
   (l_reg.folio,l_reg.subcuenta,l_reg.siefore,vide,vident_pago,vvol) --c22-6.7

                  WHEN l_reg.subcuenta = 11                          --c22-6.2
                     LET vcom = interes                              --c22-6.2
                     INSERT INTO safre_tmp:tmp_interes_sub VALUES    --c22-6.5
   (l_reg.folio,l_reg.subcuenta,l_reg.siefore,vide,vident_pago,vcom) --c22-6.7

               END CASE

               INSERT INTO cta_interes_rcv VALUES (l_reg.*)
            END IF
    
            LET g_reg.total_interes = g_reg.total_interes + interes
         END FOREACH

         SELECT UNIQUE nss 
         FROM   dis_cuenta
         WHERE  folio = folio_bus
         AND    subcuenta       IN (1,2,3)
         AND    tipo_movimiento IN (1,2,4)
         INTO TEMP unico_nss

         SELECT COUNT(*)
         INTO   g_reg.num_nss
         FROM   unico_nss

         DROP TABLE unico_nss

         LET tot_int_char = g_reg.total_interes
         LET tot_nss_char = g_reg.num_nss
         LET resul_dis    = 'FINALIZADO ', 
                            'Interes : ',tot_int_char CLIPPED,' ',
                            'Cuentas : ',tot_nss_char CLIPPED

         CALL Actualiza_etapa(folio_bus,cont_etapa,resul_dis)

      END IF                  --c22-6.4
   END FOREACH

   CALL Actualiza_etapa(0,1,'FINALIZADO')
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
       "DISB011",               -- proceso_cod
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
                 " WHERE proceso_cod = 'DISB011' ",
                 " AND etapa_cod = ",vetapa_cod CLIPPED

   PREPARE claexe3 FROM cla_sel
   DECLARE cur_proceso3 CURSOR FOR claexe3
   OPEN cur_proceso3
      FETCH cur_proceso3 INTO vrow
   CLOSE cur_proceso3

   LET vhora_final   = TIME

   LET cla_upd= "UPDATE dis_ctrl_proceso ",
                "SET    dis_ctrl_proceso.hora_final = ","'",vhora_final,"'",",",
                "       dis_ctrl_proceso.folio      = ",vfolio,",",
                "       dis_ctrl_proceso.resultado  = ","'",vresultado,"'",
                " WHERE dis_ctrl_proceso.proceso_cod= 'DISB011' ",
                " AND   dis_ctrl_proceso.etapa_cod  = ",vetapa_cod,
                " AND   dis_ctrl_proceso.consecutivo= ",vrow CLIPPED
      PREPARE claexe35 FROM cla_upd
      EXECUTE claexe35
END FUNCTION


FUNCTION calcula_dias(fecha)
   DEFINE
      fecha DATE,
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

