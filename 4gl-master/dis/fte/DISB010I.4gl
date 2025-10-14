###############################################################################
#Proyecto          => SAFRE  ( MEXICO )
#Owner             => E.F.P
#Programa DISB010I => INTEGRA CALCULO DE tmp_com_saldo EN dis_cuenta
#Creado            => 24 Julio  1998.
#By                => GERARDO ALFONSO VEGA PAREDES
#Actualizacion     => 03 OCTUBRE 2002
#By                => GERARDO ALFONSO VEGA PAREDES
#Sistema           => DIS
#Fecha modify      => 7 de Marzo de 2005 Alejandro Ramirez
#Descripcion       => Adecuacion de multisiefore.
-------------------------------------------------------------------------------
#Fecha modifica    => 20 de Diciembre de 2005 Alejandro Ramirez
#Descripcion       => (v3) Se ingreso la lectura de las siefores en tabla.
-------------------------------------------------------------------------------
#Versión           => v4
#Autor             => GERARDO ALFONSO VEGA PAREDES
#Fecha             => 11 junio 2009.
#Descripción       => se quito foreach de sifore de dis_val_comision y se puso
#                  => directo la integración de la tabla tmp_com_saldo y
#                  => glo_valor_accion uniendo la sifore de cada tabla y pasar
#                  => como parametro de dis_val_comision para asegurar que no
#                  => duplique la integracion
-------------------------------------------------------------------------------
#Versión           => v5
#Autor             => GERARDO ALFONSO VEGA PAREDES
#Fecha             => 11 junio 2009.
#Descripción       => Se parametrizo el id_aportante con tab_afore_local
###############################################################################
DATABASE safre_af

GLOBALS
   DEFINE
      x_siefore          SMALLINT,
      x_folio            INTEGER,
      x_id_aportante     CHAR(20),
      x_fecha_corte      DATE,
      x_precio_accion    DECIMAL(16,6),
      hist_fecha_aplica  DATE,
      HOY                DATE,
      opc                CHAR(1),
      usuario            CHAR(8),
      inserta            CHAR(700), --v3
      vfolio             INTEGER    --v3

   DEFINE
      g_reg RECORD LIKE dis_cuenta.*
END GLOBALS


MAIN
   OPTIONS PROMPT LINE last,
   ACCEPT KEY CONTROL-I

   CALL STARTLOG("disb010i.log")

   ERROR " DISB010I:  PROCESANDO INTEGRACION COMISION SALDO"

   CALL init()
   CALL actualiza_historico()

   ERROR " DISB010I:  INTEGRACION COMISION SALDO FINALIZADO"
END MAIN


FUNCTION init()
   DEFINE
      prioridad_h    CHAR(30),
      prioridad_l    CHAR(30)

   DEFINE
      vcodigo_afore  SMALLINT,
      vid_aportante  CHAR(11)

   LET prioridad_h = "set pdqpriority high"
   LET prioridad_l = "set pdqpriority low"

   PREPARE clapri_h FROM prioridad_h
   PREPARE clapri_l FROM prioridad_l

   SELECT codigo_afore
   INTO   vcodigo_afore
   FROM   tab_afore_local

   CASE vcodigo_afore
      WHEN 516 LET vid_aportante = "XXI"
      WHEN 532 LET vid_aportante = "HSBC"
      WHEN 544 LET vid_aportante = "ING"
      WHEN 562 LET vid_aportante = "INVERCAP"
      WHEN 564 LET vid_aportante = "METLIFE"
      WHEN 568 LET vid_aportante = "COPPEL"
      WHEN 574 LET vid_aportante = "SCOTIA"
      WHEN 578 LET vid_aportante = "PENISSSTE"
   END CASE

   INSERT INTO glo_folio VALUES (0)
   SELECT MAX(folio)          --v3
   INTO   vfolio              --v3
   FROM   glo_folio           --v3

   SELECT USER
   INTO   usuario
   FROM   seg_modulo
   WHERE  modulo_cod = 'dis'

   LET HOY = TODAY

   LET inserta = " SELECT 110,",
                 "        subcuenta,",
                 "        siefore,",
                          vfolio,",",
                 "        0,",
                 "        nss,",
                 "        '',",
                 "        0,",
                 "        fecha_aplica,",
                 "        fecha_aplica - 1,",
                 "        fecha_aplica,",
                 "        SUM(monto_provision) * -1,",
                 "        0,",
                 "        precio_del_dia,",   --x_precio_accion,
                 "        0,",
                 "        0,",
                 "'",vid_aportante,"',",                --v5
                 "        6,",
                 "        fecha_aplica,",     --HOY,
                 "        'safre',",          --usuario,
                 "        fecha_aplica,",     --HOY,
                 "        1",
                 " FROM   safre_tmp:tmp_com_saldo, glo_valor_accion ",
                 " WHERE  fecha_valuacion = fecha_aplica ",
                 " AND    codigo_siefore = siefore ",                     --v4
                 " GROUP  BY 2,3,6,9,10,11,14,19,21 ",
                 " HAVING SUM(monto_provision) <> 0 "

   LET inserta = inserta CLIPPED

   EXECUTE clapri_h

   PREPARE clains FROM inserta                              --v4
   DECLARE integra CURSOR FOR clains                        --v4

   FOREACH integra INTO g_reg.*                             --v4
      INSERT INTO safre_af:dis_cuenta VALUES (g_reg.*)      --v4
   END FOREACH

   EXECUTE clapri_l
END FUNCTION


FUNCTION actualiza_historico()

   ERROR " DISB010I:  ACTUALIZANDO HISTORIA"

   UPDATE cta_his_com_saldo
   SET    estado = 2
   WHERE  estado = 1

END FUNCTION

