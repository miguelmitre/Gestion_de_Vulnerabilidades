###############################################################################
# Owner             => E.F.P.
# Programa DISB010C => INDIVIDUALIZACION COMISION SALDO MENSUAL
# Fecha creacion    => 17 DE ABRIL DE 2000
# By                => GERARDO ALFONSO VEGA PAREDES
# Actualizacion     => 03 OCTUBRE 2002
#                      EMPLEANDO SQLS MASIVOS TABLAS TEMPORALES
# Sistema           => DIS
# Fecha modify      => 7 de Marzo de 2005 Alejandro Ramirez
# Descripcion       => Adecuacion de multisiefore.
-------------------------------------------------------------------------------
# Fecha modifica    => 20 de Dic de 2003 Alejandro Ramirez
# Descripcion       => (v3) Se quitaron los valores duros, para considerarlos
#                   => en tabla tab_tasa_comision, donde la subcuenta 15 se
#                   => calcula con distinto valor a las demas.
-------------------------------------------------------------------------------
# Fecha modifica    => 25 Julio 2006     Alejandro Ramirez                     
# Descripcion       => (v5) Sentencias para impedir se ejecuten 2 veces los pro
-------------------------------------------------------------------------------
# Versión           => v4
# Autor             => GERARDO ALFONSO VEGA PAREDES
# Fecha             => 12 junio 2006.
# Descripcion       => Se agreo un if por si viene un subcuenta en nulo no
#                   => haga el insert a safre_tmp:tmp_cta_saldo
-------------------------------------------------------------------------------
# Versión           => v6
# Autor             => GERARDO ALFONSO VEGA PAREDES
# Fecha             => 19 febrero 2008.
# Descripción       => Revisión del programa para que tomo n siefores
-------------------------------------------------------------------------------
# Versión           => v7
# Autor             => GERARDO ALFONSO VEGA PAREDES
# Fecha             => 27 febrero 2008.
# Descripción       => Se agregó variable para anio bisiesto
-------------------------------------------------------------------------------
# Versión           => v8
# Autor             => GERARDO ALFONSO VEGA PAREDES
# Fecha             => 11 junio 2009.
# Descripción       => funcionalidad para que no calcule a registros en cero
###############################################################################
DATABASE safre_af

GLOBALS
   DEFINE
      HOY,
      g_fecha_top,
      g_fecha_ini_prov,
      g_fecha_fin_prov,
      g_fecha_corte_ant,
      g_corte_cambio,
      g_fecha_cambio      DATE

   DEFINE 
      g_nss               CHAR(011),
      usuario	          CHAR(008),
      HORA                CHAR(008),
      enter               CHAR(001) 

   DEFINE
      prioridad_h         CHAR(030),
      prioridad_l         CHAR(030),
      promedio            CHAR(500),
      inserta             CHAR(100) 

   DEFINE
      i                   SMALLINT

   DEFINE
      d                   DECIMAL(10,0),
      g_tasa_1            DECIMAL(16,6),
      g_tasa_2            DECIMAL(16,6),
      g_sel_saldo         CHAR(600)

   DEFINE
      opc                 CHAR(01)

   DEFINE
      vhora_final         CHAR(08)    --v5

   DEFINE
      cla_upd             CHAR(450)   --v5

   DEFINE
      tmpfecha            DATE        --v7

   DEFINE
      tmp_anno            INTEGER     --v7
END GLOBALS


MAIN
   OPTIONS ERROR LINE FIRST ,
   ACCEPT KEY CONTROL-I
   DEFER INTERRUPT

   CALL STARTLOG("disb010c.log")
   ERROR " DISB010C:  PROCESANDO CALCULO COMISION POR SALDO"

   CALL init()
   CALL borra_comision()
   CALL genera_saldos()
   CALL carga_movimientos()
   CALL dispersa_comision()
   CALL actualiza_historico()

   ---v5
   LET vhora_final = TIME
   LET cla_upd = "UPDATE dis_ctrl_proceso ",
                 "SET    hora_final = ","'",vhora_final,"'",",",
                 "       resultado  = 'FINALIZADO' ",
                 " WHERE proceso_cod  = 'DISB010C' ",
                 " AND   resultado like 'PROC%'" CLIPPED

   PREPARE claupdexe FROM cla_upd 
   EXECUTE claupdexe

   ERROR " DISB010C:  CALCULO COMISION POR SALDO FINALIZADO"
END MAIN


FUNCTION init()
   LET HOY  = TODAY
   LET HORA = TIME

   LET prioridad_h = "set pdqpriority high"
   LET prioridad_h = prioridad_h CLIPPED
   LET prioridad_l = "set pdqpriority off"

   LET promedio = " SELECT AVG(precio_del_dia),COUNT(*) ",
                  " FROM glo_valor_accion ",
                  " WHERE codigo_siefore = ? ",   --c22-6
                  " AND  fecha_valuacion BETWEEN ? ",
                  " AND ? "

   LET inserta = " INSERT INTO safre_tmp:tmp_com_saldo VALUES (?,?,?,?,?,?)"
   LET inserta = inserta CLIPPED

   SELECT USER
   INTO   usuario
   FROM   glo_parametro

   SELECT UNIQUE c.fecha_corte
   INTO   tmpfecha
   FROM   cta_corte_saldo c,
          cta_his_com_saldo h
   WHERE  c.fecha_corte = h.fecha_corte
   AND    h.subcuenta   = 1
   AND    h.estado      = 0

--   LET tmpfecha = TODAY              --v7
   IF YEAR(tmpfecha) MOD 4 = 0 THEN  --v7
      LET tmp_anno = 36600           --v7
   ELSE                              --v7
      LET tmp_anno = 36500           --v7
   END IF

   SELECT MAX(fecha_aplica)
   INTO   g_fecha_corte_ant
   FROM   cta_his_com_saldo
   WHERE  subcuenta   = 1
   AND    estado      = 2

   SELECT c.fecha_aplica, c.fecha_corte
   INTO   g_fecha_ini_prov, g_fecha_fin_prov
   FROM   cta_corte_saldo c, cta_his_com_saldo h
   WHERE  c.fecha_corte = h.fecha_corte
   AND    h.subcuenta   = 1
   AND    h.estado      = 0
   GROUP  BY 1,2                                       --v2

   LET g_fecha_top      = g_fecha_fin_prov

   # AFORE XXX
   {                               --v3
   LET g_tasa_1         = 0.25
   LET g_tasa_2         = 0.25
   LET g_corte_cambio   = "01/01/1997" -- IGUAL fecha_corte cta_corte_saldo
   LET g_fecha_cambio   = "09/01/1998" -- Fec a partir del cambio
   }
END FUNCTION


FUNCTION borra_comision()
   DEFINE x_fecha_aplica     DATE,
          x_fecha_max        DATE,
          x_estado           SMALLINT

   SELECT MAX(fecha_aplica), MIN(estado)
   INTO   x_fecha_aplica, x_estado
   FROM   cta_his_com_saldo

   IF x_estado = 0 THEN
      ERROR " DISB010C:  BORRANDO CALCULOS DE comis_saldo_ind"
      WHENEVER ERROR CONTINUE
      DATABASE safre_tmp
      DROP TABLE tmp_com_saldo

      CREATE TABLE tmp_com_saldo 
         (
         nss char(11) not null ,
         subcuenta smallint not null ,
         siefore         smallint,              --c22-6
         fecha_aplica date not null ,
         monto_provision decimal(16,6) not null ,
         total_acciones decimal(16,6)
         );

      DATABASE safre_af
      WHENEVER ERROR STOP
   ELSE
      ERROR " DISB010C:  INCONSISTENCIA DE DATOS, tmp_com_saldo NO BORRADO"
      SLEEP 5  --c22-6
   END IF
END FUNCTION


FUNCTION genera_saldos()
   ERROR " DISB010C:  GENERANDO SALDOS"

   DATABASE safre_tmp

   WHENEVER ERROR CONTINUE
   DROP TABLE tmp_saldo_subcta 
   WHENEVER ERROR STOP

   CREATE TABLE tmp_saldo_subcta 
      (
      nss               CHAR(11),
      subcuenta         SMALLINT,
      siefore           SMALLINT,
      fecha_valor       DATE,
      fecha_conversion  DATE,
      monto_en_acciones DECIMAL(16,6),
      usuario           CHAR(8)
      )

   LET g_sel_saldo = "INSERT INTO tmp_saldo_subcta ",
                     "SELECT a.nss,",
                            "b.subct_ramo,",
                            "a.siefore,",                       --c22-6
                        "'",g_fecha_corte_ant,"',",
                        "'",g_fecha_corte_ant,"',",
                        " CASE WHEN a.siefore = 1 THEN ",       --c22-6
                        "    SUM(a.monto_en_acciones) ",        --c22-6
                        "      WHEN a.siefore = 3 THEN ",       --v3
                        "    SUM(a.monto_en_acciones) ",        --v3
                        " ELSE ",                               --c22-6
                        "    SUM(a.monto_en_acciones) ",        --c22-6
                        " END, ",                               --c22-6
                        "    'safre'",
                        " FROM  safre_af:dis_cuenta a, ",
                              " safre_af:tab_subcuenta b ",
                        " WHERE a.fecha_conversion  < ",
                              "'",g_fecha_corte_ant,"'",
                        " AND   a.subcuenta         NOT IN (4,8,14,19,35,36) ",
                        " AND   b.subct_cod         = a.subcuenta ",
                        " GROUP BY 1,2,3,7 "

   LET g_sel_saldo = g_sel_saldo CLIPPED
   LET prioridad_h = "set pdqpriority high"

   PREPARE clapri_h FROM prioridad_h

   EXECUTE clapri_h

   PREPARE e_sel_saldo FROM g_sel_saldo

   EXECUTE e_sel_saldo

   CREATE INDEX tmp_saldo_subcta1 
   ON tmp_saldo_subcta (nss,subcuenta,siefore,fecha_conversion) --c22-6

   PREPARE clapri_l FROM prioridad_l
   EXECUTE clapri_l

   UPDATE STATISTICS FOR TABLE tmp_saldo_subcta

   DATABASE safre_af

   ERROR " DISB010C:  GENERAR SALDOS FINALIZADO"
END FUNCTION


FUNCTION carga_movimientos()
   ERROR " DISB010C:  CARGANDO MOVIMIENTOS"

   DATABASE safre_tmp
   WHENEVER ERROR CONTINUE
   DROP TABLE tmp_movtos_nss

   WHENEVER ERROR STOP
   CREATE TABLE tmp_movtos_nss
      (
      nss char(11),
      subcuenta integer,
      siefore   integer,               --c22-6
      fecha_conversion date,
      monto_en_acciones decimal(16,6)
      );

   WHENEVER ERROR STOP
   DATABASE safre_af

   PREPARE clapri_h1 FROM prioridad_h
   EXECUTE clapri_h1

   INSERT INTO safre_tmp:tmp_movtos_nss
   SELECT nss,
          subct_ramo,
          siefore,                                      --c22-6
          fecha_conversion,
          SUM(monto_en_acciones)
   FROM   dis_cuenta a , tab_subcuenta b
   WHERE  a.fecha_conversion BETWEEN g_fecha_ini_prov
                                 AND g_fecha_fin_prov
   AND    a.subcuenta NOT IN (4,8,14,19,35,36)          --c22-6
   AND    b.subct_cod = a.subcuenta
   GROUP BY 1,2,3,4                                     --c22-6

   DATABASE safre_tmp

   CREATE INDEX tmp_movtos_nss1 
   ON tmp_movtos_nss (nss,subcuenta,siefore,fecha_conversion) --c22-6

   UPDATE STATISTICS FOR TABLE tmp_movtos_nss

   DATABASE safre_af

   ERROR " DISB010C:  CARGA MOVIMIENTOS FINALIZADA"
END FUNCTION


FUNCTION dispersa_comision()
   DEFINE reg_1 RECORD
      nss                   CHAR(11),
      subcuenta             SMALLINT,
      siefore               SMALLINT,      --c22-6
      fecha_conversion      DATE,
      monto_en_acciones     DECIMAL(16,6) ,
      tipo                  CHAR(1)
   END RECORD

   DEFINE
      x_fecha_top,
      x_fecha_ini_prov,
      x_fecha_fin_prov      DATE

   DEFINE
      s_dias1               SMALLINT,
      s_dias2               SMALLINT,
      s_dias                SMALLINT

   LET x_fecha_top = g_fecha_top

   PREPARE clapri_l1 FROM prioridad_l
   EXECUTE clapri_l1

   PREPARE cla_promedio FROM promedio
   PREPARE cla_ins FROM inserta

   ERROR " DISB010C:  DISPERSANDO COMISION SALDO"

   DECLARE cur_1 CURSOR FOR
   SELECT nss,
          subcuenta,
          siefore,               --c22-6
          fecha_conversion,
          monto_en_acciones,
          "M"
   FROM   safre_tmp:tmp_movtos_nss
   WHERE  monto_en_acciones <> 0    --v8
   UNION ALL
   SELECT nss,
          subcuenta, 
          siefore,               --c22-6
          fecha_conversion,
          monto_en_acciones,
          "S"
   FROM   safre_tmp:tmp_saldo_subcta
   WHERE  monto_en_acciones <> 0    --v8

   FOREACH cur_1 INTO reg_1.*
      IF reg_1.tipo = "M" THEN
         LET x_fecha_ini_prov = f_calculo(reg_1.fecha_conversion) +1
      ELSE
         LET x_fecha_ini_prov = g_fecha_ini_prov
      END IF

      LET x_fecha_fin_prov = x_fecha_top

      CALL calcula_comision (reg_1.*, x_fecha_ini_prov, x_fecha_fin_prov)
   END FOREACH

   CLOSE cur_1
   FREE cur_1

   DATABASE safre_tmp

   PREPARE clapri_h2 FROM prioridad_h
   EXECUTE clapri_h2

   CREATE INDEX tmp_com_saldo1 
   ON tmp_com_saldo (siefore,subcuenta,nss,fecha_aplica) --c22-6

   PREPARE clapri_l2 FROM prioridad_l
   EXECUTE clapri_l2

   UPDATE STATISTICS FOR TABLE tmp_com_saldo

   DATABASE safre_af

   ERROR " DISB010C:  DISPERSION COMISION SALDO FINALIZADA"
END FUNCTION 


FUNCTION calcula_comision (x_reg, x_fecha_ini_prov, x_fecha_fin_prov)
   DEFINE x_reg RECORD
      nss                 CHAR(11),
      subcuenta           SMALLINT,
      siefore             SMALLINT,        --c22-6
      fecha_conversion    DATE,
      monto_en_acciones   DECIMAL(16,6),
      tipo                CHAR(1)
   END RECORD

   DEFINE reg_com RECORD #loc #reg_com
      nss                 CHAR(11),
      subcuenta           SMALLINT,
      siefore             SMALLINT,        --c22-6
      fecha_aplica        DATE,
      monto_provision     DECIMAL(16,6),
      total_acciones      DECIMAL(16,6)
   END RECORD

   DEFINE
      x_fecha_ini_prov    DATE,
      x_fecha_fin_prov    DATE,
      x_fecha_cambio      DATE

   DEFINE
      s_dias1             SMALLINT,
      s_dias2             SMALLINT,
      s_dias              SMALLINT

   DEFINE
      d_comis_saldo       DECIMAL(16,6),
      d_comis_saldo_pre1  DECIMAL(16,6),
      d_comis_saldo_pre2  DECIMAL(16,6),
      d_precio_del_dia1   DECIMAL(16,6),
      d_precio_del_dia2   DECIMAL(16,6),
      d_precio_del_dia    DECIMAL(16,6),
      x_tasa              DECIMAL(16,6)

   LET s_dias1            = 0
   LET s_dias2            = 0
   LET s_dias             = 0
   LET d_comis_saldo      = 0
   LET d_comis_saldo_pre1 = 0
   LET d_comis_saldo_pre2 = 0
   LET d_precio_del_dia1  = 0
   LET d_precio_del_dia2  = 0
   LET d_precio_del_dia   = 0
   LET x_tasa             = 0
    
   LET g_corte_cambio     = NULL           --v3
   LET g_fecha_cambio     = NULL           --v3
   LET g_tasa_1           = 0              --v3
   LET g_tasa_2           = 0              --v3

   SELECT corte_cambio,                    --v3 
          fecha_cambio,                    --v3
          val_porcentaje,                  --v3
          val_porcentaje2                  --v3
   INTO   g_corte_cambio,                  --v3 
          g_fecha_cambio,                  --v3
          g_tasa_1,                        --v3
          g_tasa_2                         --v3
   FROM   dis_val_comision                 --v3
   WHERE  subcuenta = x_reg.subcuenta      --v3
   AND    siefore   = x_reg.siefore        --v3
   AND    tipo_comision = 110              --v3

   IF x_fecha_fin_prov <> g_corte_cambio THEN
  
      CALL precio_del_dia (x_reg.siefore,
                           x_fecha_ini_prov,
                           x_fecha_fin_prov)--c22-6
      RETURNING d_precio_del_dia, s_dias
    
      IF x_fecha_fin_prov < g_fecha_cambio THEN
         LET x_tasa = g_tasa_1
      ELSE
         LET x_tasa = g_tasa_2
      END IF

      LET d_comis_saldo = x_reg.monto_en_acciones * 
                          s_dias                  *
                          d_precio_del_dia        *
                          x_tasa/tmp_anno     --v7

      IF d_comis_saldo IS NULL THEN
         LET d_comis_saldo = 0
      END IF
   ELSE
      LET x_fecha_cambio = g_fecha_cambio

      CALL precio_del_dia (x_reg.siefore,
                           x_fecha_ini_prov,
                           x_fecha_cambio ) --c22-6
      RETURNING d_precio_del_dia1, s_dias1

      IF s_dias1 IS NULL THEN 
         LET s_dias1 = 0
         LET d_precio_del_dia1 = 0
      END IF

      LET d_comis_saldo_pre1 = x_reg.monto_en_acciones * 
                               s_dias1                 *
                               d_precio_del_dia1       *
                               g_tasa_1/tmp_anno    --v7

      IF d_comis_saldo_pre1 IS NULL THEN
         LET d_comis_saldo_pre1 = 0
      END IF

      IF x_fecha_ini_prov > g_fecha_cambio THEN
         LET x_fecha_cambio = x_fecha_ini_prov - 1
      END IF

      CALL precio_del_dia (x_reg.siefore,
                           x_fecha_cambio + 1,
                           x_fecha_fin_prov) --c22-6
      RETURNING d_precio_del_dia2, s_dias2

      LET d_comis_saldo_pre2 = x_reg.monto_en_acciones  * 
                                s_dias2                 *
                                d_precio_del_dia2       *
                                g_tasa_2/tmp_anno   --v7

      IF d_comis_saldo_pre2 IS NULL THEN
         LET d_comis_saldo_pre2 = 0
      END IF

      LET d_comis_saldo = d_comis_saldo_pre1 +
                          d_comis_saldo_pre2
   END IF

{
   IF x_reg.subcuenta = 1 OR     --c22-6
      x_reg.subcuenta = 2 OR     --c22-6
      x_reg.subcuenta = 5 OR     --c22-6
      x_reg.subcuenta = 6 OR     --c22-6
      x_reg.subcuenta = 9 THEN   --c22-6
      LET x_reg.subcuenta = 1    --c22-6
   END IF                        --c22-6

   IF x_reg.subcuenta = 3 OR     --c22-6
      x_reg.subcuenta = 10 then  --c22-6
      let x_reg.subcuenta = 3    --c22-6
   END IF                        --c22-6

   IF x_reg.subcuenta = 11 OR    --c22-6
      x_reg.subcuenta = 12 then  --c22-6
      let x_reg.subcuenta = 11   --c22-6
   END IF                        --c22-6

   IF x_reg.subcuenta = 7 then   --c22-6
      let x_reg.subcuenta = 7    --c22-6
   END IF  --c22-6

   IF x_reg.subcuenta = 13 then  --c22-6
      let x_reg.subcuenta = 13   --c22-6
   END IF                        --c22-6

   IF x_reg.subcuenta = 15 then  --v3
      let x_reg.subcuenta = 15   --v3
   END IF                        --v3
}

   LET reg_com.nss             = x_reg.nss
   LET reg_com.subcuenta       = x_reg.subcuenta
   LET reg_com.siefore         = x_reg.siefore         --c22-6

   SELECT fecha_habil
   INTO   reg_com.fecha_aplica
   FROM   cta_corte_saldo
   WHERE  fecha_corte = x_fecha_fin_prov

--   LET reg_com.fecha_aplica    = x_fecha_fin_prov + 1
   LET reg_com.monto_provision = d_comis_saldo
   LET reg_com.total_acciones  = x_reg.monto_en_acciones 

   IF reg_com.subcuenta is not null then   --v4
      EXECUTE cla_ins USING reg_com.*      --v4
   END IF                                  --v4
END FUNCTION


FUNCTION actualiza_historico()
   DEFINE
      x_subcuenta           SMALLINT,
      x_siefore             SMALLINT,      --c22-6
      x_monto_provision     DECIMAL(20,6),
      x_ctas_proc	    INTEGER,
      x_estado   	    SMALLINT,
      cuenta_hist	    SMALLINT

   LET cuenta_hist = 0

   SELECT count(*)
   INTO   cuenta_hist
   FROM   cta_his_com_saldo
   WHERE  estado = 0

   DECLARE cur_sal CURSOR FOR
   SELECT subcuenta,
          siefore, 
          SUM(monto_provision), 
          COUNT(*)                         --c22-6
   FROM   safre_tmp:tmp_com_saldo
   GROUP  BY 1,2                           --c22-6

   FOREACH cur_sal INTO x_subcuenta, 
                        x_siefore,         --c22-6
                        x_monto_provision,
                        x_ctas_proc

      IF x_ctas_proc = 0 THEN
         LET x_estado = 2
      ELSE
         LET x_estado = 1
      END IF

      UPDATE cta_his_com_saldo
      SET    provision_afore = x_monto_provision, 
             ctas_proc       = x_ctas_proc,
             estado          = x_estado 
      WHERE  subcuenta       = x_subcuenta
      AND    siefore         = x_siefore      --c22-6
      AND    fecha_corte     = g_fecha_top
      AND    estado          = 0
 
      LET cuenta_hist = cuenta_hist - 1
   END FOREACH
   CLOSE cur_sal
   FREE cur_sal

   let cuenta_hist = 1

   IF cuenta_hist > 0 THEN
      UPDATE cta_his_com_saldo
      SET    provision_afore = 0,
             ctas_proc       = 0,
             estado          = 2
      WHERE  fecha_corte     = g_fecha_top
      AND    estado          = 0
   END IF
END FUNCTION


FUNCTION f_calculo (x_fecha)
   DEFINE diaSemana	  SMALLINT,
          feriado	  SMALLINT,
          finSemana	  SMALLINT,
          x_fecha	  DATE,
          x_fecha_calculo DATE

   LET diaSemana = NULL
   LET feriado   = NULL
   LET finSemana = NULL

   LET diaSemana = WEEKDAY(x_fecha + 1)  

   IF diaSemana = 0 OR diaSemana = 6 THEN
      LET finSemana = 1
   END IF

   SELECT *
   FROM   tab_feriado 
   WHERE  feria_fecha = (x_fecha + 1)  

   IF SQLCA.SQLCODE <> NOTFOUND THEN
      LET feriado = 1
   END IF 

   IF feriado = 1 OR finSemana = 1 THEN
      CALL habil_siguiente(x_fecha + 1) 
      RETURNING x_fecha_calculo
      LET x_fecha_calculo = x_fecha_calculo - 1
   ELSE
      LET x_fecha_calculo = x_fecha
   END IF

   RETURN x_fecha_calculo
END FUNCTION


FUNCTION habil_siguiente(diaActual)
   DEFINE
      diaTmp       DATE,
      contador     SMALLINT,
      diaActual    DATE
   
   DEFINE
      diaHabilSig  DATE,
      diaSemana    SMALLINT,
      feriado      SMALLINT,
      finSemana    SMALLINT
	  
   LET diaHabilSig = diaActual

   WHILE TRUE
      LET feriado   = 0
      LET finSemana = 0
      LET diaSemana = WEEKDAY(diaHabilSig)  

      IF diaSemana = 0 OR diaSemana = 6 THEN
     	 LET finSemana = 1
      END IF
  	     
      SELECT *
      FROM   tab_feriado 
      WHERE  feria_fecha = diaHabilSig
    	
      IF SQLCA.SQLCODE <> NOTFOUND THEN
         LET feriado = 1
      END IF 
		
      IF feriado = 1 OR finSemana = 1 THEN
         LET diaHabilSig = diaHabilSig + 1 UNITS DAY
      ELSE
         EXIT WHILE
      END IF
   END WHILE

   RETURN diaHabilSig
END FUNCTION


FUNCTION precio_del_dia (vsiefore, fecha_ini, fecha_fin)  --c22-6
   DEFINE
      vsiefore       SMALLINT,
      fecha_ini,
      fecha_fin      DATE

   DEFINE
      precio         DECIMAL(16,6),
      dias           SMALLINT

   DECLARE cur_pre CURSOR FOR cla_promedio
   OPEN cur_pre USING vsiefore, fecha_ini, fecha_fin      --c22-6
   FETCH cur_pre INTO precio, dias 
   CLOSE cur_pre
   FREE cur_pre

   RETURN precio,dias
END FUNCTION

