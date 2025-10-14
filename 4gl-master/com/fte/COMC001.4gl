###############################################################################
#Proyecto          => Sistema de Afores. ( MEXICO )			      #
#Propietario       => E.F.P.        					      #
#Programa          => COMC001                                                 #
#Descripcion       => Consulta de comisiones por Nomina                       #
#Fecha             => 28 agosto 2001.                                         #
#By                => GERARDO ALFONSO VEGA PAREDES.			      #
#Sistema           => COM. 					              #
###############################################################################

DATABASE safre_af

GLOBALS
   DEFINE g_reg1 RECORD
      codven       LIKE com_bono_resumen.codven,
      nombre       CHAR(30),
      cod_promotor LIKE pro_mae_promotor.cod_promotor 
   END RECORD

   DEFINE g_reg2 ARRAY[1000] OF RECORD
      periodo        CHAR(06),
      fecha_calculo  DATE,
      total_comision DECIMAL(12,2),
      total_bono     DECIMAL(12,2)
   END RECORD

   DEFINE g_reg3 ARRAY[1000] OF RECORD
      n_seguro         CHAR(11),
      fentcons         DATE,
      n_rfc            CHAR(13),
      num_sm           DECIMAL(6,2),
      tipo_pago        CHAR(02),
      monto_comision   DECIMAL(12,2),
      cod_esq_comision SMALLINT
   END RECORD

   DEFINE g_reg4 RECORD
      n_seguro          CHAR(11),
      fecha_elaboracion DATE,
      fentcons          DATE
   END RECORD

   DEFINE g_reg5 ARRAY[1000] OF RECORD
      desde       DECIMAL(6,2),
      hasta       DECIMAL(6,2),
      periodo     CHAR(06),
      salario     DECIMAL(12,2),
      sm          DECIMAL(6,2),
      num_sm      DECIMAL(6,2),
      comision    DECIMAL(12,2),
      fecha_corte DATE
   END RECORD

   DEFINE hoy DATE

   DEFINE cla_sel CHAR(400)

   DEFINE i   SMALLINT,
          pos SMALLINT

   DEFINE periodo        CHAR(06),
          vigencia       SMALLINT,
          total_comision DECIMAL(12,2)

   DEFINE opc CHAR(01)

END GLOBALS

MAIN
   OPTIONS 
      PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY control-o
	
   DEFER INTERRUPT

   LET hoy = TODAY

   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "COMC0011" ATTRIBUTE( BORDER)
   DISPLAY " COMC001               CONSULTA  DE  COMISIONES                                " AT 3,1 ATTRIBUTE(REVERSE) 
   DISPLAY hoy USING "dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)
   DISPLAY "RESUMEN DE PAGOS" AT 7,28

   CALL Consulta()

   CLOSE WINDOW ventana_1
END MAIN

FUNCTION Inicializa()
   DEFINE i smallint

   INITIALIZE g_reg1 TO NULL 

   FOR i=1 TO 9
      DISPLAY g_reg2[i].* TO scr_1[i].*
   END FOR

   CLEAR SCREEN
END FUNCTION

FUNCTION Consulta()
   DEFINE
      pat CHAR(40),
      mat char(40),
      nom char(40),
      pos SMALLINT   

   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " CONSULTA " AT 1,65 ATTRIBUTE (REVERSE)
   DISPLAY " [ Ctrl-C ] Salir de Consulta" AT 2,1

   LET INT_FLAG = FALSE

   INPUT BY NAME g_reg1.* WITHOUT DEFAULTS
      AFTER FIELD codven
         SELECT paterno,
                materno,
                nombres
         INTO   pat,
                mat,
                nom
         FROM   com_nomina
         WHERE  nomina_cod[3,10] = g_reg1.codven
         IF STATUS = NOTFOUND THEN
            ERROR "NO EXISTE ESTE NUMERO DE NOMINA"
            NEXT FIELD codven
         END IF
         SELECT cod_promotor
         INTO   g_reg1.cod_promotor
         FROM   pro_mae_promotor 
         WHERE  codven[3,10] = g_reg1.codven

         LET g_reg1.nombre = nom CLIPPED," ",
                             pat CLIPPED," ",
                             mat CLIPPED 

         DISPLAY BY NAME g_reg1.nombre,g_reg1.cod_promotor
  
         EXIT INPUT
   
      ON KEY (INTERRUPT)
         LET INT_FLAG = TRUE
         EXIT INPUT
   END INPUT

   IF INT_FLAG THEN
      ERROR "Operacion abortada"
      LET INT_FLAG = FALSE
      RETURN
   END IF

   LET cla_sel ="SELECT 1,a.fecha_corte,a.total_comision,b.total_bono ",
                "FROM   com_comis_resumen a,OUTER com_bono_resumen b",
               " WHERE  a.codven = ","'",g_reg1.codven,"'",
               " AND    a.codven = b.codven ",
               " AND    a.cod_tipo_prom = b.cod_tipo_prom ",
               " AND    a.coduni_n1     = b.coduni_n1 ",
               " AND    a.nivel         = b.nivel ",
               " AND    a.fecha_corte   = b.fecha_corte ",
               " ORDER BY 1" CLIPPED

   ERROR "Buscando Informacion"

   PREPARE claexe FROM cla_sel
   DECLARE cursor_1 CURSOR FOR claexe
   LET pos = 1
   FOREACH cursor_1 INTO g_reg2[pos].*

      LET cla_sel = "SELECT periodo ",
                    "FROM   com_quincena ",
                    "WHERE  fecha_sol_proceso BETWEEN ","'",g_reg2[pos].fecha_calculo,"'",
                    "AND    ","'",g_reg2[pos].fecha_calculo,"'"

      PREPARE claexe4 FROM cla_sel
      DECLARE cursor_4 CURSOR FOR claexe4
      FOREACH cursor_4 INTO g_reg2[pos].periodo
      END FOREACH

      LET pos = pos + 1
      IF pos >= 1000 THEN
         ERROR "Sobrepaso la capacidad del arreglo"
         EXIT FOREACH
      END IF 
   END FOREACH

   ERROR ""

   CALL  SET_COUNT(pos-1)

   IF (pos-1) >= 1 THEN
      DISPLAY ARRAY g_reg2 TO scr_1.*
         ON KEY (CONTROL-M)
            LET i = ARR_CURR()
            CALL Comision_detalle(g_reg2[i].fecha_calculo)
         ON KEY (INTERRUPT)
            EXIT DISPLAY
      END DISPLAY
      DISPLAY "" AT 4,1
   ELSE
      ERROR "ARCHIVO DE COMISIONES VACIO"
      SLEEP 1
      ERROR ""
   END IF

   DISPLAY "" AT 8,1 ATTRIBUTE(REVERSE)

   CALL inicializa()

END FUNCTION

FUNCTION Comision_detalle(vfecha_corte)

   DEFINE vfecha_corte DATE

   OPEN WINDOW ventana_2 AT 8,2 WITH FORM "COMC0012"
   DISPLAY "DETALLE DE PAGOS" AT 1,28

   LET cla_sel = "SELECT b.n_seguro,",
                        "a.fentcons,",
                        "b.n_rfc,",
                        "a.num_sm,",
                        "a.tipo_pago,",
                        "a.monto_comision,",
                        "a.cod_esq_comision ",
                 "FROM   com_comis_detalle a, afi_mae_afiliado b ",
                 "WHERE  a.n_folio       = b.n_folio ",
                 "AND    a.codven        = ","'",g_reg1.codven,"'",
                 "AND    a.fecha_corte = ","'",vfecha_corte,"'" CLIPPED

   PREPARE claexe2 FROM cla_sel
   DECLARE cursor_2 CURSOR FOR claexe2

   LET pos = 1

   LET total_comision = 0

   FOREACH cursor_2 INTO g_reg3[pos].*

      LET total_comision = total_comision + g_reg3[pos].monto_comision

      IF g_reg3[pos].tipo_pago = 0 THEN
         LET g_reg3[pos].tipo_pago = "AF"
      END IF

      IF g_reg3[pos].tipo_pago = 1 THEN
         LET g_reg3[pos].tipo_pago = "CO"
      END IF

      LET pos = pos + 1
      IF pos > 1000 THEN
         ERROR "Sobrepaso la capacidad del arreglo"
         EXIT FOREACH
      END IF
   END FOREACH
                 
   ERROR ""

   CALL SET_COUNT(pos-1)

   SELECT vigen_complemento
   INTO   vigencia
   FROM   com_tipo_promotor a, pro_mae_promotor b
   WHERE  cod_tipo_prom = nivel
   AND    codven        = g_reg1.codven

   LET cla_sel = "SELECT periodo ",
                 "FROM   com_quincena ",
                 "WHERE  fecha_sol_proceso BETWEEN ","'",vfecha_corte,"'",
                 "AND    ","'",vfecha_corte,"'"

   PREPARE claexe3 FROM cla_sel
   DECLARE cursor_3 CURSOR FOR claexe3
   FOREACH cursor_3 INTO periodo
   END FOREACH

   IF (pos-1) >= 1 THEN
      DISPLAY BY NAME periodo,vigencia,total_comision
      DISPLAY ARRAY g_reg3 TO scr_2.*
         ON KEY (CONTROL-M)
            LET i = ARR_CURR()
            CALL Complemento_detalle(g_reg3[i].n_seguro,
                                     vfecha_corte,
                                     g_reg3[i].fentcons,
                                     g_reg3[i].cod_esq_comision)
         ON KEY (INTERRUPT)
            EXIT DISPLAY
      END DISPLAY
   ELSE
      ERROR "NO EXISTE DETALLE DE PAGOS"
      SLEEP 1
      ERROR ""
   END IF
   CLOSE WINDOW ventana_2
END FUNCTION

FUNCTION Complemento_detalle(nss,vfecha_corte,vfentcons,vcod_esq_comision)
   DEFINE nss               CHAR(11),
          vfecha_corte      DATE,
          vfentcons         DATE,
          vcod_esq_comision SMALLINT

   OPEN WINDOW ventana_3 AT 8,2 WITH FORM "COMC0013"
   DISPLAY "PAGO COMPLEMENTO" AT 1,28

   SELECT fecha_elaboracion
   INTO   g_reg4.fecha_elaboracion
   FROM   afi_mae_afiliado
   WHERE  n_seguro = nss

   LET g_reg4.n_seguro = nss
   LET g_reg4.fentcons = vfentcons

   DISPLAY BY NAME g_reg4.n_seguro,g_reg4.fecha_elaboracion,g_reg4.fentcons   

   LET cla_sel = "SELECT a.salario_base_comis,",
                        "a.num_sm,",
                        "a.monto_comision,",
                        "a.fecha_corte ",
                 "FROM   com_comis_detalle a,afi_mae_afiliado b ",
                 "WHERE  a.n_folio = b.n_folio ",
                 " AND    a.codven      = ","'",g_reg1.codven,"'",
                 " AND    b.n_seguro    = ","'",nss,"'"
          
   PREPARE claexe5 FROM cla_sel
   DECLARE cursor_5 CURSOR FOR claexe5

   LET pos = 1

   FOREACH cursor_5 INTO g_reg5[pos].salario,
                         g_reg5[pos].num_sm,
                         g_reg5[pos].comision,
                         g_reg5[pos].fecha_corte

      SELECT rango_desde,rango_hasta
      INTO   g_reg5[pos].desde,g_reg5[pos].hasta
      FROM   com_cuadro_comis
      WHERE  cod_esq_comision = vcod_esq_comision
      AND    rango_desde     <= g_reg5[pos].num_sm
      AND    rango_hasta     >= g_reg5[pos].num_sm

      SELECT monto_sm
      INTO   g_reg5[pos].sm
      FROM   tab_salario_minimo a
      WHERE  (a.fecha_hasta_sm >= g_reg4.fentcons
      AND     a.fecha_desde_sm <= g_reg4.fentcons)
      OR     (a.fecha_desde_sm <= g_reg4.fentcons
      AND     a.fecha_hasta_sm IS NULL)

      LET cla_sel = "SELECT periodo ",
                    "FROM   com_quincena ",
                    "WHERE  fecha_sol_proceso BETWEEN ","'",g_reg5[pos].fecha_corte,"'",
                    "AND    ","'",g_reg5[pos].fecha_corte,"'"

      PREPARE claexe6 FROM cla_sel
      DECLARE cursor_6 CURSOR FOR claexe6
      FOREACH cursor_6 INTO g_reg5[pos].periodo
      END FOREACH
      
      LET pos = pos + 1
      IF pos > 1000 THEN
         ERROR "Sobrepaso la capacidad del arreglo"
         EXIT FOREACH
      END IF
   END FOREACH
                 
   ERROR ""

   CALL SET_COUNT(pos-1)

   IF (pos-1) >= 1 THEN
      DISPLAY ARRAY g_reg5 TO scr_2.*
         ON KEY (INTERRUPT)
            EXIT DISPLAY
      END DISPLAY
   ELSE
      ERROR "NO EXISTE DETALLE DE COMPLEMENTO"
      SLEEP 1
      ERROR ""
   END IF
   CLOSE WINDOW ventana_3
END FUNCTION
