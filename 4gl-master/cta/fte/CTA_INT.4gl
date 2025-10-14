# -------------------------------------------------------------------

# Programa : EXC_INT.4gl
# Objeto
# Modulo   : Interfases
# Realizo  : Patricia Martinez Sierra
# Fecha    : 9 de Octubre de 2000
# Modificado  : MIGUEL ANGEL HERNANDEZ MARTINEZ
# Fecha    : 25 de julio de 2001
# --------------------------------------------------------------------
DATABASE safre_af
GLOBALS
   DEFINE p_tabcampo RECORD LIKE tab_campo.*
END GLOBALS 
# ---------------------------------------------------------------------
# obtiene fecha de fin de mes
# ---------------------------------------------------------------------
FUNCTION fin_mes(fecha)
   DEFINE mes          SMALLINT,
          fecha        DATE,
          bisiesto,
          ano,
          v_ban,
          ult_dia      SMALLINT,
          v_fecha      CHAR(12),
          v_ano,
          v_mes ,
          v_ult_dia    CHAR(5)

   LET v_ban = FALSE
   LET ano = YEAR(fecha)
   LET bisiesto =  ano MOD 4

   IF bisiesto = 0 THEN
      LET v_ban = TRUE
   END IF

   LET mes = MONTH(fecha)

   IF mes = 4 OR mes = 6 OR mes = 9 OR mes = 11 THEN
      LET ult_dia = 30
   ELSE
      IF mes = 2 THEN
         IF v_ban THEN
            LET ult_dia = 29
         ELSE
            LET ult_dia = 28
         END IF
      ELSE
         LET ult_dia = 31
      END IF
   END IF

   LET v_mes  = mes
   LET v_ano = ano
   LET v_ult_dia = ult_dia
   LET v_fecha = v_mes CLIPPED,"/",v_ult_dia CLIPPED, "/", v_ano 
   LET fecha =v_fecha

   RETURN fecha,ult_dia 

END FUNCTION
# ----------------------------------------------------------------------
# fin de mes mas n dias habiles
# ----------------------------------------------------------------------
FUNCTION mes_h(fecha,ndias)
   DEFINE fecha       DATE,
          fechafin    DATE,
          ndias       INTEGER,
          dias        INTEGER,
          dia_inhabil INTEGER,
          diaSemana   INTEGER,
          feriado     INTEGER,
          finSemana   INTEGER

   LET fechafin = fecha

   WHILE TRUE
      LET feriado   = 0
      LET finSemana = 0
      LET diaSemana = WEEKDAY(fechafin)

      IF diaSemana = 0 OR diaSemana = 6 THEN
         LET finSemana = 1
      END IF

      SELECT *
      FROM   tab_feriado
      WHERE  feria_fecha = fechafin

      IF STATUS <> NOTFOUND THEN
         LET feriado = 1
      END IF

      IF finSemana = 1 OR feriado = 1 THEN
         LET dia_inhabil = dia_inhabil + 1
      ELSE
         LET dias = dias + 1
      END IF

      IF dias = ndias THEN
         EXIT WHILE
      END IF

      LET fechafin = fechafin + 1 UNITS DAY
   END WHILE

   RETURN fechafin

END FUNCTION
# ------------------------------------------------------------------
# se genera el reporte 
# ------------------------------------------------------------------
REPORT r_report(v_rep,v_codigo,v_reg)

   DEFINE v_rep         CHAR(3000),   #registro a tratar
          v_reg         SMALLINT,    #tipo_reg (1,2,3 etc) detalle,cza etc.. 
          v_codigo      SMALLINT,    #codigo de layout
          v_campo       CHAR(100),
          v_long,
          blanco        SMALLINT,
          i,
          e,
          j             SMALLINT

   OUTPUT
      PAGE LENGTH 1
      LEFT MARGIN 0
      RIGHT MARGIN 0
      TOP MARGIN 0
      BOTTOM MARGIN 0

   FORMAT

   ON EVERY ROW
      #cabezera
      DECLARE cur_mae CURSOR FOR
      SELECT *  
      FROM   tab_campo
      WHERE  layout_cod = v_codigo
      AND    tipo_reg = v_reg
      ORDER BY 3

      FOREACH cur_mae INTO p_tabcampo.*
         LET blanco = FALSE

         IF p_tabcampo.valor_cte IS NOT NULL THEN
            PRINT COLUMN p_tabcampo.pos_ini, p_tabcampo.valor_cte CLIPPED ;
            CONTINUE FOREACH
         END IF

         IF p_tabcampo.tipo_dato = "F" THEN
            PRINT COLUMN p_tabcampo.pos_ini,p_tabcampo.long SPACES ;
            CONTINUE FOREACH
         END IF

         LET v_long = LENGTH(v_rep CLIPPED) + 1
         LET i = 1
         LET e = 0
         LET v_campo = ""

         IF p_tabcampo.campo_desc = "nom_trabajador" THEN
            LET e = p_tabcampo.long
            LET v_campo = v_campo CLIPPED, v_rep[i,e]
            LET v_rep = v_rep [e + 2,v_long]
         ELSE
            FOR j = 1 to v_long
               IF v_rep[j,i] = "|" THEN
                  LET v_rep = v_rep [i + 1,v_long]
                  LET blanco = FALSE
                  EXIT FOR 
               ELSE
                  IF v_rep[j,i] = " " THEN
                     LET blanco = TRUE
                  ELSE 
                     IF blanco THEN
                        LET blanco =  FALSE
                        IF p_tabcampo.tipo_dato <> "#" THEN
                           LET v_campo = v_campo CLIPPED, " ", v_rep[j,i]
                        ELSE
                           LET v_campo = v_campo CLIPPED, v_rep[j,i]
                        END IF
                     ELSE
                        LET v_campo = v_campo CLIPPED, v_rep[j,i]
                     END IF
                  END IF
               END IF
               LET i = i + 1
            END FOR
         END IF

         CALL formatea_campo(v_campo,
                             p_tabcampo.tipo_dato,
                             p_tabcampo.long,
                             p_tabcampo.decimal)
              RETURNING v_campo

         PRINT COLUMN p_tabcampo.pos_ini,v_campo CLIPPED ;
      END FOREACH

      PRINT

END REPORT
# --------------------------------------------------------------------
# formatea cualquier tipo de campo
# --------------------------------------------------------------------
FUNCTION formatea_campo(v_campo,v_tipo_dato,v_long,v_decimal)
   DEFINE v_campo,
          v_campo1       CHAR(400),
          v_tipo_dato    CHAR(1),
          v_campo_aux    CHAR(400),
          i,
          j,
          v_long,
          v_decimal      SMALLINT

   CASE v_tipo_dato
      WHEN "#"
         IF v_decimal = 0 OR v_decimal IS NULL THEN
            CALL formatea_numero(v_campo,v_long)
                 RETURNING v_campo_aux
         ELSE
            FOR i=1 TO v_long
               IF v_campo[i,i] ="." THEN
                  LET v_campo1 = v_campo[1,i-1 ], v_campo[i+1,v_long + 1]
                  EXIT FOR
               ELSE
                  LET v_campo1 = v_campo1 CLIPPED, v_campo[i,i]
               END IF
            END FOR

            LET v_campo_aux =   v_campo1

            CALL formatea_numero(v_campo_aux,v_long)
                 RETURNING v_campo_aux
         END IF

      WHEN "D"
         IF v_long = 6 THEN
            LET v_campo_aux  = v_campo[9,10] CLIPPED, v_campo[1,2] CLIPPED, v_campo[4,5]
         ELSE
            LET v_campo_aux  = v_campo[7,10] CLIPPED, v_campo[1,2] CLIPPED, v_campo[4,5]
         END IF

      WHEN "A"
         LET v_campo_aux = v_campo[1,v_long]

      WHEN "X"
         LET v_campo_aux = v_campo[1,v_long]

      WHEN "Z"
         CALL derec_texto(v_campo,v_long)
              RETURNING v_campo_aux
   END CASE

   RETURN v_campo_aux
END FUNCTION
# ---------------------------------------------------------------------
# formatea numero
# ----------------------------------------------------------------------
FUNCTION formatea_numero(l_valor,l_ltotal)
   DEFINE l_valor      CHAR(30),     # Valor a formatear
          l_ltotal     SMALLINT,     # Longitud total del formato
          l_texto      CHAR(30)

   IF l_valor IS NULL THEN
      LET l_valor = 0
   END IF

   CASE l_ltotal
      WHEN 1
         LET l_texto = l_valor USING "&"
      WHEN 2
         LET l_texto = l_valor USING "&&"
      WHEN 3
         LET l_texto = l_valor USING "&&&"
      WHEN 4
         LET l_texto = l_valor USING "&&&&"
      WHEN 5
         LET l_texto = l_valor USING "&&&&&"
      WHEN 6
         LET l_texto = l_valor USING "&&&&&&"
      WHEN 7
         LET l_texto = l_valor USING "&&&&&&&"
      WHEN 8
         LET l_texto = l_valor USING "&&&&&&&&"
      WHEN 9
         LET l_texto = l_valor USING "&&&&&&&&&"
      WHEN 10
         LET l_texto = l_valor USING "&&&&&&&&&&"
      WHEN 11
         LET l_texto = l_valor USING "&&&&&&&&&&&"
      WHEN 12
         LET l_texto = l_valor USING "&&&&&&&&&&&&"
      WHEN 13
         LET l_texto = l_valor USING "&&&&&&&&&&&&&"
      WHEN 14
         LET l_texto = l_valor USING "&&&&&&&&&&&&&&"
      WHEN 15
         LET l_texto = l_valor USING "&&&&&&&&&&&&&&&"
      WHEN 16
         LET l_texto = l_valor USING "&&&&&&&&&&&&&&&&"
      WHEN 17
         LET l_texto = l_valor USING "&&&&&&&&&&&&&&&&&"
      WHEN 18
         LET l_texto = l_valor USING "&&&&&&&&&&&&&&&&&&"
      WHEN 19
         LET l_texto = l_valor USING "&&&&&&&&&&&&&&&&&&&"
      WHEN 20
         LET l_texto = l_valor USING "&&&&&&&&&&&&&&&&&&&&"
      WHEN 21
         LET l_texto = l_valor USING "&&&&&&&&&&&&&&&&&&&&&"
      WHEN 22
         LET l_texto = l_valor USING "&&&&&&&&&&&&&&&&&&&&&&"
      WHEN 23
         LET l_texto = l_valor USING "&&&&&&&&&&&&&&&&&&&&&&&"
      WHEN 24
         LET l_texto = l_valor USING "&&&&&&&&&&&&&&&&&&&&&&&&"
      WHEN 25
         LET l_texto = l_valor USING "&&&&&&&&&&&&&&&&&&&&&&&&&"
      WHEN 26
         LET l_texto = l_valor USING "&&&&&&&&&&&&&&&&&&&&&&&&&&"
      WHEN 27
         LET l_texto = l_valor USING "&&&&&&&&&&&&&&&&&&&&&&&&&&&"
      WHEN 28
         LET l_texto = l_valor USING "&&&&&&&&&&&&&&&&&&&&&&&&&&&&"
      WHEN 29
         LET l_texto = l_valor USING "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&"
      WHEN 30
         LET l_texto = l_valor USING "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&"
   END CASE

   RETURN l_texto
END FUNCTION
# ---------------------------------------------------------------------
# valida numeros
# ---------------------------------------------------------------------
FUNCTION es_numerico(cadena,long)
   DEFINE i,
          long SMALLINT,
          cadena CHAR(100),
          v_num  CHAR(1)

   FOR i = 1 to long
      LET v_num = cadena[i,i]

      IF v_num NOT MATCHES "[0-9]" THEN
         RETURN TRUE
      END IF
   END FOR

   RETURN FALSE
END FUNCTION
# ----------------------------------------------------------------------
# Funcion para alinear un string por la izquierda.
#
# NOTA:
#
#      Valor String        Valor Formateado
#      [         z]  ----->  [z         ]
# ----------------------------------------------------------------------
FUNCTION izqui_texto(v_campo,v_long)

   DEFINE v_campo,
          vstring1    CHAR(100),
          v_long,
          ct1,
          ct2         SMALLINT

   FOR ct2 = 1 TO v_long
      IF v_campo[ct2,ct2] = " " THEN
         CONTINUE FOR
      ELSE
         LET vstring1 = v_campo[ct2,v_long]
         EXIT FOR
      END IF
   END FOR

   RETURN vstring1
END FUNCTION
# ------------------------------------------------------
# ajustar texto a la derecha
# ------------------------------------------------------
FUNCTION derec_texto(v_campo,v_long)

   DEFINE v_campo,
          tex2       CHAR(100),
          v_long,
          i,
          j,
          v_ban      SMALLINT

   LET tex2 = " "
   LET i = 1
   LET v_ban = TRUE
   LET j = 100

   IF v_campo is NOT NULL THEN
      FOR i = v_long TO 1 step -1
         IF v_campo[i,i] = " " AND v_ban = TRUE THEN
            CONTINUE FOR
         END IF

         LET v_ban = FALSE
         LET tex2[j,j] = v_campo[i,i]
         LET j = j - 1
      END FOR

      LET i = (100 - v_long) + 1
   END IF

   RETURN tex2[i,100]
END FUNCTION
# ------------------------------------------------------------------
# se genera el reporte 
# ------------------------------------------------------------------
REPORT r_report_db(v_arch,v_codigo,v_reg,v_tabla)

   DEFINE v_arch,
          v_tabla,
          v_campo       CHAR(110),
          v_reg,
          v_codigo      SMALLINT,
          v_ban,
          i,
          j             SMALLINT

   OUTPUT
      PAGE LENGTH 1
      LEFT MARGIN 0
      RIGHT MARGIN 0
      TOP MARGIN 0
      BOTTOM MARGIN 0

   FORMAT

   ON EVERY ROW
      PRINT COLUMN 1,"FILE ", "\"",v_arch CLIPPED , "\"  ("

      DECLARE cur_db CURSOR FOR
      SELECT *
      FROM   tab_campo
      WHERE  layout_cod = v_codigo
      AND    tipo_reg = v_reg
      ORDER BY 3

      LET v_ban = FALSE

      FOREACH cur_db INTO p_tabcampo.*
         IF v_ban THEN
            PRINT COLUMN 32, ","
         END IF

         PRINT COLUMN 1,p_tabcampo.campo_desc CLIPPED;
         PRINT COLUMN 20,p_tabcampo.pos_ini CLIPPED,
                         "-" CLIPPED,
                         p_tabcampo.pos_fin USING "&&&&" CLIPPED ;
         LET v_ban = TRUE
      END FOREACH

      PRINT COLUMN 32 , ");";
      PRINT
      PRINT
      PRINT COLUMN 1,"INSERT INTO  ", v_tabla  CLIPPED, ";"

END REPORT
# -------------------------------------------------------------
# valida campo
# -------------------------------------------------------------
FUNCTION valida_campo(tipo_campo,campo,long)

   DEFINE tipo_campo    CHAR(1),
          campo         CHAR(20),     #el valor del campo 
          long          SMALLINT ,    #la longitud del campo
          v_ban,
          i,
          v_tt          SMALLINT

   CASE tipo_campo
      WHEN "#"
         FOR i = 1 TO long
            IF campo[i,i] NOT MATCHES "[0-9]" THEN
               RETURN FALSE
            END IF
         END FOR

      WHEN "D"
         CALL valida_fecha(campo)
              RETURNING v_tt

      WHEN "A"
         FOR i = 1 TO long
            IF campo[i,i] NOT MATCHES "[a-z]" THEN
               IF campo[i,i] NOT MATCHES "[A-Z]" THEN
                  IF campo[i,i] != " " THEN
                     RETURN FALSE
                  END IF
               END IF
            END IF
         END FOR

         LET v_tt = TRUE

      WHEN "X"
         FOR i = 1 TO long
            IF campo[i,i] NOT MATCHES "[a-z]" THEN
               IF campo[i,i] NOT MATCHES "[A-Z]" THEN
                  IF campo[i,i] != " " THEN
                     IF campo[i,i] NOT MATCHES "[0-9]" THEN
                        RETURN FALSE
                     END IF
                  END IF
               END IF
            END IF
         END FOR

         ----- WHEN "F"
   END  CASE

   RETURN TRUE
END FUNCTION
# -----------------------------------------------------
#  valida que la fecha sea  correcta
# -----------------------------------------------------
FUNCTION valida_fecha(v_fecha)    # mm/dd/aaaa

   DEFINE v_fecha    CHAR(10),
          bisiesto,
          ult_dia,
          v_ban,
          v_ano,
          v_mes,
          v_dia      SMALLINT

   IF v_fecha IS NULL OR v_fecha = " " THEN
      RETURN FALSE
   END IF

   LET v_ano = v_fecha[7,10] 
   LET v_mes = v_fecha[1,2]
   LET v_dia = v_fecha[4,5]

   IF v_ano < 1900 THEN
      RETURN FALSE
   END IF

   IF v_mes < 1 OR v_mes > 12 THEN
      RETURN FALSE
   END IF

   IF v_dia < 1 OR v_dia > 31 THEN
      RETURN FALSE
   END IF

   LET bisiesto = v_ano MOD 4

   IF bisiesto  = 0 THEN
      LET v_ban =  TRUE
   END IF

   IF v_mes = 2 THEN
      IF v_ban THEN
         IF v_dia <= 29 THEN
            RETURN TRUE
         ELSE
            RETURN FALSE
         END IF
      ELSE
         IF v_dia <= 28 THEN
            RETURN TRUE
         ELSE
            RETURN FALSE
         END IF
      END IF
   END IF

   IF v_mes = 6 OR v_mes = 9 OR v_mes = 11 OR v_mes = 4 THEN
      IF v_dia <= 30 THEN
         RETURN TRUE
      ELSE
         RETURN FALSE
      END IF
   END IF

   RETURN TRUE

END FUNCTION
