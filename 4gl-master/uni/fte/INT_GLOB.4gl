#
# -------------------------------------------------------------------
# Programa : INT_GLOB.4gl
# Modulo   : Interfases
# Realizo  : Patricia Martinez Sierra
# Fecha    : 9 de Octubre de 2000
# --------------------------------------------------------------------
#
DATABASE safre_af 
  GLOBALS
     DEFINE p_tabcampo  RECORD LIKE tab_campo.*,
            p_maepromo  RECORD LIKE pro_mae_promotor.*
  END GLOBALS 
#
# ---------------------------------------------------------------------
# obtiene fecha de fin de mes
# ---------------------------------------------------------------------
#
FUNCTION fin_mes(fecha)
      DEFINE mes, bisiesto, ano, 
             v_ban, ult_dia           SMALLINT,
             fecha                    DATE, 
             v_fecha                  CHAR(12),
             v_ano, v_mes , v_ult_dia CHAR(5)


     LET v_ban = FALSE
     LET ano = YEAR(fecha)
     LET bisiesto =  ano MOD 4

     IF bisiesto  = 0 THEN
        LET v_ban =  TRUE
     END IF 

     LET mes = MONTH(fecha)

     IF mes = 4  OR  mes = 6  OR 
        mes = 9  OR  mes = 11 THEN
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

     LET v_mes     = mes
     LET v_ano     = ano
     LET v_ult_dia = ult_dia
     LET v_fecha   = v_mes CLIPPED,"/",v_ult_dia CLIPPED, "/", v_ano 
     LET fecha     = v_fecha

     RETURN fecha , ult_dia 

END FUNCTION
#
# ----------------------------------------------------------------------
# fin de mes mas n dias habiles
# ----------------------------------------------------------------------
#
FUNCTION mes_h(fecha,ndias)
   DEFINE fecha,       fechafin      DATE,
          ndias,       dias,
          dia_inhabil, diaSemana,
          feriado,     finSemana     INTEGER

   LET fechafin = fecha

   WHILE TRUE
       LET feriado   = 0
       LET finSemana = 0
       LET diaSemana = WEEKDAY(fechafin)

       IF diaSemana = 0 OR diaSemana = 6 THEN
          LET finSemana = 1
       END IF

       SELECT a.* FROM tab_feriado a
              WHERE a.feria_fecha = fechafin

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

   RETURN  fechafin

END FUNCTION
#
# ------------------------------------------------------------------
# se genera el reporte 
# ------------------------------------------------------------------
#
REPORT r_report(v_rep, v_codigo, v_reg )
DEFINE v_rep          CHAR(3000),    #registro a tratar
       v_reg          SMALLINT ,     #tipo_reg (1,2,3 etc) detalle,cza etc.. 
       v_codigo       SMALLINT,      #codigo de layout
       v_campo        CHAR(700),
       v_long, blanco SMALLINT, 
       i,j            SMALLINT,
       enter char(1)

 OUTPUT
    PAGE   LENGTH 1
    LEFT   MARGIN 0
    RIGHT  MARGIN 0
    TOP    MARGIN 0
    BOTTOM MARGIN 0

 FORMAT
    ON EVERY ROW
       #cabezera
       DECLARE cur_mae CURSOR FOR
               SELECT g.*  FROM  tab_campo g
                      WHERE g.layout_cod = v_codigo
                        AND g.tipo_reg   = v_reg
               ORDER BY 3
       FOREACH cur_mae INTO p_tabcampo.*
          LET blanco = FALSE

          IF p_tabcampo.valor_cte IS NOT NULL THEN
              PRINT  COLUMN p_tabcampo.pos_ini, p_tabcampo.valor_cte CLIPPED ;
              CONTINUE FOREACH
          END IF

          IF p_tabcampo.tipo_dato = "F"  THEN
              PRINT  COLUMN p_tabcampo.pos_ini, p_tabcampo.long SPACES ;
              CONTINUE FOREACH
          END IF

          LET v_long =  LENGTH(v_rep CLIPPED) + 1
          LET i = 1
          LET v_campo = ""

          FOR j = 1 to v_long  
              IF v_rep[j,i]   = "|" THEN
                   LET v_rep  = v_rep [i + 1,v_long]
                   LET blanco = FALSE
                   EXIT FOR 
              ELSE
                   IF  v_rep[j,i] = " " THEN
                       LET blanco = TRUE 
                   ELSE 
                       IF blanco  THEN 
                          LET blanco =  FALSE
                          IF p_tabcampo.tipo_dato != "#" THEN
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


          CALL formatea_campo(v_campo,           p_tabcampo.tipo_dato,
                               p_tabcampo.long,  p_tabcampo.decimal)
                               RETURNING v_campo
          PRINT  COLUMN p_tabcampo.pos_ini, v_campo CLIPPED ;

       END FOREACH
       PRINT 

END REPORT
#
# --------------------------------------------------------------------
# formatea cualquier tipo de campo
# --------------------------------------------------------------------
#
FUNCTION formatea_campo(v_campo,v_tipo_dato, v_long , v_decimal)
DEFINE  
       v_campo, v_campo1       CHAR(700),
       v_tipo_dato             CHAR(1),
       v_campo_aux             CHAR(12),
       i,j, v_long ,v_decimal  SMALLINT

  CASE v_tipo_dato
          WHEN  "#"  
                IF v_decimal = 0 OR v_decimal IS  NULL THEN
                     CALL formatea_numero(v_campo, v_long) 
                                          RETURNING v_campo
                ELSE
                    FOR i=1 TO v_long
                        IF  v_campo[i,i] ="." THEN
                            LET j = LENGTH (v_campo1)
                            LET v_campo1 = v_campo[1,j], v_campo[j+2,v_long]
                            EXIT FOR
                        ELSE
                            LET v_campo1 = v_campo1 CLIPPED, v_campo[i,i]
                        END IF
                      
                    END FOR

                    LET v_campo = v_campo1
                    CALL formatea_numero(v_campo, v_long) 
                                         RETURNING v_campo
                     
                END IF

          WHEN  "D"  
               IF v_long = 6 THEN
                   LET v_campo  = v_campo[9,10] CLIPPED, 
                                  v_campo[1,2] CLIPPED, v_campo[4,5]
                ELSE
                   LET v_campo  = v_campo[7,10] CLIPPED, 
                                  v_campo[1,2] CLIPPED, v_campo[4,5]
               END IF

                    
          WHEN  "A"  LET v_campo = v_campo[1,v_long] 
          WHEN  "X"  LET v_campo = v_campo[1,v_long]
          WHEN  "Z"  CALL derec_texto(v_campo,v_long)   
                          RETURNING v_campo
  END CASE 
RETURN v_campo
END FUNCTION
#
# ---------------------------------------------------------------------
# formatea numero
# ----------------------------------------------------------------------
#
FUNCTION formatea_numero(l_valor,l_ltotal)

    DEFINE l_valor  CHAR(30),    # Valor a formatear
           l_ltotal SMALLINT,    # Longitud total del formato
           l_texto  CHAR(24)

    IF l_valor IS NULL THEN
       LET l_valor = 0
    END IF 

    CASE l_ltotal
         WHEN  1 LET l_texto = l_valor USING "&"
         WHEN  2 LET l_texto = l_valor USING "&&"
         WHEN  3 LET l_texto = l_valor USING "&&&"
         WHEN  4 LET l_texto = l_valor USING "&&&&"
         WHEN  5 LET l_texto = l_valor USING "&&&&&"
         WHEN  6 LET l_texto = l_valor USING "&&&&&&"
         WHEN  7 LET l_texto = l_valor USING "&&&&&&&"
         WHEN  8 LET l_texto = l_valor USING "&&&&&&&&"
         WHEN  9 LET l_texto = l_valor USING "&&&&&&&&&"
         WHEN 10 LET l_texto = l_valor USING "&&&&&&&&&&"
         WHEN 11 LET l_texto = l_valor USING "&&&&&&&&&&&"
         WHEN 12 LET l_texto = l_valor USING "&&&&&&&&&&&&"
         WHEN 13 LET l_texto = l_valor USING "&&&&&&&&&&&&&"
         WHEN 14 LET l_texto = l_valor USING "&&&&&&&&&&&&&&"
         WHEN 15 LET l_texto = l_valor USING "&&&&&&&&&&&&&&&"
         WHEN 16 LET l_texto = l_valor USING "&&&&&&&&&&&&&&&&"
         WHEN 17 LET l_texto = l_valor USING "&&&&&&&&&&&&&&&&&"
         WHEN 18 LET l_texto = l_valor USING "&&&&&&&&&&&&&&&&&&"
         WHEN 19 LET l_texto = l_valor USING "&&&&&&&&&&&&&&&&&&&"
         WHEN 20 LET l_texto = l_valor USING "&&&&&&&&&&&&&&&&&&&&"
         WHEN 21 LET l_texto = l_valor USING "&&&&&&&&&&&&&&&&&&&&&"
         WHEN 22 LET l_texto = l_valor USING "&&&&&&&&&&&&&&&&&&&&&&"
         WHEN 23 LET l_texto = l_valor USING "&&&&&&&&&&&&&&&&&&&&&&&"
         WHEN 24 LET l_texto = l_valor USING "&&&&&&&&&&&&&&&&&&&&&&&&"
         WHEN 25 LET l_texto = l_valor USING "&&&&&&&&&&&&&&&&&&&&&&&&&"
         WHEN 26 LET l_texto = l_valor USING "&&&&&&&&&&&&&&&&&&&&&&&&&&"
         WHEN 27 LET l_texto = l_valor USING "&&&&&&&&&&&&&&&&&&&&&&&&&&&"
         WHEN 28 LET l_texto = l_valor USING "&&&&&&&&&&&&&&&&&&&&&&&&&&&&"
         WHEN 29 LET l_texto = l_valor USING "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&"
         WHEN 30 LET l_texto = l_valor USING "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&"
    END CASE                                                        
RETURN l_texto
END FUNCTION
#
# ---------------------------------------------------------------------    
# valida numeros                                                           
# ---------------------------------------------------------------------    
#
FUNCTION es_numerico(cadena, long)
    DEFINE i, long   SMALLINT ,
           cadena    CHAR(100),
           v_num     CHAR(1)

    FOR i = 1 to long
       LET v_num = cadena[i,i]

       IF  v_num NOT MATCHES "[0-9]"  THEN
           RETURN TRUE
       END IF

    END FOR

RETURN FALSE
END FUNCTION
#
# ----------------------------------------------------------------------
# Funcion para alinear un string por la izquierda.
#
# NOTA:
#
#      Valor String        Valor Formateado
#      [         z]  ----->  [z         ]
# ----------------------------------------------------------------------
#
FUNCTION izqui_texto(v_campo,v_long)

   DEFINE v_campo,vstring1   CHAR(100),
          v_long, ct1,ct2    SMALLINT

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
#
#
# ------------------------------------------------------
# ajustar texto a la derecha
# ------------------------------------------------------
FUNCTION derec_texto(v_campo,v_long)
    DEFINE v_campo,tex2        CHAR(100),
           v_long, i,j,v_ban   SMALLINT

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
#
#
# ------------------------------------------------------------------
# se genera el reporte 
# ------------------------------------------------------------------
#
REPORT r_report_db(v_arch, v_codigo, v_reg, v_tabla )
    DEFINE  v_arch, v_tabla,  v_campo     CHAR(110),
            v_reg,  v_codigo, v_ban,  i,j SMALLINT

     OUTPUT
        PAGE   LENGTH 1
        LEFT   MARGIN 0
        RIGHT  MARGIN 0
        TOP    MARGIN 0
        BOTTOM MARGIN 0

     FORMAT
        ON EVERY ROW

           PRINT COLUMN 1, "FILE ", "\"",v_arch CLIPPED , "\"  ("

           DECLARE cur_db CURSOR FOR
                   SELECT a.*  FROM  tab_campo a
                         WHERE a.layout_cod = v_codigo
                           AND a.tipo_reg   = v_reg
                   ORDER BY 3
                   LET v_ban =  false
           FOREACH cur_db INTO p_tabcampo.*
                     IF v_ban  THEN
                         PRINT COLUMN 32, ","
                     END IF 
                      
                     PRINT  COLUMN 1,  p_tabcampo.campo_desc CLIPPED;
                     PRINT  COLUMN 20, p_tabcampo.pos_ini CLIPPED,
                                       "-" CLIPPED, 
                                     p_tabcampo.pos_fin using "&&&&" CLIPPED ;
                    LET v_ban = TRUE
           END FOREACH

           PRINT COLUMN 32 , ");";
           PRINT 
           PRINT 
           PRINT COLUMN 1,"INSERT INTO  ", v_tabla  CLIPPED, ";"   

END REPORT
#
# -------------------------------------------------------------
# valida campo
# -------------------------------------------------------------
#
FUNCTION valida_campo(tipo_campo, campo, long)
DEFINE
      tipo_campo CHAR(1),
      campo      CHAR(20),     #el valor del campo 
      long,                    # la longitud del campo
      v_ban,
      i, 
      v_tt       SMALLINT
      

      CASE tipo_campo
          WHEN "#" 
                   FOR i = 1 TO long
                      IF campo[i,i] NOT MATCHES "[0-9]" THEN
                         RETURN FALSE   
                      END IF
                   END FOR
                   
          WHEN "D"
                   CALL valida_fecha(campo) RETURNING v_tt

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
      END  CASE

      RETURN TRUE
END FUNCTION
#
# -----------------------------------------------------
#  valida que la fecha sea  correcta
# -----------------------------------------------------
#
FUNCTION valida_fecha(v_fecha)
# mm/dd/aaaa

     DEFINE v_fecha                     CHAR(10),
            bisiesto, ult_dia,v_ban,
            v_ano , v_mes, v_dia        SMALLINT

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

     IF v_mes = 6  OR  v_mes = 9  OR 
        v_mes = 11 OR  v_mes = 4  THEN
         IF v_dia <= 30 THEN
            RETURN TRUE
         ELSE
            RETURN FALSE
         END IF
     END IF

     RETURN TRUE

END FUNCTION 
#
FUNCTION marca_rechazo(reg_4,largo)
#mr--------------------------------
    DEFINE reg_4 RECORD #loc #reg_4
               posicion        SMALLINT  ,
               cod_rechazo     CHAR(003) ,
               motivo_rechazo  CHAR(100)
           END RECORD
  
    DEFINE #loc #smallint
        largo                 ,
        pos_fin_parcial       ,
        pos_ini_parcial       SMALLINT


    CASE reg_4.posicion 
        WHEN 1
            LET reg_4.motivo_rechazo = reg_4.cod_rechazo ,
                                       reg_4.motivo_rechazo[4,largo]
        WHEN 33
            LET pos_fin_parcial      = (reg_4.posicion-2) * 3

            LET reg_4.motivo_rechazo = reg_4.motivo_rechazo[01,pos_fin_parcial],
                                       reg_4.cod_rechazo

        OTHERWISE
            LET pos_fin_parcial = (reg_4.posicion-2) * 3
            LET pos_ini_parcial =  reg_4.posicion*3+2

            LET reg_4.motivo_rechazo=reg_4.motivo_rechazo[01,pos_fin_parcial],
                                     reg_4.cod_rechazo ,
                                     reg_4.motivo_rechazo[pos_ini_parcial,largo]
    END CASE
    RETURN reg_4.motivo_rechazo
END FUNCTION
#
# ---------------------------------------------------------
#  fecha con formato de mes 
# ---------------------------------------------------------
#
FUNCTION fecha_mes()
    DEFINE v_fecha      CHAR(10),
           v_fecha_fin  CHAR(11),
           v_mes        SMALLINT,
           nom_mes      CHAR (3)

     LET v_mes = v_fecha[1,2]

     CASE v_mes
         WHEN 1  LET nom_mes = "ENE"
         WHEN 2  LET nom_mes = "FEB"
         WHEN 3  LET nom_mes = "MAR"
         WHEN 4  LET nom_mes = "ABR"
         WHEN 5  LET nom_mes = "MAY"
         WHEN 6  LET nom_mes = "JUN"
         WHEN 7  LET nom_mes = "JUL"
         WHEN 8  LET nom_mes = "AGO"
         WHEN 9  LET nom_mes = "SEP"
         WHEN 10 LET nom_mes = "OCT"
         WHEN 11 LET nom_mes = "NOV"
         WHEN 12 LET nom_mes = "DIC"
     END CASE

     LET v_fecha_fin = v_fecha[4,5], nom_mes, v_fecha[7,10]

#formato dd/mmm/aaaa
     RETURN v_fecha_fin 
END FUNCTION
#
# -------------------------------------------------------
# obtiene bimestre
# --------------------------------------------------------
#
FUNCTION bimestre(v_fec_bim)
   DEFINE v_fec_bim, 
          v_bim_ini,
          v_bim_fin        CHAR(10),
          v_mes,dia ,
          v_ano, mes_ini,
          mes_fin          SMALLINT, 
          v_x              CHAR(4),
          v_mx             CHAR(2)

   LET v_x    = v_fec_bim[1,2] 
   LET v_mes  = v_x
   LET v_x    = v_fec_bim[7,10]
   LET v_ano  = v_x

   CASE v_mes
       WHEN 1   LET v_ano =  v_ano - 1
                LET mes_ini =  10
                LET mes_fin =  12 

       WHEN 2   LET v_ano =  v_ano - 1
                LET mes_ini = 10 
                LET mes_fin = 12 
       OTHERWISE
                IF v_mes = 3  OR  v_mes = 5  OR 
                   v_mes = 7  OR  v_mes = 9  OR
                   v_mes = 11 THEN
                     LET mes_ini = v_mes - 2
                     LET mes_fin = v_mes - 1
               ELSE 
                    IF  v_mes = 4  OR  v_mes = 6  OR
                        v_mes = 8  OR  v_mes = 10 OR 
                        v_mes = 12 THEN
                          LET mes_ini = v_mes - 3
                          LET mes_fin = v_mes - 2
                    END IF
               END IF 
    END CASE 

    LET v_mx       =  mes_ini          
    LET v_bim_ini  =  v_mx CLIPPED,"/01/" CLIPPED,  v_ano
    LET v_mx       =  mes_fin          
    LET v_bim_fin  =  v_mx CLIPPED, "/01/" CLIPPED, v_ano

    CALL fin_mes(v_bim_fin) RETURNING v_bim_fin, dia

    RETURN v_bim_ini, v_bim_fin

END FUNCTION
#
# ---------------------------------------------------------
# valida nss 
# ---------------------------------------------------------
#
FUNCTION valida_nss(num_nss)

     DEFINE num_nss        CHAR(11),
            v_ban, digito  SMALLINT,
            v_status       LIKE afi_mae_afiliado.status

     IF num_nss = "47998226634"  THEN
        ERROR "nss = 47998226634 "
     END IF 

     CALL valida_campo(num_nss, "#", 11) RETURNING v_ban

     IF v_ban = FALSE THEN
         RETURN FALSE
     END IF

     IF LENGTH(num_nss) != 11 THEN
         RETURN FALSE
     END IF 

     CALL  digito_verif(num_nss[1,10],10) RETURNING digito
     IF digito = 32000 THEN
        RETURN FALSE
     END IF

     IF LENGTH(num_nss) = 11 AND
        digito <> num_nss[11] THEN
        RETURN FALSE
     END IF

     SELECT  "a.X"  FROM afi_mae_afiliado a
             WHERE a.n_seguro = num_nss

     IF STATUS <> NOTFOUND THEN
        RETURN FALSE
     END IF

    --- adecuacion mmc 250601

     SELECT 'a.X'  FROM  afi_solicitud a
            WHERE  a.n_seguro = num_nss
              AND  a.status_interno <> 40
     GROUP BY 1

     IF ( STATUS = 0 ) THEN
         RETURN FALSE
     END IF

     RETURN  TRUE

END FUNCTION
#
# ---------------------------------------------------------
# valida promotores
# ---------------------------------------------------------
#
FUNCTION valida_promotor(clave)
      DEFINE clave  LIKE pro_mae_promotor.codven

      SELECT a.* INTO  p_maepromo.* FROM pro_mae_promotor a
             WHERE a.cod_promotor = clave

      IF STATUS <> NOTFOUND THEN  
          IF p_maepromo.status != 1 THEN
                RETURN FALSE
          ELSE
                   RETURN TRUE
          END IF
      ELSE
          RETURN FALSE
      END IF
      
END FUNCTION
#
# ---------------------------------------------------------
# Digito Verificador
# ---------------------------------------------------------
#
FUNCTION digito_verif(valor,longitud )
  DEFINE cadena          CHAR(20),
         valor           CHAR(10),
         longitud        SMALLINT,
         suma            SMALLINT,
         sumachar        CHAR(2),
         digito          SMALLINT,
         i,j             SMALLINT,
         temp            CHAR(2),
         ultima          SMALLINT,
         t               SMALLINT,
         x     ARRAY[10] OF CHAR(1)

       LET x[1]  = valor[1]
       LET x[2]  = valor[2]
       LET x[3]  = valor[3]
       LET x[4]  = valor[4]
       LET x[5]  = valor[5]
       LET x[6]  = valor[6]
       LET x[7]  = valor[7]
       LET x[8]  = valor[8]
       LET x[9]  = valor[9]
       LET x[10] = valor[10]

       FOR t = 1 TO 10
           IF x[t] <> "0" AND x[t] <> "1" AND
              x[t] <> "2" AND x[t] <> "3" AND
              x[t] <> "4" AND x[t] <> "5" AND
              x[t] <> "6" AND x[t] <> "7" AND
              x[t] <> "8" AND x[t] <> "9" THEN
               LET digito = 32000
               RETURN  digito
           END IF
       END FOR
       LET j = 0

       FOR i = 1 TO longitud
          LET j = j + 1
          IF i MOD 2 = 0 THEN
              LET temp      = valor[i] * 2
              LET cadena[j] = temp[1]

              IF LENGTH(temp) > 1 THEN
                  LET j = j + 1
                  LET cadena[j] = temp[2]
              END IF
         ELSE
              LET cadena[j] = valor[i]
         END IF
       END FOR

       LET suma = 0
       FOR i = 1 TO j
          LET suma = suma + cadena[i]
       END FOR

       LET sumachar = suma
       LET ultima = LENGTH(sumachar)  

       LET digito = 10 - sumachar[ultima]  

       IF digito = 10 THEN
           LET digito = 0
       END IF

       RETURN digito  

END FUNCTION
