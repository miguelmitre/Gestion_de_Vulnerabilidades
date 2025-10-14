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
     DEFINE p_tabcampo  RECORD LIKE safre_af:tab_campo.*,
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
       i,j            SMALLINT

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
               SELECT g.*  FROM  safre_af:tab_campo g
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
##display "vcampo :",v_campo sleep 1
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
                   SELECT a.*  FROM  safre_af:tab_campo a
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

FUNCTION llena_comision()
  DEFINE   cat       RECORD
              afore_x      CHAR(03),
              comis_afo    DECIMAL(5,2)
           END RECORD,

           llena_com   RECORD
               afore_a,  afore_b,
               afore_c,  afore_d,
               afore_e,  afore_f,
               afore_g,  afore_h,
               afore_i,  afore_j,
               afore_k,  afore_l,
               afore_m,  afore_n,
               afore_o,  afore_p,
               afore_q,  afore_r,
               afore_s,  afore_t,
               afore_u,  afore_v,
               afore_w,  afore_x,
               afore_y,  afore_z        CHAR(20), 

               comis_a,  comis_b,
               comis_c,  comis_d,
               comis_e,  comis_f,
               comis_g,  comis_h,
               comis_i,  comis_j,
               comis_k,  comis_l,
               comis_m,  comis_n,
               comis_o,  comis_p,
               comis_q,  comis_r,
               comis_s,  comis_t,
               comis_u,  comis_v,
               comis_w,  comis_x,
               comis_y,  comis_z        CHAR(07)
           END RECORD,

           j,i,long        SMALLINT,
           afore           CHAR(20),
           comision        CHAR(07),
           cuantos         SMALLINT,
           max_fecha       DATE,
           enter           CHAR(01)

   INITIALIZE llena_com.* TO NULL

   SELECT MAX(a.fecha) INTO max_fecha FROM tab_comparativo a
   WHERE  a.tipo_comparativo = 1

   LET cuantos = 0
   SELECT COUNT(*) INTO cuantos
   FROM   tab_comparativo a
   WHERE  a.fecha = max_fecha
   AND    a.tipo_comparativo = 1
   IF cuantos < 15 THEN
      PROMPT "No estan completas las Afores en el Comparativo, <Enter> "
      FOR enter
      RETURN llena_com.*,1
   END IF

   LET j = 1
   DECLARE apt_comis CURSOR FOR
      SELECT a.afore_cod, a.porcentaje
      FROM   tab_comparativo a
      WHERE  a.fecha = max_fecha
     AND    a.tipo_comparativo = 1
      ORDER BY a.porcentaje
   FOREACH apt_comis INTO cat.*

      INITIALIZE afore, comision TO NULL

      IF cat.afore_x = "999" THEN
         LET afore = "PROMEDIO            "
      ELSE
         SELECT m.afore_desc INTO afore FROM tab_afore m
         WHERE  m.afore_cod = cat.afore_x
      END IF

      LET long = 0    LET i = 0
      LET long = LENGTH(afore)
      IF long = 0 THEN
         LET afore = "                    "
      ELSE
             IF long < 20 THEN
                LET long = long + 1
                FOR i = long TO 20
                    LET afore[i,i] = " "
                END FOR
             END IF
      END IF

      LET comision = cat.comis_afo USING "##&.&&","%"

      CASE j
           WHEN 1  LET llena_com.afore_a = afore
                   LET llena_com.comis_a = comision
           WHEN 2  LET llena_com.afore_b = afore
                   LET llena_com.comis_b = comision
           WHEN 3  LET llena_com.afore_c = afore
                   LET llena_com.comis_c = comision
           WHEN 4  LET llena_com.afore_d = afore
                   LET llena_com.comis_d = comision
           WHEN 5  LET llena_com.afore_e = afore
                   LET llena_com.comis_e = comision
           WHEN 6  LET llena_com.afore_f = afore
                   LET llena_com.comis_f = comision
           WHEN 7  LET llena_com.afore_g = afore
                   LET llena_com.comis_g = comision
           WHEN 8  LET llena_com.afore_h = afore
                   LET llena_com.comis_h = comision
           WHEN 9  LET llena_com.afore_i = afore
                   LET llena_com.comis_i = comision
           WHEN 10 LET llena_com.afore_j = afore
                   LET llena_com.comis_j = comision
           WHEN 11 LET llena_com.afore_k = afore
                   LET llena_com.comis_k = comision
           WHEN 12 LET llena_com.afore_l = afore
                   LET llena_com.comis_l = comision
           WHEN 13 LET llena_com.afore_m = afore
                   LET llena_com.comis_m = comision
           WHEN 14 LET llena_com.afore_n = afore
                   LET llena_com.comis_n = comision
           WHEN 15 LET llena_com.afore_o = afore
                   LET llena_com.comis_o = comision
           WHEN 16 LET llena_com.afore_p = afore
                   LET llena_com.comis_p = comision
           WHEN 17 LET llena_com.afore_q = afore
                   LET llena_com.comis_q = comision
           WHEN 18 LET llena_com.afore_r = afore
                   LET llena_com.comis_r = comision
           WHEN 19 LET llena_com.afore_s = afore
                   LET llena_com.comis_s = comision
           WHEN 20 LET llena_com.afore_t = afore
                   LET llena_com.comis_t = comision
           WHEN 21 LET llena_com.afore_u = afore
                   LET llena_com.comis_u = comision
           WHEN 22 LET llena_com.afore_v = afore
                   LET llena_com.comis_v = comision
           WHEN 23 LET llena_com.afore_w = afore
                   LET llena_com.comis_w = comision
           WHEN 24 LET llena_com.afore_x = afore
                   LET llena_com.comis_x = comision
           WHEN 25 LET llena_com.afore_y = afore
                   LET llena_com.comis_y = comision
           WHEN 26 LET llena_com.afore_z = afore
                   LET llena_com.comis_z = comision
      END CASE

      LET j = j + 1
   END FOREACH

   IF llena_com.afore_p IS NULL THEN
       LET  llena_com.afore_p    = "                    "
       LET  llena_com.comis_p    = "       "
   END IF

      IF llena_com.afore_q IS NULL THEN
         LET llena_com.afore_q = "                    "
         LET llena_com.comis_q = "       "
      END IF

      IF llena_com.afore_r IS NULL THEN
         LET llena_com.afore_r = "                    "
         LET llena_com.comis_r = "       "
      END IF

      IF llena_com.afore_s IS NULL THEN
         LET llena_com.afore_s = "                    "
         LET llena_com.comis_s = "       "
      END IF

      IF llena_com.afore_t IS NULL THEN
         LET llena_com.afore_t = "                    "
         LET llena_com.comis_t = "       "
      END IF

      IF llena_com.afore_u IS NULL THEN
         LET llena_com.afore_u = "                    "
         LET llena_com.comis_u = "       "
      END IF

      IF llena_com.afore_v IS NULL THEN
         LET llena_com.afore_v = "                    "
         LET llena_com.comis_v = "       "
      END IF

      IF llena_com.afore_w IS NULL THEN
         LET llena_com.afore_w = "                    "
         LET llena_com.comis_w = "       "
      END IF

      IF llena_com.afore_x IS NULL THEN
         LET llena_com.afore_x = "                    "
         LET llena_com.comis_x = "       "
      END IF

      IF llena_com.afore_y IS NULL THEN
         LET llena_com.afore_y = "                    "
         LET llena_com.comis_y = "       "
      END IF

      IF llena_com.afore_z IS NULL THEN
         LET llena_com.afore_z = "                    "
         LET llena_com.comis_z = "       "
      END IF

   RETURN llena_com.*,0
END FUNCTION
################################################################################
FUNCTION digito_verif(valor,longitud )
  DEFINE cadena    CHAR(20),
         valor     CHAR(10),
         longitud  SMALLINT,
         suma      SMALLINT,
         sumachar  CHAR(2),
         digito    SMALLINT,
         i,j       SMALLINT,
         temp      CHAR(2)

  DEFINE ultima   SMALLINT
  DEFINE t        SMALLINT 

  define x array[10] of char(1)

       LET x[1] =valor[1]
       LET x[2] =valor[2]
       LET x[3] =valor[3]
       LET x[4] =valor[4]
       LET x[5] =valor[5]
       LET x[6] =valor[6]
       LET x[7] =valor[7]
       LET x[8] =valor[8]
       LET x[9] =valor[9]
       LET x[10] =valor[10]

  FOR t = 1 TO 10
    IF x[t] <> "0" AND
       x[t] <> "1" AND
       x[t] <> "2" AND
       x[t] <> "3" AND
       x[t] <> "4" AND
       x[t] <> "5" AND
       x[t] <> "6" AND
       x[t] <> "7" AND
       x[t] <> "8" AND
       x[t] <> "9" THEN
       LET digito = 32000
       RETURN  digito
    END IF
  END FOR

  LET j = 0
  FOR i = 1 TO longitud
     LET j = j + 1
     IF i MOD 2 = 0 THEN
         LET temp = valor[i] * 2
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
################################################################################
FUNCTION valida_fecha_rfc(a)

define a char(6)
define x array[6] of char(1)
define t smallint

       LET x[1] =a[1]
       LET x[2] =a[2]
       LET x[3] =a[3]
       LET x[4] =a[4]
       LET x[5] =a[5]
       LET x[6] =a[6]

FOR t = 1 TO 6
    IF x[t] <> "0" AND
       x[t] <> "1" AND
       x[t] <> "2" AND
       x[t] <> "3" AND
       x[t] <> "4" AND
       x[t] <> "5" AND
       x[t] <> "6" AND
       x[t] <> "7" AND
       x[t] <> "8" AND
       x[t] <> "9" THEN
       RETURN  FALSE
    END IF
END FOR
RETURN TRUE
END FUNCTION
###########################################################
FUNCTION var_dig_curp(curp)
   DEFINE
     dv_curp            CHAR(1),
     curp               CHAR(18),
     arr                ARRAY[18] OF RECORD
                        curp_pos        CHAR(1)
                        END RECORD,
     i                  SMALLINT,
     arr1               ARRAY[36] OF RECORD
                        char            CHAR(1),
                        val             SMALLINT
                        END RECORD,
     j                  SMALLINT,
     arr2               ARRAY[17] OF RECORD
                        cons            SMALLINT
                        END RECORD,
     k                  SMALLINT,
     resultado          INTEGER,
     dism               SMALLINT,
     f                  SMALLINT,
     n                  SMALLINT,
     a                  SMALLINT,
     arr3               ARRAY[17] OF RECORD
                        mult            INTEGER
                        END RECORD,
     res_mult           INTEGER,
     acu_mult           INTEGER,
     residuo            SMALLINT,
     dig_ver_curp       SMALLINT,
     pasa               CHAR(1)

   LET pasa = 0

   ### SEPARA CURP POR POSICIONES
   LET arr[1].curp_pos  = curp[1]  LET arr[2].curp_pos  = curp[2]
   LET arr[3].curp_pos  = curp[3]  LET arr[4].curp_pos  = curp[4]
   LET arr[5].curp_pos  = curp[5]  LET arr[6].curp_pos  = curp[6]
   LET arr[7].curp_pos  = curp[7]  LET arr[8].curp_pos  = curp[8]
   LET arr[9].curp_pos  = curp[9]  LET arr[10].curp_pos = curp[10]
   LET arr[11].curp_pos = curp[11] LET arr[12].curp_pos = curp[12]
   LET arr[13].curp_pos = curp[13] LET arr[14].curp_pos = curp[14]
   LET arr[15].curp_pos = curp[15] LET arr[16].curp_pos = curp[16]
   LET arr[17].curp_pos = curp[17] LET arr[18].curp_pos = curp[18]

   ### PREPARA CARACTER PARA VALORES
   LET j = 0
   FOR j = 1 TO 36
      LET arr1[j].char = j
      LET arr1[j].val  = j
   END FOR

   LET arr1[10].char = 'A' LET arr1[11].char = 'B' LET arr1[12].char = 'C'
   LET arr1[13].char = 'D' LET arr1[14].char = 'E' LET arr1[15].char = 'F'
   LET arr1[16].char = 'G' LET arr1[17].char = 'H' LET arr1[18].char = 'I'
   LET arr1[19].char = 'J' LET arr1[20].char = 'K' LET arr1[21].char = 'L'
   LET arr1[22].char = 'M' LET arr1[23].char = 'N' LET arr1[24].char = 'Ñ'
   LET arr1[25].char = 'O' LET arr1[26].char = 'P' LET arr1[27].char = 'Q'
   LET arr1[28].char = 'R' LET arr1[29].char = 'S' LET arr1[30].char = 'T'
   LET arr1[31].char = 'U' LET arr1[32].char = 'V' LET arr1[33].char = 'W'
   LET arr1[34].char = 'X' LET arr1[35].char = 'Y' LET arr1[36].char = 'Z'

   ### PREPARA CONSTANTES
   LET k    = 0
   LET dism = 18
   FOR k = 1 TO 17
      LET arr2[k].cons = dism
      LET dism = dism - 1
   END FOR

   ### OBTIENE DIGITO
   LET f = 0
   LET n = 0
   LET a = 0
   LET res_mult     = 0
   LET residuo      = 0
   LET dig_ver_curp = 0
   FOR f = 1 TO 17
      FOR n = 1 TO 36
        IF arr[f].curp_pos  = arr1[n].char THEN
           LET arr3[f].mult = arr1[n].val * arr2[f].cons
           LET res_mult     = arr3[f].mult
           LET acu_mult     = acu_mult + res_mult
        END IF
      END FOR
   END FOR

   ### OBTIENE RESIDUO Y SE RESTA CON CONSTANTE
   LET residuo = acu_mult MOD 10
   IF residuo = 0 THEN
      LET dig_ver_curp = 0
   ELSE
      LET dig_ver_curp = 10 - residuo
   END IF

   ### VALIDA RESULTADO DE D.V. VS POS. 18
   IF arr[18].curp_pos = dig_ver_curp THEN
      LET pasa = 1
   ELSE
      LET pasa = 0
   END IF

   RETURN pasa, dig_ver_curp

END FUNCTION
###########################################################################
FUNCTION valida_est_curp(curp)
  DEFINE
     curp                                       CHAR(18),
     arr_curp                                   ARRAY[18] OF RECORD
                                                curp_pos        CHAR(1)
                                                END RECORD,
     i                                          SMALLINT,
     arr_letr                                   ARRAY[27] OF RECORD
                                                car             CHAR(1)
                                                END RECORD,
     j                                          SMALLINT,
     arr_nume                                   ARRAY[10] OF RECORD
                                                num             CHAR(1)
                                                END RECORD,
     k                                          SMALLINT,
     pasa                                       CHAR(1),
     contador1                                  SMALLINT,
     contador2                                  SMALLINT,
     contador3                                  SMALLINT,
     contador4                                  SMALLINT,
     contador5                                  SMALLINT,
     desc_err                                   CHAR(60),
     desp_err                                   SMALLINT

   LET pasa = 0

   ### SEPARA CURP POR POSICIONES
   LET arr_curp[01].curp_pos = curp[01]  LET arr_curp[02].curp_pos = curp[02]
   LET arr_curp[03].curp_pos = curp[03]  LET arr_curp[04].curp_pos = curp[04]
   LET arr_curp[05].curp_pos = curp[05]  LET arr_curp[06].curp_pos = curp[06]
   LET arr_curp[07].curp_pos = curp[07]  LET arr_curp[08].curp_pos = curp[08]
   LET arr_curp[09].curp_pos = curp[09]  LET arr_curp[10].curp_pos = curp[10]
   LET arr_curp[11].curp_pos = curp[11]  LET arr_curp[12].curp_pos = curp[12]
   LET arr_curp[13].curp_pos = curp[13]  LET arr_curp[14].curp_pos = curp[14]
   LET arr_curp[15].curp_pos = curp[15]  LET arr_curp[16].curp_pos = curp[16]

   LET arr_curp[17].curp_pos = curp[17]  LET arr_curp[18].curp_pos = curp[18]

   ### INICIALIZA ARREGLO CON VALORES ALFABETICOS
   LET arr_letr[01].car = 'A'  LET arr_letr[02].car = 'B'
   LET arr_letr[03].car = 'C'  LET arr_letr[04].car = 'D'
   LET arr_letr[05].car = 'E'  LET arr_letr[06].car = 'F'
   LET arr_letr[07].car = 'G'  LET arr_letr[08].car = 'H'
   LET arr_letr[09].car = 'I'  LET arr_letr[10].car = 'J'
   LET arr_letr[11].car = 'K'  LET arr_letr[12].car = 'L'
   LET arr_letr[13].car = 'M'  LET arr_letr[14].car = 'N'
   LET arr_letr[15].car = 'Ñ'  LET arr_letr[16].car = 'O'
   LET arr_letr[17].car = 'P'  LET arr_letr[18].car = 'Q'
   LET arr_letr[19].car = 'R'  LET arr_letr[20].car = 'S'
   LET arr_letr[21].car = 'T'  LET arr_letr[22].car = 'U'
   LET arr_letr[23].car = 'V'  LET arr_letr[24].car = 'W'
   LET arr_letr[25].car = 'X'  LET arr_letr[26].car = 'Y'
   LET arr_letr[27].car = 'Z'

   ### INICIALIZA ARREGLO CON VALORES NUMERICOS
   LET k = 0
   FOR k = 1 TO 9
     LET arr_nume[k].num = k
   END FOR
   LET arr_nume[10].num = 0

   ### Valida curp
   LET i         = 0
   LET j         = 0
   LET k         = 0
   LET contador1 = 0
   LET contador2 = 0
   LET contador3 = 0
   LET contador4 = 0
   LET contador5 = 0
   LET desp_err  = 0

   FOR i = 1 TO 18

     ### Valida letras (Pos 1 a 4)
     IF i >= 1 AND i <= 4 THEN
        FOR j = 1 TO 27
           IF arr_curp[i].curp_pos = arr_letr[j].car THEN
              LET contador1 = contador1 + 1
           END IF
        END FOR
     END IF

     ### Valida numeros (Pos 5 a 10)
     IF i >= 5 AND i <= 10 THEN
        FOR k = 1 TO 10
           IF arr_curp[i].curp_pos = arr_nume[k].num THEN
              LET contador2 = contador2 + 1
           END IF
        END FOR
     END IF

     ### Valida sexo (Pos 11)
     IF i = 11 THEN
        IF arr_curp[i].curp_pos NOT MATCHES "[HM]" THEN
           LET contador3 = 1
        END IF
     END IF
     ### Valida letras (Pos 12 a 16)
     IF i >= 12 AND i <= 16 THEN
        FOR j = 1 TO 27
           IF arr_curp[i].curp_pos = arr_letr[j].car THEN
              LET contador4 = contador4 + 1
           END IF
        END FOR
     END IF

     ### Valida numeros (Pos 17 a 18)
     IF i >= 17 AND i <= 18 THEN
        FOR k = 1 TO 10
           IF arr_curp[i].curp_pos = arr_nume[k].num THEN
              LET contador5 = contador5 + 1
           END IF
        END FOR
     END IF

   END FOR

   IF contador1 < 04 THEN
      LET pasa = 1
      LET desc_err = "Error en las primeras 4 posiciones de la CURP"
      LET desp_err = 1
   END IF

   IF desp_err = 0 THEN
      IF contador2 < 06 THEN
         LET pasa = 1
         LET desc_err = "Error en las posiciones 5 a 10 de la CURP"
         LET desp_err = 1
      END IF
   END IF

   IF desp_err = 0 THEN
      IF contador3 = 01 THEN
         LET pasa = 1
         LET desc_err = "Error en la posicion 11 de la CURP"
         LET desp_err = 1
      END IF
   END IF

   IF desp_err = 0 THEN
      IF contador4 < 05 THEN
         LET pasa = 1
         LET desc_err = "Error en las posiciones 12 a 16 de la CURP"
         LET desp_err = 1
      END IF
   END IF

   IF desp_err = 0 THEN
      IF contador5 < 02 THEN
         LET pasa = 1
         LET desc_err = "Error en las posiciones 17 a 18 de la CURP"
         LET desp_err = 1
      END IF
   END IF

   RETURN pasa, desc_err

END FUNCTION
###################################################################
FUNCTION Digito_prom(vcod_promotor)

   DEFINE vcod_promotor CHAR(10)

   DEFINE vano          CHAR(02),
          iano          INTEGER,
          iano_act      INTEGER,
          iano4d        INTEGER,
          vcodigo_error CHAR(02)

   DEFINE vmes CHAR(02),
          imes INTEGER

   DEFINE vpos1 CHAR(01),
          vpos2 CHAR(01),
          vpos3 CHAR(01),
          vpos4 CHAR(01),
          vpos5 CHAR(01),
          vpos6 CHAR(01),
          vpos7 CHAR(01),
          vpos8 CHAR(01),
          vpos9 CHAR(01),
          vpos10 CHAR(01),
--          ipos10 SMALLINT,
          vdigito CHAR(10)

   DEFINE ipos1 INTEGER,
          ipos2 INTEGER,
          ipos3 INTEGER,
          ipos4 INTEGER,
          ipos5 INTEGER,
          ipos6 INTEGER,
          ipos7 INTEGER,
          ipos8 INTEGER,
          ipos9 INTEGER,
          ipos10 INTEGER,
          ddigito DECIMAL(12,1),
          zdigito CHAR(03),
          xdigpos3 CHAR(01),
          ydigpos3 SMALLINT,
          idigpos3 SMALLINT,
          gdigpos3 CHAR(01),
          icalcula INTEGER

   DEFINE xpos9  CHAR(02),
          xpos9a CHAR(01),
          xpos9b CHAR(01),
          ypos9a SMALLINT,
          ypos9b SMALLINT,

          xpos8  CHAR(02),
          xpos8a CHAR(01),
          xpos8b CHAR(01),
          ypos8a SMALLINT,
          ypos8b SMALLINT,

          xpos7  CHAR(02),
          xpos7a CHAR(01),
          xpos7b CHAR(01),
          ypos7a SMALLINT,
          ypos7b SMALLINT,

          xpos6  CHAR(02),
          xpos6a CHAR(01),
          xpos6b CHAR(01),
          ypos6a SMALLINT,
          ypos6b SMALLINT,

          xpos5  CHAR(02),
          xpos5a CHAR(01),
          xpos5b CHAR(01),
          ypos5a SMALLINT,
          ypos5b SMALLINT,

          xpos4  CHAR(02),
          xpos4a CHAR(01),
          xpos4b CHAR(01),
          ypos4a SMALLINT,
          ypos4b SMALLINT,

          xpos3  CHAR(02),
          xpos3a CHAR(01),
          xpos3b CHAR(01),
          ypos3a SMALLINT,
          ypos3b SMALLINT,

          xpos2  CHAR(02),
          xpos2a CHAR(01),
          xpos2b CHAR(01),
          ypos2a SMALLINT,
          ypos2b SMALLINT,

          xpos1  CHAR(02),
          xpos1a CHAR(01),
          xpos1b CHAR(01),
          ypos1a SMALLINT,
          ypos1b SMALLINT


   LET vcodigo_error ="00" --sin error en la funcion

   ------ valida que el codigo sea de 10 ------
   IF LENGTH(vcod_promotor) = 10 THEN
      LET vcodigo_error ="00" --sin error en la funcion
   ELSE
      LET vcodigo_error ="01" --longitud codigo promotor diferente de 10

   END IF

   ------ valida que el año sea entre 1997 y el año actual ------
   LET vpos1 = vcod_promotor[1]
   IF vpos1 not MATCHES "[0123456789]" THEN
      LET vcodigo_error = "02" --la posicion de 5 a 9 no es un numero
   END IF

   LET vpos2 = vcod_promotor[2]
   IF vpos2 not MATCHES "[0123456789]" THEN
      LET vcodigo_error = "02" --la posicion de 5 a 9 no es un numero
   END IF

   LET iano_act = YEAR(TODAY)
   LET vano = vcod_promotor[1,2]
   LET iano = vano

   IF iano <= 96 THEN
      LET iano4d = 2000 + iano
   ELSE
      LET iano4d = 1900 + iano
   END IF

   IF iano4d < 1997 OR iano4d > iano_act THEN
      LET vcodigo_error = "02" --año no esta dentro del rango 97 y año actual
   END IF
   --------------------------------------------------------------

   ------ valida que el mes este entre 1 y 12 ------
   LET vpos3 = vcod_promotor[3]
   IF vpos3 not MATCHES "[0123456789]" THEN
      LET vcodigo_error = "03" --la posicion de 5 a 9 no es un numero
   END IF

   LET vpos4 = vcod_promotor[4]
   IF vpos4 not MATCHES "[0123456789]" THEN
      LET vcodigo_error = "03" --la posicion de 5 a 9 no es un numero
   END IF

   LET vmes = vcod_promotor[3,4]
   LET imes = vmes

   IF imes < 1 OR imes > 12 THEN
      LET vcodigo_error = "03" --mes no esta dentro del rango 1 y 12
   END IF
   --------------------------------------------------------------

   ------ valida que las posiciones 5 a 9 sean numericas ------

   LET vpos5 = vcod_promotor[5]
   IF vpos5 not MATCHES "[0123456789]" THEN
      LET vcodigo_error = "04" --la posicion de 5 a 9 no es un numero
   END IF

   LET vpos6 = vcod_promotor[6]
   IF vpos6 not MATCHES "[0123456789]" THEN
      LET vcodigo_error = "04" --la posicion de 5 a 9 no es un numero
   END IF

   LET vpos7 = vcod_promotor[7]
   IF vpos7 not MATCHES "[0123456789]" THEN
      LET vcodigo_error = "04" --la posicion de 5 a 9 no es un numero
   END IF

   LET vpos8 = vcod_promotor[8]
   IF vpos8 not MATCHES "[0123456789]" THEN
      LET vcodigo_error = "04" --la posicion de 5 a 9 no es un numero
   END IF

   LET vpos9 = vcod_promotor[9]
   IF vpos9 not MATCHES "[0123456789]" THEN
      LET vcodigo_error = "04" --la posicion de 5 a 9 no es un numero
   END IF
   --------------------------------------------------------------

   ----- valida digito verificador ------

   LET vpos10 = vcod_promotor[10]
   LET ipos10 = vpos10
   IF vpos10 not MATCHES "[0123456789]" THEN
      LET vcodigo_error = "05" --la posicion 10 no es numero
   END IF

   LET ipos1 = vpos1
   LET ipos2 = vpos2
   LET ipos3 = vpos3
   LET ipos4 = vpos4
   LET ipos5 = vpos5
   LET ipos6 = vpos6
   LET ipos7 = vpos7
   LET ipos8 = vpos8
   LET ipos9 = vpos9
   LET ipos10 = vpos10

   LET ipos9 = ipos9 * 2
   LET ipos8 = ipos8 * 1
   LET ipos7 = ipos7 * 2
   LET ipos6 = ipos6 * 1
   LET ipos5 = ipos5 * 2
   LET ipos4 = ipos4 * 1
   LET ipos3 = ipos3 * 2
   LET ipos2 = ipos2 * 1
   LET ipos1 = ipos1 * 2

   LET xpos9 = ipos9
   IF LENGTH(xpos9) = 2 THEN
      LET xpos9a = xpos9[1]
      LET xpos9b = xpos9[2]
   ELSE
      LET xpos9a = xpos9[1]
      LET xpos9b = "0"
   END IF
   LET ypos9a = xpos9a
   LET ypos9b = xpos9b

   LET xpos8 = ipos8
   IF LENGTH(xpos8) = 2 THEN
      LET xpos8a = xpos8[1]
      LET xpos8b = xpos8[2]
   ELSE
      LET xpos8a = xpos8[1]
      LET xpos8b = "0"
   END IF
   LET ypos8a = xpos8a
   LET ypos8b = xpos8b

   LET xpos7 = ipos7
   IF LENGTH(xpos7) = 2 THEN
      LET xpos7a = xpos7[1]
      LET xpos7b = xpos7[2]
   ELSE
      LET xpos7a = xpos7[1]
      LET xpos7b = "0"
   END IF
   LET ypos7a = xpos7a
   LET ypos7b = xpos7b

   LET xpos6 = ipos6
   IF LENGTH(xpos6) = 2 THEN
      LET xpos6a = xpos6[1]
      LET xpos6b = xpos6[2]
   ELSE
      LET xpos6a = xpos6[1]
      LET xpos6b = "0"
   END IF
   LET ypos6a = xpos6a
   LET ypos6b = xpos6b

   LET xpos5 = ipos5
   IF LENGTH(xpos5) = 2 THEN
      LET xpos5a = xpos5[1]
      LET xpos5b = xpos5[2]
   ELSE
      LET xpos5a = xpos5[1]
      LET xpos5b = "0"
   END IF
   LET ypos5a = xpos5a
   LET ypos5b = xpos5b

   LET xpos4 = ipos4
   IF LENGTH(xpos4) = 2 THEN
      LET xpos4a = xpos4[1]
      LET xpos4b = xpos4[2]
   ELSE
      LET xpos4a = xpos4[1]
      LET xpos4b = "0"
   END IF
   LET ypos4a = xpos4a

   LET ypos4b = xpos4b

   LET xpos3 = ipos3
   IF LENGTH(xpos3) = 2 THEN
      LET xpos3a = xpos3[1]
      LET xpos3b = xpos3[2]
   ELSE
      LET xpos3a = xpos3[1]
      LET xpos3b = "0"
   END IF
   LET ypos3a = xpos3a
   LET ypos3b = xpos3b

   LET xpos2 = ipos2
   IF LENGTH(xpos2) = 2 THEN
      LET xpos2a = xpos2[1]
      LET xpos2b = xpos2[2]
   ELSE
      LET xpos2a = xpos2[1]
      LET xpos2b = "0"
   END IF
   LET ypos2a = xpos2a
   LET ypos2b = xpos2b

   LET xpos1 = ipos1
   IF LENGTH(xpos1) = 2 THEN
      LET xpos1a = xpos1[1]
      LET xpos1b = xpos1[2]
   ELSE
      LET xpos1a = xpos1[1]
      LET xpos1b = "0"
   END IF
   LET ypos1a = xpos1a
   LET ypos1b = xpos1b

   LET icalcula = ypos1a + ypos1b +
                  ypos2a + ypos2b +
                  ypos3a + ypos3b +
                  ypos4a + ypos4b +
                  ypos5a + ypos5b +
                  ypos6a + ypos6b +
                  ypos7a + ypos7b +
                  ypos8a + ypos8b +
                  ypos9a + ypos9b

   LET ddigito = icalcula / 10

   LET zdigito = ddigito
   LET xdigpos3 = zdigito[3]
   LET ydigpos3 = xdigpos3
   LET idigpos3 = 10 - ydigpos3 --parametro a regresar
   LET gdigpos3 = idigpos3

   IF ipos10 <> idigpos3 THEN
      LET vcodigo_error = "06" --digito verificador diferente del capturado
   END IF

   LET vcod_promotor = vpos1,vpos2,vpos3,vpos4,vpos5,
                       vpos6,vpos7,vpos8,vpos9,gdigpos3
   --------------------------------------------------------------

   RETURN vcod_promotor,
          vcodigo_error,
          idigpos3


END FUNCTION

--fh

FUNCTION verifica_nombre(no_bre,campo)
#ve---------------------------------
  DEFINE no_bre          CHAR(40),
         bl1, bl2, bl3   SMALLINT,
         i,  long, var   SMALLINT,
         vval            CHAR(80),
         espe            CHAR(40),
         campo           SMALLINT

  INITIALIZE vval TO NULL
  LET bl1 = 0   LET bl2 = 0  LET bl3 = 0  
  LET i = 0     LET long = 0 LET var = 0

  LET long   = LENGTH(no_bre CLIPPED)
  IF no_bre[1,3] MATCHES "XXX" THEN
     LET bl3 = 1
  END IF

  FOR i = 1 TO long
      IF i < 40 THEN
         IF no_bre[i,i+1] = "  " THEN
            LET bl1 = 1
         END IF
      END IF
  END FOR 

  INITIALIZE espe TO NULL
  LET i = 0
  CASE campo
  WHEN 1
  FOR i = 1 TO long
      IF no_bre[i,i] = "[" OR no_bre[i,i] = '"'  OR
         no_bre[i,i] = "]" OR no_bre[i,i] = "#"  OR
         no_bre[i,i] = "$" OR no_bre[i,i] = "%"  OR
         no_bre[i,i] = "&" OR no_bre[i,i] = "="  OR
         no_bre[i,i] = "/" OR no_bre[i,i] = "?"  OR
         no_bre[i,i] = "-" OR no_bre[i,i] = "'"  OR
         no_bre[i,i] = "(" OR no_bre[i,i] = ")"  OR
         no_bre[i,i] = "^" OR no_bre[i,i] = "!"  OR
         no_bre[i,i] = "~" OR no_bre[i,i] = "_"  OR
         no_bre[i,i] = ":"  OR
         no_bre[i,i] = "," OR no_bre[i,i] = ";"  OR
         no_bre[i,i] = "<" OR no_bre[i,i] = ">"  OR
         no_bre[i,i] = "@" OR no_bre[i,i] = "|"  OR
         no_bre[i,i] = "{" OR no_bre[i,i] = "}"  OR
         no_bre[i,i] = "+" OR no_bre[i,i] = "*"  OR
         no_bre[i,i] = "`" OR no_bre[i,i] = "1"  OR
         no_bre[i,i] = "2" OR no_bre[i,i] = "3"  OR
         no_bre[i,i] = "4" OR no_bre[i,i] = "5"  OR
         no_bre[i,i] = "6" OR no_bre[i,i] = "7"  OR
         no_bre[i,i] = "8" OR no_bre[i,i] = "9"  OR
         no_bre[i,i] = "0" OR no_bre[i,i] = "¿"  OR
         no_bre[i,i] = "¡" OR no_bre[i,i] = "Ä"  OR
         no_bre[i,i] = "É" OR no_bre[i,i] = "Í"  OR
         no_bre[i,i] = "Ó" OR no_bre[i,i] = "Ú"  OR
         no_bre[i,i] = "¨" OR no_bre[i,i] = "Ä"  OR
         no_bre[i,i] = "Ë" OR no_bre[i,i] = "Ï"  OR
         no_bre[i,i] = "Ö" OR no_bre[i,i] = "Ö"  OR
         no_bre[i,i] = "Ü" OR no_bre[i,i] = "á"  OR
         no_bre[i,i] = "é" OR no_bre[i,i] = "í"  OR
         no_bre[i,i] = "ó" OR no_bre[i,i] = "ú"  OR
         no_bre[i,i] = "ä" OR no_bre[i,i] = "ë"  OR
         no_bre[i,i] = "ï" OR no_bre[i,i] = "ö"  OR
         no_bre[i,i] = "ü" OR no_bre[i,i] = "´"  OR
         no_bre[i,i] = "Á" THEN
         LET espe[i,i] = no_bre[i,i]
         LET bl2 = 1
         EXIT FOR
      END IF
  END FOR
  OTHERWISE
  FOR i = 1 TO long
      IF no_bre[i,i] = "[" OR no_bre[i,i] = '"'  OR
         no_bre[i,i] = "]" OR no_bre[i,i] = "#"  OR
         no_bre[i,i] = "$" OR no_bre[i,i] = "%"  OR
         no_bre[i,i] = "&" OR no_bre[i,i] = "="  OR
         no_bre[i,i] = "/" OR no_bre[i,i] = "?"  OR
         no_bre[i,i] = "-" OR no_bre[i,i] = "'"  OR
         no_bre[i,i] = "(" OR no_bre[i,i] = ")"  OR
         no_bre[i,i] = "^" OR no_bre[i,i] = "!"  OR
         no_bre[i,i] = "~" OR no_bre[i,i] = "_"  OR
         no_bre[i,i] = "." OR no_bre[i,i] = ":"  OR
         no_bre[i,i] = "," OR no_bre[i,i] = ";"  OR
         no_bre[i,i] = "<" OR no_bre[i,i] = ">"  OR
         no_bre[i,i] = "@" OR no_bre[i,i] = "|"  OR
         no_bre[i,i] = "{" OR no_bre[i,i] = "}"  OR
         no_bre[i,i] = "+" OR no_bre[i,i] = "*"  OR
         no_bre[i,i] = "`" OR no_bre[i,i] = "1"  OR
         no_bre[i,i] = "2" OR no_bre[i,i] = "3"  OR
         no_bre[i,i] = "4" OR no_bre[i,i] = "5"  OR
         no_bre[i,i] = "6" OR no_bre[i,i] = "7"  OR
         no_bre[i,i] = "8" OR no_bre[i,i] = "9"  OR
         no_bre[i,i] = "0" OR no_bre[i,i] = "¿"  OR
         no_bre[i,i] = "¡" OR no_bre[i,i] = "Ä"  OR
         no_bre[i,i] = "É" OR no_bre[i,i] = "Í"  OR
         no_bre[i,i] = "Ó" OR no_bre[i,i] = "Ú"  OR
         no_bre[i,i] = "¨" OR no_bre[i,i] = "Ä"  OR
         no_bre[i,i] = "Ë" OR no_bre[i,i] = "Ï"  OR
         no_bre[i,i] = "Ö" OR no_bre[i,i] = "Ö"  OR
         no_bre[i,i] = "Ü" OR no_bre[i,i] = "á"  OR
         no_bre[i,i] = "é" OR no_bre[i,i] = "í"  OR
         no_bre[i,i] = "ó" OR no_bre[i,i] = "ú"  OR
         no_bre[i,i] = "ä" OR no_bre[i,i] = "ë"  OR
         no_bre[i,i] = "ï" OR no_bre[i,i] = "ö"  OR
         no_bre[i,i] = "ü" OR no_bre[i,i] = "´"  OR
         no_bre[i,i] = "Á" THEN

         LET espe[i,i] = no_bre[i,i]
         LET bl2 = 1
         EXIT FOR
      END IF
  END FOR
  END CASE

  IF bl1 = 1 THEN
     LET vval = "tiene mas de 1 espacio "
     LET var = 1
  END IF

  IF bl2 = 1 THEN
     LET long = 0
     LET long = LENGTH(vval CLIPPED)
     IF long > 1 THEN
         LET vval = vval CLIPPED ,", caracteres especiales "
         LET var = 1
     ELSE
         LET vval = vval CLIPPED ," tiene caracteres especiales "
         LET var = 1
     END IF
  END IF

  IF bl3 = 1 THEN
     LET long = 0
     LET long = LENGTH(vval CLIPPED)
     IF long > 1 THEN
        LET vval = vval  CLIPPED,", comienza con XXX "
        LET var = 1
     ELSE
        LET vval = vval  CLIPPED," no debe comenzar con XXX "
        LET var = 1
     END IF
  END IF

  IF bl1 = 0  AND bl2 = 0 AND bl3 = 0 THEN
     LET var = 0
     LET vval = "                 "
  END IF

  RETURN var, vval
END FUNCTION

###################################################################
FUNCTION verifica_rfc(r_f_c)
#ve---------------------------------
  DEFINE r_f_c           CHAR(4),
         bl1, bl2        SMALLINT,
         i,  long, var   SMALLINT,
         vval            CHAR(80),
         espe            CHAR(4)

  INITIALIZE vval TO NULL
  LET bl1 = 0   LET bl2 = 0
  LET i = 0     LET long = 0 LET var = 0

  LET long   = LENGTH(r_f_c CLIPPED)

  FOR i = 1 TO long
      IF i < 4 THEN
         IF r_f_c[i,i+1] = " " THEN
            LET bl1 = 1
         END IF
      END IF
  END FOR
  INITIALIZE espe TO NULL
  LET i = 0
  FOR i = 1 TO long
      IF r_f_c[i,i] = "[" OR r_f_c[i,i] = '"'  OR
         r_f_c[i,i] = "]" OR r_f_c[i,i] = "#"  OR
         r_f_c[i,i] = "$" OR r_f_c[i,i] = "%"  OR
         r_f_c[i,i] = "&" OR r_f_c[i,i] = "="  OR
         r_f_c[i,i] = "/" OR r_f_c[i,i] = "?"  OR
         r_f_c[i,i] = "-" OR r_f_c[i,i] = "'"  OR
         r_f_c[i,i] = "(" OR r_f_c[i,i] = ")"  OR
         r_f_c[i,i] = "^" OR r_f_c[i,i] = "!"  OR
         r_f_c[i,i] = "~" OR r_f_c[i,i] = "_"  OR
         r_f_c[i,i] = "." OR r_f_c[i,i] = ":"  OR
         r_f_c[i,i] = "," OR r_f_c[i,i] = ";"  OR
         r_f_c[i,i] = "<" OR r_f_c[i,i] = ">"  OR
         r_f_c[i,i] = "@" OR r_f_c[i,i] = "|"  OR
         r_f_c[i,i] = "{" OR r_f_c[i,i] = "}"  OR
         r_f_c[i,i] = "+" OR r_f_c[i,i] = "*"  OR
         r_f_c[i,i] = "`" OR r_f_c[i,i] = "1"  OR
         r_f_c[i,i] = "2" OR r_f_c[i,i] = "3"  OR
         r_f_c[i,i] = "4" OR r_f_c[i,i] = "5"  OR
         r_f_c[i,i] = "6" OR r_f_c[i,i] = "7"  OR
         r_f_c[i,i] = "8" OR r_f_c[i,i] = "9"  OR
         r_f_c[i,i] = "0" OR r_f_c[i,i] = "¿"  OR
         r_f_c[i,i] = "¡" OR r_f_c[i,i] = "Ä"  OR
         r_f_c[i,i] = "É" OR r_f_c[i,i] = "Í"  OR
         r_f_c[i,i] = "Ó" OR r_f_c[i,i] = "Ú"  OR
         r_f_c[i,i] = "¨" OR r_f_c[i,i] = "Ä"  OR
         r_f_c[i,i] = "Ë" OR r_f_c[i,i] = "Ï"  OR
         r_f_c[i,i] = "Ö" OR r_f_c[i,i] = "Ö"  OR
         r_f_c[i,i] = "Ü" OR r_f_c[i,i] = "á"  OR
         r_f_c[i,i] = "é" OR r_f_c[i,i] = "í"  OR
         r_f_c[i,i] = "ó" OR r_f_c[i,i] = "ú"  OR
         r_f_c[i,i] = "ä" OR r_f_c[i,i] = "ë"  OR
         r_f_c[i,i] = "ï" OR r_f_c[i,i] = "ö"  OR
         r_f_c[i,i] = "ü" OR r_f_c[i,i] = "´"  OR
         r_f_c[i,i] = "Á" OR r_f_c[i,i] = " "  THEN

         LET espe[i,i] = r_f_c[i,i]
         LET bl2 = 1
         EXIT FOR
      END IF
  END FOR

  IF bl1 = 1 THEN
     LET vval = "tiene espacio "
     LET var = 1
  END IF

  IF bl2 = 1 THEN
     LET long = 0
     LET long = LENGTH(vval CLIPPED)
     IF long > 1 THEN
         LET vval = vval CLIPPED ,", caracteres especiales  o espacio"
         LET var = 1
     ELSE
         LET vval = vval CLIPPED ," tiene caracteres especiales  o espacio"
         LET var = 1
     END IF
  END IF

  IF bl1 = 0  AND bl2 = 0  THEN
     LET var = 0
     LET vval = "                 "
  END IF

  RETURN var, vval
END FUNCTION

FUNCTION arma_clave_rfc(paterno, materno, nombres, fena)
#ac----------------------------------------------------------------
   DEFINE paterno, materno, nombres     CHAR(40),
          fena                          DATE    ,
          sexo                          SMALLINT,
          estadon                       SMALLINT,
          sexo1                         CHAR(01),
          fena1                         CHAR(06),
          pa_t1, ma_t1, no_t1           CHAR(40),
          pa_t,  ma_t,  no_t            CHAR(02),
          pater,  pater1, pater2        CHAR(40),
          pater3, pater4, pater5        CHAR(40),
          pa_papa, ma_mama              CHAR(40),
          patmat                        CHAR(03),
          mater , mater1, mater2,
          mater3, mater4, mater5        CHAR(40),
          patmatnom, patmatnom1         CHAR(04),
          nom_b , nom_b1, nom_b2,
          nom_b3, nom_b4, nom_b5        CHAR(40),
          cve_mex                       CHAR(02),
          ent_fed1                      CHAR(02),
          cve_cur                       CHAR(17),
          pa_pa, ma_ma, no_no           CHAR(01),
          ch_ll                         CHAR(02),
          consonante                    CHAR(03),

          bla, ban, i, long, bb, j      SMALLINT

   DEFINE enter char(1)

   INITIALIZE pa_t1, ma_t1, no_t1 TO NULL
   INITIALIZE pa_t,  ma_t,  no_t  TO NULL
   INITIALIZE pa_pa, ma_ma, no_no, consonante, ch_ll TO NULL
   INITIALIZE pater, pater1, pater2, pater3, pater4, pater5, pa_papa  TO NULL

   LET long = 0  LET i = 0   LET bb = 0   LET j = 0

##paterno

   LET j = 1
   LET pa_t1 = paterno CLIPPED
   LET long = LENGTH(pa_t1 CLIPPED)
   LET long = long + 1

   FOR i = 1 TO long
       IF pa_t1[i,i] = " " THEN
          LET bla = bla + 1
          CASE bla
             WHEN 1
                    IF i = 2 THEN
                       LET pater[2,2] = "X"
                       EXIT FOR
                    END IF

                    LET pater1 = pater
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = pater1
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 1
                       EXIT FOR
                    END IF
             WHEN 2
                    LET pater2 = pater
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = pater2
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 2
                       EXIT FOR
                    END IF
             WHEN 3
                    LET pater3 = pater
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = pater3
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 3
                       EXIT FOR
                    END IF
             WHEN 4
                    LET pater4 = pater
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = pater4
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 4
                       EXIT FOR
                    END IF
             WHEN 5
                    LET pater5 = pater
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = pater5
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 5
                       EXIT FOR
                    END IF
          END CASE
       ELSE
           LET pater[j,j] = pa_t1[i,i]
           LET bb = 6
           LET j = j + 1
       END IF
   END FOR

   CASE bb
        WHEN 1 LET pa_t1 = pater1
        WHEN 2 LET pa_t1 = pater2
        WHEN 3 LET pa_t1 = pater3
        WHEN 4 LET pa_t1 = pater4
        WHEN 5 LET pa_t1 = pater5
        WHEN 6 LET pa_t1 = pater
   END CASE

   IF pa_t1 IS NULL OR pa_t1 = " " THEN
      LET pa_t1 = pater1 CLIPPED
   END IF

   LET j = 1
   FOR i = 1 TO long
       IF j = 1 THEN
          LET pa_t[j,j] = pa_t1[i,i]
          IF pa_t[j,j] = "Ñ" OR pa_t[j,j] = "ñ" THEN
             LET pa_t[j,j] = "X"
          END IF
{
          IF pa_t[j,j] = "C" OR pa_t[j,j] = "L" THEN
             LET ch_ll[j,j] = pa_t[j,j]
          END IF
}
          LET j = j + 1

       ELSE

          IF pa_t1[i,i] MATCHES "[AEIOU]" THEN
             LET pa_t[j,j] = pa_t1[i,i]

             IF j = 2 THEN
                EXIT FOR
             END IF
          END IF
       END IF
   END FOR

{
   LET j = 1
   FOR i = 1 TO long
      LET pa_pa[j,j] = pa_papa[i,i]
      IF i > 1 THEN
         IF pa_pa[j,j] NOT MATCHES "[AEIOU]" THEN
            IF pa_pa[j,j] = "Ñ" OR pa_pa[j,j] = "ñ" THEN
               LET pa_pa = "X"
               EXIT FOR
            ELSE
               LET pa_pa = pa_papa[i,i]
               EXIT FOR
            END IF
         END IF
      END IF
   END FOR
}

## materno
   INITIALIZE mater, mater1, mater2, mater3, mater4, mater5  TO NULL

   LET long = 0  LET i = 0   LET bb = 0  LET bla = 0  LET j = 1

   LET ma_t1 = materno CLIPPED
   LET long = LENGTH(ma_t1 CLIPPED)

   IF long IS NULL OR long = 0 THEN
      LET ma_t = "X"
      LET patmat = pa_t CLIPPED, ma_t CLIPPED
      LET ma_ma = "X"

   ELSE
         FOR i = 1 TO long
             IF ma_t1[i,i] = " " THEN
                LET bla = bla + 1
                CASE bla
                   WHEN 1
                          LET mater1 = mater CLIPPED
                          SELECT "X" FROM afi_articulo
                                  WHERE palabra = mater1
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 1
                             EXIT FOR
                          END IF
                   WHEN 2
                          LET mater2 = mater CLIPPED
                          SELECT "X" FROM afi_articulo
                                  WHERE palabra = mater2
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 2
                             EXIT FOR
                          END IF
                   WHEN 3
                          LET mater3 = mater CLIPPED
                          SELECT "X" FROM afi_articulo
                                  WHERE palabra = mater3
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 3
                             EXIT FOR
                          END IF
                   WHEN 4
                          LET mater4 = mater CLIPPED
                          SELECT "X" FROM afi_articulo
                               WHERE palabra = mater4
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 4
                             EXIT FOR
                          END IF
                   WHEN 5
                          LET mater5 = mater CLIPPED
                          SELECT "X" FROM afi_articulo
                                  WHERE palabra = mater5
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 5
                             EXIT FOR
                          END IF
                END CASE
             ELSE
                 LET mater[j,j] = ma_t1[i,i]
                 LET bb = 6
                 LET j  = j + 1
             END IF
         END FOR

         CASE bb
              WHEN 1 LET ma_t1 = mater1
              WHEN 2 LET ma_t1 = mater2
              WHEN 3 LET ma_t1 = mater3
              WHEN 4 LET ma_t1 = mater4
              WHEN 5 LET ma_t1 = mater5
              WHEN 6 LET ma_t1 = mater
         END CASE
         IF ma_t1  IS NULL OR ma_t1 = " " THEN
            LET ma_t1 = mater1
         END IF

         LET ma_mama = ma_t1 CLIPPED
         FOR i = 1 TO long
             IF i = 1 THEN
                LET ma_t[i,i] = ma_t1[i,i]
                IF ma_t[i,i] = "Ñ" OR ma_t[i,i] = "ñ" THEN
                   LET ma_t[i,i] = "X"
                END IF
                EXIT FOR
             END IF
         END FOR

{
         LET j = 1
         FOR i = 1 TO long
            LET ma_ma[j,j] = ma_mama[i,i]
            IF i > 1 THEN
               IF ma_ma[j,j] NOT MATCHES "[AEIOU]" THEN
                  IF ma_ma[j,j] = "Ñ" OR ma_ma[j,j] = "ñ" THEN
                     LET ma_ma = "X"
                     EXIT FOR
                  ELSE
                     LET ma_ma = ma_mama[i,i]
                     EXIT FOR
                  END IF
               END IF
            END IF
         END FOR
}
   END IF

## nombres
   INITIALIZE nom_b, nom_b1, nom_b2, nom_b3, nom_b4, nom_b5  TO NULL

   LET long = 0  LET i = 0   LET bb = 0  LET bla = 0  LET j = 1

   LET no_t1 = nombres CLIPPED
   LET long = LENGTH(no_t1 CLIPPED)
##   LET long = long + 1
   FOR i = 1 TO long
       IF no_t1[i,i] = " " THEN
          LET bla = bla + 1
          CASE bla
             WHEN 1
                    LET nom_b1 = nom_b CLIPPED
                    LET nom_b1 = nom_b1 CLIPPED
                    SELECT "f.X" FROM afi_articulo f
                            WHERE f.palabra MATCHES nom_b1
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE nom_b TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 1
                       EXIT FOR
                    END IF
             WHEN 2
                    LET nom_b2 = nom_b CLIPPED
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = nom_b2
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE nom_b TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 2
                       EXIT FOR
                    END IF
             WHEN 3
                    LET nom_b3 = nom_b CLIPPED
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = nom_b3
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE nom_b TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 3
                       EXIT FOR
                    END IF
             WHEN 4
                    LET nom_b4 = nom_b CLIPPED
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = nom_b4
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE nom_b TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 4
                       EXIT FOR
                    END IF
             WHEN 5
                    LET nom_b5 = nom_b CLIPPED
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = nom_b5
                    IF STATUS != NOTFOUND THEN
                       LET j = 1
                       INITIALIZE nom_b TO NULL
                    ELSE
                       LET bb = 5
                       EXIT FOR
                    END IF
          END CASE
       ELSE
           LET nom_b[j,j] = no_t1[i,i]
           LET bb = 6
           LET j  = j + 1
       END IF
   END FOR

   CASE bb
        WHEN 1 LET no_t1 = nom_b1
        WHEN 2 LET no_t1 = nom_b2
        WHEN 3 LET no_t1 = nom_b3
        WHEN 4 LET no_t1 = nom_b4
        WHEN 5 LET no_t1 = nom_b5
        WHEN 6 LET no_t1 = nom_b
   END CASE

   IF no_t1 IS NULL OR no_t1 = " " THEN
      LET nom_b1 = no_t1
   END IF


   FOR i = 1 TO long
     IF 1 = 1 THEN
        LET no_t[i,i] = no_t1[i,i]
        IF no_t[i,i] = "Ñ" OR no_t[i,i] = "ñ" THEN
           LET no_t[i,i] = "X"
        END IF
        EXIT FOR
     END IF
   END FOR
   LET patmatnom = pa_t CLIPPED, ma_t CLIPPED, no_t CLIPPED

   SELECT b.palabra_si INTO patmatnom1 FROM afi_no_conviene b
          WHERE palabra_no = patmatnom
   IF STATUS != NOTFOUND THEN
      LET patmatnom = patmatnom1 CLIPPED
   END IF

{
   LET j = 1
   FOR i = 1 TO long
      LET no_no[j,j] = no_t1[i,i]
      IF i > 1 THEN
         IF no_no[j,j] MATCHES "[AEIOU]" THEN
            DISPLAY ""
         ELSE
            IF no_no[j,j] = "¥" OR no_no[j,j] = "¤" THEN
               LET no_no = "X"
               EXIT FOR
            ELSE
               LET no_no = no_t1[i,i]
               EXIT FOR
            END IF
         END IF
      ELSE
           DISPLAY ""
      END IF
   END FOR
}
##fecha nacimiento
   LET fena1 = fena USING "YYMMDD"

## cve_cur
   LET cve_cur = patmatnom CLIPPED, fena1 CLIPPED

   RETURN cve_cur

END FUNCTION

FUNCTION llena_comision_tras()
  DEFINE   cat       RECORD
              afore_x      CHAR(03),
              comis_afo    DECIMAL(5,2)
           END RECORD,

           llena_com   RECORD
               afore_a,  afore_b,
               afore_c,  afore_d,
               afore_e,  afore_f,
               afore_g,  afore_h,
               afore_i,  afore_j,
               afore_k,  afore_l,
               afore_m,  afore_n,
               afore_o,  afore_p,
               afore_q,  afore_r,
               afore_s,  afore_t,
               afore_u,  afore_v,
               afore_w                  CHAR(20),
               comis_a,  comis_b,
               comis_c,  comis_d,
               comis_e,  comis_f,
               comis_g,  comis_h,
               comis_i,  comis_j,
               comis_k,  comis_l,
               comis_m,  comis_n,
               comis_o,  comis_p,
               comis_q,  comis_r,
               comis_s,  comis_t,
               comis_u,  comis_v,
               comis_w                  CHAR(07)

           END RECORD,

           j, i, long      SMALLINT,
           afore           CHAR(20),
           comision        CHAR(07),
           cuantos         SMALLINT,
           max_fecha       DATE,
           afo_pro         CHAR(20),
           com_pro         CHAR(07),
           enter           CHAR(01)

   INITIALIZE llena_com.* TO NULL

   SELECT MAX(a.fecha) INTO max_fecha FROM tab_comparativo a
   WHERE  a.tipo_comparativo = 1

   LET cuantos = 0
   SELECT COUNT(*) INTO cuantos
   FROM   tab_comparativo a
   WHERE  a.fecha = max_fecha
   AND    a.tipo_comparativo = 1
   IF cuantos < 16 THEN
      PROMPT "No estan completas las Afores en el Comparativo, <Enter> "
      FOR enter
      RETURN llena_com.*,1
   END IF

   INITIALIZE afo_pro, com_pro TO NULL
   LET j = 1

   DECLARE apt_comis_trasp CURSOR FOR
      SELECT a.afore_cod, a.porcentaje
      FROM   tab_comparativo a
      WHERE  a.fecha = max_fecha
        AND  a.tipo_comparativo = 1
      ORDER BY a.porcentaje
   FOREACH apt_comis_trasp INTO cat.*

      INITIALIZE afore, comision TO NULL

      IF cat.afore_x = "999" THEN
         LET afo_pro = "PROMEDIO            "
         LET com_pro = cat.comis_afo USING "##&.&&","%"
         CONTINUE FOREACH
      END IF

      SELECT m.afore_desc INTO afore FROM tab_afore m
      WHERE  m.afore_cod = cat.afore_x

      LET long = 0    LET i = 0
      LET long = LENGTH(afore)
      IF long = 0 THEN
         LET afore = "                    "
      ELSE
             IF long < 20 THEN
                LET long = long + 1
                FOR i = long TO 20
                    LET afore[i,i] = " "
                END FOR
             END IF
      END IF

      LET comision = cat.comis_afo USING "##&.&&","%"

      CASE j
           WHEN 1  LET llena_com.afore_a = afore
                   LET llena_com.comis_a = comision
           WHEN 2  LET llena_com.afore_b = afore
                   LET llena_com.comis_b = comision
           WHEN 3  LET llena_com.afore_c = afore
                   LET llena_com.comis_c = comision
           WHEN 4  LET llena_com.afore_d = afore
                   LET llena_com.comis_d = comision
           WHEN 5  LET llena_com.afore_e = afore
                   LET llena_com.comis_e = comision
           WHEN 6  LET llena_com.afore_f = afore
                   LET llena_com.comis_f = comision
           WHEN 7  LET llena_com.afore_g = afore
                   LET llena_com.comis_g = comision
           WHEN 8  LET llena_com.afore_h = afore
                   LET llena_com.comis_h = comision
           WHEN 9  LET llena_com.afore_i = afore
                   LET llena_com.comis_i = comision
           WHEN 10 LET llena_com.afore_j = afore
                   LET llena_com.comis_j = comision
           WHEN 11 LET llena_com.afore_k = afore
                   LET llena_com.comis_k = comision
           WHEN 12 LET llena_com.afore_l = afore
                   LET llena_com.comis_l = comision
           WHEN 13 LET llena_com.afore_m = afore
                   LET llena_com.comis_m = comision
           WHEN 14 LET llena_com.afore_n = afore
                   LET llena_com.comis_n = comision
           WHEN 15 LET llena_com.afore_o = afore
                   LET llena_com.comis_o = comision
           WHEN 16 LET llena_com.afore_p = afore
                   LET llena_com.comis_p = comision
           WHEN 17 LET llena_com.afore_q = afore
                   LET llena_com.comis_q = comision
           WHEN 18 LET llena_com.afore_r = afore
                   LET llena_com.comis_r = comision
           WHEN 19 LET llena_com.afore_s = afore
                   LET llena_com.comis_s = comision
           WHEN 20 LET llena_com.afore_t = afore
                   LET llena_com.comis_t = comision
           WHEN 21 LET llena_com.afore_u = afore
                   LET llena_com.comis_u = comision
           WHEN 22 LET llena_com.afore_v = afore
                   LET llena_com.comis_v = comision
           WHEN 23 LET llena_com.afore_w = afore
                   LET llena_com.comis_w = comision
      END CASE


      LET j = j + 1
   END FOREACH

   CASE j
        WHEN 16 LET llena_com.afore_p = afo_pro
                LET llena_com.comis_p = com_pro
        WHEN 17 LET llena_com.afore_q = afo_pro
                LET llena_com.comis_q = com_pro
        WHEN 18 LET llena_com.afore_r = afo_pro
                LET llena_com.comis_r = com_pro
        WHEN 19 LET llena_com.afore_s = afo_pro
                LET llena_com.comis_s = com_pro
        WHEN 20 LET llena_com.afore_t = afo_pro
                LET llena_com.comis_t = com_pro
        WHEN 21 LET llena_com.afore_u = afo_pro
                LET llena_com.comis_u = com_pro
        WHEN 22 LET llena_com.afore_v = afo_pro
                LET llena_com.comis_v = com_pro
        WHEN 23 LET llena_com.afore_w = afo_pro
                LET llena_com.comis_w = com_pro
   END CASE

   IF llena_com.afore_q IS NULL THEN
         LET llena_com.afore_q = "                    "
         LET llena_com.comis_q = "       "
   END IF
   IF llena_com.afore_r IS NULL THEN
         LET llena_com.afore_r = "                    "
         LET llena_com.comis_r = "       "
   END IF
   IF llena_com.afore_s IS NULL THEN
         LET llena_com.afore_s = "                    "
         LET llena_com.comis_s = "       "
   END IF
   IF llena_com.afore_t IS NULL THEN
         LET llena_com.afore_t = "                    "
         LET llena_com.comis_t = "       "
   END IF
   IF llena_com.afore_u IS NULL THEN
         LET llena_com.afore_u = "                    "
         LET llena_com.comis_u = "       "
   END IF
   IF llena_com.afore_v IS NULL THEN
         LET llena_com.afore_v = "                    "
         LET llena_com.comis_v = "       "
   END IF
   IF llena_com.afore_w IS NULL THEN
         LET llena_com.afore_w = "                    "
         LET llena_com.comis_w = "       "
   END IF
   RETURN llena_com.*,0
END FUNCTION
###############################################################################
FUNCTION arma_clave(paterno, materno, nombres, fena, estadon, sexo)
#ac----------------------------------------------------------------
   DEFINE paterno, materno, nombres     CHAR(40),
          fena                          DATE    ,
          sexo                          SMALLINT,
          estadon                       SMALLINT,
          sexo1                         CHAR(01),
          fena1                         CHAR(06),
          pa_t1, ma_t1, no_t1           CHAR(40),
          pa_t,  ma_t,  no_t            CHAR(02),
          pater,  pater1, pater2        CHAR(40),
          pater3, pater4, pater5        CHAR(40),
          pa_papa, ma_mama              CHAR(40),
          patmat                        CHAR(03),
          mater , mater1, mater2,
          mater3, mater4, mater5        CHAR(40),
          patmatnom, patmatnom1         CHAR(04),
          nom_b , nom_b1, nom_b2,
          nom_b3, nom_b4, nom_b5        CHAR(40),
          cve_mex                       CHAR(02),
          ent_fed1                      CHAR(02),
          cve_cur                       CHAR(17),
          pa_pa, ma_ma, no_no           CHAR(01),
          ch_ll                         CHAR(02),
          consonante                    CHAR(03),

          bla, ban, i, long, bb, j      SMALLINT,
          enter                         CHAR(1)

   INITIALIZE pa_t1, ma_t1, no_t1 TO NULL
   INITIALIZE pa_t,  ma_t,  no_t  TO NULL
   INITIALIZE pa_pa, ma_ma, no_no, consonante, ch_ll TO NULL
   INITIALIZE pater, pater1, pater2, pater3, pater4, pater5, pa_papa  TO NULL

   LET long = 0  LET i = 0   LET bb = 0   LET j = 0

##paterno

   LET j = 1
   LET pa_t1 = paterno CLIPPED
   LET long = LENGTH(pa_t1 CLIPPED)
   LET long = long + 1

   FOR i = 1 TO long
       IF pa_t1[i,i] = " " THEN
          LET bla = bla + 1
          CASE bla
             WHEN 1
                    LET pater1 = pater
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = pater1
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 1
                       EXIT FOR
                    END IF
             WHEN 2
                    LET pater2 = pater
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = pater2
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 2
                       EXIT FOR
                    END IF
             WHEN 3
                    LET pater3 = pater
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = pater3
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 3
                       EXIT FOR
                    END IF
             WHEN 4
                    LET pater4 = pater
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = pater4
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 4
                       EXIT FOR
                    END IF
             WHEN 5
                    LET pater5 = pater
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = pater5
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 5
                       EXIT FOR
                    END IF
          END CASE
       ELSE
           LET pater[j,j] = pa_t1[i,i]
           LET bb = 6
           LET j = j + 1
       END IF
   END FOR

   CASE bb
        WHEN 1 LET pa_t1 = pater1
        WHEN 2 LET pa_t1 = pater2
        WHEN 3 LET pa_t1 = pater3
        WHEN 4 LET pa_t1 = pater4
        WHEN 5 LET pa_t1 = pater5
        WHEN 6 LET pa_t1 = pater
   END CASE
   IF pa_t1 IS NULL OR pa_t1 = " " THEN
      LET pa_t1 = pater1 CLIPPED
   END IF

   LET pa_papa = pa_t1 CLIPPED

   LET j = 1
   FOR i = 1 TO long
       IF j = 1 THEN
          LET pa_t[j,j] = pa_t1[i,i]
          IF pa_t[j,j] = "Ã" OR pa_t[j,j] = "Ã±" THEN
             LET pa_t[j,j] = "X"
          END IF
          IF pa_t[j,j] = "C" OR pa_t[j,j] = "L" THEN
             LET ch_ll[j,j] = pa_t[j,j]
          END IF
          LET j = j + 1

       ELSE

          IF pa_t1[i,i] MATCHES "[AEIOU]" THEN
             LET pa_t[j,j] = pa_t1[i,i]

             IF j = 2 THEN
                EXIT FOR
             END IF
{
          ELSE
             LET ch_ll[j,j] = pa_t1[i,i]
             IF ch_ll = "CH" OR ch_ll[j,j] = "LL" THEN
                LET j = 2
             ELSE
                 LET pa_t[j,j] = pa_t1[i,i]
                 IF j = 2 THEN
                    EXIT FOR
                 END IF
             END IF
}
          END IF
       END IF
   END FOR

   LET j = 1
   FOR i = 1 TO long
      LET pa_pa[j,j] = pa_papa[i,i]
      IF i > 1 THEN
         IF pa_pa[j,j] NOT MATCHES "[AEIOU]" THEN
            IF pa_pa[j,j] = "Ã" OR pa_pa[j,j] = "Ã±" THEN
               LET pa_pa = "X"
               EXIT FOR
            ELSE
               LET pa_pa = pa_papa[i,i]
               EXIT FOR
            END IF
         END IF
      END IF
   END FOR

## materno
   INITIALIZE mater, mater1, mater2, mater3, mater4, mater5  TO NULL

   LET long = 0  LET i = 0   LET bb = 0  LET bla = 0  LET j = 1

   LET ma_t1 = materno CLIPPED
   LET long = LENGTH(ma_t1 CLIPPED)

   IF long IS NULL OR long = 0 THEN
      LET ma_t = "X"
      LET patmat = pa_t CLIPPED, ma_t CLIPPED
      LET ma_ma = "X"

   ELSE
         FOR i = 1 TO long
             IF ma_t1[i,i] = " " THEN
                LET bla = bla + 1
                CASE bla
                   WHEN 1
                          LET mater1 = mater CLIPPED
                          SELECT "X" FROM afi_articulo
                                  WHERE palabra = mater1
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 1
                             EXIT FOR
                          END IF
                   WHEN 2
                          LET mater2 = mater CLIPPED
                          SELECT "X" FROM afi_articulo
                                  WHERE palabra = mater2
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 2
                             EXIT FOR
                          END IF
                   WHEN 3
                          LET mater3 = mater CLIPPED
                          SELECT "X" FROM afi_articulo
                                  WHERE palabra = mater3
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 3
                             EXIT FOR
                          END IF
                   WHEN 4
                          LET mater4 = mater CLIPPED
                          SELECT "X" FROM afi_articulo
                               WHERE palabra = mater4
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 4
                             EXIT FOR
                          END IF
                   WHEN 5
                          LET mater5 = mater CLIPPED
                          SELECT "X" FROM afi_articulo
                                  WHERE palabra = mater5
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 5
                             EXIT FOR
                          END IF
                END CASE
             ELSE
                 LET mater[j,j] = ma_t1[i,i]
                 LET bb = 6
                 LET j  = j + 1
             END IF
         END FOR

         CASE bb
              WHEN 1 LET ma_t1 = mater1
              WHEN 2 LET ma_t1 = mater2
              WHEN 3 LET ma_t1 = mater3
              WHEN 4 LET ma_t1 = mater4
              WHEN 5 LET ma_t1 = mater5
              WHEN 6 LET ma_t1 = mater
         END CASE
         IF ma_t1  IS NULL OR ma_t1 = " " THEN
            LET ma_t1 = mater1
         END IF

         LET ma_mama = ma_t1 CLIPPED
         FOR i = 1 TO long
             IF i = 1 THEN
                LET ma_t[i,i] = ma_t1[i,i]
                IF ma_t[i,i] = "Ã" OR ma_t[i,i] = "Ã±" THEN
                   LET ma_t[i,i] = "X"
                END IF
                EXIT FOR
             END IF
         END FOR

         LET j = 1
         FOR i = 1 TO long
            LET ma_ma[j,j] = ma_mama[i,i]
            IF i > 1 THEN
               IF ma_ma[j,j] NOT MATCHES "[AEIOU]" THEN
                  IF ma_ma[j,j] = "Ã" OR ma_ma[j,j] = "Ã±" THEN
                     LET ma_ma = "X"
                     EXIT FOR
                  ELSE
                     LET ma_ma = ma_mama[i,i]
                     EXIT FOR
                  END IF
               END IF
            END IF
         END FOR
   END IF

## nombres
   INITIALIZE nom_b, nom_b1, nom_b2, nom_b3, nom_b4, nom_b5  TO NULL

   LET long = 0  LET i = 0   LET bb = 0  LET bla = 0  LET j = 1

   LET no_t1 = nombres CLIPPED
   LET long = LENGTH(no_t1 CLIPPED)
##   LET long = long + 1
   FOR i = 1 TO long
       IF no_t1[i,i] = " " THEN
          LET bla = bla + 1
          CASE bla
             WHEN 1
                    LET nom_b1 = nom_b CLIPPED
                    LET nom_b1 = nom_b1 CLIPPED
                    SELECT "f.X" FROM afi_articulo f
                            WHERE f.palabra MATCHES nom_b1
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE nom_b TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 1
                       EXIT FOR
                    END IF
             WHEN 2
                    LET nom_b2 = nom_b CLIPPED
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = nom_b2
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE nom_b TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 2
                       EXIT FOR
                    END IF
             WHEN 3
                    LET nom_b3 = nom_b CLIPPED
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = nom_b3
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE nom_b TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 3
                       EXIT FOR
                    END IF
             WHEN 4
                    LET nom_b4 = nom_b CLIPPED
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = nom_b4
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE nom_b TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 4
                       EXIT FOR
                    END IF
             WHEN 5
                    LET nom_b5 = nom_b CLIPPED
                    SELECT "X" FROM afi_articulo
                            WHERE palabra = nom_b5
                    IF STATUS != NOTFOUND THEN
                       LET j = 1
                       INITIALIZE nom_b TO NULL
                    ELSE
                       LET bb = 5
                       EXIT FOR
                    END IF
          END CASE
       ELSE
           LET nom_b[j,j] = no_t1[i,i]
           LET bb = 6
           LET j  = j + 1
       END IF
   END FOR

   CASE bb
        WHEN 1 LET no_t1 = nom_b1
        WHEN 2 LET no_t1 = nom_b2
        WHEN 3 LET no_t1 = nom_b3
        WHEN 4 LET no_t1 = nom_b4
        WHEN 5 LET no_t1 = nom_b5
        WHEN 6 LET no_t1 = nom_b
   END CASE

   IF no_t1 IS NULL OR no_t1 = " " THEN
      LET nom_b1 = no_t1
   END IF

   FOR i = 1 TO long
     IF i = 1 THEN
        LET no_t[i,i] = no_t1[i,i]
        IF no_t[i,i] = "Ã" OR no_t[i,i] = "Ã±" THEN
           LET no_t[i,i] = "X"
        END IF
        EXIT FOR
     END IF
   END FOR
   LET patmatnom = pa_t CLIPPED, ma_t CLIPPED, no_t CLIPPED

   SELECT b.palabra_si INTO patmatnom1 FROM afi_no_conviene b
          WHERE palabra_no = patmatnom
   IF STATUS != NOTFOUND THEN
      LET patmatnom = patmatnom1 CLIPPED
   END IF

   LET j = 1
   FOR i = 1 TO long
      LET no_no[j,j] = no_t1[i,i]
      IF i > 1 THEN
         IF no_no[j,j] MATCHES "[AEIOU]" THEN
            DISPLAY ""
         ELSE
            IF no_no[j,j] = "Â¥" OR no_no[j,j] = "Â¤" THEN
               LET no_no = "X"
               EXIT FOR
            ELSE
               LET no_no = no_t1[i,i]
               EXIT FOR
            END IF
         END IF
      ELSE
           DISPLAY ""
      END IF
   END FOR

##fecha nacimiento
   LET fena1 = fena USING "YYMMDD"
##sexo
   CASE sexo
     WHEN 1 LET sexo1 = "H"
     WHEN 2 LET sexo1 = "M"
   END CASE

##ent. federativa
   SELECT a.estad_ren INTO ent_fed1 FROM tab_edo_norma a
          WHERE a.estad_cod = estadon
   IF STATUS = NOTFOUND THEN
      LET ent_fed1 = "  "
   END IF


## consonantes
 LET consonante = pa_pa CLIPPED, ma_ma CLIPPED, no_no CLIPPED

## cve_cur
   LET cve_cur = patmatnom CLIPPED, fena1 CLIPPED, sexo1 CLIPPED,
                 ent_fed1  CLIPPED, consonante CLIPPED

   RETURN cve_cur
END FUNCTION
###################################################################
# FUNCION PARA CONSTANCIAS DE REGISTROS
###################################################################
FUNCTION llena_comision_registro()
  DEFINE   cat       RECORD
              afore_x      CHAR(03),
              comis_afo    DECIMAL(5,2)
           END RECORD,

           llena_com   RECORD
               afore_a,  afore_b,
               afore_c,  afore_d,
               afore_e,  afore_f,
               afore_g,  afore_h,
               afore_i,  afore_j,
               afore_k,  afore_l,
               afore_m,  afore_n,
               afore_o,  afore_p,
               afore_q,  afore_r,
               afore_s,  afore_t,
               afore_u,  afore_v,
               afore_w,  afore_x,
               afore_y,  afore_z        CHAR(20),
               comis_a,  comis_b,
               comis_c,  comis_d,
               comis_e,  comis_f,
               comis_g,  comis_h,
               comis_i,  comis_j,
               comis_k,  comis_l,
               comis_m,  comis_n,
               comis_o,  comis_p,
               comis_q,  comis_r,
               comis_s,  comis_t,
               comis_u,  comis_v,
               comis_w,  comis_x,
               comis_y,  comis_z        CHAR(07)

           END RECORD,

           j, i, long      SMALLINT,
           afore           CHAR(20),
           comision        CHAR(07),
           cuantos         SMALLINT,
           max_fecha       DATE,
           afo_pro         CHAR(20),
           com_pro         CHAR(07),
           enter           CHAR(01)

   INITIALIZE llena_com.* TO NULL

   SELECT MAX(a.fecha) INTO max_fecha FROM tab_comparativo a
   WHERE  a.tipo_comparativo = 1

   LET cuantos = 0
   SELECT COUNT(*) INTO cuantos
   FROM   tab_comparativo a
   WHERE  a.fecha = max_fecha
   AND    a.tipo_comparativo = 1
   IF cuantos < 16 THEN
      PROMPT "No estan completas las Afores en el Comparativo, <Enter> "
      FOR enter
      RETURN llena_com.*,1
   END IF

   INITIALIZE afo_pro, com_pro TO NULL
   LET j = 1

   DECLARE apt_comis_reg CURSOR FOR
      SELECT a.afore_cod, a.porcentaje
      FROM   tab_comparativo a
      WHERE  a.fecha = max_fecha
        AND  a.tipo_comparativo = 1
      ORDER BY a.porcentaje
   FOREACH apt_comis_reg INTO cat.*

      INITIALIZE afore, comision TO NULL

      IF cat.afore_x = "999" THEN
         LET afo_pro = "PROMEDIO            "
         LET com_pro = cat.comis_afo USING "##&.&&","%"
         CONTINUE FOREACH
      END IF

      SELECT m.afore_desc INTO afore FROM tab_afore m
      WHERE  m.afore_cod = cat.afore_x

      LET long = 0    LET i = 0
      LET long = LENGTH(afore)
      IF long = 0 THEN
         LET afore = "                    "
      ELSE
             IF long < 20 THEN
                LET long = long + 1
                FOR i = long TO 20
                    LET afore[i,i] = " "
                END FOR
             END IF
      END IF

      LET comision = cat.comis_afo USING "##&.&&","%"

      CASE j
           WHEN 1  LET llena_com.afore_a = afore
                   LET llena_com.comis_a = comision
           WHEN 2  LET llena_com.afore_b = afore
                   LET llena_com.comis_b = comision
           WHEN 3  LET llena_com.afore_c = afore
                   LET llena_com.comis_c = comision
           WHEN 4  LET llena_com.afore_d = afore
                   LET llena_com.comis_d = comision
           WHEN 5  LET llena_com.afore_e = afore
                   LET llena_com.comis_e = comision
           WHEN 6  LET llena_com.afore_f = afore
                   LET llena_com.comis_f = comision
           WHEN 7  LET llena_com.afore_g = afore
                   LET llena_com.comis_g = comision
           WHEN 8  LET llena_com.afore_h = afore
                   LET llena_com.comis_h = comision
           WHEN 9  LET llena_com.afore_i = afore
                   LET llena_com.comis_i = comision
           WHEN 10 LET llena_com.afore_j = afore
                   LET llena_com.comis_j = comision
           WHEN 11 LET llena_com.afore_k = afore
                   LET llena_com.comis_k = comision
           WHEN 12 LET llena_com.afore_l = afore
                   LET llena_com.comis_l = comision
           WHEN 13 LET llena_com.afore_m = afore
                   LET llena_com.comis_m = comision
           WHEN 14 LET llena_com.afore_n = afore
                   LET llena_com.comis_n = comision
           WHEN 15 LET llena_com.afore_o = afore
                   LET llena_com.comis_o = comision
           WHEN 16 LET llena_com.afore_p = afore
                   LET llena_com.comis_p = comision
           WHEN 17 LET llena_com.afore_q = afore
                   LET llena_com.comis_q = comision
           WHEN 18 LET llena_com.afore_r = afore
                   LET llena_com.comis_r = comision
           WHEN 19 LET llena_com.afore_s = afore
                   LET llena_com.comis_s = comision
           WHEN 20 LET llena_com.afore_t = afore
                   LET llena_com.comis_t = comision
           WHEN 21 LET llena_com.afore_u = afore
                   LET llena_com.comis_u = comision
           WHEN 22 LET llena_com.afore_v = afore
                   LET llena_com.comis_v = comision
           WHEN 23 LET llena_com.afore_w = afore
                   LET llena_com.comis_w = comision
           WHEN 24 LET llena_com.afore_x = afore
                   LET llena_com.comis_x = comision
           WHEN 25 LET llena_com.afore_y = afore
                   LET llena_com.comis_y = comision
           WHEN 26 LET llena_com.afore_z = afore
                   LET llena_com.comis_z = comision
      END CASE


      LET j = j + 1
   END FOREACH

   CASE j
        WHEN 16 LET llena_com.afore_p = afo_pro
                LET llena_com.comis_p = com_pro
        WHEN 17 LET llena_com.afore_q = afo_pro
                LET llena_com.comis_q = com_pro
        WHEN 18 LET llena_com.afore_r = afo_pro
                LET llena_com.comis_r = com_pro
        WHEN 19 LET llena_com.afore_s = afo_pro
                LET llena_com.comis_s = com_pro
        WHEN 20 LET llena_com.afore_t = afo_pro
                LET llena_com.comis_t = com_pro
        WHEN 21 LET llena_com.afore_u = afo_pro
                LET llena_com.comis_u = com_pro
        WHEN 22 LET llena_com.afore_v = afo_pro
                LET llena_com.comis_v = com_pro
        WHEN 23 LET llena_com.afore_w = afo_pro
                LET llena_com.comis_w = com_pro
        WHEN 24 LET llena_com.afore_x = afo_pro
                LET llena_com.comis_x = com_pro
        WHEN 25 LET llena_com.afore_y = afo_pro
                LET llena_com.comis_y = com_pro
        WHEN 26 LET llena_com.afore_z = afo_pro
                LET llena_com.comis_z = com_pro
   END CASE

   IF llena_com.afore_q IS NULL THEN
         LET llena_com.afore_q = "                    "
         LET llena_com.comis_q = "       "
   END IF
   IF llena_com.afore_r IS NULL THEN
         LET llena_com.afore_r = "                    "
         LET llena_com.comis_r = "       "
   END IF
   IF llena_com.afore_s IS NULL THEN
         LET llena_com.afore_s = "                    "
         LET llena_com.comis_s = "       "
   END IF
   IF llena_com.afore_t IS NULL THEN
         LET llena_com.afore_t = "                    "
         LET llena_com.comis_t = "       "
   END IF
   IF llena_com.afore_u IS NULL THEN
         LET llena_com.afore_u = "                    "
         LET llena_com.comis_u = "       "
   END IF
   IF llena_com.afore_v IS NULL THEN
         LET llena_com.afore_v = "                    "
         LET llena_com.comis_v = "       "
   END IF
   IF llena_com.afore_w IS NULL THEN
         LET llena_com.afore_w = "                    "
         LET llena_com.comis_w = "       "
   END IF
   IF llena_com.afore_x IS NULL THEN
         LET llena_com.afore_x = "                    "
         LET llena_com.comis_x = "       "
   END IF
   IF llena_com.afore_y IS NULL THEN
         LET llena_com.afore_y = "                    "
         LET llena_com.comis_y = "       "
   END IF
   IF llena_com.afore_z IS NULL THEN
         LET llena_com.afore_z = "                    "
         LET llena_com.comis_z = "       "
   END IF
   RETURN llena_com.*,0
END FUNCTION
###############################################################################
