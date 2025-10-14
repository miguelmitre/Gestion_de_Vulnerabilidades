
#Owner             => E.F.P.                                                   #
#Programa INT_GLOB => FUNCIONES (fin_mes, mes_h, formatea_campo, bimestre      #
#                  => formatea_numero, es_numerico, izqui_texto, derec_texto,  #
#                  => r_report_db, valida_campo, valida_fecha, marca_rechazo,  #
#                  => fecha_mes, valida_nss, valida_promotor, digito_verif )   #
#                  => actualiza_bat_ctr                                        #
#Fecha creacion    => 17 DE DICIEMBRE DEL 2001                                 #
#By                => FRANCO ESTEBAN ULLOA VIDELA                              #
#Actualizacion     =>                                                          #
#Fecha actualiz.   =>                                                          #
#Sistema           => MIG                                                      #
################################################################################

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
{
FUNCTION actualiza_bat_ctr(f_pid,f_proceso_cod,f_opera_cod,f_folio)
#ab---------------------
   DEFINE
           f_pid                  INTEGER,
           f_proceso_cod          INTEGER,
           f_opera_cod            INTEGER,
           f_folio                INTEGER

   DEFINE  v_cat                  CHAR(600),
           vv_fecha_log           CHAR(030),
           vv_prog                CHAR(010),
           paso                   CHAR(100)
   DEFINE  curr  char(30)
   DEFINE  v_fecha_log   DATETIME  YEAR  TO  SECOND

   DEFINE  reg_ruta      RECORD  LIKE safre_af:seg_modulo.*
   SELECT  A.*
     INTO  reg_ruta.*
     FROM  safre_af:seg_modulo A
    WHERE  modulo_cod         = "bat"
 
   UPDATE  safre_af:bat_ctr_operacion
      SET  folio              =  1000 ,      
	   estado_cod         =  4    ,
 	   fecha_fin          =  CURRENT ,
           folio              =  f_folio
    WHERE  pid                =  f_pid
      AND  proceso_cod        =  f_proceso_cod
      AND  opera_cod          =  f_opera_cod


   UPDATE  safre_af:bat_tmp_predecesor
      SET  bandera_ejecuta    =  1
    WHERE  pid_prod           =  f_pid
      AND  proceso_cod_prod   =  f_proceso_cod
      AND  opera_cod_prod     =  f_opera_cod

   LET     v_fecha_log        =  CURRENT
   LET     vv_fecha_log       =  v_fecha_log

   SELECT  A.programa_cod 
     INTO  vv_prog 
     FROM  safre_af:bat_ctr_operacion A
    WHERE  A.pid              =  f_pid
      AND  A.proceso_cod      =  f_proceso_cod
      AND  A.opera_cod        =  f_opera_cod
           
   IF      vv_prog            =  "TAAB007"     THEN
           UPDATE  safre_af:bat_ctr_proceso
              SET  folio              =  f_folio ,      
	           estado_cod         =  4    ,
	           fecha_fin          =  CURRENT
            WHERE  pid                =  f_pid
              AND  proceso_cod        =  f_proceso_cod
   END IF
   LET     paso               = "nohup:"            ,
           f_pid               USING  "&&&&&",":",
           f_proceso_cod       USING  "&&&&&",":",
           f_opera_cod         USING  "&&&&&"

   LET     v_cat              = "echo '"                ,
           vv_fecha_log[1,4]       ,   
           vv_fecha_log[6,7]       ,  
           vv_fecha_log[9,10]      ,  
           vv_fecha_log[12,13]     ,   
           vv_fecha_log[15,16]     ,    
           vv_fecha_log[18,19]     ,
           "|"                     ,
           vv_prog  CLIPPED        ,
           "|"                     ,
           "FINOK"                 ,
           "|"                     ,
           reg_ruta.ruta_listados CLIPPED,  #/safre_lst
           "/"                     ,
           paso CLIPPED            ,
           "'"                     ,
           " >> "                  ,
           reg_ruta.ruta_envio CLIPPED ,#/safre_prc/bat/envio
           "/"                     ,
           "aad_safre.out"

   LET     v_cat             = v_cat CLIPPED
   RUN     v_cat
END FUNCTION
}

FUNCTION actualiza_operacion(f_pid,f_proceso_cod,f_opera_cod,f_folio)
#ao---------------------------------
   DEFINE
           f_pid                  ,
           f_proceso_cod          ,
           f_opera_cod            ,
           f_folio                INTEGER

DEFINE txt    CHAR(600)
DEFINE cadena CHAR(600)

LET txt  = "EXECUTE PROCEDURE safre_af:actualiza_batch(",
           "'",f_pid         USING"&&&&&","',",
           "'",f_proceso_cod USING"&&&&&","',",
           "'",f_opera_cod   USING"&&&&&","',",
           f_folio                ,")"
          
LET txt = txt CLIPPED

PREPARE qry FROM txt
DECLARE cur_bat CURSOR FOR qry
FOREACH cur_bat INTO cadena
END FOREACH
RUN cadena

END FUNCTION      

FUNCTION genera_tmp_cuenta (p_nss  ,
                            p_fecha_ini ,
                            p_fecha_fin )

   DEFINE p_nss           CHAR(11),
          p_fecha_ini             ,
          p_fecha_fin     DATE    

   DEFINE v_nombre_tabla  CHAR(20) ,
          v_anio                   ,
          v_anio_ini               ,
          v_anio_fin      SMALLINT

   DEFINE v_anio_c        CHAR(02) ,
          sel_his         CHAR(1000)

   DEFINE v_fecha         DATE

   WHENEVER ERROR CONTINUE
      DROP TABLE tmp_dis_cuenta;
   WHENEVER ERROR STOP

   LET v_anio_ini = YEAR( p_fecha_ini )
   LET v_anio_fin = YEAR( p_fecha_fin )


   FOR v_anio = v_anio_ini TO v_anio_fin

      LET v_fecha  = MDY(1,1,v_anio)
      LET v_anio_c = v_fecha USING "YY"

      LET v_nombre_tabla = "dis_cuenta",v_anio_c CLIPPED

      SELECT "tabla"
      FROM   systables
      WHERE  tabname = v_nombre_tabla

      IF SQLCA.SQLCODE = 0 THEN

         LET sel_his = sel_his CLIPPED,
                     " SELECT * ",
                     " FROM  ",v_nombre_tabla          ,
                     " WHERE nss = ","'",p_nss,"'"  ,
                     " UNION ALL "
      END IF
   END FOR

   LET sel_his = sel_his CLIPPED,
               " SELECT * ",
               " FROM dis_cuenta ",
               " WHERE nss = ","'",p_nss,"'"  ,
               " AND fecha_conversion <= ","'",p_fecha_fin,"'",
               " INTO TEMP tmp_dis_cuenta "

--   PREPARE eje_prioridad FROM "SET PDQPRIORITY HIGH"
--   EXECUTE eje_prioridad
   PREPARE eje_sel_his FROM sel_his
   EXECUTE eje_sel_his

   CREATE INDEX tmp_dis_cuenta1 on tmp_dis_cuenta ( tipo_movimiento )
   UPDATE STATISTICS FOR TABLE tmp_dis_cuenta

   DELETE 
   FROM  tmp_dis_cuenta
   WHERE tipo_movimiento = 999

   CREATE INDEX tmp_dis_cuenta2 on tmp_dis_cuenta ( fecha_conversion )
   UPDATE STATISTICS FOR TABLE tmp_dis_cuenta

END FUNCTION

FUNCTION    f_910_totales_por_folio(i_tabla,i_folio,i_fecha_conversion)
   DEFINE   l_today                      DATE
   DEFINE   l_enter   char(01)
   DEFINE   l_provision                  CHAR(13)
   DEFINE   i_tabla                      CHAR(50),
            i_folio                      LIKE  dis_cuenta.folio,
            i_fecha_conversion           LIKE  dis_cuenta.fecha_conversion,
            l_sql                              CHAR(500)
   DEFINE   ind                               ,
            g_lastkey                         ,
            l_arr_count                       ,
            l_scr_line                        ,
            l_arr_curr                        SMALLINT
   DEFINE   l_tot_sie           ARRAY[100]   OF    RECORD
            siefore                         LIKE  dis_cuenta.siefore,
            siefore_desc                    CHAR(15),
            precio_accion                   LIKE  dis_cuenta.precio_accion,
            monto_en_acciones               LIKE  dis_cuenta.monto_en_acciones,
            monto_en_pesos                  LIKE  dis_cuenta.monto_en_pesos
                                            END   RECORD
   LET      l_today                        =  TODAY
   LET      l_provision                    =  " LIQUIDADOS "
   IF       i_tabla[1,8]                   =  "dis_prov"     THEN
            LET       l_provision          =  "PROVISIONADOS"
   END IF
   LET      ind                            =  1
   LET      l_sql                          =
          ' SELECT  b.siefore,0,0,',
          '         SUM(b.monto_en_acciones),SUM(b.monto_en_pesos) ',
          '  FROM  ', i_tabla,
          ' WHERE  b.folio           = ',i_folio,
          '   AND  b.estado          =  6 ',
          ' GROUP BY 1,2,3 ',
          ' ORDER BY 1,2,3 '    CLIPPED
   PREPARE  sql_tot_sie       FROM  l_sql
   DECLARE  cur_tot_sis     CURSOR  FOR    sql_tot_sie
   FOREACH  cur_tot_sis       INTO   l_tot_sie[ind].*
        SELECT  razon_social
          INTO  l_tot_sie[ind].siefore_desc
          FROM  safre_af:tab_siefore_local
         WHERE  codigo_siefore             =  l_tot_sie[ind].siefore;
        SELECT  precio_del_dia
          INTO  l_tot_sie[ind].precio_accion
          FROM  safre_af:glo_valor_accion
         WHERE  codigo_siefore             =  l_tot_sie[ind].siefore 
           AND  fecha_valuacion            =  i_fecha_conversion;
        LET     ind                        =  ind   +  1
   END  FOREACH
   LET      ind                            =  ind   -  1
   CALL     SET_COUNT( ind )
   OPEN     WINDOW   tot_sie       AT   02,02   WITH  FORM 
            "INTCGLO010"    ATTRIBUTE(BORDER)
   DISPLAY  "<<INTCGLO010>>   <<  TOTALES ",l_provision," POR  SIEFORE  >>    ",l_today USING "dd-mm-yyyy","       "    AT  1,1    ATTRIBUTE  (REVERSE)
   DISPLAY  " <Control-C>  Regresa a Consulta de Folios   "    AT  2,1
   DISPLAY  "FOLIO:                                                                                         "  AT  3,1  ATTRIBUTE(REVERSE,green)
   DISPLAY  i_folio      TO   folio  ATTRIBUTE(REVERSE,green)
   INPUT    ARRAY    l_tot_sie      WITHOUT   DEFAULTS    FROM    scr_sie.*
            BEFORE   ROW
                     LET      l_arr_curr          =  ARR_CURR()
                     LET      l_arr_count         =  ARR_COUNT()
                     LET      l_scr_line          =  SCR_LINE()
                     IF       l_arr_curr          >  l_arr_count    THEN
                              EXIT     INPUT
                     END IF
            AFTER    FIELD    siefore
                     IF       l_arr_curr         >=  (l_arr_count)  THEN
                              LET      g_lastkey  =  FGL_LASTKEY()
                              IF     ((g_lastkey  =  FGL_KEYVAL("down"))   OR
                                      (g_lastkey  =  FGL_KEYVAL("return")) OR
                                      (g_lastkey  =  FGL_KEYVAL("tab"))    OR
                                      (g_lastkey  =  FGL_KEYVAL("right"))) THEN
                                       ERROR    "  NO HAY MAS TOTALES.....     "
                                       NEXT     FIELD    siefore
                              END IF
                     END IF
             ON KEY  ( INTERRUPT )
                     EXIT     INPUT
   END INPUT
   CLEAR   FORM 
   CLOSE   WINDOW    tot_sie
END FUNCTION
#############################################################################
