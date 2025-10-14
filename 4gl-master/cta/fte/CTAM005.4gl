--************************************************************************
--*********** CTAM005  :  Programa que genera  informacion 
--***********          :  para estado de cuenta
--***********          :  Identifica cuota 
--***********          :  Obtiene saldos
--***********          :  Obtiene datos generales de NSS
--***********
--************************************************************************
DATABASE safre_af

DEFINE
   g_mensaje1           ,
   g_mensaje2           ,
   g_mensaje3           CHAR(100),
   g_mensaje4           CHAR(100),
   g_mensaje5           CHAR(100),
   g_usuario            CHAR(8)  ,
   g_respuesta          CHAR(001),
   enter                CHAR(01)

DEFINE
   g_cancela            SMALLINT
     
DEFINE
   g_fecha_corte        ,
   g_fecha_ini          ,
   g_fecha_lote         DATE

DEFINE
   hoy                  DATE

DEFINE
   g_ruta_uni_exp      CHAR(100)

DEFINE
   g_archivo           CHAR(200)


MAIN
   OPTIONS PROMPT LINE LAST
   DEFER INTERRUPT
   CALL inicio()
   CALL proceso_principal()
END MAIN


FUNCTION inicio()
   LET hoy = TODAY

   SELECT USER
   INTO  g_usuario
   FROM  systables
   WHERE tabid = 1

   SELECT ruta_envio
   INTO  g_archivo
   FROM  seg_modulo
   WHERE modulo_cod = "cta"
END FUNCTION


FUNCTION proceso_principal()
   OPEN WINDOW  w_menu AT 4,4 WITH FORM "CTAM0051" ATTRIBUTES(BORDER)

   DISPLAY "CTAM005 GENERA INFORMACION CUENTAS SALDO CERO  <Ctrl-C> SALIR   " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY HOY USING"DD-MM-YYYY" AT 3,64 ATTRIBUTE(REVERSE)

   MENU "SALDO CERO"
      COMMAND "Prepara" "Prepara Informacion Saldo CERO"
         CALL fn_lee_datos(1)
         CLEAR FORM
      COMMAND "Identifica" "Identifica Cuentas Saldo CERO"
         CALL fn_lee_datos(2)
         CLEAR FORM
      COMMAND "Genera archivo" "Generacion de archivo RSC.DDMMYYYY"
         CALL fn_lee_datos(4)
         CLEAR FORM
      COMMAND "Revisa ejecucion" "Despliega la ejecucion de los procesos."
         CALL fn_revisa_proceso()
         CLEAR FORM
      COMMAND KEY(M)"cifras Mes" "Despliega total mes Cuentas Saldo Cero,"
         CALL fn_revisa_registro(1)
         CLEAR FORM
      COMMAND KEY(C)"Cifras totales" "Despliega total Cuentas Saldo Cero,"
         CALL fn_revisa_registro(2)
         CLEAR FORM
      COMMAND "Salir" "Salir del Programa"
         EXIT MENU
   END MENU
END FUNCTION


FUNCTION fn_valida_proceso (p_fecha_corte, p_paso)
DEFINE
   p_paso         ,
   v_paso         ,
   v_paso_ant     ,
   v_status       SMALLINT

DEFINE
   p_fecha_corte  ,
   v_fecha_fin    DATE

   LET v_paso      = NULL
   LET v_fecha_fin = NULL

   LET v_paso_ant  = p_paso - 1

   LET v_status = 0


   IF v_paso_ant <> 0 THEN
       --  Verifica ejecucion del paso anterior

      SELECT etapa, fecha_fin
      INTO  v_paso, v_fecha_fin
      FROM  cta_ctr_saldo_cero
      WHERE fecha_corte = p_fecha_corte
      AND   etapa       = v_paso_ant

      IF v_paso IS NULL OR v_paso = 0 THEN
         ERROR "FALTA EJECUCION DEL PASO ANTERIOR. VERIFIQUE"
         SLEEP 3
         ERROR ""
         LET v_status = 1
      ELSE
         IF v_fecha_fin IS NULL OR v_fecha_fin = "12/31/1899" THEN
            ERROR "PROCESO ANTERIOR AUN NO FINALIZA."
            SLEEP 3
            ERROR ""
            LET v_status = 1
         END IF
      END IF
   END IF

   IF v_status = 0 THEN
      LET v_paso      = NULL
      LET v_fecha_fin = NULL

      ---  Verifica si ya se ha ejecutado el proceso anteriormente

      SELECT etapa, fecha_fin
      INTO  v_paso, v_fecha_fin
      FROM  cta_ctr_saldo_cero
      WHERE fecha_corte = p_fecha_corte
      AND   etapa       = p_paso

      IF v_paso = p_paso  THEN
         IF v_fecha_fin IS NULL OR v_fecha_fin = "12/31/1899" THEN
            ERROR "PROCESO YA EN EJECUCION, PARA ESTA FECHA."
            SLEEP 3
            ERROR ""
            LET v_status = 1
         ELSE
            ERROR "PROCESO YA EJECUTADO, PARA ESTA FECHA."
            SLEEP 3
            ERROR ""
            LET v_status = 1
         END IF
      END IF
   END IF

   RETURN v_status
END FUNCTION


FUNCTION fn_valida_saldo (p_fecha_corte, p_paso)
DEFINE
   p_paso         ,
   v_paso         ,
   v_status       SMALLINT

DEFINE
   p_fecha_corte  ,
   v_fecha_fin    DATE

   LET v_paso      = NULL
   LET v_fecha_fin = NULL
  
   LET v_status = 0
   
   ---***Verifica si existen cuentas con saldo cero

   SELECT "X" 
   FROM  safre_tmp:tmp_sdo_saldo_cero
   WHERE fecha_conversion = p_fecha_corte
   GROUP BY 1
 
   IF STATUS = NOTFOUND THEN
      ERROR "NO EXISTEN SALDOS, PARA ESTA FECHA"
      SLEEP 3
      ERROR "" 

      LET v_status = 1
   END IF
   
   RETURN v_status
END FUNCTION


FUNCTION fn_inserta_paso ( p_paso, p_fecha_corte )
DEFINE
   p_paso          SMALLINT

DEFINE
   p_fecha_corte   DATE

INSERT INTO cta_ctr_saldo_cero
VALUES ( p_fecha_corte,
         p_paso       ,
         CURRENT      ,
         NULL         ,
         USER
       )
END FUNCTION


FUNCTION fn_lee_datos(l_paso)
DEFINE
   l_paso      SMALLINT,
   v_status    SMALLINT

DEFINE
   v_comando   CHAR(150),
   v_comando1  CHAR(150)

   CASE l_paso
      WHEN 1
         LET g_mensaje1 = "ESTE PROCESO PREPARA INFORMACION PARA SALDO CERO."
      WHEN 2
         LET g_mensaje1 = "ESTE PROCESO IDENTIFICA NSS CON SALDO CERO."
      WHEN 4
         LET g_mensaje1 = "ESTE PROCESO GENERA EL ARCHIVO DE CUENTAS      ",
                          " CON SALDO CERO                        "
   END CASE

   LET g_mensaje2 = "INDIQUE LA FECHA DEL PERIODO:"

   LET g_mensaje5 = "INDIQUE LA FECHA DE LOTE    :"

   LET g_mensaje3 = "ESTA SEGURO DE CONTINUAR EJECUCION S/N:"

   LET g_cancela = FALSE

   LET v_status = 0

   INPUT BY NAME g_fecha_corte,
                 g_fecha_lote ,
                 g_respuesta

      BEFORE INPUT 
         DISPLAY BY NAME g_mensaje1,
                         g_mensaje2

         IF l_paso = 1 OR l_paso = 2 THEN
            DISPLAY BY NAME g_mensaje5
         END IF

      BEFORE FIELD g_fecha_corte
         DISPLAY "" TO formonly.g_mensaje3
         DISPLAY "" TO formonly.g_respuesta

      AFTER FIELD g_fecha_corte
         IF g_fecha_corte IS NULL THEN
            ERROR "DEBE INDICAR FECHA DE CORTE" 
            SLEEP 3
            ERROR ""
            NEXT FIELD g_fecha_corte
         ELSE
            LET v_comando = "EXECUTE PROCEDURE fn_chk_fin_mes('",g_fecha_corte,
                                                                "')"
            PREPARE eje_fecha FROM v_comando
            EXECUTE eje_fecha INTO v_status
                
            IF v_status = 1 THEN
               IF l_paso = 2 THEN
                  IF fn_valida_proceso(g_fecha_corte,l_paso) <> 0 THEN
                     { IF fn_valida_saldo(g_fecha_corte,4) <> 0 THEN
                          NEXT FIELD g_fecha_corte
                       END IF
                     ELSE }
                        NEXT FIELD g_fecha_corte
                  END IF
               ELSE
                  IF fn_valida_proceso(g_fecha_corte,l_paso) <> 0 THEN
                     NEXT FIELD g_fecha_corte
                  ELSE
                     SELECT fecha_lote
                     INTO  g_fecha_lote
                     FROM  cta_cza_saldo_cero
                     WHERE fecha_corte = g_fecha_corte

                     NEXT FIELD g_respuesta
                  END IF
               END IF
            ELSE
               ERROR "LA FECHA CORTE NO ES FIN DE MES"
               SLEEP 3
               ERROR ""
               NEXT FIELD g_fecha_corte
            END IF
         END IF

      AFTER FIELD g_fecha_lote
         IF g_fecha_lote IS NULL THEN
            ERROR "DEBE INDICAR FECHA DE LOTE" 
            SLEEP 3
            ERROR ""
            NEXT FIELD g_fecha_lote
         END IF

      BEFORE FIELD g_respuesta 
         DISPLAY BY NAME g_mensaje3

      AFTER FIELD g_respuesta 
         IF g_respuesta <> "S" AND g_respuesta <> "N" THEN
            ERROR "SOLO INDIQUE S o N "
            SLEEP 3
            ERROR ""
            NEXT FIELD g_respuesta
         ELSE
            IF g_respuesta = "N" THEN
               ERROR "PROCESO CANCELADO."
               SLEEP 3
               ERROR ""
               LET g_cancela = TRUE
               EXIT INPUT
            ELSE
               IF l_paso = 2 THEN
                  PREPARE eje_ini FROM  
                  "EXECUTE FUNCTION fn_fecha_ini_saldo_cero(?)"
                  EXECUTE eje_ini USING g_fecha_corte
                                  INTO  g_fecha_ini

                  INSERT INTO cta_cza_saldo_cero VALUES ( g_fecha_corte ,
                                                          g_fecha_lote  ,
                                                          g_fecha_ini   ,
                                                          NULL          ,
                                                          NULL          ,
                                                          NULL          ,
                                                          NULL          ,
                                                          hoy )
               END IF

               IF l_paso = 4 THEN
                  CALL elimina_post()
               END IF

               CALL fn_inserta_paso (l_paso, g_fecha_corte)
 
               CALL fn_ejecuta_paso(g_fecha_corte, g_fecha_lote, l_paso)

               IF l_paso = 4 THEN
                  DATABASE safre_tmp
                     DROP TABLE nss_borra
                  DATABASE safre_af
               END IF
               EXIT INPUT
            END IF
         END IF

      ON KEY (CONTROL-C,INTERRUPT)
         ERROR "PROCESO CANCELADO."
         SLEEP 3
         ERROR ""
         LET g_cancela = TRUE
         LET int_flag  = FALSE
         EXIT INPUT

   END INPUT
END FUNCTION


FUNCTION fn_revisa_proceso()
   LET g_mensaje1 = "ESTA OPCION MUESTRA EL ESTADO DE EJECUCION DE LOS PASOS"

   LET g_mensaje2 = "INDIQUE LA FECHA DEL PERIODO: "

   LET g_cancela = FALSE

   INPUT BY NAME g_fecha_corte

      BEFORE INPUT 
         DISPLAY BY NAME g_mensaje1 ,
                         g_mensaje2
                            
      BEFORE FIELD g_fecha_corte
         DISPLAY "" TO formonly.g_mensaje3
         DISPLAY "" TO formonly.g_respuesta

      AFTER FIELD g_fecha_corte
         IF g_fecha_corte IS NULL THEN
            ERROR "DEBE INDICAR FECHA DE CORTE" 
            SLEEP 3
            ERROR ""
            NEXT FIELD g_fecha_corte
         END IF

         IF fn_despliega_pasos(g_fecha_corte) = 0 THEN
            ERROR "NO EXISTE EJECUCION PARA LA FECHA."
            SLEEP 3
            ERROR ""
            NEXT FIELD g_fecha_corte
         END IF

      ON KEY (CONTROL-C,INTERRUPT)
         ERROR "PROCESO CANCELADO."
         SLEEP 3
         ERROR ""
         LET g_cancela = TRUE
         LET int_flag  = FALSE
         EXIT INPUT

   END INPUT
END FUNCTION


FUNCTION fn_revisa_registro(v_tipo)
DEFINE
   v_tipo    SMALLINT

   LET g_mensaje1 = "ESTA OPCION MUESTRA EL TOTAL DE CUENTAS SALDO CERO"

   LET g_mensaje2 = "INDIQUE LA FECHA DEL PERIODO: "

   LET g_cancela = FALSE

   INPUT BY NAME g_fecha_corte

      BEFORE INPUT 
         DISPLAY BY NAME g_mensaje1 ,
                         g_mensaje2
                            
      BEFORE FIELD g_fecha_corte
         DISPLAY "" TO formonly.g_mensaje3
         DISPLAY "" TO formonly.g_respuesta

      AFTER FIELD g_fecha_corte
         IF g_fecha_corte IS NULL THEN
            ERROR "DEBE INDICAR FECHA DE CORTE" 
            SLEEP 3
            ERROR ""
            NEXT FIELD g_fecha_corte
         END IF

         IF fn_despliega_registro(g_fecha_corte, v_tipo) = 0 THEN
            ERROR "NO EXISTEN REGISTROS PARA LA FECHA."
            SLEEP 3
            ERROR ""
            NEXT FIELD g_fecha_corte
         END IF

      ON KEY (CONTROL-C,INTERRUPT)
         ERROR "PROCESO CANCELADO."
         SLEEP 3
         ERROR ""
         LET g_cancela = TRUE
         LET int_flag  = FALSE
         EXIT INPUT

   END INPUT
END FUNCTION


FUNCTION fn_despliega_registro(l_fecha_corte, v_tipo)
DEFINE
   l_fecha_corte	DATE

DEFINE
   pos,v_status         SMALLINT

DEFINE
   v_tipo               SMALLINT

DEFINE
   v_cons_fec           CHAR(400)

DEFINE
   arr_fmtoa1 ARRAY[240] OF RECORD
                               desc_oper        CHAR(12),
                               desc_cta         CHAR(11),
                               desc_tra         CHAR(19),
                               total_registros  INTEGER,
                               ffecha_corte     DATE
                            END RECORD


   LET v_cons_fec = "SELECT DECODE(operacion,'1','INHABILITAR',",
                                  " '2','HABILITAR') desc_oper,",
                    " DECODE(id_cuenta,'01','CTAS 01',",
                                  " '02','CTAS 02',",
                                  " '03','CTAS 03',",
                                  " '04','CTAS 04') desc_cta,",
                    " DECODE(tipo_trabajador,'01','AFILIADO',",
                                  " '02','NO AFILIADO',",
                                  " '03','ASIGNADO') desc_tra,",
                    " total_registros, ",
                    " fecha_corte ",
                    " FROM   cta_total_saldo_cero "

   IF v_tipo = 1 THEN
      LET v_cons_fec = v_cons_fec CLIPPED,
                       " WHERE  fecha_corte = '", l_fecha_corte,"'"
   ELSE
      LET v_cons_fec = v_cons_fec CLIPPED,
                       " WHERE  fecha_corte <= '", l_fecha_corte,"'"
   END IF

   LET v_cons_fec = v_cons_fec CLIPPED, " ORDER BY 1,2"
   LET v_cons_fec = v_cons_fec CLIPPED

   PREPARE eje_cons_fec FROM v_cons_fec

   OPEN WINDOW  w_submenu1 AT 6,4 WITH FORM "CTAM0053" ATTRIBUTES(BORDER)
   DISPLAY "CTAM005      CIFRAS TOTALES CUENTAS SALDO CERO     < Ctrl-C > SALIR            " AT 3,1 ATTRIBUTE(REVERSE)

   DECLARE cur_fa1 CURSOR FOR eje_cons_fec
 
   LET pos = 1

   FOREACH cur_fa1 INTO arr_fmtoa1[pos].*
      LET pos = pos + 1
      IF pos >= 8 THEN
         EXIT FOREACH
      END IF
   END FOREACH

   CLOSE cur_fa1
   FREE  cur_fa1

   IF (pos-1) < 1 THEN
      LET v_status = 0
   ELSE 
      LET v_status = 1
      CALL SET_COUNT(pos-1)
      DISPLAY l_fecha_corte TO fecha_corte

      DISPLAY ARRAY arr_fmtoa1 TO scr_1.*

         ON KEY ( INTERRUPT )
            LET pos = 0
            EXIT DISPLAY
            END DISPLAY
   END IF

   CLOSE WINDOW w_submenu1

   RETURN v_status
END FUNCTION


FUNCTION fn_despliega_pasos(l_fecha_corte)
DEFINE
   l_fecha_corte	DATE

DEFINE
   pos,v_status         SMALLINT

DEFINE
   v_consulta           CHAR(250)

DEFINE
   arr_fmtoa ARRAY[8] OF RECORD
                            etapa         CHAR(1),
                            desc_paso     CHAR(15),
                            fecha_inicio  DATETIME YEAR TO SECOND,
                            fecha_fin     DATETIME YEAR TO SECOND,
                            usuario       CHAR(8)
                         END RECORD


   LET v_consulta = 'SELECT etapa,',
                    'DECODE(etapa,1,"PREPARA INF SDO",',
                           '2,"IDENT. CUENTAS 03",',
                           '3,"IDENT. CUENTAS 04",',
                           '4,"GENERA ARCH.") desc_paso,',
                           'fecha_inicio,',
                           'fecha_fin,',
                           'usuario ',
                    'FROM   cta_ctr_saldo_cero ',
                    'WHERE  fecha_corte = ? ',
                    ' ORDER BY fecha_inicio,etapa'

   LET v_consulta = v_consulta CLIPPED
   PREPARE eje_consulta FROM v_consulta

   OPEN WINDOW  w_submenu AT 6,4 WITH FORM "CTAM0052" ATTRIBUTES(BORDER)
   DISPLAY "CTAM005    GENERA INF. EDO DE CUENTA SALDO CERO     < Ctrl-C > SALIR     " AT 3,1 ATTRIBUTE(REVERSE)


   DECLARE cur_fa CURSOR FOR eje_consulta
 
   LET pos = 1

   FOREACH cur_fa USING l_fecha_corte INTO arr_fmtoa[pos].*
      LET pos = pos + 1
      IF pos >= 8 THEN
         EXIT FOREACH
      END IF
   END FOREACH

   CLOSE cur_fa
   FREE  cur_fa

   IF (pos-1) < 1 THEN
      LET v_status = 0
   ELSE 
      LET v_status = 1
      CALL SET_COUNT(pos-1)
      DISPLAY l_fecha_corte TO fecha_corte

      DISPLAY ARRAY arr_fmtoa TO scr_1.*

         ON KEY ( INTERRUPT )
            LET pos = 0
            EXIT DISPLAY
            END DISPLAY
   END IF

   CLOSE WINDOW w_submenu
   RETURN v_status
END FUNCTION 


FUNCTION fn_ejecuta_paso( p_fecha_corte, p_fecha_lote, p_paso )
DEFINE
   p_fecha_corte  DATE,
   p_fecha_lote   DATE,
   v_fecha_inicio DATE

DEFINE
   p_paso         SMALLINT

DEFINE
   v_comando      CHAR(200)

   CASE p_paso
      WHEN 1
         LET v_comando = "nohup time fglgo CTAB120 1 ",p_fecha_corte," ", 
                                                       p_fecha_lote," ",
                         "1> ",g_usuario CLIPPED,".salida ",
                         "2> ",g_usuario CLIPPED,".error &"
                    ERROR "VERIFIQUE ARCHIVOS: ",g_usuario CLIPPED,".salida y ",
                                                 g_usuario CLIPPED,".error"
         SLEEP 5
         ERROR ""

      WHEN 2
         LET v_comando = "nohup time fglgo CTAB120 2 ",p_fecha_corte," ", 
                                                       p_fecha_lote," ",
                         "1> ",g_usuario CLIPPED,".salida ",
                         "2> ",g_usuario CLIPPED,".error &"
                    ERROR "VERIFIQUE ARCHIVOS: ",g_usuario CLIPPED,".salida y ",
                                                 g_usuario CLIPPED,".error"
         SLEEP 5
         ERROR ""

      WHEN 4
         LET v_comando = "nohup time fglgo CTAB121 ",p_fecha_corte," ", 
                                                     p_fecha_lote," ",
                         "1> ",g_usuario CLIPPED,".salida ",
                         "2> ",g_usuario CLIPPED,".error &"
                    ERROR "VERIFIQUE ARCHIVOS: ",g_usuario CLIPPED,".salida y ",
                                                 g_usuario CLIPPED,".error"
         SLEEP 5
         ERROR ""

         LET g_archivo = g_archivo CLIPPED,"/",g_usuario CLIPPED,".RSC.",
                         TODAY USING "DDMMYYYY"
         ERROR "EL ARCHIVO SE GENERA COMO :",g_archivo
         SLEEP 8
         ERROR ""
   END CASE
                 
   RUN v_comando
END FUNCTION


FUNCTION elimina_post()
   DEFINE
      nss_bo   CHAR(11),
      rpp      CHAR(1)

   WHILE TRUE
      PROMPT "DESEA ELIMINAR REGISTROS CON SALDO POSTERIOR S/N : " FOR CHAR rpp
      IF rpp MATCHES '[SsNn]' THEN
         EXIT WHILE
      END IF
   END WHILE

   IF rpp = "S" OR rpp = "s" THEN
      DECLARE cur_del CURSOR FOR
      SELECT nss FROM safre_tmp:nss_borra
      WHERE  fecha_corte = g_fecha_corte

      FOREACH cur_del INTO nss_bo
         DELETE FROM cta_act_marca
         WHERE nss = nss_bo
         AND   marca_cod = 151

         DELETE FROM cta_his_marca
         WHERE nss = nss_bo
         AND   marca_cod = 151
         AND   fecha_fin IS NULL
 
         DELETE FROM cta_saldo_cero
         WHERE nss = nss_bo
         AND   fecha_corte = g_fecha_corte
      END FOREACH
   END IF
END FUNCTION

