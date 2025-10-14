################################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                              #
#Owner             => E.F.P.                                                   #
#Programa RETM818  => PROGRAMA PARA EL CONTROL DE CONSULTAS DE RETIROS,        #
#                     CONECTANDO CON EL PROGRAMA CORRESPONDIENTE SEGUN SEA EL  #
#                     TIPO DE RETIRO.                                          #
#Fecha creacion    => 16 DE AGOSTO DEL 2004                                    #
#Autor             => ISAI JIMENEZ ROJAS                                       #
#Sistema           => RET                                                      #
#Modificacion      => 8-Sep-04 Inclusion de Retiro parcial 18:00               #
################################################################################

DATABASE safre_af

DEFINE g_hoy            DATE

-- Arreglo con datos que seran mostrados en pantalla
DEFINE ga_deta             ARRAY[100] of RECORD
       folio               LIKE dis_cuenta.folio,
       tipo_retiro         LIKE ret_solicitud_tx.tipo_retiro,
       fecha_conversion    LIKE dis_cuenta.fecha_conversion,
       monto_en_pesos      LIKE dis_cuenta.monto_en_pesos,
       monto_en_pesos_isr  LIKE dis_cuenta.monto_en_pesos,
       monto_total         LIKE dis_cuenta.monto_en_pesos
       END RECORD
-- Arreglo Auxiliar con datos de complemento que no seran mostrados en pantalla
DEFINE ga_aux              ARRAY[100] OF RECORD
       regimen             LIKE ret_solicitud_tx.regimen
       END RECORD 
         
#========================================================================#
#                                                                        #
#========================================================================#
MAIN
   DEFINE lr_datos_afil  RECORD
          n_seguro       LIKE afi_mae_afiliado.n_seguro,
          n_rfc          LIKE afi_mae_afiliado.n_rfc   ,
          n_unico        LIKE afi_mae_afiliado.n_unico ,
          paterno        LIKE afi_mae_afiliado.paterno ,
          materno        LIKE afi_mae_afiliado.materno ,
          nombres        LIKE afi_mae_afiliado.nombres
          END RECORD
   
   DEFINE i          SMALLINT
   DEFINE v_nss_aux  CHAR(11)
   DEFINE v_tecla    CHAR(1)
   DEFINE v_cmd  CHAR(100)
   DEFINE v_pos      SMALLINT   -- Posicion seleccionada del arreglo
         
   -- INICIO DEL PROGRAMA
           
   LET lr_datos_afil.n_seguro = ARG_VAL(1)   --nss
   LET g_hoy                  = TODAY
    
   -- TITULOS INICIALES
   
   OPEN WINDOW f8161 AT 2,3 WITH FORM "RETM8181" ATTRIBUTE (BORDER)  
   DISPLAY " RETM818                  DATOS DEL AFILIADO                    ",
            "               " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY g_hoy USING "DD-MM-YYYY"  AT 3,66 ATTRIBUTE(REVERSE)
   DISPLAY "                     DETALLES DE LIQUIDACIONES DE RETIRO         ",
            "               " AT 7,1 ATTRIBUTE(REVERSE)
   
   DISPLAY " CONSULTA " AT 1,66 ATTRIBUTE(REVERSE)
   
   -- VALIDACION INICIAL
   
   IF lr_datos_afil.n_seguro IS NULL OR lr_datos_afil.n_seguro = " " THEN 
      -- Permite la busqueda del Afiliado
      CALL consulta_afiliado() RETURNING lr_datos_afil.n_seguro    
   END IF

   -- SELECCION Y DESPLEGADO DE LOS DATOS DEL AFILIADO
   -- ( A partir de esta linea se usa solo el NSS )
   
   SELECT afi_mae_afiliado.n_rfc  ,
          afi_mae_afiliado.n_unico,
          afi_mae_afiliado.paterno,
          afi_mae_afiliado.materno,
          afi_mae_afiliado.nombres
     INTO lr_datos_afil.n_rfc  ,  
          lr_datos_afil.n_unico,  
          lr_datos_afil.paterno,  
          lr_datos_afil.materno,  
          lr_datos_afil.nombres 
     FROM afi_mae_afiliado
    WHERE afi_mae_afiliado.n_seguro = lr_datos_afil.n_seguro
    
   IF SQLCA.SQLCODE = NOTFOUND THEN
      ERROR "NSS ERRONEO, NO SE ENCUENTRA EL NSS EN MAESTRO DE AFILIADOS..."
      SLEEP 3
      EXIT PROGRAM
   END IF

   DISPLAY BY NAME lr_datos_afil.*  

   -- CONSULTA DE LOS DETALLES
   
   DECLARE cur_1 CURSOR FOR
   SELECT b.regimen,                  --No integradoe en arreglo para desplegado
          a.folio,
          b.tipo_retiro,
          a.fecha_conversion,
          SUM(a.monto_en_pesos)
     FROM dis_cuenta       a,
          ret_solicitud_tx b
    WHERE a.nss   = lr_datos_afil.n_seguro
      AND ( a.nss = b.nss    AND                  --Join
            a.consecutivo_lote = b.consecutivo )  --Join
      AND a.subcuenta NOT IN (3,10)
      AND a.tipo_movimiento BETWEEN 800 AND 899
    GROUP BY 1,2,3,4
   UNION
   SELECT "97",                       -- Para los detalles de PARCIAL
          folio,
          "I",
          fecha_conversion,
          SUM(monto_en_pesos)
     FROM dis_cuenta
    WHERE nss   = lr_datos_afil.n_seguro
      AND subcuenta NOT IN (3,10)            -- Aportaciones Voluntarias
      AND tipo_movimiento IN (870, 875)      -- Movs. Retiro Parcial
    GROUP BY 1,2,3,4
   UNION ------------------------------------------------------------------
   SELECT "97",                              -- Regimen para TRANSFERENCIAS
          folio,
          "A",
          fecha_conversion,
          SUM(monto_en_pesos)
     FROM dis_cuenta
    WHERE nss   = lr_datos_afil.n_seguro
      AND subcuenta NOT IN (3,10)            -- Aportaciones Voluntarias
      AND tipo_movimiento IN (800)           -- Movs. transferencia imss
    GROUP BY 1,2,3,4
   UNION ------------------------------------------------------------------
   SELECT "73",                              -- Regimen para TRANSFERENCIAS
          folio,
          "B",
          fecha_conversion,
          SUM(monto_en_pesos)
     FROM dis_cuenta
    WHERE nss = lr_datos_afil.n_seguro
      AND subcuenta NOT IN (3,10)            -- Aportaciones Voluntarias
      AND tipo_movimiento IN (810)           -- Movs. transferencia Gob fed
    GROUP BY 1,2,3,4
   UNION ------------------------------------------------------------------
   SELECT "97",                              -- Regimen para TRANSFERENCIAS
          folio,
          "C",
          fecha_conversion,
          SUM(monto_en_pesos)
     FROM dis_cuenta
    WHERE nss = lr_datos_afil.n_seguro
      AND subcuenta NOT IN (3,10)            -- Aportaciones Voluntarias
      AND tipo_movimiento IN (815)           -- Movs. transferencia 
    GROUP BY 1,2,3,4

   -- RECUPERACION DE CADA UNO DE LOS DETALLES
   LET i = 1

   FOREACH cur_1 INTO ga_aux[i].regimen, ga_deta[i].*
      IF i > 100 THEN
         ERROR "SE HA ALCANZADO EL NUMERO MAXIMO DE DETALLES(100)"
         SLEEP 3
         EXIT FOREACH
      END IF

      -- SELECCION DE LA INFORMACION DE ISR

      SELECT SUM(a.monto_en_pesos)
        INTO ga_deta[i].monto_en_pesos_isr
        FROM dis_cuenta       a
       WHERE a.nss = lr_datos_afil.n_seguro
         AND a.subcuenta NOT IN (3,10)
         AND a.tipo_movimiento = 10    -- ISR

      IF ga_deta[i].monto_en_pesos_isr IS NULL THEN
         LET ga_deta[i].monto_en_pesos_isr = 0
      END IF

      {
      SELECT SUM(a.monto_en_pesos)
        INTO ga_deta[i].monto_en_pesos_isr
        FROM dis_cuenta       a,
             ret_solicitud_tx b
       WHERE a.nss = lr_datos_afil.n_seguro
         AND ( a.nss = b.nss    AND                  --Join
               a.consecutivo_lote = b.consecutivo )  --Join
         AND a.subcuenta NOT IN (3,10)
         AND a.tipo_movimiento = 10
      }

      -- OBTENCION DE LA COLUMNA TOTAL
      
      LET ga_deta[i].monto_total = ga_deta[i].monto_en_pesos + 
                                   ga_deta[i].monto_en_pesos_isr

      LET i=i+1

   END FOREACH

   LET i = i -1

   IF i = 0 THEN
      OPTIONS PROMPT LINE LAST
      PROMPT "ESTE NSS NO TIENE RETIROS ASOCIADOS, PRESIONE <RETURN> PARA SALIR" 
             ATTRIBUTE(REVERSE) FOR CHAR v_tecla
   ELSE
      CALL SET_COUNT(i)

      -- DESPLEGADO DEL ARREGLO DE DETALLES DE RETIRO

      MESSAGE "Enter : Consultar Datos,  Control-C : Salir"
      
      DISPLAY ARRAY ga_deta TO scr.*
      
         -- SELECCION DEL PROGRAMA CORRESPONDIENTE POR EL TIPO DE RETIRO
         ON KEY (CONTROL-M)
            LET v_pos = ARR_CURR()  -- Posicion dentrl del Arreglo
            -- Invocacion al Programa correspondiente
            CASE ga_deta[v_pos].tipo_retiro
                 WHEN "J" LET v_cmd = "fglgo RETM801 Q ",lr_datos_afil.n_seguro
                          RUN v_cmd
                 WHEN "D" LET v_cmd = "fglgo RETM802 Q ",lr_datos_afil.n_seguro
                          RUN v_cmd
                 WHEN "E" 
                          IF ga_aux[v_pos].regimen = "73" THEN
                             LET v_cmd="fglgo RETM803 Q ",lr_datos_afil.n_seguro
                          ELSE
                             LET v_cmd="fglgo RETM804 Q ",lr_datos_afil.n_seguro
                          END IF
                          RUN v_cmd
                 WHEN "G" LET v_cmd = "fglgo RETM805 Q ",lr_datos_afil.n_seguro
                          RUN v_cmd
                 WHEN "H" LET v_cmd = "fglgo RETM806 Q ",lr_datos_afil.n_seguro
                          RUN v_cmd
                 WHEN "I" LET v_cmd = "fglgo RETM809 Q ",lr_datos_afil.n_seguro
                          RUN v_cmd
                 WHEN "F"
                          IF ga_aux[v_pos].regimen = "73" THEN
                             LET v_cmd="fglgo RETM807 Q ",lr_datos_afil.n_seguro
                          ELSE
                             LET v_cmd="fglgo RETM808 Q ",lr_datos_afil.n_seguro
                          END IF
                          RUN v_cmd
                 OTHERWISE
                          ERROR "OPCION NO DISPONIBLE POR EL TIPO DE RETIRO"
                          SLEEP 2
            END CASE
               
      END DISPLAY
   END IF

END MAIN

#==========================================================================#
#  Funcion : consulta_afiliado                                             #
#  Objetivo: Permite consultar afiliados de afi_mae_afiliado de aquellos   #
#            que existan em dis_cuenta con tipo_movimiento entre 800 y 899 #
#            devolviendo el NSS seleccionado por el usuario                #          
#  Autor   : Isai Jimenez                                                  #
#==========================================================================#

FUNCTION consulta_afiliado()

   DEFINE v_condicion     CHAR(300)
   DEFINE v_instruccion   CHAR(400)
   DEFINE v_pos, i        SMALLINT
   DEFINE v_opcion        CHAR(1)
   DEFINE v_nss           CHAR(11)
   DEFINE v_bandera       SMALLINT   -- para controlar la ruptura del ciclo
   
   DEFINE la_afil         ARRAY[100] OF  RECORD
          n_seguro        LIKE afi_mae_afiliado.n_seguro,
          paterno         LIKE afi_mae_afiliado.paterno ,
          materno         LIKE afi_mae_afiliado.materno ,
          nombres         LIKE afi_mae_afiliado.nombres
          END RECORD
   
   -- Para hacerce el proceso varias veces en caso de ser necesario
   LET v_opcion = "S"
   LET v_bandera = 0 

   WHILE v_bandera = 0
   
      CLEAR FORM
      
      MESSAGE "Esc : Buscar,  Control-C : Salir"
      
      -- Inicializacion del Arreglo
      FOR i=1 TO 100 
          INITIALIZE la_afil[i].* TO NULL
      END FOR
      LET v_nss = NULL      
      
      -- Se solicita el Criterio de Busqueda
      CONSTRUCT BY NAME v_condicion ON n_seguro,
                                       n_rfc   ,
                                       n_unico ,
                                       paterno ,
                                       materno ,
                                       nombres
      
      -- Para revision de costos de Optimizador                                 
      IF FGL_GETENV("SQEXPLAIN") THEN
         SET EXPLAIN ON
      END IF
         
      -- Se Arma la instruccion de Busqueda
      LET v_instruccion = "SELECT n_seguro,paterno,materno,nombres ",
                          "  FROM afi_mae_afiliado ",
                          " WHERE ", v_condicion CLIPPED,
                          "   AND n_seguro IN ",
                          " (SELECT nss FROM dis_cuenta ",
                          "   WHERE tipo_movimiento BETWEEN 800 AND 899 )",
                         #"   WHERE subcuenta NOT IN (3,10) )",
                          " ORDER BY 2,3 "
                          
      PREPARE exe_afil FROM v_instruccion
      IF SQLCA.SQLCODE != 0 THEN
         ERROR "ERROR EN LA PREPARACION DE LA INSTRUCCION"
         SLEEP 3
         EXIT PROGRAM
      END IF
         
      DISPLAY "BUSCANDO INFORMACION" AT 19,1 ATTRIBUTE(REVERSE)
      
      -- Se buscan los Afiliados que cumplan el criterio
      LET i=1
         
      DECLARE cur_afil CURSOR FOR exe_afil
      
      FOREACH cur_afil INTO la_afil[i].*
         LET i = i + 1
         IF i > 100 THEN
            ERROR "SE HA ALCANZADO EL LIMITE DE ELEMENTOS A MOSTRAR"
            EXIT FOREACH
         END IF
      END FOREACH
         
      DISPLAY "" AT 19,1
      
      -- Para revision de costos de Optimizador
      IF FGL_GETENV("SQEXPLAIN") THEN
         SET EXPLAIN OFF
      END IF
         
      LET i = i - 1
      
      IF i = 0 THEN
         ERROR "NO HAY REGISTROS DE ESE CRITERIO CON RETIROS ASOCIADOS..." 
         LET v_bandera = 0   -- Se vuelve a solicitar el criterio de busqueda
      ELSE
         IF i = 1 THEN
            -- Solo se recupero un elemento, no es necesario desplegar arreglo
            LET v_nss     = la_afil[i].n_seguro -- NSS a retornar
            LET v_bandera = 1                   -- Salida del ciclo
         ELSE
            -- Muestra arreglo de Afiliados que cumplen condicion
            CALL SET_COUNT(i)
            OPEN WINDOW w_afil AT 2,2 WITH FORM "RETM8182" 
                 ATTRIBUTE(BORDER,MESSAGE LINE LAST -1)
                 
                 -- Titulos de La Ventana Auxiliar
                 DISPLAY " RETM818                  RESULTADO DE LA BUSQUEDA  ",
                         "                         " AT 3,1 ATTRIBUTE(REVERSE)
                 DISPLAY " Enter : Acepta registro   Ctrl-C : Cancelar " AT 2,1
                 DISPLAY g_hoy USING "DD-MM-YYYY"  AT 3,67 ATTRIBUTE(REVERSE)
                 
                 LET INT_FLAG = FALSE

                 DISPLAY ARRAY la_afil TO scr.*
                    ON KEY (CONTROL-M)
                       LET v_pos = ARR_CURR()
                       LET v_nss = la_afil[v_pos].n_seguro
                       EXIT DISPLAY
                 END DISPLAY
            CLOSE WINDOW w_afil
            
            IF INT_FLAG THEN
               LET v_nss    = NULL
               LET INT_FLAG = FALSE
            END IF
            LET v_bandera = 1  -- Terminar el ciclo
         END IF
      END IF
   END WHILE
                                  
   RETURN v_nss
   
END FUNCTION

   
