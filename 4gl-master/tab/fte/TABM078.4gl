################################################################################
#Proyecto                   : Sistema de AFORES (México)                       #
#Owner                      : E.F.P.                                           #
#Programa TAMB078           : Comparativos                                     #
#Fecha                      : 12 Septiembre de 1997                            #
#Por                        : José Manuel Vizcaino Culebra                     #
#Fecha modifica             : 19 Noviembre 2003                                #
#Modifico                   : Juan Carlos Mendoza Moreno                       #
#Sistema                    : TAB                                              #
#Actualiza                  : Eduardo Joaquin Resendiz Medina                  #
#Fecha Actualiza            : 13 Jun 2005 (se agrego tipo_comparativo, periodo)#
#                           : 05 Mar 2008 (Se agrego campo Siefore            )#
################################################################################
DATABASE safre_af
GLOBALS

   DEFINE g_param_dis	RECORD LIKE seg_modulo.*

   DEFINE aux_pausa        CHAR(1),
          sw_1             SMALLINT,
          usuario          CHAR(8),
          hoy              DATE,
          pos              INTEGER,
          sel_where        CHAR(30000),
          cla_where        CHAR(30000),
          g_impre          CHAR(300),
          g_lista          CHAR(300)

   DEFINE g_reg     RECORD 
          afore_cod        SMALLINT,
          siefore_cod      SMALLINT,
          porcentaje       DECIMAL(6,2),
          fecha            DATE,
          tipo_comparativo SMALLINT,
          periodo          SMALLINT,
          usuario          CHAR(8), 
          factualiza       DATE
   END RECORD

   DEFINE l_record  ARRAY[30000] OF RECORD
          codigo           SMALLINT,
          siefore          SMALLINT,
          porciento        DECIMAL(6,2),
          dato_fecha       DATE,
          tipo_comparativo SMALLINT,
          desc_tipo        CHAR(40),
          periodo          SMALLINT,
          desc_periodo     CHAR(15)
   END RECORD

   DEFINE x_afore_desc   CHAR(40)
   DEFINE vsiefore_desc CHAR(40)
   DEFINE vafore_f       SMALLINT

   DEFINE vhabil         SMALLINT

   DEFINE aux_val        SMALLINT

   DEFINE l_reg ARRAY[1000] OF RECORD
          afore_cod     INTEGER,
          afore_desc    CHAR(50)
          END RECORD

   DEFINE l_reg_sie ARRAY[1000] OF RECORD
          siefore_cod     INTEGER,
          siefore_desc    CHAR(50)
          END RECORD

   DEFINE x_x           CHAR(500),
          x_buscar      CHAR(30)

   DEFINE v_desc_tipo CHAR(40)
   DEFINE v_desc_periodo   CHAR(15)

   DEFINE l_reg2 ARRAY[1000] OF RECORD
          tipo_comparativo       INTEGER,
          desc_tipo_comparativo  CHAR(50)
          END RECORD

--   DEFINE v_desc_tipo            CHAR(40)
   DEFINE cla_where2             CHAR(1000),
          carga                  CHAR(100)
END GLOBALS
################################################################################
MAIN

   OPTIONS PROMPT LINE LAST,
   INPUT WRAP              ,
   ACCEPT KEY control-o

   DEFER INTERRUPT
   CALL STARTLOG("TABM078.log")
   CALL inicio()
   CALL proceso()

END MAIN
################################################################################
FUNCTION inicio()

   SELECT USER, ruta_listados,ruta_rescate
   INTO   usuario,
          g_param_dis.ruta_listados,
          g_param_dis.ruta_rescate
   FROM   seg_modulo
   WHERE  modulo_cod = "tab"

   LET vhabil = 0

   DATABASE safre_tmp
      DROP TABLE plano_indices

      CREATE TABLE plano_indices
          (afore SMALLINT,
           siefore SMALLINT,
           porcentaje DECIMAL(7,3),
           fecha_indice CHAR(10),
           tipo_comp SMALLINT,
           periodo SMALLINT)

   DATABASE safre_af

END FUNCTION
################################################################################
FUNCTION proceso()

   LET HOY = TODAY
   OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0781" ATTRIBUTE( BORDER)
   DISPLAY        " TAMB078           CATALOGO COMPARATIVO DE COMISIONES                          " AT 3,1 ATTRIBUTE(REVERSE) 
   DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)

   MENU "COMPARATIVO DE COMISIONES"
      COMMAND "Agrega" "Agrega "
         CALL Agrega()
         CLEAR SCREEN
      COMMAND "Consulta" "Consulta "
         CALL Consulta()
         CLEAR SCREEN
      COMMAND "Modifica" "Modifica"
         CALL Modifica()
         CLEAR SCREEN
      COMMAND "Elimina" "Elimina "
         CALL Elimina()
         CLEAR SCREEN
      COMMAND "Carga Archivo" "Carga Archivo de Indice de Rendimiento Neto "
         CALL carga_archivo()
         CLEAR SCREEN
      COMMAND "Salir" "Salir del Programa "
         EXIT MENU
   END MENU
   CLOSE WINDOW ventana_1

END FUNCTION
################################################################################
FUNCTION Inicializa()

   LET sw_1 = 0
   INITIALIZE x_afore_desc TO NULL
   INITIALIZE g_reg.* TO NULL
   INITIALIZE v_desc_periodo TO NULL
   INITIALIZE v_desc_tipo TO NULL
   INITIALIZE vsiefore_desc TO NULL
   DISPLAY BY NAME g_reg.afore_cod
   DISPLAY BY NAME g_reg.siefore_cod
   DISPLAY BY NAME g_reg.porcentaje
   DISPLAY BY NAME g_reg.fecha
   DISPLAY BY NAME g_reg.tipo_comparativo
   DISPLAY BY NAME g_reg.periodo
   DISPLAY BY NAME x_afore_desc
   DISPLAY BY NAME v_desc_tipo
   DISPLAY BY NAME v_desc_periodo
   DISPLAY BY NAME vsiefore_desc

END FUNCTION
################################################################################
FUNCTION Agrega()
	
   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " ( Esc ) Agrega                 (Ctrl-c) Salir                                 " AT 1,1 ATTRIBUTE(BOLD)
   DISPLAY " AGREGA " AT 1,69 ATTRIBUTE(REVERSE,BOLD)
   LET g_reg.porcentaje = NULL
   LET sw_1 = 0
   INPUT BY NAME  g_reg.afore_cod,
                  g_reg.siefore_cod,
                  g_reg.porcentaje,
                  g_reg.fecha,
                  g_reg.tipo_comparativo,
                  g_reg.periodo
      BEFORE FIELD afore_cod	
         IF sw_1 = 0 THEN
            LET sw_1 = 1
           DISPLAY BY NAME g_reg.afore_cod
         END IF

      AFTER FIELD afore_cod	
     IF g_reg.afore_cod IS NULL THEN
        OPEN WINDOW vent_1 AT 07,15 WITH FORM "TABM0783" 
        ATTRIBUTE(BORDER)
        DISPLAY "                   A F O R E S                           " 
        AT 2,1 ATTRIBUTE(REVERSE)

        LET int_flag = FALSE

        CONSTRUCT BY NAME cla_where ON afore_cod
          ON KEY (control-m)
             LET int_flag = FALSE
             EXIT CONSTRUCT
              ON KEY (control-c)
             IF int_flag = TRUE THEN
            EXIT CONSTRUCT
                 END IF
            END CONSTRUCT

            IF int_flag = TRUE THEN
               ERROR "BUSQUEDA CANCELADA..."
               SLEEP 2
               ERROR ""
               CLEAR SCREEN
               CLOSE WINDOW vent_1
               NEXT FIELD afore_cod
            END IF
    
        LET x_x = " SELECT afore_cod, afore_desc FROM tab_afore ",
                  " WHERE ", cla_where CLIPPED , 
                      #" AND marca <> 1 ",
                  " ORDER BY 2 " CLIPPED
            LET x_x = x_x CLIPPED
        PREPARE curg29 FROM x_x
        DECLARE cur_g29 CURSOR FOR curg29
        LET pos = 1
        FOREACH cur_g29 INTO l_reg[pos].*
           LET pos = pos + 1
           IF pos >= 1000 THEN
              ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
              EXIT FOREACH
           END IF
        END FOREACH
    
        IF (pos-1) < 1 THEN
           ERROR "ARCHIVO AFORE..... VACIO"
        END IF
        IF (pos-1) >= 1 THEN
        CALL SET_COUNT(pos-1)
        DISPLAY ARRAY l_reg TO scr_1.*
          ON KEY ( CONTROL-M )
             LET pos = ARR_CURR()
         LET g_reg.afore_cod = l_reg[pos].afore_cod
         LET x_afore_desc    = l_reg[pos].afore_desc
             EXIT DISPLAY
          ON KEY ( INTERRUPT )
         ERROR "Usted debe escojer un registro"
         LET pos = ARR_CURR()
              ON KEY ( CONTROL-C )
         ERROR "Usted debe escojer un registro"
         LET pos = ARR_CURR()
        END DISPLAY
        CLOSE WINDOW vent_1
            DISPLAY BY NAME g_reg.afore_cod, x_afore_desc
        END IF

        IF g_reg.afore_cod = 0 THEN
           LET x_afore_desc = ''
           DISPLAY BY NAME x_afore_desc
           NEXT FIELD afore_cod
        END IF

        SELECT @afore_desc
        INTO   x_afore_desc
        FROM   safre_af:tab_afore
        WHERE  @afore_cod = g_reg.afore_cod
        #AND    @marca     = 1

        IF SQLCA.SQLCODE <> 0 THEN
           SELECT @afore_desc, @afore_fusion
           INTO   x_afore_desc, vafore_f
           FROM   safre_af:tab_afore
           WHERE  @afore_cod = g_reg.afore_cod
           #AND    @marca     = 0

           IF SQLCA.SQLCODE = 0 THEN
              IF vafore_f IS NOT NULL THEN
                 SELECT @afore_desc
                     INTO   x_afore_desc
                     FROM   tab_afore
                     WHERE  @afore_cod = vafore_f

                     LET g_reg.afore_cod = vafore_f
              END IF

                  DISPLAY BY NAME g_reg.afore_cod, 
                  x_afore_desc
           END IF
        {ELSE
           ERROR " Codigo Afore no existe o no permitido"
           SLEEP 3
           LET g_reg.afore_cod = ''
           DISPLAY BY NAME g_reg.afore_cod
           NEXT FIELD afore_cod}
        END IF
     END IF

     IF g_reg.afore_cod = "999" THEN
        LET x_afore_desc = "COMISION PROMEDIO"
        DISPLAY BY NAME x_afore_desc
        NEXT FIELD siefore_cod
     END IF

     IF g_reg.afore_cod = "998" THEN
        LET x_afore_desc = "BANCO DE MEXICO"
        DISPLAY BY NAME x_afore_desc
        NEXT FIELD siefore_cod
     END IF

     SELECT @Afore_desc, @afore_fusion
     INTO   x_afore_desc, vafore_f
     FROM   safre_af:tab_afore
     WHERE  @afore_cod = g_reg.afore_cod
     #AND    @marca     = 0
    
     IF SQLCA.SQLCODE = 0 THEN
        IF vafore_f IS NOT NULL THEN
               SELECT @afore_desc
           INTO   x_afore_desc
           FROM   tab_afore
           WHERE  @afore_cod = vafore_f

               LET g_reg.afore_cod = vafore_f
        END IF

        DISPLAY BY NAME g_reg.afore_cod, x_afore_desc
     ELSE
        ERROR " Clave de Afore no existe "

        LET g_reg.afore_cod = ''
        DISPLAY BY NAME x_afore_desc
        NEXT FIELD afore_cod
     END IF

         {SELECT afore_desc
         INTO   x_afore_desc
         FROM   tab_afore
         WHERE  afore_cod = g_reg.afore_cod
         IF x_afore_desc IS NULL THEN
            ERROR " NO EXISTE AFORE."
            NEXT FIELD afore_cod
         END IF
         DISPLAY BY NAME x_afore_desc}

###################>
     AFTER FIELD siefore_cod

     IF g_reg.siefore_cod IS NULL THEN
        OPEN WINDOW vent_sie AT 07,15 WITH FORM "TABM0786" 
        ATTRIBUTE(BORDER)
        DISPLAY "               S I E F O R E S                           " 
        AT 2,1 ATTRIBUTE(REVERSE)

        LET int_flag = FALSE

        CONSTRUCT BY NAME cla_where ON siefore_cod
          ON KEY (control-m)
             LET int_flag = FALSE
             EXIT CONSTRUCT
              ON KEY (control-c)
             IF int_flag = TRUE THEN
            EXIT CONSTRUCT
                 END IF
            END CONSTRUCT

            IF int_flag = TRUE THEN
               ERROR "BUSQUEDA CANCELADA..."
               SLEEP 2
               ERROR ""
               CLEAR SCREEN
               CLOSE WINDOW vent_sie
               NEXT FIELD siefore_cod
            END IF
    
        LET x_x = " SELECT siefore_cod, siefore_desc FROM tab_siefore ",
                  " WHERE ", cla_where CLIPPED , 
                   " AND afore_cod = ", g_reg.afore_cod ,
                  " ORDER BY 2 " CLIPPED
            LET x_x = x_x CLIPPED
        PREPARE curg29_sie FROM x_x
        DECLARE cur_g29_sie CURSOR FOR curg29_sie
        LET pos = 1
        FOREACH cur_g29_sie INTO l_reg_sie[pos].*
           LET pos = pos + 1
           IF pos >= 1000 THEN
              ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
              EXIT FOREACH
           END IF
        END FOREACH
    
        IF (pos-1) < 1 THEN
           ERROR "ARCHIVO SIEFORE..... VACIO"
        END IF
        IF (pos-1) >= 1 THEN
        CALL SET_COUNT(pos-1)
        DISPLAY ARRAY l_reg_sie TO scr_1.*
          ON KEY ( CONTROL-M )
             LET pos = ARR_CURR()
         LET g_reg.siefore_cod = l_reg_sie[pos].siefore_cod
         LET vsiefore_desc    = l_reg_sie[pos].siefore_desc
             EXIT DISPLAY
          ON KEY ( INTERRUPT )
         ERROR "Usted debe escojer un registro"
         LET pos = ARR_CURR()
              ON KEY ( CONTROL-C )
         ERROR "Usted debe escojer un registro"
         LET pos = ARR_CURR()
        END DISPLAY
        CLOSE WINDOW vent_sie
            DISPLAY BY NAME g_reg.siefore_cod, vsiefore_desc
        END IF

        IF g_reg.siefore_cod = 0 THEN
           LET vsiefore_desc = ''
           DISPLAY BY NAME vsiefore_desc
           NEXT FIELD siefore_cod
        END IF

        SELECT @siefore_desc
        INTO   vsiefore_desc
        FROM   safre_af:tab_siefore
        WHERE  @siefore_cod = g_reg.siefore_cod
        AND    @afore_cod = g_reg.afore_cod 

        IF SQLCA.SQLCODE = NOTFOUND THEN 
           ERROR "Siefore no existe "
           NEXT FIELD siefore_cod
        ELSE
           DISPLAY BY NAME g_reg.siefore_cod, 
                           vsiefore_desc
           NEXT FIELD porcentaje
        END IF

        DISPLAY BY NAME g_reg.siefore_cod, vsiefore_desc
     ELSE
        SELECT @siefore_desc
        INTO   vsiefore_desc
        FROM   safre_af:tab_siefore
        WHERE  @siefore_cod = g_reg.siefore_cod
        AND    @afore_cod = g_reg.afore_cod 

        IF SQLCA.SQLCODE = NOTFOUND THEN 
           ERROR "Siefore no existe "
           NEXT FIELD siefore_cod
        ELSE
           DISPLAY BY NAME g_reg.siefore_cod, 
                           vsiefore_desc
           NEXT FIELD porcentaje
        END IF

     END IF

###################<

      AFTER FIELD porcentaje
         IF g_reg.porcentaje IS NULL THEN --OR g_reg.porcentaje = 0 THEN
            ERROR " EL PORCENTAJE DE AFORE NO PUEDE SER NULO."
            NEXT FIELD  porcentaje
         END IF
         IF g_reg.porcentaje > 100 THEN
            ERROR " LA COMISION NO PUEDE SER MAYOR A 100%"
            NEXT FIELD porcentaje
         END IF

      AFTER FIELD fecha
         IF g_reg.fecha IS NULL OR g_reg.fecha = 0 THEN
            ERROR " LA FECHA NO PUEDE SER NULO"
            NEXT FIELD fecha
         ELSE                      
            {SELECT "OK"   
            FROM   tab_comparativo
            WHERE afore_cod = g_reg.afore_cod 
            AND   fecha  = g_reg.fecha
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
               ERROR  " REGISTRO DADO DE ALTA, VERIFIQUE... " 
               NEXT FIELD fecha
            END IF}

         END IF 

         CALL dia_habil(g_reg.fecha) RETURNING vhabil
         IF vhabil = 1 THEN
           ERROR "LA FECHA DE CAPTURA NO ES DIA HABIL"
           NEXT FIELD fecha
         END IF  


         IF g_reg.fecha > HOY THEN
            ERROR "FECHA CAPTURA NO PUEDE SER MAYOR AL DIA DE HOY"
            NEXT FIELD fecha
         END IF

         AFTER FIELD tipo_comparativo
------------>erm 11 Julio 2006
         IF g_reg.tipo_comparativo IS NULL THEN
            OPEN WINDOW vent_2 AT 07,15 WITH FORM "TABM0784" 
            ATTRIBUTE(BORDER)
            DISPLAY "               TIPO COMPARATIVO                           " 
            AT 2,1 ATTRIBUTE(REVERSE)

            LET int_flag = FALSE

           CONSTRUCT BY NAME cla_where2 ON tipo_comparativo
             ON KEY (control-m)
                LET int_flag = FALSE
                EXIT CONSTRUCT
                 ON KEY (control-c)
                IF int_flag = TRUE THEN
               EXIT CONSTRUCT
                    END IF
           END CONSTRUCT

           IF int_flag = TRUE THEN
              ERROR "BUSQUEDA CANCELADA..."
              SLEEP 2
              ERROR ""
              CLEAR SCREEN
              CLOSE WINDOW vent_2
              NEXT FIELD tipo_cmparativo
           END IF
 
           LET x_x = " SELECT tipo_comparativo,desc_tipo_comparativo FROM tab_tipo_comparativo ",
                 " WHERE ", cla_where2 CLIPPED , 
                 " ORDER BY 1 " CLIPPED
           LET x_x = x_x CLIPPED
           PREPARE curg30 FROM x_x
           DECLARE cur_g30 CURSOR FOR curg30
           LET pos = 1

           FOREACH cur_g30 INTO l_reg2[pos].*
              LET pos = pos + 1
              IF pos >= 1000 THEN
                 ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
                 EXIT FOREACH
              END IF
           END FOREACH

           IF (pos-1) < 1 THEN
              ERROR "ARCHIVO TIPO COMPARATIVO..... VACIO"
           END IF

           IF (pos-1) >= 1 THEN
              CALL SET_COUNT(pos-1)
              DISPLAY ARRAY l_reg2 TO scr_1.*
                ON KEY ( CONTROL-M )
                   LET pos = ARR_CURR()
                LET g_reg.tipo_comparativo = l_reg2[pos].tipo_comparativo
                LET v_desc_tipo            = l_reg2[pos].desc_tipo_comparativo
                   EXIT DISPLAY
                   ON KEY ( INTERRUPT )
                   ERROR "Usted debe escojer un registro"
                LET pos = ARR_CURR()
                   ON KEY ( CONTROL-C )
                   ERROR "Usted debe escojer un registro"
                LET pos = ARR_CURR()
              END DISPLAY
              CLOSE WINDOW vent_2
                  DISPLAY BY NAME g_reg.tipo_comparativo,v_desc_tipo
           END IF
         END IF

------------<
         IF g_reg.tipo_comparativo IS NULL OR 
            g_reg.tipo_comparativo = "" THEN
            ERROR "EL TIPO COMPARATIVO DEBE SER CAPTURADO..."
            SLEEP 2
            ERROR ""
            NEXT FIELD tipo_comparativo
         ELSE
            {IF g_reg.tipo_comparativo < 1 AND 
               g_reg.tipo_comparativo > 2 THEN
               ERROR " EL TIPO COMPARATIVO SOLO DEBE SER 1 o 2, VERIFIQUE...."
               SLEEP 2
               ERROR ""
               NEXT FIELD  tipo_comparativo
            END IF}

            SELECT desc_tipo_comparativo
            INTO   v_desc_tipo
            FROM   safre_af:tab_tipo_comparativo
            WHERE  tipo_comparativo = g_reg.tipo_comparativo
   
            IF SQLCA.SQLCODE = NOTFOUND THEN
               ERROR "NO EXISTE DESCRIPCION EN CATALOGO...VERIFIQUE"
               SLEEP 2
               ERROR ""
            END IF
         END IF

         SELECT "OK"   
         FROM   tab_comparativo
         WHERE afore_cod        = g_reg.afore_cod 
         AND   fecha            = g_reg.fecha
         AND   tipo_comparativo = g_reg.tipo_comparativo
         GROUP BY 1

         IF STATUS <> NOTFOUND THEN
            ERROR  " REGISTRO DADO DE ALTA, VERIFIQUE... " 
            NEXT FIELD fecha
         END IF

         DISPLAY BY NAME v_desc_tipo

         AFTER FIELD periodo
         IF g_reg.periodo IS NULL OR 
            g_reg.periodo = "" THEN
            ERROR "EL PERIODO DEBE SER CAPTURADO..."
            SLEEP 2
            ERROR ""
            NEXT FIELD periodo
         ELSE 
            LET v_desc_periodo = "ANIOS"
            DISPLAY BY NAME v_desc_periodo
         END IF


  ON KEY ( ESC )
         IF g_reg.afore_cod IS NULL THEN
            ERROR "CODIGO DE AFORE NO PUEDE SER NULO."
            NEXT FIELD  afore_cod
         END IF
         SELECT afore_desc
         INTO   x_afore_desc
         FROM   tab_afore
         WHERE  afore_cod = g_reg.afore_cod

         IF x_afore_desc IS NULL THEN
            ERROR " NO EXISTE AFORE."
            NEXT FIELD afore_cod
         END IF

         DISPLAY BY NAME x_afore_desc
         IF g_reg.porcentaje IS NULL THEN --OR g_reg.porcentaje = 0 THEN
            ERROR " EL PORCENTAJE DE AFORE NO PUEDE SER NULO."
            NEXT FIELD  porcentaje
         END IF
         IF g_reg.fecha IS NULL THEN
            ERROR " LA FECHA NO PUEDE SER NULO"
            NEXT FIELD fecha
         ELSE
            SELECT "OK"
            FROM   tab_comparativo
            WHERE afore_cod        = g_reg.afore_cod
            AND   fecha            = g_reg.fecha
            AND   tipo_comparativo = g_reg.tipo_comparativo
            GROUP BY 1

            IF SQLCA.SQLCODE = 0  THEN
               ERROR  " REGISTRO DADO DE ALTA, VERIFIQUE... "
               NEXT FIELD fecha
            END IF

         END IF
         IF g_reg.fecha > HOY THEN
            ERROR "FECHA CAPTURA NO PUEDE SER MAYOR AL DIA DE HOY"
            NEXT FIELD fecha
         END IF

         IF g_reg.afore_cod IS NULL THEN
            ERROR "CODIGO DE AFORE NO PUEDER SER NULO"
            NEXT FIELD afore_cod
         END IF
         
         IF g_reg.porcentaje IS NULL THEN
            ERROR "PORCENTAJE DE AFORE NO PUEDE SER NULO"
            NEXT FIELD porcentaje
         END IF

         IF g_reg.tipo_comparativo IS NULL OR 
            g_reg.tipo_comparativo = "" THEN
            ERROR "EL TIPO COMPARATIVO NO PUEDE SER NULO..."
            SLEEP 2
            ERROR ""
            NEXT FIELD tipo_comparativo
         ELSE
            {IF g_reg.tipo_comparativo < 1 AND 
               g_reg.tipo_comparativo > 2 THEN
               ERROR " EL TIPO COMPARATIVO SOLO DEBE SER 1 o 2, VERIFIQUE...."
               SLEEP 2
               ERROR ""
               NEXT FIELD  tipo_comparativo
            END IF}
         END IF

         IF g_reg.periodo IS NULL OR 
            g_reg.periodo = "" THEN
            ERROR "EL PERIODO NO PUEDE SER NULO..."
            SLEEP 2
            ERROR ""
            NEXT FIELD periodo
         END IF

         LET g_reg.factualiza = TODAY
         LET g_reg.usuario = usuario

         INSERT INTO tab_comparativo VALUES ( g_reg.* )
         ERROR "REGISTRO INGRESADO"
         SLEEP 1
         CALL Inicializa()
         NEXT FIELD afore_cod	

      ON KEY (INTERRUPT)
         CALL Inicializa()
      EXIT INPUT

   END INPUT

END FUNCTION
################################################################################
FUNCTION Consulta()
	
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0782" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta             (Ctrl-p) Imprimir              (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                         COMPARATIVO  DE  COMISIONES                           " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 
      LET int_flag = FALSE

      CONSTRUCT cla_where ON afore_cod,
                             fecha,
                             tipo_comparativo,
                             periodo
                        FROM afore_cod,
                             fecha,
                             tipo_comparativo,
                             periodo
         ON KEY (control-m)

            ERROR "PROCESANDO INFORMACION..."
            SLEEP 1
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (control-c)
         
            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "BUSQUEDA CANCELADA..."
         SLEEP 1
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      LET sel_where = "SELECT afore_cod,siefore_cod,porcentaje,fecha,",
                      "tipo_comparativo,periodo FROM tab_comparativo WHERE ",
                      cla_where CLIPPED,
	                  "ORDER BY 3,1 "
      PREPARE query FROM sel_where
      DECLARE cursor_1 CURSOR FOR query
      LET pos = 1

      FOREACH cursor_1 INTO l_record[pos].codigo,           
                            l_record[pos].siefore,          
                            l_record[pos].porciento,        
                            l_record[pos].dato_fecha,       
                            l_record[pos].tipo_comparativo, 
                            l_record[pos].periodo

         LET l_record[pos].codigo            = l_record[pos].codigo
         LET l_record[pos].siefore           = l_record[pos].siefore
         LET l_record[pos].porciento         = l_record[pos].porciento
         LET l_record[pos].dato_fecha        = l_record[pos].dato_fecha
         LET l_record[pos].tipo_comparativo  = l_record[pos].tipo_comparativo
         LET l_record[pos].desc_tipo         = l_record[pos].desc_tipo
         LET l_record[pos].periodo           = l_record[pos].periodo
         LET l_record[pos].desc_periodo      = l_record[pos].desc_periodo

         IF l_record[pos].periodo IS NOT NULL THEN
            LET l_record[pos].desc_periodo = "ANIO(S)"
         ELSE 
            LET l_record[pos].desc_periodo = ""
         END IF
         
         SELECT desc_tipo_comparativo
           INTO l_record[pos].desc_tipo
           FROM safre_af:tab_tipo_comparativo
          WHERE tipo_comparativo = l_record[pos].tipo_comparativo

         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL  SET_COUNT(pos-1)
         ERROR ""	

         DISPLAY ARRAY l_record   TO scr_1.*

            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()
               LET g_reg.afore_cod        = l_record[pos].codigo
               LET g_reg.siefore_cod      = l_record[pos].siefore
               LET g_reg.porcentaje       = l_record[pos].porciento
               LET g_reg.fecha            = l_record[pos].dato_fecha
               LET g_reg.tipo_comparativo = l_record[pos].tipo_comparativo
               LET g_reg.periodo          = l_record[pos].periodo
               LET v_desc_periodo         = l_record[pos].desc_periodo
               EXIT DISPLAY
            ON KEY (control-p)
               ERROR "PROCESANDO INFORMACION..."
               CALL impresion(pos)
            ON KEY (INTERRUPT)
            EXIT DISPLAY
         END DISPLAY

         CLOSE WINDOW ventana_2
      ELSE
         ERROR "ARCHIVO DE COMPARATIVOS... VACIO"
         SLEEP 1
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF
   END IF

   SELECT desc_tipo_comparativo
   INTO   v_desc_tipo
   FROM   safre_af:tab_tipo_comparativo
   WHERE  tipo_comparativo = g_reg.tipo_comparativo

   SELECT siefore_desc
   INTO   vsiefore_desc
   FROM   safre_af:tab_siefore
   WHERE  siefore_cod = g_reg.siefore_cod
   AND    afore_cod   = g_reg.afore_cod

   DISPLAY BY NAME g_reg.afore_cod,
                   g_reg.siefore_cod,
                   g_reg.porcentaje,
                   g_reg.fecha,
                   g_reg.tipo_comparativo,
                   v_desc_tipo,
                   g_reg.periodo,
                   v_desc_periodo

   DISPLAY BY NAME vsiefore_desc

      SELECT afore_desc
      INTO   x_afore_desc
      FROM   tab_afore
      WHERE  afore_cod = g_reg.afore_cod
      DISPLAY BY NAME x_afore_desc                   

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "CONSULTA CANCELADA ..."
         SLEEP 2
         ERROR ""
         CLEAR FORM
         CLEAR SCREEN
         RETURN
      ELSE
         PROMPT "<ENTER> Para Salir de la Consulta" for aux_pausa
      END IF

   CLEAR FORM

   CLEAR SCREEN

END FUNCTION
################################################################################
FUNCTION  Modifica()

   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0782" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta                                            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "               Escoja con <ENTER> el Comparativo a modificar                 " AT 2,1
      DISPLAY "                         COMPARATIVO  DE  COMISIONES                           " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 
      LET int_flag = FALSE

      CONSTRUCT cla_where ON afore_cod,
                             fecha,
                             tipo_comparativo,
                             periodo
                        FROM afore_cod,
                             fecha,
                             tipo_comparativo,
                             periodo
         ON KEY (control-m)
            ERROR "PROCESANDO INFORMACION..."
            LET int_flag = FALSE
         EXIT CONSTRUCT
	 ON KEY (control-c)
            LET int_flag = TRUE
	 EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
	 ERROR "BUSQUEDA CANCELADA..."
	 SLEEP 1
	 ERROR ""
	 CLEAR SCREEN 
	 CLOSE WINDOW ventana_2
         RETURN
      END IF
      LET sel_where = "SELECT afore_cod,siefore_cod,porcentaje,fecha,",
                      "tipo_comparativo,periodo FROM tab_comparativo WHERE ",
                      cla_where CLIPPED,
	                  "ORDER BY 3,1 "
      PREPARE query1 FROM sel_where
      DECLARE cursor_2 CURSOR FOR query1
      LET pos = 1

      FOREACH cursor_2 INTO l_record[pos].codigo,           
                            l_record[pos].siefore,          
                            l_record[pos].porciento,        
                            l_record[pos].dato_fecha,       
                            l_record[pos].tipo_comparativo, 
                            l_record[pos].periodo
      
      IF l_record[pos].periodo IS NOT NULL THEN
            LET l_record[pos].desc_periodo = "ANIO(S)"
      ELSE 
            LET l_record[pos].desc_periodo = ""
      END IF

      SELECT desc_tipo_comparativo
        INTO l_record[pos].desc_tipo
        FROM safre_af:tab_tipo_comparativo
       WHERE tipo_comparativo = l_record[pos].tipo_comparativo

         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL  SET_COUNT(pos-1)
         ERROR ""	


	 DISPLAY ARRAY l_record TO scr_1.*
            ON KEY (control-m)
	       LET pos = ARR_CURR()
	           LET g_reg.afore_cod        = l_record[pos].codigo
	            LET g_reg.siefore_cod      = l_record[pos].siefore
               LET g_reg.porcentaje       = l_record[pos].porciento
               LET g_reg.fecha            = l_record[pos].dato_fecha 
               LET g_reg.tipo_comparativo = l_record[pos].tipo_comparativo
               LET g_reg.periodo          = l_record[pos].periodo
               LET v_desc_periodo         = l_record[pos].desc_periodo

	    EXIT DISPLAY
            ON KEY (INTERRUPT)
               ERROR "DEBE ELEGIR UN REGISTRO."
	       LET pos = ARR_CURR()
	 END DISPLAY

	 CLOSE WINDOW ventana_2
      ELSE
         ERROR "ARCHIVO DE COMPARATIVOS... VACIO"
         SLEEP 1
	 ERROR ""
	 CLOSE WINDOW ventana_2
         RETURN
      END IF

   SELECT siefore_desc
   INTO   vsiefore_desc
   FROM   safre_af:tab_siefore
   WHERE  siefore_cod = g_reg.siefore_cod
   AND    afore_cod   = g_reg.afore_cod
   DISPLAY BY NAME vsiefore_desc

   SELECT desc_tipo_comparativo
   INTO   v_desc_tipo
   FROM   safre_af:tab_tipo_comparativo
   WHERE  tipo_comparativo = g_reg.tipo_comparativo
   DISPLAY BY NAME v_desc_tipo,
                   v_desc_periodo

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
      DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE,BOLD)

      INPUT BY NAME g_reg.afore_cod,
                    g_reg.siefore_cod,
                    g_reg.porcentaje,
                    g_reg.fecha,
                    g_reg.tipo_comparativo,
                    g_reg.periodo  WITHOUT DEFAULTS
         BEFORE FIELD afore_cod
            SELECT afore_desc
            INTO   x_afore_desc
            FROM   tab_afore
            WHERE  afore_cod = g_reg.afore_cod
            DISPLAY BY NAME x_afore_desc
            NEXT FIELD porcentaje
	 
	  AFTER FIELD porcentaje
	    IF g_reg.porcentaje IS NULL THEN
	       ERROR "PORCENTAJE DE AFORE NO DEBE SER NULO."
	       NEXT FIELD porcentaje
	    END IF
       
         CALL Pregunta()

         IF aux_pausa MATCHES "[Ss]" THEN
            UPDATE tab_comparativo
            SET    porcentaje       = g_reg.porcentaje,
                   fecha            = fecha,
                   tipo_comparativo = g_reg.tipo_comparativo,
                   periodo          = g_reg.periodo,
                   usuario          = usuario,
                   factualiza       = TODAY
	    WHERE  afore_cod = g_reg.afore_cod	
            AND    fecha     = g_reg.fecha
            AND    tipo_comparativo = g_reg.tipo_comparativo
            AND    siefore_cod      = g_reg.siefore_cod

	    ERROR "REGISTRO MODIFICADO"
	    SLEEP 1
	    ERROR ""
            CALL Inicializa()
	 ELSE
            ERROR "PROCESO DE MODIFICAR CANCELADO."
	    SLEEP 1
	    ERROR "" 
	 END IF

         EXIT INPUT

         ON KEY (INTERRUPT)
            CALL Inicializa()
         EXIT INPUT

      END INPUT
   ELSE
      ERROR "ARCHIVO DE COMPARATIVOS... VACIO."
   END IF

   CLEAR SCREEN

END FUNCTION
################################################################################
FUNCTION Elimina()

   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0782" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta                                            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "               Escoja con <ENTER> el Comparativo a eliminar                  " AT 2,1
      DISPLAY "                         COMPARATIVO  DE  COMISIONES                           " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 
      LET int_flag = FALSE

      CONSTRUCT cla_where ON afore_cod,
                             fecha,
                             tipo_comparativo,
                             periodo
                        FROM afore_cod,
                             fecha,
                             tipo_comparativo,
                             periodo
         ON KEY (control-m)
	    ERROR "PROCESANDO INFORMACION..."
	    LET int_flag = FALSE
	 EXIT CONSTRUCT
	 ON KEY (control-c)
	    LET int_flag = TRUE
	 EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
	 ERROR "BUSQUEDA CANCELADA..."
	 SLEEP 1
	 ERROR ""
	 CLEAR SCREEN
	 CLOSE WINDOW ventana_2
         RETURN
      END IF

      LET sel_where = "SELECT afore_cod,siefore_cod,porcentaje,fecha,",
                      "tipo_comparativo,periodo FROM tab_comparativo WHERE ",
                      cla_where CLIPPED,
	                  "ORDER BY 3,1 "
      PREPARE query2 FROM sel_where
      DECLARE cursor_3 CURSOR FOR query2
      LET pos = 1

      FOREACH cursor_3 INTO l_record[pos].codigo,           
                            l_record[pos].siefore,          
                            l_record[pos].porciento,        
                            l_record[pos].dato_fecha,       
                            l_record[pos].tipo_comparativo, 
                            l_record[pos].periodo

      IF l_record[pos].periodo IS NOT NULL THEN
            LET l_record[pos].desc_periodo = "ANIO(S)"
      ELSE 
            LET l_record[pos].desc_periodo = ""
      END IF

      SELECT desc_tipo_comparativo
        INTO l_record[pos].desc_tipo
        FROM safre_af:tab_tipo_comparativo
       WHERE tipo_comparativo = l_record[pos].tipo_comparativo

         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL  SET_COUNT(pos-1)
         ERROR ""	
	 DISPLAY ARRAY l_record TO scr_1.*
            ON KEY (control-m)
	       LET pos = ARR_CURR()
               LET g_reg.afore_cod  = l_record[pos].codigo
               LET g_reg.siefore_cod = l_record[pos].siefore
               LET g_reg.porcentaje = l_record[pos].porciento
               LET g_reg.fecha      = l_record[pos].dato_fecha
               LET g_reg.tipo_comparativo = l_record[pos].tipo_comparativo
               LET g_reg.periodo          = l_record[pos].periodo
               LET v_desc_periodo         = l_record[pos].desc_periodo

	    EXIT DISPLAY
            ON KEY (INTERRUPT)
               ERROR "DEBE ELEGIR UN REGISTRO."
	       LET pos = ARR_CURR()
	 END DISPLAY
	 CLOSE WINDOW ventana_2
      ELSE
         ERROR "ARCHIVO DE COMPARATIVOS... VACIO"
         SLEEP 1
	 ERROR ""
	 CLOSE WINDOW ventana_2
         RETURN
      END IF

   SELECT desc_tipo_comparativo
   INTO   v_desc_tipo
   FROM   safre_af:tab_tipo_comparativo
   WHERE  tipo_comparativo = g_reg.tipo_comparativo
   DISPLAY BY NAME v_desc_tipo,
                   v_desc_periodo

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
      DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY BY NAME g_reg.afore_cod,
                      g_reg.siefore_cod,
                      g_reg.porcentaje,
                      g_reg.fecha,
                      g_reg.tipo_comparativo,
                      v_desc_tipo,
                      g_reg.periodo,
                      v_desc_periodo
      SELECT afore_desc
      INTO   x_afore_desc
      FROM   tab_afore
      WHERE  afore_cod = g_reg.afore_cod
      DISPLAY BY NAME x_afore_desc

      SELECT siefore_desc
      INTO   vsiefore_desc
      FROM   safre_af:tab_siefore
      WHERE  siefore_cod = g_reg.siefore_cod
      AND    afore_cod   = g_reg.afore_cod
      DISPLAY BY NAME vsiefore_desc

      CALL Pregunta()

      IF aux_pausa MATCHES "[Ss]" THEN
         DELETE FROM tab_comparativo
         WHERE afore_cod      = g_reg.afore_cod
         AND   siefore_cod    = g_reg.siefore_cod
         AND   fecha          = g_reg.fecha
         AND tipo_comparativo = g_reg.tipo_comparativo
         AND periodo          = g_reg.periodo
	
         ERROR "REGISTRO ELIMINADO."
	 SLEEP 1
	 ERROR ""
      ELSE
	 ERROR "PROCESO CANCELADO."
	 SLEEP 1
	 ERROR ""
      END IF

      CALL Inicializa()
   ELSE
      ERROR "ARCHIVO DE COMPARATIVOS... VACIO."
   END IF

   CLEAR SCREEN

END FUNCTION
################################################################################
FUNCTION Pregunta()
   
   PROMPT "Esta seguro de realizar esta operación S/N ? " FOR CHAR aux_pausa

END FUNCTION
################################################################################
FUNCTION impresion(pos)

   DEFINE i, pos SMALLINT
   LET g_impre = g_param_dis.ruta_listados CLIPPED,
		 "/",usuario CLIPPED,".IMPTRECHA",
		 hoy USING "dd-mm-yyyy" CLIPPED
   START REPORT rpt_tabrechcurp TO g_impre
   FOR i = 1 TO (pos+1)
      LET g_reg.afore_cod        = l_record[i].codigo
      LET g_reg.siefore_cod      = l_record[i].siefore
      LET g_reg.porcentaje       = l_record[i].porciento
      LET g_reg.fecha            = l_record[i].dato_fecha
      LET g_reg.tipo_comparativo = l_record[pos].tipo_comparativo
      LET g_reg.periodo          = l_record[pos].periodo

      IF g_reg.afore_cod IS NULL THEN
         EXIT FOR
      END IF
      OUTPUT TO REPORT rpt_tabrechcurp(g_reg.*)
   END FOR

   FINISH REPORT rpt_tabrechcurp
   ERROR "LISTADO GENERADO..."
   SLEEP 1
   ERROR ""
   LET g_lista = "lp ",g_impre
   RUN g_lista

END FUNCTION
################################################################################
REPORT rpt_tabrechcurp(g_reg)

   DEFINE g_reg		RECORD 
          afore_cod        SMALLINT,
          siefore_cod      SMALLINT,
          porcentaje       DECIMAL(6,2),
          fecha            DATE,
          tipo_comparativo SMALLINT,
          periodo          SMALLINT,
          usuario           CHAR(8),
          factualiza        DATE
   END RECORD
   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
      PAGE HEADER
         PRINT COLUMN 03," TABM078 ",
               COLUMN 21," CATALOGO COMPARATIVO DE COMISIONES ",
               COLUMN 67,TODAY USING "dd-mm-yyyy"
         SKIP 2 LINE

         PRINT COLUMN 05,"AFORE",
               COLUMN 09,"SIEFORE",
               COLUMN 10,"PORCENTAJE",
               COLUMN 15,"FECHA",
               COLUMN 25,"   TIPO"
         PRINT COLUMN 25,"COMPARATIVO",
               COLUMN 30,"PERIODO"
               
         SKIP 1 LINE
      ON EVERY ROW
         PRINT COLUMN 05,g_reg.afore_cod ,
               COLUMN 09,g_reg.siefore_cod,
               COLUMN 10,g_reg.porcentaje,
               COLUMN 15,g_reg.fecha USING "dd-mm-yyyy",
               COLUMN 25,g_reg.tipo_comparativo,
               COLUMN 30,g_reg.periodo
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
      ON LAST ROW
         SKIP 4 LINE 
         PRINT COLUMN 03, "Total de registros: ",COUNT(*) USING "<<<<"
END REPORT


################################################################################

FUNCTION dia_habil(diaActual)
#hs--------------------------------

    DEFINE
        diaTmp    DATE,
        contador  SMALLINT,
        diaActual DATE

    DEFINE
        diaHabilSig DATE,
        diaSemana   SMALLINT,
        feriado     SMALLINT,
        finSemana   SMALLINT,
	vhabil      SMALLINT

    LET diaHabilSig = diaActual

    WHILE TRUE
        LET feriado   = 0
        LET finSemana = 0
        LET vhabil      = 0
        LET diaSemana = WEEKDAY(diaHabilSig)

        IF diaSemana = 0 OR diaSemana = 6 THEN
            LET finSemana = 1
        END IF

        SELECT *
        FROM   tab_feriado 
        WHERE  feria_fecha = diaHabilSig

        IF STATUS <> NOTFOUND THEN
            LET feriado = 1
        END IF

        IF feriado = 1 OR finSemana = 1 THEN
	    LET vhabil = 1
            EXIT WHILE
        ELSE
	    LET vhabil = 0
            EXIT WHILE
        END IF

    END WHILE

    RETURN vhabil

END FUNCTION

FUNCTION carga_archivo()

    DEFINE g_plano_indices INTEGER
    DEFINE generar CHAR(16),
           varchivo CHAR(16)

   DEFINE cadena CHAR(200)

    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp

        DROP TABLE tab_indice_rend
    WHENEVER ERROR STOP

    CREATE TABLE tab_indice_rend
        (campo1  SMALLINT,      ---afore
         campo2  SMALLINT,      ---siefore
         campo3  DECIMAL(6,2),  ---porcentaje
         campo4  DATE,          ---fecha indice
         campo5  SMALLINT,      ---tipo_comparativo
         campo6  SMALLINT)      ---periodo

        DATABASE safre_af

    OPEN WINDOW ventana_5 AT 4,4 WITH FORM "TABM0785" ATTRIBUTE(BORDER)
    DISPLAY " TABM078              CARGA INDICES DE RENDIMIENTO NETO                            " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                           < Ctrl-C > Salir                                        " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)
    DISPLAY g_param_dis.ruta_rescate AT 6,10

    LET int_flag = FALSE

    INPUT BY NAME generar

    AFTER FIELD generar
        IF generar IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD generar
        END IF

        SELECT @nombre_archivo
        INTO   varchivo
        FROM   tab_ctr_arh_proc
        WHERE  @nombre_archivo = generar

        IF STATUS <> NOTFOUND THEN
            ERROR "ARCHIVO YA PROCESADO"
            SLEEP 1
            ERROR " "
            INITIALIZE generar TO NULL
            CLEAR FORM
            NEXT FIELD generar
        END IF

        LET carga = NULL
        LET carga = g_param_dis.ruta_rescate CLIPPED,"/",
                    generar CLIPPED

        LET cadena ="vi ",carga CLIPPED, "<cambia.cmd>/dev/null"
        RUN cadena CLIPPED 

        WHENEVER ERROR CONTINUE
            LOAD FROM carga DELIMITER ","
            INSERT INTO safre_tmp:plano_indices
        WHENEVER ERROR STOP

        SELECT COUNT(*) 
        INTO   g_plano_indices
        FROM   safre_tmp:plano_indices

        IF g_plano_indices IS NULL OR
           g_plano_indices = 0 THEN
            ERROR "NOMBRE DE ARCHIVO INCORRECTO O VACIO, VERIFIQUE " 
            SLEEP 3
            EXIT PROGRAM
        END IF

        ERROR "Procesando Informacion"
        SLEEP 2
        ERROR ""

        CALL actualiza_datos(generar)

        ON KEY ( INTERRUPT )
        LET int_flag = TRUE
        ERROR "proceso_cancelado"
        SLEEP 2
        EXIT PROGRAM

        ON KEY (control-c)
        LET int_flag = TRUE
        ERROR "proceso_cancelado"
        SLEEP 2
        EXIT PROGRAM
{
        IF int_flag = TRUE THEN
           LET int_flag = FALSE
           ERROR "BUSQUEDA CANCELADA..."
           SLEEP 2
           ERROR ""
           CLEAR SCREEN
           EXIT INPUT
           CLOSE WINDOW ventana_5
           RETURN
        END IF
} 
        CLOSE WINDOW ventana_5
        --EXIT PROGRAM
        RETURN

    END INPUT

END FUNCTION

FUNCTION actualiza_datos(archivo)
#-------------------------

    DEFINE 
        cont_reg ,
        cont_reg2,
        cont_reg3,
        total_reg     INTEGER

    DEFINE archivo CHAR(16)

    DEFINE carga_reg     RECORD
        afore        SMALLINT,
        siefore      SMALLINT,
        porcentaje   DECIMAL(7,3),
        fecha_indice CHAR(10),
        tipo_comp    SMALLINT,
        periodo      SMALLINT
    END RECORD

    DEFINE   
         vafore            SMALLINT,      ---afore
         vsiefore          SMALLINT,      ---siefore
         vporcentaje       DECIMAL(7,3),      ---porcentaje
         vfecha_indice     CHAR(10),      ---fecha indice
         vtipo_comparativo SMALLINT,      ---tipo_comparativo
         vperiodo          SMALLINT       ---periodo

    DEFINE vfecha   CHAR(10)
    DEFINE ban_afo  SMALLINT
    DEFINE ban_porc SMALLINT
    DEFINE ban_tip  SMALLINT
    DEFINE ban_fec  SMALLINT
    DEFINE ban_per  SMALLINT
    DEFINE contador INTEGER

    DEFINE GIMPRE_RECH CHAR(200)
    DEFINE desc_err    CHAR(200)


    LET cont_reg = 0
    LET cont_reg2= 0
    LET cont_reg3= 0
    LET ban_afo  = 0
    LET ban_porc = 0
    LET ban_tip  = 0
    LET ban_fec  = 0
    LET contador = 0
    INITIALIZE GIMPRE_RECH TO NULL

    LET GIMPRE_RECH    = g_param_dis.ruta_listados CLIPPED,"/", 
                         usuario CLIPPED,
                         ".REND_NETO_ERR.",
                         TODAY USING "DDMMYY"

    START REPORT rep_rechazos TO GIMPRE_RECH 

    INSERT INTO tab_ctr_arh_proc 
    VALUES (archivo, cont_reg,cont_reg2,cont_reg3, TODAY)

    SELECT COUNT(*)
    INTO   total_reg
    FROM   safre_tmp:plano_indices

    DECLARE cursor_ind CURSOR FOR 
    SELECT  *
    FROM    safre_tmp:plano_indices

    FOREACH cursor_ind INTO carga_reg.*
        LET ban_afo  = 0
        LET ban_porc = 0
        LET ban_tip  = 0
        LET ban_fec  = 0
        LET cont_reg = cont_reg + 1

        IF cont_reg <> total_reg  THEN
            LET vafore            = carga_reg.afore
            LET vsiefore          = carga_reg.siefore
            LET vporcentaje       = carga_reg.porcentaje
            LET vfecha_indice     = carga_reg.fecha_indice
            LET vtipo_comparativo = carga_reg.tipo_comp
            LET vperiodo          = carga_reg.periodo


           IF vfecha_indice IS NOT NULL THEN 
               IF (vfecha_indice[1,2] > 12 OR
                  vfecha_indice[4,5] > 31) OR
                  (vfecha_indice[1,2] = 2 AND
                   vfecha_indice[4,5] > 29) THEN
                   LET ban_fec = 1
               ELSE
                   LET ban_fec = 0
                   LET vfecha =  vfecha_indice[1,2],"/",
                                 vfecha_indice[4,5],"/",
                                 vfecha_indice[7,10]
                   {SELECT COUNT(*) 
                   INTO contador
                   FROM tab_comparativo
                   WHERE fecha = vfecha
                   IF contador > 0 THEN
                      LET ban_fec = 1
                   ELSE
                      LET ban_fec = 0
                   END IF}
               END IF
           ELSE
               LET vfecha = NULL
               LET ban_fec = 1
           END IF 
        END IF

        SELECT 'X' FROM tab_afore
        WHERE afore_cod = vafore

        IF SQLCA.SQLCODE = NOTFOUND THEN
           LET ban_afo = 1
        ELSE
           LET ban_afo = 0
        END IF

        IF vporcentaje > 100 THEN
           LET ban_porc = 1
        ELSE
           LET ban_porc = 0
        END IF

        IF vperiodo > 25 THEN
           LET ban_per = 1
        ELSE
           LET ban_per = 0
        END IF

        SELECT 'X' FROM tab_tipo_comparativo
        WHERE tipo_comparativo = vtipo_comparativo

        IF SQLCA.SQLCODE = NOTFOUND THEN
           LET ban_tip = 1
        ELSE
           LET ban_tip = 0
        END IF

        IF  ban_afo  = 0 AND
            ban_porc = 0 AND
            ban_per  = 0 AND
            ban_tip  = 0 AND
            ban_fec  = 0 THEN

            LET cont_reg2 = cont_reg2 + 1

            INSERT INTO tab_comparativo
            VALUES (vafore,
                    vsiefore,
                    vporcentaje,
                    vfecha_indice,
                    vtipo_comparativo,
                    vperiodo,
                    usuario,
                    TODAY)
        ELSE
           LET cont_reg3 = cont_reg3 + 1      #rechazados

          INITIALIZE desc_err TO NULL

          IF ban_afo  = 1 THEN
             LET desc_err = desc_err CLIPPED,"Afore,"
          END IF
          IF ban_porc = 1 THEN
             LET desc_err = desc_err CLIPPED, "porcent,"
          END IF
          IF ban_per = 1 THEN
             LET desc_err = desc_err CLIPPED,"period,"
          END IF
          IF ban_tip  = 1 THEN
             LET desc_err = desc_err CLIPPED, "Tipo Comp,"
          END IF
          IF ban_fec = 1 THEN
             LET desc_err = desc_err CLIPPED, "Fecha"
          END IF

          OUTPUT TO REPORT rep_rechazos 
                   (vafore,
                    vsiefore,
                    vporcentaje,
                    vfecha_indice,
                    vtipo_comparativo,
                    vperiodo,
                    desc_err)

        END IF

    END FOREACH

    FINISH REPORT rep_rechazos

    CALL despliega_totales(cont_reg,cont_reg2,cont_reg3)

END FUNCTION

FUNCTION despliega_totales(contreg1,contreg2,contreg3)
#dt-------------------------
DEFINE contreg1,
       contreg2,
       contreg3 INTEGER

DEFINE enter CHAR(1)

        DISPLAY "                             DATOS A PROCESAR                                   "
            AT 8,1 ATTRIBUTE ( REVERSE )

        DISPLAY "Total de Registros del Archivo     : ",
                 contreg1 USING "#######&" AT 9,15 ATTRIBUTE ( BOLD )

        DISPLAY "Registros aceptados : ",
                 contreg2 USING "#######&" AT 10,15 ATTRIBUTE ( BOLD )

        DISPLAY "Registros rechazados   : ",
                 contreg3 USING "#######&" AT 11,15 ATTRIBUTE ( BOLD )

        PROMPT "<Enter> para continuar" FOR enter
        EXIT PROGRAM

END FUNCTION

REPORT rep_rechazos(rafore_cod,rsiefore,rporcentaje,rfecha,rtipo_comparativo,
                    rperiodo,rdesc_err)

   DEFINE
          rafore_cod        SMALLINT,
          rsiefore          SMALLINT,
          rporcentaje       DECIMAL(6,2),
          rfecha            DATE,
          rtipo_comparativo SMALLINT,
          rperiodo          SMALLINT,
          rdesc_err         CHAR(100)

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
      PAGE HEADER
         PRINT COLUMN 03," TABM078 ",
               COLUMN 21," RECHAZOS DE INDICES DE RENDIMIENTO NETO",
               COLUMN 67,TODAY USING "dd-mm-yyyy"
         SKIP 2 LINE

         PRINT COLUMN 01,"AFORE",
               COLUMN 09,"SIEFORE",
               COLUMN 18,"PORCENTAJE",
               COLUMN 30,"FECHA",
               COLUMN 40,"TIP COMP",
               COLUMN 50,"PERIODO",
               COLUMN 60,"OBSERVACION"
               
         SKIP 1 LINE
      ON EVERY ROW
         PRINT COLUMN 01,rafore_cod ,
               COLUMN 09,rsiefore,
               COLUMN 18,rporcentaje,
               COLUMN 30,rfecha USING "dd-mm-yyyy",
               COLUMN 35,rtipo_comparativo,
               COLUMN 45,rperiodo,
               COLUMN 55,rdesc_err CLIPPED
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
      ON LAST ROW
         SKIP 4 LINE 
         PRINT COLUMN 03, "Total de registros: ",COUNT(*) USING "<<<<"
END REPORT

