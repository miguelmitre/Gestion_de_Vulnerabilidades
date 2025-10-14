################################################################################
#Proyecto                   : Sistema de AFORES (México)                       #
#Owner                      : E.F.P.                                           #
#Programa TABM133           : Rendimiento Neto                                 #
#Fecha                      : 08 de Abril de 2008                              #
#Por                        : Eduardo Joaquin Resendiz Medina                  #
#Sistema                    : TAB                                              #
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
          rendimiento      DECIMAL(6,2),
          comision         DECIMAL(6,2),
          rendimiento_neto DECIMAL(6,2),
          fecha_ini        DATE,
          fecha_fin        DATE,
          orden_consar     SMALLINT,
          fecha_cifras_al  DATE,
          usuario          CHAR(8), 
          factualiza       DATE
   END RECORD

   DEFINE l_record  ARRAY[30000] OF RECORD
          codigo           SMALLINT,
          siefore          SMALLINT,
          rendimiento      DECIMAL(6,2),
          comision         DECIMAL(6,2),
          rendimiento_neto DECIMAL(6,2),
          dato_fecha_ini   DATE,
          fecha_fin        DATE,
          orden_consar     SMALLINT,
          fecha_cifras_al  DATE
   END RECORD

   DEFINE x_afore_desc   CHAR(40)
   DEFINE vsiefore_desc CHAR(40)
   DEFINE vafore_f       SMALLINT

   DEFINE vhabil         SMALLINT
   DEFINE vrend_neto     DECIMAL(6,2)
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

   DEFINE vdia_fin,
          vmes_fin      SMALLINT,
          vdia_fin2     CHAR(2),
          vmes_fin2     CHAR(2),
          vanio_fin     CHAR(4),
          vfecha_fin    CHAR(10),
          vfecha_ini    CHAR(10)

   DEFINE cla_where2             CHAR(1000),
          carga                  CHAR(100)
END GLOBALS

MAIN

   OPTIONS PROMPT LINE LAST,
   INPUT WRAP              ,
   ACCEPT KEY control-o

   DEFER INTERRUPT
   CALL STARTLOG("TABM133.log")
   CALL inicio()
   CALL proceso()

END MAIN

FUNCTION inicio()

   SELECT USER, ruta_listados,ruta_rescate
   INTO   usuario,
          g_param_dis.ruta_listados,
          g_param_dis.ruta_rescate
   FROM   seg_modulo
   WHERE  modulo_cod = "tab"

   LET vhabil = 0

   DATABASE safre_tmp
    WHENEVER ERROR CONTINUE
      DROP TABLE plano_rend_neto
    WHENEVER ERROR STOP

      CREATE TABLE plano_rend_neto
          (afore SMALLINT,
           siefore SMALLINT,
           rendimiento DECIMAL(6,2),
           comision DECIMAL(6,2),
           rendimento_neto DECIMAL(6,2),
           fecha_ini_indice CHAR(10),
           fecha_fin CHAR(10),
           orden_consar SMALLINT,
           fecha_cifras_al DATE)

   DATABASE safre_af

END FUNCTION

FUNCTION proceso()

   LET HOY = TODAY
   OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM1331" ATTRIBUTE( BORDER)
   DISPLAY        " TABM133                CATALOGO RENDIMIENTO NETO                              " AT 3,1 ATTRIBUTE(REVERSE) 
   DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)

   MENU "plano_rend_neto"
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
      COMMAND "Carga Archivo" "Carga Archivo de Rendimiento Neto "
         CALL carga_archivo()
         CLEAR SCREEN
      COMMAND "Salir" "Salir del Programa "
         EXIT MENU
   END MENU
   CLOSE WINDOW ventana_1

END FUNCTION

FUNCTION Inicializa()

   LET sw_1 = 0
   INITIALIZE x_afore_desc TO NULL
   INITIALIZE g_reg.* TO NULL
   INITIALIZE vsiefore_desc TO NULL
   DISPLAY BY NAME g_reg.afore_cod
   DISPLAY BY NAME g_reg.siefore_cod
   DISPLAY BY NAME g_reg.rendimiento
   DISPLAY BY NAME g_reg.comision
   DISPLAY BY NAME g_reg.rendimiento_neto
   DISPLAY BY NAME g_reg.fecha_ini
   DISPLAY BY NAME g_reg.fecha_fin
   DISPLAY BY NAME g_reg.orden_consar
   DISPLAY BY NAME g_reg.fecha_cifras_al
   DISPLAY BY NAME x_afore_desc
   DISPLAY BY NAME vsiefore_desc

END FUNCTION

FUNCTION Agrega()
	
   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " ( Esc ) Agrega                 (Ctrl-c) Salir                                 " AT 1,1 ATTRIBUTE(BOLD)
   DISPLAY " AGREGA " AT 1,69 ATTRIBUTE(REVERSE,BOLD)
   LET g_reg.rendimiento = NULL
   LET sw_1 = 0
   INPUT BY NAME  g_reg.afore_cod,
                  g_reg.siefore_cod,
                  g_reg.rendimiento,
                  g_reg.comision,
                  --g_reg.rendimiento_neto,
                  g_reg.fecha_ini,
                  --g_reg.fecha_fin
                  g_reg.orden_consar,
                  g_reg.fecha_cifras_al

      BEFORE FIELD afore_cod
         IF sw_1 = 0 THEN
            LET sw_1 = 1
           DISPLAY BY NAME g_reg.afore_cod
         END IF

      AFTER FIELD afore_cod
     IF g_reg.afore_cod IS NULL THEN
        OPEN WINDOW vent_1 AT 07,15 WITH FORM "TABM1333" 
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

        IF SQLCA.SQLCODE <> 0 THEN
           SELECT @afore_desc, @afore_fusion
           INTO   x_afore_desc, vafore_f
           FROM   safre_af:tab_afore
           WHERE  @afore_cod = g_reg.afore_cod

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
        OPEN WINDOW vent_sie AT 07,15 WITH FORM "TABM1336" 
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
           NEXT FIELD rendimiento
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
           NEXT FIELD rendimiento
        END IF

     END IF

###################<

      AFTER FIELD rendimiento
         IF g_reg.rendimiento IS NULL OR
            g_reg.rendimiento = 0 THEN
            ERROR " EL RENDIMIENTO NO PUEDE SER NULO."
            NEXT FIELD  rendimiento
         END IF

         {IF g_reg.rendimiento < 0 THEN
            ERROR " EL RENDIMIENTO NO PUEDE SER MENOR A 0%"
            NEXT FIELD rendimiento
         END IF}

      AFTER FIELD comision
         IF g_reg.comision IS NULL OR
            g_reg.comision = 0 THEN
            ERROR " LA COMISION NO PUEDE SER NULO."
            NEXT FIELD  comision
         END IF

         IF g_reg.comision < 0 THEN
            ERROR " LA COMISION NO PUEDE SER MENOR A 0%"
            NEXT FIELD comision
         END IF
{
      AFTER FIELD rendimiento_neto
         IF g_reg.rendimiento_neto IS NULL OR
            g_reg.rendimiento_neto = 0 THEN
            ERROR " EL RENDIMIENTO NO PUEDE SER NULO."
            NEXT FIELD  rendimiento_neto
         END IF

         LET vrend_neto = g_reg.rendimiento - g_reg.comision

         IF vrend_neto <> g_reg.rendimiento_neto THEN
            ERROR "EL IMPORTE DEL RENDIMIENTO NETO NO ES CORRECTO "
            NEXT FIELD rendimiento
         END IF
}
         LET vrend_neto = g_reg.rendimiento - g_reg.comision
         LET g_reg.rendimiento_neto = vrend_neto
         DISPLAY vrend_neto TO rendimiento_neto

      AFTER FIELD fecha_ini
         IF g_reg.fecha_ini IS NULL OR g_reg.fecha_ini = 0 THEN
            ERROR " LA FECHA INICIO NO PUEDE SER NULO"
            NEXT FIELD fecha_ini
         ELSE                      
            SELECT "OK"   
            FROM   tab_rendimiento_neto
            WHERE  afore_cod   = g_reg.afore_cod 
            AND    siefore_cod = g_reg.siefore_cod
            AND   fecha_ini        = g_reg.fecha_ini
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
               ERROR  " REGISTRO YA EXISTE, VERIFIQUE... " 
               NEXT FIELD fecha_ini
            END IF
         END IF 
{
         CALL dia_habil(g_reg.fecha_ini) RETURNING vhabil
         IF vhabil = 1 THEN
           ERROR "LA FECHA DE CAPTURA NO ES DIA HABIL"
           NEXT FIELD fecha_ini
         END IF  

         IF g_reg.fecha_ini > HOY THEN
            ERROR "FECHA CAPTURA NO PUEDE SER MAYOR AL DIA DE HOY"
            NEXT FIELD fecha_ini
         END IF
}
         LET vfecha_ini = g_reg.fecha_ini
         IF vfecha_ini[4,5] <> 15 THEN
            ERROR "SOLO SE PREMITE CAPTURAR EL DÍA 15 DE CADA MES"
            NEXT FIELD fecha_ini
         END IF

         SELECT "OK"   
         FROM   tab_rendimiento_neto
         WHERE  afore_cod        = g_reg.afore_cod 
         AND    fecha_ini            = g_reg.fecha_ini
         AND    siefore_cod      = g_reg.siefore_cod
         GROUP BY 1

         IF STATUS <> NOTFOUND THEN
            ERROR  " REGISTRO YA EXISTE, VERIFIQUE... " 
            NEXT FIELD fecha_ini
         END IF

         LET vdia_fin        = vfecha_ini[4,5]
         LET vdia_fin        = vdia_fin - 1
         LET vmes_fin        = vfecha_ini[1,2]

         IF vmes_fin = 12 THEN
            LET vmes_fin        = 1
         ELSE
            LET vmes_fin        = vmes_fin + 1
         END IF

         IF vmes_fin = 1 THEN
            LET vanio_fin       = vfecha_ini[7,10] + 1
         ELSE
            LET vanio_fin       = vfecha_ini[7,10]
         END IF

         IF vdia_fin = 1 OR
            vdia_fin = 2 OR
            vdia_fin = 3 OR
            vdia_fin = 4 OR
            vdia_fin = 5 OR
            vdia_fin = 6 OR
            vdia_fin = 7 OR
            vdia_fin = 8 OR
            vdia_fin = 9 THEN
               LET vdia_fin2 = "0",vdia_fin
         ELSE
               LET vdia_fin2 = vdia_fin
         END IF

         IF vmes_fin = 1 OR
            vmes_fin = 2 OR
            vmes_fin = 3 OR
            vmes_fin = 4 OR
            vmes_fin = 5 OR
            vmes_fin = 6 OR
            vmes_fin = 7 OR
            vmes_fin = 8 OR
            vmes_fin = 9 THEN
               LET vmes_fin2 = "0",vmes_fin
         ELSE
               LET vmes_fin2 = vmes_fin
         END IF

         LET vfecha_fin      = vmes_fin2 CLIPPED,"/",vdia_fin2 CLIPPED,"/",vanio_fin CLIPPED
         LET g_reg.fecha_fin = vfecha_fin

         DISPLAY BY NAME g_reg.fecha_fin

         NEXT FIELD orden_consar

      AFTER FIELD orden_consar
         IF g_reg.orden_consar IS NULL OR
            g_reg.orden_consar = 0 THEN
            ERROR " EL ORDEN CONSAR NO PUEDE SER NULO O CERO"
            NEXT FIELD orden_consar
         ELSE
            SELECT 'X'
            FROM tab_rendimiento_neto
            WHERE siefore_cod = g_reg.siefore_cod
            AND   fecha_ini   = g_reg.fecha_ini
            AND   fecha_fin   = g_reg.fecha_fin
            AND   orden_consar= g_reg.orden_consar

            IF SQLCA.SQLCODE = 0 THEN
               ERROR "ORDEN CONSAR NO PUEDE SER REPETIDO PARA ESTA SIEFORE "
               NEXT FIELD orden_consar
            ELSE
               NEXT FIELD fecha_cifras_al
            END IF
            NEXT FIELD fecha_cifras_al
         END IF

      AFTER FIELD fecha_cifras_al
         IF g_reg.fecha_cifras_al IS NULL OR
            g_reg.fecha_cifras_al MATCHES "[ *]" THEN
            ERROR " FECHA CIFRAS AL NO PUEDE SER NULO O TENER ESPACIO"
            NEXT FIELD fecha_cifras_al
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

         IF g_reg.rendimiento IS NULL OR
            g_reg.rendimiento = 0 THEN
            ERROR " EL rendimiento DE AFORE NO PUEDE SER NULO."
            NEXT FIELD  rendimiento
         END IF

         IF g_reg.comision IS NULL OR
            g_reg.comision = 0 THEN
            ERROR " EL rendimiento DE AFORE NO PUEDE SER NULO."
            NEXT FIELD  comision
         END IF

         LET vrend_neto = g_reg.rendimiento - g_reg.comision

         IF vrend_neto <> g_reg.rendimiento_neto THEN
            ERROR "EL IMPORTE DEL RENDIMIENTO NETO NO ES CORRECTO "
            NEXT FIELD rendimiento
         END IF

         IF g_reg.fecha_ini IS NULL THEN
            ERROR " LA FECHA INICIO NO PUEDE SER NULO"
            NEXT FIELD fecha_ini
         ELSE
            SELECT "OK"
            FROM   tab_rendimiento_neto
            WHERE afore_cod        = g_reg.afore_cod
            AND   fecha_ini        = g_reg.fecha_ini
            AND   siefore_cod      = g_reg.siefore_cod
            GROUP BY 1

            IF SQLCA.SQLCODE = 0  THEN
               ERROR  " REGISTRO DADO DE ALTA, VERIFIQUE... "
               NEXT FIELD fecha_ini
            END IF
         END IF

{         IF g_reg.fecha_ini > HOY THEN
            ERROR "FECHA CAPTURA NO PUEDE SER MAYOR AL DIA DE HOY"
            NEXT FIELD fecha_ini
         END IF
}
         LET g_reg.factualiza = TODAY
         LET g_reg.usuario    = usuario

         INSERT INTO tab_rendimiento_neto VALUES ( g_reg.* )
         ERROR "REGISTRO INGRESADO"
         SLEEP 1
         CALL Inicializa()
         NEXT FIELD afore_cod	

      ON KEY (INTERRUPT)
         CALL Inicializa()
      EXIT INPUT

   END INPUT

END FUNCTION

FUNCTION Consulta()
	
   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM1332" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta             (Ctrl-p) Imprimir              (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                             RENDIMIENTO   NETO                                " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 
      LET int_flag = FALSE

      CONSTRUCT cla_where ON afore_cod,
                             siefore_cod,
                             fecha_ini
                        FROM afore_cod,
                             siefore_cod,
                             fecha_ini

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

      LET sel_where = "SELECT afore_cod,siefore_cod,rendimiento,comision,rendimiento_neto,",
                      " fecha_ini,fecha_fin,orden_consar,fecha_cifras_al ",
                      "FROM tab_rendimiento_neto WHERE ",
                      cla_where CLIPPED,
                      "ORDER BY 9 DESC,2,8 ASC"
      PREPARE query FROM sel_where
      DECLARE cursor_1 CURSOR FOR query
      LET pos = 1

      FOREACH cursor_1 INTO l_record[pos].codigo,
                            l_record[pos].siefore,
                            l_record[pos].rendimiento,
                            l_record[pos].comision,
                            l_record[pos].rendimiento_neto,
                            l_record[pos].dato_fecha_ini,
                            l_record[pos].fecha_fin,
                            l_record[pos].orden_consar,
                            l_record[pos].fecha_cifras_al

         LET l_record[pos].codigo            = l_record[pos].codigo
         LET l_record[pos].siefore           = l_record[pos].siefore
         LET l_record[pos].rendimiento       = l_record[pos].rendimiento
         LET l_record[pos].comision          = l_record[pos].comision
         LET l_record[pos].rendimiento_neto  = l_record[pos].rendimiento_neto
         LET l_record[pos].dato_fecha_ini        = l_record[pos].dato_fecha_ini
         LET l_record[pos].fecha_fin           = l_record[pos].fecha_fin
         LET l_record[pos].orden_consar      = l_record[pos].orden_consar
         LET l_record[pos].fecha_cifras_al   = l_record[pos].fecha_cifras_al

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
               LET g_reg.rendimiento      = l_record[pos].rendimiento
               LET g_reg.comision         = l_record[pos].comision
               LET g_reg.rendimiento_neto = l_record[pos].rendimiento_neto
               LET g_reg.fecha_ini        = l_record[pos].dato_fecha_ini
               LET g_reg.fecha_fin         = l_record[pos].fecha_fin
               LET g_reg.orden_consar     = l_record[pos].orden_consar
               LET g_reg.fecha_cifras_al  = l_record[pos].fecha_cifras_al


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

   SELECT siefore_desc
   INTO   vsiefore_desc
   FROM   safre_af:tab_siefore
   WHERE  siefore_cod = g_reg.siefore_cod
   AND    afore_cod   = g_reg.afore_cod

   DISPLAY BY NAME g_reg.afore_cod,
                   g_reg.siefore_cod,
                   g_reg.rendimiento,
                   g_reg.comision,
                   g_reg.rendimiento_neto,
                   g_reg.fecha_ini,
                   g_reg.fecha_fin,
                   g_reg.orden_consar,
                   g_reg.fecha_cifras_al


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

FUNCTION  Modifica()

   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM1332" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta                                            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "               Escoja con <ENTER> el  Rendimiento Neto a modificar             " AT 2,1
      DISPLAY "                            RENDIMIENTO   NETO                                 " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 
      LET int_flag = FALSE

      CONSTRUCT cla_where ON afore_cod,
                             siefore_cod,
                             fecha_ini
                        FROM afore_cod,
                             siefore_cod,
                             fecha_ini

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
      LET sel_where = "SELECT afore_cod,siefore_cod,rendimiento,comision,rendimiento_neto,",
                      " fecha_ini,fecha_fin,orden_consar,fecha_cifras_al ",
                      "FROM tab_rendimiento_neto WHERE ",
                      cla_where CLIPPED,
                     "ORDER BY 9 DESC,2,8 ASC"
      PREPARE query1 FROM sel_where
      DECLARE cursor_2 CURSOR FOR query1
      LET pos = 1

      FOREACH cursor_2 INTO l_record[pos].codigo,
                            l_record[pos].siefore,
                            l_record[pos].rendimiento,
                            l_record[pos].comision,
                            l_record[pos].rendimiento_neto,
                            l_record[pos].dato_fecha_ini,
                            l_record[pos].fecha_fin,
                            l_record[pos].orden_consar,
                            l_record[pos].fecha_cifras_al

         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL  SET_COUNT(pos-1)
         ERROR ""	

         DISPLAY ARRAY l_record TO scr_1.*
            ON KEY (control-M)
            LET pos = ARR_CURR()
               LET g_reg.afore_cod        = l_record[pos].codigo
               LET g_reg.siefore_cod      = l_record[pos].siefore
               LET g_reg.rendimiento      = l_record[pos].rendimiento
               LET g_reg.comision         = l_record[pos].comision
               LET g_reg.rendimiento_neto = l_record[pos].rendimiento_neto
               LET g_reg.fecha_ini        = l_record[pos].dato_fecha_ini
               LET g_reg.fecha_fin        = l_record[pos].fecha_fin
               LET g_reg.orden_consar     = l_record[pos].orden_consar
               LET g_reg.fecha_cifras_al  = l_record[pos].fecha_cifras_al

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

   DISPLAY BY NAME g_reg.afore_cod,
                   g_reg.siefore_cod,
                   g_reg.rendimiento,
                   g_reg.comision,
                   g_reg.rendimiento_neto,
                   g_reg.fecha_ini,
                   g_reg.fecha_fin,
                   g_reg.orden_consar,
                   g_reg.fecha_cifras_al

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

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
      DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE,BOLD)

      INPUT BY NAME g_reg.rendimiento,
                    g_reg.comision,
                    --g_reg.rendimiento_neto,
                    --g_reg.fecha_ini,
                    --g_reg.fecha_fin  
                    g_reg.orden_consar,
                    g_reg.fecha_cifras_al
                    WITHOUT DEFAULTS

      AFTER FIELD rendimiento
         IF g_reg.rendimiento IS NULL OR
            g_reg.rendimiento = 0 THEN
            ERROR " EL RENDIMIENTO NO PUEDE SER NULO."
            NEXT FIELD  rendimiento
         END IF

         {IF g_reg.rendimiento < 0 THEN
            ERROR " EL RENDIMIENTO NO PUEDE SER MENOR A 0%"
            NEXT FIELD rendimiento
         END IF}

      AFTER FIELD comision
         IF g_reg.comision IS NULL OR
            g_reg.comision = 0 THEN
            ERROR " LA COMISION NO PUEDE SER NULO."
            NEXT FIELD  comision
         END IF

         IF g_reg.comision < 0 THEN
            ERROR " LA COMISION NO PUEDE SER MENOR A 0%"
            NEXT FIELD comision
         END IF

{      AFTER FIELD rendimiento_neto
         IF g_reg.rendimiento_neto IS NULL OR
            g_reg.rendimiento_neto = 0 THEN
            ERROR " EL RENDIMIENTO NO PUEDE SER NULO."
            NEXT FIELD  rendimiento_neto
         END IF

         LET vrend_neto = g_reg.rendimiento - g_reg.comision

         IF vrend_neto <> g_reg.rendimiento_neto THEN
            ERROR "EL IMPORTE DEL RENDIMIENTO NETO NO ES CORRECTO "
            NEXT FIELD rendimiento
         END IF
}
         LET vrend_neto = g_reg.rendimiento - g_reg.comision
         LET g_reg.rendimiento_neto = vrend_neto
         DISPLAY vrend_neto TO rendimiento_neto

         NEXT FIELD orden_consar

      AFTER FIELD orden_consar
         IF g_reg.orden_consar IS NULL OR
            g_reg.orden_consar = 0 THEN
            ERROR " EL ORDEN CONSAR NO PUEDE SER NULO O CERO"
            NEXT FIELD orden_consar
         ELSE
{            SELECT 'X'
            FROM tab_rendimiento_neto
            WHERE siefore_cod = g_reg.siefore_cod
            AND   fecha_ini   = g_reg.fecha_ini
            AND   fecha_fin   = g_reg.fecha_fin
            AND   orden_consar= g_reg.orden_consar

            IF SQLCA.SQLCODE = 0 THEN
               ERROR "ORDEN CONSAR NO PUEDE SER REPETIDO PARA ESTA SIEFORE "
               NEXT FIELD orden_consar
            ELSE
               NEXT FIELD fecha_cifras_al
            END IF}
            NEXT FIELD fecha_cifras_al
         END IF

      AFTER FIELD fecha_cifras_al
         IF g_reg.fecha_cifras_al IS NULL OR
            g_reg.fecha_cifras_al MATCHES "[ *]" THEN
            ERROR " FECHA CIFRAS AL NO PUEDE SER NULO O TENER ESPACIO"
            NEXT FIELD fecha_cifras_al
         END IF

         CALL Pregunta()

         LET aux_pausa = aux_pausa CLIPPED
         IF aux_pausa MATCHES "[Ss]" THEN

            UPDATE tab_rendimiento_neto
            SET    rendimiento      = g_reg.rendimiento,
                   comision         = g_reg.comision,
                   rendimiento_neto = g_reg.rendimiento_neto,
                   orden_consar     = g_reg.orden_consar,
                   fecha_cifras_al  = g_reg.fecha_cifras_al,
                   usuario          = usuario,
                   factualiza       = TODAY
            WHERE  afore_cod        = g_reg.afore_cod	
            AND    fecha_ini            = g_reg.fecha_ini
            AND    siefore_cod      = g_reg.siefore_cod
            AND    fecha_fin          = g_reg.fecha_fin

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

FUNCTION Elimina()

   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM1332" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta                                            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "               Escoja con <ENTER> el  Rendimiento Neto a eliminar              " AT 2,1
      DISPLAY "                             RENDIMIENTO   NETO                                " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 
      LET int_flag = FALSE

      CONSTRUCT cla_where ON afore_cod,
                             siefore_cod,
                             fecha_ini
                        FROM afore_cod,
                             siefore_cod,
                             fecha_ini

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

      LET sel_where = "SELECT afore_cod,siefore_cod,rendimiento,comision,rendimiento_neto,",
                      " fecha_ini,fecha_fin,orden_consar,fecha_cifras_al ",
                      "FROM tab_rendimiento_neto WHERE ",
                      cla_where CLIPPED,
                      "ORDER BY 9 DESC,2,8 ASC"
      PREPARE query2 FROM sel_where
      DECLARE cursor_3 CURSOR FOR query2
      LET pos = 1

      FOREACH cursor_3 INTO l_record[pos].codigo,
                            l_record[pos].siefore,
                            l_record[pos].rendimiento,
                            l_record[pos].comision,
                            l_record[pos].rendimiento_neto,
                            l_record[pos].dato_fecha_ini,
                            l_record[pos].fecha_fin,
                            l_record[pos].orden_consar,
                            l_record[pos].fecha_cifras_al

         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL  SET_COUNT(pos-1)
         ERROR ""	

         DISPLAY ARRAY l_record TO scr_1.*

            ON KEY (control-m)
               LET pos = ARR_CURR()
               LET pos = ARR_CURR()
               LET g_reg.afore_cod        = l_record[pos].codigo
               LET g_reg.siefore_cod      = l_record[pos].siefore
               LET g_reg.rendimiento      = l_record[pos].rendimiento
               LET g_reg.comision         = l_record[pos].comision
               LET g_reg.rendimiento_neto = l_record[pos].rendimiento_neto
               LET g_reg.fecha_ini        = l_record[pos].dato_fecha_ini
               LET g_reg.fecha_fin        = l_record[pos].fecha_fin
               LET g_reg.orden_consar     = l_record[pos].orden_consar
               LET g_reg.fecha_cifras_al  = l_record[pos].fecha_cifras_al

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


      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
      DISPLAY " ELIMINA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)

   DISPLAY BY NAME g_reg.afore_cod,
                   g_reg.siefore_cod,
                   g_reg.rendimiento,
                   g_reg.comision,
                   g_reg.rendimiento_neto,
                   g_reg.fecha_ini,
                   g_reg.fecha_fin,
                   g_reg.orden_consar,
                   g_reg.fecha_cifras_al

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
         DELETE FROM tab_rendimiento_neto
         WHERE afore_cod      = g_reg.afore_cod
         AND   siefore_cod    = g_reg.siefore_cod
         AND   fecha_ini          = g_reg.fecha_ini
         AND   fecha_fin        = g_reg.fecha_fin

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

FUNCTION Pregunta()
   
   PROMPT "Esta seguro de realizar esta operación S/N ? " FOR CHAR aux_pausa

END FUNCTION

FUNCTION impresion(pos)

   DEFINE i, pos SMALLINT

   LET g_impre = g_param_dis.ruta_listados CLIPPED,
       "/",usuario CLIPPED,".IMPTRECHA",
       hoy USING "dd-mm-yyyy" CLIPPED

   START REPORT rpt_tabrendneto TO g_impre

   FOR i = 1 TO (pos+1)
      LET g_reg.afore_cod        = l_record[i].codigo
      LET g_reg.siefore_cod      = l_record[i].siefore
      LET g_reg.rendimiento      = l_record[i].rendimiento
      LET g_reg.comision         = l_record[i].comision
      LET g_reg.rendimiento_neto = l_record[i].rendimiento_neto
      LET g_reg.fecha_ini        = l_record[i].dato_fecha_ini
      LET g_reg.fecha_fin        = l_record[i].fecha_fin
      LET g_reg.orden_consar     = l_record[pos].orden_consar
      LET g_reg.fecha_cifras_al  = l_record[pos].fecha_cifras_al

      IF g_reg.afore_cod IS NULL THEN
         EXIT FOR
      END IF
      OUTPUT TO REPORT rpt_tabrendneto(g_reg.*)
   END FOR

   FINISH REPORT rpt_tabrendneto
   ERROR "LISTADO GENERADO..."
   SLEEP 1
   ERROR ""
   LET g_lista = "lp ",g_impre
   RUN g_lista

END FUNCTION

REPORT rpt_tabrendneto(g_reg)

   DEFINE g_reg       RECORD 
          afore_cod        SMALLINT,
          siefore_cod      SMALLINT,
          rendimiento      DECIMAL(6,2),
          comision         DECIMAL(6,2),
          rendimiento_neto DECIMAL(6,2),
          fecha_ini        DATE,
          fecha_fin        DATE,
          orden_consar     SMALLINT,
          fecha_cifras_al  DATE,
          usuario          CHAR(8),
          factualiza       DATE
   END RECORD

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60

   FORMAT

      PAGE HEADER
         PRINT COLUMN 03," TABM133 ",
               COLUMN 21," CATALOGO RENDIMIENTO NETO ",
               COLUMN 67,TODAY USING "dd-mm-yyyy"
         SKIP 2 LINE

         PRINT COLUMN 05,"AFORE",
               COLUMN 09,"SIEFORE",
               COLUMN 10,"RENDIMIENTO",
               COLUMN 16,"COMISION",
               COLUMN 22,"RENDIMIENTO NETO",
               COLUMN 30,"FECHA INI",
               COLUMN 30,"FECHA FIN"
               
         SKIP 1 LINE
      ON EVERY ROW
         PRINT COLUMN 05,g_reg.afore_cod ,
               COLUMN 09,g_reg.siefore_cod,
               COLUMN 10,g_reg.rendimiento,
               COLUMN 16,g_reg.comision,
               COLUMN 22,g_reg.rendimiento_neto,
               COLUMN 15,g_reg.fecha_ini USING "dd-mm-yyyy",
               COLUMN 30,g_reg.fecha_fin
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
      ON LAST ROW
         SKIP 4 LINE 
         PRINT COLUMN 03, "Total de registros: ",COUNT(*) USING "<<<<"
END REPORT

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

    DEFINE g_plano_rend_neto INTEGER
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
         campo3  DECIMAL(6,2),  ---rendimiento
         campo4  DECIMAL(6,2),  ---comision
         campo5  DECIMAL(6,2),  ---rendimiento_neto
         campo6  DATE,          ---fecha_ini indice
         campo7  DATE,          ---fecha_fin
         campo8  SMALLINT,      ---orden_consar
         campo9  DATE)          ---fecha_cifras_al

        DATABASE safre_af

    OPEN WINDOW ventana_5 AT 4,4 WITH FORM "TABM1335" ATTRIBUTE(BORDER)
    DISPLAY " TABM133              CARGA INDICES DE RENDIMIENTO NETO                            " AT 3,1 ATTRIBUTE(REVERSE)
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
            INSERT INTO safre_tmp:plano_rend_neto
        WHENEVER ERROR STOP

        SELECT COUNT(*) 
        INTO   g_plano_rend_neto
        FROM   safre_tmp:plano_rend_neto

        IF g_plano_rend_neto IS NULL OR
           g_plano_rend_neto = 0 THEN
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
        afore            SMALLINT,
        siefore          SMALLINT,
        rendimiento      DECIMAL(6,2),
        comision         DECIMAL(6,2),
        rendimiento_neto DECIMAL(6,2),
        fecha_ini_indice CHAR(10),
        fecha_fin        CHAR(10),
        orden_consar     SMALLINT,
        fecha_cifras_al  DATE
    END RECORD

    DEFINE   
         vafore            SMALLINT,      ---afore
         vsiefore          SMALLINT,      ---siefore
         vrendimiento      DECIMAL(6,2),  ---rendimiento
         vcomision         DECIMAL(6,2),
         vrend_neto        DECIMAL(6,2),
         vfecha_ini_indice CHAR(10),      ---fecha_ini indice
         vfecha_fin        CHAR(10)       ---fecha_fin

    DEFINE vorden_consar    SMALLINT
    DEFINE vfecha_cifras_al DATE

    DEFINE vfecha_ini   CHAR(10)
    DEFINE ban_afo  SMALLINT
    DEFINE ban_rend SMALLINT
    DEFINE ban_com  SMALLINT
    DEFINE ban_neto SMALLINT
    DEFINE ban_fec1 SMALLINT
    DEFINE ban_fec2 SMALLINT
    DEFINE contador INTEGER

    DEFINE GIMPRE_RECH CHAR(200)
    DEFINE desc_err    CHAR(200)
    DEFINE vrend_neto2 DECIMAL(6,2)

    LET cont_reg = 0
    LET cont_reg2= 0
    LET cont_reg3= 0
    LET ban_afo  = 0
    LET ban_rend = 0
    LET ban_com  = 0
    LET ban_neto = 0
    LET ban_fec1 = 0
    LET ban_fec2 = 0
    LET contador = 0
    INITIALIZE GIMPRE_RECH TO NULL
    INITIALIZE carga_reg.*   TO NULL

    LET GIMPRE_RECH    = g_param_dis.ruta_listados CLIPPED,"/", 
                         usuario CLIPPED,
                         ".RENDI_NETO_ERR.",
                         TODAY USING "DDMMYY"

    START REPORT rep_rechazos TO GIMPRE_RECH 

    INSERT INTO tab_ctr_arh_proc 
    VALUES (archivo, cont_reg,cont_reg2,cont_reg3, TODAY)

    SELECT COUNT(*)
    INTO   total_reg
    FROM   safre_tmp:plano_rend_neto

    DECLARE cursor_ind CURSOR FOR 
    SELECT  *
    FROM    safre_tmp:plano_rend_neto

    FOREACH cursor_ind INTO carga_reg.*
        LET ban_afo  = 0
        LET ban_rend = 0
        LET ban_com  = 0
        LET ban_neto = 0
        LET ban_fec1 = 0
        LET ban_fec2 = 0
        LET cont_reg = cont_reg + 1
        LET vafore   = 0
        LET vsiefore = 0
        LET vrendimiento = 0
        LET vcomision    = 0
        LET vrend_neto   = 0
        INITIALIZE vfecha_ini_indice TO NULL
        INITIALIZE vfecha_fin        TO NULL
        LET vorden_consar = 0
        INITIALIZE vfecha_cifras_al  TO NULL

        IF cont_reg <= total_reg  THEN
            LET vafore            = carga_reg.afore
            LET vsiefore          = carga_reg.siefore
            LET vrendimiento      = carga_reg.rendimiento
            LET vcomision         = carga_reg.comision
            LET vrend_neto        = carga_reg.rendimiento_neto
            LET vfecha_ini_indice = carga_reg.fecha_ini_indice
            LET vfecha_fin        = carga_reg.fecha_fin
            LET vorden_consar     = carga_reg.orden_consar
            LET vfecha_cifras_al  = carga_reg.fecha_cifras_al

           IF vfecha_ini_indice IS NOT NULL THEN 
               IF (vfecha_ini_indice[1,2] > 12 OR
                  vfecha_ini_indice[4,5] > 31) OR
                  (vfecha_ini_indice[1,2] = 2 AND
                   vfecha_ini_indice[4,5] > 29) THEN
                   LET ban_fec1 = 1
               ELSE
                   LET ban_fec1 = 0
                   LET vfecha_ini =  vfecha_ini_indice[1,2],"/",
                                 vfecha_ini_indice[4,5],"/",
                                 vfecha_ini_indice[7,10]
                   {SELECT COUNT(*) 
                   INTO contador
                   FROM tab_rendimiento_neto
                   WHERE fecha_ini = vfecha_ini
                   IF contador > 0 THEN
                      LET ban_fec = 1
                   ELSE
                      LET ban_fec = 0
                   END IF}
               END IF
           ELSE
               LET vfecha_ini = NULL
               LET ban_fec1 = 1
           END IF 
        END IF

        SELECT 'X' FROM tab_afore
        WHERE afore_cod = vafore

        IF SQLCA.SQLCODE = NOTFOUND THEN
           LET ban_afo = 1
        ELSE
           LET ban_afo = 0
        END IF

{        IF vrendimiento  0 THEN
           LET ban_rend = 1
        ELSE
           LET ban_rend = 0
        END IF

        IF vcomision < 0 THEN
           LET ban_com = 1
        ELSE
           LET ban_com = 0
        END IF
}

        LET ban_rend = 0
        LET ban_com  = 0

        LET vrend_neto2 = vrendimiento - vcomision
        IF carga_reg.rendimiento_neto <> vrend_neto2 THEN
           LET ban_neto = 1
        ELSE
           LET ban_neto = 0
        END IF

        IF vfecha_ini[4,5] <> 15 THEN
           LET ban_fec1 = 1
        ELSE
           LET ban_fec1 = 0
        END IF

        LET ban_fec2 = 0

        IF  ban_afo  = 0 AND
            ban_rend = 0 AND
            ban_com  = 0 AND
            ban_neto = 0 AND 
            ban_fec1 = 0 AND
            ban_fec2 = 0 THEN

            LET cont_reg2 = cont_reg2 + 1

            INSERT INTO tab_rendimiento_neto
            VALUES (vafore,
                    vsiefore,
                    vrendimiento,
                    vcomision,
                    vrend_neto,
                    vfecha_ini_indice,
                    vfecha_fin,
                    vorden_consar,
                    vfecha_cifras_al,
                    usuario,
                    TODAY)
        ELSE
           LET cont_reg3 = cont_reg3 + 1      #rechazados

          INITIALIZE desc_err TO NULL

          IF ban_afo  = 1 THEN
             LET desc_err = desc_err CLIPPED,"Afore,"
          END IF
          IF ban_neto = 1 THEN
             LET desc_err = desc_err CLIPPED, "Neto,"
          END IF
          IF ban_fec1 = 1 THEN
             LET desc_err = desc_err CLIPPED, "Fecha ini"
          END IF

          OUTPUT TO REPORT rep_rechazos 
                   (vafore,
                    vsiefore,
                    vrendimiento,
                    vcomision,
                    vrend_neto,
                    vfecha_ini_indice,
                    vfecha_fin,
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

        DISPLAY "Archivo de rechazos en:",g_param_dis.ruta_listados CLIPPED,"/", 
                         usuario CLIPPED,
                         ".RENDI_NETO_ERR."
        

        PROMPT "<Enter> para continuar" FOR enter
        EXIT PROGRAM

END FUNCTION

REPORT rep_rechazos(rafore_cod,rsiefore,rrendimiento,rcomision,rrend_neto,rfecha_ini,
                    rfecha_fin,rdesc_err)

   DEFINE
          rafore_cod        SMALLINT,
          rsiefore          SMALLINT,
          rrendimiento      DECIMAL(6,2),
          rcomision         DECIMAL(6,2),
          rrend_neto        DECIMAL(6,2),
          rfecha_ini        DATE,
          rfecha_fin        DATE,
          rdesc_err         CHAR(100)

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
      PAGE HEADER
         PRINT COLUMN 03," TABM133 ",
               COLUMN 21," RECHAZOS DE INDICES DE RENDIMIENTO NETO",
               COLUMN 67,TODAY USING "dd-mm-yyyy"
         SKIP 2 LINE

         PRINT COLUMN 01,"AFORE",
               COLUMN 09,"SIEFORE",
               COLUMN 18,"RENDIMIENTO",
               COLUMN 30,"COMISION",
               COLUMN 42,"REND. NETO",
               COLUMN 54,"FECHA INI",
               COLUMN 65,"FECHA FIN",
               COLUMN 80,"OBSERVACION"

         SKIP 1 LINE
      ON EVERY ROW
         PRINT COLUMN 01,rafore_cod ,
               COLUMN 09,rsiefore,
               COLUMN 18,rrendimiento,
               COLUMN 30,rcomision,
               COLUMN 42,rrend_neto,
               COLUMN 54,rfecha_ini USING "dd-mm-yyyy",
               COLUMN 65,rfecha_fin USING "dd-mm-yyyy",
               COLUMN 80,rdesc_err CLIPPED
      PAGE TRAILER
         SKIP 2 LINE
         PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
      ON LAST ROW
         SKIP 4 LINE 
         PRINT COLUMN 03, "Total de registros: ",COUNT(*) USING "<<<<"
END REPORT

