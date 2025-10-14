######################################################################
#Proyecto          => Sistema de AFORE.( MEXICO )                    #
#Propietario       => E.F.P.                                         #
#Programa          =>                                                #
#Descripcion       => CATALOGO DE NOMBRES DE ARCHIVOS.               #
#Por               => OMAR SANDOVAL BADILLO                          #
#Fecha             => 13 Semptiembre 2005                            #
#Sistema           => CTA                                            #
######################################################################
DATABASE safre_af

GLOBALS
   DEFINE g_param  RECORD LIKE seg_modulo.*

   DEFINE hoy           DATE,
          cla_where     CHAR(300),
          sel_where     CHAR(300),
          opc           CHAR(01),
          USER          CHAR(08)

   DEFINE l_reg   RECORD
          modulo_cod     CHAR(03),
          nom_programa   CHAR(10),
          dsc_programa   CHAR(30),
          tipo_informe   SMALLINT,
          dsc_informe    CHAR(30),
          ruta_envio     CHAR(30)
   END RECORD

   DEFINE l_reg1 ARRAY [10] OF RECORD
          fijo_variable SMALLINT,
          parametro     CHAR(15),
          formato       CHAR(10)
   END RECORD
 
   DEFINE l_reg2 ARRAY [1] OF RECORD
          resultado     CHAR(100)
   END RECORD

   DEFINE pos,
          arr_c, 
          arr_l,
          arr_t    SMALLINT

END GLOBALS
#####################################################################
MAIN
   OPTIONS PROMPT LINE LAST,
      INPUT WRAP,
      ACCEPT KEY CONTROL-O

   DEFER INTERRUPT

   SELECT usuario
   INTO   USER
   FROM   seg_modulo
   WHERE modulo_cod = "cta"

   LET hoy = TODAY
   INITIALIZE l_reg.* TO NULL

   CALL proceso()
END MAIN
#####################################################################
FUNCTION proceso()

   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "CTAB0101" ATTRIBUTE(BORDER)
   DISPLAY " CTAB010              CATALOGO DE NOMBRES DE ARCHIVOS                          " AT 3,1 ATTRIBUTE(REVERSE) 
   DISPLAY "                   PARAMETROS QUE CONFORMAN EL ARCHIVO                         " AT 11,1 ATTRIBUTE(REVERSE) 
   DISPLAY HOY USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

   MENU "NOMBRES DE ARCHIVOS"
      COMMAND "Agrega" "Agrega datos para generacion de archivo"
         CALL agrega()
      COMMAND "Consulta" "Consulta datos para generacion de archivo"
         CALL consulta()
      COMMAND "Modifica" "Modifica datos para generacion de archivo"
         CALL modifica()
      COMMAND "Elimina" "Elimina datos para generacion de archivo"
         CALL elimina()
      COMMAND "Salir" "Salir del Programa"
         EXIT MENU
   END MENU

   CLOSE WINDOW ventana_1
END FUNCTION
#####################################################################
FUNCTION Inicializa()

   DEFINE i  SMALLINT

   INITIALIZE l_reg.* TO NULL
--   DISPLAY BY NAME l_reg.*

   FOR i = 1 TO 10
      INITIALIZE l_reg1[i].* TO NULL
      #DISPLAY ARRAY l_reg1 TO sc_r.* 
--      DISPLAY BY NAME l_reg1[i].* 
   END FOR

END FUNCTION
#####################################################################
FUNCTION agrega()

   DEFINE x_longitud     SMALLINT,
          x_valor        CHAR(10),
          x_tipo         SMALLINT,
          x_formato      CHAR(10)
 
   DEFINE i  SMALLINT,
          j  SMALLINT,
          x  SMALLINT,
          xx  SMALLINT,
          inicio  SMALLINT

   DEFINE vmodulo        CHAR(03),
          vprograma      CHAR(10),
          vprograma_desc CHAR(40),
          vinforme       SMALLINT,
          vinforme_desc  CHAR(40),
          vruta          CHAR(30)

   DEFINE valor_arch ARRAY[20] OF RECORD
         numero_parametro             SMALLINT,
         valor                        CHAR(10),
         formato                      CHAR(10)
   END RECORD

   INPUT BY NAME l_reg.* #WITHOUT DEFAULTS

      AFTER FIELD modulo_cod
         IF l_reg.modulo_cod IS NULL THEN
            LET vmodulo = pantalla_modulo()   
            LET l_reg.modulo_cod = vmodulo
         ELSE
            SELECT "X"
            FROM   seg_modulo
            WHERE  modulo_cod = LOWER(l_reg.modulo_cod)

            IF SQLCA.SQLCODE = 100 THEN
               ERROR "NO EXISTE EL MODULO INDICADO"
               NEXT FIELD modulo_cod
            END IF
         END IF

         LET l_reg.modulo_cod = "CTA"
         DISPLAY BY NAME l_reg.modulo_cod

      AFTER FIELD nom_programa
         IF l_reg.nom_programa IS NULL THEN
            CALL pantalla_programa(l_reg.modulo_cod)
               RETURNING vprograma,
                         vprograma_desc

            LET l_reg.nom_programa = vprograma
            LET l_reg.dsc_programa = vprograma_desc
         ELSE
            SELECT "X"
            FROM   seg_programa
            --WHERE  modulo_cod   = LOWER(l_reg.modulo_cod)
            WHERE  modulo_cod   = l_reg.modulo_cod
            AND    programa_cod = l_reg.nom_programa 

            IF SQLCA.SQLCODE = 100 THEN
               ERROR "NO EXISTE EL PROGRAMA INDICADO"
               NEXT FIELD nom_programa
            ELSE
               SELECT programa_cod,
                      programa_desc
               INTO   l_reg.nom_programa,
                      l_reg.dsc_programa
               FROM   seg_programa
               --WHERE  modulo_cod   = LOWER(l_reg.modulo_cod)
               WHERE  modulo_cod   = l_reg.modulo_cod
               AND    programa_cod = l_reg.nom_programa
            END IF
         END IF

         LET l_reg.nom_programa = "CTAB009"
         DISPLAY BY NAME l_reg.nom_programa,
                         l_reg.dsc_programa 
         NEXT FIELD tipo_informe

      AFTER FIELD tipo_informe
         IF l_reg.tipo_informe IS NULL THEN
            CALL pantalla_informe()
               RETURNING vinforme,
                         vinforme_desc

            LET l_reg.tipo_informe = vinforme
            LET l_reg.dsc_informe  = vinforme_desc
           
            IF l_reg.tipo_informe IS NULL AND
               l_reg.dsc_informe  IS NULL THEN
                  ERROR "EL CAMPO NO PUEDE SER NULO "
                  NEXT FIELD tipo_informe
            END IF
         ELSE
            SELECT "X"
            FROM   tab_tipo_informe
            WHERE  tipo_informe   = l_reg.tipo_informe
            GROUP BY 1

            IF SQLCA.SQLCODE = 100 THEN
               ERROR "NO EXISTE EL TIPO DE INFORME INDICADO"
               NEXT FIELD tipo_informe
            ELSE  
               SELECT descripcion
               INTO   l_reg.dsc_informe
               FROM   tab_tipo_informe
               WHERE  tipo_informe   = l_reg.tipo_informe
               ORDER BY 1
            END IF
         END IF

         SELECT "X"
         FROM   cta_formato_archivos
         WHERE  tipo_informe = l_reg.tipo_informe
         AND    programa_cod = l_reg.nom_programa
         AND    modulo_cod   = l_reg.modulo_cod
         GROUP BY 1

         IF SQLCA.SQLCODE = 0 THEN
            ERROR "YA EXISTEN PARAMETROS PARA ESTE TIPO DE INFORME ..."
         END IF
{
         ELSE
            DISPLAY BY NAME l_reg.tipo_informe,
                            l_reg.dsc_informe
            NEXT FIELD ruta_envio
         END IF
}
      DISPLAY BY NAME l_reg.tipo_informe,
                      l_reg.dsc_informe
      NEXT FIELD ruta_envio

      AFTER FIELD ruta_envio
         IF l_reg.ruta_envio IS NULL THEN
            LET vruta = pantalla_ruta()
            LET l_reg.ruta_envio = vruta
         ELSE
            SELECT "X"
            FROM   seg_modulo
            WHERE  ruta_envio = l_reg.ruta_envio

            IF SQLCA.SQLCODE = 100 THEN
               ERROR "NO EXISTE LA RUTA INDICADA"
               NEXT FIELD ruta_envio
            END IF
         END IF

         DISPLAY BY NAME l_reg.ruta_envio

                  DECLARE c_formato CURSOR FOR
                  SELECT numero_parametro,
                         valor,
                         formato
                  FROM   cta_formato_archivos
                  WHERE  tipo_informe = l_reg.tipo_informe
                  ORDER BY 1
 
                  LET j = 1
                  FOREACH c_formato INTO valor_arch[j].*
                     IF valor_arch[j].formato IS NULL OR
                        valor_arch[j].formato = " " THEN
                         LET l_reg2[1].resultado = l_reg2[1].resultado CLIPPED,
                                                   valor_arch[j].valor CLIPPED
                     ELSE
                         LET l_reg2[1].resultado = l_reg2[1].resultado CLIPPED,
                                                   valor_arch[j].formato CLIPPED
                     END IF
                  END FOREACH
         INPUT ARRAY l_reg1 WITHOUT  DEFAULTS FROM sc_r.*

            BEFORE ROW
               LET arr_c = ARR_CURR()
               LET arr_l = SCR_LINE()
               LET arr_t = ARR_COUNT()

            AFTER FIELD fijo_variable
               IF l_reg1[arr_c].fijo_variable IS NULL OR
                  l_reg1[arr_c].fijo_variable = " " THEN
                     ERROR "EL CAMPO NO PUEDE SER NULO"
               ELSE
                  CASE l_reg1[arr_c].fijo_variable
                     WHEN 1 CALL valores_fijos()
                               RETURNING x_longitud,
                                         x_valor
                            IF x_valor IS NULL OR x_valor = " " THEN
                               NEXT FIELD fijo_variable
                            ELSE
                               LET l_reg1[arr_c].parametro = x_valor
                               LET l_reg1[arr_c].formato   = NULL
                            END IF

                   LET l_reg2[1].resultado = l_reg2[1].resultado CLIPPED,
                                             l_reg1[arr_c].parametro CLIPPED
                   DISPLAY BY NAME l_reg2[1].resultado

                     WHEN 2 CALL valores_variables()
                               RETURNING x_tipo,
                                         x_formato
                            --IF x_formato IS NULL OR x_formato = " " THEN
                            --   NEXT FIELD fijo_variable
                            --ELSE
                               CASE x_tipo
                                  WHEN 1 
                                      LET l_reg1[arr_c].parametro = "HOY"
                                      LET l_reg1[arr_c].formato   = x_formato
                                  WHEN 2 
                                      LET l_reg1[arr_c].parametro = "x_folio"
                                      LET l_reg1[arr_c].formato   = x_formato
                                  WHEN 3 
                                      LET l_reg1[arr_c].parametro = "g_usuario"
                                      LET l_reg1[arr_c].formato   = x_formato
                               END CASE
                            --END IF

                   LET l_reg2[1].resultado = l_reg2[1].resultado CLIPPED,
                                             l_reg1[arr_c].formato CLIPPED
                   DISPLAY BY NAME l_reg2[1].resultado
                  END CASE
               END IF

               DISPLAY BY NAME l_reg1[arr_c].fijo_variable,
                               l_reg1[arr_c].parametro,
                               l_reg1[arr_c].formato

         ON KEY(ESC)
            SELECT "X"
            FROM   cta_formato_archivos
            WHERE  tipo_informe = l_reg.tipo_informe
            GROUP BY 1

            IF SQLCA.SQLCODE = 0 THEN
               SELECT max(numero_parametro) + 1
               INTO   inicio 
               FROM   cta_formato_archivos
               WHERE  tipo_informe = l_reg.tipo_informe

               FOR x = 1 TO arr_t
                  IF l_reg1[x].fijo_variable IS NULL OR
                     l_reg1[x].fijo_variable = 0 THEN
                  ELSE
                     INSERT INTO cta_formato_archivos 
                            VALUES(l_reg.modulo_cod ,
                                   l_reg.nom_programa ,
                                   l_reg.tipo_informe,
                                   l_reg.ruta_envio ,
                                   inicio,
                                   l_reg1[x].fijo_variable,
                                   l_reg1[x].parametro,
                                   l_reg1[x].formato,
                                   USER,
                                   hoy)
                     LET inicio = inicio + 1
                  END IF
               END FOR
            ELSE
               FOR x = 1 TO arr_t
                  IF l_reg1[x].fijo_variable IS NULL OR
                     l_reg1[x].fijo_variable = 0 THEN
                  ELSE
                     INSERT INTO cta_formato_archivos
                            VALUES(l_reg.modulo_cod ,
                                   l_reg.nom_programa ,
                                   l_reg.tipo_informe,
                                   l_reg.ruta_envio ,
                                   x,
                                   l_reg1[x].fijo_variable,
                                   l_reg1[x].parametro,
                                   l_reg1[x].formato,
                                   USER,
                                   hoy)
                  END IF
               END FOR
            END IF

            ERROR "REGISTRO INGRESADO" 
            CLEAR FORM
            INITIALIZE l_reg2[1].resultado TO NULL
            CALL Inicializa()
            EXIT INPUT

         ON KEY(INTERRUPT)   
            CALL Inicializa()
            INITIALIZE l_reg2[1].resultado TO NULL
            ERROR "PROCESO DE AGREGA CANCELADO ..."   
            CLEAR FORM
            EXIT INPUT
         END INPUT

      ON KEY(ESC)
         IF l_reg.modulo_cod IS NULL THEN
            LET vmodulo = pantalla_modulo()
            LET l_reg.modulo_cod = vmodulo
         ELSE
            SELECT "X"
            FROM   seg_modulo
            WHERE  modulo_cod = l_reg.modulo_cod

            IF SQLCA.SQLCODE = 100 THEN
               ERROR "NO EXISTE EL MODULO INDICADO"
               NEXT FIELD modulo_cod
            END IF
         END IF

         IF l_reg.nom_programa IS NULL THEN
            CALL pantalla_programa(l_reg.modulo_cod)
               RETURNING vprograma,
                         vprograma_desc

            LET l_reg.nom_programa = vprograma
            LET l_reg.dsc_programa = vprograma_desc
         ELSE
            SELECT "X"
            FROM   seg_programa
            WHERE  modulo_cod   = l_reg.modulo_cod
            AND    programa_cod = l_reg.nom_programa

            IF SQLCA.SQLCODE = 100 THEN
               ERROR "NO EXISTE EL PROGRAMA INDICADO"
               NEXT FIELD nom_programa
            END IF
         END IF

         IF l_reg.tipo_informe IS NULL THEN
            CALL pantalla_informe()
               RETURNING vinforme,
                         vinforme_desc

            LET l_reg.tipo_informe = vinforme
            LET l_reg.dsc_informe  = vinforme_desc

            IF l_reg.tipo_informe IS NULL AND
               l_reg.dsc_informe  IS NULL THEN
                  ERROR "EL CAMPO NO PUEDE SER NULO "
                  NEXT FIELD tipo_informe
            END IF
         ELSE
            SELECT "X"
            FROM   tab_tipo_informe
            WHERE  tipo_informe   = l_reg.tipo_informe
            GROUP BY 1

            IF SQLCA.SQLCODE = 100 THEN
               ERROR "NO EXISTE EL TIPO DE INFORME INDICADO"
               NEXT FIELD tipo_informe
            END IF
         END IF

         IF l_reg.ruta_envio IS NULL THEN
            LET vruta = pantalla_ruta()
            LET l_reg.ruta_envio = vruta
         ELSE
            SELECT "X"
            FROM   seg_modulo
            WHERE  ruta_envio = l_reg.ruta_envio

            IF SQLCA.SQLCODE = 100 THEN
               ERROR "NO EXISTE LA RUTA INDICADA"
               NEXT FIELD ruta_envio
            END IF
         END IF

      ON KEY(CONTROL-C)
         CALL Inicializa()
         EXIT INPUT
   END INPUT
   CLEAR FORM

END FUNCTION
#####################################################################
FUNCTION valores_fijos()

   DEFINE valor_fijo  RECORD
      campo_long         SMALLINT,
      campo_val          CHAR(10)
   END RECORD

   OPEN WINDOW ventana_6 AT 6,6 WITH FORM "CTAB0106" ATTRIBUTE(BORDER)
   DISPLAY " <Esc> Grabar       " AT 2,1
   DISPLAY " Longitud  Valor    " AT 4,1 ATTRIBUTE(REVERSE)

   INPUT BY NAME valor_fijo.*
      AFTER FIELD campo_long
         IF valor_fijo.campo_long IS NULL THEN
            ERROR " EL VALOR NO PUEDE SER NULO"
            NEXT FIELD campo_long
         ELSE
            IF valor_fijo.campo_long > 10 THEN
               ERROR "LA LONGITUD MAXIMA ES 10"
               INITIALIZE valor_fijo.campo_long TO NULL
               NEXT FIELD campo_long
            END IF
         END IF

         DISPLAY BY NAME valor_fijo.campo_long

      AFTER FIELD campo_val
        IF valor_fijo.campo_val IS NULL THEN 
         ERROR "DEBE INGRESAR UN VALOR DE ",valor_fijo.campo_long," POSICIONES" 
         NEXT FIELD campo_val
        ELSE
           IF LENGTH(valor_fijo.campo_val) > valor_fijo.campo_long OR
              LENGTH(valor_fijo.campo_val) < valor_fijo.campo_long THEN
              ERROR "EL VALOR NO CORRESPONDE A LA LONGITUD INDICADA"
              INITIALIZE valor_fijo.campo_val TO NULL
              NEXT FIELD campo_val
           END IF
        END IF

        DISPLAY BY NAME valor_fijo.campo_val 

       ON KEY(ESC)
         IF valor_fijo.campo_long IS NULL THEN
            ERROR " EL VALOR NO PUEDE SER NULO"
            NEXT FIELD campo_long
         ELSE
            IF valor_fijo.campo_long > 10 THEN
               ERROR "LA LONGITUD MAXIMA ES 10"
               NEXT FIELD campo_long
            END IF
         END IF

         DISPLAY BY NAME valor_fijo.campo_long

         IF valor_fijo.campo_val IS NULL THEN
          ERROR "DEBE INGRESAR UN VALOR DE ",valor_fijo.campo_long," POSICIONES"
          NEXT FIELD campo_val
         ELSE
           IF LENGTH(valor_fijo.campo_val) > valor_fijo.campo_long OR
              LENGTH(valor_fijo.campo_val) < valor_fijo.campo_long THEN
              ERROR "EL VALOR NO CORRESPONDE A LA LONGITUD INDICADA"
              LET valor_fijo.campo_val = NULL
              DISPLAY BY NAME valor_fijo.campo_val
              NEXT FIELD campo_val
           END IF
         END IF

         DISPLAY BY NAME valor_fijo.campo_val 
         EXIT INPUT
         CLOSE WINDOW ventana_6

      ON KEY(CONTROL-C)
         INITIALIZE valor_fijo.* TO NULL
         EXIT INPUT
         CLOSE WINDOW ventana_6

   END INPUT

   CLOSE WINDOW ventana_6

   RETURN valor_fijo.campo_long,
          valor_fijo.campo_val
       
END FUNCTION
#######################################################################
FUNCTION valores_variables()

   DEFINE valor_variable  RECORD
      valor         SMALLINT,
      formato       CHAR(10)
   END RECORD

   OPEN WINDOW ventana_7 AT 6,6 WITH FORM "CTAB0107" ATTRIBUTE(BORDER)
   DISPLAY " <Esc> Grabar               " AT 2,1
   DISPLAY "   Datos Variables          " AT 4,1 ATTRIBUTE(REVERSE)

   INPUT BY NAME valor_variable.*

      AFTER FIELD valor
         IF valor_variable.valor IS NULL THEN
            ERROR " EL VALOR NO PUEDE SER NULO"
            NEXT FIELD valor
         ELSE
            CASE valor_variable.valor
              WHEN 1 ERROR "FORMATOS: 'DDMMYYYY' 'DDMMYY' 'YYMMDD' ",
                                     "'YYYYMMDD' 'MMDDYY' 'MMDDYYYY'" 
                     NEXT FIELD formato

              WHEN 2 LET valor_variable.formato = "<<<<<<<<<"
                     DISPLAY BY NAME valor_variable.formato
                     NEXT FIELD valor

              WHEN 3 LET valor_variable.formato = "????????"
                     DISPLAY BY NAME valor_variable.formato
                     NEXT FIELD valor
            END CASE
         END IF

      AFTER FIELD formato
        IF valor_variable.formato IS NULL THEN
           ERROR "EL FORMATO NO PUEDE SER NULO"
           NEXT FIELD formato
        ELSE
           IF valor_variable.formato = "DDMMYYYY" OR 
              valor_variable.formato = "DDMMYY"   OR
              valor_variable.formato = "YYMMDD"   OR
              valor_variable.formato = "YYYYMMDD" OR 
              valor_variable.formato = "MMDDYY"   OR
              valor_variable.formato = "MMDDYYYY" THEN
                 ERROR "EL FORMATO CORRECTO"
                 NEXT FIELD valor
           ELSE
              ERROR "EL FORMATO NO ES CORRECTO"
              NEXT FIELD formato
           END IF
        END IF

        DISPLAY BY NAME valor_variable.formato

      ON KEY(ESC)
{
         IF (valor_variable.valor IS NULL AND 
             valor_variable.formato IS NULL)  OR
            (valor_variable.valor IS NOT NULL AND
             valor_variable.formato IS NULL) THEN
}
         IF valor_variable.valor IS NULL AND 
             valor_variable.formato IS NULL THEN
               ERROR "LOS DOS CAMPOS DEBEN CONTENER UN VALOR" 
               NEXT FIELD valor
         END IF

         EXIT INPUT
         CLOSE WINDOW ventana_7

      ON KEY(INTERRUPT)
         INITIALIZE valor_variable.* TO NULL
         EXIT INPUT
         CLOSE WINDOW ventana_7
   END INPUT

   CLOSE WINDOW ventana_7

   RETURN valor_variable.valor,
          valor_variable.formato


END FUNCTION
#######################################################################
FUNCTION consulta()

   DEFINE l_record ARRAY[100] OF RECORD
       numero_parametro  SMALLINT,
       programa_cod   CHAR(15),
       tipo_informe   SMALLINT,
       tipo_dato      SMALLINT,
       valor          CHAR(10),
       formato        CHAR(10),
       ruta_envio     CHAR(40)
   END RECORD

   OPEN WINDOW ventana_8 AT 2,2 WITH FORM "CTAB0108" ATTRIBUTE(BORDER) 
   DISPLAY " CONSULTA " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY "<Enter> Consultar " AT 1,60
   DISPLAY " CTAB010              CATALOGO DE NOMBRES DE ARCHIVOS                          " AT 3,1 ATTRIBUTE(REVERSE) 
   DISPLAY HOY USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

   LET int_flag = FALSE

   CONSTRUCT cla_where ON modulo_cod,
                          programa_cod,
                          tipo_informe
                     FROM modulo_cod,
                          programa_cod,
                          tipo_informe

      ON KEY (CONTROL-M)
         LET int_flag = FALSE
         EXIT CONSTRUCT
      ON KEY (CONTROL-C)
         LET int_flag = TRUE
         EXIT CONSTRUCT
   END CONSTRUCT

   IF int_flag = TRUE THEN
      LET int_flag = FALSE
      ERROR "BUSQUEDA CANCELADA..."
      SLEEP 2
      ERROR ""
      CLEAR SCREEN
      CLOSE WINDOW ventana_8
      RETURN
   END IF

   LET sel_where = " SELECT numero_parametro,",
                          "programa_cod,",
                          "tipo_informe,",
                          "tipo_dato,",
                          "valor,",
                          "formato,",
                          "ruta_envio ", 
                   " FROM   cta_formato_archivos ",
                   " WHERE ", cla_where CLIPPED,
                   " ORDER BY 3,1 "

   PREPARE query FROM sel_where
   DECLARE con_1 CURSOR FOR query

   LET pos = 1

   FOREACH con_1 INTO l_record[pos].*
      LET pos = pos + 1
   END FOREACH

   LET pos = pos - 1

   IF pos >= 1 THEN
      CALL SET_COUNT(pos)

      DISPLAY ARRAY l_record TO scr1.*
         ON KEY(INTERRUPT)
            CLEAR FORM
            EXIT DISPLAY
      END DISPLAY
   ELSE
      ERROR "REGISTRO NO ENCONTRADO"
      CLEAR SCREEN
      CLOSE WINDOW ventana_8
      RETURN
   END IF
   CLOSE WINDOW ventana_8

END FUNCTION
#####################################################################
FUNCTION modifica()

   DEFINE l_record ARRAY[100] OF RECORD
       numero_parametro SMALLINT,      
       programa_cod   CHAR(15),
       tipo_informe   SMALLINT,
       tipo_dato      SMALLINT,
       valor          CHAR(10),
       formato        CHAR(10),
       ruta_envio     CHAR(40)
   END RECORD

   DEFINE l_mod ARRAY[100] OF RECORD
      numero_parametro SMALLINT,      
      programa_cod   CHAR(15),
      tipo_informe   SMALLINT,
      tipo_dato      SMALLINT,
      valor          CHAR(10),
      formato        CHAR(10),
      ruta_envio     CHAR(40)
   END RECORD

   DEFINE x_longitud     SMALLINT,
          x_valor        CHAR(10),
          x_tipo         SMALLINT,
          x_formato      CHAR(10)

   DEFINE i  SMALLINT,
          x  SMALLINT


   OPEN WINDOW ventana_9 AT 2,2 WITH FORM "CTAB0108" ATTRIBUTE(BORDER)
   DISPLAY " MODIFICA " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY "<Enter> Modificar " AT 1,60
   DISPLAY " CTAB010              CATALOGO DE NOMBRES DE ARCHIVOS                          " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

   LET int_flag = FALSE

   CONSTRUCT cla_where ON modulo_cod,
                          programa_cod,
                          tipo_informe
                     FROM modulo_cod,
                          programa_cod,
                          tipo_informe

      ON KEY (CONTROL-M)
         LET int_flag = FALSE
         EXIT CONSTRUCT
      ON KEY (CONTROL-C)
         LET int_flag = TRUE
         EXIT CONSTRUCT
   END CONSTRUCT

   IF int_flag = TRUE THEN
      LET int_flag = FALSE
      ERROR "MODIFICA CANCELADA..."
      SLEEP 2
      ERROR ""
      CLEAR SCREEN
      CLOSE WINDOW ventana_9
      RETURN
   END IF

   LET sel_where = "SELECT numero_parametro,",
                          "programa_cod,",
                          "tipo_informe,",
                          "tipo_dato,",
                          "valor,",
                          "formato,",
                          "ruta_envio ",
                   "FROM   cta_formato_archivos ",
                   "WHERE ", cla_where CLIPPED,
                   "ORDER BY 3,1 "

   PREPARE query1 FROM sel_where
   DECLARE con2 CURSOR FOR query1

   LET pos = 1

   FOREACH con2 INTO l_record[pos].*
      LET l_mod[pos].*  = l_record[pos].*
      LET pos = pos + 1
   END FOREACH

   LET pos = pos - 1
   CALL Inicializa()

   IF pos >= 1 THEN
      CALL SET_COUNT(pos)

      INPUT ARRAY l_mod WITHOUT DEFAULTS FROM scr1.*
      #ATTRIBUTES(MAXCOUNT = pos,COUNT = pos)

         #BEFORE ROW
         BEFORE FIELD tipo_dato
            LET arr_c = ARR_CURR()
            LET arr_l = SCR_LINE()
            LET arr_t = ARR_COUNT()

            IF l_mod[arr_c].tipo_dato IS NULL OR
               l_mod[arr_c].tipo_dato = " " THEN
               ERROR "NO HAY MAS REGISTROS PARA MOSTRAR ..."
            END IF

         ON KEY(CONTROL-M)
         --AFTER FIELD tipo_dato
            IF l_mod[arr_c].tipo_dato IS NULL OR
               l_mod[arr_c].tipo_dato = " " THEN
                  ERROR "EL CAMPO NO PUEDE SER NULO"
            ELSE
               CASE l_mod[arr_c].tipo_dato
                  WHEN 1 CALL valores_fijos()
                            RETURNING x_longitud,
                                      x_valor
                         IF x_valor IS NULL OR x_valor = " " THEN
                            NEXT FIELD tipo_dato
                         ELSE
                            LET l_mod[arr_c].valor   = x_valor
                            LET l_mod[arr_c].formato = NULL
                         END IF
                  WHEN 2 CALL valores_variables()
                            RETURNING x_tipo,
                                      x_formato
                        -- IF x_formato IS NULL OR x_formato = " " THEN
                        --    NEXT FIELD tipo_dato
                        -- ELSE
                            CASE x_tipo
                               WHEN 1 
                                  LET l_mod[arr_c].valor   = "HOY"
                                  LET l_mod[arr_c].formato = x_formato
                               WHEN 2 
                                  LET l_mod[arr_c].valor   = "x_folio"
                                  LET l_mod[arr_c].formato = x_formato
                               WHEN 3 
                                  LET l_mod[arr_c].valor   = "g_usuario"
                                  LET l_mod[arr_c].formato = x_formato
                            END CASE
                         --END IF
                END CASE
             END IF

            ON KEY(ESC)
             PROMPT "Esta seguro S/N ? " FOR CHAR opc
                IF opc MATCHES "[Ss]" THEN
                   FOR x = 1 to arr_t
                     IF l_mod[x].tipo_dato IS NULL OR
                        l_mod[x].tipo_dato = 0 THEN
                     ELSE
                        UPDATE cta_formato_archivos
                        SET    tipo_dato = l_mod[x].tipo_dato,
                               valor     = l_mod[x].valor,
                               formato   = l_mod[x].formato
                        WHERE  numero_parametro = l_mod[x].numero_parametro 
                        AND    tipo_informe     = l_mod[x].tipo_informe
                     END IF
                  END FOR
                  ERROR "REGISTRO MODIFICADO "
                  SLEEP 2
                  ERROR ""
                ELSE 
                  ERROR "MODIFICA CANCELADO ..."
                  SLEEP 2
                  ERROR ""
                END IF

                CLEAR FORM
                EXIT INPUT

            ON KEY(INTERRUPT)
               CALL Inicializa()
               ERROR "PROCESO DE MODIFICA CANCELADO ..."
               CLEAR FORM
               EXIT INPUT
               CLOSE WINDOW ventana_9
            END INPUT
   ELSE
      ERROR "REGISTRO NO ENCONTRADO"
      CLEAR SCREEN
      CLOSE WINDOW ventana_9
      RETURN
   END IF
   CLOSE WINDOW ventana_9

END FUNCTION
#####################################################################
FUNCTION elimina()

   DEFINE l_record ARRAY[100] OF RECORD
       modulo_cod       CHAR(03),
       numero_parametro SMALLINT,      
       programa_cod   CHAR(15),
       tipo_informe   SMALLINT,
       tipo_dato      SMALLINT,
       valor          CHAR(10),
       formato        CHAR(10),
       ruta_envio     CHAR(40)
   END RECORD

   DEFINE l_elim ARRAY[100] OF RECORD
      numero_parametro SMALLINT,
      programa_cod   CHAR(15),
      tipo_informe   SMALLINT,
      tipo_dato      SMALLINT,
      valor          CHAR(10),
      formato        CHAR(10),
      ruta_envio     CHAR(40)
   END RECORD

   DEFINE x_longitud     SMALLINT,
          x_valor        CHAR(10),
          x_tipo         SMALLINT,
          x_formato      CHAR(10)

   DEFINE i  SMALLINT,
          x  SMALLINT

   OPEN WINDOW ventana_10 AT 2,2 WITH FORM "CTAB0108" ATTRIBUTE(BORDER)
   DISPLAY " ELIMINA " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY "<Enter> Eliminar  " AT 1,60
   DISPLAY " CTAB010              CATALOGO DE NOMBRES DE ARCHIVOS                          " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

   LET int_flag = FALSE

   CONSTRUCT cla_where ON modulo_cod,
                          programa_cod,
                          tipo_informe
                     FROM modulo_cod,
                          programa_cod,
                          tipo_informe

      ON KEY (CONTROL-M)
         LET int_flag = FALSE
         EXIT CONSTRUCT
      ON KEY (CONTROL-C)
         LET int_flag = TRUE
         EXIT CONSTRUCT
   END CONSTRUCT

   IF int_flag = TRUE THEN
      LET int_flag = FALSE
      ERROR "ELIMINA CANCELADA..."
      SLEEP 2
      ERROR ""
      CLEAR SCREEN
      CLOSE WINDOW ventana_10
      RETURN
   END IF

   LET sel_where = "SELECT modulo_cod,",
                          "numero_parametro,",
                          "programa_cod,",
                          "tipo_informe,",
                          "tipo_dato,",
                          "valor,",
                          "formato,",
                          "ruta_envio ",
                   "FROM   cta_formato_archivos ",
                   "WHERE ", cla_where CLIPPED,
                   "ORDER BY 4,2 "

   PREPARE query2 FROM sel_where
   DECLARE con3 CURSOR FOR query2

   LET pos = 1

   FOREACH con3 INTO l_record[pos].*
      LET l_elim[pos].numero_parametro = l_record[pos].numero_parametro 
      LET l_elim[pos].programa_cod = l_record[pos].programa_cod 
      LET l_elim[pos].tipo_informe = l_record[pos].tipo_informe 
      LET l_elim[pos].tipo_dato  = l_record[pos].tipo_dato 
      LET l_elim[pos].valor      = l_record[pos].valor 
      LET l_elim[pos].formato    = l_record[pos].formato 
      LET l_elim[pos].ruta_envio = l_record[pos].ruta_envio 
      LET pos = pos + 1
   END FOREACH

   LET pos = pos - 1

   IF pos >= 1 THEN
      CALL SET_COUNT(pos)

      DISPLAY ARRAY l_elim TO scr1.*
         ON KEY(CONTROL-M)
            LET pos = ARR_CURR()   

            PROMPT "Esta seguro S/N ? " FOR CHAR opc
            IF opc MATCHES "[Ss]" THEN
               DELETE  FROM cta_formato_archivos
               WHERE  numero_parametro = l_elim[pos].numero_parametro
               AND    modulo_cod = l_record[pos].modulo_cod
               AND    tipo_informe = l_elim[pos].tipo_informe
               AND    ruta_envio = l_elim[pos].ruta_envio

               ERROR "REGISTRO ELIMINADO "
               SLEEP 2
               ERROR ""
            ELSE
               ERROR "ELIMINA CANCELADO ..."
               SLEEP 2
               ERROR ""
            END IF

            CLEAR FORM
            EXIT DISPLAY

         ON KEY(INTERRUPT)
            CALL Inicializa()
            ERROR "PROCESO DE MELIMINA CANCELADO ..."
            CLEAR FORM
            EXIT DISPLAY
            CLOSE WINDOW ventana_10
      END DISPLAY
   ELSE
      ERROR "REGISTRO NO ENCONTRADO"
      CLEAR SCREEN
      CLOSE WINDOW ventana_10
      RETURN
   END IF
   CLOSE WINDOW ventana_10
  
END FUNCTION
#####################################################################
FUNCTION pantalla_modulo()

   DEFINE l_modulo ARRAY[100] OF RECORD
     modulo_cod    CHAR(03),
     modulo_desc   CHAR(20)
   END RECORD

   DEFINE vmodulo  CHAR(03)
   DEFINE pos      SMALLINT

   OPEN WINDOW ventana_2 AT 6,6 WITH FORM "CTAB0102" ATTRIBUTE(BORDER)
   DISPLAY " <Enter> Seleccion                " AT 2,1
   DISPLAY " Modulo         Descripcion       " AT 4,1 ATTRIBUTE(REVERSE)

   DECLARE cur_1 CURSOR FOR
   SELECT UPPER(modulo_cod),
          modulo_desc
   FROM   seg_modulo
   ORDER BY 1,2

   LET pos = 1
   FOREACH cur_1 INTO l_modulo[pos].*
      LET pos = pos + 1 
   END FOREACH

   INITIALIZE l_modulo[pos].* TO NULL

   LET pos = pos - 1

   IF pos >= 1 THEN
      CALL SET_COUNT(pos)
  
      DISPLAY ARRAY l_modulo TO scr_1.*
         ON KEY (CONTROL-M)
            LET pos = ARR_CURR()
            LET vmodulo = l_modulo[pos].modulo_cod
            EXIT DISPLAY
         ON KEY (INTERRUPT)
            ERROR "DEBE SELECCIONAR UN MODULO "
      END DISPLAY

      CLOSE WINDOW ventana_2
   ELSE
      ERROR "REGISTROS DE MODULO .... NO EXISTE"
      CLOSE WINDOW ventana_2
   END IF

   RETURN vmodulo

END FUNCTION
#######################################################################
FUNCTION pantalla_programa(vmodulo)

   DEFINE l_programa ARRAY[100] OF RECORD
     programa_cod    CHAR(10),
     programa_desc   CHAR(30)
   END RECORD

   DEFINE vprograma       CHAR(10)
   DEFINE vprograma_desc  CHAR(30)
   DEFINE vmodulo         CHAR(03)
   DEFINE pos             SMALLINT

   OPEN WINDOW ventana_3 AT 6,6 WITH FORM "CTAB0103" ATTRIBUTE(BORDER)
   DISPLAY " <Enter> Seleccion                            " AT 2,1
   DISPLAY " Programa              Descripcion            " AT 4,1 
           ATTRIBUTE(REVERSE)

   DECLARE cur_2 CURSOR FOR
   SELECT UPPER(programa_cod),
          programa_desc
   FROM   seg_programa
   WHERE  modulo_cod = LOWER(vmodulo)
   ORDER BY 1,2

   LET pos = 1
   FOREACH cur_2 INTO l_programa[pos].*
      LET pos = pos + 1
   END FOREACH

   INITIALIZE l_programa[pos].* TO NULL

   LET pos = pos - 1

   IF pos >= 1 THEN
      CALL SET_COUNT(pos)

      DISPLAY ARRAY l_programa TO scr_2.*
         ON KEY (CONTROL-M)
            LET pos = ARR_CURR()
            LET vprograma      = l_programa[pos].programa_cod
            LET vprograma_desc = l_programa[pos].programa_desc
            EXIT DISPLAY

         ON KEY (INTERRUPT)
            ERROR "DEBE SELECCIONAR UN PROGRAMA "
      END DISPLAY

      CLOSE WINDOW ventana_3
   ELSE
      ERROR "REGISTROS DE PROGRAMA .... NO EXISTE"
      CLOSE WINDOW ventana_3
   END IF

   RETURN vprograma,
          vprograma_desc

END FUNCTION
#######################################################################
FUNCTION pantalla_informe()

   DEFINE l_informe ARRAY[100] OF RECORD
     tipo_informe    SMALLINT, 
     dsc_informe     CHAR(40)
   END RECORD

   DEFINE vinforme       CHAR(10)
   DEFINE vinforme_desc  CHAR(30)
   DEFINE pos             SMALLINT

   OPEN WINDOW ventana_4 AT 6,6 WITH FORM "CTAB0104" ATTRIBUTE(BORDER)
   DISPLAY " <Enter> Seleccion                            " AT 2,1
   DISPLAY " Tipo informe          Descripcion            " AT 4,1
           ATTRIBUTE(REVERSE)

   DECLARE cur_3 CURSOR FOR
   SELECT tipo_informe,
          descripcion
   FROM   tab_tipo_informe
   ORDER BY 1

   LET pos = 1
   FOREACH cur_3 INTO l_informe[pos].*
      LET pos = pos + 1
   END FOREACH

   INITIALIZE l_informe[pos].* TO NULL

   LET pos = pos - 1

   IF pos >= 1 THEN
      CALL SET_COUNT(pos)

      DISPLAY ARRAY l_informe TO scr_3.*
         ON KEY (CONTROL-M)
            LET pos = ARR_CURR()
            LET vinforme      = l_informe[pos].tipo_informe
            LET vinforme_desc = l_informe[pos].dsc_informe
            EXIT DISPLAY

         ON KEY (INTERRUPT)
            ERROR "DEBE SELECCIONAR UN TIPO DE INFORME "
      END DISPLAY

      CLOSE WINDOW ventana_4
   ELSE
      ERROR "REGISTROS DE PROGRAMA .... NO EXISTE"
      CLOSE WINDOW ventana_4
   END IF

   RETURN vinforme,
          vinforme_desc

END FUNCTION
#######################################################################
FUNCTION pantalla_ruta()

   DEFINE l_ruta ARRAY[100] OF RECORD
     modulo_cod      CHAR(03),
     ruta_envio      CHAR(30) 
   END RECORD

   DEFINE vmodulo        CHAR(03)
   DEFINE vruta_envio    CHAR(30)
   DEFINE pos            SMALLINT

   OPEN WINDOW ventana_5 AT 6,6 WITH FORM "CTAB0105" ATTRIBUTE(BORDER)
   DISPLAY " <Enter> Seleccion                            " AT 2,1
   DISPLAY " Modulo                Descripcion            " AT 4,1
           ATTRIBUTE(REVERSE)

   DECLARE cur_4 CURSOR FOR
   SELECT UPPER(modulo_cod),
          ruta_envio
   FROM   seg_modulo
   ORDER BY 1

   LET pos = 1
   FOREACH cur_4 INTO l_ruta[pos].*
      LET pos = pos + 1
   END FOREACH

   INITIALIZE l_ruta[pos].* TO NULL

   LET pos = pos - 1

   IF pos >= 1 THEN
      CALL SET_COUNT(pos)

      DISPLAY ARRAY l_ruta TO scr_4.*
         ON KEY (CONTROL-M)
            LET pos = ARR_CURR()
            LET vruta_envio = l_ruta[pos].ruta_envio
            EXIT DISPLAY

         ON KEY (INTERRUPT)
            ERROR "DEBE SELECCIONAR UNA RUTA DESTINO "
      END DISPLAY

      CLOSE WINDOW ventana_5
   ELSE
      ERROR "REGISTROS DE PROGRAMA .... NO EXISTE"
      CLOSE WINDOW ventana_5
   END IF

   RETURN vruta_envio

END FUNCTION
#######################################################################
