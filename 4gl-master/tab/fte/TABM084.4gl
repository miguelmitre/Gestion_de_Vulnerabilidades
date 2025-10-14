-------------------------------------------------------------------------
# PROYECTO        => SISTEMA DE AFORES                                  #
# PROPIETARIO     => E.F.P.                                             #
# PROGRAMA        => CATALOGO DE AFORE LOCAL                            #
# FECHA           => 27 DE ENERO DEL 2004                               #
# AUTOR           => ERIKA PAOLA VERA PIÑA                              # 
-------------------------------------------------------------------------
DATABASE safre_af
GLOBALS
   DEFINE g_param_dis RECORD LIKE seg_modulo.*

   DEFINE aux_pausa  CHAR(1),
          sw_1       SMALLINT,
          hoy        DATE,
          usuario    CHAR(8),
          pos        INTEGER,
          sel_where  CHAR(300),
          cla_where  CHAR(300),
          g_impre    CHAR(300),
          g_lista    CHAR(300)

   DEFINE g_reg         RECORD
          codigo_afore  SMALLINT,
          razon_social  CHAR(50),
          representante CHAR(50),
          calle         CHAR(50),
          numero        INTEGER,
          depto         CHAR(10),
          colonia       CHAR(60),
          delegacion    SMALLINT,
          ciudad        SMALLINT,
          estado        SMALLINT,
          cod_postal    CHAR(10),
          telefono      CHAR(10)
   END RECORD
          
   DEFINE l_record ARRAY[10] OF RECORD
          codigo_afore  SMALLINT,
          razon_social  CHAR(50),
          representante CHAR(50),
          calle         CHAR(50),
          numero        INTEGER,
          depto         CHAR(10),
          colonia       CHAR(60),
          delegacion    SMALLINT,
          ciudad        SMALLINT,
          estado        SMALLINT,
          cod_postal    CHAR(10),
          telefono      CHAR(10)
   END RECORD
   DEFINE l_record1 ARRAY[10] OF RECORD
          codigo_afore  SMALLINT,
          razon_social  CHAR(50)
   END RECORD

   DEFINE x_colonia           SMALLINT,
          x_desc_delegacion   CHAR(40),
          x_desc_ciudad       CHAR(40),
          x_desc_estado       CHAR(40)

END GLOBALS
-----------------------------------------------------------------------------
MAIN
   OPTIONS 
      PROMPT LINE LAST,
      INPUT   WRAP,
      ACCEPT  KEY CONTROL-O

      DEFER INTERRUPT

      CALL inicio()
      CALL proceso()

END MAIN
-----------------------------------------------------------------------------
FUNCTION inicio()

   SELECT USER,
          ruta_listados
   INTO   usuario,
          g_param_dis.ruta_listados
   FROM   seg_modulo
   WHERE  modulo_cod = "tab"

END FUNCTION
----------------------------------------------------------------------------
FUNCTION proceso()

   LET HOY = TODAY

   OPEN WINDOW ventana_1 AT 3,3 WITH FORM "TABM0841" ATTRIBUTE (BORDER)

   DISPLAY " TABM084                 CATALOGOS DE AFORE LOCAL                              " AT 3,1    ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)

   MENU "CATALOGO DE AFORES "
--      COMMAND " Agrega" "Agrega Afore"
--         CALL Agrega()
--         CLEAR SCREEN
      COMMAND "Consulta" "Consulta Afore"
         CALL Consulta()
         CLEAR SCREEN
      COMMAND "Modifica" "Modifica Afore"
         CALL Modifica()
         CLEAR SCREEN
--      COMMAND "Elimina" "Elimina  Afore"
--         CALL Elimina()
--         CLEAR SCREEN
      COMMAND "Salir" "Salir del Programa"
         EXIT MENU
   END MENU

   CLOSE WINDOW ventana_1

END FUNCTION
----------------------------------------------------------------------------
FUNCTION Inicializa()

   LET sw_1 = 0

   INITIALIZE g_reg.* TO NULL

   DISPLAY BY NAME g_reg.codigo_afore,
                   g_reg.razon_social,
                   g_reg.representante,
                   g_reg.calle,
                   g_reg.numero,
                   g_reg.depto,
                   g_reg.cod_postal,
                   g_reg.colonia,
                   g_reg.delegacion,
                   g_reg.ciudad,
                   g_reg.estado,
                   g_reg.telefono
END FUNCTION
-----------------------------------------------------------------------------
FUNCTION Agrega()

   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " (ESC ) Agrega                  (CTRL-C) Salir                                 " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " AGREGA " AT 1,64 ATTRIBUTE(REVERSE)

   LET g_reg.razon_social = NULL
   LET sw_1 = 0
   
   INPUT BY NAME g_reg.codigo_afore,
                 g_reg.razon_social,
                 g_reg.representante,
                 g_reg.calle,
                 g_reg.numero,
                 g_reg.depto,
                 g_reg.cod_postal,
                 g_reg.colonia,
                 g_reg.delegacion,
                 g_reg.ciudad,
                 g_reg.estado,
                 g_reg.telefono

   BEFORE FIELD codigo_afore

      IF sw_1 = 0 THEN
         LET sw_1 = 1

         SELECT MAX(codigo_afore)
         INTO   g_reg.codigo_afore
         FROM   tab_afore_local

         IF g_reg.codigo_afore = 0 OR
            g_reg.codigo_afore IS NULL THEN
            LET g_reg.codigo_afore = 1
         ELSE
            LET g_reg.codigo_afore = g_reg.codigo_afore + 1
         END IF

         DISPLAY BY NAME g_reg.codigo_afore
      END IF

   AFTER FIELD codigo_afore

       IF g_reg.codigo_afore IS NULL THEN
          ERROR "Codigo del Afore NO Puede Ser Nulo"
          NEXT FIELD codigo_afore
       END IF

       SELECT "X"
       FROM   tab_afore_local
       WHERE  codigo_afore = g_reg.codigo_afore

       IF STATUS <> NOTFOUND THEN
          ERROR "Codigo ya Ingresado"
          NEXT FIELD codigo_afore
       END IF

    AFTER FIELD razon_social

       IF g_reg.razon_social IS NULL THEN
          ERROR "NO puede ser nulo"
          NEXT FIELD razon_social
       END IF

       SELECT "X"
       FROM   tab_afore_local
       WHERE  razon_social = g_reg.razon_social

       IF STATUS <> NOTFOUND THEN
          ERROR "Ya Ingresado"
          NEXT FIELD razon_social
       END IF

    AFTER FIELD representante

       IF g_reg.representante IS NULL THEN
          ERROR "NO puede ser nulo"
          NEXT FIELD representante
       END IF

    AFTER FIELD calle

       IF g_reg.calle IS NULL THEN
          ERROR "NO puede ser nulo"
          NEXT FIELD calle
       END IF

    AFTER FIELD numero

       IF g_reg.numero IS NULL THEN
		    ERROR "NO puede ser nulo"
			 NEXT FIELD numero
		 END IF

    AFTER FIELD depto

    AFTER FIELD cod_postal

       IF g_reg.cod_postal IS NULL THEN

       CALL Despliega_codigo_postal()

       RETURNING g_reg.cod_postal,    -- codigo_postal
                 g_reg.colonia,       -- descripcion colonia
                 g_reg.delegacion,    -- codigo_delegacion
                 x_desc_delegacion,   -- descripcion delegacion
                 g_reg.ciudad,        -- codigo_ciudad
                 x_desc_ciudad,       -- descripcion ciudad
                 g_reg.estado,        -- codigo_estado
                 x_desc_estado        -- descripcion estado
       ELSE
          IF LENGTH(g_reg.cod_postal) <> 5 THEN
             ERROR "Ingrese cinco digitos para indicar Codigo Postal"
             NEXT FIELD cod_postal
          END IF

          SELECT "X" 
          FROM   tab_codpos
          WHERE  cpos_cod = g_reg.cod_postal

          IF STATUS = NOTFOUND THEN
             ERROR "Codigo Postal no existe en catalogo"
             NEXT FIELD cod_postal
          ELSE
             CALL Despliega_colonias(g_reg.cod_postal)
             RETURNING g_reg.colonia,       -- descripcion colonia
                       g_reg.delegacion,    -- codigo_delegacion
                       x_desc_delegacion,   -- descripcion delegacion
                       g_reg.ciudad,        -- codigo_ciudad
                       x_desc_ciudad,       -- descripcion ciudad
                       g_reg.estado,        -- codigo_estado
                       x_desc_estado        -- descripcion estado
          END IF
       END IF 

       DISPLAY BY NAME g_reg.colonia,
                       g_reg.delegacion,
                       g_reg.ciudad,
                       g_reg.estado

       NEXT FIELD telefono

    AFTER FIELD telefono

       ON KEY ( ESC )
          IF g_reg.codigo_afore IS NULL THEN
             ERROR "NO puede ser nulo"
             NEXT FIELD codigo_afore
          END IF

          SELECT "X"
          FROM   tab_afore_local
          WHERE  codigo_afore = g_reg.codigo_afore

          IF STATUS <> NOTFOUND THEN
             ERROR "Ya Ingresado"
             NEXT FIELD codigo_afore
          END IF

          IF g_reg.razon_social IS NULL THEN
             ERROR "NO puede ser nulo"
             NEXT FIELD razon_social
          END IF

          IF g_reg.representante IS NULL THEN
             ERROR "NO puede ser nulo"
             NEXT FIELD representante
          END IF

          IF g_reg.calle IS NULL THEN
             ERROR "NO puede ser nulo"
             NEXT FIELD calle
          END IF

          IF g_reg.numero IS NULL THEN
             ERROR "NO puede ser nulo"
             NEXT FIELD numero
          END IF

          IF g_reg.cod_postal IS NULL THEN
             ERROR "NO puede ser nulo"
             NEXT FIELD cod_postal
          END IF

          INSERT INTO tab_afore_local
          VALUES(g_reg.*)
          ERROR "REGISTRO INGRESADO"
          SLEEP 2
          ERROR ""

          CALL  Inicializa()

          NEXT FIELD codigo_afore 

          ON KEY (INTERRUPT)
             CALL Inicializa()
             EXIT INPUT
          END INPUT

   CLEAR FORM
   CLEAR SCREEN
END FUNCTION
-----------------------------------------------------------------------------
FUNCTION Consulta()

   LET pos = 2

   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)

      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0842" ATTRIBUTE( BORDER)

      DISPLAY " (ENTER) Consulta               (CTRL-P) Imprimir          (CTRL-C)Salir          " AT 1,1 ATTRIBUTE(REVERSE)
      DISPLAY "                                 A F O R E                                     " AT 3,1 ATTRIBUTE(REVERSE)
      DISPLAY "          Escoja con <ENTER> el registro a Consultar                           " AT 2,1
 

      CONSTRUCT cla_where ON codigo_afore
                        FROM codigo_afore

         ON KEY (CONTROL-M)
            ERROR "PROCESANDO INFORMACION..."
            SLEEP 2
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (CONTROL-C)
            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT
 
      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "BUSQUEDA CANCELADA ..."
         SLEEP 2
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      LET sel_where = " SELECT * ",
                      " FROM tab_afore_local ",
                      " WHERE ",cla_where CLIPPED,
                      " ORDER BY 1"

      PREPARE query FROM sel_where

      DECLARE cursor_1 CURSOR FOR query

      LET pos = 1

      FOREACH cursor_1 INTO l_record[pos].*
         LET l_record1[pos].codigo_afore = l_record[pos].codigo_afore
         LET l_record1[pos].razon_social = l_record[pos].razon_social
         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)
         ERROR ""

         DISPLAY ARRAY l_record1 TO scr_1.*

         ON KEY (CONTROL-M)
            LET pos = ARR_CURR()
            LET g_reg.codigo_afore = l_record[pos].codigo_afore
            LET g_reg.razon_social = l_record[pos].razon_social
            LET g_reg.representante = l_record[pos].representante
            LET g_reg.calle = l_record[pos].calle
            LET g_reg.numero = l_record[pos].numero
            LET g_reg.depto = l_record[pos].depto
            LET g_reg.colonia = l_record[pos].colonia
            LET g_reg.delegacion = l_record[pos].delegacion
            LET g_reg.ciudad = l_record[pos].ciudad
            LET g_reg.estado = l_record[pos].estado
            LET g_reg.cod_postal = l_record[pos].cod_postal
            LET g_reg.telefono = l_record[pos].telefono
            EXIT DISPLAY
         ON KEY (CONTROL-P)
            ERROR "PROCESANDO INFORMACION ..."
            CALL impresion(pos)
         ON KEY (INTERRUPT)
            EXIT DISPLAY
         END DISPLAY
         CLOSE WINDOW ventana_2
      ELSE
         ERROR "ARCHIVO ... VACIO"
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF
   END IF

   DISPLAY BY NAME g_reg.codigo_afore,
                   g_reg.razon_social,
                   g_reg.representante,
                   g_reg.calle,
                   g_reg.numero,
                   g_reg.depto,
                   g_reg.colonia,
                   g_reg.delegacion,
                   g_reg.ciudad,
                   g_reg.estado,
                   g_reg.cod_postal,
                   g_reg.telefono

   PROMPT "Con <ENTER> Sale de la Consulta" for aux_pausa
   CLEAR FORM
   CLEAR SCREEN
END FUNCTION
-----------------------------------------------------------------------------
FUNCTION Modifica()

   LET pos = 2

   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)

      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0842" ATTRIBUTE(BORDER)

      DISPLAY " (ENTER) Consulta                                       (CTRL-C)Salir          " AT 1,1 ATTRIBUTE(REVERSE)
      DISPLAY "          Escoja con <ENTER> El Registro a Modificar                           " AT 2,1
      DISPLAY "                                A F O R E                                      " AT 3,1 ATTRIBUTE(REVERSE)
 
      LET int_flag = FALSE

      CONSTRUCT cla_where ON codigo_afore
                        FROM codigo_afore

         ON KEY (CONTROL-M)
            ERROR "PROCESANDO INFORMACION..."
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (CONTROL-C)
            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT

         IF int_flag = TRUE THEN
            LET int_flag = FALSE
            ERROR "BUSQUEDA CANCELADA ..."
            SLEEP 2
            ERROR ""
            CLEAR SCREEN
            CLOSE WINDOW ventana_2
            RETURN
         END IF

         LET sel_where = " SELECT * ",
                         " FROM tab_afore_local  WHERE ",
                           cla_where CLIPPED,
                         " ORDER BY 1 "

         PREPARE query1 FROM sel_where

         DECLARE cursor_2 CURSOR FOR query1

         LET pos = 1

         FOREACH cursor_2 INTO l_record[pos].*
            LET l_record1[pos].codigo_afore = l_record[pos].codigo_afore
            LET l_record1[pos].razon_social = l_record[pos].razon_social
            LET pos = pos + 1
         END FOREACH

         INITIALIZE l_record[pos].* TO NULL

         IF (pos-1) >= 1 THEN
            CALL SET_COUNT(pos-1)
            ERROR ""

         DISPLAY ARRAY l_record1 TO scr_1.*

         ON KEY (CONTROL-M)
            LET pos = ARR_CURR()
            LET g_reg.codigo_afore = l_record[pos].codigo_afore
            LET g_reg.razon_social = l_record[pos].razon_social
            LET g_reg.representante = l_record[pos].representante
            LET g_reg.calle = l_record[pos].calle
            LET g_reg.numero = l_record[pos].numero
            LET g_reg.depto = l_record[pos].depto
            LET g_reg.colonia = l_record[pos].colonia
            LET g_reg.delegacion = l_record[pos].delegacion
            LET g_reg.ciudad = l_record[pos].ciudad
            LET g_reg.estado = l_record[pos].estado
            LET g_reg.cod_postal = l_record[pos].cod_postal
            LET g_reg.telefono = l_record[pos].telefono
            EXIT DISPLAY
         ON KEY (INTERRUPT)
            ERROR "Debe elegir un registro."
            LET pos = ARR_CURR()
         END DISPLAY
         CLOSE WINDOW ventana_2
     ELSE
         ERROR "ARCHIVO... VACIO"
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
     END IF
 
     DISPLAY BY NAME g_reg.codigo_afore,
                     g_reg.razon_social,
                     g_reg.representante,
                     g_reg.calle,
                     g_reg.numero,
                     g_reg.depto,
                     g_reg.colonia,
                     g_reg.delegacion,
                     g_reg.ciudad,
                     g_reg.estado,
                     g_reg.cod_postal,
                     g_reg.telefono

     DISPLAY "" AT 1,1
     DISPLAY "" AT 2,1
     DISPLAY " (CTRL-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
     DISPLAY "  MODIFICA " AT 1,66 ATTRIBUTE(REVERSE)
     INPUT BY NAME g_reg.codigo_afore,
                   g_reg.razon_social,
                   g_reg.representante,
                   g_reg.calle,
                   g_reg.numero,
                   g_reg.depto,
                   g_reg.cod_postal,
                   g_reg.colonia,
                   g_reg.delegacion,
                   g_reg.ciudad,
                   g_reg.estado,
                   g_reg.telefono WITHOUT DEFAULTS

   BEFORE FIELD codigo_afore
      NEXT FIELD razon_social

   AFTER FIELD razon_social
      IF g_reg.razon_social IS NULL THEN
         ERROR "NO debe ser nulo."
         NEXT FIELD razon_social
      END IF

   AFTER FIELD representante
      IF g_reg.representante IS NULL THEN
         ERROR "NO debe ser nulo."
         NEXT FIELD representante
      END IF

   AFTER FIELD calle
      IF g_reg.calle IS NULL THEN
         ERROR "NO debe ser nulo."
         NEXT FIELD calle
      END IF

   AFTER FIELD numero
      IF g_reg.numero IS NULL THEN
         ERROR "NO debe ser nulo."
         NEXT FIELD numero
      END IF

   AFTER FIELD depto

    AFTER FIELD cod_postal

       IF g_reg.cod_postal IS NULL THEN

       CALL Despliega_codigo_postal()

       RETURNING g_reg.cod_postal,    -- codigo_postal
                 g_reg.colonia,       -- descripcion colonia
                 g_reg.delegacion,    -- codigo_delegacion
                 x_desc_delegacion,   -- descripcion delegacion
                 g_reg.ciudad,        -- codigo_ciudad
                 x_desc_ciudad,       -- descripcion ciudad
                 g_reg.estado,        -- codigo_estado
                 x_desc_estado        -- descripcion estado

       ELSE
          IF LENGTH(g_reg.cod_postal) <> 5 THEN
             ERROR "Ingrese cinco digitos para indicar Codigo Postal"
             NEXT FIELD cod_postal
          END IF

          SELECT "X"
          FROM   tab_codpos
          WHERE  cpos_cod = g_reg.cod_postal

          IF STATUS = NOTFOUND THEN
             ERROR "Codigo Postal no existe en catalogo"
             NEXT FIELD cod_postal
          ELSE
             CALL Despliega_colonias(g_reg.cod_postal)
             RETURNING g_reg.colonia,       -- descripcion colonia
                       g_reg.delegacion,    -- codigo_delegacion
                       x_desc_delegacion,   -- descripcion delegacion
                       g_reg.ciudad,        -- codigo_ciudad
                       x_desc_ciudad,       -- descripcion ciudad
                       g_reg.estado,        -- codigo_estado
                       x_desc_estado        -- descripcion estado
                END IF
       END IF

       DISPLAY BY NAME g_reg.colonia,
                       g_reg.delegacion,
                       g_reg.ciudad,
                       g_reg.estado
   
   NEXT FIELD telefono
       
   AFTER FIELD telefono 
    
   CALL Pregunta()

     IF aux_pausa MATCHES "[Ss]" THEN
        UPDATE tab_afore_local SET
               razon_social = g_reg.razon_social,
               representante = g_reg.representante,
               calle = g_reg.calle,
               numero = g_reg.numero,
               depto = g_reg.depto,
               cod_postal = g_reg.cod_postal,
               colonia = g_reg.colonia,
               delegacion = g_reg.delegacion,
               ciudad = g_reg.ciudad,
               estado = g_reg.estado,
               telefono = g_reg.telefono
         WHERE codigo_afore = g_reg.codigo_afore
         ERROR "REGISTRO MODIFICADO"
         SLEEP 2
         ERROR ""
        CALL Inicializa()
     ELSE
         ERROR "PROCESO DE MODIFICAR CANCELADO."
         SLEEP 2
         ERROR ""
     END IF
     EXIT INPUT

   ON KEY (INTERRUPT)
        CALL Inicializa()
        EXIT INPUT
        END INPUT
     ELSE
        ERROR "ARCHIVO... VACIO."
     END IF

     CLEAR FORM
     CLEAR SCREEN
END FUNCTION
---------------------------------------------------------------------------
FUNCTION Elimina()

   LET pos = 2

   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)

      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TABM0842" ATTRIBUTE(BORDER)

      DISPLAY " (ENTER) Consulta                                        (CTRL-C) Salir     " AT 1,1 ATTRIBUTE (REVERSE)
      DISPLAY "              Escoga con <ENTER> el Registro a Eliminar                        " AT 2,1
      DISPLAY "                               A F O R E                                      " AT 3,1 ATTRIBUTE(REVERSE)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON codigo_afore
                        FROM codigo_afore

         ON KEY (CONTROL-M)
            ERROR "PROCESANDO INFORMACION..."
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
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      LET sel_where = " SELECT * FROM tab_afore_local WHERE ",
                        cla_where CLIPPED,
                      " ORDER BY 1 "

      PREPARE query2 FROM sel_where

      DECLARE cursor_3 CURSOR FOR query2

      LET pos = 1

      FOREACH cursor_3 INTO l_record[pos].*
         LET l_record1[pos].codigo_afore = l_record[pos].codigo_afore
         LET l_record1[pos].razon_social = l_record[pos].razon_social
         LET g_reg.representante = l_record[pos].representante
         LET g_reg.calle = l_record[pos].calle
         LET g_reg.numero = l_record[pos].numero
         LET g_reg.depto = l_record[pos].depto
         LET g_reg.colonia = l_record[pos].colonia
         LET g_reg.delegacion = l_record[pos].delegacion
         LET g_reg.ciudad = l_record[pos].ciudad
         LET g_reg.estado = l_record[pos].estado
         LET g_reg.cod_postal = l_record[pos].cod_postal                                 LET g_reg.telefono = l_record[pos].telefono
         LET pos = pos + 1
      END FOREACH

       INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)
         ERROR ""

      DISPLAY ARRAY l_record1 TO scr_1.*

      ON KEY (CONTROL-M)
         LET pos = ARR_CURR()
         LET g_reg.codigo_afore = l_record[pos].codigo_afore
         LET g_reg.razon_social = l_record[pos].razon_social 
         EXIT DISPLAY
      ON KEY (INTERRUPT)
         ERROR "Debe elegir una Afore,"
         LET pos = ARR_CURR()
         END DISPLAY
         CLOSE WINDOW ventana_2
       ELSE
         ERROR "ARCHIVO DE INFORMES... VACIO"
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " (CTRL-C) Salir " AT 1,1 ATTRIBUTE(BOLD)
      DISPLAY "  ELIMINA " AT 1,66 ATTRIBUTE(REVERSE,BOLD)

      DISPLAY BY NAME g_reg.codigo_afore,
                      g_reg.razon_social,
                      g_reg.representante,
                      g_reg.calle,
                      g_reg.numero,
                      g_reg.colonia,
                      g_reg.delegacion,
                      g_reg.ciudad,
                      g_reg.estado,
                      g_reg.cod_postal,
                      g_reg.telefono

      CALL Pregunta()

      IF aux_pausa MATCHES "[Ss]" THEN
         DELETE FROM tab_afore_local
         WHERE codigo_afore = g_reg.codigo_afore

         ERROR "REGISTRO ELIMINADO."
         SLEEP 2
         ERROR ""
      ELSE
         ERROR "PROCESO CANCELADO."
         SLEEP 2
         ERROR ""
      END IF

      CALL Inicializa()
      ELSE
         ERROR "ARCHIVO DE INFORMES... VACIO."
      END IF
         CLEAR FORM
         CLEAR SCREEN
END FUNCTION
----------------------------------------------------------------------------
FUNCTION Pregunta()

   PROMPT "Estas seguro S/N ? " FOR CHAR aux_pausa

END FUNCTION
-----------------------------------------------------------------------------
FUNCTION impresion(pos)

   DEFINE i, pos SMALLINT

   LET g_impre = g_param_dis.ruta_listados CLIPPED,"/",
                 usuario CLIPPED, ".IMPCPTOECTA",
                 hoy USING "dd-mm-yyyy" CLIPPED

    START REPORT rpt_tabtipinfo to g_impre

    FOR i = 1 to (pos+1)
        LET g_reg.codigo_afore = l_record[i].codigo_afore
        LET g_reg.razon_social = l_record[i].razon_social
        LET g_reg.representante = l_record[i].representante
        LET g_reg.calle = l_record[i].calle
        LET g_reg.numero = l_record[i].numero
        LET g_reg.depto = l_record[i].depto
        LET g_reg.colonia = l_record[i].colonia
        LET g_reg.delegacion = l_record[i].delegacion
        LET g_reg.ciudad = l_record[i].ciudad
        LET g_reg.estado = l_record[i].estado
        LET g_reg.cod_postal = l_record[i].cod_postal
        LET g_reg.telefono = l_record[i].telefono


        IF g_reg.codigo_afore IS NULL THEN
           EXIT FOR
        END IF

       OUTPUT TO REPORT rpt_tabtipinfo(g_reg.*)
    END FOR

    FINISH REPORT rpt_tabtipinfo
       ERROR "LISTADO GENERADO..."
       SLEEP 2
       ERROR ""

       LET g_lista = "lp ", g_impre
       RUN g_lista

END FUNCTION
-----------------------------------------------------------------------------
REPORT rpt_tabtipinfo(g_reg)

    DEFINE g_reg         RECORD
          codigo_afore  SMALLINT,
          razon_social  CHAR(50),
          representante CHAR(50),
          calle         CHAR(50),
          numero        INTEGER,
          depto         CHAR(10),
          colonia       CHAR(15),
          delegacion    SMALLINT,
          ciudad        SMALLINT,
          estado        SMALLINT,
          cod_postal    CHAR(5),
          telefono      CHAR(10)
   END RECORD

   OUTPUT
      TOP MARGIN    1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   88

   FORMAT
      PAGE HEADER
         PRINT COLUMN 01,'\033e\033(10U\033&l1O\033&k2S\033&l12d'
         PRINT COLUMN 01,'\033(s13H\033(s7B'
         PRINT COLUMN 02," TABM084 ",
               COLUMN 128,TODAY USING "dd-mm-yyyy"
    
         PRINT COLUMN 01,'\033e\033(s218T\033(s9H\033(s7B'

         PRINT COLUMN 34," CATALOGO DE AFORE LOCAL "

         PRINT COLUMN 01,'\033e\033(s218T\033(s15H\033(s7B'
         SKIP 2 LINE

         PRINT COLUMN 01,"COD",
               COLUMN 05,"RAZON SOCIAL",
               COLUMN 30,"REPRESENTANTE",
               COLUMN 60,"CALLE",
               COLUMN 90,"NUM",
               COLUMN 95,"DEPTO",
               COLUMN 106,"COLONIA",
               COLUMN 121,"DEL",
               COLUMN 125,"CD",
               COLUMN 129,"EDO",
               COLUMN 135,"C.P.",
               COLUMN 141,"TEL"
         SKIP 1 LINE
      ON EVERY ROW
         PRINT COLUMN 01,'\033e\033(s218T\033(s15H\033(s7B'
         SKIP 1 LINE
         PRINT COLUMN 01,g_reg.codigo_afore USING "<<<",
               COLUMN 05,g_reg.razon_social CLIPPED,
               COLUMN 30,g_reg.representante CLIPPED,
               COLUMN 60,g_reg.calle CLIPPED,
               COLUMN 90,g_reg.numero USING "<<<<<",
               COLUMN 95,g_reg.depto CLIPPED,
               COLUMN 106,g_reg.colonia CLIPPED,
               COLUMN 121,g_reg.delegacion USING "<<<",
               COLUMN 125,g_reg.ciudad USING "<<<",
               COLUMN 129,g_reg.estado USING "<<<",
               COLUMN 135,g_reg.cod_postal ,
               COLUMN 141,g_reg.telefono 
             SKIP 2 LINE
          ON LAST ROW
             PRINT COLUMN 01," TOTAL DE REGISTROS : ",COUNT(*) USING "<<<"
END REPORT
---------------------------------------------------------------------------
FUNCTION Despliega_codigo_postal()
   DEFINE aux_vas SMALLINT
	
   DEFINE l_reg ARRAY[1000] OF RECORD
          cod           CHAR(05),
	  descrip       CHAR(25),
	  descripcion	CHAR(25)
   END RECORD,
          reg       RECORD
          cod_colon SMALLINT,
          colonia   CHAR(40),
          deleg     SMALLINT, 
          ciudad    SMALLINT,
          estado    SMALLINT
   END RECORD,
          desdeleg  CHAR(40),
          desciuda  CHAR(40),
          desestad  CHAR(40),
          vestad    SMALLINT 
    
   DEFINE x_x      CHAR(300),
          x_buscar CHAR(30)
	
   DEFINE pos   SMALLINT
 
   OPEN WINDOW vent_1 AT 05,07 WITH FORM "TABM0843" ATTRIBUTE(BORDER)
 
   DISPLAY "                        CODIGOS POSTALES                          " AT 2,1 ATTRIBUTE(REVERSE)

   INPUT BY NAME x_buscar
	      
   AFTER FIELD x_buscar
		    
      IF x_buscar IS NULL THEN
         ERROR "Descripcion a Buscar NO puede ser nulo"
         NEXT FIELD x_buscar
       ELSE
         EXIT INPUT 
      END IF
    END INPUT
   
  WHENEVER ERROR CONTINUE
       
   PROMPT 'DIGITA CODIGO ESTADO DE ESTA COLONIA ...' for vestad attribute(reverse)
   WHENEVER ERROR STOP

   IF vestad IS NULL THEN
      ERROR "SOLO PUEDE SER CODIGO" SLEEP 1 
   END IF

   IF vestad IS NULL THEN
       LET vestad=0
   END IF

   ERROR "BUSCANDO INFORMACION ..."

   WHILE TRUE


   LET x_x = " SELECT c.cpos_cod,a.colon_desc,b.deleg_desc FROM ",
             " tab_codpos c,tab_colonia a,tab_delegacion b ",
             " WHERE c.cpos_cod=a.cpos_cod and c.deleg_cod=b.deleg_cod ",
             " and a.colon_desc MATCHES ",'"*',x_buscar CLIPPED,'*"',
             " and c.estad_cod=",vestad CLIPPED,
             " ORDER BY 2 " CLIPPED
 
   PREPARE curg21 FROM x_x
	   DECLARE cur_g21 CURSOR FOR curg21
	   LET pos = 1
	   FOREACH cur_g21 INTO l_reg[pos].*
           
           IF status=100 THEN
              EXIT FOREACH
           END IF
		 
           LET pos = pos + 1
	
       	   IF pos >= 1000 THEN
              ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
       	      EXIT FOREACH
           END IF
	   END FOREACH
           ERROR ""
	   
           IF (pos-1) < 1 THEN
	       ERROR "ARCHIVO CODIGOS POSTALES... VACIO"
	   END IF
	   
           CALL SET_COUNT(pos-1)
           DISPLAY ARRAY l_reg TO scr_1.*
	           
           ON KEY ( INTERRUPT )
              LET pos = 0
              LET pos = ARR_CURR()
              LET l_reg[pos].descripcion = NULL
              LET reg.deleg = NULL
              LET desdeleg  = NULL
              LET reg.ciudad= NULL
              LET desciuda  = NULL
              LET reg.estado= NULL
              LET desestad  = NULL
	      EXIT DISPLAY
           ON KEY ( CONTROL-M )
              LET pos = ARR_CURR()
      	      SELECT deleg_cod,ciudad_cod,estad_cod 
              INTO   reg.deleg,reg.ciudad,reg.estado
              FROM tab_codpos  
              WHERE cpos_cod = l_reg[pos].cod
          
              SELECT deleg_desc 
              INTO desdeleg 
              FROM tab_delegacion 
              WHERE deleg_cod = reg.deleg

              SELECT ciudad_desc INTO desciuda
              FROM tab_ciudad 
              WHERE ciudad_cod = reg.ciudad
          
              SELECT estad_desc INTO desestad
              FROM tab_estado 
              WHERE estad_cod = reg.estado
              EXIT DISPLAY
	      END DISPLAY
	      
           IF pos <> 0 THEN
	      EXIT WHILE
	   END IF
	END WHILE

      CLOSE WINDOW vent_1

      RETURN l_reg[pos].cod,       -- codigo_colonia
             l_reg[pos].descrip,   -- descripcion colonia
             reg.deleg,            -- codigo_delegacion
             desdeleg,             -- descripcion delegacion
             reg.ciudad,           -- codigo_ciudad
             desciuda,             -- descripcion ciudad
             reg.estado,           -- codigo_estado
             desestad              -- descripcion estado
END FUNCTION
-----------------------------------------------------------------------
FUNCTION Despliega_colonias(xcpos_cod)

   DEFINE xcpos_cod     CHAR(05), 
          aux_val	SMALLINT,
          x_x 		CHAR(300),
          x_buscar 	CHAR(30),
          pos		SMALLINT

   DEFINE reg RECORD
          cod_colon     SMALLINT,
          colonia       CHAR(40),
          deleg         SMALLINT, 
          ciudad        SMALLINT,
          estado        SMALLINT
   END RECORD

   DEFINE desdeleg      CHAR(40),
          desciuda      CHAR(40),
          desestad      CHAR(40)

   DEFINE l_reg ARRAY[1000] OF RECORD
          cod           CHAR(05),
          codigo	INTEGER,
          descripcion	CHAR(40)
   END RECORD

   ERROR "BUSCANDO INFORMACION ..."

   DECLARE cur_cp CURSOR FOR
   SELECT cpos_cod,
          colon_cod,
          colon_desc 
   FROM tab_colonia
   WHERE cpos_cod = xcpos_cod
#  ORDER BY 2 
      
   LET pos = 1

   FOREACH cur_cp INTO l_reg[pos].*
      LET pos = pos + 1
   END FOREACH

   ERROR ""

   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)
   
      OPEN WINDOW ventana_cp AT 6,08 WITH FORM "TABM0844" ATTRIBUTE (BORDER)
      
      DISPLAY " (ENTER) Elegir                                   (Ctrl - C) Salir " AT 1,1 ATTRIBUTE(BOLD)
   
      DISPLAY "                      C  O  L  O  N  I  A  S                       " AT 2,1 ATTRIBUTE (REVERSE,BOLD)
     
       DISPLAY ARRAY l_reg to scr_1.*
      
        ON KEY (CONTROL-M)
           LET pos = ARR_CURR()
           
            SELECT deleg_cod,
                   ciudad_cod,
                   estad_cod          
            INTO   reg.deleg,
                   reg.ciudad,
                   reg.estado       
            FROM   tab_codpos                          
            WHERE  cpos_cod = l_reg[pos].cod              
                                              
            SELECT deleg_desc                            
            INTO   desdeleg                                
            FROM   tab_delegacion                         
            WHERE  deleg_cod = reg.deleg                  
                                              
            SELECT ciudad_desc 
            INTO   desciuda 
            FROM   tab_ciudad 
            WHERE  ciudad_cod = reg.ciudad                        
                                              
            SELECT estad_desc 
            INTO   desestad 
            FROM   tab_estado 
            WHERE  estad_cod = reg.estado                       

            EXIT DISPLAY
  
            ON KEY(INTERRUPT)
               LET pos = ARR_CURR()
               LET l_reg[pos].descripcion = NULL
               LET reg.deleg = NULL
               LET desdeleg  = NULL
               LET reg.ciudad= NULL
               LET desciuda  = NULL
               LET reg.estado= NULL
               LET desestad  = NULL
               EXIT DISPLAY
         END DISPLAY
         CLOSE WINDOW ventana_cp
      ELSE
         ERROR "ARCHIVO DE COLONIAS ..... VACIO"
      END IF

      RETURN
         l_reg[pos].descripcion,
         reg.deleg,
         desdeleg,
         reg.ciudad,
         desciuda,
         reg.estado,
         desestad 

END FUNCTION
-----------------------------------------------------------------------
