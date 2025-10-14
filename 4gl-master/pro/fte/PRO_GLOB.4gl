###############################################################################
#Proyecto          => SISTEMA DE AFORES.( MEXICO )                            #
#Sistema           => PRO.                                                    #
#Programa PRO_GLOB => FUNCIONES GLOBALES                                      #
#Fecha             => 23 DE MARZO DE 1999                                     #
#ELABORADO  Por    => FRANCO ESTEBAN ULLOA                                    #
#Fecha Modificacion=> 23 de Marzo del 2004                                    #
#Modificado Por    => LAURA EUGENIA CORTES GUZMAN                             #
###############################################################################

DATABASE  safre_af

FUNCTION Despliega_localidades()
   DEFINE aux_valSMALLINT,

          l_reg ARRAY[1000] OF RECORD
              codigo      INTEGER,
              descripcion CHAR(50)
          END RECORD,

          x_x             CHAR(100),
          x_buscar        CHAR(30),
          pos             SMALLINT

   OPEN WINDOW vent_1 AT 05,12 WITH FORM "PRO_GLOBP1" ATTRIBUTE(BORDER)
   DISPLAY "                   L O C A L I D A D E S                 " 
                AT 2,1 ATTRIBUTE(REVERSE)
   INPUT BY NAME x_buscar
         AFTER FIELD x_buscar
       IF x_buscar IS NULL THEN
          ERROR "Descripcion a Buscar NO puede ser nulo"
          NEXT FIELD x_buscar
       ELSE
          EXIT INPUT
       END IF
   END INPUT

   WHILE TRUE
         LET x_x = " SELECT local_cod,local_desc FROM safre_af:tablocal ",
                   " WHERE local_desc MATCHES ",'"',x_buscar CLIPPED,'"',
                   " ORDER BY 2 " CLIPPED
         PREPARE curg1 FROM x_x
         DECLARE cur_g1 CURSOR FOR curg1
            LET pos = 1
         FOREACH cur_g1 INTO l_reg[pos].*
            LET pos = pos + 1
            IF pos >= 1000 THEN
                ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
                EXIT FOREACH
            END IF
         END FOREACH
         IF (pos-1) < 1 THEN
            ERROR "ARCHIVO LOCALIDADES... VACIO"
         END IF
         CALL SET_COUNT(pos-1)
         DISPLAY ARRAY l_reg TO scr_1.*
                 ON KEY ( INTERRUPT )
            LET pos = 0
            EXIT DISPLAY

         ON KEY ( CONTROL-M )
            LET pos = ARR_CURR()
            EXIT DISPLAY

         END DISPLAY
         IF pos <> 0 THEN
             EXIT WHILE
         END IF
   END WHILE
   CLOSE WINDOW vent_1
   RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Despliega_estados()
   DEFINE aux_val          SMALLINT,
          x_x              CHAR(100),
          x_buscar         CHAR(30),

          l_reg ARRAY[1000] OF RECORD
              codigo       INTEGER,
              descripcion  CHAR(50)
          END RECORD,

          pos              SMALLINT

   OPEN WINDOW vent_1 AT 05,12 WITH FORM "PRO_GLOBP1" ATTRIBUTE(BORDER)
   DISPLAY "                ENTIDADES   FEDERATIVAS               " 
           AT 2,1 ATTRIBUTE(REVERSE)

   --INPUT BY NAME x_buscar
   --      AFTER FIELD x_buscar
   --          IF x_buscar IS NULL THEN
   --             ERROR "Descripcion a Buscar NO puede ser nulo"
   --             NEXT FIELD x_buscar
   --          ELSE
   --             EXIT INPUT
   --          END IF
   --END INPUT
      
   WHILE TRUE
         LET x_x = " SELECT estad_cod,estad_desc FROM safre_af:tab_estado ",
   --                " WHERE estad_desc MATCHES ",'"',x_buscar CLIPPED,'"',
                   " ORDER BY 1 ASC " CLIPPED

         PREPARE curg2 FROM x_x
         DECLARE cur_g2 CURSOR FOR curg2
            LET pos = 1
         FOREACH cur_g2 INTO l_reg[pos].*
            LET pos = pos + 1
            IF pos >= 1000 THEN
                ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
                EXIT FOREACH
            END IF
         END FOREACH

         IF (pos-1) < 1 THEN
            ERROR "ARCHIVO ESTADOS..... VACIO"
         END IF

         CALL SET_COUNT(pos-1)
         DISPLAY ARRAY l_reg TO scr_1.*
                 ON KEY ( INTERRUPT )
                    LET pos = 0
                    EXIT DISPLAY

                 ON KEY ( CONTROL-M )
                    LET pos = ARR_CURR()
                    EXIT DISPLAY
         END DISPLAY

         IF pos <> 0 THEN
             EXIT WHILE
         END IF
   END WHILE
   CLOSE WINDOW vent_1
   RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION

################################################################################
FUNCTION Despliega_sexos()
   DEFINE aux_val      SMALLINT,

          l_reg ARRAY[1000] OF RECORD
              codigo      INTEGER,
              descripcion CHAR(50)
          END RECORD,

          x_x        CHAR(100),
          x_buscar   CHAR(30),
          pos        SMALLINT

   OPEN WINDOW vent_1 AT 05,12 WITH FORM "PRO_GLOBP1" ATTRIBUTE(BORDER)
   DISPLAY "                      S  E  X  O  S                      " 
                   AT 2,1 ATTRIBUTE(REVERSE)
   INPUT BY NAME x_buscar
         AFTER FIELD x_buscar
           IF x_buscar IS NULL THEN
              ERROR "Descripcion a Buscar NO puede ser nulo"
              NEXT FIELD x_buscar
           ELSE
              EXIT INPUT
           END IF
   END INPUT

   WHILE TRUE
         LET x_x = " SELECT sexo_cod,sexo_desc FROM safre_af:tabsexo ",
                   " WHERE sexo_desc MATCHES ",'"',x_buscar CLIPPED,'"',
                   " ORDER BY 2 " CLIPPED
         PREPARE curg3 FROM x_x
         DECLARE cur_g3 CURSOR FOR curg3
         LET pos = 1
         FOREACH cur_g3 INTO l_reg[pos].*
            LET pos = pos + 1
            IF pos >= 1000 THEN
                ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
                EXIT FOREACH
            END IF
         END FOREACH

         IF (pos-1) < 1 THEN
            ERROR "ARCHIVO SEXO..... VACIO"
         END IF

         CALL SET_COUNT(pos-1)
         DISPLAY ARRAY l_reg TO scr_1.*
                 ON KEY ( INTERRUPT )
                    LET pos = 0
                    EXIT DISPLAY
                 ON KEY ( CONTROL-M )
                    LET pos = ARR_CURR()
                    EXIT DISPLAY
         END DISPLAY

         IF pos <> 0 THEN
             EXIT WHILE
         END IF
   END WHILE
   CLOSE WINDOW vent_1
   RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Despliega_tipos_trabajador()
   DEFINE aux_val    SMALLINT,

          l_reg ARRAY[1000] OF RECORD
             codigo      INTEGER,
             descripcion CHAR(50)
          END RECORD,

          x_x        CHAR(100),
          x_buscar   CHAR(30),
          pos        SMALLINT

   OPEN WINDOW vent_1 AT 05,12 WITH FORM "PRO_GLOBP1" ATTRIBUTE(BORDER)
   DISPLAY "                 TIPOS   DE    TRABAJADOR                " 
                   AT 2,1 ATTRIBUTE(REVERSE)

   INPUT BY NAME x_buscar
         AFTER FIELD x_buscar
            IF x_buscar IS NULL THEN
               ERROR "Descripcion a Buscar NO puede ser nulo"
               NEXT FIELD x_buscar
            ELSE
               EXIT INPUT
            END IF
   END INPUT

   WHILE TRUE
         LET x_x = " SELECT tiptr_cod,tiptr_desc FROM safre_af:tabtiptr ",
                   " WHERE tiptr_desc MATCHES ",'"',x_buscar CLIPPED,'"',
                   " ORDER BY 1 " CLIPPED

         PREPARE curg4 FROM x_x
         DECLARE cur_g4 CURSOR FOR curg4
            LET pos = 1
         FOREACH cur_g4 INTO l_reg[pos].*
            LET pos = pos + 1
            IF pos >= 1000 THEN
                ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
                EXIT FOREACH
            END IF
         END FOREACH

         IF (pos-1) < 1 THEN
            ERROR "ARCHIVO TIPO DE TRABAJADOR..... VACIO"
         END IF

         CALL SET_COUNT(pos-1)
         DISPLAY ARRAY l_reg TO scr_1.*
                 ON KEY ( INTERRUPT )
                    LET pos = 0
                    EXIT DISPLAY
                 ON KEY ( CONTROL-M )
                    LET pos = ARR_CURR()
                    EXIT DISPLAY
          END DISPLAY

          IF pos <> 0 THEN
             EXIT WHILE
          END IF
   END WHILE
   CLOSE WINDOW vent_1
   RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
############################################################################
FUNCTION Despliega_delegaciones()
   DEFINE aux_val        SMALLINT,

          l_reg ARRAY[1000] OF RECORD
             codigo        INTEGER,
             descripcion   CHAR(50)
          END RECORD,

          x_x           CHAR(100),
          x_buscar      CHAR(30),
          pos           SMALLINT

   OPEN WINDOW vent_1 AT 05,12 WITH FORM "PRO_GLOBP1" ATTRIBUTE(BORDER)
   DISPLAY "                 D E L E G A C I O N E S                  " 
                   AT 2,1 ATTRIBUTE(REVERSE)
   INPUT BY NAME x_buscar
         AFTER FIELD x_buscar
            IF x_buscar IS NULL THEN
               ERROR "Descripcion a Buscar NO puede ser nulo"
               NEXT FIELD x_buscar
            ELSE
               EXIT INPUT
            END IF
   END INPUT

   WHILE TRUE
         LET x_x = " SELECT deleg_cod,deleg_desc FROM safre_af:tab_delegacion ",
                   " WHERE deleg_desc MATCHES ",'"',x_buscar CLIPPED,'"',
                   " ORDER BY 2 " CLIPPED

         PREPARE curg5 FROM x_x
         DECLARE cur_g5 CURSOR FOR curg5
            LET pos = 1
         FOREACH cur_g5 INTO l_reg[pos].*
            LET pos = pos + 1
            IF pos >= 1000 THEN
                ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
                EXIT FOREACH
            END IF
         END FOREACH

         IF (pos-1) < 1 THEN
            ERROR "ARCHIVO DELEGACIONES..... VACIO"
         END IF

         CALL SET_COUNT(pos-1)
         DISPLAY ARRAY l_reg TO scr_1.*
                 ON KEY ( INTERRUPT )
                    LET pos = 0
                    EXIT DISPLAY
                 ON KEY ( CONTROL-M )
                    LET pos = ARR_CURR()
                    EXIT DISPLAY
         END DISPLAY

         IF pos <> 0 THEN
             EXIT WHILE
         END IF
   END WHILE
   CLOSE WINDOW vent_1
   RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Despliega_ciudades()
   DEFINE aux_val     SMALLINT,

          l_reg ARRAY[1000] OF RECORD
              codigo       INTEGER,
              descripcion  CHAR(50)
          END RECORD,

          x_x        char(100),
          x_buscar   char(30),
          pos        SMALLINT

   OPEN WINDOW vent_1 AT 05,12 WITH FORM "PRO_GLOBP1" ATTRIBUTE(BORDER)
   DISPLAY "                     C I U D A D E S                      " 
           AT 2,1 ATTRIBUTE(REVERSE)

   INPUT BY NAME x_buscar
         AFTER FIELD x_buscar
            IF x_buscar IS NULL THEN
               ERROR "Descripcion a Buscar NO puede ser nulo"
               NEXT FIELD x_buscar
            ELSE
               EXIT INPUT
            END IF
   END INPUT

   WHILE TRUE
         LET x_x = " SELECT ciudad_cod,ciudad_desc FROM safre_af:tab_ciudad ",
                   " WHERE ciudad_desc MATCHES ",'"',x_buscar CLIPPED,'"',
                   " ORDER BY 2 " CLIPPED

         PREPARE curg6 FROM x_x
         DECLARE cur_g6 CURSOR FOR curg6
            LET pos = 1
         FOREACH cur_g6 INTO l_reg[pos].*
            LET pos = pos + 1
            IF pos >= 1000 THEN
               ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
               EXIT FOREACH
            END IF
         END FOREACH

         IF (pos-1) < 1 THEN
            ERROR "ARCHIVO CUIDADES..... VACIO"
         END IF

         CALL SET_COUNT(pos-1)
         DISPLAY ARRAY l_reg TO scr_1.*
                 ON KEY ( INTERRUPT )
                    LET pos = 0
                    EXIT DISPLAY

                 ON KEY ( CONTROL-M )
                    LET pos = ARR_CURR()
                    EXIT DISPLAY
         END DISPLAY

         IF pos <> 0 THEN
            EXIT WHILE
         END IF
   END WHILE
   CLOSE WINDOW vent_1
   RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Despliega_parentescos()
   DEFINE aux_val   SMALLINT,
   
          l_reg ARRAY[1000] OF RECORD
             codigo  INTEGER,
             descripcion CHAR(50)
          END RECORD,

          x_x         char(100),
          x_buscar    char(30),
          pos         SMALLINT

   OPEN WINDOW vent_1 AT 05,12 WITH FORM "PRO_GLOBP1" ATTRIBUTE(BORDER)
   DISPLAY "                 P A R E N T E S C O S                   " 
                   AT 2,1 ATTRIBUTE(REVERSE)
   INPUT BY NAME x_buscar
         AFTER FIELD x_buscar
            IF x_buscar IS NULL THEN
               ERROR "Descripcion a Buscar NO puede ser nulo"
               NEXT FIELD x_buscar
            ELSE
               EXIT INPUT
            END IF
   END INPUT

   WHILE TRUE
         LET x_x = " SELECT paren_cod,paren_desc FROM safre_af:tabparen ",
                   " WHERE paren_desc MATCHES ",'"',x_buscar CLIPPED,'"',
                   " ORDER BY 2 " CLIPPED

         PREPARE curg7 FROM x_x
         DECLARE cur_g7 CURSOR FOR curg7
            LET pos = 1
         FOREACH cur_g7 INTO l_reg[pos].*
            LET pos = pos + 1
            IF pos >= 1000 THEN
               ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
               EXIT FOREACH
            END IF
         END FOREACH

         IF (pos-1) < 1 THEN
            ERROR "ARCHIVO PARENTESCO..... VACIO"
         END IF

         CALL SET_COUNT(pos-1)
         DISPLAY ARRAY l_reg TO scr_1.*
                 ON KEY ( INTERRUPT )
                    LET pos = 0
                    EXIT DISPLAY

                 ON KEY ( CONTROL-M )
                    LET pos = ARR_CURR()
                    EXIT DISPLAY
         END DISPLAY

         IF pos <> 0 THEN
            EXIT WHILE
         END IF
   END WHILE
   CLOSE WINDOW vent_1
   RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Despliega_promotores()
DEFINE aux_pausa     SMALLINT,
       cod           DECIMAL(10,0),
       pat,mat,nom   CHAR(60),

       l_reg ARRAY[2000] OF RECORD
           codigo          CHAR(10),
           descripcion     CHAR(50)
       END RECORD,

       pos           SMALLINT,
       x_buscar      CHAR(60),
       x_texto       CHAR(100)

OPEN WINDOW vent_1 AT 07,12 WITH FORM "PRO_GLOBP2" ATTRIBUTE(BORDER)
DISPLAY "                 P R O M O T O R E S                     " 
                AT 2,1 ATTRIBUTE(REVERSE)
INPUT BY NAME x_buscar
      AFTER FIELD x_buscar
         IF x_buscar IS NULL THEN
            ERROR "Descripcion a Buscar NO puede ser nulo"
            NEXT FIELD x_buscar
         ELSE
            EXIT INPUT
         END IF
END INPUT

WHILE TRUE
      LET x_texto = " SELECT codven,paterno,materno,nombres ",
                    " FROM pro_mae_promotor WHERE paterno MATCHES ",
                    '"',x_buscar CLIPPED,
                    '"',
                    " ORDER BY 2 " CLIPPED

      WHENEVER ERROR CONTINUE
      ERROR "Buscando Informacion"

      PREPARE curg8 FROM x_texto
      DECLARE cur_g8 CURSOR FOR curg8
         LET pos = 1
      FOREACH cur_g8 INTO cod,pat,mat,nom
         LET l_reg[pos].codigo = cod
         LET l_reg[pos].descripcion = pat CLIPPED," ",
                                      mat CLIPPED," ",
                                      nom CLIPPED
         LET pos = pos + 1
         IF pos > 2000 THEN
             ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
             EXIT FOREACH
         END IF
      END FOREACH
      FREE curg8

      IF (pos-1) < 1 THEN
         ERROR "ARCHIVO PROMOTORES..... VACIO"
      END IF

      CALL SET_COUNT(pos-1)
      DISPLAY ARRAY l_reg TO scr_1.*
          ON KEY ( INTERRUPT )
             LET pos = 0
             EXIT DISPLAY
          ON KEY ( CONTROL-M )
             LET pos = ARR_CURR()
             EXIT DISPLAY
      END DISPLAY

      IF pos <> 0 THEN
         EXIT WHILE
      END IF
END WHILE
CLOSE WINDOW vent_1

WHENEVER ERROR STOP
RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Despliega_safre_af()
DEFINE aux_val           SMALLINT,

       l_reg ARRAY[1000] OF RECORD
           codigo        INTEGER,
           descripcion   CHAR(50)
       END RECORD,

       x_x               char(100),
       x_buscar          char(30),
       pos               SMALLINT

OPEN WINDOW vent_1 AT 05,12 WITH FORM "PRO_GLOBP1" ATTRIBUTE(BORDER)
DISPLAY "                   A F O R E S                           " 
                AT 2,1 ATTRIBUTE(REVERSE)
INPUT BY NAME x_buscar
      AFTER FIELD x_buscar
         IF x_buscar IS NULL THEN
            ERROR "Descripcion a Buscar NO puede ser nulo"
            NEXT FIELD x_buscar
         ELSE
            EXIT INPUT
         END IF
END INPUT

WHILE TRUE
      LET x_x = " SELECT codigo_afore,razon_social FROM safre_af:tabafore ",
                " WHERE razon_social MATCHES ",'"',x_buscar CLIPPED,'"',
                " ORDER BY 2 " CLIPPED

      PREPARE curg9 FROM x_x
      DECLARE cur_g9 CURSOR FOR curg9
         LET pos = 1
      FOREACH cur_g9 INTO l_reg[pos].*
         LET pos = pos + 1
         IF pos >= 1000 THEN
             ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
             EXIT FOREACH
         END IF
      END FOREACH
      IF (pos-1) < 1 THEN
         ERROR "ARCHIVO AFORE..... VACIO"
      END IF
      CALL SET_COUNT(pos-1)
      DISPLAY ARRAY l_reg TO scr_1.*
              ON KEY ( INTERRUPT )
         LET pos = 0
         EXIT DISPLAY
      ON KEY ( CONTROL-M )
         LET pos = ARR_CURR()
         EXIT DISPLAY
      END DISPLAY
      IF pos <> 0 THEN
 EXIT WHILE
      END IF
END WHILE
CLOSE WINDOW vent_1
RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Despliega_siefores()
DEFINE aux_val   SMALLINT
DEFINE l_reg ARRAY[1000] OF RECORD
       codigo   INTEGER,
       descripcion   CHAR(50)
END RECORD
DEFINE x_x  char(100),
       x_buscar  char(30)
DEFINE pos  SMALLINT
OPEN WINDOW vent_1 AT 05,12 WITH FORM "PRO_GLOBP1" ATTRIBUTE(BORDER)
DISPLAY "                  S  I  E  A F  O  R  E  S              " 
                AT 2,1 ATTRIBUTE(REVERSE)
INPUT BY NAME x_buscar
      AFTER FIELD x_buscar
    IF x_buscar IS NULL THEN
       ERROR "Descripcion a Buscar NO puede ser nulo"
       NEXT FIELD x_buscar
    ELSE
       EXIT INPUT
    END IF
END INPUT
WHILE TRUE
      LET x_x = " SELECT codigo_siefore,razon_social FROM safre_af:tabsiafo ",
                " WHERE razon_social MATCHES ",'"',x_buscar CLIPPED,'"',
                " ORDER BY 2 " CLIPPED
      PREPARE curg10 FROM x_x
      DECLARE cur_g10 CURSOR FOR curg10
         LET pos = 1
      FOREACH cur_g10 INTO l_reg[pos].*
         LET pos = pos + 1
         IF pos >= 1000 THEN
             ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
             EXIT FOREACH
         END IF
      END FOREACH
      IF (pos-1) < 1 THEN
         ERROR "ARCHIVO SIEAFORE..... VACIO"
      END IF
      CALL SET_COUNT(pos-1)
      DISPLAY ARRAY l_reg TO scr_1.*
              ON KEY ( INTERRUPT )
                  LET pos = 0
                  EXIT DISPLAY
               ON KEY ( CONTROL-M )
                  LET pos = ARR_CURR()
                  EXIT DISPLAY
               END DISPLAY
      IF pos <> 0 THEN
 EXIT WHILE
      END IF
END WHILE
CLOSE WINDOW vent_1
RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Despliega_subcuentas()
DEFINE aux_val          SMALLINT,
       aux_pausa        SMALLINT,
       cod              INTEGER,
       pat,mat,nom      CHAR(60),
       x_x              char(100),
       x_buscar         char(30),

       l_reg ARRAY[1000] OF RECORD
           codigo       INTEGER,
           descripcion  CHAR(50)
       END RECORD,
       pos       SMALLINT

OPEN WINDOW vent_1 AT 05,12 WITH FORM "PRO_GLOBP1" ATTRIBUTE(BORDER)
DISPLAY "                    S U B C U E N T A S                    " 
        AT 2,1 ATTRIBUTE(REVERSE)
    INPUT BY NAME x_buscar
          AFTER FIELD x_buscar
        IF x_buscar IS NULL THEN
           ERROR "Descripcion a Buscar NO puede ser nulo"
           NEXT FIELD x_buscar
        ELSE
           EXIT INPUT
        END IF
    END INPUT

    WHILE TRUE
          LET x_x = " SELECT subct_cod,subct_desc FROM safre_af:tabsubct ",
                    " WHERE subct_desc MATCHES ",'"',x_buscar CLIPPED,'"',
                    " ORDER BY 2 " CLIPPED

          PREPARE curg11 FROM x_x
          DECLARE cur_g11 CURSOR FOR curg11
             LET pos = 1
          FOREACH cur_g11 INTO l_reg[pos].*
             LET pos = pos + 1
             IF pos >= 1000 THEN
                ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
                EXIT FOREACH
             END IF
          END FOREACH

          IF (pos-1) < 1 THEN
             ERROR "ARCHIVO SUBCUENTAS..... VACIO"
          END IF
          CALL SET_COUNT(pos-1)

          DISPLAY ARRAY l_reg TO scr_1.*
                  ON KEY ( INTERRUPT )
                     LET pos = 0
                     EXIT DISPLAY
                  ON KEY ( CONTROL-M )
                     LET pos = ARR_CURR()
                     EXIT DISPLAY
          END DISPLAY

          IF pos <> 0 THEN
             EXIT WHILE
          END IF
    END WHILE

    CLOSE WINDOW vent_1
    RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Despliega_agencias()
DEFINE aux_val            SMALLINT,
       aux_pausa          SMALLINT,
       cod                INTEGER,
       pat,mat,nom        CHAR(60),

       l_reg ARRAY[1000] OF RECORD
           codigo         char(10),
           descripcion    CHAR(50)
       END RECORD,

       pos                SMALLINT,
       x_x                char(100),
       x_buscar           char(30)

   OPEN WINDOW vent_1 AT 05,12 WITH FORM "PRO_GLOBP5" ATTRIBUTE(BORDER)
   DISPLAY "                      A G E N C I A ",
           "S                           " AT 2,1 ATTRIBUTE(REVERSE)
   INPUT BY NAME x_buscar
         AFTER FIELD x_buscar
             IF x_buscar IS NULL THEN
                ERROR "Descripcion a Buscar NO puede ser nulo"
                NEXT FIELD x_buscar
             ELSE
                EXIT INPUT
             END IF
   END INPUT

   WHILE TRUE
      LET x_x = " SELECT coduni_n1,nombre_uni_n1 FROM com_nivel1 ",
                " WHERE coduni_n1 MATCHES ",'"',x_buscar CLIPPED,'"',
                " ORDER BY 1,2 " CLIPPED

      PREPARE curg12 FROM x_x
      DECLARE cur_g12 CURSOR FOR curg12
         LET pos = 1
      FOREACH cur_g12 INTO l_reg[pos].*
         LET pos = pos + 1
         IF pos >= 1000 THEN
             ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
             EXIT FOREACH
         END IF
      END FOREACH

      IF (pos-1) < 1 THEN
         ERROR "ARCHIVO AGENCIAS..... VACIO"
      END IF

      CALL SET_COUNT(pos-1)
      DISPLAY ARRAY l_reg TO scr_1.*
              ON KEY ( INTERRUPT )
                  LET pos = 0
                  EXIT DISPLAY

              ON KEY ( CONTROL-M )
                  LET pos = ARR_CURR()
                  EXIT DISPLAY
      END DISPLAY

      IF pos <> 0 AND pos >= 1 THEN
          EXIT WHILE
      END IF
   END WHILE

   CLOSE WINDOW vent_1
   RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Despliega_supervisores()
DEFINE aux_val          SMALLINT
DEFINE aux_pausa        SMALLINT
DEFINE cod              INTEGER
DEFINE pat,mat,nom      CHAR(60)

DEFINE l_reg ARRAY[1000] OF RECORD
          codigo       INTEGER,
          descripcion  CHAR(50)
       END RECORD

DEFINE pos              SMALLINT
DEFINE x_x              char(100),
       x_buscar         char(30)

OPEN WINDOW vent_1 AT 05,12 WITH FORM "PRO_GLOBP1" ATTRIBUTE(BORDER)
           DISPLAY "                   S U P E R V I S O R E S                "
                   AT 2,1 ATTRIBUTE(REVERSE)
INPUT BY NAME x_buscar
      AFTER FIELD x_buscar
        IF x_buscar IS NULL THEN
           ERROR "Descripcion a Buscar NO puede ser nulo"
           NEXT FIELD x_buscar
        ELSE
           EXIT INPUT
        END IF
END INPUT

WHILE TRUE
      LET x_x = " SELECT super_cod,super_desc FROM safre_af:tabsuper ",
                " WHERE super_desc MATCHES ",'"',x_buscar CLIPPED,'"',
                " ORDER BY 2 " CLIPPED

      PREPARE curg13 FROM x_x
      DECLARE cur_g13 CURSOR FOR curg13
         LET pos = 1
      FOREACH cur_g13 INTO l_reg[pos].*
         LET pos = pos + 1
         IF pos >= 1000 THEN
            ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
            EXIT FOREACH
         END IF
      END FOREACH

      IF (pos-1) < 1 THEN
         ERROR "ARCHIVO SUPERVISOR..... VACIO"
      END IF

      CALL SET_COUNT(pos-1)
      DISPLAY ARRAY l_reg TO scr_1.*
              ON KEY ( INTERRUPT )
                 LET pos = 0
                 EXIT DISPLAY
              ON KEY ( CONTROL-M )
                 LET pos = ARR_CURR()
                 EXIT DISPLAY
      END DISPLAY

      IF pos <> 0 THEN
         EXIT WHILE
      END IF
END WHILE
CLOSE WINDOW vent_1
RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
###############################################################
FUNCTION Despliega_niveles()
DEFINE aux_val           SMALLINT
DEFINE aux_pausa         SMALLINT
DEFINE cod               INTEGER
DEFINE pat,mat,nom       CHAR(60)

DEFINE l_reg ARRAY[1000] OF RECORD
           codigo        INTEGER,
           descripcion   CHAR(50)
       END RECORD

DEFINE pos               SMALLINT

DEFINE x_x               char(100),
       x_buscar          char(30)

OPEN WINDOW vent_1 AT 05,12 WITH FORM "PRO_GLOBP1" ATTRIBUTE(BORDER)
DISPLAY "             N I V E L E S  (CARGOS)                 " 
        AT 2,1 ATTRIBUTE(REVERSE)

INPUT BY NAME x_buscar
      AFTER FIELD x_buscar
         IF x_buscar IS NULL THEN
            ERROR "Descripcion a Buscar NO puede ser nulo"
            NEXT FIELD x_buscar
         ELSE
            EXIT INPUT
         END IF
END INPUT

WHILE TRUE
      LET x_x = " SELECT nivel_cod,nivel_desc FROM safre_af:tabnivel ",
                " WHERE nivel_desc MATCHES ",'"',x_buscar CLIPPED,'"',
                " ORDER BY 2 " CLIPPED

      PREPARE curg14 FROM x_x
      DECLARE cur_g14 CURSOR FOR curg14
          LET pos = 1
      FOREACH cur_g14 INTO l_reg[pos].*
          LET pos = pos + 1

          IF pos >= 1000 THEN
              ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
              EXIT FOREACH
          END IF

      END FOREACH

      IF (pos-1) < 1 THEN
         ERROR "ARCHIVO NIVELES(CARGOS)..... VACIO"
      END IF

      CALL SET_COUNT(pos-1)
      DISPLAY ARRAY l_reg TO scr_1.*
              ON KEY ( INTERRUPT )
                 LET pos = 0
                 EXIT DISPLAY
              ON KEY ( CONTROL-M )
                 LET pos = ARR_CURR()
                 EXIT DISPLAY
      END DISPLAY

      IF pos <> 0 THEN
         EXIT WHILE
      END IF
END WHILE
CLOSE WINDOW vent_1
RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Despliega_patrones()
DEFINE aux_pausa         SMALLINT
DEFINE aux_val           SMALLINT

DEFINE x_x               char(100),
       x_buscar          char(30)

DEFINE l_reg ARRAY[1000] OF RECORD
          codigo         char(11),
          descripcion    CHAR(50)
       END RECORD

DEFINE pos                     SMALLINT

OPEN WINDOW vent_1 AT 09,2 WITH FORM "PRO_GLOBP" ATTRIBUTE(BORDER)
DISPLAY "                    P A T R O N E S                      " 
        AT 2,1 ATTRIBUTE(REVERSE)

INPUT BY NAME x_buscar
      AFTER FIELD x_buscar
         IF x_buscar IS NULL THEN
            ERROR "Descripcion a Buscar NO puede ser nulo"
            NEXT FIELD x_buscar
         ELSE
            EXIT INPUT
         END IF
END INPUT
WHILE TRUE
      LET x_x = " SELECT reg_patronal,razon_social FROM tab_patron ",
                " WHERE razon_social MATCHES ",'"',x_buscar CLIPPED,'"',
                " ORDER BY 2 " CLIPPED

      PREPARE curg15 FROM x_x
      DECLARE cur_g15 CURSOR FOR curg15
         LET pos = 1
      FOREACH cur_g15 INTO l_reg[pos].*
         LET pos = pos + 1
         IF pos >= 1000 THEN
             ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
             EXIT FOREACH
         END IF
      END FOREACH

      IF (pos-1) < 1 THEN
         ERROR "ARCHIVO PATRONES... VACIO"
      END IF

      CALL SET_COUNT(pos-1)
      DISPLAY ARRAY l_reg TO scr_1.*
              ON KEY ( INTERRUPT )
                 LET pos = 0
                 EXIT DISPLAY
              ON KEY ( CONTROL-M )
                 LET pos = ARR_CURR()
                 EXIT DISPLAY
      END DISPLAY

      IF pos <> 0 THEN
          EXIT WHILE
      END IF
END WHILE
CLOSE WINDOW vent_1
RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Despliega_estados_civiles()
DEFINE aux_val           SMALLINT

DEFINE l_reg ARRAY[1000] OF RECORD
          codigo         INTEGER,
          descripcion    CHAR(50)
       END RECORD

DEFINE x_x               char(100),
       x_buscar          char(30)

DEFINE pos               SMALLINT

OPEN WINDOW vent_1 AT 05,12 WITH FORM "PRO_GLOBP1" ATTRIBUTE(BORDER)
DISPLAY "               E S T A D O S   C I V I L E S             " 
                AT 2,1 ATTRIBUTE(REVERSE)
INPUT BY NAME x_buscar
      AFTER FIELD x_buscar
         IF x_buscar IS NULL THEN
            ERROR "Descripcion a Buscar NO puede ser nulo"
            NEXT FIELD x_buscar
         ELSE
            EXIT INPUT
         END IF
END INPUT

WHILE TRUE
      LET x_x = " SELECT * FROM safre_af:tabecivi ",
                " WHERE ecivi_desc MATCHES ",'"',x_buscar CLIPPED,'"',
                " ORDER BY 2 " CLIPPED

      PREPARE curg16 FROM x_x
      DECLARE cur_g16 CURSOR FOR curg16
         LET pos = 1
      FOREACH cur_g16 INTO l_reg[pos].*
         LET pos = pos + 1
         IF pos >= 1000 THEN
            ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
            EXIT FOREACH
         END IF
      END FOREACH

      IF (pos-1) < 1 THEN
         ERROR "ARCHIVO ESTADOS CIVILES ... VACIO"
      END IF

      CALL SET_COUNT(pos-1)
      DISPLAY ARRAY l_reg TO scr_1.*
              ON KEY ( INTERRUPT )
         LET pos = 0
         EXIT DISPLAY
      ON KEY ( CONTROL-M )
         LET pos = ARR_CURR()
         EXIT DISPLAY
      END DISPLAY

      IF pos <> 0 THEN
         EXIT WHILE
      END IF
END WHILE
CLOSE WINDOW vent_1
RETURN l_reg[pos].codigo,l_reg[pos].descripcion
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
return true
end function
################################################################################
FUNCTION Despliega_pais()
DEFINE aux_val           SMALLINT
DEFINE l_reg ARRAY[1000] OF RECORD
          codigo         char(03),
          descripcion    CHAR(50)
       END RECORD
DEFINE x_x               char(100),
       x_buscar          char(30)

DEFINE pos               SMALLINT

OPEN WINDOW vent_1 AT 05,12 WITH FORM "PRO_GLOBP1" ATTRIBUTE(BORDER)
DISPLAY "                    P A I S E S                          " 
                AT 2,1 ATTRIBUTE(REVERSE)
INPUT BY NAME x_buscar
      AFTER FIELD x_buscar
         IF x_buscar IS NULL THEN
            ERROR "Descripcion a Buscar NO puede ser nulo"
            NEXT FIELD x_buscar
         ELSE
            EXIT INPUT
         END IF
END INPUT

WHILE TRUE
      LET x_x = " SELECT * FROM safre_af:tabpais ",
                " WHERE pais_desc MATCHES ",'"',x_buscar CLIPPED,'"',
                " ORDER BY 2 " CLIPPED

      PREPARE curg17 FROM x_x
      DECLARE cur_g17 CURSOR FOR curg17
         LET pos = 1
      FOREACH cur_g17 INTO l_reg[pos].*
         LET pos = pos + 1
         IF pos >= 1000 THEN
            ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
            EXIT FOREACH
         END IF
      END FOREACH

      IF (pos-1) < 1 THEN
         ERROR "ARCHIVO PAISES ... VACIO"
      END IF

      CALL SET_COUNT(pos-1)
      DISPLAY ARRAY l_reg TO scr_1.*
              ON KEY ( INTERRUPT )
                 LET pos = 0
                 EXIT DISPLAY
              ON KEY ( CONTROL-M )
                 LET pos = ARR_CURR()
                 EXIT DISPLAY
      END DISPLAY

      IF pos <> 0 THEN
          EXIT WHILE
      END IF
END WHILE
CLOSE WINDOW vent_1
RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Despliega_documento_probatorio()
DEFINE aux_val           SMALLINT

DEFINE l_reg ARRAY[1000] OF RECORD
          codigo         INTEGER,
          descripcion    CHAR(50)
       END RECORD
DEFINE x_x               char(100),
       x_buscar          char(30)

DEFINE pos               SMALLINT

OPEN WINDOW vent_1 AT 05,12 WITH FORM "PRO_GLOBP1" ATTRIBUTE(BORDER)
DISPLAY "            TIPOS DE DOCUMENTOS PROBATORIOS              " 
                AT 2,1 ATTRIBUTE(REVERSE)
INPUT BY NAME x_buscar
      AFTER FIELD x_buscar
          IF x_buscar IS NULL THEN
             ERROR "Descripcion a Buscar NO puede ser nulo"
             NEXT FIELD x_buscar
          ELSE
             EXIT INPUT
          END IF
END INPUT

WHILE TRUE
      LET x_x = " SELECT * FROM safre_af:tabdocprob ",
                " WHERE docprob_desc MATCHES ",'"',x_buscar CLIPPED,'"',
                " ORDER BY 2 " CLIPPED

      PREPARE curg18 FROM x_x
      DECLARE cur_g18 CURSOR FOR curg18
         LET pos = 1
      FOREACH cur_g18 INTO l_reg[pos].*
         LET pos = pos + 1
         IF pos >= 1000 THEN
            ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
            EXIT FOREACH
         END IF
      END FOREACH
      IF (pos-1) < 1 THEN
         ERROR "ARCHIVO DOCUMENTO PROBATORIO... VACIO"
      END IF

      CALL SET_COUNT(pos-1)
      DISPLAY ARRAY l_reg TO scr_1.*
              ON KEY ( INTERRUPT )
                 LET pos = 0
                 EXIT DISPLAY
              ON KEY ( CONTROL-M )
                 LET pos = ARR_CURR()
                 EXIT DISPLAY
      END DISPLAY

      IF pos <> 0 THEN
         EXIT WHILE
      END IF
END WHILE
CLOSE WINDOW vent_1
RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Despliega_tipo_promotor()
DEFINE aux_val       SMALLINT

DEFINE l_reg ARRAY[1000] OF RECORD
          codigo        INTEGER,
          descripcion   CHAR(50)
       END RECORD

DEFINE x_x           char(100),
       x_buscar      char(30)

DEFINE pos           SMALLINT

OPEN WINDOW vent_1 AT 05,12 WITH FORM "PRO_GLOBP1" ATTRIBUTE(BORDER)
DISPLAY "                 TIPOS DE PROMOTOR                       " 
                AT 2,1 ATTRIBUTE(REVERSE)
INPUT BY NAME x_buscar
      AFTER FIELD x_buscar
    IF x_buscar IS NULL THEN
       ERROR "Descripcion a Buscar NO puede ser nulo"
       NEXT FIELD x_buscar
    ELSE
       EXIT INPUT
    END IF
END INPUT

WHILE TRUE
      LET x_x = " SELECT cod_tipo_prom,desc_tipo FROM com_tipo_promotor ",
                " WHERE desc_tipo MATCHES ",'"',x_buscar CLIPPED,'"',
                " ORDER BY 1 " CLIPPED

      PREPARE curg119 FROM x_x
      DECLARE cur_g119 CURSOR FOR curg119
         LET pos = 1
      FOREACH cur_g119 INTO l_reg[pos].*
          LET pos = pos + 1
          IF pos >= 1000 THEN
             ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
             EXIT FOREACH
          END IF
      END FOREACH

      IF (pos-1) < 1 THEN
         ERROR "EN ARCHIVO TIPOS DE PROMOTOR NO EXISTE EL PATRON BUSQUEDA"
      END IF

      CALL SET_COUNT(pos-1)
      DISPLAY ARRAY l_reg TO scr_1.*
              ON KEY ( INTERRUPT )
                 LET pos = 0
                 EXIT DISPLAY
              ON KEY ( CONTROL-M )
                 LET pos = ARR_CURR()
                 EXIT DISPLAY
      END DISPLAY

      IF pos <> 0 THEN
         EXIT WHILE
      END IF
END WHILE
CLOSE WINDOW vent_1
RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Despliega_icefas()
DEFINE aux_val           SMALLINT

DEFINE l_reg ARRAY[1000] OF RECORD
          codigo         INTEGER,
          descripcion    CHAR(50)
       END RECORD

DEFINE x_x               char(100),
       x_buscar          char(30)

DEFINE pos               SMALLINT

OPEN WINDOW vent_1 AT 05,12 WITH FORM "PRO_GLOBP1" ATTRIBUTE(BORDER)
DISPLAY "            I C E F A S                                  " 
                AT 2,1 ATTRIBUTE(REVERSE)
INPUT BY NAME x_buscar
      AFTER FIELD x_buscar
         IF x_buscar IS NULL THEN
            ERROR "Descripcion a Buscar NO puede ser nulo"
            NEXT FIELD x_buscar
         ELSE
            EXIT INPUT
         END IF
END INPUT

WHILE TRUE
      LET x_x = " SELECT * FROM safre_af:tabicefa ",
                " WHERE icefa_desc MATCHES ",'"',x_buscar CLIPPED,'"',
                " ORDER BY 1 " CLIPPED

      PREPARE curg19 FROM x_x
      DECLARE cur_g19 CURSOR FOR curg19
          LET pos = 1
      FOREACH cur_g19 INTO l_reg[pos].*
          LET pos = pos + 1
          IF pos >= 1000 THEN
             ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
             EXIT FOREACH
          END IF
      END FOREACH

      IF (pos-1) < 1 THEN
         ERROR "ARCHIVO ICEFAS... VACIO"
      END IF

      CALL SET_COUNT(pos-1)
      DISPLAY ARRAY l_reg TO scr_1.*
              ON KEY ( INTERRUPT )
                 LET pos = 0
                 EXIT DISPLAY
              ON KEY ( CONTROL-M )
                 LET pos = ARR_CURR()
                 EXIT DISPLAY
      END DISPLAY

      IF pos <> 0 THEN
         EXIT WHILE
      END IF
END WHILE
CLOSE WINDOW vent_1
RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Despliega_cancelacion()
DEFINE aux_val           SMALLINT
DEFINE l_reg ARRAY[1000] OF RECORD
          codigo         INTEGER,
          descripcion    CHAR(50)
        END RECORD
DEFINE x_x               char(100),
       x_buscar          char(30)
DEFINE pos               SMALLINT

OPEN WINDOW vent_1 AT 05,12 WITH FORM "PRO_GLOBP1" ATTRIBUTE(BORDER)
DISPLAY "            CODIGOS DE CANCELACION                       " 
                AT 2,1 ATTRIBUTE(REVERSE)
INPUT BY NAME x_buscar
      AFTER FIELD x_buscar
        IF x_buscar IS NULL THEN
           ERROR "Descripcion a Buscar NO puede ser nulo"
           NEXT FIELD x_buscar
        ELSE
           EXIT INPUT
        END IF
END INPUT

WHILE TRUE
      LET x_x = " SELECT * FROM safre_af:tabcancel ",
                " WHERE cancel_desc MATCHES ",'"',x_buscar CLIPPED,'"',
                " ORDER BY 2 " CLIPPED

      PREPARE curg20 FROM x_x
      DECLARE cur_g20 CURSOR FOR curg20
         LET pos = 1
      FOREACH cur_g20 INTO l_reg[pos].*
         LET pos = pos + 1
         IF pos >= 1000 THEN
            ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
            EXIT FOREACH
         END IF
      END FOREACH

      IF (pos-1) < 1 THEN
         ERROR "ARCHIVO ICEFAS... VACIO"
      END IF

      CALL SET_COUNT(pos-1)
      DISPLAY ARRAY l_reg TO scr_1.*
              ON KEY ( INTERRUPT )
                 LET pos = 0
                 EXIT DISPLAY
              ON KEY ( CONTROL-M )
                 LET pos = ARR_CURR()
                 EXIT DISPLAY
      END DISPLAY

      IF pos <> 0 THEN
         EXIT WHILE
      END IF
END WHILE
CLOSE WINDOW vent_1
RETURN l_reg[pos].codigo#,l_reg[pos].descripcion
END FUNCTION
################################################################################
FUNCTION Verifica_documento_probatorio(x_num,x_param)
DEFINE x_num        SMALLINT
DEFINE x_param      CHAR(20)
DEFINE op1_uno      CHAR(2)
DEFINE op1_dos      CHAR(3)
DEFINE op1_tres     CHAR(2)
DEFINE op1_cuatro   CHAR(4)
DEFINE op1_cinco    CHAR(5)

DEFINE numero       INTEGER

DEFINE op2_uno      CHAR(1)

DEFINE op3_uno      CHAR(9)
DEFINE op3_dos      CHAR(7)

DEFINE op4_uno      CHAR(9)
DEFINE op4_dos      CHAR(2)
DEFINE op4_tres     CHAR(5)

IF x_num <> 6 THEN
   IF LENGTH(x_param) <> 16 THEN
      ERROR "El Largo de Documento Probatorio debe ser 16"
      RETURN FALSE
   END IF
END IF

CASE x_num
WHEN 1 # ACTA DE NACIMIENTO #

       LET op1_uno   = x_param[1,2]
       LET op1_dos   = x_param[3,5]
       LET op1_tres  = x_param[6,7]
       LET op1_cuatro = x_param[8,11]
       LET op1_cinco = x_param[12,16]
       LET numero = op1_uno

       SELECT "a.X" FROM safre_af:tab_estado a
       WHERE a.estad_cod = numero

       IF STATUS = NOTFOUND THEN
          ERROR "Entidad Federativa Inexistente ( POSICION 1/2 ) ",
                op1_uno
          RETURN FALSE
       END IF
       LET numero = op1_dos
       IF numero IS NULL THEN 
            ERROR "Municipio Erroneo NO es un Numero ",
                  "( POCISION 3/4/5 ) ",op1_dos RETURN FALSE
       END IF
       LET numero = op1_tres
       IF numero IS NULL THEN 
            ERROR "An~o Erroneo NO es un Numero ",
                  "( POCISION 6/7 ) ",op1_tres RETURN FALSE
       END IF
       LET numero = op1_cuatro
       IF numero IS NULL THEN 
            ERROR "Libro Erroneo NO es un Numero ",
                  "( POCISION 8/9/10/11 ) ",op1_cuatro RETURN FALSE
       END IF
       LET numero = op1_cinco
       IF numero IS NULL THEN 
            ERROR "Acta Erronea NO es un Numero ",
                  "( POSICION 12/13/14/15/16 ) ",op1_cinco RETURN FALSE
       END IF

WHEN 2 # ACTA DE NACIMIENTO #

       LET op2_uno = x_param[1] 
       IF op2_uno <> " " THEN
          ERROR "La Primera Pocision del Documento ",
                "debe ser un Blanco ",op2_uno
          RETURN FALSE
       END IF

WHEN 3 # DOCUMENTO MIGRATORIO #

       LET op3_uno = x_param[1,9] 
       LET op3_dos = x_param[10,16] 

       IF op3_uno <> "         " THEN
          ERROR "La Nueve Primeras Pocisiones del Documento ",
                  "deben ser Blancos ",op3_uno
          RETURN FALSE
       END IF

       LET numero = op3_dos
       IF numero IS NULL THEN 
            ERROR "Reg. Nacional Extrangeros NO es Numero ",
                  "( POSICION 10/11/12/13/14/15/16 ) ",op3_dos RETURN FALSE
       END IF

WHEN 4 # DOCUMENTO DE NATURALIZACION DE LA SRE #

       LET op4_uno = x_param[1,9] 
       LET op4_dos = x_param[10,11] 
       LET op4_tres = x_param[12,16] 

       IF op4_uno <> "         " THEN
          ERROR "La Nueve Primeras Pocisiones del Documento ",
                  "deben ser Blancos ",op4_uno
          RETURN FALSE
       END IF

       LET numero = op4_dos
       IF numero IS NULL THEN 
          ERROR "An~o de Registro ( POSICION 10/11 ) ",op4_dos RETURN FALSE
       END IF

       LET numero = op4_tres

       IF numero IS NULL THEN 
          ERROR "N. Folio Carta ( POSICION 12/13/14/15/16 ) ",
                op4_tres RETURN FALSE
       END IF

WHEN 5 # DOCUMENTO CURP #
       #SE DEBE VERIFICAR ALGORITMO DE VALIDACION H.F.
END CASE
RETURN TRUE
END FUNCTION

FUNCTION Despliega_colonias(xcpos_cod)
   DEFINE xcpos_cod         CHAR(05), 
          aux_val           SMALLINT,
          x_x               CHAR(300),
          x_buscar          CHAR(30),
          pos               SMALLINT,

          reg record
             cod_colon smallint,
             colonia char(40),
             deleg   smallint, 
             ciudad  smallint,
             estado  smallint
          end record,

          desdeleg char(40),
          desciuda char(40),
          desestad char(40),

          l_reg ARRAY[1000] OF RECORD
             cod            CHAR(05),
             codigo         INTEGER,
             descripcion    CHAR(40)
          END RECORD

    ERROR "BUSCANDO INFORMACION ..."

    DECLARE cur_cp CURSOR FOR
        SELECT cpos_cod,colon_cod,colon_desc 
        FROM safre_af:tab_colonia
        WHERE cpos_cod = xcpos_cod
      
        LET pos = 1
    FOREACH cur_cp INTO l_reg[pos].*
         LET pos = pos + 1
    END FOREACH
    
    ERROR ""
   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)
      OPEN WINDOW ventana_cp AT 6,08 WITH FORM "PRO_GLOBP3" ATTRIBUTE (BORDER)
      DISPLAY " (ENTER) Elegir                                   (Ctrl - C)",
              " Salir " AT 1,1 ATTRIBUTE(BOLD)
      DISPLAY "                      C  O  L  O  N  I  A  ",
              "S                       " AT 2,1 ATTRIBUTE (REVERSE,BOLD)
     
         DISPLAY ARRAY l_reg to scr_1.*
            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()

             SELECT deleg_cod,ciudad_cod,estad_cod 
               INTO   reg.deleg,reg.ciudad,reg.estado
               FROM safre_af:tab_codpos  
               WHERE cpos_cod = l_reg[pos].cod

               SELECT deleg_desc 
               INTO desdeleg 
               FROM safre_af:tab_delegacion 
               WHERE deleg_cod = reg.deleg

               SELECT ciudad_desc INTO desciuda
               FROM safre_af:tab_ciudad WHERE
               ciudad_cod = reg.ciudad

               SELECT estad_desc INTO desestad
               FROM safre_af:tab_estado WHERE
               estad_cod = reg.estado

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
   RETURN l_reg[pos].descripcion,
          reg.deleg,
          desdeleg,
          reg.ciudad,
          desciuda,
          reg.estado,
          desestad 

END FUNCTION

FUNCTION Despliega_codigo_postal()
DEFINE aux_val           SMALLINT
DEFINE l_reg ARRAY[1000] OF RECORD
          cod            char(05),
          descrip        char(25),
          descripcion    CHAR(25)
       END RECORD,

       reg record
         cod_colon smallint,
         colonia char(40),
         deleg   smallint, 
         ciudad  smallint,
         estado  smallint
       end record,

       desdeleg char(40),
       desciuda char(40),
       desestad char(40),
       vestad   smallint 

DEFINE x_x      char(300),
       x_buscar char(30)

DEFINE pos      SMALLINT

OPEN WINDOW vent_1 AT 05,07 WITH FORM "PRO_GLOBP4" ATTRIBUTE(BORDER)
DISPLAY "                        CODIGOS POSTALE",
                 "S                          " AT 2,1 ATTRIBUTE(REVERSE)
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
PROMPT 'DIGITA CODIGO ESTADO DE ESTA COLONIA ...' 
       for vestad attribute(reverse)

WHENEVER ERROR STOP

if vestad is null then
   ERROR "SOLO PUEDE SER CODIGO" SLEEP 1 
end if

if vestad is null then
   let vestad=0
end if

ERROR "BUSCANDO INFORMACION ..."

WHILE TRUE
            LET x_x = " SELECT c.cpos_cod  ,",
                      "        a.colon_desc,",
                      "        b.deleg_desc ",
                      " FROM   tab_codpos c,tab_colonia a,tab_delegacion b ",
                      " WHERE  c.cpos_cod   = a.cpos_cod ",
                      " AND    c.deleg_cod  = b.deleg_cod ",
                      " AND    a.colon_desc MATCHES ",'"',x_buscar CLIPPED,'"',
                      " AND    c.estad_cod  =",vestad CLIPPED,
                      " ORDER BY 2 " CLIPPED

      PREPARE curg21 FROM x_x
      DECLARE cur_g21 CURSOR FOR curg21
          LET pos = 1
      FOREACH cur_g21 INTO l_reg[pos].*
          if status=100 then
             exit foreach
          end if

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
            FROM   safre_af:tab_codpos  
            WHERE  cpos_cod = l_reg[pos].cod

            SELECT deleg_desc 
            INTO   desdeleg 
            FROM   safre_af:tab_delegacion 
            WHERE  deleg_cod = reg.deleg

            SELECT ciudad_desc INTO desciuda
            FROM   safre_af:tab_ciudad 
            WHERE  ciudad_cod = reg.ciudad
          
            SELECT estad_desc INTO desestad
            FROM   safre_af:tab_estado 
            WHERE  estad_cod = reg.estado

            EXIT DISPLAY
      END DISPLAY

      IF pos <> 0 THEN
         EXIT WHILE
      END IF
END WHILE

CLOSE WINDOW vent_1
      RETURN   l_reg[pos].cod,
               l_reg[pos].descrip,
               reg.deleg,
               desdeleg,
               reg.ciudad,
               desciuda,
               reg.estado,
               desestad 
END FUNCTION
################################################################################
FUNCTION Despliega_area()
DEFINE aux_val           SMALLINT
DEFINE l_reg ARRAY[1000] OF RECORD
          codigo            INTEGER,
          descripcion       CHAR(50)
       END RECORD
DEFINE x_x               char(100),
       x_buscar          char(30)
DEFINE pos               SMALLINT

OPEN WINDOW vent_1 AT 05,12 WITH FORM "PRO_GLOBP1" ATTRIBUTE(BORDER)
DISPLAY "                   A R E A ",
                 "S                             " AT 2,1 ATTRIBUTE(REVERSE)
INPUT BY NAME x_buscar
      AFTER FIELD x_buscar
         IF x_buscar IS NULL THEN
            ERROR "Descripcion a Buscar NO puede ser nulo"
            NEXT FIELD x_buscar
         ELSE
            EXIT INPUT
         END IF
END INPUT

WHILE TRUE
      LET x_x = " SELECT area_code,area_desc FROM safre_af:tabarea ",
                " WHERE area_desc MATCHES ",'"',x_buscar CLIPPED,'"',
                " ORDER BY 2 " CLIPPED

      PREPARE cur28 FROM x_x
      DECLARE cur_28 CURSOR FOR cur28
      LET pos = 1
      FOREACH cur_28 INTO l_reg[pos].*
         LET pos = pos + 1
         IF pos >= 1000 THEN
            ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
            EXIT FOREACH
         END IF
      END FOREACH

      IF (pos-1) < 1 THEN
         ERROR "ARCHIVO AREAS... VACIO"
      END IF

      CALL SET_COUNT(pos-1)
      DISPLAY ARRAY l_reg TO scr_1.*
              ON KEY ( INTERRUPT )
         LET pos = 0
         EXIT DISPLAY
      ON KEY ( CONTROL-M )
         LET pos = ARR_CURR()
         EXIT DISPLAY
      END DISPLAY
      IF pos <> 0 THEN
         EXIT WHILE
      END IF
END WHILE
CLOSE WINDOW vent_1
RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION

FUNCTION valida_rfc (registro_1)
    DEFINE registro_1 RECORD
           paterno    CHAR(40)  ,
           materno    CHAR(40)  ,
           nombres    CHAR(40)  ,
           fnaci      DATE
    END RECORD

    DEFINE acumulado  INTEGER 
    DEFINE ind_1      SMALLINT
    DEFINE ind_2      SMALLINT
    DEFINE nacimiento CHAR(10)
    DEFINE ind_3      SMALLINT
    DEFINE inicio     SMALLINT
    DEFINE cociente   SMALLINT
    DEFINE residuo    SMALLINT
    DEFINE paso       SMALLINT
    DEFINE vocales    CHAR(05)
    DEFINE digito     CHAR(01)
    DEFINE rfc        CHAR(10)

    LET vocales = "AEIOU"
    LET nacimiento = registro_1.fnaci

    LET inicio = 1
    IF registro_1.paterno[1,3] = "DE "   OR 
       registro_1.paterno[1,3] = "LA "  THEN
       LET inicio = 4
    END IF
    IF registro_1.paterno[1,4] = "DEL "   OR 
       registro_1.paterno[1,3] = "LAS "   OR 
       registro_1.paterno[1,3] = "LOS "  THEN
       LET inicio = 5
    END IF
    IF registro_1.paterno[1,3] = "DE LA "  THEN
       LET inicio = 7
    END IF
    IF registro_1.paterno[1,3] = "DE LAS "   OR 
       registro_1.paterno[1,3] = "DE LOS "  THEN
       LET inicio = 8
    END IF

    LET rfc[1] = registro_1.paterno[inicio]

    LET inicio = inicio + 1
    FOR ind_1 = inicio to LENGTH(registro_1.paterno)
        FOR ind_2 = 1 to 5
            IF registro_1.paterno[ind_1] = vocales[ind_2] THEN

               LET rfc[2] = registro_1.paterno[ind_1]

               LET ind_2 = 6
               LET ind_1 = 50
            END IF 
        END FOR
    END FOR

    LET inicio = 1
    IF registro_1.materno[1,3] = "DE "   OR 
       registro_1.materno[1,3] = "LA "  THEN
       LET inicio = 4
    END IF
    IF registro_1.materno[1,4] = "DEL "   OR 
       registro_1.materno[1,3] = "LAS "   OR 
       registro_1.materno[1,3] = "LOS "  THEN
       LET inicio = 5
    END IF
    IF registro_1.materno[1,3] = "DE LA "  THEN
       LET inicio = 7
    END IF
    IF registro_1.materno[1,3] = "DE LAS "   OR 
       registro_1.materno[1,3] = "DE LOS "  THEN
       LET inicio = 8
    END IF

    LET rfc[3] = registro_1.materno[inicio]

    LET rfc[4] = registro_1.nombres[1]

    LET rfc[5,6] = nacimiento[9,10]

    LET rfc[7,8] = nacimiento[1,2]

    LET rfc[9,10] = nacimiento[4,5]

    RETURN rfc
 
END FUNCTION
    
################################################################################
FUNCTION sel_recibo()
   DEFINE aux_val      SMALLINT,

          l_reg ARRAY[1000] OF RECORD
              codigo      INTEGER,
              descripcion CHAR(50)
          END RECORD,

          x_x        CHAR(100),
          x_buscar   CHAR(30),
          pos        SMALLINT

   OPEN WINDOW pro_globp6 AT 05,12 WITH FORM "PRO_GLOBP6" ATTRIBUTE(BORDER)
   DISPLAY "                    TIPOS DE RECIBOS                     " 
                   AT 2,1 ATTRIBUTE(REVERSE)
   INPUT BY NAME x_buscar
         AFTER FIELD x_buscar
           IF x_buscar IS NULL THEN
              ERROR "Descripcion a Buscar NO puede ser nulo"
              NEXT FIELD x_buscar
           ELSE
              EXIT INPUT
           END IF
   END INPUT

   WHILE TRUE
         LET x_x = " SELECT codigo,descripcion FROM tab_tipo_recibo ",
                   " WHERE descripcion MATCHES ",'"',x_buscar CLIPPED,'"',
                   " ORDER BY 1 " CLIPPED
         PREPARE cur_recibo FROM x_x
         DECLARE cur_reci_bo CURSOR FOR cur_recibo
         LET pos = 1
         FOREACH cur_reci_bo INTO l_reg[pos].*
            LET pos = pos + 1
            IF pos >= 1000 THEN
                ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
                EXIT FOREACH
            END IF
         END FOREACH

         IF (pos-1) < 1 THEN
            ERROR "ARCHIVO TIPOS DE RECIBO..... VACIO"
         END IF

         CALL SET_COUNT(pos-1)
         DISPLAY ARRAY l_reg TO scr_1.*
                 ON KEY ( INTERRUPT )
                    LET pos = 0
                    EXIT DISPLAY
                 ON KEY ( CONTROL-M )
                    LET pos = ARR_CURR()
                    EXIT DISPLAY
         END DISPLAY

         IF pos <> 0 THEN
             EXIT WHILE
         END IF
   END WHILE
   CLOSE WINDOW pro_globp6
   RETURN l_reg[pos].codigo
END FUNCTION
################################################################################
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

###########################################################################
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
     	  IF arr_curp[i].curp_pos MATCHES"[0-9]" THEN 
     	     FOR k = 1 TO 10
              IF arr_curp[i].curp_pos = arr_nume[k].num THEN
                 LET contador5 = contador5 + 1
              END IF
           END FOR
     	  ELSE	
           FOR j = 1 TO 27
              IF arr_curp[i].curp_pos = arr_letr[j].car THEN
                 LET contador5 = contador5 + 1
              END IF
           END FOR
        END IF   
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

   FOR i = 1 TO 18
     IF (arr_curp[i].curp_pos = ' ') THEN
         LET pasa = 1
         LET desc_err = "Error la CURP tiene espacios en blanco"
         EXIT FOR
     END IF
   END FOR

   RETURN pasa, desc_err

END FUNCTION
######################################################################

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
                    SELECT "a.X" FROM afi_articulo a
                            WHERE a.palabra = pater1
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 1
                       EXIT FOR
                    END IF
             WHEN 2
                    LET pater2 = pater
                    SELECT "a.X" FROM afi_articulo a
                            WHERE a.palabra = pater2
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 2
                       EXIT FOR
                    END IF
             WHEN 3
                    LET pater3 = pater
                    SELECT "a.X" FROM afi_articulo a
                            WHERE a.palabra = pater3
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 3
                       EXIT FOR
                    END IF
             WHEN 4
                    LET pater4 = pater
                    SELECT "a.X" FROM afi_articulo a
                            WHERE a.palabra = pater4
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE pater TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 4
                       EXIT FOR
                    END IF
             WHEN 5
                    LET pater5 = pater
                    SELECT "a.X" FROM afi_articulo a
                            WHERE a.palabra = pater5
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
                          SELECT "a.X" FROM afi_articulo a
                                  WHERE a.palabra = mater1
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 1
                             EXIT FOR
                          END IF
                   WHEN 2
                          LET mater2 = mater CLIPPED
                          SELECT "a.X" FROM afi_articulo a
                                  WHERE a.palabra = mater2
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 2
                             EXIT FOR
                          END IF
                   WHEN 3
                          LET mater3 = mater CLIPPED
                          SELECT "a.X" FROM afi_articulo a
                                  WHERE a.palabra = mater3
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 3
                             EXIT FOR
                          END IF
                   WHEN 4
                          LET mater4 = mater CLIPPED
                          SELECT "a.X" FROM afi_articulo a
                               WHERE a.palabra = mater4
                          IF STATUS != NOTFOUND THEN
                             INITIALIZE mater TO NULL
                             LET j = 1
                          ELSE
                             LET bb = 4
                             EXIT FOR
                          END IF
                   WHEN 5
                          LET mater5 = mater CLIPPED
                          SELECT "a.X" FROM afi_articulo a
                                  WHERE a.palabra = mater5
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
                    SELECT "a.X" FROM afi_articulo a
                            WHERE a.palabra = nom_b2
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE nom_b TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 2
                       EXIT FOR
                    END IF
             WHEN 3
                    LET nom_b3 = nom_b CLIPPED
                    SELECT "a.X" FROM afi_articulo a
                            WHERE a.palabra = nom_b3
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE nom_b TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 3
                       EXIT FOR
                    END IF
             WHEN 4
                    LET nom_b4 = nom_b CLIPPED
                    SELECT "a.X" FROM afi_articulo a
                            WHERE a.palabra = nom_b4
                    IF STATUS != NOTFOUND THEN
                       INITIALIZE nom_b TO NULL
                       LET j = 1
                    ELSE
                       LET bb = 4
                       EXIT FOR
                    END IF
             WHEN 5
                    LET nom_b5 = nom_b CLIPPED
                    SELECT "a.X" FROM afi_articulo a
                            WHERE a.palabra = nom_b5
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
          WHERE b.palabra_no = patmatnom
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

FUNCTION grado_escolar()
    DEFINE arr_4 ARRAY[2000] OF RECORD
        codigo          CHAR(1),
        descripcion     CHAR(20)
    END RECORD

    DEFINE
        txt_1                 CHAR(1500) ,
        x_busca               CHAR(2000)

    DEFINE
        pos                      SMALLINT,
        sw_1                     SMALLINT

    INITIALIZE x_busca, txt_1 TO NULL
    LET sw_1 = 0

    OPEN WINDOW pro_blobp7 AT 06,13 WITH FORM "PRO_GLOBP7" ATTRIBUTE(BORDER)
    DISPLAY "<ENTER> Aceptar    <CTRL-C> Salir    ",
            "                        " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY "                  GRADO ESCOLAR                     ",
            "                          " AT 2,1 ATTRIBUTE(REVERSE)

        LET  arr_4[1].codigo = 'A' 
        LET  arr_4[1].descripcion ='PRIMARIA'
        LET  arr_4[2].codigo = 'B' 
        LET  arr_4[2].descripcion ='SECUNDARIA'
        LET  arr_4[3].codigo = 'C' 
        LET  arr_4[3].descripcion ='COMERCIAL'
        LET  arr_4[4].codigo = 'D' 
        LET  arr_4[4].descripcion ='PREPARATORIA'
        LET  arr_4[5].codigo = 'E' 
        LET  arr_4[5].descripcion ='BACHILLERATO'
        LET  arr_4[6].codigo = 'F' 
        LET  arr_4[6].descripcion ='CARRERA TECNICA'
        LET  arr_4[7].codigo = 'G' 
        LET  arr_4[7].descripcion ='LICENCIATURA PASANTE'
        LET  arr_4[8].codigo = 'H' 
        LET  arr_4[8].descripcion ='LICENCIATURA TITULADO'
        LET  arr_4[9].codigo = 'I' 
        LET  arr_4[9].descripcion ='MAESTRIA'
        LET  arr_4[10].codigo = 'J' 
        LET  arr_4[10].descripcion ='DIPLOMADO'
        LET  arr_4[11].codigo = 'K' 
        LET  arr_4[11].descripcion ='POSTGRADO'
        LET  arr_4[12].codigo = 'L' 
        LET  arr_4[12].descripcion ='DOCTORADO'

        LET pos = 12
        IF (pos-1) < 1 THEN
            LET sw_1 = 1
            ERROR "SELECCION GRUPO DE VENTA..... VACIO"
        END IF

        CALL SET_COUNT(pos-1)

        DISPLAY ARRAY arr_4 TO scr_1.*
            ON KEY (CONTROL-C, INTERRUPT)
                LET sw_1 = 1
                EXIT DISPLAY

            ON KEY (CONTROL-M)
                LET pos = ARR_CURR()
                EXIT DISPLAY
        END DISPLAY

    CLOSE WINDOW pro_blobp7

    IF sw_1 = 1 THEN
        RETURN "","                      "
    END IF
    RETURN arr_4[pos].codigo
END FUNCTION

#NUEVO 

FUNCTION despliega_referencias(lc_unico)

      DEFINE lc_unico  CHAR(18)
      DEFINE vresp    CHAR(1)

      DEFINE gr_ref RECORD
        nombre1               CHAR(120),              
        curp1                 CHAR(18) ,
        telefono1             CHAR(10) ,
        parentesco1           CHAR(02) ,
        desc_parentesco1      CHAR(40) , 
        nombre2               CHAR(120), 
        curp2                 CHAR(18) ,
        telefono2             CHAR(10) ,
        parentesco2           CHAR(02) ,
        desc_parentesco2      CHAR(40) ,
        num_jefe              CHAR(10) ,
        curpjefe              CHAR(18) , 
        tipo_contratacion     CHAR(01) ,
        desc_tipo_contratacion CHAR(15),
        numero_empleado        CHAR(15),
        entidad_federativa     SMALLINT,
        desc_entidad_federativa CHAR(40) 
      END RECORD

      OPEN WINDOW v1 AT  2,4 WITH FORM "PROM02511" ATTRIBUTE(BORDER)
      DISPLAY "                 INFORMACION REFERENCIAS                    " AT 1,1 ATTRIBUTE(REVERSE)
      DISPLAY "      REFERENCIA PERSONAL 1                                           " AT  5,1 ATTRIBUTE(REVERSE)
      DISPLAY "      REFERENCIA PERSONAL 2                                           " AT 10,1 ATTRIBUTE(REVERSE)
      DISPLAY "      DATOS LAROBALES                                                 " AT 15,1 ATTRIBUTE(REVERSE)  
         #NO DEBE EXISTIR MAS DE UN REGISTRO POR CURP
           SELECT nombre_ref1,
                  unico_ref1 ,
                  tel_ref1   ,
                  paren_ref1 ,
                  nombre_ref2,
                  unico_ref2 ,  
                  tel_ref2   ,
                  paren_ref2 ,
                  num_jefe   ,
                  curp_jefe  ,
                  tipo_contrato,
                  cod_ent_tra  ,
                  ent_trabajo  
           INTO gr_ref.nombre1    ,
                gr_ref.curp1      ,
                gr_ref.telefono1  ,
                gr_ref.parentesco1,
                gr_ref.nombre2    ,
                gr_ref.curp2      ,
                gr_ref.telefono2  ,
                gr_ref.parentesco2,
                gr_ref.num_jefe   ,
                gr_ref.curpjefe   ,
                gr_ref.tipo_contratacion,
                gr_ref.entidad_federativa,
                gr_ref.desc_entidad_federativa
           FROM   pro_solicitud_referencias
           WHERE  unico = lc_unico

           IF SQLCA.SQLCODE <> 0 THEN 
              ERROR "PROMOTOR NO TIENE REFERENCIAS"
              ATTRIBUTE (REVERSE)
              SLEEP 4
              ERROR " "
              CLOSE WINDOW v1
              RETURN
           END IF
           
          SELECT paren_desc
          INTO gr_ref.desc_parentesco1
          FROM tab_parentesco_cuo
          WHERE paren_cod = gr_ref.parentesco1
          
          SELECT paren_desc
          INTO gr_ref.desc_parentesco2
          FROM tab_parentesco_cuo
          WHERE paren_cod = gr_ref.parentesco2
          
          SELECT codven
          INTO gr_ref.numero_empleado
          FROM pro_mae_promotor
          WHERE unico = lc_unico
          GROUP BY 1
 #CPL-3604         
          IF gr_ref.tipo_contratacion = "1" THEN 
          	LET gr_ref.desc_tipo_contratacion = "INTERNA"
          ELSE 
             IF gr_ref.tipo_contratacion = "2" THEN 
          	 LET gr_ref.desc_tipo_contratacion = "EXTERNA"
              ELSE
             		IF gr_ref.tipo_contratacion = "3" THEN 
          	 			LET gr_ref.desc_tipo_contratacion = "CENEVAL INTERNO"
              	ELSE            
                  LET gr_ref.desc_tipo_contratacion = "CENEVAL EXTERNA"
                END IF
            END IF 
          END IF 

      DISPLAY BY NAME gr_ref.*
       
      PROMPT "               Presione <Enter> Para Continuar                "
      ATTRIBUTE (REVERSE)
      FOR vresp
      ATTRIBUTE (REVERSE)

      CLOSE WINDOW v1
END FUNCTION
FUNCTION Despliega_parentesco_cuo()
   DEFINE aux_val   SMALLINT,
   
          l_reg ARRAY[1000] OF RECORD
             codigo  CHAR(02),
             descripcion CHAR(40)
          END RECORD,

          x_x         char(100),
          x_buscar    char(30),
          pos         SMALLINT

   OPEN WINDOW vent_1 AT 05,12 WITH FORM "PRO_GLOBP1" ATTRIBUTE(BORDER)
   DISPLAY "                 P A R E N T E S C O S                   " 
                   AT 2,1 ATTRIBUTE(REVERSE)
   --INPUT BY NAME x_buscar
   --      AFTER FIELD x_buscar
   --         IF x_buscar IS NULL THEN
   --            ERROR "Descripcion a Buscar NO puede ser nulo"
   --            NEXT FIELD x_buscar
   --         ELSE
   --            EXIT INPUT
   --         END IF
   --END INPUT

   WHILE TRUE
         LET x_x = " SELECT paren_cod,paren_desc FROM tab_parentesco_cuo ",
                  -- " WHERE paren_desc ",--'"',x_buscar CLIPPED,'"',
                   " ORDER BY 1 ASC " CLIPPED

         PREPARE curg7cu FROM x_x
         DECLARE cur_g7cu CURSOR FOR curg7cu
            LET pos = 1
         FOREACH cur_g7cu INTO l_reg[pos].*
            LET pos = pos + 1
            IF pos >= 1000 THEN
               ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
               EXIT FOREACH
            END IF
         END FOREACH

         IF (pos-1) < 1 THEN
            ERROR "ARCHIVO PARENTESCO..... VACIO"
         END IF

         CALL SET_COUNT(pos-1)
         DISPLAY ARRAY l_reg TO scr_1.*
                 ON KEY ( INTERRUPT )
                    LET pos = 1
                    EXIT DISPLAY

                 ON KEY ( CONTROL-M )
                    LET pos = ARR_CURR()
                    EXIT DISPLAY
         END DISPLAY

         IF pos <> 0 THEN
            EXIT WHILE
         END IF
   END WHILE
   CLOSE WINDOW vent_1
   RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION
################################################################################
#==============================================================================#
#MLM-3507 SE AGREGA CONSULTA BIOMETRICOS
FUNCTION consulta_biometrico(lc_curp)

      DEFINE lc_curp  LIKE pro_mae_promotor.unico
      DEFINE vresp    CHAR(1)

      DEFINE r_biometrico RECORD 
      	cod_promotor    LIKE pro_mae_promotor.cod_promotor   ,
      	status_interno  LIKE afi_com_det_op15.status_interno ,
      	status_desc     CHAR(30)                             ,
      	motivo_rechazo1 LIKE afi_ctr_det_op15.motivo_rechazo1,
      	motivo_rechazo2 LIKE afi_ctr_det_op15.motivo_rechazo2,
      	motivo_rechazo3 LIKE afi_ctr_det_op15.motivo_rechazo3,
      	fecha_recep     LIKE afi_ctr_det_op15.fecha_recep    ,
      	dia1_desc       LIKE tab_rdeta.rdeta_desc_c          , 
      	dia2_desc       LIKE tab_rdeta.rdeta_desc_c          , 
      	dia3_desc       LIKE tab_rdeta.rdeta_desc_c
      END RECORD

      INITIALIZE r_biometrico.* TO NULL 

      OPEN WINDOW v2 AT  7,10 WITH FORM "PROM0254" ATTRIBUTE(BORDER)
      DISPLAY "                     BIOMETRICOS PROMOTORES                      "
              AT 1,1 ATTRIBUTE(REVERSE)

           SELECT status_interno,
                  motivo_rechazo1,
                  motivo_rechazo2,
                  motivo_rechazo3, 
                  fecha_recep
           INTO   
               r_biometrico.status_interno,
               r_biometrico.motivo_rechazo1,
               r_biometrico.motivo_rechazo2,
               r_biometrico.motivo_rechazo3, 
               r_biometrico.fecha_recep 
           FROM   afi_ctr_det_op15
           WHERE  curp = lc_curp

           IF SQLCA.SQLCODE <> 0 THEN  
           	LET r_biometrico.status_interno = 10
              #ERROR "PROMOTOR NO CUENTA CON BIOMETRICOS"
              #ATTRIBUTE (REVERSE)
              #SLEEP 4
              #ERROR " "
              #CLOSE WINDOW v1
              #RETURN
           END IF
                      
           SELECT cod_promotor
           INTO r_biometrico.cod_promotor 
           FROM pro_mae_promotor
           WHERE unico = lc_curp
           
           IF r_biometrico.cod_promotor IS NULL THEN 
             DECLARE cur_cod CURSOR FOR 
               SELECT cod_promotor
               FROM pro_solicitud
               WHERE unico = lc_curp           	
               AND fingre IN ( SELECT MAX(fingre) 
                               FROM pro_solicitud
                               WHERE unico = lc_curp )
               GROUP BY 1
               

             FOREACH cur_cod INTO r_biometrico.cod_promotor 
                EXIT FOREACH
             END FOREACH           
           END IF 
           
           
           IF r_biometrico.motivo_rechazo1 IS NOT NULL THEN 
           	 SELECT rdeta_desc_c
           	 INTO r_biometrico.dia1_desc
           	 FROM tab_rdeta
           	 WHERE rdeta_cod = r_biometrico.motivo_rechazo1
           	 AND modulo_cod = "bio"
          END IF 

           IF r_biometrico.motivo_rechazo2 IS NOT NULL THEN 
           	 SELECT rdeta_desc_c
           	 INTO r_biometrico.dia2_desc
           	 FROM tab_rdeta
           	 WHERE rdeta_cod = r_biometrico.motivo_rechazo2
           	 AND modulo_cod = "bio"
          END IF           

           IF r_biometrico.motivo_rechazo3 IS NOT NULL THEN 
           	 SELECT rdeta_desc_c
           	 INTO r_biometrico.dia3_desc
           	 FROM tab_rdeta
           	 WHERE rdeta_cod = r_biometrico.motivo_rechazo3
           	 AND modulo_cod = "bio"
          END IF 
        
          CASE r_biometrico.status_interno
          	 WHEN 10 
          	 	  LET r_biometrico.status_desc = "SIN BIOMETRICO"
          	 WHEN 20
          	 	  LET r_biometrico.status_desc =	"ENVIADO EE"
          	 WHEN 30 
          	 		LET r_biometrico.status_desc = "TEMPORAL"          	
          	 WHEN 40 
          	 	  LET r_biometrico.status_desc = "RECHAZADO EE"		    
          	 WHEN 50 
          	 		LET r_biometrico.status_desc = "EN TRANSITO EE"
          	 WHEN 60 
          	 		LET r_biometrico.status_desc = "EN PROCESO DE CANCELACION"          	 		 
          	 WHEN 70 
          	 		LET r_biometrico.status_desc = "CANCELADO EE"
          	 WHEN 80 
          	 		LET r_biometrico.status_desc = "PERMANENTE EE"		
          	 		
          END CASE	 
          
      DISPLAY BY NAME r_biometrico.*

      PROMPT "               Presione <Enter> Para Continuar                "
      ATTRIBUTE (REVERSE)
      FOR vresp
      ATTRIBUTE (REVERSE)

      CLOSE WINDOW v2
END FUNCTION