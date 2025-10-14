##########################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                       #
#Propietario       => E.F.P.                                             #
#Programa AFIM013  => MANTENIMIENTO A TELEFONOS DE AFILIADOS             #
#Fecha             => 5 DE NOVIEMBRE DE 2000                             #
#Autor             => MAURO MUNIZ CABLLERO         		         #
#Sistema           => AFI. 					         #
#                  => EDUARDO J RESENDIZ MEDINA 15 DE JULIO 2008  28-18  #
##########################################################################

DATABASE safre_af

GLOBALS

    DEFINE tel ARRAY [8] OF RECORD
        tel_cod    SMALLINT,
        tel_desc   CHAR(10),
        pais_cod   CHAR(3) ,
        pais_desc  CHAR(20),
        cve_lada   CHAR(3),
        telefono   CHAR(40),
        extension  INTEGER ,
        idrow      INTEGER,
        hora_ini   DATETIME HOUR TO MINUTE,                 ---28-18 31072008
        tipo_hora_ini CHAR(1),              ---28-18
        desc_tipo_hora_ini CHAR(2),         ---28-18
        hora_fin   DATETIME HOUR TO MINUTE,                 ---28-18 31072008
        tipo_hora_fin CHAR(1),              ---28-18
        desc_tipo_hora_fin CHAR(2),         ---28-18
        dia        CHAR(1),                 ---28-18
        desc_dia           CHAR(18)         ---28-18
    END RECORD

    DEFINE ACCION    CHAR(1)
    DEFINE enter     CHAR(1)
    DEFINE g_usuario CHAR(8)
    DEFINE HOY	     DATE
    
    DEFINE
        arr       ,
        src       SMALLINT

    DEFINE g_afili RECORD 
        n_seguro         CHAR(11),
        n_unico          CHAR(18),
        n_rfc            CHAR(13),
        paterno          CHAR(40),
        materno          CHAR(40),
        nombres          CHAR(40),
        fena             DATE,
        n_folio          DECIMAL(10,0),
        tipo_solicitud   SMALLINT,
        tel_cod          SMALLINT,
        tel_desc         CHAR(10),
        pais_cod         CHAR(3) ,
        pais_desc        CHAR(20),
        cve_lada         CHAR(3) ,
        telefono         CHAR(40),
        extension        INTEGER,
        hora_ini   DATETIME HOUR TO MINUTE,                 ---28-18 31072008
        tipo_hora_ini    CHAR(1),              ---28-18
        desc_tipo_hora_ini CHAR(2),            ---28-18
        hora_fin   DATETIME HOUR TO MINUTE,                 ---28-18 31072008
        tipo_hora_fin    CHAR(1),              ---28-18
        desc_tipo_hora_fin CHAR(2),            ---28-18
        dia              CHAR(1),              ---28-18
        desc_dia         CHAR(18)              ---28-18
    END RECORD

    DEFINE pate,mate,nome CHAR(50)
    DEFINE x_seguro       CHAR(11)
    DEFINE x_unico        CHAR(18)
    DEFINE x_rfc          CHAR(13)
    DEFINE x_fena         DATE
    DEFINE aux_pausa	  CHAR(1)

    DEFINE long_tel INTEGER
    DEFINE long_cve SMALLINT
    DEFINE c_long   CHAR(15)
    DEFINE c_cve    CHAR(3)

    DEFINE vserie   SMALLINT             ---erm 06 Septiembre 2007
END GLOBALS

MAIN

    OPTIONS PROMPT LINE LAST,
        INPUT WRAP ,
        ACCEPT KEY CONTROL-I
        DEFER INTERRUPT

    CALL STARTLOG("AFIM013.log")
    CALL inicio()
    CALL proceso_principal()

END MAIN

FUNCTION inicio()
#i---------------

    LET g_afili.n_seguro       = ARG_VAL(1)
    LET g_afili.n_folio        = ARG_VAL(2)
    LET g_afili.tipo_solicitud = ARG_VAL(3)
    LET ACCION                 = ARG_VAL(4)

    LET HOY                    = TODAY

    IF g_afili.n_seguro = " " THEN
      ERROR "ESTE PROGRAMA SOLO PUEDE SER EJECUTADO DESDE SU MODULO PRINCIPAL" 
        SLEEP 3
	EXIT PROGRAM
    END IF

        SELECT n_rfc, 
               n_unico,
               paterno, 
               materno, 
               nombres, 
               fena, 
               USER
        INTO   g_afili.n_rfc,
               g_afili.n_unico,
               g_afili.paterno,
               g_afili.materno,
               g_afili.nombres,
               g_afili.fena,
               g_usuario
        FROM   afi_mae_afiliado
        WHERE  n_folio = g_afili.n_folio
        AND    tipo_solicitud = g_afili.tipo_solicitud

END FUNCTION

FUNCTION proceso_principal()
#pp------------------------

    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "AFIM0031" ATTRIBUTES(BORDER)
    DISPLAY " AFIM013                MANTENIMIENTO  AFILIADOS                               " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                           T E L E F O N O S                                   " AT 8,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING "dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

    DISPLAY BY NAME g_afili.*

        CASE 
	   WHEN ACCION = "A" OR ACCION = "M" OR ACCION = "E" 
	        MENU " TELEFONOS "
                      COMMAND "Agrega " "Agrega Telefonos"
		              LET ACCION = "A"
	                      CALL Inicializa()
	                      CALL Agrega()
	                      CALL Inicializa()
                      COMMAND "Consulta " "Consulta Telefonos"
		              LET ACCION = "C"
	                      CALL Inicializa()
                              CALL Consulta()
	                      CALL Inicializa() 
                      COMMAND "Modifica " "Modifica Telefonos"
		              LET ACCION = "M"
	                      CALL Inicializa()
                              CALL Modifica()
	                      CALL Inicializa() 
                      COMMAND "Elimina " "Elimina Telefonos"
	                      CALL Inicializa()
                              CALL Elimina()
	                      CALL Inicializa() 
                      COMMAND "Salir " "Salir de Programa"
		              EXIT MENU
	         END MENU 
	   WHEN ACCION = "C" 
	        MENU " TELEFONOS "
                      COMMAND "Consulta " "Consulta Telefonos"
		              LET ACCION = "C"
	                      CALL Inicializa()
                              CALL Consulta()
	                      CALL Inicializa() 
                      COMMAND "Salir " "Salir de Programa"
		              EXIT MENU
	         END MENU 
        END CASE

END FUNCTION

FUNCTION Inicializa()
#I------------------

    DEFINE i SMALLINT

    FOR i = 1 TO 8
        INITIALIZE tel[i].* TO NULL
    END FOR 

END FUNCTION

FUNCTION Agrega()
#A---------------

    DEFINE i SMALLINT

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " AGREGA " AT 1,67 ATTRIBUTE(REVERSE)
    DISPLAY "[ Esc ] Graba     [ Ctrl-C ] Salir sin Grabar  " 
            AT 1,1 ATTRIBUTE(BOLD)

    DISPLAY BY NAME g_afili.*

    INPUT ARRAY tel WITHOUT DEFAULTS FROM scr_1.*  

        BEFORE FIELD tel_cod
            LET arr = ARR_CURR()
            LET src = SCR_LINE()

        AFTER FIELD tel_cod
            IF tel[arr].tel_cod IS NULL THEN
                ERROR "Tipo de telefono NO puede ser NULO"
 	        NEXT FIELD tel_cod
            END IF

            SELECT tel_desc
	    INTO   tel[arr].tel_desc
            FROM   tab_telefono
	    WHERE  tel_cod = tel[arr].tel_cod

            IF SQLCA.SQLCODE <> 0 THEN
                ERROR "Tipo de telefono no existe en catalogo"
                NEXT FIELD tel_cod
            END IF

            DISPLAY tel[arr].tel_cod TO scr_1[src].tel_cod
            DISPLAY tel[arr].tel_desc TO scr_1[src].tel_desc

            NEXT FIELD pais_cod

        BEFORE FIELD pais_cod
            LET arr = ARR_CURR()
            LET src = SCR_LINE()

            LET tel[arr].pais_cod  = "052"
            LET tel[arr].pais_desc = "MEXICO"

            DISPLAY tel[arr].pais_cod TO scr_1[src].pais_cod
            DISPLAY tel[arr].pais_desc TO scr_1[src].pais_desc
 
        AFTER FIELD pais_cod
            IF tel[arr].pais_cod IS NULL THEN
                LET tel[arr].pais_cod = "052"
                LET tel[arr].pais_desc = "MEXICO"
            ELSE
                SELECT pais_desc
                INTO   tel[arr].pais_desc 
                FROM   tab_lada0
                WHERE  pais_cod = tel[arr].pais_cod
            
                IF STATUS = NOTFOUND THEN
                    LET tel[arr].pais_desc = NULL
                END IF
            END IF

            DISPLAY tel[arr].pais_cod TO scr_1[src].pais_cod
            DISPLAY tel[arr].pais_desc TO scr_1[src].pais_desc
 
--            NEXT FIELD cve_lada           --erm 06 Septembre 2007
--->erm 06 Septiembre 2007
        #IF tel[arr].tel_cod <> 4 AND
        IF tel[arr].tel_cod <> 7 THEN
            NEXT FIELD cve_lada
        ELSE
            NEXT FIELD telefono
        END IF
---<

        BEFORE FIELD cve_lada
            LET arr = ARR_CURR()
            LET src = SCR_LINE()

        AFTER FIELD cve_lada
            LET long_cve = LENGTH(tel[arr].cve_lada)

--->erm 06 Setiembre 2007
           #IF tel[arr].tel_cod <> 4 THEN
               IF tel[arr].cve_lada IS NULL OR
                  tel[arr].cve_lada = "   " THEN
                  ERROR "Clave lada NO puede ser Nulo "
                  NEXT FIELD cve_lada
               ELSE
                  FOR i = 1 TO long_cve
                      IF tel[arr].cve_lada[i] <> '1' AND
                         tel[arr].cve_lada[i] <> '2' AND
                         tel[arr].cve_lada[i] <> '3' AND
                         tel[arr].cve_lada[i] <> '4' AND
                         tel[arr].cve_lada[i] <> '5' AND
                         tel[arr].cve_lada[i] <> '6' AND
                         tel[arr].cve_lada[i] <> '7' AND
                         tel[arr].cve_lada[i] <> '8' AND
                         tel[arr].cve_lada[i] <> '9' AND
                         tel[arr].cve_lada[i] <> '0' THEN
                          ERROR "Clave LADA debe ser numerica "
                          NEXT FIELD cve_lada
                      END IF
                  END FOR

                  IF long_cve <> 2 AND
                     long_cve <> 3 THEN
                      ERROR "Clave lada debe ser de 2 o 3 pisiciones "
                      NEXT FIELD cve_lada
                  ELSE
                      SELECT UNIQUE ('X')
                      FROM tab_tel_numeracion
                      WHERE cld = tel[arr].cve_lada
                      IF SQLCA.SQLCODE = NOTFOUND THEN
                          ERROR "Clave Lada No existe en el catalogo de COFETEL "
                          NEXT FIELD cve_lada
                      END IF
                  END IF
               END IF
           #END IF
---<
            FOR i = 1 TO long_cve
                IF tel[arr].cve_lada[i] <> '1' AND
                   tel[arr].cve_lada[i] <> '2' AND
                   tel[arr].cve_lada[i] <> '3' AND
                   tel[arr].cve_lada[i] <> '4' AND
                   tel[arr].cve_lada[i] <> '5' AND
                   tel[arr].cve_lada[i] <> '6' AND
                   tel[arr].cve_lada[i] <> '7' AND
                   tel[arr].cve_lada[i] <> '8' AND
                   tel[arr].cve_lada[i] <> '9' AND
                   tel[arr].cve_lada[i] <> '0' THEN
                    ERROR "Clave LADA solo contiene digitos"
                    NEXT FIELD cve_lada
                END IF
            END FOR

            DISPLAY tel[arr].cve_lada TO scr_1[src].cve_lada
            NEXT FIELD telefono

        BEFORE FIELD telefono
            LET arr = ARR_CURR()
            LET src = SCR_LINE()

        AFTER FIELD telefono
            IF tel[arr].telefono IS NULL THEN
                ERROR "Tipo de telefono NO puede ser NULO"
 	        NEXT FIELD telefono
            END IF

--->erm 06 Septiembre 2007
            LET long_tel = LENGTH(tel[arr].telefono)

            IF tel[arr].tel_cod    <> 7 THEN
               #IF tel[arr].tel_cod <> 4 THEN
                  IF long_cve      =  2 THEN
                     IF long_tel   <> 8 THEN
                         ERROR "Longitud del Numero Telefonico incluyendo clave lada debe ser de 10 posiciones"
                         SLEEP 3
                         NEXT FIELD telefono
                     ELSE
                         LET vserie = tel[arr].telefono[1,4]
                         SELECT 'X'
                         FROM  tab_tel_numeracion
                         WHERE serie = vserie
                         AND   cld   = tel[arr].cve_lada
                         GROUP BY 1
                         IF SQLCA.SQLCODE = NOTFOUND THEN
                            ERROR "Lada y Serie de 4 posiciones no existe en el catalogo de COFETEL "
                            SLEEP 3
                            ERROR ""
                            NEXT FIELD telefono
                         END IF
                     END IF
                  END IF
               --END IF

                  IF long_cve      =  3 THEN
                     IF long_tel   <> 7 THEN
                         ERROR "Longitud del Numero Telefonico incluyendo clave lada debe ser de 10 posiciones"
                         SLEEP 3
                         NEXT FIELD telefono
                     ELSE
                         LET vserie = tel[arr].telefono[1,3]
                         SELECT 'X'
                         FROM  tab_tel_numeracion
                         WHERE serie = vserie
                         AND   cld   = tel[arr].cve_lada
                         GROUP BY 1
                         IF SQLCA.SQLCODE = NOTFOUND THEN
                            ERROR "Lada y Serie de 3 posiciones no existe en el catalogo de COFETEL "
                            SLEEP 3
                            ERROR ""
                            NEXT FIELD telefono
                         END IF
                     END IF
                  END IF
               {ELSE
                   IF long_tel <> 13 THEN
                      ERROR "Numero de Celular invalido"
                      SLEEP 3
                      NEXT FIELD telefono
                   ELSE
                      IF tel[arr].telefono[1,3] <> '044' AND
                         tel[arr].telefono[1,3] <> '045' THEN
                          ERROR "Inicio de celular debe ser 044 o 045 segun sea el caso "
                          NEXT FIELD telefono
                      ELSE 
                          IF tel[arr].telefono[1,3] = '044' THEN
                              IF tel[arr].telefono[4,5] <> '55' THEN
                                 ERROR "Posiciones 4 y 5 del telefono invalidas "
                                 NEXT FIELD telefono
                              END IF
                          ELSE
                              IF tel[arr].telefono[4,5] = '55' THEN
                                 ERROR "Posiciones 4 y 5 del telefono invalidas "
                                 NEXT FIELD telefono
                              END IF
                          END IF
                      END IF
                   END IF
               END IF}
            END IF

            LET c_long = tel[arr].telefono

            IF tel[arr].tel_cod <> 7 THEN
                FOR i = 1 TO long_tel
                    IF c_long[i] <> '1' AND
                       c_long[i] <> '2' AND
                       c_long[i] <> '3' AND
                       c_long[i] <> '4' AND
                       c_long[i] <> '5' AND
                       c_long[i] <> '6' AND
                       c_long[i] <> '7' AND
                       c_long[i] <> '8' AND
                       c_long[i] <> '9' AND
                       c_long[i] <> '0' THEN
                        ERROR "Numero de telefono solo contiene digitos"
                        NEXT FIELD telefono
                    END IF
                END FOR
            END IF
---<
            NEXT FIELD extension

        BEFORE FIELD extension
            LET arr = ARR_CURR()
            LET src = SCR_LINE()
--->28-18
        AFTER FIELD extension
            --IF g_afili.tipo_solicitud <> 2 THEN
               LET tel[arr].hora_ini           = '00:00'
               LET tel[arr].tipo_hora_ini      = '1'
               LET tel[arr].desc_tipo_hora_ini = 'AM'
               LET tel[arr].hora_fin           = '00:00'
               LET tel[arr].tipo_hora_fin      = '1'
               LET tel[arr].desc_tipo_hora_fin = 'AM'
               LET tel[arr].dia                = 1

               DISPLAY tel[arr].hora_ini TO scr_1[src].hora_ini
               DISPLAY tel[arr].tipo_hora_ini TO scr_1[src].tipo_hora_ini
               DISPLAY tel[arr].desc_tipo_hora_ini TO 
               scr_1[src].desc_tipo_hora_ini

               DISPLAY tel[arr].hora_fin TO scr_1[src].hora_fin
               DISPLAY tel[arr].tipo_hora_fin TO scr_1[src].tipo_hora_fin
               DISPLAY tel[arr].desc_tipo_hora_fin TO 
               scr_1[src].desc_tipo_hora_fin
               DISPLAY tel[arr].dia TO scr_1[src].dia
               NEXT FIELD dia
            --END IF

        BEFORE FIELD hora_ini
            LET arr = ARR_CURR()
            LET src = SCR_LINE()
            --IF g_afili.tipo_solicitud <> 2 THEN
               NEXT FIELD dia
            --END IF

        AFTER FIELD hora_ini
           --IF g_afili.tipo_solicitud <> 2 THEN
               NEXT FIELD dia
           --END IF

           IF tel[arr].hora_ini IS NULL THEN
               ERROR "ingrese la hora inicial para localizar al trabajador "
               NEXT FIELD hora_ini
           ELSE
               IF tel[arr].hora_ini > "12:00" THEN
                  LET tel[arr].tipo_hora_ini = 2
                  LET tel[arr].desc_tipo_hora_ini = 'PM'
                  DISPLAY tel[arr].tipo_hora_ini TO scr_1[src].tipo_hora_ini
                  DISPLAY tel[arr].desc_tipo_hora_ini  TO scr_1[src].desc_tipo_hora_ini
                  NEXT FIELD hora_fin
               ELSE
                  LET tel[arr].tipo_hora_ini = 1
                  LET tel[arr].desc_tipo_hora_ini = 'AM'
                  --NEXT FIELD tipo_hora_ini
                  DISPLAY tel[arr].tipo_hora_ini TO scr_1[src].tipo_hora_ini
                  DISPLAY tel[arr].desc_tipo_hora_ini  TO scr_1[src].desc_tipo_hora_ini
                  NEXT FIELD hora_fin
               END IF
           END IF

        BEFORE FIELD hora_fin
            LET arr = ARR_CURR()
            LET src = SCR_LINE()
            --IF g_afili.tipo_solicitud <> 2 THEN
               NEXT FIELD dia
            --END IF

        AFTER FIELD hora_fin
           --IF g_afili.tipo_solicitud <> 2 THEN
               NEXT FIELD dia
           --END IF

           IF tel[arr].hora_fin IS NULL THEN
               ERROR "ingrese la hora maxima para localizar al trabajador "
               NEXT FIELD hora_fin
           ELSE
               IF tel[arr].hora_fin > "12:00" THEN
                  LET tel[arr].tipo_hora_fin = 2
                  LET tel[arr].desc_tipo_hora_fin = 'PM'
                  DISPLAY tel[arr].tipo_hora_fin TO scr_1[src].tipo_hora_fin
                  DISPLAY tel[arr].desc_tipo_hora_fin  TO scr_1[src].desc_tipo_hora_fin
                  NEXT FIELD dia
               ELSE
                  LET tel[arr].tipo_hora_fin = 1
                  LET tel[arr].desc_tipo_hora_fin = 'AM'
                  DISPLAY tel[arr].tipo_hora_fin TO scr_1[src].tipo_hora_fin
                  DISPLAY tel[arr].desc_tipo_hora_fin  TO scr_1[src].desc_tipo_hora_fin
                  NEXT FIELD dia
               END IF
           END IF

        BEFORE FIELD dia
            LET arr = ARR_CURR()
            LET src = SCR_LINE()

        AFTER FIELD dia
          {IF g_afili.tipo_solicitud = 2 THEN
             IF tel[arr].dia IS NULL THEN
                ERROR "Ingrese el codigo para el rango de dias en que",
                      " se localizará al trabajador"
                NEXT FIELD dia
             ELSE
                CASE tel[arr].dia
                   WHEN 1
                      LET tel[arr].desc_dia = 'de Lunes a Viernes'
                   WHEN 2
                      LET tel[arr].desc_dia = 'Sabado o Domingo'
                   WHEN 3
                      LET tel[arr].desc_dia = 'de Lunes a Domingo'
                END CASE
                  DISPLAY tel[arr].desc_dia TO scr_1[src].desc_dia
--             NEXT FIELD tel_cod
             END IF
          END IF}
---<28-18
            LET arr = ARR_CURR()
            LET src = SCR_LINE()

      ON KEY ( ESC )

	 FOR i = 1 TO 8
             IF tel[i].telefono IS NOT NULL THEN
                IF g_afili.tipo_solicitud = 2 THEN
                   IF tel[i].hora_ini IS NULL OR                    ---28 18
                      tel[i].hora_fin IS NULL THEN                  ---28 18
                      ERROR "Ingrese la Hora Inicial u Hora Final "     ---28 18
                      NEXT FIELD hora_ini                         ---28 18
                   END IF
                END IF                                           ---28 18
	         INSERT INTO afi_telefono 
	         VALUES (g_afili.n_seguro,
		         g_afili.n_folio,
		         g_afili.tipo_solicitud,
		         tel[i].pais_cod,
                         tel[i].cve_lada,
		         tel[i].extension,
		         tel[i].telefono,
		         tel[i].tel_cod,
                         tel[i].hora_ini,                 ---28-18
                         tel[i].tipo_hora_ini,            ---28-18
                         tel[i].hora_fin,                 ---28-18
                         tel[i].tipo_hora_fin,            ---28-18
                         tel[i].dia,                      ---28-18
                         g_usuario,
                         HOY) 
             END IF
	 END FOR

            ERROR "Registros Ingresados ok"
            SLEEP 2
            ERROR ""

	 EXIT INPUT

      ON KEY ( INTERRUPT )
	 CALL Inicializa()
	 EXIT INPUT

    END INPUT

END FUNCTION

FUNCTION Consulta() 
#c-----------------

    DEFINE 
        i   SMALLINT,
        pos SMALLINT

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " CONSULTA " AT 1,67 ATTRIBUTE(REVERSE)
    DISPLAY " [ Ctrl-C ] Salir "
            AT 1,1 ATTRIBUTE(BOLD)      

    DISPLAY BY NAME g_afili.*

    LET pos = 1

    DECLARE cur_c CURSOR FOR
    SELECT t.tel_cod  ,
           d.tel_desc ,
           t.pais_cod ,
           p.pais_desc,
           t.cve_lada ,
           t.telefono ,
           t.extension,
           t.rowid,
           t.tel_hora_ini,                ---28-18 
           t.tel_tp_hora_ini,             ---28-18 
           '',                            ---28-18
           t.tel_hora_fin,                ---28-18 
           t.tel_tp_hora_fin,             ---28-18 
           '',                            ---28-18
           t.tel_dia,                     ---28-18 
           ''                             ---28-18
    FROM   afi_telefono  t,
           tab_telefono d,
    OUTER (tab_lada0      p)
    WHERE  t.nss      = g_afili.n_seguro
    AND    t.n_folio  = g_afili.n_folio
    AND    t.tipo_solicitud = g_afili.tipo_solicitud
    AND    d.tel_cod  = t.tel_cod
    AND    p.pais_cod = t.pais_cod

    FOREACH cur_c INTO tel[pos].*
--->28-18
        CASE tel[pos].tipo_hora_ini
           WHEN 1
              LET tel[pos].desc_tipo_hora_ini = 'AM'
           WHEN 2
              LET tel[pos].desc_tipo_hora_ini = 'PM'
         END CASE

        CASE tel[pos].tipo_hora_fin
           WHEN 1
              LET tel[pos].desc_tipo_hora_fin = 'AM'
           WHEN 2
              LET tel[pos].desc_tipo_hora_fin = 'PM'
         END CASE

        CASE tel[pos].dia
           WHEN 1
              LET tel[pos].desc_dia = 'de Lunes a Viernes'
           WHEN 2
              LET tel[pos].desc_dia = 'Sabado y Domingo'
           WHEN 3
              LET tel[pos].desc_dia = 'de Lunes a Domingo'
         END CASE
---<28-18
        LET pos = pos + 1

        IF pos > 8 THEN
            ERROR "Fue sobrepasada la capacidad maxima del arreglo"
            SLEEP 2
            EXIT FOREACH
        END IF
    END FOREACH

    IF (pos-1) < 1 THEN
        ERROR "No existen registros telefonicos asociados"
        SLEEP 3
        ERROR ""
        CLEAR FORM
    END IF 

    IF (pos-1) >= 1 THEN
        CALL SET_COUNT (pos-1)
        DISPLAY ARRAY tel TO scr_1.*

        ON KEY(INTERRUPT)
            CALL Inicializa()
            EXIT DISPLAY 

        END DISPLAY
    END IF

END FUNCTION

FUNCTION Modifica()
#m-----------------

    DEFINE 
        i   SMALLINT,
        pos SMALLINT

    DEFINE ban_rdn DATE

    DISPLAY "" at 1,1
    DISPLAY "" AT 2,1
    DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE)
    DISPLAY "[ Esc ] Graba     [ Ctrl-C ] Salir sin Grabar  " 
            AT 1,1 ATTRIBUTE(BOLD)

    DISPLAY BY NAME g_afili.*

    LET pos = 1

    DECLARE cur_m CURSOR FOR
    SELECT t.tel_cod  ,
           d.tel_desc ,
           t.pais_cod ,
           p.pais_desc,
           t.cve_lada ,
           t.telefono ,
           t.extension,
           t.rowid,
           t.tel_hora_ini,                ---28-18 
           t.tel_tp_hora_ini,             ---28-18 
           '',                            ---28-18
           t.tel_hora_fin,                ---28-18 
           t.tel_tp_hora_fin,             ---28-18 
           '',                            ---28-18
           t.tel_dia,                     ---28-18 
           ''                             ---28-18
    FROM   afi_telefono  t,
           tab_telefono d,
    OUTER (tab_lada0      p)
    WHERE  t.nss      = g_afili.n_seguro
    AND    t.n_folio  = g_afili.n_folio
    AND    t.tipo_solicitud = g_afili.tipo_solicitud
    AND    d.tel_cod  = t.tel_cod
    AND    p.pais_cod = t.pais_cod

    FOREACH cur_m INTO tel[pos].*
--->28-18
        CASE tel[pos].tipo_hora_ini
           WHEN 1
              LET tel[pos].desc_tipo_hora_ini = 'AM'
           WHEN 2
              LET tel[pos].desc_tipo_hora_ini = 'PM'
         END CASE

        CASE tel[pos].tipo_hora_fin
           WHEN 1
              LET tel[pos].desc_tipo_hora_fin = 'AM'
           WHEN 2
              LET tel[pos].desc_tipo_hora_fin = 'PM'
         END CASE

        CASE tel[pos].dia
           WHEN 1
              LET tel[pos].desc_dia = 'de Lunes a Viernes'
           WHEN 2
              LET tel[pos].desc_dia = 'Sabado y Domingo'
           WHEN 3
              LET tel[pos].desc_dia = 'de Lunes a Domingo'
         END CASE
---<28-18
        LET pos = pos + 1

        IF pos > 8 THEN
            ERROR "Fue sobrepasada la capacidad maxima del arreglo"
            SLEEP 2
            EXIT FOREACH
        END IF
    END FOREACH

    IF (pos-1) < 1 THEN
        ERROR "No existen registros telefonicos asociados"
        SLEEP 3
        ERROR ""
        CLEAR FORM
    END IF 

    IF (pos-1) >= 1 THEN
        CALL SET_COUNT (pos-1)
    
        INPUT ARRAY tel WITHOUT DEFAULTS FROM scr_1.*  

        BEFORE FIELD tel_cod
            LET arr = ARR_CURR()
            LET src = SCR_LINE()

        AFTER FIELD tel_cod
            IF tel[arr].tel_cod IS NULL THEN
                ERROR "Tipo de telefono NO puede ser NULO"
 	        NEXT FIELD tel_cod
            END IF

            SELECT tel_desc
	    INTO   tel[arr].tel_desc
            FROM   tab_telefono
	    WHERE  tel_cod = tel[arr].tel_cod

            IF SQLCA.SQLCODE <> 0 THEN
                ERROR "Tipo de telefono no existe en catalogo"
                NEXT FIELD tel_cod
            END IF

            DISPLAY tel[arr].tel_cod TO scr_1[src].tel_cod
            DISPLAY tel[arr].tel_desc TO scr_1[src].tel_desc

        BEFORE FIELD pais_cod
            LET arr = ARR_CURR()
            LET src = SCR_LINE()

        AFTER FIELD pais_cod
            IF tel[arr].pais_cod IS NULL THEN
                LET tel[arr].pais_cod = "052"
                LET tel[arr].pais_desc = "MEXICO"
            ELSE
                SELECT pais_desc
                INTO   tel[arr].pais_desc 
                FROM   tab_lada0
                WHERE  pais_cod = tel[arr].pais_cod
            
                IF STATUS = NOTFOUND THEN
                    LET tel[arr].pais_desc = NULL
                END IF
            END IF

            DISPLAY tel[arr].pais_cod TO scr_1[src].pais_cod
            DISPLAY tel[arr].pais_desc TO scr_1[src].pais_desc
 
            --- NEXT FIELD cve_lada
--            NEXT FIELD telefono       ---erm 06 Septiembre 2007

--->erm 06 Septiembre 2007
        #IF tel[arr].tel_cod <> 4 AND
        IF tel[arr].tel_cod <> 7 THEN 
            NEXT FIELD cve_lada
        ELSE
            NEXT FIELD telefono
        END IF

        AFTER FIELD cve_lada
            LET long_cve = LENGTH(tel[arr].cve_lada)
              
            #IF tel[arr].tel_cod <> 4 THEN
                IF tel[arr].cve_lada IS NULL OR
                   tel[arr].cve_lada = "   " THEN
                   ERROR "Clave lada NO puede ser Nulo "
                   NEXT FIELD cve_lada
                ELSE
                   FOR i = 1 TO long_cve
                       IF tel[arr].cve_lada[i] <> '1' AND
                          tel[arr].cve_lada[i] <> '2' AND
                          tel[arr].cve_lada[i] <> '3' AND
                          tel[arr].cve_lada[i] <> '4' AND
                          tel[arr].cve_lada[i] <> '5' AND
                          tel[arr].cve_lada[i] <> '6' AND
                          tel[arr].cve_lada[i] <> '7' AND
                          tel[arr].cve_lada[i] <> '8' AND
                          tel[arr].cve_lada[i] <> '9' AND
                          tel[arr].cve_lada[i] <> '0' THEN
                           ERROR "Clave LADA debe ser numerica "
                           NEXT FIELD cve_lada
                       END IF
                   END FOR

                   IF long_cve <> 2 AND
                      long_cve <> 3 THEN
                       ERROR "Clave lada debe ser de 2 o 3 pisiciones "
                       NEXT FIELD cve_lada
                   ELSE
                       SELECT UNIQUE('X')
                       FROM tab_tel_numeracion
                       WHERE cld = tel[arr].cve_lada
                       IF SQLCA.SQLCODE = NOTFOUND THEN
                           ERROR "Clave Lada No existe en el catalogo de COFETEL "
                           NEXT FIELD cve_lada
                       END IF
                   END IF
               END IF
            #END IF

            FOR i = 1 TO long_cve
                IF tel[arr].cve_lada[i] <> '1' AND
                   tel[arr].cve_lada[i] <> '2' AND
                   tel[arr].cve_lada[i] <> '3' AND
                   tel[arr].cve_lada[i] <> '4' AND
                   tel[arr].cve_lada[i] <> '5' AND
                   tel[arr].cve_lada[i] <> '6' AND
                   tel[arr].cve_lada[i] <> '7' AND
                   tel[arr].cve_lada[i] <> '8' AND
                   tel[arr].cve_lada[i] <> '9' AND
                   tel[arr].cve_lada[i] <> '0' THEN
                    ERROR "Clave LADA solo contiene digitos"
                    NEXT FIELD cve_lada
                END IF
            END FOR
---<

        {AFTER FIELD cve_lada
            LET long_cve = LENGTH(tel[arr].cve_lada)

            FOR i = 1 TO long_cve
                IF tel[arr].cve_lada[i] <> '1' AND
                   tel[arr].cve_lada[i] <> '2' AND
                   tel[arr].cve_lada[i] <> '3' AND
                   tel[arr].cve_lada[i] <> '4' AND
                   tel[arr].cve_lada[i] <> '5' AND
                   tel[arr].cve_lada[i] <> '6' AND
                   tel[arr].cve_lada[i] <> '7' AND
                   tel[arr].cve_lada[i] <> '8' AND
                   tel[arr].cve_lada[i] <> '9' AND
                   tel[arr].cve_lada[i] <> '0' THEN
                    ERROR "Clave LADA solo contiene digitos"
                    NEXT FIELD cve_lada
                END IF
            END FOR

            DISPLAY tel[arr].cve_lada TO scr_1[src].cve_lada

            NEXT FIELD telefono
}
        BEFORE FIELD telefono
            LET arr = ARR_CURR()
            LET src = SCR_LINE()
--->erm 06 Septiembre 2007
            #IF tel[arr].tel_cod = 4 AND
            {IF tel[arr].cve_lada IS NOT NULL THEN
                INITIALIZE tel[arr].cve_lada TO NULL
                DISPLAY tel[arr].cve_lada TO scr_1[src].cve_lada
            END IF}
---<

        AFTER FIELD telefono
            IF tel[arr].telefono IS NULL THEN
                ERROR "Tipo de telefono NO puede ser NULO"
 	        NEXT FIELD telefono
            END IF
--->erm 06 Septiembre 2007
            LET long_tel = LENGTH(tel[arr].telefono)
            LET long_cve = LENGTH(tel[arr].cve_lada)

            IF tel[arr].tel_cod    <> 7 THEN
               #IF tel[arr].tel_cod <> 4 THEN
                  IF long_cve      =  2 THEN
                     IF long_tel   <> 8 THEN
                         ERROR "Longitud del Numero Telefonico incluyendo clave lada debe ser de 10 posiciones"
                         SLEEP 3
                         NEXT FIELD telefono
                     ELSE
                         LET vserie = tel[arr].telefono[1,4]
                         SELECT 'X'
                         FROM  tab_tel_numeracion
                         WHERE serie = vserie
                         AND   cld   = tel[arr].cve_lada
                         GROUP BY 1
                         IF SQLCA.SQLCODE = NOTFOUND THEN
                            ERROR "Lada y Serie de 4 posiciones no existe en el catalogo de COFETEL "
                            SLEEP 3
                            ERROR ""
                            NEXT FIELD telefono
                         END IF
                     END IF
                  END IF
               --END IF

                  IF long_cve      =  3 THEN
                     IF long_tel   <> 7 THEN
                         ERROR "Longitud del Numero Telefonico incluyendo clave lada debe ser de 10 posiciones"
                         SLEEP 3
                         NEXT FIELD telefono
                     ELSE
                         LET vserie = tel[arr].telefono[1,3]
                         SELECT 'X'
                         FROM  tab_tel_numeracion
                         WHERE serie = vserie
                         AND   cld   = tel[arr].cve_lada
                         GROUP BY 1
                         IF SQLCA.SQLCODE = NOTFOUND THEN
                            ERROR "Lada y Serie de 3 posiciones no existe en el catalogo de COFETEL "
                            SLEEP 3
                            ERROR ""
                            NEXT FIELD telefono
                         END IF
                     END IF
                  END IF
               {ELSE
                   IF long_tel <> 13 THEN
                      ERROR "Longitud del numero de Celular invalido"
                      SLEEP 3
                      NEXT FIELD telefono
                   ELSE
                      IF tel[arr].telefono[1,3] <> '044' AND
                         tel[arr].telefono[1,3] <> '045' THEN
                          ERROR "Inicio de celular debe ser 044 o 045 segun sea el caso "
                          NEXT FIELD telefono
                      ELSE 
                          IF tel[arr].telefono[1,3] = '044' THEN
                              IF tel[arr].telefono[4,5] <> '55' THEN
                                 ERROR "Posiciones 4 y 5 del telefono invalidas "
                                 NEXT FIELD telefono
                              END IF
                          ELSE
                              IF tel[arr].telefono[4,5] = '55' THEN
                                 ERROR "Posiciones 4 y 5 del telefono invalidas "
                                 NEXT FIELD telefono
                              END IF
                          END IF
                      END IF
                   END IF
               END IF}
            END IF
---<

            NEXT FIELD extension

        BEFORE FIELD extension
--->28-18
        AFTER FIELD extension
            --IF g_afili.tipo_solicitud <> 2 THEN
               LET tel[arr].hora_ini           = '00:00'
               LET tel[arr].tipo_hora_ini      = '1'
               LET tel[arr].desc_tipo_hora_ini = 'AM'
               LET tel[arr].hora_fin           = '00:00'
               LET tel[arr].tipo_hora_fin      = '1'
               LET tel[arr].desc_tipo_hora_fin = 'AM'
               LET tel[arr].dia                = 1

               DISPLAY tel[arr].hora_ini TO scr_1[src].hora_ini
               DISPLAY tel[arr].tipo_hora_ini TO scr_1[src].tipo_hora_ini
               DISPLAY tel[arr].desc_tipo_hora_ini TO
               scr_1[src].desc_tipo_hora_ini

               DISPLAY tel[arr].hora_fin TO scr_1[src].hora_fin
               DISPLAY tel[arr].tipo_hora_fin TO scr_1[src].tipo_hora_fin
               DISPLAY tel[arr].desc_tipo_hora_fin TO
               scr_1[src].desc_tipo_hora_fin
               DISPLAY tel[arr].dia TO scr_1[src].dia
               NEXT FIELD dia
            --END IF

        BEFORE FIELD hora_ini
            LET arr = ARR_CURR()
            LET src = SCR_LINE()
            --IF g_afili.tipo_solicitud <> 2 THEN
               NEXT FIELD dia
            --END IF

        AFTER FIELD hora_ini
           --IF g_afili.tipo_solicitud <> 2 THEN
               NEXT FIELD dia
           --END IF

           IF tel[arr].hora_ini IS NULL THEN
               ERROR "ingrese la hora inicial para localizar al trabajador "
               NEXT FIELD hora_ini
           ELSE
               IF tel[arr].hora_ini > "12:00" THEN
                  LET tel[arr].tipo_hora_ini = 2
                  LET tel[arr].desc_tipo_hora_ini = 'PM'
                  DISPLAY tel[arr].tipo_hora_ini TO scr_1[src].tipo_hora_ini
                  DISPLAY tel[arr].desc_tipo_hora_ini  TO scr_1[src].desc_tipo_hora_ini
                  NEXT FIELD hora_fin
               ELSE
                  LET tel[arr].tipo_hora_ini = 1
                  LET tel[arr].desc_tipo_hora_ini = 'AM'
                  --NEXT FIELD tipo_hora_ini
                  DISPLAY tel[arr].tipo_hora_ini TO scr_1[src].tipo_hora_ini
                  DISPLAY tel[arr].desc_tipo_hora_ini  TO scr_1[src].desc_tipo_hora_ini
                  NEXT FIELD hora_fin
               END IF
           END IF

        BEFORE FIELD hora_fin
            LET arr = ARR_CURR()
            LET src = SCR_LINE()
            --IF g_afili.tipo_solicitud <> 2 THEN
               NEXT FIELD dia
            --END IF

        AFTER FIELD hora_fin
           --IF g_afili.tipo_solicitud <> 2 THEN
               NEXT FIELD dia
           --END IF

           IF tel[arr].hora_fin IS NULL THEN
               ERROR "ingrese la hora maxima para localizar al trabajador "
               NEXT FIELD hora_fin
           ELSE
               IF tel[arr].hora_fin > "12:00" THEN
                  LET tel[arr].tipo_hora_fin = 2
                  LET tel[arr].desc_tipo_hora_fin = 'PM'
                  DISPLAY tel[arr].tipo_hora_fin TO scr_1[src].tipo_hora_fin
                  DISPLAY tel[arr].desc_tipo_hora_fin  TO scr_1[src].desc_tipo_hora_fin
                  NEXT FIELD dia
               ELSE
                  LET tel[arr].tipo_hora_fin = 1
                  LET tel[arr].desc_tipo_hora_fin = 'AM'
                  DISPLAY tel[arr].tipo_hora_fin TO scr_1[src].tipo_hora_fin
                  DISPLAY tel[arr].desc_tipo_hora_fin  TO scr_1[src].desc_tipo_hora_fin
                  NEXT FIELD dia
               END IF
           END IF

        BEFORE FIELD dia
            LET arr = ARR_CURR()
            LET src = SCR_LINE()

        AFTER FIELD dia
          {IF g_afili.tipo_solicitud = 2 THEN
          IF tel[arr].dia IS NULL THEN
             ERROR "Ingrese el codigo para el rango de dias en que se localizará al trabajador"
             NEXT FIELD dia
          ELSE
             CASE tel[arr].dia
                WHEN 1
                   LET tel[arr].desc_dia = 'de Lunes a Viernes'
                WHEN 2
                   LET tel[arr].desc_dia = 'Sabado o Domingo'
                WHEN 3
                   LET tel[arr].desc_dia = 'de Lunes a Domingo'
             END CASE
               DISPLAY tel[arr].desc_dia TO scr_1[src].desc_dia
--             NEXT FIELD tel_cod
          END IF
          END IF}

---<28-18
            LET arr = ARR_CURR()
            LET src = SCR_LINE()

      ON KEY ( ESC )

	 FOR i = 1 TO pos
           --->erm 06 Septiembre 2007
           IF i < pos THEN
              IF tel[i].tel_cod IS NULL THEN
                 ERROR "Codigo de telefono en el registro ", i, " no puede ser nulo  "
                 NEXT FIELD tel_cod
              ELSE
                 SELECT 'X'
                 FROM tab_telefono
                 WHERE tel_cod = tel[i].tel_cod
                 IF SQLCA.SQLCODE = NOTFOUND THEN
                    ERROR "Codigo de telefono en el registro ", i, " No existe en catalogo  "
                    NEXT FIELD tel_cod
                 END IF
              END IF

              IF tel[i].pais_cod IS NULL then
                 ERROR "Pais en el registro ", i, " no puede ser nulo "
                 SLEEP 2
                 ERROR ""
                 LET tel[i].pais_cod = '052'
                 LET tel[i].pais_desc = "MEX"
              END IF

              #IF tel[i].tel_cod <> 4 AND
                  IF tel[i].tel_cod <> 7 THEN
                     IF tel[i].cve_lada IS NULL AND
                        tel[i].cve_lada = "   " THEN
                        ERROR "Clave lada en el registro ", i, " no puede ser nulo "
                        NEXT FIELD tel_cod
                     ELSE
                        LET long_cve = LENGTH(tel[i].cve_lada)

                        SELECT UNIQUE('X')
                        FROM tab_tel_numeracion
                        WHERE cld = tel[i].cve_lada
                        IF SQLCA.SQLCODE = NOTFOUND THEN
                           ERROR "Clave lada en el registro ", i, " No existe en catalogo de COFETEL"
                           NEXT FIELD tel_cod
                        END IF

                        IF long_cve = 2 THEN
                           LET vserie = tel[i].telefono[1,4]
                           SELECT "X"
                           FROM   tab_tel_numeracion
                           WHERE  serie = vserie
                             AND  cld   = tel[i].cve_lada
                           GROUP BY 1
                           IF SQLCA.SQLCODE = NOTFOUND THEN
                              ERROR "Lada y Serie en el registro ",i,
                                    " No existe en catalogo de COFETEL."
                              SLEEP 3
                              ERROR ""
                              NEXT FIELD telefono
                           END IF
                        END IF

                        IF long_cve = 3 THEN
                           LET vserie = tel[i].telefono[1,3]
                           SELECT "X"
                           FROM   tab_tel_numeracion
                           WHERE  serie = vserie
                             AND  cld   = tel[i].cve_lada
                           GROUP BY 1
                           IF SQLCA.SQLCODE = NOTFOUND THEN
                              ERROR "Lada y Serie en el registro ",i,
                                    " No existe en catalogo de COFETEL."
                              SLEEP 3
                              ERROR ""
                              NEXT FIELD telefono
                           END IF
                        END IF
                     END IF
                  END IF

                  IF tel[i].telefono IS NULL THEN
                     ERROR "Telefono en el registro ", i, " no puede ser nulo "
                     NEXT FIELD tel_cod
                  END IF
            --END IF

              IF g_afili.tipo_solicitud = 2 THEN
                IF tel[i].hora_ini IS NULL OR                    ---28 18
                   tel[i].hora_fin IS NULL THEN                  ---28 18
                     ERROR "Ingrese la Hora Inicial u Hora Final "     ---28 18
                     NEXT FIELD hora_ini                         ---28 18
                END IF                                             ---28 18
             END IF
---<
                  IF tel[i].telefono IS NOT NULL THEN
	             UPDATE afi_telefono 
	             SET    pais_cod   = tel[i].pais_cod,
                            cve_lada   = tel[i].cve_lada,               ---erm 06 Septiembre 2007
		            extension  = tel[i].extension,
		            cve_lada   = tel[i].cve_lada,
		            telefono   = tel[i].telefono,
		            tel_cod    = tel[i].tel_cod,
                            tel_hora_ini = tel[i].hora_ini,          ---28-18
                            tel_tp_hora_ini = tel[i].tipo_hora_ini,  ---28-18
                            tel_hora_fin = tel[i].hora_fin,          ---28-18
                            tel_tp_hora_fin = tel[i].tipo_hora_fin,  ---28-18
                            tel_dia      = tel[i].dia,               ---28-18
                            usuario    = g_usuario,
                            factualiza = HOY
                     WHERE  rowid = tel[i].idrow
                  END IF
           END IF                                       ---erm 06 Septiembre 2007
	 END FOR

--->erm 06 Septiembre 2007
            ERROR "Registros actualizados"
            SLEEP 2
            ERROR ""
---<
         #NOT DOM OP 17
         IF g_afili.tipo_solicitud = 1  OR
            g_afili.tipo_solicitud = 3  OR
            g_afili.tipo_solicitud = 6  OR
            g_afili.tipo_solicitud = 8  OR
            g_afili.tipo_solicitud = 10 OR
            g_afili.tipo_solicitud = 11 OR
            g_afili.tipo_solicitud = 12 THEN

            SELECT MAX(factualiza)
            INTO   ban_rdn
            FROM   afi_ctr_domicilio
            WHERE  n_folio         = g_afili.n_folio
            AND    tipo_solicitud  = g_afili.tipo_solicitud
            AND    nss             = g_afili.n_seguro
            OR     curp            = g_afili.n_unico
            AND    status_interno IN (20,40,42)
            AND    cod_resultado   = '02'
            --GROUP BY 1
            IF ban_rdn IS NOT NULL THEN
               WHILE TRUE
               PROMPT "Desea reenviar la notificacion ",
                      "del domcilio OP 17 [S/N]: " FOR aux_pausa
               IF aux_pausa MATCHES "[SsNn]" THEN
                  IF aux_pausa MATCHES "[Ss]" THEN
                     UPDATE afi_mae_afiliado
                     SET    documento_6    = 2
                     WHERE  n_folio        = g_afili.n_folio
                     AND    tipo_solicitud = g_afili.tipo_solicitud
                  END IF
                  EXIT WHILE
               END IF
               END WHILE
            END IF
         END IF
         #NOT DOM OP 17

	 EXIT INPUT

      ON KEY ( INTERRUPT )
	 CALL Inicializa()
	 EXIT INPUT

        END INPUT
    END IF 

END FUNCTION

FUNCTION Elimina()
#e----------------

    DEFINE 
        i   SMALLINT,
        pos SMALLINT

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " CONSULTA " AT 1,67 ATTRIBUTE(REVERSE)
    DISPLAY " [ Ctrl-C ] Salir     [ Ctrl-B ] Eliminar  "
            AT 1,1 ATTRIBUTE(BOLD)      

    DISPLAY BY NAME g_afili.*

    LET pos = 1

    DECLARE cur_e CURSOR FOR
    SELECT t.tel_cod  ,
           d.tel_desc ,
           t.pais_cod ,
           p.pais_desc,
           t.cve_lada ,
           t.telefono ,
           t.extension,
           t.rowid,
           t.tel_hora_ini,                ---28-18 
           t.tel_tp_hora_ini,             ---28-18 
           '',                            ---28-18
           t.tel_hora_fin,                ---28-18 
           t.tel_tp_hora_fin,             ---28-18 
           '',                            ---28-18
           t.tel_dia,                     ---28-18 
           ''                             ---28-18 
    FROM   afi_telefono  t,
           tab_telefono d,
    OUTER (tab_lada0      p)
    WHERE  t.nss      = g_afili.n_seguro
    AND    t.n_folio  = g_afili.n_folio
    AND    t.tipo_solicitud = g_afili.tipo_solicitud
    AND    d.tel_cod  = t.tel_cod
    AND    p.pais_cod = t.pais_cod

    FOREACH cur_e INTO tel[pos].*
--->28-18
        CASE tel[pos].tipo_hora_ini
           WHEN 1
              LET tel[pos].desc_tipo_hora_ini = 'AM'
           WHEN 2
              LET tel[pos].desc_tipo_hora_ini = 'PM'
         END CASE

        CASE tel[pos].tipo_hora_fin
           WHEN 1
              LET tel[pos].desc_tipo_hora_fin = 'AM'
           WHEN 2
              LET tel[pos].desc_tipo_hora_fin = 'PM'
         END CASE

        CASE tel[pos].dia
           WHEN 1
              LET tel[pos].desc_dia = 'de Lunes a Viernes'
           WHEN 2
              LET tel[pos].desc_dia = 'Sabado y Domingo'
           WHEN 3
              LET tel[pos].desc_dia = 'de Lunes a Domingo'
         END CASE
---<28-18
        LET pos = pos + 1

        IF pos > 8 THEN
            ERROR "Fue sobrepasada la capacidad maxima del arreglo"
            SLEEP 2
            EXIT FOREACH
        END IF
    END FOREACH

    IF (pos-1) < 1 THEN
        ERROR "No existen registros telefonicos asociados"
        SLEEP 3
        ERROR ""
        CLEAR FORM
    END IF 

    IF (pos-1) >= 1 THEN
        CALL SET_COUNT (pos-1)
        DISPLAY ARRAY tel TO scr_1.*

        ON KEY ( CONTROL-b )
            LET pos = ARR_CURR()

            WHILE TRUE
                CALL Pregunta()
                IF aux_pausa  MATCHES "[SsNn]" THEN
                    IF aux_pausa MATCHES "[Nn]" THEN
                        RETURN
                    ELSE
                        EXIT WHILE
                    END IF
                ELSE
                    DISPLAY "Solo debe presionar (S)i o (N)o" AT 19,2
                END IF
            END WHILE

            IF aux_pausa MATCHES "[Ss]" THEN 
                DELETE FROM afi_telefono
                WHERE nss      = g_afili.n_seguro
                AND   n_folio  = g_afili.n_folio
                AND   tipo_solicitud = tipo_solicitud
                AND   rowid    = tel[pos].idrow

                ERROR "REGISTRO ELIMINADO" SLEEP 1 ERROR ""
            END IF

            SLEEP 2 ERROR ""
            CALL Consulta()
            EXIT DISPLAY 

        ON KEY(INTERRUPT)
            CALL Inicializa()
            EXIT DISPLAY 

        END DISPLAY
    END IF

END FUNCTION

FUNCTION Pregunta()
#p-----------------

    PROMPT "Desea eliminar el registro [S/N] ?" FOR aux_pausa

END FUNCTION
