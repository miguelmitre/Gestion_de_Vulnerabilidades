#############################################################################
#Proyecto          => AFORES ( MEXICO )                                     #
#Propietario       => E.F.P.                                                #
#Programa CTAB115  => CARGA DE ARCHIVO ORDENES SELECCION SIEFORE ACEPTADAS  #
#Sistema           => CTA                                                   #
#Autor             => MAURO MUNIZ CABALLERO                                 #
#Fecha             => 25 DE AGOSTO DE 2005                                  #
#############################################################################

DATABASE safre_af

GLOBALS

    DEFINE aux_pausa        CHAR(1)
    DEFINE enter            CHAR(1)
    DEFINE g_usuario        CHAR(8)
    DEFINE hora             CHAR(8)
    DEFINE vnti             CHAR(11)
    DEFINE generar          CHAR(20)
    DEFINE varchivo         CHAR(20)
    DEFINE vdesc_afo        CHAR(20)
    DEFINE carga            CHAR(50)
    DEFINE eje_marca        CHAR(200)
    DEFINE eje_sie          CHAR(200)
    DEFINE eje_vol          CHAR(200)
    DEFINE eje_fne          CHAR(200)
    DEFINE eje_nti          CHAR(200)
    DEFINE G_LISTA          CHAR(200)
    DEFINE G_IMPRE          CHAR(200)
    DEFINE gimpresion       CHAR(200)

    DEFINE hoy              DATE
    DEFINE vfecha_cau       DATE

    DEFINE bnd_proceso      SMALLINT
    DEFINE marca            SMALLINT
    DEFINE xcodigo_marca    SMALLINT
    DEFINE xcodigo_rechazo  SMALLINT
    DEFINE vcod_afo         SMALLINT
    DEFINE vsie_rec         SMALLINT
    DEFINE vsie_ced         SMALLINT
    DEFINE vtipo_proc       SMALLINT
    DEFINE vtipo_trasp      SMALLINT
    DEFINE vmedio           SMALLINT
    DEFINE vmarca_edo       SMALLINT
    DEFINE vmarca_cau       SMALLINT
    DEFINE vcodigo_rch      SMALLINT

    DEFINE vind_edad        SMALLINT
    DEFINE vgpo_reg         SMALLINT

    DEFINE g_plano1         INTEGER
    DEFINE vcorrelativo     INTEGER
    DEFINE cuantos          INTEGER
    DEFINE vtotal           INTEGER
    DEFINE vaprobados       INTEGER
    DEFINE vrechazados      INTEGER

    DEFINE g_seg_modulo RECORD LIKE seg_modulo.*

    DEFINE reg_bat RECORD
        pid            INTEGER,
        proceso_cod    INTEGER,
        opera_cod      INTEGER,
        nombre_archivo CHAR(25)
    END RECORD

    DEFINE r_existe    SMALLINT
    DEFINE r_edad      SMALLINT
    DEFINE r_criterio  SMALLINT
    DEFINE r_ind_edad  SMALLINT
    DEFINE v_curp      CHAR(18)
    DEFINE v_rfc       CHAR(13)
    DEFINE v_fena      DATE

END GLOBALS

MAIN

    DISPLAY " "
    DISPLAY ".1"

    CALL STARTLOG("CTAB115.log")
    CALL inicio() #i

    IF NOT bnd_proceso THEN
        DEFER INTERRUPT
        OPTIONS INPUT WRAP,
            PROMPT LINE LAST,
            ACCEPT KEY CONTROL-I

        CALL proceso_principal()   #pp
    ELSE
        CALL sube_archivo()
        CALL rescata_valores()     #rv
        CALL actualiza_operacion() #rv
    END IF

END MAIN

FUNCTION inicio()
#i---------------

    LET reg_bat.pid            = ARG_VAL(1)
    LET reg_bat.proceso_cod    = ARG_VAL(2)
    LET reg_bat.opera_cod      = ARG_VAL(3)
    LET reg_bat.nombre_archivo = ARG_VAL(4)

    LET bnd_proceso = 0
    LET marca       = 210
    LET hoy         = TODAY
    LET hora        = TIME
    LET vtipo_proc  = 1
    LET vtipo_trasp = 1
    LET vmedio      = 10
    LET vmarca_edo  = 0
    LET vmarca_cau  = 0
    LET vcodigo_rch = 0
    LET vaprobados  = 0
    LET vrechazados = 0
    LET vtotal      = 0
    LET vfecha_cau  = ""
    LET vnti        = ""

    IF reg_bat.pid THEN
        DISPLAY "INICIANDO PROCESO"
        LET bnd_proceso = 1
    END IF

    SELECT *, USER
    INTO   g_seg_modulo.*, g_usuario
    FROM   seg_modulo
    WHERE  modulo_cod = 'cta'

    SELECT t.codigo_afore, t.razon_social
      INTO vcod_afo, vdesc_afo
      FROM tab_afore_local t

    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp
        DROP TABLE safre_tmp:plano_siefore
    WHENEVER ERROR STOP

    CREATE TABLE safre_tmp:plano_siefore
        (n_registros CHAR(500))

    DATABASE safre_af

    LET eje_sie = " EXECUTE FUNCTION fn_orden_seleccion (?,?,?,?,?,?,?)"
    LET eje_fne = " EXECUTE FUNCTION fn_fnacimiento (?,?)"
    LET eje_nti = " EXECUTE FUNCTION fn_obtiene_nti (?)"

    LET G_LISTA = g_seg_modulo.ruta_listados CLIPPED,"/",
                  g_usuario CLIPPED,".ORD_SEL_ACEP.",
                  HOY USING "ddmmyyyy","_",hora[1,2],
                  hora[4,5],hora[7,8]

END FUNCTION

FUNCTION crea_tablas()
#ct-------------------

    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp
        DROP TABLE cza_siefore_acep
        DROP TABLE cza_trab_sie_ace
        DROP TABLE det_siefore_acep
        DROP TABLE sum_trab_sie_ace
        DROP TABLE sum_siefore_acep
    WHENEVER ERROR STOP

    CREATE TABLE cza_siefore_acep
        (campo1  CHAR(2),
         campo2  CHAR(2),
         campo3  CHAR(2),
         campo4  CHAR(2),
         campo5  CHAR(3),
         campo6  CHAR(2),
         campo7  CHAR(3),
         campo8  CHAR(10),
         campo9  CHAR(3),
         campo10 CHAR(2),
         campo11 CHAR(2),
         campo12 CHAR(3),
         campo13 CHAR(3),
         campo14 CHAR(3))

    CREATE TABLE cza_trab_sie_ace
        (campo1  CHAR(2),
         campo2  CHAR(2),
         campo3  CHAR(11),
         campo4  CHAR(18),
         campo5  CHAR(13),
         campo6  CHAR(10),
         campo7  CHAR(8),
         campo8  CHAR(10),
         campo9  CHAR(2),
         campo10 CHAR(3),
         campo11 CHAR(10),
         campo12 CHAR(8),
         campo13 CHAR(2),
         campo14 CHAR(3),
         campo15 CHAR(3),
         campo16 CHAR(3))

    CREATE TABLE det_siefore_acep
        (campo1  CHAR(2),
         campo2  CHAR(11),
         campo3  CHAR(18),
         campo4  CHAR(2),
         campo5  CHAR(2),
         campo6  CHAR(2),
         campo7  CHAR(8),
         campo8  CHAR(8), 
         campo9  CHAR(15),
         campo10 CHAR(15),
         campo11 CHAR(15),
         campo12 CHAR(15),
         campo13 CHAR(15),
         campo14 CHAR(3), 
         campo15 CHAR(3), 
         campo16 CHAR(3))

    CREATE TABLE sum_trab_sie_ace
        (campo1    CHAR(2),
         campo2    CHAR(11),
         campo3    CHAR(18),
         campo4    CHAR(10),
         campo5    CHAR(15))

    CREATE TABLE sum_siefore_acep
        (campo1  CHAR(2),
         campo2  CHAR(2),
         campo3  CHAR(2),
         campo4  CHAR(2),
         campo5  CHAR(3),
         campo6  CHAR(2),
         campo7  CHAR(3),
         campo8  CHAR(10),
         campo9  CHAR(15))

    DATABASE safre_af

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 4,4 WITH FORM "CTAB1151" ATTRIBUTE(BORDER)
    DISPLAY " CTAB115  CARGA ARCHIVO ORDENES SELECCION ACEPTADAS PROCESAR                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "          < Ctrl-B > Consulta archivos               < Ctrl-C > Salir          " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)

    DISPLAY g_seg_modulo.ruta_rescate  AT  7,10

    INPUT BY NAME generar
        AFTER FIELD generar

        IF generar IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD generar
        END IF

        SELECT nombre_archivo
        INTO   varchivo
        FROM   cta_ctr_arh_sie
        WHERE  nombre_archivo = generar

        IF STATUS <> NOTFOUND THEN
            ERROR "ARCHIVO YA PROCESADO"
            SLEEP 3
            ERROR ""
            INITIALIZE generar TO NULL
            CLEAR FORM
            NEXT FIELD generar
        END IF

        LET carga = NULL
        LET carga = g_seg_modulo.ruta_rescate CLIPPED,"/", generar CLIPPED

        WHENEVER ERROR CONTINUE
            LOAD FROM carga DELIMITER ","
            INSERT INTO safre_tmp:plano_siefore
        WHENEVER ERROR STOP

        SELECT COUNT(*)
        INTO   g_plano1
        FROM   safre_tmp:plano_siefore

        IF g_plano1 IS NULL OR g_plano1 = 0 THEN
            ERROR "NOMBRE DE ARCHIVO INCORRECTO O VACIO, VERIFIQUE "
            SLEEP 5
            EXIT PROGRAM
        END IF

        ERROR "Procesando Informacion"

        CALL rescata_valores() #rv

        EXIT INPUT

        ON KEY (control -b)
            CALL despliega_archivos()

    END INPUT

END FUNCTION

FUNCTION sube_archivo()
#sa--------------------

    LET carga = g_seg_modulo.ruta_rescate CLIPPED,"/",
                reg_bat.nombre_archivo CLIPPED

    SELECT nombre_archivo
    INTO   varchivo
    FROM   cta_ctr_arh_sie
    WHERE  nombre_archivo = reg_bat.nombre_archivo

    IF STATUS <> NOTFOUND THEN
        DISPLAY "Program stopped, ARCHIVO YA PROCESADO"
        EXIT PROGRAM
    END IF

    LOAD FROM carga INSERT INTO safre_tmp:plano_siefore

    SELECT count(*)
    INTO   cuantos
    FROM   safre_tmp:plano_siefore

    IF cuantos = 0 OR
       cuantos IS NULL THEN
        DISPLAY  "Program stopped, NOMBRE DE ARCHIVO INCORRECTO O VACIO"
        EXIT PROGRAM
    END IF

END FUNCTION

FUNCTION rescata_valores()
#rv-----------------------

    CALL crea_tablas()        #ct
    CALL actualiza_datos()    #ad
    CALL revisa_datos()       #rd
    CALL actualiza_tablas()   #am

END FUNCTION

FUNCTION actualiza_datos()
#ad-----------------------

    DEFINE cont_reg   INTEGER
    DEFINE total_reg  INTEGER
    DEFINE carga_reg  CHAR(550)

    DEFINE
        campo1001 CHAR(2),
        campo1002 CHAR(2),
        campo1003 CHAR(2),
        campo1004 CHAR(2),
        campo1005 CHAR(3),
        campo1006 CHAR(2),
        campo1007 CHAR(3),
        campo1008 CHAR(10),
        campo1009 CHAR(3),
        campo1010 CHAR(2),
        campo1011 CHAR(2),
        campo1012 CHAR(3),
        campo1013 CHAR(3),
        campo1014 CHAR(3),

        campo1101 CHAR(2),
        campo1102 CHAR(2),
        campo1103 CHAR(11),
        campo1104 CHAR(18),
        campo1105 CHAR(13),
        campo1106 CHAR(10),
        campo1107 CHAR(8),
        campo1108 CHAR(10),
        campo1109 CHAR(2),
        campo1110 CHAR(3),
        campo1111 CHAR(10),
        campo1112 CHAR(8),
        campo1113 CHAR(2),
        campo1114 CHAR(3),
        campo1115 CHAR(3),
        campo1116 CHAR(3),

        campo201  CHAR(2) ,
        campo202  CHAR(11),
        campo203  CHAR(18),
        campo204  CHAR(2) ,
        campo205  CHAR(2) ,
        campo206  CHAR(2) ,
        campo207  CHAR(8) ,
        campo208  CHAR(8) ,
        campo209  CHAR(15),
        campo210  CHAR(15),
        campo211  CHAR(15),
        campo212  CHAR(15),
        campo213  CHAR(15),
        campo214  CHAR(3) ,
        campo215  CHAR(3) ,
        campo216  CHAR(3) ,

        campo901  CHAR(2),
        campo902  CHAR(10),
        campo903  CHAR(15),

        campo911  CHAR(2),
        campo912  CHAR(2),
        campo913  CHAR(2),
        campo914  CHAR(2),
        campo915  CHAR(3),
        campo916  CHAR(2),
        campo917  CHAR(3),
        campo918  CHAR(10),
        campo919  CHAR(15)

    LET cont_reg = 0

    SELECT COUNT(*)
    INTO   total_reg
    FROM   safre_tmp:plano_siefore

    DECLARE cursor_1 CURSOR FOR
    SELECT *
      FROM  safre_tmp:plano_siefore

    FOREACH cursor_1 INTO carga_reg
        LET cont_reg = cont_reg + 1

        IF carga_reg[1,2] = '01' THEN
            LET campo1001  = carga_reg[001,002]
            LET campo1002  = carga_reg[003,004]
            LET campo1003  = carga_reg[005,006]
            LET campo1004  = carga_reg[007,008]
            LET campo1005  = carga_reg[009,011]
            LET campo1006  = carga_reg[012,013]
            LET campo1007  = carga_reg[014,016]
            LET campo1008  = carga_reg[017,024]
            LET campo1009  = carga_reg[025,027]
            LET campo1010  = carga_reg[028,029]
            LET campo1011  = carga_reg[030,031]
            LET campo1012  = carga_reg[032,034]
            LET campo1013  = carga_reg[035,037]
            LET campo1014  = carga_reg[038,040]

            LET campo1008 = campo1008[5,6],"/",
                            campo1008[7,8],"/",
                            campo1008[1,4]

            INSERT INTO safre_tmp:cza_siefore_acep
            VALUES (campo1001,
                    campo1002,
                    campo1003,
                    campo1004,
                    campo1005,
                    campo1006,
                    campo1007,
                    campo1008,
                    campo1009,
                    campo1010,
                    campo1011,
                    campo1012,
                    campo1013,
                    campo1014)
        END IF

        IF carga_reg[1,2] = '02' THEN
            LET campo1101  = carga_reg[001,002]
            LET campo1102  = carga_reg[003,004]
            LET campo1103  = carga_reg[005,015]
            LET campo1104  = carga_reg[016,033]
            LET campo1105  = carga_reg[034,046]
            LET campo1106  = carga_reg[047,054]
            LET campo1107  = carga_reg[055,060]
            LET campo1108  = carga_reg[061,070]
            LET campo1109  = carga_reg[071,072]
            LET campo1110  = carga_reg[073,075]
            LET campo1111  = carga_reg[076,083]
            LET campo1112  = carga_reg[084,089]
            LET campo1113  = carga_reg[090,091]
            LET campo1114  = carga_reg[092,094]
            LET campo1115  = carga_reg[095,097]
            LET campo1116  = carga_reg[098,100]

            LET campo1106 = campo1106[5,6],"/",
                            campo1106[7,8],"/",
                            campo1106[1,4]

            LET campo1107 = campo1107[1,2],":",
                            campo1107[3,4],":",
                            campo1107[5,6]

          {
            LET campo1111 = campo1111[5,6],"/",
                            campo1111[7,8],"/",
                            campo1111[1,4]

            LET campo1112 = campo1112[1,2],":",
                            campo1112[3,4],":",
                            campo1112[5,6]
          }

            INSERT INTO safre_tmp:cza_trab_sie_ace
            VALUES (campo1101,
                    campo1102,
                    campo1103,
                    campo1104,
                    campo1105,
                    campo1106,
                    campo1107,
                    campo1108,
                    campo1109,
                    campo1110,
                    campo1111,
                    campo1112,
                    campo1113,
                    campo1114,
                    campo1115,
                    campo1116)

        END IF

        IF carga_reg[1,2] = '03' THEN
            LET campo201  = carga_reg[001,002]
            LET campo202  = carga_reg[003,013]
            LET campo203  = carga_reg[014,031]
            LET campo204  = carga_reg[032,033]
            LET campo205  = carga_reg[034,035]
            LET campo206  = carga_reg[036,037]
            LET campo207  = carga_reg[038,045]
            LET campo208  = carga_reg[046,053]
            LET campo209  = carga_reg[054,068]
            LET campo210  = carga_reg[069,083]
            LET campo211  = carga_reg[084,098]
            LET campo212  = carga_reg[099,113]
            LET campo213  = carga_reg[114,128]
            LET campo214  = carga_reg[129,131]
            LET campo215  = carga_reg[132,134]
            LET campo216  = carga_reg[135,137]

            LET campo209 = campo209 / 1000000
            LET campo210 = campo210 / 1000000
            LET campo210 = campo211 / 1000000
            LET campo212 = campo212 / 1000000
            LET campo213 = campo213 / 100

            INSERT INTO safre_tmp:det_siefore_acep
            VALUES (campo201,
                    campo202,
                    campo203,
                    campo204,
                    campo205,
                    campo206,
                    campo207,
                    campo208,
                    campo209,
                    campo210,
                    campo211,
                    campo212,
                    campo213,
                    campo214,
                    campo215,
                    campo216)

        END IF

        IF carga_reg[1,2] = '08' THEN
            LET campo901 = carga_reg[001,002]
            LET campo902 = carga_reg[003,010]
            LET campo903 = carga_reg[011,025]
                            
            LET campo902 = campo902[5,6],"/",
                           campo902[7,8],"/",
                           campo902[1,4]

           INSERT INTO safre_tmp:sum_trab_sie_ace
           VALUES (campo901,
                   campo1103,
                   campo1104,
                   campo902,
                   campo903)
        END IF

        IF carga_reg[1,2] = '09' THEN
            LET campo911 = carga_reg[001,002]
            LET campo912 = carga_reg[003,004]
            LET campo913 = carga_reg[005,006]
            LET campo914 = carga_reg[007,008]
            LET campo915 = carga_reg[009,011]
            LET campo916 = carga_reg[012,013]
            LET campo917 = carga_reg[014,016]
            LET campo918 = carga_reg[017,024]
            LET campo919 = carga_reg[025,039]
                            
            LET campo918 = campo918[5,6],"/",
                           campo918[7,8],"/",
                           campo918[1,4]

           INSERT INTO safre_tmp:sum_siefore_acep
           VALUES (campo911,
                   campo912,
                   campo913,
                   campo914,
                   campo915,
                   campo916,
                   campo917,
                   campo918,
                   campo919)

        END IF

    END FOREACH

END FUNCTION

FUNCTION revisa_datos()
#----------------------

    DEFINE
        aux_pausa    CHAR(1),
        rechazo_011  CHAR(2),
        rechazo_013  CHAR(2),
        rechazo_021  CHAR(2),
        rechazo_022  CHAR(2),
        rechazo_03   CHAR(2),
        rechazo_08   CHAR(2),
        rechazo_091  CHAR(2),
        rechazo_093  CHAR(2)

    # ENCABEZADO1 #
    SELECT campo1,
           campo3
    INTO   rechazo_011,
           rechazo_013
    FROM safre_tmp:cza_siefore_acep

    IF rechazo_011 <> "01" THEN
        IF bnd_proceso THEN
            DISPLAY "Program stopped, Tipo registro debe ser 01 en encabezado1"
            EXIT PROGRAM
        ELSE
            CLEAR SCREEN
            DISPLAY "Tipo de registro debe ser 01 en ENCABEZADO1" AT 10,1
            PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR" FOR aux_pausa
            EXIT PROGRAM
        END IF
    END IF

    IF rechazo_013 <> "80" THEN
        IF bnd_proceso THEN
            DISPLAY "Program stopped, identificador operacion dede ser 80 en encabezado1"
            EXIT PROGRAM
        ELSE
            CLEAR SCREEN
            DISPLAY "Tipo de registro debe ser 01 en ENCABEZADO1" AT 10,1
            PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR" FOR aux_pausa
            EXIT PROGRAM
        END IF
    END IF

    # ENCABEZADO2 #
    SELECT UNIQUE campo1,
           campo2
    INTO   rechazo_021,
           rechazo_022
    FROM safre_tmp:cza_trab_sie_ace

    IF rechazo_021 <> "02" THEN
        IF bnd_proceso THEN
            DISPLAY "Program stopped, Tipo registro debe ser 02 en encabezado2"
            EXIT PROGRAM
        ELSE
            CLEAR SCREEN
            DISPLAY "Tipo de registro debe ser 02 en ENCABEZADO2" AT 10,1
            PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR" FOR aux_pausa
            EXIT PROGRAM
        END IF
    END IF

    IF rechazo_022 <> "80" THEN
        IF bnd_proceso THEN
            DISPLAY "Program stopped, Tipo registro debe ser 80 en encabezado2"
            EXIT PROGRAM
        ELSE
            CLEAR SCREEN
            DISPLAY "Tipo de registro debe ser 80 en ENCABEZADO2" AT 10,1
            PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR" FOR aux_pausa
            EXIT PROGRAM
        END IF
    END IF

    # DETALLE #
    SELECT UNIQUE campo1
    INTO   rechazo_03
    FROM   safre_tmp:det_siefore_acep

    IF rechazo_03 <> "03" THEN
        IF bnd_proceso THEN
            DISPLAY "Program stopped, Tipo registro debe ser 03 en detalle"
            EXIT PROGRAM
        ELSE
            CLEAR SCREEN
            DISPLAY "Tipo de registro debe ser 03 en DETALLE" AT 10,1
            PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR" FOR aux_pausa
            EXIT PROGRAM
        END IF
    END IF

    # SUMARIO1 #
    SELECT UNIQUE campo1
    INTO   rechazo_08
    FROM safre_tmp:sum_trab_sie_ace

    IF rechazo_08 <> "08" THEN
        IF bnd_proceso THEN
            DISPLAY "Program stopped, Tipo registro debe ser 80 en sumario1"
            EXIT PROGRAM
        ELSE
            CLEAR SCREEN
            DISPLAY "Tipo de registro debe ser 80 en SUMARIO1" AT 10,1
            PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR" FOR aux_pausa
            EXIT PROGRAM
        END IF
    END IF

    # SUMARIO2 #
    SELECT campo1,
           campo3
    INTO   rechazo_091,
           rechazo_093
    FROM safre_tmp:sum_siefore_acep

    IF rechazo_091 <> "09" THEN
        IF bnd_proceso THEN
            DISPLAY "Program stopped, Tipo registro debe ser 09 en sumario2"
            EXIT PROGRAM
        ELSE
            CLEAR SCREEN
            DISPLAY "Tipo de registro debe ser 09 en SUMARIO2" AT 10,1
            PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR" FOR aux_pausa
            EXIT PROGRAM
        END IF
    END IF

    IF rechazo_093 <> "80" THEN
        IF bnd_proceso THEN
            DISPLAY "Program stopped, identificador operacion dede ser 80 en suamrio2"
            EXIT PROGRAM
        ELSE
            CLEAR SCREEN
            DISPLAY "Tipo de registro debe ser 80 en SUMARIO2" AT 10,1
            PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR" FOR aux_pausa
            EXIT PROGRAM
        END IF
    END IF

END FUNCTION

FUNCTION actualiza_tablas() 
#at------------------------ 

    DEFINE vexiste SMALLINT
    DEFINE vedad   SMALLINT
    DEFINE vestado SMALLINT
    DEFINE vfolio  INTEGER
    DEFINE vresult CHAR(2)
    DEFINE vmotivo CHAR(3)
    DEFINE vhora   CHAR(8)
    DEFINE vfecha  CHAR(10)
    DEFINE nss_ant CHAR(11)

    DEFINE reg_cza1 RECORD
        campo1001 CHAR(2),
        campo1002 CHAR(2),
        campo1003 CHAR(2),
        campo1004 CHAR(2),
        campo1005 CHAR(3),
        campo1006 CHAR(2),
        campo1007 CHAR(3),
        campo1008 CHAR(10),
        campo1009 CHAR(3),
        campo1010 CHAR(2),
        campo1011 CHAR(2),
        campo1012 CHAR(3),
        campo1013 CHAR(3),
        campo1014 CHAR(3)
    END RECORD

    DEFINE reg_cza2 RECORD
        campo1101 CHAR(2),
        campo1102 CHAR(2),
        campo1103 CHAR(11),
        campo1104 CHAR(18),
        campo1105 CHAR(13),
        campo1106 CHAR(10),
        campo1107 CHAR(8),
        campo1108 CHAR(10),
        campo1109 CHAR(2),
        campo1110 CHAR(3),
        campo1111 CHAR(10),
        campo1112 CHAR(8),
        campo1113 CHAR(2),
        campo1114 CHAR(3),
        campo1115 CHAR(3),
        campo1116 CHAR(3)
    END RECORD

    DEFINE reg_det RECORD
        campo201  CHAR(2) ,
        campo202  CHAR(11),
        campo203  CHAR(18),
        campo204  CHAR(2) ,
        campo205  CHAR(2) ,
        campo206  CHAR(2) ,
        campo207  CHAR(8) ,
        campo208  CHAR(8) ,
        campo209  CHAR(15),
        campo210  CHAR(15),
        campo211  CHAR(15),
        campo212  CHAR(15),
        campo213  CHAR(15),
        campo214  CHAR(3) ,
        campo215  CHAR(3) ,
        campo216  CHAR(3)
    END RECORD

    DEFINE reg_sum1 RECORD
        campo901  CHAR(2),
        campo902  CHAR(11),
        campo903  CHAR(18),
        campo904  CHAR(10),
        campo905  CHAR(15)
    END RECORD

    DEFINE reg_sum2 RECORD
        campo911  CHAR(2),
        campo912  CHAR(2),
        campo913  CHAR(2),
        campo914  CHAR(2),
        campo915  CHAR(3),
        campo916  CHAR(2),
        campo917  CHAR(3),
        campo918  CHAR(10),
        campo919  CHAR(15)
    END RECORD

    # ENCABEZADO 1 #
    SELECT c.*
      INTO reg_cza1.*
      FROM safre_tmp:cza_siefore_acep c

    SELECT "X"
      FROM cta_cza_sie_acep
     WHERE @fecha_operacion = reg_cza1.campo1008

    IF SQLCA.SQLCODE <> 0 THEN
        INSERT INTO cta_cza_sie_acep
        VALUES(reg_cza1.*, g_usuario, hoy)
    END IF

    # ENCABEZADO 2 #
    DECLARE cur_cza2 CURSOR FOR
    SELECT e.*
      FROM safre_tmp:cza_trab_sie_ace e

    START REPORT listado TO G_LISTA

    FOREACH cur_cza2 INTO reg_cza2.*
        SELECT "X"
          FROM cta_cza_afil_acep
         WHERE @fecha_operacion = reg_cza1.campo1008
           AND @nss             = reg_cza2.campo1103
           AND @curp            = reg_cza2.campo1104
         GROUP BY 1

        IF SQLCA.SQLCODE <> 0 THEN
            INSERT INTO cta_cza_afil_acep
            VALUES(reg_cza2.*, reg_cza1.campo1008)

            # DETALLE #

            DECLARE cur_det CURSOR FOR
            SELECT td.*
              FROM safre_tmp:det_siefore_acep td
             WHERE td.campo2 = reg_cza2.campo1103

            FOREACH cur_det INTO reg_det.*

                    SELECT @codigo_siefore
                      INTO vsie_rec
                      FROM tab_siefore_local
                     WHERE @razon_social = reg_det.campo208

                    SELECT t.razon_social
                      INTO reg_det.campo207
                      FROM tab_siefore_local t
                     WHERE t.codigo_siefore IN ( SELECT c.codigo_siefore
                                                   FROM cta_nss_regimen c
                                                  WHERE c.nss           =  reg_det.campo202
                                                    AND c.grupo_regimen = reg_det.campo205 )

                    SELECT "X"
                      FROM afi_mae_afiliado A
                     WHERE A.n_seguro = reg_det.campo202

                    IF SQLCA.SQLCODE <> 0 THEN
                        PREPARE sol_nti FROM eje_nti
                        DECLARE curs4 CURSOR FOR sol_nti

                        OPEN curs4 USING reg_det.campo203
                        FETCH curs4 INTO vnti

                        CLOSE curs4

                        LET reg_det.campo202 = vnti
                    END IF  

                    PREPARE ver_edad FROM eje_fne
                    DECLARE cur_edad CURSOR FOR ver_edad

                    OPEN cur_edad USING reg_det.campo202, hoy
                    FETCH cur_edad INTO r_existe,
                                        r_edad,
                                        r_criterio,
                                        r_ind_edad,
                                        v_curp,
                                        v_rfc,
                                        v_fena

                    PREPARE sol_sie FROM eje_sie
                    DECLARE curs1 CURSOR FOR sol_sie

                    OPEN curs1 USING reg_det.campo202,
                                     r_ind_edad,
                                     reg_det.campo205,
                                     vsie_rec,
                                     vtipo_proc,
                                     vtipo_trasp,
                                     vmedio
                    FETCH curs1 INTO vexiste, vedad, vestado, vfolio

                    CLOSE curs1

                    CASE vestado
                        WHEN 0 LET vresult = '01'
                               LET vmotivo = '000'
                               LET reg_det.campo214 = vmotivo
                               LET vaprobados = vaprobados + 1

                        WHEN 2 LET vresult     = '02'
                               LET vmotivo     = '001'
                               LET reg_det.campo214 = vmotivo
                               LET vrechazados = vrechazados + 1

                        OTHERWISE LET vresult     = '02'
                                  LET vmotivo     = '002'
                                  LET reg_det.campo214 = vmotivo
                                  LET vrechazados = vrechazados + 1
                    END CASE


                    OUTPUT TO REPORT listado(reg_det.campo202, reg_det.campo203, reg_det.campo205,
                                             vresult, vmotivo, reg_cza1.campo1008)

                IF vnti IS NOT NULL THEN
                    LET reg_det.campo202 = reg_cza2.campo1103 
                END IF

                SELECT "X"
                  FROM cta_det_sie_acep
                 WHERE @nss             = reg_det.campo202
                   AND @fecha_operacion = reg_cza1.campo1008
                   AND @cve_subcta      = reg_det.campo205

                IF SQLCA.SQLCODE <> 0 THEN
                    INSERT INTO cta_det_sie_acep
                    VALUES(reg_det.*, reg_cza1.campo1008,vfolio)
                END IF

            END FOREACH

    ##ACTUALIZAR ENCABEZADO DE ACUERDO CON LOS DIAGNOSTICOS DEL DETALLE##
            SELECT 'X'
              FROM cta_det_sie_acep
             WHERE nss             = reg_det.campo202
               AND fecha_operacion = reg_cza1.campo1008
               AND diagnostico1    = '000'     # DIAGNOSTICO SOLICITUD ACEPTADA#
             GROUP BY 1

            IF SQLCA.SQLCODE <> 0 THEN
                LET vresult = '02'

                DECLARE cur_rechazo CURSOR FOR
                 SELECT c.diagnostico1
                   FROM cta_det_sie_acep c
                  WHERE c.nss             = reg_det.campo202
                    AND c.fecha_operacion = reg_cza1.campo1008

                FOREACH cur_rechazo INTO vmotivo
                    EXIT FOREACH     ## TOMAR EL PRIMER DIAGNOSTICO PARA EL ENCABEZADO
                END FOREACH
            ELSE
                LET vresult = '01'
                LET vmotivo = '000'
            END IF

            SELECT EXTEND (@fecha_inicio, HOUR TO SECOND),
                   EXTEND (fecha_inicio, YEAR TO DAY)
              INTO vhora, vfecha
              FROM cta_solicitud_regimen
             WHERE @folio_solicitud = vfolio

            LET vfecha = vfecha[6,7],"/",vfecha[9,10],"/",vfecha[1,4]

            UPDATE cta_cza_afil_acep
               SET cod_result      = vresult,
                   motivo_procede  = vmotivo,
                   fecha_ejecuta   = vfecha,
                   hora_ejecuta    = vhora
             WHERE nss             = reg_det.campo202
               AND fecha_operacion = reg_cza1.campo1008

        END IF

    END FOREACH

    # SUMARIO 1 #
    DECLARE cur_sum1 CURSOR FOR
    SELECT z.*
      FROM safre_tmp:sum_trab_sie_ace z

    FOREACH cur_sum1 INTO reg_sum1.*
        SELECT "X"
          FROM cta_sum_afil_acep
         WHERE @fecha_operacion = reg_sum1.campo904
           AND @nss            = reg_sum1.campo902
           AND @curp           = reg_sum1.campo903

        IF SQLCA.SQLCODE <> 0 THEN
            INSERT INTO cta_sum_afil_acep
            VALUES(reg_sum1.*)
        END IF
    END FOREACH

    # SUMARIO 2 #
    SELECT s.*
      INTO reg_sum2.*
      FROM safre_tmp:sum_siefore_acep s

    SELECT "X"
      FROM cta_sum_sie_acep
     WHERE @fecha_operacion = reg_sum2.campo918

    IF SQLCA.SQLCODE <> 0 THEN
        INSERT INTO cta_sum_sie_acep
        VALUES(reg_sum2.*)  
    END IF

    FINISH REPORT listado

    LET G_LISTA = "chmod 666 ",g_seg_modulo.ruta_listados CLIPPED,"/",
                  g_usuario CLIPPED,".ORD_SEL_ACEP.",
                  HOY USING "ddmmyyyy","_",hora[1,2],
                  hora[4,5],hora[7,8]

    RUN G_LISTA

    -- actualiza historico de archivos curp afi_ctr_arh_curp
    LET vtotal = vaprobados + vrechazados

    INSERT INTO cta_ctr_arh_sie
    VALUES (generar, vtotal, vaprobados, vrechazados, hoy, g_usuario)

    DISPLAY "  SOLICITUDES A PROCESAR  " AT 15,15

    DISPLAY "Total de Registros del lote     : ", 
            vtotal USING "#######&" AT 16,15
    DISPLAY "Registros aceptados    : ", 
            vaprobados USING "#######&" AT 17,15
    DISPLAY "Registros rechazados   : ", 
            vrechazados USING "#######&" AT 18,15

    LET gimpresion = "lp ",G_LISTA
    RUN gimpresion

    ERROR "LISTADO GENERADO"
    SLEEP 2

    PROMPT "Presione [Enter] para terminar" FOR enter

    RETURN

END FUNCTION

REPORT listado(vnss, vcurp, subcta, vresult, vmotivo, vfecha_op)
#l------------------------------------------------------

    DEFINE vnss       CHAR(11)
    DEFINE vcurp      CHAR(18)
    DEFINE subcta     CHAR(2)
    DEFINE vresult    CHAR(2)
    DEFINE vmotivo    CHAR(3)
    DEFINE vfecha_op  DATE
    DEFINE vcont      INTEGER

    OUTPUT
        PAGE LENGTH 90
        TOP  MARGIN 0
        BOTTOM MARGIN 0
        LEFT MARGIN 0
        RIGHT MARGIN 0

    FORMAT
        FIRST PAGE HEADER
        PRINT COLUMN 01, vcod_afo,"   ", vdesc_afo,
              COLUMN 68, hoy USING "DD/MM/YYYY"
        SKIP 2 LINE
        PRINT COLUMN 01,"-------------------------------------------------------------------------"
        PRINT COLUMN 06,"         CIFRAS CONTROL DE ORDENES DE SELECCION DE SIEFORE"
        PRINT COLUMN 01,"-------------------------------------------------------------------------"
        SKIP 2 LINE

        PRINT COLUMN 01,"-------------------------------------------------------------------------"
        PRINT
            COLUMN 06, "NSS"       ,
            COLUMN 22, "CURP"      ,
            COLUMN 34, "CVE SUBCTA",
            COLUMN 47, "RESULTADO" ,
            COLUMN 61, "F. OPERA"
        PRINT COLUMN 01,"-------------------------------------------------------------------------"

    ON EVERY ROW
       LET vcont = vcont + 1
       PRINT
           COLUMN   2, vnss,
           COLUMN  15, vcurp,
           COLUMN  38, subcta,
           COLUMN  48, vresult,
           COLUMN  51, vmotivo,
           COLUMN  60, vfecha_op USING "DD/MM/YYYY"

    ON LAST ROW
        PRINT
        PRINT COLUMN 01,"-------------------------------------------------------------------------"
        PRINT
            COLUMN 10, "TOTAL REG", vcont

END REPORT

FUNCTION despliega_archivos()
#da--------------------------

    DEFINE aux_pausa CHAR(1)
    DEFINE HOY       DATE
    DEFINE SW_1      SMALLINT

    OPTIONS
        PROMPT LINE LAST,
        INPUT WRAP,
        ACCEPT KEY control-o

        WHENEVER ERROR STOP

        LET HOY = TODAY
        OPEN WINDOW window_1 AT 2,2 WITH FORM "CTAB1152" ATTRIBUTE( BORDER)
        DISPLAY " CTAB115        CONSULTA ARCHIVOS DE SELECCION DE SIEFORE                      " AT 3,1
        ATTRIBUTE(REVERSE)
        DISPLAY HOY USING "dd-mm-yyyy" AT 3,63
        ATTRIBUTE(REVERSE)

        MENU "CONSULTA ARCHIVOS SELECCION SIEFORE"
            COMMAND "Consulta" "Consultar Archivos Seleccion Siefore"
                CALL Consulta()
            COMMAND "Salir" "Salir del Programa"
                EXIT MENU
        END MENU

        CLOSE WINDOW window_1

END FUNCTION

FUNCTION Consulta()
#c-----------------

    DEFINE ga_record   ARRAY[3000] OF RECORD
        nombre_archivo LIKE cta_ctr_arh_sie.nombre_archivo,
        total_reg      LIKE cta_ctr_arh_sie.total_reg,
        total_acep     LIKE cta_ctr_arh_sie.total_acep,
        total_rech     LIKE cta_ctr_arh_sie.total_rech,
        fecha_proceso  LIKE cta_ctr_arh_sie.factualiza,
        usuario        LIKE cta_ctr_arh_sie.usuario
    END RECORD

    DEFINE
        vcontar       INTEGER,
        vtotal        INTEGER,
        vaprobados    INTEGER,
        vrechazos     INTEGER

    DEFINE pos        SMALLINT

    SELECT count(*) INTO vcontar FROM cta_ctr_arh_sie
    SELECT sum(total_acep) INTO vaprobados FROM cta_ctr_arh_sie
    SELECT sum(total_rech) INTO vrechazos  FROM cta_ctr_arh_sie

    DISPLAY "                                                                                  " AT 1,1
    DISPLAY "    CTRL-C cancela                                                                " AT 2,1
    DISPLAY " CONSULTA " AT 2,65

    DECLARE curp_12 CURSOR FOR
    SELECT *
    FROM   cta_ctr_arh_sie
    ORDER BY 5

    LET pos = 1

    FOREACH curp_12 INTO ga_record[pos].*
        LET pos = pos + 1
    END FOREACH

    IF (pos-1) >= 1 THEN
        CALL  SET_COUNT(pos-1)
        DISPLAY BY NAME vcontar
        DISPLAY BY NAME vtotal
        DISPLAY BY NAME vaprobados
        DISPLAY BY NAME vrechazos

        DISPLAY ARRAY ga_record TO scr_1.*

        ON KEY (INTERRUPT)
            CLEAR FORM
            EXIT DISPLAY

        END DISPLAY
    ELSE
        ERROR "ARCHIVO DE ORDENES DE SELECCION VACIO"
    END IF

END FUNCTION

FUNCTION actualiza_operacion()
#ao---------------------------

    UPDATE bat_ctr_operacion
    SET    estado_operacion = 4,
           fecha_fin        = CURRENT,
           nombre_archivo   = nom_afi
    WHERE  pid              = reg_bat.pid
    AND    proceso_cod      = reg_bat.proceso_cod
    AND    opera_cod        = reg_bat.opera_cod

    UPDATE bat_ctr_proceso
    SET    estado_proceso = 4,
           fecha_fin      = CURRENT
    WHERE  pid            = reg_bat.pid
    AND    proceso_cod    = reg_bat.proceso_cod

END FUNCTION
