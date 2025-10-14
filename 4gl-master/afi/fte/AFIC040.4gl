
#Proyecto          => AFORES ( MEXICO )                                     #
#Propietario       => E.F.P.                                                #
#Programa AFIC040  => CARGA DE ARCHIVO MODIFICACION DE DATOS OP 54          #
#Sistema           => AFI                                                   #
#Autor             => EDUARDO JOAQUIN RESENDIZ MEDINA                       #
#Fecha             => 24 JUNIO  DE 2009                                     #
#Req:425           => JCPV 30/08/2011.  Pendiente x Impacto en DATAMART     #
#############################################################################
#Modificado        => JGHM - 03 Oct 2013  PST-1117                                #
#                  - Incluir validacion de registro IMSS validando solo para los  #
#                    aceptados y tipo trabajador IMSS si NSS existe en afiliados  #
#                    y esta habilitado                                            #
# Modificacion     => 24 Febrero 2015  Jairo Palafox,                       # 
#                  =>  CUO      Rechaza por falta de expediente             #
###################################################################################

DATABASE safre_af

GLOBALS

    DEFINE reg_mod       RECORD LIKE afi_mae_afiliado.*
    DEFINE reg_ant       RECORD LIKE afi_mae_modifica.*
    DEFINE reg_dom       RECORD LIKE afi_domicilio.*
    DEFINE g_paramgrales RECORD LIKE seg_modulo.*
    DEFINE reg_op54      RECORD LIKE afi_ctr_det_op54.*
    DEFINE gr_o54        RECORD LIKE afi_ctr_det_op54.*

    DEFINE
        vresp           CHAR(1),
        aux_pausa       CHAR(1),
        enter           CHAR(1),
        vcod_operacion  CHAR(2),
        vdiag_proceso   CHAR(3),
        vn_seguro       CHAR(11),
        vnti            CHAR(11),
        vnss_issste     CHAR(11),
        varchivo        CHAR(20),
        vcurp           CHAR(18),
        vcurp_mod       CHAR(18),
        vcurp_solicitud CHAR(18),
        vvcurp          CHAR(18),
        generar         CHAR(20),
        vgenerar        CHAR(20),
        operacion       CHAR(40),
        carga           CHAR(80),
        corr            CHAR(100),
        vn_folio        DECIMAL(10,0),
        vpat,vmat,vnom  CHAR(40),
        vfena_afi       DATE,
        vestadon_afi    SMALLINT,
        vsexo_afi       SMALLINT,
        vfena_compara   CHAR(10),
        vfena_compara2  CHAR(10),
        vfena_compara3  CHAR(06),
        vn_rfc          CHAR(13)

    DEFINE
        vcalle           CHAR(60),
        vcolonia         CHAR(30),
        vdelega          CHAR(30),
        vcodpos          CHAR(5),
        vdeleg_cod       INTEGER,
        vcalle_concatena CHAR(60),
        v_estado         SMALLINT,
        v_ciudad         SMALLINT

    DEFINE
        vhoy ,
        HOY,
        xx_fecha  DATE

    DEFINE
        marca     ,
        bnd_proceso SMALLINT

    DEFINE
        total_reg        ,
        cont_tot         ,
        g_plano_procesar ,
        vtotal           ,
        vaprobados       ,
        vrechazados      ,
        vpendientes      ,
        vaprobados_posdup,
        vrechazados_posdup,            
        vpendientes_posdup INTEGER

    DEFINE reg_bat RECORD
        pid            INTEGER,
        proceso_cod    INTEGER,
        opera_cod      INTEGER,
        nombre_archivo CHAR(25)
    END RECORD

    DEFINE
        vnss            CHAR(11) ,
        vmarca_entra    SMALLINT ,
        vmarca_ant      SMALLINT ,
        vmarca_estado   SMALLINT ,
        vcodigo_rechazo SMALLINT ,
        vvcodigo_rechazo SMALLINT ,
        g_usuario       CHAR(8)  ,
        ejecuta         CHAR(300),
        xcodigo_marca   SMALLINT ,
        xcodigo_rechazo SMALLINT ,
        vcodigo_marca   SMALLINT ,
        edo_proc        SMALLINT ,
        vnom_afore      CHAR(122),
        vnom_proce      CHAR(122),
        v_marca         CHAR(100),
        v_desmarca      CHAR(100)

    DEFINE
        reg_carta      RECORD LIKE safre_af:int_ctr_carta.*,
        consulta_carta CHAR(120)

    DEFINE vdia            CHAR(02)
    DEFINE vmes            CHAR(02)
    DEFINE vanio           CHAR(04)
    DEFINE vfena           CHAR(08)
    DEFINE vfena2          CHAR(10)

    DEFINE vdia_rfc        CHAR(02)
    DEFINE vmes_rfc        CHAR(02)
    DEFINE vanio_rfc       CHAR(04)
    DEFINE vfena_rfc       CHAR(08)
    DEFINE vfena_rfc2      CHAR(06)

    DEFINE vdia_curp       CHAR(02)
    DEFINE vmes_curp       CHAR(02)
    DEFINE vanio_curp      CHAR(04)
    DEFINE vfena_curp      CHAR(08)
    DEFINE vfena_curp2     CHAR(06)

    DEFINE vfelote         CHAR(08)
    DEFINE vfelote2        CHAR(10)
    DEFINE vsexo           SMALLINT
    DEFINE vedo_civil      SMALLINT
    DEFINE vestadon        CHAR(02)
    DEFINE vedocivil       CHAR(01)
    DEFINE vsexo2          CHAR(01)
    DEFINE status_int      SMALLINT
    DEFINE ban_status      SMALLINT
    DEFINE vcurp_afil      CHAR(18)
    DEFINE ban_op13        SMALLINT
    DEFINE vtipo_solicitud SMALLINT
    DEFINE vvtipo_solicitud SMALLINT
    DEFINE vcadena         CHAR(17)
    DEFINE vconsecutivo    SMALLINT
    DEFINE vn_folio_arch   INTEGER
    DEFINE vrech_int       SMALLINT
    DEFINE ban_600         SMALLINT

    DEFINE
        campo_011     CHAR(02),
        campo_012     CHAR(02),
        campo_013     CHAR(02),
        campo_014     CHAR(02),
        campo_015     CHAR(03),
        campo_016     CHAR(02),
        campo_017     CHAR(03),
        campo_018     CHAR(08),
        campo_019     CHAR(03),
        campo_110     CHAR(08),
        campo_111     CHAR(01),
        campo_112     CHAR(09),

        campo_01      CHAR(02),
        campo_02      CHAR(01),
        campo_03      CHAR(12),
        campo_04      CHAR(40),
        campo_05      CHAR(07),
        campo_06      CHAR(05),
        campo_07      CHAR(05),
        campo_08      CHAR(13),
        campo_09      CHAR(18),
        campo_10      CHAR(11),
        campo_11      CHAR(40),
        campo_12      CHAR(40),
        campo_13      CHAR(40),
        campo_14      CHAR(13),
        campo_15      CHAR(18),
        campo_16      CHAR(11),
        campo_17      CHAR(40),
        campo_18      CHAR(40),
        campo_19      CHAR(40),
        campo_20      CHAR(08),
        campo_21      SMALLINT,
        campo_22      CHAR(01),
        campo_23      SMALLINT,
        campo_24      CHAR(60),
        campo_25      CHAR(30),
        campo_26      CHAR(30),
        campo_27      CHAR(05),
--        campo_28      CHAR(24),
        campo_28      CHAR(1),
        --campo_29      CHAR(100),
        campo_29      CHAR(23),
        --campo_30      CHAR(02),
        --campo_31      CHAR(03),
        --campo_32      CHAR(03),
        --campo_33      CHAR(03)      
        campo_30      CHAR(100),
        campo_31      CHAR(02),
        campo_32      CHAR(03),
        campo_33      CHAR(03),
        campo_34      CHAR(03)


    DEFINE reg_det RECORD
         tipo_registro       CHAR(02),
         patron_tipo_trab    CHAR(01),
         patron_rfc_ent      CHAR(12),
         patron_nombre_ent   CHAR(40),
         patron_id_centro_pago CHAR(7),
         patron_cve_ramo     CHAR(05),
         patron_vce_pagadu   CHAR(05),
         rfc_ant             CHAR(13),
         curp_ant            CHAR(18),
         nsi_ant             CHAR(11),
         paterno_ant         CHAR(40),
         materno_ant         CHAR(40),
         nombres_ant         CHAR(40),
         rfc_mod             CHAR(13),
         curp_mod            CHAR(18),
         nsi_mod             CHAR(11),
         paterno_mod         CHAR(40),
         materno_mod         CHAR(40),
         nombres_mod         CHAR(40),
         fena                CHAR(08),
         estadon             SMALLINT,
         sexo                CHAR(01),
         edocivil            SMALLINT,
         domicilio_mod       CHAR(60),
         colonia             CHAR(30),
         delegacion          CHAR(30),
         codpos              CHAR(05),
         tipo_movimiento     CHAR(01),   --
         cod_resul_op        CHAR(02),
         diag_proc1          CHAR(03),
         diag_proc2          CHAR(03),
         diag_proc3          CHAR(03)
    END RECORD

    DEFINE vsec_pension      CHAR(02)                              #425
    DEFINE arr_pant_pen ARRAY[1500] OF RECORD                      #425
         curp     char(18),
         nombre   char(40),
         acepta   char(01),
         rechaza  char(01),
         diagnos  char(01)
     END RECORD

    DEFINE r_mod  RECORD                                          #425
     curp_ant        CHAR(18),
     nombres_ant     CHAR(20),
     paterno_ant     CHAR(20),
     materno_ant     CHAR(20),
     marca_peis      CHAR(03),
     fena            DATE,
     rfc_ant         CHAR(13),
     fentcons        DATE,
     curp_modificado CHAR(18),
     nombres_modificado CHAR(20),
     paterno_modificado CHAR(20),
     materno_modificado CHAR(20),
     fena_modificado    DATE,
     rfc_modificado     CHAR(13),
     curp               CHAR(18),
     nombre_datamart    CHAR(20),
     paterno_datamart   CHAR(20),
     materno_datamart   CHAR(18),
     regimen            CHAR(02),
     fecha_resolucion   DATE,
     diag_datamart      CHAR(05),
     acepta             CHAR(01),
     rechaza            CHAR(01) 
    END RECORD 
	  DEFINE reg_exp       RECORD  --CUO13
          con_exp       SMALLINT,
          sin_exp       SMALLINT
    END RECORD
END GLOBALS

MAIN

    DISPLAY " "
    DISPLAY ".1"

    CALL STARTLOG('AFIC040.log')
    CALL inicio() #i

    IF NOT bnd_proceso THEN
        DEFER INTERRUPT
            OPTIONS INPUT WRAP,
            PROMPT LINE LAST,
            ACCEPT KEY CONTROL-I

        CALL ventana_proceso()                                   #425
    ELSE
        CALL rescata_valores()     #rv
        CALL actualiza_operacion() #rv
    END IF

END MAIN

FUNCTION inicio()
#i---------------

    LET reg_bat.pid         = ARG_VAL(1)
    LET reg_bat.proceso_cod = ARG_VAL(2)
    LET reg_bat.opera_cod   = ARG_VAL(3)
    LET bnd_proceso         = 0

    IF reg_bat.pid THEN
        DISPLAY "INICIANDO PROCESO ..."
        LET bnd_proceso = 1
    END IF

    LET HOY      = TODAY
    LET marca    = 600
    --LET edo_proc = 605    ----borrar edo_proc
    LET edo_proc = 600
    LET generar  = "S"

    LET vmarca_estado   = 0
    LET vcodigo_rechazo = 0

    SELECT *, USER
    INTO   g_paramgrales.*, g_usuario
    FROM   seg_modulo
    WHERE  modulo_cod = 'afi'

    CREATE TEMP TABLE plano_procesar
        (n_registros CHAR(700))

    --LET v_marca    = " EXECUTE PROCEDURE marca_cuenta ( ?,?,?,?,?,?,?,? )"

    LET v_desmarca = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"

END FUNCTION

FUNCTION crea_tablas()
#ct-------------------

    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp

        DROP TABLE cza_mod_op54
        DROP TABLE det_mod_op54
        DROP TABLE tmp_det_mod_op54
    WHENEVER ERROR STOP

    CREATE TABLE cza_mod_op54
        (campo1  CHAR(2),
         campo2  CHAR(2),
         campo3  CHAR(2),
         campo4  CHAR(2),
         campo5  CHAR(3),
         campo6  CHAR(2),
         campo7  CHAR(3),
         campo8  CHAR(8),
         campo9  CHAR(3),
         campo10 CHAR(8),
         campo11 CHAR(1),
         campo12 CHAR(9))

     CREATE TABLE det_mod_op54
        (tipo_registro       CHAR(02),
         patron_tipo_trab    CHAR(01),
         patron_rfc_ent      CHAR(12),
         patron_nombre_ent   CHAR(40),
         patron_id_centro_pago CHAR(7),
         patron_cve_ramo     CHAR(05),
         patron_vce_pagadu   CHAR(05),
         rfc_ant             CHAR(13),
         curp_ant            CHAR(18),
         nsi_ant             CHAR(11),
         paterno_ant         CHAR(40),
         materno_ant         CHAR(40),
         nombres_ant         CHAR(40),
         rfc_mod             CHAR(13),
         curp_mod            CHAR(18),
         nsi_mod             CHAR(11),
         paterno_mod         CHAR(40),
         materno_mod         CHAR(40),
         nombres_mod         CHAR(40),
         fena                CHAR(08),
         estadon             SMALLINT,
         sexo                CHAR(01),
         edocivil            SMALLINT,
         domicilio_mod       CHAR(60),
         colonia             CHAR(30),
         delegacion          CHAR(30),
         codpos              CHAR(05),
         tipo_movimiento     CHAR(01),   --
         cod_resul_op        CHAR(02),
         diag_proc1          CHAR(03),
         diag_proc2          CHAR(03),
         diag_proc3          CHAR(03));

     CREATE TABLE tmp_det_mod_op54
        (tipo_registro       CHAR(02),
         patron_tipo_trab    CHAR(01),
         patron_rfc_ent      CHAR(12),
         patron_nombre_ent   CHAR(40),
         patron_id_centro_pago CHAR(7),
         patron_cve_ramo     CHAR(05),
         patron_vce_pagadu   CHAR(05),
         rfc_ant             CHAR(13),
         curp_ant            CHAR(18),
         nsi_ant             CHAR(11),
         paterno_ant         CHAR(40),
         materno_ant         CHAR(40),
         nombres_ant         CHAR(40),
         rfc_mod             CHAR(13),
         curp_mod            CHAR(18),
         nsi_mod             CHAR(11),
         paterno_mod         CHAR(40),
         materno_mod         CHAR(40),
         nombres_mod         CHAR(40),
         fena                CHAR(08),
         estadon             SMALLINT,
         sexo                CHAR(01),
         edocivil            SMALLINT,
         domicilio_mod       CHAR(60),
         colonia             CHAR(30),
         delegacion          CHAR(30),
         codpos              CHAR(05),
         tipo_movimiento     CHAR(01),   --
         cod_resul_op        CHAR(02),
         diag_proc1          CHAR(03),
         diag_proc2          CHAR(03),
         diag_proc3          CHAR(03));


    DATABASE safre_af

END FUNCTION
----  425 ---->
FUNCTION ventana_proceso()

    OPEN WINDOW ventana_0 AT 4,4 WITH 4 ROWS,72 COLUMNS ATTRIBUTE( BORDER)
    DISPLAY "                   OPERACION 54                    " AT 3,11 ATTRIBUTE (REVERSE)
    DISPLAY " AFIC404  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING " dd-mm-yyyy " AT 3,60 ATTRIBUTE(REVERSE)

    MENU "MENU "
        COMMAND "CARGA    " "Carga Archivo Operacion 54"
            CALL proceso_principal()
        COMMAND "PENDIENTES" "Diagnostica Registros Pendientes"                
            CALL diagnostica_pendientes()
        COMMAND "Salir" "Salir del Programa"
            EXIT MENU
    END MENU

    CLOSE WINDOW ventana_0

END FUNCTION     ---ventana_proceso() 
---- 425 <----
FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 4,4 WITH FORM "AFIC0401" ATTRIBUTE(BORDER)
    DISPLAY " AFIC040    CARGA DE ARCHIVO RESPUESTA MODIF POR DEPENDENCIA ISSSTE OP 54          " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                             Ctrl-C > Salir                                        " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 1,63 ATTRIBUTE(REVERSE)

    LET int_flag = FALSE

    DISPLAY g_paramgrales.ruta_rescate AT 6,10

    INPUT BY NAME generar

    AFTER FIELD generar
        IF generar IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD generar
        END IF

        LET vgenerar = generar CLIPPED

        SELECT nombre_archivo
        INTO   varchivo
        FROM   afi_ctr_arh_proc
        WHERE  nombre_archivo = vgenerar
        GROUP BY 1

        IF STATUS <> NOTFOUND THEN
            ERROR "ARCHIVO YA PROCESADO"
            SLEEP 2
            ERROR " "
            INITIALIZE generar TO NULL
            CLEAR FORM
            NEXT FIELD generar
        END IF

        LET carga = NULL
        LET carga = g_paramgrales.ruta_rescate CLIPPED,"/", generar CLIPPED

        WHENEVER ERROR CONTINUE
            LOAD FROM carga      --DELIMITER ","
            INSERT INTO plano_procesar
        WHENEVER ERROR STOP

        SELECT COUNT(*)
        INTO   g_plano_procesar
        FROM   plano_procesar

        IF g_plano_procesar IS NULL OR
           g_plano_procesar = 0 THEN
            ERROR "NOMBRE DE ARCHIVO INCORRECTO O VACIO, VERIFIQUE"
            SLEEP 3
            EXIT PROGRAM
        END IF

        ERROR "Procesando Informacion"

        CALL rescata_valores()

        EXIT PROGRAM

        ON KEY (INTERRUPT)
            IF int_flag = TRUE THEN
               EXIT INPUT
            END IF
        ON KEY (CONTROL - C)
            IF int_flag = TRUE THEN
               EXIT INPUT
            END IF
        END INPUT 

        IF int_flag = TRUE THEN
           LET int_flag = FALSE
           ERROR "   CARGA CANCELADA...   "
           SLEEP 2
           ERROR ""
           CLEAR SCREEN
           CLOSE WINDOW ventana_1
        END IF

    --END INPUT

END FUNCTION     #proceso_principal()
---- 425 ---->
FUNCTION diagnostica_pendientes()

   DEFINE vfecha_carga    DATE
   DEFINE vacepta         INTEGER
   DEFINE vrechaza        INTEGER
   DEFINE vpendientes     INTEGER

   LET   vacepta = 0
   LET   vrechaza = 0
   LET   vpendientes = 0

   SELECT UNIQUE(factualiza)
   INTO   vfecha_carga
   FROM afi_ctr_det_op54
   WHERE  factualiza = TODAY

   IF  STATUS = NOTFOUND  THEN
     PROMPT "NO EXISTE ARCHIVO OPERACION 54 PARA HOY <enter> para salir" FOR enter
     RETURN
    ELSE
     SELECT COUNT(*)
     INTO vacepta
     FROM afi_ctr_det_op54
     WHERE factualiza = TODAY
     AND   status_interno = 120

     SELECT COUNT(*)
     INTO vrechaza
     FROM afi_ctr_det_op54
     WHERE factualiza = TODAY
     AND   status_interno = 121

     SELECT COUNT(*)
     INTO vpendientes
     FROM afi_ctr_det_op54
     WHERE factualiza = TODAY
     --AND   status_interno = 122
     AND   status_interno IN (122,123,124)
     IF    vpendientes  = 0   THEN
       PROMPT "No existen Pendientes <enter> para salir" FOR enter
       RETURN
      ELSE 
       CALL ventana_pendientes(vfecha_carga,vacepta,vrechaza,vpendientes)
     END IF
   END IF

END FUNCTION     #diagnostica_pendientes()
---- 425 <---- 
---- 425 ---->
FUNCTION ventana_pendientes(fcarga,racep,rrech,rpend)

    DEFINE fcarga         DATE
    DEFINE racep          INTEGER
    DEFINE rrech          INTEGER 
    DEFINE rpend          INTEGER 
    DEFINE contador       INTEGER
    DEFINE qry_pen        CHAR(500)
    DEFINE vcurp          CHAR(18)
    DEFINE j              SMALLINT
    DEFINE src            SMALLINT
    DEFINE k              SMALLINT
    DEFINE tot_reg_pen    SMALLINT
    DEFINE posicion       SMALLINT
    DEFINE vrechazo       SMALLINT
    DEFINE vedo_int       SMALLINT

    LET tot_reg_pen = 0
    LET posicion    = 1
    LET k           = 0

    LET qry_pen = " SELECT b.curp_ant, ",
                           " trim(b.paterno_ant)||' '||trim(b.materno_ant)||' '||trim(b.nombres_ant), ",
                           " ' ',               ",     #acep
                           " ' ',               ",     #rech
                           " ' ',               ",     #diag
                           " b.status_interno,  ",
                           " b.rechazo_interno  ",
                    " FROM   afi_ctr_det_op54 b ",
                    --" WHERE  b.status_interno   = 122 ",
                    " WHERE  b.status_interno   IN (122,123,124) ",
                    " AND    b.factualiza = TODAY    " 

    PREPARE prep_pendientes FROM qry_pen
    DECLARE cur_pant_pen CURSOR FOR prep_pendientes

    FOREACH cur_pant_pen INTO arr_pant_pen[posicion].*, vedo_int,vrechazo
     IF  vrechazo > 0 THEN
       LET arr_pant_pen[posicion].acepta = ' '
       LET arr_pant_pen[posicion].rechaza= 'X'
       IF vedo_int = 123 OR
          vedo_int = 124 THEN
          LET arr_pant_pen[posicion].diagnos= 'X'
       ELSE
          LET arr_pant_pen[posicion].diagnos= ' '
       END IF
      ELSE
       LET arr_pant_pen[posicion].acepta = 'X'
       LET arr_pant_pen[posicion].rechaza= ' '
       IF vedo_int = 123 OR
          vedo_int = 124 THEN
          LET arr_pant_pen[posicion].diagnos= 'X'
       ELSE
          LET arr_pant_pen[posicion].diagnos= ' '
       END IF
     END IF

     LET posicion = posicion + 1
     IF  posicion > 1500 THEN
        PROMPT "Se rebasa la capacidad del arreglo,<enter> para continuar" FOR enter
        SLEEP 4
        LET posicion    = posicion - 1
        EXIT FOREACH
     END IF
    END FOREACH
    INITIALIZE arr_pant_pen [posicion].* TO NULL
    LET tot_reg_pen = posicion - 1

    IF tot_reg_pen >= 1 THEN
        OPEN WINDOW ventana_3 AT 3,2 WITH FORM "AFIC0402" ATTRIBUTE(BORDER)
        DISPLAY " AFIC040      ARCHIVO DE MODIFICACION DE DATOS OPERACION 54                     " AT 2,1 ATTRIBUTE(REVERSE)
        DISPLAY "         < Ctrl-C > Salir                 <Ctrl-V)  Detalle Pendientes          " AT 1,1 
        DISPLAY HOY USING " DD-MM-YYYY " AT 2,62 ATTRIBUTE(REVERSE)
        DISPLAY "                REGISTROS PENDIENTES EN CARGA DE OPERACION 54                   " AT 4,1 ATTRIBUTE (REVERSE)
        LET fcarga = fcarga USING "DD-MM-YYYY"
        DISPLAY BY NAME fcarga,rpend,racep,rrech
        CALL SET_COUNT (tot_reg_pen)
        DISPLAY ARRAY arr_pant_pen  TO scr_1.*
            ON KEY (INTERRUPT)
               EXIT DISPLAY
            IF FGL_LASTKEY() = FGL_KEYVAL("DOWN") OR
               FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
               LET j = ARR_CURR()
               LET src = SCR_LINE()
            END IF

            --ON KEY (RETURN)      
            ON KEY (CONTROL-V)
              LET j = ARR_CURR()
              LET src = SCR_LINE()
              --LET k = SCR_LINE()
              LET vcurp  = arr_pant_pen[j].curp          
              LET arr_pant_pen[j].diagnos = 'X' 
              --DISPLAY BY NAME arr_pant_pen[j].diagnos
              --DISPLAY BY NAME arr_pant_pen[j].acepta  
              --DISPLAY BY NAME arr_pant_pen[j].rechaza 
              DISPLAY arr_pant_pen[j].acepta  TO scr_1[src].acepta
              DISPLAY arr_pant_pen[j].rechaza TO scr_1[src].rechaza
              DISPLAY arr_pant_pen[j].diagnos TO scr_1[src].diagnos
              ERROR "Curp seleccionada ", vcurp
              CALL ventana_detalle(vcurp,j) 
              --DISPLAY BY NAME arr_pant_pen[j].diagnos
              --DISPLAY BY NAME arr_pant_pen[j].acepta  
              --DISPLAY BY NAME arr_pant_pen[j].rechaza 
              DISPLAY arr_pant_pen[j].acepta  TO scr_1[src].acepta
              DISPLAY arr_pant_pen[j].rechaza TO scr_1[src].rechaza
              DISPLAY arr_pant_pen[j].diagnos TO scr_1[src].diagnos
        END DISPLAY

        IF FGL_LASTKEY() = FGL_KEYVAL("TAB") THEN 
           LET INT_FLAG = TRUE
        END IF

        IF int_flag = TRUE THEN
            CLOSE WINDOW ventana_3
            LET int_flag = FALSE
        END IF


    ELSE
       #ERROR "El archivo no tiene pendientes."
       #SLEEP 3
       #ERROR ""
    END IF
    RETURN

END FUNCTION      #ventana_pendientes()
---- 425 <----
---- 425 ---->
FUNCTION ventana_detalle(vcurp,j)
    DEFINE vcurp char(18)
    DEFINE j    smallint
 
    --## Modifico SELECT obtener el folio_archivo del pendiente para asignar el mismo en his 
    --## Sep 2012 
    INITIALIZE r_mod TO NULL
    SELECT acdo.curp_ant,
           acdo.nombres_ant,
           acdo.paterno_ant,
           acdo.materno_ant,
           MAX(cam.marca_cod),
           ama.fena,
           acdo.rfc_ant,
           ama.fentcons,
           acdo.curp_modificado,
           acdo.nombres_modificado,
           acdo.paterno_modificado,
           acdo.materno_modificado,
           acdo.fena_modificado,
           acdo.rfc_modificado ,
           rdi.curp,
           rdi.nombre_datamart,
           rdi.paterno_datamart,
           rdi.materno_datamart,
           rdi.regimen,
           rdi.fecha_resolucion,
           rdi.diag_datamart,
           ' ',
           ' ',
           acdo.n_seguro,
           acdo.n_folio,
           acdo.tipo_solicitud,
           acdo.folio_archivo
    INTO   r_mod.curp_ant,
           r_mod.nombres_ant,
           r_mod.paterno_ant,
           r_mod.materno_ant,
           r_mod.marca_peis,
           r_mod.fena,
           r_mod.rfc_ant,
           r_mod.fentcons,
           r_mod.curp_modificado,
           r_mod.nombres_modificado,
           r_mod.paterno_modificado,
           r_mod.materno_modificado,
           r_mod.fena_modificado,
           r_mod.rfc_modificado,
           r_mod.curp,
           r_mod.nombre_datamart,
           r_mod.paterno_datamart,
           r_mod.materno_datamart,
           r_mod.regimen,
           r_mod.fecha_resolucion,
           r_mod.diag_datamart,
           r_mod.acepta,
           r_mod.rechaza,
           vn_seguro,
           vn_folio,
           vvtipo_solicitud,
           vn_folio_arch
    FROM   afi_ctr_det_op54  acdo,
           afi_mae_afiliado  ama,
    OUTER  cta_act_marca cam,
           ret_datamart_issste rdi
    WHERE  acdo.curp_ant   =  vcurp
    AND    acdo.n_seguro   =  ama.n_seguro
    AND    acdo.n_folio    =  ama.n_folio
    AND    acdo.n_seguro   =  cam.nss
    AND    acdo.curp_ant   =  rdi.curp
    --AND    acdo.n_seguro   = rdi.nss      --pst-425
    AND    rdi.diag_datamart = 101
    AND    acdo.status_interno IN (122,123,124)
    AND    acdo.factualiza = TODAY
    AND    cam.marca_cod   <> 600
    --AND    ama.tipo_solicitud = 8
    AND    ama.tipo_solicitud <> 14
    AND    rdi.sec_pension = (SELECT  MAX(sec_pension) 
                                FROM  ret_datamart_issste 
                               WHERE  curp = vcurp 
                                 AND  diag_datamart = 101) 
    GROUP BY 1,2,3,4,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27

    IF     SQLCA.SQLCODE  =  NOTFOUND     THEN
        LET arr_pant_pen[j].diagnos = ' ' 
        ERROR "CURP NO EXISTE EN DATAMART"
        SLEEP 2
        ERROR " "
        RETURN
    END IF

    OPEN WINDOW ventana_4 AT 2,2 WITH FORM "AFIC0403" ATTRIBUTE(BORDER)
    DISPLAY " AFIC040  DETALLE DE REGISTROS PENDIENTES DE CARGA DE OPERACION 54             " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY " < CTRL-C > Salir " AT 1,26
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)
    --DISPLAY "          DETALLE DE REGISTROS PENDIENTES EN CARGA DE OPERACION 54              " AT 4,1 ATTRIBUTE (REVERSE)

    INPUT BY NAME r_mod.* WITHOUT DEFAULTS 

     AFTER FIELD acepta
      IF r_mod.acepta not matches "[Xx]" THEN
         NEXT FIELD rechaza
         ELSE
         UPDATE afi_ctr_det_op54
         SET status_interno    = 124,             #ACEPTADA DATAMART
             rechazo_interno   = 0
         WHERE  curp_ant       =  vcurp
         AND    status_interno IN  (122,123,124)
         AND    factualiza     = TODAY
         LET arr_pant_pen[j].acepta = 'X'
         LET arr_pant_pen[j].rechaza = ' '
         --CALL marca_cuenta()

         LET vmarca_ant = 0
         LET ban_600    = 0

         SELECT marca_cod
         INTO   vmarca_ant
         FROM   cta_act_marca
         WHERE  nss       = vn_seguro
         AND    marca_cod = 600

         IF vmarca_ant <> 600 THEN
            CALL marca_cuenta()

            IF xcodigo_rechazo <> 0 THEN 
               LET ban_600 = 1
               ERROR "CUENTA EN PROCESO OPERATIVO, SE RECHAZA REGISTRO..."
               SLEEP 3
               ERROR ""

               UPDATE afi_ctr_det_op54
               SET status_interno    = 123,             #RECHAZA DATAMART
                   rechazo_interno   = 44               #CUENTA EN PROCESO OPERATIVO
               WHERE  curp_ant       =  vcurp
               AND    status_interno IN  (122,123,124)
               AND    factualiza     = TODAY

               CALL actualiza_002(vcurp,vn_seguro,vvtipo_solicitud)

               LET arr_pant_pen[j].acepta = ' '
               LET arr_pant_pen[j].rechaza = 'X'
            ELSE
               CALL actualiza_001(vcurp,vn_seguro,vvtipo_solicitud)
            END IF
         ELSE
            CALL actualiza_001(vcurp,vn_seguro,vvtipo_solicitud)
         END IF

         EXIT INPUT
      END IF 

     AFTER FIELD rechaza
      IF r_mod.rechaza not matches "[Xx]" THEN
         NEXT FIELD acepta 
         ELSE
         UPDATE afi_ctr_det_op54
         SET status_interno = 123,                #RECHAZADA DATAMART
             rechazo_interno = 55                 #RECHAZADA X DATAMART
         WHERE  curp_ant     =  vcurp
         AND factualiza      = TODAY
         --AND status_interno  = 122
         AND status_interno  IN (122,123,124)
         LET arr_pant_pen[j].rechaza = 'X'
         LET arr_pant_pen[j].acepta  = ' '

         CALL actualiza_002 (vcurp,vn_seguro,vvtipo_solicitud)

         LET vmarca_ant = 0

         SELECT marca_cod
         INTO   vmarca_ant
         FROM   cta_act_marca
         WHERE  nss       = vn_seguro
         AND    marca_cod = 600

         IF vmarca_ant = 600 THEN
            CALL desmarca_cuenta(vn_seguro,vmarca_ant,g_usuario,vn_folio)
         END IF

         EXIT INPUT
      END IF 

    ON KEY (INTERRUPT) 
      EXIT INPUT

    END INPUT

    CLOSE WINDOW ventana_4 
END FUNCTION     #ventana_detalle()
---- 425 <----
FUNCTION rescata_valores()
#rd-----------------------

    CALL crea_tablas()
    CALL actualiza_datos()    #ad
    --CALL revisa_datos()       #rd
    --CALL Actualiza_Maeafili() #am
    CALL lista_err()          #le
    CALL lista_acep()

END FUNCTION

FUNCTION actualiza_datos()
#-------------------------

    DEFINE
        cont_reg      INTEGER,
        cont_reg_val  INTEGER,
        contador      INTEGER

    DEFINE
        carga_reg     CHAR(700)
    DEFINE vcuenta_cza SMALLINT

    DEFINE ban_encuentra SMALLINT
    DEFINE ban_fena ,
           ban_sex  ,
           ban_nac  ,
           ban_est  ,
           ban_pat  ,
           ban_mat  ,
           ban_nom  ,
           ban_rfc  SMALLINT
--->de 444
   DEFINE 
      arr_centpago ARRAY[11] OF RECORD
      cent_pos CHAR(1)
   END RECORD,
      i SMALLINT,
      arr_centnume ARRAY[10] OF RECORD
      num CHAR(1)
   END RECORD,
      k SMALLINT,
      pasa CHAR(1),
      contador1 SMALLINT,
      desc_err CHAR(60),
      desp_err SMALLINT

    LET ban_fena  = 0
    LET ban_sex   = 0
    LET ban_nac   = 0
    LET ban_est   = 0
    LET ban_pat   = 0
    LET ban_mat   = 0
    LET ban_nom   = 0
    LET ban_rfc   = 0
    LET vrechazados = 0
    LET vpendientes = 0
    LET vaprobados= 0
    LET vaprobados_posdup = 0
    LET vrechazados_posdup  = 0
    LET vpendientes_posdup  = 0
    LET contador = 0

    LET cont_reg = 0
    LET cont_reg_val = 0
    LET vconsecutivo = 0
    LET vn_folio_arch = 0
    LET ban_encuentra = 0

    SELECT MAX(folio_archivo)
    INTO   vn_folio_arch
    --FROM   safre_af:afi_ctr_det_op54
    FROM   safre_af:afi_ctr_cza_op54

    IF vn_folio_arch IS NULL OR
       vn_folio_arch = 0 THEN
         LET vn_folio_arch = 1
    ELSE
         LET vn_folio_arch = vn_folio_arch + 1
    END IF

    SELECT COUNT(*)
    INTO   total_reg
    FROM   plano_procesar

###############################################################################
#validacion previa, carga todo el archivo a temporal
    DECLARE cursor_x CURSOR FOR
    SELECT  *
    FROM    plano_procesar

    FOREACH cursor_x INTO carga_reg
        LET cont_reg_val = cont_reg_val + 1

        IF cont_reg_val  <> 1 THEN
            LET campo_01 = carga_reg[001,002]
            LET campo_02 = carga_reg[003,003]
            LET campo_03 = carga_reg[004,015]
            LET campo_04 = carga_reg[016,055]
            LET campo_05 = carga_reg[056,062]
            LET campo_06 = carga_reg[063,067]
            LET campo_07 = carga_reg[068,072]
            LET campo_08 = carga_reg[073,085]
            LET campo_09 = carga_reg[086,103]
            LET campo_10 = carga_reg[104,114]
            LET campo_11 = carga_reg[115,154]
            LET campo_12 = carga_reg[155,194]
            LET campo_13 = carga_reg[195,234]
            LET campo_14 = carga_reg[235,247]
            LET campo_15 = carga_reg[248,265]
            LET campo_16 = carga_reg[266,276]
            LET campo_17 = carga_reg[277,316]
            LET campo_18 = carga_reg[317,356]
            LET campo_19 = carga_reg[357,396]
            LET campo_20 = carga_reg[397,404]
            LET campo_21 = carga_reg[405,406]
            LET campo_22 = carga_reg[407,407]
            LET campo_23 = carga_reg[408,408]
            LET campo_24 = carga_reg[409,468]
            LET campo_25 = carga_reg[469,498]
            LET campo_26 = carga_reg[499,528]
            LET campo_27 = carga_reg[529,533]
            LET campo_28 = carga_reg[534,534]
            LET campo_29 = carga_reg[535,567]
            LET campo_30 = carga_reg[568,667]
            LET campo_31 = carga_reg[668,669]
            LET campo_32 = carga_reg[670,672]
            LET campo_33 = carga_reg[673,675]
            LET campo_34 = carga_reg[676,678]

        INSERT INTO safre_tmp:tmp_det_mod_op54
        VALUES (campo_01,
                campo_02,
                campo_03,
                campo_04,
                campo_05,
                campo_06,
                campo_07,
                campo_08,
                campo_09,
                campo_10,
                campo_11,
                campo_12,
                campo_13,
                campo_14,
                campo_15,
                campo_16,
                campo_17,
                campo_18,
                campo_19,
                campo_20,
                campo_21,
                campo_22,
                campo_23,
                campo_24,
                campo_25,
                campo_26,
                campo_27,
                campo_28,
                --campo_29,
                --campo_30,
                campo_31,
                campo_32,
                campo_33,
                campo_34)

        END IF
    END FOREACH

#rescata los datos para validar el tipo_movimiento vs id_cet_pago

    DECLARE cursor_x1 CURSOR FOR 
    SELECT * 
    FROM   safre_tmp:tmp_det_mod_op54

    FOREACH cursor_x1 INTO reg_det.*

       LET contador = contador + 1

       IF reg_det.tipo_movimiento = '2' THEN
--->de 444
           IF reg_det.patron_id_centro_pago IS NOT NULL AND
              reg_det.patron_id_centro_pago <> '0000000' THEN 

              LET arr_centpago[01].cent_pos = reg_det.patron_id_centro_pago[01]
              LET arr_centpago[02].cent_pos = reg_det.patron_id_centro_pago[02]
              LET arr_centpago[03].cent_pos = reg_det.patron_id_centro_pago[03]
              LET arr_centpago[04].cent_pos = reg_det.patron_id_centro_pago[04]
              LET arr_centpago[05].cent_pos = reg_det.patron_id_centro_pago[05]
              LET arr_centpago[06].cent_pos = reg_det.patron_id_centro_pago[06]
              LET arr_centpago[07].cent_pos = reg_det.patron_id_centro_pago[07]

              LET k = 0
              FOR k = 1 TO 9
                LET arr_centnume[k].num = k
              END FOR 
              LET arr_centnume[10].num = 0

              ### Valida nss
              LET i         = 0
              LET k         = 0
              LET contador1 = 0
              LET desp_err  = 0
     
              FOR i = 1 TO 7
                 IF i >= 1 AND i <= 7 THEN
                    FOR k = 1 TO 10 
                       IF arr_centpago[i].cent_pos = arr_centnume[k].num THEN
                          LET contador1 = contador1 + 1
                       END IF
                    END FOR 
                 END IF

              END FOR 

               IF desp_err = 0 THEN
                  IF contador1 < 7 THEN
                     LET desc_err = "Caracteres NO validos en las posiciones del nss"
                     ERROR  "Estructura de archivo incorrecta en reg: ",contador,", id 5 contiene letras" 
                     PROMPT "VALIDACION PREVIA, ARCHIVO RECHAZADO NO PROCESADO, Enter para SALIR" FOR enter
                     EXIT PROGRAM
                  END IF
               END IF 


--               ERROR  "Estructura de archivo incorrecta en reg: ",contador,", id 28 = 2, ICP dif. de 0000000" 
--               PROMPT "VALIDACION PREVIA, ARCHIVO RECHAZADO NO PROCESADO, Enter para SALIR" FOR enter
{               SELECT COUNT(*)
               INTO  vcuenta_cza
               FROM  afi_ctr_cza_op54
               WHERE cve_ent_dest       = rechazo_007
               AND   fecha_transfe_lote = vfec_lote2
               AND   consecutivo        = rechazo_009

               IF vcuenta_cza > 0 THEN 
                  DELETE
                  FROM  afi_ctr_cza_op54
                  WHERE cve_ent_dest       = rechazo_007
                  AND   fecha_transfe_lote = vfec_lote2
                  AND   consecutivo        = rechazo_009
                  AND   factualiza         = TODAY
               END IF}
               --EXIT PROGRAM
           ELSE
               ERROR  "Estructura de archivo incorrecta en reg: ",contador,", id 5 nulo o igual a 0000000" 
               PROMPT "VALIDACION PREVIA, ARCHIVO RECHAZADO NO PROCESADO, Enter para SALIR" FOR enter
               EXIT PROGRAM
           END IF
       ELSE
          IF reg_det.tipo_movimiento = ' ' THEN
              IF reg_det.patron_id_centro_pago IS NOT NULL AND
                 reg_det.patron_id_centro_pago <> '0000000' THEN 


                 LET arr_centpago[01].cent_pos = reg_det.patron_id_centro_pago[01]
                 LET arr_centpago[02].cent_pos = reg_det.patron_id_centro_pago[02]
                 LET arr_centpago[03].cent_pos = reg_det.patron_id_centro_pago[03]
                 LET arr_centpago[04].cent_pos = reg_det.patron_id_centro_pago[04]
                 LET arr_centpago[05].cent_pos = reg_det.patron_id_centro_pago[05]
                 LET arr_centpago[06].cent_pos = reg_det.patron_id_centro_pago[06]
                 LET arr_centpago[07].cent_pos = reg_det.patron_id_centro_pago[07]

                 LET k = 0
                 FOR k = 1 TO 9
                   LET arr_centnume[k].num = k
                 END FOR 
                 LET arr_centnume[10].num = 0

                 ### Valida nss
                 LET i         = 0
                 LET k         = 0
                 LET contador1 = 0
                 LET desp_err  = 0

                 FOR i = 1 TO 7
                    IF i >= 1 AND i <= 7 THEN
                       FOR k = 1 TO 10 
                          IF arr_centpago[i].cent_pos = arr_centnume[k].num THEN
                             LET contador1 = contador1 + 1
                          END IF
                       END FOR 
                    END IF

                 END FOR 

                  IF desp_err = 0 THEN
                     IF contador1 < 7 THEN
                        LET desc_err = "Caracteres NO validos en las posiciones del nss"
                        ERROR  "Estructura de archivo incorrecta en reg: ",contador,", id 5 contiene letras" 
                        PROMPT "VALIDACION PREVIA, ARCHIVO RECHAZADO NO PROCESADO, Enter para SALIR" FOR enter
                        EXIT PROGRAM
                     END IF
                  END IF 



--              IF reg_det.patron_id_centro_pago = '0000000' THEN
--                  ERROR  "Estructura de archivo incorrecta en reg: ",contador, ", id 28 = balnco, ICP = 0000000" 
--                  PROMPT "VALIDACION PREVIA, ARCHIVO RECHAZADO NO PROCESADO, Enter para SALIR" FOR enter
{
                  SELECT COUNT(*)
                  INTO  vcuenta_cza
                  FROM  afi_ctr_cza_op54
                  WHERE cve_ent_dest       = rechazo_007
                  AND   fecha_transfe_lote = vfec_lote2
                  AND   consecutivo        = rechazo_009

                  IF vcuenta_cza > 0 THEN 
                     DELETE
                     FROM  afi_ctr_cza_op54
                     WHERE cve_ent_dest       = rechazo_007
                     AND   fecha_transfe_lote = vfec_lote2
                     AND   consecutivo        = rechazo_009
                     AND   factualiza         = TODAY
                  END IF
 }
                  --EXIT PROGRAM
              ELSE
                 ERROR  "Estructura de archivo incorrecta en reg: ",contador,", id 5 nulo o igual a 0000000" 
                 PROMPT "VALIDACION PREVIA, ARCHIVO RECHAZADO NO PROCESADO, Enter para SALIR" FOR enter
                 EXIT PROGRAM
              END IF 
          ELSE
              ERROR  "Estructura de archivo incorrecta en reg: ",contador,", id 28 dif. de balnco o de 2" 
              PROMPT "VALIDACION PREVIA, ARCHIVO RECHAZADO NO PROCESADO, Enter para SALIR" FOR enter
{
              SELECT COUNT(*)
              INTO  vcuenta_cza
              FROM  afi_ctr_cza_op54
              WHERE cve_ent_dest       = rechazo_007
              AND   fecha_transfe_lote = vfec_lote2
              AND   consecutivo        = rechazo_009

              IF vcuenta_cza > 0 THEN 
                 DELETE
                 FROM  afi_ctr_cza_op54
                 WHERE cve_ent_dest       = rechazo_007
                 AND   fecha_transfe_lote = vfec_lote2
                 AND   consecutivo        = rechazo_009
                 AND   factualiza         = TODAY
              END IF
}
              EXIT PROGRAM
          END IF
       END IF
    END FOREACH
###############################################################################

    DECLARE cursor_1 CURSOR FOR
    SELECT  *
    FROM    plano_procesar

    FOREACH cursor_1 INTO carga_reg
        LET cont_reg = cont_reg + 1
        IF cont_reg = 1 THEN
            LET campo_011 = carga_reg[001,002]
            LET campo_012 = carga_reg[003,004]
            LET campo_013 = carga_reg[005,006]
            LET campo_014 = carga_reg[007,008]
            LET campo_015 = carga_reg[009,011]
            LET campo_016 = carga_reg[012,013]
            LET campo_017 = carga_reg[014,016]
            LET campo_018 = carga_reg[017,024]
            LET campo_019 = carga_reg[025,027]
            LET campo_110 = carga_reg[028,035]
            LET campo_111 = carga_reg[036,036]
            LET campo_112 = carga_reg[037,045]

            INSERT INTO safre_tmp:cza_mod_op54
            VALUES (campo_011,
                    campo_012,
                    campo_013,
                    campo_014,
                    campo_015,
                    campo_016,
                    campo_017,
                    campo_018,
                    campo_019,
                    campo_110,
                    campo_111,
                    campo_112)

            CALL revisa_datos()       #rd

            LET vdia       = NULL
            LET vmes       = NULL
            LET vanio      = NULL
            LET vfelote= NULL
            LET vfelote2= NULL

            LET vfelote= campo_018
            LET vdia   = vfelote[5,6]
            LET vmes   = vfelote[7,8]
            LET vanio  = vfelote[1,4]
            LET vfelote2= vdia,"/",vmes,"/",vanio


            INSERT INTO safre_af:afi_ctr_cza_op54
            VALUES (vn_folio_arch,
                    campo_011,
                    campo_012,
                    campo_013,
                    campo_014,
                    campo_015,
                    campo_016,
                    campo_017,
                    vfelote2 ,
                    campo_019,
                    campo_110,
                    campo_111,
                    campo_112,
                    120      ,
                    g_usuario,
                    TODAY)

        END IF

        --IF cont_reg <> total_reg AND cont_reg <> 1 THEN
        IF cont_reg  <> 1 THEN
            LET campo_01 = carga_reg[001,002]
            LET campo_02 = carga_reg[003,003]
            LET campo_03 = carga_reg[004,015]
            LET campo_04 = carga_reg[016,055]
            LET campo_05 = carga_reg[056,062]
            LET campo_06 = carga_reg[063,067]
            LET campo_07 = carga_reg[068,072]
            LET campo_08 = carga_reg[073,085]
            LET campo_09 = carga_reg[086,103]
            LET campo_10 = carga_reg[104,114]
            LET campo_11 = carga_reg[115,154]
            LET campo_12 = carga_reg[155,194]
            LET campo_13 = carga_reg[195,234]
            LET campo_14 = carga_reg[235,247]
            LET campo_15 = carga_reg[248,265]
            LET campo_16 = carga_reg[266,276]
            LET campo_17 = carga_reg[277,316]
            LET campo_18 = carga_reg[317,356]
            LET campo_19 = carga_reg[357,396]
            LET campo_20 = carga_reg[397,404]
            LET campo_21 = carga_reg[405,406]
            LET campo_22 = carga_reg[407,407]
            LET campo_23 = carga_reg[408,408]
            LET campo_24 = carga_reg[409,468]
            LET campo_25 = carga_reg[469,498]
            LET campo_26 = carga_reg[499,528]
            LET campo_27 = carga_reg[529,533]
            LET campo_28 = carga_reg[534,534]
            LET campo_29 = carga_reg[535,567]
            LET campo_30 = carga_reg[568,667]
            LET campo_31 = carga_reg[668,669]
            LET campo_32 = carga_reg[670,672]
            LET campo_33 = carga_reg[673,675]
            LET campo_34 = carga_reg[676,678]

        INSERT INTO safre_tmp:det_mod_op54
        VALUES (campo_01,
                campo_02,
                campo_03,
                campo_04,
                campo_05,
                campo_06,
                campo_07,
                campo_08,
                campo_09,
                campo_10,
                campo_11,
                campo_12,
                campo_13,
                campo_14,
                campo_15,
                campo_16,
                campo_17,
                campo_18,
                campo_19,
                campo_20,
                campo_21,
                campo_22,
                campo_23,
                campo_24,
                campo_25,
                campo_26,
                campo_27,
                campo_28,
                --campo_29,
                --campo_30,
                campo_31,
                campo_32,
                campo_33,
                campo_34)

           LET vdia   = NULL
           LET vmes   = NULL
           LET vanio  = NULL
           LET vfena  = NULL
           LET vfena2 = NULL
           LET vdia_rfc   = NULL
           LET vmes_rfc   = NULL
           LET vanio_rfc  = NULL
           LET vdia_curp  = NULL
           LET vmes_curp  = NULL
           LET vanio_curp = NULL
           INITIALIZE vfena_compara  TO NULL
           INITIALIZE vfena_compara2 TO NULL
           INITIALIZE vfena_compara3 TO NULL
           INITIALIZE vfena_rfc      TO NULL
           INITIALIZE vfena_rfc2     TO NULL
           INITIALIZE vfena_curp     TO NULL
           INITIALIZE vfena_curp2    TO NULL

           LET vfena  = campo_20
           LET vdia   = vfena[7,8]
           LET vmes   = vfena[5,6]
           LET vanio  = vfena[1,4]
           LET vanio_rfc = vfena[3,4]
           LET vfena2 = vmes,"/",vdia,"/",vanio
           LET vfena_compara = vmes,"/",vdia,"/",vanio
           LET vfena_compara2 = vfena_compara
           LET vfena_compara3 = vmes,vdia,vanio_rfc


           IF campo_22 = "F" THEN
              LET vsexo = 2
           ELSE
              IF campo_22 = "M" THEN
                 LET vsexo = 1
              ELSE
                 LET vsexo = 0
              END IF
           END IF

           IF campo_23 = 0 THEN
              LET vedo_civil = 1
           ELSE
              IF campo_23 = 1 THEN
                 LET vedo_civil = 2
              ELSE
                 LET vedo_civil = 0
              END IF
           END IF

           LET vn_folio           = 0
           LET status_int         = 0
           LET ban_status         = 0
           LET ban_encuentra      = 0
           LET vtipo_solicitud    = 0
           LET vrech_int          = 0
           LET ban_600            = 0
           INITIALIZE vcurp_afil  TO NULL
           INITIALIZE vn_seguro   TO NULL
           INITIALIZE vnti        TO NULL
           INITIALIZE vnss_issste TO NULL
           INITIALIZE vcurp       TO NULL

           INITIALIZE vpat        TO NULL
           INITIALIZE vmat        TO NULL
           INITIALIZE vnom        TO NULL
           INITIALIZE vfena_afi   TO NULL
           LET vestadon_afi       = 0
           LET vsexo_afi          = 0
           INITIALIZE vn_rfc      TO NULL

           SELECT 'X'
           FROM   safre_af:afi_ctr_det_op54
           WHERE  curp_ant      = campo_09
           AND    factualiza    = TODAY
           AND    folio_archivo = vn_folio_arch

           IF SQLCA.SQLCODE <> NOTFOUND THEN

              SELECT n_unico,n_seguro,n_folio,tipo_solicitud
              INTO   vcurp_afil,vn_seguro,vn_folio,vtipo_solicitud
              FROM   safre_af:afi_mae_afiliado
              WHERE  n_unico = campo_09
              AND    tipo_solicitud = 8

              LET vrech_int  = 42           #42 CURP duplicado en arhcivo
              LET status_int = 121
              LET ban_status = 1
           ELSE

              IF campo_017 = '578' THEN        #afore PENISS
                 SELECT n_unico,n_seguro,n_folio,tipo_solicitud,fena,n_rfc
                 INTO   vcurp_afil,vn_seguro,vn_folio,vtipo_solicitud,vfena_afi,vn_rfc
                 FROM   safre_af:afi_mae_afiliado
                 WHERE  n_unico = campo_09              #campo_09 (curp_ant)
                 AND    tipo_solicitud = 8

                 IF SQLCA.SQLCODE  = NOTFOUND THEN
                    SELECT n_unico,n_seguro,n_folio,tipo_solicitud,
                           paterno,materno,nombres,fena,estadon,sexo,n_rfc
                    INTO   vcurp_afil,vn_seguro,vn_folio,vtipo_solicitud,
                           vpat,vmat,vnom,vfena_afi,vestadon_afi,vsexo_afi,vn_rfc
                    FROM   safre_af:afi_mae_afiliado
                    WHERE  n_unico = campo_15           #campo_15 (curp_mod)
                    AND    tipo_solicitud = 8

                    IF STATUS = 0 THEN

                       LET vpat      = vpat     CLIPPED
                       LET vmat      = vmat     CLIPPED
                       LET vnom      = vnom     CLIPPED
                       LET vn_rfc    = vn_rfc   CLIPPED

                       LET campo_14  = campo_14 CLIPPED        #rfc_mod
                       LET campo_17  = campo_17 CLIPPED        #paterno_mod
                       LET campo_18  = campo_18 CLIPPED        #materno_mod
                       LET campo_19  = campo_19 CLIPPED        #nombres_mod
                       LET campo_20  = campo_20 CLIPPED        #fena_mod

                       IF vfena_afi MATCHES vfena_compara2 OR
                          vfena_afi LIKE    vfena_compara2 OR
                          vfena_afi =       vfena_compara2 THEN
                           LET ban_fena = 0
                       ELSE
                           LET ban_fena = 1
                       END IF 

                       IF vestadon_afi MATCHES campo_21 OR
                          vestadon_afi LIKE    campo_21 OR
                          vestadon_afi =       campo_21 THEN
                          LET ban_est = 0
                       ELSE
                          LET ban_est = 1
                       END IF

                       IF vsexo_afi MATCHES vsexo OR
                          vsexo_afi LIKE    vsexo OR
                          vsexo_afi =       vsexo THEN
                          LET ban_sex = 0 
                       ELSE
                          LET ban_sex = 1
                       END IF

                       IF vn_rfc MATCHES campo_14 OR
                          vn_rfc LIKE    campo_14 OR
                          vn_rfc =       campo_14 THEN
                           LET ban_rfc = 0
                       ELSE
                           LET ban_rfc = 1
                       END IF 

                       IF vpat      MATCHES campo_17 OR
                          vpat      LIKE    campo_17 OR
                          vpat      =       campo_17 THEN
                         LET ban_pat = 0
                       ELSE
                          LET ban_pat = 1
                       END IF

                       IF vmat      MATCHES campo_18 OR
                          vmat      LIKE    campo_18 OR
                          vmat      =       campo_18 THEN
                           LET ban_mat = 0
                       ELSE
                          LET ban_mat = 1
                       END IF

                       IF vnom      MATCHES campo_19 OR
                          vnom      LIKE    campo_19 OR
                          vnom      =       campo_19 THEN
                          LET ban_nom = 0
                       ELSE
                          LET ban_nom = 1
                       END IF

                       IF ((ban_fena = 1)  AND           #MODIFICACION DE FENA Y RFC
                          (ban_rfc  = 1)) THEN
                          LET vfena_rfc  = campo_14[5,10]
                          LET vfena_curp = campo_15[5,10]
                          --LET vfena  = campo_20
                          LET vdia_rfc   = vfena_rfc[5,6]
                          LET vmes_rfc   = vfena_rfc[3,4]
                          LET vanio_rfc  = vfena_rfc[1,2]
                          LET vfena_rfc2 = vmes_rfc,vdia_rfc,vanio_rfc
                          LET vdia_curp   = vfena_curp[5,6]
                          LET vmes_curp   = vfena_curp[3,4]
                          LET vanio_curp  = vfena_curp[1,2]
                          LET vfena_curp2 = vmes_curp,vdia_curp,vanio_curp
                          IF  vfena_compara3 <> vfena_curp2 OR
                              vfena_rfc2     <> vfena_curp2 THEN
                              IF vfena_rfc2 <> vfena_curp2 THEN
                                 LET vrech_int  = 52    #52 INCOSISTENCIA EN FECHA NAC CURP VS RFC NUEVO
                                 LET status_int = 121
                                 LET ban_status = 1
                              END IF

                              IF vfena_curp2 <> vfena_compara3 THEN
                                 LET vrech_int  = 53    #53 INCOSISTENCIA EN FECHA NAC CURP VS FECHA NAC NUEVO
                                 LET status_int = 121
                                 LET ban_status = 1
                              END IF

                              IF vfena_curp2 <> vfena_compara3 AND
                                 vfena_curp2 <> vfena_rfc2 THEN
                                 LET vrech_int  = 54    #54 INCOSISTENCIA EN FECHA NAC CURP VS RFC Y FECHA NAC NUEVO
                                 LET status_int = 121
                                 LET ban_status = 1
                              END IF
                          END IF
                       END IF

                       IF ((ban_fena = 0)  AND
                           (ban_sex  = 0)  AND
                           (ban_nac  = 0)  AND
                           (ban_est  = 0)  AND
                           (ban_pat  = 0)  AND
                           (ban_mat  = 0)  AND
                           (ban_nom  = 0)  AND
                           (ban_rfc  = 0)) THEN       #inclusion de RFC 25102010
#45 CURP nueva existe pero sin diferencias en base curp
                            LET vrech_int  = 45    #45 CURP nueva existe pero sin diferencias
                            LET status_int = 121
                            LET ban_status = 1
                       ELSE
#valida que la modificación de RFC si proceda deacuerdo a validacion de fechas VS fena y CURP
                            IF vrech_int = 52 OR
                               vrech_int = 53 OR
                               vrech_int = 54 THEN
                                LET status_int = 121
--                                LET vrech_int  = 55    #55 rfc diferente pero con inconsistencias en fechas
                                LET ban_status = 1
                            ELSE
                                LET status_int = 120   #punto 1.b
                                LET ban_status = 0
                            END IF
                       END IF
                    ELSE
-->PST-811
{                       LET vrech_int  = 46         #46 CURP ant y nueva inexistente punto 3
                       LET status_int = 121
                       LET ban_status = 1
                    END IF}
                    #BUSCA TIPOS DE SOLICITUD <> DE 8 PST-811
--                       SELECT count(*)
--                       INTO   vcount
--                       FROM   safre_af:afi_mae_afiliado,
--                       WHERE  n_unico = campo_09
--                       AND    tipo_solicitud NOT IN (8,14,25)

--                       IF vcount >= 2 THEN
--                        LET vrech_int  = 56 # CURP DUPLICADA EN MAESTRO DE AFILIADOS <> 8,14

#toma solo el registro habilitado
                       SELECT n_unico,n_seguro,n_folio,tipo_solicitud
                       INTO   vcurp_afil,vn_seguro,vn_folio,vtipo_solicitud
                       FROM   safre_af:afi_mae_afiliado,
                       OUTER  (cta_act_marca)
                       WHERE  n_unico = campo_09
                       AND    tipo_solicitud NOT IN (8,14,25)
                       AND    n_seguro = nss
--                       AND    marca_cod NOT IN (120,
--                                                130,
--                                                150)
                       AND NOT  EXISTS   (SELECT nss
                                          FROM cta_act_marca
                                          WHERE nss       = n_seguro 
                                          AND   marca_cod IN(120,130,150)) #marcas de inhabilitacion
                       GROUP BY 1,2,3,4

                       IF SQLCA.SQLCODE = NOTFOUND THEN
                          LET vrech_int  = 43     #43 CURP inexistente 
                          LET status_int = 121
                          LET ban_status = 1
                       ELSE
                          --LET status_int = 13     #13 CURP existe se va por op 13
                          LET status_int = 120      #13 CURP existe se va por op 13
                          LET ban_op13   = 1
                          LET ban_status = 0
                       END IF
                    END IF
--<PST-811
                 ELSE                              #curp ant encontrada

                    SELECT 'X'
                    FROM   safre_af:afi_mae_afiliado
                    WHERE  n_unico = campo_15           #campo_15 (curp_mod)
                    AND    tipo_solicitud = 8

                    IF STATUS = 0 THEN              #curp mod encontrada

                       SELECT n_unico,n_seguro,n_folio,tipo_solicitud,
                              paterno,materno,nombres,fena,estadon,sexo,n_rfc
                       INTO   vcurp_afil,vn_seguro,vn_folio,vtipo_solicitud,
                              vpat,vmat,vnom,vfena_afi,vestadon_afi,vsexo_afi,vn_rfc
                       FROM   safre_af:afi_mae_afiliado
                       WHERE  n_unico = campo_15           #campo_15 (curp_mod)
                       AND    tipo_solicitud = 8

                       IF campo_09 <> campo_15 THEN   #curp ant y curp mod encontrada y diferentes entre si

                          LET vrech_int  = 51         #51 CURP ant y nueva ya existente punto 2.b
                          LET status_int = 121
                          LET ban_status = 1

                       --END IF
                    --END IF

###curp ant y curp nueva iguales punto 2.a
                    --IF campo_09 = campo_15 THEN
                       ELSE
                          LET vpat      = vpat     CLIPPED
                          LET vmat      = vmat     CLIPPED
                          LET vnom      = vnom     CLIPPED

                          LET campo_14  = campo_14 CLIPPED
                          LET campo_17  = campo_17 CLIPPED
                          LET campo_18  = campo_18 CLIPPED
                          LET campo_19  = campo_19 CLIPPED
                          LET campo_20  = campo_20 CLIPPED

                          IF vfena_afi MATCHES vfena_compara2 OR
                             vfena_afi LIKE    vfena_compara2 OR
                             vfena_afi =       vfena_compara2 THEN
                              LET ban_fena = 0
                          ELSE
                              LET ban_fena = 1
                          END IF 

                          IF vestadon_afi MATCHES campo_21 OR
                             vestadon_afi LIKE    campo_21 OR
                             vestadon_afi =       campo_21 THEN
                             LET ban_est = 0
                          ELSE
                             LET ban_est = 1
                          END IF

                          IF vsexo_afi MATCHES campo_22 OR
                             vsexo_afi LIKE    campo_22 OR
                             vsexo_afi =       campo_22 THEN
                             LET ban_sex = 0 
                          ELSE
                             LET ban_sex = 1
                          END IF

                          IF vn_rfc MATCHES campo_14 OR
                             vn_rfc LIKE    campo_14 OR
                             vn_rfc =       campo_14 THEN
                              LET ban_rfc = 0
                          ELSE
                              LET ban_rfc = 1
                          END IF 

                          IF vpat      MATCHES campo_17 OR
                             vpat      LIKE    campo_17 OR
                             vpat      =       campo_17 THEN
                            LET ban_pat = 0
                          ELSE
                             LET ban_pat = 1
                          END IF

                          IF vmat      MATCHES campo_18 OR
                             vmat      LIKE    campo_18 OR
                             vmat      =       campo_18 THEN
                              LET ban_mat = 0
                          ELSE
                             LET ban_mat = 1
                          END IF

                          IF vnom      MATCHES campo_19 OR
                             vnom      LIKE    campo_19 OR
                             vnom      =       campo_19 THEN
                             LET ban_nom = 0
                          ELSE
                             LET ban_nom = 1
                          END IF

                          IF ((ban_fena = 1)  OR           #MODIFICACION DE FENA Y RFC
                             (ban_rfc  = 1)) THEN
                             LET vfena_rfc  = campo_14[5,10]
                             LET vfena_curp = campo_15[5,10]
                             --LET vfena  = campo_20
                             LET vdia_rfc   = vfena_rfc[5,6]
                             LET vmes_rfc   = vfena_rfc[3,4]
                             LET vanio_rfc  = vfena_rfc[1,2]
                             LET vfena_rfc2 = vmes_rfc,vdia_rfc,vanio_rfc
                             LET vdia_curp   = vfena_curp[5,6]
                             LET vmes_curp   = vfena_curp[3,4]
                             LET vanio_curp  = vfena_curp[1,2]
                             LET vfena_curp2 = vmes_curp,vdia_curp,vanio_curp
                             IF  vfena_compara3 <> vfena_curp2 OR
                                 vfena_rfc2     <> vfena_curp2 THEN
                                 IF vfena_rfc2 <> vfena_curp2 THEN
                                    LET vrech_int  = 52    #52 INCOSISTENCIA EN FECHA NAC CURP VS RFC NUEVO
                                    LET status_int = 121
                                    LET ban_status = 1
                                 END IF

                                 IF vfena_curp2 <> vfena_compara3 THEN
                                    LET vrech_int  = 53    #53 INCOSISTENCIA EN FECHA NAC CURP VS FECHA NAC NUEVO
                                    LET status_int = 121
                                    LET ban_status = 1
                                 END IF

                                 IF vfena_curp2 <> vfena_compara3 AND
                                    vfena_curp2 <> vfena_rfc2 THEN
                                    LET vrech_int  = 54    #54 INCOSISTENCIA EN FECHA NAC CURP VS RFC Y FECHA NAC NUEVO
                                    LET status_int = 121
                                    LET ban_status = 1
                                 END IF
                             END IF
                          END IF


                          IF ((ban_fena = 0)  AND
                              (ban_sex  = 0)  AND
                              (ban_nac  = 0)  AND
                              (ban_est  = 0)  AND
                              (ban_pat  = 0)  AND
                              (ban_mat  = 0)  AND
                              (ban_nom  = 0)  AND
                              (ban_rfc  = 0)) THEN       #inclusion de RFC 25102010
#50 CURP ant y nueva ex   iste iguales pero sin diferencias en base curp
                               LET vrech_int  = 50    #50 SIN DIFERENCIAS EN DATOS A MODIFICAR 
                               LET status_int = 121
                               LET ban_status = 1
                          ELSE
                             IF vrech_int  = 52 OR
                                vrech_int  = 53 OR
                                vrech_int  = 54 THEN
                                LET status_int = 121
--                                LET vrech_int  = 55    #55 rfc diferente pero con inconsistencias en fechas
                                LET ban_status = 1
                             ELSE
                                LET status_int = 120
                                LET ban_status = 0
                             END IF
                          END IF
                       END IF
                    ELSE
### curp ant existe nueva no se verifica fechas de nac vs CURP

                       IF vfena_afi MATCHES vfena_compara2 OR
                          vfena_afi LIKE    vfena_compara2 OR
                          vfena_afi =       vfena_compara2 THEN
                           LET ban_fena = 0
                       ELSE
                           LET ban_fena = 1
                       END IF 

                       IF vn_rfc MATCHES campo_14 OR
                          vn_rfc LIKE    campo_14 OR
                          vn_rfc =       campo_14 THEN
                           LET ban_rfc = 0
                       ELSE
                           LET ban_rfc = 1
                       END IF 


                       IF ((ban_fena = 1)  OR           #MODIFICACION DE FENA Y RFC
                          (ban_rfc  = 1)) THEN
                          LET vfena_rfc  = campo_14[5,10]
                          LET vfena_curp = campo_15[5,10]
                          --LET vfena  = campo_20
                          LET vdia_rfc   = vfena_rfc[5,6]
                          LET vmes_rfc   = vfena_rfc[3,4]
                          LET vanio_rfc  = vfena_rfc[1,2]
                          LET vfena_rfc2 = vmes_rfc,vdia_rfc,vanio_rfc
                          LET vdia_curp   = vfena_curp[5,6]
                          LET vmes_curp   = vfena_curp[3,4]
                          LET vanio_curp  = vfena_curp[1,2]
                          LET vfena_curp2 = vmes_curp,vdia_curp,vanio_curp
                          IF  vfena_compara3 <> vfena_curp2 OR
                              vfena_rfc2     <> vfena_curp2 THEN
                              IF vfena_rfc2 <> vfena_curp2 THEN
                                 LET vrech_int  = 52    #52 INCOSISTENCIA EN FECHA NAC CURP VS RFC NUEVO
                                 LET status_int = 121
                                 LET ban_status = 1
                              END IF

                              IF vfena_curp2 <> vfena_compara3 THEN
                                 LET vrech_int  = 53    #53 INCOSISTENCIA EN FECHA NAC CURP VS FECHA NAC NUEVO
                                 LET status_int = 121
                                 LET ban_status = 1
                              END IF

                              IF vfena_curp2 <> vfena_compara3 AND
                                 vfena_curp2 <> vfena_rfc2 THEN
                                 LET vrech_int  = 54    #54 INCOSISTENCIA EN FECHA NAC CURP VS RFC Y FECHA NAC NUEVO
                                 LET status_int = 121
                                 LET ban_status = 1
                              END IF
                          END IF
                       END IF

                       IF vrech_int  = 52 OR
                          vrech_int  = 53 OR
                          vrech_int  = 54 THEN
                       ELSE
                          LET status_int = 120
                          LET ban_status = 0
                       END IF

--                       LET status_int = 120      #punto 3.b
--                       LET ban_status = 0

                    END IF
                 END IF
              ELSE
                 SELECT n_unico,n_seguro,n_folio,tipo_solicitud
                 INTO   vcurp_afil,vn_seguro,vn_folio,vtipo_solicitud
                 FROM   safre_af:afi_mae_afiliado
                 WHERE  n_unico = campo_09
                 AND    tipo_solicitud = 8
                 GROUP BY 1,2,3,4

                 IF SQLCA.SQLCODE = NOTFOUND THEN
                    LET vrech_int  = 41        #41 CURP inexistente no afiliado
                    LET status_int = 121
                    LET ban_status = 1
                 ELSE
                    LET ban_encuentra = 1
                    LET vrech_int  = 0        # CURP existe aprobada
                    LET status_int = 120      # CURP existe se va por op 53
                    LET ban_status = 0
                    LET ban_op13   = 0        # CURP existe se va por op 53
                 END IF

                 IF ban_encuentra = 0 THEN
                    SELECT n_unico,n_seguro,n_folio,tipo_solicitud
                    INTO   vcurp_afil,vn_seguro,vn_folio,vtipo_solicitud
                    FROM   safre_af:afi_mae_afiliado
                    WHERE  n_unico = campo_09
                    AND    tipo_solicitud <> 8
                    GROUP BY 1,2,3,4

                    IF SQLCA.SQLCODE = NOTFOUND THEN
                       LET vrech_int  = 43     #43 CURP inexistente 
                       LET status_int = 121
                       LET ban_status = 1
                    ELSE
                       --LET status_int = 13     #13 CURP existe se va por op 13
                       LET status_int = 120      #13 CURP existe se va por op 13
                       LET ban_op13   = 1
                       LET ban_status = 0
                    END IF
                 END IF
              END IF
              --LET status_int = 120
              --LET ban_status = 0
           END IF

           IF ban_status = 0 THEN

   ---- 425 ---->
              LET vsec_pension = ' '
              SELECT MAX(sec_pension)
              INTO   vsec_pension
              FROM   ret_datamart_issste
              WHERE  curp       =         campo_09
              AND    diag_datamart = 101
              --AND   nss            = vn_seguro

              SELECT "X"
              FROM   ret_datamart_issste
              WHERE  curp        =     campo_09
              --AND    nss           = vn_seguro
              AND    diag_datamart = 101
              AND    sec_pension = vsec_pension
              IF  SQLCA.SQLCODE <> NOTFOUND  THEN
--->debe rechazar los que esten en procesos operativos
                 LET vmarca_ant = 0
                 LET ban_600    = 0

                 SELECT marca_cod
                 INTO   vmarca_ant
                 FROM   cta_act_marca
                 WHERE  nss       = vn_seguro
                 AND    marca_cod = 600

                 IF vmarca_ant <> 600 THEN
                    CALL marca_cuenta()

                    IF xcodigo_rechazo <> 0 THEN 
                       LET ban_status = 1
                       LET status_int = 121
                       LET vrech_int  = 44        #44 CUENTA EN PROCESO OPERATIVO
                    ELSE
                       LET ban_status = 2
                       --LET vrech_int  = 0
                       LET status_int = 122
                    END IF
                 ELSE
                    IF vmarca_ant = 600 THEN
                       --CALL desmarca_cuenta(vn_seguro,vmarca_ant,g_usuario,vn_folio)
                       CALL marca_cuenta()

                       IF xcodigo_rechazo <> 0 THEN 
                          LET ban_status = 1
                          LET status_int = 121
                          LET vrech_int  = 44        #44 CUENTA EN PROCESO OPERATIVO
                          LET ban_600    = 1
                       ELSE
                          LET ban_status = 2
                          --LET vrech_int  = 0
                          LET status_int = 122
                       END IF
                    END IF
                 END IF
---<
                --LET ban_status    =   2
                --LET status_int    =   122          #Pendiente
                --LET vrech_int     =   0
              --END IF
              ELSE
   ----- 425 <-----

                 LET vmarca_ant = 0
                 LET ban_600    = 0

                 SELECT marca_cod
                 INTO   vmarca_ant
                 FROM   cta_act_marca
                 WHERE  nss       = vn_seguro
                 AND    marca_cod = 600

                 IF vmarca_ant <> 600 THEN
                    CALL marca_cuenta()

                    IF xcodigo_rechazo <> 0 THEN 
                       LET ban_status = 1
                       LET status_int = 121
                       LET vrech_int  = 44        #44 CUENTA EN PROCESO OPERATIVO
                    ELSE
                       LET ban_status = 0
                       LET vrech_int  = 0
                       LET status_int = 120
                    END IF
                 ELSE
                    IF vmarca_ant = 600 THEN
                       --CALL desmarca_cuenta(vn_seguro,vmarca_ant,g_usuario,vn_folio)
                       CALL marca_cuenta()

                       IF xcodigo_rechazo <> 0 THEN 
                          LET ban_status = 1
                          LET status_int = 121
                          LET vrech_int  = 44        #44 CUENTA EN PROCESO OPERATIVO
                          LET ban_600    = 1
                       ELSE
                          LET ban_status = 0
                          LET vrech_int  = 0
                          LET status_int = 120
                       END IF
                    END IF
                 END IF
              END IF          --425
           END IF
           --CUO13
           IF ban_status = 0 THEN
              CALL verifica_exp()
                 IF reg_exp.con_exp = 1 THEN
                    LET ban_status = 0
                    LET vrech_int  = 0
                    LET status_int = 120
                 ELSE
                    LET ban_status = 1
                    LET status_int = 121
                    LET vrech_int  = 56     #NO CUENTA CON EXPEDIENTE
                 END IF
           END IF     
		
        LET vconsecutivo = vconsecutivo + 1
{
---- 425 ---->
           LET vsec_pension = ' '
           IF ban_status = 0 THEN
              SELECT MAX(sec_pension)
              INTO   vsec_pension
              FROM   ret_datamart_issste
              WHERE  curp       =         campo_09

              SELECT "X"
              FROM   ret_datamart_issste
              WHERE  curp        =     campo_09
              AND    sec_pension = vsec_pension
              IF  SQLCA.SQLCODE <> NOTFOUND  THEN
                LET ban_status    =   2
                LET status_int    =   122          #Pendiente
                --LET vrech_int     =   0
              END IF
           END IF
----- 425 <-----
}

        INSERT INTO safre_af:afi_ctr_det_op54
        VALUES (vn_seguro,
                vn_folio,
                vtipo_solicitud,
                vconsecutivo,
                vn_folio_arch,
                campo_01,
                campo_02,
                campo_03,
                campo_04,
                campo_05,
                campo_06,
                campo_07,
                campo_08,
                campo_09,
                campo_10,
                campo_11,
                campo_12,
                campo_13,
                campo_14,
                campo_15,
                campo_16,
                campo_17,
                campo_18,
                campo_19,
                vfena2  ,
                campo_21,
                vsexo   ,
                --campo_23,
                vedo_civil,
                campo_24,
                campo_25,
                campo_26,
                campo_27,
                campo_28,
                --campo_29,
                campo_30,
                campo_31,
                campo_32,
                campo_33,
                status_int,
                vrech_int,
                g_usuario,
                TODAY)

           IF  status_int = 122  THEN                     #425
             IF campo_28 = " " THEN         #" " modificacion por dependencia
               LET vpendientes = vpendientes + 1
              ELSE
               IF campo_28 = "2" THEN         #" " modificacion por dependencia
                 LET vpendientes_posdup = vpendientes_posdup + 1
               END IF
             END IF
            ELSE                                          #425

           SELECT b.nti, b.nss_issste, b.curp
           INTO   vnti,vnss_issste,vcurp
           FROM   safre_af:cta_ctr_reg_ind b
           WHERE  b.curp = campo_09
           AND    b.nti  = vn_seguro

           CASE ban_status
               WHEN 1
                   IF campo_28 = " " THEN         #" " modificacion por dependencia
                      LET vrechazados = vrechazados + 1
                          CALL actualiza_02()
                          CALL despliega_totales()
                   END IF

                   IF campo_28 = "2" THEN         #"2" modificacion por Posibles Duplicados
                      LET vrechazados_posdup = vrechazados_posdup + 1
                          CALL actualiza_02()
                          CALL despliega_totales()
                   END IF

               WHEN 0
                   IF campo_28 = " " THEN         #" " modificacion por dependencia
                      LET vaprobados = vaprobados + 1
                          CALL actualiza_01()
                          CALL despliega_totales()  
                   END IF

                   IF campo_28 = "2" THEN         #"2" modificacion por dependencia
                      LET vaprobados_posdup = vaprobados_posdup + 1
                          CALL actualiza_01()
                          CALL despliega_totales()  
                   END IF
           END CASE
{
           LET vcadena = "00000000000000000"

           IF ban_op13 = 1 THEN
              INSERT INTO afi_ctr_curp
              VALUES (vn_seguro       ,
                      campo_15        ,
                      vn_folio        ,
                      vtipo_solicitud ,
                      vcadena         ,
                      0               ,
                      ''              ,
                      ''              ,
                      ''              ,
                      campo_15        ,
                      ''              ,
                      ''              ,
                      '13'            ,
                      g_usuario       ,
                      TODAY)

--->afi_mae_modifica
              INSERT INTO afi_mae_modifica
              VALUES (vtipo_solicitud,
                      vn_folio       ,
                      HOY            ,
                      ''             ,
                      0              ,
                      NULL           ,
                      NULL           ,
                      campo_17       ,#paterno modificado
                      campo_18       ,#materno_modificado
                      campo_19       ,#nombres modificado
                      vn_seguro      ,
                      campo_14       ,#rfc modificado
                      campo_15       ,#n_unico modificado
                      vsexo          ,
                      vedo_civil     ,
                      vfena2         ,
                      0              ,
                      0              ,
                      campo_21       ,#estadon modificado
                      'MEX'          ,
                      0              ,
                      ''             ,
                      ''             ,
                      ''             ,
                      0              ,
                      0              ,
                      HOY            ,
                      g_usuario      ,
                      0              ,#cod_operacion
                      '0'            ,#diag_proceso
                      120            ,
                      NULL           ,
                      ''             ,
                      0              ,
                      0              ,
                      NULL           ,
                      0              ,
                      '' )

           END IF
}
          END IF                                               #425
        END IF
    END FOREACH

       LET cont_tot = total_reg - 1


       --# Incluyendo modificacion de validación para movimientos IMSS Oct 2013 
       CALL  fn_VerIMSS()  

       CALL despliega_totales()

       IF NOT bnd_proceso THEN
           ERROR ""
           PROMPT "Proceso finalizado satisfactoriamente, [Enter] Para Continuar "
           ATTRIBUTE (REVERSE)
           FOR vresp ATTRIBUTE (REVERSE)

           --CLOSE WINDOW w_curp
       ELSE
           DISPLAY "Programa finalizado satisfactoriamente"
       END IF

       LET vaprobados = vaprobados + vaprobados_posdup
       LET vrechazados = vrechazados + vrechazados_posdup

       INSERT INTO afi_ctr_arh_proc
       VALUES (generar,cont_tot, vaprobados, vrechazados, 0, TODAY)

       RETURN

END FUNCTION       #actualiza_datos()

FUNCTION revisa_datos()
#----------------------

    DEFINE
        rechazo_lote CHAR(9),
        rechazo_deta CHAR(3),
        l_reg        RECORD LIKE tab_rch_lote.*,
        x_reg        RECORD LIKE tab_rdeta.*,
        aux_pausa    CHAR(1)


    DEFINE
        rechazo_09  CHAR(02),
        rechazo_001 CHAR(02),
        rechazo_002 CHAR(02),
        rechazo_003 CHAR(02),
        rechazo_007 CHAR(03),
        rechazo_008 CHAR(08),
        rechazo_009 SMALLINT

    DEFINE vcuenta_cza SMALLINT
    DEFINE vfec_lote   CHAR(10)
    DEFINE vfec_lote2  DATE

    # ENCABEZADO #
    SELECT campo1,
           campo2,
           campo3,
           campo7,        --tipo_ent_destino  PST-338
           campo8,        --fecha_tranfe_lote PST-338
           campo9,        --consecutivo       PST-338
           campo12
    INTO   rechazo_001,
           rechazo_002,
           rechazo_003,
           rechazo_007,
           rechazo_008,
           rechazo_009,
           rechazo_lote
    FROM safre_tmp:cza_mod_op54

    SELECT *
    INTO   l_reg.*
    FROM   tab_rch_lote
    WHERE  rlote_cod = rechazo_lote

    LET bnd_proceso = 1

    IF STATUS  <> NOTFOUND THEN
        IF bnd_proceso THEN
            DISPLAY "Program stopped, ERROR DE PROCESO,        NO PUEDE CONTINUAR "
            EXIT PROGRAM
        ELSE
            CLEAR SCREEN
            DISPLAY l_reg.rlote_cod AT 10,1
            DISPLAY l_reg.rlote_desc_c AT 11,1
            PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR" FOR aux_pausa
            EXIT PROGRAM
        END IF
    END IF

    IF rechazo_001 <> "01" THEN
        IF bnd_proceso THEN
            DISPLAY "Program stopped, Tipo de registro debe ser 01 en encabezado"
            EXIT PROGRAM
        ELSE
            CLEAR SCREEN
            DISPLAY "Tipo de registro debe ser 01 en ENCABEZADO" AT 10,1
            PROMPT "ERROR DE PROCESO NO        PUEDE CONTINUAR" FOR aux_pausa
            EXIT PROGRAM
        END IF
    END IF

    IF rechazo_002 <> "01" THEN
        IF bnd_proceso THEN
            DISPLAY "Program stopped, Identificador de servicio debe ser 01 en encabezado"
            EXIT PROGRAM
        ELSE
            CLEAR SCREEN
            DISPLAY "Identificador de Servicio debe ser 01 en ENCABEZADO" AT 10,1
            PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR" FOR aux_pausa
            EXIT PROGRAM
        END IF
    END IF

    IF rechazo_003 <> "54" AND 
       rechazo_003 <> "52" THEN
        IF bnd_proceso THEN
            DISPLAY "Program stopped, Identificador de operacion debe ser 54 en encabezado"
            EXIT PROGRAM
        ELSE
            CLEAR SCREEN
            DISPLAY "Identificador de Operacion debe ser 54 en ENCABEZADO" AT 10,1
            PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR" FOR aux_pausa
            EXIT PROGRAM
        END IF
    END IF

    LET vfec_lote = rechazo_008[5,6],"/",rechazo_008[7,8],"/",rechazo_008[1,4]
    LET vfec_lote2 = vfec_lote

    SELECT COUNT(*)
    INTO  vcuenta_cza
    FROM  afi_ctr_cza_op54
    WHERE cve_ent_dest       = rechazo_007
    AND   fecha_transfe_lote = vfec_lote2
    AND   consecutivo        = rechazo_009

    IF vcuenta_cza > 0 THEN 
       DISPLAY "Validacion de Estructura del archivo, Archivo YA PROCESADO, NO PUEDE CONTINUAR"
       EXIT PROGRAM
    END IF

   LET bnd_proceso = 0
   RETURN

END FUNCTION       #revisa_datos()

FUNCTION actualiza_02()
#a02-------------------

    IF ban_600 = 0 THEN
       UPDATE afi_mae_afiliado
       --SET    status_interno  = 121                            #425
       SET    status_interno  = 120                              #425
       WHERE  n_unico         = vcurp_afil
       AND    n_seguro        = vn_seguro
       AND    n_folio         = vn_folio
       AND    tipo_solicitud  = vtipo_solicitud
    END IF
{
    IF vn_seguro IS NULL THEN 
      LET vn_seguro = '00000000000'
    END IF

    IF vn_folio IS NULL THEN
      LET vn_folio = 0
    END IF

    INSERT INTO afi_mae_modifica
    VALUES (8,
            vn_folio       ,#n_folio
            HOY            ,
            ''             ,
            0              ,
            NULL           ,
            NULL           ,
            campo_17       ,#paterno modificado
            campo_18       ,#materno_modificado
            campo_19       ,#nombres modificado
            vn_seguro      ,
            campo_14       ,#rfc modificado
            campo_15       ,#n_unico modificado
            vsexo          ,
            vedo_civil     ,
            vfena2         ,
            0              ,
            0              ,
            campo_21       ,#estadon modificado
            'MEX'          ,
            0              ,
            ''             ,
            ''             ,
            ''             ,
            0              ,
            0              ,
            HOY            ,
            g_usuario      ,
            0              ,#cod_operacion
            '0'            ,#diag_proceso
            121            ,#121 rechazo op 54
            NULL           ,
            ''             ,
            0              ,
            0              ,
            NULL           ,
            0              ,
            '' )

    INSERT INTO afi_rechaza_proc
    VALUES (vn_seguro, vdiag_proceso, vhoy, generar)}

END FUNCTION       #actualiza_02()

FUNCTION actualiza_01()
#a01-------------------

    DEFINE marca2   SMALLINT
    DEFINE mrow     INTEGER
    DEFINE pat_proc CHAR(40)
    DEFINE mat_proc CHAR(40)
    DEFINE nom_proc CHAR(40)
    DEFINE vrow     INTEGER
    DEFINE f_recep  DATE
    DEFINE vnom_mod CHAR(120)
    DEFINE vnom_bd  CHAR(120)


    INITIALIZE reg_ant.*  TO NULL

    LET vcalle     = NULL
    LET vcolonia   = NULL
    LET vdelega    = NULL
    LET vcodpos    = NULL
    LET vdeleg_cod = 0
    LET vcalle_concatena = NULL
    LET vnom_afore = NULL
    LET vnom_proce = NULL


    --LET marca2 = 610

    SELECT a.n_seguro,
           a.n_folio,
           a.tipo_solicitud,
           a.paterno,
           a.materno,
           a.nombres,
           a.n_rfc,
           a.n_unico,
           a.fena,
           a.estadon,
           a.sexo,
           a.edo_civil
    INTO   reg_ant.n_seguro,
           reg_ant.n_folio,
           reg_ant.tipo_solicitud,
           reg_ant.paterno,
           reg_ant.materno,
           reg_ant.nombres,
           reg_ant.n_rfc,
           reg_ant.n_unico,
           reg_ant.fena,
           reg_ant.estadon,
           reg_ant.sexo,
           reg_ant.edo_civil
    FROM   afi_mae_afiliado a
    WHERE  a.n_unico        = vcurp_afil
    AND    a.tipo_solicitud = vtipo_solicitud
    --AND    a.n_seguro = vn_seguro

    SELECT a.* 
    INTO   reg_dom.*
    FROM   afi_domicilio a
    WHERE  a.nss            = reg_ant.n_seguro
    AND    a.n_folio        = reg_ant.n_folio
    AND    a.tipo_solicitud = reg_ant.tipo_solicitud
    AND    a.marca_envio    = 'X'

    SELECT estad_cod,deleg_cod,ciudad_cod
    INTO   v_estado,vdeleg_cod,v_ciudad
    FROM   tab_codpos
    WHERE  cpos_cod = reg_op54.cpos_cod_modificado

    UPDATE afi_mae_afiliado
    --SET    status_interno  = 200
    SET    status_interno  = 120
    WHERE  n_seguro        = vn_seguro
    AND    n_unico         = vcurp_afil
    AND    n_folio         = vn_folio
    AND    tipo_solicitud  = vtipo_solicitud
    --AND    status_interno in (130,240,300)
{
    UPDATE afi_mae_afiliado
    SET    paterno            = reg_op54.paterno_modificado,
           materno            = reg_op54.materno_modificado,
           nombres            = reg_op54.nombres_modificado,
           n_unico            = reg_op54.curp_modificado,
           n_rfc              = reg_op54.rfc_modificado,
           usuario            = g_usuario,
           fena               = vfena2,
           estadon            = reg_op54.estadon_modificado,
           --sexo               = reg_op54.sexo_modificado,
           sexo               = vsexo,
           --edo_civil          = reg_op54.edo_civil_modificado
           edo_civil          = vedo_civil
    WHERE  --n_seguro           = vn_seguro
           n_unico            = vcurp
    AND    n_folio            = reg_ant.n_folio
    AND    tipo_solicitud     = reg_ant.tipo_solicitud
    --AND    status_interno IN  (130,160)

    IF reg_op54.domicilio_modificado IS NOT NULL OR
       reg_op54.domicilio_modificado NOT MATCHES "[ *]" THEN
        UPDATE afi_domicilio
        SET    calle          = reg_op54.domicilio_modificado,
               numero         = '',
               depto          = '',
               colonia        = reg_op54.colonia_modificado,
               delega         = vdeleg_cod,
               codpos         = reg_op54.cpos_cod_modificado,
               estado         = v_estado,
               ciudad         = v_ciudad,
               usuario        = g_usuario,
               factualiza     = TODAY
        WHERE  nss            = reg_ant.n_seguro
        AND    n_folio        = reg_ant.n_folio
        AND    tipo_solicitud = reg_ant.tipo_solicitud
        AND    marca_envio    = 'X'
    END IF

    UPDATE cta_ctr_reg_ind
    SET    nss_issste = reg_op54.nsi_modificado,
           curp       = reg_op54.curp_modificado
    WHERE  curp       = vcurp

    INSERT INTO afi_mae_modifica
    VALUES (8,
            reg_ant.n_folio,
            HOY            ,
            ''             ,
            0              ,
            NULL           ,
            NULL           ,
            campo_17       ,#paterno modificado
            campo_18       ,#materno_modificado
            campo_19       ,#nombres modificado
            vn_seguro      ,
            campo_14       ,#rfc modificado
            campo_15       ,#n_unico modificado
            vsexo          ,
            vedo_civil     ,
            vfena2         ,
            0              ,
            0              ,
            campo_21       ,#estadon modificado
            'MEX'          ,
            0              ,
            ''             ,
            ''             ,
            ''             ,
            0              ,
            0              ,
            HOY            ,
            g_usuario      ,
            0              ,#cod_operacion
            '0'            ,#diag_proceso
            120            ,
            NULL           ,
            ''             ,
            0              ,
            0              ,
            NULL           ,
            0              ,
            '' )
}
    LET vcalle_concatena = reg_dom.calle, " ",reg_dom.numero, " ",reg_dom.depto

#inserta datos originales antes de modificacion a historico
    INSERT INTO afi_ctr_his_op54
    VALUES (vn_seguro,
            vn_folio,
            vtipo_solicitud,
            vconsecutivo,
            vn_folio_arch,
            reg_ant.n_rfc      ,
            reg_ant.n_unico    ,#curp_maeafil
            reg_ant.n_seguro   ,#nsi_mae_afil
            reg_ant.paterno    ,
            reg_ant.materno    ,
            reg_ant.nombres    ,
            reg_ant.fena       ,
            reg_ant.estadon    ,
            reg_ant.sexo       ,
            reg_ant.edo_civil  ,
            reg_dom.calle      ,
            reg_dom.numero     ,
            reg_dom.depto      ,
            reg_dom.colonia    ,
            reg_dom.delega     ,
            reg_dom.codpos     ,
            reg_dom.estado     ,
            reg_dom.ciudad     ,
            HOY                ,
            g_usuario          ,
            HOY)
    --CALL desmarca_cuenta(vn_seguro,marca2,g_usuario,vn_folio)

END FUNCTION       #actualiza_01()

FUNCTION despliega_totales()
#dt-------------------------

     IF NOT bnd_proceso THEN
        DISPLAY "                  DATOS A PROCESAR                 "
            AT 8,1 ATTRIBUTE ( REVERSE )

        DISPLAY "Total de Registros del lote : ",
                 cont_tot USING "#######&" AT 9,15 ATTRIBUTE ( BOLD )

        DISPLAY "MODIFICACIONES ORDINARIAS                          "
            AT 10,15

        DISPLAY "Registros aceptados         : ",
                 vaprobados USING "#######&" AT 11,15 ATTRIBUTE ( BOLD )

        DISPLAY "Registros rechazados        : ",
                 vrechazados USING "#######&" AT 12,15 ATTRIBUTE ( BOLD )

        DISPLAY "Registros pendientes        : ",
                 vpendientes USING "#######&" AT 13,15 ATTRIBUTE ( BOLD )

        DISPLAY "MODIFICACIONES POR POSIBLES DUPLICADOS             "
            AT 14,15

        DISPLAY "Registros aceptados         : ",
                 vaprobados_posdup USING "#######&" AT 15,15 ATTRIBUTE ( BOLD )

        DISPLAY "Registros rechazados        : ",
                 vrechazados_posdup USING "#######&" AT 16,15 ATTRIBUTE ( BOLD )

        DISPLAY "Registros pendientes        : ",
                 vpendientes_posdup USING "#######&" AT 17,15 ATTRIBUTE ( BOLD )

        DISPLAY "Folio Archivo               : ",
                 vn_folio_arch USING "#######&" AT 18,15 ATTRIBUTE ( BOLD )

    ELSE
        DISPLAY "                  DATOS A PROCESAR                 "

        DISPLAY "Total de Registros del lote  : ",
                 cont_tot USING "#######&" AT 9,15 ATTRIBUTE ( BOLD )

        DISPLAY "MODIFICACIONES ORDINARIAS                          "
            AT 10,15

        DISPLAY "Registros aceptados         : ",
                 vaprobados USING "#######&" AT 11,15 ATTRIBUTE ( BOLD )

        DISPLAY "Registros rechazados        : ",
                 vrechazados USING "#######&" AT 12,15 ATTRIBUTE ( BOLD )

        DISPLAY "MODIFICACIONES POR POSIBLES DUPLICADOS             "
            AT 14,15

        DISPLAY "Registros aceptados         : ",
                 vaprobados_posdup USING "#######&" AT 15,15 ATTRIBUTE ( BOLD )

        DISPLAY "Registros rechazados        : ",
                 vrechazados_posdup USING "#######&" AT 16,15 ATTRIBUTE ( BOLD )

        DISPLAY "Folio Archivo               : ",
                 vn_folio_arch USING "#######&" AT 18,15 ATTRIBUTE ( BOLD )

    END IF

    --IF NOT bnd_proceso THEN
        --PROMPT "Presione [Enter] para continuar" FOR enter
    --END IF

END FUNCTION

FUNCTION lista_err()
#-------------------

     DEFINE hora   CHAR(8)
     DEFINE HOY                  DATE
     DEFINE vcod_rechazo  CHAR(8)
     DEFINE vcont   INTEGER

     DEFINE gr_curp RECORD
         n_unico    LIKE afi_mae_afiliado.n_unico,
         paterno     CHAR(13),
         materno     CHAR(13),
         nombres     CHAR(16),
         status_int  SMALLINT,
         rechazo_int SMALLINT
     END RECORD

     DEFINE G_LISTA     CHAR(200)
     DEFINE G_IMPRIME     CHAR(200)

    DEFINE resp   CHAR(1)

    LET hora = TIME
    LET HOY = TODAY

    LET G_LISTA =  g_paramgrales.ruta_listados CLIPPED,"/",
                   g_usuario CLIPPED,".RECHAZOS_OP54ISSSTE.",
                   HOY USING "dd-mm-yy","_",hora CLIPPED

    START REPORT listado TO G_LISTA

    DECLARE c_carga CURSOR FOR
        SELECT b.curp_ant,
               b.paterno_ant,
               b.materno_ant,
               b.nombres_ant,
               b.status_interno,
               b.rechazo_interno
        FROM   safre_af:afi_ctr_det_op54 b
        WHERE  b.status_interno NOT IN (121,123,13)            #425
        AND    b.factualiza   = TODAY
        ORDER BY 2,1

    FOREACH c_carga INTO gr_curp.*
        OUTPUT TO REPORT listado(gr_curp.*)
    END FOREACH

    FINISH REPORT listado

    ERROR "LISTADO GENERADO" SLEEP 2

    LET G_LISTA =  "chmod 777 ",g_paramgrales.ruta_listados CLIPPED,"/",
                   g_usuario CLIPPED, ".RECHAZOS_OP54ISSSTE.",
                   HOY USING "dd-mm-yy","_",hora CLIPPED

    RUN G_LISTA

    {LET G_IMPRIME = "lp ",g_paramgrales.ruta_listados CLIPPED,"/",
                    g_usuario CLIPPED,".RECHAZOS_OP54ISSSTE.",
                    HOY USING "dd-mm-yy","_",hora CLIPPED

    RUN G_IMPRIME
}
END FUNCTION

REPORT listado(rpt)
#l-----------------

    DEFINE rpt RECORD
        n_curp      LIKE afi_mae_afiliado.n_unico,
        paterno     CHAR(13),
        materno     CHAR(13),
        nombres     CHAR(16),
        status_int  SMALLINT,
        rechazo_int SMALLINT
    END RECORD

    DEFINE
        l_estado     CHAR(16),
        aux_sexo     CHAR(10),
        razon_social CHAR(40),
        vcont        SMALLINT,
        rpt_desc_rechazo CHAR(50) 

    OUTPUT
        PAGE LENGTH 90
        TOP  MARGIN 0
        BOTTOM MARGIN 0
        LEFT MARGIN 0
        RIGHT MARGIN 0

    --------ADECUARLO PARA IMPRESION

{    FORMAT
    ON EVERY ROW

        PRINT COLUMN 1, rpt.*
--    ORDER EXTERNAL BY rpt.cod_rechazo
}
    FORMAT
    PAGE HEADER

    PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT
            COLUMN 001,"========================================"            ,
            COLUMN 040,"========================================"     ,
            COLUMN 080,"========================================"     ,
            COLUMN 120,"========================================"     ,
            COLUMN 160,"====="
        PRINT
            COLUMN 001,razon_social                                     ,
            COLUMN 140,"FECHA :",HOY USING "DD/MM/YYYY"
            --COLUMN 149,hora [1,5]
        PRINT
            COLUMN 001,"AFIC040"                                     ,
            COLUMN 035," E S T A D O   D E   M O D I F I C A D O S  R E C H A Z A D O S   P O R   A F O R E ",
            COLUMN 140,"NUM.PAG.:",pageno USING "####,###"
            SKIP 1 LINE
        PRINT
            COLUMN 001,"----------------------------------------"     ,
            COLUMN 040,"----------------------------------------"     ,
            COLUMN 080,"----------------------------------------"     ,
            COLUMN 120,"----------------------------------------"     ,
            COLUMN 160,"-----"
       SKIP 1 LINES
       PRINT
            COLUMN 001, "    Curp",
            COLUMN 038, "Ap. Paterno",
            COLUMN 054, "Ap. Materno",
            COLUMN 070, "Nombres",
            COLUMN 095, "Cod/Desc. Rechazo",
            COLUMN 148,        "Codigo Rechazo"
       PRINT
            COLUMN 001,"========================================" ,
            COLUMN 040,"========================================" ,
            COLUMN 080,"========================================" ,
            COLUMN 120,"========================================" ,
            COLUMN 160,"====="

    ON EVERY ROW
       LET vcont = vcont + 1

       CASE rpt.rechazo_int
          WHEN 40
             LET rpt_desc_rechazo = "40 CURP INACTIVO"
          WHEN 41
             LET rpt_desc_rechazo = "41 CURP INEXISTENTE NO AFILIADO"
          WHEN 42
             LET rpt_desc_rechazo = "42 CURP DUPLICADO EN ARCHIVO"
          WHEN 43
             LET rpt_desc_rechazo = "43 CURP INEXISTENTE EN AFORE"
          WHEN 44
             LET rpt_desc_rechazo = "44 CUENTA EN PROCESO OPERATIVO"
          WHEN 45
             LET rpt_desc_rechazo = "45 CURP NUEVA EXISTE PERO S/DIFERENCIAS"
          WHEN 46
             LET rpt_desc_rechazo = "46 CURP ANT Y NUEVA INEXISTENTE"
          WHEN 47
             LET rpt_desc_rechazo = "47 CURP ANT EXISTE PERO S/DIFEERENCIAS"
          WHEN 48
             LET rpt_desc_rechazo = "48 CURP NUEVA YA EXISTENTE"
          WHEN 49
             LET rpt_desc_rechazo = "49 CURP ANTERIOR INEXISTENTE"
          WHEN 50
             LET rpt_desc_rechazo = "50 SIN DIFERENCIAS EN DATOS A MODIFICAR"
          WHEN 51
             LET rpt_desc_rechazo = "51 CURP ANT Y NUEVA YA EXISTENTES"
          WHEN 52
             LET rpt_desc_rechazo = "52 INCOSISTENCIA FEC NAC CURP VS RFC NUEVO"
          WHEN 53
             LET rpt_desc_rechazo = "53 INCOSISTENCIA FEC NAC CURP VS FEC NAC NUEVO"
          WHEN 54
             LET rpt_desc_rechazo = "54 INCOSISTENCIA FEC NAC CURP VS RFC/FEC NAC NUEVO"
          WHEN 55                                              --- modificacion validacion IMSS Oct 2013
             LET rpt_desc_rechazo = "55 NSS IMSS NO REGISTRADO EN AFILIADOS"
		      WHEN 56
             LET rpt_desc_rechazo = "56 NO CUENTA CON EXPEDIENTE"                 --CUO13	 
       END CASE

       PRINT
            COLUMN 001, rpt.n_curp,
            COLUMN 038, rpt.paterno,
            COLUMN 054, rpt.materno,
            COLUMN 070, rpt.nombres,
            COLUMN 095, rpt_desc_rechazo,
            COLUMN 157, rpt.status_int

    AFTER GROUP OF rpt.status_int
       SKIP 1 LINES
       PRINT
            COLUMN 145, "================"
       PRINT
            COLUMN 125, "TOTAL POR CODIGO DE RECHAZO     :", GROUP COUNT(*) USING "#####"
       SKIP 2 LINES


END REPORT

-------------------------------------------------------------------------------
FUNCTION lista_acep()
#-------------------

     DEFINE hora   CHAR(8)
     DEFINE HOY                  DATE
     DEFINE vcod_rechazo  CHAR(8)
     DEFINE vcont   INTEGER

     DEFINE gr_curp RECORD
         n_unico    LIKE afi_mae_afiliado.n_unico,
         paterno     CHAR(13),
         materno     CHAR(13),
         nombres     CHAR(16),
         status_int  SMALLINT
     END RECORD

     DEFINE G_LISTA2     CHAR(200)
     DEFINE G_IMPRIME2     CHAR(200)

    DEFINE resp   CHAR(1)

    LET hora = TIME
    LET HOY = TODAY

    LET G_LISTA2 =  g_paramgrales.ruta_listados CLIPPED,"/",
                   g_usuario CLIPPED,".ACEPTADOS_OP54ISSSTE.",
                   HOY USING "dd-mm-yy","_",hora CLIPPED

    START REPORT listado2 TO G_LISTA2

    DECLARE c_carga2 CURSOR FOR
        SELECT b.curp_ant,
               b.paterno_ant,
               b.materno_ant,
               b.nombres_ant,
               b.status_interno
        FROM   safre_af:afi_ctr_det_op54 b
        WHERE  b.status_interno IN (120,124,13)                  #425
        AND    b.factualiza   = TODAY
        ORDER BY 2,1

    FOREACH c_carga2 INTO gr_curp.*
        OUTPUT TO REPORT listado2(gr_curp.*)
    END FOREACH

    FINISH REPORT listado2

    ERROR "LISTADO GENERADO" SLEEP 2

    LET G_LISTA2 =  "chmod 777 ",g_paramgrales.ruta_listados CLIPPED,"/",
                   g_usuario CLIPPED, ".ACEPTADOS_OP54ISSSTE.",
                   HOY USING "dd-mm-yy","_",hora CLIPPED

    RUN G_LISTA2
{
    LET G_IMPRIME2 = "lp ",g_paramgrales.ruta_listados CLIPPED,"/",
                    g_usuario CLIPPED,".ACEPTADOS_OP54ISSSTE.",
                    HOY USING "dd-mm-yy","_",hora CLIPPED

    RUN G_IMPRIME2
}
END FUNCTION

REPORT listado2(rpt)
#l-----------------

    DEFINE rpt RECORD
        n_curp      LIKE afi_mae_afiliado.n_unico,
        paterno     CHAR(13),
        materno     CHAR(13),
        nombres     CHAR(16),
        status_int  SMALLINT
    END RECORD

    DEFINE
        l_estado     CHAR(16),
        aux_sexo     CHAR(10),
        razon_social CHAR(40),
        vcont      SMALLINT

    OUTPUT
        PAGE LENGTH 90
        TOP  MARGIN 0
        BOTTOM MARGIN 0
        LEFT MARGIN 0
        RIGHT MARGIN 0

    --------ADECUARLO PARA IMPRESION

{    FORMAT
    ON EVERY ROW

        PRINT COLUMN 1, rpt.*
--    ORDER EXTERNAL BY rpt.cod_rechazo
}
    FORMAT
    PAGE HEADER

    PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT
            COLUMN 001,"========================================"            ,
            COLUMN 040,"========================================"     ,
            COLUMN 080,"========================================"     ,
            COLUMN 120,"========================================"     ,
            COLUMN 160,"====="
        PRINT
            COLUMN 001,razon_social                                     ,
            COLUMN 140,"FECHA :",HOY USING "DD/MM/YYYY"
            --COLUMN 149,hora [1,5]
        PRINT
            COLUMN 001,"AFIC040"                                     ,
            COLUMN 035," E S T A D O   D E   M O D I F I C A D O S  A C E P T A D O S   P O R   A F O R E ",
            COLUMN 140,"NUM.PAG.:",pageno USING "####,###"
            SKIP 1 LINE
        PRINT
            COLUMN 001,"----------------------------------------"     ,
            COLUMN 040,"----------------------------------------"     ,
            COLUMN 080,"----------------------------------------"     ,
            COLUMN 120,"----------------------------------------"     ,
            COLUMN 160,"-----"
       SKIP 1 LINES
       PRINT
            COLUMN 001, "    Curp",
            COLUMN 038, "Ap. Paterno",
            COLUMN 054, "Ap. Materno",
            COLUMN 070, "Nombres",
            COLUMN 088, "Desc. Acept",
            COLUMN 148,        "Status Int"
       PRINT
            COLUMN 001,"========================================" ,
            COLUMN 040,"========================================" ,
            COLUMN 080,"========================================" ,
            COLUMN 120,"========================================" ,
            COLUMN 160,"====="

    ON EVERY ROW
       LET vcont = vcont + 1

       PRINT
            COLUMN 001, rpt.n_curp,
            COLUMN 038, rpt.paterno,
            COLUMN 054, rpt.materno,
            COLUMN 070, rpt.nombres,
            COLUMN 083, '',
            COLUMN 157, 120

    AFTER GROUP OF rpt.status_int
       SKIP 1 LINES
       PRINT
            COLUMN 145, "================"
       PRINT
            COLUMN 125, "TOTAL POR CODIGO DE ACEPTADO     :", GROUP COUNT(*) USING "#####"
       SKIP 2 LINES


END REPORT

FUNCTION Consulta()
#C-----------------

    DEFINE ga_record ARRAY[3000] OF RECORD
        nombre_archivo LIKE afi_ctr_arh_proc.nombre_archivo,
        total_reg      LIKE afi_ctr_arh_proc.total_reg,
        total_aprob    LIKE afi_ctr_arh_proc.total_aprob,
        total_rech     LIKE afi_ctr_arh_proc.total_rech,
        total_dup      LIKE afi_ctr_arh_proc.total_pend,
        fecha_proceso  LIKE afi_ctr_arh_proc.fecha_proceso
    END RECORD

    DEFINE
        vcount    ,
        vtotal    ,
        vaprobados ,
        vrechazos  ,
        vduplicados,
        vtot_curp  INTEGER

    DEFINE
        pos    SMALLINT

    SELECT COUNT(*)
    INTO   vcount
    FROM   afi_ctr_arh_proc

    SELECT SUM(@total_reg)
    INTO   vtotal
    FROM   afi_ctr_arh_proc

    SELECT SUM(@total_aprob)
    INTO   vaprobados
    FROM   afi_ctr_arh_proc

    SELECT SUM(@total_rech)
    INTO   vrechazos
    FROM   afi_ctr_arh_proc

    SELECT SUM(@total_pend)
    INTO   vduplicados
    FROM   afi_ctr_arh_proc

    DISPLAY "                                                                                    " AT 1,1
    DISPLAY "  CTRL-C cancela                                                                    " AT 2,1
    DISPLAY " CONSULTA " AT 2,65

    DECLARE curp_12 CURSOR FOR
    SELECT @nombre_archivo, @total_reg, @total_aprob, @total_rech, @total_pend,
           @fecha_proceso
    FROM afi_ctr_arh_proc
    ORDER BY 6 DESC

    LET pos = 1

    FOREACH curp_12 INTO ga_record[pos].*
        LET pos = pos + 1
    END FOREACH

    IF (pos-1) >= 1 THEN
        CALL  SET_COUNT(pos-1)

        DISPLAY BY NAME vcount
        DISPLAY BY NAME vtotal
        DISPLAY BY NAME vaprobados
        DISPLAY BY NAME vrechazos
        DISPLAY BY NAME vduplicados

        DISPLAY ARRAY ga_record TO scr_1.*

        ON KEY (INTERRUPT)
            CLEAR FORM
            EXIT DISPLAY

        END DISPLAY
    ELSE
        ERROR "ARCHIVO DE PROCESAR VACIO"
    END IF

END FUNCTION
#######################################################################
FUNCTION desmarca_cuenta(vnss,vmarca,vusuario,vn_folio)
#dc----------------------------------------------------

    DEFINE
        vnss          CHAR(11),
        vmarca        SMALLINT,
        vusuario      CHAR(8),
        vn_folio      DECIMAL(10,0),
        pestado_marca SMALLINT,
        pmarca_causa  SMALLINT,
        vcorrelativo  INTEGER

    LET pestado_marca = 0
    LET pmarca_causa  = 0

    SELECT d.correlativo
    INTO   vcorrelativo
    FROM   cta_act_marca d
    WHERE  d.nss = vnss
    AND    d.marca_cod = vmarca

    PREPARE eje_desmarca FROM v_desmarca

    EXECUTE eje_desmarca
    USING vnss,
          vmarca,
          vcorrelativo,
          pestado_marca,
          pmarca_causa,
          vusuario

END FUNCTION
#######################################################################
FUNCTION marca_cuenta()
#mc--------------------
  DEFINE
    pmarca_causa SMALLINT,
    errmsg       CHAR(70)

  LET pmarca_causa = 0
  LET xcodigo_rechazo = 0

  #se suprime el vn_folio por rebasar el limite de un INTEGER
  LET ejecuta = "EXECUTE PROCEDURE marca_cuenta(",
                "'",vn_seguro,"'",
                ",",edo_proc,
                --",",vn_folio,
                ",",1,
                ",",vmarca_estado,
                ",",vcodigo_rechazo,
                ",",pmarca_causa,
                ",","'","'", ",",
                "'",g_usuario,"'",")"

  LET ejecuta = ejecuta CLIPPED

  PREPARE clausula_spl FROM ejecuta
  DECLARE cursor_marca CURSOR FOR clausula_spl
  OPEN cursor_marca
--->despliega error 
     WHENEVER ERROR CONTINUE
       --FOREACH cur_10 INTO lr_saldo.*
       FETCH cursor_marca INTO xcodigo_marca, xcodigo_rechazo
           IF SQLCA.SQLCODE < 0 THEN              
              LET errmsg = err_get(SQLCA.SQLCODE)
              ERROR errmsg 
              PROMPT "No puede continuar, Enter para continuar " for enter
              EXIT PROGRAM
           END IF
          WHENEVER ERROR STOP
---<
    --FETCH cursor_marca INTO xcodigo_marca, xcodigo_rechazo
  CLOSE cursor_marca

  RETURN 

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


FUNCTION actualiza_001(fcurp,fn_seguro,ftipo_solicitud)
#a01-------------------

    DEFINE marca2   SMALLINT
    DEFINE mrow     INTEGER
    DEFINE pat_proc CHAR(40)
    DEFINE mat_proc CHAR(40)
    DEFINE nom_proc CHAR(40)
    DEFINE vrow     INTEGER
    DEFINE f_recep  DATE
    DEFINE vnom_mod CHAR(120)
    DEFINE vnom_bd  CHAR(120)
    DEFINE fcurp    CHAR(18)
    DEFINE fn_seguro CHAR(11)
    DEFINE ftipo_solicitud SMALLINT


    INITIALIZE reg_ant.*  TO NULL

    LET vcalle     = NULL
    LET vcolonia   = NULL
    LET vdelega    = NULL
    LET vcodpos    = NULL
    LET vdeleg_cod = 0
    LET vcalle_concatena = NULL
    LET vnom_afore = NULL
    LET vnom_proce = NULL


    --LET marca2 = 610

    SELECT a.n_seguro,
           a.n_folio,
           a.tipo_solicitud,
           a.paterno,
           a.materno,
           a.nombres,
           a.n_rfc,
           a.n_unico,
           a.fena,
           a.estadon,
           a.sexo,
           a.edo_civil
    INTO   reg_ant.n_seguro,
           reg_ant.n_folio,
           reg_ant.tipo_solicitud,
           reg_ant.paterno,
           reg_ant.materno,
           reg_ant.nombres,
           reg_ant.n_rfc,
           reg_ant.n_unico,
           reg_ant.fena,
           reg_ant.estadon,
           reg_ant.sexo,
           reg_ant.edo_civil
    FROM   afi_mae_afiliado a
    WHERE  a.n_unico        = fcurp
    AND    a.tipo_solicitud = ftipo_solicitud
    --AND    a.n_seguro = vn_seguro

    SELECT a.* 
    INTO   reg_dom.*
    FROM   afi_domicilio a
    WHERE  a.nss            = reg_ant.n_seguro
    AND    a.n_folio        = reg_ant.n_folio
    AND    a.tipo_solicitud = reg_ant.tipo_solicitud
    AND    a.marca_envio    = 'X'

    SELECT estad_cod,deleg_cod,ciudad_cod
    INTO   v_estado,vdeleg_cod,v_ciudad
    FROM   tab_codpos
    WHERE  cpos_cod = reg_op54.cpos_cod_modificado

    UPDATE afi_mae_afiliado
    --SET    status_interno  = 200
    SET    status_interno  = 120
    WHERE  n_seguro        = fn_seguro
    AND    n_unico         = fcurp
    --AND    n_folio         = vn_folio
    AND    tipo_solicitud  = ftipo_solicitud
    --AND    status_interno in (130,240,300)

    LET vcalle_concatena = reg_dom.calle, " ",reg_dom.numero, " ",reg_dom.depto

    -- ## Se toma el folio del detalle para q no cambie  Sep 2012 
    -- ## Entonces inhibo esto  
    --SELECT MAX(folio_archivo)
    --INTO vn_folio_arch
    --FROM afi_ctr_his_op54

    SELECT MAX(consecutivo)
    INTO vconsecutivo
    FROM afi_ctr_his_op54
    WHERE folio_archivo = vn_folio_arch

    IF  vconsecutivo    IS NULL   THEN 
        LET  vconsecutivo   =  0 
    END IF
    LET vconsecutivo = vconsecutivo + 1

#inserta datos originales antes de modificacion a historico
    INSERT INTO afi_ctr_his_op54
    VALUES (fn_seguro,
            reg_ant.n_folio,
            ftipo_solicitud,
            vconsecutivo,
            vn_folio_arch,
            reg_ant.n_rfc      ,
            reg_ant.n_unico    ,#curp_maeafil
            reg_ant.n_seguro   ,#nsi_mae_afil
            reg_ant.paterno    ,
            reg_ant.materno    ,
            reg_ant.nombres    ,
            reg_ant.fena       ,
            reg_ant.estadon    ,
            reg_ant.sexo       ,
            reg_ant.edo_civil  ,
            reg_dom.calle      ,
            reg_dom.numero     ,
            reg_dom.depto      ,
            reg_dom.colonia    ,
            reg_dom.delega     ,
            reg_dom.codpos     ,
            reg_dom.estado     ,
            reg_dom.ciudad     ,
            HOY                ,
            g_usuario          ,
            HOY)


END FUNCTION       #actualiza_001()

FUNCTION actualiza_002(fcurp,fn_seguro,ftipo_solicitud)
#a02-------------------
    DEFINE fcurp    CHAR(18)
    DEFINE fn_seguro CHAR(11)
    DEFINE ftipo_solicitud SMALLINT

    IF ban_600 = 0 THEN
       UPDATE afi_mae_afiliado
       --SET    status_interno  = 121
       SET    status_interno  = 120
       WHERE  n_unico         = fcurp
       AND    n_seguro        = fn_seguro
--       AND    n_folio         = vn_folio
       AND    tipo_solicitud  = ftipo_solicitud
    END IF

END FUNCTION       #actualiza_002()


-- # Validacion de registros IMSS, verificacion si existe afiliado habilitado
FUNCTION  fn_VerIMSS()  
 DEFINE   ls_lei                        SMALLINT
 DEFINE   ls_ok                         SMALLINT
 DEFINE   lc_query                      CHAR(1000)
 DEFINE   ls_act                        SMALLINT
 

 LET      lc_query   =   ' SELECT  *                                           ',
                         '   FROM  safre_af:afi_ctr_det_op54                   ',
                         '  WHERE  status_interno        IN  (120,124)         ',
                         '    AND  factualiza             =  TODAY             ',
                         '    AND  tipo_trabajador        =  1                 '
 PREPARE  p_Selo54             FROM   lc_query
 DECLARE  d_Selo54       CURSOR FOR   p_Selo54

 LET      lc_query   =   ' SELECT  COUNT(*)                                                        ',
                         '   FROM  safre_af:afi_mae_afiliado                                       ', 
                         '  WHERE  n_unico                =  ?                                     ',
                         '    AND  n_seguro              <>  ?                                     ',
                         '    AND  tipo_solicitud   NOT  IN  (14)                                  ',
                         '    AND               NOT  EXISTS  (SELECT  nss                          ',    -- ## Marcas inhabilitado 
                         '                                      FROM  cta_act_marca                ',
                         '                                     WHERE  nss         =  n_seguro      ',
                         '                                       AND  marca_cod  IN  (120,130,150))'
 PREPARE  p_BusAct             FROM   lc_query
 
 LET      lc_query   =   ' UPDATE  safre_af:afi_ctr_det_op54                                       ',
                         '    SET  status_interno          =  ?,                                   ',
                         '         rechazo_interno         =  55                                   ',
                         '  WHERE  curp_modificado         =  ?                                    ',
                         '    AND  status_interno         IN  (120, 124)                           '
 PREPARE  p_Updo54             FROM   lc_query             
  
 FOREACH  d_Selo54             INTO   gr_o54.*

     LET      ls_act              =   0
     EXECUTE  p_BusAct        USING   gr_o54.curp_ant,
                                      gr_o54.nsi_ant 
                               INTO   ls_act
     IF       ls_act              =   0    THEN     --- no hay entonces cambia a rechazado 
          IF       gr_o54.status_interno      =   120    THEN  
               LET   gr_o54.status_interno    =   121
          END IF
          IF       gr_o54.status_interno      =   124    THEN  
               LET   gr_o54.status_interno    =   123
          END IF
          EXECUTE  p_Updo54               USING   gr_o54.status_interno,
                                                  gr_o54.curp_modificado 
          IF       SQLCA.SQLCODE              =   0     THEN  
              LET      vaprobados                 =   vaprobados   - 1
              LET      vrechazados                =   vrechazados  + 1
          END  IF
     END IF
 END FOREACH 

END FUNCTION

-->CUO13
FUNCTION verifica_exp()
#mc--------------------
  DEFINE errmsg               CHAR(70)
  
  INITIALIZE errmsg TO NULL
  LET reg_exp.con_exp = 0
  LET reg_exp.sin_exp = 0

  LET ejecuta = "EXECUTE PROCEDURE fn_verifica_expediente(",
                "'",vn_seguro,"'",
                ",",vn_folio,
                ",",vtipo_solicitud,")"

  LET ejecuta = ejecuta CLIPPED

  PREPARE clausula2_spl FROM ejecuta
  DECLARE cursor2_marca CURSOR FOR clausula2_spl
  OPEN cursor2_marca
    WHENEVER ERROR CONTINUE
    FETCH cursor2_marca INTO reg_exp.con_exp, reg_exp.sin_exp
       IF SQLCA.SQLCODE < 0 THEN              
          LET errmsg = err_get(SQLCA.SQLCODE)
          ERROR errmsg 
          PROMPT "No puede continuar, Enter para continuar " for enter
       END IF
  CLOSE cursor2_marca

  RETURN 

END FUNCTION
--<CUO13



