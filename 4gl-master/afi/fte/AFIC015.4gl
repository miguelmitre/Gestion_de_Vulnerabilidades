#############################################################################
#Proyecto          => AFORES ( MEXICO )                                     #
#Propietario       => E.F.P.                                                #
#Programa AFIC015  => CARGA DE ARCHIVO LIBERACION DE TRAMITE DE CURP        #
#Sistema           => AFI.                                                  #
#Autor             => FERNANDO HERRERA HERNANDEZ                            #
#Fecha             => 24 DE DICIEMBRE DE 2003                               #
#Version para      => METLIFE                                               #
#Modifico          => FERNANDO HERRERA HERNANDEZ                            #
#Fecha             => 28 DE JUNIO DE 2005 (Cambio MPT V7 CURP)              #
#Actualizacion     => EDUARDO RESENDIZ MEDINA 15 MARZO 2006 listado_3       #
#############################################################################

DATABASE safre_af

GLOBALS

    DEFINE g_reg  RECORD
        generar       	    CHAR(20)
    END RECORD

    DEFINE varchivo         CHAR(14)
    DEFINE hoy              DATE
    DEFINE tiempo	    CHAR(08)
    DEFINE aux_pausa        CHAR(01)
    DEFINE corr             CHAR(100)
    DEFINE total_reg        INTEGER
    DEFINE carga            CHAR(50)
    DEFINE g_plano1         INTEGER
    DEFINE vn_seguro        CHAR(11)
    DEFINE vn_folio         CHAR(18)
    DEFINE vcurp            CHAR(18)
    DEFINE vf_rechazo       CHAR(10)
    DEFINE vcurp_solicitud  CHAR(18)
    DEFINE vfec_alta_curp   CHAR(10)
    DEFINE vhoy             DATE
    DEFINE vvcurp           CHAR(18)
    DEFINE vtotal           INTEGER
    DEFINE vvig_venc        INTEGER
    DEFINE vrech_ren	    INTEGER
    DEFINE vresp            CHAR(1)
    DEFINE vdiag_proceso    CHAR(8)
    DEFINE vcod_operacion   CHAR(2)
    DEFINE vcodigo_39       INTEGER
    DEFINE vdesc_cod_39     CHAR(30)
    DEFINE vstatus_ren      CHAR(30)
    DEFINE vent_asigna      CHAR(30)
    DEFINE cfecha_a8        CHAR(8)
    DEFINE cfecha_asig      CHAR(10)
    DEFINE vfecha_asig      DATE
    DEFINE cfecha_recep	    CHAR(10)
    DEFINE vfecha_recep	    DATE
    DEFINE g_usuario        CHAR(8)
    DEFINE vdoc_prob_ren    CHAR(16)
    DEFINE vstatus_interno  SMALLINT
    DEFINE vfena            CHAR(10)

    #Var. batch
    DEFINE reg_bat RECORD
        pid                 INTEGER,
        proceso_cod         INTEGER,
        opera_cod           INTEGER,
        nombre_archivo      CHAR(25)
    END RECORD

    DEFINE bnd_proceso 	    SMALLINT
    #Var. batch

    DEFINE G_LISTA          CHAR(200)
    DEFINE g_seg_modulo     RECORD LIKE seg_modulo.*
    DEFINE resp             CHAR(1)
    DEFINE salida	    CHAR(100)

    #Var. Marca      
    DEFINE
    opc			    CHAR(1),
    vnss		    CHAR(11),
    marca		    SMALLINT,
    vmarca_entra	    SMALLINT,
    vmarca_estado	    SMALLINT,
    vcodigo_rechazo         SMALLINT,
    ejecuta		    CHAR(300),
    xcodigo_marca	    SMALLINT,
    xcodigo_rechazo	    SMALLINT,
    xcodigo_convive	    SMALLINT,
    edo_proc		    SMALLINT,
    su_st_int		    SMALLINT,
    operacion		    CHAR(40),
    xdes_marca		    CHAR(50),
    v_desmarca              CHAR(100)
    #Var. Marca      

    DEFINE G_LISTA3 CHAR(200)
    DEFINE G_IMPRE3 CHAR(200)

END GLOBALS
#############################################################################

MAIN

    DISPLAY " "
    DISPLAY ".1"

    CALL STARTLOG("AFIC015.log")
    CALL inicio() #i

    IF NOT bnd_proceso THEN
        DEFER INTERRUPT
        OPTIONS INPUT WRAP,
            PROMPT LINE LAST,
            ACCEPT KEY CONTROL-I

        CALL proceso_principal()   #pp
    ELSE
        CALL rescata_valores()     #rv
        CALL actualiza_operacion() #rv
    END IF

END MAIN
#############################################################################

FUNCTION inicio()
#i---------------

    LET reg_bat.pid         = ARG_VAL(1)
    LET reg_bat.proceso_cod = ARG_VAL(2)
    LET reg_bat.opera_cod   = ARG_VAL(3)
    LET bnd_proceso         = 0
    LET vhoy                = TODAY
    LET vvig_venc           = 0
    LET vrech_ren           = 0

    LET edo_proc	    = 600
    LET vmarca_estado	    = 0
    LET vcodigo_rechazo     = 0 

    IF reg_bat.pid THEN
        DISPLAY "INICIANDO PROCESO ..."
        LET bnd_proceso = 1
    END IF 

    LET hoy           = TODAY
    LET tiempo        = TIME
    LET g_reg.generar = "S"

    SELECT *, USER
    INTO   g_seg_modulo.*, g_usuario
    FROM   seg_modulo
    WHERE  modulo_cod = 'afi'

    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp
        DROP TABLE safre_tmp:plano1_lib_curp
    WHENEVER ERROR STOP

    CREATE TABLE safre_tmp:plano1_lib_curp
        (n_registros CHAR(550))

    DATABASE safre_af

    LET salida = g_seg_modulo.ruta_listados CLIPPED, "/",
                 g_usuario CLIPPED,
                 "_", TODAY USING "ddmmyy" CLIPPED,
                 "_", tiempo[1,2], tiempo[4,5], tiempo[7,8], 
                 ".ctr_lib_curp" CLIPPED 

   LET G_LISTA3 = g_seg_modulo.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                  ".REPORTE_LIBCURP." CLIPPED,
                  HOY USING "ddmmyy"


    LET v_desmarca = " EXECUTE PROCEDURE desmarca_cuenta ( ?,?,?,?,?,? )"

END FUNCTION
#############################################################################

FUNCTION crea_tablas() 
#ct-------------------

    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp
        DROP TABLE safre_tmp:encabezado1_lib_curp
        DROP TABLE safre_tmp:detalle1_lib_curp
        DROP TABLE safre_tmp:sumario1_lib_curp
    WHENEVER ERROR STOP

    CREATE TABLE safre_tmp:encabezado1_lib_curp
        (campo1  		CHAR(02),
         campo2  		CHAR(02),
         campo3  		CHAR(02),
         campo4  		CHAR(02),
         campo5  		CHAR(03),
         campo6  		CHAR(02),
         campo7  		CHAR(03),
         campo8  		CHAR(08),
         campo9  		CHAR(03),
         campo10 		CHAR(03),
         campo11 		CHAR(02),
         campo12 		CHAR(08),
         campo13 		CHAR(08),
         campo14 		CHAR(08),
         campo15 		CHAR(08),
         campo16 		CHAR(08),
         campo17 		CHAR(01),
         campo18 		CHAR(09),
         campo19 		CHAR(02),
         campo20 		CHAR(03),
         campo21 		CHAR(02),
         campo22 		CHAR(03)
        )

    CREATE TABLE safre_tmp:detalle1_lib_curp
        (tipo_registro       	CHAR(02),
         contador_servicio   	CHAR(10),
         ident_tip_lib       	CHAR(02),
         nss_solicitud       	CHAR(11),
         curp_solicitud      	CHAR(18),
         rfc_trabajador      	CHAR(13), #Filler
         paterno             	CHAR(40),
         materno             	CHAR(40),
         nombres             	CHAR(40),
         fecha_nacimiento    	CHAR(08),
         clave_promotor      	CHAR(10), #Filler
         fec_recp_sol_afore  	CHAR(08),
         folio_solicitud     	DECIMAL(16,0),
         sexo                	SMALLINT,
         entidad_nacimiento  	CHAR(02),
         ind_infonavit       	CHAR(01), #Filler
         nacionalidad        	CHAR(03),
         tip_prob            	CHAR(01),
         fol_prob            	CHAR(10),
         doc_prob            	CHAR(16),
         cod_operacion       	CHAR(02),
         diag_proceso        	CHAR(15),
         ident_lote_origen   	CHAR(16),
         nss_oficial         	CHAR(11), #Filler
         ind_nss_modificado  	CHAR(01), #Filler
         fec_alta_curp       	CHAR(08),
         curp_oficial        	CHAR(18),
         ind_curp_modif      	CHAR(01),
         fecha_recep_sello   	CHAR(08),
         hora_recep_sello    	CHAR(08),
         consecutivo_recep   	CHAR(08),
         tipo_transaccion    	CHAR(03),
         status_renapo       	CHAR(02),
         ent_asigna_curp     	INTEGER,
         archivo_alta        	CHAR(6),
         archivo_modifica    	CHAR(6),
         paterno_renapo      	CHAR(40),
         materno_renapo      	CHAR(40),
         nombres_renapo      	CHAR(40),
         nacionalidad_ren    	CHAR(03),
         tip_prob_ren        	CHAR(01),
         fol_prob_ren        	CHAR(10),
         doc_prob_ren        	CHAR(16)
        )

    CREATE TABLE safre_tmp:sumario1_lib_curp
        (campo1    		CHAR(02),
         campo2    		CHAR(02),
         campo3    		CHAR(03),
         campo4    		CHAR(08),
         campo5    		CHAR(03),
         campo6    		CHAR(02),
         campo7    		CHAR(02),
         campo8    		CHAR(09),
         campo9    		CHAR(09)
        )

    DATABASE safre_af

END FUNCTION
#############################################################################

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 4,4 WITH FORM "AFIC0151" ATTRIBUTE(BORDER)
    DISPLAY " AFIC015      CARGA DE ARCHIVO LIBERACION TRAMITE DE CURP                      " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                           < Ctrl-C > Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,63 ATTRIBUTE(REVERSE)
    DISPLAY g_seg_modulo.ruta_rescate AT 10,10

    INPUT BY NAME g_reg.generar
        AFTER FIELD generar
        IF g_reg.generar IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD generar
        END IF

        LET varchivo = g_reg.generar[1,14]

        SELECT "X"             
        FROM   afi_ctr_arh_lcurp
        WHERE  nombre_archivo = varchivo            

        IF STATUS <> NOTFOUND THEN
            ERROR "ARCHIVO YA PROCESADO"
            SLEEP 1
            ERROR " "
            INITIALIZE g_reg.generar TO NULL
            CLEAR FORM
            NEXT FIELD generar
        END IF

        LET carga = NULL
        LET carga = g_seg_modulo.ruta_rescate CLIPPED,"/",
                    g_reg.generar CLIPPED

        WHENEVER ERROR CONTINUE
            LOAD FROM carga DELIMITER "," 
            INSERT INTO safre_tmp:plano1_lib_curp
        WHENEVER ERROR STOP

        SELECT COUNT(*) 
        INTO   g_plano1 
        FROM   safre_tmp:plano1_lib_curp

        IF g_plano1 IS NULL OR g_plano1 = 0 THEN
            ERROR "NOMBRE DE ARCHIVO INCORRECTO O VACIO, VERIFIQUE " 
            SLEEP 5
            EXIT PROGRAM
        END IF

        ERROR "Procesando Informacion"

        CALL rescata_valores() #

        EXIT PROGRAM

        ON KEY (CONTROL - B)
           CALL despliega_archivos()

    END INPUT

END FUNCTION
#############################################################################

FUNCTION rescata_valores()
#rv-----------------------

    CALL crea_tablas()        #ct
    CALL actualiza_datos()    #ad
    CALL revisa_datos()       #rd
    CALL Historico()          #h 

END FUNCTION
#############################################################################

FUNCTION actualiza_datos()
#ad-----------------------

    DEFINE 
        cont_reg      INTEGER
 
    DEFINE 
        carga_reg     CHAR(550)

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
        campo_110     CHAR(03),
        campo_111     CHAR(02),
        campo_112     CHAR(08),
        campo_113     CHAR(08),
        campo_114     CHAR(08),
        campo_115     CHAR(08),
        campo_116     CHAR(08),
        campo_117     CHAR(01),
        campo_118     CHAR(09),
        campo_119     CHAR(02),
        campo_210     CHAR(03),
        campo_211     CHAR(02),
        campo_212     CHAR(03),

        campo_01      CHAR(02),
        campo_02      CHAR(10),
        campo_03      CHAR(02),
        campo_04      CHAR(11),
        campo_05      CHAR(18),
        campo_06      CHAR(13),
        campo_07      CHAR(40),
        campo_08      CHAR(40),
        campo_09      CHAR(40),
        campo_10      CHAR(08),
        campo_11      CHAR(10),
        campo_12      CHAR(08),
        campo_13      CHAR(10),
        campo_14      CHAR(01),
        campo_15      CHAR(02),
        campo_16      CHAR(01),
        campo_17      CHAR(03),
        campo_18      CHAR(01),
        campo_19      CHAR(10),
        campo_20      CHAR(16),
        campo_21      CHAR(02),
        campo_22      CHAR(15),
        campo_23      CHAR(16),
        campo_24      CHAR(11),
        campo_25      CHAR(01),
        campo_26      CHAR(08),
        campo_27      CHAR(18),
        campo_28      CHAR(01),
        campo_29      CHAR(08),
        campo_30      CHAR(08),
        campo_31      CHAR(08),
        campo_32      CHAR(03),
        campo_33      CHAR(02),
        campo_34      CHAR(05),
        campo_35      CHAR(06),
        campo_36      CHAR(06),
        campo_37      CHAR(40),
        campo_38      CHAR(40),
        campo_39      CHAR(40),
        campo_40      CHAR(03),
        campo_41      CHAR(01),
        campo_42      CHAR(10),
        campo_43      CHAR(16),

        campo_201     CHAR(02),
        campo_202     CHAR(02),
        campo_203     CHAR(03),
        campo_204     CHAR(08),
        campo_205     CHAR(03),
        campo_206     CHAR(02),
        campo_207     CHAR(02),
        campo_208     CHAR(09),
        campo_209     CHAR(09)

    LET cont_reg = 0

    SELECT COUNT(*)
    INTO   total_reg
    FROM   safre_tmp:plano1_lib_curp

    DECLARE cursor_1 CURSOR FOR 
    SELECT  * 
    FROM    safre_tmp:plano1_lib_curp

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
            LET campo_110 = carga_reg[028,030]
            LET campo_111 = carga_reg[031,032]
            LET campo_112 = carga_reg[033,040]
            LET campo_113 = carga_reg[041,048]
            LET campo_114 = carga_reg[049,056]
            LET campo_115 = carga_reg[057,064]
            LET campo_116 = carga_reg[065,072]
            LET campo_117 = carga_reg[073,073]
            LET campo_118 = carga_reg[074,082]
            LET campo_119 = carga_reg[083,084]
            LET campo_210 = carga_reg[085,087]
            LET campo_211 = carga_reg[088,089]
            LET campo_212 = carga_reg[090,092]

            INSERT INTO safre_tmp:encabezado1_lib_curp
            VALUES (campo_011 ,
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
                    campo_112,
                    campo_113,
                    campo_114,
                    campo_115,
                    campo_116,
                    campo_117,
                    campo_118,
                    campo_119,
                    campo_210,
                    campo_211,
                    campo_212 
                   )

            LET vf_rechazo = campo_018[5,6],"/",
                             campo_018[7,8],"/",
                             campo_018[1,4]
        END IF

        IF cont_reg <> total_reg  AND  cont_reg <> 1 THEN
            LET campo_01 = carga_reg[001,002]
            LET campo_02 = carga_reg[003,012]
            LET campo_03 = carga_reg[013,014]
            LET campo_04 = carga_reg[015,025]
            LET campo_05 = carga_reg[026,043]
            LET campo_06 = carga_reg[044,056]
            LET campo_07 = carga_reg[057,096]
            LET campo_08 = carga_reg[097,136]
            LET campo_09 = carga_reg[137,176]
            LET campo_10 = carga_reg[177,184]
            LET campo_11 = carga_reg[185,194]
            LET campo_12 = carga_reg[195,202]
            LET campo_13 = carga_reg[203,212]
            LET campo_14 = carga_reg[213,213]
            LET campo_15 = carga_reg[214,215]
            LET campo_16 = carga_reg[216,216]
            LET campo_17 = carga_reg[217,219]
            LET campo_18 = carga_reg[220,220]
            LET campo_19 = carga_reg[221,230]
            LET campo_20 = carga_reg[231,246]
            LET campo_21 = carga_reg[247,248]
            LET campo_22 = carga_reg[249,263]
            LET campo_23 = carga_reg[264,279]
            LET campo_24 = carga_reg[280,290]
            LET campo_25 = carga_reg[291,291]
            LET campo_26 = carga_reg[292,299]
            LET campo_27 = carga_reg[300,317]
            LET campo_28 = carga_reg[318,318]
            LET campo_29 = carga_reg[319,326]
            LET campo_30 = carga_reg[327,334]
            LET campo_31 = carga_reg[335,342]
            LET campo_32 = carga_reg[343,345]
            LET campo_33 = carga_reg[346,347]
            LET campo_34 = carga_reg[348,352]
            LET campo_35 = carga_reg[353,358]
            LET campo_36 = carga_reg[359,364]
            LET campo_37 = carga_reg[365,404]
            LET campo_38 = carga_reg[405,444]
            LET campo_39 = carga_reg[445,484]
            LET campo_40 = carga_reg[485,487]
            LET campo_41 = carga_reg[488,488]
            LET campo_42 = carga_reg[489,498]
            LET campo_43 = carga_reg[499,514]

            INSERT INTO safre_tmp:detalle1_lib_curp
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
                    campo_29,
                    campo_30,
                    campo_31,
                    campo_32,
                    campo_33,
                    campo_34,
                    campo_35,
                    campo_36,
                    campo_37,
                    campo_38,
                    campo_39,
                    campo_40,
                    campo_41,
                    campo_42,
                    campo_43
                   )
        END IF

        IF cont_reg = total_reg THEN
            LET campo_201 = carga_reg[001,002]
            LET campo_202 = carga_reg[003,004]
            LET campo_203 = carga_reg[005,007]
            LET campo_204 = carga_reg[008,015]
            LET campo_205 = carga_reg[016,018]
            LET campo_206 = carga_reg[019,020]
            LET campo_207 = carga_reg[021,022]
            LET campo_208 = carga_reg[023,031]
            LET campo_209 = carga_reg[032,040]

           INSERT INTO safre_tmp:sumario1_lib_curp
           VALUES (campo_201,
                   campo_202,
                   campo_203,
                   campo_204,
                   campo_205,
                   campo_206,
                   campo_207,
                   campo_208,
                   campo_209
                  )
        END IF

    END FOREACH

END FUNCTION
#############################################################################

FUNCTION revisa_datos()
#-----------------------

    DEFINE 
        rechazo_lote CHAR(3),
        rechazo_deta CHAR(3),
        l_reg RECORD LIKE tab_rch_lote.* ,
        x_reg RECORD LIKE tab_rdeta.* ,
        aux_pausa    CHAR(1)

    DEFINE 
        rechazo_09         CHAR(02),
        rechazo_001        CHAR(02),
        rechazo_002        CHAR(02),
        rechazo_003        CHAR(02),
        rechazo_019        CHAR(02),
        rechazo_020        CHAR(03),
        rechazo_021        CHAR(02)

    DEFINE 
        uno   CHAR(03),
        dos   CHAR(03),
        tre   CHAR(03),
        cua   CHAR(03),
        cin   CHAR(03),
        l_status_int  SMALLINT

    DEFINE 
        x_fecha   CHAR(10),
        xx_fecha  DATE

    # ENCABEZADO #
    SELECT campo1,
           campo2,
           campo3,
           campo18,
           campo19 
    INTO   rechazo_001,
           rechazo_002,
           rechazo_003,
           rechazo_lote,
           rechazo_019
    FROM safre_tmp:encabezado1_lib_curp

    SELECT * 
    INTO   l_reg.* 
    FROM   tab_rch_lote
    WHERE  rlote_cod = rechazo_lote

    IF STATUS  <> NOTFOUND THEN
      IF bnd_proceso THEN
        DISPLAY "Program stopped, ERROR DE PROCESO NO PUEDE CONTINUAR" 
        DISPLAY l_reg.rlote_cod 
        DISPLAY l_reg.rlote_desc_c 
        EXIT PROGRAM
      ELSE
        DISPLAY l_reg.rlote_cod AT 10,1
        DISPLAY l_reg.rlote_desc_c AT 11,1
        PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR" FOR aux_pausa
        EXIT PROGRAM
      END IF
    END IF

    IF rechazo_001 <> "01" THEN
      IF bnd_proceso THEN
        DISPLAY "Program stopped, Tipo registro de ser 01 en encabezado"
        EXIT PROGRAM
      ELSE
        CLEAR SCREEN
        DISPLAY "Tipo de registro debe ser 01 en ENCABEZADO" AT 10,1
        PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR" FOR aux_pausa
        EXIT PROGRAM
      END IF
    END IF

    IF rechazo_002 <> "07" THEN
      IF bnd_proceso THEN
        DISPLAY "Program stopped, Identificador de servicio de ser 07 en encabezado"
        EXIT PROGRAM
      ELSE
        CLEAR SCREEN
        DISPLAY "Identificador de Servicio debe ser 07 en ENCABEZADO" AT 10,1
        PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR" FOR aux_pausa
        EXIT PROGRAM
      END IF
    END IF

    IF rechazo_003 <> "71" THEN
      IF bnd_proceso THEN
        DISPLAY "Program stopped, Identificador de operacion de ser 71 en encabezado"
        EXIT PROGRAM
      ELSE
        CLEAR SCREEN
        DISPLAY "Identificador de Operacion debe ser 71 en ENCABEZADO" AT 10,1
        PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR" FOR aux_pausa
        EXIT PROGRAM
      END IF
    END IF

    IF rechazo_019 <> "03" THEN
      IF bnd_proceso THEN
        DISPLAY "Program stopped, Codigo de empresa operadora debe ser 03 en encabezado"
        EXIT PROGRAM
      ELSE
        CLEAR SCREEN
        DISPLAY "Codigo de Empresa Operadora debe ser 03 en ENCABEZADO" AT 10,1
        PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR" FOR aux_pausa
        EXIT PROGRAM
      END IF
    END IF

    IF rechazo_020 <> "001" THEN
      IF bnd_proceso THEN
        DISPLAY "Program stopped, Clave entidad origen debe ser 001 en encabezado"
        EXIT PROGRAM
      ELSE
        CLEAR SCREEN
        DISPLAY "Clave Entidad Origen debe ser 001 en ENCABEZADO" AT 10,1
        PROMPT "ERROR DE PROCESO NO PUEDE CONTINUAR" FOR aux_pausa
        EXIT PROGRAM
      END IF
    END IF

END FUNCTION
#############################################################################

FUNCTION Historico()
#----------------------------

    DEFINE reg		 	RECORD LIKE safre_tmp:detalle1_lib_curp.*
    DEFINE reg_mod		RECORD LIKE safre_af:afi_mae_modifica.*
    DEFINE reg_mae		RECORD LIKE safre_af:afi_mae_afiliado.*
    DEFINE vfecha_modifica 	DATE

    IF NOT bnd_proceso THEN
        OPEN WINDOW w_curp AT 8,4 WITH FORM "AFIC0152"
        ATTRIBUTE (BORDER)
    END IF

    SELECT COUNT(*) INTO vtotal FROM safre_tmp:detalle1_lib_curp

        DECLARE c_curp CURSOR FOR 
        SELECT * 
        FROM   safre_tmp:detalle1_lib_curp a
        ORDER BY a.nss_solicitud

        FOREACH c_curp INTO reg.* 

        SELECT *
          INTO reg_mae.*
          FROM afi_mae_afiliado
         WHERE @n_seguro  = reg.nss_solicitud

        LET vstatus_interno = reg_mae.status_interno

        LET cfecha_asig   = reg.fec_alta_curp[5,6], '/',
                            reg.fec_alta_curp[7,8], '/',
                            reg.fec_alta_curp[1,4]

        LET vfecha_asig   = cfecha_asig

        LET cfecha_recep  = reg.fecha_recep_sello[5,6], '/',
                            reg.fecha_recep_sello[7,8], '/',
                            reg.fecha_recep_sello[1,4]

        LET vfecha_recep  = cfecha_recep

        LET vfena         = reg.fecha_nacimiento[5,6], '/',
                            reg.fecha_nacimiento[7,8], '/',
                            reg.fecha_nacimiento[1,4]

        CASE reg.ident_tip_lib
          WHEN "00"
            LET vvig_venc       = vvig_venc + 1
            IF reg_mae.n_unico IS NULL OR
               reg_mae.n_unico MATCHES ' *' THEN
               LET vstatus_interno = 100
            ELSE
               LET vstatus_interno = 200
            END IF
          WHEN "01"
            LET vrech_ren = vrech_ren + 1
        END CASE

        INSERT INTO afi_lib_tram_curp
        VALUES (reg.nss_solicitud, 
                reg.curp_solicitud,
                reg.paterno, 
                reg.materno,
                reg.nombres,
                vfena,
                reg.sexo,
                reg.entidad_nacimiento,
                reg.nacionalidad,
                reg.tip_prob, 
                reg.fol_prob,
                reg.doc_prob,
                reg.ident_tip_lib,
                reg.cod_operacion,
                reg.diag_proceso,
                reg.status_renapo,
                reg.ent_asigna_curp,
                vfecha_asig,
                vfecha_recep,
                reg.hora_recep_sello,
                reg.consecutivo_recep,
                vhoy,
                g_reg.generar,
                g_usuario)

        CALL desmarca_cuenta(reg.nss_solicitud) #mc

        UPDATE afi_mae_afiliado 
           SET   status_interno = vstatus_interno
         WHERE @n_seguro        = reg.nss_solicitud

    END FOREACH

    -- actualiza historico de archivos curp afi_ctr_arh_lcurp

    INSERT INTO afi_ctr_arh_lcurp
    VALUES (g_reg.generar, vtotal, vvig_venc, vrech_ren, vhoy)

    IF NOT bnd_proceso THEN
        DISPLAY "                      DATOS LIBERACION TRAMITE CURP                            "
        AT 10,1 ATTRIBUTE ( REVERSE )

        DISPLAY "Total de Registros del lote     : ",
                 vtotal USING "#######&" AT 6,15 ATTRIBUTE ( BOLD )

        DISPLAY "Registros con Vigencia Vencida : ",
                 vvig_venc USING "#######&" AT 8,15 ATTRIBUTE ( BOLD )

        DISPLAY "Registros con Rechazo a RENAPO  : ",
                 vrech_ren USING "#######&" AT 10,15 ATTRIBUTE ( BOLD )

        DISPLAY "Archivo: ",
                 salida CLIPPED AT 12,5 ATTRIBUTE ( BOLD )

        PROMPT "Presione <ENTER> Para Continuar"
        ATTRIBUTE (REVERSE)
        FOR vresp
        ATTRIBUTE (REVERSE)

        CLOSE WINDOW w_curp
    ELSE
        DISPLAY "                      DATOS LIBERACION TRAMITE CURP                            "

        DISPLAY "Total de Registros del lote     : ",
                 vtotal USING "#######&" 

        DISPLAY "Registros con Vigencia Vencida  : ",
                 vvig_venc USING "#######&" 

        DISPLAY "Registros con Rechazo a RENAPO  : ",
                 vrech_ren USING "#######&" 

        DISPLAY "Archivo: ",
                 salida CLIPPED 
    END IF

    START REPORT rep_cif_ctr TO salida
    START REPORT listado_3 TO G_LISTA3              ----erm 15Marzo 2006
       OUTPUT TO REPORT rep_cif_ctr (g_reg.generar, vtotal, vvig_venc, 
                                     vrech_ren, vhoy)
       OUTPUT TO REPORT listado_3(g_reg.generar, vtotal, vvig_venc, 
                                     vrech_ren,salida)      ---erm 15 Marzo 2006
    FINISH REPORT rep_cif_ctr
    FINISH REPORT listado_3

    LET G_IMPRE3 = "lp ", G_LISTA3 CLIPPED
    RUN G_IMPRE3

    ERROR "Generando reporte. . ."
    SLEEP 2

RETURN

END FUNCTION
#############################################################################

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
        OPEN WINDOW window_1 AT 2,2 WITH FORM "AFIC0153" ATTRIBUTE( BORDER)
        DISPLAY " AFIC015        CONSULTA ARCHIVOS DE LIBERACION DE TRAMITE CURP                " AT 3,1 
        ATTRIBUTE(REVERSE) 
        DISPLAY HOY USING "dd-mm-yyyy" AT 3,63 
        ATTRIBUTE(REVERSE)

        MENU "CONSULTA ARCHIVOS CURP PROCESADOS"
            COMMAND "Consulta" "Consultar Archivos Curp"
                CALL Consulta()
            COMMAND "Salir" "Salir del Programa"
                EXIT MENU
        END MENU

        CLOSE WINDOW window_1

END FUNCTION
#############################################################################
 
FUNCTION Consulta()
#c-----------------

    DEFINE ga_record   ARRAY[3000] OF RECORD
        nombre_archivo LIKE afi_ctr_arh_lcurp.nombre_archivo,
        total_reg      LIKE afi_ctr_arh_lcurp.total_reg,
        total_vig_ven  LIKE afi_ctr_arh_lcurp.total_vig_ven,
        total_rec_ren  LIKE afi_ctr_arh_lcurp.total_rec_ren,
        fecha_proceso  LIKE afi_ctr_arh_lcurp.fecha_proceso
    END RECORD

    DEFINE
        vcount        INTEGER,
        vtotal        INTEGER,
        vvig_ven      INTEGER,
        vrec_ren      INTEGER

    DEFINE pos        SMALLINT

    SELECT count(*)            INTO vcount   FROM afi_ctr_arh_lcurp
    SELECT sum(@total_reg)     INTO vtotal   FROM afi_ctr_arh_lcurp
    SELECT sum(@total_vig_ven) INTO vvig_ven FROM afi_ctr_arh_lcurp
    SELECT sum(@total_rec_ren) INTO vrec_ren FROM afi_ctr_arh_lcurp

    DISPLAY "                                                                                  " AT 1,1
    DISPLAY "    CTRL-C cancela                                                                " AT 2,1
    DISPLAY " CONSULTA " AT 2,65

    DECLARE curp_12 CURSOR FOR
    SELECT *
    FROM afi_ctr_arh_lcurp
    ORDER BY 5

    LET pos = 1

    FOREACH curp_12 INTO ga_record[pos].*
        LET pos = pos + 1
    END FOREACH

   IF (pos-1) >= 1 THEN
        CALL  SET_COUNT(pos-1)
        DISPLAY BY NAME vcount
        DISPLAY BY NAME vtotal 
        DISPLAY BY NAME vvig_ven 
        DISPLAY BY NAME vrec_ren 

        DISPLAY ARRAY ga_record TO scr_1.*

        ON KEY (INTERRUPT)
            CLEAR FORM
            EXIT DISPLAY

        END DISPLAY
    ELSE
        ERROR "ARCHIVO DE LIBERACION DE TRAMITE CURP VACIO"
    END IF

END FUNCTION
#############################################################################

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
#############################################################################

REPORT rep_cif_ctr (lgenerar, ltotal, lvig_venc, lrech_ren, lhoy)

   DEFINE
     lgenerar	CHAR(20),
     ltotal	,
     lvig_venc	,
     lrech_ren	INTEGER,
     lhoy	DATE 

   OUTPUT
     PAGE 	LENGTH  90
     TOP	MARGIN	0
     BOTTOM	MARGIN	0
     LEFT 	MARGIN 	0
     RIGHT	MARGIN	0

   FORMAT
     ON EVERY ROW
        PRINT COLUMN 05, "DATOS PROCESADOS LIBERACION TRAMITE CURP",
              COLUMN 58, "AFIC015"

        PRINT COLUMN 05, "ARCHIVO: ", lgenerar CLIPPED,
              COLUMN 55, TODAY USING "DD/MM/YYYY"
        SKIP 3 LINE

        PRINT COLUMN 10, "Total de Registros del lote		 :",
                          ltotal     USING "#######&"
        PRINT

        PRINT COLUMN 10, "Total de Registros vigencia vencida	 :",
              		  lvig_venc  USING "#######&"
        PRINT

        PRINT COLUMN 10, "Total de Registros con Rechazo a RENAPO: ", 
                          lrech_ren  USING "#######&"
  
         
END REPORT
#############################################################################
FUNCTION desmarca_cuenta(vnss)
#dc----------------------------------------------------

   DEFINE
       vnss           CHAR(11),
       vmarca         SMALLINT,
       vusuario       CHAR(8),
       vn_folio          DECIMAL(10,0),
       pestado_marca  SMALLINT,
       pmarca_causa   SMALLINT,
       vcorrelativo   INTEGER

   LET pestado_marca = 0
   LET pmarca_causa  = 0
   LET vmarca        = 605

   SELECT d.correlativo
   INTO   vcorrelativo
   FROM   cta_act_marca d
   WHERE  d.nss = vnss
   AND    d.marca_cod = 605

   PREPARE eje_desmarca FROM v_desmarca

   EXECUTE eje_desmarca
   USING vnss,
         vmarca,
         vcorrelativo,
         pestado_marca,
         pmarca_causa,
         g_usuario

END FUNCTION
#############################################################################

FUNCTION marca_cuenta(vn_seguro, vn_folio)
#mc--------------------

  DEFINE 
      pmarca_causa SMALLINT,
      vn_seguro    CHAR(11), 
      vn_folio     DECIMAL(10,0)

  LET pmarca_causa = 0

  LET ejecuta = "EXECUTE PROCEDURE marca_cuenta(",
                "'",vn_seguro,"'",
                ",",edo_proc,
                ",",vn_folio,
                ",",vmarca_estado,
                ",",vcodigo_rechazo,
                ",",pmarca_causa,
                ",","'","'", ",",
                "'",g_usuario,"'",")"

  LET ejecuta = ejecuta CLIPPED
 
  PREPARE clausula_spl FROM ejecuta
  
  DECLARE cursor_marca CURSOR FOR clausula_spl

  OPEN cursor_marca

  FETCH cursor_marca INTO xcodigo_marca, xcodigo_rechazo

  CLOSE cursor_marca

END FUNCTION
#############################################################################


REPORT listado_3(lgenerar, ltotal, laprobados, lrechazados,lnombre)    ---erm 13 Marzo 2006

  DEFINE
    cont_reg    INTEGER,
    l_sol       CHAR(12),
    lnombre     CHAR(200),
    lgenerar    CHAR(30),
    ltotal      SMALLINT,
    laprobados  SMALLINT,
    lrechazados SMALLINT

  OUTPUT
    LEFT MARGIN   0
    RIGHT MARGIN  0
    TOP MARGIN    0
    BOTTOM MARGIN 0

  FORMAT
--    PAGE HEADER
--    ON EVERY ROW

    ON LAST ROW

      PRINT
        COLUMN 03,"========================================",
        COLUMN 40,"==================================================="
      PRINT
        COLUMN 15,"     LIBERACION DE CURP "
      PRINT
        COLUMN 03,"----------------------------------------",
        COLUMN 40,"---------------------------------------------------"
      SKIP 2 LINE
      PRINT
        COLUMN 03,"Fecha                  : ", TODAY USING "dd-mm-yyyy"
      SKIP 1 LINE
      PRINT 
        COLUMN 03,"Clave Operador         : ", g_usuario CLIPPED
      SKIP 1 LINE
      PRINT
        COLUMN 03,"Nombre Archivo Procesar: ","__________________________________"
      SKIP 1 LINE
      PRINT
        COLUMN 03,"Nombre de Archivo Procesado: ",lgenerar CLIPPED
      SKIP 1 LINE
      PRINT 
        COLUMN 03,"Hora de Generacion Proceso : ",tiempo CLIPPED
      SKIP 1 LINE
      PRINT
        COLUMN 03,"No. Registros del Lote              : ", ltotal USING "#######&"
      SKIP 1 LINE
      PRINT
        COLUMN 03,"No. Registros Vencidos              : ", laprobados USING "#######&"
      SKIP 1 LINE
      PRINT
        COLUMN 03,"No. Registros con Rechazos a RENAPO : ", lrechazados USING "#######&"
      SKIP 1 LINE
      PRINT
         COLUMN 03,"Nombre y Ruta Reporte a Detalle :"
      PRINT
        COLUMN 05,lnombre CLIPPED
      SKIP 2 LINE
      PRINT
        COLUMN 03,"========================================",
        COLUMN 40,"==================================================="

END REPORT