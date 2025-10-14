###############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                            #
#Propietario       => E.F.P.                                                  #
#Programa AFIC010  => RECIBE ARCHIVO RESPUESTA CONFRONTA POSIBLES DUPLICADOS  #
#Fecha             => 13 DE OCTUBRE DE 2000.                                  #
#Actualizado       => ALEJANDRO CAMPOS SUAREZ.                                #
#Sistema           => AFI                                                     #
#Actualizado       => EDUARDO RESENDIZ MEDINA  29 MARZO 2006                  #
###############################################################################

DATABASE safre_af

GLOBALS

       DEFINE reg_cza_dup RECORD
              tipo_registro    CHAR(02) ,
              id_servicio      CHAR(02) ,
              id_operacion     CHAR(02) ,
              tipo_ent_origen  CHAR(02) ,
              cve_ent_origen   CHAR(03) ,
              tipo_ent_dest    CHAR(02) ,
              cve_ent_dest     CHAR(03) ,
              fecha_presentacion        DATE     ,
              consecutivo      SMALLINT ,
              cod_result       CHAR(02) ,
              mot_rechazo      CHAR(09)
       END RECORD

       DEFINE reg_det_dup_sol RECORD
              tipo_registro    CHAR(02) ,
              contador         DECIMAL(10,0),
              tipo_ent_sol     CHAR(02) ,
              cve_ent_sol      CHAR(03) ,
              curp_sol         CHAR(18) ,
              nss_sol          CHAR(11) ,
              rfc_sol          CHAR(13) ,
              paterno          CHAR(40) ,
              materno          CHAR(40) ,
              nombres          CHAR(40) ,
              sexo             CHAR(01) ,
              ent_nac          CHAR(02) ,
              fena             DATE     ,
              nombre_imss      CHAR(50) ,
              doc_prob         CHAR(01) ,
              reg_asociados    SMALLINT ,
              diag_aclara      CHAR(02) ,
              cod_result       CHAR(02) ,
              diag_proceso1    CHAR(03) ,
              diag_proceso2    CHAR(03) ,
              diag_proceso3    CHAR(03) ,
              diag_proceso4    CHAR(03) ,
              diag_proceso5    CHAR(03) ,
              id_lote_origen   CHAR(16) ,
              fecha_presentacion DATE,     ---se elimina
              estado           SMALLINT,    -- se elimina
              id_origen_reg    CHAR(01)
       END RECORD

       DEFINE reg_det_dup_aso RECORD
              tipo_registro    CHAR(02) ,
              contador         DECIMAL(10,0),
              nss_sol          CHAR(11) ,
              tipo_ent_dup     CHAR(02) ,
              cve_ent_dup      CHAR(03) ,
              curp_dup         CHAR(18) ,
              nss_dup          CHAR(11) ,
              rfc_dup          CHAR(13) ,
              paterno          CHAR(40) ,
              materno          CHAR(40) ,
              nombres          CHAR(40) ,
              sexo             CHAR(01) ,
              ent_nac          CHAR(02) ,
              fena             DATE     ,
              nombre_imss      CHAR(50) ,
              diag_aclara      CHAR(02) ,
              cod_result       CHAR(02) ,
              diag_proceso1    CHAR(03) ,
              diag_proceso2    CHAR(03) ,
              diag_proceso3    CHAR(03) ,
              diag_proceso4    CHAR(03) ,
              diag_proceso5    CHAR(03) ,
              id_lote_origen   CHAR(16) ,
              fecha_presentacion DATE,
              estado           SMALLINT,
              id_origen_reg    CHAR(01)
       END RECORD
              
       DEFINE reg_sum_dup     RECORD
              tipo_registro    CHAR(02) ,
              num_registros    DECIMAL(9,0),
              tot_reg_aso      DECIMAL(9,0)
       END RECORD

       DEFINE enter            CHAR(1)  ,
              vcve_ent_sol     CHAR(3)  ,
              usuario          CHAR(8)  , 
              HORA             CHAR(8)  , 
              c_fecha8         CHAR(8)  , 
              c_fecha10        CHAR(10) , 
              vnss_sol         CHAR(11) , 
              vcurp_sol        CHAR(18) ,
              vrazon_social    CHAR(50) ,
              archivo_dupli    CHAR(200),
              salida           CHAR(200),
              carga_reg        CHAR(380),
              HOY              DATE     ,
              i                SMALLINT ,
              pos              SMALLINT ,
              vtotal           INTEGER  ,
              vsol             INTEGER  ,
              vaso             INTEGER  ,
              cuantos          INTEGER  ,
              nss_tot          INTEGER  ,
              cont             INTEGER  ,
              vfolio           INTEGER

       DEFINE nom_archivo      CHAR(16),
	      varchivo         CHAR(16)

       DEFINE reg_1 RECORD 
              nss_sol          CHAR(11),
              curp_sol         CHAR(18)
       END RECORD

       DEFINE g_seg_modulo RECORD LIKE seg_modulo.*  

       DEFINE arr_1 ARRAY[10000] OF RECORD 
              nss_sol          CHAR(11),
              curp_sol         CHAR(18)
       END RECORD

       DEFINE arr_2 ARRAY[10000] OF RECORD 
              cve_ent_asc      CHAR(03),
              nss_asc          CHAR(11),
              curp_asc         CHAR(18)
       END RECORD

       DEFINE vnss_sol_2       INTEGER
       DEFINE vcurp_sol_2      INTEGER
       DEFINE carga_valida     CHAR(380)
       DEFINE tot_reg          INTEGER
       DEFINE g_lista          CHAR(200)
       DEFINE vcod_afore       CHAR(05)
       DEFINE vimpre           CHAR(200)

       DEFINE carga_val        RECORD
              id_origen        CHAR(01),
              nss              CHAR(11),
              curp             CHAR(18)
       END RECORD


END GLOBALS

MAIN

    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP,
        PROMPT LINE LAST,
        ACCEPT KEY CONTROL-I 

    CALL inicio()
    CALL carga()

END MAIN

FUNCTION inicio()
#i---------------

    LET HOY  = TODAY
    LET HORA = TIME

    SELECT *, user
    INTO   g_seg_modulo.*, usuario
    FROM   seg_modulo
    WHERE  modulo_cod = 'afi'

    SELECT codigo_afore,razon_social
    INTO   vcod_afore,vrazon_social
    FROM   tab_afore_local

    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp
        DROP TABLE paso_dupli
        CREATE TABLE paso_dupli (n_registros CHAR(380)) 
        DATABASE safre_af
    WHENEVER ERROR STOP

   LET salida = g_seg_modulo.ruta_listados CLIPPED,"/",usuario CLIPPED,
                ".RESP_DUP.",
                HOY USING "DDMMYY","_",HORA

   LET g_lista = g_seg_modulo.ruta_listados CLIPPED,"/",usuario CLIPPED,
                ".REC_RES_CONF_DUP.",
                HOY USING "DDMMYY"

END FUNCTION

FUNCTION Inicializa()
#i-------------------

    INITIALIZE arr_1[pos].* TO NULL
    INITIALIZE arr_2[pos].* TO NULL

END FUNCTION

FUNCTION carga()
#p----------------

    OPEN WINDOW ventana_1 AT 3,3 WITH FORM "AFIC0061" ATTRIBUTE(BORDER)

    DISPLAY " [ Esc ] Aceptar                                          [ Ctrl-C ] Salir    " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY " AFIC010     RECEPCION RESPUESTA CONFRONTA POSIBLES DUPLICADOS                 " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)
    DISPLAY "                                                                               " AT 19,1

    INPUT BY NAME nom_archivo WITHOUT DEFAULTS

    AFTER FIELD nom_archivo                 
        IF nom_archivo IS NULL THEN     
            ERROR "Campo NO puede ser NULO" 
            NEXT FIELD nom_archivo          
        END IF                              

        SELECT "X"
	FROM   afi_ctr_arh_dup
	WHERE  nombre_archivo = nom_archivo

	IF SQLCA.SQLCODE = 0 THEN
	    ERROR "ARCHIVO YA PROCESADO"
	    SLEEP 2
	    ERROR " "
	    LET nom_archivo = NULL
	    CLEAR FORM
	    NEXT FIELD nom_archivo
        END IF

    ON KEY (ESC) 
       IF nom_archivo IS NULL THEN     
           ERROR "Campo NO puede ser NULO" 
           NEXT FIELD nom_archivo          
       END IF                              
    
       LET archivo_dupli = g_seg_modulo.ruta_rescate CLIPPED,"/",
                           nom_archivo CLIPPED 

       WHENEVER ERROR CONTINUE 
          ERROR "PROCESANDO INFORMACION "

          LOAD FROM archivo_dupli 
          INSERT INTO safre_tmp:paso_dupli

       WHENEVER ERROR STOP

          SELECT count(*)
          INTO   cuantos
          FROM   safre_tmp:paso_dupli

          IF SQLCA.SQLCODE <> 0 THEN 
             LET cuantos = 0
          END IF 

          IF cuantos = 0 THEN
             ERROR " NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO "
             SLEEP 3
             NEXT FIELD nom_archivo
             ERROR ""
          ELSE
             --->erm
             DECLARE c_valida CURSOR FOR
             SELECT * 
             FROM safre_tmp:paso_dupli
             LET cont = 0
          
             FOREACH c_valida INTO carga_valida

               CASE carga_valida[279,279]
                 WHEN "1"
                   LET carga_val.id_origen = carga_valida[279,279]
                   LET carga_val.nss       = carga_valida[036,046]
                   IF carga_val.nss IS NULL OR
                      carga_val.nss = "           " THEN
                      ERROR "ARCHIVO RECHAZADO, REGISTROS.CON ID.ORIGEN REG.= "
                            ,carga_val.id_origen CLIPPED," SIN NSS, "
                      PROMPT "PROCESO CANCELADO <ENTER> P/SALIR " FOR enter
                      EXIT PROGRAM
                   END IF
                 WHEN "2"
                   LET carga_val.id_origen = carga_valida[279,279]
                   LET carga_val.curp      = carga_valida[018,035]
                   IF carga_val.curp IS NULL OR
                      carga_val.curp = "                  " THEN
                      ERROR "ARCHIVO RECHAZADO, REGISTROS.CON ID.ORIGEN REG.= "
                            ,carga_val.id_origen CLIPPED," SIN CURP,"
                      PROMPT "PROCESO CANCELADO <ENTER> P/SALIR " FOR enter
                      EXIT PROGRAM
                   END IF
               END CASE

               CASE carga_valida[287,287]
                 WHEN "1"
                   LET carga_val.id_origen = carga_valida[287,287]
                   LET carga_val.nss       = carga_valida[013,023]
                   IF carga_val.nss IS NULL OR
                      carga_val.nss = "           " THEN
                      ERROR "ARCHIVO RECHAZADO, REGISTROS.CON ID.ORIGEN REG.= "
                            ,carga_val.id_origen CLIPPED," SIN NSS, "
                      PROMPT "PROCESO CANCELADO <ENTER> P/SALIR " FOR enter
                      EXIT PROGRAM
                   END IF
                 WHEN "2"
                   LET carga_val.id_origen = carga_valida[287,287]
                   LET carga_val.curp      = carga_valida[029,046]
                   IF carga_val.curp IS NULL OR
                      carga_val.curp = "                  " THEN
                      ERROR "ARCHIVO RECHAZADO, REGS.CON ID.ORIGEN REG = "
                            ,carga_val.id_origen CLIPPED," SIN CURP,"
                      PROMPT "PROCESO CANCELADO <ENTER> P/SALIR " FOR enter
                      EXIT PROGRAM
                   END IF
               END CASE

             END FOREACH
---<erm
             EXIT INPUT
          END IF

       ON KEY (INTERRUPT)                                                  
           PROMPT "PROCESO CANCELADO ...<ENTER> PARA SALIR " FOR enter
           EXIT PROGRAM                                                    

    END INPUT                                                               

    DECLARE cursor_1 CURSOR FOR 
    SELECT * 
    FROM   safre_tmp:paso_dupli

    LET cont = 0
 
    START REPORT listado_2 TO salida
    START REPORT listado1  TO g_lista

    FOREACH cursor_1 INTO carga_reg

        CASE carga_reg[1,2]
            WHEN "01"       #---ENCABEZADO DE CURP DUPlICADOS---#
                LET reg_cza_dup.tipo_registro         = carga_reg[001,002]
                LET reg_cza_dup.id_servicio           = carga_reg[003,004]
                LET reg_cza_dup.id_operacion          = carga_reg[005,006]
                LET reg_cza_dup.tipo_ent_origen       = carga_reg[007,008] 
                LET reg_cza_dup.cve_ent_origen        = carga_reg[009,011] 
                LET reg_cza_dup.tipo_ent_dest         = carga_reg[012,013] 
                LET reg_cza_dup.cve_ent_dest          = carga_reg[014,016] 
                LET c_fecha8                          = carga_reg[017,024] 
                LET reg_cza_dup.consecutivo           = carga_reg[025,027] 
                LET reg_cza_dup.cod_result            = carga_reg[028,029] 
                LET reg_cza_dup.mot_rechazo           = carga_reg[030,038] 

                LET c_fecha10 = c_fecha8[5,6],"/",
                                c_fecha8[7,8],"/",
                                c_fecha8[1,4]

                LET reg_cza_dup.fecha_presentacion = c_fecha10

                IF reg_cza_dup.id_operacion <> '36' THEN
		    PROMPT "Archivo no es 'Respuesta Pos Dup', [Enter] p/salir"
		    FOR enter
		    EXIT PROGRAM
                END IF 

            WHEN "02"       #---DETALLE DE CURP DE SOLICITUD---#           
                LET reg_det_dup_sol.tipo_registro     = carga_reg[001,002]
                LET reg_det_dup_sol.contador          = carga_reg[003,012]
                LET reg_det_dup_sol.tipo_ent_sol      = carga_reg[013,014]
                LET reg_det_dup_sol.cve_ent_sol       = carga_reg[015,017] 
                LET reg_det_dup_sol.curp_sol          = carga_reg[018,035] 
                LET reg_det_dup_sol.nss_sol           = carga_reg[036,046] 
                LET reg_det_dup_sol.rfc_sol           = carga_reg[047,059] 
                LET reg_det_dup_sol.paterno           = carga_reg[060,099] 
                LET reg_det_dup_sol.materno           = carga_reg[100,139] 
                LET reg_det_dup_sol.nombres           = carga_reg[140,179] 
                LET reg_det_dup_sol.sexo              = carga_reg[180,180] 
                LET reg_det_dup_sol.ent_nac           = carga_reg[181,182]
                LET c_fecha8                          = carga_reg[183,190]
                LET reg_det_dup_sol.nombre_imss       = carga_reg[191,240]
                LET reg_det_dup_sol.doc_prob          = carga_reg[241,241]
                LET reg_det_dup_sol.reg_asociados     = carga_reg[242,243]
                LET reg_det_dup_sol.diag_aclara       = carga_reg[244,245]
                LET reg_det_dup_sol.cod_result        = carga_reg[246,247]
                LET reg_det_dup_sol.diag_proceso1     = carga_reg[248,250]
                LET reg_det_dup_sol.diag_proceso2     = carga_reg[251,253]
                LET reg_det_dup_sol.diag_proceso3     = carga_reg[254,256]
                LET reg_det_dup_sol.diag_proceso4     = carga_reg[257,259]
                LET reg_det_dup_sol.diag_proceso5     = carga_reg[260,262]
                LET reg_det_dup_sol.id_lote_origen    = carga_reg[263,278]
                LET reg_det_dup_sol.id_origen_reg     = carga_reg[279,279]

                LET c_fecha10 = c_fecha8[5,6],"/",
                                c_fecha8[7,8],"/",
                                c_fecha8[1,4]

                LET reg_det_dup_sol.fena = c_fecha10

                SELECT fecha_presentacion
                INTO   reg_det_dup_sol.fecha_presentacion
		FROM   afi_det_dup_sol
		WHERE  nss_sol        = reg_det_dup_sol.nss_sol
                AND    curp_sol       = reg_det_dup_sol.curp_sol
		AND    id_lote_origen = reg_det_dup_sol.id_lote_origen

		IF reg_det_dup_sol.fecha_presentacion IS NOT NULL THEN
                    UPDATE afi_det_dup_sol 
                    SET    cod_result     = reg_det_dup_sol.cod_result,
                           diag_proceso1  = reg_det_dup_sol.diag_proceso1,
                           diag_proceso2  = reg_det_dup_sol.diag_proceso2,
                           diag_proceso3  = reg_det_dup_sol.diag_proceso3,
                           diag_proceso4  = reg_det_dup_sol.diag_proceso4,
                           diag_proceso5  = reg_det_dup_sol.diag_proceso5
		    WHERE  nss_sol        = reg_det_dup_sol.nss_sol
                    AND    curp_sol       = reg_det_dup_sol.curp_sol
		    AND    id_lote_origen = reg_det_dup_sol.id_lote_origen
                
		    LET reg_det_dup_sol.estado = 1
                ELSE
		    LET reg_det_dup_sol.estado = 2
                END IF

            WHEN "03"       #---DETALLE DE CURP ASOCIADO---#           
                LET reg_det_dup_aso.tipo_registro     = carga_reg[001,002]
                LET reg_det_dup_aso.contador          = carga_reg[003,012]
                LET reg_det_dup_aso.nss_sol           = carga_reg[013,023]
                LET reg_det_dup_aso.tipo_ent_dup      = carga_reg[024,025] 
                LET reg_det_dup_aso.cve_ent_dup       = carga_reg[026,028] 
                LET reg_det_dup_aso.curp_dup          = carga_reg[029,046] 
                LET reg_det_dup_aso.nss_dup           = carga_reg[047,057] 
                LET reg_det_dup_aso.rfc_dup           = carga_reg[058,070] 
                LET reg_det_dup_aso.paterno           = carga_reg[071,110] 
                LET reg_det_dup_aso.materno           = carga_reg[111,150] 
                LET reg_det_dup_aso.nombres           = carga_reg[151,190] 
                LET reg_det_dup_aso.sexo              = carga_reg[191,191]
                LET reg_det_dup_aso.ent_nac           = carga_reg[192,193]
                LET c_fecha8                          = carga_reg[194,201]
                LET reg_det_dup_aso.nombre_imss       = carga_reg[202,251]
                LET reg_det_dup_aso.diag_aclara       = carga_reg[252,253]
                LET reg_det_dup_aso.cod_result        = carga_reg[254,255]
                LET reg_det_dup_aso.diag_proceso1     = carga_reg[256,258]
                LET reg_det_dup_aso.diag_proceso2     = carga_reg[259,261]
                LET reg_det_dup_aso.diag_proceso3     = carga_reg[262,264]
                LET reg_det_dup_aso.diag_proceso4     = carga_reg[265,267]
                LET reg_det_dup_aso.diag_proceso5     = carga_reg[268,270]
                LET reg_det_dup_aso.id_lote_origen    = carga_reg[271,286]
                LET reg_det_dup_aso.id_origen_reg     = carga_reg[287,287]

                LET c_fecha10 = c_fecha8[5,6],"/",
	                        c_fecha8[7,8],"/",
	                        c_fecha8[1,4]

                LET reg_det_dup_aso.fena = c_fecha10

                SELECT fecha_presentacion
		INTO   reg_det_dup_aso.fecha_presentacion
                FROM   afi_det_dup_aso
                WHERE  nss_dup        = reg_det_dup_aso.nss_dup
                AND    nss_sol        = reg_det_dup_aso.nss_sol
                AND    curp_dup       = reg_det_dup_aso.curp_dup
                AND    id_lote_origen = reg_det_dup_aso.id_lote_origen

		IF reg_det_dup_aso.fecha_presentacion IS NOT NULL THEN
                    UPDATE afi_det_dup_aso
                    SET    cod_result     = reg_det_dup_sol.cod_result,
                           diag_proceso1  = reg_det_dup_sol.diag_proceso1,
                           diag_proceso2  = reg_det_dup_sol.diag_proceso2,
                           diag_proceso3  = reg_det_dup_sol.diag_proceso3,
                           diag_proceso4  = reg_det_dup_sol.diag_proceso4,
                           diag_proceso5  = reg_det_dup_sol.diag_proceso5
                    WHERE  nss_dup        = reg_det_dup_aso.nss_dup
                    AND    nss_sol        = reg_det_dup_aso.nss_sol
                    AND    curp_dup       = reg_det_dup_aso.curp_dup
                    AND    id_lote_origen = reg_det_dup_aso.id_lote_origen
                
		    LET reg_det_dup_aso.estado = 1
                ELSE
		    LET reg_det_dup_aso.estado = 2
                END IF

            WHEN "09"  
                LET reg_sum_dup.tipo_registro         = carga_reg[001,002]
                LET reg_sum_dup.num_registros         = carga_reg[003,011]
                LET reg_sum_dup.tot_reg_aso           = carga_reg[012,020]

	        LET vtotal = reg_sum_dup.num_registros
        END CASE

        IF reg_det_dup_sol.nss_sol IS NOT NULL AND
           reg_det_dup_aso.nss_sol =
           reg_det_dup_aso.nss_sol THEN
            OUTPUT TO REPORT listado_2(reg_det_dup_sol.*, reg_det_dup_aso.*) #l2
        END IF

    END FOREACH

    FINISH REPORT listado_2

--    START REPORT listado1 TO g_lista

    SELECT COUNT(*)
    INTO   vsol
    FROM   afi_det_dup_sol
    WHERE  folio = vfolio
 
    LET nss_tot = vsol

--->erm 28 Mzo 2006
    LET vnss_sol_2  = 0
    LET vcurp_sol_2 = 0
    LET tot_reg     = 0

    SELECT COUNT(*)
    INTO   vnss_sol_2
    FROM   safre_tmp:paso_dupli
    WHERE  (n_registros[001,002] = 02 AND 
            n_registros[279,279] = 1)

    SELECT COUNT(*)
    INTO   vcurp_sol_2
    FROM   safre_tmp:paso_dupli
    WHERE  (n_registros[001,002] = 02 AND 
            n_registros[279,279] = 2)

    LET tot_reg = vnss_sol_2 + vcurp_sol_2

    DISPLAY "TOTAL DE REGISTROS POR NSS  : ", vnss_sol_2  AT 15,5
    DISPLAY "TOTAL DE REGISTROS POR CURP : ", vcurp_sol_2 AT 16,5
    DISPLAY "TOTAL DE REGISTROS          : ", tot_reg     AT 18,5

    LET tot_reg = vnss_sol_2 + vcurp_sol_2

    OUTPUT TO REPORT listado1(vnss_sol_2,vcurp_sol_2,tot_reg)
---<

--    DISPLAY BY NAME nss_tot

    SELECT COUNT(*)
    INTO   vaso
    FROM   afi_det_dup_aso
    WHERE  folio = vfolio
 
    LET vimpre = "chmod 777 ",g_lista
    RUN vimpre
 
    FINISH REPORT listado1
 
    LET vimpre = "lp ", g_lista
    --LET vimpre = "vi ", g_lista CLIPPED
    RUN vimpre

    ERROR "REPORTE IMPRESO.."
 
    LET pos = 1
    PROMPT "PROCESO TERMINADO, [Enter] p/salir" FOR enter

    INSERT INTO afi_ctr_arh_dup
    VALUES (nom_archivo, vtotal, vsol, vaso,vnss_sol_2, vcurp_sol_2, HOY)

END FUNCTION

REPORT listado_2(reg_det_dup_sol, reg_det_dup_aso)
#l2--------------------

    DEFINE reg_det_dup_sol RECORD
        tipo_registro    CHAR(02) ,
        contador         DECIMAL(10,0),
        tipo_ent_sol     CHAR(02) ,
        cve_ent_sol      CHAR(03) ,
        curp_sol         CHAR(16) ,
        nss_sol          CHAR(11) ,
        rfc_sol          CHAR(13) ,
        paterno          CHAR(40) ,
        materno          CHAR(40) ,
        nombres          CHAR(40) ,
        sexo             CHAR(01) ,
        ent_nac          CHAR(02) ,
        fena             DATE     ,
        nombre_imss      CHAR(50) ,
        doc_prob         CHAR(01) ,
        reg_asociados    SMALLINT ,
        diag_aclara      CHAR(02) ,
        cod_result       CHAR(02) ,
        diag_proceso1    CHAR(03) ,
        diag_proceso2    CHAR(03) ,
        diag_proceso3    CHAR(03) ,
        diag_proceso4    CHAR(03) ,
        diag_proceso5    CHAR(03) ,
        id_lote_origen   CHAR(16) ,
        fecha_presentacion DATE,	
        estado           SMALLINT,
        id_origen_reg    CHAR(01)
    END RECORD

    DEFINE reg_det_dup_aso RECORD
        tipo_registro    CHAR(02) ,
        contador         DECIMAL(10,0),
        nss_sol          CHAR(11) ,
        tipo_ent_dup     CHAR(02) ,
        cve_ent_dup      CHAR(03) ,
        curp_dup         CHAR(16) ,
        nss_dup          CHAR(11) ,
        rfc_dup          CHAR(13) ,
        paterno          CHAR(40) ,
        materno          CHAR(40) ,
        nombres          CHAR(40) ,
        sexo             CHAR(01) ,
        ent_nac          CHAR(02) ,
        fena             DATE     ,
        nombre_imss      CHAR(50) ,
        diag_aclara      CHAR(02) ,
        cod_result       CHAR(02) ,
        diag_proceso1    CHAR(03) ,
        diag_proceso2    CHAR(03) ,
        diag_proceso3    CHAR(03) ,
        diag_proceso4    CHAR(03) ,
        diag_proceso5    CHAR(03) ,
        id_lote_origen   CHAR(16) ,
        fecha_presentacion DATE,	
        estado           SMALLINT,
        id_origen_reg    CHAR(01)
    END RECORD

    DEFINE 
        vnombre1      CHAR(50),
        vnombre2      CHAR(50)

    OUTPUT
        PAGE LENGTH 155
	LEFT MARGIN 0
	RIGHT MARGIN 0
	TOP MARGIN 0
	BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT
            COLUMN 20,vcod_afore,"  ",vrazon_social,
            COLUMN 60,"FECHA  : ",TODAY USING "DD-MM-YYYY" 
        PRINT
            COLUMN 60,"NO PAG : ",PAGENO USING "####,###"
        PRINT
            COLUMN 20,"RESPUESTA A LA CONFRONTAR DEL",
                      "PROCESO DE POSIBLES DUPLICADOS"
        PRINT

    ON EVERY ROW
        LET vnombre1 = reg_det_dup_sol.paterno CLIPPED," ",
                       reg_det_dup_sol.materno CLIPPED," ",
                       reg_det_dup_sol.nombres CLIPPED

        LET vnombre2 = reg_det_dup_aso.paterno CLIPPED," ",
                       reg_det_dup_aso.materno CLIPPED," ",
                       reg_det_dup_aso.nombres CLIPPED

        PRINT 
            COLUMN 001,"DUP",
            COLUMN 005,reg_det_dup_sol.nss_sol,    
            COLUMN 030,reg_det_dup_sol.curp_sol,   
            COLUMN 050,vnombre1,    
            COLUMN 100,reg_det_dup_sol.reg_asociados USING "--", 
            COLUMN 113,reg_det_dup_sol.cve_ent_sol, 
            COLUMN 118,reg_det_dup_sol.fecha_presentacion USING"YYYYMMDD", 
            COLUMN 128,reg_det_dup_sol.cod_result,
            COLUMN 133,reg_det_dup_sol.diag_proceso1,
            COLUMN 138,reg_det_dup_sol.diag_proceso2,
            COLUMN 143,reg_det_dup_sol.diag_proceso3,
            COLUMN 148,reg_det_dup_sol.diag_proceso4,
            COLUMN 153,reg_det_dup_sol.diag_proceso5 
        PRINT 
            COLUMN 005,reg_det_dup_aso.nss_sol, 
            COLUMN 017,reg_det_dup_aso.nss_dup, 
            COLUMN 030,reg_det_dup_aso.curp_dup,
            COLUMN 050,vnombre2, 
            COLUMN 113,reg_det_dup_aso.cve_ent_dup, 
            COLUMN 118,reg_det_dup_aso.fecha_presentacion USING"YYYYMMDD", 
            COLUMN 128,reg_det_dup_aso.cod_result,
            COLUMN 133,reg_det_dup_aso.diag_proceso1,
            COLUMN 138,reg_det_dup_aso.diag_proceso2,
            COLUMN 143,reg_det_dup_aso.diag_proceso3,
            COLUMN 148,reg_det_dup_aso.diag_proceso4,
            COLUMN 153,reg_det_dup_aso.diag_proceso5 

END REPORT

REPORT listado1(rnss_sol_2,rcurp_sol_2,rnss_tot)

    DEFINE rnss_sol_2 ,
           rcurp_sol_2,
           rnss_tot INTEGER

    OUTPUT
    PAGE LENGTH 90
    LEFT MARGIN 0
    RIGHT MARGIN 0
    TOP MARGIN 0
    BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER
--        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT COLUMN 03,vrazon_social,
              COLUMN 65,"FECHA  : ",TODAY USING "DD-MM-YYYY" 
        PRINT COLUMN 65,"Pagina : ",PAGENO USING "####,###"
        SKIP 2 LINE
        PRINT COLUMN 03,"AFIC010       ",
                        "   RECEPCION RESPUESTA CONFRONTA POSIBLES DUPLICADOS   "
        PRINT COLUMN 03,"------------------------------------------------------------",
                        "--------------------------"
        SKIP 2 LINES
        PRINT COLUMN 03,"NOMBRE DEL ARCHIVO CARGADO   :  ",nom_archivo CLIPPED
        SKIP 2 LINE 
        PRINT COLUMN 03,"TOTAL DE REGISTROS POR NSS   :  ",rnss_sol_2  USING "##########"
        SKIP 1 LINE
        PRINT COLUMN 03,"TOTAL DE REGISTROS POR CURP  :  ",rcurp_sol_2 USING "##########"
        SKIP 1 LINE
        PRINT COLUMN 03,"TOTAL DE REGISTROS           :  ",rnss_tot    USING "##########"

END REPORT
