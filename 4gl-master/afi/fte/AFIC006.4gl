###############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                            #
#Propietario       => E.F.P.                                                  #
#Programa AFIC006  => RECIBE ARCHIVO PLANO PARA CONFRONTA POSIBLES DUPLICADOS #
#Fecha             => 13 DE OCTUBRE DE 2000.                                  #
#Actualizado       => ALEJANDRO CAMPOS SUAREZ.                                #
#Sistema           => AFI                                                     #
#Actualiado        => VERONICA LOPEZ SANCHEZ.   30 DE MARZO 2006              #
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
              fe_presen        DATE     ,
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
              c_fecha8         CHAR(8)  , 
              c_fecha10        CHAR(10) , 
              vnss_sol         CHAR(11) , 
              vcurp_sol        CHAR(18) ,
              archivo_dupli    CHAR(200),
              carga_reg        CHAR(380),
              hoy              DATE     ,
              i                SMALLINT ,
              pos              SMALLINT ,
              sw               SMALLINT ,
              nss_tot          INTEGER  ,
              vsol             INTEGER  ,
              vaso             INTEGER  ,
              cuantos          INTEGER  ,
              vtotal           INTEGER  ,
              cont             INTEGER  ,
              vfolio           INTEGER  ,
              reg_normales     INTEGER  ,
              reg_no_afi       INTEGER  ,
              g_lista          CHAR(300),
              cod_afore        CHAR(05) ,
              vrazon_social    CHAR(50) ,
              vimpre           CHAR(200)

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

END GLOBALS

MAIN

    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP,
        PROMPT LINE LAST,
        ACCEPT KEY CONTROL-I 

    CALL STARTLOG('AFIC006.log')
    CALL inicio()
    CALL carga()

END MAIN

FUNCTION inicio()
#i---------------

    LET HOY = TODAY

    SELECT *, user
    INTO   g_seg_modulo.*, usuario
    FROM   seg_modulo
    WHERE  modulo_cod = 'afi'

    SELECT codigo_afore,razon_social
    INTO   cod_afore,vrazon_social
    FROM   tab_afore_local

    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp
        DROP TABLE paso_dupli
        CREATE TABLE paso_dupli (n_registros CHAR(380)) 
        DATABASE safre_af
    WHENEVER ERROR STOP

    LET g_lista = g_seg_modulo.ruta_listados CLIPPED,"/",usuario CLIPPED,
                 ".REC_NOT_NSS_DUP.",
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

    DISPLAY " AFIC006     RECEPCION DE INFORMACION PARA POSIBLES DUPLICADOS                 " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

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

    SELECT MAX(folio) + 1
    INTO   vfolio
    FROM   afi_folio_dup 
   
    IF vfolio IS NULL THEN
        LET vfolio = 1
    END IF

    DISPLAY BY NAME vfolio

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
             EXIT INPUT
          END IF

       ON KEY (INTERRUPT)                                                  
           PROMPT "PROCESO CANCELADO ...<ENTER> PARA SALIR " FOR enter
           EXIT PROGRAM                                                    

    END INPUT                                                               

    INSERT INTO afi_folio_dup VALUES(vfolio)

    DECLARE cursor_1 CURSOR FOR 

    SELECT * 
    FROM   safre_tmp:paso_dupli

    LET cont = 0
 
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

                LET reg_cza_dup.fe_presen = c_fecha10

                IF reg_cza_dup.id_operacion = '36' THEN
	            SELECT "X"
		    FROM   afi_cza_dup
                    WHERE  fecha_presentacion = reg_cza_dup.fe_presen

		    IF SQLCA.SQLCODE <> 0 THEN
                        INSERT INTO afi_cza_dup     
                        VALUES (vfolio, reg_cza_dup.*, usuario, HOY)
                    END IF
                ELSE
		    PROMPT "Archivo no es 'Posibles Duplicados', Enter p/salir"
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

                IF reg_det_dup_sol.id_origen_reg = 1 THEN
                    IF reg_det_dup_sol.nss_sol = " " THEN
                       LET sw = 1
                    END IF
                ELSE
                    IF reg_det_dup_sol.curp_sol = " " THEN
                       LET sw = 1
                    END IF
                END IF
                
                IF sw = 1 THEN
                    PROMPT " ARCHIVO RECHAZADO <ENTER> PARA SALIR "
                    FOR CHAR enter
                    EXIT PROGRAM

                    DELETE
                    FROM    afi_cza_dup
                    WHERE   folio = vfolio    
                END IF
                    
                SELECT "X"
		FROM   afi_det_dup_sol
		WHERE  nss_sol            = reg_det_dup_sol.nss_sol
                AND    curp_sol           = reg_det_dup_sol.curp_sol
		AND    fecha_presentacion = reg_cza_dup.fe_presen

		IF SQLCA.SQLCODE <> 0 THEN
                    INSERT INTO afi_det_dup_sol 
                    VALUES(vfolio, 
			   reg_det_dup_sol.*, 
			   reg_cza_dup.fe_presen,
			   0,
                           " ",
			   usuario, 
			   HOY)
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

                {IF reg_det_dup_aso.id_origen_reg = 1 THEN
                    IF reg_det_dup_aso.nss_sol = " " THEN
                       LET sw = 1
                    END IF
                ELSE
                    IF reg_det_dup_aso.curp_dup = " " THEN
                       LET sw = 1
                    END IF
                END IF}

                IF (reg_det_dup_aso.id_origen_reg < 1) OR
                   (reg_det_dup_aso.id_origen_reg > 3) THEN
                   LET sw = 1
                END IF

                IF sw = 1 THEN
                    PROMPT " ARCHIVO RECHAZADO <ENTER> PARA SALIR "
                    FOR CHAR enter
                    EXIT PROGRAM

                    DELETE
                    FROM    afi_cza_dup
                    WHERE   folio = vfolio

                    DELETE
                    FROM    afi_det_dup_sol
                    WHERE   folio = vfolio 
                END IF

                SELECT "X"
                FROM   afi_det_dup_aso
		WHERE  nss_dup            = reg_det_dup_aso.nss_dup
		AND    nss_sol            = reg_det_dup_aso.nss_sol
                AND    curp_dup           = reg_det_dup_aso.curp_dup
               	AND    fecha_presentacion = reg_cza_dup.fe_presen

		IF SQLCA.SQLCODE <> 0 THEN
                    INSERT INTO afi_det_dup_aso 
                    VALUES(vfolio, 
                           reg_det_dup_aso.*, 
	                   reg_cza_dup.fe_presen,
		           0,
                           " ",
		           usuario, 
		           HOY)
                END IF

            WHEN "09"       #---DETALLE DE CURP ASOCIADO---#           
                LET reg_sum_dup.tipo_registro         = carga_reg[001,002]
                LET reg_sum_dup.num_registros         = carga_reg[003,011]
                LET reg_sum_dup.tot_reg_aso           = carga_reg[012,020]

	        SELECT "X"
	        FROM   afi_sum_dup
	        WHERE  fecha_presentacion = reg_cza_dup.fe_presen

	        IF SQLCA.SQLCODE <> 0 THEN
                    INSERT INTO afi_sum_dup 
                    VALUES(vfolio, 
			   reg_sum_dup.*, 
			   reg_cza_dup.fe_presen,
			   usuario, 
			   HOY)
                END IF
                
	        LET vtotal = reg_sum_dup.num_registros

        END CASE
    END FOREACH

    LET reg_normales = 0
    LET reg_no_afi   = 0

    SELECT COUNT(*)
    INTO   reg_normales
    FROM   afi_det_dup_sol
    WHERE  folio = vfolio
    AND    id_origen_reg = 1

    SELECT COUNT(*)
    INTO   reg_no_afi
    FROM   afi_det_dup_sol
    WHERE  folio = vfolio
    AND    id_origen_reg = 2

    SELECT COUNT(*)
    INTO   vsol
    FROM   afi_det_dup_sol
    WHERE  folio = vfolio
 
    LET nss_tot = vsol

    DISPLAY BY NAME nss_tot

    DISPLAY "TOTAL REGISTROS NORMALES     : ",reg_normales  AT 10,18
    DISPLAY "TOTAL REGISTROS NO AFILIADOS : ",reg_no_afi    AT 12,18

    START REPORT listado1 TO g_lista
    OUTPUT TO REPORT listado1(reg_normales,reg_no_afi,nss_tot)
    FINISH REPORT listado1

    SELECT COUNT(*)
    INTO   vaso
    FROM   afi_det_dup_aso
    WHERE  folio = vfolio

    LET vimpre = "chmod 777 ",g_lista
    RUN vimpre

    LET vimpre   = "lp ", g_lista
    --LET vimpre = "vi ", g_lista CLIPPED
    RUN vimpre

    ERROR "REPORTE IMPRESO.."
 
    LET pos = 1
    PROMPT "PROCESO TERMINADO, [Enter] p/salir" FOR enter

    INSERT INTO afi_ctr_arh_dup
    VALUES (nom_archivo,vtotal,vsol,vaso,reg_normales,reg_no_afi,HOY)

END FUNCTION

REPORT listado1(lreg_normales,lreg_no_afi,lnss_tot)

    DEFINE lreg_normales ,
           lreg_no_afi   ,
           lnss_tot      INTEGER

    OUTPUT
    PAGE LENGTH   155
    LEFT MARGIN   0
    RIGHT MARGIN  0
    TOP MARGIN    0
    BOTTOM MARGIN 0

    FORMAT
    PAGE HEADER
        PRINT COLUMN 03,cod_afore, "  ",vrazon_social,
              COLUMN 65,"FECHA  : ",TODAY USING "DD-MM-YYYY"
        PRINT COLUMN 65,"Pagina : ",PAGENO USING "####,###"
        SKIP 2 LINE
        PRINT COLUMN 03,"AFIC006       ",
                        "        RECEPCION DE SOLICITUDES POSIBLES DUPLICADOS"
        PRINT COLUMN 03,"------------------------------------------------------------",
                        "--------------------------"
        SKIP 2 LINES
        PRINT COLUMN 03,"FOLIO ASIGNADO POR EL SISTEMA:  ",vfolio  USING "##########"
        PRINT COLUMN 03,"NOMBRE DEL ARCHIVO CARGADO   :  ",nom_archivo CLIPPED
        SKIP 2 LINE
        PRINT COLUMN 03,"TOTAL DE REGISTROS POR NSS   :  ",lreg_normales  USING "##########"
        SKIP 1 LINE
        PRINT COLUMN 03,"TOTAL DE REGISTROS POR CURP  :  ",lreg_no_afi  USING "##########"
        SKIP 1 LINE
        PRINT COLUMN 03,"TOTAL DE REGISTROS           :  ",lnss_tot    USING "##########"

END REPORT
