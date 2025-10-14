#############################################################################
#Proyecto          => AFORES ( MEXICO )                                     #
#Propietario       => E.F.P.                                                #
#Programa AFIC005  => CARGA DE ARCHIVO DISPERSION CURP                      #
#Sistema           => AFI.                                                  #
#Autor             => MAURO MUNIZ CABALLERO                                 #
#Fecha             => 18 DE ENERO DE 2001                                   #
#############################################################################

DATABASE safre_af

GLOBALS

    DEFINE g_reg  RECORD
        generar       CHAR(20)
    END RECORD

    DEFINE varchivo         CHAR(14)
    DEFINE hoy              DATE
    DEFINE aux_pausa        CHAR(01)
    DEFINE ejecuta          CHAR(100)
    DEFINE corr             CHAR(100)
    DEFINE total_reg        INTEGER
    DEFINE carga            CHAR(50)
    DEFINE g_plano1         INTEGER
    DEFINE vn_seguro        CHAR(11)
    DEFINE vn_folio         CHAR(18)
    DEFINE vcurp            CHAR(18)
    DEFINE vf_rechazo       CHAR(10)
    DEFINE vcurp_solicitud  CHAR(18)
    DEFINE vfec_alta_comp   CHAR(10)
    DEFINE vhoy             DATE
    DEFINE vvcurp           CHAR(18)
    DEFINE vtotal           INTEGER
    DEFINE vaprobados       INTEGER
    DEFINE vduplicados      INTEGER
    DEFINE vrechazados      INTEGER
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
    DEFINE g_usuario        CHAR(8)
    DEFINE vpaterno         CHAR(40)
    DEFINE vmaterno         CHAR(40)
    DEFINE vnombres         CHAR(40)

    DEFINE g_seg_modulo RECORD LIKE seg_modulo.*

    DEFINE reg_bat RECORD
        pid            INTEGER,
        proceso_cod    INTEGER,
        opera_cod      INTEGER,
        nombre_archivo CHAR(25)
    END RECORD

    DEFINE 
        bnd_proceso     SMALLINT,
        marca           SMALLINT,
        xcodigo_marca   SMALLINT ,
        xcodigo_rechazo SMALLINT

    DEFINE reg_comp1 RECORD LIKE safre_tmp:det_comp1.*
    DEFINE reg_comp2 RECORD LIKE safre_tmp:det_comp2.*
    DEFINE reg_comp3 RECORD LIKE safre_tmp:det_comp3.*
    DEFINE reg_comp4 RECORD LIKE safre_tmp:det_comp4.*
    DEFINE reg_comp5 RECORD LIKE safre_tmp:det_comp5.*

END GLOBALS

MAIN

    DISPLAY " "
    DISPLAY ".1"

    CALL STARTLOG("AFIC0052.log")
    CALL inicio() #i
    CALL hace_compara()       #hc

END MAIN

FUNCTION inicio()
#i---------------

    LET reg_bat.pid         = ARG_VAL(1)
    LET reg_bat.proceso_cod = ARG_VAL(2)
    LET reg_bat.opera_cod   = ARG_VAL(3)

    LET bnd_proceso         = 0

    LET marca               = 605

    IF reg_bat.pid THEN
        DISPLAY "INICIANDO PROCESO ..."
        LET bnd_proceso = 1
    END IF 

    LET hoy = TODAY
    LET g_reg.generar = "S"

    SELECT *, USER
    INTO   g_seg_modulo.*, g_usuario
    FROM   seg_modulo
    WHERE  modulo_cod = 'afi'

    WHENEVER ERROR CONTINUE
        DATABASE safre_tmp
        DROP TABLE safre_tmp:plano1_comp
    WHENEVER ERROR STOP

    CREATE TABLE safre_tmp:plano1_comp
        (n_registros CHAR(490))

    DATABASE safre_af

    CREATE TEMP TABLE tmp_comp4
	(tipo_registro char(2),
	 cve_operacion char(2),
	n_seguro char(11),
	n_rfc char(13),
	n_unico char(18),
	paterno char(40),
	materno char(40),
	nombres char(40),
	fena date,
	sexo smallint,
	estadon smallint,
	status_cta smallint,
	nss_proc char(11),
	n_rfc_proc char(13),
	n_unico_proc char(18),
	paterno_proc char(40),
	materno_proc char(40),
	nombres_proc char(40),
	fena_proc date,
	fecha_recepcion date,
	sexo_proc smallint,
	estadon_proc smallint,
	carta smallint,
	f_transf_lote date,
	consec_lote_dia smallint,
        estado smallint,
        factualiza date,
         usuario char(8)
	); 

END FUNCTION

FUNCTION hace_compara()
#hc--------------------

    ---DEFINE reg_det RECORD LIKE safre_tmp:tmp_det_notifica.*
    DEFINE reg_det RECORD LIKE safre_af:afi_det_notifica.*
    DEFINE reg_cza RECORD LIKE safre_tmp:tmp_cza_notifica.*
    DEFINE reg_mae RECORD LIKE safre_af:afi_mae_afiliado.*

    SELECT *
    INTO reg_cza.*
    FROM safre_tmp:tmp_cza_notifica

    DECLARE cur_det CURSOR FOR
    SELECT d.*
    ---FROM   safre_tmp:tmp_det_notifica d
    FROM   safre_af:afi_det_notifica d
    WHERE  d.factualiza >= '12/20/2002'

    FOREACH cur_det INTO reg_det.*
    SELECT a.*
    INTO   reg_mae.*
    FROM   safre_af:afi_mae_afiliado a
    WHERE  a.n_seguro = reg_det.n_seguro

    IF SQLCA.SQLCODE <> 0 THEN ----- tabla 1 nombre no existe
        INSERT INTO safre_tmp:det_comp1
	VALUES (99,
		reg_det.cve_operacion,
		reg_det.n_seguro,
		reg_mae.paterno,
		reg_mae.materno,
		reg_mae.nombres,
		reg_det.paterno,
		reg_det.materno,
		reg_det.nombres,
		reg_det.f_transf_lote,
		reg_cza.consec_dia)
    ELSE
	LET reg_mae.paterno = reg_mae.paterno CLIPPED
	LET reg_mae.materno = reg_mae.materno CLIPPED
	LET reg_mae.nombres = reg_mae.nombres CLIPPED
	LET reg_det.paterno_proc = reg_det.paterno_proc CLIPPED
	LET reg_det.materno_proc = reg_det.materno_proc CLIPPED
	LET reg_det.nombres_proc = reg_det.nombres_proc CLIPPED

        IF reg_mae.paterno = reg_det.paterno_proc AND
           reg_mae.materno = reg_det.materno_proc AND
           reg_mae.nombres = reg_det.nombres_proc THEN
            SELECT 'X'
	    FROM   afi_mae_modifica m
	    WHERE  m.n_seguro = reg_det.n_seguro
            GROUP BY 1

            IF SQLCA.SQLCODE <> 0 THEN  ---- tabla 2 no varia sin modif
		INSERT INTO safre_tmp:det_comp2
		VALUES(reg_det.*)  ---, reg_cza.consec_dia)
            ELSE
					---- tabla 3 hist modificaciones
                INSERT INTO safre_tmp:det_comp3   
                SELECT *,''
	        FROM   afi_mae_modifica m
	        WHERE  m.n_seguro = reg_det.n_seguro

                UPDATE safre_tmp:det_comp3
		SET    consec_lote_dia = reg_cza.consec_dia
		WHERE  consec_lote_dia IS NULL

					---- tabla 4 no varia con modif
		INSERT INTO safre_tmp:det_comp4
		VALUES(reg_det.*)  ---, reg_cza.consec_dia)

		INSERT INTO tmp_comp4
		VALUES(reg_det.*) ---, reg_cza.consec_dia)
            END IF
        ELSE      ----- tabla 1 nombre varia 
            INSERT INTO safre_tmp:det_comp1
	    VALUES (reg_det.tipo_registro,
		    reg_det.cve_operacion,
		    reg_det.n_seguro,
		    reg_mae.paterno,
		    reg_mae.materno,
		    reg_mae.nombres,
		    reg_det.paterno_proc,
		    reg_det.materno_proc,
		    reg_det.nombres_proc,
		    reg_det.f_transf_lote,
		    reg_cza.consec_dia)
        END IF
    END IF
    END FOREACH

					---- tabla 5 comparativo
    DECLARE cur_det5 CURSOR FOR
    SELECT b.n_seguro,
           MIN(a.fecha_modifica),
           b.paterno_proc,
           b.materno_proc,
           b.nombres_proc,
           a.paterno,
           a.materno,
           a.nombres,
	   ''
    FROM   safre_af:afi_mae_modifica a, tmp_comp4 b
    WHERE  a.n_seguro = b.n_seguro
    AND    a.paterno = b.paterno_proc
    AND    a.materno = b.materno_proc
    AND    a.nombres = b.nombres_proc
    GROUP BY 1,3,4,5,6,7,8

    FOREACH cur_det5 INTO reg_comp5.*
	LET reg_comp5.consec_lote_dia = reg_cza.consec_dia

        INSERT INTO safre_tmp:det_comp5 VALUES(reg_comp5.*)
    END FOREACH

END FUNCTION

