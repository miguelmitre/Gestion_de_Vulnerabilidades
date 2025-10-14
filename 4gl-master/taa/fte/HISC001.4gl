#######################################################################
# PROGRAMA    : HISC001
# DESCRIPCION : RECIBE ARCHIVO DE RECHAZOS DE HISTORICO DE TRASPASO
# AUTOR       : JESUS DAVID YAÑEZ MORENO
# FECHA CREAC.: 10 NOV 2003
#######################################################################

DATABASE safre_af
GLOBALS 
DEFINE vfolio INTEGER
DEFINE enter CHAR(001) 
DEFINE HOY DATE 
DEFINE cuenta_reg INTEGER
DEFINE total_nss  INTEGER
DEFINE ruta_taa RECORD LIKE seg_modulo.*
DEFINE nombre_archivo CHAR(020)
DEFINE carga          CHAR(100)
DEFINE reg CHAR(270)
DEFINE reg_taa_his_rech_01 RECORD LIKE taa_his_rech_01.*
DEFINE reg_taa_his_rech_02 RECORD LIKE taa_his_rech_02.*
DEFINE reg_taa_his_rech_03 RECORD LIKE taa_his_rech_03.*
DEFINE reg_taa_his_rech_04 RECORD LIKE taa_his_rech_04.*
END GLOBALS

MAIN
    OPTIONS INPUT WRAP,
    ACCEPT KEY CONTROL-I,
    PROMPT LINE LAST
    DEFER INTERRUPT 
    
CALL init()

    OPEN WINDOW hisc0011  AT 3,3 WITH FORM "HISC0011" ATTRIBUTE(BORDER)
    DISPLAY " <CTRL-C> Cancelar                                                             " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " HISC001             CARGA ARCHIVO RECHAZO HISTORICOS TRASPASOS                " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD/MM/YYYY" AT 3,67 ATTRIBUTE(REVERSE)

INPUT BY NAME nombre_archivo	
AFTER FIELD nombre_archivo
  IF nombre_archivo IS NULL THEN 
     ERROR"Nombre de archivo no puede ser nulo"
  END IF
ON KEY(ESC)
  IF nombre_archivo IS NULL THEN 
     ERROR"Nombre de archivo no puede ser nulo"
  ELSE 
     EXIT INPUT
  END IF
ON KEY(INTERRUPT)
  ERROR "Proceso Cancelado..."
  SLEEP 2
  EXIT PROGRAM
END INPUT
   
DISPLAY "Procesando información ..." AT 19,1 ATTRIBUTE(REVERSE)

LET carga = ruta_taa.ruta_rescate CLIPPED,"/",nombre_archivo
whenever error continue
LOAD FROM carga INSERT INTO sube_registro_rch
whenever error stop
LET cuenta_reg = 0

SELECT COUNT(*)
INTO   cuenta_reg 
FROM   sube_registro_rch

IF (cuenta_reg IS NULL OR
   cuenta_reg = 0 )THEN
   ERROR'Archivo vacio o no existe...'
   SLEEP 3 
   EXIT PROGRAM
END IF


SELECT MAX(a.folio) + 1
INTO   vfolio
FROM   taa_his_folio a

INSERT INTO taa_his_folio VALUES(vfolio)

DECLARE cur_1 CURSOR FOR 
  SELECT *
  FROM sube_registro_rch
FOREACH cur_1 INTO reg

IF reg[1,2] = '01' THEN 
   LET reg_taa_his_rech_01.folio              = vfolio
   LET reg_taa_his_rech_01.tipo_registro      = reg[1,2]
   LET reg_taa_his_rech_01.ident_servicio     = reg[2,4]
   LET reg_taa_his_rech_01.ident_operacion    = reg[5,6]
   LET reg_taa_his_rech_01.tipo_ent_origen    = reg[7,8]
   LET reg_taa_his_rech_01.cve_ent_origen     = reg[9,11]
   LET reg_taa_his_rech_01.tipo_ent_destino   = reg[12,13]
   LET reg_taa_his_rech_01.cve_ent_destino    = reg[14,16]
   LET reg_taa_his_rech_01.fecha_presentacion = reg[17,24]
   LET reg_taa_his_rech_01.consecutivo_lote   = reg[25,27]
   LET reg_taa_his_rech_01.codigo_resultado   = reg[28,29]
   LET reg_taa_his_rech_01.motivo_rechazo1    = reg[30,32]
   LET reg_taa_his_rech_01.motivo_rechazo2    = reg[33,35]
   LET reg_taa_his_rech_01.motivo_rechazo3    = reg[36,38]
   LET total_nss = total_nss + 1
   DISPLAY "NSS cargados : ",total_nss AT 15,2
   INSERT INTO taa_his_rech_01 VALUES (reg_taa_his_rech_01.*)
END IF
IF reg[1,2] = '02' THEN
   LET reg_taa_his_rech_02.folio             = vfolio
   LET reg_taa_his_rech_02.cont_serv         = reg[3,12]
   LET reg_taa_his_rech_02.nss               = reg[59,69]
   LET reg_taa_his_rech_02.reg               = reg
   LET reg_taa_his_rech_02.codigo_resultado  = reg[254,255]
   LET reg_taa_his_rech_02.motivo_rechazo1   = reg[256,258]
   LET reg_taa_his_rech_02.motivo_rechazo2   = reg[259,261]
   LET reg_taa_his_rech_02.motivo_rechazo3   = reg[262,264]
   LET reg_taa_his_rech_02.motivo_rechazo4   = reg[265,267]
   LET reg_taa_his_rech_02.motivo_rechazo5   = reg[268,270]
   INSERT INTO taa_his_rech_02 VALUES (reg_taa_his_rech_02.*)
END IF
IF reg[1,2] = '03' THEN

   LET reg_taa_his_rech_03.folio             = vfolio
  LET reg_taa_his_rech_03.cont_serv         = reg[3,12]
  LET reg_taa_his_rech_03.nss               = reg[31,41]
  LET reg_taa_his_rech_03.consecutivo       = reg[44,46]
  LET reg_taa_his_rech_03.ident_proceso     = reg[42,43]
  LET reg_taa_his_rech_03.reg               = reg
  LET reg_taa_his_rech_03.codigo_resultado  = reg[254,255]
  LET reg_taa_his_rech_03.motivo_rechazo1   = reg[256,258]
  LET reg_taa_his_rech_03.motivo_rechazo2   = reg[259,261]
  LET reg_taa_his_rech_03.motivo_rechazo3   = reg[262,264]
  LET reg_taa_his_rech_03.motivo_rechazo4   = reg[265,267]
  LET reg_taa_his_rech_03.motivo_rechazo5   = reg[268,270]
  INSERT INTO taa_his_rech_03 VALUES (reg_taa_his_rech_03.*)
END IF

IF reg[1,2] = '04' THEN
   LET reg_taa_his_rech_04.folio             = vfolio
  LET reg_taa_his_rech_04.cont_serv         = reg[3,12]
  LET reg_taa_his_rech_04.nss               = reg[42,52]
  LET reg_taa_his_rech_04.consecutivo       = reg[73,75]
  LET reg_taa_his_rech_04.nss_uni           = reg[13,23]
  LET reg_taa_his_rech_04.ident_proceso     = reg[71,72]
  LET reg_taa_his_rech_04.reg               = reg
  LET reg_taa_his_rech_04.codigo_resultado  = reg[254,255]
  LET reg_taa_his_rech_04.motivo_rechazo1   = reg[256,258]
  LET reg_taa_his_rech_04.motivo_rechazo2   = reg[259,261]
  LET reg_taa_his_rech_04.motivo_rechazo3   = reg[262,264]
  LET reg_taa_his_rech_04.motivo_rechazo4   = reg[265,267]
  LET reg_taa_his_rech_04.motivo_rechazo5   = reg[268,270]
  INSERT INTO taa_his_rech_04 VALUES (reg_taa_his_rech_04.*)
END IF

END FOREACH

DISPLAY "NSS cargados : ",total_nss AT 15,2
PROMPT "<ENTER> Para salir..." ATTRIBUTE (REVERSE)FOR CHAR enter
END MAIN

FUNCTION init()
#i-------------
LET HOY = TODAY
LET total_nss = 0

CREATE TEMP TABLE sube_registro_rch(reg char(1000))

SELECT a.*
INTO   ruta_taa.*
FROM   seg_modulo a
WHERE  a.modulo_cod = 'taa'

END FUNCTION
