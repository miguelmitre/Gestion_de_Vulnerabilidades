###############################################################################
#Proyecto          => AFORE ( MEXICO )                                        #
#Propietario       => E.F.P.                                                  #
#Programa ESPC001  => ACTUALIZACION INDICADOR EDAD (cta_ctr_cuenta)           #
#Autor             => MAURO MUÑIZ CABALLERO / FERNANDO HERRERA HERNANDEZ      #
#Fecha             => 15 DE OCTUBRE DE 2009                                   #
#Sistema           => CTA                                                     #
###############################################################################

DATABASE safre_af

GLOBALS
  DEFINE
    g_param_cta        RECORD LIKE seg_modulo.*

  DEFINE
    s_codigo_afore     LIKE tab_afore_local.codigo_afore

  DEFINE g_reg         RECORD
    nss                CHAR(11),
    ind_edad           SMALLINT,
    criterio           SMALLINT
  END RECORD

  DEFINE
    hoy                DATE,
    actualiza_ind_sel  CHAR(300),
    contador           INTEGER,
    aceptado           INTEGER,
    rechazado          INTEGER,
    x_rechazo          SMALLINT,
    enter              CHAR(1)

END GLOBALS

MAIN 
  OPTIONS INPUT WRAP,
  PROMPT LINE LAST,
  ACCEPT KEY CONTROL-I
  DEFER INTERRUPT

  CALL STARTLOG("ESPC001.log")
  CALL inicio()            #i
  CALL proceso_principal() #pp
END MAIN
 
FUNCTION inicio()
#i-------------
  LET hoy = today

  SELECT codigo_afore
  INTO   s_codigo_afore
  FROM   tab_afore_local

  SELECT *
  INTO   g_param_cta.*
  FROM   seg_modulo
  WHERE  modulo_cod = 'cta'

   LET actualiza_ind_sel   = " EXECUTE FUNCTION fn_actualiza_ind_edad (?,?,?) "
   PREPARE eje_actualiza_ind_edad FROM actualiza_ind_sel

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

  LET contador  = 0
  LET aceptado  = 0
  LET rechazado = 0

  ERROR "PROCESANDO INFORMACION."

  DECLARE c1 CURSOR FOR
  SELECT a.nss, a.sie, b.criterio_edad
  FROM   safre_tmp:nss_dif_reg_ind a, cta_ctr_cuenta b
  WHERE  a.nss = b.nss
  AND    a.nss NOT  IN(SELECT unique nss
                      FROM   cta_det_sie_acep
                      WHERE  diagnostico1 <> '002')
  FOREACH c1 INTO g_reg.*

    LET contador = contador + 1

    DISPLAY "REGISTROS PROCESADOS INDICADOR EDAD: ", contador AT 13,3

    DECLARE c2 CURSOR FOR eje_actualiza_ind_edad
    OPEN    c2 USING g_reg.nss,
                     g_reg.ind_edad,
                     g_reg.criterio
    FETCH c2 INTO x_rechazo

    IF x_rechazo = 1 THEN
       LET aceptado  = aceptado  + 1
    ELSE
       LET rechazado = rechazado + 1
    END IF

    DISPLAY "REGISTROS ACEPTADOS:  ", aceptado  AT 14,3
    DISPLAY "REGISTROS RECHAZADOS: ", rechazado AT 15,3

  END FOREACH

  PROMPT "PROCESO FINALIZADO, PRESIONE ENTER PARA SALIR" FOR enter

END FUNCTION
