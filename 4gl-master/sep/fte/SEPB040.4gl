DATABASE safre_af
GLOBALS
DEFINE g_folio INTEGER ,
       g_nss_separado CHAR(011),
       g_nss_separador CHAR(011),
       g_fecha_proceso DATE     
DEFINE g_enter CHAR(1)



END GLOBALS
MAIN
  OPTIONS INPUT WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT


LET g_folio = ARG_VAL(1)
LET g_nss_separado = ARG_VAL(2)
LET g_nss_separador = ARG_VAL(3)
LET g_fecha_proceso = ARG_VAL(4)


    OPEN   WINDOW  SEPM040  AT  2,2  WITH FORM "SEPM040" ATTRIBUTE(BORDER)
    DISPLAY  "SEPM040              REVERSO DE LIQUIDACION DE SEPARACION                         "    AT  3,1 ATTRIBUTE  (REVERSE)
    DISPLAY  "Fecha:",g_fecha_proceso USING "DD-MM-YYYY" AT 3,60 ATTRIBUTE(REVERSE)
    DISPLAY  "FOLIO         :",g_folio              AT  4,1
    DISPLAY  "NSS SEPARADO  :",g_nss_separado       AT  5,1
    DISPLAY  "NSS SEPARADOR :",g_nss_separador      AT  6,1


    WHILE  TRUE
      PROMPT " DESEA CONFIRMAR EL REVERSO DE LA PAREJA DE SEPARACION [S/N] ? " FOR g_enter
           IF    g_enter  MATCHES "[sSnN]" THEN
              IF   g_enter  MATCHES "[sS]" THEN
                     EXIT WHILE
                ELSE
                     EXIT PROGRAM
              END IF
           END IF
    END WHILE


DELETE FROM dis_cuenta
WHERE folio = g_folio
AND nss in (g_nss_separado,g_nss_separador)
AND fecha_conversion = g_fecha_proceso

DISPLAY "ELIMINANDO REGISTROS LIQUIDADOS.......OK" AT 9,3

DELETE FROM dis_provision
WHERE folio = g_folio
AND (nss    = g_nss_separado
 OR nss    = g_nss_separador);

DISPLAY "ELIMINANDO REGISTROS PROVISIONADOS.......OK" AT 10,3

DELETE FROM  sep_reg_patro_separador
WHERE folio = g_folio
AND   nss_separado = g_nss_separado 
AND   nss_separador = g_nss_separador

DISPLAY "ELIMINANDO REGISTROS DE PATRONES A SEPARAR.......OK" AT 11,3

UPDATE sep_det_reg_sol_reclamante 
SET estado = 5
WHERE folio = g_folio
AND n_seguro = g_nss_separado
AND nss = g_nss_separador

DISPLAY "ACTUALIZANDO MAESTRO DE SEPARACION.......OK" AT 12,3

DELETE FROM sep_his_dis_cuenta
WHERE folio = g_folio
AND (nss    = g_nss_separado
 OR nss    = g_nss_separador);

DELETE FROM sep_mov_para_separar
WHERE folio = g_folio
AND (nss    = g_nss_separado
 OR nss    = g_nss_separador);

DELETE FROM sep_movimientos_pendientes
WHERE folio = g_folio
AND (nss    = g_nss_separado
 OR nss    = g_nss_separador);

DELETE FROM sep_movimientos_separados
WHERE folio = g_folio
AND (nss    = g_nss_separado
 OR nss    = g_nss_separador);

DELETE FROM sep_tmp_viv_280
WHERE folio = g_folio
AND (nss    = g_nss_separado
 OR nss    = g_nss_separador);

DELETE FROM sep_mov_inconsistentes
WHERE folio = g_folio
AND (nss    = g_nss_separado
 OR nss    = g_nss_separador);

DISPLAY "ELIMINANDO HISTORICO DE MOV ENTRE SIEFORES.......OK" AT 13,3

DELETE FROM tes_solicitud
WHERE nss = g_nss_separador
AND   tipo_traspaso = 11
AND folio_origen = g_folio

DISPLAY "ELIMINANDO SOLICITUDES DE TES INDEBIDAS POR SEPARACION.......OK" 
AT 14,3

PROMPT  "REVERSO FINALIZADO....<enter> PARA SALIR" FOR char g_enter

END MAIN

