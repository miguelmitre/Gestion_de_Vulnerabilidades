################################################################################
#Proyecto          => SISTEMA DE AFORES ( SAFRE )                              #
#Propietario       => E.F.P.                                                   #
#Programa TRACIU02 => Consulta de Carga y actualizacion de Unificaciones       #
#Fecha creacion    => 10 Feb 2010                                              #
#Desarrado por     => MAGR                                                     #
#Sistema           => RET                                                      #
#Programa          => TRACIU02c --- Funciones de Consulta                      #
################################################################################
DATABASE safre_af

GLOBALS  "TRACIU02g.4gl"

FUNCTION f_preparac()
   DEFINE  lc_query              CHAR(1500)

    --- SELECCIONA FOLIOS SIN LIQUIDAR
    LET     lc_query    = "  SELECT folio, f_carga, COUNT(*)      ",
                          "    FROM safre_af:tra_issste_unifica  ",
                          "   WHERE f_actualiza  IS NULL          ",
                          "   GROUP BY 1,2                        ",
                          "   ORDER BY 1 DESC                     "
    PREPARE p_sel_uni1         FROM lc_query
    DECLARE d_sel_uni1    CURSOR FOR  p_sel_uni1
END FUNCTION


FUNCTION f_consulta_folio()
    DEFINE  lr_regt        RECORD
            folio                    INTEGER,
            f_carga                  DATE,
            cuantos                  INTEGER
            END RECORD

    DEFINE  lar_regt       ARRAY [200] OF RECORD
            folio                    INTEGER,
            f_carga                  DATE,
            cuantos                  INTEGER
    END RECORD
    DEFINE  li_i                     SMALLINT
    DEFINE  li_j                     SMALLINT
    DEFINE  li_folio                 INTEGER
    DEFINE  ld_fecha                 DATE

    CALL f_preparac()
    INITIALIZE lar_regt    TO  NULL
    LET        ld_fecha    =   TODAY
    LET        li_i = 1
    FOREACH d_sel_uni1    INTO   lar_regt[li_i].*
            LET    li_i   = li_i   + 1
            IF     li_i   > 199   THEN
                   Display " Consulta limitada a los ultimos 200 movimientos "
                   EXIT FOREACH
         END IF
    END FOREACH

   CALL SET_COUNT(li_i - 1)
   OPEN WINDOW f_03      AT 02,02 WITH FORM "TRACU0014"                                               ATTRIBUTE(BORDER)
   DISPLAY " [Esc] Selecciona [Ctrl-C] Salir                                              " AT 01, 03 ATTRIBUTE(REVERSE)
   DISPLAY "FOLIOS PENDIENTES DE ACTUALIZACION AL MAESTRO UNI-SAR-ISSSTE"    AT 05, 01 ATTRIBUTE(REVERSE)
   DISPLAY "            FOLIO      FECHA CARGA                     REGISTROS              " AT 06, 01 ATTRIBUTE(REVERSE)
   DISPLAY  ld_fecha                             TO       fecha_dia
   INPUT ARRAY lar_regt WITHOUT DEFAULTS FROM l_reg.*

        BEFORE FIELD folio
            LET li_j      = ARR_CURR()
            LET li_folio  = lar_regt[li_j].folio

        AFTER FIELD folio
            LET li_j      = ARR_CURR()
            LET li_folio  = lar_regt[li_j].folio

        ON KEY (ESC)
            LET  li_j      = ARR_CURR()
            LET  li_folio  = lar_regt[li_j].folio
            EXIT INPUT

        ON KEY (INTERRUPT)
            LET  li_folio = 0
            EXIT INPUT

        ON KEY (control-c)
            LET  li_folio = 0
            EXIT INPUT

   END INPUT
   CLOSE WINDOW f_03
   RETURN li_folio
END FUNCTION



