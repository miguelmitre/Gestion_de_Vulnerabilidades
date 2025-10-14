##########################################################################
#Proyecto          => AFORE ( MEXICO )                                   #
#Propietario       => E.F.P.                                             #
#Programa TUIC0001  => MODIFICA FECHAS DE FOLIOS TRASPASO UNIFIC. ISSSTE #
#Fecha             => 27 DE OCTUBRE    DE 2010                           #
#Autor             => JOSE FRANCISCO LUGO CORNEJO                        #
#Sistema           => TUI (TRASPASOS UNIFICACIÓN ISSSTE-IMSS             #
##########################################################################
DATABASE    safre_af
GLOBALS
   DEFINE   reg_1        ARRAY[2000]  OF    RECORD
            folio                           INTEGER        ,
            estado                          CHAR(17)       ,
            f_presentacion                  DATE           ,
            f_provision                  DATE           ,
            f_liquidacion                   DATE
                                      END   RECORD
   DEFINE   reg_2        ARRAY[2000]  OF    RECORD
            estado                          SMALLINT,
            desc_estado                     CHAR(30),
            total_cuentas                   INTEGER 
                                      END   RECORD
   DEFINE
            g_hoy                           DATE
   DEFINE  
            i_arr_1                         ,
            i_arr_2                         ,
            g_cur                           ,
            x                               ,
            g_scr                           ,
            g_procedente                    ,
            g_provisionada                  ,
            g_vencida                       SMALLINT
   DEFINE
            g_folio                         INTEGER
   DEFINE
            g_enter                         CHAR
END GLOBALS

MAIN
   OPTIONS 
            PROMPT    LINE LAST,
            INPUT     WRAP,
            ACCEPT    KEY      CONTROL-I
            DEFER     INTERRUPT
   CALL     F_001_modifica_folios()
END MAIN

FUNCTION    F_001_modifica_folios()
   CALL     STARTLOG  ("TUIC0001.log")
   LET      g_hoy                 =  TODAY
   SELECT   a.estado    INTO  g_procedente
     FROM   safre_af:taa_cd_edo_cedente a
    WHERE   a.descripcion         = "PROCEDENTE" 
      AND   a.tipo                =  2;
   SELECT   a.estado    INTO  g_provisionada
     FROM   safre_af:taa_cd_edo_cedente a
    WHERE   a.descripcion         = "PROVISIONADA" 
      AND   a.tipo                =  3;
   SELECT   a.estado    INTO  g_vencida
     FROM   safre_af:taa_cd_edo_cedente a
    WHERE   a.descripcion         = "VENCIDA" 
      AND   a.tipo                =  3;
   OPEN     WINDOW    ventana_1      AT  2,2 
            WITH      FORM    "TUIC0001"     ATTRIBUTE(BORDER)
   DISPLAY  " <<TUIC0001>>         MODIFICA  FOLIOS  DE  TRASPASOS UNIFICACIÓN  ISSSTE-IMSS ",
            "                 "    AT  1,2   ATTRIBUTE(REVERSE)
   DISPLAY  g_hoy     USING   "DD-MM-YYYY"   AT   1,63   ATTRIBUTE(REVERSE)
   DISPLAY  "   <<Enter>> Continuar                  <<Ctrl-c>> Salir",
            "                         "      AT   3,2    ATTRIBUTE(REVERSE)
   MENU     " TRASPASOS  UNIFICACIÓN  ISSSTE-IMSS "
            COMMAND   "Modifica" " Modifica Folios de Traspasos Unificación ISSSTE-IMSS"
                      CALL     F_010_Modifica()
            COMMAND   "Salir " " Salir de Programa"
            EXIT MENU
   END MENU
   CLOSE    WINDOW    ventana_1
END FUNCTION

FUNCTION    F_010_Modifica()
   DEFINE   l_select                  ,
            l_construct               CHAR(1000)
   FOR      x                 =  1   TO  20
            INITIALIZE        reg_1[x].*      TO  NULL
   END FOR   
   LET      int_flag                  =  FALSE
   CONSTRUCT  l_construct            ON  a.folio
                                   FROM  folio
            ON KEY(INTERRUPT)
                     EXIT     PROGRAM
            ON KEY ( RETURN )
                     LET      int_flag         =  FALSE
                     EXIT     CONSTRUCT
   END      CONSTRUCT
   LET      l_select                =
            " SELECT a.folio,  ",
            " b.descripcion, ",
            " a.fecha_presentacion, ",
            " a.fecha_provision, ",
            " a.fecha_liquidacion  ",
            " FROM    safre_af:tui_ctr_folio a, taa_cd_edo_cedente b",
            " WHERE   b.tipo    IN(2)  ",
            "   AND   a.estado       IN(101,102) ",
            "   AND   a.estado       =  b.estado ",
            "   AND ",l_construct   CLIPPED,
            " ORDER BY 1 DESC  "
   LET      l_select = l_select CLIPPED
   PREPARE  qry_consul           FROM  l_select 
   DECLARE  cursor_c           CURSOR  FOR    qry_consul
   LET      i_arr_1                 =  1
   FOREACH  cursor_c             INTO  reg_1[i_arr_1].*
            LET       i_arr_1          =  i_arr_1   +  1
   END FOREACH
   CALL     SET_COUNT(i_arr_1   -  1)
   IF       (i_arr_1-1)             <  1     THEN
            ERROR    "      NO  EXISTEN  FOLIOS     "
            PROMPT " TECLEE ENTER PARA  SALIR   "  FOR  g_enter
            RETURN
   END IF
   INPUT    ARRAY    reg_1    WITHOUT  DEFAULTS   FROM  scr_1.*
            BEFORE   ROW
                     LET      g_cur              =  ARR_CURR()
                     LET      g_scr              =  SCR_LINE()
            AFTER    FIELD    f_envio_saldo
                     IF       reg_1[g_scr].folio      >   0   AND  
                              reg_1[g_scr].f_provision IS NULL  THEN
                              ERROR   " LA FECHA NO PUEDE SER NULA "
                              NEXT  FIELD  f_envio_saldo 
                     END IF
                     IF       reg_1[g_scr].folio      >   0   THEN
                              NEXT  FIELD  f_liquidacion   
                     END IF
            AFTER    FIELD    f_liquidacion
                     IF       reg_1[g_scr].folio      >   0   AND  
                              reg_1[g_scr].f_liquidacion  IS  NULL  THEN
                              ERROR   " LA FECHA NO PUEDE SER NULA "
                              NEXT  FIELD  f_liquidacion   
                     END IF
            ON KEY ( ESC )
                     CALL     actualiza_fechas() 
                     CLEAR    FORM
                     EXIT     INPUT
            ON KEY(INTERRUPT)
                     ERROR    " MODIFICACION  TERMINADA..."
                     SLEEP    2
                     ERROR    ""
                     CLEAR    FORM
                     EXIT     INPUT
   END      INPUT
END FUNCTION

FUNCTION    actualiza_fechas()
   FOR      x          =  1        TO  6
            IF       reg_1[x].folio          IS  NULL  THEN
                     CONTINUE  FOR
            END IF
            UPDATE   safre_af:tui_ctr_folio
               SET   fecha_provision     =  reg_1[x].f_provision,
                     fecha_liquidacion      =  reg_1[x].f_liquidacion
             WHERE   folio                  =  reg_1[x].folio;
   END FOR
END FUNCTION
