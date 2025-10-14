##########################################################################
#Proyecto          => AFORE ( MEXICO )                                   #
#Propietario       => E.F.P.                                             #
#Programa TUIL0004  => CONSULTA DE FOLIOS DE TASPASOS AFORE-AFORE CEDENTE#
#Fecha             => 25 DE JULIO DE 2003                                #
#Autor             => JOSE FRANCISCO LUGO CORNEJO                        #
#Sistema           => TCAA(CEDENTE)                                      #
##########################################################################
DATABASE    safre_af
GLOBALS
   DEFINE   reg_1        ARRAY[2000]  OF    RECORD
            folio                           INTEGER        ,
            estado                          CHAR(17)       ,
            f_presentacion                  DATE           ,
            f_provision                     DATE           ,
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
            g_src                           ,
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
            INPUT     WRAP
            DEFER     INTERRUPT
   CALL     F_001_consulta_folios()
END MAIN

FUNCTION    F_001_consulta_folios()
   CALL     STARTLOG  ("TUIL0004.log")
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
            WITH      FORM    "TUIL0004"     ATTRIBUTE(BORDER)
   DISPLAY  " <<TUIL0004>>         CONSULTA  FOLIOS  DE  TRASPASOS",
            "                 "    AT  1,2   ATTRIBUTE(REVERSE)
   DISPLAY  g_hoy     USING   "DD-MM-YYYY"   AT   1,63   ATTRIBUTE(REVERSE)
   DISPLAY  "   <<Enter>> Continuar                  <<Ctrl-c>> Salir",
            "                         "      AT   3,2    ATTRIBUTE(REVERSE)
   MENU     " TRASPASOS  CEDIDOS "
            COMMAND   "Consulta" " Consulta Folios de Traspasos Unific. ISSSTE-IMSS"
                      CALL     F_010_Consulta()
            COMMAND   "Salir " " Salir de Programa"
            EXIT MENU
   END MENU
   CLOSE    WINDOW    ventana_1
END FUNCTION

FUNCTION    F_010_Consulta()
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
            " WHERE   b.tipo    IN(2,3)  ",
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
   IF       (i_arr_1-1)            >=  1     THEN
            CALL     SET_COUNT(i_arr_1   -  1)
            DISPLAY  ARRAY   reg_1        TO  scr_1.*
	             ON KEY  ( RETURN )
                              LET      g_cur            =  ARR_CURR()
                     CALL     F_020_control_registros()
                     LET      g_cur                     =  ARR_CURR()
                     LET      g_src                     =  SCR_LINE()
                     ON KEY(INTERRUPT)
                              ERROR    " CONSULTA  TERMINADA..."
                              SLEEP    2
                              ERROR    ""
                              CLEAR    FORM
                              EXIT     DISPLAY 
            END      DISPLAY
   ELSE 
            ERROR    "      NO  EXISTE  FOLIO     "
            SLEEP    2
            ERROR    ""
   END IF 
END FUNCTION

FUNCTION    F_020_control_registros()
   DEFINE   l_qry                      CHAR(1000)
   FOR      x                       =  1   TO  20
            INITIALIZE    reg_2[x].*       TO  NULL
   END FOR   
   OPEN     WINDOW      ventana_2          AT  2,2 
            WITH        FORM    "TUIL00041"             ATTRIBUTE( BORDER)
   DISPLAY  " <<TUIL00041>>  CONSULTA FOLIOS DE TRASPASOS UNIFICACIÓN ISSSTE-IMSS",
            "                 "            AT  1,2       ATTRIBUTE(REVERSE)
   DISPLAY  g_hoy   USING  "DD-MM-YYYY"    AT  1,63      ATTRIBUTE(REVERSE)
   DISPLAY  "   <<Ctrl-c>> Salir                             ",
            "                       "      AT  3,2       ATTRIBUTE(REVERSE)
   DISPLAY  BY  NAME   reg_1[g_cur].folio
   LET      l_qry    =
           ' SELECT   a.estado,count(a.estado) ',
           ' FROM     safre_af:tui_sdo_uni_issste a ',
           ' WHERE    a.folio       =  ',reg_1[g_cur].folio CLIPPED,
           ' GROUP  BY 1 ',
           ' ORDER  BY 1;'
    LET      l_qry                    =  l_qry    CLIPPED
    LET      i_arr_2                  =  1
    PREPARE  sql_edos              FROM  l_qry
    DECLARE  C_2    CURSOR          FOR  sql_edos
    FOREACH  C_2                   INTO  reg_2[i_arr_2].estado,
                                         reg_2[i_arr_2].total_cuentas
             SELECT   a.descripcion      INTO  reg_2[i_arr_2].desc_estado  
               FROM   safre_af:taa_cd_edo_cedente  a
              WHERE   a.estado              =  reg_2[i_arr_2].estado
                AND   a.tipo               IN  (1,2,3)
             LET      i_arr_2               =  i_arr_2   +  1
    END FOREACH
    CALL     SET_COUNT(i_arr_2 - 1)
    DISPLAY  ARRAY    reg_2 TO scr_2.*
    END      DISPLAY
    CLOSE    WINDOW   ventana_2
END FUNCTION
