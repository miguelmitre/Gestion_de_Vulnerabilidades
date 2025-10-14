##########################################################################
#Proyecto          => AFORE ( MEXICO )                                   #
#Propietario       => E.F.P.                                             #
#Programa TUIC0002  => REVERSA  FOLIOS TRASPASO UNIFIC. ISSSTE           #
#Fecha             => 27 DE OCTUBRE    DE 2010                           #
#Autor             => JOSE FRANCISCO LUGO CORNEJO                        #
#Sistema           => TUI (TRASPASOS UNIFICACIÓN ISSSTE-IMSS             #
##########################################################################
DATABASE    safre_af
GLOBALS
   DEFINE   reg_1           RECORD
            folio                           INTEGER        ,
            estado                          CHAR(17)       ,
            f_presentacion                  DATE           ,
            f_provision                     DATE           ,
            f_liquidacion                   DATE
                                      END   RECORD
   DEFINE
            g_today                           DATE,
            g_procedente                    ,
            g_provisionada                  ,
            g_vencida                       SMALLINT
   DEFINE
            g_enter                         CHAR
END GLOBALS

MAIN
   OPTIONS 
            PROMPT    LINE LAST,
            INPUT     WRAP,
            ACCEPT    KEY      CONTROL-I
#           DEFER     INTERRUPT
   CALL     F_001_modifica_folios()
END MAIN

FUNCTION    F_001_modifica_folios()
   DEFINE   l_select                  ,
            l_construct               CHAR(1000)
   CALL     STARTLOG  ("TUIC0002.log")
   LET      g_today               =  TODAY
   SELECT   a.estado    INTO  g_procedente
     FROM   safre_af:taa_cd_edo_cedente a
    WHERE   a.descripcion         = "PROCEDENTE" 
      AND   a.tipo                =  2;
   SELECT   a.estado    INTO  g_provisionada
     FROM   safre_af:taa_cd_edo_cedente a
    WHERE   a.descripcion         = "PROVISIONADA" 
      AND   a.tipo                =  3;
   OPEN     WINDOW    ventana_1      AT  2,2 
            WITH      FORM    "TUIC0002"     ATTRIBUTE(BORDER)
   DISPLAY  " <TUIC0002>  REVERSA OPERACIONES TRASPASOS UNIFICACIÓN ISSSTE-IMSS ", "                 "    AT  1,2   ATTRIBUTE(REVERSE)
   DISPLAY  " <CONTROL-B> CARGA OP. 09 <CONTROL-T> PROVISION <CONTROL-F> LIQUIDACIÓN   ", "                 "    AT  3,2   ATTRIBUTE(REVERSE)
   DISPLAY  g_today     USING   "DD-MM-YYYY"   AT   5,63   ATTRIBUTE(REVERSE)
   DISPLAY  "   <<Enter>> Muestra Folios             <<Ctrl-c>> Salir",
            "                         "      AT   5,2    ATTRIBUTE(REVERSE)
   LET      int_flag                  =  FALSE
   SELECT   MAX(F.folio)
     INTO   reg_1.folio
     FROM   safre_af:tui_ctr_folio   F
    WHERE   F.fecha_provision  BETWEEN  TODAY  AND  TODAY  +  15;
   IF       reg_1.folio        IS  NULL    OR
            reg_1.folio               =  0     THEN
            ERROR    "   NO   EXISTE  FOLIO  A  REVERSAR...         "
            PROMPT   " TECLEE <Enter> para Salir?..  "  FOR  g_enter
            EXIT  PROGRAM
   END IF

    SELECT  a.folio,
            b.descripcion,
            a.fecha_presentacion,
            a.fecha_provision,
            a.fecha_liquidacion 
     INTO   reg_1.*
     FROM   safre_af:tui_ctr_folio a, taa_cd_edo_cedente b
    WHERE   a.folio             =  reg_1.folio
      AND   a.estado            =  b.estado
   DISPLAY  reg_1.folio               TO  FORMONLY.folio
   DISPLAY  reg_1.estado              TO  FORMONLY.desc_estado
   DISPLAY  reg_1.f_presentacion      TO  FORMONLY.f_presentacion
   DISPLAY  reg_1.f_provision         TO  FORMONLY.f_provision
   DISPLAY  reg_1.f_liquidacion       TO  FORMONLY.f_liquidacion
   INPUT    BY NAME   reg_1.folio  WITHOUT  DEFAULTS 
            ON KEY ( CONTROL-B )
                     CALL     f_020_reversa_carga_op_09() 
            ON KEY ( CONTROL-T )
                     CALL     f_030_reversa_provision() 
            ON KEY ( CONTROL-F )
                     CALL     f_040_reversa_liquidacion() 
            ON KEY(INTERRUPT)
                     ERROR    " MODIFICACION  TERMINADA..."
                     SLEEP    2
                     ERROR    ""
                     CLEAR    FORM
                     EXIT     INPUT
   END      INPUT
   CLOSE    WINDOW    ventana_1
END FUNCTION


FUNCTION    f_020_reversa_carga_op_09()
   DEFINE   l_procedente                 SMALLINT
   SELECT   COUNT(*)
     INTO   l_procedente
     FROM   safre_af:tui_ctr_folio  a
    WHERE   a.folio                  =  reg_1.folio
      AND   a.estado                 =  101;
   IF       l_procedente          IS  NULL    OR
            l_procedente           =  0      THEN
            ERROR   "   EL FOLIO NO ESTÁ PROCEDENTE FAVOR DE VERIFICAR...   "
            PROMPT   " TECLEE <Enter> para Salir?..  "  FOR  g_enter
            RETURN
   END IF

   DELETE   FROM   safre_af:tui_ctr_folio 
    WHERE   folio                  =  reg_1.folio;

   DELETE   FROM   safre_af:tui_cza_uni_issste
    WHERE   folio                  =  reg_1.folio;

   DELETE   FROM   safre_af:tui_sdo_uni_issste
    WHERE   folio                  =  reg_1.folio;

   DELETE   FROM   safre_af:tui_int_uni_issste
    WHERE   folio                  =  reg_1.folio;

   DELETE   FROM   safre_af:tui_sum_uni_issste
    WHERE   folio                  =  reg_1.folio;

   ERROR   "   REVERSO  DE  CARGA DE OP.09  FINALIZADO...         "
   PROMPT   " TECLEE <Enter> para Salir?..  "  FOR  g_enter
   EXIT  PROGRAM
END FUNCTION

FUNCTION   f_030_reversa_provision()
   DEFINE  l_provision                  SMALLINT
   SELECT  COUNT(*)
     INTO  l_provision    
     FROM  safre_af:tui_ctr_folio  a
    WHERE  folio                  =  reg_1.folio
      AND  estado                 =  102;
   IF       l_provision           IS  NULL    OR
            l_provision            =  0      THEN
            ERROR   "   EL FOLIO NO ESTÁ PROVISIONADO FAVOR DE VERIFICAR...   "
            PROMPT   " TECLEE <Enter> para Salir?..  "  FOR  g_enter
            RETURN
   END IF

   UPDATE  safre_af:tui_ctr_folio
      SET  estado                 =  101
    WHERE  folio                  =  reg_1.folio;

  UPDATE   safre_af:tui_sdo_uni_issste
     SET   estado                 =  101
   WHERE   folio                  =  reg_1.folio 
     AND   estado                 =  102;

  UPDATE   safre_af:tui_int_uni_issste
     SET   estado                 =  101
    WHERE  folio                  =  reg_1.folio 
      AND  estado                 =  102;

   DELETE  FROM     safre_af:dis_provision
    WHERE  folio                  =  reg_1.folio ;

   ERROR    "   REVERSO  DE  PROVISIÓN  FINALIZADO...         "
   PROMPT   " TECLEE <Enter> para Salir?..  "  FOR  g_enter
   EXIT  PROGRAM
END FUNCTION

FUNCTION    f_040_reversa_liquidacion()
   DEFINE   l_liquidado                  SMALLINT
   SELECT   COUNT(*)
     INTO   l_liquidado
     FROM   safre_af:tui_ctr_folio  a
    WHERE   a.folio                  =  reg_1.folio
      AND   a.estado                 =  103;
   IF       l_liquidado           IS  NULL    OR
            l_liquidado            =  0      THEN
            ERROR   "   EL FOLIO NO ESTÁ LIQUIDADO FAVOR DE VERIFICAR...   "
            PROMPT   " TECLEE <Enter> para Salir?..  "  FOR  g_enter
            RETURN
   END IF

   UPDATE   safre_af:tui_ctr_folio
      SET  estado                 =  102
    WHERE  folio                  =  reg_1.folio
      AND  estado                 =  103;

   UPDATE  safre_af:tui_sdo_uni_issste
      SET  estado                  =  102
    WHERE  folio                   =  reg_1.folio 
      AND  estado                  =  103;

   UPDATE  safre_af:tui_int_uni_issste
      SET  estado                  =  102
    WHERE  folio                   =  reg_1.folio 
      AND  estado                  =  103;

   DELETE  FROM     safre_af:dis_cuenta
    WHERE  folio                  =  reg_1.folio ;

   ERROR    "   REVERSO  DE  LIQUIDACIÓN  FINALIZADO...         "
   PROMPT   " TECLEE <Enter> para Salir?..  "  FOR  g_enter
   EXIT  PROGRAM
END FUNCTION
