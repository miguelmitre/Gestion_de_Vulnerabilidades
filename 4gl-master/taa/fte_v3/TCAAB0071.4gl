###########################################################################
#Proyecto           => AFORE ( MEXICO )                                   #
#Propietario        => E.F.P.                                             #
#Programa TCAAB007  => INHABILITA CUENTAS LIQUIDADAS POR TRASPASO         #
#Autor              => JOSE FRANCISCO LUGO CORNEJO                        #
#Fecha              => 5  MAYO   2009                                     #
#Sistema            => TCAA                                               #
###########################################################################
#Modificacion       => 12-Sept-2011 Se agrega marca para cuentas con + de #
#                  175 Salarios minimos. REQ_XXI-823--> Alejandro Chagoya #
###########################################################################

DATABASE   safre_af

GLOBALS
   DEFINE
           g_hoy                           DATE
   DEFINE
           g_provisionado                  ,  
           g_liquidada                     ,  
           g_cero                          ,
           g_siefore_max                   ,
           g_tipo_traspaso                 SMALLINT
   DEFINE
           g_folio                         ,
           g_procesados                    INTEGER
   DEFINE
           g_nom_tab_taa_cd_det            CHAR(040),  
           g_nss                           CHAR(011),
           g_enter                         CHAR(001),
           g_usuario                       CHAR(008),
           g_desc_tipo_traspaso            CHAR(023),
           g_ejecuta                       CHAR(300)

END GLOBALS


MAIN
   OPTIONS
            PROMPT   LINE   LAST,
            ACCEPT   KEY    CONTROL-I
            DEFER    INTERRUPT
   CALL     STARTLOG('TCAAB007.log')
   CALL     F_001_inicio()
   CALL     F_500_proceso()
END MAIN

FUNCTION    F_001_inicio()
   LET      g_hoy                    =  TODAY 
   LET      g_cero                   =  0
END FUNCTION

FUNCTION    F_500_proceso()
   DEFINE   l_registros                   INTEGER
   DEFINE   l_texto_query                 CHAR(500)
   OPEN WINDOW  TCAAB0071   AT 2,2  WITH  FORM  "TCAAB0071"  ATTRIBUTE(BORDER)
   DISPLAY  "TCAAB0071    INHABILITA CUENTAS LIQUIDADAS POR TRASPASO DE HOY ",
            "                  "  AT  3,1  ATTRIBUTE(REVERSE)
   DISPLAY  " <Esc> Continuar                       < CTRL-C > Salir  ",
            "                         "   AT  1,1   ATTRIBUTE(REVERSE)
   DISPLAY  g_hoy USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)
   ERROR   '   CONTANDO  REGISTROS...               '  ATTRIBUTE(REVERSE)
   SELECT   COUNT(*),user
     INTO   l_registros,g_usuario
     FROM   taa_cd_det_cedido
    WHERE   fecha_trasp                 =  g_hoy
      AND   tipo_traspaso    NOT  IN('73','74','75','83','84','85')
      AND   estado                      =  103
      AND   n_seguro   NOT  IN(SELECT  nss  FROM  cta_act_marca
                                WHERE  marca_cod      =  120);
   DISPLAY  l_registros                TO  FORMONLY.l_imss
   SELECT   COUNT(*)
     INTO   l_registros
     FROM   taa_cd_det_cedido
    WHERE   fecha_trasp                 =  g_hoy
      AND   tipo_traspaso         IN('73','74','75','83','84','85')
      AND   estado                      =  103
      AND   n_seguro   NOT  IN(SELECT  nss  FROM  cta_act_marca
                                WHERE  marca_cod      =  120);

   DISPLAY  l_registros                TO  FORMONLY.l_issste
   ERROR   '                                                 ' 
   WHILE    TRUE
            PROMPT   "ESTA SEGURO DE GENERAR EL PROCESO [S/N] ? "  FOR  g_enter
            IF       g_enter    MATCHES    "[sS]"      THEN
                     EXIT     WHILE
            ELSE
                     ERROR    "PROCESO CANCELADO" ATTRIBUTE(REVERSE) 
                     SLEEP    2
                     EXIT     PROGRAM
            END IF
   END WHILE
   DISPLAY  "  PROCESANDO INFORMACION ......                                "
               AT   18,1   ATTRIBUTE(REVERSE)

   CALL     F_520_inhabilita() 
   CALL f_verifica()    --->ACS Sep-2011
   SELECT   COUNT(*),user
     INTO   l_registros,g_usuario
     FROM   taa_cd_det_cedido
    WHERE   fecha_trasp                 =  g_hoy
      AND   tipo_traspaso    NOT  IN('73','74','75','83','84','85')
      AND   estado                      =  103
      AND   n_seguro        IN(SELECT  nss  FROM  cta_act_marca
                                WHERE  marca_cod      =  120);
   DISPLAY  l_registros                TO  FORMONLY.l_imss_inh
   SELECT   COUNT(*)
     INTO   l_registros
     FROM   taa_cd_det_cedido
    WHERE   fecha_trasp                 =  g_hoy
      AND   tipo_traspaso         IN('73','74','75','83','84','85')
      AND   estado                      =  103
      AND   n_seguro        IN(SELECT  nss  FROM  cta_act_marca
                                WHERE  marca_cod      =  120);
   DISPLAY  l_registros                TO  FORMONLY.l_issste_inh

   PROMPT  "  PROCESO FINALIZADO, TECLEE  <Enter>  Para Salir         "
            ATTRIBUTE(REVERSE)     FOR    g_enter
   CLOSE  WINDOW   TCAAB0071
END FUNCTION

FUNCTION    F_520_inhabilita()
   DEFINE   l_scta                        ,
            l_scta_fin                    ,
            verifica_liq                  SMALLINT
   LET      g_procesados            =  0
   DECLARE  cur_ced       CURSOR  FOR
   SELECT   folio,n_seguro
     FROM   taa_cd_det_cedido
    WHERE   fecha_trasp             =  g_hoy
      AND   estado                  =  103 
    ORDER   BY  1,2
   FOREACH  cur_ced         INTO  g_folio,g_nss
            CALL     F_225_inhabilita()

            LET      g_procesados         =  g_procesados     +  1
            DISPLAY  g_procesados        TO  FORMONLY.l_procesados
   END FOREACH
END FUNCTION

FUNCTION    F_225_inhabilita()        
   DEFINE   l_marca_entra                      SMALLINT
   SELECT   b.marca_cod
     INTO   l_marca_entra
     FROM   safre_af:taa_cd_det_cedido  a,safre_af:taa_cd_tipo_traspaso  b
    WHERE   a.folio                 =  g_folio
      AND   a.n_seguro              =  g_nss
      AND   a.estado                =  103
      AND   a.tipo_traspaso         =  b.tipo_traspaso;
   LET      g_ejecuta               = 
           "EXECUTE  PROCEDURE  desmarca_cuenta(?,?,?,?,?,?)"
   LET      g_ejecuta               =  g_ejecuta   CLIPPED
   PREPARE  clausula_spl1        FROM  g_ejecuta
   EXECUTE  clausula_spl1       USING  g_nss,
                                       l_marca_entra,
                                       g_folio,
                                       g_cero,
                                       l_marca_entra,
                                       g_usuario
END FUNCTION

################################################################################
##Anexo C, desmarca cuentas con + de 175 salarios minimos --> ACS-Sep2011
FUNCTION f_verifica()
DEFINE r_marca RECORD LIKE safre_af:cta_act_marca.*,
	     l_estado  SMALLINT
   
   LET l_estado = 40
   LET g_ejecuta = " "
   LET g_ejecuta =
            "EXECUTE  PROCEDURE  desmarca_cuenta(?,?,?,?,?,?)"
   LET g_ejecuta =  g_ejecuta CLIPPED
   PREPARE  sp_desmarca FROM  g_ejecuta
   
DECLARE cur_m CURSOR FOR
  SELECT a.*
  FROM cta_act_marca a , taa_cd_det_cedido b,
       taa_cd_tipo_traspaso c
  WHERE b.fecha_trasp =  g_hoy
  AND b.estado = 105
  AND a.nss = b.n_seguro
  AND a.marca_cod = c.marca_cod
  AND a.correlativo = b.folio

  INITIALIZE r_marca.* TO NULL
  
 FOREACH cur_m INTO r_marca.*
    
   EXECUTE  sp_desmarca          USING  r_marca.nss,            --pnss          
                                        r_marca.marca_cod,      --pmarca_entra  
                                        r_marca.correlativo,    --pcorrelativo  
                                        l_estado,               --pestado_marca 
                                        r_marca.marca_cod,      --pmarca_causa  
                                        g_usuario               --pusuario      
END FOREACH

END FUNCTION 
