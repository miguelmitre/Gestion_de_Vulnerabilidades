##############################################################################
#Proyecto          => SAFRE ( MEXICO )                                       #
#Propietario       => E.F.P.        			                     #
#Programa MENU     => MENU GENERAL DEL SISTEMA  SAFRE                        #
#Sistema           => MENU					             #
#AUTOR             => JOSE FRANCISCO LUGO CORNEJO. (24 Dic 04)               #
##############################################################################
DATABASE    safre_af
GLOBALS
   DEFINE 
            g_lastkey                       ,
            g_arr_menu                      ,
            g_arr_curr                      ,
            g_scr_line                      ,
            g_arr_count                     ,
            g_ind                           ,
            g_nivel_menu                    ,
            g_pos_nivel                     ,
            g_menu_anterior                 ,
            g_sin_opciones                  ,
            g_cambia_menu                   ,
            g_tot_altas                     ,
            g_perfil_cod                    ,
            g_administrador                 ,
            g_perfil_usr                    SMALLINT
   
   DEFINE 
            g_usuario                       CHAR(08),
            g_usuario_desc                  ,
            g_perfil_desc                   CHAR(50),
            g_ruta_exp                      CHAR(80),
            g_ejecuta_opcion                ,
            g_menu_cod                      ,
            g_menu_cod_ant                  ,
            g_menu_cod_desde                ,
            g_menu_cod_hasta                CHAR(12),
            g_enter                         CHAR(01),
            g_razon_social                  CHAR(30)
            
   DEFINE 
            g_hoy                           DATE
   DEFINE 
            g_men_safre                     RECORD  LIKE  safre_af:men_safre.*
   
   DEFINE   reg_scr_menu        ARRAY[30]   OF   RECORD  
            num_menu                        SMALLINT,
            activo                          CHAR(01),
            menu_cod                        LIKE  safre_af:men_safre.menu_cod,
            titulo                          LIKE  safre_af:men_safre.titulo,
            opcion                          LIKE  safre_af:men_safre.opcion 
                                      END   RECORD 

   DEFINE   reg_scr_ruta        ARRAY[10]   OF    RECORD  
            menu_cod                        LIKE  safre_af:men_safre.menu_cod,
            titulo                          CHAR(80)
                                            END   RECORD 

   DEFINE   reg_scr_mant_menu   ARRAY[30]   OF    RECORD  
            menu_cod                        LIKE  safre_af:men_safre.menu_cod,
            opcion                          LIKE  safre_af:men_safre.opcion,
            modulo_cod                      LIKE  safre_af:men_safre.modulo_cod,
            titulo                          CHAR(80)
                                            END   RECORD 
END GLOBALS

MAIN
   OPTIONS
            HELP       FILE     "MENU0001.o",
            PROMPT     LINE     LAST,
            INPUT      WRAP,
            ACCEPT     KEY      CONTROL-I
            DEFER      INTERRUPT
   CALL     f_001_inicio()
   CALL     f_200_proceso()
   CALL     f_900_fin()
END MAIN

FUNCTION    f_001_inicio()
   DEFINE   l_num_respaldo                  SMALLINT
   DEFINE   l_comando                       ,
            l_sql                           CHAR(300)
   SELECT   razon_social
     INTO   g_razon_social
     FROM   safre_af:tab_afore_local;
   SELECT   MAX(perfil_cod)
     INTO   g_administrador
     FROM   safre_af:seg_usuario
    WHERE   usuario_cod                    =  "safre";
   LET      l_comando                      =  "mkmessage ",
            "MENU0001.showhelp"," ",
            "MENU0001.o"
   RUN      l_comando
   CALL     f_010_inicializa_reg_scr_ruta()
   LET      g_hoy                          =  TODAY
   LET      g_menu_anterior                =  0
   CALL     f_020_trae_usuario()
   IF       g_perfil_usr             =  g_administrador   THEN
            CREATE   TEMP   TABLE   men_safre_temp
                                   (menu_cod             CHAR(12),
                                    nivel_menu           SMALLINT,
                                    titulo               CHAR(60),
                                    opcion               CHAR(10),
                                    modulo_cod           CHAR(04),
                                    estado               SMALLINT);
            CREATE   TEMP   TABLE   men_privilegios_temp
                                   (perfil_cod           SMALLINT,
                                    menu_cod             CHAR(12),
                                    opcion               CHAR(10),
                                    estado               SMALLINT);
            CREATE   TEMP   TABLE   men_safre_altas_temp
                                   (menu_cod             CHAR(12),
                                    nivel_menu           SMALLINT,
                                    titulo               CHAR(60),
                                    opcion               CHAR(10),
                                    modulo_cod           CHAR(04));

   END IF
   CREATE   TEMP   TABLE   status_fglgo  (status_fglgo   SMALLINT);
END FUNCTION

FUNCTION    f_010_inicializa_reg_scr_ruta()
   FOR      g_ind           =  1      TO   10
            INITIALIZE   reg_scr_ruta[g_ind].*    TO  NULL    
   END FOR
END FUNCTION

FUNCTION    f_020_trae_usuario()
   SELECT   a.usuario_cod,a.usuario_desc,a.perfil_cod,b.perfil_desc
   INTO     g_usuario,g_usuario_desc,g_perfil_usr,g_perfil_desc
   FROM     safre_af:seg_usuario a, safre_af:seg_perfil b
   WHERE    a.usuario_cod              =  USER
     AND    a.perfil_cod               =  b.perfil_cod;
   IF       SQLCA.SQLCODE             <>  0  THEN
            DISPLAY  "     USUARIO NO REGISTRADO EN EL SISTEMA SAFRE                    "       AT  10,1     ATTRIBUTE  (REVERSE)
            PROMPT   "   TECLEE <ENTER> PARA FINALIZAR           "  FOR  g_enter
            EXIT     PROGRAM
   END IF
END FUNCTION

FUNCTION    f_200_proceso()
   OPEN     WINDOW     ventana_1      AT   2,2    WITH    21   ROWS , 78
            COLUMNS    ATTRIBUTE(BORDER)
   DISPLAY  "           << SISTEMA DE ADMINISTRACION DE FONDOS PARA EL RETIRO >>             "       AT    3,1   ATTRIBUTE   (REVERSE)
   DISPLAY  "SAFRE v2.0                                         ",
            "   <Ctrl-T>Ayuda    E.F.P.     "         AT    4,1
   DISPLAY  " ",g_razon_social        AT    4,21    ATTRIBUTE   (REVERSE)
   DISPLAY  " USUARIO: ",g_usuario , "  ",g_usuario_desc,"                                  "   AT   21,1      ATTRIBUTE(REVERSE)
   DISPLAY  g_hoy    USING    "DD-MM-YYYY"   AT   21,68  ATTRIBUTE(REVERSE)
   MENU     "SAFRE "
     	    COMMAND    "Afore" " Menu de Afore."   HELP 1
                     LET       g_menu_cod           =  "010000000000"
                     CALL      f_205_inicia_menu()
            COMMAND  KEY (E)   "sErvicios" " Servicios al Afiliado."
                     LET       g_menu_cod           =  "020000000000"
                     CALL      f_205_inicia_menu()
            COMMAND  "Interfase" " Interfaces del Sistema."
                     LET       g_menu_cod           =  "030000000000"
                     CALL      f_205_inicia_menu()
            COMMAND  KEY (D) "aDministracion" " Menu Administracion del Sistema"
                     LET       g_menu_cod           =  "040000000000"
                     CALL      f_205_inicia_menu()
            COMMAND  "Salir"   " Salir del Programa"
	             EXIT      MENU
    END MENU
END FUNCTION

FUNCTION    f_205_inicia_menu()
   LET      g_perfil_cod                      =  NULL
   LET      g_nivel_menu                      =  2
   LET      reg_scr_ruta[1].menu_cod          =  "000000000000"
   LET      reg_scr_ruta[1].titulo            =
            "<<  MENU  PRINCIPAL  >>       "
   OPEN     WINDOW   menu     AT   7,02
            WITH     FORM     "MENU00011"   ATTRIBUTE(BORDER)
   CALL     f_020_trae_usuario()
   WHILE    TRUE
            IF       g_menu_cod               =  "000000000000"   THEN
                     EXIT     WHILE
            END IF
            CALL     f_210_arma_menu_safre()
            CALL     f_220_presenta_opciones()
   END      WHILE
   CLEAR    FORM
   CLOSE    WINDOW   menu
END FUNCTION

FUNCTION    f_210_arma_menu_safre()
   DEFINE   l_comando                        CHAR(200),
            l_titulo                         CHAR(100)
   FOR      g_ind             =  1       TO  30
            INITIALIZE   reg_scr_menu[g_ind].*    TO  NULL    
   END FOR
   LET      g_sin_opciones                =  0
   LET      g_pos_nivel                   =  g_nivel_menu   *  2
   LET      g_menu_cod_desde              =  g_menu_cod
   LET      g_menu_cod_desde[g_pos_nivel  -  1, g_pos_nivel]   =  "01"
   LET      g_menu_cod_hasta              =  g_menu_cod
   LET      g_menu_cod_hasta[g_pos_nivel  -  1 , g_pos_nivel]  =  "99"
   LET      g_arr_menu                    =  1
   SELECT   t.titulo
     INTO   l_titulo
     FROM   safre_af:men_safre t
    WHERE   t.menu_cod                    =  g_menu_cod;
   DISPLAY  "MENU00011                                                                          "   AT   1,1   ATTRIBUTE(REVERSE)
   LET      l_titulo               =  "MENU: << "  CLIPPED  ,
            g_menu_cod[g_pos_nivel - 3 , g_pos_nivel -2]
            USING    "##"   CLIPPED , "  ", l_titulo  CLIPPED ," >>"   CLIPPED
   LET      reg_scr_ruta[g_nivel_menu].menu_cod       =  g_menu_cod
   LET      reg_scr_ruta[g_nivel_menu].titulo         =  l_titulo
   DISPLAY  l_titulo      AT       2,1  ATTRIBUTE(REVERSE)
   DECLARE  c_menu_acc        CURSOR    FOR
   SELECT   "  "," ",b.menu_cod,b.titulo,b.opcion
     FROM   safre_af:men_privilegios a, safre_af:men_safre b
    WHERE   a.perfil_cod           =  g_perfil_usr
      AND   a.menu_cod             =  b.menu_cod
      AND   a.opcion               =  b.opcion
      AND   b.nivel_menu           =  g_nivel_menu 
      AND   b.menu_cod       BETWEEN  g_menu_cod_desde    AND  g_menu_cod_hasta
   ORDER    BY  3;
   FOREACH  c_menu_acc          INTO  reg_scr_menu[g_arr_menu].*
            LET      reg_scr_menu[g_arr_menu].num_menu       =
                     reg_scr_menu[g_arr_menu].menu_cod
                                 [g_pos_nivel - 1 ,g_pos_nivel]
            IF       g_perfil_usr          =  g_administrador      THEN
                     IF       g_perfil_cod           THEN
                              SELECT   UNIQUE("*")
                                INTO   reg_scr_menu[g_arr_menu].activo
                                FROM   safre_af:men_privilegios  p
                               WHERE   p.perfil_cod           =  g_perfil_cod  
                                 AND   p.menu_cod             =
                                       reg_scr_menu[g_arr_menu].menu_cod;
                     END IF
            END IF
            LET      g_arr_menu         =  g_arr_menu        +  1
   END      FOREACH
   IF       g_arr_menu                  =  1    THEN
            LET       reg_scr_menu[g_arr_menu].menu_cod      =  NULL
            LET       reg_scr_menu[g_arr_menu].titulo        = 
                      "NO TIENE OPCIONES DADAS DE ALTA"
            LET       g_arr_menu                 =  2
            LET       g_sin_opciones             =  1
   END IF
   LET      g_arr_menu             =  g_arr_menu      -  1
   CALL     SET_COUNT(g_arr_menu)
END FUNCTION

FUNCTION    f_220_presenta_opciones()
   DEFINE   l_no_existe_menu                ,
            l_num_menu                      SMALLINT
   DEFINE   l_menu_cod_ant                  CHAR(12)
   CLEAR    FORM 
   IF       g_perfil_cod      IS  NOT  NULL      THEN
            DISPLAY  "PERFIL: ",g_perfil_cod," ",g_perfil_desc,"      "
                     AT       1,15     ATTRIBUTE   (REVERSE) 
   ELSE
            DISPLAY  "PERFIL: ",g_perfil_usr," ",g_perfil_desc,"      " 
                     AT       1,15     ATTRIBUTE   (REVERSE) 
   END IF
   INPUT    ARRAY    reg_scr_menu    WITHOUT   DEFAULTS   FROM   scr_menu.*
            BEFORE   ROW
                     LET      g_arr_curr            =  ARR_CURR()
                     LET      g_scr_line            =  SCR_LINE()
                     LET      g_arr_count           =  ARR_COUNT()
                     IF       g_arr_curr            >  g_arr_count     THEN
                              EXIT INPUT
                     END IF
                     DISPLAY  reg_scr_menu[g_arr_curr].num_menu        TO
                              scr_menu[g_scr_line].num_menu  ATTRIBUTE(REVERSE)
                     DISPLAY  reg_scr_menu[g_arr_curr].activo          TO
                              scr_menu[g_scr_line].activo
                     DISPLAY  reg_scr_menu[g_arr_curr].titulo          TO
                              scr_menu[g_scr_line].titulo    ATTRIBUTE(REVERSE)
                     DISPLAY  reg_scr_menu[g_arr_curr].opcion          TO
                              scr_menu[g_scr_line].opcion    ATTRIBUTE(REVERSE)
            AFTER    ROW
                     LET      g_arr_curr             =  ARR_CURR()
                     LET      g_scr_line             =  SCR_LINE()
                     DISPLAY  reg_scr_menu[g_arr_curr].*      TO
                              scr_menu[g_scr_line].*
            ON KEY   ( RETURN )
                     LET      g_arr_curr             =  ARR_CURR()
                     LET      g_scr_line             =  SCR_LINE()
                     LET      l_no_existe_menu       =  0
                     LET      l_num_menu             =  0
                     IF       FIELD_TOUCHED(num_menu)         THEN
                              CALL     get_fldbuf(num_menu)
                              RETURNING  l_num_menu
                              LET      l_no_existe_menu       =  1
                              FOR      g_ind                  =  1      TO  30
                                       IF    l_num_menu                  =
                                             reg_scr_menu[g_ind].num_menu   THEN
                                             LET      l_no_existe_menu   =  0
                                             LET      g_arr_curr         = g_ind
                                             EXIT     FOR
                                       END IF
                              END FOR
                              IF       l_no_existe_menu              THEN
                                       ERROR   " SOLO EXISTEN LAS OPCIONES",
                                               " QUE  PRESENTA EL MENU EN ",
                                               "LINEA..........."
                                       EXIT    INPUT
                              END IF
                     END IF
                     IF       g_sin_opciones          THEN
                              EXIT  INPUT
                     END IF
                     IF       reg_scr_menu[g_arr_curr].opcion[1,4]        <>
                              "MENU"        THEN
                              IF       g_perfil_cod           THEN
                                       ERROR   "   SOLO PUEDE HABILITAR OPCION",
                                               "ES A PERFILES................."
                                       EXIT    INPUT
                              END IF
                              LET      g_ejecuta_opcion         =
                                       reg_scr_menu[g_arr_curr].menu_cod
                              CALL     f_230_ejecuta_programa()
                              EXIT  INPUT
                     ELSE
                              IF       g_nivel_menu             <  6     THEN 
                                       LET     g_menu_cod             =
                                               reg_scr_menu[g_arr_curr].menu_cod
                                       LET     g_nivel_menu           =
                                               g_nivel_menu           +  1
                                       EXIT    INPUT 
                              END IF
                     END IF
            ON KEY   ( CONTROL-N )
                     IF       g_perfil_usr           =  g_administrador  AND
                              g_perfil_cod          IS  NULL      THEN
                              CALL     f_240_altas_cambios_menu()
                              EXIT INPUT
                     END IF
            ON KEY   ( CONTROL-B )
                     LET      g_arr_curr             =  ARR_CURR()
                     LET      g_scr_line             =  SCR_LINE()
                     IF       g_perfil_usr           =  g_administrador   THEN
                              CALL     f_270_baja_opciones_menu()
                     END IF
                     EXIT INPUT
            ON KEY   ( CONTROL-F )
                     CALL     f_280_presenta_ruta_menu()
                     EXIT     INPUT
            ON KEY   ( CONTROL-U )
                     LET      g_arr_curr             =  ARR_CURR()
                     LET      g_scr_line             =  SCR_LINE()
                     IF       g_perfil_usr           =  g_administrador   THEN
                              CALL     f_290_alta_opciones_perfil()
                              EXIT     INPUT
                     END IF
            ON KEY   ( INTERRUPT )
                     LET      g_arr_curr             =  ARR_CURR()
                     LET      g_scr_line             =  SCR_LINE()
                     LET      g_menu_cod_ant         =  g_menu_cod
                     IF       g_nivel_menu           >  2    THEN 
                              LET      g_nivel_menu            =
                                       g_nivel_menu            -  1
                              LET      g_menu_anterior         =  1
                              LET      g_menu_cod[g_pos_nivel  -  3,  12]    =
                                       "0000000000"
                              FOR      g_ind          =  g_nivel_menu   TO   10    
                                       INITIALIZE   reg_scr_ruta[g_ind].*   
                                                      TO   NULL
                              END FOR
                     ELSE
                              LET      g_menu_cod         =  "000000000000" 
                     END IF
                     EXIT     INPUT 
            ON KEY   ( F7 )
                     SELECT   s.menu_cod          INTO  g_ejecuta_opcion 
                       FROM   safre_af:men_safre s,  safre_af:men_privilegios p
                      WHERE   s.opcion               = "CTACL001"
                        AND   p.perfil_cod           =  g_perfil_usr
                        AND   p.menu_cod             =  s.menu_cod;
                     IF       STATUS                <>  NOTFOUND    THEN
                              CALL     f_230_ejecuta_programa()
                     ELSE
                              ERROR    "  NO TIENE ACCESO A LA CONSULTA DE",
                                       " SALDOS .............             " 
                     END IF
            ON KEY   ( F8 )
                     SELECT   s.menu_cod          INTO  g_ejecuta_opcion
                       FROM   safre_af:men_safre s
                      WHERE   s.opcion               =  "SEGSPOOL";
                     CALL     f_230_ejecuta_programa()
            ON KEY   ( F9 )
                     SELECT   s.menu_cod          INTO  g_ejecuta_opcion
                       FROM   safre_af:men_safre s
                      WHERE   s.opcion               =  "SEGVIEW";
                     CALL     f_230_ejecuta_programa()
            ON KEY   ( CONTROL-T )
                     CALL     SHOWHELP(1)
   END      INPUT
   CLEAR    FORM
END FUNCTION

FUNCTION    f_230_ejecuta_programa()
   DEFINE   l_modulo_cod                         ,
            l_hora                               CHAR(08),
            l_ruta_exp                           CHAR(80),
            l_ruta_listados                      CHAR(80),
            l_arch_st_fglgo                      CHAR(200),
            l_programa                           CHAR(10),
            l_comando                            CHAR(300)

   DEFINE   l_status_fglgo                       SMALLINT
   SELECT   m.ruta_exp,m.ruta_listados,p.opcion
     INTO   l_ruta_exp,l_ruta_listados,l_programa
     FROM   seg_modulo  m,   safre_af:men_safre  p
    WHERE   p.menu_cod                 =  g_ejecuta_opcion
      AND   p.modulo_cod               =  m.modulo_cod;
   LET      l_hora                     =  TIME
   LET      l_comando                  =
            "cd ",l_ruta_exp   CLIPPED,";fglgo ",l_programa    CLIPPED,
            ";echo -e $?   >   ",l_ruta_listados     CLIPPED,
            "/status_fglgo"    CLIPPED, l_hora       CLIPPED
   RUN      l_comando 
   LET      l_arch_st_fglgo             =  l_ruta_listados    CLIPPED,
            "/status_fglgo"    CLIPPED,   l_hora    CLIPPED
   LOAD     FROM    l_arch_st_fglgo
   INSERT   INTO    status_fglgo;
   LET      l_comando                  =  "rm  ",l_ruta_listados     CLIPPED,
                                          "/status_fglgo", l_hora    CLIPPED
   RUN      l_comando
   SELECT   status_fglgo    INTO     l_status_fglgo
     FROM   status_fglgo;
   DELETE   FROM     status_fglgo;
   IF       l_status_fglgo             =  1     THEN
            LET      l_comando                  =
                     l_ruta_exp   CLIPPED,"/",  l_programa      CLIPPED
            ERROR    " EL PROGRAMA ",    l_comando       CLIPPED,
                     "  << NO EXISTE O TIENE ERROR >>            "
            RETURN
   END      IF
END FUNCTION

FUNCTION    f_240_altas_cambios_menu()
   DEFINE   l_arr_count                          ,
            l_scr_line                           ,
            l_existe                             ,
            l_perfil_cod                         ,
            l_tot_opciones                       ,
            l_arr_curr                           SMALLINT
   DEFINE   l_menu_cod_ant                       ,
            l_menu_cod_new                       ,
            l_menu_cod                           ,
            l_menu_cod_a                         CHAR(12)
   DEFINE   l_sql                                CHAR(500)
   LET      l_tot_opciones                  =  0
   CALL     f_250_inicializa_mantenimiento()
   LET      l_tot_opciones                  =  g_ind      -  1
   CALL     SET_COUNT(l_tot_opciones)
   LET      g_cambia_menu                   =  0
   LET      INT_FLAG                        =  FALSE
   OPEN     WINDOW   menu03   AT    2,2  WITH FORM "MENU00013" ATTRIBUTE(BORDER)
   DISPLAY  "MENU00013          <<   ALTAS Y CAMBIOS AL MENU SAFRE  >>                                "    AT  1,1 ATTRIBUTE  (REVERSE)
   DISPLAY  "   OPCION      MODULO     TITULO  DE  LA  OPCION                                         "    AT  3,1 ATTRIBUTE  (REVERSE)
   INPUT    ARRAY    reg_scr_mant_menu 
            WITHOUT  DEFAULTS          FROM  scr_mant_menu.*
            BEFORE   ROW
                     LET      g_arr_curr          =  arr_curr()
                     LET      g_scr_line          =  scr_line()
            AFTER    FIELD    opcion
                     IF       reg_scr_mant_menu[g_arr_curr].
                              opcion[1,4]         =  "MENU"        THEN
                              IF       g_nivel_menu             >  5     THEN
                                       IF       reg_scr_mant_menu[g_arr_curr].
                                                opcion[1,4]     = "MENU"   THEN
                                                ERROR
                                                "   ESTA EN EL LIMITE PARA ",
                                                "MENUS SI DESEA MAS NIVELES ",                                                  "COMUNIQUESE  A  E.F.P.     "
                                                NEXT     FIELD      opcion
                                       END IF
                              END IF
                              LET      reg_scr_mant_menu[g_arr_curr].modulo_cod
                                       =  "men"
                              NEXT     FIELD    titulo
                     END IF
            BEFORE   FIELD    modulo_cod
                     IF       reg_scr_mant_menu[g_arr_curr].opcion  IS  NULL  OR
                              reg_scr_mant_menu[g_arr_curr].opcion  =  " "  THEN
                              ERROR  "   El  NOMBRE  DEL  PROGRAMA  INVALIDO ",
                                     "............                  "
                              NEXT   FIELD     opcion
                     END IF
                     IF       f_255_checa_duplicados(g_arr_curr,g_scr_line) THEN
                              ERROR    "   OPCION DUPLICADA EN ESTE MENU .....",
                                       ".............                         "
                              DISPLAY  reg_scr_mant_menu[g_arr_curr].*   TO 
                                       scr_mant_menu[g_scr_line].*
                              NEXT     FIELD    opcion
                     END IF
            AFTER    FIELD    modulo_cod
                     IF       reg_scr_mant_menu[g_arr_curr].modulo_cod
                                                        IS     NULL        OR
                              reg_scr_mant_menu[g_arr_curr].modulo_cod
                                                              =   " "      THEN
                              ERROR    " LA CLAVE DE MODULO NO PUEDE SER NULA",
                                       "........................             "
                              NEXT      FIELD    modulo_cod
                     END IF
                     SELECT   "X"
                       FROM   safre_af:seg_modulo
                      WHERE   modulo_cod              =
                              reg_scr_mant_menu[g_arr_curr].modulo_cod
                     IF       STATUS                  =  NOTFOUND      THEN
                              ERROR    "    EL MODULO NO EXISTE .............",
                                       "..........                           "
                              NEXT     FIELD     modulo_cod
                     END IF
            BEFORE   FIELD    titulo
                     IF       reg_scr_mant_menu[g_arr_curr].
                              titulo           IS   NULL         THEN
                              LET      reg_scr_mant_menu[g_arr_curr].
                                       menu_cod                  =  NULL
                     END IF
            AFTER    FIELD    titulo
                     IF       reg_scr_mant_menu[g_arr_curr].
                              titulo           IS   NULL        OR
                              reg_scr_mant_menu[g_arr_curr].
                              titulo            =  " "          THEN
                              ERROR    "     EL TITULO NO PUEDE SER NULO......",
                                       "...........                           "
                              NEXT     FIELD    titulo
                     END IF
            ON KEY   ( ESC )
                     EXIT     INPUT 
            ON KEY   ( CONTROL-T )
                     CALL     SHOWHELP(1)
   END INPUT
   IF       INT_FLAG              THEN
            CLEAR    FORM
            CLOSE    WINDOW       menu03
            LET      INT_FLAG             =  FALSE
            RETURN
   END IF
   LET      l_menu_cod_ant           =   " "
   FOR      g_ind                    =  1     TO  30
            IF       reg_scr_mant_menu[g_ind].menu_cod    IS  NULL    THEN
                     CONTINUE  FOR
            END IF
            IF       l_menu_cod_ant           <>
                     reg_scr_mant_menu[g_ind].menu_cod      THEN
                     LET      l_menu_cod_ant       =
                              reg_scr_mant_menu[g_ind].menu_cod
            ELSE 
                     INITIALIZE  reg_scr_mant_menu[g_ind].*   TO  NULL
            END IF
   END FOR
   FOR      g_ind          =  1        TO  30
            IF       reg_scr_mant_menu[g_ind].menu_cod   IS  NOT   NULL   THEN
                     LET      l_tot_opciones       =  l_tot_opciones  -  1
            END IF
   END FOR
   IF       l_tot_opciones             <>   0     THEN
            ERROR    "   EN EL MENU DE ALTAS NO PUEDE ELIMINAR OPCIONES ",
                     "..........                                        "
            CLEAR    FORM
            CLOSE    WINDOW    menu03
            RETURN
   END IF
   WHILE    TRUE
            PROMPT   "   ESTA SEGURO DE CONTINUAR LA ACTUALIZACION [S/N] ?    "
                     FOR       g_enter
            IF       g_enter   MATCHES   "[sSnN]"   THEN
                     IF        g_enter     MATCHES   "[sS]"    THEN
                               EXIT     WHILE
                     ELSE
                               CLEAR    FORM 
                               CLOSE    WINDOW      menu03
                               RETURN
                     END IF
            END IF
   END WHILE
   CALL     f_260_actualiza_menu()
   CALL     SET_COUNT(g_tot_altas   -  1)
   CLEAR    FORM 
   CLOSE    WINDOW    menu03
END FUNCTION

FUNCTION    f_250_inicializa_mantenimiento()
   DELETE   FROM      men_safre_altas_temp;
   DELETE   FROM      men_privilegios_temp;  
   INSERT   INTO      men_privilegios_temp
   SELECT   perfil_cod,menu_cod,opcion,0
     FROM   safre_af:men_privilegios
    WHERE   perfil_cod          <>   g_administrador;     
   FOR      g_ind                =  1     TO  30
            INITIALIZE   reg_scr_mant_menu[g_ind].*   TO  NULL
   END FOR
   LET      g_ind                =  1
   DECLARE  c_mant       CURSOR  FOR 
   SELECT   m.menu_cod,m.opcion,m.modulo_cod,titulo 
     FROM   safre_af:men_safre   m
    WHERE   m.nivel_menu         =  g_nivel_menu
      AND   m .menu_cod    BETWEEN  g_menu_cod_desde   AND  g_menu_cod_hasta
   ORDER    BY  1;
   FOREACH  c_mant            INTO  reg_scr_mant_menu[g_ind].*
            LET      g_ind           =  g_ind      +  1
   END      FOREACH
END FUNCTION

FUNCTION    f_255_checa_duplicados(arr_c,l_scr)
   DEFINE   l_result                          CHAR(05)
   DEFINE   i_val                             ,
            arr_c                             ,
            l_scr                             SMALLINT
   FOR      i_val                   =  1      TO   10
            IF       i_val                    <>   arr_c     THEN
                     IF        reg_scr_mant_menu[arr_c].opcion   =
                               reg_scr_mant_menu[i_val].opcion          AND
                               reg_scr_mant_menu[arr_c].opcion   =
                               reg_scr_mant_menu[i_val].opcion          THEN
                               LET      l_result        =  TRUE
                               INITIALIZE  reg_scr_mant_menu[arr_c].*   TO  NULL
                     END IF
            END IF
   END FOR
   RETURN   l_result
END FUNCTION

FUNCTION    f_260_actualiza_menu()
   DEFINE   l_menu_cod_ant                   ,
            l_menu_cod_new                   CHAR(12)
   DEFINE   l_sql                            CHAR(500)
   FOR      g_ind                =  1        TO  30
            IF       reg_scr_mant_menu[g_ind].titulo      IS  NULL  THEN
                     CONTINUE  FOR
            END IF
            LET      l_menu_cod_ant       =  reg_scr_mant_menu[g_ind].menu_cod
            IF       l_menu_cod_ant                  IS  NULL    THEN
                     LET      l_menu_cod_ant          =
                              g_menu_cod
                     LET      l_menu_cod_ant[g_pos_nivel - 1, g_pos_nivel]   =
                              g_ind      USING    "&&"        
                     LET      reg_scr_mant_menu[g_ind].menu_cod    =
                              l_menu_cod_ant
                     CONTINUE  FOR
            END IF
            LET      l_menu_cod_new         =  l_menu_cod_ant
            LET      l_menu_cod_new[g_pos_nivel   -  1,  g_pos_nivel]   =
                     g_ind    USING     "&&"
            LET      reg_scr_mant_menu[g_ind].menu_cod     =
                     l_menu_cod_new
            DELETE   FROM     men_safre_temp   
            LET      l_sql                  =  
                     ' INSERT  INTO     men_safre_temp                ',
                     ' SELECT  n.*,0    FROM  safre_af:men_safre  n   ',
                     ' WHERE   n.menu_cod[1,',g_pos_nivel,']    =     ',
                     '"',l_menu_cod_ant[1,g_pos_nivel],'"             ',
                     ' AND     n.nivel_menu       > ',g_nivel_menu
            PREPARE  sql_1     FROM   l_sql
            EXECUTE  sql_1
            LET      l_sql                  =
                     ' UPDATE  men_safre_temp                      ',
                     '    SET  menu_cod[1,',g_pos_nivel,']     =   ',
                     '"',l_menu_cod_new[1,  g_pos_nivel], '" ,     ',
                     '         estado             =  1             ',
                     '  WHERE  menu_cod[1,',g_pos_nivel,']     =   ',
                     '"',l_menu_cod_ant[1,  g_pos_nivel], '"'       ,
                     '    AND  estado             =  0              '
            PREPARE  sql_2     FROM     l_sql
            EXECUTE  sql_2
            LET      l_sql                  =
                     ' UPDATE   men_privilegios_temp               ',
                     '    SET   menu_cod[1,',g_pos_nivel,']    =   ',
                     '"',l_menu_cod_new[1,  g_pos_nivel], '" ,     ',
                     '          estado            =  1             ',
                     '  WHERE   menu_cod[1,',g_pos_nivel,']  =     ',
                     '"',l_menu_cod_ant[1,  g_pos_nivel ] ,  '"    ',
                     '    AND   estado            =  0             ' 
            PREPARE  sql_3     FROM   l_sql
            EXECUTE  sql_3
             INSERT  INTO   men_safre_altas_temp
             SELECT  menu_cod, nivel_menu, titulo, opcion, modulo_cod
               FROM  men_safre_temp;
   END FOR
   FOR      g_ind           =  1       TO  30
            IF        reg_scr_mant_menu[g_ind].titulo     IS  NULL  THEN
                      CONTINUE FOR
            END IF
            INSERT    INTO     men_safre_altas_temp
                      VALUES   (
                               reg_scr_mant_menu[g_ind].menu_cod,
                               g_nivel_menu,
                               reg_scr_mant_menu[g_ind].titulo,
                               reg_scr_mant_menu[g_ind].opcion,
                               reg_scr_mant_menu[g_ind].modulo_cod
                               );
   END      FOR
   DELETE   FROM     safre_af:men_safre
    WHERE   menu_cod     BETWEEN   g_menu_cod_desde   AND   g_menu_cod_hasta 
      AND   nivel_menu             >=  g_nivel_menu;
   INSERT   INTO     safre_af:men_safre
   SELECT   *        FROM     men_safre_altas_temp;
   DELETE   FROM     safre_af:men_privilegios;
   INSERT   INTO     safre_af:men_privilegios
   SELECT   perfil_cod,menu_cod,opcion
     FROM   men_privilegios_temp;
   LET      l_sql                   =
            ' INSERT     INTO      safre_af:men_privilegios  ',
            ' SELECT ','"',g_administrador    USING   "###",'" , ',
            ' menu_cod,opcion ' ,
            ' FROM      safre_af:men_safre; '
            PREPARE    sql_priv       FROM   l_sql
            EXECUTE    sql_priv
END FUNCTION

FUNCTION    f_270_baja_opciones_menu()
   DEFINE   l_tiene_opciones                  ,
            l_opcion_ok                       SMALLINT
   DEFINE   l_menu_des                        , 
            l_menu_has                        CHAR(12)
   LET      l_tiene_opciones               =  0
   IF       reg_scr_menu[g_arr_curr].opcion[1,4]   =  "MENU"   THEN
            LET      l_menu_des         =  reg_scr_menu[g_arr_curr].menu_cod
            LET      l_menu_has         =  reg_scr_menu[g_arr_curr].menu_cod
            LET      l_menu_des[g_pos_nivel   +  1, g_pos_nivel  +  2]  =  "01"
            LET      l_menu_has[g_pos_nivel   +  1, g_pos_nivel  +  2]  =  "99"
            IF       g_perfil_cod      IS  NULL    THEN
                     SELECT   COUNT(*)     INTO  l_tiene_opciones
                       FROM   safre_af:men_safre
                      WHERE   menu_cod  BETWEEN  l_menu_des  AND  l_menu_has
                        AND   nivel_menu          =  (g_nivel_menu  +  1);
            ELSE
                     SELECT   COUNT(*)         INTO  l_tiene_opciones
                       FROM   safre_af:men_privilegios
                      WHERE   perfil_cod          =  g_perfil_cod
                        AND   menu_cod            =
                              reg_scr_menu[g_arr_curr].menu_cod;
                     IF       NOT  l_tiene_opciones      THEN
                              ERROR     "   EL PERFIL NO TIENE HABILITADA ESTA",
                                        " OPCION ..........                   "
                              RETURN
                     END IF
                     SELECT   COUNT(*)       INTO  l_tiene_opciones
                       FROM   safre_af:men_privilegios
                      WHERE   perfil_cod          =  g_perfil_cod
                        AND   menu_cod    BETWEEN  l_menu_des  AND  l_menu_has;
            END IF
            IF       l_tiene_opciones        THEN
                     ERROR    " ANTES TIENE QUE DAR DE BAJA LAS OPCIONES QUE ",
                              " CONTIENE ESTE MENU ......                    "  
                     RETURN
            END IF
   END IF
   IF       g_perfil_cod      IS  NULL   THEN
            WHILE    TRUE
                     PROMPT  "   ESTA SEGURO DE DAR DE BAJA LA OPCION DEL",
                             " MENU SAFRE  [S/N] ?   "   FOR   g_enter
                     IF       g_enter  MATCHES "[sSnN]" THEN
                              IF       g_enter  MATCHES "[sS]" THEN
                                       EXIT WHILE
                              ELSE
                                       ERROR  "   BAJA DE OPCION CANCELADA ...",
                                              ".......                        "
                                       RETURN
                              END IF
                     END IF
            END      WHILE
            DELETE   FROM    safre_af:men_safre
             WHERE   menu_cod           =  reg_scr_menu[g_arr_curr].menu_cod
               AND   opcion             =  reg_scr_menu[g_arr_curr].opcion;

            DELETE   FROM    safre_af:men_privilegios
             WHERE   menu_cod           =  reg_scr_menu[g_arr_curr].menu_cod;
   ELSE
            DELETE   FROM    safre_af:men_privilegios
             WHERE   perfil_cod         =  g_perfil_cod
               AND   menu_cod           =  reg_scr_menu[g_arr_curr].menu_cod;
            ERROR    " OPCION INHABILITADA PARA EL PERFIL :", g_perfil_cod,"              "  
   END IF
   CALL     f_250_inicializa_mantenimiento()
   CALL     f_260_actualiza_menu()
END FUNCTION

FUNCTION    f_280_presenta_ruta_menu()
   DEFINE   ind                               ,
            l_arr_count                       ,
            l_scr_line                        ,
            l_arr_curr                        SMALLINT
   LET      g_cambia_menu                  =  0
   CALL     SET_COUNT( g_nivel_menu )
   OPEN     WINDOW   menu02       AT   09,02   WITH  FORM 
            "MENU00012"    ATTRIBUTE(BORDER)
   DISPLAY  "MENU00012          << RUTA DE NAVEGACION DEL MENU EN LINEA  >>                                "    AT  1,1    ATTRIBUTE  (REVERSE)
   DISPLAY  " <Enter>Para Seleccionar  Menu              <Control-C>",
            "Regresa al Menu Actual          "    AT  2,1
   INPUT    ARRAY    reg_scr_ruta   WITHOUT   DEFAULTS    FROM     scr_ruta.*
            BEFORE   ROW
                     LET      l_arr_curr           =  ARR_CURR()
                     LET      l_arr_count          =  ARR_COUNT()
                     LET      l_scr_line           =  SCR_LINE()
                     IF       l_arr_curr           >  l_arr_count    THEN
                              EXIT     INPUT
                     END IF
                     DISPLAY  reg_scr_ruta[l_arr_curr].*   TO
                              scr_ruta[l_scr_line].*   ATTRIBUTE(REVERSE) 
            AFTER    FIELD    menu_cod
                     IF       l_arr_curr          >=  (l_arr_count)  THEN
                              LET      g_lastkey   =  FGL_LASTKEY()
                              IF     ((g_lastkey   =  FGL_KEYVAL("down"))   OR
                                      (g_lastkey   =  FGL_KEYVAL("return")) OR
                                      (g_lastkey   =  FGL_KEYVAL("tab"))    OR
                                      (g_lastkey   =  FGL_KEYVAL("right"))) THEN
                                       ERROR    "  NO HAY MAS OPCIONES EN ESA",
                                                " DIRECCION ........           "
                                       NEXT     FIELD    menu_cod
                              END IF
                     END IF
            AFTER    ROW
                     LET      l_arr_curr           =  ARR_CURR()
                     LET      l_scr_line           =  SCR_LINE()
                     DISPLAY  reg_scr_ruta[l_arr_curr].*            TO
                              scr_ruta[l_scr_line].*
            ON KEY   ( RETURN )
                     LET     l_arr_curr            =  ARR_CURR()
                     LET     l_scr_line            =  SCR_LINE()
                     LET     g_menu_cod            =
                             reg_scr_ruta[l_arr_curr].menu_cod
                     LET     g_nivel_menu          =  l_arr_curr
                     LET     g_cambia_menu         =  1 
                     FOR     ind                   =  l_arr_curr    TO  10    
                             INITIALIZE    reg_scr_ruta[ind].*      TO  NULL
                     END FOR
                     EXIT    INPUT 
            ON KEY   ( ESC )
                     EXIT    INPUT 
            ON KEY   ( CONTROL-T )
                     CALL    SHOWHELP(1)
   END INPUT
   CLEAR   FORM 
   CLOSE   WINDOW    menu02
END FUNCTION

FUNCTION    f_290_alta_opciones_perfil()
   DEFINE   l_nivel_menu                       ,
            l_pos_nivel                        ,
            l_hay_menu                         SMALLINT
   DEFINE   l_menu_cod                         ,
            l_menu_desde                       ,
            l_menu_hasta                       CHAR(12)
   DEFINE   l_opcion                           CHAR(10)
   IF       g_perfil_cod        IS  NULL    THEN
            CALL     f_300_selecciona_perfil()
            IF       g_perfil_cod     IS  NOT  NULL  THEN
                     DISPLAY  "PERFIL: ",g_perfil_cod," ", g_perfil_desc,
                              "      "   AT    1,15    ATTRIBUTE(REVERSE) 
            END IF
            RETURN
   END IF
   LET      l_hay_menu            =  0
   SELECT   COUNT(*)    INTO   l_hay_menu
     FROM   safre_af:men_privilegios
    WHERE   perfil_cod            =  g_perfil_cod
      AND   menu_cod              =  reg_scr_menu[g_arr_curr].menu_cod;
   IF       l_hay_menu      THEN
            ERROR   "   ESTA OPCION YA FUE HABILITADA ANTERIORMENTE",
                    " ...........                                  "
            RETURN
   END IF
   IF       g_nivel_menu          >  2     THEN 
            IF      reg_scr_menu[g_arr_curr].opcion[1,4]    =  "MENU"    THEN
                    ERROR    " ESTA OPCION ES UN MENU Y SE ACTIVA HABILITANDO",
                             " AL MENOS UNO DE SUS PROGRAMAS ...             "
                    RETURN
            END IF
            LET     l_menu_cod             =  g_menu_cod
            LET     l_nivel_menu           =  g_nivel_menu
            FOR     g_ind      =  1       TO  5
                    IF       l_nivel_menu           <  1       THEN 
                             EXIT     FOR
                    END IF 
                    LET      l_hay_menu             =  0
                    SELECT   COUNT(*)    INTO   l_hay_menu
                      FROM   safre_af:men_privilegios
                     WHERE   perfil_cod             =  g_perfil_cod
                       AND   menu_cod               =  l_menu_cod;
                    IF       NOT      l_hay_menu       THEN
                             SELECT   opcion     INTO   l_opcion
                               FROM   safre_af:men_safre
                              WHERE   menu_cod      =  l_menu_cod 
                             INSERT   INTO   safre_af:men_privilegios
                                      VALUES(g_perfil_cod,
                                             l_menu_cod,
                                             l_opcion);
                    END IF
                    LET      l_nivel_menu           =  l_nivel_menu    -  1
                    IF       l_nivel_menu           =  1        THEN
                             EXIT     FOR
                    END IF
                    LET      l_pos_nivel            =  l_nivel_menu      *  2
                    LET      l_menu_cod[l_pos_nivel  -1 , l_pos_nivel]   =  "00"
           END FOR
   END IF
   IF      g_nivel_menu                  =  2    THEN
           LET      l_menu_desde         =  reg_scr_menu[g_arr_curr].menu_cod
           LET      l_menu_hasta         =  reg_scr_menu[g_arr_curr].menu_cod
           LET      l_menu_desde[5,12]   =  "00000000"
           LET      l_menu_hasta[5,12]   =  "99999999"
           DECLARE  c_per     CURSOR   FOR  
           SELECT   m.menu_cod, m.opcion
             FROM   safre_af:men_safre   m
            WHERE   m.menu_cod     BETWEEN  l_menu_desde     AND  l_menu_hasta;
           FOREACH  c_per             INTO  l_menu_cod,l_opcion
                    INSERT   INTO     safre_af:men_privilegios
                             VALUES   (g_perfil_cod, l_menu_cod, l_opcion)
           END FOREACH
           LET      l_menu_desde[3,12]   =  "0000000000"
           SELECT   f.opcion
             INTO   l_opcion
             FROM   men_safre f 
            WHERE   menu_cod             =  l_menu_desde;
           LET      l_hay_menu           =  0
           SELECT   COUNT(*)    INTO   l_hay_menu
             FROM   safre_af:men_privilegios
            WHERE   perfil_cod           =  g_perfil_cod
              AND   menu_cod             =  l_menu_desde;
           IF       NOT      l_hay_menu     THEN
                    INSERT   INTO       safre_af:men_privilegios
                             VALUES   ( g_perfil_cod,l_menu_desde,l_opcion );
           END IF
   ELSE
           INSERT   INTO     safre_af:men_privilegios
                    VALUES  (g_perfil_cod,
                             reg_scr_menu[g_arr_curr].menu_cod,
                             reg_scr_menu[g_arr_curr].opcion);
   END IF
   ERROR   "  OPCION  HABILITADA PARA EL PERFIL :",g_perfil_cod,"  .......                 "
END FUNCTION

FUNCTION    f_300_selecciona_perfil()
   DEFINE   l_cur                           ,
            l_arr_count                     ,
            l_scr_line                      ,
            l_perfil_cod                    ,
            l_arr_curr                      SMALLINT          
   DEFINE   l_perfil_desc                   CHAR(50)
   DEFINE   l_select                        ,
            l_construct                     CHAR(1000)
   DEFINE   l_reg_perfil                    ARRAY[1000]  OF   RECORD
            cursor                          CHAR(01),
            perfil_cod                      INTEGER,
            perfil_desc                     CHAR(60)
                                                        END   RECORD
   OPEN     WINDOW   MENU00014        AT  4,2 
            WITH     FORM    "MENU00014"   ATTRIBUTE(BORDER)
   DISPLAY  "MENU00014             <<   PERFIL A ADMINISTRAR MENU   >>                                   "    AT  1,1   ATTRIBUTE    (REVERSE)
   DISPLAY  " <Esc>Salir           <Enter> Selecciona  Perfil        ",
            " <Control-c>Cancelar            "   AT  3,1  ATTRIBUTE  (REVERSE)
   FOR      g_ind                 =  1        TO  1000
            INITIALIZE     l_reg_perfil[g_ind].*      TO  NULL
   END FOR
   LET      g_ind                 =  1
   LET      l_perfil_cod          =  NULL
   LET      l_perfil_desc         =  NULL
   CONSTRUCT  l_construct        ON  a.perfil_cod   FROM    perfil_cons
            ON KEY  ( RETURN )
                      LET       INT_FLAG        =  FALSE
                      EXIT      CONSTRUCT
            ON KEY  ( INTERRUPT )
                      EXIT      CONSTRUCT
   END CONSTRUCT
   IF       INT_FLAG              =  FALSE     THEN
            LET       l_select             =
                      " SELECT   ' ',a.perfil_cod ,         ",
                      "          a.perfil_desc              ",
                      "   FROM   safre_af:seg_perfil   a    ",
                      "  WHERE  ",l_construct   CLIPPED,
                      "  ORDER   BY  2  "
            LET       l_select          =  l_select CLIPPED
            PREPARE   qry_consul         FROM    l_select
            DECLARE   cursor_c     CURSOR FOR    qry_consul
            FOREACH   cursor_c           INTO    l_reg_perfil[g_ind].*
                      LET        g_ind           =  g_ind   +  1
            END FOREACH
   END IF
   IF      (g_ind  -  1)          >=  1    THEN
            CALL     SET_COUNT(g_ind  - 1)
            INPUT    ARRAY    l_reg_perfil  WITHOUT DEFAULTS  FROM  scr_perfil.*
                     BEFORE   ROW
                              LET      l_arr_curr         =  ARR_CURR()
                              LET      l_arr_count        =  ARR_COUNT()
                              LET      l_scr_line         =  SCR_LINE()
                              IF       l_arr_curr         >  l_arr_count    THEN
                                       EXIT INPUT
                              END IF
                              DISPLAY  l_reg_perfil[l_arr_curr].*   TO
                                       scr_perfil[l_scr_line].*
                                       ATTRIBUTE(REVERSE)
                     AFTER    ROW
                              LET      l_arr_curr         =  ARR_CURR()
                              LET      l_scr_line         =  SCR_LINE()
                              DISPLAY  l_reg_perfil[l_arr_curr].*   TO
                                       scr_perfil[l_scr_line].*
                     AFTER    FIELD    cursor
                              IF       l_arr_curr        >=  (l_arr_count) THEN
                                       LET      g_lastkey     =   FGL_LASTKEY()
                                       IF     ((g_lastkey                 =
                                                FGL_KEYVAL("down"))      OR
                                               (g_lastkey                 =
                                                FGL_KEYVAL("return"))    OR
                                               (g_lastkey                 = 
                                                FGL_KEYVAL("tab"))       OR
                                               (g_lastkey                 =
                                                FGL_KEYVAL("right")))      THEN
                                                ERROR  "   NO HAY MAS OPCIONES",
                                                       "EN ESA DIRECCION .....",
                                                       "........              "
                                                NEXT    FIELD     cursor
                                       END IF
                              END IF
                     ON KEY   ( RETURN )
                              LET      l_perfil_cod          =
                                       l_reg_perfil[l_arr_curr].perfil_cod
                              LET      l_perfil_desc         =
                                       l_reg_perfil[l_arr_curr].perfil_desc
                              EXIT     INPUT
                     ON KEY  ( INTERRUPT )
                              EXIT     INPUT
            END INPUT
   END IF
   IF       l_perfil_cod        IS  NULL   THEN
            PROMPT   "   NO EXISTE PERFIL  TECLEE <Enter> para Salir",
                     " ..................                   "   FOR    g_enter
   END IF
   IF       INT_FLAG             =    TRUE         THEN
            LET      l_perfil_cod          =  NULL
   ELSE
            LET      g_perfil_cod          =  l_perfil_cod
            LET      g_perfil_desc         =  l_perfil_desc
   END IF
   LET      INT_FLAG             =  FALSE
   CLEAR    FORM 
   CLOSE    WINDOW  MENU00014
END FUNCTION

FUNCTION    f_900_fin()
   CLOSE    WINDOW      ventana_1
END FUNCTION
