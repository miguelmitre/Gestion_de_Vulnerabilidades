######################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                   #
#Propietario       => E.F.P.                                         #
#Programa TCAAB009 => Mantenimiento de Traspasos Indebidos.          #
#by                => JOSE FRANCISCO LUGO CORNEJO.                   #
#Fecha             => 19 de Mayo del 2007.                           #
#Sistema           => TCAA.                                          #
######################################################################
DATABASE safre_af

GLOBALS
   DEFINE  g_reg       RECORD
           nss                 LIKE  safre_af:taa_cd_indebidos.nss,
	   n_unico             CHAR(50),
	   nombre              CHAR(50),
	   rfc                 LIKE  safre_af:taa_cd_det_cedido.rfc,
	   tipo_traspaso       LIKE  safre_af:taa_cd_indebidos.tipo_rendi,
	   fecha_trasp         DATE,
	   tipo_rendi          LIKE  safre_af:taa_cd_indebidos.tipo_rendi,
	   estado              LIKE  safre_af:taa_cd_indebidos.estado
                         END RECORD


   DEFINE  g_param_dis   RECORD  LIKE   glo_parametro.*

   DEFINE  l_record_0      ARRAY[3000]  OF  RECORD 
           nss                 LIKE  safre_af:taa_cd_indebidos.nss,
	   n_unico             CHAR(18),
	   nombre              CHAR(50),
	   tipo_traspaso       LIKE  safre_af:taa_cd_det_cedido.tipo_traspaso,
	   fecha_trasp         LIKE  safre_af:taa_cd_det_cedido.fecha_trasp
   END RECORD

   DEFINE  l_record      ARRAY[3000]  OF  RECORD 
           nss                 LIKE  safre_af:taa_cd_indebidos.nss,
	   n_unico             CHAR(18),
	   nombre              CHAR(50),
	   tipo_rendi          LIKE safre_af:taa_cd_indebidos.tipo_rendi,
	   fecha_trasp         DATE,
	   fecha_liquidacion   DATE,
           estado              SMALLINT
                               END RECORD
   DEFINE  l_record_2    ARRAY[3000]  OF  RECORD 
           nss                 LIKE  safre_af:taa_cd_indebidos.nss,
	   n_unico             CHAR(18),
	   nombre              CHAR(50),
	   tipo_rendi          LIKE safre_af:taa_cd_indebidos.tipo_rendi,
	   fecha_liquidacion   DATE,
           estado              SMALLINT
                               END RECORD


   DEFINE  sw_1                SMALLINT,
           aux_pausa           CHAR(01),
           g_enter             CHAR(01),
           HOY                 DATE,
           fecha               DATE,
           aux_estad_desc      CHAR(40),
           usuario             CHAR(08),
           pos                 SMALLINT,
           cla_where           CHAR(1000),
           sel_where           CHAR(1000),
           g_lista             CHAR(1000),
           g_impre             CHAR(1000),
           genter              CHAR(01)            

END GLOBALS
##########################################################################
MAIN
   OPTIONS 
       PROMPT LINE LAST,
       INPUT WRAP,
       ACCEPT KEY control-o

   DEFER INTERRUPT

   CALL     STARTLOG(FGL_GETENV("USER")||".TCAAB009.log")
   CALL     inicio()
   CALL     proceso()

END MAIN
##########################################################################
FUNCTION    inicio()
   DEFINE   l_qry                     CHAR(1000)
   SELECT   USER,*
     INTO   usuario
     FROM   safre_af:tab_afore_local

   SELECT   ruta_spool
     INTO   g_param_dis.ruta_spool
     FROM   glo_parametro

   INITIALIZE g_reg.*, fecha TO NULL

   LET      l_qry                 =
             " SELECT  TRIM(a.nombre)||' '||TRIM(a.paterno)||' '|| ",
             "         TRIM(a.materno),a.fecha_trasp ",
             "   FROM  taa_cd_det_cedido  a ",
             "  WHERE  a.n_seguro  = ? ",
             "    AND  a.estado = '103'"
   LET      l_qry                 =  l_qry    CLIPPED
   PREPARE  qry_nombre        FROM  l_qry

END FUNCTION
##########################################################################
FUNCTION proceso()

    LET HOY = TODAY

    OPEN WINDOW ventana   AT 3,3 WITH FORM "TCAAB009" ATTRIBUTE( BORDER)
    DISPLAY " TCAAB009           TRASPASOS INDEBIDOS COMPLEMENTARIOS                           "  AT  3,1  ATTRIBUTE(REVERSE) 
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)
    DISPLAY "            <<      DEVOLUCION DE COMISIONES E INTERESES   >>                     "  AT  7,1  ATTRIBUTE(REVERSE) 
    DISPLAY "                   "  AT  17,1
    MENU "MENU DE CUENTAS       "
       COMMAND "Altas" "altas nss"     
          CALL altas()
       COMMAND "Consulta" "Consulta nss"
          CALL consulta()
       COMMAND "Modifica" "Modifica nss"
          CALL Modifica()
       COMMAND "Elimina"  "Elimina nss"
          CALL elimina()
       COMMAND "Salir" "Salir del Programa"
          EXIT MENU
    END MENU

    CLEAR SCREEN

END FUNCTION

FUNCTION    Inicializa()
   LET      sw_1                =  0
   INITIALIZE g_reg.*          TO  NULL
END FUNCTION

FUNCTION  altas()
   DEFINE   l_arr_count,
             l_scr_line     SMALLINT
   DEFINE  l_existe                  SMALLINT
   DEFINE  l_provisionados           SMALLINT

   LET pos = 2

   OPEN WINDOW ventana_0 AT 3,3 WITH FORM "TCAAB009_0" ATTRIBUTE( BORDER)
   DISPLAY " <TCAAB009_0>       TRASPASOS INDEBIDOS COMPLEMENTARIOS                         " AT 1,1 ATTRIBUTE(REVERSE,BOLD) 
   DISPLAY "               < ENTER > para Seleccionar Registro                                  " AT 2,1 ATTRIBUTE(BOLD)
   DISPLAY "                        <<  ALTA  DE  CUENTAS >>                                     " AT 3,1 ATTRIBUTE(REVERSE,BOLD)
   SELECT   COUNT(*)
     INTO   l_provisionados
    FROM    safre_af:taa_cd_indebidos
   WHERE    estado                =  102;
   IF       l_provisionados              THEN
            ERROR  "  PARA  ALTA  DE CUENTAS DEBE LIQUIDAR  LA PROVISION PENDIENTE ...  "
            PROMPT  "  TECLEE <ENTER>  PARA  SALIR  ...   "  FOR  g_enter
            CLEAR SCREEN
            CLOSE WINDOW ventana_0
            RETURN
   END  IF

   LET int_flag = FALSE

   CONSTRUCT   cla_where     ON  n_seguro      ,
                                 tipo_traspaso ,
                                 fecha_trasp
                           FROM  nss           ,
                                 tipo_traspaso ,
                                 fecha_trasp
           ON KEY (ESC)
                  LET int_flag = FALSE
                  EXIT CONSTRUCT
           ON KEY (CONTROL-C)
                  LET int_flag = TRUE
                  EXIT CONSTRUCT
   END CONSTRUCT

   IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "BUSQUEDA CANCELADA..."
         SLEEP 2
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_0
         RETURN
   END IF

   LET      sel_where    =  " SELECT   n_seguro,n_unico,tipo_traspaso ",
		            "   FROM   safre_af:taa_cd_det_cedido   ",
		            "  WHERE   ", cla_where    CLIPPED,
		            "    AND   tipo_traspaso   IN('21','73') ", # CPL-1285
		            "    AND   estado   =  '103' ",
		            "    AND   n_seguro   NOT  IN  ",
                            "         (SELECT   nss  ",
                            "            FROM   safre_af:taa_cd_indebidos) ",
		            "    AND   n_seguro   NOT  IN  ",
                            "         (SELECT   nss  ",
                            "            FROM   safre_af:taa_cd_his_rendi_dia)",
                            " ORDER  BY  1; "
   LET      sel_where             =  sel_where   CLIPPED

   PREPARE  qry_inde          FROM   sel_where
   DECLARE  cursor_2 CURSOR   FOR  qry_inde

   LET pos = 1
   INITIALIZE l_record_0[pos].* TO NULL
   FOREACH   cursor_2            INTO  l_record_0[pos].nss,
                                       l_record_0[pos].n_unico,
                                       l_record_0[pos].tipo_traspaso
             EXECUTE  qry_nombre    USING  l_record_0[pos].nss
                                     INTO  l_record_0[pos].nombre,
                                           l_record_0[pos].fecha_trasp
             LET      pos    =  pos    + 1
   END FOREACH

   IF (pos-1)  < 1 THEN
         ERROR " NO ENCONTRO REGISTROS........ "
         PROMPT  " TECLEE <ENTER> PARA SALIR......      "  FOR  g_enter
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_0
         RETURN
   END IF
   CALL SET_COUNT(pos-1)

   INPUT    ARRAY    l_record_0   WITHOUT   DEFAULTS    FROM     scr_1.*
            ON KEY (CONTROL-M)
               LET      pos     =  ARR_CURR()
               IF    (  l_record_0[pos].nss            IS  NULL    OR
                        l_record_0[pos].nss             =  " "     OR
                        l_record_0[pos].nss             =  "" )    THEN
                        ERROR   "NSS NO PUEDE SER NULO ............    "
               ELSE
               LET      l_arr_count           =  ARR_COUNT()
               LET      l_scr_line            =  SCR_LINE()
               LET      g_reg.nss             =  l_record_0[pos].nss
               LET      g_reg.n_unico         =  l_record_0[pos].n_unico
               LET      g_reg.nombre          =  l_record_0[pos].nombre
               LET      g_reg.fecha_trasp     =  l_record_0[pos].fecha_trasp
               LET      g_reg.tipo_traspaso   =  l_record_0[pos].tipo_traspaso
               LET      l_existe              =  0
               SELECT   1       INTO   l_existe
                 FROM   safre_af:taa_cd_indebidos
                WHERE   nss                   =  g_reg.nss;
               IF       l_existe       THEN
                        ERROR    "  NSS  YA  REGISTRADO  PARA  DEVOLUCION  "
                        CALL Inicializa()
               ELSE
                        SELECT   rfc
                          INTO   g_reg.rfc 
                          FROM   safre_af:taa_cd_det_cedido
                         WHERE   n_seguro    =  g_reg.nss
                           AND   estado            =  103;

                        CALL   inserta_registro()
                        DISPLAY  l_record_0[pos].* TO scr_1[l_scr_line].*
               END IF 
               END IF 

            ON KEY (INTERRUPT)
               CLEAR  FORM
               EXIT  INPUT
   END INPUT
   LET      INT_FLAG             =  FALSE
   CALL Inicializa()
   CLOSE WINDOW ventana_0
END FUNCTION

FUNCTION  inserta_registro()
   DEFINE  l_fecha_conversion        DATE
   DEFINE  l_comision_cal            SMALLINT
   OPEN WINDOW ventana_1 AT 6,3 WITH FORM "TCAAB009_1" ATTRIBUTE( BORDER)
   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY "<Esc> para Alta de Registro             (Ctrl-C) para  Salir " AT 1,1 ATTRIBUTE(BOLD)
   DISPLAY " ALTAS " AT 1,68 ATTRIBUTE(REVERSE,BOLD)

   INPUT BY NAME  g_reg.*  WITHOUT DEFAULTS

         BEFORE FIELD estado
               NEXT FIELD tipo_rendi

         AFTER FIELD   tipo_rendi
               LET   g_reg.estado = 101 
               DISPLAY BY NAME g_reg.estado 
             IF g_reg.tipo_rendi IS NULL THEN 
                ERROR "TIPO DE RENDIMIENTO NO PUEDE SER NULO "
                NEXT FIELD tipo_rendi
             END IF

             IF (g_reg.tipo_rendi <> 1   AND 
                 g_reg.tipo_rendi <> 2       )   THEN
                 ERROR "TIPO DE RENDIMIENTO SOLO PUEDE SER 1 / 2 "
                 NEXT FIELD tipo_rendi
             END IF

               LET      l_fecha_conversion     = 
                        MDY(MONTH(g_reg.fecha_trasp),
                        "01",YEAR(g_reg.fecha_trasp))
               LET      l_fecha_conversion     =
                        l_fecha_conversion     +  1   UNITS  MONTH
               LET      l_comision_cal         =  0

         ON KEY ( ESC )

             IF g_reg.tipo_rendi IS NULL THEN 
                ERROR "TIPO DE RENDIMIENTO NO PUEDE SER NULO "
                NEXT FIELD tipo_rendi
             END IF

             IF (g_reg.tipo_rendi <> 1   AND 
                 g_reg.tipo_rendi <> 2       )   THEN
                 ERROR "TIPO DE RENDIMIENTO SOLO PUEDE SER 1 / 2 "
                 NEXT FIELD tipo_rendi
             END IF

               PROMPT  "ESTA SEGURO DE CARGAR LA INFORMACION S/N ? "  FOR genter
               IF     ( genter  = "S" OR genter  = "s" )       THEN
                        INSERT  INTO   safre_af:taa_cd_indebidos 
                              VALUES   ("0", 
                                         g_reg.nss         ,
                                         g_reg.tipo_rendi  ,
                                         " "               ,
                                         HOY           ,
                                         --" "               ,
                                        "101" ,
                                         usuario
                                       );
                             ERROR  "  REGISTRO  DADO DE ALTA CORRECTAMENTE "
                             SLEEP  2
                             ERROR  ""
                          INITIALIZE      l_record_0[pos].*  TO  NULL
                             EXIT   INPUT     
               ELSE 
                             EXIT   INPUT
               END IF
               CALL      Inicializa()
               NEXT      FIELD   nss        
         ON KEY (INTERRUPT)
               EXIT   INPUT
   END INPUT
   LET      INT_FLAG             =  FALSE
   CLEAR  FORM
   CALL Inicializa()
   CLOSE WINDOW ventana_1
END FUNCTION

FUNCTION    consulta()
   LET      pos               =  2
   OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TCAAB009_2" ATTRIBUTE( BORDER)
   DISPLAY " <TCAAB009_2>       TRASPASOS INDEBIDOS COMPLEMENTARIOS                                     "  AT  1,1 ATTRIBUTE(REVERSE) 
   DISPLAY "    (Enter) Consultar       (Ctrl-p) Impresion           (Ctrl-C) Salir                     "  AT  2,1 ATTRIBUTE(BOLD)
   DISPLAY "                   <<  CONSULTA DE CUENTAS  >>                                              "  AT  3,1 ATTRIBUTE(REVERSE,BOLD)

   LET int_flag = FALSE

   CONSTRUCT cla_where  ON   nss               ,
                             tipo_rendi        ,
                             fecha_liquidacion ,
                             a.estado   
                        FROM nss               ,
                             tipo_rendi        ,
                             fecha_liq_inde ,
                             estado
         ON KEY (ESC)
            LET int_flag = FALSE
            EXIT CONSTRUCT

         ON KEY (CONTROL-C)
            LET int_flag = TRUE
            EXIT CONSTRUCT
   END CONSTRUCT

   IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "BUSQUEDA CANCELADA..."
         SLEEP 2
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_2
         RETURN
   END IF

   LET sel_where =    "SELECT a.nss,b.n_unico,a.tipo_rendi,",
                      "       a.fecha_liquidacion,a.estado ",
		      " FROM safre_af:taa_cd_indebidos a , " ,
                      "      safre_af:taa_cd_det_cedido b " ,
                      " WHERE ", cla_where CLIPPED,
                      " AND   a.nss = b.n_seguro " ,
                      " AND   b.estado = 103 "     ,
                      " ORDER BY fecha_liquidacion  DESC "

   PREPARE query FROM sel_where
   DECLARE cursor_1 CURSOR FOR query

   LET pos = 1
   FOREACH cursor_1 INTO l_record_2[pos].nss,l_record_2[pos].n_unico,
                         l_record_2[pos].tipo_rendi,
                         l_record_2[pos].fecha_liquidacion,l_record_2[pos].estado
           EXECUTE  qry_nombre     USING  l_record_2[pos].nss
              INTO  l_record_2[pos].nombre,g_reg.fecha_trasp
         LET pos = pos + 1
   END FOREACH

   INITIALIZE l_record_2[pos].* TO NULL

   IF (pos-1) <  1 THEN
         ERROR " NO EXISNTEN REGISTROS PARA DEVOLUCION DE COMISIONES......"
         SLEEP 2
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
   END IF
   CALL SET_COUNT(pos-1)

   INPUT    ARRAY    l_record_2    WITHOUT   DEFAULTS   FROM   scr_2.*
           ON KEY (CONTROL-M)
               LET    pos = ARR_CURR()
               LET    g_reg.nss             =  l_record_2[pos].nss
               LET    g_reg.n_unico         =  l_record_2[pos].n_unico
               LET    g_reg.nombre          =  l_record_2[pos].nombre
	       LET    g_reg.tipo_rendi      =  l_record_2[pos].tipo_rendi
               SELECT  fecha_trasp
                 INTO  g_reg.fecha_trasp
                 FROM  taa_cd_det_cedido
                WHERE  n_seguro             =  g_reg.nss
                  AND  estado               =  103;
	       LET     g_reg.estado         =  l_record_2[pos].estado

               SELECT   rfc,tipo_traspaso
                INTO    g_reg.rfc,g_reg.tipo_traspaso
                FROM    safre_af:taa_cd_det_cedido
               WHERE    n_seguro              =  g_reg.nss
                 AND    estado                =  103;

              CALL consulta_registro()
               EXIT INPUT

            ON KEY (control-p)
               ERROR "PROCESANDO IMPRESION..."
               CALL impresion(pos)

            ON KEY (INTERRUPT)
               EXIT  INPUT
         END INPUT

#  LET      INT_FLAG             =  FALSE
   CALL Inicializa()
   CLOSE WINDOW ventana_2
   CLEAR SCREEN
END FUNCTION
################################################################################
FUNCTION  Modifica()
   DEFINE    l_arr_count               ,
             l_provisionados           ,
             l_scr_line         SMALLINT

   LET pos = 2

   OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TCAAB009_2" ATTRIBUTE( BORDER)
      DISPLAY " <TCAAB009_2>       TRASPASOS INDEBIDOS COMPLEMENTARIOS                         " AT 1,1 ATTRIBUTE(REVERSE,BOLD) 
      DISPLAY "               Seleccione con < ENTER > el Registro a Modificar                 " AT 2,1 ATTRIBUTE(BOLD)
      DISPLAY "                     <<  MODIFICACION DE CUENTAS  >>                                     " AT 3,1 ATTRIBUTE(REVERSE,BOLD)
      LET int_flag = FALSE

      CONSTRUCT cla_where ON nss    ,
                             tipo_rendi ,
                             fecha_liquidacion,
                             a.estado
                        FROM nss  ,
                             tipo_rendi ,
                             fecha_liq_inde, 
                             estado 
         ON KEY (ESC)
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (CONTROL-C)
            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "BUSQUEDA CANCELADA..."
         SLEEP 2
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      LET sel_where    =  " SELECT  a.nss,b.n_unico,a.tipo_rendi,",
                          "         a.estado,a.fecha_liquidacion",
		          "   FROM  safre_af:taa_cd_indebidos a  ," ,
		          "         safre_af:taa_cd_det_cedido b  " ,
                          "  WHERE ", cla_where CLIPPED,
                          "  AND    a.nss = b.n_seguro " ,
                          "  AND    b.estado = 103 ",
                          "    AND    a.estado   IN  (101,921) ",
                          "  ORDER  BY  1 ;"

      PREPARE  query_alta      FROM  sel_where

      DECLARE  cur_alta      CURSOR  FOR  query_alta

      LET pos = 1
      FOREACH cur_alta     INTO   l_record_2[pos].nss,l_record_2[pos].n_unico,
                                  l_record_2[pos].tipo_rendi,
                                  l_record_2[pos].estado,
                                  l_record_2[pos].fecha_liquidacion
               EXECUTE  qry_nombre       USING  l_record_2[pos].nss
                  INTO  l_record_2[pos].nombre,g_reg.fecha_trasp
         LET pos = pos +1
      END FOREACH

      INITIALIZE l_record_2[pos].* TO NULL

      IF (pos-1)  < 1 THEN
                ERROR " NO HAY REGISTROS PARA MODIFICAR .........."
                SLEEP  3
                CLEAR  FORM
                CLOSE WINDOW ventana_2
                RETURN
      END IF
         CALL SET_COUNT(pos-1)
        INPUT    ARRAY    l_record_2   WITHOUT   DEFAULTS    FROM     scr_2.*
            ON KEY (CONTROL-M)
               LET      pos     =  ARR_CURR()
               IF    (  l_record_2[pos].nss            IS  NULL    OR
                        l_record_2[pos].nss             =  " "     OR
                        l_record_2[pos].nss             =  "" )    THEN
                        ERROR   "NSS NO PUEDE SER NULO ............    "
               ELSE
                        LET    l_arr_count        =  ARR_COUNT()
                        LET    l_scr_line         =  SCR_LINE()
                        LET    g_reg.nss          =  l_record_2[pos].nss       
                        LET    g_reg.tipo_rendi   =  l_record_2[pos].tipo_rendi
                        LET    g_reg.nombre       =  l_record_2[pos].nombre       
                        LET    g_reg.estado       =  l_record_2[pos].estado
                        SELECT rfc,tipo_traspaso,fecha_trasp
                         INTO  g_reg.rfc,g_reg.tipo_traspaso,g_reg.fecha_trasp
                         FROM  safre_af:taa_cd_det_cedido
                        WHERE  n_seguro              =  g_reg.nss
                          AND  estado                =  103; 
                        CALL   modifica_registro() 
                        DISPLAY  l_record_2[pos].tipo_rendi      TO
                                 scr_2[l_scr_line].tipo_rendi
                        DISPLAY  l_record_2[pos].estado           TO
                                 scr_2[l_scr_line].estado
               END IF
             

            ON KEY (INTERRUPT)
               CLEAR  FORM
               EXIT  INPUT
         END INPUT  
   LET      INT_FLAG             =  FALSE
   CALL Inicializa()
   CLOSE WINDOW ventana_2
   CLEAR  FORM
END FUNCTION

FUNCTION modifica_registro()
   OPEN WINDOW ventana_1 AT 6,3 WITH FORM "TCAAB009_1" ATTRIBUTE( BORDER)
      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " <Esc> para Modificar        (Ctrl-C) para Salir " AT 1,1 ATTRIBUTE(BOLD)
      DISPLAY " MODIFICA " AT 1,68 ATTRIBUTE(REVERSE,BOLD)

      INPUT BY NAME  g_reg.*  WITHOUT DEFAULTS

         BEFORE FIELD nss
            NEXT FIELD tipo_rendi

         AFTER FIELD tipo_rendi
            IF g_reg.tipo_rendi IS NULL    OR
               g_reg.tipo_rendi = " "      OR
               g_reg.tipo_rendi = ""       THEN
               ERROR "El tipo de rendimiento no puede ser nulo"
               NEXT FIELD  tipo_rendi
            END IF 

         AFTER FIELD estado
            IF g_reg.estado IS NULL    OR
               g_reg.estado = " "      OR
               g_reg.estado = ""       THEN
               ERROR "El Estado  no puede ser nulo"
               NEXT FIELD  estado
            END IF 

            CALL Pregunta()

            IF aux_pausa MATCHES "[Ss]" THEN
               UPDATE safre_af:taa_cd_indebidos
               SET       tipo_rendi           =  g_reg.tipo_rendi,
                         estado               =  g_reg.estado
               WHERE     nss                  =  g_reg.nss      
               ERROR "REGISTRO MODIFICADO" 
               SLEEP 2
               LET      l_record[pos].tipo_rendi     =  g_reg.tipo_rendi
               LET      l_record[pos].estado         =  g_reg.estado
               CALL     Inicializa()
            ELSE
               ERROR    "PROCESO DE MODIFICACION,CANCELADO"
               SLEEP 2
            END IF

            ERROR ""
            EXIT INPUT
         ON KEY ( INTERRUPT )
            CLEAR  FORM
            EXIT INPUT
      END INPUT
      LET      INT_FLAG             =  FALSE
     CALL Inicializa()
     CLOSE WINDOW ventana_1
END FUNCTION

FUNCTION elimina()
   DEFINE   l_arr_count               ,
            l_provisionados           ,
            l_scr_line         SMALLINT
   LET pos = 2
      CALL  SET_COUNT(pos-1)

      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "TCAAB009_2" ATTRIBUTE( BORDER)
      DISPLAY " <TCAAB009_2>       TRASPASOS INDEBIDOS COMPLEMENTARIOS                        "   AT 1,1 ATTRIBUTE(REVERSE,BOLD) 
      DISPLAY "               Seleccione con < ENTER > el Registro a Eliminar                  " AT 2,1  ATTRIBUTE(BOLD) 
      DISPLAY "                     <<  ELIMINA CUENTAS   >>                                     " AT 3,1 ATTRIBUTE(REVERSE,BOLD)

      LET int_flag = FALSE

      CONSTRUCT cla_where ON nss,
                             tipo_rendi ,
                             fecha_liquidacion ,
                             a.estado 
                        FROM nss ,
                             tipo_rendi ,
                             fecha_liq_inde,
                             estado
         ON KEY (ESC)
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (CONTROL-C)
            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "BUSQUEDA CANCELADA..."
         SLEEP 2
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      LET sel_where   =  " SELECT  a.nss,a.tipo_rendi,a.estado, ",
                         "         a.fecha_liquidacion ", 
		         "   FROM  safre_af:taa_cd_indebidos a, ",
                         "         safre_af:taa_cd_det_cedido b ",  
                         " WHERE  ", cla_where   CLIPPED,
                         " AND    a.nss        =  b.n_seguro ", 
                         " AND    a.estado     =  101",
                         " ORDER  BY  1; "
      PREPARE query2 FROM sel_where

      DECLARE cursor_3 CURSOR FOR query2

      LET pos = 1
      FOREACH cursor_3     INTO  l_record_2[pos].nss,l_record_2[pos].tipo_rendi,
                                 l_record_2[pos].estado,
                                 l_record_2[pos].fecha_liquidacion
              EXECUTE  qry_nombre       USING  l_record_2[pos].nss
                 INTO  l_record_2[pos].nombre,g_reg.fecha_trasp
               LET      pos      =  pos  +  1
      END FOREACH

      INITIALIZE l_record_2[pos].* TO NULL

      IF (pos-1)  < 1 THEN
         ERROR   "  NO  EXISTEN  REGISTROS  PARA  ELIMINAR ........ "
         CALL Inicializa()
         CLOSE WINDOW ventana_2
         CLEAR  FORM
         RETURN
      END IF

      CALL SET_COUNT(pos-1) 
     INPUT    ARRAY    l_record_2   WITHOUT   DEFAULTS    FROM   scr_2.*
            ON KEY (CONTROL-M)
               LET      pos     =  ARR_CURR()
               IF    (  l_record_2[pos].nss            IS  NULL    OR
                        l_record_2[pos].nss             =  " "     OR
                        l_record_2[pos].nss             =  "" )    THEN
                        ERROR   "NSS NO PUEDE SER NULO ............    "
               ELSE
               LET      l_arr_count           =  ARR_COUNT()
               LET      l_scr_line            =  SCR_LINE()
               LET      g_reg.nss             =  l_record_2[pos].nss
               LET      g_reg.nombre          =  l_record_2[pos].nombre
               LET      g_reg.tipo_rendi      =  l_record_2[pos].tipo_rendi
               LET      g_reg.estado          =  l_record_2[pos].estado
               SELECT   rfc,tipo_traspaso,fecha_trasp
                INTO    g_reg.rfc,g_reg.tipo_traspaso,g_reg.fecha_trasp
                FROM    safre_af:taa_cd_det_cedido
               WHERE    n_seguro              =  g_reg.nss
                 AND    estado                =  103;
               CALL     elimina_registro()
               DISPLAY  l_record_2[pos].* TO scr_2[l_scr_line].*
               END IF
            ON KEY (INTERRUPT)
               CLEAR  FORM
               EXIT  INPUT
         END INPUT
   LET      INT_FLAG             =  FALSE
   CALL Inicializa()
   CLOSE WINDOW ventana_2
   CLEAR  FORM
END FUNCTION

FUNCTION elimina_registro()
   OPEN WINDOW ventana_1 AT 6,3 WITH FORM "TCAAB009_1" ATTRIBUTE( BORDER)
      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY " <Esc>  para  Eliminar                  (Ctrl-C)para   Salir " AT 1,1 ATTRIBUTE(BOLD)
      DISPLAY " ELIMINA " AT 1,69 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY BY NAME  g_reg.*
      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "ELIMINACION CANCELADA..."
         SLEEP 2
         ERROR ""
         CALL Inicializa()
         RETURN
      END IF

      CALL Pregunta()

      IF aux_pausa MATCHES "[Ss]" THEN
         DELETE FROM safre_af:taa_cd_indebidos
         WHERE nss         = g_reg.nss  

         ERROR "REGISTRO ELIMINADO" 
         SLEEP 2
         INITIALIZE      l_record[pos].*  TO  NULL
      ELSE
         ERROR "ELIMINAR CANCELADO" 
         SLEEP 2
      END IF

      ERROR ""

   CALL    Inicializa()
   CLOSE   WINDOW      ventana_1
END FUNCTION

FUNCTION consulta_registro()
   OPEN WINDOW ventana_1 AT 6,3 WITH FORM "TCAAB009_1" ATTRIBUTE( BORDER)
      DISPLAY "" AT 1,1
      DISPLAY "" AT 2,1
      DISPLAY "                                        (Ctrl-C)para   Salir " AT 1,1 ATTRIBUTE(BOLD)
      DISPLAY " CONSULTA " AT 1,69 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY BY NAME  g_reg.*
      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "TERMINANDO CONSULTA..."
         SLEEP 2
         ERROR ""
         CALL Inicializa()
         RETURN
      END IF

     PROMPT "Oprima cualquier tecla para regresar..." for char g_enter

     ERROR "TERMINANDO CONSULTA.." 
         SLEEP 1
         INITIALIZE      l_record[pos].*  TO  NULL
      ERROR ""

   CALL Inicializa()
   CLOSE   WINDOW ventana_1
END FUNCTION

FUNCTION Pregunta()
   PROMPT "Esta seguro ¿ S/N ? " FOR aux_pausa
END FUNCTION

FUNCTION impresion(pos)
   DEFINE i,pos SMALLINT

   LET g_impre = g_param_dis.ruta_spool CLIPPED,"/",usuario CLIPPED,
                 ".INDEBIDOS-",hoy USING "dd-mm-yyyy" CLIPPED

   START REPORT rpt_tabllama TO g_impre

   FOR  i        =  1 TO (pos+1)
        LET      g_reg.nss               =  l_record[i].nss
        LET      g_reg.nombre            =  l_record[i].nombre
        LET      g_reg.tipo_rendi        =  l_record[i].tipo_rendi       
        LET      g_reg.fecha_trasp       =  l_record[i].fecha_liquidacion      
        IF       g_reg.nss    IS  NULL   THEN
                 EXIT FOR
       END IF
       OUTPUT  TO  REPORT    rpt_tabllama()
   END FOR

   FINISH REPORT rpt_tabllama

   ERROR "LISTADO GENERADO..."
   SLEEP 2
   ERROR ""
   DISPLAY  "Nombre:",g_impre  AT  18,1
 --  LET g_lista = "lp ",g_impre
--     LET g_lista = "vi ",g_impre
--   RUN g_lista
END FUNCTION

REPORT rpt_tabllama()

   OUTPUT
      TOP MARGIN 1
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH  60
   FORMAT
      PAGE HEADER
         PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d' 
         PRINT COLUMN 02,"TCAAB009 ",
               COLUMN 70,"LISTADO DE TRASPASOS INDEBIDOS COMPLEMENTARIOS  ",
               COLUMN 165,TODAY USING "dd-mm-yyyy"
         SKIP 3 LINE

         PRINT COLUMN 03,"NSS ",
               COLUMN 15,"NOMBRE",
               COLUMN 65,"T-R",
               COLUMN 70,"F_TRASPASO"
         
         SKIP 1 LINE

      ON EVERY ROW
            PRINT COLUMN 2 ,g_reg.nss      ,
                  COLUMN 15,g_reg.nombre  ,
                  COLUMN 65,g_reg.tipo_rendi   USING  "&",
                  COLUMN 70,g_reg.fecha_trasp  USING  "DD-MM-YYYY"
      PAGE TRAILER
         SKIP 2 LINE
         PRINT  COLUMN 60," Pagina : ",PAGENO USING "<<<<<"
      ON LAST ROW
         SKIP 4 LINE
         PRINT COLUMN 01," Total de registros : ",COUNT(*) USING "<<<<"
END REPORT
