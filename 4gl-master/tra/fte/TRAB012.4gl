##############################################################################
#Owner             => E.F.P.
#Programa TRAB012  => CAPTURA DE DOCUMENTOS PRESENTADOS PARA TRASPASOS DE
#                     DE ICEFAS DE LOS TRABAJADORES  DEL  ISSSTE.         
#Fecha creacion    => 04 DE MAYO DE 1999
#By                => JOSE FRANCISCO LUGO CORNEJO
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Fecha de Mod      => Diciembre DEL 2013 .
#                  => Version Inv-Coppel
#Ultima Mod        => ENERO DEL 2006.
#Sistema           => TRA-ICE-ISSSTE
#############################################################################

DATABASE safre_af

GLOBALS
    DEFINE   uno smallint,
             cero smallint
    DEFINE   sal_consulta     SMALLINT
    DEFINE   band_inc         SMALLINT
    DEFINE   band_inc1         SMALLINT
    DEFINE   salida_modulo    SMALLINT
    DEFINE   ver_salida       SMALLINT
    DEFINE   t_solicitud        ,
             t_icefa            ,
             t_comp             ,
             t_ident    CHAR(100)
    DEFINE   lastkey,j smallint
    DEFINE   d_f_presentacion DATETIME YEAR TO SECOND
    DEFINE   band             SMALLINT
    DEFINE   g_n_seguro       CHAR(11),
             g_n_seguro_ent   CHAR(11),
             g_rfc_ent        CHAR(13),
             g_cve_ced_cuenta CHAR(03),
             g_nro_ctrl_icefa CHAR(30),
             g_nombre         CHAR(20),
             g_user           CHAR(08),
             enter            CHAR(1)

    DEFINE   ga_doctos          ARRAY[4] OF RECORD 
             tipo_docto         LIKE tra_his_doctos.tipo_docto      ,
             num_docto          LIKE tra_his_doctos.num_docto       ,
             descripcion        LIKE tra_cat_doctos.descripcion     ,
             cod_cal            LIKE tra_his_doctos.cod_cal         ,
             desc_cal           LIKE tra_st_doctos.desc_cal         ,
             f_presentacion     LIKE tra_his_doctos.f_presentacion  ,
             usuario            LIKE tra_his_doctos.usuario      
    END RECORD

    DEFINE   bk_doctos          ARRAY[4] OF RECORD 
             tipo_docto         LIKE tra_his_doctos.tipo_docto      ,
             num_docto          LIKE tra_his_doctos.num_docto       ,
             descripcion        LIKE tra_cat_doctos.descripcion     ,
             cod_cal            LIKE tra_his_doctos.cod_cal         ,
             desc_cal           LIKE tra_st_doctos.desc_cal         ,
             f_presentacion     LIKE tra_his_doctos.f_presentacion  ,
             usuario            LIKE tra_his_doctos.usuario      
    END RECORD

    DEFINE   ga_st_doctos     ARRAY[20] OF  RECORD  LIKE  tra_st_doctos.*

    DEFINE   bf_cat_doctos    ARRAY[50] OF   RECORD
             tipo_docto         LIKE  tra_cat_doctos.tipo_docto,
             num_docto          LIKE  tra_cat_doctos.num_docto,
             descripcion        LIKE  tra_cat_doctos.descripcion
                              END RECORD
    DEFINE   reg_his_doctos   RECORD  LIKE  tra_his_doctos.*
                              

    DEFINE   i_cat               ,
             i_st_max            , 
             i_cat_max           , 
             g_seleccion         , 
             i_st                SMALLINT 
    DEFINE   HOY                 DATE

END GLOBALS

FUNCTION TRAB012( vg_n_seguro        ,
                  vg_n_seguro_ent    ,
                  vg_rfc_ent         ,
                  vg_cve_ced_cuenta  ,
                  vg_nro_ctrl_icefa)


    DEFINE   vg_n_seguro       CHAR(11),
             vg_n_seguro_ent   CHAR(11),
             vg_rfc_ent        CHAR(13),
             vg_cve_ced_cuenta CHAR(03),
             vg_nro_ctrl_icefa CHAR(030)

    OPTIONS
    ACCEPT KEY CONTROL-I ,
    INPUT WRAP           ,
    PROMPT LINE LAST
    LET salida_modulo = 0
    LET ver_salida = 0
    LET band_inc   = 0
    LET band_inc1  = 0
    LET sal_consulta = 0
    LET uno = 1
    LET cero = 0

    LET g_n_seguro       = vg_n_seguro
    LET g_n_seguro_ent   = vg_n_seguro_ent
    LET g_rfc_ent        = vg_rfc_ent
    LET g_cve_ced_cuenta = vg_cve_ced_cuenta
    LET g_nro_ctrl_icefa = vg_nro_ctrl_icefa

    OPEN WINDOW TRAB0120 AT 10,2 WITH FORM "TRAB0120" ATTRIBUTE(BORDER)
    CALL inicio()        #i
    IF salida_modulo = 1 THEN
       IF sal_consulta = 1 THEN
          CLOSE WINDOW TRAB0120
          RETURN uno
       ELSE 
          CLOSE WINDOW TRAB0120
          RETURN cero
       END IF
    ELSE 
       IF sal_consulta = 1 THEN
          RETURN uno
       ELSE 
          RETURN cero
       END IF
    END IF
      

END FUNCTION

FUNCTION inicio()
#i---------------
    DEFINE  f                  SMALLINT
    DEFINE  l_nombre                  ,
            l_paterno                 ,
            l_materno          CHAR(60)


  IF (g_n_seguro IS NOT NULL AND
      g_n_seguro <> " " ) THEN

    SELECT "OK" 
    FROM   tra_his_doctos  a
    WHERE  a.n_seguro       = g_n_seguro
    AND    a.n_seguro_ent   = g_n_seguro_ent
    AND    a.rfc_ent        = g_rfc_ent
    AND    a.cve_ced_cuenta = g_cve_ced_cuenta
    AND    a.nro_ctrl_icefa = g_nro_ctrl_icefa
    GROUP BY 1
  
    IF STATUS = NOTFOUND THEN
      FOR f = 1 TO 4  
       INSERT INTO tra_his_doctos VALUES(g_n_seguro       ,
                                         g_n_seguro_ent   ,
                                         g_rfc_ent        ,
                                         g_cve_ced_cuenta ,
                                         g_nro_ctrl_icefa ,
                                         f                , #tipo docto
                                         0                , #num docto
                                         9                , #cod cal
                                         CURRENT          , #f_presentacion 
                                         'default')         #usuario
      END FOR
    END IF
  ELSE
    RETURN
  END IF 
                                         
    LET HOY              = TODAY
    LET i_cat            = 1
    LET i_st             = 1

    LET t_solicitud ="                    TIPOS  DE  SOLICITUD                 "
    LET t_icefa     ="                    TIPOS  DE  ICEFAS                    "
    LET t_comp      ="             COMPROBANTES  DE  DEPENDENCIA               "
    LET t_ident     ="         COMPROBANTES  DE  IDENTIFICACION  PERSONAL      "


    WHENEVER ERROR CONTINUE
      DROP TABLE tmp_verifica
    WHENEVER ERROR STOP

    CREATE TEMP TABLE tmp_verifica(indice smallint)

    SELECT user
    INTO   g_user
    FROM   tab_afore_local 

    CALL proceso_principal() 
    IF salida_modulo = 1 THEN
       RETURN
    END IF 
END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

   DEFINE i                          ,
          x                          ,
          l_ya_hay_reg               ,
          arr_c                      ,
          arr_coun                   , 
          l_cnt_doctos               ,
          l_scr                      ,
          val                 SMALLINT


    DISPLAY "                    CALIFICACION DE IMAGENES DIGITALIZADAS                     " AT 1,1 ATTRIBUTE(REVERSE)

    FOR i = 1 TO 4
       INITIALIZE ga_doctos[i].* TO NULL
       INITIALIZE bk_doctos[i].* TO NULL
    END FOR

    FOR i = 1 TO 4

     SELECT max(a.f_presentacion)
     INTO   d_f_presentacion
     FROM   tra_his_doctos a
     WHERE  a.n_seguro        = g_n_seguro
     AND    a.n_seguro_ent    = g_n_seguro_ent
     AND    a.rfc_ent         = g_rfc_ent
     AND    a.cve_ced_cuenta  = g_cve_ced_cuenta
     AND    a.nro_ctrl_icefa  = g_nro_ctrl_icefa
     AND    a.tipo_docto      = i

     SELECT a.tipo_docto      ,
            a.num_docto       ,   
            c.descripcion     ,
            a.cod_cal         ,
            b.desc_cal        ,
            a.f_presentacion  ,
            a.usuario
     INTO   ga_doctos[i].*
     FROM   tra_his_doctos a , 
            OUTER tra_st_doctos  b ,
            tra_cat_doctos c
     WHERE  a.n_seguro       = g_n_seguro
     AND    a.n_seguro_ent   = g_n_seguro_ent
     AND    a.rfc_ent        = g_rfc_ent
     AND    a.cve_ced_cuenta = g_cve_ced_cuenta
     AND    a.nro_ctrl_icefa = g_nro_ctrl_icefa
     AND    a.f_presentacion = d_f_presentacion
     AND    a.tipo_docto     = i 
     AND    a.cod_cal        = b.cod_cal
     AND    a.tipo_docto     = c.tipo_docto
     AND    a.num_docto      = c.num_docto
    
     LET    ga_doctos[i].tipo_docto = i 
     LET    bk_doctos[i].* = ga_doctos[i].*
   END FOR

   FOR j = 1 TO 4      
      IF ga_doctos[j].cod_cal = 0 THEN 
         INSERT INTO tmp_verifica VALUES(j)
      ELSE
         ERROR"REGISTRO CON DOCUMENTOS INCOMPLETOS..."
         LET band_inc1 = 1 
      END IF

   END FOR

   LET   band = 1 

   WHILE band = 1

   CALL SET_COUNT(i)

   INPUT ARRAY ga_doctos 
   WITHOUT DEFAULTS 
   FROM sarr_mod_dcto.*
 
      BEFORE ROW
            LET arr_c = ARR_CURR()
            LET l_scr = SCR_LINE()

      AFTER FIELD num_docto 
          LET lastkey = FGL_LASTKEY() 

          SELECT "OK" 
          FROM   tmp_verifica 
          WHERE  indice = arr_c

          IF STATUS <> NOTFOUND THEN
            IF (lastkey <> FGL_KEYVAL('down') AND
                lastkey <> FGL_KEYVAL('UP')) THEN
              ERROR"NO PUEDE SER MODIFICADO ..."
              LET ga_doctos[arr_c].num_docto = bk_doctos[arr_c].num_docto
              DISPLAY ga_doctos[arr_c].num_docto       TO
                      sarr_mod_dcto[arr_c].num_docto
              NEXT FIELD num_docto
            ELSE 
              ERROR"NO PUEDE SER MODIFICADO ..."
              LET ga_doctos[arr_c].num_docto = bk_doctos[arr_c].num_docto
              DISPLAY  ga_doctos[arr_c].num_docto       TO
                       sarr_mod_dcto[arr_c].num_docto
              ERROR""
            END IF 
   
          END IF
         
          IF (ga_doctos[arr_c].num_docto IS NOT NULL AND
              ga_doctos[arr_c].num_docto <> " "     )  THEN

                 CALL valida_cat_doctos(arr_c,ga_doctos[arr_c].num_docto)
                 RETURNING  l_cnt_doctos,ga_doctos[arr_c].descripcion

                   IF l_cnt_doctos = 0 THEN

                      ERROR"NO EXISTE EL TIPO DE COMPROBANTE..."

                      LET ga_doctos[arr_c].num_docto = 
                      bk_doctos[arr_c].num_docto

                      LET ga_doctos[arr_c].descripcion = 
                      bk_doctos[arr_c].descripcion

                      DISPLAY  ga_doctos[arr_c].num_docto       TO
                               sarr_mod_dcto[arr_c].num_docto

                      DISPLAY  ga_doctos[arr_c].descripcion      TO
                               sarr_mod_dcto[arr_c].descripcion
                      NEXT FIELD num_docto
                   ELSE 

                      DISPLAY  ga_doctos[arr_c].descripcion         TO
                               sarr_mod_dcto[arr_c].descripcion

                      IF (FIELD_TOUCHED(num_docto) AND 
                          bk_doctos[arr_c].cod_cal <> 0)  THEN

                        LET ga_doctos[arr_c].f_presentacion = CURRENT 
                        LET ga_doctos[arr_c].usuario        = g_user

                        DISPLAY  ga_doctos[arr_c].f_presentacion      TO
                                 sarr_mod_dcto[arr_c].f_presentacion

                        DISPLAY  ga_doctos[arr_c].usuario             TO
                                 sarr_mod_dcto[arr_c].usuario
                      END IF
                   END IF
          END IF 

          IF (ga_doctos[arr_c].num_docto IS NULL OR
              ga_doctos[arr_c].num_docto = " "     ) THEN

                  CALL f_cat_doctos(arr_c,bk_doctos[arr_c].num_docto,
                                          bk_doctos[arr_c].descripcion) 
                  RETURNING ga_doctos[arr_c].num_docto,
                            ga_doctos[arr_c].descripcion

              
                  LET ga_doctos[arr_c].f_presentacion = CURRENT 
                  LET ga_doctos[arr_c].usuario        = g_user


                  DISPLAY ga_doctos[arr_c].num_docto     TO
                          sarr_mod_dcto[arr_c].num_docto  

                  DISPLAY ga_doctos[arr_c].descripcion   TO
                          sarr_mod_dcto[arr_c].descripcion

                  DISPLAY  ga_doctos[arr_c].f_presentacion      TO
                           sarr_mod_dcto[arr_c].f_presentacion

                  DISPLAY  ga_doctos[arr_c].usuario             TO
                           sarr_mod_dcto[arr_c].usuario
     
          END IF 

      AFTER FIELD cod_cal
         IF ga_doctos[arr_c].num_docto = 0 THEN
             ERROR"ERROR EN CAPTURA DE CAMPO..."
             LET ga_doctos[arr_c].* = bk_doctos[arr_c].*

             DISPLAY ga_doctos[arr_c].*     TO
                     sarr_mod_dcto[arr_c].*
             ERROR""

         ELSE 
           IF ga_doctos[arr_c].cod_cal IS NULL THEN

              CALL f_st_doctos(bk_doctos[arr_c].cod_cal, 
                               bk_doctos[arr_c].desc_cal)
              RETURNING ga_doctos[arr_c].cod_cal ,
                        ga_doctos[arr_c].desc_cal

               IF ga_doctos[arr_c].cod_cal = 9 THEN

                ERROR"DESCRIPCION INCOMPATIBLE..."

                LET ga_doctos[arr_c].* = bk_doctos[arr_c].*

                DISPLAY ga_doctos[arr_c].* TO sarr_mod_dcto[arr_c].*

                DISPLAY ga_doctos[arr_c].desc_cal         TO
                        sarr_mod_dcto[arr_c].desc_cal
 
                NEXT FIELD cod_cal

               ELSE

                  DISPLAY ga_doctos[arr_c].cod_cal   TO
                          sarr_mod_dcto[arr_c].cod_cal

                  DISPLAY ga_doctos[arr_c].desc_cal   TO
                      sarr_mod_dcto[arr_c].desc_cal

                  IF (FIELD_TOUCHED(cod_cal) AND 
                      bk_doctos[arr_c].cod_cal <> 0) THEN

                    LET ga_doctos[arr_c].f_presentacion = CURRENT
                    LET ga_doctos[arr_c].usuario        = g_user

                    DISPLAY ga_doctos[arr_c].f_presentacion   TO
                            sarr_mod_dcto[arr_c].f_presentacion

                    DISPLAY ga_doctos[arr_c].usuario   TO
                            sarr_mod_dcto[arr_c].usuario
                  END IF
               END IF
           ELSE
             CALL valida_cod_cal(ga_doctos[arr_c].cod_cal)
             RETURNING  ga_doctos[arr_c].desc_cal

             IF ga_doctos[arr_c].cod_cal = 9 THEN

                ERROR"DESCRIPCION INCOMPATIBLE..."

                LET ga_doctos[arr_c].*  = bk_doctos[arr_c].*

                DISPLAY ga_doctos[arr_c].* TO sarr_mod_dcto[arr_c].*

                NEXT FIELD cod_cal
             ELSE 
                 IF FIELD_TOUCHED(cod_cal) THEN

                   LET ga_doctos[arr_c].f_presentacion = CURRENT
                   LET ga_doctos[arr_c].usuario        = g_user

                   DISPLAY ga_doctos[arr_c].desc_cal         TO
                           sarr_mod_dcto[arr_c].desc_cal

                   DISPLAY ga_doctos[arr_c].f_presentacion   TO
                           sarr_mod_dcto[arr_c].f_presentacion

                   DISPLAY ga_doctos[arr_c].usuario   TO
                           sarr_mod_dcto[arr_c].usuario
                 END IF
             END IF
           END IF
         END IF
      ON KEY(ESC)
          FOR i = 1 TO 4 
              IF (ga_doctos[i].num_docto = 0 AND 
                  ga_doctos[i].cod_cal <> 9     )  THEN
                  ERROR"INCOMPATIBILIDAD DE CODIGOS..."
                  NEXT FIELD num_docto
              END IF
              IF (ga_doctos[i].num_docto <> 0 AND 
                  ga_doctos[i].cod_cal = 9     )  THEN
                  ERROR"INCOMPATIBILIDAD DE CODIGOS..."
                  NEXT FIELD num_docto
              END IF
              IF band_inc = 0 THEN
                 IF ga_doctos[i].cod_cal <> 0  THEN
                    LET band_inc = 1
                 END IF
              END IF
          END FOR
          LET band = 0
          EXIT INPUT
      ON KEY(INTERRUPT)
         PROMPT "Proceso cancelado <Enter> para Salir..." for char enter
         LET band       = 0
         LET ver_salida = 1
         EXIT INPUT
    END INPUT
   END WHILE

   IF ver_salida = 1 THEN
      IF band_inc1 = 1  THEN 
          CLOSE WINDOW trab0120
          LET sal_consulta = 1
          RETURN 
      ELSE 
          CLOSE WINDOW trab0120
          RETURN 
      END IF 
   END IF
   WHILE TRUE
      PROMPT "TECLEE <S> PARA GRABAR o <N> PARA CANCELAR :" FOR  enter
          IF enter MATCHES "[sSnN]" THEN
             IF enter MATCHES "[sS]" THEN
                   CALL   insert_doctos()
                   IF band_inc = 1 THEN
                      LET sal_consulta = 1
                   END IF
                   EXIT WHILE
                   RETURN
              ELSE 
                  ERROR"PROCESO CANCELADO" ATTRIBUTE(REVERSE) 
                  SLEEP  1 
                  LET salida_modulo = 1 
                  IF band_inc = 1 THEN
                     LET sal_consulta = 1 
                  END IF
                  RETURN
             END IF
          END IF
        END WHILE
END FUNCTION

FUNCTION insert_doctos()
#id---------------------

    DEFINE v                SMALLINT

    FOR v = 1 TO 4

        IF (ga_doctos[v].num_docto <> bk_doctos[v].num_docto OR
            ga_doctos[v].cod_cal   <> bk_doctos[v].cod_cal  ) THEN

           INSERT INTO tra_his_doctos
           VALUES (g_n_seguro        ,
                   g_n_seguro_ent    ,
                   g_rfc_ent         ,
                   g_cve_ced_cuenta  ,
                   g_nro_ctrl_icefa  ,
                   ga_doctos[v].tipo_docto,
                   ga_doctos[v].num_docto,
                   ga_doctos[v].cod_cal,
                   ga_doctos[v].f_presentacion,
                   ga_doctos[v].usuario)
                   
        END IF
    END FOR
    ERROR"ACTUALIZACIÓN CONCLUIDA..."
    SLEEP 2
    ERROR""
    CLOSE WINDOW trab0120
END FUNCTION

FUNCTION f_cat_doctos(l_tipo,l_num_docto,l_descripcion)
#fcd---------------------------------------------------

    DEFINE   l_tipo              SMALLINT
    DEFINE   l_num_docto         SMALLINT
    DEFINE   l_descripcion       LIKE tra_cat_doctos.descripcion
    DEFINE   i                   SMALLINT
    DEFINE   pa_elem             SMALLINT
    DEFINE   ga_cat_doctos       ARRAY[20] OF RECORD 
             num_docto           SMALLINT ,
             descripcion         LIKE tra_cat_doctos.descripcion
    END RECORD

    OPEN WINDOW win_doc  AT 4,15  WITH FORM "TRAB0122" ATTRIBUTE(BORDER)

    DISPLAY  "  <ENTER> SELECCIONAR                   <ESC> SALIR        "  
    AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY  "       USE  <FLECHAS>  PARA DESPLAZARSE                    "  
    AT 2,1 ATTRIBUTE(BOLD)
  
    CASE l_tipo
     WHEN 1
        DISPLAY  t_solicitud AT 4,1 ATTRIBUTE(REVERSE)
     EXIT CASE
     WHEN 2
        DISPLAY  t_icefa AT 4,1 ATTRIBUTE(REVERSE)
     EXIT CASE
     WHEN 3
        DISPLAY  t_comp AT 4,1 ATTRIBUTE(REVERSE)
     EXIT CASE
     WHEN 4
        DISPLAY  t_ident AT 4,1 ATTRIBUTE(REVERSE)
     EXIT CASE
     OTHERWISE 
     EXIT CASE
    END CASE

    FOR i = 1 TO 20
        INITIALIZE ga_cat_doctos[i].* TO NULL
    END FOR

    LET i_cat = 1

    DECLARE cur_cat_docto CURSOR FOR

      SELECT a.num_docto,
             a.descripcion
      FROM   tra_cat_doctos a
      WHERE  a.tipo_docto = l_tipo
      ORDER  BY  1,2

      FOREACH cur_cat_docto INTO ga_cat_doctos[i_cat].num_docto,
                                 ga_cat_doctos[i_cat].descripcion
              LET i_cat = i_cat + 1
      END FOREACH

    CALL SET_COUNT(i-1)

    LET g_seleccion =  0

    DISPLAY ARRAY ga_cat_doctos TO sa_cat_doctos.*
       ON KEY(CONTROL-M)
          LET pa_elem = arr_curr()
          EXIT DISPLAY
       ON KEY(INTERRUPT)
          LET g_seleccion = 1
          EXIT DISPLAY
    END DISPLAY
    CLOSE WINDOW win_doc
    IF g_seleccion = 1 THEN
       RETURN l_num_docto  ,l_descripcion
    ELSE 
       RETURN ga_cat_doctos[pa_elem].num_docto,
              ga_cat_doctos[pa_elem].descripcion
    END IF
END FUNCTION 

FUNCTION  f_st_doctos(v_cod_cal,v_desc_cal)
#fsd---------------------------------------

    DEFINE v_cod_cal     LIKE tra_st_doctos.cod_cal
    DEFINE v_desc_cal     LIKE tra_st_doctos.desc_cal

    DEFINE   pa_elem             SMALLINT

    OPEN WINDOW win_st AT 14,40 WITH FORM "TRAB0121" ATTRIBUTE(BORDER)

    DISPLAY  "  CONDICIONES  DEL  DOCUMENTO      "  AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY  "<ENTER> SELECCIONAR    <ESC> SALIR "  AT 2,1 ATTRIBUTE(REVERSE)
    DISPLAY  "  USE <FLECHAS> PARA DESPLAZARSE   "  AT 3,1 ATTRIBUTE(BOLD)

    LET i_st  = 1

    DECLARE cur_st_docto CURSOR FOR

    SELECT   *
    FROM   tra_st_doctos
    ORDER  BY  cod_cal

    FOREACH cur_st_docto INTO ga_st_doctos[i_st].*
        LET i_st = i_st + 1
    END FOREACH

    CALL SET_COUNT(i_st - 1)

    LET g_seleccion = 0

    DISPLAY ARRAY ga_st_doctos TO sa_st_doctos.*

       ON KEY (CONTROL-M)
          LET      pa_elem         =  arr_curr()
          EXIT DISPLAY
       ON KEY (INTERRUPT)
          LET      g_seleccion     =  1
          LET      pa_elem         =  arr_curr()
          EXIT DISPLAY
    END DISPLAY

    CLOSE    WINDOW   win_st

    IF g_seleccion = 1 THEN
       RETURN v_cod_cal ,
              v_desc_cal
    ELSE

       RETURN ga_st_doctos[pa_elem].cod_cal,
              ga_st_doctos[pa_elem].desc_cal
    END IF

END FUNCTION 

FUNCTION  valida_cat_doctos(l_tipo_docto,l_num_docto)
#vcd------------------------------------------------

    DEFINE  l_tipo_docto     SMALLINT
    DEFINE  l_num_docto      SMALLINT
    DEFINE  l_count          SMALLINT
    DEFINE  l_desc           LIKE tra_cat_doctos.descripcion

    SELECT A.descripcion,COUNT(*)   
    INTO   l_desc,l_count
    FROM   tra_cat_doctos A
    WHERE  A.tipo_docto   =  l_tipo_docto 
    AND    A.num_docto    =  l_num_docto
    GROUP BY 1

    IF l_count IS NULL THEN 
       LET l_count = 0 
    END IF

    RETURN  l_count,l_desc

END FUNCTION

FUNCTION  valida_cod_cal(l_cod_cal)
#vcc-------------------------------

    DEFINE  l_desc_cal       CHAR(50)
    DEFINE  l_cod_cal        SMALLINT

    SELECT  desc_cal   
    INTO    l_desc_cal
    FROM    tra_st_doctos
    WHERE   cod_cal       =  l_cod_cal

    RETURN  l_desc_cal
            
END FUNCTION
