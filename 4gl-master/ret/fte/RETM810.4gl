#################################################################################
#Owner             => E.F.P.                                                    #
#Programa RETM810  => CAPTURA DE BENEFICIARIOS PARA SOLICITUDES DE RETIROS      #
#                                    (VERSION COPPEL)                           #
#                                                                               #
#Fecha creacion    => 20 DE FEBRERO DE 2004                                     #
#By                => JOSE FRANCISCO LUGO CORNEJO                               #
#Fecha actualiz.   => 22 DE OCTUBRE DE 2004                                     #
#Actualizacion     => ISAI JIMENEZ                                              #
#                  =>                                                           #
#Fecha actualiz.   => 10 DE AGOSTO DE 2010                                      #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                  #
#                  => Se modifica para que permita la captura de otros tipos de #
#                     pago en el registro de pago de beneficiarios              #
#Sistema           => RET                                                       #
#################################################################################
DATABASE safre_af

GLOBALS
    DEFINE gar_banco ARRAY[50] OF RECORD LIKE tab_banco.*

    DEFINE gar_beneficiario ARRAY[5] OF RECORD
        consec_benef        LIKE  ret_beneficiario.consec_benef     ,
        paterno             LIKE  ret_beneficiario.paterno          ,
        materno             LIKE  ret_beneficiario.materno          ,
        nombres             LIKE  ret_beneficiario.nombres          ,
        paren_cod           LIKE  ret_beneficiario.paren_cod        ,
        paren_des           CHAR(30)                                ,
        porcentaje          LIKE  ret_beneficiario.porcentaje       ,
        tipo_pago           LIKE  ret_beneficiario.tipo_pago        ,
        banco               LIKE  ret_beneficiario.banco            ,
        ciudad_cod          INTEGER                                 ,
        estad_cod           INTEGER                                 ,
        cod_sucursal        INTEGER                                 ,
        num_cuenta          LIKE  ret_beneficiario.num_cuenta       ,
        monto_en_pesos      LIKE  ret_beneficiario.monto_en_pesos   ,
        fecha_pago          LIKE  ret_beneficiario.fecha_pago       ,
        dom_calle           LIKE  ret_beneficiario.dom_calle        ,
        dom_numero_ext      LIKE  ret_beneficiario.dom_numero_ext   ,
        dom_numero_int      LIKE  ret_beneficiario.dom_numero_int   ,
        dom_codpos          LIKE  ret_beneficiario.dom_codpos       ,
        dom_colonia         LIKE  ret_beneficiario.dom_colonia      ,
        dom_delega          LIKE  ret_beneficiario.dom_delega       ,
        nom_delega          CHAR(30)                                ,
        dom_ciudad_cod      LIKE  ret_beneficiario.dom_ciudad_cod   ,
        nom_ciudad_cod      CHAR(30)                                ,
        dom_estado_cod      LIKE  ret_beneficiario.dom_estado_cod   ,
        nom_estado_cod      CHAR(30)                                ,
        dom_telefono        LIKE  ret_beneficiario.dom_telefono
    END RECORD

    DEFINE gar_estado ARRAY[1000] OF RECORD
        estad_cod               SMALLINT    ,
        estad_desc              CHAR(40)    ,
        ciudad_cod              SMALLINT    ,
        ciudad_desc             CHAR(40)
    END RECORD

    DEFINE gr_domicilio RECORD
        ciudad                  LIKE afi_domicilio.ciudad               ,
        dom_calle               LIKE afi_domicilio.calle                ,
        dom_numero_ext          LIKE afi_domicilio.numero               ,
        dom_numero_int          LIKE afi_domicilio.depto                ,
        dom_codpos              LIKE afi_domicilio.codpos               ,
        dom_colonia             LIKE afi_domicilio.colonia              ,
        dom_delega              LIKE ret_beneficiario.dom_delega        ,
        nom_delega              CHAR(30)                                ,
        nom_ciudad_cod          CHAR(30)                                ,
        dom_estado_cod          LIKE  ret_beneficiario.dom_estado_cod   ,
        nom_estado_cod          CHAR(30)
    END RECORD

    DEFINE
        HOY                         DATE

    DEFINE
        pos_xxx                     ,
        i                           ,
        arr_c                       ,
        arr_s                       ,
        i_banco                     ,
        g_elimina                   ,
        g_una_vez                   ,
        g_res_porcentaje            SMALLINT

    DEFINE
        g_consecutivo               INTEGER

    DEFINE
        x_error                     CHAR(500)   ,
        x_x                         CHAR(200)   ,
        x_buscar                    CHAR(100)   ,
        g_nss                       CHAR(11)    ,
        g_paterno                   CHAR(20)    ,
        g_materno                   CHAR(20)    ,
        g_nombre                    CHAR(20)    ,
        g_nombre_c                  CHAR(50)    ,
        g_paren_des                 CHAR(20)    ,
        g_user                      CHAR(08)    ,
        g_opcion                    CHAR(01)    ,
        enter                       CHAR(1)

END GLOBALS


MAIN
    DEFER INTERRUPT
    OPTIONS
        ERROR  LINE LAST    ,
        PROMPT LINE LAST    ,
        ACCEPT KEY CONTROL-I

    CALL f_lib_crea_log()
    CALL control_beneficiarios()

END MAIN


FUNCTION control_beneficiarios()
   DEFINE  l_parm3           CHAR(03)
   DEFINE  l_ya_existe       SMALLINT
   DEFINE   prep_qry         CHAR(500)

    -- -----------------------------------------------------------------------------

   LET HOY            = TODAY

   OPEN WINDOW RETBEN000 AT 2,2 WITH FORM "RETM8101" ATTRIBUTE(BORDER)
   DISPLAY "                      DATOS  DE  BENEFICIARIOS                                " AT 4,1 ATTRIBUTE(REVERSE)
   DISPLAY "                  DATOS  BANCARIOS  PARA  PAGO  DEL  RETIRO                          " AT 9,1 ATTRIBUTE(REVERSE)
   DISPLAY "                    DOMICILIO  DEL  BENEFICIARIO                                   " AT 12,1 ATTRIBUTE(REVERSE)

    LET g_user = f_lib_obten_user()

    LET g_nss         = ARG_VAL(1)
    LET g_consecutivo = ARG_VAL(2)
    LET l_parm3       = ARG_VAL(3)

    CALL f_obten_domicilio(g_nss)
        RETURNING g_paterno     ,
                  g_materno     ,
                  g_nombre      ,
                  gr_domicilio.*

   LET g_nombre_c   = g_nombre  CLIPPED , " ",
                      g_paterno CLIPPED , " ",
                      g_materno CLIPPED

   LET i_banco = 1

   DECLARE c_bancos CURSOR FOR
   SELECT *
   FROM   tab_banco
   ORDER  BY  1

   FOREACH c_bancos INTO gar_banco[i_banco].*
      LET i_banco = i_banco + 1
   END FOREACH

   LET i_banco     = i_banco  -  1
   LET l_ya_existe = 0

   CALL inicializa()

    MENU "MENU BENEFICIARIOS"
        COMMAND KEY(A) "(A)grega" "Agrega beneficiario"
            CALL   agrega()

        COMMAND KEY(C) "(C)onsulta" "Consulta beneficiario"
            CALL   consulta()

        COMMAND KEY(M) "(M)odifica" "Modifica beneficiario"
            CALL modificaciones() #m

        COMMAND KEY(E) "(E)limina" "Elimina beneficiario"
            LET    g_elimina        =  1
            CALL   consulta()
            LET    g_elimina        =  0

        COMMAND "Salir"

            --Verifica que el NSS quede con beneficiarios
            IF cuenta_beneficiarios() = 0 THEN
               ERROR " EL AFILIADO NO PUEDE QUEDAR SIN BENEFICIARIOS " --IJR
            ELSE
               IF suma_porcentajes() != 100 THEN
                  ERROR " LOS PORCENTAJES DEBEN SUMAR EL 100% "   --IJR
               ELSE
                  EXIT MENU
               END IF
            END IF
    END MENU

END FUNCTION


FUNCTION  inicializa()
    DEFINE i SMALLINT

    FOR i = 1 TO 5
        INITIALIZE  gar_beneficiario[i].* TO  NULL
        LET gar_beneficiario[i].porcentaje      = 0
        LET gar_beneficiario[i].consec_benef    = i
        LET gar_beneficiario[i].dom_codpos      = 0
        LET gar_beneficiario[i].tipo_pago       = 1
        LET gar_beneficiario[i].ciudad_cod      = gr_domicilio.ciudad
        LET gar_beneficiario[i].estad_cod       = gr_domicilio.dom_estado_cod
        LET gar_beneficiario[i].dom_calle       = gr_domicilio.dom_calle
        LET gar_beneficiario[i].dom_numero_ext  = gr_domicilio.dom_numero_ext
        LET gar_beneficiario[i].dom_numero_int  = gr_domicilio.dom_numero_int
        LET gar_beneficiario[i].dom_codpos      = gr_domicilio.dom_codpos
        LET gar_beneficiario[i].dom_colonia     = gr_domicilio.dom_colonia
        LET gar_beneficiario[i].dom_delega      = gr_domicilio.dom_delega
        LET gar_beneficiario[i].nom_delega      = gr_domicilio.nom_delega
        LET gar_beneficiario[i].nom_ciudad_cod  = gr_domicilio.nom_ciudad_cod
        LET gar_beneficiario[i].dom_estado_cod  = gr_domicilio.dom_estado_cod
        LET gar_beneficiario[i].nom_estado_cod  = gr_domicilio.nom_estado_cod
    END FOR

    CLEAR FORM
    DISPLAY  g_nss      TO nss
    DISPLAY  g_nombre_c TO nombre
    LET      arr_s     = 1
    LET      g_elimina = 0

END FUNCTION
{===========================================================================}
{ Objetivo : Permitir la captura de beneficiarios                           }
{ Regresa  :                                                                }
{ Modifico : IJR 22/10/2004 11:25a.m.                                       }
{===========================================================================}
FUNCTION agrega()

   DEFINE
        ls_es_judicial          ,
        i                       ,
        l_porcentaje            ,
        x                       ,
        l_count                 SMALLINT

    DEFINE
        lc_tipo_pension         CHAR(2)     ,
        l_mensaje               CHAR(80)

    -- -----------------------------------------------------------------------------

   DISPLAY "   [ Esc ]  Salir de  Agregar  Beneficiarios        " AT 2,1

   -- Verifica el porcentaje Total de beneficiarios
   SELECT SUM( NVL(porcentaje,0) ),COUNT(*)
   INTO   l_porcentaje, l_count
   FROM   ret_beneficiario
   WHERE  nss          = g_nss
   AND    consecutivo  = g_consecutivo

   IF l_porcentaje = 100  THEN
      LET l_mensaje=" LOS BENEFICIARIOS(",l_count USING "&",") YA ESTAN AL 100% "
      ERROR l_mensaje
      CALL consulta()
   ELSE
      LET  g_una_vez = 1
      LET INT_FLAG = FALSE

      CALL SET_COUNT(5)

      --CAPTURA DEL BENEFICIARIO

      INPUT ARRAY  gar_beneficiario WITHOUT DEFAULTS FROM  sa_benef.*

         ---------------------------
         BEFORE ROW
         ---------------------------
            LET arr_c = ARR_CURR()

            CALL f_determina_laudo(g_nss, g_consecutivo)
                RETURNING lc_tipo_pension   ,
                          ls_es_judicial

            IF (ls_es_judicial = TRUE) THEN
                LET gar_beneficiario[arr_c].tipo_pago = 5
                DISPLAY gar_beneficiario[arr_c].tipo_pago TO sa_benef[arr_s].tipo_pago
            END IF


         ---------------------------
         BEFORE FIELD  paterno
         ---------------------------
            LET arr_c = ARR_CURR()

            IF g_una_vez THEN

                IF lc_tipo_pension = "VO" OR
                   lc_tipo_pension = "VI" OR
                   lc_tipo_pension = "OR" OR
                   lc_tipo_pension = "RE" OR
                   lc_tipo_pension = "AS"
                THEN
                    LET gar_beneficiario[arr_c].paterno   = NULL
                    LET gar_beneficiario[arr_c].materno   = NULL
                    LET gar_beneficiario[arr_c].nombres   = NULL
                    LET gar_beneficiario[arr_c].paren_cod = 0
                    LET gar_beneficiario[arr_c].paren_des = NULL

                    ERROR "CAPTURE BENEFICIARIOS"
                ELSE
                    LET gar_beneficiario[arr_c].paterno   = g_paterno
                    LET gar_beneficiario[arr_c].materno   = g_materno
                    LET gar_beneficiario[arr_c].nombres   = g_nombre
                    LET gar_beneficiario[arr_c].paren_cod = 12
                    LET gar_beneficiario[arr_c].paren_des = "TITULAR"
                END IF

                DISPLAY gar_beneficiario[arr_c].paterno   TO sa_benef[arr_s].paterno
                DISPLAY gar_beneficiario[arr_c].materno   TO sa_benef[arr_s].materno
                DISPLAY gar_beneficiario[arr_c].nombres   TO sa_benef[arr_s].nombres
                DISPLAY gar_beneficiario[arr_c].paren_cod TO sa_benef[arr_s].paren_cod
                DISPLAY gar_beneficiario[arr_c].paren_des TO sa_benef[arr_s].paren_des
                DISPLAY gar_beneficiario[arr_c].consec_benef TO sa_benef[arr_s].consec_benef

                LET g_una_vez = 0
            END IF

            LET l_porcentaje = 0
            FOR i = 1 TO 5
               IF gar_beneficiario[i].dom_codpos IS NOT NULL AND
                  gar_beneficiario[i].paterno    IS NOT NULL AND
                  gar_beneficiario[i].materno    IS NOT NULL THEN
                  LET l_porcentaje = l_porcentaje + gar_beneficiario[i].porcentaje
               END IF
            END FOR

            LET g_res_porcentaje = 100 - l_porcentaje
            IF gar_beneficiario[arr_c].porcentaje IS NULL  OR
               gar_beneficiario[arr_c].porcentaje = 0   THEN
               LET     gar_beneficiario[arr_c].porcentaje = g_res_porcentaje
               DISPLAY gar_beneficiario[arr_c].porcentaje TO sa_benef[arr_s].porcentaje
            END IF

         -----------------------
         AFTER FIELD paterno
         -----------------------
            IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD PREVIOUS
            END IF

            IF gar_beneficiario[arr_c].paterno IS NULL THEN
               ERROR "  APELLIDO  PATERNO  NO  PUEDE  SER  NULO  "
               NEXT FIELD paterno
            END IF

         ---------------------------
         AFTER FIELD nombres
         ---------------------------
            IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD PREVIOUS
            END IF

            IF gar_beneficiario[arr_c].nombres    IS    NULL   THEN
               ERROR "   EL  NOMBRE  NO  PUEDE  SER  NULO  "
               NEXT FIELD nombres
            END IF

         ------------------------
         AFTER FIELD paren_cod
         ------------------------
            IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD PREVIOUS
            END IF

            IF gar_beneficiario[arr_c].paren_cod IS NOT NULL  THEN
               -- Valida que el codigo sea valido buscando la descripcion del beneficiario
               SELECT paren_desc
               INTO   gar_beneficiario[arr_c].paren_des
               FROM   tab_parentesco
               WHERE  paren_cod  =  gar_beneficiario[arr_c].paren_cod

               IF SQLCA.SQLCODE = NOTFOUND THEN
                  ERROR "        NO  EXISTE  CLAVE  DE  PARENTESCO    "
                  NEXT FIELD paren_cod
               ELSE
                  DISPLAY gar_beneficiario[arr_c].paren_des TO sa_benef[arr_s].paren_des
               END IF
            ELSE
               --Ventana de Ayuda de Parentescos
               CALL Despliegar_parentescos()
               RETURNING gar_beneficiario[arr_c].paren_cod,gar_beneficiario[arr_c].paren_des

               IF gar_beneficiario[arr_c].paren_cod <= 0 THEN
                  ERROR "        NO  EXISTE  CLAVE  DE  PARENTESCO    "
                  NEXT FIELD paren_cod
               END IF

               DISPLAY gar_beneficiario[arr_c].paren_cod TO sa_benef[arr_s].paren_cod
               DISPLAY gar_beneficiario[arr_c].paren_des TO sa_benef[arr_s].paren_des
               NEXT FIELD porcentaje
            END IF

         ------------------------
         AFTER FIELD porcentaje
         ------------------------
            IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD PREVIOUS
            END IF

            IF gar_beneficiario[arr_c].porcentaje IS NULL THEN
               ERROR "   EL  PORCENTAJE  NO  PUEDE  SER  NULO  "
               NEXT FIELD porcentaje
            ELSE
               LET l_porcentaje = 0
               FOR i = 1 TO  5
                  IF gar_beneficiario[i].dom_codpos    IS  NOT  NULL  AND
                     gar_beneficiario[i].paterno       IS  NOT  NULL  AND
                     gar_beneficiario[i].materno       IS  NOT  NULL  AND
                     gar_beneficiario[i].porcentaje    >  0          THEN
                     LET l_porcentaje = l_porcentaje + gar_beneficiario[i].porcentaje
                  END IF
               END FOR
               IF l_porcentaje > 100 THEN
                    ERROR "       EL PORCENTAJE  EXCEDE DEL 100%      "
                    NEXT FIELD porcentaje
               END IF
            END IF

         ------------------------
         BEFORE FIELD tipo_pago
         ------------------------
            -- CPL-1696
            -- Se muestra el tipo de pago 5 para los retiros de laudo
            IF (ls_es_judicial = TRUE) THEN
                LET gar_beneficiario[arr_c].tipo_pago = 5
                DISPLAY gar_beneficiario[arr_c].tipo_pago TO sa_benef[arr_s].tipo_pago
                NEXT FIELD banco
            END IF

         ------------------------
         AFTER FIELD tipo_pago
         ------------------------
            IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD PREVIOUS
            END IF

            -- CPL-1696
            -- Se valida que se capture el tipo 5 solo para los retiros por laudo judicial
            IF ( (ls_es_judicial = FALSE) AND (gar_beneficiario[arr_c].tipo_pago = 5) ) THEN
                ERROR "TIPO NO VALIDO PARA RETIROS QUE NO SEAN LAUDO JUDICIAL"
                NEXT FIELD  tipo_pago
            ELSE
                -- CPL-1587
                -- Se muestra el tipo de pago 4 en la captura
                IF gar_beneficiario[arr_c].tipo_pago IS NULL THEN
                   ERROR "TIPOS DE PAGO: 1=CAJA COPPEL, 2=BANCOMER, 3=BANCOPPEL, 4=OTROS BANCOS"
                   NEXT FIELD  tipo_pago
                ELSE
                    IF gar_beneficiario[arr_c].tipo_pago = 3 THEN
                        NEXT FIELD ciudad_cod
                    END IF
                END IF
            END IF

         ------------------------
         AFTER FIELD banco
         ------------------------
            IF FGL_LASTKEY()= FGL_KEYVAL("LEFT") THEN
               NEXT FIELD PREVIOUS
            END IF

            IF gar_beneficiario[arr_c].banco IS NOT NULL  THEN
               --Checa si es un banco valido
               SELECT COUNT(*)
               INTO   l_count
               FROM   tab_banco
               WHERE  banco  =  gar_beneficiario[arr_c].banco

               IF l_count = 0  THEN
                  ERROR "        NO  EXISTE  CLAVE  DE  BANCO    "
                  NEXT FIELD banco
               END IF
            ELSE
               CALL trae_banco()
               DISPLAY  gar_beneficiario[arr_c].banco TO sa_benef[arr_S].banco
               NEXT FIELD ciudad_cod
            END IF

         ------------------------
         AFTER FIELD ciudad_cod
         ------------------------
            IF FGL_LASTKEY()= FGL_KEYVAL("LEFT") THEN
               NEXT FIELD PREVIOUS
            END IF

            IF gar_beneficiario[arr_c].ciudad_cod IS NULL  OR
               gar_beneficiario[arr_c].ciudad_cod = 0 THEN
               LET pos_xxx = 0
               INITIALIZE x_x, x_buscar TO NULL

               -- despleagado de Ayuda de ciudades
               OPEN WINDOW glob_ret8 AT 08,07 WITH FORM "GLOB_RET8" ATTRIBUTE(BORDER)
               DISPLAY "                      ESTADO / CIUDADES                                   " AT 1,1 ATTRIBUTE(REVERSE)

               LET x_x = " SELECT A.estad_cod,B.estad_desc,A.ciudad_cod,",
                         " A.ciudad_desc FROM tab_ciudad A,tab_estado B ",
                         " WHERE  A.estad_cod = B.estad_cod ",
                         " ORDER BY 1 "

               PREPARE pre_18 FROM x_x
               DECLARE cur_18 CURSOR FOR pre_18

               LET pos_xxx = 1
               FOREACH cur_18 INTO gar_estado[pos_xxx].*
                  LET pos_xxx = pos_xxx + 1
                  IF pos_xxx >= 1000 THEN
                     ERROR "    FUE SOBREPASADA LA CAPACIDAD MAXIMA DEL ARREGLO" ATTRIBUTE(NORMAL)
                     EXIT FOREACH
                  END IF
               END FOREACH
               LET pos_xxx = pos_xxx-1

               IF pos_xxx < 1 THEN
                  ERROR "    REGISTRO INEXISTENTE" ATTRIBUTE(NORMAL)
                  SLEEP 2
                  ERROR ""
                  CLOSE WINDOW glob_ret8
                  NEXT FIELD  ciudad_cod
               ELSE
                  CALL SET_COUNT(pos_xxx)
                  -- Desplegado de la Lista de Ciudades
                  DISPLAY ARRAY gar_estado TO scr_1.*
                     ON KEY ( INTERRUPT )
                        LET pos_xxx = 0
                        EXIT DISPLAY
                     ON KEY ( CONTROL-M )
                        LET pos_xxx = ARR_CURR()
                        LET gar_beneficiario[arr_c].dom_estado_cod = gar_estado[pos_xxx].estad_cod
                        --
                        LET gar_beneficiario[arr_c].estad_cod      = gar_estado[pos_xxx].estad_cod
                        --
                        LET gar_beneficiario[arr_c].ciudad_cod     = gar_estado[pos_xxx].ciudad_cod
                        EXIT DISPLAY
                  END DISPLAY
               END IF
               CLOSE WINDOW glob_ret8
               -- Desplegado del codigo de ciudad seleccionada
               DISPLAY BY NAME gar_beneficiario[arr_c].ciudad_cod
               DISPLAY BY NAME gar_beneficiario[arr_c].estad_cod
            ELSE
               --Si no es nulo valida que sea una ciudad valida
               SELECT "X"
               FROM   tab_ciudad
               WHERE  ciudad_cod = gar_beneficiario[arr_c].ciudad_cod

               IF STATUS = NOTFOUND THEN
                  ERROR "    CODIGO INEXISTENTE"  ATTRIBUTE(NORMAL)
                  NEXT FIELD ciudad_cod
               END IF
               --
               SELECT estad_cod INTO gar_beneficiario[arr_c].estad_cod
               FROM   tab_ciudad
               WHERE  ciudad_cod = gar_beneficiario[arr_c].ciudad_cod
               DISPLAY BY NAME gar_beneficiario[arr_c].ciudad_cod
               DISPLAY BY NAME gar_beneficiario[arr_c].estad_cod
               --
            END IF

            IF gar_beneficiario[arr_c].tipo_pago = 3 THEN
                NEXT FIELD num_cuenta
            END IF

         ------------------------
         AFTER FIELD cod_sucursal
         ------------------------
            IF FGL_LASTKEY()= FGL_KEYVAL("LEFT") THEN
               NEXT FIELD PREVIOUS
            END IF

            IF gar_beneficiario[arr_c].cod_sucursal IS NULL  THEN
               ERROR "  LA  SUCURSAL  NO  PUEDE  SER  NULA  "
               NEXT FIELD cod_sucursal
            END IF

         ------------------------
         BEFORE FIELD num_cuenta
         ------------------------
            IF gar_beneficiario[arr_c].tipo_pago = 2  THEN   -- Orden de Pago
               LET gar_beneficiario[arr_c].num_cuenta = NULL
               DISPLAY  gar_beneficiario[arr_c].num_cuenta TO sa_benef[arr_s].num_cuenta
               NEXT FIELD dom_calle
            END IF
         ------------------------
         AFTER FIELD num_cuenta
         ------------------------
            IF FGL_LASTKEY()= FGL_KEYVAL("LEFT") THEN
               NEXT FIELD PREVIOUS
            END IF

            IF gar_beneficiario[arr_c].tipo_pago = 1 AND gar_beneficiario[arr_c].num_cuenta IS NULL THEN
               ERROR "DEBE PROPORCIONAR EL NUMERO DE CUENTA"   --IJR
               NEXT FIELD num_cuenta
            END IF

         ------------------------
         AFTER FIELD dom_calle
         ------------------------
            IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
               IF gar_beneficiario[arr_c].tipo_pago  = 2 THEN
                  NEXT FIELD cod_sucursal
               ELSE
                  NEXT FIELD PREVIOUS
               END IF
            END IF

            IF gar_beneficiario[arr_c].dom_calle IS NULL  THEN
               ERROR "  LA  CALLE  NO  PUEDE  SER  NULA  "
               NEXT FIELD dom_calle
            END IF

         ------------------------
         AFTER FIELD dom_codpos
         ------------------------
            IF FGL_LASTKEY()= FGL_KEYVAL("LEFT") THEN
               NEXT FIELD PREVIOUS
            END IF

            IF gar_beneficiario[arr_c].dom_codpos IS NULL THEN
               -- Muestra catalogo de codigos
               CALL Despliega_codigo_postal_1(gar_beneficiario[arr_c].dom_estado_cod)
               RETURNING gar_beneficiario[arr_c].dom_codpos ,   -- codigo_postal
                         gar_beneficiario[arr_c].dom_colonia,   -- descripcion colonia
                         gar_beneficiario[arr_c].dom_delega ,   -- codigo_delegacion
                         gar_beneficiario[arr_c].nom_delega ,   -- descripcion delegacion
                         gar_beneficiario[arr_c].dom_ciudad_cod,  -- codigo_ciudad
                         gar_beneficiario[arr_c].nom_ciudad_cod,  -- descripcion ciudad
                         gar_beneficiario[arr_c].dom_estado_cod,  -- codigo_estado
                         gar_beneficiario[arr_c].nom_estado_cod   -- descripcion estado
            ELSE
               --Valida que sea un codigo valido
               IF LENGTH(gar_beneficiario[arr_c].dom_codpos) <> 5 THEN
                  ERROR "Ingrese cinco digitos para indicar Codigo Postal"
                  NEXT FIELD dom_codpos
               END IF

               --Verifica que exista el codigo capturado en catalogo
               SELECT "X"
               FROM   tab_codpos
               WHERE  cpos_cod = gar_beneficiario[arr_c].dom_codpos

               IF STATUS = NOTFOUND THEN
                  ERROR "CODIGO POSTAL NO EXISTE EN CATALOGO"
                  NEXT FIELD dom_codpos
               ELSE
                  CALL Despliega_colonias(gar_beneficiario[arr_c].dom_codpos)
                  RETURNING gar_beneficiario[arr_c].dom_colonia,  -- descripcion colonia
                            gar_beneficiario[arr_c].dom_delega,   -- codigo_delegacion
                            gar_beneficiario[arr_c].nom_delega,   -- descripcion delegacion
                            gar_beneficiario[arr_c].dom_ciudad_cod, -- codigo_ciudad
                            gar_beneficiario[arr_c].nom_ciudad_cod, -- descripcion ciudad
                            gar_beneficiario[arr_c].dom_estado_cod, -- codigo_estado
                            gar_beneficiario[arr_c].nom_estado_cod  -- descripcion estado
               END IF
            END IF

            --Despliega los Datos recuperados
            DISPLAY gar_beneficiario[arr_c].dom_codpos     TO sa_benef[arr_s].dom_codpos
            DISPLAY gar_beneficiario[arr_c].dom_colonia    TO sa_benef[arr_s].dom_colonia
            DISPLAY gar_beneficiario[arr_c].dom_delega     TO sa_benef[arr_s].dom_delega
            DISPLAY gar_beneficiario[arr_c].dom_ciudad_cod TO sa_benef[arr_s].dom_ciudad_cod
            DISPLAY gar_beneficiario[arr_c].dom_estado_cod TO sa_benef[arr_s].dom_estado_cod
            DISPLAY gar_beneficiario[arr_c].nom_delega     TO sa_benef[arr_s].nom_delega
            DISPLAY gar_beneficiario[arr_c].nom_ciudad_cod TO sa_benef[arr_s].nom_ciudad_cod
            DISPLAY gar_beneficiario[arr_c].nom_estado_cod TO sa_benef[arr_s].nom_estado_cod

            NEXT  FIELD  dom_telefono

         -----------
         AFTER ROW     --IJR
         -----------
            -- Por si el usuario quiere cancelar la captura
            IF FGL_LASTKEY()= FGL_KEYVAL("INTERRUPT") OR
               FGL_LASTKEY()= FGL_KEYVAL("CONTROL-C") THEN
               LET INT_FLAG = TRUE
               EXIT INPUT
            END IF

            -- No podra cambiar de registro SI los siguientes campos son nulos
            IF gar_beneficiario[arr_c].paterno    IS NULL OR
               gar_beneficiario[arr_c].nombres    IS NULL OR
               gar_beneficiario[arr_c].paren_cod  IS NULL OR
               gar_beneficiario[arr_c].tipo_pago  IS NULL OR
--               gar_beneficiario[arr_c].banco      IS NULL OR
               gar_beneficiario[arr_c].dom_codpos IS NULL THEN
               ERROR "NO PUEDE CAMBIAR DE REGISTRO HABIENDO CAMPOS VACIOS"
               NEXT FIELD paterno
            END IF

            --Cuenta y revisa los porcentajes
            LET l_porcentaje = 0

            FOR i = 1 TO 5
               IF gar_beneficiario[i].paterno    IS NOT NULL AND
                  gar_beneficiario[i].nombres    IS NOT NULL AND
                  gar_beneficiario[i].paren_cod  IS NOT NULL AND
                  gar_beneficiario[i].tipo_pago  IS NOT NULL AND
--                  gar_beneficiario[i].banco      IS NOT NULL AND
                  gar_beneficiario[i].dom_codpos IS NOT NULL THEN
                  LET l_porcentaje = l_porcentaje + gar_beneficiario[i].porcentaje
               END IF
            END FOR

            -- si ya se llego al 100 ya no permite pasar al siguiente registro
            IF l_porcentaje = 100  THEN
               EXIT INPUT
            END IF

         ------------------------
         ON KEY(ESC)
         ------------------------
            -- VALIDA LOS CAMPOS QUE NO PUEDEN SER NULOS
            IF gar_beneficiario[arr_c].paterno IS NULL  THEN
               ERROR "  EL  NOMBRE  NO  PUEDE  SER  NULO              "
               NEXT  FIELD  paterno
            END IF

            IF gar_beneficiario[arr_c].nombres IS NULL  THEN
               ERROR  "     EL  NOMBRE  NO  PUEDE  SER  NULO     "
               NEXT  FIELD  nombres
            END IF

            IF gar_beneficiario[arr_c].paren_cod IS NULL  THEN
               ERROR "     EL  PARENTESCO NO  PUEDE  SER  NULO     "
               NEXT FIELD paren_cod
            END IF

            IF gar_beneficiario[arr_c].porcentaje IS NULL  THEN
               ERROR "     EL  PORCENTAJE  DEBE  SER  MAYOR  A  CERO   "
               NEXT FIELD porcentaje
            END IF

            IF gar_beneficiario[arr_c].tipo_pago IS NULL  THEN
               ERROR "    EL  TIPO  DE  PAGO  NO  PUEDE  SER  NULO    "
               NEXT FIELD tipo_pago
            END IF

            IF gar_beneficiario[arr_c].banco IS NULL  THEN
                IF gar_beneficiario[arr_c].tipo_pago <> 3 THEN
                    NEXT FIELD  banco
                    ERROR  "      EL  BANCO  NO  PUEDE SER  NULO    "
                END IF
            END IF

            IF gar_beneficiario[arr_c].ciudad_cod IS NULL  THEN
               ERROR "    LA  CLAVE  DE  CIUDAD  NO  PUEDE  SER  NULA  "
               NEXT FIELD ciudad_cod
            END IF

            IF gar_beneficiario[arr_c].cod_sucursal IS NULL  THEN
                IF gar_beneficiario[arr_c].tipo_pago <> 3 THEN
                    ERROR "     EL  NUMERO  DE  SUCURSAL  NO  PUEDE  SER  NULO     "
                    NEXT FIELD cod_sucursal
                END IF
            END IF

            IF gar_beneficiario[arr_c].tipo_pago = 1 AND gar_beneficiario[arr_c].num_cuenta IS NULL  THEN
               ERROR "     EL  NUMERO  DE  CUENTA  NO  PUEDE  SER  NULO     "
               NEXT  FIELD  num_cuenta
            END IF

            IF gar_beneficiario[arr_c].dom_calle IS NULL  THEN
               ERROR "     EL  NOMBRE  DE  LA  CALLE  NO  PUEDE  SER  NULO   "
               NEXT FIELD  dom_calle
            END IF

            IF gar_beneficiario[arr_c].dom_codpos IS NULL  THEN
               ERROR "    EL  CODIGO  NO  PUEDE  SER  NULO  "
               NEXT FIELD dom_codpos
            END IF

            LET l_porcentaje = 0
            FOR i = 1 TO 5
               IF gar_beneficiario[i].paterno    IS NOT NULL AND
                  gar_beneficiario[i].nombres    IS NOT NULL AND
                  gar_beneficiario[i].tipo_pago  IS NOT NULL AND
                  gar_beneficiario[i].paren_cod  IS NOT NULL AND
                  gar_beneficiario[i].dom_codpos IS NOT NULL THEN
                  LET l_porcentaje = l_porcentaje + gar_beneficiario[i].porcentaje
               END IF
            END FOR

            IF l_porcentaje = 100  THEN
               EXIT INPUT
            ELSE
               ERROR "    EL  PORCENTAJE  DEBE  SER  EL  100%  "
               NEXT  FIELD  porcentaje
            END IF
         ------------------------
         ON KEY (control-c)
         ------------------------
            LET int_flag = TRUE
            EXIT INPUT

      END INPUT

      IF INT_FLAG THEN
         ERROR " CAPTURA CANCELADA POR EL USUARIO "
      ELSE
         -- inserta los detalles capturados
         FOR arr_c = 1 TO 5
            IF gar_beneficiario[arr_c].porcentaje > 0  THEN
               CALL insert_ret_beneficiario()
               ERROR "REGISTRO INGRESADO"
            END IF
         END FOR
         CLEAR  FORM
      END IF

   END IF

END FUNCTION
{===========================================================================}
{ Objetivo : REALIZAR MODIFICACIONES A BENEFICIARIOS                        }
{ Regresa  :                                                                }
{ Modifico : IJR 21/10/04                                                   }
{===========================================================================}
FUNCTION modificaciones()
#pp-------------------------
   DEFINE i              SMALLINT
   DEFINE l_porcentaje   SMALLINT
   DEFINE x              SMALLINT
   DEFINE l_no_existe    SMALLINT
   DEFINE l_count        SMALLINT
   DEFINE l_name         CHAR(50)
   DEFINE l_numregs      SMALLINT  -- cuantos elementos se cargaron en el arreglo
   DEFINE l_mensaje      CHAR(80)
   DEFINE cont_auxiliar  SMALLINT
   DEFINE tot_arr        SMALLINT

   DISPLAY "   [ Esc ]  Graba  Modificaciones     " AT 2,1

   LET l_no_existe = 0
   LET INT_FLAG = FALSE

   -- Verifica la existencia de beneficiarios

   SELECT COUNT(*)
   INTO   l_no_existe
   FROM   ret_beneficiario
   WHERE  nss         = g_nss
   AND    consecutivo = g_consecutivo

   IF NOT l_no_existe THEN
      ERROR "   LOS  BENEFICIARIOS   NO  ESTAN  DADOS  DE  ALTA   "
      CALL consulta()
   ELSE
      -- Lee los registros de beneficiarios y los carga en el arreglo
      CALL lee_ret_beneficiario() RETURNING l_numregs
      CALL SET_COUNT(l_numregs)

      --Modificaciones de los registros
      INPUT ARRAY  gar_beneficiario WITHOUT DEFAULTS FROM  sa_benef.*

         ---------------------------
         BEFORE FIELD  paterno
         ---------------------------
            LET arr_c   = ARR_CURR()
            LET tot_arr = ARR_COUNT()   --Total de elementos cargados en el arreglo

            -- Se asigna nuevo consecutivo Si no tiene valor el campo
            IF gar_beneficiario[arr_c].consec_benef IS NULL OR
               gar_beneficiario[arr_c].consec_benef = 0   THEN
               --obtiene el Nuevo numero de secuencia de beneficiario
               SELECT MAX( nvl(consec_benef,0) ) + 1
               INTO   gar_beneficiario[arr_c].consec_benef
               FROM   ret_beneficiario
               WHERE  nss = g_nss
               AND   consecutivo = g_consecutivo
               DISPLAY gar_beneficiario[arr_c].consec_benef TO sa_benef[arr_s].consec_benef
            END IF

            LET l_porcentaje = 0
            FOR i = 1 TO 5
               IF gar_beneficiario[i].dom_codpos IS NOT NULL AND
                  gar_beneficiario[i].paterno    IS NOT NULL AND
                  gar_beneficiario[i].materno    IS NOT NULL THEN
                  LET l_porcentaje = l_porcentaje + gar_beneficiario[i].porcentaje
               END IF
            END FOR

            LET g_res_porcentaje = 100 - l_porcentaje
            IF gar_beneficiario[arr_c].porcentaje IS NULL OR
               gar_beneficiario[arr_c].porcentaje         = 0  THEN
               LET     gar_beneficiario[arr_c].porcentaje = g_res_porcentaje
               DISPLAY gar_beneficiario[arr_c].porcentaje TO sa_benef[arr_s].porcentaje
            END IF
         ---------------------------
         AFTER FIELD paterno
         ---------------------------
            IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD PREVIOUS
            END IF

            IF gar_beneficiario[arr_c].paterno IS NULL  THEN
               ERROR "   APELLIDO  PATERNO  NO  PUEDE  SER  NULO  "
               NEXT FIELD paterno
            END IF
         ------------------------
         AFTER FIELD nombres
         ------------------------
            IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
               NEXT FIELD PREVIOUS
            END IF

            IF gar_beneficiario[arr_c].nombres IS NULL  THEN
               ERROR  "   EL  NOMBRE  NO  PUEDE  SER  NULO  "
               NEXT FIELD nombres
            END IF
         ------------------------
         AFTER FIELD paren_cod
         ------------------------
            IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
               NEXT FIELD PREVIOUS
            END IF

            IF gar_beneficiario[arr_c].paren_cod IS NOT NULL  THEN
               -- Valida que el codigo sea valido buscando la descripcion del beneficiario
               SELECT paren_desc
               INTO   gar_beneficiario[arr_c].paren_des
               FROM   tab_parentesco
               WHERE  paren_cod = gar_beneficiario[arr_c].paren_cod

               IF SQLCA.SQLCODE = NOTFOUND THEN
                  ERROR "        NO  EXISTE  CLAVE  DE  PARENTESCO    "
                  NEXT FIELD paren_cod
               ELSE
                  DISPLAY gar_beneficiario[arr_c].paren_des TO sa_benef[arr_s].paren_des
               END IF
            ELSE
               --Despliega Pantalla de parentescos
               CALL Despliegar_parentescos()
               RETURNING gar_beneficiario[arr_c].paren_cod,gar_beneficiario[arr_c].paren_des

               IF gar_beneficiario[arr_c].paren_cod <= 0 THEN
                  ERROR "        NO  EXISTE  CLAVE  DE  PARENTESCO    "
                  NEXT FIELD paren_cod
               END IF

               DISPLAY   gar_beneficiario[arr_c].paren_cod TO sa_benef[arr_s].paren_cod
               DISPLAY   gar_beneficiario[arr_c].paren_des TO sa_benef[arr_s].paren_des
               NEXT FIELD porcentaje
            END IF
         ------------------------
         AFTER FIELD  porcentaje
         ------------------------
            IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD PREVIOUS
            END IF

       IF gar_beneficiario[arr_c].porcentaje IS NULL  THEN
              ERROR "   EL  PORCENTAJE  NO  PUEDE  SER  NULO  "
              NEXT  FIELD  porcentaje
           ELSE
              LET l_porcentaje = 0
              FOR i = 1 TO 5
                 IF gar_beneficiario[i].dom_codpos IS NOT NULL AND
                    gar_beneficiario[i].paterno    IS NOT NULL AND
                    gar_beneficiario[i].materno    IS NOT NULL AND
                    gar_beneficiario[i].porcentaje > 0  THEN
                    LET l_porcentaje = l_porcentaje + gar_beneficiario[i].porcentaje
                 END IF
              END FOR

              IF l_porcentaje > 100  THEN
                 ERROR "       EL PORCENTAJE  EXCEDE DEL 100%      "
                 NEXT FIELD porcentaje
              END IF
           END IF
         ------------------------
         AFTER FIELD tipo_pago
         ------------------------
            IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
                NEXT FIELD PREVIOUS
            END IF

            IF gar_beneficiario[arr_c].tipo_pago IS NULL  THEN
               ERROR " TIPOS DE PAGO ( 1=ABONO EN CUENTA  2=ORDENES DE PAGO  3=PAGO CON CHEQUE )        "
               NEXT FIELD tipo_pago
            END IF
         ------------------------
         AFTER FIELD banco
         ------------------------
            IF FGL_LASTKEY()= FGL_KEYVAL("LEFT") THEN
               NEXT FIELD PREVIOUS
            END IF

            IF gar_beneficiario[arr_c].banco IS NOT NULL  THEN
               --Checa si es un banco valido
               SELECT COUNT(*)
               INTO  l_count
               FROM  tab_banco
               WHERE banco = gar_beneficiario[arr_c].banco

               IF l_count = 0  THEN
                  ERROR  "        NO  EXISTE  CLAVE  DE  BANCO    "
                  NEXT  FIELD  banco
               END IF
            ELSE
               CALL   trae_banco()
               DISPLAY  gar_beneficiario[arr_c].banco  TO   sa_benef[arr_S].banco
              NEXT    FIELD  ciudad_cod
            END IF
         ------------------------
         AFTER FIELD ciudad_cod
         ------------------------
            IF FGL_LASTKEY()= FGL_KEYVAL("LEFT") THEN
               NEXT FIELD PREVIOUS
            END IF

            IF gar_beneficiario[arr_c].ciudad_cod IS NULL THEN
               LET pos_xxx = 0
               INITIALIZE x_x, x_buscar TO NULL

               OPEN WINDOW glob_ret8 AT 08,07 WITH FORM "GLOB_RET8" ATTRIBUTE(BORDER)
               DISPLAY "                      ESTADO / CIUDADES                                   " AT 1,1 ATTRIBUTE(REVERSE)

               LET x_x = " SELECT A.estad_cod,B.estad_desc,A.ciudad_cod,",
                     " A.ciudad_desc FROM tab_ciudad A,tab_estado B ",
                         " WHERE  A.estad_cod = B.estad_cod ",
                         " ORDER BY 1 "

               PREPARE pre_181 FROM x_x
               DECLARE cur_181 CURSOR FOR pre_181

               LET pos_xxx = 1
               FOREACH cur_181 INTO gar_estado[pos_xxx].*
                  LET pos_xxx = pos_xxx + 1
                  IF pos_xxx >= 1000 THEN
                     ERROR "    FUE SOBREPASADA LA CAPACIDAD MAXIMA DEL ARREGLO" ATTRIBUTE(NORMAL)
                     EXIT FOREACH
                  END IF
               END FOREACH
               LET pos_xxx = pos_xxx-1

               IF pos_xxx  < 1 THEN
                  ERROR "    REGISTRO INEXISTENTE" ATTRIBUTE(NORMAL)
                  SLEEP 2
                  ERROR ""
                  CLOSE WINDOW glob_ret8
                  NEXT FIELD  ciudad_cod
               ELSE
                  CALL SET_COUNT(pos_xxx)

                  DISPLAY ARRAY gar_estado TO scr_1.*
                           ON KEY ( INTERRUPT )
                              LET pos_xxx = 0
                              EXIT DISPLAY
                           ON KEY ( CONTROL-M )
                              LET pos_xxx = ARR_CURR()
                              LET gar_beneficiario[arr_c].dom_estado_cod = gar_estado[pos_xxx].estad_cod
                              LET gar_beneficiario[arr_c].ciudad_cod     = gar_estado[pos_xxx].ciudad_cod
                              --
                              LET gar_beneficiario[arr_c].estad_cod      = gar_estado[pos_xxx].estad_cod
                              --
                              EXIT DISPLAY
                  END DISPLAY
              END IF
              CLOSE WINDOW glob_ret8
              DISPLAY BY NAME gar_beneficiario[arr_c].ciudad_cod
            ELSE
               --Si no es nulo valida que sea una ciudad valida
               SELECT  estad_cod
                 INTO  gar_beneficiario[arr_c].dom_estado_cod   --IJR
                 FROM  tab_ciudad
                WHERE  ciudad_cod  = gar_beneficiario[arr_c].ciudad_cod

               IF STATUS = NOTFOUND THEN
                  ERROR "    CODIGO INEXISTENTE"  ATTRIBUTE(NORMAL)
                  NEXT FIELD ciudad_cod
               END IF
               --
               SELECT estad_cod INTO gar_beneficiario[arr_c].estad_cod
               FROM   tab_ciudad
               WHERE  ciudad_cod = gar_beneficiario[arr_c].ciudad_cod
               DISPLAY BY NAME gar_beneficiario[arr_c].ciudad_cod
               DISPLAY BY NAME gar_beneficiario[arr_c].estad_cod
               --
            END IF

         ------------------------
         AFTER FIELD cod_sucursal
         ------------------------
            IF FGL_LASTKEY()= FGL_KEYVAL("LEFT") THEN
               NEXT FIELD PREVIOUS
            END IF

            IF gar_beneficiario[arr_c].cod_sucursal IS NULL  THEN
               ERROR "     EL  NUMERO  DE  SUCURSAL  NO  PUEDE  SER  NULO     "
               NEXT FIELD cod_sucursal
            END IF

         ------------------------
         BEFORE FIELD num_cuenta
         ------------------------

       IF gar_beneficiario[arr_c].tipo_pago = 2 THEN
               LET gar_beneficiario[arr_c].num_cuenta = NULL
               DISPLAY gar_beneficiario[arr_c].num_cuenta TO sa_benef[arr_s].num_cuenta
          NEXT FIELD dom_calle
       END IF

         ------------------------
         AFTER FIELD num_cuenta
         ------------------------

            IF FGL_LASTKEY()= FGL_KEYVAL("LEFT") THEN
               NEXT FIELD PREVIOUS
            END IF

            IF gar_beneficiario[arr_c].tipo_pago = 1 AND gar_beneficiario[arr_c].num_cuenta IS NULL THEN
               ERROR "DEBE PROPORCIONAR EL NUMERO DE CUENTA"   --IJR
               NEXT FIELD num_cuenta
            END IF

         ------------------------
         AFTER FIELD dom_calle
         ------------------------
            IF FGL_LASTKEY() = FGL_KEYVAL("LEFT") THEN
               IF gar_beneficiario[arr_c].tipo_pago  = 2 THEN
                  NEXT FIELD cod_sucursal
               ELSE
                  NEXT FIELD PREVIOUS
               END IF
            END IF

            IF gar_beneficiario[arr_c].dom_calle IS NULL  THEN
               ERROR "  LA  CALLE  NO  PUEDE  SER  NULA  "
               NEXT FIELD dom_calle
            END IF

         ------------------------
         AFTER FIELD dom_codpos
         ------------------------
            IF FGL_LASTKEY()= FGL_KEYVAL("LEFT") THEN
               NEXT FIELD PREVIOUS
            END IF

            IF gar_beneficiario[arr_c].dom_codpos IS NULL THEN
               CALL Despliega_codigo_postal_1(gar_beneficiario[arr_c].dom_estado_cod )
                  RETURNING gar_beneficiario[arr_c].dom_codpos,    -- codigo_postal
                            gar_beneficiario[arr_c].dom_colonia,   -- descripcion colonia
                            gar_beneficiario[arr_c].dom_delega,    -- codigo_delegacion
                            gar_beneficiario[arr_c].nom_delega,    -- descripcion delegacion
                            gar_beneficiario[arr_c].dom_ciudad_cod,  -- codigo_ciudad
                            gar_beneficiario[arr_c].nom_ciudad_cod,  -- descripcion ciudad
                            gar_beneficiario[arr_c].dom_estado_cod,  -- codigo_estado
                            gar_beneficiario[arr_c].nom_estado_cod   -- descripcion estado

            ELSE
               --Valida que sea un codigo valido
               IF LENGTH(gar_beneficiario[arr_c].dom_codpos) <> 5 THEN
                  ERROR "Ingrese cinco digitos para indicar Codigo Postal"
                  NEXT FIELD dom_codpos
               END IF

               --Verifica que exista el codigo capturado en catalogo
               SELECT "X"
                 FROM tab_codpos
                WHERE cpos_cod = gar_beneficiario[arr_c].dom_codpos

               IF STATUS = NOTFOUND THEN
                  ERROR "Codigo Postal no existe en catalogo"
                  NEXT FIELD dom_codpos
               ELSE
                  CALL Despliega_colonias(gar_beneficiario[arr_c].dom_codpos)
                  RETURNING gar_beneficiario[arr_c].dom_colonia,      -- descripcion colonia
                            gar_beneficiario[arr_c].dom_delega,       -- codigo_delegacion
                            gar_beneficiario[arr_c].nom_delega,       -- descripcion delegacion
                            gar_beneficiario[arr_c].dom_ciudad_cod,   -- codigo_ciudad
                            gar_beneficiario[arr_c].nom_ciudad_cod,   -- descripcion ciudad
                            gar_beneficiario[arr_c].dom_estado_cod,   -- codigo_estado
                            gar_beneficiario[arr_c].nom_estado_cod     -- descripcion estado
               END IF
            END IF

            --Despliega los Datos recuperados
            DISPLAY gar_beneficiario[arr_c].dom_colonia     TO sa_benef[arr_s].dom_colonia
            DISPLAY gar_beneficiario[arr_c].dom_delega      TO sa_benef[arr_s].dom_delega
            DISPLAY gar_beneficiario[arr_c].dom_ciudad_cod  TO sa_benef[arr_s].dom_ciudad_cod
            DISPLAY gar_beneficiario[arr_c].dom_estado_cod  TO sa_benef[arr_s].dom_estado_cod
            DISPLAY gar_beneficiario[arr_c].nom_delega      TO sa_benef[arr_s].nom_delega
            DISPLAY gar_beneficiario[arr_c].nom_ciudad_cod  TO sa_benef[arr_s].nom_ciudad_cod
            DISPLAY gar_beneficiario[arr_c].nom_estado_cod  TO sa_benef[arr_s].nom_estado_cod

            NEXT  FIELD  dom_telefono

         -----------
         AFTER ROW     --IJR
         -----------
            -- Por si el usuario quiere cancelar la captura
            IF FGL_LASTKEY()= FGL_KEYVAL("INTERRUPT") OR
               FGL_LASTKEY()= FGL_KEYVAL("CONTROL-C") THEN
               LET INT_FLAG = TRUE
               EXIT INPUT
            END IF

            -- No podra cambiar de registro SI los siguientes campos son nulos
            IF gar_beneficiario[arr_c].paterno    IS NULL OR
               gar_beneficiario[arr_c].nombres    IS NULL OR
               gar_beneficiario[arr_c].paren_cod  IS NULL OR
               gar_beneficiario[arr_c].tipo_pago  IS NULL OR
--               gar_beneficiario[arr_c].banco      IS NULL OR
               gar_beneficiario[arr_c].dom_codpos IS NULL THEN
               ERROR "NO PUEDE CAMBIAR DE REGISTRO HABIENDO CAMPOS VACIOS"
               NEXT FIELD paterno
            END IF

            --Cuenta y revisa los porcentajes
            LET l_porcentaje  = 0
            LET cont_auxiliar = 0   -- Cuantos registros cargados

            FOR i = 1 TO 5
               IF gar_beneficiario[i].paterno    IS NOT NULL AND
                  gar_beneficiario[i].nombres    IS NOT NULL AND
                  gar_beneficiario[i].paren_cod  IS NOT NULL AND
                  gar_beneficiario[i].tipo_pago  IS NOT NULL AND
--                  gar_beneficiario[i].banco      IS NOT NULL AND
                  gar_beneficiario[i].dom_codpos IS NOT NULL THEN
                  LET l_porcentaje  = l_porcentaje + gar_beneficiario[i].porcentaje
                  LET cont_auxiliar = cont_auxiliar + 1
               END IF
            END FOR

            -- si ya se llego al 100 ya no permite pasar al siguiente registro
            IF l_porcentaje = 100 AND (arr_c = tot_arr ) AND FGL_LASTKEY()= FGL_KEYVAL("DOWN")
                THEN
               LET l_mensaje = "LOS PORCENTAJES SE ENCUENTRAN AL ",l_porcentaje USING "<<&","%"
               ERROR l_mensaje
               NEXT FIELD paterno
            END IF

         ------------------------
         ON KEY(ESC)
         ------------------------
             -- Valida los campos que no pueden ser nulos
               IF gar_beneficiario[arr_c].paterno IS NULL  THEN
                  ERROR "     EL  NOMBRE  NO  PUEDE  SER  NULO            "
                  NEXT FIELD paterno
               END IF

               IF gar_beneficiario[arr_c].nombres IS NULL  THEN
                  ERROR "     EL  NOMBRE  NO  PUEDE  SER  NULO     "
                  NEXT FIELD nombres
               END IF

               IF gar_beneficiario[arr_c].paren_cod IS NULL  THEN
                  ERROR "     EL  PARENTESCO NO  PUEDE  SER  NULO     "
                  NEXT FIELD paren_cod
               END IF

               IF gar_beneficiario[arr_c].porcentaje IS NULL  THEN
                  ERROR "       EL  PORCENTAJE  DEBE  SER  MAYOR  A  CERO      "
                  NEXT FIELD porcentaje
               END IF

               IF gar_beneficiario[arr_c].tipo_pago IS NULL  THEN
                  ERROR "    EL  TIPO  DE  PAGO  NO  PUEDE  SER  NULO    "
                  NEXT FIELD tipo_pago
               END IF

               IF gar_beneficiario[arr_c].banco IS NULL  THEN
                  NEXT FIELD banco
                  ERROR  "      EL  BANCO  NO  PUEDE SER  NULO    "
               END IF

               IF gar_beneficiario[arr_c].ciudad_cod IS NULL  THEN
                  ERROR "    LA  CLAVE  DE  CIUDAD  NO  PUEDE  SER  NULA  "
                  NEXT FIELD ciudad_cod
               END IF

               IF gar_beneficiario[arr_c].dom_calle IS NULL  THEN
                  ERROR "   EL  NOMBRE  DE  LA  CALLE  NO  PUEDE  SER  NULO  "
                  NEXT FIELD dom_calle
               END IF

               IF gar_beneficiario[arr_c].dom_codpos IS NULL THEN
                  ERROR "    EL  CODIGO  NO  PUEDE  SER  NULO  "
                  NEXT FIELD dom_codpos
               END IF

               LET l_porcentaje = 0
               FOR i = 1 TO 5
                  IF gar_beneficiario[i].paterno    IS NOT NULL AND
                     gar_beneficiario[i].nombres    IS NOT NULL AND
                     gar_beneficiario[i].tipo_pago  IS NOT NULL AND
                     gar_beneficiario[i].paren_cod  IS NOT NULL AND
                     gar_beneficiario[i].dom_codpos IS NOT NULL  THEN
                     LET l_porcentaje = l_porcentaje + gar_beneficiario[i].porcentaje
                  END IF
               END FOR

               IF l_porcentaje = 100  THEN
                  EXIT INPUT
               ELSE
                  ERROR "    EL  PORCENTAJE  DEBE  SER  EL  100%  "
                  NEXT FIELD porcentaje
               END IF
         ------------------------
         ON KEY (control-c)
         ------------------------
            LET int_flag = TRUE
            EXIT INPUT

      END INPUT

      -- VALIDACION DEL EXITO DE LA CAPTURA
      IF INT_FLAG THEN
         ERROR " CAPTURA CANCELADA POR EL USUARIO "
      ELSE

         WHILE  TRUE
            PROMPT "   TECLEE <S> ACTUALIZA  BENEFICIARIO o <N> PARA CANCELAR :"
            FOR  enter

            IF enter MATCHES "[sSnN]" THEN
               IF enter MATCHES "[sS]" THEN
                  FOR arr_c = 1 TO 5
                     IF gar_beneficiario[arr_c].porcentaje > 0  THEN
                        -- BORRA EL BENEFICIARIO ANTERIOR Y CAPTURA LOS NUEVOS
                        CALL insert_ret_beneficiario_1()
                     END IF
                  END FOR
                  EXIT WHILE
               ELSE
                  EXIT WHILE
               END IF
            END IF
         END WHILE
         --CALL    inicializa()
      END IF
   END IF

END FUNCTION


######################## -- agrego miguel funcion -- ########################
FUNCTION insert_ret_beneficiario_1()

    DEFINE lr_beneficiario RECORD LIKE ret_beneficiario.*

    -- -----------------------------------------------------------------------------

    INITIALIZE lr_beneficiario.* TO NULL

    LET lr_beneficiario.nss                 = g_nss
    LET lr_beneficiario.consecutivo         = g_consecutivo
    LET lr_beneficiario.consec_benef        = gar_beneficiario[arr_c].consec_benef
    LET lr_beneficiario.tipo_pago           = gar_beneficiario[arr_c].tipo_pago
    LET lr_beneficiario.paren_cod           = gar_beneficiario[arr_c].paren_cod
    LET lr_beneficiario.paterno             = gar_beneficiario[arr_c].paterno
    LET lr_beneficiario.materno             = gar_beneficiario[arr_c].materno
    LET lr_beneficiario.nombres             = gar_beneficiario[arr_c].nombres
    LET lr_beneficiario.porcentaje          = gar_beneficiario[arr_c].porcentaje
    LET lr_beneficiario.tienda_cod          = 0
    LET lr_beneficiario.banco               = gar_beneficiario[arr_c].banco
    LET lr_beneficiario.cod_sucursal        = gar_beneficiario[arr_c].cod_sucursal
    LET lr_beneficiario.ciudad_cod          = gar_beneficiario[arr_c].ciudad_cod
    LET lr_beneficiario.estad_cod           = gar_beneficiario[arr_c].estad_cod
    LET lr_beneficiario.num_cuenta          = gar_beneficiario[arr_c].num_cuenta
    LET lr_beneficiario.dom_calle           = gar_beneficiario[arr_c].dom_calle
    LET lr_beneficiario.dom_numero_ext      = gar_beneficiario[arr_c].dom_numero_ext
    LET lr_beneficiario.dom_numero_int      = gar_beneficiario[arr_c].dom_numero_int
    LET lr_beneficiario.dom_codpos          = gar_beneficiario[arr_c].dom_codpos
    LET lr_beneficiario.dom_colonia         = gar_beneficiario[arr_c].dom_colonia
    LET lr_beneficiario.dom_delega          = gar_beneficiario[arr_c].dom_delega
    LET lr_beneficiario.dom_ciudad_cod      = gar_beneficiario[arr_c].dom_ciudad_cod
    LET lr_beneficiario.dom_estado_cod      = gar_beneficiario[arr_c].dom_estado_cod
    LET lr_beneficiario.dom_telefono        = gar_beneficiario[arr_c].dom_telefono
    LET lr_beneficiario.fecha_captura       = HOY
    LET lr_beneficiario.usuario_captura     = g_user

    SELECT *
    FROM   ret_beneficiario
    WHERE 1=0
    INTO TEMP tmp_ret_beneficiario

    INSERT INTO tmp_ret_beneficiario
    VALUES(lr_beneficiario.*)

    WHENEVER ERROR CONTINUE
        DELETE
        FROM   ret_beneficiario
        WHERE  nss        = g_nss
        AND  consecutivo  = g_consecutivo
        AND  consec_benef = gar_beneficiario[arr_c].consec_benef

        IF SQLCA.SQLCODE < 0  THEN
            PROMPT "  ERROR  de UPDATE ret_beneficiario AVISE A SISTEMAS "
                FOR  enter
        END IF

        INSERT INTO ret_beneficiario
        SELECT *
        FROM  tmp_ret_beneficiario

        IF SQLCA.SQLCODE < 0  THEN
            PROMPT  "  ERROR  de UPDATE ret_beneficiario AVISE A SISTEMAS "
                FOR  enter
        END IF

    WHENEVER ERROR STOP

    DROP TABLE tmp_ret_beneficiario

END FUNCTION
{===========================================================================}
{ Objetivo : Recuperar los beneficiarios para el nss y consecutivo recibido }
{          : como parametros globales                                       }
{ Regresa  : El numero de Registros cargados en el arreglo                  }
{ Modifico : IJR 21/10/04                                                   }
{===========================================================================}
FUNCTION lee_ret_beneficiario()
   DEFINE   l_count       SMALLINT
   DEFINE   i             SMALLINT

   -- Verifica si Tiene Beneficiarios el NSS
   SELECT COUNT(*)
   INTO   l_count
   FROM   ret_beneficiario
   WHERE  nss         =  g_nss
   AND    consecutivo =  g_consecutivo

   IF l_count = 0  THEN
      ERROR "     NO   HAY  BENEFICIARIOS  PARA  ESTE  AFILIADO    "
   ELSE
      --Selecciona los Datos de los Beneficiarios
      DECLARE  c_cons CURSOR FOR
      SELECT ben.consec_benef  ,
             ben.paterno       ,
             ben.materno       ,
             ben.nombres       ,
             ben.paren_cod     ,
             par.paren_desc    ,
             ben.porcentaje    ,
             ben.tipo_pago     ,
             ben.banco         ,
             ben.ciudad_cod    ,
             --
             ben.estad_cod     ,
             --
             ben.cod_sucursal  ,
             ben.num_cuenta    ,
             ben.monto_en_pesos,
             ben.fecha_pago    ,
             ben.dom_calle     ,
             ben.dom_numero_ext,
             ben.dom_numero_int,
             ben.dom_codpos    ,
             ben.dom_colonia   ,
             ben.dom_delega    ,
             del.deleg_desc    ,
             ben.dom_ciudad_cod,
             ciu.ciudad_desc,
             ben.dom_estado_cod,
             edo.estad_desc,
             ben.dom_telefono
      FROM  ret_beneficiario       ben ,
        OUTER tab_parentesco   par ,
            OUTER tab_delegacion   del ,
            OUTER tab_ciudad       ciu ,
            OUTER tab_estado       edo
      WHERE ben.nss            = g_nss
      AND   ben.consecutivo    = g_consecutivo
      AND   ben.paren_cod      = par.paren_cod
      AND   ben.dom_estado_cod = edo.estad_cod
      AND   ben.dom_estado_cod = del.estad_cod
      AND   ben.dom_delega     = del.deleg_cod
      AND   ben.dom_estado_cod = ciu.estad_cod
      AND   ben.dom_ciudad_cod = ciu.ciudad_cod

      LET l_count = 1    -- Para la Posicion dentro del Arreglo

      FOREACH c_cons INTO gar_beneficiario[l_count].*
         LET l_count = l_count + 1
      END FOREACH

      LET l_count = l_count - 1
   END IF

   RETURN l_count  -- Regresa el Numero registros cargados en el arreglo

END FUNCTION
{===========================================================================}
{ Objetivo : Consultar beneficiarios                                        }
{ Regresa  :                                                                }
{ Modifico : IJR 21/10/04                                                   }
{===========================================================================}
FUNCTION consulta()
   DEFINE  i               SMALLINT
   DEFINE  l_hay_bajas     SMALLINT
   DEFINE  l_numregs       SMALLINT -- Numero de registros cargados de beneficiarios en arreglo

   IF NOT g_elimina THEN
      DISPLAY "   [ Esc ]       Salir modo Consulta        " AT 2,1
   ELSE
      DISPLAY "  [ Esc ] Salir  [Control-E] Elimina Registro [Control-T]  Elimina Todos  " AT 2,1
   END IF

   LET l_hay_bajas = 0

   DISPLAY  g_nss      TO nss
   DISPLAY  g_nombre_c TO nombre

   CALL lee_ret_beneficiario() RETURNING l_numregs -- Recupera los beneficiarios

   IF l_numregs > 0 THEN
      CALL SET_COUNT(l_numregs)  -- Inicializa el num de elementos a desplegar

      -- DESPLEGADO DEL ARREGLO DE BENEFICIARIOS

      DISPLAY ARRAY gar_beneficiario TO sa_benef.*  -- Despliega Beneficiarios

         ON KEY(CONTROL-T)
            -- ELIMINAR TODOS
            IF g_elimina  THEN
               FOR i = 1 TO 5
                  IF gar_beneficiario[i].paterno IS NOT NULL  THEN
                   INITIALIZE gar_beneficiario[i].* TO NULL
                   LET gar_beneficiario[i].paterno = "BAJA"
                  END IF
               END FOR
               CLEAR FORM
               DISPLAY g_nss TO nss
               DISPLAY g_nombre_c TO nombre
            END IF

         ON KEY(CONTROL-E)
            --ELIMINA SOLO 1
            IF g_elimina  THEN
               LET arr_c = arr_curr()
               INITIALIZE gar_beneficiario[arr_c].* TO NULL
               LET gar_beneficiario[arr_c].consec_benef = arr_c
               LET gar_beneficiario[arr_c].paterno      = "BAJA"
               CLEAR FORM
               DISPLAY g_nss TO nss
               DISPLAY g_nombre_c TO nombre
            END IF

         ON KEY(ESC)
            IF g_elimina  THEN
               FOR i = 1 TO 5
                  IF gar_beneficiario[i].paterno = "BAJA"  THEN
                     LET l_hay_bajas = 1
                  END IF
               END FOR

               WHILE TRUE
                  IF NOT l_hay_bajas  THEN
                     EXIT WHILE
                  END IF
                  PROMPT "  TECLEE <S> PARA BORRAR BENEFICIARIO o <N> PARA CANCELAR :" FOR  enter
                  IF enter MATCHES "[sSnN]"  THEN
                     IF enter MATCHES "[sS]"  THEN
                        FOR i=1 TO 5
                         IF gar_beneficiario[i].paterno = "BAJA"  THEN
                                DELETE FROM ret_beneficiario
                                WHERE  nss = g_nss
                                AND  consecutivo  = g_consecutivo
                                AND  consec_benef = i
                             END IF

                             IF SQLCA.SQLCODE < 0  THEN
                                ERROR "    ERROR AL BORRAR REGISTRO  "
                             END IF
                        END FOR
                        EXIT WHILE
                     ELSE
                        EXIT WHILE
                     END IF
                  END IF
               END WHILE
            END IF
            CALL inicializa()
            EXIT DISPLAY
      END  DISPLAY
   END IF

END FUNCTION
{===========================================================================}
{ Objetivo : Insertar en la Tabla los beneficiarios                         }
{ Regresa  :                                                                }
{ Modifico :                                                                }
{===========================================================================}
FUNCTION insert_ret_beneficiario()

    DEFINE lr_beneficiario RECORD LIKE ret_beneficiario.*

    -- -----------------------------------------------------------------------------

    INITIALIZE lr_beneficiario.* TO NULL



    LET lr_beneficiario.nss                 = g_nss
    LET lr_beneficiario.consecutivo         = g_consecutivo
    LET lr_beneficiario.consec_benef        = gar_beneficiario[arr_c].consec_benef
    LET lr_beneficiario.tipo_pago           = gar_beneficiario[arr_c].tipo_pago
    LET lr_beneficiario.paren_cod           = gar_beneficiario[arr_c].paren_cod
    LET lr_beneficiario.paterno             = gar_beneficiario[arr_c].paterno
    LET lr_beneficiario.materno             = gar_beneficiario[arr_c].materno
    LET lr_beneficiario.nombres             = gar_beneficiario[arr_c].nombres
    LET lr_beneficiario.porcentaje          = gar_beneficiario[arr_c].porcentaje
    LET lr_beneficiario.tienda_cod          = 0
    LET lr_beneficiario.banco               = gar_beneficiario[arr_c].banco
    LET lr_beneficiario.cod_sucursal        = gar_beneficiario[arr_c].cod_sucursal
    LET lr_beneficiario.ciudad_cod          = gar_beneficiario[arr_c].ciudad_cod
    LET lr_beneficiario.estad_cod           = gar_beneficiario[arr_c].estad_cod
    LET lr_beneficiario.num_cuenta          = gar_beneficiario[arr_c].num_cuenta
    LET lr_beneficiario.dom_calle           = gar_beneficiario[arr_c].dom_calle
    LET lr_beneficiario.dom_numero_ext      = gar_beneficiario[arr_c].dom_numero_ext
    LET lr_beneficiario.dom_numero_int      = gar_beneficiario[arr_c].dom_numero_int
    LET lr_beneficiario.dom_codpos          = gar_beneficiario[arr_c].dom_codpos
    LET lr_beneficiario.dom_colonia         = gar_beneficiario[arr_c].dom_colonia
    LET lr_beneficiario.dom_delega          = gar_beneficiario[arr_c].dom_delega
    LET lr_beneficiario.dom_ciudad_cod      = gar_beneficiario[arr_c].dom_ciudad_cod
    LET lr_beneficiario.dom_estado_cod      = gar_beneficiario[arr_c].dom_estado_cod
    LET lr_beneficiario.dom_telefono        = gar_beneficiario[arr_c].dom_telefono
    LET lr_beneficiario.fecha_captura       = HOY
    LET lr_beneficiario.usuario_captura     = g_user

    WHENEVER ERROR CONTINUE   -- miguel bitacora

        INSERT INTO ret_beneficiario
        VALUES(lr_beneficiario.*)

        IF (SQLCA.SQLCODE < 0) THEN
        --Si se presenta un error se registra en Bitacora
            LET x_error = "INSERT ret_beneficiario:",
                          " nss ",g_nss,
                          " consecutivo ",g_consecutivo,
                          ERR_GET(sqlca.sqlcode)

            CALL ERRORLOG(x_error CLIPPED)

            PROMPT "  ERROR AL INSERTAR REGISTROS AVISE A SISTEMAS " FOR  enter
            EXIT  PROGRAM
        END IF

    WHENEVER ERROR STOP

END FUNCTION


FUNCTION update_ret_beneficiario()
    UPDATE  ret_beneficiario
    SET   tipo_pago        =  gar_beneficiario[arr_c].tipo_pago   ,
          paren_cod        =  gar_beneficiario[arr_c].paren_cod   ,
          paterno          =  gar_beneficiario[arr_c].paterno     ,
          materno          =  gar_beneficiario[arr_c].materno     ,
          nombres          =  gar_beneficiario[arr_c].nombres     ,
          porcentaje       =  gar_beneficiario[arr_c].porcentaje  ,
          banco            =  gar_beneficiario[arr_c].banco       ,
          ciudad_cod       =  gar_beneficiario[arr_c].ciudad_cod  ,
          --
          estad_cod        =  gar_beneficiario[arr_c].estad_cod   ,
          --
          num_cuenta       =  gar_beneficiario[arr_c].num_cuenta  ,
          dom_calle        =  gar_beneficiario[arr_c].dom_calle   ,
          dom_numero_ext   =  gar_beneficiario[arr_c].dom_numero_ext,
          dom_numero_int   =  gar_beneficiario[arr_c].dom_numero_int,
          dom_codpos       =  gar_beneficiario[arr_c].dom_codpos    ,
          dom_colonia      =  gar_beneficiario[arr_c].dom_colonia   ,
          dom_delega       =  gar_beneficiario[arr_c].dom_delega    ,
          dom_ciudad_cod   =  gar_beneficiario[arr_c].dom_ciudad_cod,
          dom_estado_cod   =  gar_beneficiario[arr_c].dom_estado_cod,
          dom_telefono     =  gar_beneficiario[arr_c].dom_telefono  ,
          fecha_modifica   =  HOY    ,      #fecha_modifica
          usuario_modifica =  g_user        #usuario_modifica
   WHERE  nss        = g_nss
   AND  consecutivo  = g_consecutivo
   AND  consec_benef = gar_beneficiario[arr_c].consec_benef

---omar
   IF SQLCA.SQLCODE < 0 THEN
      LET x_error = "UPDATE ret_beneficiario:",
            "nss ",g_nss,
            "consecutivo ",g_consecutivo,
            err_get(SQLCA.SQLCODE)

      CALL errorlog(x_error CLIPPED)

      PROMPT  " ERROR DE UPDATE ret_beneficiario AVISE A SISTEMAS "
      FOR enter
      EXIT PROGRAM
   END IF
END FUNCTION


FUNCTION trae_banco()
    DEFINE   pa_elem             SMALLINT

    OPEN WINDOW win_st  AT 14,40  WITH FORM "RETM8103" ATTRIBUTE(BORDER)

    DISPLAY  "        CATALOGO  DE  BANCOS      "  AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY  "<ENTER> SELECCIONAR    <ESC> SALIR "  AT 2,1 ATTRIBUTE(REVERSE)
    DISPLAY  "  USE <FLECHAS> PARA DESPLAZARSE   "  AT 3,1 ATTRIBUTE(BOLD)

    CALL SET_COUNT(i_banco)

    DISPLAY ARRAY gar_banco TO sa_bancos.*

       ON KEY (ESC)
          LET pa_elem = arr_curr()
          EXIT DISPLAY

       ON KEY (CONTROL-M)
          LET pa_elem = arr_curr()
          LET gar_beneficiario[arr_c].banco = gar_banco[pa_elem].banco
          EXIT DISPLAY

    END DISPLAY
    CLOSE WINDOW win_st
END FUNCTION

FUNCTION cuenta_beneficiarios()

   DEFINE v_contador   SMALLINT

   --Verifica que el NSS quede con beneficiarios
   SELECT COUNT(*)
   INTO   v_contador
   FROM   ret_beneficiario
   WHERE  nss          = g_nss
   AND    consecutivo  = g_consecutivo

   RETURN v_contador

END FUNCTION

FUNCTION suma_porcentajes()
   DEFINE v_porcentaje  SMALLINT

   -- Suma ls porcentajes para el afiliado
   SELECT SUM( NVL(porcentaje,0) )
   INTO   v_porcentaje
   FROM   ret_beneficiario
   WHERE  nss          = g_nss
   AND    consecutivo  = g_consecutivo

   RETURN v_porcentaje

END FUNCTION

FUNCTION Despliega_codigo_postal_1(vestad)
   DEFINE aux_vas SMALLINT

   DEFINE l_reg ARRAY[1000] OF RECORD
          cod           CHAR(05),
          descrip       CHAR(25),
          descripcion   CHAR(25)
   END RECORD,
          reg       RECORD
          cod_colon SMALLINT,
          colonia   CHAR(40),
          deleg     SMALLINT,
          ciudad    SMALLINT,
          estado    SMALLINT
   END RECORD,
          desdeleg  CHAR(40),
          desciuda  CHAR(40),
          desestad  CHAR(40),
          codigo_estado SMALLINT,
          vestad    SMALLINT

   DEFINE x_x      CHAR(300),
          x_buscar CHAR(30)

   DEFINE pos      SMALLINT,
          codigo   INTEGER

   OPEN WINDOW vent_xx AT 05,07 WITH FORM "PRO_GLOBP4" ATTRIBUTE(BORDER)

   DISPLAY "                        CODIGOS POSTALES                          " AT 2,1 ATTRIBUTE(REVERSE)

{
   INPUT BY NAME x_buscar
      AFTER FIELD x_buscar
         IF x_buscar IS NULL THEN
            ERROR "Descripcion a Buscar NO puede ser nulo"
            NEXT FIELD x_buscar
         ELSE
            EXIT INPUT
         END IF
   END INPUT

   WHENEVER ERROR CONTINUE

   PROMPT 'DIGITA CODIGO ESTADO DE ESTA COLONIA ...' for vestad attribute(reverse)

   WHENEVER ERROR STOP

   IF vestad IS NULL THEN
       LET vestad=0
   END IF
}
   ERROR "BUSCANDO INFORMACION ..."

   WHILE TRUE


   LET x_x = " SELECT c.cpos_cod,a.colon_desc,b.deleg_desc FROM ",
             " tab_codpos c,tab_colonia a,tab_delegacion b ",
             " WHERE c.cpos_cod=a.cpos_cod and c.deleg_cod=b.deleg_cod ",
       --      " and a.colon_desc MATCHES ",'"*',x_buscar CLIPPED,'*"',
             " and a.colon_desc MATCHES '*' ",
             " and c.estad_cod=",vestad CLIPPED,
             " ORDER BY 2 " CLIPPED

   PREPARE curg21_x FROM x_x

   DECLARE cur_g21_x CURSOR FOR curg21_x
   LET pos = 1
   FOREACH cur_g21_x INTO l_reg[pos].*
      IF status=100 THEN
         EXIT FOREACH
      END IF

      LET pos = pos + 1

      IF pos >= 1000 THEN
         ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
         EXIT FOREACH
      END IF
   END FOREACH
   ERROR ""

   IF (pos-1) < 1 THEN
      ERROR "ARCHIVO CODIGOS POSTALES... VACIO"
   END IF

   CALL SET_COUNT(pos-1)

   DISPLAY ARRAY l_reg TO scr_1.*

       ON KEY ( INTERRUPT )
          LET pos = 0
          LET pos = ARR_CURR()
          LET l_reg[pos].descripcion = NULL
          LET reg.deleg = NULL
          LET desdeleg  = NULL
          LET reg.ciudad= NULL
          LET desciuda  = NULL
          LET reg.estado= NULL
          LET desestad  = NULL
          EXIT DISPLAY

       ON KEY ( CONTROL-M )
          LET codigo = l_reg[pos].cod
          LET pos = ARR_CURR()
          SELECT deleg_cod,ciudad_cod,estad_cod
          INTO   reg.deleg,reg.ciudad,reg.estado
          FROM tab_codpos
          WHERE cpos_cod = l_reg[pos].cod

          SELECT deleg_desc
          INTO desdeleg
          FROM tab_delegacion
          WHERE deleg_cod = reg.deleg

          SELECT ciudad_desc INTO desciuda
          FROM tab_ciudad
          WHERE ciudad_cod = reg.ciudad

          SELECT estad_desc INTO desestad
          FROM tab_estado
          WHERE estad_cod = reg.estado

          EXIT DISPLAY
   END DISPLAY

   IF pos <> 0 THEN
      EXIT WHILE
   END IF
   END WHILE

   CLOSE WINDOW vent_xx
   RETURN l_reg[pos].cod,       -- codigo_colonia
          l_reg[pos].descrip,   -- descripcion colonia
          reg.deleg,            -- codigo_delegacion
          desdeleg,             -- descripcion delegacion
          reg.ciudad,           -- codigo_ciudad
          desciuda,             -- descripcion ciudad
          reg.estado,           -- codigo_estado
          desestad              -- descripcion estado
END FUNCTION
FUNCTION Despliega_colonias(xcpos_cod)
   DEFINE
      xcpos_cod         CHAR(05),
      aux_val       SMALLINT,
      x_x       CHAR(300),
      x_buscar      CHAR(30),
      pos       SMALLINT,
      reg record
         cod_colon smallint,
         colonia char(40),
         deleg   smallint,
         ciudad  smallint,
         estado  smallint
      end record,
      desdeleg char(40),
      desciuda char(40),
      desestad char(40),
      l_reg ARRAY[1000] OF RECORD
         cod            CHAR(05),
         codigo     INTEGER,
         descripcion    CHAR(40)
      END RECORD

ERROR "BUSCANDO INFORMACION ..."

      DECLARE cur_cp CURSOR FOR
      SELECT cpos_cod,colon_cod,colon_desc
      FROM safre_af:tab_colonia
      WHERE cpos_cod = xcpos_cod
#      ORDER BY 2

      LET pos = 1
      FOREACH cur_cp INTO l_reg[pos].*
         LET pos = pos + 1
      END FOREACH

ERROR ""
      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)
     OPEN WINDOW ventana_cp AT 6,08 WITH FORM "PRO_GLOBP3" ATTRIBUTE (BORDER)
         DISPLAY " (ENTER) Elegir                                   (Ctrl - C) Salir " AT 1,1 ATTRIBUTE(BOLD)
         DISPLAY "                      C  O  L  O  N  I  A  S                       " AT 2,1 ATTRIBUTE (REVERSE,BOLD)

         DISPLAY ARRAY l_reg to scr_1.*
            ON KEY (CONTROL-M)
               LET pos = ARR_CURR()

               SELECT deleg_cod,ciudad_cod,estad_cod
               INTO   reg.deleg,reg.ciudad,reg.estado
               FROM safre_af:tab_codpos
               WHERE cpos_cod = l_reg[pos].cod

               SELECT deleg_desc
               INTO desdeleg
               FROM safre_af:tab_delegacion
               WHERE deleg_cod = reg.deleg

               SELECT ciudad_desc INTO desciuda
               FROM safre_af:tab_ciudad WHERE
               ciudad_cod = reg.ciudad

               SELECT estad_desc INTO desestad
               FROM safre_af:tab_estado WHERE
               estad_cod = reg.estado

               EXIT DISPLAY
            ON KEY(INTERRUPT)
               LET pos = ARR_CURR()
               LET l_reg[pos].descripcion = NULL
               LET reg.deleg = NULL
               LET desdeleg  = NULL
               LET reg.ciudad= NULL
               LET desciuda  = NULL
               LET reg.estado= NULL
               LET desestad  = NULL
               EXIT DISPLAY
         END DISPLAY
         CLOSE WINDOW ventana_cp
      ELSE
         ERROR "ARCHIVO DE COLONIAS ..... VACIO"
      END IF
      RETURN
         l_reg[pos].descripcion,
         reg.deleg,
         desdeleg,
         reg.ciudad,
         desciuda,
         reg.estado,
         desestad

END FUNCTION

FUNCTION Despliegar_parentescos()
    DEFINE aux_val      SMALLINT
    DEFINE l_reg ARRAY[1000] OF RECORD
              codigo        INTEGER,
              descripcion   CHAR(50)
           END RECORD
    DEFINE x_x              char(100),
           x_buscar     char(30)
    DEFINE pos              SMALLINT

    OPEN WINDOW vent_1 AT 05,12 WITH FORM "PRO_GLOBP1" ATTRIBUTE(BORDER)
    DISPLAY "                 P A R E N T E S C O S                   " AT 2,1 ATTRIBUTE(REVERSE)

      LET x_x = " SELECT paren_cod,paren_desc FROM safre_af:tab_parentesco ",
                " WHERE paren_desc MATCHES '*' ",
                " ORDER BY 1 " CLIPPED
      PREPARE curg7 FROM x_x
      DECLARE cur_g7 CURSOR FOR curg7
      LET pos = 1
      FOREACH cur_g7 INTO l_reg[pos].*
            LET pos = pos + 1
            IF pos >= 1000 THEN
           ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
           EXIT FOREACH
        END IF
      END FOREACH

      IF (pos-1) < 1 THEN
         ERROR "ARCHIVO PARENTESCO..... VACIO"
      END IF
      CALL SET_COUNT(pos-1)

      DISPLAY ARRAY l_reg TO scr_1.*

         ON KEY ( INTERRUPT )
            LET pos = 0
            EXIT DISPLAY

         ON KEY ( CONTROL-M )
            LET pos = ARR_CURR()
            EXIT DISPLAY

      END DISPLAY

    CLOSE WINDOW vent_1
    RETURN l_reg[pos].codigo,l_reg[pos].descripcion
END FUNCTION

#---------------------------------------------------------------------------#
# f_determina_laudo : Determina si el registro capturado es por tramite     #
#                     judicial IMSS o ISSSTE                                #
#---------------------------------------------------------------------------#
FUNCTION f_determina_laudo(pr_solicitud)

    DEFINE pr_solicitud RECORD
        nss             LIKE ret_solicitud_tx.nss           ,
        consecutivo     LIKE ret_solicitud_tx.consecutivo
    END RECORD

    DEFINE lr_datos_sol RECORD
        tipo_pension    LIKE ret_solicitud_tx.tipo_pension  ,
        es_laudo        SMALLINT
    END RECORD

    DEFINE lc_tipo_retiro LIKE ret_solicitud_tx.tipo_retiro

    -- -----------------------------------------------------------------------------

    LET lr_datos_sol.tipo_pension   = "  "
    LET lr_datos_sol.es_laudo       = FALSE

    SELECT tipo_pension ,
           tipo_retiro
    INTO   lr_datos_sol.tipo_pension    ,
           lc_tipo_retiro
    FROM   ret_solicitud_tx
    WHERE  nss         = g_nss
    AND    consecutivo = g_consecutivo

    IF (STATUS <> NOTFOUND) THEN
        IF (lc_tipo_retiro = "G") THEN
            LET lr_datos_sol.es_laudo   = TRUE
        END IF
    ELSE
        SELECT tipo_pension ,
               tipo_retiro
        INTO   lr_datos_sol.tipo_pension    ,
               lc_tipo_retiro
        FROM   ret_sol_issste_tx
        WHERE  nss         = g_nss
        AND    consecutivo = g_consecutivo

        IF (STATUS <> NOTFOUND) THEN
            IF (lc_tipo_retiro = "D") THEN
                LET lr_datos_sol.es_laudo   = TRUE
            END IF
        END IF

    END IF

    RETURN lr_datos_sol.*

END FUNCTION

#---------------------------------------------------------------------------#
# f_obten_domicilio : Obtiene los datos registrados de domicilio para el    #
#                     nss actual                                            #
#---------------------------------------------------------------------------#
FUNCTION f_obten_domicilio(pc_nss)

    DEFINE pc_nss LIKE ret_beneficiario.nss

    DEFINE lr_afiliado RECORD
        num_folio       LIKE afi_mae_afiliado.n_folio           ,
        tipo_sol        LIKE afi_mae_afiliado.tipo_solicitud
    END RECORD

    DEFINE lr_nombre RECORD
        paterno         LIKE afi_mae_afiliado.paterno   ,
        materno         LIKE afi_mae_afiliado.materno   ,
        nombres         LIKE afi_mae_afiliado.nombres
    END RECORD

    DEFINE lr_domicilio RECORD
        ciudad              LIKE afi_domicilio.ciudad               ,
        dom_calle           LIKE afi_domicilio.calle                ,
        dom_numero_ext      LIKE afi_domicilio.numero               ,
        dom_numero_int      LIKE afi_domicilio.depto                ,
        dom_codpos          LIKE afi_domicilio.codpos               ,
        dom_colonia         LIKE afi_domicilio.colonia              ,
        dom_delega          LIKE ret_beneficiario.dom_delega        ,
        nom_delega          CHAR(30)                                ,
        nom_ciudad_cod      CHAR(30)                                ,
        dom_estado_cod      LIKE  ret_beneficiario.dom_estado_cod   ,
        nom_estado_cod      CHAR(30)
    END RECORD

    -- -----------------------------------------------------------------------------

    -- Obtiene los datos del afiliado
    SELECT paterno          ,
           materno          ,
           nombres          ,
           n_folio          ,
           tipo_solicitud
    INTO   lr_nombre.*      ,
           lr_afiliado.*
    FROM   afi_mae_afiliado
    WHERE  n_seguro = pc_nss

    SELECT a.ciudad,
           a.calle,
           a.numero,
           a.depto,
           a.codpos,
           a.colonia,
           a.delega,
           b.deleg_desc,
           c.ciudad_desc,
           a.estado,
           d.estad_desc
    INTO   lr_domicilio.*
    FROM   afi_domicilio a          ,
           OUTER tab_delegacion b   ,
           OUTER tab_ciudad c       ,
           OUTER tab_estado d
    WHERE  a.nss            = pc_nss
    AND    a.n_folio        = lr_afiliado.num_folio
    AND    a.tipo_solicitud = lr_afiliado.tipo_sol
    AND    a.marca_envio    = "X"
    AND    b.deleg_cod      = a.delega
    AND    c.ciudad_cod     = a.ciudad
    AND    d.estad_cod      = a.estado
    GROUP BY 1,2,3,4,5,6,7,8,9,10,11

    RETURN lr_nombre.*      ,
           lr_domicilio.*

END FUNCTION

