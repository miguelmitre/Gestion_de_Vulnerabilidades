########################################################################
#Proyecto       => Sistema de Afores.( MEXICO )                        #
#Propietario    => E.F.P.                                              #
#Programa       => AFIM031                                             #
#Descripcion    => CAPTURA DE SOLICITURES DE CAMBIO DE SIEFORE         #
#Sistema        => AFI                                                 #
#Fecha          => 25 DE OCTUBRE DEL 2004                              #
#Por            => FERNANDO HERRERA HERNANDEZ                          #
########################################################################
DATABASE safre_af
GLOBALS
   DEFINE
     nss                CHAR(11),
     curp               CHAR(18),
     rfc                CHAR(13),
     paterno            CHAR(40),
     materno            CHAR(40),
     nombres            CHAR(40),
     nombre_completo    CHAR(120),
     cve_lada           CHAR(3),
     telefono           CHAR(40),
     extension          INTEGER,
     ffirma_sol         DATE,
     fcaptura           DATE,
     edo_sol            SMALLINT,
     desc_edo_sol       CHAR(60),
     vusuario           CHAR(12),
     venter             CHAR(1),
     vhoy               DATE,
     rseg_modulo RECORD LIKE seg_modulo.*,
     rm_afiliado RECORD LIKE afi_mae_afiliado.*,
     proceso            SMALLINT,
     edo_cta            SMALLINT,
     f_marca            DATE,
     edo_act            SMALLINT,
     edo_desc           CHAR(50),
     l_act              CHAR(25),
     listado            CHAR(200),
     impresion          CHAR(200),
     vfcaptura date,
     vedo_sol           SMALLINT

     DEFINE
     tipo_solicitud    SMALLINT,
     folio_solicitud   INTEGER,
     folio_afiliacion  INTEGER,
     edad              SMALLINT,
     siefore_asig      SMALLINT,
     desc_siefore_asig CHAR(63),
     siefore_sol       SMALLINT,
     desc_siefore_sol  CHAR(63),
     r_existe          SMALLINT,
     r_edad            SMALLINT,
     r_criterio        SMALLINT,
     v_curp            CHAR(18),
     v_rfc             CHAR(13),
     v_fena            DATE,
     li_siefore_ori    SMALLINT

END GLOBALS
#######################################################################
MAIN

  DEFER INTERRUPT
  OPTIONS
     MESSAGE LINE LAST,
     PROMPT  LINE LAST

  CALL STARTLOG ("AFIM031.log")
  CALL inicio() #i
  CALL proceso_principal() #pp

END MAIN
#######################################################################
FUNCTION inicio()
#i--------------- Inicializa variables y crea tablas temporales

  DEFINE lc_query CHAR(400)

  LET vhoy = TODAY

  SELECT *, USER
  INTO   rseg_modulo.*, vusuario
  FROM   seg_modulo
  WHERE  modulo_cod = 'afi'

  LET lc_query = "EXECUTE FUNCTION fn_fnacimiento(?,?)"

  PREPARE get_edad FROM lc_query

  {CREATE TEMP TABLE nss_ced_glob
    (nss CHAR(11),
     proceso SMALLINT)

    INSERT INTO nss_ced_glob
    SELECT t.n_seguro, 1
    FROM   taa_cd_det_cedido t
    WHERE  t.fecha_trasp <= TODAY
    AND    t.estado      IN (103,12)

    INSERT INTO nss_ced_glob
    SELECT u.nss_cta1, 2
    FROM   uni_unificado u
    WHERE  u.estado    = 100
    AND    u.fliquida <= TODAY}

END FUNCTION
#######################################################################
FUNCTION proceso_principal()
#pp------------------------ Despliega pantalla. Captura de la solicitud

   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "AFIM0311" ATTRIBUTE(BORDER)
   --DISPLAY " [CTRL-C]Salir  [CTRL-T]Cif Ctrl  [CTRL-O]Cons" AT 1,1
   DISPLAY " AFIM031          CAPTURA SOLICITUD TRANSFERENCIA A SB1                        " AT 2,1 ATTRIBUTE(REVERSE)
   DISPLAY vhoy USING "DD/MM/YYYY" AT 2,67 ATTRIBUTE(REVERSE)
           #-----------------------------------------------------------------------------
   DISPLAY "                   D A T O S   D E L   T R A B A J A D O R                   " AT  5,1 ATTRIBUTE (REVERSE)
   DISPLAY "                             T E L E F O N O                                 " AT 10,1 ATTRIBUTE (REVERSE)
   DISPLAY "                            S O L I C I T U D                                " AT 13,1 ATTRIBUTE (REVERSE)

   MENU "SOLICITUDES DE TRANSFERENCIA"
      COMMAND "Captura" "Captura solicitudes de transferencia de siefore"
         CALL captura()
      COMMAND "Consulta" "Consulta solicitudes de transferencia de siefore"
         CALL consulta()
      {COMMAND "Confirmar" "Confirma solicitudes de transferencia de siefore"
         CALL confirmar()}
      COMMAND "Salir" "Salir del Programa"
         EXIT MENU
   END MENU
END FUNCTION
################################################################################
FUNCTION captura()
   DEFINE lc_query CHAR(400)

   CALL inicia_var() #iv
   SELECT folio
   INTO   folio_solicitud
   FROM   folio_afi_solicitud_siefore

   IF folio_solicitud IS NULL THEN
      LET folio_solicitud = 0
   END IF
   LET folio_solicitud = folio_solicitud + 1
   CALL inserta_folio(folio_solicitud)

   INPUT BY NAME nss,
   	             curp,
   	             siefore_asig,
   	             siefore_sol,
   	             cve_lada,
   	             telefono,
   	             extension,
   	             ffirma_sol,                 
                 edo_sol WITHOUT DEFAULTS
      BEFORE FIELD nss
      	 LET fcaptura = vhoy
         DISPLAY BY NAME nss,
                         curp,
                         folio_solicitud,
                         rfc,
                         nombre_completo,
                         cve_lada,
                         telefono,
                         extension,
                         ffirma_sol,
                         fcaptura,
                         edo_sol,
                         desc_edo_sol,
                         siefore_sol,
                         fcaptura

      AFTER FIELD nss
      	 {IF nss IS NULL THEN
            ERROR "Campo -NO- puede ser -NULO-."
            SLEEP 2
            NEXT FIELD nss
         END IF}

      	 IF nss IS NOT NULL THEN
            SELECT 'X'
            FROM   afi_solicitud_siefore
            WHERE  @nss = nss
            AND    @edo_sol = 1
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
               ERROR "Registro ya aceptado."
               SLEEP 2
               ERROR ""
               NEXT FIELD nss
            END IF

            SELECT MAX(@fcaptura), @edo_sol
            INTO   vfcaptura, vedo_sol
            FROM   afi_solicitud_siefore
            WHERE  @nss = nss
            AND    @edo_sol NOT IN (0,1)
            GROUP BY 2

            IF STATUS <> NOTFOUND THEN
                PROMPT "Registro ya capturado, ", vfcaptura USING "DD-MM-YYYY",
                       " -Edo- ", vedo_sol,  " desea continuar: " FOR venter
                IF venter MATCHES "[Ss]" THEN
                ELSE
                  NEXT FIELD nss
                END IF
            END IF

            LET lc_query = "SELECT * ",
                           "FROM   afi_mae_afiliado ",
                           "WHERE  n_seguro       = '",nss, "' "

            PREPARE prep_afi_mae_afiliado FROM lc_query
            EXECUTE prep_afi_mae_afiliado INTO rm_afiliado.*

            IF STATUS = NOTFOUND THEN
               ERROR "TRABAJADOR NO REGISTRADO EN LA AFORE"
               NEXT FIELD nss
            END IF

            SELECT 'X'
            FROM   taa_cd_det_cedido t
            WHERE  t.n_seguro     = nss
            AND    t.fecha_trasp <= TODAY
            AND    t.estado      IN (103,12)
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
               LET edo_sol      = 7
               LET desc_edo_sol = "CUENTA TRASPASADA"
            ELSE
               SELECT 'X'
               FROM   uni_unificado u
               WHERE  u.nss_cta1  = nss
               AND    u.estado    = 100
               AND    u.fliquida <= TODAY
               GROUP BY 1

               IF STATUS <> NOTFOUND THEN
                  LET edo_sol      = 8
                  LET desc_edo_sol = "CUENTA UNIFICADA"
               END IF
            END IF

         {SELECT @proceso
         INTO   proceso
         FROM   nss_ced_glob
         WHERE  @nss = nss
         IF STATUS <> NOTFOUND THEN
            CASE proceso
              WHEN 1
                LET edo_sol      = 7
                LET desc_edo_sol = "CUENTA TRASPASADA"
              WHEN 2
                LET edo_sol      = 8
                LET desc_edo_sol = "CUENTA UNIFICADA"
            END CASE
         END IF}

            DISPLAY BY NAME edo_sol, desc_edo_sol
            LET curp             = rm_afiliado.n_unico
            LET rfc              = rm_afiliado.n_rfc
            LET nombre_completo  = rm_afiliado.nombres CLIPPED, " ",
                                   rm_afiliado.paterno CLIPPED, " ",
                                   rm_afiliado.materno CLIPPED
            LET tipo_solicitud   = rm_afiliado.tipo_solicitud
            LET folio_afiliacion = rm_afiliado.n_folio

            DISPLAY BY NAME curp, rfc, nombre_completo, tipo_solicitud, folio_afiliacion

            DECLARE cur_act CURSOR FOR
            SELECT a.marca_cod,
                   a.fecha_ini,
                   a.marca_causa
            FROM   cta_act_marca a
            WHERE  a.nss = nss
            ORDER BY 2

            FOREACH cur_act INTO edo_cta, f_marca, edo_act
                SELECT ta.marca_desc
                INTO   l_act
                FROM   tab_marca ta
                WHERE  ta.marca_cod = edo_act
                AND    ta.marca_cod > 0

                SELECT ma.marca_desc
                INTO   edo_desc
                FROM   tab_marca ma
                WHERE  ma.marca_cod = edo_cta
                AND    ma.marca_cod > 0

                IF edo_act > 0 THEN
                    EXIT FOREACH
                END IF
            END FOREACH
            DISPLAY edo_desc AT 1,50 ATTRIBUTE(REVERSE)

            EXECUTE get_edad INTO r_existe, edad, r_criterio, v_curp, v_rfc, v_fena
      	                     USING nss, vhoy
            --OBTENER SIEFORE
            SELECT @siefore
            INTO   siefore_asig
            FROM   tab_rango_edad
            WHERE  edad_min <= edad
            AND    edad_max >= edad

            SELECT razon_social
            INTO   desc_siefore_asig
            FROM   tab_siefore_local
            WHERE  codigo_siefore = siefore_asig

            DISPLAY BY NAME siefore_asig, desc_siefore_asig, edad

            NEXT FIELD siefore_asig
         END IF

      AFTER FIELD curp
      	 IF curp IS NULL AND nss IS NULL THEN
      	 	  ERROR "Debe introducir Nss o Curp"
      	 	  NEXT FIELD nss
      	 END IF

         LET lc_query = "SELECT * ",
                        "FROM   afi_mae_afiliado ",
                        "WHERE  n_unico       = '",curp, "' "

         PREPARE prep_afi_mae_afiliado_curp FROM lc_query
         EXECUTE prep_afi_mae_afiliado_curp INTO rm_afiliado.*

         IF STATUS = NOTFOUND THEN
         	  ERROR "TRABAJADOR NO REGISTRADO EN LA AFORE"
            NEXT FIELD nss
         END IF

         LET nss              = rm_afiliado.n_seguro
         LET rfc              = rm_afiliado.n_rfc
         LET nombre_completo  = rm_afiliado.nombres CLIPPED, " ",
                                rm_afiliado.paterno CLIPPED, " ",
                                rm_afiliado.materno CLIPPED
         LET tipo_solicitud   = rm_afiliado.tipo_solicitud
         LET folio_afiliacion = rm_afiliado.n_folio

         SELECT 'X'
         FROM   afi_solicitud_siefore
         WHERE  @nss = nss
         AND    @edo_sol = 1
         GROUP BY 1

         IF STATUS <> NOTFOUND THEN
            ERROR "Registro ya aceptado."
            SLEEP 2
            ERROR ""
            NEXT FIELD nss
         END IF

         SELECT MAX(@fcaptura), @edo_sol
         INTO   vfcaptura, vedo_sol
         FROM   afi_solicitud_siefore
         WHERE  @nss = nss
         AND    @edo_sol NOT IN (0,1)
         GROUP BY 2

         IF STATUS <> NOTFOUND THEN
             PROMPT "Registro ya capturado, ", vfcaptura USING "DD-MM-YYYY",
                    " -Edo- ", vedo_sol,  " desea continuar: " FOR venter
             IF venter MATCHES "[Ss]" THEN
             ELSE
               NEXT FIELD nss
             END IF
         END IF

         SELECT 'X'
         FROM   taa_cd_det_cedido t
         WHERE  t.n_seguro     = nss
         AND    t.fecha_trasp <= TODAY
         AND    t.estado      IN (103,12)
         GROUP BY 1

         IF STATUS <> NOTFOUND THEN
            LET edo_sol      = 7
            LET desc_edo_sol = "CUENTA TRASPASADA"
         ELSE
            SELECT 'X'
            FROM   uni_unificado u
            WHERE  u.nss_cta1  = nss
            AND    u.estado    = 100
            AND    u.fliquida <= TODAY
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
               LET edo_sol      = 8
               LET desc_edo_sol = "CUENTA UNIFICADA"
            END IF
         END IF

         EXECUTE get_edad INTO r_existe, edad, r_criterio, v_curp, v_rfc, v_fena
      	                  USING nss, vhoy

      	 --OBTENER SIEFORE
            SELECT @siefore
            INTO   siefore_asig
            FROM   tab_rango_edad
            WHERE  edad_min <= edad
            AND    edad_max >= edad

            SELECT razon_social
            INTO   desc_siefore_asig
            FROM   tab_siefore_local
            WHERE  codigo_siefore = siefore_asig


         DISPLAY BY NAME nss,edo_sol, desc_edo_sol,rfc, nombre_completo,tipo_solicitud,folio_afiliacion,
                         edad, siefore_asig, desc_siefore_asig

      AFTER FIELD siefore_asig
      	 --OBTENER SIEFORE
         SELECT @siefore
         INTO   li_siefore_ori
         FROM   tab_rango_edad
         WHERE  edad_min <= edad
         AND    edad_max >= edad

      	 IF siefore_asig IS NULL THEN
      	 	  INITIALIZE desc_siefore_asig TO NULL
      	 	  DISPLAY BY NAME desc_siefore_asig
            CALL lista_siefores(li_siefore_ori) RETURNING siefore_asig, desc_siefore_asig
            DISPLAY BY NAME siefore_asig, desc_siefore_asig
      	 ELSE
      	 	  IF siefore_asig > li_siefore_ori THEN
      	 	  	 ERROR "SIEFORE INCORRECTA. No puede elegir una siefore mayor a ", li_siefore_ori USING "<<"
      	 	  	 NEXT FIELD siefore_asig
      	 	  END IF      	 	  
      	 END IF
      	 
      	 SELECT razon_social
         INTO   desc_siefore_asig
         FROM   tab_siefore_local
         WHERE  codigo_siefore = siefore_asig

         DISPLAY BY NAME desc_siefore_asig
      	 
      	 ERROR ""

      AFTER FIELD siefore_sol
      	 IF siefore_sol IS NULL THEN
      	 	  ERROR "SIEFORE NO PUEDE SER NULA"
      	 	  NEXT FIELD siefore_sol
      	 ELSE
      	 	  SELECT razon_social
            INTO   desc_siefore_sol
            FROM   tab_siefore_local
            WHERE  codigo_siefore = siefore_sol

            DISPLAY BY NAME desc_siefore_sol
      	 END IF
      	 ERROR ""

      AFTER FIELD cve_lada
         NEXT FIELD telefono

      AFTER FIELD telefono
         NEXT FIELD extension

      AFTER FIELD extension
         NEXT FIELD ffirma_sol

      AFTER FIELD ffirma_sol
         IF ffirma_sol IS NULL THEN
            ERROR "Fecha de firma de solicitud -NO PUEDE SER NULA-."
            SLEEP 2
            NEXT FIELD ffirma_sol
         END IF

         IF ffirma_sol > vhoy THEN
            ERROR "La fecha firma de solicitud -NO PUEDE SER MAYOR ",
                  "A LA FECHA DE HOY-."
            SLEEP 2
            NEXT FIELD ffirma_sol
         END IF

         NEXT FIELD edo_sol
      
      {BEFORE FIELD edo_sol
         IF edo_sol IS NOT NULL THEN
            SELECT descripcion
            INTO   desc_edo_sol
            FROM   tab_rch_sol_sie
            WHERE  codigo <> 0
              AND  codigo  = edo_sol
            IF STATUS = NOTFOUND THEN
               ERROR "Codigo Inexistente."
               LET edo_sol = NULL
               DISPLAY BY NAME edo_sol
               NEXT FIELD edo_sol
            END IF

            IF siefore_sol > siefore_asig THEN
            	 ERROR "DEBE SELECCIONAR ESTADO 8"
            	 NEXT FIELD edo_sol
            END IF

            PROMPT "Desea almacenar la informacion [S/N]: " FOR venter
            IF venter MATCHES "[Ss]" THEN
               CALL inserta_sol() #is
               CLEAR FORM
               NEXT FIELD nss
            ELSE
            	 CLEAR FORM
               NEXT FIELD nss
            END IF
         END IF}

      AFTER FIELD edo_sol
         IF edo_sol IS NULL OR
            edo_sol = " "   THEN
            CALL desp_estados() #de
            RETURNING edo_sol, desc_edo_sol
         ELSE
            SELECT descripcion
            INTO   desc_edo_sol
            FROM   tab_rch_sol_sie
            WHERE  codigo <> 0
              AND  codigo  = edo_sol
            IF STATUS = NOTFOUND THEN
               ERROR "Codigo Inexistente."
               NEXT FIELD edo_sol
            END IF
         END IF

         IF edo_sol = 0 THEN
            ERROR "Codigo Inexistente."
            NEXT FIELD edo_sol
         END IF

         DISPLAY BY NAME edo_sol, desc_edo_sol

         IF siefore_sol > siefore_asig AND edo_sol != 8 THEN
            	 ERROR "DEBE SELECCIONAR ESTADO 8"
            	 NEXT FIELD edo_sol
         END IF
         ERROR ""

         PROMPT "Desea almacenar la informacion [S/N]: " FOR venter
         IF venter MATCHES "[Ss]" THEN
            CALL inserta_sol() #is
            ERROR "SOLICITUD INGRESADA"
            SLEEP 2
            ERROR ""
            CLEAR FORM
            RETURN
         ELSE
         	  CLEAR FORM
            NEXT FIELD nss
         END IF

{      ON KEY ( CONTROL - T )
         CALL desp_cifras_ctr() #dcc

      ON KEY ( CONTROL - O )
         CALL desp_consulta() #dc}

      ON KEY (INTERRUPT, CONTROL-C)
         EXIT INPUT
   END INPUT

   IF int_flag THEN
   	  CLEAR FORM
      ERROR "Proceso cancelado. . ."
      SLEEP 2
      CALL borra_folio(folio_solicitud)
      ERROR ""
   END IF

END FUNCTION
#######################################################################
FUNCTION inicia_var()
#iv----------------- Inicia valor de variables de captura

   DISPLAY "                                                                               " AT 1,50
   LET nss             = NULL
   LET curp            = NULL
   LET rfc             = NULL
   LET paterno         = NULL
   LET materno         = NULL
   LET nombres         = NULL
   LET nombre_completo = NULL
   LET cve_lada        = NULL
   LET telefono        = NULL
   LET extension       = NULL
   LET ffirma_sol      = NULL
   LET fcaptura        = NULL
   LET edo_sol         = NULL
   LET desc_edo_sol    = NULL
   LET edo_desc        = NULL
   LET edo_cta         = 0
   LET venter          = NULL
   INITIALIZE rm_afiliado.* TO NULL

   INITIALIZE nss              TO NULL
   INITIALIZE curp             TO NULL
   INITIALIZE siefore_asig     TO NULL
   INITIALIZE siefore_sol      TO NULL
   INITIALIZE cve_lada         TO NULL
   INITIALIZE telefono         TO NULL
   INITIALIZE extension        TO NULL
   INITIALIZE ffirma_sol       TO NULL
   INITIALIZE fcaptura         TO NULL
   INITIALIZE edo_sol          TO NULL
   INITIALIZE tipo_solicitud   TO NULL
   INITIALIZE folio_afiliacion TO NULL



END FUNCTION
#######################################################################
FUNCTION desp_estados()
#de------------------ Despliega estados de la solicitud

  DEFINE aux_val                SMALLINT
  DEFINE l_reg                  ARRAY[1000] OF RECORD
         codigo                 SMALLINT,
         descripcion            CHAR(80)
                                END RECORD
  DEFINE x_x                    CHAR(100),
         x_buscar               CHAR(30)
  DEFINE pos                    SMALLINT
  OPEN WINDOW vent_1 AT 12,3 WITH FORM "AFIM0312" ATTRIBUTE(BORDER)
  DISPLAY "                           ESTADO SOLICITUD                                " AT 1,1 ATTRIBUTE(REVERSE)
  DISPLAY "                      [ENTER] Seleccionar Registro                         " AT 10,1 ATTRIBUTE(REVERSE)

  WHILE TRUE
  	 IF siefore_sol > siefore_asig THEN
  	 	  LET x_x = " SELECT * FROM tab_rch_sol_sie ",
               " WHERE codigo = 8",
               " ORDER BY 1 " CLIPPED
  	 ELSE
  	 	  LET x_x = " SELECT * FROM tab_rch_sol_sie ",
               " WHERE codigo <> 0",
               " AND codigo <= 5",
               " ORDER BY 1 " CLIPPED
  	 END IF

     PREPARE curg17 FROM x_x
     DECLARE cur_g17 CURSOR FOR curg17
     LET pos = 1
     FOREACH cur_g17 INTO l_reg[pos].*
       LET pos = pos + 1
       IF pos >= 1000 THEN
         ERROR "Fue Sobrepasada la capacidad maxima del arreglo"
         EXIT FOREACH
       END IF
     END FOREACH
     IF (pos-1) < 1 THEN
         ERROR "ARCHIVO ESTADO SOLICITUDES ... VACIO"
     END IF
     CALL SET_COUNT(pos-1)
     DISPLAY ARRAY l_reg TO scr_1.*
       ON KEY ( INTERRUPT )
         LET pos = 0
         ERROR "DEBE SELECCIONAR UN ESTADO"
         EXIT DISPLAY
       ON KEY ( CONTROL-M )
         LET pos = ARR_CURR()
         EXIT DISPLAY
     END DISPLAY
     IF pos <> 0 THEN
       EXIT WHILE
     END IF
  END WHILE
  CLOSE WINDOW vent_1
  RETURN l_reg[pos].codigo,l_reg[pos].descripcion

END FUNCTION
#######################################################################
FUNCTION inserta_sol()
#is------------------  Inserta solicitudes de cambio de siefore

   INSERT INTO afi_solicitud_siefore VALUES(
      folio_solicitud,   --folio_solicitud
      nss,               --nss
      curp,              --curp
      tipo_solicitud,    --tipo_solicitud
      folio_afiliacion,  --folio_afiliacion
      siefore_asig,      --siefore_asig
      siefore_sol,       --siefore_sol
      cve_lada,          --cve_lada
      telefono,          --telefono
      extension,         --extension
      fcaptura,          --fcaptura
      ffirma_sol,        --ffirma_sol
      edo_sol            --edo_sol
   )

   {INSERT INTO afi_transf_siefore VALUES
   ( nss,              --nss
     ffirma_sol,       --ffirma_sol
     fcaptura,         --fcaptura
     '',               --ftransferencia
     cve_lada,         --cve_lada
     telefono,         --telefono
     extension,        --extension
     edo_sol,          --edo_sol
     vhoy,             --factualiza
     vusuario          --usuario
   )}

END FUNCTION
#######################################################################
REPORT lcifras_control(ltot_sol, ltot_ace, ltot_rec, lcif)
   DEFINE
      ltot_sol          INTEGER,
      ltot_ace          INTEGER,
      ltot_rec          INTEGER

   DEFINE lcif          RECORD
     edo_sol            SMALLINT,
     des_rec            CHAR(60),
     tot_con            INTEGER
                        END RECORD

   DEFINE cod_afore   SMALLINT
   DEFINE descripcion CHAR(30)

   OUTPUT
      PAGE   LENGTH 60
      TOP    MARGIN 0
      BOTTOM MARGIN 0
      RIGHT  MARGIN 0
      LEFT   MARGIN 0

   FORMAT
      PAGE HEADER

      SELECT a.codigo_afore,b.afore_desc
      INTO   cod_afore,descripcion
      FROM   tab_afore_local a,tab_afore b
      WHERE  a.codigo_afore = b.afore_cod

      PRINT
      PRINT COLUMN 05,cod_afore,"     ",descripcion,
            COLUMN 85,"FECHA : ",TODAY USING"DD/MM/YYYY"
      SKIP 3 LINES

      PRINT COLUMN 28, "REPORTE CIFRAS CONTROL CAPTURA SOLICITUD ",
                       "TRANSFERENCIA A SB1"
      SKIP 2 LINES

      PRINT COLUMN 48, "AFILIACION"
      SKIP 4 LINES

      PRINT
      PRINT COLUMN 05, "PROGRAMA : AFIM031"
      PRINT COLUMN 05, "PAGINA   : ",PAGENO USING"####"

      SKIP 3 LINES

      PRINT COLUMN 05, "TOTAL SOLICITUDES: ",
            COLUMN 85, ltot_sol USING "&&&&&&"

      PRINT
      PRINT COLUMN 05, "TOTAL ACEPTADAS  : ",
            COLUMN 85, ltot_ace USING "&&&&&&"

      PRINT
      PRINT COLUMN 05, "TOTAL RECHAZADAS : ",
            COLUMN 85, ltot_rec USING "&&&&&&"

      PRINT COLUMN 07, "CONCEPTO RECHAZO"

      ON EVERY ROW
      PRINT COLUMN 08, lcif.des_rec CLIPPED,
            COLUMN 85, lcif.tot_con USING "&&&&&&"

END REPORT
#######################################################################
FUNCTION desp_consulta()
#dc-------------------- Despliega informacion de solicitudes

  DEFINE a_con                  ARRAY[1000] OF RECORD
         anss                   CHAR(11),
         anombre                CHAR(50),
         acve_lada              CHAR(5),
         atelefono              CHAR(40),
         aextension             CHAR(5),
         affirma_sol            DATE,
         afcaptura              DATE,
         aedo_sol               SMALLINT,
         adesc_edo_sol          CHAR(60)
                                END RECORD,
         r_con                  RECORD
         anss                   CHAR(11),
         acve_lada              CHAR(5),
         atelefono              CHAR(40),
         aextension             CHAR(5),
         affirma_sol            DATE,
         afcaptura              DATE,
         aedo_sol               SMALLINT
                                END RECORD,
         inter_noti             CHAR(300),
         txt                    CHAR(300),
         pos                    INTEGER,
         j                      INTEGER,
         apaterno               CHAR(40),
         amaterno               CHAR(40),
         anombres               CHAR(40)

  LET pos      = 0
  LET j        = 0
  LET int_flag = FALSE
  LET txt      = NULL

  OPEN WINDOW ventana_3 AT 2,2 WITH FORM "AFIM0314" ATTRIBUTE(BORDER)
  DISPLAY " [ESC]Salir" AT 1,1
  DISPLAY " AFIM031      CONSULTA CAPTURA SOLICITUD TRANSFERENCIA A SB1                   " AT 2,1 ATTRIBUTE(REVERSE)
  DISPLAY vhoy USING "DD/MM/YYYY" AT 2,67 ATTRIBUTE(REVERSE)

  CONSTRUCT inter_noti ON nss, fcaptura FROM nss, fcaptura
     ON KEY(ESC)
        LET int_flag = FALSE
        EXIT CONSTRUCT
     ON KEY(INTERRUPT)
        LET INT_FLAG = TRUE
        EXIT CONSTRUCT
  END CONSTRUCT

  IF int_flag = TRUE THEN
     #LET INT_FLAG = FALSE
     ERROR "BUSQUEDA CANCELADA..."
     SLEEP 2
     ERROR ""
     CLEAR SCREEN
     CLOSE WINDOW ventana_3
  END IF

  IF int_flag = FALSE THEN

     LET txt = " SELECT nss, ffirma_sol, fcaptura, ",
               " cve_lada, telefono, extension, edo_sol",
               " FROM afi_transf_siefore",
               " WHERE ", inter_noti CLIPPED,
               " ORDER BY fcaptura"
     PREPARE c_cons FROM txt
     DECLARE cursor_cons CURSOR FOR c_cons
     FOREACH cursor_cons INTO r_con.anss, r_con.affirma_sol,
                              r_con.afcaptura, r_con.acve_lada,
                              r_con.atelefono, r_con.aextension,
                              r_con.aedo_sol

        LET pos = pos + 1

        LET a_con[pos].anss        = r_con.anss
        LET a_con[pos].affirma_sol = r_con.affirma_sol
        LET a_con[pos].afcaptura   = r_con.afcaptura
        LET a_con[pos].acve_lada   = r_con.acve_lada
        LET a_con[pos].atelefono   = r_con.atelefono
        LET a_con[pos].aextension  = r_con.aextension
        LET a_con[pos].aedo_sol    = r_con.aedo_sol


        SELECT @paterno, @materno, @nombres
          INTO apaterno, amaterno, anombres
          FROM afi_mae_afiliado
         WHERE n_seguro = a_con[pos].anss
        LET a_con[pos].anombre = anombres CLIPPED, " ",
                                apaterno CLIPPED, " ",
                                amaterno CLIPPED

        SELECT @descripcion
          INTO a_con[pos].adesc_edo_sol
          FROM tab_rch_sol_sie
         WHERE codigo = a_con[pos].aedo_sol


        IF pos >= 30000  THEN
           ERROR "Sobrepaso Capacidad Maxima del Arreglo."
           EXIT FOREACH
        END IF

        DISPLAY "TOTAL SOLICITUDES: ", pos USING "######" AT 20,1
        ATTRIBUTE(REVERSE)

     END FOREACH

     IF (pos) = 0 THEN
        ERROR "NO HAY INFORMACION..."
        SLEEP 3
     END IF

     CALL SET_COUNT(pos)
     DISPLAY ARRAY a_con TO scr_3.*
     ON KEY ( INTERRUPT )
         LET pos = 0
         EXIT DISPLAY
     END DISPLAY

     CLOSE WINDOW ventana_3
  END IF

END FUNCTION
#######################################################################
FUNCTION lista_siefores(li_siefore)
   DEFINE li_siefore,
          li_pos        SMALLINT

   DEFINE lr_siefore    ARRAY[6] OF RECORD
          siefore       SMALLINT,
          desc_sieforte CHAR(36)
   END RECORD

   OPEN WINDOW ventana_sie AT 13,3 WITH FORM "AFIM0315" ATTRIBUTE (BORDER)
   DISPLAY "                             S I E F O R E                                 " AT 2,1 ATTRIBUTE(REVERSE)
   DISPLAY "                      [ENTER] Seleccionar Registro                         " AT 9,1 ATTRIBUTE(REVERSE)

   DECLARE cur_siefore CURSOR FOR
   SELECT a.siefore,
          b.razon_social
   FROM   tab_rango_edad a,
          tab_siefore_local b
   WHERE  a.siefore = b.codigo_siefore
   AND    a.siefore <= li_siefore
   AND    a.siefore IN (1,2,3,4,5)

   LET li_pos = 1
   FOREACH cur_siefore INTO lr_siefore[li_pos].*
      LET li_pos = li_pos + 1
   END FOREACH

   LET li_pos = li_pos - 1

   IF (li_pos) >= 1 THEN
      CALL SET_COUNT(li_pos)
      DISPLAY ARRAY lr_siefore TO scr_02.*
         ON KEY (CONTROL-M)
            LET li_pos = ARR_CURR()
            EXIT DISPLAY
         ON KEY (INTERRUPT)
            ERROR "DEBE ESCOJER UN REGISTRO..."
            LET li_pos = ARR_CURR()
         ON KEY (CONTROL-C)
            ERROR "DEBE ESCOJER UN REGISTRO..."
            LET li_pos = ARR_CURR()
      END DISPLAY
   ELSE
      ERROR "NO EXISTE INFORMACION PARA MOSTRAR ..."
      SLEEP 2
      ERROR ""
   END IF
   CLOSE WINDOW ventana_sie

   RETURN lr_siefore[li_pos].siefore,
          lr_siefore[li_pos].desc_sieforte
END FUNCTION
################################################################################
FUNCTION inserta_folio(li_folio_solicitud)
   DEFINE li_folio_solicitud INTEGER

   DELETE
   FROM folio_afi_solicitud_siefore

   INSERT INTO folio_afi_solicitud_siefore VALUES(li_folio_solicitud)

END FUNCTION
################################################################################
FUNCTION borra_folio(li_folio_solicitud)
   DEFINE li_folio_solicitud INTEGER

   DELETE
   FROM  folio_afi_solicitud_siefore

   LET li_folio_solicitud = li_folio_solicitud-1

   INSERT INTO folio_afi_solicitud_siefore VALUES(li_folio_solicitud)

END FUNCTION
################################################################################
FUNCTION consulta()
   DEFINE lc_sql     CHAR(700),
          cla_where  CHAR(500),
          lc_archivo CHAR(100),
          lc_comando CHAR(150)

   DEFINE li_pos    SMALLINT,
          li_cont   SMALLINT
   DEFINE lar_consulta ARRAY [500] OF RECORD
             folio_solicitud   INTEGER,
             nss               CHAR(11),
             curp              CHAR(18),
             sie_sol           SMALLINT,
             ffirma_sol        DATE,
             fcaptura          DATE,
             edo_sol           SMALLINT,
             confirma          CHAR(1)
          END RECORD

   OPEN WINDOW ventana_6 AT 2,2 WITH FORM "AFIM0316" ATTRIBUTE(BORDER)
   DISPLAY " [ESC]Consultar     [CTRL-C]Salir" AT 1,1
   DISPLAY " AFIM031  CONSULTA DE SOLICITUDES DE TRANSFERENCIA DE SIEFORE        " AT 2,1 ATTRIBUTE(REVERSE)
   DISPLAY vhoy USING "DD/MM/YYYY" AT 2,68 ATTRIBUTE(REVERSE)
           #-----------------------------------------------------------------------------
   CALL inicia_var()
   LET int_flag = FALSE

   INPUT BY NAME nss              ,
                 curp             ,
                 folio_solicitud  ,
                 tipo_solicitud   ,
                 folio_afiliacion ,
                 siefore_asig     ,
                 siefore_sol      ,
                 ffirma_sol       ,
                 fcaptura         ,
                 edo_sol

      AFTER FIELD siefore_asig
      	 IF siefore_asig IS NOT NULL THEN
      	 	  SELECT razon_social
            INTO   desc_siefore_asig
            FROM   tab_siefore_local
            WHERE  codigo_siefore = siefore_asig

            DISPLAY BY NAME desc_siefore_asig
      	 END IF

      AFTER FIELD siefore_sol
      	 IF siefore_sol IS NOT NULL THEN
      	 	  SELECT razon_social
            INTO   desc_siefore_sol
            FROM   tab_siefore_local
            WHERE  codigo_siefore = siefore_asig

            DISPLAY BY NAME desc_siefore_sol
         END IF

      AFTER FIELD edo_sol
      	 IF edo_sol IS NOT NULL THEN
      	 	  SELECT descripcion
            INTO   desc_edo_sol
            FROM   tab_rch_sol_sie
            WHERE  codigo  = edo_sol

            DISPLAY BY NAME desc_edo_sol
      	 END IF

      ON KEY (ESC)
         LET int_flag = FALSE
         EXIT INPUT
      ON KEY (CONTROL-C)
         LET int_flag = TRUE
         EXIT INPUT
   END INPUT

   IF int_flag = TRUE THEN
      LET int_flag = FALSE
      ERROR "CONSULTA CANCELADA..."
      SLEEP 2
      ERROR ""
      CLEAR SCREEN
      CLOSE WINDOW ventana_6
      RETURN
   END IF

   CALL my_construct() RETURNING cla_where

   LET lc_sql   = "SELECT folio_solicitud, ", --folio_solicitud
                          "nss, ",            --nss
                          "curp, ",           --curp
                          "siefore_sol, ",    --sie_so
                          "ffirma_sol, ",     --ffirma_sol
                          "fcaptura, ",       --fcaptura
                          "edo_sol, ",        --edo_sol
                          "'X' ",                --confirma
                  "FROM   afi_solicitud_siefore ",
                  "WHERE ", cla_where CLIPPED,
                  "ORDER BY 1,2,3 "

   PREPARE pconsulta FROM lc_sql
   DECLARE cur_cons CURSOR FOR pconsulta

   LET li_pos = 1

   FOREACH cur_cons INTO lar_consulta[li_pos].*
   	  LET lar_consulta[li_pos].confirma = " "
      LET li_pos = li_pos + 1
   END FOREACH

   LET li_pos = li_pos - 1

   IF li_pos >= 1 THEN
      CALL SET_COUNT(li_pos)
      DISPLAY "Folio_sol    NSS           CURP        Sie_sol   F.Firma  F.Captura  Edo    ." AT 11,1 ATTRIBUTE(REVERSE)
      DISPLAY "[CTRL-P]Imp" AT 1,39
      DISPLAY ARRAY lar_consulta TO scr_cons.*
         ON KEY(INTERRUPT)
            CLEAR FORM
            EXIT DISPLAY
         ON KEY(CONTROL-P)
         	  --IMPRIMIR
         	  --LET lc_archivo = "/safre_prc/cta/lst/AFIM031_cons.", vhoy USING "DDMMYYYY"
         	  LET lc_archivo = "/afore/sct/safre_prc/cta/envio/AFIM031_cons.", vhoy USING "DDMMYYYY"

         	  START REPORT rpt_consulta TO lc_archivo
         	  FOR li_cont = 1 TO li_pos
         	  	 OUTPUT TO REPORT rpt_consulta(lar_consulta[li_cont].*,1)
         	  END FOR
         	  FINISH REPORT rpt_consulta

         	  LET lc_comando = "lp ", lc_archivo CLIPPED
         	  RUN lc_comando
         	  ERROR "Reporte Generado"
         	  SLEEP 2
         	  ERROR ""
         	  EXIT DISPLAY
      END DISPLAY
   ELSE
      ERROR "REGISTRO NO ENCONTRADO"
      SLEEP 1
      CLEAR SCREEN
      CLOSE WINDOW ventana_6
      RETURN
   END IF
   CLOSE WINDOW ventana_6

END FUNCTION
################################################################################
FUNCTION my_construct()
   DEFINE lc_where CHAR(500)

   LET lc_where = "1 = 1 "

   IF nss IS NOT NULL THEN
   	  LET lc_where = lc_where CLIPPED, "AND nss='", nss, "' "
   END IF

   IF curp IS NOT NULL THEN
   	  LET lc_where = lc_where CLIPPED, "AND curp='", curp, "' "
   END IF

   IF folio_solicitud IS NOT NULL THEN
   	  LET lc_where = lc_where CLIPPED, "AND folio_solicitud=", folio_solicitud
   END IF

   IF tipo_solicitud IS NOT NULL THEN
   	  LET lc_where = lc_where CLIPPED, "AND tipo_solicitud=", tipo_solicitud
   END IF

   IF folio_afiliacion IS NOT NULL THEN
   	  LET lc_where = lc_where CLIPPED, "AND folio_afiliacion=", folio_afiliacion
   END IF

   IF siefore_asig IS NOT NULL THEN
   	  LET lc_where = lc_where CLIPPED, "AND siefore_asig=", siefore_asig
   END IF

   IF siefore_sol IS NOT NULL THEN
   	  LET lc_where = lc_where CLIPPED, "AND siefore_sol=", siefore_sol
   END IF

   IF ffirma_sol IS NOT NULL THEN
   	  LET lc_where = lc_where CLIPPED, "AND ffirma_sol='", ffirma_sol, "' "
   END IF

   IF fcaptura IS NOT NULL THEN
   	  LET lc_where = lc_where CLIPPED, "AND fcaptura='", fcaptura, "' "
   END IF

   IF edo_sol IS NOT NULL THEN
   	  LET lc_where = lc_where CLIPPED, "AND edo_sol=", edo_sol
   END IF

   RETURN lc_where

END FUNCTION
################################################################################
REPORT rpt_consulta(lr_reporte, li_tipo_rpt)
   DEFINE lr_reporte RECORD
             folio_solicitud   INTEGER,
             nss               CHAR(11),
             curp              CHAR(18),
             sie_sol           SMALLINT,
             ffirma_sol        DATE,
             fcaptura          DATE,
             edo_sol           SMALLINT,
             confirma          CHAR(1)
          END RECORD

   DEFINE li_tipo_rpt SMALLINT

   DEFINE lc_desc_edo CHAR(65)

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      --PAGE LENGTH   1

   FORMAT
   PAGE HEADER
      IF li_tipo_rpt = 1 THEN
      	 --REPORTE DE CONSULTA
         PRINT '\033E',
               '\033(s8H',
               '\033(s4B',
               '\033015'
         PRINT COLUMN 07, "REPORTE DE SOLICITUDES DE TRASNFERENCIA DE SIEFORES",'\033015',
               '\033(s10H',
               '\033(s1B',
               '\033015'
      ELSE
      	--REPORTE DE CONFRIMACIÓN
      	 PRINT '\033E',
               '\033(s10H',
               '\033(s4B',
               '\033015'
         PRINT COLUMN 07, "REPORTE DE SOLICITUDES CONFIRMADAS DE TRASNFERENCIA DE SIEFORES",'\033015',
               '\033(s10H',
               '\033(s1B',
               '\033015'
      END IF

      SKIP 1 LINE

      PRINT COLUMN 01, "Programa: AFIM031",
            COLUMN 64, "Fecha: ", vhoy USING "DD/MM/YYYY",'\033015'            

      IF li_tipo_rpt = 1 THEN
      	 --REPORTE DE CONSULTA
      	 PRINT '\033(s20H',
               '\033(s3B',
               '\033015'
         PRINT COLUMN 01, "Folio Solicitud    NSS           CURP          Siefore Solicitada   Fecha Firma  Fecha Captura  Estado",'\033015',
               '\033(s20H',
               '\033(s1B',
               '\033015'
      ELSE
      	 PRINT '\033(s14H',
               '\033(s3B',
               '\033015'
      	 PRINT COLUMN 01, "Folio Solicitud    NSS           CURP          Siefore Solicitada   Fecha Firma  Fecha Captura  Estado",'\033015',
               '\033(s14H',
               '\033(s1B',
               '\033015'
      END IF

   ON EVERY ROW
      SELECT a.descripcion
      INTO   lc_desc_edo
      FROM   tab_rch_sol_sie a,
             afi_solicitud_siefore b
      WHERE  a.codigo          = b.edo_sol
      AND    b.folio_solicitud = lr_reporte.folio_solicitud

      PRINT COLUMN 07, lr_reporte.folio_solicitud USING "########&" ,
            COLUMN 17, lr_reporte.nss                               ,
            COLUMN 29, lr_reporte.curp                              ,
            COLUMN 55, lr_reporte.sie_sol         USING "&"         ,
            COLUMN 70, lr_reporte.ffirma_sol      USING "DD/MM/YYYY",
            COLUMN 84, lr_reporte.fcaptura        USING "DD/MM/YYYY",
            COLUMN 97, lc_desc_edo

END REPORT
################################################################################
FUNCTION confirmar()
   DEFINE lc_sql     CHAR(700),
          cla_where  CHAR(500),
          lc_archivo CHAR(100),
          lc_comando CHAR(150)

   DEFINE li_pos    SMALLINT,
          li_cont   SMALLINT
   DEFINE lar_consulta ARRAY [500] OF RECORD
             folio_solicitud   INTEGER,
             nss               CHAR(11),
             curp              CHAR(18),
             sie_sol           SMALLINT,
             ffirma_sol        DATE,
             fcaptura          DATE,
             edo_sol           SMALLINT,
             confirma          CHAR(1)
          END RECORD

   OPEN WINDOW ventana_6 AT 2,2 WITH FORM "AFIM0316" ATTRIBUTE(BORDER)
   DISPLAY " [ESC]Consultar     [CTRL-C]Salir" AT 1,1
   DISPLAY " AFIM031    CONFIRMAR SOLICITUDES DE TRANSFERENCIA DE SIEFORE        " AT 2,1 ATTRIBUTE(REVERSE)
   DISPLAY vhoy USING "DD/MM/YYYY" AT 2,68 ATTRIBUTE(REVERSE)
           #-----------------------------------------------------------------------------
   CALL inicia_var()
   LET int_flag = FALSE

   INPUT BY NAME nss              ,
                 curp             ,
                 folio_solicitud  ,
                 tipo_solicitud   ,
                 folio_afiliacion ,
                 siefore_asig     ,
                 siefore_sol      ,
                 ffirma_sol       ,
                 fcaptura         ,
                 edo_sol

      AFTER FIELD siefore_asig
      	 IF siefore_asig IS NOT NULL THEN
      	 	  SELECT razon_social
            INTO   desc_siefore_asig
            FROM   tab_siefore_local
            WHERE  codigo_siefore = siefore_asig

            DISPLAY BY NAME desc_siefore_asig
      	 END IF

      AFTER FIELD siefore_sol
      	 IF siefore_sol IS NOT NULL THEN
      	 	  SELECT razon_social
            INTO   desc_siefore_sol
            FROM   tab_siefore_local
            WHERE  codigo_siefore = siefore_asig

            DISPLAY BY NAME desc_siefore_sol
         END IF

      AFTER FIELD edo_sol
      	 IF edo_sol IS NOT NULL THEN
      	 	  SELECT descripcion
            INTO   desc_edo_sol
            FROM   tab_rch_sol_sie
            WHERE  codigo  = edo_sol

            DISPLAY BY NAME desc_edo_sol
      	 END IF

      ON KEY (ESC)
         LET int_flag = FALSE
         EXIT INPUT
      ON KEY (CONTROL-C)
         LET int_flag = TRUE
         EXIT INPUT
   END INPUT

   IF int_flag = TRUE THEN
      LET int_flag = FALSE
      ERROR "CONSULTA CANCELADA..."
      SLEEP 2
      ERROR ""
      CLEAR SCREEN
      CLOSE WINDOW ventana_6
      RETURN
   END IF

   CALL my_construct() RETURNING cla_where

   LET lc_sql   = "SELECT folio_solicitud, ", --folio_solicitud
                          "nss, ",            --nss
                          "curp, ",           --curp
                          "siefore_sol, ",    --sie_so
                          "ffirma_sol, ",     --ffirma_sol
                          "fcaptura, ",       --fcaptura
                          "edo_sol, ",        --edo_sol
                          "'X' ",                --confirma
                  "FROM   afi_solicitud_siefore ",
                  "WHERE ", cla_where CLIPPED,
                  "AND   edo_sol <> 1",
                  "ORDER BY 1,2,3 "

   PREPARE pconfirma FROM lc_sql
   DECLARE cur_conf CURSOR FOR pconfirma

   LET li_pos = 1

   FOREACH cur_conf INTO lar_consulta[li_pos].*
   	  LET lar_consulta[li_pos].confirma = " "
      LET li_pos = li_pos + 1
   END FOREACH

   LET li_pos = li_pos - 1

   IF li_pos >= 1 THEN
      CALL SET_COUNT(li_pos)
      DISPLAY "Folio_sol    NSS           CURP        Sie_sol   F.Firma  F.Captura  Edo    ." AT 11,1 ATTRIBUTE(REVERSE)
      --DISPLAY "[CTRL-P]Imp" AT 1,39
      DISPLAY " [CTRL-G]Confirmar   [X]Marcar          [CTRL-C]Salir               " AT 1,1

      INPUT ARRAY lar_consulta WITHOUT DEFAULTS FROM scr_cons.*
      	 ATTRIBUTES(MAXCOUNT = li_pos,COUNT = li_pos)
         ON KEY(INTERRUPT, CONTROL-C)
         	  ERROR "CONFIRMACION CANCELADA"
         	  SLEEP 2
         	  ERROR ""
            CLEAR FORM
            EXIT INPUT
         ON KEY(CONTROL-G)
         	  --IMPRIMIR
         	  --LET lc_archivo = "/safre_prc/cta/lst/AFIM031_cons.", vhoy USING "DDMMYYYY"
         	  ERROR "ENTRO"
         	  LET lc_archivo = "/afore/sct/safre_prc/cta/envio/AFIM031_conf.", vhoy USING "DDMMYYYY"

         	  START REPORT rpt_consulta TO lc_archivo
         	  FOR li_cont = 1 TO li_pos
         	  	 IF lar_consulta[li_cont].confirma = "X" THEN
         	  	 	  CALL confirma_solicitud(lar_consulta[li_cont].folio_solicitud)
         	  	    OUTPUT TO REPORT rpt_consulta(lar_consulta[li_cont].*,2)
         	  	 END IF
         	  END FOR
         	  FINISH REPORT rpt_consulta

         	  ERROR "Solicitudes Confirmadas"
         	  SLEEP 2
         	  ERROR ""
         	  LET lc_comando = "lp ", lc_archivo CLIPPED
         	  RUN lc_comando
         	  ERROR "Reporte Generado"
         	  SLEEP 2
         	  ERROR ""
         	  EXIT INPUT
      END INPUT
   ELSE
      ERROR "REGISTROS POR CONFIRMAR NO ENCONTRADOS"
      SLEEP 1
      CLEAR SCREEN
      CLOSE WINDOW ventana_6
      RETURN
   END IF
   CLOSE WINDOW ventana_6

END FUNCTION
################################################################################
FUNCTION confirma_solicitud(li_folio_solicitud)
   DEFINE li_folio_solicitud SMALLINT
   DEFINE lc_sql CHAR(100)

   LET lc_sql = "UPDATE afi_solicitud_siefore ",
                "SET    edo_sol = 1 ",
                "WHERE  folio_solicitud = ?"

   PREPARE upd_edo FROM lc_sql
   EXECUTE upd_edo USING li_folio_solicitud

END FUNCTION
################################################################################