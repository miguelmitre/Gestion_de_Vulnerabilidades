##############################################################################
#Project              => SAFRE (Mexico)                                      #
#Owner                => E.F.P.                                              #
#Programa AFIM032     => CAPTURA DATOS SOLICITUD ACTIVACION NSS              #
#Creado por           => EDUARDO JOAQUIN RESENDIZ MEDINA                     #
#Fecha elaboracion    => 18 abril 2006                                       #
#Sistema              => AFI                                                 #
##############################################################################
DATABASE safre_af 

GLOBALS

   DEFINE  x_nss            CHAR(11)
   DEFINE  x_folio          INTEGER
   DEFINE  x_curp           CHAR(18)
   DEFINE  x_origen         CHAR(20)
   DEFINE  x_cod_promotor   LIKE afi_mae_afiliado.cod_promotor
   DEFINE  x_tipo_solicitud LIKE afi_mae_afiliado.tipo_solicitud

   DEFINE  x_status        SMALLINT,
           n_veces         SMALLINT,
           b_rechazo       SMALLINT, 
           x_origen_cod    SMALLINT,
           i               SMALLINT,
           estados         SMALLINT

   DEFINE opc              CHAR(1)
   DEFINE estado_desc      CHAR(60)

   DEFINE enc              RECORD
          n_unico           CHAR(18),
          folio            INTEGER,
          tipo_solicitud   SMALLINT,
          d_tiposolicitud  CHAR(6),
          nss              CHAR(11),
          paterno          CHAR(50),
          materno          CHAR(50),
          nombres          CHAR(50),
          f_ingreso        DATE,
          desc_afore       CHAR(37),
          f_registro       DATE,
          status           CHAR(50),
          rfc              CHAR(18)
   END RECORD

   DEFINE enc2  ARRAY[32767] OF RECORD
          n_unico           CHAR(18),
          folio            INTEGER,
          tipo_solicitud   SMALLINT,
          d_tiposolicitud  CHAR(6),
          nss              CHAR(11),
          paterno          CHAR(50),
          materno          CHAR(50),
          nombres          CHAR(50),
          f_ingreso        DATE,
          desc_afore       CHAR(37),
          f_registro       DATE,
          status           CHAR(50),
          --estados          SMALLINT,
          rfc              CHAR(18)
   END RECORD

   DEFINE solact  RECORD 
          n_seguro         CHAR(11),   ---afi_mae_afiliado
          nss              CHAR(11),   ---afi_sol_activacion
          curp             CHAR(18),
          paterno          CHAR(40),
          materno          CHAR(40),
          nombres          CHAR(40),
          status_int       SMALLINT,
          cod_op           CHAR(2),
          desc_op          CHAR(4),
          des_rch          CHAR(23),
          fecha_sol        DATE,
          fecha_envio      DATE,
          fecha_activa     DATE,
          factualiza       DATE,
          usuario          CHAR(8)
   END RECORD

   DEFINE solact2 ARRAY[10000] OF RECORD
          n_seguro         CHAR(11),    ---afi_mae_afiliado
          nss              CHAR(11),    ---afi_sol_activacion
          curp             CHAR(18),
          paterno          CHAR(40),
          materno          CHAR(40),
          nombres          CHAR(40),
          status_int       SMALLINT,
          cod_op           CHAR(2),
          desc_op          CHAR(4),
          des_rch          CHAR(23),
          fecha_sol        DATE,
          fecha_envio      DATE,
          fecha_activa     DATE,
          factualiza       DATE,
          usuario          CHAR(8)
   END RECORD

   DEFINE datos RECORD
          f_solicitud        LIKE afi_mae_afiliado.fecha_elaboracion,
          f_envio            LIKE afi_mae_afiliado.fecha_envio,
          f_certifica        LIKE afi_mae_afiliado.fentcons,
          cod_promotor       CHAR(11),
          nombre_pro         CHAR(30),
          rechazo_cod        LIKE afi_rechaza_cert.rdeta_cod,
          rechazo_desc       CHAR(30), 
          telefonop          CHAR(20),
          sexo               LIKE afi_mae_afiliado.sexo,
          desc_sexo          CHAR(60),
          fena               LIKE afi_mae_afiliado.fena,
          estadon            LIKE afi_mae_afiliado.estadon,
          desc_estadon       CHAR(60),
          nacionalidad       LIKE afi_mae_afiliado.nacionalidad,
          ind_infonavit      LIKE afi_mae_afiliado.ind_infonavit
   END RECORD

   DEFINE domicilio ARRAY[100] OF RECORD
          calle              LIKE  afi_domicilio.calle,
          num_ext            LIKE  afi_domicilio.numero,
          num_int            LIKE  afi_domicilio.depto,
          colon_desc         CHAR(50),
          codpos             LIKE  afi_domicilio.codpos,
          deleg_desc         CHAR(50),
          deleg_cod          LIKE afi_domicilio.delega,
          ciudad_desc        CHAR(50),
          ciudad_cod         LIKE afi_domicilio.ciudad,
          estad_desc         CHAR(50),
          estad_cod          LIKE afi_domicilio.estado,
          telefono           CHAR(40),
          extension          CHAR(05),
          tipo_dom           LIKE afi_domicilio.dom_cod,
          marca_envio        LIKE afi_domicilio.marca_envio,
          marca_desc         CHAR(16),
          dom_desc           CHAR(20)
          #tel_desc           CHAR(15)
   END RECORD

   DEFINE txt_2            CHAR(300)
   DEFINE txt_3            CHAR(500)
   DEFINE x_busca          CHAR(140)
   DEFINE g_afore          RECORD LIKE tab_afore_local.*
   DEFINE g_usuario        CHAR(8)
   DEFINE HOY              DATE
   DEFINE ACCION           CHAR(1)
   DEFINE pos              SMALLINT
   DEFINE total_pos        SMALLINT
   DEFINE pasa,
          digito           SMALLINT
   DEFINE desc_err         CHAR(60)
   DEFINE cod_ret          CHAR(1)
   DEFINE vstatus          SMALLINT
   DEFINE total_dom        SMALLINT

END GLOBALS

MAIN

    DEFER INTERRUPT
    OPTIONS PROMPT LINE LAST,
    INPUT WRAP,
--    ACCEPT KEY CONTROL-I,
    COMMENT LINE LAST

    CALL STARTLOG("AFIM032.log")
    CALL inicio()
    CALL proceso_principal()

END MAIN

FUNCTION inicio()
#i---------------

  SELECT *,USER 
  INTO   g_afore.*,g_usuario 
  FROM   tab_afore_local

  LET HOY = TODAY

END FUNCTION

FUNCTION proceso_principal()

  DEFINE bnd_mod CHAR(1)

  OPEN WINDOW borde1 AT 2,2 WITH 21 rows,78 COLUMNS ATTRIBUTE(BORDER)

  DISPLAY " AFIM032                 CAPTURA SOLICITUDES ACTIVACION                        " AT 3,1 ATTRIBUTE(REVERSE)
  DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)

  MENU "ACTIVACION"
    COMMAND "Agrega" "Agrega Solicitud de Activacion"
      LET ACCION = "A"
      CALL agrega()
    COMMAND "Consulta" "Consulta de Solicitudes No Afiliados"
      LET ACCION = "C"
      CALL consulta()
--      CALL busca_datos()
--      CALL Inicializa()
    COMMAND "Salir" "Salir del Programa"
      EXIT MENU
  END MENU

  CLOSE WINDOW borde1

END FUNCTION

FUNCTION Inicializa()

  CLEAR FORM
  INITIALIZE enc.* TO NULL
  INITIALIZE enc2 TO NULL 
  LET vstatus = 0
  LET total_dom = 0

END FUNCTION

FUNCTION Inicializa2()

--  CLEAR FORM
--  INITIALIZE datos.* TO NULL
  INITIALIZE domicilio TO NULL
  DISPLAY datos.* TO scr_afi.*
  DISPLAY ARRAY domicilio TO scr_dom.*

END FUNCTION

FUNCTION agrega()

   LET b_rechazo      = 0
   LET x_origen_cod   = 0
   LET x_origen       = "INEXISTENTE"
   LET x_status       = 0
   LET x_cod_promotor = NULL
   #LET enc.desc_afore = ""

   LET pos = 2

   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)

      OPEN WINDOW AFIM0321 AT 2,2 WITH FORM "AFIM0321" ATTRIBUTE(BORDER)
      DISPLAY "[ESC]Busca Datos Grales.  <FLECHAS>Navega por los Regs.  <ENTER>Busca Domicil." AT 1,1 
      DISPLAY "AFIM032                   SOLICITUD DE ACTIVACION                             " AT 2,1 ATTRIBUTE(REVERSE)   
      DISPLAY HOY USING "dd-mm-yyyy" AT 2,64 ATTRIBUTE(REVERSE)

       LET int_flag              = FALSE

       CONSTRUCT BY NAME x_busca ON n_unico,
                                    paterno,
                                    materno,
                                    nombres

{           ON KEY (CONTROL-M)
                   LET int_flag = FALSE
                   EXIT CONSTRUCT
}
           ON KEY (CONTROL-C)
                   LET int_flag=TRUE
                   EXIT CONSTRUCT

           ON KEY (INTERRUPT) 
                   LET int_flag=TRUE
                   EXIT CONSTRUCT

           ON KEY (Esc)
               LET int_flag = FALSE
               EXIT CONSTRUCT

       END CONSTRUCT
         IF int_flag = TRUE THEN
            CLOSE WINDOW AFIM0321
            LET int_flag = FALSE
            ERROR "BUSQUEDA CANCELADA..."
            SLEEP 2
            ERROR ""
            RETURN
         END IF

         ERROR "Buscando Datos ..."

         LET txt_2 = "SELECT n_unico,",
                          " n_folio,",
                          " tipo_solicitud,",
                          " n_seguro,",
                          " paterno,",
                          " materno,",
                          " nombres,",
                          " frecafor,",
                          " fentcons,",
                          " status_interno,",
--                          " cod_promotor,",
--                          " estadon,",
                          " n_rfc ",
                     "FROM  afi_mae_afiliado ",
                     "WHERE ",x_busca CLIPPED,
                     " AND tipo_solicitud = 8"{,
                     " AND (n_unico IS NOT NULL",
                     " OR  n_unico <> ' ')"}

         PREPARE pre_1 FROM txt_2
         DECLARE c_cur_1 CURSOR FOR pre_1

         LET pos = 1

         FOREACH c_cur_1 INTO enc2[pos].n_unico,       
                            enc2[pos].folio,         
                            enc2[pos].tipo_solicitud,
                            enc2[pos].nss,           
                            enc2[pos].paterno,       
                            enc2[pos].materno,       
                            enc2[pos].nombres,       
                            enc2[pos].f_ingreso,     
                            enc2[pos].f_registro,    
                            enc2[pos].status,        
                            --enc2[pos].estados,       
                            enc2[pos].rfc            

            IF STATUS <> NOTFOUND THEN
               LET x_status     = enc2[POS].status
               LET x_origen     = "NO AFILIADO"
               LET x_origen_cod = 1
               ERROR ""


            ELSE
              LET txt_2 = "SELECT n_unico,",
                                 " n_folio,",            
                                 " tipo_solicitud,",     
                                 " n_seguro,",           
                                 " paterno,",            
                                 " materno,",            
                                 " nombres,",            
                                 " frecafor,",           
                                 " fentcons,",           
                                 " status_interno,",     
--                                 " cod_promotor,",       
--                                 " estadon,",            
                                 " n_rfc ",              
                           "FROM  afi_solicitud ",
                           "WHERE ", x_busca CLIPPED,
                           " AND tipo_solicitud = 8"{,
                           " AND (n_unico IS NOT NULL",
                           " OR  n_unico <> ' ')"}

               PREPARE pre_2 FROM txt_2      
               DECLARE c_cur_2 CURSOR FOR pre_2    
               FOREACH c_cur_2 INTO enc2[pos].n_unico,       
                                  enc2[pos].folio,         
                                  enc2[pos].tipo_solicitud,
                                  enc2[pos].nss,           
                                  enc2[pos].paterno,       
                                  enc2[pos].materno,       
                                  enc2[pos].nombres,       
                                  enc2[pos].f_ingreso,     
                                  enc2[pos].f_registro,    
                                  enc2[pos].status,        
                                  --enc2[pos].estados,       
                                  enc2[pos].rfc

                    IF STATUS <> NOTFOUND THEN
                        LET x_status     = enc2[pos].status
                        LET x_origen     = "SOLICITUD"
                        LET x_origen_cod = 2
                        ERROR ""
                        IF enc.status = 100 THEN
                            ERROR "CURP con NSS ya activado verifique "
                            SLEEP 2
                            CLOSE WINDOW AFIM0321
                            LET int_flag = FALSE
                            ERROR "BUSQUEDA CANCELADA..."
                            SLEEP 2
                            ERROR ""
                            RETURN
                        END IF
                    END IF
            --END IF    

            CASE        
               WHEN enc2[pos].tipo_solicitud = 8
                  LET enc2[pos].d_tiposolicitud = "NO AFI"
            END CASE    
                        
            LET enc.n_unico         = enc2[pos].n_unico          
            LET enc.folio           = enc2[pos].folio            
            LET enc.tipo_solicitud  = enc2[pos].tipo_solicitud   
            LET enc.nss             = enc2[pos].nss              
            LET enc.paterno         = enc2[pos].paterno          
            LET enc.materno         = enc2[pos].materno          
            LET enc.nombres         = enc2[pos].nombres          
            LET enc.f_ingreso       = enc2[pos].f_ingreso        
            LET enc.f_registro      = enc2[pos].f_registro       
            LET enc.status          = enc2[pos].status           
--            LET estados             = enc2[pos].estados          
            LET enc.rfc             = enc2[pos].rfc              
            LET x_tipo_solicitud    = enc2[pos].tipo_solicitud

            CALL muestra_status(x_origen_cod,x_origen,x_status)
               RETURNING estado_desc

            LET enc2[pos].status = estado_desc
            LET x_nss      = enc2[pos].nss
 --           LET enc2[pos].nss    = ""

             LET pos = pos + 1

            END FOREACH
            END IF

            CALL muestra_status(x_origen_cod,x_origen,x_status)
               RETURNING estado_desc

            LET enc2[pos].status = estado_desc
            LET x_nss      = enc2[pos].nss

            LET pos = pos + 1
         END FOREACH
--     INITIALIZE enc2[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)
            LET total_pos = pos -1

            DISPLAY "*** Total Registros Encontrados ", total_pos, " ***" AT 21,37
            DISPLAY ARRAY enc2 TO scr_afili.*

            ON KEY (CONTROL-M)
               DISPLAY "CONTROL: [N]Captura NSS                                             [C] Salir  " AT 1,1
               DISPLAY "                         Datos Generales del Solicitante                       " AT 10,1 ATTRIBUTE(REVERSE)
               LET pos = ARR_CURR()
               LET enc.n_unico        = enc2[pos].n_unico
               LET enc.folio          = enc2[pos].folio
               LET enc.nss            = enc2[pos].nss
               LET x_tipo_solicitud   = enc2[pos].tipo_solicitud
            CALL Despliega_datos(enc.n_unico,
                   enc.nss,
                   x_origen_cod,
                   x_tipo_solicitud,
                   enc.folio)

            ON KEY (INTERRUPT)
               LET pos = ARR_CURR()
               EXIT DISPLAY
         END DISPLAY

         CLOSE WINDOW AFIM0321
      ELSE
         LET txt_2 = "SELECT unique n_unico ",
                     "FROM  afi_mae_afiliado ",
                     "WHERE ", x_busca CLIPPED
         PREPARE pre_4 FROM txt_2      
         DECLARE c_cur_4 CURSOR FOR pre_4    
         FOREACH c_cur_4 INTO enc.n_unico
           SELECT a.status_interno
             INTO vstatus
             FROM afi_sol_activacion a
            WHERE a.curp = enc.n_unico
              AND a.status_interno <> 40
         END FOREACH
         IF vstatus > 0 THEN
            ERROR "CURP ya ingresada en Solicitud de Activacion..."
            SLEEP 3
            ERROR "CURP no puede volver a ingresarse..."
            SLEEP 3
            ERROR "" 
            CLOSE WINDOW AFIM0321
         ELSE
            ERROR "No existen datos para esta condicion ..."
            SLEEP 3
            ERROR ""
            CLOSE WINDOW AFIM0321
         END IF
      END IF

   END IF

END FUNCTION


FUNCTION consulta()

   DEFINE vcod_operacion CHAR(2),
          vmot_rech1     CHAR(50),
          vcod_op        CHAR(2)

   LET b_rechazo      = 0
   LET x_origen_cod   = 0
   LET x_origen       = "INEXISTENTE"
   LET x_status       = 0
   LET x_cod_promotor = NULL
   #LET enc.desc_afore = ""

   LET pos = 2

   IF (pos-1) >= 1 THEN
      CALL  SET_COUNT(pos-1)

      OPEN WINDOW AFIM0321 AT 2,2 WITH FORM "AFIM0322" ATTRIBUTE(BORDER)
      DISPLAY "[ESC]Busca Datos Grales.  <FLECHAS>Navega por los Regs.  <ENTER>Busca Domicil." AT 1,1 
      DISPLAY "AFIM032                    CONSULTA DE ACTIVACION                             " AT 2,1 ATTRIBUTE(REVERSE)   
      DISPLAY HOY USING "dd-mm-yyyy" AT 2,64 ATTRIBUTE(REVERSE)

       LET int_flag              = FALSE

       CONSTRUCT BY NAME x_busca ON curp,
                                    paterno,
                                    materno,
                                    nombres

{           ON KEY (CONTROL-M)
                   LET int_flag = FALSE
                   EXIT CONSTRUCT
}
           ON KEY (CONTROL-C)
                   LET int_flag=TRUE
                   EXIT CONSTRUCT

           ON KEY (INTERRUPT) 
                   LET int_flag=TRUE
                   EXIT CONSTRUCT

           ON KEY (Esc)
               LET int_flag = FALSE
               EXIT CONSTRUCT

       END CONSTRUCT
         IF int_flag = TRUE THEN
            CLOSE WINDOW AFIM0321
            LET int_flag = FALSE
            ERROR "BUSQUEDA CANCELADA..."
            SLEEP 2
            ERROR ""
            RETURN
         END IF

         ERROR "Buscando Datos ..."

         LET txt_3 = "SELECT a.n_seguro,b.nss,b.curp,b.paterno,b.materno,b.nombres,",
                     "b.status_interno,b.cod_operacion,'','',b.fecha_solicitud,",
                     "b.fecha_envio,b.fecha_activacion,b.fecha_actualiza,",
                     "b.usuario,a.n_folio",
                     " FROM afi_mae_afiliado a, afi_sol_activacion b",
                     " WHERE ",x_busca CLIPPED,
                     " AND a.n_unico = b.curp"
                     #" AND a.n_seguro LIKE 'I%'"

         PREPARE pre_3 FROM txt_3
         DECLARE c_cur_3 CURSOR FOR pre_3

         LET pos = 1

         FOREACH c_cur_3 INTO solact2[pos].*,x_folio

            IF solact2[pos].cod_op          = "01" AND
               solact2[pos].status_int      = 100 THEN
               LET  solact2[pos].desc_op    = "ACEP"
               LET  solact2[pos].des_rch    = "ACEPTADO"
            ELSE
               IF solact2[pos].cod_op       = "02" THEN 
                  LET solact2[pos].desc_op  = "RECH"
                  SELECT cod_operacion,mot_rech1
                  INTO   vcod_op,vmot_rech1
                  FROM   afi_rch_activacion
                  WHERE  nss  = solact2[pos].nss 
                  AND    curp = solact2[pos].curp

                  IF vmot_rech1 IS NOT NULL OR
                     vmot_rech1 <> " " THEN
                        SELECT rdeta_desc_c
                        INTO   solact2[pos].des_rch
                        FROM   tab_rdeta
                        WHERE  modulo_cod = "afi"
                        AND    rdeta_cod  = vmot_rech1
                  END IF
               END IF
            END IF

            LET solact.* = solact2[pos].*
            LET solact.des_rch = solact2[pos].des_rch

            LET pos = pos + 1
         END FOREACH
--        INITIALIZE solact2[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)
            LET total_pos = pos -1

            DISPLAY "*** Total Registros Encontrados ", total_pos, " ***" AT 21,37
            DISPLAY ARRAY solact2 TO scr_sol.*

            ON KEY (CONTROL-M)
               DISPLAY "                         Datos Generales del Solicitante                       " AT 10,1 ATTRIBUTE(REVERSE)
               LET pos = ARR_CURR()
               LET solact.curp      = solact2[pos].curp
               LET solact.nss       = solact2[pos].nss
               LET solact.n_seguro  = solact2[pos].n_seguro
               LET x_origen_cod     = 1
               LET x_tipo_solicitud = "8"

            CALL Despliega_datos(solact.curp,
                   solact.n_seguro,
                   x_origen_cod,
                   x_tipo_solicitud,
                   x_folio)

            ON KEY (INTERRUPT)
               LET pos = ARR_CURR()
               EXIT DISPLAY

            ON KEY (CONTROL-C)
               LET pos = ARR_CURR()
               --LET INT_FLAG = TRUE
               EXIT DISPLAY
         END DISPLAY

         CLOSE WINDOW AFIM0321
      ELSE
         ERROR "No existen datos para esta condicion ..."
         SLEEP 3
         ERROR ""
         CLOSE WINDOW AFIM0321
      END IF
   END IF

END FUNCTION


FUNCTION busca_datos()       --x_nss,x_folio,x_tipo_solicitud

   LET b_rechazo      = 0
   LET x_origen_cod   = 0
   LET x_origen       = "INEXISTENTE"
   LET x_status       = 0
   LET x_cod_promotor = NULL

   OPEN WINDOW AFIM0321 AT 2,2 WITH FORM "AFIM0321" ATTRIBUTE(BORDER)
   DISPLAY "                          CONSULTA DE PREAPERTURA                              " AT 2,1 ATTRIBUTE(REVERSE)   

   ERROR "Buscando Datos ..."

   SELECT n_seguro,		#Maeafili
          n_folio,
          tipo_solicitud,
          n_unico,
          paterno,
          materno,
          nombres,
          frecafor,
          fentcons,
          status_interno,
          cod_promotor,
          estadon,
          n_rfc
   INTO   enc.nss,
          enc.folio,
          enc.tipo_solicitud,
          enc.n_unico,
          enc.paterno,
          enc.materno,
          enc.nombres,
          enc.f_ingreso,
          enc.f_registro,
          enc.status,
          x_cod_promotor,
          estados,
          enc.rfc
   FROM   afi_mae_afiliado
   WHERE  n_unico = x_curp

   IF STATUS <> NOTFOUND THEN
      LET x_status     = enc.status
      LET x_origen     = "AFILIADO"
      LET x_origen_cod = 1
      ERROR ""
   ELSE
      SELECT n_seguro,
             n_folio,
             tipo_solicitud,
             n_unico,
             paterno,
             materno,
             nombres,
             frecafor,
             fentcons,
             status_interno,
             cod_promotor,
             estadon,
             n_rfc
      INTO   enc.nss,
             enc.folio,
             enc.tipo_solicitud,
             enc.n_unico,
             enc.paterno,
             enc.materno,
             enc.nombres,
             enc.f_ingreso,
             enc.f_registro,
             enc.status,
             x_cod_promotor,
             estados,
             enc.rfc
      FROM   afi_solicitud
      WHERE  n_unico = x_curp
      AND    n_folio  = x_folio
      AND    tipo_solicitud = x_tipo_solicitud

      IF STATUS <> NOTFOUND THEN
         LET x_status     = enc.status
         LET x_origen     = "SOLICITUD"
         LET x_origen_cod = 2
         ERROR ""
      END IF
   END IF

   CASE
      WHEN enc.tipo_solicitud = 8
         LET enc.d_tiposolicitud = "NO AFI"
   END CASE

   CALL muestra_status(x_origen_cod,x_origen,x_status)
      RETURNING estado_desc

   LET enc.status = estado_desc

   DISPLAY BY NAME enc.n_unico THRU enc.rfc
   DISPLAY BY NAME enc.desc_afore
   DISPLAY "                         Datos Generales del Solicitante                       " AT 10,1 ATTRIBUTE(REVERSE)

   CALL Despliega_datos(enc.n_unico,
                        enc.folio,
                        x_origen_cod,
                        enc.tipo_solicitud,
                        enc.folio)

END FUNCTION


FUNCTION Despliega_datos(x_curp,x_nss,origen_cod,x_tipo_solicitud,x_folio) 

   DEFINE x_nss            CHAR(11)
   DEFINE x_folio          LIKE afi_mae_afiliado.n_folio
   DEFINE x_curp           CHAR(18)
   DEFINE x_tipo_solicitud SMALLINT
   DEFINE pat              CHAR(20),
          mat              CHAR(20),
          nom              CHAR(20)

   DEFINE opc              CHAR(1)
   DEFINE tel              CHAR(40)

   DEFINE motivorec        SMALLINT, 
          b_verificacion   SMALLINT,
          origen_cod       SMALLINT,
          i                SMALLINT,
          n_veces          SMALLINT

   DEFINE enc RECORD
          nss              CHAR(11),
          folio            INTEGER,
          curp             CHAR(18),
          paterno          CHAR(50),
          materno          CHAR(50),
          nombres          CHAR(50),
          f_ingreso        DATE,
          desc_afore       CHAR(37),
          f_registro       DATE,
          status           CHAR(50),
          rfc              CHAR(18)
   END RECORD

   DEFINE mintelefono        INTEGER,
          cuantos_tel        SMALLINT

   DEFINE x_cve_lada         CHAR(3),
          xx_dom_desc        CHAR(20)

   OPTIONS 
      PROMPT LINE LAST,
      INPUT WRAP

   INITIALIZE datos.* TO NULL
   INITIALIZE domicilio TO NULL

   LET b_verificacion = 0

   CASE origen_cod
      WHEN 1
         SELECT count(*) 
         INTO   n_veces 
         FROM   afi_mae_afiliado
         WHERE  n_unico = x_curp
         AND    n_folio  = x_folio
         AND    n_seguro   = x_nss
         AND    tipo_solicitud = x_tipo_solicitud

         IF n_veces > 1 THEN
            ##RETURN n_veces
         ELSE
            SELECT MIN(rowid)
            INTO   mintelefono
            FROM   afi_telefono
            WHERE  nss = x_nss
--            AND    n_folio = x_folio
            AND    tipo_solicitud = x_tipo_solicitud

            DECLARE cur_dom CURSOR FOR
            SELECT dom.calle,
                   dom.numero,
                   dom.depto,
                   dom.colonia,
                   dom.codpos,
                   dom.delega,
                   dom.ciudad,
                   dom.estado,
                   dom.dom_cod,
                   dom.marca_envio
            FROM   afi_domicilio dom
            WHERE  dom.nss            = x_nss 
--            AND    dom.n_folio        = x_folio
            AND    dom.tipo_solicitud = x_tipo_solicitud
            ORDER BY 10 DESC

            LET i = 1

            FOREACH cur_dom INTO domicilio[i].calle,
                                 domicilio[i].num_ext,
                                 domicilio[i].num_int,
                                 domicilio[i].colon_desc,
                                 domicilio[i].codpos,
                                 domicilio[i].deleg_cod,
                                 domicilio[i].ciudad_cod,
                                 domicilio[i].estad_cod,
                                 domicilio[i].tipo_dom,
                                 domicilio[i].marca_envio

               SELECT dom_desc
               INTO   domicilio[i].dom_desc
               FROM   tab_domicilio
               WHERE  dom_cod = domicilio[i].tipo_dom

               IF domicilio[i].marca_envio = "X" THEN
                  LET domicilio[i].marca_desc = "CORRESPONDENCIA"
               ELSE
                  LET domicilio[i].marca_desc = "_______________"
               END IF

               SELECT telefono,
                      cve_lada,
                      extension
               INTO   domicilio[i].telefono,
                      x_cve_lada,
                      domicilio[i].extension
               FROM   afi_telefono
               WHERE  nss = x_nss
  --             AND    n_folio = x_folio
               AND    tipo_solicitud = x_tipo_solicitud
               AND    rowid = mintelefono

               IF x_cve_lada IS NULL OR x_cve_lada = " " THEN
                  LET domicilio[i].telefono = domicilio[i].telefono
               ELSE
                  LET domicilio[i].telefono = "(",x_cve_lada CLIPPED,") ",
                                              domicilio[i].telefono
               END IF

               SELECT estad_desc
               INTO   domicilio[i].estad_desc
               FROM   tab_estado 
               WHERE  estad_cod = domicilio[i].estad_cod

               IF STATUS = NOTFOUND THEN
                  LET domicilio[i].estad_desc = "NO ESPECIFICADO"
               END IF

               SELECT deleg_desc
               INTO   domicilio[i].deleg_desc
               FROM   tab_delegacion
               WHERE  deleg_cod = domicilio[i].deleg_cod

               IF STATUS = NOTFOUND THEN
                  LET domicilio[i].deleg_desc = "NO ESPECIFICADO"
               END IF

               SELECT ciudad_desc 
               INTO   domicilio[i].ciudad_desc
               FROM   tab_ciudad
               WHERE  ciudad_cod = domicilio[i].ciudad_cod

               IF STATUS = NOTFOUND THEN
                  LET domicilio[i].ciudad_desc = "NO ESPECIFICADO"
               END IF

               LET i = i + 1
            END FOREACH

            SELECT afi.fecha_elaboracion,
                   afi.fecha_envio,
                   afi.fentcons,
                   afi.cod_promotor,
                   afi.sexo,
                   afi.fena,
                   afi.estadon,
                   afi.ind_infonavit,
                   afi.nacionalidad
            INTO   datos.f_solicitud,
                   datos.f_envio,
                   datos.f_certifica,
                   datos.cod_promotor,
                   datos.sexo,
                   datos.fena,
                   datos.estadon,
                   datos.ind_infonavit,
                   datos.nacionalidad
            FROM   afi_mae_afiliado afi
            WHERE  afi.n_unico = x_curp 
            AND    afi.n_folio  = x_folio
            AND    afi.n_seguro      = x_nss
            AND    afi.tipo_solicitud = x_tipo_solicitud

            SELECT paterno,
                   materno,
                   nombres,
                   fono
            INTO   pat,
                   mat,
                   nom,
                   tel
            FROM   pro_mae_promotor
            WHERE  cod_promotor = datos.cod_promotor
         END IF 
      WHEN 2
         SELECT COUNT(*) 
         INTO   n_veces 
         FROM   afi_solicitud
         WHERE  n_unico = x_curp
         AND    n_folio = x_folio
         AND    n_seguro       = x_nss
         AND    tipo_solicitud = x_tipo_solicitud

         IF n_veces > 1 THEN
            ##RETURN n_veces 
         ELSE
            SELECT MIN(rowid)
            INTO   mintelefono
            FROM   afi_telefono
            WHERE  nss = x_nss
--            AND    n_folio = x_folio
            AND    tipo_solicitud = x_tipo_solicitud

            DECLARE cur_dom1 CURSOR FOR
            SELECT dom.calle,
                   dom.numero,
                   dom.depto,
                   dom.colonia,
                   dom.codpos,
                   dom.delega,
                   dom.ciudad,
                   dom.estado,
                   dom.dom_cod,
                   dom.marca_envio
            FROM   afi_domicilio dom
            WHERE  dom.nss            = x_nss 
--            AND    dom.n_folio        = x_folio
            AND    dom.tipo_solicitud = x_tipo_solicitud
            ORDER BY 10 DESC

            LET i = 1
            FOREACH cur_dom1 INTO domicilio[i].calle,
                                  domicilio[i].num_ext,
                                  domicilio[i].num_int,
                                  domicilio[i].colon_desc,
                                  domicilio[i].codpos,
                                  domicilio[i].deleg_cod,
                                  domicilio[i].ciudad_cod,
                                  domicilio[i].estad_cod,
                                  domicilio[i].tipo_dom,
                                  domicilio[i].marca_envio

               SELECT dom_desc
               INTO   domicilio[i].dom_desc
               FROM   tab_domicilio
               WHERE  dom_cod = domicilio[i].tipo_dom

               SELECT telefono,
                      cve_lada,
                      extension
               INTO   domicilio[i].telefono,
                      x_cve_lada,
                      domicilio[i].extension
               FROM   afi_telefono
               WHERE  nss = x_nss
--               AND    n_folio = x_folio
               AND    tipo_solicitud = x_tipo_solicitud
               AND    rowid = mintelefono

               IF x_cve_lada IS NULL OR x_cve_lada = " " THEN
                  LET domicilio[i].telefono = domicilio[i].telefono
               ELSE
                  LET domicilio[i].telefono = "(",x_cve_lada CLIPPED,") ",
                                              domicilio[i].telefono
               END IF

              IF domicilio[i].marca_envio = "X" THEN
                 LET domicilio[i].marca_desc = "CORRESPONDENCIA"
              ELSE
                 LET domicilio[i].marca_desc = "_______________"
              END IF

              SELECT estad_desc
              INTO   domicilio[i].estad_desc
              FROM   tab_estado 
              WHERE  estad_cod = domicilio[i].estad_cod

              IF STATUS = NOTFOUND THEN
                 LET domicilio[i].estad_desc = "NO ESPECIFICADO"
              END IF

              SELECT deleg_desc
              INTO   domicilio[i].deleg_desc
              FROM   tab_delegacion
              WHERE  deleg_cod = domicilio[i].deleg_cod

              IF STATUS = NOTFOUND THEN
                 LET domicilio[i].deleg_desc = "NO ESPECIFICADO"
              END IF

              SELECT ciudad_desc 
              INTO   domicilio[i].ciudad_desc
              FROM   tab_ciudad
              WHERE  ciudad_cod = domicilio[i].ciudad_cod

              IF STATUS = NOTFOUND THEN
                 LET domicilio[i].ciudad_desc = "NO ESPECIFICADO"
              END IF
              LET i = i + 1
            END FOREACH

            SELECT afi.fecha_elaboracion,
                   afi.fecha_envio,
                   afi.fentcons,
                   afi.cod_promotor,
                   afi.sexo,
                   afi.fena,
                   afi.estadon,
                   afi.ind_infonavit,
                   afi.nacionalidad
            INTO   datos.f_solicitud,
                   datos.f_envio,
                   datos.f_certifica,
                   datos.cod_promotor,
                   datos.sexo,
                   datos.fena,
                   datos.estadon,
                   datos.ind_infonavit,
                   datos.nacionalidad
            FROM   afi_solicitud afi
            WHERE  afi.n_unico = x_curp 
            AND    afi.n_seguro = x_nss
            AND    afi.n_folio  = x_folio
            AND    afi.tipo_solicitud = x_tipo_solicitud

            SELECT paterno,
                   materno,
                   nombres,
                   fono
            INTO   pat,
                   mat,
                   nom,
                   tel
            FROM   pro_mae_promotor
            WHERE  cod_promotor = datos.cod_promotor
         END IF 

      WHEN 3
      WHEN 4
         SELECT count(*) 
         INTO   n_veces 
         FROM   rec_mae_externo
         WHERE  n_unico = x_curp

         IF n_veces > 1 THEN
            ## RETURN n_veces 
         ELSE 
            LET i = 1

            SELECT factualiza,
                   "",
                   "",
                   "",
                   fonop,
                   callep,
                   numep,
                   deptop,
                   codposp,
                   coloniap,
                   delegap,
                   ciudadp,
                   estadop,
                   sexo,
                   fena,
                   estadop,
                   "",
                   ""
            INTO   datos.f_solicitud,
                   datos.f_envio,
                   datos.f_certifica,
                   datos.cod_promotor,
                   domicilio[i].telefono,
                   domicilio[i].calle,
                   domicilio[i].num_ext,
                   domicilio[i].num_int,
                   domicilio[i].codpos,
                   domicilio[i].colon_desc,
                   domicilio[i].deleg_cod,
                   domicilio[i].ciudad_cod,
                   domicilio[i].estad_cod,
                   datos.sexo,
                   datos.fena,
                   datos.estadon,
                   datos.ind_infonavit,
                   datos.nacionalidad
            FROM   rec_mae_externo
            WHERE  n_unico = x_curp

            LET datos.cod_promotor = ""
            LET domicilio[i].marca_desc = "_______________"

            SELECT estad_desc
            INTO   domicilio[i].estad_desc
            FROM   tab_estado 
            WHERE  estad_cod = domicilio[i].estad_cod

            IF STATUS = NOTFOUND THEN
               LET domicilio[i].estad_desc = "NO EXISTE"
            END IF

            SELECT deleg_desc
            INTO   domicilio[i].deleg_desc
            FROM   tab_delegacion
            WHERE  deleg_cod = domicilio[i].deleg_cod

            IF STATUS = NOTFOUND THEN
               LET domicilio[i].deleg_desc = "NO EXISTE"
            END IF

            SELECT ciudad_desc
            INTO   domicilio[i].ciudad_desc
            FROM   tab_ciudad
            WHERE  ciudad_cod = domicilio[i].ciudad_cod

            IF STATUS = NOTFOUND THEN
               LET domicilio[i].ciudad_desc = "NO EXISTE"
            END IF
            LET i = i + 1
         END IF  

      WHEN 0
         LET b_verificacion = 1
         EXIT CASE 
   END CASE

   IF b_verificacion = 0 THEN
      LET n_veces = 1

      SELECT COUNT(*)
      INTO   cuantos_tel
      FROM   afi_telefono
      WHERE  nss = x_nss
--      AND    n_folio = x_folio
      AND    tipo_solicitud = x_tipo_solicitud

      IF cuantos_tel > 1 THEN
         DISPLAY " Control : <T>+ Telefonos  <C>Salir " AT 21,1 ATTRIBUTE(REVERSE)
      END IF

      LET datos.telefonop  = tel
      LET datos.nombre_pro = pat CLIPPED, " ",
                             mat CLIPPED, " ",
                             nom CLIPPED

      SELECT estad_desc 
      INTO   datos.desc_estadon
      FROM   tab_estado
      WHERE  estad_cod = datos.estadon

      IF STATUS = NOTFOUND THEN
         LET datos.desc_estadon = "NO EXISTE"
      END IF

      SELECT sexo_desc
      INTO   datos.desc_sexo
      FROM   tab_sexo
      WHERE  sexo_cod = datos.sexo

      LET enc.nss   = x_nss
      LET enc.folio = ""
      LET datos.rechazo_desc = enc.desc_afore[1,30] 
      ERROR ""

      DISPLAY datos.* TO scr_afi.*

      LET total_dom = i - 1

      DISPLAY "                                                                              " AT 15,1 ATTRIBUTE(REVERSE)
      DISPLAY " Domicilios : " AT 15,1 ATTRIBUTE(REVERSE)
      DISPLAY total_dom AT 15,15 ATTRIBUTE(REVERSE)
      --DISPLAY "Control[C]Regresar" AT 15,25 ATTRIBUTE(REVERSE)
      --DISPLAY "_" AT 15,62 ATTRIBUTE(REVERSE)
      --DISPLAY "_" AT 15,78  ATTRIBUTE(REVERSE)
      
      DISPLAY "                                           " AT 21,37

      IF (i-1) >= 1 THEN
         CALL SET_COUNT(i-1)

         DISPLAY ARRAY domicilio TO scr_dom.*
            ON KEY (INTERRUPT)
               EXIT DISPLAY

               ON KEY (CONTROL-N)
                  CALL captura(datos.f_solicitud,datos.f_envio)
                     EXIT DISPLAY
                     CLOSE WINDOW AFIM0321
                     RETURN

               ON KEY (CONTROL-T)
                  IF cuantos_tel > 1 THEN
                     CALL consulta_mas_telefonos(x_nss,
                                                 x_folio,
                                                 x_tipo_solicitud)
                  END IF
         END DISPLAY
--         PROMPT "[Enter] para Regresar ... " FOR opc 
      ELSE
         ERROR "NO TIENE DOMICILIO..."
         SLEEP 2
         ERROR ""
         CALL Inicializa2()
--         DISPLAY "" AT 10,1
      END IF
--      CLOSE WINDOW AFIM0321
   ELSE
      ERROR " NO EXISTEN DATOS REGISTRADOS ..."
      SLEEP 2
      ERROR "" 
      LET n_veces = 0
   END IF 
END FUNCTION
#####################################################################
FUNCTION consulta_mas_telefonos(xx_nss,
                                xx_n_folio,
                                xx_tipo_solicitud)

   DEFINE xx_nss             CHAR(11),
          xx_n_folio         DECIMAL(10,0),
          xx_tipo_solicitud  SMALLINT,
          pos                SMALLINT

   DEFINE reg_tel ARRAY[30] OF RECORD
          trap            CHAR(1),
          telefono        CHAR(40),
          tel_cod         SMALLINT,
          extension       CHAR(20)
   END RECORD

   DEFINE x_cve_lada      CHAR(3)
   DEFINE x_email         CHAR(40)
   DEFINE cur_row         SMALLINT,
          scr_row         SMALLINT,
          cont_inp        SMALLINT,
          item_row_cnt    SMALLINT

   DEFINE sql_stat        INTEGER
   DEFINE sw              SMALLINT,
          i               SMALLINT,
          opc             CHAR(1)

   OPEN WINDOW AFIM0323 AT 18,49 WITH FORM "AFIM0323"
   DISPLAY "<Ctrl-C> Salir" AT 5,1 ATTRIBUTE(REVERSE)

   LET x_email = NULL

   DECLARE cursor_tel CURSOR FOR
   SELECT "",
          telefono,
          tel_cod,
          extension,
          cve_lada
   FROM   afi_telefono
   WHERE  nss = xx_nss
--   AND    n_folio = xx_n_folio
   AND    tipo_solicitud = xx_tipo_solicitud

   LET pos = 1

   FOREACH cursor_tel INTO reg_tel[pos].*,
                           x_cve_lada

      IF x_cve_lada IS NULL OR x_cve_lada = " " THEN
         LET reg_tel[pos].telefono = reg_tel[pos].telefono
      ELSE
         LET reg_tel[pos].telefono = "(",x_cve_lada CLIPPED,") ",
                                     reg_tel[pos].telefono
      END IF

      LET pos = pos + 1
   END FOREACH

   LET pos = pos - 1

   IF pos > 0 THEN
      CALL SET_COUNT(pos)
      LET cont_inp = TRUE

      WHILE (cont_inp = TRUE)
        INPUT ARRAY reg_tel WITHOUT DEFAULTS FROM scr_tel_1.*
           AFTER ROW
              LET cur_row = ARR_CURR()
              LET scr_row = SCR_LINE()

              DISPLAY reg_tel[cur_row].* TO scr_tel_1[scr_row].*

           BEFORE ROW
              LET cur_row = ARR_CURR()
              LET scr_row = SCR_LINE()

              IF reg_tel[cur_row].tel_cod = 7 THEN
                 ERROR "E-mail: ",reg_tel[cur_row].telefono
              END IF

              IF (cur_row = pos + 1) THEN
                 LET cont_inp = TRUE
                 EXIT INPUT
              ELSE
                 LET scr_row = SCR_LINE()
                 DISPLAY reg_tel[cur_row].* TO scr_tel_1[scr_row].* 
                 LET cont_inp = FALSE
              END IF

           ON KEY (INTERRUPT)
              LET sw = 1
              EXIT INPUT
        END INPUT
      END WHILE
      CLOSE WINDOW AFIM0323
   ELSE
      ERROR " NO EXISTEN DATOS REGISTRADOS ..."
      SLEEP 2
      ERROR "" 
      CLOSE WINDOW AFIM0323
   END IF
END FUNCTION
#####################################################
FUNCTION muestra_status(x_origen_cod,x_origen,x_estado)
   DEFINE x_origen_cod    SMALLINT
   DEFINE x_estado        SMALLINT
   DEFINE x_estado_desc   CHAR(50),
          x_origen        CHAR(20)

   CASE x_origen_cod
      WHEN 1 SELECT est.estado_desc
             INTO x_estado_desc
             FROM tab_status_afi est
             WHERE est.estado_cod = x_estado

      WHEN 2 SELECT est.estado_desc
             INTO x_estado_desc
             FROM tab_status_afi est
             WHERE est.estado_cod = x_estado

      WHEN 3 SELECT est.estado_desc
             INTO x_estado_desc
             FROM tab_status_afi est
             WHERE est.estado_cod = x_estado
   END CASE

   IF x_estado_desc IS NULL THEN
      LET x_estado_desc = ""
   END IF

   LET x_estado_desc = x_origen CLIPPED,": ",x_estado_desc CLIPPED
   LET x_estado_desc = x_estado_desc CLIPPED

   RETURN x_estado_desc
END FUNCTION

FUNCTION captura(f_solicitud,f_envio)

   DEFINE f_solicitud  DATE
   DEFINE f_envio      DATE

   LET INT_FLAG = FALSE
   LET enc.nss = ""

   INPUT BY NAME enc.nss WITHOUT DEFAULTS
   BEFORE FIELD nss

   AFTER FIELD nss

             IF enc.nss IS NOT NULL OR
                enc.nss <> " " THEN 
                LET pasa = 0
                CALL valida_g_nss(enc.nss) 
                RETURNING pasa, desc_err, cod_ret

                IF pasa = 1 THEN
                   CASE cod_ret
                     WHEN "1"
                       ERROR "",desc_err
                       SLEEP  2
                       ERROR ""
                       NEXT FIELD nss
                     WHEN "2"
                       ERROR "",desc_err
                       SLEEP  2
                       ERROR ""
                       NEXT FIELD nss
                     WHEN "3"
                       ERROR "",desc_err
                       SLEEP  2
                       ERROR ""
                       NEXT FIELD nss
                     WHEN "4"
                       ERROR "",desc_err
                       SLEEP  2
                       ERROR ""
                       NEXT FIELD nss
                   END CASE
                END IF

                CALL valida_nss_db(enc.nss) 
                RETURNING pasa, desc_err, cod_ret

                IF pasa = 1 THEN
                   CASE cod_ret
                      WHEN "1"
                        ERROR "", desc_err
                        SLEEP  2
                        ERROR ""
                        #NEXT FIELD nss
                      WHEN "2"
                        ERROR "",desc_err
                        SLEEP  2
                        ERROR ""
                        NEXT FIELD nss
                      WHEN "3"
                        ERROR "",desc_err
                        SLEEP  2
                        ERROR ""
                        NEXT FIELD nss
                      WHEN "4"
                        ERROR "",desc_err
                        SLEEP  2
                        ERROR ""
                        NEXT FIELD nss
                   END CASE
                END IF

                CALL digito_verif(enc.nss[1,10],10) RETURNING digito

                IF digito = 32000 THEN
                   ERROR "N.S.S. solo contiene digitos"
                   NEXT FIELD nss
                END IF

                IF LENGTH(enc.nss) = 11  AND
                   digito <> enc.nss[11] THEN
                   ERROR "Digito Verificador Invalido, el digito debe ser: ",
                         digito
                   SLEEP 2
                   NEXT FIELD nss
                END IF
             END IF

      ON KEY (CONTROL-C)
              LET INT_FLAG = TRUE
              EXIT INPUT

      ON KEY ( ESC )
              LET INT_FLAG = FALSE
              EXIT INPUT

   END INPUT

      IF INT_FLAG = FALSE THEN
         SELECT COUNT(*)
         INTO  vstatus
         FROM  afi_sol_activacion
         WHERE curp = enc2[pos].n_unico
         AND   status_interno IN (10,30,100)
         
         IF vstatus > 0 THEN
               ERROR "CURP ya ingresada en Solicitud de Activacion..."
               SLEEP 3
               ERROR "CURP no puede volver a ingresarse..."
               SLEEP 3
               ERROR "" 
               RETURN
         ELSE
            
            INSERT INTO afi_sol_activacion
                 VALUES (enc.nss,
                         enc2[pos].n_unico,
                         enc2[pos].paterno,
                         enc2[pos].materno,
                         enc2[pos].nombres,
                         "10",
                         "0",        --operacion
                         TODAY,       --fecha_solicitud
                         "",       --fecha_envio
                         "",          --fecha_activacion
                         TODAY,       --fecha_actualiza
                         g_usuario)

               ERROR "REGISTRO ALMACENADO EN SOLICTUDES DE ACTIVACION..."
               SLEEP 3
               ERROR ""
               CALL inicializa()
               RETURN
         END IF

      END IF

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         CLEAR FORM
         --DISPLAY "" AT 10,1
         --DISPLAY "" AT 2,1
         ERROR "CAPTURA CANCELADA "
         SLEEP 3
         ERROR ""
         CALL inicializa()
         EXIT PROGRAM
      END IF

END FUNCTION
