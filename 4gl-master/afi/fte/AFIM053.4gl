#############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                          #
#Propietario       => E.F.P.                                                #
#Programa AFIM053  => MANTENIMIENTO DE AFILIADOS (BAJA CURP BDNSAR)         #
#Fecha             => 28 DE ABRIL DE 2010                                   #
#Por               => EDUARDO JOAQUIN RESENDIZ MENDINA                      #
#############################################################################
DATABASE safre_af
 
GLOBALS

   DEFINE g_master RECORD
     n_seguro       LIKE afi_mae_afiliado.n_seguro,
     n_unico        LIKE afi_mae_afiliado.n_unico,
     tipo_solicitud LIKE afi_mae_afiliado.tipo_solicitud,
     n_folio        LIKE afi_mae_afiliado.n_folio,
     paterno        LIKE afi_mae_afiliado.paterno,
     materno        LIKE afi_mae_afiliado.materno,
     nombres        LIKE afi_mae_afiliado.nombres,
     tip_prob       LIKE afi_mae_afiliado.tip_prob,
     desc_prob      CHAR(20)
   END RECORD

   DEFINE g_master_afi RECORD LIKE afi_mae_afiliado.*

   DEFINE vn_seguro CHAR(11),
          vn_unico  CHAR(18),
          sel_where CHAR(500),
          cla_where CHAR(100),
          vdesc_prob CHAR(20),
          vstatmod         ,
          vstatusint SMALLINT

   DEFINE g_afore   RECORD LIKE tab_afore_local.*
   DEFINE g_usuario CHAR(8)
   DEFINE HOY       DATE
   DEFINE ban_sep   SMALLINT

END GLOBALS


MAIN

    DEFER INTERRUPT
    OPTIONS PROMPT LINE LAST,
        INPUT WRAP,
        ACCEPT KEY CONTROL-I,
        COMMENT LINE LAST

    CALL STARTLOG("AFIM053.log")
    CALL inicio()
    --CALL proceso_principal()

END MAIN

FUNCTION inicio()
#i---------------

    INITIALIZE g_master.* TO NULL
    INITIALIZE cla_where TO NULL

    SELECT *, @USER 
    INTO   g_afore.*,g_usuario 
    FROM   tab_afore_local

    LET HOY = TODAY

    DISPLAY "--------------------------------------------------------------------------------" AT 3,1 

    MENU "Solicitud Baja CURP"
       COMMAND "Agrega" "Agrega Solictud"
          CALL proceso_principal()
          CLEAR SCREEN
       COMMAND "Consulta" "Consulta "
          CALL Consulta()
       COMMAND "Salir" "Salir del Programa "
          EXIT PROGRAM
    END MENU

    --CALL proceso_principal()

END FUNCTION

FUNCTION Inicializa()

    INITIALIZE g_master.* TO NULL
    DISPLAY "                                                                               " AT 9,1 ATTRIBUTE(REVERSE)

END FUNCTION

FUNCTION proceso_principal()
#C-----------------

    DEFINE tot_afil  SMALLINT

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "AFIM0531" ATTRIBUTE(BORDER)
    DISPLAY " <ENTER> Aceptar                                              Ctlr[C]Cancelar   " AT 2,1 
    DISPLAY " AFIM053                 SOLICITUD DE BAJA DE CURP                              " AT 3,1 ATTRIBUTE(REVERSE)

    LET int_flag = FALSE
    INITIALIZE g_master.* TO NULL

    CONSTRUCT cla_where ON n_seguro,
                           n_unico
                      FROM n_seguro,
                           n_unico

       ON KEY (control-m)
          LET int_flag = FALSE
          EXIT CONSTRUCT
       ON KEY (control-c)
          LET int_flag = TRUE
          EXIT CONSTRUCT
    END CONSTRUCT

    IF int_flag = TRUE THEN
       LET int_flag = FALSE
       ERROR "BUSQUEDA CANCELADA..."
       SLEEP 2
       ERROR ""
       CLEAR SCREEN
       CLOSE WINDOW ventana_1
       RETURN
    ELSE
       CLOSE WINDOW ventana_1
       IF NOT Rescata_datos(cla_where) THEN
           ERROR "Afiliado NO existe"
           SLEEP 2
           CALL inicio() 
       END IF
    END IF

END FUNCTION

FUNCTION Rescata_datos(rcla_where)

    DEFINE rcla_where CHAR(100)

    DEFINE l_estado   ,
           l_desc     CHAR(25),
           l_act      CHAR(25),
           vst_int    SMALLINT
    DEFINE edo_desc   CHAR(50),
           enter2     CHAR(1),
           enter      CHAR(1)

    INITIALIZE g_master.* TO NULL

    LET sel_where = "SELECT n_seguro,",
                    " n_unico,",
                    " tipo_solicitud,",
                    " n_folio,",
                    " paterno,",
                    " materno,",
                    " nombres,",
                    " tip_prob,",
                    " '',",
                    " status_interno",
                    " FROM afi_mae_afiliado WHERE ", cla_where CLIPPED


    PREPARE query2 FROM sel_where
    DECLARE cursor_2 CURSOR FOR query2
    OPEN cursor_2
    FETCH cursor_2 INTO g_master.*,vst_int

    IF SQLCA.SQLCODE = NOTFOUND THEN
       RETURN FALSE
    END IF

    CLOSE cursor_2

    SELECT docprob_desc
    INTO g_master.desc_prob
    FROM tab_doc_prob
    WHERE docprob_cod = g_master.tip_prob

    SELECT 'X'
      FROM cta_act_marca
     WHERE @nss        = g_master.n_seguro
       AND @marca_cod IN (120,130,150)
     GROUP BY 1
    IF STATUS <> NOTFOUND THEN
       ERROR "Registro inhabilitado "
       SLEEP 3
       ERROR "Solictud Cancelada"
       SLEEP 2
       EXIT PROGRAM
    END IF

    SELECT 'X'
      FROM cta_act_marca
     WHERE @nss        = g_master.n_seguro
       AND @marca_cod IN (241,242,243,244,245)
     GROUP BY 1
    IF STATUS <> NOTFOUND THEN
       ERROR "Registro en proceso de Unificacion "
       SLEEP 3
       ERROR "Solictud Cancelada"
       SLEEP 2
       EXIT PROGRAM
    END IF

    SELECT 'X'
      FROM taa_cd_det_cedido
     WHERE @n_seguro = g_master.n_seguro
       AND @estado  IN (12,103)
     GROUP BY 1
    IF STATUS <> NOTFOUND THEN
       ERROR "Registro traspasado "
       SLEEP 3
       ERROR "Solictud Cancelada"
       SLEEP 2
       EXIT PROGRAM
    END IF



    OPEN WINDOW ventana_2 AT 2,2 WITH FORM "AFIM0532" ATTRIBUTE(BORDER)
    DISPLAY "                                                             Ctrl[C] Cancelar   " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " AFIM053                 SOLICITUD DE BAJA DE CURP                              " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                           DATOS DEL TRABAJADOR                                 " AT 8,1 ATTRIBUTE(REVERSE)
    DISPLAY "                                                                                " AT 10,1 ATTRIBUTE(REVERSE)
    CALL rescata_status(vst_int)
    DISPLAY BY NAME g_master.*

    PROMPT "Desea Solicitar la Baja de CURP? S/N " FOR enter2

    IF enter2 MATCHES "[Ss]" THEN
       UPDATE afi_mae_afiliado
       SET    status_interno = 240         #SOLICITUD BAJA CURP
       WHERE n_seguro = g_master.n_seguro
       AND   n_unico  = g_master.n_unico

       CALL inserta_modificacion ()
    ELSE
       IF enter2 NOT MATCHES "[nN]" THEN
           ERROR "Solo debe presionar (S)i o (N)o"
           SLEEP 3
           ERROR "Solicitud Cancelada "
           SLEEP 2
           ERROR ""
           CLOSE WINDOW ventana_2
           CALL inicio()
       ELSE
           ERROR "Solicitud Cancelada "
           SLEEP 2
           ERROR ""
           CLOSE WINDOW ventana_2
           CALL inicio()
       END IF
    END IF
    RETURN TRUE
    --CLOSE WINDOW ventana_2
END FUNCTION

FUNCTION rescata_status(valor)
#rs---------------------------

    DEFINE edo_desc   CHAR(50)
    DEFINE edo_act    SMALLINT
    DEFINE edo_cta    SMALLINT
    DEFINE edo_proc1   SMALLINT
    DEFINE valor      SMALLINT
    DEFINE valor2     SMALLINT
    DEFINE l_estado   ,
           l_desc     CHAR(25),
           l_act      CHAR(25)
    DEFINE f_marca    DATE

    LET edo_desc  = NULL
    LET l_estado  = NULL
    LET l_desc    = NULL
    LET edo_cta   = 0
    LET edo_proc1 = 0
    LET vstatmod  = 0

    LET valor2 = valor
    SELECT @status_interno
      INTO valor
      FROM afi_mae_modifica
     WHERE @n_seguro      = g_master.n_seguro
       AND @cod_operacion = 0
    IF valor IS NULL THEN
       LET valor = valor2
    END IF
       
    SELECT tsa.estado_desc
    INTO   l_estado
    FROM   tab_status_afi tsa
    WHERE  tsa.estado_cod = valor

    SELECT 'X'
    FROM   cta_act_marca a
    WHERE  a.nss          = g_master.n_seguro
    AND    a.marca_cod    = 280

    LET ban_sep = 0
    IF SQLCA.SQLCODE <> NOTFOUND THEN
       LET ban_sep = 1
    END IF

    DECLARE cur_marca CURSOR FOR
    SELECT c.marca_activa, c.rechazo_cod, a.fecha_ini, a.hora_ini
    FROM   cta_convivencia c, cta_act_marca a
    WHERE  c.marca_entra  = 600
    AND    c.rechazo_cod  > 0
    AND    a.nss          = g_master.n_seguro
    AND    c.marca_activa = a.marca_cod
    ORDER BY 3,4

    FOREACH cur_marca INTO edo_proc1, vstatmod, f_marca
        IF vstatmod > 0 THEN
            EXIT FOREACH
        END IF
    END FOREACH

    IF edo_proc1 IS NULL THEN
        LET edo_proc1 = 0
    ELSE
        SELECT tm.marca_desc
        INTO   l_desc
        FROM   tab_marca tm
        WHERE  tm.marca_cod = edo_proc1
        AND    tm.marca_cod > 0
    END IF

    DECLARE cur_act CURSOR FOR
    SELECT a.marca_cod, a.fecha_ini, a.marca_causa
    FROM   cta_act_marca a
    WHERE  a.nss = g_master.n_seguro
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

    IF vstatmod IS NULL THEN
        LET vstatmod = 0
    END IF

    IF edo_cta = 5 THEN
        LET vstatmod = 2
    END IF

    IF vstatusint = 130 THEN
        LET vstatmod = 2
    END IF

    IF (vstatusint = 160) THEN
       #(vstatusint = 100) THEN
        LET vstatmod = 1
    END IF

    IF edo_proc1 = edo_cta THEN
        LET edo_desc = ' '
    END IF

    DISPLAY l_estado CLIPPED," /",
            l_desc CLIPPED," / ", 
            edo_desc CLIPPED," ",
            l_act AT 10,3 ATTRIBUTE(REVERSE)
 
END FUNCTION 


FUNCTION inserta_modificacion()
#im----------------------------

    DEFINE 
        marca_ant,
        su_st_int SMALLINT,
        operacion CHAR(35),
        cadena    CHAR(17)

    DEFINE vseparacion SMALLINT

    SELECT @USER
    INTO g_usuario
    FROM systables
    GROUP BY 1

    LET su_st_int = 240         #SOLICITUD BAJA CURP
    LET operacion = 'MODIF. AFILIADO SOLIC BAJA CURP'
    LET cadena    = '00000000000000000'

    SELECT a.*
    INTO g_master_afi.*
    FROM afi_mae_afiliado a
    WHERE  a.n_seguro       = g_master.n_seguro
    AND    a.n_folio        = g_master.n_folio
    AND    a.tipo_solicitud = g_master.tipo_solicitud

    SELECT "X"
    FROM   afi_mae_modifica a
    WHERE  a.n_seguro       = g_master.n_seguro
    AND    a.n_folio        = g_master.n_folio
    AND    a.tipo_solicitud = g_master.tipo_solicitud
    AND    a.cod_operacion  = 0
    GROUP BY 1

    IF SQLCA.SQLCODE = 0 THEN
        DELETE FROM afi_mae_modifica
        WHERE  n_seguro       = g_master.n_seguro
        AND    n_folio        = g_master.n_folio
        AND    tipo_solicitud = g_master.tipo_solicitud
        AND    cod_operacion  = 0
    END IF

    UPDATE afi_mae_afiliado 
    SET    status_interno = 240
    WHERE  n_seguro       = g_master.n_seguro
    AND    n_folio        = g_master.n_folio
    AND    tipo_solicitud = g_master.tipo_solicitud

    SELECT @USER
    INTO   g_usuario
    FROM   tab_afore_local
    GROUP BY 1

    SELECT 'X'
    FROM   afi_ctr_curp a
    WHERE  a.nss            = g_master.n_seguro
    AND    a.n_folio        = g_master.n_folio
    AND    a.tipo_solicitud = g_master.tipo_solicitud
    AND    a.fecha_envio    IS NULL
    GROUP BY 1

    IF STATUS <> NOTFOUND THEN
       UPDATE afi_ctr_curp
       SET    cadena_dif     = cadena
       WHERE  nss            = g_master.n_seguro
       AND    n_folio        = g_master.n_folio
       AND    tipo_solicitud = g_master.tipo_solicitud
       AND    fecha_envio    IS NULL
    ELSE
       INSERT INTO afi_ctr_curp
       VALUES (g_master.n_seguro           ,
               g_master.n_unico            ,
               g_master.n_folio            ,
               g_master.tipo_solicitud     ,
               cadena                      ,
               0                           ,--cve_afo_nss_asoc
               ''                          ,--ident_tip_trab
               ''                          ,--cod_operacion  
               ''                          ,--diag_proceso   
               ''                          ,--curp_oficial   
               ''                          ,--fecha_envio    
               ''                          ,--fecha_respuesta
               '19'                        ,--cve_operacion  
               g_usuario                   ,
               HOY                         )--fecha_registro 
    END IF

    IF ban_sep = 1 THEN
       LET vseparacion = 280
    ELSE
       INITIALIZE vseparacion TO null
    END IF 

    INSERT INTO afi_mae_modifica
    VALUES(g_master_afi.tipo_solicitud     ,
           g_master_afi.n_folio            ,
           g_master_afi.fecha_elaboracion  ,
           g_master_afi.folio_edo_cta      ,
           g_master_afi.cod_afore_ced      ,
           g_master_afi.femision           ,
           g_master_afi.frecafor           ,
           g_master_afi.paterno            ,
           g_master_afi.materno            ,
           g_master_afi.nombres            ,
           g_master_afi.n_seguro           ,
           g_master_afi.n_rfc              ,
           g_master_afi.n_unico            ,
           g_master_afi.sexo               ,
           g_master_afi.edo_civil          ,
           g_master_afi.fena               ,
           g_master_afi.salario_base_comis ,
           0                               ,
           g_master_afi.estadon            ,
           g_master_afi.nacionalidad       ,
           g_master_afi.tip_prob           ,
           g_master_afi.fol_prob           ,
           g_master_afi.doc_prob           ,
           g_master_afi.ind_infonavit      ,
           ''                              ,        --05 ene 2009
           g_master_afi.const_curp         ,
           HOY                             ,
           g_usuario                       ,
           0                               ,
           0                               ,
           240                             ,
           ''                              ,
           ''                              ,
           vseparacion                     ,  #interfase identifica registros con separacion
           ''                              ,
           ''                              ,
           ''                              ,
           '' )

    IF SQLCA.SQLCODE = 0 THEN
{        SELECT marca_cod
        INTO   marca_ant
        FROM   cta_act_marca
        WHERE  nss       = g_master.n_seguro
        AND    marca_cod = 600

        IF marca_ant <> 600 THEN
            CALL marca_cuenta()
        END IF

        CALL inserta_logico(su_st_int, operacion)}
        ERROR "SOLICITUD BAJA CURP REALIZADA " 
        SLEEP 3
        ERROR ""
        CLOSE WINDOW ventana_2
        CALL inicio()
    ELSE
        ERROR "REGISTRO CON DATOS ERRONEOS, NO SE HIZO MODIFICACION"
        SLEEP 2
        ERROR ""
    END IF

END FUNCTION

FUNCTION Consulta()

   DEFINE l_record ARRAY[30000] OF RECORD
      fecha_proceso    DATE,
      nss              CHAR(11),
      n_unico          CHAR(18),
      status_interno   SMALLINT,
      desc_status_int  CHAR(28)
   END RECORD

   DEFINE vestado_desc  CHAR(30)
   DEFINE vestado_cod   SMALLINT
   DEFINE pos          INTEGER

   LET pos = 2
   IF (pos-1) >= 1 THEN
      CALL SET_COUNT(pos-1)
      OPEN WINDOW ventana_2 AT 6,3 WITH FORM "AFIM0533" ATTRIBUTE( BORDER)
      DISPLAY " (Enter) Consulta                                            (Ctrl-C) Salir    " AT 1,1 ATTRIBUTE(REVERSE,BOLD)
      DISPLAY "                CONSULTA NSS CON SOLICITUDES DE BAJA CURP                      " AT 3,1 ATTRIBUTE(REVERSE,BOLD) 
      LET int_flag = FALSE

      CONSTRUCT cla_where ON b.fecha_modifica,
                             a.n_seguro,
                             a.n_unico
                        FROM fecha_registro,
                             nss,
                             CURP

         ON KEY (control-m)

            ERROR "PROCESANDO INFORMACION..."
            SLEEP 1
            LET int_flag = FALSE
            EXIT CONSTRUCT
         ON KEY (control-c)

            LET int_flag = TRUE
            EXIT CONSTRUCT
      END CONSTRUCT

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "BUSQUEDA CANCELADA..."
         SLEEP 1
         ERROR ""
         CLEAR SCREEN
         CLOSE WINDOW ventana_2
         RETURN
      END IF

      LET sel_where = "SELECT b.fecha_modifica,a.n_seguro,a.n_unico,b.status_interno, '' ",
                      "FROM afi_mae_afiliado a,afi_mae_modifica b,afi_ctr_curp c WHERE ",
                      cla_where CLIPPED,
                      " AND    a.n_seguro       = b.n_seguro ",
                      " AND    a.n_folio        = b.n_folio ",
                      " AND    a.tipo_solicitud = b.tipo_solicitud ",
                      " AND    a.n_seguro       = c.nss ",
                      " AND    c.cve_operacion  = '19' ",
                      " AND    b.status_interno BETWEEN 240 AND 270 ",
                      "ORDER BY 1,2,3,4 "
      PREPARE query FROM sel_where
      DECLARE cursor_1 CURSOR FOR query
      LET pos = 1

      FOREACH cursor_1 INTO l_record[pos].fecha_proceso,
                            l_record[pos].nss,
                            l_record[pos].n_unico,
                            l_record[pos].status_interno,
                            l_record[pos].desc_status_int

      LET vestado_cod = l_record[pos].status_interno
      SELECT estado_desc
      INTO   vestado_desc
      FROM   tab_status_afi
      WHERE  estado_cod = vestado_cod

         LET l_record[pos].fecha_proceso   = l_record[pos].fecha_proceso
         LET l_record[pos].nss             = l_record[pos].nss
         LET l_record[pos].n_unico         = l_record[pos].n_unico
         LET l_record[pos].status_interno  = l_record[pos].status_interno
         LET l_record[pos].desc_status_int = vestado_desc


         LET pos = pos + 1
      END FOREACH

      INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL  SET_COUNT(pos-1)
         ERROR ""

         DISPLAY ARRAY l_record   TO scr_1.*

            ON KEY (control-p)
               ERROR "PROCESANDO INFORMACION..."
               --CALL impresion(pos)
            ON KEY (INTERRUPT)
            EXIT DISPLAY
            CLOSE WINDOW ventana_2
         END DISPLAY

         CLOSE WINDOW ventana_2
      ELSE
         ERROR "ARCHIVO DE COMPARATIVOS... VACIO"
         SLEEP 1
         ERROR ""
         CLOSE WINDOW ventana_2
         RETURN
      END IF
   END IF
{
   SELECT siefore_desc
   INTO   vsiefore_desc
   FROM   safre_af:tab_siefore
   WHERE  siefore_cod = g_reg.siefore_cod
   AND    afore_cod   = g_reg.afore_cod

   DISPLAY BY NAME g_reg.afore_cod,
                   g_reg.siefore_cod,
                   g_reg.rendimiento,
                   g_reg.comision,
                   g_reg.rendimiento_neto,
                   g_reg.fecha_ini,
                   g_reg.fecha_fin,
                   g_reg.orden_consar,
                   g_reg.fecha_cifras_al


   DISPLAY BY NAME vsiefore_desc

      SELECT afore_desc
      INTO   x_afore_desc
      FROM   tab_afore
      WHERE  afore_cod = g_reg.afore_cod
      DISPLAY BY NAME x_afore_desc

      IF int_flag = TRUE THEN
         LET int_flag = FALSE
         ERROR "CONSULTA CANCELADA ..."
         SLEEP 2
         ERROR ""
         CLEAR FORM
         CLEAR SCREEN
         RETURN
      ELSE
         PROMPT "<ENTER> Para Salir de la Consulta" for aux_pausa
      END IF
}

   CLEAR SCREEN

END FUNCTION
