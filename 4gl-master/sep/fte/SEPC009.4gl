#Proyecto          => Sistema de Afores.( MEXICO )                            #
#Propietario       => E.F.P.                             		      #
#Programa          => VERIFICA Y ACTUALIZA CONTROL CUENTA                     #
#Fecha             => 11 de abril del 2000                                    #
#Realizado por     => ARMANDO RODRIGUEZ CASTROPAREDES.                        #
#Actualizado       => 6 septiembre 2004                                       #
#Modulo            => UNI                                                     #
###############################################################################
DATABASE safre_af
GLOBALS
define enter char(001)
        DEFINE #glo #integer
            cont              ,
            cont1             ,
            cont2             ,
            cont3             ,
            tot_registros     INTEGER

        DEFINE 
            HOY               DATE,
            vfecha_causa      DATE,
            G_LISTA	      CHAR(500),
            cat               CHAR(500),
            vmarca            CHAR(100),
	    aux_pausa         CHAR(1),
	    char              CHAR(1),
	    vusuario          CHAR(8),
            vrecibido         CHAR(40),
	    vcuenta_sep       SMALLINT, 
	    vintra_cta1       SMALLINT, 
	    vextra_uni        SMALLINT, 
	    vextra_cta1       SMALLINT, 
	    vconvive_cod      SMALLINT, 
	    vrechazo_cod      SMALLINT, 
	    codigo            INTEGER, 
	    vfolio2           INTEGER, 
	    vfolio            INTEGER, 
           vestado_causa     ,
           vmarca_causa      ,
           vcorrelativo      INTEGER

	DEFINE g_paramgrales   	RECORD LIKE seg_modulo.*

END GLOBALS

MAIN
    OPTIONS INPUT WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT

    CALL inicio()  #i
    CALL proceso_principal()  #pp

END MAIN

FUNCTION inicio()
#i---------------

    LET HOY = TODAY

    SELECT codigo_afore
    INTO   codigo
    FROM   tab_afore_local

    SELECT estado
    INTO   vrecibido
    FROM   sep_estado_separacion
    WHERE  des_estado = "RECIBIDO"

    SELECT marca_cod
    INTO   vcuenta_sep
    FROM   tab_marca
    WHERE  marca_desc = "SEPARACION DE CUENTAS"

    LET vmarca = " EXECUTE PROCEDURE marca_cuenta (?,?,?,?,?,?,?,?)"
    PREPARE marcaje FROM vmarca

    LET vrechazo_cod  = 0
    LET vconvive_cod  = 0
    LET vcorrelativo  = 0
    LET vmarca_causa  = 0
    LET vfecha_causa  = ""

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "SEPC0091" ATTRIBUTE(BORDER)
    DISPLAY "SEPC009         REALIZA MARCAJE DE LA CUENTA INDIVIDUAL                              " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                         SUJETA A SEPARACION                                         " AT 4,1 ATTRIBUTE(REVERSE)

    DISPLAY "          [Esc] Iniciar                       [Ctrl-C] Salir                         " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

    INPUT BY NAME vfolio

      AFTER FIELD vfolio
          SELECT "X"
          FROM   sep_det_solicitud  a,
                 sep_det_reg_sol_reclamante b
          WHERE  a.folio  = vfolio
          AND    a.n_seguro = b.n_seguro
          AND    a.folio    = b.folio
          AND    b.estado   = vrecibido
          GROUP BY 1

          IF STATUS = NOTFOUND THEN
              ERROR "NO EXISTEN SOLICITUDES POR MARCAR CON ESTE FOLIO"
	      SLEEP 3
              ERROR ""
	      EXIT INPUT
	      EXIT PROGRAM
          END IF

    ON KEY (ESC)
    ERROR " PROCESANDO INFORMACION " 
          SELECT "X"
          FROM   sep_det_solicitud  a,
                 sep_det_reg_sol_reclamante b
          WHERE  a.folio  = vfolio
          AND    a.n_seguro = b.n_seguro
          AND    a.folio    = b.folio
          AND    b.estado   = vrecibido
          GROUP BY 1

        IF STATUS = NOTFOUND THEN
            ERROR "NO EXISTEN SOLICITUDES POR MARCAR CON ESTE FOLIO"
            SLEEP 3
            ERROR ""
            EXIT PROGRAM
        END IF

      CALL marca_cuenta() #cu

      DISPLAY "MARCAJE TOTAL REALIZADO                      " AT 14,20
      DISPLAY "NSS MARCADOS ADMINISTRADOS                   ",cont  AT 15,20
      DISPLAY "NSS NO MARCADOS ADMINISTRADOS                ",cont1 AT 16,20

      PROMPT " VERIFICACION REALIZADA. PRESIONE <ENTER> PARA SALIR "
      FOR CHAR enter
      EXIT INPUT

      ON KEY (INTERRUPT)
         ERROR " PROCESO CANCELADO "
         SLEEP 2
         EXIT PROGRAM

    END INPUT
    CLEAR WINDOW ventana_1
    CLOSE WINDOW ventana_1

END FUNCTION

FUNCTION marca_cuenta()

    DEFINE xnss           CHAR(11)
    DEFINE nss_actual     CHAR(11)
    DEFINE vmovimiento    CHAR(02)
    DEFINE xrechazo       CHAR(03)
    DEFINE ejecuta        CHAR(300)
    DEFINE vmarca_ent     SMALLINT
    DEFINE xmarca         SMALLINT
    DEFINE estado_cta_act SMALLINT
    DEFINE estado_pro_act SMALLINT

    LET cont  = 0
    LET cont1 = 0
    DECLARE cur_1 CURSOR FOR

       SELECT unique a.n_seguro ,
                     a.folio
       FROM   sep_det_solicitud a ,
              sep_det_reg_sol_reclamante b
       WHERE  a.folio       = vfolio
       AND    a.n_seguro    = b.n_seguro
       AND    a.folio       = b.folio
       AND    b.estado      = vrecibido
       ORDER BY 1

    FOREACH cur_1 INTO xnss,vcorrelativo

       LET  cont = cont + 1
       LET vmarca_ent = vcuenta_sep

       DECLARE cur_mar CURSOR FOR marcaje
       OPEN  cur_mar USING
             xnss,      #nss
             vmarca_ent,       #marca_entra
             vcorrelativo,     #correlativo
             vestado_causa,    #estado_marca
             vrechazo_cod,     #codigo_rechazo
             vmarca_causa,     #marca_causa
             vfecha_causa,     #fecha_causa
             vusuario          #usuario

       FETCH cur_mar INTO xmarca,
                          xrechazo
       CLOSE cur_mar

       IF xrechazo > 0 THEN
	   LET cont1 = cont1 + 1

           UPDATE sep_det_solicitud
           SET    diag_confronta     = "02"
           WHERE  n_seguro           = xnss
           AND    folio              = vfolio

       END IF

    END FOREACH

    UPDATE sep_cza_solicitud
    SET    estado = 21
    WHERE  folio  = vfolio
    AND    estado = vrecibido

    UPDATE sep_det_reg_sol_reclamante 
    SET    estado = 21
    WHERE  n_seguro in (SELECT b.n_seguro 
                        FROM   sep_det_solicitud b
                        WHERE  b.folio = vfolio)
    AND    folio  = vfolio
    AND    estado = 2 
                        
END FUNCTION
