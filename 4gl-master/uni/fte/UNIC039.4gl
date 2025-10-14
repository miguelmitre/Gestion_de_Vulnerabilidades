###############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                            #
#Propietario       => E.F.P.                             		      #
#Programa          => ACTUALIZA IMPROCEDENTES NO RESPONSABLES                 #
#Fecha             =>  3 octubre 2002                                         #
#Actualizado       => ARMANDO RODRIGUEZ CASTROPAREDES.                        #
#Fecha act         =>  5 marzo 2003                                           #
#Modificado por    => OMAR SANDOVAL BADILLO                                   #
#Fecha modifica    =>  12 NOVIEMBRE 2004                                      #
###############################################################################
DATABASE safre_af
GLOBALS
    DEFINE enter CHAR(001)

    DEFINE 
        cont              ,
        cont1             ,
        cont2             ,
        cont3             ,
        tot_registros     INTEGER

    DEFINE 
        HOY               DATE,
        G_LISTA	       	  CHAR(500),
        cat            	  CHAR(500),
        borra          	  CHAR(200),
        aux_pausa         CHAR(1),
        char              CHAR(1),
        vpregunta         CHAR(1),
        codigo            CHAR(3),
        vconfronta        INTEGER,
        vaceptado         INTEGER,
        vintrafore        INTEGER,
        vimprocede        INTEGER,
        vpendiente        INTEGER,
        vcedido           INTEGER,
        vlote          	  INTEGER, 
        vfolio2        	  INTEGER, 
        vfolio         	  INTEGER    

    DEFINE g_paramgrales  RECORD LIKE seg_modulo.*
    DEFINE cla_sel        CHAR(100)
    DEFINE cla_sel1       CHAR(100)

    DEFINE disp1,
           disp2,
           disp3          SMALLINT

END GLOBALS
###############################################
MAIN
    OPTIONS INPUT WRAP,
    PROMPT LINE LAST,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT

    CALL STARTLOG("UNIC039.log")
    CALL inicio() 
    CALL proceso_principal() 

END MAIN
###############################################
FUNCTION inicio()

    LET HOY = TODAY

    SELECT codigo_afore
    INTO   codigo
    FROM   tab_afore_local

    SELECT estado
    INTO   vconfronta
    FROM   uni_status
    WHERE  descripcion = "CONFRONTADO"

    SELECT estado
    INTO   vintrafore
    FROM   uni_status
    WHERE  descripcion = "INTRA AFORE"

    SELECT estado
    INTO   vimprocede
    FROM   uni_status
    WHERE  descripcion = "IMPROCEDENTE"

    SELECT estado
    INTO   vpendiente
    FROM   uni_status
    WHERE  descripcion = "PENDIENTE"

    SELECT estado
    INTO   vaceptado
    FROM   uni_status
    WHERE  descripcion = "ACEPTADO CONFRONTA"

    SELECT estado
    INTO   vcedido
    FROM   uni_status
    WHERE  descripcion = "TRASPASO CEDENTE"

END FUNCTION
###############################################
FUNCTION proceso_principal()

   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "UNIC0391" ATTRIBUTE(BORDER)
   DISPLAY "UNIC039       ACTUALIZA ESTADOS PARA NO RESPONSABLES                                " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY "    < Esc > Grabar                                        < Ctrl-C > Salir          " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"dd-mm-yyyy" AT 3,67 ATTRIBUTE(REVERSE)

   INPUT BY NAME vfolio,
                 vpregunta

      AFTER FIELD vfolio
         SELECT "X"
         FROM   uni_unificador
         WHERE  folio            = vfolio
         AND    cve_afo_recep    = "000"
         AND    estado           = vconfronta
         GROUP BY 1

         IF STATUS = NOTFOUND THEN
            SELECT "X"
            FROM   uni_unificador
            WHERE  folio = vfolio
            AND    cve_afo_recep = codigo
            AND    estado = vconfronta
            GROUP BY 1

            IF STATUS = NOTFOUND THEN
               ERROR "NO HAY SOLICITUDES POR MARCAR DE IMPROCEDENTES"
               SLEEP 2
               ERROR ""
               NEXT FIELD vfolio
            END IF
         END IF

      AFTER  FIELD vpregunta
         IF (vpregunta = "S" OR  
            vpregunta = "s")  THEN

            ERROR " PROCESANDO INFORMACION "

            CALL actualiza_improcedente()
            CALL actualiza_cedido()
            CALL actualiza_procedente()

            DISPLAY "                       DESMARCA FAMILIAS NO PROCEDENTES                        " AT 12,1 ATTRIBUTE(REVERSE)

            CALL UNIC012(vfolio)
               RETURNING disp1,
                         disp2,
                         disp3 

            DISPLAY "Total a desmarcar : ",disp1 AT 14,10
            DISPLAY "Unificadores      : ",disp2 AT 16,10
            DISPLAY "Unificados        : ",disp3 AT 18,10

            ERROR " "
            PROMPT "PROCESO FINALIZADO ...[ENTER] PARA CONTINUAR "
            FOR enter
            EXIT INPUT
         ELSE
            ERROR" PROCESO CANCELADO "
            SLEEP 2
            ERROR ""
            EXIT PROGRAM
         END IF

      ON KEY (INTERRUPT)
         ERROR " PROCESO CANCELADO "
         SLEEP 2
         EXIT PROGRAM
   END INPUT

   CLEAR WINDOW ventana_1
   CLOSE WINDOW ventana_1
END FUNCTION
###############################################
FUNCTION actualiza_improcedente()

    DEFINE   nss_impro  CHAR(11)
    DEFINE   xnss_cta1  CHAR(11)

    DECLARE cur_11 CURSOR FOR
    SELECT nss_uni
    FROM   uni_unificador
    WHERE  folio            = vfolio
    AND    cve_afo_recep    = "000"
    #AND    status_convoca   = "0"
    AND    estado           = vconfronta

    FOREACH cur_11 INTO nss_impro
       DECLARE cur_12 CURSOR FOR
       SELECT nss_cta1
       FROM   uni_unificado
       WHERE  folio        = vfolio
       AND    nss_uni      = nss_impro
       AND    estado       = vconfronta
       AND    diag_unifica <> "01"
       GROUP BY 1

       FOREACH cur_12 INTO xnss_cta1
          UPDATE uni_unificado
          SET    estado          = vimprocede,
                 estado_unifica  = 0,
                 estado_traspaso = 1
          WHERE  nss_uni         = nss_impro
	  AND    nss_cta1        = xnss_cta1
	  AND    folio           = vfolio
	  AND    estado          = vconfronta
 
          UPDATE uni_unificador
          SET    estado          = vimprocede,
                 estado_familia  = 0,
                 estado_traspaso = 1
          WHERE  nss_uni         = nss_impro
	  AND    folio           = vfolio
	  AND    estado          = vconfronta
       END FOREACH
   END FOREACH
END FUNCTION
###############################################
FUNCTION actualiza_procedente()

   DEFINE   nss_intra  CHAR(11)
   DEFINE   movimiento CHAR(02)

   DECLARE cur_9 CURSOR FOR
   SELECT nss_uni
   FROM   uni_unificador
   WHERE  folio  = vfolio
   AND    estado = vconfronta
   AND    cve_afo_recep = codigo
    
   FOREACH cur_9 INTO nss_intra

       SELECT "X"
       FROM   uni_unificado
       WHERE  folio        = vfolio
       AND    nss_uni      = nss_intra
       AND    diag_unifica = "01"
       GROUP BY 1

       IF STATUS <> NOTFOUND THEN
          UPDATE uni_unificador
          SET    estado          = 25,
                 estado_traspaso = 1
          WHERE  nss_uni         = nss_intra
	  AND    folio           = vfolio
	  AND    estado = 20
          AND    cve_afo_recep   = codigo

          SELECT "X"
          FROM   uni_unificador
          WHERE  nss_uni         = nss_intra
          AND    estado          = 25
	  AND    folio           = vfolio
          AND    cve_afo_recep   = codigo
          GROUP BY 1

          IF STATUS <> NOTFOUND THEN
             UPDATE uni_unificado
             SET    estado          = 25,
                    estado_traspaso = 1
             WHERE  nss_uni         = nss_intra
	     AND    folio           = vfolio
	     AND    estado          = 20
          END IF
 
       END IF
   END FOREACH

   UPDATE uni_cza_notifica
   SET    estado = vaceptado
   WHERE  folio  = vfolio

   UPDATE uni_sum_notifica
   SET    estado = vaceptado
   WHERE  folio  = vfolio
END FUNCTION
###############################################
FUNCTION actualiza_cedido()

   DEFINE   nss_intra  CHAR(11)
   DEFINE   movimiento CHAR(02)

   DECLARE cur_10 CURSOR FOR
   SELECT nss_uni,ident_movimiento
   FROM   uni_unificador
   WHERE  folio  = vfolio
   AND    estado = vconfronta
   AND    cve_afo_recep <> codigo
    
   FOREACH cur_10 INTO nss_intra,movimiento

      SELECT "X"
      FROM   uni_unificado
      WHERE  folio   = vfolio
      AND    nss_uni = nss_intra
      AND    diag_unifica = "01"
      GROUP BY 1

      IF STATUS <> NOTFOUND THEN
         UPDATE uni_unificador
         SET    estado          = vcedido,
                estado_traspaso = 1
         WHERE  nss_uni         = nss_intra
	 AND    estado          = 20
         AND    folio           = vfolio

         UPDATE uni_unificado
         SET    estado          = vcedido,
                estado_traspaso = 1
         WHERE  nss_uni         = nss_intra
	 AND    folio  = vfolio
	 AND    estado = 20
      END IF
   END FOREACH
END FUNCTION
