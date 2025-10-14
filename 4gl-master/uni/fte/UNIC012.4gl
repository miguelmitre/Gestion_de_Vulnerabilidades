###############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                            #
#Propietario       => E.F.P.       					      #
#Programa          => VERIFICA Y ACTUALIZA CONTROL CUENTA/ confrontado        #
#Fecha             => 2 de junio del 2000                                     #
#Realizado por     => ARMANDO RODRIGUEZ CASTROPAREDES.                        #
#Fecha Act         => 19 abril 2002                                           #
#Modificado por    => OMAR SANDOVAL BADILLO                                   #
#Fecha modifica    => 12 NOVIEMBRE 2004                                       #
#Modulo            => UNI                                                     #
###############################################################################
DATABASE safre_af
GLOBALS
        DEFINE enter_12          CHAR(001)

        DEFINE 
            cont1_12             ,
            cont2_12             ,
            cont3_12             ,
            tot_registros_12     INTEGER

        DEFINE 
            HOY_12               DATE,
	    vusuario_12          CHAR(8),
	    vdesmarca_12         CHAR(100),
	    receptor_12          CHAR(3),
	    aux_pausa_12         CHAR(1),
	    char_12              CHAR(1),
	    xcodigo_12           SMALLINT, 
	    xrechazo_12          SMALLINT, 
	    codigo_12            INTEGER, 
	    vimprocedente_12     INTEGER, 
	    vintra_uni_12        INTEGER, 
	    vintra_cta1_12       INTEGER, 
	    vextra_uni_12        INTEGER, 
	    vextra_cta1_12       INTEGER, 
	    vconfrontado_12      INTEGER, 
	    vsolicitado_12       INTEGER, 
	    vfolio_12            INTEGER,
            vmarca_causa_12        ,
            vcorrelativo_12        ,
            vestado_marca_12       SMALLINT

	DEFINE g_paramgrales_12  RECORD LIKE seg_modulo.*

        DEFINE disp1,
               disp2,
               disp3             SMALLINT

        DEFINE opc    CHAR(1)

END GLOBALS
##################################################
FUNCTION UNIC012(vfolio_12)

   DEFINE vfolio_12      SMALLINT

   CALL inicio_12()
   CALL proceso_principal_12(vfolio_12)
      RETURNING disp1,
                disp2,
                disp3

   RETURN disp1,
          disp2,
          disp3

END FUNCTION
################################################
FUNCTION inicio_12()

    LET HOY_12 = TODAY
    LET receptor_12  = "000"

    SELECT codigo_afore
    INTO   codigo_12
    FROM   tab_afore_local

    SELECT estado
    INTO   vconfrontado_12
    FROM   uni_status
    WHERE  descripcion = "CONFRONTADO"

    SELECT estado
    INTO   vimprocedente_12
    FROM   uni_status
    WHERE  descripcion = "IMPROCEDENTE"

    SELECT estado,
	   USER
    INTO   vsolicitado_12,
	   vusuario_12
    FROM   uni_status
    WHERE  descripcion = "SOLICITADO"

    SELECT marca_cod
    INTO   vintra_uni_12
    FROM   tab_marca
    WHERE  marca_desc = "UNIFICACION INTRA-AFORE"

    SELECT marca_cod
    INTO   vextra_uni_12
    FROM   tab_marca
    WHERE  marca_desc = "UNIFICACION EXTRA-AFORE"

    LET vintra_uni_12  = 241
    LET vintra_cta1_12 = 242
    LET vextra_uni_12  = 243
    LET vextra_cta1_12 = 244

    LET vdesmarca_12 = " EXECUTE PROCEDURE desmarca_cuenta (?,?,?,?,?,?) "

    LET vcorrelativo_12  = 0
    --LET vmarca_causa_12  = 30
    LET vmarca_causa_12  = 0
    LET vestado_marca_12 = 30


END FUNCTION
################################################
FUNCTION proceso_principal_12(vfolio_12)

   DEFINE vfolio_12         SMALLINT

   SELECT "X"
   FROM   uni_unificado
   WHERE  folio  = vfolio_12
   AND    diag_unifica = "02"
   AND    estado       = vimprocedente_12
   GROUP BY 1

   IF STATUS = NOTFOUND THEN
      ERROR "NO EXISTE EL FOLIO POR DESMARCAR"
      SLEEP 2
      EXIT PROGRAM
   END IF

   CALL control_unifica_12(vfolio_12) 
      RETURNING disp1,
                disp2,
                disp3

    RETURN disp1,
           disp2,
           disp3

END FUNCTION
################################################
FUNCTION control_unifica_12(vfolio_12)

   DEFINE vfolio_12         SMALLINT

   DEFINE vmovimiento_12    CHAR(02),
          vmovimiento1_12   CHAR(02),
          xrechazo_12       CHAR(03),
          xrechazo1_12      CHAR(03),
          vmarca_12         SMALLINT,
          vmarca1_12        SMALLINT,
          xmarca_12         SMALLINT,
          xmarca1_12        SMALLINT,
          ejecuta_12        CHAR(300),
          ejecuta1_12       CHAR(300)

   DEFINE nss_unifica_12    CHAR(11)
   DEFINE nss_cuenta_12     CHAR(11)

   LET cont1_12 = 0
   LET cont2_12 = 0
   LET cont3_12 = 0

   DECLARE cur_1 CURSOR FOR
      SELECT a.nss_uni,
      a.nss_cta1
      FROM   uni_unificado a
      WHERE  a.folio        = vfolio_12
      AND    a.diag_unifica = "02"
      AND    a.estado       = vimprocedente_12

   FOREACH cur_1 INTO nss_unifica_12,
                      nss_cuenta_12

      LET  cont1_12 = cont1_12 + 1

      SELECT "X"
      FROM   uni_unificado
      WHERE  folio        = vfolio_12
      AND    nss_cta1     = nss_cuenta_12
      AND    cve_ent_cta1 = codigo_12
      GROUP BY 1

      IF STATUS <> NOTFOUND THEN

         LET vmovimiento_12 = ""

	 SELECT ident_movimiento
	 INTO   vmovimiento_12
	 FROM   uni_unificador
	 WHERE  folio        = vfolio_12
	 AND    nss_uni      = nss_unifica_12
	 GROUP BY 1

         LET  cont2_12 = cont2_12 + 1
         IF vmovimiento_12 = "01" THEN
	    LET vmarca_12 = vintra_cta1_12
         ELSE
	    LET vmarca_12 = vextra_cta1_12
         END IF


         PREPARE marcaje  FROM vdesmarca_12
         EXECUTE marcaje USING
              nss_cuenta_12,        # nss
              vmarca_12,            # marca_entra
              vcorrelativo_12,      # correlativo
              vestado_marca_12,     # estado_marca
              vmarca_causa_12,      # marca_causa
              vusuario_12           # usuario
       END IF

	SELECT "X"
	FROM   uni_unificador
	WHERE  folio        = vfolio_12
	AND    nss_uni      = nss_unifica_12
	AND    cve_ent_nss  = codigo_12
	GROUP BY 1

	IF STATUS <> NOTFOUND THEN

           LET vmovimiento1_12 = ""

           SELECT ident_movimiento
           INTO   vmovimiento1_12
           FROM   uni_unificador
           WHERE  folio        = vfolio_12
           AND    nss_uni      = nss_unifica_12
           GROUP BY 1

           LET  cont3_12 = cont3_12 + 1
           IF vmovimiento1_12 = "01" THEN
	      LET vmarca1_12 = vintra_uni_12
           ELSE
	      LET vmarca1_12 = vextra_uni_12
           END IF

           PREPARE marcaje1 FROM vdesmarca_12
           EXECUTE marcaje1 USING
              nss_unifica_12,       # nss
              vmarca1_12,           # marca_entra
              vcorrelativo_12,      # correlativo
              vestado_marca_12,     # estado_marca
              vmarca_causa_12,      # marca_causa
              vusuario_12           # usuario
	END IF

    END FOREACH
{
    UPDATE uni_cza_notifica
    SET    estado = vsolicitado_12 
    WHERE  folio  = vfolio_12
    AND    estado = vconfrontado_12
}
    RETURN cont1_12,
           cont3_12,      --- Regresa el acumulador de unificador
           cont2_12       --- Regresa el acumulador de unificado

END FUNCTION
