###############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                            #
#Propietario       => E.F.P.                   	                              #
#Programa          => VERIFICA Y ACTUALIZA CONTROL CUENTA                     #
#Fecha             => 11 de abril del 2000                                    #
#Realizado por     => ARMANDO RODRIGUEZ CASTROPAREDES.                        #
#Actualizado       => 14 noviembre 2003                                       #
#Modificado por    => OMAR SANDOVAL BADILLO   #se adecua como funcion         #
#Fecha modifica    => 11 noviembre 2004                                       #
#Fecha modifica    => 08 marzo del 2005                                       #
#Modulo            => UNI                                                     #
###############################################################################
# fecha Actuali   => CPL-1867 04/02/2015                                      #
# Autor           => Cristian  Morales Roblero                                #
# Actualizacion   => se agrega tabla para el almacenaje de los nss que no se  #
#                    marcaron                                                 #
###############################################################################

DATABASE safre_af
GLOBALS
   DEFINE enter_09 char(001)

   DEFINE codigo_09            ,
	       vfolio2_09           ,
	       vfolio_09            ,
          cont1_09             ,
          cont2_09             ,
          cont21_09            ,
          cont3_09             ,
          cont4_09             ,
          cont41_09             ,
          tot_registros_09     INTEGER

   DEFINE vfecha_causa_09      ,
          HOY_09               DATE

   DEFINE G_LISTA_09	      CHAR(500),
          cat_09              CHAR(500),
          vmarca_09           CHAR(100),
	       aux_pausa_09        CHAR(1),
	       char_09             CHAR(1),
	       vusuario_09         CHAR(8),
          vrecibido_09        CHAR(40),
          vconfronta_09       CHAR(40)

   DEFINE vintra_uni_09        ,
	       vintra_cta1_09       ,
	       vextra_uni_09        ,
	       vextra_cta1_09       ,
	       vconvive_cod_09      ,
	       vrechazo_cod_09      ,
          vestado_causa_09     ,
          vmarca_causa_09      ,
          vcorrelativo_09      SMALLINT

   DEFINE g_paramgrales_09 RECORD LIKE seg_modulo.*

   DEFINE disp1,
          disp2,
          disp21,
          disp3,
          disp4,
          disp41               INTEGER
END GLOBALS
##################################################
FUNCTION UNIC009(vfolio_09)

    DEFINE vfolio_09     INTEGER

    DEFINE disp1,
           disp2,
           disp21,
           disp3,
           disp4,
           disp41        INTEGER

    CALL inicio()
    CALL proceso_principal(vfolio_09)
       RETURNING disp1,
                 disp2,
                 disp21,
                 disp3,
                 disp4,
                 disp41

    RETURN disp1,
           disp2,
           disp21,
           disp3,
           disp4,
           disp41
END FUNCTION
##################################################
FUNCTION inicio()

    LET HOY_09 = TODAY

    SELECT codigo_afore
    INTO   codigo_09
    FROM   tab_afore_local

    SELECT estado,USER
    INTO   vconfronta_09,
           vusuario_09
    FROM   uni_status
    WHERE  descripcion = "CONFRONTADO"

    SELECT estado
    INTO   vrecibido_09
    FROM   uni_status
    WHERE  descripcion = "RECIBIDO"

    SELECT marca_cod
    INTO   vintra_uni_09
    FROM   tab_marca
    WHERE  marca_desc = "UNIFICACION INTRA-AFORE"

    SELECT marca_cod
    INTO   vintra_cta1_09
    FROM   tab_marca
    WHERE  marca_desc = "UNIFICACION INTRA-AFORE U"

    SELECT marca_cod
    INTO   vextra_uni_09
    FROM   tab_marca
    WHERE  marca_desc = "UNIFICACION EXTRA-AFORE"

    SELECT marca_cod
    INTO   vextra_cta1_09
    FROM   tab_marca
    WHERE  marca_desc = "UNIFICACION EXTRA-AFORE U"

    LET vmarca_09 = " EXECUTE PROCEDURE marca_cuenta (?,?,?,?,?,?,?,?)"
    PREPARE marcaje FROM vmarca_09

    LET vrechazo_cod_09  = 0
    LET vconvive_cod_09  = 0
    LET vcorrelativo_09  = 0
    LET vmarca_causa_09  = 0
    LET vfecha_causa_09  = ""
    #CPL-1867
    WHENEVER ERROR CONTINUE
    DROP TABLE tmp_uni_nss_sinmarca

    CREATE TEMP TABLE tmp_uni_nss_sinmarca (nss    CHAR(11),
                                            tipo   CHAR(10))
    WHENEVER ERROR STOP

END FUNCTION
##################################################
FUNCTION proceso_principal(vfolio_09)

   DEFINE vfolio_09    INTEGER

   SELECT "X"
   FROM   uni_cza_notifica
   WHERE  folio  = vfolio_09
   AND    estado = vrecibido_09
   GROUP BY 1

   IF STATUS = NOTFOUND THEN
      ERROR "NO EXISTEN SOLICITUDES POR MARCAR CON ESTE FOLIO"
      SLEEP 3
      ERROR ""
      EXIT PROGRAM
   END IF

   CALL control_unifica(vfolio_09)
   CALL control_cuenta(vfolio_09)

   RETURN cont1_09,
          cont2_09,
          cont21_09,
          cont3_09,
          cont4_09,
          cont41_09
END FUNCTION
##################################################
FUNCTION control_unifica(vfolio_09)

   DEFINE vfolio_09         INTEGER ,
          nss_unifica_09    CHAR(11),
          nss_actual_09     CHAR(11),
          vmovimiento_09    CHAR(02),
          xrechazo_09       SMALLINT,
          ejecuta_09        CHAR(300),
          vmarca_ent_09     SMALLINT,
          xmarca_09         SMALLINT,
          estado_cta_act_09 SMALLINT,
          estado_pro_act_09 SMALLINT

   DEFINE opc char(1)

   LET cont1_09 = 0
   LET cont2_09 = 0
   LET cont21_09 = 0

   DECLARE cur_1 CURSOR FOR
   SELECT nss_uni,
          ident_movimiento
   FROM   uni_unificador
   WHERE  folio       = vfolio_09
   AND    cve_ent_nss = codigo_09
   AND    estado      = vrecibido_09
   ORDER BY 1,2

   FOREACH cur_1 INTO nss_unifica_09,
                      vmovimiento_09

      LET  cont1_09 = cont1_09 + 1

      IF vmovimiento_09 = "01" THEN
	 LET vmarca_ent_09 = vintra_uni_09
      ELSE
	 LET vmarca_ent_09 = vextra_uni_09
      END IF

      DECLARE cur_mar CURSOR FOR marcaje
      OPEN  cur_mar USING nss_unifica_09,      #pnss
		          vmarca_ent_09,       #marca_entra
                          vcorrelativo_09,     #correlativo
                          vestado_causa_09,    #estado_marca
		          vrechazo_cod_09,     #codigo_rechazo
		          vmarca_causa_09,     #marca_causa
		          vfecha_causa_09,     #fecha_causa
		          vusuario_09          #usuario

      FETCH cur_mar INTO xmarca_09,
                         xrechazo_09
      CLOSE cur_mar

      IF xrechazo_09 > 0 THEN
         LET cont2_09 = cont2_09 + 1

         #CPL-1867
         INSERT INTO tmp_uni_nss_sinmarca
         VALUES      (nss_unifica_09,"UNIFICADOR")

         UPDATE uni_unificador
         SET    resul_operacion  = "02",
                cve_afo_recep    = "000"
         WHERE  nss_uni          = nss_unifica_09
         AND    folio            = vfolio_09
         AND    ident_movimiento = vmovimiento_09
      ELSE
         LET cont21_09 = cont21_09 + 1
      END IF

   END FOREACH
END FUNCTION
###################################################################
FUNCTION control_cuenta(vfolio_09)

   DEFINE vfolio_09         INTEGER,
          nss_ctau_09       CHAR(11),
          nss_unif_09       CHAR(11),
          nss_actual_09     CHAR(11),
          vmovimiento_09    CHAR(02),
          xrechazo_09       SMALLINT,
          vmarca_ent_09     SMALLINT,
          xmarca_09         SMALLINT,
          ejecuta1_09       CHAR(300),
          estado_cta_act_09 SMALLINT,
          estado_pro_act_09 SMALLINT

   LET cont3_09 = 0
   LET cont4_09 = 0
   LET cont41_09 = 0

   DECLARE cur_2 CURSOR FOR

   SELECT nss_cta1,
	  nss_uni
   FROM   uni_unificado
   WHERE  folio        = vfolio_09
   AND    cve_ent_cta1 = codigo_09
   AND    estado       = vrecibido_09

   FOREACH cur_2 INTO nss_ctau_09,
                      nss_unif_09

      SELECT ident_movimiento
      INTO   vmovimiento_09
      FROM   uni_unificador
      WHERE  nss_uni = nss_unif_09
      AND    folio   = vfolio_09
      AND    estado  = vrecibido_09
      GROUP BY 1

      LET  cont3_09 = cont3_09 + 1

      IF vmovimiento_09 = "01" THEN
	 LET vmarca_ent_09 = vintra_cta1_09
      ELSE
	 LET vmarca_ent_09 = vextra_cta1_09
      END IF

      DECLARE cur_mar1 CURSOR FOR marcaje
      OPEN  cur_mar1 USING nss_ctau_09,         #pnss
		           vmarca_ent_09,       #marca_entra
                           vcorrelativo_09,     #correlativo
                           vestado_causa_09,    #estado_marca
		           vrechazo_cod_09,     #codigo_rechazo
		           vmarca_causa_09,     #marca_causa
		           vfecha_causa_09,     #fecha_causa
		           vusuario_09          #usuario
      FETCH cur_mar1 INTO  xmarca_09,
			   xrechazo_09
      CLOSE cur_mar1

      IF xrechazo_09 > 0 THEN
	 LET cont4_09 = cont4_09 + 1
         #CPL-1867
         INSERT INTO tmp_uni_nss_sinmarca
         VALUES      (nss_ctau_09,"UNIFICADO")

         UPDATE uni_unificado
         SET    resulta_operacion = "02",
                diag_unifica      = "02"
         WHERE  nss_cta1          = nss_ctau_09
         AND    folio             = vfolio_09
         #AND    ident_movimiento  = vmovimiento_09
      ELSE
         LET cont41_09 = cont41_09 + 1
      END IF

   END FOREACH

   UPDATE uni_cza_notifica
   SET    estado = vconfronta_09
   WHERE  folio  = vfolio_09
   AND    estado = vrecibido_09

END FUNCTION
