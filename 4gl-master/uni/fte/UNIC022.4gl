###############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                            #
#Propietario       => E.F.P. 			                              #
#Programa          => ACTUALIZA ESTADOS PARA TRASPASO UNIFICACION             #
#Fecha             => 12 febrero 2002                                         #
#Actualizado       => ARMANDO RODRIGUEZ CASTROPAREDES                         #
#Fecha             => 20 septiembre 2002                                      #
#Modificado por    => OMAR SANDOVAL BADILLO                                   #
#Fecha modifica    => 15 NOVIEMBRE 2004                                       #
#Modificado por    => JGHM
#Fecha modifica    => 26 Nov 09 
###############################################################################
DATABASE safre_af
GLOBALS
        DEFINE reg_3_22 RECORD
            nss               CHAR(11),
            folio             INTEGER,
            estado            SMALLINT
        END RECORD

        DEFINE reg_4_22 RECORD
            nss               CHAR(11),
            folio             INTEGER,
            estado            SMALLINT
        END RECORD

        DEFINE reg_5_22 RECORD
            nss               CHAR(11),
            folio             INTEGER,
            folio_liquida     INTEGER,
            estado            SMALLINT
        END RECORD

        DEFINE 
            HOY_22               DATE,
            vfecha_22            DATE,
            vfecha1_22           DATE,
            vfinicio_22          DATE,
	    enter_22             CHAR(1),
	    vpregunta_22         CHAR(1),
	    usuario_22           CHAR(8),
	    vid_22               CHAR(06),
            vdia_22              SMALLINT,
            marca_22             SMALLINT,
            cont1_22             INTEGER,
            cont2_22             INTEGER,
            cont_22              INTEGER,
            vtraspaso_22         INTEGER,
            vsolicitado_22       INTEGER,
            vaceptado_22         INTEGER,
            vcomplemento_22      INTEGER,
            vliquidado_22        INTEGER,
	    codigo_22            INTEGER

   DEFINE xx_programa   CHAR(07)
END GLOBALS
#########################################################
FUNCTION UNIC022(x_programa)
   
   DEFINE x_programa  CHAR(07)

   CALL STARTLOG("UNIC022.log")

   LET xx_programa = x_programa CLIPPED

   CALL inicio_22()
   CALL proceso_principal_22()

END FUNCTION
#########################################################
FUNCTION inicio_22()

    DEFINE opc CHAR(1)
    DEFINE x_mes    SMALLINT,
           xxxx_dia SMALLINT,
           xxxx_mes SMALLINT

    LET HOY_22 = TODAY

    SELECT codigo_afore,
           user
    INTO   codigo_22,
           usuario_22
    FROM   tab_afore_local

    SELECT estado
    INTO   vtraspaso_22
    FROM   uni_status
    WHERE  descripcion = "TRASPASADO"

    SELECT estado
    INTO   vsolicitado_22
    FROM   uni_status
    WHERE  descripcion = "SOLICITADO"

    SELECT estado
    INTO   vaceptado_22
    FROM   uni_status
    WHERE  descripcion = "ACEPTADO CONFRONTA"

    SELECT estado
    INTO   vcomplemento_22
    FROM   uni_status
    WHERE  descripcion = "TRASPASO COMPLEMENTARIO"

    SELECT estado
    INTO   vliquidado_22
    FROM   uni_status
    WHERE  descripcion = "LIQUIDADO"


   IF xx_programa = "UNIM004" THEN
       LET x_mes = MONTH(HOY_22)
       LET x_mes = x_mes - 1

       --- Se cambia fecha para ampliar el rango de un mes, para q tome desde 1-1-08 a la fecha
       --- tons se pone fija 26 Nov 09
       LET vfecha1_22 = '01/01/2008'                
       --LET vfecha1_22 = MDY(x_mes,1,YEAR(HOY_22))
       LET vfecha_22  = HOY_22 
    ELSE
       IF xx_programa = "UNIC011" THEN
#osb
          LET xxxx_dia = DAY(HOY_22)
          LET xxxx_mes = MONTH(HOY_22) - 1

          LET vfinicio_22 = HOY_22 - xxxx_dia
          LET vfecha_22   = vfinicio_22

          LET vfecha1_22 = vfinicio_22 - xxxx_mes
          LET vfecha1_22 = MDY(MONTH(vfecha1_22),1,YEAR(vfecha1_22))
          LET xxxx_dia   = DAY(vfecha1_22)
          LET vfecha1_22 = vfecha1_22 - xxxx_dia

          LET vfecha_22 = HOY_22
          LET vfecha_22  = habil_anterior_22(vfecha_22) 
          LET vfecha1_22 = habil_siguiente_22(vfecha1_22) 

{
          LET  vfinicio_22 = HOY_22 -1 units day
          LET  vdia_22     = DAY(vfinicio_22) - 1
          LET  vfinicio_22 = vfinicio_22 - vdia_22 UNITS DAY
          LET  vfecha1_22  = vfinicio_22 - 1 units day
          LET  vfecha_22   = vfinicio_22
          LET  vfecha_22   = vfecha_22 + 1 units month
          LET  vfecha_22   = vfecha_22 - 1 units day
}

       END IF
    END IF

END FUNCTION
#########################################################
FUNCTION proceso_principal_22()

      LET marca_22 = 0

      SELECT "X"
      FROM   uni_det_traspaso
      WHERE  estado = vtraspaso_22 
      GROUP BY 1

      IF STATUS <> NOTFOUND THEN
         LET marca_22 = 1
      END IF

      SELECT "X"
      FROM   uni_det_certifica
      WHERE  estado = vtraspaso_22 
      GROUP BY 1

      IF STATUS <> NOTFOUND THEN
         LET marca_22 = 1
      END IF

      SELECT "X"
      FROM   uni_det_asignado
      WHERE  estado = vtraspaso_22 
      GROUP BY 1

      IF STATUS <> NOTFOUND THEN
         LET marca_22 = 1
      END IF

      LET marca_22 = 1

      IF marca_22 = 0  THEN
         ERROR "NO HAY CUENTAS SOLICITADAS PARA ACTUALIZAR" 
         SLEEP 4
      ELSE
{
         IF vfecha_22 <> HOY_22 THEN
            ERROR "ESTE PROCESO SE REALIZA EL ULTIMO DIA HABIL"
            SLEEP 4
            ERROR ""
         END IF
}		    
         ERROR "PROCESANDO INFORMACION ..."
         CALL actualiza_traspaso_22()
         #CALL actualiza_intra_22()
         #CALL actualiza_certifica_22()
         CALL actualiza_asignado_22()
         CALL complemento_afo_22()
      END IF

END FUNCTION
#########################################################
FUNCTION actualiza_traspaso_22()
   DEFINE  vnss_22           CHAR(11)
   DEFINE  vnss_uni_22       CHAR(11)
   DEFINE  vnss_cta1_22      CHAR(11)
   DEFINE  vfolio_22         INTEGER 

#-------actualiza los trapasos afore-afore
   LET vnss_22 = ""
   LET vnss_uni_22 = ""
   LET vnss_cta1_22 = ""

   DECLARE cur_31 CURSOR FOR
   SELECT unique nss
   FROM   taa_rcv_recepcion
   #WHERE  fecha_mov_banxico = vfecha
   WHERE  fecha_mov_banxico BETWEEN vfecha1_22
			        AND vfecha_22
   AND    tipo_traspaso     = 12
   AND    ident_operacion   = "09"

   FOREACH cur_31 INTO vnss_22
      SELECT "X"
      FROM   uni_det_traspaso
      WHERE  nss    = vnss_22
      AND    estado = vtraspaso_22
      GROUP BY 1

      IF STATUS <> NOTFOUND THEN
         SELECT max(folio)
         INTO   vfolio_22
         FROM   uni_det_traspaso
         WHERE  nss    = vnss_22
         AND    estado = vtraspaso_22
         #GROUP BY 1

         UPDATE uni_det_traspaso
         SET    estado = vliquidado_22
         WHERE  nss    = vnss_22
         AND    estado = vtraspaso_22
         AND    folio  = vfolio_22

         SELECT "X"
         FROM   uni_unificado
         WHERE  nss_uni = vnss_22
         AND    estado  in (30,40)
         AND    folio   = vfolio_22
         GROUP BY 1

         IF STATUS = NOTFOUND THEN
            SELECT "X"
            FROM   uni_unificado
            WHERE  nss_cta1 = vnss_22
            AND    estado   = vsolicitado_22
            AND    folio    = vfolio_22
            GROUP BY 1

            IF STATUS = NOTFOUND THEN
               #ERROR "No se encontro el NSS:",vnss CLIPPED
            ELSE
               SELECT nss_uni
               INTO   vnss_uni_22
               FROM   uni_unificado
               WHERE  nss_cta1 = vnss_22
               AND    estado   = vsolicitado_22
               AND    folio    = vfolio_22
               GROUP BY 1

               UPDATE uni_unificado
               SET    estado   = vtraspaso_22,
                      estado_traspaso = 1,
                      estado_unifica  = 1
               WHERE  nss_cta1 = vnss_22
               AND    estado   = vsolicitado_22
               AND    folio    = vfolio_22

               UPDATE uni_unificador
               SET    estado      = vtraspaso_22,
                      estado_traspaso = 1,
                      estado_familia  = 1
               WHERE  nss_uni     = vnss_uni_22
               AND    estado      = vsolicitado_22
               AND    folio       = vfolio_22
               AND    cve_ent_nss = codigo_22
            END IF
         ELSE
            UPDATE uni_unificado
            SET    estado          = vtraspaso_22,
                   estado_traspaso = 1,
                   estado_unifica  = 1
            WHERE  nss_uni         = vnss_22
            AND    estado          = vsolicitado_22
            AND    folio           = vfolio_22
            AND    cve_ent_cta1    = codigo_22

            UPDATE uni_unificador
            SET    estado      = vtraspaso_22,
                   estado_traspaso = 1,
                   estado_familia  = 1
            WHERE  nss_uni     = vnss_22
            AND    estado      = vsolicitado_22
            AND    folio       = vfolio_22
         END IF
      ELSE 
         #ERROR "No existe registro como solicitado",vnss CLIPPED
         #SLEEP 3
      END IF
   END FOREACH
   
END FUNCTION
#########################################################
FUNCTION actualiza_intra_22()

   DEFINE  vnss_22        CHAR(11)
   DEFINE  vpesos_22      DECIMAL(16,6)
   DEFINE  vaccion_22     DECIMAL(16,6)

#-------actualiza los trapasos intra-afore

   DECLARE cur_5 CURSOR FOR
   SELECT nss_uni,
          folio,
          folio_liquida,
          estado
   FROM   uni_unificador
   WHERE  estado           = vliquidado_22
   AND    ident_movimiento = "01"

   LET cont1_22 = 0
   LET cont2_22 = 0

   FOREACH cur_5 INTO reg_5_22.*

      DECLARE cur_6 CURSOR FOR
      SELECT nss_cta1
      FROM   uni_unificado
      WHERE  nss_uni       = reg_5_22.nss
      AND    folio         = reg_5_22.folio
      AND    folio_liquida = reg_5_22.folio_liquida
      AND    estado        = vliquidado_22
      GROUP BY 1
 
      FOREACH cur_6 INTO vnss_22
         IF STATUS <> NOTFOUND THEN
            SELECT SUM(monto_en_acciones)
            INTO   vaccion_22
            FROM   dis_cuenta
            WHERE  nss              = vnss_22
            AND    subcuenta in (1,2,3,5,6,7,9,10)
            AND    tipo_movimiento  > 0
            AND    fecha_conversion <= HOY_22

            IF  vaccion_22 > 0  THEN
                LET cont1_22 = cont1_22 + 1

                UPDATE uni_unificado
                SET    estado        = vcomplemento_22
                WHERE  nss_cta1      = reg_5_22.nss
                AND    folio         = reg_5_22.folio
                AND    folio_liquida = reg_5_22.folio_liquida
                AND    estado        = vliquidado_22
            END IF

            SELECT SUM(monto_en_pesos)
            INTO   vpesos_22
            FROM   dis_cuenta
            WHERE  nss              = vnss_22
            AND    subcuenta IN (4,8)
            AND    tipo_movimiento NOT IN(888)
            AND    fecha_conversion <= HOY_22

            IF  vpesos_22 > 0  THEN
                LET cont2_22 = cont2_22 + 1

                UPDATE uni_unificado
                SET    estado        = vcomplemento_22
                WHERE  nss_cta1      = reg_5_22.nss
                AND    folio         = reg_5_22.folio
                AND    folio_liquida = reg_5_22.folio_liquida
                AND    estado        = vliquidado_22
            END IF
         END IF
      END FOREACH
   END FOREACH

   IF cont1_22 > 0 THEN
      DISPLAY "Total de NSS con saldo en RCV :      ",cont1_22 AT 15,3
   END IF

   IF cont2_22 > 0 THEN
      DISPLAY "Total de NSS con saldo en VIVIENDA : ",cont1_22 AT 16,3
   END IF

END FUNCTION
#########################################################
FUNCTION actualiza_certifica_22()
   DEFINE  vnss_22           CHAR(11)
   DEFINE  vnss_uni_22       CHAR(11)
   DEFINE  vnss_cta1_22      CHAR(11)
   DEFINE  vfolio_22         INTEGER 

#-------actualiza los trapasos ps-afore
   LET vnss_22 = ""
   LET vnss_uni_22 = ""
   LET vnss_cta1_22 = ""

   DECLARE cur_32 CURSOR FOR
   SELECT unique nss_afo_recep
   FROM   taa_det_recep_ps
   WHERE  ident_operacion   = "09"
   AND    fecha_mov_banxico = vfecha1_22
   AND    tipo_traspaso     = 13

   FOREACH cur_32 INTO vnss_22

      SELECT "X"
      FROM   uni_det_certifica
      WHERE  nss    = vnss_22
      AND    estado = vtraspaso_22
      GROUP BY 1

      IF STATUS <> NOTFOUND THEN

         SELECT max(folio)
         INTO   vfolio_22
         FROM   uni_det_certifica
         WHERE  nss    = vnss_22
         AND    estado = vtraspaso_22
         #GROUP BY 1

         UPDATE uni_det_certifica
         SET    estado = vliquidado_22
         WHERE  nss    = vnss_22
         AND    estado = vtraspaso_22
         AND    folio  = vfolio_22

         SELECT "X"
         FROM   uni_unificado
         WHERE  nss_uni = vnss_22
         AND    estado  = vsolicitado_22
         AND    folio   = vfolio_22
         GROUP BY 1

         IF STATUS = NOTFOUND THEN
            SELECT "X"
            FROM   uni_unificado
            WHERE  nss_cta1 = vnss_22
            AND    estado   = vsolicitado_22
            AND    folio    = vfolio_22
            GROUP BY 1

            IF STATUS = NOTFOUND THEN
               ERROR "No se encontro el NSS:",vnss_22 CLIPPED
            ELSE
               SELECT nss_uni
               INTO   vnss_uni_22
               FROM   uni_unificado
               WHERE  nss_cta1 = vnss_22
               AND    estado   = vsolicitado_22
               AND    folio    = vfolio_22
               GROUP BY 1

               UPDATE uni_unificado
               SET    estado   = vtraspaso_22,
                      estado_traspaso = 1,
                      estado_unifica  = 1
               WHERE  nss_cta1 = vnss_22
               AND    estado   = vsolicitado_22
               AND    folio    = vfolio_22

               UPDATE uni_unificador
               SET    estado      = vtraspaso_22,
                      estado_traspaso = 1,
                      estado_familia  = 1
               WHERE  nss_uni     = vnss_uni_22
               AND    estado      = vsolicitado_22
               AND    folio       = vfolio_22
               AND    cve_ent_nss = codigo_22
            END IF
         ELSE
            UPDATE uni_unificado
            SET    estado          = vtraspaso_22,
                   estado_traspaso = 1,
                   estado_unifica  = 1
            WHERE  nss_uni         = vnss_22
            AND    estado          = vsolicitado_22
            AND    folio           = vfolio_22
            AND    cve_ent_cta1    = codigo_22

            UPDATE uni_unificador
            SET    estado      = vtraspaso_22,
                   estado_traspaso = 1,
                   estado_familia  = 1
            WHERE  nss_uni     = vnss_22
            AND    estado      = vsolicitado_22
            AND    folio       = vfolio_22
         END IF
      ELSE 
         ERROR "No existe registro como solicitado",vnss_22 CLIPPED
         SLEEP 3
      END IF
   END FOREACH

END FUNCTION
#########################################################
FUNCTION complemento_afo_22()

   DEFINE  vnss_22           CHAR(11)
   DEFINE  vnss_uni_22       CHAR(11)
   DEFINE  vnss_cta1_22      CHAR(11)
   DEFINE  vfolio_22         INTEGER 

   DECLARE cur_33 CURSOR FOR
   SELECT unique nss_afo_recep
   FROM   taa_det_recep_ps
   WHERE  ident_operacion   = "12"
   AND    fecha_mov_banxico = vfecha1_22
   AND    tipo_traspaso     = 13

   FOREACH cur_33 INTO vnss_22

      SELECT max(folio)
      INTO   vfolio_22
      FROM   uni_det_certifica
      WHERE  nss    = vnss_22
      AND    estado = 40

      SELECT "X"
      FROM   uni_unificado
      WHERE  nss_uni = vnss_22
      AND    estado  = vliquidado_22
      AND    folio   = vfolio
      GROUP BY 1

      IF STATUS = NOTFOUND THEN
         SELECT "X"
         FROM   uni_unificado
         WHERE  nss_cta1 = vnss_22
         AND    estado   = vliquidado_22
         AND    folio    = vfolio_22
         GROUP BY 1

         IF STATUS = NOTFOUND THEN
            #ERROR "No se encontro el NSS:",vnss_22 CLIPPED
         ELSE
            SELECT nss_uni
            INTO   vnss_uni_22
            FROM   uni_unificado
            WHERE  nss_cta1 = vnss_22
            AND    estado   = vliquidado_22
            AND    folio    = vfolio_22
            GROUP BY 1

            UPDATE uni_unificado
            SET    estado   = vcomplemento_22,
                   estado_traspaso = estado_traspaso_22 + 1
            WHERE  nss_cta1 = vnss_22
            AND    estado   = vliquidado_22
            AND    folio    = vfolio_22

            UPDATE uni_unificador
            SET    estado      = vcomplemento_22,
                   estado_traspaso = estado_traspaso_22 + 1
            WHERE  nss_uni     = vnss_uni_22
            AND    estado      = vliquidado_22
            AND    folio       = vfolio_22
         END IF
      END IF
   END FOREACH

   LET vnss_22 = ""
   LET vnss_uni_22 = ""

   DECLARE cur_34 CURSOR FOR
   SELECT unique nss
   FROM   taa_rcv_recepcion
   WHERE  ident_operacion   = "12"
   #AND    fecha_mov_banxico = vfecha_22
   AND    fecha_mov_banxico BETWEEN vfecha1_22
			        AND vfecha_22
   AND    tipo_traspaso     = 12

   FOREACH cur_34 INTO vnss_22

      SELECT folio
      INTO   vfolio_22
      FROM   uni_det_traspaso
      WHERE  nss    = vnss_22
      GROUP BY 1

      SELECT "X"
      FROM   uni_unificado
      WHERE  nss_uni = vnss_22
      AND    estado  = vliquidado_22
      AND    folio   = vfolio_22
      GROUP BY 1

      IF STATUS = NOTFOUND THEN
         SELECT "X"
         FROM   uni_unificado
         WHERE  nss_cta1 = vnss_22
         AND    estado   = vliquidado_22
         AND    folio    = vfolio_22
         GROUP BY 1

         IF STATUS = NOTFOUND THEN
            #ERROR "No se encontro el NSS:",vnss_22 CLIPPED
         ELSE
            SELECT nss_uni
            INTO   vnss_uni_22
            FROM   uni_unificado
            WHERE  nss_cta1 = vnss_22
            AND    estado   = vliquidado_22
            AND    folio    = vfolio_22
            GROUP BY 1

            UPDATE uni_unificado
            SET    estado   = vcomplemento_22,
                   estado_traspaso = estado_traspaso + 1
            WHERE  nss_cta1 = vnss_22
            AND    estado   = vliquidado_22
            AND    folio    = vfolio_22

            UPDATE uni_unificador
            SET    estado      = vcomplemento_22,
                   estado_traspaso = estado_traspaso + 1
            WHERE  nss_uni     = vnss_uni_22
            AND    estado      = vliquidado_22
            AND    folio       = vfolio_22
         END IF
      END IF
   END FOREACH

END FUNCTION
#########################################################
FUNCTION actualiza_asignado_22()
   DEFINE  vnss_22           CHAR(11)
   DEFINE  vnss_uni_22       CHAR(11)
   DEFINE  vnss_cta1_22      CHAR(11)
   DEFINE  vfolio_22         INTEGER 

#-------actualiza los trapasos afore-afore
   LET vnss_22 = ""
   LET vnss_uni_22 = ""
   LET vnss_cta1_22 = ""

   DECLARE cur_35 CURSOR FOR
   SELECT unique nss
   FROM   taa_rcv_recepcion
   #WHERE  fecha_mov_banxico = vfecha_22
   WHERE  fecha_mov_banxico BETWEEN vfecha1_22
			        AND vfecha_22
   AND    tipo_traspaso     = 20
   AND    ident_operacion   = "09"

   FOREACH cur_35 INTO vnss_22

      SELECT "X"
      FROM   uni_det_asignado
      WHERE  nss    = vnss_22
      AND    estado = vtraspaso_22
      GROUP BY 1

      IF STATUS <> NOTFOUND THEN

         SELECT folio
         INTO   vfolio_22
         FROM   uni_det_asignado
         WHERE  nss    = vnss_22
         AND    estado = vtraspaso_22
         GROUP BY 1

         UPDATE uni_det_asignado
         SET    estado = vliquidado_22
         WHERE  nss    = vnss_22
         AND    estado = vtraspaso_22
         AND    folio  = vfolio_22

         SELECT "X"
         FROM   uni_unificado
         WHERE  nss_uni = vnss_22
         AND    estado  IN (30,40) 
         AND    folio   = vfolio_22
         GROUP BY 1

         IF STATUS = NOTFOUND THEN
            SELECT "X"
            FROM   uni_unificado
            WHERE  nss_cta1 = vnss_22
            AND    estado   = vsolicitado_22
            AND    folio    = vfolio_22
            GROUP BY 1

            IF STATUS = NOTFOUND THEN
               #ERROR "No se encontro el NSS:",vnss_22 CLIPPED
            ELSE
               SELECT nss_uni
               INTO   vnss_uni_22
               FROM   uni_unificado
               WHERE  nss_cta1 = vnss_22
               AND    estado   = vsolicitado_22
               AND    folio    = vfolio_22
               GROUP BY 1

               UPDATE uni_unificado
               SET    estado   = vtraspaso_22,
                      estado_traspaso = 1,
                      estado_unifica  = 1
               WHERE  nss_cta1 = vnss_22
               AND    estado   = vsolicitado_22
               AND    folio    = vfolio_22

               UPDATE uni_unificador
               SET    estado      = vtraspaso_22,
                      estado_traspaso = 1,
                      estado_familia  = 1
               WHERE  nss_uni     = vnss_uni_22
               AND    estado      = vsolicitado_22
               AND    folio       = vfolio_22
               AND    cve_ent_nss = codigo_22
            END IF
         ELSE

            UPDATE uni_unificado
            SET    estado          = vtraspaso_22,
                   estado_traspaso = 1,
                   estado_unifica  = 1
            WHERE  nss_uni         = vnss_22
            AND    estado          = vsolicitado_22
            AND    folio           = vfolio_22
            AND    cve_ent_cta1    = codigo_22

            UPDATE uni_unificador
            SET    estado      = vtraspaso_22,
                   estado_traspaso = 1,
                   estado_familia  = 1
            WHERE  nss_uni     = vnss_22
            AND    estado      = vsolicitado_22
            AND    folio       = vfolio_22
         END IF
      ELSE 
         #ERROR "No existe registro como solicitado",vnss_22 CLIPPED
         #SLEEP 3
      END IF
   END FOREACH
   
END FUNCTION
#########################################################
FUNCTION habil_siguiente_22(diaActual_22)

   DEFINE diaTmp_22        DATE,
          contador_22      SMALLINT,
          diaActual_22     DATE

   DEFINE diaHabilSig_22   DATE,
          diaSemana_22     SMALLINT,
          feriado_22       SMALLINT,
          finSemana_22     SMALLINT

   LET diaHabilSig_22 = diaActual_22 +1 UNITS DAY

        WHILE TRUE
            LET feriado_22   = 0
            LET finSemana_22 = 0
            LET diaSemana_22 = WEEKDAY(diaHabilSig_22) 

            IF diaSemana_22 = 0 OR diaSemana_22 = 6 THEN
                LET finSemana_22 = 1
            END IF

            SELECT *
            FROM   tab_feriado
            WHERE  feria_fecha = diaHabilSig_22

            IF STATUS <> NOTFOUND THEN
                LET feriado_22 = 1
            END IF

            IF feriado_22 = 1 OR finSemana_22 = 1 THEN
                LET diaHabilSig_22 = diaHabilSig_22 + 1 UNITS DAY
            ELSE
                EXIT WHILE
            END IF
        END WHILE

        RETURN diaHabilSig_22 
END FUNCTION
#########################################################
FUNCTION habil_anterior_22(diaActual_22)

   DEFINE diaTmp_22	   DATE,
   	  contador_22	   SMALLINT,
	  diaActual_22	   DATE,
          diaHabilAnt_22   DATE,
	  diaSemana_22	   SMALLINT,
	  feriado_22	   SMALLINT,
	  finSemana_22	   SMALLINT

   LET diaHabilAnt_22 = diaActual_22

	WHILE TRUE
   	    LET feriado_22   = 0
   	    LET finSemana_22 = 0
   	    LET diaSemana_22 = WEEKDAY(diaHabilAnt_22)  

   	    IF diaSemana_22 = 0 OR diaSemana_22 = 6 THEN
      	        LET finSemana_22 = 1
   	    END IF
  	     
   	    SELECT *
   	    FROM   tab_feriado 
   	    WHERE  feria_fecha = diaHabilAnt_22
    	
   	    IF STATUS <> NOTFOUND THEN
       	        LET feriado_22 = 1
   	    END IF 
		
   	    IF feriado_22 = 1 OR finSemana_22 = 1 THEN
       	        LET diaHabilAnt_22 = diaHabilAnt_22 - 1 UNITS DAY
   	    ELSE
       	        EXIT WHILE
   	    END IF
	END WHILE

	RETURN diaHabilAnt_22
END FUNCTION #habil_anterior
