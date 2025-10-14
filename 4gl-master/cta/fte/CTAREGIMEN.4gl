DATABASE safre_tmp

DEFINE    v_nss         CHAR(11),
          v_siefore     SMALLINT,
          v_siefore_rcv SMALLINT,
          v_usuario     CHAR(8) ,
          v_factualiza  DATE    ,
          v_grupo_regimen ,
          v_error SMALLINT

DEFINE    p_sie_adicional SMALLINT

DEFINE    v_registros_ori,
          v_registros_act DECIMAL(10,0)

MAIN

  LET p_sie_adicional = ARG_VAL(1)

  LET v_error = 0

  IF fn_crea_regimen_inv() = 1 THEN
     ERROR "DIFERENCIA DE REGISTROS EN EL REGIMEN DE INVERSION"
     SLEEP 3
     ERROR ""
  END IF
  
  CALL actualiza_etapa()

END MAIN

FUNCTION fn_crea_regimen_inv()

DECLARE c_act_ri CURSOR FOR

   SELECT a.nss          ,
          a.siefore_rcv  ,
          a.usuario      ,
          a.factualiza
     FROM safre_af:cta_nss_regimen a,
          safre_af:cta_cuota_multisie_nss b
    WHERE a.nss = b.nss
      AND a.siefore_rcv = b.siefore
   UNION ALL
   SELECT a.nss          ,
          a.siefore_rcv  ,
          a.usuario      ,
          a.factualiza
     FROM safre_af:cta_nss_regimen a
    WHERE a.nss NOT IN ( SELECT nss 
                           FROM safre_af:cta_cuota_multisie_nss b )
   UNION ALL
   SELECT a.nss          ,
          b.siefore      ,
          USER           ,
          TODAY
     FROM safre_af:cta_nss_regimen a,
          safre_af:cta_cuota_multisie_nss b
    WHERE a.nss = b.nss
      AND a.siefore_rcv <> b.siefore


FOREACH c_act_ri INTO v_nss        ,
                      v_siefore_rcv,
                      v_usuario    ,
                      v_factualiza

   CALL fn_agrega_regimen_inv_nss(v_nss,
                                  v_siefore_rcv,
                                  v_usuario    ,
                                  v_factualiza,
                                  p_sie_adicional)

END FOREACH

CLOSE c_act_ri

UPDATE STATISTICS FOR TABLE tmp_cta_nss_regimen;

  SELECT COUNT(*)
    INTO v_registros_ori
    FROM safre_af:cta_nss_regimen

  SELECT COUNT(*)
    INTO v_registros_act
    FROM tmp_cta_nss_regimen

  IF (v_registros_ori * 11)  <> v_registros_act THEN
    LET v_error = 1 
  END IF

RETURN v_error

END FUNCTION

FUNCTION fn_agrega_regimen_inv_nss ( l_nss,
                                     l_siefore_rcv,
                                     l_usuario,
                                     l_factualiza,
                                     l_sie_adicional )

DEFINE l_nss         CHAR(11),
       l_siefore     SMALLINT,
       l_siefore_rcv SMALLINT,
       l_usuario     CHAR(8) ,
       l_factualiza  DATE    ,
       l_sie_adicional ,
       v_grupo_regimen SMALLINT


   FOR v_grupo_regimen = 1 TO 11

   IF l_sie_adicional = 6 THEN
      CASE 
          WHEN v_grupo_regimen = 2 OR
               v_grupo_regimen = 3 OR
               v_grupo_regimen = 4 OR
               v_grupo_regimen = 9 

               LET l_siefore = l_sie_adicional 

          WHEN v_grupo_regimen = 5 
               LET l_siefore = 0 

          WHEN v_grupo_regimen = 6   -- Grupo vivienda IMSS
               LET l_siefore = 11    -- Siefore vivienda IMSS

          OTHERWISE
               LET l_siefore = l_siefore_rcv
      END CASE
   ELSE
      CASE 
          WHEN v_grupo_regimen = 3 
               LET l_siefore = 1

          WHEN v_grupo_regimen = 5 
               LET l_siefore = 0 

          WHEN v_grupo_regimen = 6   -- Grupo vivienda IMSS
               LET l_siefore = 11    -- Siefore vivienda IMSS

          OTHERWISE
               LET l_siefore = l_siefore_rcv

      END CASE
   END IF  

   INSERT INTO tmp_cta_nss_regimen 
          VALUES ( l_nss      ,
                   v_grupo_regimen, 
                   l_siefore,
                   l_usuario  ,
                   l_factualiza )
   END FOR

END FUNCTION

FUNCTION actualiza_etapa()
   DEFINE li_folio INTEGER
   DEFINE vhora_fin        DATETIME HOUR TO SECOND
   DEFINE vfecha_fin       DATE
   
  SELECT MAX(folio)
  INTO   li_folio
  FROM   safre_af:dis_ctrl_proceso
  WHERE  proceso_cod = 'CTAB035'
  AND    etapa_cod = 5
	
	LET vfecha_fin = TODAY
  LET vhora_fin  = CURRENT
	
	UPDATE safre_af:dis_ctrl_proceso
  SET    hora_final = vhora_fin,
         parametro2 = vfecha_fin,
         resultado  = "TERMINA PREPARACION de Regimen Multisiefore"
  WHERE  folio = li_folio
  AND    proceso_cod   = 'CTAB035'
  AND    etapa_cod     = 5;
END FUNCTION
